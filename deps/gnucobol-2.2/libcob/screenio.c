/*
   Copyright (C) 2001-2012, 2014-2017 Free Software Foundation, Inc.
   Written by Keisuke Nishida, Roger While, Simon Sobisch, Edward Hart

   This file is part of GnuCOBOL.

   The GnuCOBOL runtime library is free software: you can redistribute it
   and/or modify it under the terms of the GNU Lesser General Public License
   as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   GnuCOBOL is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with GnuCOBOL.  If not, see <http://www.gnu.org/licenses/>.
*/


#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#ifdef	HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef	HAVE_LOCALE_H
#include <locale.h>
#endif

#ifdef	_WIN32
/* Later pdcurses versions require define before the include for DLL build */
#define	PDC_DLL_BUILD	1
#include <io.h>
#endif

#if defined(HAVE_NCURSESW_NCURSES_H)
#include <ncursesw/ncurses.h>
#define COB_GEN_SCREENIO
#elif defined(HAVE_NCURSESW_CURSES_H)
#include <ncursesw/curses.h>
#define COB_GEN_SCREENIO
#elif defined(HAVE_NCURSES_H)
#include <ncurses.h>
#define COB_GEN_SCREENIO
#elif defined(HAVE_NCURSES_NCURSES_H)
#include <ncurses/ncurses.h>
#define COB_GEN_SCREENIO
#elif defined(HAVE_PDCURSES_H)
#include <pdcurses.h>
#define COB_GEN_SCREENIO
#elif defined(HAVE_CURSES_H)
#include <curses.h>
#define COB_GEN_SCREENIO
#endif

/* Force symbol exports */
#define	COB_LIB_EXPIMP

#include "libcob.h"
#include "coblocal.h"

#ifdef	HAVE_CURSES_FREEALL
extern void	_nc_freeall (void);
#endif

struct cob_inp_struct {
	cob_screen		*scr;
	size_t			up_index;
	size_t			down_index;
	int			this_y;
	int			this_x;
};

#define	COB_INP_FLD_MAX		512U

#define	COB_INP_SIZE	(COB_INP_FLD_MAX * sizeof(struct cob_inp_struct))

#define	COB_CH_UL		((const chtype)'_')
#define	COB_CH_SP		((const chtype)' ')
#define	COB_CH_AS		((const chtype)'*')

/* Local variables */

static cob_global		*cobglobptr;
static cob_settings		*cobsetptr;

/* Local variables when screenio activated */

#ifdef	COB_GEN_SCREENIO
static const cob_field_attr	const_alpha_attr =
				{COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};
static struct cob_inp_struct	*cob_base_inp;
static size_t			totl_index;
static size_t			cob_has_color;
static int			global_return;
static int			cob_current_y;
static int			cob_current_x;
static short			fore_color;
static short			back_color;
static int			origin_y;
static int			origin_x;
static int			display_cursor_y;
static int			display_cursor_x;
static int			accept_cursor_y;
static int			accept_cursor_x;
static int			pending_accept;
static int			got_sys_char;
#endif

/* Local function prototypes when screenio activated */

#ifdef	COB_GEN_SCREENIO
static void cob_screen_init	(void);
#endif

/* Local functions */

static void
cob_speaker_beep (void)
{
	int	fd;

	fd = fileno (stdout);
	if (fd >= 0) {
		(void)write (fd, "\a", (size_t)1);
	}
}

static COB_INLINE COB_A_INLINE void
init_cob_screen_if_needed (void)
{
	if (!cobglobptr) {
		cob_fatal_error (COB_FERROR_INITIALIZED);
	}
#ifdef	COB_GEN_SCREENIO
	if (!cobglobptr->cob_screen_initialized) {
		cob_screen_init ();
	}
#endif
}

#ifdef	COB_GEN_SCREENIO

static void
cob_beep (void)
{
	switch (COB_BEEP_VALUE) {
	case 1:
		(void)flash ();
		return;
	case 2:
		cob_speaker_beep ();
		return;
	case 9:
		return;
	default:
		(void)beep ();
		return;
	}
}

static void
cob_convert_key (int *keyp, const cob_u32_t field_accept)
{
	/* Map key to KEY_xxx value */
	switch (*keyp) {
	case '\n':
	case '\r':
	case '\004':
	case '\032':
		*keyp = KEY_ENTER;
		break;
	case '\t':
		*keyp = KEY_STAB;
		break;
	case '\b':
	case 0177:
		*keyp = KEY_BACKSPACE;
		break;
	case 01005:
		*keyp = KEY_EOL;	/* Alt-Delete */
		break;
	case 01040:
		*keyp = KEY_CLOSE;	/* Alt-left-arrow */
		break;
	case 01062:
		*keyp = KEY_PREVIOUS;	/* Alt-right-arrow */
		break;

#ifdef	KEY_A1
	/* A1, A3, C1, C3 must be present */
	case KEY_A1:
		*keyp = KEY_HOME;
		break;
	case KEY_A3:
		*keyp = KEY_PPAGE;
		break;
	case KEY_C1:
		*keyp = KEY_END;
		break;
	case KEY_C3:
		*keyp = KEY_NPAGE;
		break;
	/* Any or all of A2, B1-3, C2 MAY be present */
	/* Note B2 ignored */
#ifdef	KEY_A2
	case KEY_A2:
		*keyp = KEY_UP;
		break;
#endif
#ifdef	KEY_B1
	case KEY_B1:
		*keyp = KEY_LEFT;
		break;
#endif
#ifdef	KEY_B3
	case KEY_B3:
		*keyp = KEY_RIGHT;
		break;
#endif
#ifdef	KEY_C2
	case KEY_C2:
		*keyp = KEY_DOWN;
		break;
#endif

#if	defined(__PDCURSES__) && defined(PADSLASH)
	case PADSLASH:
		*keyp = '/';
		break;
	case PADSTAR:
		*keyp = '*';
		break;
	case PADMINUS:
		*keyp = '-';
		break;
	case PADPLUS:
		*keyp = '+';
		break;
	case PADENTER:
		*keyp = KEY_ENTER;
		break;
#ifdef	PAD0
	case PAD0:
		*keyp = KEY_IC;
		break;
	case PADSTOP:
		*keyp = KEY_DC;
		break;
#endif	/* PAD0 */
#endif	/* __PDCURSES__ */
#endif	/* KEY_A1 */
	default:
		break;
	}

	/* Check if key should be ignored */
	switch (*keyp) {
	/* 2012/08/30 removed to allow Tab key in extended Accept.
	case KEY_STAB:
		if (field_accept) {
			*keyp = 0;
		}
		break;
	*/
	case '\033':
		if (!COB_EXTENDED_STATUS || !COB_USE_ESC) {
			*keyp = 0;
		}
		break;
	case KEY_PPAGE:
	case KEY_NPAGE:
	case KEY_PRINT:
		if (!COB_EXTENDED_STATUS) {
			*keyp = 0;
		}
		break;
	case KEY_UP:
	case KEY_DOWN:
		if (field_accept && !COB_EXTENDED_STATUS) {
			*keyp = 0;
		}
		break;
	default:
		break;
	}
}


static void
raise_ec_on_invalid_line_or_col (const int line, const int column)
{
	int	max_y;
	int	max_x;

	getmaxyx (stdscr, max_y, max_x);
	if (line < 0 || line >= max_y) {
		cob_set_exception (COB_EC_SCREEN_LINE_NUMBER);
	}
	if (column < 0 || column >= max_x) {
		cob_set_exception (COB_EC_SCREEN_STARTING_COLUMN);
	}
}

static int
cob_move_cursor (const int line, const int column)
{
	int status = move (line, column);

	if (status == ERR) {
		raise_ec_on_invalid_line_or_col (line, column);
	}
	return status;
}

void
cob_set_cursor_pos (int line, int column)
{
	init_cob_screen_if_needed ();
	(void) move (line, column);
}

static void
cob_move_to_beg_of_last_line (void)
{
	int	max_y;
	int	max_x;

	COB_UNUSED (max_x);

	getmaxyx (stdscr, max_y, max_x);
	/* We don't need to check for exceptions here; it will always be fine */
	move (max_y, 0);
}

enum screen_statement {
	ACCEPT_STATEMENT,
	DISPLAY_STATEMENT
};

static void
cob_screen_attr (cob_field *fgc, cob_field *bgc, const cob_flags_t attr,
		 const enum screen_statement stmt)
{
	int		i;
	int		styles = 0;
	int		line;
	int		column;
	short		fgcolor;
	short		bgcolor;
	short		fgdef;
	short		bgdef;

	attrset (A_NORMAL);
	if (attr & COB_SCREEN_REVERSE) {
		styles |= A_REVERSE;
	}
	if (attr & COB_SCREEN_HIGHLIGHT) {
		styles |= A_BOLD;
	}
	if (attr & COB_SCREEN_LOWLIGHT) {
		styles |= A_DIM;
	}
	if (attr & COB_SCREEN_BLINK) {
		styles |= A_BLINK;
	}
	if (attr & COB_SCREEN_UNDERLINE) {
		styles |= A_UNDERLINE;
	}
	if (styles) {
		attron (styles);
	}
	if (cob_has_color) {
		fgcolor = fore_color;
		bgcolor = back_color;
		if (fgc) {
			switch (cob_get_int (fgc)) {
			case COB_SCREEN_BLACK:
				fgcolor = COLOR_BLACK;
				break;
			case COB_SCREEN_BLUE:
				fgcolor = COLOR_BLUE;
				break;
			case COB_SCREEN_GREEN:
				fgcolor = COLOR_GREEN;
				break;
			case COB_SCREEN_CYAN:
				fgcolor = COLOR_CYAN;
				break;
			case COB_SCREEN_RED:
				fgcolor = COLOR_RED;
				break;
			case COB_SCREEN_MAGENTA:
				fgcolor = COLOR_MAGENTA;
				break;
			case COB_SCREEN_YELLOW:
				fgcolor = COLOR_YELLOW;
				break;
			case COB_SCREEN_WHITE:
				fgcolor = COLOR_WHITE;
				break;
			default:
				break;
			}
		}
		if (bgc) {
			switch (cob_get_int (bgc)) {
			case COB_SCREEN_BLACK:
				bgcolor = COLOR_BLACK;
				break;
			case COB_SCREEN_BLUE:
				bgcolor = COLOR_BLUE;
				break;
			case COB_SCREEN_GREEN:
				bgcolor = COLOR_GREEN;
				break;
			case COB_SCREEN_CYAN:
				bgcolor = COLOR_CYAN;
				break;
			case COB_SCREEN_RED:
				bgcolor = COLOR_RED;
				break;
			case COB_SCREEN_MAGENTA:
				bgcolor = COLOR_MAGENTA;
				break;
			case COB_SCREEN_YELLOW:
				bgcolor = COLOR_YELLOW;
				break;
			case COB_SCREEN_WHITE:
				bgcolor = COLOR_WHITE;
				break;
			default:
				break;
			}
		}
		for (i = 0; i < (int)COLOR_PAIRS; i++) {
			pair_content ((short)i, &fgdef, &bgdef);
			if (fgdef == fgcolor && bgdef == bgcolor) {
				break;
			}
			if (fgdef == 0 && bgdef == 0) {
				init_pair ((short)i, fgcolor, bgcolor);
				break;
			}
		}
		if (i != (int)COLOR_PAIRS) {
#ifdef	HAVE_COLOR_SET
			color_set ((short)COLOR_PAIR(i), NULL);
#else
			attrset (COLOR_PAIR(i));
#endif
			bkgdset (COLOR_PAIR(i));
		} else {
			if (!styles) {
				attrset (A_NORMAL);
			}
		}
	}
	/* BLANK SCREEN colors the whole screen. */
	if (attr & COB_SCREEN_BLANK_SCREEN) {
		getyx (stdscr, line, column);
		clear ();
		cob_move_cursor (line, column);
	}

	if (stmt == DISPLAY_STATEMENT) {
		/* BLANK LINE colors the whole line. */
		if (attr & COB_SCREEN_BLANK_LINE) {
			getyx (stdscr, line, column);
			cob_move_cursor (line, 0);
			clrtoeol ();
			cob_move_cursor (line, column);
		}
		if (attr & COB_SCREEN_ERASE_EOL) {
			clrtoeol ();
		}
		if (attr & COB_SCREEN_ERASE_EOS) {
			clrtobot ();
		}
	}
	if (attr & COB_SCREEN_BELL) {
		cob_beep ();
	}
}

static void
cob_screen_init (void)
{
#ifdef	HAVE_LIBPDCURSES
	size_t	i;
#endif

	if (cobglobptr->cob_screen_initialized) {
		return;
	}

	cob_base_inp = NULL;
	totl_index = 0;
	cob_has_color = 0;
	global_return = 0;
	cob_current_y = 0;
	cob_current_x = 0;
	fore_color = 0;
	back_color = 0;
	display_cursor_y = 0;
	display_cursor_x = 0;
	accept_cursor_y = 0;
	accept_cursor_x = 0;
	pending_accept = 0;
	got_sys_char = 0;

	fflush (stdout);
	fflush (stderr);

#if	0	/* RXWRXW sigtin */
#ifndef _WIN32
	/* If the process is backgrounded (running non interactively), */
	/* SIGTTIN or SIGTOU is emitted. If not caught, turns into a SIGSTP */
#ifdef	SIGTTIN
	signal(SIGTTIN, SIG_IGN);
#endif
#ifdef	SIGTTOU
	signal(SIGTTOU, SIG_IGN);
#endif
#endif
#endif

#if	0	/* RXWRXW - setlocale */
#ifdef	HAVE_SETLOCALE
	if (cobglobptr->cob_locale_orig) {
		setlocale (LC_ALL, cobglobptr->cob_locale_orig);
	}
	if (cobglobptr->cob_locale_ctype) {
		setlocale (LC_CTYPE, cobglobptr->cob_locale_ctype);
	}
#endif
#endif

	if (!initscr ()) {
		cob_runtime_error (_("failed to initialize curses"));
		/* FIXME: likely should raise an exception instead */
		cob_stop_run (1);
	}
	cobglobptr->cob_screen_initialized = 1;
#ifdef	HAVE_USE_LEGACY_CODING
	use_legacy_coding (2);
#endif

#if	0	/* RXWRXW - setlocale */
#ifdef	HAVE_SETLOCALE
	if (cobglobptr->cob_locale) {
		setlocale (LC_ALL, cobglobptr->cob_locale);
		setlocale (LC_CTYPE, "C");
	}
#endif
#endif

	cbreak ();
	keypad (stdscr, 1);
	nonl ();
	noecho ();
	if (has_colors ()) {
		start_color ();
		pair_content ((short)0, &fore_color, &back_color);
		if (COLOR_PAIRS) {
#ifdef	HAVE_LIBPDCURSES
			/* pdcurses sets ALL pairs to default fg/bg */
			/* IMHO a bug. */
			for (i = 1; i < (size_t)COLOR_PAIRS; ++i) {
				init_pair ((short)i, 0, 0);
			}
#endif
			cob_has_color = 1;
		}
	}
	attrset (A_NORMAL);
	getmaxyx (stdscr, COB_MAX_Y_COORD, COB_MAX_X_COORD);

	/* Depending on insert mode set vertical bar cursor (on)
	   or square cursor (off) - note: the cursor change may has no
	   effect in all curses implementations / terminals */
	if (cobsetptr->cob_insert_mode == 0) {
		(void)curs_set(2);	/* set square cursor */
	} else {
		(void)curs_set(1);	/* set vertical bar cursor */
	}
}

static void
cob_check_pos_status (const int fret)
{
	cob_field	*f;
	int		sline;
	int		scolumn;
	char		buff[23]; /* 10: make the compiler happy as "int" *could*
						         have more digits than we "assume" */

	if (fret) {
		cob_set_exception (COB_EC_IMP_ACCEPT);
	}
	COB_ACCEPT_STATUS = fret;
	if (!COB_MODULE_PTR) {
		return;
	}
	if (COB_MODULE_PTR->crt_status) {
		if (COB_FIELD_IS_NUMERIC (COB_MODULE_PTR->crt_status)) {
			cob_set_int (COB_MODULE_PTR->crt_status, fret);
		} else {
			sprintf(buff, "%4.4d", fret);
			memcpy (COB_MODULE_PTR->crt_status->data, buff,
				(size_t)4);
		}
	}
	if (COB_MODULE_PTR->cursor_pos) {
		getyx (stdscr, sline, scolumn);
		f = COB_MODULE_PTR->cursor_pos;
		if (COB_FIELD_IS_NUMERIC (f) &&
		    COB_FIELD_TYPE (f) != COB_TYPE_NUMERIC_DISPLAY) {
			sline *= 1000;
			sline += scolumn;
			cob_set_int (f, sline);
		} else {
			if (f->size < 6) {
				sline *= 100;
				sline += scolumn;
				sprintf (buff, "%4.4d", sline);
				memcpy (f->data, buff, (size_t)4);
			} else {
				sline *= 1000;
				sline += scolumn;
				sprintf (buff, "%6.6d", sline);
				memcpy (f->data, buff, (size_t)6);
			}
		}
	}
}

static void
raise_ec_on_truncation (const int item_size)
{
	int	y;
	int	x;
	int	max_y;
	int	max_x;

	COB_UNUSED (y);
	COB_UNUSED (max_y);

	getyx (stdscr, y, x);
	getmaxyx (stdscr, max_y, max_x);

	if (x + item_size - 1 > max_x) {
		cob_set_exception (COB_EC_SCREEN_ITEM_TRUNCATED);
	}
}

static void
cob_addnstr (const char *data, const int size)
{
	raise_ec_on_truncation (size);
	addnstr (data, size);
}

static void
cob_addch (const chtype c)
{
	raise_ec_on_truncation (1);
	addch (c);
}

/* Use only when raise_ec_on_truncation is called beforehand. */
static void
cob_addch_no_trunc_check (const chtype c)
{
	addch (c);
}

static void
cob_addnch (const int n, const chtype c)
{
	int	count;

	raise_ec_on_truncation (n);
	for (count = 0; count < n; count++) {
		cob_addch_no_trunc_check (c);
	}
}

static int
is_first_screen_item (cob_screen *s)
{
	do {
		if (s->prev) {
			return 0;
		}
		s = s->parent;
	} while (s);

	return 1;
}

static cob_screen *
get_last_child (cob_screen * const s)
{
	cob_screen	*child;

	for (child = s->child; child->next; child = child->next);

	if (child->child) {
		return get_last_child (child);
	} else {
		return child;
	}
}

static cob_screen *
get_prev_screen_item (cob_screen * const s)
{
	if (s->prev) {
		if (s->prev->child) {
			return get_last_child (s->prev);
		} else {
			return s->prev;
		}
	} else if (s->parent) {
		return s->parent;
	} else {
		return NULL;
	}
}


#define UPDATE_CLAUSE_FUNC(clause_name_upper, clause_name_lower)	\
	static void							\
	update_##clause_name_lower (cob_screen *s, int * const count,	\
				    int * const found_clause)		\
	{								\
		if (s->attr & COB_SCREEN_##clause_name_upper##_PLUS) {	\
			*count += cob_get_int (s->clause_name_lower);	\
		} else if (s->attr & COB_SCREEN_##clause_name_upper##_MINUS) { \
			*count -= cob_get_int (s->clause_name_lower);	\
		} else {						\
			*count += cob_get_int (s->clause_name_lower) - 1; \
			*found_clause = 1;				\
		}							\
	}

UPDATE_CLAUSE_FUNC (LINE, line)
UPDATE_CLAUSE_FUNC (COLUMN, column)

#undef UPDATE_CLAUSE_FUNC

static size_t
get_size (cob_screen *s)
{
	if (s->field) {
		return s->field->size;
	} else { /* s->value */
		return s->value->size;
	}

}
static void
get_screen_item_line_and_col (cob_screen * s, int * const line,
			      int * const col)
{
	int		found_line = 0;
	int		found_col = 0;
	int	        is_screen_to_display = 1;
	int		is_parent;

	*line = 0;
	*col = 0;

	for (; s; s = get_prev_screen_item (s)) {
		if (s->line) {
			if (!found_line) {
				update_line (s, line, &found_line);
			}

			if (!s->column) {
				found_col = 1;
			}
		}

		if (!found_col) {
			is_parent = !!s->child;

			if (!is_screen_to_display && !is_parent) {
				*col += get_size (s) - 1;
			}

			if (s->column) {
				update_column (s, col, &found_col);
			}

			if (!s->column && !is_parent && !is_first_screen_item (s)) {
				/*
				  Note that parents are excluded; the standard
				  assumes COL + 1, unless otherwise specified,
				  on all screen items. This seems silly on group
				  items, hence why this non-standard extension.
				*/
				++(*col);
			}
		}

	        is_screen_to_display = 0;
	}

	*line += origin_y;
	*col += origin_x;
}

static void
cob_screen_puts (cob_screen *s, cob_field *f, const cob_u32_t is_input,
		 const enum screen_statement stmt)
{
	unsigned char	*p;
	size_t		size;
	int		line;
	int		column;
	chtype		promptchar;

	get_screen_item_line_and_col (s, &line, &column);

	/* Move to specified position */
	cob_move_cursor (line, column);
	cob_current_y = line;
	cob_current_x = column;
#if	0	/* RXWRXW - Attr */
	cob_screen_attr (s->foreg, s->backg, s->attr);
#endif
	if (s->attr & COB_SCREEN_INPUT) {
		cob_screen_attr (s->foreg, s->backg, s->attr, stmt);
		if (s->prompt) {
			promptchar = s->prompt->data[0];
		} else {
			promptchar = COB_CH_UL;
		}
		p = f->data;
		raise_ec_on_truncation (f->size);
		for (size = 0; size < f->size; size++, p++) {
			if (s->attr & COB_SCREEN_SECURE) {
				cob_addch_no_trunc_check (COB_CH_AS);
			} else if (*p <= ' ') {
				cob_addch_no_trunc_check (promptchar);
			} else {
				cob_addch_no_trunc_check ((const chtype)*p);
			}
		}
	} else if (!is_input) {
		cob_screen_attr (s->foreg, s->backg, s->attr, stmt);
		cob_addnstr ((char *)f->data, (int)f->size);
	} else {
		column += (int)f->size;
		cob_move_cursor (line, column);
	}

	if (stmt == DISPLAY_STATEMENT) {
		display_cursor_y = line;
		display_cursor_x = column + f->size;
	} else { /* ACCEPT_STATEMENT */
		accept_cursor_y = line;
		accept_cursor_x = column + f->size;
	}

	refresh ();
}

static COB_INLINE COB_A_INLINE int
cob_field_is_numeric_or_numeric_edited (cob_field *field)
{
	return (COB_FIELD_IS_NUMERIC (field)
		|| COB_FIELD_TYPE (field) == COB_TYPE_NUMERIC_EDITED);
}

static int
field_is_empty (cob_screen *s)
{
	unsigned char	*data = s->field->data;
	size_t		size = s->field->size;
	size_t		i;

	for (i = 0; i < size; ++i) {
		if (!isspace (data[i])) {
			return 0;
		}
	}

	return 1;
}

static int
field_is_zero (cob_screen *s)
{
	unsigned char	*data = s->field->data;
	size_t		size = s->field->size;
	size_t		i;
	unsigned char	decimal_point = COB_MODULE_PTR->decimal_point;

	for (i = 0; i < size; ++i) {
		if (!(isspace (data[i]) || data[i] == '0'
		      || data[i] == decimal_point)) {
			return 0;
		}
	}

	return 1;
}

static int
pic_has_zero_suppression (const cob_pic_symbol *pic)
{
	int	i;

	for (i = 0; pic[i].symbol != '\0'; ++i) {
		/*
		  NB: + and - are floating-insertion editing characters, not
		  zero-suppression ones.
		*/
		if (pic[i].symbol == 'Z' || pic[i].symbol == '*') {
			return 1;
		}
	}

	return 0;
}

static int
get_num_int_digits_for_no_zero_sup (const cob_pic_symbol *pic)
{
	int	i;
	int	num_digits = 0;
	char	numeric_separator = COB_MODULE_PTR->numeric_separator;

	for (i = 0; pic[i].symbol != '\0'; ++i) {
		if (pic[i].symbol == '9'
		    || pic[i].symbol == 'Z'
		    || pic[i].symbol == '*') {
			num_digits += pic[i].times_repeated;
		} else if (!(pic[i].symbol == numeric_separator
			     || pic[i].symbol == 'B'
			     || pic[i].symbol == '0'
			     || pic[i].symbol == '/')
			   && num_digits != 0) {
			break;
		}
	}

	return num_digits;
}

static int
field_is_zero_or_no_zero_suppression (cob_screen *s)
{
	const cob_pic_symbol	*pic = COB_FIELD_PIC (s->field);
	size_t		i;
	size_t		size = COB_FIELD_SIZE (s->field);
	unsigned char	*data = COB_FIELD_DATA (s->field);
	int		num_integer_digits;
	int		num_digits_seen = 0;

	if (field_is_zero (s) || !pic_has_zero_suppression (pic)) {
		return 1;
	}

	num_integer_digits = get_num_int_digits_for_no_zero_sup (pic);

	/*
	  Verify there are sufficient non-zero digits before a decimal
	  point/the end to fill the integer part of the field.
	*/
	for (i = 0; i < size; ++i) {
		if (isdigit (data[i])) {
			if (data[i] != '0' || num_digits_seen != 0) {
				++num_digits_seen;
			}
		} else if (!isspace (data[i]) && num_digits_seen != 0) {
			break;
		}
	}

	return num_digits_seen >= num_integer_digits;
}

/* Assuming s->field is alphanumeric */
static int
field_is_full (cob_screen *s)
{
	unsigned char	*data = s->field->data;
	size_t		size = s->field->size;

	/* Per the standard, only the first and last chars need be non-space. */
	return !isspace (*data) && !isspace (*(data + size - 1));
}

static int
satisfied_full_clause (cob_screen *s)
{
	if (!(s->attr & COB_SCREEN_FULL)) {
		return 1;
	}

	if (COB_FIELD_IS_NUMERIC (s->field)) {
		return !field_is_zero (s);
	} else if (COB_FIELD_TYPE (s->field) == COB_TYPE_NUMERIC_EDITED) {
		return field_is_zero_or_no_zero_suppression (s);
	} else { /* field is alphanumeric */
		return field_is_full (s) || field_is_empty (s);
	}
}

static int
satisfied_required_clause (cob_screen *s)
{
	if (!(s->attr & COB_SCREEN_REQUIRED)) {
		return 1;
	}

	if (cob_field_is_numeric_or_numeric_edited (s->field)) {
		return !field_is_zero (s);
	} else { /* field is alphanumeric */
		return !field_is_empty (s);
	}
}

static int
valid_field_data (cob_field *field)
{
	if (COB_FIELD_IS_NUMERIC (field)) {
		return cob_check_numval (field, NULL, 0, 0) == 0;
	} else if (field->attr->type == COB_TYPE_NUMERIC_EDITED) {
		return cob_check_numval (field, NULL, 1, 0) == 0;
	} else {
		return 1;
	}
}

static void
refresh_field (cob_screen *s)
{
	int		y;
	int		x;

	getyx (stdscr, y, x);
	cob_screen_puts (s, s->field, cobsetptr->cob_legacy, ACCEPT_STATEMENT);
	cob_move_cursor (y, x);
}

static void
format_field (cob_screen *s)
{
	cob_field	field;
	size_t		size = s->field->size;
	unsigned char	*data;

	/*
	  We copy the data into another field and move it back to format the
	  numeric data neatly, rather than re-implement that logic here. We
	  assume the data is valid.
	*/
	data = cob_malloc (size);
	memcpy (data, s->field->data, size);
	COB_FIELD_INIT (size, data, s->field->attr);

	if (COB_FIELD_IS_NUMERIC (s->field)) {
		cob_move (cob_intr_numval (&field), s->field);
	} else if (field.attr->type == COB_TYPE_NUMERIC_EDITED) {
		cob_move (cob_intr_numval_c (&field, NULL), s->field);
	}

	cob_free (data);

	refresh_field (s);
}

/* Finalize field on leaving it: checks and conversions */
static int
finalize_field_input (cob_screen *s)
{
	/* Only numeric types need to be validated and formatted. */
	if (cob_field_is_numeric_or_numeric_edited (s->field)) {
		if (!valid_field_data (s->field)) {
			return 1;
		}
		format_field (s);
	}

	if (!satisfied_full_clause (s) || !satisfied_required_clause (s)) {
		return 1;
	}

	return 0;
}

static int
finalize_all_fields (struct cob_inp_struct *sptr, const size_t total_idx)
{
	const struct cob_inp_struct *end = sptr + total_idx;

	for (; sptr < end; ++sptr) {
		if (finalize_field_input (sptr->scr)) {
			return 1;
		}
	}

	return 0;
}

static void
cob_screen_get_all (const int initial_curs, const int gettimeout)
{
	size_t			curr_index = (size_t)initial_curs;
	struct cob_inp_struct	*sptr = cob_base_inp + curr_index;
	cob_screen		*s = sptr->scr;
	unsigned char		*p = s->field->data;
	unsigned char		*p2;
	unsigned char		move_char;
	int			keyp;
	int			sline = sptr->this_y;
	int			scolumn = sptr->this_x;
	int			cline;
	int			ccolumn;
	int			rightpos = scolumn + (int)s->field->size - 1;
	int			ateof = 0;
	int			ungetched = 0;
	int			status;
	int			count;
	chtype			promptchar;

	status = cob_move_cursor (sline, scolumn);
	if (status != ERR) {
		pending_accept = 0;
	}
	cob_screen_attr (s->foreg, s->backg, s->attr, ACCEPT_STATEMENT);

	for (; ;) {
		if (s->prompt) {
			promptchar = s->prompt->data[0];
		} else {
			promptchar = COB_CH_UL;
		}

		refresh ();
		errno = 0;
		timeout (gettimeout);
		keyp = getch ();

		/* FIXME: modularize (cob_screen_get_all, field_accept) and
		          use identical handling of keys wherever possible */

		if (keyp == ERR) {
			global_return = 8001;
			goto screen_return;
		}
		if (keyp > KEY_F0 && keyp < KEY_F(65)) {
			global_return = 1000 + keyp - KEY_F0;
			goto screen_return;
		}

		cob_convert_key (&keyp, 0);
		if (keyp <= 0) {
			(void)flushinp ();
			cob_beep ();
			continue;
		}

		getyx (stdscr, cline, ccolumn);

		switch (keyp) {
		case KEY_ENTER:
			if (finalize_all_fields (cob_base_inp, totl_index)) {
				cob_beep ();
				continue;
			}
			goto screen_return;
		case KEY_PPAGE:
			global_return = 2001;
			goto screen_return;
		case KEY_NPAGE:
			global_return = 2002;
			goto screen_return;
		case KEY_PRINT:
			global_return = 2006;
			goto screen_return;
		case '\033':
			global_return = 2005;
			goto screen_return;
		case KEY_STAB:
			finalize_field_input (s);

			if (curr_index < totl_index - 1) {
				curr_index++;
			} else {
				curr_index = 0;
			}
			sptr = cob_base_inp + curr_index;
			s = sptr->scr;
			sline = sptr->this_y;
			scolumn = sptr->this_x;
			ateof = 0;
			rightpos = scolumn + (int)s->field->size - 1;
			p = s->field->data;
			cob_move_cursor (sline, scolumn);
			cob_screen_attr (s->foreg, s->backg, s->attr,
					 ACCEPT_STATEMENT);
			continue;
		case KEY_BTAB:
			finalize_field_input (s);

			if (curr_index > 0) {
				curr_index--;
			} else {
				curr_index = totl_index - 1;
			}
			sptr = cob_base_inp + curr_index;
			s = sptr->scr;
			sline = sptr->this_y;
			scolumn = sptr->this_x;
			ateof = 0;
			rightpos = scolumn + (int)s->field->size - 1;
			if (ungetched) {
				ungetched = 0;
				p = s->field->data + rightpos;
				cob_move_cursor (sline, rightpos);
			} else {
				p = s->field->data;
				cob_move_cursor (sline, scolumn);
			}
			cob_screen_attr (s->foreg, s->backg, s->attr,
					 ACCEPT_STATEMENT);
			continue;
		case KEY_UP:
			finalize_field_input (s);

			curr_index = sptr->up_index;
			sptr = cob_base_inp + curr_index;
			s = sptr->scr;
			sline = sptr->this_y;
			scolumn = sptr->this_x;
			ateof = 0;
			rightpos = scolumn + (int)s->field->size - 1;
			p = s->field->data;
			cob_move_cursor (sline, scolumn);
			cob_screen_attr (s->foreg, s->backg, s->attr,
					 ACCEPT_STATEMENT);
			continue;
		case KEY_DOWN:
			finalize_field_input (s);

			curr_index = sptr->down_index;
			sptr = cob_base_inp + curr_index;
			s = sptr->scr;
			sline = sptr->this_y;
			scolumn = sptr->this_x;
			ateof = 0;
			rightpos = scolumn + (int)s->field->size - 1;
			p = s->field->data;
			cob_move_cursor (sline, scolumn);
			cob_screen_attr (s->foreg, s->backg, s->attr,
					 ACCEPT_STATEMENT);
			continue;
		case KEY_HOME:
			finalize_field_input (s);

			curr_index = 0;
			sptr = cob_base_inp;
			s = sptr->scr;
			sline = sptr->this_y;
			scolumn = sptr->this_x;
			ateof = 0;
			rightpos = scolumn + (int)s->field->size - 1;
			p = s->field->data;
			cob_move_cursor (sline, scolumn);
			cob_screen_attr (s->foreg, s->backg, s->attr,
					 ACCEPT_STATEMENT);
			continue;
		case KEY_END:
			finalize_field_input (s);

			curr_index = totl_index - 1;
			sptr = cob_base_inp + curr_index;
			s = sptr->scr;
			sline = sptr->this_y;
			scolumn = sptr->this_x;
			ateof = 0;
			rightpos = scolumn + (int)s->field->size - 1;
			p = s->field->data;
			cob_move_cursor (sline, scolumn);
			cob_screen_attr (s->foreg, s->backg, s->attr,
					 ACCEPT_STATEMENT);
			continue;
		case KEY_BACKSPACE:
			/* Backspace key. */
			if ((int) ccolumn > scolumn) {
				ateof = 0;
				/* Shift remainder left with cursor. */
				for (count = ccolumn; count < rightpos + 1; count++) {
					/* Get character. */
					p2 = s->field->data + count - scolumn ;
					move_char = *p2;
					/* Move the character left. */
					p2 = s->field->data + count - scolumn - 1;
					*p2 = move_char;
					/* Update screen with moved character. */
					cob_move_cursor (cline, count - 1);
					if (s->attr & COB_SCREEN_NO_ECHO) {
						cob_addch (COB_CH_SP);
					} else if (s->attr & COB_SCREEN_SECURE) {
						cob_addch (COB_CH_AS);
					} else if (move_char == ' ') {
						cob_addch (promptchar);
					} else {
						cob_addch (move_char);
					}
				}
				/* Put space as the right most character. */
				p2 = s->field->data + s->field->size - 1;
				if (COB_FIELD_IS_NUMERIC (s->field)) {
					*p2 = '0';
				} else {
					*p2 = ' ';
				}
				/* Add space to screen. */
				cob_move_cursor (cline, count - 1);
				if (s->attr & COB_SCREEN_NO_ECHO) {
					cob_addch (COB_CH_SP);
				} else if (s->attr & COB_SCREEN_SECURE) {
					cob_addch (COB_CH_AS);
				} else if (*p2 == ' ') {
					cob_addch (promptchar);
				} else {
					cob_addch (*p2);
				}
				/* Move cursor left one from current. */
				ccolumn--;
				cob_move_cursor (cline, ccolumn);
				p--;
			} else {
				ungetched = 1;
				ungetch (KEY_BACKSPACE);
				ungetch (KEY_BTAB);
			}
			continue;
		case KEY_LEFT:
			if (ccolumn > scolumn) {
				ccolumn--;
				cob_move_cursor (cline, ccolumn);
				p = s->field->data + ccolumn - scolumn;
			} else {
				ungetched = 1;
				ungetch (KEY_BTAB);
			}
			continue;
		case KEY_RIGHT:
			if (ccolumn < rightpos) {
				ccolumn++;
				cob_move_cursor (cline, ccolumn);
				p = s->field->data + ccolumn - scolumn;
			} else {
				ungetch ('\t');
			}
			continue;
		case KEY_IC:
			/* Insert key toggle */
			/* If off turn on, if on turn off;
			additional: switch between vertical bar cursor (on) and
			square cursor (off) - note: the cursor change may has no
			effect in all curses implementations / terminals */
			if (cobsetptr->cob_insert_mode == 0) {
				cobsetptr->cob_insert_mode = 1;     /* on */
				(void)curs_set(1);	/* switch to vertical bar cursor */
			} else {
				cobsetptr->cob_insert_mode = 0;     /* off */
				(void)curs_set(2);	/* switch to square cursor */
			}
			continue;
		case KEY_DC:
			/* Delete key. */
			/* Delete character, move remainder left. */
			for (count = ccolumn; count < rightpos; count++) {
				/* Get character one position to right. */
				p2 = s->field->data + count - scolumn + 1;
				move_char = *p2;
				/* Move the character left. */
				p2 = s->field->data + count - scolumn;
				*p2 = move_char;
				/* Update screen with moved character. */
				cob_move_cursor (cline, count);
				if (s->attr & COB_SCREEN_NO_ECHO) {
					cob_addch (COB_CH_SP);
				} else if (s->attr & COB_SCREEN_SECURE) {
					cob_addch (COB_CH_AS);
				} else if (move_char == ' ') {
					cob_addch (promptchar);
				} else {
					cob_addch (move_char);
				}
			}
			/* Put space as the right most character. */
			p2 = s->field->data + s->field->size - 1;
			if (COB_FIELD_IS_NUMERIC (s->field)) {
				*p2 = '0';
			} else {
				*p2 = ' ';
			}
			/* Add space to screen. */
			cob_move_cursor (cline, count);
			if (s->attr & COB_SCREEN_NO_ECHO) {
				cob_addch (COB_CH_SP);
			} else if (s->attr & COB_SCREEN_SECURE) {
				cob_addch (COB_CH_AS);
			} else {
				if (*p2 == ' ') {
					cob_addch (promptchar);
				} else {
					cob_addch (*p2);
				}
			}
			/* Put cursor back to original position. */
			cob_move_cursor (cline, ccolumn);
			continue;
		default:
			break;
		}

		/* Handle printable character. */
#if 0 /* FIXME: we can't handle anything > UCHAR_MAX here because of
		*p = (unsigned char) keyp;
		--> revise */
		if (keyp > 037 && keyp < (int)A_CHARTEXT) {
#else
		if (keyp > 037 && keyp <= UCHAR_MAX) {
#endif
			/* Numeric field check. */
			if (cob_field_is_numeric_or_numeric_edited (s->field)) {
				if (keyp < '0' || keyp > '9') {
					cob_beep ();
					continue;
				}
			}

			/* Handle UPPER/LOWER. */
			if (s->attr & COB_SCREEN_UPPER) {
				if (islower (keyp)) {
					keyp = toupper (keyp);
				}
			} else if (s->attr & COB_SCREEN_LOWER) {
				if (isupper (keyp)) {
					keyp = tolower (keyp);
				}
			}

			/* Insert character, if requested. */
			if (cobsetptr->cob_insert_mode == 1) {
				/* get last character in field */
				/* check and beep if field is already full,
				ignore numeric fields for now */
				if (cob_field_is_numeric_or_numeric_edited (s->field)) {
					p2 = (unsigned char *)" ";
				} else {
					p2 = s->field->data + rightpos - scolumn;
				}
				if (*p2 != ' ') {
					cob_beep ();
					continue;
				}
				/* Move remainder to the right. */
				for (count = rightpos; count > ccolumn - 1; count--) {
					/* Get character */
					p2 = s->field->data + count - scolumn - 1;
					move_char = *p2;
					/* Move character one right. */
					p2 = s->field->data + count - scolumn;
					*p2 = move_char;
					/* Update screen with moved character. */
					if ((int) count > scolumn) {
						cob_move_cursor (cline, count);
						if (move_char != ' ') {
							if (s->attr & COB_SCREEN_NO_ECHO) {
								cob_addch (COB_CH_SP);
							} else if (s->attr & COB_SCREEN_SECURE) {
								cob_addch (COB_CH_AS);
							} else {
								cob_addch (move_char);
							}
						}
					}
				}
				cob_move_cursor (cline, ccolumn);
			}

			/* actual storing the key */
			*p = (unsigned char) keyp;

			/* Display character or '*' if secure. */
			if (s->attr & COB_SCREEN_SECURE) {
				cob_addch (COB_CH_AS);
			} else if (s->attr & COB_SCREEN_NO_ECHO) {
				cob_addch (COB_CH_SP);
			} else {
				cob_addch ((const chtype)keyp);
			}
			if (ccolumn == rightpos) {
				/* Auto-skip at end of field. */
				if (s->attr & COB_SCREEN_AUTO) {
					if (curr_index == totl_index - 1) {
						goto screen_return;
					} else {
						ungetch ('\t');
					}
				}
				cob_move_cursor (cline, ccolumn);
				/* check if we (still) are at last position and inform
				   user with a beep (after having processed his key) */
				if (ateof) {
					cob_beep ();
				} else {
					ateof = 1;
				}
			} else {
				p++;
			}
			continue;
		}
		(void)flushinp ();
		cob_beep ();
	}
screen_return:
	refresh ();
}

static int
compare_yx (const void *m1, const void *m2)
{
	const struct cob_inp_struct	*s1;
	const struct cob_inp_struct	*s2;

	s1 = m1;
	s2 = m2;
	if (s1->this_y < s2->this_y) {
		return -1;
	}
	if (s1->this_y > s2->this_y) {
		return 1;
	}
	if (s1->this_x < s2->this_x) {
		return -1;
	}
	if (s1->this_x > s2->this_x) {
		return 1;
	}
	return 0;
}

static void
cob_screen_moveyx (cob_screen *s)
{
	int	y;
	int	x;
	int	line;
	int	column;

	if (s->line || s->column ||
	    s->attr & (COB_SCREEN_LINE_PLUS | COB_SCREEN_LINE_MINUS |
		       COB_SCREEN_COLUMN_PLUS |COB_SCREEN_COLUMN_MINUS)) {
		getyx (stdscr, y, x);
#if	1	/* RXWRXW - Column adjust */
		if (x > 0) {
			x--;
		}
#endif
		if (!s->line) {
			line = y;
		} else {
			line = origin_y + cob_get_int (s->line);
			if (line < 0) {
				line = y;
			}
		}
		if (!s->column) {
			column = x;
		} else {
			column = origin_x + cob_get_int (s->column);
			if (column < 0) {
				column = x;
			}
		}
		if (s->attr & COB_SCREEN_LINE_PLUS) {
			line = y + line;
		} else if (s->attr & COB_SCREEN_LINE_MINUS) {
			line = y - line;
		}
		if (s->attr & COB_SCREEN_COLUMN_PLUS) {
			column = x + column;
		} else if (s->attr & COB_SCREEN_COLUMN_MINUS) {
			column = x - column;
		}

		cob_move_cursor (line, column);
		refresh ();
		cob_current_y = line;
		cob_current_x = column;
	}
}

static size_t
cob_prep_input (cob_screen *s)
{
	struct cob_inp_struct	*sptr;
	int			n;

	switch (s->type) {
	case COB_SCREEN_TYPE_GROUP:
		cob_screen_moveyx (s);
		for (s = s->child; s; s = s->next) {
			cob_prep_input (s);
		}
		break;
	case COB_SCREEN_TYPE_FIELD:
		cob_screen_puts (s, s->field, cobsetptr->cob_legacy,
				 ACCEPT_STATEMENT);
		if (s->attr & COB_SCREEN_INPUT) {
			if (totl_index >= COB_INP_FLD_MAX) {
				return 1;
			}
			sptr = cob_base_inp + totl_index;
			sptr->scr = s;
			sptr->this_y = cob_current_y;
			sptr->this_x = cob_current_x;
			totl_index++;
		}
		break;
	case COB_SCREEN_TYPE_VALUE:
		cob_screen_puts (s, s->value, cobsetptr->cob_legacy,
				 ACCEPT_STATEMENT);
		if (s->occurs) {
			for (n = 1; n < s->occurs; ++n) {
				cob_screen_puts (s, s->value, cobsetptr->cob_legacy,
						 ACCEPT_STATEMENT);
			}
		}
		break;
	case COB_SCREEN_TYPE_ATTRIBUTE:
#if	0	/* RXWRXW - Attr */
		cob_screen_attr (s->foreg, s->backg, s->attr);
#endif
		break;
	default:
		break;
	}
	return 0;
}

static void
cob_screen_iterate (cob_screen *s)
{
	int	n;

	switch (s->type) {
	case COB_SCREEN_TYPE_GROUP:
		cob_screen_moveyx (s);
		for (s = s->child; s; s = s->next) {
			cob_screen_iterate (s);
		}
		break;
	case COB_SCREEN_TYPE_FIELD:
		cob_screen_puts (s, s->field, 0, DISPLAY_STATEMENT);
		break;
	case COB_SCREEN_TYPE_VALUE:
		cob_screen_puts (s, s->value, 0, DISPLAY_STATEMENT);
		if (s->occurs) {
			for (n = 1; n < s->occurs; ++n) {
				cob_screen_puts (s, s->value, 0,
						 DISPLAY_STATEMENT);
			}
		}
		break;
	case COB_SCREEN_TYPE_ATTRIBUTE:
		cob_screen_attr (s->foreg, s->backg, s->attr,
				 DISPLAY_STATEMENT);
		break;
	default:
		break;
	}
}

static void
get_line_and_col_from_num (cob_field *pos, int *line, int *column)
{
	int	pos_val = cob_get_int (pos);
	int	max_line_column;

	/*
	  This is used when only a LINE clause is specified, not an AT clause.
	*/
	if (pos->size < 4) {
		*line = pos_val;
		*column = 1;
		return;
	}

	if (pos->size == 4) {
		max_line_column = 100;
	} else if (pos->size == 6) {
		max_line_column = 1000;
	} else {
		/* Throw an exception? EC-SCREEN-IMP-LINE-VAR-LENGTH? */
		max_line_column = 1; /* set to some value that don't chrash */
	}
	*line = (pos_val / max_line_column);
	*column = (pos_val % max_line_column);
}

static void
get_line_column (cob_field *fline, cob_field *fcol, int *line, int *col)
{
	if (fline == NULL) {
		*line = 1;
	} else {
		*line = cob_get_int (fline);
	}

	if (fcol == NULL) {
		*col = 1;
	} else {
		*col = cob_get_int (fcol);
	}
}

static COB_INLINE COB_A_INLINE int
col_where_last_stmt_ended (const enum screen_statement stmt)
{
	return stmt == DISPLAY_STATEMENT ? display_cursor_x : accept_cursor_x;
}

static COB_INLINE COB_A_INLINE int
line_where_last_stmt_ended (const enum screen_statement stmt)
{
	return stmt == DISPLAY_STATEMENT ? display_cursor_y : accept_cursor_y;
}

static void
extract_line_and_col_vals (cob_field *line, cob_field *column,
			   const enum screen_statement stmt,
			   const int zero_line_col_allowed,
			   int *sline, int *scolumn)
{
	int cobol_line;
	int cobol_col;

	if (column == NULL) {
		if (line == NULL) {
			*sline = 0;
			*scolumn = 0;
			return;
		} else {
			/*
			  line actually contains both the line and field
			  numbers.
			*/
			get_line_and_col_from_num (line, &cobol_line,
						   &cobol_col);
		}
	} else {
		get_line_column (line, column, &cobol_line, &cobol_col);
	}

	if (cobol_line == 0) {
		if (cobol_col == 0) {
			if (zero_line_col_allowed) {
				*sline = line_where_last_stmt_ended (stmt);
				*scolumn = col_where_last_stmt_ended (stmt);
			} else {
				cob_set_exception (COB_EC_SCREEN_LINE_NUMBER);
				cob_set_exception (COB_EC_SCREEN_STARTING_COLUMN);
				*sline = 0;
				*scolumn = 0;
			}
		} else {
			if (zero_line_col_allowed) {
				*sline = line_where_last_stmt_ended (stmt) + 1;
			} else {
				cob_set_exception (COB_EC_SCREEN_LINE_NUMBER);
				*sline = 0;
			}
			*scolumn = cobol_col - 1;
		}
	} else if (cobol_col == 0) {
		*sline = cobol_line - 1;
		if (zero_line_col_allowed) {
			*scolumn = col_where_last_stmt_ended (stmt);
		} else {
			cob_set_exception (COB_EC_SCREEN_STARTING_COLUMN);
			*scolumn = 0;
		}
	} else {
		*sline = cobol_line - 1;
		*scolumn = cobol_col - 1;
	}

	/* TO-DO: If scolumn == max_x + 1, go to start of next line */
}

static void
screen_display (cob_screen *s, const int line, const int column)
{
	int		status;
	init_cob_screen_if_needed ();

	origin_y = line;
	origin_x = column;

	status = cob_move_cursor (line, column);
	if (status != ERR) {
		pending_accept = 1;
	}
	cob_screen_iterate (s);
	refresh ();
}

static void
screen_accept (cob_screen *s, const int line, const int column,
	       cob_field *ftimeout)
{
	struct cob_inp_struct	*sptr;
	struct cob_inp_struct	*sptr2;
	size_t			idx;
	size_t			n;
	size_t			posu;
	size_t			posd;
	size_t			prevy;
	size_t			firsty;
	int			starty;
	int			initial_curs;
	int			gettimeout;

	init_cob_screen_if_needed ();
	if (!cob_base_inp) {
		cob_base_inp = cob_malloc (COB_INP_SIZE);
	} else {
		memset (cob_base_inp, 0, COB_INP_SIZE);
	}
	cobglobptr->cob_exception_code = 0;
	cob_current_y = 0;
	cob_current_x = 0;
	totl_index = 0;
	origin_y = line;
	origin_x = column;

	cob_move_cursor (line, column);

	/* Prepare input fields */
	if (cob_prep_input (s)) {
		cob_check_pos_status (9001);
		return;
	}

	/* No input field is an error */
	if (!totl_index) {
		cob_check_pos_status (8000);
		return;
	}

	if (ftimeout) {
		gettimeout = cob_get_int (ftimeout) * COB_TIMEOUT_SCALE;
		if (gettimeout >= 0 && gettimeout < 500) {
			gettimeout = 500;
		}
	} else {
		gettimeout = -1;
	}

	/* Sort input fields on line, column */
	qsort (cob_base_inp, totl_index,
	       sizeof(struct cob_inp_struct), compare_yx);

	posu = 0;
	posd = 0;
	prevy = 0;
	firsty = 0;
	sptr = cob_base_inp;
	starty = sptr->this_y;
	initial_curs = -1;
	/* Set up array for Cursor UP/DOWN */
	for (n = 0; n < totl_index; n++) {
		sptr = cob_base_inp + n;
		if ((sptr->scr->attr & COB_SCREEN_INITIAL) && initial_curs < 0) {
			initial_curs = (int)n;
		}
		if (sptr->this_y > starty) {
			if (!firsty) {
				firsty = n;
			}
			starty = sptr->this_y;
			sptr2 = cob_base_inp + posd;
			for (idx = posd; idx < n; idx++, sptr2++) {
				sptr2->down_index = n;
			}
			posu = prevy;
			prevy = n;
			posd = n;
		}
		sptr->up_index = posu;
	}
	sptr = cob_base_inp;
	for (n = 0; n < firsty; n++, sptr++) {
		sptr->up_index = posd;
	}
	global_return = 0;
	if (initial_curs < 0) {
		initial_curs = 0;
	}
	cob_screen_get_all (initial_curs, gettimeout);
	cob_check_pos_status (global_return);
}

static void
field_display (cob_field *f, const int line, const int column, cob_field *fgc,
	       cob_field *bgc, cob_field *fscroll, cob_field *size_is,
	       const cob_flags_t fattr)
{
	int		sline;
	int		scolumn;
	int		ssize_is = 0;	/* WITH SIZE IS */
	int		size_display;	/* final size to display */
	int		status;
	char	fig_const;	/* figurative constant character */

	/* LCOV_EXCL_START */
	if (unlikely (!f)) {
		cob_fatal_error(COB_FERROR_CODEGEN);
	}
	/* LCOV_EXCL_STOP */

	init_cob_screen_if_needed ();

	origin_y = 0;
	origin_x = 0;

	/* Field size to display */
	size_display = (int)f->size;
	/* WITH SIZE IS */
	if (size_is) {
		ssize_is = cob_get_int (size_is);
		/* Use WITH SIZE IS when less than field size */
		if (ssize_is > 0 && ssize_is < (int)f->size) {
			size_display = ssize_is;
		}
	} else if (fattr & COB_SCREEN_NO_DISP) {
		size_display = 0;
	}

	if (fscroll) {
		sline = cob_get_int (fscroll);
		if (fattr & COB_SCREEN_SCROLL_DOWN) {
			sline = -sline;
		}
		scrollok (stdscr, 1);
		scrl (sline);
		scrollok (stdscr, 0);
		refresh ();
	}

	sline = line;
	scolumn = column;
	status = cob_move_cursor (sline, scolumn);
	if (status != ERR) {
		pending_accept = 1;
	}

	cob_screen_attr (fgc, bgc, fattr, DISPLAY_STATEMENT);
	if (!(fattr & COB_SCREEN_NO_DISP)) {
		/* figurative constant and WITH SIZE repeats the character */
		if ((size_is)
		    && f->attr->type == COB_TYPE_ALPHANUMERIC_ALL
		    && (int)f->size == 1) {
			fig_const = f->data[0];
			cob_addnch (ssize_is, fig_const);
		} else {
			cob_addnstr ((char *)f->data, f->size);
			/* WITH SIZE larger than field displays trailing spaces */
			cob_addnch (size_display - f->size, COB_CH_SP);
		}
	}

	display_cursor_y = sline;
	display_cursor_x = scolumn + size_display;

	if (fattr & COB_SCREEN_EMULATE_NL) {
		if (++sline >= LINES) {
			sline = 0;
		}
		cob_move_cursor (sline, 0);
	}
	refresh ();
}

static void
field_accept (cob_field *f, const int sline, const int scolumn, cob_field *fgc,
	      cob_field *bgc, cob_field *fscroll, cob_field *ftimeout,
	      cob_field *prompt, cob_field *size_is, const cob_flags_t fattr)
{
	unsigned char	*p;
	unsigned char	*p2;
	size_t		count;
	int		keyp;
	int		fret;
	int		cline = 0;
	size_t		ccolumn = 0;
	size_t		rightpos;
	int		ateof;
	unsigned char	move_char;      /* data shift character */
	int		prompt_char;    /* prompt character */
	int		gettimeout;
	int		status;
	chtype		promptchar;
	int		ssize_is = 0;	/* WITH SIZE IS */
	size_t		size_accept = 0;	/* final size to accept */
	cob_field	temp;
#if	0	/* RXWRXW - Screen update */
	cob_field	char_temp;
	unsigned char	space_buff[4];
#endif

	memset (COB_TERM_BUFF, ' ', (size_t)COB_MEDIUM_MAX);
	temp.data = COB_TERM_BUFF;
	temp.attr = &const_alpha_attr;
#if	0	/* RXWRXW - Screen update */
	char_temp.data = space_buff;
	char_temp.attr = &const_alpha_attr;
	char_temp.size = 1;
	space_buff[0] = ' ';
	space_buff[1] = 0;
#endif

	origin_y = 0;
	origin_x = 0;

	/* Set the default prompt character */
	if (prompt) {
		promptchar = prompt->data[0];
	} else {
		promptchar = COB_CH_UL;
	}
	init_cob_screen_if_needed ();

	if (ftimeout) {
		gettimeout = cob_get_int (ftimeout) * COB_TIMEOUT_SCALE;
		if (gettimeout >= 0 && gettimeout < 500) {
			gettimeout = 500;
		}
	} else {
		gettimeout = -1;
	}

	if (fscroll) {
		keyp = cob_get_int (fscroll);
		if (fattr & COB_SCREEN_SCROLL_DOWN) {
			keyp = -keyp;
		}
		scrollok (stdscr, 1);
		scrl (keyp);
		scrollok (stdscr, 0);
		refresh ();
	}
	cobglobptr->cob_exception_code = 0;

	status = cob_move_cursor (sline, scolumn);
	if (status != ERR) {
		pending_accept = 0;
	}

	cob_screen_attr (fgc, bgc, fattr, ACCEPT_STATEMENT);

	if (f) {
		/* Field size to accept */
		size_accept = f->size;
		/* WITH SIZE IS */
		if (size_is) {
			ssize_is = cob_get_int (size_is);
			/* Use WITH SIZE IS when less than field size */
			if (ssize_is > 0 && ssize_is < (int)f->size) {
			  size_accept = ssize_is;
			}
		}

		p = COB_TERM_BUFF;
		temp.size = size_accept;
		if (fattr & COB_SCREEN_UPDATE) {
			cob_move (f, &temp);
		}
		/* SIZE IS greater than field, blank out trailing screen */
		if (ssize_is > (int)f->size) {
			cob_addnch (ssize_is, COB_CH_SP);
			cob_move_cursor (sline, scolumn);
		}
		raise_ec_on_truncation (size_accept);
		for (count = 0; count < size_accept; count++) {
			if (fattr & COB_SCREEN_SECURE) {
				cob_addch_no_trunc_check (COB_CH_AS);
			} else if (fattr & COB_SCREEN_NO_ECHO) {
				cob_addch_no_trunc_check (COB_CH_SP);
			} else if (fattr & COB_SCREEN_UPDATE) {
				fret = *p++;
				cob_addch_no_trunc_check ((const chtype)fret);
			} else if (COB_FIELD_IS_NUMERIC (f)) {
				cob_addch_no_trunc_check ('0');
			} else if (fattr & COB_SCREEN_PROMPT) {
				cob_addch_no_trunc_check (promptchar);
			} else {
				cob_addch_no_trunc_check (COB_CH_SP);
			}
		}
		cob_move_cursor (sline, scolumn);
#if	0	/* RXWRXW - Screen update */
		if (!(fattr & COB_SCREEN_UPDATE)) {
			if (cob_field_is_numeric_or_numeric_edited (f)) {
				cob_set_int (f, 0);
			} else {
				cob_move (&char_temp, f);
			}
		}
#endif

		accept_cursor_y = sline;
		accept_cursor_x = scolumn + size_accept;

		rightpos = scolumn + size_accept - 1;
		p = COB_TERM_BUFF;
	} else {
		rightpos = 0;
		p = NULL;
	}
	fret = 0;
	ateof = 0;
	count = 0;

	/* Get characters from keyboard, processing each one. */
	for (; ;) {
		if (f) {
			/* Get current line, column. */
			getyx (stdscr, cline, ccolumn);
			/* Trailing prompts. */
			if (fattr & COB_SCREEN_NO_ECHO) {
				prompt_char = COB_CH_SP;
			} else if (COB_FIELD_IS_NUMERIC (f)) {
				prompt_char = '0';
			} else if (fattr & COB_SCREEN_PROMPT) {
				prompt_char = promptchar;
			} else {
				prompt_char = COB_CH_SP;
			}
			for (count = rightpos; (int)count > scolumn - 1; count--) {
				/* Get character */
				p2 = COB_TERM_BUFF + count - scolumn;
				move_char = *p2;
				/* Field prompts. */
				if (COB_FIELD_IS_NUMERIC (f)) {
					/* Numeric prompt zeros. */
					if (move_char == '0') {
						cob_move_cursor (cline, count);
						cob_addch (prompt_char);
					} else {
						/* Switch to remove prompts from within field. */
						if (fattr & COB_SCREEN_NO_ECHO) {
							prompt_char = COB_CH_SP;
						} else if (fattr & COB_SCREEN_SECURE) {
							prompt_char = COB_CH_AS;
						} else {
							prompt_char = '0';
						}
					}
				} else {
					/* Alpha prompts. */
					if (move_char == ' ') {
						cob_move_cursor (cline, count);
						cob_addch (prompt_char);
					} else {
						/* Switch to remove prompts from within field. */
						if (fattr & COB_SCREEN_NO_ECHO) {
							prompt_char = COB_CH_SP;
						} else if (fattr & COB_SCREEN_SECURE) {
							prompt_char = COB_CH_AS;
						} else {
							prompt_char = COB_CH_SP;
						}
					}
				}
			}
			/* Cursor to current column. */
			cob_move_cursor (cline, ccolumn);
			/* Refresh screen. */
			refresh ();
		}
		errno = 0;
		timeout (gettimeout);

		/* Get a character. */
		keyp = getch ();

		/* Key error - time out. */
		if (keyp == ERR) {
			fret = 8001;
			goto field_return;
		}
		/* Return function keys F1 through F64 */
		if (keyp > KEY_F0 && keyp < KEY_F(65)) {
			fret = 1000 + keyp - KEY_F0;
			goto field_return;
		}

		cob_convert_key (&keyp, 1U);
		if (keyp <= 0) {
			(void)flushinp ();
			cob_beep ();
			continue;
		}

		/* Return special keys */
		switch (keyp) {
		case KEY_ENTER:
			/* Enter. */
			goto field_return;
		case KEY_PPAGE:
			/* Page up. */
			fret = 2001;
			goto field_return;
		case KEY_NPAGE:
			/* Page down. */
			fret = 2002;
			goto field_return;
		case KEY_UP:
			/* Up arrow. */
			fret = 2003;
			goto field_return;
		case KEY_DOWN:
			/* Down arrow. */
			fret = 2004;
			goto field_return;
		case KEY_PRINT:
			/* Print key. */
			/* pdcurses not returning this ? */
			fret = 2006;
			goto field_return;
		case 033:
			/* Escape key. */
			fret = 2005;
			goto field_return;
		case KEY_STAB:
			/* Tab key. */
			fret = 2007;
			goto field_return;
		case KEY_BTAB:
			/* Shift-Tab key, Back tab. */
			fret = 2008;
			goto field_return;
		default:
			break;
		}

		/* extension: ACCEPT OMITTED */
		if (unlikely (!f)) {
			/* special keys for ACCEPT OMITTED */
			switch (keyp) {
			case KEY_LEFT:
				fret = 2009;
				goto field_return;
			case KEY_RIGHT:
				fret = 2010;
				goto field_return;
			case KEY_IC:
				/* Insert key. */
				fret = 2011;
				goto field_return;
			case KEY_DC:
				/* Delete key. */
				fret = 2012;
				goto field_return;
			case KEY_BACKSPACE:
				/* Backspace key. */
				fret = 2013;
				goto field_return;
			case KEY_HOME:
				/* Home key. */
				fret = 2014;
				goto field_return;
			case KEY_END:
				/* End key. */
				fret = 2015;
				goto field_return;
			default:
				(void)flushinp ();
				cob_beep ();
				continue;
			}
		}

		/* Positioning keys */
		switch (keyp) {
		case KEY_BACKSPACE:
			/* Backspace key. */
			if ((int) ccolumn > scolumn) {
				/* Shift remainder left with cursor. */
				for (count = ccolumn; count < rightpos + 1; count++) {
					/* Get character. */
					p2 = COB_TERM_BUFF + count - scolumn ;
					move_char = *p2;
					/* Move the character left. */
					p2 = COB_TERM_BUFF + count - scolumn - 1;
					*p2 = move_char;
					/* Update screen with moved character. */
					cob_move_cursor (cline, count - 1);
					if (fattr & COB_SCREEN_NO_ECHO) {
						cob_addch (COB_CH_SP);
					} else if (fattr & COB_SCREEN_SECURE) {
						cob_addch (COB_CH_AS);
					} else {
						cob_addch (move_char);
					}
				}
				/* Put space as the right most character. */
				p2 = COB_TERM_BUFF + size_accept - 1;
				if (fattr & COB_SCREEN_NO_ECHO) {
					*p2 = COB_CH_SP;
				} else if (COB_FIELD_IS_NUMERIC (f)) {
					*p2 = '0';
				} else {
					*p2 = COB_CH_SP;
				}
				/* Move cursor left one from current. */
				ccolumn--;
				cob_move_cursor (cline, ccolumn);
				p--;
			}
			ateof = 0;
			continue;
		case KEY_HOME:
			/* Home key, cursor to start of characters. */
			/* Prepare for empty field. */
			ccolumn = rightpos;
			move_char = ' ';
			/* Find non-blank from left. */
			for (count = scolumn; count <= rightpos; count++) {
				/* Get character. */
				p2 = COB_TERM_BUFF + count - scolumn;
				move_char = *p2;
				/* Non blank stop. */
				if (move_char != ' ') {
					ccolumn = count;
					break;
				}
			}
			cob_move_cursor (cline, ccolumn);
			p = COB_TERM_BUFF + ccolumn - scolumn;
			ateof = 0;
			continue;
		case 01026:
			/* Alt-Home key, cursor to start of field. */
			cob_move_cursor (sline, scolumn);
			p = COB_TERM_BUFF;
			ateof = 0;
			continue;
		case KEY_END:
			/* End key, cursor to end of characters. */
			/* Prepare for empty field. */
			ccolumn = scolumn;
			move_char = ' ';
			/* Find non blank from right. */
			for (count = rightpos; (int) count >= scolumn; count--) {
				/* Get character. */
				p2 = COB_TERM_BUFF + count - scolumn;
				move_char = *p2;
				/* Non blank stop. */
				if (move_char != ' ') {
					ccolumn = count;
					break;
				}
			}
			/* Cursor to first blank after. */
			if (move_char != ' ' && ccolumn != rightpos) {
				ccolumn++;
			}
			cob_move_cursor (cline, ccolumn);
			p = COB_TERM_BUFF + ccolumn - scolumn;
			ateof = 0;
			continue;
		case 01021:
			/* Alt-End key, cursor to end of size of field */
			cob_move_cursor (sline, rightpos);
			p = COB_TERM_BUFF + size_accept - 1;
			ateof = 0;
			continue;
		case KEY_LEFT:
		case KEY_CLOSE:
			/* Left-arrow     KEY_LEFT  auto-skip. */
			/* Alt-left-arrow KEY_CLOSE no auto-skip. */
			ateof = 0;
			if ((int) ccolumn > scolumn) {
				ccolumn--;
				cob_move_cursor (cline, ccolumn);
				p = COB_TERM_BUFF + ccolumn - scolumn;
				continue;
			}
			/* End of field, auto-skip, return left-arrow. */
			if (fattr & COB_SCREEN_AUTO && keyp == KEY_LEFT) {
				fret = 2009;
				goto field_return;
			}
			cob_beep ();
			continue;
		case KEY_RIGHT:
		case KEY_PREVIOUS:
			/* Right-arrow     KEY_RIGHT    auto-skip. */
			/* Alt-right-arrow KEY_PREVIOUS no auto-skip. */
			if (ccolumn < rightpos) {
				ccolumn++;
				cob_move_cursor (cline, ccolumn);
				p = COB_TERM_BUFF + ccolumn - scolumn;
				continue;
			}
			/* End of field, auto-skip, return right-arrow. */
			if (fattr & COB_SCREEN_AUTO && keyp == KEY_RIGHT) {
				fret = 2010;
				goto field_return;
			}
			cob_beep ();
			continue;
		case KEY_IC:
			/* Insert key toggle */
			/* If off turn on, if on turn off;
			   additional: switch between vertical bar cursor (on) and
			   square cursor (off) - note: the cursor change may has no
			   effect in all curses implementations / terminals */
			if (cobsetptr->cob_insert_mode == 0) {
				cobsetptr->cob_insert_mode = 1;     /* on */
				(void)curs_set(1);	/* switch to vertical bar cursor */
			} else {
				cobsetptr->cob_insert_mode = 0;     /* off */
				(void)curs_set(2);	/* switch to square cursor */
			}
			continue;
		case KEY_DC:
			/* Delete key. */
			/* Delete character, move remainder left. */
			for (count = ccolumn; count < rightpos; count++) {
				/* Get character one position to right. */
				p2 = COB_TERM_BUFF + count - scolumn + 1;
				move_char = *p2;
				/* Move the character left. */
				p2 = COB_TERM_BUFF + count - scolumn;
				*p2 = move_char;
				/* Update screen with moved character. */
				cob_move_cursor (cline, count);
				if (fattr & COB_SCREEN_NO_ECHO) {
					cob_addch (COB_CH_SP);
				} else if (fattr & COB_SCREEN_SECURE) {
					cob_addch (COB_CH_AS);
				} else {
					cob_addch (move_char);
				}
			}
			/* Put space as the right most character. */
			p2 = COB_TERM_BUFF + size_accept - 1;
			if (fattr & COB_SCREEN_NO_ECHO) {
				*p2 = COB_CH_SP;
			} else if (COB_FIELD_IS_NUMERIC (f)) {
				*p2 = '0';
			} else {
				*p2 = COB_CH_SP;
			}
			/* Put cursor back to original position. */
			cob_move_cursor (cline, ccolumn);
			continue;
		case KEY_EOL:
			/* Alt-Delete key, erase cursor to end of field. */
			for (count = ccolumn; count <= rightpos; count++) {
				/* Character position. */
				p2 = COB_TERM_BUFF + count - scolumn;
				/* Blank character. */
				if (fattr & COB_FIELD_IS_NUMERIC (f)) {
					move_char = '0';
				} else {
					move_char = COB_CH_SP;
				}
				*p2 = move_char;
				/* Update screen with blank character. */
				cob_move_cursor (cline, count);
				if (fattr & COB_SCREEN_NO_ECHO) {
					cob_addch (COB_CH_SP);
				} else if (fattr & COB_SCREEN_SECURE) {
					cob_addch (COB_CH_AS);
				} else {
					cob_addch (move_char);
				}
			}
			/* Put cursor back to original position. */
			cob_move_cursor (cline, ccolumn);
			continue;
		default:
			break;
		}

		/* Handle printable character. */
#if 0 /* FIXME: we can't handle anything > UCHAR_MAX here because of
		*p = (unsigned char) keyp;
		--> revise */
		if (keyp > 037 && keyp < (int)A_CHARTEXT) {
#else
		if (keyp > 037 && keyp <= UCHAR_MAX) {
#endif
			/* Numeric field check. */
			if (cob_field_is_numeric_or_numeric_edited (f)) {
				if (keyp < '0' || keyp > '9') {
					cob_beep ();
					continue;
				}
			}

			/* Handle UPPER/LOWER. */
			if (fattr & COB_SCREEN_UPPER) {
				if (islower (keyp)) {
					keyp = toupper (keyp);
				}
			} else if (fattr & COB_SCREEN_LOWER) {
				if (isupper (keyp)) {
					keyp = tolower (keyp);
				}
			}

			/* Insert character, if requested. */
			if (cobsetptr->cob_insert_mode == 1) {
				/* get last character in field */
				/* check and beep if field is already full,
				   ignore numeric fields for now */
				if (cob_field_is_numeric_or_numeric_edited (f)) {
					p2 = (unsigned char *)" ";
				} else {
					p2 = COB_TERM_BUFF + rightpos - scolumn;
				}
				if (*p2 != ' ') {
					cob_beep ();
					continue;
				}
				/* Move remainder to the right. */
				for (count = rightpos; count > ccolumn - 1; count--) {
					/* Get character */
					p2 = COB_TERM_BUFF + count - scolumn - 1;
					move_char = *p2;
					/* Move character one right. */
					p2 = COB_TERM_BUFF + count - scolumn;
					*p2 = move_char;
					/* Update screen with moved character. */
					if ((int) count > scolumn) {
						cob_move_cursor (cline, count);
						if (move_char != ' ') {
							if (fattr & COB_SCREEN_NO_ECHO) {
								cob_addch (COB_CH_SP);
							} else if (fattr & COB_SCREEN_SECURE) {
								cob_addch (COB_CH_AS);
							} else {
								cob_addch (move_char);
							}
						}
					}
				}
				cob_move_cursor (cline, ccolumn);
			}

			/* actual storing the key */
			*p = (unsigned char)keyp;

			count = 1;
			/* Display character or '*' if secure. */
			if (fattr & COB_SCREEN_SECURE) {
				cob_addch (COB_CH_AS);
			} else if (fattr & COB_SCREEN_NO_ECHO) {
				cob_addch (COB_CH_SP);
			} else {
				cob_addch ((const chtype)keyp);
			}
			if (ccolumn == rightpos) {
				/* Auto-skip at end of field. */
				if (fattr & COB_SCREEN_AUTO) {
					break;
				}
				cob_move_cursor (cline, ccolumn);
				/* check if we (still) are at last position and inform
				   user with a beep (after having processed his key) */
				if (ateof) {
					cob_beep ();
				} else {
					ateof = 1;
				}
			} else {
				p++;
			}
			continue;
		}
		(void)flushinp ();
		cob_beep ();
	}
 field_return:
	if (f) {
		cob_move_cursor (sline, rightpos + 1);
	}
	refresh ();
	cob_check_pos_status (fret);
	if (!f) {
		return;
	}
	cob_move (&temp, f);
}

static void
field_accept_from_curpos (cob_field *f, cob_field *fgc,
	      cob_field *bgc, cob_field *fscroll, cob_field *ftimeout,
	      cob_field *prompt, cob_field *size_is, const cob_flags_t fattr)
{
	int		cline;
	size_t		ccolumn;

	/* Get current line, column. */
	getyx (stdscr, cline, ccolumn);

	/* accept field */
	field_accept (f, cline, ccolumn, fgc, bgc, fscroll, ftimeout, prompt, size_is, fattr);
}

static void
field_display_at_curpos (cob_field *f,
	cob_field *fgc, cob_field *bgc, cob_field *fscroll,
	cob_field *size_is, const cob_flags_t fattr)
{
	int		cline;
	size_t		ccolumn;

	/* Get current line, column. */
	getyx (stdscr, cline, ccolumn);

	field_display (f, cline, ccolumn, fgc, bgc, fscroll, size_is, fattr);
}

/* Global functions */

void
cob_screen_display (cob_screen *s, cob_field *line, cob_field *column,
		    const int zero_line_col_allowed)
{
	int	sline;
	int	scolumn;

	extract_line_and_col_vals (line, column, DISPLAY_STATEMENT,
				   zero_line_col_allowed, &sline, &scolumn);
	screen_display (s, sline, scolumn);
}
void
cob_screen_accept (cob_screen *s, cob_field *line, cob_field *column,
		   cob_field *ftimeout, const int zero_line_col_allowed)
{
	int	sline;
	int	scolumn;

	extract_line_and_col_vals (line, column, ACCEPT_STATEMENT,
				   zero_line_col_allowed, &sline, &scolumn);
	screen_accept (s, sline, scolumn, ftimeout);
}

void
cob_field_display (cob_field *f, cob_field *line, cob_field *column,
		   cob_field *fgc, cob_field *bgc, cob_field *fscroll,
		   cob_field *size_is, const cob_flags_t fattr)
{
	int	sline;
	int	scolumn;

	/*
	  LINE/COL 0 is always allowed as it is impossible to specify it in the
	  standard format (DISPLAY ... UPON CRT) and all implementations of the
	  extended screen format (DISPLAY ... WITH UNDERLINE, HIGHLIGHT, etc.)
	  require it.
	*/
	extract_line_and_col_vals (line, column, DISPLAY_STATEMENT, 1, &sline,
				   &scolumn);
	field_display (f, sline, scolumn, fgc, bgc, fscroll, size_is, fattr);
}

void
cob_field_accept (cob_field *f, cob_field *line, cob_field *column,
		  cob_field *fgc, cob_field *bgc, cob_field *fscroll,
		  cob_field *ftimeout, cob_field *prompt, cob_field *size_is,
		  const cob_flags_t fattr)
{
	int	sline;
	int	scolumn;

	/* See above comment in cob_field_display. */
	extract_line_and_col_vals (line, column, ACCEPT_STATEMENT, 1, &sline,
				   &scolumn);
	field_accept (f, sline, scolumn, fgc, bgc, fscroll, ftimeout, prompt,
		      size_is, fattr);
}

int
cob_sys_clear_screen (void)
{
	init_cob_screen_if_needed ();
	clear ();
	refresh ();
	cob_current_y = 0;
	cob_current_x = 0;
	return 0;
}

void
cob_screen_set_mode (const cob_u32_t smode)
{
	if (!smode) {
		refresh ();
		def_prog_mode ();
		endwin ();
	} else {
		reset_prog_mode ();
		refresh ();
	}
}

/* display a C string without auto-newline */
int
cob_display_text (const char *text)
{
	cob_field	field;
	cob_field_attr	attr;

	if (text[0] == 0) return 0;

	COB_FIELD_INIT (strlen (text), (unsigned char *)text, &attr);
	COB_ATTR_INIT (COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL);

	field_display_at_curpos (&field, NULL, NULL, NULL, NULL, 0);

	return 0;
}

/* C: get a char x'01' thru x'255' or keyboard status > 1000  (or 0) */
int
cob_get_char (void)
{
	cob_field		field;
	unsigned char		c = ' ';
	cob_field_attr		attr;

	COB_FIELD_INIT (1, &c, &attr);
	COB_ATTR_INIT (COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL);

	field_accept_from_curpos (&field, NULL, NULL, NULL, NULL, NULL, NULL,
				  COB_SCREEN_AUTO);
	if (c == ' ') {
		return COB_ACCEPT_STATUS;
	} else {
		return (int)c;
	}
}

/* get a C string with given max-length - returns keyboard status */
int
cob_get_text (char *text, int size)
{
	cob_field	field;
	cob_field_attr	attr;

	if (size > 0) {
		COB_FIELD_INIT (size, (unsigned char *)text, &attr);
		COB_ATTR_INIT (COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL);
		field_accept_from_curpos (&field, NULL, NULL, NULL, NULL, NULL, NULL, 0);
	} else {
		field_accept (NULL, 0, 0, NULL, NULL, NULL, NULL, NULL, NULL, 0);
	}

	return COB_ACCEPT_STATUS;
}

/* display a formatted C string without auto-newline */
int
cob_display_formatted_text (const char *fmt, ...)
{
	int		size;
	cob_field	field;
	cob_field_attr	attr;
	va_list		ap;
	char		buff [COB_NORMAL_BUFF];

	va_start (ap, fmt);
	size = vsnprintf (buff, COB_NORMAL_BUFF, fmt, ap);
	va_end (ap);

	if (size < 0) {
		return -1;
	}

	if (buff[0] == 0) {
		return 0;
	}

	field.size = cob_min_int (size, COB_NORMAL_MAX);
	field.data = (unsigned char *)&buff;
	COB_ATTR_INIT (COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL);
	field.attr = &attr;

	field_display_at_curpos (&field, NULL, NULL, NULL, NULL, 0);

	return 0;
}

void
cob_exit_screen (void)
{
	cob_flags_t	flags;
	char		exit_msg[COB_MINI_BUFF];

	if (!cobglobptr) {
		return;
	}
	if (cobglobptr->cob_screen_initialized) {
		if (pending_accept && cobsetptr->cob_exit_wait) {
			if (cobsetptr->cob_exit_msg[0] != 0) {
				snprintf (exit_msg, COB_MINI_BUFF, "\n%s ", cobsetptr->cob_exit_msg);
				cob_display_text (exit_msg);
			} else {
				cob_display_text (" ");
			}
			flags = COB_SCREEN_NO_ECHO;
			field_accept_from_curpos (NULL, NULL, NULL, NULL, NULL, NULL, NULL, flags);
		}
		cobglobptr->cob_screen_initialized = 0;
		clear ();
		cob_move_to_beg_of_last_line ();
		delwin (stdscr);
		endwin ();
#ifdef	HAVE_CURSES_FREEALL
		_nc_freeall ();
#endif
		if (cob_base_inp) {
			cob_free (cob_base_inp);
			cob_base_inp = NULL;
		}
	}
	COB_ACCEPT_STATUS = 0;
}

#else	/* COB_GEN_SCREENIO */

void
cob_exit_screen (void)
{
}

void
cob_field_display (cob_field *f, cob_field *line, cob_field *column,
		   cob_field *fgc, cob_field *bgc, cob_field *fscroll,
		   cob_field *size_is, const cob_flags_t fattr)
{
	COB_UNUSED (f);
	COB_UNUSED (line);
	COB_UNUSED (column);
	COB_UNUSED (fgc);
	COB_UNUSED (bgc);
	COB_UNUSED (fscroll);
	COB_UNUSED (size_is);
	COB_UNUSED (fattr);
}

void
cob_field_accept (cob_field *f, cob_field *line, cob_field *column,
		  cob_field *fgc, cob_field *bgc, cob_field *fscroll,
		  cob_field *ftimeout, cob_field *prompt,
		  cob_field *size_is, const cob_flags_t fattr)
{
	COB_UNUSED (f);
	COB_UNUSED (line);
	COB_UNUSED (column);
	COB_UNUSED (fgc);
	COB_UNUSED (bgc);
	COB_UNUSED (fscroll);
	COB_UNUSED (ftimeout);
	COB_UNUSED (prompt);
	COB_UNUSED (size_is);
	COB_UNUSED (fattr);
}

void
cob_screen_display (cob_screen *s, cob_field *line, cob_field *column,
		    const int zero_line_col_allowed)
{
	COB_UNUSED (s);
	COB_UNUSED (line);
	COB_UNUSED (column);
	COB_UNUSED (zero_line_col_allowed);
}

void
cob_screen_accept (cob_screen *s, cob_field *line,
		   cob_field *column, cob_field *ftimeout,
		    const int zero_line_col_allowed)
{
	COB_UNUSED (s);
	COB_UNUSED (line);
	COB_UNUSED (column);
	COB_UNUSED (ftimeout);
	COB_UNUSED (zero_line_col_allowed);
}

void
cob_screen_set_mode (const cob_u32_t smode)
{
	COB_UNUSED (smode);
}

int
cob_sys_clear_screen (void)
{
	return 0;
}

#endif	/* COB_GEN_SCREENIO */

void
cob_screen_line_col (cob_field *f, const int l_or_c)
{
	init_cob_screen_if_needed ();
#ifdef	COB_GEN_SCREENIO
	if (!l_or_c) {
		cob_set_int (f, (int)LINES);
	} else {
		cob_set_int (f, (int)COLS);
	}
#else
	if (!l_or_c) {
		cob_set_int (f, 24);
	} else {
		cob_set_int (f, 80);
	}
#endif
}

int
cob_sys_sound_bell (void)
{
	if (COB_BEEP_VALUE == 9) {
		return 0;
	}
#ifdef	COB_GEN_SCREENIO
	if (!cobglobptr->cob_screen_initialized &&
	    COB_BEEP_VALUE != 2) {
		cob_screen_init ();
	}
	cob_beep ();
#else
	cob_speaker_beep ();
#endif
	return 0;
}

void
cob_accept_escape_key (cob_field *f)
{
	cob_set_int (f, COB_ACCEPT_STATUS);
}

/* get CurSoR position on screen */
int
cob_sys_get_csr_pos (unsigned char *fld)
{
#ifdef	COB_GEN_SCREENIO
	int	cline;
	int	ccol;
#endif

	COB_CHK_PARMS (CBL_GET_CSR_POS, 1);

#ifdef	COB_GEN_SCREENIO
	getyx (stdscr, cline, ccol);
	fld[0] = (unsigned char)cline;
	fld[1] = (unsigned char)ccol;

#else
	fld[0] = 1U;
	fld[1] = 1U;
#endif
	return 0;
}

/* COBOL: get a char (or x'00' for function keys)
   call a second time when getting x'00' leads to the function keys
   1001-1199 as x'01' - x'C7', 2001 - 2055 as x'C9' - x'FF'
   No implementation of MF function tables so far.
*/
int
cob_sys_get_char (char c)
{
#ifdef	COB_GEN_SCREENIO
	int ret;
#endif

	COB_CHK_PARMS (CBL_READ_KBD_CHAR, 1);

#ifdef	COB_GEN_SCREENIO
	if (!got_sys_char) {
		ret = cob_get_char ();
		if (ret > 255) {
			c = 0;
			got_sys_char = 1;
		} else {
			c = (char) ret;
		}
	} else {
		got_sys_char = 0;
		if (COB_ACCEPT_STATUS == 0) {
			return cob_sys_get_char (c);
		} else if (COB_ACCEPT_STATUS > 1000 && COB_ACCEPT_STATUS < 1201) {
			c = (char) (COB_ACCEPT_STATUS - 1000);
		} else if (COB_ACCEPT_STATUS > 2000 && COB_ACCEPT_STATUS < 2056) {
			c = (char) (COB_ACCEPT_STATUS - 1800);
		} else {
			return -1;
		}
	}
#else
	COB_UNUSED (c);
#endif
	return 0;
}

/* set CurSoR position on screen */
int
cob_sys_set_csr_pos (unsigned char *fld)
{
#ifdef	COB_GEN_SCREENIO
	int	cline;
	int	ccol;
#endif

	COB_CHK_PARMS (CBL_SET_CSR_POS, 1);

#ifdef	COB_GEN_SCREENIO
	init_cob_screen_if_needed ();
	cline = fld[0];
	ccol= fld[1];
	return move (cline, ccol);
#else
	COB_UNUSED (fld);
	return 0;
#endif
}

/* get current screen size */
int
cob_sys_get_scr_size (unsigned char *line, unsigned char *col)
{
	COB_CHK_PARMS (CBL_GET_SCR_SIZE, 2);

#ifdef	COB_GEN_SCREENIO
	init_cob_screen_if_needed ();
	*line = (unsigned char)LINES;
	*col = (unsigned char)COLS;
#else
	*line = 24U;
	*col = 80U;
#endif
	return 0;
}

int
cob_get_scr_cols (void)
{
	init_cob_screen_if_needed();
#ifdef	COB_GEN_SCREENIO
	return (int)COLS;
#else
	return 80;
#endif
}

int
cob_get_scr_lines (void)
{
	init_cob_screen_if_needed();
#ifdef	COB_GEN_SCREENIO
	return (int)LINES;
#else
	return 24;
#endif
}

void
cob_init_screenio (cob_global *lptr, cob_settings *sptr)
{
	cobglobptr = lptr;
	cobsetptr  = sptr;
}
