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
#include <string.h>
#include <stdarg.h>
#include <errno.h>
#ifdef	HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <time.h>

/* Force symbol exports */
#define	COB_LIB_EXPIMP

#include "libcob.h"
#include "coblocal.h"

/* Local variables */

static cob_global		*cobglobptr;
static cob_settings		*cobsetptr;

static const unsigned short	bin_digits[] =
	{ 1, 3, 5, 8, 10, 13, 15, 17, 20 };

static const cob_field_attr	const_alpha_attr =
				{COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};

/* DISPLAY */

static void
display_numeric (cob_field *f, FILE *fp)
{
	int		i;
	unsigned short	digits;
	signed short	scale;
	int		size;
	cob_field_attr	attr;
	cob_field	temp;

	digits = COB_FIELD_DIGITS (f);
	scale = COB_FIELD_SCALE (f);
	size = digits + (COB_FIELD_HAVE_SIGN (f) ? 1 : 0);
	if (size >= COB_MEDIUM_MAX) {
		fputs (_("(Not representable)"), fp);
		return;
	}
	COB_ATTR_INIT (COB_TYPE_NUMERIC_DISPLAY, digits, scale, 0, NULL);
	temp.size = size;
	temp.data = COB_TERM_BUFF;
	temp.attr = &attr;
	if (COB_FIELD_HAVE_SIGN (f)) {
		attr.flags = COB_FLAG_HAVE_SIGN | COB_FLAG_SIGN_SEPARATE;
		if (COB_FIELD_SIGN_LEADING (f) ||
		    COB_FIELD_TYPE (f) != COB_TYPE_NUMERIC_DISPLAY) {
			attr.flags |= COB_FLAG_SIGN_LEADING;
		}
	}

	cob_move (f, &temp);
	for (i = 0; i < size; ++i) {
		putc (temp.data[i], fp);
	}
}

static void
pretty_display_numeric (cob_field *f, FILE *fp)
{
	cob_pic_symbol	*p;
	unsigned char	*q = COB_TERM_BUFF;
	int		i;
	unsigned short	digits = COB_FIELD_DIGITS (f);
	signed short	scale = COB_FIELD_SCALE (f);
	int		size = digits + !!COB_FIELD_HAVE_SIGN (f) + !!scale;
	cob_field_attr	attr;
	cob_field	temp;
	cob_pic_symbol	pic[6] = {{ '\0' }};


	if (size > COB_MEDIUM_MAX) {
		fputs (_("(Not representable)"), fp);
		return;
	}
	temp.size = size;
	temp.data = q;
	temp.attr = &attr;
	COB_ATTR_INIT (COB_TYPE_NUMERIC_EDITED, digits, scale, 0,
		       (const cob_pic_symbol *)pic);
	p = pic;

	if (COB_FIELD_HAVE_SIGN (f)) {
		if (COB_FIELD_SIGN_SEPARATE (f)
		 && !COB_FIELD_SIGN_LEADING(f)) {
			/* done later */
		} else {
			p->symbol = '+';
			p->times_repeated = 1;
			++p;
		}
	}
	if (scale > 0) {
		if (digits - scale > 0) {
			p->symbol = '9';
			p->times_repeated = digits - scale;
			++p;
		}
		
		p->symbol = COB_MODULE_PTR->decimal_point;
		p->times_repeated = 1;
		++p;

		p->symbol = '9';
		p->times_repeated = scale;
		++p;
	} else {
		p->symbol = '9';
		p->times_repeated = digits;
		++p;
	}
	if (COB_FIELD_HAVE_SIGN (f)) {
		if (COB_FIELD_SIGN_SEPARATE (f)
		 && !COB_FIELD_SIGN_LEADING(f)) {
			p->symbol = '+';
			p->times_repeated = 1;
			++p;
		}
	}
	p->symbol = '\0';

	cob_move (f, &temp);
	for (i = 0; i < size; ++i) {
		putc (q[i], fp);
	}
}

static void
display_alnum (cob_field *f, FILE *fp)
{
	size_t	i;

	for (i = 0; i < f->size; ++i) {
		putc (f->data[i], fp);
	}
}

static void
display_common (cob_field *f, FILE *fp)
{
	unsigned char	*p;
	union {
		double		f1doub;
		float		f1float;
	} un;
	int		n;
#if	0	/* RXWRXW - Print bin */
	cob_field	temp;
	cob_field_attr	attr;
#endif

	if (f->size == 0) {
		return;
	}
	switch (COB_FIELD_TYPE (f)) {
	case COB_TYPE_NUMERIC_DOUBLE:
		memcpy (&un.f1doub, f->data, sizeof (double));
		fprintf (fp, "%-.16G", un.f1doub);
		return;
	case COB_TYPE_NUMERIC_FLOAT:
		memcpy (&un.f1float, f->data, sizeof (float));
		fprintf (fp, "%-.8G", (double)un.f1float);
		return;
	case COB_TYPE_NUMERIC_FP_DEC64:
	case COB_TYPE_NUMERIC_FP_DEC128:
		cob_print_ieeedec (f, fp);
		return;
	default:
		break;
	}
	if (COB_FIELD_IS_POINTER (f)) {
		fprintf (fp, "0x");
#ifdef	WORDS_BIGENDIAN
		p = f->data;
		for (n = 0; n < sizeof(void *); ++n, ++p) {
#else
		p = f->data + sizeof(void *) - 1;
		for (n = sizeof(void *) - 1; n >= 0; --n, --p) {
#endif
			fprintf (fp, "%x%x", *p >> 4, *p & 0xF);
		}
		return;
	} else if (COB_FIELD_REAL_BINARY(f) ||
		   (COB_FIELD_TYPE(f) == COB_TYPE_NUMERIC_BINARY &&
		    !COB_MODULE_PTR->flag_pretty_display)) {
		cob_print_realbin (f, fp, bin_digits[f->size]);
		return;
#if	0	/* RXWRXW - print bin */
	} else if (COB_FIELD_TYPE(f) == COB_TYPE_NUMERIC_BINARY &&
		    !COB_MODULE_PTR->flag_pretty_display) {
		attr = *f->attr;
		temp = *f;
		attr.digits = bin_digits[f->size];
		temp.attr = &attr;
		display_numeric (&temp, fp);
		return;
#endif
	} else if (COB_FIELD_IS_NUMERIC (f)) {
		if (COB_MODULE_PTR->flag_pretty_display) {
			pretty_display_numeric (f, fp);
		} else {
			display_numeric (f, fp);
		}
		return;
	}
	display_alnum (f, fp);
}

void
cob_display (const int to_stderr, const int newline, const int varcnt, ...)
{
	FILE		*fp;
	cob_field	*f;
	int		i;
	int		nlattr;
	cob_u32_t	disp_redirect;
	va_list		args;

	disp_redirect = 0;
	if (to_stderr) {
		fp = stderr;
	} else {
		fp = stdout;
		if (cobglobptr->cob_screen_initialized) {
			if (!COB_DISP_TO_STDERR) {
				disp_redirect = 1;
			} else {
				fp = stderr;
			}
		}
	}

	nlattr = newline ? COB_SCREEN_EMULATE_NL : 0;
	va_start (args, varcnt);
	for (i = 0; i < varcnt; ++i) {
		f = va_arg (args, cob_field *);
		if (unlikely (disp_redirect)) {
			cob_field_display (f, NULL, NULL, NULL, NULL,
					   NULL, NULL, nlattr);
		} else {
			display_common (f, fp);
		}
	}
	va_end (args);

	if (newline && !disp_redirect) {
		putc ('\n', fp);
		fflush (fp);
	}
}

/* ACCEPT */

void
cob_accept (cob_field *f)
{
	unsigned char	*p;
	size_t		size;
	int		ipchr;
	cob_field	temp;

	if (cobglobptr->cob_screen_initialized) {
		cob_field_accept (f, NULL, NULL, NULL, NULL,
				  NULL, NULL, NULL, NULL,
				  COB_SCREEN_PROMPT);
		return;
	}
	if (COB_MODULE_PTR->crt_status) {
		if (COB_FIELD_IS_NUMERIC (COB_MODULE_PTR->crt_status)) {
			cob_set_int (COB_MODULE_PTR->crt_status, 0);
		} else {
			memset (COB_MODULE_PTR->crt_status->data, '0', (size_t)4);
		}
	}
	/* extension: ACCEPT OMITTED */
	if (unlikely (!f)) {
		for (; ; ) {
			ipchr = getchar ();
			if (ipchr == '\n' || ipchr == EOF) {
				break;
			} else if (ipchr == 03) {
				cob_raise (2);
			}
		}
		return;
	}
	p = COB_TERM_BUFF;
	temp.data = p;
	temp.attr = &const_alpha_attr;
	size = 0;
	/* Read a line */
	for (; size < COB_MEDIUM_MAX; ) {
		ipchr = getchar ();
		if (unlikely (ipchr == EOF)) {
			cob_set_exception (COB_EC_IMP_ACCEPT);
			if (!size) {
				size = 1;
				p[0] = ' ';
				p[1] = 0;
			}
			break;
		} else if (ipchr == 03) {
			cob_raise (2);
		} else if (ipchr == '\n') {
			break;
		}
		p[size++] = (char) ipchr;
	}
	temp.size = size;
	if (COB_FIELD_TYPE(f) == COB_TYPE_NUMERIC_DISPLAY) {
		if (temp.size > f->size) {
			temp.size = f->size;
		}
	}
	cob_move (&temp, f);
}

void
cob_init_termio (cob_global *lptr, cob_settings *sptr)
{
	cobglobptr = lptr;
	cobsetptr  = sptr;
}
