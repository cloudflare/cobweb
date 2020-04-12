/*
   Copyright (C) 2001-2012, 2014-2017 Free Software Foundation, Inc.
   Written by Keisuke Nishida, Roger While, Simon Sobisch

   This file is part of GnuCOBOL.

   The GnuCOBOL compiler is free software: you can redistribute it
   and/or modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   GnuCOBOL is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GnuCOBOL.  If not, see <http://www.gnu.org/licenses/>.
*/


#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <stdarg.h>

#include "cobc.h"
#include "tree.h"

static char		*errnamebuff = NULL;
static struct cb_label	*last_section = NULL;
static struct cb_label	*last_paragraph = NULL;

static int conf_error_displayed = 0;
static int last_error_line = 0;
static const char	*last_error_file = "Unknown";
static FILE			*sav_lst_file = NULL;
static int		ignore_error = 0;

#define COBC_ERRBUF_SIZE		1024

size_t				cb_msg_style;

static void
print_error (const char *file, int line, const char *prefix,
	     const char *fmt, va_list ap)
{
	char			errmsg[BUFSIZ];
	struct list_error	*err;
	struct list_files	*cfile;

	if (!file) {
		file = cb_source_file;
	}
	if (!line) {
		line = cb_source_line;
	}

	/* Print section and/or paragraph name */
	if (current_section != last_section) {
		if (current_section && !current_section->flag_dummy_section) {
			if (file) {
				fprintf (stderr, "%s: ", file);
			}
			fprintf (stderr, _("in section '%s':"),
				(char *)current_section->name);
			fputs ("\n", stderr);
		}
		last_section = current_section;
	}
	if (current_paragraph != last_paragraph) {
		if (current_paragraph && !current_paragraph->flag_dummy_paragraph) {
			if (file) {
				fprintf (stderr, "%s: ", file);
			}
			fprintf (stderr, _("in paragraph '%s':"),
				(char *)current_paragraph->name);
			fputs ("\n", stderr);
		}
		last_paragraph = current_paragraph;
	}

	/* Print the error */
	if (file) {
		if (cb_msg_style == CB_MSG_STYLE_MSC) {
			fprintf (stderr, "%s (%d): ", file, line);
		} else {
			fprintf (stderr, "%s: %d: ", file, line);
		}
	}
	if (prefix) {
		fprintf (stderr, "%s", prefix);
	}
	vsprintf (errmsg, fmt, ap);
	fprintf (stderr, "%s\n", errmsg);

	if (cb_src_list_file) {
		/* If we have a file, queue message for listing processing */
		if (cb_current_file) {
			/* set up listing error */
			err = cobc_malloc (sizeof (struct list_error));
			err->line = line;
			if (file) {
				err->file = cobc_strdup (file);
			} else {
				err->file = NULL;
			}
			if (prefix) {
				err->prefix = cobc_strdup (prefix);
			} else {
				err->prefix = NULL;
			}
			err->msg = cobc_strdup (errmsg);

			/* set correct listing entry for this file */
			cfile = cb_current_file;
			if (!cfile->name || strcmp (cfile->name, file)) {
				cfile = cfile->copy_head;
				while (cfile) {
					if (cfile->name && !strcmp (cfile->name, file)) {
						break;
					}
					cfile = cfile->next;
				}
			}
			/* if file doesn't exist in the list then add to current file */
			if (!cfile) {
				cfile = cb_current_file;
			}
			/* add error to listing entry */
			err->next = cfile->err_head;
			cfile->err_head = err;

		/* Otherwise, just write error to the listing file */
		} else {
			fprintf (cb_src_list_file, "%s\n", errmsg);
		}
	}
}

static void
configuration_error_head (void)
{
	if (conf_error_displayed) {
		return;
	}
	conf_error_displayed = 1;
	fputs (_("configuration error:"), stderr);
	putc ('\n', stderr);
}

/* reentrant version of strerror */
char *
cb_get_strerror (void)
{
#ifdef HAVE_STRERROR
	return (char *)cobc_main_strdup (strerror (errno));
#else
	char * msg;
	msg = cobc_main_malloc ((size_t)COBC_ERRBUF_SIZE);
	snprintf (msg, COBC_ERRBUF_SIZE - 1, _("system error %d"), errno);
	return msg;
#endif
}

int
cb_set_ignore_error (int state)
{
	int prev = ignore_error;
	ignore_error = state;
	return prev;
}

void
cb_warning (int pref, const char *fmt, ...)
{
	va_list ap;

	if (!pref) {
		return;
	}

	va_start (ap, fmt);
	print_error (NULL, 0,
		(pref == COBC_WARN_AS_ERROR) ? _("error [-Werror]: ") : _("warning: "),
		fmt, ap);
	va_end (ap);

	if (sav_lst_file) {
		return;
	}
	if (pref == COBC_WARN_AS_ERROR) {
		if (++errorcount > cb_max_errors) {
			cobc_too_many_errors ();
		}
	} else {
		warningcount++;
	}
}

void
cb_error (const char *fmt, ...)
{
	va_list ap;

	cobc_in_repository = 0;
#if	0	/* RXWRXW - Is this right? */
	cobc_cs_check = 0;
#endif
	va_start (ap, fmt);
	print_error (NULL, 0, ignore_error ?
		_("error (ignored): "):_("error: "), fmt, ap);
	va_end (ap);

	if (sav_lst_file) {
		return;
	}
	if (ignore_error) {
		warningcount++;
	} else {
		if (++errorcount > cb_max_errors) {
			cobc_too_many_errors ();
		}
	}
}

void
cb_perror (const int config_error, const char *fmt, ...)
{
	va_list ap;

	if (config_error) {
		configuration_error_head();
	}

	va_start (ap, fmt);
	print_error (NULL, 0, "", fmt, ap);
	va_end (ap);


	if (++errorcount > cb_max_errors) {
		cobc_too_many_errors ();
	}
}

/* Warning/error for pplex.l input routine */
/* At this stage we have not parsed the newline so that */
/* cb_source_line needs to be adjusted by newline_count in pplex.l */

void
cb_plex_warning (int pref, const size_t sline, const char *fmt, ...)
{
	va_list ap;

	if (!pref) {
		return;
	}

	va_start (ap, fmt);
	print_error (NULL, (int)(cb_source_line + sline),
		(pref == COBC_WARN_AS_ERROR) ? _("error [-Werror]: ") : _("warning: "),
		fmt, ap);
	va_end (ap);

	if (sav_lst_file) {
		return;
	}
	if (pref == COBC_WARN_AS_ERROR) {
		if (++errorcount > cb_max_errors) {
			cobc_too_many_errors ();
		}
	} else {
		warningcount++;
	}
}

void
cb_plex_error (const size_t sline, const char *fmt, ...)
{
	va_list ap;

	va_start (ap, fmt);
	print_error (NULL, (int)(cb_source_line + sline), ("error: "), fmt, ap);
	va_end (ap);

	if (sav_lst_file) {
		return;
	}
	if (++errorcount > cb_max_errors) {
		cobc_too_many_errors ();
	}
}

unsigned int
cb_plex_verify (const size_t sline, const enum cb_support tag,
		const char *feature)
{
	switch (tag) {
	case CB_OK:
		return 1;
	case CB_WARNING:
		cb_plex_warning (COBC_WARN_FILLER, sline, _("%s used"), feature);
		return 1;
	case CB_ARCHAIC:
		cb_plex_warning (cb_warn_archaic, sline, _("%s is archaic in %s"),
			feature, cb_config_name);
		return 1;
	case CB_OBSOLETE:
		cb_plex_warning (cb_warn_obsolete, sline, _("%s is obsolete in %s"),
			feature, cb_config_name);
		return 1;
	case CB_SKIP:
		return 0;
	case CB_IGNORE:
		cb_plex_warning (warningopt, sline, _("%s ignored"), feature);
		return 0;
	case CB_ERROR:
		cb_plex_error (sline, _("%s used"), feature);
		return 0;
	case CB_UNCONFORMABLE:
		cb_plex_error (sline, _("%s does not conform to %s"),
			feature, cb_config_name);
		return 0;
	default:
		break;
	}
	return 0;
}

/* Warning/Error for config.c */
void
configuration_warning (const char *fname, const int line, const char *fmt, ...)
{
	va_list args;

	conf_error_displayed = 0;
	fputs (_("configuration warning:"), stderr);
	fputc (' ', stderr);

	/* Prefix */
	if (fname != last_error_file
		|| line != last_error_line) {
		last_error_file = fname;
		last_error_line = line;
		if (fname) {
			fprintf (stderr, "%s: ", fname);
		}
		if (line) {
			fprintf (stderr, "%d: ", line);
		}
	}

	/* Body */
	va_start(args, fmt);
	vfprintf (stderr, fmt, args);
	va_end(args);

	/* Postfix */
	putc('\n', stderr);
	fflush(stderr);

	if (sav_lst_file) {
		return;
	}
	warningcount++;
}
void
configuration_error (const char *fname, const int line,
                     const int finish_error, const char *fmt, ...)
{
	va_list args;

	configuration_error_head();

	/* Prefix */
	if (fname != last_error_file
		|| line != last_error_line) {
		last_error_file = fname;
		last_error_line = line;
		if (fname) {
			fprintf (stderr, "%s: ", fname);
		}
		if (line) {
			fprintf (stderr, "%d: ", line);
		}
	}

	/* Body */
	va_start(args, fmt);
	vfprintf (stderr, fmt, args);
	va_end(args);

	/* Postfix */
	if (!finish_error) {
		putc(';', stderr);
		putc('\n', stderr);
		putc('\t', stderr);
	} else {
		putc('\n', stderr);
		fflush(stderr);
	}

	if (sav_lst_file) {
		return;
	}

	if (++errorcount > cb_max_errors) {
		cobc_too_many_errors ();
	}
}

/* Generic warning/error routines */
void
cb_warning_x (int pref, cb_tree x, const char *fmt, ...)
{
	va_list ap;

	if (!pref) {
		return;
	}

	va_start (ap, fmt);
	print_error (x->source_file, x->source_line,
		(pref == COBC_WARN_AS_ERROR) ? _("error [-Werror]: ") : _("warning: "),
		fmt, ap);
	va_end (ap);

	if (sav_lst_file) {
		return;
	}
	if (pref == COBC_WARN_AS_ERROR) {
		if (++errorcount > cb_max_errors) {
			cobc_too_many_errors ();
		}
	} else {
		warningcount++;
	}
}

void
cb_error_x (cb_tree x, const char *fmt, ...)
{
	va_list ap;

	va_start (ap, fmt);
	print_error (x->source_file, x->source_line, ignore_error ?
		_("error (ignored): "):_("error: "), fmt, ap);
	va_end (ap);

	if (sav_lst_file) {
		return;
	}
	if (ignore_error) {
		warningcount++;
	} else {
		if (++errorcount > cb_max_errors) {
			cobc_too_many_errors ();
		}
	}
}

/**
 * verify if the given compiler option is supported by the current std/configuration
 * \param	x	tree whose position is used for raising warning/errors
 * \return	1 = ok/warning/obsolete, 0 = skip/ignore/error/unconformable
 */
unsigned int
cb_verify_x (cb_tree x, const enum cb_support tag, const char *feature)
{
	if (!x) {
		x = cobc_parse_malloc (sizeof (struct cb_tree_common));
		x->source_file = NULL;
		x->source_line = 0;
	}

	switch (tag) {
	case CB_OK:
		return 1;
	case CB_WARNING:
		cb_warning_x (COBC_WARN_FILLER, x, _("%s used"), feature);
		return 1;
	case CB_ARCHAIC:
		cb_warning_x (cb_warn_archaic, x, _("%s is archaic in %s"),
			feature, cb_config_name);
		return 1;
	case CB_OBSOLETE:
		cb_warning_x (cb_warn_obsolete, x, _("%s is obsolete in %s"),
			feature, cb_config_name);
		return 1;
	case CB_SKIP:
		return 0;
	case CB_IGNORE:
		cb_warning_x (warningopt, x, _("%s ignored"), feature);
		return 0;
	case CB_ERROR:
		cb_error_x (x, _("%s used"), feature);
		return 0;
	case CB_UNCONFORMABLE:
		cb_error_x (x, _("%s does not conform to %s"), feature, cb_config_name);
		return 0;
	default:
		break;
	}
	return 0;
}

/**
 * verify if the given compiler option is supported by the current std/configuration
 * current position is used for raising warning/errors
 * \returns	1 = ok/warning/obsolete, 0 = skip/ignore/error/unconformable
 */

unsigned int
cb_verify (const enum cb_support tag, const char *feature)
{
	return cb_verify_x (NULL, tag, feature);
}

void
redefinition_error (cb_tree x)
{
	struct cb_word	*w;

	w = CB_REFERENCE (x)->word;
	cb_error_x (x, _("redefinition of '%s'"), w->name);
	if (w->items) {
		if (CB_VALUE (w->items)->source_line == 0) {
			return;
		}
		listprint_suppress ();
		cb_error_x (CB_VALUE (w->items),
			    _("'%s' previously defined here"), w->name);
		listprint_restore ();
	}
}

void
redefinition_warning (cb_tree x, cb_tree y)
{
	struct cb_word	*w;
	cb_tree		z;

	w = CB_REFERENCE (x)->word;
	cb_warning_x (COBC_WARN_FILLER, x, _("redefinition of '%s'"), w->name);
	z = NULL;
	if (y) {
		z = y;
	} else if (w->items) {
		z = CB_VALUE (w->items);
	}

	if (z) {
		if (z->source_line == 0) {
			return;
		}
		listprint_suppress ();
		cb_warning_x (COBC_WARN_FILLER, z, _("'%s' previously defined here"), w->name);
		listprint_restore ();
	}
}

void
undefined_error (cb_tree x)
{
	struct cb_reference	*r = CB_REFERENCE (x);
	cb_tree			c;
	const char		*error_message;

	if (!errnamebuff) {
		errnamebuff = cobc_main_malloc ((size_t)COB_NORMAL_BUFF);
	}

	/* Get complete variable name */
	snprintf (errnamebuff, (size_t)COB_NORMAL_MAX, "%s", CB_NAME (x));
	errnamebuff[COB_NORMAL_MAX] = 0;
	for (c = r->chain; c; c = CB_REFERENCE (c)->chain) {
		strcat (errnamebuff, " IN ");
		strcat (errnamebuff, CB_NAME (c));
	}

	if (is_reserved_word (CB_NAME (x))) {
		error_message = _("'%s' cannot be used here");
	} else if (is_default_reserved_word (CB_NAME (x))) {
		error_message = _("'%s' is not defined, but is a reserved word in another dialect");
	} else {
		error_message = _("'%s' is not defined");
	}

	if (r->flag_optional) {
		cb_warning_x (COBC_WARN_FILLER, x, error_message, errnamebuff);
	} else {
		cb_error_x (x, error_message, errnamebuff);
	}
}

void
ambiguous_error (cb_tree x)
{
	struct cb_word	*w;
	struct cb_field	*p;
	struct cb_label	*l2;
	cb_tree		l;
	cb_tree		y;

	w = CB_REFERENCE (x)->word;
	if (w->error == 0) {
		if (!errnamebuff) {
			errnamebuff = cobc_main_malloc ((size_t)COB_NORMAL_BUFF);
		}
		/* Display error the first time */
		snprintf (errnamebuff, (size_t)COB_NORMAL_MAX, "%s", CB_NAME (x));
		errnamebuff[COB_NORMAL_MAX] = 0;
		for (l = CB_REFERENCE (x)->chain; l; l = CB_REFERENCE (l)->chain) {
			strcat (errnamebuff, " IN ");
			strcat (errnamebuff, CB_NAME (l));
		}
		cb_error_x (x, _("'%s' is ambiguous; needs qualification"), errnamebuff);
		w->error = 1;

		/* Display all fields with the same name */
		for (l = w->items; l; l = CB_CHAIN (l)) {
			y = CB_VALUE (l);
			errnamebuff[0] = 0;
			strcat (errnamebuff, w->name);
			switch (CB_TREE_TAG (y)) {
			case CB_TAG_FIELD:
				for (p = CB_FIELD (y)->parent; p; p = p->parent) {
					strcat (errnamebuff, " IN ");
					strcat (errnamebuff, cb_name (CB_TREE(p)));
				}
				break;
			case CB_TAG_LABEL:
				l2 = CB_LABEL (y);
				if (l2->section) {
					strcat (errnamebuff, " IN ");
					strcat (errnamebuff,
						(const char *)(l2->section->name));
				}
				break;
			default:
				break;
			}
			if (y->source_line == 0) {
				continue;
			}
			listprint_suppress ();
			cb_error_x (y, _("'%s' defined here"), errnamebuff);
			listprint_restore ();
		}
	}
}

/* error routine for flex */
void
flex_fatal_error (const char *msg, const char * filename, const int line_num)
{
	/* LCOV_EXCL_START */
	cobc_err_msg (_ ("fatal error: %s"), msg);
	cobc_abort (filename, line_num);
	/* LCOV_EXCL_STOP */
}

void
group_error (cb_tree x, const char *clause)
{
	cb_error_x (x, _("group item '%s' cannot have %s clause"),
		    cb_name (x), clause);
}

void
level_redundant_error (cb_tree x, const char *clause)
{
	const char		*s;
	const struct cb_field	*f;

	s = cb_name (x);
	f = CB_FIELD_PTR (x);
	if (f->flag_item_78) {
		cb_error_x (x, _("constant item '%s' cannot have a %s clause"), s, clause);
	} else {
		cb_error_x (x, _("level %02d item '%s' cannot have a %s clause"), f->level,
			    s, clause);
	}
}

void
level_require_error (cb_tree x, const char *clause)
{
	const char		*s;
	const struct cb_field	*f;

	s = cb_name (x);
	f = CB_FIELD_PTR (x);
	if (f->flag_item_78) {
		cb_error_x (x, _("constant item '%s' requires a %s clause"), s, clause);
	} else {
		cb_error_x (x, _("level %02d item '%s' requires a %s clause"), f->level,
			    s, clause);
	}
}

void
level_except_error (cb_tree x, const char *clause)
{
	const char		*s;
	const struct cb_field	*f;

	s = cb_name (x);
	f = CB_FIELD_PTR (x);
	if (f->flag_item_78) {
		cb_error_x (x, _("constant item '%s' can only have a %s clause"), s, clause);
	} else {
		cb_error_x (x, _("level %02d item '%s' can only have a %s clause"), f->level,
			    s, clause);
	}
}

/* routines for temporary disable listing output of warnings/errors */
void
listprint_suppress (void)
{
	if (cb_src_list_file) {
		sav_lst_file = cb_src_list_file;
		cb_src_list_file = NULL;
	}
}

void
listprint_restore (void)
{
	if (sav_lst_file) {
		cb_src_list_file = sav_lst_file;
		sav_lst_file = NULL;
	}
}
