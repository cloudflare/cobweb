/*
   Copyright (C) 2001-2012, 2014-2017 Free Software Foundation, Inc.
   Written by Keisuke Nishida, Roger While, Simon Sobisch, Ron Norman

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
#include <limits.h>

#include "cobc.h"
#include "tree.h"

/* sanity checks */
#if COB_MAX_FIELD_SIZE > INT_MAX
#error COB_MAX_FIELD_SIZE is too big, must be less than INT_MAX
#endif
#if COB_MAX_UNBOUNDED_SIZE > INT_MAX
#error COB_MAX_UNBOUNDED_SIZE is too big, must be less than INT_MAX
#endif

/* Global variables */

cb_tree			cb_depend_check = NULL;
size_t			cb_needs_01 = 0;

/* Local variables */

static struct cb_field	*last_real_field = NULL;
static int		occur_align_size = 0;
static const int	pic_digits[] = { 2, 4, 7, 9, 12, 14, 16, 18 };
#define CB_MAX_OPS	16
static int			op_pos = 1, op_val_pos;
static char			op_type [CB_MAX_OPS];
static char			op_prec [CB_MAX_OPS];
static cob_s64_t	op_val  [CB_MAX_OPS];
static int			op_scale[CB_MAX_OPS];

/* Is list of values really an expression */
static int
cb_is_expr (cb_tree ch)
{
	cb_tree			t, l;
	int				num;

	if (op_pos >= 0) {
		for (num=0; num < CB_MAX_OPS; num++) {
			op_type [num] = ' ';
			op_prec [num] = 0;
			op_val  [num] = 0;
		}
	}
	op_pos = op_val_pos = -1;
	num = 0;
	for (l = ch; l; l = CB_CHAIN (l)) {
		t = CB_VALUE (l);
		if (t && CB_LITERAL(t)) {
			if (++num > 1)
				return 1;
		}
	}
	return 0;
}

static void
cb_eval_op ( void )
{
	cob_s64_t	lval, rval, xval;
	int			lscale, rscale, xscale;

	if (op_pos >= 0
	 && op_val_pos > 0) {
		lval = op_val [op_val_pos-1];
		lscale = op_scale [op_val_pos-1];
		rval = op_val [op_val_pos];
		rscale = op_scale [op_val_pos];
		op_val_pos--;
		switch (op_type [op_pos]) {
		case '+':
		case '-':
			while (lscale > rscale) {
				rval = rval * 10;
				rscale++;
			}
			while (lscale < rscale) {
				lval = lval * 10;
				lscale++;
			}
			xscale = lscale;
			if (op_type [op_pos] == '+')
				xval = lval + rval;
			else
				xval = lval - rval;
			break;
		case '*':
			xscale = lscale + rscale;
			xval = lval * rval;
			break;
		case '/':
			while (rscale > 0) {
				lval = lval * 10;
				rscale--;
			}
			if (rval == 0) {
				xscale = 0;
				xval = 0;
				cb_error (_("Constant expression has Divide by ZERO"));
			} else {
				xscale = lscale;
				xval = lval / rval;
			}
			break;
		case '^':
			while (rscale > 0) {	/* Only whole number exponents */
				rval = rval / 10;
				rscale--;
			}
			if (rval == 0 || lval == 1) {
				xval = 1;
				xscale = 0;
			} else {
				xval = lval;
				xscale = lscale;
				while(--rval > 0) {
					xscale = xscale + lscale;
					xval = xval * lval;
				}
			}
			break;
		case '&':
			xscale = 0;
			xval = (lval && rval);
			break;
		case '|':
			xscale = 0;
			xval = (lval || rval);
			break;
		case '>':
			xscale = 0;
			xval = (lval > rval);
			break;
		case '<':
			xscale = 0;
			xval = (lval < rval);
			break;
		case '=':
			xscale = 0;
			xval = (lval == rval);
			break;
		case ']':
			xscale = 0;
			xval = (lval >= rval);
			break;
		case '[':
			xscale = 0;
			xval = (lval <= rval);
			break;
		case '~':
			xscale = 0;
			xval = (lval != rval);
			break;
		case '(':
			cb_error (_("missing right parenthesis"));
			op_pos--;
			return;
		default:
			op_pos--;
			return;
		}
		op_pos--;
		while (xscale > 0 
			&& (xval % 10) == 0) {
			xscale--;
			xval = xval / 10;
		}
		op_scale [op_val_pos] = xscale;
		op_val [op_val_pos] = xval;
	}
}

static void
cb_push_op ( char op, int prec )
{
	while (op_pos >= 0
	   &&  op_val_pos > 0
	   &&  prec > 0
	   &&  op_type [op_pos] != '('
	   &&  prec <= op_prec [op_pos]) {
		cb_eval_op ();
	}
	op_pos++;
	op_type [op_pos] = op;
	op_prec [op_pos] = (char) prec;
}

/* Evaluate expression and store as new Numeric Literal */
static cb_tree
cb_evaluate_expr (cb_tree ch, int normal_prec)
{
	cb_tree			t, l;
	cob_s64_t		xval;
	int				unop = 1, xscale, k;
	char			result[48];
	struct cb_literal	*lp;

	for (l = ch; l; l = CB_CHAIN (l)) {
		t = CB_VALUE (l);
		if (t && CB_LITERAL(t)) {
			lp = CB_LITERAL(t);
			if (CB_NUMERIC_LITERAL_P(t)) {
				xval = atoll((const char *)lp->data);
				xscale = lp->scale;
				if (unop) {
					if (lp->sign < 0)		/* Unary op, change sign */
						xval = -xval;
				} else {
					if (lp->sign < 0) {		/* Treat 'sign' as binary op */
						cb_push_op ('-', 4);
					} else if (lp->sign > 0) {
						cb_push_op ('+', 4);
					}
				}
				while (xscale > 0 
					&& (xval % 10) == 0) {	/* Remove decimal zeros */
					xscale--;
					xval = xval / 10;
				}
				op_val_pos++;
				op_val [op_val_pos] = xval;
				op_scale [op_val_pos] = xscale;
				unop = 0;
			} else {
				switch (lp->data[0]) {
				case '(':
					cb_push_op ('(', 0);
					unop = 1;
					break;
				case ')':
					unop = 0;
					for (k=op_pos; k >= 0 && op_type[k] != '('; k--);
					if (op_type [k] != '(')
						cb_error (_("missing left parenthesis"));
					while (op_pos >= 0
					   &&  op_val_pos > 0) {
						if (op_type [op_pos] == '(') {
							break;
						}
						cb_eval_op ();
					}
					if (op_pos >= 0
					 && op_type [op_pos] == '(')
						op_pos--;
					break;
				case '+':
					cb_push_op ('+', 4);
					unop = 1;
					break;
				case '-':
					cb_push_op ('-', 4);
					unop = 1;
					break;
				case '*':
					cb_push_op ('*', normal_prec ? 6 : 4);
					unop = 1;
					break;
				case '/':
					cb_push_op ('/', normal_prec ? 6 : 4);
					unop = 1;
					break;
				case '&':
					cb_push_op ('&', normal_prec ? 8 : 4);
					unop = 1;
					break;
				case '|':
					cb_push_op ('|', normal_prec ? 8 : 4);
					unop = 1;
					break;
				case '^':
					cb_push_op ('^', normal_prec ? 7 : 4);
					unop = 1;
					break;
				default:
					cb_error (_("invalid operator '%s' in expression"),lp->data);
					break;
				}
			}
		}
	}
	while (op_pos >= 0
	   &&  op_val_pos > 0) {
		if (op_type [op_pos] == '(') {
			cb_error (_("missing right parenthesis"));
			op_pos--;
			continue;
		}
		cb_eval_op ();
	}
	if (op_pos >= 0
	 && op_type [op_pos] == '(') {
		cb_error (_("missing right parenthesis"));
	} else if (op_pos >= 0) {
		cb_error (_("'%c' operator misplaced"),op_type [op_pos]);
	}
	xval	= op_val [0];
	xscale	= op_scale [0];
	while (xscale > 0) { 		/* Reduce to 'fixed point numeric' */
		xscale--;
		xval = xval / 10;
	}
	while (xscale < 0) { 		/* Reduce to 'fixed point numeric' */
		xscale++;
		xval = xval * 10;
	}
	sprintf(result, CB_FMT_LLD, xval);
	return cb_build_numeric_literal (0, result, xscale);
}

int
cb_get_level (cb_tree x)
{
	const unsigned char	*p;
	const char		*name;
	int			level;

	if (CB_INVALID_TREE (x)) {
		return 0;
	}
	name = CB_NAME (x);
	level = 0;
	/* Get level */
	for (p = (const unsigned char *)name; *p; p++) {
		if (!isdigit ((int)(*p))) {
			goto level_error;
		}
		level = level * 10 + (*p - '0');
		if (level > 88) {
			goto level_error;
		}
	}

	/* Check level */
	switch (level) {
	case 66:
	case 77:
	case 78:
	case 88:
		break;
	default:
		if (level < 1 || level > 49) {
			goto level_error;
		}
		break;
	}

	return level;

level_error:
	cb_error_x (x, _("invalid level number '%s'"), name);
	return 0;
}

cb_tree
cb_build_field_tree (cb_tree level, cb_tree name, struct cb_field *last_field,
		     enum cb_storage storage, struct cb_file *fn,
		     const int expl_level)
{
	struct cb_reference	*r;
	struct cb_field		*f;
	struct cb_field		*p;
	struct cb_field		*field_fill;
	cb_tree			dummy_fill;
	cb_tree			l;
	cb_tree			x;
	int			lv;

	if (!expl_level) {
		if (level == cb_error_node || name == cb_error_node) {
			return cb_error_node;
		}
		/* Check the level number */
		lv = cb_get_level (level);
		if (!lv) {
			return cb_error_node;
		}
	} else {
		lv = expl_level;
	}

	/* Build the field */
	r = CB_REFERENCE (name);
	f = CB_FIELD (cb_build_field (name));
	f->storage = storage;
	last_real_field = last_field;
	if (lv == 78) {
		f->level = 01;
		f->flag_item_78 = 1;
		f->flag_constant = 0;
		return CB_TREE (f);
	} else {
		f->level = lv;
	}
	if (f->level == 01 && storage == CB_STORAGE_FILE && fn) {
		if (fn->flag_external) {
			f->flag_external = 1;
			current_program->flag_has_external = 1;
		} else if (fn->flag_global) {
			f->flag_is_global = 1;
		}
	}
	if (last_field) {
		if (last_field->level == 77 && f->level != 01 &&
		    f->level != 77 && f->level != 66 && f->level != 88) {
			cb_error_x (name, _("level number must begin with 01 or 77"));
			return cb_error_node;
		}
	}

	/* Checks for redefinition */
	if (cb_warn_redefinition && r->word->count > 1 && !r->flag_filler_ref) {
		if (f->level == 01 || f->level == 77) {
			redefinition_warning (name, NULL);
		} else {
			for (l = r->word->items; l; l = CB_CHAIN (l)) {
				x = CB_VALUE (l);
				if (!CB_FIELD_P (x) ||
				    CB_FIELD (x)->level == 01 ||
				    CB_FIELD (x)->level == 77 ||
				    (last_field && f->level == last_field->level &&
				     CB_FIELD (x)->parent == last_field->parent)) {
					redefinition_warning (name, x);
					break;
				}
			}
		}
	}

	if (last_field && last_field->level == 88) {
		last_field = last_field->parent;
	}

	/* Link the field into the tree */
	if (f->level == 01 || f->level == 77) {
		/* Top level */
		cb_needs_01 = 0;
		if (last_field) {
			cb_field_founder (last_field)->sister = f;
		}
	} else if (!last_field || cb_needs_01) {
		/* Invalid top level */
		cb_error_x (name, _("level number must begin with 01 or 77"));
		return cb_error_node;
	} else if (f->level == 66) {
		/* Level 66 */
		f->parent = cb_field_founder (last_field);
		for (p = f->parent->children; p && p->sister; p = p->sister) ;
		if (p) {
			p->sister = f;
		}
	} else if (f->level == 88) {
		/* Level 88 */
		f->parent = last_field;
		if (last_real_field && last_real_field->level == 88) {
			/* Level 88 sister */
			last_real_field->sister = f;
		} else {
			/* First Level 88 on this item */
			last_field->validation = f;
			last_field = f;
		}
	} else if (f->level > last_field->level) {
		/* Lower level */
		last_field->children = f;
		f->parent = last_field;
	} else if (f->level == last_field->level) {
		/* Same level */
same_level:
		last_field->sister = f;
		f->parent = last_field->parent;
	} else {
		/* Upper level */
		for (p = last_field->parent; p /* <- silence warnings */; p = p->parent) {
			if (p->level == f->level) {
				last_field = p;
				goto same_level;
			}
			if (cb_relax_level_hierarchy && p->level < f->level) {
				break;
			}
		}
		if (cb_relax_level_hierarchy
		&& p /* <- silence warnings */) {
			dummy_fill = cb_build_filler ();
			field_fill = CB_FIELD (cb_build_field (dummy_fill));
			cb_warning_x (COBC_WARN_FILLER, name,
				      _("no previous data item of level %02d"),
				      f->level);
			field_fill->level = f->level;
			field_fill->flag_filler = 1;
			field_fill->storage = storage;
			field_fill->children = p->children;
			field_fill->parent = p;
			for (p = p->children; p; p = p->sister) {
				p->parent = field_fill;
			}
			field_fill->parent->children = field_fill;
			field_fill->sister = f;
			f->parent = field_fill->parent;
			/* last_field = field_fill; */
		} else {
			cb_error_x (name,
				    _("no previous data item of level %02d"),
				    f->level);
			return cb_error_node;
		}
	}

	/* Inherit parents properties */
	if (f->parent) {
		f->usage = f->parent->usage;
		f->indexes = f->parent->indexes;
		f->flag_sign_leading = f->parent->flag_sign_leading;
		f->flag_sign_separate = f->parent->flag_sign_separate;
		f->flag_is_global = f->parent->flag_is_global;
	}
	return CB_TREE (f);
}

struct cb_field *
cb_resolve_redefines (struct cb_field *field, cb_tree redefines)
{
	struct cb_field		*f;
	struct cb_reference	*r;
	const char		*name;
	cb_tree			x;
	cb_tree			candidate;
	cb_tree			items;

	r = CB_REFERENCE (redefines);
	name = CB_NAME (redefines);
	x = CB_TREE (field);

	/* Check qualification */
	if (r->chain) {
		cb_error_x (x, _("'%s' cannot be qualified here"), name);
		return NULL;
	}

	/* Check subscripts */
	if (r->subs) {
		cb_error_x (x, _("'%s' cannot be subscripted here"), name);
		return NULL;
	}

	/* Resolve the name in the current group (if any) */
	if (field->parent && field->parent->children) {
		for (f = field->parent->children; f; f = f->sister) {
			if (strcasecmp (f->name, name) == 0) {
				break;
			}
		}
		if (f == NULL) {
			cb_error_x (x, _("'%s' is not defined in '%s'"), name, field->parent->name);
			return NULL;
		}
	} else {
		/* Get last defined name */
		candidate = NULL;
		items = r->word->items;
		for (; items; items = CB_CHAIN (items)) {
			if (CB_FIELD_P (CB_VALUE (items))) {
				candidate = CB_VALUE (items);
			}
		}
		if (!candidate) {
			undefined_error (redefines);
			return NULL;
		}
		f = CB_FIELD_PTR (candidate);
	}

	/* Check level number */
	if (f->level != field->level) {
		cb_error_x (x, _("level number of REDEFINES entries must be identical"));
		return NULL;
	}

	if (!cb_indirect_redefines && f->redefines) {
		cb_error_x (x, _("'%s' is not the original definition"), f->name);
		return NULL;
	}

	/* Return the original definition */
	while (f->redefines) {
		f = f->redefines;
	}
	return f;
}

static COB_INLINE COB_A_INLINE void
emit_incompatible_pic_and_usage_error (cb_tree item, const enum cb_usage usage)
{
	cb_error_x (item, _("PICTURE clause not compatible with USAGE %s"),
		    cb_get_usage_string (usage));
}

static unsigned int
check_picture_item (cb_tree x, struct cb_field *f)
{
	char			*pp;
	struct cb_literal	*lp;
	int			vorint;
	char			pic[24];

	if (f->storage == CB_STORAGE_SCREEN) {
		/* RXWRXW Fixme - Corner cases */
		if (f->values) {
			sprintf (pic, "X(%d)", (int)CB_LITERAL(CB_VALUE(f->values))->size);
		} else if (f->screen_from) {
			sprintf (pic, "X(%d)", (int)CB_FIELD_PTR (f->screen_from)->size);
		} else if (f->screen_to) {
			sprintf (pic, "X(%d)", (int)CB_FIELD_PTR (f->screen_to)->size);
		} else {
			f->flag_no_field = 1;
			strcpy (pic, "X(1)");
		}
		f->pic = CB_PICTURE (cb_build_picture (pic));
		return 0;
	}
	if (f->flag_item_78) {
		if (!f->values || CB_VALUE(f->values) == cb_error_node) {
			level_require_error (x, "VALUE");
			return 1;
		}
		f->count++;
		lp = CB_LITERAL(CB_VALUE(f->values));
		if (CB_NUMERIC_LITERAL_P(CB_VALUE(f->values))) {
			memset (pic, 0, sizeof (pic));
			pp = pic;
			if (lp->sign) {
				*pp++ = 'S';
			}
			vorint = (int)lp->size - lp->scale;
			if (vorint) {
				pp += sprintf (pp, "9(%d)", vorint);
			}
			if (lp->scale) {
				sprintf (pp, "V9(%d)", lp->scale);
			}
			if (lp->size < 10) {
				f->usage = CB_USAGE_COMP_5;
			} else {
				f->usage = CB_USAGE_DISPLAY;
			}
			f->pic = CB_PICTURE (cb_build_picture (pic));
			f->pic->category = CB_CATEGORY_NUMERIC;
		} else {
			sprintf (pic, "X(%d)", (int)lp->size);
			f->pic = CB_PICTURE (cb_build_picture (pic));
			f->pic->category = CB_CATEGORY_ALPHANUMERIC;
			f->usage = CB_USAGE_DISPLAY;
		}
		return 0;
	}

	if (f->level == 1) {
		cb_error_x (x, _("PICTURE clause required for '%s'"),
			    cb_name (x));
		return 1;
	}
	if (!f->values || CB_VALUE(f->values) == cb_error_node) {
		cb_error_x (x, _("PICTURE clause required for '%s'"),
			    cb_name (x));
		return 1;
	}
	if (CB_NUMERIC_LITERAL_P(CB_VALUE(f->values))) {
		cb_error_x (x, _("a non-numeric literal is expected for '%s'"),
			    cb_name (x));
		return 1;
	}
	vorint = (int)CB_LITERAL(CB_VALUE(f->values))->size;
	/* Checkme: should we raise an error for !cb_relaxed_syntax_checks? */
	cb_warning_x (warningopt, x, _("defining implicit picture size %d for '%s'"),
		    vorint, cb_name (x));
	sprintf (pic, "X(%d)", vorint);
	f->pic = CB_PICTURE (cb_build_picture (pic));
	f->pic->category = CB_CATEGORY_ALPHANUMERIC;
	f->usage = CB_USAGE_DISPLAY;
	return 0;
}

static unsigned int
validate_field_1 (struct cb_field *f)
{
	cb_tree		x;
	cb_tree		l;
	struct cb_field *p;
	cob_pic_symbol	*pstr;
	int		n;
	int		need_picture;
	unsigned int	ret;
	int		i;

	/* LCOV_EXCL_START */
	if (unlikely(!f)) {	/* checked to keep the analyzer happy */
		cobc_err_msg (_("call to %s with NULL pointer"), "validate_field_1");
		COBC_ABORT();
	}
	/* LCOV_EXCL_STOP */

	if (f->flag_invalid) {
		return 1;
	}
	x = CB_TREE (f);
	if (f->flag_any_length) {
		if (f->storage != CB_STORAGE_LINKAGE) {
			cb_error_x (x, _("'%s' ANY LENGTH only allowed in LINKAGE"), cb_name (x));
			return 1;
		}
		if (f->level != 01) {
			cb_error_x (x, _("'%s' ANY LENGTH must be 01 level"), cb_name (x));
			return 1;
		}
		if (f->flag_item_based || f->flag_external) {
			cb_error_x (x, _("'%s' ANY LENGTH cannot be BASED/EXTERNAL"), cb_name (x));
			return 1;
		}
		if (f->flag_occurs || f->depending ||
		    f->children || f->values || f->flag_blank_zero) {
			cb_error_x (x, _("'%s' ANY LENGTH has invalid definition"), cb_name (x));
			return 1;
		}
		if (!f->pic) {
			if (f->flag_any_numeric) {
				f->pic = CB_PICTURE (cb_build_picture ("9"));
			} else {
				f->pic = CB_PICTURE (cb_build_picture ("X"));
			}
		} else if (f->flag_any_numeric) {
			if (f->pic->category != CB_CATEGORY_NUMERIC) 
				cb_error (_("'%s' ANY NUMERIC must be PIC 9"),
					  f->name);
		} else if (f->pic->category != CB_CATEGORY_ALPHANUMERIC) {
			cb_error (_("'%s' ANY LENGTH must be PIC X or PIC N"),
				  f->name);
		}
		/*
		  TO-DO: Replace pic->orig check with f->usage ==
		  CB_USAGE_NATIONAL. Currently NATIONAL items are marked as
		  having ALPHANUMERIC category and USAGE DISPLAY.
		*/
		if (!((f->pic->size == 1 && f->usage == CB_USAGE_DISPLAY)
		      || (f->pic->size == 2 && *f->pic->orig == 'N'))) {
			if (f->flag_any_numeric) {
				cb_error_x (x, _("'%s' ANY NUMERIC has invalid definition"), cb_name (x));
			} else {
				cb_error_x (x, _("'%s' ANY LENGTH has invalid definition"), cb_name (x));
			}
			return 1;
		}
		f->count++;
		return 0;
	}

	if (f->level == 77) {
		if (f->storage != CB_STORAGE_WORKING &&
		    f->storage != CB_STORAGE_LOCAL &&
		    f->storage != CB_STORAGE_LINKAGE) {
			cb_error_x (x, _("'%s' 77 level not allowed here"), cb_name (x));
		}
	}
	if (f->flag_external) {
		if (f->level != 01 && f->level != 77) {
			cb_error_x (x, _("'%s' EXTERNAL must be specified at 01/77 level"), cb_name (x));
		}
		if (f->storage != CB_STORAGE_WORKING &&
		    f->storage != CB_STORAGE_FILE) {
			cb_error_x (x, _("'%s' EXTERNAL can only be specified in WORKING-STORAGE section"),
				    cb_name (x));
		}
		if (f->flag_item_based) {
			cb_error_x (x, _("'%s' EXTERNAL and BASED are mutually exclusive"), cb_name (x));
		}
		if (f->redefines) {
			cb_error_x (x, _("'%s' EXTERNAL not allowed with REDEFINES"), cb_name (x));
		}
	}
	if (f->flag_item_based) {
		if (f->storage != CB_STORAGE_WORKING &&
		    f->storage != CB_STORAGE_LOCAL &&
		    f->storage != CB_STORAGE_LINKAGE) {
			cb_error_x (x, _("'%s' BASED not allowed here"), cb_name (x));
		}
		if (f->redefines) {
			cb_error_x (x, _("'%s' BASED not allowed with REDEFINES"), cb_name (x));
		}
		if (f->level != 01 && f->level != 77) {
			cb_error_x (x, _("'%s' BASED only allowed at the 01 and 77 levels"), cb_name (x));
		}
	}

	/* Validate OCCURS */
	if (f->flag_occurs) {
		if (!cb_verify (cb_top_level_occurs_clause, "01/77 OCCURS") &&
		    (f->level == 01 || f->level == 77)) {
			level_redundant_error (x, "OCCURS");
		}
		for (l = f->index_list; l; l = CB_CHAIN (l)) {
			CB_FIELD_PTR (CB_VALUE (l))->flag_is_global = f->flag_is_global;
		}
	}

	/* Validate OCCURS DEPENDING */
	if (f->depending) {
		/* Cache field for later checking */
		cb_depend_check = cb_list_add (cb_depend_check, x);

		if (!cb_complex_odo) {
			/* The data item that contains a OCCURS DEPENDING clause shall not
			   be subordinate to a data item that has an OCCURS clause */
			for (p = f->parent; p; p = p->parent) {
				if (p->flag_occurs) {
					cb_error_x (CB_TREE (p),
							_("'%s' cannot have the OCCURS clause due to '%s'"),
							cb_name (CB_TREE (p)),
							cb_name (x));
					break;
				}
			}
		}
	}

	/* Validate REDEFINES */
	if (f->redefines && f->level != 66) {
		/* Check OCCURS */
		if (f->redefines->flag_occurs) {
			cb_warning_x (COBC_WARN_FILLER, x,
				      _("the original definition '%s' should not have OCCURS clause"),
				      f->redefines->name);
		}

		/* Check definition */
		for (p = f->redefines->sister; p && p != f; p = p->sister) {
			if (!p->redefines) {
				cb_error_x (x, _("REDEFINES must follow the original definition"));
				break;
			}
		}

		/* Check variable occurrence */
		if (f->depending || cb_field_variable_size (f)) {
			cb_error_x (x, _("'%s' cannot be variable length"), f->name);
		}
		if (cb_field_variable_size (f->redefines)) {
			cb_error_x (x, _("the original definition '%s' cannot be variable length"),
				    f->redefines->name);
		}
	}

	if (f->level == 66) {
		return 0;
	}

	if (f->children) {
		/* Group item */

		if (f->pic) {
			group_error (x, "PICTURE");
		}
		if (f->flag_justified) {
			group_error (x, "JUSTIFIED RIGHT");
		}
		if (f->flag_blank_zero) {
			group_error (x, "BLANK WHEN ZERO");
		}

		ret = 0;
		if (f->storage == CB_STORAGE_SCREEN &&
		    (f->screen_from || f->screen_to || f->values || f->pic)) {
			cb_error_x (x, _("SCREEN group item '%s' has invalid clause"),
				    cb_name (x));
			ret = 1;
		}
		for (f = f->children; f; f = f->sister) {
			ret |= validate_field_1 (f);
		}
		if (ret) {
			return 1;
		}
	} else {
		/* Elementary item */

		/* Validate PICTURE */
		switch (f->usage) {
		case CB_USAGE_INDEX:
		case CB_USAGE_HNDL:
		case CB_USAGE_HNDL_WINDOW:
		case CB_USAGE_HNDL_SUBWINDOW:
		case CB_USAGE_HNDL_FONT:
		case CB_USAGE_HNDL_THREAD:
		case CB_USAGE_HNDL_MENU:
		case CB_USAGE_HNDL_VARIANT:
		case CB_USAGE_HNDL_LM:
		case CB_USAGE_LENGTH:
		case CB_USAGE_OBJECT:
		case CB_USAGE_POINTER:
		case CB_USAGE_PROGRAM_POINTER:
		case CB_USAGE_FLOAT:
		case CB_USAGE_DOUBLE:
		case CB_USAGE_LONG_DOUBLE:
		case CB_USAGE_FP_BIN32:
		case CB_USAGE_FP_BIN64:
		case CB_USAGE_FP_BIN128:
		case CB_USAGE_FP_DEC64:
		case CB_USAGE_FP_DEC128:
		case CB_USAGE_SIGNED_CHAR:
		case CB_USAGE_SIGNED_SHORT:
		case CB_USAGE_SIGNED_INT:
		case CB_USAGE_SIGNED_LONG:
		case CB_USAGE_UNSIGNED_CHAR:
		case CB_USAGE_UNSIGNED_SHORT:
		case CB_USAGE_UNSIGNED_INT:
		case CB_USAGE_UNSIGNED_LONG:
			need_picture = 0;
			break;
		default:
			if (!f->flag_is_external_form) {
				need_picture = 1;
			} else {
				need_picture = 0;
			}
			break;
		}

		if (f->pic == NULL && need_picture != 0) {
			if (check_picture_item (x, f)) {
				return 1;
			}
		}
		if (f->pic != NULL && need_picture == 0) {
			cb_error_x (x, _("'%s' cannot have PICTURE clause"),
				    cb_name (x));
		}

		/* Validate USAGE */
		switch (f->usage) {
		case CB_USAGE_DISPLAY:
			if (current_program->flag_trailing_separate &&
			    f->pic &&
			    f->pic->category == CB_CATEGORY_NUMERIC &&
			    !f->flag_sign_leading) {
				f->flag_sign_separate = 1;
			}
			break;
		case CB_USAGE_SIGNED_CHAR:
			f->usage = CB_USAGE_COMP_5;
			f->pic = cb_build_binary_picture ("BINARY-CHAR", 2, 1);
			f->flag_real_binary = 1;
			break;
		case CB_USAGE_SIGNED_SHORT:
			f->usage = CB_USAGE_COMP_5;
			f->pic = cb_build_binary_picture ("BINARY-SHORT", 4, 1);
			f->flag_real_binary = 1;
			break;
		case CB_USAGE_SIGNED_INT:
			f->usage = CB_USAGE_COMP_5;
			f->pic = cb_build_binary_picture ("BINARY-LONG", 9, 1);
			f->flag_real_binary = 1;
			break;
		case CB_USAGE_SIGNED_LONG:
			f->usage = CB_USAGE_COMP_5;
			f->pic = cb_build_binary_picture ("BINARY-DOUBLE", 18, 1);
			f->flag_real_binary = 1;
			break;
		case CB_USAGE_UNSIGNED_CHAR:
			f->usage = CB_USAGE_COMP_5;
			f->pic = cb_build_binary_picture ("BINARY-CHAR", 2, 0);
			f->flag_real_binary = 1;
			break;
		case CB_USAGE_UNSIGNED_SHORT:
			f->usage = CB_USAGE_COMP_5;
			f->pic = cb_build_binary_picture ("BINARY-SHORT", 4, 0);
			f->flag_real_binary = 1;
			break;
		case CB_USAGE_UNSIGNED_INT:
			f->usage = CB_USAGE_COMP_5;
			f->pic = cb_build_binary_picture ("BINARY-LONG", 9, 0);
			f->flag_real_binary = 1;
			break;
		case CB_USAGE_UNSIGNED_LONG:
			f->usage = CB_USAGE_COMP_5;
			f->pic = cb_build_binary_picture ("BINARY-DOUBLE", 18, 0);
			f->flag_real_binary = 1;
			break;
		case CB_USAGE_BINARY:
		case CB_USAGE_PACKED:
		case CB_USAGE_BIT:
			if (f->pic->category != CB_CATEGORY_NUMERIC) {
				emit_incompatible_pic_and_usage_error (x, f->usage);
			}
			break;
		case CB_USAGE_COMP_6:
			if (f->pic->category != CB_CATEGORY_NUMERIC) {
				emit_incompatible_pic_and_usage_error (x, f->usage);
			}
			if (f->pic->have_sign) {
				cb_warning_x (COBC_WARN_FILLER, x, _("'%s' COMP-6 with sign - changing to COMP-3"), cb_name (x));
				f->usage = CB_USAGE_PACKED;
			}
			break;
		case CB_USAGE_COMP_5:
			f->flag_real_binary = 1;
		case CB_USAGE_COMP_X:
			if (f->pic) {
				if (f->pic->category != CB_CATEGORY_NUMERIC &&
				    f->pic->category != CB_CATEGORY_ALPHANUMERIC) {
					emit_incompatible_pic_and_usage_error (x, f->usage);
				}
			}
			break;
		default:
			break;
		}

		/* Validate SIGN */
		if (f->flag_sign_clause) {
			if (!(f->pic && f->pic->have_sign)) {
				cb_error_x (x, _("elementary items with SIGN clause must have S in PICTURE"));
			} else if (f->usage != CB_USAGE_DISPLAY
				   && f->usage != CB_USAGE_NATIONAL) {
				cb_error_x (x, _("elementary items with SIGN clause must be USAGE DISPLAY or NATIONAL"));
			}
		}

		/* Validate JUSTIFIED RIGHT */
		if (f->flag_justified) {
			switch (f->pic->category) {
			case CB_CATEGORY_ALPHABETIC:
			case CB_CATEGORY_ALPHANUMERIC:
				break;
			default:
				cb_error_x (x, _("'%s' cannot have JUSTIFIED RIGHT"), cb_name (x));
				break;
			}
		}

		/* Validate SYNCHRONIZED */

		/* Validate BLANK ZERO */
		if (f->flag_blank_zero) {
			if (f->pic->have_sign
			    && f->pic->category != CB_CATEGORY_NUMERIC_EDITED) {
				cb_error_x (x, _("'%s' cannot have S in PICTURE string and BLANK WHEN ZERO"),
					    cb_name (x));
			}

			if (f->usage != CB_USAGE_DISPLAY && f->usage != CB_USAGE_NATIONAL) {
				cb_error_x (x, _("'%s' cannot have BLANK WHEN ZERO without being USAGE DISPLAY or NATIONAL"),
					    cb_name (x));
			}

			switch (f->pic->category) {
			case CB_CATEGORY_NUMERIC:
				/* Reconstruct the picture string */
				if (f->pic->scale > 0) {
					/* Size for genned string */
					if (f->pic->have_sign) {
						n = 4;
					} else {
						n = 3;
					}
					f->pic->str = cobc_parse_malloc ((size_t)n * sizeof (cob_pic_symbol));
					pstr = f->pic->str;
					if (f->pic->have_sign) {
						pstr->symbol = '+';
						pstr->times_repeated = 1;
						++pstr;
					}
					pstr->symbol = '9';
					pstr->times_repeated = (int)f->pic->digits - f->pic->scale;
					++pstr;

					pstr->symbol = 'V';
					pstr->times_repeated = 1;
					++pstr;

					pstr->symbol = '9';
					pstr->times_repeated = f->pic->scale;
					++pstr;

					f->pic->size++;
				} else {
					/* Size for genned string */
					if (f->pic->have_sign) {
						n = 2;
					} else {
						n = 1;
					}
					f->pic->str = cobc_parse_malloc ((size_t)n * sizeof(cob_pic_symbol));
					pstr = f->pic->str;
					if (f->pic->have_sign) {
						pstr->symbol = '+';
						pstr->times_repeated = 1;
						++pstr;
					}
					pstr->symbol = '9';
					pstr->times_repeated = f->pic->digits;
				}
				f->pic->lenstr = n;
				f->pic->category = CB_CATEGORY_NUMERIC_EDITED;
				break;
			case CB_CATEGORY_NUMERIC_EDITED:
				for (i = 0; f->pic->str[i].symbol != '\0'; ++i) {
					if (f->pic->str[i].symbol == '*') {
						cb_error_x (x, _("'%s' cannot have * in PICTURE string and BLANK WHEN ZERO"),
							cb_name (x));
						break;
					}
				}
				break;
			default:
				cb_error_x (x, _("'%s' is not numeric, so cannot have BLANK WHEN ZERO"), cb_name (x));
				break;
			}
		}

		/* Validate VALUE */
		if (f->values) {
			if (CB_PAIR_P (CB_VALUE (f->values)) || CB_CHAIN (f->values)) {
				cb_error_x (x, _("only level 88 items may have multiple values"));
			}

			/* ISO+IEC+1989-2002: 13.16.42.2-10 */
			if (cb_warn_ignored_initial_val) {
				for (p = f; p; p = p->parent) {
					if (p->flag_external) {
						cb_warning_x (cb_warn_ignored_initial_val, x,
							_("initial VALUE clause ignored for %s item '%s'"),
							"EXTERNAL", cb_name (CB_TREE(f)));
					} else if (p->redefines) {
						cb_warning_x (cb_warn_ignored_initial_val, x,
							_("initial VALUE clause ignored for %s item '%s'"),
							"REDEFINES", cb_name (CB_TREE(f)));
					}
				}
			}
		}

		/* Validate FULL */
		if (f->screen_flag & COB_SCREEN_FULL
			&& f->pic && f->pic->category == CB_CATEGORY_NUMERIC) {
			cb_warning_x (warningopt, x,
				_("FULL has no effect on numeric items; you may want REQUIRED or PIC Z"));
		}
	}

	return 0;
}

static void
setup_parameters (struct cb_field *f)
{
	unsigned int	flag_local;
	char		pic[8];

	/* Determine the class */
	if (f->children) {
		/* Group field */
		flag_local = f->flag_local;
		for (f = f->children; f; f = f->sister) {
			f->flag_local = !!flag_local;
			setup_parameters (f);
		}
	} else {
		/* Regular field */
		switch (f->usage) {
		case CB_USAGE_BINARY:
#ifndef WORDS_BIGENDIAN
			if (cb_binary_byteorder == CB_BYTEORDER_BIG_ENDIAN) {
				f->flag_binary_swap = 1;
			}
#endif
			break;

		case CB_USAGE_INDEX:
		case CB_USAGE_HNDL:
		case CB_USAGE_HNDL_WINDOW:
		case CB_USAGE_HNDL_SUBWINDOW:
		case CB_USAGE_HNDL_FONT:
		case CB_USAGE_HNDL_THREAD:
		case CB_USAGE_HNDL_MENU:
		case CB_USAGE_HNDL_VARIANT:
		case CB_USAGE_HNDL_LM:
			f->pic = CB_PICTURE (cb_build_picture ("S9(9)"));
			break;

		case CB_USAGE_LENGTH:
			f->pic = CB_PICTURE (cb_build_picture ("9(9)"));
			break;

		case CB_USAGE_POINTER:
		case CB_USAGE_PROGRAM_POINTER:
			f->pic = CB_PICTURE (cb_build_picture ("9(10)"));
			break;
		case CB_USAGE_FLOAT:
			f->pic = CB_PICTURE (cb_build_picture ("S9(7)V9(8)"));
			break;
		case CB_USAGE_DOUBLE:
			f->pic = CB_PICTURE (cb_build_picture ("S9(17)V9(17)"));
			break;
		case CB_USAGE_FP_DEC64:
			/* RXWRXW - Scale Fix me */
			f->pic = CB_PICTURE (cb_build_picture ("S9(17)V9(16)"));
			break;
		case CB_USAGE_FP_DEC128:
			/* RXWRXW - Scale Fix me */
			f->pic = CB_PICTURE (cb_build_picture ("S999V9(34)"));
			break;

		case CB_USAGE_COMP_5:
			f->flag_real_binary = 1;
		case CB_USAGE_COMP_X:
			if (f->pic->category == CB_CATEGORY_ALPHANUMERIC) {
				if (f->pic->size > 8) {
					strcpy (pic, "9(36)");
				} else {
					sprintf (pic, "9(%d)", pic_digits[f->pic->size - 1]);
				}
				f->pic = CB_PICTURE (cb_build_picture (pic));
			}
#ifndef WORDS_BIGENDIAN
			if (f->usage == CB_USAGE_COMP_X &&
			    cb_binary_byteorder == CB_BYTEORDER_BIG_ENDIAN) {
				f->flag_binary_swap = 1;
			}
#endif
			break;

		default:
			break;
		}
	}
}

static void
compute_binary_size (struct cb_field *f, const int size)
{
	if (cb_binary_size == CB_BINARY_SIZE_1_2_4_8) {
		f->size = ((size <= 2) ? 1 :
			   (size <= 4) ? 2 :
			   (size <= 9) ? 4 : (size <= 18) ? 8 : 16);
		return;
	}
	if (cb_binary_size == CB_BINARY_SIZE_2_4_8) {
		if (f->flag_real_binary && size <= 2) {
			f->size = 1;
		} else {
			f->size = ((size <= 4) ? 2 :
				   (size <= 9) ? 4 : (size <= 18) ? 8 : 16);
		}
		return;
	}
	if (cb_binary_size != CB_BINARY_SIZE_1__8) {
		f->size = size;
		return;
	}
	if (f->pic->have_sign) {
		switch (size) {
		case 0:
		case 1:
		case 2:
			f->size = 1;
			return;
		case 3:
		case 4:
			f->size = 2;
			return;
		case 5:
		case 6:
			f->size = 3;
			return;
		case 7:
		case 8:
		case 9:
			f->size = 4;
			return;
		case 10:
		case 11:
			f->size = 5;
			return;
		case 12:
		case 13:
		case 14:
			f->size = 6;
			return;
		case 15:
		case 16:
			f->size = 7;
			return;
		case 17:
		case 18:
			f->size = 8;
			return;
		case 19:
		case 20:
		case 21:
			f->size = 9;
			return;
		case 22:
		case 23:
			f->size = 10;
			return;
		case 24:
		case 25:
		case 26:
			f->size = 11;
			return;
		case 27:
		case 28:
			f->size = 12;
			return;
		case 29:
		case 30:
		case 31:
			f->size = 13;
			return;
		case 32:
		case 33:
			f->size = 14;
			return;
		case 34:
		case 35:
			f->size = 15;
			return;
		default:
			f->size = 16;
			return;
		}
	}
	switch (size) {
	case 0:
	case 1:
	case 2:
		f->size = 1;
		return;
	case 3:
	case 4:
		f->size = 2;
		return;
	case 5:
	case 6:
	case 7:
		f->size = 3;
		return;
	case 8:
	case 9:
		f->size = 4;
		return;
	case 10:
	case 11:
	case 12:
		f->size = 5;
		return;
	case 13:
	case 14:
		f->size = 6;
		return;
	case 15:
	case 16:
		f->size = 7;
		return;
	case 17:
	case 18:
	case 19:
		f->size = 8;
		return;
	case 20:
	case 21:
		f->size = 9;
		return;
	case 22:
	case 23:
	case 24:
		f->size = 10;
		return;
	case 25:
	case 26:
		f->size = 11;
		return;
	case 27:
	case 28:
		f->size = 12;
		return;
	case 29:
	case 30:
	case 31:
		f->size = 13;
		return;
	case 32:
	case 33:
		f->size = 14;
		return;
	case 34:
	case 35:
	case 36:
		f->size = 15;
		return;
	default:
		f->size = 16;
		return;
	}
}

static struct cb_field *
get_last_child (struct cb_field *f)
{
	do {
		f = f->children;
		while (f->sister) {
			f = f->sister;
		}
	} while (f->children);

	return f;
}

static int
compute_size (struct cb_field *f)
{
	struct cb_field	*c;
	int		size;
	cob_u64_t	size_check;
	int		align_size;
	int		pad;
	int		unbounded_items = 0;
	int		unbounded_parts = 1;

	int maxsz;
	struct cb_field *c0;

	if (f->level == 66) {
		/* Rename */
		if (f->rename_thru) {
			f->size = f->rename_thru->offset + f->rename_thru->size -
				  f->redefines->offset;
		} else {
			f->size = f->redefines->size;
		}
		return f->size;
	}

	if (f->children) {
		/* Groups */
		if (f->flag_synchronized && warningopt) {
			cb_warning_x (COBC_WARN_FILLER, CB_TREE(f), _("ignoring SYNCHRONIZED for group item '%s'"),
				    cb_name (CB_TREE (f)));
		}
unbounded_again:
		size_check = 0;
		occur_align_size = 1;
		for (c = f->children; c; c = c->sister) {
			if (c->redefines) {
				c->offset = c->redefines->offset;
				compute_size (c);
				/* Increase the size if redefinition is larger */
				if (c->level != 66 &&
				    c->size * c->occurs_max >
				    c->redefines->size * c->redefines->occurs_max) {
					if (cb_larger_redefines_ok) {
						cb_warning_x (COBC_WARN_FILLER, CB_TREE (c),
							      _("size of '%s' larger than size of '%s'"),
							      c->name, c->redefines->name);
						maxsz = c->redefines->size * c->redefines->occurs_max;
						for (c0 = c->redefines->sister; c0 != c; c0 = c0->sister) {
							if (c0->size * c0->occurs_max > maxsz) {
								maxsz = c0->size * c0->occurs_max;
							}
						}
						if (c->size * c->occurs_max > maxsz) {
							size_check += (c->size * c->occurs_max) - maxsz;
						}
					} else {
						cb_error_x (CB_TREE (c),
							    _("size of '%s' larger than size of '%s'"),
							    c->name, c->redefines->name);
					}
				}
			} else {
				c->offset = f->offset + (int) size_check;
				compute_size (c);
				if (c->flag_unbounded) {
					unbounded_items++;
					c->occurs_max = (COB_MAX_UNBOUNDED_SIZE / c->size / unbounded_parts) - 1;
				}
				size_check += c->size * c->occurs_max;

				/* Word alignment */
				if (c->flag_synchronized &&
				    cb_verify (cb_synchronized_clause, "SYNC")) {
					align_size = 1;
					switch (c->usage) {
					case CB_USAGE_BINARY:
					case CB_USAGE_COMP_5:
					case CB_USAGE_COMP_X:
					case CB_USAGE_FLOAT:
					case CB_USAGE_DOUBLE:
					case CB_USAGE_LONG_DOUBLE:
					case CB_USAGE_FP_BIN32:
					case CB_USAGE_FP_BIN64:
					case CB_USAGE_FP_BIN128:
					case CB_USAGE_FP_DEC64:
					case CB_USAGE_FP_DEC128:
						if (c->size == 2 ||
						    c->size == 4 ||
						    c->size == 8 ||
						    c->size == 16) {
							align_size = c->size;
						}
						break;
					case CB_USAGE_INDEX:
					case CB_USAGE_HNDL:
					case CB_USAGE_HNDL_WINDOW:
					case CB_USAGE_HNDL_SUBWINDOW:
					case CB_USAGE_HNDL_FONT:
					case CB_USAGE_HNDL_THREAD:
					case CB_USAGE_HNDL_MENU:
					case CB_USAGE_HNDL_VARIANT:
					case CB_USAGE_HNDL_LM:
					case CB_USAGE_LENGTH:
						align_size = sizeof (int);
						break;
					case CB_USAGE_OBJECT:
					case CB_USAGE_POINTER:
					case CB_USAGE_PROGRAM_POINTER:
						align_size = sizeof (void *);
						break;
					default:
						break;
					}
					if (c->offset % align_size != 0) {
						pad = align_size - (c->offset % align_size);
						c->offset += pad;
						size_check += pad;
					}
					if (align_size > occur_align_size) {
						occur_align_size = align_size;
					}
				}
			}
		}
		/* Ensure items within OCCURS are aligned correctly. */
		if (f->occurs_max > 1 && (size_check % occur_align_size) != 0) {
			pad = occur_align_size - (size_check % occur_align_size);
			size_check += pad;
			/*
			  Add padding to last item, which will be (partly)
			  responsible for misalignment. If the item is not SYNC,
			  we have no problem. If it is SYNC, then it has been
			  aligned on a smaller boundary than occur_align_size: a
			  2-, 4- or 8-byte boundary. The needed padding will
			  be a multiple of 2, 4 or 8 bytes, so adding extra
			  padding will not break its alignment.
			*/
			if (f->children) {
			        get_last_child (f)->offset += pad;
			} else {
				/* ToDo: add appropriate message (untranslated) */
				COBC_ABORT ();	/* LCOV_EXCL_LINE */
			}
		}
		/* size check for group items */
		if (unbounded_items) {
			if (size_check > COB_MAX_UNBOUNDED_SIZE) {
				/* Hack: run again for finding the correct max_occurs for unbounded items */
				if (unbounded_parts == 1 && unbounded_items != 1) {
					unbounded_parts = unbounded_items;
				} else {
					unbounded_parts++;
				}
				goto unbounded_again;
			}
		} else if (size_check > COB_MAX_FIELD_SIZE) {
			cb_error_x (CB_TREE (f),
					_("'%s' cannot be larger than %d bytes"),
					f->name, COB_MAX_FIELD_SIZE);
		}
		f->size = (int) size_check;
	} else if (!f->flag_is_external_form) {
		/* Elementary item */
		switch (f->usage) {
		case CB_USAGE_COMP_X:
			if (f->pic->category == CB_CATEGORY_ALPHANUMERIC) {
				break;
			}
			size = f->pic->size;
			f->size = ((size <= 2) ? 1 : (size <= 4) ? 2 :
				   (size <= 7) ? 3 : (size <= 9) ? 4 :
				   (size <= 12) ? 5 : (size <= 14) ? 6 :
				   (size <= 16) ? 7 : (size <= 19) ? 8 :
				   (size <= 21) ? 9 : (size <= 24) ? 10 :
				   (size <= 26) ? 11 : (size <= 28) ? 12 :
				   (size <= 31) ? 13 : (size <= 33) ? 14 :
				   (size <= 36) ? 15 : 16);
			break;
		case CB_USAGE_BINARY:
		case CB_USAGE_COMP_5:
			size = f->pic->size;
#if	0	/* RXWRXW - Max binary */
			if (size > COB_MAX_BINARY) {
				f->flag_binary_swap = 0;
				size = 38;
				cb_error_x (CB_TREE (f),
					    _("'%s' binary field cannot be larger than %d digits"),
					    f->name, COB_MAX_BINARY);
			}
#else
			if (size > 18) {
				f->flag_binary_swap = 0;
				size = 18;
				cb_error_x (CB_TREE (f),
					    _("'%s' binary field cannot be larger than %d digits"),
					    f->name, 18);
			}
#endif
			compute_binary_size (f, size);
			break;
		case CB_USAGE_DISPLAY:
			f->size = f->pic->size;
			/* size check for single items */
			if (f->size > COB_MAX_FIELD_SIZE) {
				cb_error_x (CB_TREE (f),
						_("'%s' cannot be larger than %d bytes"),
						f->name, COB_MAX_FIELD_SIZE);
			}
			if (f->pic->have_sign && f->flag_sign_separate) {
				f->size++;
			}
			break;
		case CB_USAGE_PACKED:
			f->size = f->pic->size / 2 + 1;
			break;
		case CB_USAGE_COMP_6:
			f->size = (f->pic->size + 1) / 2;
			break;
		case CB_USAGE_INDEX:
		case CB_USAGE_HNDL:
		case CB_USAGE_HNDL_WINDOW:
		case CB_USAGE_HNDL_SUBWINDOW:
		case CB_USAGE_HNDL_FONT:
		case CB_USAGE_HNDL_THREAD:
		case CB_USAGE_HNDL_MENU:
		case CB_USAGE_HNDL_VARIANT:
		case CB_USAGE_HNDL_LM:
		case CB_USAGE_LENGTH:
			f->size = sizeof (int);
			break;
		case CB_USAGE_FLOAT:
			f->size = sizeof (float);
			break;
		case CB_USAGE_DOUBLE:
			f->size = sizeof (double);
			break;
		case CB_USAGE_LONG_DOUBLE:
			f->size = 16;
			break;
		case CB_USAGE_FP_BIN32:
			f->size = 4;
			break;
		case CB_USAGE_FP_BIN64:
		case CB_USAGE_FP_DEC64:
			f->size = 8;
			break;
		case CB_USAGE_FP_BIN128:
		case CB_USAGE_FP_DEC128:
			f->size = 16;
			break;
		case CB_USAGE_OBJECT:
		case CB_USAGE_POINTER:
		case CB_USAGE_PROGRAM_POINTER:
			f->size = sizeof (void *);
			break;
		default:
			/* LCOV_EXCL_START */
			cobc_err_msg (_("unexpected USAGE: %d"),
					(int)f->usage);
			COBC_ABORT ();
			/* LCOV_EXCL_STOP */
		}
	}

	/* The size of redefining field should not be larger than
	   the size of redefined field unless the redefined field
	   is level 01 and non-external */
	if (f->redefines && f->redefines->flag_external &&
	    (f->size * f->occurs_max > f->redefines->size * f->redefines->occurs_max)) {
		if (cb_larger_redefines_ok) {
			cb_warning_x (COBC_WARN_FILLER, CB_TREE (f), _("size of '%s' larger than size of '%s'"),
				      f->name, f->redefines->name);
		} else {
			cb_error_x (CB_TREE (f), _("size of '%s' larger than size of '%s'"),
				    f->name, f->redefines->name);
		}
	}

	return f->size;
}

static int
validate_field_value (struct cb_field *f)
{
	if (f->values) {
		validate_move (CB_VALUE (f->values), CB_TREE (f), 1);
	}

	if (f->children) {
		for (f = f->children; f; f = f->sister) {
			validate_field_value (f);
		}
	}

	return 0;
}

void
cb_validate_field (struct cb_field *f)
{
	struct cb_field		*c;

	if (f->flag_is_verified) {
		return;
	}
	if (validate_field_1 (f) != 0) {
		f->flag_invalid = 1;
		return;
	}
	if (f->flag_item_78) {
		f->flag_is_verified = 1;
		return;
	}

	/* Set up parameters */
	if (f->storage == CB_STORAGE_LOCAL ||
	    f->storage == CB_STORAGE_LINKAGE ||
	    f->flag_item_based) {
		f->flag_local = 1;
	}
	if (f->storage == CB_STORAGE_LINKAGE || f->flag_item_based) {
		f->flag_base = 1;
	}
	setup_parameters (f);

	/* Compute size */
	occur_align_size = 1;
	compute_size (f);
	if (!f->redefines) {
		f->memory_size = f->size * f->occurs_max;
	} else if (f->redefines->memory_size < f->size * f->occurs_max) {
		f->redefines->memory_size = f->size * f->occurs_max;
	}

	validate_field_value (f);
	if (f->flag_is_global) {
		f->count++;
		for (c = f->children; c; c = c->sister) {
			c->flag_is_global = 1;
			c->count++;
		}
	}

	f->flag_is_verified = 1;
}

void
cb_validate_88_item (struct cb_field *f)
{
	cb_tree x = CB_TREE (f);
	cb_tree l;
	cb_tree t;

	if (CB_VALID_TREE(f->parent) &&
	    CB_TREE_CLASS (f->parent) == CB_CLASS_NUMERIC) {
		for (l = f->values; l; l = CB_CHAIN (l)) {
			t = CB_VALUE (l);
			if (t == cb_space || t == cb_low ||
			    t == cb_high || t == cb_quote) {
				cb_error_x (x, _("literal type does not match numeric data type"));
			}
		}
	}
}

struct cb_field *
cb_validate_78_item (struct cb_field *f, const cob_u32_t no78add)
{
	cb_tree			x;
	cob_u32_t		noadd, prec;

	if (f->flag_internal_constant) {	/* Keep all internal CONSTANTs */
		prec = 1;
	} else if (f->flag_constant) {		/* 01 CONSTANT is verified in parser.y */
		prec = 1;
	} else {
		if (!cb_verify (cb_constant_78, "78 VALUE")) {
			return last_real_field;
		}
		prec = 0;
	}

	if (cb_is_expr (f->values) ) {
		f->values = CB_LIST_INIT(cb_evaluate_expr (f->values, prec));
	}

	x = CB_TREE (f);
	noadd = no78add;
	if (CB_INVALID_TREE(f->values) 
	 || CB_INVALID_TREE(CB_VALUE(f->values))) {
		level_require_error (x, "VALUE");
		noadd = 1;
	}

	if (!noadd) {
		cb_add_78 (f);
	}
	return last_real_field;
}

static struct cb_field *
get_next_record_field (const struct cb_field *f)
{
	if (f->children) {
		return f->children;
	}

	while (f) {
		if (f->sister) {
			return f->sister;
		} else {
			f = f->parent;
		}
	}

	return NULL;
}

static int
error_if_rename_thru_is_before_redefines (const struct cb_field * const item)
{
	struct cb_field	*f = cb_field_founder (item->redefines);

	/*
	  Perform depth-first search on the record containing the RENAMES items.
	*/
	while (f) {
		/* Error if we find rename_thru before redefines */
		if (f == item->rename_thru) {
			cb_error_x (CB_TREE (item),
				    _("THRU item '%s' may not come before '%s'"),
				    cb_name (CB_TREE (item->rename_thru)),
				    cb_name (CB_TREE (item->redefines)));
			return 1;
		} else if (f == item->redefines) {
		        return 0;
		}

		f = get_next_record_field (f);
	}

	return 0;
}

static void
error_if_is_or_in_occurs (const struct cb_field * const field,
			  const struct cb_field * const referring_field)
{
	struct cb_field *parent;

	if (field->flag_occurs) {
		cb_error_x (CB_TREE (referring_field),
			    _("RENAMES cannot start/end at the OCCURS item '%s'"),
			    cb_name (CB_TREE (field)));
	}

	for (parent = field->parent; parent; parent = parent->parent) {
		if (parent->flag_occurs) {
			cb_error_x (CB_TREE (referring_field),
				    _("cannot use RENAMES on part of the table '%s'"),
				    cb_name (CB_TREE (parent)));
		}
	}
}

static void
error_if_invalid_type_in_renames_range (const struct cb_field * const item)
{
	const struct cb_field	*end;
	const struct cb_field	*f = item->redefines;
	enum cb_category	category;

	/* Find last item in RENAMES range */
	if (item->rename_thru) {
		if (item->rename_thru->children) {
			end = get_last_child (item->rename_thru);
		} else {
			end = item->rename_thru;
		}
	} else {
		end = item->redefines;
	}

	/*
	  Check all items are not pointers, object references or OCCURS
	  DEPENDING tables.
	*/
	while (f) {
		category = cb_tree_category (CB_TREE (f));
		if (category == CB_CATEGORY_OBJECT_REFERENCE
		    || category == CB_CATEGORY_DATA_POINTER
		    || category == CB_CATEGORY_PROGRAM_POINTER) {
			cb_error_x (CB_TREE (item),
				    _("RENAMES may not contain '%s' as it is a pointer or object reference"),
				    cb_name (CB_TREE (f)));
		} else if (f->depending) {
			cb_error_x (CB_TREE (item),
				    _("RENAMES may not contain '%s' as it is an OCCURS DEPENDING table"),
				    cb_name (CB_TREE (f)));

		}

		if (f == end) {
			break;
		} else {
			f = get_next_record_field (f);
		}
	}
}

void
cb_validate_renames_item (struct cb_field *item)
{
	const cb_tree	item_tree = CB_TREE (item);
	const char	*redefines_name = cb_name (CB_TREE (item->redefines));
        const char	*rename_thru_name = cb_name (CB_TREE (item->rename_thru));
	struct cb_field *founder;
	struct cb_field *f;

	founder = cb_field_founder (item->redefines);
	if (item->parent != founder) {
		cb_error_x (item_tree,
			    _("'%s' must immediately follow the record '%s'"),
			    cb_name (item_tree),
			    cb_name (CB_TREE (founder)));
	}

	if (item->rename_thru
	    && founder != cb_field_founder (item->rename_thru)) {
		cb_error_x (item_tree,
			    _("'%s' and '%s' must be in the same record"),
			    redefines_name, rename_thru_name);
	}

	if (item->redefines == item->rename_thru) {
		cb_error_x (item_tree,
			    _("THRU item must be different to '%s'"),
			    redefines_name);
	} else if (!error_if_rename_thru_is_before_redefines (item)) {
		error_if_invalid_type_in_renames_range (item);
	}

	for (f = item->rename_thru; f; f = f->parent) {
		if (f->parent == item->redefines) {
			cb_error_x (item_tree,
				    _("THRU item '%s' may not be subordinate to '%s'"),
				    rename_thru_name, redefines_name);
			break;
		}
	}

	error_if_is_or_in_occurs (item->redefines, item);
	if (item->rename_thru) {
		error_if_is_or_in_occurs (item->rename_thru, item);
	}
}

void
cb_clear_real_field (void)
{
	last_real_field = NULL;
}

struct cb_field *
cb_get_real_field (void)
{
	return last_real_field;
}

const char *
cb_get_usage_string (const enum cb_usage usage)
{
	switch (usage) {
	case CB_USAGE_BINARY:
		return "COMP";
	case CB_USAGE_BIT:
		return "BIT";
	case CB_USAGE_COMP_5:
		return "COMP-5";
	case CB_USAGE_COMP_X:
		return "COMP-X";
	case CB_USAGE_DISPLAY:
		return "DISPLAY";
	case CB_USAGE_FLOAT:
		return "COMP-1";
		/* return "FLOAT-SHORT"; */
	case CB_USAGE_DOUBLE:
		return "COMP-2";
		/* return "FLOAT-LONG"; */
	case CB_USAGE_INDEX:
		return "INDEX";
	case CB_USAGE_NATIONAL:
		return "NATIONAL";
	case CB_USAGE_OBJECT:
		return "OBJECT REFERENCE";
	case CB_USAGE_PACKED:
		return "COMP-3";
		/* return "PACKED-DECIMAL"; */
	case CB_USAGE_POINTER:
		return "POINTER";
	case CB_USAGE_LENGTH:
		/* Probably---generates a cob_u32_t item.*/
		return "BINARY-LONG";
	case CB_USAGE_PROGRAM_POINTER:
		return "PROGRAM-POINTER";
	case CB_USAGE_UNSIGNED_CHAR:
		return "UNSIGNED-CHAR";
	case CB_USAGE_SIGNED_CHAR:
		return "SIGNED-CHAR";
	case CB_USAGE_UNSIGNED_SHORT:
		return "UNSIGNED-SHORT";
	case CB_USAGE_SIGNED_SHORT:
		return "SIGNED-SHORT";
	case CB_USAGE_UNSIGNED_INT:
		return "UNSIGNED-INT";
	case CB_USAGE_SIGNED_INT:
		return "SIGNED-INT";
	case CB_USAGE_UNSIGNED_LONG:
		return "UNSIGNED-LONG";
	case CB_USAGE_SIGNED_LONG:
		return "SIGNED-LONG";
	case CB_USAGE_COMP_6:
		return "COMP-6";
	case CB_USAGE_FP_DEC64:
		return "FLOAT-DECIMAL-16";
	case CB_USAGE_FP_DEC128:
		return "FLOAT-DECIMAL-34";
	case CB_USAGE_FP_BIN32:
		return "FLOAT-BINARY-32";
	case CB_USAGE_FP_BIN64:
		return "FLOAT-BINARY-64";
	case CB_USAGE_FP_BIN128:
		return "FLOAT-BINARY-128";
	case CB_USAGE_LONG_DOUBLE:
		return "FLOAT-EXTENDED";
	case CB_USAGE_HNDL:
		return "HANDLE";
	case CB_USAGE_HNDL_WINDOW:
		return "HANDLE OF WINDOW";
	case CB_USAGE_HNDL_SUBWINDOW:
		return "HANDLE OF SUBWINDOW";
	case CB_USAGE_HNDL_FONT:
		return "HANDLE OF FONT";
	case CB_USAGE_HNDL_THREAD:
		return "HANDLE OF THREAD";
	case CB_USAGE_HNDL_MENU:
		return "HANDLE OF MENU";
	case CB_USAGE_HNDL_VARIANT:
		return "VARIANT";
	case CB_USAGE_HNDL_LM:
		return "HANDLE OF LAYOUT-MANAGER";
	default:
		/* LCOV_EXCL_START */
		cb_error (_("unexpected USAGE: %d"), usage);
		COBC_ABORT ();
		/* LCOV_EXCL_STOP */
	}
}
