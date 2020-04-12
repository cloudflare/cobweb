/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.4"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
#line 28 "parser.y" /* yacc.c:339  */

#include "config.h"

#include <stdlib.h>
#include <string.h>

#define	COB_IN_PARSER	1
#include "cobc.h"
#include "tree.h"

#ifndef	_STDLIB_H
#define	_STDLIB_H 1
#endif

#define YYSTYPE			cb_tree
#define yyerror(x)		cb_error ("%s", x)

#define emit_statement(x) \
do { \
  if (!skip_statements) { \
	CB_ADD_TO_CHAIN (x, current_program->exec_list); \
  } \
}  ONCE_COB

#define push_expr(type, node) \
  current_expr = cb_build_list (cb_int (type), node, current_expr)

/* Statement terminator definitions */
#define TERM_NONE		0
#define TERM_ACCEPT		1U
#define TERM_ADD		2U
#define TERM_CALL		3U
#define TERM_COMPUTE		4U
#define TERM_DELETE		5U
#define TERM_DISPLAY		6U
#define TERM_DIVIDE		7U
#define TERM_EVALUATE		8U
#define TERM_IF			9U
#define TERM_MULTIPLY		10U
#define TERM_PERFORM		11U
#define TERM_READ		12U
#define TERM_RECEIVE		13U
#define TERM_RETURN		14U
#define TERM_REWRITE		15U
#define TERM_SEARCH		16U
#define TERM_START		17U
#define TERM_STRING		18U
#define TERM_SUBTRACT		19U
#define TERM_UNSTRING		20U
#define TERM_WRITE		21U
#define TERM_MAX		22U	/* Always last entry, used for array size */

#define	TERMINATOR_WARNING(x,z)	terminator_warning (x, TERM_##z, #z)
#define	TERMINATOR_ERROR(x,z)	terminator_error (x, TERM_##z, #z)
#define	TERMINATOR_CLEAR(x,z)	terminator_clear (x, TERM_##z)

/* Defines for duplicate checks */
/* Note - We use <= 16 for common item definitons and */
/* > 16 for non-common item definitions e.g. REPORT and SCREEN */
#define	SYN_CLAUSE_1		(1U << 0)
#define	SYN_CLAUSE_2		(1U << 1)
#define	SYN_CLAUSE_3		(1U << 2)
#define	SYN_CLAUSE_4		(1U << 3)
#define	SYN_CLAUSE_5		(1U << 4)
#define	SYN_CLAUSE_6		(1U << 5)
#define	SYN_CLAUSE_7		(1U << 6)
#define	SYN_CLAUSE_8		(1U << 7)
#define	SYN_CLAUSE_9		(1U << 8)
#define	SYN_CLAUSE_10		(1U << 9)
#define	SYN_CLAUSE_11		(1U << 10)
#define	SYN_CLAUSE_12		(1U << 11)
#define	SYN_CLAUSE_13		(1U << 12)
#define	SYN_CLAUSE_14		(1U << 13)
#define	SYN_CLAUSE_15		(1U << 14)
#define	SYN_CLAUSE_16		(1U << 15)
#define	SYN_CLAUSE_17		(1U << 16)
#define	SYN_CLAUSE_18		(1U << 17)
#define	SYN_CLAUSE_19		(1U << 18)
#define	SYN_CLAUSE_20		(1U << 19)
#define	SYN_CLAUSE_21		(1U << 20)
#define	SYN_CLAUSE_22		(1U << 21)
#define	SYN_CLAUSE_23		(1U << 22)
#define	SYN_CLAUSE_24		(1U << 23)
#define	SYN_CLAUSE_25		(1U << 24)
#define	SYN_CLAUSE_26		(1U << 25)
#define	SYN_CLAUSE_27		(1U << 26)
#define	SYN_CLAUSE_28		(1U << 27)
#define	SYN_CLAUSE_29		(1U << 28)
#define	SYN_CLAUSE_30		(1U << 29)
#define	SYN_CLAUSE_31		(1U << 30)
#define	SYN_CLAUSE_32		(1U << 31)

#define	EVAL_DEPTH		32
#define	PROG_DEPTH		16

/* Global variables */

struct cb_program		*current_program = NULL;
struct cb_statement		*current_statement = NULL;
struct cb_label			*current_section = NULL;
struct cb_label			*current_paragraph = NULL;
cb_tree				defined_prog_list = NULL;
int				cb_exp_line = 0;

cb_tree				cobc_printer_node = NULL;
int				functions_are_all = 0;
int				non_const_word = 0;
int				suppress_data_exceptions = 0;
int				call_line_number;
unsigned int			cobc_repeat_last_token = 0;
unsigned int			cobc_in_id = 0;
unsigned int			cobc_in_procedure = 0;
unsigned int			cobc_in_repository = 0;
unsigned int			cobc_force_literal = 0;
unsigned int			cobc_cs_check = 0;
unsigned int			cobc_allow_program_name = 0;

/* Local variables */

enum tallying_phrase {
	NO_PHRASE,
	FOR_PHRASE,
	CHARACTERS_PHRASE,
	ALL_LEADING_TRAILING_PHRASES,
	VALUE_REGION_PHRASE
};

enum key_clause_type {
	NO_KEY,
	RECORD_KEY,
	RELATIVE_KEY
};
	 
static struct cb_statement	*main_statement;

static cb_tree			current_expr;
static struct cb_field		*current_field;
static struct cb_field		*description_field;
static struct cb_file		*current_file;
static struct cb_cd		*current_cd;
static struct cb_report		*current_report;
static struct cb_report		*report_instance;

static struct cb_file		*linage_file;
static cb_tree			next_label_list;

static char			*stack_progid[PROG_DEPTH];

static enum cb_storage		current_storage;

static cb_tree			perform_stack;
static cb_tree			qualifier;
static cb_tree			keys_list;

static cb_tree			save_tree;
static cb_tree			start_tree;

static unsigned int		check_unreached;
static unsigned int		in_declaratives;
static unsigned int		in_debugging;
static unsigned int		current_linage;
static unsigned int		report_count;
static unsigned int		first_prog;
static unsigned int		setup_from_identification;
static unsigned int		use_global_ind;
static unsigned int		same_area;
static unsigned int		inspect_keyword;
static unsigned int		main_flag_set;
static int			next_label_id;
static int			eval_level;
static int			eval_inc;
static int			eval_inc2;
static int			depth;
static int			first_nested_program;
static int			call_mode;
static int			size_mode;
static cob_flags_t		set_attr_val_on;
static cob_flags_t		set_attr_val_off;
static cob_flags_t		check_duplicate;
static cob_flags_t		check_on_off_duplicate;
static cob_flags_t		check_pic_duplicate;
static cob_flags_t		check_line_col_duplicate;
static unsigned int		skip_statements;
static unsigned int		start_debug;
static unsigned int		save_debug;
static unsigned int		needs_field_debug;
static unsigned int		needs_debug_item;
static unsigned int		env_div_seen;
static cob_flags_t		header_check;
static unsigned int		call_nothing;
static enum tallying_phrase	previous_tallying_phrase;
static cb_tree			default_rounded_mode;
static enum key_clause_type	key_type;

static enum cb_display_type	display_type;
static int			is_first_display_item;
static cb_tree			advancing_value;
static cb_tree			upon_value;
static cb_tree			line_column;

static int			term_array[TERM_MAX];
static cb_tree			eval_check[EVAL_DEPTH][EVAL_DEPTH];

/* Defines for header presence */

#define	COBC_HD_ENVIRONMENT_DIVISION	(1U << 0)
#define	COBC_HD_CONFIGURATION_SECTION	(1U << 1)
#define	COBC_HD_SPECIAL_NAMES		(1U << 2)
#define	COBC_HD_INPUT_OUTPUT_SECTION	(1U << 3)
#define	COBC_HD_FILE_CONTROL		(1U << 4)
#define	COBC_HD_I_O_CONTROL		(1U << 5)
#define	COBC_HD_DATA_DIVISION		(1U << 6)
#define	COBC_HD_FILE_SECTION		(1U << 7)
#define	COBC_HD_WORKING_STORAGE_SECTION	(1U << 8)
#define	COBC_HD_LOCAL_STORAGE_SECTION	(1U << 9)
#define	COBC_HD_LINKAGE_SECTION		(1U << 10)
#define	COBC_HD_COMMUNICATION_SECTION	(1U << 11)
#define	COBC_HD_REPORT_SECTION		(1U << 12)
#define	COBC_HD_SCREEN_SECTION		(1U << 13)
#define	COBC_HD_PROCEDURE_DIVISION	(1U << 14)
#define	COBC_HD_PROGRAM_ID		(1U << 15)

/* Static functions */

static void
begin_statement (const char *name, const unsigned int term)
{
	if (check_unreached) {
		cb_warning (cb_warn_unreachable, _("unreachable statement '%s'"), name);
	}
	current_paragraph->flag_statement = 1;
	current_statement = cb_build_statement (name);
	CB_TREE (current_statement)->source_file = cb_source_file;
	CB_TREE (current_statement)->source_line = cb_source_line;
	current_statement->flag_in_debug = in_debugging;
	emit_statement (CB_TREE (current_statement));
	if (term) {
		term_array[term]++;
	}
	main_statement = current_statement;
}

/* create a new statement with base attributes of current_statement
   and set this as new current_statement */
static void
begin_implicit_statement (void)
{
	struct cb_statement	*new_statement;
	new_statement = cb_build_statement (NULL);
	new_statement->common = current_statement->common;
	new_statement->name = current_statement->name;
	new_statement->flag_in_debug = !!in_debugging;
	current_statement = new_statement;
	main_statement->body = cb_list_add (main_statement->body,
					    CB_TREE (current_statement));
}

# if 0 /* activate only for debugging purposes for attribs */
static
void print_bits (cob_flags_t num)
{
	unsigned int 	size = sizeof (cob_flags_t);
	unsigned int	max_pow = 1 << (size * 8 - 1);
	int 		i = 0;

	for(; i < size * 8; ++i){
		/* Print last bit and shift left. */
		fprintf (stderr, "%u ", num & max_pow ? 1 : 0);
		num = num << 1;
	}
	fprintf (stderr, "\n");
}
#endif

static void
emit_entry (const char *name, const int encode, cb_tree using_list, cb_tree convention)
{
	cb_tree		l;
	cb_tree		label;
	cb_tree		x;
	cb_tree		entry_conv;
	struct cb_field	*f, *ret_f;
	int			param_num;
	char		buff[COB_MINI_BUFF];

	snprintf (buff, (size_t)COB_MINI_MAX, "E$%s", name);
	label = cb_build_label (cb_build_reference (buff), NULL);
	if (encode) {
		CB_LABEL (label)->name = cb_encode_program_id (name);
		CB_LABEL (label)->orig_name = name;
	} else {
		CB_LABEL (label)->name = name;
		CB_LABEL (label)->orig_name = current_program->orig_program_id;
	}
	CB_LABEL (label)->flag_begin = 1;
	CB_LABEL (label)->flag_entry = 1;
	label->source_file = cb_source_file;
	label->source_line = cb_source_line;
	emit_statement (label);

	if (current_program->flag_debugging) {
		emit_statement (cb_build_debug (cb_debug_contents,
						"START PROGRAM", NULL));
	}

	param_num = 1;
	for (l = using_list; l; l = CB_CHAIN (l)) {
		x = CB_VALUE (l);
		if (CB_VALID_TREE (x) && cb_ref (x) != cb_error_node) {
			f = CB_FIELD (cb_ref (x));
			if (!current_program->flag_chained) {
				if (f->storage != CB_STORAGE_LINKAGE) {
					cb_error_x (x, _("'%s' is not in LINKAGE SECTION"), f->name);
				}
				if (f->flag_item_based || f->flag_external) {
					cb_error_x (x, _("'%s' cannot be BASED/EXTERNAL"), f->name);
				}
				f->flag_is_pdiv_parm = 1;
			} else {
				if (f->storage != CB_STORAGE_WORKING) {
					cb_error_x (x, _("'%s' is not in WORKING-STORAGE SECTION"), f->name);
				}
				f->flag_chained = 1;
				f->param_num = param_num;
				param_num++;
			}
			if (f->level != 01 && f->level != 77) {
				cb_error_x (x, _("'%s' not level 01 or 77"), f->name);
			}
			if (f->redefines) {
				cb_error_x (x, _ ("'%s' REDEFINES field not allowed here"), f->name);
			}
			/* add a "receiving" entry for the USING parameter */
			if (cb_listing_xref) {
				cobc_xref_link (&f->xref, CB_REFERENCE (x)->common.source_line, 1);
			}
		}
	}


	if (current_program->returning &&
		cb_ref (current_program->returning) != cb_error_node) {
		ret_f = CB_FIELD (cb_ref (current_program->returning));
		if (ret_f->redefines) {
			cb_error_x (current_program->returning, _("'%s' REDEFINES field not allowed here"), ret_f->name);
		}
	} else {
		ret_f = NULL;
	}

	/* Check dangling LINKAGE items */
	if (cb_warn_linkage) {
		for (f = current_program->linkage_storage; f; f = f->sister) {
			if (f == ret_f) {
				continue;
			}
			for (l = using_list; l; l = CB_CHAIN (l)) {
				x = CB_VALUE (l);
				if (CB_VALID_TREE (x) && cb_ref (x) != cb_error_node) {
					if (f == CB_FIELD (cb_ref (x))) {
						break;
					}
				}
			}
			if (!l && !f->redefines) {
				cb_warning (cb_warn_linkage, _("LINKAGE item '%s' is not a PROCEDURE USING parameter"), f->name);
			}
		}
	}

	/* Check returning item against using items when FUNCTION */
	if (current_program->prog_type == CB_FUNCTION_TYPE && current_program->returning) {
		for (l = using_list; l; l = CB_CHAIN (l)) {
			x = CB_VALUE (l);
			if (CB_VALID_TREE (x) && cb_ref (x) != cb_error_node) {
				f = CB_FIELD (cb_ref (x));
				if (ret_f == f) {
					cb_error_x (x, _("'%s' USING item duplicates RETURNING item"), f->name);
				}
			}
		}
	}

	for (l = current_program->entry_list; l; l = CB_CHAIN (l)) {
		if (strcmp ((const char *)name,
			    (const char *)(CB_LABEL(CB_PURPOSE(l))->name)) == 0) {
			cb_error_x (CB_TREE (current_statement),
				    _("ENTRY '%s' duplicated"), name);
		}
	}

	if (convention) {
		entry_conv = convention;
	} else {
		entry_conv = current_program->entry_convention;
	}

	current_program->entry_list =
		cb_list_append (current_program->entry_list,
				CB_BUILD_PAIR (label, CB_BUILD_PAIR(entry_conv, using_list)));
}

static size_t
increment_depth (void)
{
	if (++depth >= PROG_DEPTH) {
		cb_error (_("maximum nested program depth exceeded (%d)"),
			  PROG_DEPTH);
		return 1;
	}
	return 0;
}

static void
terminator_warning (cb_tree stmt, const unsigned int termid,
		    const char *name)
{
	char		terminator[32];

	check_unreached = 0;
	if (term_array[termid]) {
		term_array[termid]--;
	/* LCOV_EXCL_START */
	} else {
		cobc_err_msg ("call to '%s' without any open term for %s",
			"terminator_warning", name);
		COBC_ABORT ();
	}
	/* LCOV_EXCL_END */
	snprintf (terminator, 32, "END-%s", name);
	if (is_reserved_word (terminator)) {
		cb_warning_x (cb_warn_terminator, CB_TREE (current_statement),
			_("%s statement not terminated by %s"), name, terminator);
	}

	/* Free tree associated with terminator */
	if (stmt) {
		cobc_parse_free (stmt);
	}
}

static void
terminator_error (cb_tree stmt, const unsigned int termid, const char *name)
{
	char		terminator[32];

	check_unreached = 0;
	if (term_array[termid]) {
		term_array[termid]--;
	/* LCOV_EXCL_START */
	} else {
		cobc_err_msg ("call to '%s' without any open term for %s",
			"terminator_error", name);
		COBC_ABORT ();
	}
	/* LCOV_EXCL_END */
	snprintf (terminator, 32, "END-%s", name);
	if (is_reserved_word (terminator)) {
		cb_error_x (CB_TREE (current_statement),
			_("%s statement not terminated by %s"), name, terminator);
	} else {
		cb_error_x (CB_TREE (current_statement),
			_("%s statement not terminated"), name);
	}

	/* Free tree associated with terminator */
	if (stmt) {
		cobc_parse_free (stmt);
	}
}

static void
terminator_clear (cb_tree stmt, const unsigned int termid)
{
	struct cb_perform	*p;
	check_unreached = 0;
	if (term_array[termid]) {
		term_array[termid]--;
	/* LCOV_EXCL_START */
	} else {
		cobc_err_msg ("call to '%s' without any open term for %s",
			"terminator_warning", current_statement->name);
		COBC_ABORT ();
	}
	/* LCOV_EXCL_END */
	if (termid == TERM_PERFORM
	 && perform_stack) {
		p = CB_PERFORM (CB_VALUE (perform_stack));
		if (p->perform_type == CB_PERFORM_UNTIL) {
			cb_terminate_cond ();
		}
	}
	/* Free tree associated with terminator */
	if (stmt) {
		cobc_parse_free (stmt);
	}
}

static int
literal_value (cb_tree x)
{
	if (x == cb_space) {
		return ' ';
	} else if (x == cb_zero) {
		return '0';
	} else if (x == cb_quote) {
		return cb_flag_apostrophe ? '\'' : '"';
	} else if (x == cb_null) {
		return 0;
	} else if (x == cb_low) {
		return 0;
	} else if (x == cb_high) {
		return 255;
	} else if (CB_TREE_CLASS (x) == CB_CLASS_NUMERIC) {
		return cb_get_int (x);
	} else {
		return CB_LITERAL (x)->data[0];
	}
}

static void
setup_use_file (struct cb_file *fileptr)
{
	struct cb_file	*newptr;

	if (fileptr->organization == COB_ORG_SORT) {
		cb_error (_("USE statement invalid for SORT file"));
	}
	if (fileptr->flag_global) {
		newptr = cobc_parse_malloc (sizeof(struct cb_file));
		*newptr = *fileptr;
		newptr->handler = current_section;
		newptr->handler_prog = current_program;
		if (!use_global_ind) {
			current_program->local_file_list =
				cb_list_add (current_program->local_file_list,
					     CB_TREE (newptr));
		} else {
			current_program->global_file_list =
				cb_list_add (current_program->global_file_list,
					     CB_TREE (newptr));
		}
	} else {
		fileptr->handler = current_section;
	}
}

static void
emit_duplicate_clause_message (const char *clause)
{
	/* FIXME: replace by a new warning level that is set
	   to warn/error depending on cb_relaxed_syntax_checks */
	if (cb_relaxed_syntax_checks) {
		cb_warning (COBC_WARN_FILLER, _("duplicate %s clause"), clause);
	} else {
		cb_error (_("duplicate %s clause"), clause);
	}
}

static void
check_repeated (const char *clause, const cob_flags_t bitval, cob_flags_t *already_seen)
{
	if (*already_seen & bitval) {
		emit_duplicate_clause_message (clause);
	} else {
		*already_seen |= bitval;
	}
}

static void
setup_occurs (void)
{
	check_repeated ("OCCURS", SYN_CLAUSE_7, &check_pic_duplicate);
	if (current_field->indexes == COB_MAX_SUBSCRIPTS) {
		cb_error (_ ("maximum OCCURS depth exceeded (%d)"),
			COB_MAX_SUBSCRIPTS);
	} else {
		current_field->indexes++;
	}

	if (current_field->flag_unbounded) {
		if (current_field->storage != CB_STORAGE_LINKAGE) {
			cb_error_x (CB_TREE(current_field), _("'%s' is not in LINKAGE SECTION"),
				cb_name (CB_TREE(current_field)));
		}
	}

	if (current_field->flag_item_based) {
		cb_error (_ ("%s and %s are mutually exclusive"), "BASED", "OCCURS");
	} else if (current_field->flag_external) {
		cb_error (_ ("%s and %s are mutually exclusive"), "EXTERNAL", "OCCURS");
	}
	current_field->flag_occurs = 1;
}

static void
setup_occurs_min_max (cb_tree occurs_min, cb_tree occurs_max)
{
	if (occurs_max) {
		current_field->occurs_min = cb_get_int (occurs_min);
		if (occurs_max != cb_int0) {
			current_field->occurs_max = cb_get_int (occurs_max);
			if (!current_field->depending) {
				if (cb_relaxed_syntax_checks) {
					cb_warning (COBC_WARN_FILLER, _ ("TO phrase without DEPENDING phrase"));
					cb_warning (COBC_WARN_FILLER, _ ("maximum number of occurences assumed to be exact number"));
					current_field->occurs_min = 1; /* CHECKME: why using 1 ? */
				} else {
					cb_error (_ ("TO phrase without DEPENDING phrase"));
				}
			}
			if (current_field->occurs_max <= current_field->occurs_min) {
				cb_error (_ ("OCCURS TO must be greater than OCCURS FROM"));
			}
		} else {
			current_field->occurs_max = 0;
		}
	} else {
		current_field->occurs_min = 1; /* CHECKME: why using 1 ? */
		current_field->occurs_max = cb_get_int (occurs_min);
		if (current_field->depending) {
			cb_verify (cb_odo_without_to, _ ("OCCURS DEPENDING ON without TO phrase"));
		}
	}
}

static void
check_relaxed_syntax (const cob_flags_t lev)
{
	const char	*s;

	switch (lev) {
	case COBC_HD_ENVIRONMENT_DIVISION:
		s = "ENVIRONMENT DIVISION";
		break;
	case COBC_HD_CONFIGURATION_SECTION:
		s = "CONFIGURATION SECTION";
		break;
	case COBC_HD_SPECIAL_NAMES:
		s = "SPECIAL-NAMES";
		break;
	case COBC_HD_INPUT_OUTPUT_SECTION:
		s = "INPUT-OUTPUT SECTION";
		break;
	case COBC_HD_FILE_CONTROL:
		s = "FILE-CONTROL";
		break;
	case COBC_HD_I_O_CONTROL:
		s = "I-O-CONTROL";
		break;
	case COBC_HD_DATA_DIVISION:
		s = "DATA DIVISION";
		break;
	case COBC_HD_FILE_SECTION:
		s = "FILE SECTION";
		break;
	case COBC_HD_WORKING_STORAGE_SECTION:
		s = "WORKING-STORAGE SECTION";
		break;
	case COBC_HD_LOCAL_STORAGE_SECTION:
		s = "LOCAL-STORAGE SECTION";
		break;
	case COBC_HD_LINKAGE_SECTION:
		s = "LINKAGE SECTION";
		break;
	case COBC_HD_COMMUNICATION_SECTION:
		s = "COMMUNICATION SECTION";
		break;
	case COBC_HD_REPORT_SECTION:
		s = "REPORT SECTION";
		break;
	case COBC_HD_SCREEN_SECTION:
		s = "SCREEN SECTION";
		break;
	case COBC_HD_PROCEDURE_DIVISION:
		s = "PROCEDURE DIVISION";
		break;
	case COBC_HD_PROGRAM_ID:
		s = "PROGRAM-ID";
		break;
	default:
		s = "Unknown";
		break;
	}
	if (cb_relaxed_syntax_checks) {
		cb_warning (COBC_WARN_FILLER, _("%s header missing - assumed"), s);
	} else {
		cb_error (_("%s header missing"), s);
	}
}

/* check if headers are present - return 0 if fine, 1 if missing
   Lev1 must always be present and is checked
   Lev2/3/4, if non-zero (forced) may be present
*/
static int
check_headers_present (const cob_flags_t lev1, const cob_flags_t lev2,
		       const cob_flags_t lev3, const cob_flags_t lev4)
{
	int ret = 0;
	if (!(header_check & lev1)) {
		header_check |= lev1;
		check_relaxed_syntax (lev1);
		ret = 1;
	}
	if (lev2) {
		if (!(header_check & lev2)) {
			header_check |= lev2;
			check_relaxed_syntax (lev2);
			ret = 1;
		}
	}
	if (lev3) {
		if (!(header_check & lev3)) {
			header_check |= lev3;
			check_relaxed_syntax (lev3);
			ret = 1;
		}
	}
	if (lev4) {
		if (!(header_check & lev4)) {
			header_check |= lev4;
			check_relaxed_syntax (lev4);
			ret = 1;
		}
	}
	return ret;
}

static void
build_nested_special (const int ndepth)
{
	cb_tree		x;
	cb_tree		y;

	if (!ndepth) {
		return;
	}

	/* Inherit special name mnemonics from parent */
	for (x = current_program->mnemonic_spec_list; x; x = CB_CHAIN (x)) {
		y = cb_build_reference (cb_name(CB_PURPOSE(x)));
		if (CB_SYSTEM_NAME_P (CB_VALUE(x))) {
			cb_define (y, CB_VALUE(x));
		} else {
			cb_build_constant (y, CB_VALUE(x));
		}
	}
}

static void
clear_initial_values (void)
{
	perform_stack = NULL;
	current_statement = NULL;
	main_statement = NULL;
	qualifier = NULL;
	in_declaratives = 0;
	in_debugging = 0;
	use_global_ind = 0;
	check_duplicate = 0;
	check_pic_duplicate = 0;
	skip_statements = 0;
	start_debug = 0;
	save_debug = 0;
	needs_field_debug = 0;
	needs_debug_item = 0;
	env_div_seen = 0;
	header_check = 0;
	next_label_id = 0;
	current_linage = 0;
	set_attr_val_on = 0;
	set_attr_val_off = 0;
	report_count = 0;
	current_storage = CB_STORAGE_WORKING;
	eval_level = 0;
	eval_inc = 0;
	eval_inc2 = 0;
	inspect_keyword = 0;
	check_unreached = 0;
	cobc_in_id = 0;
	cobc_in_procedure = 0;
	cobc_in_repository = 0;
	cobc_force_literal = 0;
	non_const_word = 0;
	suppress_data_exceptions = 0;
	same_area = 1;
	memset ((void *)eval_check, 0, sizeof(eval_check));
	memset ((void *)term_array, 0, sizeof(term_array));
	linage_file = NULL;
	current_file = NULL;
	current_cd = NULL;
	current_report = NULL;
	report_instance = NULL;
	next_label_list = NULL;
	default_rounded_mode = cb_int (COB_STORE_ROUND);
}

/*
  We must check for redefinitions of program-names and external program names
  outside of the usual reference/word_list methods as it may have to be done in
  a case-sensitive way.
*/
static void
begin_scope_of_program_name (struct cb_program *program)
{
	const char	*prog_name = program->program_name;
	const char	*prog_id = program->orig_program_id;
	const char	*elt_name;
	const char	*elt_id;
	cb_tree		l;

	/* Error if a program with the same name has been defined. */
	for (l = defined_prog_list; l; l = CB_CHAIN (l)) {
		elt_name = ((struct cb_program *) CB_VALUE (l))->program_name;
		elt_id = ((struct cb_program *) CB_VALUE (l))->orig_program_id;
		if (cb_fold_call && strcasecmp (prog_name, elt_name) == 0) {
			cb_error_x ((cb_tree) program,
				    _("redefinition of program name '%s'"),
				    elt_name);
		} else if (strcmp (prog_id, elt_id) == 0) {
		        cb_error_x ((cb_tree) program,
				    _("redefinition of program ID '%s'"),
				    elt_id);
			return;
		}
	}

	/* Otherwise, add the program to the list. */
	defined_prog_list = cb_list_add (defined_prog_list,
					 (cb_tree) program);
}

static void
remove_program_name (struct cb_list *l, struct cb_list *prev)
{
	if (prev == NULL) {
		defined_prog_list = l->chain;
	} else {
		prev->chain = l->chain;
	}
	cobc_parse_free (l);
}

/* Remove the program from defined_prog_list, if necessary. */
static void
end_scope_of_program_name (struct cb_program *program, const unsigned char type)
{
	struct	cb_list	*prev = NULL;
	struct	cb_list *l = (struct cb_list *) defined_prog_list;

	/* create empty entry if the program has no PROCEDURE DIVISION, error for UDF */
	if (!program->entry_list) {
		if (type == CB_FUNCTION_TYPE) {
			cb_error (_("FUNCTION '%s' has no PROCEDURE DIVISION"), program->program_name);
		} else {
			emit_entry (program->program_id, 0, NULL, NULL);
		}
	}

	if (program->nested_level == 0) {
		return;
	}

	/* Remove any subprograms */
	l = CB_LIST (defined_prog_list);
	while (l) {
		if (CB_PROGRAM (l->value)->nested_level > program->nested_level) {
			remove_program_name (l, prev);
		} else {
			prev = l;
		}
		if (prev && prev->chain != NULL) {
			l = CB_LIST (prev->chain);
		} else {
			l = NULL;
		}
	}

	/* Remove the specified program, if it is not COMMON */
	if (!program->flag_common) {
		l = (struct cb_list *) defined_prog_list;
		while (l) {
			if (strcmp (program->orig_program_id,
				    CB_PROGRAM (l->value)->orig_program_id)
			    == 0) {
				remove_program_name (l, prev);
				if (prev && prev->chain != NULL) {
					l = CB_LIST (prev->chain);
				} else {
					l = NULL;
				}
				break;
			} else {
				prev = l;
				if (l->chain != NULL) {
					l = CB_LIST (l->chain);
				} else {
					l = NULL;
				}
			}
		}
	}
}

static void
setup_program_start (void)
{
	if (setup_from_identification) {
		setup_from_identification = 0;
		return;
	}
	current_section = NULL;
	current_paragraph = NULL;

	if (depth != 0 && first_nested_program) {
		check_headers_present (COBC_HD_PROCEDURE_DIVISION, 0, 0, 0);
	}
	first_nested_program = 1;
}

static int
setup_program (cb_tree id, cb_tree as_literal, const unsigned char type)
{
	setup_program_start ();

	if (first_prog) {
		first_prog = 0;
	} else {
		if (!current_program->flag_validated) {
			current_program->flag_validated = 1;
			cb_validate_program_body (current_program);
		}

		clear_initial_values ();
		current_program = cb_build_program (current_program, depth);
		build_nested_special (depth);
		cb_set_intr_when_compiled ();
		cb_build_registers ();
	}

	if (CB_LITERAL_P (id)) {
		stack_progid[depth] = (char *)(CB_LITERAL (id)->data);
	} else {
		stack_progid[depth] = (char *)(CB_NAME (id));
	}

	if (depth != 0 && type == CB_FUNCTION_TYPE) {
		cb_error (_("functions may not be defined within a program/function"));
	}

	if (increment_depth ()) {
		return 1;
	}

	current_program->program_id = cb_build_program_id (id, as_literal, type == CB_FUNCTION_TYPE);
	current_program->prog_type = type;

	if (type == CB_PROGRAM_TYPE) {
		if (!main_flag_set) {
			main_flag_set = 1;
			current_program->flag_main = !!cobc_flag_main;
		}
	} else { /* CB_FUNCTION_TYPE */
		current_program->flag_recursive = 1;
	}

	if (CB_REFERENCE_P (id)) {
	        cb_define (id, CB_TREE (current_program));
	}

	begin_scope_of_program_name (current_program);

	return 0;
}

static void
decrement_depth (const char *name, const unsigned char type)
{
	int	d;

	if (depth) {
		depth--;
	}

	if (!strcmp (stack_progid[depth], name)) {
		return;
	}

	if (type == CB_FUNCTION_TYPE) {
		cb_error (_("END FUNCTION '%s' is different from FUNCTION-ID '%s'"),
			  name, stack_progid[depth]);
		return;
	}

	/* Set depth to that of whatever program we just ended, if it exists. */
	for (d = depth; d >= 0; --d) {
		if (!strcmp (stack_progid[d], name)) {
			depth = d;
			return;
		}
	}

	if (depth != d) {
		cb_error (_("END PROGRAM '%s' is different from PROGRAM-ID '%s'"),
			  name, stack_progid[depth]);
	}
}

static void
clean_up_program (cb_tree name, const unsigned char type)
{
	char		*s;

	end_scope_of_program_name (current_program, type);

	if (name) {
		if (CB_LITERAL_P (name)) {
			s = (char *)(CB_LITERAL (name)->data);
		} else {
			s = (char *)(CB_NAME (name));
		}

		decrement_depth (s, type);
	}

	current_section = NULL;
	current_paragraph = NULL;
	if (!current_program->flag_validated) {
		current_program->flag_validated = 1;
		cb_validate_program_body (current_program);
	}
}

static const char *
get_literal_or_word_name (const cb_tree x)
{
	if (CB_LITERAL_P (x)) {
		return (const char *) CB_LITERAL (x)->data;
	} else { /* CB_REFERENCE_P (x) */
		return (const char *) CB_NAME (x);
	}
}

/* verify and set picture sign for currency */
static void
set_currency_picture_symbol (const cb_tree x)
{
	unsigned char	*s		= CB_LITERAL (x)->data;

	if (CB_LITERAL (x)->size != 1) {
		cb_error_x (x, _("PICTURE SYMBOL for CURRENCY must be one character long"));
		return;
	}
	switch (*s) {
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
	case 'A':
	case 'B':
	case 'C':
	case 'D':
	case 'E':
	case 'N':
	case 'P':
	case 'R':
	case 'S':
	case 'V':
	case 'X':
	case 'Z':
	case 'a':
	case 'b':
	case 'c':
	case 'd':
	case 'e':
	case 'n':
	case 'p':
	case 'r':
	case 's':
	case 'v':
	case 'x':
	case 'z':
	case '+':
	case '-':
	case ',':
	case '.':
	case '*':
	case '/':
	case ';':
	case '(':
	case ')':
	case '=':
	case '\'':
	case '"':
	case ' ':
		cb_error_x (x, _("invalid character '%c' in PICTURE SYMBOL for CURRENCY"), s[0]);
		return;
	default:
		break;
	}
	current_program->currency_symbol = s[0];
}

/* Return 1 if the prototype name is the same as the current function's. */
static int
check_prototype_redefines_current_element (const cb_tree prototype_name)
{
	const char	*name = get_literal_or_word_name (prototype_name);

	if (strcasecmp (name, current_program->program_name) == 0) {
		cb_warning_x (COBC_WARN_FILLER, prototype_name,
			_("prototype has same name as current function and will be ignored"));
		return 1;
	}

	return 0;
}

/* Returns 1 if the prototype has been duplicated. */
static int
check_for_duplicate_prototype (const cb_tree prototype_name,
			       const cb_tree prototype)
{
	cb_tree	dup;

	if (CB_WORD_COUNT (prototype_name) > 0) {
		/* Make sure the duplicate is a prototype */
		dup = cb_ref (prototype_name);
		if (!CB_PROTOTYPE_P (dup)) {
			redefinition_error (prototype_name);
			return 1;
		}

		/* Check the duplicate prototypes match */
		if (strcmp (CB_PROTOTYPE (prototype)->ext_name,
			    CB_PROTOTYPE (dup)->ext_name)
		    || CB_PROTOTYPE (prototype)->type != CB_PROTOTYPE (dup)->type) {
			cb_error_x (prototype_name,
				    _("duplicate REPOSITORY entries for '%s' do not match"),
				    get_literal_or_word_name (prototype_name));
		} else {
			cb_warning_x (COBC_WARN_FILLER, prototype_name,
				      _("duplicate REPOSITORY entry for '%s'"),
				      get_literal_or_word_name (prototype_name));
		}
		return 1;
	}

	return 0;
}

static void
setup_prototype (cb_tree prototype_name, cb_tree ext_name,
		  const int type, const int is_current_element)
{
	cb_tree	prototype;
	int	name_redefinition_allowed;

	if (!is_current_element
	    && check_prototype_redefines_current_element (prototype_name)) {
		return;
	}

	prototype = cb_build_prototype (prototype_name, ext_name, type);

	if (!is_current_element
	    && check_for_duplicate_prototype (prototype_name, prototype)) {
		return;
	}

	name_redefinition_allowed = type == CB_PROGRAM_TYPE
		&& is_current_element && cb_program_name_redefinition;
	if (!name_redefinition_allowed) {
		if (CB_LITERAL_P (prototype_name)) {
			cb_define (cb_build_reference ((const char *)CB_LITERAL (prototype_name)->data), prototype);
		} else {
			cb_define (prototype_name, prototype);
		}

		if (type == CB_PROGRAM_TYPE) {
			current_program->program_spec_list =
				cb_list_add (current_program->program_spec_list, prototype);
		} else { /* CB_FUNCTION_TYPE */
			current_program->user_spec_list =
				cb_list_add (current_program->user_spec_list, prototype);
		}
	}
}

static void
error_if_invalid_level_for_renames (cb_tree item)
{
	int	level = CB_FIELD (cb_ref (item))->level;

	if (level == 1 || level == 66 || level == 77) {
	        cb_verify (cb_renames_uncommon_levels,
			   _("RENAMES of 01-, 66- and 77-level items"));
	} else if (level == 88) {
		cb_error (_("RENAMES may not reference a level 88"));
	}
}

static int
set_current_field (cb_tree level, cb_tree name)
{
	cb_tree	x  = cb_build_field_tree (level, name, current_field,
					  current_storage, current_file, 0);
	cobc_parse_free (level);

	if (CB_INVALID_TREE (x)) {
	        return 1;
	} else {
		current_field = CB_FIELD (x);
		check_pic_duplicate = 0;
	}

	return 0;
}

static void
check_not_both (const cob_flags_t flag1, const cob_flags_t flag2,
		const char *flag1_name, const char *flag2_name,
		const cob_flags_t flags, const cob_flags_t flag_to_set)
{
	if (flag_to_set == flag1 && (flags & flag2)) {
		cb_error (_("cannot specify both %s and %s"),
			  flag1_name, flag2_name);
	} else if (flag_to_set == flag2 && (flags & flag1)) {
		cb_error (_("cannot specify both %s and %s"),
			  flag1_name, flag2_name);

	}
}

static COB_INLINE COB_A_INLINE void
check_not_highlight_and_lowlight (const cob_flags_t flags,
				  const cob_flags_t flag_to_set)
{
	check_not_both (COB_SCREEN_HIGHLIGHT, COB_SCREEN_LOWLIGHT,
			"HIGHLIGHT", "LOWLIGHT", flags, flag_to_set);
}

static void
set_screen_attr (const char *clause, const cob_flags_t bitval)
{
	if (current_field->screen_flag & bitval) {
		emit_duplicate_clause_message (clause);
	} else {
		current_field->screen_flag |= bitval;
	}
}

static void
emit_conflicting_clause_message (const char *clause, const char *conflicting)
{
	if (cb_relaxed_syntax_checks) {
		cb_warning (COBC_WARN_FILLER, _("cannot specify both %s and %s; %s is ignored"),
			clause, conflicting, clause);
	} else {
		cb_error (_("cannot specify both %s and %s"),
			clause, conflicting);
	}

}

static void
set_attr_with_conflict (const char *clause, const cob_flags_t bitval,
			const char *confl_clause, const cob_flags_t confl_bit,
			const int local_check_duplicate, cob_flags_t *flags)
{
	if (local_check_duplicate && (*flags & bitval)) {
		emit_duplicate_clause_message (clause);
	} else if (*flags & confl_bit) {
		emit_conflicting_clause_message (clause, confl_clause);
	} else {
	*flags |= bitval;
	}
}

static COB_INLINE COB_A_INLINE void
set_screen_attr_with_conflict (const char *clause, const cob_flags_t bitval,
			       const char *confl_clause,
			       const cob_flags_t confl_bit)
{
	set_attr_with_conflict (clause, bitval, confl_clause, confl_bit, 1,
				&current_field->screen_flag);
}

static COB_INLINE COB_A_INLINE int
has_dispattr (const cob_flags_t attrib)
{
	return current_statement->attr_ptr
		&& current_statement->attr_ptr->dispattrs & attrib;
}

static void
attach_attrib_to_cur_stmt (void)
{
	if (!current_statement->attr_ptr) {
		current_statement->attr_ptr =
			cobc_parse_malloc (sizeof(struct cb_attr_struct));
	}
}

static COB_INLINE COB_A_INLINE void
set_dispattr (const cob_flags_t attrib)
{
	attach_attrib_to_cur_stmt ();
	current_statement->attr_ptr->dispattrs |= attrib;
}

static COB_INLINE COB_A_INLINE void
set_dispattr_with_conflict (const char *attrib_name, const cob_flags_t attrib,
			    const char *confl_name,
			    const cob_flags_t confl_attrib)
{
	attach_attrib_to_cur_stmt ();
	set_attr_with_conflict (attrib_name, attrib, confl_name, confl_attrib, 0,
				&current_statement->attr_ptr->dispattrs);
}

static void
bit_set_attr (const cb_tree on_off, const cob_flags_t attr_val)
{
	if (on_off == cb_int1) {
		set_attr_val_on |= attr_val;
	} else {
		set_attr_val_off |= attr_val;
	}
}

static void
set_field_attribs (cb_tree fgc, cb_tree bgc, cb_tree scroll,
		   cb_tree timeout, cb_tree prompt, cb_tree size_is)
{
	/* [WITH] FOREGROUND-COLOR [IS] */
	if (fgc) {
		current_statement->attr_ptr->fgc = fgc;
	}
	/* [WITH] BACKGROUND-COLOR [IS] */
	if (bgc) {
		current_statement->attr_ptr->bgc = bgc;
	}
	/* [WITH] SCROLL UP | DOWN */
	if (scroll) {
		current_statement->attr_ptr->scroll = scroll;
	}
	/* [WITH] TIME-OUT [AFTER] */
	if (timeout) {
		current_statement->attr_ptr->timeout = timeout;
	}
	/* [WITH] PROMPT CHARACTER [IS] */
	if (prompt) {
		current_statement->attr_ptr->prompt = prompt;
	}
	/* [WITH] SIZE [IS] */
	if (size_is) {
		current_statement->attr_ptr->size_is = size_is;
	}
}

static void
set_attribs (cb_tree fgc, cb_tree bgc, cb_tree scroll,
	     cb_tree timeout, cb_tree prompt, cb_tree size_is,
	     const cob_flags_t attrib)
{
	attach_attrib_to_cur_stmt ();
	set_field_attribs (fgc, bgc, scroll, timeout, prompt, size_is);

	current_statement->attr_ptr->dispattrs |= attrib;
}

static void
set_attribs_with_conflict  (cb_tree fgc, cb_tree bgc, cb_tree scroll,
			    cb_tree timeout, cb_tree prompt, cb_tree size_is,
			    const char *clause_name, const cob_flags_t attrib,
			    const char *confl_name, const cob_flags_t confl_attrib)
{
	attach_attrib_to_cur_stmt ();
	set_field_attribs (fgc, bgc, scroll, timeout, prompt, size_is);

	set_dispattr_with_conflict (clause_name, attrib, confl_name,
				    confl_attrib);
}

static cob_flags_t
zero_conflicting_flag (const cob_flags_t screen_flag, cob_flags_t parent_flag,
				const cob_flags_t flag1, const cob_flags_t flag2)
{
	if (screen_flag & flag1) {
		parent_flag &= ~flag2;
	} else if (screen_flag & flag2) {
		parent_flag &= ~flag1;
	}

	return parent_flag;
}

static cob_flags_t
zero_conflicting_flags (const cob_flags_t screen_flag, cob_flags_t parent_flag)
{
	parent_flag = zero_conflicting_flag (screen_flag, parent_flag,
					     COB_SCREEN_BLANK_LINE,
					     COB_SCREEN_BLANK_SCREEN);
	parent_flag = zero_conflicting_flag (screen_flag, parent_flag,
					     COB_SCREEN_ERASE_EOL,
					     COB_SCREEN_ERASE_EOS);
	parent_flag = zero_conflicting_flag (screen_flag, parent_flag,
					     COB_SCREEN_HIGHLIGHT,
					     COB_SCREEN_LOWLIGHT);

	return parent_flag;
}

static void
check_and_set_usage (const enum cb_usage usage)
{
	check_repeated ("USAGE", SYN_CLAUSE_5, &check_pic_duplicate);
	current_field->usage = usage;
}

static void
check_preceding_tallying_phrases (const enum tallying_phrase phrase)
{
	switch (phrase) {
	case FOR_PHRASE:
		if (previous_tallying_phrase == ALL_LEADING_TRAILING_PHRASES) {
			cb_error (_("FOR phrase cannot immediately follow ALL/LEADING/TRAILING"));
		} else if (previous_tallying_phrase == FOR_PHRASE) {
			cb_error (_("missing CHARACTERS/ALL/LEADING/TRAILING phrase after FOR phrase"));
		}
		break;

	case CHARACTERS_PHRASE:
	case ALL_LEADING_TRAILING_PHRASES:
		if (previous_tallying_phrase == NO_PHRASE) {
			cb_error (_("missing FOR phrase before CHARACTERS/ALL/LEADING/TRAILING phrase"));
		} else if (previous_tallying_phrase == CHARACTERS_PHRASE
			   || previous_tallying_phrase == ALL_LEADING_TRAILING_PHRASES) {
			cb_error (_("missing value between CHARACTERS/ALL/LEADING/TRAILING words"));
		}
		break;

	case VALUE_REGION_PHRASE:
		if (!(previous_tallying_phrase == ALL_LEADING_TRAILING_PHRASES
		      || previous_tallying_phrase == VALUE_REGION_PHRASE)) {
			cb_error (_("missing ALL/LEADING/TRAILING before value"));
		}
		break;

		/* LCOV_EXCL_START */
	default:
		/* This should never happen (and therefore doesn't get a translation) */
		cb_error ("unexpected tallying phrase");
		COBC_ABORT();
		/* LCOV_EXCL_END */
	}

	previous_tallying_phrase = phrase;
}

static int
has_relative_pos (struct cb_field const *field)
{
	return !!(field->screen_flag
		  & (COB_SCREEN_LINE_PLUS | COB_SCREEN_LINE_MINUS
		     | COB_SCREEN_COLUMN_PLUS | COB_SCREEN_COLUMN_MINUS));
}

static int
is_recursive_call (cb_tree target)
{
	const char *target_name = "";

	if (CB_LITERAL_P (target)) {
		target_name = (const char *)(CB_LITERAL(target)->data);
	} else if (CB_REFERENCE_P (target)
		   && CB_PROTOTYPE_P (cb_ref (target))) {
		target_name = CB_PROTOTYPE (cb_ref (target))->ext_name;
	}

	return !strcmp (target_name, current_program->orig_program_id);
}

static void
check_not_88_level (cb_tree x)
{
	struct cb_field	*f;

	if (x == cb_error_node || x->tag != CB_TAG_REFERENCE) {
		return;
	}

	f = CB_FIELD (cb_ref (x));

	if (f != (struct cb_field *) cb_error_node && f->level == 88) {
		cb_error (_("88-level cannot be used here"));
	}
}

static int
is_screen_field (cb_tree x)
{
	if (CB_FIELD_P (x)) {
		return (CB_FIELD (x))->storage == CB_STORAGE_SCREEN;
	} else if (CB_REFERENCE_P (x)) {
		return is_screen_field (cb_ref (x));
	} else {
		return 0;
	}
}

static void
error_if_no_advancing_in_screen_display (cb_tree advancing)
{
	if (advancing != cb_int1) {
		cb_error (_("cannot specify NO ADVANCING in screen DISPLAY"));
	}
}

static cb_tree
get_default_display_device (void)
{
	if (current_program->flag_console_is_crt
	    || cb_console_is_crt) {
		return cb_null;
	} else {
		return cb_int0;
	}
}

static COB_INLINE COB_A_INLINE int
contains_one_screen_field (struct cb_list *x_list)
{
	return (cb_tree) x_list != cb_null
		&& cb_list_length ((cb_tree) x_list) == 1
		&& is_screen_field (x_list->value);
}

static int
contains_only_screen_fields (struct cb_list *x_list)
{
	if ((cb_tree) x_list == cb_null) {
		return 0;
	}

	for (; x_list; x_list = (struct cb_list *) x_list->chain) {
		if (!is_screen_field (x_list->value)) {
			return 0;
		}
	}

	return 1;
}

static int
contains_fields_and_screens (struct cb_list *x_list)
{
	int	field_seen = 0;
	int	screen_seen = 0;

	if ((cb_tree) x_list == cb_null) {
		return 0;
	}

	for (; x_list; x_list = (struct cb_list *) x_list->chain) {
		if (is_screen_field (x_list->value)) {
			screen_seen = 1;
		} else {
			field_seen = 1;
		}
	}

	return screen_seen && field_seen;
}

static enum cb_display_type
deduce_display_type (cb_tree x_list, cb_tree local_upon_value, cb_tree local_line_column,
		     struct cb_attr_struct * const attr_ptr)
{
	int	using_default_device_which_is_crt =
		local_upon_value == NULL && get_default_display_device () == cb_null;

	/* TODO: Seperate CGI DISPLAYs here */
	if (contains_only_screen_fields ((struct cb_list *) x_list)) {
		if (!contains_one_screen_field ((struct cb_list *) x_list)
		    || attr_ptr) {
			cb_verify_x (x_list, cb_accept_display_extensions,
				     _("non-standard DISPLAY"));
		}

		if (local_upon_value != NULL && local_upon_value != cb_null) {
			cb_error_x (x_list, _("screens may only be displayed on CRT"));
		}

		return SCREEN_DISPLAY;
	} else if (contains_fields_and_screens ((struct cb_list *) x_list)) {
		cb_error_x (x_list, _("cannot mix screens and fields in the same DISPLAY statement"));
		return MIXED_DISPLAY;
	} else if (local_line_column || attr_ptr) {
		if (local_upon_value != NULL && local_upon_value != cb_null) {
			cb_error_x (x_list, _("screen clauses may only be used for DISPLAY on CRT"));
		}

		cb_verify_x (x_list, cb_accept_display_extensions,
			     _("non-standard DISPLAY"));

		return FIELD_ON_SCREEN_DISPLAY;
	} else if (local_upon_value == cb_null || using_default_device_which_is_crt) {
		/* This is the only format permitted by the standard */
		return FIELD_ON_SCREEN_DISPLAY;
	} else if (display_type == FIELD_ON_SCREEN_DISPLAY && local_upon_value == NULL) {
		/* This is for when fields without clauses follow fields with screen clauses */
		return FIELD_ON_SCREEN_DISPLAY;
	} else {
		return DEVICE_DISPLAY;
	}
}

static void
set_display_type (cb_tree x_list, cb_tree local_upon_value,
		  cb_tree local_line_column, struct cb_attr_struct * const attr_ptr)
{
	display_type = deduce_display_type (x_list, local_upon_value, local_line_column, attr_ptr);
}

static void
error_if_different_display_type (cb_tree x_list, cb_tree local_upon_value,
				 cb_tree local_line_column, struct cb_attr_struct * const attr_ptr)
{
        const enum cb_display_type	type =
		deduce_display_type (x_list, local_upon_value, local_line_column, attr_ptr);

	/* Avoid re-displaying the same error for mixed DISPLAYs */
	if (type == display_type || display_type == MIXED_DISPLAY) {
		return;
	}

	if (type != MIXED_DISPLAY) {
		if (type == SCREEN_DISPLAY || display_type == SCREEN_DISPLAY) {
			cb_error_x (x_list, _("cannot mix screens and fields in the same DISPLAY statement"));
		} else {
			/*
			  The only other option is that there is a mix of
			  FIELD_ON_SCREEN_DISPLAY and DEVICE_DISPLAY.
			*/
			cb_error_x (x_list, _("ambiguous DISPLAY; put items to display on device in separate DISPLAY"));
		}
	}

	display_type = MIXED_DISPLAY;
}

static void
error_if_not_usage_display_or_nonnumeric_lit (cb_tree x)
{
	const int	is_numeric_literal = CB_NUMERIC_LITERAL_P (x);
	const int	is_field_with_usage_not_display =
		CB_REFERENCE_P (x) && CB_FIELD (cb_ref (x))
		&& CB_FIELD (cb_ref (x))->usage != CB_USAGE_DISPLAY;

	if (is_numeric_literal) {
		cb_error_x (x, _("%s is not an alphanumeric literal"), CB_LITERAL (x)->data);
	} else if (is_field_with_usage_not_display) {
		cb_error_x (x, _("'%s' is not USAGE DISPLAY"), cb_name (x));
	}
}


#line 1743 "parser.c" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 1
#endif

/* In a future release of Bison, this section will be replaced
   by #include "y.tab.h".  */
#ifndef YY_YY_PARSER_H_INCLUDED
# define YY_YY_PARSER_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    TOKEN_EOF = 0,
    ACCEPT = 258,
    ACCESS = 259,
    ADD = 260,
    ADDRESS = 261,
    ADVANCING = 262,
    AFTER = 263,
    ALL = 264,
    ALLOCATE = 265,
    ALPHABET = 266,
    ALPHABETIC = 267,
    ALPHABETIC_LOWER = 268,
    ALPHABETIC_UPPER = 269,
    ALPHANUMERIC = 270,
    ALPHANUMERIC_EDITED = 271,
    ALSO = 272,
    ALTER = 273,
    ALTERNATE = 274,
    AND = 275,
    ANY = 276,
    ARE = 277,
    AREA = 278,
    AREAS = 279,
    ARGUMENT_NUMBER = 280,
    ARGUMENT_VALUE = 281,
    AS = 282,
    ASCENDING = 283,
    ASCII = 284,
    ASSIGN = 285,
    AT = 286,
    ATTRIBUTE = 287,
    AUTO = 288,
    AUTOMATIC = 289,
    AWAY_FROM_ZERO = 290,
    BACKGROUND_COLOR = 291,
    BACKGROUND_HIGH = 292,
    BACKGROUND_LOW = 293,
    BACKGROUND_STANDARD = 294,
    BASED = 295,
    BEFORE = 296,
    BELL = 297,
    BINARY = 298,
    BINARY_C_LONG = 299,
    BINARY_CHAR = 300,
    BINARY_DOUBLE = 301,
    BINARY_LONG = 302,
    BINARY_SHORT = 303,
    BLANK = 304,
    BLINK = 305,
    BLOCK = 306,
    BOTTOM = 307,
    BOX = 308,
    BOXED = 309,
    BY = 310,
    BYTE_LENGTH = 311,
    CALL = 312,
    CANCEL = 313,
    CAPACITY = 314,
    CARD_PUNCH = 315,
    CARD_READER = 316,
    CASSETTE = 317,
    CD = 318,
    CF = 319,
    CH = 320,
    CHAINING = 321,
    CHARACTER = 322,
    CHARACTERS = 323,
    CLASS = 324,
    CLASSIFICATION = 325,
    CLASS_NAME = 326,
    CLOSE = 327,
    COBOL = 328,
    CODE = 329,
    CODE_SET = 330,
    COLLATING = 331,
    COL = 332,
    COLOR = 333,
    COLS = 334,
    COLUMN = 335,
    COLUMNS = 336,
    COMMA = 337,
    COMMAND_LINE = 338,
    COMMA_DELIM = 339,
    COMMIT = 340,
    COMMON = 341,
    COMMUNICATION = 342,
    COMP = 343,
    COMPUTE = 344,
    COMP_1 = 345,
    COMP_2 = 346,
    COMP_3 = 347,
    COMP_4 = 348,
    COMP_5 = 349,
    COMP_6 = 350,
    COMP_X = 351,
    CONCATENATE_FUNC = 352,
    CONDITION = 353,
    CONFIGURATION = 354,
    CONSTANT = 355,
    CONTAINS = 356,
    CONTENT = 357,
    CONTINUE = 358,
    CONTROL = 359,
    CONTROLS = 360,
    CONVERSION = 361,
    CONVERTING = 362,
    COPY = 363,
    CORRESPONDING = 364,
    COUNT = 365,
    CRT = 366,
    CRT_UNDER = 367,
    CURRENCY = 368,
    CURRENT_DATE_FUNC = 369,
    CURSOR = 370,
    CYCLE = 371,
    DATA = 372,
    DATE = 373,
    DAY = 374,
    DAY_OF_WEEK = 375,
    DE = 376,
    DEBUGGING = 377,
    DECIMAL_POINT = 378,
    DECLARATIVES = 379,
    DEFAULT = 380,
    DEFAULT_FONT = 381,
    DELETE = 382,
    DELIMITED = 383,
    DELIMITER = 384,
    DEPENDING = 385,
    DESCENDING = 386,
    DESTINATION = 387,
    DESTROY = 388,
    DETAIL = 389,
    DISABLE = 390,
    DISC = 391,
    DISK = 392,
    DISPLAY = 393,
    DISPLAY_OF_FUNC = 394,
    DIVIDE = 395,
    DIVISION = 396,
    DOWN = 397,
    DUPLICATES = 398,
    DYNAMIC = 399,
    EBCDIC = 400,
    EC = 401,
    ECHO = 402,
    EGI = 403,
    EIGHTY_EIGHT = 404,
    ENABLE = 405,
    ELSE = 406,
    EMI = 407,
    END = 408,
    END_ACCEPT = 409,
    END_ADD = 410,
    END_CALL = 411,
    END_COMPUTE = 412,
    END_DELETE = 413,
    END_DISPLAY = 414,
    END_DIVIDE = 415,
    END_EVALUATE = 416,
    END_FUNCTION = 417,
    END_IF = 418,
    END_MULTIPLY = 419,
    END_PERFORM = 420,
    END_PROGRAM = 421,
    END_READ = 422,
    END_RECEIVE = 423,
    END_RETURN = 424,
    END_REWRITE = 425,
    END_SEARCH = 426,
    END_START = 427,
    END_STRING = 428,
    END_SUBTRACT = 429,
    END_UNSTRING = 430,
    END_WRITE = 431,
    ENTRY = 432,
    ENTRY_CONVENTION = 433,
    ENVIRONMENT = 434,
    ENVIRONMENT_NAME = 435,
    ENVIRONMENT_VALUE = 436,
    EOL = 437,
    EOP = 438,
    EOS = 439,
    EQUAL = 440,
    ERASE = 441,
    ERROR = 442,
    ESCAPE = 443,
    ESI = 444,
    EVALUATE = 445,
    EVENT_STATUS = 446,
    EXCEPTION = 447,
    EXCEPTION_CONDITION = 448,
    EXCLUSIVE = 449,
    EXIT = 450,
    EXPONENTIATION = 451,
    EXTEND = 452,
    EXTERNAL = 453,
    EXTERNAL_FORM = 454,
    F = 455,
    FD = 456,
    FILE_CONTROL = 457,
    FILE_ID = 458,
    FILLER = 459,
    FINAL = 460,
    FIRST = 461,
    FIXED = 462,
    FIXED_FONT = 463,
    FLOAT_BINARY_128 = 464,
    FLOAT_BINARY_32 = 465,
    FLOAT_BINARY_64 = 466,
    FLOAT_DECIMAL_16 = 467,
    FLOAT_DECIMAL_34 = 468,
    FLOAT_DECIMAL_7 = 469,
    FLOAT_EXTENDED = 470,
    FLOAT_LONG = 471,
    FLOAT_SHORT = 472,
    FLOATING = 473,
    FONT = 474,
    FOOTING = 475,
    FOR = 476,
    FOREGROUND_COLOR = 477,
    FOREVER = 478,
    FORMATTED_DATE_FUNC = 479,
    FORMATTED_DATETIME_FUNC = 480,
    FORMATTED_TIME_FUNC = 481,
    FREE = 482,
    FROM = 483,
    FROM_CRT = 484,
    FULL = 485,
    FUNCTION = 486,
    FUNCTION_ID = 487,
    FUNCTION_NAME = 488,
    GENERATE = 489,
    GIVING = 490,
    GLOBAL = 491,
    GO = 492,
    GOBACK = 493,
    GRAPHICAL = 494,
    GREATER = 495,
    GREATER_OR_EQUAL = 496,
    GRID = 497,
    GROUP = 498,
    HANDLE = 499,
    HEADING = 500,
    HIGHLIGHT = 501,
    HIGH_VALUE = 502,
    ICON = 503,
    ID = 504,
    IDENTIFIED = 505,
    IDENTIFICATION = 506,
    IF = 507,
    IGNORE = 508,
    IGNORING = 509,
    IN = 510,
    INDEPENDENT = 511,
    INDEX = 512,
    INDEXED = 513,
    INDICATE = 514,
    INITIALIZE = 515,
    INITIALIZED = 516,
    INITIATE = 517,
    INPUT = 518,
    INPUT_OUTPUT = 519,
    INSPECT = 520,
    INTERMEDIATE = 521,
    INTO = 522,
    INTRINSIC = 523,
    INVALID = 524,
    INVALID_KEY = 525,
    IS = 526,
    I_O = 527,
    I_O_CONTROL = 528,
    JUSTIFIED = 529,
    KEPT = 530,
    KEY = 531,
    KEYBOARD = 532,
    LABEL = 533,
    LARGE_FONT = 534,
    LAST = 535,
    LAYOUT_MANAGER = 536,
    LEADING = 537,
    LEFT = 538,
    LEFTLINE = 539,
    LENGTH = 540,
    LENGTH_FUNC = 541,
    LENGTH_OF = 542,
    LESS = 543,
    LESS_OR_EQUAL = 544,
    LIMIT = 545,
    LIMITS = 546,
    LINAGE = 547,
    LINAGE_COUNTER = 548,
    LINE = 549,
    LINE_COUNTER = 550,
    LINES = 551,
    LINKAGE = 552,
    LITERAL = 553,
    LM_RESIZE = 554,
    LOCALE = 555,
    LOCALE_DATE_FUNC = 556,
    LOCALE_TIME_FUNC = 557,
    LOCALE_TIME_FROM_FUNC = 558,
    LOCAL_STORAGE = 559,
    LOCK = 560,
    LOWER = 561,
    LOWER_CASE_FUNC = 562,
    LOWLIGHT = 563,
    LOW_VALUE = 564,
    MANUAL = 565,
    MAGNETIC_TAPE = 566,
    MEMORY = 567,
    MEDIUM_FONT = 568,
    MENU = 569,
    MERGE = 570,
    MESSAGE = 571,
    MINUS = 572,
    MNEMONIC_NAME = 573,
    MODE = 574,
    MODULES = 575,
    MOVE = 576,
    MULTIPLE = 577,
    MULTIPLY = 578,
    NAME = 579,
    NATIONAL = 580,
    NATIONAL_EDITED = 581,
    NATIONAL_OF_FUNC = 582,
    NATIVE = 583,
    NEAREST_AWAY_FROM_ZERO = 584,
    NEAREST_EVEN = 585,
    NEAREST_TOWARD_ZERO = 586,
    NEGATIVE = 587,
    NESTED = 588,
    NEXT = 589,
    NEXT_PAGE = 590,
    NO = 591,
    NO_DATA = 592,
    NO_ECHO = 593,
    NORMAL = 594,
    NOT = 595,
    NOTHING = 596,
    NOT_END = 597,
    NOT_EOP = 598,
    NOT_ESCAPE = 599,
    NOT_EQUAL = 600,
    NOT_EXCEPTION = 601,
    NOT_INVALID_KEY = 602,
    NOT_OVERFLOW = 603,
    NOT_SIZE_ERROR = 604,
    NO_ADVANCING = 605,
    NUMBER = 606,
    NUMBERS = 607,
    NUMERIC = 608,
    NUMERIC_EDITED = 609,
    NUMVALC_FUNC = 610,
    OBJECT_COMPUTER = 611,
    OCCURS = 612,
    OF = 613,
    OFF = 614,
    OMITTED = 615,
    ON = 616,
    ONLY = 617,
    OPEN = 618,
    OPTIONAL = 619,
    OPTIONS = 620,
    OR = 621,
    ORDER = 622,
    ORGANIZATION = 623,
    OTHER = 624,
    OUTPUT = 625,
    OVERLINE = 626,
    PACKED_DECIMAL = 627,
    PADDING = 628,
    PAGE = 629,
    PAGE_COUNTER = 630,
    PARAGRAPH = 631,
    PERFORM = 632,
    PH = 633,
    PF = 634,
    PHYSICAL = 635,
    PICTURE = 636,
    PICTURE_SYMBOL = 637,
    PLUS = 638,
    POINTER = 639,
    POP_UP = 640,
    POSITION = 641,
    POSITIVE = 642,
    PRESENT = 643,
    PREVIOUS = 644,
    PRINT = 645,
    PRINTER = 646,
    PRINTER_1 = 647,
    PRINTING = 648,
    PRIORITY = 649,
    PROCEDURE = 650,
    PROCEDURES = 651,
    PROCEED = 652,
    PROGRAM = 653,
    PROGRAM_ID = 654,
    PROGRAM_NAME = 655,
    PROGRAM_POINTER = 656,
    PROHIBITED = 657,
    PROMPT = 658,
    PROTECTED = 659,
    PURGE = 660,
    QUEUE = 661,
    QUOTE = 662,
    RANDOM = 663,
    RD = 664,
    READ = 665,
    READY_TRACE = 666,
    RECEIVE = 667,
    RECORD = 668,
    RECORDING = 669,
    RECORDS = 670,
    RECURSIVE = 671,
    REDEFINES = 672,
    REEL = 673,
    REFERENCE = 674,
    REFERENCES = 675,
    RELATIVE = 676,
    RELEASE = 677,
    REMAINDER = 678,
    REMOVAL = 679,
    RENAMES = 680,
    REPLACE = 681,
    REPLACING = 682,
    REPORT = 683,
    REPORTING = 684,
    REPORTS = 685,
    REPOSITORY = 686,
    REQUIRED = 687,
    RESERVE = 688,
    RESET = 689,
    RESET_TRACE = 690,
    RETRY = 691,
    RETURN = 692,
    RETURNING = 693,
    REVERSE = 694,
    REVERSE_FUNC = 695,
    REVERSE_VIDEO = 696,
    REVERSED = 697,
    REWIND = 698,
    REWRITE = 699,
    RF = 700,
    RH = 701,
    RIGHT = 702,
    ROLLBACK = 703,
    ROUNDED = 704,
    ROUNDING = 705,
    RUN = 706,
    S = 707,
    SAME = 708,
    SCREEN = 709,
    SCREEN_CONTROL = 710,
    SCROLL = 711,
    SD = 712,
    SEARCH = 713,
    SECONDS = 714,
    SECTION = 715,
    SECURE = 716,
    SEGMENT = 717,
    SEGMENT_LIMIT = 718,
    SELECT = 719,
    SEMI_COLON = 720,
    SEND = 721,
    SENTENCE = 722,
    SEPARATE = 723,
    SEQUENCE = 724,
    SEQUENTIAL = 725,
    SET = 726,
    SEVENTY_EIGHT = 727,
    SHADOW = 728,
    SHARING = 729,
    SIGN = 730,
    SIGNED = 731,
    SIGNED_INT = 732,
    SIGNED_LONG = 733,
    SIGNED_SHORT = 734,
    SIXTY_SIX = 735,
    SIZE = 736,
    SIZE_ERROR = 737,
    SMALL_FONT = 738,
    SORT = 739,
    SORT_MERGE = 740,
    SOURCE = 741,
    SOURCE_COMPUTER = 742,
    SPACE = 743,
    SPECIAL_NAMES = 744,
    STANDARD = 745,
    STANDARD_1 = 746,
    STANDARD_2 = 747,
    START = 748,
    STATIC = 749,
    STATUS = 750,
    STDCALL = 751,
    STEP = 752,
    STOP = 753,
    STRING = 754,
    SUB_QUEUE_1 = 755,
    SUB_QUEUE_2 = 756,
    SUB_QUEUE_3 = 757,
    SUBSTITUTE_FUNC = 758,
    SUBSTITUTE_CASE_FUNC = 759,
    SUBTRACT = 760,
    SUBWINDOW = 761,
    SUM = 762,
    SUPPRESS = 763,
    SYMBOLIC = 764,
    SYNCHRONIZED = 765,
    SYSTEM_DEFAULT = 766,
    SYSTEM_OFFSET = 767,
    TAB = 768,
    TABLE = 769,
    TALLYING = 770,
    TAPE = 771,
    TERMINAL = 772,
    TERMINATE = 773,
    TEXT = 774,
    TEST = 775,
    THAN = 776,
    THEN = 777,
    THREAD = 778,
    THREADS = 779,
    THRU = 780,
    TIME = 781,
    TIME_OUT = 782,
    TIMES = 783,
    TITLE = 784,
    TO = 785,
    TOK_AMPER = 786,
    TOK_CLOSE_PAREN = 787,
    TOK_COLON = 788,
    TOK_DIV = 789,
    TOK_DOT = 790,
    TOK_EQUAL = 791,
    TOK_EXTERN = 792,
    TOK_FALSE = 793,
    TOK_FILE = 794,
    TOK_GREATER = 795,
    TOK_INITIAL = 796,
    TOK_LESS = 797,
    TOK_MINUS = 798,
    TOK_MUL = 799,
    TOK_NULL = 800,
    TOK_OVERFLOW = 801,
    TOK_OPEN_PAREN = 802,
    TOK_PLUS = 803,
    TOK_TRUE = 804,
    TOP = 805,
    TOWARD_GREATER = 806,
    TOWARD_LESSER = 807,
    TRADITIONAL_FONT = 808,
    TRAILING = 809,
    TRANSFORM = 810,
    TRIM_FUNC = 811,
    TRUNCATION = 812,
    TYPE = 813,
    U = 814,
    UNBOUNDED = 815,
    UNDERLINE = 816,
    UNIT = 817,
    UNLOCK = 818,
    UNSIGNED = 819,
    UNSIGNED_INT = 820,
    UNSIGNED_LONG = 821,
    UNSIGNED_SHORT = 822,
    UNSTRING = 823,
    UNTIL = 824,
    UP = 825,
    UPDATE = 826,
    UPON = 827,
    UPON_ARGUMENT_NUMBER = 828,
    UPON_COMMAND_LINE = 829,
    UPON_ENVIRONMENT_NAME = 830,
    UPON_ENVIRONMENT_VALUE = 831,
    UPPER = 832,
    UPPER_CASE_FUNC = 833,
    USAGE = 834,
    USE = 835,
    USER = 836,
    USER_DEFAULT = 837,
    USER_FUNCTION_NAME = 838,
    USING = 839,
    V = 840,
    VALUE = 841,
    VARIABLE = 842,
    VARIANT = 843,
    VARYING = 844,
    WAIT = 845,
    WHEN = 846,
    WHEN_COMPILED_FUNC = 847,
    WINDOW = 848,
    WITH = 849,
    WORD = 850,
    WORDS = 851,
    WORKING_STORAGE = 852,
    WRAP = 853,
    WRITE = 854,
    YYYYDDD = 855,
    YYYYMMDD = 856,
    ZERO = 857,
    SHIFT_PREFER = 858
  };
#endif
/* Tokens.  */
#define TOKEN_EOF 0
#define ACCEPT 258
#define ACCESS 259
#define ADD 260
#define ADDRESS 261
#define ADVANCING 262
#define AFTER 263
#define ALL 264
#define ALLOCATE 265
#define ALPHABET 266
#define ALPHABETIC 267
#define ALPHABETIC_LOWER 268
#define ALPHABETIC_UPPER 269
#define ALPHANUMERIC 270
#define ALPHANUMERIC_EDITED 271
#define ALSO 272
#define ALTER 273
#define ALTERNATE 274
#define AND 275
#define ANY 276
#define ARE 277
#define AREA 278
#define AREAS 279
#define ARGUMENT_NUMBER 280
#define ARGUMENT_VALUE 281
#define AS 282
#define ASCENDING 283
#define ASCII 284
#define ASSIGN 285
#define AT 286
#define ATTRIBUTE 287
#define AUTO 288
#define AUTOMATIC 289
#define AWAY_FROM_ZERO 290
#define BACKGROUND_COLOR 291
#define BACKGROUND_HIGH 292
#define BACKGROUND_LOW 293
#define BACKGROUND_STANDARD 294
#define BASED 295
#define BEFORE 296
#define BELL 297
#define BINARY 298
#define BINARY_C_LONG 299
#define BINARY_CHAR 300
#define BINARY_DOUBLE 301
#define BINARY_LONG 302
#define BINARY_SHORT 303
#define BLANK 304
#define BLINK 305
#define BLOCK 306
#define BOTTOM 307
#define BOX 308
#define BOXED 309
#define BY 310
#define BYTE_LENGTH 311
#define CALL 312
#define CANCEL 313
#define CAPACITY 314
#define CARD_PUNCH 315
#define CARD_READER 316
#define CASSETTE 317
#define CD 318
#define CF 319
#define CH 320
#define CHAINING 321
#define CHARACTER 322
#define CHARACTERS 323
#define CLASS 324
#define CLASSIFICATION 325
#define CLASS_NAME 326
#define CLOSE 327
#define COBOL 328
#define CODE 329
#define CODE_SET 330
#define COLLATING 331
#define COL 332
#define COLOR 333
#define COLS 334
#define COLUMN 335
#define COLUMNS 336
#define COMMA 337
#define COMMAND_LINE 338
#define COMMA_DELIM 339
#define COMMIT 340
#define COMMON 341
#define COMMUNICATION 342
#define COMP 343
#define COMPUTE 344
#define COMP_1 345
#define COMP_2 346
#define COMP_3 347
#define COMP_4 348
#define COMP_5 349
#define COMP_6 350
#define COMP_X 351
#define CONCATENATE_FUNC 352
#define CONDITION 353
#define CONFIGURATION 354
#define CONSTANT 355
#define CONTAINS 356
#define CONTENT 357
#define CONTINUE 358
#define CONTROL 359
#define CONTROLS 360
#define CONVERSION 361
#define CONVERTING 362
#define COPY 363
#define CORRESPONDING 364
#define COUNT 365
#define CRT 366
#define CRT_UNDER 367
#define CURRENCY 368
#define CURRENT_DATE_FUNC 369
#define CURSOR 370
#define CYCLE 371
#define DATA 372
#define DATE 373
#define DAY 374
#define DAY_OF_WEEK 375
#define DE 376
#define DEBUGGING 377
#define DECIMAL_POINT 378
#define DECLARATIVES 379
#define DEFAULT 380
#define DEFAULT_FONT 381
#define DELETE 382
#define DELIMITED 383
#define DELIMITER 384
#define DEPENDING 385
#define DESCENDING 386
#define DESTINATION 387
#define DESTROY 388
#define DETAIL 389
#define DISABLE 390
#define DISC 391
#define DISK 392
#define DISPLAY 393
#define DISPLAY_OF_FUNC 394
#define DIVIDE 395
#define DIVISION 396
#define DOWN 397
#define DUPLICATES 398
#define DYNAMIC 399
#define EBCDIC 400
#define EC 401
#define ECHO 402
#define EGI 403
#define EIGHTY_EIGHT 404
#define ENABLE 405
#define ELSE 406
#define EMI 407
#define END 408
#define END_ACCEPT 409
#define END_ADD 410
#define END_CALL 411
#define END_COMPUTE 412
#define END_DELETE 413
#define END_DISPLAY 414
#define END_DIVIDE 415
#define END_EVALUATE 416
#define END_FUNCTION 417
#define END_IF 418
#define END_MULTIPLY 419
#define END_PERFORM 420
#define END_PROGRAM 421
#define END_READ 422
#define END_RECEIVE 423
#define END_RETURN 424
#define END_REWRITE 425
#define END_SEARCH 426
#define END_START 427
#define END_STRING 428
#define END_SUBTRACT 429
#define END_UNSTRING 430
#define END_WRITE 431
#define ENTRY 432
#define ENTRY_CONVENTION 433
#define ENVIRONMENT 434
#define ENVIRONMENT_NAME 435
#define ENVIRONMENT_VALUE 436
#define EOL 437
#define EOP 438
#define EOS 439
#define EQUAL 440
#define ERASE 441
#define ERROR 442
#define ESCAPE 443
#define ESI 444
#define EVALUATE 445
#define EVENT_STATUS 446
#define EXCEPTION 447
#define EXCEPTION_CONDITION 448
#define EXCLUSIVE 449
#define EXIT 450
#define EXPONENTIATION 451
#define EXTEND 452
#define EXTERNAL 453
#define EXTERNAL_FORM 454
#define F 455
#define FD 456
#define FILE_CONTROL 457
#define FILE_ID 458
#define FILLER 459
#define FINAL 460
#define FIRST 461
#define FIXED 462
#define FIXED_FONT 463
#define FLOAT_BINARY_128 464
#define FLOAT_BINARY_32 465
#define FLOAT_BINARY_64 466
#define FLOAT_DECIMAL_16 467
#define FLOAT_DECIMAL_34 468
#define FLOAT_DECIMAL_7 469
#define FLOAT_EXTENDED 470
#define FLOAT_LONG 471
#define FLOAT_SHORT 472
#define FLOATING 473
#define FONT 474
#define FOOTING 475
#define FOR 476
#define FOREGROUND_COLOR 477
#define FOREVER 478
#define FORMATTED_DATE_FUNC 479
#define FORMATTED_DATETIME_FUNC 480
#define FORMATTED_TIME_FUNC 481
#define FREE 482
#define FROM 483
#define FROM_CRT 484
#define FULL 485
#define FUNCTION 486
#define FUNCTION_ID 487
#define FUNCTION_NAME 488
#define GENERATE 489
#define GIVING 490
#define GLOBAL 491
#define GO 492
#define GOBACK 493
#define GRAPHICAL 494
#define GREATER 495
#define GREATER_OR_EQUAL 496
#define GRID 497
#define GROUP 498
#define HANDLE 499
#define HEADING 500
#define HIGHLIGHT 501
#define HIGH_VALUE 502
#define ICON 503
#define ID 504
#define IDENTIFIED 505
#define IDENTIFICATION 506
#define IF 507
#define IGNORE 508
#define IGNORING 509
#define IN 510
#define INDEPENDENT 511
#define INDEX 512
#define INDEXED 513
#define INDICATE 514
#define INITIALIZE 515
#define INITIALIZED 516
#define INITIATE 517
#define INPUT 518
#define INPUT_OUTPUT 519
#define INSPECT 520
#define INTERMEDIATE 521
#define INTO 522
#define INTRINSIC 523
#define INVALID 524
#define INVALID_KEY 525
#define IS 526
#define I_O 527
#define I_O_CONTROL 528
#define JUSTIFIED 529
#define KEPT 530
#define KEY 531
#define KEYBOARD 532
#define LABEL 533
#define LARGE_FONT 534
#define LAST 535
#define LAYOUT_MANAGER 536
#define LEADING 537
#define LEFT 538
#define LEFTLINE 539
#define LENGTH 540
#define LENGTH_FUNC 541
#define LENGTH_OF 542
#define LESS 543
#define LESS_OR_EQUAL 544
#define LIMIT 545
#define LIMITS 546
#define LINAGE 547
#define LINAGE_COUNTER 548
#define LINE 549
#define LINE_COUNTER 550
#define LINES 551
#define LINKAGE 552
#define LITERAL 553
#define LM_RESIZE 554
#define LOCALE 555
#define LOCALE_DATE_FUNC 556
#define LOCALE_TIME_FUNC 557
#define LOCALE_TIME_FROM_FUNC 558
#define LOCAL_STORAGE 559
#define LOCK 560
#define LOWER 561
#define LOWER_CASE_FUNC 562
#define LOWLIGHT 563
#define LOW_VALUE 564
#define MANUAL 565
#define MAGNETIC_TAPE 566
#define MEMORY 567
#define MEDIUM_FONT 568
#define MENU 569
#define MERGE 570
#define MESSAGE 571
#define MINUS 572
#define MNEMONIC_NAME 573
#define MODE 574
#define MODULES 575
#define MOVE 576
#define MULTIPLE 577
#define MULTIPLY 578
#define NAME 579
#define NATIONAL 580
#define NATIONAL_EDITED 581
#define NATIONAL_OF_FUNC 582
#define NATIVE 583
#define NEAREST_AWAY_FROM_ZERO 584
#define NEAREST_EVEN 585
#define NEAREST_TOWARD_ZERO 586
#define NEGATIVE 587
#define NESTED 588
#define NEXT 589
#define NEXT_PAGE 590
#define NO 591
#define NO_DATA 592
#define NO_ECHO 593
#define NORMAL 594
#define NOT 595
#define NOTHING 596
#define NOT_END 597
#define NOT_EOP 598
#define NOT_ESCAPE 599
#define NOT_EQUAL 600
#define NOT_EXCEPTION 601
#define NOT_INVALID_KEY 602
#define NOT_OVERFLOW 603
#define NOT_SIZE_ERROR 604
#define NO_ADVANCING 605
#define NUMBER 606
#define NUMBERS 607
#define NUMERIC 608
#define NUMERIC_EDITED 609
#define NUMVALC_FUNC 610
#define OBJECT_COMPUTER 611
#define OCCURS 612
#define OF 613
#define OFF 614
#define OMITTED 615
#define ON 616
#define ONLY 617
#define OPEN 618
#define OPTIONAL 619
#define OPTIONS 620
#define OR 621
#define ORDER 622
#define ORGANIZATION 623
#define OTHER 624
#define OUTPUT 625
#define OVERLINE 626
#define PACKED_DECIMAL 627
#define PADDING 628
#define PAGE 629
#define PAGE_COUNTER 630
#define PARAGRAPH 631
#define PERFORM 632
#define PH 633
#define PF 634
#define PHYSICAL 635
#define PICTURE 636
#define PICTURE_SYMBOL 637
#define PLUS 638
#define POINTER 639
#define POP_UP 640
#define POSITION 641
#define POSITIVE 642
#define PRESENT 643
#define PREVIOUS 644
#define PRINT 645
#define PRINTER 646
#define PRINTER_1 647
#define PRINTING 648
#define PRIORITY 649
#define PROCEDURE 650
#define PROCEDURES 651
#define PROCEED 652
#define PROGRAM 653
#define PROGRAM_ID 654
#define PROGRAM_NAME 655
#define PROGRAM_POINTER 656
#define PROHIBITED 657
#define PROMPT 658
#define PROTECTED 659
#define PURGE 660
#define QUEUE 661
#define QUOTE 662
#define RANDOM 663
#define RD 664
#define READ 665
#define READY_TRACE 666
#define RECEIVE 667
#define RECORD 668
#define RECORDING 669
#define RECORDS 670
#define RECURSIVE 671
#define REDEFINES 672
#define REEL 673
#define REFERENCE 674
#define REFERENCES 675
#define RELATIVE 676
#define RELEASE 677
#define REMAINDER 678
#define REMOVAL 679
#define RENAMES 680
#define REPLACE 681
#define REPLACING 682
#define REPORT 683
#define REPORTING 684
#define REPORTS 685
#define REPOSITORY 686
#define REQUIRED 687
#define RESERVE 688
#define RESET 689
#define RESET_TRACE 690
#define RETRY 691
#define RETURN 692
#define RETURNING 693
#define REVERSE 694
#define REVERSE_FUNC 695
#define REVERSE_VIDEO 696
#define REVERSED 697
#define REWIND 698
#define REWRITE 699
#define RF 700
#define RH 701
#define RIGHT 702
#define ROLLBACK 703
#define ROUNDED 704
#define ROUNDING 705
#define RUN 706
#define S 707
#define SAME 708
#define SCREEN 709
#define SCREEN_CONTROL 710
#define SCROLL 711
#define SD 712
#define SEARCH 713
#define SECONDS 714
#define SECTION 715
#define SECURE 716
#define SEGMENT 717
#define SEGMENT_LIMIT 718
#define SELECT 719
#define SEMI_COLON 720
#define SEND 721
#define SENTENCE 722
#define SEPARATE 723
#define SEQUENCE 724
#define SEQUENTIAL 725
#define SET 726
#define SEVENTY_EIGHT 727
#define SHADOW 728
#define SHARING 729
#define SIGN 730
#define SIGNED 731
#define SIGNED_INT 732
#define SIGNED_LONG 733
#define SIGNED_SHORT 734
#define SIXTY_SIX 735
#define SIZE 736
#define SIZE_ERROR 737
#define SMALL_FONT 738
#define SORT 739
#define SORT_MERGE 740
#define SOURCE 741
#define SOURCE_COMPUTER 742
#define SPACE 743
#define SPECIAL_NAMES 744
#define STANDARD 745
#define STANDARD_1 746
#define STANDARD_2 747
#define START 748
#define STATIC 749
#define STATUS 750
#define STDCALL 751
#define STEP 752
#define STOP 753
#define STRING 754
#define SUB_QUEUE_1 755
#define SUB_QUEUE_2 756
#define SUB_QUEUE_3 757
#define SUBSTITUTE_FUNC 758
#define SUBSTITUTE_CASE_FUNC 759
#define SUBTRACT 760
#define SUBWINDOW 761
#define SUM 762
#define SUPPRESS 763
#define SYMBOLIC 764
#define SYNCHRONIZED 765
#define SYSTEM_DEFAULT 766
#define SYSTEM_OFFSET 767
#define TAB 768
#define TABLE 769
#define TALLYING 770
#define TAPE 771
#define TERMINAL 772
#define TERMINATE 773
#define TEXT 774
#define TEST 775
#define THAN 776
#define THEN 777
#define THREAD 778
#define THREADS 779
#define THRU 780
#define TIME 781
#define TIME_OUT 782
#define TIMES 783
#define TITLE 784
#define TO 785
#define TOK_AMPER 786
#define TOK_CLOSE_PAREN 787
#define TOK_COLON 788
#define TOK_DIV 789
#define TOK_DOT 790
#define TOK_EQUAL 791
#define TOK_EXTERN 792
#define TOK_FALSE 793
#define TOK_FILE 794
#define TOK_GREATER 795
#define TOK_INITIAL 796
#define TOK_LESS 797
#define TOK_MINUS 798
#define TOK_MUL 799
#define TOK_NULL 800
#define TOK_OVERFLOW 801
#define TOK_OPEN_PAREN 802
#define TOK_PLUS 803
#define TOK_TRUE 804
#define TOP 805
#define TOWARD_GREATER 806
#define TOWARD_LESSER 807
#define TRADITIONAL_FONT 808
#define TRAILING 809
#define TRANSFORM 810
#define TRIM_FUNC 811
#define TRUNCATION 812
#define TYPE 813
#define U 814
#define UNBOUNDED 815
#define UNDERLINE 816
#define UNIT 817
#define UNLOCK 818
#define UNSIGNED 819
#define UNSIGNED_INT 820
#define UNSIGNED_LONG 821
#define UNSIGNED_SHORT 822
#define UNSTRING 823
#define UNTIL 824
#define UP 825
#define UPDATE 826
#define UPON 827
#define UPON_ARGUMENT_NUMBER 828
#define UPON_COMMAND_LINE 829
#define UPON_ENVIRONMENT_NAME 830
#define UPON_ENVIRONMENT_VALUE 831
#define UPPER 832
#define UPPER_CASE_FUNC 833
#define USAGE 834
#define USE 835
#define USER 836
#define USER_DEFAULT 837
#define USER_FUNCTION_NAME 838
#define USING 839
#define V 840
#define VALUE 841
#define VARIABLE 842
#define VARIANT 843
#define VARYING 844
#define WAIT 845
#define WHEN 846
#define WHEN_COMPILED_FUNC 847
#define WINDOW 848
#define WITH 849
#define WORD 850
#define WORDS 851
#define WORKING_STORAGE 852
#define WRAP 853
#define WRITE 854
#define YYYYDDD 855
#define YYYYMMDD 856
#define ZERO 857
#define SHIFT_PREFER 858

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_PARSER_H_INCLUDED  */

/* Copy the second part of user declarations.  */

#line 3002 "parser.c" /* yacc.c:358  */

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif


#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  3
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   11100

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  604
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  1028
/* YYNRULES -- Number of rules.  */
#define YYNRULES  2384
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  3448

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   858

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint16 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,   134,
     135,   136,   137,   138,   139,   140,   141,   142,   143,   144,
     145,   146,   147,   148,   149,   150,   151,   152,   153,   154,
     155,   156,   157,   158,   159,   160,   161,   162,   163,   164,
     165,   166,   167,   168,   169,   170,   171,   172,   173,   174,
     175,   176,   177,   178,   179,   180,   181,   182,   183,   184,
     185,   186,   187,   188,   189,   190,   191,   192,   193,   194,
     195,   196,   197,   198,   199,   200,   201,   202,   203,   204,
     205,   206,   207,   208,   209,   210,   211,   212,   213,   214,
     215,   216,   217,   218,   219,   220,   221,   222,   223,   224,
     225,   226,   227,   228,   229,   230,   231,   232,   233,   234,
     235,   236,   237,   238,   239,   240,   241,   242,   243,   244,
     245,   246,   247,   248,   249,   250,   251,   252,   253,   254,
     255,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,   344,
     345,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   360,   361,   362,   363,   364,
     365,   366,   367,   368,   369,   370,   371,   372,   373,   374,
     375,   376,   377,   378,   379,   380,   381,   382,   383,   384,
     385,   386,   387,   388,   389,   390,   391,   392,   393,   394,
     395,   396,   397,   398,   399,   400,   401,   402,   403,   404,
     405,   406,   407,   408,   409,   410,   411,   412,   413,   414,
     415,   416,   417,   418,   419,   420,   421,   422,   423,   424,
     425,   426,   427,   428,   429,   430,   431,   432,   433,   434,
     435,   436,   437,   438,   439,   440,   441,   442,   443,   444,
     445,   446,   447,   448,   449,   450,   451,   452,   453,   454,
     455,   456,   457,   458,   459,   460,   461,   462,   463,   464,
     465,   466,   467,   468,   469,   470,   471,   472,   473,   474,
     475,   476,   477,   478,   479,   480,   481,   482,   483,   484,
     485,   486,   487,   488,   489,   490,   491,   492,   493,   494,
     495,   496,   497,   498,   499,   500,   501,   502,   503,   504,
     505,   506,   507,   508,   509,   510,   511,   512,   513,   514,
     515,   516,   517,   518,   519,   520,   521,   522,   523,   524,
     525,   526,   527,   528,   529,   530,   531,   532,   533,   534,
     535,   536,   537,   538,   539,   540,   541,   542,   543,   544,
     545,   546,   547,   548,   549,   550,   551,   552,   553,   554,
     555,   556,   557,   558,   559,   560,   561,   562,   563,   564,
     565,   566,   567,   568,   569,   570,   571,   572,   573,   574,
     575,   576,   577,   578,   579,   580,   581,   582,   583,   584,
     585,   586,   587,   588,   589,   590,   591,   592,   593,   594,
     595,   596,   597,   598,   599,   600,   601,   602,   603
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,  2422,  2422,  2422,  2454,  2455,  2459,  2459,  2468,  2469,
    2473,  2474,  2478,  2478,  2501,  2513,  2522,  2525,  2529,  2530,
    2534,  2542,  2551,  2559,  2560,  2569,  2569,  2574,  2578,  2573,
    2594,  2593,  2609,  2620,  2627,  2628,  2635,  2636,  2639,  2640,
    2644,  2653,  2662,  2663,  2670,  2671,  2675,  2679,  2685,  2687,
    2695,  2703,  2706,  2716,  2718,  2727,  2731,  2735,  2741,  2743,
    2750,  2754,  2758,  2762,  2771,  2776,  2777,  2786,  2790,  2791,
    2792,  2805,  2806,  2807,  2808,  2809,  2813,  2814,  2815,  2816,
    2817,  2820,  2821,  2831,  2832,  2836,  2837,  2838,  2839,  2851,
    2850,  2859,  2860,  2863,  2864,  2877,  2876,  2888,  2889,  2890,
    2891,  2895,  2896,  2900,  2901,  2902,  2903,  2907,  2915,  2922,
    2938,  2949,  2953,  2957,  2961,  2968,  2969,  2974,  2975,  2980,
    2979,  2990,  2991,  2992,  2999,  3000,  3004,  3008,  3014,  3015,
    3022,  3029,  3034,  3044,  3045,  3049,  3050,  3054,  3066,  3068,
    3072,  3073,  3077,  3078,  3082,  3083,  3084,  3085,  3086,  3087,
    3088,  3089,  3090,  3091,  3092,  3093,  3101,  3100,  3129,  3140,
    3159,  3167,  3170,  3171,  3175,  3182,  3197,  3218,  3217,  3241,
    3247,  3253,  3259,  3265,  3271,  3281,  3285,  3292,  3296,  3301,
    3300,  3311,  3315,  3322,  3323,  3324,  3325,  3326,  3327,  3331,
    3332,  3339,  3354,  3357,  3364,  3372,  3376,  3387,  3407,  3415,
    3426,  3427,  3433,  3454,  3455,  3459,  3463,  3484,  3507,  3582,
    3585,  3594,  3613,  3629,  3647,  3665,  3682,  3699,  3709,  3710,
    3717,  3718,  3726,  3727,  3737,  3738,  3743,  3742,  3781,  3782,
    3788,  3789,  3793,  3794,  3795,  3796,  3797,  3798,  3799,  3800,
    3801,  3802,  3803,  3804,  3805,  3812,  3818,  3828,  3839,  3852,
    3865,  3898,  3899,  3900,  3905,  3906,  3907,  3908,  3912,  3913,
    3914,  3915,  3916,  3917,  3918,  3921,  3922,  3928,  3929,  3933,
    3937,  3938,  3943,  3946,  3947,  3954,  3962,  3963,  3964,  3971,
    3993,  3995,  4000,  4010,  4018,  4033,  4040,  4042,  4043,  4049,
    4049,  4056,  4061,  4066,  4073,  4074,  4075,  4079,  4090,  4091,
    4095,  4100,  4105,  4110,  4121,  4132,  4142,  4151,  4152,  4153,
    4159,  4171,  4178,  4179,  4185,  4193,  4194,  4195,  4201,  4202,
    4203,  4210,  4211,  4215,  4216,  4222,  4250,  4251,  4252,  4253,
    4260,  4259,  4275,  4276,  4280,  4283,  4284,  4294,  4291,  4308,
    4309,  4317,  4318,  4326,  4327,  4331,  4352,  4351,  4368,  4375,
    4379,  4385,  4386,  4390,  4400,  4415,  4416,  4417,  4418,  4419,
    4420,  4421,  4422,  4423,  4430,  4437,  4437,  4437,  4443,  4467,
    4506,  4546,  4547,  4554,  4555,  4559,  4560,  4567,  4578,  4583,
    4594,  4595,  4599,  4600,  4606,  4617,  4635,  4636,  4640,  4641,
    4642,  4646,  4653,  4660,  4669,  4678,  4679,  4680,  4681,  4682,
    4691,  4692,  4698,  4735,  4736,  4749,  4764,  4765,  4769,  4779,
    4792,  4794,  4793,  4808,  4809,  4813,  4830,  4829,  4850,  4851,
    4855,  4856,  4857,  4860,  4862,  4863,  4867,  4868,  4872,  4873,
    4874,  4875,  4876,  4877,  4878,  4879,  4880,  4881,  4882,  4886,
    4890,  4892,  4896,  4897,  4901,  4902,  4903,  4904,  4905,  4906,
    4907,  4910,  4912,  4913,  4917,  4918,  4922,  4923,  4924,  4925,
    4926,  4927,  4931,  4936,  4938,  4937,  4953,  4957,  4957,  4974,
    4975,  4979,  4980,  4981,  4983,  4982,  4997,  5010,  5016,  5018,
    5022,  5029,  5033,  5044,  5047,  5059,  5060,  5061,  5063,  5067,
    5071,  5075,  5079,  5083,  5087,  5091,  5095,  5099,  5103,  5107,
    5111,  5115,  5126,  5127,  5131,  5132,  5136,  5137,  5138,  5142,
    5143,  5147,  5172,  5175,  5183,  5182,  5195,  5223,  5222,  5237,
    5241,  5248,  5252,  5256,  5263,  5264,  5268,  5269,  5270,  5271,
    5272,  5273,  5274,  5275,  5276,  5277,  5282,  5286,  5295,  5296,
    5297,  5298,  5299,  5300,  5301,  5302,  5303,  5304,  5305,  5306,
    5307,  5308,  5309,  5316,  5340,  5368,  5371,  5379,  5380,  5384,
    5409,  5420,  5421,  5425,  5429,  5433,  5437,  5441,  5445,  5449,
    5453,  5457,  5461,  5465,  5469,  5473,  5478,  5483,  5487,  5491,
    5495,  5500,  5504,  5509,  5513,  5518,  5522,  5526,  5534,  5538,
    5542,  5550,  5554,  5558,  5562,  5566,  5570,  5574,  5578,  5582,
    5590,  5598,  5602,  5606,  5610,  5614,  5618,  5626,  5627,  5631,
    5632,  5635,  5637,  5638,  5639,  5640,  5641,  5642,  5645,  5647,
    5653,  5660,  5673,  5682,  5683,  5692,  5699,  5711,  5729,  5730,
    5734,  5735,  5739,  5740,  5743,  5744,  5749,  5750,  5757,  5758,
    5764,  5766,  5768,  5767,  5776,  5777,  5781,  5805,  5806,  5810,
    5827,  5828,  5831,  5833,  5836,  5843,  5844,  5849,  5860,  5871,
    5882,  5893,  5922,  5921,  5930,  5931,  5935,  5936,  5939,  5941,
    5953,  5962,  5977,  6000,  6019,  6021,  6020,  6040,  6042,  6041,
    6057,  6059,  6058,  6067,  6068,  6075,  6074,  6087,  6088,  6089,
    6096,  6101,  6105,  6106,  6112,  6119,  6125,  6162,  6166,  6171,
    6177,  6178,  6183,  6184,  6185,  6186,  6187,  6191,  6198,  6205,
    6212,  6219,  6225,  6226,  6231,  6230,  6237,  6238,  6242,  6243,
    6244,  6245,  6246,  6247,  6248,  6249,  6250,  6251,  6252,  6253,
    6254,  6255,  6256,  6257,  6261,  6268,  6269,  6270,  6271,  6272,
    6273,  6274,  6277,  6278,  6279,  6282,  6283,  6287,  6294,  6300,
    6301,  6305,  6306,  6310,  6317,  6321,  6328,  6329,  6333,  6340,
    6341,  6345,  6346,  6350,  6351,  6352,  6356,  6357,  6361,  6362,
    6366,  6373,  6380,  6388,  6390,  6389,  6410,  6411,  6415,  6416,
    6420,  6422,  6421,  6481,  6499,  6501,  6505,  6510,  6515,  6519,
    6523,  6528,  6533,  6538,  6543,  6552,  6556,  6560,  6564,  6568,
    6574,  6578,  6583,  6588,  6593,  6598,  6603,  6608,  6617,  6621,
    6625,  6630,  6634,  6638,  6642,  6646,  6657,  6662,  6667,  6668,
    6669,  6670,  6671,  6672,  6673,  6674,  6675,  6684,  6689,  6700,
    6701,  6705,  6706,  6710,  6711,  6715,  6716,  6720,  6730,  6733,
    6737,  6744,  6754,  6757,  6761,  6768,  6780,  6790,  6800,  6818,
    6799,  6845,  6845,  6883,  6887,  6886,  6900,  6899,  6919,  6920,
    6925,  6947,  6949,  6953,  6964,  6966,  6974,  6982,  6990,  6996,
    7000,  7034,  7037,  7050,  7055,  7065,  7093,  7095,  7094,  7131,
    7132,  7136,  7137,  7138,  7156,  7157,  7169,  7168,  7216,  7217,
    7221,  7270,  7290,  7293,  7323,  7328,  7322,  7341,  7341,  7377,
    7384,  7385,  7386,  7387,  7388,  7389,  7390,  7391,  7392,  7393,
    7394,  7395,  7396,  7397,  7398,  7399,  7400,  7401,  7402,  7403,
    7404,  7405,  7406,  7407,  7408,  7409,  7410,  7411,  7412,  7413,
    7414,  7415,  7416,  7417,  7418,  7419,  7420,  7421,  7422,  7423,
    7424,  7425,  7426,  7427,  7428,  7429,  7430,  7431,  7432,  7433,
    7434,  7435,  7436,  7437,  7438,  7439,  7454,  7466,  7465,  7476,
    7475,  7510,  7509,  7520,  7524,  7528,  7533,  7538,  7543,  7548,
    7552,  7556,  7560,  7564,  7569,  7573,  7577,  7581,  7585,  7589,
    7593,  7597,  7604,  7605,  7611,  7613,  7617,  7618,  7622,  7623,
    7627,  7631,  7632,  7641,  7642,  7647,  7648,  7652,  7653,  7657,
    7673,  7689,  7702,  7706,  7707,  7711,  7718,  7724,  7730,  7735,
    7740,  7745,  7750,  7755,  7761,  7767,  7773,  7777,  7781,  7785,
    7789,  7800,  7805,  7810,  7815,  7820,  7825,  7831,  7836,  7841,
    7846,  7852,  7858,  7864,  7870,  7875,  7880,  7887,  7894,  7903,
    7904,  7905,  7909,  7910,  7911,  7915,  7916,  7920,  7924,  7942,
    7941,  7950,  7954,  7958,  7962,  7969,  7970,  7977,  7981,  7992,
    7991,  8001,  8005,  8017,  8018,  8026,  8025,  8034,  8035,  8039,
    8045,  8045,  8052,  8051,  8065,  8064,  8105,  8109,  8118,  8123,
    8128,  8148,  8154,  8174,  8178,  8188,  8192,  8197,  8201,  8200,
    8217,  8218,  8223,  8231,  8255,  8257,  8261,  8270,  8283,  8286,
    8290,  8294,  8299,  8322,  8323,  8327,  8328,  8332,  8336,  8340,
    8351,  8355,  8362,  8366,  8374,  8378,  8385,  8392,  8396,  8407,
    8406,  8418,  8422,  8429,  8430,  8440,  8439,  8447,  8448,  8452,
    8457,  8465,  8466,  8467,  8468,  8469,  8474,  8473,  8485,  8486,
    8494,  8493,  8502,  8509,  8513,  8523,  8534,  8552,  8551,  8560,
    8567,  8578,  8577,  8586,  8590,  8594,  8599,  8607,  8611,  8622,
    8621,  8630,  8633,  8635,  8641,  8643,  8644,  8645,  8646,  8654,
    8653,  8665,  8669,  8673,  8677,  8681,  8682,  8683,  8684,  8685,
    8689,  8697,  8706,  8707,  8712,  8711,  8755,  8759,  8766,  8767,
    8771,  8775,  8780,  8784,  8785,  8789,  8793,  8797,  8801,  8808,
    8809,  8814,  8813,  8831,  8833,  8837,  8838,  8842,  8846,  8847,
    8848,  8849,  8854,  8859,  8853,  8873,  8874,  8879,  8884,  8878,
    8903,  8902,  8923,  8924,  8925,  8929,  8930,  8935,  8938,  8945,
    8946,  8952,  8953,  8957,  8958,  8959,  8960,  8961,  8965,  8966,
    8970,  8971,  8976,  8977,  8981,  8991,  9007,  9012,  9018,  9024,
    9029,  9034,  9040,  9046,  9052,  9058,  9062,  9066,  9070,  9074,
    9079,  9084,  9089,  9094,  9100,  9105,  9110,  9117,  9127,  9131,
    9142,  9141,  9150,  9154,  9158,  9162,  9166,  9173,  9177,  9188,
    9187,  9199,  9198,  9207,  9226,  9225,  9252,  9260,  9261,  9266,
    9277,  9288,  9302,  9306,  9314,  9315,  9320,  9329,  9338,  9343,
    9352,  9353,  9358,  9433,  9434,  9435,  9439,  9440,  9444,  9448,
    9459,  9458,  9470,  9472,  9497,  9511,  9533,  9555,  9575,  9598,
    9599,  9607,  9606,  9615,  9626,  9625,  9635,  9642,  9641,  9654,
    9663,  9667,  9678,  9698,  9697,  9706,  9710,  9714,  9721,  9724,
    9731,  9737,  9743,  9748,  9760,  9759,  9767,  9775,  9776,  9780,
    9781,  9782,  9787,  9790,  9797,  9801,  9809,  9816,  9817,  9818,
    9819,  9820,  9821,  9822,  9834,  9837,  9847,  9846,  9855,  9861,
    9873,  9872,  9881,  9885,  9886,  9887,  9891,  9892,  9893,  9894,
    9901,  9900,  9921,  9931,  9940,  9944,  9951,  9956,  9961,  9966,
    9971,  9976,  9984,  9985,  9989,  9994, 10000, 10002, 10003, 10004,
   10005, 10009, 10037, 10040, 10044, 10048, 10052, 10059, 10066, 10076,
   10075, 10088, 10087, 10095, 10099, 10110, 10109, 10118, 10122, 10129,
   10133, 10144, 10143, 10151, 10152, 10156, 10181, 10182, 10183, 10184,
   10188, 10189, 10193, 10194, 10195, 10196, 10208, 10207, 10219, 10231,
   10228, 10242, 10254, 10262, 10269, 10273, 10286, 10293, 10305, 10308,
   10313, 10317, 10330, 10337, 10338, 10342, 10343, 10346, 10347, 10352,
   10362, 10361, 10374, 10373, 10383, 10412, 10413, 10417, 10421, 10425,
   10429, 10436, 10437, 10441, 10445, 10448, 10450, 10454, 10463, 10464,
   10465, 10468, 10470, 10474, 10478, 10482, 10490, 10491, 10495, 10496,
   10500, 10504, 10514, 10525, 10524, 10533, 10538, 10539, 10543, 10544,
   10545, 10549, 10550, 10554, 10558, 10559, 10563, 10567, 10571, 10581,
   10580, 10588, 10598, 10609, 10608, 10617, 10624, 10628, 10639, 10638,
   10650, 10659, 10662, 10666, 10670, 10677, 10681, 10691, 10703, 10702,
   10711, 10715, 10724, 10725, 10730, 10733, 10741, 10745, 10752, 10760,
   10764, 10775, 10774, 10782, 10785, 10790, 10792, 10796, 10802, 10803,
   10804, 10805, 10808, 10810, 10817, 10816, 10830, 10831, 10832, 10833,
   10834, 10835, 10836, 10837, 10841, 10842, 10846, 10847, 10853, 10862,
   10869, 10870, 10874, 10878, 10882, 10888, 10894, 10898, 10902, 10906,
   10915, 10919, 10923, 10932, 10941, 10942, 10946, 10955, 10956, 10960,
   10964, 10973, 10982, 10994, 10993, 11002, 11001, 11033, 11036, 11056,
   11057, 11060, 11061, 11069, 11070, 11075, 11080, 11090, 11107, 11112,
   11122, 11140, 11139, 11149, 11162, 11165, 11173, 11176, 11181, 11186,
   11194, 11195, 11196, 11197, 11198, 11199, 11203, 11211, 11212, 11216,
   11220, 11231, 11230, 11240, 11248, 11259, 11266, 11270, 11274, 11282,
   11294, 11297, 11304, 11308, 11315, 11316, 11317, 11318, 11325, 11324,
   11334, 11341, 11342, 11346, 11360, 11361, 11366, 11367, 11371, 11372,
   11376, 11380, 11391, 11390, 11399, 11403, 11407, 11411, 11419, 11423,
   11433, 11444, 11445, 11452, 11451, 11460, 11466, 11478, 11477, 11485,
   11499, 11498, 11506, 11523, 11522, 11532, 11540, 11541, 11546, 11547,
   11552, 11559, 11560, 11565, 11572, 11573, 11577, 11578, 11582, 11583,
   11587, 11591, 11602, 11601, 11610, 11611, 11612, 11613, 11614, 11618,
   11645, 11648, 11660, 11670, 11675, 11680, 11685, 11693, 11733, 11734,
   11738, 11781, 11791, 11814, 11815, 11816, 11817, 11821, 11830, 11836,
   11846, 11855, 11864, 11865, 11872, 11871, 11883, 11893, 11894, 11899,
   11902, 11906, 11910, 11917, 11918, 11922, 11923, 11924, 11928, 11932,
   11944, 11945, 11946, 11956, 11960, 11967, 11975, 11976, 11980, 11981,
   11985, 11993, 11994, 11999, 12000, 12001, 12011, 12015, 12022, 12030,
   12031, 12035, 12045, 12046, 12047, 12057, 12061, 12068, 12076, 12077,
   12081, 12091, 12092, 12093, 12103, 12107, 12114, 12122, 12123, 12127,
   12138, 12139, 12146, 12148, 12157, 12161, 12168, 12176, 12177, 12181,
   12191, 12192, 12202, 12206, 12213, 12221, 12222, 12226, 12236, 12237,
   12241, 12242, 12252, 12256, 12263, 12271, 12272, 12276, 12287, 12290,
   12299, 12302, 12310, 12314, 12323, 12327, 12337, 12345, 12352, 12352,
   12363, 12364, 12368, 12369, 12371, 12373, 12375, 12376, 12378, 12379,
   12380, 12381, 12382, 12384, 12385, 12386, 12389, 12391, 12395, 12398,
   12400, 12401, 12402, 12403, 12404, 12405, 12407, 12408, 12409, 12410,
   12411, 12414, 12415, 12419, 12420, 12424, 12425, 12429, 12430, 12434,
   12438, 12444, 12448, 12454, 12455, 12456, 12460, 12461, 12462, 12466,
   12467, 12468, 12472, 12476, 12480, 12481, 12482, 12485, 12486, 12496,
   12508, 12517, 12529, 12538, 12550, 12565, 12566, 12571, 12580, 12586,
   12596, 12610, 12632, 12636, 12657, 12669, 12710, 12724, 12725, 12730,
   12736, 12737, 12742, 12754, 12755, 12756, 12763, 12774, 12775, 12779,
   12787, 12795, 12799, 12806, 12815, 12816, 12822, 12831, 12842, 12859,
   12863, 12870, 12871, 12872, 12879, 12880, 12884, 12888, 12895, 12896,
   12900, 12901, 12905, 12906, 12907, 12908, 12912, 12916, 12920, 12924,
   12928, 12949, 12953, 12960, 12961, 12962, 12966, 12967, 12968, 12969,
   12970, 12974, 12978, 12985, 12986, 12990, 12991, 12995, 13002, 13009,
   13010, 13011, 13015, 13016, 13020, 13024, 13028, 13032, 13033, 13037,
   13041, 13042, 13046, 13050, 13051, 13060, 13064, 13070, 13074, 13078,
   13082, 13083, 13089, 13093, 13097, 13098, 13102, 13109, 13119, 13138,
   13156, 13163, 13170, 13177, 13187, 13191, 13198, 13205, 13215, 13225,
   13235, 13248, 13252, 13260, 13268, 13272, 13282, 13296, 13319, 13341,
   13357, 13358, 13359, 13360, 13361, 13362, 13366, 13370, 13387, 13391,
   13398, 13399, 13400, 13401, 13402, 13403, 13404, 13410, 13414, 13418,
   13422, 13426, 13430, 13435, 13439, 13443, 13447, 13451, 13455, 13459,
   13463, 13470, 13471, 13475, 13476, 13477, 13481, 13482, 13483, 13484,
   13488, 13492, 13496, 13503, 13507, 13511, 13518, 13525, 13532, 13542,
   13542, 13553, 13560, 13570, 13577, 13587, 13591, 13604, 13608, 13623,
   13631, 13632, 13636, 13637, 13641, 13642, 13647, 13650, 13658, 13661,
   13668, 13670, 13671, 13675, 13676, 13680, 13681, 13682, 13687, 13690,
   13703, 13707, 13715, 13719, 13723, 13727, 13731, 13735, 13739, 13743,
   13750, 13751, 13755, 13756, 13766, 13767, 13776, 13780, 13784, 13788,
   13795, 13796, 13797, 13798, 13799, 13800, 13801, 13802, 13803, 13804,
   13805, 13806, 13807, 13808, 13809, 13810, 13811, 13812, 13813, 13814,
   13815, 13816, 13817, 13818, 13819, 13820, 13821, 13822, 13823, 13824,
   13825, 13826, 13827, 13828, 13829, 13830, 13831, 13832, 13833, 13834,
   13835, 13836, 13837, 13838, 13839, 13840, 13841, 13842, 13846, 13847,
   13848, 13849, 13850, 13851, 13852, 13853, 13854, 13855, 13856, 13857,
   13858, 13859, 13860, 13861, 13862, 13863, 13864, 13865, 13866, 13873,
   13873, 13874, 13874, 13875, 13875, 13876, 13876, 13877, 13877, 13877,
   13878, 13878, 13879, 13879, 13880, 13880, 13881, 13881, 13882, 13882,
   13883, 13883, 13884, 13884, 13885, 13885, 13886, 13886, 13887, 13887,
   13888, 13888, 13889, 13889, 13890, 13890, 13891, 13891, 13892, 13892,
   13893, 13893, 13894, 13894, 13895, 13895, 13895, 13896, 13896, 13897,
   13897, 13898, 13898, 13899, 13899, 13900, 13900, 13901, 13901, 13901,
   13902, 13902, 13902, 13903, 13903, 13903, 13904, 13904, 13905, 13905,
   13905, 13906, 13906, 13907, 13907, 13907, 13908, 13908, 13908, 13909,
   13909, 13910, 13910, 13911, 13911, 13912, 13912, 13913, 13913, 13913,
   13914, 13914, 13915, 13915, 13916, 13916, 13916, 13916, 13917, 13917,
   13918, 13918, 13919, 13919, 13920, 13920, 13921, 13921, 13921, 13922,
   13922, 13923, 13923, 13924, 13924, 13925, 13925, 13925, 13926, 13926,
   13927, 13927, 13928, 13928, 13929, 13929, 13930, 13930, 13931, 13931,
   13932, 13932, 13933, 13933, 13934, 13934, 13935, 13935, 13935, 13936,
   13936, 13937, 13937, 13938, 13938, 13942, 13942, 13943, 13943, 13944,
   13944, 13945, 13945, 13946, 13946, 13947, 13947, 13948, 13948, 13949,
   13949, 13950, 13950, 13951, 13951, 13951, 13952, 13952, 13953, 13953,
   13954, 13954, 13955, 13955, 13956, 13956, 13959, 13960, 13961, 13965,
   13965, 13966, 13966, 13967, 13967, 13968, 13968, 13969, 13969, 13970,
   13970, 13971, 13971, 13972, 13972
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 1
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "$undefined", "ACCEPT", "ACCESS", "ADD",
  "ADDRESS", "ADVANCING", "AFTER", "ALL", "ALLOCATE", "ALPHABET",
  "ALPHABETIC", "\"ALPHABETIC-LOWER\"", "\"ALPHABETIC-UPPER\"",
  "ALPHANUMERIC", "\"ALPHANUMERIC-EDITED\"", "ALSO", "ALTER", "ALTERNATE",
  "AND", "ANY", "ARE", "AREA", "AREAS", "\"ARGUMENT-NUMBER\"",
  "\"ARGUMENT-VALUE\"", "AS", "ASCENDING", "ASCII", "ASSIGN", "AT",
  "ATTRIBUTE", "AUTO", "AUTOMATIC", "\"AWAY-FROM-ZERO\"",
  "\"BACKGROUND-COLOR\"", "\"BACKGROUND-HIGH\"", "\"BACKGROUND-LOW\"",
  "\"BACKGROUND-STANDARD\"", "BASED", "BEFORE", "BELL", "BINARY",
  "\"BINARY-C-LONG\"", "\"BINARY-CHAR\"", "\"BINARY-DOUBLE\"",
  "\"BINARY-LONG\"", "\"BINARY-SHORT\"", "BLANK", "BLINK", "BLOCK",
  "BOTTOM", "BOX", "BOXED", "BY", "\"BYTE-LENGTH\"", "CALL", "CANCEL",
  "CAPACITY", "\"CARD-PUNCH\"", "\"CARD-READER\"", "CASSETTE", "CD", "CF",
  "CH", "CHAINING", "CHARACTER", "CHARACTERS", "CLASS", "CLASSIFICATION",
  "\"class-name\"", "CLOSE", "COBOL", "CODE", "\"CODE-SET\"", "COLLATING",
  "COL", "COLOR", "COLS", "COLUMN", "COLUMNS", "COMMA", "\"COMMAND-LINE\"",
  "\"comma delimiter\"", "COMMIT", "COMMON", "COMMUNICATION", "COMP",
  "COMPUTE", "\"COMP-1\"", "\"COMP-2\"", "\"COMP-3\"", "\"COMP-4\"",
  "\"COMP-5\"", "\"COMP-6\"", "\"COMP-X\"", "\"FUNCTION CONCATENATE\"",
  "CONDITION", "CONFIGURATION", "CONSTANT", "CONTAINS", "CONTENT",
  "CONTINUE", "CONTROL", "CONTROLS", "CONVERSION", "CONVERTING", "COPY",
  "CORRESPONDING", "COUNT", "CRT", "\"CRT-UNDER\"", "CURRENCY",
  "\"FUNCTION CURRENT-DATE\"", "CURSOR", "CYCLE", "DATA", "DATE", "DAY",
  "\"DAY-OF-WEEK\"", "DE", "DEBUGGING", "\"DECIMAL-POINT\"",
  "DECLARATIVES", "DEFAULT", "\"DEFAULT-FONT\"", "DELETE", "DELIMITED",
  "DELIMITER", "DEPENDING", "DESCENDING", "DESTINATION", "DESTROY",
  "DETAIL", "DISABLE", "DISC", "DISK", "DISPLAY",
  "\"FUNCTION DISPLAY-OF\"", "DIVIDE", "DIVISION", "DOWN", "DUPLICATES",
  "DYNAMIC", "EBCDIC", "EC", "ECHO", "EGI", "\"88\"", "ENABLE", "ELSE",
  "EMI", "END", "\"END-ACCEPT\"", "\"END-ADD\"", "\"END-CALL\"",
  "\"END-COMPUTE\"", "\"END-DELETE\"", "\"END-DISPLAY\"", "\"END-DIVIDE\"",
  "\"END-EVALUATE\"", "\"END FUNCTION\"", "\"END-IF\"", "\"END-MULTIPLY\"",
  "\"END-PERFORM\"", "\"END PROGRAM\"", "\"END-READ\"", "\"END-RECEIVE\"",
  "\"END-RETURN\"", "\"END-REWRITE\"", "\"END-SEARCH\"", "\"END-START\"",
  "\"END-STRING\"", "\"END-SUBTRACT\"", "\"END-UNSTRING\"",
  "\"END-WRITE\"", "ENTRY", "\"ENTRY-CONVENTION\"", "ENVIRONMENT",
  "\"ENVIRONMENT-NAME\"", "\"ENVIRONMENT-VALUE\"", "EOL", "EOP", "EOS",
  "EQUAL", "ERASE", "ERROR", "ESCAPE", "ESI", "EVALUATE",
  "\"EVENT STATUS\"", "EXCEPTION", "\"EXCEPTION CONDITION\"", "EXCLUSIVE",
  "EXIT", "\"exponentiation operator\"", "EXTEND", "EXTERNAL",
  "\"EXTERNAL-FORM\"", "F", "FD", "\"FILE-CONTROL\"", "\"FILE-ID\"",
  "FILLER", "FINAL", "FIRST", "FIXED", "\"FIXED-FONT\"",
  "\"FLOAT-BINARY-128\"", "\"FLOAT-BINARY-32\"", "\"FLOAT-BINARY-64\"",
  "\"FLOAT-DECIMAL-16\"", "\"FLOAT-DECIMAL-34\"", "\"FLOAT-DECIMAL-7\"",
  "\"FLOAT-EXTENDED\"", "\"FLOAT-LONG\"", "\"FLOAT-SHORT\"", "FLOATING",
  "FONT", "FOOTING", "FOR", "\"FOREGROUND-COLOR\"", "FOREVER",
  "\"FUNCTION FORMATTED-DATE\"", "\"FUNCTION FORMATTED-DATETIME\"",
  "\"FUNCTION FORMATTED-TIME\"", "FREE", "FROM", "\"FROM CRT\"", "FULL",
  "FUNCTION", "\"FUNCTION-ID\"", "\"intrinsic function name\"", "GENERATE",
  "GIVING", "GLOBAL", "GO", "GOBACK", "GRAPHICAL", "GREATER",
  "\"GREATER OR EQUAL\"", "GRID", "GROUP", "HANDLE", "HEADING",
  "HIGHLIGHT", "\"HIGH-VALUE\"", "ICON", "ID", "IDENTIFIED",
  "IDENTIFICATION", "IF", "IGNORE", "IGNORING", "IN", "INDEPENDENT",
  "INDEX", "INDEXED", "INDICATE", "INITIALIZE", "INITIALIZED", "INITIATE",
  "INPUT", "\"INPUT-OUTPUT\"", "INSPECT", "INTERMEDIATE", "INTO",
  "INTRINSIC", "INVALID", "\"INVALID KEY\"", "IS", "\"I-O\"",
  "\"I-O-CONTROL\"", "JUSTIFIED", "KEPT", "KEY", "KEYBOARD", "LABEL",
  "\"LARGE-FONT\"", "LAST", "\"LAYOUT-MANAGER\"", "LEADING", "LEFT",
  "LEFTLINE", "LENGTH", "\"FUNCTION LENGTH/BYTE-LENGTH\"", "\"LENGTH OF\"",
  "LESS", "\"LESS OR EQUAL\"", "LIMIT", "LIMITS", "LINAGE",
  "\"LINAGE-COUNTER\"", "LINE", "\"LINE-COUNTER\"", "LINES", "LINKAGE",
  "\"Literal\"", "\"LM-RESIZE\"", "LOCALE", "\"FUNCTION LOCALE-DATE\"",
  "\"FUNCTION LOCALE-TIME\"", "\"FUNCTION LOCALE-TIME-FROM-SECONDS\"",
  "\"LOCAL-STORAGE\"", "LOCK", "LOWER", "\"FUNCTION LOWER-CASE\"",
  "LOWLIGHT", "\"LOW-VALUE\"", "MANUAL", "\"MAGNETIC-TAPE\"", "MEMORY",
  "\"MEDIUM-FONT\"", "MENU", "MERGE", "MESSAGE", "MINUS",
  "\"Mnemonic name\"", "MODE", "MODULES", "MOVE", "MULTIPLE", "MULTIPLY",
  "NAME", "NATIONAL", "\"NATIONAL-EDITED\"", "\"FUNCTION NATIONAL-OF\"",
  "NATIVE", "\"NEAREST-AWAY-FROM-ZERO\"", "\"NEAREST-EVEN\"",
  "\"NEAREST-TOWARD-ZERO\"", "NEGATIVE", "NESTED", "NEXT", "\"NEXT PAGE\"",
  "NO", "\"NO DATA\"", "\"NO-ECHO\"", "NORMAL", "NOT", "NOTHING",
  "\"NOT END\"", "\"NOT EOP\"", "\"NOT ESCAPE\"", "\"NOT EQUAL\"",
  "\"NOT EXCEPTION\"", "\"NOT INVALID KEY\"", "\"NOT OVERFLOW\"",
  "\"NOT SIZE ERROR\"", "\"NO ADVANCING\"", "NUMBER", "NUMBERS", "NUMERIC",
  "\"NUMERIC-EDITED\"", "\"FUNCTION NUMVAL-C\"", "\"OBJECT-COMPUTER\"",
  "OCCURS", "OF", "OFF", "OMITTED", "ON", "ONLY", "OPEN", "OPTIONAL",
  "OPTIONS", "OR", "ORDER", "ORGANIZATION", "OTHER", "OUTPUT", "OVERLINE",
  "\"PACKED-DECIMAL\"", "PADDING", "PAGE", "\"PAGE-COUNTER\"", "PARAGRAPH",
  "PERFORM", "PH", "PF", "PHYSICAL", "PICTURE", "\"PICTURE SYMBOL\"",
  "PLUS", "POINTER", "\"POP-UP\"", "POSITION", "POSITIVE", "PRESENT",
  "PREVIOUS", "PRINT", "PRINTER", "PRINTER_1", "PRINTING", "PRIORITY",
  "PROCEDURE", "PROCEDURES", "PROCEED", "PROGRAM", "\"PROGRAM-ID\"",
  "\"program name\"", "\"PROGRAM-POINTER\"", "PROHIBITED", "PROMPT",
  "\"PROTECTED\"", "PURGE", "QUEUE", "QUOTE", "RANDOM", "RD", "READ",
  "\"READY TRACE\"", "RECEIVE", "RECORD", "RECORDING", "RECORDS",
  "RECURSIVE", "REDEFINES", "REEL", "REFERENCE", "REFERENCES", "RELATIVE",
  "RELEASE", "REMAINDER", "REMOVAL", "RENAMES", "REPLACE", "REPLACING",
  "REPORT", "REPORTING", "REPORTS", "REPOSITORY", "REQUIRED", "RESERVE",
  "RESET", "\"RESET TRACE\"", "RETRY", "RETURN", "RETURNING", "REVERSE",
  "\"FUNCTION REVERSE\"", "\"REVERSE-VIDEO\"", "REVERSED", "REWIND",
  "REWRITE", "RF", "RH", "RIGHT", "ROLLBACK", "ROUNDED", "ROUNDING", "RUN",
  "S", "SAME", "SCREEN", "\"SCREEN CONTROL\"", "SCROLL", "SD", "SEARCH",
  "SECONDS", "SECTION", "SECURE", "SEGMENT", "\"SEGMENT-LIMIT\"", "SELECT",
  "\"semi-colon\"", "SEND", "SENTENCE", "SEPARATE", "SEQUENCE",
  "SEQUENTIAL", "SET", "\"78\"", "SHADOW", "SHARING", "SIGN", "SIGNED",
  "\"SIGNED-INT\"", "\"SIGNED-LONG\"", "\"SIGNED-SHORT\"", "\"66\"",
  "SIZE", "\"SIZE ERROR\"", "\"SMALL-FONT\"", "SORT", "\"SORT-MERGE\"",
  "SOURCE", "\"SOURCE-COMPUTER\"", "SPACE", "\"SPECIAL-NAMES\"",
  "STANDARD", "\"STANDARD-1\"", "\"STANDARD-2\"", "START", "STATIC",
  "STATUS", "STDCALL", "STEP", "STOP", "STRING", "SUB_QUEUE_1",
  "SUB_QUEUE_2", "SUB_QUEUE_3", "\"FUNCTION SUBSTITUTE\"",
  "\"FUNCTION SUBSTITUTE-CASE\"", "SUBTRACT", "SUBWINDOW", "SUM",
  "SUPPRESS", "SYMBOLIC", "SYNCHRONIZED", "\"SYSTEM-DEFAULT\"",
  "\"SYSTEM-OFFSET\"", "TAB", "TABLE", "TALLYING", "TAPE", "TERMINAL",
  "TERMINATE", "TEXT", "TEST", "THAN", "THEN", "THREAD", "THREADS", "THRU",
  "TIME", "\"TIME-OUT\"", "TIMES", "TITLE", "TO", "\"&\"", "\")\"",
  "\":\"", "\"/\"", "\".\"", "\"=\"", "\"EXTERN\"", "\"FALSE\"",
  "\"FILE\"", "\">\"", "\"INITIAL\"", "\"<\"", "\"-\"", "\"*\"",
  "\"NULL\"", "\"OVERFLOW\"", "\"(\"", "\"+\"", "\"TRUE\"", "TOP",
  "\"TOWARD-GREATER\"", "\"TOWARD-LESSER\"", "\"TRADITIONAL-FONT\"",
  "TRAILING", "TRANSFORM", "\"FUNCTION TRIM\"", "TRUNCATION", "TYPE", "U",
  "UNBOUNDED", "UNDERLINE", "UNIT", "UNLOCK", "UNSIGNED",
  "\"UNSIGNED-INT\"", "\"UNSIGNED-LONG\"", "\"UNSIGNED-SHORT\"",
  "UNSTRING", "UNTIL", "UP", "UPDATE", "UPON", "\"UPON ARGUMENT-NUMBER\"",
  "\"UPON COMMAND-LINE\"", "\"UPON ENVIRONMENT-NAME\"",
  "\"UPON ENVIRONMENT-VALUE\"", "UPPER", "\"FUNCTION UPPER-CASE\"",
  "USAGE", "USE", "USER", "\"USER-DEFAULT\"", "\"user function name\"",
  "USING", "V", "VALUE", "VARIABLE", "VARIANT", "VARYING", "WAIT", "WHEN",
  "\"FUNCTION WHEN-COMPILED\"", "WINDOW", "WITH", "\"Identifier\"",
  "WORDS", "\"WORKING-STORAGE\"", "WRAP", "WRITE", "YYYYDDD", "YYYYMMDD",
  "ZERO", "SHIFT_PREFER", "$accept", "start", "$@1", "compilation_group",
  "nested_list", "$@2", "source_element_list", "source_element",
  "simple_prog", "$@3", "program_definition", "function_definition",
  "_end_program_list", "end_program_list", "end_program", "end_function",
  "_program_body", "_identification_header", "identification_or_id",
  "program_id_paragraph", "$@4", "$@5", "function_id_paragraph", "$@6",
  "program_id_name", "end_program_name", "_as_literal", "_program_type",
  "program_type_clause", "init_or_recurse_and_common", "init_or_recurse",
  "_options_paragraph", "_options_clauses", "_default_rounded_clause",
  "_entry_convention_clause", "convention_type",
  "_intermediate_rounding_clause", "intermediate_rounding_choice",
  "_environment_division", "_environment_header", "_configuration_section",
  "_configuration_paragraphs", "standard_order_conf_section",
  "nonstandard_order_conf_section", "_configuration_header",
  "_source_object_computer_paragraphs",
  "source_object_computer_paragraphs", "source_computer_paragraph", "$@7",
  "_source_computer_entry", "_with_debugging_mode",
  "object_computer_paragraph", "$@8", "_object_computer_entry",
  "object_clauses_list", "object_clauses", "object_computer_memory",
  "object_computer_sequence", "object_computer_segment",
  "object_computer_class", "locale_class", "computer_words",
  "_repository_paragraph", "repository_paragraph", "$@9",
  "_repository_entry", "repository_list", "repository_name",
  "repository_name_list", "_special_names_paragraph",
  "special_names_paragraph", "special_names_header",
  "_special_names_sentence_list", "special_names_sentence_list",
  "special_name_list", "special_name", "mnemonic_name_clause", "$@10",
  "mnemonic_choices", "_special_name_mnemonic_on_off", "on_off_clauses",
  "on_off_clauses_1", "alphabet_name_clause", "@11", "alphabet_definition",
  "alphabet_literal_list", "alphabet_literal", "@12",
  "alphabet_also_sequence", "alphabet_lits", "space_or_zero",
  "symbolic_characters_clause", "_sym_in_word", "symbolic_collection",
  "symbolic_chars_list", "symbolic_chars_phrase", "char_list",
  "integer_list", "class_name_clause", "class_item_list", "class_item",
  "locale_clause", "currency_sign_clause", "_with_pic_symbol",
  "decimal_point_clause", "numeric_sign_clause", "cursor_clause",
  "crt_status_clause", "screen_control", "event_status",
  "_input_output_section", "_input_output_header", "_file_control_header",
  "_i_o_control_header", "_file_control_sequence", "file_control_entry",
  "$@13", "_select_clauses_or_error", "_select_clause_sequence",
  "select_clause", "assign_clause", "printer_name", "general_device_name",
  "line_seq_device_name", "_line_adv_file", "_ext_clause",
  "assignment_name", "_assignment_name", "access_mode_clause",
  "access_mode", "alternative_record_key_clause", "_suppress_clause",
  "collating_sequence_clause", "alphabet_name", "file_status_clause",
  "_file_or_sort", "lock_mode_clause", "$@14", "lock_mode", "_lock_with",
  "organization_clause", "organization", "padding_character_clause",
  "record_delimiter_clause", "record_key_clause", "key_or_split_keys",
  "relative_key_clause", "reserve_clause", "no_or_integer",
  "sharing_clause", "sharing_option", "_i_o_control", "i_o_control_list",
  "i_o_control_clause", "same_clause", "_same_option",
  "multiple_file_tape_clause", "$@15", "multiple_file_list",
  "multiple_file", "_multiple_file_position", "_data_division", "$@16",
  "_data_division_header", "_file_section_header",
  "_file_description_sequence", "file_description",
  "file_description_entry", "$@17", "file_type",
  "_file_description_clause_sequence", "file_description_clause",
  "block_contains_clause", "_records_or_characters", "record_clause",
  "_record_depending", "_from_integer", "_to_integer",
  "label_records_clause", "value_of_clause", "file_id", "valueof_name",
  "data_records_clause", "linage_clause", "_linage_sequence",
  "linage_lines", "linage_footing", "linage_top", "linage_bottom",
  "recording_mode_clause", "recording_mode", "u_or_s", "code_set_clause",
  "_for_sub_records_clause", "report_clause", "report_keyword",
  "rep_name_list", "_communication_section", "$@18",
  "_communication_description_sequence", "communication_description",
  "communication_description_entry", "$@19",
  "_communication_description_clause_sequence",
  "communication_description_clause", "_input_cd_clauses",
  "named_input_cd_clauses", "named_input_cd_clause",
  "unnamed_input_cd_clauses", "_output_cd_clauses", "output_cd_clauses",
  "output_cd_clause", "_i_o_cd_clauses", "named_i_o_cd_clauses",
  "named_i_o_cd_clause", "unnamed_i_o_cd_clauses",
  "_working_storage_section", "$@20", "_record_description_list", "$@21",
  "record_description_list", "data_description", "$@22", "level_number",
  "_filler", "_entry_name", "user_entry_name", "const_global",
  "lit_or_length", "con_identifier", "fp32_usage", "fp64_usage",
  "fp128_usage", "pointer_len", "renames_entry", "_renames_thru",
  "condition_name_entry", "$@23", "constant_entry", "$@24",
  "constant_source", "constant_78_source", "constant_expression_list",
  "constant_expression", "_data_description_clause_sequence",
  "data_description_clause", "redefines_clause", "external_clause",
  "_as_extname", "_global_clause", "global_clause", "picture_clause",
  "usage_clause", "usage", "float_usage", "double_usage", "_font_name",
  "_layout_name", "sign_clause", "report_occurs_clause", "_occurs_step",
  "occurs_clause", "_occurs_to_integer", "_occurs_from_integer",
  "_occurs_integer_to", "_occurs_depending", "_capacity_in",
  "_occurs_initialized", "_occurs_keys_and_indexed", "$@25", "occurs_keys",
  "occurs_key_list", "occurs_key_field", "ascending_or_descending",
  "_occurs_indexed", "occurs_indexed", "occurs_index_list", "occurs_index",
  "justified_clause", "synchronized_clause", "blank_clause",
  "based_clause", "value_clause", "$@26", "value_item_list", "value_item",
  "_false_is", "any_length_clause", "external_form_clause",
  "identified_by_clause", "_local_storage_section", "$@27",
  "_linkage_section", "$@28", "_report_section", "$@29",
  "_report_description_sequence", "report_description", "$@30",
  "_report_description_options", "report_description_option",
  "control_clause", "control_field_list", "page_limit_clause",
  "page_line_column", "_page_heading_list", "page_detail",
  "heading_clause", "first_detail", "last_heading", "last_detail",
  "footing_clause", "_report_group_description_list",
  "report_group_description_entry", "$@31", "_report_group_options",
  "report_group_option", "type_clause", "type_option", "_control_final",
  "_or_page", "next_group_clause", "sum_clause_list", "_reset_clause",
  "data_or_final", "present_when_condition", "varying_clause",
  "line_clause", "line_keyword_clause", "column_clause",
  "col_keyword_clause", "report_line_integer_list", "line_or_plus",
  "report_col_integer_list", "col_or_plus", "source_clause",
  "group_indicate_clause", "report_usage_clause", "_screen_section",
  "$@32", "_screen_description_list", "screen_description_list",
  "screen_description", "$@33", "_screen_options", "screen_option", "eol",
  "eos", "plus_plus", "minus_minus", "screen_line_number",
  "_screen_line_plus_minus", "screen_col_number", "_screen_col_plus_minus",
  "screen_occurs_clause", "screen_global_clause", "_procedure_division",
  "$@34", "$@35", "$@36", "_procedure_using_chaining", "$@37", "$@38",
  "procedure_param_list", "procedure_param", "_procedure_type",
  "_size_optional", "size_is_integer", "_procedure_optional",
  "_procedure_returning", "_procedure_declaratives", "$@39",
  "_procedure_list", "procedure", "section_header", "$@40",
  "_use_statement", "paragraph_header", "invalid_statement", "_segment",
  "statement_list", "@41", "@42", "statements", "$@43", "statement",
  "accept_statement", "$@44", "accept_body", "$@45", "$@46",
  "accp_identifier", "_accept_clauses", "accept_clauses", "accept_clause",
  "accept_from_screen_clauses", "accept_from_screen_clause",
  "lines_or_number", "at_line_column", "line_number", "column_number",
  "mode_is_block", "accp_attr", "no_echo", "reverse_video",
  "update_default", "end_accept", "add_statement", "$@47", "add_body",
  "_add_to", "end_add", "allocate_statement", "$@48", "allocate_body",
  "allocate_returning", "alter_statement", "$@49", "alter_body",
  "alter_entry", "_proceed_to", "call_statement", "$@50", "call_body",
  "$@51", "_mnemonic_conv", "program_or_prototype",
  "_id_or_lit_or_func_as", "nested_or_prototype", "call_using", "$@52",
  "call_param_list", "call_param", "call_type", "call_returning",
  "return_give", "null_or_omitted", "call_exception_phrases",
  "_call_on_exception", "call_on_exception", "_call_not_on_exception",
  "call_not_on_exception", "end_call", "cancel_statement", "$@53",
  "cancel_body", "id_or_lit_or_program_name", "close_statement", "$@54",
  "close_body", "close_files", "_close_option", "close_window", "$@55",
  "_close_display_option", "compute_statement", "$@56", "compute_body",
  "end_compute", "commit_statement", "continue_statement",
  "destroy_statement", "$@57", "destroy_body", "delete_statement", "$@58",
  "delete_body", "delete_file_list", "end_delete", "disable_statement",
  "$@59", "enable_disable_handling", "_enable_disable_key",
  "communication_mode", "display_statement", "$@60", "display_body",
  "screen_or_device_display", "display_list", "display_atom", "$@61",
  "disp_list", "display_clauses", "display_clause", "display_upon",
  "crt_under", "display_message_box", "$@62", "_display_message_clauses",
  "display_message_clauses", "display_message_clause", "display_window",
  "$@63", "$@64", "sub_or_window", "display_floating_window", "@65",
  "$@66", "display_initial_window", "$@67", "intial_type", "_graphical",
  "_upon_window_handle", "display_window_clauses", "display_window_clause",
  "no_scroll_wrap", "shadow_boxed", "pop_up_or_handle", "pop_up_area",
  "handle_is_in", "disp_attr", "end_display", "divide_statement", "$@68",
  "divide_body", "end_divide", "enable_statement", "$@69",
  "entry_statement", "$@70", "entry_body", "evaluate_statement", "$@71",
  "evaluate_body", "evaluate_subject_list", "evaluate_subject",
  "evaluate_condition_list", "evaluate_case_list", "evaluate_case",
  "evaluate_other", "evaluate_when_list", "evaluate_object_list",
  "evaluate_object", "_evaluate_thru_expr", "end_evaluate",
  "exit_statement", "$@72", "exit_body", "exit_program_returning",
  "free_statement", "$@73", "free_body", "generate_statement", "$@74",
  "generate_body", "goto_statement", "$@75", "go_body", "goto_depending",
  "goback_statement", "if_statement", "$@76", "if_else_statements",
  "if_then", "if_true", "if_false", "end_if", "initialize_statement",
  "$@77", "initialize_body", "_initialize_filler", "_initialize_value",
  "_initialize_replacing", "initialize_replacing_list",
  "initialize_replacing_item", "initialize_category",
  "_initialize_default", "initiate_statement", "$@78", "initiate_body",
  "inspect_statement", "$@79", "inspect_body", "send_identifier",
  "inspect_list", "inspect_tallying", "$@80", "inspect_replacing",
  "inspect_converting", "tallying_list", "tallying_item", "replacing_list",
  "replacing_item", "rep_keyword", "replacing_region", "inspect_region",
  "inspect_before", "inspect_after", "merge_statement", "$@81",
  "move_statement", "$@82", "move_body", "multiply_statement", "$@83",
  "multiply_body", "end_multiply", "open_statement", "$@84", "open_body",
  "open_file_entry", "open_mode", "open_sharing", "open_option",
  "perform_statement", "$@85", "perform_body", "$@86", "end_perform",
  "term_or_dot", "perform_procedure", "perform_option", "perform_test",
  "cond_or_exit", "perform_varying_list", "perform_varying",
  "purge_statement", "$@87", "read_statement", "$@88", "read_body",
  "_read_into", "_lock_phrases", "ignoring_lock",
  "advancing_lock_or_retry", "_retry_phrase", "retry_phrase",
  "retry_options", "_extended_with_lock", "extended_with_lock",
  "_read_key", "read_handler", "end_read", "ready_statement",
  "receive_statement", "$@89", "receive_body", "message_or_segment",
  "_data_sentence_phrases", "_no_data_sentence", "no_data_sentence",
  "_with_data_sentence", "with_data_sentence", "end_receive",
  "release_statement", "$@90", "release_body", "reset_statement",
  "return_statement", "$@91", "return_body", "end_return",
  "rewrite_statement", "$@92", "rewrite_body", "_with_lock", "with_lock",
  "end_rewrite", "rollback_statement", "search_statement", "$@93",
  "search_body", "search_varying", "search_at_end", "search_whens",
  "search_when", "end_search", "send_statement", "$@94", "send_body",
  "_from_identifier", "from_identifier", "with_indicator",
  "_replacing_line", "set_statement", "$@95", "set_body", "on_or_off",
  "up_or_down", "set_environment", "set_attr", "set_attr_clause",
  "set_attr_one", "set_to", "set_up_down", "set_to_on_off_sequence",
  "set_to_on_off", "set_to_true_false_sequence", "set_to_true_false",
  "set_last_exception_to_off", "set_thread_priority", "sort_statement",
  "$@96", "sort_body", "@97", "sort_key_list", "_key_list",
  "_sort_duplicates", "sort_collating", "sort_input", "sort_output",
  "start_statement", "$@98", "start_body", "sizelen_clause", "start_key",
  "start_op", "disallowed_op", "not_equal_op", "end_start",
  "stop_statement", "$@99", "stop_returning", "_status_x", "stop_argument",
  "stop_literal", "string_statement", "$@100", "string_body",
  "string_item_list", "string_item", "_string_delimited",
  "string_delimiter", "_with_pointer", "end_string", "subtract_statement",
  "$@101", "subtract_body", "end_subtract", "suppress_statement",
  "_printing", "terminate_statement", "$@102", "terminate_body",
  "transform_statement", "$@103", "transform_body", "unlock_statement",
  "$@104", "unlock_body", "unstring_statement", "$@105", "unstring_body",
  "_unstring_delimited", "unstring_delimited_list",
  "unstring_delimited_item", "unstring_into", "unstring_into_item",
  "_unstring_into_delimiter", "_unstring_into_count", "_unstring_tallying",
  "end_unstring", "use_statement", "$@106", "use_phrase",
  "use_file_exception", "use_global", "use_file_exception_target",
  "use_debugging", "debugging_list", "debugging_target", "_all_refs",
  "use_start_end", "program_start_end", "use_reporting", "use_exception",
  "use_ex_keyw", "write_statement", "$@107", "write_body", "from_option",
  "write_option", "before_or_after", "write_handler", "end_write",
  "_accept_exception_phrases", "_accp_on_exception", "accp_on_exception",
  "escape_or_exception", "_accp_not_on_exception", "accp_not_on_exception",
  "not_escape_or_not_exception", "_display_exception_phrases",
  "_disp_on_exception", "disp_on_exception", "_disp_not_on_exception",
  "disp_not_on_exception", "on_size_error_phrases", "_on_size_error",
  "on_size_error", "_not_on_size_error", "not_on_size_error",
  "_on_overflow_phrases", "_on_overflow", "on_overflow",
  "_not_on_overflow", "not_on_overflow", "return_at_end", "at_end",
  "_at_end_clause", "at_end_clause", "_not_at_end_clause",
  "not_at_end_clause", "at_eop_clauses", "_at_eop_clause", "at_eop_clause",
  "_not_at_eop_clause", "not_at_eop_clause", "_invalid_key_phrases",
  "invalid_key_phrases", "_invalid_key_sentence", "invalid_key_sentence",
  "_not_invalid_key_sentence", "not_invalid_key_sentence", "_thread_start",
  "_thread_handle", "thread_reference_optional", "_scroll_lines",
  "condition", "expr", "partial_expr", "$@108", "expr_tokens",
  "expr_token", "_not", "not", "condition_or_class", "eq", "gt", "lt",
  "ge", "le", "exp_list", "_e_sep", "exp", "exp_term", "exp_factor",
  "exp_unary", "exp_atom", "line_linage_page_counter", "arithmetic_x_list",
  "arithmetic_x", "record_name", "file_or_record_name", "table_name",
  "file_name_list", "file_name", "cd_name", "report_name",
  "mnemonic_name_list", "mnemonic_name", "procedure_name_list",
  "procedure_name", "label", "integer_label", "reference_list",
  "reference", "single_reference", "optional_reference_list",
  "optional_reference", "reference_or_literal", "undefined_word",
  "unique_word", "target_x_list", "target_x", "_x_list", "x_list", "x",
  "call_x", "x_common", "report_x_list", "expr_x", "arith_x",
  "prog_or_entry", "alnum_or_id", "simple_display_value",
  "simple_display_all_value", "simple_value", "simple_all_value",
  "id_or_lit", "id_or_lit_or_func", "id_or_lit_or_length_or_func",
  "num_id_or_lit", "positive_id_or_lit", "pos_num_id_or_lit",
  "from_parameter", "sub_identifier", "table_identifier",
  "sub_identifier_1", "display_identifier", "numeric_identifier",
  "identifier_or_file_name", "identifier", "identifier_1",
  "identifier_list", "target_identifier", "target_identifier_1",
  "qualified_word", "subref", "refmod", "integer", "symbolic_integer",
  "report_integer", "class_value", "literal", "basic_literal",
  "basic_value", "function", "func_no_parm", "func_one_parm",
  "func_multi_parm", "func_refmod", "func_args", "trim_args", "length_arg",
  "$@109", "numvalc_args", "locale_dt_args", "formatted_datetime_args",
  "formatted_time_args", "not_const_word", "flag_all", "flag_duplicates",
  "flag_initialized", "flag_initialized_to", "to_init_val", "_flag_next",
  "_flag_not", "flag_optional", "flag_rounded", "round_mode",
  "round_choice", "flag_separate", "_from_idx_to_idx", "_dest_index",
  "error_stmt_recover", "verb", "scope_terminator", "_advancing", "_after",
  "_are", "_area", "_areas", "_as", "_at", "_before", "_binary", "_box",
  "_by", "_character", "_characters", "_contains", "_controls", "_data",
  "_end_of", "_file", "_final", "_for", "_from", "_in", "_in_order",
  "_index", "_indicate", "_initial", "_into", "_is", "_is_equal",
  "_is_are", "_is_in", "_key", "_left_or_right", "_line", "_line_or_lines",
  "_limits", "_lines", "_message", "_mode", "_number", "_numbers", "_of",
  "_on", "_onoff_status", "_other", "_procedure", "_program", "_record",
  "_records", "_right", "_sign", "_signed", "_sign_is", "_size",
  "_standard", "_status", "_symbolic", "_tape", "_terminal", "_then",
  "_times", "_to", "_to_using", "_when", "_when_set_to", "_with",
  "coll_sequence", "column_or_col", "columns_or_cols", "comp_equal",
  "exception_or_error", "in_of", "label_option", "line_or_lines",
  "lock_records", "object_char_or_word_or_modules", "records",
  "reel_or_unit", "scroll_line_or_lines", "size_or_length", "with_dups",
  "prog_coll_sequence", "detail_keyword", "ch_keyword", "cf_keyword",
  "ph_keyword", "pf_keyword", "rh_keyword", "rf_keyword",
  "control_keyword", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,   344,
     345,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   360,   361,   362,   363,   364,
     365,   366,   367,   368,   369,   370,   371,   372,   373,   374,
     375,   376,   377,   378,   379,   380,   381,   382,   383,   384,
     385,   386,   387,   388,   389,   390,   391,   392,   393,   394,
     395,   396,   397,   398,   399,   400,   401,   402,   403,   404,
     405,   406,   407,   408,   409,   410,   411,   412,   413,   414,
     415,   416,   417,   418,   419,   420,   421,   422,   423,   424,
     425,   426,   427,   428,   429,   430,   431,   432,   433,   434,
     435,   436,   437,   438,   439,   440,   441,   442,   443,   444,
     445,   446,   447,   448,   449,   450,   451,   452,   453,   454,
     455,   456,   457,   458,   459,   460,   461,   462,   463,   464,
     465,   466,   467,   468,   469,   470,   471,   472,   473,   474,
     475,   476,   477,   478,   479,   480,   481,   482,   483,   484,
     485,   486,   487,   488,   489,   490,   491,   492,   493,   494,
     495,   496,   497,   498,   499,   500,   501,   502,   503,   504,
     505,   506,   507,   508,   509,   510,   511,   512,   513,   514,
     515,   516,   517,   518,   519,   520,   521,   522,   523,   524,
     525,   526,   527,   528,   529,   530,   531,   532,   533,   534,
     535,   536,   537,   538,   539,   540,   541,   542,   543,   544,
     545,   546,   547,   548,   549,   550,   551,   552,   553,   554,
     555,   556,   557,   558,   559,   560,   561,   562,   563,   564,
     565,   566,   567,   568,   569,   570,   571,   572,   573,   574,
     575,   576,   577,   578,   579,   580,   581,   582,   583,   584,
     585,   586,   587,   588,   589,   590,   591,   592,   593,   594,
     595,   596,   597,   598,   599,   600,   601,   602,   603,   604,
     605,   606,   607,   608,   609,   610,   611,   612,   613,   614,
     615,   616,   617,   618,   619,   620,   621,   622,   623,   624,
     625,   626,   627,   628,   629,   630,   631,   632,   633,   634,
     635,   636,   637,   638,   639,   640,   641,   642,   643,   644,
     645,   646,   647,   648,   649,   650,   651,   652,   653,   654,
     655,   656,   657,   658,   659,   660,   661,   662,   663,   664,
     665,   666,   667,   668,   669,   670,   671,   672,   673,   674,
     675,   676,   677,   678,   679,   680,   681,   682,   683,   684,
     685,   686,   687,   688,   689,   690,   691,   692,   693,   694,
     695,   696,   697,   698,   699,   700,   701,   702,   703,   704,
     705,   706,   707,   708,   709,   710,   711,   712,   713,   714,
     715,   716,   717,   718,   719,   720,   721,   722,   723,   724,
     725,   726,   727,   728,   729,   730,   731,   732,   733,   734,
     735,   736,   737,   738,   739,   740,   741,   742,   743,   744,
     745,   746,   747,   748,   749,   750,   751,   752,   753,   754,
     755,   756,   757,   758,   759,   760,   761,   762,   763,   764,
     765,   766,   767,   768,   769,   770,   771,   772,   773,   774,
     775,   776,   777,   778,   779,   780,   781,   782,   783,   784,
     785,   786,   787,   788,   789,   790,   791,   792,   793,   794,
     795,   796,   797,   798,   799,   800,   801,   802,   803,   804,
     805,   806,   807,   808,   809,   810,   811,   812,   813,   814,
     815,   816,   817,   818,   819,   820,   821,   822,   823,   824,
     825,   826,   827,   828,   829,   830,   831,   832,   833,   834,
     835,   836,   837,   838,   839,   840,   841,   842,   843,   844,
     845,   846,   847,   848,   849,   850,   851,   852,   853,   854,
     855,   856,   857,   858
};
# endif

#define YYPACT_NINF -3001

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-3001)))

#define YYTABLE_NINF -2334

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
   -3001,  1013,  1122, -3001, -3001, -3001,  1328, -3001,   163, -3001,
   -3001,  1134, -3001, -3001, -3001,     1,   953,  1024, -3001,  1019,
    1086, -3001, -3001, -3001,   823,   823,   701,   756,  1074,  1376,
     781,   920,  1182,  1796,   871,   917,   924,   163,   163, -3001,
   -3001,  1082,  1485, -3001, -3001,  1222, -3001,  1153,  1236, -3001,
    1507,    90,    90,  1253,  1311,  1535,  1535,  1535,    90,  1334,
    1282,  1298,  1535,  1322,  1329,  1774, -3001, -3001, -3001, -3001,
    1928,  1495,  1398,  1761,  1095,  2055,  2055,  1216, -3001, -3001,
   -3001, -3001,  1635, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001,  1017,  1017,  1777,  1729,  1744, -3001,   972,  3618,
    5153,  1382,   793, -3001,  1383,  1388, -3001, -3001, -3001, -3001,
    1535,  1535, -3001,  1535, -3001,  1333,  1850,  1333,  1535,  1535,
   -3001, -3001,  1333, -3001, -3001, -3001,  1342,  1362,  1820,  1510,
   -3001, -3001,  1820,   -93,  1510, -3001,   -93, -3001,  2055,  2070,
   -3001, -3001,  1343, -3001, -3001, -3001,  1918,  1918,  1501, -3001,
    1773,  1102, -3001,  1729, -3001,  1102, -3001, -3001, -3001, -3001,
   -3001,    34,  5509, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001,   613, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001,  1504, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
    1503, -3001, -3001,  1582, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
    1391, -3001,   611,    92, -3001, -3001,   735,  1535,  1280,  1333,
    1681,   755, -3001, -3001, -3001, -3001,  1691,  1448,   342,    15,
   -3001,  1397, -3001,  1342, -3001,    77, -3001, -3001, -3001, -3001,
   -3001, -3001,  1368,   655,  1535,    84, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001,  1701,  1474, -3001,
    1694,  1535,  1758, -3001, -3001,  1497, -3001,  1512, -3001, -3001,
    1598, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001,   487,  6405, 10158,   551,   972,
     472,  1183,   312,  -241,    89,  1160,  3075,  7273,  1160,   972,
    1073,  1080,   312,  1333,  1508, -3001, -3001,  7273, -3001, -3001,
     312,  1447,  2233,  1333,  6744,  7273, -3001,  1352,   -42,  1450,
    1455,  1450,  1333,  1455,  -219,    98,  1450,   308,  1333,  1455,
   -3001, -3001, -3001, -3001,  1333, -3001, -3001, -3001, -3001, -3001,
   -3001,  1505,  7273,  6545, -3001, -3001,  1447,   109,  1333,  1455,
    5670,  -219,  1593,  1969, -3001,   783,  1523, -3001, -3001,  1530,
     680,   695, -3001,   578, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001,  1280, -3001,  1539, -3001,  -172, -3001, -3001,  1333, -3001,
    1599, -3001,  1600,  1589,  1997,  1535, -3001, -3001, -3001,   898,
   -3001, -3001, -3001, -3001, -3001,   784,  2004,  1535,   105,  1482,
   -3001,   183, -3001, -3001,    42, -3001, -3001, -3001, -3001,  1785,
     655, -3001,  1814,    90,    90, -3001,  1368, -3001, -3001,   -63,
   -3001,  1535,   226,  1636,  1552, -3001, -3001,    49,    49,   630,
    1554, -3001,  1083,  1936, -3001,  1778,  1867,  1742,  1347, -3001,
    1333, -3001, -3001,  1556,  1565,  1569, -3001,  1571,  8762,   755,
     755, -3001,  1578,  1579,  1580, -3001, -3001, -3001,  1581,   755,
   -3001, -3001, -3001, -3001, -3001,  1333, -3001,  1584, -3001,  1569,
   -3001, -3001,  1946, -3001,  6793, -3001, -3001, -3001, -3001,  1601,
   -3001, -3001,  1586,  1587,  1588,  8762, 10187, 10158, 10187, -3001,
     160,  1087, -3001,  1941, -3001, -3001, -3001,   258,  1601, -3001,
   -3001,   551, -3001,  1608, -3001,   755, -3001,  1984,   -42, -3001,
   -3001,   472, -3001, -3001, -3001, -3001, -3001,  1455, -3001,  1092,
    1742,  1985,   208, -3001,  1692, -3001, -3001,  1505,  1601,  1455,
    1988,  1734,  2043, -3001, -3001,  1333,  1634,  1637, -3001, -3001,
   -3001,  1450,  1914, -3001,  1394,  2102, -3001, -3001, -3001, -3001,
   -3001,  2000,    62,  6879, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001,  1914,  5502,  1445,  1454,  1996,   168, -3001,  1869, -3001,
   -3001, -3001,  2008,    80, -3001, -3001, -3001,  4138, -3001, -3001,
    2056,   613, -3001, -3001, -3001,   312, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001,  1649, -3001, -3001,   287, -3001,  1447, -3001,
   -3001,    53, -3001, -3001, -3001, -3001, -3001, -3001,  1627,  7273,
   -3001,  1645,  2012,  2124, -3001, -3001, -3001, -3001,  1352, -3001,
    1706, -3001, -3001,  8060,  1661, -3001, -3001,  2020,   -34,  2021,
     708, -3001,  1960, -3001,  2022,  1734,  1595,  2024, -3001,  1960,
    1333,  2025,  1606, -3001, -3001,  1970,  8762,  2007, -3001, -3001,
   -3001, -3001, -3001, -3001,  1882, -3001,   312, -3001, -3001, -3001,
    1807,   705, -3001,   187,  2170, -3001,    85, -3001,  2031,  1232,
    6160, -3001, 10158,  1657, -3001,  2034,  7127, -3001,  2080,  1333,
    1333,  2037,  7214,  1447, -3001, -3001,  -169, -3001, -3001, -3001,
   -3001,  4016, -3001,  1986, -3001, -3001,  1458, -3001,  2038,  2084,
   -3001, -3001,  2039,  1960,  1683,  1760,  1917,  1628,  1628,  1628,
     671,  1687,  8227, -3001, -3001, -3001,  1640, -3001, -3001, -3001,
    1865, -3001,    90, -3001,  1106, -3001,   185, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001,  1167, -3001,    95, -3001,  1280, -3001, -3001,  1849,
   -3001, -3001, -3001, -3001,  1535,  1763,  1938, -3001, -3001, -3001,
   -3001,   901,  1535,  1642,  1971, -3001,  1918,   923,  1918,  1703,
   -3001, -3001,  1705,  2119, -3001,  1785, -3001,    90, -3001, -3001,
   -3001, -3001, -3001,  1709,   193,   910, -3001, -3001, -3001, -3001,
    1535, -3001, -3001,    78, -3001,   494,  -171,   291, -3001, -3001,
   -3001, -3001, -3001,  3379, -3001,  2146,  2621, -3001,   351, -3001,
    1727, 10158, 10158,  9510, -3001, -3001, -3001,  1601, -3001,  1667,
    1669, 10158, 10158, 10158,  8762,  1672,  1738,  8762, -3001, -3001,
   -3001,  7595,  2041, -3001,  1347, 10158, -3001,  8762, 10158, -3001,
    1601, -3001, -3001, -3001,  1300, -3001,  1998, 10158, 10158, 10158,
   10158, 10158, -3001,  1831, -3001,  1877,  1981, -3001, -3001,  5670,
   -3001,  1333,  1092, -3001, -3001, -3001,   721,   728,  1333, -3001,
   -3001, -3001, -3001, -3001, 10158,  1961, -3001,  1657, -3001,  1455,
   -3001, -3001, -3001, -3001,  1846, -3001, -3001, -3001, -3001, -3001,
   -3001,   492, -3001,  1695, -3001,  7273, -3001, -3001, -3001, -3001,
   -3001,  1943,  2095, -3001, -3001,  5502,   843,  1719,  1699,    62,
      62,    62,    62, -3001, -3001,  7273,  7595,  1711, -3001, -3001,
    1073,   201, -3001,  1707, -3001,  1712, -3001, -3001,  -128, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001,  5835, -3001, -3001,
   -3001,  1304, -3001, -3001, -3001,     4, -3001,  2161,  1604,  2120,
   -3001,  8762,   137, -3001, -3001,  1900, -3001, -3001,   108, 10158,
   -3001,  1798,   312, -3001, -3001,  7595, -3001,  1737,  1846,  1742,
   -3001,  1131,  1131,   505, -3001,  2088,  2088,  -244,  1808,  1809,
   -3001,   766, -3001, -3001,  1816, -3001, -3001, -3001, -3001, -3001,
    1734, -3001, -3001, -3001, -3001,  2071,  2233, -3001, -3001, -3001,
    2072, -3001, -3001, -3001,  1846,  2187, -3001, -3001,  1333,  2187,
    1333,  1737,   272,  1811, -3001, -3001,  1601, -3001,  1812, -3001,
   -3001,   364,  1815,  1368, -3001, -3001,  6319, -3001,  2293,   470,
     113, -3001, -3001, -3001,  1535, -3001,   650,  7273, -3001, -3001,
      71,    17,  1168, 10158, -3001, -3001, -3001,  1333, -3001,  2295,
   -3001,  2123,  2125, -3001, -3001,  7595, -3001, -3001, -3001, -3001,
    8762, -3001, -3001, -3001, -3001, -3001,  2295,  2085, -3001, -3001,
     303, -3001,  1819,  1897,  2061, -3001, -3001, -3001,  1935,  1826,
   -3001,  1827, -3001, -3001, -3001,  2263, -3001,  1730, -3001, -3001,
    1825, -3001, -3001, -3001,  2342,  1832, -3001, -3001, -3001, -3001,
    1167, -3001,  2068,  1938, -3001, -3001, -3001,   689, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,  2050, -3001,
   -3001, -3001,   540, -3001, -3001, -3001,  1973, -3001,  2288, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,   -94, -3001,
    1535,  1894,  2013, -3001, -3001, -3001,  2254,   594, -3001, -3001,
    1535,  1358,  3379, -3001, -3001, -3001,   896,  1853,  9418, -3001,
   -3001,  1358, -3001, -3001, -3001,  1779,  1787, -3001,  8762,  1358,
    2112,  1895,  2042, -3001, -3001, -3001,  2065, -3001, -3001, -3001,
   -3001, -3001, -3001,   588, -3001,  1333,   352,  1056,  1862,   356,
    1863, -3001,   365,   617,  8762, -3001, -3001,   240,  1866,  1871,
    1872,   389, -3001,  1601, -3001,  1874, -3001,  1333,   399,  1875,
    1742,   672, -3001,   -59,   574,   312, -3001,  1240,  1878,   430,
   -3001,  1870,  1831,  1087,  1087, -3001, -3001, -3001,   312, -3001,
    1888,   551, -3001, -3001,   627,  2370,   598, -3001, -3001,  1956,
   -3001,  1977, -3001,  1000,  1535, -3001, -3001, -3001,  1670,    20,
   -3001, -3001, -3001,  2143, -3001,  7273, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001,    38, -3001, -3001,  5028, -3001, -3001,  2794,
    1333, -3001, -3001, -3001, -3001, -3001, -3001,  2186,   672,  2189,
   -3001, -3001, -3001, -3001, -3001, -3001,  2405, -3001,  1901,   366,
   -3001, -3001,   201, -3001,  1823,  1304, -3001, -3001, -3001, -3001,
   -3001,  1508, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
    2069, -3001, -3001, -3001,  2266, -3001,  1508, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001,  2005,  1508, -3001,  1905, -3001,  2381,
   -3001, -3001, -3001,  8447, -3001,  8762,  1790, -3001, -3001, -3001,
    2297,   245,   244,    19,   312,   312,   672,  2203,   126,  1455,
    1131,  1906, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,  2188,
    8389,    -3,  2249,  1333,   551, -3001,   303,  2072,  1333, -3001,
   -3001, -3001, -3001,  1333,   798,   643, -3001,  1854, -3001,  1857,
   -3001,   303,    79,  8762,  2090,  1143,   592, -3001,   588,  2092,
   -3001, -3001, -3001,  7273,  1368,  1368,  1368,  1368,  1368,  1368,
    1368,  1368,   470, -3001,   772,    20,   747, -3001,  1957,  1957,
   -3001, -3001, -3001, 10158,  9611,  1168,  -155,  7244,  1333,  1333,
     672,  2216,  1938,  1923,  2445,  1333,   757, -3001, -3001,  1846,
    2449,   611, -3001,  1925,  2001,  2036,  1879,  1264,  1333, -3001,
   -3001, -3001,  1264,  2366,  1535,  1529,  1529,  1535,    23,  1694,
    1535,  2446, -3001,  2111, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001,    90,   809,  3509, -3001,  1954, -3001,
    2250, -3001,  1167, -3001, -3001,   111, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001,   909,  1535, -3001, -3001,  1881,  1937, -3001, -3001, -3001,
   -3001, -3001,  2423, -3001, -3001, -3001, -3001, -3001,  1559, -3001,
    1473, -3001, -3001, -3001, -3001,  2127,  2127, -3001, -3001,  2127,
     629, -3001,  1535, -3001, -3001, -3001, -3001, -3001,  1535, -3001,
   -3001,  1535, -3001, -3001, -3001, -3001, -3001,   162, -3001, -3001,
   -3001,  2412,  2002, -3001, -3001, -3001, -3001,   -31, -3001,  1535,
   -3001, -3001,  2473, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001,  1358, -3001, -3001, -3001, -3001,   142, -3001,
   -3001, -3001, -3001,  1692,  9775,  1586,  9936,  1586, -3001,  1950,
   -3001, -3001,  1333,  1586,  1586,  1586,  8762, -3001,  1692,   541,
    1586,   351, -3001, -3001, -3001,  2135,  2003,   -44,  2251,   672,
   10100,  1586,  1586,   348, -3001, -3001, -3001, -3001, -3001,  2088,
   -3001, -3001, -3001, -3001, -3001,  2152, -3001, -3001, -3001,   910,
   -3001, 10158, -3001, -3001, -3001, -3001,  2142,  2220,   673,  1719,
     719, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,  1535,
   -3001, -3001, -3001, -3001,   -38, -3001,  1535, -3001,    -1,  1535,
   -3001, -3001, -3001, -3001,    13,  1535, -3001, -3001, -3001, -3001,
   -3001,   848,   848,   312, -3001,   312,    73,   201, -3001, -3001,
   -3001,  2405, -3001, -3001, -3001,  1333, -3001, -3001, -3001,  2340,
    1907,  1438,   333,  1908,   348,  8762, -3001, -3001,  2437, -3001,
    1315, -3001, -3001,  1790, -3001,  1315,  2274,  2275, -3001,  2028,
   -3001, -3001,  1535, -3001, -3001,  2222,  2133, -3001, -3001,   312,
   -3001,   312,  2134,  2134,  2140, -3001,  1234, -3001, -3001, -3001,
    1333, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,  2498,
   -3001,  2280, -3001, -3001,   225,   711, -3001, -3001, -3001, -3001,
    2167,  2360,    20, -3001,  1144, -3001, -3001, -3001, -3001,  1857,
    2087, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001,  7273, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001,    18, -3001,  1333, -3001, -3001, -3001,  1346, -3001,
   -3001, -3001, 10158, -3001,  7273,  7273,  1056, -3001,  1307,  -114,
    2131, -3001, -3001, -3001,  1692,  1692, -3001,   312,  1983, -3001,
     348, -3001,  2150, -3001,  8762, -3001,  2392,  2009, -3001,   643,
   -3001,   792, -3001, -3001, -3001,  1987,  2066,  2073,   199, -3001,
    1942, -3001,  2296,  2014,  8824,   975,  2301, -3001,  1938,  1934,
    1535,  2446,  1948,   628,   684,  1938,  1944,  1535, -3001, -3001,
   -3001,   -62,  1439, -3001, -3001, -3001,  2006, -3001,  2401, -3001,
    2366,  1455,  2521, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001,   132, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001,   594,   594,   594, -3001, -3001, -3001, -3001,
     594,   594,   594, -3001, -3001,  1535,  1535,   592,   592,   592,
   -3001,   629, -3001,  1535,   230, -3001, -3001,   -87, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,  2316, -3001,
   -3001, -3001,  2310, -3001, -3001, -3001, -3001, -3001, -3001,  2311,
   -3001, -3001,  1327, -3001, -3001, -3001, -3001, -3001,  1711,  2409,
   -3001,  1025, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
     -60,   -60,   -60,   -60,  7273, -3001,   719, -3001,  4857,   594,
   -3001, -3001,   594, -3001, -3001, -3001, -3001,   661,  2395,   594,
     592,   592,   594, -3001,    82,  1938,  2342, -3001,  1535, -3001,
    8977, -3001, -3001, -3001, -3001, -3001, -3001,  4096,  8977,   672,
    2126,   672,  2128,  5991, -3001,  -108,    96, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001,  1438, -3001,  2433, -3001, -3001,  1508,
   -3001,  1315, -3001,  1315,   348,  2015,  2015, -3001,  2545,  2514,
   -3001, -3001, -3001, -3001,   181,  1333, -3001, -3001, -3001,   672,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001,  1293, -3001,  2393,
    1333,  7273,  2069,  2252,  2287, -3001,   990, -3001, -3001, -3001,
     906, -3001, -3001, -3001,  2448,  2229, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001,  2264, -3001, -3001, -3001,  2276, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001,  1056, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001,  2223,  2026,  1535,   -87,  2316,   672,
    1993, -3001,  2445, -3001,  2188,  2464,  2188,  -114,  1218, -3001,
   -3001,  1652,  2513,   611, -3001,  2045,  2117, -3001,  1196,  1535,
   -3001,  1333, -3001,    11, -3001, -3001,   485,   564,   773,   913,
     915,  1990, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001,   995, -3001,  2137, -3001,   857, -3001,
   -3001, -3001, -3001,  1333,  2307, -3001, -3001, -3001,   -57, -3001,
   -3001, -3001,  1535, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001,  1664,   553, -3001,  1991, -3001,  1196, -3001,  2052,
   -3001,  2364, -3001, -3001, -3001,  1948, -3001, -3001, -3001, -3001,
   -3001, -3001,  2291,    48,  2188,   938,  1535, -3001, -3001,  1535,
   -3001, -3001,  1694,  1734,  -207, -3001,  2121,  1535,  2522,   704,
     -70,   717,  1737, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001,  2097, -3001,  2290, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001,  2550,  1535,  1455,  1455,  1167, -3001, -3001,
     153, -3001, -3001, -3001, -3001, -3001, -3001,   673,   629, -3001,
    1665, -3001, -3001, -3001,   592, -3001, -3001,  2339,  2465, -3001,
     613, -3001, -3001, -3001,   848, -3001, -3001,  7273,  7273,  7273,
    7273, -3001, -3001, -3001, -3001, -3001, -3001,  1742, -3001, -3001,
   -3001, -3001, -3001, -3001,  1333, -3001,  1535,  7273, -3001,  -197,
   -3001, -3001,   312, -3001,   312, -3001, -3001,  7273, -3001, -3001,
   -3001, -3001, -3001, -3001,  2543,  2474, -3001, -3001,  1315, -3001,
    7273,  7273, -3001, -3001,  2205,  1455,   706, -3001,  1333, -3001,
   -3001,  2158, -3001, -3001, -3001,  2547,  2298, -3001,  1535,  1175,
   -3001, -3001,   724,  2299,  2302, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001,  1333, -3001,  2465, -3001, -3001, -3001,  2074,
   -3001,  1333,  2188, -3001,  1333, -3001, -3001, -3001, -3001, -3001,
    2265,  2427, -3001, -3001, -3001, -3001,    90, -3001,   611, -3001,
     611, -3001,  2076, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001,  2089, -3001,  1196, -3001,  1770, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001,  2011,  1189, -3001, -3001,  2556,  2057,  2086, -3001,
   -3001, -3001, -3001, -3001,  8622,  2591, -3001,  2295,  2151,  2151,
   -3001,  1196,  1938,   378,  1333, -3001, -3001, -3001, -3001,  1938,
   -3001,  1589, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
     685,   685,  1535,  2222, -3001, -3001,  1190, -3001,  1078,  1535,
    1535,  1535,  1535, -3001,  1513, -3001,   126,  1535,  1694, -3001,
    2154,  1934,  1455, -3001,  2234, -3001,   -13, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001,   592,  2339,   -87,   587,   107,  8977,
   -3001, -3001, -3001, -3001, -3001, -3001,  1333, -3001, -3001, -3001,
     -87,   -87, -3001, -3001, -3001, -3001, -3001,  7273, -3001, -3001,
   -3001, -3001,  1535,  1455,  1455,  2227, -3001, -3001, -3001,  7273,
   -3001,  1333, -3001, -3001,  2167,  2360, -3001, -3001, -3001, -3001,
     -87,   932, -3001, -3001,  1333, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001,   571,  -186, -3001,
   -3001,  1742, -3001, -3001,  1196, -3001, -3001,  1117,  2326, -3001,
   -3001, -3001, -3001, -3001,  2188,  2399,  2101,  1938,  2101, -3001,
    2335, -3001,   673,  2522, -3001, -3001, -3001, -3001, -3001, -3001,
    1333, -3001,    69,  1774,    67, -3001, -3001, -3001, -3001,    25,
    1535, -3001, -3001,  3117, -3001, -3001,   684,  2139,  1333,  1333,
   -3001, -3001, -3001, -3001,  1333,  1535, -3001, -3001, -3001,  1938,
   -3001,  2511,  2107,   592, -3001, -3001, -3001, -3001, -3001,  2303,
     200,  1742, -3001, -3001, -3001, -3001, -3001,  1333, -3001, -3001,
   -3001, -3001,   551,  1455,  1535,  2075, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001,  1490, -3001, -3001,
   -3001, -3001, -3001,  2241,  2519, -3001, -3001,  2236,  -199, -3001,
    2171, -3001,  2105,  1333, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001,  2053,  1938,  2122, -3001,  2523, -3001,
    2525, -3001, -3001, -3001, -3001,   684,   684, -3001, -3001, -3001,
   -3001,  2431, -3001, -3001,  2052,  1938, -3001, -3001, -3001, -3001,
    1333, -3001, -3001, -3001, -3001, -3001,   693, -3001, -3001,   693,
    2649, -3001, -3001, -3001, -3001, -3001, -3001, -3001,   693,   693,
     693,   694, -3001, -3001, -3001,  -154, -3001, -3001,   874, -3001,
    2129,   592, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
    1333, -3001, -3001,   551, -3001,  2231,  2168,   274,  2069,   -22,
    1447, -3001,  7972, -3001, -3001,  -199,  2136,  2132,  1535, -3001,
   -3001, -3001,  1938,  2402,  2069,  2069,   202, -3001, -3001,  2637,
    1774, -3001,    14, -3001, -3001, -3001, -3001,  1825, -3001, -3001,
   -3001, -3001, -3001, -3001,  1535,  1333,  2078, -3001,  2078, -3001,
   -3001, -3001, -3001, -3001, -3001,  1333, -3001,  1431, -3001, -3001,
   -3001,    86, -3001, -3001,   501, -3001,  2144, -3001,  2145, -3001,
   -3001,   571, -3001,   202,  1333,  1333,  2295, -3001,  2411, -3001,
     244,  2222,   257,   684,  2540,  2166, -3001, -3001,  1333,  1333,
     -45, -3001, -3001, -3001, -3001, -3001,  2282,  1254,    86, -3001,
   -3001,   652,  1292,   223, -3001, -3001, -3001, -3001, -3001,   202,
   -3001,  2091, -3001, -3001,  1535,   244, -3001,  2069, -3001,  2093,
   -3001,  1333,  2321, -3001, -3001,  2069, -3001, -3001,  2325,  1333,
   -3001,    12,  2415,  2416,  2553,  2408, -3001,   652, -3001,  1724,
     764,  2159,   669,  9392, -3001, -3001,  2091, -3001,  1333, -3001,
    1333,    99,  1098,   -54, -3001, -3001,  1535,  2330,  1333,  1535,
    1535,  1535,  1535, -3001,  2420,    30,  2421, -3001,  2414, -3001,
    1294, -3001, -3001,  1333,  2593,  1314,  2425,   195,  2432,  2422,
   -3001,   976, -3001, -3001,  1333,  2192, -3001,  1535,  1535,  2446,
      45, -3001, -3001, -3001, -3001,  2475,  2505, -3001,  1535, -3001,
   -3001, -3001, -3001,   581, -3001,  1535,    -1,  1535,  2233, -3001,
   -3001, -3001, -3001,  2127, -3001,  2565,  1938, -3001,  2647, -3001,
   -3001,  1535, -3001, -3001,  1333, -3001, -3001,  1333, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,  2479,
    2127, -3001,  1333, -3001,  1347, -3001, -3001, -3001,  1563, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001,  1455,  1333,  1938,
   -3001,  1333,  1333,  1333,  1333,  1535,  1535,  1535,  1535,  1535,
   -3001,  1333,  1535,  1535,  1535,  1535,  1535,  1535,  1535,  1535,
    1535,  1535,  1535, -3001,  1333,  1535,   673, -3001, -3001,  1535,
    2446,  2418,  2130, -3001, -3001, -3001,  1333,   594, -3001, -3001,
     594, -3001, -3001,   594, -3001, -3001,  1535,  2101,  1535,  1938,
   -3001, -3001, -3001, -3001,  1535, -3001, -3001, -3001,  2101, -3001,
   -3001, -3001, -3001,  1333,  1333,  1333,  1333,  1333,  1333,  1333,
    1333,  1333,  1333,  1333,  1333,  1333,  1333,  1333,  1333,  1333,
    1333,  1333, -3001, -3001, -3001, -3001,  1690,   -48, -3001,  1333,
   -3001, -3001, -3001,  1033, -3001,   673, -3001,  1033,  2411, -3001,
   -3001, -3001, -3001, -3001,  1333, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001,  1333, -3001,  1452, -3001,
   -3001,  2418, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
     594, -3001, -3001, -3001,   594, -3001, -3001,  1333,  1333,   904,
    1535,  1535,  1653, -3001, -3001, -3001, -3001, -3001, -3001,  1931,
   -3001, -3001, -3001,  1333, -3001, -3001, -3001,  1535,  2418,  2418,
   -3001,  2472,  1535,  1535, -3001,  2332,  1333,  2418, -3001, -3001,
   -3001,  2418,  2418,  2460,  1295,  2446,  2480,  1938,  2138,  1535,
    1742, -3001,  1535,  1535,  1333, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,  1316, -3001,
     -14, -3001, -3001, -3001,  1295,  2446,  1333, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001,   199, -3001,  1535,  2122, -3001, 10498,
   10498,  1603,  2584,  2496, -3001,  1938,  1316, -3001, -3001,  1938,
     -14, -3001, -3001,   199, -3001,  1333, -3001,  1316,  2101, -3001,
    1692, 10321, -3001, -3001,   114,   794, -3001, -3001,  1158, -3001,
   -3001, -3001, -3001,   -47,   -47, -3001, -3001, -3001, -3001, -3001,
   10498, -3001, -3001, -3001, -3001, -3001,  1333, -3001,  2525, -3001,
    2069, -3001, -3001, -3001, -3001, -3001, -3001, -3001,  2361, -3001,
    2361, -3001,  2671, -3001,  2235,   146,  2354, -3001, -3001, 10498,
    1938, -3001, -3001, -3001, -3001, -3001, -3001, -3001
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       2,     0,    12,     1,     3,     5,    23,     4,    65,    26,
      25,    23,     8,    10,    11,     0,     0,     0,    13,   339,
      81,     9,    30,    27,    48,    48,     0,     0,     0,   851,
     341,     0,   218,    68,     0,     0,     0,    65,    65,    24,
      66,     0,     0,    22,   897,     0,   343,     0,     0,    64,
     220,     0,     0,     0,  2301,  2245,  2245,  2245,     0,     0,
       0,     0,  2245,     0,     0,  2214,   156,    67,    69,    70,
      73,    85,    86,    75,   117,   138,   136,     0,   142,   144,
     145,   146,   192,   148,   147,   149,   150,   151,   152,   153,
     154,   155,     0,     0,    51,    16,     0,   340,  1076,     0,
       0,     0,   337,    82,     0,     0,   224,  1927,  1926,   167,
    2245,  2245,  2302,  2245,  2246,     0,     0,     0,  2245,  2245,
      95,   119,     0,    89,   137,  2215,     0,  2245,    72,   117,
      87,    88,   133,    83,   117,    74,   118,   135,   139,     0,
     140,   143,     0,   191,    33,    32,    36,    36,     0,    49,
      53,     0,    14,    17,    18,     0,    15,  1080,  1077,  1078,
    1079,   853,     0,   957,  1049,  1059,  1065,  1072,  1119,  1125,
    1145,  1140,  1146,  1151,  1147,  1159,  1169,  1270,  1279,  1281,
    1284,  1310,  1321,  1324,  1327,  1319,  1333,  1344,  1366,  1370,
    1409,  1411,  1415,     0,  1421,  1436,  1460,  1462,  1492,  1493,
    1509,  1512,  1513,  1518,  1527,  1528,  1541,  1554,  1593,  1611,
       0,  1648,  1662,  1671,  1673,   879,  1677,  1680,  1683,  1734,
     899,   900,   901,   902,   903,   904,   905,   906,   908,   907,
     909,   911,   910,   912,   913,   914,   915,   916,   917,   918,
     919,   920,   921,   922,   923,   924,   925,   926,   927,   928,
     929,   930,   931,   932,   933,   934,   935,   936,   937,   938,
     939,   940,   941,   942,   943,   944,   945,   946,   947,   948,
     949,   950,   951,   952,   953,   954,   898,   342,   349,   350,
     463,   344,   466,     0,   219,   221,   222,  2245,     0,     0,
       0,  2011,   213,  1919,   211,   216,     0,     0,    97,   121,
     215,    91,  1928,   194,   195,  2250,   198,  2016,  1565,  1564,
     157,   161,   164,  2284,  2245,     0,    76,    71,   118,    77,
     134,    78,    84,    79,    80,   141,   193,     0,     0,    28,
    2273,  2245,    58,    35,    34,     0,    19,     0,   856,   854,
     873,  2120,  2121,  2122,  2123,  2124,  2125,  2126,  2127,  2128,
    2129,  2130,  2131,  2132,  2118,  2168,  2169,  2170,  2171,  2172,
    2173,  2174,  2175,  2176,  2177,  2178,  2179,  2180,  2181,  2182,
    2183,  2184,  2185,  2186,  2187,  2188,  2133,  2134,  2135,  2136,
    2137,  2138,  2139,  2140,  2141,  2142,  2143,  2144,  2145,  2146,
    2147,  2148,  2149,  2150,  2151,  2152,  2153,  2154,  2155,  2156,
    2157,  2158,  2159,  2160,  2161,  2162,  2163,  2116,  2164,  2165,
    2166,  2167,   956,  2117,  2119,     0,     0,     0,     0,  1076,
       0,     0,     0,     0,     0,  1164,     0,     0,  1164,  1076,
    1828,  1312,     0,     0,  2324,  1104,  1103,     0,  1332,  1828,
       0,     0,     0,     0,     0,     0,   955,     0,  1818,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1644,  1647,  1631,  1645,  1823,  1646,  1633,  1642,  1634,  1643,
    1999,  2003,     0,     0,  1672,  1670,     0,   897,     0,     0,
       0,     0,     0,   410,   345,  2079,     0,  1904,   346,     0,
    2095,   318,   225,     0,  2023,  2019,  2024,  2022,  2020,  2025,
    2021,   202,   203,   205,   214,   209,  2345,  2346,     0,   207,
       0,  2213,     0,  2308,     0,  2245,  2368,   115,    96,  2212,
     101,   103,   104,   105,   106,  2212,     0,  2245,     0,     0,
     120,     0,   124,    90,    93,   196,  2252,  2251,   199,     0,
    2284,  2287,  2286,     0,     0,   158,   162,    37,    31,  2245,
    2274,  2245,     0,     0,     0,    20,    21,   861,   861,     0,
       0,   983,  2011,  1047,   959,  2271,   982,  2280,     0,  2056,
       0,  2051,  2057,     0,     0,  2063,  2034,     0,     0,  1889,
    1891,  2030,     0,     0,     0,  2054,  2035,  1950,     0,  1893,
    2033,  2055,  2031,  2058,  2059,     0,  2036,     0,  2053,  2063,
    2052,  2032,  1057,  1944,  1055,  1936,  1939,  1938,  1942,  2026,
    2028,  1943,  2060,     0,     0,     0,     0,     0,     0,  1060,
       0,  1878,  1881,  1883,  1886,  1959,  1888,  2084,  1957,  1958,
    1916,  1066,  1067,     0,  1912,  1914,  1913,  1117,  1818,  1975,
    1124,  1120,  1121,  1123,  1974,  1136,  1126,  1127,  1128,  1131,
    2280,  1143,     0,  1895,  2098,  1931,  2006,  2010,  1932,     0,
    1157,  2294,  2218,  1148,  2004,  1150,  2318,     0,  1166,  1168,
    1160,     0,  1225,  1224,  2030,  2208,  1187,  1223,  1216,  1222,
    1215,  1268,  1763,  1934,  1182,  1184,  1176,  1177,  1212,  1178,
    1179,  1225,  1181,     0,  1938,  1277,     0,  1280,     0,  1282,
    1291,  1290,  1308,     0,  1287,  1289,  1827,  2245,  1314,  1318,
    1316,  1319,  1317,  1311,  1322,  1323,  1929,  1325,  1326,  2325,
    1328,  1910,  1320,  1338,  1826,  1345,  1347,  1906,  1367,  1368,
    1371,     0,  1373,  1374,  1375,  1410,  1597,  1993,  1994,     0,
    1412,     0,  1419,     0,  1429,  1426,  1428,  1427,  1422,  1423,
    1430,  2233,  1437,  1448,     0,  1905,  1461,  1490,  2090,  1507,
       0,  1510,  1737,  1898,  1516,  2294,     0,  1525,  1899,  1737,
       0,  1539,  1532,  1901,  1542,  1545,     0,     0,  1909,  1555,
    1556,  1557,  1558,  1559,  1560,  1584,  1561,  1587,  1562,  1563,
       0,     0,  1907,     0,     0,  1992,  2010,  1594,  1629,  1616,
    1635,  1822,     0,  2001,  2002,  1660,     0,  1651,  1654,     0,
       0,  1668,     0,  1674,  1675,   885,   891,   880,   881,   882,
     884,     0,  1678,     0,  1996,  1681,  2296,  1977,  1700,  1686,
    1976,  1978,  1748,  1737,     0,     0,   674,     0,     0,     0,
     468,     0,     0,   472,   473,   471,     0,   348,   351,   223,
       0,  2096,     0,   330,   326,   217,     0,   321,   323,   324,
     173,   172,   187,   183,   188,   169,   186,   184,   170,   171,
     185,   168,   174,   175,   177,   204,     0,  2334,   208,     0,
    2012,   212,  2367,  2309,  2245,     0,     0,   100,   102,    98,
     116,  2212,  2245,     0,     0,   131,    36,     0,    36,     0,
     122,   125,     0,     0,  2017,   197,   200,     0,  2285,   165,
     159,   160,   163,     0,     0,     0,    55,    57,    56,    54,
    2245,    50,  2211,   861,   858,   864,     0,   861,   874,   875,
     848,  1048,   958,   984,  2272,     0,     0,  2281,     0,  2027,
       0,     0,     0,     0,  2049,  2069,  1945,  1946,  1947,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  2050,  1058,
    1050,     0,     0,  1937,     0,     0,  2037,     0,     0,  1960,
    1961,  1962,  1885,  1956,     0,  1884,  2086,     0,     0,     0,
       0,     0,  2085,  1063,  1068,  1070,     0,  1118,  1073,  1083,
    1122,     0,  1131,  2358,  2359,  1129,     0,  1132,     0,  1144,
    1141,  2342,  2341,  1896,     0,  2100,  1897,  2008,  2009,  1154,
    1155,  1158,  1152,  2295,  1475,  2219,  1149,  2005,  2319,  1165,
    1167,  1162,  1226,     0,  2209,     0,  1269,  1170,   894,   894,
    1175,  1769,  1766,  1183,  1180,  1935,  2333,  1227,     0,  1763,
    1763,  1763,  1763,  1278,  1271,     0,     0,  1087,  1309,  1285,
    1828,  1828,  1286,  1293,  1294,   894,  1844,  1842,  2246,  1848,
    1845,  1837,  1841,  1839,  1840,  1836,  1838,  1829,  1830,  1843,
    1832,     0,  1315,  1313,  1930,  1330,  1339,  1340,  1349,     0,
    1369,     0,  1396,  1380,  1372,  1377,  1378,  1379,  1601,     0,
    1995,     0,     0,  1420,  1416,     0,  1424,  2333,  1475,  2280,
    1450,     0,     0,  1916,  1980,  1820,  1820,     0,  1446,     0,
    1979,  1913,   485,  1981,     0,  1819,  1491,  1463,  2091,  2092,
    2294,  1508,  1494,  1496,  1497,     0,     0,  1511,  1517,  1514,
    1465,  1900,  1526,  1519,  1475,  1534,  1540,  1529,     0,  1534,
       0,  2333,  1543,     0,  1967,  1969,  1970,  1971,     0,  1585,
    1588,     0,     0,     0,  1908,  1567,     0,  1566,     0,     0,
    2008,  1630,  1612,  1618,  2245,  1619,  1614,     0,  1632,  1637,
       0,  1873,  1871,     0,  2000,  1661,  1649,     0,  1652,  2210,
    1653,     0,     0,  1669,  1663,     0,  1676,   886,   890,   883,
       0,  2297,  2298,  1682,  1701,  1684,  2210,     0,  1749,  1735,
    1739,   464,     0,     0,   677,   482,   514,   517,     0,     0,
     469,     0,   479,   480,   474,   481,   477,  2245,  2097,   226,
    2224,   327,   328,   329,  2195,     0,   319,   322,   176,   179,
       0,   206,     0,     0,  2366,   109,    99,     0,  1920,   108,
     126,   127,   130,   132,   128,   129,   123,    92,     0,   201,
     166,    29,    40,    43,    47,    46,  2292,    41,    42,  2102,
    2103,  2104,  2105,  2106,  2107,  2108,  2109,    52,     0,   859,
    2245,     0,   871,   869,   862,   863,   876,  2203,  2205,   989,
    2245,  1750,   985,   986,   988,   990,     0,     0,     0,   981,
     977,  1750,  2340,  2339,   974,   966,   968,   969,     0,  1750,
       0,     0,     0,   997,   961,   972,     0,   980,   963,   979,
     964,  1964,  1963,     0,  1949,     0,  1873,  1871,     0,  1873,
       0,  2065,  1873,     0,     0,  1890,  1892,  1873,     0,     0,
       0,  1873,  1953,  1954,  1955,     0,  1894,     0,  1873,     0,
    2280,  1772,  1056,  2010,  1932,     0,  2029,     0,     0,  1873,
    1887,  2088,  1063,  1877,  1876,  1880,  1879,  1882,     0,  1061,
       0,     0,  1915,  1074,     0,  1081,  1138,  1130,  1135,     0,
    2229,     0,  1933,  1772,  2245,  2099,  2007,  1156,  2228,  1808,
    1476,  1477,  1161,     0,  1217,  1201,  1768,   895,  1771,  1764,
    1770,  1765,  1767,     0,  1193,  1192,  1185,  1188,  1190,     0,
       0,  1213,  1220,  1173,  1174,  1171,  1172,     0,  1772,     0,
    1088,  1283,  1288,  1303,  1305,  1304,  1298,  1300,  1306,  1828,
    1295,  1292,  1828,  1296,     0,  1847,  1831,  1858,  1859,  1860,
    1849,  2324,  1866,  1869,  1868,  1870,  1862,  1855,  1857,  1856,
    1861,  1863,  1865,  1867,  1833,  1850,  1851,  1852,  1853,  1854,
    2282,  1329,  1911,  1341,  1342,   894,  2324,  1357,  1358,  1360,
    1362,  1363,  1359,  1361,  1352,  2324,  1348,     0,  1397,     0,
    1399,  1398,  1400,  1382,  1392,     0,     0,  1376,  2365,  2283,
       0,  1603,     0,  2234,     0,  1413,  1772,     0,     0,     0,
       0,     0,   494,   490,   493,   492,   491,   607,   609,   506,
     502,   504,   505,   507,   503,   508,   610,   608,   509,   510,
     487,   498,   499,   500,   495,   496,   497,   489,   486,  2232,
    1448,  1439,  1828,     0,     0,  1449,     0,  1465,     0,  1738,
    1989,  1990,  1991,     0,     0,  1521,   894,     0,  1533,     0,
    1547,  1739,     0,     0,     0,     0,     0,  1586,     0,     0,
    1590,  1589,  1581,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1569,  1570,  2093,  1808,     0,  1636,  2312,  2312,
    1874,  1875,  2013,     0,     0,     0,  1658,     0,     0,     0,
    1772,     0,   892,     0,  2080,     0,  1658,  1744,  1743,  1475,
    2189,   466,   411,     0,     0,   680,     0,   557,     0,   470,
     476,   536,   483,  2216,  2245,     0,     0,  2245,  2216,  2273,
    2245,  2193,   347,     0,   352,   355,   356,   357,   358,   359,
     360,   361,   362,   363,     0,     0,     0,  2225,  2316,  2196,
    2228,   320,     0,   178,   210,     0,   112,   114,   113,   110,
     111,    94,    45,  2293,    39,    44,    60,    61,    62,    63,
      59,     0,  2245,   868,   872,     0,     0,   849,  1984,  1001,
    1982,  1983,     0,  1756,  1757,  1761,  1762,   960,  1758,   894,
    1753,   894,   987,  2338,  2337,  2275,  2275,   999,  1000,  2275,
       0,  1006,  2245,  1017,  1018,  1019,  1008,  1009,  2245,  1010,
    1046,  2245,  1011,  1014,  1012,  1013,  1015,     0,  1040,  1041,
    1021,  1023,     0,  1024,  1044,  1042,  1043,     0,  1026,  2245,
    1016,  1007,  2191,  1029,  1045,  1032,   991,  1020,  1025,  1031,
     978,   965,   967,  1750,   975,   970,   971,   998,  2202,   973,
    1966,  1948,  1965,  2098,     0,  2060,     0,  2060,  2064,     0,
    2041,  2070,     0,  2060,  2060,  2060,     0,  2043,  2098,     0,
    2060,     0,   894,   894,  1051,  1778,  1775,  2008,  2009,  1772,
       0,  2060,  2060,     0,  2087,  1062,  1064,  1071,  1069,  1820,
    1085,  1086,  1082,  1084,  1137,     0,  1134,  1133,  1142,     0,
    1480,     0,   894,   894,  1153,  1809,  1815,  1812,     0,  1227,
    1203,   897,  1199,  1200,  1197,  1196,  1198,  1195,  1189,  2245,
    1256,  1257,  1258,  1246,     0,  1249,  2245,  1250,  2222,  2245,
    1253,  1254,  1191,  1259,     0,  2245,  1255,  1262,  1260,  1194,
    1228,  2333,  2333,     0,  1272,     0,  1094,  1828,  1828,  1302,
     894,  1299,  1835,  1834,  1864,     0,   894,  1343,  1334,  1337,
       0,     0,  1364,     0,     0,     0,  1393,  1395,     0,  1388,
    1402,  1389,  1390,  1381,  1384,  1402,     0,  1969,  2364,     0,
    2336,  1595,  2245,   650,   651,  2256,     0,  2235,  1602,  1414,
    1417,     0,  2288,  2288,     0,  1431,  1432,  1902,   488,   501,
       0,  1438,  1444,  1445,   894,  1441,  1455,  1451,  1456,  1452,
    1457,     0,  1447,  1454,  1467,  1498,  1466,   894,   894,  1515,
    1797,     0,  1808,  1522,     0,  1535,  1828,  1828,  1530,  1536,
    1552,  1551,  1550,  1549,  1548,  1568,  1591,  1592,  1987,  1988,
    1580,     0,  1583,  1572,  1573,  1574,  1578,  1575,  1579,  1576,
    1577,  1571,  2094,  1628,     0,  1625,  1626,  1620,     0,  1613,
    2363,  2362,     0,  2313,  1640,  1640,  1872,  2014,     0,  1781,
       0,  1656,  1655,  1657,  2098,  2098,  1664,     0,     0,   893,
       0,  2081,  1687,  1688,     0,  1691,  1694,  1698,  1692,  1521,
    2190,     0,   465,   413,   675,     0,     0,   773,  2250,   515,
       0,   558,     0,   512,  2245,  2200,     0,  2217,     0,     0,
    2245,  2193,     0,     0,     0,     0,     0,  2245,   406,  2194,
     407,     0,     0,   408,   353,   354,     0,   227,  2294,  2317,
    2216,     0,   180,   181,  2353,  2355,  2354,   107,   865,   866,
     870,     0,   860,   877,   879,  1005,  1751,  1759,  1755,  1752,
    1754,  1760,  2276,     0,     0,     0,  2018,   992,  1985,  1986,
       0,     0,     0,  1039,  1030,  2245,  2245,  1824,  1824,     0,
    2192,     0,   976,  2245,   962,   993,   995,  1772,  2076,  2047,
    2078,  2048,  2042,  2074,  2044,  2045,  2046,  2072,  2112,  2067,
    2068,  2040,  1933,  1780,  1777,  1773,  1779,  1774,  1776,  2007,
    1052,  2061,     0,  2038,  2039,  2089,  1972,  1973,  1087,     0,
    2101,     0,  1814,  1817,  1810,  1816,  1811,  1813,  1163,  1218,
    2247,  2247,  2247,  2247,     0,  1202,  1204,  1205,     0,     0,
    1247,  1248,     0,   829,   831,  1251,  1252,     0,     0,     0,
    1824,  1824,     0,  1241,  2253,     0,  2195,  1240,  2245,  1233,
    1214,  1229,  1236,  1235,  1231,  1242,  1243,     0,  1221,  1772,
    1895,  1772,  1895,  1089,  1090,   864,     0,  1301,  1307,  1297,
    1331,  1336,  1341,  1350,  1353,  1354,  2220,  2321,  1346,  2324,
    1351,  1402,  1968,  1402,     0,  2241,  2241,  1387,  1403,  1404,
    1385,  1391,  1386,  2335,  1605,     0,  2257,  1599,  2236,  1772,
    2289,   315,   316,   317,  1435,  1425,  1903,     0,  1821,  1442,
       0,     0,  2282,     0,  1486,  1468,  1481,  1474,  1470,  1483,
       0,   894,   894,  1495,  1504,  1501,  1796,  1799,  1790,  1798,
    1791,  1520,  1523,     0,   894,   894,  1537,  2261,  1544,  1582,
    1627,  1617,  1621,  1622,  1623,  1624,  1615,  1638,  1641,  1639,
    2015,   894,   894,  1650,  1787,  1784,  2245,  1772,  2112,  1772,
     888,  1679,  2080,  1690,  2232,  1696,  2232,  1781,  1745,  1742,
    1741,  2263,   412,   466,   678,     0,     0,   338,     0,  2245,
     559,     0,   511,     0,   661,   563,  2303,  2303,  2303,  2303,
    2303,  2329,   564,   567,   568,   569,   570,   571,   572,   603,
     601,   602,   604,   605,   577,   573,  2299,   606,   632,   574,
     560,   575,   576,     0,  2306,   586,   587,   585,  2258,   589,
     590,   588,  2245,   537,   538,   539,   540,   541,   542,   561,
     565,   566,   543,   544,   545,   546,   547,   548,   549,   550,
     551,   552,     0,     0,  2201,     0,   516,     0,   484,   375,
     284,   403,  2356,  2357,  1923,   384,  1921,  2348,  2347,   377,
    1925,  1924,  2269,  2214,  2232,     0,  2245,   381,   380,  2245,
     409,   229,  2273,  2294,  2326,   300,     0,  2245,  2212,  2256,
     302,     0,  2333,   288,   228,   287,   231,   232,   233,   234,
     235,   236,     0,   237,     0,   238,   299,   239,   240,   241,
     242,   243,   244,  2206,  2245,     0,   325,     0,   867,   879,
     897,  1002,  1004,  1003,  1035,  1033,  1034,     0,     0,  1037,
       0,  1036,  1028,  1038,     0,   994,  1053,  2237,  2114,  2062,
    1098,  1139,  1479,  1478,  2333,  2248,  2249,     0,     0,     0,
       0,  1211,  1206,  1265,  1263,   830,   832,  2280,  1264,  1267,
    1266,  1261,  2255,  2254,     0,  1232,  2245,     0,  1230,     0,
    1237,  1274,     0,  1273,     0,  1091,  1092,     0,  1096,  1095,
    1097,   894,  1355,  2221,     0,     0,  1383,  1394,  1402,  2242,
       0,     0,  1405,  1406,     0,     0,  1608,  1604,  1598,  1418,
    1434,     0,  1443,  1440,  1458,     0,     0,  1471,  2245,  1808,
    1469,  1482,     0,     0,     0,  1485,  1506,  1503,  1499,  1505,
    1500,  1502,  1524,  1531,  1538,  2262,  1553,  1789,  1786,  1782,
    1788,  1783,  1785,     0,  1666,  2114,  1665,  1702,   887,     0,
    1689,     0,  2232,  1693,     0,  1685,   894,   894,  1736,  1747,
    1805,  1802,  1746,  2264,  2265,  1740,     0,   414,   466,   676,
     466,   681,     0,   533,   535,   534,   528,   532,   530,   531,
     527,   529,   526,   667,   662,   664,     0,   513,   670,   671,
    2304,   600,   599,   592,   591,   598,   597,   596,   595,   594,
     593,  2330,     0,     0,  2300,   658,   636,     0,   628,   553,
    2307,  2259,  2260,   659,     0,   555,   672,  2210,  2110,  2110,
     520,   519,     0,   365,     0,   402,  1922,  2270,   386,     0,
     368,  2308,   395,   397,   401,   400,   396,   398,   394,   399,
       0,     0,  2245,  2256,  2327,  2328,   267,   303,  2294,  2245,
    2245,  2245,  2245,   312,  2197,   313,     0,  2245,  2273,  2207,
       0,     0,   331,   332,   335,   182,   897,  1022,  1027,  2360,
    2361,  1825,   996,  2238,     0,  2237,  1772,  1107,  2243,  1219,
    1210,  1209,  1207,  1208,  2223,  1245,     0,  1234,  1238,  1239,
    1772,  1772,  1093,  1941,  1940,  1998,  1335,     0,  1365,  1401,
    1408,  1407,  2245,  1606,     0,     0,  1596,  1600,  1433,     0,
    1473,     0,  1464,  1489,  1797,  1794,  1488,  1472,  1484,  1659,
    1772,  1710,   889,  1695,     0,  1699,  1804,  1807,  1800,  1806,
    1801,  1803,   416,   415,   679,   683,   774,     0,   668,   665,
     523,  2280,   526,   518,   521,   524,   660,   611,   618,   582,
     579,   581,   583,   578,  2232,   630,  2322,   633,  2322,   562,
       0,   554,     0,  2212,   620,   621,   376,   367,   366,   364,
     404,  1917,   385,  2214,   373,   382,   379,   383,   378,     0,
    2245,   269,   268,   265,   302,   298,     0,     0,     0,     0,
    2198,  2199,   311,   314,     0,  2245,   301,   283,   333,     0,
     334,     0,     0,     0,  1054,   894,   894,   894,  1075,  1114,
    1110,  2280,  2244,  1101,  1106,  1105,  1100,     0,  1244,  1276,
    1275,  1356,     0,  1609,  2245,     0,  1487,  1792,  1793,  1795,
    1667,  2203,  1733,  1732,  1711,  1703,  1704,  2191,  1705,  1706,
    1707,  1708,  1731,     0,     0,  1697,   418,   682,   776,   666,
       0,   663,     0,     0,   525,   612,   613,   617,   616,   615,
     614,   580,   619,   584,     0,     0,   628,  2323,     0,   629,
     634,   556,   673,  2111,  1918,     0,     0,   387,   388,   389,
     390,     0,   369,  2231,   375,     0,   277,   278,   276,   275,
       0,   258,   259,   260,   254,   255,   272,   261,   262,   272,
       0,   263,   264,   253,   251,   252,   257,   256,   272,   272,
     272,     0,   304,   305,   306,   307,   310,   285,     0,   336,
       0,     0,  2115,  1112,  1116,  1113,  1108,  1115,  1109,  1111,
       0,  1099,  1607,     0,  1828,     0,  2310,     0,  2282,  2228,
       0,   684,     0,   780,   775,   777,     0,     0,  2245,   522,
     637,   631,     0,   638,  2282,  2282,   640,   393,   392,  2202,
    2214,   374,  2082,   273,   248,   274,   249,  2224,   250,   246,
     247,   270,   245,   271,  2245,     0,   294,   293,   294,   290,
     878,  2113,  1102,  1610,  1459,     0,  2311,     0,  1729,  1728,
    1727,     0,   417,   419,  2241,   685,     0,   781,     0,   778,
    2332,     0,   639,   640,     0,     0,  2210,   625,   645,   646,
     647,  2256,   644,     0,   371,   280,  2083,   266,     0,   308,
       0,   292,   291,  1730,  2344,  2343,  2290,  1723,  1717,  1718,
    1720,   440,     0,     0,   783,   784,   779,   669,   627,   640,
     635,     0,   641,   648,  2245,     0,   391,  2282,   370,     0,
     279,   309,     0,   297,  2291,  2282,  1726,  1721,  1724,     0,
    1719,  2245,     0,     0,     0,     0,   421,   441,   442,   423,
     451,     0,  2245,  2305,   626,   657,   654,   655,     0,   643,
       0,     0,     0,     0,  1725,  1722,  2245,     0,     0,  2245,
    2245,  2245,  2245,   443,     0,  2272,     0,  2315,     0,   420,
     424,   426,   425,     0,     0,     0,     0,     0,     0,     0,
     422,   452,   454,   453,     0,     0,   689,  2245,  2245,  2193,
    2266,   712,   688,   692,   693,     0,  2226,   804,  2245,   795,
     796,   797,   788,  2329,   789,  2245,  2222,  2245,     0,   809,
     802,   792,   803,  2275,   793,     0,     0,   801,   811,   808,
     806,  2245,   794,   805,     0,   812,   800,     0,   824,   818,
     822,   821,   819,   823,   785,   825,   820,   807,   798,     0,
    2275,   656,   649,   372,     0,   189,   190,   282,     0,  2351,
    2352,   295,  1716,  1713,  1715,  1714,  1709,  1712,     0,     0,
     449,     0,     0,     0,     0,  2245,  2245,  2245,  2245,  2245,
     427,     0,  2245,  2245,  2245,  2245,  2245,  2245,  2245,  2245,
    2245,  2245,  2245,   455,     0,  2245,     0,  2383,  2384,  2245,
    2193,     0,   686,   690,  2227,   694,     0,     0,   786,   787,
       0,   790,   791,     0,   827,   813,  2245,  2322,  2245,     0,
     828,   826,   846,   814,  2245,   281,   296,   444,  2322,   448,
     446,   450,   445,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   691,  2267,  2268,   700,   697,   478,   713,   695,
     817,   815,   816,   838,   845,     0,   799,   842,   652,   436,
     432,   433,   437,   435,     0,   438,   428,   434,   429,   430,
     431,   460,   456,   457,   461,   459,     0,   458,   696,  2349,
    2350,   699,   714,   481,   835,   833,   836,   834,   839,   840,
       0,   810,   843,   844,     0,   447,   653,     0,     0,     0,
    2245,  2245,     0,   701,   702,   703,   704,   705,   706,     0,
     716,   837,   841,     0,   462,  2370,  2369,  2245,     0,     0,
    2372,     0,  2245,  2245,   698,  2305,     0,     0,   711,   707,
    2371,     0,     0,  2239,  2277,  2193,     0,     0,     0,  2245,
    2280,   715,  2245,  2245,     0,   721,   723,   732,   724,   726,
     729,   717,   718,   719,   728,   730,   733,   720,     0,   725,
       0,   727,   731,   722,  2277,  2193,     0,   708,   710,   709,
    2240,   771,  2278,  2279,  2250,   757,  2245,   628,  1828,     0,
       0,     0,     0,     0,   765,     0,   755,   761,   764,     0,
     758,   766,   769,  2250,   760,     0,   756,     0,  2322,   753,
    2098,   749,  1951,  2374,     0,     0,  2376,  2378,     0,  2382,
    2380,   734,   738,   742,   742,   736,   740,   735,   741,   772,
       0,   763,   762,   768,   767,   759,     0,   747,   634,   770,
    2282,   748,  1952,  2373,  2377,  2375,  2381,  2379,   745,   737,
     745,   739,     0,   439,   623,     0,     0,   744,   743,     0,
       0,   622,   752,   750,   751,   746,   754,   624
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -3001, -3001, -3001, -3001, -3001, -3001, -3001,  2719, -3001, -3001,
   -3001, -3001, -3001, -3001,  2578, -3001,  1697, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001,  2640,  2580,   -69, -3001, -3001, -3001,
    1486,  2717, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001,  1745,  2672, -3001, -3001,
   -3001,  2674, -3001, -3001,  2218,  -327, -3001, -3001, -3001, -3001,
   -3001,  2447,  1574,  1771, -3001, -3001, -3001,  2215, -3001, -3001,
    1610, -3001, -3001,  2675,  1396,   224, -3001, -3001, -3001, -3001,
    2201, -3001, -3001, -3001, -3001, -3001,  1880, -3001, -3001, -1185,
   -3001, -3001, -3001, -3001, -3001,  2450, -3001, -3001, -3001, -3001,
    2248, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001,  -943, -3001, -3001,
   -3001, -3001, -3001,   134, -3001, -3001, -3001, -3001, -3001,  -182,
   -3001,   149, -3001, -3001, -3001,   -92, -3001, -3001, -3001, -3001,
     143, -3001, -3001,  1904, -3001, -3001, -3001, -3001, -3001,   139,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001,   -82, -3001, -3001,
   -3001,   164, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001,  -297, -3001, -3001,
   -3001,  -263, -3001, -3001,  -304, -3001, -3001, -3001, -1525, -3001,
   -3001,  1930, -3001, -2661, -3001, -2748,  -821, -3001,  -744,  -932,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -2522, -3001,
   -3001, -3001, -3001, -2293, -3001, -3001, -3001, -3001, -3001, -3001,
     787, -2813,  -261,   194, -1931, -1925, -3001, -3001, -2188, -3001,
   -3001, -3001, -2720, -3001, -3001,  -645, -3001, -3001, -1471, -3001,
    -230,  -194, -3001,  1297, -3001, -2173, -3001,  -256, -2097, -3001,
   -2064, -3001, -1949, -3001,   454, -1526, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,  -622,
    -647, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -1629, -3001,  -596, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001,  -118, -3001, -3001, -3001,  -308,  -306,  -455,  -454,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001,  2237,   870, -3001,   649,  1525, -3001, -3001,
   -3001, -3001, -1799, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
    -945, -3001, -3001,   -24, -3001,  2697, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001,  1516, -3001,   745, -3001,  -885, -3001,
   -3001,  -873, -3001,  -232, -1267,  1108, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001,  1451, -3001, -3001, -3001,  2175,
   -3001, -3001, -3001, -3001, -3001,  1341, -3001, -3001, -3001,   720,
   -3001, -3001,   667, -3001, -3001,  -771, -3001, -3001, -3001,    47,
   -3001,    43, -3001, -3001, -3001, -3001,  2174, -3001, -3001, -3001,
   -3001,  1829, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001,  2391, -3001, -3001, -3001, -3001, -3001, -3001, -3001,  2141,
   -3001, -3001, -3001,  1427, -3001, -3001, -3001, -3001, -3001, -3001,
     731, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001,  2147,  1037, -1728, -2015, -3001, -3001, -3001, -3001, -3001,
     690, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001,  1791, -3001, -3001,  1792, -3001,
   -3001,  1418,  1020, -3001, -3001, -3001, -3001, -3001,  2148, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001,   696, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001,   692,  1772, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001,  1764, -3001, -3001,
     998, -3001,  1379, -3001, -3001, -1782,   686,   688, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
    2106, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -2515,  1337, -3001, -3001, -3001,   663, -3001, -3001, -3001, -3001,
   -3001,  1336, -3001, -3001, -3001,  -996,   966, -3001, -3001,   665,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
     659, -3001,   662, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001,   900, -1698, -3001, -3001, -3001,
   -3001, -3001, -3001,  1726,   962, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001,  -254, -3001, -3001,
   -3001, -3001,  1312, -3001, -3001, -3001,  2094, -3001,  2096, -3001,
   -3001, -3001, -3001,  2419, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001,   928, -3001, -3001, -3001, -3001, -3001, -3001,
    2077, -3001, -3001,  1290, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001,   637, -3001,  1301, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
    -107, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001,
     593,  1339,  1360, -3001, -3001, -1011, -3001,  1219, -3001, -3001,
    1220, -3001,   965, -3001,  1861, -3001,  1864, -1241, -3001,  1138,
   -3001,  1146,   651, -3001,   664, -3001,   668, -3001, -3001, -3001,
   -1829,   229, -1470, -3001, -3001,   383, -3001,   387, -1495,   660,
   -3001,  1123, -3001,  1126,  2271, -1024,  2456,  -375, -1508,  -399,
    -977, -3001, -3001,  1852, -3001,  1858,  1498, -1405,   982,   984,
     986,   987,   956,   744,  -379,   846,  1142, -3001,  1417,  -376,
    -907,  -398,  2476,  2451,  2156, -1922,  -279,   555,  -473, -3001,
    -759, -3001,  -366, -1923,  1945, -2666,   -73,  1693, -3001,   599,
   -1372,   -39,  2622,  -361,  -365, -3001,  -336,  -255, -3001,   482,
   -3001,  -881, -1655, -3001,  1387,  -719, -1713, -3001,  1174,  -425,
    1952, -3001, -1915, -1576, -1499,  -156,  -442,  -533,  -393, -3001,
   -3001, -3001,   120,  -532,  -242, -3001, -3001,  1868,  -577,  -618,
    -127,  2040,  -875,  2079,  -431,  3118,  -566,  -312, -3001, -3001,
   -3001,   510,  2347, -3001, -3001, -3001, -3001,   960, -3001, -3001,
   -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -3001, -1705,
   -3001,  1169,   368,   712,   444, -3001, -3001, -3001, -3001,   154,
   -1957,   826, -3001, -3001, -2466, -3001, -3001, -3001, -1156, -2101,
   -2240, -1479, -3001, -3001, -3001,    26, -3001, -1348, -3001, -1482,
   -3001,   321, -3001, -2095, -3001,   -50,  -265, -1952, -3001, -2272,
   -3001, -3001, -3001, -3001, -3001,  2394, -1554, -1643,  -397,  -642,
   -1440,  2424,  1085, -3001, -3001,  -634, -3001, -3001, -3001,  -159,
   -3001,   370, -3001,  1393, -2030, -3001, -3001, -3001, -2637,  -409,
   -3001, -3001, -3001,  -483,   958, -2061, -1671, -3001, -3001,  1173,
   -3001, -3001,  -170, -3001,  1363, -3001, -3001, -3001,    50, -3001,
   -3000,  -401, -3001, -3001, -3001, -3001, -3001, -3001
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,     4,     5,     6,    11,    12,     7,     8,
      13,    14,   152,   153,   154,   156,    18,    15,    16,    24,
      35,   549,    25,    34,   146,   335,   328,   913,  1256,  1257,
    1258,    37,   149,   150,   332,   919,   554,  1650,    19,    20,
      32,    67,    68,    69,    33,   321,    70,    71,   301,   533,
     902,    72,   298,   518,   519,   520,   521,   522,   523,   524,
    1639,   525,   135,   318,   299,   530,   531,   532,   897,   319,
      74,    75,   137,    76,    77,    78,    79,   127,   310,   911,
     311,   312,    80,   287,   871,   872,   873,  1632,  2012,   874,
    3137,    81,   143,    82,   303,   304,   305,   905,    83,   501,
     502,    84,    85,   878,    86,    87,    88,    89,    90,    91,
      49,    50,   106,   491,   286,   492,  1626,  2007,  2008,  2366,
    2367,  2868,  2869,  2870,  2871,  2743,  2932,  2924,  2368,  2849,
    2369,  3010,  2370,  2331,  2371,  2372,  2373,  2374,  2939,  2981,
    2375,  2376,  2377,  2378,  2379,  2874,  2380,  2381,  2614,  2382,
    1875,   855,   856,   857,   858,  1224,   859,  1220,  2622,  2623,
    2760,    29,   280,    30,    46,   102,   281,   282,   848,   283,
    1217,  1614,  1615,  2729,  1616,  3008,  2844,  2583,  1617,  1618,
    2349,  2736,  1619,  1620,  2732,  2837,  2838,  2839,  2840,  1621,
    2598,  2599,  1622,  2585,  1623,  1624,  2002,   836,  1973,  2252,
    2527,  2528,  2806,  2899,  2953,  3059,  3060,  3061,  3062,  3026,
    3027,  3028,  3070,  3071,  3072,  3073,   483,  1591,   484,   485,
     840,   841,  1601,   842,  1213,  1214,  1215,  1985,  2542,  1510,
    1511,  1512,  1513,  1514,   843,  2262,   844,  1596,   845,  1597,
    2326,  2703,  2704,  2543,  1984,  2303,  2304,  2305,  2721,  1980,
    1981,  2307,  2308,  2309,  1515,  1516,  2821,  2823,  2312,  3337,
    3441,  2313,  2718,  2826,  2567,  2916,  2715,  2963,  2967,  3005,
    2968,  2969,  2970,  2971,  3285,  2972,  3036,  3037,  2314,  2315,
    2316,  2317,  1979,  2698,  2544,  2545,  2811,  2319,  2320,  2321,
    1204,  2253,  1595,  2530,  1977,  2695,  2807,  2901,  2993,  3032,
    3082,  3083,  3185,  3084,  3235,  3268,  3293,  3294,  3295,  3296,
    3297,  3298,  3182,  3238,  3300,  3315,  3341,  3342,  3401,  3429,
    3437,  3343,  3344,  3421,  3443,  3345,  3346,  3347,  3348,  3349,
    3350,  3376,  3377,  3380,  3381,  3351,  3352,  3353,  2257,  2808,
    2904,  2905,  2906,  2995,  3033,  3124,  2115,  2116,  3278,  3279,
    3195,  3280,  3203,  3284,  3125,  3126,    43,  1276,  2024,    44,
     340,   558,   557,   923,   924,   925,  1272,  1273,  1655,   560,
    1657,  2389,   477,   817,   818,  1582,  2508,   819,   820,  1958,
    1386,  1387,  1791,   821,   100,   220,   221,   415,   563,   933,
    1728,   564,  1281,  1282,  1283,  2054,  2055,  1308,  2129,  1677,
    1678,  1285,  1716,  1717,  1818,  1719,   932,   222,   416,   602,
     962,   960,   223,   417,   619,  1359,   224,   418,   631,   632,
    1361,   225,   419,   637,  1769,   161,  1363,  1364,  1772,  1411,
    1826,  2143,  2144,  2145,  2637,   437,  2776,  2768,  2888,  2769,
    2886,  2770,   988,   226,   420,   641,   642,   227,   421,   646,
     647,   995,   648,   991,  1774,   228,   422,   651,  1000,   229,
     230,   231,   424,   663,   232,   423,   660,  1009,  1012,   233,
     425,   670,  1382,   671,   234,   426,   681,   682,   683,   684,
    1036,   685,  1396,  1397,  1398,  1796,   686,  1790,  2105,  2106,
    2107,   687,  1037,  1821,   688,   689,  1789,  2414,   690,  1822,
     691,  1023,  1401,  2130,  2131,  2132,  2133,  2134,  2135,  2136,
    1819,  1027,   235,   427,   695,  1044,   236,   428,   237,   429,
     699,   238,   430,   702,   703,   704,  1052,  1053,  1054,  1421,
    1055,  1416,  1417,  1829,  1049,   239,   431,   713,   438,   240,
     432,   714,   241,   433,   717,   242,   434,   720,  1451,   243,
     244,   439,  1454,  1077,  1455,  1836,  1838,   245,   440,   725,
    1078,  1464,  1842,  2154,  2155,  2156,  2158,   246,   441,   728,
     247,   442,   730,   731,  1084,  1085,  1476,  1086,  1087,  1853,
    1854,  1473,  1474,  1475,  1847,  2167,  2168,  2169,   248,   443,
     249,   444,   740,   250,   445,   742,  1094,   251,   447,   748,
     749,   750,  1098,  2185,   252,   448,   752,  1884,  2473,  1885,
    1105,  1106,  1107,  1887,  1889,  1890,   253,   449,   254,   450,
     757,  1534,  2194,  2195,  2196,  1379,  1380,  1381,  2480,  2198,
    2479,  2672,  1117,   255,   256,   451,   759,  1125,  2203,  2490,
    2204,  2488,  2205,  1122,   257,   452,   761,   258,   259,   453,
     764,  1129,   260,   454,   767,  1902,  1903,  1133,   261,   262,
     455,   771,  1139,  1537,  1908,  1909,  1137,   263,   456,   774,
    1141,  1142,  1541,  2218,   264,   457,   779,   313,  1158,   780,
     781,  1562,  1563,   782,   783,   784,   785,   786,   787,   788,
     789,   265,   458,   735,  2174,  1088,  2468,  1481,  1861,  2466,
    2666,   266,   459,   798,  1565,  1166,  1934,  1935,  1936,  1162,
     267,   800,  1168,  2227,   466,   467,   268,   472,   805,   806,
     807,  1180,  1952,  1949,  1176,   269,   473,   811,  1184,   270,
     475,   271,   476,   813,   272,   478,   822,   273,   479,   825,
     274,   480,   828,  1197,  1962,  1963,  1586,  1965,  2245,  2513,
    2247,  1195,  2509,  2681,  2795,  2796,  2797,  3146,  2798,  2988,
    2989,  3019,  2799,  2950,  2800,  2801,  2802,   275,   481,   832,
    1127,  1589,  1590,  2518,  1199,  1667,  2029,  1668,  1669,  2026,
    1670,  1671,  1030,  1391,  1031,  1389,  1032,  1754,  2077,  1755,
    2075,  1756,  2233,  2501,  2234,  2499,  2235,  1899,  2673,  2788,
    1900,  2208,  2209,  2519,  2690,  2520,  2688,  2521,  1784,  1785,
    2096,  1786,  2094,  1787,   753,  1520,   468,  2399,   723,   724,
     706,   707,  1067,  1068,  1424,  1069,  1444,  1445,  1446,  1447,
    1448,  1449,  1171,  1573,  1317,   621,   622,   623,   624,   603,
     652,  1003,   768,   769,   772,  1876,  1877,   565,   729,   791,
     792,  1075,  1108,   634,   635,  2730,  2341,  1239,  2335,  2336,
    2342,   109,   306,   715,   654,  1034,   604,   605,  2652,   606,
    3391,  1331,   626,  1313,  1731,  1855,  2161,  1144,  2162,   643,
     829,  1109,  1659,  2037,  2400,  1529,  1660,   736,   795,   823,
    1856,  2654,   607,   470,   665,   655,   656,   471,   803,   804,
    1661,   906,  2039,   503,   608,   609,   610,   611,   612,   613,
     614,   966,   944,  1339,  1323,  1324,  1335,  1328,  1318,  1320,
     846,  1964,  2975,   983,  1352,  1764,  1120,  1938,   852,  1006,
    1375,  1267,  2724,  2408,  2636,   412,   413,   414,  1971,  2051,
    2000,  1630,  2752,  2327,  1286,  1287,  2620,  1025,   926,   526,
     126,  1988,  1016,  2454,  2117,  1628,  3186,  1371,  2845,   754,
    1868,  2634,  3361,  2460,  2777,  1071,  2417,   539,  2434,  2177,
    2573,  2496,  2525,  3181,  2588,  3064,   551,  2033,  3364,   938,
    1482,   543,  2181,  3015,  1644,  2383,  1193,  2565,   113,  2552,
    2323,   884,  2947,  1944,  3065,  2010,  1019,  2159,  2828,  2118,
    2606,  2562,  2812,  2137,  1862,  1679,  1310,  1004,  2986,   508,
    2339,  3271,  3141,  2017,  1992,   997,  2631,  1942,  1483,   527,
    3307,  3313,  3404,  3405,  3406,  3407,  3408,  3086
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
     314,   693,   939,   814,   488,   115,   116,   117,   998,  1104,
    1835,   733,   122,   110,  1888,   794,  1206,  1207,  1208,   118,
      99,  1718,   879,  1577,   653,   721,  2258,  1014,  2057,  1167,
    1781,   705,  1154,  2034,  2333,  2318,  2035,  1880,   620,  1008,
    1584,   625,   292,  2068,   295,  1633,   946,  1917,  1284,   300,
     737,   903,   633,  2310,  1341,  1997,  2251,  1143,   540,  2311,
     288,   289,   956,   290,  1901,   737,  1972,   716,   296,   297,
    1939,  2461,  2210,  2171,  1418,   716,  1338,   315,   329,   726,
    1007,  2830,  1521,   969,  1388,   107,  1348,  2611,  2612,  2386,
     692,   107,   716,   486,  2138,  2987,   793,  1050,   662,   536,
     338,  1570,  1489,  2590,   922,   629,  2913,   770,  3134,  -852,
    1423,  2047,  1229,  2771,   894,  2438,   125, -1994,  2391,  2392,
    2393,  2835,  3046,  2438,  1987,  2394,  2395,  2396,   922,  1995,
     734,  1130,  1778,   922,  1450,  1872, -2282,   812,  1535,  1408,
    2761,  2346,   649,  3142,   661, -1995,  1468,  2902,  3156,  1792,
    1793,  1394, -2324,  -850,  2957,  2120,  1212,  1478,  3428,  1937,
    1081,  1090,  1882,  1395, -2333,  2388,   996,  1824,   831,  2846,
    1518,   758,   696,  1277,   765, -2210, -2003,  1309,  1008,  2014,
     799,  2113,   722,  2114,   899,  1174,  1225,  2347,  1486,   741,
     743, -2001,   888,   650,  2423,   545,  2199,  2424,  2448,  1370,
     826, -2256,   737,  1431,  2428,  1469,  2176,  2431,   114,  3143,
   -2333,  2415,  1059,   751,   650,  2803,   504,   808,  3144,  1160,
    3118,   536,  1413,  1045,  3031,  2390,  2571,  1911,   976, -2333,
    1863,  1912,  2192,    22,  2231,  1646,  1647,   493,   974, -2282,
     625,   625,   625,  1079,  2902,  1870,   528,  2241,  1274,  2237,
    2238,  1478,  2446,  2705,  1028,  1080,  2110,  2609,  1568,  2648,
    3012,  1277,  1752,    60,   544,   633,   948,  2892,  1913,  2979,
    1114,  -852,  1863,   838,  1866,  -852,   546,  1182,  1580,  1252,
    1720,   552,  2011,   114,  2036,  -642,  2903,  1690,  1724, -2333,
    1782,  1187,  3312,   650,   114,  2843,  2548,  -687,   659,   916,
    1118,   141,   630,   971,   629,   629,   629, -2202,  1648,  2043,
   -2202,  1587,  3011,  3169,   650,  -850,  3145,  1170,   650,  -850,
     766,  1859,  1112,  2604,  1570,  1522, -1956,  -687,  -687,  1155,
     469,  2740,  2934,  1864,  3423,  3179,  3180,  2432,   895,  1956,
    1186,  -852,    17,  1470,  1588,  1523,   922,  1035,   537,   963,
    1074,  3442, -2331,  2433,   487,  1119,   778,   568,  -852,  3320,
    -852,  1074,  1467,   141,  2549, -2230,  1188,  1783,   992,  3379,
     650,  3402,  3132,  1270,  2772,  1864,   291,  2605,  2943,  2456,
    1010,  2457,  2935,  2903,   630,  -850,  1867,  1413,  -642,  1376,
    2572,  1253,  2765,  1001,    63,  1753, -2079,  1284,  1346,  2215,
      23,  2649,  -850,  3013,  -850,  2810,  1314,  2211,  1029,   511,
    1569,  2814, -2212,   529,   528,  1275,  2111,   737,   512,  1471,
   -1546,   716,   877,  1172, -1546,  1151,   625,  2948,  1074,  1794,
     307,  2015,  2232,  2847,   576,  1046,  1570,   963,  1759,   877,
    1570,  1113,  1418,  1741,  2464,  1418,  2727,  2013,  2773,  1570,
    1288,  2458,  1070,  2973, -1956,   576,  1271,  1490, -2320,  -687,
    2966, -1546,  1873,  1649,  1147,   886,  1372,  2774, -2210,  1479,
     537,  1583,   -38,  1570, -1846,  2403,  2416,   893, -2333,  2193,
    1082, -2232,  1571,  1570,  1091,   581,  1527,   776,   802,  3272,
     629, -2333, -2210,  2842,  -687,  2848,   586, -2210,  2199,   914,
   -2333,   915,  3335,  1173,   909,   910,   581,   853,  -852,   996,
    1839,  1394,  1554,  2952,  1570,  2449,  -857,   586,  2080,   982,
    1555,  3237,   815,  1395, -2202, -2256,  3047,  2220,  1311,   719,
   -2333,  2385,  1883,  2348,   576,   566,  1874,   627,  1383,  2048,
     644,   487,  1376,  2663,   664,  1169,   694,  1205,   291,  1572,
    2402,   808,  -850,  1399,  1441,   576,  3157,   963,  1878,   576,
    3244, -2333,   732,   653,   888,   625,   625,   625,  1083,  1517,
    1517,  3248,  1327,  1327,  1327,   625,   625,   625,  2589,  1723,
     568,   529,   816,  2121,   801,   581,  1347,  3135,   777,   625,
    2626,  1905,   625,  1969,   590,   576,   586,  -687,   824,   291,
     830,   625,   625,   625,   625,   625,   581,   860,  1480,  1254,
     581,   576, -2245,  -857,  1488,   590, -2202,   586,   339,  2836,
    1230,   586,  2833,  2053,  2438,  1373,   778,  1099,   625,   629,
     629,   629,   802,  1795,  1797, -2210,   877,   890,   854,   629,
     629,   629,  1334,  2310,   815,  1334,   581,  3388,   653,  2311,
    2674,   705,  2775,   629,   513,  1334,   629,   586,  1542, -2210,
    1173,  1378,   581,   877, -2210,   629,   629,   629,   629,   629,
    2146,  1051,   302,   586,   291,   592,  2659,   831,   650,   108,
    2974,   291,  2450,  1566,   291,   108,  2639,   487,   815,  1385,
     940,  1472,   629,   291,   590,  1531,   592,   653,  1751,  3004,
     896,  3136,  1480,   977,   816,  1571,  1342,  2016,   978,  1452,
   -2210,  2053,  2052,   625,  1860,   590,  1556,  1156,   900,   590,
    1226,  3170,   917,   861,  2028,  1758,  2031,   716,  2529,  -855,
    1377,  1485,   596,  1714,  1255,  1830,   973,   973,   973,  1414,
     514,   291,  2783,  3077,  1002,  2088,  2767, -2295,   816,  1312,
    1415,  3418,  2476,   596,  1557,   590,  1848,  1157,  -687,  1235,
    -467,   644,  2511,   918,  2514,  2465,  1757,  2949, -2333,  1147,
     639,   590, -2073,  3078,  3079,   592,  1104,   629,  1558,  2765,
     963,  1732,   291,   977,  3021,  1017,  1074,   653,   978,   601,
    1407,  1409, -1956,  2728,  1575,  3002,   592,   625,  2602,  1288,
     592, -1956, -1956,   291,  1748,   515, -1956,  2073,  2074, -2026,
     601,   516,  1070,  1219,  1532, -2333,  2406,  1571,   576,   877,
     837,  1571,  2628,  2069,  1915,   862,  -855,  1241,  2201,  1245,
    1571,   464,   596,  2610,  1233,  2578,   592,  2092,  2093,  3022,
    1487,  1559,  1237,  2056,  2100,  3120,  2789,   561,   435,   630,
    1418,   511,   592,   596,  1571,  2157,  1101,   596,  1102,  2992,
     512,   629,  2591, -2320,  1571,  2067, -1546, -1546,  1250,   581,
    1268,  2991,   640,  1110,  1277,  3188,   863, -2210,  1147,  1277,
     586,   877,   291,  1775, -2075,  2149,  1730,   864, -2077,   601,
     307,  2151,   307,   596,  1545,  1571,  1145,  1738,  2441,  1547,
    2443,  1552,  2123,   291,  1414,  2632,   865,   291,  2936,   596,
     601,  1560,  1567,  1399,   601,  1415,  2139,  3066,  2141,   576,
   -2202, -2071,   973, -2202,  1242, -2202,   541,  2036, -2202,  1181,
    1581, -2066,   837,  2766, -2333, -2333,  3121,   517,  2469,  2189,
     114,  2664,  2018,   291,   737,  1259,   291,   653, -2333,  1370,
     601,  1897,  2206,  2207,   435,  2425,  1254,  1431,  1517,   291,
    1770,  2550,  1762,  2791,  2179,   511,   601,  2101,   511,  3122,
     581,   639,  3130,  1673,   512,  1270,  1674,   512,   590, -2333,
    2176,   586,  2340,  2735,  2676,   866,  1147,  2146,  2337,  1636,
     928,  2923,  2931,  1766,   278,  1768,  2504,  1739,  2506,  2484,
    3075,  2566,  2324,  2693,   756,  2694,   760,  2004,   489,  2675,
     506,   775,  1334,     3,  3424,   307,  1732,   853,  2699,  2104,
     850,  1752,  1834,   778,  1123,  3305,  1368,  1771,  2990,  2212,
    2684,  1561,  1940, -2030,  2019,  3189, -2030,  1114,  3306,  3425,
    2550,  3075,  2459,  3080,   851,  2005,  1955,  1840,  2202,  2551,
    2239,   436,  1904,  2613, -2228,  2699,  1843,  1369,  1271,   592,
    2213,   973,   973,   973,  2755,  2990,   867,   291,  2937,   868,
     869,   973,   973,   973,  1332,  3286,  2665,  1332,  2792,   590,
    3067,  1255,   562,  -467,  3123,   973,   877,  1332,   973,  1112,
     307,  -467,  2124,  1950,    26,  2070,   513,   973,   973,   973,
     973,   973,  2310,  1950,  1918,   964,  1635,  2579,  2311,   830,
     778,  1366,  1932,   507,  1564,  2426,   596,  1933,  2338,   716,
    1074,  3147,  3178,  1869,   973,  2793,  2163,  3336,  2553,  3066,
     963, -2333,  1491,  2767,    -7,  2762,    28, -2202,  2592,  2079,
    1898, -2333, -2202,   838,  2125,  2593,   291,  3023,   854,  1740,
     542,   839, -2228,  1370,  1753,   307,  1243,   436,  1892,  2483,
     592,  3024,  1280,  1147,  1147,    27,  2249,  1625,  2794,  2056,
    1124,  3025,  2561,   601, -2295,  1492,  1493,  1494,  1495,  1496,
     870,  2484,   514,   291,  2938,    31,   737,   291,    36,   291,
    1675,  1244,   877, -1905,  1946,  1948,  1658,   625,   625,   490,
    1637,  1145,  2625,  2325,  3081,   568,  -467,   307,  1113,   973,
     513,  2212,  2250,   513, -2280,    41,  2533,   596,  3338,  2072,
    1651,  1497,  1498,  3234,   291,   929,  1021,    51,  1941, -2202,
    1662,  1147,  2824,  2126, -2202,  1153,    39,   877,  2038,  1260,
    1261,  1262,  2213,  2815,   877,  2243,  1530,   515,  2102,  2550,
     279,  3339,  1099,   516,  3354,   838,  2486,  2487,  1538,  3068,
    1540,   629,   629,   839,  2882, -2333, -2079,   291,   291,  2493,
    2494,  1638,  3240,  3057,   601,  3241, -2280,  2103,  3242,   291,
     291, -2314,  1676,  3069,  1238,    52,  2497,  2498,   291,   291,
     157,    40,  3067,   973, -2003, -2333,   514,  1576,  1922,   514,
    1923,  1924,  1925,  1926,  1927,  1928,  1929,  1930,  1441, -2280,
    1145,   708,  1263,   802,  2485,   144,  1427,  1428,  1429,   889,
      45,  2127,  1953,  2165,  1779,  2816, -2333,    53,  1897,    54,
   -2202,    55,  2087,  2804,  2741,  2104,  2355,  2555,   506,    56,
    1499,  1500,  1501,  1502,  1503,  1504,  1505,  1506,  1507,  1752,
    3274,   877,   291,   937,    -6,  1946,  2166,  1946,   625,   291,
     625,   515,  1134,  2098,   515,  3301,  3340,   516,  3365,  3302,
     516,    -6,  2356,    -6,  2872,  1430,  -847,  2128,  3426,   890,
      47,  2082,  2941,     9,   625,    10,  2944,   291,  2742,  2550,
    2594,  2550,  2534,  2187,  1658,  2764,  2817, -2333,  3384, -1905,
     333,  2516,  2091,  3427, -2271,   625,    92,    57,  2707,  2779,
    2780,  2200,  3386,  2087,   862,  1393,  3275,   145,  1145,  1919,
    3138,  2722,   629,   666,   629,  2140,  1200,  2142, -2333,  2148,
    2818,  3415,   667,   887,  1334,  1733,  1236,   877,  1163,  2790,
     107,   507,   877,   576,  1332,  1782,    48,  3054,   629,  2212,
    1457,    60,    93,  1458,  1459,  1959,   709,   710,  2951,    94,
    3196,  1264,  1265,  2917,  2918,   863,   158,  1266,   159,   629,
    2708,  3068,   139,   653,  2964,  2965,   864,  2557,   711,  2559,
    2213,  1101,  1753,  1102,  2412,  3057,  1904,  3204,  1782,  1431,
    -405,  1013,  2998, -2314,   581,  3069,  2485,  2595,  2050,  2744,
   -2245, -2280,   334,  2709,  1074,   586,  2656,  2214,  1164,   160,
     993,  3139,  1165,  3140,  -405,  1508,    58,  1898, -2280,  1221,
    1820,    -6,  1783,  2596,  2020,  2597,    61,   494,  3034,  2087,
     668,  2895,  1509,  1147,   139,   114,  2750,  2751,  -847, -2333,
     712,  1147,  -847,   977,  1432,  1433,  1663,  1982,   978,   744,
    1664,  2954,  1986,  2413,  1989,  3029,  -405,  1994,  1996,   653,
    1998,  2517,  2535,  2226,  3030,  1783,   625,  3040,   977,    59,
   -2333,  2686,  2687,   978,   866,  3043,  3276,     9,   495,    10,
   -2333,  3277,    63, -2280,   877,  2003,  1432,  1433, -2280,   496,
    1222,  1223,  1434,  1435,   576,  1145,  1857,  2186,  2470,   977,
    2819,  3006,  2021,   590,   978,  1918,  1918,  1918,  -847,  2038,
    3055,   700,  3016,  1456,  2036,   745,  1457,    97,  2984,  1458,
    1459,   979,   701,  2985,   746,  -847,    98,  -847,  3314,  2471,
     629,   980,  2040,   114,  1434,  1435,  1436,  -405,  2041,  2638,
    1110,  2042,  2563,  1891,  3355,   581,  3362,  3363,  1895,  1437,
    3017,  3374,  1147,  1896,   994,   867,   586,  1438,  3289,  2049,
     307,  1663,  1914,  1145,  1439,  1664,  2219,  3393,  3310,  2063,
    2820,    62,  3290,  2401,  3018,  -405,  2184,   669,  1918,  1918,
     129,  1550,   101,   133,   592,  3419,   877,   497,   103,  2228,
    2228,  1440,  1551,   973,   973,  2710,   104,  3291,  1954,  3375,
   -2314,  1574,  1665,   317,  1666,  1966,  1966,  3394,   323,   105,
    -405,   977,  2711,  2482,  3390,  3392,   978,  -405,  3310,  2603,
    3163,   308,   747,   309,  3305,    65,   291,   308,  2536,   309,
    2537,  -405,  3292,  2087,    95,    96,  3422,  3306,   316,  2538,
    2539,   596,   320,  2540,  2541,  2429,  2430,  3412,   111,  2109,
    2455,   140,   949,   950,   590,  3432,  2112,  3311,  3417,  2119,
     638,  2569,   955,  1460,  1461,  2122,  3128,  2108,   498,   870,
     698,    42,    51,  1760,  3305,  -847,   645,  2712,   487,   568,
   -2314,  1603,  2713,   977,  3446,  2644,   112,  3306,   978,  3056,
    2533,  1462,  1463,  1269, -2314, -2314, -2314,  1269,   601,  1849,
    3164,   460,  2702,  3057,    73,  1604,   114,    51,   986,   119,
    3001,    66,  2175,  3058,  3165,  3166,  3167,   120,   132,   134,
    2883,  2884,  2885,  1353,  1354,   499,  1099,  1112,   877,   487,
      52,    51,  1350,   121, -2271,   592,  2418,  2419,  2420,  2230,
    1441,   128,   125,   977,  1442,   136,  1443,  1605,   978,  2421,
     977,    60,  -405,  -405,   973,   978,   973,   123,  1850,  2409,
    3389,  2329,  2575,  2576,   124,    52,  1332,  -405,  2343,  -405,
     977,  1191,    53,  1192,    54,   978,    55,  3054,   322,  2616,
     973,   324,   500,  2086,    56,    63,  1442,   569,  1443,    52,
     142,  1370,   596,  1780,  2020,   151,  1112,  1316,  1319,  1322,
    2260,   973,   148,  1665,   571,  1666,   155,    53,   644,    54,
     461,    55,  1329,  1330,  2577,  2655,  2926,   277,   284,    56,
    1919,  1919,  1919,   285,  1349,  2928,  2929,  2930,   291,  1460,
    1461,    53,   294,    54,  2322,    55,  2475,   302,   326,    51,
    2332,    61,  1990,    56,  1991,   327,  2523,  2345,  2524,   601,
     330,   331,    57,  2809,   462,  2150,  2038,  1462,  1463,  2629,
    2702,  2630,  1918,  2350,  2086,  1145,  2534, -1975, -1975, -1975,
   -1975,   446,  2627,  1857,  -405,   474,  3139,  3395,  3140,   505,
    3435,  3396,  3397,   293,  3269,   293,  3270,    57,   482,   509,
     293,   463,   517,  1919,  1919,  2397,  2398,    52,  2435,   547,
    2188,   114,   510,  2404,  1403,  1404,  1405,  1406,  1606,   548,
    1292,    57,  1293,   550,   572,   573,   574,   576,  1039,  1040,
    1041,  1042,  1607,   575,   553,  -405,   464, -1974, -1974, -1974,
   -1974,  3398,   555,   972,   108,   975,   559,   576,   719,    53,
    3055,    54,   727,    55,  2650,   755,  2651,   556,  3399,  3400,
     487,    56,   802,   834,  2221,  1101,   835,  1102,   847,  2813,
    1734,    58,   973,  1736,   876,   849,    51,   881,   581,   882,
     883,  1742,  1851,   885,   892,  1746,   577,   898,  2437,   586,
    2086,    51,  1749,   904,  1145,   908,   920,   921,   581,   930,
     931,   582,   583,   584,   934,   936,    58,   585,   291,   586,
     937,   959,  2467,   941,  2700,   465,  2624,  2186,  2554,  2556,
    2558,  2560,   942,  1112,    59,  1112,   943,    60,   945,    57,
      58,  1355,  1356,  1357,    52,   951,   952,   953,   954,  2890,
   -2314,   957,   964,   965,   967,   968,  2535,   981,   985,    52,
     987,  1005,   999,  1608,  1609,   588,  1011,  1013,  1015,    59,
    1112,  1018,    60,  1022,  1020,  1024,  1043,   293,  1610,  1026,
    1611,  2568,  2640,  2641,  2642,  2643,    53,  1047,    54,  1048,
      55,  1076,  1072,    59,  1089,  1092,  1093,   590,    56,  1095,
    1097,    53,  2647,    54,  1115,    55,  2503,  1116,  1126,  1121,
    1131,  1128,  1918,    56,  1132,  1138,  1136,   590,  1140,  1148,
     778,  1152,  1159,  1161,  1173,  2660,  2661,  1175,  1179,  2546,
   -2314,  1183,  1196,  1194,  1190,  1198,    62,  2997,  1201,  3056,
    1202,  1203,  1210,  1205, -2314, -2314, -2314,    61,    58,  1218,
     591,  1232,  1234,  3057,  2615,  1216,   307,  1238,  1246,  1240,
    1247,  1248,   568,  3058,  1251,  2059,    57,  2061,    63,  2841,
      64,    62,  2574,  2064,  2065,  2066,  1289,  1315,   592,  1351,
    2071,    57,  1325,  2701,  1326,  1612,  1112,  1336,  1337,  1358,
      65,  2083,  2084,  1112,  1360,    62,  1345,  1919,   592,   630,
    1374,    59,  1378,    63,  2086,    64,   636,  1028,  1384,  1029,
     657,  1400,  1402,   593,   594,  1410,  2600,  2832,  1419,  2601,
     657,   718,  2536,  1422,  2537,    65,  3236,  2608,   657,    64,
    1891,   738,  1453,  2538,  2539,   596,  1613,  2540,  2541,   291,
     763,  1918,   763,   773,  1466,   796,   738,  1082,  1484,    65,
     569,   877,  1519,  1524,  2621,   596,  1526,  1525,  1528,  1533,
    1536,  1543,  1544,  2624,  1852,  1546,   597,   571,  1553,   763,
     922,  1578,  1585,  1579,  1592,    58,    66,  1593,  1594,    61,
    1598,  1599,  1600,  1602,  1627,  1629,  1634,  1631,   598,  1641,
      58,  1643,   601,   599,  1645,  1652,   880,  1654,  1656,  1680,
    1721,  2271,   600,    62,  2186,   291,  2646,  1722,  1725,  1729,
    1726,    66,   601,  1727,  1735,  1737,  3299,  1773,  1743,  1776,
    1763,  1777,  2781,  1744,  1745,  2352,  1747,  1750,    59,  1673,
    1761,  1292,  1674,  1293,  2785,    66,  3273,    64,  1767,  1788,
    2353,  1823,  1827,    59,  1825,  1832,  1828,  2955,  2671,  1837,
    1479,  2354,  1841,  3318,  3319,  1844,  1845,    65,  1871,  1918,
    1858,  1879,  3357,   751,  1886,  1906,  3358,  3359,  1907,  1916,
    1921,  1957,  1943,  1960,  1961,  2726,  1970,   572,   573,   574,
    1974,  1975,  2733,   738,  1976,  1978,   575,  1987,  1999,  2001,
    2009,  1370,  2023,  3378,  2025,  3382,  2022,  1859,  2032,  2045,
     576,  2050,  2062,  2046,  1752,  1753, -2002,  2692,  2089,  1783,
    1782,  2152,  2164,  2153,  2160,  2172, -1997,  2173,  2176,   636,
    2178,  3378,  2183,  2180,  2186,  3382,  2190,  1919,  2191,  1898,
      62,  2731,  3378,  1897,  2217,  2236,  2242,   644,  2240,   577,
     657,  2244,  2254,    66,  2246,    62,  2255,  2256,  2259,  2330,
    1112,   581,  2260,  2344,   582,   583,   584,  2328,  2387,  2261,
     585,  2351,   586,  2334,  2407, -1949, -2000,  2411,  2427,  2442,
    2453,  2444,  2739,  2165,  2645,  2166,  2459,  2477,  2472,  2746,
    2747,  2748,  2749,  2478,    65,  2201,  2202,  2754,  3205,  2492,
    2495,  2231,  2232,  2507,  2512,  3323,  2526,  2532,  2570,    65,
    2531,  2561,  2582,   657,  2564,  2584,  2580,  2587,   588,   511,
    2829,  2607,  2617,  2619,   657,  2618,  2633,  2635,  2657,  2658,
    2662,  2668,  2669,  2670,  2677,   325,  2286,  2678,  2517,  2682,
    2516,  2696,  2782,  2706,  2697,  2714,  2717,  2716,  2720,  2723,
    2759,  1111,  2784,  2679,  2756,  2822,  3324,  2825,  3325,  2827,
    2873,  2683,  2879,  2831,  2685,  2880,  1919,  2881,   773,  2897,
     590,  2898,  2907,  2908,  2894,  2900,  1290,  1291,  2910,  2766,
      66,  2919,  2912,  2914,   657,  2915,  2927,  2834,  2946,  2355,
    2945,   657,  2960,  2962,  2940,    66,  3326,  1531,  2791,  2966,
    3007,  2959,  2980,   591,  3009,  2875,  2876,  3014,   738,  2994,
    2996,  2877,  3042,  3044,  3041,  3051,  3035,  3149,  3370,  3327,
    2850,  3049,  3050,  3052,  3076,  2356,  3155,  3158,  2911,  3159,
    1292,  3168,  1293,  3162,  1294,  2878,  -289,  3172,  3171,  3175,
    3184,  3183,  2043,  2290,  3198,  3202,  2036,  3320,  2921,  3360,
    3328,   592,  3409,  3366,  3410, -2079,  3439,  3436,  3445,  3368,
      21,   336,  3440,   147,  2893,   337,   593,   594,  1642,  1295,
    1296,  1297,    38,   891,   131,   130,   901,   912,   534,   875,
     138,  3232,  1228,   535,  1919,  2757,  2982,  2745,  2922,  2753,
    1227,  2758,  2920,  3160,  3053,  2738,  2778,  3173,  2719,  2357,
    1209,  2306,  3119,  3434,  2358,  3039,  3003,  2875,   596,  1865,
    3131,  2581,  3431,  3438,  3414,  2829,  1532,  2958,  3191,   597,
    3192,  2786,  3282,  3283,  2447,   927,  1653,   276,  1672,  2405,
    1298,  3127,  1299,  1765,  2805,  2044,   984,  2294,  2410,  1300,
    2445,   598,  2887,  1301,  2359,   990,   599,  2889,  3329,   697,
    3281,  1367,  2360,  1798,  1033,   600,  2099,  2440,   291,  1343,
    1799,  1800,  1801,  1802,  2361,   601,  1803,  2422,  1038,  3330,
    1831,  1412,   644,  1804,  1805,  1420,  2452,  2147,  2451,  1477,
    1465,  2170,  1846,  2474,  1096,  2463,  2462,  1881,  2961,  1073,
    2197,  2481,  2731,  1894,  2491,  1539,  2489,  3331,  2186,  2248,
    1860,  2216,  1806,  2229,  1931,  2362,  1967,   797,  1149,  2510,
    1910,  3020,  1150,  1178,  2978,  2363,  1893,  1968,  2027,  2030,
    3332,  2999,  3000,  1392,  2078,  1390,  -286,  2891,  2515,  2502,
    1807,  2076,  2500,  2787,  2691,  2731,  2834,  2689,  2522,   989,
    2097,  3333,  2095,   790,  1343,  1302,  1425,  1303,  1978,  1426,
    2222,  3334,  2223,  1833,  2224,  2225,  1135,   538,   762,  2653,
    1640,  1362,   833,  2909,  2586,  1920,  2364,  2085,  2834,   778,
    2365,  1365,  3194,   636,  3239,  1249,   958,  2725,  2090,  2680,
    2505,  2896,  2436,  2977,  3038,  1231,  2763,  3383,  2182,   935,
     657,  2734,  1945,  1343,   907,  2731,  2384,  3133,  3206,  1993,
    3403,  3048,  2976,     0,     0,     0,     0,     0,     0,  3197,
    1808,     0,  3085,  3129,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   625,   625,     0,  3148,     0,     0,  3151,
    3152,  3153,  3154,     0,     0,     0,     0,     0,     0,     0,
    2942,     0,     0,     0,     0,   625,  1809,     0,     0,   657,
       0,     0,  3208,     0,     0,     0,     0,  3176,  3177,     0,
       0,     0,     0,     0,   625,     0,     0,     0,  3187,     0,
    1810,     0,     0,     0,     0,  3190,     0,  3193,     0,     0,
       0,     0,     0,  1343,     0,     0,     0,   629,   629,  2834,
       0,  3199,     0,   625,     0,  2983,     0,     0,     0,     0,
       0,     0,  3246,     0,     0,  1304,     0,     0,     0,   629,
       0,   567,     0,     0,   568,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   629,     0,
       0,     0,  1811,     0,     0,  3213,  3214,  3215,  3216,  3217,
       0,     0,  3219,  3220,  3221,  3222,  3223,  3224,  3225,  3226,
    3227,  3228,  3229,     0,     0,  3231,     0,   629,     0,  3233,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1812,   738,  3243,  1305,  3245,  3063,
    3074,     0,     0,     0,  3247,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1813,     0,     0,  3150,     0,
       0,     0,   569,     0,     0,     0,     0,  2851,  2852,  2853,
       0,     0,     0,  3161,     0,     0,     0,     0,     0,   571,
       0,     0,     0,     0,  3174,     0,     0,     0,     0,     0,
    3367,     0,  1306,     0,     0,   738,     0,     0,     0,   657,
       0,     0,     0,   657,     0,     0,  1307,     0,  1530,     0,
       0,     0,     0,     0,     0,     0,   657,     0,     0,   636,
       0,     0,     0,  1704,  3200,  1705,  1706,  3201,     0,     0,
    3308,  3309,     0,     0,     0,     0,     0,     0,  3411,     0,
    1814,     0,  3413,  2854,  2855,  2856,     0,  3317,     0,     0,
       0,     0,  3321,  3322,     0,     0,     0,     0,  3207,     0,
       0,  3209,  3210,  3211,  3212,  1815,   657,     0,     0,  3369,
       0,  3218,  3371,  3372,  1816,     0,     0,     0,     0,     0,
       0,     0,     0,   672,  3230,     0,   644,     0,     0,   572,
     573,   574,     0,     0,     0,     0,   664,     0,   575,     0,
       0,     0,     0,  3447,     0,     0,  3387,     0,     0,     0,
       0,     0,   576,     0,     0,     0,     0,     0,     0,     0,
       0,   673,     0,  3249,  3250,  3251,  3252,  3253,  3254,  3255,
    3256,  3257,  3258,  3259,  3260,  3261,  3262,  3263,  3264,  3265,
    3266,  3267,   657,   657,   657,  1817,     0,     0,     0,  1017,
       0,   577,   578,     0,     0,   644,     0,     0,   579,     0,
     580,     0,     0,   674,  3287,     0,   582,   583,   584,     0,
    2857,  2858,   585,     0,   586,     0,  3288,     0,     0,     0,
       0,   675,   636,   587,  2859,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  3303,  3304,     0,
    1277,  2860, -2333,     0,   738, -2333, -2333, -2333, -2333,     0,
    1278, -2333,     0,  3316,     0,     0,     0,     0,  2861, -2333,
     588,     0,     0,     0,     0,   676,  3356,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   738,   657,     0,
     589,     0,     0,     0,  3373,     0, -2202, -2333,     0, -2202,
       0,     0,     0,     0,     0,     0,  1983,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  3385,     0,     0,     0,
       0,     0,   590,     0,     0, -2333,     0,  2862,     0,   973,
     973,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0, -2333,  3416,     0,  2863,  2864,  2865,
    2006,   973,     0,  -230,     0,   591,     0,     0,     0,     0,
       0,     0,     0,  3430,  3430,  2866,     0,     0,  -230,     0,
     973,     0,     0,     0,     0,   628,  3433,     0,     0,  -230,
     658,     0,     0,     0,     0,     0,     0,     0,   738,     0,
     658,     0,  -230,     0,     0,  3444,     0,     0,   658,   973,
       0,     0,     0,   592,     0,   677,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   658,     0,     0,   593,   594,
       0,   678,     0,     0,     0,  -230,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0, -2333,     0,     0,     0,     0,     0,     0,  1279, -2333,
     293,     0,     0,     0,     0,     0,   679,     0,     0,   162,
     596,   163,     0,   164,     0, -2333,     0,   657,   165,     0,
       0,   597,     0,  2867,     0,     0,   166,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   598,     0,     0,     0,     0,   599,     0,
       0,     0,     0, -2333,     0,     0,     0,   600,   680,     0,
     291,     0,     0, -2202,     0,   167,   168,   601,     0,     0,
       0,     0,     0,     0,     0, -2333,     0, -2333,     0,     0,
     169,   657,     0,   657,     0,     0,   947,     0,  1280,     0,
       0,     0,     0,   170,     0,     0,     0,   171,     0,     0,
       0,     0,     0,     0,     0, -2333,     0, -2333,     0,     0,
       0,   172,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   970,   628,   628,   628,   657, -2333,   657,
       0,     0,     0,     0,     0,   173,     0,     0,     0,     0,
   -2333,   174,     0,   175,     0,     0,   176,     0,   177,     0,
       0,     0,     0,     0,     0, -2202,     0,  -230,   178,     0,
     658,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0, -2333, -2333,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   179,     0,     0,     0,     0,
       0,     0,     0,  -230,     0,     0,     0,     0,   180,     0,
       0, -2333,     0,   181,  -230,     0,     0,     0, -2333,     0,
   -2333, -2333,     0,     0,     0,   657,     0,     0,     0,     0,
       0,     0,     0,   658,     0, -2333,     0,     0,     0,   738,
   -2333,     0,     0,     0,   658,   182,     0,     0,     0,     0,
       0,     0,   183,     0,     0,   184,   185,     0,     0,     0,
   -2333,     0,   293,     0,     0,     0,     0,     0,     0, -2333,
     186,     0,     0,     0,     0,     0,     0,  -230,   187,     0,
     188,     0,  -230,   189,     0,     0,     0,     0,     0,     0,
       0,     0, -2333,     0,  1146,     0,     0,     0,     0,     0,
       0,   738,   738,   738,   658, -2204, -2333,     0,   738,   738,
     738,   658,     0,     0,     0,   738,   738,   738,     0,   738,
     628,     0,  -230,     0,     0,     0,     0,     0,     0,     0,
    -230,     0,     0,   190,     0,     0,     0,     0,     0,   191,
   -2333,   192,  -230,     0,     0,     0,     0,     0,     0,     0,
   -2333,     0,   193,     0,     0,     0, -2333,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   877,     0,     0,     0,   738,  -230,  -230,
     738,   194,     0,  -230,     0,     0,     0,   738,   738,   738,
     738,     0,     0,  -230,     0,   195,     0,     0,     0,     0,
       0,     0,     0,     0,  -230,     0,     0,   657,     0,   657,
       0,     0,     0,     0,     0,     0,     0,   162,     0,   163,
       0,   164,     0,   196,     0,     0,   165,     0,   197,   198,
     199,     0,     0,     0,   166,     0,     0,     0,     0,     0,
     200,     0,     0,   293,  -230,     0,     0,   657,  -230,     0,
       0,     0,     0,   201,     0,   202,     0,     0,     0,   628,
     628,   628,   203,     0,     0,     0,   204,     0,     0,   628,
     628,   628,  1333,   167,   168,  1333,   205,     0,     0,  1344,
       0,     0,     0,   628,   206,  1333,   628,     0,   169,   207,
       0,     0,     0,     0,     0,   628,   628,   628,   628,   628,
       0,   170,   208,     0,     0,   171,     0,   657,     0,     0,
       0,   209,     0,     0,     0,     0,   210,   211,     0,   172,
       0,     0,   628,   212,     0,     0,   213,     0,     0,  2547,
       0,     0,  1799,  1800,  1801,  1802,   214,     0,  1803,     0,
       0,     0,     0,   173,   567,  1804,  1805,   568,     0,   174,
       0,   175,     0,   215,   176,     0,   177,     0,  1056,     0,
       0,     0,     0,     0,  1344,     0,   178,     0,     0,     0,
       0,     0,     0,   216,  1806,     0,     0,     0,     0,     0,
       0,   217,     0,     0,     0,     0,   218,     0,     0,     0,
       0,     0,     0,   179,     0,     0,     0,     0,     0,  1146,
       0,     0,  1807,     0,     0,     0,   180,   628,     0,     0,
     658,   181,     0,  1344,     0,     0,     0,   219,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   569,     0,     0,     0,     0,
       0,     0,     0,   182,     0,     0,     0,     0,     0,     0,
     183,     0,   571,   184,   185,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   738,     0,   186,   658,
       0,     0,   738,     0,     0,     0,   187,     0,   188,     0,
       0,   189,  1808,     0,     0,     0,     0,     0,     0,     0,
       0,   628,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1344,     0,     0,     0,     0,  1146,     0,
     657,     0,   657,     0,     0,     0,     0,     0,  1809,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   190,     0,     0,  1057,     0,  2667,   191,     0,   192,
       0,     0,  1810,     0,     0,     0,     0,     0,     0,     0,
     193,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   572,   573,   574,     0,     0,     0,     0,     0,
       0,   575,     0,     0,     0,     0,     0,     0,     0,   194,
       0,     0,     0,     0,     0,   576,     0,     0,     0,     0,
       0,     0,     0,   195,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1811,     0,     0,     0,     0,  1058,
       0,     0,     0,     0,     0,     0,  1146,     0,     0,     0,
       0,   196,     0,     0,   577,   578,   197,   198,   199,     0,
       0,   579,  2439,   580,     0,     0,   581,     0,   200,   582,
     583,   584,  1333,     0,     0,   585,     0,   586,     0,     0,
       0,   201,   293,   202,     0,     0,   587,     0,     0,   658,
     203,     0,     0,   658,   204,     0,     0,  1813,  2737,  2737,
       0,     0,     0,     0,   205,     0,   658,     0,  1059,     0,
       0,     0,   206,     0,     0,     0,     0,   207,     0,     0,
       0,     0,     0,   588,     0,     0,     0,     0,     0,     0,
     208,     0,   738,     0,  1060,     0,     0,     0,     0,   209,
       0,     0,     0,   589,   210,   211,     0,     0,     0,     0,
       0,   212,     0,     0,   213,     0,   658,     0,     0,     0,
       0,     0,     0,     0,   214,  1704,     0,  1705,  1706,     0,
       0,     0,     0,     0,     0,   590,     0,     0,     0,     0,
       0,  1189,  1814,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   216,     0,     0,     0,     0,     0,  1815,   591,   217,
       0,     0,     0,     0,   218,     0,  1816,     0,     0,     0,
       0,     0,     0,  1146,  1146,     0,     0,     0,   293,     0,
       0,     0,   658,   658,   658,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   293,   219,   293,   293,     0,     0,
       0,     0,   293,     0,     0,     0,   592,     0,     0,     0,
       0,   738,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   593,   594,     0,     0,     0,     0,     0,     0,     0,
     636,     0,     0,     0,     0,     0,     0,  1817,     0,     0,
       0,  1146,     0,     0,     0,     0,     0,     0,     0,     0,
    1061,     0,  1062,     0,     0,     0,     0,     0,     0,     0,
       0,  1063,  1064,   596,     0,  1065,  1066,     0,     0,     0,
       0,   628,   628,     0,   597,     0,     0,     0,   658,     0,
       0,     0,     0,   293,   293,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   598,     0,   293,     0,
       0,   599,     0,     0,  2925,     0,     0,  2925,     0,     0,
     600,     0,     0,   291,     0,     0,  2925,  2925,  2925,  2933,
     601,     0,     0,     0,     0,     0,     0,     0,     0,   738,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   636,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   293,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   636,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   293,   293,     0,     0,     0,     0,     0,     0,
       0,   293,     0,     0,     0,     0,   293,   293,     0,     0,
       0,     0,   628,     0,   628,     0,   636,     0,   162,     0,
     163,     0,   164,     0,  1333,     0,     0,   165,     0,     0,
       0,     0,     0,     0,     0,   166,     0,   658,   628,   293,
       0,     0,     0,     0,     0,     0,     0,  3045,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   628,
       0,     0,     0,     0,     0,     0,   293,     0,   293,     0,
       0,     0,     0,     0,   167,   168,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   169,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   658,   170,   658,     0,     0,   171,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     172,     0,     0,  1146,     0,     0,     0,     0,     0,     0,
       0,  1146,     0,     0,  -896,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   173,     0,     0,   658,     0,   658,
     174,     0,   175,     0,     0,   176,     0,   177,     0,     0,
     293,     0,     0,     0,     0,     0,     0,   178,  -896,     0,
    -896,  -896,  -896,  -896,  -896,  -896,  -896,  -896,  -896,     0,
    -896,  -896,  -896,     0,  -896,  -896,  -896,  -896,  -896,  -896,
    -896,  -896,  -896,  -896,   179,     0,     0,     0,     0,     0,
    -896,     0,     0,     0,     0,  -896,     0,   180,     0,  -896,
       0,     0,   181,     0,     0,   738,     0,     0,   738,  1277,
     628,   738,     0,     0, -2333, -2333, -2333, -2333,     0,     0,
   -2333,     0,     0,     0,     0,   658,     0, -2333, -2333,     0,
       0,     0,  1146,     0,   182,     0,     0,     0,     0,     0,
       0,   183,     0,     0,   184,   185,     0,     0,     0,     0,
       0,     0,     0,     0,     0, -2202, -2333,     0, -2202,   186,
       0,     0,     0,     0,     0,     0,     0,   187,     0,   188,
       0,     0,   189,     0,     0,     0,     0,  -896,     0,     0,
       0,     0,     0,     0, -2333,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   738,     0,
       0,     0,   738,     0,   162,     0,   163,     0,   164,     0,
       0,     0,     0,   165,     0,     0,     0,     0,     0,     0,
       0,   166,   190,     0,     0,     0,     0,     0,   191,     0,
     192,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   193,     0,     0,  -896,     0,     0,     0,     0,  -896,
    -896,  -896,     0,  -896,  -896,  -896,  -896,     0,     0,     0,
     167,   168,     0,     0, -2333,     0,     0,     0,     0,     0,
     194,     0,     0,     0,     0,   169,     0,     0,     0,     0,
       0,     0,     0,     0,   195,     0,     0,     0,   170,     0,
       0,     0,   171,     0,     0,     0,     0,     0,     0,     0,
   -2333,     0,     0,     0,     0,     0,   172,   658,     0,   658,
       0,     0,   196,     0,     0,     0,     0,   197,   198,   199,
       0,     0,     0,     0, -2333,     0,     0,     0,     0,   200,
     173,     0,     0,     0,     0,     0,   174,     0,   175,     0,
       0,   176,   201,   177,   202,     0,     0,   658,     0,     0,
       0,   203,     0,   178,     0,   204,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   205,     0,     0,     0,     0,
       0,     0, -2202,   206,     0,     0,     0,     0,   207,     0,
     179,     0,     0,     0,     0,     0, -2333,     0,     0,  -896,
       0,   208,     0,   180,     0,     0,     0,  1280,   181,     0,
     209,     0,     0,     0,     0,   210,   211,   658,     0,     0,
       0,     0,   212,     0,     0,   213,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   214,     0,     0, -2333,     0,
     182,     0,     0,     0,     0,     0,     0,   183,     0,     0,
     184,   185,  -896,     0,     0,     0,     0,     0,     0, -2333,
       0,     0,     0,  -896,     0,   186,     0,     0,     0,     0,
       0,     0,   216,   187, -2202,   188,     0,     0,   189,     0,
     217,     0,     0,     0,     0,   218,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -896,     0,
       0,     0,     0,     0,     0,     0,   219,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0, -2333,   190, -2333,
   -2333,     0,     0,     0,   191,     0,   192,     0,     0,     0,
       0,     0,     0,     0, -2333,     0,     0,   193,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   567, -2333,
       0,   568,   341,     0,   342,     0,   194,     0, -2333,   343,
       0,     0,     0,     0,     0,     0,     0,   344,     0,     0,
     195,     0,     0, -1186,     0,     0,     0,     0, -1186, -1186,
   -1186, -1186,     0,     0, -1186,     0,     0,     0,     0,     0,
       0, -1186, -1186,     0,     0,     0,     0,     0,   196,     0,
     658,     0,   658,   197,   198,   199,   345,   346,     0,     0,
       0,     0,     0,     0,     0,   200,     0,     0,     0, -1186,
   -1186,   347, -1186,     0,     0,     0,     0,     0,   201, -2333,
     202,     0,     0,     0,   348,     0,     0,   203,   349,   569,
    1393,   204,     0,     0,     0,     0,     0,     0, -1186,     0,
       0,   205,   350,     0,     0,     0,   571,     0,     0,   206,
       0,     0,   877,     0,   207,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   351,   208,     0,     0,
       0,     0,     0,     0,     0,     0,   209,   352,     0,   353,
       0,   210,   211,     0,     0,     0,     0,     0,   212,     0,
     354,   213,     0,   355,   356,   357,   358,   359,   360,   361,
     362,   214,   363,   364,   365,     0,   366,   367,   368,   369,
     370,   371,   372,   373,   374,   375,   376,     0, -1186,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   377,
       0,     0,     0,     0,   378,     0,     0,     0,   216,     0,
       0,     0,     0,     0,     0,     0,   217,     0,     0,     0,
       0,   218,     0,     0, -1186,     0,   572,   573,   574,     0,
       0,     0,     0,     0,     0,   575,   379,     0,     0,     0,
       0,     0,     0,   380,     0,     0,   381,   382, -1186,   576,
       0,     0,   219,     0,     0,     0,     0,     0,     0,     0,
       0,   383,     0,     0,     0,     0,     0,   569,     0,   384,
       0,   385,     0,     0,   386,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   571,     0,     0,     0,   577,   578,
       0,     0,     0,     0,     0,   579, -1186,   580,     0,     0,
     581,     0,     0,   582,   583,   584,     0,     0,     0,   585,
   -1186,   586,     0,     0,     0,     0,     0,     0,     0,     0,
     587, -1186,     0,     0,   387,     0,     0,     0,     0,     0,
     388,     0,   389,     0,     0,     0,     0,     0,     0,     0,
       0,   567,     0,   390,   568,     0,     0, -2245, -2245, -2245,
       0,     0, -1186,     0,     0,  1056,     0,   588,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   391, -1186,     0,     0,     0,   589,     0,     0,
       0,     0,     0,     0,     0,     0,   392,     0, -1186,     0,
       0,     0,     0,     0,   572,   573,   574,     0,     0,     0,
       0,     0,     0,   575,     0,     0, -2245,     0,     0,   590,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   393,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   394,   569,     0,     0,     0,     0,     0,     0,     0,
       0, -1186,   591, -1186, -1186,     0,   395,     0,     0,   571,
       0,     0,     0,   396,     0,     0,   577,   397, -1186,     0,
       0,     0,     0,     0,     0,     0,     0,   398,   827,     0,
       0,   582,   583,   584,     0,     0,     0,   585,     0,     0,
     399,     0,     0, -1186,     0,     0,     0,     0,     0,     0,
     592,     0, -1186,   400,     0,     0,     0, -1094,     0,     0,
   -1094,     0,   401,     0,     0,   593,   594,   402,   403,     0,
       0,     0,     0,     0,   404,     0,     0,   405,     0,     0,
   -2245,     0,     0,     0,     0,   588,     0,   406,     0,     0,
       0,  1057,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   407,     0,   922,   596,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   597,   572,
     573,   574,     0, -1186,   408,     0,     0,     0,   575,     0,
       0,     0,   409,     0, -1186, -2245, -2245,   410,     0,     0,
     598,     0,   576,     0,     0,   599,     0,     0, -1094,     0,
       0,     0,     0, -2210,   600,     0, -1186,   291,     0,     0,
       0,     0,     0,     0,   601, -1094,  1058,     0,   411,     0,
     591,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   577,   578, -2245, -2245,     0,     0,     0,   579,     0,
     580,     0,     0,   581,     0,     0,   582,   583,   584,     0,
       0,     0,   585,     0,   586,     0,     0,     0,     0,     0,
       0,     0,     0,   587,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   567, -2245,     0,   568,
       0,     0,     0,   593,   594,  1059,     0,     0,     0,     0,
   -2245,     0,     0,     0,     0,     0,     0,     0, -2245,     0,
     588,     0,     0,     0,     0, -2245,     0,     0,     0,     0,
       0,  1060,     0,     0,     0,     0,     0,     0,     0,     0,
     589,     0,     0,     0,     0, -1094, -1094, -1094,     0,     0,
       0,     0, -2245,     0, -1094,     0,   597,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0, -1094,     0,
       0,     0,   590,     0,     0,     0,     0,     0,   598,     0,
       0,     0,     0,   599,     0,     0,     0,   569,     0,     0,
       0,     0,   600,     0,     0,   291,     0,     0,     0,     0,
       0,     0,     0,     0,   571,   591,     0, -1094, -1094,     0,
       0,     0,     0,     0, -1094,     0, -1094,     0,     0, -1094,
       0,     0, -1094, -1094, -1094,     0,     0,     0, -1094,     0,
   -1094,     0,     0,     0,     0,     0,     0,     0,     0, -1094,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   592,     0,   567,     0,     0,   568,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   593,   594,
       0,     0,     0,     0,     0,     0, -1094, -2333,     0,     0,
       0, -1094,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0, -1094,  1061,     0,  1062,
       0, -2245,     0,     0,     0, -2245,     0, -2245,  1063,  1064,
     596,     0,  1065,  1066,   572,   573,   574,     0,     0,     0,
       0,   597,     0,   575,     0,   435,     0,     0, -1094,     0,
       0,     0,     0,     0,     0,     0,     0,   576,     0,     0,
   -2210,   567,     0,   598,   568,     0,   569,     0,   599,     0,
       0,     0,     0,     0,     0,     0,     0,   600,     0,     0,
     291, -1094,     0,   571,     0,     0,     0,   601,     0,     0,
       0,     0,     0,     0,     0,     0,   577,   578,     0,     0,
       0,     0,     0,   579,     0,   580,     0,     0,   581,     0,
       0,   582,   583,   584,     0,     0,     0,   585,     0,   586,
       0,     0, -1094,     0,     0,     0,     0,     0,   587, -1094,
       0,     0,     0,     0,     0,     0,     0,   628,   628,     0,
       0,     0,     0,     0, -1094, -1094,  1548,     0,     0, -2333,
       0,     0,   569,     0,     0,     0,     0,     0,     0,   628,
       0,     0,     0,     0,   570,   588,     0,     0,     0,   571,
       0,     0,     0,     0,     0,     0,     0,     0,   628,     0,
       0,     0,     0,     0,     0,   589, -1094,     0,     0,     0,
       0,     0,     0,   572,   573,   574,     0, -1094,     0,     0,
       0,   567,   575,     0,   568, -1094,     0,   628,     0,     0,
       0,     0,     0,     0,     0,     0,   576,   590,     0, -1094,
       0,     0,     0,     0, -1094,     0,     0, -2210,     0,     0,
       0,     0,     0, -1094,     0,     0, -1094,     0,     0,     0,
       0,     0,     0, -1094,     0,     0,     0,     0,   436,     0,
     591,     0,     0,     0,     0,   577,   578,     0,     0,     0,
       0,     0,   579,     0,   580,     0,     0,   581,     0,     0,
     582,   583,   584,     0,     0,     0,   585,     0,   586,   572,
     573,   574,     0,     0,     0,     0,     0,   587,   575,     0,
       0,     0,   569,     0,     0,     0,     0,     0,   592,     0,
       0,     0,   576,     0,   809,     0,     0,     0,     0,   571,
       0,     0,     0,   593,   594,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   588,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   577,   578,     0,   589,     0,     0,     0,   579,     0,
     580,     0,     0,   581,     0,   596,   582,   583,   584,     0,
       0,     0,   585,     0,   586,     0,   597,     0,     0,     0,
       0,     0,     0,   587,     0,     0,   590,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   598,     0,
       0,     0,     0,   599,     0,     0,     0,     0,     0,     0,
     567,     0,   600,   568,   877,   291,     0,     0,     0,   591,
     588,     0,   601,     0,     0,     0,     0,     0,     0,   572,
     573,   574,     0,     0,     0,     0,     0,     0,   575,     0,
     589,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   576,     0,     0,     0,     0,     0,     0,   567,
    1549,     0,   568,     0,     0,     0,     0,   592,     0,     0,
       0,     0,   590,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   593,   594,     0,     0,     0,     0,     0,     0,
       0,   577,   578,     0,     0,     0,     0,     0,   579,     0,
     580,   569,     0,   581,     0,   591,   582,   583,   584,     0,
       0,     0,   585,   739,   586,     0,     0,  1550,   571,     0,
       0,     0,     0,   587,   596,     0,     0,     0,  1551,     0,
       0,     0,     0,     0,     0,   597,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   567,     0,     0,   568,     0,
     569,     0,     0,   592,     0,     0,     0,   598,     0,     0,
     588,     0,   599,     0,     0,     0,     0,   571,   593,   594,
       0,   600,     0,     0,   291,     0,     0,     0,     0,   595,
     589,   601,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     596,     0,   590,     0,     0,     0,     0,     0,     0,     0,
       0,   597,     0,     0,     0,     0,     0,     0,   572,   573,
     574,     0,     0,     0,     0,     0,   569,   575,     0,     0,
       0,     0,     0,   598,     0,   591,     0,     0,   599,     0,
       0,   576,     0,   571,     0,     0,     0,   600,     0,     0,
     291,     0,     0,     0,     0,     0,     0,   601,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   572,   573,   574,
       0,     0,     0,     0,     0,     0,   575,     0,     0,     0,
     577,   578,     0,   592,     0,     0,     0,   579,     0,   580,
     576,     0,   581,     0,     0,   582,   583,   584,   593,   594,
       0,   585,     0,   586,     0,     0,     0,     0,     0,   810,
       0,     0,   587,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   577,
     578,     0,     0,     0,     0,     0,   579,     0,   580,     0,
     596,   581,     0,     0,   582,   583,   584,     0,     0,   588,
     585,   597,   586,   572,   573,   574,     0,     0,     0,     0,
       0,   587,   575,     0,     0,     0,     0,     0,     0,   589,
       0,     0,     0,   598,     0,     0,   576,     0,   599,     0,
       0,     0,     0,   567,     0,     0,   568,   600,     0,     0,
     291,     0,     0,     0,     0,     0,     0,   601,   588,     0,
       0,   590,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   577,   578,     0,   589,     0,
       0,     0,   579,     0,   580,     0,     0,   581,     0,     0,
     582,   583,   584,     0,   591,     0,   585,     0,   586,     0,
       0,     0,     0,     0,     0,     0,     0,   587,     0,     0,
     590,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     567,     0,     0,   568,   569,     0,     0,     0,     0,     0,
       0,     0,   592,   591,   588,     0,     0,     0,     0,   676,
       0,   571,     0,     0,     0,     0,     0,   593,   594,     0,
     567,     0,     0,   568,   589,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   567,
       0,   592,   568,     0,     0,     0,   590,     0,     0,   596,
       0,     0,     0,     0,     0,     0,   593,   594,     0,     0,
     597,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   569,     0,     0,     0,     0,     0,     0,     0,   591,
       0,     0,   598,   961,     0,     0,     0,   599,   571,     0,
       0,     0,     0,     0,     0,     0,   600,     0,   596,   291,
       0,   569,     0,     0,     0,     0,   601,     0,     0,   597,
       0,   572,   573,   574,     0,     0,     0,     0,   571,     0,
     575,     0,     0,     0,     0,     0,     0,   592,     0,     0,
     569,   598,     0,     0,   576,     0,   599,     0,     0,     0,
       0,     0,   593,   594,     0,   600,     0,   571,   291,     0,
       0,     0,     0,     0,  1177,   601,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   577,   578,     0,     0,     0,     0,     0,
     579,     0,   580,     0,   596,   581,     0,     0,   582,   583,
     584,     0,     0,     0,   585,   597,   586,     0,   572,   573,
     574,     0,  1185,     0,     0,   587,     0,   575,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   598,     0,     0,
       0,   576,   599,     0,     0,     0,     0,     0,   572,   573,
     574,   600,     0,     0,   291,     0,     0,   575,     0,     0,
       0,   601,   588,     0,     0,     0,     0,     0,     0,     0,
       0,   576,     0,     0,     0,     0,     0,   572,   573,   574,
     577,   578,   589,     0,     0,     0,   575,   579,     0,   580,
       0,     0,   581,     0,     0,   582,   583,   584,     0,     0,
     576,   585,     0,   586,     0,     0,     0,     0,     0,     0,
     577,   578,   587,     0,   590,     0,     0,   579,     0,   580,
       0,     0,   581,     0,     0,   582,   583,   584,     0,     0,
       0,   585,     0,   586,     0,     0,     0,     0,     0,   577,
     578,     0,   587,     0,     0,     0,   579,   591,   580,   588,
       0,   581,     0,     0,   582,   583,   584,     0,     0,     0,
     585,     0,   586,     0,     0,     0,     0,     0,     0,   589,
       0,   587,     0,     0,     0,     0,     0,     0,     0,   588,
       0,  1340,     0,     0,   568,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   592,     0,     0,     0,   589,
       0,   590,     0,     0,     0,     0,     0,     0,   588,     0,
     593,   594,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   589,     0,
       0,   590,     0,     0,   591,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   596,     0,     0,     0,     0,     0,     0,     0,
     590,     0,     0,   597,   591,     0,     0,     0,     0,     0,
       0,     0,   569,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   592,     0,     0,   598,     0,     0,     0,   571,
     599,     0,     0,   591,     0,     0,     0,   593,   594,   600,
       0,     0,   291,     0,     0,  1951,     0,     0,     0,   601,
       0,     0,   592,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   593,   594,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   596,
       0,   592,     0,     0,     0,     0,     0,     0,     0,     0,
     597,     0,     0,     0,     0,     0,   593,   594,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   596,
       0,     0,   598,     0,     0,     0,     0,   599,     0,     0,
     597,     0,     0,     0,     0,     0,   600,     0,     0,   291,
       0,     0,     0,     0,     0,     0,   601,     0,   596,   572,
     573,   574,   598,     0,     0,     0,     0,   599,   575,   597,
       0,     0,     0,     0,     0,     0,   600,     0,     0,   291,
       0,     0,   576,     0,     0,     0,   601,     0,     0,     0,
       0,   598,     0,     0,     0,     0,   599,     0,     0,     0,
       0,     0,     0,     0,     0,   600,     0,     0,   291,     0,
       0,     0,     0,     0,     0,   601,     0,     0,     0,     0,
       0,   577,   578,     0,     0,     0,     0,     0,   579,     0,
     580,     0,     0,   581,     0,     0,   582,   583,   584,     0,
       0,     0,   585,     0,   586,     0,     0,     0,     0,     0,
       0,     0,     0,   587,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     588,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     589,     0,     0,  2956,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   590,     0,     0,  -478,     0,     0,  -478,  -478,
    -478,  -478,     0,     0,  -478,  -478,  -478,  -478,  -478,  -478,
    -478,  -478,  -478,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   591,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -478,
    -478,     0,  -478,     0,     0,     0,     0,     0,     0,     0,
    -478,     0,  -478,  -478,  -478,  -478,  -478,  -478,  -478,   568,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   592,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   593,   594,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -478,     0,     0,     0,     0,     0,  1099,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     596,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   597,     0,     0,     0,     0,     0,   569,  -478,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   598,   571,     0,  1212,     0,   599,     0,
       0,  -478,  -478,  -478,  -478,  -478,     0,   600,  -478,  -478,
     291,     0,     0,     0,  -478,     0,     0,   601,     0,     0,
    -478,     0,  -478,     0,     0,     0,     0,     0,  -478,     0,
       0,     0,     0,     0,  -478,     0,  -478,     0,  -478,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1211,  -478,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -478,     0,     0,  -478,     0,  -478,     0,
       0,     0,     0,     0,  -478,     0,  -478,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -478,  -478,     0,     0,
    -478,  -478,  -478,  -478,  -478,  -478,  -478,     0,     0,     0,
    -478,     0,     0,  1100,   572,   573,   574,     0,     0,     0,
       0,     0,     0,   575,     0,     0,     0,  -478,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   576,  -478,     0,
    -478,     0,     0,     0,     0,  -478,     0,  -478,  -478,  -478,
    -478,  -478,  -478,  -478,     0,     0,     0,     0,     0,  -478,
       0,  -478,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -478,  -478,  1101,   577,  1102,     0,     0,
       0,     0,     0,  -478,     0,     0,  -478,     0,  1103,     0,
       0,   582,   583,   584,     0,  -478,     0,   585,     0,   586,
       0,     0,     0,  -478,     0,  -478,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   568,     0,
       0,     0,     0,     0,  -478,     0,     0,     0,     0,     0,
       0,  -478,     0,  -478,  -478,   588,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -478,  -478,     0,     0,     0,
       0,  1212,     0,  -478,     0,     0,  -478,  -478,  -478,  -478,
    -478,     0,     0,  -478,  -478,  1099,     0,  -478,     0,  -478,
    -478,  -478,     0,  -478,     0,     0,  1468,     0,     0,     0,
       0,     0,  -478,  -478,     0,     0,     0,   590,     0,     0,
       0,  -478,     0,     0,     0,     0,     0,  -478,     0,     0,
       0,     0,     0,     0,  -478,  -478,   569,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -478,     0,
     591,  -478,  -478,   571,     0,     0,     0,  -478,     0,  -478,
       0,     0,     0,  -478,     0,  1469,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -478,     0,     0,     0,
       0,     0,     0,  -478,     0,     0,     0,  -478,  -478,  -478,
       0,     0,     0,     0, -1396,     0,     0,     0,   592,     0,
       0,  -478,  -478,     0,     0,     0,  -478,     0,  -478,     0,
       0, -1396,     0,   593,   594,     0,     0,  1205,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
   -2333,     0,     0,     0,  -478,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -478,
       0,     0,     0,     0,     0,   596,     0,     0,  -478,     0,
       0,  -478,  1100,   572,   573,   574,   597,     0,     0,     0,
       0,     0,   575,     0,     0,     0,     0,     0,  -478, -1453,
       0,     0,     0,     0,     0,     0,   576,     0,   598,     0,
       0,     0,     0,   599,  -478,     0,     0,     0,     0, -1453,
       0,     0,   600,  1470,   877,   291,     0,     0,     0,     0,
       0,     0,   601,     0,     0,  2265,  2266,  2267,  2268,  2269,
    2270, -1396, -1396, -1396,  1101,   577,  1102,     0,     0,     0,
   -1396,     0,     0,     0,     0,     0,     0,   581,     0,     0,
     582,   583,   584,     0, -1396,     0,   585,     0,   586,     0,
       0,     0,  -478,     0,  -478,  -478,  -478,     0,     0,     0,
    2272,     0,  1497,  1498,  2273,  2274,  2275,  2276,  2277,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1471,
       0,     0,     0, -1396,     0,     0,     0,  -478,     0,     0,
       0,     0,     0,     0,   588, -1396,     0,     0, -1396, -1396,
   -1396,     0,     0,     0, -1396,     0, -1396,     0,     0,     0,
    2278,     0,  -478,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -478,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -478,  -478,  -478,     0,   590,     0,     0,     0,
       0,     0, -1396,     0,     0,     0,  -478,     0,     0,     0,
       0,     0,     0,  -478,     0,     0,     0,     0,     0,     0,
       0,     0,  1205,     0,     0,     0,     0,     0,     0,   591,
       0,  2279,  2280,  2281,  2282,  2283,     0,     0,  1506,  1507,
       0,     0,     0,     0,     0,  2263,     0,     0,     0,     0,
       0,     0,     0,     0, -1396,     0,     0,     0,     0,   569,
       0,     0,     0,     0,  2264,     0,  2284,  2265,  2266,  2267,
    2268,  2269,  2270,  2271,     0,     0,   571,   592,     0,  2285,
       0,     0,     0,     0,     0,     0,     0, -1396,     0,     0,
       0,     0,   593,   594,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0, -2333,
       0,     0,  2272,     0,  1497,  1498,  2273,  2274,  2275,  2276,
    2277,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   596, -1396,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   597,     0,  2287,     0,     0,
   -1396, -1396,     0,     0,     0,     0,     0,     0, -1453,     0,
       0,     0,  2278,     0,     0,     0,     0,   598,     0,     0,
       0,     0,   599,     0,     0,     0,     0,     0, -1453,     0,
       0,   600,     0,   877,   291,     0,   572,   573,   574,     0,
       0,   601, -1396,     0,  2289,   575,     0,     0,     0,     0,
       0,  1472,     0, -1396,     0,     0,  2291,     0,  1277,   576,
       0,     0,     0, -2333, -2333, -2333, -2333,     0,     0, -2333,
       0,     0,     0,  2292,     0, -1396, -2333, -2333,     0,     0,
   -1396,  2123,     0,  2279,  2280,  2281,  2282,  2283,     0, -1396,
    1506,  1507, -1396,     0,     0,     0,     0,     0,   577, -1396,
       0,     0,     0,     0, -2202, -2333,     0, -2202,     0,     0,
     581,     0,     0,   582,   583,   584,     0,     0,  2284,   585,
       0,   586,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  2285,     0, -2333,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   114,     0,     0,  2286,  2295,
    2296,  2297,     0,     0,     0,     0, -2305,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   588,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2287,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0, -2333,     0,     0,     0,     0,     0,   590,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  2288,     0,     0,     0,     0,     0,  2299,  2300,  2301,
       0,     0,     0,     0,     0,     0,  2289,     0,     0, -2333,
       0,     0,   591,     0,     0,  2290,     0,     0,  2291,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  2124,     0, -2333,     0,  2292,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  2293,     0,     0,     0,     0,     0,     0,     0,     0,
     592,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   593,   594,     0,     0,     0,
       0, -2202,     0,  2125,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0, -2333,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2294,
       0,  2295,  2296,  2297,     0,     0,     0,   596,     0,     0,
       0,     0,     0, -2333,     0,     0,     0,     0,   597,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  2298,     0,     0,     0,     0,     0,
     598,     0,     0,     0,     0,   599,     0,     0, -2333,     0,
       0,     0,     0,     0,   600,     0,     0,   291,     0,  -475,
       0,     0,  2126, -2202,   601,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0, -2305,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2299,
    2300,  2301,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  2302,     0,     0,     0,     0,     0,     0,
    1978,     0,     0,     0,     0,     0, -2333,     0, -2333, -2333,
       0,     0,     0,     0,     0,  3087,     0,     0,  3088,  3089,
    3090,  3091,     0, -2333,  3092,  2265,  2266,  2267,  2268,  2269,
    2270,  3093,  3094,     0,     0,     0,     0,     0,     0,     0,
    2127,  1681,     0,     0,  1682,  1683,  1684,  1685, -2333,     0,
    1686,     0,     0,     0,     0,     0,     0, -2333,  1687,  1673,
    3095,     0,  1674,     0,     0,     0,     0,     0,     0,     0,
    2272,     0,  1497,  1498,  2273,  2274,  2275,  2276,  2277,     0,
       0,     0,     0,     0,     0,     0,  1688,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  2128,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1689,     0,     0,     0,     0,     0,
    2278,     0,     0,     0,     0,     0,     0,     0, -2333,     0,
       0,     0,     0,  1690,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   877,     0,     0,     0,     0,     0,     0,  3096,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  2279,  2280,  2281,  2282,  2283,     0,   569,  1506,  1507,
       0,     0,     0,     0,  3097,     0,     0,     0,     0,     0,
    3098,     0,  3099,     0,   571,     0,     0,     0, -2245,     0,
       0,     0,     0,     0,  3100,     0,  2284,     0,  3101,     0,
    1691,     0,     0,     0,     0,     0,     0,     0,  1692,  2285,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   114,  1693,     0,  2286,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  3102,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  3103,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    3104,     0,  1694,     0,     0,     0,     0,     0,   569,     0,
       0,     0,     0,     0,     0,     0,     0,  2287,     0,     0,
       0,     0,     0,     0,  1695,   571,  1696,     0,  3105,     0,
    1698,     0,     0,     0,   572,   573,   574,     0,     0,     0,
       0,     0,     0,   575,     0,     0,     0,     0,     0,  3106,
       0,  1699,     0,     0,  1697,     0,  1698,   576,     0,     0,
       0,     0,     0,  3107,  2289,     0,     0,     0,     0,     0,
       0,     0,     0,  2290,     0,     0,  2291,  1699,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1700,
       0,     0,     0,  2292,     0,  3108,   577,   615,     0,     0,
       0,     0,     0,   579,     0,   580,     0,     0,   581,     0,
       0,   582,   583,   584,     0,     0,     0,   585,     0,   586,
       0,  1701,  1702,     0,  3109,     0,     0,     0,     0,     0,
       0,  1704,     0,  1705,  1706,   572,   573,   574,     0,     0,
       0,     0,     0,     0,   575,     0,     0,     0,     0,     0,
    1703,     0,     0,  3110,     0,     0,     0,  1704,   576,  1705,
    1706,     0,     0,     0,     0,   588,     0,  2294,     0,  2295,
    2296,  2297,   569,  3111,  1707,     0,     0,     0,     0,  1708,
       0,     0,  3112,     0,     0,   589,     0,     0,     0,   571,
       0,     0,     0,     0,     0,     0,     0,   577,   615,  1709,
       0,     0,     0,     0,   579,  3113,   580,     0,  1710,   581,
       0,     0,   582,   583,   584,     0,     0,   590,   585,     0,
     586,     0,  3114,     0,     0,     0,     0,  -782,     0,     0,
       0,  1711,     0,  3115,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1712,     0,     0,     0,     0,
     591,     0,     0,  3116,     0,     0,     0,  2299,  2300,  2301,
       0,     0,     0,     0,     0,     0,   588,     0,     0,     0,
       0,  2302,     0,     0,     0,     0,  3117,     0,  1978,  1713,
       0,     0,     0,     0,     0,     0,   589,     0,     0,  1714,
       0,     0,     0,     0,     0,  1715,     0,     0,   592,   572,
     573,   574,     0,     0,     0,     0,     0,     0,   575,     0,
       0,     0,     0,   593,   594,     0,     0,     0,   590,     0,
       0,     0,   576,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   569,     0,     0,     0,     0,     0,     0,
       0,     0,  1321,     0,     0,     0,     0,     0,     0,     0,
     571,   591,     0,   616,     0,   596,     0,   617,   618,     0,
       0,   577,   615,     0,     0,     0,   597,     0,   579,     0,
     580,     0,     0,   581,     0,     0,   582,   583,   584,     0,
       0,     0,   585,     0,   586,     0,     0,     0,   598,     0,
       0,     0,     0,   599,     0,     0,     0,     0,     0,   592,
       0,     0,   600,     0,     0,   291,     0,     0,     0,     0,
       0,     0,   601,     0,   593,   594,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     588,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1947,     0,     0,     0,     0,     0,     0,
     589,     0,     0,     0,   616,     0,   596,     0,   617,   618,
     572,   573,   574,     0,     0,     0,     0,   597,     0,   575,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   590,   576,     0,     0,     0,     0,     0,   598,
       0,     0,     0,     0,   599,     0,     0,   569,     0,     0,
       0,     0,     0,   600,     0,     0,   291,     0,     0,     0,
       0,     0,     0,   601,   571,   591,     0,     0,     0,     0,
       0,     0,   577,   615,     0,     0,     0,     0,     0,   579,
       0,   580,     0,     0,   581,     0,     0,   582,   583,   584,
       0,     0,     0,   585,     0,   586,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   569,     0,     0,     0,     0,
       0,     0,     0,   592,     0,     0,     0,     0,     0,     0,
       0,     0,   571,     0,     0,     0,     0,     0,   593,   594,
       0,     0,     0,     0,   569,     0,     0,  2058,     0,     0,
       0,   588,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   571,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   589,     0,     0,     0,     0,     0,     0,   616,     0,
     596,     0,   617,   618,   572,   573,   574,     0,     0,     0,
       0,   597,     0,   575,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   590,     0,     0,     0,   576,     0,     0,
       0,     0,     0,   598,     0,     0,     0,     0,   599,     0,
       0,     0,     0,     0,     0,     0,     0,   600,     0,     0,
     291,     0,     0,     0,     0,     0,   591,   601,     0,     0,
       0,     0,   572,   573,   574,     0,   577,   615,     0,     0,
       0,   575,     0,   579,     0,   580,     0,     0,   581,     0,
       0,   582,   583,   584,     0,   576,     0,   585,     0,   586,
       0,   572,   573,   574,     0,     0,     0,     0,   569,     0,
     575,     0,     0,     0,   592,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   576,   571,     0,     0,     0,   593,
     594,     0,     0,     0,   577,   615,     0,     0,  2060,     0,
       0,   579,     0,   580,     0,   588,   581,     0,     0,   582,
     583,   584,     0,     0,     0,   585,     0,   586,     0,     0,
       0,     0,     0,   577,   615,   589,     0,     0,     0,   616,
     579,   596,   580,   617,   618,   581,     0,     0,   582,   583,
     584,     0,   597,     0,   585,     0,   586,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   590,     0,     0,
       0,     0,     0,   588,   598,     0,     0,     0,     0,   599,
       0,     0,     0,     0,     0,     0,     0,     0,   600,     0,
       0,   291,     0,   589,     0,     0,     0,     0,   601,     0,
     591,     0,   588,     0,     0,   572,   573,   574,     0,     0,
       0,     0,     0,     0,   575,     0,     0,     0,     0,     0,
       0,     0,   589,     0,     0,   590,     0,     0,   576,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   592,     0,
       0,     0,     0,     0,   590,   569,     0,     0,   591,     0,
       0,     0,     0,   593,   594,     0,     0,   577,   615,     0,
       0,     0,   571,     0,   579,     0,   580,     0,     0,   581,
       0,     0,   582,   583,   584,     0,     0,   591,   585,     0,
     586,     0,  2081,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   616,     0,   596,   592,   617,   618,     0,
       0,     0,     0,     0,     0,     0,   597,     0,     0,     0,
       0,   593,   594,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   592,   588,     0,   598,     0,
       0,     0,     0,   599,     0,     0,     0,     0,     0,     0,
     593,   594,   600,     0,     0,   291,   589,     0,     0,     0,
       0,   616,   601,   596,     0,   617,   618,     0,     0,     0,
       0,     0,     0,     0,   597,     0,     0,     0,     0,     0,
       0,     0,   572,   573,   574,     0,     0,     0,   590,     0,
       0,   575,   596,     0,   617,     0,   598,     0,     0,     0,
       0,   599,     0,   597,     0,   576,     0,     0,     0,     0,
     600,     0,     0,   291,     0,  3420,     0,     0,     0,     0,
     601,   591,     0,     0,     0,   598,     0,     0,     0,     0,
     599,     0,     0,     0,     0,     0,     0,     0,     0,   600,
       0,     0,   291,     0,   577,   615,     0,     0,     0,   601,
       0,   579,     0,   580,     0,     0,   581,     0,     0,   582,
     583,   584,     0,     0,     0,   585,     0,   586,     0,   592,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   593,   594,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   588,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   596,     0,     0,     0,
       0,     0,     0,   589,     0,     0,     0,   597,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   598,
       0,     0,     0,     0,   599,   590,     0,     0,     0,     0,
       0,     0,     0,   600,     0,     0,   291,     0,     0,     0,
       0,     0,     0,   601,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   591,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   592,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   593,   594,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   596,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   597,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   598,     0,     0,     0,
       0,   599,     0,     0,     0,     0,     0,     0,     0,     0,
     600,     0,     0,   291,     0,     0,     0,     0,     0,     0,
     601
};

static const yytype_int16 yycheck[] =
{
     127,   426,   568,   476,   283,    55,    56,    57,   650,   753,
    1450,   442,    62,    52,  1522,   457,   837,   838,   839,    58,
      44,  1288,   505,  1179,   422,   434,  1978,   661,  1733,   800,
    1378,   430,   791,  1676,  1991,  1984,  1679,  1519,   417,   657,
    1196,   417,   115,  1748,   117,  1230,   578,  1546,   933,   122,
     443,   534,   418,  1984,   961,  1609,  1971,   776,   312,  1984,
     110,   111,   595,   113,  1534,   458,  1591,   432,   118,   119,
    1565,  2166,  1901,  1855,  1051,   440,   957,   127,   147,   440,
     657,  2718,  1106,   615,  1029,     1,   967,  2359,  2360,  2011,
     426,     1,   457,     1,  1822,     9,   457,    17,     9,    22,
      66,    84,  1098,  2343,    55,   417,  2826,     9,     9,     0,
    1055,   142,    17,     6,     9,  2130,    68,    32,  2033,  2034,
    2035,    52,   110,  2138,   101,  2040,  2041,  2042,    55,  1608,
     442,   765,  1373,    55,   130,     9,    28,   473,  1134,  1046,
     153,   203,   421,   197,   423,    32,     9,  2808,   118,   111,
     112,  1036,   153,     0,  2902,   142,   204,   143,   205,  1564,
     107,   738,   165,  1036,   122,    33,   649,  1408,   480,   144,
    1102,   450,   427,    31,   453,   102,   235,   936,   796,    68,
     459,   182,   437,   184,     1,   803,     1,   249,  1095,   444,
     445,   235,   519,     6,  2109,   111,  1894,  2112,   102,   221,
     479,   271,   595,   185,  2119,    68,   276,  2122,   271,   263,
     382,   271,   340,   255,     6,  2681,   289,   472,   272,   796,
    3033,    22,    21,    55,     1,  2024,   283,   148,    68,   384,
      28,   152,     7,   232,   348,   329,   330,   287,   617,   131,
     616,   617,   618,   726,  2905,  1486,   231,  1960,   419,  1954,
    1955,   143,   360,  2546,   192,   728,   294,  2358,   187,   456,
     305,    31,   349,   356,   314,   631,   578,  2782,   189,  2935,
     753,   162,    28,   472,   255,   166,   315,   810,  1185,    86,
    1291,   331,  1630,   271,   298,    28,  2808,   125,  1299,   220,
     270,   460,  3292,     6,   271,   228,   285,    74,   539,    73,
     334,    77,   298,   615,   616,   617,   618,    77,   402,   147,
      80,     8,  2978,   118,     6,   162,   370,   800,     6,   166,
     539,    76,   753,   530,    84,   569,    68,   104,   105,   142,
     210,  2603,   486,   131,   220,   290,   291,   255,   233,  1580,
     813,   232,   179,   206,    41,   589,    55,   683,   271,   604,
     715,   205,   538,   271,   595,   389,   318,     9,   249,   245,
     251,   726,  1081,   139,   353,   298,   535,   347,   647,   383,
       6,  3371,  3038,   481,   267,   131,   595,   584,  2893,  2161,
     659,  2163,   536,  2905,   298,   232,   367,    21,   131,  1007,
     447,   198,   192,   185,   487,   482,   595,  1282,   964,  1907,
     399,   598,   249,   448,   251,   591,   938,  1902,   346,    67,
     339,  2704,    70,   398,   231,   586,   454,   810,    76,   282,
     148,   786,   594,   802,   152,   786,   802,   153,   793,   391,
     298,   320,   546,   408,   247,   267,    84,   692,  1345,   594,
      84,   753,  1419,  1324,   263,  1422,    68,  1632,   341,    84,
     933,  2164,   707,  2919,   196,   247,   564,  1099,   125,   236,
     258,   189,   336,   557,   776,   515,   998,   360,   419,   361,
     271,  1190,   535,    84,   602,  2051,   536,   527,   253,   254,
     427,   523,   465,    84,   739,   298,  1120,   179,   547,  3237,
     802,   204,   419,  2733,   271,   470,   309,   419,  2196,   549,
     275,   551,  3315,   547,   543,   544,   298,   322,   399,   992,
    1455,  1396,    42,   535,    84,   419,   438,   309,  1759,   261,
      50,  3182,   535,  1396,   294,   595,   514,  1932,   177,   530,
     305,  2010,   535,   595,   247,   415,   410,   417,  1021,   570,
     420,   595,  1160,  2465,   424,   800,   426,   595,   595,   532,
    2049,   806,   399,  1036,   536,   247,   526,   812,  1490,   247,
    3197,   336,   442,   961,   891,   941,   942,   943,   515,  1101,
    1102,  3208,   951,   952,   953,   951,   952,   953,   530,  1298,
       9,   398,   595,   570,   464,   298,   965,   488,   280,   965,
    2389,  1536,   968,  1589,   407,   247,   309,   374,   478,   595,
     480,   977,   978,   979,   980,   981,   298,    29,   594,   416,
     298,   247,   589,   535,  1097,   407,   386,   309,   584,   550,
     525,   309,  2723,   481,  2639,  1004,   318,    56,  1004,   941,
     942,   943,   547,   595,  1393,   586,   594,   595,   453,   951,
     952,   953,   954,  2574,   535,   957,   298,  3367,  1046,  2574,
    2479,  1050,   545,   965,   312,   967,   968,   309,  1141,   586,
     547,   436,   298,   594,   586,   977,   978,   979,   980,   981,
    1826,   591,   595,   309,   595,   488,  2458,   989,     6,   595,
    2920,   595,   586,  1166,   595,   595,  2414,   595,   535,  1025,
     570,   554,  1004,   595,   407,  1126,   488,  1095,  1340,  2971,
     595,   602,   594,   543,   595,   465,   961,   596,   548,  1075,
     419,   481,  1723,  1089,   469,   407,   246,   530,   535,   407,
     535,   526,   496,   145,  1669,  1343,  1671,  1092,  2253,   438,
    1009,  1092,   545,   571,   541,   369,   616,   617,   618,   538,
     398,   595,  2664,    74,   536,  1769,   546,    43,   595,   398,
     549,  3388,  2192,   545,   284,   407,  1475,   570,   535,   886,
     149,   641,  2244,   537,  2246,   584,  1343,   493,   276,  1081,
     298,   407,   532,   104,   105,   488,  1520,  1089,   308,   192,
    1035,  1313,   595,   543,   132,   665,  1151,  1185,   548,   602,
    1045,  1046,   534,   415,  1173,  2968,   488,  1173,  2352,  1282,
     488,   543,   544,   595,  1337,   463,   548,  1752,  1753,   235,
     602,   469,  1067,   852,  1126,   590,  2057,   465,   247,   594,
     149,   465,  2398,   282,  1543,   247,   535,   896,   117,   898,
     465,   523,   545,   129,   884,   282,   488,  1782,  1783,   187,
    1095,   371,   892,  1728,   125,  3033,  2675,   360,   235,   298,
    1827,    67,   488,   545,   465,   522,   285,   545,   287,  2954,
      76,  1173,  2344,   530,   465,  1746,   594,   595,   907,   298,
     920,   370,   400,   753,    31,   294,   298,   586,  1190,    31,
     309,   594,   595,  1366,   532,  1830,   298,   309,   532,   602,
     298,  1836,   298,   545,   530,   465,   776,   532,  2139,  1153,
    2141,  1156,    54,   595,   538,  2404,   328,   595,    34,   545,
     602,   441,  1167,  1396,   602,   549,  1823,   153,  1825,   247,
      77,   532,   802,    80,     1,    77,   271,   298,    80,   809,
    1185,   532,   149,   346,   336,   285,  3033,   595,  2179,  1884,
     271,   235,    33,   595,  1337,    35,   595,  1345,   305,   221,
     602,   153,  1897,  1898,   235,   294,   416,   185,  1490,   595,
     333,   476,   532,    31,  1871,    67,   602,   248,    67,  3033,
     298,   298,  3033,    77,    76,   481,    80,    76,   407,   336,
     276,   309,   298,   298,  2479,   407,  1298,  2143,   360,   300,
     360,   298,   298,  1358,   201,  1361,  2237,   380,  2239,   275,
    3030,   144,    27,  2528,   449,  2530,   451,   198,   273,  2479,
     255,   456,  1324,     0,   220,   298,  1548,   322,  2544,  1790,
     340,   349,  1431,   318,   316,   121,   305,   400,  2951,   305,
    2512,   561,   285,   528,   125,   454,   531,  1520,   134,   245,
     476,  3071,   541,   374,   364,   236,  1579,  1456,   337,   564,
    1957,   438,  1535,   336,   122,  2581,  1465,   336,   564,   488,
     336,   941,   942,   943,  2618,  2988,   488,   595,   194,   491,
     492,   951,   952,   953,   954,  3248,   370,   957,   146,   407,
     316,   541,   595,   472,  3033,   965,   594,   967,   968,  1520,
     298,   480,   244,  1576,   141,   554,   312,   977,   978,   979,
     980,   981,  3033,  1586,  1546,   531,  1233,   554,  3033,   989,
     318,   991,   340,   358,  1164,   454,   545,   345,   490,  1484,
    1485,  3043,  3079,  1484,  1004,   193,  1845,  3315,   564,   153,
    1385,   481,     1,   546,     0,  2634,   117,   294,   200,  1757,
     342,   384,   294,   472,   296,   207,   595,   495,   453,   532,
     495,   480,   424,   221,   482,   298,   233,   438,  1524,   253,
     488,   509,   319,  1475,  1476,   141,   374,  1217,   236,  2054,
     462,   519,   591,   602,   470,    44,    45,    46,    47,    48,
     602,   275,   398,   595,   310,    99,  1579,   595,   365,   595,
     294,   268,   594,   110,  1573,  1574,   602,  1573,  1574,   464,
     511,  1081,  2387,   228,   535,     9,   595,   298,  1520,  1089,
     312,   305,  1971,   312,   219,   141,    20,   545,  3315,  1751,
    1270,    90,    91,  3180,   595,   595,   671,    11,   481,   386,
    1280,  1543,  2714,   385,   386,   530,   535,   594,  1680,   329,
     330,   331,   336,   126,   594,  1964,  1126,   463,   529,   476,
     457,  3315,    56,   469,  3315,   472,  2201,  2202,  1138,   495,
    1140,  1573,  1574,   480,  2763,   275,   595,   595,   595,  2214,
    2215,   582,  3187,   509,   602,  3190,   281,   558,  3193,   595,
     595,   517,   386,   519,   595,    69,  2231,  2232,   595,   595,
     318,   535,   316,  1173,   528,   305,   398,  1177,  1553,   398,
    1554,  1555,  1556,  1557,  1558,  1559,  1560,  1561,   536,   314,
    1190,   231,   402,   547,   590,   298,    12,    13,    14,   535,
     539,   473,  1577,     8,  1374,   208,   336,   111,   153,   113,
     398,   115,  1763,  2681,   144,  2106,   258,   564,   255,   123,
     209,   210,   211,   212,   213,   214,   215,   216,   217,   349,
     317,   594,   595,   358,   232,  1734,    41,  1736,  1734,   595,
    1736,   463,   769,  1788,   463,  3280,  3315,   469,  3325,  3284,
     469,   249,   294,   251,  2746,    71,     0,   529,   220,   595,
     460,  1760,  2881,   249,  1760,   251,  2894,   595,   198,   476,
     452,   476,   196,  1876,   602,  2636,   279,   305,  3355,   316,
     298,   183,  1781,   245,   110,  1781,   535,   191,   219,  2650,
    2651,  1894,  3364,  1844,   247,   572,   383,   400,  1298,  1546,
     322,  2577,  1734,   263,  1736,  1823,   833,  1825,   336,  1828,
     313,  3383,   272,   535,  1746,  1315,   535,   594,   206,  2680,
       1,   358,   594,   247,  1324,   270,   264,   153,  1760,   305,
      12,   356,   535,    15,    16,  1582,   376,   377,  2898,   535,
    3103,   551,   552,  2835,  2836,   298,   494,   557,   496,  1781,
     281,   495,    76,  1871,  2914,  2915,   309,   564,   398,   564,
     336,   285,   482,   287,   459,   509,  1969,  3130,   270,   185,
      51,   413,  2963,   517,   298,   519,   590,   559,     8,   421,
     236,   506,   400,   314,  1869,   309,  2451,  1906,   276,   537,
     418,   413,   280,   415,    75,   384,   300,   342,   523,   413,
    1400,   399,   347,   585,  1651,   587,   431,   247,  2999,  1960,
     370,    41,   401,  1845,   138,   271,    23,    24,   162,   305,
     460,  1853,   166,   543,   240,   241,   188,  1597,   548,   197,
     192,  2899,  1602,   528,  1604,   263,   117,  1607,  1608,  1957,
    1610,   343,   366,  1942,   272,   347,  1942,  3007,   543,   353,
     336,  2516,  2517,   548,   407,  3015,   543,   249,   298,   251,
     590,   548,   487,   588,   594,  1624,   240,   241,   593,   309,
     484,   485,   288,   289,   247,  1475,  1476,  1876,   305,   543,
     483,  2973,  1652,   407,   548,  2047,  2048,  2049,   232,  2051,
     316,   538,   358,     9,   298,   263,    12,   535,   187,    15,
      16,   534,   549,   192,   272,   249,   141,   251,  3299,   336,
    1942,   544,  1682,   271,   288,   289,   332,   198,  1688,  2410,
    1520,  1691,  2284,  1523,  3315,   298,   351,   352,  1528,   345,
     396,   335,  1964,  1533,   562,   488,   309,   353,   206,  1709,
     298,   188,  1542,  1543,   360,   192,  1921,    64,    65,  1742,
     553,   455,   220,  2048,   420,   236,   442,   517,  2120,  2121,
      70,   538,   460,    73,   488,  3390,   594,   407,   535,  1944,
    1945,   387,   549,  1573,  1574,   506,   460,   245,  1578,   383,
     406,   533,   344,   129,   346,  1585,  1586,   104,   134,   202,
     271,   543,   523,  2196,  3369,  3370,   548,   278,    65,  2353,
     406,   359,   370,   361,   121,   509,   595,   359,   532,   361,
     534,   292,   280,  2164,    37,    38,  3391,   134,   128,   543,
     544,   545,   132,   547,   548,  2120,  2121,  3376,   495,  1799,
    2159,   535,   579,   580,   407,  3410,  1806,   104,  3387,  1809,
     419,  2293,   589,   325,   326,  1815,  3033,  1791,   488,   602,
     429,   395,    11,   533,   121,   399,   593,   588,   595,     9,
     486,    51,   593,   543,  3439,  2427,   475,   134,   548,   495,
      20,   353,   354,   923,   500,   501,   502,   927,   602,     9,
     486,   298,  2546,   509,    33,    75,   271,    11,   635,   475,
    2966,   595,  1862,   519,   500,   501,   502,   535,    73,    74,
    2765,  2766,  2767,   977,   978,   545,    56,  2258,   594,   595,
      69,    11,   532,   535,   110,   488,  2101,  2102,  2103,   532,
     536,    70,    68,   543,   540,    74,   542,   117,   548,  2104,
     543,   356,   413,   414,  1734,   548,  1736,   535,    68,   532,
    3368,  1988,   198,   199,   535,    69,  1746,   428,  1995,   430,
     543,   413,   111,   415,   113,   548,   115,   153,   133,  2362,
    1760,   136,   602,  1763,   123,   487,   540,    97,   542,    69,
     255,   221,   545,   223,  2021,   166,  2327,   941,   942,   943,
     236,  1781,   125,   344,   114,   346,   162,   111,  1788,   113,
     407,   115,   952,   953,   250,  2447,  2859,   535,   535,   123,
    2047,  2048,  2049,   535,   968,  2868,  2869,  2870,   595,   325,
     326,   111,    82,   113,  1984,   115,  2191,   595,   595,    11,
    1990,   431,   413,   123,   415,    27,   294,  1997,   296,   602,
     449,   178,   191,  2697,   451,  1835,  2398,   353,   354,   294,
    2704,   296,  2404,  2002,  1844,  1845,   196,   573,   574,   575,
     576,   467,  2397,  1853,   535,   393,   413,   374,   415,   298,
    3420,   378,   379,   115,   294,   117,   296,   191,   597,   298,
     122,   488,   595,  2120,  2121,  2045,  2046,    69,  2125,   298,
    1880,   271,   554,  2053,  1039,  1040,  1041,  1042,   278,   535,
      79,   191,    81,   319,   224,   225,   226,   247,   573,   574,
     575,   576,   292,   233,   266,   586,   523,   573,   574,   575,
     576,   428,   535,   616,   595,   618,   438,   247,   530,   111,
     316,   113,   595,   115,  2442,   595,  2444,   535,   445,   446,
     595,   123,   547,   460,  1934,   285,    87,   287,   535,  2701,
    1316,   300,  1942,  1319,   525,   535,    11,   468,   298,   469,
     481,  1327,   282,    76,    70,  1331,   286,   595,  2128,   309,
    1960,    11,  1338,   298,  1964,   271,   450,   535,   298,   535,
     154,   301,   302,   303,   316,   228,   300,   307,   595,   309,
     358,   155,  2175,   547,   334,   602,  2385,  2386,  2267,  2268,
    2269,  2270,   547,  2544,   353,  2546,   547,   356,   547,   191,
     300,   979,   980,   981,    69,   547,   547,   547,   547,  2771,
     406,   547,   531,   547,   547,   547,   366,   196,   530,    69,
     156,   449,   157,   413,   414,   355,   158,   413,   105,   353,
    2581,   517,   356,   239,   517,    53,   160,   289,   428,   159,
     430,  2288,  2417,  2418,  2419,  2420,   111,   298,   113,   161,
     115,   522,   116,   353,   547,   530,   164,   407,   123,    55,
     474,   111,  2437,   113,   523,   115,  2236,   167,   228,   168,
     595,   169,  2634,   123,   170,   589,   171,   407,   228,   192,
     318,   394,    32,   172,   547,  2460,  2461,   173,   128,  2259,
     486,   174,   128,   175,   228,   176,   455,  2961,   535,   495,
     460,   304,   535,   595,   500,   501,   502,   431,   300,   364,
     440,   382,   469,   509,  2361,   595,   298,   595,   535,   268,
     535,   122,     9,   519,   535,  1735,   191,  1737,   487,  2732,
     489,   455,  2302,  1743,  1744,  1745,   110,   530,   488,   261,
    1750,   191,   595,   493,   595,   535,  2697,   595,   530,   438,
     509,  1761,  1762,  2704,   397,   455,   235,  2404,   488,   298,
     319,   353,   436,   487,  2164,   489,   418,   192,   593,   346,
     422,   572,   593,   503,   504,   584,  2346,  2722,   591,  2349,
     432,   433,   532,   591,   534,   509,  3181,  2357,   440,   489,
    2190,   443,   151,   543,   544,   545,   586,   547,   548,   595,
     452,  2763,   454,   455,   204,   457,   458,   427,   530,   509,
      97,   594,   244,   525,  2384,   545,   520,   528,   267,   267,
     153,   530,   530,  2622,   554,   530,   556,   114,    55,   481,
      55,   228,   267,   228,   535,   300,   595,   460,   297,   431,
     425,   535,   535,   100,   539,    23,   298,   535,   578,   319,
     300,   398,   602,   583,    86,   481,   508,   364,   124,   526,
     601,    49,   592,   455,  2663,   595,  2436,   600,   276,   324,
     495,   595,   602,   351,   532,   532,  3271,    27,   532,   443,
     530,   424,  2657,   532,   532,     4,   532,   532,   353,    77,
     532,    79,    80,    81,  2669,   595,  3237,   489,   530,   276,
      19,   235,    17,   353,   235,   602,   525,  2900,  2478,   163,
     361,    30,   427,  3308,  3309,   530,    55,   509,   235,  2881,
     143,   535,  3317,   255,   195,   591,  3321,  3322,   591,   359,
     358,   235,   495,   530,     9,  2582,     7,   224,   225,   226,
     535,   460,  2589,   595,   428,   586,   233,   101,    22,   358,
     516,   221,   535,  3348,    51,  3350,   595,    76,   351,    67,
     247,     8,   532,   481,   349,   482,   235,  2526,   336,   347,
     270,   151,    55,   586,   586,   221,   221,   469,   276,   631,
     367,  3376,   362,   369,  2783,  3380,     8,  2634,   228,   342,
     455,  2584,  3387,   153,   427,   384,   366,  2397,   535,   286,
     652,   129,   535,   595,   515,   455,   460,   454,   586,   595,
    2961,   298,   236,   589,   301,   302,   303,   236,    17,   525,
     307,   535,   309,   595,   228,   235,   235,   138,   153,   423,
     117,   423,  2602,     8,  2434,    41,   541,   305,   165,  2609,
    2610,  2611,  2612,   276,   509,   117,   337,  2617,  3134,   305,
     294,   348,   546,   580,   110,   243,    63,   460,   271,   509,
     535,   591,   530,   715,   447,   221,   595,   296,   355,    67,
    2717,   470,   495,    43,   726,   305,   257,   132,    55,   125,
     395,   443,    55,   305,   305,   535,   274,   305,   343,   535,
     183,   535,  2662,   602,   525,    59,   530,   560,    27,   468,
     386,   753,   395,  2503,   470,   299,   294,   228,   296,   528,
     491,  2511,  2759,   298,  2514,   124,  2763,   530,   770,   398,
     407,   122,   471,   538,   569,   409,    25,    26,   595,   346,
     595,   220,   530,   130,   786,   130,     7,  2730,   490,   258,
     429,   793,   530,   261,   535,   595,   334,  3098,    31,   258,
     130,   535,   594,   440,   508,  2748,  2749,   395,   810,   535,
     535,  2754,   361,   358,   591,   132,   595,   357,  3330,   357,
    2740,   276,   276,   285,   535,   294,   276,   276,  2825,   285,
      79,   276,    81,   110,    83,  2755,   305,   285,   276,   517,
     205,   236,   147,   381,    67,   236,   298,   245,  2845,   259,
     388,   488,   138,   243,   228,   595,    55,   366,   374,   591,
      11,   153,   497,    93,  2784,   155,   503,   504,  1252,   118,
     119,   120,    25,   525,    72,    71,   531,   546,   301,   501,
      75,  3176,   872,   303,  2881,  2621,  2938,  2608,  2850,  2616,
     856,  2622,  2844,  3060,  3027,  2601,  2646,  3071,  2574,   368,
     840,  1984,  3033,  3418,   373,  3005,  2970,  2850,   545,  1482,
    3036,  2327,  3404,  3430,  3380,  2912,  3098,  2905,  3096,   556,
    3096,  2671,  3247,  3247,  2145,   558,  1271,   100,  1282,  2054,
     179,  3033,   181,  1352,  2684,  1697,   631,   475,  2088,   188,
    2143,   578,  2769,   192,   413,   641,   583,  2770,   486,   428,
    3245,   992,   421,  1396,   683,   592,  1789,  2137,   595,   961,
      36,    37,    38,    39,   433,   602,    42,  2106,   691,   507,
    1422,  1050,  2722,    49,    50,  1053,  2154,  1827,  2152,  1085,
    1078,  1853,  1473,  2190,   748,  2169,  2168,  1520,  2908,   711,
    1894,  2196,  2935,  1527,  2205,  1139,  2204,   535,  3147,  1969,
     469,  1909,    78,  1945,  1562,   474,  1586,   458,   784,  2242,
    1541,  2988,   786,   806,  2934,   484,  1526,  1586,  1668,  1670,
     558,  2964,  2965,  1032,  1756,  1031,   495,  2777,  2247,  2235,
     106,  1755,  2234,  2674,  2521,  2978,  2979,  2520,  2248,   638,
    1787,   579,  1786,   457,  1046,   294,  1058,   296,   586,  1067,
    1938,   589,  1938,  1425,  1938,  1938,   770,   305,   452,  2447,
    1237,   986,   481,  2813,  2335,  1548,   535,  1763,  3011,   318,
     539,   989,  3098,  1075,  3186,   905,   599,  2579,  1779,  2505,
    2238,  2797,  2126,  2927,  3004,   876,  2635,  3354,  1873,   565,
    1092,  2591,  1569,  1095,   540,  3038,  2008,  3040,  3138,  1606,
    3371,  3021,  2922,    -1,    -1,    -1,    -1,    -1,    -1,  3106,
     186,    -1,  3032,  3033,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  3369,  3370,    -1,  3046,    -1,    -1,  3049,
    3050,  3051,  3052,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2890,    -1,    -1,    -1,    -1,  3391,   222,    -1,    -1,  1151,
      -1,    -1,  3149,    -1,    -1,    -1,    -1,  3077,  3078,    -1,
      -1,    -1,    -1,    -1,  3410,    -1,    -1,    -1,  3088,    -1,
     246,    -1,    -1,    -1,    -1,  3095,    -1,  3097,    -1,    -1,
      -1,    -1,    -1,  1185,    -1,    -1,    -1,  3369,  3370,  3132,
      -1,  3111,    -1,  3439,    -1,  2945,    -1,    -1,    -1,    -1,
      -1,    -1,  3199,    -1,    -1,   454,    -1,    -1,    -1,  3391,
      -1,     6,    -1,    -1,     9,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  3410,    -1,
      -1,    -1,   308,    -1,    -1,  3155,  3156,  3157,  3158,  3159,
      -1,    -1,  3162,  3163,  3164,  3165,  3166,  3167,  3168,  3169,
    3170,  3171,  3172,    -1,    -1,  3175,    -1,  3439,    -1,  3179,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   350,  1277,  3196,   526,  3198,  3029,
    3030,    -1,    -1,    -1,  3204,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   371,    -1,    -1,  3048,    -1,
      -1,    -1,    97,    -1,    -1,    -1,    -1,    60,    61,    62,
      -1,    -1,    -1,  3063,    -1,    -1,    -1,    -1,    -1,   114,
      -1,    -1,    -1,    -1,  3074,    -1,    -1,    -1,    -1,    -1,
    3327,    -1,   581,    -1,    -1,  1337,    -1,    -1,    -1,  1341,
      -1,    -1,    -1,  1345,    -1,    -1,   595,    -1,  3098,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1358,    -1,    -1,  1361,
      -1,    -1,    -1,   439,  3114,   441,   442,  3117,    -1,    -1,
    3290,  3291,    -1,    -1,    -1,    -1,    -1,    -1,  3375,    -1,
     456,    -1,  3379,   136,   137,   138,    -1,  3307,    -1,    -1,
      -1,    -1,  3312,  3313,    -1,    -1,    -1,    -1,  3148,    -1,
      -1,  3151,  3152,  3153,  3154,   481,  1408,    -1,    -1,  3329,
      -1,  3161,  3332,  3333,   490,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   218,  3174,    -1,  3176,    -1,    -1,   224,
     225,   226,    -1,    -1,    -1,    -1,  3186,    -1,   233,    -1,
      -1,    -1,    -1,  3440,    -1,    -1,  3366,    -1,    -1,    -1,
      -1,    -1,   247,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   256,    -1,  3213,  3214,  3215,  3216,  3217,  3218,  3219,
    3220,  3221,  3222,  3223,  3224,  3225,  3226,  3227,  3228,  3229,
    3230,  3231,  1484,  1485,  1486,   561,    -1,    -1,    -1,  3239,
      -1,   286,   287,    -1,    -1,  3245,    -1,    -1,   293,    -1,
     295,    -1,    -1,   298,  3254,    -1,   301,   302,   303,    -1,
     263,   264,   307,    -1,   309,    -1,  3266,    -1,    -1,    -1,
      -1,   316,  1524,   318,   277,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  3287,  3288,    -1,
      31,   294,    33,    -1,  1546,    36,    37,    38,    39,    -1,
      41,    42,    -1,  3303,    -1,    -1,    -1,    -1,   311,    50,
     355,    -1,    -1,    -1,    -1,   360,  3316,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1579,  1580,    -1,
     375,    -1,    -1,    -1,  3334,    -1,    77,    78,    -1,    80,
      -1,    -1,    -1,    -1,    -1,    -1,  1598,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  3356,    -1,    -1,    -1,
      -1,    -1,   407,    -1,    -1,   106,    -1,   370,    -1,  3369,
    3370,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   125,  3385,    -1,   390,   391,   392,
       1,  3391,    -1,     4,    -1,   440,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  3403,  3404,   408,    -1,    -1,    19,    -1,
    3410,    -1,    -1,    -1,    -1,   417,  3416,    -1,    -1,    30,
     422,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1680,    -1,
     432,    -1,    43,    -1,    -1,  3435,    -1,    -1,   440,  3439,
      -1,    -1,    -1,   488,    -1,   490,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   457,    -1,    -1,   503,   504,
      -1,   506,    -1,    -1,    -1,    76,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   222,    -1,    -1,    -1,    -1,    -1,    -1,   229,   230,
    1742,    -1,    -1,    -1,    -1,    -1,   541,    -1,    -1,     1,
     545,     3,    -1,     5,    -1,   246,    -1,  1759,    10,    -1,
      -1,   556,    -1,   516,    -1,    -1,    18,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   578,    -1,    -1,    -1,    -1,   583,    -1,
      -1,    -1,    -1,   284,    -1,    -1,    -1,   592,   593,    -1,
     595,    -1,    -1,   294,    -1,    57,    58,   602,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   306,    -1,   308,    -1,    -1,
      72,  1823,    -1,  1825,    -1,    -1,   578,    -1,   319,    -1,
      -1,    -1,    -1,    85,    -1,    -1,    -1,    89,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   336,    -1,   338,    -1,    -1,
      -1,   103,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   615,   616,   617,   618,  1869,   359,  1871,
      -1,    -1,    -1,    -1,    -1,   127,    -1,    -1,    -1,    -1,
     371,   133,    -1,   135,    -1,    -1,   138,    -1,   140,    -1,
      -1,    -1,    -1,    -1,    -1,   386,    -1,   258,   150,    -1,
     652,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   403,   404,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   177,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   294,    -1,    -1,    -1,    -1,   190,    -1,
      -1,   432,    -1,   195,   305,    -1,    -1,    -1,   439,    -1,
     441,   442,    -1,    -1,    -1,  1957,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   715,    -1,   456,    -1,    -1,    -1,  1971,
     461,    -1,    -1,    -1,   726,   227,    -1,    -1,    -1,    -1,
      -1,    -1,   234,    -1,    -1,   237,   238,    -1,    -1,    -1,
     481,    -1,  1994,    -1,    -1,    -1,    -1,    -1,    -1,   490,
     252,    -1,    -1,    -1,    -1,    -1,    -1,   368,   260,    -1,
     262,    -1,   373,   265,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   513,    -1,   776,    -1,    -1,    -1,    -1,    -1,
      -1,  2033,  2034,  2035,   786,   526,   527,    -1,  2040,  2041,
    2042,   793,    -1,    -1,    -1,  2047,  2048,  2049,    -1,  2051,
     802,    -1,   413,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     421,    -1,    -1,   315,    -1,    -1,    -1,    -1,    -1,   321,
     561,   323,   433,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     571,    -1,   334,    -1,    -1,    -1,   577,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   594,    -1,    -1,    -1,  2109,   469,   470,
    2112,   363,    -1,   474,    -1,    -1,    -1,  2119,  2120,  2121,
    2122,    -1,    -1,   484,    -1,   377,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   495,    -1,    -1,  2139,    -1,  2141,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,     3,
      -1,     5,    -1,   405,    -1,    -1,    10,    -1,   410,   411,
     412,    -1,    -1,    -1,    18,    -1,    -1,    -1,    -1,    -1,
     422,    -1,    -1,  2175,   535,    -1,    -1,  2179,   539,    -1,
      -1,    -1,    -1,   435,    -1,   437,    -1,    -1,    -1,   941,
     942,   943,   444,    -1,    -1,    -1,   448,    -1,    -1,   951,
     952,   953,   954,    57,    58,   957,   458,    -1,    -1,   961,
      -1,    -1,    -1,   965,   466,   967,   968,    -1,    72,   471,
      -1,    -1,    -1,    -1,    -1,   977,   978,   979,   980,   981,
      -1,    85,   484,    -1,    -1,    89,    -1,  2239,    -1,    -1,
      -1,   493,    -1,    -1,    -1,    -1,   498,   499,    -1,   103,
      -1,    -1,  1004,   505,    -1,    -1,   508,    -1,    -1,  2261,
      -1,    -1,    36,    37,    38,    39,   518,    -1,    42,    -1,
      -1,    -1,    -1,   127,     6,    49,    50,     9,    -1,   133,
      -1,   135,    -1,   535,   138,    -1,   140,    -1,    20,    -1,
      -1,    -1,    -1,    -1,  1046,    -1,   150,    -1,    -1,    -1,
      -1,    -1,    -1,   555,    78,    -1,    -1,    -1,    -1,    -1,
      -1,   563,    -1,    -1,    -1,    -1,   568,    -1,    -1,    -1,
      -1,    -1,    -1,   177,    -1,    -1,    -1,    -1,    -1,  1081,
      -1,    -1,   106,    -1,    -1,    -1,   190,  1089,    -1,    -1,
    1092,   195,    -1,  1095,    -1,    -1,    -1,   599,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    97,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   227,    -1,    -1,    -1,    -1,    -1,    -1,
     234,    -1,   114,   237,   238,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2398,    -1,   252,  1151,
      -1,    -1,  2404,    -1,    -1,    -1,   260,    -1,   262,    -1,
      -1,   265,   186,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1173,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1185,    -1,    -1,    -1,    -1,  1190,    -1,
    2442,    -1,  2444,    -1,    -1,    -1,    -1,    -1,   222,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   315,    -1,    -1,   196,    -1,  2468,   321,    -1,   323,
      -1,    -1,   246,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     334,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   224,   225,   226,    -1,    -1,    -1,    -1,    -1,
      -1,   233,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   363,
      -1,    -1,    -1,    -1,    -1,   247,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   377,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   308,    -1,    -1,    -1,    -1,   271,
      -1,    -1,    -1,    -1,    -1,    -1,  1298,    -1,    -1,    -1,
      -1,   405,    -1,    -1,   286,   287,   410,   411,   412,    -1,
      -1,   293,   336,   295,    -1,    -1,   298,    -1,   422,   301,
     302,   303,  1324,    -1,    -1,   307,    -1,   309,    -1,    -1,
      -1,   435,  2584,   437,    -1,    -1,   318,    -1,    -1,  1341,
     444,    -1,    -1,  1345,   448,    -1,    -1,   371,  2600,  2601,
      -1,    -1,    -1,    -1,   458,    -1,  1358,    -1,   340,    -1,
      -1,    -1,   466,    -1,    -1,    -1,    -1,   471,    -1,    -1,
      -1,    -1,    -1,   355,    -1,    -1,    -1,    -1,    -1,    -1,
     484,    -1,  2634,    -1,   366,    -1,    -1,    -1,    -1,   493,
      -1,    -1,    -1,   375,   498,   499,    -1,    -1,    -1,    -1,
      -1,   505,    -1,    -1,   508,    -1,  1408,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   518,   439,    -1,   441,   442,    -1,
      -1,    -1,    -1,    -1,    -1,   407,    -1,    -1,    -1,    -1,
      -1,   535,   456,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   555,    -1,    -1,    -1,    -1,    -1,   481,   440,   563,
      -1,    -1,    -1,    -1,   568,    -1,   490,    -1,    -1,    -1,
      -1,    -1,    -1,  1475,  1476,    -1,    -1,    -1,  2730,    -1,
      -1,    -1,  1484,  1485,  1486,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2746,   599,  2748,  2749,    -1,    -1,
      -1,    -1,  2754,    -1,    -1,    -1,   488,    -1,    -1,    -1,
      -1,  2763,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   503,   504,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2782,    -1,    -1,    -1,    -1,    -1,    -1,   561,    -1,    -1,
      -1,  1543,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     532,    -1,   534,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   543,   544,   545,    -1,   547,   548,    -1,    -1,    -1,
      -1,  1573,  1574,    -1,   556,    -1,    -1,    -1,  1580,    -1,
      -1,    -1,    -1,  2835,  2836,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   578,    -1,  2850,    -1,
      -1,   583,    -1,    -1,  2856,    -1,    -1,  2859,    -1,    -1,
     592,    -1,    -1,   595,    -1,    -1,  2868,  2869,  2870,  2871,
     602,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2881,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2893,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2935,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2951,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2964,  2965,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2973,    -1,    -1,    -1,    -1,  2978,  2979,    -1,    -1,
      -1,    -1,  1734,    -1,  1736,    -1,  2988,    -1,     1,    -1,
       3,    -1,     5,    -1,  1746,    -1,    -1,    10,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    18,    -1,  1759,  1760,  3011,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  3019,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1781,
      -1,    -1,    -1,    -1,    -1,    -1,  3038,    -1,  3040,    -1,
      -1,    -1,    -1,    -1,    57,    58,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1823,    85,  1825,    -1,    -1,    89,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,    -1,    -1,  1845,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1853,    -1,    -1,   117,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   127,    -1,    -1,  1869,    -1,  1871,
     133,    -1,   135,    -1,    -1,   138,    -1,   140,    -1,    -1,
    3132,    -1,    -1,    -1,    -1,    -1,    -1,   150,   151,    -1,
     153,   154,   155,   156,   157,   158,   159,   160,   161,    -1,
     163,   164,   165,    -1,   167,   168,   169,   170,   171,   172,
     173,   174,   175,   176,   177,    -1,    -1,    -1,    -1,    -1,
     183,    -1,    -1,    -1,    -1,   188,    -1,   190,    -1,   192,
      -1,    -1,   195,    -1,    -1,  3187,    -1,    -1,  3190,    31,
    1942,  3193,    -1,    -1,    36,    37,    38,    39,    -1,    -1,
      42,    -1,    -1,    -1,    -1,  1957,    -1,    49,    50,    -1,
      -1,    -1,  1964,    -1,   227,    -1,    -1,    -1,    -1,    -1,
      -1,   234,    -1,    -1,   237,   238,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    77,    78,    -1,    80,   252,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   260,    -1,   262,
      -1,    -1,   265,    -1,    -1,    -1,    -1,   270,    -1,    -1,
      -1,    -1,    -1,    -1,   106,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  3280,    -1,
      -1,    -1,  3284,    -1,     1,    -1,     3,    -1,     5,    -1,
      -1,    -1,    -1,    10,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    18,   315,    -1,    -1,    -1,    -1,    -1,   321,    -1,
     323,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   334,    -1,    -1,   337,    -1,    -1,    -1,    -1,   342,
     343,   344,    -1,   346,   347,   348,   349,    -1,    -1,    -1,
      57,    58,    -1,    -1,   186,    -1,    -1,    -1,    -1,    -1,
     363,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   377,    -1,    -1,    -1,    85,    -1,
      -1,    -1,    89,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     222,    -1,    -1,    -1,    -1,    -1,   103,  2139,    -1,  2141,
      -1,    -1,   405,    -1,    -1,    -1,    -1,   410,   411,   412,
      -1,    -1,    -1,    -1,   246,    -1,    -1,    -1,    -1,   422,
     127,    -1,    -1,    -1,    -1,    -1,   133,    -1,   135,    -1,
      -1,   138,   435,   140,   437,    -1,    -1,  2179,    -1,    -1,
      -1,   444,    -1,   150,    -1,   448,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   458,    -1,    -1,    -1,    -1,
      -1,    -1,   294,   466,    -1,    -1,    -1,    -1,   471,    -1,
     177,    -1,    -1,    -1,    -1,    -1,   308,    -1,    -1,   482,
      -1,   484,    -1,   190,    -1,    -1,    -1,   319,   195,    -1,
     493,    -1,    -1,    -1,    -1,   498,   499,  2239,    -1,    -1,
      -1,    -1,   505,    -1,    -1,   508,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   518,    -1,    -1,   350,    -1,
     227,    -1,    -1,    -1,    -1,    -1,    -1,   234,    -1,    -1,
     237,   238,   535,    -1,    -1,    -1,    -1,    -1,    -1,   371,
      -1,    -1,    -1,   546,    -1,   252,    -1,    -1,    -1,    -1,
      -1,    -1,   555,   260,   386,   262,    -1,    -1,   265,    -1,
     563,    -1,    -1,    -1,    -1,   568,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   591,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   599,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   439,   315,   441,
     442,    -1,    -1,    -1,   321,    -1,   323,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   456,    -1,    -1,   334,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     6,   481,
      -1,     9,     3,    -1,     5,    -1,   363,    -1,   490,    10,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    18,    -1,    -1,
     377,    -1,    -1,    31,    -1,    -1,    -1,    -1,    36,    37,
      38,    39,    -1,    -1,    42,    -1,    -1,    -1,    -1,    -1,
      -1,    49,    50,    -1,    -1,    -1,    -1,    -1,   405,    -1,
    2442,    -1,  2444,   410,   411,   412,    57,    58,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   422,    -1,    -1,    -1,    77,
      78,    72,    80,    -1,    -1,    -1,    -1,    -1,   435,   561,
     437,    -1,    -1,    -1,    85,    -1,    -1,   444,    89,    97,
     572,   448,    -1,    -1,    -1,    -1,    -1,    -1,   106,    -1,
      -1,   458,   103,    -1,    -1,    -1,   114,    -1,    -1,   466,
      -1,    -1,   594,    -1,   471,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   127,   484,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   493,   138,    -1,   140,
      -1,   498,   499,    -1,    -1,    -1,    -1,    -1,   505,    -1,
     151,   508,    -1,   154,   155,   156,   157,   158,   159,   160,
     161,   518,   163,   164,   165,    -1,   167,   168,   169,   170,
     171,   172,   173,   174,   175,   176,   177,    -1,   186,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   190,
      -1,    -1,    -1,    -1,   195,    -1,    -1,    -1,   555,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   563,    -1,    -1,    -1,
      -1,   568,    -1,    -1,   222,    -1,   224,   225,   226,    -1,
      -1,    -1,    -1,    -1,    -1,   233,   227,    -1,    -1,    -1,
      -1,    -1,    -1,   234,    -1,    -1,   237,   238,   246,   247,
      -1,    -1,   599,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   252,    -1,    -1,    -1,    -1,    -1,    97,    -1,   260,
      -1,   262,    -1,    -1,   265,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   114,    -1,    -1,    -1,   286,   287,
      -1,    -1,    -1,    -1,    -1,   293,   294,   295,    -1,    -1,
     298,    -1,    -1,   301,   302,   303,    -1,    -1,    -1,   307,
     308,   309,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     318,   319,    -1,    -1,   315,    -1,    -1,    -1,    -1,    -1,
     321,    -1,   323,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     6,    -1,   334,     9,    -1,    -1,    12,    13,    14,
      -1,    -1,   350,    -1,    -1,    20,    -1,   355,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   363,   371,    -1,    -1,    -1,   375,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   377,    -1,   386,    -1,
      -1,    -1,    -1,    -1,   224,   225,   226,    -1,    -1,    -1,
      -1,    -1,    -1,   233,    -1,    -1,    71,    -1,    -1,   407,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   410,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   422,    97,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   439,   440,   441,   442,    -1,   437,    -1,    -1,   114,
      -1,    -1,    -1,   444,    -1,    -1,   286,   448,   456,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   458,   298,    -1,
      -1,   301,   302,   303,    -1,    -1,    -1,   307,    -1,    -1,
     471,    -1,    -1,   481,    -1,    -1,    -1,    -1,    -1,    -1,
     488,    -1,   490,   484,    -1,    -1,    -1,     6,    -1,    -1,
       9,    -1,   493,    -1,    -1,   503,   504,   498,   499,    -1,
      -1,    -1,    -1,    -1,   505,    -1,    -1,   508,    -1,    -1,
     185,    -1,    -1,    -1,    -1,   355,    -1,   518,    -1,    -1,
      -1,   196,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   535,    -1,    55,   545,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   556,   224,
     225,   226,    -1,   561,   555,    -1,    -1,    -1,   233,    -1,
      -1,    -1,   563,    -1,   572,   240,   241,   568,    -1,    -1,
     578,    -1,   247,    -1,    -1,   583,    -1,    -1,    97,    -1,
      -1,    -1,    -1,   102,   592,    -1,   594,   595,    -1,    -1,
      -1,    -1,    -1,    -1,   602,   114,   271,    -1,   599,    -1,
     440,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   286,   287,   288,   289,    -1,    -1,    -1,   293,    -1,
     295,    -1,    -1,   298,    -1,    -1,   301,   302,   303,    -1,
      -1,    -1,   307,    -1,   309,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   318,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     6,   332,    -1,     9,
      -1,    -1,    -1,   503,   504,   340,    -1,    -1,    -1,    -1,
     345,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   353,    -1,
     355,    -1,    -1,    -1,    -1,   360,    -1,    -1,    -1,    -1,
      -1,   366,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     375,    -1,    -1,    -1,    -1,   224,   225,   226,    -1,    -1,
      -1,    -1,   387,    -1,   233,    -1,   556,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   247,    -1,
      -1,    -1,   407,    -1,    -1,    -1,    -1,    -1,   578,    -1,
      -1,    -1,    -1,   583,    -1,    -1,    -1,    97,    -1,    -1,
      -1,    -1,   592,    -1,    -1,   595,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   114,   440,    -1,   286,   287,    -1,
      -1,    -1,    -1,    -1,   293,    -1,   295,    -1,    -1,   298,
      -1,    -1,   301,   302,   303,    -1,    -1,    -1,   307,    -1,
     309,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   318,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   488,    -1,     6,    -1,    -1,     9,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   503,   504,
      -1,    -1,    -1,    -1,    -1,    -1,   355,   187,    -1,    -1,
      -1,   360,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   375,   532,    -1,   534,
      -1,   536,    -1,    -1,    -1,   540,    -1,   542,   543,   544,
     545,    -1,   547,   548,   224,   225,   226,    -1,    -1,    -1,
      -1,   556,    -1,   233,    -1,   235,    -1,    -1,   407,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   247,    -1,    -1,
     419,     6,    -1,   578,     9,    -1,    97,    -1,   583,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   592,    -1,    -1,
     595,   440,    -1,   114,    -1,    -1,    -1,   602,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   286,   287,    -1,    -1,
      -1,    -1,    -1,   293,    -1,   295,    -1,    -1,   298,    -1,
      -1,   301,   302,   303,    -1,    -1,    -1,   307,    -1,   309,
      -1,    -1,   481,    -1,    -1,    -1,    -1,    -1,   318,   488,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  3369,  3370,    -1,
      -1,    -1,    -1,    -1,   503,   504,   177,    -1,    -1,   339,
      -1,    -1,    97,    -1,    -1,    -1,    -1,    -1,    -1,  3391,
      -1,    -1,    -1,    -1,   109,   355,    -1,    -1,    -1,   114,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  3410,    -1,
      -1,    -1,    -1,    -1,    -1,   375,   545,    -1,    -1,    -1,
      -1,    -1,    -1,   224,   225,   226,    -1,   556,    -1,    -1,
      -1,     6,   233,    -1,     9,   564,    -1,  3439,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   247,   407,    -1,   578,
      -1,    -1,    -1,    -1,   583,    -1,    -1,   586,    -1,    -1,
      -1,    -1,    -1,   592,    -1,    -1,   595,    -1,    -1,    -1,
      -1,    -1,    -1,   602,    -1,    -1,    -1,    -1,   438,    -1,
     440,    -1,    -1,    -1,    -1,   286,   287,    -1,    -1,    -1,
      -1,    -1,   293,    -1,   295,    -1,    -1,   298,    -1,    -1,
     301,   302,   303,    -1,    -1,    -1,   307,    -1,   309,   224,
     225,   226,    -1,    -1,    -1,    -1,    -1,   318,   233,    -1,
      -1,    -1,    97,    -1,    -1,    -1,    -1,    -1,   488,    -1,
      -1,    -1,   247,    -1,   109,    -1,    -1,    -1,    -1,   114,
      -1,    -1,    -1,   503,   504,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   355,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   286,   287,    -1,   375,    -1,    -1,    -1,   293,    -1,
     295,    -1,    -1,   298,    -1,   545,   301,   302,   303,    -1,
      -1,    -1,   307,    -1,   309,    -1,   556,    -1,    -1,    -1,
      -1,    -1,    -1,   318,    -1,    -1,   407,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   578,    -1,
      -1,    -1,    -1,   583,    -1,    -1,    -1,    -1,    -1,    -1,
       6,    -1,   592,     9,   594,   595,    -1,    -1,    -1,   440,
     355,    -1,   602,    -1,    -1,    -1,    -1,    -1,    -1,   224,
     225,   226,    -1,    -1,    -1,    -1,    -1,    -1,   233,    -1,
     375,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   247,    -1,    -1,    -1,    -1,    -1,    -1,     6,
     481,    -1,     9,    -1,    -1,    -1,    -1,   488,    -1,    -1,
      -1,    -1,   407,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   503,   504,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   286,   287,    -1,    -1,    -1,    -1,    -1,   293,    -1,
     295,    97,    -1,   298,    -1,   440,   301,   302,   303,    -1,
      -1,    -1,   307,   109,   309,    -1,    -1,   538,   114,    -1,
      -1,    -1,    -1,   318,   545,    -1,    -1,    -1,   549,    -1,
      -1,    -1,    -1,    -1,    -1,   556,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     6,    -1,    -1,     9,    -1,
      97,    -1,    -1,   488,    -1,    -1,    -1,   578,    -1,    -1,
     355,    -1,   583,    -1,    -1,    -1,    -1,   114,   503,   504,
      -1,   592,    -1,    -1,   595,    -1,    -1,    -1,    -1,   514,
     375,   602,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     545,    -1,   407,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   556,    -1,    -1,    -1,    -1,    -1,    -1,   224,   225,
     226,    -1,    -1,    -1,    -1,    -1,    97,   233,    -1,    -1,
      -1,    -1,    -1,   578,    -1,   440,    -1,    -1,   583,    -1,
      -1,   247,    -1,   114,    -1,    -1,    -1,   592,    -1,    -1,
     595,    -1,    -1,    -1,    -1,    -1,    -1,   602,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   224,   225,   226,
      -1,    -1,    -1,    -1,    -1,    -1,   233,    -1,    -1,    -1,
     286,   287,    -1,   488,    -1,    -1,    -1,   293,    -1,   295,
     247,    -1,   298,    -1,    -1,   301,   302,   303,   503,   504,
      -1,   307,    -1,   309,    -1,    -1,    -1,    -1,    -1,   514,
      -1,    -1,   318,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   286,
     287,    -1,    -1,    -1,    -1,    -1,   293,    -1,   295,    -1,
     545,   298,    -1,    -1,   301,   302,   303,    -1,    -1,   355,
     307,   556,   309,   224,   225,   226,    -1,    -1,    -1,    -1,
      -1,   318,   233,    -1,    -1,    -1,    -1,    -1,    -1,   375,
      -1,    -1,    -1,   578,    -1,    -1,   247,    -1,   583,    -1,
      -1,    -1,    -1,     6,    -1,    -1,     9,   592,    -1,    -1,
     595,    -1,    -1,    -1,    -1,    -1,    -1,   602,   355,    -1,
      -1,   407,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   286,   287,    -1,   375,    -1,
      -1,    -1,   293,    -1,   295,    -1,    -1,   298,    -1,    -1,
     301,   302,   303,    -1,   440,    -1,   307,    -1,   309,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   318,    -1,    -1,
     407,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       6,    -1,    -1,     9,    97,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   488,   440,   355,    -1,    -1,    -1,    -1,   360,
      -1,   114,    -1,    -1,    -1,    -1,    -1,   503,   504,    -1,
       6,    -1,    -1,     9,   375,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     6,
      -1,   488,     9,    -1,    -1,    -1,   407,    -1,    -1,   545,
      -1,    -1,    -1,    -1,    -1,    -1,   503,   504,    -1,    -1,
     556,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    97,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   440,
      -1,    -1,   578,   530,    -1,    -1,    -1,   583,   114,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   592,    -1,   545,   595,
      -1,    97,    -1,    -1,    -1,    -1,   602,    -1,    -1,   556,
      -1,   224,   225,   226,    -1,    -1,    -1,    -1,   114,    -1,
     233,    -1,    -1,    -1,    -1,    -1,    -1,   488,    -1,    -1,
      97,   578,    -1,    -1,   247,    -1,   583,    -1,    -1,    -1,
      -1,    -1,   503,   504,    -1,   592,    -1,   114,   595,    -1,
      -1,    -1,    -1,    -1,   267,   602,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   286,   287,    -1,    -1,    -1,    -1,    -1,
     293,    -1,   295,    -1,   545,   298,    -1,    -1,   301,   302,
     303,    -1,    -1,    -1,   307,   556,   309,    -1,   224,   225,
     226,    -1,   228,    -1,    -1,   318,    -1,   233,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   578,    -1,    -1,
      -1,   247,   583,    -1,    -1,    -1,    -1,    -1,   224,   225,
     226,   592,    -1,    -1,   595,    -1,    -1,   233,    -1,    -1,
      -1,   602,   355,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   247,    -1,    -1,    -1,    -1,    -1,   224,   225,   226,
     286,   287,   375,    -1,    -1,    -1,   233,   293,    -1,   295,
      -1,    -1,   298,    -1,    -1,   301,   302,   303,    -1,    -1,
     247,   307,    -1,   309,    -1,    -1,    -1,    -1,    -1,    -1,
     286,   287,   318,    -1,   407,    -1,    -1,   293,    -1,   295,
      -1,    -1,   298,    -1,    -1,   301,   302,   303,    -1,    -1,
      -1,   307,    -1,   309,    -1,    -1,    -1,    -1,    -1,   286,
     287,    -1,   318,    -1,    -1,    -1,   293,   440,   295,   355,
      -1,   298,    -1,    -1,   301,   302,   303,    -1,    -1,    -1,
     307,    -1,   309,    -1,    -1,    -1,    -1,    -1,    -1,   375,
      -1,   318,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   355,
      -1,     6,    -1,    -1,     9,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   488,    -1,    -1,    -1,   375,
      -1,   407,    -1,    -1,    -1,    -1,    -1,    -1,   355,    -1,
     503,   504,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   375,    -1,
      -1,   407,    -1,    -1,   440,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   545,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     407,    -1,    -1,   556,   440,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    97,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   488,    -1,    -1,   578,    -1,    -1,    -1,   114,
     583,    -1,    -1,   440,    -1,    -1,    -1,   503,   504,   592,
      -1,    -1,   595,    -1,    -1,   481,    -1,    -1,    -1,   602,
      -1,    -1,   488,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   503,   504,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   545,
      -1,   488,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     556,    -1,    -1,    -1,    -1,    -1,   503,   504,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   545,
      -1,    -1,   578,    -1,    -1,    -1,    -1,   583,    -1,    -1,
     556,    -1,    -1,    -1,    -1,    -1,   592,    -1,    -1,   595,
      -1,    -1,    -1,    -1,    -1,    -1,   602,    -1,   545,   224,
     225,   226,   578,    -1,    -1,    -1,    -1,   583,   233,   556,
      -1,    -1,    -1,    -1,    -1,    -1,   592,    -1,    -1,   595,
      -1,    -1,   247,    -1,    -1,    -1,   602,    -1,    -1,    -1,
      -1,   578,    -1,    -1,    -1,    -1,   583,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   592,    -1,    -1,   595,    -1,
      -1,    -1,    -1,    -1,    -1,   602,    -1,    -1,    -1,    -1,
      -1,   286,   287,    -1,    -1,    -1,    -1,    -1,   293,    -1,
     295,    -1,    -1,   298,    -1,    -1,   301,   302,   303,    -1,
      -1,    -1,   307,    -1,   309,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   318,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     355,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     375,    -1,    -1,     1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   407,    -1,    -1,    33,    -1,    -1,    36,    37,
      38,    39,    -1,    -1,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   440,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    77,
      78,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      88,    -1,    90,    91,    92,    93,    94,    95,    96,     9,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   488,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   503,   504,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     138,    -1,    -1,    -1,    -1,    -1,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     545,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   556,    -1,    -1,    -1,    -1,    -1,    97,   186,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   578,   114,    -1,   204,    -1,   583,    -1,
      -1,   209,   210,   211,   212,   213,    -1,   592,   216,   217,
     595,    -1,    -1,    -1,   222,    -1,    -1,   602,    -1,    -1,
     228,    -1,   230,    -1,    -1,    -1,    -1,    -1,   236,    -1,
      -1,    -1,    -1,    -1,   242,    -1,   244,    -1,   246,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,   257,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   271,    -1,    -1,   274,    -1,    21,    -1,
      -1,    -1,    -1,    -1,   282,    -1,   284,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   294,    40,    -1,    -1,
      43,    44,    45,    46,    47,    48,    49,    -1,    -1,    -1,
     308,    -1,    -1,   223,   224,   225,   226,    -1,    -1,    -1,
      -1,    -1,    -1,   233,    -1,    -1,    -1,   325,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   247,   336,    -1,
     338,    -1,    -1,    -1,    -1,    88,    -1,    90,    91,    92,
      93,    94,    95,    96,    -1,    -1,    -1,    -1,    -1,   357,
      -1,   359,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   371,   372,   285,   286,   287,    -1,    -1,
      -1,    -1,    -1,   381,    -1,    -1,   384,    -1,   298,    -1,
      -1,   301,   302,   303,    -1,   138,    -1,   307,    -1,   309,
      -1,    -1,    -1,   401,    -1,   403,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     9,    -1,
      -1,    -1,    -1,    -1,   432,    -1,    -1,    -1,    -1,    -1,
      -1,   439,    -1,   441,   442,   355,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   198,   199,    -1,    -1,    -1,
      -1,   204,    -1,   461,    -1,    -1,   209,   210,   211,   212,
     213,    -1,    -1,   216,   217,    56,    -1,   475,    -1,   477,
     478,   479,    -1,   481,    -1,    -1,     9,    -1,    -1,    -1,
      -1,    -1,   490,   236,    -1,    -1,    -1,   407,    -1,    -1,
      -1,   244,    -1,    -1,    -1,    -1,    -1,   250,    -1,    -1,
      -1,    -1,    -1,    -1,   257,   513,    97,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   271,    -1,
     440,   274,   530,   114,    -1,    -1,    -1,   535,    -1,   282,
      -1,    -1,    -1,   541,    -1,    68,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   554,    -1,    -1,    -1,
      -1,    -1,    -1,   561,    -1,    -1,    -1,   565,   566,   567,
      -1,    -1,    -1,    -1,    97,    -1,    -1,    -1,   488,    -1,
      -1,   579,   325,    -1,    -1,    -1,   584,    -1,   586,    -1,
      -1,   114,    -1,   503,   504,    -1,    -1,   595,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     520,    -1,    -1,    -1,   357,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   372,
      -1,    -1,    -1,    -1,    -1,   545,    -1,    -1,   381,    -1,
      -1,   384,   223,   224,   225,   226,   556,    -1,    -1,    -1,
      -1,    -1,   233,    -1,    -1,    -1,    -1,    -1,   401,   569,
      -1,    -1,    -1,    -1,    -1,    -1,   247,    -1,   578,    -1,
      -1,    -1,    -1,   583,   417,    -1,    -1,    -1,    -1,   589,
      -1,    -1,   592,   206,   594,   595,    -1,    -1,    -1,    -1,
      -1,    -1,   602,    -1,    -1,    43,    44,    45,    46,    47,
      48,   224,   225,   226,   285,   286,   287,    -1,    -1,    -1,
     233,    -1,    -1,    -1,    -1,    -1,    -1,   298,    -1,    -1,
     301,   302,   303,    -1,   247,    -1,   307,    -1,   309,    -1,
      -1,    -1,   475,    -1,   477,   478,   479,    -1,    -1,    -1,
      88,    -1,    90,    91,    92,    93,    94,    95,    96,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   282,
      -1,    -1,    -1,   286,    -1,    -1,    -1,   510,    -1,    -1,
      -1,    -1,    -1,    -1,   355,   298,    -1,    -1,   301,   302,
     303,    -1,    -1,    -1,   307,    -1,   309,    -1,    -1,    -1,
     138,    -1,   535,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   554,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   565,   566,   567,    -1,   407,    -1,    -1,    -1,
      -1,    -1,   355,    -1,    -1,    -1,   579,    -1,    -1,    -1,
      -1,    -1,    -1,   586,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   595,    -1,    -1,    -1,    -1,    -1,    -1,   440,
      -1,   209,   210,   211,   212,   213,    -1,    -1,   216,   217,
      -1,    -1,    -1,    -1,    -1,    21,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   407,    -1,    -1,    -1,    -1,    97,
      -1,    -1,    -1,    -1,    40,    -1,   244,    43,    44,    45,
      46,    47,    48,    49,    -1,    -1,   114,   488,    -1,   257,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   440,    -1,    -1,
      -1,    -1,   503,   504,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   520,
      -1,    -1,    88,    -1,    90,    91,    92,    93,    94,    95,
      96,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   545,   488,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   556,    -1,   325,    -1,    -1,
     503,   504,    -1,    -1,    -1,    -1,    -1,    -1,   569,    -1,
      -1,    -1,   138,    -1,    -1,    -1,    -1,   578,    -1,    -1,
      -1,    -1,   583,    -1,    -1,    -1,    -1,    -1,   589,    -1,
      -1,   592,    -1,   594,   595,    -1,   224,   225,   226,    -1,
      -1,   602,   545,    -1,   372,   233,    -1,    -1,    -1,    -1,
      -1,   554,    -1,   556,    -1,    -1,   384,    -1,    31,   247,
      -1,    -1,    -1,    36,    37,    38,    39,    -1,    -1,    42,
      -1,    -1,    -1,   401,    -1,   578,    49,    50,    -1,    -1,
     583,    54,    -1,   209,   210,   211,   212,   213,    -1,   592,
     216,   217,   595,    -1,    -1,    -1,    -1,    -1,   286,   602,
      -1,    -1,    -1,    -1,    77,    78,    -1,    80,    -1,    -1,
     298,    -1,    -1,   301,   302,   303,    -1,    -1,   244,   307,
      -1,   309,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   257,    -1,   106,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   271,    -1,    -1,   274,   477,
     478,   479,    -1,    -1,    -1,    -1,   282,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   355,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   325,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   186,    -1,    -1,    -1,    -1,    -1,   407,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   357,    -1,    -1,    -1,    -1,    -1,   565,   566,   567,
      -1,    -1,    -1,    -1,    -1,    -1,   372,    -1,    -1,   222,
      -1,    -1,   440,    -1,    -1,   381,    -1,    -1,   384,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   244,    -1,   246,    -1,   401,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   417,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     488,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   503,   504,    -1,    -1,    -1,
      -1,   294,    -1,   296,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   308,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   475,
      -1,   477,   478,   479,    -1,    -1,    -1,   545,    -1,    -1,
      -1,    -1,    -1,   336,    -1,    -1,    -1,    -1,   556,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   510,    -1,    -1,    -1,    -1,    -1,
     578,    -1,    -1,    -1,    -1,   583,    -1,    -1,   371,    -1,
      -1,    -1,    -1,    -1,   592,    -1,    -1,   595,    -1,   535,
      -1,    -1,   385,   386,   602,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   554,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   565,
     566,   567,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   579,    -1,    -1,    -1,    -1,    -1,    -1,
     586,    -1,    -1,    -1,    -1,    -1,   439,    -1,   441,   442,
      -1,    -1,    -1,    -1,    -1,    33,    -1,    -1,    36,    37,
      38,    39,    -1,   456,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     473,    33,    -1,    -1,    36,    37,    38,    39,   481,    -1,
      42,    -1,    -1,    -1,    -1,    -1,    -1,   490,    50,    77,
      78,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      88,    -1,    90,    91,    92,    93,    94,    95,    96,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    78,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   529,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   106,    -1,    -1,    -1,    -1,    -1,
     138,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   561,    -1,
      -1,    -1,    -1,   125,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   594,    -1,    -1,    -1,    -1,    -1,    -1,   186,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   209,   210,   211,   212,   213,    -1,    97,   216,   217,
      -1,    -1,    -1,    -1,   222,    -1,    -1,    -1,    -1,    -1,
     228,    -1,   230,    -1,   114,    -1,    -1,    -1,   236,    -1,
      -1,    -1,    -1,    -1,   242,    -1,   244,    -1,   246,    -1,
     222,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   230,   257,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   271,   246,    -1,   274,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   284,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   294,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     308,    -1,   284,    -1,    -1,    -1,    -1,    -1,    97,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   325,    -1,    -1,
      -1,    -1,    -1,    -1,   306,   114,   308,    -1,   336,    -1,
     338,    -1,    -1,    -1,   224,   225,   226,    -1,    -1,    -1,
      -1,    -1,    -1,   233,    -1,    -1,    -1,    -1,    -1,   357,
      -1,   359,    -1,    -1,   336,    -1,   338,   247,    -1,    -1,
      -1,    -1,    -1,   371,   372,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   381,    -1,    -1,   384,   359,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   371,
      -1,    -1,    -1,   401,    -1,   403,   286,   287,    -1,    -1,
      -1,    -1,    -1,   293,    -1,   295,    -1,    -1,   298,    -1,
      -1,   301,   302,   303,    -1,    -1,    -1,   307,    -1,   309,
      -1,   403,   404,    -1,   432,    -1,    -1,    -1,    -1,    -1,
      -1,   439,    -1,   441,   442,   224,   225,   226,    -1,    -1,
      -1,    -1,    -1,    -1,   233,    -1,    -1,    -1,    -1,    -1,
     432,    -1,    -1,   461,    -1,    -1,    -1,   439,   247,   441,
     442,    -1,    -1,    -1,    -1,   355,    -1,   475,    -1,   477,
     478,   479,    97,   481,   456,    -1,    -1,    -1,    -1,   461,
      -1,    -1,   490,    -1,    -1,   375,    -1,    -1,    -1,   114,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   286,   287,   481,
      -1,    -1,    -1,    -1,   293,   513,   295,    -1,   490,   298,
      -1,    -1,   301,   302,   303,    -1,    -1,   407,   307,    -1,
     309,    -1,   530,    -1,    -1,    -1,    -1,   535,    -1,    -1,
      -1,   513,    -1,   541,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   527,    -1,    -1,    -1,    -1,
     440,    -1,    -1,   561,    -1,    -1,    -1,   565,   566,   567,
      -1,    -1,    -1,    -1,    -1,    -1,   355,    -1,    -1,    -1,
      -1,   579,    -1,    -1,    -1,    -1,   584,    -1,   586,   561,
      -1,    -1,    -1,    -1,    -1,    -1,   375,    -1,    -1,   571,
      -1,    -1,    -1,    -1,    -1,   577,    -1,    -1,   488,   224,
     225,   226,    -1,    -1,    -1,    -1,    -1,    -1,   233,    -1,
      -1,    -1,    -1,   503,   504,    -1,    -1,    -1,   407,    -1,
      -1,    -1,   247,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    97,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   532,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     114,   440,    -1,   543,    -1,   545,    -1,   547,   548,    -1,
      -1,   286,   287,    -1,    -1,    -1,   556,    -1,   293,    -1,
     295,    -1,    -1,   298,    -1,    -1,   301,   302,   303,    -1,
      -1,    -1,   307,    -1,   309,    -1,    -1,    -1,   578,    -1,
      -1,    -1,    -1,   583,    -1,    -1,    -1,    -1,    -1,   488,
      -1,    -1,   592,    -1,    -1,   595,    -1,    -1,    -1,    -1,
      -1,    -1,   602,    -1,   503,   504,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     355,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   532,    -1,    -1,    -1,    -1,    -1,    -1,
     375,    -1,    -1,    -1,   543,    -1,   545,    -1,   547,   548,
     224,   225,   226,    -1,    -1,    -1,    -1,   556,    -1,   233,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   407,   247,    -1,    -1,    -1,    -1,    -1,   578,
      -1,    -1,    -1,    -1,   583,    -1,    -1,    97,    -1,    -1,
      -1,    -1,    -1,   592,    -1,    -1,   595,    -1,    -1,    -1,
      -1,    -1,    -1,   602,   114,   440,    -1,    -1,    -1,    -1,
      -1,    -1,   286,   287,    -1,    -1,    -1,    -1,    -1,   293,
      -1,   295,    -1,    -1,   298,    -1,    -1,   301,   302,   303,
      -1,    -1,    -1,   307,    -1,   309,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    97,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   488,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   114,    -1,    -1,    -1,    -1,    -1,   503,   504,
      -1,    -1,    -1,    -1,    97,    -1,    -1,   512,    -1,    -1,
      -1,   355,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   375,    -1,    -1,    -1,    -1,    -1,    -1,   543,    -1,
     545,    -1,   547,   548,   224,   225,   226,    -1,    -1,    -1,
      -1,   556,    -1,   233,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   407,    -1,    -1,    -1,   247,    -1,    -1,
      -1,    -1,    -1,   578,    -1,    -1,    -1,    -1,   583,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   592,    -1,    -1,
     595,    -1,    -1,    -1,    -1,    -1,   440,   602,    -1,    -1,
      -1,    -1,   224,   225,   226,    -1,   286,   287,    -1,    -1,
      -1,   233,    -1,   293,    -1,   295,    -1,    -1,   298,    -1,
      -1,   301,   302,   303,    -1,   247,    -1,   307,    -1,   309,
      -1,   224,   225,   226,    -1,    -1,    -1,    -1,    97,    -1,
     233,    -1,    -1,    -1,   488,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   247,   114,    -1,    -1,    -1,   503,
     504,    -1,    -1,    -1,   286,   287,    -1,    -1,   512,    -1,
      -1,   293,    -1,   295,    -1,   355,   298,    -1,    -1,   301,
     302,   303,    -1,    -1,    -1,   307,    -1,   309,    -1,    -1,
      -1,    -1,    -1,   286,   287,   375,    -1,    -1,    -1,   543,
     293,   545,   295,   547,   548,   298,    -1,    -1,   301,   302,
     303,    -1,   556,    -1,   307,    -1,   309,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   407,    -1,    -1,
      -1,    -1,    -1,   355,   578,    -1,    -1,    -1,    -1,   583,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   592,    -1,
      -1,   595,    -1,   375,    -1,    -1,    -1,    -1,   602,    -1,
     440,    -1,   355,    -1,    -1,   224,   225,   226,    -1,    -1,
      -1,    -1,    -1,    -1,   233,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   375,    -1,    -1,   407,    -1,    -1,   247,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   488,    -1,
      -1,    -1,    -1,    -1,   407,    97,    -1,    -1,   440,    -1,
      -1,    -1,    -1,   503,   504,    -1,    -1,   286,   287,    -1,
      -1,    -1,   114,    -1,   293,    -1,   295,    -1,    -1,   298,
      -1,    -1,   301,   302,   303,    -1,    -1,   440,   307,    -1,
     309,    -1,   532,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   543,    -1,   545,   488,   547,   548,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   556,    -1,    -1,    -1,
      -1,   503,   504,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   488,   355,    -1,   578,    -1,
      -1,    -1,    -1,   583,    -1,    -1,    -1,    -1,    -1,    -1,
     503,   504,   592,    -1,    -1,   595,   375,    -1,    -1,    -1,
      -1,   543,   602,   545,    -1,   547,   548,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   556,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   224,   225,   226,    -1,    -1,    -1,   407,    -1,
      -1,   233,   545,    -1,   547,    -1,   578,    -1,    -1,    -1,
      -1,   583,    -1,   556,    -1,   247,    -1,    -1,    -1,    -1,
     592,    -1,    -1,   595,    -1,   434,    -1,    -1,    -1,    -1,
     602,   440,    -1,    -1,    -1,   578,    -1,    -1,    -1,    -1,
     583,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   592,
      -1,    -1,   595,    -1,   286,   287,    -1,    -1,    -1,   602,
      -1,   293,    -1,   295,    -1,    -1,   298,    -1,    -1,   301,
     302,   303,    -1,    -1,    -1,   307,    -1,   309,    -1,   488,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   503,   504,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   355,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   545,    -1,    -1,    -1,
      -1,    -1,    -1,   375,    -1,    -1,    -1,   556,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   578,
      -1,    -1,    -1,    -1,   583,   407,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   592,    -1,    -1,   595,    -1,    -1,    -1,
      -1,    -1,    -1,   602,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   440,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   488,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   503,   504,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   545,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   556,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   578,    -1,    -1,    -1,
      -1,   583,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     592,    -1,    -1,   595,    -1,    -1,    -1,    -1,    -1,    -1,
     602
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint16 yystos[] =
{
       0,   605,   606,     0,   607,   608,   609,   612,   613,   249,
     251,   610,   611,   614,   615,   621,   622,   179,   620,   642,
     643,   611,   232,   399,   623,   626,   141,   141,   117,   765,
     767,    99,   644,   648,   627,   624,   365,   635,   635,   535,
     535,   141,   395,   960,   963,   539,   768,   460,   264,   714,
     715,    11,    69,   111,   113,   115,   123,   191,   300,   353,
     356,   431,   455,   487,   489,   509,   595,   645,   646,   647,
     650,   651,   655,   667,   674,   675,   677,   678,   679,   680,
     686,   695,   697,   702,   705,   706,   708,   709,   710,   711,
     712,   713,   535,   535,   535,   620,   620,   535,   141,   987,
     988,   460,   769,   535,   460,   202,   716,     1,   595,  1465,
    1465,   495,   475,  1592,   271,  1569,  1569,  1569,  1465,   475,
     535,   535,  1569,   535,   535,    68,  1554,   681,   667,   674,
     655,   651,   650,   674,   650,   666,   667,   676,   677,   678,
     535,   679,   255,   696,   298,   400,   628,   628,   125,   636,
     637,   166,   616,   617,   618,   162,   619,   318,   494,   496,
     537,  1029,     1,     3,     5,    10,    18,    57,    58,    72,
      85,    89,   103,   127,   133,   135,   138,   140,   150,   177,
     190,   195,   227,   234,   237,   238,   252,   260,   262,   265,
     315,   321,   323,   334,   363,   377,   405,   410,   411,   412,
     422,   435,   437,   444,   448,   458,   466,   471,   484,   493,
     498,   499,   505,   508,   518,   535,   555,   563,   568,   599,
     989,   990,  1011,  1016,  1020,  1025,  1047,  1051,  1059,  1063,
    1064,  1065,  1068,  1073,  1078,  1116,  1120,  1122,  1125,  1139,
    1143,  1146,  1149,  1153,  1154,  1161,  1171,  1174,  1192,  1194,
    1197,  1201,  1208,  1220,  1222,  1237,  1238,  1248,  1251,  1252,
    1256,  1262,  1263,  1271,  1278,  1295,  1305,  1314,  1320,  1329,
    1333,  1335,  1338,  1341,  1344,  1371,   989,   535,   201,   457,
     766,   770,   771,   773,   535,   535,   718,   687,  1569,  1569,
    1569,   595,  1460,  1501,    82,  1460,  1569,  1569,   656,   668,
    1460,   652,   595,   698,   699,   700,  1466,   298,   359,   361,
     682,   684,   685,  1281,  1504,  1569,   674,   666,   667,   673,
     674,   649,   650,   666,   650,   535,   595,    27,   630,   630,
     449,   178,   638,   298,   400,   629,   618,   629,    66,   584,
     964,     3,     5,    10,    18,    57,    58,    72,    85,    89,
     103,   127,   138,   140,   151,   154,   155,   156,   157,   158,
     159,   160,   161,   163,   164,   165,   167,   168,   169,   170,
     171,   172,   173,   174,   175,   176,   177,   190,   195,   227,
     234,   237,   238,   252,   260,   262,   265,   315,   321,   323,
     334,   363,   377,   410,   422,   437,   444,   448,   458,   471,
     484,   493,   498,   499,   505,   508,   518,   535,   555,   563,
     568,   599,  1539,  1540,  1541,   991,  1012,  1017,  1021,  1026,
    1048,  1052,  1060,  1069,  1066,  1074,  1079,  1117,  1121,  1123,
    1126,  1140,  1144,  1147,  1150,   235,   438,  1039,  1142,  1155,
    1162,  1172,  1175,  1193,  1195,  1198,   467,  1202,  1209,  1221,
    1223,  1239,  1249,  1253,  1257,  1264,  1272,  1279,  1296,  1306,
     298,   407,   451,   488,   523,   602,  1318,  1319,  1420,  1496,
    1497,  1501,  1321,  1330,   393,  1334,  1336,   976,  1339,  1342,
    1345,  1372,   597,   820,   822,   823,     1,   595,  1450,   273,
     464,   717,   719,  1569,   247,   298,   309,   407,   488,   545,
     602,   703,   704,  1507,  1460,   298,   255,   358,  1613,   298,
     554,    67,    76,   312,   398,   463,   469,   595,   657,   658,
     659,   660,   661,   662,   663,   665,  1553,  1623,   231,   398,
     669,   670,   671,   653,   665,   699,    22,   271,  1466,  1571,
    1281,   271,   495,  1585,  1569,   111,  1465,   298,   535,   625,
     319,  1580,  1569,   266,   640,   535,   535,   966,   965,   438,
     973,   360,   595,   992,   995,  1451,  1496,     6,     9,    97,
     109,   114,   224,   225,   226,   233,   247,   286,   287,   293,
     295,   298,   301,   302,   303,   307,   309,   318,   355,   375,
     407,   440,   488,   503,   504,   514,   545,   556,   578,   583,
     592,   602,  1013,  1443,  1470,  1471,  1473,  1496,  1508,  1509,
    1510,  1511,  1512,  1513,  1514,   287,   543,   547,   548,  1018,
    1438,  1439,  1440,  1441,  1442,  1443,  1476,  1496,  1509,  1511,
     298,  1022,  1023,  1456,  1457,  1458,  1501,  1027,  1029,   298,
     400,  1049,  1050,  1483,  1496,   593,  1053,  1054,  1056,  1450,
       6,  1061,  1444,  1445,  1468,  1499,  1500,  1501,  1509,   539,
    1070,  1450,     9,  1067,  1496,  1498,   263,   272,   370,   517,
    1075,  1077,   218,   256,   298,   316,   360,   490,   506,   541,
     593,  1080,  1081,  1082,  1083,  1085,  1090,  1095,  1098,  1099,
    1102,  1104,  1470,  1483,  1496,  1118,  1471,  1075,  1029,  1124,
     538,   549,  1127,  1128,  1129,  1423,  1424,  1425,   231,   376,
     377,   398,   460,  1141,  1145,  1467,  1468,  1148,  1501,   530,
    1151,  1603,  1471,  1422,  1423,  1163,  1467,   595,  1173,  1452,
    1176,  1177,  1496,  1508,  1511,  1297,  1491,  1492,  1501,   109,
    1196,  1471,  1199,  1471,   197,   263,   272,   370,  1203,  1204,
    1205,   255,  1210,  1418,  1563,   595,  1451,  1224,  1450,  1240,
    1451,  1250,  1446,  1501,  1254,  1450,   539,  1258,  1446,  1447,
       9,  1265,  1448,  1501,  1273,  1451,   179,   280,   318,  1280,
    1283,  1284,  1287,  1288,  1289,  1290,  1291,  1292,  1293,  1294,
    1420,  1453,  1454,  1467,  1490,  1492,  1501,  1297,  1307,  1450,
    1315,  1496,   547,  1502,  1503,  1322,  1323,  1324,  1471,   109,
     514,  1331,  1470,  1337,  1452,   535,   595,   977,   978,   981,
     982,   987,  1340,  1493,  1496,  1343,  1450,   298,  1346,  1484,
    1496,  1511,  1373,  1447,   460,    87,   801,   149,   472,   480,
     824,   825,   827,   838,   840,   842,  1524,   535,   772,   535,
     340,   364,  1532,   322,   453,   755,   756,   757,   758,   760,
      29,   145,   247,   298,   309,   328,   407,   488,   491,   492,
     602,   688,   689,   690,   693,   704,   525,   594,   707,  1607,
    1501,   468,   469,   481,  1595,    76,  1569,   535,   659,   535,
     595,   658,    70,  1569,     9,   233,   595,   672,   595,     1,
     535,   671,   654,  1607,   298,   701,  1505,  1585,   271,  1465,
    1465,   683,   684,   631,  1569,  1569,    73,   496,   537,   639,
     450,   535,    55,   967,   968,   969,  1552,   967,   360,   595,
     535,   154,  1010,   993,   316,  1579,   228,   358,  1583,  1510,
    1496,   547,   547,   547,  1516,   547,  1497,  1509,  1511,  1613,
    1613,   547,   547,   547,   547,  1613,  1491,   547,  1516,   155,
    1015,   530,  1014,  1471,   531,   547,  1515,   547,   547,  1497,
    1509,  1511,  1442,  1496,  1438,  1442,    68,   543,   548,   534,
     544,   196,   261,  1527,  1023,   530,  1613,   156,  1046,  1418,
    1050,  1057,  1450,   418,   562,  1055,  1607,  1619,  1583,   157,
    1062,   185,   536,  1445,  1611,   449,  1533,  1502,  1503,  1071,
    1450,   158,  1072,   413,  1589,   105,  1556,  1496,   517,  1600,
     517,  1451,   239,  1105,    53,  1551,   159,  1115,   192,   346,
    1386,  1388,  1390,  1083,  1469,  1470,  1084,  1096,  1105,   573,
     574,   575,   576,   160,  1119,    55,   267,   298,   161,  1138,
      17,   591,  1130,  1131,  1132,  1134,    20,   196,   271,   340,
     366,   532,   534,   543,   544,   547,   548,  1426,  1427,  1429,
    1471,  1569,   116,  1142,  1468,  1455,   522,  1157,  1164,  1607,
    1452,   107,   427,   515,  1178,  1179,  1181,  1182,  1299,   547,
    1502,  1471,   530,   164,  1200,    55,  1204,   474,  1206,    56,
     223,   285,   287,   298,   832,  1214,  1215,  1216,  1456,  1485,
    1496,  1501,  1508,  1511,  1607,   523,   167,  1236,   334,   389,
    1530,   168,  1247,   316,   462,  1241,   228,  1374,   169,  1255,
    1589,   595,   170,  1261,  1374,  1448,   171,  1270,   589,  1266,
     228,  1274,  1275,  1479,  1481,  1496,  1509,  1511,   192,  1290,
    1292,  1467,   394,   530,  1454,   142,   530,   570,  1282,    32,
    1502,   172,  1313,   206,   276,   280,  1309,  1039,  1316,  1471,
    1607,  1436,  1438,   547,  1503,   173,  1328,   267,  1324,   128,
    1325,  1496,  1491,   174,  1332,   228,  1452,   460,   535,   535,
     228,   413,   415,  1590,   175,  1355,   128,  1347,   176,  1378,
    1374,   535,   460,   304,   894,   595,   830,   830,   830,   825,
     535,     1,   204,   828,   829,   830,   595,   774,   364,  1465,
     761,   413,   484,   485,   759,     1,   535,   757,   690,    17,
     525,  1507,   382,  1569,   469,  1504,   535,  1569,   595,  1461,
     268,   630,     1,   233,   268,   630,   535,   535,   122,  1505,
    1465,   535,    86,   198,   416,   541,   632,   633,   634,    35,
     329,   330,   331,   402,   551,   552,   557,  1535,  1569,   968,
     481,   564,   970,   971,   419,   586,   961,    31,    41,   229,
     319,   996,   997,   998,  1002,  1005,  1548,  1549,  1607,   110,
      25,    26,    79,    81,    83,   118,   119,   120,   179,   181,
     188,   192,   294,   296,   454,   526,   581,   595,  1001,  1454,
    1610,   177,   398,  1477,  1497,   530,  1436,  1438,  1522,  1436,
    1523,   532,  1436,  1518,  1519,   595,   595,  1438,  1521,  1521,
    1521,  1475,  1496,  1509,  1511,  1520,   595,   530,  1475,  1517,
       6,  1444,  1471,  1501,  1509,   235,  1510,  1438,  1475,  1436,
     532,   261,  1528,  1439,  1439,  1440,  1440,  1440,   438,  1019,
     397,  1024,  1458,  1030,  1031,  1484,  1496,  1055,   305,   336,
     221,  1561,  1497,  1438,   319,  1534,  1503,  1450,   436,  1229,
    1230,  1231,  1076,  1607,   593,  1470,   984,   985,   984,  1389,
    1390,  1387,  1388,   572,  1002,  1005,  1086,  1087,  1088,  1607,
     572,  1106,   593,  1386,  1386,  1386,  1386,  1471,  1444,  1471,
     584,  1033,  1129,    21,   538,   549,  1135,  1136,  1424,   591,
    1132,  1133,   591,   984,  1428,  1429,  1427,    12,    13,    14,
      71,   185,   240,   241,   288,   289,   332,   345,   353,   360,
     387,   536,   540,   542,  1430,  1431,  1432,  1433,  1434,  1435,
     130,  1152,  1456,   151,  1156,  1158,     9,    12,    15,    16,
     325,   326,   353,   354,  1165,  1169,   204,  1479,     9,    68,
     206,   282,   554,  1185,  1186,  1187,  1180,  1181,   143,   361,
     594,  1301,  1584,  1622,   530,  1467,  1444,  1471,  1607,  1229,
    1583,     1,    44,    45,    46,    47,    48,    90,    91,   209,
     210,   211,   212,   213,   214,   215,   216,   217,   384,   401,
     833,   834,   835,   836,   837,   858,   859,  1497,   833,   244,
    1419,  1419,   569,   589,   525,   528,   520,  1589,   267,  1489,
    1496,  1508,  1511,   267,  1225,  1229,   153,  1267,  1496,  1267,
    1496,  1276,  1607,   530,   530,   530,   530,  1281,   177,   481,
     538,   549,  1471,    55,    42,    50,   246,   284,   308,   371,
     441,   561,  1285,  1286,  1569,  1308,  1607,  1471,   187,   339,
      84,   465,   532,  1437,   533,  1438,  1496,  1552,   228,   228,
    1444,  1471,   979,  1479,  1552,   267,  1350,     8,    41,  1375,
    1376,   821,   535,   460,   297,   896,   841,   843,   425,   535,
     535,   826,   100,    51,    75,   117,   278,   292,   413,   414,
     428,   430,   535,   586,   775,   776,   778,   782,   783,   786,
     787,   793,   796,   798,   799,  1569,   720,   539,  1559,    23,
    1545,   535,   691,   693,   298,  1504,   300,   511,   582,   664,
    1461,   319,   634,   398,  1588,    86,   329,   330,   402,   557,
     641,  1569,   481,   971,   364,   972,   124,   974,   602,  1486,
    1490,  1504,  1569,   188,   192,   344,   346,  1379,  1381,  1382,
    1384,  1385,   998,    77,    80,   294,   386,  1003,  1004,  1609,
     526,    33,    36,    37,    38,    39,    42,    50,    78,   106,
     125,   222,   230,   246,   284,   306,   308,   336,   338,   359,
     371,   403,   404,   432,   439,   441,   442,   456,   461,   481,
     490,   513,   527,   561,   571,   577,  1006,  1007,  1008,  1009,
    1379,   601,   600,  1479,  1379,   276,   495,   351,   994,   324,
     298,  1478,  1497,  1496,  1437,   532,  1437,   532,   532,   380,
     532,  1475,  1437,   532,   532,   532,  1437,   532,  1491,  1437,
     532,  1583,   349,   482,  1391,  1393,  1395,  1502,  1503,  1444,
     533,   532,   532,   530,  1529,  1019,  1468,   530,  1456,  1028,
     333,   400,  1032,    27,  1058,  1607,   443,   424,  1391,  1569,
     223,  1561,   270,   347,  1412,  1413,  1415,  1417,   276,  1100,
    1091,   986,   111,   112,   391,   595,  1089,  1454,  1087,    36,
      37,    38,    39,    42,    49,    50,    78,   106,   186,   222,
     246,   308,   350,   371,   456,   481,   490,   561,  1008,  1114,
    1496,  1097,  1103,   235,  1391,   235,  1034,    17,   525,  1137,
     369,  1135,   602,  1430,  1603,  1584,  1159,   163,  1160,   984,
    1603,   427,  1166,  1603,   530,    55,  1186,  1188,  1479,     9,
      68,   282,   554,  1183,  1184,  1479,  1494,  1496,   143,    76,
     469,  1302,  1608,    28,   131,   877,   255,   367,  1564,  1467,
    1391,   235,     9,   336,   410,   754,  1449,  1450,   833,   535,
    1563,  1215,   165,   535,  1211,  1213,   195,  1217,  1422,  1218,
    1219,  1496,  1456,  1376,  1225,  1496,  1496,   153,   342,  1401,
    1404,  1406,  1259,  1260,  1607,   984,   591,   591,  1268,  1269,
    1375,   148,   152,   189,  1496,  1479,   359,  1488,  1490,  1504,
    1478,   358,  1471,  1281,  1281,  1281,  1281,  1281,  1281,  1281,
    1281,  1286,   340,   345,  1310,  1311,  1312,  1431,  1531,  1412,
     285,   481,  1621,   495,  1597,  1597,  1438,   532,  1438,  1327,
    1607,   481,  1326,  1471,  1496,  1491,  1391,   235,   983,  1504,
     530,     9,  1348,  1349,  1525,  1351,  1496,  1327,  1351,  1229,
       7,  1542,   822,   802,   535,   460,   428,   898,   586,   886,
     853,   854,  1569,  1501,   848,   831,  1569,   101,  1555,  1569,
     413,   415,  1618,  1618,  1569,  1555,  1569,  1580,  1569,    22,
    1544,   358,   800,  1465,   198,   236,     1,   721,   722,   516,
    1599,  1561,   692,   693,    68,   320,   596,  1617,    33,   125,
    1504,  1569,   595,   535,   962,    51,  1383,  1384,   984,  1380,
    1381,   984,   351,  1581,  1581,  1581,   298,  1487,  1490,  1506,
    1569,  1569,  1569,   147,  1009,    67,   481,   142,   570,  1569,
       8,  1543,  1379,   481,   999,  1000,  1002,  1533,   512,  1515,
     512,  1515,   532,  1460,  1515,  1515,  1515,  1475,  1533,   282,
     554,  1515,  1497,   984,   984,  1394,  1395,  1392,  1393,  1503,
    1391,   532,  1438,  1515,  1515,  1482,  1496,  1508,  1419,   336,
    1535,  1438,   984,   984,  1416,  1417,  1414,  1415,  1483,  1106,
     125,   248,   529,   558,  1039,  1092,  1093,  1094,   987,  1569,
     294,   454,  1569,   182,   184,   950,   951,  1558,  1603,  1569,
     142,   570,  1569,    54,   244,   296,   385,   473,   529,  1002,
    1107,  1108,  1109,  1110,  1111,  1112,  1113,  1607,  1107,  1444,
    1445,  1444,  1445,  1035,  1036,  1037,  1552,  1136,  1423,   984,
    1496,   984,   151,   586,  1167,  1168,  1169,   522,  1170,  1601,
     586,  1480,  1482,  1479,    55,     8,    41,  1189,  1190,  1191,
    1184,  1189,   221,   469,  1298,  1569,   276,  1573,   367,  1444,
     369,  1586,  1586,   362,   442,  1207,  1450,  1607,  1496,   984,
       8,   228,     7,   254,  1226,  1227,  1228,  1230,  1233,  1260,
    1607,   117,   337,  1242,  1244,  1246,   984,   984,  1405,  1406,
    1404,  1412,   305,   336,  1423,  1422,  1268,   427,  1277,  1471,
    1431,  1496,  1432,  1433,  1434,  1435,  1438,  1317,  1471,  1317,
     532,   348,   546,  1396,  1398,  1400,   384,  1533,  1533,  1444,
     535,  1480,   366,  1479,   129,  1352,   515,  1354,  1259,   374,
    1454,  1486,   803,   895,   535,   460,   454,   942,  1571,   586,
     236,   525,   839,    21,    40,    43,    44,    45,    46,    47,
      48,    49,    88,    92,    93,    94,    95,    96,   138,   209,
     210,   211,   212,   213,   244,   257,   274,   325,   357,   372,
     381,   384,   401,   417,   475,   477,   478,   479,   510,   565,
     566,   567,   579,   849,   850,   851,   854,   855,   856,   857,
     858,   859,   862,   865,   882,   883,   884,   885,   886,   891,
     892,   893,  1569,  1594,    27,   228,   844,  1547,   236,  1504,
     595,   737,  1569,  1544,   595,  1462,  1463,   360,   490,  1614,
     298,  1460,  1464,  1504,   589,  1569,   203,   249,   595,   784,
    1465,   535,     4,    19,    30,   258,   294,   368,   373,   413,
     421,   433,   474,   484,   535,   539,   723,   724,   732,   734,
     736,   738,   739,   740,   741,   744,   745,   746,   747,   748,
     750,   751,   753,  1589,  1608,  1555,  1449,    17,    33,   975,
     976,  1486,  1486,  1486,  1486,  1486,  1486,  1569,  1569,  1421,
    1488,  1421,  1488,  1487,  1569,  1000,  1391,   228,  1537,   532,
    1033,   138,   459,   528,  1101,   271,   536,  1570,  1570,  1570,
    1570,  1471,  1094,  1486,  1486,   294,   454,   153,  1486,  1421,
    1421,  1486,   255,   271,  1572,  1504,  1545,  1569,  1108,   336,
    1114,  1391,   423,  1391,   423,  1036,   360,   970,   102,   419,
     586,  1159,  1168,   117,  1557,  1603,  1189,  1189,  1480,   541,
    1567,  1567,  1191,  1190,   263,   584,  1303,  1460,  1300,  1391,
     305,   336,   165,  1212,  1219,  1471,  1584,   305,   276,  1234,
    1232,  1233,  1607,   253,   275,   590,   984,   984,  1245,  1246,
    1243,  1244,   305,   984,   984,   294,  1575,   984,   984,  1399,
    1400,  1397,  1398,  1569,  1391,  1537,  1391,   580,   980,  1356,
    1349,  1563,   110,  1353,  1563,  1396,   183,   343,  1377,  1407,
    1409,  1411,  1413,   294,   296,  1576,    63,   804,   805,   822,
     897,   535,   460,    20,   196,   366,   532,   534,   543,   544,
     547,   548,   832,   847,   888,   889,  1569,  1501,   285,   353,
     476,   564,  1593,   564,  1593,   564,  1593,   564,  1593,   564,
    1593,   591,  1605,  1583,   447,  1591,   144,   868,  1504,  1497,
     271,   283,   447,  1574,  1569,   198,   199,   250,   282,   554,
     595,   888,   530,   781,   221,   797,  1463,   296,  1578,   530,
    1554,  1563,   200,   207,   452,   559,   585,   587,   794,   795,
    1569,  1569,  1580,  1589,   530,   584,  1604,   470,  1569,  1553,
     129,  1573,  1573,   336,   752,  1504,  1607,   495,   305,    43,
    1550,  1569,   762,   763,  1450,   693,   976,  1483,  1487,   294,
     296,  1620,  1488,   257,  1565,   132,  1538,  1038,  1039,  1107,
    1471,  1471,  1471,  1471,  1583,  1496,  1569,  1471,   456,   598,
    1445,  1445,  1472,  1473,  1495,  1497,   984,    55,   125,  1189,
    1471,  1471,   395,  1449,   235,   370,  1304,  1501,   443,    55,
     305,  1569,  1235,  1402,  1404,  1406,  1412,   305,   305,  1496,
    1538,  1357,   535,  1496,  1563,  1496,   984,   984,  1410,  1411,
    1408,  1409,  1465,   822,   822,   899,   535,   525,   887,   889,
     334,   493,   832,   845,   846,   847,   602,   219,   281,   314,
     506,   523,   588,   593,    59,   870,   560,   530,   866,   857,
      27,   852,  1552,   468,  1536,  1536,  1504,    68,   415,   777,
    1459,  1460,   788,  1504,  1595,   298,   785,  1501,   785,  1569,
    1573,   144,   198,   729,   421,   745,  1569,  1569,  1569,  1569,
      23,    24,  1546,   754,  1569,  1580,   470,   737,   763,   386,
     764,   153,  1488,  1565,  1391,   192,   346,   546,  1041,  1043,
    1045,     6,   267,   341,   360,   545,  1040,  1568,  1496,  1391,
    1391,  1471,  1569,  1449,   395,  1471,  1496,  1405,  1403,  1404,
    1391,    31,   146,   193,   236,  1358,  1359,  1360,  1362,  1366,
    1368,  1369,  1370,  1548,  1561,  1496,   806,   900,   943,   832,
     591,   890,  1606,  1583,   847,   126,   208,   279,   313,   483,
     553,   860,   299,   861,  1563,   228,   867,   528,  1602,  1504,
    1602,   298,  1483,  1553,  1460,    52,   550,   789,   790,   791,
     792,  1607,  1554,   228,   780,  1562,   144,   408,   470,   733,
    1569,    60,    61,    62,   136,   137,   138,   263,   264,   277,
     294,   311,   370,   390,   391,   392,   408,   516,   725,   726,
     727,   728,  1464,   491,   749,  1460,  1460,  1460,  1569,  1504,
     124,   530,  1488,   984,   984,   984,  1044,  1045,  1042,  1043,
    1583,  1496,  1214,  1569,   569,    41,  1543,   398,   122,   807,
     409,   901,   827,   842,   944,   945,   946,   471,   538,  1496,
     595,  1504,   530,   866,   130,   130,   869,  1464,  1464,   220,
     781,  1504,   749,   298,   731,  1501,   731,     7,   731,   731,
     731,   298,   730,  1501,   486,   536,    34,   194,   310,   742,
     535,  1488,  1496,  1214,  1422,   429,   490,  1596,   153,   493,
    1367,  1584,   535,   808,  1561,  1452,     1,   829,   946,   535,
     530,  1569,   261,   871,  1584,  1584,   258,   872,   874,   875,
     876,   877,   879,  1548,  1554,  1526,  1622,  1559,  1569,  1459,
     594,   743,   743,  1496,   187,   192,  1612,     9,  1363,  1364,
    1457,   370,  1567,   902,   535,   947,   535,   832,   872,  1460,
    1460,  1552,   879,   875,  1573,   873,  1464,   130,   779,   508,
     735,  1459,   305,   448,   395,  1587,   358,   396,   420,  1365,
    1364,   132,   187,   495,   509,   519,   813,   814,   815,   263,
     272,     1,   903,   948,   872,   595,   880,   881,  1569,   874,
    1584,   591,   361,  1584,   358,  1501,   110,   514,  1569,   276,
     276,   132,   285,   815,   153,   316,   495,   509,   519,   809,
     810,   811,   812,  1496,  1579,  1598,   153,   316,   495,   519,
     816,   817,   818,   819,  1496,  1598,   535,    74,   104,   105,
     374,   535,   904,   905,   907,  1569,  1631,    33,    36,    37,
      38,    39,    42,    49,    50,    78,   186,   222,   228,   230,
     242,   246,   284,   294,   308,   336,   357,   371,   403,   432,
     461,   481,   490,   513,   530,   541,   561,   584,   855,   856,
     862,   882,   884,   886,   949,   958,   959,  1007,  1008,  1569,
    1609,   881,  1459,  1460,     9,   488,   602,   694,   322,   413,
     415,  1616,   197,   263,   272,   370,  1361,  1449,  1569,   357,
    1496,  1569,  1569,  1569,  1569,   276,   118,   526,   276,   285,
     811,  1496,   110,   406,   486,   500,   501,   502,   276,   118,
     526,   276,   285,   818,  1496,   517,  1569,  1569,  1544,   290,
     291,  1577,   916,   236,   205,   906,  1560,  1569,   294,   454,
    1569,   950,   951,  1569,  1489,   954,  1581,  1504,    67,  1569,
    1496,  1496,   236,   956,  1581,  1510,  1616,  1496,  1504,  1496,
    1496,  1496,  1496,  1569,  1569,  1569,  1569,  1569,  1496,  1569,
    1569,  1569,  1569,  1569,  1569,  1569,  1569,  1569,  1569,  1569,
    1496,  1569,  1483,  1569,  1544,   908,  1506,   827,   917,  1498,
    1486,  1486,  1486,  1569,  1602,  1569,  1504,  1569,  1602,  1496,
    1496,  1496,  1496,  1496,  1496,  1496,  1496,  1496,  1496,  1496,
    1496,  1496,  1496,  1496,  1496,  1496,  1496,  1496,   909,   294,
     296,  1615,   829,   830,   317,   383,   543,   548,   952,   953,
     955,  1483,   952,   953,   957,   878,   879,  1496,  1496,   206,
     220,   245,   280,   910,   911,   912,   913,   914,   915,  1506,
     918,  1486,  1486,  1496,  1496,   121,   134,  1624,  1569,  1569,
      65,   104,  1624,  1625,  1610,   919,  1496,  1569,  1506,  1506,
     245,  1569,  1569,   243,   294,   296,   334,   357,   388,   486,
     507,   535,   558,   579,   589,   855,   862,   863,   882,   884,
     886,   920,   921,   925,   926,   929,   930,   931,   932,   933,
     934,   939,   940,   941,  1609,  1610,  1496,  1506,  1506,  1506,
     259,  1566,   351,   352,  1582,  1544,   243,  1504,   591,  1569,
    1583,  1569,  1569,  1496,   335,   383,   935,   936,  1506,   383,
     937,   938,  1506,  1582,  1544,  1496,  1571,  1569,   866,  1422,
    1476,  1474,  1476,    64,   104,   374,   378,   379,   428,   445,
     446,   922,  1624,  1625,  1626,  1627,  1628,  1629,  1630,   138,
     228,  1504,   936,  1504,   938,  1571,  1496,   936,  1602,  1533,
     434,   927,  1476,   220,   220,   245,   220,   245,   205,   923,
    1496,   923,  1476,  1496,   869,  1584,   366,   924,   924,    55,
     497,   864,   205,   928,  1496,   374,  1476,  1504
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint16 yyr1[] =
{
       0,   604,   606,   605,   607,   607,   609,   608,   610,   610,
     611,   611,   613,   612,   614,   615,   616,   616,   617,   617,
     618,   619,   620,   621,   621,   622,   622,   624,   625,   623,
     627,   626,   628,   628,   629,   629,   630,   630,   631,   631,
     632,   632,   632,   632,   633,   633,   634,   634,   635,   635,
     636,   637,   637,   638,   638,   639,   639,   639,   640,   640,
     641,   641,   641,   641,   642,   643,   643,   644,   645,   645,
     645,   646,   646,   646,   646,   646,   647,   647,   647,   647,
     647,   648,   648,   649,   649,   650,   650,   650,   650,   652,
     651,   653,   653,   654,   654,   656,   655,   657,   657,   657,
     657,   658,   658,   659,   659,   659,   659,   660,   661,   662,
     663,   664,   664,   664,   664,   665,   665,   666,   666,   668,
     667,   669,   669,   669,   670,   670,   671,   671,   671,   671,
     671,   672,   672,   673,   673,   674,   674,   675,   676,   676,
     677,   677,   678,   678,   679,   679,   679,   679,   679,   679,
     679,   679,   679,   679,   679,   679,   681,   680,   682,   682,
     682,   682,   683,   683,   684,   685,   685,   687,   686,   688,
     688,   688,   688,   688,   688,   689,   689,   690,   690,   691,
     690,   692,   692,   693,   693,   693,   693,   693,   693,   694,
     694,   695,   696,   696,   697,   698,   698,   699,   700,   700,
     701,   701,   702,   703,   703,   704,   704,   705,   706,   707,
     707,   708,   709,   710,   711,   712,   713,   714,   715,   715,
     716,   716,   717,   717,   718,   718,   720,   719,   721,   721,
     722,   722,   723,   723,   723,   723,   723,   723,   723,   723,
     723,   723,   723,   723,   723,   724,   724,   724,   724,   724,
     724,   725,   725,   725,   726,   726,   726,   726,   727,   727,
     727,   727,   727,   727,   727,   728,   728,   729,   729,   729,
     730,   730,   731,   731,   731,   732,   733,   733,   733,   734,
     735,   735,   735,   736,   737,   738,   739,   739,   739,   741,
     740,   742,   742,   742,   743,   743,   743,   743,   744,   744,
     745,   745,   745,   745,   746,   747,   748,   749,   749,   749,
     750,   751,   752,   752,   753,   754,   754,   754,   755,   755,
     755,   756,   756,   757,   757,   758,   759,   759,   759,   759,
     761,   760,   762,   762,   763,   764,   764,   766,   765,   767,
     767,   768,   768,   769,   769,   770,   772,   771,   771,   773,
     773,   774,   774,   775,   775,   775,   775,   775,   775,   775,
     775,   775,   775,   775,   776,   777,   777,   777,   778,   778,
     778,   779,   779,   780,   780,   781,   781,   782,   783,   783,
     784,   784,   785,   785,   786,   787,   788,   788,   789,   789,
     789,   790,   791,   792,   793,   794,   794,   794,   794,   794,
     795,   795,   796,   797,   797,   798,   799,   799,   800,   800,
     801,   802,   801,   803,   803,   804,   806,   805,   807,   807,
     808,   808,   808,   809,   809,   809,   810,   810,   811,   811,
     811,   811,   811,   811,   811,   811,   811,   811,   811,   812,
     813,   813,   814,   814,   815,   815,   815,   815,   815,   815,
     815,   816,   816,   816,   817,   817,   818,   818,   818,   818,
     818,   818,   819,   820,   821,   820,   822,   823,   822,   824,
     824,   825,   825,   825,   826,   825,   825,   827,   828,   828,
     829,   829,   830,   831,   831,   832,   832,   832,   832,   833,
     833,   833,   833,   833,   833,   833,   833,   833,   833,   833,
     833,   833,   834,   834,   835,   835,   836,   836,   836,   837,
     837,   838,   839,   839,   841,   840,   842,   843,   842,   844,
     844,   845,   845,   845,   846,   846,   847,   847,   847,   847,
     847,   847,   847,   847,   847,   847,   848,   848,   849,   849,
     849,   849,   849,   849,   849,   849,   849,   849,   849,   849,
     849,   849,   849,   850,   851,   852,   852,   853,   853,   854,
     855,   856,   856,   857,   857,   857,   857,   857,   857,   857,
     857,   857,   857,   857,   857,   857,   857,   857,   857,   857,
     857,   857,   857,   857,   857,   857,   857,   857,   857,   857,
     857,   857,   857,   857,   857,   857,   857,   857,   857,   857,
     857,   857,   857,   857,   857,   857,   857,   858,   858,   859,
     859,   860,   860,   860,   860,   860,   860,   860,   861,   861,
     862,   862,   863,   864,   864,   865,   865,   865,   866,   866,
     867,   867,   868,   868,   869,   869,   870,   870,   871,   871,
     872,   872,   873,   872,   872,   872,   874,   875,   875,   876,
     877,   877,   878,   878,   879,   880,   880,   881,   882,   883,
     884,   885,   887,   886,   888,   888,   889,   889,   890,   890,
     891,   891,   892,   893,   894,   895,   894,   896,   897,   896,
     898,   899,   898,   900,   900,   902,   901,   903,   903,   903,
     904,   904,   904,   904,   905,   906,   907,   908,   908,   908,
     909,   909,   910,   910,   910,   910,   910,   911,   912,   913,
     914,   915,   916,   916,   918,   917,   919,   919,   920,   920,
     920,   920,   920,   920,   920,   920,   920,   920,   920,   920,
     920,   920,   920,   920,   921,   922,   922,   922,   922,   922,
     922,   922,   923,   923,   923,   924,   924,   925,   926,   927,
     927,   928,   928,   929,   930,   931,   932,   932,   933,   934,
     934,   935,   935,   936,   936,   936,   937,   937,   938,   938,
     939,   940,   941,   942,   943,   942,   944,   944,   945,   945,
     946,   947,   946,   946,   948,   948,   949,   949,   949,   949,
     949,   949,   949,   949,   949,   949,   949,   949,   949,   949,
     949,   949,   949,   949,   949,   949,   949,   949,   949,   949,
     949,   949,   949,   949,   949,   949,   949,   949,   949,   949,
     949,   949,   949,   949,   949,   949,   949,   949,   949,   950,
     950,   951,   951,   952,   952,   953,   953,   954,   955,   955,
     955,   956,   957,   957,   957,   958,   959,   960,   961,   962,
     960,   963,   960,   964,   965,   964,   966,   964,   967,   967,
     968,   969,   969,   969,   970,   970,   970,   970,   970,   970,
     971,   972,   972,   973,   973,   973,   974,   975,   974,   976,
     976,   977,   977,   977,   977,   977,   979,   978,   980,   980,
     981,   982,   983,   983,   985,   986,   984,   988,   987,   987,
     989,   989,   989,   989,   989,   989,   989,   989,   989,   989,
     989,   989,   989,   989,   989,   989,   989,   989,   989,   989,
     989,   989,   989,   989,   989,   989,   989,   989,   989,   989,
     989,   989,   989,   989,   989,   989,   989,   989,   989,   989,
     989,   989,   989,   989,   989,   989,   989,   989,   989,   989,
     989,   989,   989,   989,   989,   989,   989,   991,   990,   993,
     992,   994,   992,   992,   992,   992,   992,   992,   992,   992,
     992,   992,   992,   992,   992,   992,   992,   992,   992,   992,
     992,   992,   995,   995,   996,   996,   997,   997,   998,   998,
     998,   998,   998,   999,   999,  1000,  1000,  1001,  1001,  1002,
    1002,  1002,  1003,  1004,  1004,  1005,  1006,  1006,  1006,  1006,
    1006,  1006,  1006,  1006,  1006,  1006,  1006,  1006,  1006,  1006,
    1006,  1006,  1006,  1006,  1006,  1006,  1006,  1006,  1006,  1006,
    1006,  1006,  1006,  1006,  1006,  1006,  1006,  1006,  1006,  1007,
    1007,  1007,  1008,  1008,  1008,  1009,  1009,  1010,  1010,  1012,
    1011,  1013,  1013,  1013,  1013,  1014,  1014,  1015,  1015,  1017,
    1016,  1018,  1018,  1019,  1019,  1021,  1020,  1022,  1022,  1023,
    1024,  1024,  1026,  1025,  1028,  1027,  1029,  1029,  1029,  1029,
    1029,  1030,  1030,  1031,  1031,  1032,  1032,  1033,  1034,  1033,
    1035,  1035,  1036,  1036,  1037,  1037,  1037,  1037,  1038,  1038,
    1038,  1038,  1038,  1039,  1039,  1040,  1040,  1041,  1041,  1041,
    1042,  1042,  1043,  1043,  1044,  1044,  1045,  1046,  1046,  1048,
    1047,  1049,  1049,  1050,  1050,  1052,  1051,  1053,  1053,  1054,
    1054,  1055,  1055,  1055,  1055,  1055,  1057,  1056,  1058,  1058,
    1060,  1059,  1061,  1062,  1062,  1063,  1064,  1066,  1065,  1067,
    1067,  1069,  1068,  1070,  1070,  1071,  1071,  1072,  1072,  1074,
    1073,  1075,  1076,  1076,  1077,  1077,  1077,  1077,  1077,  1079,
    1078,  1080,  1080,  1080,  1080,  1080,  1080,  1080,  1080,  1080,
    1081,  1081,  1082,  1082,  1084,  1083,  1085,  1085,  1086,  1086,
    1087,  1087,  1087,  1087,  1087,  1088,  1088,  1088,  1088,  1089,
    1089,  1091,  1090,  1092,  1092,  1093,  1093,  1094,  1094,  1094,
    1094,  1094,  1096,  1097,  1095,  1098,  1098,  1100,  1101,  1099,
    1103,  1102,  1104,  1104,  1104,  1105,  1105,  1106,  1106,  1107,
    1107,  1108,  1108,  1108,  1108,  1108,  1108,  1108,  1109,  1109,
    1110,  1110,  1111,  1111,  1112,  1113,  1114,  1114,  1114,  1114,
    1114,  1114,  1114,  1114,  1114,  1114,  1114,  1114,  1114,  1114,
    1114,  1114,  1114,  1114,  1114,  1114,  1114,  1114,  1115,  1115,
    1117,  1116,  1118,  1118,  1118,  1118,  1118,  1119,  1119,  1121,
    1120,  1123,  1122,  1124,  1126,  1125,  1127,  1128,  1128,  1129,
    1129,  1129,  1130,  1130,  1131,  1131,  1132,  1133,  1134,  1134,
    1135,  1135,  1136,  1136,  1136,  1136,  1137,  1137,  1138,  1138,
    1140,  1139,  1141,  1141,  1141,  1141,  1141,  1141,  1141,  1142,
    1142,  1144,  1143,  1145,  1147,  1146,  1148,  1150,  1149,  1151,
    1152,  1152,  1153,  1155,  1154,  1156,  1156,  1156,  1157,  1157,
    1158,  1159,  1160,  1160,  1162,  1161,  1163,  1164,  1164,  1165,
    1165,  1165,  1166,  1166,  1167,  1167,  1168,  1169,  1169,  1169,
    1169,  1169,  1169,  1169,  1170,  1170,  1172,  1171,  1173,  1173,
    1175,  1174,  1176,  1177,  1177,  1177,  1178,  1178,  1178,  1178,
    1180,  1179,  1181,  1182,  1183,  1183,  1184,  1184,  1184,  1184,
    1184,  1184,  1185,  1185,  1186,  1186,  1187,  1187,  1187,  1187,
    1187,  1188,  1189,  1189,  1189,  1189,  1189,  1190,  1191,  1193,
    1192,  1195,  1194,  1196,  1196,  1198,  1197,  1199,  1199,  1200,
    1200,  1202,  1201,  1203,  1203,  1204,  1205,  1205,  1205,  1205,
    1206,  1206,  1207,  1207,  1207,  1207,  1209,  1208,  1210,  1211,
    1210,  1210,  1212,  1212,  1213,  1213,  1214,  1214,  1215,  1215,
    1215,  1215,  1215,  1216,  1216,  1217,  1217,  1218,  1218,  1219,
    1221,  1220,  1223,  1222,  1224,  1225,  1225,  1226,  1226,  1226,
    1226,  1227,  1227,  1228,  1228,  1229,  1229,  1230,  1231,  1231,
    1231,  1232,  1232,  1233,  1233,  1233,  1234,  1234,  1235,  1235,
    1236,  1236,  1237,  1239,  1238,  1240,  1241,  1241,  1242,  1242,
    1242,  1243,  1243,  1244,  1245,  1245,  1246,  1247,  1247,  1249,
    1248,  1250,  1251,  1253,  1252,  1254,  1255,  1255,  1257,  1256,
    1258,  1259,  1259,  1260,  1260,  1261,  1261,  1262,  1264,  1263,
    1265,  1265,  1266,  1266,  1267,  1267,  1268,  1268,  1269,  1270,
    1270,  1272,  1271,  1273,  1273,  1274,  1274,  1275,  1276,  1276,
    1276,  1276,  1277,  1277,  1279,  1278,  1280,  1280,  1280,  1280,
    1280,  1280,  1280,  1280,  1281,  1281,  1282,  1282,  1283,  1284,
    1285,  1285,  1286,  1286,  1286,  1286,  1286,  1286,  1286,  1286,
    1287,  1287,  1287,  1288,  1289,  1289,  1290,  1291,  1291,  1292,
    1292,  1293,  1294,  1296,  1295,  1298,  1297,  1299,  1299,  1300,
    1300,  1301,  1301,  1302,  1302,  1303,  1303,  1303,  1304,  1304,
    1304,  1306,  1305,  1307,  1308,  1308,  1309,  1309,  1309,  1309,
    1310,  1310,  1310,  1310,  1310,  1310,  1311,  1312,  1312,  1313,
    1313,  1315,  1314,  1314,  1314,  1316,  1316,  1316,  1316,  1316,
    1317,  1317,  1318,  1318,  1319,  1319,  1319,  1319,  1321,  1320,
    1322,  1323,  1323,  1324,  1325,  1325,  1326,  1326,  1327,  1327,
    1328,  1328,  1330,  1329,  1331,  1331,  1331,  1331,  1332,  1332,
    1333,  1334,  1334,  1336,  1335,  1337,  1337,  1339,  1338,  1340,
    1342,  1341,  1343,  1345,  1344,  1346,  1347,  1347,  1348,  1348,
    1349,  1350,  1350,  1351,  1352,  1352,  1353,  1353,  1354,  1354,
    1355,  1355,  1357,  1356,  1358,  1358,  1358,  1358,  1358,  1359,
    1360,  1360,  1361,  1361,  1361,  1361,  1361,  1362,  1363,  1363,
    1364,  1364,  1364,  1365,  1365,  1365,  1365,  1366,  1367,  1367,
    1368,  1369,  1370,  1370,  1372,  1371,  1373,  1374,  1374,  1375,
    1375,  1375,  1375,  1376,  1376,  1377,  1377,  1377,  1378,  1378,
    1379,  1379,  1379,  1380,  1380,  1381,  1382,  1382,  1383,  1383,
    1384,  1385,  1385,  1386,  1386,  1386,  1387,  1387,  1388,  1389,
    1389,  1390,  1391,  1391,  1391,  1392,  1392,  1393,  1394,  1394,
    1395,  1396,  1396,  1396,  1397,  1397,  1398,  1399,  1399,  1400,
    1401,  1401,  1402,  1402,  1403,  1403,  1404,  1405,  1405,  1406,
    1407,  1407,  1408,  1408,  1409,  1410,  1410,  1411,  1412,  1412,
    1413,  1413,  1414,  1414,  1415,  1416,  1416,  1417,  1418,  1418,
    1419,  1419,  1420,  1420,  1421,  1421,  1422,  1423,  1425,  1424,
    1426,  1426,  1427,  1427,  1427,  1427,  1427,  1427,  1427,  1427,
    1427,  1427,  1427,  1427,  1427,  1427,  1428,  1428,  1429,  1430,
    1430,  1430,  1430,  1430,  1430,  1430,  1430,  1430,  1430,  1430,
    1430,  1430,  1430,  1431,  1431,  1432,  1432,  1433,  1433,  1434,
    1435,  1436,  1436,  1437,  1437,  1437,  1438,  1438,  1438,  1439,
    1439,  1439,  1440,  1440,  1441,  1441,  1441,  1442,  1442,  1443,
    1443,  1443,  1443,  1443,  1443,  1444,  1444,  1445,  1446,  1447,
    1447,  1448,  1449,  1449,  1450,  1451,  1452,  1453,  1453,  1454,
    1455,  1455,  1456,  1457,  1457,  1457,  1458,  1459,  1459,  1460,
    1461,  1462,  1462,  1463,  1464,  1464,  1465,  1465,  1466,  1467,
    1467,  1468,  1468,  1468,  1469,  1469,  1470,  1470,  1471,  1471,
    1472,  1472,  1473,  1473,  1473,  1473,  1473,  1473,  1473,  1473,
    1473,  1474,  1474,  1475,  1475,  1475,  1476,  1476,  1476,  1476,
    1476,  1476,  1476,  1477,  1477,  1478,  1478,  1479,  1480,  1481,
    1481,  1481,  1482,  1482,  1483,  1483,  1484,  1484,  1484,  1485,
    1485,  1485,  1486,  1486,  1486,  1487,  1487,  1488,  1488,  1489,
    1489,  1489,  1490,  1491,  1492,  1492,  1493,  1494,  1495,  1496,
    1497,  1497,  1497,  1497,  1498,  1498,  1499,  1500,  1500,  1500,
    1500,  1501,  1501,  1502,  1503,  1503,  1504,  1505,  1506,  1507,
    1507,  1507,  1507,  1507,  1507,  1507,  1508,  1508,  1509,  1509,
    1510,  1510,  1510,  1510,  1510,  1510,  1510,  1511,  1511,  1511,
    1511,  1511,  1511,  1511,  1511,  1511,  1511,  1511,  1511,  1511,
    1511,  1512,  1512,  1513,  1513,  1513,  1514,  1514,  1514,  1514,
    1515,  1515,  1515,  1516,  1516,  1516,  1517,  1517,  1517,  1519,
    1518,  1520,  1520,  1521,  1521,  1522,  1522,  1523,  1523,  1524,
    1525,  1525,  1526,  1526,  1527,  1527,  1528,  1528,  1529,  1529,
    1530,  1530,  1530,  1531,  1531,  1532,  1532,  1532,  1533,  1533,
    1534,  1534,  1535,  1535,  1535,  1535,  1535,  1535,  1535,  1535,
    1536,  1536,  1537,  1537,  1538,  1538,  1539,  1539,  1539,  1539,
    1540,  1540,  1540,  1540,  1540,  1540,  1540,  1540,  1540,  1540,
    1540,  1540,  1540,  1540,  1540,  1540,  1540,  1540,  1540,  1540,
    1540,  1540,  1540,  1540,  1540,  1540,  1540,  1540,  1540,  1540,
    1540,  1540,  1540,  1540,  1540,  1540,  1540,  1540,  1540,  1540,
    1540,  1540,  1540,  1540,  1540,  1540,  1540,  1540,  1541,  1541,
    1541,  1541,  1541,  1541,  1541,  1541,  1541,  1541,  1541,  1541,
    1541,  1541,  1541,  1541,  1541,  1541,  1541,  1541,  1541,  1542,
    1542,  1543,  1543,  1544,  1544,  1545,  1545,  1546,  1546,  1546,
    1547,  1547,  1548,  1548,  1549,  1549,  1550,  1550,  1551,  1551,
    1552,  1552,  1553,  1553,  1554,  1554,  1555,  1555,  1556,  1556,
    1557,  1557,  1558,  1558,  1559,  1559,  1560,  1560,  1561,  1561,
    1562,  1562,  1563,  1563,  1564,  1564,  1564,  1565,  1565,  1566,
    1566,  1567,  1567,  1568,  1568,  1569,  1569,  1570,  1570,  1570,
    1571,  1571,  1571,  1572,  1572,  1572,  1573,  1573,  1574,  1574,
    1574,  1575,  1575,  1576,  1576,  1576,  1577,  1577,  1577,  1578,
    1578,  1579,  1579,  1580,  1580,  1581,  1581,  1582,  1582,  1582,
    1583,  1583,  1584,  1584,  1585,  1585,  1585,  1585,  1586,  1586,
    1587,  1587,  1588,  1588,  1589,  1589,  1590,  1590,  1590,  1591,
    1591,  1592,  1592,  1593,  1593,  1594,  1594,  1594,  1595,  1595,
    1596,  1596,  1597,  1597,  1598,  1598,  1599,  1599,  1600,  1600,
    1601,  1601,  1602,  1602,  1603,  1603,  1604,  1604,  1604,  1605,
    1605,  1606,  1606,  1607,  1607,  1608,  1608,  1609,  1609,  1610,
    1610,  1611,  1611,  1612,  1612,  1613,  1613,  1614,  1614,  1615,
    1615,  1616,  1616,  1617,  1617,  1617,  1618,  1618,  1619,  1619,
    1620,  1620,  1621,  1621,  1622,  1622,  1623,  1623,  1623,  1624,
    1624,  1625,  1625,  1626,  1626,  1627,  1627,  1628,  1628,  1629,
    1629,  1630,  1630,  1631,  1631
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     2,     1,     1,     0,     2,     1,     2,
       1,     1,     0,     2,     5,     5,     0,     1,     1,     2,
       3,     3,     3,     0,     3,     1,     1,     0,     0,     8,
       0,     6,     1,     1,     1,     1,     0,     2,     0,     3,
       1,     1,     1,     1,     2,     2,     1,     1,     0,     3,
       4,     0,     5,     0,     3,     1,     1,     1,     0,     4,
       1,     1,     1,     1,     3,     0,     3,     2,     0,     1,
       1,     3,     2,     1,     2,     1,     3,     3,     3,     3,
       3,     0,     3,     0,     1,     1,     1,     2,     2,     0,
       4,     0,     3,     0,     3,     0,     4,     0,     2,     3,
       2,     1,     2,     1,     1,     1,     1,     5,     3,     3,
       4,     1,     1,     1,     1,     1,     2,     0,     1,     0,
       4,     0,     2,     3,     1,     2,     3,     3,     3,     3,
       3,     1,     2,     0,     1,     2,     1,     2,     0,     1,
       2,     3,     1,     2,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     0,     3,     2,     3,
       3,     1,     0,     1,     1,     3,     4,     0,     5,     1,
       1,     1,     1,     1,     1,     1,     2,     1,     3,     0,
       4,     1,     3,     1,     1,     1,     1,     1,     1,     1,
       1,     2,     0,     2,     3,     1,     2,     3,     1,     2,
       1,     2,     4,     1,     2,     1,     3,     4,     5,     0,
       3,     3,     5,     3,     4,     3,     3,     5,     0,     3,
       0,     2,     0,     2,     0,     2,     0,     5,     2,     2,
       0,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     5,     5,     5,     5,     5,
       5,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     0,     3,     0,     1,     1,
       1,     1,     0,     1,     1,     4,     1,     1,     1,     7,
       0,     4,     3,     3,     1,     4,     0,     1,     1,     0,
       5,     2,     2,     1,     0,     4,     5,     2,     3,     1,
       1,     3,     1,     2,     4,     4,     4,     1,     3,     4,
       4,     3,     1,     1,     3,     2,     2,     2,     0,     2,
       3,     1,     2,     1,     1,     5,     0,     1,     1,     1,
       0,     6,     1,     2,     2,     0,     2,     0,    10,     0,
       3,     0,     3,     0,     2,     2,     0,     5,     3,     1,
       1,     0,     2,     2,     2,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     5,     0,     1,     1,     4,     6,
       9,     0,     3,     0,     2,     0,     2,     3,     5,     5,
       1,     1,     1,     1,     3,     5,     0,     2,     1,     1,
       1,     4,     2,     2,     4,     1,     1,     1,     1,     1,
       1,     1,     4,     0,     2,     2,     2,     2,     1,     2,
       0,     0,     5,     0,     2,     2,     0,     5,     0,     2,
       4,     3,     4,     0,     1,     1,     1,     2,     4,     4,
       4,     4,     4,     4,     4,     4,     4,     4,     4,    11,
       0,     1,     1,     2,     4,     4,     4,     6,     4,     3,
       4,     0,     1,     1,     1,     2,     4,     4,     4,     4,
       4,     4,     6,     0,     0,     5,     0,     0,     2,     2,
       3,     1,     1,     1,     0,     4,     3,     2,     0,     1,
       1,     1,     1,     0,     2,     1,     2,     2,     3,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     5,     0,     2,     0,     4,     5,     0,     7,     2,
       2,     1,     3,     1,     1,     2,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     0,     2,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     2,     3,     0,     2,     0,     1,     2,
       1,     1,     3,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     3,     3,
       4,     3,     3,     3,     4,     1,     1,     1,     1,     1,
       1,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     0,     1,     1,     1,     1,     1,     1,     0,     1,
       3,     3,     6,     0,     2,     6,     8,     7,     0,     2,
       0,     2,     0,     2,     0,     3,     0,     3,     0,     1,
       0,     2,     0,     3,     1,     1,     1,     1,     2,     4,
       1,     1,     0,     1,     3,     1,     2,     1,     2,     2,
       3,     1,     0,     5,     1,     2,     3,     1,     0,     4,
       2,     2,     2,     4,     0,     0,     5,     0,     0,     5,
       0,     0,     5,     0,     2,     0,     6,     0,     2,     2,
       2,     3,     1,     1,     2,     2,     4,     1,     4,     2,
       0,     2,     1,     1,     1,     1,     1,     3,     4,     4,
       4,     3,     0,     2,     0,     5,     0,     2,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     3,     1,     1,     2,     1,     2,
       1,     1,     0,     2,     2,     0,     2,     4,     4,     0,
       3,     1,     1,     3,     6,     2,     3,     2,     2,     3,
       2,     1,     2,     2,     1,     1,     1,     2,     2,     1,
       4,     2,     3,     0,     0,     5,     0,     1,     2,     3,
       1,     0,     4,     3,     0,     2,     2,     2,     1,     1,
       2,     2,     1,     1,     1,     1,     1,     1,     1,     3,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       4,     1,     1,     2,     2,     3,     3,     3,     1,     1,
       1,     1,     1,     1,     1,     1,     2,     2,     2,     1,
       2,     1,     2,     1,     1,     1,     1,     4,     0,     1,
       1,     4,     0,     1,     1,     3,     2,     0,     0,     0,
      10,     0,     4,     0,     0,     3,     0,     3,     1,     2,
       4,     0,     2,     2,     0,     3,     3,     4,     2,     1,
       3,     0,     1,     0,     2,     2,     0,     0,     7,     0,
       2,     1,     1,     2,     1,     1,     0,     6,     0,     2,
       2,     1,     0,     1,     0,     0,     3,     0,     2,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     2,     2,     0,     4,     0,
       4,     0,     5,     3,     3,     4,     3,     4,     3,     3,
       4,     4,     3,     4,     3,     4,     5,     3,     4,     3,
       3,     3,     1,     1,     0,     1,     1,     2,     1,     1,
       1,     2,     3,     1,     2,     1,     3,     1,     2,     2,
       2,     2,     3,     3,     3,     3,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     4,     1,     1,     1,     1,     4,     3,     1,
       2,     1,     1,     3,     3,     3,     3,     3,     3,     2,
       1,     1,     1,     1,     1,     1,     1,     0,     1,     0,
       4,     4,     5,     6,     8,     0,     2,     0,     1,     0,
       3,     3,     4,     0,     2,     0,     3,     1,     2,     4,
       0,     2,     0,     4,     0,     8,     0,     1,     1,     1,
       1,     1,     2,     0,     2,     1,     1,     0,     0,     3,
       1,     2,     2,     3,     0,     2,     2,     2,     0,     3,
       2,     2,     4,     1,     1,     1,     1,     0,     2,     2,
       0,     1,     2,     2,     0,     1,     2,     0,     1,     0,
       3,     1,     2,     1,     1,     0,     3,     1,     1,     2,
       3,     0,     1,     3,     3,     2,     0,     4,     0,     3,
       0,     4,     4,     0,     1,     1,     1,     0,     3,     2,
       1,     0,     4,     4,     2,     1,     2,     0,     1,     0,
       3,     3,     0,     3,     0,     2,     1,     2,     1,     0,
       4,     3,     3,     3,     3,     2,     1,     1,     1,     1,
       2,     1,     1,     2,     0,     3,     1,     1,     1,     2,
       1,     2,     1,     1,     2,     2,     2,     2,     2,     1,
       1,     0,     5,     0,     1,     1,     2,     3,     3,     3,
       3,     2,     0,     0,     5,     1,     1,     0,     0,     7,
       0,     5,     1,     1,     1,     0,     1,     0,     2,     1,
       2,     1,     2,     1,     3,     1,     1,     2,     3,     3,
       1,     1,     1,     1,     4,     3,     1,     2,     2,     1,
       1,     2,     2,     1,     1,     1,     1,     1,     1,     1,
       1,     3,     1,     3,     3,     3,     3,     3,     0,     1,
       0,     4,     4,     6,     6,     8,     8,     0,     1,     0,
       3,     0,     3,     3,     0,     4,     2,     1,     3,     1,
       1,     1,     2,     1,     1,     2,     2,     3,     2,     3,
       1,     3,     2,     1,     1,     1,     0,     2,     0,     1,
       0,     3,     0,     2,     1,     2,     1,     1,     1,     0,
       2,     0,     3,     1,     0,     3,     1,     0,     3,     3,
       0,     3,     2,     0,     6,     5,     3,     2,     0,     1,
       0,     0,     0,     1,     0,     3,     5,     0,     2,     0,
       3,     3,     0,     2,     1,     2,     4,     1,     1,     1,
       1,     1,     1,     1,     0,     3,     0,     3,     1,     2,
       0,     3,     2,     1,     1,     1,     2,     1,     1,     1,
       0,     3,     2,     5,     1,     2,     2,     2,     1,     1,
       1,     2,     1,     2,     4,     2,     0,     1,     1,     1,
       1,     4,     0,     1,     1,     2,     2,     3,     3,     0,
       3,     0,     3,     3,     4,     0,     4,     4,     6,     0,
       1,     0,     3,     1,     2,     5,     1,     1,     1,     1,
       0,     3,     0,     3,     2,     1,     0,     3,     4,     0,
       6,     4,     0,     1,     1,     1,     1,     3,     0,     2,
       1,     3,     3,     0,     3,     1,     1,     1,     3,     7,
       0,     3,     0,     4,     7,     0,     2,     0,     1,     2,
       1,     2,     3,     3,     1,     0,     1,     1,     4,     4,
       2,     0,     1,     1,     3,     2,     0,     3,     1,     1,
       0,     1,     1,     0,     4,     5,     1,     1,     0,     2,
       2,     0,     1,     2,     0,     1,     2,     0,     1,     0,
       3,     2,     1,     0,     4,     4,     0,     1,     0,     4,
       5,     0,     1,     2,     3,     0,     1,     1,     0,     4,
       4,     6,     0,     2,     0,     2,     1,     2,     3,     0,
       1,     0,     3,     2,     5,     0,     1,     2,     2,     2,
       2,     2,     0,     2,     0,     3,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     4,     3,
       1,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       4,     3,     5,     4,     1,     2,     3,     1,     2,     3,
       3,     4,     4,     0,     3,     0,     7,     0,     5,     0,
       2,     0,     2,     0,     3,     0,     2,     4,     0,     2,
       4,     0,     4,     4,     0,     3,     0,     4,     1,     1,
       1,     2,     2,     2,     2,     1,     1,     2,     1,     0,
       1,     0,     4,     2,     2,     0,     2,     1,     4,     4,
       0,     1,     1,     1,     1,     1,     1,     1,     0,     4,
       5,     1,     2,     2,     0,     3,     1,     1,     0,     4,
       0,     1,     0,     4,     4,     6,     6,     8,     0,     1,
       2,     0,     1,     0,     3,     1,     2,     0,     3,     5,
       0,     3,     2,     0,     4,     6,     0,     3,     1,     3,
       2,     2,     2,     3,     0,     3,     0,     3,     0,     3,
       0,     1,     0,     3,     1,     1,     1,     1,     1,     7,
       0,     1,     1,     1,     1,     1,     1,     4,     1,     2,
       1,     2,     3,     0,     1,     2,     1,     3,     1,     1,
       4,     1,     1,     1,     0,     4,     6,     0,     2,     0,
       4,     3,     3,     1,     1,     0,     1,     1,     0,     1,
       0,     2,     2,     0,     1,     2,     1,     1,     0,     1,
       2,     1,     1,     0,     2,     2,     0,     1,     2,     0,
       1,     2,     0,     2,     2,     0,     1,     2,     0,     1,
       2,     0,     2,     2,     0,     1,     2,     0,     1,     2,
       2,     2,     2,     2,     0,     1,     2,     0,     1,     2,
       2,     2,     0,     1,     2,     0,     1,     2,     0,     1,
       2,     2,     0,     1,     2,     0,     1,     2,     0,     2,
       0,     3,     2,     1,     0,     2,     1,     1,     0,     2,
       1,     2,     1,     2,     3,     3,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     0,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     2,     1,     1,     1,     1,     1,
       1,     1,     3,     0,     1,     1,     3,     3,     1,     3,
       3,     1,     3,     1,     2,     2,     1,     3,     1,     1,
       3,     1,     3,     1,     3,     1,     2,     2,     1,     1,
       2,     1,     1,     2,     1,     1,     1,     1,     2,     1,
       0,     2,     1,     1,     1,     3,     1,     1,     2,     1,
       1,     1,     2,     1,     1,     1,     1,     1,     1,     1,
       2,     1,     1,     3,     0,     1,     1,     2,     1,     1,
       1,     1,     1,     1,     1,     2,     2,     2,     4,     3,
       1,     1,     2,     1,     1,     1,     1,     1,     1,     1,
       2,     2,     2,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     2,     1,     1,     1,     1,
       3,     2,     2,     1,     1,     2,     1,     3,     2,     2,
       1,     1,     3,     3,     4,     5,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     2,     1,     3,
       1,     1,     1,     1,     1,     1,     1,     2,     5,     5,
       5,     4,     5,     4,     5,     5,     5,     5,     5,     2,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       0,     4,     5,     0,     3,     2,     1,     3,     3,     0,
       2,     1,     3,     1,     3,     1,     3,     1,     3,     0,
       0,     1,     0,     1,     0,     1,     0,     2,     0,     2,
       0,     1,     1,     0,     1,     0,     1,     2,     0,     2,
       0,     3,     1,     1,     1,     1,     1,     1,     1,     1,
       0,     2,     0,     5,     0,     3,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     0,
       1,     0,     1,     0,     1,     0,     1,     0,     1,     1,
       0,     1,     0,     1,     0,     1,     0,     1,     0,     1,
       0,     1,     0,     1,     0,     1,     0,     1,     0,     1,
       0,     1,     0,     3,     0,     1,     0,     1,     0,     1,
       0,     1,     0,     1,     0,     1,     2,     0,     1,     0,
       1,     0,     1,     0,     1,     0,     1,     0,     1,     1,
       0,     1,     1,     0,     1,     1,     0,     1,     0,     1,
       1,     0,     1,     0,     1,     1,     0,     2,     2,     0,
       1,     0,     1,     0,     1,     0,     1,     0,     1,     1,
       0,     1,     0,     1,     0,     2,     1,     1,     0,     1,
       0,     1,     0,     1,     0,     1,     0,     1,     1,     0,
       1,     0,     1,     0,     1,     0,     1,     2,     0,     1,
       0,     1,     0,     1,     0,     1,     0,     1,     0,     1,
       0,     1,     0,     1,     0,     1,     0,     1,     1,     0,
       1,     0,     3,     0,     1,     2,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     2,     2,     1,     1,
       1,     1,     1,     1,     2,     1,     3,     2,     1,     1,
       1,     2,     1,     2,     1,     2,     1,     2,     1,     2,
       1,     2,     1,     2,     2
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                                              );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            /* Fall through.  */
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        YYSTYPE *yyvs1 = yyvs;
        yytype_int16 *yyss1 = yyss;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yystacksize);

        yyss = yyss1;
        yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yytype_int16 *yyss1 = yyss;
        union yyalloc *yyptr =
          (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 2422 "parser.y" /* yacc.c:1646  */
    {
	clear_initial_values ();
	current_program = NULL;
	defined_prog_list = NULL;
	cobc_cs_check = 0;
	main_flag_set = 0;
	current_program = cb_build_program (NULL, 0);
	cb_set_intr_when_compiled ();
	cb_build_registers ();
  }
#line 8765 "parser.c" /* yacc.c:1646  */
    break;

  case 3:
#line 2433 "parser.y" /* yacc.c:1646  */
    {
	if (!current_program->flag_validated) {
		current_program->flag_validated = 1;
		cb_validate_program_body (current_program);
	}
	if (depth > 1) {
		cb_error (_("multiple PROGRAM-ID's without matching END PROGRAM"));
	}
	if (cobc_flag_main && !main_flag_set) {
		cb_error (_("executable requested but no program found"));
	}
	if (errorcount > 0) {
		YYABORT;
	}
	if (!current_program->entry_list) {
		emit_entry (current_program->program_id, 0, NULL, NULL);
	}
  }
#line 8788 "parser.c" /* yacc.c:1646  */
    break;

  case 6:
#line 2459 "parser.y" /* yacc.c:1646  */
    {
	first_prog = 1;
	depth = 0;
	setup_from_identification = 0;
  }
#line 8798 "parser.c" /* yacc.c:1646  */
    break;

  case 12:
#line 2478 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		l;

	current_section = NULL;
	current_paragraph = NULL;
	l = cb_build_alphanumeric_literal (demangle_name,
					   strlen (demangle_name));
	current_program->program_id = cb_build_program_id (l, NULL, 0);
	current_program->prog_type = CB_PROGRAM_TYPE;
	if (!main_flag_set) {
		main_flag_set = 1;
		current_program->flag_main = cobc_flag_main;
	}
	check_relaxed_syntax (COBC_HD_PROGRAM_ID);
  }
#line 8818 "parser.c" /* yacc.c:1646  */
    break;

  case 13:
#line 2495 "parser.y" /* yacc.c:1646  */
    {
	clean_up_program (NULL, CB_PROGRAM_TYPE);
  }
#line 8826 "parser.c" /* yacc.c:1646  */
    break;

  case 16:
#line 2522 "parser.y" /* yacc.c:1646  */
    {
	clean_up_program (NULL, CB_PROGRAM_TYPE);
  }
#line 8834 "parser.c" /* yacc.c:1646  */
    break;

  case 20:
#line 2535 "parser.y" /* yacc.c:1646  */
    {
	first_nested_program = 0;
	clean_up_program ((yyvsp[-1]), CB_PROGRAM_TYPE);
  }
#line 8843 "parser.c" /* yacc.c:1646  */
    break;

  case 21:
#line 2543 "parser.y" /* yacc.c:1646  */
    {
	clean_up_program ((yyvsp[-1]), CB_FUNCTION_TYPE);
  }
#line 8851 "parser.c" /* yacc.c:1646  */
    break;

  case 24:
#line 2561 "parser.y" /* yacc.c:1646  */
    {
	setup_program_start ();
	setup_from_identification = 1;
  }
#line 8860 "parser.c" /* yacc.c:1646  */
    break;

  case 27:
#line 2574 "parser.y" /* yacc.c:1646  */
    {
	cobc_in_id = 1;
  }
#line 8868 "parser.c" /* yacc.c:1646  */
    break;

  case 28:
#line 2578 "parser.y" /* yacc.c:1646  */
    {
	if (setup_program ((yyvsp[-1]), (yyvsp[0]), CB_PROGRAM_TYPE)) {
		YYABORT;
	}

	setup_prototype ((yyvsp[-1]), (yyvsp[0]), CB_PROGRAM_TYPE, 1);
  }
#line 8880 "parser.c" /* yacc.c:1646  */
    break;

  case 29:
#line 2586 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
	cobc_in_id = 0;
  }
#line 8889 "parser.c" /* yacc.c:1646  */
    break;

  case 30:
#line 2594 "parser.y" /* yacc.c:1646  */
    {
	cobc_in_id = 1;
  }
#line 8897 "parser.c" /* yacc.c:1646  */
    break;

  case 31:
#line 2598 "parser.y" /* yacc.c:1646  */
    {
	if (setup_program ((yyvsp[-2]), (yyvsp[-1]), CB_FUNCTION_TYPE)) {
		YYABORT;
	}
	setup_prototype ((yyvsp[-2]), (yyvsp[-1]), CB_FUNCTION_TYPE, 1);
	cobc_cs_check = 0;
	cobc_in_id = 0;
  }
#line 8910 "parser.c" /* yacc.c:1646  */
    break;

  case 32:
#line 2610 "parser.y" /* yacc.c:1646  */
    {
	if (CB_REFERENCE_P ((yyvsp[0])) && CB_WORD_COUNT ((yyvsp[0])) > 0) {
		redefinition_error ((yyvsp[0]));
	}
	/*
	  The program name is a key part of defining the current_program, so we
	  mustn't lose it (unlike in undefined_word).
	*/
	(yyval) = (yyvsp[0]);
  }
#line 8925 "parser.c" /* yacc.c:1646  */
    break;

  case 33:
#line 2621 "parser.y" /* yacc.c:1646  */
    {
	cb_trim_program_id ((yyvsp[0]));
  }
#line 8933 "parser.c" /* yacc.c:1646  */
    break;

  case 35:
#line 2629 "parser.y" /* yacc.c:1646  */
    {
	cb_trim_program_id ((yyvsp[0]));
  }
#line 8941 "parser.c" /* yacc.c:1646  */
    break;

  case 36:
#line 2635 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 8947 "parser.c" /* yacc.c:1646  */
    break;

  case 37:
#line 2636 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 8953 "parser.c" /* yacc.c:1646  */
    break;

  case 40:
#line 2645 "parser.y" /* yacc.c:1646  */
    {
	if (!current_program->nested_level) {
		cb_error (_("COMMON may only be used in a contained program"));
	} else {
		current_program->flag_common = 1;
		cb_add_common_prog (current_program);
	}
  }
#line 8966 "parser.c" /* yacc.c:1646  */
    break;

  case 41:
#line 2654 "parser.y" /* yacc.c:1646  */
    {
	if (!current_program->nested_level) {
		cb_error (_("COMMON may only be used in a contained program"));
	} else {
		current_program->flag_common = 1;
		cb_add_common_prog (current_program);
	}
  }
#line 8979 "parser.c" /* yacc.c:1646  */
    break;

  case 43:
#line 2664 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING (_("CALL prototypes"));
  }
#line 8987 "parser.c" /* yacc.c:1646  */
    break;

  case 46:
#line 2676 "parser.y" /* yacc.c:1646  */
    {
	current_program->flag_initial = 1;
  }
#line 8995 "parser.c" /* yacc.c:1646  */
    break;

  case 47:
#line 2680 "parser.y" /* yacc.c:1646  */
    {
	current_program->flag_recursive = 1;
  }
#line 9003 "parser.c" /* yacc.c:1646  */
    break;

  case 49:
#line 2689 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
  }
#line 9011 "parser.c" /* yacc.c:1646  */
    break;

  case 51:
#line 2703 "parser.y" /* yacc.c:1646  */
    {
	default_rounded_mode = cb_int (COB_STORE_ROUND);
  }
#line 9019 "parser.c" /* yacc.c:1646  */
    break;

  case 52:
#line 2707 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		default_rounded_mode = (yyvsp[0]);
	} else {
		default_rounded_mode = cb_int (COB_STORE_ROUND);
	}
  }
#line 9031 "parser.c" /* yacc.c:1646  */
    break;

  case 54:
#line 2719 "parser.y" /* yacc.c:1646  */
    {
	current_program->entry_convention = (yyvsp[0]);
	current_program->entry_convention->source_file = cb_source_file;
	current_program->entry_convention->source_line = cb_source_line;
  }
#line 9041 "parser.c" /* yacc.c:1646  */
    break;

  case 55:
#line 2728 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (CB_CONV_COBOL);
  }
#line 9049 "parser.c" /* yacc.c:1646  */
    break;

  case 56:
#line 2732 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (0);
  }
#line 9057 "parser.c" /* yacc.c:1646  */
    break;

  case 57:
#line 2736 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (CB_CONV_STDCALL);
  }
#line 9065 "parser.c" /* yacc.c:1646  */
    break;

  case 59:
#line 2744 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("INTERMEDIATE ROUNDING");
  }
#line 9073 "parser.c" /* yacc.c:1646  */
    break;

  case 60:
#line 2751 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_NEAR_AWAY_FROM_ZERO);
  }
#line 9081 "parser.c" /* yacc.c:1646  */
    break;

  case 61:
#line 2755 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_NEAR_EVEN);
  }
#line 9089 "parser.c" /* yacc.c:1646  */
    break;

  case 62:
#line 2759 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_PROHIBITED);
  }
#line 9097 "parser.c" /* yacc.c:1646  */
    break;

  case 63:
#line 2763 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_TRUNCATION);
  }
#line 9105 "parser.c" /* yacc.c:1646  */
    break;

  case 66:
#line 2778 "parser.y" /* yacc.c:1646  */
    {
	header_check |= COBC_HD_ENVIRONMENT_DIVISION;
  }
#line 9113 "parser.c" /* yacc.c:1646  */
    break;

  case 70:
#line 2793 "parser.y" /* yacc.c:1646  */
    {
	cb_verify (cb_incorrect_conf_sec_order,
		   _("incorrect order of CONFIGURATION SECTION paragraphs"));
  }
#line 9122 "parser.c" /* yacc.c:1646  */
    break;

  case 82:
#line 2822 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION, 0, 0, 0);
	header_check |= COBC_HD_CONFIGURATION_SECTION;
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "CONFIGURATION SECTION");
	}
  }
#line 9134 "parser.c" /* yacc.c:1646  */
    break;

  case 88:
#line 2840 "parser.y" /* yacc.c:1646  */
    {
	cb_verify (cb_incorrect_conf_sec_order,
		   _("incorrect order of SOURCE- and OBJECT-COMPUTER paragraphs"));
  }
#line 9143 "parser.c" /* yacc.c:1646  */
    break;

  case 89:
#line 2851 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION, 0, 0);
  }
#line 9152 "parser.c" /* yacc.c:1646  */
    break;

  case 94:
#line 2865 "parser.y" /* yacc.c:1646  */
    {
	current_program->flag_debugging = 1;
	needs_debug_item = 1;
	cobc_cs_check = 0;
	cb_build_debug_item ();
  }
#line 9163 "parser.c" /* yacc.c:1646  */
    break;

  case 95:
#line 2877 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION, 0, 0);
  }
#line 9172 "parser.c" /* yacc.c:1646  */
    break;

  case 96:
#line 2882 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
  }
#line 9180 "parser.c" /* yacc.c:1646  */
    break;

  case 107:
#line 2908 "parser.y" /* yacc.c:1646  */
    {
	cb_verify (cb_memory_size_clause, "MEMORY SIZE");
  }
#line 9188 "parser.c" /* yacc.c:1646  */
    break;

  case 108:
#line 2916 "parser.y" /* yacc.c:1646  */
    {
	current_program->collating_sequence = (yyvsp[0]);
  }
#line 9196 "parser.c" /* yacc.c:1646  */
    break;

  case 109:
#line 2923 "parser.y" /* yacc.c:1646  */
    {
	int segnum;
	
	if (cb_verify (cb_section_segments, "SEGMENT LIMIT")) {
		segnum = cb_get_int ((yyvsp[0]));
		if (segnum == 0 || segnum > 49) {
			cb_error (_("segment-number must be in range of values 1 to 49"));
			(yyval) = NULL;
		}
	}
	/* Ignore */
  }
#line 9213 "parser.c" /* yacc.c:1646  */
    break;

  case 110:
#line 2939 "parser.y" /* yacc.c:1646  */
    {
	if (current_program->classification) {
		cb_error (_("duplicate CLASSIFICATION clause"));
	} else {
		current_program->classification = (yyvsp[0]);
	}
  }
#line 9225 "parser.c" /* yacc.c:1646  */
    break;

  case 111:
#line 2950 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 9233 "parser.c" /* yacc.c:1646  */
    break;

  case 112:
#line 2954 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 9241 "parser.c" /* yacc.c:1646  */
    break;

  case 113:
#line 2958 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 9249 "parser.c" /* yacc.c:1646  */
    break;

  case 114:
#line 2962 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 9257 "parser.c" /* yacc.c:1646  */
    break;

  case 119:
#line 2980 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION, 0, 0);
  }
#line 9266 "parser.c" /* yacc.c:1646  */
    break;

  case 120:
#line 2985 "parser.y" /* yacc.c:1646  */
    {
	cobc_in_repository = 0;
  }
#line 9274 "parser.c" /* yacc.c:1646  */
    break;

  case 123:
#line 2993 "parser.y" /* yacc.c:1646  */
    {
	yyerrok;
  }
#line 9282 "parser.c" /* yacc.c:1646  */
    break;

  case 126:
#line 3005 "parser.y" /* yacc.c:1646  */
    {
	functions_are_all = 1;
  }
#line 9290 "parser.c" /* yacc.c:1646  */
    break;

  case 127:
#line 3009 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[-1]) != cb_error_node) {
		setup_prototype ((yyvsp[-1]), (yyvsp[0]), CB_FUNCTION_TYPE, 0);
	}
  }
#line 9300 "parser.c" /* yacc.c:1646  */
    break;

  case 129:
#line 3016 "parser.y" /* yacc.c:1646  */
    {
	  if ((yyvsp[-1]) != cb_error_node
	      && cb_verify (cb_program_prototypes, _("PROGRAM phrase"))) {
		setup_prototype ((yyvsp[-1]), (yyvsp[0]), CB_PROGRAM_TYPE, 0);
	}
  }
#line 9311 "parser.c" /* yacc.c:1646  */
    break;

  case 130:
#line 3023 "parser.y" /* yacc.c:1646  */
    {
	  yyerrok;
  }
#line 9319 "parser.c" /* yacc.c:1646  */
    break;

  case 131:
#line 3030 "parser.y" /* yacc.c:1646  */
    {
	current_program->function_spec_list =
		cb_list_add (current_program->function_spec_list, (yyvsp[0]));
  }
#line 9328 "parser.c" /* yacc.c:1646  */
    break;

  case 132:
#line 3035 "parser.y" /* yacc.c:1646  */
    {
	current_program->function_spec_list =
		cb_list_add (current_program->function_spec_list, (yyvsp[0]));
  }
#line 9337 "parser.c" /* yacc.c:1646  */
    break;

  case 137:
#line 3055 "parser.y" /* yacc.c:1646  */
    {
	check_duplicate = 0;
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION, 0, 0);
	header_check |= COBC_HD_SPECIAL_NAMES;
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	}
  }
#line 9351 "parser.c" /* yacc.c:1646  */
    break;

  case 156:
#line 3101 "parser.y" /* yacc.c:1646  */
    {
	char system_name[16];
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	check_duplicate = 0;
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
		save_tree = NULL;
	} else {
		/* get system name and revert word-combination of scanner.l,
		   if necessary (e.g. SWITCH A <--> SWITCH_A) */
		system_name[15] = 0;
		strncpy(system_name, CB_NAME ((yyvsp[0])), 15);
		if (system_name [6] == '_') {
			system_name [6] = ' ';
		}
		/* lookup system name */
		save_tree = get_system_name (system_name);
		if (!save_tree) {
			cb_error_x ((yyvsp[0]), _("invalid system-name '%s'"), system_name);
		}
	}
  }
#line 9380 "parser.c" /* yacc.c:1646  */
    break;

  case 158:
#line 3130 "parser.y" /* yacc.c:1646  */
    {
	if (save_tree) {
		if (CB_SYSTEM_NAME(save_tree)->token != CB_DEVICE_CONSOLE) {
			cb_error_x (save_tree, _("invalid %s clause"), "");
		} else {
			current_program->flag_console_is_crt = 1;
		}
	}
  }
#line 9394 "parser.c" /* yacc.c:1646  */
    break;

  case 159:
#line 3141 "parser.y" /* yacc.c:1646  */
    {
	if (save_tree) {
		if (CB_SYSTEM_NAME(save_tree)->token != CB_FEATURE_CONVENTION) {
			cb_error_x (save_tree, _("invalid %s clause"), "SPECIAL NAMES");
		} else if (CB_VALID_TREE ((yyvsp[0]))) {
			CB_SYSTEM_NAME(save_tree)->value = (yyvsp[-2]);
			cb_define ((yyvsp[0]), save_tree);
			CB_CHAIN_PAIR (current_program->mnemonic_spec_list,
					(yyvsp[0]), save_tree);
			/* remove non-standard context-sensitive words when identical to mnemonic */
			if (strcasecmp (CB_NAME((yyvsp[0])), "EXTERN") == 0 ||
			    strcasecmp (CB_NAME((yyvsp[0])), "STDCALL") == 0 ||
			    strcasecmp (CB_NAME((yyvsp[0])), "STATIC") == 0) {
				remove_context_sensitivity (CB_NAME((yyvsp[0])), CB_CS_CALL);
			}
		}
	}
  }
#line 9417 "parser.c" /* yacc.c:1646  */
    break;

  case 160:
#line 3160 "parser.y" /* yacc.c:1646  */
    {
	if (save_tree && CB_VALID_TREE ((yyvsp[-1]))) {
		cb_define ((yyvsp[-1]), save_tree);
		CB_CHAIN_PAIR (current_program->mnemonic_spec_list,
				(yyvsp[-1]), save_tree);
	}
  }
#line 9429 "parser.c" /* yacc.c:1646  */
    break;

  case 164:
#line 3176 "parser.y" /* yacc.c:1646  */
    {
	  check_on_off_duplicate = 0;
  }
#line 9437 "parser.c" /* yacc.c:1646  */
    break;

  case 165:
#line 3183 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		x;

	/* cb_define_switch_name checks param validity */
	x = cb_define_switch_name ((yyvsp[0]), save_tree, (yyvsp[-2]) == cb_int1);
	if (x) {
		if ((yyvsp[-2]) == cb_int1) {
			check_repeated ("ON", SYN_CLAUSE_1, &check_on_off_duplicate);
		} else {
			check_repeated ("OFF", SYN_CLAUSE_2, &check_on_off_duplicate);
		}
		CB_CHAIN_PAIR (current_program->mnemonic_spec_list, (yyvsp[0]), x);
	}
  }
#line 9456 "parser.c" /* yacc.c:1646  */
    break;

  case 166:
#line 3198 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		x;

	/* cb_define_switch_name checks param validity */
	x = cb_define_switch_name ((yyvsp[0]), save_tree, (yyvsp[-2]) == cb_int1);
	if (x) {
		if ((yyvsp[-2]) == cb_int1) {
			check_repeated ("ON", SYN_CLAUSE_1, &check_on_off_duplicate);
		} else {
			check_repeated ("OFF", SYN_CLAUSE_2, &check_on_off_duplicate);
		}
		CB_CHAIN_PAIR (current_program->mnemonic_spec_list, (yyvsp[0]), x);
	}
  }
#line 9475 "parser.c" /* yacc.c:1646  */
    break;

  case 167:
#line 3218 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
		(yyval) = NULL;
	} else {
		/* Returns null on error */
		(yyval) = cb_build_alphabet_name ((yyvsp[0]));
	}
  }
#line 9492 "parser.c" /* yacc.c:1646  */
    break;

  case 168:
#line 3231 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[-2])) {
		current_program->alphabet_name_list =
			cb_list_add (current_program->alphabet_name_list, (yyvsp[-2]));
	}
	cobc_cs_check = 0;
  }
#line 9504 "parser.c" /* yacc.c:1646  */
    break;

  case 169:
#line 3242 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[(-1) - (1)])) {
		CB_ALPHABET_NAME ((yyvsp[(-1) - (1)]))->alphabet_type = CB_ALPHABET_NATIVE;
	}
  }
#line 9514 "parser.c" /* yacc.c:1646  */
    break;

  case 170:
#line 3248 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[(-1) - (1)])) {
		CB_ALPHABET_NAME ((yyvsp[(-1) - (1)]))->alphabet_type = CB_ALPHABET_ASCII;
	}
  }
#line 9524 "parser.c" /* yacc.c:1646  */
    break;

  case 171:
#line 3254 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[(-1) - (1)])) {
		CB_ALPHABET_NAME ((yyvsp[(-1) - (1)]))->alphabet_type = CB_ALPHABET_ASCII;
	}
  }
#line 9534 "parser.c" /* yacc.c:1646  */
    break;

  case 172:
#line 3260 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[(-1) - (1)])) {
		CB_ALPHABET_NAME ((yyvsp[(-1) - (1)]))->alphabet_type = CB_ALPHABET_EBCDIC;
	}
  }
#line 9544 "parser.c" /* yacc.c:1646  */
    break;

  case 173:
#line 3266 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[(-1) - (1)])) {
		CB_ALPHABET_NAME ((yyvsp[(-1) - (1)]))->alphabet_type = CB_ALPHABET_ASCII;
	}
  }
#line 9554 "parser.c" /* yacc.c:1646  */
    break;

  case 174:
#line 3272 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[(-1) - (1)])) {
		CB_ALPHABET_NAME ((yyvsp[(-1) - (1)]))->alphabet_type = CB_ALPHABET_CUSTOM;
		CB_ALPHABET_NAME ((yyvsp[(-1) - (1)]))->custom_list = (yyvsp[0]);
	}
  }
#line 9565 "parser.c" /* yacc.c:1646  */
    break;

  case 175:
#line 3282 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
  }
#line 9573 "parser.c" /* yacc.c:1646  */
    break;

  case 176:
#line 3286 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0]));
  }
#line 9581 "parser.c" /* yacc.c:1646  */
    break;

  case 177:
#line 3293 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 9589 "parser.c" /* yacc.c:1646  */
    break;

  case 178:
#line 3297 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_PAIR ((yyvsp[-2]), (yyvsp[0]));
  }
#line 9597 "parser.c" /* yacc.c:1646  */
    break;

  case 179:
#line 3301 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[-1]));
  }
#line 9605 "parser.c" /* yacc.c:1646  */
    break;

  case 180:
#line 3305 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-1]);
  }
#line 9613 "parser.c" /* yacc.c:1646  */
    break;

  case 181:
#line 3312 "parser.y" /* yacc.c:1646  */
    {
	cb_list_add ((yyvsp[-1]), (yyvsp[0]));
  }
#line 9621 "parser.c" /* yacc.c:1646  */
    break;

  case 182:
#line 3316 "parser.y" /* yacc.c:1646  */
    {
	cb_list_add ((yyvsp[-3]), (yyvsp[0]));
  }
#line 9629 "parser.c" /* yacc.c:1646  */
    break;

  case 183:
#line 3322 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 9635 "parser.c" /* yacc.c:1646  */
    break;

  case 184:
#line 3323 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_space; }
#line 9641 "parser.c" /* yacc.c:1646  */
    break;

  case 185:
#line 3324 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_zero; }
#line 9647 "parser.c" /* yacc.c:1646  */
    break;

  case 186:
#line 3325 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_quote; }
#line 9653 "parser.c" /* yacc.c:1646  */
    break;

  case 187:
#line 3326 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_norm_high; }
#line 9659 "parser.c" /* yacc.c:1646  */
    break;

  case 188:
#line 3327 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_norm_low; }
#line 9665 "parser.c" /* yacc.c:1646  */
    break;

  case 189:
#line 3331 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_space; }
#line 9671 "parser.c" /* yacc.c:1646  */
    break;

  case 190:
#line 3332 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_zero; }
#line 9677 "parser.c" /* yacc.c:1646  */
    break;

  case 191:
#line 3340 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else if ((yyvsp[-1])) {
		CB_CHAIN_PAIR (current_program->symbolic_char_list, (yyvsp[-1]), (yyvsp[0]));
	}
  }
#line 9692 "parser.c" /* yacc.c:1646  */
    break;

  case 192:
#line 3354 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 9700 "parser.c" /* yacc.c:1646  */
    break;

  case 193:
#line 3358 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 9708 "parser.c" /* yacc.c:1646  */
    break;

  case 194:
#line 3366 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 9716 "parser.c" /* yacc.c:1646  */
    break;

  case 195:
#line 3373 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 9724 "parser.c" /* yacc.c:1646  */
    break;

  case 196:
#line 3377 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		(yyval) = cb_list_append ((yyvsp[-1]), (yyvsp[0]));
	} else {
		(yyval) = (yyvsp[-1]);
	}
  }
#line 9736 "parser.c" /* yacc.c:1646  */
    break;

  case 197:
#line 3388 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		l1;
	cb_tree		l2;

	if (cb_list_length ((yyvsp[-2])) != cb_list_length ((yyvsp[0]))) {
		cb_error (_("invalid %s clause"), "SYMBOLIC");
		(yyval) = NULL;
	} else {
		l1 = (yyvsp[-2]);
		l2 = (yyvsp[0]);
		for (; l1; l1 = CB_CHAIN (l1), l2 = CB_CHAIN (l2)) {
			CB_PURPOSE (l1) = CB_VALUE (l2);
		}
		(yyval) = (yyvsp[-2]);
	}
  }
#line 9757 "parser.c" /* yacc.c:1646  */
    break;

  case 198:
#line 3408 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0]) == NULL) {
		(yyval) = NULL;
	} else {
		(yyval) = CB_LIST_INIT ((yyvsp[0]));
	}
  }
#line 9769 "parser.c" /* yacc.c:1646  */
    break;

  case 199:
#line 3416 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0]) == NULL) {
		(yyval) = (yyvsp[-1]);
	} else {
		(yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0]));
	}
  }
#line 9781 "parser.c" /* yacc.c:1646  */
    break;

  case 200:
#line 3426 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 9787 "parser.c" /* yacc.c:1646  */
    break;

  case 201:
#line 3427 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0])); }
#line 9793 "parser.c" /* yacc.c:1646  */
    break;

  case 202:
#line 3434 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		x;

	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		/* Returns null on error */
		x = cb_build_class_name ((yyvsp[-2]), (yyvsp[0]));
		if (x) {
			current_program->class_name_list =
				cb_list_add (current_program->class_name_list, x);
		}
	}
  }
#line 9815 "parser.c" /* yacc.c:1646  */
    break;

  case 203:
#line 3454 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 9821 "parser.c" /* yacc.c:1646  */
    break;

  case 204:
#line 3455 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0])); }
#line 9827 "parser.c" /* yacc.c:1646  */
    break;

  case 205:
#line 3460 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 9835 "parser.c" /* yacc.c:1646  */
    break;

  case 206:
#line 3464 "parser.y" /* yacc.c:1646  */
    {
	if (CB_TREE_CLASS ((yyvsp[-2])) != CB_CLASS_NUMERIC &&
	    CB_LITERAL_P ((yyvsp[-2])) && CB_LITERAL ((yyvsp[-2]))->size != 1) {
		cb_error (_("CLASS literal with THRU must have size 1"));
	}
	if (CB_TREE_CLASS ((yyvsp[0])) != CB_CLASS_NUMERIC &&
	    CB_LITERAL_P ((yyvsp[0])) && CB_LITERAL ((yyvsp[0]))->size != 1) {
		cb_error (_("CLASS literal with THRU must have size 1"));
	}
	if (literal_value ((yyvsp[-2])) <= literal_value ((yyvsp[0]))) {
		(yyval) = CB_BUILD_PAIR ((yyvsp[-2]), (yyvsp[0]));
	} else {
		(yyval) = CB_BUILD_PAIR ((yyvsp[0]), (yyvsp[-2]));
	}
  }
#line 9855 "parser.c" /* yacc.c:1646  */
    break;

  case 207:
#line 3485 "parser.y" /* yacc.c:1646  */
    {
	cb_tree	l;

	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		/* Returns null on error */
		l = cb_build_locale_name ((yyvsp[-2]), (yyvsp[0]));
		if (l) {
			current_program->locale_list =
				cb_list_add (current_program->locale_list, l);
		}
	}
  }
#line 9877 "parser.c" /* yacc.c:1646  */
    break;

  case 208:
#line 3508 "parser.y" /* yacc.c:1646  */
    {
	unsigned char	*s = CB_LITERAL ((yyvsp[-1]))->data;
	unsigned int	error_ind = 0;
	unsigned int	char_seen = 0;

	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		check_repeated ("CURRENCY", SYN_CLAUSE_1, &check_duplicate);
		if (strcmp("$", (const char *)s) != 0) {
			if ((yyvsp[0]) && CB_LITERAL ((yyvsp[-1]))->size != 1) {
				CB_PENDING_X ((yyvsp[-1]), _("CURRENCY SIGN longer than one character"));
				error_ind = 1;
			}
			while (*s) {
				switch (*s) {
				case '0':
				case '1':
				case '2':
				case '3':
				case '4':
				case '5':
				case '6':
				case '7':
				case '8':
				case '9':
				case '+':
				case '-':
				case ',':
				case '.':
				case '*':
					error_ind = 2;
					break;
				case ' ':
					break;
				default:
					char_seen = 1;
					break;
				}
				s++;
			}
			if (!char_seen) {
				error_ind = 2;
			}
		} else {
			if (error_ind > 1) {;
				CB_PENDING_X ((yyvsp[-1]), _("CURRENCY SIGN other than '$'"));
			}
		}
		switch (error_ind) {
		case 0:
		case 1:
			/* FIXME: currency sign/symbol are currently mixed in cobc and libcob */
			/* current_program->currency_sign = CB_LITERAL ($4); */
			break;
		default:
			cb_error_x ((yyvsp[-1]), _("invalid CURRENCY SIGN '%s'"), (char*)CB_LITERAL ((yyvsp[-1]))->data);
			break;
		}
		if ((yyvsp[0])) {
			set_currency_picture_symbol ((yyvsp[0]));
		} else {
			set_currency_picture_symbol ((yyvsp[-1]));
		}
	}
  }
#line 9951 "parser.c" /* yacc.c:1646  */
    break;

  case 209:
#line 3582 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 9959 "parser.c" /* yacc.c:1646  */
    break;

  case 210:
#line 3586 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 9967 "parser.c" /* yacc.c:1646  */
    break;

  case 211:
#line 3595 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		check_repeated ("DECIMAL-POINT", SYN_CLAUSE_2, &check_duplicate);
		current_program->decimal_point = ',';
		current_program->numeric_separator = '.';
	}
  }
#line 9984 "parser.c" /* yacc.c:1646  */
    break;

  case 212:
#line 3614 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		current_program->flag_trailing_separate = 1;
	}
  }
#line 9999 "parser.c" /* yacc.c:1646  */
    break;

  case 213:
#line 3630 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		check_repeated ("CURSOR", SYN_CLAUSE_3, &check_duplicate);
		current_program->cursor_pos = (yyvsp[0]);
	}
  }
#line 10015 "parser.c" /* yacc.c:1646  */
    break;

  case 214:
#line 3648 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		check_repeated ("CRT STATUS", SYN_CLAUSE_4, &check_duplicate);
		current_program->crt_status = (yyvsp[0]);
	}
  }
#line 10031 "parser.c" /* yacc.c:1646  */
    break;

  case 215:
#line 3666 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		check_repeated ("SCREEN CONTROL", SYN_CLAUSE_5, &check_duplicate);
		CB_PENDING ("SCREEN CONTROL");
	}
  }
#line 10047 "parser.c" /* yacc.c:1646  */
    break;

  case 216:
#line 3683 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		check_repeated ("EVENT STATUS", SYN_CLAUSE_6, &check_duplicate);
		CB_PENDING ("EVENT STATUS");
	}
  }
#line 10063 "parser.c" /* yacc.c:1646  */
    break;

  case 217:
#line 3704 "parser.y" /* yacc.c:1646  */
    {
	cb_validate_program_environment (current_program);
  }
#line 10071 "parser.c" /* yacc.c:1646  */
    break;

  case 219:
#line 3711 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION, 0, 0, 0);
	header_check |= COBC_HD_INPUT_OUTPUT_SECTION;
  }
#line 10080 "parser.c" /* yacc.c:1646  */
    break;

  case 221:
#line 3719 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_INPUT_OUTPUT_SECTION, 0, 0);
	header_check |= COBC_HD_FILE_CONTROL;
  }
#line 10090 "parser.c" /* yacc.c:1646  */
    break;

  case 223:
#line 3728 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_INPUT_OUTPUT_SECTION, 0, 0);
	header_check |= COBC_HD_I_O_CONTROL;
  }
#line 10100 "parser.c" /* yacc.c:1646  */
    break;

  case 226:
#line 3743 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_INPUT_OUTPUT_SECTION,
			       COBC_HD_FILE_CONTROL, 0);
	check_duplicate = 0;
	if (CB_VALID_TREE ((yyvsp[0]))) {
		/* Build new file */
		current_file = build_file ((yyvsp[0]));
		current_file->optional = CB_INTEGER ((yyvsp[-1]))->val;

		/* Add file to current program list */
		CB_ADD_TO_CHAIN (CB_TREE (current_file),
				 current_program->file_list);
	} else if (current_program->file_list) {
		current_program->file_list
			= CB_CHAIN (current_program->file_list);
	}
        key_type = NO_KEY;
  }
#line 10124 "parser.c" /* yacc.c:1646  */
    break;

  case 227:
#line 3763 "parser.y" /* yacc.c:1646  */
    {
	if (current_file->organization == COB_ORG_INDEXED
	    && key_type == RELATIVE_KEY) {
		cb_error_x (current_file->key,
			    _("Cannot use RELATIVE KEY clause on INDEXED files"));
	} else if (current_file->organization == COB_ORG_RELATIVE
		   && key_type == RECORD_KEY) {
		cb_error_x (current_file->key,
			    _("Cannot use RECORD KEY clause on RELATIVE files"));
	}
	  
	if (CB_VALID_TREE ((yyvsp[-2]))) {
		validate_file (current_file, (yyvsp[-2]));
	}
  }
#line 10144 "parser.c" /* yacc.c:1646  */
    break;

  case 229:
#line 3783 "parser.y" /* yacc.c:1646  */
    {
	yyerrok;
  }
#line 10152 "parser.c" /* yacc.c:1646  */
    break;

  case 245:
#line 3813 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ASSIGN", SYN_CLAUSE_1, &check_duplicate);
	cobc_cs_check = 0;
	current_file->assign = cb_build_assignment_name (current_file, (yyvsp[0]));
  }
#line 10162 "parser.c" /* yacc.c:1646  */
    break;

  case 246:
#line 3819 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ASSIGN", SYN_CLAUSE_1, &check_duplicate);
	cobc_cs_check = 0;
	if ((yyvsp[0])) {
		current_file->assign = cb_build_assignment_name (current_file, (yyvsp[0]));
	} else {
		current_file->flag_fileid = 1;
	}
  }
#line 10176 "parser.c" /* yacc.c:1646  */
    break;

  case 247:
#line 3829 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ASSIGN", SYN_CLAUSE_1, &check_duplicate);
	cobc_cs_check = 0;
	current_file->organization = COB_ORG_LINE_SEQUENTIAL;
	if ((yyvsp[0])) {
		current_file->assign = cb_build_assignment_name (current_file, (yyvsp[0]));
	} else {
		current_file->flag_fileid = 1;
	}
  }
#line 10191 "parser.c" /* yacc.c:1646  */
    break;

  case 248:
#line 3840 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ASSIGN", SYN_CLAUSE_1, &check_duplicate);
	cobc_cs_check = 0;
	if ((yyvsp[0])) {
		current_file->assign = cb_build_assignment_name (current_file, (yyvsp[0]));
	} else {
		current_file->flag_ext_assign = 0;
		current_file->assign =
			cb_build_alphanumeric_literal ("stdout", (size_t)6);
		current_file->special = COB_SELECT_STDOUT;
	}
  }
#line 10208 "parser.c" /* yacc.c:1646  */
    break;

  case 249:
#line 3853 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ASSIGN", SYN_CLAUSE_1, &check_duplicate);
	cobc_cs_check = 0;
	if ((yyvsp[0])) {
		current_file->assign = cb_build_assignment_name (current_file, (yyvsp[0]));
	} else {
		current_file->flag_ext_assign = 0;
		current_file->assign =
			cb_build_alphanumeric_literal ("stdin", (size_t)5);
		current_file->special = COB_SELECT_STDIN;
	}
  }
#line 10225 "parser.c" /* yacc.c:1646  */
    break;

  case 250:
#line 3866 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ASSIGN", SYN_CLAUSE_1, &check_duplicate);
	cobc_cs_check = 0;
	current_file->organization = COB_ORG_LINE_SEQUENTIAL;
	if ((yyvsp[0])) {
		current_file->assign = cb_build_assignment_name (current_file, (yyvsp[0]));
	} else {
		/* RM/COBOL always expects an assignment name here - we ignore this
		   for PRINTER + PRINTER-1 as ACUCOBOL allows this for using as alias */
		current_file->flag_ext_assign = 0;
		if ((yyvsp[-1]) == cb_int0) {
			current_file->assign =
				cb_build_alphanumeric_literal ("PRINTER", (size_t)7);
		} else if ((yyvsp[-1]) == cb_int1) {
			current_file->assign =
				cb_build_alphanumeric_literal ("PRINTER-1", (size_t)9);
		} else {
			current_file->assign =
				cb_build_alphanumeric_literal ("LPT1", (size_t)4);
		}

	}
  }
#line 10253 "parser.c" /* yacc.c:1646  */
    break;

  case 251:
#line 3898 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int0; }
#line 10259 "parser.c" /* yacc.c:1646  */
    break;

  case 252:
#line 3899 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 10265 "parser.c" /* yacc.c:1646  */
    break;

  case 253:
#line 3900 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int4; }
#line 10271 "parser.c" /* yacc.c:1646  */
    break;

  case 266:
#line 3923 "parser.y" /* yacc.c:1646  */
    {
	current_file->flag_line_adv = 1;
  }
#line 10279 "parser.c" /* yacc.c:1646  */
    break;

  case 268:
#line 3930 "parser.y" /* yacc.c:1646  */
    {
	current_file->flag_ext_assign = 1;
  }
#line 10287 "parser.c" /* yacc.c:1646  */
    break;

  case 272:
#line 3943 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 10295 "parser.c" /* yacc.c:1646  */
    break;

  case 275:
#line 3955 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
	check_repeated ("ACCESS", SYN_CLAUSE_2, &check_duplicate);
  }
#line 10304 "parser.c" /* yacc.c:1646  */
    break;

  case 276:
#line 3962 "parser.y" /* yacc.c:1646  */
    { current_file->access_mode = COB_ACCESS_SEQUENTIAL; }
#line 10310 "parser.c" /* yacc.c:1646  */
    break;

  case 277:
#line 3963 "parser.y" /* yacc.c:1646  */
    { current_file->access_mode = COB_ACCESS_DYNAMIC; }
#line 10316 "parser.c" /* yacc.c:1646  */
    break;

  case 278:
#line 3964 "parser.y" /* yacc.c:1646  */
    { current_file->access_mode = COB_ACCESS_RANDOM; }
#line 10322 "parser.c" /* yacc.c:1646  */
    break;

  case 279:
#line 3972 "parser.y" /* yacc.c:1646  */
    {
	struct cb_alt_key *p;
	struct cb_alt_key *l;

	p = cobc_parse_malloc (sizeof (struct cb_alt_key));
	p->key = (yyvsp[-2]);
	p->duplicates = CB_INTEGER ((yyvsp[-1]))->val;
	p->next = NULL;

	/* Add to the end of list */
	if (current_file->alt_key_list == NULL) {
		current_file->alt_key_list = p;
	} else {
		l = current_file->alt_key_list;
		for (; l->next; l = l->next) { ; }
		l->next = p;
	}
  }
#line 10345 "parser.c" /* yacc.c:1646  */
    break;

  case 280:
#line 3993 "parser.y" /* yacc.c:1646  */
    { }
#line 10351 "parser.c" /* yacc.c:1646  */
    break;

  case 281:
#line 3996 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("SUPPRESS WHEN ALL");
  }
#line 10359 "parser.c" /* yacc.c:1646  */
    break;

  case 282:
#line 4001 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("SUPPRESS WHEN SPACE/ZERO");
  }
#line 10367 "parser.c" /* yacc.c:1646  */
    break;

  case 283:
#line 4011 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("COLLATING", SYN_CLAUSE_3, &check_duplicate);
	CB_PENDING ("COLLATING SEQUENCE");
  }
#line 10376 "parser.c" /* yacc.c:1646  */
    break;

  case 284:
#line 4019 "parser.y" /* yacc.c:1646  */
    {
	if (CB_ALPHABET_NAME_P (cb_ref ((yyvsp[0])))) {
		(yyval) = (yyvsp[0]);
	} else {
		cb_error_x ((yyvsp[0]), _("'%s' is not an alphabet-name"),
			cb_name ((yyvsp[0])));
		(yyval) = cb_error_node;
	}
  }
#line 10390 "parser.c" /* yacc.c:1646  */
    break;

  case 285:
#line 4034 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("STATUS", SYN_CLAUSE_4, &check_duplicate);
	current_file->file_status = (yyvsp[0]);
  }
#line 10399 "parser.c" /* yacc.c:1646  */
    break;

  case 289:
#line 4049 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("LOCK", SYN_CLAUSE_5, &check_duplicate);
  }
#line 10407 "parser.c" /* yacc.c:1646  */
    break;

  case 291:
#line 4057 "parser.y" /* yacc.c:1646  */
    {
	current_file->lock_mode |= COB_LOCK_MANUAL;
	cobc_cs_check = 0;
  }
#line 10416 "parser.c" /* yacc.c:1646  */
    break;

  case 292:
#line 4062 "parser.y" /* yacc.c:1646  */
    {
	current_file->lock_mode |= COB_LOCK_AUTOMATIC;
	cobc_cs_check = 0;
  }
#line 10425 "parser.c" /* yacc.c:1646  */
    break;

  case 293:
#line 4067 "parser.y" /* yacc.c:1646  */
    {
	current_file->lock_mode |= COB_LOCK_EXCLUSIVE;
	cobc_cs_check = 0;
  }
#line 10434 "parser.c" /* yacc.c:1646  */
    break;

  case 296:
#line 4076 "parser.y" /* yacc.c:1646  */
    {
	current_file->lock_mode |= COB_LOCK_MULTIPLE;
  }
#line 10442 "parser.c" /* yacc.c:1646  */
    break;

  case 297:
#line 4080 "parser.y" /* yacc.c:1646  */
    {
	current_file->lock_mode |= COB_LOCK_MULTIPLE;
	CB_PENDING ("WITH ROLLBACK");
  }
#line 10451 "parser.c" /* yacc.c:1646  */
    break;

  case 300:
#line 4096 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ORGANIZATION", SYN_CLAUSE_6, &check_duplicate);
	current_file->organization = COB_ORG_INDEXED;
  }
#line 10460 "parser.c" /* yacc.c:1646  */
    break;

  case 301:
#line 4101 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ORGANIZATION", SYN_CLAUSE_6, &check_duplicate);
	current_file->organization = COB_ORG_SEQUENTIAL;
  }
#line 10469 "parser.c" /* yacc.c:1646  */
    break;

  case 302:
#line 4106 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ORGANIZATION", SYN_CLAUSE_6, &check_duplicate);
	current_file->organization = COB_ORG_RELATIVE;
  }
#line 10478 "parser.c" /* yacc.c:1646  */
    break;

  case 303:
#line 4111 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ORGANIZATION", SYN_CLAUSE_6, &check_duplicate);
	current_file->organization = COB_ORG_LINE_SEQUENTIAL;
  }
#line 10487 "parser.c" /* yacc.c:1646  */
    break;

  case 304:
#line 4122 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("PADDING", SYN_CLAUSE_7, &check_duplicate);
	cb_verify (cb_padding_character_clause, "PADDING CHARACTER");
  }
#line 10496 "parser.c" /* yacc.c:1646  */
    break;

  case 305:
#line 4133 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("RECORD DELIMITER", SYN_CLAUSE_8, &check_duplicate);
  }
#line 10504 "parser.c" /* yacc.c:1646  */
    break;

  case 306:
#line 4143 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("RECORD KEY", SYN_CLAUSE_9, &check_duplicate);
	current_file->key = (yyvsp[0]);
	key_type = RECORD_KEY;
  }
#line 10514 "parser.c" /* yacc.c:1646  */
    break;

  case 307:
#line 4151 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 10520 "parser.c" /* yacc.c:1646  */
    break;

  case 308:
#line 4152 "parser.y" /* yacc.c:1646  */
    { CB_PENDING ("SPLIT KEYS"); }
#line 10526 "parser.c" /* yacc.c:1646  */
    break;

  case 309:
#line 4153 "parser.y" /* yacc.c:1646  */
    { CB_PENDING ("SPLIT KEYS"); }
#line 10532 "parser.c" /* yacc.c:1646  */
    break;

  case 310:
#line 4160 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("RELATIVE KEY", SYN_CLAUSE_10, &check_duplicate);
	current_file->key = (yyvsp[0]);
	key_type = RELATIVE_KEY;
  }
#line 10542 "parser.c" /* yacc.c:1646  */
    break;

  case 311:
#line 4172 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("RESERVE", SYN_CLAUSE_11, &check_duplicate);
  }
#line 10550 "parser.c" /* yacc.c:1646  */
    break;

  case 314:
#line 4186 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SHARING", SYN_CLAUSE_12, &check_duplicate);
	current_file->sharing = (yyvsp[0]);
  }
#line 10559 "parser.c" /* yacc.c:1646  */
    break;

  case 315:
#line 4193 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 10565 "parser.c" /* yacc.c:1646  */
    break;

  case 316:
#line 4194 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_LOCK_OPEN_EXCLUSIVE); }
#line 10571 "parser.c" /* yacc.c:1646  */
    break;

  case 317:
#line 4195 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 10577 "parser.c" /* yacc.c:1646  */
    break;

  case 320:
#line 4204 "parser.y" /* yacc.c:1646  */
    {
	yyerrok;
  }
#line 10585 "parser.c" /* yacc.c:1646  */
    break;

  case 325:
#line 4223 "parser.y" /* yacc.c:1646  */
    {
	cb_tree l;

	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_I_O_CONTROL, 0);
	switch (CB_INTEGER ((yyvsp[-3]))->val) {
	case 0:
		/* SAME AREA */
		break;
	case 1:
		/* SAME RECORD */
		for (l = (yyvsp[0]); l; l = CB_CHAIN (l)) {
			if (CB_VALID_TREE (CB_VALUE (l))) {
				CB_FILE (cb_ref (CB_VALUE (l)))->same_clause = same_area;
			}
		}
		same_area++;
		break;
	case 2:
		/* SAME SORT-MERGE */
		break;
	}
  }
#line 10614 "parser.c" /* yacc.c:1646  */
    break;

  case 326:
#line 4250 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int0; }
#line 10620 "parser.c" /* yacc.c:1646  */
    break;

  case 327:
#line 4251 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 10626 "parser.c" /* yacc.c:1646  */
    break;

  case 328:
#line 4252 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int2; }
#line 10632 "parser.c" /* yacc.c:1646  */
    break;

  case 329:
#line 4253 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int2; }
#line 10638 "parser.c" /* yacc.c:1646  */
    break;

  case 330:
#line 4260 "parser.y" /* yacc.c:1646  */
    {
	/* Fake for TAPE */
	cobc_cs_check = CB_CS_ASSIGN;
  }
#line 10647 "parser.c" /* yacc.c:1646  */
    break;

  case 331:
#line 4265 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_I_O_CONTROL, 0);
	cb_verify (cb_multiple_file_tape_clause, "MULTIPLE FILE TAPE");
	cobc_cs_check = 0;
  }
#line 10659 "parser.c" /* yacc.c:1646  */
    break;

  case 337:
#line 4294 "parser.y" /* yacc.c:1646  */
    {
	current_storage = CB_STORAGE_WORKING;
  }
#line 10667 "parser.c" /* yacc.c:1646  */
    break;

  case 338:
#line 4303 "parser.y" /* yacc.c:1646  */
    {
	cb_validate_program_data (current_program);
  }
#line 10675 "parser.c" /* yacc.c:1646  */
    break;

  case 340:
#line 4310 "parser.y" /* yacc.c:1646  */
    {
	header_check |= COBC_HD_DATA_DIVISION;
  }
#line 10683 "parser.c" /* yacc.c:1646  */
    break;

  case 342:
#line 4319 "parser.y" /* yacc.c:1646  */
    {
	current_storage = CB_STORAGE_FILE;
	check_headers_present (COBC_HD_DATA_DIVISION, 0, 0, 0);
	header_check |= COBC_HD_FILE_SECTION;
  }
#line 10693 "parser.c" /* yacc.c:1646  */
    break;

  case 345:
#line 4333 "parser.y" /* yacc.c:1646  */
    {
	if (CB_VALID_TREE (current_file)) {
		if (CB_VALID_TREE ((yyvsp[0]))) {
			if (current_file->reports) {
				cb_error (_("RECORD description invalid with REPORT"));
			} else {
				finalize_file (current_file, CB_FIELD ((yyvsp[0])));
			}
		} else if (!current_file->reports) {
			cb_error (_("RECORD description missing or invalid"));
		}
	}
  }
#line 10711 "parser.c" /* yacc.c:1646  */
    break;

  case 346:
#line 4352 "parser.y" /* yacc.c:1646  */
    {
	current_storage = CB_STORAGE_FILE;
	check_headers_present (COBC_HD_DATA_DIVISION,
			       COBC_HD_FILE_SECTION, 0, 0);
	check_duplicate = 0;
	if (CB_INVALID_TREE ((yyvsp[0])) || cb_ref ((yyvsp[0])) == cb_error_node) {
		YYERROR;
	}
	current_file = CB_FILE (cb_ref ((yyvsp[0])));
	if (CB_VALID_TREE (current_file)) {
		if ((yyvsp[-1])) {
			current_file->organization = COB_ORG_SORT;
		}
	}
  }
#line 10731 "parser.c" /* yacc.c:1646  */
    break;

  case 348:
#line 4369 "parser.y" /* yacc.c:1646  */
    {
	yyerrok;
  }
#line 10739 "parser.c" /* yacc.c:1646  */
    break;

  case 349:
#line 4376 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 10747 "parser.c" /* yacc.c:1646  */
    break;

  case 350:
#line 4380 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 10755 "parser.c" /* yacc.c:1646  */
    break;

  case 353:
#line 4391 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("EXTERNAL", SYN_CLAUSE_1, &check_duplicate);
#if	0	/* RXWRXW - Global/External */
	if (current_file->flag_global) {
		cb_error (_("file cannot have both EXTERNAL and GLOBAL clauses"));
	}
#endif
	current_file->flag_external = 1;
  }
#line 10769 "parser.c" /* yacc.c:1646  */
    break;

  case 354:
#line 4401 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("GLOBAL", SYN_CLAUSE_2, &check_duplicate);
#if	0	/* RXWRXW - Global/External */
	if (current_file->flag_external) {
		cb_error (_("file cannot have both EXTERNAL and GLOBAL clauses"));
	}
#endif
	if (current_program->prog_type == CB_FUNCTION_TYPE) {
		cb_error (_("%s is invalid in a user FUNCTION"), "GLOBAL");
	} else {
		current_file->flag_global = 1;
		current_program->flag_file_global = 1;
	}
  }
#line 10788 "parser.c" /* yacc.c:1646  */
    break;

  case 364:
#line 4431 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("BLOCK", SYN_CLAUSE_3, &check_duplicate);
	/* ignore */
  }
#line 10797 "parser.c" /* yacc.c:1646  */
    break;

  case 368:
#line 4444 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("RECORD", SYN_CLAUSE_4, &check_duplicate);
	if (current_file->organization == COB_ORG_LINE_SEQUENTIAL) {
		cb_warning (warningopt, _("RECORD clause ignored for LINE SEQUENTIAL"));
	} else {
		current_file->record_max = cb_get_int ((yyvsp[-1]));
		if (current_file->record_max < 1)  {
			current_file->record_max = 1;
			cb_error (_("RECORD clause invalid"));
		}
		if (current_file->organization == COB_ORG_INDEXED) {
			if (current_file->record_max > MAX_FD_RECORD_IDX)  {
				current_file->record_max = MAX_FD_RECORD_IDX;
				cb_error (_("RECORD size (IDX) exceeds maximum allowed (%d)"),
					  MAX_FD_RECORD_IDX);
			}
		} else if (current_file->record_max > MAX_FD_RECORD)  {
			current_file->record_max = MAX_FD_RECORD;
			cb_error (_("RECORD size exceeds maximum allowed (%d)"),
				  MAX_FD_RECORD);
		}
	}
  }
#line 10825 "parser.c" /* yacc.c:1646  */
    break;

  case 369:
#line 4468 "parser.y" /* yacc.c:1646  */
    {
	int	error_ind = 0;

	check_repeated ("RECORD", SYN_CLAUSE_4, &check_duplicate);
	if (current_file->organization == COB_ORG_LINE_SEQUENTIAL) {
		cb_warning (warningopt, _("RECORD clause ignored for LINE SEQUENTIAL"));
	} else {
		current_file->record_min = cb_get_int ((yyvsp[-3]));
		current_file->record_max = cb_get_int ((yyvsp[-1]));
		if (current_file->record_min < 0)  {
			current_file->record_min = 0;
			error_ind = 1;
		}
		if (current_file->record_max < 1)  {
			current_file->record_max = 1;
			error_ind = 1;
		}
		if (current_file->organization == COB_ORG_INDEXED) {
			if (current_file->record_max > MAX_FD_RECORD_IDX)  {
				current_file->record_max = MAX_FD_RECORD_IDX;
				cb_error (_("RECORD size (IDX) exceeds maximum allowed (%d)"),
					  MAX_FD_RECORD_IDX);
			error_ind = 1;
			}
		} else if (current_file->record_max > MAX_FD_RECORD)  {
			current_file->record_max = MAX_FD_RECORD;
			cb_error (_("RECORD size exceeds maximum allowed (%d)"),
				  MAX_FD_RECORD);
			error_ind = 1;
		}
		if (current_file->record_max <= current_file->record_min)  {
			error_ind = 1;
		}
		if (error_ind) {
			cb_error (_("RECORD clause invalid"));
		}
	}
  }
#line 10868 "parser.c" /* yacc.c:1646  */
    break;

  case 370:
#line 4508 "parser.y" /* yacc.c:1646  */
    {
	int	error_ind = 0;

	check_repeated ("RECORD", SYN_CLAUSE_4, &check_duplicate);
	current_file->record_min = (yyvsp[-3]) ? cb_get_int ((yyvsp[-3])) : 0;
	current_file->record_max = (yyvsp[-2]) ? cb_get_int ((yyvsp[-2])) : 0;
	if ((yyvsp[-3]) && current_file->record_min < 0)  {
		current_file->record_min = 0;
		error_ind = 1;
	}
	if ((yyvsp[-2]) && current_file->record_max < 1)  {
		current_file->record_max = 1;
		error_ind = 1;
	}
	if ((yyvsp[-2])) {
		if (current_file->organization == COB_ORG_INDEXED) {
			if (current_file->record_max > MAX_FD_RECORD_IDX)  {
				current_file->record_max = MAX_FD_RECORD_IDX;
				cb_error (_("RECORD size (IDX) exceeds maximum allowed (%d)"),
					  MAX_FD_RECORD_IDX);
				error_ind = 1;
			}
		} else if (current_file->record_max > MAX_FD_RECORD)  {
			current_file->record_max = MAX_FD_RECORD;
			cb_error (_("RECORD size exceeds maximum allowed (%d)"),
				  MAX_FD_RECORD);
			error_ind = 1;
		}
	}
	if (((yyvsp[-3]) || (yyvsp[-2])) && current_file->record_max <= current_file->record_min)  {
		error_ind = 1;
	}
	if (error_ind) {
		cb_error (_("RECORD clause invalid"));
	}
  }
#line 10909 "parser.c" /* yacc.c:1646  */
    break;

  case 372:
#line 4548 "parser.y" /* yacc.c:1646  */
    {
	current_file->record_depending = (yyvsp[0]);
  }
#line 10917 "parser.c" /* yacc.c:1646  */
    break;

  case 373:
#line 4554 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 10923 "parser.c" /* yacc.c:1646  */
    break;

  case 374:
#line 4555 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 10929 "parser.c" /* yacc.c:1646  */
    break;

  case 375:
#line 4559 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 10935 "parser.c" /* yacc.c:1646  */
    break;

  case 376:
#line 4560 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 10941 "parser.c" /* yacc.c:1646  */
    break;

  case 377:
#line 4568 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("LABEL", SYN_CLAUSE_5, &check_duplicate);
	cb_verify (cb_label_records_clause, "LABEL RECORDS");
  }
#line 10950 "parser.c" /* yacc.c:1646  */
    break;

  case 378:
#line 4579 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("VALUE OF", SYN_CLAUSE_6, &check_duplicate);
	cb_verify (cb_value_of_clause, "VALUE OF");
  }
#line 10959 "parser.c" /* yacc.c:1646  */
    break;

  case 379:
#line 4584 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("VALUE OF", SYN_CLAUSE_6, &check_duplicate);
	cb_verify (cb_value_of_clause, "VALUE OF");
	if (!current_file->assign) {
		current_file->assign = cb_build_assignment_name (current_file, (yyvsp[0]));
	}
  }
#line 10971 "parser.c" /* yacc.c:1646  */
    break;

  case 384:
#line 4607 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("DATA", SYN_CLAUSE_7, &check_duplicate);
	cb_verify (cb_data_records_clause, "DATA RECORDS");
  }
#line 10980 "parser.c" /* yacc.c:1646  */
    break;

  case 385:
#line 4619 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("LINAGE", SYN_CLAUSE_8, &check_duplicate);
	if (current_file->organization != COB_ORG_LINE_SEQUENTIAL &&
	    current_file->organization != COB_ORG_SEQUENTIAL) {
		cb_error (_("LINAGE clause with wrong file type"));
	} else {
		current_file->linage = (yyvsp[-2]);
		current_file->organization = COB_ORG_LINE_SEQUENTIAL;
		if (current_linage == 0) {
			linage_file = current_file;
		}
		current_linage++;
	}
  }
#line 10999 "parser.c" /* yacc.c:1646  */
    break;

  case 391:
#line 4647 "parser.y" /* yacc.c:1646  */
    {
	current_file->latfoot = (yyvsp[0]);
  }
#line 11007 "parser.c" /* yacc.c:1646  */
    break;

  case 392:
#line 4654 "parser.y" /* yacc.c:1646  */
    {
	current_file->lattop = (yyvsp[0]);
  }
#line 11015 "parser.c" /* yacc.c:1646  */
    break;

  case 393:
#line 4661 "parser.y" /* yacc.c:1646  */
    {
	current_file->latbot = (yyvsp[0]);
  }
#line 11023 "parser.c" /* yacc.c:1646  */
    break;

  case 394:
#line 4670 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
	check_repeated ("RECORDING", SYN_CLAUSE_9, &check_duplicate);
	/* ignore */
  }
#line 11033 "parser.c" /* yacc.c:1646  */
    break;

  case 399:
#line 4683 "parser.y" /* yacc.c:1646  */
    {
	if (current_file->organization != COB_ORG_SEQUENTIAL) {
		cb_error (_("RECORDING MODE U or S can only be used with RECORD SEQUENTIAL files"));
	}
  }
#line 11043 "parser.c" /* yacc.c:1646  */
    break;

  case 402:
#line 4699 "parser.y" /* yacc.c:1646  */
    {
	struct cb_alphabet_name	*al;

	check_repeated ("CODE SET", SYN_CLAUSE_10, &check_duplicate);

	if (CB_VALID_TREE ((yyvsp[-1]))) {
		al = CB_ALPHABET_NAME (cb_ref ((yyvsp[-1])));
		switch (al->alphabet_type) {
#ifdef	COB_EBCDIC_MACHINE
		case CB_ALPHABET_ASCII:
#else
		case CB_ALPHABET_EBCDIC:
#endif
		case CB_ALPHABET_CUSTOM:
			current_file->code_set = al;
			break;
		default:
			if (CB_VALID_TREE ((yyvsp[-1]))) {
				cb_warning_x (warningopt, (yyvsp[-1]), _("ignoring CODE-SET '%s'"),
						  cb_name ((yyvsp[-1])));
			}
			break;
		}
	}

	if (current_file->organization != COB_ORG_LINE_SEQUENTIAL &&
	    current_file->organization != COB_ORG_SEQUENTIAL) {
		cb_error (_("CODE-SET clause invalid for file type"));
	}

	if (warningopt) {
		CB_PENDING ("CODE-SET");
	}
  }
#line 11082 "parser.c" /* yacc.c:1646  */
    break;

  case 404:
#line 4737 "parser.y" /* yacc.c:1646  */
    {
	  if (warningopt) {
		  CB_PENDING ("FOR sub-records");
	  }

	  current_file->code_set_items = CB_LIST ((yyvsp[0]));
  }
#line 11094 "parser.c" /* yacc.c:1646  */
    break;

  case 405:
#line 4750 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("REPORT", SYN_CLAUSE_11, &check_duplicate);
	CB_PENDING("REPORT WRITER");
	if (current_file->organization != COB_ORG_LINE_SEQUENTIAL &&
	    current_file->organization != COB_ORG_SEQUENTIAL) {
		cb_error (_("REPORT clause with wrong file type"));
	} else {
		current_file->reports = (yyvsp[0]);
		current_file->organization = COB_ORG_LINE_SEQUENTIAL;
	}
  }
#line 11110 "parser.c" /* yacc.c:1646  */
    break;

  case 408:
#line 4770 "parser.y" /* yacc.c:1646  */
    {
	current_report = build_report ((yyvsp[0]));
	current_report->file = current_file;
	CB_ADD_TO_CHAIN (CB_TREE (current_report), current_program->report_list);
	if (report_count == 0) {
		report_instance = current_report;
	}
	report_count++;
  }
#line 11124 "parser.c" /* yacc.c:1646  */
    break;

  case 409:
#line 4780 "parser.y" /* yacc.c:1646  */
    {
	current_report = build_report ((yyvsp[0]));
	CB_ADD_TO_CHAIN (CB_TREE (current_report), current_program->report_list);
	if (report_count == 0) {
		report_instance = current_report;
	}
	report_count++;
  }
#line 11137 "parser.c" /* yacc.c:1646  */
    break;

  case 411:
#line 4794 "parser.y" /* yacc.c:1646  */
    {
	current_storage = CB_STORAGE_COMMUNICATION;
	check_headers_present (COBC_HD_DATA_DIVISION, 0, 0, 0);
	header_check |= COBC_HD_COMMUNICATION_SECTION;
	/* add a compiler configuration if either */
	if (cb_std_define > CB_STD_85) {
		cb_verify (CB_UNCONFORMABLE, _ ("COMMUNICATION SECTION"));
	} else if (cb_verify (CB_OBSOLETE, _("COMMUNICATION SECTION"))) {
		CB_PENDING ("COMMUNICATION SECTION");
	}
  }
#line 11153 "parser.c" /* yacc.c:1646  */
    break;

  case 415:
#line 4815 "parser.y" /* yacc.c:1646  */
    {
	if (CB_VALID_TREE (current_cd)) {
		if (CB_VALID_TREE ((yyvsp[0]))) {
			cb_finalize_cd (current_cd, CB_FIELD ((yyvsp[0])));
		} else if (!current_cd->record) {
			cb_error (_("CD record missing"));
		}
	}
  }
#line 11167 "parser.c" /* yacc.c:1646  */
    break;

  case 416:
#line 4830 "parser.y" /* yacc.c:1646  */
    {
	/* CD internally defines a new file */
	if (CB_VALID_TREE ((yyvsp[0]))) {
		current_cd = cb_build_cd ((yyvsp[0]));

		CB_ADD_TO_CHAIN (CB_TREE (current_cd),
				 current_program->cd_list);
	} else {
		current_cd = NULL;
		/* TO-DO: Is this necessary? */
		if (current_program->cd_list) {
			current_program->cd_list
				= CB_CHAIN (current_program->cd_list);
		}
	}
	check_duplicate = 0;
  }
#line 11189 "parser.c" /* yacc.c:1646  */
    break;

  case 464:
#line 4938 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_DATA_DIVISION, 0, 0, 0);
	header_check |= COBC_HD_WORKING_STORAGE_SECTION;
	current_storage = CB_STORAGE_WORKING;
  }
#line 11199 "parser.c" /* yacc.c:1646  */
    break;

  case 465:
#line 4944 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		CB_FIELD_ADD (current_program->working_storage, CB_FIELD ((yyvsp[0])));
	}
  }
#line 11209 "parser.c" /* yacc.c:1646  */
    break;

  case 466:
#line 4953 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 11217 "parser.c" /* yacc.c:1646  */
    break;

  case 467:
#line 4957 "parser.y" /* yacc.c:1646  */
    {
	current_field = NULL;
	description_field = NULL;
	cb_clear_real_field ();
  }
#line 11227 "parser.c" /* yacc.c:1646  */
    break;

  case 468:
#line 4963 "parser.y" /* yacc.c:1646  */
    {
	struct cb_field *p;

	for (p = description_field; p; p = p->sister) {
		cb_validate_field (p);
	}
	(yyval) = CB_TREE (description_field);
  }
#line 11240 "parser.c" /* yacc.c:1646  */
    break;

  case 474:
#line 4983 "parser.y" /* yacc.c:1646  */
    {
	if (set_current_field ((yyvsp[-1]), (yyvsp[0]))) {
		YYERROR;
	}
  }
#line 11250 "parser.c" /* yacc.c:1646  */
    break;

  case 475:
#line 4989 "parser.y" /* yacc.c:1646  */
    {
	if (!qualifier) {
		current_field->flag_filler = 1;
	}
	if (!description_field) {
		description_field = current_field;
	}
  }
#line 11263 "parser.c" /* yacc.c:1646  */
    break;

  case 476:
#line 4998 "parser.y" /* yacc.c:1646  */
    {
	/* Free tree associated with level number */
	cobc_parse_free ((yyvsp[-2]));
	yyerrok;
	cb_unput_dot ();
	check_pic_duplicate = 0;
	check_duplicate = 0;
	current_field = cb_get_real_field ();
  }
#line 11277 "parser.c" /* yacc.c:1646  */
    break;

  case 477:
#line 5011 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 11285 "parser.c" /* yacc.c:1646  */
    break;

  case 480:
#line 5023 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_filler ();
	qualifier = NULL;
	keys_list = NULL;
	non_const_word = 0;
  }
#line 11296 "parser.c" /* yacc.c:1646  */
    break;

  case 482:
#line 5034 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
	qualifier = (yyvsp[0]);
	keys_list = NULL;
	non_const_word = 0;
  }
#line 11307 "parser.c" /* yacc.c:1646  */
    break;

  case 483:
#line 5044 "parser.y" /* yacc.c:1646  */
    {
	(yyval)= NULL;
  }
#line 11315 "parser.c" /* yacc.c:1646  */
    break;

  case 484:
#line 5048 "parser.y" /* yacc.c:1646  */
    {
	if (current_program->prog_type == CB_FUNCTION_TYPE) {
		cb_error (_("%s is invalid in a user FUNCTION"), "GLOBAL");
		(yyval)= NULL;
	} else {
		(yyval) = cb_null;
	}
  }
#line 11328 "parser.c" /* yacc.c:1646  */
    break;

  case 485:
#line 5059 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 11334 "parser.c" /* yacc.c:1646  */
    break;

  case 486:
#line 5060 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_const_length ((yyvsp[0])); }
#line 11340 "parser.c" /* yacc.c:1646  */
    break;

  case 487:
#line 5061 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_const_length ((yyvsp[0])); }
#line 11346 "parser.c" /* yacc.c:1646  */
    break;

  case 488:
#line 5063 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_const_length ((yyvsp[0])); }
#line 11352 "parser.c" /* yacc.c:1646  */
    break;

  case 489:
#line 5068 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 11360 "parser.c" /* yacc.c:1646  */
    break;

  case 490:
#line 5072 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 11368 "parser.c" /* yacc.c:1646  */
    break;

  case 491:
#line 5076 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int2;
  }
#line 11376 "parser.c" /* yacc.c:1646  */
    break;

  case 492:
#line 5080 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int4;
  }
#line 11384 "parser.c" /* yacc.c:1646  */
    break;

  case 493:
#line 5084 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (8);
  }
#line 11392 "parser.c" /* yacc.c:1646  */
    break;

  case 494:
#line 5088 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int ((int)sizeof(long));
  }
#line 11400 "parser.c" /* yacc.c:1646  */
    break;

  case 495:
#line 5092 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int ((int)sizeof(void *));
  }
#line 11408 "parser.c" /* yacc.c:1646  */
    break;

  case 496:
#line 5096 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int ((int)sizeof(float));
  }
#line 11416 "parser.c" /* yacc.c:1646  */
    break;

  case 497:
#line 5100 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int ((int)sizeof(double));
  }
#line 11424 "parser.c" /* yacc.c:1646  */
    break;

  case 498:
#line 5104 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (4);
  }
#line 11432 "parser.c" /* yacc.c:1646  */
    break;

  case 499:
#line 5108 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (8);
  }
#line 11440 "parser.c" /* yacc.c:1646  */
    break;

  case 500:
#line 5112 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (16);
  }
#line 11448 "parser.c" /* yacc.c:1646  */
    break;

  case 501:
#line 5116 "parser.y" /* yacc.c:1646  */
    {
	yyerrok;
	cb_unput_dot ();
	check_pic_duplicate = 0;
	check_duplicate = 0;
	current_field = cb_get_real_field ();
  }
#line 11460 "parser.c" /* yacc.c:1646  */
    break;

  case 511:
#line 5148 "parser.y" /* yacc.c:1646  */
    {
	if (set_current_field ((yyvsp[-4]), (yyvsp[-3]))) {
		YYERROR;
	}

	if (cb_ref ((yyvsp[-1])) != cb_error_node) {
		error_if_invalid_level_for_renames ((yyvsp[-1]));
		current_field->redefines = CB_FIELD (cb_ref ((yyvsp[-1])));
	}

	if ((yyvsp[0])) {
		error_if_invalid_level_for_renames ((yyvsp[0]));
		current_field->rename_thru = CB_FIELD (cb_ref ((yyvsp[0])));
	} else {
		/* If there is no THRU clause, RENAMES acts like REDEFINES. */
		current_field->pic = current_field->redefines->pic;
	}

	cb_validate_renames_item (current_field);
  }
#line 11485 "parser.c" /* yacc.c:1646  */
    break;

  case 512:
#line 5172 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 11493 "parser.c" /* yacc.c:1646  */
    break;

  case 513:
#line 5176 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]) == cb_error_node ? NULL : (yyvsp[0]);
  }
#line 11501 "parser.c" /* yacc.c:1646  */
    break;

  case 514:
#line 5183 "parser.y" /* yacc.c:1646  */
    {
	if (set_current_field ((yyvsp[-1]), (yyvsp[0]))) {
		YYERROR;
	}
  }
#line 11511 "parser.c" /* yacc.c:1646  */
    break;

  case 515:
#line 5189 "parser.y" /* yacc.c:1646  */
    {
	cb_validate_88_item (current_field);
  }
#line 11519 "parser.c" /* yacc.c:1646  */
    break;

  case 516:
#line 5196 "parser.y" /* yacc.c:1646  */
    {
	cb_tree x;
	int	level;

	cobc_cs_check = 0;
	level = cb_get_level ((yyvsp[-4]));
	/* Free tree associated with level number */
	cobc_parse_free ((yyvsp[-4]));
	if (level != 1) {
		cb_error (_("CONSTANT item not at 01 level"));
	} else if ((yyvsp[0])) {
		if (cb_verify(cb_constant_01, "01 CONSTANT")) {
			x = cb_build_constant ((yyvsp[-3]), (yyvsp[0]));
			CB_FIELD (x)->flag_item_78 = 1;
			CB_FIELD (x)->flag_constant = 1;
			CB_FIELD (x)->level = 1;
			CB_FIELD (x)->values = (yyvsp[0]);
			cb_needs_01 = 1;
			if ((yyvsp[-1])) {
				CB_FIELD (x)->flag_is_global = 1;
			}
			/* Ignore return value */
			(void)cb_validate_78_item (CB_FIELD (x), 0);
		}
	}
  }
#line 11550 "parser.c" /* yacc.c:1646  */
    break;

  case 517:
#line 5223 "parser.y" /* yacc.c:1646  */
    {
	if (set_current_field ((yyvsp[-1]), (yyvsp[0]))) {
		YYERROR;
	}
  }
#line 11560 "parser.c" /* yacc.c:1646  */
    break;

  case 518:
#line 5230 "parser.y" /* yacc.c:1646  */
    {
	/* Reset to last non-78 item */
	current_field = cb_validate_78_item (current_field, 0);
  }
#line 11569 "parser.c" /* yacc.c:1646  */
    break;

  case 519:
#line 5238 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 11577 "parser.c" /* yacc.c:1646  */
    break;

  case 520:
#line 5242 "parser.y" /* yacc.c:1646  */
    { 
	(yyval) = CB_LIST_INIT(cb_build_const_from ((yyvsp[0])));
  }
#line 11585 "parser.c" /* yacc.c:1646  */
    break;

  case 521:
#line 5249 "parser.y" /* yacc.c:1646  */
    {
	current_field->values = (yyvsp[0]);
  }
#line 11593 "parser.c" /* yacc.c:1646  */
    break;

  case 522:
#line 5253 "parser.y" /* yacc.c:1646  */
    {
	current_field->values = CB_LIST_INIT (cb_build_const_start (current_field, (yyvsp[0])));
  }
#line 11601 "parser.c" /* yacc.c:1646  */
    break;

  case 523:
#line 5257 "parser.y" /* yacc.c:1646  */
    {
	current_field->values = CB_LIST_INIT (cb_build_const_next (current_field));
  }
#line 11609 "parser.c" /* yacc.c:1646  */
    break;

  case 524:
#line 5263 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 11615 "parser.c" /* yacc.c:1646  */
    break;

  case 525:
#line 5264 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0])); }
#line 11621 "parser.c" /* yacc.c:1646  */
    break;

  case 526:
#line 5268 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 11627 "parser.c" /* yacc.c:1646  */
    break;

  case 527:
#line 5269 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_alphanumeric_literal ("(", 1); }
#line 11633 "parser.c" /* yacc.c:1646  */
    break;

  case 528:
#line 5270 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_alphanumeric_literal (")", 1); }
#line 11639 "parser.c" /* yacc.c:1646  */
    break;

  case 529:
#line 5271 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_alphanumeric_literal ("+", 1); }
#line 11645 "parser.c" /* yacc.c:1646  */
    break;

  case 530:
#line 5272 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_alphanumeric_literal ("-", 1); }
#line 11651 "parser.c" /* yacc.c:1646  */
    break;

  case 531:
#line 5273 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_alphanumeric_literal ("*", 1); }
#line 11657 "parser.c" /* yacc.c:1646  */
    break;

  case 532:
#line 5274 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_alphanumeric_literal ("/", 1); }
#line 11663 "parser.c" /* yacc.c:1646  */
    break;

  case 533:
#line 5275 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_alphanumeric_literal ("&", 1); }
#line 11669 "parser.c" /* yacc.c:1646  */
    break;

  case 534:
#line 5276 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_alphanumeric_literal ("|", 1); }
#line 11675 "parser.c" /* yacc.c:1646  */
    break;

  case 535:
#line 5277 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_alphanumeric_literal ("^", 1); }
#line 11681 "parser.c" /* yacc.c:1646  */
    break;

  case 536:
#line 5282 "parser.y" /* yacc.c:1646  */
    {
	/* Required to check redefines */
	(yyval) = NULL;
  }
#line 11690 "parser.c" /* yacc.c:1646  */
    break;

  case 537:
#line 5288 "parser.y" /* yacc.c:1646  */
    {
	/* Required to check redefines */
	(yyval) = cb_true;
  }
#line 11699 "parser.c" /* yacc.c:1646  */
    break;

  case 553:
#line 5317 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("REDEFINES", SYN_CLAUSE_1, &check_pic_duplicate);
	if ((yyvsp[-2]) != NULL) {
		if (cb_relaxed_syntax_checks) {
			cb_warning_x (COBC_WARN_FILLER, (yyvsp[0]), _("REDEFINES clause should follow entry-name"));
		} else {
			cb_error_x ((yyvsp[0]), _("REDEFINES clause must follow entry-name"));
		}
	}

	current_field->redefines = cb_resolve_redefines (current_field, (yyvsp[0]));
	if (current_field->redefines == NULL) {
		current_field->flag_is_verified = 1;
		current_field->flag_invalid = 1;
		YYERROR;
	}
  }
#line 11721 "parser.c" /* yacc.c:1646  */
    break;

  case 554:
#line 5341 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("EXTERNAL", SYN_CLAUSE_2, &check_pic_duplicate);
	if (current_storage != CB_STORAGE_WORKING) {
		cb_error (_("%s not allowed here"), "EXTERNAL");
	} else if (current_field->level != 1 && current_field->level != 77) {
		cb_error (_("%s only allowed at 01/77 level"), "EXTERNAL");
	} else if (!qualifier) {
		cb_error (_("%s requires a data name"), "EXTERNAL");
#if	0	/* RXWRXW - Global/External */
	} else if (current_field->flag_is_global) {
		cb_error (_("%s and %s are mutually exclusive"), "GLOBAL", "EXTERNAL");
#endif
	} else if (current_field->flag_item_based) {
		cb_error (_("%s and %s are mutually exclusive"), "BASED", "EXTERNAL");
	} else if (current_field->redefines) {
		cb_error (_("%s and %s are mutually exclusive"), "EXTERNAL", "REDEFINES");
	} else if (current_field->flag_occurs) {
		cb_error (_("%s and %s are mutually exclusive"), "EXTERNAL", "OCCURS");
	} else {
		current_field->flag_external = 1;
		current_program->flag_has_external = 1;
	}
  }
#line 11749 "parser.c" /* yacc.c:1646  */
    break;

  case 555:
#line 5368 "parser.y" /* yacc.c:1646  */
    {
	current_field->ename = cb_to_cname (current_field->name);
  }
#line 11757 "parser.c" /* yacc.c:1646  */
    break;

  case 556:
#line 5372 "parser.y" /* yacc.c:1646  */
    {
	current_field->ename = cb_to_cname ((const char *)CB_LITERAL ((yyvsp[0]))->data);
  }
#line 11765 "parser.c" /* yacc.c:1646  */
    break;

  case 559:
#line 5385 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("GLOBAL", SYN_CLAUSE_3, &check_pic_duplicate);
	if (current_field->level != 1 && current_field->level != 77) {
		cb_error (_("%s only allowed at 01/77 level"), "GLOBAL");
	} else if (!qualifier) {
		cb_error (_("%s requires a data name"), "GLOBAL");
#if	0	/* RXWRXW - Global/External */
	} else if (current_field->flag_external) {
		cb_error (_("%s and %s are mutually exclusive"), "GLOBAL", "EXTERNAL");
#endif
	} else if (current_program->prog_type == CB_FUNCTION_TYPE) {
		cb_error (_("%s is invalid in a user FUNCTION"), "GLOBAL");
	} else if (current_storage == CB_STORAGE_LOCAL) {
		cb_error (_("%s not allowed here"), "GLOBAL");
	} else {
		current_field->flag_is_global = 1;
	}
  }
#line 11788 "parser.c" /* yacc.c:1646  */
    break;

  case 560:
#line 5410 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("PICTURE", SYN_CLAUSE_4, &check_pic_duplicate);
	current_field->pic = CB_PICTURE ((yyvsp[0]));
  }
#line 11797 "parser.c" /* yacc.c:1646  */
    break;

  case 563:
#line 5426 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_BINARY);
  }
#line 11805 "parser.c" /* yacc.c:1646  */
    break;

  case 564:
#line 5430 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_BINARY);
  }
#line 11813 "parser.c" /* yacc.c:1646  */
    break;

  case 565:
#line 5434 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_FLOAT);
  }
#line 11821 "parser.c" /* yacc.c:1646  */
    break;

  case 566:
#line 5438 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_DOUBLE);
  }
#line 11829 "parser.c" /* yacc.c:1646  */
    break;

  case 567:
#line 5442 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_PACKED);
  }
#line 11837 "parser.c" /* yacc.c:1646  */
    break;

  case 568:
#line 5446 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_BINARY);
  }
#line 11845 "parser.c" /* yacc.c:1646  */
    break;

  case 569:
#line 5450 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_COMP_5);
  }
#line 11853 "parser.c" /* yacc.c:1646  */
    break;

  case 570:
#line 5454 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_COMP_6);
  }
#line 11861 "parser.c" /* yacc.c:1646  */
    break;

  case 571:
#line 5458 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_COMP_X);
  }
#line 11869 "parser.c" /* yacc.c:1646  */
    break;

  case 572:
#line 5462 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_DISPLAY);
  }
#line 11877 "parser.c" /* yacc.c:1646  */
    break;

  case 573:
#line 5466 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_INDEX);
  }
#line 11885 "parser.c" /* yacc.c:1646  */
    break;

  case 574:
#line 5470 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_PACKED);
  }
#line 11893 "parser.c" /* yacc.c:1646  */
    break;

  case 575:
#line 5474 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_POINTER);
	current_field->flag_is_pointer = 1;
  }
#line 11902 "parser.c" /* yacc.c:1646  */
    break;

  case 576:
#line 5479 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_PROGRAM_POINTER);
	current_field->flag_is_pointer = 1;
  }
#line 11911 "parser.c" /* yacc.c:1646  */
    break;

  case 577:
#line 5484 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_HNDL);
  }
#line 11919 "parser.c" /* yacc.c:1646  */
    break;

  case 578:
#line 5488 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_HNDL_WINDOW);
  }
#line 11927 "parser.c" /* yacc.c:1646  */
    break;

  case 579:
#line 5492 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_HNDL_SUBWINDOW);
  }
#line 11935 "parser.c" /* yacc.c:1646  */
    break;

  case 580:
#line 5496 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_HNDL_FONT);
	CB_PENDING ("HANDLE OF FONT");
  }
#line 11944 "parser.c" /* yacc.c:1646  */
    break;

  case 581:
#line 5501 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_HNDL_THREAD);
  }
#line 11952 "parser.c" /* yacc.c:1646  */
    break;

  case 582:
#line 5505 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_HNDL_MENU);
	CB_PENDING ("HANDLE OF MENU");
  }
#line 11961 "parser.c" /* yacc.c:1646  */
    break;

  case 583:
#line 5510 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_HNDL_VARIANT);
  }
#line 11969 "parser.c" /* yacc.c:1646  */
    break;

  case 584:
#line 5514 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_HNDL_LM);
	CB_PENDING ("HANDLE OF LAYOUT-MANAGER");
  }
#line 11978 "parser.c" /* yacc.c:1646  */
    break;

  case 585:
#line 5519 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_SIGNED_SHORT);
  }
#line 11986 "parser.c" /* yacc.c:1646  */
    break;

  case 586:
#line 5523 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_SIGNED_INT);
  }
#line 11994 "parser.c" /* yacc.c:1646  */
    break;

  case 587:
#line 5527 "parser.y" /* yacc.c:1646  */
    {
#ifdef COB_32_BIT_LONG
	check_and_set_usage (CB_USAGE_SIGNED_INT);
#else
	check_and_set_usage (CB_USAGE_SIGNED_LONG);
#endif
  }
#line 12006 "parser.c" /* yacc.c:1646  */
    break;

  case 588:
#line 5535 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_UNSIGNED_SHORT);
  }
#line 12014 "parser.c" /* yacc.c:1646  */
    break;

  case 589:
#line 5539 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_UNSIGNED_INT);
  }
#line 12022 "parser.c" /* yacc.c:1646  */
    break;

  case 590:
#line 5543 "parser.y" /* yacc.c:1646  */
    {
#ifdef COB_32_BIT_LONG
	check_and_set_usage (CB_USAGE_UNSIGNED_INT);
#else
	check_and_set_usage (CB_USAGE_UNSIGNED_LONG);
#endif
  }
#line 12034 "parser.c" /* yacc.c:1646  */
    break;

  case 591:
#line 5551 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_SIGNED_CHAR);
  }
#line 12042 "parser.c" /* yacc.c:1646  */
    break;

  case 592:
#line 5555 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_UNSIGNED_CHAR);
  }
#line 12050 "parser.c" /* yacc.c:1646  */
    break;

  case 593:
#line 5559 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_SIGNED_SHORT);
  }
#line 12058 "parser.c" /* yacc.c:1646  */
    break;

  case 594:
#line 5563 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_UNSIGNED_SHORT);
  }
#line 12066 "parser.c" /* yacc.c:1646  */
    break;

  case 595:
#line 5567 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_SIGNED_INT);
  }
#line 12074 "parser.c" /* yacc.c:1646  */
    break;

  case 596:
#line 5571 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_UNSIGNED_INT);
  }
#line 12082 "parser.c" /* yacc.c:1646  */
    break;

  case 597:
#line 5575 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_SIGNED_LONG);
  }
#line 12090 "parser.c" /* yacc.c:1646  */
    break;

  case 598:
#line 5579 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_UNSIGNED_LONG);
  }
#line 12098 "parser.c" /* yacc.c:1646  */
    break;

  case 599:
#line 5583 "parser.y" /* yacc.c:1646  */
    {
#ifdef COB_32_BIT_LONG
	check_and_set_usage (CB_USAGE_SIGNED_INT);
#else
	check_and_set_usage (CB_USAGE_SIGNED_LONG);
#endif
  }
#line 12110 "parser.c" /* yacc.c:1646  */
    break;

  case 600:
#line 5591 "parser.y" /* yacc.c:1646  */
    {
#ifdef COB_32_BIT_LONG
	check_and_set_usage (CB_USAGE_UNSIGNED_INT);
#else
	check_and_set_usage (CB_USAGE_UNSIGNED_LONG);
#endif
  }
#line 12122 "parser.c" /* yacc.c:1646  */
    break;

  case 601:
#line 5599 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_FP_BIN32);
  }
#line 12130 "parser.c" /* yacc.c:1646  */
    break;

  case 602:
#line 5603 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_FP_BIN64);
  }
#line 12138 "parser.c" /* yacc.c:1646  */
    break;

  case 603:
#line 5607 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_FP_BIN128);
  }
#line 12146 "parser.c" /* yacc.c:1646  */
    break;

  case 604:
#line 5611 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_FP_DEC64);
  }
#line 12154 "parser.c" /* yacc.c:1646  */
    break;

  case 605:
#line 5615 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_FP_DEC128);
  }
#line 12162 "parser.c" /* yacc.c:1646  */
    break;

  case 606:
#line 5619 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("USAGE", SYN_CLAUSE_5, &check_pic_duplicate);
	CB_UNFINISHED ("USAGE NATIONAL");
  }
#line 12171 "parser.c" /* yacc.c:1646  */
    break;

  case 620:
#line 5654 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SIGN", SYN_CLAUSE_6, &check_pic_duplicate);
	current_field->flag_sign_clause = 1;
	current_field->flag_sign_separate = ((yyvsp[0]) ? 1 : 0);
	current_field->flag_sign_leading  = 1;
  }
#line 12182 "parser.c" /* yacc.c:1646  */
    break;

  case 621:
#line 5661 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SIGN", SYN_CLAUSE_6, &check_pic_duplicate);
	current_field->flag_sign_clause = 1;
	current_field->flag_sign_separate = ((yyvsp[0]) ? 1 : 0);
	current_field->flag_sign_leading  = 0;
  }
#line 12193 "parser.c" /* yacc.c:1646  */
    break;

  case 622:
#line 5675 "parser.y" /* yacc.c:1646  */
    {
	/* most of the field attributes are set when parsing the phrases */;
	setup_occurs ();
	setup_occurs_min_max ((yyvsp[-4]), (yyvsp[-3]));
  }
#line 12203 "parser.c" /* yacc.c:1646  */
    break;

  case 624:
#line 5684 "parser.y" /* yacc.c:1646  */
    {
	current_field->step_count = cb_get_int ((yyvsp[0]));
  }
#line 12211 "parser.c" /* yacc.c:1646  */
    break;

  case 625:
#line 5694 "parser.y" /* yacc.c:1646  */
    {
	/* most of the field attributes are set when parsing the phrases */;
	setup_occurs ();
	setup_occurs_min_max ((yyvsp[-4]), (yyvsp[-3]));
  }
#line 12221 "parser.c" /* yacc.c:1646  */
    break;

  case 626:
#line 5701 "parser.y" /* yacc.c:1646  */
    {
	current_field->flag_unbounded = 1;
	if (current_field->parent) {
		current_field->parent->flag_unbounded = 1;
	}
	current_field->depending = (yyvsp[-1]);
	/* most of the field attributes are set when parsing the phrases */;
	setup_occurs ();
	setup_occurs_min_max ((yyvsp[-6]), cb_int0);
  }
#line 12236 "parser.c" /* yacc.c:1646  */
    break;

  case 627:
#line 5713 "parser.y" /* yacc.c:1646  */
    {
	setup_occurs ();
	current_field->occurs_min = (yyvsp[-3]) ? cb_get_int ((yyvsp[-3])) : 0;
	if ((yyvsp[-2])) {
		current_field->occurs_max = cb_get_int ((yyvsp[-2]));
		if (current_field->occurs_max <= current_field->occurs_min) {
			cb_error (_("OCCURS TO must be greater than OCCURS FROM"));
		}
	} else {
		current_field->occurs_max = 0;
	}
	CB_PENDING("OCCURS DYNAMIC");
  }
#line 12254 "parser.c" /* yacc.c:1646  */
    break;

  case 628:
#line 5729 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 12260 "parser.c" /* yacc.c:1646  */
    break;

  case 629:
#line 5730 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 12266 "parser.c" /* yacc.c:1646  */
    break;

  case 630:
#line 5734 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 12272 "parser.c" /* yacc.c:1646  */
    break;

  case 631:
#line 5735 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 12278 "parser.c" /* yacc.c:1646  */
    break;

  case 632:
#line 5739 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 12284 "parser.c" /* yacc.c:1646  */
    break;

  case 633:
#line 5740 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[-1]); }
#line 12290 "parser.c" /* yacc.c:1646  */
    break;

  case 635:
#line 5745 "parser.y" /* yacc.c:1646  */
    {
	current_field->depending = (yyvsp[0]);
  }
#line 12298 "parser.c" /* yacc.c:1646  */
    break;

  case 637:
#line 5751 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_index ((yyvsp[0]), cb_zero, 0, current_field);
	CB_FIELD_PTR ((yyval))->special_index = 1U;
  }
#line 12307 "parser.c" /* yacc.c:1646  */
    break;

  case 639:
#line 5759 "parser.y" /* yacc.c:1646  */
    {
	/* current_field->initialized = 1; */
  }
#line 12315 "parser.c" /* yacc.c:1646  */
    break;

  case 642:
#line 5768 "parser.y" /* yacc.c:1646  */
    {
	if (!cb_relaxed_syntax_checks) {
		cb_error (_("INDEXED should follow ASCENDING/DESCENDING"));
	} else {
		cb_warning (warningopt, _("INDEXED should follow ASCENDING/DESCENDING"));
	}
  }
#line 12327 "parser.c" /* yacc.c:1646  */
    break;

  case 646:
#line 5782 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		cb_tree		l;
		struct cb_key	*keys;
		int		i;
		int		nkeys;

		l = (yyvsp[0]);
		nkeys = cb_list_length ((yyvsp[0]));
		keys = cobc_parse_malloc (sizeof (struct cb_key) * nkeys);

		for (i = 0; i < nkeys; i++) {
			keys[i].dir = CB_PURPOSE_INT (l);
			keys[i].key = CB_VALUE (l);
			l = CB_CHAIN (l);
		}
		current_field->keys = keys;
		current_field->nkeys = nkeys;
	}
  }
#line 12352 "parser.c" /* yacc.c:1646  */
    break;

  case 649:
#line 5811 "parser.y" /* yacc.c:1646  */
    {
	cb_tree l;

	for (l = (yyvsp[0]); l; l = CB_CHAIN (l)) {
		CB_PURPOSE (l) = (yyvsp[-3]);
		if (qualifier && !CB_REFERENCE(CB_VALUE(l))->chain &&
		    strcasecmp (CB_NAME(CB_VALUE(l)), CB_NAME(qualifier))) {
			CB_REFERENCE(CB_VALUE(l))->chain = qualifier;
		}
	}
	keys_list = cb_list_append (keys_list, (yyvsp[0]));
	(yyval) = keys_list;
  }
#line 12370 "parser.c" /* yacc.c:1646  */
    break;

  case 650:
#line 5827 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_ASCENDING); }
#line 12376 "parser.c" /* yacc.c:1646  */
    break;

  case 651:
#line 5828 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_DESCENDING); }
#line 12382 "parser.c" /* yacc.c:1646  */
    break;

  case 654:
#line 5837 "parser.y" /* yacc.c:1646  */
    {
	current_field->index_list = (yyvsp[0]);
  }
#line 12390 "parser.c" /* yacc.c:1646  */
    break;

  case 655:
#line 5843 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 12396 "parser.c" /* yacc.c:1646  */
    break;

  case 656:
#line 5845 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0])); }
#line 12402 "parser.c" /* yacc.c:1646  */
    break;

  case 657:
#line 5850 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_index ((yyvsp[0]), cb_int1, 1U, current_field);
	CB_FIELD_PTR ((yyval))->special_index = 1U;
  }
#line 12411 "parser.c" /* yacc.c:1646  */
    break;

  case 658:
#line 5861 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("JUSTIFIED", SYN_CLAUSE_8, &check_pic_duplicate);
	current_field->flag_justified = 1;
  }
#line 12420 "parser.c" /* yacc.c:1646  */
    break;

  case 659:
#line 5872 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SYNCHRONIZED", SYN_CLAUSE_9, &check_pic_duplicate);
	current_field->flag_synchronized = 1;
  }
#line 12429 "parser.c" /* yacc.c:1646  */
    break;

  case 660:
#line 5883 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("BLANK", SYN_CLAUSE_10, &check_pic_duplicate);
	current_field->flag_blank_zero = 1;
  }
#line 12438 "parser.c" /* yacc.c:1646  */
    break;

  case 661:
#line 5894 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("BASED", SYN_CLAUSE_11, &check_pic_duplicate);
	if (current_storage != CB_STORAGE_WORKING &&
	    current_storage != CB_STORAGE_LINKAGE &&
	    current_storage != CB_STORAGE_LOCAL) {
		cb_error (_("%s not allowed here"), "BASED");
	} else if (current_field->level != 1 && current_field->level != 77) {
		cb_error (_("%s only allowed at 01/77 level"), "BASED");
	} else if (!qualifier) {
		cb_error (_("%s requires a data name"), "BASED");
	} else if (current_field->flag_external) {
		cb_error (_("%s and %s are mutually exclusive"), "BASED", "EXTERNAL");
	} else if (current_field->redefines) {
		cb_error (_("%s and %s are mutually exclusive"), "BASED", "REDEFINES");
	} else if (current_field->flag_any_length) {
		cb_error (_("%s and %s are mutually exclusive"), "BASED", "ANY LENGTH");
	} else if (current_field->flag_occurs) {
		cb_error (_("%s and %s are mutually exclusive"), "BASED", "OCCURS");
	} else {
		current_field->flag_item_based = 1;
	}
  }
#line 12465 "parser.c" /* yacc.c:1646  */
    break;

  case 662:
#line 5922 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("VALUE", SYN_CLAUSE_12, &check_pic_duplicate);
	current_field->values = (yyvsp[0]);
  }
#line 12474 "parser.c" /* yacc.c:1646  */
    break;

  case 664:
#line 5930 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 12480 "parser.c" /* yacc.c:1646  */
    break;

  case 665:
#line 5931 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0])); }
#line 12486 "parser.c" /* yacc.c:1646  */
    break;

  case 666:
#line 5935 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_BUILD_PAIR ((yyvsp[-2]), (yyvsp[0])); }
#line 12492 "parser.c" /* yacc.c:1646  */
    break;

  case 669:
#line 5942 "parser.y" /* yacc.c:1646  */
    {
	if (current_field->level != 88) {
		cb_error (_("FALSE clause only allowed for 88 level"));
	}
	current_field->false_88 = CB_LIST_INIT ((yyvsp[0]));
  }
#line 12503 "parser.c" /* yacc.c:1646  */
    break;

  case 670:
#line 5954 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ANY", SYN_CLAUSE_14, &check_pic_duplicate);
	if (current_field->flag_item_based) {
		cb_error (_("%s and %s are mutually exclusive"), "BASED", "ANY LENGTH");
	} else {
		current_field->flag_any_length = 1;
	}
  }
#line 12516 "parser.c" /* yacc.c:1646  */
    break;

  case 671:
#line 5963 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ANY", SYN_CLAUSE_14, &check_pic_duplicate);
	if (current_field->flag_item_based) {
		cb_error (_("%s and %s are mutually exclusive"), "BASED", "ANY NUMERIC");
	} else {
		current_field->flag_any_length = 1;
		current_field->flag_any_numeric = 1;
	}
  }
#line 12530 "parser.c" /* yacc.c:1646  */
    break;

  case 672:
#line 5978 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("EXTERNAL-FORM", SYN_CLAUSE_2, &check_pic_duplicate);
	CB_PENDING("EXTERNAL-FORM");
	if (current_storage != CB_STORAGE_WORKING) {
		cb_error (_("%s not allowed here"), "EXTERNAL-FORM");
	} else if (current_field->level != 1) {	/* docs say: at group level */
		cb_error (_("%s only allowed at 01 level"), "EXTERNAL-FORM");
	} else if (!qualifier) {
		cb_error (_("%s requires a data name"), "EXTERNAL-FORM");
	} else if (current_field->redefines) {
		cb_error (_("%s and %s combination not allowed"), "EXTERNAL-FORM", "REDEFINES");
	} else {
		current_field->flag_is_external_form = 1;
	}
  }
#line 12550 "parser.c" /* yacc.c:1646  */
    break;

  case 673:
#line 6001 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("IDENTIFIED BY", SYN_CLAUSE_3, &check_pic_duplicate);
	if (!current_field->flag_is_external_form) {
		CB_PENDING("EXTERNAL-FORM (IDENTIFIED BY)");
		if (current_storage != CB_STORAGE_WORKING) {
			cb_error (_("%s not allowed here"), "IDENTIFIED BY");
		} else if (!qualifier) {
			cb_error (_("%s requires a data name"), "IDENTIFIED BY");
		} else if (current_field->redefines) {
			cb_error (_("%s and %s combination not allowed"), "IDENTIFIED BY", "REDEFINES");
		}
	}
	current_field->external_form_identifier = (yyvsp[0]);
  }
#line 12569 "parser.c" /* yacc.c:1646  */
    break;

  case 675:
#line 6021 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_DATA_DIVISION, 0, 0, 0);
	header_check |= COBC_HD_LOCAL_STORAGE_SECTION;
	current_storage = CB_STORAGE_LOCAL;
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "LOCAL-STORAGE");
	}
  }
#line 12582 "parser.c" /* yacc.c:1646  */
    break;

  case 676:
#line 6030 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		current_program->local_storage = CB_FIELD ((yyvsp[0]));
	}
  }
#line 12592 "parser.c" /* yacc.c:1646  */
    break;

  case 678:
#line 6042 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_DATA_DIVISION, 0, 0, 0);
	header_check |= COBC_HD_LINKAGE_SECTION;
	current_storage = CB_STORAGE_LINKAGE;
  }
#line 12602 "parser.c" /* yacc.c:1646  */
    break;

  case 679:
#line 6048 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		current_program->linkage_storage = CB_FIELD ((yyvsp[0]));
	}
  }
#line 12612 "parser.c" /* yacc.c:1646  */
    break;

  case 681:
#line 6059 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING("REPORT SECTION");
	current_storage = CB_STORAGE_REPORT;
	cb_clear_real_field ();
  }
#line 12622 "parser.c" /* yacc.c:1646  */
    break;

  case 685:
#line 6075 "parser.y" /* yacc.c:1646  */
    {
	if (CB_INVALID_TREE ((yyvsp[0]))) {
		YYERROR;
	} else {
		current_report = CB_REPORT (cb_ref ((yyvsp[0])));
	}
	check_duplicate = 0;
  }
#line 12635 "parser.c" /* yacc.c:1646  */
    break;

  case 689:
#line 6090 "parser.y" /* yacc.c:1646  */
    {
	yyerrok;
  }
#line 12643 "parser.c" /* yacc.c:1646  */
    break;

  case 690:
#line 6097 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("GLOBAL", SYN_CLAUSE_1, &check_duplicate);
	cb_error (_("GLOBAL is not allowed with RD"));
  }
#line 12652 "parser.c" /* yacc.c:1646  */
    break;

  case 691:
#line 6102 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("CODE", SYN_CLAUSE_2, &check_duplicate);
  }
#line 12660 "parser.c" /* yacc.c:1646  */
    break;

  case 694:
#line 6113 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("CONTROL", SYN_CLAUSE_3, &check_duplicate);
  }
#line 12668 "parser.c" /* yacc.c:1646  */
    break;

  case 696:
#line 6127 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("PAGE", SYN_CLAUSE_4, &check_duplicate);
	if (!current_report->heading) {
		current_report->heading = 1;
	}
	if (!current_report->first_detail) {
		current_report->first_detail = current_report->heading;
	}
	if (!current_report->last_control) {
		if (current_report->last_detail) {
			current_report->last_control = current_report->last_detail;
		} else if (current_report->footing) {
			current_report->last_control = current_report->footing;
		} else {
			current_report->last_control = current_report->lines;
		}
	}
	if (!current_report->last_detail && !current_report->footing) {
		current_report->last_detail = current_report->lines;
		current_report->footing = current_report->lines;
	} else if (!current_report->last_detail) {
		current_report->last_detail = current_report->footing;
	} else if (!current_report->footing) {
		current_report->footing = current_report->last_detail;
	}
	if (current_report->heading > current_report->first_detail ||
	    current_report->first_detail > current_report->last_control ||
	    current_report->last_control > current_report->last_detail ||
	    current_report->last_detail > current_report->footing) {
		cb_error (_("invalid %s clause"), "PAGE");
	}
  }
#line 12705 "parser.c" /* yacc.c:1646  */
    break;

  case 697:
#line 6163 "parser.y" /* yacc.c:1646  */
    {
	current_report->lines = cb_get_int ((yyvsp[0]));
  }
#line 12713 "parser.c" /* yacc.c:1646  */
    break;

  case 698:
#line 6167 "parser.y" /* yacc.c:1646  */
    {
	current_report->lines = cb_get_int ((yyvsp[-3]));
	current_report->columns = cb_get_int ((yyvsp[-1]));
  }
#line 12722 "parser.c" /* yacc.c:1646  */
    break;

  case 699:
#line 6172 "parser.y" /* yacc.c:1646  */
    {
	current_report->lines = cb_get_int ((yyvsp[-1]));
  }
#line 12730 "parser.c" /* yacc.c:1646  */
    break;

  case 707:
#line 6192 "parser.y" /* yacc.c:1646  */
    {
	current_report->heading = cb_get_int ((yyvsp[0]));
  }
#line 12738 "parser.c" /* yacc.c:1646  */
    break;

  case 708:
#line 6199 "parser.y" /* yacc.c:1646  */
    {
	current_report->first_detail = cb_get_int ((yyvsp[0]));
  }
#line 12746 "parser.c" /* yacc.c:1646  */
    break;

  case 709:
#line 6206 "parser.y" /* yacc.c:1646  */
    {
	current_report->last_control = cb_get_int ((yyvsp[0]));
  }
#line 12754 "parser.c" /* yacc.c:1646  */
    break;

  case 710:
#line 6213 "parser.y" /* yacc.c:1646  */
    {
	current_report->last_detail = cb_get_int ((yyvsp[0]));
  }
#line 12762 "parser.c" /* yacc.c:1646  */
    break;

  case 711:
#line 6220 "parser.y" /* yacc.c:1646  */
    {
	current_report->footing = cb_get_int ((yyvsp[0]));
  }
#line 12770 "parser.c" /* yacc.c:1646  */
    break;

  case 714:
#line 6231 "parser.y" /* yacc.c:1646  */
    {
	check_pic_duplicate = 0;
  }
#line 12778 "parser.c" /* yacc.c:1646  */
    break;

  case 734:
#line 6262 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("TYPE", SYN_CLAUSE_16, &check_pic_duplicate);
  }
#line 12786 "parser.c" /* yacc.c:1646  */
    break;

  case 747:
#line 6288 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("NEXT GROUP", SYN_CLAUSE_17, &check_pic_duplicate);
  }
#line 12794 "parser.c" /* yacc.c:1646  */
    break;

  case 748:
#line 6295 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SUM", SYN_CLAUSE_19, &check_pic_duplicate);
  }
#line 12802 "parser.c" /* yacc.c:1646  */
    break;

  case 753:
#line 6311 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("PRESENT", SYN_CLAUSE_20, &check_pic_duplicate);
  }
#line 12810 "parser.c" /* yacc.c:1646  */
    break;

  case 755:
#line 6322 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("LINE", SYN_CLAUSE_21, &check_pic_duplicate);
  }
#line 12818 "parser.c" /* yacc.c:1646  */
    break;

  case 758:
#line 6334 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("COLUMN", SYN_CLAUSE_18, &check_pic_duplicate);
  }
#line 12826 "parser.c" /* yacc.c:1646  */
    break;

  case 770:
#line 6367 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SOURCE", SYN_CLAUSE_22, &check_pic_duplicate);
  }
#line 12834 "parser.c" /* yacc.c:1646  */
    break;

  case 771:
#line 6374 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("GROUP", SYN_CLAUSE_23, &check_pic_duplicate);
  }
#line 12842 "parser.c" /* yacc.c:1646  */
    break;

  case 772:
#line 6381 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("USAGE", SYN_CLAUSE_24, &check_pic_duplicate);
  }
#line 12850 "parser.c" /* yacc.c:1646  */
    break;

  case 774:
#line 6390 "parser.y" /* yacc.c:1646  */
    {
	current_storage = CB_STORAGE_SCREEN;
	current_field = NULL;
	description_field = NULL;
	cb_clear_real_field ();
  }
#line 12861 "parser.c" /* yacc.c:1646  */
    break;

  case 775:
#line 6397 "parser.y" /* yacc.c:1646  */
    {
	struct cb_field *p;

	if (description_field) {
		for (p = description_field; p; p = p->sister) {
			cb_validate_field (p);
		}
		current_program->screen_storage = description_field;
		current_program->flag_screen = 1;
	}
  }
#line 12877 "parser.c" /* yacc.c:1646  */
    break;

  case 781:
#line 6422 "parser.y" /* yacc.c:1646  */
    {
	cb_tree	x;

	x = cb_build_field_tree ((yyvsp[-1]), (yyvsp[0]), current_field, current_storage,
				 current_file, 0);
	/* Free tree associated with level number */
	cobc_parse_free ((yyvsp[-1]));
	check_pic_duplicate = 0;
	if (CB_INVALID_TREE (x)) {
		YYERROR;
	}

	current_field = CB_FIELD (x);
	if (current_field->parent) {
		current_field->screen_foreg = current_field->parent->screen_foreg;
		current_field->screen_backg = current_field->parent->screen_backg;
		current_field->screen_prompt = current_field->parent->screen_prompt;
	}
  }
#line 12901 "parser.c" /* yacc.c:1646  */
    break;

  case 782:
#line 6442 "parser.y" /* yacc.c:1646  */
    {
	cob_flags_t	flags;

	if (current_field->parent) {
		flags = current_field->parent->screen_flag;
		flags &= ~COB_SCREEN_BLANK_LINE;
		flags &= ~COB_SCREEN_BLANK_SCREEN;
		flags &= ~COB_SCREEN_ERASE_EOL;
		flags &= ~COB_SCREEN_ERASE_EOS;
		flags &= ~COB_SCREEN_LINE_PLUS;
		flags &= ~COB_SCREEN_LINE_MINUS;
		flags &= ~COB_SCREEN_COLUMN_PLUS;
		flags &= ~COB_SCREEN_COLUMN_MINUS;

		flags = zero_conflicting_flags (current_field->screen_flag,
						flags);

		current_field->screen_flag |= flags;
	}

	if (current_field->screen_flag & COB_SCREEN_INITIAL) {
		if (!(current_field->screen_flag & COB_SCREEN_INPUT)) {
			cb_error (_("INITIAL specified on non-input field"));
		}
	}
	if (!qualifier) {
		current_field->flag_filler = 1;
	}

	if (likely (current_field)) {
		if (!description_field) {
			description_field = current_field;
		}
		if (current_field->flag_occurs
		    && !has_relative_pos (current_field)) {
			cb_error (_("relative LINE/COLUMN clause required with OCCURS"));
		}
	}
  }
#line 12945 "parser.c" /* yacc.c:1646  */
    break;

  case 783:
#line 6482 "parser.y" /* yacc.c:1646  */
    {
	/* Free tree associated with level number */
	cobc_parse_free ((yyvsp[-2]));
	yyerrok;
	cb_unput_dot ();
	check_pic_duplicate = 0;
	check_duplicate = 0;
#if	1	/* RXWRXW Screen field */
	if (current_field) {
		current_field->flag_is_verified = 1;
		current_field->flag_invalid = 1;
	}
#endif
	current_field = cb_get_real_field ();
  }
#line 12965 "parser.c" /* yacc.c:1646  */
    break;

  case 786:
#line 6506 "parser.y" /* yacc.c:1646  */
    {
	set_screen_attr_with_conflict ("BLANK LINE", COB_SCREEN_BLANK_LINE,
				       "BLANK SCREEN", COB_SCREEN_BLANK_SCREEN);
  }
#line 12974 "parser.c" /* yacc.c:1646  */
    break;

  case 787:
#line 6511 "parser.y" /* yacc.c:1646  */
    {
	set_screen_attr_with_conflict ("BLANK SCREEN", COB_SCREEN_BLANK_SCREEN,
				       "BLANK LINE", COB_SCREEN_BLANK_LINE);
  }
#line 12983 "parser.c" /* yacc.c:1646  */
    break;

  case 788:
#line 6516 "parser.y" /* yacc.c:1646  */
    {
	set_screen_attr ("BELL", COB_SCREEN_BELL);
  }
#line 12991 "parser.c" /* yacc.c:1646  */
    break;

  case 789:
#line 6520 "parser.y" /* yacc.c:1646  */
    {
	set_screen_attr ("BLINK", COB_SCREEN_BLINK);
  }
#line 12999 "parser.c" /* yacc.c:1646  */
    break;

  case 790:
#line 6524 "parser.y" /* yacc.c:1646  */
    {
	set_screen_attr_with_conflict ("ERASE EOL", COB_SCREEN_ERASE_EOL,
				       "ERASE EOS", COB_SCREEN_ERASE_EOS);
  }
#line 13008 "parser.c" /* yacc.c:1646  */
    break;

  case 791:
#line 6529 "parser.y" /* yacc.c:1646  */
    {
	set_screen_attr_with_conflict ("ERASE EOS", COB_SCREEN_ERASE_EOS,
				       "ERASE EOL", COB_SCREEN_ERASE_EOL);
  }
#line 13017 "parser.c" /* yacc.c:1646  */
    break;

  case 792:
#line 6534 "parser.y" /* yacc.c:1646  */
    {
	set_screen_attr_with_conflict ("HIGHLIGHT", COB_SCREEN_HIGHLIGHT,
				       "LOWLIGHT", COB_SCREEN_LOWLIGHT);
  }
#line 13026 "parser.c" /* yacc.c:1646  */
    break;

  case 793:
#line 6539 "parser.y" /* yacc.c:1646  */
    {
	set_screen_attr_with_conflict ("LOWLIGHT", COB_SCREEN_LOWLIGHT,
				       "HIGHLIGHT", COB_SCREEN_HIGHLIGHT);
  }
#line 13035 "parser.c" /* yacc.c:1646  */
    break;

  case 794:
#line 6544 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING("STANDARD intensity");
#if 0 /* in general we could simply remove high/low, but for syntax checks
	we still need a flag */
	set_screen_attr_with_conflict ("LOWLIGHT", COB_SCREEN_LOWLIGHT,
				       "HIGHLIGHT", COB_SCREEN_HIGHLIGHT);
#endif
  }
#line 13048 "parser.c" /* yacc.c:1646  */
    break;

  case 795:
#line 6553 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING("BACKGROUND intensity");
  }
#line 13056 "parser.c" /* yacc.c:1646  */
    break;

  case 796:
#line 6557 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING("BACKGROUND intensity");
  }
#line 13064 "parser.c" /* yacc.c:1646  */
    break;

  case 797:
#line 6561 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING("BACKGROUND intensity");
  }
#line 13072 "parser.c" /* yacc.c:1646  */
    break;

  case 798:
#line 6565 "parser.y" /* yacc.c:1646  */
    {
	set_screen_attr ("REVERSE-VIDEO", COB_SCREEN_REVERSE);
  }
#line 13080 "parser.c" /* yacc.c:1646  */
    break;

  case 799:
#line 6569 "parser.y" /* yacc.c:1646  */
    {
	/* set_screen_attr ("SIZE", COB_SCREEN_SIZE); */
	CB_PENDING ("SIZE clause");
	current_field->size = cb_get_int ((yyvsp[0]));
  }
#line 13090 "parser.c" /* yacc.c:1646  */
    break;

  case 800:
#line 6575 "parser.y" /* yacc.c:1646  */
    {
	set_screen_attr ("UNDERLINE", COB_SCREEN_UNDERLINE);
  }
#line 13098 "parser.c" /* yacc.c:1646  */
    break;

  case 801:
#line 6579 "parser.y" /* yacc.c:1646  */
    {
	set_screen_attr ("OVERLINE", COB_SCREEN_OVERLINE);
	CB_PENDING ("OVERLINE");
  }
#line 13107 "parser.c" /* yacc.c:1646  */
    break;

  case 802:
#line 6584 "parser.y" /* yacc.c:1646  */
    {
	set_screen_attr ("GRID", COB_SCREEN_GRID);
	CB_PENDING ("GRID");
  }
#line 13116 "parser.c" /* yacc.c:1646  */
    break;

  case 803:
#line 6589 "parser.y" /* yacc.c:1646  */
    {
	set_screen_attr ("LEFTLINE", COB_SCREEN_LEFTLINE);
	CB_PENDING ("LEFTLINE");
  }
#line 13125 "parser.c" /* yacc.c:1646  */
    break;

  case 804:
#line 6594 "parser.y" /* yacc.c:1646  */
    {
	set_screen_attr_with_conflict ("AUTO", COB_SCREEN_AUTO,
				       "TAB", COB_SCREEN_TAB);
  }
#line 13134 "parser.c" /* yacc.c:1646  */
    break;

  case 805:
#line 6599 "parser.y" /* yacc.c:1646  */
    {
	set_screen_attr_with_conflict ("TAB", COB_SCREEN_TAB,
				       "AUTO", COB_SCREEN_AUTO);
  }
#line 13143 "parser.c" /* yacc.c:1646  */
    break;

  case 806:
#line 6604 "parser.y" /* yacc.c:1646  */
    {
	set_screen_attr_with_conflict ("SECURE", COB_SCREEN_SECURE,
				       "NO-ECHO", COB_SCREEN_NO_ECHO);
  }
#line 13152 "parser.c" /* yacc.c:1646  */
    break;

  case 807:
#line 6609 "parser.y" /* yacc.c:1646  */
    {
	if (cb_no_echo_means_secure) {
		set_screen_attr ("SECURE", COB_SCREEN_SECURE);
	} else {
		set_screen_attr_with_conflict ("NO-ECHO", COB_SCREEN_NO_ECHO,
					       "SECURE", COB_SCREEN_SECURE);
	}
  }
#line 13165 "parser.c" /* yacc.c:1646  */
    break;

  case 808:
#line 6618 "parser.y" /* yacc.c:1646  */
    {
	set_screen_attr ("REQUIRED", COB_SCREEN_REQUIRED);
  }
#line 13173 "parser.c" /* yacc.c:1646  */
    break;

  case 809:
#line 6622 "parser.y" /* yacc.c:1646  */
    {
	set_screen_attr ("FULL", COB_SCREEN_FULL);
  }
#line 13181 "parser.c" /* yacc.c:1646  */
    break;

  case 810:
#line 6626 "parser.y" /* yacc.c:1646  */
    {
	set_screen_attr ("PROMPT", COB_SCREEN_PROMPT);
	current_field->screen_prompt = (yyvsp[0]);
  }
#line 13190 "parser.c" /* yacc.c:1646  */
    break;

  case 811:
#line 6631 "parser.y" /* yacc.c:1646  */
    {
	set_screen_attr ("PROMPT", COB_SCREEN_PROMPT);
  }
#line 13198 "parser.c" /* yacc.c:1646  */
    break;

  case 812:
#line 6635 "parser.y" /* yacc.c:1646  */
    {
	set_screen_attr ("INITIAL", COB_SCREEN_INITIAL);
  }
#line 13206 "parser.c" /* yacc.c:1646  */
    break;

  case 813:
#line 6639 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("LINE", SYN_CLAUSE_16, &check_pic_duplicate);
  }
#line 13214 "parser.c" /* yacc.c:1646  */
    break;

  case 814:
#line 6643 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("COLUMN", SYN_CLAUSE_17, &check_pic_duplicate);
  }
#line 13222 "parser.c" /* yacc.c:1646  */
    break;

  case 815:
#line 6647 "parser.y" /* yacc.c:1646  */
    {
#if 0 /* TODO: implement, and add reverse to BACKGROUND/FOREGROUND-COLOR */
	check_repeated ("COLOR", SYN_CLAUSE_19, &check_pic_duplicate);
	set_screen_attr_with_conflict ("COLOR", COB_SCREEN_COLOR,
				       "BACKGROUND-COLOR", COB_SCREEN_BACKGROUND_COLOR);
	set_screen_attr_with_conflict ("COLOR", COB_SCREEN_COLOR,
				       "FOREGROUND-COLOR", FOREGROUND_COLOR);
#endif
	CB_PENDING ("COLOR clause");
  }
#line 13237 "parser.c" /* yacc.c:1646  */
    break;

  case 816:
#line 6658 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("FOREGROUND-COLOR", SYN_CLAUSE_18, &check_pic_duplicate);
	current_field->screen_foreg = (yyvsp[0]);
  }
#line 13246 "parser.c" /* yacc.c:1646  */
    break;

  case 817:
#line 6663 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("BACKGROUND-COLOR", SYN_CLAUSE_19, &check_pic_duplicate);
	current_field->screen_backg = (yyvsp[0]);
  }
#line 13255 "parser.c" /* yacc.c:1646  */
    break;

  case 826:
#line 6676 "parser.y" /* yacc.c:1646  */
    {
	check_not_88_level ((yyvsp[0]));

	check_repeated ("USING", SYN_CLAUSE_20, &check_pic_duplicate);
	current_field->screen_from = (yyvsp[0]);
	current_field->screen_to = (yyvsp[0]);
	current_field->screen_flag |= COB_SCREEN_INPUT;
  }
#line 13268 "parser.c" /* yacc.c:1646  */
    break;

  case 827:
#line 6685 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("FROM", SYN_CLAUSE_21, &check_pic_duplicate);
	current_field->screen_from = (yyvsp[0]);
  }
#line 13277 "parser.c" /* yacc.c:1646  */
    break;

  case 828:
#line 6690 "parser.y" /* yacc.c:1646  */
    {
	check_not_88_level ((yyvsp[0]));

	check_repeated ("TO", SYN_CLAUSE_22, &check_pic_duplicate);
	current_field->screen_to = (yyvsp[0]);
	current_field->screen_flag |= COB_SCREEN_INPUT;
  }
#line 13289 "parser.c" /* yacc.c:1646  */
    break;

  case 837:
#line 6721 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		current_field->screen_line = (yyvsp[0]);
	}
  }
#line 13299 "parser.c" /* yacc.c:1646  */
    break;

  case 838:
#line 6730 "parser.y" /* yacc.c:1646  */
    {
	/* Nothing */
  }
#line 13307 "parser.c" /* yacc.c:1646  */
    break;

  case 839:
#line 6734 "parser.y" /* yacc.c:1646  */
    {
	current_field->screen_flag |= COB_SCREEN_LINE_PLUS;
  }
#line 13315 "parser.c" /* yacc.c:1646  */
    break;

  case 840:
#line 6738 "parser.y" /* yacc.c:1646  */
    {
	current_field->screen_flag |= COB_SCREEN_LINE_MINUS;
  }
#line 13323 "parser.c" /* yacc.c:1646  */
    break;

  case 841:
#line 6745 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		current_field->screen_column = (yyvsp[0]);
	}
  }
#line 13333 "parser.c" /* yacc.c:1646  */
    break;

  case 842:
#line 6754 "parser.y" /* yacc.c:1646  */
    {
	/* Nothing */
  }
#line 13341 "parser.c" /* yacc.c:1646  */
    break;

  case 843:
#line 6758 "parser.y" /* yacc.c:1646  */
    {
	current_field->screen_flag |= COB_SCREEN_COLUMN_PLUS;
  }
#line 13349 "parser.c" /* yacc.c:1646  */
    break;

  case 844:
#line 6762 "parser.y" /* yacc.c:1646  */
    {
	current_field->screen_flag |= COB_SCREEN_COLUMN_MINUS;
  }
#line 13357 "parser.c" /* yacc.c:1646  */
    break;

  case 845:
#line 6769 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING (_("OCCURS screen items"));
	check_repeated ("OCCURS", SYN_CLAUSE_23, &check_pic_duplicate);
	current_field->occurs_max = cb_get_int ((yyvsp[-1]));
	current_field->occurs_min = current_field->occurs_max;
	current_field->indexes++;
	current_field->flag_occurs = 1;
  }
#line 13370 "parser.c" /* yacc.c:1646  */
    break;

  case 846:
#line 6781 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING (_("GLOBAL screen items"));
  }
#line 13378 "parser.c" /* yacc.c:1646  */
    break;

  case 847:
#line 6790 "parser.y" /* yacc.c:1646  */
    {
	current_section = NULL;
	current_paragraph = NULL;
	check_pic_duplicate = 0;
	check_duplicate = 0;
	if (!current_program->entry_convention) {
		current_program->entry_convention = cb_int (CB_CONV_COBOL);
	}
  }
#line 13392 "parser.c" /* yacc.c:1646  */
    break;

  case 848:
#line 6800 "parser.y" /* yacc.c:1646  */
    {
	current_section = NULL;
	current_paragraph = NULL;
	check_pic_duplicate = 0;
	check_duplicate = 0;
	cobc_in_procedure = 1U;
	cb_set_system_names ();
	if ((yyvsp[-3])) {
		if (current_program->entry_convention) {
			cb_warning (COBC_WARN_FILLER, _("overriding convention specified in ENTRY-CONVENTION"));
		}
		current_program->entry_convention = (yyvsp[-3]);
	} else if (!current_program->entry_convention) {
		current_program->entry_convention = cb_int (CB_CONV_COBOL);
	}
	header_check |= COBC_HD_PROCEDURE_DIVISION;
  }
#line 13414 "parser.c" /* yacc.c:1646  */
    break;

  case 849:
#line 6818 "parser.y" /* yacc.c:1646  */
    {
	if (current_program->flag_main && !current_program->flag_chained && (yyvsp[-4])) {
		cb_error (_("executable program requested but PROCEDURE/ENTRY has USING clause"));
	}
	/* Main entry point */
	emit_entry (current_program->program_id, 0, (yyvsp[-4]), NULL);
	current_program->num_proc_params = cb_list_length ((yyvsp[-4]));
	if (current_program->source_name) {
		emit_entry (current_program->source_name, 1, (yyvsp[-4]), NULL);
	}
  }
#line 13430 "parser.c" /* yacc.c:1646  */
    break;

  case 850:
#line 6830 "parser.y" /* yacc.c:1646  */
    {
	if (current_paragraph) {
		if (current_paragraph->exit_label) {
			emit_statement (current_paragraph->exit_label);
		}
		emit_statement (cb_build_perform_exit (current_paragraph));
	}
	if (current_section) {
		if (current_section->exit_label) {
			emit_statement (current_section->exit_label);
		}
		emit_statement (cb_build_perform_exit (current_section));
	}
  }
#line 13449 "parser.c" /* yacc.c:1646  */
    break;

  case 851:
#line 6845 "parser.y" /* yacc.c:1646  */
    {
	cb_tree label;

	/* No PROCEDURE DIVISION header here */
	/* Only a statement is allowed as first element */
	/* Thereafter, sections/paragraphs may be used */
	check_pic_duplicate = 0;
	check_duplicate = 0;
	if (!current_program->entry_convention) {
		current_program->entry_convention = cb_int (CB_CONV_COBOL);
	}
	cobc_in_procedure = 1U;
	label = cb_build_reference ("MAIN SECTION");
	current_section = CB_LABEL (cb_build_label (label, NULL));
	current_section->flag_section = 1;
	current_section->flag_dummy_section = 1;
	current_section->flag_skip_label = !!skip_statements;
	current_section->flag_declaratives = !!in_declaratives;
	current_section->xref.skip = 1;
	CB_TREE (current_section)->source_file = cb_source_file;
	CB_TREE (current_section)->source_line = cb_source_line;
	emit_statement (CB_TREE (current_section));
	label = cb_build_reference ("MAIN PARAGRAPH");
	current_paragraph = CB_LABEL (cb_build_label (label, NULL));
	current_paragraph->flag_declaratives = !!in_declaratives;
	current_paragraph->flag_skip_label = !!skip_statements;
	current_paragraph->flag_dummy_paragraph = 1;
	current_paragraph->xref.skip = 1;
	CB_TREE (current_paragraph)->source_file = cb_source_file;
	CB_TREE (current_paragraph)->source_line = cb_source_line;
	emit_statement (CB_TREE (current_paragraph));
	cb_set_system_names ();
  }
#line 13487 "parser.c" /* yacc.c:1646  */
    break;

  case 853:
#line 6883 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 13495 "parser.c" /* yacc.c:1646  */
    break;

  case 854:
#line 6887 "parser.y" /* yacc.c:1646  */
    {
	call_mode = CB_CALL_BY_REFERENCE;
	size_mode = CB_SIZE_4;
  }
#line 13504 "parser.c" /* yacc.c:1646  */
    break;

  case 855:
#line 6892 "parser.y" /* yacc.c:1646  */
    {
	if (cb_list_length ((yyvsp[0])) > MAX_CALL_FIELD_PARAMS) {
		cb_error (_("number of parameters exceeds maximum %d"),
			  MAX_CALL_FIELD_PARAMS);
	}
	(yyval) = (yyvsp[0]);
  }
#line 13516 "parser.c" /* yacc.c:1646  */
    break;

  case 856:
#line 6900 "parser.y" /* yacc.c:1646  */
    {
	call_mode = CB_CALL_BY_REFERENCE;
	if (current_program->prog_type == CB_FUNCTION_TYPE) {
		cb_error (_("CHAINING invalid in user FUNCTION"));
	} else {
		current_program->flag_chained = 1;
	}
  }
#line 13529 "parser.c" /* yacc.c:1646  */
    break;

  case 857:
#line 6909 "parser.y" /* yacc.c:1646  */
    {
	if (cb_list_length ((yyvsp[0])) > MAX_CALL_FIELD_PARAMS) {
		cb_error (_("number of parameters exceeds maximum %d"),
			  MAX_CALL_FIELD_PARAMS);
	}
	(yyval) = (yyvsp[0]);
  }
#line 13541 "parser.c" /* yacc.c:1646  */
    break;

  case 858:
#line 6919 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 13547 "parser.c" /* yacc.c:1646  */
    break;

  case 859:
#line 6921 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_append ((yyvsp[-1]), (yyvsp[0])); }
#line 13553 "parser.c" /* yacc.c:1646  */
    break;

  case 860:
#line 6926 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		x;
	struct cb_field	*f;

	x = cb_build_identifier ((yyvsp[0]), 0);
	if ((yyvsp[-1]) == cb_int1 && CB_VALID_TREE (x) && cb_ref (x) != cb_error_node) {
		f = CB_FIELD (cb_ref (x));
		f->flag_is_pdiv_opt = 1;
	}

	if (call_mode == CB_CALL_BY_VALUE
	    && CB_REFERENCE_P ((yyvsp[0]))
	    && CB_FIELD (cb_ref ((yyvsp[0])))->flag_any_length) {
		cb_error_x ((yyvsp[0]), _("ANY LENGTH items may only be BY REFERENCE formal parameters"));
	}

	(yyval) = CB_BUILD_PAIR (cb_int (call_mode), x);
	CB_SIZES ((yyval)) = size_mode;
  }
#line 13577 "parser.c" /* yacc.c:1646  */
    break;

  case 862:
#line 6950 "parser.y" /* yacc.c:1646  */
    {
	call_mode = CB_CALL_BY_REFERENCE;
  }
#line 13585 "parser.c" /* yacc.c:1646  */
    break;

  case 863:
#line 6954 "parser.y" /* yacc.c:1646  */
    {
	if (current_program->flag_chained) {
		cb_error (_("%s not allowed in CHAINED programs"), "BY VALUE");
	} else {
		CB_UNFINISHED (_("parameters passed BY VALUE"));
		call_mode = CB_CALL_BY_VALUE;
	}
  }
#line 13598 "parser.c" /* yacc.c:1646  */
    break;

  case 865:
#line 6967 "parser.y" /* yacc.c:1646  */
    {
	if (call_mode != CB_CALL_BY_VALUE) {
		cb_error (_("SIZE only allowed for BY VALUE items"));
	} else {
		size_mode = CB_SIZE_AUTO;
	}
  }
#line 13610 "parser.c" /* yacc.c:1646  */
    break;

  case 866:
#line 6975 "parser.y" /* yacc.c:1646  */
    {
	if (call_mode != CB_CALL_BY_VALUE) {
		cb_error (_("SIZE only allowed for BY VALUE items"));
	} else {
		size_mode = CB_SIZE_4;
	}
  }
#line 13622 "parser.c" /* yacc.c:1646  */
    break;

  case 867:
#line 6983 "parser.y" /* yacc.c:1646  */
    {
	if (call_mode != CB_CALL_BY_VALUE) {
		cb_error (_("SIZE only allowed for BY VALUE items"));
	} else {
		size_mode = CB_SIZE_AUTO | CB_SIZE_UNSIGNED;
	}
  }
#line 13634 "parser.c" /* yacc.c:1646  */
    break;

  case 868:
#line 6991 "parser.y" /* yacc.c:1646  */
    {
	if (size_mode) {
		size_mode |= CB_SIZE_UNSIGNED;
	}
  }
#line 13644 "parser.c" /* yacc.c:1646  */
    break;

  case 870:
#line 7001 "parser.y" /* yacc.c:1646  */
    {
	unsigned char *s = CB_LITERAL ((yyvsp[0]))->data;
	size_mode = 0;

	if (call_mode != CB_CALL_BY_VALUE) {
		cb_error (_("SIZE only allowed for BY VALUE items"));
	} else if (CB_LITERAL ((yyvsp[0]))->size != 1) {
		cb_error_x ((yyvsp[0]), _("invalid value for SIZE"));
	} else {
		size_mode = 0;
		switch (*s) {
		case '1':
			size_mode = CB_SIZE_1;
			break;
		case '2':
			size_mode = CB_SIZE_2;
			break;
		case '4':
			size_mode = CB_SIZE_4;
			break;
		case '8':
			size_mode = CB_SIZE_8;
			break;
		default:
			cb_error_x ((yyvsp[0]), _("invalid value for SIZE"));
			break;
		}
	}
  }
#line 13678 "parser.c" /* yacc.c:1646  */
    break;

  case 871:
#line 7034 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int0;
  }
#line 13686 "parser.c" /* yacc.c:1646  */
    break;

  case 872:
#line 7038 "parser.y" /* yacc.c:1646  */
    {
	if (call_mode != CB_CALL_BY_REFERENCE) {
		cb_error (_("OPTIONAL only allowed for BY REFERENCE items"));
		(yyval) = cb_int0;
	} else {
		(yyval) = cb_int1;
	}
  }
#line 13699 "parser.c" /* yacc.c:1646  */
    break;

  case 873:
#line 7050 "parser.y" /* yacc.c:1646  */
    {
	if (current_program->prog_type == CB_FUNCTION_TYPE) {
		cb_error (_("RETURNING clause is required for a FUNCTION"));
	}
  }
#line 13709 "parser.c" /* yacc.c:1646  */
    break;

  case 874:
#line 7056 "parser.y" /* yacc.c:1646  */
    {
	if (current_program->flag_main) {
		cb_error (_("RETURNING clause cannot be OMITTED for main program"));
	}
	if (current_program->prog_type == CB_FUNCTION_TYPE) {
		cb_error (_("RETURNING clause cannot be OMITTED for a FUNCTION"));
	}
	current_program->flag_void = 1;
  }
#line 13723 "parser.c" /* yacc.c:1646  */
    break;

  case 875:
#line 7066 "parser.y" /* yacc.c:1646  */
    {
	struct cb_field	*f;

	if (cb_ref ((yyvsp[0])) != cb_error_node) {
		f = CB_FIELD_PTR ((yyvsp[0]));
		/* standard rule: returning item is allocated in the
		   activating runtime element */
		if (f->storage != CB_STORAGE_LINKAGE) {
			cb_error (_("RETURNING item is not defined in LINKAGE SECTION"));
		} else if (f->level != 1 && f->level != 77) {
			cb_error (_("RETURNING item must have level 01"));
		} else if (f->flag_occurs) {
			cb_error (_("RETURNING item should not have OCCURS"));
		} else {
			if (current_program->prog_type == CB_FUNCTION_TYPE) {
				if (f->flag_any_length) {
					cb_error (_("function RETURNING item may not be ANY LENGTH"));
				}

				f->flag_is_returning = 1;
			}
			current_program->returning = (yyvsp[0]);
		}
	}
  }
#line 13753 "parser.c" /* yacc.c:1646  */
    break;

  case 877:
#line 7095 "parser.y" /* yacc.c:1646  */
    {
	in_declaratives = 1;
	emit_statement (cb_build_comment ("DECLARATIVES"));
  }
#line 13762 "parser.c" /* yacc.c:1646  */
    break;

  case 878:
#line 7101 "parser.y" /* yacc.c:1646  */
    {
	if (needs_field_debug) {
		start_debug = 1;
	}
	in_declaratives = 0;
	in_debugging = 0;
	if (current_paragraph) {
		if (current_paragraph->exit_label) {
			emit_statement (current_paragraph->exit_label);
		}
		emit_statement (cb_build_perform_exit (current_paragraph));
		current_paragraph = NULL;
	}
	if (current_section) {
		if (current_section->exit_label) {
			emit_statement (current_section->exit_label);
		}
		current_section->flag_fatal_check = 1;
		emit_statement (cb_build_perform_exit (current_section));
		current_section = NULL;
	}
	skip_statements = 0;
	emit_statement (cb_build_comment ("END DECLARATIVES"));
	check_unreached = 0;
  }
#line 13792 "parser.c" /* yacc.c:1646  */
    break;

  case 883:
#line 7139 "parser.y" /* yacc.c:1646  */
    {
	if (next_label_list) {
		cb_tree	plabel;
		char	name[32];

		snprintf (name, sizeof(name), "L$%d", next_label_id);
		plabel = cb_build_label (cb_build_reference (name), NULL);
		CB_LABEL (plabel)->flag_next_sentence = 1;
		emit_statement (plabel);
		current_program->label_list =
			cb_list_append (current_program->label_list, next_label_list);
		next_label_list = NULL;
		next_label_id++;
	}
	/* check_unreached = 0; */
	cb_end_statement();
  }
#line 13814 "parser.c" /* yacc.c:1646  */
    break;

  case 885:
#line 7158 "parser.y" /* yacc.c:1646  */
    {
	/* check_unreached = 0; */
	cb_end_statement();
  }
#line 13823 "parser.c" /* yacc.c:1646  */
    break;

  case 886:
#line 7169 "parser.y" /* yacc.c:1646  */
    {
	non_const_word = 0;
	check_unreached = 0;
	if (cb_build_section_name ((yyvsp[-1]), 0) == cb_error_node) {
		YYERROR;
	}

	/* Exit the last paragraph/section */
	if (current_paragraph) {
		if (current_paragraph->exit_label) {
			emit_statement (current_paragraph->exit_label);
		}
		emit_statement (cb_build_perform_exit (current_paragraph));
	}
	if (current_section) {
		if (current_section->exit_label) {
			emit_statement (current_section->exit_label);
		}
		emit_statement (cb_build_perform_exit (current_section));
	}
	if (current_program->flag_debugging && !in_debugging) {
		if (current_paragraph || current_section) {
			emit_statement (cb_build_comment (
					"DEBUGGING - Fall through"));
			emit_statement (cb_build_debug (cb_debug_contents,
					"FALL THROUGH", NULL));
		}
	}

	/* Begin a new section */
	current_section = CB_LABEL (cb_build_label ((yyvsp[-1]), NULL));
	current_section->flag_section = 1;
	/* Careful here, one negation */
	current_section->flag_real_label = !in_debugging;
	current_section->flag_declaratives = !!in_declaratives;
	current_section->flag_skip_label = !!skip_statements;
	CB_TREE (current_section)->source_file = cb_source_file;
	CB_TREE (current_section)->source_line = cb_source_line;
	current_paragraph = NULL;
  }
#line 13868 "parser.c" /* yacc.c:1646  */
    break;

  case 887:
#line 7211 "parser.y" /* yacc.c:1646  */
    {
	emit_statement (CB_TREE (current_section));
  }
#line 13876 "parser.c" /* yacc.c:1646  */
    break;

  case 890:
#line 7222 "parser.y" /* yacc.c:1646  */
    {
	cb_tree label;

	non_const_word = 0;
	check_unreached = 0;
	if (cb_build_section_name ((yyvsp[-1]), 1) == cb_error_node) {
		YYERROR;
	}

	/* Exit the last paragraph */
	if (current_paragraph) {
		if (current_paragraph->exit_label) {
			emit_statement (current_paragraph->exit_label);
		}
		emit_statement (cb_build_perform_exit (current_paragraph));
		if (current_program->flag_debugging && !in_debugging) {
			emit_statement (cb_build_comment (
					"DEBUGGING - Fall through"));
			emit_statement (cb_build_debug (cb_debug_contents,
					"FALL THROUGH", NULL));
		}
	}

	/* Begin a new paragraph */
	if (!current_section) {
		label = cb_build_reference ("MAIN SECTION");
		current_section = CB_LABEL (cb_build_label (label, NULL));
		current_section->flag_section = 1;
		current_section->flag_dummy_section = 1;
		current_section->flag_declaratives = !!in_declaratives;
		current_section->flag_skip_label = !!skip_statements;
		current_section->xref.skip = 1;
		CB_TREE (current_section)->source_file = cb_source_file;
		CB_TREE (current_section)->source_line = cb_source_line;
		emit_statement (CB_TREE (current_section));
	}
	current_paragraph = CB_LABEL (cb_build_label ((yyvsp[-1]), current_section));
	current_paragraph->flag_declaratives = !!in_declaratives;
	current_paragraph->flag_skip_label = !!skip_statements;
	current_paragraph->flag_real_label = !in_debugging;
	current_paragraph->segment = current_section->segment;
	CB_TREE (current_paragraph)->source_file = cb_source_file;
	CB_TREE (current_paragraph)->source_line = cb_source_line;
	emit_statement (CB_TREE (current_paragraph));
  }
#line 13926 "parser.c" /* yacc.c:1646  */
    break;

  case 891:
#line 7271 "parser.y" /* yacc.c:1646  */
    {
	non_const_word = 0;
	check_unreached = 0;
	if (cb_build_section_name ((yyvsp[0]), 0) != cb_error_node) {
		if (is_reserved_word (CB_NAME ((yyvsp[0])))) {
			cb_error_x ((yyvsp[0]), _("'%s' is not a statement"), CB_NAME ((yyvsp[0])));
		} else if (is_default_reserved_word (CB_NAME ((yyvsp[0])))) {
			cb_error_x ((yyvsp[0]), _("unknown statement '%s'; it may exist in another dialect"),
				    CB_NAME ((yyvsp[0])));
		} else {
			cb_error_x ((yyvsp[0]), _("unknown statement '%s'"), CB_NAME ((yyvsp[0])));
		}
	}
	YYERROR;
  }
#line 13946 "parser.c" /* yacc.c:1646  */
    break;

  case 892:
#line 7290 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 13954 "parser.c" /* yacc.c:1646  */
    break;

  case 893:
#line 7294 "parser.y" /* yacc.c:1646  */
    {
	int segnum = cb_get_int ((yyvsp[0]));
	
	(yyval) = NULL;
	if (cb_verify (cb_section_segments, "SECTION segment")) {
		if (segnum > 99) {
			cb_error (_("SECTION segment-number must be less than or equal to 99"));
		} else {
			if (in_declaratives && segnum > 49) {
				cb_error (_("SECTION segment-number in DECLARATIVES must be less than 50"));
			}
			if (!in_declaratives) {
				current_program->flag_segments = 1;
				current_section->segment = segnum;
			} else {
				/* Simon: old version did not allow segments in declaratives at all
					ToDo: check codegen for possible missing parts */
				CB_PENDING (_("SECTION segment within DECLARATIVES"));
			}
		}
	}
  }
#line 13981 "parser.c" /* yacc.c:1646  */
    break;

  case 894:
#line 7323 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = current_program->exec_list;
	current_program->exec_list = NULL;
	check_unreached = 0;
  }
#line 13991 "parser.c" /* yacc.c:1646  */
    break;

  case 895:
#line 7328 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_TREE (current_statement);
	current_statement = NULL;
  }
#line 14000 "parser.c" /* yacc.c:1646  */
    break;

  case 896:
#line 7333 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_reverse (current_program->exec_list);
	current_program->exec_list = (yyvsp[-2]);
	current_statement = CB_STATEMENT ((yyvsp[-1]));
  }
#line 14010 "parser.c" /* yacc.c:1646  */
    break;

  case 897:
#line 7341 "parser.y" /* yacc.c:1646  */
    {
	cb_tree label;

	if (!current_section) {
		label = cb_build_reference ("MAIN SECTION");
		current_section = CB_LABEL (cb_build_label (label, NULL));
		current_section->flag_section = 1;
		current_section->flag_dummy_section = 1;
		current_section->flag_skip_label = !!skip_statements;
		current_section->flag_declaratives = !!in_declaratives;
		current_section->xref.skip = 1;
		CB_TREE (current_section)->source_file = cb_source_file;
		CB_TREE (current_section)->source_line = cb_source_line;
		emit_statement (CB_TREE (current_section));
	}
	if (!current_paragraph) {
		label = cb_build_reference ("MAIN PARAGRAPH");
		current_paragraph = CB_LABEL (cb_build_label (label, NULL));
		current_paragraph->flag_declaratives = !!in_declaratives;
		current_paragraph->flag_skip_label = !!skip_statements;
		current_paragraph->flag_dummy_paragraph = 1;
		current_paragraph->xref.skip = 1;
		CB_TREE (current_paragraph)->source_file = cb_source_file;
		CB_TREE (current_paragraph)->source_line = cb_source_line;
		emit_statement (CB_TREE (current_paragraph));
	}
	if (check_headers_present (COBC_HD_PROCEDURE_DIVISION, 0, 0, 0) == 1) {
		if (current_program->prog_type == CB_PROGRAM_TYPE) {
			emit_entry (current_program->program_id, 0, NULL, NULL);
		}
	}
  }
#line 14047 "parser.c" /* yacc.c:1646  */
    break;

  case 898:
#line 7374 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
  }
#line 14055 "parser.c" /* yacc.c:1646  */
    break;

  case 899:
#line 7378 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
  }
#line 14063 "parser.c" /* yacc.c:1646  */
    break;

  case 955:
#line 7441 "parser.y" /* yacc.c:1646  */
    {
	if (cb_verify (cb_next_sentence_phrase, "NEXT SENTENCE")) {
		cb_tree label;
		char	name[32];

		begin_statement ("NEXT SENTENCE", 0);
		sprintf (name, "L$%d", next_label_id);
		label = cb_build_reference (name);
		next_label_list = cb_list_add (next_label_list, label);
		emit_statement (cb_build_goto (label, NULL));
	}
	check_unreached = 0;
  }
#line 14081 "parser.c" /* yacc.c:1646  */
    break;

  case 956:
#line 7455 "parser.y" /* yacc.c:1646  */
    {
	yyerrok;
	cobc_cs_check = 0;
  }
#line 14090 "parser.c" /* yacc.c:1646  */
    break;

  case 957:
#line 7466 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("ACCEPT", TERM_ACCEPT);
	cobc_cs_check = CB_CS_ACCEPT;
  }
#line 14099 "parser.c" /* yacc.c:1646  */
    break;

  case 959:
#line 7476 "parser.y" /* yacc.c:1646  */
    {
	  check_duplicate = 0;
	  check_line_col_duplicate = 0;
	  line_column = NULL;
  }
#line 14109 "parser.c" /* yacc.c:1646  */
    break;

  case 960:
#line 7482 "parser.y" /* yacc.c:1646  */
    {
	/* Check for invalid use of screen clauses */
	if (current_statement->attr_ptr
	    || (!is_screen_field ((yyvsp[-3])) && line_column)) {
		cb_verify_x ((yyvsp[-3]), cb_accept_display_extensions,
			     _("non-standard ACCEPT"));
	}

	if (cb_accept_update && !has_dispattr (COB_SCREEN_NO_UPDATE)) {
		set_dispattr (COB_SCREEN_UPDATE);
	}
	if (cb_accept_auto && !has_dispattr (COB_SCREEN_TAB)) {
		set_dispattr (COB_SCREEN_AUTO);
	}
	if ((yyvsp[-3]) == cb_null && current_statement->attr_ptr) {
		if (current_statement->attr_ptr->prompt) {
			emit_conflicting_clause_message ("ACCEPT OMITTED",
				_("PROMPT clause"));
		}
		if (current_statement->attr_ptr->size_is) {
			emit_conflicting_clause_message ("ACCEPT OMITTED",
				_("SIZE IS clause"));
		}
	}
	cobc_cs_check = 0;
	cb_emit_accept ((yyvsp[-3]), line_column, current_statement->attr_ptr);
  }
#line 14141 "parser.c" /* yacc.c:1646  */
    break;

  case 961:
#line 7510 "parser.y" /* yacc.c:1646  */
    {
	  check_duplicate = 0;
	  check_line_col_duplicate = 0;
	  line_column = NULL;
  }
#line 14151 "parser.c" /* yacc.c:1646  */
    break;

  case 962:
#line 7516 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
	CB_PENDING ("ACCEPT FROM SCREEN");
  }
#line 14160 "parser.c" /* yacc.c:1646  */
    break;

  case 963:
#line 7521 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_line_or_col ((yyvsp[-2]), 0);
  }
#line 14168 "parser.c" /* yacc.c:1646  */
    break;

  case 964:
#line 7525 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_line_or_col ((yyvsp[-2]), 1);
  }
#line 14176 "parser.c" /* yacc.c:1646  */
    break;

  case 965:
#line 7529 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
	cb_emit_accept_date_yyyymmdd ((yyvsp[-3]));
  }
#line 14185 "parser.c" /* yacc.c:1646  */
    break;

  case 966:
#line 7534 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
	cb_emit_accept_date ((yyvsp[-2]));
  }
#line 14194 "parser.c" /* yacc.c:1646  */
    break;

  case 967:
#line 7539 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
	cb_emit_accept_day_yyyyddd ((yyvsp[-3]));
  }
#line 14203 "parser.c" /* yacc.c:1646  */
    break;

  case 968:
#line 7544 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
	cb_emit_accept_day ((yyvsp[-2]));
  }
#line 14212 "parser.c" /* yacc.c:1646  */
    break;

  case 969:
#line 7549 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_day_of_week ((yyvsp[-2]));
  }
#line 14220 "parser.c" /* yacc.c:1646  */
    break;

  case 970:
#line 7553 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_escape_key ((yyvsp[-3]));
  }
#line 14228 "parser.c" /* yacc.c:1646  */
    break;

  case 971:
#line 7557 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_exception_status ((yyvsp[-3]));
  }
#line 14236 "parser.c" /* yacc.c:1646  */
    break;

  case 972:
#line 7561 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_time ((yyvsp[-2]));
  }
#line 14244 "parser.c" /* yacc.c:1646  */
    break;

  case 973:
#line 7565 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
	cb_emit_accept_user_name ((yyvsp[-3]));
  }
#line 14253 "parser.c" /* yacc.c:1646  */
    break;

  case 974:
#line 7570 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_command_line ((yyvsp[-2]));
  }
#line 14261 "parser.c" /* yacc.c:1646  */
    break;

  case 975:
#line 7574 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_environment ((yyvsp[-3]));
  }
#line 14269 "parser.c" /* yacc.c:1646  */
    break;

  case 976:
#line 7578 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_get_environment ((yyvsp[-1]), (yyvsp[-4]));
  }
#line 14277 "parser.c" /* yacc.c:1646  */
    break;

  case 977:
#line 7582 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_arg_number ((yyvsp[-2]));
  }
#line 14285 "parser.c" /* yacc.c:1646  */
    break;

  case 978:
#line 7586 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_arg_value ((yyvsp[-3]));
  }
#line 14293 "parser.c" /* yacc.c:1646  */
    break;

  case 979:
#line 7590 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_mnemonic ((yyvsp[-2]), (yyvsp[0]));
  }
#line 14301 "parser.c" /* yacc.c:1646  */
    break;

  case 980:
#line 7594 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_name ((yyvsp[-2]), (yyvsp[0]));
  }
#line 14309 "parser.c" /* yacc.c:1646  */
    break;

  case 981:
#line 7598 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("ACCEPT MESSAGE COUNT");
  }
#line 14317 "parser.c" /* yacc.c:1646  */
    break;

  case 983:
#line 7606 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_null;
  }
#line 14325 "parser.c" /* yacc.c:1646  */
    break;

  case 989:
#line 7624 "parser.y" /* yacc.c:1646  */
    {
	  check_repeated ("FROM CRT", SYN_CLAUSE_2, &check_duplicate);
  }
#line 14333 "parser.c" /* yacc.c:1646  */
    break;

  case 990:
#line 7628 "parser.y" /* yacc.c:1646  */
    {
	  check_repeated ("MODE IS BLOCK", SYN_CLAUSE_3, &check_duplicate);
  }
#line 14341 "parser.c" /* yacc.c:1646  */
    break;

  case 992:
#line 7633 "parser.y" /* yacc.c:1646  */
    {
	check_repeated (_("TIME-OUT or BEFORE TIME clauses"), SYN_CLAUSE_4,
			&check_duplicate);
	set_attribs (NULL, NULL, NULL, (yyvsp[0]), NULL, NULL, 0);
  }
#line 14351 "parser.c" /* yacc.c:1646  */
    break;

  case 999:
#line 7658 "parser.y" /* yacc.c:1646  */
    {
	set_attr_with_conflict ("LINE", SYN_CLAUSE_1,
				_("AT screen-location"), SYN_CLAUSE_3, 1,
				&check_line_col_duplicate);

	if ((CB_LITERAL_P ((yyvsp[0])) && cb_get_int ((yyvsp[0])) == 0) || (yyvsp[0]) == cb_zero) {
		cb_verify (cb_accept_display_extensions, "LINE 0");
	}

	if (!line_column) {
		line_column = CB_BUILD_PAIR ((yyvsp[0]), cb_int0);
	} else {
		CB_PAIR_X (line_column) = (yyvsp[0]);
	}
  }
#line 14371 "parser.c" /* yacc.c:1646  */
    break;

  case 1000:
#line 7674 "parser.y" /* yacc.c:1646  */
    {
	set_attr_with_conflict ("COLUMN", SYN_CLAUSE_2,
				_("AT screen-location"), SYN_CLAUSE_3, 1,
				&check_line_col_duplicate);

	if ((CB_LITERAL_P ((yyvsp[0])) && cb_get_int ((yyvsp[0])) == 0) || (yyvsp[0]) == cb_zero) {
		cb_verify (cb_accept_display_extensions, "COLUMN 0");
	}

	if (!line_column) {
		line_column = CB_BUILD_PAIR (cb_int0, (yyvsp[0]));
	} else {
		CB_PAIR_Y (line_column) = (yyvsp[0]);
	}
  }
#line 14391 "parser.c" /* yacc.c:1646  */
    break;

  case 1001:
#line 7690 "parser.y" /* yacc.c:1646  */
    {
	set_attr_with_conflict (_("AT screen-location"), SYN_CLAUSE_3,
				_("LINE or COLUMN"), SYN_CLAUSE_1 | SYN_CLAUSE_2,
				1, &check_line_col_duplicate);

	cb_verify (cb_accept_display_extensions, "AT clause");

	line_column = (yyvsp[0]);
  }
#line 14405 "parser.c" /* yacc.c:1646  */
    break;

  case 1002:
#line 7702 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 14411 "parser.c" /* yacc.c:1646  */
    break;

  case 1003:
#line 7706 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 14417 "parser.c" /* yacc.c:1646  */
    break;

  case 1004:
#line 7707 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 14423 "parser.c" /* yacc.c:1646  */
    break;

  case 1005:
#line 7712 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
  }
#line 14431 "parser.c" /* yacc.c:1646  */
    break;

  case 1006:
#line 7719 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("AUTO", SYN_CLAUSE_5, &check_duplicate);
	set_dispattr_with_conflict ("AUTO", COB_SCREEN_AUTO,
				    "TAB", COB_SCREEN_TAB);
  }
#line 14441 "parser.c" /* yacc.c:1646  */
    break;

  case 1007:
#line 7725 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("TAB", SYN_CLAUSE_6, &check_duplicate);
	set_dispattr_with_conflict ("TAB", COB_SCREEN_TAB,
				    "AUTO", COB_SCREEN_AUTO);
  }
#line 14451 "parser.c" /* yacc.c:1646  */
    break;

  case 1008:
#line 7731 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("BELL", SYN_CLAUSE_7, &check_duplicate);
	set_dispattr (COB_SCREEN_BELL);
  }
#line 14460 "parser.c" /* yacc.c:1646  */
    break;

  case 1009:
#line 7736 "parser.y" /* yacc.c:1646  */
    {
        check_repeated ("BLINK", SYN_CLAUSE_8, &check_duplicate);
	set_dispattr (COB_SCREEN_BLINK);
  }
#line 14469 "parser.c" /* yacc.c:1646  */
    break;

  case 1010:
#line 7741 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("CONVERSION", SYN_CLAUSE_9, &check_duplicate);
	CB_PENDING ("ACCEPT CONVERSION");
  }
#line 14478 "parser.c" /* yacc.c:1646  */
    break;

  case 1011:
#line 7746 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("FULL", SYN_CLAUSE_10, &check_duplicate);
	set_dispattr (COB_SCREEN_FULL);
  }
#line 14487 "parser.c" /* yacc.c:1646  */
    break;

  case 1012:
#line 7751 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("LEFTLINE", SYN_CLAUSE_12, &check_duplicate);
	set_dispattr (COB_SCREEN_LEFTLINE);
  }
#line 14496 "parser.c" /* yacc.c:1646  */
    break;

  case 1013:
#line 7756 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("LOWER", SYN_CLAUSE_13, &check_duplicate);
	set_dispattr_with_conflict ("LOWER", COB_SCREEN_LOWER,
				    "UPPER", COB_SCREEN_UPPER);
  }
#line 14506 "parser.c" /* yacc.c:1646  */
    break;

  case 1014:
#line 7762 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("HIGHLIGHT", SYN_CLAUSE_11, &check_duplicate);
	set_dispattr_with_conflict ("HIGHLIGHT", COB_SCREEN_HIGHLIGHT,
				    "LOWLIGHT", COB_SCREEN_LOWLIGHT);
  }
#line 14516 "parser.c" /* yacc.c:1646  */
    break;

  case 1015:
#line 7768 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("LOWLIGHT", SYN_CLAUSE_14, &check_duplicate);
	set_dispattr_with_conflict ("LOWLIGHT", COB_SCREEN_LOWLIGHT,
				    "HIGHLIGHT", COB_SCREEN_HIGHLIGHT);
  }
#line 14526 "parser.c" /* yacc.c:1646  */
    break;

  case 1016:
#line 7774 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING("STANDARD intensity");
  }
#line 14534 "parser.c" /* yacc.c:1646  */
    break;

  case 1017:
#line 7778 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING("BACKGROUND intensity");
  }
#line 14542 "parser.c" /* yacc.c:1646  */
    break;

  case 1018:
#line 7782 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING("BACKGROUND intensity");
  }
#line 14550 "parser.c" /* yacc.c:1646  */
    break;

  case 1019:
#line 7786 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING("BACKGROUND intensity");
  }
#line 14558 "parser.c" /* yacc.c:1646  */
    break;

  case 1020:
#line 7790 "parser.y" /* yacc.c:1646  */
    {
	if (cb_no_echo_means_secure) {
		check_repeated ("SECURE", SYN_CLAUSE_20, &check_duplicate);
		set_dispattr (COB_SCREEN_SECURE);
	} else {
		check_repeated ("NO-ECHO", SYN_CLAUSE_15, &check_duplicate);
		set_dispattr_with_conflict ("NO-ECHO", COB_SCREEN_NO_ECHO,
					    "SECURE", COB_SCREEN_SECURE);
	}
  }
#line 14573 "parser.c" /* yacc.c:1646  */
    break;

  case 1021:
#line 7801 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("OVERLINE", SYN_CLAUSE_16, &check_duplicate);
	set_dispattr (COB_SCREEN_OVERLINE);
  }
#line 14582 "parser.c" /* yacc.c:1646  */
    break;

  case 1022:
#line 7806 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("PROMPT", SYN_CLAUSE_17, &check_duplicate);
	set_attribs (NULL, NULL, NULL, NULL, (yyvsp[0]), NULL, COB_SCREEN_PROMPT);
  }
#line 14591 "parser.c" /* yacc.c:1646  */
    break;

  case 1023:
#line 7811 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("PROMPT", SYN_CLAUSE_17, &check_duplicate);
	set_dispattr (COB_SCREEN_PROMPT);
  }
#line 14600 "parser.c" /* yacc.c:1646  */
    break;

  case 1024:
#line 7816 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("REQUIRED", SYN_CLAUSE_18, &check_duplicate);
	set_dispattr (COB_SCREEN_REQUIRED);
  }
#line 14609 "parser.c" /* yacc.c:1646  */
    break;

  case 1025:
#line 7821 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("REVERSE-VIDEO", SYN_CLAUSE_19, &check_duplicate);
	set_dispattr (COB_SCREEN_REVERSE);
  }
#line 14618 "parser.c" /* yacc.c:1646  */
    break;

  case 1026:
#line 7826 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SECURE", SYN_CLAUSE_20, &check_duplicate);
	set_dispattr_with_conflict ("SECURE", COB_SCREEN_SECURE,
				    "NO-ECHO", COB_SCREEN_NO_ECHO);
  }
#line 14628 "parser.c" /* yacc.c:1646  */
    break;

  case 1027:
#line 7832 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SIZE", SYN_CLAUSE_21, &check_duplicate);
	set_attribs (NULL, NULL, NULL, NULL, NULL, (yyvsp[0]), 0);
  }
#line 14637 "parser.c" /* yacc.c:1646  */
    break;

  case 1028:
#line 7837 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SIZE", SYN_CLAUSE_21, &check_duplicate);
	set_attribs (NULL, NULL, NULL, NULL, NULL, (yyvsp[0]), 0);
  }
#line 14646 "parser.c" /* yacc.c:1646  */
    break;

  case 1029:
#line 7842 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("UNDERLINE", SYN_CLAUSE_22, &check_duplicate);
	set_dispattr (COB_SCREEN_UNDERLINE);
  }
#line 14655 "parser.c" /* yacc.c:1646  */
    break;

  case 1030:
#line 7847 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("NO UPDATE", SYN_CLAUSE_23, &check_duplicate);
	set_dispattr_with_conflict ("NO UPDATE", COB_SCREEN_NO_UPDATE,
				    "UPDATE", COB_SCREEN_UPDATE);
  }
#line 14665 "parser.c" /* yacc.c:1646  */
    break;

  case 1031:
#line 7853 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("UPDATE", SYN_CLAUSE_24, &check_duplicate);
	set_dispattr_with_conflict ("UPDATE", COB_SCREEN_UPDATE,
				    "NO UPDATE", COB_SCREEN_NO_UPDATE);
  }
#line 14675 "parser.c" /* yacc.c:1646  */
    break;

  case 1032:
#line 7859 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("UPPER", SYN_CLAUSE_25, &check_duplicate);
	set_dispattr_with_conflict ("UPPER", COB_SCREEN_UPPER,
				    "LOWER", COB_SCREEN_LOWER);
  }
#line 14685 "parser.c" /* yacc.c:1646  */
    break;

  case 1033:
#line 7865 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("FOREGROUND-COLOR", SYN_CLAUSE_26, &check_duplicate);
	check_repeated ("BACKGROUND-COLOR", SYN_CLAUSE_27, &check_duplicate);
	CB_PENDING ("COLOR");
  }
#line 14695 "parser.c" /* yacc.c:1646  */
    break;

  case 1034:
#line 7871 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("FOREGROUND-COLOR", SYN_CLAUSE_26, &check_duplicate);
	set_attribs ((yyvsp[0]), NULL, NULL, NULL, NULL, NULL, 0);
  }
#line 14704 "parser.c" /* yacc.c:1646  */
    break;

  case 1035:
#line 7876 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("BACKGROUND-COLOR", SYN_CLAUSE_27, &check_duplicate);
	set_attribs (NULL, (yyvsp[0]), NULL, NULL, NULL, NULL, 0);
  }
#line 14713 "parser.c" /* yacc.c:1646  */
    break;

  case 1036:
#line 7881 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SCROLL UP", SYN_CLAUSE_28, &check_duplicate);
	set_attribs_with_conflict (NULL, NULL, (yyvsp[0]), NULL, NULL, NULL,
				   "SCROLL UP", COB_SCREEN_SCROLL_UP,
				   "SCROLL DOWN", COB_SCREEN_SCROLL_DOWN);
  }
#line 14724 "parser.c" /* yacc.c:1646  */
    break;

  case 1037:
#line 7888 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SCROLL DOWN", SYN_CLAUSE_19, &check_duplicate);
	set_attribs_with_conflict (NULL, NULL, (yyvsp[0]), NULL, NULL, NULL,
				   "SCROLL DOWN", COB_SCREEN_SCROLL_DOWN,
				   "SCROLL UP", COB_SCREEN_SCROLL_UP);
  }
#line 14735 "parser.c" /* yacc.c:1646  */
    break;

  case 1038:
#line 7895 "parser.y" /* yacc.c:1646  */
    {
	check_repeated (_("TIME-OUT or BEFORE TIME clauses"), SYN_CLAUSE_4,
			&check_duplicate);
	set_attribs (NULL, NULL, NULL, (yyvsp[0]), NULL, NULL, 0);
  }
#line 14745 "parser.c" /* yacc.c:1646  */
    break;

  case 1047:
#line 7921 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), ACCEPT);
  }
#line 14753 "parser.c" /* yacc.c:1646  */
    break;

  case 1048:
#line 7925 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), ACCEPT);
# if 0 /* activate only for debugging purposes for attribs */
	if (current_statement->attr_ptr) {
		print_bits (current_statement->attr_ptr->dispattrs);
	} else {
		fprintf(stderr, "No Attribs\n");
	}
#endif
  }
#line 14768 "parser.c" /* yacc.c:1646  */
    break;

  case 1049:
#line 7942 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("ADD", TERM_ADD);
  }
#line 14776 "parser.c" /* yacc.c:1646  */
    break;

  case 1051:
#line 7951 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_arithmetic ((yyvsp[-1]), '+', cb_build_binary_list ((yyvsp[-3]), '+'));
  }
#line 14784 "parser.c" /* yacc.c:1646  */
    break;

  case 1052:
#line 7955 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_arithmetic ((yyvsp[-1]), 0, cb_build_binary_list ((yyvsp[-4]), '+'));
  }
#line 14792 "parser.c" /* yacc.c:1646  */
    break;

  case 1053:
#line 7959 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_corresponding (cb_build_add, (yyvsp[-2]), (yyvsp[-4]), (yyvsp[-1]));
  }
#line 14800 "parser.c" /* yacc.c:1646  */
    break;

  case 1054:
#line 7963 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("ADD TABLE");
	cb_emit_tab_arithmetic (cb_build_add, (yyvsp[-4]), (yyvsp[-6]), (yyvsp[-3]), (yyvsp[-2]), (yyvsp[-1]));
  }
#line 14809 "parser.c" /* yacc.c:1646  */
    break;

  case 1056:
#line 7971 "parser.y" /* yacc.c:1646  */
    {
	cb_list_add ((yyvsp[-2]), (yyvsp[0]));
  }
#line 14817 "parser.c" /* yacc.c:1646  */
    break;

  case 1057:
#line 7978 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), ADD);
  }
#line 14825 "parser.c" /* yacc.c:1646  */
    break;

  case 1058:
#line 7982 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), ADD);
  }
#line 14833 "parser.c" /* yacc.c:1646  */
    break;

  case 1059:
#line 7992 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("ALLOCATE", 0);
	cobc_cs_check = CB_CS_ALLOCATE;
	current_statement->flag_no_based = 1;
  }
#line 14843 "parser.c" /* yacc.c:1646  */
    break;

  case 1061:
#line 8002 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_allocate ((yyvsp[-2]), (yyvsp[0]), NULL, (yyvsp[-1]));
  }
#line 14851 "parser.c" /* yacc.c:1646  */
    break;

  case 1062:
#line 8006 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0]) == NULL) {
		cb_error_x (CB_TREE (current_statement),
			    _("ALLOCATE CHARACTERS requires RETURNING clause"));
	} else {
		cb_emit_allocate (NULL, (yyvsp[0]), (yyvsp[-3]), (yyvsp[-1]));
	}
  }
#line 14864 "parser.c" /* yacc.c:1646  */
    break;

  case 1063:
#line 8017 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 14870 "parser.c" /* yacc.c:1646  */
    break;

  case 1064:
#line 8018 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 14876 "parser.c" /* yacc.c:1646  */
    break;

  case 1065:
#line 8026 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("ALTER", 0);
	cb_verify (cb_alter_statement, "ALTER");
  }
#line 14885 "parser.c" /* yacc.c:1646  */
    break;

  case 1069:
#line 8040 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_alter ((yyvsp[-3]), (yyvsp[0]));
  }
#line 14893 "parser.c" /* yacc.c:1646  */
    break;

  case 1072:
#line 8052 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("CALL", TERM_CALL);
	cobc_cs_check = CB_CS_CALL;
	call_nothing = 0;
	cobc_allow_program_name = 1;
	call_line_number = cb_source_line;
  }
#line 14905 "parser.c" /* yacc.c:1646  */
    break;

  case 1074:
#line 8065 "parser.y" /* yacc.c:1646  */
    {
	cobc_allow_program_name = 0;
  }
#line 14913 "parser.c" /* yacc.c:1646  */
    break;

  case 1075:
#line 8072 "parser.y" /* yacc.c:1646  */
    {
	int call_conv = 0;

	if (current_program->prog_type == CB_PROGRAM_TYPE
	    && !current_program->flag_recursive
	    && is_recursive_call ((yyvsp[-5]))) {
		cb_warning_x (COBC_WARN_FILLER, (yyvsp[-5]), _("recursive program call - assuming RECURSIVE attribute"));
		current_program->flag_recursive = 1;
	}
	call_conv = current_call_convention;
	if ((yyvsp[-7])) {
		if (CB_INTEGER_P ((yyvsp[-7]))) {
			call_conv |= CB_INTEGER ((yyvsp[-7]))->val;
			if (CB_INTEGER ((yyvsp[-7]))->val & CB_CONV_COBOL) {
				call_conv &= ~CB_CONV_STDCALL;
			} else {
				call_conv &= ~CB_CONV_COBOL;
			}
		} else {
			call_conv = cb_get_int((yyvsp[-7]));
		}
	}
	/* For CALL ... RETURNING NOTHING, set the call convention bit */
	if (call_nothing) {
		call_conv |= CB_CONV_NO_RET_UPD;
	}
	cb_emit_call ((yyvsp[-5]), (yyvsp[-2]), (yyvsp[-1]), CB_PAIR_X ((yyvsp[0])), CB_PAIR_Y ((yyvsp[0])),
		      cb_int (call_conv), (yyvsp[-6]), (yyvsp[-3]));
  }
#line 14947 "parser.c" /* yacc.c:1646  */
    break;

  case 1076:
#line 8105 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
	cobc_cs_check = 0;
  }
#line 14956 "parser.c" /* yacc.c:1646  */
    break;

  case 1077:
#line 8110 "parser.y" /* yacc.c:1646  */
    {
	if (current_call_convention & CB_CONV_COBOL) {
		(yyval) = cb_int (CB_CONV_STATIC_LINK | CB_CONV_COBOL);
	} else {
		(yyval) = cb_int (CB_CONV_STATIC_LINK);
	}
	cobc_cs_check = 0;
  }
#line 14969 "parser.c" /* yacc.c:1646  */
    break;

  case 1078:
#line 8119 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (CB_CONV_STDCALL);
	cobc_cs_check = 0;
  }
#line 14978 "parser.c" /* yacc.c:1646  */
    break;

  case 1079:
#line 8124 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (0);
	cobc_cs_check = 0;
  }
#line 14987 "parser.c" /* yacc.c:1646  */
    break;

  case 1080:
#line 8129 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		x;

	x = cb_ref ((yyvsp[0]));
	if (CB_VALID_TREE (x)) {
		if (CB_SYSTEM_NAME(x)->token != CB_FEATURE_CONVENTION) {
			cb_error_x ((yyvsp[0]), _("invalid mnemonic name"));
			(yyval) = NULL;
		} else {
			(yyval) = CB_SYSTEM_NAME(x)->value;
		}
	} else {
		(yyval) = NULL;
	}
	cobc_cs_check = 0;
  }
#line 15008 "parser.c" /* yacc.c:1646  */
    break;

  case 1081:
#line 8149 "parser.y" /* yacc.c:1646  */
    {
	if (CB_LITERAL_P ((yyvsp[0]))) {
		cb_trim_program_id ((yyvsp[0]));
	}
  }
#line 15018 "parser.c" /* yacc.c:1646  */
    break;

  case 1082:
#line 8155 "parser.y" /* yacc.c:1646  */
    {
	cb_verify (cb_program_prototypes, _("CALL/CANCEL with program-prototype-name"));
	/* hack to push the prototype name */
	if ((yyvsp[0]) && CB_REFERENCE_P ((yyvsp[0]))) {
		if ((yyvsp[-1])) {
			cb_warning_x (COBC_WARN_FILLER, (yyvsp[-1]), _("id/literal ignored, using prototype name"));
		}
		(yyval) = (yyvsp[0]);
	} else if ((yyvsp[-1]) && CB_LITERAL_P ((yyvsp[-1]))) {
		(yyval) = (yyvsp[-1]);
	} else {
		cb_error (_("NESTED phrase is only valid with literal"));
		(yyval) = cb_error_node;
	}
  }
#line 15038 "parser.c" /* yacc.c:1646  */
    break;

  case 1083:
#line 8174 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 15046 "parser.c" /* yacc.c:1646  */
    break;

  case 1084:
#line 8179 "parser.y" /* yacc.c:1646  */
    {
	if (CB_LITERAL_P ((yyvsp[-1]))) {
		cb_trim_program_id ((yyvsp[-1]));
	}
	(yyval) = (yyvsp[-1]);
  }
#line 15057 "parser.c" /* yacc.c:1646  */
    break;

  case 1085:
#line 8189 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("NESTED phrase for CALL statement");
  }
#line 15065 "parser.c" /* yacc.c:1646  */
    break;

  case 1087:
#line 8197 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 15073 "parser.c" /* yacc.c:1646  */
    break;

  case 1088:
#line 8201 "parser.y" /* yacc.c:1646  */
    {
	call_mode = CB_CALL_BY_REFERENCE;
	size_mode = CB_SIZE_4;
  }
#line 15082 "parser.c" /* yacc.c:1646  */
    break;

  case 1089:
#line 8206 "parser.y" /* yacc.c:1646  */
    {
	if (cb_list_length ((yyvsp[0])) > MAX_CALL_FIELD_PARAMS) {
		cb_error_x (CB_TREE (current_statement),
			    _("number of parameters exceeds maximum %d"),
			    MAX_CALL_FIELD_PARAMS);
	}
	(yyval) = (yyvsp[0]);
  }
#line 15095 "parser.c" /* yacc.c:1646  */
    break;

  case 1090:
#line 8217 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 15101 "parser.c" /* yacc.c:1646  */
    break;

  case 1091:
#line 8219 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_append ((yyvsp[-1]), (yyvsp[0])); }
#line 15107 "parser.c" /* yacc.c:1646  */
    break;

  case 1092:
#line 8224 "parser.y" /* yacc.c:1646  */
    {
	if (call_mode != CB_CALL_BY_REFERENCE) {
		cb_error_x (CB_TREE (current_statement),
			    _("OMITTED only allowed when parameters are passed BY REFERENCE"));
	}
	(yyval) = CB_BUILD_PAIR (cb_int (call_mode), cb_null);
  }
#line 15119 "parser.c" /* yacc.c:1646  */
    break;

  case 1093:
#line 8232 "parser.y" /* yacc.c:1646  */
    {
	int	save_mode;

	save_mode = call_mode;
	if (call_mode != CB_CALL_BY_REFERENCE) {
		if (CB_FILE_P ((yyvsp[0])) || (CB_REFERENCE_P ((yyvsp[0])) &&
		    CB_FILE_P (CB_REFERENCE ((yyvsp[0]))->value))) {
			cb_error_x (CB_TREE (current_statement),
				    _("invalid file name reference"));
		} else if (call_mode == CB_CALL_BY_VALUE) {
			if (cb_category_is_alpha ((yyvsp[0]))) {
				cb_warning_x (COBC_WARN_FILLER, (yyvsp[0]),
					      _("BY CONTENT assumed for alphanumeric item"));
				save_mode = CB_CALL_BY_CONTENT;
			}
		}
	}
	(yyval) = CB_BUILD_PAIR (cb_int (save_mode), (yyvsp[0]));
	CB_SIZES ((yyval)) = size_mode;
	call_mode = save_mode;
  }
#line 15145 "parser.c" /* yacc.c:1646  */
    break;

  case 1095:
#line 8258 "parser.y" /* yacc.c:1646  */
    {
	call_mode = CB_CALL_BY_REFERENCE;
  }
#line 15153 "parser.c" /* yacc.c:1646  */
    break;

  case 1096:
#line 8262 "parser.y" /* yacc.c:1646  */
    {
	if (current_program->flag_chained) {
		cb_error_x (CB_TREE (current_statement),
			    _("%s not allowed in CHAINED programs"), "BY CONTENT");
	} else {
		call_mode = CB_CALL_BY_CONTENT;
	}
  }
#line 15166 "parser.c" /* yacc.c:1646  */
    break;

  case 1097:
#line 8271 "parser.y" /* yacc.c:1646  */
    {
	if (current_program->flag_chained) {
		cb_error_x (CB_TREE (current_statement),
			    _("%s not allowed in CHAINED programs"), "BY VALUE");
	} else {
		call_mode = CB_CALL_BY_VALUE;
	}
  }
#line 15179 "parser.c" /* yacc.c:1646  */
    break;

  case 1098:
#line 8283 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 15187 "parser.c" /* yacc.c:1646  */
    break;

  case 1099:
#line 8287 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 15195 "parser.c" /* yacc.c:1646  */
    break;

  case 1100:
#line 8291 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_null;
  }
#line 15203 "parser.c" /* yacc.c:1646  */
    break;

  case 1101:
#line 8295 "parser.y" /* yacc.c:1646  */
    {
	call_nothing = CB_CONV_NO_RET_UPD;
	(yyval) = cb_null;
  }
#line 15212 "parser.c" /* yacc.c:1646  */
    break;

  case 1102:
#line 8300 "parser.y" /* yacc.c:1646  */
    {
	struct cb_field	*f;

	if (cb_ref ((yyvsp[0])) != cb_error_node) {
		f = CB_FIELD_PTR ((yyvsp[0]));
		if (f->level != 1 && f->level != 77) {
			cb_error (_("RETURNING item must have level 01 or 77"));
			(yyval) = NULL;
		} else if (f->storage != CB_STORAGE_LINKAGE &&
			   !f->flag_item_based) {
			cb_error (_("RETURNING item must be a LINKAGE SECTION item or have BASED clause"));
			(yyval) = NULL;
		} else {
			(yyval) = cb_build_address ((yyvsp[0]));
		}
	} else {
		(yyval) = NULL;
	}
  }
#line 15236 "parser.c" /* yacc.c:1646  */
    break;

  case 1107:
#line 8333 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_PAIR (NULL, NULL);
  }
#line 15244 "parser.c" /* yacc.c:1646  */
    break;

  case 1108:
#line 8337 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_PAIR ((yyvsp[-1]), (yyvsp[0]));
  }
#line 15252 "parser.c" /* yacc.c:1646  */
    break;

  case 1109:
#line 8341 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		cb_verify (cb_not_exception_before_exception,
			_("NOT EXCEPTION before EXCEPTION"));
	}
	(yyval) = CB_BUILD_PAIR ((yyvsp[0]), (yyvsp[-1]));
  }
#line 15264 "parser.c" /* yacc.c:1646  */
    break;

  case 1110:
#line 8352 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 15272 "parser.c" /* yacc.c:1646  */
    break;

  case 1111:
#line 8356 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 15280 "parser.c" /* yacc.c:1646  */
    break;

  case 1112:
#line 8363 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 15288 "parser.c" /* yacc.c:1646  */
    break;

  case 1113:
#line 8367 "parser.y" /* yacc.c:1646  */
    {
	cb_verify (cb_call_overflow, "ON OVERFLOW");
	(yyval) = (yyvsp[0]);
  }
#line 15297 "parser.c" /* yacc.c:1646  */
    break;

  case 1114:
#line 8375 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 15305 "parser.c" /* yacc.c:1646  */
    break;

  case 1115:
#line 8379 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 15313 "parser.c" /* yacc.c:1646  */
    break;

  case 1116:
#line 8386 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 15321 "parser.c" /* yacc.c:1646  */
    break;

  case 1117:
#line 8393 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), CALL);
  }
#line 15329 "parser.c" /* yacc.c:1646  */
    break;

  case 1118:
#line 8397 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), CALL);
  }
#line 15337 "parser.c" /* yacc.c:1646  */
    break;

  case 1119:
#line 8407 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("CANCEL", 0);
	cobc_allow_program_name = 1;
  }
#line 15346 "parser.c" /* yacc.c:1646  */
    break;

  case 1120:
#line 8412 "parser.y" /* yacc.c:1646  */
    {
	cobc_allow_program_name = 0;
  }
#line 15354 "parser.c" /* yacc.c:1646  */
    break;

  case 1121:
#line 8419 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_cancel ((yyvsp[0]));
  }
#line 15362 "parser.c" /* yacc.c:1646  */
    break;

  case 1122:
#line 8423 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_cancel ((yyvsp[0]));
  }
#line 15370 "parser.c" /* yacc.c:1646  */
    break;

  case 1124:
#line 8431 "parser.y" /* yacc.c:1646  */
    {
	cb_verify (cb_program_prototypes, _("CALL/CANCEL with program-prototype-name"));
  }
#line 15378 "parser.c" /* yacc.c:1646  */
    break;

  case 1125:
#line 8440 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("CLOSE", 0);
  }
#line 15386 "parser.c" /* yacc.c:1646  */
    break;

  case 1129:
#line 8453 "parser.y" /* yacc.c:1646  */
    {
	begin_implicit_statement ();
	cb_emit_close ((yyvsp[-1]), (yyvsp[0]));
  }
#line 15395 "parser.c" /* yacc.c:1646  */
    break;

  case 1130:
#line 8458 "parser.y" /* yacc.c:1646  */
    {
	begin_implicit_statement ();
	cb_emit_close ((yyvsp[-1]), (yyvsp[0]));
  }
#line 15404 "parser.c" /* yacc.c:1646  */
    break;

  case 1131:
#line 8465 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_CLOSE_NORMAL); }
#line 15410 "parser.c" /* yacc.c:1646  */
    break;

  case 1132:
#line 8466 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_CLOSE_UNIT); }
#line 15416 "parser.c" /* yacc.c:1646  */
    break;

  case 1133:
#line 8467 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_CLOSE_UNIT_REMOVAL); }
#line 15422 "parser.c" /* yacc.c:1646  */
    break;

  case 1134:
#line 8468 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_CLOSE_NO_REWIND); }
#line 15428 "parser.c" /* yacc.c:1646  */
    break;

  case 1135:
#line 8469 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_CLOSE_LOCK); }
#line 15434 "parser.c" /* yacc.c:1646  */
    break;

  case 1136:
#line 8474 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("GRAPHICAL WINDOW");
	current_statement->name = "CLOSE WINDOW";
  }
#line 15443 "parser.c" /* yacc.c:1646  */
    break;

  case 1137:
#line 8479 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_close_window ((yyvsp[-1]), (yyvsp[0]));
  }
#line 15451 "parser.c" /* yacc.c:1646  */
    break;

  case 1138:
#line 8485 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 15457 "parser.c" /* yacc.c:1646  */
    break;

  case 1139:
#line 8486 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int0; }
#line 15463 "parser.c" /* yacc.c:1646  */
    break;

  case 1140:
#line 8494 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("COMPUTE", TERM_COMPUTE);
  }
#line 15471 "parser.c" /* yacc.c:1646  */
    break;

  case 1142:
#line 8503 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_arithmetic ((yyvsp[-3]), 0, (yyvsp[-1]));
  }
#line 15479 "parser.c" /* yacc.c:1646  */
    break;

  case 1143:
#line 8510 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), COMPUTE);
  }
#line 15487 "parser.c" /* yacc.c:1646  */
    break;

  case 1144:
#line 8514 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), COMPUTE);
  }
#line 15495 "parser.c" /* yacc.c:1646  */
    break;

  case 1145:
#line 8524 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("COMMIT", 0);
	cb_emit_commit ();
  }
#line 15504 "parser.c" /* yacc.c:1646  */
    break;

  case 1146:
#line 8535 "parser.y" /* yacc.c:1646  */
    {
	size_t	save_unreached;

	/* Do not check unreached for CONTINUE */
	save_unreached = check_unreached;
	check_unreached = 0;
	begin_statement ("CONTINUE", 0);
	cb_emit_continue ();
	check_unreached = (unsigned int) save_unreached;
  }
#line 15519 "parser.c" /* yacc.c:1646  */
    break;

  case 1147:
#line 8552 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("DESTROY", 0);
	CB_PENDING ("GRAPHICAL CONTROL");
  }
#line 15528 "parser.c" /* yacc.c:1646  */
    break;

  case 1149:
#line 8561 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_destroy (NULL);
  }
#line 15536 "parser.c" /* yacc.c:1646  */
    break;

  case 1150:
#line 8568 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_destroy ((yyvsp[0]));
  }
#line 15544 "parser.c" /* yacc.c:1646  */
    break;

  case 1151:
#line 8578 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("DELETE", TERM_DELETE);
  }
#line 15552 "parser.c" /* yacc.c:1646  */
    break;

  case 1153:
#line 8587 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_delete ((yyvsp[-3]));
  }
#line 15560 "parser.c" /* yacc.c:1646  */
    break;

  case 1155:
#line 8595 "parser.y" /* yacc.c:1646  */
    {
	begin_implicit_statement ();
	cb_emit_delete_file ((yyvsp[0]));
  }
#line 15569 "parser.c" /* yacc.c:1646  */
    break;

  case 1156:
#line 8600 "parser.y" /* yacc.c:1646  */
    {
	begin_implicit_statement ();
	cb_emit_delete_file ((yyvsp[0]));
  }
#line 15578 "parser.c" /* yacc.c:1646  */
    break;

  case 1157:
#line 8608 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), DELETE);
  }
#line 15586 "parser.c" /* yacc.c:1646  */
    break;

  case 1158:
#line 8612 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), DELETE);
  }
#line 15594 "parser.c" /* yacc.c:1646  */
    break;

  case 1159:
#line 8622 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("DISABLE", 0);
  }
#line 15602 "parser.c" /* yacc.c:1646  */
    break;

  case 1163:
#line 8636 "parser.y" /* yacc.c:1646  */
    {
	  /* Add cb_verify for <= COBOL-85 */
  }
#line 15610 "parser.c" /* yacc.c:1646  */
    break;

  case 1169:
#line 8654 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("DISPLAY", TERM_DISPLAY);
	cobc_cs_check = CB_CS_DISPLAY;
	display_type = UNKNOWN_DISPLAY;
	is_first_display_item = 1;
  }
#line 15621 "parser.c" /* yacc.c:1646  */
    break;

  case 1171:
#line 8666 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_env_name ((yyvsp[-2]));
  }
#line 15629 "parser.c" /* yacc.c:1646  */
    break;

  case 1172:
#line 8670 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_env_value ((yyvsp[-2]));
  }
#line 15637 "parser.c" /* yacc.c:1646  */
    break;

  case 1173:
#line 8674 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_arg_number ((yyvsp[-2]));
  }
#line 15645 "parser.c" /* yacc.c:1646  */
    break;

  case 1174:
#line 8678 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_command_line ((yyvsp[-2]));
  }
#line 15653 "parser.c" /* yacc.c:1646  */
    break;

  case 1180:
#line 8690 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0]) != NULL) {
		error_if_different_display_type ((yyvsp[0]), NULL, NULL, NULL);
		cb_emit_display ((yyvsp[0]), NULL, cb_int1, NULL, NULL, 0,
				 display_type);
	}
  }
#line 15665 "parser.c" /* yacc.c:1646  */
    break;

  case 1181:
#line 8698 "parser.y" /* yacc.c:1646  */
    {
	set_display_type ((yyvsp[0]), NULL, NULL, NULL);
	cb_emit_display ((yyvsp[0]), NULL, cb_int1, NULL, NULL, 1,
			 display_type);
  }
#line 15675 "parser.c" /* yacc.c:1646  */
    break;

  case 1184:
#line 8712 "parser.y" /* yacc.c:1646  */
    {
	check_duplicate = 0;
	check_line_col_duplicate = 0;
	advancing_value = cb_int1;
	upon_value = NULL;
	line_column = NULL;
  }
#line 15687 "parser.c" /* yacc.c:1646  */
    break;

  case 1185:
#line 8720 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[-2]) == cb_null) {
		/* Emit DISPLAY OMITTED. */
		CB_UNFINISHED_X (CB_TREE(current_statement), "DISPLAY OMITTED");
		error_if_no_advancing_in_screen_display (advancing_value);
	}

	/* Emit device or screen DISPLAY. */

	/*
	  Check that disp_list does not contain an invalid mix of fields.
	*/
	if (display_type == UNKNOWN_DISPLAY) {
		set_display_type ((yyvsp[-2]), upon_value, line_column,
				  current_statement->attr_ptr);
	} else {
		error_if_different_display_type ((yyvsp[-2]), upon_value,
						 line_column,
						 current_statement->attr_ptr);
	}

	if (display_type == SCREEN_DISPLAY
	    || display_type == FIELD_ON_SCREEN_DISPLAY) {
		error_if_no_advancing_in_screen_display (advancing_value);
	}

	cb_emit_display ((yyvsp[-2]), upon_value, advancing_value, line_column,
			 current_statement->attr_ptr,
			 is_first_display_item, display_type);

	is_first_display_item = 0;
  }
#line 15724 "parser.c" /* yacc.c:1646  */
    break;

  case 1186:
#line 8756 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 15732 "parser.c" /* yacc.c:1646  */
    break;

  case 1187:
#line 8760 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_null;
  }
#line 15740 "parser.c" /* yacc.c:1646  */
    break;

  case 1190:
#line 8772 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("UPON", SYN_CLAUSE_1, &check_duplicate);
  }
#line 15748 "parser.c" /* yacc.c:1646  */
    break;

  case 1191:
#line 8776 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("NO ADVANCING", SYN_CLAUSE_2, &check_duplicate);
	advancing_value = cb_int0;
  }
#line 15757 "parser.c" /* yacc.c:1646  */
    break;

  case 1192:
#line 8781 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("MODE IS BLOCK", SYN_CLAUSE_3, &check_duplicate);
  }
#line 15765 "parser.c" /* yacc.c:1646  */
    break;

  case 1195:
#line 8790 "parser.y" /* yacc.c:1646  */
    {
	upon_value = cb_build_display_mnemonic ((yyvsp[0]));
  }
#line 15773 "parser.c" /* yacc.c:1646  */
    break;

  case 1196:
#line 8794 "parser.y" /* yacc.c:1646  */
    {
	upon_value = cb_build_display_name ((yyvsp[0]));
  }
#line 15781 "parser.c" /* yacc.c:1646  */
    break;

  case 1197:
#line 8798 "parser.y" /* yacc.c:1646  */
    {
	upon_value = cb_int0;
  }
#line 15789 "parser.c" /* yacc.c:1646  */
    break;

  case 1198:
#line 8802 "parser.y" /* yacc.c:1646  */
    {
	upon_value = cb_null;
  }
#line 15797 "parser.c" /* yacc.c:1646  */
    break;

  case 1201:
#line 8814 "parser.y" /* yacc.c:1646  */
    {
	CB_UNFINISHED_X (CB_TREE(current_statement), "DISPLAY MESSAGE");
	upon_value = NULL;  
  }
#line 15806 "parser.c" /* yacc.c:1646  */
    break;

  case 1202:
#line 8819 "parser.y" /* yacc.c:1646  */
    {
	/* for now: minimal support for display and prompt only */
	if (upon_value) {
		cb_emit_display (CB_LIST_INIT (upon_value), NULL, NULL, NULL,
				 NULL, 1, FIELD_ON_SCREEN_DISPLAY);
	}
	cb_emit_display ((yyvsp[-2]), NULL, NULL, NULL,
			 NULL, 1, FIELD_ON_SCREEN_DISPLAY);
	cb_emit_accept (cb_null, NULL, NULL);
  }
#line 15821 "parser.c" /* yacc.c:1646  */
    break;

  case 1207:
#line 8843 "parser.y" /* yacc.c:1646  */
    {
	upon_value = (yyvsp[0]);
  }
#line 15829 "parser.c" /* yacc.c:1646  */
    break;

  case 1212:
#line 8854 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("GRAPHICAL WINDOW");
	current_statement->name = "DISPLAY WINDOW";
  }
#line 15838 "parser.c" /* yacc.c:1646  */
    break;

  case 1213:
#line 8859 "parser.y" /* yacc.c:1646  */
    {
	check_duplicate = 0;
	check_line_col_duplicate = 0;
	line_column = NULL;
	upon_value = NULL; /* Hack: stores the POP-UP AREA */
  }
#line 15849 "parser.c" /* yacc.c:1646  */
    break;

  case 1214:
#line 8866 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_display_window (NULL, upon_value, (yyvsp[-2]), line_column,
			 current_statement->attr_ptr);
  }
#line 15858 "parser.c" /* yacc.c:1646  */
    break;

  case 1217:
#line 8879 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("GRAPHICAL WINDOW");
	current_statement->name = "DISPLAY FLOATING WINDOW";
  }
#line 15867 "parser.c" /* yacc.c:1646  */
    break;

  case 1218:
#line 8884 "parser.y" /* yacc.c:1646  */
    {
	check_duplicate = 0;
	check_line_col_duplicate = 0;
	line_column = NULL;
	upon_value = NULL; /* Hack: stores the POP-UP AREA */
  }
#line 15878 "parser.c" /* yacc.c:1646  */
    break;

  case 1219:
#line 8891 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[-5])) {
		/* TODO: set "CELL WIDTH" and "CELL HEIGHT" to "LABEL FONT" */
		/* if not set already */
	}
	cb_emit_display_window (cb_int0, upon_value, (yyvsp[-3]), line_column,
			 current_statement->attr_ptr);
  }
#line 15891 "parser.c" /* yacc.c:1646  */
    break;

  case 1220:
#line 8903 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("GRAPHICAL WINDOW");
	current_statement->name = "DISPLAY INITIAL WINDOW";
	check_duplicate = 0;
	check_line_col_duplicate = 0;
	line_column = NULL;
	upon_value = NULL; /* Hack: stores the POP-UP AREA */
  }
#line 15904 "parser.c" /* yacc.c:1646  */
    break;

  case 1221:
#line 8912 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[-3])) {
		/* TODO: set "CELL WIDTH" and "CELL HEIGHT" to "LABEL FONT" */
		/* if not set already */
	}
	cb_emit_display_window ((yyvsp[-4]), upon_value, NULL, line_column,
			 current_statement->attr_ptr);
  }
#line 15917 "parser.c" /* yacc.c:1646  */
    break;

  case 1222:
#line 8923 "parser.y" /* yacc.c:1646  */
    {(yyval) = cb_int1;}
#line 15923 "parser.c" /* yacc.c:1646  */
    break;

  case 1223:
#line 8924 "parser.y" /* yacc.c:1646  */
    {(yyval) = cb_int2;}
#line 15929 "parser.c" /* yacc.c:1646  */
    break;

  case 1224:
#line 8925 "parser.y" /* yacc.c:1646  */
    {(yyval) = cb_int3;}
#line 15935 "parser.c" /* yacc.c:1646  */
    break;

  case 1225:
#line 8929 "parser.y" /* yacc.c:1646  */
    {(yyval) = NULL;}
#line 15941 "parser.c" /* yacc.c:1646  */
    break;

  case 1226:
#line 8930 "parser.y" /* yacc.c:1646  */
    {(yyval) = cb_int1;}
#line 15947 "parser.c" /* yacc.c:1646  */
    break;

  case 1227:
#line 8935 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 15955 "parser.c" /* yacc.c:1646  */
    break;

  case 1228:
#line 8939 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 15963 "parser.c" /* yacc.c:1646  */
    break;

  case 1232:
#line 8954 "parser.y" /* yacc.c:1646  */
    {
	/* TODO: store */
  }
#line 15971 "parser.c" /* yacc.c:1646  */
    break;

  case 1244:
#line 8982 "parser.y" /* yacc.c:1646  */
    {
	if (upon_value) {
		emit_duplicate_clause_message("POP-UP AREA");
	}
	upon_value = (yyvsp[0]);
  }
#line 15982 "parser.c" /* yacc.c:1646  */
    break;

  case 1245:
#line 8992 "parser.y" /* yacc.c:1646  */
    {
	if (strcmp (current_statement->name, "DISPLAY WINDOW")) {
		cb_error_x ((yyvsp[-2]), _("HANDLE clause invalid for %s"), 
			current_statement->name);
		upon_value = cb_error_node;
	} else{
		if (upon_value) {
			emit_duplicate_clause_message("POP-UP AREA / HANDLE IN");
		}
		upon_value = (yyvsp[0]);
	}
  }
#line 15999 "parser.c" /* yacc.c:1646  */
    break;

  case 1246:
#line 9008 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("BELL", SYN_CLAUSE_4, &check_duplicate);
	set_dispattr (COB_SCREEN_BELL);
  }
#line 16008 "parser.c" /* yacc.c:1646  */
    break;

  case 1247:
#line 9013 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("BLANK LINE", SYN_CLAUSE_5, &check_duplicate);
	set_dispattr_with_conflict ("BLANK LINE", COB_SCREEN_BLANK_LINE,
				    "BLANK SCREEN", COB_SCREEN_BLANK_SCREEN);
  }
#line 16018 "parser.c" /* yacc.c:1646  */
    break;

  case 1248:
#line 9019 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("BLANK SCREEN", SYN_CLAUSE_6, &check_duplicate);
	set_dispattr_with_conflict ("BLANK SCREEN", COB_SCREEN_BLANK_SCREEN,
				    "BLANK LINE", COB_SCREEN_BLANK_LINE);
  }
#line 16028 "parser.c" /* yacc.c:1646  */
    break;

  case 1249:
#line 9025 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("BLINK", SYN_CLAUSE_7, &check_duplicate);
	set_dispattr (COB_SCREEN_BLINK);
  }
#line 16037 "parser.c" /* yacc.c:1646  */
    break;

  case 1250:
#line 9030 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("CONVERSION", SYN_CLAUSE_8, &check_duplicate);
	cb_warning (COBC_WARN_FILLER, _("ignoring CONVERSION"));
  }
#line 16046 "parser.c" /* yacc.c:1646  */
    break;

  case 1251:
#line 9035 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ERASE EOL", SYN_CLAUSE_9, &check_duplicate);
	set_dispattr_with_conflict ("ERASE EOL", COB_SCREEN_ERASE_EOL,
				    "ERASE EOS", COB_SCREEN_ERASE_EOS);
  }
#line 16056 "parser.c" /* yacc.c:1646  */
    break;

  case 1252:
#line 9041 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ERASE EOS", SYN_CLAUSE_10, &check_duplicate);
	set_dispattr_with_conflict ("ERASE EOS", COB_SCREEN_ERASE_EOS,
				    "ERASE EOL", COB_SCREEN_ERASE_EOL);
  }
#line 16066 "parser.c" /* yacc.c:1646  */
    break;

  case 1253:
#line 9047 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("HIGHLIGHT", SYN_CLAUSE_11, &check_duplicate);
	set_dispattr_with_conflict ("HIGHLIGHT", COB_SCREEN_HIGHLIGHT,
				    "LOWLIGHT", COB_SCREEN_LOWLIGHT);
  }
#line 16076 "parser.c" /* yacc.c:1646  */
    break;

  case 1254:
#line 9053 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("LOWLIGHT", SYN_CLAUSE_12, &check_duplicate);
	set_dispattr_with_conflict ("LOWLIGHT", COB_SCREEN_LOWLIGHT,
				    "HIGHLIGHT", COB_SCREEN_HIGHLIGHT);
  }
#line 16086 "parser.c" /* yacc.c:1646  */
    break;

  case 1255:
#line 9059 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING("STANDARD intensity");
  }
#line 16094 "parser.c" /* yacc.c:1646  */
    break;

  case 1256:
#line 9063 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING("BACKGROUND intensity");
  }
#line 16102 "parser.c" /* yacc.c:1646  */
    break;

  case 1257:
#line 9067 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING("BACKGROUND intensity");
  }
#line 16110 "parser.c" /* yacc.c:1646  */
    break;

  case 1258:
#line 9071 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING("BACKGROUND intensity");
  }
#line 16118 "parser.c" /* yacc.c:1646  */
    break;

  case 1259:
#line 9075 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("OVERLINE", SYN_CLAUSE_13, &check_duplicate);
	set_dispattr (COB_SCREEN_OVERLINE);
  }
#line 16127 "parser.c" /* yacc.c:1646  */
    break;

  case 1260:
#line 9080 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("REVERSE-VIDEO", SYN_CLAUSE_14, &check_duplicate);
	set_dispattr (COB_SCREEN_REVERSE);
  }
#line 16136 "parser.c" /* yacc.c:1646  */
    break;

  case 1261:
#line 9085 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SIZE", SYN_CLAUSE_15, &check_duplicate);
	set_attribs (NULL, NULL, NULL, NULL, NULL, (yyvsp[0]), 0);
  }
#line 16145 "parser.c" /* yacc.c:1646  */
    break;

  case 1262:
#line 9090 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("UNDERLINE", SYN_CLAUSE_16, &check_duplicate);
	set_dispattr (COB_SCREEN_UNDERLINE);
  }
#line 16154 "parser.c" /* yacc.c:1646  */
    break;

  case 1263:
#line 9095 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("FOREGROUND-COLOR", SYN_CLAUSE_17, &check_duplicate);
	check_repeated ("BACKGROUND-COLOR", SYN_CLAUSE_18, &check_duplicate);
	CB_PENDING ("COLOR");
  }
#line 16164 "parser.c" /* yacc.c:1646  */
    break;

  case 1264:
#line 9101 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("FOREGROUND-COLOR", SYN_CLAUSE_17, &check_duplicate);
	set_attribs ((yyvsp[0]), NULL, NULL, NULL, NULL, NULL, 0);
  }
#line 16173 "parser.c" /* yacc.c:1646  */
    break;

  case 1265:
#line 9106 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("BACKGROUND-COLOR", SYN_CLAUSE_18, &check_duplicate);
	set_attribs (NULL, (yyvsp[0]), NULL, NULL, NULL, NULL, 0);
  }
#line 16182 "parser.c" /* yacc.c:1646  */
    break;

  case 1266:
#line 9111 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SCROLL UP", SYN_CLAUSE_19, &check_duplicate);
	set_attribs_with_conflict (NULL, NULL, (yyvsp[0]), NULL, NULL, NULL,
				   "SCROLL UP", COB_SCREEN_SCROLL_UP,
				   "SCROLL DOWN", COB_SCREEN_SCROLL_DOWN);
  }
#line 16193 "parser.c" /* yacc.c:1646  */
    break;

  case 1267:
#line 9118 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SCROLL DOWN", SYN_CLAUSE_20, &check_duplicate);
	set_attribs_with_conflict (NULL, NULL, (yyvsp[0]), NULL, NULL, NULL,
				   "SCROLL DOWN", COB_SCREEN_SCROLL_DOWN,
				   "SCROLL UP", COB_SCREEN_SCROLL_UP);
  }
#line 16204 "parser.c" /* yacc.c:1646  */
    break;

  case 1268:
#line 9128 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), DISPLAY);
  }
#line 16212 "parser.c" /* yacc.c:1646  */
    break;

  case 1269:
#line 9132 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), DISPLAY);
  }
#line 16220 "parser.c" /* yacc.c:1646  */
    break;

  case 1270:
#line 9142 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("DIVIDE", TERM_DIVIDE);
  }
#line 16228 "parser.c" /* yacc.c:1646  */
    break;

  case 1272:
#line 9151 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_arithmetic ((yyvsp[-1]), '/', (yyvsp[-3]));
  }
#line 16236 "parser.c" /* yacc.c:1646  */
    break;

  case 1273:
#line 9155 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_arithmetic ((yyvsp[-1]), 0, cb_build_binary_op ((yyvsp[-3]), '/', (yyvsp[-5])));
  }
#line 16244 "parser.c" /* yacc.c:1646  */
    break;

  case 1274:
#line 9159 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_arithmetic ((yyvsp[-1]), 0, cb_build_binary_op ((yyvsp[-5]), '/', (yyvsp[-3])));
  }
#line 16252 "parser.c" /* yacc.c:1646  */
    break;

  case 1275:
#line 9163 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_divide ((yyvsp[-5]), (yyvsp[-7]), (yyvsp[-3]), (yyvsp[-1]));
  }
#line 16260 "parser.c" /* yacc.c:1646  */
    break;

  case 1276:
#line 9167 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_divide ((yyvsp[-7]), (yyvsp[-5]), (yyvsp[-3]), (yyvsp[-1]));
  }
#line 16268 "parser.c" /* yacc.c:1646  */
    break;

  case 1277:
#line 9174 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), DIVIDE);
  }
#line 16276 "parser.c" /* yacc.c:1646  */
    break;

  case 1278:
#line 9178 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), DIVIDE);
  }
#line 16284 "parser.c" /* yacc.c:1646  */
    break;

  case 1279:
#line 9188 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("ENABLE", 0);
  }
#line 16292 "parser.c" /* yacc.c:1646  */
    break;

  case 1281:
#line 9199 "parser.y" /* yacc.c:1646  */
    {
	check_unreached = 0;
	begin_statement ("ENTRY", 0);
  }
#line 16301 "parser.c" /* yacc.c:1646  */
    break;

  case 1283:
#line 9208 "parser.y" /* yacc.c:1646  */
    {
	if (current_program->nested_level) {
		cb_error (_("%s is invalid in nested program"), "ENTRY");
	} else if (current_program->prog_type == CB_FUNCTION_TYPE) {
		cb_error (_("%s is invalid in a user FUNCTION"), "ENTRY");
	} else if (cb_verify (cb_entry_statement, "ENTRY")) {
		if (!cobc_check_valid_name ((char *)(CB_LITERAL ((yyvsp[-1]))->data), ENTRY_NAME)) {
			emit_entry ((char *)(CB_LITERAL ((yyvsp[-1]))->data), 1, (yyvsp[0]), (yyvsp[-2]));
		}
	}
  }
#line 16317 "parser.c" /* yacc.c:1646  */
    break;

  case 1284:
#line 9226 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("EVALUATE", TERM_EVALUATE);
	eval_level++;
	if (eval_level >= EVAL_DEPTH) {
		cb_error (_("maximum evaluate depth exceeded (%d)"),
			  EVAL_DEPTH);
		eval_level = 0;
		eval_inc = 0;
		eval_inc2 = 0;
		YYERROR;
	} else {
		for (eval_inc = 0; eval_inc < EVAL_DEPTH; ++eval_inc) {
			eval_check[eval_level][eval_inc] = NULL;
		}
		eval_inc = 0;
		eval_inc2 = 0;
	}
	cb_end_cond (cb_any);
	cb_save_cond ();
	cb_true_side ();
  }
#line 16343 "parser.c" /* yacc.c:1646  */
    break;

  case 1286:
#line 9253 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_evaluate ((yyvsp[-1]), (yyvsp[0]));
	eval_level--;
  }
#line 16352 "parser.c" /* yacc.c:1646  */
    break;

  case 1287:
#line 9260 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 16358 "parser.c" /* yacc.c:1646  */
    break;

  case 1288:
#line 9262 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-2]), (yyvsp[0])); }
#line 16364 "parser.c" /* yacc.c:1646  */
    break;

  case 1289:
#line 9267 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
	eval_check[eval_level][eval_inc++] = (yyvsp[0]);
	if (eval_inc >= EVAL_DEPTH) {
		cb_error (_("maximum evaluate depth exceeded (%d)"),
			  EVAL_DEPTH);
		eval_inc = 0;
		YYERROR;
	}
  }
#line 16379 "parser.c" /* yacc.c:1646  */
    break;

  case 1290:
#line 9278 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_true;
	eval_check[eval_level][eval_inc++] = NULL;
	if (eval_inc >= EVAL_DEPTH) {
		cb_error (_("maximum evaluate depth exceeded (%d)"),
			  EVAL_DEPTH);
		eval_inc = 0;
		YYERROR;
	}
  }
#line 16394 "parser.c" /* yacc.c:1646  */
    break;

  case 1291:
#line 9289 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_false;
	eval_check[eval_level][eval_inc++] = cb_false;
	if (eval_inc >= EVAL_DEPTH) {
		cb_error (_("maximum evaluate depth exceeded (%d)"),
			  EVAL_DEPTH);
		eval_inc = 0;
		YYERROR;
	}
  }
#line 16409 "parser.c" /* yacc.c:1646  */
    break;

  case 1292:
#line 9303 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0]));
  }
#line 16417 "parser.c" /* yacc.c:1646  */
    break;

  case 1293:
#line 9308 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 16425 "parser.c" /* yacc.c:1646  */
    break;

  case 1294:
#line 9314 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 16431 "parser.c" /* yacc.c:1646  */
    break;

  case 1295:
#line 9316 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0])); }
#line 16437 "parser.c" /* yacc.c:1646  */
    break;

  case 1296:
#line 9322 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_CHAIN ((yyvsp[0]), (yyvsp[-1]));
	eval_inc2 = 0;
  }
#line 16446 "parser.c" /* yacc.c:1646  */
    break;

  case 1297:
#line 9331 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_CHAIN ((yyvsp[0]), NULL);
	eval_inc2 = 0;
  }
#line 16455 "parser.c" /* yacc.c:1646  */
    break;

  case 1298:
#line 9339 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
	eval_inc2 = 0;
  }
#line 16464 "parser.c" /* yacc.c:1646  */
    break;

  case 1299:
#line 9345 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[-2]), (yyvsp[0]));
	eval_inc2 = 0;
  }
#line 16473 "parser.c" /* yacc.c:1646  */
    break;

  case 1300:
#line 9352 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 16479 "parser.c" /* yacc.c:1646  */
    break;

  case 1301:
#line 9354 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-2]), (yyvsp[0])); }
#line 16485 "parser.c" /* yacc.c:1646  */
    break;

  case 1302:
#line 9359 "parser.y" /* yacc.c:1646  */
    {
	cb_tree	not0;
	cb_tree	e1;
	cb_tree	e2;
	cb_tree	x;
	cb_tree	parm1;

	not0 = cb_int0;
	e2 = (yyvsp[0]);
	x = NULL;
	parm1 = (yyvsp[-1]);
	if (eval_check[eval_level][eval_inc2]
	 && eval_check[eval_level][eval_inc2] != cb_false) {
		/* Check if the first token is NOT */
		/* It may belong to the EVALUATE, however see */
		/* below when it may be part of a partial expression */
		if (CB_PURPOSE_INT (parm1) == '!') {
			/* Pop stack if subject not TRUE / FALSE */
			not0 = cb_int1;
			x = parm1;
			parm1 = CB_CHAIN (parm1);
		}
		/* Partial expression handling */
		switch (CB_PURPOSE_INT (parm1)) {
		/* Relational conditions */
		case '<':
		case '>':
		case '[':
		case ']':
		case '~':
		case '=':
		/* Class conditions */
		case '9':
		case 'A':
		case 'L':
		case 'U':
		case 'P':
		case 'N':
		case 'O':
		case 'C':
			if (e2) {
				cb_error_x (e2, _("invalid THROUGH usage"));
				e2 = NULL;
			}
			not0 = CB_PURPOSE (parm1);
			if (x) {
				/* Rebind the NOT to the partial expression */
				parm1 = cb_build_list (cb_int ('!'), NULL, parm1);
			}
			/* Insert subject at head of list */
			parm1 = cb_build_list (cb_int ('x'),
					    eval_check[eval_level][eval_inc2], parm1);
			break;
		}
	}

	/* Build expr now */
	e1 = cb_build_expr (parm1);

	eval_inc2++;
	(yyval) = CB_BUILD_PAIR (not0, CB_BUILD_PAIR (e1, e2));

	if (eval_check[eval_level][eval_inc2-1] == cb_false) {
		/* It was  EVALUATE FALSE; So flip condition */
		if (e1 == cb_true)
			e1 = cb_false;
		else if (e1 == cb_false)
			e1 = cb_true;
	}
	cb_terminate_cond ();
	cb_end_cond (e1);
	cb_save_cond ();
	cb_true_side ();
  }
#line 16564 "parser.c" /* yacc.c:1646  */
    break;

  case 1303:
#line 9433 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_any; eval_inc2++; }
#line 16570 "parser.c" /* yacc.c:1646  */
    break;

  case 1304:
#line 9434 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_true; eval_inc2++; }
#line 16576 "parser.c" /* yacc.c:1646  */
    break;

  case 1305:
#line 9435 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_false; eval_inc2++; }
#line 16582 "parser.c" /* yacc.c:1646  */
    break;

  case 1306:
#line 9439 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 16588 "parser.c" /* yacc.c:1646  */
    break;

  case 1307:
#line 9440 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 16594 "parser.c" /* yacc.c:1646  */
    break;

  case 1308:
#line 9445 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), EVALUATE);
  }
#line 16602 "parser.c" /* yacc.c:1646  */
    break;

  case 1309:
#line 9449 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), EVALUATE);
  }
#line 16610 "parser.c" /* yacc.c:1646  */
    break;

  case 1310:
#line 9459 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("EXIT", 0);
	cobc_cs_check = CB_CS_EXIT;
  }
#line 16619 "parser.c" /* yacc.c:1646  */
    break;

  case 1311:
#line 9464 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
  }
#line 16627 "parser.c" /* yacc.c:1646  */
    break;

  case 1313:
#line 9473 "parser.y" /* yacc.c:1646  */
    {
	if (in_declaratives && use_global_ind) {
		cb_error_x (CB_TREE (current_statement),
			    _("EXIT PROGRAM is not allowed within a USE GLOBAL procedure"));
	}
	if (current_program->prog_type != CB_PROGRAM_TYPE) {
		cb_error_x (CB_TREE (current_statement),
			    _("EXIT PROGRAM not allowed within a FUNCTION"));
	}
	if (current_program->flag_main) {
		check_unreached = 0;
	} else {
		check_unreached = 1;
	}
	if ((yyvsp[0])) {
		if (!current_program->cb_return_code) {
			cb_error_x ((yyvsp[0]), _("RETURNING/GIVING not allowed for non-returning sources"));
		} else {
			cb_emit_move ((yyvsp[0]), CB_LIST_INIT (current_program->cb_return_code));
		}
	}
	current_statement->name = (const char *)"EXIT PROGRAM";
	cb_emit_exit (0);
  }
#line 16656 "parser.c" /* yacc.c:1646  */
    break;

  case 1314:
#line 9498 "parser.y" /* yacc.c:1646  */
    {
	if (in_declaratives && use_global_ind) {
		cb_error_x (CB_TREE (current_statement),
			    _("EXIT FUNCTION is not allowed within a USE GLOBAL procedure"));
	}
	if (current_program->prog_type != CB_FUNCTION_TYPE) {
		cb_error_x (CB_TREE (current_statement),
			    _("EXIT FUNCTION only allowed within a FUNCTION"));
	}
	check_unreached = 1;
	current_statement->name = (const char *)"EXIT FUNCTION";
	cb_emit_exit (0);
  }
#line 16674 "parser.c" /* yacc.c:1646  */
    break;

  case 1315:
#line 9512 "parser.y" /* yacc.c:1646  */
    {
	struct cb_perform	*p;
	cb_tree			plabel;
	char			name[64];

	if (!perform_stack) {
		cb_error_x (CB_TREE (current_statement),
			    _("EXIT PERFORM is only valid with inline PERFORM"));
	} else if (CB_VALUE (perform_stack) != cb_error_node) {
		p = CB_PERFORM (CB_VALUE (perform_stack));
		if (!p->cycle_label) {
			sprintf (name, "EXIT PERFORM CYCLE %d", cb_id);
			p->cycle_label = cb_build_reference (name);
			plabel = cb_build_label (p->cycle_label, NULL);
			CB_LABEL (plabel)->flag_begin = 1;
			CB_LABEL (plabel)->flag_dummy_exit = 1;
		}
		current_statement->name = (const char *)"EXIT PERFORM CYCLE";
		cb_emit_goto (CB_LIST_INIT (p->cycle_label), NULL);
	}
  }
#line 16700 "parser.c" /* yacc.c:1646  */
    break;

  case 1316:
#line 9534 "parser.y" /* yacc.c:1646  */
    {
	struct cb_perform	*p;
	cb_tree			plabel;
	char			name[64];

	if (!perform_stack) {
		cb_error_x (CB_TREE (current_statement),
			    _("EXIT PERFORM is only valid with inline PERFORM"));
	} else if (CB_VALUE (perform_stack) != cb_error_node) {
		p = CB_PERFORM (CB_VALUE (perform_stack));
		if (!p->exit_label) {
			sprintf (name, "EXIT PERFORM %d", cb_id);
			p->exit_label = cb_build_reference (name);
			plabel = cb_build_label (p->exit_label, NULL);
			CB_LABEL (plabel)->flag_begin = 1;
			CB_LABEL (plabel)->flag_dummy_exit = 1;
		}
		current_statement->name = (const char *)"EXIT PERFORM";
		cb_emit_goto (CB_LIST_INIT (p->exit_label), NULL);
	}
  }
#line 16726 "parser.c" /* yacc.c:1646  */
    break;

  case 1317:
#line 9556 "parser.y" /* yacc.c:1646  */
    {
	cb_tree	plabel;
	char	name[64];

	if (!current_section) {
		cb_error_x (CB_TREE (current_statement),
			    _("EXIT SECTION is only valid with an active SECTION"));
	} else {
		if (!current_section->exit_label) {
			sprintf (name, "EXIT SECTION %d", cb_id);
			current_section->exit_label = cb_build_reference (name);
			plabel = cb_build_label (current_section->exit_label, NULL);
			CB_LABEL (plabel)->flag_begin = 1;
			CB_LABEL (plabel)->flag_dummy_exit = 1;
		}
		current_statement->name = (const char *)"EXIT SECTION";
		cb_emit_goto (CB_LIST_INIT (current_section->exit_label), NULL);
	}
  }
#line 16750 "parser.c" /* yacc.c:1646  */
    break;

  case 1318:
#line 9576 "parser.y" /* yacc.c:1646  */
    {
	cb_tree	plabel;
	char	name[64];

	if (!current_paragraph) {
		cb_error_x (CB_TREE (current_statement),
			    _("EXIT PARAGRAPH is only valid with an active PARAGRAPH"));
	} else {
		if (!current_paragraph->exit_label) {
			sprintf (name, "EXIT PARAGRAPH %d", cb_id);
			current_paragraph->exit_label = cb_build_reference (name);
			plabel = cb_build_label (current_paragraph->exit_label, NULL);
			CB_LABEL (plabel)->flag_begin = 1;
			CB_LABEL (plabel)->flag_dummy_exit = 1;
		}
		current_statement->name = (const char *)"EXIT PARAGRAPH";
		cb_emit_goto (CB_LIST_INIT (current_paragraph->exit_label), NULL);
	}
  }
#line 16774 "parser.c" /* yacc.c:1646  */
    break;

  case 1319:
#line 9598 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 16780 "parser.c" /* yacc.c:1646  */
    break;

  case 1320:
#line 9599 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 16786 "parser.c" /* yacc.c:1646  */
    break;

  case 1321:
#line 9607 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("FREE", 0);
	current_statement->flag_no_based = 1;
  }
#line 16795 "parser.c" /* yacc.c:1646  */
    break;

  case 1323:
#line 9616 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_free ((yyvsp[0]));
  }
#line 16803 "parser.c" /* yacc.c:1646  */
    break;

  case 1324:
#line 9626 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("GENERATE", 0);
	CB_PENDING("GENERATE");
  }
#line 16812 "parser.c" /* yacc.c:1646  */
    break;

  case 1327:
#line 9642 "parser.y" /* yacc.c:1646  */
    {
	if (!current_paragraph->flag_statement) {
		current_paragraph->flag_first_is_goto = 1;
	}
	begin_statement ("GO TO", 0);
	save_debug = start_debug;
	start_debug = 0;
  }
#line 16825 "parser.c" /* yacc.c:1646  */
    break;

  case 1329:
#line 9655 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_goto ((yyvsp[-1]), (yyvsp[0]));
	start_debug = save_debug;
  }
#line 16834 "parser.c" /* yacc.c:1646  */
    break;

  case 1330:
#line 9663 "parser.y" /* yacc.c:1646  */
    {
	check_unreached = 1;
	(yyval) = NULL;
  }
#line 16843 "parser.c" /* yacc.c:1646  */
    break;

  case 1331:
#line 9668 "parser.y" /* yacc.c:1646  */
    {
	check_unreached = 0;
	(yyval) = (yyvsp[0]);
  }
#line 16852 "parser.c" /* yacc.c:1646  */
    break;

  case 1332:
#line 9679 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("GOBACK", 0);
	check_unreached = 1;
	if ((yyvsp[0])) {
		if (!current_program->cb_return_code) {
			cb_error_x ((yyvsp[0]), _("RETURNING/GIVING not allowed for non-returning sources"));
		} else {
			cb_emit_move ((yyvsp[0]), CB_LIST_INIT (current_program->cb_return_code));
		}
	}
	cb_emit_exit (1U);
  }
#line 16869 "parser.c" /* yacc.c:1646  */
    break;

  case 1333:
#line 9698 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("IF", TERM_IF);
  }
#line 16877 "parser.c" /* yacc.c:1646  */
    break;

  case 1335:
#line 9707 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_if ((yyvsp[(-1) - (5)]), (yyvsp[-3]), (yyvsp[0]));
  }
#line 16885 "parser.c" /* yacc.c:1646  */
    break;

  case 1336:
#line 9711 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_if ((yyvsp[(-1) - (3)]), NULL, (yyvsp[0]));
  }
#line 16893 "parser.c" /* yacc.c:1646  */
    break;

  case 1337:
#line 9715 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_if ((yyvsp[(-1) - (2)]), (yyvsp[0]), NULL);
  }
#line 16901 "parser.c" /* yacc.c:1646  */
    break;

  case 1338:
#line 9721 "parser.y" /* yacc.c:1646  */
    {
	cb_save_cond ();
  }
#line 16909 "parser.c" /* yacc.c:1646  */
    break;

  case 1339:
#line 9725 "parser.y" /* yacc.c:1646  */
    {
	cb_save_cond ();
  }
#line 16917 "parser.c" /* yacc.c:1646  */
    break;

  case 1340:
#line 9731 "parser.y" /* yacc.c:1646  */
    {
	  cb_true_side ();
  }
#line 16925 "parser.c" /* yacc.c:1646  */
    break;

  case 1341:
#line 9737 "parser.y" /* yacc.c:1646  */
    {
	  cb_false_side ();
  }
#line 16933 "parser.c" /* yacc.c:1646  */
    break;

  case 1342:
#line 9744 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-4) - (0)]), IF);
	cb_terminate_cond ();
  }
#line 16942 "parser.c" /* yacc.c:1646  */
    break;

  case 1343:
#line 9749 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-4) - (1)]), IF);
	cb_terminate_cond ();
  }
#line 16951 "parser.c" /* yacc.c:1646  */
    break;

  case 1344:
#line 9760 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("INITIALIZE", 0);
  }
#line 16959 "parser.c" /* yacc.c:1646  */
    break;

  case 1346:
#line 9769 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_initialize ((yyvsp[-4]), (yyvsp[-3]), (yyvsp[-2]), (yyvsp[-1]), (yyvsp[0]));
  }
#line 16967 "parser.c" /* yacc.c:1646  */
    break;

  case 1347:
#line 9775 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 16973 "parser.c" /* yacc.c:1646  */
    break;

  case 1348:
#line 9776 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_true; }
#line 16979 "parser.c" /* yacc.c:1646  */
    break;

  case 1349:
#line 9780 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 16985 "parser.c" /* yacc.c:1646  */
    break;

  case 1350:
#line 9781 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_true; }
#line 16991 "parser.c" /* yacc.c:1646  */
    break;

  case 1351:
#line 9782 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[-2]); }
#line 16997 "parser.c" /* yacc.c:1646  */
    break;

  case 1352:
#line 9787 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 17005 "parser.c" /* yacc.c:1646  */
    break;

  case 1353:
#line 9791 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 17013 "parser.c" /* yacc.c:1646  */
    break;

  case 1354:
#line 9798 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 17021 "parser.c" /* yacc.c:1646  */
    break;

  case 1355:
#line 9803 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_append ((yyvsp[-1]), (yyvsp[0]));
  }
#line 17029 "parser.c" /* yacc.c:1646  */
    break;

  case 1356:
#line 9810 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_PAIR ((yyvsp[-3]), (yyvsp[0]));
  }
#line 17037 "parser.c" /* yacc.c:1646  */
    break;

  case 1357:
#line 9816 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (CB_CATEGORY_ALPHABETIC); }
#line 17043 "parser.c" /* yacc.c:1646  */
    break;

  case 1358:
#line 9817 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (CB_CATEGORY_ALPHANUMERIC); }
#line 17049 "parser.c" /* yacc.c:1646  */
    break;

  case 1359:
#line 9818 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (CB_CATEGORY_NUMERIC); }
#line 17055 "parser.c" /* yacc.c:1646  */
    break;

  case 1360:
#line 9819 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (CB_CATEGORY_ALPHANUMERIC_EDITED); }
#line 17061 "parser.c" /* yacc.c:1646  */
    break;

  case 1361:
#line 9820 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (CB_CATEGORY_NUMERIC_EDITED); }
#line 17067 "parser.c" /* yacc.c:1646  */
    break;

  case 1362:
#line 9821 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (CB_CATEGORY_NATIONAL); }
#line 17073 "parser.c" /* yacc.c:1646  */
    break;

  case 1363:
#line 9822 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (CB_CATEGORY_NATIONAL_EDITED); }
#line 17079 "parser.c" /* yacc.c:1646  */
    break;

  case 1364:
#line 9834 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 17087 "parser.c" /* yacc.c:1646  */
    break;

  case 1365:
#line 9838 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_true;
  }
#line 17095 "parser.c" /* yacc.c:1646  */
    break;

  case 1366:
#line 9847 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("INITIATE", 0);
	CB_PENDING("INITIATE");
  }
#line 17104 "parser.c" /* yacc.c:1646  */
    break;

  case 1368:
#line 9856 "parser.y" /* yacc.c:1646  */
    {
	begin_implicit_statement ();
	if ((yyvsp[0]) != cb_error_node) {
	}
  }
#line 17114 "parser.c" /* yacc.c:1646  */
    break;

  case 1369:
#line 9862 "parser.y" /* yacc.c:1646  */
    {
	begin_implicit_statement ();
	if ((yyvsp[0]) != cb_error_node) {
	}
  }
#line 17124 "parser.c" /* yacc.c:1646  */
    break;

  case 1370:
#line 9873 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("INSPECT", 0);
	inspect_keyword = 0;
  }
#line 17133 "parser.c" /* yacc.c:1646  */
    break;

  case 1380:
#line 9901 "parser.y" /* yacc.c:1646  */
    {
	previous_tallying_phrase = NO_PHRASE;
	cb_init_tallying ();
  }
#line 17142 "parser.c" /* yacc.c:1646  */
    break;

  case 1381:
#line 9906 "parser.y" /* yacc.c:1646  */
    {
	if (!(previous_tallying_phrase == CHARACTERS_PHRASE
	      || previous_tallying_phrase == VALUE_REGION_PHRASE)) {
		cb_error (_("TALLYING clause is incomplete"));
	} else {
		cb_emit_inspect ((yyvsp[-3]), (yyvsp[0]), TALLYING_CLAUSE);
	}

	(yyval) = (yyvsp[-3]);
  }
#line 17157 "parser.c" /* yacc.c:1646  */
    break;

  case 1382:
#line 9922 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_inspect ((yyvsp[-2]), (yyvsp[0]), REPLACING_CLAUSE);
	inspect_keyword = 0;
  }
#line 17166 "parser.c" /* yacc.c:1646  */
    break;

  case 1383:
#line 9932 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		x;
	x = cb_build_converting ((yyvsp[-3]), (yyvsp[-1]), (yyvsp[0]));
	cb_emit_inspect ((yyvsp[-5]), x, CONVERTING_CLAUSE);
  }
#line 17176 "parser.c" /* yacc.c:1646  */
    break;

  case 1384:
#line 9941 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 17184 "parser.c" /* yacc.c:1646  */
    break;

  case 1385:
#line 9945 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_append ((yyvsp[-1]), (yyvsp[0]));
  }
#line 17192 "parser.c" /* yacc.c:1646  */
    break;

  case 1386:
#line 9952 "parser.y" /* yacc.c:1646  */
    {
	check_preceding_tallying_phrases (FOR_PHRASE);
	(yyval) = cb_build_tallying_data ((yyvsp[-1]));
  }
#line 17201 "parser.c" /* yacc.c:1646  */
    break;

  case 1387:
#line 9957 "parser.y" /* yacc.c:1646  */
    {
	check_preceding_tallying_phrases (CHARACTERS_PHRASE);
	(yyval) = cb_build_tallying_characters ((yyvsp[0]));
  }
#line 17210 "parser.c" /* yacc.c:1646  */
    break;

  case 1388:
#line 9962 "parser.y" /* yacc.c:1646  */
    {
	check_preceding_tallying_phrases (ALL_LEADING_TRAILING_PHRASES);
	(yyval) = cb_build_tallying_all ();
  }
#line 17219 "parser.c" /* yacc.c:1646  */
    break;

  case 1389:
#line 9967 "parser.y" /* yacc.c:1646  */
    {
	check_preceding_tallying_phrases (ALL_LEADING_TRAILING_PHRASES);
	(yyval) = cb_build_tallying_leading ();
  }
#line 17228 "parser.c" /* yacc.c:1646  */
    break;

  case 1390:
#line 9972 "parser.y" /* yacc.c:1646  */
    {
	check_preceding_tallying_phrases (ALL_LEADING_TRAILING_PHRASES);
	(yyval) = cb_build_tallying_trailing ();
  }
#line 17237 "parser.c" /* yacc.c:1646  */
    break;

  case 1391:
#line 9977 "parser.y" /* yacc.c:1646  */
    {
	check_preceding_tallying_phrases (VALUE_REGION_PHRASE);
	(yyval) = cb_build_tallying_value ((yyvsp[-1]), (yyvsp[0]));
  }
#line 17246 "parser.c" /* yacc.c:1646  */
    break;

  case 1392:
#line 9984 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 17252 "parser.c" /* yacc.c:1646  */
    break;

  case 1393:
#line 9985 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_append ((yyvsp[-1]), (yyvsp[0])); }
#line 17258 "parser.c" /* yacc.c:1646  */
    break;

  case 1394:
#line 9990 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_replacing_characters ((yyvsp[-1]), (yyvsp[0]));
	inspect_keyword = 0;
  }
#line 17267 "parser.c" /* yacc.c:1646  */
    break;

  case 1395:
#line 9995 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 17275 "parser.c" /* yacc.c:1646  */
    break;

  case 1397:
#line 10002 "parser.y" /* yacc.c:1646  */
    { inspect_keyword = 1; }
#line 17281 "parser.c" /* yacc.c:1646  */
    break;

  case 1398:
#line 10003 "parser.y" /* yacc.c:1646  */
    { inspect_keyword = 2; }
#line 17287 "parser.c" /* yacc.c:1646  */
    break;

  case 1399:
#line 10004 "parser.y" /* yacc.c:1646  */
    { inspect_keyword = 3; }
#line 17293 "parser.c" /* yacc.c:1646  */
    break;

  case 1400:
#line 10005 "parser.y" /* yacc.c:1646  */
    { inspect_keyword = 4; }
#line 17299 "parser.c" /* yacc.c:1646  */
    break;

  case 1401:
#line 10010 "parser.y" /* yacc.c:1646  */
    {
	switch (inspect_keyword) {
		case 1:
			(yyval) = cb_build_replacing_all ((yyvsp[-3]), (yyvsp[-1]), (yyvsp[0]));
			break;
		case 2:
			(yyval) = cb_build_replacing_leading ((yyvsp[-3]), (yyvsp[-1]), (yyvsp[0]));
			break;
		case 3:
			(yyval) = cb_build_replacing_first ((yyvsp[-3]), (yyvsp[-1]), (yyvsp[0]));
			break;
		case 4:
			(yyval) = cb_build_replacing_trailing ((yyvsp[-3]), (yyvsp[-1]), (yyvsp[0]));
			break;
		default:
			cb_error_x (CB_TREE (current_statement),
				    _("INSPECT missing ALL/FIRST/LEADING/TRAILING"));
			(yyval) = cb_build_replacing_all ((yyvsp[-3]), (yyvsp[-1]), (yyvsp[0]));
			break;
	}
  }
#line 17325 "parser.c" /* yacc.c:1646  */
    break;

  case 1402:
#line 10037 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_inspect_region_start ();
  }
#line 17333 "parser.c" /* yacc.c:1646  */
    break;

  case 1403:
#line 10041 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add (cb_build_inspect_region_start (), (yyvsp[0]));
  }
#line 17341 "parser.c" /* yacc.c:1646  */
    break;

  case 1404:
#line 10045 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add (cb_build_inspect_region_start (), (yyvsp[0]));
  }
#line 17349 "parser.c" /* yacc.c:1646  */
    break;

  case 1405:
#line 10049 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add (cb_list_add (cb_build_inspect_region_start (), (yyvsp[-1])), (yyvsp[0]));
  }
#line 17357 "parser.c" /* yacc.c:1646  */
    break;

  case 1406:
#line 10053 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add (cb_list_add (cb_build_inspect_region_start (), (yyvsp[-1])), (yyvsp[0]));
  }
#line 17365 "parser.c" /* yacc.c:1646  */
    break;

  case 1407:
#line 10060 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_FUNCALL_1 ("cob_inspect_before", (yyvsp[0]));
  }
#line 17373 "parser.c" /* yacc.c:1646  */
    break;

  case 1408:
#line 10067 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_FUNCALL_1 ("cob_inspect_after", (yyvsp[0]));
  }
#line 17381 "parser.c" /* yacc.c:1646  */
    break;

  case 1409:
#line 10076 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("MERGE", 0);
	current_statement->flag_merge = 1;
  }
#line 17390 "parser.c" /* yacc.c:1646  */
    break;

  case 1411:
#line 10088 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("MOVE", 0);
  }
#line 17398 "parser.c" /* yacc.c:1646  */
    break;

  case 1413:
#line 10096 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_move ((yyvsp[-2]), (yyvsp[0]));
  }
#line 17406 "parser.c" /* yacc.c:1646  */
    break;

  case 1414:
#line 10100 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_move_corresponding ((yyvsp[-2]), (yyvsp[0]));
  }
#line 17414 "parser.c" /* yacc.c:1646  */
    break;

  case 1415:
#line 10110 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("MULTIPLY", TERM_MULTIPLY);
  }
#line 17422 "parser.c" /* yacc.c:1646  */
    break;

  case 1417:
#line 10119 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_arithmetic ((yyvsp[-1]), '*', (yyvsp[-3]));
  }
#line 17430 "parser.c" /* yacc.c:1646  */
    break;

  case 1418:
#line 10123 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_arithmetic ((yyvsp[-1]), 0, cb_build_binary_op ((yyvsp[-5]), '*', (yyvsp[-3])));
  }
#line 17438 "parser.c" /* yacc.c:1646  */
    break;

  case 1419:
#line 10130 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), MULTIPLY);
  }
#line 17446 "parser.c" /* yacc.c:1646  */
    break;

  case 1420:
#line 10134 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), MULTIPLY);
  }
#line 17454 "parser.c" /* yacc.c:1646  */
    break;

  case 1421:
#line 10144 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("OPEN", 0);
  }
#line 17462 "parser.c" /* yacc.c:1646  */
    break;

  case 1425:
#line 10157 "parser.y" /* yacc.c:1646  */
    {
	cb_tree l;
	cb_tree x;

	if ((yyvsp[-3]) && (yyvsp[0])) {
		cb_error_x (CB_TREE (current_statement),
			    _("%s and %s are mutually exclusive"), "SHARING", _("LOCK clauses"));
	}
	if ((yyvsp[0])) {
		x = (yyvsp[0]);
	} else {
		x = (yyvsp[-3]);
	}

	for (l = (yyvsp[-1]); l; l = CB_CHAIN (l)) {
		if (CB_VALID_TREE (CB_VALUE (l))) {
			begin_implicit_statement ();
			cb_emit_open (CB_VALUE (l), (yyvsp[-4]), x);
		}
	}
  }
#line 17488 "parser.c" /* yacc.c:1646  */
    break;

  case 1426:
#line 10181 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_OPEN_INPUT); }
#line 17494 "parser.c" /* yacc.c:1646  */
    break;

  case 1427:
#line 10182 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_OPEN_OUTPUT); }
#line 17500 "parser.c" /* yacc.c:1646  */
    break;

  case 1428:
#line 10183 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_OPEN_I_O); }
#line 17506 "parser.c" /* yacc.c:1646  */
    break;

  case 1429:
#line 10184 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_OPEN_EXTEND); }
#line 17512 "parser.c" /* yacc.c:1646  */
    break;

  case 1430:
#line 10188 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 17518 "parser.c" /* yacc.c:1646  */
    break;

  case 1431:
#line 10189 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 17524 "parser.c" /* yacc.c:1646  */
    break;

  case 1432:
#line 10193 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 17530 "parser.c" /* yacc.c:1646  */
    break;

  case 1433:
#line 10194 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 17536 "parser.c" /* yacc.c:1646  */
    break;

  case 1434:
#line 10195 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_LOCK_OPEN_EXCLUSIVE); }
#line 17542 "parser.c" /* yacc.c:1646  */
    break;

  case 1435:
#line 10197 "parser.y" /* yacc.c:1646  */
    {
	(void)cb_verify (CB_OBSOLETE, "REVERSED");
	(yyval) = NULL;
  }
#line 17551 "parser.c" /* yacc.c:1646  */
    break;

  case 1436:
#line 10208 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("PERFORM", TERM_PERFORM);
	/* Turn off field debug - PERFORM is special */
	save_debug = start_debug;
	start_debug = 0;
	cobc_cs_check = CB_CS_PERFORM;
  }
#line 17563 "parser.c" /* yacc.c:1646  */
    break;

  case 1438:
#line 10223 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_perform ((yyvsp[0]), (yyvsp[-2]), (yyvsp[-3]), (yyvsp[-1]));
	start_debug = save_debug;
	cobc_cs_check = 0;
  }
#line 17573 "parser.c" /* yacc.c:1646  */
    break;

  case 1439:
#line 10231 "parser.y" /* yacc.c:1646  */
    {
	CB_ADD_TO_CHAIN ((yyvsp[-1]), perform_stack);
	/* Restore field debug before inline statements */
	start_debug = save_debug;
	cobc_cs_check = 0;
  }
#line 17584 "parser.c" /* yacc.c:1646  */
    break;

  case 1440:
#line 10238 "parser.y" /* yacc.c:1646  */
    {
	perform_stack = CB_CHAIN (perform_stack);
	cb_emit_perform ((yyvsp[-4]), (yyvsp[-1]), (yyvsp[-5]), (yyvsp[-3]));
  }
#line 17593 "parser.c" /* yacc.c:1646  */
    break;

  case 1441:
#line 10246 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_perform ((yyvsp[-2]), NULL, (yyvsp[-3]), (yyvsp[-1]));
	start_debug = save_debug;
	cobc_cs_check = 0;
  }
#line 17603 "parser.c" /* yacc.c:1646  */
    break;

  case 1442:
#line 10255 "parser.y" /* yacc.c:1646  */
    {
	if (cb_relaxed_syntax_checks) {
		TERMINATOR_WARNING ((yyvsp[(-4) - (0)]), PERFORM);
	} else {
		TERMINATOR_ERROR ((yyvsp[(-4) - (0)]), PERFORM);
	}
  }
#line 17615 "parser.c" /* yacc.c:1646  */
    break;

  case 1443:
#line 10263 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-4) - (1)]), PERFORM);
  }
#line 17623 "parser.c" /* yacc.c:1646  */
    break;

  case 1444:
#line 10270 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), PERFORM);
  }
#line 17631 "parser.c" /* yacc.c:1646  */
    break;

  case 1445:
#line 10274 "parser.y" /* yacc.c:1646  */
    {
	if (cb_relaxed_syntax_checks) {
		TERMINATOR_WARNING ((yyvsp[(-2) - (1)]), PERFORM);
	} else {
		TERMINATOR_ERROR ((yyvsp[(-2) - (1)]), PERFORM);
	}
	/* Put the dot token back into the stack for reparse */
	cb_unput_dot ();
  }
#line 17645 "parser.c" /* yacc.c:1646  */
    break;

  case 1446:
#line 10287 "parser.y" /* yacc.c:1646  */
    {
	/* Return from $1 */
	CB_REFERENCE ((yyvsp[0]))->length = cb_true;
	CB_REFERENCE ((yyvsp[0]))->flag_decl_ok = 1;
	(yyval) = CB_BUILD_PAIR ((yyvsp[0]), (yyvsp[0]));
  }
#line 17656 "parser.c" /* yacc.c:1646  */
    break;

  case 1447:
#line 10294 "parser.y" /* yacc.c:1646  */
    {
	/* Return from $3 */
	CB_REFERENCE ((yyvsp[0]))->length = cb_true;
	CB_REFERENCE ((yyvsp[-2]))->flag_decl_ok = 1;
	CB_REFERENCE ((yyvsp[0]))->flag_decl_ok = 1;
	(yyval) = CB_BUILD_PAIR ((yyvsp[-2]), (yyvsp[0]));
  }
#line 17668 "parser.c" /* yacc.c:1646  */
    break;

  case 1448:
#line 10305 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_perform_once (NULL);
  }
#line 17676 "parser.c" /* yacc.c:1646  */
    break;

  case 1449:
#line 10309 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_perform_times ((yyvsp[-1]));
	current_program->loop_counter++;
  }
#line 17685 "parser.c" /* yacc.c:1646  */
    break;

  case 1450:
#line 10314 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_perform_forever (NULL);
  }
#line 17693 "parser.c" /* yacc.c:1646  */
    break;

  case 1451:
#line 10318 "parser.y" /* yacc.c:1646  */
    {
	cb_tree varying;

	if (!(yyvsp[0])) {
		(yyval) = cb_build_perform_forever (NULL);
	} else {
		if ((yyvsp[-2]) == CB_AFTER)
			cb_build_perform_after_until();
		varying = CB_LIST_INIT (cb_build_perform_varying (NULL, NULL, NULL, (yyvsp[0])));
		(yyval) = cb_build_perform_until ((yyvsp[-2]), varying);
	}
  }
#line 17710 "parser.c" /* yacc.c:1646  */
    break;

  case 1452:
#line 10331 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_perform_until ((yyvsp[-2]), (yyvsp[0]));
  }
#line 17718 "parser.c" /* yacc.c:1646  */
    break;

  case 1453:
#line 10337 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_BEFORE; }
#line 17724 "parser.c" /* yacc.c:1646  */
    break;

  case 1454:
#line 10338 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 17730 "parser.c" /* yacc.c:1646  */
    break;

  case 1455:
#line 10342 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 17736 "parser.c" /* yacc.c:1646  */
    break;

  case 1456:
#line 10343 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 17742 "parser.c" /* yacc.c:1646  */
    break;

  case 1457:
#line 10346 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 17748 "parser.c" /* yacc.c:1646  */
    break;

  case 1458:
#line 10348 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-2]), (yyvsp[0])); }
#line 17754 "parser.c" /* yacc.c:1646  */
    break;

  case 1459:
#line 10353 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_perform_varying ((yyvsp[-6]), (yyvsp[-4]), (yyvsp[-2]), (yyvsp[0]));
  }
#line 17762 "parser.c" /* yacc.c:1646  */
    break;

  case 1460:
#line 10362 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("PURGE", 0);
  }
#line 17770 "parser.c" /* yacc.c:1646  */
    break;

  case 1461:
#line 10366 "parser.y" /* yacc.c:1646  */
    {
  }
#line 17777 "parser.c" /* yacc.c:1646  */
    break;

  case 1462:
#line 10374 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("READ", TERM_READ);
	cobc_cs_check = CB_CS_READ;
  }
#line 17786 "parser.c" /* yacc.c:1646  */
    break;

  case 1464:
#line 10384 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;

	if (CB_VALID_TREE ((yyvsp[-6]))) {
		struct cb_file	*cf;

		cf = CB_FILE(cb_ref ((yyvsp[-6])));
		if ((yyvsp[-2]) && (cf->lock_mode & COB_LOCK_AUTOMATIC)) {
			cb_error_x (CB_TREE (current_statement),
				    _("LOCK clause invalid with file LOCK AUTOMATIC"));
		} else if ((yyvsp[-1]) &&
		      (cf->organization != COB_ORG_RELATIVE &&
		       cf->organization != COB_ORG_INDEXED)) {
			cb_error_x (CB_TREE (current_statement),
				    _("KEY clause invalid with this file type"));
		} else if (current_statement->handler_type == INVALID_KEY_HANDLER &&
			   (cf->organization != COB_ORG_RELATIVE &&
			    cf->organization != COB_ORG_INDEXED)) {
			cb_error_x (CB_TREE (current_statement),
				    _("INVALID KEY clause invalid with this file type"));
		} else {
			cb_emit_read ((yyvsp[-6]), (yyvsp[-5]), (yyvsp[-3]), (yyvsp[-1]), (yyvsp[-2]));
		}
	}
  }
#line 17816 "parser.c" /* yacc.c:1646  */
    break;

  case 1465:
#line 10412 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 17822 "parser.c" /* yacc.c:1646  */
    break;

  case 1466:
#line 10413 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 17828 "parser.c" /* yacc.c:1646  */
    break;

  case 1467:
#line 10418 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 17836 "parser.c" /* yacc.c:1646  */
    break;

  case 1468:
#line 10422 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int3;
  }
#line 17844 "parser.c" /* yacc.c:1646  */
    break;

  case 1469:
#line 10426 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 17852 "parser.c" /* yacc.c:1646  */
    break;

  case 1470:
#line 10430 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 17860 "parser.c" /* yacc.c:1646  */
    break;

  case 1473:
#line 10442 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("ADVANCING ON LOCK");
  }
#line 17868 "parser.c" /* yacc.c:1646  */
    break;

  case 1477:
#line 10455 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("RETRY");
	cobc_cs_check = 0;
  }
#line 17877 "parser.c" /* yacc.c:1646  */
    break;

  case 1483:
#line 10475 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 17885 "parser.c" /* yacc.c:1646  */
    break;

  case 1484:
#line 10479 "parser.y" /* yacc.c:1646  */
    {
   (yyval) = cb_int5;
  }
#line 17893 "parser.c" /* yacc.c:1646  */
    break;

  case 1485:
#line 10483 "parser.y" /* yacc.c:1646  */
    {
	/* TO-DO: Merge with RETRY phrase */
	(yyval) = cb_int4;
  }
#line 17902 "parser.c" /* yacc.c:1646  */
    break;

  case 1486:
#line 10490 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 17908 "parser.c" /* yacc.c:1646  */
    break;

  case 1487:
#line 10491 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 17914 "parser.c" /* yacc.c:1646  */
    break;

  case 1490:
#line 10501 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), READ);
  }
#line 17922 "parser.c" /* yacc.c:1646  */
    break;

  case 1491:
#line 10505 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), READ);
  }
#line 17930 "parser.c" /* yacc.c:1646  */
    break;

  case 1492:
#line 10515 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("READY TRACE", 0);
	cb_emit_ready_trace ();
  }
#line 17939 "parser.c" /* yacc.c:1646  */
    break;

  case 1493:
#line 10525 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("RECEIVE", TERM_RECEIVE);
  }
#line 17947 "parser.c" /* yacc.c:1646  */
    break;

  case 1507:
#line 10568 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), RECEIVE);
  }
#line 17955 "parser.c" /* yacc.c:1646  */
    break;

  case 1508:
#line 10572 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), RECEIVE);
  }
#line 17963 "parser.c" /* yacc.c:1646  */
    break;

  case 1509:
#line 10581 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("RELEASE", 0);
  }
#line 17971 "parser.c" /* yacc.c:1646  */
    break;

  case 1511:
#line 10589 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_release ((yyvsp[-1]), (yyvsp[0]));
  }
#line 17979 "parser.c" /* yacc.c:1646  */
    break;

  case 1512:
#line 10599 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("RESET TRACE", 0);
	cb_emit_reset_trace ();
  }
#line 17988 "parser.c" /* yacc.c:1646  */
    break;

  case 1513:
#line 10609 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("RETURN", TERM_RETURN);
  }
#line 17996 "parser.c" /* yacc.c:1646  */
    break;

  case 1515:
#line 10618 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_return ((yyvsp[-3]), (yyvsp[-1]));
  }
#line 18004 "parser.c" /* yacc.c:1646  */
    break;

  case 1516:
#line 10625 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), RETURN);
  }
#line 18012 "parser.c" /* yacc.c:1646  */
    break;

  case 1517:
#line 10629 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), RETURN);
  }
#line 18020 "parser.c" /* yacc.c:1646  */
    break;

  case 1518:
#line 10639 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("REWRITE", TERM_REWRITE);
	/* Special in debugging mode */
	save_debug = start_debug;
	start_debug = 0;
  }
#line 18031 "parser.c" /* yacc.c:1646  */
    break;

  case 1520:
#line 10651 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_rewrite ((yyvsp[-4]), (yyvsp[-3]), (yyvsp[-1]));
	start_debug = save_debug;
  }
#line 18040 "parser.c" /* yacc.c:1646  */
    break;

  case 1521:
#line 10659 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 18048 "parser.c" /* yacc.c:1646  */
    break;

  case 1523:
#line 10667 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 18056 "parser.c" /* yacc.c:1646  */
    break;

  case 1524:
#line 10671 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int2;
  }
#line 18064 "parser.c" /* yacc.c:1646  */
    break;

  case 1525:
#line 10678 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), REWRITE);
  }
#line 18072 "parser.c" /* yacc.c:1646  */
    break;

  case 1526:
#line 10682 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), REWRITE);
  }
#line 18080 "parser.c" /* yacc.c:1646  */
    break;

  case 1527:
#line 10692 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("ROLLBACK", 0);
	cb_emit_rollback ();
  }
#line 18089 "parser.c" /* yacc.c:1646  */
    break;

  case 1528:
#line 10703 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("SEARCH", TERM_SEARCH);
  }
#line 18097 "parser.c" /* yacc.c:1646  */
    break;

  case 1530:
#line 10712 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_search ((yyvsp[-3]), (yyvsp[-2]), (yyvsp[-1]), (yyvsp[0]));
  }
#line 18105 "parser.c" /* yacc.c:1646  */
    break;

  case 1531:
#line 10717 "parser.y" /* yacc.c:1646  */
    {
	current_statement->name = (const char *)"SEARCH ALL";
	cb_emit_search_all ((yyvsp[-4]), (yyvsp[-3]), (yyvsp[-1]), (yyvsp[0]));
  }
#line 18114 "parser.c" /* yacc.c:1646  */
    break;

  case 1532:
#line 10724 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 18120 "parser.c" /* yacc.c:1646  */
    break;

  case 1533:
#line 10725 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 18126 "parser.c" /* yacc.c:1646  */
    break;

  case 1534:
#line 10730 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 18134 "parser.c" /* yacc.c:1646  */
    break;

  case 1535:
#line 10735 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 18142 "parser.c" /* yacc.c:1646  */
    break;

  case 1536:
#line 10742 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
  }
#line 18150 "parser.c" /* yacc.c:1646  */
    break;

  case 1537:
#line 10746 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[0]), (yyvsp[-1]));
  }
#line 18158 "parser.c" /* yacc.c:1646  */
    break;

  case 1538:
#line 10754 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_if_check_break ((yyvsp[-1]), (yyvsp[0]));
  }
#line 18166 "parser.c" /* yacc.c:1646  */
    break;

  case 1539:
#line 10761 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), SEARCH);
  }
#line 18174 "parser.c" /* yacc.c:1646  */
    break;

  case 1540:
#line 10765 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), SEARCH);
  }
#line 18182 "parser.c" /* yacc.c:1646  */
    break;

  case 1541:
#line 10775 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("SEND", 0);
  }
#line 18190 "parser.c" /* yacc.c:1646  */
    break;

  case 1543:
#line 10783 "parser.y" /* yacc.c:1646  */
    {
  }
#line 18197 "parser.c" /* yacc.c:1646  */
    break;

  case 1544:
#line 10786 "parser.y" /* yacc.c:1646  */
    {
  }
#line 18204 "parser.c" /* yacc.c:1646  */
    break;

  case 1547:
#line 10797 "parser.y" /* yacc.c:1646  */
    {
  }
#line 18211 "parser.c" /* yacc.c:1646  */
    break;

  case 1554:
#line 10817 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("SET", 0);
	set_attr_val_on = 0;
	set_attr_val_off = 0;
	cobc_cs_check = CB_CS_SET;
  }
#line 18222 "parser.c" /* yacc.c:1646  */
    break;

  case 1555:
#line 10824 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
  }
#line 18230 "parser.c" /* yacc.c:1646  */
    break;

  case 1564:
#line 10841 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 18236 "parser.c" /* yacc.c:1646  */
    break;

  case 1565:
#line 10842 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int0; }
#line 18242 "parser.c" /* yacc.c:1646  */
    break;

  case 1566:
#line 10846 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int0; }
#line 18248 "parser.c" /* yacc.c:1646  */
    break;

  case 1567:
#line 10847 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 18254 "parser.c" /* yacc.c:1646  */
    break;

  case 1568:
#line 10854 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_setenv ((yyvsp[-2]), (yyvsp[0]));
  }
#line 18262 "parser.c" /* yacc.c:1646  */
    break;

  case 1569:
#line 10863 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_set_attribute ((yyvsp[-2]), set_attr_val_on, set_attr_val_off);
  }
#line 18270 "parser.c" /* yacc.c:1646  */
    break;

  case 1572:
#line 10875 "parser.y" /* yacc.c:1646  */
    {
	bit_set_attr ((yyvsp[0]), COB_SCREEN_BELL);
  }
#line 18278 "parser.c" /* yacc.c:1646  */
    break;

  case 1573:
#line 10879 "parser.y" /* yacc.c:1646  */
    {
	bit_set_attr ((yyvsp[0]), COB_SCREEN_BLINK);
  }
#line 18286 "parser.c" /* yacc.c:1646  */
    break;

  case 1574:
#line 10883 "parser.y" /* yacc.c:1646  */
    {
	bit_set_attr ((yyvsp[0]), COB_SCREEN_HIGHLIGHT);
	check_not_highlight_and_lowlight (set_attr_val_on | set_attr_val_off,
					  COB_SCREEN_HIGHLIGHT);
  }
#line 18296 "parser.c" /* yacc.c:1646  */
    break;

  case 1575:
#line 10889 "parser.y" /* yacc.c:1646  */
    {
	bit_set_attr ((yyvsp[0]), COB_SCREEN_LOWLIGHT);
	check_not_highlight_and_lowlight (set_attr_val_on | set_attr_val_off,
					  COB_SCREEN_LOWLIGHT);
  }
#line 18306 "parser.c" /* yacc.c:1646  */
    break;

  case 1576:
#line 10895 "parser.y" /* yacc.c:1646  */
    {
	bit_set_attr ((yyvsp[0]), COB_SCREEN_REVERSE);
  }
#line 18314 "parser.c" /* yacc.c:1646  */
    break;

  case 1577:
#line 10899 "parser.y" /* yacc.c:1646  */
    {
	bit_set_attr ((yyvsp[0]), COB_SCREEN_UNDERLINE);
  }
#line 18322 "parser.c" /* yacc.c:1646  */
    break;

  case 1578:
#line 10903 "parser.y" /* yacc.c:1646  */
    {
	bit_set_attr ((yyvsp[0]), COB_SCREEN_LEFTLINE);
  }
#line 18330 "parser.c" /* yacc.c:1646  */
    break;

  case 1579:
#line 10907 "parser.y" /* yacc.c:1646  */
    {
	bit_set_attr ((yyvsp[0]), COB_SCREEN_OVERLINE);
  }
#line 18338 "parser.c" /* yacc.c:1646  */
    break;

  case 1580:
#line 10916 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_set_to ((yyvsp[-3]), cb_build_ppointer ((yyvsp[0])));
  }
#line 18346 "parser.c" /* yacc.c:1646  */
    break;

  case 1581:
#line 10920 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_set_to ((yyvsp[-2]), (yyvsp[0]));
  }
#line 18354 "parser.c" /* yacc.c:1646  */
    break;

  case 1582:
#line 10924 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_move (cb_build_length ((yyvsp[0])), (yyvsp[-4]));
  }
#line 18362 "parser.c" /* yacc.c:1646  */
    break;

  case 1583:
#line 10933 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_set_up_down ((yyvsp[-3]), (yyvsp[-2]), (yyvsp[0]));
  }
#line 18370 "parser.c" /* yacc.c:1646  */
    break;

  case 1586:
#line 10947 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_set_on_off ((yyvsp[-2]), (yyvsp[0]));
  }
#line 18378 "parser.c" /* yacc.c:1646  */
    break;

  case 1589:
#line 10961 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_set_true ((yyvsp[-2]));
  }
#line 18386 "parser.c" /* yacc.c:1646  */
    break;

  case 1590:
#line 10965 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_set_false ((yyvsp[-2]));
  }
#line 18394 "parser.c" /* yacc.c:1646  */
    break;

  case 1591:
#line 10974 "parser.y" /* yacc.c:1646  */
    {
	  cb_emit_set_last_exception_to_off ();
  }
#line 18402 "parser.c" /* yacc.c:1646  */
    break;

  case 1592:
#line 10983 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_set_thread_priority ((yyvsp[-3]), (yyvsp[0]));
	CB_PENDING ("THREAD");
  }
#line 18411 "parser.c" /* yacc.c:1646  */
    break;

  case 1593:
#line 10994 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("SORT", 0);
  }
#line 18419 "parser.c" /* yacc.c:1646  */
    break;

  case 1595:
#line 11002 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		x;

	x = cb_ref ((yyvsp[-3]));
	if (CB_VALID_TREE (x)) {
		if (CB_INVALID_TREE ((yyvsp[-2]))) {
			if (CB_FILE_P (x)) {
				cb_error (_("file sort requires KEY phrase"));
			} else {
				/* FIXME: use key definition from OCCURS */
				cb_error (_("%s is not implemented"), _("table SORT without keys"));
			}
			(yyval) = NULL;
		} else {
			cb_emit_sort_init ((yyvsp[-3]), (yyvsp[-2]), (yyvsp[0]));
			(yyval)= (yyvsp[-3]);
		}
	} else {
		(yyval) = NULL;
	}
  }
#line 18445 "parser.c" /* yacc.c:1646  */
    break;

  case 1596:
#line 11024 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[-2]) && CB_VALID_TREE ((yyvsp[-6]))) {
		cb_emit_sort_finish ((yyvsp[-6]));
	}
  }
#line 18455 "parser.c" /* yacc.c:1646  */
    break;

  case 1597:
#line 11033 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 18463 "parser.c" /* yacc.c:1646  */
    break;

  case 1598:
#line 11038 "parser.y" /* yacc.c:1646  */
    {
	cb_tree l;
	cb_tree lparm;

	if ((yyvsp[0]) == NULL) {
		l = CB_LIST_INIT (NULL);
	} else {
		l = (yyvsp[0]);
	}
	lparm = l;
	for (; l; l = CB_CHAIN (l)) {
		CB_PURPOSE (l) = (yyvsp[-2]);
	}
	(yyval) = cb_list_append ((yyvsp[-4]), lparm);
  }
#line 18483 "parser.c" /* yacc.c:1646  */
    break;

  case 1599:
#line 11056 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 18489 "parser.c" /* yacc.c:1646  */
    break;

  case 1600:
#line 11057 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0])); }
#line 18495 "parser.c" /* yacc.c:1646  */
    break;

  case 1602:
#line 11062 "parser.y" /* yacc.c:1646  */
    {
	/* The OC sort is a stable sort. ie. Dups are per default in order */
	/* Therefore nothing to do here */
  }
#line 18504 "parser.c" /* yacc.c:1646  */
    break;

  case 1603:
#line 11069 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_null; }
#line 18510 "parser.c" /* yacc.c:1646  */
    break;

  case 1604:
#line 11070 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_ref ((yyvsp[0])); }
#line 18516 "parser.c" /* yacc.c:1646  */
    break;

  case 1605:
#line 11075 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0]) && CB_FILE_P (cb_ref ((yyvsp[0])))) {
		cb_error (_("file sort requires USING or INPUT PROCEDURE"));
	}
  }
#line 18526 "parser.c" /* yacc.c:1646  */
    break;

  case 1606:
#line 11081 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[-2])) {
		if (!CB_FILE_P (cb_ref ((yyvsp[-2])))) {
			cb_error (_("USING invalid with table SORT"));
		} else {
			cb_emit_sort_using ((yyvsp[-2]), (yyvsp[0]));
		}
	}
  }
#line 18540 "parser.c" /* yacc.c:1646  */
    break;

  case 1607:
#line 11091 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[-4])) {
		if (!CB_FILE_P (cb_ref ((yyvsp[-4])))) {
			cb_error (_("INPUT PROCEDURE invalid with table SORT"));
		} else if (current_statement->flag_merge) {
			cb_error (_("INPUT PROCEDURE invalid with MERGE"));
		} else {
			cb_emit_sort_input ((yyvsp[0]));
		}
	}
	cobc_cs_check = 0;
  }
#line 18557 "parser.c" /* yacc.c:1646  */
    break;

  case 1608:
#line 11107 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[(-1) - (0)]) && CB_FILE_P (cb_ref ((yyvsp[(-1) - (0)])))) {
		cb_error (_("file sort requires GIVING or OUTPUT PROCEDURE"));
	}
  }
#line 18567 "parser.c" /* yacc.c:1646  */
    break;

  case 1609:
#line 11113 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[(-1) - (2)])) {
		if (!CB_FILE_P (cb_ref ((yyvsp[(-1) - (2)])))) {
			cb_error (_("GIVING invalid with table SORT"));
		} else {
			cb_emit_sort_giving ((yyvsp[(-1) - (2)]), (yyvsp[0]));
		}
	}
  }
#line 18581 "parser.c" /* yacc.c:1646  */
    break;

  case 1610:
#line 11123 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[(-1) - (4)])) {
		if (!CB_FILE_P (cb_ref ((yyvsp[(-1) - (4)])))) {
			cb_error (_("OUTPUT PROCEDURE invalid with table SORT"));
		} else {
			cb_emit_sort_output ((yyvsp[0]));
		}
	}
	cobc_cs_check = 0;
  }
#line 18596 "parser.c" /* yacc.c:1646  */
    break;

  case 1611:
#line 11140 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("START", TERM_START);
	start_tree = cb_int (COB_EQ);
  }
#line 18605 "parser.c" /* yacc.c:1646  */
    break;

  case 1613:
#line 11150 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[-1]) && !(yyvsp[-2])) {
		cb_error_x (CB_TREE (current_statement),
			    _("SIZE/LENGTH invalid here"));
	} else {
		cb_emit_start ((yyvsp[-3]), start_tree, (yyvsp[-2]), (yyvsp[-1]));
	}
  }
#line 18618 "parser.c" /* yacc.c:1646  */
    break;

  case 1614:
#line 11162 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 18626 "parser.c" /* yacc.c:1646  */
    break;

  case 1615:
#line 11166 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 18634 "parser.c" /* yacc.c:1646  */
    break;

  case 1616:
#line 11173 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 18642 "parser.c" /* yacc.c:1646  */
    break;

  case 1617:
#line 11177 "parser.y" /* yacc.c:1646  */
    {
	start_tree = (yyvsp[-1]);
	(yyval) = (yyvsp[0]);
  }
#line 18651 "parser.c" /* yacc.c:1646  */
    break;

  case 1618:
#line 11182 "parser.y" /* yacc.c:1646  */
    {
	start_tree = cb_int (COB_FI);
	(yyval) = NULL;
  }
#line 18660 "parser.c" /* yacc.c:1646  */
    break;

  case 1619:
#line 11187 "parser.y" /* yacc.c:1646  */
    {
	start_tree = cb_int (COB_LA);
	(yyval) = NULL;
  }
#line 18669 "parser.c" /* yacc.c:1646  */
    break;

  case 1620:
#line 11194 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_EQ); }
#line 18675 "parser.c" /* yacc.c:1646  */
    break;

  case 1621:
#line 11195 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int ((yyvsp[-1]) ? COB_LE : COB_GT); }
#line 18681 "parser.c" /* yacc.c:1646  */
    break;

  case 1622:
#line 11196 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int ((yyvsp[-1]) ? COB_GE : COB_LT); }
#line 18687 "parser.c" /* yacc.c:1646  */
    break;

  case 1623:
#line 11197 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int ((yyvsp[-1]) ? COB_LT : COB_GE); }
#line 18693 "parser.c" /* yacc.c:1646  */
    break;

  case 1624:
#line 11198 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int ((yyvsp[-1]) ? COB_GT : COB_LE); }
#line 18699 "parser.c" /* yacc.c:1646  */
    break;

  case 1625:
#line 11199 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_NE); }
#line 18705 "parser.c" /* yacc.c:1646  */
    break;

  case 1626:
#line 11204 "parser.y" /* yacc.c:1646  */
    {
	cb_error_x (CB_TREE (current_statement),
		    _("NOT EQUAL condition not allowed on START statement"));
  }
#line 18714 "parser.c" /* yacc.c:1646  */
    break;

  case 1629:
#line 11217 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), START);
  }
#line 18722 "parser.c" /* yacc.c:1646  */
    break;

  case 1630:
#line 11221 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), START);
  }
#line 18730 "parser.c" /* yacc.c:1646  */
    break;

  case 1631:
#line 11231 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("STOP RUN", 0);
  }
#line 18738 "parser.c" /* yacc.c:1646  */
    break;

  case 1632:
#line 11235 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_stop_run ((yyvsp[0]));
	check_unreached = 1;
	cobc_cs_check = 0;
  }
#line 18748 "parser.c" /* yacc.c:1646  */
    break;

  case 1633:
#line 11241 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("STOP", 0);
	cb_emit_display (CB_LIST_INIT ((yyvsp[0])), cb_int0, cb_int1, NULL,
			 NULL, 1, DEVICE_DISPLAY);
	cb_emit_accept (cb_null, NULL, NULL);
	cobc_cs_check = 0;
  }
#line 18760 "parser.c" /* yacc.c:1646  */
    break;

  case 1634:
#line 11249 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("STOP THREAD", 0);
	cb_emit_stop_thread ((yyvsp[0]));
	cobc_cs_check = 0;
	cb_warning_x (COBC_WARN_FILLER, (yyvsp[0]), _("%s is replaced by %s"), "STOP THREAD", "STOP RUN");
  }
#line 18771 "parser.c" /* yacc.c:1646  */
    break;

  case 1635:
#line 11259 "parser.y" /* yacc.c:1646  */
    {
	if (current_program->cb_return_code) {
		(yyval) = current_program->cb_return_code;
	} else {
		(yyval) = cb_int0;
	}
  }
#line 18783 "parser.c" /* yacc.c:1646  */
    break;

  case 1636:
#line 11267 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 18791 "parser.c" /* yacc.c:1646  */
    break;

  case 1637:
#line 11271 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 18799 "parser.c" /* yacc.c:1646  */
    break;

  case 1638:
#line 11275 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		(yyval) = (yyvsp[0]);
	} else {
		(yyval) = cb_int1;
	}
  }
#line 18811 "parser.c" /* yacc.c:1646  */
    break;

  case 1639:
#line 11283 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		(yyval) = (yyvsp[0]);
	} else {
		(yyval) = cb_int0;
	}
  }
#line 18823 "parser.c" /* yacc.c:1646  */
    break;

  case 1640:
#line 11294 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 18831 "parser.c" /* yacc.c:1646  */
    break;

  case 1641:
#line 11298 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 18839 "parser.c" /* yacc.c:1646  */
    break;

  case 1642:
#line 11305 "parser.y" /* yacc.c:1646  */
    {
	cb_verify (cb_stop_literal_statement, _("STOP literal"));
  }
#line 18847 "parser.c" /* yacc.c:1646  */
    break;

  case 1643:
#line 11309 "parser.y" /* yacc.c:1646  */
    {
	cb_verify (cb_stop_identifier_statement, _("STOP identifier"));
  }
#line 18855 "parser.c" /* yacc.c:1646  */
    break;

  case 1644:
#line 11315 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 18861 "parser.c" /* yacc.c:1646  */
    break;

  case 1645:
#line 11316 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_space; }
#line 18867 "parser.c" /* yacc.c:1646  */
    break;

  case 1646:
#line 11317 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_zero; }
#line 18873 "parser.c" /* yacc.c:1646  */
    break;

  case 1647:
#line 11318 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_quote; }
#line 18879 "parser.c" /* yacc.c:1646  */
    break;

  case 1648:
#line 11325 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("STRING", TERM_STRING);
	save_tree = NULL;
  }
#line 18888 "parser.c" /* yacc.c:1646  */
    break;

  case 1650:
#line 11335 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_string (save_tree, (yyvsp[-2]), (yyvsp[-1]));
  }
#line 18896 "parser.c" /* yacc.c:1646  */
    break;

  case 1653:
#line 11347 "parser.y" /* yacc.c:1646  */
    {
    if (!save_tree) {
		save_tree = CB_LIST_INIT ((yyvsp[-1]));
	} else {
		save_tree = cb_list_add (save_tree, (yyvsp[-1]));
	}
	if ((yyvsp[0])) {
		save_tree = cb_list_add (save_tree, (yyvsp[0]));
	}
  }
#line 18911 "parser.c" /* yacc.c:1646  */
    break;

  case 1654:
#line 11360 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 18917 "parser.c" /* yacc.c:1646  */
    break;

  case 1655:
#line 11362 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 18923 "parser.c" /* yacc.c:1646  */
    break;

  case 1656:
#line 11366 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_BUILD_PAIR (cb_int0, NULL); }
#line 18929 "parser.c" /* yacc.c:1646  */
    break;

  case 1657:
#line 11367 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_BUILD_PAIR ((yyvsp[0]), NULL); }
#line 18935 "parser.c" /* yacc.c:1646  */
    break;

  case 1658:
#line 11371 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 18941 "parser.c" /* yacc.c:1646  */
    break;

  case 1659:
#line 11372 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 18947 "parser.c" /* yacc.c:1646  */
    break;

  case 1660:
#line 11377 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), STRING);
  }
#line 18955 "parser.c" /* yacc.c:1646  */
    break;

  case 1661:
#line 11381 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), STRING);
  }
#line 18963 "parser.c" /* yacc.c:1646  */
    break;

  case 1662:
#line 11391 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("SUBTRACT", TERM_SUBTRACT);
  }
#line 18971 "parser.c" /* yacc.c:1646  */
    break;

  case 1664:
#line 11400 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_arithmetic ((yyvsp[-1]), '-', cb_build_binary_list ((yyvsp[-3]), '+'));
  }
#line 18979 "parser.c" /* yacc.c:1646  */
    break;

  case 1665:
#line 11404 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_arithmetic ((yyvsp[-1]), 0, cb_build_binary_list (CB_BUILD_CHAIN ((yyvsp[-3]), (yyvsp[-5])), '-'));
  }
#line 18987 "parser.c" /* yacc.c:1646  */
    break;

  case 1666:
#line 11408 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_corresponding (cb_build_sub, (yyvsp[-2]), (yyvsp[-4]), (yyvsp[-1]));
  }
#line 18995 "parser.c" /* yacc.c:1646  */
    break;

  case 1667:
#line 11412 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("SUBTRACT TABLE");
	cb_emit_tab_arithmetic (cb_build_sub, (yyvsp[-4]), (yyvsp[-6]), (yyvsp[-3]), (yyvsp[-2]), (yyvsp[-1]));
  }
#line 19004 "parser.c" /* yacc.c:1646  */
    break;

  case 1668:
#line 11420 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), SUBTRACT);
  }
#line 19012 "parser.c" /* yacc.c:1646  */
    break;

  case 1669:
#line 11424 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), SUBTRACT);
  }
#line 19020 "parser.c" /* yacc.c:1646  */
    break;

  case 1670:
#line 11434 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("SUPPRESS", 0);
	if (!in_declaratives) {
		cb_error_x (CB_TREE (current_statement),
			    _("SUPPRESS statement must be within DECLARATIVES"));
	}
	CB_PENDING("SUPPRESS");
  }
#line 19033 "parser.c" /* yacc.c:1646  */
    break;

  case 1673:
#line 11452 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("TERMINATE", 0);
	CB_PENDING("TERMINATE");
  }
#line 19042 "parser.c" /* yacc.c:1646  */
    break;

  case 1675:
#line 11461 "parser.y" /* yacc.c:1646  */
    {
	begin_implicit_statement ();
	if ((yyvsp[0]) != cb_error_node) {
	}
  }
#line 19052 "parser.c" /* yacc.c:1646  */
    break;

  case 1676:
#line 11467 "parser.y" /* yacc.c:1646  */
    {
	begin_implicit_statement ();
	if ((yyvsp[0]) != cb_error_node) {
	}
  }
#line 19062 "parser.c" /* yacc.c:1646  */
    break;

  case 1677:
#line 11478 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("TRANSFORM", 0);
  }
#line 19070 "parser.c" /* yacc.c:1646  */
    break;

  case 1679:
#line 11486 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		x;

	x = cb_build_converting ((yyvsp[-2]), (yyvsp[0]), cb_build_inspect_region_start ());
	cb_emit_inspect ((yyvsp[-4]), x, TRANSFORM_STATEMENT);
  }
#line 19081 "parser.c" /* yacc.c:1646  */
    break;

  case 1680:
#line 11499 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("UNLOCK", 0);
  }
#line 19089 "parser.c" /* yacc.c:1646  */
    break;

  case 1682:
#line 11507 "parser.y" /* yacc.c:1646  */
    {
	if (CB_VALID_TREE ((yyvsp[-1]))) {
		if (CB_FILE (cb_ref ((yyvsp[-1])))->organization == COB_ORG_SORT) {
			cb_error_x (CB_TREE (current_statement),
				    _("UNLOCK invalid for SORT files"));
		} else {
			cb_emit_unlock ((yyvsp[-1]));
		}
	}
  }
#line 19104 "parser.c" /* yacc.c:1646  */
    break;

  case 1683:
#line 11523 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("UNSTRING", TERM_UNSTRING);
  }
#line 19112 "parser.c" /* yacc.c:1646  */
    break;

  case 1685:
#line 11534 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_unstring ((yyvsp[-5]), (yyvsp[-4]), (yyvsp[-3]), (yyvsp[-2]), (yyvsp[-1]));
  }
#line 19120 "parser.c" /* yacc.c:1646  */
    break;

  case 1686:
#line 11540 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 19126 "parser.c" /* yacc.c:1646  */
    break;

  case 1687:
#line 11542 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 19132 "parser.c" /* yacc.c:1646  */
    break;

  case 1688:
#line 11546 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 19138 "parser.c" /* yacc.c:1646  */
    break;

  case 1689:
#line 11548 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-2]), (yyvsp[0])); }
#line 19144 "parser.c" /* yacc.c:1646  */
    break;

  case 1690:
#line 11553 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_unstring_delimited ((yyvsp[-1]), (yyvsp[0]));
  }
#line 19152 "parser.c" /* yacc.c:1646  */
    break;

  case 1691:
#line 11559 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 19158 "parser.c" /* yacc.c:1646  */
    break;

  case 1692:
#line 11561 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0])); }
#line 19164 "parser.c" /* yacc.c:1646  */
    break;

  case 1693:
#line 11566 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_unstring_into ((yyvsp[-2]), (yyvsp[-1]), (yyvsp[0]));
  }
#line 19172 "parser.c" /* yacc.c:1646  */
    break;

  case 1694:
#line 11572 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 19178 "parser.c" /* yacc.c:1646  */
    break;

  case 1695:
#line 11573 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 19184 "parser.c" /* yacc.c:1646  */
    break;

  case 1696:
#line 11577 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 19190 "parser.c" /* yacc.c:1646  */
    break;

  case 1697:
#line 11578 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 19196 "parser.c" /* yacc.c:1646  */
    break;

  case 1698:
#line 11582 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 19202 "parser.c" /* yacc.c:1646  */
    break;

  case 1699:
#line 11583 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 19208 "parser.c" /* yacc.c:1646  */
    break;

  case 1700:
#line 11588 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), UNSTRING);
  }
#line 19216 "parser.c" /* yacc.c:1646  */
    break;

  case 1701:
#line 11592 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), UNSTRING);
  }
#line 19224 "parser.c" /* yacc.c:1646  */
    break;

  case 1702:
#line 11602 "parser.y" /* yacc.c:1646  */
    {
	skip_statements = 0;
	in_debugging = 0;
  }
#line 19233 "parser.c" /* yacc.c:1646  */
    break;

  case 1709:
#line 11620 "parser.y" /* yacc.c:1646  */
    {
	if (!in_declaratives) {
		cb_error (_("USE statement must be within DECLARATIVES"));
	} else if (!current_section) {
		cb_error (_("SECTION header missing before USE statement"));
	} else {
		current_section->flag_begin = 1;
		current_section->flag_return = 1;
		current_section->flag_declarative_exit = 1;
		current_section->flag_real_label = 1;
		current_section->flag_skip_label = 0;
		CB_EXCEPTION_ENABLE (COB_EC_I_O) = 1;
		if (use_global_ind) {
			current_section->flag_global = 1;
			current_program->global_list =
				cb_list_add (current_program->global_list,
					     CB_TREE (current_section));
		}
		emit_statement (cb_build_comment ("USE AFTER ERROR"));
	}
  }
#line 19259 "parser.c" /* yacc.c:1646  */
    break;

  case 1710:
#line 11645 "parser.y" /* yacc.c:1646  */
    {
	use_global_ind = 0;
  }
#line 19267 "parser.c" /* yacc.c:1646  */
    break;

  case 1711:
#line 11649 "parser.y" /* yacc.c:1646  */
    {
	if (current_program->prog_type == CB_FUNCTION_TYPE) {
		cb_error (_("%s is invalid in a user FUNCTION"), "GLOBAL");
	} else {
		use_global_ind = 1;
		current_program->flag_global_use = 1;
	}
  }
#line 19280 "parser.c" /* yacc.c:1646  */
    break;

  case 1712:
#line 11661 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		l;

	for (l = (yyvsp[0]); l; l = CB_CHAIN (l)) {
		if (CB_VALID_TREE (CB_VALUE (l))) {
			setup_use_file (CB_FILE (cb_ref (CB_VALUE (l))));
		}
	}
  }
#line 19294 "parser.c" /* yacc.c:1646  */
    break;

  case 1713:
#line 11671 "parser.y" /* yacc.c:1646  */
    {
	current_program->global_handler[COB_OPEN_INPUT].handler_label = current_section;
	current_program->global_handler[COB_OPEN_INPUT].handler_prog = current_program;
  }
#line 19303 "parser.c" /* yacc.c:1646  */
    break;

  case 1714:
#line 11676 "parser.y" /* yacc.c:1646  */
    {
	current_program->global_handler[COB_OPEN_OUTPUT].handler_label = current_section;
	current_program->global_handler[COB_OPEN_OUTPUT].handler_prog = current_program;
  }
#line 19312 "parser.c" /* yacc.c:1646  */
    break;

  case 1715:
#line 11681 "parser.y" /* yacc.c:1646  */
    {
	current_program->global_handler[COB_OPEN_I_O].handler_label = current_section;
	current_program->global_handler[COB_OPEN_I_O].handler_prog = current_program;
  }
#line 19321 "parser.c" /* yacc.c:1646  */
    break;

  case 1716:
#line 11686 "parser.y" /* yacc.c:1646  */
    {
	current_program->global_handler[COB_OPEN_EXTEND].handler_label = current_section;
	current_program->global_handler[COB_OPEN_EXTEND].handler_prog = current_program;
  }
#line 19330 "parser.c" /* yacc.c:1646  */
    break;

  case 1717:
#line 11694 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		plabel;
	char		name[64];

	cb_verify (cb_use_for_debugging, "USE FOR DEBUGGING");

	if (!in_declaratives) {
		cb_error (_("USE statement must be within DECLARATIVES"));
	} else if (current_program->nested_level) {
		cb_error (_("USE DEBUGGING not supported in contained program"));
	} else {
		in_debugging = 1;
		current_section->flag_begin = 1;
		current_section->flag_return = 1;
		current_section->flag_declarative_exit = 1;
		current_section->flag_real_label = 0;
		current_section->flag_is_debug_sect = 1;
		if (!needs_debug_item) {
			needs_debug_item = 1;
			cb_build_debug_item ();
		}
		if (!current_program->flag_debugging) {
			skip_statements = 1;
			current_section->flag_skip_label = 1;
		} else {
			current_program->flag_gen_debug = 1;
			sprintf (name, "EXIT SECTION %d", cb_id);
			plabel = cb_build_reference (name);
			plabel = cb_build_label (plabel, NULL);
			CB_LABEL (plabel)->flag_begin = 1;
			CB_LABEL (plabel)->flag_dummy_exit = 1;
			current_section->exit_label = plabel;
			emit_statement (cb_build_comment ("USE FOR DEBUGGING"));
		}
	}
  }
#line 19371 "parser.c" /* yacc.c:1646  */
    break;

  case 1720:
#line 11739 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		l;
	cb_tree		x;
	cb_tree		z;

	if (current_program->flag_debugging) {
		CB_REFERENCE ((yyvsp[0]))->debug_section = current_section;
		CB_REFERENCE ((yyvsp[0]))->flag_debug_code = 1;
		CB_REFERENCE ((yyvsp[0]))->flag_all_debug = 0;

		z = CB_LIST_INIT ((yyvsp[0]));
		current_program->debug_list =
			cb_list_append (current_program->debug_list, z);
		/* Check backward refs to file/data names */
		/* Label refs will be checked later (forward/backward ref) */
		if (CB_WORD_COUNT ((yyvsp[0])) > 0) {
			l = CB_VALUE (CB_WORD_ITEMS ((yyvsp[0])));
			switch (CB_TREE_TAG (l)) {
			case CB_TAG_CD:
				CB_CD (l)->debug_section = current_section;
				CB_CD (l)->flag_field_debug = 1;
				break;
			case CB_TAG_FILE:
				CB_FILE (l)->debug_section = current_section;
				CB_FILE (l)->flag_fl_debug = 1;
				break;
			case CB_TAG_FIELD:
				x = cb_ref ((yyvsp[0]));
				if (CB_INVALID_TREE (x)) {
					break;
				}
				needs_field_debug = 1;
				CB_FIELD (x)->debug_section = current_section;
				CB_FIELD (x)->flag_field_debug = 1;
				CB_PURPOSE (z) = x;
				break;
			default:
				break;
			}
		}
	}
  }
#line 19418 "parser.c" /* yacc.c:1646  */
    break;

  case 1721:
#line 11782 "parser.y" /* yacc.c:1646  */
    {
	if (current_program->flag_debugging) {
		if (current_program->all_procedure) {
			cb_error (_("duplicate USE DEBUGGING ON ALL PROCEDURES"));
		} else {
			current_program->all_procedure = current_section;
		}
	}
  }
#line 19432 "parser.c" /* yacc.c:1646  */
    break;

  case 1722:
#line 11792 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		x;

	if (current_program->flag_debugging) {
		/* Reference must be a data item */
		x = cb_ref ((yyvsp[0]));
		if (CB_INVALID_TREE (x) || !CB_FIELD_P (x)) {
			cb_error (_("invalid target for DEBUGGING ALL"));
		} else {
			needs_field_debug = 1;
			CB_FIELD (x)->debug_section = current_section;
			CB_FIELD (x)->flag_field_debug = 1;
			CB_FIELD (x)->flag_all_debug = 1;
			CB_REFERENCE ((yyvsp[0]))->debug_section = current_section;
			CB_REFERENCE ((yyvsp[0]))->flag_debug_code = 1;
			CB_REFERENCE ((yyvsp[0]))->flag_all_debug = 1;
			CB_CHAIN_PAIR (current_program->debug_list, x, (yyvsp[0]));
		}
	}
  }
#line 19457 "parser.c" /* yacc.c:1646  */
    break;

  case 1727:
#line 11822 "parser.y" /* yacc.c:1646  */
    {
	if (current_program->nested_level) {
		cb_error (_("%s is invalid in nested program"), "USE AT");
	}
  }
#line 19467 "parser.c" /* yacc.c:1646  */
    break;

  case 1728:
#line 11831 "parser.y" /* yacc.c:1646  */
    {
	emit_statement (cb_build_comment ("USE AT PROGRAM START"));
	/* emit_entry ("_START", 0, NULL, NULL); */
	CB_PENDING ("USE AT PROGRAM START");
  }
#line 19477 "parser.c" /* yacc.c:1646  */
    break;

  case 1729:
#line 11837 "parser.y" /* yacc.c:1646  */
    {
	emit_statement (cb_build_comment ("USE AT PROGRAM END"));
	/* emit_entry ("_END", 0, NULL, NULL); */
	CB_PENDING ("USE AT PROGRAM END");
  }
#line 19487 "parser.c" /* yacc.c:1646  */
    break;

  case 1730:
#line 11847 "parser.y" /* yacc.c:1646  */
    {
	current_section->flag_real_label = 1;
	emit_statement (cb_build_comment ("USE BEFORE REPORTING"));
	CB_PENDING ("USE BEFORE REPORTING");
  }
#line 19497 "parser.c" /* yacc.c:1646  */
    break;

  case 1731:
#line 11856 "parser.y" /* yacc.c:1646  */
    {
	current_section->flag_real_label = 1;
	emit_statement (cb_build_comment ("USE AFTER EXCEPTION CONDITION"));
	CB_PENDING ("USE AFTER EXCEPTION CONDITION");
  }
#line 19507 "parser.c" /* yacc.c:1646  */
    break;

  case 1734:
#line 11872 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("WRITE", TERM_WRITE);
	/* Special in debugging mode */
	save_debug = start_debug;
	start_debug = 0;
  }
#line 19518 "parser.c" /* yacc.c:1646  */
    break;

  case 1736:
#line 11884 "parser.y" /* yacc.c:1646  */
    {
	if (CB_VALID_TREE ((yyvsp[-5]))) {
		cb_emit_write ((yyvsp[-5]), (yyvsp[-4]), (yyvsp[-3]), (yyvsp[-1]));
	}
	start_debug = save_debug;
  }
#line 19529 "parser.c" /* yacc.c:1646  */
    break;

  case 1737:
#line 11893 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 19535 "parser.c" /* yacc.c:1646  */
    break;

  case 1738:
#line 11894 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 19541 "parser.c" /* yacc.c:1646  */
    break;

  case 1739:
#line 11899 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int0;
  }
#line 19549 "parser.c" /* yacc.c:1646  */
    break;

  case 1740:
#line 11903 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_write_advancing_lines ((yyvsp[-3]), (yyvsp[-1]));
  }
#line 19557 "parser.c" /* yacc.c:1646  */
    break;

  case 1741:
#line 11907 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_write_advancing_mnemonic ((yyvsp[-2]), (yyvsp[0]));
  }
#line 19565 "parser.c" /* yacc.c:1646  */
    break;

  case 1742:
#line 11911 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_write_advancing_page ((yyvsp[-2]));
  }
#line 19573 "parser.c" /* yacc.c:1646  */
    break;

  case 1743:
#line 11917 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_BEFORE; }
#line 19579 "parser.c" /* yacc.c:1646  */
    break;

  case 1744:
#line 11918 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_AFTER; }
#line 19585 "parser.c" /* yacc.c:1646  */
    break;

  case 1748:
#line 11929 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), WRITE);
  }
#line 19593 "parser.c" /* yacc.c:1646  */
    break;

  case 1749:
#line 11933 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), WRITE);
  }
#line 19601 "parser.c" /* yacc.c:1646  */
    break;

  case 1752:
#line 11947 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		cb_verify (cb_not_exception_before_exception,
			_("NOT EXCEPTION before EXCEPTION"));
	}
  }
#line 19612 "parser.c" /* yacc.c:1646  */
    break;

  case 1753:
#line 11957 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 19620 "parser.c" /* yacc.c:1646  */
    break;

  case 1754:
#line 11961 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 19628 "parser.c" /* yacc.c:1646  */
    break;

  case 1755:
#line 11968 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = ACCEPT_HANDLER;
	current_statement->ex_handler = (yyvsp[0]);
  }
#line 19637 "parser.c" /* yacc.c:1646  */
    break;

  case 1760:
#line 11986 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = ACCEPT_HANDLER;
	current_statement->not_ex_handler = (yyvsp[0]);
  }
#line 19646 "parser.c" /* yacc.c:1646  */
    break;

  case 1765:
#line 12002 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		cb_verify (cb_not_exception_before_exception,
			_("NOT EXCEPTION before EXCEPTION"));
	}
  }
#line 19657 "parser.c" /* yacc.c:1646  */
    break;

  case 1766:
#line 12012 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 19665 "parser.c" /* yacc.c:1646  */
    break;

  case 1767:
#line 12016 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 19673 "parser.c" /* yacc.c:1646  */
    break;

  case 1768:
#line 12023 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = DISPLAY_HANDLER;
	current_statement->ex_handler = (yyvsp[0]);
  }
#line 19682 "parser.c" /* yacc.c:1646  */
    break;

  case 1771:
#line 12036 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = DISPLAY_HANDLER;
	current_statement->not_ex_handler = (yyvsp[0]);
  }
#line 19691 "parser.c" /* yacc.c:1646  */
    break;

  case 1774:
#line 12048 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		cb_verify (cb_not_exception_before_exception,
			_("NOT SIZE ERROR before SIZE ERROR"));
	}
  }
#line 19702 "parser.c" /* yacc.c:1646  */
    break;

  case 1775:
#line 12058 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 19710 "parser.c" /* yacc.c:1646  */
    break;

  case 1776:
#line 12062 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 19718 "parser.c" /* yacc.c:1646  */
    break;

  case 1777:
#line 12069 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = SIZE_ERROR_HANDLER;
	current_statement->ex_handler = (yyvsp[0]);
  }
#line 19727 "parser.c" /* yacc.c:1646  */
    break;

  case 1780:
#line 12082 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = SIZE_ERROR_HANDLER;
	current_statement->not_ex_handler = (yyvsp[0]);
  }
#line 19736 "parser.c" /* yacc.c:1646  */
    break;

  case 1783:
#line 12094 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		cb_verify (cb_not_exception_before_exception,
			_("NOT OVERFLOW before OVERFLOW"));
	}
  }
#line 19747 "parser.c" /* yacc.c:1646  */
    break;

  case 1784:
#line 12104 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 19755 "parser.c" /* yacc.c:1646  */
    break;

  case 1785:
#line 12108 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 19763 "parser.c" /* yacc.c:1646  */
    break;

  case 1786:
#line 12115 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = OVERFLOW_HANDLER;
	current_statement->ex_handler = (yyvsp[0]);
  }
#line 19772 "parser.c" /* yacc.c:1646  */
    break;

  case 1789:
#line 12128 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = OVERFLOW_HANDLER;
	current_statement->not_ex_handler = (yyvsp[0]);
  }
#line 19781 "parser.c" /* yacc.c:1646  */
    break;

  case 1791:
#line 12140 "parser.y" /* yacc.c:1646  */
    {
	cb_verify (cb_not_exception_before_exception, "NOT AT END before AT END");
  }
#line 19789 "parser.c" /* yacc.c:1646  */
    break;

  case 1793:
#line 12149 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		cb_verify (cb_not_exception_before_exception, "NOT AT END before AT END");
	}
  }
#line 19799 "parser.c" /* yacc.c:1646  */
    break;

  case 1794:
#line 12158 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 19807 "parser.c" /* yacc.c:1646  */
    break;

  case 1795:
#line 12162 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 19815 "parser.c" /* yacc.c:1646  */
    break;

  case 1796:
#line 12169 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = AT_END_HANDLER;
	current_statement->ex_handler = (yyvsp[0]);
  }
#line 19824 "parser.c" /* yacc.c:1646  */
    break;

  case 1799:
#line 12182 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = AT_END_HANDLER;
	current_statement->not_ex_handler = (yyvsp[0]);
  }
#line 19833 "parser.c" /* yacc.c:1646  */
    break;

  case 1801:
#line 12193 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		cb_verify (cb_not_exception_before_exception,
			_("NOT AT END-OF-PAGE before AT END-OF-PAGE"));
	}
  }
#line 19844 "parser.c" /* yacc.c:1646  */
    break;

  case 1802:
#line 12203 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 19852 "parser.c" /* yacc.c:1646  */
    break;

  case 1803:
#line 12207 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 19860 "parser.c" /* yacc.c:1646  */
    break;

  case 1804:
#line 12214 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = EOP_HANDLER;
	current_statement->ex_handler = (yyvsp[0]);
  }
#line 19869 "parser.c" /* yacc.c:1646  */
    break;

  case 1807:
#line 12227 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = EOP_HANDLER;
	current_statement->not_ex_handler = (yyvsp[0]);
  }
#line 19878 "parser.c" /* yacc.c:1646  */
    break;

  case 1811:
#line 12243 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		cb_verify (cb_not_exception_before_exception,
			_("NOT INVALID KEY before INVALID KEY"));
	}
  }
#line 19889 "parser.c" /* yacc.c:1646  */
    break;

  case 1812:
#line 12253 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 19897 "parser.c" /* yacc.c:1646  */
    break;

  case 1813:
#line 12257 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 19905 "parser.c" /* yacc.c:1646  */
    break;

  case 1814:
#line 12264 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = INVALID_KEY_HANDLER;
	current_statement->ex_handler = (yyvsp[0]);
  }
#line 19914 "parser.c" /* yacc.c:1646  */
    break;

  case 1817:
#line 12277 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = INVALID_KEY_HANDLER;
	current_statement->not_ex_handler = (yyvsp[0]);
  }
#line 19923 "parser.c" /* yacc.c:1646  */
    break;

  case 1818:
#line 12287 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 19931 "parser.c" /* yacc.c:1646  */
    break;

  case 1819:
#line 12291 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
	CB_PENDING ("THREAD");
  }
#line 19940 "parser.c" /* yacc.c:1646  */
    break;

  case 1820:
#line 12299 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 19948 "parser.c" /* yacc.c:1646  */
    break;

  case 1821:
#line 12303 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
	CB_PENDING ("THREAD");
  }
#line 19957 "parser.c" /* yacc.c:1646  */
    break;

  case 1822:
#line 12311 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 19965 "parser.c" /* yacc.c:1646  */
    break;

  case 1823:
#line 12315 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 19973 "parser.c" /* yacc.c:1646  */
    break;

  case 1824:
#line 12324 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_one;
  }
#line 19981 "parser.c" /* yacc.c:1646  */
    break;

  case 1825:
#line 12328 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-1]);
  }
#line 19989 "parser.c" /* yacc.c:1646  */
    break;

  case 1826:
#line 12338 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_cond ((yyvsp[0]));
	cb_end_cond ((yyval));
  }
#line 19998 "parser.c" /* yacc.c:1646  */
    break;

  case 1827:
#line 12346 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_expr ((yyvsp[0]));
  }
#line 20006 "parser.c" /* yacc.c:1646  */
    break;

  case 1828:
#line 12352 "parser.y" /* yacc.c:1646  */
    {
	current_expr = NULL;
	cb_exp_line = cb_source_line;
  }
#line 20015 "parser.c" /* yacc.c:1646  */
    break;

  case 1829:
#line 12357 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_reverse (current_expr);
  }
#line 20023 "parser.c" /* yacc.c:1646  */
    break;

  case 1832:
#line 12368 "parser.y" /* yacc.c:1646  */
    { push_expr ('x', (yyvsp[0])); }
#line 20029 "parser.c" /* yacc.c:1646  */
    break;

  case 1835:
#line 12373 "parser.y" /* yacc.c:1646  */
    { push_expr ('x', cb_zero); }
#line 20035 "parser.c" /* yacc.c:1646  */
    break;

  case 1836:
#line 12375 "parser.y" /* yacc.c:1646  */
    { push_expr ('(', NULL); }
#line 20041 "parser.c" /* yacc.c:1646  */
    break;

  case 1837:
#line 12376 "parser.y" /* yacc.c:1646  */
    { push_expr (')', NULL); }
#line 20047 "parser.c" /* yacc.c:1646  */
    break;

  case 1838:
#line 12378 "parser.y" /* yacc.c:1646  */
    { push_expr ('+', NULL); }
#line 20053 "parser.c" /* yacc.c:1646  */
    break;

  case 1839:
#line 12379 "parser.y" /* yacc.c:1646  */
    { push_expr ('-', NULL); }
#line 20059 "parser.c" /* yacc.c:1646  */
    break;

  case 1840:
#line 12380 "parser.y" /* yacc.c:1646  */
    { push_expr ('*', NULL); }
#line 20065 "parser.c" /* yacc.c:1646  */
    break;

  case 1841:
#line 12381 "parser.y" /* yacc.c:1646  */
    { push_expr ('/', NULL); }
#line 20071 "parser.c" /* yacc.c:1646  */
    break;

  case 1842:
#line 12382 "parser.y" /* yacc.c:1646  */
    { push_expr ('^', NULL); }
#line 20077 "parser.c" /* yacc.c:1646  */
    break;

  case 1844:
#line 12385 "parser.y" /* yacc.c:1646  */
    { push_expr ('&', NULL); }
#line 20083 "parser.c" /* yacc.c:1646  */
    break;

  case 1845:
#line 12386 "parser.y" /* yacc.c:1646  */
    { push_expr ('|', NULL); }
#line 20089 "parser.c" /* yacc.c:1646  */
    break;

  case 1848:
#line 12395 "parser.y" /* yacc.c:1646  */
    { push_expr ('!', NULL); }
#line 20095 "parser.c" /* yacc.c:1646  */
    break;

  case 1849:
#line 12398 "parser.y" /* yacc.c:1646  */
    { push_expr ('C', (yyvsp[0])); }
#line 20101 "parser.c" /* yacc.c:1646  */
    break;

  case 1850:
#line 12400 "parser.y" /* yacc.c:1646  */
    { push_expr ('=', NULL); }
#line 20107 "parser.c" /* yacc.c:1646  */
    break;

  case 1851:
#line 12401 "parser.y" /* yacc.c:1646  */
    { push_expr ('>', NULL); }
#line 20113 "parser.c" /* yacc.c:1646  */
    break;

  case 1852:
#line 12402 "parser.y" /* yacc.c:1646  */
    { push_expr ('<', NULL); }
#line 20119 "parser.c" /* yacc.c:1646  */
    break;

  case 1853:
#line 12403 "parser.y" /* yacc.c:1646  */
    { push_expr (']', NULL); }
#line 20125 "parser.c" /* yacc.c:1646  */
    break;

  case 1854:
#line 12404 "parser.y" /* yacc.c:1646  */
    { push_expr ('[', NULL); }
#line 20131 "parser.c" /* yacc.c:1646  */
    break;

  case 1855:
#line 12405 "parser.y" /* yacc.c:1646  */
    { push_expr ('~', NULL); }
#line 20137 "parser.c" /* yacc.c:1646  */
    break;

  case 1856:
#line 12407 "parser.y" /* yacc.c:1646  */
    { push_expr ('O', NULL); }
#line 20143 "parser.c" /* yacc.c:1646  */
    break;

  case 1857:
#line 12408 "parser.y" /* yacc.c:1646  */
    { push_expr ('9', NULL); }
#line 20149 "parser.c" /* yacc.c:1646  */
    break;

  case 1858:
#line 12409 "parser.y" /* yacc.c:1646  */
    { push_expr ('A', NULL); }
#line 20155 "parser.c" /* yacc.c:1646  */
    break;

  case 1859:
#line 12410 "parser.y" /* yacc.c:1646  */
    { push_expr ('L', NULL); }
#line 20161 "parser.c" /* yacc.c:1646  */
    break;

  case 1860:
#line 12411 "parser.y" /* yacc.c:1646  */
    { push_expr ('U', NULL); }
#line 20167 "parser.c" /* yacc.c:1646  */
    break;

  case 1861:
#line 12414 "parser.y" /* yacc.c:1646  */
    { push_expr ('P', NULL); }
#line 20173 "parser.c" /* yacc.c:1646  */
    break;

  case 1862:
#line 12415 "parser.y" /* yacc.c:1646  */
    { push_expr ('N', NULL); }
#line 20179 "parser.c" /* yacc.c:1646  */
    break;

  case 1871:
#line 12445 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
  }
#line 20187 "parser.c" /* yacc.c:1646  */
    break;

  case 1872:
#line 12449 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[-2]), (yyvsp[0]));
  }
#line 20195 "parser.c" /* yacc.c:1646  */
    break;

  case 1876:
#line 12460 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_binary_op ((yyvsp[-2]), '+', (yyvsp[0])); }
#line 20201 "parser.c" /* yacc.c:1646  */
    break;

  case 1877:
#line 12461 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_binary_op ((yyvsp[-2]), '-', (yyvsp[0])); }
#line 20207 "parser.c" /* yacc.c:1646  */
    break;

  case 1878:
#line 12462 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 20213 "parser.c" /* yacc.c:1646  */
    break;

  case 1879:
#line 12466 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_binary_op ((yyvsp[-2]), '*', (yyvsp[0])); }
#line 20219 "parser.c" /* yacc.c:1646  */
    break;

  case 1880:
#line 12467 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_binary_op ((yyvsp[-2]), '/', (yyvsp[0])); }
#line 20225 "parser.c" /* yacc.c:1646  */
    break;

  case 1881:
#line 12468 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 20231 "parser.c" /* yacc.c:1646  */
    break;

  case 1882:
#line 12473 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_binary_op ((yyvsp[-2]), '^', (yyvsp[0]));
  }
#line 20239 "parser.c" /* yacc.c:1646  */
    break;

  case 1883:
#line 12476 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 20245 "parser.c" /* yacc.c:1646  */
    break;

  case 1884:
#line 12480 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 20251 "parser.c" /* yacc.c:1646  */
    break;

  case 1885:
#line 12481 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_binary_op (cb_zero, '-', (yyvsp[0])); }
#line 20257 "parser.c" /* yacc.c:1646  */
    break;

  case 1886:
#line 12482 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 20263 "parser.c" /* yacc.c:1646  */
    break;

  case 1887:
#line 12485 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[-1]); }
#line 20269 "parser.c" /* yacc.c:1646  */
    break;

  case 1888:
#line 12486 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 20275 "parser.c" /* yacc.c:1646  */
    break;

  case 1889:
#line 12497 "parser.y" /* yacc.c:1646  */
    {
	if (current_linage > 1) {
		cb_error (_("LINAGE-COUNTER must be qualified here"));
		(yyval) = cb_error_node;
	} else if (current_linage == 0) {
		cb_error (_("invalid LINAGE-COUNTER usage"));
		(yyval) = cb_error_node;
	} else {
		(yyval) = linage_file->linage_ctr;
	}
  }
#line 20291 "parser.c" /* yacc.c:1646  */
    break;

  case 1890:
#line 12509 "parser.y" /* yacc.c:1646  */
    {
	if (CB_FILE_P (cb_ref ((yyvsp[0])))) {
		(yyval) = CB_FILE (cb_ref ((yyvsp[0])))->linage_ctr;
	} else {
		cb_error_x ((yyvsp[0]), _("'%s' is not a file name"), CB_NAME ((yyvsp[0])));
		(yyval) = cb_error_node;
	}
  }
#line 20304 "parser.c" /* yacc.c:1646  */
    break;

  case 1891:
#line 12518 "parser.y" /* yacc.c:1646  */
    {
	if (report_count > 1) {
		cb_error (_("LINE-COUNTER must be qualified here"));
		(yyval) = cb_error_node;
	} else if (report_count == 0) {
		cb_error (_("invalid LINE-COUNTER usage"));
		(yyval) = cb_error_node;
	} else {
		(yyval) = report_instance->line_counter;
	}
  }
#line 20320 "parser.c" /* yacc.c:1646  */
    break;

  case 1892:
#line 12530 "parser.y" /* yacc.c:1646  */
    {
	if (CB_REPORT_P (cb_ref ((yyvsp[0])))) {
		(yyval) = CB_REPORT (cb_ref ((yyvsp[0])))->line_counter;
	} else {
		cb_error_x ((yyvsp[0]), _("'%s' is not a report name"), CB_NAME ((yyvsp[0])));
		(yyval) = cb_error_node;
	}
  }
#line 20333 "parser.c" /* yacc.c:1646  */
    break;

  case 1893:
#line 12539 "parser.y" /* yacc.c:1646  */
    {
	if (report_count > 1) {
		cb_error (_("PAGE-COUNTER must be qualified here"));
		(yyval) = cb_error_node;
	} else if (report_count == 0) {
		cb_error (_("invalid PAGE-COUNTER usage"));
		(yyval) = cb_error_node;
	} else {
		(yyval) = report_instance->page_counter;
	}
  }
#line 20349 "parser.c" /* yacc.c:1646  */
    break;

  case 1894:
#line 12551 "parser.y" /* yacc.c:1646  */
    {
	if (CB_REPORT_P (cb_ref ((yyvsp[0])))) {
		(yyval) = CB_REPORT (cb_ref ((yyvsp[0])))->page_counter;
	} else {
		cb_error_x ((yyvsp[0]), _("'%s' is not a report name"), CB_NAME ((yyvsp[0])));
		(yyval) = cb_error_node;
	}
  }
#line 20362 "parser.c" /* yacc.c:1646  */
    break;

  case 1895:
#line 12565 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 20368 "parser.c" /* yacc.c:1646  */
    break;

  case 1896:
#line 12567 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_append ((yyvsp[-1]), (yyvsp[0])); }
#line 20374 "parser.c" /* yacc.c:1646  */
    break;

  case 1897:
#line 12572 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_PAIR ((yyvsp[0]), (yyvsp[-1]));
  }
#line 20382 "parser.c" /* yacc.c:1646  */
    break;

  case 1898:
#line 12580 "parser.y" /* yacc.c:1646  */
    { cb_build_identifier ((yyvsp[0]), 0); }
#line 20388 "parser.c" /* yacc.c:1646  */
    break;

  case 1899:
#line 12587 "parser.y" /* yacc.c:1646  */
    {
	if (!CB_FILE_P (cb_ref ((yyvsp[0])))) {
		(yyval) = (yyvsp[0]);
	} else {
		cb_error_x ((yyvsp[0]), _("%s requires a record name as subject"),
			current_statement->name);
		(yyval) = cb_error_node;
	}
  }
#line 20402 "parser.c" /* yacc.c:1646  */
    break;

  case 1900:
#line 12597 "parser.y" /* yacc.c:1646  */
    {
	if (CB_FILE_P (cb_ref ((yyvsp[0])))) {
		(yyval) = (yyvsp[0]);
	} else {
		cb_error_x ((yyvsp[0]), _("'%s' is not a file name"), CB_NAME ((yyvsp[0])));
		(yyval) = cb_error_node;
	}
  }
#line 20415 "parser.c" /* yacc.c:1646  */
    break;

  case 1901:
#line 12611 "parser.y" /* yacc.c:1646  */
    {
	cb_tree x;

	x = cb_ref ((yyvsp[0]));
	if (!CB_FIELD_P (x)) {
		(yyval) = cb_error_node;
	} else if (!CB_FIELD (x)->index_list) {
		cb_error_x ((yyvsp[0]), _("'%s' not indexed"), cb_name ((yyvsp[0])));
		listprint_suppress ();
		cb_error_x (x, _("'%s' defined here"), cb_name (x));
		listprint_restore ();
		(yyval) = cb_error_node;
	} else {
		(yyval) = (yyvsp[0]);
	}
  }
#line 20436 "parser.c" /* yacc.c:1646  */
    break;

  case 1902:
#line 12633 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
  }
#line 20444 "parser.c" /* yacc.c:1646  */
    break;

  case 1903:
#line 12637 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		l;

	if (CB_VALID_TREE ((yyvsp[0]))) {
		for (l = (yyvsp[-1]); l; l = CB_CHAIN (l)) {
			if (CB_VALID_TREE (CB_VALUE (l)) &&
			    !strcasecmp (CB_NAME ((yyvsp[0])), CB_NAME (CB_VALUE (l)))) {
				cb_error_x ((yyvsp[0]), _("multiple reference to '%s' "),
					    CB_NAME ((yyvsp[0])));
				break;
			}
		}
		if (!l) {
			(yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0]));
		}
	}
  }
#line 20466 "parser.c" /* yacc.c:1646  */
    break;

  case 1904:
#line 12658 "parser.y" /* yacc.c:1646  */
    {
	if (CB_FILE_P (cb_ref ((yyvsp[0])))) {
		(yyval) = (yyvsp[0]);
	} else {
		cb_error_x ((yyvsp[0]), _("'%s' is not a file name"), CB_NAME ((yyvsp[0])));
		(yyval) = cb_error_node;
	}
  }
#line 20479 "parser.c" /* yacc.c:1646  */
    break;

  case 1905:
#line 12670 "parser.y" /* yacc.c:1646  */
    {
	if (CB_CD_P (cb_ref ((yyvsp[0])))) {
		(yyval) = (yyvsp[0]);
	} else {
		cb_error_x ((yyvsp[0]), _("'%s' is not a CD name"), CB_NAME ((yyvsp[0])));
		(yyval) = cb_error_node;
	}
  }
#line 20492 "parser.c" /* yacc.c:1646  */
    break;

  case 1906:
#line 12711 "parser.y" /* yacc.c:1646  */
    {
	if (CB_REPORT_P (cb_ref ((yyvsp[0])))) {
		(yyval) = (yyvsp[0]);
	} else {
		cb_error_x ((yyvsp[0]), _("'%s' is not a report name"), CB_NAME ((yyvsp[0])));
		(yyval) = cb_error_node;
	}
  }
#line 20505 "parser.c" /* yacc.c:1646  */
    break;

  case 1907:
#line 12724 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 20511 "parser.c" /* yacc.c:1646  */
    break;

  case 1908:
#line 12726 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0])); }
#line 20517 "parser.c" /* yacc.c:1646  */
    break;

  case 1909:
#line 12730 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 20523 "parser.c" /* yacc.c:1646  */
    break;

  case 1910:
#line 12736 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 20529 "parser.c" /* yacc.c:1646  */
    break;

  case 1911:
#line 12738 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0])); }
#line 20535 "parser.c" /* yacc.c:1646  */
    break;

  case 1912:
#line 12743 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
	CB_REFERENCE ((yyval))->offset = CB_TREE (current_section);
	CB_REFERENCE ((yyval))->flag_in_decl = !!in_declaratives;
	CB_REFERENCE ((yyval))->section = current_section;
	CB_REFERENCE ((yyval))->paragraph = current_paragraph;
	CB_ADD_TO_CHAIN ((yyval), current_program->label_list);
  }
#line 20548 "parser.c" /* yacc.c:1646  */
    break;

  case 1915:
#line 12757 "parser.y" /* yacc.c:1646  */
    {
	CB_REFERENCE ((yyvsp[-2]))->chain = (yyvsp[0]);
  }
#line 20556 "parser.c" /* yacc.c:1646  */
    break;

  case 1916:
#line 12764 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_reference ((char *)(CB_LITERAL ((yyvsp[0]))->data));
	(yyval)->source_file = (yyvsp[0])->source_file;
	(yyval)->source_line = (yyvsp[0])->source_line;
  }
#line 20566 "parser.c" /* yacc.c:1646  */
    break;

  case 1917:
#line 12774 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 20572 "parser.c" /* yacc.c:1646  */
    break;

  case 1918:
#line 12775 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0])); }
#line 20578 "parser.c" /* yacc.c:1646  */
    break;

  case 1919:
#line 12780 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
	CB_ADD_TO_CHAIN ((yyval), current_program->reference_list);
  }
#line 20587 "parser.c" /* yacc.c:1646  */
    break;

  case 1920:
#line 12788 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
	CB_ADD_TO_CHAIN ((yyval), current_program->reference_list);
  }
#line 20596 "parser.c" /* yacc.c:1646  */
    break;

  case 1921:
#line 12796 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
  }
#line 20604 "parser.c" /* yacc.c:1646  */
    break;

  case 1922:
#line 12800 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0]));
  }
#line 20612 "parser.c" /* yacc.c:1646  */
    break;

  case 1923:
#line 12807 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
	CB_REFERENCE((yyval))->flag_optional = 1;
	CB_ADD_TO_CHAIN ((yyval), current_program->reference_list);
  }
#line 20622 "parser.c" /* yacc.c:1646  */
    break;

  case 1926:
#line 12823 "parser.y" /* yacc.c:1646  */
    {
	if (CB_WORD_COUNT ((yyvsp[0])) > 0) {
		redefinition_error ((yyvsp[0]));
		(yyval) = cb_error_node;
	} else {
		(yyval) = (yyvsp[0]);
	}
  }
#line 20635 "parser.c" /* yacc.c:1646  */
    break;

  case 1927:
#line 12832 "parser.y" /* yacc.c:1646  */
    {
	yyclearin;
	yyerrok;
	(yyval) = cb_error_node;
  }
#line 20645 "parser.c" /* yacc.c:1646  */
    break;

  case 1928:
#line 12843 "parser.y" /* yacc.c:1646  */
    {
	if (CB_REFERENCE ((yyvsp[0]))->flag_duped || CB_WORD_COUNT ((yyvsp[0])) > 0) {
		redefinition_error ((yyvsp[0]));
		(yyval) = NULL;
	} else {
		CB_WORD_COUNT ((yyvsp[0]))++;
		(yyval) = (yyvsp[0]);
	}
  }
#line 20659 "parser.c" /* yacc.c:1646  */
    break;

  case 1929:
#line 12860 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
  }
#line 20667 "parser.c" /* yacc.c:1646  */
    break;

  case 1930:
#line 12864 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0]));
  }
#line 20675 "parser.c" /* yacc.c:1646  */
    break;

  case 1933:
#line 12873 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_address ((yyvsp[0]));
  }
#line 20683 "parser.c" /* yacc.c:1646  */
    break;

  case 1934:
#line 12879 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 20689 "parser.c" /* yacc.c:1646  */
    break;

  case 1935:
#line 12880 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 20695 "parser.c" /* yacc.c:1646  */
    break;

  case 1936:
#line 12885 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
  }
#line 20703 "parser.c" /* yacc.c:1646  */
    break;

  case 1937:
#line 12889 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0]));
  }
#line 20711 "parser.c" /* yacc.c:1646  */
    break;

  case 1945:
#line 12909 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_length ((yyvsp[0]));
  }
#line 20719 "parser.c" /* yacc.c:1646  */
    break;

  case 1946:
#line 12913 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_length ((yyvsp[0]));
  }
#line 20727 "parser.c" /* yacc.c:1646  */
    break;

  case 1947:
#line 12917 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_length ((yyvsp[0]));
  }
#line 20735 "parser.c" /* yacc.c:1646  */
    break;

  case 1948:
#line 12921 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_ppointer ((yyvsp[0]));
  }
#line 20743 "parser.c" /* yacc.c:1646  */
    break;

  case 1949:
#line 12925 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_address ((yyvsp[0]));
  }
#line 20751 "parser.c" /* yacc.c:1646  */
    break;

  case 1950:
#line 12929 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		x;
	cb_tree		switch_id;

	x = cb_ref ((yyvsp[0]));
	if (CB_VALID_TREE (x)) {
		if (CB_SYSTEM_NAME (x)->category != CB_SWITCH_NAME) {
			cb_error_x ((yyvsp[0]), _("invalid mnemonic identifier"));
			(yyval) = cb_error_node;
		} else {
			switch_id = cb_int (CB_SYSTEM_NAME (x)->token);
			(yyval) = CB_BUILD_FUNCALL_1 ("cob_switch_value", switch_id);
		}
	} else {
		(yyval) = cb_error_node;
	}
  }
#line 20773 "parser.c" /* yacc.c:1646  */
    break;

  case 1951:
#line 12950 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
  }
#line 20781 "parser.c" /* yacc.c:1646  */
    break;

  case 1952:
#line 12954 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0]));
  }
#line 20789 "parser.c" /* yacc.c:1646  */
    break;

  case 1960:
#line 12971 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_length ((yyvsp[0]));
  }
#line 20797 "parser.c" /* yacc.c:1646  */
    break;

  case 1961:
#line 12975 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_length ((yyvsp[0]));
  }
#line 20805 "parser.c" /* yacc.c:1646  */
    break;

  case 1962:
#line 12979 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_length ((yyvsp[0]));
  }
#line 20813 "parser.c" /* yacc.c:1646  */
    break;

  case 1967:
#line 12996 "parser.y" /* yacc.c:1646  */
    {
	error_if_not_usage_display_or_nonnumeric_lit ((yyvsp[0]));
  }
#line 20821 "parser.c" /* yacc.c:1646  */
    break;

  case 1968:
#line 13003 "parser.y" /* yacc.c:1646  */
    {
	error_if_not_usage_display_or_nonnumeric_lit ((yyvsp[0]));
  }
#line 20829 "parser.c" /* yacc.c:1646  */
    break;

  case 1974:
#line 13021 "parser.y" /* yacc.c:1646  */
    {
	check_not_88_level ((yyvsp[0]));
  }
#line 20837 "parser.c" /* yacc.c:1646  */
    break;

  case 1976:
#line 13029 "parser.y" /* yacc.c:1646  */
    {
	check_not_88_level ((yyvsp[0]));
  }
#line 20845 "parser.c" /* yacc.c:1646  */
    break;

  case 1979:
#line 13038 "parser.y" /* yacc.c:1646  */
    {
	check_not_88_level ((yyvsp[0]));
  }
#line 20853 "parser.c" /* yacc.c:1646  */
    break;

  case 1982:
#line 13047 "parser.y" /* yacc.c:1646  */
    {
	check_not_88_level ((yyvsp[0]));
  }
#line 20861 "parser.c" /* yacc.c:1646  */
    break;

  case 1984:
#line 13052 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_zero;
  }
#line 20869 "parser.c" /* yacc.c:1646  */
    break;

  case 1985:
#line 13061 "parser.y" /* yacc.c:1646  */
    {
	check_not_88_level ((yyvsp[0]));
  }
#line 20877 "parser.c" /* yacc.c:1646  */
    break;

  case 1987:
#line 13071 "parser.y" /* yacc.c:1646  */
    {
	check_not_88_level ((yyvsp[0]));
  }
#line 20885 "parser.c" /* yacc.c:1646  */
    break;

  case 1989:
#line 13079 "parser.y" /* yacc.c:1646  */
    {
	check_not_88_level ((yyvsp[0]));
  }
#line 20893 "parser.c" /* yacc.c:1646  */
    break;

  case 1992:
#line 13089 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_identifier ((yyvsp[0]), 0); }
#line 20899 "parser.c" /* yacc.c:1646  */
    break;

  case 1993:
#line 13093 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_identifier ((yyvsp[0]), 1); }
#line 20905 "parser.c" /* yacc.c:1646  */
    break;

  case 1994:
#line 13097 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 20911 "parser.c" /* yacc.c:1646  */
    break;

  case 1995:
#line 13098 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[-1]); }
#line 20917 "parser.c" /* yacc.c:1646  */
    break;

  case 1996:
#line 13103 "parser.y" /* yacc.c:1646  */
    {
	error_if_not_usage_display_or_nonnumeric_lit ((yyvsp[0]));
  }
#line 20925 "parser.c" /* yacc.c:1646  */
    break;

  case 1997:
#line 13110 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0]) != cb_error_node
	    && cb_tree_category ((yyvsp[0])) != CB_CATEGORY_NUMERIC) {
		cb_error_x ((yyvsp[0]), _("'%s' is not numeric"), cb_name ((yyvsp[0])));
	}
  }
#line 20936 "parser.c" /* yacc.c:1646  */
    break;

  case 1998:
#line 13120 "parser.y" /* yacc.c:1646  */
    {
	int     reference_to_existing_object;

	if (CB_REFERENCE_P ((yyvsp[0])) && (CB_FIELD_P (cb_ref ((yyvsp[0])))
				    || CB_FILE_P (cb_ref ((yyvsp[0]))))) {
		(yyval) = cb_build_identifier ((yyvsp[0]), 0);
	} else {
		reference_to_existing_object =
			CB_REFERENCE_P ((yyvsp[0])) && cb_ref ((yyvsp[0])) != cb_error_node;
		if (!CB_REFERENCE_P ((yyvsp[0])) || reference_to_existing_object) {
			cb_error_x ((yyvsp[0]), _("'%s' is not a field or file"), cb_name ((yyvsp[0])));
		}
		(yyval) = cb_error_node;
	}
  }
#line 20956 "parser.c" /* yacc.c:1646  */
    break;

  case 1999:
#line 13139 "parser.y" /* yacc.c:1646  */
    {
	int     reference_to_existing_object;

	if (CB_REFERENCE_P ((yyvsp[0])) && CB_FIELD_P (cb_ref ((yyvsp[0])))) {
		(yyval) = cb_build_identifier ((yyvsp[0]), 0);
	} else {
		reference_to_existing_object =
			CB_REFERENCE_P ((yyvsp[0])) && cb_ref ((yyvsp[0])) != cb_error_node;
		if (!CB_REFERENCE_P ((yyvsp[0])) || reference_to_existing_object) {
			cb_error_x ((yyvsp[0]), _("'%s' is not a field"), cb_name ((yyvsp[0])));
		}
		(yyval) = cb_error_node;
	}
  }
#line 20975 "parser.c" /* yacc.c:1646  */
    break;

  case 2000:
#line 13157 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-2]);
	if (start_debug) {
		cb_check_field_debug ((yyvsp[-2]));
	}
  }
#line 20986 "parser.c" /* yacc.c:1646  */
    break;

  case 2001:
#line 13164 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-1]);
	if (start_debug) {
		cb_check_field_debug ((yyvsp[-1]));
	}
  }
#line 20997 "parser.c" /* yacc.c:1646  */
    break;

  case 2002:
#line 13171 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-1]);
	if (start_debug) {
		cb_check_field_debug ((yyvsp[-1]));
	}
  }
#line 21008 "parser.c" /* yacc.c:1646  */
    break;

  case 2003:
#line 13178 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
	if (start_debug) {
		cb_check_field_debug ((yyvsp[0]));
	}
  }
#line 21019 "parser.c" /* yacc.c:1646  */
    break;

  case 2004:
#line 13188 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
  }
#line 21027 "parser.c" /* yacc.c:1646  */
    break;

  case 2005:
#line 13192 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0]));
  }
#line 21035 "parser.c" /* yacc.c:1646  */
    break;

  case 2006:
#line 13199 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_identifier ((yyvsp[0]), 0);
  }
#line 21043 "parser.c" /* yacc.c:1646  */
    break;

  case 2007:
#line 13206 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-2]);
	if (CB_REFERENCE_P ((yyvsp[-2]))) {
		CB_REFERENCE ((yyvsp[-2]))->flag_target = 1;
	}
	if (start_debug) {
		cb_check_field_debug ((yyvsp[-2]));
	}
  }
#line 21057 "parser.c" /* yacc.c:1646  */
    break;

  case 2008:
#line 13216 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-1]);
	if (CB_REFERENCE_P ((yyvsp[-1]))) {
		CB_REFERENCE ((yyvsp[-1]))->flag_target = 1;
	}
	if (start_debug) {
		cb_check_field_debug ((yyvsp[-1]));
	}
  }
#line 21071 "parser.c" /* yacc.c:1646  */
    break;

  case 2009:
#line 13226 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-1]);
	if (CB_REFERENCE_P ((yyvsp[-1]))) {
		CB_REFERENCE ((yyvsp[-1]))->flag_target = 1;
	}
	if (start_debug) {
		cb_check_field_debug ((yyvsp[-1]));
	}
  }
#line 21085 "parser.c" /* yacc.c:1646  */
    break;

  case 2010:
#line 13236 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
	if (CB_REFERENCE_P ((yyvsp[0]))) {
		CB_REFERENCE ((yyvsp[0]))->flag_target = 1;
	}
	if (start_debug) {
		cb_check_field_debug ((yyvsp[0]));
	}
  }
#line 21099 "parser.c" /* yacc.c:1646  */
    break;

  case 2011:
#line 13249 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 21107 "parser.c" /* yacc.c:1646  */
    break;

  case 2012:
#line 13253 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-2]);
	CB_REFERENCE ((yyvsp[-2]))->chain = (yyvsp[0]);
  }
#line 21116 "parser.c" /* yacc.c:1646  */
    break;

  case 2013:
#line 13261 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-3]);
	CB_REFERENCE ((yyvsp[-3]))->subs = cb_list_reverse ((yyvsp[-1]));
  }
#line 21125 "parser.c" /* yacc.c:1646  */
    break;

  case 2014:
#line 13269 "parser.y" /* yacc.c:1646  */
    {
	CB_REFERENCE ((yyvsp[-4]))->offset = (yyvsp[-2]);
  }
#line 21133 "parser.c" /* yacc.c:1646  */
    break;

  case 2015:
#line 13273 "parser.y" /* yacc.c:1646  */
    {
	CB_REFERENCE ((yyvsp[-5]))->offset = (yyvsp[-3]);
	CB_REFERENCE ((yyvsp[-5]))->length = (yyvsp[-1]);
  }
#line 21142 "parser.c" /* yacc.c:1646  */
    break;

  case 2016:
#line 13283 "parser.y" /* yacc.c:1646  */
    {
	if (cb_tree_category ((yyvsp[0])) != CB_CATEGORY_NUMERIC
	    || CB_LITERAL ((yyvsp[0]))->sign < 0
	    || CB_LITERAL ((yyvsp[0]))->scale) {
		cb_error (_("non-negative integer value expected"));
		(yyval) = cb_build_numeric_literal(-1, "1", 0);
	} else {
		(yyval) = (yyvsp[0]);
	}
  }
#line 21157 "parser.c" /* yacc.c:1646  */
    break;

  case 2017:
#line 13297 "parser.y" /* yacc.c:1646  */
    {
	int	n;

	if (cb_tree_category ((yyvsp[0])) != CB_CATEGORY_NUMERIC) {
		cb_error (_("integer value expected"));
		(yyval) = cb_int1;
	} else if (CB_LITERAL ((yyvsp[0]))->sign || CB_LITERAL ((yyvsp[0]))->scale) {
		cb_error (_("integer value expected"));
		(yyval) = cb_int1;
	} else {
		n = cb_get_int ((yyvsp[0]));
		if (n < 1 || n > 256) {
			cb_error (_("invalid symbolic integer"));
			(yyval) = cb_int1;
		} else {
			(yyval) = (yyvsp[0]);
		}
	}
  }
#line 21181 "parser.c" /* yacc.c:1646  */
    break;

  case 2018:
#line 13320 "parser.y" /* yacc.c:1646  */
    {
	int	n;

	if (cb_tree_category ((yyvsp[0])) != CB_CATEGORY_NUMERIC
	    || CB_LITERAL ((yyvsp[0]))->sign
	    || CB_LITERAL ((yyvsp[0]))->scale) {
		cb_error (_("unsigned positive integer value expected"));
		(yyval) = cb_int1;
	} else {
		n = cb_get_int ((yyvsp[0]));
		if (n < 1) {
			cb_error (_("unsigned positive integer value expected"));
			(yyval) = cb_int1;
		} else {
			(yyval) = (yyvsp[0]);
		}
	}
  }
#line 21204 "parser.c" /* yacc.c:1646  */
    break;

  case 2019:
#line 13342 "parser.y" /* yacc.c:1646  */
    {
	int	n;

	if (cb_tree_category ((yyvsp[0])) == CB_CATEGORY_NUMERIC) {
		if (CB_LITERAL ((yyvsp[0]))->sign || CB_LITERAL ((yyvsp[0]))->scale) {
			cb_error (_("integer value expected"));
		} else {
			n = cb_get_int ((yyvsp[0]));
			if (n < 1 || n > 256) {
				cb_error (_("invalid CLASS value"));
			}
		}
	}
	(yyval) = (yyvsp[0]);
  }
#line 21224 "parser.c" /* yacc.c:1646  */
    break;

  case 2020:
#line 13357 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_space; }
#line 21230 "parser.c" /* yacc.c:1646  */
    break;

  case 2021:
#line 13358 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_zero; }
#line 21236 "parser.c" /* yacc.c:1646  */
    break;

  case 2022:
#line 13359 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_quote; }
#line 21242 "parser.c" /* yacc.c:1646  */
    break;

  case 2023:
#line 13360 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_high; }
#line 21248 "parser.c" /* yacc.c:1646  */
    break;

  case 2024:
#line 13361 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_low; }
#line 21254 "parser.c" /* yacc.c:1646  */
    break;

  case 2025:
#line 13362 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_null; }
#line 21260 "parser.c" /* yacc.c:1646  */
    break;

  case 2026:
#line 13367 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 21268 "parser.c" /* yacc.c:1646  */
    break;

  case 2027:
#line 13371 "parser.y" /* yacc.c:1646  */
    {
	struct cb_literal	*l;

	if (CB_LITERAL_P ((yyvsp[0]))) {
		/* We must not alter the original definition */
		l = cobc_parse_malloc (sizeof(struct cb_literal));
		*l = *(CB_LITERAL((yyvsp[0])));
		l->all = 1;
		(yyval) = CB_TREE (l);
	} else {
		(yyval) = (yyvsp[0]);
	}
  }
#line 21286 "parser.c" /* yacc.c:1646  */
    break;

  case 2028:
#line 13388 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 21294 "parser.c" /* yacc.c:1646  */
    break;

  case 2029:
#line 13392 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_concat_literals ((yyvsp[-2]), (yyvsp[0]));
  }
#line 21302 "parser.c" /* yacc.c:1646  */
    break;

  case 2030:
#line 13398 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 21308 "parser.c" /* yacc.c:1646  */
    break;

  case 2031:
#line 13399 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_space; }
#line 21314 "parser.c" /* yacc.c:1646  */
    break;

  case 2032:
#line 13400 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_zero; }
#line 21320 "parser.c" /* yacc.c:1646  */
    break;

  case 2033:
#line 13401 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_quote; }
#line 21326 "parser.c" /* yacc.c:1646  */
    break;

  case 2034:
#line 13402 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_high; }
#line 21332 "parser.c" /* yacc.c:1646  */
    break;

  case 2035:
#line 13403 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_low; }
#line 21338 "parser.c" /* yacc.c:1646  */
    break;

  case 2036:
#line 13404 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_null; }
#line 21344 "parser.c" /* yacc.c:1646  */
    break;

  case 2037:
#line 13411 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_intrinsic ((yyvsp[-1]), NULL, (yyvsp[0]), 0);
  }
#line 21352 "parser.c" /* yacc.c:1646  */
    break;

  case 2038:
#line 13415 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_intrinsic ((yyvsp[-4]), CB_LIST_INIT ((yyvsp[-2])), (yyvsp[0]), 0);
  }
#line 21360 "parser.c" /* yacc.c:1646  */
    break;

  case 2039:
#line 13419 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_intrinsic ((yyvsp[-4]), (yyvsp[-2]), (yyvsp[0]), 0);
  }
#line 21368 "parser.c" /* yacc.c:1646  */
    break;

  case 2040:
#line 13423 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_intrinsic ((yyvsp[-4]), (yyvsp[-2]), (yyvsp[0]), 0);
  }
#line 21376 "parser.c" /* yacc.c:1646  */
    break;

  case 2041:
#line 13427 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_intrinsic ((yyvsp[-3]), (yyvsp[-1]), NULL, 0);
  }
#line 21384 "parser.c" /* yacc.c:1646  */
    break;

  case 2042:
#line 13431 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING (_("PHYSICAL argument for LENGTH functions"));
	(yyval) = cb_build_intrinsic ((yyvsp[-4]), (yyvsp[-2]), NULL, 0);
  }
#line 21393 "parser.c" /* yacc.c:1646  */
    break;

  case 2043:
#line 13436 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_intrinsic ((yyvsp[-3]), (yyvsp[-1]), NULL, 0);
  }
#line 21401 "parser.c" /* yacc.c:1646  */
    break;

  case 2044:
#line 13440 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_intrinsic ((yyvsp[-4]), (yyvsp[-2]), (yyvsp[0]), 0);
  }
#line 21409 "parser.c" /* yacc.c:1646  */
    break;

  case 2045:
#line 13444 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_intrinsic ((yyvsp[-4]), (yyvsp[-2]), (yyvsp[0]), 0);
  }
#line 21417 "parser.c" /* yacc.c:1646  */
    break;

  case 2046:
#line 13448 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_intrinsic ((yyvsp[-4]), (yyvsp[-2]), (yyvsp[0]), 0);
  }
#line 21425 "parser.c" /* yacc.c:1646  */
    break;

  case 2047:
#line 13452 "parser.y" /* yacc.c:1646  */
    {
	  (yyval) = cb_build_intrinsic ((yyvsp[-4]), (yyvsp[-2]), (yyvsp[0]), 0);
  }
#line 21433 "parser.c" /* yacc.c:1646  */
    break;

  case 2048:
#line 13456 "parser.y" /* yacc.c:1646  */
    {
	  (yyval) = cb_build_intrinsic ((yyvsp[-4]), (yyvsp[-2]), (yyvsp[0]), 0);
  }
#line 21441 "parser.c" /* yacc.c:1646  */
    break;

  case 2049:
#line 13460 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_intrinsic ((yyvsp[-1]), (yyvsp[0]), NULL, 0);
  }
#line 21449 "parser.c" /* yacc.c:1646  */
    break;

  case 2050:
#line 13464 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_intrinsic ((yyvsp[-1]), (yyvsp[0]), NULL, 1);
  }
#line 21457 "parser.c" /* yacc.c:1646  */
    break;

  case 2060:
#line 13489 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 21465 "parser.c" /* yacc.c:1646  */
    break;

  case 2061:
#line 13493 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_PAIR ((yyvsp[-2]), NULL);
  }
#line 21473 "parser.c" /* yacc.c:1646  */
    break;

  case 2062:
#line 13497 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_PAIR ((yyvsp[-3]), (yyvsp[-1]));
  }
#line 21481 "parser.c" /* yacc.c:1646  */
    break;

  case 2063:
#line 13504 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 21489 "parser.c" /* yacc.c:1646  */
    break;

  case 2064:
#line 13508 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-1]);
  }
#line 21497 "parser.c" /* yacc.c:1646  */
    break;

  case 2065:
#line 13512 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 21505 "parser.c" /* yacc.c:1646  */
    break;

  case 2066:
#line 13519 "parser.y" /* yacc.c:1646  */
    {
	cb_tree	x;

	x = CB_LIST_INIT ((yyvsp[0]));
	(yyval) = cb_list_add (x, cb_int0);
  }
#line 21516 "parser.c" /* yacc.c:1646  */
    break;

  case 2067:
#line 13526 "parser.y" /* yacc.c:1646  */
    {
	cb_tree	x;

	x = CB_LIST_INIT ((yyvsp[-2]));
	(yyval) = cb_list_add (x, cb_int1);
  }
#line 21527 "parser.c" /* yacc.c:1646  */
    break;

  case 2068:
#line 13533 "parser.y" /* yacc.c:1646  */
    {
	cb_tree	x;

	x = CB_LIST_INIT ((yyvsp[-2]));
	(yyval) = cb_list_add (x, cb_int2);
  }
#line 21538 "parser.c" /* yacc.c:1646  */
    break;

  case 2069:
#line 13542 "parser.y" /* yacc.c:1646  */
    {
	suppress_data_exceptions = 1;
  }
#line 21546 "parser.c" /* yacc.c:1646  */
    break;

  case 2070:
#line 13546 "parser.y" /* yacc.c:1646  */
    {
	suppress_data_exceptions = 0;
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
  }
#line 21555 "parser.c" /* yacc.c:1646  */
    break;

  case 2071:
#line 13554 "parser.y" /* yacc.c:1646  */
    {
	cb_tree	x;

	x = CB_LIST_INIT ((yyvsp[0]));
	(yyval) = cb_list_add (x, cb_null);
  }
#line 21566 "parser.c" /* yacc.c:1646  */
    break;

  case 2072:
#line 13561 "parser.y" /* yacc.c:1646  */
    {
	cb_tree	x;

	x = CB_LIST_INIT ((yyvsp[-2]));
	(yyval) = cb_list_add (x, (yyvsp[0]));
  }
#line 21577 "parser.c" /* yacc.c:1646  */
    break;

  case 2073:
#line 13571 "parser.y" /* yacc.c:1646  */
    {
	cb_tree	x;

	x = CB_LIST_INIT ((yyvsp[0]));
	(yyval) = cb_list_add (x, cb_null);
  }
#line 21588 "parser.c" /* yacc.c:1646  */
    break;

  case 2074:
#line 13578 "parser.y" /* yacc.c:1646  */
    {
	cb_tree	x;

	x = CB_LIST_INIT ((yyvsp[-2]));
	(yyval) = cb_list_add (x, cb_ref ((yyvsp[0])));
  }
#line 21599 "parser.c" /* yacc.c:1646  */
    break;

  case 2075:
#line 13588 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[0]), cb_int0);
  }
#line 21607 "parser.c" /* yacc.c:1646  */
    break;

  case 2076:
#line 13592 "parser.y" /* yacc.c:1646  */
    {
	const int	num_args = cb_list_length ((yyvsp[-2]));

	if (num_args == 4) {
		cb_error_x ((yyvsp[-2]), _("cannot specify offset and SYSTEM-OFFSET at the same time"));
	}

	(yyval) = cb_list_add ((yyvsp[-2]), cb_int1);
  }
#line 21621 "parser.c" /* yacc.c:1646  */
    break;

  case 2077:
#line 13605 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[0]), cb_int0);
  }
#line 21629 "parser.c" /* yacc.c:1646  */
    break;

  case 2078:
#line 13609 "parser.y" /* yacc.c:1646  */
    {
	const int	num_args = cb_list_length ((yyvsp[-2]));

	if (num_args == 3) {
		cb_error_x ((yyvsp[-2]), _("cannot specify offset and SYSTEM-OFFSET at the same time"));
	}

	(yyval) = cb_list_add ((yyvsp[-2]), cb_int1);
  }
#line 21643 "parser.c" /* yacc.c:1646  */
    break;

  case 2079:
#line 13623 "parser.y" /* yacc.c:1646  */
    {
	non_const_word = 1;
  }
#line 21651 "parser.c" /* yacc.c:1646  */
    break;

  case 2080:
#line 13631 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int0; }
#line 21657 "parser.c" /* yacc.c:1646  */
    break;

  case 2081:
#line 13632 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 21663 "parser.c" /* yacc.c:1646  */
    break;

  case 2082:
#line 13636 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int0; }
#line 21669 "parser.c" /* yacc.c:1646  */
    break;

  case 2083:
#line 13637 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 21675 "parser.c" /* yacc.c:1646  */
    break;

  case 2084:
#line 13641 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 21681 "parser.c" /* yacc.c:1646  */
    break;

  case 2085:
#line 13642 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 21687 "parser.c" /* yacc.c:1646  */
    break;

  case 2086:
#line 13647 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 21695 "parser.c" /* yacc.c:1646  */
    break;

  case 2087:
#line 13651 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 21703 "parser.c" /* yacc.c:1646  */
    break;

  case 2088:
#line 13658 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 21711 "parser.c" /* yacc.c:1646  */
    break;

  case 2089:
#line 13662 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 21719 "parser.c" /* yacc.c:1646  */
    break;

  case 2090:
#line 13669 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int0; }
#line 21725 "parser.c" /* yacc.c:1646  */
    break;

  case 2091:
#line 13670 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 21731 "parser.c" /* yacc.c:1646  */
    break;

  case 2092:
#line 13671 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int2; }
#line 21737 "parser.c" /* yacc.c:1646  */
    break;

  case 2093:
#line 13675 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 21743 "parser.c" /* yacc.c:1646  */
    break;

  case 2094:
#line 13676 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_true; }
#line 21749 "parser.c" /* yacc.c:1646  */
    break;

  case 2095:
#line 13680 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (cb_flag_optional_file); }
#line 21755 "parser.c" /* yacc.c:1646  */
    break;

  case 2096:
#line 13681 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 21761 "parser.c" /* yacc.c:1646  */
    break;

  case 2097:
#line 13682 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int0; }
#line 21767 "parser.c" /* yacc.c:1646  */
    break;

  case 2098:
#line 13687 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int0;
  }
#line 21775 "parser.c" /* yacc.c:1646  */
    break;

  case 2099:
#line 13691 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		(yyval) = (yyvsp[0]);
	} else {
		(yyval) = default_rounded_mode;
	}
	cobc_cs_check = 0;
  }
#line 21788 "parser.c" /* yacc.c:1646  */
    break;

  case 2100:
#line 13703 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
	cobc_cs_check = 0;
  }
#line 21797 "parser.c" /* yacc.c:1646  */
    break;

  case 2101:
#line 13708 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
	cobc_cs_check = 0;
  }
#line 21806 "parser.c" /* yacc.c:1646  */
    break;

  case 2102:
#line 13716 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_AWAY_FROM_ZERO);
  }
#line 21814 "parser.c" /* yacc.c:1646  */
    break;

  case 2103:
#line 13720 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_NEAR_AWAY_FROM_ZERO);
  }
#line 21822 "parser.c" /* yacc.c:1646  */
    break;

  case 2104:
#line 13724 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_NEAR_EVEN);
  }
#line 21830 "parser.c" /* yacc.c:1646  */
    break;

  case 2105:
#line 13728 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_NEAR_TOWARD_ZERO);
  }
#line 21838 "parser.c" /* yacc.c:1646  */
    break;

  case 2106:
#line 13732 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_PROHIBITED);
  }
#line 21846 "parser.c" /* yacc.c:1646  */
    break;

  case 2107:
#line 13736 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_TOWARD_GREATER);
  }
#line 21854 "parser.c" /* yacc.c:1646  */
    break;

  case 2108:
#line 13740 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_TOWARD_LESSER);
  }
#line 21862 "parser.c" /* yacc.c:1646  */
    break;

  case 2109:
#line 13744 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_TRUNCATION);
  }
#line 21870 "parser.c" /* yacc.c:1646  */
    break;

  case 2110:
#line 13750 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 21876 "parser.c" /* yacc.c:1646  */
    break;

  case 2111:
#line 13751 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 21882 "parser.c" /* yacc.c:1646  */
    break;

  case 2112:
#line 13755 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 21888 "parser.c" /* yacc.c:1646  */
    break;

  case 2113:
#line 13757 "parser.y" /* yacc.c:1646  */
    {
	cb_tree	x;

	x = CB_LIST_INIT ((yyvsp[-3]));
	(yyval) = cb_list_add (x, (yyvsp[-1]));
  }
#line 21899 "parser.c" /* yacc.c:1646  */
    break;

  case 2114:
#line 13766 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 21905 "parser.c" /* yacc.c:1646  */
    break;

  case 2115:
#line 13768 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 21913 "parser.c" /* yacc.c:1646  */
    break;

  case 2116:
#line 13777 "parser.y" /* yacc.c:1646  */
    {
	cobc_repeat_last_token = 1;
  }
#line 21921 "parser.c" /* yacc.c:1646  */
    break;

  case 2117:
#line 13781 "parser.y" /* yacc.c:1646  */
    {
	cobc_repeat_last_token = 1;
  }
#line 21929 "parser.c" /* yacc.c:1646  */
    break;

  case 2118:
#line 13785 "parser.y" /* yacc.c:1646  */
    {
	cobc_repeat_last_token = 0;
  }
#line 21937 "parser.c" /* yacc.c:1646  */
    break;

  case 2119:
#line 13789 "parser.y" /* yacc.c:1646  */
    {
	cobc_repeat_last_token = 0;
  }
#line 21945 "parser.c" /* yacc.c:1646  */
    break;


#line 21949 "parser.c" /* yacc.c:1646  */
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 13974 "parser.y" /* yacc.c:1906  */

