/*
   Copyright (C) 2007-2012, 2014-2017 Free Software Foundation, Inc.
   Written by Roger While, Simon Sobisch, Ron Norman

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


#ifndef COB_LOCAL_H
#define COB_LOCAL_H

#ifdef	HAVE_STRINGS_H
#include <strings.h>
#endif

/* We use this file to define/prototype things that should not be
   exported to user space
*/

#ifdef HAVE_ISFINITE
#define ISFINITE isfinite
#else
#define ISFINITE finite
#endif
#if	defined(_MSC_VER) || defined(__BORLANDC__) || defined(__WATCOMC__)

#include <float.h>
#undef ISFINITE
#define	ISFINITE		_finite
#endif


#if	defined(ENABLE_NLS) && defined(COB_NLS_RUNTIME)
#include "lib/gettext.h"
#define _(s)		gettext(s)
#define N_(s)		gettext_noop(s)
#else
#define _(s)		s
#define N_(s)		s
#endif


#if	defined(_WIN32) || defined(__CYGWIN__)
#define COB_HIDDEN	extern
#elif	defined(__GNUC__) && __GNUC__ >= 4
/* Also OK for icc which defines __GNUC__ */
#define COB_HIDDEN	extern __attribute__ ((visibility("hidden")))
#elif	defined(__SUNPRO_C) && (__SUNPRO_C >= 0x550)
/* Note - >= 0x590 supports gcc syntax */
#define COB_HIDDEN	extern __hidden
#else
#define COB_HIDDEN	extern
#endif

#ifndef	F_OK
#define	F_OK		0
#endif

#ifndef	X_OK
#define	X_OK		1
#endif

#ifndef	W_OK
#define	W_OK		2
#endif

#ifndef	R_OK
#define	R_OK		4
#endif

/* Stacked field depth */
#define COB_DEPTH_LEVEL		32U

/* Not-A-Number */
#define COB_DECIMAL_NAN		-32768

/* Infinity */
#define COB_DECIMAL_INF		-32767

/* GMP decimal default */
#define COB_MPZ_DEF		1024UL

/* GMP floating precision */
#define COB_MPF_PREC		2048UL

/* Complex calculation cutoff value */
/* This MUST be <= COB_MPF_PREC */
#define COB_MPF_CUTOFF		1024UL


/* Floating-decimal */
#ifdef	WORDS_BIGENDIAN
#define	COB_128_MSW(x)		x[0]
#define	COB_128_LSW(x)		x[1]
#define	COB_MPZ_ENDIAN		1
#else
#define	COB_128_MSW(x)		x[1]
#define	COB_128_LSW(x)		x[0]
#define	COB_MPZ_ENDIAN		-1
#endif

/* Mask for inf/nan */
#define	COB_DEC_SPECIAL		COB_U64_C(0x7800000000000000)
/* Mask for extended */
#define	COB_DEC_EXTEND		COB_U64_C(0x6000000000000000)
/* Mask for sign */
#define	COB_DEC_SIGN		COB_U64_C(0x8000000000000000)

#define	COB_64_IS_SPECIAL(x)	((x & COB_DEC_SPECIAL) == COB_DEC_SPECIAL)
#define	COB_128_IS_SPECIAL(x)	\
	((COB_128_MSW(x) & COB_DEC_SPECIAL) == COB_DEC_SPECIAL)
#define	COB_64_IS_EXTEND(x)	((x & COB_DEC_EXTEND) == COB_DEC_EXTEND)
#define	COB_128_IS_EXTEND(x)	\
	((COB_128_MSW(x) & COB_DEC_EXTEND) == COB_DEC_EXTEND)

/* Exponent 1 - 10 bits after sign bit */
#define	COB_64_EXPO_1		COB_U64_C(0x7FE0000000000000)
/* Significand 1 */
#define	COB_64_SIGF_1		COB_U64_C(0x001FFFFFFFFFFFFF)
/* Exponent 2 - 10 bits after (sign bit + 2) */
#define	COB_64_EXPO_2		COB_U64_C(0x1FF8000000000000)
/* Significand 2 */
#define	COB_64_SIGF_2		COB_U64_C(0x0007FFFFFFFFFFFF)
/* Extended or bit */
#define	COB_64_OR_EXTEND	COB_U64_C(0x0020000000000000)

/* Exponent 1 - 14 bits after sign bit */
#define	COB_128_EXPO_1		COB_U64_C(0x7FFE000000000000)
/* Significand 1 */
#define	COB_128_SIGF_1		COB_U64_C(0x0001FFFFFFFFFFFF)
/* Exponent 2 - 14 bits after (sign bit + 2) */
#define	COB_128_EXPO_2		COB_U64_C(0x1FFF800000000000)
/* Significand 2 */
#define	COB_128_SIGF_2		COB_U64_C(0x00007FFFFFFFFFFF)
/* Extended or bit */
#define	COB_128_OR_EXTEND	COB_U64_C(0x0002000000000000)

/* Field/attribute initializers */
#define COB_FIELD_INIT(x,y,z)	do { \
	field.size = x; \
	field.data = y; \
	field.attr = z; \
	} ONCE_COB

#define COB_ATTR_INIT(u,v,x,y,z)	do { \
	attr.type = u; \
	attr.digits = v; \
	attr.scale = x; \
	attr.flags = y; \
	attr.pic = z; \
	} ONCE_COB

#define COB_GET_SIGN(f)		\
	(COB_FIELD_HAVE_SIGN (f) ? cob_real_get_sign (f) : 0)
#define COB_PUT_SIGN(f,s)	\
	do { if (COB_FIELD_HAVE_SIGN (f)) cob_real_put_sign (f, s); } ONCE_COB

#ifdef	COB_PARAM_CHECK
#define	COB_CHK_PARMS(x,z)	\
	cob_parameter_check (#x, z)
#else
#define	COB_CHK_PARMS(x,z)
#endif

/* byte offset to structure member */
#if !defined(_OFFSET_OF_) && !defined(offsetof)
#define _OFFSET_OF_
#define offsetof(s_name,m_name) (int)(long)&(((s_name*)0))->m_name
#endif

/* Convert a digit (e.g., '0') into an integer (e.g., 0) */
#define COB_D2I(x)		((x) & 0x0F)
#if	0	/* RXWRXW - D2I */
#define COB_D2I(x)		((x) - '0')
#endif

/* Convert an integer (e.g., 0) into a digit (e.g., '0') */
#define COB_I2D(x)		((x) + '0')

#define	COB_MODULE_PTR		cobglobptr->cob_current_module
#define	COB_TERM_BUFF		cobglobptr->cob_term_buff
#define	COB_ACCEPT_STATUS	cobglobptr->cob_accept_status
#define	COB_MAX_Y_COORD		cobglobptr->cob_max_y
#define	COB_MAX_X_COORD		cobglobptr->cob_max_x

#define	COB_DISP_TO_STDERR	cobsetptr->cob_disp_to_stderr
#define	COB_BEEP_VALUE		cobsetptr->cob_beep_value
#define	COB_TIMEOUT_SCALE	cobsetptr->cob_timeout_scale
#define	COB_EXTENDED_STATUS	cobsetptr->cob_extended_status
#define	COB_USE_ESC		cobsetptr->cob_use_esc

#ifdef __cplusplus
extern "C" {
#endif

/* Global settings structure */

typedef struct __cob_settings {
	unsigned int	cob_display_warn;	/* Display warnings */
	unsigned int	cob_env_mangle;		/* Mangle env names */
	unsigned int	cob_line_trace;	
	unsigned int	cob_config_cur;		/* Current runtime.cfg file being processed */
	unsigned int	cob_config_num;		/* Number of different runtime.cfg files read */
	char		**cob_config_file;	/* Keep all file names for later reporting */
	char		*cob_trace_filename;
	char		*cob_user_name;
	char		*cob_sys_lang;		/* LANG setting from env */
	char		*cob_sys_term;		/* TERM setting from env */
	char		*cob_sys_type;		/* OSTYPE setting from env */
	char		*cob_debug_log;
	char		*cob_date;		/* Date override for testing purposes / UTC hint */
	struct cob_time	cob_time_constant;

	/* call.c */
	unsigned int	cob_physical_cancel;
	unsigned int	name_convert;
	char		*cob_preload_str;
	char		*cob_library_path;
	char		*cob_preload_str_set;

	size_t		*resolve_size;	/* Array size of resolve_path*/
	char		*cob_preload_resolved;
	char		*cob_preload_env;

	/* fileio.c */
	unsigned int	cob_unix_lf;		/* Use POSIX LF */
	unsigned int	cob_do_sync;
	unsigned int	cob_ls_uses_cr;
	unsigned int	cob_ls_nulls;
	unsigned int	cob_ls_fixed;
	unsigned int	cob_varseq_type;
	char 		*cob_file_path;
	char		*bdb_home;
	size_t		cob_sort_memory;
	size_t		cob_sort_chunk;

	/* move.c */
	unsigned int	cob_local_edit;

	/* screenio.c */
	unsigned int 	cob_legacy;
	unsigned int	cob_disp_to_stderr;	/* Redirect to stderr */
	unsigned int	cob_beep_value;		/* Bell disposition */
	unsigned int	cob_extended_status;	/* Extended status */
	unsigned int	cob_use_esc;		/* Check ESC key */
	unsigned int	cob_timeout_scale;	/* timeout scale */
	unsigned int	cob_insert_mode;	/* insert toggle, 0=off, 1=on */
	unsigned int	cob_exit_wait;		/* wait on program exit if no ACCEPT came after last DISPLAY */
	char			*cob_exit_msg;		/* message for cob_exit_wait */

} cob_settings;


struct config_enum {
	const char	*match;			/* Alternate word that could be used */
	const char	*value;			/* Internal value for this 'word' */
};

/* Format of table for capturing run-time config information */
struct config_tbl {
	const char	*env_name;		/* Env Var name */
	const char	*conf_name;		/* Name used in run-time config file */
	const char	*default_val;		/* Default value */
	struct config_enum *enums;		/* Table of Alternate values */
	unsigned int		env_group;		/* Grouping for display of run-time options */
	unsigned int		data_type;		/* Data type */
	unsigned int		data_loc;		/* Location within structure */
	unsigned int		data_len;		/* Length of referenced field */
	unsigned int		config_num;		/* Set by which runtime.cfg file */
	unsigned int		set_by;			/* value set by a different keyword */
	unsigned long		min_value;		/* Minimum accepted value */
	unsigned long		max_value;		/* Maximum accepted value */
};

#define ENV_NOT		(1 << 1)		/* Negate True/False value setting */
#define ENV_INT		(1 << 2)		/* an 'int' */
#define ENV_SIZE	(1 << 3)		/* size; number with K - kb, M - mb, G - GB */
#define ENV_BOOL	(1 << 4)		/* int boolean; Yes, True, 1, No, False, 0, ... */
#define ENV_CHAR	(1 << 5)		/* inline 'char[]' field */
#define ENV_STR		(1 << 6)		/* a pointer to a string */
#define ENV_PATH	(1 << 7)		/* a pointer to one or more file system paths [fp1:fp2:fp3] */
#define ENV_ENUM	(1 << 8)		/* Value must in 'enum' list as match */
#define ENV_ENUMVAL	(1 << 9)		/* Value must in 'enum' list as match or value */
#define ENV_FILE 	(1 << 10)		/* a pointer to a directory/file [single path] */

#define STS_ENVSET	(1 << 15)		/* value set via Env Var */
#define STS_CNFSET	(1 << 16)		/* value set via config file */
#define STS_ENVCLR	(1 << 17)		/* value removed from Env Var */
#define STS_RESET	(1 << 18)		/* value was reset back to default */
#define STS_FNCSET	(1 << 19)		/* value set via function call */

#define GRP_HIDE	0
#define GRP_CALL	1
#define GRP_FILE	2
#define GRP_SCREEN	3
#define GRP_MISC	4
#define GRP_SYSENV	5
#define GRP_MAX 	6

#define SETPOS(member)	offsetof(cob_settings,member),sizeof(cobsetptr->member),0,0


/* Local function prototypes */
COB_HIDDEN void		cob_init_numeric	(cob_global *);
COB_HIDDEN void		cob_init_termio		(cob_global *, cob_settings *);
COB_HIDDEN void		cob_init_fileio		(cob_global *, cob_settings *);
COB_HIDDEN void		cob_init_call		(cob_global *, cob_settings *);
COB_HIDDEN void		cob_init_intrinsic	(cob_global *);
COB_HIDDEN void		cob_init_strings	(void);
COB_HIDDEN void		cob_init_move		(cob_global *, cob_settings *);
COB_HIDDEN void		cob_init_screenio	(cob_global *, cob_settings *);

COB_HIDDEN void		cob_exit_screen		(void);

COB_HIDDEN void		cob_exit_numeric	(void);
COB_HIDDEN void		cob_exit_fileio		(void);
COB_HIDDEN void		cob_exit_call		(void);
COB_HIDDEN void		cob_exit_intrinsic	(void);
COB_HIDDEN void		cob_exit_strings	(void);

COB_HIDDEN int		cob_real_get_sign	(cob_field *);
COB_HIDDEN void		cob_real_put_sign	(cob_field *, const int);

COB_HIDDEN void		cob_decimal_setget_fld	(cob_field *, cob_field *,
						 const int);
COB_HIDDEN void		cob_decimal_move_temp	(cob_field *, cob_field *);
COB_HIDDEN void		cob_print_ieeedec	(const cob_field *, FILE *);
COB_HIDDEN void		cob_print_realbin	(const cob_field *, FILE *,
						 const int);

COB_HIDDEN void		cob_screen_set_mode	(const cob_u32_t);
COB_HIDDEN int		cob_get_exception_code	(void);
COB_HIDDEN int		cob_check_env_true	(char*);
COB_HIDDEN int		cob_check_env_false	(char*);
COB_HIDDEN const char	*cob_get_exception_name	(void);
COB_HIDDEN void		cob_field_to_string	(const cob_field *, void *,
						 const size_t);
COB_HIDDEN void		cob_parameter_check	(const char *, const int);
COB_HIDDEN void		cob_runtime_error	(const char *, ...) COB_A_FORMAT12;
COB_HIDDEN void		cob_runtime_warning	(const char *, ...) COB_A_FORMAT12;

COB_HIDDEN char*	cob_save_env_value	(char*, char*);
COB_HIDDEN cob_settings *cob_get_settings_ptr	(void);

COB_HIDDEN int		cob_ctoi		(const char);

#if 0 /* currently not used */
COB_HIDDEN char		*cob_int_to_string		(int, char*);
COB_HIDDEN char		*cob_int_to_formatted_bytestring	(int, char*);
#endif
COB_HIDDEN char		*cob_strcat		(char*, char*, int);
COB_HIDDEN char		*cob_strjoin		(char**, int, char*);

COB_HIDDEN int		cob_min_int		(const int, const int);
COB_HIDDEN int		cob_max_int		(const int, const int);

#ifdef __cplusplus
}
#endif

#undef	COB_HIDDEN

#endif	/* COB_LOCAL_H */
