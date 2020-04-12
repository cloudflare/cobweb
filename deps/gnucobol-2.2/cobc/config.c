/*
   Copyright (C) 2003-2012, 2014-2017 Free Software Foundation, Inc.
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
#include "defaults.h"

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>

#include "cobc.h"

enum cb_config_type {
	CB_ANY = 0,
	CB_INT,			/* integer */
	CB_STRING,		/* "..." */
	CB_BOOLEAN,		/* 'yes', 'no' */
	CB_SUPPORT		/* 'ok', 'archaic', 'obsolete',
				   'skip', 'ignore', 'unconformable' */
};

/* Global variables */

#define CB_CONFIG_ANY(type,var,name,doc)	type		var = (type)0;
#define CB_CONFIG_INT(var,name,min,max,odoc,doc)	unsigned int		var = 0;
#define CB_CONFIG_STRING(var,name,doc)	const char	*var = NULL;
#define CB_CONFIG_BOOLEAN(var,name,doc)	int		var = 0;
#define CB_CONFIG_SUPPORT(var,name,doc)	enum cb_support	var = CB_OK;

#include "config.def"

#undef	CB_CONFIG_ANY
#undef	CB_CONFIG_INT
#undef	CB_CONFIG_STRING
#undef	CB_CONFIG_BOOLEAN
#undef	CB_CONFIG_SUPPORT

#define CB_CONFIG_ANY(type,var,name,doc)	, {CB_ANY, name, (void *)&var}
#define CB_CONFIG_INT(var,name,min,max,odoc,doc)	, {CB_INT, name, (void *)&var, NULL, min, max}
#define CB_CONFIG_STRING(var,name,doc)	, {CB_STRING, name, (void *)&var}
#define CB_CONFIG_BOOLEAN(var,name,doc)	, {CB_BOOLEAN, name, (void *)&var}
#define CB_CONFIG_SUPPORT(var,name,doc)	, {CB_SUPPORT, name, (void *)&var}

/* Local variables */

static struct config_struct {
	const enum cb_config_type	type;
	const char			*name;		/* Print name set in compiler configuration */
	void				*var;		/* Var name */
	char				*val;		/* value from configuration / command line */
#if 0 /* Currently not used */
	const int			doc;		/* documented, 1 = yes */
#endif
	int					min_value;		/* Minimum accepted value */
	long				max_value;		/* Maximum accepted value */

} config_table[] = {
	{CB_STRING, "include"},
	{CB_STRING, "includeif"},
	{CB_STRING, "not-reserved"},
	{CB_STRING, "reserved"},
	{CB_STRING, "not-intrinsic-function"},
	{CB_STRING, "intrinsic-function"},
	{CB_STRING, "not-system-name"},
	{CB_STRING, "system-name"},
	{CB_STRING, "not-register"},
	{CB_STRING, "register"}
#include "config.def"
};

#undef	CB_CONFIG_ANY
#undef	CB_CONFIG_INT
#undef	CB_CONFIG_STRING
#undef	CB_CONFIG_BOOLEAN
#undef	CB_CONFIG_SUPPORT

#define	CB_CONFIG_SIZE	sizeof(config_table) / sizeof(struct config_struct)

/* Configuration includes */
static struct includelist {
	struct includelist	*next;
	const char		*name;
} *conf_includes = NULL;

/* type of include */
enum cb_include_type {
	CB_INCLUDE_MANDATORY = 0,
	CB_INCLUDE_OPTIONAL,
	CB_INCLUDE_RESOLVE_WORDS
};

const char	*words_file = NULL;

/* Local declarations */

static int cb_read_conf (const char *, FILE *);

/* Local functions */

static char *
read_string (char *text)
{
	char	*s;

	if (*text == '\"') {
		text++;
	}
	s = cobc_main_strdup (text);
	for (text = s; *text; text++) {
		if (*text == '\"') {
			*text = '\0';
		}
	}
	return s;
}

static void
invalid_value (const char *fname, const int line, const char *name, const char *val,
	       const char *str, const int min, const long max)
{
	configuration_error (fname, line, 0,
		_("invalid value '%s' for configuration tag '%s'"), val, name);
	if (str) {
		configuration_error (fname, line, 1,
			_("should be one of the following values: %s"), str);
	} else if (max == min && max == 0) {
		configuration_error (fname, line, 1, _("must be numeric"));
	} else if (max) {
		configuration_error (fname, line, 1, _("maximum value: %lu"), (unsigned long)max);
	} else {
		configuration_error (fname, line, 1, _("minimum value: %d"), min);
	}
}

static int
check_valid_value (const char *fname, const int line, const char *name, const char *val,
		   const void *var, const int min_value, const long max_value)
{
	int ret = 1;
	long v;

	v = atol (val);

	if (v < min_value) {
		invalid_value (fname, line, name, val, NULL, min_value, 0);
		ret = 0;
	}
	if (v > max_value) {
		invalid_value (fname, line, name, val, NULL, 0, max_value);
		ret = 0;
	}
	if (ret) {
		*((int *)var) = v;
	}
	return ret;
}

static void
unsupported_value (const char *fname, const int line, const char *name, const char *val)
{
	configuration_error (fname, line, 1,
		_("unsupported value '%s' for configuration tag '%s'"), val, name);
}

static void
split_and_iterate_on_comma_separated_str (
	void (* const func)(const char *, const char *, const int),
	const int transform_case, const int replace_colons,
	const char *val, const char *fname, const int line)
{
	unsigned int	i;
	unsigned int	j = 0;
	char	word_buff[COB_MINI_BUFF];

	for (i = 0; val[i] && j < COB_MINI_MAX; i++) {
		/* note: we actually want spaces in,
		   especially for mnemonics "SWITCH A" and registers "LENGTH OF"
		*/
		switch (val[i]) {
		case ' ':
			/* Remove spaces if not escaped, espacially needed for
			   mnemonics "SWITCH A" and registers "LENGTH OF" */
			if (j > 0 && word_buff[j - 1] == '\\') {
				word_buff[j - 1] = ' ';
			}
			break;
		case '\t':
			/* Tabs are always removed. */
			break;
		case ',':
			word_buff[j] = 0;
			(*func) (word_buff, fname, line);
			memset (word_buff, 0, COB_MINI_BUFF);
			j = 0;
			break;
		case ':':
			if (replace_colons) {
				word_buff[j++] = '=';
				break;
			}
		default:
			if (transform_case == 1) {
				word_buff[j++] = (char)toupper ((int)val[i]);
			} else if (transform_case == 2) {
				word_buff[j++] = (char)tolower ((int)val[i]);
			} else {;
				word_buff[j++] = val[i];
			}
			break;
		}
	}
	word_buff[j] = 0;

	if (j != 0) {
		(*func) (word_buff, fname, line);
	}
}

static int
cb_load_conf_file (const char *conf_file, const enum cb_include_type include_type)
{
	FILE	*fp;
	char	buff[COB_SMALL_BUFF];
	char	filename[COB_NORMAL_BUFF];
	struct includelist	*c, *cc;
	int	i, ret;

	for (i = 0; conf_file[i] != 0 && conf_file[i] != SLASH_CHAR; i++);
	if (conf_file[i] == 0) {			/* Just a name, No directory */
		if (access(conf_file, F_OK) != 0) {	/* and file does not exist */
			/* check for path of previous configuration file (for includes) */
			c = conf_includes;
			if (c) {
				while (c->next != NULL) {
					c = c->next;
				}
			}
			filename[0] = 0;
			if (c && c->name) {
				strcpy(buff, conf_includes->name);
				for (i = (int)strlen(buff); i != 0 && buff[i] != SLASH_CHAR; i--);
				if (i != 0) {
					buff[i] = 0;
					snprintf (filename, (size_t)COB_NORMAL_MAX, "%s%c%s", buff, SLASH_CHAR, conf_file);
					filename[COB_NORMAL_MAX] = 0;
					if (access (filename, F_OK) == 0) {	/* and prefixed file exist */
						conf_file = filename;		/* Prefix last directory */
					} else {
						filename[0] = 0;
					}
				}
			}
			if (filename[0] == 0) {
				/* check for COB_CONFIG_DIR (use default if not in environment) */
				snprintf (filename, (size_t)COB_NORMAL_MAX, "%s%c%s", cob_config_dir, SLASH_CHAR, conf_file);
				filename[COB_NORMAL_MAX] = 0;
				if (access (filename, F_OK) == 0) {	/* and prefixed file exist */
					conf_file = filename;		/* Prefix COB_CONFIG_DIR */
				}
			}
		}
	}

	/* check for recursion */
	c = cc = conf_includes;
	while (c != NULL) {
		if (c->name /* <- silence warnings */ && strcmp(c->name, conf_file) == 0) {
			configuration_error (conf_file, 0, 1, _("recursive inclusion"));
			return -2;
		}
		cc = c;
		c = c->next;
	}

	/* Special "check only" type */
	if (include_type == CB_INCLUDE_RESOLVE_WORDS) {
		words_file = cobc_main_strdup (conf_file);
		return access (words_file, F_OK);
	}

	/* Open the configuration file */
	fp = fopen (conf_file, "r");
	if (fp == NULL) {
		if (include_type != CB_INCLUDE_OPTIONAL) {
			cb_perror (1, "%s: %s", conf_file, cb_get_strerror ());
			return -1;
		} else {
			return 0;
		}
	}

	/* add current entry to list*/
	c = cob_malloc (sizeof (struct includelist));
	c->next = NULL;
	c->name = conf_file;
	if (cc != NULL) {
		cc->next = c;
	} else {
		conf_includes = c;
	}

	/* Read the configuration file */
	ret = cb_read_conf (conf_file, fp);

	fclose (fp);

	/* remove current entry from memory and list*/
	if (cc) {
		cc->next = NULL;
	} else {
		conf_includes = NULL;
	}
	cob_free (c);

	return ret;
}

/* Read the configuration file previously opened */
static int
cb_read_conf (const char *conf_file, FILE *fp)
{
	int			sub_ret, ret;
	int			line;
	char			buff[COB_SMALL_BUFF];
	enum cb_include_type	include_type;

	/* Read the configuration file */
	ret = 0;
	line = 0;
	while (fgets (buff, COB_SMALL_BUFF, fp)) {
		line++;
		sub_ret = cb_config_entry (buff, conf_file, line);
		if (sub_ret == 1 || sub_ret == 3) {
			if (sub_ret == 1) {
				include_type = CB_INCLUDE_MANDATORY;
			} else {
				include_type = CB_INCLUDE_OPTIONAL;
			}
			sub_ret = cb_load_conf_file (buff, include_type);
			if (sub_ret < 0) {
				ret = -1;
				configuration_error (conf_file, line, 1,
						    _("configuration file was included here"));
				break;
			}
		}
		if (sub_ret != 0) ret = sub_ret;
	}
	return ret;
}


/* Global functions */

int
cb_load_std (const char *name)
{
	return cb_load_conf (name, CB_INCLUDE_MANDATORY);
}

int
cb_load_conf (const char *fname, const int prefix_dir)
{
	const char	*name;
	int		ret;
	size_t		i;
	char		buff[COB_NORMAL_BUFF];

	/* Warn if we drop the configuration read already */
	if (unlikely(cb_config_name != NULL)) {
		configuration_warning (fname, 0,
			_("The previous loaded configuration '%s' will be discarded."),
			cb_config_name);
	}

	/* Initialize the configuration table */
	for (i = 0; i < CB_CONFIG_SIZE; i++) {
		config_table[i].val = NULL;
	}

	/* Get the name for the configuration file */
	if (prefix_dir) {
		snprintf (buff, (size_t)COB_NORMAL_MAX,
			  "%s%c%s", cob_config_dir, SLASH_CHAR, fname);
		name = buff;
	} else {
		name = fname;
	}

	ret = cb_load_conf_file (name, CB_INCLUDE_MANDATORY);

	/* Checks for missing definitions */
	if (ret == 0) {
		for (i = 10U; i < CB_CONFIG_SIZE; i++) {
			if (config_table[i].val == NULL) {
				/* as there are likely more than one definition missing group it */
				if (ret == 0) {
					configuration_error (fname, 0, 1, _("missing definitions:"));
				}
				configuration_error (fname, 0, 1, _("\tno definition of '%s'"),
						config_table[i].name);
				ret = -1;
			}
		}
	}

	return ret;
}

int
cb_load_words (void)
{
	FILE	*fp;
	int		ret;

	/* Open the word-list file */
	fp = fopen (words_file, "r");
	if (fp == NULL) {
		cb_perror (1, "%s: %s", words_file, cb_get_strerror ());
		return -1;
	}

	/* Read the word-list file */
	ret = cb_read_conf (words_file, fp);

	fclose (fp);

	return ret;
}

int
cb_config_entry (char *buff, const char *fname, const int line)
{
	char		*s;
	const char	*name;
	char		*e;
	char		*val;
	void		*var;
	enum cb_support	support_val;
	size_t		i;
	size_t		j;

	/* ignore leading white-spaces */
	while (*buff == '\t' || *buff == ' ') {
		buff++;
	}

	/* ignore empty / comment line */
	if (*buff == 0 || *buff == '\r' || *buff == '\n' || *buff == '#') {
		return 0;
	}

	/* get tag */
	s = strpbrk (buff, " \t:=");
	if (!s) {
		/* no tag separator --> error (remove CR LF for message) */
		for (j = strlen(buff); buff[j - 1] == '\r' || buff[j - 1] == '\n';) {
			buff[--j] = 0;
		}
		configuration_error (fname, line, 1,
			_("invalid configuration tag '%s'"), buff);
		return -1;
	}
	*s = 0;

	/* Find entry */
	for (i = 0; i < CB_CONFIG_SIZE; i++) {
		if (strcmp (buff, config_table[i].name) == 0) {
			break;
		}
	}
	if (i == CB_CONFIG_SIZE) {
		configuration_error (fname, line, 1,
			_("unknown configuration tag '%s'"), buff);
		return -1;
	}
#if 0 /* currently not possible (all entries from config.def are included
         --> no gettext for messages here */
	/* if not included in documentation: reject for command line */
	if (!fname && config_table[i].doc == 0) {
		configuration_error (NULL, 0, 1,
			"'%s' cannot be set via command line", config_table[i].name);
		return -1;
	}
#endif

	/* Check for reserved word tag, if requested */
	if (fname == words_file) {
		if (strcmp (buff, "reserved")
		&&  strcmp (buff, "not-reserved")
		&&  strcmp (buff, "intrinsic-function")
		&&  strcmp (buff, "not-intrinsic-function")
		&&  strcmp (buff, "system-name")
		&&  strcmp (buff, "not-system-name")
		&&  strcmp (buff, "register")
		&&  strcmp (buff, "not-register")) {
			configuration_error (fname, line, 1,
				_("invalid configuration tag '%s' in word-list"), buff);
			return -1;
		}
	}

	/* Get value */
	/* Move pointer to beginning of value */
	for (s++; *s && strchr (" \t:=", *s); s++) {
		;
	}
	/* Set end pointer to first # (comment) or end of value */
	for (e = s + 1; *e && !strchr ("#", *e); e++) {
		;
	}
	/* Remove trailing white-spaces */
	for (--e; e >= s && strchr (" \t\r\n", *e); e--) {
		;
	}
	e[1] = 0;

	/* Set value */
	name = config_table[i].name;
	var = config_table[i].var;
	val = s;
	switch (config_table[i].type) {
	case CB_ANY:
		if (strcmp (name, "assign-clause") == 0) {
			if (strcmp (val, "cobol2002") == 0) {
				unsupported_value (fname, line, name, val);
				return -1;
			} else if (strcmp (val, "mf") == 0) {
				cb_assign_clause = CB_ASSIGN_MF;
			} else if (strcmp (val, "ibm") == 0) {
				cb_assign_clause = CB_ASSIGN_IBM;
			} else {
				invalid_value (fname, line, name, val, "cobol2002, mf, ibm", 0, 0);
				return -1;
			}
		} else if (strcmp (name, "binary-size") == 0) {
			if (strcmp (val, "2-4-8") == 0) {
				cb_binary_size = CB_BINARY_SIZE_2_4_8;
			} else if (strcmp (val, "1-2-4-8") == 0) {
				cb_binary_size = CB_BINARY_SIZE_1_2_4_8;
			} else if (strcmp (val, "1--8") == 0) {
				cb_binary_size = CB_BINARY_SIZE_1__8;
			} else {
				invalid_value (fname, line, name, val, "2-4-8, 1-2-4-8, 1--8", 0, 0);
				return -1;
			}
		} else if (strcmp (name, "binary-byteorder") == 0) {
			if (strcmp (val, "native") == 0) {
				cb_binary_byteorder = CB_BYTEORDER_NATIVE;
			} else if (strcmp (val, "big-endian") == 0) {
				cb_binary_byteorder = CB_BYTEORDER_BIG_ENDIAN;
			} else {
				invalid_value (fname, line, name, val, "native, big-endian", 0, 0);
				return -1;
			}
		} else if (strcmp (name, "standard-define") != 0) {
			configuration_error (fname, line, 1, _("Invalid type for '%s'"), name);
			return -1;
		}
		break;
	case CB_INT:
		for (j = 0; val[j]; j++) {
			if (val[j] < '0' || val[j] > '9') {
				invalid_value (fname, line, name, val, NULL, 0, 0);
				return -1;
			}
		}

		if (check_valid_value (fname, line, name, val, var,
				       config_table[i].min_value, config_table[i].max_value)) {
			break;
		} else {
			return -1;
		}
	case CB_STRING:
		val = read_string (val);

		if (strcmp (name, "include") == 0
		||  strcmp (name, "includeif") == 0) {
			/* Include another conf file */
			s = cob_expand_env_string ((char *)val);
			cobc_main_free ((void *) val);
			strncpy (buff, s, COB_SMALL_MAX);
			/* special case: use cob_free (libcob) here as the memory
			   was allocated in cob_expand_env_string -> libcob */
			cob_free (s);
			if (strcmp (name, "includeif") == 0) {
				return 3;
			} else {
				return 1;
			}
		} else if (strcmp (name, "reserved-words") == 0) {
			/* store translated to lower case */
			for (e = (char *)val; *e; e++) {
				if (isupper (*e)) {
					*e = (cob_u8_t)tolower (*e);
				}
			}
			/* if explicit requested: disable */
			if (strcmp (val, "default") == 0
			    || strcmp (val, "off") == 0) {
				*((const char **)var) = NULL;
			} else {
				*((const char **)var) = val;
				snprintf (buff, (size_t)COB_NORMAL_MAX, "%s.words", val);
				/* check if name.words exists and store the resolved name to words_file */
				if (cb_load_conf_file (buff, CB_INCLUDE_RESOLVE_WORDS) != 0) {
					configuration_error (fname, line, 1, _("Could not access word list for '%s'"), val);
					cb_perror (1, "%s: %s", words_file, cb_get_strerror ());
					return -1;
				};
			}
		} else if (strcmp (name, "not-reserved") == 0) {
			split_and_iterate_on_comma_separated_str (&remove_reserved_word, 0, 0, val, fname, line);
			split_and_iterate_on_comma_separated_str (&deactivate_intrinsic, 1, 0, val, fname, line);
			split_and_iterate_on_comma_separated_str (&deactivate_system_name, 1, 0, val, fname, line);
			split_and_iterate_on_comma_separated_str (&remove_register, 1, 0, val, fname, line);
		} else if (strcmp (name, "reserved") == 0) {
			split_and_iterate_on_comma_separated_str (&add_reserved_word, 0, 1, val, fname, line);
		} else if (strcmp (name, "not-intrinsic-function") == 0) {
			split_and_iterate_on_comma_separated_str (&deactivate_intrinsic, 1, 0, val, fname, line);
		} else if (strcmp (name, "intrinsic-function") == 0) {
			split_and_iterate_on_comma_separated_str (&activate_intrinsic, 1, 1, val, fname, line);
		} else if (strcmp (name, "not-system-name") == 0) {
			split_and_iterate_on_comma_separated_str (&deactivate_system_name, 1, 0, val, fname, line);
		} else if (strcmp (name, "system-name") == 0) {
			split_and_iterate_on_comma_separated_str (&activate_system_name, 1, 1, val, fname, line);
		} else if (strcmp (name, "not-register") == 0) {
			split_and_iterate_on_comma_separated_str (&remove_register, 1, 0, val, fname, line);
		} else if (strcmp (name, "register") == 0) {
			split_and_iterate_on_comma_separated_str (&add_register, 1, 1, val, fname, line);
		} else {
			*((const char **)var) = val;
		}

		break;
	case CB_BOOLEAN:
		if (strcmp (val, "yes") == 0) {
			*((int *)var) = 1;
		} else if (strcmp (val, "no") == 0) {
			*((int *)var) = 0;
		} else {
			invalid_value (fname, line, name, val, "yes, no", 0, 0);
			return -1;
		}
		break;
	case CB_SUPPORT:
		/* check if we are in "minimal mode" */
		s = (char *)val;
		if (*s == '+') s++;
		if (strcmp (s, "ok") == 0) {
			support_val = CB_OK;
		} else if (strcmp (s, "warning") == 0) {
			support_val = CB_WARNING;
		} else if (strcmp (s, "archaic") == 0) {
			support_val = CB_ARCHAIC;
		} else if (strcmp (s, "obsolete") == 0) {
			support_val = CB_OBSOLETE;
		} else if (strcmp (s, "skip") == 0) {
			support_val = CB_SKIP;
		} else if (strcmp (s, "ignore") == 0) {
			support_val = CB_IGNORE;
		} else if (strcmp (s, "error") == 0) {
			support_val = CB_ERROR;
		} else if (strcmp (s, "unconformable") == 0) {
			support_val = CB_UNCONFORMABLE;
		} else {
			invalid_value (fname, line, name, s,
				       "ok, warning, archaic, obsolete, skip, ignore, error, unconformable", 0, 0);
			return -1;
		}
		/* handling of special "adjust" mode */
		if (s != val) {
			if (*((enum cb_support *)var) != CB_SKIP
			&&  *((enum cb_support *)var) != CB_IGNORE
			&&  *((enum cb_support *)var) > support_val) {
				*((enum cb_support *)var) = support_val;
			}
			break;
		}
		/* normal handling */
		*((enum cb_support *)var) = support_val;
		break;
	default:
		configuration_error (fname, line, 1, _("invalid type for '%s'"), name);
		return -1;
	}
	/* copy valid entries to config table */
	if (config_table[i].val) {
		cobc_main_free ((void *)config_table[i].val);
	}
	config_table[i].val = cobc_main_strdup (val);
	return 0;
}
