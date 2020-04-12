/*
   Copyright (C) 2002-2012, 2014-2017 Free Software Foundation, Inc.
   Written by Keisuke Nishida, Roger While, Simon Sobisch, Ron Norman

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

#define _LFS64_LARGEFILE		1
#define _LFS64_STDIO			1
#define _FILE_OFFSET_BITS		64
#define _LARGEFILE64_SOURCE		1
#ifdef	_AIX
#define _LARGE_FILES			1
#endif	/* _AIX */
#if defined(__hpux__) && !defined(__LP64__)
#define _APP32_64BIT_OFF_T		1
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef	HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef	HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef	_WIN32

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <direct.h>
#include <io.h>
#ifndef __WATCOMC__
#define	fdcobsync	_commit
#else
#define	fdcobsync	fsync
#endif
#if !defined(__BORLANDC__) && !defined(__WATCOMC__)
#define	getcwd		_getcwd
#define	chdir		_chdir
#define	mkdir		_mkdir
#define	rmdir		_rmdir
#define	open		_open
#define	close		_close
#define	unlink		_unlink
#define	fdopen		_fdopen
#ifndef lseek
#define lseek		_lseeki64
#endif
#endif
#define off_t		cob_s64_t

#ifndef	_O_TEMPORARY
#define	_O_TEMPORARY	0
#endif

#else
#if	defined(HAVE_FDATASYNC)
#define	fdcobsync	fdatasync
#else
#define	fdcobsync	fsync
#endif

#ifndef	O_BINARY
#define	O_BINARY	0
#endif

#endif

/* Force symbol exports */
#define	COB_LIB_EXPIMP

#include "libcob.h"
#include "coblocal.h"

#ifdef	WITH_DB

#include <db.h>

#elif	defined(WITH_CISAM) || defined(WITH_DISAM) || defined(WITH_VBISAM)

#define	WITH_ANY_ISAM
#include <signal.h>

#define	COB_WITH_STATUS_02

#ifdef	WITH_CISAM
#include <isam.h>
#define	isfullclose(x)	isclose (x)
#endif

#ifdef	WITH_DISAM
#ifndef DISAM_NO_ISCONFIG
#include <isconfig.h>
#ifndef ISCOBOL_STATS
#undef	COB_WITH_STATUS_02
#endif
#endif
#include <disam.h>
#define	isfullclose(x)	isclose (x)
#endif

#ifdef	WITH_VBISAM
#include <vbisam.h>
#if	1	/* RXWRXW - Status 02 */
#undef	COB_WITH_STATUS_02
#endif
#endif

/* Isam File handler packet */

struct indexfile {
	char		*filename;	/* ISAM data file name */
	char		*savekey;	/* Area to save last primary key read */
	char		*recwrk;	/* Record work/save area */
	size_t		nkeys;		/* Actual keys in file */
	int		isfd;		/* ISAM file number */
	int		recnum;		/* Last record number read */
	int		saverecnum;	/* isrecnum of next record to process */
	int		saveerrno;	/* savefileposition errno */
	int		lmode;		/* File lock mode for 'isread' */
	int		curkey;		/* Current active index */
	int		startcond;	/* Previous 'start' condition value */
	int		readdir;	/* Read direction: ISPREV or ISNEXT */
	int		lenkey;		/* Length of savekey area */
	int		eofpending;	/* End of file pending */
	int		readdone;	/* A 'read' has been succesfully done */
	int		startiscur;	/* The 'start' record is current */
	int		wrkhasrec;	/* 'recwrk' holds the next|prev record */
	struct keydesc	key[1];		/* Table of key information */
					/* keydesc is defined in (d|c|vb)isam.h */
};

#endif

struct file_list {
	struct file_list	*next;
	cob_file		*file;
};

#ifdef	WORDS_BIGENDIAN
#define	COB_MAYSWAP_16(x)	((unsigned short)(x))
#define	COB_MAYSWAP_32(x)	((unsigned int)(x))
#else
#define	COB_MAYSWAP_16(x)	(COB_BSWAP_16((unsigned short)(x)))
#define	COB_MAYSWAP_32(x)	(COB_BSWAP_32((unsigned int)(x)))
#endif

/* SORT definitions */

#define COBSORTEND		1
#define COBSORTABORT		2
#define COBSORTFILEERR		3
#define COBSORTNOTOPEN		4


/* Sort item */
struct cobitem {
	struct cobitem		*next;
	unsigned char		end_of_block;
	unsigned char		block_byte;
	unsigned char		unique[sizeof(size_t)];
	unsigned char		item[1];
};

/* Sort memory chunk */
struct sort_mem_struct {
	struct sort_mem_struct	*next;
	unsigned char		*mem_ptr;
};

/* Sort queue structure */
struct queue_struct {
	struct cobitem		*first;
	struct cobitem		*last;
	size_t			count;
};

/* Sort temporary file structure */
struct file_struct {
	FILE			*fp;
	size_t			count;	/* Count of blocks in temporary files */
};

/* Sort base structure */
struct cobsort {
	void			*pointer;
	struct cobitem		*empty;
	void			*sort_return;
	cob_field		*fnstatus;
	struct sort_mem_struct	*mem_base;
	size_t			unique;
	size_t			size;
	size_t			alloc_size;
	size_t			mem_size;
	size_t			mem_used;
	size_t			mem_total;
	size_t			chunk_size;
	size_t			r_size;
	size_t			w_size;
	size_t			switch_to_file;
	unsigned int		retrieving;
	unsigned int		files_used;
	int			destination_file;
	int			retrieval_queue;
	struct queue_struct	queue[4];
	struct file_struct	file[4];
};

/* End SORT definitions */


/* Local variables */

static cob_global	*cobglobptr;
static cob_settings	*cobsetptr;

static unsigned int	eop_status;
static unsigned int	check_eop_status;
static size_t		cob_vsq_len;

static struct file_list	*file_cache;

static char		*file_open_env;
static char		*file_open_name;
static char		*file_open_buff;

static char		*runtime_buffer;

static const int	status_exception[] = {
	0,				/* 0x */
	COB_EC_I_O_AT_END,		/* 1x */
	COB_EC_I_O_INVALID_KEY,		/* 2x */
	COB_EC_I_O_PERMANENT_ERROR,	/* 3x */
	COB_EC_I_O_LOGIC_ERROR,		/* 4x */
	COB_EC_I_O_RECORD_OPERATION,	/* 5x */
	COB_EC_I_O_FILE_SHARING,	/* 6x */
	COB_EC_I_O,			/* Unused */
	COB_EC_I_O,			/* Unused */
	COB_EC_I_O_IMP			/* 9x */
};

static const char	* const prefix[] = { "DD_", "dd_", "" };
#define NUM_PREFIX	sizeof(prefix) / sizeof(char *)

static int dummy_delete		(cob_file *);
static int dummy_rnxt_rewrite	(cob_file *, const int);
static int dummy_read		(cob_file *, cob_field *, const int);
static int dummy_start		(cob_file *, const int, cob_field *);

static int cob_file_open	(cob_file *, char *, const int, const int);
static int cob_file_close	(cob_file *, const int);
static int cob_file_write_opt	(cob_file *, const int);

static int sequential_read	(cob_file *, const int);
static int sequential_write	(cob_file *, const int);
static int sequential_rewrite	(cob_file *, const int);
static int lineseq_read		(cob_file *, const int);
static int lineseq_write	(cob_file *, const int);
static int relative_start	(cob_file *, const int, cob_field *);
static int relative_read	(cob_file *, cob_field *, const int);
static int relative_read_next	(cob_file *, const int);
static int relative_write	(cob_file *, const int);
static int relative_rewrite	(cob_file *, const int);
static int relative_delete	(cob_file *);

static int indexed_open		(cob_file *, char *, const int, const int);
static int indexed_close	(cob_file *, const int);
static int indexed_start	(cob_file *, const int, cob_field *);
static int indexed_read		(cob_file *, cob_field *, const int);
static int indexed_read_next	(cob_file *, const int);
static int indexed_write	(cob_file *, const int);
static int indexed_delete	(cob_file *);
static int indexed_rewrite	(cob_file *, const int);

static const struct cob_fileio_funcs indexed_funcs = {
	indexed_open,
	indexed_close,
	indexed_start,
	indexed_read,
	indexed_read_next,
	indexed_write,
	indexed_rewrite,
	indexed_delete
};

static const struct cob_fileio_funcs sequential_funcs = {
	cob_file_open,
	cob_file_close,
	dummy_start,
	dummy_read,
	sequential_read,
	sequential_write,
	sequential_rewrite,
	dummy_delete
};

static const struct cob_fileio_funcs lineseq_funcs = {
	cob_file_open,
	cob_file_close,
	dummy_start,
	dummy_read,
	lineseq_read,
	lineseq_write,
	dummy_rnxt_rewrite,
	dummy_delete
};

static const struct cob_fileio_funcs relative_funcs = {
	cob_file_open,
	cob_file_close,
	relative_start,
	relative_read,
	relative_read_next,
	relative_write,
	relative_rewrite,
	relative_delete
};

static const struct cob_fileio_funcs	*fileio_funcs[COB_ORG_MAX] = {
	&sequential_funcs,
	&lineseq_funcs,
	&relative_funcs,
	&indexed_funcs,
	NULL
};

#if	defined(WITH_INDEX_EXTFH) || defined(WITH_SEQRA_EXTFH)
extern void	extfh_cob_init_fileio	(const struct cob_fileio_funcs *,
					const struct cob_fileio_funcs *,
					const struct cob_fileio_funcs *,
					int (*)(cob_file *, const int));
extern void	extfh_cob_exit_fileio	(void);
#endif

#ifdef	WITH_INDEX_EXTFH
extern void extfh_indexed_unlock	(cob_file *);
extern int extfh_indexed_locate		(cob_file *, char *);
extern int extfh_indexed_open		(cob_file *, char *, const int, const int);
extern int extfh_indexed_close		(cob_file *, const int);
extern int extfh_indexed_start		(cob_file *, const int, cob_field *);
extern int extfh_indexed_read		(cob_file *, cob_field *, const int);
extern int extfh_indexed_read_next	(cob_file *, const int);
extern int extfh_indexed_write		(cob_file *, const int);
extern int extfh_indexed_delete		(cob_file *);
extern int extfh_indexed_rewrite	(cob_file *, const int);
#endif

#ifdef	WITH_SEQRA_EXTFH
extern void extfh_seqra_unlock		(cob_file *);
extern int extfh_seqra_locate		(cob_file *, char *);
extern int extfh_cob_file_open		(cob_file *, char *, const int, const int);
extern int extfh_cob_file_close		(cob_file *, const int);
extern int extfh_sequential_read	(cob_file *, const int);
extern int extfh_sequential_write	(cob_file *, const int);
extern int extfh_sequential_rewrite	(cob_file *, const int);
extern int extfh_relative_start		(cob_file *, const int, cob_field *);
extern int extfh_relative_read		(cob_file *, cob_field *, const int);
extern int extfh_relative_read_next	(cob_file *, const int);
extern int extfh_relative_write		(cob_file *, const int);
extern int extfh_relative_rewrite	(cob_file *, const int);
extern int extfh_relative_delete	(cob_file *);
#endif

#ifdef	WITH_DB

static DB_ENV		*bdb_env;
static char		*bdb_buff;
static const char	**bdb_data_dir;
static void		*record_lock_object;
static size_t		rlo_size;
static unsigned int	bdb_lock_id;

#define DB_PUT(db,flags)	db->put (db, NULL, &p->key, &p->data, flags)
#define DB_GET(db,flags)	db->get (db, NULL, &p->key, &p->data, flags)
#define DB_SEQ(db,flags)	db->c_get (db, &p->key, &p->data, flags)
#define DB_DEL(db,key,flags)	db->del (db, NULL, key, flags)
#define DB_CLOSE(db)		db->close (db, 0)
#define DB_SYNC(db)		db->sync (db, 0)
#define	cob_dbtsize_t		u_int32_t

#define DBT_SET(key,fld)			\
	key.data = fld->data;			\
	key.size = (cob_dbtsize_t) fld->size

struct indexed_file {
	DB		**db;		/* Database handlers */
	DBC		**cursor;
	char		*filename;	/* Needed for record locks */
	unsigned char	*last_key;	/* The last key written */
	unsigned char	*temp_key;	/* Used for temporary storage */
	unsigned char	**last_readkey;	/* The last key read */
	unsigned int	*last_dupno;	/* The last number of duplicates read */
	int		*rewrite_sec_key;
	DBT		key;
	DBT		data;
	DB_LOCK		bdb_file_lock;
	DB_LOCK		bdb_record_lock;
	size_t		key_index;
	unsigned int	bdb_lock_id;
	int		write_cursor_open;
	int		record_locked;
	int		filenamelen;
};

#endif	/* WITH_DB */


/* Local functions */

static int
dummy_delete (cob_file *f)
{
	COB_UNUSED (f);

	return COB_STATUS_91_NOT_AVAILABLE;
}

static int
dummy_rnxt_rewrite (cob_file *f, const int opt)
{
	COB_UNUSED (f);
	COB_UNUSED (opt);

	return COB_STATUS_91_NOT_AVAILABLE;
}

static int
dummy_read (cob_file *f, cob_field *key, const int read_opts)
{
	COB_UNUSED (f);
	COB_UNUSED (key);
	COB_UNUSED (read_opts);

	return COB_STATUS_91_NOT_AVAILABLE;
}

static int
dummy_start (cob_file *f, const int cond, cob_field *key)
{
	COB_UNUSED (f);
	COB_UNUSED (cond);
	COB_UNUSED (key);

	return COB_STATUS_91_NOT_AVAILABLE;
}

static char *
cob_chk_file_env (const char *src)
{
	char		*p;
	char		*q;
	char		*s;
	size_t		i;

	if (unlikely (cobsetptr->cob_env_mangle)) {
		q = cob_strdup (src);
		s = q;
		for (i = 0; i < strlen (s); ++i) {
			if (!isalnum ((int)s[i])) {
				s[i] = '_';
			}
		}
	} else {
		q = NULL;
		s = (char *)src;
	}
	p = NULL;
	for (i = 0; i < NUM_PREFIX; ++i) {
		snprintf (file_open_env, (size_t)COB_FILE_MAX, "%s%s",
			  prefix[i], s);
		file_open_env[COB_FILE_MAX] = 0;
		p = getenv (file_open_env);
		if (p) {
			break;
		}
	}
	if (unlikely (q)) {
		cob_free (q);
	}
	return p;
}

static void
cob_chk_file_mapping (void)
{
	char		*p;
	char		*src;
	char		*dst;
	char		*saveptr;
	char		*orig;
	unsigned int	dollar;

	if (unlikely (!COB_MODULE_PTR->flag_filename_mapping)) {
		return;
	}

	/* Misuse "dollar" here to indicate a separator */
	dollar = 0;
	for (p = file_open_name; *p; p++) {
		if (*p == '/' || *p == '\\') {
			dollar = 1;
			break;
		}
	}

	src = file_open_name;

	/* Simple case - No separators */
	if (likely(dollar == 0)) {
		/* Ignore leading dollar */
		if (*src == '$') {
			src++;
		}
		/* Check for DD_xx, dd_xx, xx environment variables */
		/* If not found, use as is including the dollar character */
		if ((p = cob_chk_file_env (src)) != NULL) {
			strncpy (file_open_name, p, (size_t)COB_FILE_MAX);
		} else if (cobsetptr->cob_file_path) {
			snprintf (file_open_buff, (size_t)COB_FILE_MAX, "%s%c%s",
				  cobsetptr->cob_file_path, SLASH_CHAR, file_open_name);
			file_open_buff[COB_FILE_MAX] = 0;
			strncpy (file_open_name, file_open_buff,
				 (size_t)COB_FILE_MAX);
		}
		return;
	}

	/* Complex */
	/* Isolate first element (everything before the slash) */
	/* If it starts with a slash, it's absolute, do nothing */
	/* Else if it starts with a $, mark and skip over the $ */
	/* Try mapping on resultant string - DD_xx, dd_xx, xx */
	/* If successful, use the mapping */
	/* If not, use original element EXCEPT if we started */
	/* with a $, in which case, we ignore the element AND */
	/* the following slash */

	dollar = 0;
	dst = file_open_buff;
	*dst = 0;

	if (*src == '$') {
		dollar = 1;
		src++;
	}

	orig = cob_strdup (src);
	saveptr = orig;

	/* strtok strips leading delimiters */
	if (*src == '/' || *src == '\\') {
		strcpy (file_open_buff, SLASH_STR);
	} else {
		file_open_buff[COB_FILE_MAX] = 0;
		p = strtok (orig, "/\\");
		orig = NULL;
		if ((src = cob_chk_file_env (p)) != NULL) {
			strncpy (file_open_buff, src, (size_t)COB_FILE_MAX);
			dollar = 0;
		} else if (!dollar) {
			strncpy (file_open_buff, p, (size_t)COB_FILE_MAX);
		}
	}
	/* First element completed, loop through remaining */
	/* elements delimited by slash */
	/* Check each for $ mapping */
	for (; ;) {
		p = strtok (orig, "/\\");
		if (!p) {
			break;
		}
		if (!orig) {
			if (dollar) {
				dollar = 0;
			} else {
				strcat (file_open_buff, SLASH_STR);
			}
		} else {
			orig = NULL;
		}
		if (*p == '$' && (src = cob_chk_file_env (p + 1)) != NULL) {
			strncat (file_open_buff, src, (size_t)COB_FILE_MAX);
		} else {
			strncat (file_open_buff, p, (size_t)COB_FILE_MAX);
		}
	}
	strcpy (file_open_name, file_open_buff);
	cob_free (saveptr);
}

static void
cob_sync (cob_file *f)
{
#ifdef	WITH_DB
	struct indexed_file	*p;
	size_t			i;
#elif	defined(WITH_ANY_ISAM)
	struct indexfile	*fh;
#endif

	if (f->organization == COB_ORG_INDEXED) {
#ifdef	WITH_DB
		p = f->file;
		if (p) {
			for (i = 0; i < f->nkeys; ++i) {
				if (p->db[i]) {
					DB_SYNC (p->db[i]);
				}
			}
		}
#elif	defined(WITH_ANY_ISAM)
		fh = f->file;
		if (fh) {
			isflush (fh->isfd);
		}
#endif
		return;
	}
	if (f->organization != COB_ORG_SORT) {
		if (f->organization == COB_ORG_LINE_SEQUENTIAL) {
			if (f->file) {
				fflush ((FILE *)f->file);
			}
		}
		if (f->fd >= 0) {
			fdcobsync (f->fd);
		}
	}
}

static void
cob_cache_file (cob_file *f)
{
	struct file_list	*l;

	for (l = file_cache; l; l = l->next) {
		if (f == l->file) {
			return;
		}
	}
	l = cob_malloc (sizeof (struct file_list));
	l->file = f;
	l->next = file_cache;
	file_cache = l;
}

static void
save_status (cob_file *f, cob_field *fnstatus, const int status)
{
	cobglobptr->cob_error_file = f;
	if (likely(status == 0)) {
		memset (f->file_status, '0', (size_t)2);
		if (fnstatus) {
			memset (fnstatus->data, '0', (size_t)2);
		}
		/* EOP is non-fatal therefore 00 status but needs exception */
		if (unlikely (eop_status)) {
			eop_status = 0;
			cob_set_exception (COB_EC_I_O_EOP);
		} else {
			cob_set_exception (0);
		}
		if (unlikely (cobsetptr->cob_do_sync)) {
			cob_sync (f);
		}
		return;
	}
	cob_set_exception (status_exception[status / 10]);
	f->file_status[0] = (unsigned char)COB_I2D (status / 10);
	f->file_status[1] = (unsigned char)COB_I2D (status % 10);
	if (fnstatus) {
		memcpy (fnstatus->data, f->file_status, (size_t)2);
	}
}

/* Regular file */

static size_t
file_linage_check (cob_file *f)
{
	cob_linage	*lingptr;

	lingptr = f->linorkeyptr;
	lingptr->lin_lines = cob_get_int (lingptr->linage);
	if (lingptr->lin_lines < 1) {
		goto linerr;
	}
	if (lingptr->latfoot) {
		lingptr->lin_foot = cob_get_int (lingptr->latfoot);
		if (lingptr->lin_foot < 1 ||
		    lingptr->lin_foot > lingptr->lin_lines) {
			goto linerr;
		}
	} else {
		lingptr->lin_foot = 0;
	}
	if (lingptr->lattop) {
		lingptr->lin_top = cob_get_int (lingptr->lattop);
		if (lingptr->lin_top < 0) {
			goto linerr;
		}
	} else {
		lingptr->lin_top = 0;
	}
	if (lingptr->latbot) {
		lingptr->lin_bot = cob_get_int (lingptr->latbot);
		if (lingptr->lin_bot < 0) {
			goto linerr;
		}
	} else {
		lingptr->lin_bot = 0;
	}
	return 0;
linerr:
	cob_set_int (lingptr->linage_ctr, 0);
	return 1;
}

static int
cob_linage_write_opt (cob_file *f, const int opt)
{
	cob_linage		*lingptr;
	FILE			*fp;
	int			i;
	int			n;

	fp = (FILE *)f->file;
	lingptr = f->linorkeyptr;
	if (unlikely (opt & COB_WRITE_PAGE)) {
		i = cob_get_int (lingptr->linage_ctr);
		if (i == 0) {
			return COB_STATUS_57_I_O_LINAGE;
		}
		n = lingptr->lin_lines;
		for (; i < n; ++i) {
			putc ('\n', fp);
		}
		for (i = 0; i < lingptr->lin_bot; ++i) {
			putc ('\n', fp);
		}
		if (file_linage_check (f)) {
			return COB_STATUS_57_I_O_LINAGE;
		}
		for (i = 0; i < lingptr->lin_top; ++i) {
			putc ('\n', fp);
		}
		cob_set_int (lingptr->linage_ctr, 1);
	} else if (opt & COB_WRITE_LINES) {
		n = cob_get_int (lingptr->linage_ctr);
		if (n == 0) {
			return COB_STATUS_57_I_O_LINAGE;
		}
		cob_add_int (lingptr->linage_ctr, opt & COB_WRITE_MASK, 0);
		i = cob_get_int (lingptr->linage_ctr);
		/* Set EOP status if requested */
		if (check_eop_status && lingptr->lin_foot) {
			if (i >= lingptr->lin_foot) {
				eop_status = 1;
			}
		}
		if (i > lingptr->lin_lines) {
			/* Set EOP status if requested */
			if (check_eop_status) {
				eop_status = 1;
			}
			for (; n < lingptr->lin_lines; ++n) {
				putc ('\n', fp);
			}
			for (i = 0; i < lingptr->lin_bot; ++i) {
				putc ('\n', fp);
			}
			if (file_linage_check (f)) {
				return COB_STATUS_57_I_O_LINAGE;
			}
			cob_set_int (lingptr->linage_ctr, 1);
			for (i = 0; i < lingptr->lin_top; ++i) {
				putc ('\n', fp);
			}
		} else {
			for (i = (opt & COB_WRITE_MASK) - 1; i > 0; --i) {
				putc ('\n', fp);
			}
		}
	}
	return 0;
}

static unsigned int
cob_seq_write_opt (cob_file *f, const int opt)
{
	int	i;

	if (opt & COB_WRITE_LINES) {
		i = opt & COB_WRITE_MASK;
		if (!i) {
			/* AFTER/BEFORE 0 */
			if (write (f->fd, "\r", (size_t)1) != 1) {
				return 1;
			}
		} else {
			for (i = opt & COB_WRITE_MASK; i > 0; --i) {
				if (write (f->fd, "\n", (size_t)1) != 1) {
					return 1;
				}
			}
		}
	} else if (opt & COB_WRITE_PAGE) {
		if (write (f->fd, "\f", (size_t)1) != 1) {
			return 1;
		}
	}
	return 0;
}

static int
cob_file_write_opt (cob_file *f, const int opt)
{
	int	i;

	if (unlikely (f->flag_select_features & COB_SELECT_LINAGE)) {
		return cob_linage_write_opt (f, opt);
	}
	if (opt & COB_WRITE_LINES) {
		i = opt & COB_WRITE_MASK;
		if (!i) {
			/* AFTER/BEFORE 0 */
			putc ('\r', (FILE *)f->file);
		} else {
			for (; i > 0; --i) {
				putc ('\n', (FILE *)f->file);
			}
		}
	} else if (opt & COB_WRITE_PAGE) {
		putc ('\f', (FILE *)f->file);
	}
	return 0;
}

static int
cob_fd_file_open (cob_file *f, char *filename, const int mode, const int sharing)
{
	int		fd;
	int		fdmode;
	int		fperms;
	unsigned int	nonexistent;
#ifdef	HAVE_FCNTL
	int		ret;
	struct flock	lock;
#endif

	/* Note filename points to file_open_name */
	/* cob_chk_file_mapping manipulates file_open_name directly */

	COB_UNUSED (sharing);

	cob_chk_file_mapping ();

	nonexistent = 0;
	errno = 0;
	if (access (filename, F_OK) && errno == ENOENT) {
		if (mode != COB_OPEN_OUTPUT && f->flag_optional == 0) {
			return COB_STATUS_35_NOT_EXISTS;
		}
		nonexistent = 1;
	}

	fdmode = O_BINARY;
	fperms = 0;
	f->fd = -1;
	switch (mode) {
	case COB_OPEN_INPUT:
		fdmode |= O_RDONLY;
		break;
	case COB_OPEN_OUTPUT:
		nonexistent = 0;
		fdmode |= O_CREAT | O_TRUNC;
		if (f->organization == COB_ORG_RELATIVE) {
			fdmode |= O_RDWR;
		} else {
			fdmode |= O_WRONLY;
		}
#ifdef	_WIN32
		fperms = _S_IREAD | _S_IWRITE ;
#else
		fperms = COB_FILE_MODE;
#endif
		break;
	case COB_OPEN_I_O:
		if (nonexistent) {
			fdmode |= O_CREAT | O_RDWR;
#ifdef	_WIN32
			fperms = _S_IREAD | _S_IWRITE ;
#else
			fperms = COB_FILE_MODE;
#endif
		} else {
			fdmode |= O_RDWR;
		}
		break;
	case COB_OPEN_EXTEND:
		fdmode |= O_CREAT | O_RDWR | O_APPEND;
#ifdef	_WIN32
		fperms = _S_IREAD | _S_IWRITE ;
#else
		fperms = COB_FILE_MODE;
#endif
		break;
	}

	errno = 0;
	fd = open (filename, fdmode, fperms);

	switch (errno) {
	case 0:
		if (mode == COB_OPEN_EXTEND && fd >= 0) {
			lseek (fd, (off_t) 0, SEEK_END);
		}
		f->open_mode = mode;
		break;
	case ENOENT:
		if (mode == COB_OPEN_EXTEND || mode == COB_OPEN_OUTPUT) {
			return COB_STATUS_30_PERMANENT_ERROR;
		}
		if (f->flag_optional) {
			f->open_mode = mode;
			f->flag_nonexistent = 1;
			f->flag_end_of_file = 1;
			f->flag_begin_of_file = 1;
			return COB_STATUS_05_SUCCESS_OPTIONAL;
		}
		return COB_STATUS_35_NOT_EXISTS;
	case EACCES:
	case EISDIR:
	case EROFS:
		return COB_STATUS_37_PERMISSION_DENIED;
	case EAGAIN:
		return COB_STATUS_61_FILE_SHARING;
	default:
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	f->fd = fd;

#ifdef	HAVE_FCNTL
	/* Lock the file */
	if (memcmp (filename, "/dev/", (size_t)5)) {
		memset ((void *)&lock, 0, sizeof (struct flock));
		if (mode != COB_OPEN_INPUT) {
			lock.l_type = F_WRLCK;
		} else {
			lock.l_type = F_RDLCK;
		}
		lock.l_whence = SEEK_SET;
		lock.l_start = 0;
		lock.l_len = 0;
		errno = 0;
		if (fcntl (fd, F_SETLK, &lock) < 0) {
			ret = errno;
			close (fd);
			f->fd = -1;
			switch (ret) {
			case EACCES:
			case EAGAIN:
			case EDEADLK:
				return COB_STATUS_61_FILE_SHARING;
			default:
				return COB_STATUS_30_PERMANENT_ERROR;
			}
		}
	}
#endif
	if (f->flag_optional && nonexistent) {
		return COB_STATUS_05_SUCCESS_OPTIONAL;
	}
	return 0;
}

static int
cob_file_open (cob_file *f, char *filename, const int mode, const int sharing)
{
	/* Note filename points to file_open_name */
	/* cob_chk_file_mapping manipulates file_open_name directly */

#ifdef	WITH_SEQRA_EXTFH
	int		ret;

	ret = extfh_seqra_locate (f, filename);
	switch (ret) {
	case COB_NOT_CONFIGURED:
		cob_chk_file_mapping ();
		if (access (filename, F_OK) && errno == ENOENT) {
			if (mode != COB_OPEN_OUTPUT && f->flag_optional == 0) {
				return COB_STATUS_35_NOT_EXISTS;
			}
		}
		break;
	case COB_STATUS_00_SUCCESS:
		break;
	default:
		return ret;
	}
	ret = extfh_cob_file_open (f, filename, mode, sharing);
	switch (ret) {
	case COB_STATUS_00_SUCCESS:
		f->open_mode = mode;
		break;
	case COB_STATUS_35_NOT_EXISTS:
		if (f->flag_optional) {
			f->open_mode = mode;
			f->flag_nonexistent = 1;
			f->flag_end_of_file = 1;
			f->flag_begin_of_file = 1;
			return COB_STATUS_05_SUCCESS_OPTIONAL;
		}
		break;
	}
	return ret;

#else

	FILE			*fp;
	const char		*fmode;
	cob_linage		*lingptr;
#ifdef	HAVE_FCNTL
	int			ret;
	struct flock		lock;
#endif
	unsigned int		nonexistent;

	if (f->organization != COB_ORG_LINE_SEQUENTIAL) {
		return cob_fd_file_open (f, filename, mode, sharing);
	}

	cob_chk_file_mapping ();

	nonexistent = 0;
	errno = 0;
	if (access (filename, F_OK) && errno == ENOENT) {
		nonexistent = 1;
		if (mode != COB_OPEN_OUTPUT && f->flag_optional == 0) {
			return COB_STATUS_35_NOT_EXISTS;
		}
	}

	fp = NULL;
	fmode = NULL;
	/* Open the file */
	switch (mode) {
	case COB_OPEN_INPUT:
		if (!cobsetptr->cob_unix_lf) {
			fmode = "r";
		} else {
			fmode = "rb";
		}
		break;
	case COB_OPEN_OUTPUT:
		if (!cobsetptr->cob_unix_lf) {
			fmode = "w";
		} else {
			fmode = "wb";
		}
		break;
	case COB_OPEN_I_O:
		return COB_STATUS_37_PERMISSION_DENIED;
	case COB_OPEN_EXTEND:
		/* Problem with	VC (tested MSC_VER 15) if file isn't there: */
		/* Both modes create the file and return a bad pointer */
		/* Mode "a+"  sets EINVAL, further actions on the file do work */
		/* Mode "ab+" doesn't set errno, but we dont want a binary file */
		/* Possible Solutions: */
		/* a) Create the file and reopen it with a+ */
		/* b) Check this stuff in EINVAL and just go on */
		if (!cobsetptr->cob_unix_lf) {
			fmode = "a+";
		} else {
			fmode = "ab+";
		}
		break;
	/* LCOV_EXCL_START */
	default:
		cob_fatal_error(COB_FERROR_CODEGEN);
	/* LCOV_EXCL_STOP */
	}

	errno = 0;
	fp = fopen (filename, fmode);
	switch (errno) {
	case 0:
		f->open_mode = mode;
		break;
	case EINVAL:
		if (f->flag_optional && nonexistent) {
			f->open_mode = mode;
		} else {
			return COB_STATUS_30_PERMANENT_ERROR;
		}
		break;
	case ENOENT:
		if (mode == COB_OPEN_EXTEND || mode == COB_OPEN_OUTPUT) {
			return COB_STATUS_30_PERMANENT_ERROR;
		}
		if (f->flag_optional) {
			f->open_mode = mode;
			f->flag_nonexistent = 1;
			f->flag_end_of_file = 1;
			f->flag_begin_of_file = 1;
			return COB_STATUS_05_SUCCESS_OPTIONAL;
		}
		return COB_STATUS_35_NOT_EXISTS;
	case EACCES:
	case EISDIR:
	case EROFS:
		return COB_STATUS_37_PERMISSION_DENIED;
	case EAGAIN:
		return COB_STATUS_61_FILE_SHARING;
	default:
		return COB_STATUS_30_PERMANENT_ERROR;
	}

	if (unlikely (f->flag_select_features & COB_SELECT_LINAGE)) {
		if (file_linage_check (f)) {
			fclose (fp);
			return COB_STATUS_57_I_O_LINAGE;
		}
		f->flag_needs_top = 1;
		lingptr = f->linorkeyptr;
		cob_set_int (lingptr->linage_ctr, 1);
	}
	f->file = fp;
	f->fd = fileno (fp);

#ifdef	HAVE_FCNTL
	/* Lock the file */
	if (memcmp (filename, "/dev/", (size_t)5)) {
		memset ((void *)&lock, 0, sizeof (struct flock));
		if (mode != COB_OPEN_INPUT) {
			lock.l_type = F_WRLCK;
		} else {
			lock.l_type = F_RDLCK;
		}
		lock.l_whence = SEEK_SET;
		lock.l_start = 0;
		lock.l_len = 0;
		if (fcntl (fileno (fp), F_SETLK, &lock) < 0) {
			ret = errno;
			fclose (fp);
			switch (ret) {
			case EACCES:
			case EAGAIN:
			case EDEADLK:
				return COB_STATUS_61_FILE_SHARING;
			default:
				return COB_STATUS_30_PERMANENT_ERROR;
			}
		}
	}
#endif
	if (f->flag_optional && nonexistent) {
		return COB_STATUS_05_SUCCESS_OPTIONAL;
	}
	return 0;

#endif
}

static int
cob_file_close (cob_file *f, const int opt)
{
#ifdef	WITH_SEQRA_EXTFH
	return extfh_cob_file_close (f, opt);
#else

#ifdef	HAVE_FCNTL
	struct flock lock;
#endif

	switch (opt) {
	case COB_CLOSE_NORMAL:
	case COB_CLOSE_LOCK:
	case COB_CLOSE_NO_REWIND:
		if (f->organization == COB_ORG_LINE_SEQUENTIAL) {
			if (f->flag_needs_nl &&
			    !(f->flag_select_features & COB_SELECT_LINAGE)) {
				f->flag_needs_nl = 0;
				putc ('\n', (FILE *)f->file);
			}
		} else if (f->flag_needs_nl) {
			f->flag_needs_nl = 0;
			if (f->fd >= 0) {
				if (write (f->fd, "\n", (size_t)1) != 1) {
				}
			}
		}
#ifdef	HAVE_FCNTL
		/* Unlock the file */
		memset ((void *)&lock, 0, sizeof (struct flock));
		lock.l_type = F_UNLCK;
		lock.l_whence = SEEK_SET;
		lock.l_start = 0;
		lock.l_len = 0;
		if (f->fd >= 0) {
			fcntl (f->fd, F_SETLK, &lock);
		}
#endif
		/* Close the file */
		if (f->organization == COB_ORG_LINE_SEQUENTIAL) {
			if (f->file) {
				fclose ((FILE *)f->file);
			}
		} else {
			if (f->fd >= 0) {
				close (f->fd);
			}
		}
		if (opt == COB_CLOSE_NO_REWIND) {
			f->open_mode = COB_OPEN_CLOSED;
			return COB_STATUS_07_SUCCESS_NO_UNIT;
		}
		return COB_STATUS_00_SUCCESS;
	default:
		if (f->fd >= 0 && f->open_mode != COB_OPEN_INPUT) {
			fdcobsync (f->fd);
		}
		return COB_STATUS_07_SUCCESS_NO_UNIT;
	}
#endif
}

/* SEQUENTIAL */

static int
sequential_read (cob_file *f, const int read_opts)
{
	int	bytesread;
	union {
		unsigned char	sbuff[4];
		unsigned short	sshort[2];
		unsigned int	sint;
	} recsize;

#ifdef	WITH_SEQRA_EXTFH
	int	extfh_ret;

	extfh_ret = extfh_sequential_read (f, read_opts);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#else
	COB_UNUSED (read_opts);
#endif

	if (unlikely (f->flag_operation != 0)) {
		f->flag_operation = 0;
		lseek (f->fd, (off_t)0, SEEK_CUR);
	}

	if (unlikely (f->record_min != f->record_max)) {
		/* Read record size */

		bytesread = read (f->fd, recsize.sbuff, cob_vsq_len);
		if (unlikely (bytesread != (int)cob_vsq_len)) {
			if (bytesread == 0) {
				return COB_STATUS_10_END_OF_FILE;
			} else {
				return COB_STATUS_30_PERMANENT_ERROR;
			}
		}
		switch (cobsetptr->cob_varseq_type) {
		case 1:
			f->record->size = COB_MAYSWAP_32 (recsize.sint);
			break;
		case 2:
			f->record->size = recsize.sint;
			break;
		default:
			f->record->size = COB_MAYSWAP_16 (recsize.sshort[0]);
			break;
		}
	}

	/* Read record */
	bytesread = read (f->fd, f->record->data, f->record->size);
	if (unlikely (bytesread != (int)f->record->size)) {
		if (bytesread == 0) {
			return COB_STATUS_10_END_OF_FILE;
		/* LCOV_EXCL_START */
		} else if (bytesread < 0) {
			return COB_STATUS_30_PERMANENT_ERROR;
		/* LCOV_EXCL_STOP */
		} else {
			return COB_STATUS_04_SUCCESS_INCOMPLETE;
		}
	}
	return COB_STATUS_00_SUCCESS;
}

static int
sequential_write (cob_file *f, const int opt)
{
	union {
		unsigned char	sbuff[4];
		unsigned short	sshort[2];
		unsigned int	sint;
	} recsize;

#ifdef	WITH_SEQRA_EXTFH
	int	extfh_ret;

	extfh_ret = extfh_sequential_write (f, opt);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#endif

	if (unlikely (f->flag_operation == 0)) {
		f->flag_operation = 1;
		lseek (f->fd, (off_t)0, SEEK_CUR);
	}

	/* WRITE AFTER */
	if (unlikely (opt & COB_WRITE_AFTER)) {
		if (cob_seq_write_opt (f, opt)) {
			return COB_STATUS_30_PERMANENT_ERROR;
		}
		f->flag_needs_nl = 1;
	}

	if (unlikely (f->record_min != f->record_max)) {
		/* Write record size */

		switch (cobsetptr->cob_varseq_type) {
		case 1:
			recsize.sint = COB_MAYSWAP_32 (f->record->size);
			break;
		case 2:
			recsize.sint = f->record->size;
			break;
		default:
			recsize.sint = 0;
			recsize.sshort[0] = COB_MAYSWAP_16 (f->record->size);
			break;
		}

		if (unlikely (write (f->fd, recsize.sbuff, cob_vsq_len) !=
			     (int)cob_vsq_len)) {
			return COB_STATUS_30_PERMANENT_ERROR;
		}
	}

	/* Write record */
	if (unlikely (write (f->fd, f->record->data, f->record->size) !=
		     (int)f->record->size)) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}

	/* WRITE BEFORE */
	if (unlikely (opt & COB_WRITE_BEFORE)) {
		if (cob_seq_write_opt (f, opt)) {
			return COB_STATUS_30_PERMANENT_ERROR;
		}
		f->flag_needs_nl = 0;
	}

	return COB_STATUS_00_SUCCESS;
}

static int
sequential_rewrite (cob_file *f, const int opt)
{
#ifdef	WITH_SEQRA_EXTFH
	int	extfh_ret;

	extfh_ret = extfh_sequential_rewrite (f, opt);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#else
	COB_UNUSED (opt);
#endif
	f->flag_operation = 1;
	if (lseek (f->fd, -(off_t) f->record->size, SEEK_CUR) == (off_t)-1) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	if (write (f->fd, f->record->data, f->record->size) != (int)f->record->size) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	return COB_STATUS_00_SUCCESS;
}

/* LINE SEQUENTIAL */

static int
lineseq_read (cob_file *f, const int read_opts)
{
	unsigned char	*dataptr;
	size_t		i = 0;
	int		n;

#ifdef	WITH_SEQRA_EXTFH
	int		extfh_ret;

	extfh_ret = extfh_sequential_read (f, read_opts);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#else
	COB_UNUSED (read_opts);
#endif

	dataptr = f->record->data;
	for (; ;) {
		n = getc ((FILE *)f->file);
		if (unlikely (n == EOF)) {
			if (!i) {
				return COB_STATUS_10_END_OF_FILE;
			} else {
				break;
			}
		}
		if (unlikely (n == 0 && cobsetptr->cob_ls_nulls != 0)) {
			n = getc ((FILE *)f->file);
			/* LCOV_EXCL_START */
			if (n == EOF) {
				return COB_STATUS_30_PERMANENT_ERROR;
			}
			/* LCOV_EXCL_STOP */
		} else {
			if (n == '\r') {
				continue;
			}
			if (n == '\n') {
				break;
			}
		}
		if (likely(i < f->record_max)) {
			*dataptr++ = (unsigned char)n;
			i++;
		}
	}
	if (i < f->record_max) {
		/* Fill the record with spaces */
		memset ((unsigned char *)f->record->data + i, ' ',
			f->record_max - i);
	}
	f->record->size = i;
	return COB_STATUS_00_SUCCESS;
}

static int
lineseq_write (cob_file *f, const int opt)
{
	unsigned char		*p;
	cob_linage		*lingptr;
	size_t			size;
	int			i;
	int			ret;

#ifdef	WITH_SEQRA_EXTFH
	int		extfh_ret;

	extfh_ret = extfh_sequential_write (f, opt);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#endif

	/* Determine the size to be written */
	if (unlikely (cobsetptr->cob_ls_fixed != 0)) {
		size = f->record->size;
	} else {
		for (i = (int)f->record->size - 1; i >= 0; --i) {
			if (f->record->data[i] != ' ') {
				break;
			}
		}
		size = i + 1;
	}

	if (unlikely (f->flag_select_features & COB_SELECT_LINAGE)) {
		if (f->flag_needs_top) {
			f->flag_needs_top = 0;
			lingptr = f->linorkeyptr;
			for (i = 0; i < lingptr->lin_top; ++i) {
				putc ('\n', (FILE *)f->file);
			}
		}
	}
	/* WRITE AFTER */
	if (opt & COB_WRITE_AFTER) {
		ret = cob_file_write_opt (f, opt);
		if (ret) {
			return ret;
		}
		f->flag_needs_nl = 1;
	}

	/* Write to the file */
	if (size) {
		if (unlikely (cobsetptr->cob_ls_nulls != 0)) {
			p = f->record->data;
			for (i = 0; i < (int)size; ++i, ++p) {
				if (*p < ' ') {
					putc (0, (FILE *)f->file);
				}
				putc ((int)(*p), (FILE *)f->file);
			}
		/* LCOV_EXCL_START */
		} else if (unlikely (fwrite (f->record->data, size, (size_t)1,
			     (FILE *)f->file) != 1)) {
			return COB_STATUS_30_PERMANENT_ERROR;
		}
		/* LCOV_EXCL_STOP */
	}

	if (unlikely (f->flag_select_features & COB_SELECT_LINAGE)) {
		putc ('\n', (FILE *)f->file);
	} else if (cobsetptr->cob_ls_uses_cr) {
		if (opt & COB_WRITE_PAGE) {
			putc ('\r', (FILE *)f->file);
		} else if ((opt & COB_WRITE_BEFORE) && f->flag_needs_nl) {
			putc ('\r', (FILE *)f->file);
		}
	}

	/* WRITE BEFORE */
	if (opt & COB_WRITE_BEFORE) {
		ret = cob_file_write_opt (f, opt);
		if (ret) {
			return ret;
		}
		f->flag_needs_nl = 0;
	}

	return COB_STATUS_00_SUCCESS;
}

/* RELATIVE */

static int
relative_start (cob_file *f, const int cond, cob_field *k)
{
	off_t		off;
	size_t		relsize;
	int		kindex;
	int		ksindex;
	int		kcond;
	struct stat	st;

#ifdef	WITH_SEQRA_EXTFH
	int	extfh_ret;

	extfh_ret = extfh_relative_start (f, cond, k);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#endif

	if (fstat (f->fd, &st) != 0 || st.st_size == 0) {
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}

	relsize = f->record_max + sizeof (f->record->size);

	/* Get the index */
	switch (cond) {
	case COB_FI:
		kcond = COB_GE;
		kindex = 0;
		break;
	case COB_LA:
		kcond = COB_LE;
		kindex = st.st_size / relsize;
		kindex--;
		break;
	case COB_LT:
	case COB_LE:
		kcond = cond;
		kindex = cob_get_int (k) - 1;
		/* Check against current file size */
		ksindex = st.st_size / relsize;
		ksindex--;
		if (kindex > ksindex) {
			kindex = ksindex;
			if (cond == COB_LT) {
				/* Cater for decrement below */
				kindex++;
			}
		}
		break;
	default:
		kcond = cond;
		kindex = cob_get_int (k) - 1;
		break;
	}

	if (kindex < 0) {
		/* Only valid ops are GE and GT in this case */
		switch (kcond) {
		case COB_GE:
			kindex = 0;
			break;
		case COB_GT:
			/* Set to cater for increment below */
			kindex = -1;
			break;
		default:
			return COB_STATUS_23_KEY_NOT_EXISTS;
		}
	}

	if (kcond == COB_LT) {
		kindex--;
		if (kindex < 0) {
			return COB_STATUS_23_KEY_NOT_EXISTS;
		}
	} else if (kcond == COB_GT) {
		kindex++;
	}

	f->flag_operation = 0;

	/* Seek index */
	for (;;) {
		if (kindex < 0) {
			break;
		}
		off = kindex * relsize;
		if (off >= st.st_size) {
			break;
		}
		if (lseek (f->fd, off, SEEK_SET) == (off_t)-1) {
			break;
		}

		/* Check if a valid record */
		if (read (f->fd, &f->record->size, sizeof (f->record->size))
		    == sizeof (f->record->size) && f->record->size > 0) {
#if	0	/* RXWRXW - Set key - COBOL standards */
			cob_set_int (k, kindex + 1);
#endif
			lseek (f->fd, off, SEEK_SET);
			return COB_STATUS_00_SUCCESS;
		}

		switch (kcond) {
		case COB_EQ:
			return COB_STATUS_23_KEY_NOT_EXISTS;
		case COB_LT:
		case COB_LE:
			kindex--;
			break;
		case COB_GT:
		case COB_GE:
			kindex++;
			break;
		}
	}
	return COB_STATUS_23_KEY_NOT_EXISTS;
}

static int
relative_read (cob_file *f, cob_field *k, const int read_opts)
{
	off_t	off;
	size_t	relsize;
	int	relnum;
#ifdef	WITH_SEQRA_EXTFH
	int	extfh_ret;

	extfh_ret = extfh_relative_read (f, k, read_opts);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#else
	COB_UNUSED (read_opts);
#endif

	if (unlikely (f->flag_operation != 0)) {
		f->flag_operation = 0;
		lseek (f->fd, (off_t)0, SEEK_CUR);
	}

	relnum = cob_get_int (k) - 1;
	if (relnum < 0) {
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}
	relsize = f->record_max + sizeof (f->record->size);
	off = relnum * relsize;
	if (lseek (f->fd, off, SEEK_SET) == (off_t)-1 ||
	    read (f->fd, &f->record->size, sizeof (f->record->size))
		   != sizeof (f->record->size)) {
			return COB_STATUS_23_KEY_NOT_EXISTS;
	}

	if (f->record->size == 0) {
		lseek (f->fd, off, SEEK_SET);
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}

	if (read (f->fd, f->record->data, f->record_max) != (int)f->record_max) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	return COB_STATUS_00_SUCCESS;
}

static int
relative_read_next (cob_file *f, const int read_opts)
{
	off_t		curroff;
	cob_s64_t	relsize;
	int		relnum;
	int		bytesread;
	cob_u32_t	moveback;
	struct stat	st;

#ifdef	WITH_SEQRA_EXTFH
	int		extfh_ret;

	extfh_ret = extfh_relative_read_next (f, read_opts);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#endif

	if (unlikely (f->flag_operation != 0)) {
		f->flag_operation = 0;
		lseek (f->fd, (off_t)0, SEEK_CUR);
	}

	relsize = f->record_max + sizeof (f->record->size);
	if (fstat (f->fd, &st) != 0 || st.st_size == 0) {
		return COB_STATUS_10_END_OF_FILE;
	}
	/* LCOV_EXCL_START */
	if (st.st_size < relsize) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	/* LCOV_EXCL_STOP */

	curroff = lseek (f->fd, (off_t)0, SEEK_CUR);
	moveback = 0;

	switch (read_opts & COB_READ_MASK) {
	case COB_READ_FIRST:
		curroff = lseek (f->fd, (off_t)0, SEEK_SET);
		break;
	case COB_READ_LAST:
		curroff = st.st_size - relsize;
		curroff = lseek (f->fd, curroff, SEEK_SET);
		moveback = 1;
		break;
	case COB_READ_PREVIOUS:
		if (f->flag_first_read) {
			break;
		} else if (curroff > relsize) {
			curroff -= (relsize * 2);
			curroff = lseek (f->fd, curroff, SEEK_SET);
		} else {
			return COB_STATUS_10_END_OF_FILE;
		}
		moveback = 1;
		break;
	case COB_READ_NEXT:
	default:
		break;
	}

	for (;;) {
		bytesread = read (f->fd, &f->record->size, sizeof (f->record->size));
		if (bytesread != sizeof (f->record->size)) {
			if (bytesread != 0) {
				return COB_STATUS_30_PERMANENT_ERROR;
			} else {
				break;
			}
		}

		if (f->record->size > 0) {
			if (read (f->fd, f->record->data, f->record_max) != (int)f->record_max) {
				return COB_STATUS_30_PERMANENT_ERROR;
			}
			if (f->keys[0].field) {
				relnum = (int)((curroff / relsize) + 1);
				cob_set_int (f->keys[0].field, 0);
				if (cob_add_int (f->keys[0].field, relnum,
						 COB_STORE_KEEP_ON_OVERFLOW) != 0) {
					lseek (f->fd, curroff, SEEK_SET);
					return COB_STATUS_14_OUT_OF_KEY_RANGE;
				}
			}
			if (moveback) {
				curroff -= relsize;
				curroff = lseek (f->fd, curroff, SEEK_SET);
			}
			return COB_STATUS_00_SUCCESS;
		}
		if (moveback) {
			if (curroff > relsize) {
				curroff -= (relsize * 2);
				curroff = lseek (f->fd, curroff, SEEK_SET);
			} else {
				break;
			}
		} else {
			curroff = lseek (f->fd, (off_t) f->record_max, SEEK_CUR);
		}
	}
	return COB_STATUS_10_END_OF_FILE;
}

static int
relative_write (cob_file *f, const int opt)
{
	off_t	off;
	size_t	size;
	size_t	relsize;
	int	i;
	int	kindex;
#ifdef	WITH_SEQRA_EXTFH
	int	extfh_ret;

	extfh_ret = extfh_relative_write (f, opt);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#else
	COB_UNUSED (opt);
#endif

	if (unlikely (f->flag_operation == 0)) {
		f->flag_operation = 1;
		lseek (f->fd, (off_t)0, SEEK_CUR);
	}

	relsize = f->record_max + sizeof (f->record->size);
	if (f->access_mode != COB_ACCESS_SEQUENTIAL) {
		kindex = cob_get_int (f->keys[0].field) - 1;
		if (kindex < 0) {
			return COB_STATUS_24_KEY_BOUNDARY;
		}
		off = (off_t) (relsize * kindex);
		if (lseek (f->fd, off, SEEK_SET) == (off_t)-1) {
			return COB_STATUS_24_KEY_BOUNDARY;
		}
		if (read (f->fd, &size, sizeof (size)) > 0) {
			if (size > 0) {
				return COB_STATUS_22_KEY_EXISTS;
			}
		}
	} else {
		off = lseek (f->fd, (off_t)0, SEEK_CUR);
	}
	lseek (f->fd, off, SEEK_SET);

	if (write (f->fd, &f->record->size, sizeof (f->record->size)) != sizeof (f->record->size)) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	if (write (f->fd, f->record->data, f->record_max) != (int)f->record_max) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}

	/* Update RELATIVE KEY */
	if (f->access_mode == COB_ACCESS_SEQUENTIAL) {
		if (f->keys[0].field) {
			off += relsize;
			i = (int)(off / relsize);
			cob_set_int (f->keys[0].field, i);
		}
	}

	return COB_STATUS_00_SUCCESS;
}

static int
relative_rewrite (cob_file *f, const int opt)
{
	off_t	off;
	size_t	relsize;
	int	relnum;
#ifdef	WITH_SEQRA_EXTFH
	int	extfh_ret;

	extfh_ret = extfh_relative_rewrite (f, opt);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#else
	COB_UNUSED (opt);
#endif

	f->flag_operation = 1;
	if (f->access_mode == COB_ACCESS_SEQUENTIAL) {
		lseek (f->fd, -(off_t) f->record_max, SEEK_CUR);
	} else {
		relsize = f->record_max + sizeof (f->record->size);
		relnum = cob_get_int (f->keys[0].field) - 1;
		if (relnum < 0) {
			return COB_STATUS_24_KEY_BOUNDARY;
		}
		off = relnum * relsize;
		if (lseek (f->fd, off, SEEK_SET) == (off_t)-1 ||
		    read (f->fd, &f->record->size, sizeof (f->record->size))
			   != sizeof (f->record->size)) {
				return COB_STATUS_23_KEY_NOT_EXISTS;
		}
		lseek (f->fd, (off_t)0, SEEK_CUR);
	}

	if (write (f->fd, f->record->data, f->record_max) != (int)f->record_max) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	return COB_STATUS_00_SUCCESS;
}

static int
relative_delete (cob_file *f)
{
	off_t	off;
	size_t	relsize;
	int	relnum;
#ifdef	WITH_SEQRA_EXTFH
	int	extfh_ret;

	extfh_ret = extfh_relative_delete (f);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#endif

	f->flag_operation = 1;
	relnum = cob_get_int (f->keys[0].field) - 1;
	if (relnum < 0) {
		return COB_STATUS_24_KEY_BOUNDARY;
	}
	relsize = f->record_max + sizeof (f->record->size);
	off = relnum * relsize;
	if (lseek (f->fd, off, SEEK_SET) == (off_t)-1 ||
	    read (f->fd, &f->record->size, sizeof (f->record->size))
		   != sizeof (f->record->size)) {
			return COB_STATUS_23_KEY_NOT_EXISTS;
	}
	lseek (f->fd, off, SEEK_SET);

	f->record->size = 0;
	if (write (f->fd, &f->record->size, sizeof (f->record->size)) != sizeof (f->record->size)) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	lseek (f->fd, (off_t) f->record_max, SEEK_CUR);
	return COB_STATUS_00_SUCCESS;
}

/* INDEXED */

#ifdef	WITH_ANY_ISAM

static int
fisretsts (const int dfltsts)
{
	/* Translate ISAM status to COBOL status */
	switch (iserrno) {
	case 0:
		return COB_STATUS_00_SUCCESS;
	case ENOREC:
		return COB_STATUS_23_KEY_NOT_EXISTS;
	case EENDFILE:
		if (dfltsts != COB_STATUS_23_KEY_NOT_EXISTS) {
			return COB_STATUS_10_END_OF_FILE;
		}
		break;
	case EDUPL:
	case EKEXISTS:
		return COB_STATUS_22_KEY_EXISTS;
	case EPERM:
	case EACCES:
	case EISDIR:
		return COB_STATUS_37_PERMISSION_DENIED;
	case ENOENT:
		return COB_STATUS_35_NOT_EXISTS;
	case EBADFILE:
		return COB_STATUS_30_PERMANENT_ERROR;
	case ELOCKED:
		return COB_STATUS_51_RECORD_LOCKED;
	case EFLOCKED:
		return COB_STATUS_61_FILE_SHARING;
	case ENOCURR:
		if (dfltsts != COB_STATUS_10_END_OF_FILE) {
			return COB_STATUS_21_KEY_INVALID;
		}
		break;
	default:
		break;
	}
	return dfltsts;
}

/* Free memory for indexfile packet */

static void
freefh (struct indexfile *fh)
{
	if (fh == NULL) {
		return;
	}
	if (fh->filename) {
		cob_free ((void *)fh->filename);
	}
	if (fh->savekey) {
		cob_free ((void *)fh->savekey);
	}
	if (fh->recwrk) {
		cob_free ((void *)fh->recwrk);
	}
	cob_free ((void *)fh);
}

/* Restore ISAM file positioning */
static void
restorefileposition (cob_file *f)
{
	struct indexfile	*fh;
	struct keydesc		k0;

	fh = f->file;
	memset ((void *)&k0, 0, sizeof(k0));
	if (fh->saverecnum >= 0) {
		/* Switch back to index */
		isrecnum = fh->saverecnum;
		/* Switch to recnum mode */
		isstart (fh->isfd, &k0, 0, (void *)fh->recwrk, ISEQUAL);
		/* Read by record number */
		isread (fh->isfd, (void *)fh->recwrk, ISEQUAL);
		isstart (fh->isfd, &fh->key[fh->curkey], 0,
			 (void *)fh->recwrk, ISEQUAL);
		isread (fh->isfd, (void *)fh->recwrk, ISEQUAL);
		while (isrecnum != fh->saverecnum) {
			/* Read back into position */
			if (isread (fh->isfd, (void *)fh->recwrk, fh->readdir)) {
				break;
			}
		}
		if (isrecnum == fh->saverecnum) {
			if (fh->readdir == ISNEXT) {
				/* Back off by one so next read gets this */
				isread (fh->isfd, (void *)fh->recwrk, ISPREV);
			} else {
				isread (fh->isfd, (void *)fh->recwrk, ISNEXT);
			}
		}
	} else if (fh->readdone && fh->curkey == 0) {
		memcpy (fh->recwrk + fh->key[0].k_start,
			fh->savekey, (size_t)fh->key[0].k_leng);
		isstart (fh->isfd, &fh->key[fh->curkey], 0,
			 (void *)fh->recwrk, ISGTEQ);
	}
}

/* Save ISAM file positioning information for later 'restorefileposition' */

static void
savefileposition (cob_file *f)
{
	struct indexfile	*fh;

	fh = f->file;
	if (fh->curkey >= 0 && fh->readdir != -1) {
		/* Switch back to index */
		if (fh->wrkhasrec != fh->readdir) {
			fh->eofpending = 0;
			fh->wrkhasrec = 0;
			/* Read next record in file */
			if (isread (fh->isfd, (void *)fh->recwrk, fh->readdir)) {
				fh->saverecnum = -1;
				fh->saveerrno = iserrno;
				if (fh->saveerrno == EENDFILE ||
				    fh->saveerrno == ENOREC)  {
					fh->eofpending = fh->readdir;
				}
			} else {
				fh->saverecnum = isrecnum;
				fh->saveerrno = 0;
			}
			/* Restore saved record data */
			memcpy (fh->recwrk, f->record->data, f->record_max);
		}
	} else {
		fh->saverecnum = -1;
	}
}
#endif	/* WITH_ANY_ISAM */

#ifdef	WITH_DB

#if	0	/* RXWRXW - BDB msg */
static void
bdb_msgcall_set (DB_ENV *dbe, const char *err)
{
	COB_UNUSED (dbe);

	cob_runtime_error (_("BDB error: %s"), err);
	cob_stop_run (1);
}

static void
bdb_errcall_set (DB_ENV *dbe, const char *prefix, const char *err)
{
	COB_UNUSED (dbe);

	cob_runtime_error (_("BDB error: %s %s"), prefix, err);
	cob_stop_run (1);
}
#endif

static void
join_environment (void)
{
	cob_u32_t	flags;
	int		ret;

	if (cobsetptr->bdb_home == NULL) {
		return;
	}
	ret = db_env_create (&bdb_env, 0);
	if (ret) {
		cob_runtime_error (_("cannot join BDB environment (%s), error: %d %s"),
				   "env_create", ret, db_strerror (ret));
		cob_stop_run (1);
	}
#if	0	/* RXWRXW - BDB msg */
	bdb_env->set_errcall (bdb_env, bdb_errcall_set);
#if (DB_VERSION_MAJOR > 4) || ((DB_VERSION_MAJOR == 4) && (DB_VERSION_MINOR > 2))
	bdb_env->set_msgcall (bdb_env, bdb_msgcall_set);
#endif
#else
	bdb_env->set_errfile (bdb_env, stderr);
#if (DB_VERSION_MAJOR > 4) || ((DB_VERSION_MAJOR == 4) && (DB_VERSION_MINOR > 2))
	bdb_env->set_msgfile (bdb_env, stderr);
#endif
#endif
	bdb_env->set_cachesize (bdb_env, 0, 2*1024*1024, 0);
	bdb_env->set_alloc (bdb_env, cob_malloc, realloc, cob_free);
	flags = DB_CREATE | DB_INIT_MPOOL | DB_INIT_CDB;
	ret = bdb_env->open (bdb_env, cobsetptr->bdb_home, flags, 0);
	if (ret) {
		cob_runtime_error (_("cannot join BDB environment (%s), error: %d %s"),
				   "env->open", ret, db_strerror (ret));
		bdb_env->close (bdb_env, 0);
		bdb_env = NULL;
		cob_stop_run (1);
	}
#if (DB_VERSION_MAJOR > 4) || ((DB_VERSION_MAJOR == 4) && (DB_VERSION_MINOR > 1))
	bdb_env->get_data_dirs (bdb_env, &bdb_data_dir);
#endif
	bdb_env->lock_id (bdb_env, &bdb_lock_id);
}

static int
lock_record (cob_file *f, const char *key, const unsigned int keylen)
{
	struct indexed_file	*p;
	size_t			len;
	int			ret;
	DBT			dbt;

	p = f->file;
	len = keylen + p->filenamelen + 1;
	if (len > rlo_size) {
		cob_free (record_lock_object);
		record_lock_object = cob_malloc (len);
		rlo_size = len;
	}
	memcpy ((char *)record_lock_object, p->filename,
		(size_t)(p->filenamelen + 1));
	memcpy ((char *)record_lock_object + p->filenamelen + 1, key,
		(size_t)keylen);
	memset (&dbt, 0, sizeof(dbt));
	dbt.size = (cob_dbtsize_t) len;
	dbt.data = record_lock_object;
	ret = bdb_env->lock_get (bdb_env, p->bdb_lock_id, DB_LOCK_NOWAIT,
				&dbt, DB_LOCK_WRITE, &p->bdb_record_lock);
	if (!ret) {
		p->record_locked = 1;
	}
	return ret;
}

static int
test_record_lock (cob_file *f, const char *key, const unsigned int keylen)
{
	struct indexed_file	*p;
	size_t			len;
	int			ret;
	DBT			dbt;
	DB_LOCK			test_lock;

	p = f->file;
	len = keylen + p->filenamelen + 1;
	if (len > rlo_size) {
		cob_free (record_lock_object);
		record_lock_object = cob_malloc (len);
		rlo_size = len;
	}
	memcpy ((char *)record_lock_object, p->filename,
		(size_t)(p->filenamelen + 1));
	memcpy ((char *)record_lock_object + p->filenamelen + 1, key,
		(size_t)keylen);
	memset (&dbt, 0, sizeof(dbt));
	dbt.size = (cob_dbtsize_t) len;
	dbt.data = record_lock_object;
	ret = bdb_env->lock_get (bdb_env, p->bdb_lock_id, DB_LOCK_NOWAIT,
				&dbt, DB_LOCK_WRITE, &test_lock);
	if (!ret) {
		bdb_env->lock_put (bdb_env, &test_lock);
	}
	return ret;
}

static int
unlock_record (cob_file *f)
{
	struct indexed_file	*p;
	int			ret;

	p = f->file;
	if (p->record_locked == 0) {
		return 0;
	}
	ret = bdb_env->lock_put (bdb_env, &p->bdb_record_lock);
	p->record_locked = 0;
	return ret;
}

/* Get the next number in a set of duplicates */
static unsigned int
get_dupno (cob_file *f, const cob_u32_t i)
{
	struct indexed_file	*p;
	int			ret;
	unsigned int		dupno;

	p = f->file;
	dupno = 0;
	DBT_SET (p->key, f->keys[i].field);
	memcpy (p->temp_key, p->key.data, (size_t)p->key.size);
	p->db[i]->cursor (p->db[i], NULL, &p->cursor[i], 0);
	ret = DB_SEQ (p->cursor[i], DB_SET_RANGE);
	while (ret == 0 && memcmp (p->key.data, p->temp_key, (size_t)p->key.size) == 0) {
		memcpy (&dupno, (cob_u8_ptr)p->data.data + f->keys[0].field->size,
			sizeof(unsigned int));
		ret = DB_SEQ (p->cursor[i], DB_NEXT);
	}
	p->cursor[i]->c_close (p->cursor[i]);
	p->cursor[i] = NULL;
	return ++dupno;
}

static int
check_alt_keys (cob_file *f, const int rewrite)
{
	struct indexed_file	*p;
	size_t			i;
	int			ret;

	p = f->file;
	for (i = 1; i < f->nkeys; ++i) {
		if (!f->keys[i].flag) {
			DBT_SET (p->key, f->keys[i].field);
			ret = DB_GET (p->db[i], 0);
			if (ret == 0) {
				if (rewrite) {
					if (memcmp (p->data.data, f->keys[0].field->data, f->keys[0].field->size)) {
						return 1;
					}
				} else {
					return 1;
				}
			}
		}
	}
	return 0;
}

static int
indexed_write_internal (cob_file *f, const int rewrite, const int opt)
{
	struct indexed_file	*p;
	cob_u32_t		i;
	unsigned int		dupno;
	cob_u32_t		flags;
	int			close_cursor;

	p = f->file;
	if (bdb_env) {
		flags = DB_WRITECURSOR;
	} else {
		flags = 0;
	}
	if (p->write_cursor_open) {
		close_cursor = 0;
	} else {
		p->db[0]->cursor (p->db[0], NULL, &p->cursor[0], flags);
		p->write_cursor_open = 1;
		close_cursor = 1;
	}

	/* Check duplicate alternate keys */
	if (f->nkeys > 1 && !rewrite) {
		if (check_alt_keys (f, 0)) {
			if (close_cursor) {
				p->cursor[0]->c_close (p->cursor[0]);
				p->cursor[0] = NULL;
				p->write_cursor_open = 0;
			}
			return COB_STATUS_22_KEY_EXISTS;
		}
		DBT_SET (p->key, f->keys[0].field);
	}

	/* Write data */
	if (p->cursor[0]->c_get (p->cursor[0], &p->key, &p->data, DB_SET) == 0) {
		if (close_cursor) {
			p->cursor[0]->c_close (p->cursor[0]);
			p->cursor[0] = NULL;
			p->write_cursor_open = 0;
		}
		return COB_STATUS_22_KEY_EXISTS;
	}
	p->data.data = f->record->data;
	p->data.size = (cob_dbtsize_t) f->record->size;
	p->cursor[0]->c_put (p->cursor[0], &p->key, &p->data, DB_KEYFIRST);

	/* Write secondary keys */
	p->data = p->key;
	for (i = 1; i < f->nkeys; ++i) {
		if (rewrite && ! p->rewrite_sec_key[i]) {
			continue;
		}
		if (f->keys[i].flag) {
			flags =  0;
			dupno = get_dupno(f, i);
			memcpy (p->temp_key, f->keys[0].field->data,
				   f->keys[0].field->size);
			memcpy (p->temp_key + f->keys[0].field->size, &dupno,
				   sizeof(unsigned int));
			p->data.data = p->temp_key;
			p->data.size = f->keys[0].field->size + sizeof(unsigned int);;
		} else {
			flags = DB_NOOVERWRITE;
		}

		DBT_SET (p->key, f->keys[i].field);
		if (DB_PUT (p->db[i], flags) != 0) {
			if (close_cursor) {
				p->cursor[0]->c_close (p->cursor[0]);
				p->cursor[0] = NULL;
				p->write_cursor_open = 0;
			}
			return COB_STATUS_22_KEY_EXISTS;
		}
	}

	if (opt & COB_WRITE_LOCK) {
		if (bdb_env != NULL) {
			DBT_SET (p->key, f->keys[0].field);
			if (lock_record (f, p->key.data, p->key.size)) {
				if (close_cursor) {
					p->cursor[0]->c_close (p->cursor[0]);
					p->cursor[0] = NULL;
					p->write_cursor_open = 0;
				}
				return COB_STATUS_51_RECORD_LOCKED;
			}
		}
	}
	if (close_cursor) {
		p->cursor[0]->c_close (p->cursor[0]);
		p->cursor[0] = NULL;
		p->write_cursor_open = 0;
	}
	return COB_STATUS_00_SUCCESS;
}

static int
indexed_start_internal (cob_file *f, const int cond, cob_field *key,
			const int read_opts, const int test_lock)
{
	struct indexed_file	*p;
	int			ret;
	unsigned int		dupno;

	dupno = 0;
	p = f->file;
	/* Look up for the key */
	for (p->key_index = 0; p->key_index < f->nkeys; p->key_index++) {
		if (f->keys[p->key_index].field->data == key->data) {
			break;
		}
	}

	/* Search */
	DBT_SET (p->key, key);
	/* The open cursor makes this function atomic */
	if (p->key_index != 0) {
		p->db[0]->cursor (p->db[0], NULL, &p->cursor[0], 0);
	}
	p->db[p->key_index]->cursor (p->db[p->key_index], NULL, &p->cursor[p->key_index], 0);
	if (cond == COB_FI) {
		ret = DB_SEQ (p->cursor[p->key_index], DB_FIRST);
	} else if (cond == COB_LA) {
		ret = DB_SEQ (p->cursor[p->key_index], DB_LAST);
	} else {
		ret = DB_SEQ (p->cursor[p->key_index], DB_SET_RANGE);
	}
	switch (cond) {
	case COB_EQ:
		if (ret == 0) {
			ret = memcmp (p->key.data, key->data, key->size);
		}
		break;
	case COB_LT:
		if (ret != 0) {
			ret = DB_SEQ (p->cursor[p->key_index], DB_LAST);
		} else {
			ret = DB_SEQ (p->cursor[p->key_index], DB_PREV);
		}
		break;
	case COB_LE:
		if (ret != 0) {
			ret = DB_SEQ (p->cursor[p->key_index], DB_LAST);
		} else if (memcmp (p->key.data, key->data, key->size) != 0) {
			ret = DB_SEQ (p->cursor[p->key_index], DB_PREV);
		} else if (f->keys[p->key_index].flag) {
			ret = DB_SEQ (p->cursor[p->key_index], DB_NEXT_NODUP);
			if (ret != 0) {
				ret = DB_SEQ (p->cursor[p->key_index], DB_LAST);
			} else {
				ret = DB_SEQ (p->cursor[p->key_index], DB_PREV);
			}
		}
		break;
	case COB_GT:
		while (ret == 0 && memcmp (p->key.data, key->data, key->size) == 0) {
			ret = DB_SEQ (p->cursor[p->key_index], DB_NEXT);
		}
		break;
	case COB_GE:
		/* nothing */
		break;
	case COB_FI:
		/* nothing */
		break;
	case COB_LA:
		/* nothing */
		break;
	}

	if (ret == 0 && p->key_index > 0) {
		/* Temporarily save alternate key */
		memcpy (p->temp_key, p->key.data, f->keys[p->key_index].field->size);
		if (f->keys[p->key_index].flag) {
			memcpy (&dupno, (cob_u8_ptr)p->data.data + f->keys[0].field->size, sizeof(unsigned int));
		}
		p->key.data = p->data.data;
		p->key.size = f->keys[0].field->size;
		ret = DB_GET (p->db[0], 0);
	}

	if (ret == 0 && test_lock) {
		if (!(read_opts & COB_READ_IGNORE_LOCK)) {
			ret = test_record_lock (f, p->key.data, p->key.size);
			if (ret) {
				p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
				p->cursor[p->key_index] = NULL;
				if (p->key_index != 0) {
					p->cursor[0]->c_close (p->cursor[0]);
					p->cursor[0] = NULL;
				}
				return COB_STATUS_51_RECORD_LOCKED;
			}
		}
		if (read_opts & COB_READ_LOCK) {
			ret = lock_record (f, p->key.data, p->key.size);
			if (ret) {
				p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
				p->cursor[p->key_index] = NULL;
				if (p->key_index != 0) {
					p->cursor[0]->c_close (p->cursor[0]);
					p->cursor[0] = NULL;
				}
				return COB_STATUS_51_RECORD_LOCKED;
			}
		}
	}

	if (ret == 0) {
		if (p->key_index == 0) {
			memcpy (p->last_readkey[0], p->key.data, f->keys[0].field->size);
		} else {
			memcpy (p->last_readkey[p->key_index],
				    p->temp_key, f->keys[p->key_index].field->size);
			memcpy (p->last_readkey[p->key_index + f->nkeys], p->key.data, f->keys[0].field->size);
			if (f->keys[p->key_index].flag) {
				p->last_dupno[p->key_index] = dupno;
			}
		}
	}

	p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
	p->cursor[p->key_index] = NULL;
	if (p->key_index != 0) {
		p->cursor[0]->c_close (p->cursor[0]);
		p->cursor[0] = NULL;
	}

	return (ret == 0) ? COB_STATUS_00_SUCCESS : COB_STATUS_23_KEY_NOT_EXISTS;
}

static int
indexed_delete_internal (cob_file *f, const int rewrite)
{
	struct indexed_file	*p;
	size_t			i;
	size_t			offset;
	DBT			prim_key;
	int			ret;
	cob_u32_t		flags;
	int			close_cursor;

	p = f->file;
	if (bdb_env) {
		flags = DB_WRITECURSOR;
	} else {
		flags = 0;
	}
	if (p->write_cursor_open) {
		close_cursor = 0;
	} else {
		p->db[0]->cursor (p->db[0], NULL, &p->cursor[0], flags);
		p->write_cursor_open = 1;
		close_cursor = 1;
	}
	if (bdb_env != NULL) {
		unlock_record (f);
	}
	/* Find the primary key */
	if (f->access_mode != COB_ACCESS_SEQUENTIAL) {
		DBT_SET (p->key, f->keys[0].field);
	}
	ret = DB_SEQ (p->cursor[0], DB_SET);
	if (ret != 0 && f->access_mode != COB_ACCESS_SEQUENTIAL) {
		if (close_cursor) {
			p->cursor[0]->c_close (p->cursor[0]);
			p->cursor[0] = NULL;
			p->write_cursor_open = 0;
		}
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}
	if (bdb_env != NULL) {
		ret = test_record_lock (f, p->key.data, p->key.size);
		if (ret) {
			if (close_cursor) {
				p->cursor[0]->c_close (p->cursor[0]);
				p->cursor[0] = NULL;
				p->write_cursor_open = 0;
			}
			return COB_STATUS_51_RECORD_LOCKED;
		}
	}
	prim_key = p->key;

	/* Delete the secondary keys */
	offset = (char *) p->data.data - (char *) f->record->data;
	for (i = 1; i < f->nkeys; ++i) {
		DBT_SET (p->key, f->keys[i].field);
		p->key.data = (char *)p->key.data + offset;
		/* rewrite: no delete if secondary key is unchanged */
		if (rewrite) {
			p->rewrite_sec_key[i] = memcmp (p->key.data, f->keys[i].field->data, (size_t)p->key.size);
			if (!p->rewrite_sec_key[i]) {
				continue;
			}
		}
		if (!f->keys[i].flag) {
			DB_DEL (p->db[i], &p->key, 0);
		} else {
			DBT	sec_key = p->key;

			p->db[i]->cursor (p->db[i], NULL, &p->cursor[i], flags);
			if (DB_SEQ (p->cursor[i], DB_SET_RANGE) == 0) {
				while (sec_key.size == p->key.size &&
				       memcmp (p->key.data, sec_key.data,
					       (size_t)sec_key.size) == 0) {
					if (memcmp (p->data.data, prim_key.data,
						    (size_t)prim_key.size) == 0) {
						p->cursor[i]->c_del (p->cursor[i], 0);
					}
					if (DB_SEQ (p->cursor[i], DB_NEXT) != 0) {
						break;
					}
				}
			}
			p->cursor[i]->c_close (p->cursor[i]);
			p->cursor[i] = NULL;
		}
	}

	/* Delete the record */
	p->cursor[0]->c_del (p->cursor[0], 0);

	if (close_cursor) {
		p->cursor[0]->c_close (p->cursor[0]);
		p->cursor[0] = NULL;
		p->write_cursor_open = 0;
	}
	return COB_STATUS_00_SUCCESS;
}

/* Check if a file exists in bdb data dirs */

static int
is_absolute (const char *filename)
{
#ifdef	_WIN32
	if (filename[0] == '/' || filename[0] == '\\') {
		return 1;
	} else {
		if (isalpha (filename[0]) && filename[1] == ':' &&
		  (filename[2] == '/' || filename[2] == '\\')) {
			return 1;
		} else {
			return 0;
		}
	}
#else
	if (filename[0] == '/') {
		return 1;
	} else {
		return 0;
	}
#endif
}

static int
bdb_nofile (const char *filename)
{
	cob_u32_t	i;

	if (!bdb_env || is_absolute (filename)) {
		errno = 0;
		if (access (filename, F_OK) && errno == ENOENT) {
			return 1;
		}
		return 0;
	}

	for (i = 0; bdb_data_dir && bdb_data_dir[i]; ++i) {
		bdb_buff[COB_SMALL_MAX] = 0;
		if (is_absolute (bdb_data_dir[i])) {
			snprintf (bdb_buff, (size_t)COB_SMALL_MAX, "%s%c%s",
				  bdb_data_dir[i], SLASH_CHAR, filename);
		} else {
			snprintf (bdb_buff, (size_t)COB_SMALL_MAX, "%s%c%s%c%s",
				  cobsetptr->bdb_home, SLASH_CHAR, bdb_data_dir[i], SLASH_CHAR, filename);
		}
		errno = 0;
		if (access (bdb_buff, F_OK) == 0 || errno != ENOENT) {
			return 0;
		}
	}
	if (i == 0) {
		bdb_buff[COB_SMALL_MAX] = 0;
		snprintf (bdb_buff, (size_t)COB_SMALL_MAX, "%s%c%s",
			  cobsetptr->bdb_home, SLASH_CHAR, filename);
		errno = 0;
		if (access (bdb_buff, F_OK) == 0 || errno != ENOENT) {
			return 0;
		}
	}
	return 1;
}

#endif	/* WITH_DB */

/* Delete file */

static void
indexed_file_delete (cob_file *f, const char *filename)
{
#ifdef	WITH_ANY_ISAM
	COB_UNUSED (f);

	snprintf (file_open_buff, (size_t)COB_FILE_MAX, "%s.idx", filename);
	file_open_buff[COB_FILE_MAX] = 0;
	unlink (file_open_buff);
	snprintf (file_open_buff, (size_t)COB_FILE_MAX, "%s.dat", filename);
	file_open_buff[COB_FILE_MAX] = 0;
	unlink (file_open_buff);
#elif	defined(WITH_DB)
	size_t	i;

	for (i = 0; i < f->nkeys; ++i) {
		if (i == 0) {
			snprintf (file_open_buff, (size_t)COB_FILE_MAX, "%s",
				  filename);
		} else {
			snprintf (file_open_buff, (size_t)COB_FILE_MAX, "%s.%d",
				  filename, (int)i);
		}
		file_open_buff[COB_FILE_MAX] = 0;
		unlink (file_open_buff);
	}
#else
	COB_UNUSED (f);
	COB_UNUSED (filename);
#endif
}

/* OPEN INDEXED file */

static int
indexed_open (cob_file *f, char *filename, const int mode, const int sharing)
{
	/* Note filename points to file_open_name */
	/* cob_chk_file_mapping manipulates file_open_name directly */

#ifdef	WITH_INDEX_EXTFH
	int		ret;

	ret = extfh_indexed_locate (f, filename);
	switch (ret) {
	case COB_NOT_CONFIGURED:
		cob_chk_file_mapping ();
		if (access (filename, F_OK) && errno == ENOENT) {
			if (mode != COB_OPEN_OUTPUT && f->flag_optional == 0) {
				return COB_STATUS_35_NOT_EXISTS;
			}
		}
		break;
	case COB_STATUS_00_SUCCESS:
		break;
	default:
		return ret;
	}
	ret = extfh_indexed_open (f, filename, mode, sharing);
	switch (ret) {
	case COB_STATUS_00_SUCCESS:
		f->open_mode = mode;
		break;
	case COB_STATUS_35_NOT_EXISTS:
		if (f->flag_optional) {
			f->open_mode = mode;
			f->flag_nonexistent = 1;
			f->flag_end_of_file = 1;
			f->flag_begin_of_file = 1;
			return COB_STATUS_05_SUCCESS_OPTIONAL;
		}
		break;
	}
	return ret;

#elif	defined(WITH_ANY_ISAM)

	struct indexfile	*fh;
	size_t			k;
	int			ret;
	int			omode;
	int			lmode;
	int			vmode;
	int			dobld;
	int			isfd;
	int			checkvalue;
	struct dictinfo		di;		/* Defined in (c|d|vb)isam.h */

	COB_UNUSED (sharing);

	cob_chk_file_mapping ();

	if (mode == COB_OPEN_INPUT) {
		checkvalue = R_OK;
	} else {
		checkvalue = R_OK | W_OK;
	}

	snprintf (file_open_buff, (size_t)COB_FILE_MAX, "%s.idx", filename);
	file_open_buff[COB_FILE_MAX] = 0;
	errno = 0;
	if (access (file_open_buff, checkvalue)) {
		if (!(errno == ENOENT &&
		      (mode == COB_OPEN_OUTPUT || f->flag_optional == 1))) {
			switch (errno) {
			case ENOENT:
				return COB_STATUS_35_NOT_EXISTS;
			case EACCES:
				return COB_STATUS_37_PERMISSION_DENIED;
			default:
				return COB_STATUS_30_PERMANENT_ERROR;
			}
		}
	}

	snprintf (file_open_buff, (size_t)COB_FILE_MAX, "%s.dat", filename);
	file_open_buff[COB_FILE_MAX] = 0;
	errno = 0;
	if (access (file_open_buff, checkvalue)) {
		if (!(errno == ENOENT &&
		      (mode == COB_OPEN_OUTPUT || f->flag_optional == 1))) {
			switch (errno) {
			case ENOENT:
				return COB_STATUS_35_NOT_EXISTS;
			case EACCES:
				return COB_STATUS_37_PERMISSION_DENIED;
			default:
				return COB_STATUS_30_PERMANENT_ERROR;
			}
		}
	}

	ret = COB_STATUS_00_SUCCESS;
	omode = 0;
	lmode = 0;
	vmode = 0;
	dobld = 0;
	isfd = -1;
#ifdef	ISVARLEN
	if (f->record_min != f->record_max) {
		vmode = ISVARLEN;
		isreclen = f->record_min;
	}
#endif
	if (!f->lock_mode) {
		if (mode != COB_OPEN_INPUT) {
			lmode = ISEXCLLOCK;
		} else {
			lmode = ISMANULOCK;
		}
	} else if ((f->lock_mode & COB_FILE_EXCLUSIVE)) {
		lmode = ISEXCLLOCK;
	} else if ((f->lock_mode & COB_LOCK_AUTOMATIC) && mode != COB_OPEN_INPUT) {
		lmode = ISAUTOLOCK;
	} else {
		lmode = ISMANULOCK;
	}
	switch (mode) {
	case COB_OPEN_INPUT:
		omode = ISINPUT;
		break;
	case COB_OPEN_OUTPUT:
		lmode = ISEXCLLOCK;
		omode = ISOUTPUT;
		iserrno = 0;
		isfd = isopen ((void *)filename, ISINPUT | ISEXCLLOCK | vmode);
		if (iserrno == EFLOCKED) {
			return COB_STATUS_61_FILE_SHARING;
		} else {
			if (isfd >= 0) {
				isfullclose (isfd);
			}
			iserase ((void *)filename);
		}
		dobld = 1;
		break;
	case COB_OPEN_I_O:
		omode = ISINOUT;
		break;
	case COB_OPEN_EXTEND:
		lmode = ISEXCLLOCK;
		omode = ISINOUT;
		break;
	}
	fh = cob_malloc (sizeof(struct indexfile) +
			 ((sizeof (struct keydesc)) * (f->nkeys + 1)));
	/* Copy index information */
	for (k = 0; k < f->nkeys; ++k) {
		fh->key[k].k_flags = f->keys[k].flag ? ISDUPS : ISNODUPS;
		fh->key[k].k_nparts = 1;		/* Single field key */
		fh->key[k].k_start = f->keys[k].offset;
		fh->key[k].k_leng = f->keys[k].field->size;
		if (fh->lenkey < fh->key[k].k_leng) {
			fh->lenkey = fh->key[k].k_leng;
		}
		fh->key[k].k_type = CHARTYPE;
	}
	iserrno = 0;
	fh->lmode = 0;
	if (dobld) {
dobuild:
		isfd = isbuild ((void *)filename, (int)f->record_max, &fh->key[0],
				vmode | ISINOUT | ISEXCLLOCK);
	} else {
		if (lmode == ISAUTOLOCK) {
			fh->lmode = ISLOCK;
			lmode = ISMANULOCK;
		}
		isfd = isopen ((void *)filename, omode | lmode | vmode);
		if (isfd < 0) {
			if (f->flag_optional) {
				if (mode == COB_OPEN_EXTEND ||
				    mode == COB_OPEN_I_O) {
					dobld = 1;
					ret = COB_STATUS_05_SUCCESS_OPTIONAL;
					goto dobuild;
				}
#if	0	/* RXWRXW - freefh */
				fh->isfd = isfd;
				fh->filename = cob_strdup (filename);
				/* Active index is unknown at this time */
				fh->curkey = -1;
				f->file = fh;
#endif
				freefh (fh);
				f->open_mode = mode;
				f->flag_end_of_file = 1;
				f->flag_begin_of_file = 1;
				if (f->flag_nonexistent) {
					return COB_STATUS_00_SUCCESS;
				}
				f->flag_nonexistent = 1;
				return COB_STATUS_05_SUCCESS_OPTIONAL;
			}
		} else {
			memset(&di, 0, sizeof(di));
			isindexinfo (isfd, (void *)&di, 0);
			/* Mask off ISVARLEN */
			fh->nkeys = di.di_nkeys & 0x7F;
			if (fh->nkeys != f->nkeys) {
				ret = COB_STATUS_39_CONFLICT_ATTRIBUTE;
			}
			for (k = 0; k < fh->nkeys && !ret; ++k) {
				memset (&fh->key[k], 0, sizeof(struct keydesc));
				isindexinfo (isfd, &fh->key[k], (int)(k+1));
				if (fh->lenkey < fh->key[k].k_leng) {
					fh->lenkey = fh->key[k].k_leng;
				}
				/* Verify that COBOL keys match real ISAM keys */
				if (f->keys[k].flag) {
					if (!(fh->key[k].k_flags & ISDUPS)) {
						ret = COB_STATUS_39_CONFLICT_ATTRIBUTE;
					}
				} else {
					if (fh->key[k].k_flags & ISDUPS) {
						ret = COB_STATUS_39_CONFLICT_ATTRIBUTE;
					}
				}
				if (fh->key[k].k_nparts != 1 ||
				    (int)fh->key[k].k_start != (int)f->keys[k].offset ||
				    (int)fh->key[k].k_leng != (int)f->keys[k].field->size) {
					ret = COB_STATUS_39_CONFLICT_ATTRIBUTE;
				}
			}
		}
	}
	if (isfd < 0) {
		ret = fisretsts (COB_STATUS_30_PERMANENT_ERROR);
		freefh (fh);
		return ret;
	}
	if (ret > 9) {
		isfullclose (isfd);
		freefh (fh);
		return ret;
	}
	if (dobld) {
		for (k = 1; k < f->nkeys; ++k) {
			iserrno = 0;
			if (isaddindex (isfd, &fh->key[k])) {
				ret = COB_STATUS_39_CONFLICT_ATTRIBUTE;
			}
		}
		if (ret > 9) {
			isfullclose (isfd);
			iserase ((void *)filename);
			freefh (fh);
			return ret;
		}
	}
	f->file = fh;
	f->open_mode = mode;
	fh->isfd = isfd;
	fh->filename = cob_strdup (filename);
	fh->savekey = cob_malloc ((size_t)(fh->lenkey + 1));
	fh->recwrk = cob_malloc ((size_t)(f->record_max + 1));
	/* Active index is unknown at this time */
	fh->curkey = -1;
	f->flag_nonexistent = 0;
	f->flag_end_of_file = 0;
	f->flag_begin_of_file = 0;
	return ret;

#elif	defined(WITH_DB)

	struct indexed_file	*p;
	size_t			i;
	size_t			j;
	size_t			maxsize;
	db_lockmode_t		lock_mode;
	int			handle_created;
	cob_u32_t		flags = 0;
	int			ret = 0;
	int			nonexistent;
#if	0	/* RXWRXW - Access check BDB Human */
	int			checkvalue;
#endif

	COB_UNUSED (sharing);
	if (cobsetptr->bdb_home != NULL
	 && bdb_env == NULL) {		/* Join BDB, on first OPEN of INDEXED file */
		join_environment ();
	}
	cob_chk_file_mapping ();

#if	0	/* RXWRXW - Access check BDB Human */
	if (mode == COB_OPEN_INPUT) {
		checkvalue = R_OK;
	} else {
		checkvalue = R_OK | W_OK;
	}
#endif

	nonexistent = 0;
	if (bdb_nofile (filename)) {
		nonexistent = 1;
		if (mode != COB_OPEN_OUTPUT && f->flag_optional == 0) {
			return COB_STATUS_35_NOT_EXISTS;
		}
	}

	p = cob_malloc (sizeof (struct indexed_file));
	if (bdb_env != NULL) {
		if (mode == COB_OPEN_OUTPUT || mode == COB_OPEN_EXTEND ||
		    (f->lock_mode & COB_FILE_EXCLUSIVE) ||
		    (mode == COB_OPEN_I_O && !f->lock_mode)) {
			lock_mode = DB_LOCK_WRITE;
		} else {
			lock_mode = DB_LOCK_READ;
		}
		p->key.size = (cob_dbtsize_t) strlen (filename);
		p->key.data = filename;
		ret = bdb_env->lock_get (bdb_env, bdb_lock_id, DB_LOCK_NOWAIT,
					&p->key, lock_mode, &p->bdb_file_lock);
		if (ret) {
			cob_free (p);
			if (ret == DB_LOCK_NOTGRANTED) {
				return COB_STATUS_61_FILE_SHARING;
			} else {
				return COB_STATUS_30_PERMANENT_ERROR;
			}
		}
	}

	switch (mode) {
	case COB_OPEN_INPUT:
		flags |= DB_RDONLY;
		break;
	case COB_OPEN_OUTPUT:
		flags |= DB_CREATE;
		break;
	case COB_OPEN_I_O:
	case COB_OPEN_EXTEND:
		flags |= DB_CREATE;
		break;
	}

	p->db = cob_malloc (sizeof (DB *) * f->nkeys);
	p->cursor = cob_malloc (sizeof (DBC *) * f->nkeys);
	p->filenamelen = (int) strlen (filename);
	p->last_readkey = cob_malloc (sizeof (unsigned char *) * 2 * f->nkeys);
	p->last_dupno = cob_malloc (sizeof (unsigned int) * f->nkeys);
	p->rewrite_sec_key = cob_malloc (sizeof (int) * f->nkeys);
	maxsize = 0;
	for (i = 0; i < f->nkeys; ++i) {
		if (f->keys[i].field->size > maxsize) {
			maxsize = f->keys[i].field->size;
		}
	}
	for (i = 0; i < f->nkeys; ++i) {
		/* File name */
		runtime_buffer[COB_FILE_MAX] = 0;
		if (i == 0) {
			snprintf (runtime_buffer, (size_t)COB_FILE_MAX, "%s",
				  filename);
		} else {
			snprintf (runtime_buffer, (size_t)COB_FILE_MAX, "%s.%d",
				 filename, (int)i);
		}
#if	0	/* RXWRXW - Access check BDB Human */
		ret = access (runtime_buffer, checkvalue);
		if (ret != 0) {
			if (errno == ENOENT &&
			    (mode == COB_OPEN_OUTPUT || f->flag_optional == 1)) {
				ret = 0;
				/* Check here if the directory exists ? */
#if	0	/* RXWRXW - Check dir */
				if (!directory exists) {
					ret = ENOENT;
				} else {
					ret = 0;
				}
#endif
			} else {
				ret = errno;
			}
			if (ret != 0) {
				switch (ret) {
				case ENOENT:
					ret = COB_STATUS_35_NOT_EXISTS;
					break;
				case EACCES:
					ret = COB_STATUS_37_PERMISSION_DENIED;
					break;
				default:
					ret = COB_STATUS_30_PERMANENT_ERROR;
					break;
				}
				/* FIXME: BDB cleanup is missing here */
				return ret;
			}
		}
#endif

		/* btree info */
		ret = db_create (&p->db[i], bdb_env, 0);
		if (!ret) {
			handle_created = 1;
			if (mode == COB_OPEN_OUTPUT) {
				if (bdb_env) {
					bdb_env->dbremove (bdb_env, NULL, runtime_buffer, NULL, 0);
				} else {
					/* FIXME: test "First READ on empty SEQUENTIAL INDEXED file ..."
					   on OPEN-OUTPUT results with MinGW & BDB 6 in
					   BDB1565 DB->pget: method not permitted before handle's open method
					*/
					p->db[i]->remove (p->db[i], runtime_buffer, NULL, 0);
					ret = db_create (&p->db[i], bdb_env, 0);
				}
			}
			if (!ret) {
				if (f->keys[i].flag) {
					p->db[i]->set_flags (p->db[i], DB_DUP);
				}
			}
		} else {
			handle_created = 0;
		}
		/* Open db */
		if (!ret) {
			/* FIXME: test "First READ on empty SEQUENTIAL INDEXED file ..."
			   on OPEN-OUTPUT results with MinGW & BDB 6 in
			   BDB0588 At least one secondary cursor must be specified to DB->join
			*/
			ret = p->db[i]->open (p->db[i], NULL, runtime_buffer, NULL,
						DB_BTREE, flags, COB_FILE_MODE);
		}
		if (ret) {
			for (j = 0; j < i; ++j) {
				DB_CLOSE (p->db[j]);
			}
			if (handle_created) {
				DB_CLOSE (p->db[i]);
			}
			cob_free (p->db);
			cob_free (p->last_readkey);
			cob_free (p->last_dupno);
			cob_free (p->rewrite_sec_key);
			cob_free (p->cursor);
			if (bdb_env != NULL) {
				bdb_env->lock_put (bdb_env, &p->bdb_file_lock);
			}
			cob_free (p);
			switch (ret) {
			case DB_LOCK_NOTGRANTED:
				return COB_STATUS_61_FILE_SHARING;
			case ENOENT:
				if (mode == COB_OPEN_EXTEND ||
				    mode == COB_OPEN_OUTPUT) {
					return COB_STATUS_30_PERMANENT_ERROR;
				}
				if (f->flag_optional) {
					if (mode == COB_OPEN_I_O) {
						return COB_STATUS_30_PERMANENT_ERROR;
					}
					f->open_mode = mode;
					f->flag_nonexistent = 1;
					f->flag_end_of_file = 1;
					f->flag_begin_of_file = 1;
					/* RXWRXW - Check directory exists? */
					return COB_STATUS_05_SUCCESS_OPTIONAL;
				}
				return COB_STATUS_35_NOT_EXISTS;
			default:
				return COB_STATUS_30_PERMANENT_ERROR;
			}

		}

		p->last_readkey[i] = cob_malloc (maxsize);
		p->last_readkey[f->nkeys + i] = cob_malloc (maxsize);
	}

	p->temp_key = cob_malloc (maxsize + sizeof(unsigned int));
	f->file = p;
	p->key_index = 0;
	p->last_key = NULL;

	memset ((void *)&p->key, 0, sizeof (DBT));
	memset ((void *)&p->data, 0, sizeof (DBT));
	p->filename = cob_malloc (strlen (filename) + 1);
	strcpy (p->filename, filename);
	p->write_cursor_open = 0;
	p->record_locked = 0;
	if (bdb_env != NULL) {
		bdb_env->lock_id (bdb_env, &p->bdb_lock_id);
	}

	DBT_SET (p->key, f->keys[0].field);
	p->db[0]->cursor (p->db[0], NULL, &p->cursor[0], 0);
	ret = DB_SEQ (p->cursor[0], DB_FIRST);
	p->cursor[0]->c_close (p->cursor[0]);
	p->cursor[0] = NULL;
	if (!ret) {
		memcpy (p->last_readkey[0], p->key.data, (size_t)p->key.size);
	} else {
		p->data.data = NULL;
	}

	f->open_mode = mode;
	if (f->flag_optional && nonexistent) {
		return COB_STATUS_05_SUCCESS_OPTIONAL;
	}
	return 0;

#else
	COB_UNUSED (f);
	COB_UNUSED (filename);
	COB_UNUSED (sharing);
	COB_UNUSED (mode);

	return COB_STATUS_91_NOT_AVAILABLE;
#endif
}

/* Close the INDEXED file */

static int
indexed_close (cob_file *f, const int opt)
{
#ifdef	WITH_INDEX_EXTFH

	return extfh_indexed_close (f, opt);

#elif	defined(WITH_ANY_ISAM)

	struct indexfile	*fh;

	COB_UNUSED (opt);

	fh = f->file;
	if (fh == NULL) {
		return COB_STATUS_00_SUCCESS;
	}
	if (fh->isfd >= 0) {
		isfullclose (fh->isfd);
	}
	freefh (fh);
	f->file = NULL;
	return COB_STATUS_00_SUCCESS;

#elif	defined(WITH_DB)

	struct indexed_file	*p;
	int			i;

	COB_UNUSED (opt);

	p = f->file;
	/* Close DB's */
	for (i = 0; i < (int)f->nkeys; ++i) {
		if (p->cursor[i]) {
			p->cursor[i]->c_close (p->cursor[i]);
		}
	}
	for (i = (int)f->nkeys - 1; i >= 0; --i) {
		if (p->db[i]) {
			DB_CLOSE (p->db[i]);
		}
		cob_free (p->last_readkey[i]);
		cob_free (p->last_readkey[f->nkeys + i]);
	}

	if (p->last_key) {
		cob_free (p->last_key);
	}
	cob_free (p->temp_key);
	cob_free (p->db);
	cob_free (p->last_readkey);
	cob_free (p->last_dupno);
	cob_free (p->rewrite_sec_key);
	cob_free (p->filename);
	cob_free (p->cursor);
	if (bdb_env != NULL) {
		unlock_record (f);
		bdb_env->lock_put (bdb_env, &p->bdb_file_lock);
		bdb_env->lock_id_free (bdb_env, p->bdb_lock_id);
	}
	cob_free (p);

	return COB_STATUS_00_SUCCESS;

#else
	COB_UNUSED (f);
	COB_UNUSED (opt);

	return COB_STATUS_91_NOT_AVAILABLE;

#endif
}


/* START INDEXED file with positioning */

static int
indexed_start (cob_file *f, const int cond, cob_field *key)
{
#ifdef	WITH_INDEX_EXTFH

	return extfh_indexed_start (f, cond, key);

#elif	defined(WITH_ANY_ISAM)

	struct indexfile	*fh;
	size_t			k;
	int			mode;
	int			klen;
	int			savecond;

	fh = f->file;
	f->flag_read_done = 0;
	f->flag_first_read = 0;
	fh->readdone = 0;
	fh->eofpending = 0;
	fh->startiscur = 0;
	fh->wrkhasrec = 0;
	if (f->flag_nonexistent) {
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}
	for (k = 0; k < f->nkeys; ++k) {
		if (f->keys[k].field->data == key->data) {
			break;
		}
	}
	/* Use size of data field; This may indicate a partial key */
	klen = key->size;
	if (klen < 1 || klen > fh->key[k].k_leng) {
		/* Max key length for this index */
		klen = fh->key[k].k_leng;
	}
	mode = ISGTEQ;
	fh->startiscur = 1;
	savecond = cond;
	switch (cond) {
	case COB_EQ:
		mode = ISEQUAL;
		fh->readdir = ISNEXT;
		break;
	case COB_GE:
		mode = ISGTEQ;
		fh->readdir = ISNEXT;
		break;
	case COB_GT:
		mode = ISGREAT;
		fh->readdir = ISNEXT;
		break;
	case COB_LE:
		mode = ISGTEQ;
		fh->readdir = ISPREV;
		break;
	case COB_LT:
		mode = ISGTEQ;
		fh->readdir = ISPREV;
		break;
	case COB_FI:
		mode = ISFIRST;
		fh->readdir = ISNEXT;
		break;
	case COB_LA:
		mode = ISLAST;
		fh->readdir = ISPREV;
		break;
	default:
		return COB_STATUS_21_KEY_INVALID;
	}
	if (isstart (fh->isfd, &fh->key[k], klen, (void *)f->record->data, mode)) {
		if (cond == COB_LE || cond == COB_LT) {
			if (isstart (fh->isfd, &fh->key[k], klen, (void *)f->record->data, ISLAST)) {
				fh->curkey = -1;
				fh->startcond = -1;
				fh->readdir = -1;
				fh->startiscur = 0;
				return fisretsts (COB_STATUS_23_KEY_NOT_EXISTS);
			} else {
				savecond = COB_LA;
			}
		} else {
			fh->curkey = -1;
			fh->startcond = -1;
			fh->readdir = -1;
			fh->startiscur = 0;
			return fisretsts (COB_STATUS_23_KEY_NOT_EXISTS);
		}
	}
	fh->startcond = savecond;
	memcpy (fh->savekey, f->record->data + fh->key[k].k_start,
		(size_t)fh->key[k].k_leng);
	fh->curkey = k;
	f->flag_end_of_file = 0;
	f->flag_begin_of_file = 0;
	f->flag_first_read = 1;
	return COB_STATUS_00_SUCCESS;

#elif	defined(WITH_DB)

	return indexed_start_internal (f, cond, key, 0, 0);

#else
	COB_UNUSED (f);
	COB_UNUSED (cond);
	COB_UNUSED (key);

	return COB_STATUS_91_NOT_AVAILABLE;
#endif
}

/* Random READ of the INDEXED file  */

static int
indexed_read (cob_file *f, cob_field *key, const int read_opts)
{
#ifdef	WITH_INDEX_EXTFH

	return extfh_indexed_read (f, key, read_opts);

#elif	defined(WITH_ANY_ISAM)

	struct indexfile	*fh;
	size_t			k;
	int			ret;
	int			lmode;

	fh = f->file;
	fh->eofpending = 0;
	fh->startiscur = 0;
	fh->wrkhasrec = 0;
	if (f->flag_nonexistent) {
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}
	for (k = 0; k < f->nkeys; ++k) {
		if (f->keys[k].field->data == key->data) {
			break;
		}
	}
	if (fh->curkey != (int)k) {
		/* Switch to this index */
		isstart (fh->isfd, &fh->key[k], 0,
			 (void *)f->record->data, ISEQUAL);
		fh->curkey = k;
		fh->wrkhasrec = 0;
	}
	fh->startcond = -1;
	lmode = 0;
	if (read_opts & COB_READ_LOCK) {
		lmode = ISLOCK;
	} else if (read_opts & COB_READ_WAIT_LOCK) {
		lmode = ISLCKW;
	} else if ((f->lock_mode & COB_LOCK_AUTOMATIC)) {
		if (f->open_mode != COB_OPEN_INPUT) {
			if (!(read_opts & COB_READ_IGNORE_LOCK)) {
				lmode = ISLOCK;
			}
		}
	}
#ifdef	ISSKIPLOCK
	if (read_opts & COB_READ_IGNORE_LOCK) {
		lmode = ISSKIPLOCK;
	}
#endif
	if ((fh->lmode & ISLOCK) && !(f->lock_mode & COB_LOCK_MULTIPLE)) {
		isrelease (fh->isfd);
	}
	iserrno = 0;
	fh->readdir = -1;
	ret = COB_STATUS_00_SUCCESS;
	if (isread (fh->isfd, (void *)f->record->data, ISEQUAL | lmode)) {
		ret = fisretsts (COB_STATUS_21_KEY_INVALID);
	}
	if (unlikely (ret != 0)) {
		memset (fh->savekey, 0, (size_t)fh->key[0].k_leng);
		fh->recnum = 0;
		fh->readdone = 0;
		return ret;
	}
	f->flag_first_read = 0;
	f->flag_read_done = 1;
	fh->readdone = 1;
	f->flag_end_of_file = 0;
	f->flag_begin_of_file = 0;
	memcpy (fh->savekey, f->record->data + fh->key[0].k_start,
		(size_t)fh->key[0].k_leng);
	fh->recnum = isrecnum;
#ifdef	ISVARLEN
	if (f->record_min != f->record_max) {
		f->record->size = isreclen;
	}
#endif
	return 0;

#elif	defined(WITH_DB)

	struct indexed_file	*p;
	int			ret;
	int			bdb_opts;
	int			test_lock;

	p = f->file;
	test_lock = 0;
	bdb_opts = read_opts;
	if (bdb_env != NULL) {
		if (f->open_mode != COB_OPEN_I_O ||
		    (f->lock_mode & COB_FILE_EXCLUSIVE)) {
			bdb_opts &= ~COB_READ_LOCK;
		} else if ((f->lock_mode & COB_LOCK_AUTOMATIC) &&
			   !(bdb_opts & COB_READ_NO_LOCK)) {
			bdb_opts |= COB_READ_LOCK;
		}
		unlock_record (f);
		test_lock = 1;
	} else {
		bdb_opts &= ~COB_READ_LOCK;
	}

	ret = indexed_start_internal (f, COB_EQ, key, bdb_opts, test_lock);
	if (ret != COB_STATUS_00_SUCCESS) {
		return ret;
	}

	f->record->size = p->data.size;
	memcpy (f->record->data, p->data.data, (size_t)p->data.size);

	return COB_STATUS_00_SUCCESS;

#else
	COB_UNUSED (f);
	COB_UNUSED (key);
	COB_UNUSED (read_opts);

	return COB_STATUS_91_NOT_AVAILABLE;
#endif
}

/* Sequential READ of the INDEXED file */

static int
indexed_read_next (cob_file *f, const int read_opts)
{
#ifdef	WITH_INDEX_EXTFH

	return extfh_indexed_read_next (f, read_opts);

#elif	defined(WITH_ANY_ISAM)

	struct indexfile	*fh;
	int			ret;
	int			lmode;
	int			domoveback;

	fh = f->file;
	ret = COB_STATUS_00_SUCCESS;
	lmode = 0;

	if (fh->curkey == -1) {
		/* Switch to primary index */
		isstart (fh->isfd, &fh->key[0], 0, NULL, ISFIRST);
		fh->curkey = 0;
		fh->readdir = ISNEXT;
		fh->startcond = -1;
		fh->startiscur = 0;
		fh->wrkhasrec = 0;
	}
	if (read_opts & COB_READ_LOCK) {
		lmode = ISLOCK;
	} else if (read_opts & COB_READ_WAIT_LOCK) {
		lmode = ISLCKW;
	} else if ((f->lock_mode & COB_LOCK_AUTOMATIC) &&
		   f->open_mode != COB_OPEN_INPUT) {
		if (!(read_opts & COB_READ_IGNORE_LOCK)) {
			lmode = ISLOCK;
		}
	}
#ifdef	ISSKIPLOCK
	if (read_opts & COB_READ_IGNORE_LOCK) {
		lmode |= ISSKIPLOCK;
	}
#endif
	if ((fh->lmode & ISLOCK) && !(f->lock_mode & COB_LOCK_MULTIPLE)) {
		isrelease (fh->isfd);
	}
	iserrno = 0;
	switch (read_opts & COB_READ_MASK) {
	case COB_READ_NEXT:
		fh->readdir = ISNEXT;
		if (fh->eofpending == ISNEXT) {
			fh->eofpending = 0;
			fh->wrkhasrec = 0;
			return COB_STATUS_10_END_OF_FILE;
		}
		if (fh->startiscur) {
			if (fh->startcond == COB_LA) {
				if (isread (fh->isfd, (void *)f->record->data, ISLAST | lmode)) {
					ret = fisretsts (COB_STATUS_10_END_OF_FILE);
				}
			} else if (fh->startcond == COB_FI) {
				if (isread (fh->isfd, (void *)f->record->data, ISFIRST | lmode)) {
					ret = fisretsts (COB_STATUS_10_END_OF_FILE);
				}
			} else if (isread (fh->isfd, (void *)f->record->data, ISCURR)) {
				ret = fisretsts (COB_STATUS_10_END_OF_FILE);
			} else {
				switch (fh->startcond) {
				case COB_GE:
					domoveback = 0;
					while (iserrno == 0
					&& memcmp (f->record->data + fh->key[fh->curkey].k_start, fh->savekey, (size_t)fh->key[fh->curkey].k_leng) == 0) {
						isread (fh->isfd, (void *)f->record->data, ISPREV);
						domoveback = 1;
					}
					if (domoveback) {
						isread (fh->isfd, (void *)f->record->data, iserrno == 0 ? ISNEXT : ISFIRST);
					}
					break;
				case COB_LE:
					domoveback = 0;
					while (iserrno == 0
					&& memcmp (f->record->data + fh->key[fh->curkey].k_start, fh->savekey, (size_t)fh->key[fh->curkey].k_leng) == 0) {
						isread (fh->isfd, (void *)f->record->data, ISNEXT);
						domoveback = 1;
					}
					if (domoveback) {
						isread (fh->isfd, (void *)f->record->data, iserrno == 0 ? ISPREV : ISLAST);
					}
					break;
				case COB_LT:
					while (iserrno == 0
					&& memcmp (f->record->data + fh->key[fh->curkey].k_start, fh->savekey, (size_t)fh->key[fh->curkey].k_leng) >= 0) {
						isread (fh->isfd, (void *)f->record->data, ISPREV);
					}
					break;
				case COB_GT:
					while (iserrno == 0
					&& memcmp (f->record->data + fh->key[fh->curkey].k_start, fh->savekey, (size_t)fh->key[fh->curkey].k_leng) <= 0) {
						isread (fh->isfd, (void *)f->record->data, ISNEXT);
					}
					break;
				}
				if (isread (fh->isfd, (void *)f->record->data, ISCURR | lmode)) {
					ret = fisretsts (COB_STATUS_10_END_OF_FILE);
				}
			}
			fh->startcond = -1;
			fh->startiscur = 0;
		} else if (fh->wrkhasrec == ISNEXT) {
			memcpy (f->record->data, fh->recwrk, f->record_max);
			if (fh->lmode & ISLOCK) {
				/* Now lock 'peek ahead' record */
				if (isread (fh->isfd, (void *)f->record->data,
				    ISCURR | fh->lmode)) {
					ret = fisretsts (COB_STATUS_10_END_OF_FILE);
				}
			}
		} else {
			if (fh->wrkhasrec == ISPREV) {
				isread (fh->isfd, (void *)f->record->data, ISNEXT);
				fh->wrkhasrec = 0;
			}
			if (isread (fh->isfd, (void *)f->record->data, ISNEXT | lmode)) {
				ret = fisretsts (COB_STATUS_10_END_OF_FILE);
			}
		}
		break;
	case COB_READ_PREVIOUS:
		fh->readdir = ISPREV;
		if (fh->eofpending == ISPREV) {
			fh->eofpending = 0;
			fh->wrkhasrec = 0;
			return COB_STATUS_10_END_OF_FILE;
		}
		if (fh->startiscur) {
			if (fh->startcond == COB_FI) {
				if (isread (fh->isfd, (void *)f->record->data, ISFIRST | lmode)) {
					ret = fisretsts (COB_STATUS_10_END_OF_FILE);
				}
			} else if (fh->startcond == COB_LA) {
				if (isread (fh->isfd, (void *)f->record->data, ISLAST | lmode)) {
					ret = fisretsts (COB_STATUS_10_END_OF_FILE);
				}
			} else if (isread (fh->isfd, (void *)f->record->data, ISCURR | lmode)) {
				ret = fisretsts (COB_STATUS_10_END_OF_FILE);
			} else {
				switch (fh->startcond) {
				case COB_LE:
					domoveback = 0;
					while (iserrno == 0
					&& memcmp (f->record->data + fh->key[fh->curkey].k_start, fh->savekey, (size_t)fh->key[fh->curkey].k_leng) == 0) {
						isread (fh->isfd, (void *)f->record->data, ISNEXT);
						domoveback = 1;
					}
					if (domoveback) {
						isread (fh->isfd, (void *)f->record->data, ISPREV);
					}
					break;
				case COB_LT:
					while (iserrno == 0
					&& memcmp (f->record->data + fh->key[fh->curkey].k_start, fh->savekey, (size_t)fh->key[fh->curkey].k_leng) >= 0) {
						isread (fh->isfd, (void *)f->record->data, ISPREV);
					}
					break;
				case COB_GT:
					while (iserrno == 0
					&& memcmp (f->record->data + fh->key[fh->curkey].k_start, fh->savekey, (size_t)fh->key[fh->curkey].k_leng) <= 0) {
						isread (fh->isfd, (void *)f->record->data, ISNEXT);
					}
					break;
				case COB_GE:
					while (iserrno == 0
					&& memcmp (f->record->data + fh->key[fh->curkey].k_start, fh->savekey, (size_t)fh->key[fh->curkey].k_leng) < 0) {
						isread (fh->isfd, (void *)f->record->data, ISNEXT);
					}
					break;
				}
				if (isread (fh->isfd, (void *)f->record->data, ISCURR | lmode)) {
					ret = fisretsts (COB_STATUS_10_END_OF_FILE);
				}
			}
			fh->startcond = -1;
			fh->startiscur = 0;
		} else if (fh->wrkhasrec == ISPREV) {
			memcpy (f->record->data, fh->recwrk, f->record_max);
			if (fh->lmode & ISLOCK) {
				/* Now lock 'peek ahead' record */
				if (isread (fh->isfd, (void *)f->record->data,
				    ISCURR | fh->lmode)) {
					ret = fisretsts (COB_STATUS_10_END_OF_FILE);
				}
			}
		} else {
			if (fh->wrkhasrec == ISNEXT) {
				isread (fh->isfd, (void *)f->record->data, ISPREV);
				fh->wrkhasrec = 0;
			}
			if (isread (fh->isfd, (void *)f->record->data, ISPREV | lmode)) {
				ret = fisretsts (COB_STATUS_10_END_OF_FILE);
			}
		}
		break;
	case COB_READ_FIRST:
		fh->readdir = ISNEXT;
		if (isread (fh->isfd, (void *)f->record->data, ISFIRST | lmode)) {
			ret = fisretsts (COB_STATUS_10_END_OF_FILE);
		}
		break;
	case COB_READ_LAST:
		fh->readdir = ISPREV;
		if (isread (fh->isfd, (void *)f->record->data, ISLAST | lmode)) {
			ret = fisretsts (COB_STATUS_10_END_OF_FILE);
		}
		break;
	default:
		fh->readdir = ISNEXT;
		if (isread (fh->isfd, (void *)f->record->data, ISNEXT | lmode)) {
			ret = fisretsts (COB_STATUS_10_END_OF_FILE);
		}
		break;
	}
	if (unlikely (ret != 0)) {
		memset (fh->savekey, 0, (size_t)fh->key[0].k_leng);
		fh->recnum = 0;
		fh->readdone = 0;
		fh->wrkhasrec = 0;
		return ret;
	}
	fh->eofpending = 0;
	f->flag_first_read = 0;
	f->flag_read_done = 1;
	fh->readdone = 1;
	f->flag_end_of_file = 0;
	f->flag_begin_of_file = 0;
	memcpy (fh->savekey, f->record->data + fh->key[0].k_start,
		(size_t)fh->key[0].k_leng);
	fh->recnum = isrecnum;
#ifdef	ISVARLEN
	if (f->record_min != f->record_max) {
		f->record->size = isreclen;
	}
#endif

#ifdef	COB_WITH_STATUS_02
	if((isstat1 == '0') && (isstat2 == '2')) {
		return COB_STATUS_02_SUCCESS_DUPLICATE;
	}
#endif
	return 0;

#elif	defined(WITH_DB)

	struct indexed_file	*p;
	int			ret;
	int			read_nextprev;
	cob_u32_t		nextprev;
	int			file_changed;
	int			bdb_opts;
	unsigned int		dupno;

	p = f->file;
	nextprev = DB_NEXT;
	dupno = 0;
	file_changed = 0;

	dupno = 0;

	bdb_opts = read_opts;
	if (bdb_env != NULL) {
		if (f->open_mode != COB_OPEN_I_O ||
		    (f->lock_mode & COB_FILE_EXCLUSIVE)) {
			bdb_opts &= ~COB_READ_LOCK;
		} else if ((f->lock_mode & COB_LOCK_AUTOMATIC) &&
			   !(bdb_opts & COB_READ_NO_LOCK)) {
			bdb_opts |= COB_READ_LOCK;
		}
		unlock_record (f);
	} else {
		bdb_opts &= ~COB_READ_LOCK;
	}

	if (unlikely (bdb_opts & COB_READ_PREVIOUS)) {
		if (f->flag_end_of_file) {
			nextprev = DB_LAST;
		} else {
			nextprev = DB_PREV;
		}
	} else if (f->flag_begin_of_file) {
		nextprev = DB_FIRST;
	}
	/* The open cursor makes this function atomic */
	if (p->key_index != 0) {
		p->db[0]->cursor (p->db[0], NULL, &p->cursor[0], 0);
	}
	p->db[p->key_index]->cursor (p->db[p->key_index], NULL, &p->cursor[p->key_index], 0);

	if (f->flag_first_read) {
		/* Data is read in indexed_open or indexed_start */
		if (p->data.data == NULL || (f->flag_first_read == 2 &&
		    nextprev == DB_PREV)) {
			p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
			p->cursor[p->key_index] = NULL;
			if (p->key_index != 0) {
				p->cursor[0]->c_close (p->cursor[0]);
				p->cursor[0] = NULL;
			}
			return COB_STATUS_10_END_OF_FILE;
		}
		/* Check if previously read data still exists */
		p->key.size = (cob_dbtsize_t) f->keys[p->key_index].field->size;
		p->key.data = p->last_readkey[p->key_index];
		ret = DB_SEQ (p->cursor[p->key_index], DB_SET);
		if (!ret && p->key_index > 0) {
			if (f->keys[p->key_index].flag) {
				memcpy (&dupno, (cob_u8_ptr)p->data.data + f->keys[0].field->size, sizeof(unsigned int));
				while (ret == 0 &&
				      memcmp (p->key.data, p->last_readkey[p->key_index], (size_t)p->key.size) == 0 &&
				      dupno < p->last_dupno[p->key_index]) {
					ret = DB_SEQ (p->cursor[p->key_index], DB_NEXT);
					memcpy (&dupno, (cob_u8_ptr)p->data.data + f->keys[0].field->size, sizeof(unsigned int));
				}
				if (ret == 0 &&
				   memcmp (p->key.data, p->last_readkey[p->key_index], (size_t)p->key.size) == 0 &&
				   dupno == p->last_dupno[p->key_index]) {
					ret = memcmp (p->last_readkey[p->key_index + f->nkeys], p->data.data, f->keys[0].field->size);
				} else {
					ret = 1;
				}
			} else {
				ret = memcmp (p->last_readkey[p->key_index + f->nkeys], p->data.data, f->keys[0].field->size);
			}
			if (!ret) {
				p->key.size = (cob_dbtsize_t) f->keys[0].field->size;
				p->key.data = p->last_readkey[p->key_index + f->nkeys];
				ret = DB_GET (p->db[0], 0);
			}
		}
		file_changed = ret;
		if (bdb_env != NULL && !file_changed) {
			if (!(bdb_opts & COB_READ_IGNORE_LOCK)) {
				ret = test_record_lock (f, p->key.data, p->key.size);
				if (ret) {
					p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
					p->cursor[p->key_index] = NULL;
					if (p->key_index != 0) {
						p->cursor[0]->c_close (p->cursor[0]);
						p->cursor[0] = NULL;
					}
					return COB_STATUS_51_RECORD_LOCKED;
				}
			}
			if (bdb_opts & COB_READ_LOCK) {
				ret = lock_record (f, p->key.data, p->key.size);
				if (ret) {
					p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
					p->cursor[p->key_index] = NULL;
					if (p->key_index != 0) {
						p->cursor[0]->c_close (p->cursor[0]);
						p->cursor[0] = NULL;
					}
					return COB_STATUS_51_RECORD_LOCKED;
				}
			}
		}
	}
	if (!f->flag_first_read || file_changed) {
		if (nextprev == DB_FIRST || nextprev == DB_LAST) {
			read_nextprev = 1;
		} else {
			p->key.size = (cob_dbtsize_t) f->keys[p->key_index].field->size;
			p->key.data = p->last_readkey[p->key_index];
			ret = DB_SEQ (p->cursor[p->key_index], DB_SET_RANGE);
			/* ret != 0 possible, records may be deleted since last read */
			if (ret != 0) {
				if (nextprev == DB_PREV) {
					nextprev = DB_LAST;
					read_nextprev = 1;
				} else {
					p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
					p->cursor[p->key_index] = NULL;
					if (p->key_index != 0) {
						p->cursor[0]->c_close (p->cursor[0]);
						p->cursor[0] = NULL;
					}
					return COB_STATUS_10_END_OF_FILE;
				}
			} else {
				if (memcmp (p->key.data, p->last_readkey[p->key_index], (size_t)p->key.size) == 0) {
					if (p->key_index > 0 && f->keys[p->key_index].flag) {
						memcpy (&dupno, (cob_u8_ptr)p->data.data + f->keys[0].field->size, sizeof(unsigned int));
						while (ret == 0 &&
						memcmp (p->key.data, p->last_readkey[p->key_index], (size_t)p->key.size) == 0 &&
						dupno < p->last_dupno[p->key_index]) {
							ret = DB_SEQ (p->cursor[p->key_index], DB_NEXT);
							memcpy (&dupno, (cob_u8_ptr)p->data.data + f->keys[0].field->size, sizeof(unsigned int));
						}
						if (ret != 0) {
							if (nextprev == DB_PREV) {
								nextprev = DB_LAST;
								read_nextprev = 1;
							} else {
								p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
								p->cursor[p->key_index] = NULL;
								if (p->key_index != 0) {
									p->cursor[0]->c_close (p->cursor[0]);
									p->cursor[0] = NULL;
								}
								return COB_STATUS_10_END_OF_FILE;
							}
						} else {
							if (memcmp (p->key.data, p->last_readkey[p->key_index], (size_t)p->key.size) == 0 &&
								dupno == p->last_dupno[p->key_index]) {
								read_nextprev = 1;
							} else {
								if (nextprev == DB_PREV) {
									read_nextprev = 1;
								} else {
									read_nextprev = 0;
								}
							}
						}
					} else {
						read_nextprev = 1;
					}
				} else {
					if (nextprev == DB_PREV) {
						read_nextprev = 1;
					} else {
						read_nextprev = 0;
					}
				}
			}
		}
		if (read_nextprev) {
			ret = DB_SEQ (p->cursor[p->key_index], nextprev);
			if (ret != 0) {
				p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
				p->cursor[p->key_index] = NULL;
				if (p->key_index != 0) {
					p->cursor[0]->c_close (p->cursor[0]);
					p->cursor[0] = NULL;
				}
				return COB_STATUS_10_END_OF_FILE;
			}
		}

		if (p->key_index > 0) {
			/* Temporarily save alternate key */
			memcpy (p->temp_key, p->key.data, (size_t)p->key.size);
			if (f->keys[p->key_index].flag) {
				memcpy (&dupno, (cob_u8_ptr)p->data.data + f->keys[0].field->size, sizeof(unsigned int));
			}
			p->key.data = p->data.data;
			p->key.size = f->keys[0].field->size;
			if (DB_GET (p->db[0], 0) != 0) {
				p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
				p->cursor[p->key_index] = NULL;
				p->cursor[0]->c_close (p->cursor[0]);
				p->cursor[0] = NULL;
				return COB_STATUS_23_KEY_NOT_EXISTS;
			}
		}
		if (bdb_env != NULL) {
			if (!(bdb_opts & COB_READ_IGNORE_LOCK)) {
				ret = test_record_lock (f, p->key.data, p->key.size);
				if (ret) {
					p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
					p->cursor[p->key_index] = NULL;
					if (p->key_index != 0) {
						p->cursor[0]->c_close (p->cursor[0]);
						p->cursor[0] = NULL;
					}
					return COB_STATUS_51_RECORD_LOCKED;
				}
			}
			if (bdb_opts & COB_READ_LOCK) {
				ret = lock_record (f, p->key.data, p->key.size);
				if (ret) {
					p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
					p->cursor[p->key_index] = NULL;
					if (p->key_index != 0) {
						p->cursor[0]->c_close (p->cursor[0]);
						p->cursor[0] = NULL;
					}
					return COB_STATUS_51_RECORD_LOCKED;
				}
			}
		}
		if (p->key_index == 0) {
			memcpy (p->last_readkey[0], p->key.data, (size_t)p->key.size);
		} else {
			memcpy (p->last_readkey[p->key_index], p->temp_key,
				    f->keys[p->key_index].field->size);
			memcpy (p->last_readkey[p->key_index + f->nkeys], p->key.data, f->keys[0].field->size);
			if (f->keys[p->key_index].flag) {
				p->last_dupno[p->key_index] = dupno;
			}
		}
	}

	p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
	p->cursor[p->key_index] = NULL;
	if (p->key_index != 0) {
		p->cursor[0]->c_close (p->cursor[0]);
		p->cursor[0] = NULL;
	}

	f->record->size = p->data.size;
	memcpy (f->record->data, p->data.data, (size_t)p->data.size);

	return COB_STATUS_00_SUCCESS;

#else
	COB_UNUSED (f);
	COB_UNUSED (read_opts);

	return COB_STATUS_91_NOT_AVAILABLE;
#endif
}


/* WRITE to the INDEXED file  */

static int
indexed_write (cob_file *f, const int opt)
{
#ifdef	WITH_INDEX_EXTFH

	return extfh_indexed_write (f, opt);

#elif	defined(WITH_ANY_ISAM)

	struct indexfile	*fh;

	COB_UNUSED (opt);

	fh = f->file;
	if (f->flag_nonexistent) {
		return COB_STATUS_48_OUTPUT_DENIED;
	}
	if (f->access_mode == COB_ACCESS_SEQUENTIAL &&
	    memcmp (fh->savekey, f->record->data + fh->key[0].k_start,
		    (size_t)fh->key[0].k_leng) > 0) {
		return COB_STATUS_21_KEY_INVALID;
	}

#ifdef	ISVARLEN
	if (f->record_min != f->record_max) {
		isreclen = f->record->size;
	}
#endif
	if (unlikely (iswrite (fh->isfd, (void *)f->record->data))) {
		return fisretsts (COB_STATUS_49_I_O_DENIED);
	}
	memcpy (fh->savekey, f->record->data + fh->key[0].k_start,
		(size_t)fh->key[0].k_leng);

#ifdef	COB_WITH_STATUS_02
	if((isstat1 == '0') && (isstat2 == '2')) {
		return COB_STATUS_02_SUCCESS_DUPLICATE;
	}
#endif
	return 0;

#elif	defined(WITH_DB)

	struct indexed_file	*p;

	if (f->flag_nonexistent) {
		return COB_STATUS_48_OUTPUT_DENIED;
	}
	p = f->file;
	if (bdb_env != NULL) {
		unlock_record (f);
	}

	/* Check record key */
	DBT_SET (p->key, f->keys[0].field);
	if (!p->last_key) {
		p->last_key = cob_malloc ((size_t)p->key.size);
	} else if (f->access_mode == COB_ACCESS_SEQUENTIAL &&
		   memcmp (p->last_key, p->key.data, (size_t)p->key.size) > 0) {
		return COB_STATUS_21_KEY_INVALID;
	}
	memcpy (p->last_key, p->key.data, (size_t)p->key.size);

	return indexed_write_internal (f, 0, opt);

#else
	COB_UNUSED (f);
	COB_UNUSED (opt);

	return COB_STATUS_91_NOT_AVAILABLE;
#endif
}


/* DELETE record from the INDEXED file  */

static int
indexed_delete (cob_file *f)
{
#ifdef	WITH_INDEX_EXTFH

	return extfh_indexed_delete (f);

#elif	defined(WITH_ANY_ISAM)

	struct indexfile	*fh;
	int			ret;

	fh = f->file;
	ret = COB_STATUS_00_SUCCESS;
	if (f->flag_nonexistent) {
		return COB_STATUS_49_I_O_DENIED;
	}
	if (fh->curkey == -1) {
		/* Switch to primary index */
		isstart (fh->isfd, &fh->key[0], 0,
			 (void *)f->record->data, ISEQUAL);
		fh->curkey = 0;
		fh->readdir = ISNEXT;
	} else {
		savefileposition (f);
		if (fh->curkey != 0) {
			/* Switch to primary index */
			isstart (fh->isfd, &fh->key[0], 0,
				 (void *)f->record->data, ISEQUAL);
		}
	}
	if (isread (fh->isfd, (void *)f->record->data, ISEQUAL | ISLOCK)) {
		ret = fisretsts (COB_STATUS_21_KEY_INVALID);
	} else if (isdelete (fh->isfd, (void *)f->record->data)) {
		ret = fisretsts (COB_STATUS_49_I_O_DENIED);
	}
	restorefileposition (f);
	return ret;

#elif	defined(WITH_DB)

	if (f->flag_nonexistent) {
		return COB_STATUS_49_I_O_DENIED;
	}
	return indexed_delete_internal (f, 0);

#else
	COB_UNUSED (f);

	return COB_STATUS_91_NOT_AVAILABLE;
#endif
}

/* REWRITE record to the INDEXED file  */

static int
indexed_rewrite (cob_file *f, const int opt)
{
#ifdef	WITH_INDEX_EXTFH

	return extfh_indexed_rewrite (f, opt);

#elif	defined(WITH_ANY_ISAM)

	struct indexfile	*fh;
	size_t			k;
	int			ret;

	COB_UNUSED (opt);

	fh = f->file;
	ret = COB_STATUS_00_SUCCESS;
	if (f->flag_nonexistent) {
		return COB_STATUS_49_I_O_DENIED;
	}

	if (f->access_mode == COB_ACCESS_SEQUENTIAL &&
	    memcmp (fh->savekey, f->record->data + fh->key[0].k_start,
		    (size_t)fh->key[0].k_leng) != 0) {
		return COB_STATUS_21_KEY_INVALID;
	}
	if (fh->curkey >= 0) {
		/* Index is active */
		/* Save record data */
		memcpy (fh->recwrk, f->record->data, f->record_max);
/* RXWRXW - readdir */
		fh->readdir = ISNEXT;
		savefileposition (f);
		memcpy (fh->recwrk, f->record->data, f->record_max);
		if (fh->curkey != 0) {
			/* Activate primary index */
			isstart (fh->isfd, &fh->key[0], 0, (void *)fh->recwrk,
				 ISEQUAL);
		}
		/* Verify record exists */
		if (isread (fh->isfd, (void *)fh->recwrk, ISEQUAL)) {
			restorefileposition (f);
			return COB_STATUS_21_KEY_INVALID;
		}
		for (k = 1; k < f->nkeys && ret == COB_STATUS_00_SUCCESS; ++k) {
			if (fh->key[k].k_flags & ISDUPS) {
				continue;
			}
			memcpy (fh->recwrk, f->record->data, f->record_max);
			isstart (fh->isfd, &fh->key[k], fh->key[k].k_leng,
				 (void *)fh->recwrk, ISEQUAL);
			if (!isread (fh->isfd, (void *)fh->recwrk, ISEQUAL) &&
			    isrecnum != fh->recnum) {
				ret = COB_STATUS_22_KEY_EXISTS;
				break;
			}
		}
		if (ret == COB_STATUS_00_SUCCESS) {
			memcpy (fh->recwrk, f->record->data, f->record_max);
			isstart (fh->isfd, &fh->key[0], 0, (void *)fh->recwrk,
				 ISEQUAL);
			if (isread (fh->isfd, (void *)fh->recwrk, ISEQUAL | ISLOCK)) {
				ret = fisretsts (COB_STATUS_49_I_O_DENIED);
			} else {
#ifdef	ISVARLEN
				if (f->record_min != f->record_max) {
					isreclen = f->record->size;
				}
#endif
				if (isrewcurr (fh->isfd, (void *)f->record->data)) {
					ret = fisretsts (COB_STATUS_49_I_O_DENIED);
				}
			}
		}
		restorefileposition (f);

#ifdef	COB_WITH_STATUS_02
		if(!ret && (isstat1 == '0') && (isstat2 == '2')) {
			return COB_STATUS_02_SUCCESS_DUPLICATE;
		}
#endif

		return ret;
	}

	memcpy (fh->recwrk, f->record->data, f->record_max);
	if (isread (fh->isfd, (void *)fh->recwrk, ISEQUAL | ISLOCK)) {
		ret = fisretsts (COB_STATUS_49_I_O_DENIED);
	} else {
#ifdef	ISVARLEN
		if (f->record_min != f->record_max) {
			isreclen = f->record->size;
		}
#endif
		if (isrewrite (fh->isfd, (void *)f->record->data)) {
			ret = fisretsts (COB_STATUS_49_I_O_DENIED);
		}
	}
	if (!ret) {
		if ((f->lock_mode & COB_LOCK_AUTOMATIC) &&
		    !(f->lock_mode & COB_LOCK_MULTIPLE)) {
			isrelease (fh->isfd);
		}
#ifdef	COB_WITH_STATUS_02
		if((isstat1 == '0') && (isstat2 == '2')) {
			return COB_STATUS_02_SUCCESS_DUPLICATE;
		}
#endif
	}
	return ret;

#elif	defined(WITH_DB)

	struct indexed_file	*p;
	int			ret;
	cob_u32_t		flags;

	if (f->flag_nonexistent) {
		return COB_STATUS_49_I_O_DENIED;
	}
	p = f->file;
	if (bdb_env) {
		flags = DB_WRITECURSOR;
	} else {
		flags = 0;
	}
	p->db[0]->cursor (p->db[0], NULL, &p->cursor[0], flags);
	p->write_cursor_open = 1;
	if (bdb_env != NULL) {
		unlock_record (f);
	}

	/* Check duplicate alternate keys */
	if (check_alt_keys (f, 1)) {
		p->cursor[0]->c_close (p->cursor[0]);
		p->cursor[0] = NULL;
		p->write_cursor_open = 0;
		return COB_STATUS_22_KEY_EXISTS;
	}

	/* Delete the current record */
	ret = indexed_delete_internal (f, 1);

	if (ret != COB_STATUS_00_SUCCESS) {
		p->cursor[0]->c_close (p->cursor[0]);
		p->cursor[0] = NULL;
		p->write_cursor_open = 0;
		return ret;
	}

	/* Write data */
	DBT_SET (p->key, f->keys[0].field);
	ret = indexed_write_internal (f, 1, opt);

	p->cursor[0]->c_close (p->cursor[0]);
	p->cursor[0] = NULL;
	p->write_cursor_open = 0;
	return ret;

#else
	COB_UNUSED (f);
	COB_UNUSED (opt);

	return COB_STATUS_91_NOT_AVAILABLE;
#endif
}


static void
cob_file_unlock (cob_file *f)
{
#ifdef	WITH_DB
	struct indexed_file	*p;
#elif	defined(WITH_ANY_ISAM)
	struct indexfile	*fh;
#endif

#ifndef	WITH_SEQRA_EXTFH
#ifdef	HAVE_FCNTL
	struct flock		lock;
#endif
#endif

	if (COB_FILE_SPECIAL(f)) {
		return;
	}

	if (f->open_mode != COB_OPEN_CLOSED &&
	    f->open_mode != COB_OPEN_LOCKED) {
		if (f->organization == COB_ORG_SORT) {
			return;
		}
		if (f->organization != COB_ORG_INDEXED) {
#ifndef	WITH_SEQRA_EXTFH
			if (f->fd >= 0) {
				fdcobsync (f->fd);
			}
#ifdef	HAVE_FCNTL
			if (!(f->lock_mode & COB_FILE_EXCLUSIVE)) {
				/* Unlock the file */
				memset ((void *)&lock, 0, sizeof (struct flock));
				lock.l_type = F_UNLCK;
				lock.l_whence = SEEK_SET;
				lock.l_start = 0;
				lock.l_len = 0;
				if (f->fd >= 0) {
					fcntl (f->fd, F_SETLK, &lock);
				}
			}
#endif

#endif
		} else {
#ifdef	WITH_INDEX_EXTFH
			extfh_indexed_unlock (f);
#elif	defined(WITH_DB)
			p = f->file;
			if (bdb_env != NULL && p) {
				unlock_record (f);
				bdb_env->lock_put (bdb_env, &p->bdb_file_lock);
			}
#elif	defined(WITH_ANY_ISAM)
			fh = f->file;
			if (fh) {
				isrelease (fh->isfd);
			}
#endif
		}
	}
}

/* Global functions */

/*
 * Allocate memory for 'IS EXTERNAL' cob_file
 */
void
cob_file_external_addr (const char *exname,
		cob_file **pfl, cob_file_key **pky,
		const int nkeys, const int linage)
{
	cob_file	*fl;
	fl = cob_external_addr (exname, sizeof(cob_file));
	if (fl->file_version == 0)
		fl->file_version = COB_FILE_VERSION;

	if (nkeys > 0
	 && fl->keys == NULL) {
		fl->keys = cob_cache_malloc (sizeof(cob_file_key) * nkeys);
	}

	if (pky != NULL) {
		*pky = fl->keys;
	}
	
	if (linage > 0
	 && fl->linorkeyptr == NULL) {
		fl->linorkeyptr = cob_cache_malloc (sizeof(cob_linage));
	}
	*pfl = fl;
}

/*
 * Allocate memory for cob_file
 */
void
cob_file_malloc (cob_file **pfl, cob_file_key **pky,
		 const int nkeys, const int linage)
{
	cob_file	*fl;
	fl = cob_cache_malloc (sizeof(cob_file));
	fl->file_version = COB_FILE_VERSION;

	if (nkeys > 0
	 && pky != NULL) {
		*pky = fl->keys = cob_cache_malloc (sizeof(cob_file_key) * nkeys);
	}

	if (linage > 0) {
		fl->linorkeyptr = cob_cache_malloc (sizeof(cob_linage));
	}
	*pfl = fl;
}

/*
 * Free memory for cob_file
 */
void
cob_file_free (cob_file **pfl, cob_file_key **pky)
{
	cob_file	*fl;
	if (pky != NULL) {
		if (*pky != NULL) {
			cob_cache_free (*pky);
			*pky = NULL;
		}
	}
	if (pfl != NULL) {
		fl = *pfl;
		if (fl->linorkeyptr) {
			cob_cache_free (fl->linorkeyptr);
			fl->linorkeyptr = NULL;
		}
		if (*pfl != NULL) {
			cob_cache_free (*pfl);
			*pfl = NULL;
		}
	}
}


void
cob_unlock_file (cob_file *f, cob_field *fnstatus)
{
	cob_file_unlock (f);
	save_status (f, fnstatus, COB_STATUS_00_SUCCESS);
}

void
cob_open (cob_file *f, const int mode, const int sharing, cob_field *fnstatus)
{
	f->flag_read_done = 0;

	/* File was previously closed with lock */
	if (f->open_mode == COB_OPEN_LOCKED) {
		save_status (f, fnstatus, COB_STATUS_38_CLOSED_WITH_LOCK);
		return;
	}

	/* File is already open */
	if (f->open_mode != COB_OPEN_CLOSED) {
		save_status (f, fnstatus, COB_STATUS_41_ALREADY_OPEN);
		return;
	}

	f->last_open_mode = mode;
	f->flag_nonexistent = 0;
	f->flag_end_of_file = 0;
	f->flag_begin_of_file = 0;
	f->flag_first_read = 2;
	f->flag_operation = 0;
	f->lock_mode &= ~COB_LOCK_OPEN_EXCLUSIVE;
	f->lock_mode |= sharing;

	if (unlikely (COB_FILE_STDIN (f))) {
		if (mode != COB_OPEN_INPUT) {
			save_status (f, fnstatus, COB_STATUS_30_PERMANENT_ERROR);
			return;
		}
		f->file = stdin;
		f->fd = fileno (stdin);
		f->open_mode = mode;
		save_status (f, fnstatus, COB_STATUS_00_SUCCESS);
		return;
	}
	if (unlikely (COB_FILE_STDOUT (f))) {
		if (mode != COB_OPEN_OUTPUT) {
			save_status (f, fnstatus, COB_STATUS_30_PERMANENT_ERROR);
			return;
		}
		f->file = stdout;
		f->fd = fileno (stdout);
		f->open_mode = mode;
		save_status (f, fnstatus, COB_STATUS_00_SUCCESS);
		return;
	}

	/* Obtain the file name */
	cob_field_to_string (f->assign, file_open_name, (size_t)COB_FILE_MAX);

	cob_cache_file (f);

	/* Open the file */
	save_status (f, fnstatus,
		     fileio_funcs[(int)f->organization]->open (f, file_open_name,
								mode, sharing));
}

void
cob_close (cob_file *f, cob_field *fnstatus, const int opt, const int remfil)
{
	struct file_list	*l;
	struct file_list	*m;
	int			ret;

	f->flag_read_done = 0;
	f->flag_operation = 0;

	f->lock_mode &= ~COB_LOCK_OPEN_EXCLUSIVE;

	if (COB_FILE_SPECIAL (f)) {
		f->open_mode = COB_OPEN_CLOSED;
		f->file = NULL;
		f->fd = -1;
		save_status (f, fnstatus, COB_STATUS_00_SUCCESS);
		return;
	}

	if (unlikely (remfil)) {
		/* Remove from cache - Needed for CANCEL */
		/* Setting m silences false compiler warning */
		m = file_cache;
		for (l = file_cache; l; l = l->next) {
			if (f == l->file) {
				if (l == file_cache) {
					file_cache = l->next;
				} else {
					m->next = l->next;
				}
				cob_free (l);
				break;
			}
			m = l;
		}
	}

	if (f->open_mode == COB_OPEN_CLOSED) {
		save_status (f, fnstatus, COB_STATUS_42_NOT_OPEN);
		return;
	}

	if (f->flag_nonexistent) {
		ret = COB_STATUS_00_SUCCESS;
	} else {
		ret = fileio_funcs[(int)f->organization]->close (f, opt);
	}

	if (ret == COB_STATUS_00_SUCCESS) {
		switch (opt) {
		case COB_CLOSE_LOCK:
			f->open_mode = COB_OPEN_LOCKED;
			break;
		default:
			f->open_mode = COB_OPEN_CLOSED;
			break;
		}
	}

	save_status (f, fnstatus, ret);
}

#if	0	/* RXWRXW - unlock */
void
cob_unlock (cob_file *f)
{
	int	ret;

	f->flag_read_done = 0;

	if (f->open_mode == COB_OPEN_CLOSED) {
		save_status (f, fnstatus, COB_STATUS_42_NOT_OPEN);
		return;
	}

	if (f->flag_nonexistent) {
		ret = COB_STATUS_00_SUCCESS;
	} else {
		ret = fileio_funcs[(int)f->organization]->close (f, opt);
	}

	save_status (f, fnstatus, ret);
}
#endif

void
cob_start (cob_file *f, const int cond, cob_field *key,
	   cob_field *keysize, cob_field *fnstatus)
{
	int		ret;
	int		size;
	cob_field	tempkey;

	f->flag_read_done = 0;
	f->flag_first_read = 0;

	if (unlikely (f->open_mode != COB_OPEN_I_O &&
		     f->open_mode != COB_OPEN_INPUT)) {
		save_status (f, fnstatus, COB_STATUS_47_INPUT_DENIED);
		return;
	}

	if (unlikely (f->access_mode == COB_ACCESS_RANDOM)) {
		save_status (f, fnstatus, COB_STATUS_47_INPUT_DENIED);
		return;
	}

	if (f->flag_nonexistent) {
		save_status (f, fnstatus, COB_STATUS_23_KEY_NOT_EXISTS);
		return;
	}

	size = 0;
	if (unlikely (keysize)) {
		size = cob_get_int (keysize);
		if (size < 1 || size > (int)key->size) {
			save_status (f, fnstatus, COB_STATUS_23_KEY_NOT_EXISTS);
			return;
		}
		tempkey = *key;
		tempkey.size = (size_t)size;
		ret = fileio_funcs[(int)f->organization]->start (f, cond, &tempkey);
	} else {
		ret = fileio_funcs[(int)f->organization]->start (f, cond, key);
	}
	if (ret == COB_STATUS_00_SUCCESS) {
		f->flag_end_of_file = 0;
		f->flag_begin_of_file = 0;
		f->flag_first_read = 1;
	}

	save_status (f, fnstatus, ret);
}

void
cob_read (cob_file *f, cob_field *key, cob_field *fnstatus, const int read_opts)
{
	int	ret;

	f->flag_read_done = 0;

	if (unlikely (f->open_mode != COB_OPEN_INPUT &&
		     f->open_mode != COB_OPEN_I_O)) {
		save_status (f, fnstatus, COB_STATUS_47_INPUT_DENIED);
		return;
	}

	if (unlikely (f->flag_nonexistent)) {
		if (f->flag_first_read == 0) {
			save_status (f, fnstatus, COB_STATUS_23_KEY_NOT_EXISTS);
			return;
		}
		f->flag_first_read = 0;
		save_status (f, fnstatus, COB_STATUS_10_END_OF_FILE);
		return;
	}

	/* Sequential read at the end of file is an error */
	if (key == NULL) {
		if (unlikely (f->flag_end_of_file &&
			     !(read_opts & COB_READ_PREVIOUS))) {
			save_status (f, fnstatus, COB_STATUS_46_READ_ERROR);
			return;
		}
		if (unlikely (f->flag_begin_of_file &&
			     (read_opts & COB_READ_PREVIOUS))) {
			save_status (f, fnstatus, COB_STATUS_46_READ_ERROR);
			return;
		}
		ret = fileio_funcs[(int)f->organization]->read_next (f, read_opts);
	} else {
		ret = fileio_funcs[(int)f->organization]->read (f, key, read_opts);
	}

	switch (ret) {
	case COB_STATUS_00_SUCCESS:
	case COB_STATUS_02_SUCCESS_DUPLICATE:
		f->flag_first_read = 0;
		f->flag_read_done = 1;
		f->flag_end_of_file = 0;
		f->flag_begin_of_file = 0;
		if (f->variable_record) {
			cob_set_int (f->variable_record, (int) f->record->size);
		}
		break;
	case COB_STATUS_10_END_OF_FILE:
		if (read_opts & COB_READ_PREVIOUS) {
			f->flag_begin_of_file = 1;
		} else {
			f->flag_end_of_file = 1;
		}
		break;
	}

	save_status (f, fnstatus, ret);
}

void
cob_read_next (cob_file *f, cob_field *fnstatus, const int read_opts)
{
	int	ret;

	f->flag_read_done = 0;

	if (unlikely (f->open_mode != COB_OPEN_INPUT &&
		     f->open_mode != COB_OPEN_I_O)) {
		save_status (f, fnstatus, COB_STATUS_47_INPUT_DENIED);
		return;
	}

	if (unlikely (f->flag_nonexistent)) {
		if (f->flag_first_read == 0) {
			save_status (f, fnstatus, COB_STATUS_46_READ_ERROR);
			return;
		}
		f->flag_first_read = 0;
		save_status (f, fnstatus, COB_STATUS_10_END_OF_FILE);
		return;
	}

	/* Sequential read at the end of file is an error */
	if (unlikely (f->flag_end_of_file && !(read_opts & COB_READ_PREVIOUS))) {
		save_status (f, fnstatus, COB_STATUS_46_READ_ERROR);
		return;
	}
	if (unlikely (f->flag_begin_of_file && (read_opts & COB_READ_PREVIOUS))) {
		save_status (f, fnstatus, COB_STATUS_46_READ_ERROR);
		return;
	}

	ret = fileio_funcs[(int)f->organization]->read_next (f, read_opts);

	switch (ret) {
	case COB_STATUS_00_SUCCESS:
	case COB_STATUS_02_SUCCESS_DUPLICATE:
		f->flag_first_read = 0;
		f->flag_read_done = 1;
		f->flag_end_of_file = 0;
		f->flag_begin_of_file = 0;
		if (f->variable_record) {
			cob_set_int (f->variable_record, (int) f->record->size);
		}
		break;
	case COB_STATUS_10_END_OF_FILE:
		if (read_opts & COB_READ_PREVIOUS) {
			f->flag_begin_of_file = 1;
		} else {
			f->flag_end_of_file = 1;
		}
		break;
	}

	save_status (f, fnstatus, ret);
}

void
cob_write (cob_file *f, cob_field *rec, const int opt, cob_field *fnstatus,
	   const unsigned int check_eop)
{
	f->flag_read_done = 0;

	if (f->access_mode == COB_ACCESS_SEQUENTIAL) {
		if (unlikely (f->open_mode != COB_OPEN_OUTPUT &&
			     f->open_mode != COB_OPEN_EXTEND)) {
			save_status (f, fnstatus, COB_STATUS_48_OUTPUT_DENIED);
			return;
		}
	} else {
		if (unlikely (f->open_mode != COB_OPEN_OUTPUT &&
			     f->open_mode != COB_OPEN_I_O)) {
			save_status (f, fnstatus, COB_STATUS_48_OUTPUT_DENIED);
			return;
		}
	}

	if (f->variable_record) {
		f->record->size = (size_t)cob_get_int (f->variable_record);
		if (unlikely (f->record->size > rec->size)) {
			f->record->size = rec->size;
		}
	} else {
		f->record->size = rec->size;
	}

	if (f->record->size < f->record_min || f->record_max < f->record->size) {
		save_status (f, fnstatus, COB_STATUS_44_RECORD_OVERFLOW);
		return;
	}

	check_eop_status = check_eop;
	save_status (f, fnstatus,
		     fileio_funcs[(int)f->organization]->write (f, opt));
}

void
cob_rewrite (cob_file *f, cob_field *rec, const int opt, cob_field *fnstatus)
{
	int	read_done;

	read_done = f->flag_read_done;
	f->flag_read_done = 0;

	if (unlikely (f->open_mode != COB_OPEN_I_O)) {
		save_status (f, fnstatus, COB_STATUS_49_I_O_DENIED);
		return;
	}

	if (f->access_mode == COB_ACCESS_SEQUENTIAL && !read_done) {
		save_status (f, fnstatus, COB_STATUS_43_READ_NOT_DONE);
		return;
	}

	if (unlikely (f->organization == COB_ORG_SEQUENTIAL)) {
		if (f->record->size != rec->size) {
			save_status (f, fnstatus, COB_STATUS_44_RECORD_OVERFLOW);
			return;
		}

		if (f->variable_record) {
			if (f->record->size != (size_t)cob_get_int (f->variable_record)) {
				save_status (f, fnstatus, COB_STATUS_44_RECORD_OVERFLOW);
				return;
			}
		}
	}

	save_status (f, fnstatus,
		     fileio_funcs[(int)f->organization]->rewrite (f, opt));
}

void
cob_delete (cob_file *f, cob_field *fnstatus)
{
	int	read_done;

	read_done = f->flag_read_done;
	f->flag_read_done = 0;

	if (unlikely (f->open_mode != COB_OPEN_I_O)) {
		save_status (f, fnstatus, COB_STATUS_49_I_O_DENIED);
		return;
	}

	if (f->access_mode == COB_ACCESS_SEQUENTIAL && !read_done) {
		save_status (f, fnstatus, COB_STATUS_43_READ_NOT_DONE);
		return;
	}

	save_status (f, fnstatus,
		     fileio_funcs[(int)f->organization]->fdelete (f));
}

void
cob_commit (void)
{
	struct file_list	*l;

	for (l = file_cache; l; l = l->next) {
		if (l->file) {
			cob_file_unlock (l->file);
		}
	}
}

void
cob_rollback (void)
{
	struct file_list	*l;

	for (l = file_cache; l; l = l->next) {
		if (l->file) {
			cob_file_unlock (l->file);
		}
	}
}

void
cob_delete_file (cob_file *f, cob_field *fnstatus)
{
	if (f->organization == COB_ORG_SORT) {
		save_status (f, fnstatus, COB_STATUS_30_PERMANENT_ERROR);
		return;
	}

	/* File was previously closed with lock */
	if (f->open_mode == COB_OPEN_LOCKED) {
		save_status (f, fnstatus, COB_STATUS_38_CLOSED_WITH_LOCK);
		return;
	}

	/* File is open */
	if (f->open_mode != COB_OPEN_CLOSED) {
		save_status (f, fnstatus, COB_STATUS_41_ALREADY_OPEN);
		return;
	}

	if (unlikely (COB_FILE_STDIN (f) || COB_FILE_STDOUT (f))) {
		save_status (f, fnstatus, COB_STATUS_30_PERMANENT_ERROR);
		return;
	}

	/* Obtain the file name */
	cob_field_to_string (f->assign, file_open_name, (size_t)COB_FILE_MAX);
	cob_chk_file_mapping ();

	if (f->organization != COB_ORG_INDEXED) {
#ifdef	WITH_SEQRA_EXTFH
		save_status (f, fnstatus, COB_STATUS_91_NOT_AVAILABLE);
		return;
#else
		unlink (file_open_name);
#endif
	} else {
#ifdef	WITH_INDEX_EXTFH
		save_status (f, fnstatus, COB_STATUS_91_NOT_AVAILABLE);
		return;
#else
		indexed_file_delete (f, file_open_name);
#endif
	}
	save_status (f, fnstatus, COB_STATUS_00_SUCCESS);
}

/* System routines */

static void *
cob_str_from_fld (const cob_field *f)
{
	void		*mptr;
	unsigned char	*s;
	int		i;
	int		n;
	int		j;
#if	0	/* Quotes in file */
	int		quote_switch;

	quote_switch = 0;
#endif

	if (!f) {
		return cob_malloc ((size_t)1);
	}
	for (i = (int) f->size - 1; i >= 0; --i) {
		if (f->data[i] != ' ' && f->data[i] != 0) {
			break;
		}
	}
	i++;
	/* i is 0 or > 0 */
	mptr = cob_malloc ((size_t)(i + 1));
	s = mptr;
	j = 0;
	for (n = 0; n < i; ++n) {
		if (f->data[n] == '"') {
			continue;
		}
		s[j++] = f->data[n];
#if	0	/* Quotes in file */
		if (f->data[n] == '"') {
			quote_switch = !quote_switch;
			continue;
		}
		s[j] = f->data[n];
		if (quote_switch) {
			j++;
			continue;
		}
		if (s[j] == ' ' || s[j] == 0) {
			s[j] = 0;
			break;
		}
		j++;
#endif
	}
	return mptr;
}

static int
open_cbl_file (unsigned char *file_name, unsigned char *file_access,
	       unsigned char *file_handle, const int file_flags)
{
	char	*fn;
	int	flag = O_BINARY;
	int	fd;

	COB_UNUSED (file_name);

	if (!COB_MODULE_PTR->cob_procedure_params[0]) {
		memset (file_handle, -1, (size_t)4);
		return -1;
	}
	flag |= file_flags;
	switch (*file_access & 0x3F) {
		case 1:
			flag |= O_RDONLY;
			break;
		case 2:
			flag |= O_CREAT | O_TRUNC | O_WRONLY;
			break;
		case 3:
			flag |= O_RDWR;
			break;
		default:
			cob_runtime_warning (_("call to CBL_OPEN_FILE with wrong access mode: %d"), *file_access & 0x3F);
			memset (file_handle, -1, (size_t)4);
			return -1;
	}
	fn = cob_str_from_fld (COB_MODULE_PTR->cob_procedure_params[0]);
	fd = open (fn, flag, COB_FILE_MODE);
	if (fd < 0) {
		cob_free (fn);
		memset (file_handle, -1, (size_t)4);
		return 35;
	}
	cob_free (fn);
	memcpy (file_handle, &fd, (size_t)4);
	return 0;
}

int
cob_sys_open_file (unsigned char *file_name, unsigned char *file_access,
		   unsigned char *file_lock, unsigned char *file_dev,
		   unsigned char *file_handle)
{
	COB_UNUSED (file_lock);
	COB_UNUSED (file_dev);

	COB_CHK_PARMS (CBL_OPEN_FILE, 5);

	return open_cbl_file (file_name, file_access, file_handle, 0);
}

int
cob_sys_create_file (unsigned char *file_name, unsigned char *file_access,
		     unsigned char *file_lock, unsigned char *file_dev,
		     unsigned char *file_handle)
{
	/*
	 * @param: file_access : 1 (read-only), 2 (write-only), 3 (both)
	 * @param: file_lock : not implemented, set 0
	 * @param: file_dev : not implemented, set 0
	 */

	if (*file_lock != 0) {
		cob_runtime_warning (_("call to CBL_CREATE_FILE with wrong file_lock: %d"), *file_lock);
	}
	if (*file_dev != 0) {
		cob_runtime_warning (_("call to CBL_CREATE_FILE with wrong file_dev: %d"), *file_dev);
	}

	COB_CHK_PARMS (CBL_CREATE_FILE, 5);

	return open_cbl_file (file_name, file_access, file_handle, O_CREAT | O_TRUNC);
}

int
cob_sys_read_file (unsigned char *file_handle, unsigned char *file_offset,
		   unsigned char *file_len, unsigned char *flags,
		   unsigned char *buf)
{
	cob_s64_t	off;
	int		fd;
	int		len;
	int		rc;
	struct stat	st;

	COB_CHK_PARMS (CBL_READ_FILE, 5);

	rc = 0;
	memcpy (&fd, file_handle, (size_t)4);
	memcpy (&off, file_offset, (size_t)8);
	memcpy (&len, file_len, (size_t)4);
#ifndef	WORDS_BIGENDIAN
	off = COB_BSWAP_64 (off);
	len = COB_BSWAP_32 (len);
#endif
	if (lseek (fd, (off_t)off, SEEK_SET) == (off_t)-1) {
		return -1;
	}
	if (len > 0) {
		rc = read (fd, buf, (size_t)len);
		if (rc < 0) {
			rc = -1;
		} else if (rc == 0) {
			rc = 10;
		} else {
			rc = 0;
		}
	}
	if ((*flags & 0x80) != 0) {
		if (fstat (fd, &st) < 0) {
			return -1;
		}
		off = st.st_size;
#ifndef	WORDS_BIGENDIAN
		off = COB_BSWAP_64 (off);
#endif
		memcpy (file_offset, &off, (size_t)8);
	}
	return rc;
}

int
cob_sys_write_file (unsigned char *file_handle, unsigned char *file_offset,
		    unsigned char *file_len, unsigned char *flags,
		    unsigned char *buf)
{
	cob_s64_t	off;
	int		fd;
	int		len;
	int		rc;

	COB_UNUSED (flags);

	COB_CHK_PARMS (CBL_WRITE_FILE, 5);

	memcpy (&fd, file_handle, (size_t)4);
	memcpy (&off, file_offset, (size_t)8);
	memcpy (&len, file_len, (size_t)4);
#ifndef	WORDS_BIGENDIAN
	off = COB_BSWAP_64 (off);
	len = COB_BSWAP_32 (len);
#endif
	if (lseek (fd, (off_t)off, SEEK_SET) == (off_t)-1) {
		return -1;
	}
	rc = write (fd, buf, (size_t)len);
	if (rc < 0) {
		return 30;
	}
	return 0;
}

int
cob_sys_close_file (unsigned char *file_handle)
{
	int	fd;

	COB_CHK_PARMS (CBL_CLOSE_FILE, 1);

	memcpy (&fd, file_handle, (size_t)4);
	return close (fd);
}

int
cob_sys_flush_file (unsigned char *file_handle)
{
	COB_UNUSED (file_handle);

	COB_CHK_PARMS (CBL_FLUSH_FILE, 1);

	return 0;
}

int
cob_sys_delete_file (unsigned char *file_name)
{
	char	*fn;
	int	ret;

	COB_UNUSED (file_name);

	COB_CHK_PARMS (CBL_DELETE_FILE, 1);

	if (!COB_MODULE_PTR->cob_procedure_params[0]) {
		return -1;
	}
	fn = cob_str_from_fld (COB_MODULE_PTR->cob_procedure_params[0]);
	ret = unlink (fn);
	cob_free (fn);
	if (ret) {
		return 128;
	}
	return 0;
}

int
cob_sys_copy_file (unsigned char *fname1, unsigned char *fname2)
{
	char	*fn1;
	char	*fn2;
	int	flag = O_BINARY;
	int	ret;
	int	i;
	int	fd1, fd2;

	COB_UNUSED (fname1);
	COB_UNUSED (fname2);

	COB_CHK_PARMS (CBL_COPY_FILE, 2);

	if (!COB_MODULE_PTR->cob_procedure_params[0]) {
		return -1;
	}
	if (!COB_MODULE_PTR->cob_procedure_params[1]) {
		return -1;
	}
	fn1 = cob_str_from_fld (COB_MODULE_PTR->cob_procedure_params[0]);
	flag |= O_RDONLY;
	fd1 = open (fn1, flag, 0);
	if (fd1 < 0) {
		cob_free (fn1);
		return -1;
	}
	cob_free (fn1);
	fn2 = cob_str_from_fld (COB_MODULE_PTR->cob_procedure_params[1]);
	flag &= ~O_RDONLY;
	flag |= O_CREAT | O_TRUNC | O_WRONLY;
	fd2 = open (fn2, flag, COB_FILE_MODE);
	if (fd2 < 0) {
		close (fd1);
		cob_free (fn2);
		return -1;
	}
	cob_free (fn2);

	ret = 0;
	while ((i = read (fd1, file_open_buff, COB_FILE_BUFF)) > 0) {
		if (write (fd2, file_open_buff, (size_t)i) < 0) {
			ret = -1;
			break;
		}
	}
	close (fd1);
	close (fd2);
	return ret;
}

int
cob_sys_check_file_exist (unsigned char *file_name, unsigned char *file_info)
{
	char		*fn;
	struct tm	*tm;
	cob_s64_t	sz;
	struct stat	st;
	short		y;
	short		d, m, hh, mm, ss;

	COB_UNUSED (file_name);

	COB_CHK_PARMS (CBL_CHECK_FILE_EXIST, 2);

	if (!COB_MODULE_PTR->cob_procedure_params[0]) {
		return -1;
	}
	if (!COB_MODULE_PTR->cob_procedure_params[1]) {
		return -1;
	}
	if (COB_MODULE_PTR->cob_procedure_params[1]->size < 16U) {
		cob_runtime_error (_("'%s' - File detail area is too short"), "CBL_CHECK_FILE_EXIST");
		cob_stop_run (1);
	}

	fn = cob_str_from_fld (COB_MODULE_PTR->cob_procedure_params[0]);
	if (stat (fn, &st) < 0) {
		cob_free (fn);
		return 35;
	}
	cob_free (fn);
	sz = (cob_s64_t)st.st_size;
	tm = localtime (&st.st_mtime);
	d = (short)tm->tm_mday;
	m = (short)(tm->tm_mon + 1);
	y = (short)(tm->tm_year + 1900);
	hh = (short)tm->tm_hour;
	mm = (short)tm->tm_min;
	/* Leap seconds ? */
	if (tm->tm_sec >= 60) {
		ss = 59;
	} else {
		ss = (short)tm->tm_sec;
	}

#ifndef	WORDS_BIGENDIAN
	sz = COB_BSWAP_64 (sz);
	y = COB_BSWAP_16 (y);
#endif
	memcpy (file_info, &sz, (size_t)8);
	file_info[8] = (unsigned char)d;
	file_info[9] = (unsigned char)m;
	memcpy (file_info+10, &y, (size_t)2);
	file_info[12] = (unsigned char)hh;
	file_info[13] = (unsigned char)mm;
	file_info[14] = (unsigned char)ss;
	file_info[15] = 0;
	return 0;
}

int
cob_sys_rename_file (unsigned char *fname1, unsigned char *fname2)
{
	char	*fn1;
	char	*fn2;
	int	ret;

	COB_UNUSED (fname1);
	COB_UNUSED (fname2);

	COB_CHK_PARMS (CBL_RENAME_FILE, 2);

	if (!COB_MODULE_PTR->cob_procedure_params[0]) {
		return -1;
	}
	if (!COB_MODULE_PTR->cob_procedure_params[1]) {
		return -1;
	}
	fn1 = cob_str_from_fld (COB_MODULE_PTR->cob_procedure_params[0]);
	fn2 = cob_str_from_fld (COB_MODULE_PTR->cob_procedure_params[1]);
	ret = rename (fn1, fn2);
	cob_free (fn1);
	cob_free (fn2);
	if (ret) {
		return 128;
	}
	return 0;
}

int
cob_sys_get_current_dir (const int flags, const int dir_length,
			 unsigned char *dir)
{
	char	*dirname;
	int	dir_size;
	int	has_space;

	COB_CHK_PARMS (CBL_GET_CURRENT_DIR, 3);

	if (dir_length < 1) {
		return 128;
	}
	if (flags) {
		return 129;
	}
	memset (dir, ' ', (size_t)dir_length);
	dirname = getcwd (NULL, (size_t)0);
	if (dirname == NULL) {
		return 128;
	}
	dir_size = (int) strlen (dirname);
	has_space = 0;
	if (strchr (dirname, ' ')) {
		has_space = 2;
	}
	if (dir_size + has_space > dir_length) {
		cob_free (dirname);
		return 128;
	}
	if (has_space) {
		*dir = '"';
		memcpy (&dir[1], dirname, (size_t)dir_size);
		dir[dir_size + 1] = '"';
	} else {
		memcpy (dir, dirname, (size_t)dir_size);
	}
	cob_free (dirname);
	return 0;
}

int
cob_sys_create_dir (unsigned char *dir)
{
	char	*fn;
	int	ret;

	COB_UNUSED (dir);

	COB_CHK_PARMS (CBL_CREATE_DIR, 1);

	if (!COB_MODULE_PTR->cob_procedure_params[0]) {
		return -1;
	}
	fn = cob_str_from_fld (COB_MODULE_PTR->cob_procedure_params[0]);
#ifdef	_WIN32
	ret = mkdir (fn);
#else
	ret = mkdir (fn, 0770);
#endif
	cob_free (fn);
	if (ret) {
		return 128;
	}
	return 0;
}

int
cob_sys_change_dir (unsigned char *dir)
{
	char	*fn;
	int	ret;

	COB_UNUSED (dir);

	COB_CHK_PARMS (CBL_CHANGE_DIR, 1);

	if (!COB_MODULE_PTR->cob_procedure_params[0]) {
		return -1;
	}
	fn = cob_str_from_fld (COB_MODULE_PTR->cob_procedure_params[0]);
	ret = chdir (fn);
	cob_free (fn);
	if (ret) {
		return 128;
	}
	return 0;
}

int
cob_sys_delete_dir (unsigned char *dir)
{
	char	*fn;
	int	ret;

	COB_UNUSED (dir);

	COB_CHK_PARMS (CBL_DELETE_DIR, 1);

	if (!COB_MODULE_PTR->cob_procedure_params[0]) {
		return -1;
	}
	fn = cob_str_from_fld (COB_MODULE_PTR->cob_procedure_params[0]);
	ret = rmdir (fn);
	cob_free (fn);
	if (ret) {
		return 128;
	}
	return 0;
}

int
cob_sys_mkdir (unsigned char *dir)
{
	int		ret;

	COB_CHK_PARMS (C$MAKEDIR, 1);

	ret = cob_sys_create_dir (dir);
	if (ret < 0) {
		ret = 128;
	}
	return ret;
}

int
cob_sys_chdir (unsigned char *dir, unsigned char *status)
{
	int		ret;

	COB_UNUSED (status);

	COB_CHK_PARMS (C$CHDIR, 2);

	ret = cob_sys_change_dir (dir);
	if (ret < 0) {
		ret = 128;
	}
	cob_set_int (COB_MODULE_PTR->cob_procedure_params[1], ret);
	return ret;
}

int
cob_sys_copyfile (unsigned char *fname1, unsigned char *fname2,
		  unsigned char *file_type)
{
	int		ret;

	/* RXW - Type is not yet evaluated */
	COB_UNUSED (file_type);

	COB_CHK_PARMS (C$COPY, 3);

	if (cobglobptr->cob_call_params < 3) {
		return 128;
	}
	ret = cob_sys_copy_file (fname1, fname2);
	if (ret < 0) {
		ret = 128;
	}
	return ret;
}

int
cob_sys_file_info (unsigned char *file_name, unsigned char *file_info)
{
	char			*fn;
	struct tm		*tm;
	cob_u64_t		sz;
	unsigned int		dt;
	short			y;
	short			d, m, hh, mm, ss;
	struct stat		st;

	COB_UNUSED (file_name);

	COB_CHK_PARMS (C$FILEINFO, 2);

	if (cobglobptr->cob_call_params < 2 ||
	    !COB_MODULE_PTR->cob_procedure_params[0]) {
		return 128;
	}
	if (!COB_MODULE_PTR->cob_procedure_params[1]) {
		return 128;
	}
	if (COB_MODULE_PTR->cob_procedure_params[1]->size < 16U) {
		cob_runtime_error (_("'%s' - File detail area is too short"), "C$FILEINFO");
		cob_stop_run (1);
	}

	fn = cob_str_from_fld (COB_MODULE_PTR->cob_procedure_params[0]);
	if (stat (fn, &st) < 0) {
		cob_free (fn);
		return 35;
	}
	cob_free (fn);
	sz = (cob_u64_t)st.st_size;
	tm = localtime (&st.st_mtime);
	d = (short)tm->tm_mday;
	m = (short)(tm->tm_mon + 1);
	y = (short)(tm->tm_year + 1900);
	hh = (short)tm->tm_hour;
	mm = (short)tm->tm_min;
	/* Leap seconds ? */
	if (tm->tm_sec >= 60) {
		ss = 59;
	} else {
		ss = (short)tm->tm_sec;
	}

#ifndef	WORDS_BIGENDIAN
	sz = COB_BSWAP_64 (sz);
#endif
	memcpy (file_info, &sz, (size_t)8);
	dt = (y * 10000) + (m * 100) + d;
#ifndef	WORDS_BIGENDIAN
	dt = COB_BSWAP_32 (dt);
#endif
	memcpy (file_info + 8, &dt, (size_t)4);
	dt = (hh * 1000000) + (mm * 10000) + (ss * 100);
#ifndef	WORDS_BIGENDIAN
	dt = COB_BSWAP_32 (dt);
#endif
	memcpy (file_info + 12, &dt, (size_t)4);
	return 0;
}

int
cob_sys_file_delete (unsigned char *file_name, unsigned char *file_type)
{
	int	ret;

	/* RXW - Type is not yet evaluated */
	COB_UNUSED (file_type);

	COB_CHK_PARMS (C$DELETE, 2);

	if (cobglobptr->cob_call_params < 2 ||
	    !COB_MODULE_PTR->cob_procedure_params[0]) {
		return 128;
	}
	ret = cob_sys_delete_file (file_name);
	if (ret < 0) {
		ret = 128;
	}
	return ret;
}

/* SORT */

static int
sort_cmps (const unsigned char *s1, const unsigned char *s2, const size_t size,
	   const unsigned char *col)
{
	size_t			i;
	int			ret;

	if (unlikely (col)) {
		for (i = 0; i < size; ++i) {
			if ((ret = col[s1[i]] - col[s2[i]]) != 0) {
				return ret;
			}
		}
	} else {
		for (i = 0; i < size; ++i) {
			if ((ret = s1[i] - s2[i]) != 0) {
				return ret;
			}
		}
	}
	return 0;
}

static COB_INLINE void
unique_copy (unsigned char *s1, const unsigned char *s2)
{
	size_t	size;

	size = sizeof(size_t);
	do {
		*s1++ = *s2++;
	} while (--size);
}

static int
cob_file_sort_compare (struct cobitem *k1, struct cobitem *k2, void *pointer)
{
	cob_file	*f;
	size_t		i;
	size_t		u1;
	size_t		u2;
	int		cmp;
	cob_field	f1;
	cob_field	f2;

	f = pointer;
	for (i = 0; i < f->nkeys; ++i) {
		f1 = f2 = *(f->keys[i].field);
		f1.data = k1->item + f->keys[i].offset;
		f2.data = k2->item + f->keys[i].offset;
		if (unlikely (COB_FIELD_IS_NUMERIC (&f1))) {
			cmp = cob_numeric_cmp (&f1, &f2);
		} else {
			cmp = sort_cmps (f1.data, f2.data, f1.size,
					 f->sort_collating);
		}
		if (cmp != 0) {
			return (f->keys[i].flag == COB_ASCENDING) ? cmp : -cmp;
		}
	}
	unique_copy ((unsigned char *)&u1, k1->unique);
	unique_copy ((unsigned char *)&u2, k2->unique);
	if (u1 < u2) {
		return -1;
	}
	return 1;
}

static void
cob_free_list (struct cobsort *hp)
{
	struct sort_mem_struct	*s1;
	struct sort_mem_struct	*s2;

	s1 = hp->mem_base;
	for (; s1;) {
		s2 = s1;
		s1 = s1->next;
		cob_free (s2->mem_ptr);
		cob_free (s2);
	}
}

static struct cobitem *
cob_new_item (struct cobsort *hp, const size_t size)
{
	struct cobitem		*q;
	struct sort_mem_struct	*s;

	COB_UNUSED (size);

	/* Creation of an empty item */
	if (unlikely (hp->empty != NULL)) {
		q = hp->empty;
		hp->empty = q->next;
		q->block_byte = 0;
		q->next = NULL;
		q->end_of_block = 0;
		return (void *)q;
	}
	if (unlikely ((hp->mem_used + hp->alloc_size) > hp->mem_size)) {
		s = cob_fast_malloc (sizeof (struct sort_mem_struct));
		s->mem_ptr = cob_fast_malloc (hp->chunk_size);
		s->next = hp->mem_base;
		hp->mem_base = s;
		hp->mem_size = hp->chunk_size;
		hp->mem_total += hp->chunk_size;
		hp->mem_used = 0;
	}
	q = (struct cobitem *)(hp->mem_base->mem_ptr + hp->mem_used);
	hp->mem_used += hp->alloc_size;
	if (unlikely (hp->mem_total >= cobsetptr->cob_sort_memory)) {
		if ((hp->mem_used + hp->alloc_size) > hp->mem_size) {
			hp->switch_to_file = 1;
		}
	}
	q->block_byte = 0;
	q->next = NULL;
	q->end_of_block = 0;
	return q;
}

static FILE *
cob_srttmpfile (void)
{
	FILE		*fp;
	char		*filename;
	int		fd;

	filename = cob_malloc ((size_t)COB_FILE_BUFF);
	cob_temp_name(filename, NULL);
	cob_incr_temp_iteration();
#ifdef	_WIN32
	fd = open (filename,
		    _O_CREAT | _O_TRUNC | _O_RDWR | _O_BINARY | _O_TEMPORARY,
		    _S_IREAD | _S_IWRITE);
#else
	fd = open (filename, O_CREAT | O_TRUNC | O_RDWR | O_BINARY, COB_FILE_MODE);
#endif
	if (fd < 0) {
		cob_free (filename);
		return NULL;
	}
	(void)unlink (filename);
	fp = fdopen (fd, "w+b");
	if (!fp) {
		close (fd);
	}
	cob_free (filename);
	return fp;
}

static int
cob_get_sort_tempfile (struct cobsort *hp, const int n)
{
	if (hp->file[n].fp == NULL) {
		hp->file[n].fp = cob_srttmpfile ();
		if (hp->file[n].fp == NULL) {
			cob_runtime_error (_("SORT is unable to acquire temporary file"));
			cob_stop_run (1);
		}
	} else {
		rewind (hp->file[n].fp);
	}
	hp->file[n].count = 0;
	return hp->file[n].fp == NULL;
}

static int
cob_sort_queues (struct cobsort *hp)
{
	struct cobitem	*q;
	int		source;
	int		destination;
	int		move;
	int		n;
	int		end_of_block[2];

	source = 0;
	while (hp->queue[source + 1].count != 0) {
		destination = source ^ 2;
		hp->queue[destination].first = NULL;
		hp->queue[destination].count = 0;
		hp->queue[destination + 1].first = NULL;
		hp->queue[destination + 1].count = 0;
		for (;;) {
			end_of_block[0] = hp->queue[source].count == 0;
			end_of_block[1] = hp->queue[source + 1].count == 0;
			if (end_of_block[0] && end_of_block[1]) {
				break;
			}
			while (!end_of_block[0] || !end_of_block[1]) {
				if (end_of_block[0]) {
					move = 1;
				} else if (end_of_block[1]) {
					move = 0;
				} else {
					n = cob_file_sort_compare
						(hp->queue[source].first,
						hp->queue[source + 1].first,
						hp->pointer);
					move = n < 0 ? 0 : 1;
				}
				q = hp->queue[source + move].first;
				if (q->end_of_block) {
					end_of_block[move] = 1;
				}
				hp->queue[source + move].first = q->next;
				if (hp->queue[destination].first == NULL) {
					hp->queue[destination].first = q;
				} else {
					hp->queue[destination].last->next = q;
				}
				hp->queue[destination].last = q;
				hp->queue[source + move].count--;
				hp->queue[destination].count++;
				q->next = NULL;
				q->end_of_block = 0;
			}
			hp->queue[destination].last->end_of_block = 1;
			destination ^= 1;
		}
		source = destination & 2;
	}
	return source;
}

static int
cob_read_item (struct cobsort *hp, const int n)
{
	FILE	*fp;

	fp = hp->file[n].fp;
	if (getc (fp) != 0) {
		hp->queue[n].first->end_of_block = 1;
	} else {
		hp->queue[n].first->end_of_block = 0;
		/* LCOV_EXCL_START */
		if (unlikely (fread (hp->queue[n].first->unique,
				hp->r_size, (size_t)1, fp) != 1)) {
			return 1;
		}
		/* LCOV_EXCL_STOP */
	}
	return 0;
}

static int
cob_write_block (struct cobsort *hp, const int n)
{
	struct cobitem	*q;
	FILE		*fp;

	fp = hp->file[hp->destination_file].fp;
	for (;;) {
		q = hp->queue[n].first;
		if (q == NULL) {
			break;
		}
		/* LCOV_EXCL_START */
		if (unlikely (fwrite (&(q->block_byte),
				hp->w_size, (size_t)1, fp) != 1)) {
			return 1;
		}
		/* LCOV_EXCL_STOP */
		hp->queue[n].first = q->next;
		q->next = hp->empty;
		hp->empty = q;
	}
	hp->queue[n].count = 0;
	hp->file[hp->destination_file].count++;
	/* LCOV_EXCL_START */
	if (unlikely (putc (1, fp) != 1)) {
		return 1;
	}
	/* LCOV_EXCL_STOP */
	return 0;
}

static void
cob_copy_check (cob_file *to, cob_file *from)
{
	unsigned char	*toptr;
	unsigned char	*fromptr;
	size_t		tosize;
	size_t		fromsize;

	toptr = to->record->data;
	fromptr = from->record->data;
	tosize = to->record->size;
	fromsize = from->record->size;
	if (unlikely (tosize > fromsize)) {
		memcpy (toptr, fromptr, fromsize);
		memset (toptr + fromsize, ' ', tosize - fromsize);
	} else {
		memcpy (toptr, fromptr, tosize);
	}
}

static int
cob_file_sort_process (struct cobsort *hp)
{
	int	i;
	int	source;
	int	destination;
	int	n;
	int	move;
	int	res;

	hp->retrieving = 1;
	n = cob_sort_queues (hp);
#if	0	/* RXWRXW - Cannot be true */
	/* LCOV_EXCL_START */
	if (unlikely (n < 0)) {
		return COBSORTABORT;
	}
	/* LCOV_EXCL_STOP */
#endif
	if (likely(!hp->files_used)) {
		hp->retrieval_queue = n;
		return 0;
	}
	/* LCOV_EXCL_START */
	if (unlikely (cob_write_block (hp, n))) {
		return COBSORTFILEERR;
	}
	/* LCOV_EXCL_STOP */
	for (i = 0; i < 4; ++i) {
		hp->queue[i].first = hp->empty;
		hp->empty = hp->empty->next;
		hp->queue[i].first->next = NULL;
	}
	rewind (hp->file[0].fp);
	rewind (hp->file[1].fp);
	/* LCOV_EXCL_START */
	if (unlikely (cob_get_sort_tempfile (hp, 2))) {
		return COBSORTFILEERR;
	}
	if (unlikely (cob_get_sort_tempfile (hp, 3))) {
		return COBSORTFILEERR;
	}
	/* LCOV_EXCL_STOP */
	source = 0;
	while (hp->file[source].count > 1) {
		destination = source ^ 2;
		hp->file[destination].count = 0;
		hp->file[destination + 1].count = 0;
		while (hp->file[source].count > 0) {
			/* LCOV_EXCL_START */
			if (unlikely (cob_read_item (hp, source))) {
				return COBSORTFILEERR;
			}
			/* LCOV_EXCL_STOP */
			if (hp->file[source + 1].count > 0) {
				/* LCOV_EXCL_START */
				if (unlikely (cob_read_item (hp, source + 1))) {
					return COBSORTFILEERR;
				}
				/* LCOV_EXCL_STOP */
			} else {
				hp->queue[source + 1].first->end_of_block = 1;
			}
			while (!hp->queue[source].first->end_of_block ||
			       !hp->queue[source + 1].first->end_of_block) {
				if (hp->queue[source].first->end_of_block) {
					move = 1;
				} else if (hp->queue[source + 1].first->end_of_block) {
					move = 0;
				} else {
					res = cob_file_sort_compare
						(hp->queue[source].first,
						hp->queue[source + 1].first,
						hp->pointer);
					move = res < 0 ? 0 : 1;
				}
				/* LCOV_EXCL_START */
				if (unlikely (fwrite (
				    &(hp->queue[source + move].first->block_byte),
				    hp->w_size, (size_t)1,
				    hp->file[destination].fp) != 1)) {
					return COBSORTFILEERR;
				}
				if (unlikely(cob_read_item (hp, source + move))) {
					return COBSORTFILEERR;
				}
				/* LCOV_EXCL_STOP */
			}
			hp->file[destination].count++;
			/* LCOV_EXCL_START */
			if (unlikely (putc (1, hp->file[destination].fp) != 1)) {
				return COBSORTFILEERR;
			}
			/* LCOV_EXCL_STOP */
			hp->file[source].count--;
			hp->file[source + 1].count--;
			destination ^= 1;
		}
		source = destination & 2;
		rewind (hp->file[0].fp);
		rewind (hp->file[1].fp);
		rewind (hp->file[2].fp);
		rewind (hp->file[3].fp);
	}
	hp->retrieval_queue = source;
	/* LCOV_EXCL_START */
	if (unlikely (cob_read_item (hp, source))) {
		return COBSORTFILEERR;
	}
	if (unlikely (cob_read_item (hp, source + 1))) {
		return COBSORTFILEERR;
	}
	/* LCOV_EXCL_STOP */
	return 0;
}

static int
cob_file_sort_submit (cob_file *f, const unsigned char *p)
{
	struct cobsort		*hp;
	struct cobitem		*q;
	struct queue_struct	*z;
	int			n;

	hp = f->file;
	if (unlikely (!hp)) {
		return COBSORTNOTOPEN;
	}
	if (unlikely (hp->retrieving)) {
		return COBSORTABORT;
	}
	if (unlikely (hp->switch_to_file)) {
		if (!hp->files_used) {
			/* LCOV_EXCL_START */
			if (unlikely (cob_get_sort_tempfile (hp, 0))) {
				return COBSORTFILEERR;
			}
			if (unlikely (cob_get_sort_tempfile (hp, 1))) {
				return COBSORTFILEERR;
			}
			/* LCOV_EXCL_STOP */
			hp->files_used = 1;
			hp->destination_file = 0;
		}
		n = cob_sort_queues (hp);
#if	0	/* RXWRXW - Cannot be true */
		/* LCOV_EXCL_START */
		if (unlikely (n < 0)) {
			return COBSORTABORT;
		}
		/* LCOV_EXCL_STOP */
#endif
		/* LCOV_EXCL_START */
		if (unlikely (cob_write_block (hp, n))) {
			return COBSORTFILEERR;
		}
		/* LCOV_EXCL_STOP */
		hp->destination_file ^= 1;
	}
	q = cob_new_item (hp, sizeof (struct cobitem) + hp->size);
	q->end_of_block = 1;
	unique_copy (q->unique, (const unsigned char *)&(hp->unique));
	hp->unique++;
	memcpy (q->item, p, hp->size);
	if (hp->queue[0].count <= hp->queue[1].count) {
		z = &hp->queue[0];
	} else {
		z = &hp->queue[1];
	}
	q->next = z->first;
	z->first = q;
	z->count++;
	return 0;
}

static int
cob_file_sort_retrieve (cob_file *f, unsigned char *p)
{
	struct cobsort		*hp;
	struct cobitem		*next;
	struct queue_struct	*z;
	int			move;
	int			source;
	int			res;

	hp = f->file;
	if (unlikely (!hp)) {
		return COBSORTNOTOPEN;
	}
	if (unlikely (!hp->retrieving)) {
		res = cob_file_sort_process (hp);
		if (res) {
			return res;
		}
	}
	if (unlikely (hp->files_used)) {
		source = hp->retrieval_queue;
		if (hp->queue[source].first->end_of_block) {
			if (hp->queue[source + 1].first->end_of_block) {
				return COBSORTEND;
			}
			move = 1;
		} else if (hp->queue[source + 1].first->end_of_block) {
			move = 0;
		} else {
			res = cob_file_sort_compare (hp->queue[source].first,
						hp->queue[source + 1].first,
						hp->pointer);
			move = res < 0 ? 0 : 1;
		}
		memcpy (p, hp->queue[source + move].first->item, hp->size);
		/* LCOV_EXCL_START */
		if (unlikely (cob_read_item (hp, source + move))) {
			return COBSORTFILEERR;
		}
		/* LCOV_EXCL_STOP */
	} else {
		z = &hp->queue[hp->retrieval_queue];
		if (z->first == NULL) {
			return COBSORTEND;
		}
		memcpy (p, z->first->item, hp->size);
		next = z->first->next;
		z->first->next = hp->empty;
		hp->empty = z->first;
		z->first = next;
	}
	return 0;
}

void
cob_file_sort_using (cob_file *sort_file, cob_file *data_file)
{
	int		ret;

	cob_open (data_file, COB_OPEN_INPUT, 0, NULL);
	for (;;) {
		cob_read_next (data_file, NULL, COB_READ_NEXT);
		if (data_file->file_status[0] != '0') {
			break;
		}
		cob_copy_check (sort_file, data_file);
		ret = cob_file_sort_submit (sort_file, sort_file->record->data);
		if (ret) {
			break;
		}
	}
	cob_close (data_file, NULL, COB_CLOSE_NORMAL, 0);
}

void
cob_file_sort_giving (cob_file *sort_file, const size_t varcnt, ...)
{
	cob_file	**fbase;
	struct cobsort	*hp;
	size_t		i;
	int		ret;
	int		opt;
	va_list		args;

	fbase = cob_malloc (varcnt * sizeof(cob_file *));
	va_start (args, varcnt);
	for (i = 0; i < varcnt; ++i) {
		fbase[i] = va_arg (args, cob_file *);
	}
	va_end (args);
	for (i = 0; i < varcnt; ++i) {
		cob_open (fbase[i], COB_OPEN_OUTPUT, 0, NULL);
	}
	for (;;) {
		ret = cob_file_sort_retrieve (sort_file, sort_file->record->data);
		if (ret) {
			if (ret == COBSORTEND) {
				sort_file->file_status[0] = '1';
				sort_file->file_status[1] = '0';
			} else {
				hp = sort_file->file;
				if (hp->sort_return) {
					*(int *)(hp->sort_return) = 16;
				}
				sort_file->file_status[0] = '3';
				sort_file->file_status[1] = '0';
			}
			break;
		}
		for (i = 0; i < varcnt; ++i) {
			if (COB_FILE_SPECIAL (fbase[i]) ||
			    fbase[i]->organization == COB_ORG_LINE_SEQUENTIAL) {
				opt = COB_WRITE_BEFORE | COB_WRITE_LINES | 1;
			} else {
				opt = 0;
			}
			fbase[i]->record->size = fbase[i]->record_max;
			cob_copy_check (fbase[i], sort_file);
			cob_write (fbase[i], fbase[i]->record, opt, NULL, 0);
		}
	}
	for (i = 0; i < varcnt; ++i) {
		cob_close (fbase[i], NULL, COB_CLOSE_NORMAL, 0);
	}
	cob_free (fbase);
}

void
cob_file_sort_init (cob_file *f, const unsigned int nkeys,
		    const unsigned char *collating_sequence,
		    void *sort_return, cob_field *fnstatus)
{
	struct cobsort	*p;
	size_t		n;

	p = cob_malloc (sizeof (struct cobsort));
	p->fnstatus = fnstatus;
	p->size = f->record_max;
	p->r_size = f->record_max + sizeof(size_t);
	p->w_size = f->record_max + sizeof(size_t) + 1;
	n = sizeof (struct cobitem) - offsetof (struct cobitem, item);
	if (f->record_max <= n) {
		p->alloc_size = sizeof (struct cobitem);
	} else {
		p->alloc_size = offsetof (struct cobitem, item) + f->record_max;
	}
	if (p->alloc_size % sizeof(void *)) {
		p->alloc_size += sizeof(void *) - (p->alloc_size % sizeof(void *));
	}
	p->chunk_size = cobsetptr->cob_sort_chunk;
	if (p->chunk_size % p->alloc_size) {
		p->chunk_size += p->alloc_size - (p->chunk_size % p->alloc_size);
	}
	p->pointer = f;
	if (sort_return) {
		p->sort_return = sort_return;
		*(int *)sort_return = 0;
	}
	p->mem_base = cob_fast_malloc (sizeof (struct sort_mem_struct));
	p->mem_base->mem_ptr = cob_fast_malloc (p->chunk_size);
	p->mem_base->next = NULL;
	p->mem_size = p->chunk_size;
	p->mem_total = p->chunk_size;
	f->file = p;
	f->keys = cob_malloc (sizeof (cob_file_key) * nkeys);
	f->nkeys = 0;
	if (collating_sequence) {
		f->sort_collating = collating_sequence;
	} else {
		f->sort_collating = COB_MODULE_PTR->collating_sequence;
	}
	save_status (f, fnstatus, COB_STATUS_00_SUCCESS);
}

void
cob_file_sort_init_key (cob_file *f, cob_field *field, const int flag,
			const unsigned int offset)
{
	f->keys[f->nkeys].field = field;
	f->keys[f->nkeys].flag = flag;
	f->keys[f->nkeys].offset = offset;
	f->nkeys++;
}

void
cob_file_sort_close (cob_file *f)
{
	struct cobsort	*hp;
	cob_field	*fnstatus;
	size_t		i;

	fnstatus = NULL;
	hp = f->file;
	if (likely(hp)) {
		fnstatus = hp->fnstatus;
		cob_free_list (hp);
		for (i = 0; i < 4; ++i) {
			if (hp->file[i].fp != NULL) {
				fclose (hp->file[i].fp);
			}
		}
		cob_free (hp);
	}
	if (f->keys) {
		cob_free (f->keys);
	}
	f->file = NULL;
	save_status (f, fnstatus, COB_STATUS_00_SUCCESS);
}

void
cob_file_release (cob_file *f)
{
	struct cobsort	*hp;
	cob_field	*fnstatus;
	int		ret;

	fnstatus = NULL;
	hp = f->file;
	if (likely(hp)) {
		fnstatus = hp->fnstatus;
	}
	ret = cob_file_sort_submit (f, f->record->data);
	if (!ret) {
		save_status (f, fnstatus, COB_STATUS_00_SUCCESS);
		return;
	}
	if (likely(hp && hp->sort_return)) {
		*(int *)(hp->sort_return) = 16;
	}
	save_status (f, fnstatus, COB_STATUS_30_PERMANENT_ERROR);
}

void
cob_file_return (cob_file *f)
{
	struct cobsort	*hp;
	cob_field	*fnstatus;
	int		ret;

	fnstatus = NULL;
	hp = f->file;
	if (likely(hp)) {
		fnstatus = hp->fnstatus;
	}
	ret = cob_file_sort_retrieve (f, f->record->data);
	switch (ret) {
	case 0:
		save_status (f, fnstatus, COB_STATUS_00_SUCCESS);
		return;
	case COBSORTEND:
		save_status (f, fnstatus, COB_STATUS_10_END_OF_FILE);
		return;
	}
	if (likely(hp && hp->sort_return)) {
		*(int *)(hp->sort_return) = 16;
	}
	save_status (f, fnstatus, COB_STATUS_30_PERMANENT_ERROR);
}

/* Initialization/Termination
   cobsetpr-values with type ENV_PATH or ENV_STR
   like bdb_home and cob_file_path are taken care in cob_exit_common()!
*/

void
cob_exit_fileio (void)
{
	struct file_list	*l;
	struct file_list	*p;

	for (l = file_cache; l; l = l->next) {
		if (l->file && l->file->open_mode != COB_OPEN_CLOSED &&
		    l->file->open_mode != COB_OPEN_LOCKED &&
		    !l->file->flag_nonexistent) {
			if (COB_FILE_SPECIAL (l->file)) {
				continue;
			}
			cob_close (l->file, NULL, COB_CLOSE_NORMAL, 0);
			if (cobsetptr->cob_display_warn) {
				cob_field_to_string (l->file->assign,
						     runtime_buffer,
						     (size_t)COB_FILE_MAX);
				cob_runtime_warning (_("implicit CLOSE of %s ('%s')"),
					l->file->select_name, runtime_buffer);
			}
		}
	}
#ifdef	WITH_DB
	if (bdb_env) {
		bdb_env->lock_id_free (bdb_env, bdb_lock_id);
		bdb_env->close (bdb_env, 0);
		bdb_env = NULL;
	}
	if (record_lock_object) {
		cob_free (record_lock_object);
		record_lock_object = NULL;
	}
	if (bdb_buff) {
		cob_free (bdb_buff);
		bdb_buff = NULL;
	}

#elif	defined(WITH_ANY_ISAM)
#ifndef	WITH_DISAM
	(void)iscleanup ();
#endif
#endif

#if	defined(WITH_INDEX_EXTFH) || defined(WITH_SEQRA_EXTFH)
	extfh_cob_exit_fileio ();
#endif

	if (runtime_buffer) {
		cob_free (runtime_buffer);
		runtime_buffer = NULL;
	}

	for (l = file_cache; l;) {
		p = l;
		l = l->next;
		cob_free (p);
	}
	file_cache = NULL;
}

void
cob_init_fileio (cob_global *lptr, cob_settings *sptr)
{

	cobglobptr = lptr;
	cobsetptr  = sptr;
	file_cache = NULL;
	eop_status = 0;
	check_eop_status = 0;
	if (cobsetptr->cob_sort_chunk > (cobsetptr->cob_sort_memory / 2)) {
		cobsetptr->cob_sort_chunk = cobsetptr->cob_sort_memory / 2;
	}

	if (cobsetptr->cob_varseq_type == 3) {
		cob_vsq_len = 2;
	} else {
		cob_vsq_len = 4;
	}

	runtime_buffer = cob_fast_malloc ((size_t)(4 * COB_FILE_BUFF));
	file_open_env = runtime_buffer + COB_FILE_BUFF;
	file_open_name = runtime_buffer + (2 * COB_FILE_BUFF);
	file_open_buff = runtime_buffer + (3 * COB_FILE_BUFF);

#ifdef	WITH_DB
	bdb_env = NULL;
	bdb_data_dir = NULL;
	record_lock_object = cob_malloc ((size_t)1024);
	bdb_buff = cob_malloc ((size_t)COB_SMALL_BUFF);
	rlo_size = 1024;
#endif

#if	defined(WITH_INDEX_EXTFH) || defined(WITH_SEQRA_EXTFH)
	extfh_cob_init_fileio (&sequential_funcs, &lineseq_funcs,
			       &relative_funcs, &cob_file_write_opt);
#endif
}
