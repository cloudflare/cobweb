/*
   Copyright (C) 2006-2012 Free Software Foundation, Inc.
   Written by Roger While

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
#include <stdarg.h>
#include <string.h>
#include <ctype.h>

#include "cobc.h"
#include "tree.h"

static void
output_storage (const char *fmt, ...)
{
	va_list		ap;

	if (cb_storage_file) {
		va_start (ap, fmt);
		vfprintf (cb_storage_file, fmt, ap);
		va_end (ap);
		fputc ('\n', cb_storage_file);
	}
}

void
cob_gen_optim (const enum cb_optim val)
{
	switch (val) {

	case COB_SET_SCREEN:
		output_storage ("static void COB_NOINLINE");
		output_storage ("cob_set_screen (cob_screen *s, cob_screen *next,");
		output_storage ("		cob_screen *prev, cob_screen *child, cob_screen *parent,");
		output_storage ("		cob_field *field, cob_field *value,");
		output_storage ("		cob_field *line, cob_field *column,");
		output_storage ("		cob_field *foreg, cob_field *backg, cob_field *prompt,");
		output_storage ("		const int type, const int occurs, const int attr)");
		output_storage ("{");
		output_storage ("	s->next = next;");
		output_storage ("	s->prev = prev;");
		output_storage ("	s->child = child;");
		output_storage ("	s->parent = parent;");
		output_storage ("	s->field = field;");
		output_storage ("	s->value = value;");
		output_storage ("	s->line = line;");
		output_storage ("	s->column = column;");
		output_storage ("	s->foreg = foreg;");
		output_storage ("	s->backg = backg;");
		output_storage ("	s->prompt = prompt;");
		output_storage ("	s->type = type;");
		output_storage ("	s->occurs = occurs;");
		output_storage ("	s->attr = attr;");
		output_storage ("}");
		return;

	case COB_POINTER_MANIP:
		output_storage ("static void COB_NOINLINE");
		output_storage ("cob_pointer_manip (cob_field *f1, cob_field *f2, const unsigned int addsub)");
		output_storage ("{");
		output_storage ("	unsigned char   *tmptr;");
		output_storage ("	memcpy (&tmptr, f1->data, sizeof(void *));");
		output_storage ("	if (addsub) {");
		output_storage ("		tmptr -= cob_get_int (f2);");
		output_storage ("	} else {");
		output_storage ("		tmptr += cob_get_int (f2);");
		output_storage ("	}");
		output_storage ("	memcpy (f1->data, &tmptr, sizeof(void *));");
		output_storage ("}");
		return;

	case COB_GET_NUMDISP:
		output_storage ("static int COB_NOINLINE");
		output_storage ("cob_get_numdisp (const void *data, const size_t size)");
		output_storage ("{");
		output_storage ("	const unsigned char	*p;");
		output_storage ("	size_t			n;");
		output_storage ("	int    			 retval;");

		output_storage ("	p = (const unsigned char *)data;");
		output_storage ("	retval = 0;");
		output_storage ("	for (n = 0; n < size; ++n, ++p) {");
		output_storage ("		retval *= 10;");
		output_storage ("		retval += (*p & 0x0F);");
		output_storage ("	}");
		output_storage ("	return retval;");
		output_storage ("}");
		return;

	case COB_CMP_PACKED_INT:
		output_storage ("static int COB_NOINLINE");
		output_storage ("cob_cmp_packed_int (const cob_field *f, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	unsigned char	*p;");
		output_storage ("	size_t		size;");
		output_storage ("	cob_s64_t	val;");

		output_storage ("	val = 0;");
		output_storage ("	p = f->data;");
		output_storage ("	for (size = 0; size < f->size - 1; ++size, ++p) {");
		output_storage ("		val *= 10;");
		output_storage ("		val += *p >> 4;");
		output_storage ("		val *= 10;");
		output_storage ("		val += *p & 0x0f;");
		output_storage ("	}");
		output_storage ("	val *= 10;");
		output_storage ("	val += *p >> 4;");
		output_storage ("	if ((*p & 0x0f) == 0x0d) {");
		output_storage ("		val = -val;");
		output_storage ("	}");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_GET_PACKED_INT:
		output_storage ("static int COB_NOINLINE");
		output_storage ("cob_get_packed_int (const cob_field *f)");
		output_storage ("{");
		output_storage ("	unsigned char	*p;");
		output_storage ("	size_t		size;");
		output_storage ("	int		val = 0;");

		output_storage ("	p = f->data;");
		output_storage ("	for (size = 0; size < f->size - 1; ++size, ++p) {");
		output_storage ("		val *= 10;");
		output_storage ("		val += *p >> 4;");
		output_storage ("		val *= 10;");
		output_storage ("		val += *p & 0x0f;");
		output_storage ("	}");
		output_storage ("	val *= 10;");
		output_storage ("	val += *p >> 4;");
		output_storage ("	if ((*p & 0x0f) == 0x0d) {");
		output_storage ("		val = -val;");
		output_storage ("	}");
		output_storage ("	return val;");
		output_storage ("}");
		return;

	case COB_ADD_PACKED_INT:
		output_storage ("static int COB_NOINLINE");
		output_storage ("cob_add_packed_int (cob_field *f, const int val)");
		output_storage ("{");
		output_storage ("	unsigned char	*p;");
		output_storage ("	size_t		size;");
		output_storage ("	int		carry = 0;");
		output_storage ("	int		n;");
		output_storage ("	int		inc;");

		output_storage ("	if (val == 0) {");
		output_storage ("		return 0;");
		output_storage ("	}");
		output_storage ("	p = f->data + f->size - 1;");
		output_storage ("	if ((*p & 0x0f) == 0x0d) {");
		output_storage ("		if (val > 0) {");
		output_storage ("			return cob_add_int (f, val, 0);");
		output_storage ("		}");
		output_storage ("		n = -val;");
		output_storage ("	} else {");
		output_storage ("		if (val < 0) {");
		output_storage ("			return cob_add_int (f, val, 0);");
		output_storage ("		}");
		output_storage ("		n = val;");
		output_storage ("	}");
		output_storage ("	inc = (*p >> 4) + (n %% 10);");
		output_storage ("	n /= 10;");
		output_storage ("	carry = inc / 10;");
		output_storage ("	*p = ((inc %% 10) << 4) | (*p & 0x0f);");
		output_storage ("	p--;");

		output_storage ("	for (size = 0; size < f->size - 1; ++size, --p) {");
		output_storage ("		if (!carry && !n) {");
		output_storage ("			break;");
		output_storage ("		}");
		output_storage ("		inc = ((*p >> 4) * 10) + (*p & 0x0f) + carry + (n %% 100);");
		output_storage ("		carry = inc / 100;");
		output_storage ("		n /= 100;");
		output_storage ("		inc %%= 100;");
		output_storage ("		*p = ((inc / 10) << 4) | (inc %% 10);");
		output_storage ("	}");
		output_storage ("	return 0;");
		output_storage ("}");
		return;

	/* Aligned variants */

	/* Aligned compares */

	case COB_CMP_ALIGN_U16:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmp_align_u16 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	unsigned short	val;");

		output_storage ("	if (unlikely(n < 0)) {");
		output_storage ("		return 1;");
		output_storage ("	}");
		output_storage ("	val = *(unsigned short __unaligned *)p;");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMP_ALIGN_S16:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmp_align_s16 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	short	val;");

		output_storage ("	val = *(short __unaligned *)p;");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMP_ALIGN_U32:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmp_align_u32 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	unsigned int	val;");

		output_storage ("	if (unlikely(n < 0)) {");
		output_storage ("		return 1;");
		output_storage ("	}");
		output_storage ("	val = *(unsigned int __unaligned *)p;");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMP_ALIGN_S32:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmp_align_s32 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	int	val;");

		output_storage ("	val = *(int __unaligned *)p;");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMP_ALIGN_U64:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmp_align_u64 (const void *p, const cob_u64_t n)");
		output_storage ("{");
		output_storage ("	cob_u64_t	val;");

		output_storage ("	val = *(cob_u64_t __unaligned *)p;");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMP_ALIGN_S64:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmp_align_s64 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	cob_s64_t	val;");

		output_storage ("	val = *(cob_s64_t __unaligned *)p;");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	/* Aligned adds */

	case COB_ADD_ALIGN_U16:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_add_align_u16 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	*(unsigned short __unaligned *)p += val;");
		output_storage ("}");
		return;

	case COB_ADD_ALIGN_S16:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_add_align_s16 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	*(short __unaligned *)p += val;");
		output_storage ("}");
		return;

	case COB_ADD_ALIGN_U32:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_add_align_u32 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	*(unsigned int __unaligned *)p += val;");
		output_storage ("}");
		return;

	case COB_ADD_ALIGN_S32:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_add_align_s32 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	*(int __unaligned *)p += val;");
		output_storage ("}");
		return;

	case COB_ADD_ALIGN_U64:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_add_align_u64 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	*(cob_u64_t __unaligned *)p += val;");
		output_storage ("}");
		return;

	case COB_ADD_ALIGN_S64:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_add_align_s64 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	*(cob_s64_t __unaligned *)p += val;");
		output_storage ("}");
		return;

	/* Aligned subtracts */

	case COB_SUB_ALIGN_U16:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_sub_align_u16 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	*(unsigned short __unaligned *)p -= val;");
		output_storage ("}");
		return;

	case COB_SUB_ALIGN_S16:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_sub_align_s16 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	*(short __unaligned *)p -= val;");
		output_storage ("}");
		return;

	case COB_SUB_ALIGN_U32:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_sub_align_u32 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	*(unsigned int __unaligned *)p -= val;");
		output_storage ("}");
		return;

	case COB_SUB_ALIGN_S32:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_sub_align_s32 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	*(int __unaligned *)p -= val;");
		output_storage ("}");
		return;

	case COB_SUB_ALIGN_U64:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_sub_align_u64 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	*(cob_u64_t __unaligned *)p -= val;");
		output_storage ("}");
		return;

	case COB_SUB_ALIGN_S64:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_sub_align_s64 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	*(cob_s64_t __unaligned *)p -= val;");
		output_storage ("}");
		return;

	case COB_CMPSWP_ALIGN_U16:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmpswp_align_u16 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	unsigned short	val;");

		output_storage ("	if (unlikely(n < 0)) {");
		output_storage ("		return 1;");
		output_storage ("	}");
		output_storage ("	val = COB_BSWAP_16 (*(unsigned short __unaligned *)p);");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMPSWP_ALIGN_S16:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmpswp_align_s16 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	short	val;");

		output_storage ("	val = COB_BSWAP_16 (*(short __unaligned *)p);");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMPSWP_ALIGN_U32:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmpswp_align_u32 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	unsigned int	val;");

		output_storage ("	if (unlikely(n < 0)) {");
		output_storage ("		return 1;");
		output_storage ("	}");
		output_storage ("	val = COB_BSWAP_32 (*(unsigned int __unaligned *)p);");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMPSWP_ALIGN_S32:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmpswp_align_s32 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	int	val;");

		output_storage ("	val = COB_BSWAP_32 (*(int __unaligned *)p);");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMPSWP_ALIGN_U64:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmpswp_align_u64 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	cob_u64_t	val;");

		output_storage ("	if (unlikely(n < 0)) {");
		output_storage ("		return 1;");
		output_storage ("	}");
		output_storage ("	val = COB_BSWAP_64 (*(cob_u64_t __unaligned *)p);");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMPSWP_ALIGN_S64:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmpswp_align_s64 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	cob_s64_t	val;");

		output_storage ("	val = COB_BSWAP_64 (*(cob_s64_t __unaligned *)p);");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	/* Binary compare */

	case COB_CMP_U8:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmp_u8 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	if (unlikely(n < 0)) {");
		output_storage ("		return 1;");
		output_storage ("	}");
		output_storage ("	return (*(const unsigned char *)p < n) ? -1 : (*(const unsigned char *)p > n);");
		output_storage ("}");
		return;

	case COB_CMP_S8:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmp_s8 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	return (*(const signed char *)p < n) ? -1 : (*(const signed char *)p > n);");
		output_storage ("}");
		return;

	case COB_CMP_U16:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmp_u16 (const void *p, const cob_s64_t n)");
		output_storage ("{");
#ifndef COB_ALLOW_UNALIGNED
		output_storage ("	void		*x;");
#endif
		output_storage ("	unsigned short	val;");

		output_storage ("	if (unlikely(n < 0)) {");
		output_storage ("		return 1;");
		output_storage ("	}");
#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	val = *(const unsigned short __unaligned *)p;");
#else
		output_storage ("	x = &val;");
		output_storage ("	optim_memcpy (x, p, 2);");
#endif
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMP_S16:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmp_s16 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	short	val;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	val = *(const short __unaligned *)p;");
#else
		output_storage ("	void	*x;");

		output_storage ("	x = &val;");
		output_storage ("	optim_memcpy (x, p, 2);");
#endif
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMP_U24:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmp_u24 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	unsigned char	*x;");
		output_storage ("	unsigned int	val = 0;");

		output_storage ("	if (unlikely(n < 0)) {");
		output_storage ("		return 1;");
		output_storage ("	}");
#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = ((unsigned char *)&val) + 1;");
#else
		output_storage ("	x = (unsigned char *)&val;");
#endif
		output_storage ("	optim_memcpy (x, p, 3);");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMP_S24:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmp_s24 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	unsigned char	*x;");
		output_storage ("	int		val = 0;");

#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = (unsigned char *)&val;");
#else
		output_storage ("	x = ((unsigned char *)&val) + 1;");
#endif
		output_storage ("	optim_memcpy (x, p, 3);");
		output_storage ("	val >>= 8;	/* Shift with sign */");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMP_U32:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmp_u32 (const void *p, const cob_s64_t n)");
		output_storage ("{");
#ifndef COB_ALLOW_UNALIGNED
		output_storage ("	void		*x;");
#endif
		output_storage ("	unsigned int	val;");

		output_storage ("	if (unlikely(n < 0)) {");
		output_storage ("		return 1;");
		output_storage ("	}");
#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	val = *(const unsigned int __unaligned *)p;");
#else
		output_storage ("	x = &val;");
		output_storage ("	optim_memcpy (x, p, 4);");
#endif
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMP_S32:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmp_s32 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	int	val;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	val = *(const int __unaligned *)p;");
#else
		output_storage ("	void	*x;");

		output_storage ("	x = &val;");
		output_storage ("	optim_memcpy (x, p, 4);");
#endif
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMP_U40:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmp_u40 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	cob_u64_t		val = 0;");
		output_storage ("	unsigned char		*x;");

		output_storage ("	if (unlikely(n < 0)) {");
		output_storage ("		return 1;");
		output_storage ("	}");
#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = ((unsigned char *)&val) + 3;");
#else
		output_storage ("	x = (unsigned char *)&val;");
#endif
		output_storage ("	optim_memcpy (x, p, 5);");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMP_S40:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmp_s40 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	cob_s64_t		val = 0;");
		output_storage ("	unsigned char		*x;");

#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = (unsigned char *)&val;");
#else
		output_storage ("	x = ((unsigned char *)&val) + 3;");
#endif
		output_storage ("	optim_memcpy (x, p, 5);");
		output_storage ("	val >>= 24;	/* Shift with sign */");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMP_U48:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmp_u48 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	cob_u64_t		val = 0;");
		output_storage ("	unsigned char		*x;");

		output_storage ("	if (unlikely(n < 0)) {");
		output_storage ("		return 1;");
		output_storage ("	}");
#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = ((unsigned char *)&val) + 2;");
#else
		output_storage ("	x = (unsigned char *)&val;");
#endif
		output_storage ("	optim_memcpy (x, p, 6);");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMP_S48:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmp_s48 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	cob_s64_t		val = 0;");
		output_storage ("	unsigned char		*x;");

#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = (unsigned char *)&val;");
#else
		output_storage ("	x = ((unsigned char *)&val) + 2;");
#endif
		output_storage ("	optim_memcpy (x, p, 6);");
		output_storage ("	val >>= 16;	/* Shift with sign */");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMP_U56:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmp_u56 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	cob_u64_t		val = 0;");
		output_storage ("	unsigned char		*x;");

		output_storage ("	if (unlikely(n < 0)) {");
		output_storage ("		return 1;");
		output_storage ("	}");
#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = ((unsigned char *)&val) + 1;");
#else
		output_storage ("	x = (unsigned char *)&val;");
#endif
		output_storage ("	optim_memcpy (x, p, 7);");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMP_S56:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmp_s56 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	cob_s64_t		val = 0;");
		output_storage ("	unsigned char		*x;");

#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = (unsigned char *)&val;");
#else
		output_storage ("	x = ((unsigned char *)&val) + 1;");
#endif
		output_storage ("	optim_memcpy (x, p, 7);");
		output_storage ("	val >>= 8;	/* Shift with sign */");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMP_U64:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmp_u64 (const void *p, const cob_u64_t n)");
		output_storage ("{");
#ifndef COB_ALLOW_UNALIGNED
		output_storage ("	void		*x;");
#endif
		output_storage ("	cob_u64_t	val;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	val = *(const cob_u64_t __unaligned *)p;");
#else
		output_storage ("	x = &val;");
		output_storage ("	optim_memcpy (x, p, 8);");
#endif
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMP_S64:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmp_s64 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	cob_s64_t	val;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	val = *(const cob_s64_t __unaligned *)p;");
#else
		output_storage ("	void		*x;");

		output_storage ("	x = &val;");
		output_storage ("	optim_memcpy (x, p, 8);");
#endif
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	/* Add/Subtract */

	case COB_ADD_U8:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_add_u8 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	*(unsigned char *)p += val;");
		output_storage ("}");
		return;

	case COB_ADD_S8:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_add_s8 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	*(signed char *)p += val;");
		output_storage ("}");
		return;

	case COB_ADD_U16:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_add_u16 (void *p, const int val)");
		output_storage ("{");
#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	*(unsigned short __unaligned *)p += val;");
#else
		output_storage ("	void		*x;");
		output_storage ("	unsigned short	n;");

		output_storage ("	x = &n;");
		output_storage ("	optim_memcpy (x, p, 2);");
		output_storage ("	n += val;");
		output_storage ("	optim_memcpy (p, x, 2);");
#endif
		output_storage ("}");
		return;

	case COB_ADD_S16:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_add_s16 (void *p, const int val)");
		output_storage ("{");
#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	*(short __unaligned *)p += val;");
#else
		output_storage ("	void	*x;");
		output_storage ("	short	n;");

		output_storage ("	x = &n;");
		output_storage ("	optim_memcpy (x, p, 2);");
		output_storage ("	n += val;");
		output_storage ("	optim_memcpy (p, x, 2);");
#endif
		output_storage ("}");
		return;

	case COB_ADD_U24:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_add_u24 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	unsigned char	*x;");
		output_storage ("	unsigned int	n = 0;");

#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = ((unsigned char *)&n) + 1;");
#else
		output_storage ("	x = (unsigned char *)&n;");
#endif
		output_storage ("	optim_memcpy (x, p, 3);");
		output_storage ("	n += val;");
		output_storage ("	optim_memcpy (p, x, 3);");
		output_storage ("}");
		return;

	case COB_ADD_S24:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_add_s24 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	unsigned char	*x;");
		output_storage ("	int		n = 0;");

#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = (unsigned char *)&n;");
#else
		output_storage ("	x = ((unsigned char *)&n) + 1;");
#endif
		output_storage ("	optim_memcpy (x, p, 3);");
		output_storage ("	n >>= 8;	/* Shift with sign */");
		output_storage ("	n += val;");
#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = ((unsigned char *)&n) + 1;");
#else
		output_storage ("	x = (unsigned char *)&n;");
#endif
		output_storage ("	optim_memcpy (p, x, 3);");
		output_storage ("}");
		return;

	case COB_ADD_U32:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_add_u32 (void *p, const int val)");
		output_storage ("{");
#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	*(unsigned int __unaligned *)p += val;");
#else
		output_storage ("	void		*x;");
		output_storage ("	unsigned int	n;");

		output_storage ("	x = &n;");
		output_storage ("	optim_memcpy (x, p, 4);");
		output_storage ("	n += val;");
		output_storage ("	optim_memcpy (p, x, 4);");
#endif
		output_storage ("}");
		return;

	case COB_ADD_S32:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_add_s32 (void *p, const int val)");
		output_storage ("{");
#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	*(int __unaligned *)p += val;");
#else
		output_storage ("	void	*x;");
		output_storage ("	int	n;");

		output_storage ("	x = &n;");
		output_storage ("	optim_memcpy (x, p, 4);");
		output_storage ("	n += val;");
		output_storage ("	optim_memcpy (p, x, 4);");
#endif
		output_storage ("}");
		return;

	case COB_ADD_U40:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_add_u40 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_u64_t		n = 0;");
		output_storage ("	unsigned char		*x;");

#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = ((unsigned char *)&n) + 3;");
#else
		output_storage ("	x = (unsigned char *)&n;");
#endif
		output_storage ("	optim_memcpy (x, p, 5);");
		output_storage ("	n += val;");
		output_storage ("	optim_memcpy (p, x, 5);");
		output_storage ("}");
		return;

	case COB_ADD_S40:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_add_s40 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_s64_t		n = 0;");
		output_storage ("	unsigned char		*x;");

#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = (unsigned char *)&n;");
#else
		output_storage ("	x = ((unsigned char *)&n) + 3;");
#endif
		output_storage ("	optim_memcpy (x, p, 5);");
		output_storage ("	n >>= 24;	/* Shift with sign */");
		output_storage ("	n += val;");
#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = ((unsigned char *)&n) + 3;");
#else
		output_storage ("	x = (unsigned char *)&n;");
#endif
		output_storage ("	optim_memcpy (p, x, 5);");
		output_storage ("}");
		return;

	case COB_ADD_U48:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_add_u48 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_u64_t		n = 0;");
		output_storage ("	unsigned char		*x;");

#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = ((unsigned char *)&n) + 2;");
#else
		output_storage ("	x = (unsigned char *)&n;");
#endif
		output_storage ("	optim_memcpy (x, p, 6);");
		output_storage ("	n += val;");
		output_storage ("	optim_memcpy (p, x, 6);");
		output_storage ("}");
		return;

	case COB_ADD_S48:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_add_s48 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_s64_t		n = 0;");
		output_storage ("	unsigned char		*x;");

#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = (unsigned char *)&n;");
#else
		output_storage ("	x = ((unsigned char *)&n) + 2;");
#endif
		output_storage ("	optim_memcpy (x, p, 6);");
		output_storage ("	n >>= 16;	/* Shift with sign */");
		output_storage ("	n += val;");
#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = ((unsigned char *)&n) + 2;");
#else
		output_storage ("	x = (unsigned char *)&n;");
#endif
		output_storage ("	optim_memcpy (p, x, 6);");
		output_storage ("}");
		return;

	case COB_ADD_U56:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_add_u56 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_u64_t		n = 0;");
		output_storage ("	unsigned char		*x;");

#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = ((unsigned char *)&n) + 1;");
#else
		output_storage ("	x = (unsigned char *)&n;");
#endif
		output_storage ("	optim_memcpy (x, p, 7);");
		output_storage ("	n += val;");
		output_storage ("	optim_memcpy (p, x, 7);");
		output_storage ("}");
		return;

	case COB_ADD_S56:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_add_s56 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_s64_t		n = 0;");
		output_storage ("	unsigned char		*x;");

#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = (unsigned char *)&n;");
#else
		output_storage ("	x = ((unsigned char *)&n) + 1;");
#endif
		output_storage ("	optim_memcpy (x, p, 7);");
		output_storage ("	n >>= 8;	/* Shift with sign */");
		output_storage ("	n += val;");
#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = ((unsigned char *)&n) + 1;");
#else
		output_storage ("	x = (unsigned char *)&n;");
#endif
		output_storage ("	optim_memcpy (p, x, 7);");
		output_storage ("}");
		return;

	case COB_ADD_U64:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_add_u64 (void *p, const int val)");
		output_storage ("{");
#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	*(cob_u64_t __unaligned *)p += val;");
#else
		output_storage ("	void		*x;");
		output_storage ("	cob_u64_t	n;");

		output_storage ("	x = &n;");
		output_storage ("	optim_memcpy (x, p, 8);");
		output_storage ("	n += val;");
		output_storage ("	optim_memcpy (p, x, 8);");
#endif
		output_storage ("}");
		return;

	case COB_ADD_S64:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_add_s64 (void *p, const int val)");
		output_storage ("{");
#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	*(cob_s64_t __unaligned *)p += val;");
#else
		output_storage ("	void		*x;");
		output_storage ("	cob_s64_t	n;");

		output_storage ("	x = &n;");
		output_storage ("	optim_memcpy (x, p, 8);");
		output_storage ("	n += val;");
		output_storage ("	optim_memcpy (p, x, 8);");
#endif
		output_storage ("}");
		return;

	case COB_SUB_U8:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_sub_u8 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	*(unsigned char *)p -= val;");
		output_storage ("}");
		return;

	case COB_SUB_S8:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_sub_s8 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	*(signed char *)p -= val;");
		output_storage ("}");
		return;

	case COB_SUB_U16:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_sub_u16 (void *p, const int val)");
		output_storage ("{");
#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	*(unsigned short __unaligned *)p -= val;");
#else
		output_storage ("	void		*x;");
		output_storage ("	unsigned short	n;");

		output_storage ("	x = &n;");
		output_storage ("	optim_memcpy (x, p, 2);");
		output_storage ("	n -= val;");
		output_storage ("	optim_memcpy (p, x, 2);");
#endif
		output_storage ("}");
		return;

	case COB_SUB_S16:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_sub_s16 (void *p, const int val)");
		output_storage ("{");
#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	*(short __unaligned *)p -= val;");
#else
		output_storage ("	void	*x;");
		output_storage ("	short	n;");

		output_storage ("	x = &n;");
		output_storage ("	optim_memcpy (x, p, 2);");
		output_storage ("	n -= val;");
		output_storage ("	optim_memcpy (p, x, 2);");
#endif
		output_storage ("}");
		return;

	case COB_SUB_U24:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_sub_u24 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	unsigned char	*x;");
		output_storage ("	unsigned int	n = 0;");

#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = ((unsigned char *)&n) + 1;");
#else
		output_storage ("	x = (unsigned char *)&n;");
#endif
		output_storage ("	optim_memcpy (x, p, 3);");
		output_storage ("	n -= val;");
		output_storage ("	optim_memcpy (p, x, 3);");
		output_storage ("}");
		return;

	case COB_SUB_S24:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_sub_s24 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	unsigned char	*x;");
		output_storage ("	int		n = 0;");

#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = (unsigned char *)&n;");
#else
		output_storage ("	x = ((unsigned char *)&n) + 1;");
#endif
		output_storage ("	optim_memcpy (x, p, 3);");
		output_storage ("	n >>= 8;	/* Shift with sign */");
		output_storage ("	n -= val;");
#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = ((unsigned char *)&n) + 1;");
#else
		output_storage ("	x = (unsigned char *)&n;");
#endif
		output_storage ("	optim_memcpy (p, x, 3);");
		output_storage ("}");
		return;

	case COB_SUB_U32:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_sub_u32 (void *p, const int val)");
		output_storage ("{");
#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	*(unsigned int __unaligned *)p -= val;");
#else
		output_storage ("	void		*x;");
		output_storage ("	unsigned int	n;");

		output_storage ("	x = &n;");
		output_storage ("	optim_memcpy (x, p, 4);");
		output_storage ("	n -= val;");
		output_storage ("	optim_memcpy (p, x, 4);");
#endif
		output_storage ("}");
		return;

	case COB_SUB_S32:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_sub_s32 (void *p, const int val)");
		output_storage ("{");
#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	*(int __unaligned *)p -= val;");
#else
		output_storage ("	void	*x;");
		output_storage ("	int	n;");

		output_storage ("	x = &n;");
		output_storage ("	optim_memcpy (x, p, 4);");
		output_storage ("	n -= val;");
		output_storage ("	optim_memcpy (p, x, 4);");
#endif
		output_storage ("}");
		return;

	case COB_SUB_U40:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_sub_u40 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_u64_t		n = 0;");
		output_storage ("	unsigned char		*x;");

#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = ((unsigned char *)&n) + 3;");
#else
		output_storage ("	x = (unsigned char *)&n;");
#endif
		output_storage ("	optim_memcpy (x, p, 5);");
		output_storage ("	n -= val;");
		output_storage ("	optim_memcpy (p, x, 5);");
		output_storage ("}");
		return;

	case COB_SUB_S40:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_sub_s40 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_s64_t		n = 0;");
		output_storage ("	unsigned char		*x;");

#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = (unsigned char *)&n;");
#else
		output_storage ("	x = ((unsigned char *)&n) + 3;");
#endif
		output_storage ("	optim_memcpy (x, p, 5);");
		output_storage ("	n >>= 24;	/* Shift with sign */");
		output_storage ("	n -= val;");
#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = ((unsigned char *)&n) + 3;");
#else
		output_storage ("	x = (unsigned char *)&n;");
#endif
		output_storage ("	optim_memcpy (p, x, 5);");
		output_storage ("}");
		return;

	case COB_SUB_U48:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_sub_u48 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_u64_t		n = 0;");
		output_storage ("	unsigned char		*x;");

#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = ((unsigned char *)&n) + 2;");
#else
		output_storage ("	x = (unsigned char *)&n;");
#endif
		output_storage ("	optim_memcpy (x, p, 6);");
		output_storage ("	n -= val;");
		output_storage ("	optim_memcpy (p, x, 6);");
		output_storage ("}");
		return;

	case COB_SUB_S48:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_sub_s48 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_s64_t		n = 0;");
		output_storage ("	unsigned char		*x;");

#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = (unsigned char *)&n;");
#else
		output_storage ("	x = ((unsigned char *)&n) + 2;");
#endif
		output_storage ("	optim_memcpy (x, p, 6);");
		output_storage ("	n >>= 16;	/* Shift with sign */");
		output_storage ("	n -= val;");
#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = ((unsigned char *)&n) + 2;");
#else
		output_storage ("	x = (unsigned char *)&n;");
#endif
		output_storage ("	optim_memcpy (p, x, 6);");
		output_storage ("}");
		return;

	case COB_SUB_U56:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_sub_u56 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_u64_t		n = 0;");
		output_storage ("	unsigned char		*x;");

#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = ((unsigned char *)&n) + 1;");
#else
		output_storage ("	x = (unsigned char *)&n;");
#endif
		output_storage ("	optim_memcpy (x, p, 7);");
		output_storage ("	n -= val;");
		output_storage ("	optim_memcpy (p, x, 7);");
		output_storage ("}");
		return;

	case COB_SUB_S56:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_sub_s56 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_s64_t		n = 0;");
		output_storage ("	unsigned char		*x;");

#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = (unsigned char *)&n;");
#else
		output_storage ("	x = ((unsigned char *)&n) + 1;");
#endif
		output_storage ("	optim_memcpy (x, p, 7);");
		output_storage ("	n >>= 8;	/* Shift with sign */");
		output_storage ("	n -= val;");
#ifdef	WORDS_BIGENDIAN
		output_storage ("	x = ((unsigned char *)&n) + 1;");
#else
		output_storage ("	x = (unsigned char *)&n;");
#endif
		output_storage ("	optim_memcpy (p, x, 7);");
		output_storage ("}");
		return;

	case COB_SUB_U64:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_sub_u64 (void *p, const int val)");
		output_storage ("{");
#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	*(cob_u64_t __unaligned *)p -= val;");
#else
		output_storage ("	void		*x;");
		output_storage ("	cob_u64_t	n;");

		output_storage ("	x = &n;");
		output_storage ("	optim_memcpy (x, p, 8);");
		output_storage ("	n -= val;");
		output_storage ("	optim_memcpy (p, x, 8);");
#endif
		output_storage ("}");
		return;

	case COB_SUB_S64:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_sub_s64 (void *p, const int val)");
		output_storage ("{");
#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	*(cob_s64_t __unaligned *)p -= val;");
#else
		output_storage ("	void		*x;");
		output_storage ("	cob_s64_t	n;");

		output_storage ("	x = &n;");
		output_storage ("	optim_memcpy (x, p, 8);");
		output_storage ("	n -= val;");
		output_storage ("	optim_memcpy (p, x, 8);");
#endif
		output_storage ("}");
		return;

	/* Binary swapped compare */

	case COB_CMPSWP_U16:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmpswp_u16 (const void *p, const cob_s64_t n)");
		output_storage ("{");
#ifndef COB_ALLOW_UNALIGNED
		output_storage ("	void		*x;");
#endif
		output_storage ("	unsigned short	val;");

		output_storage ("	if (unlikely(n < 0)) {");
		output_storage ("		return 1;");
		output_storage ("	}");
#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	val = COB_BSWAP_16 (*(unsigned short __unaligned *)p);");
#else
		output_storage ("	x = &val;");
		output_storage ("	optim_memcpy (x, p, 2);");
		output_storage ("	val = COB_BSWAP_16 (val);");
#endif
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMPSWP_S16:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmpswp_s16 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	short	val;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	val = COB_BSWAP_16 (*(short __unaligned *)p);");
#else
		output_storage ("	void	*x;");

		output_storage ("	x = &val;");
		output_storage ("	optim_memcpy (x, p, 2);");
		output_storage ("	val = COB_BSWAP_16 (val);");
#endif
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMPSWP_U24:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmpswp_u24 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	unsigned char	*x;");
		output_storage ("	unsigned int	val = 0;");

		output_storage ("	if (unlikely(n < 0)) {");
		output_storage ("		return 1;");
		output_storage ("	}");
		output_storage ("	x = ((unsigned char *)&val) + 1;");
		output_storage ("	optim_memcpy (x, p, 3);");
		output_storage ("	val = COB_BSWAP_32 (val);");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMPSWP_S24:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmpswp_s24 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	unsigned char	*x;");
		output_storage ("	int		val = 0;");

		output_storage ("	x = (unsigned char *)&val;");
		output_storage ("	optim_memcpy (x, p, 3);");
		output_storage ("	val = COB_BSWAP_32 (val);");
		output_storage ("	val >>= 8;	/* Shift with sign */");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMPSWP_U32:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmpswp_u32 (const void *p, const cob_s64_t n)");
		output_storage ("{");
#ifndef COB_ALLOW_UNALIGNED
		output_storage ("	void		*x;");
#endif
		output_storage ("	unsigned int	val;");

		output_storage ("	if (unlikely(n < 0)) {");
		output_storage ("		return 1;");
		output_storage ("	}");
#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	val = COB_BSWAP_32 (*(const unsigned int __unaligned *)p);");
#else
		output_storage ("	x = &val;");
		output_storage ("	optim_memcpy (x, p, 4);");
		output_storage ("	val = COB_BSWAP_32 (val);");
#endif
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMPSWP_S32:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmpswp_s32 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	int	val;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	val = COB_BSWAP_32 (*(const int __unaligned *)p);");
#else
		output_storage ("	void	*x;");

		output_storage ("	x = &val;");
		output_storage ("	optim_memcpy (x, p, 4);");
		output_storage ("	val = COB_BSWAP_32 (val);");
#endif
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMPSWP_U40:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmpswp_u40 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	cob_u64_t	val = 0;");
		output_storage ("	unsigned char	*x;");

		output_storage ("	if (unlikely(n < 0)) {");
		output_storage ("		return 1;");
		output_storage ("	}");
		output_storage ("	x = ((unsigned char *)&val) + 3;");
		output_storage ("	optim_memcpy (x, p, 5);");
		output_storage ("	val = COB_BSWAP_64 (val);");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMPSWP_S40:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmpswp_s40 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	cob_s64_t	val = 0;");
		output_storage ("	unsigned char	*x;");

		output_storage ("	x = (unsigned char *)&val;");
		output_storage ("	optim_memcpy (x, p, 5);");
		output_storage ("	val = COB_BSWAP_64 (val);");
		output_storage ("	val >>= 24;	/* Shift with sign */");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMPSWP_U48:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmpswp_u48 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	cob_u64_t	val = 0;");
		output_storage ("	unsigned char	*x;");

		output_storage ("	if (unlikely(n < 0)) {");
		output_storage ("		return 1;");
		output_storage ("	}");
		output_storage ("	x = ((unsigned char *)&val) + 2;");
		output_storage ("	optim_memcpy (x, p, 6);");
		output_storage ("	val = COB_BSWAP_64 (val);");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMPSWP_S48:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmpswp_s48 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	cob_s64_t	val = 0;");
		output_storage ("	unsigned char	*x;");

		output_storage ("	x = (unsigned char *)&val;");
		output_storage ("	optim_memcpy (x, p, 6);");
		output_storage ("	val = COB_BSWAP_64 (val);");
		output_storage ("	val >>= 16;	/* Shift with sign */");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMPSWP_U56:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmpswp_u56 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	cob_u64_t	val = 0;");
		output_storage ("	unsigned char	*x;");

		output_storage ("	if (unlikely(n < 0)) {");
		output_storage ("		return 1;");
		output_storage ("	}");
		output_storage ("	x = ((unsigned char *)&val) + 1;");
		output_storage ("	optim_memcpy (x, p, 7);");
		output_storage ("	val = COB_BSWAP_64 (val);");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMPSWP_S56:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmpswp_s56 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	cob_s64_t	val = 0;");
		output_storage ("	unsigned char	*x;");

		output_storage ("	x = (unsigned char *)&val;");
		output_storage ("	optim_memcpy (x, p, 7);");
		output_storage ("	val = COB_BSWAP_64 (val);");
		output_storage ("	val >>= 8;	/* Shift with sign */");
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMPSWP_U64:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmpswp_u64 (const void *p, const cob_s64_t n)");
		output_storage ("{");
#ifndef COB_ALLOW_UNALIGNED
		output_storage ("	void		*x;");
#endif
		output_storage ("	cob_u64_t	val;");

		output_storage ("	if (unlikely(n < 0)) {");
		output_storage ("		return 1;");
		output_storage ("	}");
#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	val = COB_BSWAP_64 (*(const cob_u64_t __unaligned *)p);");
#else
		output_storage ("	x = &val;");
		output_storage ("	optim_memcpy (x, p, 8);");
		output_storage ("	val = COB_BSWAP_64 (val);");
#endif
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	case COB_CMPSWP_S64:
		output_storage ("static COB_INLINE COB_A_INLINE int");
		output_storage ("cob_cmpswp_s64 (const void *p, const cob_s64_t n)");
		output_storage ("{");
		output_storage ("	cob_s64_t	val;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	val = COB_BSWAP_64 (*(const cob_s64_t __unaligned *)p);");
#else
		output_storage ("	void		*x;");
		output_storage ("	x = &val;");
		output_storage ("	optim_memcpy (x, p, 8);");
		output_storage ("	val = COB_BSWAP_64 (val);");
#endif
		output_storage ("	return (val < n) ? -1 : (val > n);");
		output_storage ("}");
		return;

	/* Binary swapped add */

	case COB_ADDSWP_U16:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_addswp_u16 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	unsigned short	n;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	n = COB_BSWAP_16 (*(unsigned short __unaligned *)p);");
		output_storage ("	n += val;");
		output_storage ("	*(unsigned short __unaligned *)p = COB_BSWAP_16(n);");
#else
		output_storage ("	unsigned char	*x;");
		output_storage ("	unsigned char	*px = p;");

		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	x[0] = px[1];");
		output_storage ("	x[1] = px[0];");
		output_storage ("	n += val;");
		output_storage ("	px[0] = x[1];");
		output_storage ("	px[1] = x[0];");
#endif
		output_storage ("}");
		return;

	case COB_ADDSWP_S16:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_addswp_s16 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	short		n;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	n = COB_BSWAP_16 (*(short __unaligned *)p);");
		output_storage ("	n += val;");
		output_storage ("	*(short __unaligned *)p = COB_BSWAP_16(n);");
#else
		output_storage ("	unsigned char	*x;");
		output_storage ("	unsigned char	*px = p;");

		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	x[0] = px[1];");
		output_storage ("	x[1] = px[0];");
		output_storage ("	n += val;");
		output_storage ("	px[0] = x[1];");
		output_storage ("	px[1] = x[0];");
#endif
		output_storage ("}");
		return;

	case COB_ADDSWP_U24:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_addswp_u24 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	unsigned char	*x;");
		output_storage ("	unsigned char	*px = p;");
		output_storage ("	unsigned int	n = 0;");

		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	x[0] = px[2];");
		output_storage ("	x[1] = px[1];");
		output_storage ("	x[2] = px[0];");
		output_storage ("	n += val;");
		output_storage ("	px[0] = x[2];");
		output_storage ("	px[1] = x[1];");
		output_storage ("	px[2] = x[0];");
		output_storage ("}");
		return;

	case COB_ADDSWP_S24:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_addswp_s24 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	unsigned char	*x;");
		output_storage ("	unsigned char	*px = p;");
		output_storage ("	int		n = 0;");

		output_storage ("	x = ((unsigned char *)&n) + 1;");
		output_storage ("	x[0] = px[2];");
		output_storage ("	x[1] = px[1];");
		output_storage ("	x[2] = px[0];");
		output_storage ("	n >>= 8;	/* Shift with sign */");
		output_storage ("	n += val;");
		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	px[0] = x[2];");
		output_storage ("	px[1] = x[1];");
		output_storage ("	px[2] = x[0];");
		output_storage ("}");
		return;

	case COB_ADDSWP_U32:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_addswp_u32 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	unsigned int	n;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	n = COB_BSWAP_32 (*(unsigned int __unaligned *)p);");
		output_storage ("	n += val;");
		output_storage ("	*(unsigned int __unaligned *)p = COB_BSWAP_32(n);");
#else
		output_storage ("	unsigned char	*x;");
		output_storage ("	unsigned char	*px = p;");

		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	x[0] = px[3];");
		output_storage ("	x[1] = px[2];");
		output_storage ("	x[2] = px[1];");
		output_storage ("	x[3] = px[0];");
		output_storage ("	n += val;");
		output_storage ("	px[0] = x[3];");
		output_storage ("	px[1] = x[2];");
		output_storage ("	px[2] = x[1];");
		output_storage ("	px[3] = x[0];");
#endif
		output_storage ("}");
		return;

	case COB_ADDSWP_S32:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_addswp_s32 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	int		n;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	n = COB_BSWAP_32 (*(int __unaligned *)p);");
		output_storage ("	n += val;");
		output_storage ("	*(int __unaligned *)p = COB_BSWAP_32(n);");
#else
		output_storage ("	unsigned char	*x;");
		output_storage ("	unsigned char	*px = p;");

		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	x[0] = px[3];");
		output_storage ("	x[1] = px[2];");
		output_storage ("	x[2] = px[1];");
		output_storage ("	x[3] = px[0];");
		output_storage ("	n += val;");
		output_storage ("	px[0] = x[3];");
		output_storage ("	px[1] = x[2];");
		output_storage ("	px[2] = x[1];");
		output_storage ("	px[3] = x[0];");
#endif
		output_storage ("}");
		return;

	case COB_ADDSWP_U40:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_addswp_u40 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_u64_t		n = 0;");
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	x[0] = px[4];");
		output_storage ("	x[1] = px[3];");
		output_storage ("	x[2] = px[2];");
		output_storage ("	x[3] = px[1];");
		output_storage ("	x[4] = px[0];");
		output_storage ("	n += val;");
		output_storage ("	px[0] = x[4];");
		output_storage ("	px[1] = x[3];");
		output_storage ("	px[2] = x[2];");
		output_storage ("	px[3] = x[1];");
		output_storage ("	px[4] = x[0];");
		output_storage ("}");
		return;

	case COB_ADDSWP_S40:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_addswp_s40 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_s64_t		n = 0;");
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	x = ((unsigned char *)&n) + 3;");
		output_storage ("	x[0] = px[4];");
		output_storage ("	x[1] = px[3];");
		output_storage ("	x[2] = px[2];");
		output_storage ("	x[3] = px[1];");
		output_storage ("	x[4] = px[0];");
		output_storage ("	n >>= 24;	/* Shift with sign */");
		output_storage ("	n += val;");
		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	px[0] = x[4];");
		output_storage ("	px[1] = x[3];");
		output_storage ("	px[2] = x[2];");
		output_storage ("	px[3] = x[1];");
		output_storage ("	px[4] = x[0];");
		output_storage ("}");
		return;

	case COB_ADDSWP_U48:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_addswp_u48 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_u64_t		n = 0;");
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	x[0] = px[5];");
		output_storage ("	x[1] = px[4];");
		output_storage ("	x[2] = px[3];");
		output_storage ("	x[3] = px[2];");
		output_storage ("	x[4] = px[1];");
		output_storage ("	x[5] = px[0];");
		output_storage ("	n += val;");
		output_storage ("	px[0] = x[5];");
		output_storage ("	px[1] = x[4];");
		output_storage ("	px[2] = x[3];");
		output_storage ("	px[3] = x[2];");
		output_storage ("	px[4] = x[1];");
		output_storage ("	px[5] = x[0];");
		output_storage ("}");
		return;

	case COB_ADDSWP_S48:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_addswp_s48 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_s64_t		n = 0;");
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	x = ((unsigned char *)&n) + 2;");
		output_storage ("	x[0] = px[5];");
		output_storage ("	x[1] = px[4];");
		output_storage ("	x[2] = px[3];");
		output_storage ("	x[3] = px[2];");
		output_storage ("	x[4] = px[1];");
		output_storage ("	x[5] = px[0];");
		output_storage ("	n >>= 16;	/* Shift with sign */");
		output_storage ("	n += val;");
		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	px[0] = x[5];");
		output_storage ("	px[1] = x[4];");
		output_storage ("	px[2] = x[3];");
		output_storage ("	px[3] = x[2];");
		output_storage ("	px[4] = x[1];");
		output_storage ("	px[5] = x[0];");
		output_storage ("}");
		return;

	case COB_ADDSWP_U56:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_addswp_u56 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_u64_t		n = 0;");
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	x[0] = px[6];");
		output_storage ("	x[1] = px[5];");
		output_storage ("	x[2] = px[4];");
		output_storage ("	x[3] = px[3];");
		output_storage ("	x[4] = px[2];");
		output_storage ("	x[5] = px[1];");
		output_storage ("	x[6] = px[0];");
		output_storage ("	n += val;");
		output_storage ("	px[0] = x[6];");
		output_storage ("	px[1] = x[5];");
		output_storage ("	px[2] = x[4];");
		output_storage ("	px[3] = x[3];");
		output_storage ("	px[4] = x[2];");
		output_storage ("	px[5] = x[1];");
		output_storage ("	px[6] = x[0];");
		output_storage ("}");
		return;

	case COB_ADDSWP_S56:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_addswp_s56 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_s64_t		n = 0;");
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	x = ((unsigned char *)&n) + 1;");
		output_storage ("	x[0] = px[6];");
		output_storage ("	x[1] = px[5];");
		output_storage ("	x[2] = px[4];");
		output_storage ("	x[3] = px[3];");
		output_storage ("	x[4] = px[2];");
		output_storage ("	x[5] = px[1];");
		output_storage ("	x[6] = px[0];");
		output_storage ("	n >>= 8;	/* Shift with sign */");
		output_storage ("	n += val;");
		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	px[0] = x[6];");
		output_storage ("	px[1] = x[5];");
		output_storage ("	px[2] = x[4];");
		output_storage ("	px[3] = x[3];");
		output_storage ("	px[4] = x[2];");
		output_storage ("	px[5] = x[1];");
		output_storage ("	px[6] = x[0];");
		output_storage ("}");
		return;

	case COB_ADDSWP_U64:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_addswp_u64 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_u64_t		n;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	n = COB_BSWAP_64 (*(cob_u64_t __unaligned *)p);");
		output_storage ("	n += val;");
		output_storage ("	*(cob_u64_t __unaligned *)p = COB_BSWAP_64(n);");
#else
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	x[0] = px[7];");
		output_storage ("	x[1] = px[6];");
		output_storage ("	x[2] = px[5];");
		output_storage ("	x[3] = px[4];");
		output_storage ("	x[4] = px[3];");
		output_storage ("	x[5] = px[2];");
		output_storage ("	x[6] = px[1];");
		output_storage ("	x[7] = px[0];");
		output_storage ("	n += val;");
		output_storage ("	px[0] = x[7];");
		output_storage ("	px[1] = x[6];");
		output_storage ("	px[2] = x[5];");
		output_storage ("	px[3] = x[4];");
		output_storage ("	px[4] = x[3];");
		output_storage ("	px[5] = x[2];");
		output_storage ("	px[6] = x[1];");
		output_storage ("	px[7] = x[0];");
#endif
		output_storage ("}");
		return;

	case COB_ADDSWP_S64:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_addswp_s64 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_s64_t		n;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	n = COB_BSWAP_64 (*(cob_s64_t __unaligned *)p);");
		output_storage ("	n += val;");
		output_storage ("	*(cob_s64_t __unaligned *)p = COB_BSWAP_64(n);");
#else
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	x[0] = px[7];");
		output_storage ("	x[1] = px[6];");
		output_storage ("	x[2] = px[5];");
		output_storage ("	x[3] = px[4];");
		output_storage ("	x[4] = px[3];");
		output_storage ("	x[5] = px[2];");
		output_storage ("	x[6] = px[1];");
		output_storage ("	x[7] = px[0];");
		output_storage ("	n += val;");
		output_storage ("	px[0] = x[7];");
		output_storage ("	px[1] = x[6];");
		output_storage ("	px[2] = x[5];");
		output_storage ("	px[3] = x[4];");
		output_storage ("	px[4] = x[3];");
		output_storage ("	px[5] = x[2];");
		output_storage ("	px[6] = x[1];");
		output_storage ("	px[7] = x[0];");
#endif
		output_storage ("}");
		return;

	/* Binary swapped subtract */

	case COB_SUBSWP_U16:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_subswp_u16 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	unsigned short	n;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	n = COB_BSWAP_16 (*(unsigned short __unaligned *)p);");
		output_storage ("	n -= val;");
		output_storage ("	*(unsigned short __unaligned *)p = COB_BSWAP_16(n);");
#else
		output_storage ("	unsigned char	*x;");
		output_storage ("	unsigned char	*px = p;");

		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	x[0] = px[1];");
		output_storage ("	x[1] = px[0];");
		output_storage ("	n -= val;");
		output_storage ("	px[0] = x[1];");
		output_storage ("	px[1] = x[0];");
#endif
		output_storage ("}");
		return;

	case COB_SUBSWP_S16:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_subswp_s16 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	short		n;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	n = COB_BSWAP_16 (*(short __unaligned *)p);");
		output_storage ("	n -= val;");
		output_storage ("	*(short __unaligned *)p = COB_BSWAP_16(n);");
#else
		output_storage ("	unsigned char	*x;");
		output_storage ("	unsigned char	*px = p;");

		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	x[0] = px[1];");
		output_storage ("	x[1] = px[0];");
		output_storage ("	n -= val;");
		output_storage ("	px[0] = x[1];");
		output_storage ("	px[1] = x[0];");
#endif
		output_storage ("}");
		return;

	case COB_SUBSWP_U24:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_subswp_u24 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	unsigned char	*x;");
		output_storage ("	unsigned char	*px = p;");
		output_storage ("	unsigned int	n = 0;");

		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	x[0] = px[2];");
		output_storage ("	x[1] = px[1];");
		output_storage ("	x[2] = px[0];");
		output_storage ("	n -= val;");
		output_storage ("	px[0] = x[2];");
		output_storage ("	px[1] = x[1];");
		output_storage ("	px[2] = x[0];");
		output_storage ("}");
		return;

	case COB_SUBSWP_S24:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_subswp_s24 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	unsigned char	*x;");
		output_storage ("	unsigned char	*px = p;");
		output_storage ("	int		n = 0;");

		output_storage ("	x = ((unsigned char *)&n) + 1;");
		output_storage ("	x[0] = px[2];");
		output_storage ("	x[1] = px[1];");
		output_storage ("	x[2] = px[0];");
		output_storage ("	n >>= 8;	/* Shift with sign */");
		output_storage ("	n -= val;");
		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	px[0] = x[2];");
		output_storage ("	px[1] = x[1];");
		output_storage ("	px[2] = x[0];");
		output_storage ("}");
		return;

	case COB_SUBSWP_U32:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_subswp_u32 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	unsigned int	n;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	n = COB_BSWAP_32 (*(unsigned int __unaligned *)p);");
		output_storage ("	n -= val;");
		output_storage ("	*(unsigned int __unaligned *)p = COB_BSWAP_32(n);");
#else
		output_storage ("	unsigned char	*x;");
		output_storage ("	unsigned char	*px = p;");

		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	x[0] = px[3];");
		output_storage ("	x[1] = px[2];");
		output_storage ("	x[2] = px[1];");
		output_storage ("	x[3] = px[0];");
		output_storage ("	n -= val;");
		output_storage ("	px[0] = x[3];");
		output_storage ("	px[1] = x[2];");
		output_storage ("	px[2] = x[1];");
		output_storage ("	px[3] = x[0];");
#endif
		output_storage ("}");
		return;

	case COB_SUBSWP_S32:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_subswp_s32 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	int		n;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	n = COB_BSWAP_32 (*(int __unaligned *)p);");
		output_storage ("	n -= val;");
		output_storage ("	*(int __unaligned *)p = COB_BSWAP_32(n);");
#else
		output_storage ("	unsigned char	*x;");
		output_storage ("	unsigned char	*px = p;");

		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	x[0] = px[3];");
		output_storage ("	x[1] = px[2];");
		output_storage ("	x[2] = px[1];");
		output_storage ("	x[3] = px[0];");
		output_storage ("	n -= val;");
		output_storage ("	px[0] = x[3];");
		output_storage ("	px[1] = x[2];");
		output_storage ("	px[2] = x[1];");
		output_storage ("	px[3] = x[0];");
#endif
		output_storage ("}");
		return;

	case COB_SUBSWP_U40:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_subswp_u40 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_u64_t		n = 0;");
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	x[0] = px[4];");
		output_storage ("	x[1] = px[3];");
		output_storage ("	x[2] = px[2];");
		output_storage ("	x[3] = px[1];");
		output_storage ("	x[4] = px[0];");
		output_storage ("	n -= val;");
		output_storage ("	px[0] = x[4];");
		output_storage ("	px[1] = x[3];");
		output_storage ("	px[2] = x[2];");
		output_storage ("	px[3] = x[1];");
		output_storage ("	px[4] = x[0];");
		output_storage ("}");
		return;

	case COB_SUBSWP_S40:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_subswp_s40 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_s64_t		n = 0;");
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	x = ((unsigned char *)&n) + 3;");
		output_storage ("	x[0] = px[4];");
		output_storage ("	x[1] = px[3];");
		output_storage ("	x[2] = px[2];");
		output_storage ("	x[3] = px[1];");
		output_storage ("	x[4] = px[0];");
		output_storage ("	n >>= 24;	/* Shift with sign */");
		output_storage ("	n -= val;");
		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	px[0] = x[4];");
		output_storage ("	px[1] = x[3];");
		output_storage ("	px[2] = x[2];");
		output_storage ("	px[3] = x[1];");
		output_storage ("	px[4] = x[0];");
		output_storage ("}");
		return;

	case COB_SUBSWP_U48:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_subswp_u48 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_u64_t		n = 0;");
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	x[0] = px[5];");
		output_storage ("	x[1] = px[4];");
		output_storage ("	x[2] = px[3];");
		output_storage ("	x[3] = px[2];");
		output_storage ("	x[4] = px[1];");
		output_storage ("	x[5] = px[0];");
		output_storage ("	n -= val;");
		output_storage ("	px[0] = x[5];");
		output_storage ("	px[1] = x[4];");
		output_storage ("	px[2] = x[3];");
		output_storage ("	px[3] = x[2];");
		output_storage ("	px[4] = x[1];");
		output_storage ("	px[5] = x[0];");
		output_storage ("}");
		return;

	case COB_SUBSWP_S48:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_subswp_s48 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_s64_t		n = 0;");
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	x = ((unsigned char *)&n) + 2;");
		output_storage ("	x[0] = px[5];");
		output_storage ("	x[1] = px[4];");
		output_storage ("	x[2] = px[3];");
		output_storage ("	x[3] = px[2];");
		output_storage ("	x[4] = px[1];");
		output_storage ("	x[5] = px[0];");
		output_storage ("	n >>= 16;	/* Shift with sign */");
		output_storage ("	n -= val;");
		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	px[0] = x[5];");
		output_storage ("	px[1] = x[4];");
		output_storage ("	px[2] = x[3];");
		output_storage ("	px[3] = x[2];");
		output_storage ("	px[4] = x[1];");
		output_storage ("	px[5] = x[0];");
		output_storage ("}");
		return;

	case COB_SUBSWP_U56:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_subswp_u56 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_u64_t		n = 0;");
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	x[0] = px[6];");
		output_storage ("	x[1] = px[5];");
		output_storage ("	x[2] = px[4];");
		output_storage ("	x[3] = px[3];");
		output_storage ("	x[4] = px[2];");
		output_storage ("	x[5] = px[1];");
		output_storage ("	x[6] = px[0];");
		output_storage ("	n -= val;");
		output_storage ("	px[0] = x[6];");
		output_storage ("	px[1] = x[5];");
		output_storage ("	px[2] = x[4];");
		output_storage ("	px[3] = x[3];");
		output_storage ("	px[4] = x[2];");
		output_storage ("	px[5] = x[1];");
		output_storage ("	px[6] = x[0];");
		output_storage ("}");
		return;

	case COB_SUBSWP_S56:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_subswp_s56 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_s64_t		n = 0;");
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	x = ((unsigned char *)&n) + 1;");
		output_storage ("	x[0] = px[6];");
		output_storage ("	x[1] = px[5];");
		output_storage ("	x[2] = px[4];");
		output_storage ("	x[3] = px[3];");
		output_storage ("	x[4] = px[2];");
		output_storage ("	x[5] = px[1];");
		output_storage ("	x[6] = px[0];");
		output_storage ("	n >>= 8;	/* Shift with sign */");
		output_storage ("	n -= val;");
		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	px[0] = x[6];");
		output_storage ("	px[1] = x[5];");
		output_storage ("	px[2] = x[4];");
		output_storage ("	px[3] = x[3];");
		output_storage ("	px[4] = x[2];");
		output_storage ("	px[5] = x[1];");
		output_storage ("	px[6] = x[0];");
		output_storage ("}");
		return;

	case COB_SUBSWP_U64:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_subswp_u64 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_u64_t	n;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	n = COB_BSWAP_64 (*(cob_u64_t __unaligned *)p);");
		output_storage ("	n -= val;");
		output_storage ("	*(cob_u64_t __unaligned *)p = COB_BSWAP_64(n);");
#else
		output_storage ("	unsigned char	*x;");
		output_storage ("	unsigned char	*px = p;");

		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	x[0] = px[7];");
		output_storage ("	x[1] = px[6];");
		output_storage ("	x[2] = px[5];");
		output_storage ("	x[3] = px[4];");
		output_storage ("	x[4] = px[3];");
		output_storage ("	x[5] = px[2];");
		output_storage ("	x[6] = px[1];");
		output_storage ("	x[7] = px[0];");
		output_storage ("	n -= val;");
		output_storage ("	px[0] = x[7];");
		output_storage ("	px[1] = x[6];");
		output_storage ("	px[2] = x[5];");
		output_storage ("	px[3] = x[4];");
		output_storage ("	px[4] = x[3];");
		output_storage ("	px[5] = x[2];");
		output_storage ("	px[6] = x[1];");
		output_storage ("	px[7] = x[0];");
#endif
		output_storage ("}");
		return;

	case COB_SUBSWP_S64:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_subswp_s64 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_s64_t		n;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	n = COB_BSWAP_64 (*(cob_s64_t __unaligned *)p);");
		output_storage ("	n -= val;");
		output_storage ("	*(cob_s64_t __unaligned *)p = COB_BSWAP_64(n);");
#else
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	x[0] = px[7];");
		output_storage ("	x[1] = px[6];");
		output_storage ("	x[2] = px[5];");
		output_storage ("	x[3] = px[4];");
		output_storage ("	x[4] = px[3];");
		output_storage ("	x[5] = px[2];");
		output_storage ("	x[6] = px[1];");
		output_storage ("	x[7] = px[0];");
		output_storage ("	n -= val;");
		output_storage ("	px[0] = x[7];");
		output_storage ("	px[1] = x[6];");
		output_storage ("	px[2] = x[5];");
		output_storage ("	px[3] = x[4];");
		output_storage ("	px[4] = x[3];");
		output_storage ("	px[5] = x[2];");
		output_storage ("	px[6] = x[1];");
		output_storage ("	px[7] = x[0];");
#endif
		output_storage ("}");
		return;

	/* Binary set swapped value */
	case COB_SETSWP_U16:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_setswp_u16 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	unsigned short	n;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	n = val;");
		output_storage ("	*(unsigned short __unaligned *)p = COB_BSWAP_16(n);");
#else
		output_storage ("	unsigned char	*x;");
		output_storage ("	unsigned char	*px = p;");

		output_storage ("	n = val;");
		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	px[0] = x[1];");
		output_storage ("	px[1] = x[0];");
#endif
		output_storage ("}");
		return;

	case COB_SETSWP_S16:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_setswp_s16 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	short		n;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	n = val;");
		output_storage ("	*(short __unaligned *)p = COB_BSWAP_16(n);");
#else
		output_storage ("	unsigned char	*x;");
		output_storage ("	unsigned char	*px = p;");

		output_storage ("	n = val;");
		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	px[0] = x[1];");
		output_storage ("	px[1] = x[0];");
#endif
		output_storage ("}");
		return;

	case COB_SETSWP_U24:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_setswp_u24 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	unsigned char	*x;");
		output_storage ("	unsigned char	*px = p;");
		output_storage ("	unsigned int	n;");

		output_storage ("	n = val;");
		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	px[0] = x[2];");
		output_storage ("	px[1] = x[1];");
		output_storage ("	px[2] = x[0];");
		output_storage ("}");
		return;

	case COB_SETSWP_S24:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_setswp_s24 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	unsigned char	*x;");
		output_storage ("	unsigned char	*px = p;");
		output_storage ("	int		n;");

		output_storage ("	n = val;");
		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	px[0] = x[2];");
		output_storage ("	px[1] = x[1];");
		output_storage ("	px[2] = x[0];");
		output_storage ("}");
		return;

	case COB_SETSWP_U32:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_setswp_u32 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	unsigned int	n;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	n = val;");
		output_storage ("	*(unsigned int __unaligned *)p = COB_BSWAP_32(n);");
#else
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	n = val;");
		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	px[0] = x[3];");
		output_storage ("	px[1] = x[2];");
		output_storage ("	px[2] = x[1];");
		output_storage ("	px[3] = x[0];");
#endif
		output_storage ("}");
		return;

	case COB_SETSWP_S32:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_setswp_s32 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	int		n;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	n = val;");
		output_storage ("	*(int __unaligned *)p = COB_BSWAP_32(n);");
#else
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	n = val;");
		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	px[0] = x[3];");
		output_storage ("	px[1] = x[2];");
		output_storage ("	px[2] = x[1];");
		output_storage ("	px[3] = x[0];");
#endif
		output_storage ("}");
		return;

	case COB_SETSWP_U40:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_setswp_u40 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_u64_t		n;");
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	n = val;");
		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	px[0] = x[4];");
		output_storage ("	px[1] = x[3];");
		output_storage ("	px[2] = x[2];");
		output_storage ("	px[3] = x[1];");
		output_storage ("	px[4] = x[0];");
		output_storage ("}");
		return;

	case COB_SETSWP_S40:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_setswp_s40 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_s64_t		n;");
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	n = val;");
		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	px[0] = x[4];");
		output_storage ("	px[1] = x[3];");
		output_storage ("	px[2] = x[2];");
		output_storage ("	px[3] = x[1];");
		output_storage ("	px[4] = x[0];");
		output_storage ("}");
		return;

	case COB_SETSWP_U48:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_setswp_u48 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_u64_t		n;");
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	n = val;");
		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	px[0] = x[5];");
		output_storage ("	px[1] = x[4];");
		output_storage ("	px[2] = x[3];");
		output_storage ("	px[3] = x[2];");
		output_storage ("	px[4] = x[1];");
		output_storage ("	px[5] = x[0];");
		output_storage ("}");
		return;

	case COB_SETSWP_S48:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_setswp_s48 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_s64_t		n;");
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	n = val;");
		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	px[0] = x[5];");
		output_storage ("	px[1] = x[4];");
		output_storage ("	px[2] = x[3];");
		output_storage ("	px[3] = x[2];");
		output_storage ("	px[4] = x[1];");
		output_storage ("	px[5] = x[0];");
		output_storage ("}");
		return;

	case COB_SETSWP_U56:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_setswp_u56 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_u64_t		n;");
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	n = val;");
		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	px[0] = x[6];");
		output_storage ("	px[1] = x[5];");
		output_storage ("	px[2] = x[4];");
		output_storage ("	px[3] = x[3];");
		output_storage ("	px[4] = x[2];");
		output_storage ("	px[5] = x[1];");
		output_storage ("	px[6] = x[0];");
		output_storage ("}");
		return;

	case COB_SETSWP_S56:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_setswp_s56 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_s64_t		n;");
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	n = val;");
		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	px[0] = x[6];");
		output_storage ("	px[1] = x[5];");
		output_storage ("	px[2] = x[4];");
		output_storage ("	px[3] = x[3];");
		output_storage ("	px[4] = x[2];");
		output_storage ("	px[5] = x[1];");
		output_storage ("	px[6] = x[0];");
		output_storage ("}");
		return;

	case COB_SETSWP_U64:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_setswp_u64 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_u64_t		n;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	n = val;");
		output_storage ("	*(cob_u64_t __unaligned *)p = COB_BSWAP_64(n);");
#else
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	n = val;");
		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	px[0] = x[7];");
		output_storage ("	px[1] = x[6];");
		output_storage ("	px[2] = x[5];");
		output_storage ("	px[3] = x[4];");
		output_storage ("	px[4] = x[3];");
		output_storage ("	px[5] = x[2];");
		output_storage ("	px[6] = x[1];");
		output_storage ("	px[7] = x[0];");
#endif
		output_storage ("}");
		return;

	case COB_SETSWP_S64:
		output_storage ("static COB_INLINE COB_A_INLINE void");
		output_storage ("cob_setswp_s64 (void *p, const int val)");
		output_storage ("{");
		output_storage ("	cob_s64_t		n;");

#ifdef	COB_ALLOW_UNALIGNED
		output_storage ("	n = val;");
		output_storage ("	*(cob_s64_t __unaligned *)p = COB_BSWAP_64(n);");
#else
		output_storage ("	unsigned char		*x;");
		output_storage ("	unsigned char		*px = p;");

		output_storage ("	n = val;");
		output_storage ("	x = (unsigned char *)&n;");
		output_storage ("	px[0] = x[7];");
		output_storage ("	px[1] = x[6];");
		output_storage ("	px[2] = x[5];");
		output_storage ("	px[3] = x[4];");
		output_storage ("	px[4] = x[3];");
		output_storage ("	px[5] = x[2];");
		output_storage ("	px[6] = x[1];");
		output_storage ("	px[7] = x[0];");
#endif
		output_storage ("}");
		return;
	default:
		/* LCOV_EXCL_START */
		cobc_err_msg (_("unexpected optimization value: %d"), val);
		COBC_ABORT ();
		/* LCOV_EXCL_STOP */
	}
}
