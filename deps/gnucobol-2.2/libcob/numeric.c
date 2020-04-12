/*
   Copyright (C) 2001-2012, 2014-2017 Free Software Foundation, Inc.
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

#ifndef	_GNU_SOURCE
#define _GNU_SOURCE	1
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>

#include <math.h>
#ifdef HAVE_FINITE_IEEEFP_H
#include <ieeefp.h>
#endif

/* Force symbol exports */
#define	COB_LIB_EXPIMP

#include "libcob.h"
#include "coblocal.h"

#define DECIMAL_CHECK(d1,d2) \
	if (unlikely (d1->scale == COB_DECIMAL_NAN || \
	    d2->scale == COB_DECIMAL_NAN)) { \
		d1->scale = COB_DECIMAL_NAN; \
		return; \
	}

/* Local variables */

static cob_global	*cobglobptr;

static const unsigned char packed_bytes[] = {
	0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09,
	0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19,
	0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28, 0x29,
	0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39,
	0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49,
	0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59,
	0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69,
	0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79,
	0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89,
	0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99
};

#if	0	/* RXWRXW - IEEE 754 */
static const unsigned char	bits8[] = {
	8, 7, 6, 6, 5, 5, 5, 5,
	4, 4, 4, 4, 4, 4, 4, 4,
	3, 3, 3, 3, 3, 3, 3, 3,
	3, 3, 3, 3, 3, 3, 3, 3,
	2, 2, 2, 2, 2, 2, 2, 2,
	2, 2, 2, 2, 2, 2, 2, 2,
	2, 2, 2, 2, 2, 2, 2, 2,
	2, 2, 2, 2, 2, 2, 2, 2,
	1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0
};
#endif

static cob_decimal	cob_d1;
static cob_decimal	cob_d2;
static cob_decimal	cob_d3;
static cob_decimal	cob_d_remainder;

static cob_decimal	*cob_decimal_base;

static mpz_t		cob_mexp;
static mpz_t		cob_mpzt;
static mpz_t		cob_mpzt2;
static mpz_t		cob_mpz_ten34m1;
static mpz_t		cob_mpz_ten16m1;
static mpz_t		cob_mpze10[COB_MAX_BINARY];

static mpf_t		cob_mpft;
static mpf_t		cob_mpft_get;

static unsigned char	packed_value[20];
static cob_u64_t	last_packed_val;


#ifdef	COB_EXPERIMENTAL

#if GMP_NAIL_BITS != 0
#error NAILS not supported
#endif

#define COB_MAX_LL	COB_S64_C(9223372036854775807)

static void
mpz_set_ull (mpz_ptr dest, const cob_u64_t val)
{
	size_t			size;

	size = (val != 0);
	dest->_mp_d[0] = val & GMP_NUMB_MASK;
#if	GMP_LIMB_BITS < 64
	if (val > GMP_NUMB_MAX) {
		dest->_mp_d[1] = val >> GMP_NUMB_BITS;
		size = 2;
	}
#endif
	dest->_mp_size = size;
}

static void
mpz_set_sll (mpz_ptr dest, const cob_s64_t val)
{
	cob_u64_t		vtmp;
	size_t			size;

	vtmp = (cob_u64_t)(val >= 0 ? (cob_u64_t)val : -(cob_u64_t)val);
	size = (vtmp != 0);
	dest->_mp_d[0] = vtmp & GMP_NUMB_MASK;
#if	GMP_LIMB_BITS < 64
	if (vtmp > GMP_NUMB_MAX) {
		dest->_mp_d[1] = vtmp >> GMP_NUMB_BITS;
		size = 2;
	}
#endif
	dest->_mp_size = (val >= 0) ? size : -size;
}

static cob_u64_t
mpz_get_ull (const mpz_ptr src)
{
	size_t			size;

	size = mpz_size (src);
	if (!size) {
		return 0;
	}
#if	GMP_LIMB_BITS > 32
	return (cob_u64_t)src->_mp_d[0];
#else
	if (size < 2) {
		return (cob_u64_t)src->_mp_d[0];
	}
	return (cob_u64_t)src->_mp_d[0] |
		((cob_u64_t)src->_mp_d[1] << GMP_NUMB_BITS);
#endif
}

static cob_s64_t
mpz_get_sll (const mpz_ptr src)
{
	int			size;
	cob_u64_t		vtmp;

	size = src->_mp_size;
	if (!size) {
		return 0;
	}
	vtmp = (cob_u64_t)src->_mp_d[0];
#if	GMP_LIMB_BITS < 64
	if (mpz_size (src) > 1) {
		vtmp |= (cob_u64_t)src->_mp_d[1] << GMP_NUMB_BITS;
	}
#endif
	if (size > 0) {
		return (cob_s64_t) vtmp & COB_MAX_LL;
	}
	return ~(((cob_s64_t) vtmp - 1LL) & COB_MAX_LL);
}

#endif	/* COB_EXPERIMENTAL */


void
cob_gmp_free (void * ptr) {
/* mpir/gmp free functions */
#ifdef HAVE_MP_GET_MEMORY_FUNCTIONS
	void (*freefunc)(void *, size_t);
	mp_get_memory_functions (NULL, NULL, &freefunc);
	freefunc (ptr, strlen((char*) ptr) + 1);
#else
	free (ptr);
#endif
}

static COB_INLINE COB_A_INLINE void
num_byte_memcpy (unsigned char *s1, const unsigned char *s2, size_t size)
{
	do {
		*s1++ = *s2++;
	} while (--size);
}

static COB_INLINE COB_A_INLINE cob_s64_t
cob_binary_get_sint64 (const cob_field * const f)
{
	cob_s64_t	n = 0;
	size_t		fsiz = 8U - f->size;

#ifndef WORDS_BIGENDIAN
	if (COB_FIELD_BINARY_SWAP (f)) {
		num_byte_memcpy ((unsigned char *)&n, f->data, f->size);
		n = COB_BSWAP_64 (n);
		/* Shift with sign */
		n >>= 8 * fsiz;
	} else {
		num_byte_memcpy (((unsigned char *)&n) + fsiz, f->data, f->size);
		/* Shift with sign */
		n >>= 8 * fsiz;
	}
#else	/* WORDS_BIGENDIAN */
	num_byte_memcpy ((unsigned char *)&n, f->data, f->size);
	/* Shift with sign */
	n >>= 8 * fsiz;
#endif	/* WORDS_BIGENDIAN */

	return n;
}

static COB_INLINE COB_A_INLINE cob_u64_t
cob_binary_get_uint64 (const cob_field * const f)
{
	cob_u64_t		n = 0;
	size_t			fsiz = 8U - f->size;

#ifndef WORDS_BIGENDIAN
	if (COB_FIELD_BINARY_SWAP (f)) {
		num_byte_memcpy (((unsigned char *)&n) + fsiz, f->data, f->size);
		n = COB_BSWAP_64 (n);
	} else {
		num_byte_memcpy ((unsigned char *)&n, f->data, f->size);
	}
#else	/* WORDS_BIGENDIAN */
	num_byte_memcpy (((unsigned char *)&n) + fsiz, f->data, f->size);
#endif	/* WORDS_BIGENDIAN */

	return n;
}

static COB_INLINE COB_A_INLINE void
cob_binary_set_uint64 (cob_field *f, cob_u64_t n)
{
#ifndef WORDS_BIGENDIAN
	unsigned char	*s;

	if (COB_FIELD_BINARY_SWAP (f)) {
		n = COB_BSWAP_64 (n);
		s = ((unsigned char *)&n) + 8 - f->size;
	} else {
		s = (unsigned char *)&n;
	}
	num_byte_memcpy (f->data, s, f->size);
#else	/* WORDS_BIGENDIAN */
	num_byte_memcpy (f->data, ((unsigned char *)&n) + 8 - f->size, f->size);
#endif	/* WORDS_BIGENDIAN */
}

static COB_INLINE COB_A_INLINE void
cob_binary_set_int64 (cob_field *f, cob_s64_t n)
{
#ifndef WORDS_BIGENDIAN
	unsigned char	*s;

	if (COB_FIELD_BINARY_SWAP (f)) {
		n = COB_BSWAP_64 (n);
		s = ((unsigned char *)&n) + 8 - f->size;
	} else {
		s = (unsigned char *)&n;
	}
	num_byte_memcpy (f->data, s, f->size);
#else	/* WORDS_BIGENDIAN */
	num_byte_memcpy (f->data, ((unsigned char *)&n) + 8 - f->size, f->size);
#endif	/* WORDS_BIGENDIAN */
}

/* Decimal number */

void
cob_decimal_init (cob_decimal *d)
{
	mpz_init2 (d->value, COB_MPZ_DEF);
	d->scale = 0;
}

void
cob_decimal_clear (cob_decimal *d)
{
	if (d) {
		mpz_clear (d->value);
		d->scale = 0;
	}
}

/** setting a decimal field from an unsigned binary long int */
void
cob_decimal_set_ullint (cob_decimal *d, const cob_u64_t n)
{
#ifdef	COB_LI_IS_LL
	mpz_set_ui (d->value, (cob_uli_t)n);
#else
	mpz_set_ui (d->value, (cob_uli_t)(n >> 32));
	mpz_mul_2exp (d->value, d->value, 32);
	mpz_add_ui (d->value, d->value, (cob_uli_t)(n & 0xFFFFFFFFU));
#endif
	d->scale = 0;
}

/** setting a decimal field from a signed binary long int */
void
cob_decimal_set_llint (cob_decimal *d, const cob_s64_t n)
{
#ifdef	COB_LI_IS_LL
	mpz_set_si (d->value, (cob_sli_t)n);
#else
	cob_u64_t	uval;
	cob_u32_t	negative;

	negative = 0;
	if (n < 0) {
		negative = 1;
		uval = (cob_u64_t)-n;
	} else {
		uval = (cob_u64_t)n;
	}
	mpz_set_ui (d->value, (cob_uli_t)(uval >> 32));
	mpz_mul_2exp (d->value, d->value, 32);
	mpz_add_ui (d->value, d->value, (cob_uli_t)(uval & 0xFFFFFFFFU));
	if (negative) {
		mpz_neg (d->value, d->value);
	}
#endif
	d->scale = 0;
}

/* Decimal <-> Decimal */

static COB_INLINE COB_A_INLINE void
cob_decimal_set (cob_decimal *dst, const cob_decimal *src)
{
	mpz_set (dst->value, src->value);
	dst->scale = src->scale;
}

/* Decimal print */
static void
cob_decimal_print (cob_decimal *d, FILE *fp)
{
	int	scale;

	if (unlikely (d->scale == COB_DECIMAL_NAN)) {
		fprintf (fp, "(Nan)");
		return;
	}
	if (unlikely (d->scale == COB_DECIMAL_INF)) {
		fprintf (fp, "(Inf)");
		return;
	}
	if (!mpz_sgn (d->value)) {
		fprintf (fp, "0E0");
		return;
	}
	mpz_set (cob_mpzt2, d->value);
	scale = d->scale;
	for ( ; ; ) {
		if (!mpz_divisible_ui_p (cob_mpzt2, 10UL)) {
			break;
		}
		mpz_tdiv_q_ui (cob_mpzt2, cob_mpzt2, 10UL);
		scale--;
	}
	gmp_fprintf (fp, "%ZdE%d", cob_mpzt2, -scale);
}

/* d->value *= 10^n, d->scale += n */
static void
shift_decimal (cob_decimal *d, const int n)
{
	if (n == 0) {
		return;
	}
	if (n > 0) {
		mpz_ui_pow_ui (cob_mexp, 10UL, (cob_uli_t)n);
		mpz_mul (d->value, d->value, cob_mexp);
	} else {
		mpz_ui_pow_ui (cob_mexp, 10UL, (cob_uli_t)-n);
		mpz_tdiv_q (d->value, d->value, cob_mexp);
	}
	d->scale += n;
}

/* Align decimal */
static void
align_decimal (cob_decimal *d1, cob_decimal *d2)
{
	if (d1->scale < d2->scale) {
		shift_decimal (d1, d2->scale - d1->scale);
	} else if (d1->scale > d2->scale) {
		shift_decimal (d2, d1->scale - d2->scale);
	}
}

/* IEEE 754 floats */

#if	0	/* Clamp */
static unsigned int
cob_clamp_decimal (cob_decimal *d, const unsigned int expomin,
		   const unsigned int expomax, const unsigned int sigfbits)
{
	int		size;
	unsigned int	count;

	if (!mpz_sgn (d->value)) {
		/* Value is zero */
		d->scale = 0;
		return 0;
	}
	/* Remove trailing 0 from decimal places (if any) */
	for ( ; d->scale > 0; d->scale--) {
		if (!mpz_divisible_ui_p (d->value, 10UL)) {
			break;
		}
		mpz_tdiv_q_ui (d->value, d->value, 10UL);
	}
	if (d->scale) {
		/* Have decimal places */
		size = (int)mpz_sizeinbase (d->value, 2);
		for (; size > sigfbits && d->scale; d->scale--) {
			mpz_tdiv_q_ui (d->value, d->value, 10UL);
			size = (int)mpz_sizeinbase (d->value, 2);
		}
		return expomin - (unsigned int)d->scale;
	}
	for (count = 0; count < expomax; ++count) {
		if (!mpz_divisible_ui_p (d->value, 10UL)) {
			break;
		}
		mpz_tdiv_q_ui (d->value, d->value, 10UL);
	}
	return expomin + count;
}
#endif

#if	0	/* Binary */
static void
cob_decimal_set_ieee_binary (cob_decimal *d, const cob_field *f)
{
	unsigned char	*data;
	unsigned int	n;
	unsigned int	expo;
	unsigned char	bd[16];

	data = f->data;
	expo = ((data[0] & 0x7FU) << 8U) | data[1];
	if (expo == 0x7FFFU) {
		mpz_set_ui (d->value, 1UL);
		d->scale = COB_DECIMAL_NAN;
		return;
	}
}
#endif

static int
cob_decimal_get_ieee64dec (cob_decimal *d, cob_field *f, const int opt)
{
	int		sign;
	cob_u64_t	expo;
	cob_u64_t	data;

	sign = mpz_sgn (d->value);
	if (!sign) {
		memset (f->data, 0, (size_t)8);
		return 0;
	}
	if (sign < 0) {
		mpz_neg (d->value, d->value);
	}
	for ( ; ; d->scale--) {
		if (!mpz_divisible_ui_p (d->value, 10UL)) {
			break;
		}
		mpz_tdiv_q_ui (d->value, d->value, 10UL);
	}
	if (mpz_cmpabs (d->value, cob_mpz_ten16m1) >= 0) {
		if (opt & COB_STORE_KEEP_ON_OVERFLOW) {
			cob_set_exception (COB_EC_SIZE_OVERFLOW);
			return cobglobptr->cob_exception_code;
		}
#if	0	/* RXWRXW - FP Trunc */
		if (d->scale > 0 ) {
			for ( ; d->scale; ) {
#endif
		for ( ; ; ) {
			mpz_tdiv_q_ui (d->value, d->value, 10UL);
			d->scale--;
			if (mpz_cmpabs (d->value, cob_mpz_ten16m1) < 0) {
				break;
			}
		}
#if	0	/* RXWRXW - FP Trunc */
		} else {
			mpz_tdiv_r (d->value, d->value, cob_mpze10[16]);
		}
#endif
	}
	if (d->scale < -398 || d->scale > 369) {
		cob_set_exception (COB_EC_SIZE_OVERFLOW);
		return cobglobptr->cob_exception_code;
	}
	expo = 398 - d->scale;
#if	0	/* Clamp */
	expo = cob_clamp_decimal (d, 398U, 369U, 53U);
#endif

	data = 0;
	mpz_export (&data, NULL, -1, (size_t)8, COB_MPZ_ENDIAN,
		    (size_t)0, d->value);
	/* Move in exponent */
	if (mpz_sizeinbase (d->value, 2) > 51U) {
		data &= COB_64_SIGF_2;
		data |= (expo << 51U) | COB_DEC_EXTEND | COB_64_OR_EXTEND;
	} else {
		data &= COB_64_SIGF_1;
		data |= (expo << 53U);
	}
	if (sign < 0) {
		data |= COB_DEC_SIGN;
	}
	memcpy (f->data, &data, (size_t)8);
	return 0;
}

static void
cob_decimal_set_ieee64dec (cob_decimal *d, const cob_field *f)
{
	cob_u64_t	expo;
	cob_u64_t	sign;
	cob_u64_t	data;

	/* bit 0 : sign bit */
	/* bits 1 - 4 : combination field */
	/* combination = 15 (all bits set) is inf/nan */
	/* combination > 11 (bits 1100) is extended exponent */
	/* Exponent length - 10 bits */

	memcpy (&data, f->data, sizeof(data));
	sign = data & COB_DEC_SIGN;
	if (COB_64_IS_SPECIAL (data)) {
		/* Inf / Nan */
		mpz_set_ui (d->value, 1UL);
		d->scale = COB_DECIMAL_NAN;
		return;
	}
	if (COB_64_IS_EXTEND (data)) {
		expo = (data & COB_64_EXPO_2) >> 51U;
		data &= COB_64_SIGF_2;
		data |= COB_64_OR_EXTEND;
		if (data > COB_U64_C(9999999999999999)) {
			mpz_set_ui (d->value, 0UL);
			d->scale = 0;
			return;
		}
	} else {
		expo = (data & COB_64_EXPO_1) >> 53U;
		data &= COB_64_SIGF_1;
	}
	if (!data) {
		/* Significand 0 */
		mpz_set_ui (d->value, 0UL);
		d->scale = 0;
		return;
	}
#ifdef	COB_LI_IS_LL
	mpz_set_ui (d->value, data);
#else
	mpz_set_ui (d->value, (cob_uli_t)(data >> 32));
	mpz_mul_2exp (d->value, d->value, 32);
	mpz_add_ui (d->value, d->value, (cob_uli_t)(data & 0xFFFFFFFFU));
#endif

	d->scale = (int)expo - 398;
	if (d->scale > 0) {
		mpz_ui_pow_ui (cob_mexp, 10UL, (cob_uli_t)d->scale);
		mpz_mul (d->value, d->value, cob_mexp);
		d->scale = 0;
	} else if (d->scale < 0) {
		d->scale = -(d->scale);
	}
	if (sign) {
		mpz_neg (d->value, d->value);
	}
}

static int
cob_decimal_get_ieee128dec (cob_decimal *d, cob_field *f, const int opt)
{
	cob_u64_t	expo;
	cob_u64_t	data[2];
	int		sign;

	sign = mpz_sgn (d->value);
	if (!sign) {
		memset (f->data, 0, (size_t)16);
		return 0;
	}
	if (sign < 0) {
		mpz_neg (d->value, d->value);
	}
	for ( ; ; d->scale--) {
		if (!mpz_divisible_ui_p (d->value, 10UL)) {
			break;
		}
		mpz_tdiv_q_ui (d->value, d->value, 10UL);
	}
	if (mpz_cmpabs (d->value, cob_mpz_ten34m1) >= 0) {
		if (opt & COB_STORE_KEEP_ON_OVERFLOW) {
			cob_set_exception (COB_EC_SIZE_OVERFLOW);
			return cobglobptr->cob_exception_code;
		}
#if	0	/* RXWRXW - FP Trunc */
		if (d->scale > 0 ) {
			for ( ; d->scale; ) {
#endif
		for ( ; ; ) {
			mpz_tdiv_q_ui (d->value, d->value, 10UL);
			d->scale--;
			if (mpz_cmpabs (d->value, cob_mpz_ten34m1) < 0) {
				break;
			}
		}
#if	0	/* RXWRXW - FP Trunc */
		} else {
			mpz_tdiv_r (d->value, d->value, cob_mpze10[34]);
		}
#endif
	}
	if (d->scale < -6176 || d->scale > 6111) {
		cob_set_exception (COB_EC_SIZE_OVERFLOW);
		return cobglobptr->cob_exception_code;
	}
	expo = 6176 - d->scale;
#if	0	/* Clamp */
	expo = cob_clamp_decimal (d, 6176U, 6111U, 113U);
#endif

	data[0] = 0;
	data[1] = 0;
	mpz_export (data, NULL, -1, (size_t)16, COB_MPZ_ENDIAN,
		    (size_t)0, d->value);
	/* Move in exponent */
#if	0	/* IEEE canonical */
	if (mpz_sizeinbase (d->value, 2) > 113U) {
		COB_128_MSW(data) &= COB_128_SIGF_2;
		COB_128_MSW(data) |= (expo << 47U) |
				     COB_DEC_EXTEND | COB_128_OR_EXTEND;
	} else {
#endif
		COB_128_MSW(data) &= COB_128_SIGF_1;
		COB_128_MSW(data) |= (expo << 49U);
#if	0	/* IEEE canonical */
	}
#endif
	if (sign < 0) {
		COB_128_MSW(data) |= COB_DEC_SIGN;
	}
	memcpy (f->data, data, (size_t)16);
	return 0;
}

static void
cob_decimal_set_ieee128dec (cob_decimal *d, const cob_field *f)
{
	cob_u64_t	expo;
	cob_u64_t	sign;
	cob_u64_t	data[2];

	/* bit 0 : sign bit */
	/* bits 1 - 4 : combination field */
	/* combination = 15 (all bits set) is inf/nan */
	/* combination > 11 (bits 1100) is extended exponent */
	/* Exponent length - 14 bits */

	memcpy (data, f->data, sizeof(data));
	sign = COB_128_MSW(data) & COB_DEC_SIGN;
	if (COB_128_IS_SPECIAL (data)) {
		/* Inf / Nan */
		mpz_set_ui (d->value, 1UL);
		d->scale = COB_DECIMAL_NAN;
		return;
	}
	if (COB_128_IS_EXTEND (data)) {
		expo = (COB_128_MSW(data) & COB_128_EXPO_2) >> 47U;
		COB_128_MSW(data) &= COB_128_SIGF_2;
		COB_128_MSW(data) |= COB_128_OR_EXTEND;
#if	0	/* RXWRXW - IEEE cap at 34 digits */
		/* Non-canonical */
		mpz_set_ui (d->value, 0);
		d->scale = 0;
		return;
#endif
	} else {
		expo = (COB_128_MSW(data) & COB_128_EXPO_1) >> 49U;
		COB_128_MSW(data) &= COB_128_SIGF_1;
	}
	if (!COB_128_MSW(data) && !COB_128_LSW(data)) {
		/* Significand 0 */
		mpz_set_ui (d->value, 0UL);
		d->scale = 0;
		return;
	}
#ifdef	COB_LI_IS_LL
	mpz_set_ui (d->value, COB_128_MSW(data));
	mpz_mul_2exp (d->value, d->value, 64UL);
	mpz_add_ui (d->value, d->value, COB_128_LSW(data));
#else
	/* RXWRXW - Fixme */
	mpz_set_ui (d->value, (cob_uli_t)(COB_128_MSW(data) >> 32U));
	mpz_mul_2exp (d->value, d->value, 32UL);
	mpz_add_ui (d->value, d->value, (cob_uli_t)(COB_128_MSW(data) & 0xFFFFFFFFU));
	mpz_mul_2exp (d->value, d->value, 32UL);
	mpz_add_ui (d->value, d->value, (cob_uli_t)(COB_128_LSW(data) >> 32U));
	mpz_mul_2exp (d->value, d->value, 32UL);
	mpz_add_ui (d->value, d->value, (cob_uli_t)(COB_128_LSW(data) & 0xFFFFFFFFU));
#endif

	if (mpz_cmpabs (d->value, cob_mpz_ten34m1) >= 0) {
		/* Non-canonical */
		mpz_set_ui (d->value, 0UL);
		d->scale = 0;
		return;
	}
	d->scale = (int)expo - 6176;
	if (d->scale > 0) {
		mpz_ui_pow_ui (cob_mexp, 10UL, (cob_uli_t)d->scale);
		mpz_mul (d->value, d->value, cob_mexp);
		d->scale = 0;
	} else if (d->scale < 0) {
		d->scale = -(d->scale);
	}
	if (sign) {
		mpz_neg (d->value, d->value);
	}
}

#if	0	/* RXWRXW - Endian */
static void
cob_decimal_set_ieee128dec (cob_decimal *d, const cob_field *f)
{
	unsigned int	sign;
	unsigned int	expo;
	unsigned int	comb;
	unsigned char	data[16];

	/* bit 0 : sign bit */
	/* bits 1 - 4 : combination field */
	/* combination = 15 (all bits set) is inf/nan */
	/* combination > 11 (bits 1100) is extended exponent */
	/* Exponent length - 14 bits */
	memcpy (data, f->data, 16);
	sign = data[0] >> 7U;
	comb = (data[0] & 0x78U) >> 3U;
	if (comb == 15U) {
		mpz_set_ui (d->value, 1UL);
		d->scale = COB_DECIMAL_NAN;
		return;
	}
	if (comb > 11U) {
		/* 5 bits from byte 0 - 8 bits from byte 1 - 1 bit from byte 2 */
		expo = ((data[0] & 0x1FU) << 9U) | (data[1] << 1U) |
			(data[2] >> 7U);
		/* Mask out expo bit in byte 2 */
		data[2] &= 0x7FU;
		/* Set 100 bits left of significand in byte 1*/
		data[1] = 0x02U;
	} else {
		/* 7 bits from byte 0 - 7 bits from byte 1 */
		expo = ((data[0] & 0x7FU) << 7U) | (data[1] >> 1U);
		/* Mask out expo bits */
		data[1] &= 0x01U;
	}
	mpz_import (d->value, 15, 1, 1, 1, 0, &data[1]);
	if (!mpz_sgn (d->value)) {
		/* Significand 0 */
		d->scale = 0;
		return;
	}
	if (sign) {
		mpz_neg (d->value, d->value);
	}
	d->scale = (int)expo - 6176;
	if (d->scale > 0) {
		mpz_ui_pow_ui (cob_mexp, 10, (cob_uli_t)d->scale);
		mpz_mul (d->value, d->value, cob_mexp);
		d->scale = 0;
	} else if (d->scale < 0) {
		d->scale = -(d->scale);
	}
}
#endif

/* Double */

static void
cob_decimal_set_double (cob_decimal *d, const double v)
{
	char			*p;
	char			*q;
	cob_u64_t		t1;
	cob_sli_t		scale;
	cob_sli_t		len;
	int			sign;
	union {
		double		d1;
		cob_u64_t	l1;
	} ud;

	memset (&t1, ' ', sizeof(t1));
	ud.d1 = v;
	if (ud.l1 == 0 || ud.l1 == t1 || !ISFINITE (v)) {
		mpz_set_ui (d->value, 0UL);
		d->scale = 0;
		return;
	}

	sign = 0;
	mpf_set_d (cob_mpft, v);

	q = mpf_get_str (NULL, &scale, 10, (size_t)96, cob_mpft);
	if (!*q) {
		mpz_set_ui (d->value, 0UL);
		d->scale = 0;
		cob_gmp_free(q);
		return;
	}
	p = q;
	if (*p == '-') {
		sign = 1;
		++p;
	}

	mpz_set_str (d->value, p, 10);

	len = (cob_sli_t)strlen (p);
	len -= scale;
	if (len >= 0) {
		d->scale = len;
	} else {
		mpz_ui_pow_ui (cob_mexp, 10UL, (cob_uli_t)-len);
		mpz_mul (d->value, d->value, cob_mexp);
		d->scale = 0;
	}

	if (sign) {
		mpz_neg (d->value, d->value);
	}
	cob_gmp_free(q);
}

static double
cob_decimal_get_double (cob_decimal *d)
{
	double		v;
	cob_sli_t	n;

	v = 0.0;
	if (unlikely (mpz_size (d->value) == 0)) {
		return v;
	}

	mpf_set_z (cob_mpft, d->value);

	n = d->scale;
	if (n < 0) {
		mpz_ui_pow_ui (cob_mexp, 10UL, (cob_uli_t)-n);
		mpf_set_z (cob_mpft_get, cob_mexp);
		mpf_mul (cob_mpft, cob_mpft, cob_mpft_get);
	} else if (n > 0) {
		mpz_ui_pow_ui (cob_mexp, 10UL, (cob_uli_t)n);
		mpf_set_z (cob_mpft_get, cob_mexp);
		mpf_div (cob_mpft, cob_mpft, cob_mpft_get);
	}

	v = mpf_get_d (cob_mpft);
	if (!ISFINITE (v)) {
		v = 0.0;
	}
	return v;
}

/* PACKED-DECIMAL */

static int
cob_packed_get_sign (const cob_field *f)
{
	unsigned char *p;

	if (!COB_FIELD_HAVE_SIGN (f) || COB_FIELD_NO_SIGN_NIBBLE (f)) {
		return 0;
	}
	p = f->data + f->size - 1;
	return ((*p & 0x0FU) == 0x0DU) ? -1 : 1;
}

#if	0	/* RXWRXW - Buggy */
static void
cob_complement_packed (cob_field *f)
{
	unsigned char	*p;
	int		ndigs;
	int		tval;
	int		carry = 0;
	unsigned int	msn;

	ndigs = COB_FIELD_DIGITS(f) - COB_FIELD_SCALE(f);
	if (COB_FIELD_NO_SIGN_NIBBLE (f)) {
		msn = COB_FIELD_SCALE(f) % 2;
	} else {
		msn = 1 - (COB_FIELD_SCALE(f) % 2);
	}

	p = f->data + (ndigs / 2) - (1 - msn);
	while (ndigs--) {
		if (!msn) {
			tval = *p & 0x0F;
		} else {
			tval = (*p & 0xF0) >> 4;
		}
		tval += carry;
		if (tval > 0) {
			carry = 1;
			tval= 10 - tval;
		} else {
			carry = 0;
		}
		if (!msn) {
			*p = (*p & 0xF0) | tval;
			msn = 1;
		} else {
			*p = (*p & 0x0F) | (tval << 4);
			msn = 0;
			p--;
		}
	}
}

static int
cob_add_packed (cob_field *f, int val, const int opt)
{
	unsigned char	*p;
	int		sign;
	int		ndigs;
	int		tval;
	int		carry = 0;
	unsigned int	msn;
	unsigned int	subtr = 0;
	unsigned int	zeroes = 0;
	unsigned int	origdigs;
	unsigned char	savedata[256];

	ndigs = COB_FIELD_DIGITS(f) - COB_FIELD_SCALE(f);
	if (ndigs <= 0) {
		return 0;
	}

	if (opt & COB_STORE_KEEP_ON_OVERFLOW) {
		memcpy (savedata, f->data, f->size);
	}

	sign = cob_packed_get_sign (f);

	if (COB_FIELD_NO_SIGN_NIBBLE (f)) {
		msn = COB_FIELD_SCALE(f) % 2;
	} else {
		msn = 1 - (COB_FIELD_SCALE(f) % 2);
	}

	/* -x +v = -(x - v), -x -v = -(x + v) */
	if (sign < 0) {
		val = -val;
	}
	if (val < 0) {
		val = -val;
		subtr = 1;
	}
	p = f->data + (ndigs / 2) - (1 - msn);
	origdigs = ndigs;
	while (ndigs--) {
		if (val) {
			carry += (val % 10);
			val /= 10;
		}
		if (!msn) {
			tval = *p & 0x0F;
		} else {
			tval = (*p & 0xF0) >> 4;
		}
		if (subtr) {
			tval -= carry;
			if (tval < 0) {
				tval += 10;
				carry = 1;
			} else {
				carry = 0;
			}
		} else {
			tval += carry;
			if (tval > 9) {
				tval = (tval + 6) & 0x0F;
				carry = 1;
			} else {
				carry = 0;
			}
		}
		if (tval == 0) {
			zeroes++;
		}
		if (!msn) {
			*p = (*p & 0xF0) | tval;
			msn = 1;
		} else {
			*p = (*p & 0x0F) | (tval << 4);
			msn = 0;
			p--;
		}
	}
	if (sign) {
		p = f->data + f->size - 1;
		if (origdigs == zeroes) {
			*p = (*p & 0xF0) | 0x0C;
		} else if (subtr && carry) {
			cob_complement_packed (f);
			sign = -sign;
			if (sign < 0) {
				*p = (*p & 0xF0) | 0x0D;
			} else {
				*p = (*p & 0xF0) | 0x0C;
			}
		}
	} else if (subtr && carry) {
		cob_complement_packed (f);
	}
	if (opt && (carry || val)) {
		/* Overflow */
		cob_set_exception (COB_EC_SIZE_OVERFLOW);
		/* If we need to throw an exception */
		if (opt & COB_STORE_KEEP_ON_OVERFLOW) {
			memcpy (f->data, savedata, f->size);
			return cobglobptr->cob_exception_code;
		}
	}
	return 0;
}
#endif

void
cob_set_packed_zero (cob_field *f)
{
	memset (f->data, 0, f->size);
	if (COB_FIELD_NO_SIGN_NIBBLE (f)) {
		return;
	}
	if (!COB_FIELD_HAVE_SIGN (f)) {
		*(f->data + f->size - 1) = 0x0F;
	} else {
		*(f->data + f->size - 1) = 0x0C;
	}
}

static void
cob_decimal_set_packed (cob_decimal *d, cob_field *f)
{
	unsigned char	*p;
	unsigned char	*endp;
	int		digits;
	int		sign;
	int		nibtest;
	unsigned int	byteval;
	unsigned int	nonzero;

	p = f->data;
	digits = COB_FIELD_DIGITS (f);
#if	0	/* RXWRXW - P Fix */
	if (digits > (f->size * 2) - 1) {
		digits = (f->size * 2) - 1;
	}
#endif
	sign = cob_packed_get_sign (f);

	if (unlikely (COB_FIELD_NO_SIGN_NIBBLE (f))) {
		endp = f->data + f->size;
		nibtest = 1;
	} else {
		endp = f->data + f->size - 1;
		nibtest = 0;
	}

	byteval = 0;
	if (digits % 2 == nibtest) {
		byteval = *p & 0x0FU;
		p++;
	}
	mpz_set_ui (d->value, (cob_uli_t)byteval);
	nonzero = !!byteval;

	for (; p < endp; p++) {
		if (nonzero) {
			mpz_mul_ui (d->value, d->value, 100UL);
		}
		if (*p) {
			mpz_add_ui (d->value, d->value,
				    (cob_uli_t)((*p >> 4U) * 10U) + (*p & 0x0FU));
			nonzero = 1;
		}
	}

	if (!nibtest) {
		if (nonzero) {
			mpz_mul_ui (d->value, d->value, 10UL);
		}
		mpz_add_ui (d->value, d->value, (cob_uli_t)(*p >> 4U));
	}

	if (sign < 0) {
		mpz_neg (d->value, d->value);
	}
	d->scale = COB_FIELD_SCALE(f);
}

static int
cob_decimal_get_packed (cob_decimal *d, cob_field *f, const int opt)
{
	unsigned char	*data;
	unsigned char	*p;
	unsigned char	*q;
	char		*mza;
	size_t		size;
	size_t		n;
	size_t		i;
	int		diff;
	int		sign;
	int		digits;
	unsigned int	x;

#if	0	/* RXWRXW stack */
	char		buff[1024];
#endif

	/* Build string */
	sign = mpz_sgn (d->value);
	if (!sign) {
		/* Value is 0 */
		cob_set_packed_zero (f);
		return 0;
	}
	if (sign < 0) {
		mpz_abs (d->value, d->value);
	}

#if	0	/* RXWRXW stack */
	if (unlikely (mpz_sizeinbase (d->value, 10) > sizeof(buff) - 1)) {
#endif
		mza = mpz_get_str (NULL, 10, d->value);
#if	0	/* RXWRXW stack */
	} else {
		mza = buff;
		(void)mpz_get_str (buff, 10, d->value);
	}
#endif
	size = strlen (mza);

	/* Store number */
	data = f->data;
	digits = COB_FIELD_DIGITS (f);
#if	0	/* RXWRXW - P Fix */
	if (digits > (f->size * 2) - 1) {
		digits = (f->size * 2) - 1;
	}
#endif
	q = (unsigned char *)mza;
	diff = (int)(digits - size);
	if (diff < 0) {
		/* Overflow */
		cob_set_exception (COB_EC_SIZE_OVERFLOW);

		/* If the statement has SIZE ERROR
		   then throw an exception */
		if (opt & COB_STORE_KEEP_ON_OVERFLOW) {
#if	0	/* RXWRXW stack */
			if (unlikely (mza != buff)) {
#endif
				cob_gmp_free(mza);

#if	0	/* RXWRXW stack */
			}
#endif
			return cobglobptr->cob_exception_code;
		}
		q += size - digits;
		size = digits;
	}
	memset (data, 0, f->size);
	if (COB_FIELD_NO_SIGN_NIBBLE (f)) {
		p = data + ((digits - 1) / 2) - ((size - 1) / 2);
		diff = (int)(size % 2);
	} else {
		p = data + (digits / 2) - (size / 2);
		diff = 1 - (int)(size % 2);
	}
	for (i = diff, n = 0; i < size + diff; i++, n++) {
		x = COB_D2I (q[n]);
		if (i % 2 == 0) {
			*p = (unsigned char) x << 4;
		} else {
			*p++ |= x;
		}
	}

#if	0	/* RXWRXW stack */
	if (unlikely (mza != buff)) {
#endif
		cob_gmp_free(mza);

#if	0	/* RXWRXW stack */
	}
#endif

	if (COB_FIELD_NO_SIGN_NIBBLE (f)) {
		return 0;
	}

	p = f->data + f->size - 1;
	if (!COB_FIELD_HAVE_SIGN (f)) {
		*p = (*p & 0xF0U) | 0x0FU;
	} else if (sign < 0) {
		*p = (*p & 0xF0U) | 0x0DU;
	} else {
		*p = (*p & 0xF0U) | 0x0CU;
	}

	return 0;
}

void
cob_set_packed_int (cob_field *f, const int val)
{
	unsigned char	*p;
	size_t		sign = 0;
	cob_u32_t	n;

	if (!val) {
		cob_set_packed_zero (f);
		return;
	}
	if (val < 0) {
		n = (cob_u32_t)-val;
		sign = 1;
	} else {
		n = (cob_u32_t)val;
	}
	memset (f->data, 0, f->size);
	p = f->data + f->size - 1;
	if (!COB_FIELD_NO_SIGN_NIBBLE (f)) {
		*p = (n % 10) << 4;
		if (!COB_FIELD_HAVE_SIGN (f)) {
			*p |= 0x0FU;
		} else if (sign) {
			*p |= 0x0DU;
		} else {
			*p |= 0x0CU;
		}
		n /= 10;
		p--;
	}
	for (; n && p >= f->data; n /= 100, p--) {
		*p = packed_bytes[n % 100];
	}
	if (COB_FIELD_NO_SIGN_NIBBLE (f)) {
		if ((COB_FIELD_DIGITS(f) % 2) == 1) {
			*(f->data) &= 0x0FU;
		}
		return;
	}
	if ((COB_FIELD_DIGITS(f) % 2) == 0) {
		*(f->data) &= 0x0FU;
	}
}

/* DISPLAY */

static void
cob_decimal_set_display (cob_decimal *d, cob_field *f)
{
	unsigned char	*data;
	unsigned char	*p;
	size_t		size;
	int		sign;
	cob_uli_t	n;

	data = COB_FIELD_DATA (f);
	size = COB_FIELD_SIZE (f);
	if (unlikely (*data == 255)) {
		mpz_ui_pow_ui (d->value, 10UL, (cob_uli_t)size);
		d->scale = COB_FIELD_SCALE(f);
		return;
	}
	if (unlikely (*data == 0)) {
		mpz_ui_pow_ui (d->value, 10UL, (cob_uli_t)size);
		mpz_neg (d->value, d->value);
		d->scale = COB_FIELD_SCALE(f);
		return;
	}
	sign = COB_GET_SIGN (f);
	/* Skip leading zeros (also invalid space/low-value) */
	while (size > 1 && (*data & 0x0FU) == 0) {
		size--;
		data++;
	}

	/* Set value */
	n = 0;

#ifdef	COB_LI_IS_LL
	if (size < 20) {
#else
	if (size < 10) {
#endif
		while (size--) {
			if (n) {
				n *= 10;
			}
			n += COB_D2I (*data);
			data++;
		}
		mpz_set_ui (d->value, n);
	} else {
		p = cob_fast_malloc (size + 1U);
		for (; n < size; ++n) {
			p[n] = (data[n] & 0x0FU) + '0';
		}
		p[size] = 0;
		mpz_set_str (d->value, (char *)p, 10);
		cob_free (p);
	}

	/* Set sign and scale */
	if (sign < 0 && mpz_sgn (d->value)) {
		mpz_neg (d->value, d->value);
	}
	d->scale = COB_FIELD_SCALE(f);
	COB_PUT_SIGN (f, sign);
}

static int
cob_decimal_get_display (cob_decimal *d, cob_field *f, const int opt)
{
	unsigned char	*data;
	char		*p;
	size_t		size;
	int		diff;
	int		sign;

	data = COB_FIELD_DATA (f);
	/* Build string */
	sign = mpz_sgn (d->value);
	if (!sign) {
		/* Value is 0 */
		memset (data, '0', COB_FIELD_SIZE (f));
		COB_PUT_SIGN (f, sign);
		return 0;
	}
	if (sign < 0) {
		mpz_abs (d->value, d->value);
	}
	p = mpz_get_str (NULL, 10, d->value);
	size = strlen (p);

	/* Store number */
	diff = (int)(COB_FIELD_SIZE (f) - size);
	if (unlikely (diff < 0)) {
		/* Overflow */
		cob_set_exception (COB_EC_SIZE_OVERFLOW);

		/* If the statement has ON SIZE ERROR or NOT ON SIZE ERROR,
		   then throw an exception */
		if (opt & COB_STORE_KEEP_ON_OVERFLOW) {
			cob_gmp_free(p);
			return cobglobptr->cob_exception_code;
		}

		/* Othersize, truncate digits */
		memcpy (data, p - diff, COB_FIELD_SIZE (f));
	} else {
		/* No overflow */
		memset (data, '0', (size_t)diff);
		memcpy (data + diff, p, size);
	}

	cob_gmp_free(p);
	COB_PUT_SIGN (f, sign);

	return 0;
}

/* BINARY */

static void
cob_decimal_set_binary (cob_decimal *d, cob_field *f)
{
#ifdef	COB_EXPERIMENTAL
#if	1	/* RXWRXW - set_usll */
	size_t		size;
	size_t		sizeb;
	size_t		idx;
	int		order;
	unsigned char	buff[COB_MAX_BINARY + 1];

	size = f->size;
#ifndef WORDS_BIGENDIAN
	if (!COB_FIELD_BINARY_SWAP (f)) {
		sizeb = size - 1;
		order = -1;
	} else {
		sizeb = 0;
		order = 1;
	}
#else
	sizeb = 0;
	order = 1;
#endif
	if (COB_FIELD_HAVE_SIGN (f) && (f->data[sizeb] & 0x80U)) {
		for (idx = 0; idx < size; ++idx) {
			buff[idx] = ~f->data[idx];
		}
		mpz_import (d->value, 1, order, size, order, 0, buff);
		mpz_com (d->value, d->value);
	} else {
		mpz_import (d->value, 1, order, size, order, 0, f->data);
	}

#else
	if (COB_FIELD_HAVE_SIGN (f)) {
		mpz_set_sll (d->value, cob_binary_get_sint64 (f));
	} else {
		mpz_set_ull (d->value, cob_binary_get_uint64 (f));
	}
#endif

#elif	defined(COB_LI_IS_LL)
	if (COB_FIELD_HAVE_SIGN (f)) {
		mpz_set_si (d->value, cob_binary_get_sint64 (f));
	} else {
		mpz_set_ui (d->value, cob_binary_get_uint64 (f));
	}
#else
	cob_u64_t		uval;
	cob_s64_t		val;
	size_t			negative;

	if (f->size <= 4) {
		if (COB_FIELD_HAVE_SIGN (f)) {
			mpz_set_si (d->value, (cob_sli_t)cob_binary_get_sint64 (f));
		} else {
			mpz_set_ui (d->value, (cob_uli_t) cob_binary_get_uint64 (f));
		}
	} else {
		negative = 0;
		if (COB_FIELD_HAVE_SIGN (f)) {
			val = cob_binary_get_sint64 (f);
			if (val < 0) {
				negative = 1;
				uval = (cob_u64_t)-val;
			} else {
				uval = (cob_u64_t)val;
			}
		} else {
			uval = cob_binary_get_uint64 (f);
		}
		mpz_set_ui (d->value, (cob_uli_t)(uval >> 32));
		mpz_mul_2exp (d->value, d->value, 32);
		mpz_add_ui (d->value, d->value, (cob_uli_t)(uval & 0xFFFFFFFFU));
		if (negative) {
			mpz_neg (d->value, d->value);
		}
	}
#endif
	d->scale = COB_FIELD_SCALE(f);
}

static int
cob_decimal_get_binary (cob_decimal *d, cob_field *f, const int opt)
{
	size_t			overflow;
	size_t			sign;
	size_t			bitnum;
	size_t			digits;

#if	!defined(COB_EXPERIMENTAL) && !defined(COB_LI_IS_LL)
	cob_s64_t		llval;
	cob_u64_t		ullval;
	unsigned int		lo;
#endif

	if (unlikely (mpz_size (d->value) == 0)) {
		memset (f->data, 0, f->size);
		return 0;
	}
	overflow = 0;
	digits = COB_FIELD_DIGITS(f);
	if (COB_FIELD_HAVE_SIGN (f)) {
		sign = 1;
	} else {
		sign = 0;
		if (mpz_sgn (d->value) < 0) {
			mpz_abs (d->value, d->value);
		}
	}
	bitnum = (f->size * 8) - sign;
	if (unlikely (mpz_sizeinbase (d->value, 2) > bitnum)) {
		if (opt & COB_STORE_KEEP_ON_OVERFLOW) {
			goto overflow;
		}
		overflow = 1;
		/* Check if truncation to PIC digits is needed */
		if (opt & COB_STORE_TRUNC_ON_OVERFLOW) {
			mpz_tdiv_r (d->value, d->value, cob_mpze10[digits]);
		} else {
#if	0	/* RXWRXW - Fdiv sign */
			mpz_fdiv_r_2exp (d->value, d->value, (f->size * 8) - sign);
#endif
			mpz_fdiv_r_2exp (d->value, d->value, (f->size * 8));
		}
	} else if (opt && COB_FIELD_BINARY_TRUNC (f)) {
		if (mpz_cmpabs (d->value, cob_mpze10[digits]) >= 0) {
			/* Overflow */
			if (opt & COB_STORE_KEEP_ON_OVERFLOW) {
				goto overflow;
			}
			overflow = 1;
			/* Check if truncation to PIC digits is needed */
			if (opt & COB_STORE_TRUNC_ON_OVERFLOW) {
				mpz_tdiv_r (d->value, d->value,
					    cob_mpze10[digits]);
			} else {
				mpz_fdiv_r_2exp (d->value, d->value, (f->size * 8));
			}
		}
	}
#ifdef	COB_LI_IS_LL
	if (!sign || (overflow && !(opt & COB_STORE_TRUNC_ON_OVERFLOW))) {
		cob_binary_set_uint64 (f, mpz_get_ui (d->value));
	} else {
		cob_binary_set_int64 (f, mpz_get_si (d->value));
	}
#elif	defined(COB_EXPERIMENTAL)
	if (!sign || (overflow && !(opt & COB_STORE_TRUNC_ON_OVERFLOW))) {
		cob_binary_set_uint64 (f, mpz_get_ull (d->value));
	} else {
		cob_binary_set_int64 (f, mpz_get_sll (d->value));
	}
#else
	if (f->size <= 4) {
		if (!sign || (overflow && !(opt & COB_STORE_TRUNC_ON_OVERFLOW))) {
			cob_binary_set_uint64 (f, (cob_u64_t)mpz_get_ui (d->value));
		} else {
			cob_binary_set_int64 (f, (cob_s64_t)mpz_get_si (d->value));
		}
	} else {
		mpz_fdiv_r_2exp (cob_mpzt, d->value, 32);
		mpz_fdiv_q_2exp (d->value, d->value, 32);
		lo = mpz_get_ui (cob_mpzt);

		if (!sign || (overflow && !(opt & COB_STORE_TRUNC_ON_OVERFLOW))) {
			ullval = mpz_get_ui (d->value);
			ullval = (ullval << 32) | lo;
			cob_binary_set_uint64 (f, ullval);
		} else {
			llval = mpz_get_si (d->value);
			llval = (llval << 32) | lo;
			cob_binary_set_int64 (f, llval);
		}
	}
#endif
	if (!overflow) {
		return 0;
	}

overflow:
	cob_set_exception (COB_EC_SIZE_OVERFLOW);
	return cobglobptr->cob_exception_code;
}

/* General field */

void
cob_decimal_set_field (cob_decimal *d, cob_field *f)
{
	union {
		double	dval;
		float	fval;
	} uval;

	switch (COB_FIELD_TYPE (f)) {
	case COB_TYPE_NUMERIC_BINARY:
		cob_decimal_set_binary (d, f);
		break;
	case COB_TYPE_NUMERIC_PACKED:
		cob_decimal_set_packed (d, f);
		break;
	case COB_TYPE_NUMERIC_FLOAT:
		memcpy ((void *)&uval.fval, f->data, sizeof(float));
		cob_decimal_set_double (d, (double)uval.fval);
		break;
	case COB_TYPE_NUMERIC_DOUBLE:
		memcpy ((void *)&uval.dval, f->data, sizeof(double));
		cob_decimal_set_double (d, uval.dval);
		break;
	case COB_TYPE_NUMERIC_FP_DEC64:
		cob_decimal_set_ieee64dec (d, f);
		break;
	case COB_TYPE_NUMERIC_FP_DEC128:
		cob_decimal_set_ieee128dec (d, f);
		break;
	default:
		cob_decimal_set_display (d, f);
		break;
	}
}

void
cob_print_ieeedec (const cob_field *f, FILE *fp)
{
	union {
		double	dval;
		float	fval;
	} uval;

	switch (COB_FIELD_TYPE (f)) {
	case COB_TYPE_NUMERIC_FP_DEC64:
		cob_decimal_set_ieee64dec (&cob_d3, f);
		break;
	case COB_TYPE_NUMERIC_FP_DEC128:
		cob_decimal_set_ieee128dec (&cob_d3, f);
		break;
	case COB_TYPE_NUMERIC_FLOAT:
		memcpy ((void *)&uval.fval, f->data, sizeof(float));
		cob_decimal_set_double (&cob_d3, (double)uval.fval);
		break;
	case COB_TYPE_NUMERIC_DOUBLE:
		memcpy ((void *)&uval.dval, f->data, sizeof(double));
		cob_decimal_set_double (&cob_d3, uval.dval);
		break;
	default:
		return;
	}
	cob_decimal_print (&cob_d3, fp);
}

void
cob_print_realbin (const cob_field *f, FILE *fp, const int size)
{
	union {
		cob_u64_t	uval;
		cob_s64_t	val;
	} llval;

	if (COB_FIELD_HAVE_SIGN (f)) {
		llval.val = cob_binary_get_sint64 (f);
		fprintf (fp, CB_FMT_PLLD, size, size, llval.val);
		return;
	}
	llval.uval = cob_binary_get_uint64 (f);
	fprintf (fp, CB_FMT_PLLU, size, size, llval.uval);
}

static void
cob_decimal_do_round (cob_decimal *d, cob_field *f, const int opt)
{
	cob_uli_t	adj;
	int		sign;
	int		scale;

	sign = mpz_sgn (d->value);
	/* Returns 0 when value is 0 */
	if (!sign) {
		return;
	}
	scale = COB_FIELD_SCALE(f);
	if (scale >= d->scale) {
		return;
	}

	switch (opt & ~(COB_STORE_MASK)) {
	case COB_STORE_TRUNCATION:
		return;
	case COB_STORE_PROHIBITED:
		cob_set_exception (COB_EC_SIZE_TRUNCATION);
		return;
	case COB_STORE_AWAY_FROM_ZERO:
		adj = d->scale - scale;
		mpz_ui_pow_ui (cob_mpzt, 10UL, adj);
		mpz_tdiv_r (cob_mpzt2, d->value, cob_mpzt);
		if (mpz_sgn (cob_mpzt2)) {
			/* Not exact number */
			if (sign < 0) {
				mpz_sub (d->value, d->value, cob_mpzt);
			} else {
				mpz_add (d->value, d->value, cob_mpzt);
			}
		}
		return;
	case COB_STORE_NEAR_TOWARD_ZERO:
		adj = d->scale - scale - 1;
		mpz_ui_pow_ui (cob_mpzt, 10UL, adj);
		mpz_mul_ui (cob_mpzt, cob_mpzt, 5UL);
		mpz_tdiv_r (cob_mpzt2, d->value, cob_mpzt);
		shift_decimal (d, scale - d->scale + 1);
		if (!mpz_sgn (cob_mpzt2)) {
			return;
		}
		if (sign > 0) {
			mpz_add_ui (d->value, d->value, 5UL);
		} else {
			mpz_sub_ui (d->value, d->value, 5UL);
		}
		return;
	case COB_STORE_TOWARD_GREATER:
		adj = d->scale - scale;
		mpz_ui_pow_ui (cob_mpzt, 10UL, adj);
		mpz_tdiv_r (cob_mpzt2, d->value, cob_mpzt);
		if (mpz_sgn (cob_mpzt2)) {
			/* Not exact number */
			if (sign > 0) {
				mpz_add (d->value, d->value, cob_mpzt);
			}
		}
		return;
	case COB_STORE_TOWARD_LESSER:
		adj = d->scale - scale;
		mpz_ui_pow_ui (cob_mpzt, 10UL, adj);
		mpz_tdiv_r (cob_mpzt2, d->value, cob_mpzt);
		if (mpz_sgn (cob_mpzt2)) {
			/* Not exact number */
			if (sign < 0) {
				mpz_sub (d->value, d->value, cob_mpzt);
			}
		}
		return;
	case COB_STORE_NEAR_EVEN:
		adj = d->scale - scale - 1;
		mpz_ui_pow_ui (cob_mpzt, 10UL, adj);
		mpz_mul_ui (cob_mpzt, cob_mpzt, 5UL);
		mpz_tdiv_r (cob_mpzt, d->value, cob_mpzt);
		shift_decimal (d, scale - d->scale + 1);
		if (!mpz_sgn (cob_mpzt)) {
			adj = mpz_tdiv_ui (d->value, 100UL);
			switch (adj) {
			case 5:
			case 25:
			case 45:
			case 65:
			case 85:
				return;
			}
		}
		if (sign > 0) {
			mpz_add_ui (d->value, d->value, 5UL);
		} else {
			mpz_sub_ui (d->value, d->value, 5UL);
		}
		return;
	case COB_STORE_NEAR_AWAY_FROM_ZERO:
	default:
		shift_decimal (d, scale - d->scale + 1);
		if (sign > 0) {
			mpz_add_ui (d->value, d->value, 5UL);
		} else {
			mpz_sub_ui (d->value, d->value, 5UL);
		}
		return;
	}
}

int
cob_decimal_get_field (cob_decimal *d, cob_field *f, const int opt)
{
	cob_field		temp;
	cob_field_attr		attr;
	union {
		double			val;
		float			fval;
	} uval;

	if (unlikely (d->scale == COB_DECIMAL_NAN)) {
		cob_set_exception (COB_EC_SIZE_OVERFLOW);
		return cobglobptr->cob_exception_code;
	}

	/* work copy */
	if (d != &cob_d1) {
		mpz_set (cob_d1.value, d->value);
		cob_d1.scale = d->scale;
		d = &cob_d1;
	}

#if	0	/* RXWRXW - Round FP */
	if (!COB_FIELD_IS_FP(f)) {
#endif
		/* Rounding */
		if ((opt & COB_STORE_ROUND)) {
			cob_decimal_do_round (d, f, opt);
		}
		/* Append or truncate decimal digits */
		shift_decimal (d, COB_FIELD_SCALE(f) - d->scale);
#if	0	/* RXWRXW - Round FP */
	}
#endif

	/* Store number */
	switch (COB_FIELD_TYPE (f)) {
	case COB_TYPE_NUMERIC_BINARY:
		return cob_decimal_get_binary (d, f, opt);
	case COB_TYPE_NUMERIC_DISPLAY:
		return cob_decimal_get_display (d, f, opt);
	case COB_TYPE_NUMERIC_PACKED:
		return cob_decimal_get_packed (d, f, opt);
	case COB_TYPE_NUMERIC_FLOAT:
		uval.fval = (float) cob_decimal_get_double (d);
		memcpy (f->data, &uval.fval, sizeof (float));
		return 0;
	case COB_TYPE_NUMERIC_DOUBLE:
		uval.val = cob_decimal_get_double (d);
		memcpy (f->data, &uval.val, sizeof (double));
		return 0;
	case COB_TYPE_NUMERIC_FP_DEC64:
		return cob_decimal_get_ieee64dec (d, f, opt);
	case COB_TYPE_NUMERIC_FP_DEC128:
		return cob_decimal_get_ieee128dec (d, f, opt);
	default:
		break;
	}
	COB_ATTR_INIT (COB_TYPE_NUMERIC_DISPLAY, COB_FIELD_DIGITS(f),
			COB_FIELD_SCALE(f), COB_FLAG_HAVE_SIGN, NULL);
	temp.size = COB_FIELD_DIGITS(f);
	temp.data = cob_malloc (COB_FIELD_DIGITS(f));
	temp.attr = &attr;
	if (cob_decimal_get_display (d, &temp, opt) == 0) {
		cob_move (&temp, f);
		cob_free (temp.data);
		return 0;
	}
	cob_free (temp.data);
	return cobglobptr->cob_exception_code;
}

/* Decimal arithmetic */

void
cob_decimal_add (cob_decimal *d1, cob_decimal *d2)
{
	DECIMAL_CHECK (d1, d2);
	align_decimal (d1, d2);
	mpz_add (d1->value, d1->value, d2->value);
}

void
cob_decimal_sub (cob_decimal *d1, cob_decimal *d2)
{
	DECIMAL_CHECK (d1, d2);
	align_decimal (d1, d2);
	mpz_sub (d1->value, d1->value, d2->value);
}

void
cob_decimal_mul (cob_decimal *d1, cob_decimal *d2)
{
	DECIMAL_CHECK (d1, d2);
	d1->scale += d2->scale;
	mpz_mul (d1->value, d1->value, d2->value);
}

void
cob_decimal_div (cob_decimal *d1, cob_decimal *d2)
{
	DECIMAL_CHECK (d1, d2);

	/* Check for division by zero */
	if (unlikely (mpz_sgn (d2->value) == 0)) {
		d1->scale = COB_DECIMAL_NAN;
		/* FIXME: we currently don't handle the fatal exception correct
		   fatal->abort. We only should set it when it *doesn't* happen
		   within a arithmetic statement with SIZE error phrase and must
		   execute the appropriate USE statement, if any before the abort
		*/
		cob_set_exception (COB_EC_SIZE_ZERO_DIVIDE);
		return;
	}
	if (unlikely (mpz_sgn (d1->value) == 0)) {
		d1->scale = 0;
		return;
	}
	d1->scale -= d2->scale;
	shift_decimal (d1, COB_MAX_DIGITS + ((d1->scale < 0) ? -d1->scale : 0));
#if	0	/* RXWRXW - cdiv */
	mpz_cdiv_q (d1->value, d1->value, d2->value);
#endif
	mpz_tdiv_q (d1->value, d1->value, d2->value);
}

int
cob_decimal_cmp (cob_decimal *d1, cob_decimal *d2)
{
	align_decimal (d1, d2);
	return mpz_cmp (d1->value, d2->value);
}

/*
 * Shift 'd1' to have same scale as 'd2'
 */
void
cob_decimal_align (cob_decimal *d1, const int scale)
{
	if (d1->scale > scale) {
		shift_decimal (d1, scale - d1->scale);
	} else if (d1->scale < scale) {
		shift_decimal (d1, d1->scale - scale);
	}
	return;
}

/* Convenience functions */

void
cob_add (cob_field *f1, cob_field *f2, const int opt)
{
	cob_decimal_set_field (&cob_d1, f1);
	cob_decimal_set_field (&cob_d2, f2);
	cob_decimal_add (&cob_d1, &cob_d2);
	(void)cob_decimal_get_field (&cob_d1, f1, opt);
}

void
cob_sub (cob_field *f1, cob_field *f2, const int opt)
{
	cob_decimal_set_field (&cob_d1, f1);
	cob_decimal_set_field (&cob_d2, f2);
	cob_decimal_sub (&cob_d1, &cob_d2);
	(void)cob_decimal_get_field (&cob_d1, f1, opt);
}

void
cob_mul (cob_field *f1, cob_field *f2, const int opt)
{
	cob_decimal_set_field (&cob_d1, f1);
	cob_decimal_set_field (&cob_d2, f2);
	cob_decimal_mul (&cob_d1, &cob_d2);
	(void)cob_decimal_get_field (&cob_d1, f1, opt);
}

void
cob_div (cob_field *f1, cob_field *f2, const int opt)
{
	cob_decimal_set_field (&cob_d1, f1);
	cob_decimal_set_field (&cob_d2, f2);
	cob_decimal_div (&cob_d1, &cob_d2);
	(void)cob_decimal_get_field (&cob_d1, f1, opt);
}

void
cob_div_quotient (cob_field *dividend, cob_field *divisor,
		  cob_field *quotient, const int opt)
{
	/* Note that cob_div_quotient and cob_div_remainder must remain */
	/* separate because of COBOL rules. The quotient must be fully */
	/* evaluated before the remainder item is evaluated */
	/* e.g. DIVIDE A BY B GIVING Z REMAINDER FLD (Z). */

	cob_decimal_set_field (&cob_d1, dividend);
	cob_decimal_set_field (&cob_d2, divisor);
	cob_decimal_set (&cob_d_remainder, &cob_d1);

	/* Compute quotient */
	cob_decimal_div (&cob_d1, &cob_d2);
	/* Check divide by zero - Exception is set in cob_decimal_div */
	if (cob_d1.scale == COB_DECIMAL_NAN) {
		/* Forces an early return from cob_div_remainder */
		cob_d_remainder.scale = COB_DECIMAL_NAN;
		return;
	}

	/* Set quotient */
	cob_decimal_set (&cob_d3, &cob_d1);
	(void)cob_decimal_get_field (&cob_d1, quotient, opt);

	/* Truncate digits from the quotient */
	shift_decimal (&cob_d3, COB_FIELD_SCALE(quotient) - cob_d3.scale);

	/* Compute remainder */
	cob_decimal_mul (&cob_d3, &cob_d2);
	cob_decimal_sub (&cob_d_remainder, &cob_d3);
}

void
cob_div_remainder (cob_field *fld_remainder, const int opt)
{
	(void)cob_decimal_get_field (&cob_d_remainder, fld_remainder, opt);
}

void
cob_decimal_setget_fld (cob_field *src, cob_field *dst, const int opt)
{
	cob_decimal_set_field (&cob_d1, src);
	(void)cob_decimal_get_field (&cob_d1, dst, opt);
}

#if	0	/* RXWRXW - Buggy */

/* Optimized arithmetic for DISPLAY */

static int
display_add_int (unsigned char *data, const size_t size, int n, const int opt)
{
	unsigned char	*sp;
	size_t		carry = 0;
	int		i;
	int		is;

	sp = data + size;
	while (n > 0) {
		i = n % 10;
		n /= 10;

		/* Check for overflow */
		if (unlikely (--sp < data)) {
			return opt;
		}

		/* Perform addition */
		is = (*sp & 0x0F) + i + carry;
		if (is > 9) {
			carry = 1;
			*sp = '0' + ((is + 6) & 0x0F);
		} else {
			carry = 0;
			*sp = '0' + is;
		}
	}
	if (carry == 0) {
		return 0;
	}

	/* Carry up */
	while (--sp >= data) {
		if ((*sp += 1) <= (unsigned char)'9') {
			return 0;
		}
		*sp = '0';
	}
	return opt;
}

static int
display_sub_int (unsigned char *data, const size_t size, int n, const int opt)
{
	unsigned char	*sp;
	size_t		carry = 0;
	int		i;

	COB_UNUSED (opt);

	sp = data + size;
	while (n > 0) {
		i = n % 10;
		n /= 10;

		/* Check for overflow */
		if (unlikely (--sp < data)) {
			return 1;
		}

#if	0	/* RXWRXW - Garbage check */
		/* Correct garbage */
		*sp = (unsigned char)('0' + (*sp & 0x0F));
#endif
		/* Perform subtraction */
		if ((*sp -= i + carry) < '0') {
			carry = 1;
			*sp += 10;
		} else {
			carry = 0;
		}
	}
	if (carry == 0) {
		return 0;
	}

	/* Carry up */
	while (--sp >= data) {
#if	0	/* RXWRXW - Garbage check */
		/* Correct garbage */
		*sp = (unsigned char)('0' + (*sp & 0x0F));
#endif
		if ((*sp -= 1) >= (unsigned char)'0') {
			return 0;
		}
		*sp = '9';
	}
	return 1;
}

static int
cob_display_add_int (cob_field *f, int n, const int opt)
{
	unsigned char	*data;
	size_t		osize;
	size_t		size;
	size_t		i;
	int		scale;
	int		sign;
	unsigned char	tfield[256];

	data = COB_FIELD_DATA (f);
	size = COB_FIELD_SIZE (f);
	osize = size;
	if (opt & COB_STORE_KEEP_ON_OVERFLOW) {
		memcpy (tfield, data, size);
	}
	scale = COB_FIELD_SCALE (f);
	sign = COB_GET_SIGN (f);
	/* -x +v = -(x - v), -x -v = -(x + v) */
	if (sign < 0) {
		n = -n;
	}

	if (unlikely (scale < 0)) {
		/* PIC 9(n)P(m) */
		if (-scale < 10) {
			/* Fix optimizer bug */
			while (scale) {
				++scale;
				n /= 10;
			}
		} else {
			n = 0;
		}
		scale = 0;
		if (n == 0) {
			return 0;
		}
	} else {
		/* PIC 9(n)V9(m) */
		size -= scale;
		if (!size) {
			COB_PUT_SIGN (f, sign);
			cob_set_exception (COB_EC_SIZE_OVERFLOW);
			if (opt & COB_STORE_KEEP_ON_OVERFLOW) {
				return cobglobptr->cob_exception_code;
			}
			return 0;
		}
	}

	if (n > 0) {
		/* Add n to the field */
		if (display_add_int (data, size, n, opt) != 0) {
			/* Overflow */
			COB_PUT_SIGN (f, sign);
			cob_set_exception (COB_EC_SIZE_OVERFLOW);
			/* If we need to restore */
			if (opt & COB_STORE_KEEP_ON_OVERFLOW) {
				memcpy (data, tfield, osize);
				return cobglobptr->cob_exception_code;
			}
		}
	} else if (n < 0) {
		/* Subtract n from the field */
		if (display_sub_int (data, size, -n, opt) != 0) {
			for (i = 0; i < size; ++i) {
				data[i] = COB_I2D (9 - COB_D2I (data[i]));
			}
			if (scale) {
				for (i = size; i < size + scale; ++i) {
					if (COB_D2I (data[i]) > 0) {
						data[i] = COB_I2D (10 - COB_D2I (data[i]));
					}
				}
			} else {
				(void)display_add_int (data, size, 1, 0);
			}
			sign = -sign;
		}
	}

	COB_PUT_SIGN (f, sign);
	return 0;
}
#endif	/* Buggy */

int
cob_add_int (cob_field *f, const int n, const int opt)
{
	int	scale;
	int	val;

	if (unlikely (n == 0)) {
		return 0;
	}
#if	0	/* RXWRXW - Buggy */
	if (COB_FIELD_TYPE (f) == COB_TYPE_NUMERIC_PACKED) {
		return cob_add_packed (f, n, opt);
	} else if (COB_FIELD_TYPE (f) == COB_TYPE_NUMERIC_DISPLAY) {
		return cob_display_add_int (f, n, opt);
	}
#endif

	/* Not optimized */
	cob_decimal_set_field (&cob_d1, f);

	if (COB_FIELD_TYPE (f) >= COB_TYPE_NUMERIC_FLOAT
	&&  COB_FIELD_TYPE (f) <= COB_TYPE_NUMERIC_FP_BIN128) {
		mpz_set_si (cob_d2.value, (cob_sli_t) n);
		cob_d2.scale = 0;
		mpz_add (cob_d1.value, cob_d1.value, cob_d2.value);
		return cob_decimal_get_field (&cob_d1, f, opt);
	}
	else {
		scale = COB_FIELD_SCALE (f);
		val = n;
		if (unlikely (scale < 0)) {
			/* PIC 9(n)P(m) */
			if (-scale < 10) {
				while (scale++) {
					val /= 10;
				}
			} else {
				val = 0;
			}
			scale = 0;
			if (!val) {
				return 0;
			}
		}
		mpz_set_si (cob_d2.value, (cob_sli_t)val);
		cob_d2.scale = 0;
		if (scale > 0) {
			mpz_ui_pow_ui (cob_mexp, 10UL, (cob_uli_t)scale);
			mpz_mul (cob_d2.value, cob_d2.value, cob_mexp);
			cob_d2.scale = cob_d1.scale;
		}
		mpz_add (cob_d1.value, cob_d1.value, cob_d2.value);
		return cob_decimal_get_field (&cob_d1, f, opt);
	}
}

int
cob_sub_int (cob_field *f, const int n, const int opt)
{
	return cob_add_int (f, -n, opt);
}

int
cob_cmp_int (cob_field *f1, const int n)
{
	cob_decimal_set_field (&cob_d1, f1);
	mpz_set_si (cob_d2.value, (cob_sli_t)n);
	cob_d2.scale = 0;
	return cob_decimal_cmp (&cob_d1, &cob_d2);
}

int
cob_cmp_uint (cob_field *f1, const unsigned int n)
{
	cob_decimal_set_field (&cob_d1, f1);
	mpz_set_ui (cob_d2.value, (cob_uli_t)n);
	cob_d2.scale = 0;
	return cob_decimal_cmp (&cob_d1, &cob_d2);
}

int
cob_cmp_llint (cob_field *f1, const cob_s64_t n)
{
#ifdef	COB_LI_IS_LL
	mpz_set_si (cob_d2.value, (cob_sli_t)n);
#else
	cob_u64_t	uval;
	cob_u32_t	negative;

	negative = 0;
	if (n < 0) {
		negative = 1;
		uval = (cob_u64_t)-n;
	} else {
		uval = (cob_u64_t)n;
	}
	mpz_set_ui (cob_d2.value, (cob_uli_t)(uval >> 32));
	mpz_mul_2exp (cob_d2.value, cob_d2.value, 32);
	mpz_add_ui (cob_d2.value, cob_d2.value, (cob_uli_t)(uval & 0xFFFFFFFFU));
	if (negative) {
		mpz_neg (cob_d2.value, cob_d2.value);
	}
#endif

	cob_d2.scale = 0;
	cob_decimal_set_field (&cob_d1, f1);
	return cob_decimal_cmp (&cob_d1, &cob_d2);
}

#ifdef COB_FLOAT_DELTA
#define TOLERANCE (double) COB_FLOAT_DELTA
#else
#define TOLERANCE (double) 0.0000001
#endif
#define FLOAT_EQ(x,y,t) (fabs(((x-y)/x)) < t)

int
cob_cmp_float (cob_field *f1, cob_field *f2)
{
	double	d1,d2;
	float	flt;
	if(COB_FIELD_TYPE (f1) == COB_TYPE_NUMERIC_FLOAT) {
		memcpy(&flt,f1->data,sizeof(float));
		d1 = flt;
	} else if(COB_FIELD_TYPE (f1) == COB_TYPE_NUMERIC_DOUBLE) {
		memcpy(&d1,f1->data,sizeof(double));
	} else {
		cob_decimal_set_field (&cob_d1, f1);
		d1 = cob_decimal_get_double(&cob_d1);
	}
	if(COB_FIELD_TYPE (f2) == COB_TYPE_NUMERIC_FLOAT) {
		memcpy(&flt,f2->data,sizeof(float));
		d2 = flt;
	} else if(COB_FIELD_TYPE (f2) == COB_TYPE_NUMERIC_DOUBLE) {
		memcpy(&d2,f2->data,sizeof(double));
	} else {
		cob_decimal_set_field (&cob_d1, f2);
		d2 = cob_decimal_get_double(&cob_d1);
	}
	if(d1 == d2)
		return 0;
	if(d1 != 0.0
	&& FLOAT_EQ(d1,d2,TOLERANCE))
		return 0;
	if(d1 < d2)
		return -1;
	return 1;
}

int
cob_numeric_cmp (cob_field *f1, cob_field *f2)
{
	if(COB_FIELD_TYPE (f1) == COB_TYPE_NUMERIC_FLOAT
	|| COB_FIELD_TYPE (f1) == COB_TYPE_NUMERIC_DOUBLE
	|| COB_FIELD_TYPE (f2) == COB_TYPE_NUMERIC_FLOAT
	|| COB_FIELD_TYPE (f2) == COB_TYPE_NUMERIC_DOUBLE) {
		return cob_cmp_float(f1,f2);
	}
	cob_decimal_set_field (&cob_d1, f1);
	cob_decimal_set_field (&cob_d2, f2);
	return cob_decimal_cmp (&cob_d1, &cob_d2);
}

int
cob_cmp_packed (cob_field *f, const cob_s64_t val)
{
	unsigned char		*p;
	cob_u64_t		n;
	size_t			size;
	size_t			inc;
	int			sign;
	unsigned char		val1[20];

	sign = cob_packed_get_sign (f);
	/* Field positive, value negative */
	if (sign >= 0 && val < 0) {
		return 1;
	}
	/* Field negative, value positive */
	if (sign < 0 && val >= 0) {
		return -1;
	}
	/* Both positive or both negative */
	if (val < 0) {
		n = (cob_u64_t)-val;
	} else {
		n = (cob_u64_t)val;
	}
	inc = 0;
	p = f->data;
	for (size = 0; size < 20; size++) {
		if (size < 20 - f->size) {
			val1[size] = 0;
		} else {
			val1[size] = p[inc++];
		}
	}
	if (COB_FIELD_NO_SIGN_NIBBLE (f)) {
		if ((COB_FIELD_DIGITS(f) % 2) == 1) {
			val1[20 - f->size] &= 0x0F;
		}
	} else {
		val1[19] &= 0xF0;
		if ((COB_FIELD_DIGITS(f) % 2) == 0) {
			val1[20 - f->size] &= 0x0F;
		}
	}
	if (n != last_packed_val) {
		last_packed_val = n;
		memset (packed_value, 0, sizeof(packed_value));
		if (n) {
			p = &packed_value[19];
			if (!COB_FIELD_NO_SIGN_NIBBLE (f)) {
				*p =  (n % 10) << 4;
				p--;
				n /= 10;
			}
			for (; n;) {
				size = n % 100;
				*p = (unsigned char)((size % 10) | ((size / 10) << 4));
				n /= 100;
				p--;
			}
		}
	}
	for (size = 0; size < 20; size++) {
		if (val1[size] != packed_value[size]) {
			if (sign < 0) {
				return packed_value[size] - val1[size];
			} else {
				return val1[size] - packed_value[size];
			}
		}
	}
	return 0;
}

/* Numeric Display compares */

#ifdef	COB_EBCDIC_MACHINE
static unsigned int
cob_get_long_ascii_sign (const unsigned char *p, cob_s64_t *val)
{
	switch (*p) {
	case 'p':
		return 1;
	case 'q':
		*val += 1;
		return 1;
	case 'r':
		*val += 2;
		return 1;
	case 's':
		*val += 3;
		return 1;
	case 't':
		*val += 4;
		return 1;
	case 'u':
		*val += 5;
		return 1;
	case 'v':
		*val += 6;
		return 1;
	case 'w':
		*val += 7;
		return 1;
	case 'x':
		*val += 8;
		return 1;
	case 'y':
		*val += 9;
		return 1;
	}
	return 0;
}
#endif

static unsigned int
cob_get_long_ebcdic_sign (const unsigned char *p, cob_s64_t *val)
{
	switch (*p) {
	case '{':
		return 0;
	case 'A':
		*val += 1;
		return 0;
	case 'B':
		*val += 2;
		return 0;
	case 'C':
		*val += 3;
		return 0;
	case 'D':
		*val += 4;
		return 0;
	case 'E':
		*val += 5;
		return 0;
	case 'F':
		*val += 6;
		return 0;
	case 'G':
		*val += 7;
		return 0;
	case 'H':
		*val += 8;
		return 0;
	case 'I':
		*val += 9;
		return 0;
	case '}':
		return 1;
	case 'J':
		*val += 1;
		return 1;
	case 'K':
		*val += 2;
		return 1;
	case 'L':
		*val += 3;
		return 1;
	case 'M':
		*val += 4;
		return 1;
	case 'N':
		*val += 5;
		return 1;
	case 'O':
		*val += 6;
		return 1;
	case 'P':
		*val += 7;
		return 1;
	case 'Q':
		*val += 8;
		return 1;
	case 'R':
		*val += 9;
		return 1;
	}
	return 0;
}

int
cob_cmp_numdisp (const unsigned char *data, const size_t size,
		 const cob_s64_t n, const cob_u32_t has_sign)
{
	const unsigned char	*p;
	cob_s64_t		val = 0;
	size_t			inc;

	p = data;
	if (!has_sign) {
		if (unlikely (n < 0)) {
			return 1;
		}
		for (inc = 0; inc < size; inc++, p++) {
			val = (val * 10) + COB_D2I (*p);
		}
		return (val < n) ? -1 : (val > n);
	}
	for (inc = 0; inc < size - 1; inc++, p++) {
		val = (val * 10) + COB_D2I (*p);
	}
	val *= 10;
	if (*p >= (unsigned char)'0' && *p <= (unsigned char)'9') {
		val += COB_D2I (*p);
	} else {
		if (unlikely (COB_MODULE_PTR->ebcdic_sign)) {
			if (cob_get_long_ebcdic_sign (p, &val)) {
				val = -val;
			}
		} else {
#ifdef	COB_EBCDIC_MACHINE
			if (cob_get_long_ascii_sign (p, &val)) {
				val = -val;
			}
#else
			if (*p >= (unsigned char)'p' && *p <= (unsigned char)'y') {
				val += (*p - (unsigned char)'p');
				val = -val;
			}
#endif
		}
	}
	return (val < n) ? -1 : (val > n);
}

void
cob_decimal_alloc (const cob_u32_t params, ...)
{
	cob_decimal	**dec;
	cob_u32_t	i;
	va_list		args;

	va_start (args, params);
	for (i = 0; i < params; ++i) {
		dec = va_arg (args, cob_decimal **);
		*dec = cob_decimal_base + i;
	}
	va_end (args);
}

void
cob_decimal_push (const cob_u32_t params, ...)
{
	cob_decimal	**dec;
	cob_u32_t	i;
	va_list		args;

	va_start (args, params);
	for (i = 0; i < params; ++i) {
		dec = va_arg (args, cob_decimal **);
		*dec = cob_malloc (sizeof(cob_decimal));
		cob_decimal_init (*dec);
	}
	va_end (args);
}

void
cob_decimal_pop (const cob_u32_t params, ...)
{
	cob_decimal	*dec;
	cob_u32_t	i;
	va_list		args;

	va_start (args, params);
	for (i = 0; i < params; ++i) {
		dec = va_arg (args, cob_decimal *);
		mpz_clear (dec->value);
		cob_free (dec);
	}
	va_end (args);
}

/* Init/Exit routines */

void
cob_exit_numeric (void)
{
	cob_decimal	*d1;
	size_t		i;

	if (cob_decimal_base) {
		d1 = cob_decimal_base;
		for (i = 0; i < COB_MAX_DEC_STRUCT; d1++, i++) {
			mpz_clear (d1->value);
		}
		cob_free (cob_decimal_base);
	}

	mpz_clear (cob_d_remainder.value);

	mpz_clear (cob_d3.value);
	mpz_clear (cob_d2.value);
	mpz_clear (cob_d1.value);

	mpz_clear (cob_mexp);
	mpz_clear (cob_mpzt2);
	mpz_clear (cob_mpzt);

	mpz_clear (cob_mpz_ten34m1);
	mpz_clear (cob_mpz_ten16m1);
	for (i = 0; i < COB_MAX_BINARY; i++) {
		mpz_clear (cob_mpze10[i]);
	}

	mpf_clear (cob_mpft_get);
	mpf_clear (cob_mpft);
}

void
cob_init_numeric (cob_global *lptr)
{
	cob_decimal	*d1;
	cob_u32_t	i;

	cobglobptr = lptr;

	memset (packed_value, 0, sizeof(packed_value));
	last_packed_val = 0;

	mpf_init2 (cob_mpft, COB_MPF_PREC);
	mpf_init2 (cob_mpft_get, COB_MPF_PREC);

	for (i = 0; i < COB_MAX_BINARY; i++) {
		mpz_init2 (cob_mpze10[i], 128UL);
		mpz_ui_pow_ui (cob_mpze10[i], 10UL, (cob_uli_t)i);
	}
	mpz_init_set (cob_mpz_ten16m1, cob_mpze10[16]);
	mpz_sub_ui (cob_mpz_ten16m1, cob_mpz_ten16m1, 1UL);
	mpz_init_set (cob_mpz_ten34m1, cob_mpze10[34]);
	mpz_sub_ui (cob_mpz_ten34m1, cob_mpz_ten34m1, 1UL);

	mpz_init2 (cob_mpzt, COB_MPZ_DEF);
	mpz_init2 (cob_mpzt2, COB_MPZ_DEF);
	mpz_init2 (cob_mexp, COB_MPZ_DEF);

	cob_decimal_init (&cob_d1);
	cob_decimal_init (&cob_d2);
	cob_decimal_init (&cob_d3);
	cob_decimal_init (&cob_d_remainder);

	cob_decimal_base = cob_malloc (COB_MAX_DEC_STRUCT * sizeof(cob_decimal));
	d1 = cob_decimal_base;
	for (i = 0; i < COB_MAX_DEC_STRUCT; d1++, i++) {
		cob_decimal_init (d1);
	}
}
