#pragma once

// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"

typedef enum _CmpRes
{
	CMP_LT,
	CMP_GT,
	CMP_EQ,
} CmpRes;

void bigint_init_unsigned(BigInt *big_int, uint64_t value);
void bigint_init_signed(BigInt *big_int, int64_t value);
void bigint_init_bigint(BigInt *dest, const BigInt *src);
void bigint_init_data(BigInt *dest, const uint64_t *digits, unsigned int digit_count, bool is_negative);
void bigint_negate(BigInt *dest, const BigInt *source);
size_t bigint_clz(const BigInt *big_int, size_t bit_count);
size_t bigint_ctz(const BigInt *big_int, size_t bit_count);
bool bigint_fits_in_bits(const BigInt *big_int, size_t bit_count, bool is_signed);
void bigint_write_twos_complement(const BigInt *big_int, uint8_t *buf, size_t bit_count, bool is_big_endian);
void bigint_read_twos_complement(BigInt *dest, const uint8_t *buf, size_t bit_count, bool is_big_endian, bool is_signed);
void bigint_add(BigInt *dest, const BigInt *op1, const BigInt *op2);
void bigint_add_wrap(BigInt *dest, const BigInt *op1, const BigInt *op2, size_t bit_count, bool is_signed);
void bigint_sub(BigInt *dest, const BigInt *op1, const BigInt *op2);
void bigint_mul(BigInt *dest, const BigInt *op1, const BigInt *op2);
void bigint_mul_wrap(BigInt *dest, const BigInt *op1, const BigInt *op2, size_t bit_count, bool is_signed);
void bigint_rem(BigInt *dest, const BigInt *op1, const BigInt *op2);
void bigint_mod(BigInt *dest, const BigInt *op1, const BigInt *op2);
void bigint_shl(BigInt *dest, const BigInt *op1, const BigInt *op2);
void bigint_shl_int(BigInt *dest, const BigInt *op1, uint64_t shift);
void bigint_shl_trunc(BigInt *dest, const BigInt *op1, const BigInt *op2, size_t bit_count, bool is_signed);
void bigint_shr(BigInt *dest, const BigInt *op1, const BigInt *op2);
void bigint_div_floor(BigInt *dest, const BigInt *op1, const BigInt *op2);
void bigint_or(BigInt *dest, const BigInt *op1, const BigInt *op2);
void bigint_and(BigInt *dest, const BigInt *op1, const BigInt *op2);
void bigint_xor(BigInt *dest, const BigInt *op1, const BigInt *op2);
void bigint_negate_wrap(BigInt *dest, const BigInt *op, size_t bit_count);
void bigint_not(BigInt *dest, const BigInt *op, size_t bit_count, bool is_signed);
bool bigint_eql(BigInt a, BigInt b);
CmpRes bigint_cmp(const BigInt *op1, const BigInt *op2);
CmpRes bigint_cmp_zero(const BigInt *op);
uint32_t bigint_hash(BigInt x);
const char *bigint_to_error_string(const BigInt *bigint, uint64_t base);
void bigint_print(BigInt *bigint, uint64_t base);
void bigint_fprint(FILE *file, BigInt *bigint, uint64_t base);
uint64_t bigint_as_unsigned(const BigInt *bigint);
int64_t bigint_as_signed(const BigInt *bigint);
long double bigint_as_float(const BigInt *bigint);
void bigint_truncate(BigInt *dst, const BigInt *op, size_t bit_count, bool is_signed);
void bigint_incr(BigInt *x);
size_t bigint_popcount_signed(const BigInt *bi, size_t bit_count);
size_t bigint_popcount_unsigned(const BigInt *big_int);
