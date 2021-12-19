// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"
#include <inttypes.h>
#include <math.h>

static inline uint32_t u32_min(uint32_t a, uint32_t b)
{
	return a < b ? a : b;
}

static inline size_t size_max(size_t a, size_t b)
{
	return a > b ? a : b;
}

static inline unsigned unsigned_max(unsigned a, unsigned b)
{
	return a > b ? a : b;
}


static char digit_to_char(uint8_t digit, bool upper)
{
	if (digit <= 9)
	{
		return (char)(digit + '0');
	}
	if (digit <= 35)
	{
		return (char)(digit + (upper ? 'A' : 'a') - 10);
	}
	FATAL_ERROR("Can't reach");
}

#define HI32(_x) ((_x) >> 32)
#define LO32(_x) ((_x) & 0xffffffff)
#define ISNEG(_x) (((uint64_t)_x) >> 63)

char *i128_to_string(Int128 op, uint64_t base, bool is_signed)
{
	assert(base >= 2 && base <= 16);
	static char digits[16] = "0123456789ABCDEF";
	char buffer[130];
	char *loc = buffer;
	bool add_minus = is_signed && ISNEG(op.high);
	if (add_minus) op = i128_neg(op);
	Int128 base_div = { 0, base };
	do
	{
		Int128 rem = i128_urem(op, base_div);
		*(loc++) = digits[rem.low];
		op = i128_udiv(op, base_div);
	} while (!i128_is_zero(op));
	char *res = malloc_arena((size_t)(loc - buffer + 2));
	char *c = res;
	if (add_minus) *(c++) = '-';
	while (loc > buffer)
	{
		*(c++) = *(--loc);
	}
	*c = 0;
	return res;
}

char *int_to_str(Int i, int radix)
{
	return i128_to_string(i.i, (uint64_t)radix, type_kind_is_signed(i.type));
}


Int int_from_real(Real d, TypeKind type)
{
	return (Int){ type_kind_is_unsigned(type) ? i128_from_float_unsigned(d) : i128_from_float_signed(d),
	              type };
}

Int128 i128_from_int(uint64_t i)
{
	return (Int128){ 0, i };
}

Int128 i128_from_str(const char *str)
{
	char c;
	Int128 x = { 0, 0 };
	while ((c = *(str++)) != 0)
	{
		x = i128_add64(i128_mult64(x, 10), (uint64_t)(c - '0'));
	}
	return x;
}

Int128 i128_from_strl(const char *str, const char *end)
{
	char c;
	Int128 x = { 0, 0 };
	while (str != end)
	{
		c = *(str++);
		x = i128_add64(i128_mult64(x, 10), (uint64_t) (c - '0'));
	}
	return x;
}

Int128 i128_from_hexstrl(const char *str, const char *end)
{
	char c;
	Int128 x = { 0, 0 };
	while (str != end)
	{
		c = *(str++);
		x = i128_add64(i128_shl64(x, 4), (uint64_t)hex_nibble(c));
	}
	return x;
}

Int128 i128_add(Int128 op1, Int128 op2)
{
	uint64_t low = op1.low + op2.low;
	uint64_t high = op1.high + op2.high;
	if (low < op1.low) high++;
	return (Int128){ high, low };
}

Int128 i128_add64(Int128 op1, uint64_t op2)
{
	uint64_t low = op1.low + op2;
	return (Int128){ low < op1.low ? op1.high + 1 : op1.high, low };
}

Int128 i128_sub(Int128 op1, Int128 op2)
{
	uint64_t low = op1.low - op2.low;
	uint64_t high = op1.high - op2.high;

	if (low > op1.low) high--;
	return (Int128){ high, low };
}

Int128 i128_sub64(Int128 op1, uint64_t op2)
{
	uint64_t low = op1.low - op2;
	return (Int128){ low > op1.low ? op1.high - 1 : op1.high, low };
}

Int128 i128_extend(Int128 op, TypeKind type)
{
	int bits = type_kind_bitsize(type);
	if (bits == 128) return op;
	uint64_t shift = 128 - (uint64_t)bits;
	op = i128_shl64(op, shift);
	bool is_signed = type_kind_is_signed(type);
	if (is_signed)
	{
		return i128_ashr64(op, shift);
	}
	return i128_lshr64(op, shift);
}

Int128 i128_and(Int128 op1, Int128 op2)
{
	return (Int128){ op1.high & op2.high, op1.low & op2.low };
}

Int128 i128_or(Int128 op1, Int128 op2)
{
	return (Int128){ op1.high | op2.high, op1.low | op2.low };
}

Int128 i128_xor(Int128 op1, Int128 op2)
{
	return (Int128){ op1.high ^ op2.high, op1.low ^ op2.low };
}

Int128 i128_neg(Int128 op1)
{
	if (!op1.low && !op1.low) return op1;
	return i128_add64(i128_not(op1), 1);
}

Int128 i128_not(Int128 op1)
{
	return (Int128){ ~op1.high, ~op1.low };
}

static Int128 int64_mult(uint64_t u, uint64_t v)
{
	uint64_t u1 = LO32(u);
	uint64_t v1 = LO32(v);
	uint64_t t = u1 * v1;
	uint64_t w3 = LO32(t);
	uint64_t k = HI32(t);

	u >>= 32;
	t = u * v1 + k;
	k = LO32(t);
	uint64_t w1 = HI32(t);

	v >>= 32;
	t = u1 * v + k;

	return (Int128){ (u * v) + w1 + HI32(t), (t << 32) + w3 };
}

Int128 i128_mult(Int128 op1, Int128 op2)
{
	Int128 low_mult = int64_mult(op1.low, op2.low);
	low_mult.high += op1.high * op2.low + op1.low * op2.high;
	return low_mult;
}

Int128 i128_mult64(Int128 op1, uint64_t op2)
{
	Int128 low_mult = int64_mult(op1.low, op2);
	low_mult.high += op1.high * op2;
	return low_mult;
}


CmpRes int128_scomp64(Int128 op1, int64_t op2)
{
	// Check all zero
	if (!op2 && !op1.high && !op1.low) return CMP_EQ;

	bool lhs_sign = ISNEG(op1.high);
	bool rhs_sign = ISNEG(op2);
	if (lhs_sign != rhs_sign)
	{
		return lhs_sign ? CMP_LT : CMP_GT;
	}

	// Handle negative values
	if (lhs_sign)
	{
		// If this isn't a clean 11111... in the top bits, it's less than.
		if (op1.high != UINT64_MAX) return CMP_LT;
		if (op1.low == op2) return CMP_EQ;
		return ((int64_t)op1.low) > op2 ? CMP_GT : CMP_LT;
	}

	if (op1.high) return CMP_GT;
	if (op1.low == op2) return CMP_EQ;
	return op1.low > (uint64_t)op2 ? CMP_GT : CMP_LT;
}

CmpRes int128_ucomp64(Int128 op1, uint64_t op2)
{
	if (op1.high) return CMP_GT;
	if (op1.low == op2) return CMP_EQ;
	return op1.low > op2 ? CMP_GT : CMP_LT;
}

CmpRes i128_ucomp(Int128 op1, Int128 op2)
{
	if (op1.high > op2.high) return CMP_GT;
	if (op1.high < op2.high) return CMP_LT;
	if (op1.low == op2.low) return CMP_EQ;
	return op1.low > op2.low ? CMP_GT : CMP_LT;
}

Int128 i128_shl64(Int128 op1, uint64_t amount)
{
	if (amount == 0) return op1;
	if (amount > 127) return (Int128){ 0, 0 };
	if (amount == 64) return (Int128){ op1.low, 0 };
	if (amount > 64) return (Int128){ op1.low << (amount - 64), 0 };
	return (Int128){ (op1.high <<= amount) | op1.low >> (64 - amount), op1.low << amount };
}

Int128 i128_shl(Int128 op1, Int128 op)
{
	if (op.high) return (Int128){ 0, 0 };
	return i128_shl64(op1, op.low);
}

Int128 i128_lshr64(Int128 op1, uint64_t amount)
{
	if (amount == 0) return op1;
	if (amount > 127) return (Int128){ 0, 0 };
	if (amount == 64) return (Int128){ 0, op1.high };
	if (amount > 64) return (Int128){ 0, op1.high >> (amount - 64) };
	op1.low >>= amount;
	op1.low |= op1.high << (64 - amount);
	op1.high >>= amount;
	return op1;
}


Int128 i128_from_float_signed(Real d)
{
	return (Int128){ 0, (uint64_t)((int64_t)d) };
}

Int128 i128_from_float_unsigned(Real d)
{
	return (Int128){ 0, (uint64_t)d };
}

bool i128_get_bit(const Int128 *op, int bit)
{
	assert(bit < 128 && bit >= 0);
	if (bit > 63)
	{
		return (op->high >> (bit - 64)) & 1;
	}
	return (op->low >> (64 - bit)) & 1;
}

bool i128_is_zero(Int128 op)
{
	return op.high == 0 && op.low == 0;
}

Int128 i128_lshr(Int128 op1, Int128 op2)
{
	if (op2.high != 0) return (Int128){ 0, 0 };
	return i128_lshr64(op1, op2.low);
}

Int128 i128_ashr64(Int128 op1, uint64_t amount)
{
	if (!ISNEG(op1.high)) return i128_lshr64(op1, amount);
	if (amount > 127) return (Int128){ UINT64_MAX, UINT64_MAX };
	if (amount == 64) return (Int128){ UINT64_MAX, op1.high };
	if (amount > 64) return (Int128){ UINT64_MAX, (uint64_t)(((int64_t)op1.high) >> (amount - 64)) };
	return (Int128){ (uint64_t)(((int64_t)op1.high) >> amount), op1.low >> amount | (op1.high << (64 - amount)) };
}

Int128 i128_ashr(Int128 op1, Int128 op2)
{
	if (op2.high != 0) return ISNEG(op1.high) ? (Int128){ UINT64_MAX, UINT64_MAX } : (Int128){ 0, 0 };
	return i128_ashr64(op1, op2.low);
}

Int128 i128_add_swrap64(Int128 op1, int64_t op2, bool *wrapped)
{
	Int128 res = i128_add64(op1, (uint64_t) op2);
	bool is_less = i128_scomp(res, op1) == CMP_LT;
	*wrapped = op2 < 0 ? !is_less : is_less;
	return res;
}

Int128 i128_add_uwrap64(Int128 op1, uint64_t op2, bool *wrapped)
{
	Int128 res = i128_add64(op1, op2);
	*wrapped = i128_ucomp(res, op1) == CMP_LT;
	return res;
}

bool i128_is_neg(Int128 op)
{
	return ISNEG(op.high);
}

CmpRes i128_comp(Int128 op1, Int128 op2, Type *type)
{
	return type_is_signed(type) ? i128_scomp(op1, op2) : i128_ucomp(op1, op2);
}

CmpRes i128_scomp(Int128 op1, Int128 op2)
{
	bool lhs_sign = op1.high & ((uint64_t)INT64_MIN);
	bool rhs_sign = op2.high & ((uint64_t)INT64_MIN);
	if (lhs_sign != rhs_sign)
	{
		return lhs_sign ? CMP_LT : CMP_GT;
	}
	if (op1.high > op2.high) return CMP_GT;
	if (op1.high < op2.high) return CMP_LT;
	if (op1.low == op2.low) return CMP_EQ;
	return op1.low > op2.low ? CMP_GT : CMP_LT;
}

static uint32_t popcnt64(uint64_t n)
{
	n -= ((n >> 1) & 0x5555555555555555);
	n = (n & 0x3333333333333333) + ((n >> 2) & 0x3333333333333333);
	return (((n + (n >> 4)) & 0xF0F0F0F0F0F0F0F) * 0x101010101010101) >> 56;
}

uint32_t i128_popcnt(Int128 i)
{
	return popcnt64(i.high) + popcnt64(i.low);
}

static uint32_t ctz64(uint64_t n)
{
	uint64_t i = ~n;
	uint32_t c = ((i ^ (i + 1)) & i) >> 63;

	i = LO32(n) + 0xffffffff;
	i = ((i & 0x100000000) ^ 0x100000000) >> 27;
	c += i;
	n >>= i;

	i = (n & 0xffff) + 0xffff;
	i = ((i & 0x10000) ^ 0x10000) >> 12;
	c += i;
	n >>= i;

	i = (n & 0xff) + 0xff;
	i = ((i & 0x100) ^ 0x100) >> 5;
	c += i;
	n >>= i;

	i = (n & 0xf) + 0xf;
	i = ((i & 0x10) ^ 0x10) >> 2;
	c += i;
	n >>= i;

	i = (n & 3) + 3;
	i = ((i & 4) ^ 4) >> 1;
	c += i;
	n >>= i;

	c += ((n & 1) ^ 1);
	return c;
}

uint32_t i128_ctz(const Int128 *n)
{
	return !n->low ? ctz64(n->high) + 64 : ctz64(n->low);
}

static uint32_t clz64(uint64_t n)
{
	uint64_t neg_n = ~n;
	uint32_t c = ((neg_n ^ (neg_n + 1)) & neg_n) >> 63;

	neg_n = (n >> 32) + 0xffffffff;
	neg_n = ((neg_n & 0x100000000) ^ 0x100000000) >> 27;
	c += neg_n;
	n <<= neg_n;

	neg_n = (n >> 48) + 0xffff;
	neg_n = ((neg_n & 0x10000) ^ 0x10000) >> 12;
	c += neg_n;
	n <<= neg_n;

	neg_n = (n >> 56) + 0xff;
	neg_n = ((neg_n & 0x100) ^ 0x100) >> 5;
	c += neg_n;
	n <<= neg_n;

	neg_n = (n >> 60) + 0xf;
	neg_n = ((neg_n & 0x10) ^ 0x10) >> 2;
	c += neg_n;
	n <<= neg_n;

	neg_n = (n >> 62) + 3;
	neg_n = ((neg_n & 4) ^ 4) >> 1;
	c += neg_n;
	n <<= neg_n;

	c += (n >> 63) ^ 1;

	return c;
}

uint32_t i128_clz(const Int128 *op)
{
	return op->high ? clz64(op->high) : clz64(op->low) + 64;
}

int i128_lsb(const Int128 *op)
{
	return (int)(127 - i128_ctz(op));
}

int i128_msb(const Int128 *op)
{
	return (int)(127 - i128_clz(op));
}

Real i128_to_float(Int128 op)
{
	return (Real)op.low + (Real)ldexp((double)op.high, 64);
}

Real i128_to_float_signed(Int128 op)
{
	if ((int64_t)op.high < 0 && (op.high != INT128_MIN.high || op.low != INT128_MIN.low))
	{
		return -i128_to_float_signed(i128_neg(op));
	}
	return (Real)op.low + (Real)ldexp((double)op.high, 64);
}

void i128_udivrem(Int128 op1, Int128 op2, Int128 *div, Int128 *rem)
{
	*div = (Int128){ 0, 0 };
	int32_t shift = (int32_t)(i128_clz(&op2) - i128_clz(&op1));
	if (shift < 0)
	{
		*rem = op1;
		return;
	}
	op2 = i128_shl64(op2, (uint64_t)shift);
	do
	{
		*div = i128_shl64(*div, 1);
		if (i128_ucomp(op1, op2) != CMP_LT)
		{
			op1 = i128_sub(op1, op2);
			div->low |= 1;
		}
		op2 = i128_lshr64(op2, 1);

	} while (shift-- != 0);
	rem->high = op1.high;
	rem->low = op1.low;
}

Int128 i128_udiv(Int128 op1, Int128 op2)
{
	Int128 div, rem;
	i128_udivrem(op1, op2, &div, &rem);
	return div;
}

Int128 i128_urem(Int128 op1, Int128 op2)
{
	Int128 div, rem;
	i128_udivrem(op1, op2, &div, &rem);
	return rem;
}

Int128 i128_srem(Int128 op1, Int128 op2)
{
	uint64_t topbit1 = op1.high & 0x8000000000000000;
	uint64_t topbit2 = op2.high & 0x8000000000000000;
	if (topbit1) op1 = i128_neg(op1);
	if (topbit2) op2 = i128_neg(op2);
	Int128 res = i128_urem(op1, op2);
	if (topbit2 ^ topbit1)
	{
		return i128_neg(res);
	}
	return res;
}

Int128 i128_from_signed(int64_t i)
{
	return (Int128){ i < 0 ? UINT64_MAX : 0, (uint64_t)i };
}

Int128 i128_from_unsigned(uint64_t i)
{
	return (Int128){ 0, i };
}

Int128 i128_sdiv(Int128 op1, Int128 op2)
{
	uint64_t topbit1 = op1.high & 0x8000000000000000;
	uint64_t topbit2 = op2.high & 0x8000000000000000;
	if (topbit1) op1 = i128_neg(op1);
	if (topbit2) op2 = i128_neg(op2);
	Int128 res = i128_udiv(op1, op2);
	if (topbit2 ^ topbit1)
	{
		return i128_neg(res);
	}
	return res;
}

static CmpRes int_icompare(Int op1, int64_t op2)
{
	if (type_kind_is_signed(op1.type))
	{
		return i128_scomp(op1.i, i128_from_signed(op2));
	}
	if (op1.i.high || op2 < 0 || op1.i.low > op2) return CMP_GT;
	if (op1.i.low < op2) return CMP_LT;
	return CMP_EQ;
}

bool binary_op_matches_res(BinaryOp op, CmpRes res)
{
	switch (op)
	{
		case BINARYOP_GT:
			return res == CMP_GT;
		case BINARYOP_GE:
			return res != CMP_LT;
		case BINARYOP_LT:
			return res == CMP_LT;
		case BINARYOP_LE:
			return res != CMP_GT;
		case BINARYOP_NE:
			return res != CMP_EQ;
		case BINARYOP_EQ:
			return res == CMP_EQ;
		default:
			UNREACHABLE
	}
}


static CmpRes int_ucompare(Int op1, uint64_t op2)
{
	if (type_kind_is_signed(op1.type) && i128_is_neg(op1.i)) return CMP_LT;
	if (op1.i.high || op1.i.low > op2) return CMP_GT;
	if (op1.i.low < op2) return CMP_LT;
	return CMP_EQ;
}

static CmpRes int_compare(Int op1, Int op2)
{
	if (type_kind_is_signed(op1.type))
	{
		if (type_kind_is_signed(op2.type)) return i128_scomp(op1.i, op2.i);
		if (int_is_neg(op1)) return CMP_LT;
		return i128_ucomp(op1.i, op2.i);
	}
	if (int_is_neg(op2)) return CMP_GT;
	return i128_ucomp(op1.i, op2.i);
}

bool int_comp(Int op1, Int op2, BinaryOp op)
{
	return binary_op_matches_res(op, int_compare(op1, op2));
}

bool int_icomp(Int op1, int64_t op2, BinaryOp op)
{
	return binary_op_matches_res(op, int_icompare(op1, op2));
}

bool int_ucomp(Int op1, uint64_t op2, BinaryOp op)
{
	return binary_op_matches_res(op, int_ucompare(op1, op2));
}

bool int_fits(Int op1, TypeKind kind)
{
	Int128 min;
	Int128 max;
	bool is_signed = false;
	switch (kind)
	{
		case TYPE_I128:
			min = INT128_MIN;
			max = INT128_MAX;
			is_signed = true;
			break;
		case TYPE_I64:
			min = i128_from_signed(INT64_MIN);
			max = i128_from_signed(INT64_MAX);
			is_signed = true;
			break;
		case TYPE_I32:
			min = i128_from_signed(INT32_MIN);
			max = i128_from_signed(INT32_MAX);
			is_signed = true;
			break;
		case TYPE_I16:
			min = i128_from_signed(INT16_MIN);
			max = i128_from_signed(INT16_MAX);
			is_signed = true;
			break;
		case TYPE_I8:
			min = i128_from_signed(INT8_MIN);
			max = i128_from_signed(INT8_MAX);
			is_signed = true;
			break;
		case TYPE_U128:
			max = UINT128_MAX;
			break;
		case TYPE_U64:
			max = (Int128){ 0, UINT64_MAX };
			break;
		case TYPE_U32:
			max = (Int128){ 0, UINT32_MAX };
			break;
		case TYPE_U16:
			max = (Int128){ 0, UINT16_MAX };
			break;
		case TYPE_U8:
			max = (Int128){ 0, UINT8_MAX };
			break;
		default:
			UNREACHABLE
	}
	bool op_is_signed = type_kind_is_signed(op1.type);
	if (is_signed)
	{
		if (op_is_signed)
		{
			if (i128_scomp(op1.i, min) == CMP_LT) return false;
			if (i128_scomp(op1.i, max) == CMP_GT) return false;
			return true;
		}
		// In the unsigned case, we don't need to test the lower limit.
		return i128_ucomp(op1.i, max) != CMP_GT;
	}
	if (op_is_signed)
	{
		if (i128_is_neg(op1.i)) return false;
		if (i128_ucomp(op1.i, max) == CMP_GT) return false;
		return true;
	}
	// In the unsigned case, we don't need to test the lower limit.
	return i128_ucomp(op1.i, max) != CMP_GT;
}

uint64_t int_to_u64(Int op)
{
	return op.i.low;
}

int64_t int_to_i64(Int op)
{
	return (int64_t)op.i.low;
}

bool int_is_zero(Int op)
{
	return !op.i.high && !op.i.low;
}

Int int_add(Int op1, Int op2)
{
	assert(op1.type == op2.type);
	return (Int){ i128_extend(i128_add(op1.i, op2.i), op1.type), op1.type };
}

Int int_add64(Int op1, uint64_t op2)
{
	return (Int){ i128_extend(i128_add64(op1.i, op2), op1.type), op1.type };
}

Int int_sub(Int op1, Int op2)
{
	assert(op1.type == op2.type);
	return (Int){ i128_extend(i128_sub(op1.i, op2.i), op1.type), op1.type };
}

Int int_sub64(Int op1, uint64_t op2)
{
	return (Int){ i128_extend(i128_sub64(op1.i, op2), op1.type), op1.type };
}

Int int_mul(Int op1, Int op2)
{
	assert(op1.type == op2.type);
	return (Int){ i128_extend(i128_mult(op1.i, op2.i), op1.type), op1.type };
}

Int int_conv(Int op, TypeKind to_type)
{
	int bitsize = type_kind_bitsize(op.type);
	int to_bitsize = type_kind_bitsize(to_type);
	// Simple conversion, to 128 is a no op.
	if (to_bitsize == bitsize || to_bitsize == 128) return (Int){ op.i, to_type };

	bool from_signed = type_kind_is_signed(op.type);
	bool to_signed = type_kind_is_signed(to_type);
	if (bitsize < to_bitsize)
	{
		// Extending to a signed or with same signedness is a no-op.
		if (to_signed || to_signed == from_signed)
		{
			return (Int){ op.i, to_type };
		}
		// Extending from a signed to unsigned
		uint64_t shift = (uint64_t)(128 - to_bitsize);
		// Cut off the top of the signed bits
		// 11101 -> 11010 -> 01101
		return (Int){ i128_lshr64(i128_shl64(op.i, shift), shift), to_type };
	}
	// The other case is cutting down bits.
	uint64_t shift = (uint64_t)(128 - to_bitsize);
	Int128 without_top_bits = i128_lshr64(i128_shl64(op.i, shift), shift);
	if (to_signed)
	{
		return (Int){ i128_ashr64(i128_shl64(without_top_bits, shift), shift), to_type };
	}
	return (Int) { without_top_bits, to_type };
}

Int int_div(Int op1, Int op2)
{
	assert(op1.type == op2.type);
	Int128 res;
	if (type_kind_is_signed(op1.type))
	{
		res = i128_sdiv(op1.i, op2.i);
	}
	else
	{
		res = i128_udiv(op1.i, op2.i);
	}
	return (Int){ res, op1.type };
}

Int int_rem(Int op1, Int op2)
{
	assert(op1.type == op2.type);
	Int128 res;
	if (type_kind_is_signed(op1.type))
	{
		res = i128_srem(op1.i, op2.i);
	}
	else
	{
		res = i128_urem(op1.i, op2.i);
	}
	return (Int){ res, op1.type };
}

Int int_and(Int op1, Int op2)
{
	assert(op1.type == op2.type);
	return (Int){ i128_and(op1.i, op2.i), op1.type };
}

Int int_or(Int op1, Int op2)
{
	assert(op1.type == op2.type);
	return (Int){ i128_or(op1.i, op2.i), op1.type };
}

Int int_xor(Int op1, Int op2)
{
	assert(op1.type == op2.type);
	return (Int){ i128_xor(op1.i, op2.i), op1.type };
}

Int int_neg(Int op)
{
	return (Int){ i128_neg(op.i), op.type };
}

Int int_not(Int op)
{
	return (Int){ i128_extend(i128_not(op.i), op.type), op.type };
}

Int int_shr64(Int op1, uint64_t op2)
{
	if (type_kind_is_unsigned(op1.type))
	{
		return (Int){ i128_extend(i128_lshr64(op1.i, op2), op1.type), op1.type };
	}
	return (Int){ i128_extend(i128_ashr64(op1.i, op2), op1.type), op1.type };
}

Int int_shl64(Int op1, uint64_t op2)
{
	if (type_kind_is_unsigned(op1.type))
	{
		return (Int){ i128_extend(i128_shl64(op1.i, op2), op1.type), op1.type };
	}
	return (Int){ i128_extend(i128_shl64(op1.i, op2), op1.type), op1.type };
}

Real int_to_real(Int op)
{
	if (type_kind_is_signed(op.type))
	{
		return i128_to_float_signed(op.i);
	}
	return i128_to_float(op.i);
}

bool int_is_neg(Int op)
{
	if (type_kind_is_unsigned(op.type)) return false;
	return i128_is_neg(op.i);
}


bool i128_can_convert_from_double(double x)
{
	return isfinite(x)
	&& x > -1
	&& x < ldexp(1, 128);
}

bool i128_can_convert_from_double_signed(double x)
{
	return isfinite(x)
	&& x >= -ldexp(1, 127)
	&& x < ldexp(1, 127);
}

Int128 i128_from_double(double x)
{
	if (x >= ldexp(1, 64))
	{
		uint64_t hi = (uint64_t)ldexp(x, -64);
		uint64_t lo = (uint64_t)(x - ldexp((double)hi, 64));
		return (Int128){ hi, lo };
	}
	return i128_from_int((uint64_t)x);
}

Int128 i128_from_double_signed(double x)
{
	return x < 0 ? i128_neg(i128_from_signed((int64_t)-x)) : i128_from_int((uint64_t)x);
}
