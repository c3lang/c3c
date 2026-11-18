// Scalars C may not have: 128-bit integers may be a struct in C and float16 and bfloat always are (see c_runtime.h),
// so their values only go through the runtime's helpers.

#include "c_codegen_internal.h"
#include <math.h>

// The runtime's name of float16 or bfloat, or NULL for other types.
static const char *c_half_name(Type *type)
{
	if (!type) return NULL;
	switch (c_safe_type_lower(type)->type_kind)
	{
		case TYPE_F16: return "f16";
		case TYPE_BF16: return "bf16";
		default: return NULL;
	}
}

bool c_type_is_half(Type *type)
{
	return c_half_name(type) != NULL;
}

// `x` of `type` as a double when `type` is float16 or bfloat, otherwise `x`.
const char *c_half_to_f64_str(Type *type, const char *x)
{
	const char *name = c_half_name(type);
	return name ? str_printf("__c3_%s_to_f64(%s)", name, x) : x;
}

// The double `x` as `type` when that is float16 or bfloat, otherwise `x`.
const char *c_half_from_f64_str(Type *type, const char *x)
{
	const char *name = c_half_name(type);
	return name ? str_printf("__c3_f64_to_%s(%s)", name, x) : x;
}

// The bits of the float constant `f`, exact in `type`.
uint64_t c_float_const_bits(Type *type, Real f)
{
	double d = (double)f;
	switch (c_safe_type_lower(type)->type_kind)
	{
		case TYPE_F32:
		{
			float f32 = (float)d;
			uint32_t u;
			memcpy(&u, &f32, 4);
			return u;
		}
		case TYPE_F16:
		case TYPE_BF16:
			break;
		default:
		{
			uint64_t u;
			memcpy(&u, &d, 8);
			return u;
		}
	}
	int mant      = c_safe_type_lower(type)->type_kind == TYPE_F16 ? 10 : 7;
	int max_exp   = (1 << (15 - mant)) - 1;
	int bias      = max_exp >> 1;
	uint64_t sign = signbit(d) ? 0x8000 : 0;
	d             = fabs(d);
	if (isnan(d)) return sign | (uint64_t)max_exp << mant | 1ULL << (mant - 1);
	if (isinf(d)) return sign | (uint64_t)max_exp << mant;
	if (d == 0) return sign;
	int e;
	double m = frexp(d, &e); // d = m * 2^e, 0.5 <= m < 1
	int biased = e - 1 + bias;
	if (biased < 1) return sign | (uint64_t)ldexp(d, bias - 1 + mant);
	return sign | (uint64_t)biased << mant | ((uint64_t)ldexp(m, mant + 1) & ((1ULL << mant) - 1));
}

bool c_type_is_int128(Type *type)
{
	if (!type) return false;
	TypeKind kind = c_safe_type_lower(type)->type_kind;
	return kind == TYPE_I128 || kind == TYPE_U128;
}

// The unsigned type `+ - * <<` and negation of `type` are done in, or NULL when C's own operators wrap. Signed
// overflow is undefined in C but wraps in C3, and C promotes the types narrower than int to a signed int.
static const char *c_int_wrap_type(Type *type)
{
	Type *flat = c_safe_type_lower(type);
	if (!type_is_integer(flat) || (type_is_unsigned(flat) && type_size(flat) >= 4)) return NULL;
	return type_size(flat) > 4 ? "uint64_t" : "uint32_t";
}

INLINE bool c_type_is_i128(Type *type)
{
	return c_safe_type_lower(type)->type_kind == TYPE_I128;
}

const char *c_int_value_str(GenContext *c, Type *type, int64_t value)
{
	if (c_type_is_half(type)) return c_half_from_f64_str(type, str_printf("(double)%" PRId64 "LL", value));
	if (!c_type_is_int128(type)) return str_printf("(%s)%" PRId64 "LL", c_type_name(c, type), value);
	const char *wide = str_printf("__c3_i128_from_i64(%" PRId64 "LL)", value);
	return c_type_is_i128(type) ? wide : str_printf("__c3_i128_as_u128(%s)", wide);
}

// `expr` of the scalar type `from` converted to `to`, as LLVM's int casts: truncation keeps the low bits, extension
// follows the signedness of `from`.
const char *c_scalar_convert_str(GenContext *c, Type *to, Type *from, const char *expr)
{
	to   = c_safe_type_lower(to);
	from = c_safe_type_lower(from);
	const char *tname = c_type_name(c, to);
	if (strcmp(tname, c_type_name(c, from)) == 0) return expr;
	if (c_type_is_half(from) || c_type_is_half(to))
	{
		// Through double, which holds every float16 and bfloat.
		if (c_type_is_half(from))
		{
			expr = c_half_to_f64_str(from, expr);
			from = type_double;
		}
		if (!c_type_is_half(to)) return c_scalar_convert_str(c, to, from, expr);
		if (type_is_float(from)) return c_half_from_f64_str(to, str_printf("(double)%s", expr));
		// An integer may not fit in a double, which would round it twice.
		bool is_signed = type_is_signed(from);
		if (c_type_is_int128(from)) return c_half_from_f64_str(to, str_printf("__c3_%c128_to_f64_sticky(%s)", is_signed ? 'i' : 'u', expr));
		return c_half_from_f64_str(to, str_printf(is_signed ? "__c3_i64_to_f64_sticky((int64_t)%s)" : "__c3_u64_to_f64_sticky((uint64_t)%s)", expr));
	}
	bool to_wide = c_type_is_int128(to);
	if (!to_wide && !c_type_is_int128(from))
	{
		bool through_int = c_type_is_c_pointer(to) != c_type_is_c_pointer(from);
		return str_printf("(%s)%s%s", tname, through_int ? "(uintptr_t)" : "", expr);
	}
	if (to_wide && c_type_is_int128(from))
	{
		return str_printf(c_type_is_i128(to) ? "__c3_u128_as_i128(%s)" : "__c3_i128_as_u128(%s)", expr);
	}
	if (!to_wide)
	{
		if (to->type_kind == TYPE_BOOL) return str_printf("__c3_u128_nonzero(%s)", expr);
		if (type_is_float(to))
		{
			bool is_double = to->type_kind == TYPE_F64 || to->type_kind == TYPE_F128;
			return str_printf("(%s)__c3_%c128_to_%s(%s)", tname, c_type_is_i128(from) ? 'i' : 'u', is_double ? "f64" : "f32", expr);
		}
		return str_printf("(%s)%s__c3_u128_lo(%s)", tname, c_type_is_c_pointer(to) ? "(uintptr_t)" : "", expr);
	}
	if (type_is_float(from)) return str_printf(c_type_is_i128(to) ? "__c3_f64_to_i128((double)%s)" : "__c3_f64_to_u128((double)%s)", expr);
	bool from_signed = type_is_signed(from) && !c_type_is_c_pointer(from);
	const char *wide = from_signed ? str_printf("__c3_i128_from_i64((int64_t)%s)", expr)
	                               : str_printf("__c3_u128_from_u64((uint64_t)%s%s)", c_type_is_c_pointer(from) ? "(uintptr_t)" : "", expr);
	if (from_signed == c_type_is_i128(to)) return wide;
	return str_printf(from_signed ? "__c3_i128_as_u128(%s)" : "__c3_u128_as_i128(%s)", wide);
}

// `l op r` for scalars of `type`. A shift amount `r` is a uint64_t (c_shift_amount_str). Vector comparisons give
// -1 / 0 lanes.
const char *c_binary_str(BinaryOp op, Type *type, const char *l, const char *r)
{
	if (op >= BINARYOP_VEC_GT && op <= BINARYOP_VEC_EQ)
	{
		return str_printf("%s ? -1 : 0", c_binary_str(op - BINARYOP_VEC_GT + BINARYOP_GT, type, l, r));
	}
	if (c_type_is_half(type))
	{
		const char *result = c_binary_str(op, type_double, c_half_to_f64_str(type, l), c_half_to_f64_str(type, r));
		return op >= BINARYOP_GT && op <= BINARYOP_EQ ? result : c_half_from_f64_str(type, result);
	}
	// C has no `%` on floating point.
	if (op == BINARYOP_MOD && type_is_float(c_safe_type_lower(type))) return str_printf("__c3_fmod(%s, %s)", l, r);
	if (!c_type_is_int128(type))
	{
		if (op != BINARYOP_ADD && op != BINARYOP_SUB && op != BINARYOP_MULT && op != BINARYOP_SHL) return str_printf("(%s %s %s)", l, c_binary_op_str(op), r);
		const char *wide = c_int_wrap_type(type);
		if (!wide) return str_printf("(%s %s %s)", l, c_binary_op_str(op), r);
		const char *rhs = op == BINARYOP_SHL ? r : str_printf("(%s)%s", wide, r);
		return str_printf("((%s)((%s)%s %s %s))", c_type_name(NULL, type), wide, l, c_binary_op_str(op), rhs);
	}
	char s = c_type_is_i128(type) ? 'i' : 'u';
	switch (op)
	{
		case BINARYOP_ADD: return str_printf("__c3_u128_add(%s, %s)", l, r);
		case BINARYOP_SUB: return str_printf("__c3_u128_sub(%s, %s)", l, r);
		case BINARYOP_MULT: return str_printf("__c3_u128_mul(%s, %s)", l, r);
		case BINARYOP_DIV: return str_printf(s == 'i' ? "__c3_i128_sdiv(%s, %s)" : "__c3_u128_udiv(%s, %s)", l, r);
		case BINARYOP_MOD: return str_printf(s == 'i' ? "__c3_i128_smod(%s, %s)" : "__c3_u128_umod(%s, %s)", l, r);
		case BINARYOP_BIT_AND: return str_printf("__c3_u128_and(%s, %s)", l, r);
		case BINARYOP_BIT_OR: return str_printf("__c3_u128_or(%s, %s)", l, r);
		case BINARYOP_BIT_XOR: return str_printf("__c3_u128_xor(%s, %s)", l, r);
		case BINARYOP_SHL: return str_printf("__c3_u128_shl(%s, (unsigned)%s)", l, r);
		case BINARYOP_SHR: return str_printf(s == 'i' ? "__c3_i128_ashr(%s, (unsigned)%s)" : "__c3_u128_lshr(%s, (unsigned)%s)", l, r);
		case BINARYOP_EQ: return str_printf("__c3_u128_eq(%s, %s)", l, r);
		case BINARYOP_NE: return str_printf("!__c3_u128_eq(%s, %s)", l, r);
		case BINARYOP_LT: return str_printf("__c3_%c128_%clt(%s, %s)", s, s == 'i' ? 's' : 'u', l, r);
		case BINARYOP_LE: return str_printf("__c3_%c128_%cle(%s, %s)", s, s == 'i' ? 's' : 'u', l, r);
		case BINARYOP_GT: return str_printf("__c3_%c128_%clt(%s, %s)", s, s == 'i' ? 's' : 'u', r, l);
		case BINARYOP_GE: return str_printf("__c3_%c128_%cle(%s, %s)", s, s == 'i' ? 's' : 'u', r, l);
		default: UNREACHABLE
	}
}

// `x` as a truth value, or its negation.
const char *c_truth_str(Type *type, const char *x, bool negate)
{
	if (c_type_is_int128(type)) return str_printf("%s__c3_u128_nonzero(%s)", negate ? "!" : "", x);
	if (c_type_is_half(type)) return c_truth_str(type_double, c_half_to_f64_str(type, x), negate);
	return str_printf("%s%s", negate ? "!" : "!!", x);
}

const char *c_int_neg_str(Type *type, const char *x)
{
	if (c_type_is_int128(type)) return str_printf("__c3_u128_neg(%s)", x);
	if (c_type_is_half(type)) return c_half_from_f64_str(type, str_printf("(-%s)", c_half_to_f64_str(type, x)));
	const char *wide = c_int_wrap_type(type);
	if (!wide) return str_printf("(-%s)", x);
	return str_printf("((%s)((%s)0 - (%s)%s))", c_type_name(NULL, type), wide, wide, x);
}

const char *c_int_limit_str(GenContext *c, Type *type, bool is_max)
{
	bool is_signed = type_is_signed(type);
	if (c_type_is_int128(type))
	{
		if (!is_signed) return is_max ? "__c3_u128_not(__c3_u128_from_u64(0))" : "__c3_u128_from_u64(0)";
		return is_max ? "__c3_u128_as_i128(__c3_u128_make(INT64_MAX, UINT64_MAX))" : "__c3_u128_as_i128(__c3_u128_make((uint64_t)1 << 63, 0))";
	}
	const char *tn = c_type_name(c, type);
	int bits       = (int)type_size(type) * 8;
	if (!is_signed) return is_max ? str_printf("(%s)(~0ULL)", tn) : str_printf("(%s)0", tn);
	return is_max ? str_printf("(%s)(((uint64_t)1 << (%d - 1)) - 1)", tn, bits) : str_printf("(%s)(~(((uint64_t)1 << (%d - 1)) - 1))", tn, bits);
}

// A shift amount `expr` of `type` as a uint64_t, saturated when it doesn't fit.
const char *c_shift_amount_str(Type *type, const char *expr)
{
	if (!c_type_is_int128(type)) return str_printf("(uint64_t)%s", expr);
	return str_printf("(__c3_u128_hi(%s) ? UINT64_MAX : __c3_u128_lo(%s))", expr, expr);
}

// The checked add, sub or mult of `type`, called as `name(a, b, &result)`.
const char *c_overflow_builtin_name(BinaryOp op, Type *type)
{
	const char *name = op == BINARYOP_ADD ? "add" : (op == BINARYOP_SUB ? "sub" : "mul");
	if (!c_type_is_int128(type)) return str_printf("__builtin_%s_overflow", name);
	return str_printf("__c3_%c128_%s_overflow", c_type_is_i128(type) ? 'i' : 'u', name);
}
