#include "compiler_internal.h"

enum IntrospectIndex
{
	INTROSPECT_INDEX_KIND = 0,
	INTROSPECT_INDEX_SIZEOF = 1,
	INTROSPECT_INDEX_INNER = 2,
	INTROSPECT_INDEX_LEN = 3,
	INTROSPECT_INDEX_ADDITIONAL = 4,
	INTROSPECT_INDEX_TOTAL,
};

bool type_is_homogenous_aggregate(Type *type, Type **base, unsigned *elements);
static inline Type *type_reduced_from_expr(Expr *expr);
static inline bool abi_type_is_type(AbiType type);

static inline bool abi_type_is_valid(AbiType type);

static inline Type *type_lowering(Type *type)
{
	Type *canonical = type_flatten(type);
	if (canonical->type_kind == TYPE_ENUM) return canonical->decl->enums.type_info->type->canonical;
	if (canonical->type_kind == TYPE_TYPEID) return type_iptr->canonical;
	if (canonical->type_kind == TYPE_ANYERR) return type_iptr->canonical;
	if (canonical->type_kind == TYPE_FAULTTYPE) return type_iptr->canonical;
	if (canonical->type_kind == TYPE_BITSTRUCT) return type_lowering(canonical->decl->bitstruct.base_type->type);
	return canonical;
}

static inline Type *type_reduced_from_expr(Expr *expr)
{
	return type_lowering(expr->type);
}

static inline bool abi_type_is_type(AbiType type)
{
	return !(type.int_bits_plus_1 & 0x01);
}

static inline bool abi_type_is_valid(AbiType type)
{
	return type.int_bits_plus_1 != 0;
}


static inline bool abi_type_is_promotable_integer_or_bool(AbiType type)
{
	if (abi_type_is_type(type))
	{
		if (!type_is_integer_or_bool_kind(type.type)) return false;
		if (type.type == type_bool) return true;
		return type.type->builtin.bitsize < platform_target.width_c_int;
	}
	// We should only get npot or > big ints here.
	assert(!is_power_of_two(type.int_bits_plus_1 - 1) || type.int_bits_plus_1 < platform_target.width_c_int);
	return false;
}

