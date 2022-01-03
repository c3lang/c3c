#include "compiler_internal.h"

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
	if (canonical->type_kind == TYPE_ERRTYPE) return type_iptr->canonical;
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


