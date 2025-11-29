#include "compiler_internal.h"

typedef enum IntrospectIndex
{
	INTROSPECT_INDEX_KIND = 0,
	INTROSPECT_INDEX_PARENTOF = 1,
	INTROSPECT_INDEX_DTABLE = 2,
	INTROSPECT_INDEX_SIZEOF = 3,
	INTROSPECT_INDEX_INNER = 4,
	INTROSPECT_INDEX_LEN = 5,
	INTROSPECT_INDEX_ADDITIONAL = 6,
	INTROSPECT_INDEX_TOTAL,
} IntrospectIndex;

bool type_is_homogenous_aggregate(Type *type, Type **base, unsigned *elements);
static inline bool abi_type_is_type(AbiType type);

static inline bool abi_type_is_valid(AbiType type);


static inline LoweredType *type_lowering(Type *type)
{
	while (1)
	{
		type = type->canonical;
		switch (type->type_kind)
		{
			case TYPE_ALIAS:
				UNREACHABLE
			case TYPE_OPTIONAL:
				type = type->optional;
				continue;
			case TYPE_TYPEDEF:
				type = type->decl->distinct->type;
				continue;
			case TYPE_CONST_ENUM:
			case TYPE_ENUM:
				type = enum_inner_type(type);
				continue;
			case TYPE_FUNC_PTR:
			{
				Type *raw_func = type->pointer;
				if (raw_func->function.prototype && raw_func->function.prototype->raw_type == raw_func) return type;
				FunctionPrototype *proto = type_get_resolved_prototype(raw_func);
				return type_get_func_ptr(proto->raw_type);
			}
			case TYPE_INTERFACE:
				return type_any;
			case TYPE_ANYFAULT:
			case TYPE_TYPEID:
				return type_iptr->canonical;
			case TYPE_BITSTRUCT:
				type = type->decl->strukt.container_type->type;
				continue;
			case TYPE_WILDCARD:
				type = type_void;
				break;
			case TYPE_POINTER:
			{
				Type *pointer = type->pointer;
				Type *flat = type_lowering(pointer);
				if (flat == pointer) return type;
				return type_get_ptr(flat);
			}
			case TYPE_SLICE:
			case TYPE_ARRAY:
			case TYPE_VECTOR:
			case TYPE_SIMD_VECTOR:
			case TYPE_FLEXIBLE_ARRAY:
			{
				Type *arr_type = type->array.base;
				Type *flat = type_lowering(arr_type);
				if (flat == arr_type) return type;
				switch (type->type_kind)
				{
					case TYPE_SLICE:
						return type_get_slice(flat);
					case TYPE_ARRAY:
						return type_get_array(flat, type->array.len);
					case VECTORS:
						return type_get_vector_from_vector(flat, type);
					case TYPE_FLEXIBLE_ARRAY:
						return type_get_flexible_array(flat);
					default:
						UNREACHABLE
				}
			}
			default:
				return type;
		}
	}
}

static inline LoweredType *type_lowering_abi(Type *type)
{
	while (1)
	{
		type = type->canonical;
		switch (type->type_kind)
		{
			case TYPE_ALIAS:
				UNREACHABLE
			case TYPE_OPTIONAL:
				type = type->optional;
				continue;
			case TYPE_TYPEDEF:
				type = type->decl->distinct->type;
				continue;
			case TYPE_CONST_ENUM:
			case TYPE_ENUM:
				type = enum_inner_type(type);
				continue;
			case TYPE_FUNC_PTR:
			{
				Type *raw_func = type->pointer;
				if (raw_func->function.prototype && raw_func->function.prototype->raw_type == raw_func) return type;
				FunctionPrototype *proto = type_get_resolved_prototype(raw_func);
				return type_get_func_ptr(proto->raw_type);
			}
			case TYPE_INTERFACE:
				return type_any;
			case TYPE_ANYFAULT:
			case TYPE_TYPEID:
				return type_iptr->canonical;
			case TYPE_BITSTRUCT:
				type = type->decl->strukt.container_type->type;
				continue;
			case TYPE_WILDCARD:
				type = type_void;
				break;
			case TYPE_POINTER:
			{
				Type *pointer = type->pointer;
				Type *flat = type_lowering_abi(pointer);
				if (flat == pointer) return type;
				return type_get_ptr(flat);
			}
			case TYPE_SLICE:
			case TYPE_ARRAY:
			case TYPE_VECTOR:
			case TYPE_FLEXIBLE_ARRAY:
			case TYPE_SIMD_VECTOR:
			{
				Type *flat = type_lowering_abi(type->array.base);
				switch (type->type_kind)
				{
					case TYPE_SLICE:
						return type_get_slice(flat);
					case TYPE_SIMD_VECTOR:
						return type_get_vector(flat, TYPE_SIMD_VECTOR, type->array.len);
					case TYPE_ARRAY:
					case TYPE_VECTOR:
						return type_get_array(flat, type->array.len);
					case TYPE_FLEXIBLE_ARRAY:
						return type_get_flexible_array(flat);
					default:
						UNREACHABLE
				}
			}
			default:
				return type;
		}
	}
}

static inline bool abi_type_match(AbiType type, Type *other_type)
{
	other_type = other_type->canonical;
	if (type.abi_type & 0x01)
	{
		switch (type.abi_type)
		{
			case ABI_TYPE_INT_24:
			case ABI_TYPE_INT_40:
			case ABI_TYPE_INT_48:
			case ABI_TYPE_INT_56:
				return false;
			case ABI_TYPE_INT_VEC_2:
				return other_type == type_get_vector(type_uint, TYPE_SIMD_VECTOR, 2);
			case ABI_TYPE_INT_VEC_4:
				return other_type == type_get_vector(type_uint, TYPE_SIMD_VECTOR, 4);
			case ABI_TYPE_FLOAT_VEC_2:
				return other_type == type_get_vector(type_float, TYPE_SIMD_VECTOR, 2);
			case ABI_TYPE_FLOAT_VEC_4:
				return other_type == type_get_vector(type_float, TYPE_SIMD_VECTOR, 4);
			case ABI_TYPE_FLOAT16_VEC_2:
				return other_type == type_get_vector(type_float16, TYPE_SIMD_VECTOR, 2);
			case ABI_TYPE_FLOAT16_VEC_4:
				return other_type == type_get_vector(type_float16, TYPE_SIMD_VECTOR, 4);
			case ABI_TYPE_BFLOAT16_VEC_2:
				return other_type == type_get_vector(type_bfloat, TYPE_SIMD_VECTOR, 2);
			case ABI_TYPE_BFLOAT16_VEC_4:
				return other_type == type_get_vector(type_bfloat, TYPE_SIMD_VECTOR, 4);
			case ABI_TYPE_LONG_VEC_2:
				return other_type == type_get_vector(type_ulong, TYPE_SIMD_VECTOR, 2);
			case ABI_TYPE_DOUBLE_VEC_2:
				return other_type == type_get_vector(type_double, TYPE_SIMD_VECTOR, 2);
			case ABI_TYPE_DOUBLE_VEC_4:
				return other_type == type_get_vector(type_double, TYPE_SIMD_VECTOR, 4);
			case ABI_TYPE_DOUBLE_VEC_8:
				return other_type == type_get_vector(type_double, TYPE_SIMD_VECTOR, 8);
		}
		UNREACHABLE
	}
	return type.type == other_type->canonical;
}
static inline bool abi_type_is_type(AbiType type)
{
	return !(type.abi_type & 0x01);
}

static inline bool abi_type_is_valid(AbiType type)
{
	return type.abi_type != 0;
}


static inline bool expr_is_vector_index_or_swizzle(Expr *expr)
{
	return (expr->expr_kind == EXPR_SUBSCRIPT && type_kind_is_real_vector(type_lowering(exprtype(expr->subscript_expr.expr))->type_kind))
			|| (expr->expr_kind == EXPR_SWIZZLE && type_kind_is_real_vector(type_lowering(exprtype(expr->swizzle_expr.parent))->type_kind));
}

const char *codegen_create_asm(Ast *ast);

extern const char * const benchmark_fns_var_name;
extern const char * const benchmark_names_var_name;
extern const char * const test_fns_var_name;
extern const char * const test_names_var_name;

INLINE Type *lowered_member_type(Decl *member)
{
	return type_lowering_abi(member->type);
}

INLINE Type *lowered_array_element_type(Type *array_type)
{
	return type_lowering_abi(array_type->array.base);
}
