#include "compiler_internal.h"

enum IntrospectIndex
{
	INTROSPECT_INDEX_KIND = 0,
	INTROSPECT_INDEX_PARENTOF = 1,
	INTROSPECT_INDEX_DTABLE = 2,
	INTROSPECT_INDEX_SIZEOF = 3,
	INTROSPECT_INDEX_INNER = 4,
	INTROSPECT_INDEX_LEN = 5,
	INTROSPECT_INDEX_ADDITIONAL = 6,
	INTROSPECT_INDEX_TOTAL,
};

bool type_is_homogenous_aggregate(Type *type, Type **base, unsigned *elements);
static inline Type *type_reduced_from_expr(Expr *expr);
static inline bool abi_type_is_type(AbiType type);

static inline bool abi_type_is_valid(AbiType type);

static inline Type *type_lowering(Type *type)
{
	while (1)
	{
		type = type->canonical;
		switch (type->type_kind)
		{
			case TYPE_TYPEDEF:
				UNREACHABLE
			case TYPE_OPTIONAL:
				type = type->optional;
				continue;
			case TYPE_DISTINCT:
				type = type->decl->distinct->type;
				continue;
			case TYPE_ENUM:
				type = type->decl->enums.type_info->type;
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
			case TYPE_FAULTTYPE:
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
					case TYPE_VECTOR:
						return type_get_vector(flat, type->array.len);
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


UNUSED static inline bool abi_type_is_promotable_integer_or_bool(AbiType type)
{
	if (abi_type_is_type(type))
	{
		if (!type_is_integer_or_bool_kind(type.type)) return false;
		if (type.type == type_bool) return true;
		return type.type->builtin.bitsize < compiler.platform.width_c_int;
	}
	// We should only get npot or > big ints here.
	ASSERT0(!is_power_of_two(type.int_bits_plus_1 - 1) || type.int_bits_plus_1 < compiler.platform.width_c_int);
	return false;
}

static inline bool expr_is_vector_index(Expr *expr)
{
	return expr->expr_kind == EXPR_SUBSCRIPT
		   && type_lowering(exprtype(expr->subscript_expr.expr))->type_kind == TYPE_VECTOR;
}

const char *codegen_create_asm(Ast *ast);

extern const char *benchmark_fns_var_name;
extern const char *benchmark_names_var_name;
extern const char *test_fns_var_name;
extern const char *test_names_var_name;
