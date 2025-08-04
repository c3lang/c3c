// Copyright (c) 2020-2025 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "sema_internal.h"


static inline bool sema_resolve_ptr_type(SemaContext *context, TypeInfo *type_info, ResolveTypeKind resolve_kind);
static inline bool sema_resolve_array_type(SemaContext *context, TypeInfo *type, ResolveTypeKind resolve_kind);
static inline bool sema_resolve_type(SemaContext *context, TypeInfo *type_info, ResolveTypeKind resolve_kind);
static bool sema_resolve_type_identifier(SemaContext *context, TypeInfo *type_info, ResolveTypeKind resolve_type_kind);
INLINE bool sema_resolve_vatype(SemaContext *context, TypeInfo *type_info);
INLINE bool sema_resolve_evaltype(SemaContext *context, TypeInfo *type_info, ResolveTypeKind resolve_kind);
INLINE bool sema_resolve_typefrom(SemaContext *context, TypeInfo *type_info, ResolveTypeKind resolve_kind);
INLINE bool sema_resolve_typeof(SemaContext *context, TypeInfo *type_info);
static int compare_function(Signature *sig, FunctionPrototype *proto);

static inline bool sema_resolve_ptr_type(SemaContext *context, TypeInfo *type_info, ResolveTypeKind resolve_kind)
{
	// Try to resolve this type shallowly.
	if (!sema_resolve_type(context, type_info->pointer, resolve_kind))
	{
		return type_info_poison(type_info);
	}
	// Construct the type after resolving the underlying type.
	type_info->type = type_get_ptr(type_info->pointer->type);
	type_info->resolve_status = RESOLVE_DONE;
	return true;
}

bool sema_resolve_type_info(SemaContext *context, TypeInfo *type_info, ResolveTypeKind kind)
{
	return sema_resolve_type(context, type_info, kind);
}

bool sema_resolve_array_like_len(SemaContext *context, TypeInfo *type_info, ArraySize *len_ref)
{
	// Get the expression describing the length.
	Expr *len_expr = type_info->array.len;

	// Analyse it.
	if (!sema_analyse_expr(context, len_expr)) return type_info_poison(type_info);

	if (!cast_to_index_len(context, len_expr, true)) return type_info_poison(type_info);

	// A constant expression is assumed.
	if (!sema_cast_const(len_expr))
	{
		SEMA_ERROR(len_expr, "Expected a constant value as length.");
		return type_info_poison(type_info);
	}

	// The constant must be an integer (and not just a distinct integer)
	if (!type_is_integer(len_expr->type->canonical))
	{
		SEMA_ERROR(len_expr, "Expected an integer value.");
		return type_info_poison(type_info);
	}

	bool is_vector = type_info->kind == TYPE_INFO_VECTOR;

	// Check the length:
	Int len = len_expr->const_expr.ixx;

	// Is it negative?
	if (int_is_neg(len))
	{
		SEMA_ERROR(len_expr,
				   is_vector ? "A vector may not have a negative width." :
				   "An array may not have a negative length.");
		return type_info_poison(type_info);
	}

	// Is it zero?
	if (int_is_zero(len))
	{
		SEMA_ERROR(len_expr,
				   is_vector ? "A vector may not have a zero width."
				   : "An array may not have zero length.");
		return type_info_poison(type_info);
	}

	// Check max values.
	if (int_icomp(len, is_vector ? compiler.build.max_vector_size / 8 : MAX_ARRAY_SIZE, BINARYOP_GT))
	{
		if (is_vector)
		{
			RETURN_VAL_SEMA_ERROR(type_info_poison(type_info), len_expr, "A vector may not exceed %d in bit width.", compiler.build.max_vector_size);
		}
		else
		{
			RETURN_VAL_SEMA_ERROR(type_info_poison(type_info), len_expr, "The array length may not exceed %lld.", MAX_ARRAY_SIZE);
		}
	}
	// We're done, return the size and mark it as a success.
	*len_ref = (ArraySize)len.i.low;
	return true;
}

static inline bool sema_check_array_type(SemaContext *context, TypeInfo *original_info, Type *base, TypeInfoKind kind, ArraySize len, Type **result_ref)
{
	Type *distinct_base = type_flatten(base);

	// We don't want to allow arrays with flexible members
	if (distinct_base->type_kind == TYPE_STRUCT)
	{
		// If the struct is resolved, we can check immediately
		if (distinct_base->decl->resolve_status == RESOLVE_DONE)
		{
			if (distinct_base->decl->has_variable_array)
			{
				RETURN_SEMA_ERROR(original_info, "Arrays of structs with flexible array members is not allowed.");
			}
		}
		else
		{
			// Otherwise we have to defer it:
			vec_add(context->unit->check_type_variable_array, original_info);
		}
	}
	switch (kind)
	{
		case TYPE_INFO_SLICE:
			if (!type_is_valid_for_array(base))
			{
				RETURN_SEMA_ERROR(original_info,
				                  "You cannot form a slice with elements of type %s.",
				                  type_quoted_error_string(base));
			}
			*result_ref = type_get_slice(base);
			break;
		case TYPE_INFO_INFERRED_ARRAY:
			if (!type_is_valid_for_array(base))
			{
				RETURN_SEMA_ERROR(original_info,
				                  "You cannot form an array with elements of type %s.",
				                  type_quoted_error_string(base));
			}
			*result_ref = type_get_inferred_array(base);
			break;
		case TYPE_INFO_INFERRED_VECTOR:
			if (!type_is_valid_for_vector(base))
			{
				RETURN_SEMA_ERROR(original_info,
				                  "You cannot form a vector with elements of type %s.",
				                  type_quoted_error_string(base));
			}
			*result_ref = type_get_inferred_vector(base);
			break;
		case TYPE_INFO_VECTOR:
			if (!type_is_valid_for_vector(base))
			{
				RETURN_SEMA_ERROR(original_info, "You cannot form a vector with elements of type %s.", type_quoted_error_string(base));
			}
			*result_ref = type_get_vector(base, len);
			break;
		case TYPE_INFO_ARRAY:
			if (!type_is_valid_for_array(base))
			{
				RETURN_SEMA_ERROR(original_info,
				                  "You cannot form an array with elements of type %s.",
				                  type_quoted_error_string(base));
			}
			*result_ref = type_get_array(base, len);
			break;
		default:
			UNREACHABLE
	}
	return true;
}

static inline bool sema_resolve_array_type(SemaContext *context, TypeInfo *type, ResolveTypeKind resolve_kind)
{
	// Check the underlying type
	if (!sema_resolve_type(context, type->array.base, resolve_kind)) return type_info_poison(type);

	ArraySize len;
	TypeInfoKind kind = type->kind;
	switch (kind)
	{
		case TYPE_INFO_ARRAY:
		case TYPE_INFO_VECTOR:
			if (!sema_resolve_array_like_len(context, type, &len)) return type_info_poison(type);
			break;
		default:
			len = 0;
			break;
	}
	if (!sema_check_array_type(context, type, type->array.base->type, kind, len, &type->type)) return type_info_poison(type);
	ASSERT(!type->array.len || sema_cast_const(type->array.len));
	type->resolve_status = RESOLVE_DONE;
	if (kind == TYPE_INFO_VECTOR)
	{
		if (kind == TYPE_INFO_VECTOR && type_size(type->type) > compiler.build.max_vector_size / 8)
		{
			RETURN_SEMA_ERROR(type, "Vectors with bitsize over %u are not supported (this vector is %llu bits), "
						   "but you can increase the maximum allowed using '--max-vector-size'.",
						   compiler.build.max_vector_size, (unsigned long long)type_size(type->type) * 8);
		}
	}
	return true;

}


static bool sema_resolve_type_identifier(SemaContext *context, TypeInfo *type_info, ResolveTypeKind resolve_type_kind)
{
	if (type_info->unresolved.name == type_string->name && !type_info->unresolved.path)
	{
		type_info->type = type_string;
		type_info->resolve_status = RESOLVE_DONE;
		return true;
	}
	Decl *decl = sema_resolve_symbol(context, type_info->unresolved.name, type_info->unresolved.path, type_info->span);

	// Already handled
	if (!decl) return type_info_poison(type_info);

	decl = decl_flatten(decl);
	switch (decl->decl_kind)
	{
		case DECL_ERASED:
			UNREACHABLE
		case DECL_STRUCT:
		case DECL_BITSTRUCT:
		case DECL_UNION:
		case DECL_ENUM:
		case DECL_INTERFACE:
			type_info->type = decl->type;
			type_info->resolve_status = RESOLVE_DONE;
			DEBUG_LOG("Resolved %s.", type_info->unresolved.name);
			return true;
		case DECL_FNTYPE:
			type_info->type = decl->type;
			type_info->resolve_status = RESOLVE_DONE;
			return true;
		case DECL_CONST_ENUM:
		case DECL_DISTINCT:
			if (resolve_type_kind & RESOLVE_TYPE_NO_CHECK_DISTINCT)
			{
				type_info->type = decl->type;
				type_info->resolve_status = RESOLVE_DONE;
				return true;
			}
			FALLTHROUGH;
		case DECL_TYPEDEF:
			if (!sema_analyse_decl(context, decl)) return type_info_poison(type_info);
			type_info->type = decl->type;
			assert (type_info->type->canonical->type_kind != TYPE_TYPEDEF);
			type_info->resolve_status = RESOLVE_DONE;
			return true;
		case DECL_POISONED:
			return type_info_poison(type_info);
		case DECL_VAR:
			if (decl->var.kind == VARDECL_PARAM_CT_TYPE || decl->var.kind == VARDECL_LOCAL_CT_TYPE)
			{
				decl->var.is_read = true;
				Expr *init_expr = decl->var.init_expr;
				if (!init_expr)
				{
					RETURN_SEMA_ERROR(type_info, "You need to assign a type to '%s' before using it.", decl->name);
				}
				ASSERT_SPAN(init_expr, expr_is_const_typeid(init_expr));
				ASSERT_SPAN(init_expr, init_expr->resolve_status == RESOLVE_DONE);
				type_info->type = init_expr->const_expr.typeid;
				return true;
			}
			FALLTHROUGH;
		case DECL_ALIAS:
		case DECL_ALIAS_PATH:
		case DECL_ATTRIBUTE:
		case DECL_ENUM_CONSTANT:
		case DECL_FAULT:
		case DECL_FUNC:
		case DECL_IMPORT:
		case DECL_LABEL:
		case DECL_MACRO:
			SEMA_ERROR(type_info, "This is not a type.");
			return type_info_poison(type_info);
		case DECL_BODYPARAM:
		case DECL_CT_ASSERT:
		case DECL_CT_ECHO:
		case DECL_CT_EXEC:
		case DECL_CT_INCLUDE:
		case DECL_DECLARRAY:
		case DECL_GROUP:
			UNREACHABLE
	}
	UNREACHABLE

}


// $evaltype("Foo")
INLINE bool sema_resolve_evaltype(SemaContext *context, TypeInfo *type_info, ResolveTypeKind resolve_kind)
{
	SEMA_DEPRECATED(type_info, "$evaltype is deprecated, use $typefrom instead.");
	Expr *expr = type_info->unresolved_type_expr;
	Expr *inner = sema_ct_eval_expr(context, true, expr, true);
	if (!inner || !expr_ok(inner)) return type_info_poison(type_info);
	if (inner->expr_kind != EXPR_TYPEINFO)
	{
		SEMA_ERROR(expr, "Only type names may be resolved with $evaltype.");
		return false;
	}
	TypeInfo *inner_type = inner->type_expr;
	if (!sema_resolve_type(context, inner_type, resolve_kind)) return false;
	switch (sema_resolve_storage_type(context, inner_type->type))
	{
		case STORAGE_ERROR:
			return false;
		case STORAGE_VOID:
		case STORAGE_UNKNOWN:
		case STORAGE_NORMAL:
			type_info->type = inner_type->type;
			return true;
		case STORAGE_WILDCARD:
			RETURN_SEMA_ERROR(expr, "$evaltype failed to resolve this to a definite type.");
		case STORAGE_COMPILE_TIME:
			RETURN_SEMA_ERROR(expr, "$evaltype does not support compile-time types.");
	}
	UNREACHABLE
}

// $typeof(...)
INLINE bool sema_resolve_typeof(SemaContext *context, TypeInfo *type_info)
{
	Expr *expr = type_info->unresolved_type_expr;
	bool in_no_eval = context->call_env.in_no_eval;
	context->call_env.in_no_eval = true;
	bool success = sema_analyse_expr_value(context, expr);
	context->call_env.in_no_eval = in_no_eval;
	if (!success) return false;
	Type *expr_type = expr->type;
	if (expr_type->type_kind == TYPE_FUNC_RAW) expr_type = type_get_func_ptr(expr_type);
	switch (sema_resolve_storage_type(context, expr_type))
	{
		case STORAGE_ERROR:
			return false;
		case STORAGE_NORMAL:
		case STORAGE_VOID:
		case STORAGE_UNKNOWN:
			type_info->type = expr_type;
			return true;
		case STORAGE_WILDCARD:
			if (expr_type->type_kind == TYPE_OPTIONAL)
			{
				type_info->type = type_get_optional(type_void);
				return true;
			}
			RETURN_SEMA_ERROR(expr, "This %sexpression lacks a concrete type.", type_is_optional(expr_type) ? "optional " : "");
		case STORAGE_COMPILE_TIME:
			type_info->type = type_untypedlist;
			return true;
	}
	UNREACHABLE
}

INLINE bool sema_resolve_typefrom(SemaContext *context, TypeInfo *type_info, ResolveTypeKind resolve_kind)
{
	Expr *expr = type_info->unresolved_type_expr;
	if (!sema_analyse_expr(context, expr)) return false;
	if (!sema_cast_const(expr))
	{
		RETURN_SEMA_ERROR(expr, "Expected a constant value.");
	}
	switch (expr->const_expr.const_kind)
	{
		case CONST_TYPEID:
			type_info->type = expr->const_expr.typeid;
			return true;
		case CONST_STRING:
			break;
		default:
			RETURN_SEMA_ERROR(expr, "Expected a constant string or typeid value.");
	}

	const char *bytes = expr->const_expr.bytes.ptr;
	ArraySize len = expr->const_expr.bytes.len;
	Expr *typefrom = sema_resolve_string_ident(context, expr, true);
	if (!typefrom || !expr_ok(typefrom)) return false;
	if (typefrom->expr_kind != EXPR_TYPEINFO)
	{
		RETURN_SEMA_ERROR(expr, "Expected a type, not a regular identifier '%.*s'.", (int)len, bytes);
	}
	TypeInfo *info = typefrom->type_expr;
	if (!sema_resolve_type(context, info, resolve_kind)) return false;
	switch (sema_resolve_storage_type(context, info->type))
	{
		case STORAGE_ERROR:
			return false;
		case STORAGE_VOID:
		case STORAGE_UNKNOWN:
		case STORAGE_NORMAL:
			type_info->type = info->type;
			return true;
		case STORAGE_WILDCARD:
			RETURN_SEMA_ERROR(expr, "$typefrom failed to resolve \"%.*s\" to a definite type.", (int)len, bytes);
		case STORAGE_COMPILE_TIME:
			RETURN_SEMA_ERROR(expr, "$typefrom does not support compile-time types.");
	}
	UNREACHABLE
}

// $vatype(...)
INLINE bool sema_resolve_vatype(SemaContext *context, TypeInfo *type_info)
{
	if (!context->current_macro)
	{
		RETURN_SEMA_ERROR(type_info, "'%s' can only be used inside of a macro.", token_type_to_string(TOKEN_CT_VATYPE));
	}
	ASSIGN_EXPR_OR_RET(Expr *arg_expr, sema_expr_analyse_ct_arg_index(context, type_info->unresolved_type_expr, NULL), false);
	if (!sema_analyse_expr_value(context, arg_expr)) return false;
	if (arg_expr->expr_kind != EXPR_TYPEINFO) RETURN_SEMA_ERROR(arg_expr, "The argument was not a type.");
	type_info->type = arg_expr->type_expr->type;
	return true;
}

bool sema_unresolved_type_is_generic(SemaContext *context, TypeInfo *type_info)
{
	RETRY:
	if (type_info->kind == TYPE_INFO_GENERIC) return true;
	if (type_info->resolve_status == RESOLVE_DONE) return false;
	if (type_info->kind != TYPE_INFO_IDENTIFIER) return false;
	if (type_info->subtype != TYPE_COMPRESSED_NONE) return false;
	Decl *decl = sema_find_path_symbol(context, type_info->unresolved.name, type_info->unresolved.path);
	if (!decl) return false;
	if (decl->decl_kind != DECL_TYPEDEF) return false;
	if (decl->resolve_status == RESOLVE_DONE) return false;
	if (decl->type_alias_decl.is_func) return false;
	type_info = decl->type_alias_decl.type_info;
	goto RETRY;
}

// Foo{...}
INLINE bool sema_resolve_generic_type(SemaContext *context, TypeInfo *type_info)
{
	TypeInfo *inner = type_info->generic.base;
	if (inner->kind != TYPE_INFO_IDENTIFIER || inner->subtype != TYPE_COMPRESSED_NONE || inner->optional)
	{
		RETURN_SEMA_ERROR(inner, "Parameterization required a concrete type name here.");
	}
	if (inner->resolve_status == RESOLVE_DONE)
	{
		if (!type_is_user_defined(inner->type))
		{
			RETURN_SEMA_ERROR(inner, "A user defined type was expected here, not %s.", type_quoted_error_string(inner->type));
		}
	}
	ASSERT_SPAN(inner, inner->resolve_status == RESOLVE_NOT_DONE);

	bool was_recursive = false;
	Decl *type = sema_analyse_parameterized_identifier(context, inner->unresolved.path, inner->unresolved.name,
	                                                   inner->span, type_info->generic.params, &was_recursive);
	if (!decl_ok(type)) return false;
	if (!sema_analyse_decl(context, type)) return false;
	type_info->type = type->type;
	if (!was_recursive) return true;
	if (!context->current_macro && (context->call_env.kind == CALL_ENV_FUNCTION || context->call_env.kind == CALL_ENV_FUNCTION_STATIC)
	    && !context->call_env.current_function->func_decl.in_macro)
	{
		RETURN_SEMA_ERROR(type_info, "Recursively generic type declarations are only allowed inside of macros. Use `alias` to define an alias for the type instead.");
	}
	return true;
}

static inline bool sema_resolve_type(SemaContext *context, TypeInfo *type_info, ResolveTypeKind resolve_kind)
{
	// Ok, already resolved.
	if (type_info->resolve_status == RESOLVE_DONE)
	{
		if (!type_info_ok(type_info)) return false;
		return true;
	}

	// We might have the resolve already running, if so then that's bad.
	if (type_info->resolve_status == RESOLVE_RUNNING)
	{
		if (type_info->kind == TYPE_INFO_GENERIC)
		{
			SEMA_ERROR(type_info, "Circular dependency resolving generic type.");
		}
		else
		{
			SEMA_ERROR(type_info,
					   "Circular dependency resolving type '%s'.",
					   type_info->unresolved.name);
		}
		return type_info_poison(type_info);
	}

	type_info->resolve_status = RESOLVE_RUNNING;
	TypeInfoCompressedKind kind = type_info->subtype;
	switch (kind)
	{
		case TYPE_COMPRESSED_NONE:
		case TYPE_COMPRESSED_PTR:
		case TYPE_COMPRESSED_SUBPTR:
		case TYPE_COMPRESSED_PTRPTR:
		case TYPE_COMPRESSED_PTRSUB:
			break;
		case TYPE_COMPRESSED_SUB:
		case TYPE_COMPRESSED_SUBSUB:
			resolve_kind = resolve_kind & ~RESOLVE_TYPE_NO_CHECK_DISTINCT;
			break;
	}
	switch (type_info->kind)
	{
		case TYPE_INFO_POISON:
			UNREACHABLE
		case TYPE_INFO_GENERIC:
			if (!sema_resolve_generic_type(context, type_info)) return type_info_poison(type_info);
			goto APPEND_QUALIFIERS;
		case TYPE_INFO_VATYPE:
			if (!sema_resolve_vatype(context, type_info)) return type_info_poison(type_info);
			goto APPEND_QUALIFIERS;
		case TYPE_INFO_CT_IDENTIFIER:
		case TYPE_INFO_IDENTIFIER:
			// $Type or Foo
			if (!sema_resolve_type_identifier(context, type_info, resolve_kind)) return type_info_poison(type_info);
			goto APPEND_QUALIFIERS;
		case TYPE_INFO_EVALTYPE:
			if (!sema_resolve_evaltype(context, type_info, resolve_kind)) return type_info_poison(type_info);
			goto APPEND_QUALIFIERS;
		case TYPE_INFO_TYPEOF:
			if (!sema_resolve_typeof(context, type_info)) return type_info_poison(type_info);
			goto APPEND_QUALIFIERS;
		case TYPE_INFO_TYPEFROM:
			if (!sema_resolve_typefrom(context, type_info, resolve_kind)) return type_info_poison(type_info);
			goto APPEND_QUALIFIERS;
		case TYPE_INFO_INFERRED_VECTOR:
		case TYPE_INFO_INFERRED_ARRAY:
			if (!(resolve_kind & RESOLVE_TYPE_ALLOW_INFER)
				&& (type_info->kind != TYPE_INFO_INFERRED_ARRAY || !(resolve_kind & RESOLVE_TYPE_ALLOW_FLEXIBLE)))
			{
				SEMA_ERROR(type_info, "Inferred %s types can only be used in declarations with initializers and as macro parameters.",
						   type_info->kind == TYPE_INFO_INFERRED_VECTOR ? "vector" : "array");
				return type_info_poison(type_info);
			}
			FALLTHROUGH;
		case TYPE_INFO_SLICE:
		case TYPE_INFO_ARRAY:
		case TYPE_INFO_VECTOR:
			if (!sema_resolve_array_type(context, type_info, resolve_kind & ~RESOLVE_TYPE_NO_CHECK_DISTINCT))
			{
				return type_info_poison(type_info);
			}
			break;
		case TYPE_INFO_POINTER:
			if (!sema_resolve_ptr_type(context, type_info, resolve_kind)) return type_info_poison(type_info);
			break;
	}
APPEND_QUALIFIERS:
	switch (kind)
	{
		case TYPE_COMPRESSED_NONE:
			break;
		case TYPE_COMPRESSED_PTR:
			type_info->type = type_get_ptr(type_info->type);
			break;
		case TYPE_COMPRESSED_SUB:
			if (!sema_check_array_type(context, type_info, type_info->type, TYPE_INFO_SLICE, 0, &type_info->type)) return type_info_poison(type_info);
			break;
		case TYPE_COMPRESSED_SUBPTR:
			if (!sema_check_array_type(context, type_info, type_info->type, TYPE_INFO_SLICE, 0, &type_info->type)) return type_info_poison(type_info);
			type_info->type = type_get_ptr(type_info->type);
			break;
		case TYPE_COMPRESSED_PTRPTR:
			type_info->type = type_get_ptr(type_get_ptr(type_info->type));
			break;
		case TYPE_COMPRESSED_PTRSUB:
			type_info->type = type_get_slice(type_get_ptr(type_info->type));
			break;
		case TYPE_COMPRESSED_SUBSUB:
			if (!sema_check_array_type(context, type_info, type_info->type, TYPE_INFO_SLICE, 0, &type_info->type)) return type_info_poison(type_info);
			type_info->type = type_get_slice(type_info->type);
			break;
	}
	if (type_info->optional)
	{
		Type *type = type_info->type;
		if (!type_is_optional(type)) type_info->type = type_get_optional(type);
	}
	type_info->resolve_status = RESOLVE_DONE;
	return true;
}

typedef struct
{
	uint32_t key;
	Type *value;
} FuncTypeEntry;

typedef struct
{
	uint32_t count;
	uint32_t capacity;
	uint32_t max_load;
	FuncTypeEntry *entries;
} FuncMap;

FuncMap map;

void type_func_prototype_init(uint32_t capacity)
{
	ASSERT(is_power_of_two(capacity) && capacity > 1);
	map.entries = CALLOC(capacity * sizeof(FuncTypeEntry));
	map.capacity = capacity;
	map.max_load = (uint32_t)(TABLE_MAX_LOAD * capacity);
}

static Type *flatten_raw_function_type(Type *type)
{
	Type *other;
	Type *current;
	switch (type->type_kind)
	{
		case TYPE_TYPEDEF:
			return flatten_raw_function_type(type->canonical);
		case TYPE_FUNC_RAW:
			return type->function.prototype->raw_type;
		case TYPE_OPTIONAL:
			current = type->optional;
			other = flatten_raw_function_type(current);
			return other == current ? type : type_get_optional(other);
		case TYPE_FUNC_PTR:
			current = type->pointer;
			other = flatten_raw_function_type(current);
			return other == current ? type : type_get_func_ptr(other);
		case TYPE_POINTER:
			current = type->pointer;
			other = flatten_raw_function_type(current);
			return other == current ? type : type_get_ptr(other);
		case TYPE_ARRAY:
			current = type->array.base;
			other = flatten_raw_function_type(current);
			return other == current ? type : type_get_array(other, type->array.len);
		case TYPE_SLICE:
			current = type->array.base;
			other = flatten_raw_function_type(current);
			return other == current ? type : type_get_slice(other);
		case TYPE_FLEXIBLE_ARRAY:
			current = type->array.base;
			other = flatten_raw_function_type(current);
			return other == current ? type : type_get_flexible_array(other);
		case TYPE_INFERRED_ARRAY:
			current = type->array.base;
			other = flatten_raw_function_type(current);
			return other == current ? type : type_get_inferred_array(other);
		default:
			return type;
	}
}
static uint32_t hash_function(Signature *sig)
{
	uintptr_t hash = sig->variadic == VARIADIC_RAW ? 0 : 1;
	hash = hash * 31 + (uintptr_t)flatten_raw_function_type(type_infoptr(sig->rtype)->type);
	Decl **params = sig->params;
	FOREACH(Decl *, param, params)
	{
		hash = hash * 31 + (uintptr_t)flatten_raw_function_type(param->type->canonical);
	}
	return (uint32_t)((hash >> 16) ^ hash);
}

static inline Type *func_create_new_func_proto(Signature *sig, CallABI abi, uint32_t hash, FuncTypeEntry *entry)
{
	unsigned param_count = vec_size(sig->params);
	FunctionPrototype *proto = CALLOCS(FunctionPrototype);
	proto->raw_variadic = sig->variadic == VARIADIC_RAW;
	proto->vararg_index = sig->vararg_index;
	Type *rtype = type_infoptr(sig->rtype)->type;
	proto->rtype = rtype;
	if (type_is_optional(rtype))
	{
		proto->is_optional = true;
		Type *real_return_type = rtype->optional;
		proto->ret_by_ref_type = rtype->optional;
		proto->ret_by_ref = !type_is_void(real_return_type);
		proto->abi_ret_type = type_fault;
	}
	else
	{
		proto->ret_by_ref_type = proto->abi_ret_type = rtype;
	}
	proto->call_abi = abi;

	if (param_count)
	{
		Type **param_types = VECNEW(Type*, param_count);
		Decl **param_copy = VECNEW(Decl*, param_count);
		for (unsigned i = 0; i < param_count; i++)
		{
			Decl *decl = decl_copy(sig->params[i]);
			decl->type = decl->type->canonical;
			decl->var.type_info = 0;
			decl->var.init_expr = NULL;
			decl->name = NULL;
			vec_add(param_types, decl->type);
			vec_add(param_copy, decl);
		}
		proto->param_types = param_types;
		proto->param_copy = param_copy;
	}

	scratch_buffer_clear();
	scratch_buffer_append("fn ");
	type_append_name_to_scratch(proto->rtype);
	scratch_buffer_append_char('(');
	FOREACH_IDX(idx, Type *, val, proto->param_types)
	{
		if (idx != 0) scratch_buffer_append(", ");
		type_append_name_to_scratch(val);
	}
	scratch_buffer_append_char(')');
	Type *type = type_new(TYPE_FUNC_RAW, scratch_buffer_interned());
	Signature *copy_sig = CALLOCS(Signature);
	*copy_sig = *sig;
	copy_sig->attrs = (CalleeAttributes) { .nodiscard = false };
	copy_sig->params = proto->param_copy;
	proto->raw_type = type;
	type->function.prototype = proto;
	type->function.decl = NULL;
	type->function.signature = copy_sig;
	type->canonical = type;
	entry->key = hash;
	entry->value = type;

	map.count++;
	if (map.count >= map.max_load)
	{
		FuncTypeEntry *entries = map.entries;
		uint32_t old_capacity = map.capacity;
		uint32_t new_capacity = map.capacity = old_capacity << 2;
		map.max_load = (uint32_t)(new_capacity * TABLE_MAX_LOAD);
		FuncTypeEntry *new_map = CALLOC(new_capacity * sizeof(FuncTypeEntry));
		uint32_t new_mask = new_capacity - 1;
		for (uint32_t i = 0; i < old_capacity; i++)
		{
			uint32_t key = entries[i].key;
			if (!key) continue;
			uint32_t index = key & new_mask;
			while (1)
			{
				entry = &new_map[index];
				if (!entry->key)
				{
					entry->key = key;
					entry->value = entries[i].value;
					break;
				}
				index = (index + 1) & new_mask;
			}
		}
		map.entries = new_map;
	}
	return type;
}


static bool compare_func_param(Type *one, Type *other)
{
	if (one == other) return true;
	one = one->canonical;
	other = other->canonical;
	if (one == other) return true;
	if (one->type_kind != other->type_kind) return false;
	switch (one->type_kind)
	{
		case TYPE_POINTER:
		case TYPE_FUNC_PTR:
			return compare_func_param(one->pointer, other->pointer);
		case TYPE_ARRAY:
			if (one->array.len != other->array.len) return false;
					FALLTHROUGH;
		case TYPE_SLICE:
		case TYPE_FLEXIBLE_ARRAY:
			return compare_func_param(one->array.base, other->array.base);
		case TYPE_FUNC_RAW:
			return one->function.prototype->raw_type == other->function.prototype->raw_type;
		case TYPE_OPTIONAL:
			return compare_func_param(one->optional, other->optional);
		default:
			return false;
	}
}

static int compare_function(Signature *sig, FunctionPrototype *proto)
{
	bool is_raw_variadic = sig->variadic == VARIADIC_RAW;
	if (is_raw_variadic != proto->raw_variadic) return -1;
	Decl **params = sig->params;
	Type **other_params = proto->param_types;
	unsigned param_count = vec_size(params);
	if (param_count != vec_size(other_params)) return -1;
	if (!compare_func_param(type_infoptr(sig->rtype)->type, proto->rtype)) return -1;
	FOREACH_IDX(i, Decl *, param, params)
	{
		Type *other_param = other_params[i];
		if (!compare_func_param(param->type, other_param->canonical)) return -1;
	}
	return 0;
}

Type *sema_resolve_type_get_func(Signature *signature, CallABI abi)
{
	uint32_t hash = hash_function(signature);
	uint32_t mask = map.capacity - 1;
	uint32_t index = hash & mask;
	FuncTypeEntry *entries = map.entries;
	while (1)
	{
		FuncTypeEntry *entry = &entries[index];
		if (!entry->key)
		{
			return func_create_new_func_proto(signature, abi, hash, entry);
		}
		if (entry->key == hash && compare_function(signature, entry->value->function.prototype) == 0)
		{
			return entry->value;
		}
		index = (index + 1) & mask;
	}
}

