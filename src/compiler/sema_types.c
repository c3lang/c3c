// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "sema_internal.h"


static inline bool sema_resolve_ptr_type(SemaContext *context, TypeInfo *type_info, ResolveTypeKind resolve_kind);
static inline bool sema_resolve_array_type(SemaContext *context, TypeInfo *type, ResolveTypeKind resolve_kind);
static inline bool sema_resolve_type(SemaContext *context, TypeInfo *type_info, ResolveTypeKind resolve_kind);
INLINE bool sema_resolve_vatype(SemaContext *context, TypeInfo *type_info);
INLINE bool sema_resolve_evaltype(SemaContext *context, TypeInfo *type_info, ResolveTypeKind resolve_kind);
INLINE bool sema_resolve_typefrom(SemaContext *context, TypeInfo *type_info);
INLINE bool sema_resolve_typeof(SemaContext *context, TypeInfo *type_info);

static inline bool sema_resolve_ptr_type(SemaContext *context, TypeInfo *type_info, ResolveTypeKind resolve_kind)
{
	// Try to resolve this type shallowly.
	if (!sema_resolve_type(context, type_info->pointer, resolve_kind | RESOLVE_TYPE_PTR))
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

	// A constant expression is assumed.
	if (!expr_is_const(len_expr))
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
	if (int_icomp(len, is_vector ? MAX_VECTOR_WIDTH : MAX_ARRAY_SIZE, BINARYOP_GT))
	{
		if (is_vector)
		{
			SEMA_ERROR(len_expr, "A vector may not exceed %d in width.", MAX_VECTOR_WIDTH);
		}
		else
		{
			SEMA_ERROR(len_expr, "The array length may not exceed %lld.", MAX_ARRAY_SIZE);
		}
		return type_info_poison(type_info);
	}
	// We're done, return the size and mark it as a success.
	*len_ref = (ArraySize)len.i.low;
	return true;
}

// TODO cleanup.
static inline bool sema_resolve_array_type(SemaContext *context, TypeInfo *type, ResolveTypeKind resolve_type_kind)
{
	TypeInfoKind kind = type->kind;
	// We can resolve the base type in a shallow way if we don't use it to determine
	// length and alignment
	if (kind == TYPE_INFO_SUBARRAY || (resolve_type_kind & RESOLVE_TYPE_IS_POINTEE))
	{
		if (!sema_resolve_type(context, type->array.base, resolve_type_kind))
		{
			return type_info_poison(type);
		}
	}
	else
	{
		if (!sema_resolve_type(context, type->array.base, resolve_type_kind & ~RESOLVE_TYPE_IS_POINTEE))
		{
			return type_info_poison(type);
		}
	}
	Type *distinct_base = type_flatten(type->array.base->type);
	if (distinct_base->type_kind == TYPE_STRUCT)
	{
		if (distinct_base->decl->resolve_status == RESOLVE_DONE)
		{
			if (distinct_base->decl->has_variable_array)
			{
				SEMA_ERROR(type, "Arrays of structs with flexible array members is not allowed.");
				return type_info_poison(type);
			}
		}
		else
		{
			vec_add(context->unit->check_type_variable_array, type);
		}
	}
	TypeInfo *base_info = type->array.base;
	Type *base = base_info->type;
	switch (type->kind)
	{
		case TYPE_INFO_SUBARRAY:
			if (!type_is_valid_for_array(base))
			{
				SEMA_ERROR(base_info,
				           "You cannot form a subarray with elements of type %s.",
				           type_quoted_error_string(base));
				return type_info_poison(type);
			}
			type->type = type_get_subarray(type->array.base->type);
			break;
		case TYPE_INFO_INFERRED_ARRAY:
			if (!type_is_valid_for_array(base))
			{
				SEMA_ERROR(base_info,
				           "You cannot form an array with elements of type %s.",
				           type_quoted_error_string(base));
				return type_info_poison(type);
			}
			type->type = type_get_inferred_array(type->array.base->type);
			break;
		case TYPE_INFO_INFERRED_VECTOR:
			if (!type_is_valid_for_vector(base))
			{
				SEMA_ERROR(base_info,
				           "You cannot form a vector with elements of type %s.",
				           type_quoted_error_string(base));
				return type_info_poison(type);

			}
			type->type = type_get_inferred_vector(type->array.base->type);
			break;
		case TYPE_INFO_VECTOR:
		{
			ArraySize width;
			if (!sema_resolve_array_like_len(context, type, &width)) return type_info_poison(type);
			if (!type_is_valid_for_vector(base))
			{
				SEMA_ERROR(base_info,
				           "You cannot form a vector with elements of type %s.",
				           type_quoted_error_string(base));
				return type_info_poison(type);

			}
			type->type = type_get_vector(type->array.base->type, width);
			break;
		}
		case TYPE_INFO_ARRAY:
		{
			if (!type_is_valid_for_array(base))
			{
				SEMA_ERROR(base_info,
				           "You cannot form an array with elements of type %s.",
				           type_quoted_error_string(base));
				return type_info_poison(type);
			}
			ArraySize size;
			if (!sema_resolve_array_like_len(context, type, &size)) return type_info_poison(type);
			type->type = type_get_array(type->array.base->type, size);
			break;
		}
		default:
			UNREACHABLE
	}
	assert(!type->array.len || expr_is_const(type->array.len));
	type->resolve_status = RESOLVE_DONE;
	return true;
}


static bool sema_resolve_type_identifier(SemaContext *context, TypeInfo *type_info)
{
	if (type_info->unresolved.name == type_string->name && !type_info->unresolved.path)
	{
		type_info->type = type_string;
		type_info->resolve_status = RESOLVE_DONE;
		return true;
	}
	Decl *decl = sema_resolve_symbol(context, type_info->unresolved.name, type_info->unresolved.path, type_info->span);

	assert(decl);
	// Already handled
	if (!decl_ok(decl))
	{
		return type_info_poison(type_info);
	}

	decl = decl_flatten(decl);
	switch (decl->decl_kind)
	{
		case DECL_ERASED:
			UNREACHABLE
		case DECL_STRUCT:
		case DECL_BITSTRUCT:
		case DECL_UNION:
		case DECL_FAULT:
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
		case DECL_TYPEDEF:
		case DECL_DISTINCT:
			if (!sema_analyse_decl(context, decl)) return type_info_poison(type_info);
			type_info->type = decl->type;
			type_info->resolve_status = RESOLVE_DONE;
			return true;
		case DECL_POISONED:
			return type_info_poison(type_info);
		case DECL_VAR:
			if (decl->var.kind == VARDECL_PARAM_CT_TYPE || decl->var.kind == VARDECL_LOCAL_CT_TYPE)
			{
				decl->var.is_read = true;
				if (!decl->var.init_expr)
				{
					SEMA_ERROR(type_info, "You need to assign a type to '%s' before using it.", decl->name);
					return false;
				}
				assert(decl->var.init_expr->expr_kind == EXPR_TYPEINFO);
				assert(decl->var.init_expr->resolve_status == RESOLVE_DONE);
				*type_info = *decl->var.init_expr->type_expr;
				return true;
			}
			FALLTHROUGH;
		case DECL_DEFINE:
		case DECL_FUNC:
		case DECL_FAULTVALUE:
		case DECL_ENUM_CONSTANT:
		case DECL_IMPORT:
		case DECL_MACRO:
		case DECL_LABEL:
		case DECL_ATTRIBUTE:
			SEMA_ERROR(type_info, "This is not a type.");
			return type_info_poison(type_info);
		case DECL_CT_ASSERT:
		case DECL_CT_ECHO:
		case DECL_DECLARRAY:
		case DECL_BODYPARAM:
		case DECL_CT_INCLUDE:
		case DECL_CT_EXEC:
		case DECL_GLOBALS:
			UNREACHABLE
	}
	UNREACHABLE

}


// $evaltype("Foo")
INLINE bool sema_resolve_evaltype(SemaContext *context, TypeInfo *type_info, ResolveTypeKind resolve_kind)
{
	Expr *expr = type_info->unresolved_type_expr;
	Expr *inner = sema_ct_eval_expr(context, "$evaltype", expr, true);
	if (!inner) return type_info_poison(type_info);
	if (inner->expr_kind != EXPR_TYPEINFO)
	{
		SEMA_ERROR(expr, "Only type names may be resolved with $evaltype.");
		return false;
	}
	TypeInfo *inner_type = inner->type_expr;
	if (!sema_resolve_type(context, inner_type, resolve_kind)) return false;
	if (type_is_invalid_storage_type(inner_type->type))
	{
		SEMA_ERROR(expr, "Compile-time types may not be used with $evaltype.");
		return false;
	}
	type_info->type = inner_type->type;
	return true;
}

// $typeof(...)
INLINE bool sema_resolve_typeof(SemaContext *context, TypeInfo *type_info)
{
	Expr *expr = type_info->unresolved_type_expr;
	if (!sema_analyse_expr(context, expr)) return false;
	if (type_is_invalid_storage_type(expr->type))
	{
		if (expr->type == type_wildcard)
		{
			RETURN_SEMA_ERROR(expr, "This expression has no concrete type.");
		}
		if (expr->type == type_wildcard_optional)
		{
			RETURN_SEMA_ERROR(expr, "This optional expression is untyped.");
		}
		RETURN_SEMA_ERROR(expr, "Expected a regular runtime expression here.");
	}
	type_info->type = expr->type;
	return true;
}

INLINE bool sema_resolve_typefrom(SemaContext *context, TypeInfo *type_info)
{
	Expr *expr = type_info->unresolved_type_expr;
	if (!sema_analyse_expr(context, expr)) return false;
	if (!expr_is_const(expr) || expr->const_expr.const_kind != CONST_TYPEID)
	{
		RETURN_SEMA_ERROR(expr, "Expected a constant typeid value.");
	}
	type_info->type = expr->const_expr.typeid;
	return true;
}

// $vatype(...)
INLINE bool sema_resolve_vatype(SemaContext *context, TypeInfo *type_info)
{
	if (!context->current_macro)
	{
		RETURN_SEMA_ERROR(type_info, "'%s' can only be used inside of a macro.", token_type_to_string(TOKEN_CT_VATYPE));
	}
	ASSIGN_EXPR_OR_RET(Expr *arg_expr, sema_expr_analyse_ct_arg_index(context, type_info->unresolved_type_expr, NULL,
	                                                                  true), false);
	if (!sema_analyse_expr_lvalue(context, arg_expr)) return false;
	if (arg_expr->expr_kind != EXPR_TYPEINFO) RETURN_SEMA_ERROR(arg_expr, "The argument was not a type.");
	type_info->type = arg_expr->type_expr->type;
	return true;
}

// Foo(<...>)
INLINE bool sema_resolve_generic_type(SemaContext *context, TypeInfo *type_info)
{
	TypeInfo *inner = type_info->generic.base;
	if (inner->kind != TYPE_INFO_IDENTIFIER && inner->subtype != TYPE_COMPRESSED_NONE && !inner->optional)
	{
		SEMA_ERROR(inner, "Parameterization required a concrete type name here.");
		return false;
	}
	assert(inner->resolve_status == RESOLVE_NOT_DONE);

	Decl *type = sema_analyse_parameterized_identifier(context, inner->unresolved.path, inner->unresolved.name, inner->span, type_info->generic.params);
	if (!decl_ok(type)) return false;
	type_info->type = type->type;
	return true;
}

static inline bool sema_resolve_type(SemaContext *context, TypeInfo *type_info, ResolveTypeKind resolve_type_kind)
{
	// Ok, already resolved.
	if (type_info->resolve_status == RESOLVE_DONE)
	{
		if (!type_info_ok(type_info)) return false;
		if (!(resolve_type_kind & RESOLVE_TYPE_ALLOW_ANY))
		{
			switch (type_no_optional(type_info->type)->canonical->type_kind)
			{
				case TYPE_ANY:
				case TYPE_INTERFACE:
					RETURN_SEMA_ERROR(type_info, "%s has no valid runtime size, you should use '%s' instead.",
					                  type_quoted_error_string(type_no_optional(type_info->type)),
					                  type_quoted_error_string(type_get_ptr(type_no_optional(type_info->type))));
				default:
					break;
			}
		}
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

	// Type compression means we don't need that many nested type infos.
	TypeInfoCompressedKind kind = type_info->subtype;
	switch (kind)
	{
		case TYPE_COMPRESSED_NONE:
			break;
		case TYPE_COMPRESSED_PTR:
		case TYPE_COMPRESSED_PTRPTR:
		case TYPE_COMPRESSED_PTRSUB:
			resolve_type_kind |= RESOLVE_TYPE_PTR;
			break;
		case TYPE_COMPRESSED_SUB:
		case TYPE_COMPRESSED_SUBPTR:
		case TYPE_COMPRESSED_SUBSUB:
			resolve_type_kind |= RESOLVE_TYPE_IS_POINTEE;
			break;
		default:
			UNREACHABLE
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
			if (!sema_resolve_type_identifier(context, type_info)) return type_info_poison(type_info);
			goto APPEND_QUALIFIERS;
		case TYPE_INFO_EVALTYPE:
			if (!sema_resolve_evaltype(context, type_info, resolve_type_kind)) return type_info_poison(type_info);
			goto APPEND_QUALIFIERS;
		case TYPE_INFO_TYPEOF:
			if (!sema_resolve_typeof(context, type_info)) return type_info_poison(type_info);
			goto APPEND_QUALIFIERS;
		case TYPE_INFO_TYPEFROM:
			if (!sema_resolve_typefrom(context, type_info)) return type_info_poison(type_info);
			goto APPEND_QUALIFIERS;
		case TYPE_INFO_INFERRED_VECTOR:
		case TYPE_INFO_INFERRED_ARRAY:
			if (!(resolve_type_kind & RESOLVE_TYPE_ALLOW_INFER)
				&& (type_info->kind != TYPE_INFO_INFERRED_ARRAY || !(resolve_type_kind & RESOLVE_TYPE_ALLOW_FLEXIBLE)))
			{
				SEMA_ERROR(type_info, "Inferred %s types can only be used in declarations with initializers and as macro parameters.",
						   type_info->kind == TYPE_INFO_INFERRED_VECTOR ? "vector" : "array");
				return type_info_poison(type_info);
			}
			FALLTHROUGH;
		case TYPE_INFO_SUBARRAY:
		case TYPE_INFO_ARRAY:
		case TYPE_INFO_VECTOR:
			if (!sema_resolve_array_type(context, type_info, resolve_type_kind))
			{
				return type_info_poison(type_info);
			}
			break;
		case TYPE_INFO_POINTER:
			if (!sema_resolve_ptr_type(context, type_info, resolve_type_kind)) return type_info_poison(type_info);
			break;
	}
APPEND_QUALIFIERS:
	switch (type_info->type->type_kind)
	{
		case TYPE_ANY:
			if (!(resolve_type_kind & RESOLVE_TYPE_ALLOW_ANY))
			{
				SEMA_ERROR(type_info, "An 'any' has undefined size, please use 'any*' instead.");
				return type_info_poison(type_info);
			}
			break;
		case TYPE_INTERFACE:
			if (!(resolve_type_kind & RESOLVE_TYPE_ALLOW_ANY))
			{
				SEMA_ERROR(type_info, "%s is an interface and has undefined size, please use %s instead.",
				           type_quoted_error_string(type_info->type),
				           type_quoted_error_string(type_get_ptr(type_info->type)));
				return type_info_poison(type_info);
			}
			break;
		default:
			break;
	}
	switch (kind)
	{
		case TYPE_COMPRESSED_NONE:
			break;
		case TYPE_COMPRESSED_PTR:
			type_info->type = type_get_ptr(type_info->type);
			break;
		case TYPE_COMPRESSED_SUB:
			type_info->type = type_get_subarray(type_info->type);
			break;
		case TYPE_COMPRESSED_SUBPTR:
			type_info->type = type_get_ptr(type_get_subarray(type_info->type));
			break;
		case TYPE_COMPRESSED_PTRPTR:
			type_info->type = type_get_ptr(type_get_ptr(type_info->type));
			break;
		case TYPE_COMPRESSED_PTRSUB:
			type_info->type = type_get_subarray(type_get_ptr(type_info->type));
			break;
		case TYPE_COMPRESSED_SUBSUB:
			type_info->type = type_get_subarray(type_get_subarray(type_info->type));
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

