// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "sema_internal.h"

static inline bool sema_resolve_ptr_type(SemaContext *context, TypeInfo *type_info)
{
	if (!sema_resolve_type_shallow(context, type_info->pointer, false, true))
	{
		return type_info_poison(type_info);
	}
	type_info->type = type_get_ptr(type_info->pointer->type);
	type_info->resolve_status = RESOLVE_DONE;
	return true;
}

bool sema_resolve_type_info(SemaContext *context, TypeInfo *type_info)
{
	return sema_resolve_type_info_maybe_inferred(context, type_info, false);
}

bool sema_resolve_type_info_maybe_inferred(SemaContext *context, TypeInfo *type_info, bool allow_inferred_type)
{
	if (!sema_resolve_type_shallow(context, type_info, allow_inferred_type, false)) return false;
	Type *type = type_no_optional(type_info->type);
	// usize and similar typedefs will not have a decl.
	if (type->type_kind == TYPE_TYPEDEF && type->decl == NULL) return true;
	if (!type_is_user_defined(type)) return true;
	return sema_analyse_decl(context, type->decl);
}

bool sema_resolve_array_like_len(SemaContext *context, TypeInfo *type_info, ArraySize *len_ref)
{
	Expr *len_expr = type_info->array.len;
	if (!sema_analyse_expr(context, len_expr)) return type_info_poison(type_info);

	if (len_expr->expr_kind != EXPR_CONST)
	{
		SEMA_ERROR(len_expr, "Expected a constant value as size.");
		return type_info_poison(type_info);
	}
	if (!type_is_integer(len_expr->type->canonical))
	{
		SEMA_ERROR(len_expr, "Expected an integer size.");
		return type_info_poison(type_info);
	}

	bool is_vector = type_info->kind == TYPE_INFO_VECTOR;
	Int len = len_expr->const_expr.ixx;
	if (int_is_neg(len))
	{
		SEMA_ERROR(len_expr,
				   is_vector ? "A vector may not have a negative width." :
				   "An array may not have a negative size.");
		return type_info_poison(type_info);
	}
	if (is_vector && int_is_zero(len))
	{
		SEMA_ERROR(len_expr, "A vector may not have a zero width.");
		return type_info_poison(type_info);
	}
	if (int_icomp(len, is_vector ? MAX_VECTOR_WIDTH : MAX_ARRAY_SIZE, BINARYOP_GT))
	{
		if (is_vector)
		{
			SEMA_ERROR(len_expr, "A vector may not exceed %d in width.", MAX_VECTOR_WIDTH);
		}
		else
		{
			SEMA_ERROR(len_expr, "The array size may not exceed %lld.", MAX_ARRAY_SIZE);
		}
		return type_info_poison(type_info);
	}
	*len_ref = (ArraySize)len.i.low;
	return true;
}

// TODO cleanup.
static inline bool sema_resolve_array_type(SemaContext *context, TypeInfo *type, bool shallow)
{
	if (type->kind == TYPE_INFO_SUBARRAY || shallow)
	{
		if (!sema_resolve_type_shallow(context, type->array.base, false, true))
		{
			return type_info_poison(type);
		}
	}
	else
	{
		if (!sema_resolve_type_info(context, type->array.base))
		{
			return type_info_poison(type);
		}
	}
	Type *distinct_base = type_flatten_distinct(type->array.base->type);
	if (distinct_base->type_kind == TYPE_STRUCT)
	{
		if (distinct_base->decl->has_variable_array)
		{
			SEMA_ERROR(type, "Arrays of structs with flexible array members is not allowed.");
			return type_info_poison(type);
		}
	}
	switch (type->kind)
	{
		case TYPE_INFO_SUBARRAY:
			type->type = type_get_subarray(type->array.base->type);
			break;
		case TYPE_INFO_INFERRED_ARRAY:
			type->type = type_get_inferred_array(type->array.base->type);
			break;
		case TYPE_INFO_INFERRED_VECTOR:
			type->type = type_get_inferred_vector(type->array.base->type);
			break;
		case TYPE_INFO_SCALED_VECTOR:
			type->type = type_get_scaled_vector(type->array.base->type);
			break;
		case TYPE_INFO_VECTOR:
		{
			ArraySize width;
			if (!sema_resolve_array_like_len(context, type, &width)) return type_info_poison(type);
			type->type = type_get_vector(type->array.base->type, width);
			break;
		}
		case TYPE_INFO_ARRAY:
		{
			ArraySize size;
			if (!sema_resolve_array_like_len(context, type, &size)) return type_info_poison(type);
			type->type = type_get_array(type->array.base->type, size);
			break;
		}
		default:
			UNREACHABLE
	}
	assert(!type->array.len || type->array.len->expr_kind == EXPR_CONST);
	type->resolve_status = RESOLVE_DONE;
	return true;
}


static bool sema_resolve_type_identifier(SemaContext *context, TypeInfo *type_info)
{
	Decl *decl = sema_resolve_symbol(context, type_info->unresolved.name, type_info->unresolved.path, type_info->span);

	// Already handled
	if (!decl_ok(decl))
	{
		return type_info_poison(type_info);
	}

	decl = decl_flatten(decl);
	switch (decl->decl_kind)
	{
		case DECL_STRUCT:
		case DECL_BITSTRUCT:
		case DECL_UNION:
		case DECL_FAULT:
		case DECL_ENUM:
			type_info->type = decl->type;
			type_info->resolve_status = RESOLVE_DONE;
			DEBUG_LOG("Resolved %s.", type_info->unresolved.name);
			return true;
		case DECL_TYPEDEF:
		case DECL_DISTINCT:
		case DECL_DEFINE:
			if (!sema_analyse_decl(context, decl)) return type_info_poison(type_info);
			type_info->type = decl->type;
			type_info->resolve_status = RESOLVE_DONE;
			return true;
		case DECL_POISONED:
			return type_info_poison(type_info);
		case DECL_VAR:
			if (decl->var.kind == VARDECL_PARAM_CT_TYPE || decl->var.kind == VARDECL_LOCAL_CT_TYPE)
			{
				if (!decl->var.init_expr)
				{
					SEMA_ERROR(type_info, "The variable '%s' is not defined yet.", decl->name);
					return false;
				}
				assert(decl->var.init_expr->expr_kind == EXPR_TYPEINFO);
				assert(decl->var.init_expr->resolve_status == RESOLVE_DONE);
				*type_info = *decl->var.init_expr->type_expr;
				return true;
			}
			FALLTHROUGH;
		case DECL_FUNC:
		case DECL_FAULTVALUE:
		case DECL_ENUM_CONSTANT:
		case DECL_IMPORT:
		case DECL_MACRO:
		case DECL_GENERIC:
		case DECL_LABEL:
		case DECL_ATTRIBUTE:
			SEMA_ERROR(type_info, "This is not a type.");
			return type_info_poison(type_info);
		case DECL_INITIALIZE:
		case DECL_FINALIZE:
		case DECL_CT_ELSE:
		case DECL_CT_IF:
		case DECL_CT_ELIF:
		case DECL_CT_SWITCH:
		case DECL_CT_CASE:
		case DECL_CT_ASSERT:
		case DECL_DECLARRAY:
		case DECL_BODYPARAM:
			UNREACHABLE
	}
	UNREACHABLE

}


bool sema_resolve_type(SemaContext *context, Type *type)
{
	switch (type->type_kind)
	{
		case TYPE_TYPEDEF:
			return sema_resolve_type(context, type->canonical);
		case TYPE_POISONED:
		case ALL_INTS:
		case ALL_FLOATS:
		case TYPE_VOID:
		case TYPE_BOOL:
		case TYPE_TYPEID:
		case TYPE_ANY:
		case TYPE_ANYERR:
		case TYPE_VECTOR:
		case TYPE_TYPEINFO:
		case TYPE_MEMBER:
		case TYPE_UNTYPED_LIST:
		case TYPE_FAILABLE_ANY:
			return true;
		case TYPE_POINTER:
			return sema_resolve_type(context, type->pointer);
		case TYPE_BITSTRUCT:
		case TYPE_DISTINCT:
		case TYPE_ENUM:
		case TYPE_FAULTTYPE:
		case TYPE_FUNC:
		case TYPE_STRUCT:
		case TYPE_UNION:
			break;
		case TYPE_ARRAY:
		case TYPE_SUBARRAY:
		case TYPE_INFERRED_ARRAY:
		case TYPE_FLEXIBLE_ARRAY:
		case TYPE_INFERRED_VECTOR:
		case TYPE_SCALED_VECTOR:
			return sema_resolve_type(context, type->array.base);
		case TYPE_OPTIONAL:
			return sema_resolve_type(context, type->failable);
	}
	return sema_analyse_decl(context, type->decl);
}

bool sema_resolve_type_shallow(SemaContext *context, TypeInfo *type_info, bool allow_inferred_type, bool in_shallow)
{
	if (type_info->resolve_status == RESOLVE_DONE) return type_info_ok(type_info);

	if (type_info->resolve_status == RESOLVE_RUNNING)
	{
		// TODO this is incorrect for unresolved expressions
		SEMA_ERROR(type_info,
		           "Circular dependency resolving type '%s'.",
		           type_info->unresolved.name);
		return type_info_poison(type_info);
	}

	type_info->resolve_status = RESOLVE_RUNNING;
	TypeInfoCompressedKind kind = type_info->subtype;
	if (kind != TYPE_COMPRESSED_NONE)
	{
		allow_inferred_type = false;
		in_shallow = true;
	}
	switch (type_info->kind)
	{
		case TYPE_INFO_POISON:
			UNREACHABLE
		case TYPE_INFO_VATYPE:
			if (context->current_macro)
			{
				ASSIGN_EXPR_OR_RET(Expr *arg_expr, sema_expr_analyse_ct_arg_index(context, type_info->unresolved_type_expr), false);
				if (arg_expr->expr_kind != EXPR_TYPEINFO)
				{
					SEMA_ERROR(arg_expr, "The argument was not a type.");
					return false;
				}
				*type_info = *arg_expr->type_expr;
				assert(type_info->resolve_status == RESOLVE_DONE);
				return true;
			}
			SEMA_ERROR(type_info, "'%s' can only be used inside of a macro.", token_type_to_string(TOKEN_CT_VATYPE));
			return false;
		case TYPE_INFO_CT_IDENTIFIER:
		case TYPE_INFO_IDENTIFIER:
			if (!sema_resolve_type_identifier(context, type_info)) return false;
			break;
		case TYPE_INFO_EVALTYPE:
		{
			Expr *expr = type_info->unresolved_type_expr;
			TokenType type;
			Expr *inner = sema_ct_eval_expr(context, "$evaltype", expr, true);
			if (!inner) return false;
			if (inner->expr_kind != EXPR_TYPEINFO)
			{
				SEMA_ERROR(expr, "Only type names may be resolved with $evaltype.");
				return type_info_poison(type_info);
			}
			if (type_is_invalid_for_typeof(expr->type))
			{
				SEMA_ERROR(expr, "Compile-time expressions may not be used with $evaltype.");
				return type_info_poison(type_info);
			}
			TypeInfo *inner_type = inner->type_expr;
			if (!sema_resolve_type_info(context, inner_type)) return false;
			type_info->type = inner_type->type;
			type_info->resolve_status = RESOLVE_DONE;
			goto APPEND_QUALIFIERS;
		}
		case TYPE_INFO_TYPEOF:
		{
			Expr *expr = type_info->unresolved_type_expr;
			if (!sema_analyse_expr(context, expr))
			{
				return type_info_poison(type_info);
			}
			if (type_is_invalid_for_typeof(expr->type))
			{
				SEMA_ERROR(expr, "Compile-time expressions are not allowed here.");
				return false;
			}
			type_info->type = expr->type;
			type_info->resolve_status = RESOLVE_DONE;
			assert(!type_info->failable);
			goto APPEND_QUALIFIERS;
		}
		case TYPE_INFO_TYPEFROM:
		{
			Expr *expr = type_info->unresolved_type_expr;
			if (!sema_analyse_expr(context, expr))
			{
				return type_info_poison(type_info);
			}
			if (!expr_is_const(expr) || expr->const_expr.const_kind != CONST_TYPEID)
			{
				SEMA_ERROR(expr, "Expected a constant typeid value.");
				return type_info_poison(type_info);
			}
			type_info->type = expr->const_expr.typeid;
			type_info->resolve_status = RESOLVE_DONE;
			assert(!type_info->failable);
			goto APPEND_QUALIFIERS;
		}
		case TYPE_INFO_INFERRED_ARRAY:
		case TYPE_INFO_INFERRED_VECTOR:
			if (!allow_inferred_type)
			{
				SEMA_ERROR(type_info, "Inferred %s types can only be used in declarations with initializers and as macro parameters.",
				           type_info->kind == TYPE_INFO_INFERRED_VECTOR ? "vector" : "array");
				return type_info_poison(type_info);
			}
			FALLTHROUGH;
		case TYPE_INFO_SCALED_VECTOR:
		case TYPE_INFO_SUBARRAY:
		case TYPE_INFO_ARRAY:
		case TYPE_INFO_VECTOR:
			if (!sema_resolve_array_type(context, type_info, in_shallow)) return false;
			break;
		case TYPE_INFO_POINTER:
			if (!sema_resolve_ptr_type(context, type_info)) return false;
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
	if (type_info->failable)
	{
		Type *type = type_info->type;
		if (!type_is_optional(type)) type_info->type = type_get_optional(type);
	}
	return true;
}

Type *sema_type_lower_by_size(Type *type, ArraySize element_size)
{
	switch (type->type_kind)
	{
		case TYPE_INFERRED_ARRAY:
			return type_get_array(type->array.base, element_size);
		case TYPE_INFERRED_VECTOR:
			return type_get_vector(type->array.base, element_size);
		default:
			return type;
	}
}
