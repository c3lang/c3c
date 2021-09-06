// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "sema_internal.h"

static inline bool sema_resolve_ptr_type(Context *context, TypeInfo *type_info)
{
	if (!sema_resolve_type_shallow(context, type_info->pointer, false, true))
	{
		return type_info_poison(type_info);
	}
	type_info->type = type_get_ptr(type_info->pointer->type);
	type_info->resolve_status = RESOLVE_DONE;
	return true;
}

// TODO cleanup.
static inline bool sema_resolve_array_type(Context *context, TypeInfo *type, bool shallow)
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

	uint64_t len;
	switch (type->kind)
	{
		case TYPE_INFO_SUBARRAY:
			type->type = type_get_subarray(type->array.base->type);
			break;
		case TYPE_INFO_INFERRED_ARRAY:
			type->type = type_get_inferred_array(type->array.base->type);
			break;
		case TYPE_INFO_ARRAY:
			if (!sema_analyse_expr(context, type_usize, type->array.len)) return type_info_poison(type);
			if (type->array.len->expr_kind != EXPR_CONST)
			{
				SEMA_ERROR(type->array.len, "Expected a constant value as array size.");
				return type_info_poison(type);
			}
			if (!type_is_any_integer(type->array.len->type->canonical))
			{
				SEMA_ERROR(type->array.len, "Expected an integer size.");
				return type_info_poison(type);
			}
			if (bigint_cmp_zero(&type->array.len->const_expr.i) == CMP_LT)
			{
				SEMA_ERROR(type->array.len, "An array may not have a negative size.");
				return type_info_poison(type);
			}
			if (!bigint_fits_in_bits(&type->array.len->const_expr.i, 64, true))
			{
				SEMA_ERROR(type->array.len, "An array length may not exceed the max of an 64 bit signed int.");
				return type_info_poison(type);
			}

			len = bigint_as_unsigned(&type->array.len->const_expr.i);
			type->type = type_get_array(type->array.base->type, len);
			break;
		default:
			UNREACHABLE
	}
	assert(!type->array.len || type->array.len->expr_kind == EXPR_CONST);
	type->resolve_status = RESOLVE_DONE;
	return true;
}


static bool sema_resolve_type_identifier(Context *context, TypeInfo *type_info)
{
	Decl *decl = sema_resolve_normal_symbol(context,
	                                        type_info->unresolved.name_loc,
	                                        type_info->unresolved.path, true);
	decl = decl_flatten(decl);
	// Already handled
	if (!decl_ok(decl))
	{
		return type_info_poison(type_info);
	}

	switch (decl->decl_kind)
	{
		case DECL_STRUCT:
		case DECL_BITSTRUCT:
		case DECL_UNION:
		case DECL_ERRTYPE:
		case DECL_ENUM:
		case DECL_TYPEDEF:
		case DECL_DISTINCT:
		case DECL_INTERFACE:
			type_info->type = decl->type;
			type_info->resolve_status = RESOLVE_DONE;
			DEBUG_LOG("Resolved %s.", TOKSTR(type_info->unresolved.name_loc));
			return true;
		case DECL_DEFINE:
		case DECL_POISONED:
			return type_info_poison(type_info);
		case DECL_VAR:
			if (decl->var.kind == VARDECL_PARAM_CT_TYPE || decl->var.kind == VARDECL_LOCAL_CT_TYPE)
			{
				assert(decl->var.init_expr->expr_kind == EXPR_TYPEINFO);
				assert(decl->var.init_expr->resolve_status == RESOLVE_DONE);
				*type_info = *decl->var.init_expr->type_expr;
				return true;
			}
			FALLTHROUGH;
		case DECL_FUNC:
		case DECL_ERRVALUE:
		case DECL_ENUM_CONSTANT:
		case DECL_ARRAY_VALUE:
		case DECL_IMPORT:
		case DECL_MACRO:
		case DECL_GENERIC:
		case DECL_LABEL:
			SEMA_TOKID_ERROR(type_info->unresolved.name_loc, "This is not a type.");
			return type_info_poison(type_info);
		case DECL_CT_ELSE:
		case DECL_CT_IF:
		case DECL_CT_ELIF:
		case DECL_ATTRIBUTE:
		case DECL_CT_SWITCH:
		case DECL_CT_CASE:
		case DECL_CT_ASSERT:
			UNREACHABLE
	}
	UNREACHABLE

}


bool sema_resolve_type(Context *context, Type *type)
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
		case TYPE_VIRTUAL_ANY:
		case TYPE_ANYERR:
		case TYPE_STRLIT:
		case TYPE_VECTOR:
		case TYPE_TYPEINFO:
		case TYPE_UNTYPED_LIST:
			return true;
		case TYPE_POINTER:
			return sema_resolve_type(context, type->pointer);
		case TYPE_BITSTRUCT:
		case TYPE_DISTINCT:
		case TYPE_ENUM:
		case TYPE_ERRTYPE:
		case TYPE_FUNC:
		case TYPE_STRUCT:
		case TYPE_UNION:
			break;
		case TYPE_ARRAY:
		case TYPE_SUBARRAY:
		case TYPE_INFERRED_ARRAY:
			return sema_resolve_type(context, type->array.base);
		case TYPE_VIRTUAL:
			TODO;
	}
	return sema_analyse_decl(context, type->decl);
}

bool sema_resolve_type_shallow(Context *context, TypeInfo *type_info, bool allow_inferred_type, bool in_shallow)
{
	if (type_info->resolve_status == RESOLVE_DONE) return type_info_ok(type_info);

	if (type_info->resolve_status == RESOLVE_RUNNING)
	{
		// TODO this is incorrect for unresolved expressions
		SEMA_TOKID_ERROR(type_info->unresolved.name_loc,
		                 "Circular dependency resolving type '%s'.",
		                 TOKSTR(type_info->unresolved.name_loc));
		return type_info_poison(type_info);
	}

	type_info->resolve_status = RESOLVE_RUNNING;

	switch (type_info->kind)
	{
		case TYPE_INFO_POISON:
		case TYPE_INFO_INC_ARRAY:
			UNREACHABLE
		case TYPE_INFO_IDENTIFIER:
			if (!sema_resolve_type_identifier(context, type_info)) return false;
			break;
		case TYPE_INFO_EXPRESSION:
		{
			Expr *expr = type_info->unresolved_type_expr;
			if (!sema_analyse_expr(context, NULL, expr))
			{
				return type_info_poison(type_info);
			}
			if (!cast_implicitly_to_runtime(expr))
			{
				SEMA_ERROR(expr, "The expression does not fit any runtime type.");
				return false;
			}
			type_info->type = expr->type;
			type_info->resolve_status = RESOLVE_DONE;
			return true;
		}
		case TYPE_INFO_INFERRED_ARRAY:
			if (!allow_inferred_type)
			{
				SEMA_ERROR(type_info, "Inferred array types can only be used in declarations with initializers.");
				return type_info_poison(type_info);
			}
			FALLTHROUGH;
		case TYPE_INFO_SUBARRAY:
		case TYPE_INFO_ARRAY:
			if (!sema_resolve_array_type(context, type_info, in_shallow)) return false;
			break;
		case TYPE_INFO_POINTER:
			if (!sema_resolve_ptr_type(context, type_info)) return false;
			break;
	}
	return true;
}

Type *sema_type_lower_by_size(Type *type, ByteSize element_size)
{
	if (type->type_kind != TYPE_INFERRED_ARRAY) return type;

	return type_get_array(type->array.base, element_size);
}
