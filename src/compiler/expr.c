// Copyright (c) 2022 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"

static inline bool expr_list_is_constant_eval(Expr **exprs);
static inline bool expr_unary_addr_is_constant_eval(Expr *expr);
static inline ConstInitializer *initializer_for_index(ConstInitializer *initializer, ArraySize index, bool from_back);

const char *expr_kind_to_string(ExprKind kind)
{
	switch (kind)
	{
		case EXPR_ACCESS: return "access";
		case EXPR_ANYSWITCH: return "anyswitch";
		case EXPR_ASM: return "asm";
		case EXPR_BENCHMARK_HOOK: return "benchmark_hook";
		case EXPR_BINARY: return "binary";
		case EXPR_BITACCESS: return "bitaccess";
		case EXPR_BITASSIGN: return "bitassign";
		case EXPR_BUILTIN: return "builtin";
		case EXPR_BUILTIN_ACCESS: return "builtin_access";
		case EXPR_CALL: return "call";
		case EXPR_CAST: return "cast";
		case EXPR_CATCH_UNWRAP: return "catch_unwrap";
		case EXPR_COMPILER_CONST: return "compiler_const";
		case EXPR_COMPOUND_LITERAL: return "compound_litera";
		case EXPR_COND: return "cond";
		case EXPR_CONST: return "const";
		case EXPR_TYPECALL: return "typecall";
		case EXPR_CT_AND_OR: return "ct_and_or";
		case EXPR_CT_ARG: return "ct_arg";
		case EXPR_CT_APPEND: return "ct_append";
		case EXPR_CT_CALL: return "ct_call";
		case EXPR_CT_CASTABLE: return "ct_castable";
		case EXPR_CT_CONCAT: return "ct_concat";
		case EXPR_CT_DEFINED: return "ct_defined";
		case EXPR_CT_EVAL: return "ct_eval";
		case EXPR_CT_IDENT: return "ct_ident";
		case EXPR_CT_IS_CONST: return "ct_is_const";
		case EXPR_DECL: return "decl";
		case EXPR_DEFAULT_ARG: return "default_arg";
		case EXPR_DESIGNATED_INITIALIZER_LIST: return "designated_initializer_list";
		case EXPR_DESIGNATOR: return "designator";
		case EXPR_DISCARD: return "discard";
		case EXPR_EMBED: return "embed";
		case EXPR_VECTOR_TO_ARRAY: return "vector_to_array";
		case EXPR_SLICE_TO_VEC_ARRAY: return "slice_to_vec_array";
		case EXPR_SCALAR_TO_VECTOR: return "scalar_to_vector";
		case EXPR_EXPRESSION_LIST: return "expression_list";
		case EXPR_EXPR_BLOCK: return "expr_block";
		case EXPR_FORCE_UNWRAP: return "force_unwrap";
		case EXPR_FLOAT_TO_INT: return "float_to_int";
		case EXPR_GENERIC_IDENT: return "generic_ident";
		case EXPR_HASH_IDENT: return "hash_ident";
		case EXPR_IDENTIFIER: return "identifier";
		case EXPR_INITIALIZER_LIST: return "initializer_list";
		case EXPR_INT_TO_FLOAT: return "int_to_float";
		case EXPR_INT_TO_PTR: return "int_to_ptr";
		case EXPR_PTR_TO_INT: return "ptr_to_int";
		case EXPR_ANYFAULT_TO_FAULT: return "anyfault_to_fault";
		case EXPR_LAMBDA: return "lambda";
		case EXPR_LAST_FAULT: return "last_fault";
		case EXPR_MACRO_BLOCK: return "macro_block";
		case EXPR_MACRO_BODY: return "macro_body";
		case EXPR_MACRO_BODY_EXPANSION: return "macro_body_expansion";
		case EXPR_MAKE_ANY: return "make_any";
		case EXPR_MAKE_SLICE: return "make_slice";
		case EXPR_MEMBER_GET: return "member_get";
		case EXPR_NAMED_ARGUMENT: return "named_argument";
		case EXPR_NOP: return "nop";
		case EXPR_OPERATOR_CHARS: return "operator_chars";
		case EXPR_OPTIONAL: return "optional";
		case EXPR_ENUM_FROM_ORD: return "enum_from_ord";
		case EXPR_OTHER_CONTEXT: return "other_context";
		case EXPR_POINTER_OFFSET: return "pointer_offset";
		case EXPR_ADDR_CONVERSION: return "addr_conversion";
		case EXPR_POISONED: return "poisoned";
		case EXPR_PTR_ACCESS: return "ptr_access";
		case EXPR_POST_UNARY: return "post_unary";
		case EXPR_RETHROW: return "rethrow";
		case EXPR_RETVAL: return "retval";
		case EXPR_RVALUE: return "rvalue";
		case EXPR_RECAST: return "recast";
		case EXPR_SLICE: return "slice";
		case EXPR_SLICE_LEN: return "slice_len";
		case EXPR_SLICE_ASSIGN: return "slice_assign";
		case EXPR_SLICE_COPY: return "slice_copy";
		case EXPR_SPLAT: return "splat";
		case EXPR_STRINGIFY: return "stringify";
		case EXPR_SUBSCRIPT: return "subscript";
		case EXPR_SUBSCRIPT_ADDR: return "subscript_addr";
		case EXPR_SUBSCRIPT_ASSIGN: return "subscript_assign";
		case EXPR_SWIZZLE: return "swizzle";
		case EXPR_TERNARY: return "ternary";
		case EXPR_TEST_HOOK: return "test_hook";
		case EXPR_TRY_UNWRAP: return "try_unwrap";
		case EXPR_TRY_UNWRAP_CHAIN: return "try_unwrap_chain";
		case EXPR_TYPEID: return "typeid";
		case EXPR_TYPEID_INFO: return "typeid_info";
		case EXPR_TYPEINFO: return "typeinfo";
		case EXPR_UNARY: return "unary";
		case EXPR_VASPLAT: return "vasplat";
		case EXPR_VECTOR_FROM_ARRAY: return "vector_from_array";
		case EXPR_EXT_TRUNC: return "ext_trunc";
		case EXPR_INT_TO_BOOL: return "int_to_bool";
		default: return "???";
	}
}
Expr *expr_negate_expr(Expr *expr)
{
	if (expr->expr_kind == EXPR_UNARY && expr->unary_expr.operator == UNARYOP_NEG)
	{
		return expr->inner_expr;
	}
	Expr *neg = expr_new_expr(EXPR_UNARY, expr);
	neg->unary_expr = (ExprUnary) { .operator = UNARYOP_NEG, .expr = expr };
	return neg;
}

bool expr_in_int_range(Expr *expr, int64_t low, int64_t high)
{
	ASSERT0(expr_is_const(expr) && expr->const_expr.const_kind == CONST_INTEGER);
	Int val = expr->const_expr.ixx;
	if (!int_fits(val, TYPE_I64)) return false;
	int64_t value = int_to_i64(val);
	return value >= low && value <= high;
}

bool expr_is_zero(Expr *expr)
{
	if (!sema_cast_const(expr)) return false;
	switch (expr->const_expr.const_kind)
	{
		case CONST_FLOAT:
			return !expr->const_expr.fxx.f;
		case CONST_INTEGER:
			return int_is_zero(expr->const_expr.ixx);
		case CONST_BOOL:
			return !expr->const_expr.b;
		case CONST_ENUM:
		case CONST_ERR:
			return !expr->const_expr.enum_err_val->enum_constant.ordinal;
		case CONST_BYTES:
		case CONST_STRING:
		{
			size_t len = expr->const_expr.bytes.len;
			for (size_t i = 0; i < len; i++)
			{
				if (expr->const_expr.bytes.ptr[i]) return false;
			}
			return true;
		}
		case CONST_POINTER:
			return !expr->const_expr.ptr;
		case CONST_TYPEID:
			return !expr->const_expr.typeid;
		case CONST_SLICE:
			return const_init_is_zero(expr->const_expr.slice_init);
		case CONST_INITIALIZER:
			return const_init_is_zero(expr->const_expr.initializer);
		case CONST_UNTYPED_LIST:
		{
			FOREACH(Expr *, e, expr->const_expr.untyped_list)
			{
				if (!expr_is_zero(e)) return false;
			}
			return true;
		}
		case CONST_REF:
			return !expr->const_expr.global_ref;
		case CONST_MEMBER:
			return false;
	}
	UNREACHABLE
}
bool expr_is_unwrapped_ident(Expr *expr)
{
	if (expr->expr_kind != EXPR_IDENTIFIER) return false;
	Decl *decl = expr->identifier_expr.decl;
	if (decl->decl_kind != DECL_VAR) return false;
	return decl->var.kind == VARDECL_UNWRAPPED && IS_OPTIONAL(decl->var.alias);
}

bool expr_may_addr(Expr *expr)
{
	if (IS_OPTIONAL(expr)) return false;
	switch (expr->expr_kind)
	{
		case EXPR_OTHER_CONTEXT:
			return expr_may_addr(expr->expr_other_context.inner);
		case EXPR_IDENTIFIER:
		{
			Decl *decl = expr->identifier_expr.decl;
			if (decl->decl_kind != DECL_VAR) return false;
			decl = decl_raw(decl);
			switch (decl->var.kind)
			{
				case VARDECL_LOCAL_CT:
				case VARDECL_LOCAL_CT_TYPE:
				case VARDECL_LOCAL:
				case VARDECL_GLOBAL:
				case VARDECL_PARAM:
				case VARDECL_PARAM_REF: // DEPRECATED
				case VARDECL_CONST:
					return true;
				case VARDECL_MEMBER:
				case VARDECL_BITMEMBER:
				case VARDECL_PARAM_CT:
				case VARDECL_PARAM_CT_TYPE:
				case VARDECL_PARAM_EXPR:
					return false;
				case VARDECL_UNWRAPPED:
				case VARDECL_ERASE:
				case VARDECL_REWRAPPED:
					UNREACHABLE
			}
		}
		case EXPR_UNARY:
			return expr->unary_expr.operator == UNARYOP_DEREF;
		case EXPR_BITACCESS:
		case EXPR_ACCESS:
			return expr_may_addr(expr->access_expr.parent);
		case EXPR_SUBSCRIPT:
		case EXPR_SLICE:
		case EXPR_MEMBER_GET:
			return true;
		case EXPR_BENCHMARK_HOOK:
		case EXPR_TEST_HOOK:
		case EXPR_VECTOR_FROM_ARRAY:
		case EXPR_ANYFAULT_TO_FAULT:
		case EXPR_VECTOR_TO_ARRAY:
		case EXPR_SLICE_TO_VEC_ARRAY:
		case EXPR_SCALAR_TO_VECTOR:
		case EXPR_PTR_ACCESS:
		case EXPR_ENUM_FROM_ORD:
		case EXPR_FLOAT_TO_INT:
		case EXPR_INT_TO_FLOAT:
		case EXPR_INT_TO_PTR:
		case EXPR_PTR_TO_INT:
		case EXPR_SLICE_LEN:
		case EXPR_RVALUE:
		case EXPR_RECAST:
		case EXPR_DISCARD:
		case EXPR_ADDR_CONVERSION:
			return false;
		case NON_RUNTIME_EXPR:
		case EXPR_ASM:
		case EXPR_BINARY:
		case EXPR_BITASSIGN:
		case EXPR_BUILTIN:
		case EXPR_BUILTIN_ACCESS:
		case EXPR_CALL:
		case EXPR_CAST:
		case EXPR_MAKE_ANY:
		case EXPR_CATCH_UNWRAP:
		case EXPR_COMPOUND_LITERAL:
		case EXPR_COND:
		case EXPR_CONST:
		case EXPR_DECL:
		case EXPR_DEFAULT_ARG:
		case EXPR_DESIGNATED_INITIALIZER_LIST:
		case EXPR_DESIGNATOR:
		case EXPR_EMBED:
		case EXPR_EXPRESSION_LIST:
		case EXPR_EXPR_BLOCK:
		case EXPR_FORCE_UNWRAP:
		case EXPR_GENERIC_IDENT:
		case EXPR_INITIALIZER_LIST:
		case EXPR_LAMBDA:
		case EXPR_LAST_FAULT:
		case EXPR_MACRO_BLOCK:
		case EXPR_MACRO_BODY:
		case EXPR_MACRO_BODY_EXPANSION:
		case EXPR_NAMED_ARGUMENT:
		case EXPR_NOP:
		case EXPR_OPERATOR_CHARS:
		case EXPR_OPTIONAL:
		case EXPR_POINTER_OFFSET:
		case EXPR_POST_UNARY:
		case EXPR_RETHROW:
		case EXPR_RETVAL:
		case EXPR_SLICE_ASSIGN:
		case EXPR_SLICE_COPY:
		case EXPR_SUBSCRIPT_ADDR:
		case EXPR_SUBSCRIPT_ASSIGN:
		case EXPR_SWIZZLE:
		case EXPR_TERNARY:
		case EXPR_TRY_UNWRAP:
		case EXPR_TRY_UNWRAP_CHAIN:
		case EXPR_TYPEID:
		case EXPR_TYPEID_INFO:
		case EXPR_VASPLAT:
		case EXPR_EXT_TRUNC:
		case EXPR_INT_TO_BOOL:
		case EXPR_MAKE_SLICE:
			return false;
	}
	UNREACHABLE
}


bool expr_is_runtime_const(Expr *expr)
{
	ASSERT0(expr->resolve_status == RESOLVE_DONE);
	RETRY:
	switch (expr->expr_kind)
	{
		case EXPR_OTHER_CONTEXT:
			expr = expr->expr_other_context.inner;
			goto RETRY;
		case EXPR_POINTER_OFFSET:
			return exprid_is_runtime_const(expr->pointer_offset_expr.ptr) && exprid_is_runtime_const(
					expr->pointer_offset_expr.offset);
		case EXPR_SWIZZLE:
		case EXPR_RETVAL:
		case EXPR_BUILTIN:
		case EXPR_CT_EVAL:
		case EXPR_VASPLAT:
		case EXPR_BENCHMARK_HOOK:
		case EXPR_TEST_HOOK:
		case EXPR_ANYSWITCH:
		case EXPR_BITASSIGN:
		case EXPR_TYPECALL:
		case EXPR_BINARY:
		case EXPR_OPERATOR_CHARS:
		case EXPR_STRINGIFY:
		case EXPR_CT_AND_OR:
		case EXPR_CT_CONCAT:
		case EXPR_CT_APPEND:
		case EXPR_CT_CASTABLE:
		case EXPR_CT_DEFINED:
		case EXPR_CT_IS_CONST:
		case EXPR_LAMBDA:
		case EXPR_EMBED:
		case EXPR_EXPR_BLOCK:
		case EXPR_DECL:
		case EXPR_CALL:
		case EXPR_CATCH_UNWRAP:
		case EXPR_MACRO_BODY_EXPANSION:
		case EXPR_TRY_UNWRAP:
		case EXPR_TRY_UNWRAP_CHAIN:
		case EXPR_POST_UNARY:
		case EXPR_SLICE_ASSIGN:
		case EXPR_SLICE_COPY:
		case EXPR_SPLAT:
		case EXPR_MACRO_BLOCK:
		case EXPR_RETHROW:
		case EXPR_MEMBER_GET:
		case EXPR_BITACCESS:
		case EXPR_COND:
		case EXPR_PTR_ACCESS:
		case EXPR_INT_TO_FLOAT:
		case EXPR_FLOAT_TO_INT:
		case EXPR_SLICE_LEN:
			return false;
		case EXPR_VECTOR_FROM_ARRAY:
		case EXPR_ANYFAULT_TO_FAULT:
		case EXPR_RVALUE:
		case EXPR_RECAST:
		case EXPR_ADDR_CONVERSION:
		case EXPR_DISCARD:
		case EXPR_INT_TO_PTR:
		case EXPR_PTR_TO_INT:
		case EXPR_ENUM_FROM_ORD:
		case EXPR_VECTOR_TO_ARRAY:
		case EXPR_SLICE_TO_VEC_ARRAY:
		case EXPR_SCALAR_TO_VECTOR:
			return expr_is_runtime_const(expr->inner_expr);
		case EXPR_MAKE_SLICE:
			expr = expr->make_slice_expr.ptr;
			if (!expr) return true;
			goto RETRY;
		case EXPR_MAKE_ANY:
			if (!expr_is_runtime_const(expr->make_any_expr.typeid)) return false;
			expr = expr->make_any_expr.inner;
			goto RETRY;
		case EXPR_ACCESS:
			expr = expr->access_expr.parent;
			goto RETRY;
		case EXPR_BUILTIN_ACCESS:
			switch (expr->builtin_access_expr.kind)
			{
				case ACCESS_ENUMNAME:
				case ACCESS_FAULTNAME:
				case ACCESS_FAULTORDINAL:
					break;
				case ACCESS_TYPEOFANYFAULT:
				case ACCESS_TYPEOFANY:
					break;
			}
			return exprid_is_runtime_const(expr->builtin_access_expr.inner);
		case EXPR_CAST:
			return exprid_is_runtime_const(expr->cast_expr.expr);
		case EXPR_INT_TO_BOOL:
			return expr_is_runtime_const(expr->int_to_bool_expr.inner);
		case EXPR_EXT_TRUNC:
			return expr_is_runtime_const(expr->ext_trunc_expr.inner);
		case EXPR_CONST:
			return true;
		case EXPR_DESIGNATOR:
			expr = expr->designator_expr.value;
			if (!expr) return true;
			goto RETRY;
		case EXPR_IDENTIFIER:
		{
			Decl *ident = expr->identifier_expr.decl;
			if (ident->decl_kind != DECL_VAR) return true;
			switch (ident->var.kind)
			{
				case VARDECL_CONST:
					if (ident->is_extern) return false;
					expr = ident->var.init_expr;
					goto RETRY;
				case VARDECL_PARAM_CT_TYPE:
				case VARDECL_LOCAL_CT_TYPE:
				case VARDECL_LOCAL_CT:
				case VARDECL_PARAM_CT:
					return true;
				default:
					return false;
			}
		}
		case EXPR_EXPRESSION_LIST:
			return expr_list_is_constant_eval(expr->expression_list);
		case EXPR_TYPEID_INFO:
			expr = exprptr(expr->typeid_info_expr.parent);
			goto RETRY;
		case EXPR_OPTIONAL:
			expr = expr->inner_expr;
			goto RETRY;
		case EXPR_DEFAULT_ARG:
			expr = expr->default_arg_expr.inner;
			goto RETRY;
		case EXPR_INITIALIZER_LIST:
			return expr_list_is_constant_eval(expr->initializer_list);
		case EXPR_DESIGNATED_INITIALIZER_LIST:
			return expr_list_is_constant_eval(expr->designated_init_list);
		case EXPR_SLICE:
			if (!exprid_is_runtime_const(expr->slice_expr.expr)) return false;
			return expr->slice_expr.range.range_type == RANGE_CONST_RANGE;
		case EXPR_SUBSCRIPT:
			if (!exprid_is_runtime_const(expr->subscript_expr.index.expr)) return false;
			expr = exprptr(expr->subscript_expr.expr);
			goto RETRY;
		case EXPR_SUBSCRIPT_ADDR:
			if (!exprid_is_runtime_const(expr->subscript_expr.index.expr)) return false;
			expr = exprptr(expr->subscript_expr.expr);
			if (expr->expr_kind == EXPR_IDENTIFIER)
			{
				Decl *decl = expr->identifier_expr.decl;
				if (decl->decl_kind == DECL_VAR)
				{
					switch (decl->var.kind)
					{
						case VARDECL_CONST:
						case VARDECL_GLOBAL:
							break;
						case VARDECL_LOCAL:
							if (decl->var.is_static) break;
						default:
							return false;
					}
					return true;
				}
			}
			goto RETRY;
		case EXPR_TERNARY:
			ASSERT0(!exprid_is_runtime_const(expr->ternary_expr.cond));
			return false;
		case EXPR_FORCE_UNWRAP:
		case EXPR_LAST_FAULT:
			return false;
		case EXPR_TYPEID:
			return true;
		case EXPR_UNARY:
			switch (expr->unary_expr.operator)
			{
				case UNARYOP_DEREF:
				case UNARYOP_ERROR:
					return false;
				case UNARYOP_ADDR:
					return expr_unary_addr_is_constant_eval(expr);
				case UNARYOP_TADDR:
					expr = expr->unary_expr.expr;
					goto RETRY;
				case UNARYOP_PLUS:
				case UNARYOP_NEG:
				case UNARYOP_BITNEG:
				case UNARYOP_NOT:
					expr = expr->unary_expr.expr;
					goto RETRY;
				case UNARYOP_INC:
				case UNARYOP_DEC:
					return false;
			}
			UNREACHABLE
		case EXPR_COMPILER_CONST:
			// Not foldable
			return false;
		case EXPR_CT_CALL:
		case EXPR_TYPEINFO:
		case EXPR_HASH_IDENT:
		case EXPR_CT_IDENT:
		case EXPR_COMPOUND_LITERAL:
		case EXPR_POISONED:
		case EXPR_CT_ARG:
		case EXPR_ASM:
		case EXPR_SUBSCRIPT_ASSIGN:
		case EXPR_GENERIC_IDENT:
		case EXPR_MACRO_BODY:
		case EXPR_NAMED_ARGUMENT:
			UNREACHABLE
		case EXPR_NOP:
			return true;
	}
	UNREACHABLE
}


static inline bool expr_list_is_constant_eval(Expr **exprs)
{
	FOREACH(Expr *, expr, exprs)
	{
		if (!expr_is_runtime_const(expr)) return false;
	}
	return true;
}

static inline bool expr_unary_addr_is_constant_eval(Expr *expr)
{
	// An address is never a constant value.
	Expr *inner = expr->unary_expr.expr;
	if (IS_OPTIONAL(inner)) return false;
	switch (inner->expr_kind)
	{
		case EXPR_ACCESS:
			return expr_is_runtime_const(inner);
		case EXPR_CONST:
		case EXPR_INITIALIZER_LIST:
		case EXPR_DESIGNATED_INITIALIZER_LIST:
			// We can't create temporaries as const locally or making them into compile time constants.
			return expr_is_runtime_const(inner);
		case EXPR_IDENTIFIER:
		{
			// The address of an identifier is side effect free.
			Decl *decl = inner->identifier_expr.decl;
			if (decl->decl_kind == DECL_FUNC) return true;
			if (decl->decl_kind != DECL_VAR) return false;
			switch (decl->var.kind)
			{
				case VARDECL_CONST:
				case VARDECL_GLOBAL:
					// Fine for both local and global init.
					return true;
				case VARDECL_LOCAL:
					// Getting the address of a local can never be constant init unless it is static.
					return decl->var.is_static;
				case VARDECL_PARAM:
				case VARDECL_MEMBER:
				case VARDECL_BITMEMBER:
				case VARDECL_PARAM_CT:
				case VARDECL_PARAM_CT_TYPE:
				case VARDECL_PARAM_REF: // DEPRECATED
				case VARDECL_PARAM_EXPR:
				case VARDECL_LOCAL_CT:
				case VARDECL_LOCAL_CT_TYPE:
				case VARDECL_UNWRAPPED:
				case VARDECL_ERASE:
				case VARDECL_REWRAPPED:
					// None of these are constant.
					return false;
			}
		}
		default:
			return false;
	}
}

void expr_insert_addr(Expr *original)
{
	ASSERT0(original->resolve_status == RESOLVE_DONE);
	if (original->expr_kind == EXPR_UNARY && original->unary_expr.operator == UNARYOP_DEREF)
	{
		*original = *original->unary_expr.expr;
		return;
	}
	Expr *inner = expr_copy(original);
	original->expr_kind = EXPR_UNARY;
	Type *inner_type = inner->type;
	bool optional = type_is_optional(inner->type);
	original->type = type_add_optional(type_get_ptr(type_no_optional(inner_type)), optional);
	original->unary_expr.operator = UNARYOP_ADDR;
	original->unary_expr.expr = inner;
}

Expr *expr_generate_decl(Decl *decl, Expr *assign)
{
	ASSERT0(decl->decl_kind == DECL_VAR);
	ASSERT0(decl->var.init_expr == NULL);
	Expr *expr_decl = expr_new(EXPR_DECL, decl->span);
	expr_decl->decl_expr = decl;
	if (!assign) decl->var.no_init = true;
	decl->var.init_expr = assign;
	return expr_decl;
}

static inline ConstInitializer *initializer_for_index(ConstInitializer *initializer, ArraySize index, bool from_back)
{
	switch (initializer->kind)
	{
		case CONST_INIT_ZERO:
		case CONST_INIT_STRUCT:
		case CONST_INIT_UNION:
		case CONST_INIT_VALUE:
			return initializer;
		case CONST_INIT_ARRAY_FULL:
		{
			unsigned len = vec_size(initializer->init_array_full);
			if (from_back)
			{
				if (index > len || !index) return NULL;
				index = len - index;
			}
			return initializer->init_array_full[index];
		}
		case CONST_INIT_ARRAY:
		{
			if (from_back)
			{
				ArraySize len = initializer->type->array.len;
				if (index > len || !index) return NULL;
				index = len - index;
			}
			FOREACH(ConstInitializer *, init, initializer->init_array.elements)
			{
				ASSERT0(init->kind == CONST_INIT_ARRAY_VALUE);
				if (init->init_array_value.index == index) return init->init_array_value.element;
			}
			return NULL;
		}
		case CONST_INIT_ARRAY_VALUE:
			UNREACHABLE
	}
	UNREACHABLE
}

void expr_rewrite_to_const_zero(Expr *expr, Type *type)
{
	expr->expr_kind = EXPR_CONST;
	Type *canonical = type->canonical;
	switch (canonical->type_kind)
	{
		case TYPE_POISONED:
		case TYPE_VOID:
		case TYPE_INFERRED_VECTOR:
		case TYPE_WILDCARD:
			UNREACHABLE
		case ALL_INTS:
			expr_rewrite_const_int(expr, type, 0);
			return;
		case ALL_FLOATS:
			expr_rewrite_const_float(expr, type, 0);
			break;
		case TYPE_BOOL:
			expr_rewrite_const_bool(expr, type, false);
			return;
		case TYPE_POINTER:
		case TYPE_FAULTTYPE:
		case TYPE_ANY:
		case TYPE_INTERFACE:
		case TYPE_ANYFAULT:
		case TYPE_TYPEID:
		case TYPE_FUNC_PTR:
			expr_rewrite_const_null(expr, type);
			return;
		case TYPE_ENUM:
			expr->const_expr.const_kind = CONST_ENUM;
			ASSERT0(canonical->decl->resolve_status == RESOLVE_DONE);
			expr->const_expr.enum_err_val = canonical->decl->enums.values[0];
			expr->resolve_status = RESOLVE_DONE;
			break;
		case TYPE_FUNC_RAW:
		case TYPE_TYPEDEF:
		case TYPE_OPTIONAL:
		case TYPE_TYPEINFO:
		case TYPE_MEMBER:
		case TYPE_UNTYPED_LIST:
		case TYPE_INFERRED_ARRAY:
		case TYPE_FLEXIBLE_ARRAY:
			UNREACHABLE
		case TYPE_SLICE:
			expr_rewrite_const_empty_slice(expr, type);
			return;
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_BITSTRUCT:
		case TYPE_VECTOR:
		case TYPE_ARRAY:
			expr_rewrite_const_initializer(expr, type, const_init_new_zero(type));
			return;
		case TYPE_DISTINCT:
			expr_rewrite_to_const_zero(expr, canonical->decl->distinct->type);
			break;
	}
	expr->type = type;
}

bool expr_rewrite_to_const_initializer_index(Type *list_type, ConstInitializer *list, Expr *result, unsigned index, bool from_back)
{
	ConstInitializer *initializer = initializer_for_index(list, index, from_back);
	ConstInitType kind = initializer ? initializer->kind : CONST_INIT_ZERO;
	switch (kind)
	{
		case CONST_INIT_ZERO:
		{
			Type *indexed_type = type_get_indexed_type(list_type);
			if (!indexed_type) return false;
			expr_rewrite_to_const_zero(result, indexed_type);
			return true;
		}
		case CONST_INIT_STRUCT:
		case CONST_INIT_UNION:
		case CONST_INIT_ARRAY:
		case CONST_INIT_ARRAY_FULL:
		case CONST_INIT_ARRAY_VALUE:
			return false;
		case CONST_INIT_VALUE:
			expr_replace(result, initializer->init_value);
			return true;
	}
	UNREACHABLE
}

// Determine if the expression has side effects
// Note! This is not the same as it being const.
bool expr_is_pure(Expr *expr)
{
	if (!expr) return true;
	switch (expr->expr_kind)
	{
		case EXPR_CAST:
			UNREACHABLE
		case EXPR_BUILTIN:
		case EXPR_BENCHMARK_HOOK:
		case EXPR_TEST_HOOK:
			return false;
		case EXPR_MAKE_SLICE:
			return expr_is_pure(expr->make_slice_expr.ptr);
		case EXPR_MAKE_ANY:
			return expr_is_pure(expr->make_any_expr.inner) && expr_is_pure(expr->make_any_expr.typeid);
		case EXPR_PTR_ACCESS:
		case EXPR_VECTOR_TO_ARRAY:
		case EXPR_SLICE_TO_VEC_ARRAY:
		case EXPR_SCALAR_TO_VECTOR:
		case EXPR_ENUM_FROM_ORD:
		case EXPR_INT_TO_FLOAT:
		case EXPR_INT_TO_PTR:
		case EXPR_PTR_TO_INT:
		case EXPR_FLOAT_TO_INT:
		case EXPR_SLICE_LEN:
		case EXPR_DISCARD:
		case EXPR_VECTOR_FROM_ARRAY:
		case EXPR_ANYFAULT_TO_FAULT:
		case EXPR_RVALUE:
		case EXPR_RECAST:
		case EXPR_ADDR_CONVERSION:
			return expr_is_pure(expr->inner_expr);
		case EXPR_INT_TO_BOOL:
			return expr_is_pure(expr->int_to_bool_expr.inner);
		case EXPR_EXT_TRUNC:
			return expr_is_pure(expr->ext_trunc_expr.inner);
		case EXPR_OTHER_CONTEXT:
			return expr_is_pure(expr->expr_other_context.inner);
		case EXPR_SWIZZLE:
			return exprid_is_pure(expr->swizzle_expr.parent);
		case EXPR_BUILTIN_ACCESS:
			return exprid_is_pure(expr->builtin_access_expr.inner);
		case EXPR_POINTER_OFFSET:
			return exprid_is_pure(expr->pointer_offset_expr.ptr) && exprid_is_pure(expr->pointer_offset_expr.offset);
		case EXPR_COMPILER_CONST:
		case EXPR_CONST:
		case EXPR_TYPECALL:
		case EXPR_CT_AND_OR:
		case EXPR_CT_CONCAT:
		case EXPR_CT_APPEND:
		case EXPR_CT_ARG:
		case EXPR_CT_CALL:
		case EXPR_CT_CASTABLE:
		case EXPR_CT_DEFINED:
		case EXPR_CT_IS_CONST:
		case EXPR_CT_EVAL:
		case EXPR_CT_IDENT:
		case EXPR_EMBED:
		case EXPR_IDENTIFIER:
		case EXPR_LAMBDA:
		case EXPR_MACRO_BODY:
		case EXPR_NOP:
		case EXPR_OPERATOR_CHARS:
		case EXPR_RETVAL:
		case EXPR_STRINGIFY:
		case EXPR_TYPEID:
		case EXPR_TYPEINFO:
		case EXPR_LAST_FAULT:
		case EXPR_MEMBER_GET:
			return true;
		case EXPR_BITASSIGN:
		case EXPR_VASPLAT:
		case EXPR_ANYSWITCH:
			return false;
		case EXPR_BINARY:
			// Anything with assignment is impure, otherwise true if sub expr are pure.
			if (expr->binary_expr.operator >= BINARYOP_ASSIGN) return false;
			return exprid_is_pure(expr->binary_expr.right) && exprid_is_pure(expr->binary_expr.left);
		case EXPR_UNARY:
			switch (expr->unary_expr.operator)
			{
				case UNARYOP_INC:
				case UNARYOP_DEC:
				case UNARYOP_TADDR:
					// ++ -- &&1
					return false;
				case UNARYOP_ERROR:
				case UNARYOP_DEREF:
				case UNARYOP_ADDR:
				case UNARYOP_NEG:
				case UNARYOP_BITNEG:
				case UNARYOP_NOT:
				case UNARYOP_PLUS:
					return expr_is_pure(expr->unary_expr.expr);
			}
			UNREACHABLE
		case EXPR_GENERIC_IDENT:
			return exprid_is_pure(expr->generic_ident_expr.parent);
		case EXPR_BITACCESS:
		case EXPR_ACCESS:
			// All access is pure if the parent is pure.
			return expr_is_pure(expr->access_expr.parent);
		case EXPR_POISONED:
			UNREACHABLE
		case EXPR_MACRO_BODY_EXPANSION:
		case EXPR_CALL:
		case EXPR_CATCH_UNWRAP:
		case EXPR_COMPOUND_LITERAL:
		case EXPR_COND:
		case EXPR_DESIGNATOR:
		case EXPR_DECL:
		case EXPR_EXPR_BLOCK:
		case EXPR_OPTIONAL:
		case EXPR_RETHROW:
		case EXPR_HASH_IDENT:
		case EXPR_MACRO_BLOCK:
		case EXPR_NAMED_ARGUMENT:
		case EXPR_INITIALIZER_LIST:
		case EXPR_DESIGNATED_INITIALIZER_LIST:
		case EXPR_POST_UNARY:
		case EXPR_SLICE_ASSIGN:
		case EXPR_SLICE_COPY:
		case EXPR_SPLAT:
		case EXPR_TRY_UNWRAP:
		case EXPR_TRY_UNWRAP_CHAIN:
		case EXPR_FORCE_UNWRAP:
		case EXPR_SUBSCRIPT_ASSIGN:
			return false;
		case EXPR_EXPRESSION_LIST:
		{
			FOREACH(Expr *, e, expr->expression_list)
			{
				if (!expr_is_pure(e)) return false;
			}
			return true;
		}
		case EXPR_TYPEID_INFO:
			return exprid_is_pure(expr->typeid_info_expr.parent);
		case EXPR_SLICE:
			if (!exprid_is_pure(expr->slice_expr.expr)) return false;
			switch (expr->slice_expr.range.range_type)
			{
				case RANGE_CONST_RANGE:
					return true;
				case RANGE_CONST_END:
				case RANGE_CONST_LEN:
					return exprid_is_pure(expr->slice_expr.range.start);
				case RANGE_DYNAMIC:
					return exprid_is_pure(expr->slice_expr.range.start)
					       && exprid_is_pure(expr->slice_expr.range.end);
			}
			UNREACHABLE
		case EXPR_SUBSCRIPT:
		case EXPR_SUBSCRIPT_ADDR:
			return exprid_is_pure(expr->subscript_expr.expr)
				   && exprid_is_pure(expr->subscript_expr.index.expr);
		case EXPR_TERNARY:
			return exprid_is_pure(expr->ternary_expr.cond)
				   && exprid_is_pure(expr->ternary_expr.else_expr)
				   && exprid_is_pure(expr->ternary_expr.then_expr);
		case EXPR_ASM:
			return false;
		case EXPR_DEFAULT_ARG:
			return expr_is_pure(expr->default_arg_expr.inner);
	}
	UNREACHABLE
}


bool expr_is_simple(Expr *expr, bool to_float)
{
	RETRY:
	switch (expr->expr_kind)
	{
		case EXPR_TERNARY:
			return exprid_is_simple(expr->ternary_expr.else_expr, to_float) && exprid_is_simple(expr->ternary_expr.then_expr, to_float);
		case EXPR_RETHROW:
			expr = expr->rethrow_expr.inner;
			goto RETRY;
		default:
			return true;
		case EXPR_BINARY:
			switch (expr->binary_expr.operator)
			{
				case BINARYOP_DIV:
					if (to_float) return false;
					FALLTHROUGH;
				case BINARYOP_MOD:
				case BINARYOP_ELSE:
					return exprid_is_simple(expr->binary_expr.left, to_float) && exprid_is_simple(expr->binary_expr.right, to_float);
				case BINARYOP_AND:
				case BINARYOP_OR:
				case BINARYOP_GT:
				case BINARYOP_GE:
				case BINARYOP_LT:
				case BINARYOP_LE:
				case BINARYOP_NE:
				case BINARYOP_EQ:
				case BINARYOP_VEC_GT:
				case BINARYOP_VEC_GE:
				case BINARYOP_VEC_LT:
				case BINARYOP_VEC_LE:
				case BINARYOP_VEC_NE:
				case BINARYOP_VEC_EQ:
				case BINARYOP_ASSIGN:
				case BINARYOP_ADD_ASSIGN:
				case BINARYOP_BIT_AND_ASSIGN:
				case BINARYOP_BIT_OR_ASSIGN:
				case BINARYOP_BIT_XOR_ASSIGN:
				case BINARYOP_DIV_ASSIGN:
				case BINARYOP_MOD_ASSIGN:
				case BINARYOP_MULT_ASSIGN:
				case BINARYOP_SHR_ASSIGN:
				case BINARYOP_SHL_ASSIGN:
				case BINARYOP_SUB_ASSIGN:
					return true;
				case BINARYOP_SHL:
				case BINARYOP_SHR:
					return to_float;
				default:
					return false;
			}
			UNREACHABLE
		case EXPR_UNARY:
			switch (expr->unary_expr.operator)
			{
				case UNARYOP_BITNEG:
					return to_float;
				case UNARYOP_NEG:
					return false;
				default:
					return true;
			}
			UNREACHABLE
	}
	UNREACHABLE
}


Expr *expr_new(ExprKind kind, SourceSpan start)
{
	Expr *expr = expr_calloc();
	expr->expr_kind = kind;
	expr->span = start;
	return expr;
}

Expr *expr_new_const_int(SourceSpan span, Type *type, uint64_t v)
{
	Expr *expr = expr_calloc();
	expr->expr_kind = EXPR_CONST;
	expr->span = span;
	expr->type = type;
	TypeKind kind = type_flatten(type)->type_kind;
	expr->const_expr.ixx.i.high = 0;
	if (type_kind_is_signed(kind))
	{
		if (v > (uint64_t)INT64_MAX) expr->const_expr.ixx.i.high = UINT64_MAX;
	}
	expr->const_expr.ixx.i.low = v;
	expr->const_expr.ixx.type = kind;
	expr->const_expr.is_character = false;
	expr->const_expr.const_kind = CONST_INTEGER;
	expr->resolve_status = RESOLVE_DONE;
	return expr;
}

Expr *expr_new_const_typeid(SourceSpan span, Type *type)
{
	Expr *expr = expr_calloc();
	expr->expr_kind = EXPR_CONST;
	expr->span = span;
	expr->type = type_typeid;
	expr->const_expr.const_kind = CONST_TYPEID;
	expr->const_expr.typeid = type;
	expr->resolve_status = RESOLVE_DONE;
	return expr;
}

Expr *expr_new_const_initializer(SourceSpan span, Type *type, ConstInitializer *initializer)
{
	Expr *expr = expr_calloc();
	expr->expr_kind = EXPR_CONST;
	expr->span = span;
	expr->type = type;
	expr->const_expr.initializer = initializer;
	expr->const_expr.const_kind = CONST_INITIALIZER;
	expr->resolve_status = RESOLVE_DONE;
	return expr;
}

Expr *expr_new_const_bool(SourceSpan span, Type *type, bool value)
{
	Expr *expr = expr_calloc();
	expr->expr_kind = EXPR_CONST;
	expr->span = span;
	expr->type = type;
	ASSERT0(type_flatten(type)->type_kind == TYPE_BOOL);
	expr->const_expr.b = value;
	expr->const_expr.const_kind = CONST_BOOL;
	expr->resolve_status = RESOLVE_DONE;
	return expr;
}

Expr *expr_new_const_string(SourceSpan span, const char *string)
{
	Expr *expr = expr_calloc();
	expr->expr_kind = EXPR_CONST;
	expr->span = span;
	expr->type = type_string;
	expr->const_expr.const_kind = CONST_STRING;
	expr->const_expr.bytes.ptr = string;
	expr->const_expr.bytes.len = strlen(string);
	expr->resolve_status = RESOLVE_DONE;
	return expr;
}

void expr_rewrite_to_builtin_access(Expr *expr, Expr *parent, BuiltinAccessKind kind, Type *type)
{
	expr->expr_kind = EXPR_BUILTIN_ACCESS;
	expr->builtin_access_expr.kind = kind;
	expr->builtin_access_expr.inner = exprid(parent);
	expr->type = type_add_optional(type, IS_OPTIONAL(parent));
	expr->resolve_status = RESOLVE_DONE;
}


Expr *expr_variable(Decl *decl)
{
	if (decl->resolve_status == RESOLVE_DONE)
	{
		Expr *expr = expr_new(EXPR_IDENTIFIER, decl->span);
		expr_resolve_ident(expr, decl);
		return expr;
	}
	Expr *expr = expr_new(EXPR_IDENTIFIER, decl->span);
	expr->identifier_expr.ident = decl->name;
	expr->resolve_status = RESOLVE_NOT_DONE;
	return expr;
}

void expr_rewrite_insert_deref(Expr *original)
{
	// Assume *(&x) => x
	if (original->expr_kind == EXPR_UNARY && original->unary_expr.operator == UNARYOP_ADDR)
	{
		*original = *original->unary_expr.expr;
		return;
	}

	// Allocate our new and create our new inner, and overwrite the original.
	Expr *inner = expr_copy(original);
	original->expr_kind = EXPR_UNARY;
	original->type = NULL;
	original->unary_expr.operator = UNARYOP_DEREF;
	original->unary_expr.expr = inner;

	// In the case the original is already resolved, we want to resolve the deref as well.
	if (original->resolve_status == RESOLVE_DONE)
	{
		Type *no_fail  = type_no_optional(inner->type);
		ASSERT0(no_fail->canonical->type_kind == TYPE_POINTER);

		// Only fold to the canonical type if it wasn't a pointer.
		Type *pointee = no_fail->type_kind == TYPE_POINTER ? no_fail->pointer : no_fail->canonical->pointer;
		original->type = type_add_optional(pointee, IS_OPTIONAL(inner));
	}
}

void expr_rewrite_const_ref(Expr *expr_to_rewrite, Decl *decl)
{
	expr_to_rewrite->const_expr = (ExprConst) {
			.global_ref = decl,
			.const_kind = CONST_REF
	};
	expr_to_rewrite->expr_kind = EXPR_CONST;
}

void expr_rewrite_const_string(Expr *expr_to_rewrite, const char *string)
{
	expr_to_rewrite->expr_kind = EXPR_CONST;
	expr_to_rewrite->const_expr.const_kind = CONST_STRING;
	expr_to_rewrite->const_expr.bytes.ptr = (char *)string;
	ArraySize len = (ArraySize)strlen(string);
	expr_to_rewrite->const_expr.bytes.len = len;
	expr_to_rewrite->resolve_status = RESOLVE_DONE;
	expr_to_rewrite->type = type_string;
}

void expr_rewrite_to_binary(Expr *expr_to_rewrite, Expr *left, Expr *right, BinaryOp op)
{
	expr_to_rewrite->binary_expr = (ExprBinary) { .operator = op, .left = exprid(left), .right = exprid(right) };
	expr_to_rewrite->expr_kind = EXPR_BINARY;
}
