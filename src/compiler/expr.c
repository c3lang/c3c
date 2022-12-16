// Copyright (c) 2022 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"

static inline bool expr_binary_is_constant_eval(Expr *expr, ConstantEvalKind eval_kind);
static inline bool expr_cast_is_constant_eval(Expr *expr, ConstantEvalKind eval_kind);
static inline bool expr_list_is_constant_eval(Expr **exprs, ConstantEvalKind eval_kind);
static inline bool expr_unary_addr_is_constant_eval(Expr *expr, ConstantEvalKind eval_kind);
static inline ConstInitializer *initializer_for_index(ConstInitializer *initializer, uint32_t index);

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
	assert(expr_is_const(expr) && expr->const_expr.const_kind == CONST_INTEGER);
	Int val = expr->const_expr.ixx;
	if (!int_fits(val, TYPE_I64)) return false;
	int64_t value = int_to_i64(val);
	return value >= low && value <= high;
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
				case VARDECL_PARAM_REF:
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
		case EXPR_GROUP:
			return expr_may_addr(expr->inner_expr);
		case EXPR_SUBSCRIPT:
		case EXPR_SLICE:
			return true;
		case EXPR_TEST_HOOK:
			return false;
		case EXPR_ARGV_TO_SUBARRAY:
		case EXPR_ASM:
		case EXPR_BINARY:
		case EXPR_BITASSIGN:
		case EXPR_BUILTIN:
		case EXPR_BUILTIN_ACCESS:
		case EXPR_CALL:
		case EXPR_CAST:
		case EXPR_CATCH:
		case EXPR_CATCH_UNWRAP:
		case EXPR_COMPILER_CONST:
		case EXPR_COMPOUND_LITERAL:
		case EXPR_COND:
		case EXPR_CONST:
		case EXPR_CT_ARG:
		case EXPR_CT_CALL:
		case EXPR_CT_CHECKS:
		case EXPR_CT_EVAL:
		case EXPR_CT_IDENT:
		case EXPR_DECL:
		case EXPR_DESIGNATED_INITIALIZER_LIST:
		case EXPR_DESIGNATOR:
		case EXPR_EXPRESSION_LIST:
		case EXPR_EXPR_BLOCK:
		case EXPR_FAILABLE:
		case EXPR_FLATPATH:
		case EXPR_FORCE_UNWRAP:
		case EXPR_HASH_IDENT:
		case EXPR_INITIALIZER_LIST:
		case EXPR_MACRO_BLOCK:
		case EXPR_MACRO_BODY_EXPANSION:
		case EXPR_NOP:
		case EXPR_OPERATOR_CHARS:
		case EXPR_POINTER_OFFSET:
		case EXPR_POISONED:
		case EXPR_POST_UNARY:
		case EXPR_RETHROW:
		case EXPR_RETVAL:
		case EXPR_SLICE_ASSIGN:
		case EXPR_SLICE_COPY:
		case EXPR_STRINGIFY:
		case EXPR_SUBSCRIPT_ADDR:
		case EXPR_SUBSCRIPT_ASSIGN:
		case EXPR_TERNARY:
		case EXPR_TRY:
		case EXPR_TRY_UNWRAP:
		case EXPR_TRY_UNWRAP_CHAIN:
		case EXPR_TYPEID:
		case EXPR_TYPEID_INFO:
		case EXPR_TYPEINFO:
		case EXPR_VARIANT:
		case EXPR_VARIANTSWITCH:
		case EXPR_VASPLAT:
			return false;
	}
	UNREACHABLE
}

static inline bool expr_binary_is_constant_eval(Expr *expr, ConstantEvalKind eval_kind)
{
	if (expr->binary_expr.operator >= BINARYOP_ASSIGN) return false;
	// Pointer add is already handled.
	if (eval_kind == CONSTANT_EVAL_GLOBAL_INIT) return false;
	Expr *left = exprptr(expr->binary_expr.left);
	Expr *right = exprptr(expr->binary_expr.right);
	if (!expr_is_constant_eval(left, eval_kind)) return false;
	if (!expr_is_constant_eval(right, eval_kind)) return false;
	return true;
}

bool expr_is_constant_eval(Expr *expr, ConstantEvalKind eval_kind)
{
	assert(expr->resolve_status == RESOLVE_DONE);
	RETRY:
	switch (expr->expr_kind)
	{
		case EXPR_POINTER_OFFSET:
			return exprid_is_constant_eval(expr->pointer_offset_expr.ptr, eval_kind) && exprid_is_constant_eval(expr->pointer_offset_expr.offset, eval_kind);
		case EXPR_RETVAL:
			return false;
		case EXPR_BUILTIN:
		case EXPR_CT_EVAL:
		case EXPR_VASPLAT:
		case EXPR_TEST_HOOK:
			return false;
		case EXPR_BITACCESS:
		case EXPR_ACCESS:
			expr = expr->access_expr.parent;
			goto RETRY;
		case EXPR_VARIANTSWITCH:
			return false;
		case EXPR_BITASSIGN:
			return false;
		case EXPR_BUILTIN_ACCESS:
			switch (expr->builtin_access_expr.kind)
			{
				case ACCESS_ENUMNAME:
				case ACCESS_FAULTNAME:
				case ACCESS_LEN:
				case ACCESS_PTR:
					break;
				case ACCESS_TYPEOFANY:
					if (eval_kind != CONSTANT_EVAL_NO_SIDE_EFFECTS) return false;
					break;
			}
			return exprid_is_constant_eval(expr->builtin_access_expr.inner, eval_kind);
		case EXPR_VARIANT:
			return exprid_is_constant_eval(expr->variant_expr.type_id, eval_kind) && exprid_is_constant_eval(expr->variant_expr.ptr, eval_kind);
		case EXPR_BINARY:
			return expr_binary_is_constant_eval(expr, eval_kind);
		case EXPR_CAST:
			return expr_cast_is_constant_eval(expr, eval_kind);
		case EXPR_CONST:
		case EXPR_OPERATOR_CHARS:
		case EXPR_STRINGIFY:
		case EXPR_CT_CHECKS:
			return true;
		case EXPR_COND:
			return expr_list_is_constant_eval(expr->cond_expr, eval_kind);
		case EXPR_DESIGNATOR:
			expr = expr->designator_expr.value;
			goto RETRY;
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
		case EXPR_MACRO_BLOCK:
		case EXPR_RETHROW:
			return false;
		case EXPR_IDENTIFIER:
			if (expr->identifier_expr.decl->decl_kind != DECL_VAR) return true;
			if (expr->identifier_expr.decl->var.kind == VARDECL_CONST)
			{
				expr = expr->identifier_expr.decl->var.init_expr;
				goto RETRY;
			}
			switch (expr->identifier_expr.decl->var.kind)
			{
				case VARDECL_CONST:
				case VARDECL_PARAM_CT_TYPE:
				case VARDECL_LOCAL_CT_TYPE:
				case VARDECL_LOCAL_CT:
				case VARDECL_PARAM_CT:
					return true;
				default:
					return false;
			}
		case EXPR_EXPRESSION_LIST:
			return expr_list_is_constant_eval(expr->expression_list, eval_kind);
		case EXPR_TYPEID_INFO:
			expr = exprptr(expr->typeid_info_expr.parent);
			goto RETRY;
		case EXPR_FAILABLE:
		case EXPR_GROUP:
			expr = expr->inner_expr;
			goto RETRY;
		case EXPR_INITIALIZER_LIST:
			return expr_list_is_constant_eval(expr->initializer_list, eval_kind);
		case EXPR_DESIGNATED_INITIALIZER_LIST:
			return expr_list_is_constant_eval(expr->designated_init_list, eval_kind);
		case EXPR_SLICE:
			return false;
			/*
			if (expr->slice_expr.start && !exprid_is_constant_eval(expr->slice_expr.start, eval_kind)) return false;
			if (expr->slice_expr.end && !exprid_is_constant_eval(expr->slice_expr.end, CONSTANT_EVAL_FOLDABLE)) return false;
			return exprid_is_constant_eval(expr->slice_expr.expr, eval_kind);*/
		case EXPR_SUBSCRIPT:
			if (!exprid_is_constant_eval(expr->subscript_expr.range.start, eval_kind)) return false;
			expr = exprptr(expr->subscript_expr.expr);
			goto RETRY;
		case EXPR_SUBSCRIPT_ADDR:
			if (!exprid_is_constant_eval(expr->subscript_expr.range.start, eval_kind)) return false;
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
			assert(!exprid_is_constant_eval(expr->ternary_expr.cond, eval_kind));
			return false;
		case EXPR_FORCE_UNWRAP:
		case EXPR_TRY:
		case EXPR_CATCH:
			expr = expr->inner_expr;
			goto RETRY;
		case EXPR_TYPEID:
			return eval_kind != CONSTANT_EVAL_CONSTANT_VALUE;
		case EXPR_UNARY:
			switch (expr->unary_expr.operator)
			{
				case UNARYOP_DEREF:
				case UNARYOP_ERROR:
					return false;
				case UNARYOP_ADDR:
					return expr_unary_addr_is_constant_eval(expr, eval_kind);
				case UNARYOP_TADDR:
					if (eval_kind == CONSTANT_EVAL_CONSTANT_VALUE || eval_kind == CONSTANT_EVAL_LOCAL_INIT) return false;
					expr = expr->unary_expr.expr;
					goto RETRY;
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
		case EXPR_FLATPATH:
		case EXPR_COMPOUND_LITERAL:
		case EXPR_POISONED:
		case EXPR_ARGV_TO_SUBARRAY:
		case EXPR_CT_ARG:
		case EXPR_ASM:
		case EXPR_SUBSCRIPT_ASSIGN:
			UNREACHABLE
		case EXPR_NOP:
			return true;
	}
	UNREACHABLE
}

static inline bool expr_cast_is_constant_eval(Expr *expr, ConstantEvalKind eval_kind)
{
	switch (expr->cast_expr.kind)
	{
		case CAST_ERROR:
			UNREACHABLE
		case CAST_BSINT:
		case CAST_BSARRY:
			return true;
		case CAST_INTENUM:
		case CAST_ANYPTR:
		case CAST_ERBOOL:
		case CAST_EUBOOL:
		case CAST_EUER:
		case CAST_EREU:
		case CAST_XIERR:
		case CAST_STRPTR:
		case CAST_PTRBOOL:
		case CAST_BOOLINT:
		case CAST_BOOLFP:
		case CAST_BOOLBOOL:
		case CAST_FPBOOL:
		case CAST_INTBOOL:
		case CAST_FPFP:
		case CAST_FPSI:
		case CAST_FPUI:
		case CAST_SISI:
		case CAST_SIUI:
		case CAST_SIFP:
		case CAST_UISI:
		case CAST_UIUI:
		case CAST_UIFP:
		case CAST_SABOOL:
		case CAST_STST:
		case CAST_VECARR:
		case CAST_ARRVEC:
		case CAST_BOOLVECINT:
			if (eval_kind != CONSTANT_EVAL_NO_SIDE_EFFECTS) return false;
			return exprid_is_constant_eval(expr->cast_expr.expr, eval_kind);
		case CAST_XIPTR:
		case CAST_PTRPTR:
		case CAST_APTSA:
		case CAST_SAPTR:
		case CAST_SASA:
		case CAST_ENUMLOW:
			return exprid_is_constant_eval(expr->cast_expr.expr, eval_kind);
		case CAST_PTRANY:
			if (eval_kind == CONSTANT_EVAL_LOCAL_INIT || eval_kind == CONSTANT_EVAL_CONSTANT_VALUE) return false;
			return exprid_is_constant_eval(expr->cast_expr.expr, eval_kind);
		case CAST_EUINT:
		case CAST_ERINT:
		case CAST_PTRXI:
			if (eval_kind == CONSTANT_EVAL_CONSTANT_VALUE) return false;
			return exprid_is_constant_eval(expr->cast_expr.expr, eval_kind);
	}
	UNREACHABLE
}

static inline bool expr_list_is_constant_eval(Expr **exprs, ConstantEvalKind eval_kind)
{
	VECEACH(exprs, i)
	{
		if (!expr_is_constant_eval(exprs[i], eval_kind)) return false;
	}
	return true;
}

static inline bool expr_unary_addr_is_constant_eval(Expr *expr, ConstantEvalKind eval_kind)
{
	// An address is never a constant value.
	if (eval_kind == CONSTANT_EVAL_CONSTANT_VALUE) return false;
	Expr *inner = expr->unary_expr.expr;
	switch (inner->expr_kind)
	{
		case EXPR_CONST:
		case EXPR_INITIALIZER_LIST:
		case EXPR_DESIGNATED_INITIALIZER_LIST:
			// We can't create temporaries as const locally or making them into compile time constants.
			if (eval_kind == CONSTANT_EVAL_LOCAL_INIT) return false;
			return expr_is_constant_eval(inner, eval_kind);
		case EXPR_IDENTIFIER:
		{
			// The address of an identifier is side effect free.
			if (eval_kind == CONSTANT_EVAL_NO_SIDE_EFFECTS) return true;
			Decl *decl = inner->identifier_expr.decl;
			if (decl->decl_kind == DECL_FUNC) return true;
			if (decl->decl_kind != DECL_VAR) return false;
			assert(eval_kind == CONSTANT_EVAL_LOCAL_INIT || eval_kind == CONSTANT_EVAL_GLOBAL_INIT);
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
				case VARDECL_PARAM_REF:
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
	assert(original->resolve_status == RESOLVE_DONE);
	if (original->expr_kind == EXPR_UNARY && original->unary_expr.operator == UNARYOP_DEREF)
	{
		*original = *original->unary_expr.expr;
		return;
	}
	Expr *inner = expr_copy(original);
	original->expr_kind = EXPR_UNARY;
	Type *inner_type = inner->type;
	bool failable = type_is_optional(inner->type);
	original->type = type_add_optional(type_get_ptr(type_no_optional(inner_type)), failable);
	original->unary_expr.operator = UNARYOP_ADDR;
	original->unary_expr.expr = inner;
}

Expr *expr_generate_decl(Decl *decl, Expr *assign)
{
	assert(decl->decl_kind == DECL_VAR);
	assert(decl->var.init_expr == NULL);
	Expr *expr_decl = expr_new(EXPR_DECL, decl->span);
	expr_decl->decl_expr = decl;
	if (!assign) decl->var.no_init = true;
	decl->var.init_expr = assign;
	return expr_decl;
}

bool expr_may_splat_as_vararg(Expr *expr, Type *variadic_base_type)
{
	Type *base_type = variadic_base_type->canonical;
	Type *canonical = expr->type->canonical;
	switch (canonical->type_kind)
	{
		case TYPE_ARRAY:
		case TYPE_SUBARRAY:
			return canonical->array.base == base_type;
		case TYPE_POINTER:
			if (canonical->pointer->type_kind == TYPE_ARRAY) return canonical->pointer->array.base == base_type;
			return false;
		default:
			return false;
	}
}

bool expr_is_compile_time(Expr *expr)
{
	switch (expr->expr_kind)
	{
		case EXPR_CONST:
			return true;
		case EXPR_MACRO_BLOCK:
		{
			AstId current = expr->macro_block.first_stmt;
			while (current)
			{
				if (!ast_is_compile_time(ast_next(&current))) return false;
			}
			return true;
		}
		default:
			return false;
	}
	UNREACHABLE
}

static inline ConstInitializer *initializer_for_index(ConstInitializer *initializer, uint32_t index)
{
	switch (initializer->kind)
	{
		case CONST_INIT_ZERO:
		case CONST_INIT_STRUCT:
		case CONST_INIT_UNION:
		case CONST_INIT_VALUE:
			return initializer;
		case CONST_INIT_ARRAY_FULL:
			return initializer->init_array_full[index];
		case CONST_INIT_ARRAY:
		{
			ConstInitializer **sub_values = initializer->init_array.elements;
			VECEACH(sub_values, i)
			{
				ConstInitializer *init = sub_values[i];
				assert(init->kind == CONST_INIT_ARRAY_VALUE);
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
	expr->const_expr.narrowable = true;
	switch (type->canonical->type_kind)
	{
		case TYPE_POISONED:
		case TYPE_VOID:
		case TYPE_INFERRED_VECTOR:
			UNREACHABLE
		case ALL_INTS:
			expr_rewrite_const_int(expr, type, 0, true);
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
		case TYPE_ANYERR:
		case TYPE_TYPEID:
			expr_rewrite_const_null(expr, type);
			return;
		case TYPE_ENUM:
			expr->const_expr.const_kind = CONST_ENUM;
			expr->const_expr.enum_err_val = type->decl->enums.values[0];
			break;
		case TYPE_FUNC:
		case TYPE_TYPEDEF:
		case TYPE_FAILABLE_ANY:
		case TYPE_OPTIONAL:
		case TYPE_TYPEINFO:
		case TYPE_MEMBER:
			UNREACHABLE
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_BITSTRUCT:
		case TYPE_ARRAY:
		case TYPE_SUBARRAY:
		case TYPE_INFERRED_ARRAY:
		case TYPE_FLEXIBLE_ARRAY:
		case TYPE_UNTYPED_LIST:
		case TYPE_SCALED_VECTOR:
		case TYPE_VECTOR:
		{
			ConstInitializer *init = CALLOCS(ConstInitializer);
			init->kind = CONST_INIT_ZERO;
			init->type = type;
			expr_rewrite_const_initializer(expr, type, init);
			return;
		}
		case TYPE_DISTINCT:
			expr_rewrite_to_const_zero(expr, type->decl->distinct_decl.base_type);
			break;
	}
	expr->type = type;
}

bool expr_rewrite_to_const_initializer_index(Type *list_type, ConstInitializer *list, Expr *result, unsigned index)
{
	ConstInitializer *initializer = initializer_for_index(list, index);
	ConstInitType kind = initializer ? initializer->kind : CONST_INIT_ZERO;
	switch (kind)
	{
		case CONST_INIT_ZERO:
			expr_rewrite_to_const_zero(result, type_get_indexed_type(list_type));
			return true;
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
		case EXPR_BUILTIN:
		case EXPR_TEST_HOOK:
			return false;
		case EXPR_BUILTIN_ACCESS:
			return exprid_is_pure(expr->builtin_access_expr.inner);
		case EXPR_VARIANT:
			return exprid_is_pure(expr->variant_expr.type_id) && exprid_is_pure(expr->variant_expr.ptr);
		case EXPR_POINTER_OFFSET:
			return exprid_is_pure(expr->pointer_offset_expr.ptr) && exprid_is_pure(expr->pointer_offset_expr.offset);
		case EXPR_COMPILER_CONST:
		case EXPR_CONST:
		case EXPR_IDENTIFIER:
		case EXPR_NOP:
		case EXPR_STRINGIFY:
		case EXPR_RETVAL:
		case EXPR_TYPEINFO:
		case EXPR_CT_EVAL:
		case EXPR_CT_IDENT:
		case EXPR_CT_CALL:
		case EXPR_TYPEID:
		case EXPR_CT_ARG:
		case EXPR_OPERATOR_CHARS:
		case EXPR_CT_CHECKS:
			return true;
		case EXPR_VASPLAT:
			return true;
		case EXPR_ARGV_TO_SUBARRAY:
		case EXPR_BITASSIGN:
			return false;
		case EXPR_VARIANTSWITCH:
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
					return expr_is_pure(expr->unary_expr.expr);
			}
			UNREACHABLE
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
		case EXPR_FAILABLE:
		case EXPR_RETHROW:
		case EXPR_HASH_IDENT:
		case EXPR_MACRO_BLOCK:
		case EXPR_FLATPATH:
		case EXPR_INITIALIZER_LIST:
		case EXPR_DESIGNATED_INITIALIZER_LIST:
		case EXPR_POST_UNARY:
		case EXPR_SLICE_ASSIGN:
		case EXPR_SLICE_COPY:
		case EXPR_TRY_UNWRAP:
		case EXPR_TRY_UNWRAP_CHAIN:
		case EXPR_FORCE_UNWRAP:
		case EXPR_SUBSCRIPT_ASSIGN:
			return false;
		case EXPR_CAST:
			return exprid_is_pure(expr->cast_expr.expr);
		case EXPR_EXPRESSION_LIST:
			VECEACH(expr->expression_list, i)
			{
				if (!expr_is_pure(expr->expression_list[i])) return false;
			}
			return true;
		case EXPR_TYPEID_INFO:
			return exprid_is_pure(expr->typeid_info_expr.parent);
		case EXPR_SLICE:
			return exprid_is_pure(expr->subscript_expr.expr)
			       && exprid_is_pure(expr->subscript_expr.range.start)
			       && exprid_is_pure(expr->subscript_expr.range.end);
		case EXPR_SUBSCRIPT:
		case EXPR_SUBSCRIPT_ADDR:
			return exprid_is_pure(expr->subscript_expr.expr)
			       && exprid_is_pure(expr->subscript_expr.range.start);
		case EXPR_TERNARY:
			return exprid_is_pure(expr->ternary_expr.cond)
			       && exprid_is_pure(expr->ternary_expr.else_expr)
			       && exprid_is_pure(expr->ternary_expr.then_expr);
		case EXPR_ASM:
			return false;
		case EXPR_TRY:
		case EXPR_GROUP:
		case EXPR_CATCH:
			return expr_is_pure(expr->inner_expr);
	}
	UNREACHABLE
}


bool expr_is_simple(Expr *expr)
{
	RETRY:
	switch (expr->expr_kind)
	{
		case EXPR_GROUP:
			expr = expr->inner_expr;
			goto RETRY;
		case EXPR_TERNARY:
			return expr_is_simple(exprptr(expr->ternary_expr.else_expr)) && expr_is_simple(exprptr(expr->ternary_expr.then_expr));
		case EXPR_RETHROW:
			expr = expr->rethrow_expr.inner;
			goto RETRY;
		default:
			return true;
		case EXPR_BINARY:
			switch (expr->binary_expr.operator)
			{
				case BINARYOP_AND:
				case BINARYOP_OR:
				case BINARYOP_GT:
				case BINARYOP_GE:
				case BINARYOP_LT:
				case BINARYOP_LE:
				case BINARYOP_NE:
				case BINARYOP_EQ:
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
				default:
					return false;
			}
			UNREACHABLE
		case EXPR_UNARY:
			switch (expr->unary_expr.operator)
			{
				case UNARYOP_NEG:
				case UNARYOP_BITNEG:
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

Expr *expr_new_const_int(SourceSpan span, Type *type, uint64_t v, bool narrowable)
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
	expr->const_expr.const_kind = CONST_INTEGER;
	expr->const_expr.narrowable = narrowable;
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

Expr *expr_new_const_bool(SourceSpan span, Type *type, bool value)
{
	Expr *expr = expr_calloc();
	expr->expr_kind = EXPR_CONST;
	expr->span = span;
	expr->type = type;
	assert(type_flatten(type)->type_kind == TYPE_BOOL);
	expr->const_expr.b = value;
	expr->const_expr.const_kind = CONST_BOOL;
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
		expr->identifier_expr.decl = decl;
		expr->resolve_status = RESOLVE_DONE;
		expr->type = decl->type;
		return expr;
	}
	Expr *expr = expr_new(EXPR_IDENTIFIER, decl->span);
	expr->identifier_expr.ident = decl->name;
	expr->resolve_status = RESOLVE_NOT_DONE;
	return expr;
}

void expr_rewrite_to_variable(Expr *expr, Decl *decl)
{
	expr->expr_kind = EXPR_IDENTIFIER;
	if (decl->resolve_status == RESOLVE_DONE)
	{
		expr->identifier_expr.decl = decl;
		expr->resolve_status = RESOLVE_DONE;
		expr->type = decl->type;
		return;
	}
	expr->identifier_expr.ident = decl->name;
	expr->resolve_status = RESOLVE_NOT_DONE;
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
		assert(no_fail->canonical->type_kind == TYPE_POINTER);

		// Only fold to the canonical type if it wasn't a pointer.
		Type *pointee = no_fail->type_kind == TYPE_POINTER ? no_fail->pointer : no_fail->canonical->pointer;
		original->type = type_add_optional(pointee, IS_OPTIONAL(inner));
	}
}

void expr_rewrite_to_string(Expr *expr_to_rewrite, const char *string)
{
	expr_to_rewrite->expr_kind = EXPR_CONST;
	expr_to_rewrite->const_expr.const_kind = CONST_STRING;
	expr_to_rewrite->const_expr.string.chars = (char *)string;
	ArraySize len = (ArraySize)strlen(string);
	expr_to_rewrite->const_expr.string.len = len;
	expr_to_rewrite->resolve_status = RESOLVE_DONE;
	expr_to_rewrite->type = type_get_ptr(type_get_array(type_char, len));
}

void expr_rewrite_to_binary(Expr *expr_to_rewrite, Expr *left, Expr *right, BinaryOp op)
{
	expr_to_rewrite->binary_expr = (ExprBinary) { .operator = op, .left = exprid(left), .right = exprid(right) };
	expr_to_rewrite->expr_kind = EXPR_BINARY;
}
