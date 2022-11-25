// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "sema_internal.h"
#include <math.h>

const char *ct_eval_error = "EVAL_ERROR";

typedef enum
{
	SUBSCRIPT_EVAL_VALUE,
	SUBSCRIPT_EVAL_REF,
	SUBSCRIPT_EVAL_ASSIGN
} SubscriptEval;

typedef struct
{
	bool macro;
	const char *name;
	const char *block_parameter;
	Decl **params;
	Expr *struct_var;
	Signature *signature;
} CalledDecl;

// Properties
static inline BuiltinFunction builtin_by_name(const char *name);
static inline TypeProperty type_property_by_name(const char *name);

static inline bool sema_expr_analyse_subscript(SemaContext *context, Expr *expr, SubscriptEval eval_type);
static inline bool sema_expr_analyse_pointer_offset(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_slice(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_group(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_access(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_compound_literal(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_builtin(SemaContext *context, Expr *expr, bool throw_error);
static inline bool sema_expr_analyse_binary(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_ct_eval(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_identifier(SemaContext *context, Type *to, Expr *expr);
static inline bool sema_expr_analyse_ct_identifier(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_hash_identifier(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_ternary(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_cast(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_or_error(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_unary(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_try(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_catch(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_rethrow(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_force_unwrap(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_typeid(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_call(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_expr_block(SemaContext *context, Type *infer_type, Expr *expr);
static inline bool sema_expr_analyse_failable(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_compiler_const(SemaContext *context, Expr *expr, bool report_missing);
static inline bool sema_expr_analyse_ct_arg(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_ct_stringify(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_ct_offsetof(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_ct_call(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_variant(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_retval(SemaContext *c, Expr *expr);
static inline bool sema_expr_analyse_expr_list(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_ct_checks(SemaContext *context, Expr *expr);

// -- binary
static bool sema_expr_analyse_sub(SemaContext *context, Expr *expr, Expr *left, Expr *right);
static bool sema_expr_analyse_add(SemaContext *context, Expr *expr, Expr *left, Expr *right);
static bool sema_expr_analyse_mult(SemaContext *context, Expr *expr, Expr *left, Expr *right);
static bool sema_expr_analyse_div(SemaContext *context, Expr *expr, Expr *left, Expr *right);
static bool sema_expr_analyse_mod(SemaContext *context, Expr *expr, Expr *left, Expr *right);
static bool sema_expr_analyse_bit(SemaContext *context, Expr *expr, Expr *left, Expr *right);
static bool sema_expr_analyse_shift(SemaContext *context, Expr *expr, Expr *left, Expr *right);
static bool sema_expr_analyse_shift_assign(SemaContext *context, Expr *expr, Expr *left, Expr *right);
static bool sema_expr_analyse_and_or(SemaContext *context, Expr *expr, Expr *left, Expr *right);
static bool sema_expr_analyse_add_sub_assign(SemaContext *context, Expr *expr, Expr *left, Expr *right);
static bool sema_expr_analyse_slice_assign(SemaContext *context, Expr *expr, Type *left_type, Expr *right, bool is_unwrapped);
static bool sema_expr_analyse_ct_identifier_assign(SemaContext *context, Expr *expr, Expr *left, Expr *right);
static bool sema_expr_analyse_ct_type_identifier_assign(SemaContext *context, Expr *expr, Expr *left, Expr *right);
static bool sema_expr_analyse_assign(SemaContext *context, Expr *expr, Expr *left, Expr *right);
static bool sema_expr_analyse_comp(SemaContext *context, Expr *expr, Expr *left, Expr *right);
static bool sema_expr_analyse_op_assign(SemaContext *context, Expr *expr, Expr *left, Expr *right, bool int_only);

// -- unary
static inline bool sema_expr_analyse_addr(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_neg(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_bit_not(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_not(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_deref(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_ct_incdec(SemaContext *context, Expr *expr, Expr *inner);
static inline bool sema_expr_analyse_incdec(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_taddr(SemaContext *context, Expr *expr);

// -- ct_call
static inline bool sema_expr_analyse_ct_alignof(SemaContext *context, Expr *expr);

static inline bool sema_expr_analyse_ct_nameof(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_ct_defined(SemaContext *context, Expr *expr);

// -- returns
static inline void context_pop_returns(SemaContext *context, Ast **restore);
static inline Ast **context_push_returns(SemaContext *context);
static inline Type *context_unify_returns(SemaContext *context);

// -- addr helpers
static bool sema_addr_check_may_take(Expr *inner);
static inline bool sema_addr_may_take_of_var(Expr *expr, Decl *decl);
static inline bool sema_addr_may_take_of_ident(Expr *inner);

// -- subscript helpers
static bool sema_subscript_rewrite_index_const_list(Expr *const_list, Expr *index, Expr *result);
static Type *sema_subscript_find_indexable_type_recursively(Type **type, Expr **parent);
static void sema_subscript_deref_array_pointers(Expr *expr);

// -- binary helper functions
static void expr_binary_unify_failability(Expr *expr, Expr *left, Expr *right);
static inline bool sema_binary_promote_top_down(SemaContext *context, Expr *binary, Expr *left, Expr *right);
static inline bool sema_binary_analyse_subexpr(SemaContext *context, Expr *binary, Expr *left, Expr *right);
static inline bool sema_binary_analyse_arithmetic_subexpr(SemaContext *context, Expr *expr, const char *error, bool bool_is_allowed);
static inline bool sema_binary_analyse_ct_identifier_lvalue(SemaContext *context, Expr *expr);
static bool sema_binary_check_unclear_op_precedence(Expr *left_side, Expr * main_expr, Expr *right_side);
static bool sema_binary_analyse_ct_common_assign(SemaContext *context, Expr *expr, Expr *left);
static bool sema_binary_arithmetic_promotion(SemaContext *context, Expr *left, Expr *right, Type *left_type, Type *right_type, Expr *parent, const char *error_message);
static bool sema_binary_is_unsigned_always_false_comparison(SemaContext *context, Expr *expr, Expr *left, Expr *right);
static bool sema_binary_is_expr_lvalue(Expr *top_expr, Expr *expr);
static void sema_binary_unify_voidptr(Expr *left, Expr *right, Type **left_type_ref, Type **right_type_ref);

// -- function helper functions
static inline bool sema_expr_analyse_var_call(SemaContext *context, Expr *expr, Type *func_ptr_type, bool failable);
static inline bool sema_expr_analyse_func_call(SemaContext *context, Expr *expr, Decl *decl, Expr *struct_var, bool failable);
static inline bool sema_call_analyse_invocation(SemaContext *context, Expr *call, CalledDecl callee, bool *failable);
static inline bool sema_call_analyse_func_invocation(SemaContext *context, Type *type, Expr *expr, Expr *struct_var,
                                                     bool failable, const char *name);
static inline bool sema_call_check_invalid_body_arguments(SemaContext *context, Expr *call, CalledDecl *callee);
INLINE bool sema_call_expand_arguments(SemaContext *context, CalledDecl *callee, Expr *call, Expr **args,
                                       unsigned func_param_count, Variadic variadic, unsigned vararg_index,
                                       bool *failable,
                                       Expr ***varargs_ref,
                                       Expr **vararg_splat_ref);
static inline int sema_call_find_index_of_named_parameter(Decl **func_params, Expr *expr);
static inline bool sema_call_check_inout_param_match(SemaContext *context, Decl *param, Expr *expr);
static bool sema_call_analyse_body_expansion(SemaContext *macro_context, Expr *call);


static bool sema_slice_len_is_in_range(SemaContext *context, Type *type, Expr *len_expr, bool from_end, bool *remove_from_end);
static bool sema_slice_index_is_in_range(SemaContext *context, Type *type, Expr *index_expr, bool end_index, bool from_end, bool *remove_from_end);

static Expr **sema_vasplat_append(SemaContext *c, Expr **init_expressions, Expr *expr);
INLINE Expr **sema_expand_vasplat(SemaContext *c, Expr **list, unsigned index);
static inline IndexDiff range_const_len(Range *range);
static inline bool sema_expr_begin_analyse(Expr *expr);
static inline bool sema_analyse_expr_dispatch(SemaContext *context, Expr *expr);

static Decl *sema_expr_analyse_var_path(SemaContext *context, Expr *expr, ExprFlatElement **elements);
static inline bool sema_expr_analyse_flat_element(SemaContext *context, ExprFlatElement *element, Type *type, Decl **member_ref, ArraySize *index_ref, Type **return_type, unsigned i, SourceSpan loc,
                                                  bool *is_missing);
static Expr *sema_expr_resolve_access_child(SemaContext *context, Expr *child, bool *missing);

static Type *sema_expr_check_type_exists(SemaContext *context, TypeInfo *type_info);
static inline Expr *sema_ct_checks_exprlist_compiles(SemaContext *context, Expr *exprlist);
static inline bool sema_cast_ct_ident_rvalue(SemaContext *context, Expr *expr);
static bool sema_expr_rewrite_to_typeid_property(SemaContext *context, Expr *expr, Expr *typeid, const char *kw);
static bool sema_expr_rewrite_to_type_property(SemaContext *context, Expr *expr, Type *type, TypeProperty property,
                                               AlignSize alignment, AlignSize offset);
static bool sema_expr_rewrite_typeid_call(Expr *expr, Expr *typeid, TypeIdInfoKind kind, Type *result_type);
static inline void sema_expr_rewrite_typeid_kind(Expr *expr, Expr *parent);
static inline void sema_expr_replace_with_enum_array(Expr *enum_array_expr, Decl *enum_decl);
static inline void sema_expr_replace_with_enum_name_array(Expr *enum_array_expr, Decl *enum_decl);
static inline void sema_expr_rewrite_to_type_nameof(Expr *expr, Type *type, TokenType name_type);

static inline bool sema_create_const_kind(Expr *expr, Type *type);
static inline bool sema_create_const_len(SemaContext *context, Expr *expr, Type *type);
static inline bool sema_create_const_inner(SemaContext *context, Expr *expr, Type *type);
static inline bool sema_create_const_min(SemaContext *context, Expr *expr, Type *type, Type *flat);
static inline bool sema_create_const_max(SemaContext *context, Expr *expr, Type *type, Type *flat);
static inline bool sema_create_const_params(SemaContext *context, Expr *expr, Type *type);
static inline void sema_create_const_membersof(SemaContext *context, Expr *expr, Type *type, AlignSize alignment,
                                               AlignSize offset);
void expr_insert_widening_type(Expr *expr, Type *infer_type);
static Expr *expr_access_inline_member(Expr *parent, Decl *parent_decl);
static inline int64_t expr_get_index_max(Expr *expr);
static inline bool expr_both_any_integer_or_integer_vector(Expr *left, Expr *right);
static inline bool expr_both_const(Expr *left, Expr *right);
static inline bool sema_identifier_find_possible_inferred(Type *to, Expr *expr);
static inline bool sema_expr_analyse_enum_constant(Expr *expr, const char *name, Decl *decl);

static inline bool sema_cast_ident_rvalue(SemaContext *context, Expr *expr);
static inline bool sema_cast_rvalue(SemaContext *context, Expr *expr);

static inline bool sema_expr_analyse_type_access(SemaContext *context, Expr *expr, TypeInfo *parent, bool was_group, Expr *identifier);
static inline bool sema_expr_analyse_member_access(SemaContext *context, Expr *expr, Expr *parent, bool was_group, Expr *identifier);
static inline bool sema_expr_fold_to_member(Expr *expr, Expr *parent, Decl *member);

// -- implementations

Expr *sema_expr_analyse_ct_arg_index(SemaContext *context, Expr *index_expr)
{
	unsigned args = vec_size(context->macro_varargs);
	uint64_t index;
	Decl *param = NULL;
	if (!sema_analyse_expr(context, index_expr)) return poisoned_expr;
	if (!type_is_integer(index_expr->type))
	{
		SEMA_ERROR(index_expr, "Expected the argument index here, but found a value of type %s.", type_quoted_error_string(index_expr->type));
		return poisoned_expr;
	}
	if (!expr_is_const(index_expr))
	{
		SEMA_ERROR(index_expr, "Vararg functions need a constant argument, but this is a runtime value.");
		return poisoned_expr;
	}
	Int index_val = index_expr->const_expr.ixx;
	if (int_is_neg(index_val))
	{
		SEMA_ERROR(index_expr, "The index cannot be negative.");
		return poisoned_expr;
	}
	Int int_max = { .i = { .low = args }, .type = TYPE_U32 };
	if (int_comp(index_val, int_max, BINARYOP_GE))
	{
		SEMA_ERROR(index_expr, "Only %u vararg%s exist.", args, args == 1 ? "" : "s");
		return poisoned_expr;
	}
	return context->macro_varargs[(size_t)index_val.i.low];
}

Expr *sema_ct_eval_expr(SemaContext *c, bool is_type_eval, Expr *inner, bool report_missing)
{
	Path *path = NULL;
	if (!sema_analyse_expr(c, inner)) return false;
	if (!expr_is_const_string(inner))
	{
		SEMA_ERROR(inner, "'%s' expects a constant string as the argument.", is_type_eval ? "$evaltype" : "$eval");
		return NULL;
	}
	const char *interned_version = NULL;
	TokenType token = sema_splitpathref(inner->const_expr.string.chars, inner->const_expr.string.len, &path, &interned_version);
	switch (token)
	{
		case TOKEN_CONST_IDENT:
			inner->identifier_expr.is_const = true;
			break;
		case TOKEN_IDENT:
			if (!interned_version)
			{
				if (report_missing)
				{
					SEMA_ERROR(inner, "'%.*s' could not be found, did you spell it right?", (int)inner->const_expr.string.len, inner->const_expr.string.chars);
				}
				return NULL;
			}
			inner->identifier_expr.is_const = false;
			break;
		case TYPE_TOKENS:
		{
			TypeInfo *info = type_info_new_base(type_from_token(token), inner->span);
			inner->expr_kind = EXPR_TYPEINFO;
			inner->resolve_status = RESOLVE_NOT_DONE;
			inner->type_expr = info;
			return inner;
		}
		case TOKEN_TYPE_IDENT:
		{
			TypeInfo *info = type_info_new(TYPE_INFO_IDENTIFIER, inner->span);
			info->unresolved.name = interned_version;
			info->unresolved.path = path;
			info->resolve_status = RESOLVE_NOT_DONE;
			inner->expr_kind = EXPR_TYPEINFO;
			inner->resolve_status = RESOLVE_NOT_DONE;
			inner->type_expr = info;
			return inner;
		}
		default:
			if (is_type_eval)
			{
				SEMA_ERROR(inner, "Only valid types may be resolved with $evaltype.");
			}
			else
			{
				SEMA_ERROR(inner, "Only plain function, variable and constant names may be resolved with $eval.");
			}
			return NULL;
	}
	inner->expr_kind = EXPR_IDENTIFIER;
	inner->resolve_status = RESOLVE_NOT_DONE;
	inner->identifier_expr.ident = interned_version;
	inner->identifier_expr.path = path;
	return inner;
}

static Expr *expr_access_inline_member(Expr *parent, Decl *parent_decl)
{
	Expr *embedded_struct = expr_new(EXPR_ACCESS, parent->span);
	embedded_struct->resolve_status = RESOLVE_DONE;
	embedded_struct->access_expr.parent = parent;
	embedded_struct->access_expr.ref = parent_decl->strukt.members[0];
	embedded_struct->type = embedded_struct->access_expr.ref->type;
	return embedded_struct;
}

static inline bool expr_both_const(Expr *left, Expr *right)
{
	return left->expr_kind == EXPR_CONST && right->expr_kind == EXPR_CONST;
}

static inline bool expr_both_any_integer_or_integer_vector(Expr *left, Expr *right)
{
	Type *flatten_left = type_flatten(left->type);
	Type *flatten_right = type_flatten(right->type);
	if (type_is_integer(flatten_left) && type_is_integer(flatten_right)) return true;

	if (flatten_left->type_kind != TYPE_VECTOR || flatten_right->type_kind != TYPE_VECTOR) return false;

	return type_is_integer(flatten_left->array.base) && type_is_integer(flatten_right->array.base);
}


static void expr_binary_unify_failability(Expr *expr, Expr *left, Expr *right)
{
	expr->type = type_add_optional(left->type, IS_OPTIONAL(right));
}

static inline void context_pop_returns(SemaContext *context, Ast **restore)
{
	if (!context->returns_cache && context->returns)
	{
		context->returns_cache = context->returns;
	}
	context->returns = restore;
}

static inline Ast **context_push_returns(SemaContext *context)
{
	Ast** old_returns = context->returns;
	if (context->returns_cache)
	{
		context->returns = context->returns_cache;
		context->returns_cache = NULL;
		vec_resize(context->returns, 0);
	}
	else
	{
		context->returns = NULL;
	}
	return old_returns;
}

int sema_check_comp_time_bool(SemaContext *context, Expr *expr)
{
	if (!sema_analyse_cond_expr(context, expr)) return -1;
	if (!expr_is_const(expr))
	{
		SEMA_ERROR(expr, "Compile time evaluation requires a compile time constant value.");
		return -1;
	}
	return expr->const_expr.b;
}


static bool sema_binary_is_expr_lvalue(Expr *top_expr, Expr *expr)
{
	switch (expr->expr_kind)
	{
		case EXPR_SUBSCRIPT_ASSIGN:
		case EXPR_CT_IDENT:
			return true;
		case EXPR_IDENTIFIER:
		{
			Decl *decl = expr->identifier_expr.decl;
			if (decl->decl_kind != DECL_VAR)
			{
				SEMA_ERROR(top_expr, "You cannot assign a value to %s.", decl_to_a_name(decl));
				return false;
			}
			if (decl->var.kind == VARDECL_CONST)
			{
				SEMA_ERROR(top_expr, "You cannot assign to a constant.");
				return false;
			}
			decl = decl_raw(decl);
			switch (decl->var.kind)
			{
				case VARDECL_LOCAL_CT:
				case VARDECL_LOCAL_CT_TYPE:
				case VARDECL_LOCAL:
				case VARDECL_GLOBAL:
				case VARDECL_PARAM:
				case VARDECL_PARAM_REF:
				case VARDECL_PARAM_CT:
				case VARDECL_PARAM_CT_TYPE:
					return true;
				case VARDECL_CONST:
				case VARDECL_PARAM_EXPR:
					UNREACHABLE
				case VARDECL_MEMBER:
				case VARDECL_BITMEMBER:
					goto ERR;
				case VARDECL_UNWRAPPED:
				case VARDECL_ERASE:
				case VARDECL_REWRAPPED:
					UNREACHABLE
			}
			UNREACHABLE
		}
		case EXPR_UNARY:
			if (expr->unary_expr.operator != UNARYOP_DEREF) goto ERR;
			if (IS_OPTIONAL(expr))
			{
				SEMA_ERROR(top_expr, "You cannot assign to a dereferenced optional.");
				return false;
			}
			return true;
		case EXPR_BITACCESS:
		case EXPR_ACCESS:
			return sema_binary_is_expr_lvalue(top_expr, expr->access_expr.parent);
		case EXPR_GROUP:
			return sema_binary_is_expr_lvalue(top_expr, expr->inner_expr);
		case EXPR_SUBSCRIPT:
		case EXPR_SLICE:
		case EXPR_SUBSCRIPT_ADDR:
			if (IS_OPTIONAL(expr))
			{
				SEMA_ERROR(top_expr, "You cannot assign to an optional value.");
				return false;
			}
			return true;
		case EXPR_HASH_IDENT:
			SEMA_ERROR(top_expr, "You cannot assign to an unevaluated expression.");
			return false;
		case EXPR_POISONED:
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
		case EXPR_DECL:
		case EXPR_DESIGNATED_INITIALIZER_LIST:
		case EXPR_DESIGNATOR:
		case EXPR_EXPRESSION_LIST:
		case EXPR_EXPR_BLOCK:
		case EXPR_FAILABLE:
		case EXPR_FLATPATH:
		case EXPR_FORCE_UNWRAP:
		case EXPR_INITIALIZER_LIST:
		case EXPR_MACRO_BLOCK:
		case EXPR_MACRO_BODY_EXPANSION:
		case EXPR_NOP:
		case EXPR_OPERATOR_CHARS:
		case EXPR_POINTER_OFFSET:
		case EXPR_POST_UNARY:
		case EXPR_RETHROW:
		case EXPR_RETVAL:
		case EXPR_SLICE_ASSIGN:
		case EXPR_SLICE_COPY:
		case EXPR_STRINGIFY:
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
		case EXPR_TEST_HOOK:
			goto ERR;
	}
	UNREACHABLE
ERR:
	SEMA_ERROR(top_expr, "An assignable expression, like a variable, was expected here.");
	return false;
}

bool sema_expr_check_assign(SemaContext *c, Expr *expr)
{
	if (!sema_binary_is_expr_lvalue(expr, expr)) return false;
	if (expr->expr_kind == EXPR_SUBSCRIPT_ASSIGN) return true;
	if (expr->expr_kind == EXPR_BITACCESS || expr->expr_kind == EXPR_ACCESS) expr = expr->access_expr.parent;
	if (expr->expr_kind == EXPR_IDENTIFIER)
	{
		expr->identifier_expr.decl->var.is_written = true;
	}
	if (expr->expr_kind != EXPR_UNARY) return true;
	Expr *inner = expr->inner_expr;
	if (inner->expr_kind != EXPR_IDENTIFIER) return true;
	Decl *decl = inner->identifier_expr.decl;
	if (decl->decl_kind != DECL_VAR) return true;
	if (!decl->var.in_param) return true;
	SEMA_ERROR(inner, "'in' parameters may not be assigned to.");
	return false;
}

static inline bool sema_cast_ident_rvalue(SemaContext *context, Expr *expr)
{
	Decl *decl = expr->identifier_expr.decl;
	decl = decl_flatten(decl);

	switch (decl->decl_kind)
	{
		case DECL_FUNC:
			SEMA_ERROR(expr, "Expected function followed by (...) or prefixed by &.");
			return expr_poison(expr);
		case DECL_MACRO:
			SEMA_ERROR(expr, "Expected a macro followed by (...).");
			return expr_poison(expr);
		case DECL_GENERIC:
			SEMA_ERROR(expr, "Expected generic function followed by (...).");
			return expr_poison(expr);
		case DECL_FAULTVALUE:
			SEMA_ERROR(expr, "Did you forget a '!' after '%s'?", decl->name);
			return expr_poison(expr);
		case DECL_ENUM_CONSTANT:
			TODO
			//expr_replace(expr, decl->enum_constant.expr);
			return true;
		case DECL_VAR:
			break;
		case DECL_DISTINCT:
		case DECL_TYPEDEF:
		case DECL_DECLARRAY:
		case DECL_BODYPARAM:
		case DECL_INITIALIZE:
		case DECL_FINALIZE:
			UNREACHABLE
		case DECL_POISONED:
			return expr_poison(expr);
		case DECL_LABEL:
			SEMA_ERROR(expr, "Did you intend to use the label '%s' here?", decl->name);
			return expr_poison(expr);
		case DECL_BITSTRUCT:
			SEMA_ERROR(expr, "Expected bitstruct followed by (...) or '.'.");
			return expr_poison(expr);
		case DECL_STRUCT:
			SEMA_ERROR(expr, "Expected struct followed by {...} or '.'.");
			return expr_poison(expr);
		case DECL_UNION:
			SEMA_ERROR(expr, "Expected union followed by {...} or '.'.");
			return expr_poison(expr);
		case DECL_ENUM:
			SEMA_ERROR(expr, "Expected enum name followed by '.' and an enum value.");
			return expr_poison(expr);
		case DECL_FAULT:
			SEMA_ERROR(expr, "Expected fault name followed by '.' and a fault value.");
			return expr_poison(expr);
		case DECL_IMPORT:
		case DECL_CT_IF:
		case DECL_CT_ELSE:
		case DECL_CT_ELIF:
		case DECL_CT_SWITCH:
		case DECL_CT_CASE:
		case DECL_ATTRIBUTE:
		case DECL_CT_ASSERT:
		case DECL_DEFINE:
			UNREACHABLE
	}
	switch (decl->var.kind)
	{
		case VARDECL_CONST:
		case VARDECL_GLOBAL:
		case VARDECL_LOCAL:
		case VARDECL_LOCAL_CT:
		case VARDECL_LOCAL_CT_TYPE:
			if (decl->var.init_expr && decl->var.init_expr->resolve_status != RESOLVE_DONE)
			{
				SEMA_ERROR(expr, "This looks like the initialization of the variable was circular.");
				return false;
			}
			break;
		default:
			break;
	}
	switch (decl->var.kind)
	{
		case VARDECL_CONST:
			if (!expr_is_constant_eval(decl->var.init_expr, CONSTANT_EVAL_NO_SIDE_EFFECTS))
			{
				UNREACHABLE
			}
			if (type_is_abi_aggregate(decl->type)) return true;
			expr_replace(expr, copy_expr_single(decl->var.init_expr));
			return sema_analyse_expr(context, expr);
		case VARDECL_PARAM_EXPR:
			UNREACHABLE
		case VARDECL_PARAM_CT_TYPE:
		case VARDECL_PARAM_CT:
		case VARDECL_LOCAL_CT:
		case VARDECL_LOCAL_CT_TYPE:
		case VARDECL_ERASE:
		case VARDECL_REWRAPPED:
			// Impossible to reach this, they are already unfolded
			UNREACHABLE
		case VARDECL_PARAM_REF:
			expr_replace(expr, copy_expr_single(decl->var.init_expr));
			return sema_cast_rvalue(context, expr);
		case VARDECL_PARAM:
		case VARDECL_GLOBAL:
		case VARDECL_LOCAL:
		case VARDECL_UNWRAPPED:
			return true;
		case VARDECL_BITMEMBER:
		case VARDECL_MEMBER:
			SEMA_ERROR(expr, "Expected '%s' followed by a method call or property.", decl->name);
			return expr_poison(expr);
	}
	UNREACHABLE
}

static inline bool sema_expr_analyse_ternary(SemaContext *context, Expr *expr)
{
	Expr *left = exprptrzero(expr->ternary_expr.then_expr);
	Expr *cond = exprptr(expr->ternary_expr.cond);
	int path = -1;
	// Normal
	if (left)
	{
		if (!sema_analyse_cond_expr(context, cond)) return expr_poison(expr);
		if (!sema_analyse_expr(context, left)) return expr_poison(expr);
		if (cond->expr_kind == EXPR_CONST)
		{
			path = cond->const_expr.b ? 1 : 0;
		}
	}
	else
	{
		// Elvis
		if (!sema_analyse_expr(context, cond)) return expr_poison(expr);
		Type *type = cond->type->canonical;
		if (type->type_kind != TYPE_BOOL && cast_to_bool_kind(type) == CAST_ERROR)
		{
			SEMA_ERROR(cond, "Cannot convert expression to boolean.");
			return false;
		}
		if (expr_is_constant_eval(cond, true))
		{
			Expr *copy = copy_expr_single(cond);
			cast(copy, type_bool);
			assert(cond->expr_kind == EXPR_CONST);
			path = cond->const_expr.b ? 1 : 0;
		}
		left = cond;
	}

	Expr *right = exprptr(expr->ternary_expr.else_expr);
	if (!sema_analyse_expr(context, right)) return expr_poison(expr);

	bool is_failable = false;
	Type *left_canonical = left->type->canonical;
	Type *right_canonical = right->type->canonical;
	if (left_canonical != right_canonical)
	{
		Type *max;
		if (left_canonical->type_kind == TYPE_FAILABLE_ANY)
		{
			max = right_canonical;
		}
		else if (right_canonical->type_kind == TYPE_FAILABLE_ANY)
		{
			max = left_canonical;
		}
		else
		{
			max = type_find_max_type(type_no_optional(left_canonical), type_no_optional(right_canonical));
		}
		if (!max)
		{
			SEMA_ERROR(expr, "Cannot find a common parent type of '%s' and '%s'",
			           type_to_error_string(left->type), type_to_error_string(right->type));
			return false;
		}
		Type *no_fail_max = type_no_optional(max);
		if (!cast_implicit(context, left, no_fail_max) || !cast_implicit(context, right, no_fail_max)) return false;
	}

	if (path > -1)
	{
		expr_replace(expr, path ? left : right);
	}

	expr_binary_unify_failability(expr, left, right);
	return true;
}

static inline bool sema_expr_analyse_enum_constant(Expr *expr, const char *name, Decl *decl)
{
	Decl *enum_constant = decl_find_enum_constant(decl, name);
	if (!enum_constant) return false;

	assert(enum_constant->resolve_status == RESOLVE_DONE);
	expr->type = decl->type;
	expr->expr_kind = EXPR_CONST;
	expr->const_expr.const_kind = enum_constant->decl_kind == DECL_ENUM_CONSTANT ? CONST_ENUM : CONST_ERR;
	expr->const_expr.enum_err_val = enum_constant;
	return true;
}


static inline bool sema_identifier_find_possible_inferred(Type *to, Expr *expr)
{
	if (to->canonical->type_kind != TYPE_ENUM && to->canonical->type_kind != TYPE_FAULTTYPE) return false;
	Decl *parent_decl = to->canonical->decl;
	switch (parent_decl->decl_kind)
	{
		case DECL_ENUM:
		case DECL_FAULT:
			return sema_expr_analyse_enum_constant(expr, expr->identifier_expr.ident, parent_decl);
		case DECL_UNION:
		case DECL_STRUCT:
		case DECL_BITSTRUCT:
			return false;
		default:
			UNREACHABLE
	}

}

static inline bool sema_expr_analyse_identifier(SemaContext *context, Type *to, Expr *expr)
{
	Decl *ambiguous_decl = NULL;
	Decl *private_symbol = NULL;

	assert(expr && expr->identifier_expr.ident);
	DEBUG_LOG("Resolving identifier '%s'", expr->identifier_expr.ident);

	assert(expr->resolve_status != RESOLVE_DONE);
	DeclId body_param;
	if (!expr->identifier_expr.path && context->current_macro && (body_param = context->current_macro->func_decl.body_param))
	{
		if (expr->identifier_expr.ident == declptr(body_param)->name)
		{
			expr->expr_kind = EXPR_MACRO_BODY_EXPANSION;
			expr->body_expansion_expr.first_stmt = 0;
			expr->body_expansion_expr.declarations = NULL;
			expr->resolve_status = RESOLVE_NOT_DONE;
			expr->type = type_void;
			return true;
		}
	}
	// Just start with inference
	if (!expr->identifier_expr.path && to)
	{
		if (sema_identifier_find_possible_inferred(to, expr)) return true;
	}

	Decl *decl = sema_find_path_symbol(context, expr->identifier_expr.ident, expr->identifier_expr.path);
	// Is this a broken decl?
	if (!decl_ok(decl)) return false;


	// Rerun if we can't do inference.
	if (!decl)
	{
		decl = sema_resolve_symbol(context, expr->identifier_expr.ident, expr->identifier_expr.path, expr->span);
		(void)decl;
		assert(!decl_ok(decl));
		return false;
	}

	if (decl->decl_kind == DECL_VAR || decl->decl_kind == DECL_FUNC || decl->decl_kind == DECL_MACRO || decl->decl_kind == DECL_GENERIC)
	{
		if (decl->unit->module != context->unit->module && !decl->is_autoimport && !expr->identifier_expr.path)
		{
			const char *message;
			switch (decl->decl_kind)
			{
				case DECL_VAR:
					message = "Globals from other modules must be prefixed with the module name.";
					break;
				case DECL_FUNC:
					message = "Functions from other modules must be prefixed with the module name.";
					break;
				case DECL_MACRO:
					message = "Macros from other modules must be prefixed with the module name.";
					break;
				case DECL_GENERIC:
					message = "Generic functions from other modules must be prefixed with the module name.";
					break;
				default:
					UNREACHABLE
			}
			SEMA_ERROR(expr, message);
			return false;
		}
	}
	if (decl->resolve_status != RESOLVE_DONE)
	{
		if (!sema_analyse_decl(context, decl)) return decl_poison(decl);
	}
	unit_register_external_symbol(context->compilation_unit, decl);
	if (decl->decl_kind == DECL_VAR)
	{
		decl->var.is_read = true;
		switch (decl->var.kind)
		{
			case VARDECL_CONST:
				if (!decl->type)
				{
					Expr *copy = copy_expr_single(decl->var.init_expr);
					if (!sema_analyse_expr(context, copy)) return false;
					if (!expr_is_constant_eval(copy, false))
					{
						SEMA_ERROR(expr, "Constant value did not evaluate to a constant.");
						return false;
					}
					expr_replace(expr, copy);
					return true;
				}
				if (IS_OPTIONAL(decl))
				{
					SEMA_ERROR(expr, "Constants may never be 'failable', please remove the '!'.");
					return false;
				}
				break;
			case VARDECL_GLOBAL:
				if (context->call_env.pure)
				{
					SEMA_ERROR(expr, "'@pure' functions may not access globals.");
					return false;
				}
			default:
				break;
		}
	}
	if (!decl->type) decl->type = type_void;
	expr->identifier_expr.decl = decl;
	expr->type = decl->type;
	return true;
}


static inline bool sema_expr_analyse_ct_identifier(SemaContext *context, Expr *expr)
{
	assert(expr && expr->ct_ident_expr.identifier);

	DEBUG_LOG("Resolving identifier '%s'", expr->ct_ident_expr.identifier);
	Decl *decl = sema_resolve_symbol(context, expr->ct_ident_expr.identifier, NULL, expr->span);

	// Already handled
	if (!decl_ok(decl))
	{
		return expr_poison(expr);
	}

	assert(decl->decl_kind == DECL_VAR);
	assert(decl->resolve_status == RESOLVE_DONE);

	expr->ct_ident_expr.decl = decl;
	expr->type = decl->type;
	return true;
}

static inline bool sema_expr_analyse_hash_identifier(SemaContext *context, Expr *expr)
{
	assert(expr && expr->hash_ident_expr.identifier);
	DEBUG_LOG("Resolving identifier '%s'", expr->hash_ident_expr.identifier);
	Decl *decl = sema_resolve_symbol(context, expr->hash_ident_expr.identifier, NULL, expr->span);

	// Already handled
	if (!decl_ok(decl))
	{
		return expr_poison(expr);
	}

	assert(decl->decl_kind == DECL_VAR);

	expr_replace(expr, copy_expr_single(decl->var.init_expr));
	REMINDER("Remove analysis for hash");
	if (!sema_analyse_expr_lvalue_fold_const(decl->var.hash_var.context, expr))
	{
		// Poison the decl so we don't evaluate twice.
		decl_poison(decl);
		return false;
	}
	return true;
}


static inline bool sema_binary_promote_top_down(SemaContext *context, Expr *binary, Expr *left, Expr *right)
{
	if (!binary->binary_expr.widen) return true;
	Type *to = binary->type;
	return cast_widen_top_down(context, left, to) && cast_widen_top_down(context, right, to);
}

static inline bool sema_binary_analyse_subexpr(SemaContext *context, Expr *binary, Expr *left, Expr *right)
{
	return (int)sema_analyse_expr(context, left) & (int)sema_analyse_expr(context, right);
}

static inline bool sema_binary_analyse_arithmetic_subexpr(SemaContext *context, Expr *expr, const char *error, bool bool_is_allowed)
{
	Expr *left = exprptr(expr->binary_expr.left);
	Expr *right = exprptr(expr->binary_expr.right);

	// 1. Analyse both sides.
	if (!sema_binary_analyse_subexpr(context, expr, left, right)) return false;

	if (!sema_binary_promote_top_down(context, expr, left, right)) return false;

	Type *left_type = type_no_optional(left->type)->canonical;
	Type *right_type = type_no_optional(right->type)->canonical;

	if (bool_is_allowed && left_type == type_bool && right_type == type_bool) return true;
	// 2. Perform promotion to a common type.
	return sema_binary_arithmetic_promotion(context, left, right, left_type, right_type, expr, error);
}


static inline int sema_call_find_index_of_named_parameter(Decl **func_params, Expr *expr)
{
	if (vec_size(expr->designator_expr.path) != 1)
	{
		SEMA_ERROR(expr, "Expected the name of a function parameter here, this looks like a member path.");
		return -1;
	}
	DesignatorElement *element = expr->designator_expr.path[0];
	if (element->kind != DESIGNATOR_FIELD)
	{
		SEMA_ERROR(expr, "Expected the name of a function parameter here, this looks like an array path field.");
		return -1;
	}
	const char *name = element->field;
	VECEACH(func_params, i)
	{
		if (func_params[i]->name == name) return (int)i;
	}
	SEMA_ERROR(expr, "There's no parameter with the name '%s'.", name);
	return -1;
}





static inline bool sema_call_check_invalid_body_arguments(SemaContext *context, Expr *call, CalledDecl *callee)
{
	Decl **body_arguments = call->call_expr.body_arguments;
	bool has_body_arguments = vec_size(body_arguments) > 0;

	// 1. Check if there are body arguments but no actual block.
	if (has_body_arguments && !callee->block_parameter)
	{
		if (callee->macro)
		{
			SEMA_ERROR(body_arguments[0], "This macro does not support arguments.");
		}
		else
		{
			SEMA_ERROR(body_arguments[0], "Only macro calls may have body arguments for a trailing block.");
		}
		return false;
	}

	// 2. If there is a body then...
	if (call->call_expr.body)
	{
		// 2a. If not a macro then this is an error.
		if (!callee->macro)
		{
			SEMA_ERROR(call, "Only macro calls may take a trailing block.");
			return false;
		}
		// 2b. If we don't have a block parameter, then this is an error as well
		if (!callee->block_parameter)
		{
			SEMA_ERROR(call, "This macro does not support trailing statements, please remove it.");
			return false;
		}

		// 2c. This is a macro and it has a block parameter. Everything is fine!
		return true;
	}

	// 3. If we don't have a body, then if it has a block parameter this is an error.
	if (callee->block_parameter)
	{
		assert(callee->macro);
		SEMA_ERROR(call, "Expected call to have a trailing statement, did you forget to add it?");
		return false;
	}

	// 4. No body and no block parameter, this is fine.
	return true;
}


INLINE bool sema_call_expand_arguments(SemaContext *context, CalledDecl *callee, Expr *call, Expr **args,
                                       unsigned func_param_count, Variadic variadic, unsigned vararg_index,
                                       bool *failable,
                                       Expr ***varargs_ref,
                                       Expr **vararg_splat_ref)
{
	unsigned num_args = vec_size(args);
	Decl **params = callee->params;

	Expr **actual_args = VECNEW(Expr*, func_param_count);
	for (unsigned i = 0; i < func_param_count; i++) vec_add(actual_args, NULL);

	// 2. Loop through the parameters.
	bool has_named = false;
	for (unsigned i = 0; i < num_args; i++)
	{
		Expr *arg = args[i];

		// 3. Handle named parameters
		if (arg->expr_kind == EXPR_DESIGNATOR)
		{
			// Find the location of the parameter.
			int index = sema_call_find_index_of_named_parameter(params, arg);

			// If it's not found then this is an error.
			if (index < 0) return false;

			// We have named parameters, that will add some restrictions.
			has_named = true;

			// 8d. We might actually be finding the typed vararg at the end,
			//     this is an error.
			if (params[index]->var.vararg)
			{
				SEMA_ERROR(arg, "Vararg parameters may not be named parameters, use normal parameters instead.", params[index]->name);
				return false;
			}

			// 8e. We might have already set this parameter, that is not allowed.
			if (actual_args[index])
			{
				SEMA_ERROR(arg, "The parameter '%s' was already set.", params[index]->name);
				return false;
			}

			// 8g. Set the parameter
			actual_args[index] = arg->designator_expr.value;
			continue;
		}

		if (has_named)
		{
			SEMA_ERROR(args[i - 1], "Named arguments must be placed after positional arguments.");
			return false;
		}

		// 11. We might have a typed variadic argument.
		if (variadic == VARIADIC_NONE && i >= func_param_count)
		{
			// 15. We have too many parameters...
			SEMA_ERROR(arg, "This argument would exceed the number of parameters, did you add too many arguments?");
			return false;
		}

		// 10. If we exceed the function parameter count (remember we reduced this by one
		//     in the case of typed vararg) we're now in a variadic list.
		if (variadic != VARIADIC_NONE && i >= vararg_index)
		{

			// 11a. Look if we did a splat
			if (call->call_expr.splat_vararg)
			{
				// 11b. Is this the last argument, or did we get some before the splat?
				if (i < num_args - 1)
				{
					SEMA_ERROR(arg,
					           "This looks like a variable argument before an splatted variable which isn't allowed. Did you add too many arguments?");
					return false;
				}
				*vararg_splat_ref = arg;
				continue;
			}
			else if (variadic == VARIADIC_ANY)
			{
				if (!sema_analyse_expr(context, arg)) return false;
				if (type_is_invalid_storage_type(arg->type))
				{
					SEMA_ERROR(arg, "A value of type %s cannot be passed as a variadic argument.",
					           type_quoted_error_string(arg->type));
					return false;
				}
				expr_insert_addr(arg);
			}
			vec_add(*varargs_ref, arg);
			continue;
		}
		actual_args[i] = arg;
	}

	// 17. Set default values.
	for (unsigned i = 0; i < func_param_count; i++)
	{
		// 17a. Assigned a value - skip
		if (actual_args[i]) continue;
		if (i == vararg_index && variadic != VARIADIC_NONE) continue;

		// 17b. Set the init expression.
		Decl *param = params[i];
		Expr *init_expr = param->var.init_expr;
		if (init_expr)
		{
			Expr *arg = actual_args[i] = copy_expr_single(init_expr);
			if (arg->resolve_status != RESOLVE_DONE)
			{

				SemaContext default_context;
				Type *rtype = NULL;
				SemaContext *new_context = context_transform_for_eval(context, &default_context, param->unit);
				bool success;
				SCOPE_START
					new_context->original_inline_line = context->original_inline_line ? context->original_inline_line : init_expr->span.row;
					success = sema_analyse_expr_rhs(new_context, param->type, arg, true);
				SCOPE_END;
				sema_context_destroy(&default_context);
				if (!success) return false;
			}
			continue;
		}

		// 17c. Vararg not set? That's fine.
		if (param->var.vararg) continue;

		// 17d. Argument missing, that's bad.
		if (!has_named || !param->name)
		{
			if (func_param_count == 1)
			{
				if (param->type)
				{
					SEMA_ERROR(call, "This call expected a parameter of type %s, did you forget it?", type_quoted_error_string(param->type));
				}
				else
				{
					SEMA_ERROR(call, "This call expected a parameter, did you forget it?");
				}
				return false;
			}
			if (variadic != VARIADIC_NONE && i > vararg_index)
			{
				if (!param)
				{
					sema_error_at_after(args[num_args - 1]->span, "Argument #%d is not set.", i);
					return false;
				}
				sema_error_at_after(args[num_args - 1]->span, "Expected '.%s = ...' after this argument.", param->name);
				return false;
			}
			if (num_args)
			{
				unsigned needed = func_param_count - num_args;
				SEMA_ERROR(args[num_args - 1],
						   "Expected %d more %s after this one, did you forget %s?",
						   needed, needed == 1 ? "argument" : "arguments", needed == 1 ? "it" : "them");
			}
			else
			{
				SEMA_ERROR(call, "'%s' expects %d parameters, but none was provided.", callee->name, func_param_count);
			}
			return false;
		}
		SEMA_ERROR(call, "The parameter '%s' must be set, did you forget it?", param->name);
		return false;
	}
	call->call_expr.arguments = actual_args;
	return true;
}

static inline bool sema_call_check_inout_param_match(SemaContext *context, Decl *param, Expr *expr)
{
	if (expr->expr_kind != EXPR_IDENTIFIER) return true;
	Decl *ident = expr->identifier_expr.decl;
	if (ident->decl_kind != DECL_VAR) return true;
	if (ident->var.out_param && param->var.in_param)
	{
		SEMA_ERROR(expr, "It's not allowed to pass an 'out' parameter into a function or macro as an 'in' argument.");
		return false;
	}
	if (ident->var.in_param && param->var.out_param)
	{
		SEMA_ERROR(expr, "It's not allowed to pass an 'in' parameter into a function or macro as an 'out' argument.");
		return false;
	}
	return true;
}
static inline bool sema_call_analyse_invocation(SemaContext *context, Expr *call, CalledDecl callee, bool *failable)
{
	// 1. Check body arguments (for macro calls, or possibly broken )
	if (!sema_call_check_invalid_body_arguments(context, call, &callee)) return false;

	// 2. Pick out all the arguments and parameters.
	Expr **args = call->call_expr.arguments;
	Signature *sig = callee.signature;
	unsigned vararg_index = sig->vararg_index;
	Variadic variadic = sig->variadic;
	Decl **decl_params = callee.params;

	if (args) args = call->call_expr.arguments = sema_expand_vasplat_exprs(context, args);

	unsigned num_args = vec_size(args);

	// 3. If this is a type call, then we have an implicit first argument.
	if (callee.struct_var)
	{
		// 3a. Insert an argument first, by adding null to the end and then moving all arguments
		//     by one step.
		vec_add(args, NULL);
		for (unsigned i = num_args; i > 0; i--)
		{
			args[i] = args[i - 1];
		}
		// 3b. Then insert the argument.
		args[0] = callee.struct_var;
		num_args++;
		call->call_expr.arguments = args;
		call->call_expr.is_type_method = true;
		assert(!call->call_expr.is_pointer_call);
	}

	// 4. Check for splat of the variadic argument.
	bool splat = call->call_expr.splat_vararg;
	if (splat)
	{
		// 4a. Is this *not* a variadic function/macro? - Then that's an error.
		if (variadic == VARIADIC_NONE)
		{
			assert(call->call_expr.arguments);
			SEMA_ERROR(call->call_expr.arguments[num_args - 1],
			           "Using the splat operator is only allowed on vararg parameters.");
			return false;
		}
	}

	// 5. Zero out all argument slots.
	unsigned param_count = vec_size(decl_params);

	// 6. We might have a typed variadic call e.g. foo(int, double...)
	//    get that type.
	Type *variadic_type = NULL;
	if (variadic == VARIADIC_TYPED || variadic == VARIADIC_ANY)
	{
		// 7a. The parameter type is <type>[], so we get the <type>
		Type *vararg_slot_type = decl_params[vararg_index]->type;
		assert(vararg_slot_type->type_kind == TYPE_SUBARRAY);
		variadic_type = vararg_slot_type->array.base;
	}

	Expr **varargs = NULL;
	Expr *vararg_splat = NULL;
	if (!sema_call_expand_arguments(context,
	                                &callee,
	                                call,
	                                args,
	                                param_count,
	                                variadic,
	                                vararg_index,
	                                failable, &varargs, &vararg_splat)) return false;

	args = call->call_expr.arguments;
	num_args = vec_size(args);

	call->call_expr.varargs = NULL;
	if (varargs)
	{
		if (variadic == VARIADIC_RAW)
		{
			foreach(Expr*, varargs)
			{
				// 12a. Analyse the expression.
				if (callee.macro)
				{
					// Just keep as lvalues
					if (!sema_analyse_expr_lvalue_fold_const(context, val)) return false;
				}
				else
				{
					if (!sema_analyse_expr(context, val)) return false;
					if (!cast_promote_vararg(val)) return false;
				}

				// Set the argument at the location.
				*failable |= IS_OPTIONAL(val);
			}
		}
		else
		{
			foreach(Expr*, varargs)
			{
				// 11e. A simple variadic value:
				if (!sema_analyse_expr_rhs(context, variadic_type, val, true)) return false;
				*failable |= IS_OPTIONAL(val);
			}
		}
		call->call_expr.varargs = varargs;
	}
	if (vararg_splat)
	{
		// 11c. Analyse the expression. We don't use any type inference here since
		//      foo(...{ 1, 2, 3 }) is a fairly worthless thing.
		if (!sema_analyse_expr(context, vararg_splat)) return false;

		// 11d. If it is allowed.
		if (!expr_may_splat_as_vararg(vararg_splat, variadic_type))
		{
			SEMA_ERROR(vararg_splat, "It's not possible to splat %s as vararg of type %s",
			           type_quoted_error_string(vararg_splat->type),
			           type_quoted_error_string(variadic_type));
			return false;
		}
		*failable |= IS_OPTIONAL(vararg_splat);
		call->call_expr.splat = vararg_splat;
	}
	// 7. Loop through the parameters.
	for (unsigned i = 0; i < num_args; i++)
	{
		Expr *arg = args[i];

		// 10. If we exceed the function parameter count (remember we reduced this by one
		//     in the case of typed vararg) we're now in a variadic list.
		if (i == vararg_index && variadic != VARIADIC_NONE)
		{
			assert(arg == NULL);
			continue;
		}

		Decl *param = decl_params[i];
		VarDeclKind kind = param->var.kind;
		Type *type = param->type;

		// 16. Analyse a regular argument.
		switch (kind)
		{
			case VARDECL_PARAM_REF:
				// &foo
				if (!sema_analyse_expr_lvalue(context, arg)) return false;
				if (!sema_expr_check_assign(context, arg)) return false;
				*failable |= IS_OPTIONAL(arg);
				if (!sema_call_check_inout_param_match(context, param, arg)) return false;
				if (type_is_invalid_storage_type(type))
				{
					SEMA_ERROR(arg, "A value of type %s cannot be passed by reference.", type_quoted_error_string(type));
					return false;
				}
				if (type && type->canonical != arg->type->canonical)
				{
					SEMA_ERROR(arg, "'%s' cannot be implicitly cast to '%s'.", type_to_error_string(arg->type), type_to_error_string(type));
					return false;
				}
				if (param && !param->alignment)
				{
					if (arg->expr_kind == EXPR_IDENTIFIER)
					{
						param->alignment = arg->identifier_expr.decl->alignment;
					}
					else
					{
						param->alignment = type_alloca_alignment(arg->type);
					}
				}
				break;
			case VARDECL_PARAM:
				// foo
				if (!sema_analyse_expr_rhs(context, type, arg, true)) return false;
				if (IS_OPTIONAL(arg)) *failable = true;
				if (type_is_invalid_storage_type(arg->type))
				{
					SEMA_ERROR(arg, "A value of type %s can only be passed as a compile time parameter.", type_quoted_error_string(arg->type));
					return false;
				}
				if (!param->alignment)
				{
					assert(callee.macro && "Only in the macro case should we need to insert the alignment.");
					param->alignment = type_alloca_alignment(arg->type);
				}
				if (!sema_call_check_inout_param_match(context, param, arg)) return false;
				break;
			case VARDECL_PARAM_EXPR:
				// #foo
				param->var.hash_var.context = context;
				param->var.hash_var.span = arg->span;
				break;
			case VARDECL_PARAM_CT:
				// $foo
				assert(callee.macro);
				if (!sema_analyse_expr_rhs(context, type, arg, true)) return false;
				if (!expr_is_constant_eval(arg, CONSTANT_EVAL_CONSTANT_VALUE))
				{
					SEMA_ERROR(arg, "A compile time parameter must always be a constant, did you mistake it for a normal paramter?");
					return false;
				}
				break;
			case VARDECL_PARAM_CT_TYPE:
				// $Foo
				if (!sema_analyse_expr_lvalue_fold_const(context, arg)) return false;
				if (arg->expr_kind != EXPR_TYPEINFO)
				{
					SEMA_ERROR(arg, "A type, like 'int' or 'double' was expected for the parameter '%s'.", param->name);
					return false;
				}
				break;
			case VARDECL_CONST:
			case VARDECL_GLOBAL:
			case VARDECL_LOCAL:
			case VARDECL_MEMBER:
			case VARDECL_BITMEMBER:
			case VARDECL_LOCAL_CT:
			case VARDECL_LOCAL_CT_TYPE:
			case VARDECL_UNWRAPPED:
			case VARDECL_REWRAPPED:
			case VARDECL_ERASE:
				UNREACHABLE
		}
		if (param && type_len_is_inferred(type))
		{
			param->type = type_no_optional(arg->type);
		}
	}
	return true;
}
static inline bool sema_call_analyse_func_invocation(SemaContext *context, Type *type, Expr *expr, Expr *struct_var,
                                                     bool failable, const char *name)
{
	Signature *sig = type->function.signature;
	CalledDecl callee = {
			.macro = false,
			.name = name,
			.block_parameter = NULL,
			.struct_var = struct_var,
			.params = sig->params,
			.signature = sig,
	};
	if (context->call_env.pure && !sig->attrs.is_pure && !expr->call_expr.attr_pure)
	{
		SEMA_ERROR(expr, "Only '@pure' functions may be called, you can override this with an attribute.");
		return false;
	}

	bool is_unused = expr->call_expr.result_unused;
	if (!sema_call_analyse_invocation(context, expr, callee, &failable)) return false;

	Type *rtype = type->function.prototype->rtype;

	if (is_unused && rtype != type_void)
	{
		if (sig->attrs.nodiscard)
		{
			SEMA_ERROR(expr, "The result of the function must be used.");
			return false;
		}
		if (type_is_optional(rtype) && !sig->attrs.maydiscard)
		{
			SEMA_ERROR(expr, "The optional result of the function must be used.");
			return false;
		}
	}

	expr->type = type_add_optional(rtype, failable);

	return true;
}

static inline bool sema_expr_analyse_var_call(SemaContext *context, Expr *expr, Type *func_ptr_type, bool failable)
{
	Decl *decl = NULL;
	if (func_ptr_type->type_kind != TYPE_POINTER || func_ptr_type->pointer->type_kind != TYPE_FUNC)
	{
		SEMA_ERROR(expr, "Only macros, functions and function pointers maybe invoked, this is of type '%s'.", type_to_error_string(func_ptr_type));
		return false;
	}
	Type *pointee = func_ptr_type->pointer;
	expr->call_expr.is_pointer_call = true;
	return sema_call_analyse_func_invocation(context, pointee, expr, NULL, failable, func_ptr_type->pointer->name);
}

// Unify returns in a macro or expression block.
static inline Type *context_unify_returns(SemaContext *context)
{
	bool all_returns_need_casts = false;
	Type *common_type = NULL;

	bool failable = false;
	bool no_return = true;
	// 1. Loop through the returns.
	VECEACH(context->returns, i)
	{
		Ast *return_stmt = context->returns[i];
		if (!return_stmt)
		{
			failable = true;
			continue;
		}
		no_return = false;
		Expr *ret_expr = return_stmt->return_stmt.expr;
		Type *rtype = ret_expr ? ret_expr->type : type_void;
		if (type_is_optional_any(rtype))
		{
			failable = true;
			continue;
		}
		if (type_is_optional(rtype))
		{
			failable = true;
			rtype = type_no_optional(rtype);
		}
		// 2. If we have no common type, set to the return type.
		if (!common_type)
		{
			common_type = rtype;
			continue;
		}

		// 3. Same type -> we're done.
		if (common_type == rtype) continue;

		// 4. Find the max of the old and new.
		Type *max = type_find_max_type(common_type, rtype);

		// 5. No match -> error.
		if (!max)
		{
			SEMA_ERROR(return_stmt, "Cannot find a common parent type of %s and %s",
			           rtype, common_type);
			Ast *prev = context->returns[i - 1];
			assert(prev);
			SEMA_NOTE(prev, "The previous return was here.");
			return NULL;
		}

		// 6. Set the new max, mark as needing a cast on all returns.
		common_type = max;
		all_returns_need_casts = true;
	}

	// If we have no return (or only anyfail)
	if (!common_type)
	{
		assert(!all_returns_need_casts && "We should never need casts here.");
		// A failable?
		if (failable)
		{
			// If there are only implicit returns, then we assume void!, otherwise it's an "anyfail"
			return no_return ? type_get_optional(type_void) : type_anyfail;
		}
		// No failable => void.
		return type_void;
	}

	// 7. Insert casts.
	if (all_returns_need_casts)
	{
		assert(!type_is_optional_type(common_type));
		VECEACH(context->returns, i)
		{
			Ast *return_stmt = context->returns[i];
			if (!return_stmt) continue;
			Expr *ret_expr = return_stmt->return_stmt.expr;
			// 8. All casts should work.
			if (!cast_implicit(context, ret_expr, common_type))
			{
				return NULL;
			}
		}
	}

	return type_add_optional(common_type, failable);
}

static inline bool sema_expr_analyse_func_call(SemaContext *context, Expr *expr, Decl *decl, Expr *struct_var, bool failable)
{
	expr->call_expr.is_pointer_call = false;
	if (decl->func_decl.attr_test)
	{
		SEMA_ERROR(expr, "@test functions may not be directly called.");
		return false;
	}
	return sema_call_analyse_func_invocation(context,
	                                         decl->type,
	                                         expr,
	                                         struct_var,
	                                         failable,
	                                         decl->name);
}




bool sema_expr_analyse_macro_call(SemaContext *context, Expr *call_expr, Expr *struct_var, Decl *decl, bool failable)
{
	assert(decl->decl_kind == DECL_MACRO);

	copy_begin();
	Decl **params = copy_decl_list_macro(decl->func_decl.signature.params);
	Ast *body = copy_ast_macro(astptr(decl->func_decl.body));
	AstId docs = decl->func_decl.docs;
	if (docs) docs = astid(copy_ast_macro(astptr(docs)));
	copy_end();
	CalledDecl callee = {
			.macro = true,
			.name = decl->name,
			.params = params,
			.block_parameter = decl->func_decl.body_param ? declptr(decl->func_decl.body_param)->name : NULL,
			.signature = &decl->func_decl.signature,
			.struct_var = struct_var
	};

	if (!sema_call_analyse_invocation(context, call_expr, callee, &failable)) return false;

	unsigned vararg_index = decl->func_decl.signature.vararg_index;
	Expr **args = call_expr->call_expr.arguments;
	VECEACH(params, i)
	{
		Decl *param = params[i];
		if (i == vararg_index)
		{
			if (!param) continue;
			// Splat? That's the simple case.
			if (call_expr->call_expr.splat_vararg)
			{

				if (!sema_analyse_expr(context, args[i] = call_expr->call_expr.splat)) return false;
			}
			else
			{
				Expr **exprs = call_expr->call_expr.varargs;
				Expr *literal = expr_new(EXPR_COMPOUND_LITERAL, exprs ? exprs[0]->span : param->span);
				Expr *initializer_list = expr_new_expr(EXPR_INITIALIZER_LIST, literal);
				initializer_list->initializer_list = exprs;
				literal->expr_compound_literal.type_info = param->var.type_info;
				literal->expr_compound_literal.initializer = initializer_list;
				if (!sema_analyse_expr(context, args[i] = literal)) return false;
			}
		}
		param->var.init_expr = args[i];
		VarDeclKind kind = param->var.kind;
		if (kind == VARDECL_PARAM_CT_TYPE || kind == VARDECL_PARAM_CT)
		{
			param->var.scope_depth = context->active_scope.depth + 1;
		}
	}

	Decl **body_params = call_expr->call_expr.body_arguments;
	unsigned body_params_count = vec_size(body_params);
	Decl **macro_body_params = decl->func_decl.body_param ? declptr(decl->func_decl.body_param)->body_params : NULL;
	unsigned expected_body_params = vec_size(macro_body_params);
	if (expected_body_params > body_params_count)
	{
		SEMA_ERROR(call_expr, "Not enough parameters for the macro body, expected %d.", expected_body_params);
		return false;
	}
	if (expected_body_params < body_params_count)
	{
		SEMA_ERROR(call_expr, "Too many parameters for the macro body, expected %d.", expected_body_params);
		return false;
	}
	for (unsigned i = 0; i < expected_body_params; i++)
	{
		Decl *body_param = macro_body_params[i];
		assert(body_param->resolve_status == RESOLVE_DONE);
		Decl *body_arg = call_expr->call_expr.body_arguments[i];
		if (!body_arg->var.type_info)
		{
			SEMA_ERROR(body_arg, "Expected a type parameter before this variable name.");
			return false;
		}
		if (!sema_resolve_type_info(context, body_arg->var.type_info)) return false;
		body_arg->type = body_arg->var.type_info->type;
		if (body_param->var.type_info)
		{
			Type *declare_type = body_param->var.type_info->type->canonical;
			if (declare_type != body_arg->type)
			{
				SEMA_ERROR(body_arg->var.type_info, "This parameter should be '%s' but was '%s'",
				           type_to_error_string(declare_type),
				           type_quoted_error_string(body_arg->type));
				return false;
			}
		}
		if (!body_arg->alignment) body_arg->alignment = type_alloca_alignment(body_arg->type);
	}


	DynamicScope old_scope = context->active_scope;
	context_change_scope_with_flags(context, SCOPE_NONE);

	SemaContext macro_context;

	Type *rtype = NULL;
	sema_context_init(&macro_context, decl->unit);
	macro_context.compilation_unit = context->unit;
	macro_context.call_env = context->call_env;
	rtype = decl->func_decl.signature.rtype ? type_infoptr(decl->func_decl.signature.rtype)->type : NULL;
	macro_context.expected_block_type = rtype;
	bool may_failable = true;
	if (rtype)
	{
		if (type_is_optional(rtype))
		{
			failable = true;
			rtype = type_no_optional(rtype);
		}
		else
		{
			may_failable = false;
		}
	}

	context_change_scope_with_flags(&macro_context, SCOPE_MACRO);

	macro_context.block_return_defer = macro_context.active_scope.defer_last;

	macro_context.current_macro = decl;
	AstId body_id = call_expr->call_expr.body;
	macro_context.yield_body = body_id ? astptr(body_id) : NULL;
	macro_context.yield_params = body_params;
	macro_context.yield_context = context;
	macro_context.macro_varargs = call_expr->call_expr.varargs;
	macro_context.original_inline_line = context->original_inline_line ? context->original_inline_line : call_expr->span.row;
	macro_context.macro_params = params;
	BlockExit** block_exit_ref = MALLOCS(BlockExit*);
	macro_context.block_exit_ref = block_exit_ref;

	VECEACH(params, i)
	{
		Decl *param = params[i];
		// Skip raw vararg
		if (!param) continue;
		if (!sema_add_local(&macro_context, param)) goto EXIT_FAIL;
	}

	AstId assert_first = 0;
	AstId* next = &assert_first;

	if (!sema_analyse_contracts(&macro_context, docs, &next)) return false;
	sema_append_contract_asserts(assert_first, body);

	if (!sema_analyse_statement(&macro_context, body)) goto EXIT_FAIL;

	params = macro_context.macro_params;
	bool is_no_return = decl->func_decl.signature.attrs.noreturn;

	if (!vec_size(macro_context.returns) || !macro_context.active_scope.jump_end)
	{
		if (rtype && rtype != type_void)
		{
			SEMA_ERROR(decl,
					   "Missing return in macro that should evaluate to %s.",
					   type_quoted_error_string(rtype));
			return SCOPE_POP_ERROR();
		}
	}
	else if (is_no_return)
	{
		SEMA_ERROR(context->returns[0], "Return used despite macro being marked '@noreturn'.");
		return SCOPE_POP_ERROR();
	}

	if (rtype)
	{
		bool inferred_len = type_len_is_inferred(rtype);
		VECEACH(macro_context.returns, i)
		{
			Ast *return_stmt = macro_context.returns[i];
			if (!return_stmt)
			{
				assert(may_failable);
				continue;
			}
			Expr *ret_expr = return_stmt->return_stmt.expr;
			if (!ret_expr)
			{
				if (rtype == type_void) continue;
				SEMA_ERROR(return_stmt, "Expected returning a value of type %s.", type_quoted_error_string(rtype));
				return SCOPE_POP_ERROR();
			}
			Type *type = ret_expr->type;
			if (inferred_len)
			{
				Type *flattened = type_flatten(type);
				if (flattened->type_kind == TYPE_ARRAY && rtype->type_kind == TYPE_INFERRED_ARRAY)
				{
					rtype = type_get_array(rtype->array.base, flattened->array.len);
					inferred_len = false;
				}
				else if (flattened->type_kind == TYPE_VECTOR && rtype->type_kind == TYPE_INFERRED_VECTOR)
				{
					rtype = type_get_vector(rtype->array.base, flattened->array.len);
					inferred_len = false;
				}
			}
			if (!cast_may_implicit(type, rtype, CAST_OPTION_SIMPLE_EXPR | (may_failable ? CAST_OPTION_ALLOW_OPTIONAL : 0)) || inferred_len)
			{
				SEMA_ERROR(ret_expr, "Expected %s, not %s.", type_quoted_error_string(rtype),
						   type_quoted_error_string(type));
				return SCOPE_POP_ERROR();
			}
			bool success = cast_implicit(context, ret_expr, rtype);
			assert(success);
			if (may_failable) ret_expr->type = type_add_optional(ret_expr->type, may_failable);
		}
		call_expr->type = type_add_optional(rtype, failable);
	}
	else
	{
		Type *sum_returns = context_unify_returns(&macro_context);
		if (!sum_returns) return SCOPE_POP_ERROR();
		call_expr->type = type_add_optional(sum_returns, failable);
	}

	assert(call_expr->type);
	if (call_expr->call_expr.result_unused)
	{
		Type *type = call_expr->type;
		if (type != type_void)
		{
			if (decl->func_decl.signature.attrs.nodiscard)
			{
				SEMA_ERROR(call_expr, "The result of the macro must be used.");
				return SCOPE_POP_ERROR();
			}
			if (type_is_optional(type) && !decl->func_decl.signature.attrs.maydiscard)
			{
				SEMA_ERROR(call_expr, "The optional result of the macro must be used.");
				return SCOPE_POP_ERROR();
			}
		}
	}
	unsigned returns_found = vec_size(macro_context.returns);
	// We may have zero normal macro returns but the active scope still has a "jump end".
	// In this case it is triggered by the @body()
	if (!returns_found && macro_context.active_scope.jump_end)
	{
		is_no_return = true;
	}
	if (returns_found == 1)
	{
		Ast *ret = macro_context.returns[0];
		Expr *result = ret ? ret->return_stmt.expr : NULL;
		if (result && expr_is_constant_eval(result, CONSTANT_EVAL_CONSTANT_VALUE))
		{
			if (ast_is_compile_time(body))
			{
				expr_replace(call_expr, result);
				goto EXIT;
			}
		}
	}

	call_expr->expr_kind = EXPR_MACRO_BLOCK;
	call_expr->macro_block.first_stmt = body->compound_stmt.first_stmt;
	call_expr->macro_block.params = params;
	call_expr->macro_block.block_exit = block_exit_ref;
EXIT:
	assert(context->active_scope.defer_last == context->active_scope.defer_start);
	context->active_scope = old_scope;
	if (is_no_return) context->active_scope.jump_end = true;
	return true;
EXIT_FAIL:
	return SCOPE_POP_ERROR();
}

static bool sema_call_analyse_body_expansion(SemaContext *macro_context, Expr *call)
{
	Decl *macro = macro_context->current_macro;
	assert(macro);
	DeclId body_param = macro->func_decl.body_param;
	assert(body_param);

	ExprCall *call_expr = &call->call_expr;
	if (vec_size(call_expr->body_arguments))
	{
		SEMA_ERROR(call, "Nested expansion is not possible.");
		return false;
	}
	if (call_expr->splat_vararg)
	{
		SEMA_ERROR(call, "Expanding parameters is not allowed for macro invocations.");
		return false;
	}
	// Theoretically we could support named arguments, but that's unnecessary.
	unsigned expressions = vec_size(call_expr->arguments);
	Decl **body_parameters = declptr(body_param)->body_params;
	if (expressions != vec_size(body_parameters))
	{
		SEMA_ERROR(call, "Expected %d parameter(s).", vec_size(body_parameters));
		return false;
	}
	Expr **args = call_expr->arguments;

	// Evaluate the expressions. TODO hash expressions
	for (unsigned i = 0; i < expressions; i++)
	{
		Expr *expr = args[i];
		if (!sema_analyse_expr(macro_context, expr)) return false;
	}

	AstId macro_defer = macro_context->active_scope.defer_last;
	Ast *first_defer = NULL;
	SemaContext *context = macro_context->yield_context;
	Decl **params = macro_context->yield_params;
	Expr *func_expr = exprptr(call_expr->function);
	assert(func_expr->expr_kind == EXPR_MACRO_BODY_EXPANSION);
	expr_replace(call, func_expr);
	call->body_expansion_expr.values = args;
	call->body_expansion_expr.declarations = macro_context->yield_params;
	AstId last_defer = context->active_scope.defer_last;
	bool success;
	bool ends_in_jump;
	SCOPE_START

		if (macro_defer)
		{
			Ast *macro_defer_ast = astptr(macro_defer);
			first_defer = macro_defer_ast;
			while (first_defer->defer_stmt.prev_defer)
			{
				first_defer = astptr(first_defer->defer_stmt.prev_defer);
			}
			first_defer->defer_stmt.prev_defer = context->active_scope.defer_last;
			context->active_scope.defer_last = macro_defer;
		}
		VECEACH(params, i)
		{
			Decl *param = params[i];
			if (!sema_add_local(context, param)) return SCOPE_POP_ERROR();
		}
		Ast *ast = copy_ast_single(macro_context->yield_body);
		call->body_expansion_expr.first_stmt = astid(ast);
		if (!sema_analyse_statement(context, ast)) return SCOPE_POP_ERROR();
		assert(ast->ast_kind == AST_COMPOUND_STMT);
		if (context->active_scope.jump_end)
		{
			macro_context->active_scope.jump_end = true;
		}
		if (first_defer)
		{
			first_defer->defer_stmt.prev_defer = 0;
			context->active_scope.defer_last = last_defer;
		}
	SCOPE_END;

	return true;
}

bool sema_expr_analyse_general_call(SemaContext *context, Expr *expr, Decl *decl, Expr *struct_var, bool failable)
{
	expr->call_expr.is_type_method = struct_var != NULL;
	if (decl == NULL)
	{
		return sema_expr_analyse_var_call(context, expr,
		                                  type_flatten_distinct_optional(exprptr(expr->call_expr.function)->type), failable);
	}
	switch (decl->decl_kind)
	{
		case DECL_MACRO:
			expr->call_expr.func_ref = declid(decl);
			expr->call_expr.is_func_ref = true;
			return sema_expr_analyse_macro_call(context, expr, struct_var, decl, failable);
		case DECL_VAR:
			assert(struct_var == NULL);
			return sema_expr_analyse_var_call(context, expr, decl->type->canonical, failable || IS_OPTIONAL(decl));
		case DECL_FUNC:
			expr->call_expr.func_ref = declid(decl);
			expr->call_expr.is_func_ref = true;
			return sema_expr_analyse_func_call(context, expr, decl, struct_var, failable);
		case DECL_GENERIC:
			expr->call_expr.func_ref = declid(decl);
			expr->call_expr.is_func_ref = true;
			TODO // Maybe generics won't happen
		case DECL_POISONED:
			return false;
		default:
			SEMA_ERROR(expr, "This expression cannot be called.");
			return false;
	}
}





static inline bool sema_expr_analyse_call(SemaContext *context, Expr *expr)
{
	Expr *func_expr = exprptr(expr->call_expr.function);

	if (!sema_analyse_expr_lvalue_fold_const(context, func_expr)) return false;
	if (func_expr->expr_kind == EXPR_MACRO_BODY_EXPANSION)
	{
		return sema_call_analyse_body_expansion(context, expr);
	}
	bool failable = func_expr->type && IS_OPTIONAL(func_expr);
	Decl *decl;
	Expr *struct_var = NULL;
	switch (func_expr->expr_kind)
	{
		case EXPR_BUILTIN:
			return sema_expr_analyse_builtin_call(context, expr);
		case EXPR_IDENTIFIER:
			decl = func_expr->identifier_expr.decl;
			break;
		case EXPR_ACCESS:
			decl = func_expr->access_expr.ref;
			switch (decl->decl_kind)
			{
				case DECL_MACRO:
				case DECL_FUNC:
					struct_var = func_expr->access_expr.parent;
					if (decl->func_decl.signature.params[0]->type->type_kind == TYPE_POINTER)
					{
						expr_insert_addr(struct_var);
					}
					break;
				default:
					break;
			}
			break;
		case EXPR_TYPEINFO:
			if (func_expr->type_expr->resolve_status == RESOLVE_DONE)
			{
				SEMA_ERROR(expr, "A type cannot be followed by (), if you intended a cast, use (type)(expression).");
			}
			else
			{
				SEMA_ERROR(expr, "A type cannot be followed by (), did you mean to use {}?");
			}
			return false;
		default:
		{
			Type *type = type_flatten_distinct(func_expr->type);
			if (type->type_kind == TYPE_POINTER)
			{
				decl = NULL;
				break;
			}
			SEMA_ERROR(expr, "This value cannot be invoked, did you accidentally add ()?");
			return false;

		}
	}
	decl = decl ? decl_flatten(decl) : NULL;
	return sema_expr_analyse_general_call(context, expr, decl, struct_var, failable);
}

static void sema_subscript_deref_array_pointers(Expr *expr)
{
	Type *expr_type = expr->type->canonical;
	if (expr_type->type_kind == TYPE_POINTER)
	{
		switch (expr_type->pointer->type_kind)
		{
			case TYPE_ARRAY:
			case TYPE_VECTOR:
				expr_rewrite_insert_deref(expr);
				break;
			default:
				break;
		}
	}
}

static bool sema_slice_len_is_in_range(SemaContext *context, Type *type, Expr *len_expr, bool from_end, bool *remove_from_end)
{
	assert(type == type->canonical);
	if (len_expr->expr_kind != EXPR_CONST) return true;

	Int const_len = len_expr->const_expr.ixx;
	if (!int_fits(const_len, TYPE_I64))
	{
		SEMA_ERROR(len_expr, "The length cannot be stored in a 64-signed integer, which isn't supported.");
		return false;
	}
	if (int_is_neg(const_len))
	{
		SEMA_ERROR(len_expr, "The length may not be negative.");
		return false;
	}
	MemberIndex len_val = (MemberIndex)const_len.i.low;
	switch (type->type_kind)
	{
		case TYPE_POINTER:
		case TYPE_FLEXIBLE_ARRAY:
			assert(!from_end);
			FALLTHROUGH;
		case TYPE_SUBARRAY:
			return true;
		case TYPE_ARRAY:
		case TYPE_VECTOR:
		{
			MemberIndex len = (MemberIndex)type->array.len;
			bool is_vector = type->type_kind == TYPE_VECTOR;
			if (from_end)
			{
				if (len_val > len)
				{
					SEMA_ERROR(len_expr, "This would result in a negative length.");
					return false;
				}
				len_expr->const_expr.ixx.i.low = len - len_val;
				*remove_from_end = true;
				return true;
			}
			// Checking end can only be done for arrays and vectors.
			if (len_val > len)
			{
				SEMA_ERROR(len_expr,
				           is_vector ? "Length out of bounds, was %lld, exceeding vector length %lld."
				                     : "Array length out of bounds, was %lld, exceeding array length %lld.",
				           (long long)len_val, (long long)len);
				return false;
			}
			return true;
		}
		default:
			UNREACHABLE
	}
	UNREACHABLE
}

static bool sema_slice_index_is_in_range(SemaContext *context, Type *type, Expr *index_expr, bool end_index, bool from_end, bool *remove_from_end)
{
	assert(type == type->canonical);
	if (index_expr->expr_kind != EXPR_CONST) return true;

	Int index = index_expr->const_expr.ixx;
	if (!int_fits(index, TYPE_I64))
	{
		SEMA_ERROR(index_expr, "The index cannot be stored in a 64-signed integer, which isn't supported.");
		return false;
	}
	if (from_end && int_is_neg(index))
	{
		SEMA_ERROR(index_expr, "Negative numbers are not allowed when indexing from the end.");
		return false;
	}
	MemberIndex idx = (MemberIndex)index.i.low;
	switch (type->type_kind)
	{
		case TYPE_POINTER:
		case TYPE_FLEXIBLE_ARRAY:
			assert(!from_end);
			return true;
		case TYPE_UNTYPED_LIST:
		case TYPE_ARRAY:
		case TYPE_VECTOR:
		{
			MemberIndex len = (MemberIndex)type->array.len;
			if (from_end)
			{
				idx = len - idx;
				index_expr->const_expr.ixx.i.low = idx;
				*remove_from_end = true;
			}
			// Checking end can only be done for arrays.
			if (end_index && idx >= len)
			{
				SEMA_ERROR(index_expr, "End index out of bounds, was %lld, exceeding %lld.", (long long)idx, (long long)len);
				return false;
			}
			if (!end_index && idx >= len)
			{
				if (len == 0)
				{
					SEMA_ERROR(index_expr, "Cannot index into a zero size list.");
					return false;
				}
				SEMA_ERROR(index_expr, "Index out of bounds, was %lld, exceeding maximum (%lld).", (long long)idx, (long long)len - 1);
				return false;
			}
			break;
		}
		case TYPE_SUBARRAY:
			// If not from end, just check the negative values.
			if (!from_end) break;
			// From end we can only do sanity checks ^0 is invalid for non-end index. ^-1 and less is invalid for all.
			if (idx == 0 && !end_index)
			{
				SEMA_ERROR(index_expr,
				           "Array index out of bounds, index from end (%lld) must be greater than zero or it will exceed the max array index.",
				           (long long) idx);
				return false;
			}
			return true;
		default:
			UNREACHABLE
	}
	if (idx < 0)
	{
		SEMA_ERROR(index_expr, "Index out of bounds, using a negative index is only allowed for pointers.");
		return false;
	}
	return true;
}

static Type *sema_subscript_find_indexable_type_recursively(Type **type, Expr **parent)
{
	while (1)
	{
		Type *inner_type = type_get_indexed_type(*type);
		if (!inner_type && type_is_substruct(*type))
		{
			Expr *embedded_struct = expr_access_inline_member(*parent, (*type)->decl);
			*type = embedded_struct->type->canonical;
			*parent = embedded_struct;
			continue;
		}
		return inner_type;
	}
}

static bool sema_subscript_rewrite_index_const_list(Expr *const_list, Expr *index, Expr *result)
{
	assert(index->expr_kind == EXPR_CONST && index->const_expr.const_kind == CONST_INTEGER);
	if (!int_fits(index->const_expr.ixx, TYPE_U32)) return false;

	uint32_t idx = index->const_expr.ixx.i.low;
	assert(const_list->const_expr.const_kind == CONST_INITIALIZER);

	return expr_rewrite_to_const_initializer_index(const_list->type, const_list->const_expr.initializer, result, idx);
}

/**
 * Find index type or overload for subscript.
 */
static Expr *sema_expr_find_index_type_or_overload_for_subscript(SemaContext *context, Expr *current_expr, SubscriptEval eval_type, Type **index_type_ptr, Decl **overload_ptr)
{
	Decl *overload = NULL;
	switch (eval_type)
	{
		case SUBSCRIPT_EVAL_REF:
			overload = sema_find_operator(context, current_expr, OVERLOAD_ELEMENT_REF);
			break;
		case SUBSCRIPT_EVAL_VALUE:
			overload = sema_find_operator(context, current_expr, OVERLOAD_ELEMENT_AT);
			break;
		case SUBSCRIPT_EVAL_ASSIGN:
			overload = sema_find_operator(context, current_expr, OVERLOAD_ELEMENT_SET);
			if (overload)
			{
				*overload_ptr = overload;
				assert(vec_size(overload->func_decl.signature.params) == 3);
				*index_type_ptr = overload->func_decl.signature.params[2]->type;
				return current_expr;
			}
			break;
	}
	// Overload found for [] and &[]
	if (overload)
	{
		*overload_ptr = overload;
		assert(overload->func_decl.signature.rtype);
		*index_type_ptr = type_infoptr(overload->func_decl.signature.rtype)->type;
		return current_expr;
	}
	// Otherwise, see if we have an indexed type.
	Type *inner_type = type_get_indexed_type(current_expr->type);
	if (inner_type)
	{
		*index_type_ptr  = inner_type;
		*overload_ptr = NULL;
		return current_expr;
	}
	if (type_is_substruct(current_expr->type))
	{
		Expr *embedded_struct = expr_access_inline_member(current_expr, current_expr->type->decl);
		return sema_expr_find_index_type_or_overload_for_subscript(context, embedded_struct, eval_type, index_type_ptr, overload_ptr);
	}
	return NULL;
}

static inline bool sema_expr_analyse_subscript(SemaContext *context, Expr *expr, SubscriptEval eval_type)
{
	assert(expr->expr_kind == EXPR_SUBSCRIPT || expr->expr_kind == EXPR_SUBSCRIPT_ADDR);

	// 1. Evaluate the expression to index.
	Expr *subscripted = exprptr(expr->subscript_expr.expr);
	if (!sema_analyse_expr_lvalue_fold_const(context, subscripted)) return false;
	sema_subscript_deref_array_pointers(subscripted);

	// 2. Evaluate the index.
	Expr *index = exprptr(expr->subscript_expr.range.start);
	if (!sema_analyse_expr(context, index)) return false;

	// 3. Check failability due to value.
	bool failable = IS_OPTIONAL(subscripted);

	Type *underlying_type = type_flatten(subscripted->type);

	Type *current_type = underlying_type;

	int64_t index_value = -1;
	bool start_from_end = expr->subscript_expr.range.start_from_end;
	int64_t size;
	if (expr_is_const_int(index) && (size = expr_get_index_max(subscripted)) >= 0)
	{
		// 4c. And that it's in range.
		if (int_is_neg(index->const_expr.ixx))
		{
			SEMA_ERROR(index, "The index may not be negative.");
			return false;
		}
		if (!int_fits(index->const_expr.ixx, TYPE_I64) || size == 0)
		{
			SEMA_ERROR(index, "The index is out of range.", size);
			return false;
		}
		index_value = int_to_i64(index->const_expr.ixx);
		if (start_from_end)
		{
			index_value = size - index_value;
		}
		if (index_value < 0 || index_value >= size)
		{
			if (start_from_end)
			{
				SEMA_ERROR(index,
				           size > 1
				           ? "An index of '%lld' from the end is out of range, a value between 1 and %lld was expected."
				           : "An index of '%lld' from the end is out of range, a value of %lld was expected.",
				           (long long)(size - index_value),
				           (long long)size);
			}
			else
			{
				SEMA_ERROR(index,
				           size > 1
				           ? "An index of '%lld' is out of range, a value between 0 and %lld was expected."
				           : "An index of '%lld' is out of range, a value of %lld was expected.",
				           (long long)index_value,
				           (long long)size - 1);
			}
			return false;
		}
	}
	// 4. If we are indexing into a complist
	if (underlying_type == type_untypedlist)
	{
		if (eval_type == SUBSCRIPT_EVAL_REF)
		{
			SEMA_ERROR(subscripted, "You need to use && to take the address of a temporary.");
			return false;
		}
		// 4a. This may either be an initializer list or a CT value
		Expr *current_expr = subscripted;
		while (subscripted->expr_kind == EXPR_CT_IDENT) current_expr = current_expr->ct_ident_expr.decl->var.init_expr;

		// 4b. Now we need to check that we actually have a valid type.
		if (index_value < 0)
		{
			SEMA_ERROR(index, "To subscript an untyped list a compile time integer index is needed.");
			return false;
		}
		if (eval_type == SUBSCRIPT_EVAL_ASSIGN) TODO;
		expr_replace(expr, current_expr->const_expr.untyped_list[index_value]);
		return true;
	}
	if (!sema_cast_rvalue(context, subscripted)) return false;

	Decl *overload = NULL;
	Type *index_type = NULL;
	Expr *current_expr = sema_expr_find_index_type_or_overload_for_subscript(context, subscripted, eval_type, &index_type, &overload);
	if (!index_type)
	{
		if (!overload && eval_type == SUBSCRIPT_EVAL_REF)
		{
			// Maybe there is a [] overload?
			if (sema_expr_find_index_type_or_overload_for_subscript(context,
			                                                        subscripted,
			                                                        SUBSCRIPT_EVAL_VALUE,
			                                                        &index_type,
			                                                        &overload))
			{
				SEMA_ERROR(expr,
				           "A function or macro with '@operator(&[])' is not defined for %s, so you need && to take the address of the temporary.",
				           type_quoted_error_string(current_expr->type));
				return false;
			}
		}
		SEMA_ERROR(subscripted, "Cannot index '%s'.", type_to_error_string(subscripted->type));
		return false;
	}
	if (overload)
	{
		if (eval_type == SUBSCRIPT_EVAL_ASSIGN)
		{
			expr->expr_kind = EXPR_SUBSCRIPT_ASSIGN;
			expr->subscript_assign_expr.expr = exprid(current_expr);
			expr->subscript_assign_expr.index = exprid(index);
			expr->subscript_assign_expr.method = declid(overload);
			return true;
		}
		Expr **args = NULL;
		vec_add(args, index);
		return sema_insert_method_call(context, expr, overload, current_expr, args);
	}

	// Cast to an appropriate type for index.
	if (!cast_to_index(index)) return false;

	// Check range
	bool remove_from_back = false;
	if (!sema_slice_index_is_in_range(context, current_type, index, false, start_from_end, &remove_from_back)) return false;
	if (remove_from_back)
	{
		start_from_end = expr->subscript_expr.range.start_from_end = false;
		(void)start_from_end;
	}

	if (eval_type == SUBSCRIPT_EVAL_REF)
	{
		index_type = type_get_ptr(index_type);
	}
	else
	{
		if (expr_is_const_initializer(current_expr) && expr_is_const(index))
		{
			if (sema_subscript_rewrite_index_const_list(current_expr, index, expr)) return true;
		}
	}
	expr->subscript_expr.expr = exprid(current_expr);
	expr->type = type_add_optional(index_type, failable);
	return true;
}

static inline bool sema_expr_analyse_pointer_offset(SemaContext *context, Expr *expr)
{
	assert(expr->expr_kind == EXPR_POINTER_OFFSET);

	// 1. Evaluate the pointer
	Expr *pointer = exprptr(expr->pointer_offset_expr.ptr);
	if (!sema_analyse_expr(context, pointer)) return false;

	// 2. Evaluate the offset.
	Expr *offset = exprptr(expr->pointer_offset_expr.offset);
	if (!sema_analyse_expr(context, offset)) return false;
	if (!cast_implicit(context, offset, type_isz)) return false;

	// 3. Store optionality
	bool is_optional = IS_OPTIONAL(pointer) || IS_OPTIONAL(offset);

	// 4. Possibly constant fold
	if (expr_is_const(pointer) && expr_is_const(offset))
	{
		assert(!is_optional);
		Int mul = { .i.low = type_size(type_flatten(pointer->type)->pointer), .type = offset->const_expr.ixx.type };
		Int offset_val = int_mul(mul, offset->const_expr.ixx);
		Int res = int_add64(offset_val, pointer->const_expr.ptr);
		pointer->const_expr.ptr = res.i.low;
		expr_replace(expr, pointer);
		return true;
	}
	expr->type = type_add_optional(pointer->type, is_optional);
	return true;
}

static inline bool sema_expr_analyse_slice(SemaContext *context, Expr *expr)
{
	assert(expr->expr_kind == EXPR_SLICE);
	Expr *subscripted = exprptr(expr->subscript_expr.expr);
	if (!sema_analyse_expr(context, subscripted)) return false;
	bool failable = IS_OPTIONAL(subscripted);
	Type *type = type_flatten(subscripted->type);
	Type *original_type = type_no_optional(subscripted->type);
	Expr *start = exprptr(expr->subscript_expr.range.start);
	Expr *end = exprptrzero(expr->subscript_expr.range.end);

	Expr *current_expr = subscripted;

	Type *inner_type = sema_subscript_find_indexable_type_recursively(&type, &current_expr);
	if (!inner_type)
	{
		SEMA_ERROR(subscripted, "Cannot index '%s'.", type_to_error_string(subscripted->type));
		return false;
	}
	expr->subscript_expr.expr = exprid(current_expr);

	if (!sema_analyse_expr(context, start)) return false;
	if (end && !sema_analyse_expr(context, end)) return false;

	// Fix index sizes
	if (!cast_to_index(start)) return false;
	if (end && !cast_to_index(end)) return false;
	if (end && end->type != start->type)
	{
		Type *common = type_find_max_type(start->type, end->type);
		if (!common || !cast_implicit(context, start, common) || !cast_implicit(context, end, common)) return false;
	}

	bool start_from_end = expr->subscript_expr.range.start_from_end;
	bool end_from_end = expr->subscript_expr.range.end_from_end;

	// Check range
	if (type->type_kind == TYPE_POINTER)
	{
		if (start_from_end)
		{
			SEMA_ERROR(start, "Indexing from the end is not allowed for pointers.");
			return false;
		}
		if (!end)
		{
			SEMA_ERROR(expr, "Omitting end index is not allowed for pointers.");
			return false;
		}
		if (end && end_from_end)
		{
			SEMA_ERROR(end, "Indexing from the end is not allowed for pointers.");
			return false;
		}
	}
	bool is_lenrange = expr->subscript_expr.range.is_len;
	bool remove_from_end = false;
	if (!sema_slice_index_is_in_range(context, type, start, false, start_from_end, &remove_from_end)) return false;
	if (remove_from_end)
	{
		start_from_end = expr->subscript_expr.range.start_from_end = false;
	}
	remove_from_end = false;
	if (end)
	{
		if (is_lenrange)
		{
			if (!sema_slice_len_is_in_range(context, type, end, end_from_end, &remove_from_end)) return false;
		}
		else
		{
			if (!sema_slice_index_is_in_range(context, type, end, true, end_from_end, &remove_from_end)) return false;
		}
	}
	if (remove_from_end)
	{
		end_from_end = expr->subscript_expr.range.end_from_end = false;
	}

	if (start && end && start->expr_kind == EXPR_CONST && end->expr_kind == EXPR_CONST)
	{
		if (!is_lenrange && start_from_end && end_from_end)
		{
			if (expr_const_compare(&start->const_expr, &end->const_expr, BINARYOP_LT))
			{
				SEMA_ERROR(start, "Start index greater than end index.");
				return false;
			}
		}
		else if (!is_lenrange && !start_from_end && !end_from_end)
		{
			if (expr_const_compare(&start->const_expr, &end->const_expr, BINARYOP_GT))
			{
				SEMA_ERROR(start, "Start index greater than end index.");
				return false;
			}
		}
		// If both are
		if (type->type_kind == TYPE_ARRAY || type->type_kind == TYPE_VECTOR)
		{
			assert(!start_from_end);
			assert(!end_from_end);
			if (!is_lenrange)
			{
				end->const_expr.ixx = int_sub(int_add64(end->const_expr.ixx, 1), start->const_expr.ixx);
				is_lenrange = expr->subscript_expr.range.is_len = true;
				(void)is_lenrange;
			}
		}
	}

	// Retain the original type when doing distinct slices.
	Type *result_type = type_get_subarray(inner_type);
	Type *original_type_canonical = original_type->canonical;
	if (original_type_canonical->type_kind == TYPE_DISTINCT && type_flatten_distinct(original_type_canonical) == result_type)
	{
		result_type = original_type;
	}
	expr->type = type_add_optional(result_type, failable);
	return true;
}


static inline bool sema_expr_analyse_group(SemaContext *context, Expr *expr)
{
	if (!sema_analyse_expr(context, expr->inner_expr)) return false;
	*expr = *expr->inner_expr;
	return true;
}

/**
 * 1. .A -> It is an enum constant.
 * 2. .foo -> It is a function.
 * 3. .@foo -> It is a macro.
 * 4. .#bar -> It is an identifier to resolve as a member or a function
 * 5. .@#bar -> It is an identifier to resolve as a macro
 * 6. .$eval(...) -> resolve the eval and retry.
 */
static Expr *sema_expr_resolve_access_child(SemaContext *context, Expr *child, bool *missing)
{
RETRY:
	assert(child->resolve_status != RESOLVE_DONE);
	switch (child->expr_kind)
	{
		case EXPR_IDENTIFIER:
			// A path is not allowed.
			if (child->identifier_expr.path) break;
			return child;
		case EXPR_HASH_IDENT:
		{
			Decl *decl = sema_resolve_symbol(context, child->hash_ident_expr.identifier, NULL, child->span);
			if (!decl_ok(decl)) return NULL;
			return sema_expr_resolve_access_child(context, decl->var.init_expr, missing);
		}
		case EXPR_CT_EVAL:
		{
			TokenType type;
			// Only report missing if missing var is NULL
			Path *path = NULL;
			Expr *result = sema_ct_eval_expr(context, false, child->inner_expr, missing == NULL);
			if (!result)
			{
				if (missing) *missing = true;
				return NULL;
			}
			expr_replace(child, result);
			goto RETRY;
		}
		default:
			break;

	}
	SEMA_ERROR(child, "Expected an identifier here.");
	return NULL;
}

static inline void sema_expr_replace_with_enum_array(Expr *enum_array_expr, Decl *enum_decl)
{
	Decl **values = enum_decl->enums.values;
	SourceSpan span = enum_array_expr->span;
	Expr *initializer = expr_new(EXPR_INITIALIZER_LIST, span);
	ArraySize elements = vec_size(values);
	Expr **element_values = elements > 0 ? VECNEW(Expr*, elements) : NULL;
	Type *kind = enum_decl->type;
	ConstKind const_kind = enum_decl->decl_kind == DECL_FAULT ? CONST_ERR : CONST_ENUM;
	for (ArraySize i = 0; i < elements; i++)
	{
		Decl *decl = values[i];
		Expr *expr = expr_new(EXPR_CONST, span);
		expr->const_expr.const_kind = const_kind;
		expr->const_expr.enum_err_val = decl;
		assert(decl_ok(decl));
		expr->type = kind;
		expr->resolve_status = RESOLVE_DONE;
		vec_add(element_values, expr);
	}
	initializer->initializer_list = element_values;
	enum_array_expr->expr_kind = EXPR_COMPOUND_LITERAL;
	enum_array_expr->expr_compound_literal.initializer = initializer;
	enum_array_expr->expr_compound_literal.type_info = type_info_new_base(type_get_array(kind, elements), span);
	enum_array_expr->resolve_status = RESOLVE_NOT_DONE;
}

static inline void sema_expr_replace_with_enum_name_array(Expr *enum_array_expr, Decl *enum_decl)
{
	Decl **values = enum_decl->enums.values;
	SourceSpan span = enum_array_expr->span;
	Expr *initializer = expr_new(EXPR_INITIALIZER_LIST, span);
	ArraySize elements = vec_size(values);
	Expr **element_values = elements > 0 ? VECNEW(Expr*, elements) : NULL;
	for (ArraySize i = 0; i < elements; i++)
	{
		Decl *decl = values[i];
		Expr *expr = expr_new(EXPR_CONST, span);
		expr_rewrite_to_string(expr, decl->name);
		vec_add(element_values, expr);
	}
	initializer->initializer_list = element_values;
	enum_array_expr->expr_kind = EXPR_COMPOUND_LITERAL;
	enum_array_expr->expr_compound_literal.initializer = initializer;
	enum_array_expr->expr_compound_literal.type_info = type_info_new_base(type_get_subarray(type_chars), span);
	enum_array_expr->resolve_status = RESOLVE_NOT_DONE;
}

static inline bool sema_expr_analyse_type_access(SemaContext *context, Expr *expr, TypeInfo *parent, bool was_group, Expr *identifier)
{
	assert(identifier->expr_kind == EXPR_IDENTIFIER);

	Type *canonical = parent->type->canonical;
	const char *name = identifier->identifier_expr.ident;
	bool is_const = identifier->identifier_expr.is_const;

	if (!is_const)
	{
		if (sema_expr_rewrite_to_type_property(context, expr, canonical, type_property_by_name(name),
		                                       type_abi_alignment(parent->type), 0)) return true;
	}

	if (!type_may_have_sub_elements(canonical))
	{
		SEMA_ERROR(expr, "'%s' does not have a property '%s'.", type_to_error_string(parent->type), name);
		return false;
	}
	Decl *decl = canonical->decl;
	// TODO add more constants that can be inspected?
	// e.g. SomeEnum.values, MyUnion.x.offset etc?
	switch (decl->decl_kind)
	{
		case DECL_ENUM:
			if (is_const)
			{
				if (!sema_expr_analyse_enum_constant(expr, name, decl))
				{
					SEMA_ERROR(expr, "'%s' has no enumeration value '%s'.", decl->name, name);
					return false;
				}
				return true;
			}
			break;
		case DECL_FAULT:
			unit_register_external_symbol(context->compilation_unit, decl);
			if (is_const)
			{
				if (!sema_expr_analyse_enum_constant(expr, name, decl))
				{
					SEMA_ERROR(expr, "'%s' has no error value '%s'.", decl->name, name);
					return false;
				}
				return true;
			}
			break;
		case DECL_UNION:
		case DECL_STRUCT:
		case DECL_DISTINCT:
		case DECL_BITSTRUCT:
			break;
		default:
			UNREACHABLE
	}

	Decl *member = sema_decl_stack_find_decl_member(decl, name);
	if (!member)
	{
		SEMA_ERROR(expr, "No method or inner struct/union '%s.%s' found.", type_to_error_string(decl->type), name);
		return false;
	}

	if (member->decl_kind == DECL_VAR || member->decl_kind == DECL_UNION || member->decl_kind == DECL_STRUCT || member->decl_kind == DECL_BITSTRUCT)
	{
		expr->expr_kind = EXPR_CONST;
		expr->const_expr = (ExprConst) {
			.member.decl = member,
			.member.align = type_abi_alignment(decl->type),
			.member.offset = decl_find_member_offset(decl, member),
			.const_kind = CONST_MEMBER
		};
		expr->type = type_member;
		return true;
	}

	expr->identifier_expr.ident = name;
	expr->expr_kind = EXPR_IDENTIFIER;
	expr->identifier_expr.decl = member;
	expr->type = member->type;
	return true;
}

static inline bool sema_expr_analyse_member_access(SemaContext *context, Expr *expr, Expr *parent, bool was_group, Expr *identifier)
{
	assert(identifier->expr_kind == EXPR_IDENTIFIER);

	Decl *decl = parent->const_expr.member.decl;
	const char *name = identifier->identifier_expr.ident;
	bool is_const = identifier->identifier_expr.is_const;

	if (is_const)
	{
		SEMA_ERROR(expr, "There is no member '%s' for %s.", name, type_to_error_string(decl->type));
		return false;
	}

	if (name == kw_offsetof)
	{
		expr_rewrite_const_int(expr, type_usize, parent->const_expr.member.offset, true);
		return true;
	}
	TypeProperty type_property = type_property_by_name(name);
	switch (type_property)
	{
		case TYPE_PROPERTY_NONE:
			break;
		case TYPE_PROPERTY_QNAMEOF:
			break;
		case TYPE_PROPERTY_NAMEOF:
			expr_rewrite_to_string(expr, decl->name ? decl->name : "");
			return true;
		case TYPE_PROPERTY_ALIGNOF:
			expr_rewrite_const_int(expr, type_usize,
								   type_min_alignment(parent->const_expr.member.offset, parent->const_expr.member.align),
								   true);
			return true;
		case TYPE_PROPERTY_MEMBERSOF:
			sema_create_const_membersof(context, expr, decl->type->canonical, parent->const_expr.member.align, parent->const_expr.member.offset);
			return true;
		case TYPE_PROPERTY_KINDOF:
		case TYPE_PROPERTY_SIZEOF:
			if (sema_expr_rewrite_to_type_property(context, expr, decl->type, type_property,
												   parent->const_expr.member.align,
												   parent->const_expr.member.offset)) return true;
			return true;
		case TYPE_PROPERTY_ELEMENTS:
		case TYPE_PROPERTY_EXTNAMEOF:
		case TYPE_PROPERTY_PARAMS:
		case TYPE_PROPERTY_RETURNS:
		case TYPE_PROPERTY_INF:
		case TYPE_PROPERTY_LEN:
		case TYPE_PROPERTY_MAX:
		case TYPE_PROPERTY_MIN:
		case TYPE_PROPERTY_NAN:
		case TYPE_PROPERTY_INNER:
		case TYPE_PROPERTY_NAMES:
		case TYPE_PROPERTY_VALUES:
			break;
	}

	Type *underlying_type = type_flatten_distinct(decl->type);

	if (!type_is_union_or_strukt(underlying_type) && underlying_type->type_kind != TYPE_BITSTRUCT)
	{
		SEMA_ERROR(parent, "No member or property '%s' was found.", name);
		return false;
	}

	Decl *underlying_type_decl = underlying_type->decl;
	Decl *member = sema_decl_stack_find_decl_member(underlying_type_decl, name);
	if (!member || !(decl_is_struct_type(member) || member->decl_kind == DECL_VAR || member->decl_kind == DECL_BITSTRUCT))
	{
		SEMA_ERROR(expr, "No member '%s' found.", name);
		return false;
	}

	expr->expr_kind = EXPR_CONST;
	expr->const_expr = (ExprConst) {
		.member.decl = member,
		.member.align = parent->const_expr.member.align,
		.member.offset = parent->const_expr.member.offset + decl_find_member_offset(decl, member),
		.const_kind = CONST_MEMBER
	};
	expr->type = type_member;
	return true;
}

static inline void sema_expr_rewrite_typeid_kind(Expr *expr, Expr *parent)
{
	Module *module = global_context_find_module(kw_std__core__types);
	Decl *type_kind = module ? module_find_symbol(module, kw_typekind) : NULL;
	Type *type_for_kind = type_kind ? type_kind->type : type_char;
	expr->expr_kind = EXPR_TYPEID_INFO;
	expr->typeid_info_expr.parent = exprid(parent);
	expr->typeid_info_expr.kind = TYPEID_INFO_KIND;
	expr->type = type_for_kind;
}


static inline bool sema_create_const_kind(Expr *expr, Type *type)
{
	Module *module = global_context_find_module(kw_std__core__types);
	Decl *type_kind = module ? module_find_symbol(module, kw_typekind) : NULL;
	Type *type_for_kind = type_kind ? type_kind->type : type_char;
	unsigned val = type_get_introspection_kind(type->type_kind);
	assert(type_for_kind->type_kind == TYPE_ENUM);
	expr_rewrite_const_int(expr, type_flatten(type_for_kind), val, false);
	return cast(expr, type_for_kind);
}

static inline bool sema_create_const_len(SemaContext *context, Expr *expr, Type *type)
{
	assert(type == type_flatten_distinct(type) && "Should be flattened already.");
	size_t len;
	switch (type->type_kind)
	{
		case TYPE_ARRAY:
		case TYPE_VECTOR:
			len = type->array.len;
			break;
		case TYPE_ENUM:
		case TYPE_FAULTTYPE:
			len = vec_size(type->decl->enums.values);
			break;
		case TYPE_SCALED_VECTOR:
		case TYPE_INFERRED_ARRAY:
		case TYPE_FLEXIBLE_ARRAY:
		case TYPE_SUBARRAY:
		default:
			return false;
	}
	expr_rewrite_const_int(expr, type_usize, len, true);
	return true;
}

static inline bool sema_create_const_inner(SemaContext *context, Expr *expr, Type *type)
{
	Type *inner = NULL;
	switch (type->type_kind)
	{
		case TYPE_POINTER:
			inner = type->pointer;
			break;
		case TYPE_OPTIONAL:
			inner = type->failable;
			break;
		case TYPE_DISTINCT:
			inner = type->decl->distinct_decl.base_type->canonical;
			break;
		case TYPE_ENUM:
			inner = type->decl->enums.type_info->type->canonical;
			break;
		case TYPE_BITSTRUCT:
			inner = type->decl->bitstruct.base_type->type->canonical;
			break;
		case TYPE_ARRAY:
		case TYPE_FLEXIBLE_ARRAY:
		case TYPE_SUBARRAY:
		case TYPE_INFERRED_ARRAY:
		case TYPE_INFERRED_VECTOR:
		case TYPE_SCALED_VECTOR:
		case TYPE_VECTOR:
			inner = type->array.base;
			break;
		default:
			return false;
	}
	expr_rewrite_const_typeid(expr, inner);
	return true;
}

static inline bool sema_create_const_min(SemaContext *context, Expr *expr, Type *type, Type *flat)
{
	if (type_is_float(flat))
	{
		expr->expr_kind = EXPR_CONST;
		expr->const_expr.const_kind = CONST_FLOAT;
		expr->type = type;
		expr->resolve_status = RESOLVE_DONE;
		expr->const_expr.fxx.type = flat->type_kind;
		switch (flat->type_kind)
		{
			case TYPE_F16:
				expr->const_expr.fxx.f = -65504.0;
				break;
			case TYPE_F32:
				expr->const_expr.fxx.f = -FLT_MAX;
				break;
			case TYPE_F64:
				expr->const_expr.fxx.f = -DBL_MAX;
				break;
			case TYPE_F128:
				REMINDER("Float 128 not complete");
				expr->const_expr.fxx.f = -DBL_MAX;
				break;
			default:
				UNREACHABLE;
		}
		return true;
	}
	else if (type_is_integer(type))
	{
		expr->expr_kind = EXPR_CONST;
		expr->const_expr.const_kind = CONST_INTEGER;
		expr->type = type;
		expr->resolve_status = RESOLVE_DONE;
		expr->const_expr.ixx.type = flat->type_kind;
		switch (flat->type_kind)
		{
			case TYPE_I8:
				expr->const_expr.ixx.i = (Int128){ 0, 0x80 };
				break;
			case TYPE_I16:
				expr->const_expr.ixx.i = (Int128){ 0, 0x8000 };
				break;
			case TYPE_I32:
				expr->const_expr.ixx.i = (Int128){ 0, 1ULL << 31 };
				break;
			case TYPE_I64:
				expr->const_expr.ixx.i = (Int128){ 0, 1ULL << 63 };
				break;
			case TYPE_I128:
				expr->const_expr.ixx.i = (Int128){ 1ULL << 63, 0 };
				break;
			default:
				expr->const_expr.ixx.i = (Int128){ 0, 0 };
				break;
		}
		return true;
	}
	return false;
}

static inline bool sema_create_const_params(SemaContext *context, Expr *expr, Type *type)
{
	if (type->type_kind != TYPE_POINTER || type->pointer->type_kind != TYPE_FUNC) return false;
	type = type->pointer;
	Signature *sig = type->function.signature;
	unsigned params = vec_size(sig->params);
	Expr **param_exprs = params ? VECNEW(Expr*, params) : NULL;
	for (unsigned i = 0; i < params; i++)
	{
		Decl *decl = sig->params[i];
		Expr *expr_element = expr_new_const_typeid(expr->span, decl->type->canonical);
		vec_add(param_exprs, expr_element);
	}
	expr_rewrite_const_untyped_list(expr, param_exprs);
	return true;
}

static inline void sema_create_const_membersof(SemaContext *context, Expr *expr, Type *type, AlignSize alignment,
                                               AlignSize offset)
{
	Decl **members = NULL;
	if (type_is_union_or_strukt(type))
	{
		members = type->decl->strukt.members;
	}
	else if (type->type_kind == TYPE_BITSTRUCT)
	{
		members = type->decl->bitstruct.members;
	}
	else
	{
		expr_rewrite_const_untyped_list(expr, NULL);
		return;
	}
	unsigned count = vec_size(members);
	Expr **member_exprs = count ? VECNEW(Expr*, count) : NULL;
	for (unsigned i = 0; i < count; i++)
	{
		Decl *decl = members[i];
		Expr *expr_element = expr_new(EXPR_CONST, expr->span);
		expr_element->type = type_member;
		expr_element->const_expr = (ExprConst) {
			.const_kind = CONST_MEMBER,
			.member.decl = decl,
			.member.offset = offset + decl->offset,
			.member.align = alignment
		};
		vec_add(member_exprs, expr_element);
	}
	expr_rewrite_const_untyped_list(expr, member_exprs);
}

static inline bool sema_create_const_max(SemaContext *context, Expr *expr, Type *type, Type *flat)
{
	if (type_is_integer(flat))
	{
		expr->expr_kind = EXPR_CONST;
		expr->const_expr.const_kind = CONST_INTEGER;
		expr->type = type;
		expr->resolve_status = RESOLVE_DONE;
		expr->const_expr.ixx.type = flat->type_kind;
		switch (flat->type_kind)
		{
			case TYPE_I8:
				expr->const_expr.ixx.i = (Int128){ 0, 0x7F };
				break;
			case TYPE_I16:
				expr->const_expr.ixx.i = (Int128){ 0, 0x7FFF };
				break;
			case TYPE_I32:
				expr->const_expr.ixx.i = (Int128){ 0, 0x7FFFFFFFLL };
				break;
			case TYPE_I64:
				expr->const_expr.ixx.i = (Int128){ 0, 0x7FFFFFFFFFFFFFFFLL };
				break;
			case TYPE_I128:
				expr->const_expr.ixx.i = (Int128){ 0x7FFFFFFFFFFFFFFFLL, 0xFFFFFFFFFFFFFFFFLL };
				break;
			case TYPE_U8:
				expr->const_expr.ixx.i = (Int128){ 0, 0xFF };
				break;
			case TYPE_U16:
				expr->const_expr.ixx.i = (Int128){ 0, 0xFFFF };
				break;
			case TYPE_U32:
				expr->const_expr.ixx.i = (Int128){ 0, 0xFFFFFFFFLL };
				break;
			case TYPE_U64:
				expr->const_expr.ixx.i = (Int128){ 0, 0xFFFFFFFFFFFFFFFFLL };
				break;
			case TYPE_U128:
				expr->const_expr.ixx.i = (Int128){ 0xFFFFFFFFFFFFFFFFLL, 0xFFFFFFFFFFFFFFFFLL };
				break;
			default:
				UNREACHABLE
		}
		return true;
	}
	else if (type_is_float(flat))
	{
		expr->expr_kind = EXPR_CONST;
		expr->const_expr.const_kind = CONST_FLOAT;
		expr->type = type;
		expr->resolve_status = RESOLVE_DONE;
		expr->const_expr.fxx.type = flat->type_kind;
		switch (flat->type_kind)
		{
			case TYPE_F16:
				expr->const_expr.fxx.f = 65504.0;
				break;
			case TYPE_F32:
				expr->const_expr.fxx.f = FLT_MAX;
				break;
			case TYPE_F64:
				expr->const_expr.fxx.f = DBL_MAX;
				break;
			case TYPE_F128:
				REMINDER("Float 128 not complete");
				expr->const_expr.fxx.f = DBL_MAX;
				break;
			default:
				UNREACHABLE;
		}
		return true;
	}
	return false;
}

static bool sema_expr_rewrite_typeid_call(Expr *expr, Expr *typeid, TypeIdInfoKind kind, Type *result_type)
{
	expr->expr_kind = EXPR_TYPEID_INFO;
	expr->typeid_info_expr.parent = exprid(typeid);
	expr->typeid_info_expr.kind = kind;
	expr->type = result_type;
	return true;
}
static bool sema_expr_rewrite_to_typeid_property(SemaContext *context, Expr *expr, Expr *typeid, const char *kw)
{
	TypeProperty property = type_property_by_name(kw);
	if (typeid->expr_kind == EXPR_CONST)
	{
		Type *type = typeid->const_expr.typeid;
		return sema_expr_rewrite_to_type_property(context, expr, type, property, type_abi_alignment(type), 0);
	}
	switch (property)
	{
		case TYPE_PROPERTY_SIZEOF:
			return sema_expr_rewrite_typeid_call(expr, typeid, TYPEID_INFO_SIZEOF, type_usize);
		case TYPE_PROPERTY_LEN:
			return sema_expr_rewrite_typeid_call(expr, typeid, TYPEID_INFO_LEN, type_usize);
		case TYPE_PROPERTY_INNER:
			return sema_expr_rewrite_typeid_call(expr, typeid, TYPEID_INFO_INNER, type_typeid);
		case TYPE_PROPERTY_KINDOF:
			sema_expr_rewrite_typeid_kind(expr, typeid);
			return true;
		case TYPE_PROPERTY_NAMES:
			return sema_expr_rewrite_typeid_call(expr, typeid, TYPEID_INFO_NAMES, type_get_subarray(type_chars));
		case TYPE_PROPERTY_ALIGNOF:
		case TYPE_PROPERTY_INF:
		case TYPE_PROPERTY_MIN:
		case TYPE_PROPERTY_MAX:
		case TYPE_PROPERTY_NAN:
		case TYPE_PROPERTY_ELEMENTS:
		case TYPE_PROPERTY_VALUES:
		case TYPE_PROPERTY_RETURNS:
		case TYPE_PROPERTY_PARAMS:
		case TYPE_PROPERTY_MEMBERSOF:
		case TYPE_PROPERTY_EXTNAMEOF:
		case TYPE_PROPERTY_NAMEOF:
		case TYPE_PROPERTY_QNAMEOF:
			// Not supported by dynamic typeid
		case TYPE_PROPERTY_NONE:
			return false;
	}
	UNREACHABLE
	return false;
}

static inline bool sema_expr_fold_to_member(Expr *expr, Expr *parent, Decl *member)
{
	ConstInitializer *init = parent->const_expr.initializer;
	ConstInitializer *result;
	switch (init->kind)
	{
		case CONST_INIT_ZERO:
			result = init;
			goto EVAL;
		case CONST_INIT_STRUCT:
		{
			Decl **members = init->type->decl->strukt.members;
			VECEACH(members, i)
			{
				if (members[i] == member)
				{
					result = init->init_struct[i];
					goto EVAL;
				}
			}
			UNREACHABLE
		}
		case CONST_INIT_UNION:
			if (init->type->decl->strukt.members[init->init_union.index] != member) return false;
			result = init->init_union.element;
			goto EVAL;
		case CONST_INIT_VALUE:
		case CONST_INIT_ARRAY:
		case CONST_INIT_ARRAY_FULL:
		case CONST_INIT_ARRAY_VALUE:
			UNREACHABLE;
	}
	UNREACHABLE
EVAL:
	switch (result->kind)
	{
		case CONST_INIT_ZERO:
			expr_rewrite_to_const_zero(expr, result->type);
			break;
		case CONST_INIT_STRUCT:
		case CONST_INIT_UNION:
		case CONST_INIT_ARRAY:
		case CONST_INIT_ARRAY_FULL:
			expr->const_expr.const_kind = CONST_INITIALIZER;
			expr->const_expr.initializer = init;
			expr->type = init->type;
			break;
		case CONST_INIT_ARRAY_VALUE:
			UNREACHABLE
		case CONST_INIT_VALUE:
			expr_replace(expr, result->init_value);
			break;
	}
	return true;
}

static bool sema_expr_rewrite_to_type_property(SemaContext *context, Expr *expr, Type *type, TypeProperty property,
                                               AlignSize alignment, AlignSize offset)
{
	assert(type == type->canonical);
	if (property == TYPE_PROPERTY_NONE) return false;

	Type *flat = type_flatten_distinct(type);
	switch (property)
	{
		case TYPE_PROPERTY_INF:
			if (!type_is_float(flat)) return false;
			expr->expr_kind = EXPR_CONST;
			expr->const_expr.const_kind = CONST_FLOAT;
			expr->const_expr.fxx = (Float) { INFINITY, flat->type_kind };
			expr->type = type;
			expr->resolve_status = RESOLVE_DONE;
			return true;
		case TYPE_PROPERTY_INNER:
			return sema_create_const_inner(context, expr, type);
		case TYPE_PROPERTY_KINDOF:
			return sema_create_const_kind(expr, type);
		case TYPE_PROPERTY_LEN:
			return sema_create_const_len(context, expr, flat);
		case TYPE_PROPERTY_MIN:
			return sema_create_const_min(context, expr, type, flat);
		case TYPE_PROPERTY_MAX:
			return sema_create_const_max(context, expr, type, flat);
		case TYPE_PROPERTY_NAMES:
			if (!type_kind_is_enumlike(flat->type_kind)) return false;
			sema_expr_replace_with_enum_name_array(expr, flat->decl);
			return sema_analyse_expr(context, expr);
		case TYPE_PROPERTY_ELEMENTS:
			if (!type_kind_is_enumlike(flat->type_kind)) return false;
			expr_rewrite_const_int(expr, type_isize, vec_size(flat->decl->enums.values), true);
			return true;
		case TYPE_PROPERTY_VALUES:
			if (!type_kind_is_enumlike(flat->type_kind)) return false;
			sema_expr_replace_with_enum_array(expr, flat->decl);
			return sema_analyse_expr(context, expr);
		case TYPE_PROPERTY_NAN:
			if (!type_is_float(flat)) return false;
			expr->expr_kind = EXPR_CONST;
			expr->const_expr.const_kind = CONST_FLOAT;
			expr->const_expr.fxx = (Float) { nan(""), flat->type_kind };
			expr->type = type;
			expr->resolve_status = RESOLVE_DONE;
			return true;
		case TYPE_PROPERTY_MEMBERSOF:
			sema_create_const_membersof(context, expr, flat, alignment, offset);
			return true;
		case TYPE_PROPERTY_PARAMS:
			return sema_create_const_params(context, expr, flat);
		case TYPE_PROPERTY_RETURNS:
			if (flat->type_kind != TYPE_POINTER || flat->pointer->type_kind != TYPE_FUNC) return false;
			flat = flat->pointer;
			expr_rewrite_const_typeid(expr, type_infoptr(flat->function.signature->rtype)->type);
			return true;
		case TYPE_PROPERTY_SIZEOF:
			expr_rewrite_const_int(expr, type_usize, type_size(type), true);
			return true;
		case TYPE_PROPERTY_NAMEOF:
			sema_expr_rewrite_to_type_nameof(expr, type, TOKEN_CT_NAMEOF);
			return true;
		case TYPE_PROPERTY_QNAMEOF:
			sema_expr_rewrite_to_type_nameof(expr, type, TOKEN_CT_QNAMEOF);
			return true;
		case TYPE_PROPERTY_ALIGNOF:
			expr_rewrite_const_int(expr, type_usize, type_abi_alignment(type), true);
			return true;
		case TYPE_PROPERTY_EXTNAMEOF:
			if (type_is_builtin(type->type_kind)) return false;
			sema_expr_rewrite_to_type_nameof(expr, type, TOKEN_CT_EXTNAMEOF);
			return true;
		case TYPE_PROPERTY_NONE:
			return false;
	}
	UNREACHABLE
}

/**
 * Analyse "x.y"
 */
static inline bool sema_expr_analyse_access(SemaContext *context, Expr *expr)
{
	Expr *parent = expr->access_expr.parent;
	bool was_group = parent->expr_kind == EXPR_GROUP;

	// 1. Resolve the left hand
	if (!sema_analyse_expr_lvalue_fold_const(context, parent)) return false;

	// 2. The right hand side may be a @ident or ident
	Expr *child = expr->access_expr.child;

	// 3. Handle xxxxxx.typeid
	if (child->expr_kind == EXPR_TYPEINFO)
	{
		if (child->type_expr->resolve_status != RESOLVE_DONE || child->type_expr->type != type_typeid)
		{
			SEMA_ERROR(child, "A type can't appear here.");
			return false;
		}

		if (parent->expr_kind == EXPR_TYPEINFO)
		{
			expr->type = type_typeid;
			expr->expr_kind = EXPR_CONST;
			expr->const_expr.const_kind = CONST_TYPEID;
			expr->const_expr.typeid = parent->type_expr->type->canonical;
			expr->resolve_status = RESOLVE_DONE;
			return true;
		}
		if (expr_is_const_member(parent))
		{
			expr->type = type_typeid;
			expr->expr_kind = EXPR_CONST;
			expr->const_expr.const_kind = CONST_TYPEID;
			expr->const_expr.typeid = parent->const_expr.member.decl->type->canonical;
			expr->resolve_status = RESOLVE_DONE;
			return true;
		}
		SEMA_ERROR(expr, "'typeid' can only be used with types, not values");
		return false;
	}

	// 3. Find the actual token.
	SourceSpan span;
	Expr *identifier = sema_expr_resolve_access_child(context, child, NULL);
	if (!identifier) return false;

	// 2. If our left-hand side is a type, e.g. MyInt.abc, handle this here.
	if (parent->expr_kind == EXPR_TYPEINFO)
	{
		return sema_expr_analyse_type_access(context, expr, parent->type_expr, was_group, identifier);
	}
	if (expr_is_const_member(parent))
	{
		return sema_expr_analyse_member_access(context, expr, parent, was_group, identifier);
	}

	// 6. Copy failability
	bool failable = IS_OPTIONAL(parent);

	assert(expr->expr_kind == EXPR_ACCESS);
	assert(parent->resolve_status == RESOLVE_DONE);

	// 7. Is this a pointer? If so we insert a deref.
	bool is_pointer = type_no_optional(parent->type)->canonical->type_kind == TYPE_POINTER;
	if (is_pointer)
	{
		if (!sema_cast_rvalue(context, parent)) return false;
		expr_rewrite_insert_deref(expr->access_expr.parent);
		parent = expr->access_expr.parent;
	}

	// 8. Depending on parent type, we have some hard coded types
	Expr *current_parent = parent;

	Type *type = type_no_optional(parent->type)->canonical;
	Type *flat_type = type_flatten(type);
	const char *kw = identifier->identifier_expr.ident;

	if (kw_type == kw && flat_type->type_kind == TYPE_ANY)
	{
		expr_rewrite_to_builtin_access(expr, parent, ACCESS_TYPEOFANY, type_typeid);
		return true;
	}

CHECK_DEEPER:

	// 9. Fix hard coded function `len` on subarrays and arrays
	if (kw == kw_len)
	{
		if (flat_type->type_kind == TYPE_SUBARRAY)
		{
			expr_rewrite_to_builtin_access(expr, current_parent, ACCESS_LEN, type_usize);
			return true;
		}
		if (flat_type->type_kind == TYPE_ARRAY || flat_type->type_kind == TYPE_VECTOR)
		{
			expr_rewrite_const_int(expr, type_isize, flat_type->array.len, true);
			return true;
		}
		if (flat_type->type_kind == TYPE_UNTYPED_LIST)
		{
			expr_rewrite_const_int(expr, type_isize, vec_size(current_parent->const_expr.untyped_list), true);
			return true;
		}
	}
	if (flat_type->type_kind == TYPE_TYPEID)
	{
		if (sema_expr_rewrite_to_typeid_property(context, expr, parent, kw)) return true;
	}

	// Hard coded ptr on subarrays and variant
	if (kw == kw_ptr)
	{
		if (flat_type->type_kind == TYPE_SUBARRAY)
		{
			expr_rewrite_to_builtin_access(expr, current_parent, ACCESS_PTR, type_get_ptr(flat_type->array.base));
			return true;
		}
		if (flat_type->type_kind == TYPE_ANY)
		{
			expr_rewrite_to_builtin_access(expr, current_parent, ACCESS_PTR, type_voidptr);
			return true;
		}
	}

	if (kw == kw_ordinal)
	{
		if (type->type_kind == TYPE_ENUM)
		{
			if (!cast(current_parent, type->decl->enums.type_info->type)) return false;
			expr_replace(expr, current_parent);
			return true;
		}
	}
	if (kw == kw_nameof)
	{
		if (type->type_kind == TYPE_ENUM)
		{
			if (current_parent->expr_kind == EXPR_CONST)
			{
				expr_rewrite_to_string(expr, current_parent->const_expr.enum_err_val->name);
				return true;
			}
			else
			{
				expr_rewrite_to_builtin_access(expr, current_parent, ACCESS_ENUMNAME, type_chars);
				return true;
			}
		}
		if (type->type_kind == TYPE_FAULTTYPE || type->type_kind == TYPE_ANYERR)
		{
			if (current_parent->expr_kind == EXPR_CONST)
			{
				expr_rewrite_to_string(expr, current_parent->const_expr.enum_err_val->name);
				return true;
			}
			expr_rewrite_to_builtin_access(expr, current_parent, ACCESS_FAULTNAME, type_chars);
			return true;
		}
	}

	// 9. At this point we may only have distinct, struct, union, error, enum
	if (!type_may_have_sub_elements(type))
	{
		Decl *ambiguous = NULL;
		Decl *private = NULL;
		Decl *method = sema_resolve_type_method(context->unit, type, kw, &ambiguous, &private);
		if (private)
		{
			SEMA_ERROR(expr, "The method '%s' has private visibility.", kw);
			return false;
		}
		if (ambiguous)
		{
			SEMA_ERROR(expr, "'%s' is an ambiguous name and so cannot be resolved, it may refer to method defined in '%s' or one in '%s'",
			           kw, method->unit->module->name->module, ambiguous->unit->module->name->module);
			return false;
		}
		if (!method)
		{
			SEMA_ERROR(expr, "There is no member or method '%s' on '%s'", kw, type_to_error_string(type));
			return false;
		}
		expr->access_expr.parent = current_parent;
		expr->type = method->type ? type_add_optional(method->type, failable) : NULL;
		expr->access_expr.ref = method;
		if (method->decl_kind == DECL_FUNC) unit_register_external_symbol(context->compilation_unit, method);
		return true;
	}

	// 10. Dump all members and methods into the scope.
	Decl *decl = type->decl;
	Decl *member = sema_decl_stack_find_decl_member(decl, kw);

	if (member && decl_is_enum_kind(decl) && member->decl_kind == DECL_VAR && parent->expr_kind == EXPR_CONST)
	{
		assert(parent->const_expr.const_kind == CONST_ENUM);
		Expr *copy_init = copy_expr_single(current_parent->const_expr.enum_err_val->enum_constant.args[member->var.index]);
		expr_replace(expr, copy_init);
		return true;
	}
	Decl *private = NULL;
	if (!member)
	{
		Decl *ambiguous = NULL;
		member = sema_resolve_method(context->unit, decl, kw, &ambiguous, &private);
		if (ambiguous)
		{
			SEMA_ERROR(expr, "'%s' is an ambiguous name and so cannot be resolved, it may refer to method defined in '%s' or one in '%s'",
					   kw, member->unit->module->name->module, ambiguous->unit->module->name->module);
			return false;
		}
	}

	if (member && member->decl_kind == DECL_FUNC)
	{
		unit_register_external_symbol(context->compilation_unit, member);
	}

	// 11. If we didn't find a match...
	if (!member)
	{
		// 11a. We have a potential embedded struct check:
		if (type_is_substruct(type))
		{
			Expr *embedded_struct = expr_access_inline_member(parent, type->decl);
			current_parent = embedded_struct;
			type = embedded_struct->type->canonical;
			goto CHECK_DEEPER;
		}

		// 11b. Otherwise we give up.
		if (private)
		{
			SEMA_ERROR(expr, "The method '%s' has private visibility.", kw);
			return false;
		}
		SEMA_ERROR(expr, "There is no field or method '%s.%s'.", type_to_error_string(parent->type), kw);
		return false;
	}

	assert(member->type);
	if (member->decl_kind == DECL_VAR)
	{
		if (member->var.kind == VARDECL_BITMEMBER)
		{
			// Transform bitstruct access to expr_bitaccess.
			expr->expr_kind = EXPR_BITACCESS;
		}
		else if (member->var.kind == VARDECL_MEMBER && expr_is_const_initializer(current_parent))
		{
			sema_expr_fold_to_member(expr, current_parent, member);
			return true;
		}
	}
	// 13. Copy properties.
	expr->access_expr.parent = current_parent;
	expr->type = type_add_optional(member->type, failable);
	expr->access_expr.ref = member;
	return true;
}

static Expr **sema_vasplat_append(SemaContext *c, Expr **init_expressions, Expr *expr)
{
	Expr **args = c->macro_varargs;
	unsigned param_count = vec_size(args);
	Range *range = &expr->vasplat_expr;
	Expr *start = exprptrzero(range->start);
	unsigned start_idx = 0;
	if (start)
	{
		if (!range->is_range)
		{
			SEMA_ERROR(expr, "$vasplat() expected a range.");
			return NULL;
		}
		if (!sema_analyse_expr(c, start)) return NULL;
		if (!expr_is_const_int(start))
		{
			SEMA_ERROR(expr, "Expected a constant integer.");
			return NULL;
		}
		Int start_index = start->const_expr.ixx;
		if (int_is_neg(start_index))
		{
			SEMA_ERROR(expr, "Expected a positive integer.");
			return NULL;
		}
		if (int_bits_needed(start_index) > 31)
		{
			SEMA_ERROR(expr, "Start index is too big.");
			return NULL;
		}
		start_idx = start_index.i.low;
		if (range->start_from_end)
		{
			start_idx = param_count - start_idx;
		}
		if (param_count < start_idx)
		{
			SEMA_ERROR(expr, "Start index exceeds the number of parameters (%d).", start_idx);
			return NULL;
		}
	}
	Expr *end = exprptrzero(range->end);
	unsigned end_idx = param_count;
	if (end)
	{
		if (!sema_analyse_expr(c, end)) return NULL;
		if (!expr_is_const_int(end))
		{
			SEMA_ERROR(expr, "Expected a constant integer.");
			return NULL;
		}
		Int end_index = end->const_expr.ixx;
		if (int_is_neg(end_index))
		{
			SEMA_ERROR(expr, "Expected a positive integer.");
			return NULL;
		}
		if (int_bits_needed(end_index) > 31)
		{
			if (range->is_len)
			{
				SEMA_ERROR(expr, "End index is too large.");
			}
			else
			{
				SEMA_ERROR(expr, "Length is too large.");
			}
			return NULL;
		}
		end_idx = end_index.i.low;
		if (range->end_from_end)
		{
			if (end_idx > param_count)
			{
				end_idx = 0;
			}
			else
			{
				end_idx = param_count - end_idx;
			}
		}
		if (range->is_len)
		{
			end_idx = start_idx + end_idx;
		}
		else
		{
			end_idx++;
		}
		if (param_count <= end_idx)
		{
			SEMA_ERROR(expr, "End index would exceed the number of parameters.");
			return NULL;
		}
	}
	for (unsigned i = start_idx; i < end_idx; i++)
	{
		vec_add(init_expressions, args[i]);
	}
	return init_expressions;
}


INLINE Expr **sema_expand_vasplat(SemaContext *c, Expr **list, unsigned index)
{
	unsigned size = vec_size(list);

	// If it was the last element then just append.
	if (index == size - 1)
	{
		vec_pop(list);
		return sema_vasplat_append(c, list, list[index]);
	}
	// Otherwise append to the end.
	list = sema_vasplat_append(c, list, list[index]);
	if (!list) return NULL;
	unsigned new_size = vec_size(list);
	unsigned added_elements = new_size - size;

	if (added_elements == 0)
	{
		for (unsigned i = index + 1; i < size; i++)
		{
			list[i - 1] = list[i];
		}
		vec_pop(list);
		return list;
	}

	// Copy those elements
	for (unsigned i = 0; i < added_elements; i++)
	{
		unsigned dest = index + i;
		unsigned source = size + i;
		// Copy the next element to the index position.
		list[dest] = list[source];
		// Copy the following into the place of the index.
		list[source] = list[dest + 1];
	}
	vec_pop(list);
	return list;
}

Expr **sema_expand_vasplat_exprs(SemaContext *c, Expr **exprs)
{
	if (!c->current_macro) return exprs;

	unsigned count = vec_size(exprs);
	bool expand;
	do
	{
		expand = false;
		for (unsigned i = 0; i < count; i++)
		{
			if (exprs[i]->expr_kind == EXPR_VASPLAT)
			{
				exprs = sema_expand_vasplat(c, exprs, i);
				// If we have null back it failed.
				if (!exprs) return NULL;
				count = vec_size(exprs);
				expand = true;
				break;
			}
		}
	} while (expand);
	return exprs;
}





static inline bool sema_expr_analyse_expr_list(SemaContext *context, Expr *expr)
{
	bool success = true;
	ByteSize last = vec_size(expr->expression_list) - 1;
	VECEACH(expr->expression_list, i)
	{
		Expr *checked_expr = expr->expression_list[i];
		success &= sema_analyse_expr(context, checked_expr);
	}
	expr->type = expr->expression_list[last]->type;
	return success;
}


static inline bool sema_expr_analyse_cast(SemaContext *context, Expr *expr)
{
	Expr *inner = exprptr(expr->cast_expr.expr);
	TypeInfo *type_info = type_infoptr(expr->cast_expr.type_info);
	bool success = sema_resolve_type_info(context, type_info);
	if (!sema_analyse_expr(context, inner) || !success) return false;

	Type *target_type = type_info->type;
	if (type_is_optional(target_type))
	{
		SEMA_ERROR(type_info, "Casting to an optional type is not allowed.");
		return false;
	}
	if (inner->type == type_untypedlist)
	{
		if (!cast_untyped_to_type(context, inner, target_type)) return false;
		expr_replace(expr, inner);
		return true;
	}
	if (!cast_may_explicit(inner->type, target_type, true, inner->expr_kind == EXPR_CONST))
	{
		return sema_error_failed_cast(expr, type_no_optional(inner->type), target_type);
	}
	if (!cast(inner, target_type))
	{
		return expr_poison(expr);
	}
	expr_replace(expr, inner);
	return true;
}

static inline IndexDiff range_const_len(Range *range)
{
	Expr *start = exprptr(range->start);
	Expr *end = exprptrzero(range->end);
	if (!expr_is_const_int(start)) return -1;
	if (!end || !expr_is_const_int(end)) return -1;
	if (!int_fits(end->const_expr.ixx, TYPE_I32)) return -1;
	if (!int_fits(start->const_expr.ixx, TYPE_I32)) return -1;
	IndexDiff end_val = (IndexDiff)int_to_i64(end->const_expr.ixx);
	if (range->is_len) return end_val;
	IndexDiff start_val = (IndexDiff)int_to_i64(start->const_expr.ixx);
	if (range->start_from_end && range->end_from_end) return start_val - end_val + 1;
	if (range->start_from_end != range->end_from_end) return -1;
	return end_val - start_val + 1;
}

static bool sema_expr_analyse_slice_assign(SemaContext *context, Expr *expr, Type *left_type, Expr *right, bool is_unwrapped)
{
	Expr *left = exprptr(expr->binary_expr.left);
	Type *base = left_type->array.base;
	if (right->expr_kind == EXPR_SLICE)
	{
		Range *left_range = &left->subscript_expr.range;
		Range *right_range = &right->subscript_expr.range;
		if (!sema_analyse_expr(context, right)) return false;
		if (cast_may_implicit(right->type, base, CAST_OPTION_SIMPLE_EXPR)) goto ASSIGN;
		if (!cast_implicit(context, right, left_type)) return false;
		IndexDiff left_len = range_const_len(left_range);
		IndexDiff right_len = range_const_len(right_range);
		if (left_len >= 0 && right_len >= 0 && left_len != right_len)
		{
			SEMA_ERROR(expr, "Length mismatch between subarrays.");
			return false;
		}
		expr->expr_kind = EXPR_SLICE_COPY;
	}
	else
	{
ASSIGN:
		if (!sema_analyse_expr_rhs(context, base, right, false)) return false;
		expr->expr_kind = EXPR_SLICE_ASSIGN;
	}

	expr->type = right->type;
	expr->slice_assign_expr.left = exprid(left);
	expr->slice_assign_expr.right = exprid(right);
	return true;
}

bool sema_expr_analyse_assign_right_side(SemaContext *context, Expr *expr, Type *left_type, Expr *right, bool is_unwrapped)
{
	if (expr && exprptr(expr->binary_expr.left)->expr_kind == EXPR_SLICE)
	{
		return sema_expr_analyse_slice_assign(context, expr, left_type, right, is_unwrapped);
	}

	// 1. Evaluate right side to required type.
	if (!sema_analyse_expr_rhs(context, left_type, right, true)) return false;
	if (IS_OPTIONAL(right) && !type_is_optional(left_type))
	{
		if (is_unwrapped)
		{
			SEMA_ERROR(exprptr(expr->binary_expr.left), "The variable is unwrapped in this context, if you don't want to unwrap it, use () around the variable to suppress unwrapping, like 'catch err = (x)' and 'try (x)'.");
			return false;
		}
		if (!left_type) left_type = type_no_optional(right->type);
		return sema_error_failed_cast(right, right->type, left_type);
	}

	// 3. Set the result to the type on the right side.
	if (expr) expr->type = right->type;

	return true;
}
static inline bool sema_expr_begin_analyse(Expr *expr)
{
	switch (expr->resolve_status)
	{
		case RESOLVE_NOT_DONE:
			expr->resolve_status = RESOLVE_RUNNING;
			return true;
		case RESOLVE_RUNNING:
			SEMA_ERROR(expr, "Recursive resolution of expression");
			return expr_poison(expr);
		case RESOLVE_DONE:
			return false;
	}
	UNREACHABLE
}


static inline bool sema_binary_analyse_ct_identifier_lvalue(SemaContext *context, Expr *expr)
{
	if (!sema_expr_begin_analyse(expr)) return expr_ok(expr);

	Decl *ambiguous_decl = NULL;
	Decl *private_symbol = NULL;
	assert(expr && expr->ct_ident_expr.identifier);
	DEBUG_LOG("Resolving identifier '%s'", expr->ct_ident_expr.identifier);
	Decl *decl = sema_find_symbol(context, expr->ct_ident_expr.identifier);

	if (!decl)
	{
		SEMA_ERROR(expr, "The compile time variable '%s' was not defined in this scope.", expr->ct_ident_expr.identifier);
		return expr_poison(expr);
	}

	if (decl->var.scope_depth < context->active_scope.depth)
	{
		SEMA_ERROR(expr, "Cannot modify '%s' inside of a runtime scope.", decl->name);
		return false;
	}
	expr->ct_ident_expr.decl = decl;
	expr->resolve_status = RESOLVE_DONE;
	return true;
}


static bool sema_expr_analyse_ct_identifier_assign(SemaContext *context, Expr *expr, Expr *left, Expr *right)
{
	if (!sema_binary_analyse_ct_identifier_lvalue(context, left)) return false;

	// 3. Evaluate right side to required type.
	if (!sema_expr_analyse_assign_right_side(context, expr, left->type, right, false)) return false;

	left->ct_ident_expr.decl->var.init_expr = right;
	expr_replace(expr, right);
	return true;
}

static bool sema_expr_analyse_ct_type_identifier_assign(SemaContext *context, Expr *expr, Expr *left, Expr *right)
{
	TypeInfo *info = left->type_expr;

	if (info->kind != TYPE_INFO_CT_IDENTIFIER)
	{
		SEMA_ERROR(left, "A type cannot be assigned to.");
		return false;
	}

	if (!sema_analyse_expr_lvalue_fold_const(context, right)) return false;

	if (right->expr_kind != EXPR_TYPEINFO)
	{
		SEMA_ERROR(right, "Expected a type here.");
		return false;
	}

	Decl *decl = sema_find_symbol(context, info->unresolved.name);

	if (!decl)
	{
		SEMA_ERROR(info, "'%s' is not defined in this scope yet.", info->unresolved.name);
		return false;
	}
	decl->var.init_expr = right;
	expr->expr_kind = EXPR_NOP;
	expr->type = type_void;

	return true;
}

/**
 * Analyse a = b
 * @return true if analysis works
 */
static bool sema_expr_analyse_assign(SemaContext *context, Expr *expr, Expr *left, Expr *right)
{
	// 1. Evaluate left side
	if (left->expr_kind == EXPR_CT_IDENT)
	{
		return sema_expr_analyse_ct_identifier_assign(context, expr, left, right);
	}

	if (left->expr_kind == EXPR_TYPEINFO)
	{
		return sema_expr_analyse_ct_type_identifier_assign(context, expr, left, right);
	}
	if (left->expr_kind == EXPR_SUBSCRIPT)
	{
		if (!sema_expr_analyse_subscript(context, left, SUBSCRIPT_EVAL_ASSIGN)) return false;
	}
	else
	{
		if (!sema_analyse_expr_lvalue(context, left)) return false;
	}

	bool is_subscript_assign = left->expr_kind == EXPR_SUBSCRIPT_ASSIGN;

	// 2. Check assignability
	if (!sema_expr_check_assign(context, left)) return false;

	bool is_unwrapped_var = expr_is_unwrapped_ident(left);

	// 3. Evaluate right side to required type.
	if (!sema_expr_analyse_assign_right_side(context, expr, left->type, right, is_unwrapped_var)) return false;

	if (is_unwrapped_var && IS_OPTIONAL(right))
	{
		sema_rewrap_var(context, left->identifier_expr.decl);
		return true;
	}
	if (left->expr_kind == EXPR_SUBSCRIPT_ASSIGN)
	{
		Expr **args = NULL;
		vec_add(args, exprptr(left->subscript_assign_expr.index));
		vec_add(args, right);
		return sema_insert_method_call(context, expr, declptr(left->subscript_assign_expr.method), exprptr(left->subscript_assign_expr.expr), args);
	}
	if (left->expr_kind == EXPR_BITACCESS)
	{
		if (!sema_bit_assignment_check(right, left->access_expr.ref)) return false;
		expr->expr_kind = EXPR_BITASSIGN;
	}
	return true;
}


/**
 * Analyse define $foo = ...
 *
 * @return true if analysis worked.
 */
static bool sema_binary_analyse_ct_common_assign(SemaContext *context, Expr *expr, Expr *left)
{

	// 1. Analyse left side.
	if (!sema_binary_analyse_ct_identifier_lvalue(context, left)) return false;

	Decl *left_var = left->ct_ident_expr.decl;

	Expr *left_value = left_var->var.init_expr;
	assert(left_value);
	assert(!IS_OPTIONAL(left_value));

	expr->binary_expr.left = exprid(left_value);

	expr->binary_expr.operator = binaryop_assign_base_op(expr->binary_expr.operator);

	if (!sema_expr_analyse_binary(context, expr)) return false;

	if (expr->expr_kind != EXPR_CONST)
	{
		SEMA_ERROR(exprptr(expr->binary_expr.right), "Expected a constant expression.");
		return false;
	}

	left->ct_ident_expr.decl->var.init_expr = expr;

	return true;
}

/**
 * Analyse *= /= %= ^= |= &=
 *
 * @return true if analysis worked.
 */
static bool sema_expr_analyse_op_assign(SemaContext *context, Expr *expr, Expr *left, Expr *right, bool int_only)
{
	if (left->expr_kind == EXPR_CT_IDENT)
	{
		return sema_binary_analyse_ct_common_assign(context, expr, left);
	}

	// 1. Analyse left side.
	if (!sema_analyse_expr_lvalue(context, left)) return false;

	// 2. Verify that the left side is assignable.
	if (!sema_expr_check_assign(context, left)) return false;

	Type *no_fail = type_no_optional(left->type);

	// 3. If this is only defined for ints (*%, ^= |= &= %=) verify that this is an int.
	if (int_only && !type_is_integer(no_fail))
	{
		SEMA_ERROR(left, "Expected an integer here.");
		return false;
	}

	// 4. In any case, these ops are only defined on numbers.
	if (!type_underlying_is_numeric(no_fail))
	{
		SEMA_ERROR(left, "Expected a numeric type here.");
		return false;
	}

	// 5. Cast the right hand side to the one on the left
	if (!sema_analyse_expr(context, right)) return false;
	if (!cast_implicit(context, right, no_fail)) return false;
	if (IS_OPTIONAL(right) && !IS_OPTIONAL(left))
	{
		SEMA_ERROR(right, "This expression cannot be failable, since the assigned variable isn't.");
		return false;
	}
	// 6. Check for zero in case of div or mod.
	if (right->expr_kind == EXPR_CONST)
	{
		if (expr->binary_expr.operator == BINARYOP_DIV_ASSIGN)
		{
			switch (right->const_expr.const_kind)
			{
				case CONST_INTEGER:
					if (int_is_zero(right->const_expr.ixx))
					{
						SEMA_ERROR(right, "Division by zero not allowed.");
						return false;
					}
					break;
				case CONST_FLOAT:
					if (right->const_expr.fxx.f == 0)
					{
						SEMA_ERROR(right, "Division by zero not allowed.");
						return false;
					}
					break;
				default:
					UNREACHABLE
			}
		}
		else if (expr->binary_expr.operator == BINARYOP_MOD_ASSIGN)
		{
			switch (right->const_expr.const_kind)
			{
				case CONST_INTEGER:
					if (int_is_zero(right->const_expr.ixx))
					{
						SEMA_ERROR(right, "% by zero not allowed.");
						return false;
					}
					break;
				default:
					UNREACHABLE
			}
		}
	}

	if (left->expr_kind == EXPR_BITACCESS)
	{
		expr->expr_kind = EXPR_BITASSIGN;
	}
	// 7. Assign type
	expr->type = left->type;
	return true;
}


/**
 * Handle a += b, a -= b
 * @return true if analysis succeeded.
 */
static bool sema_expr_analyse_add_sub_assign(SemaContext *context, Expr *expr, Expr *left, Expr *right)
{
	if (left->expr_kind == EXPR_CT_IDENT)
	{
		return sema_binary_analyse_ct_common_assign(context, expr, left);
	}

	// 1. Analyse the left hand side
	if (!sema_analyse_expr(context, left)) return false;

	// 2. Ensure the left hand side is assignable
	if (!sema_expr_check_assign(context, left)) return false;

	Type *left_type_canonical = left->type->canonical;

	// 4. Analyse right hand side
	REMINDER("Possible deep cast here.");
	if (!sema_analyse_expr(context, right)) return false;

	// 3. Copy type & set properties.
	if (IS_OPTIONAL(right) && !IS_OPTIONAL(left))
	{
		SEMA_ERROR(right, "Cannot assign a failable value to a non-failable.");
		return false;
	}
	expr->type = left->type;
	bool failable = IS_OPTIONAL(left) || IS_OPTIONAL(right);


	// 5. In the pointer case we have to treat this differently.
	if (left_type_canonical->type_kind == TYPE_POINTER)
	{

		if (!cast_decay_array_pointers(context, left)) return false;
		expr->type = left->type;

		// 7. Finally, check that the right side is indeed an integer.
		if (!type_is_integer(right->type->canonical))
		{
			SEMA_ERROR(right, "The right side was '%s' but only integers are valid on the right side of %s when the left side is a pointer.",
			           type_to_error_string(right->type),
			           token_type_to_string(binaryop_to_token(expr->binary_expr.operator)));
			return false;
		}
		return true;
	}

	// 8. Otherwise we cast rhs to lhs
	if (!cast_implicit(context, right, left->type)) return false;

	// 9. We expect a numeric type on both left and right
	if (!type_underlying_is_numeric(left->type))
	{
		SEMA_ERROR(left, "Expected a numeric type here.");
		return false;
	}
	REMINDER("Check if can remove");
	if (left->expr_kind == EXPR_BITACCESS)
	{
		expr->expr_kind = EXPR_BITASSIGN;
	}
	expr->type = type_add_optional(expr->type, failable);
	return true;
}


static bool sema_binary_arithmetic_promotion(SemaContext *context, Expr *left, Expr *right, Type *left_type, Type *right_type, Expr *parent, const char *error_message)
{
	Type *max = cast_numeric_arithmetic_promotion(type_find_max_type(left_type, right_type));
	if (!max || !type_underlying_is_numeric(max))
	{
		if (!error_message)
		{
			return sema_type_error_on_binop(parent);
		}
		SEMA_ERROR(parent, error_message, type_quoted_error_string(left->type), type_quoted_error_string(right->type));
		return false;
	}
	return cast_implicit(context, left, max) && cast_implicit(context, right, max);
}

static void sema_binary_unify_voidptr(Expr *left, Expr *right, Type **left_type_ref, Type **right_type_ref)
{
	if (*left_type_ref == *right_type_ref) return;
	if (*left_type_ref == type_voidptr)
	{
		cast(left, *right_type_ref);
		*left_type_ref = *right_type_ref;
	}
	if (*right_type_ref == type_voidptr)
	{
		cast(right, *left_type_ref);
		*right_type_ref = *left_type_ref;
	}
}

static Type *defer_iptr_cast(Expr *maybe_pointer, Expr *maybe_diff)
{
	// Do we have (iptr)(ptr) +- rhs? If so we change it to
	// (iptr)((char*)(ptr) +- 1)
	if (maybe_pointer->expr_kind == EXPR_CAST
		&& maybe_pointer->cast_expr.kind == CAST_PTRXI
		&& type_flatten(maybe_pointer->type) == type_flatten(type_iptr)
		&& cast_may_implicit(maybe_diff->type, maybe_diff->type, CAST_OPTION_SIMPLE_EXPR | CAST_OPTION_ALLOW_OPTIONAL))
	{
		Type *cast_to_iptr = maybe_pointer->type;
		maybe_pointer->cast_expr.kind = CAST_PTRPTR;
		maybe_pointer->type = type_get_ptr(type_char);
		return cast_to_iptr;
	}
	return NULL;
}

/**
 * Analyse a - b
 * @return true if analysis succeeded
 */
static bool sema_expr_analyse_sub(SemaContext *context, Expr *expr, Expr *left, Expr *right)
{
	// 1. Analyse a and b.
	if (!sema_binary_analyse_subexpr(context, expr, left, right)) return false;

	// Do we have (iptr)(ptr) - rhs? If so we change it to
	// (iptr)((char*)(ptr) - 1)
	Type *cast_to_iptr = defer_iptr_cast(left, right);

	Type *left_type = type_no_optional(left->type)->canonical;
	Type *right_type = type_no_optional(right->type)->canonical;

	// 2. Handle the ptr - x and ptr - other_pointer
	if (left_type->type_kind == TYPE_POINTER)
	{
		if (!cast_decay_array_pointers(context, left)) return false;
		left_type = type_no_optional(left->type)->canonical;

		// 3. ptr - other pointer
		if (right_type->type_kind == TYPE_POINTER)
		{
			if (!cast_decay_array_pointers(context, right)) return false;
			right_type = type_no_optional(right->type)->canonical;

			// 3a. Require that both types are the same.
			sema_binary_unify_voidptr(left, right, &left_type, &right_type);
			if (left_type != right_type)
			{
				SEMA_ERROR(expr, "'%s' - '%s' is not allowed. Subtracting pointers of different types from each other is not possible.", type_to_error_string(left_type), type_to_error_string(right_type));
				return false;
			}

			if (expr_both_const(left, right))
			{
				expr_rewrite_const_int(expr, type_isz, (left->const_expr.ptr - right->const_expr.ptr) /
						type_size(left_type->pointer), false);
				return true;
			}
			// 3b. Set the type
			expr->type = type_isz;

			return true;
		}

		right_type = right->type->canonical;

		// 4. Check that the right hand side is an integer.
		if (!type_is_integer(right_type))
		{
			SEMA_ERROR(expr, "Cannot subtract '%s' from '%s'", type_to_error_string(right_type), type_to_error_string(left_type));
			return false;
		}

		// 5. Make sure that the integer does not exceed isz in size.
		if (type_size(right_type) > type_size(type_isz))
		{
			SEMA_ERROR(expr, "Cannot subtract a '%s' from a pointer, please first cast it to '%s'.", type_to_error_string(right_type), type_to_error_string(type_isz));
			return false;
		}

		// 6. Convert to isz
		if (!cast_implicit(context, right, type_isz)) return true;

		if (left->expr_kind == EXPR_POINTER_OFFSET)
		{
			SourceSpan old_span = expr->span;
			*expr = *left;
			left->span = old_span;
			left->expr_kind = EXPR_BINARY;
			left->binary_expr = (ExprBinary){
					.operator = BINARYOP_SUB,
					.left = expr->pointer_offset_expr.offset,
					.right = exprid(right)
			};
			left->resolve_status = RESOLVE_NOT_DONE;
			if (!sema_analyse_expr(context, left)) return false;
			expr->pointer_offset_expr.offset = exprid(left);
		}
		else
		{
			expr->expr_kind = EXPR_POINTER_OFFSET;
			expr->pointer_offset_expr.ptr = exprid(left);
			expr->pointer_offset_expr.offset = exprid(expr_negate_expr(right));
			expr->pointer_offset_expr.raw_offset = false;
		}

		expr->resolve_status = RESOLVE_NOT_DONE;
		if (!sema_analyse_expr(context, expr)) return false;

		if (cast_to_iptr)
		{
			expr->resolve_status = RESOLVE_DONE;
			return cast(expr, cast_to_iptr);
		}

		return true;
	}

	if (!sema_binary_promote_top_down(context, expr, left, right)) return false;

	left_type = type_no_optional(left->type)->canonical;
	right_type = type_no_optional(right->type)->canonical;

	// 7. Attempt arithmetic promotion, to promote both to a common type.
	if (!sema_binary_arithmetic_promotion(context,
	                                      left,
	                                      right,
	                                      left_type,
	                                      right_type,
	                                      expr,
	                                      "The subtraction %s - %s is not possible."))
	{
		return false;
	}

	left_type = left->type->canonical;

	expr->type = type_add_optional(left->type, IS_OPTIONAL(right));

	// 8. Handle constant folding.
	if (expr_both_const(left, right))
	{
		expr_replace(expr, left);
		switch (left_type->type_kind)
		{
			case ALL_INTS:
				expr->const_expr.ixx = int_sub(left->const_expr.ixx, right->const_expr.ixx);
				break;
			case ALL_FLOATS:
				expr->const_expr.fxx = float_sub(left->const_expr.fxx, right->const_expr.fxx);
				break;
			default:
				UNREACHABLE
		}
	}

	return true;

}

/**
 * Analyse a + b
 * @return true if it succeeds.
 */
static bool sema_expr_analyse_add(SemaContext *context, Expr *expr, Expr *left, Expr *right)
{
	// 1. Promote everything to the recipient type – if possible
	//    this is safe in the pointer case actually.
	if (!sema_binary_analyse_subexpr(context, expr, left, right)) return false;

	Type *cast_to_iptr = defer_iptr_cast(left, right);
	if (!cast_to_iptr) cast_to_iptr = defer_iptr_cast(right, left);

	Type *left_type = type_no_optional(left->type)->canonical;
	Type *right_type = type_no_optional(right->type)->canonical;

	// 2. To detect pointer additions, reorder if needed
	if (right_type->type_kind == TYPE_POINTER && left_type->type_kind != TYPE_POINTER)
	{
		Expr *temp = right;
		right = left;
		left = temp;
		right_type = left_type;
		left_type = left->type->canonical;
		expr->binary_expr.left = exprid(left);
		expr->binary_expr.right = exprid(right);
	}

	// 3. The "left" will now always be the pointer.
	//    so check if we want to do the normal pointer add special handling.
	if (left_type->type_kind == TYPE_POINTER)
	{
		if (!cast_decay_array_pointers(context, left)) return false;

		// 3a. Check that the other side is an integer of some sort.
		if (!type_is_integer(right_type))
		{
			SEMA_ERROR(right, "A value of type '%s' cannot be added to '%s', an integer was expected here.",
			           type_to_error_string(right->type),
			           type_to_error_string(left->type));
			return false;
		}

		// 3b. Cast it to usize or isize depending on underlying type.
		//     Either is fine, but it looks a bit nicer if we actually do this and keep the sign.
		bool success = cast_implicit(context, right, type_isz);

		// No need to check the cast we just ensured it was an integer.
		assert(success && "This should always work");
		(void)success;

		// Folding offset.
		if (left->expr_kind == EXPR_POINTER_OFFSET)
		{
			SourceSpan old_span = expr->span;
			*expr = *left;
			left->span = old_span;
			left->expr_kind = EXPR_BINARY;
			left->binary_expr = (ExprBinary){
					.operator = BINARYOP_ADD,
					.left = expr->pointer_offset_expr.offset,
					.right = exprid(right)
			};
			left->resolve_status = RESOLVE_NOT_DONE;
			if (!sema_analyse_expr(context, left)) return false;
			expr->pointer_offset_expr.offset = exprid(left);
		}
		else
		{
			// Set the type and other properties.
			expr->type = left->type;
			expr->pointer_offset_expr.raw_offset = false;
			expr->pointer_offset_expr.ptr = exprid(left);
			expr->pointer_offset_expr.offset = exprid(right);
			expr->expr_kind = EXPR_POINTER_OFFSET;
		}

		expr->resolve_status = RESOLVE_NOT_DONE;
		if (!sema_analyse_expr(context, expr)) return false;

		if (cast_to_iptr)
		{
			expr->resolve_status = RESOLVE_DONE;
			return cast(expr, cast_to_iptr);
		}
		return true;
	}

	if (!sema_binary_promote_top_down(context, expr, left, right)) return false;

	left_type = type_no_optional(left->type)->canonical;
	right_type = type_no_optional(right->type)->canonical;

	assert(!cast_to_iptr);
	// 4. Do a binary arithmetic promotion
	if (!sema_binary_arithmetic_promotion(context,
	                                      left,
	                                      right,
	                                      left_type,
	                                      right_type,
	                                      expr,
	                                      "Cannot do the addition %s + %s."))
	{
		return false;
	}

	// 5. Handle the "both const" case. We should only see ints and floats at this point.
	if (expr_both_const(left, right))
	{
		expr_replace(expr, left);
		switch (left->const_expr.const_kind)
		{
			case CONST_INTEGER:
				expr->const_expr.ixx = int_add(left->const_expr.ixx, right->const_expr.ixx);
				break;
			case CONST_FLOAT:
				expr->const_expr.fxx = float_add(left->const_expr.fxx, right->const_expr.fxx);
				break;
			default:
				UNREACHABLE
		}
	}

	// 6. Set the type & other properties.
	expr_binary_unify_failability(expr, left, right);

	return true;

}

/**
 * Analyse a * b
 *
 * Will analyse a and b and then use arithmetic promotion on each.
 * It will then try to promote both to a common type,
 *
 * @return true if analysis worked.
 */
static bool sema_expr_analyse_mult(SemaContext *context, Expr *expr, Expr *left, Expr *right)
{

	// 1. Analyse the sub expressions and promote to a common type
	if (!sema_binary_analyse_arithmetic_subexpr(context, expr, "It is not possible to multiply %s by %s.", false)) return false;


	// 2. Handle constant folding.
	if (expr_both_const(left, right))
	{
		expr_replace(expr, left);
		switch (left->const_expr.const_kind)
		{
			case CONST_INTEGER:
				expr->const_expr.ixx = int_mul(left->const_expr.ixx, right->const_expr.ixx);
				break;
			case CONST_FLOAT:
				expr->const_expr.fxx = float_mul(left->const_expr.fxx, right->const_expr.fxx);
				break;
			default:
				UNREACHABLE
		}
	}

	// 3. Set the new type
	expr->type = type_add_optional(left->type, IS_OPTIONAL(right));

	return true;
}

/**
 * Analyse a / b
 * @return true if analysis completed ok.
 */
static bool sema_expr_analyse_div(SemaContext *context, Expr *expr, Expr *left, Expr *right)
{
	// 1. Analyse sub expressions and promote to a common type
	if (!sema_binary_analyse_arithmetic_subexpr(context, expr, "Cannot divide %s by %s.", false)) return false;

	// 2. Check for a constant 0 on the rhs.
	if (IS_CONST(right))
	{
		switch (right->const_expr.const_kind)
		{
			case CONST_INTEGER:
				if (int_is_zero(right->const_expr.ixx))
				{
					SEMA_ERROR(right, "This expression evaluates to zero and division by zero is not allowed.");
					return false;
				}
				break;
			case CONST_FLOAT:
				// This is allowed, as it will generate a NaN
				break;
			default:
				UNREACHABLE
		}
	}

	// 3. Perform constant folding.
	if (expr_both_const(left, right))
	{
		expr_replace(expr, left);
		switch (left->const_expr.const_kind)
		{
			case CONST_INTEGER:
				expr->const_expr.ixx = int_div(left->const_expr.ixx, right->const_expr.ixx);
				break;
			case CONST_FLOAT:
				expr->const_expr.fxx = float_div(left->const_expr.fxx, right->const_expr.fxx);
				break;
			default:
				UNREACHABLE
		}
	}

	// 4. Done.
	expr->type = type_add_optional(left->type, IS_OPTIONAL(right));
	return true;

}

/**
 * Analyse a % b
 * @return true if analysis succeeds.
 */
static bool sema_expr_analyse_mod(SemaContext *context, Expr *expr, Expr *left, Expr *right)
{
	// 1. Analyse both sides and promote to a common type
	if (!sema_binary_analyse_arithmetic_subexpr(context, expr, NULL, false)) return false;

	// 3. a % 0 is not valid, so detect it.
	if (IS_CONST(right) && int_is_zero(right->const_expr.ixx))
	{
		SEMA_ERROR(right, "Cannot perform %% with a constant zero.");
		return false;
	}

	// 4. Constant fold
	if (expr_both_const(left, right))
	{
		expr_replace(expr, left);
		// 4a. Remember this is remainder.
		expr->const_expr.ixx = int_rem(left->const_expr.ixx, right->const_expr.ixx);
	}

	expr->type = type_add_optional(left->type, IS_OPTIONAL(right));
	return true;
}

/**
 * Analyse a ^ b, a | b, a & b
 * @return true if the analysis succeeded.
 */
static bool sema_expr_analyse_bit(SemaContext *context, Expr *expr, Expr *left, Expr *right)
{

	// 1. Convert to common type if possible.
	if (!sema_binary_analyse_arithmetic_subexpr(context, expr, NULL, true)) return false;

	// 2. Check that both are integers or bools.
	bool is_bool = left->type->canonical == type_bool;
	if (!is_bool && !expr_both_any_integer_or_integer_vector(left, right))
	{
		return sema_type_error_on_binop(expr);
	}

	// 3. Do constant folding if both sides are constant.
	if (expr_both_const(left, right))
	{
		BinaryOp op = expr->binary_expr.operator;
		expr_replace(expr, left);
		if (is_bool)
		{
			switch (op)
			{
				case BINARYOP_BIT_AND:
					expr->const_expr.b = left->const_expr.b & right->const_expr.b;
					break;
				case BINARYOP_BIT_XOR:
					expr->const_expr.b = left->const_expr.b ^ right->const_expr.b;
					break;
				case BINARYOP_BIT_OR:
					expr->const_expr.b = left->const_expr.b | right->const_expr.b;
					break;
				default:
					UNREACHABLE;
			}
		}
		else
		{
			switch (op)
			{
				case BINARYOP_BIT_AND:
					expr->const_expr.ixx = int_and(left->const_expr.ixx, right->const_expr.ixx);
					break;
				case BINARYOP_BIT_XOR:
					expr->const_expr.ixx = int_xor(left->const_expr.ixx, right->const_expr.ixx);
					break;
				case BINARYOP_BIT_OR:
					expr->const_expr.ixx = int_or(left->const_expr.ixx, right->const_expr.ixx);
					break;
				default:
					UNREACHABLE;
			}
		}
	}

	// 5. Assign the type
	expr_binary_unify_failability(expr, left, right);
	return true;
}

/**
 * Analyse >> and << operations.
 * @return true if the analysis succeeded.
 */
static bool sema_expr_analyse_shift(SemaContext *context, Expr *expr, Expr *left, Expr *right)
{
	// 1. Analyze both sides.
	if (!sema_binary_analyse_subexpr(context, expr, left, right)) return false;

	// 2. Only integers or integer vectors may be shifted.
	if (!expr_both_any_integer_or_integer_vector(left, right))
	{
		return sema_type_error_on_binop(expr);
	}

	if (expr->binary_expr.widen && !cast_widen_top_down(context, left, expr->type)) return false;

	// 3. Promote lhs using the usual numeric promotion.
	if (!cast_implicit(context, left, cast_numeric_arithmetic_promotion(type_no_optional(left->type)))) return false;

	// 4. For a constant rhs side we will make a series of checks.
	if (IS_CONST(right))
	{
		// 4a. Make sure the value does not exceed the bitsize of
		//     the left hand side. We ignore this check for lhs being a constant.
		Type *left_type_no_fail = type_no_optional(left->type)->canonical;
		assert(type_kind_is_any_integer(left_type_no_fail->type_kind));
		if (int_ucomp(right->const_expr.ixx, left_type_no_fail->builtin.bitsize, BINARYOP_GT))
		{
			SEMA_ERROR(right, "The shift exceeds bitsize of %s.", type_quoted_error_string(type_no_optional(left->type)));
			return false;
		}

		// 4b. Make sure that the RHS is positive.
		if (int_is_neg(right->const_expr.ixx))
		{
			SEMA_ERROR(right, "A shift must be a positive number.");
			return false;
		}

		// 5. Fold constant expressions.
		if (IS_CONST(left))
		{
			bool shr = expr->binary_expr.operator == BINARYOP_SHR;
			expr_replace(expr, left);
			if (shr)
			{
				expr->const_expr.ixx = int_shr64(left->const_expr.ixx, right->const_expr.ixx.i.low);
			}
			else
			{
				expr->const_expr.ixx = int_shl64(left->const_expr.ixx, right->const_expr.ixx.i.low);
			}
			return true;
		}
	}

	// 6. Set the type
	expr->type = type_add_optional(left->type, IS_OPTIONAL(right));
	return true;
}

/**
 * Analyse a <<= b a >>= b
 * @return true is the analysis succeeds, false otherwise.
 */
static bool sema_expr_analyse_shift_assign(SemaContext *context, Expr *expr, Expr *left, Expr *right)
{
	if (left->expr_kind == EXPR_CT_IDENT)
	{
		return sema_binary_analyse_ct_common_assign(context, expr, left);
	}
	// 1. Analyze the two sub lhs & rhs *without coercion*
	if (!sema_binary_analyse_subexpr(context, expr, left, right)) return false;

	bool failable = IS_OPTIONAL(left) || IS_OPTIONAL(right);

	// 2. Ensure the lhs side is assignable
	if (!sema_expr_check_assign(context, left)) return false;

	// 3. Only integers may be shifted.
	if (!expr_both_any_integer_or_integer_vector(left, right)) return sema_type_error_on_binop(expr);

	// 4. For a constant right hand side we will make a series of checks.
	if (IS_CONST(right))
	{
		// 4a. Make sure the value does not exceed the bitsize of
		//     the left hand side.
		if (int_ucomp(right->const_expr.ixx, left->type->canonical->builtin.bitsize, BINARYOP_GT))
		{
			SEMA_ERROR(right, "The shift exceeds bitsize of '%s'.", type_to_error_string(left->type));
			return false;
		}

		// 4b. Make sure that the right hand side is positive.
		if (int_is_neg(right->const_expr.ixx))
		{
			SEMA_ERROR(right, "A shift must be a positive number.");
			return false;
		}
	}

	// 5. Set the type using the lhs side.

	if (left->expr_kind == EXPR_BITACCESS)
	{
		expr->expr_kind = EXPR_BITASSIGN;
	}
	expr->type = type_add_optional(left->type, failable);
	return true;
}


static bool sema_expr_analyse_and_or(SemaContext *context, Expr *expr, Expr *left, Expr *right)
{
	if (!sema_binary_analyse_subexpr(context, expr, left, right)) return false;
	if (!cast_implicit(context, left, type_bool) || !cast_implicit(context, right, type_bool)) return false;

	if (expr_both_const(left, right))
	{
		if (expr->binary_expr.operator == BINARYOP_AND)
		{
			expr_replace(expr, left);
			expr->const_expr.b &= right->const_expr.b;
		}
		else
		{
			expr_replace(expr, left);
			expr->const_expr.b |= right->const_expr.b;
		}
	}
	expr->type = type_bool;
	return true;
}




static bool sema_binary_is_unsigned_always_false_comparison(SemaContext *context, Expr *expr, Expr *left, Expr *right)
{
	if (context->active_scope.flags & SCOPE_MACRO) return true;
	if (!expr_is_const(left) && !expr_is_const(right)) return true;
	if (!type_is_integer(left->type)) return true;
	if (expr_is_const(left) && type_is_unsigned(type_flatten_distinct(right->type)))
	{
		if (int_is_neg(left->const_expr.ixx))
		{
			SEMA_ERROR(left, "Comparing an unsigned value with a negative constant is only allowed inside of macros.");
			return false;
		}
		if (!int_is_zero(left->const_expr.ixx)) return true;
		switch (expr->binary_expr.operator)
		{
			case BINARYOP_GT:
				SEMA_ERROR(left, "Comparing '0 > unsigned expression' can never be true, and is only allowed inside of macro expansions.");
				return false;
			case BINARYOP_GE:
				SEMA_ERROR(left,
				           "Comparing '0 >= unsigned expression' is the same as 0 == expr and is a common bug, "
						   "so is only allowed inside of macro expansions.");
				return false;
			default:
				return true;
		}
	}
	if (!expr_is_const(right) || !type_is_unsigned(type_flatten_distinct(left->type))) return true;
	if (int_is_neg(right->const_expr.ixx))
	{
		SEMA_ERROR(right, "Comparing an unsigned value with a negative constant is only allowed inside of macros.");
		return false;
	}
	if (!int_is_zero(right->const_expr.ixx)) return true;
	switch (expr->binary_expr.operator)
	{
		case BINARYOP_LT:
			SEMA_ERROR(right,
			           "Comparing 'unsigned expression < 0' can never be true, and is only allowed inside of macro expansions.");
			return false;
		case BINARYOP_LE:
			SEMA_ERROR(right,
					   "Comparing 'unsigned expression <= 0' is the same as expr == 0 and is a common bug, "
			           "so is only allowed inside of macro expansions.");
			return false;
		default:
			return true;
	}

}

/**
 * Analyze a == b, a != b, a > b, a < b, a >= b, a <= b
 * @return
 */
static bool sema_expr_analyse_comp(SemaContext *context, Expr *expr, Expr *left, Expr *right)
{
	// 1. Analyse left and right side without any conversions.
	if (!sema_binary_analyse_subexpr(context, expr, left, right)) return false;

	bool is_equality_type_op = expr->binary_expr.operator == BINARYOP_NE || expr->binary_expr.operator == BINARYOP_EQ;

	// Flatten enum/distinct/failable
	Type *left_type = type_flatten(left->type);
	Type *right_type = type_flatten(right->type);

	// 2. Handle the case of signed comparisons.
	//    This happens when either side has a definite integer type
	//    and those are either signed or unsigned.
	//    If either side is compint, then this does not happen.
	if ((type_is_unsigned(left_type) && type_is_signed(right_type))
		|| (type_is_signed(left_type) && type_is_unsigned(right_type)))
	{
		// 2a. Resize so that both sides have the same bit width. This will always work.
		cast_to_max_bit_size(context, left, right, left_type, right_type);
		goto DONE;
	}

	if (left_type->type_kind == TYPE_VECTOR && right_type->type_kind == TYPE_VECTOR)
	{
		if (left_type->array.len == right_type->array.len)
		{
			Type *left_vec = type_vector_type(left_type);
			Type *right_vec = type_vector_type(right_type);
			if (left_vec == right_vec) goto DONE;
			if (type_size(left_vec) != type_size(right_vec)) goto DONE;
			if (type_is_integer(left_vec) && type_is_integer(right_vec)) goto DONE;
		}
		SEMA_ERROR(expr, "Vector types %s and %s cannot be compared.",
		           type_quoted_error_string(left->type), type_quoted_error_string(right->type));
		return false;
	}

	// 3. In the normal case, treat this as a binary op, finding the max type.
	Type *max = type_find_max_type(type_no_optional(left->type)->canonical, type_no_optional(right->type)->canonical);

	// 4. If no common type, then that's an error:
	if (!max)
	{
		SEMA_ERROR(expr, "%s and %s are different types and cannot be compared.",
		           type_quoted_error_string(left->type), type_quoted_error_string(right->type));
		return false;
	}

	if (!type_is_comparable(max))
	{
		SEMA_ERROR(expr, "%s does not support comparisons, you need to manually implement a comparison if you need it.",
		           type_quoted_error_string(left->type));
		return false;
	}
	if (!is_equality_type_op)
	{
		if (!type_is_ordered(max))
		{
			SEMA_ERROR(expr, "%s can only be compared using '!=' and '==' it cannot be ordered, did you make a mistake?",
			           type_quoted_error_string(left->type));
			return false;
		}
		if (type_flatten(max)->type_kind == TYPE_POINTER)
		{

			// Only comparisons between the same type is allowed. Subtypes not allowed.
			if (left_type != right_type && left_type != type_voidptr && right_type != type_voidptr)
			{
				SEMA_ERROR(expr, "You are not allowed to compare pointers of different types, "
								 "if you need to do, first convert all pointers to void*.");
				return false;
			}
		}
	}

	// 6. Do the implicit cast.
	bool success = cast_implicit(context, left, max) && cast_implicit(context, right, max);
	assert(success);
DONE:

	// 7. Do constant folding.
	if (expr_both_const(left, right))
	{
		expr->const_expr.b = expr_const_compare(&left->const_expr, &right->const_expr, expr->binary_expr.operator);
		expr->const_expr.const_kind = CONST_BOOL;
		expr->expr_kind = EXPR_CONST;
	}
	else
	{
		if (!sema_binary_is_unsigned_always_false_comparison(context, expr, left, right)) return false;
	}

	// 8. Set the type to bool

	Type *return_type = left_type->type_kind == TYPE_VECTOR ? type_get_vector_bool(left_type) : type_bool;
	expr->type = type_add_optional(return_type, IS_OPTIONAL(left) || IS_OPTIONAL(right));

	return true;
}

/**
 * Analyse *a
 * @return true if analysis succeeds.
 */
static inline bool sema_expr_analyse_deref(SemaContext *context, Expr *expr)
{
	// 1. Check the inner expression
	Expr *inner = expr->unary_expr.expr;
	if (!sema_analyse_expr(context, inner)) return false;

	Type *inner_type_nofail = type_no_optional(inner->type);
	Type *canonical = inner_type_nofail->canonical;

	// 2. Check that we have a pointer, or dereference is not allowed.
	if (canonical->type_kind != TYPE_POINTER)
	{
		SEMA_ERROR(inner, "Cannot dereference a value of type %s, it must be a pointer.", type_quoted_error_string(inner_type_nofail));
		return false;
	}
	if (canonical->pointer == type_void)
	{
		SEMA_ERROR(inner, "A 'void*' cannot be dereferenced, you need to first cast it to a concrete type.");
		return false;
	}

	// 3. This could be a constant, in which case it is a null which is an error.
	if (inner->expr_kind == EXPR_CONST)
	{
		SEMA_ERROR(inner, "Dereferencing null is not allowed, did you do it by mistake?");
		return false;
	}

	// 4. Now the type might not be a pointer because of a typedef,
	//    otherwise we need to use the canonical representation.
	Type *deref_type = inner_type_nofail->type_kind == TYPE_POINTER ? inner_type_nofail : canonical;

	// 5. And... set the type.
	expr->type = type_add_optional(deref_type->pointer, IS_OPTIONAL(inner));

	return true;
}

static inline bool sema_addr_may_take_of_var(Expr *expr, Decl *decl)
{
	if (decl->decl_kind != DECL_VAR) return false;
	decl->var.is_addr = true;
	bool is_void = type_flatten(decl->type) == type_void;
	switch (decl->var.kind)
	{
		case VARDECL_GLOBAL:
			if (is_void)
			{
				SEMA_ERROR(expr, "You cannot take the address of a global of type %s.", type_quoted_error_string(decl->type));
				return false;
			}
			return true;
		case VARDECL_LOCAL:
			if (is_void)
			{
				SEMA_ERROR(expr, "You cannot take the address of a variable with type %s.", type_quoted_error_string(decl->type));
				return false;
			}
			return true;
		case VARDECL_PARAM:
		case VARDECL_PARAM_REF:
			if (is_void)
			{
				SEMA_ERROR(expr, "You cannot take the address of a parameter with type %s.", type_quoted_error_string(decl->type));
				return false;
			}
			return true;
		case VARDECL_CONST:
			if (!decl->var.type_info)
			{
				SEMA_ERROR(expr, "The constant is not typed, either type it or use && to take the reference to a temporary.");
				SEMA_NOTE(decl, "The constant was defined here.");
				return false;
			}
			assert(decl->type != type_void);
			return true;
		case VARDECL_PARAM_EXPR:
			SEMA_ERROR(expr, "It is not possible to take the address of a captured expression, but you can use && to take a reference to the temporary value.");
			return false;
		case VARDECL_PARAM_CT:
		case VARDECL_PARAM_CT_TYPE:
		case VARDECL_LOCAL_CT_TYPE:
		case VARDECL_LOCAL_CT:
			// May not be reached due to EXPR_CT_IDENT being handled elsewhere.
			UNREACHABLE;
		case VARDECL_MEMBER:
		case VARDECL_BITMEMBER:
		case VARDECL_UNWRAPPED:
		case VARDECL_REWRAPPED:
		case VARDECL_ERASE:
			UNREACHABLE
	}
	UNREACHABLE

}

static inline bool sema_addr_may_take_of_ident(Expr *inner)
{
	Decl *decl = decl_raw(inner->identifier_expr.decl);
	switch (decl->decl_kind)
	{
		case DECL_FUNC:
			if (decl->func_decl.attr_test)
			{
				SEMA_ERROR(inner, "You may not take the address of a '@test' function.");
				return false;
			}
			return true;
		case DECL_VAR:
			return sema_addr_may_take_of_var(inner, decl);
		case DECL_MACRO:
			SEMA_ERROR(inner, "It is not possible to take the address of a macro.");
			return false;
		case DECL_GENERIC:
			SEMA_ERROR(inner, "It is not possible to take the address of a generic function.");
			return false;
		default:
			UNREACHABLE
	}
	UNREACHABLE
}

static bool sema_addr_check_may_take(Expr *inner)
{
	switch (inner->expr_kind)
	{
		case EXPR_CT_IDENT:
			SEMA_ERROR(inner, "It's not possible to take the address of a compile time value.");
			return false;
		case EXPR_IDENTIFIER:
			return sema_addr_may_take_of_ident(inner);
		case EXPR_UNARY:
			if (inner->unary_expr.operator == UNARYOP_DEREF) return true;
			break;
		case EXPR_ACCESS:
			return sema_addr_check_may_take(inner->access_expr.parent);
		case EXPR_GROUP:
			return sema_addr_check_may_take(inner->inner_expr);
		case EXPR_SUBSCRIPT:
			return sema_addr_check_may_take(exprptr(inner->subscript_expr.expr));
		case EXPR_TYPEINFO:
			SEMA_ERROR(inner, "It is not possible to take the address of a type.");
			return false;
		case EXPR_BITACCESS:
			SEMA_ERROR(inner, "You cannot take the address of a bitstruct member.");
			return false;
		default:
			break;
	}
	SEMA_ERROR(inner, "To take the address of a temporary value, use '&&' instead of '&'.", type_to_error_string(inner->type));
	return false;
}

/**
 * Analyse &a
 * @return true if analysis succeeds.
 */
static inline bool sema_expr_analyse_addr(SemaContext *context, Expr *expr)
{
	// 1. Evaluate the expression
	Expr *inner = expr->unary_expr.expr;
	REDO:
	switch (inner->expr_kind)
	{
		case EXPR_POISONED:
			return false;
		case EXPR_GROUP:
			// We want to collapse any grouping here.
			expr_replace(inner, inner->inner_expr);
			goto REDO;
		case EXPR_SUBSCRIPT:
			inner->expr_kind = EXPR_SUBSCRIPT_ADDR;
			if (!sema_analyse_expr_lvalue_fold_const(context, inner)) return false;
			expr_replace(expr, inner);
			return true;
		default:
		{
			if (!sema_analyse_expr_lvalue(context, inner)) return expr_poison(expr);
		}
	}

	// 2. Take the address.
	if (!sema_addr_check_may_take(inner)) return expr_poison(expr);

	// 3. Get the pointer of the underlying type.
	expr->type = type_get_ptr_recurse(inner->type);

	return true;
}

/**
 * Test -a
 */
static inline bool sema_expr_analyse_neg(SemaContext *context, Expr *expr)
{
	// 1. Check the inner expression
	Expr *inner = expr->unary_expr.expr;
	if (!sema_analyse_expr(context, inner)) return false;
	if (expr->unary_expr.widen && !cast_widen_top_down(context, inner, expr->type)) return false;

	// 2. Check if it's possible to negate this (i.e. is it an int, float or vector)
	Type *no_fail = type_no_optional(inner->type);
	if (!type_may_negate(no_fail))
	{
		SEMA_ERROR(expr, "Cannot negate an expression of type %s.", type_quoted_error_string(no_fail));
		return false;
	}
	// 3. Promote the type
	Type *result_type = cast_numeric_arithmetic_promotion(no_fail);
	if (!cast_implicit(context, inner, result_type)) return false;

	// 4. If it's non-const, we're done.
	if (inner->expr_kind != EXPR_CONST)
	{
		expr->type = inner->type;
		return true;
	}

	// 5. Otherwise constant fold.
	expr_replace(expr, inner);
	switch (expr->const_expr.const_kind)
	{
		case CONST_INTEGER:
			expr->const_expr.ixx = int_neg(inner->const_expr.ixx);
			return true;
		case CONST_FLOAT:
			expr->const_expr.fxx = float_neg(expr->const_expr.fxx);
			break;
		default:
			UNREACHABLE
	}
	return true;
}

/**
 * Analyse ~x and ~123
 *
 * @return
 */
static inline bool sema_expr_analyse_bit_not(SemaContext *context, Expr *expr)
{
	// 1. Analyse the inner expression.
	Expr *inner = expr->unary_expr.expr;
	if (!sema_analyse_expr(context, inner)) return false;

	if (expr->unary_expr.widen && !cast_widen_top_down(context, inner, expr->type)) return false;

	// 2. Check that it's a vector, bool
	Type *canonical = type_no_optional(inner->type)->canonical;
	if (!type_is_integer_or_bool_kind(type_flatten_distinct(canonical)))
	{
		Type *vector_type = type_vector_type(canonical);
		if (vector_type && (type_is_integer(vector_type) || vector_type == type_bool)) goto VALID_VEC;
		SEMA_ERROR(expr, "Cannot bit negate '%s'.", type_to_error_string(inner->type));
		return false;
	}

VALID_VEC:
	// 3. The simple case, non-const.
	if (inner->expr_kind != EXPR_CONST)
	{
		expr->type = inner->type;
		return true;
	}

	// 4. Otherwise handle const bool
	expr_replace(expr, inner);
	if (expr->const_expr.const_kind == CONST_BOOL)
	{
		expr->const_expr.b = !expr->const_expr.b;
		return true;
	}

	// 5. Perform ~ constant folded
	// TODO arithmetic promotion?
	expr->const_expr.ixx = int_not(inner->const_expr.ixx);
	return true;
}

/**
 * Evaluate !a
 */
static inline bool sema_expr_analyse_not(SemaContext *context, Expr *expr)
{
	// 1. Evaluate inner
	Expr *inner = expr->unary_expr.expr;
	if (!sema_analyse_expr(context, inner)) return false;

	// 2. Check whether the type is a vector
	Type *type = type_no_optional(inner->type);
	if (type_flat_is_vector(type))
	{
		// 3. This always works, so we're done.
		expr->type = type_add_optional(type_get_vector_bool(type), IS_OPTIONAL(inner));
		return true;
	}

	// 4. Let's see if it's possible to cast it implicitly
	if (!cast_may_implicit(type, type_bool, CAST_OPTION_SIMPLE_EXPR | CAST_OPTION_ALLOW_OPTIONAL))
	{
		SEMA_ERROR(expr, "The use of '!' on %s is not allowed as it can't be converted to a boolean value.", type_quoted_error_string(inner->type));
		return false;
	}

	expr->type = type_add_optional(type_bool, IS_OPTIONAL(inner));

	if (inner->expr_kind == EXPR_CONST)
	{
		bool success = cast_implicit(context, inner, expr->type);
		assert(success);
		assert(inner->const_expr.const_kind == CONST_BOOL);
		expr->const_expr.const_kind = CONST_BOOL;
		expr->expr_kind = EXPR_CONST;
		expr->const_expr.b = !inner->const_expr.b;
	}
	return true;
}

static inline bool sema_expr_analyse_ct_incdec(SemaContext *context, Expr *expr, Expr *inner)
{
	assert(inner->expr_kind == EXPR_CT_IDENT);

	if (!sema_binary_analyse_ct_identifier_lvalue(context, inner)) return false;

	Decl *var = inner->ct_ident_expr.decl;
	Expr *start_value = var->var.init_expr;
	assert(start_value->expr_kind == EXPR_CONST);

	switch (start_value->const_expr.const_kind)
	{
		case CONST_INTEGER:
			break;
		default:
			SEMA_ERROR(expr, "The compile time variable '%s' does not hold an integer.", var->name);
			return false;
	}

	if (var->var.scope_depth < context->active_scope.depth)
	{
		SEMA_ERROR(expr, "Cannot modify '%s' inside of a runtime scope.", var->name);
		return false;
	}

	Expr *end_value = expr_copy(start_value);

	// Make the change.
	if (expr->unary_expr.operator == UNARYOP_DEC)
	{
		end_value->const_expr.ixx = int_sub64(start_value->const_expr.ixx, 1);
	}
	else
	{
		end_value->const_expr.ixx = int_add64(start_value->const_expr.ixx, 1);
	}
	var->var.init_expr = end_value;
	if (expr->expr_kind == EXPR_POST_UNARY)
	{
		expr_replace(expr, start_value);
	}
	else
	{
		expr_replace(expr, end_value);
	}
	return true;
}

/**
 * Analyse foo++ foo-- --foo ++foo
 * @return false if analysis fails.
 */
static inline bool sema_expr_analyse_incdec(SemaContext *context, Expr *expr)
{
	// 1. Analyse the lvalue to update
	Expr *inner = expr->unary_expr.expr;
	if (!sema_analyse_expr_lvalue(context, inner)) return false;

	// 2. Assert it's an l-value
	if (!sema_expr_check_assign(context, inner)) return false;

	// 3. This might be a $foo, if to handle it.
	if (inner->expr_kind == EXPR_CT_IDENT)
	{
		return sema_expr_analyse_ct_incdec(context, expr, inner);
	}

	// 4. Flatten typedef, enum, distinct, failable
	Type *type = type_flatten(inner->type);

	// 5. We can only inc/dec numbers or pointers.
	if (!type_underlying_is_numeric(type) && type->type_kind != TYPE_POINTER)
	{
		SEMA_ERROR(inner, "The expression must be a number or a pointer.");
		return false;
	}

	if (!cast_decay_array_pointers(context, inner)) return false;

	// 6. Done, the result is same as the inner type.
	expr->type = inner->type;
	return true;
}

/**
 * Take an address of a temporary &&x.
 */
static inline bool sema_expr_analyse_taddr(SemaContext *context, Expr *expr)
{
	Expr *inner = expr->unary_expr.expr;
	if (!sema_analyse_expr(context, inner)) return false;

	if (type_is_invalid_storage_type(inner->type))
	{
		SEMA_ERROR(expr, "It is not possible to take the address from a value of the type %s.",
		           type_quoted_error_string(inner->type));
		return false;
	}
	// 2. The type is the resulting type of the expression.
	expr->type = type_get_ptr_recurse(inner->type);
	return true;
}

static bool sema_binary_check_unclear_op_precedence(Expr *left_side, Expr * main_expr, Expr *right_side)
{
	static int BINOP_PREC_REQ[BINARYOP_LAST + 1] = {
			// bitwise operations
			[BINARYOP_BIT_OR] = 1,
			[BINARYOP_BIT_XOR] = 1,
			[BINARYOP_BIT_AND] = 1,

			// comparison operations
			[BINARYOP_GT] = 2,
			[BINARYOP_GE] = 2,
			[BINARYOP_LT] = 2,
			[BINARYOP_LE] = 2,
			[BINARYOP_NE] = 2,
			[BINARYOP_EQ] = 2,

			[BINARYOP_SHR] = 3,
			[BINARYOP_SHL] = 3,
	};

	int precedence_main = BINOP_PREC_REQ[main_expr->binary_expr.operator];
	if (left_side->expr_kind == EXPR_BINARY)
	{
		int precedence_left = BINOP_PREC_REQ[left_side->binary_expr.operator];
		return precedence_left && (precedence_left == precedence_main);
	}
	if (right_side->expr_kind == EXPR_BINARY)
	{
		int precedence_right = BINOP_PREC_REQ[right_side->binary_expr.operator];
		return precedence_right && (precedence_right == precedence_main);
	}
	return false;
}


static inline bool sema_expr_analyse_or_error(SemaContext *context, Expr *expr)
{
	Expr *lhs = exprptr(expr->binary_expr.left);
	Expr *rhs = exprptr(expr->binary_expr.right);
	if (lhs->expr_kind == EXPR_TERNARY || rhs->expr_kind == EXPR_TERNARY)
	{
		SEMA_ERROR(expr, "Unclear precedence using ternary with ??, please use () to remove ambiguity.");
		return false;
	}
	if (!sema_analyse_expr(context, lhs)) return false;

	if (expr->binary_expr.widen && !cast_widen_top_down(context, lhs, expr->type)) return false;

	Type *type = lhs->type;
	if (!type_is_optional(type))
	{
		SEMA_ERROR(lhs, "No failable to use '\?\?' with, please remove the '\?\?'.");
		return false;
	}

	// First we analyse the "else" and try to implictly cast.
	if (!sema_analyse_expr(context, rhs)) return false;
	if (expr->binary_expr.widen && !cast_widen_top_down(context, rhs, expr->type)) return false;

	if (lhs->expr_kind == EXPR_FAILABLE)
	{
		expr_replace(expr, rhs);
		return true;
	}

	// Here we might need to insert casts.
	Type *else_type = rhs->type;

	if (type_is_optional_any(type))
	{
		// One possibility is that both sides have the "optional any" type
		// if so then we're done.
		if (else_type == type)
		{
			expr->type = type;
			return true;
		}
		// Otherwise assign the type of "else":
		type = else_type;
	}
	else if (type_is_optional_any(else_type))
	{
		expr->type = type;
		return true;
	}
	// Remove any possible optional of the else type.
	bool add_optional = type_is_optional(else_type);
	type = type_no_optional(type);
	else_type = type_no_optional(else_type);
	Type *common = type_find_max_type(type, else_type);
	if (!common)
	{
		SEMA_ERROR(rhs, "Cannot find a common type for %s and %s.", type_quoted_error_string(type),
				   type_quoted_error_string(else_type));
		return false;
	}
	if (!cast_implicit(context, lhs, common)) return false;
	if (!cast_implicit(context, rhs, common)) return false;
	expr->type = type_add_optional(common, add_optional);
	return true;
}

static inline bool sema_expr_analyse_binary(SemaContext *context, Expr *expr)
{
	if (expr->binary_expr.operator == BINARYOP_ELSE) return sema_expr_analyse_or_error(context, expr);
	assert(expr->resolve_status == RESOLVE_RUNNING);
	Expr *left = exprptr(expr->binary_expr.left);
	Expr *right = exprptr(expr->binary_expr.right);
	// check if both sides have a binary operation where the precedence is unclear. Example: a ^ b | c
	if (sema_binary_check_unclear_op_precedence(left, expr, right))
	{
		SEMA_ERROR(expr, "You need to add explicit parentheses to clarify precedence.");
		return expr_poison(expr);
	}
	switch (expr->binary_expr.operator)
	{
		case BINARYOP_ELSE:
			UNREACHABLE // Handled previously
		case BINARYOP_ASSIGN:
			return sema_expr_analyse_assign(context, expr, left, right);
		case BINARYOP_MULT:
			return sema_expr_analyse_mult(context, expr, left, right);
		case BINARYOP_ADD:
			return sema_expr_analyse_add(context, expr, left, right);
		case BINARYOP_ADD_ASSIGN:
		case BINARYOP_SUB_ASSIGN:
			return sema_expr_analyse_add_sub_assign(context, expr, left, right);
		case BINARYOP_SUB:
			return sema_expr_analyse_sub(context, expr, left, right);
		case BINARYOP_DIV:
			return sema_expr_analyse_div(context, expr, left, right);
		case BINARYOP_MULT_ASSIGN:
		case BINARYOP_DIV_ASSIGN:
			return sema_expr_analyse_op_assign(context, expr, left, right, false);
		case BINARYOP_BIT_AND_ASSIGN:
		case BINARYOP_BIT_OR_ASSIGN:
		case BINARYOP_BIT_XOR_ASSIGN:
		case BINARYOP_MOD_ASSIGN:
			return sema_expr_analyse_op_assign(context, expr, left, right, true);
		case BINARYOP_MOD:
			return sema_expr_analyse_mod(context, expr, left, right);
		case BINARYOP_AND:
		case BINARYOP_OR:
			return sema_expr_analyse_and_or(context, expr, left, right);
		case BINARYOP_BIT_OR:
		case BINARYOP_BIT_XOR:
		case BINARYOP_BIT_AND:
			return sema_expr_analyse_bit(context, expr, left, right);
		case BINARYOP_NE:
		case BINARYOP_EQ:
		case BINARYOP_GT:
		case BINARYOP_GE:
		case BINARYOP_LT:
		case BINARYOP_LE:
			return sema_expr_analyse_comp(context, expr, left, right);
		case BINARYOP_SHR:
		case BINARYOP_SHL:
			return sema_expr_analyse_shift(context, expr, left, right);
		case BINARYOP_SHR_ASSIGN:
		case BINARYOP_SHL_ASSIGN:
			return sema_expr_analyse_shift_assign(context, expr, left, right);
		case BINARYOP_ERROR:
			break;
	}
	UNREACHABLE
}

/**
 * Analyse:
 *   *x
 *   &x
 *   x++/++x/x--/--x
 *   ~x
 *   !x
 *   &&x
 *   -x
 */
static inline bool sema_expr_analyse_unary(SemaContext *context, Expr *expr)
{
	assert(expr->resolve_status == RESOLVE_RUNNING);
	switch (expr->unary_expr.operator)
	{
		case UNARYOP_DEREF:
			return sema_expr_analyse_deref(context, expr);
		case UNARYOP_ADDR:
			return sema_expr_analyse_addr(context, expr);
		case UNARYOP_NEG:
			return sema_expr_analyse_neg(context, expr);
		case UNARYOP_BITNEG:
			return sema_expr_analyse_bit_not(context, expr);
		case UNARYOP_NOT:
			return sema_expr_analyse_not(context, expr);
		case UNARYOP_DEC:
		case UNARYOP_INC:
			return sema_expr_analyse_incdec(context, expr);
		case UNARYOP_TADDR:
			return sema_expr_analyse_taddr(context, expr);
		case UNARYOP_ERROR:
			return false;
	}
	UNREACHABLE
}


static inline bool sema_expr_analyse_try(SemaContext *context, Expr *expr)
{
	Expr *inner = expr->inner_expr;
	if (!sema_analyse_expr(context, inner)) return false;
	if (!IS_OPTIONAL(inner))
	{
		SEMA_ERROR(inner, "Expected a failable expression to 'try'.");
		return false;
	}
	expr->type = type_bool;
	return true;
}

static inline bool sema_expr_analyse_catch(SemaContext *context, Expr *expr)
{
	Expr *inner = expr->inner_expr;
	if (!sema_analyse_expr(context, inner)) return false;
	if (!IS_OPTIONAL(inner))
	{
		SEMA_ERROR(inner, "Expected a failable expression to 'catch'.");
		return false;
	}
	expr->type = type_anyerr;
	return true;
}

static inline bool sema_expr_analyse_rethrow(SemaContext *context, Expr *expr)
{
	if (context->call_env.kind != CALL_ENV_FUNCTION && context->call_env.kind != CALL_ENV_CHECKS)
	{
		SEMA_ERROR(expr, "Rethrow cannot be used outside of a function.");
		return false;
	}
	Expr *inner = expr->rethrow_expr.inner;
	if (!sema_analyse_expr(context, inner)) return false;

	if (context->active_scope.in_defer)
	{
		SEMA_ERROR(expr, "Returns are not allowed inside of defers.");
		return false;
	}
	expr->rethrow_expr.cleanup = context_get_defers(context, context->active_scope.defer_last, 0);
	if (inner->type == type_anyfail)
	{
		SEMA_ERROR(expr, "This expression will always throw, which isn't allowed.");
		return false;
	}
	expr->type = type_no_optional(inner->type);

	if (!IS_OPTIONAL(inner))
	{
		SEMA_ERROR(expr, "No failable to rethrow before '?' in the expression, please remove '?'.");
		return false;
	}


	if (context->active_scope.flags & (SCOPE_EXPR_BLOCK | SCOPE_MACRO))
	{
		vec_add(context->returns, NULL);
	}
	else
	{
		if (context->rtype && context->rtype->type_kind != TYPE_OPTIONAL)
		{
			SEMA_ERROR(expr, "This expression implicitly returns with a failable result, but the function does not allow failable results. Did you mean to use 'else' instead?");
			return false;
		}
	}

	return true;
}


static inline bool sema_expr_analyse_force_unwrap(SemaContext *context, Expr *expr)
{
	Expr *inner = expr->inner_expr;
	if (!sema_analyse_expr(context, inner)) return false;
	if (inner->type == type_anyfail)
	{
		SEMA_ERROR(expr, "This expression will always throw, which isn't allowed.");
		return false;
	}
	expr->type = type_no_optional(inner->type);
	if (!IS_OPTIONAL(inner))
	{
		SEMA_ERROR(expr, "No failable to rethrow before '!!' in the expression, please remove '!!'.");
		return false;
	}
	return true;
}

static inline bool sema_expr_analyse_typeid(SemaContext *context, Expr *expr)
{
	if (!sema_resolve_type_info(context, expr->typeid_expr)) return expr_poison(expr);
	Type *type = expr->type_expr->type;
	expr->expr_kind = EXPR_CONST;
	expr->const_expr.const_kind = CONST_TYPEID;
	expr->const_expr.typeid = type->canonical;
	expr->type = type_typeid;
	return true;
}

static inline bool sema_expr_analyse_expr_block(SemaContext *context, Type *infer_type, Expr *expr)
{
	bool success = true;
	expr->type = type_void;
	Ast **saved_returns = context_push_returns(context);
	Type *stored_block_type = context->expected_block_type;
	context->expected_block_type = infer_type;
	BlockExit **ref = MALLOCS(BlockExit*);
	BlockExit **stored_block_exit = context->block_exit_ref;
	context->block_exit_ref = ref;
	expr->expr_block.block_exit_ref = ref;
	SCOPE_START_WITH_FLAGS(SCOPE_EXPR_BLOCK)

		context->block_return_defer = context->active_scope.defer_last;
		PUSH_CONTINUE(NULL);
		PUSH_BREAK(NULL);
		PUSH_NEXT(NULL, NULL);

		AstId current = expr->expr_block.first_stmt;
		Ast *stmt = NULL;
		while (current)
		{
			stmt = ast_next(&current);
			if (!sema_analyse_statement(context, stmt))
			{
				success = false;
				goto EXIT;
			}
		}

		if (!vec_size(context->returns))
		{
			expr->type = type_void;
			goto EXIT;
		}

		// Let's unify the return statements.
		Type *sum_returns = context_unify_returns(context);
		if (!sum_returns)
		{
			success = false;
			goto EXIT;
		}
		if (type_no_optional(sum_returns) != type_void && !context->active_scope.jump_end)
		{
			Ast *ast = ast_last(astptr(expr->expr_block.first_stmt));
			SEMA_ERROR(ast, "Expected a return statement following this statement.");
			success = false;
			goto EXIT;
		}
		expr->type = sum_returns;

	EXIT:
		POP_BREAKCONT();
		POP_NEXT();

		context_pop_defers(context, &stmt->next);
	SCOPE_END;
	context->expected_block_type = stored_block_type;
	context->block_exit_ref = stored_block_exit;
	context_pop_returns(context, saved_returns);

	return success;
}



static inline bool sema_expr_analyse_failable(SemaContext *context, Expr *expr)
{
	Expr *inner = expr->inner_expr;
	if (!sema_analyse_expr(context, inner)) return false;

	if (IS_OPTIONAL(inner))
	{
		SEMA_ERROR(inner, "The inner expression is already a failable.");
		return false;
	}

	if (inner->expr_kind == EXPR_FAILABLE)
	{
		SEMA_ERROR(inner, "It looks like you added one too many '!' after the error.");
		return false;
	}

	Type *type = inner->type->canonical;
	if (type->type_kind != TYPE_FAULTTYPE && type->type_kind != TYPE_ANYERR)
	{
		SEMA_ERROR(inner, "You cannot use the '!' operator on expressions of type %s", type_quoted_error_string(type));
		return false;
	}
	expr->type = type_anyfail;
	return true;
}

static inline bool sema_expr_analyse_compiler_const(SemaContext *context, Expr *expr, bool report_missing)
{
	const char *string = expr->builtin_expr.ident;
	BuiltinDefine def = BUILTIN_DEF_NONE;
	for (unsigned i = 0; i < NUMBER_OF_BUILTIN_DEFINES; i++)
	{
		if (string == builtin_defines[i])
		{
			def = (BuiltinDefine)i;
			break;
		}
	}
	switch (def)
	{
		case BUILTIN_DEF_TIME:
			expr_rewrite_to_string(expr, time_get());
			return true;
		case BUILTIN_DEF_DATE:
			expr_rewrite_to_string(expr, date_get());
			return true;
		case BUILTIN_DEF_FILE:
			expr_rewrite_to_string(expr, context->compilation_unit->file->name);
			return true;
		case BUILTIN_DEF_MODULE:
			expr_rewrite_to_string(expr, context->compilation_unit->module->name->module);
			return true;
		case BUILTIN_DEF_LINE:
			if (context->original_inline_line)
			{
				expr_rewrite_const_int(expr, type_isize, context->original_inline_line, true);
			}
			else
			{
				expr_rewrite_const_int(expr, type_isize, expr->span.row, true);
			}
			return true;
		case BUILTIN_DEF_LINE_RAW:
			expr_rewrite_const_int(expr, type_isize, expr->span.row, true);
			return true;
		case BUILTIN_DEF_FUNCTION:
			switch (context->call_env.kind)
			{
				case CALL_ENV_GLOBAL_INIT:
				case CALL_ENV_CHECKS:
				case CALL_ENV_INITIALIZER:
				case CALL_ENV_FINALIZER:
				case CALL_ENV_ATTR:
					if (report_missing)
					{
						SEMA_ERROR(expr, "$$FUNCEXPR is not defined outside of a function.");
					}
					return false;
				case CALL_ENV_FUNCTION:
					expr->expr_kind = EXPR_IDENTIFIER;
					expr->resolve_status = RESOLVE_DONE;
					expr->identifier_expr.decl = context->call_env.current_function;
					expr->type = expr->identifier_expr.decl->type;
					return true;
			}
			UNREACHABLE
		case BUILTIN_DEF_FUNC:
			switch (context->call_env.kind)
			{
				case CALL_ENV_GLOBAL_INIT:
					expr_rewrite_to_string(expr, "<GLOBAL>");
					return true;
				case CALL_ENV_CHECKS:
					expr_rewrite_to_string(expr, "<CHECKS>");
					return true;
				case CALL_ENV_FUNCTION:
				{
					Decl *current_func = context->call_env.current_function;
					TypeInfo *func_type = type_infoptrzero(current_func->func_decl.type_parent);
					if (func_type)
					{
						scratch_buffer_clear();
						scratch_buffer_append(func_type->type->name);
						scratch_buffer_append_char('.');
						scratch_buffer_append(current_func->name);
						expr_rewrite_to_string(expr, scratch_buffer_copy());
						return true;
					}
					expr_rewrite_to_string(expr, current_func->name);
					return true;
				}
				case CALL_ENV_INITIALIZER:
					expr_rewrite_to_string(expr, "<static initializer>");
					return true;
				case CALL_ENV_FINALIZER:
					expr_rewrite_to_string(expr, "<static finalizer>");
					return true;
				case CALL_ENV_ATTR:
					expr_rewrite_to_string(expr, "<attribute>");
					return true;
			}
			UNREACHABLE
		case BUILTIN_DEF_NONE:
		{
			Expr *value = htable_get(&global_context.compiler_defines, string);
			if (!value)
			{
				if (report_missing)
				{
					SEMA_ERROR(expr, "The compiler constant '%s' was not defined, did you mistype or forget to add it?", string);
				}
				return false;
			}
			expr_replace(expr, value);
			return true;
		}
		case BUILTIN_DEF_TEST_NAMES:
			expr->type = type_get_subarray(type_chars);
			expr->test_hook_expr = BUILTIN_DEF_TEST_NAMES;
			expr->expr_kind = EXPR_TEST_HOOK;
			return true;
		case BUILTIN_DEF_TEST_FNS:
			expr->type = type_get_subarray(type_voidptr);
			expr->test_hook_expr = BUILTIN_DEF_TEST_FNS;
			expr->expr_kind = EXPR_TEST_HOOK;
			return true;
	}
	UNREACHABLE
}



static Decl *sema_expr_analyse_var_path(SemaContext *context, Expr *expr, ExprFlatElement **elements)
{
	if (!sema_analyse_expr_lvalue_fold_const(context, expr)) return NULL;
	Expr *current = expr;
	Decl *decl = NULL;
RETRY:
	switch (current->expr_kind)
	{
		case EXPR_CT_IDENT:
			current = current->identifier_expr.decl->var.init_expr;
			goto RETRY;
		case EXPR_IDENTIFIER:
			decl = current->identifier_expr.decl;
			break;
		default:
			SEMA_ERROR(expr, "A variable was expected here.");
			return NULL;
	}
	if (!sema_analyse_decl(context, decl)) return NULL;
	return decl;
}

static inline bool sema_expr_analyse_flat_element(SemaContext *context, ExprFlatElement *element, Type *type, Decl **member_ref, ArraySize *index_ref, Type **return_type, unsigned i, SourceSpan loc,
												  bool *is_missing)
{
	Expr *inner = element->inner;
	Type *actual_type = type_flatten_distinct(type);
	if (element->array)
	{
		if (!type_is_arraylike(actual_type) && actual_type->type_kind)
		{
			if (is_missing)
			{
				*is_missing = true;
				return false;
			}
			SEMA_ERROR(inner, "It's not possible to constant index into something that is not an array nor vector.");
			return false;
		}
		if (!sema_analyse_expr(context, inner)) return false;
		if (!type_is_integer(inner->type))
		{
			SEMA_ERROR(inner, "Expected an integer index.");
			return false;
		}
		if (!expr_is_const(inner))
		{
			SEMA_ERROR(inner, "Expected a constant index.");
			return false;
		}
		Int value = inner->const_expr.ixx;
		if (!int_fits(value, type_isize->canonical->type_kind))
		{
			SEMA_ERROR(inner, "The index is out of range for a %s.", type_quoted_error_string(type_isize));
			return false;
		}
		if (int_is_neg(value))
		{
			SEMA_ERROR(inner, "The index must be zero or greater.");
			return false;
		}
		type = actual_type->array.base;
		ArraySize len = actual_type->array.len;
		int64_t index = int_to_i64(value);
		if (len && index >= len)
		{
			if (is_missing)
			{
				*is_missing = true;
				*index_ref = 0;
				return false;
			}
			SEMA_ERROR(element->inner, "Index exceeds array bounds.");
			return false;
		}
		*return_type = type;
		*index_ref = index;
		*member_ref = NULL;
		return true;
	}
	inner = sema_expr_resolve_access_child(context, inner, is_missing);
	if (!inner) return false;
	if (inner->expr_kind != EXPR_IDENTIFIER)
	{
		SEMA_ERROR(inner, "Expected an identifier here.");
		return false;
	}
	const char *kw = inner->identifier_expr.ident;
	if (kw == kw_ptr)
	{
		switch (actual_type->type_kind)
		{
			case TYPE_SUBARRAY:
				*member_ref = NULL;
				*return_type = actual_type->array.base;
				return true;
			case TYPE_ANY:
				*member_ref = NULL;
				*return_type = type_voidptr;
				return true;
			default:
				break;
		}
	}
	if (kw == kw_len)
	{
		if (type_is_arraylike(actual_type) || actual_type->type_kind == TYPE_SUBARRAY)
		{
			*member_ref = NULL;
			*return_type = type_usize;
			return true;
		}
	}
	if (!type_is_union_or_strukt(actual_type))
	{
		if (is_missing)
		{
			*is_missing = true;
			return false;
		}
		if (i == 0)
		{
			sema_error_at(loc, "%s has no members.", type_quoted_error_string(type));
		}
		else
		{
			sema_error_at(loc, "There is no such member in %s.", type_quoted_error_string(type));
		}
		return false;
	}
	Decl *member = sema_decl_stack_find_decl_member(actual_type->decl, element->inner->identifier_expr.ident);
	if (!member)
	{
		if (is_missing)
		{
			*is_missing = true;
			return false;
		}
		sema_error_at(loc, "There is no such member in %s.", type_quoted_error_string(type));
		return false;
	}
	*member_ref = member;
	*return_type = member->type;
	return true;
}

static inline bool sema_expr_analyse_ct_alignof(SemaContext *context, Expr *expr)
{
	Expr *main_var = expr->ct_call_expr.main_var;
	ExprFlatElement *path = expr->ct_call_expr.flat_path;
	Decl *decl = sema_expr_analyse_var_path(context, main_var, &path);
	if (!decl) return false;
	Type *type = decl->type;
	if (type_is_invalid_storage_type(type))
	{
		SEMA_ERROR(main_var, "Cannot use '$alignof' on type %s.", type_quoted_error_string(type));
		return false;
	}
	AlignSize align = decl && !decl_is_user_defined_type(decl) ? decl->alignment : type_abi_alignment(type);
	VECEACH(path, i)
	{
		ExprFlatElement *element = &path[i];
		Decl *member;
		ArraySize index = 0;
		Type *result_type;
		if (!sema_expr_analyse_flat_element(context, element, type, &member, &index, &result_type, i, i == 0 ? main_var->span : expr->span, NULL)) return false;
		if (member)
		{
			align = type_min_alignment(member->offset, align);
		}
		else
		{
			TypeSize size = type_size(result_type);
			align = type_min_alignment(size * index, align);
		}
		type = result_type;
	}

	expr_rewrite_const_int(expr, type_isize, align, true);
	return true;
}

static inline void sema_expr_rewrite_to_type_nameof(Expr *expr, Type *type, TokenType name_type)
{
	if (name_type == TOKEN_CT_EXTNAMEOF)
	{
		expr_rewrite_to_string(expr, type->decl->extname);
		return;
	}

	if (name_type == TOKEN_CT_NAMEOF || type_is_builtin(type->type_kind))
	{
		expr_rewrite_to_string(expr, type->name);
		return;
	}
	scratch_buffer_clear();

	Module *module = type_base_module(type);
	if (module)
	{
		scratch_buffer_append(module->name->module);
		scratch_buffer_append("::");
	}
	scratch_buffer_append(type->name);
	expr_rewrite_to_string(expr, scratch_buffer_copy());
}

static inline bool sema_expr_analyse_ct_nameof(SemaContext *context, Expr *expr)
{
	Expr *main_var = expr->ct_call_expr.main_var;
	ExprFlatElement *path = expr->ct_call_expr.flat_path;
	Decl *decl = sema_expr_analyse_var_path(context, main_var, &path);
	if (!decl) return false;
	Type *type = decl->type;

	TokenType name_type = expr->ct_call_expr.token_type;

	if (vec_size(path))
	{
		SEMA_ERROR(main_var, "You can only take the name of types and variables, not their sub elements.");
		return false;
	}

	if (name_type == TOKEN_CT_EXTNAMEOF)
	{
		if (!decl->extname)
		{
			SEMA_ERROR(main_var, "'%s' does not have an external name.", decl->name);
			return false;
		}
		expr_rewrite_to_string(expr, decl->extname);
		return true;
	}
	if (!decl->unit || name_type == TOKEN_CT_NAMEOF || decl_is_local(decl))
	{
		expr_rewrite_to_string(expr, decl->name);
		return true;
	}
	scratch_buffer_clear();
	scratch_buffer_append(decl->unit->module->name->module);
	scratch_buffer_append("::");
	scratch_buffer_append(decl->name);
	expr_rewrite_to_string(expr, scratch_buffer_copy());
	return true;
}


static Type *sema_expr_check_type_exists(SemaContext *context, TypeInfo *type_info)
{
	if (type_info->resolve_status == RESOLVE_DONE)
	{
		return type_info->type;
	}
RETRY:
	switch (type_info->kind)
	{
		case TYPE_INFO_POISON:
			return poisoned_type;
		case TYPE_INFO_VECTOR:
		{
			ArraySize size;
			if (!sema_resolve_array_like_len(context, type_info, &size)) return poisoned_type;
			Type *type = sema_expr_check_type_exists(context, type_info->array.base);
			if (!type) return NULL;
			if (!type_ok(type)) return type;
			if (!type_is_valid_for_vector(type))
			{
				SEMA_ERROR(type_info->array.base,
				           "%s cannot be vectorized. Only integers, floats and booleans are allowed.",
				           type_quoted_error_string(type));
				return poisoned_type;
			}
			return type_get_vector(type, size);
		}
		case TYPE_INFO_ARRAY:
		{
			ArraySize size;
			if (!sema_resolve_array_like_len(context, type_info, &size)) return poisoned_type;
			Type *type = sema_expr_check_type_exists(context, type_info->array.base);
			if (!type) return NULL;
			if (!type_ok(type)) return type;
			return type_get_array(type, size);
		}
		case TYPE_INFO_CT_IDENTIFIER:
		case TYPE_INFO_IDENTIFIER:
		{
			Decl *decl = sema_find_path_symbol(context, type_info->unresolved.name, type_info->unresolved.path);
			if (!decl) return NULL;
			if (!decl_ok(decl)) return poisoned_type;
			return decl->type->canonical;
		}
		case TYPE_INFO_VATYPE:
			if (!sema_resolve_type_info(context, type_info)) return poisoned_type;
			return type_info->type->canonical;
		case TYPE_INFO_TYPEFROM:
		case TYPE_INFO_TYPEOF:
			if (!sema_resolve_type_info(context, type_info)) return poisoned_type;
			return type_info->type;
		case TYPE_INFO_EVALTYPE:
		{
			Expr *expr = type_info->unresolved_type_expr;
			expr = sema_ct_eval_expr(context, "$evaltype", expr, false);
			if (!expr) return NULL;
			if (expr->expr_kind != EXPR_TYPEINFO)
			{
				SEMA_ERROR(expr, "Only type names may be resolved with $evaltype.");
				return poisoned_type;
			}
			type_info = expr->type_expr;
			goto RETRY;
		}
		case TYPE_INFO_SUBARRAY:
		{
			// If it's an array, make sure we can resolve the length
			Type *type = sema_expr_check_type_exists(context, type_info->array.base);
			if (!type) return NULL;
			if (!type_ok(type)) return type;
			return type_get_subarray(type);
		}
		case TYPE_INFO_INFERRED_ARRAY:
		{
			// If it's an array, make sure we can resolve the length
			Type *type = sema_expr_check_type_exists(context, type_info->array.base);
			if (!type) return NULL;
			if (!type_ok(type)) return type;
			return type_get_inferred_array(type);
		}
		case TYPE_INFO_INFERRED_VECTOR:
		{
			// If it's a vector, make sure we can resolve the length
			Type *type = sema_expr_check_type_exists(context, type_info->array.base);
			if (!type) return NULL;
			if (!type_ok(type)) return type;
			return type_get_inferred_vector(type);
		}
		case TYPE_INFO_SCALED_VECTOR:
		{
			Type *type = sema_expr_check_type_exists(context, type_info->array.base);
			if (!type) return NULL;
			if (!type_ok(type)) return type;
			return type_get_scaled_vector(type);
		}
		case TYPE_INFO_POINTER:
		{
			// If it's an array, make sure we can resolve the length
			Type *type = sema_expr_check_type_exists(context, type_info->array.base);
			if (!type) return NULL;
			if (!type_ok(type)) return type;
			return type_get_ptr(type);
		}
	}
	UNREACHABLE
}

static inline Expr *sema_ct_checks_exprlist_compiles(SemaContext *context, Expr *exprlist)
{
	assert(exprlist->expr_kind == EXPR_EXPRESSION_LIST);
	Expr *failed = NULL;
	bool suppress_error = global_context.suppress_errors;
	global_context.suppress_errors = true;
	CallEnvKind eval_kind = context->call_env.kind;
	context->call_env.kind = CALL_ENV_CHECKS;
	SCOPE_START_WITH_FLAGS(SCOPE_CHECKS);
		FOREACH_BEGIN(Expr *expr, exprlist->expression_list)
			if (!sema_analyse_expr(context, expr))
			{
				failed = expr;
				break;
			}
		FOREACH_END();
	SCOPE_END;
	context->call_env.kind = eval_kind;
	global_context.suppress_errors = suppress_error;
	return failed;
}

static inline bool sema_expr_analyse_ct_checks(SemaContext *context, Expr *expr)
{
	Expr *err = sema_ct_checks_exprlist_compiles(context, expr->inner_expr);
	expr_rewrite_const_bool(expr, type_bool, err == NULL);
	return true;
}

static inline bool sema_expr_analyse_ct_defined(SemaContext *context, Expr *expr)
{
	if (expr->resolve_status == RESOLVE_DONE) return expr_ok(expr);

	Expr *main_var = expr->ct_call_expr.main_var;
	Type *type = NULL;
	Decl *decl = NULL;
	ExprFlatElement *flat_path = expr->ct_call_expr.flat_path;
RETRY:
	switch (main_var->expr_kind)
	{
		case EXPR_IDENTIFIER:
			// 2. An identifier does a lookup
			decl = sema_find_path_symbol(context, main_var->identifier_expr.ident, main_var->identifier_expr.path);
			// 2a. If it failed, then error
			if (!decl_ok(decl)) return false;
			// 2b. If it's missing, goto not defined
			if (!decl) goto NOT_DEFINED;
			type = decl->type;
			break;
		case EXPR_COMPILER_CONST:
			if (!sema_expr_analyse_compiler_const(context, main_var, false)) goto NOT_DEFINED;
			break;
		case EXPR_TYPEINFO:
		{
			type = sema_expr_check_type_exists(context, main_var->type_expr);
			if (!type) goto NOT_DEFINED;
			if (!type_ok(type)) return false;
			break;
		}
		case EXPR_BUILTIN:
			if (!sema_expr_analyse_builtin(context, main_var, false)) goto NOT_DEFINED;
			break;
		case EXPR_CT_EVAL:
			main_var = sema_ct_eval_expr(context, "$eval", main_var->inner_expr, false);
			if (!main_var) goto NOT_DEFINED;
			goto RETRY;
		default:
			SEMA_ERROR(main_var, "Expected an identifier here.");
			return false;
	}

	VECEACH(flat_path, i)
	{
		ExprFlatElement *element = &flat_path[i];
		Decl *member = NULL;
		ArraySize index;
		Type *ret_type;
		bool missing = false;
		if (!sema_expr_analyse_flat_element(context, element, type, &member, &index, &ret_type, i, i == 0 ? main_var->span : expr->span, &missing))
		{
			if (missing) goto NOT_DEFINED;
			return false;
		}
		type = ret_type;
	}

	expr_rewrite_const_bool(expr, type_bool, true);
	return true;

NOT_DEFINED:
	expr_rewrite_const_bool(expr, type_bool, false);
	return true;
}

static inline bool sema_expr_analyse_variant(SemaContext *context, Expr *expr)
{
	Expr *ptr = exprptr(expr->variant_expr.ptr);
	Expr *typeid = exprptr(expr->variant_expr.type_id);
	if (!sema_analyse_expr(context, ptr)) return false;
	if (!sema_analyse_expr(context, typeid)) return false;
	if (!type_is_pointer(ptr->type))
	{
		SEMA_ERROR(ptr, "This must be a pointer, but is %s.", type_quoted_error_string(ptr->type));
		return false;
	}
	if (typeid->type != type_typeid)
	{
		SEMA_ERROR(ptr, "This must of type 'typeid', but was %s.", type_quoted_error_string(ptr->type));
		return false;
	}
	expr->type = type_any;
	return true;
}

static inline bool sema_expr_analyse_ct_arg(SemaContext *context, Expr *expr)
{
	assert(expr->resolve_status == RESOLVE_RUNNING);
	TokenType type = expr->ct_arg_expr.type;
	if (!context->current_macro)
	{
		SEMA_ERROR(expr, "'%s' can only be used inside of a macro.", token_type_to_string(type));
		return false;
	}
	switch (type)
	{
		case TOKEN_CT_VACOUNT:
			expr_rewrite_const_int(expr, type_usize, vec_size(context->macro_varargs), true);
			return true;
		case TOKEN_CT_VAARG:
		{
			// A normal argument, this means we only evaluate it once.
			ASSIGN_EXPR_OR_RET(Expr *arg_expr, sema_expr_analyse_ct_arg_index(context, exprptr(expr->ct_arg_expr.arg)), false);

			Decl *decl = NULL;
			// Try to find the original param.
			FOREACH_BEGIN(Decl *val, context->macro_params)
				if (!val) continue;
				if (val->var.init_expr == arg_expr)
				{
					decl = val;
					break;
				}
			FOREACH_END();
			// Not found, so generate a new.
			if (!decl)
			{
				decl = decl_new_generated_var(arg_expr->type, VARDECL_PARAM, arg_expr->span);
				decl->var.init_expr = arg_expr;
				vec_add(context->macro_params, decl);
			}
			// Replace with the identifier.
			expr->expr_kind = EXPR_IDENTIFIER;
			expr->identifier_expr = (ExprIdentifier) { .decl = decl };
			expr->resolve_status = RESOLVE_DONE;
			expr->type = decl->type;
			return true;
		}
		case TOKEN_CT_VAEXPR:
		{
			// An expr argument, this means we copy and evaluate.
			ASSIGN_EXPR_OR_RET(Expr *arg_expr, sema_expr_analyse_ct_arg_index(context, exprptr(expr->ct_arg_expr.arg)), false);
			expr_replace(expr, copy_expr_single(arg_expr));
			return true;
		}
		case TOKEN_CT_VACONST:
		{
			// An expr argument, this means we copy and evaluate.
			ASSIGN_EXPR_OR_RET(Expr *arg_expr, sema_expr_analyse_ct_arg_index(context, exprptr(expr->ct_arg_expr.arg)), false);
			arg_expr = copy_expr_single(arg_expr);
			if (!expr_is_constant_eval(arg_expr, CONSTANT_EVAL_CONSTANT_VALUE))
			{
				SEMA_ERROR(arg_expr, "This argument needs to be a compile time constant.");
				return false;
			}
			expr_replace(expr, arg_expr);
			return true;
		}
		case TOKEN_CT_VAREF:
		{
			// A normal argument, this means we only evaluate it once.
			ASSIGN_EXPR_OR_RET(Expr *arg_expr, sema_expr_analyse_ct_arg_index(context, exprptr(expr->ct_arg_expr.arg)), false);

			if (!sema_binary_is_expr_lvalue(arg_expr, arg_expr)) return false;

			Decl *decl = NULL;
			// Try to find the original param.
			FOREACH_BEGIN(Decl *val, context->macro_params)
				if (!val) continue;
				if (val->var.init_expr == arg_expr)
				{
					decl = val;
					decl->var.kind = VARDECL_PARAM_REF;
					break;
				}
			FOREACH_END();
			// Not found, so generate a new.
			if (!decl)
			{
				decl = decl_new_generated_var(arg_expr->type, VARDECL_PARAM_REF, arg_expr->span);
				decl->var.init_expr = arg_expr;
				vec_add(context->macro_params, decl);
			}
			// Replace with the identifier.
			expr->expr_kind = EXPR_IDENTIFIER;
			expr->identifier_expr = (ExprIdentifier) { .decl = decl };
			expr->resolve_status = RESOLVE_DONE;
			expr->type = decl->type;
			return true;
		}
		case TOKEN_CT_VATYPE:
		default:
			UNREACHABLE;
	}
}

static inline bool sema_expr_analyse_ct_stringify(SemaContext *context, Expr *expr)
{
	Expr *inner = expr->inner_expr;
	// Only hash ident style stringify reaches here.
	assert(inner->expr_kind == EXPR_HASH_IDENT);
	Decl *decl = sema_resolve_symbol(context, inner->ct_ident_expr.identifier, NULL, inner->span);
	if (!decl_ok(decl)) return false;
	const char *desc = span_to_string(decl->var.hash_var.span);
	if (!desc)
	{
		SEMA_ERROR(expr, "Failed to stringify hash variable contents - they must be a single line and not exceed 255 characters.");
		return false;
	}
	expr_rewrite_to_string(expr, desc);
	return true;
}

static inline bool sema_expr_analyse_ct_eval(SemaContext *context, Expr *expr)
{
	TokenType type;
	Path *path = NULL;
	Expr *result = sema_ct_eval_expr(context, "$eval", expr->inner_expr, true);
	if (!result) return false;
	if (result->expr_kind == EXPR_TYPEINFO)
	{
		SEMA_ERROR(result, "Evaluation to a type requires the use of '$evaltype' rather than '$eval'.");
		return false;
	}
	expr_replace(expr, result);
	return sema_analyse_expr_dispatch(context, expr);
}


static inline bool sema_expr_analyse_ct_offsetof(SemaContext *context, Expr *expr)
{
	Expr *main_var = expr->ct_call_expr.main_var;
	ExprFlatElement *path = expr->ct_call_expr.flat_path;
	Decl *decl = sema_expr_analyse_var_path(context, main_var, &path);
	if (!decl) return false;
	if (!vec_size(path))
	{
		SEMA_ERROR(expr, "Expected a path to get the offset of.");
		return false;
	}

	ByteSize offset = 0;
	Type *type = decl->type;
	VECEACH(path, i)
	{
		ExprFlatElement *element = &path[i];
		Decl *member;
		ArraySize index = 0;
		Type *result_type;
		if (!sema_expr_analyse_flat_element(context, element, type, &member, &index, &result_type, i, i == 0 ? main_var->span : expr->span, NULL)) return false;
		if (member)
		{
			offset += member->offset;
		}
		else
		{
			offset += type_size(result_type) * index;
		}
		type = result_type;
	}

	expr_rewrite_const_int(expr, type_isz, offset, true);

	return true;
}

static inline bool sema_expr_analyse_ct_call(SemaContext *context, Expr *expr)
{
	switch (expr->ct_call_expr.token_type)
	{
		case TOKEN_CT_DEFINED:
			return sema_expr_analyse_ct_defined(context, expr);
		case TOKEN_CT_ALIGNOF:
			return sema_expr_analyse_ct_alignof(context, expr);
		case TOKEN_CT_OFFSETOF:
			return sema_expr_analyse_ct_offsetof(context, expr);
		case TOKEN_CT_QNAMEOF:
		case TOKEN_CT_NAMEOF:
		case TOKEN_CT_EXTNAMEOF:
			return sema_expr_analyse_ct_nameof(context, expr);
		default:
			UNREACHABLE
	}
}


static inline BuiltinFunction builtin_by_name(const char *name)
{
	for (unsigned i = 0; i < NUMBER_OF_BUILTINS; i++)
	{
		if (builtin_list[i] == name) return (BuiltinFunction)i;
	}
	return BUILTIN_NONE;
}

static inline TypeProperty type_property_by_name(const char *name)
{
	for (unsigned i = 0; i < NUMBER_OF_TYPE_PROPERTIES; i++)
	{
		if (type_property_list[i] == name) return (TypeProperty)i;
	}
	return TYPE_PROPERTY_NONE;
}

static inline bool sema_expr_analyse_retval(SemaContext *c, Expr *expr)
{
	if (c->active_scope.flags & SCOPE_MACRO)
	{
		TODO
	}
	if (expr->type == type_void)
	{
		SEMA_ERROR(expr, "'return' cannot be used on void functions.");
		return false;
	}
	Expr *return_value = c->return_expr;
	assert(return_value);
	expr->type = c->rtype;
	if (expr_is_const(return_value))
	{
		expr_replace(expr, return_value);
	}
	return true;
}

static inline bool sema_expr_analyse_builtin(SemaContext *context, Expr *expr, bool throw_error)
{
	const char *builtin_char = expr->builtin_expr.ident;
	BuiltinFunction func = builtin_by_name(builtin_char);
	if (func == BUILTIN_NONE)
	{
		if (throw_error) SEMA_ERROR(expr, "Unsupported builtin '%s'.", builtin_char);
		return false;
	}
	expr->builtin_expr.builtin = func;
	return true;
}

static inline bool sema_expr_analyse_compound_literal(SemaContext *context, Expr *expr)
{
	if (!sema_resolve_type_info(context, expr->expr_compound_literal.type_info)) return false;
	Type *type = expr->expr_compound_literal.type_info->type;
	if (type_is_optional(type))
	{
		SEMA_ERROR(expr->expr_compound_literal.type_info,
		           "The type here should always be written as a plain type and not a failable, please remove the '!'.");
		return false;
	}
	if (!sema_expr_analyse_initializer_list(context, type, expr->expr_compound_literal.initializer)) return false;
	expr_replace(expr, expr->expr_compound_literal.initializer);
	return true;
}


static inline bool sema_analyse_expr_dispatch(SemaContext *context, Expr *expr)
{
	switch (expr->expr_kind)
	{
		case EXPR_COND:
		case EXPR_DESIGNATOR:
		case EXPR_MACRO_BODY_EXPANSION:
		case EXPR_FLATPATH:
		case EXPR_NOP:
		case EXPR_TRY_UNWRAP_CHAIN:
		case EXPR_TRY_UNWRAP:
		case EXPR_CATCH_UNWRAP:
		case EXPR_VARIANTSWITCH:
		case EXPR_TYPEID_INFO:
		case EXPR_ASM:
		case EXPR_OPERATOR_CHARS:
		case EXPR_TEST_HOOK:
			UNREACHABLE
		case EXPR_VASPLAT:
			SEMA_ERROR(expr, "'$vasplat' can only be used inside of macros.");
			return false;
		case EXPR_CT_CHECKS:
			return sema_expr_analyse_ct_checks(context, expr);
		case EXPR_CT_ARG:
			return sema_expr_analyse_ct_arg(context, expr);
		case EXPR_VARIANT:
			return sema_expr_analyse_variant(context, expr);
		case EXPR_STRINGIFY:
			if (!sema_expr_analyse_ct_stringify(context, expr)) return false;
			return true;
		case EXPR_ARGV_TO_SUBARRAY:
			expr->type = type_get_subarray(type_chars);
			return true;
		case EXPR_DECL:
			if (!sema_analyse_var_decl(context, expr->decl_expr, true)) return false;
			expr->type = expr->decl_expr->type;
			return true;
		case EXPR_RETVAL:
			return sema_expr_analyse_retval(context, expr);
		case EXPR_BUILTIN:
			return sema_expr_analyse_builtin(context, expr, true);
		case EXPR_CT_CALL:
			return sema_expr_analyse_ct_call(context, expr);
		case EXPR_HASH_IDENT:
			return sema_expr_analyse_hash_identifier(context, expr);
		case EXPR_CT_IDENT:
			return sema_expr_analyse_ct_identifier(context, expr);
		case EXPR_FAILABLE:
			return sema_expr_analyse_failable(context, expr);
		case EXPR_COMPILER_CONST:
			return sema_expr_analyse_compiler_const(context, expr, true);
		case EXPR_POINTER_OFFSET:
			return sema_expr_analyse_pointer_offset(context, expr);
		case EXPR_POISONED:
			return false;
		case EXPR_SLICE_ASSIGN:
		case EXPR_BUILTIN_ACCESS:
		case EXPR_SLICE_COPY:
		case EXPR_BITASSIGN:
			// Created during semantic analysis
			UNREACHABLE
		case EXPR_MACRO_BLOCK:
			UNREACHABLE
		case EXPR_TYPEINFO:
			expr->type = type_typeinfo;
			return sema_resolve_type_info(context, expr->type_expr);
		case EXPR_SLICE:
			return sema_expr_analyse_slice(context, expr);
		case EXPR_FORCE_UNWRAP:
			return sema_expr_analyse_force_unwrap(context, expr);
		case EXPR_TRY:
			return sema_expr_analyse_try(context, expr);
		case EXPR_CATCH:
			return sema_expr_analyse_catch(context, expr);
		case EXPR_COMPOUND_LITERAL:
			return sema_expr_analyse_compound_literal(context, expr);
		case EXPR_EXPR_BLOCK:
			return sema_expr_analyse_expr_block(context, NULL, expr);
		case EXPR_RETHROW:
			return sema_expr_analyse_rethrow(context, expr);
		case EXPR_CONST:
			return true;
		case EXPR_CT_EVAL:
			return sema_expr_analyse_ct_eval(context, expr);
		case EXPR_BINARY:
			return sema_expr_analyse_binary(context, expr);
		case EXPR_TERNARY:
			return sema_expr_analyse_ternary(context, expr);
		case EXPR_UNARY:
		case EXPR_POST_UNARY:
			return sema_expr_analyse_unary(context, expr);
		case EXPR_TYPEID:
			return sema_expr_analyse_typeid(context, expr);
		case EXPR_IDENTIFIER:
			return sema_expr_analyse_identifier(context, NULL, expr);
		case EXPR_CALL:
			return sema_expr_analyse_call(context, expr);
		case EXPR_SUBSCRIPT:
			return sema_expr_analyse_subscript(context, expr, SUBSCRIPT_EVAL_VALUE);
		case EXPR_SUBSCRIPT_ADDR:
			return sema_expr_analyse_subscript(context, expr, SUBSCRIPT_EVAL_REF);
		case EXPR_GROUP:
			return sema_expr_analyse_group(context, expr);
		case EXPR_BITACCESS:
		case EXPR_SUBSCRIPT_ASSIGN:
			UNREACHABLE
		case EXPR_ACCESS:
			return sema_expr_analyse_access(context, expr);
		case EXPR_INITIALIZER_LIST:
		case EXPR_DESIGNATED_INITIALIZER_LIST:
			return sema_expr_analyse_initializer_list(context, type_untypedlist, expr);
		case EXPR_CAST:
			return sema_expr_analyse_cast(context, expr);
		case EXPR_EXPRESSION_LIST:
			return sema_expr_analyse_expr_list(context, expr);
	}
	UNREACHABLE
}


bool sema_analyse_cond_expr(SemaContext *context, Expr *expr)
{
	if (expr->expr_kind == EXPR_BINARY && expr->binary_expr.operator == BINARYOP_ASSIGN)
	{
		SEMA_ERROR(expr, "Assignment expressions must be enclosed in an extra () in conditionals.");
		return false;
	}
	if (!sema_analyse_expr(context, expr)) return false;
	if (IS_OPTIONAL(expr))
	{
		SEMA_ERROR(expr, "A failable %s cannot be implicitly converted to a regular boolean value, use 'try(<expr>)' "
		                 "and 'catch(<expr>)' to conditionally execute on success or failure.",
		           type_quoted_error_string(expr->type));
		return false;
	}
	return cast_implicit(context, expr, type_bool);
}


bool sema_analyse_expr_rhs(SemaContext *context, Type *to, Expr *expr, bool allow_failable)
{
	if (to && type_is_optional_type(to))
	{
		to = to->failable;
		assert(allow_failable);
	}
	if (!sema_analyse_inferred_expr(context, to, expr)) return false;
	if (to && allow_failable && to->canonical != expr->type->canonical && expr->type->canonical->type_kind == TYPE_FAULTTYPE)
	{
		Type *canonical = type_flatten_distinct(to);
		if (canonical != type_anyerr && canonical->type_kind != TYPE_FAULTTYPE && expr->expr_kind == EXPR_CONST)
		{
			sema_error_at_after(expr->span, "You need to add a trailing '!' here to make this an optional.");
			return false;
		}
	}
	if (to && !cast_implicit(context, expr, to)) return false;
	if (!allow_failable && IS_OPTIONAL(expr))
	{
		SEMA_ERROR(expr, "You cannot have a failable here.");
		return false;
	}
	return true;
}


static inline bool sema_cast_ct_ident_rvalue(SemaContext *context, Expr *expr)
{
	Decl *decl = expr->ct_ident_expr.decl;
	Expr *copy = copy_expr_single(decl->var.init_expr);
	if (!copy)
	{
		SEMA_ERROR(expr, "'%s' was not yet initialized to any value, assign a value to it before use.", decl->name);
		return false;
	}
	if (!sema_analyse_expr(context, copy)) return false;
	expr_replace(expr, copy);
	return true;
}

static inline bool sema_cast_rvalue(SemaContext *context, Expr *expr)
{
	if (!expr_ok(expr)) return false;
	switch (expr->expr_kind)
	{
		case EXPR_MACRO_BODY_EXPANSION:
			if (!expr->body_expansion_expr.first_stmt)
			{
				SEMA_ERROR(expr, "'@%s' must be followed by ().", declptr(context->current_macro->func_decl.body_param)->name);
				return false;
			}
			break;
		case EXPR_BUILTIN:
			SEMA_ERROR(expr, "A builtin must be followed by ().");
			return false;
		case EXPR_ACCESS:
			if (expr->access_expr.ref->decl_kind == DECL_FUNC)
			{
				SEMA_ERROR(expr, "A function name must be followed by '(' or preceeded by '&'.");
				return false;
			}
			if (expr->access_expr.ref->decl_kind == DECL_MACRO)
			{
				SEMA_ERROR(expr, "A macro name must be followed by '('.");
				return false;
			}
			break;
		case EXPR_TYPEINFO:
			SEMA_ERROR(expr, "A type must be followed by either (...) or '.'.");
			return false;
		case EXPR_CT_IDENT:
			if (!sema_cast_ct_ident_rvalue(context, expr)) return false;
			break;
		case EXPR_IDENTIFIER:
			if (!sema_cast_ident_rvalue(context, expr)) return false;
			break;
		case EXPR_UNARY:
		{
			if (expr->unary_expr.operator != UNARYOP_DEREF) break;
			Expr *inner = expr->inner_expr;
			if (inner->expr_kind != EXPR_IDENTIFIER) break;
			Decl *decl = inner->identifier_expr.decl;
			if (decl->decl_kind != DECL_VAR) break;
			if (!decl->var.out_param) break;
			SEMA_ERROR(expr, "'out' parameters may not be read.");
			return false;
		}
		default:
			break;
	}
	return true;
}

bool sema_analyse_ct_expr(SemaContext *context, Expr *expr)
{
	if (!sema_analyse_expr_lvalue_fold_const(context, expr)) return false;
	if (expr->expr_kind == EXPR_TYPEINFO)
	{
		Type *cond_val = expr->type_expr->type;
		expr->expr_kind = EXPR_CONST;
		expr->const_expr.const_kind = CONST_TYPEID;
		expr->const_expr.typeid = cond_val->canonical;
		expr->type = type_typeid;
	}
	if (!sema_cast_rvalue(context, expr)) return false;
	if (!expr_is_const(expr))
	{
		SEMA_ERROR(expr, "Expected a compile time expression.");
		return false;
	}
	return true;
}

bool sema_analyse_expr_lvalue_fold_const(SemaContext *context, Expr *expr)
{
	if (!sema_analyse_expr_lvalue(context, expr)) return false;
	if (expr->expr_kind == EXPR_CT_IDENT)
	{
		if (!sema_cast_ct_ident_rvalue(context, expr)) return false;
	}
	return true;
}

bool sema_analyse_expr_lvalue(SemaContext *context, Expr *expr)
{
	assert(expr);
	switch (expr->resolve_status)
	{
		case RESOLVE_NOT_DONE:
			expr->resolve_status = RESOLVE_RUNNING;
			if (!sema_analyse_expr_dispatch(context, expr)) return expr_poison(expr);
			expr->resolve_status = RESOLVE_DONE;
			return true;
		case RESOLVE_RUNNING:
			SEMA_ERROR(expr, "Recursive resolution of expression");
			return expr_poison(expr);
		case RESOLVE_DONE:
			return expr_ok(expr);
		default:
			UNREACHABLE
	}
}


bool sema_analyse_expr(SemaContext *context, Expr *expr)
{
	return sema_analyse_expr_lvalue(context, expr) && sema_cast_rvalue(context, expr);
}

bool sema_analyse_expr_require_const(SemaContext *context, Expr *expr)
{
	if (!sema_analyse_expr(context, expr)) return false;
	if (!expr_is_const(expr))
	{
		SEMA_ERROR(expr, "Expected a constant expression.");
		return false;
	}
	return true;
}

static inline int64_t expr_get_index_max(Expr *expr)
{
	if (expr_is_const_untyped_list(expr))
	{
		return vec_size(expr->const_expr.untyped_list);
	}
	Type *type = expr->type;
RETRY:
	switch (type->type_kind)
	{
		case TYPE_TYPEDEF:
			type = type->canonical;
			goto RETRY;
		case TYPE_DISTINCT:
			type = type->decl->distinct_decl.base_type;
			goto RETRY;
		case TYPE_UNTYPED_LIST:
			UNREACHABLE
		case TYPE_ARRAY:
		case TYPE_VECTOR:
			return type->array.len;
		case TYPE_OPTIONAL:
			type = type->failable;
			goto RETRY;
		default:
			return -1;
	}
	UNREACHABLE
}

void expr_insert_widening_type(Expr *expr, Type *infer_type)
{
	if (!infer_type) return;
	switch (expr->expr_kind)
	{
		case EXPR_BINARY:
			switch (expr->binary_expr.operator)
			{
				case BINARYOP_MULT:
				case BINARYOP_SUB:
				case BINARYOP_ADD:
				case BINARYOP_DIV:
				case BINARYOP_MOD:
				case BINARYOP_SHR:
				case BINARYOP_SHL:
				case BINARYOP_BIT_OR:
				case BINARYOP_BIT_XOR:
				case BINARYOP_BIT_AND:
				case BINARYOP_ELSE:
					if (!expr_is_simple(exprptr(expr->binary_expr.left)) || !expr_is_simple(exprptr(expr->binary_expr.right))) return;
					expr->type = infer_type;
					expr->binary_expr.widen = true;
					return;
				default:
					return;
			}
		case EXPR_GROUP:
			expr_insert_widening_type(expr->inner_expr, infer_type);
			return;
		case EXPR_TERNARY:
			if (!exprid_is_simple(expr->ternary_expr.else_expr)) return;
			if (expr->ternary_expr.then_expr && !exprid_is_simple(expr->ternary_expr.else_expr)) return;
			expr->type = infer_type;
			expr->ternary_expr.widen = true;
			return;
		case EXPR_UNARY:
			switch (expr->unary_expr.operator)
			{
				case UNARYOP_NEG:
				case UNARYOP_BITNEG:
					if (!expr_is_simple(expr->unary_expr.expr)) return;
					expr->type = infer_type;
					expr->unary_expr.widen = true;
					return;
				default:
					return;
			}
		default:
			return;
	}
	UNREACHABLE
}
bool sema_analyse_inferred_expr(SemaContext *context, Type *infer_type, Expr *expr)
{
	infer_type = type_no_optional(infer_type);
	switch (expr->resolve_status)
	{
		case RESOLVE_NOT_DONE:
			break;
		case RESOLVE_RUNNING:
			SEMA_ERROR(expr, "Recursive resolution of list.");
			return expr_poison(expr);
		case RESOLVE_DONE:
			return expr_ok(expr);
		default:
			UNREACHABLE
	}

	expr->resolve_status = RESOLVE_RUNNING;

	switch (expr->expr_kind)
	{
		case EXPR_DESIGNATED_INITIALIZER_LIST:
		case EXPR_INITIALIZER_LIST:
			if (!sema_expr_analyse_initializer_list(context, infer_type, expr)) return expr_poison(expr);
			break;
		case EXPR_EXPR_BLOCK:
			if (!sema_expr_analyse_expr_block(context, infer_type, expr)) return expr_poison(expr);
			break;
		case EXPR_IDENTIFIER:
			if (!sema_expr_analyse_identifier(context, infer_type, expr)) return expr_poison(expr);
			break;
		default:
			expr_insert_widening_type(expr, infer_type);
			if (!sema_analyse_expr_dispatch(context, expr)) return expr_poison(expr);
			break;
	}
	if (!sema_cast_rvalue(context, expr)) return false;
	expr->resolve_status = RESOLVE_DONE;
	return true;
}

TokenType sema_splitpathref(const char *string, ArraySize len, Path **path_ref, const char **ident_ref)
{
	ArraySize path_end = 0;
	*path_ref = NULL;
	*ident_ref = NULL;
	for (ArraySize i = 0; i < len; i++)
	{
		char ch = string[i];
		if (!char_is_alphanum_(ch))
		{
			if (ch == ':' && i > 0 && string[i + 1] == ':')
			{
				path_end = i;
				i++;
			}
			else
			{
				return TOKEN_INVALID_TOKEN;
			}
		}
	}
	if (path_end > 0)
	{
		*path_ref = path_create_from_string(string, path_end, INVALID_SPAN);
		string += path_end + 2;
		len -= path_end + 2;
	}
	while (len > 0)
	{
		char c = string[0];
		if (c != ' ' && c != '\t') break;
		len--;
		string++;
	}
	if (len == 0) return TOKEN_INVALID_TOKEN;
	uint32_t hash = FNV1_SEED;
	for (size_t i = 0; i < len; i++)
	{
		char c = string[i];
		if (!char_is_alphanum_(c)) return TOKEN_INVALID_TOKEN;
		hash = FNV1a(c, hash);
	}
	TokenType type;
	*ident_ref = symtab_find(string, len, hash, &type);
	if (!*ident_ref) return TOKEN_IDENT;
	switch (type)
	{
		case TOKEN_TYPE_IDENT:
		case TOKEN_IDENT:
		case TOKEN_CONST_IDENT:
			return type;
		case TYPE_TOKENS:
			if (!*path_ref) return type;
			FALLTHROUGH;
		default:
			*ident_ref = NULL;
			return TOKEN_INVALID_TOKEN;
	}
}

bool sema_insert_method_call(SemaContext *context, Expr *method_call, Decl *method_decl, Expr *parent, Expr **arguments)
{
	*method_call = (Expr) { .expr_kind = EXPR_CALL,
			.resolve_status = RESOLVE_RUNNING,
			.call_expr.func_ref = declid(method_decl),
			.call_expr.arguments = arguments,
			.call_expr.is_func_ref = true,
			.call_expr.is_type_method = true };
	Type *type = parent->type->canonical;
	Type *first = method_decl->func_decl.signature.params[0]->type;
	if (type != first)
	{
		if (first->type_kind == TYPE_POINTER && first->pointer == type)
		{
			expr_insert_addr(parent);
		}
		else if (type->type_kind == TYPE_POINTER && type->pointer == first)
		{
			expr_rewrite_insert_deref(parent);
		}
	}
	assert(parent && parent->type && first == parent->type->canonical);
	if (!sema_expr_analyse_general_call(context, method_call, method_decl, parent, false)) return expr_poison(method_call);
	method_call->resolve_status = RESOLVE_DONE;
	return true;
}

// Check if the assignment fits
bool sema_bit_assignment_check(Expr *right, Decl *member)
{
	// Don't check non-consts and non integers.
	if (!IS_CONST(right) || !type_is_integer(right->type)) return true;

	unsigned bits = member->var.end_bit - member->var.start_bit + 1;

	// If we have enough bits to fit, then we're done.
	if (bits >= type_bit_size(right->type) || int_is_zero(right->const_expr.ixx)) return true;

	if (int_bits_needed(right->const_expr.ixx) > bits)
	{
		SEMA_ERROR(right, "This constant would be truncated if stored in the bitstruct, do you need a wider bit range?");
		return false;
	}
	return true;
}
