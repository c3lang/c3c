// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "sema_internal.h"
#include <math.h>

#define RETURN_SEMA_FUNC_ERROR(_decl, _node, ...) do { sema_error_at(context, (_node)->span, __VA_ARGS__); SEMA_NOTE(_decl, "The definition was here."); return false; } while (0)
#define RETURN_NOTE_FUNC_DEFINITION do { SEMA_NOTE(callee->definition, "The definition was here."); return false; } while (0);

typedef enum
{
	CHECK_LVALUE,
	CHECK_ADDRESS,
	CHECK_VALUE,
} CheckType;
typedef struct
{
	bool macro;
	Decl *definition;
	SourceSpan call_location;
	const char *name;
	const char *block_parameter;
	Decl **params;
	Expr *struct_var;
	Signature *signature;
} CalledDecl;

// Properties
static inline BuiltinFunction builtin_by_name(const char *name);
static inline TypeProperty type_property_by_name(const char *name);
static inline bool sema_constant_fold_ops(Expr *expr);
static inline bool sema_expr_analyse_subscript(SemaContext *context, Expr *expr, CheckType check, bool check_valid);
static inline bool sema_expr_analyse_pointer_offset(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_slice(SemaContext *context, Expr *expr, CheckType check);
static inline bool sema_expr_analyse_access(SemaContext *context, Expr *expr, bool *missing_ref, CheckType check);
static inline bool sema_expr_analyse_compound_literal(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_builtin(SemaContext *context, Expr *expr, bool throw_error);
static inline bool sema_expr_analyse_binary(SemaContext *context, Expr *expr, bool *failed_ref);
static inline bool sema_expr_analyse_ct_eval(SemaContext *context, Expr *expr, CheckType check);
static inline bool sema_expr_analyse_identifier(SemaContext *context, Type *to, Expr *expr);
static inline bool sema_expr_analyse_ct_identifier(SemaContext *context, Expr *expr, CheckType check);

static inline bool sema_expr_analyse_ternary(SemaContext *context, Type *infer_type, Expr *expr);
static inline bool sema_expr_analyse_cast(SemaContext *context, Expr *expr, bool *invalid_cast_ref);
static inline bool sema_expr_analyse_or_error(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_unary(SemaContext *context, Expr *expr, bool *failed_ref, CheckType check);
static inline bool sema_expr_analyse_embed(SemaContext *context, Expr *expr, bool allow_fail);

static inline bool sema_expr_analyse_rethrow(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_force_unwrap(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_typeid(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_call(SemaContext *context, Expr *expr, bool *no_match_ref);
static inline bool sema_expr_analyse_expr_block(SemaContext *context, Type *infer_type, Expr *expr);
static inline bool sema_expr_analyse_optional(SemaContext *context, Expr *expr, bool *failed_ref);
static inline bool sema_expr_analyse_compiler_const(SemaContext *context, Expr *expr, bool report_missing);
static inline bool sema_expr_analyse_ct_arg(SemaContext *context, Type *infer_type, Expr *expr);
static inline bool sema_expr_analyse_ct_stringify(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_ct_offsetof(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_ct_call(SemaContext *context, Expr *expr);

static inline bool sema_expr_analyse_retval(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_expr_list(SemaContext *context, Expr *expr);

// -- binary
static bool sema_expr_analyse_sub(SemaContext *context, Expr *expr, Expr *left, Expr *right);
static bool sema_expr_analyse_add(SemaContext *context, Expr *expr, Expr *left, Expr *right);
static bool sema_expr_analyse_mult(SemaContext *context, Expr *expr, Expr *left, Expr *right);
static bool sema_expr_analyse_div(SemaContext *context, Expr *expr, Expr *left, Expr *right);
static bool sema_expr_analyse_mod(SemaContext *context, Expr *expr, Expr *left, Expr *right);
static bool sema_expr_analyse_bit(SemaContext *context, Expr *expr, Expr *left, Expr *right);
static bool sema_expr_analyse_enum_add_sub(SemaContext *context, Expr *expr, Expr *left, Expr *right);
static bool sema_expr_analyse_shift(SemaContext *context, Expr *expr, Expr *left, Expr *right);
static bool sema_expr_analyse_shift_assign(SemaContext *context, Expr *expr, Expr *left, Expr *right);
static bool sema_expr_analyse_and_or(SemaContext *context, Expr *expr, Expr *left, Expr *right);
static bool sema_expr_analyse_add_sub_assign(SemaContext *context, Expr *expr, Expr *left, Expr *right);
static bool sema_expr_analyse_slice_assign(SemaContext *context, Expr *expr, Type *left_type, Expr *right, bool is_unwrapped);
static bool sema_expr_analyse_ct_identifier_assign(SemaContext *context, Expr *expr, Expr *left, Expr *right);
static bool sema_expr_analyse_ct_type_identifier_assign(SemaContext *context, Expr *expr, Expr *left, Expr *right);
static bool sema_expr_analyse_assign(SemaContext *context, Expr *expr, Expr *left, Expr *right, bool *failed_ref);
static bool sema_expr_analyse_comp(SemaContext *context, Expr *expr, Expr *left, Expr *right);
static bool sema_expr_analyse_op_assign(SemaContext *context, Expr *expr, Expr *left, Expr *right, bool int_only,
                                        bool allow_bitstruct, bool is_add_sub);

// -- unary
static inline bool sema_expr_analyse_addr(SemaContext *context, Expr *expr, bool *failed_ref, CheckType check);
static inline bool sema_expr_analyse_neg_plus(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_bit_not(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_not(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_deref(SemaContext *context, Expr *expr, bool *failed_ref);
static inline bool sema_expr_analyse_ct_incdec(SemaContext *context, Expr *expr, Expr *inner);
static inline bool sema_expr_analyse_incdec(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_taddr(SemaContext *context, Expr *expr, bool *failed_ref);

// -- ct_call
static inline bool sema_expr_analyse_ct_alignof(SemaContext *context, Expr *expr);

static inline bool sema_expr_analyse_ct_nameof(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_ct_defined(SemaContext *context, Expr *expr);

// -- returns
static inline void context_pop_returns(SemaContext *context, Ast **restore);
static inline Ast **context_push_returns(SemaContext *context);
static inline Type *context_unify_returns(SemaContext *context);

// -- addr helpers
static const char *sema_addr_check_may_take(Expr *inner);
static inline const char *sema_addr_may_take_of_var(Expr *expr, Decl *decl);
static inline const char *sema_addr_may_take_of_ident(Expr *inner);

// -- subscript helpers
static bool sema_subscript_rewrite_index_const_list(Expr *const_list, ArraySize index, bool from_back, Expr *result);
static Type *sema_subscript_find_indexable_type_recursively(Type **type, Expr **parent);
static bool sema_analyse_assign_mutate_overloaded_subscript(SemaContext *context, Expr *main, Expr *subscript_expr, Type *type);

// -- binary helper functions
static void expr_binary_unify_failability(Expr *expr, Expr *left, Expr *right);

static inline bool sema_binary_analyse_subexpr(SemaContext *context, Expr *binary, Expr *left, Expr *right);
static inline bool sema_binary_analyse_arithmetic_subexpr(SemaContext *context, Expr *expr, const char *error, bool bool_and_bitstruct_is_allowed);
static bool sema_binary_check_unclear_op_precedence(Expr *left_side, Expr * main_expr, Expr *right_side);
static bool sema_binary_analyse_ct_common_assign(SemaContext *context, Expr *expr, Expr *left);
static bool sema_binary_arithmetic_promotion(SemaContext *context, Expr *left, Expr *right, Type *left_type, Type *right_type,
								 Expr *parent, const char *error_message, bool allow_bool_vec);
static bool sema_binary_is_unsigned_always_same_comparison(SemaContext *context, Expr *expr, Expr *left, Expr *right,
														   Type *lhs_type, Type *rhs_type);
static bool sema_binary_is_expr_lvalue(SemaContext *context, Expr *top_expr, Expr *expr, bool *failed_ref);
static void sema_binary_unify_voidptr(SemaContext *context, Expr *left, Expr *right, Type **left_type_ref, Type **right_type_ref);

// -- function helper functions
static inline bool sema_expr_analyse_var_call(SemaContext *context, Expr *expr, Type *func_ptr_type,
											  bool optional, bool *no_match_ref);
static inline bool sema_expr_analyse_func_call(SemaContext *context, Expr *expr, Decl *decl,
											   Expr *struct_var, bool optional, bool *no_match_ref);

static inline bool sema_call_analyse_func_invocation(SemaContext *context, Decl *decl, Type *type, Expr *expr,
													 Expr *struct_var,
													 bool optional, const char *name, bool *no_match_ref);
static inline bool sema_call_check_invalid_body_arguments(SemaContext *context, Expr *call, CalledDecl *callee);
static inline bool sema_call_evaluate_arguments(SemaContext *context, CalledDecl *callee, Expr *call, bool *optional, bool *no_match_ref);
static inline bool sema_call_check_contract_param_match(SemaContext *context, Decl *param, Expr *expr);
static bool sema_call_analyse_body_expansion(SemaContext *macro_context, Expr *call);
static bool sema_slice_index_is_in_range(SemaContext *context, Type *type, Expr *index_expr, bool end_index, bool from_end, bool *remove_from_end, bool check_valid);
static Expr **sema_vasplat_insert(SemaContext *context, Expr **init_expressions, Expr *expr, unsigned insert_point);

static inline bool sema_analyse_expr_dispatch(SemaContext *context, Expr *expr, CheckType check);

static Decl *sema_expr_analyse_var_path(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_decl_element(SemaContext *context, DesignatorElement *element, Type *type, Decl **member_ref, ArraySize *index_ref, Type **return_type, unsigned i, SourceSpan loc,
												  bool *is_missing);
static Type *sema_expr_check_type_exists(SemaContext *context, TypeInfo *type_info);
static inline bool sema_cast_ct_ident_rvalue(SemaContext *context, Expr *expr);
static bool sema_expr_rewrite_to_typeid_property(SemaContext *context, Expr *expr, Expr *typeid, const char *kw, bool *was_error);
static bool sema_expr_rewrite_to_type_property(SemaContext *context, Expr *expr, Type *type, TypeProperty property,
											   Type *parent_type);
static bool sema_type_property_is_valid_for_type(Type *original_type, TypeProperty property);
static bool sema_expr_rewrite_typeid_call(Expr *expr, Expr *typeid, TypeIdInfoKind kind, Type *result_type);
static inline void sema_expr_rewrite_typeid_kind(Expr *expr, Expr *parent);
static inline bool sema_expr_replace_with_enum_array(SemaContext *context, Expr *enum_array_expr, Decl *enum_decl);
static inline bool sema_expr_replace_with_enum_name_array(SemaContext *context, Expr *enum_array_expr, Decl *enum_decl);
static inline void sema_expr_rewrite_to_type_nameof(Expr *expr, Type *type, TokenType name_type);

static inline bool sema_create_const_kind(SemaContext *contect, Expr *expr, Type *type);
static inline bool sema_create_const_len(SemaContext *context, Expr *expr, Type *type);
static inline bool sema_create_const_inner(SemaContext *context, Expr *expr, Type *type);
static inline bool sema_create_const_min(SemaContext *context, Expr *expr, Type *type, Type *flat);
static inline bool sema_create_const_max(SemaContext *context, Expr *expr, Type *type, Type *flat);
static inline bool sema_create_const_params(SemaContext *context, Expr *expr, Type *type);
static inline void sema_create_const_membersof(SemaContext *context, Expr *expr, Type *type, AlignSize alignment,
											   AlignSize offset);
static inline void sema_create_const_methodsof(SemaContext *context, Expr *expr, Type *type);

static inline bool expr_both_any_integer_or_integer_vector(Expr *left, Expr *right);
static inline bool expr_both_any_integer_or_integer_bool_vector(Expr *left, Expr *right);
static inline bool expr_both_const(Expr *left, Expr *right);
static inline bool sema_identifier_find_possible_inferred(SemaContext *context, Type *to, Expr *expr);
static inline bool sema_expr_analyse_enum_constant(SemaContext *context, Expr *expr, const char *name, Decl *decl);

static inline bool sema_cast_ident_rvalue(SemaContext *context, Expr *expr);
static inline bool sema_cast_rvalue(SemaContext *context, Expr *expr, bool mutate);

static inline bool sema_expr_analyse_type_access(SemaContext *context, Expr *expr, Type *parent_type, Expr *identifier, bool *missing_ref);
static inline bool sema_expr_analyse_member_access(SemaContext *context, Expr *expr, Expr *parent, Expr *identifier, bool *missing_ref);
static inline bool sema_expr_fold_to_member(Expr *expr, Expr *parent, Decl *member);
static inline bool sema_expr_fold_to_index(Expr *expr, Expr *parent, SubscriptIndex index);
static inline bool sema_expr_fold_hash(SemaContext *context, Expr *expr);

static inline void sema_expr_flatten_const_ident(Expr *expr);
static inline bool sema_analyse_expr_check(SemaContext *context, Expr *expr, CheckType check);

static inline Expr **sema_prepare_splat_insert(Expr **exprs, unsigned added, unsigned insert_point);
static inline bool sema_analyse_maybe_dead_expr(SemaContext *, Expr *expr, bool is_dead, Type *infer_type);


// -- implementations

// Limit folding to integers and floats, exclude vectors.
static inline bool sema_constant_fold_ops(Expr *expr)
{
	if (!sema_cast_const(expr)) return false;
	switch (expr->const_expr.const_kind)
	{
		case CONST_INTEGER:
		case CONST_FLOAT:
		case CONST_BOOL:
		case CONST_ENUM:
		case CONST_ERR:
		case CONST_STRING:
		case CONST_POINTER:
		case CONST_TYPEID:
		case CONST_BYTES:
		case CONST_MEMBER:
			return true;
		case CONST_INITIALIZER:
		case CONST_SLICE:
		case CONST_UNTYPED_LIST:
		case CONST_REF:
			return false;
	}
	UNREACHABLE
}

Expr *sema_enter_inline_member(Expr *parent, CanonicalType *type)
{
	switch (type->type_kind)
	{
		case TYPE_STRUCT:
		{
			Decl *decl = type->decl;
			if (!decl->is_substruct) return NULL;
			Expr *embedded_struct = expr_access_inline_member(parent, decl);
			return embedded_struct;
		}
		case TYPE_DISTINCT:
		{
			Decl *decl = type->decl;
			if (!decl->is_substruct) return NULL;
			Expr *inner_expr = expr_copy(parent);
			type = type->decl->distinct->type;
			inner_expr->type = type;
			return inner_expr;
		}
		case TYPE_ENUM:
		{
			Decl *decl = type->decl;
			if (!decl->is_substruct) return NULL;
			if (parent->expr_kind == EXPR_CONST)
			{
				return copy_expr_single(parent->const_expr.enum_err_val->enum_constant.args[0]);
			}
			Expr *property = expr_new(EXPR_ACCESS, parent->span);
			property->resolve_status = RESOLVE_DONE;
			property->access_expr.parent = parent;
			property->access_expr.ref = decl->enums.parameters[0];
			property->type = property->access_expr.ref->type;
			return property;
		}
		default:
			return NULL;
	}
}

Expr *sema_expr_analyse_ct_arg_index(SemaContext *context, Expr *index_expr, unsigned *index_ref)
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
	if (!sema_cast_const(index_expr))
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
	if (index_ref) *index_ref = (unsigned)index_val.i.low;
	return context->macro_varargs[(size_t)index_val.i.low];
}

Expr *sema_ct_eval_expr(SemaContext *context, bool is_type_eval, Expr *inner, bool report_missing)
{
	Path *path = NULL;
	if (!sema_analyse_ct_expr(context, inner)) return false;
	if (!expr_is_const_string(inner))
	{
		SEMA_ERROR(inner, "'%s' expects a constant string as the argument.", is_type_eval ? "$evaltype" : "$eval");
		return NULL;
	}
	const char *interned_version = NULL;
	TokenType token = sema_splitpathref(inner->const_expr.bytes.ptr, inner->const_expr.bytes.len, &path, &interned_version);
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
					SEMA_ERROR(inner, "'%.*s' could not be found, did you spell it right?", (int)inner->const_expr.bytes.len, inner->const_expr.bytes.ptr);
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

Expr *expr_access_inline_member(Expr *parent, Decl *parent_decl)
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
	return expr_is_const(left) && expr_is_const(right);
}

static inline bool expr_both_any_integer_or_integer_vector(Expr *left, Expr *right)
{
	Type *flatten_left = type_flatten(left->type);
	Type *flatten_right = type_flatten(right->type);
	if (type_is_integer(flatten_left) && type_is_integer(flatten_right)) return true;

	if (flatten_left->type_kind != TYPE_VECTOR || flatten_right->type_kind != TYPE_VECTOR) return false;

	return type_is_integer(flatten_left->array.base) && type_is_integer(flatten_right->array.base);
}

static inline bool expr_both_any_integer_or_integer_bool_vector(Expr *left, Expr *right)
{
	Type *flatten_left = type_flatten(left->type);
	Type *flatten_right = type_flatten(right->type);
	if (type_is_integer(flatten_left) && type_is_integer(flatten_right)) return true;

	if (flatten_left->type_kind != TYPE_VECTOR || flatten_right->type_kind != TYPE_VECTOR) return false;

	return type_is_integer_or_bool_kind(flatten_left->array.base) && type_is_integer_or_bool_kind(flatten_right->array.base);
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

CondResult sema_check_comp_time_bool(SemaContext *context, Expr *expr)
{
	CondResult result = COND_MISSING;
	if (!sema_analyse_cond_expr(context, expr, &result)) return COND_MISSING;
	if (result == COND_MISSING)
	{
		SEMA_ERROR(expr, "Compile time evaluation requires a compile time constant value.");
	}
	return result;
}


static bool sema_binary_is_expr_lvalue(SemaContext *context, Expr *top_expr, Expr *expr, bool *failed_ref)
{
	switch (expr->expr_kind)
	{
		case EXPR_SWIZZLE:
			if (failed_ref) goto FAILED_REF;
			RETURN_SEMA_ERROR(expr, "You cannot use swizzling to assign to multiple elements, use element-wise assign instead.");
		case EXPR_LAMBDA:
		case EXPR_EMBED:
			if (failed_ref) goto FAILED_REF;
			RETURN_SEMA_ERROR(expr, "This expression is a value and cannot be assigned to.");
		case EXPR_CT_IDENT:
			return true;
		case EXPR_EXT_TRUNC:
		case EXPR_INT_TO_BOOL:
		case EXPR_DISCARD:
			if (failed_ref) goto FAILED_REF;
			goto ERR;
		case EXPR_OTHER_CONTEXT:
			return sema_binary_is_expr_lvalue(context, top_expr, expr->expr_other_context.inner, failed_ref);
		case EXPR_IDENTIFIER:
		{
			Decl *decl = expr->identifier_expr.decl;
			if (decl->decl_kind != DECL_VAR)
			{
				if (failed_ref) goto FAILED_REF;
				RETURN_SEMA_ERROR(top_expr, "You cannot assign a value to %s.", decl_to_a_name(decl));
			}
			if (decl->var.kind == VARDECL_CONST)
			{
				if (failed_ref) goto FAILED_REF;
				RETURN_SEMA_ERROR(top_expr, "You cannot assign to a constant.");
			}
			decl = decl_raw(decl);
			switch (decl->var.kind)
			{
				case VARDECL_PARAM_REF:
					if (failed_ref) goto FAILED_REF;
					RETURN_SEMA_ERROR(top_expr, "You cannot assign to a ref parameter.");
				case VARDECL_LOCAL_CT:
				case VARDECL_LOCAL_CT_TYPE:
				case VARDECL_LOCAL:
				case VARDECL_GLOBAL:
				case VARDECL_PARAM:
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
				if (failed_ref) goto FAILED_REF;
				RETURN_SEMA_ERROR(top_expr, "You cannot assign to a dereferenced optional.");
			}
			return true;
		case EXPR_BITACCESS:
		case EXPR_ACCESS:
			if (!sema_binary_is_expr_lvalue(context, top_expr, expr->access_expr.parent, failed_ref)) return false;
			goto CHECK_OPTIONAL;
		case EXPR_SUBSCRIPT:
		case EXPR_SLICE:
		case EXPR_SUBSCRIPT_ASSIGN:
			goto CHECK_OPTIONAL;
		case EXPR_HASH_IDENT:
			if (failed_ref) goto FAILED_REF;
			RETURN_SEMA_ERROR(top_expr, "You cannot assign to an unevaluated expression.");
		case EXPR_EXPRESSION_LIST:
			if (!vec_size(expr->expression_list)) goto ERR;
			return sema_binary_is_expr_lvalue(context, top_expr, VECLAST(expr->expression_list), failed_ref);
		case EXPR_CONST:
			if (failed_ref) goto FAILED_REF;
			RETURN_SEMA_ERROR(top_expr, "You cannot assign to a constant expression.");
		case EXPR_POISONED:
		case EXPR_ADDR_CONVERSION:
		case EXPR_ANYSWITCH:
		case EXPR_ASM:
		case EXPR_BENCHMARK_HOOK:
		case EXPR_BINARY:
		case EXPR_BITASSIGN:
		case EXPR_BUILTIN:
		case EXPR_BUILTIN_ACCESS:
		case EXPR_CALL:
		case EXPR_CAST:
		case EXPR_CATCH_UNWRAP:
		case EXPR_COMPILER_CONST:
		case EXPR_COMPOUND_LITERAL:
		case EXPR_COND:
		case EXPR_CT_AND_OR:
		case EXPR_CT_APPEND:
		case EXPR_CT_ARG:
		case EXPR_CT_CALL:
		case EXPR_CT_CASTABLE:
		case EXPR_CT_CONCAT:
		case EXPR_CT_DEFINED:
		case EXPR_CT_EVAL:
		case EXPR_CT_IS_CONST:
		case EXPR_DECL:
		case EXPR_DEFAULT_ARG:
		case EXPR_DESIGNATED_INITIALIZER_LIST:
		case EXPR_DESIGNATOR:
		case EXPR_EXPR_BLOCK:
		case EXPR_FORCE_UNWRAP:
		case EXPR_GENERIC_IDENT:
		case EXPR_INITIALIZER_LIST:
		case EXPR_LAST_FAULT:
		case EXPR_MACRO_BLOCK:
		case EXPR_MACRO_BODY:
		case EXPR_MACRO_BODY_EXPANSION:
		case EXPR_MAKE_ANY:
		case EXPR_MAKE_SLICE:
		case EXPR_MEMBER_GET:
		case EXPR_NAMED_ARGUMENT:
		case EXPR_NOP:
		case EXPR_OPERATOR_CHARS:
		case EXPR_OPTIONAL:
		case EXPR_POINTER_OFFSET:
		case EXPR_POST_UNARY:
		case EXPR_PTR_ACCESS:
		case EXPR_ENUM_FROM_ORD:
		case EXPR_SLICE_LEN:
		case EXPR_FLOAT_TO_INT:
		case EXPR_INT_TO_FLOAT:
		case EXPR_INT_TO_PTR:
		case EXPR_PTR_TO_INT:
		case EXPR_RECAST:
		case EXPR_RETHROW:
		case EXPR_RETVAL:
		case EXPR_RVALUE:
		case EXPR_SLICE_ASSIGN:
		case EXPR_SLICE_COPY:
		case EXPR_SPLAT:
		case EXPR_STRINGIFY:
		case EXPR_TERNARY:
		case EXPR_TEST_HOOK:
		case EXPR_TRY_UNWRAP:
		case EXPR_TRY_UNWRAP_CHAIN:
		case EXPR_TYPECALL:
		case EXPR_TYPEID:
		case EXPR_TYPEID_INFO:
		case EXPR_TYPEINFO:
		case EXPR_VASPLAT:
		case EXPR_ANYFAULT_TO_FAULT:
		case EXPR_VECTOR_FROM_ARRAY:
		case EXPR_VECTOR_TO_ARRAY:
		case EXPR_SLICE_TO_VEC_ARRAY:
		case EXPR_SCALAR_TO_VECTOR:
		case EXPR_SUBSCRIPT_ADDR:
			goto ERR;
	}
	UNREACHABLE
ERR:
	if (failed_ref) goto FAILED_REF;
	RETURN_SEMA_ERROR(top_expr, "An assignable expression, like a variable, was expected here.");
CHECK_OPTIONAL:
	if (IS_OPTIONAL(expr))
	{
		if (failed_ref) goto FAILED_REF;
		RETURN_SEMA_ERROR(top_expr, "You cannot assign to an optional value.");
	}
	return true;
FAILED_REF:
	*failed_ref = true;
	return false;
}

static bool expr_may_ref(Expr *expr)
{
	switch (expr->expr_kind)
	{
		case EXPR_SWIZZLE:
		case EXPR_LAMBDA:
		case EXPR_CT_IDENT:
		case EXPR_EMBED:
		case EXPR_DEFAULT_ARG:
		case EXPR_TYPECALL:
		case EXPR_MEMBER_GET:
		case EXPR_EXT_TRUNC:
		case EXPR_PTR_ACCESS:
		case EXPR_VECTOR_TO_ARRAY:
		case EXPR_SLICE_TO_VEC_ARRAY:
		case EXPR_SCALAR_TO_VECTOR:
		case EXPR_ENUM_FROM_ORD:
		case EXPR_FLOAT_TO_INT:
		case EXPR_INT_TO_FLOAT:
		case EXPR_INT_TO_PTR:
		case EXPR_PTR_TO_INT:
		case EXPR_SLICE_LEN:
		case EXPR_VECTOR_FROM_ARRAY:
		case EXPR_ANYFAULT_TO_FAULT:
		case EXPR_INT_TO_BOOL:
		case EXPR_RVALUE:
		case EXPR_RECAST:
		case EXPR_DISCARD:
		case EXPR_ADDR_CONVERSION:
		case EXPR_MAKE_SLICE:
			return false;
		case EXPR_OTHER_CONTEXT:
			return expr_may_ref(expr->expr_other_context.inner);
		case EXPR_SUBSCRIPT_ASSIGN:
			return true;
		case EXPR_IDENTIFIER:
		{
			Decl *decl = expr->identifier_expr.decl;
			if (decl->decl_kind != DECL_VAR) return false;
			if (decl->var.kind == VARDECL_CONST) return false;
			decl = decl_raw(decl);
			switch (decl->var.kind)
			{
				case VARDECL_LOCAL_CT:
				case VARDECL_LOCAL_CT_TYPE:
				case VARDECL_LOCAL:
				case VARDECL_GLOBAL:
				case VARDECL_PARAM:
				case VARDECL_PARAM_CT:
				case VARDECL_PARAM_CT_TYPE:
					return true;
				case VARDECL_PARAM_REF:
				case VARDECL_CONST:
				case VARDECL_PARAM_EXPR:
				case VARDECL_MEMBER:
				case VARDECL_BITMEMBER:
					return false;
				case VARDECL_UNWRAPPED:
				case VARDECL_ERASE:
				case VARDECL_REWRAPPED:
					UNREACHABLE
			}
			UNREACHABLE
		}
		case EXPR_UNARY:
			return expr->unary_expr.operator == UNARYOP_DEREF;
		case EXPR_BITACCESS:
		case EXPR_ACCESS:
			return expr_may_ref(expr->access_expr.parent);
		case EXPR_SUBSCRIPT:
		case EXPR_SLICE:
		case EXPR_SUBSCRIPT_ADDR:
			return true;
		case EXPR_HASH_IDENT:
			return false;
		case EXPR_EXPRESSION_LIST:
			if (!vec_size(expr->expression_list)) return false;
			return expr_may_ref(VECLAST(expr->expression_list));
		case EXPR_ANYSWITCH:
		case EXPR_ASM:
		case EXPR_BENCHMARK_HOOK:
		case EXPR_BINARY:
		case EXPR_BITASSIGN:
		case EXPR_BUILTIN:
		case EXPR_BUILTIN_ACCESS:
		case EXPR_CALL:
		case EXPR_CAST:
		case EXPR_CATCH_UNWRAP:
		case EXPR_COMPILER_CONST:
		case EXPR_COMPOUND_LITERAL:
		case EXPR_COND:
		case EXPR_CONST:
		case EXPR_CT_AND_OR:
		case EXPR_CT_APPEND:
		case EXPR_CT_ARG:
		case EXPR_CT_CALL:
		case EXPR_CT_CASTABLE:
		case EXPR_CT_CONCAT:
		case EXPR_CT_DEFINED:
		case EXPR_CT_EVAL:
		case EXPR_CT_IS_CONST:
		case EXPR_DECL:
		case EXPR_DESIGNATED_INITIALIZER_LIST:
		case EXPR_DESIGNATOR:
		case EXPR_EXPR_BLOCK:
		case EXPR_FORCE_UNWRAP:
		case EXPR_GENERIC_IDENT:
		case EXPR_INITIALIZER_LIST:
		case EXPR_LAST_FAULT:
		case EXPR_MACRO_BLOCK:
		case EXPR_MACRO_BODY:
		case EXPR_MACRO_BODY_EXPANSION:
		case EXPR_NAMED_ARGUMENT:
		case EXPR_NOP:
		case EXPR_OPERATOR_CHARS:
		case EXPR_OPTIONAL:
		case EXPR_POINTER_OFFSET:
		case EXPR_POISONED:
		case EXPR_POST_UNARY:
		case EXPR_RETHROW:
		case EXPR_RETVAL:
		case EXPR_SLICE_ASSIGN:
		case EXPR_SLICE_COPY:
		case EXPR_SPLAT:
		case EXPR_STRINGIFY:
		case EXPR_TERNARY:
		case EXPR_TEST_HOOK:
		case EXPR_TRY_UNWRAP:
		case EXPR_TRY_UNWRAP_CHAIN:
		case EXPR_TYPEID:
		case EXPR_TYPEID_INFO:
		case EXPR_TYPEINFO:
		case EXPR_VASPLAT:
		case EXPR_MAKE_ANY:
			return false;
	}
	UNREACHABLE
}

bool sema_expr_check_assign(SemaContext *context, Expr *expr, bool *failed_ref)
{
	Expr *inner;
	if (!sema_binary_is_expr_lvalue(context, expr, expr, failed_ref)) return false;
	if (expr->expr_kind == EXPR_SUBSCRIPT)
	{
		inner = exprptr(expr->subscript_expr.expr);
		if (inner->expr_kind == EXPR_IDENTIFIER) inner->identifier_expr.decl->var.is_written = true;
		goto CHECK_INNER;
	}
	if (expr->expr_kind == EXPR_BITACCESS || expr->expr_kind == EXPR_ACCESS) expr = expr->access_expr.parent;
	if (expr->expr_kind == EXPR_IDENTIFIER)
	{
		expr->identifier_expr.decl->var.is_written = true;
	}
	if (expr->expr_kind != EXPR_UNARY) return true;
	inner = expr->inner_expr;
CHECK_INNER:
	if (inner->expr_kind != EXPR_IDENTIFIER) return true;
	Decl *decl = inner->identifier_expr.decl;
	if (decl->decl_kind != DECL_VAR) return true;
	if (!decl->var.in_param) return true;
	RETURN_SEMA_ERROR(inner, "'in' parameters may not be assigned to.");
}

static inline bool sema_cast_ident_rvalue(SemaContext *context, Expr *expr)
{
	Decl *decl = expr->identifier_expr.decl;
	decl = decl_flatten(decl);
	switch (decl->decl_kind)
	{
		case DECL_FNTYPE:
		case DECL_FUNC:
			SEMA_ERROR(expr, "Expected function followed by (...) or prefixed by &.");
			return expr_poison(expr);
		case DECL_MACRO:
			SEMA_ERROR(expr, "Expected a macro followed by (...).");
			return expr_poison(expr);
		case DECL_FAULTVALUE:
			SEMA_ERROR(expr, "Did you forget a '!' after '%s'?", decl->name);
			return expr_poison(expr);
		case DECL_ENUM_CONSTANT:
			// This can't happen, inferred identifiers are folded to consts they are never identifiers.
			UNREACHABLE;
		case DECL_VAR:
			break;
		case DECL_DISTINCT:
		case DECL_TYPEDEF:
		case DECL_DECLARRAY:
		case DECL_BODYPARAM:
		case DECL_CT_INCLUDE:
		case DECL_CT_EXEC:
		case DECL_GLOBALS:
		case DECL_ERASED:
		case DECL_IMPORT:
		case DECL_ATTRIBUTE:
		case DECL_CT_ASSERT:
		case DECL_DEFINE:
		case DECL_CT_ECHO:
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
		case DECL_INTERFACE:
			SEMA_ERROR(expr, "Expected an interface to be followed by '.' when used as an expression.");
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
	}
	switch (decl->var.kind)
	{
		case VARDECL_CONST:
		case VARDECL_GLOBAL:
		case VARDECL_LOCAL:
		case VARDECL_LOCAL_CT:
		case VARDECL_LOCAL_CT_TYPE:
			if (decl->in_init)
			{
				RETURN_SEMA_ERROR(expr, "This looks like the initialization of the variable was circular.");
			}
			break;
		default:
			break;
	}
	switch (decl->var.kind)
	{
		case VARDECL_CONST:
			if (decl->is_extern) return true;
			if (type_is_abi_aggregate(decl->type)) return true;
			expr_replace(expr, copy_expr_single(decl->var.init_expr));
			if (!sema_analyse_expr(context, expr)) return false;
			if (!sema_cast_const(expr) && !expr_is_runtime_const(expr))
			{
				RETURN_SEMA_ERROR(decl->var.init_expr, "The expression must be constant.");
			}
			return true;
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

static inline bool sema_expr_analyse_ternary(SemaContext *context, Type *infer_type, Expr *expr)
{
	Expr *left = exprptrzero(expr->ternary_expr.then_expr);
	Expr *cond = exprptr(expr->ternary_expr.cond);
	CondResult path = COND_MISSING;
	// Normal
	if (left)
	{
		if (!sema_analyse_cond_expr(context, cond, &path)) return expr_poison(expr);
		if (!sema_analyse_maybe_dead_expr(context, left, path == COND_FALSE, infer_type)) return expr_poison(expr);
	}
	else
	{
		// Elvis
		if (!sema_analyse_expr(context, cond)) return expr_poison(expr);
		ASSERT_SPAN(expr, cond && cond->type);
		Type *type = cond->type->canonical;
		if (type->type_kind != TYPE_BOOL && !may_cast(context, cond, type_bool, true, true))
		{
			RETURN_SEMA_ERROR(cond, "Cannot convert expression to boolean.");
		}
		if (expr_is_const(cond))
		{
			Expr *copy = copy_expr_single(cond);
			cast_no_check(context, copy, type_bool, false);
			ASSERT_SPAN(expr, expr_is_const_bool(copy));
			path = copy->const_expr.b ? COND_TRUE : COND_FALSE;
			expr->ternary_expr.then_expr = exprid(cond);
			expr->ternary_expr.cond = exprid(copy);
			left = cond;
		}
		else
		{
			// From foo :? bar
			// (bool)($temp = foo) ? $temp : bar
			Decl *temp = decl_new_generated_var(cond->type, VARDECL_LOCAL, cond->span);
			temp->var.init_expr = expr_copy(cond);
			cond->expr_kind = EXPR_DECL;
			cond->decl_expr = temp;
			cond->resolve_status = RESOLVE_NOT_DONE;
			if (!sema_analyse_expr(context, cond)) return false;
			if (!cast_explicit(context, cond, type_bool)) return false;
			expr->ternary_expr.then_expr = exprid(left = expr_variable(temp));
			if (!sema_analyse_expr(context, left)) return false;
		}
	}

	Expr *right = exprptr(expr->ternary_expr.else_expr);
	if (!sema_analyse_maybe_dead_expr(context, right, path == COND_TRUE, infer_type)) return expr_poison(expr);

	bool is_optional = false;
	Type *left_canonical = left->type->canonical;
	Type *right_canonical = right->type->canonical;
	if (left_canonical != right_canonical)
	{
		Type *max;
		max = type_find_max_type(type_no_optional(left_canonical), type_no_optional(right_canonical));
		if (!max)
		{
			SEMA_ERROR(expr, "Cannot find a common parent type of '%s' and '%s'",
					   type_to_error_string(left->type), type_to_error_string(right->type));
			return false;
		}
		Type *no_fail_max = type_no_optional(max);
		if (!cast_implicit_binary(context, left, no_fail_max, false) || !cast_implicit_binary(context, right, no_fail_max,
																							  false)) return false;
	}

	switch (sema_resolve_storage_type(context, left->type))
	{
		case STORAGE_ERROR:
			return false;
		case STORAGE_COMPILE_TIME:
			if (left->type == type_untypedlist)
			{
				RETURN_SEMA_ERROR(expr, "The ternary would be an 'untyped list', you need to explicitly type one or both branches to a runtime type.");
			}
			RETURN_SEMA_ERROR(expr, "A ternary must always return a runtime type, but it was %s.", type_quoted_error_string(left_canonical));
			break;
		default:
			break;
	}
	if (path != COND_MISSING)
	{
		expr_replace(expr, path == COND_TRUE ? left : right);
	}

	expr_binary_unify_failability(expr, left, right);
	return true;
}

static inline bool sema_expr_analyse_enum_constant(SemaContext *context, Expr *expr, const char *name, Decl *decl)
{
	Decl *enum_constant = decl_find_enum_constant(decl, name);
	if (!enum_constant) return false;

	if (!sema_analyse_decl(context, decl)) return false;

	ASSERT_SPAN(expr, enum_constant->resolve_status == RESOLVE_DONE);
	expr->type = decl->type;

	expr->expr_kind = EXPR_CONST;
	expr->const_expr.const_kind = enum_constant->decl_kind == DECL_ENUM_CONSTANT ? CONST_ENUM : CONST_ERR;
	expr->const_expr.enum_err_val = enum_constant;
	expr->resolve_status = RESOLVE_DONE;
	return true;
}


static inline bool sema_identifier_find_possible_inferred(SemaContext *context, Type *to, Expr *expr)
{
	if (to->canonical->type_kind != TYPE_ENUM && to->canonical->type_kind != TYPE_FAULTTYPE) return false;
	Decl *parent_decl = to->canonical->decl;
	if (!decl_ok(parent_decl))
	{
		expr_poison(expr);
		return true;
	}
	switch (parent_decl->decl_kind)
	{
		case DECL_ENUM:
		case DECL_FAULT:
			return sema_expr_analyse_enum_constant(context, expr, expr->identifier_expr.ident, parent_decl);
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

	ASSERT_SPAN(expr, expr && expr->identifier_expr.ident);
	DEBUG_LOG("Resolving identifier '%s'", expr->identifier_expr.ident);

	ASSERT_SPAN(expr, expr->resolve_status != RESOLVE_DONE);
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
		if (sema_identifier_find_possible_inferred(context, to, expr)) return expr_ok(expr);
	}

	Decl *decl = sema_find_path_symbol(context, expr->identifier_expr.ident, expr->identifier_expr.path);

	// Is this a broken decl?
	if (!decl_ok(decl)) return false;

	// Rerun if we can't do inference.
	if (!decl)
	{
		decl = sema_resolve_symbol(context, expr->identifier_expr.ident, expr->identifier_expr.path, expr->span);
		(void)decl;
		ASSERT_SPAN(expr, !decl);
		return false;
	}

	if (decl_needs_prefix(decl))
	{
		if (!sema_analyse_decl(context, decl)) return false;
		if (decl->unit->module != context->unit->module && !expr->identifier_expr.path)
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
	sema_display_deprecated_warning_on_use(context, decl, expr->span);

	unit_register_external_symbol(context, decl);
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
					if (!expr_is_runtime_const(copy))
					{
						SEMA_ERROR(expr, "Constant value did not evaluate to a constant.");
						return false;
					}
					expr_replace(expr, copy);
					return true;
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
	expr->identifier_expr = (ExprIdentifier) { .decl = decl };
	expr->type = decl->type;
	return true;
}


static inline bool sema_expr_analyse_ct_identifier(SemaContext *context, Expr *expr, CheckType check)
{
	ASSERT0(expr && expr->ct_ident_expr.identifier);

	DEBUG_LOG("Resolving identifier '%s'", expr->ct_ident_expr.identifier);
	Decl *decl = sema_resolve_symbol(context, expr->ct_ident_expr.identifier, NULL, expr->span);
	if (!decl)
	{
		return expr_poison(expr);
	}

	ASSERT_SPAN(expr, decl->decl_kind == DECL_VAR);
	ASSERT_SPAN(expr, decl->resolve_status == RESOLVE_DONE);

	decl->var.is_read = true;
	expr->ct_ident_expr.decl = decl;
	expr->type = decl->type;

	if (check != CHECK_LVALUE)
	{
		if (!sema_cast_ct_ident_rvalue(context, expr)) return false;
	}
	return true;
}


static inline bool sema_binary_analyse_subexpr(SemaContext *context, Expr *binary, Expr *left, Expr *right)
{
	// Special handling of f = FOO_BAR
	if (right->resolve_status != RESOLVE_DONE && right->expr_kind == EXPR_IDENTIFIER && right->identifier_expr.is_const)
	{
		if (!sema_analyse_expr(context, left)) return false;
		if (type_flatten(left->type)->type_kind == TYPE_ENUM)
		{
			return sema_analyse_inferred_expr(context, left->type, right);
		}
	}
	// Special handling of f = FOO_BAR
	if (left->resolve_status != RESOLVE_DONE && left->expr_kind == EXPR_IDENTIFIER && left->identifier_expr.is_const)
	{
		if (!sema_analyse_expr(context, right)) return false;
		if (type_flatten(right->type)->type_kind == TYPE_ENUM)
		{
			return sema_analyse_inferred_expr(context, right->type, left);
		}
	}
	if (right->expr_kind == EXPR_INITIALIZER_LIST)
	{
		if (!sema_analyse_expr(context, left)) return false;
		if (type_kind_is_any_vector(type_flatten(left->type)->type_kind))
		{
			return sema_analyse_inferred_expr(context, left->type, right);
		}
		return sema_analyse_expr(context, right);
	}
	if (left->expr_kind == EXPR_INITIALIZER_LIST)
	{
		if (!sema_analyse_expr(context, right)) return false;
		if (type_kind_is_any_vector(type_flatten(right->type)->type_kind))
		{
			return sema_analyse_inferred_expr(context, right->type, left);
		}
		return sema_analyse_expr(context, left);
	}
	return sema_analyse_expr(context, left) && sema_analyse_expr(context, right);
}

static inline bool sema_binary_analyse_arithmetic_subexpr(SemaContext *context, Expr *expr, const char *error, bool bool_and_bitstruct_is_allowed)
{
	Expr *left = exprptr(expr->binary_expr.left);
	Expr *right = exprptr(expr->binary_expr.right);

	// 1. Analyse both sides.
	if (!sema_binary_analyse_subexpr(context, expr, left, right)) return false;

	//if (!sema_binary_promote_top_down(context, expr, left, right)) return false;

	Type *left_type = type_no_optional(left->type)->canonical;
	Type *right_type = type_no_optional(right->type)->canonical;

	if (bool_and_bitstruct_is_allowed)
	{
		if (left_type->type_kind == TYPE_BITSTRUCT && left_type == right_type) return true;
		if (left_type == type_bool && right_type == type_bool) return true;
	}
	// 2. Perform promotion to a common type.
	return sema_binary_arithmetic_promotion(context, left, right, left_type, right_type, expr, error, bool_and_bitstruct_is_allowed);
}

static inline int sema_call_find_index_of_named_parameter(SemaContext *context, Decl **func_params, Expr *expr)
{
	const char *name = expr->named_argument_expr.name;
	FOREACH_IDX(i, Decl *, func_param, func_params)
	{
		if (func_param && func_param->name == name) return (int)i;
	}
	SEMA_ERROR(expr, "There's no parameter with the name '%s'.", name);
	return -1;
}

static inline bool sema_call_check_invalid_body_arguments(SemaContext *context, Expr *call, CalledDecl *callee)
{
	Expr *macro_body = exprptrzero(call->call_expr.macro_body);
	Decl **body_arguments = macro_body ? macro_body->macro_body_expr.body_arguments : NULL;
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
	if (macro_body && macro_body->macro_body_expr.body)
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
		ASSERT_SPAN(call, callee->macro);
		SEMA_ERROR(call, "Expected call to have a trailing statement, did you forget to add it?");
		return false;
	}

	// 4. No "body" and no block parameter, this is fine.
	return true;
}

INLINE bool sema_arg_is_pass_through_ref(Expr *expr)
{
	if (expr->expr_kind != EXPR_IDENTIFIER) return false;
	Decl *decl = expr->identifier_expr.decl;
	if (decl->decl_kind != DECL_VAR) return false;
	return decl->var.kind == VARDECL_PARAM_REF;
}

static bool sema_analyse_parameter(SemaContext *context, Expr *arg, Decl *param, Decl *definition, bool *optional_ref,
								   bool *no_match_ref, bool macro, bool is_method_target)
{
	VarDeclKind kind = param->var.kind;
	Type *type = param->type;
	// 16. Analyse a regular argument.
	switch (kind)
	{
		case VARDECL_PARAM_REF:
		{
			// &foo
			bool is_subscript = false;
			switch (arg->expr_kind)
			{
				case EXPR_SUBSCRIPT:
					if (type) break;
					arg->expr_kind = EXPR_SUBSCRIPT_ADDR;
					is_subscript = true;
					break;
				case EXPR_CT_IDENT:
					SEMA_ERROR(arg, "A compile time identifier cannot be passed as a reference parameter.");
					SEMA_NOTE(definition, "The definition is here.");
					return false;
				default:
					break;
			}
			if (!sema_analyse_expr_address(context, arg)) return false;
			if (sema_arg_is_pass_through_ref(arg) && !sema_expr_check_assign(context, arg, NULL))
			{
				SEMA_NOTE(definition, "The definition is here.");
				return false;
			}
			if (!is_subscript) expr_insert_addr(arg);
			*optional_ref |= IS_OPTIONAL(arg);
			if (!sema_call_check_contract_param_match(context, param, arg))
			{
				SEMA_NOTE(definition, "The definition is here.");
				return false;
			}
			switch (sema_resolve_storage_type(context, type))
			{
				case STORAGE_ERROR:
					return false;
				case STORAGE_NORMAL:
					break;
				default:
					RETURN_SEMA_ERROR(arg, "A value of type %s cannot be passed by reference.", type_quoted_error_string(type));
			}
			if (type && type->canonical != arg->type->canonical)
			{
				SEMA_ERROR(arg, "'%s' cannot be implicitly cast to '%s'.", type_to_error_string(arg->type), type_to_error_string(type));
				SEMA_NOTE(definition, "The definition is here.");
				return false;
			}
			if (!param->alignment)
			{
				if (arg->expr_kind == EXPR_IDENTIFIER)
				{
					param->alignment = arg->identifier_expr.decl->alignment;
				}
				else
				{
					if (!sema_set_alloca_alignment(context, arg->type, &param->alignment)) return false;
				}
			}
			break;
		}
		case VARDECL_PARAM:
			// foo
			if (!sema_analyse_expr_rhs(context, type, arg, true, no_match_ref, false)) return false;
			if (IS_OPTIONAL(arg)) *optional_ref = true;
			switch (sema_resolve_storage_type(context, arg->type))
			{
				case STORAGE_ERROR:
					return false;
				case STORAGE_NORMAL:
					break;
				case STORAGE_VOID:
				case STORAGE_WILDCARD:
					RETURN_SEMA_ERROR(arg, "A 'void' value cannot be passed as a parameter.");
				case STORAGE_COMPILE_TIME:
					RETURN_SEMA_ERROR(arg, "It is only possible to use %s as a compile time parameter.",
									  type_invalid_storage_type_name(arg->type));
				case STORAGE_UNKNOWN:
					RETURN_SEMA_ERROR(arg, "A value of type '%s' has no known size so cannot be "
										   "passed as a parameter, you can pass a pointer to it though.",
									  type_quoted_error_string(arg->type));
			}
			if (!sema_call_check_contract_param_match(context, param, arg))
			{
				SEMA_NOTE(definition, "The definition was here.");
				return false;
			}
			if (!param->alignment)
			{
				ASSERT0(macro && "Only in the macro case should we need to insert the alignment.");
				if (!sema_set_alloca_alignment(context, arg->type, &param->alignment)) return false;
			}
			break;
		case VARDECL_PARAM_EXPR:
			if (param->type)
			{
				if (!sema_analyse_expr_rhs(context, param->type, arg, true, NULL, false))
				{
					SEMA_NOTE(definition, "The definition is here.");
					return false;
				}
			}
			// #foo
			if (context->is_temp)
			{
				SemaContext *temp = context;
				context = MALLOCS(SemaContext);
				*context = *temp;
			}
			{
				Expr *inner = expr_copy(arg);
				arg->expr_kind = EXPR_OTHER_CONTEXT;
				arg->expr_other_context = (ExprOtherContext) { .context = context, .inner = inner };
				arg->resolve_status = RESOLVE_NOT_DONE;
			}
			break;
		case VARDECL_PARAM_CT:
			// $foo
			ASSERT0(macro);
			if (!sema_analyse_expr_rhs(context, type, arg, true, no_match_ref, false))
			{
				SEMA_NOTE(definition, "The definition is here.");
				return false;
			}
			if (!sema_cast_const(arg) && !expr_is_runtime_const(arg))
			{
				if (is_method_target)
				{
					RETURN_SEMA_FUNC_ERROR(definition, arg, "This method is only valid on a compile time constant value.");
				}
				RETURN_SEMA_FUNC_ERROR(definition, arg, "A compile time parameter must always be a constant, did you mistake it for a normal parameter?");
			}
			break;
		case VARDECL_PARAM_CT_TYPE:
			// $Foo
			if (!sema_analyse_expr_value(context, arg)) return false;
			if (arg->expr_kind != EXPR_TYPEINFO)
			{
				RETURN_SEMA_FUNC_ERROR(definition, arg, "A type, like 'int' or 'double' was expected for the parameter '%s'.", param->name);
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
	if (type_len_is_inferred(type))
	{
		param->type = type_no_optional(arg->type);
	}
	return true;
}

INLINE bool sema_set_default_argument(SemaContext *context, CalledDecl *callee, Expr *call, Decl *param,
									  bool *no_match_ref, Expr **expr_ref, Variadic variadic, bool has_named,
									  bool after_vaarg, int needed, Expr *prev, bool *optional)
{
	Expr *init_expr = param->var.init_expr;
	if (!init_expr) return true;
	Expr *arg = copy_expr_single(init_expr);
	bool parameter_checked = false;
	if (arg->resolve_status != RESOLVE_DONE)
	{
		SemaContext default_context;
		Type *rtype = NULL;
		SemaContext *new_context = context_transform_for_eval(context, &default_context, param->unit);
		bool success;
		SCOPE_START
			new_context->original_inline_line = context->original_inline_line ? context->original_inline_line
																			  : call->span.row;
			new_context->original_module = context->original_module;
			success = sema_analyse_parameter(new_context, arg, param, callee->definition, optional, no_match_ref,
											 callee->macro, false);
		SCOPE_END;
		sema_context_destroy(&default_context);
		if (no_match_ref && *no_match_ref) return true;
		if (!success)
		{
			RETURN_NOTE_FUNC_DEFINITION;
		}
		if (!success) return false;
		parameter_checked = true;
	}
	switch (param->var.kind)
	{
		case VARDECL_PARAM_CT:
		case VARDECL_PARAM_CT_TYPE:
		case VARDECL_PARAM_EXPR:
			*expr_ref = arg;
			if (parameter_checked) return true;
			return sema_analyse_parameter(context, arg, param, callee->definition, optional, no_match_ref,
			                              callee->macro, false);
		default:
			break;
	}
	Expr *function_scope_arg = expr_new(EXPR_DEFAULT_ARG, arg->span);
	function_scope_arg->resolve_status = RESOLVE_DONE;
	function_scope_arg->type = arg->type;
	function_scope_arg->default_arg_expr.inner = arg;
	function_scope_arg->default_arg_expr.loc = callee->call_location;
	*expr_ref = function_scope_arg;
	if (parameter_checked) return true;
	return sema_analyse_parameter(context, function_scope_arg, param, callee->definition, optional, no_match_ref,
								  callee->macro, false);
}


INLINE Expr **sema_splat_arraylike_insert(SemaContext *context, Expr **args, Expr *arg, ArraySize len, ArrayIndex index)
{
	args = sema_prepare_splat_insert(args, len, index);
	if (expr_is_const(arg))
	{
		for (ArrayIndex i = 0; i < len; i++)
		{
			Expr *expr = expr_copy(arg);
			Expr *subscript = expr_new_expr(EXPR_SUBSCRIPT, expr);
			subscript->subscript_expr.index.expr = exprid(expr_new_const_int(arg->span, type_usz, i));
			subscript->subscript_expr.expr = exprid(expr);
			args[i + index] = subscript;
		}
		return args;
	}
	Decl *temp = decl_new_generated_var(arg->type, VARDECL_LOCAL, arg->span);
	Expr *decl_expr = expr_generate_decl(temp, arg);
	Expr *list = expr_new_expr(EXPR_EXPRESSION_LIST, arg);
	vec_add(list->expression_list, decl_expr);
	Expr *subscript = expr_new_expr(EXPR_SUBSCRIPT, arg);
	subscript->subscript_expr.index.expr = exprid(expr_new_const_int(arg->span, type_usz, 0));
	subscript->subscript_expr.expr = exprid(expr_variable(temp));
	vec_add(list->expression_list, subscript);
	if (!sema_analyse_expr(context, list)) return NULL;
	args[index] = list;
	for (ArrayIndex i = 1; i < len; i++)
	{
		subscript = expr_new_expr(EXPR_SUBSCRIPT, arg);
		subscript->subscript_expr.index.expr = exprid(expr_new_const_int(arg->span, type_usz, i));
		subscript->subscript_expr.expr = exprid(expr_variable(temp));
		args[index + i] = subscript;
	}
	return args;
}

static inline ArrayIndex sema_len_from_expr(Expr *expr)
{
	Type *type = type_flatten(expr->type);
	switch (type->type_kind)
	{
		case TYPE_VECTOR:
		case TYPE_ARRAY:
			return type->array.len;
		case TYPE_UNTYPED_LIST:
			return sema_len_from_const(expr);
		case TYPE_SLICE:
			break;
		default:
			return -1;
	}
	if (sema_cast_const(expr))
	{
		return sema_len_from_const(expr);
	}
	if (expr->expr_kind != EXPR_SLICE) return -1;
	return range_const_len(&expr->slice_expr.range);
}

INLINE bool sema_call_evaluate_arguments(SemaContext *context, CalledDecl *callee, Expr *call, bool *optional,
										 bool *no_match_ref)
{
	// Check body arguments (for macro calls, or possibly broken
	if (!sema_call_check_invalid_body_arguments(context, call, callee)) return false;
	// Pick out all the arguments and parameters.
	Signature *sig = callee->signature;
	unsigned vaarg_index = sig->vararg_index;
	Variadic variadic = sig->variadic;
	Decl **decl_params = callee->params;

	// If this is a type call, then we have an implicit first argument.
	if (callee->struct_var)
	{
		vec_insert_first(call->call_expr.arguments, callee->struct_var);
		call->call_expr.is_type_method = true;
		ASSERT_SPAN(call, !call->call_expr.is_pointer_call);
	}

	// Zero out all argument slots.
	unsigned func_param_count = vec_size(decl_params);

	// We might have a typed variadic call e.g. foo(int, double...)
	// get that type.
	Type *variadic_type = NULL;
	Type *variadic_slot_type = NULL;
	if (variadic == VARIADIC_TYPED || variadic == VARIADIC_ANY)
	{
		// 7a. The parameter type is <type>[], so we get the <type>
		variadic_slot_type = decl_params[vaarg_index]->type;
		ASSERT_SPAN(call, variadic_slot_type->type_kind == TYPE_SLICE);
		variadic_type = variadic_slot_type->array.base;
	}

	Expr **args = call->call_expr.arguments;
	unsigned num_args = vec_size(args);
	Decl **params = callee->params;

	ASSERT0(func_param_count < MAX_PARAMS);
	Expr **actual_args = VECNEW(Expr*, func_param_count);
	for (unsigned i = 0; i < func_param_count; i++)
	{
		vec_add(actual_args, NULL);
	}

	// 2. Loop through the parameters.
	bool has_named = false;
	bool found_splat = false;
	ArrayIndex last_index = -1;
	Expr *last_named_arg = INVALID_PTR;
	Expr *last = NULL;
	int needed = func_param_count - (callee->struct_var ? 1 : 0);
	for (unsigned i = 0; i < num_args; i++)
	{
		Expr *arg = args[i];
		if (i > 0) last = args[i - 1];
		ASSERT0(expr_ok(arg));
		if (arg->expr_kind == EXPR_VASPLAT && context->current_macro)
		{
			Expr **new_args = sema_vasplat_insert(context, args, arg, i);
			if (!new_args) return false;
			args = new_args;
			i--;
			num_args = vec_size(args);
			continue;
		}
		if (arg->expr_kind == EXPR_SPLAT)
		{
			Expr *inner = arg->inner_expr;

			if (!sema_analyse_expr(context, inner)) return false;

			// Let's try fit up a slice to the in the vaslot
			if (variadic_type && i == vaarg_index)
			{
				// Is it not the last and not a named argument, then we do a normal splat.
				if (i + 1 < num_args && args[i + 1]->expr_kind != EXPR_NAMED_ARGUMENT) goto SPLAT_NORMAL;

				// Convert an array/vector to an address of an array.
				Expr *inner_new = inner;
				if (type_is_arraylike(inner->type))
				{
					inner_new = expr_copy(inner);
					expr_insert_addr(inner_new);
				}
				if (!cast_implicit_silent(context, inner_new, variadic_slot_type, false)) goto SPLAT_NORMAL;
				if (inner != inner_new) expr_replace(inner, inner_new);
				// We splat it in the right spot!
				call->call_expr.va_is_splat = true;
				*optional |= IS_OPTIONAL(inner);
				call->call_expr.vasplat = inner;
				continue;
			}
SPLAT_NORMAL:;
			Type *flat = type_flatten(inner->type);
			switch (flat->type_kind)
			{
				case TYPE_VECTOR:
				case TYPE_ARRAY:
				case TYPE_SLICE:
				case TYPE_UNTYPED_LIST:
					// These may be splatted
					break;
				default:
					RETURN_SEMA_ERROR(arg, "An argument of type %s cannot be splatted.",
									  type_quoted_error_string(inner->type));
			}
			// This is the fallback: just splat like vasplat:
			ArrayIndex len = sema_len_from_expr(inner);
			if (len == -1) RETURN_SEMA_ERROR(arg, "Splat may not be used with raw varargs if the length is not known, but if you slice it to a constant length it will work (e.g '...val[:2]')");
			if (len == 0 && !expr_is_const(arg))
			{
				RETURN_SEMA_ERROR(arg, "A non-constant zero size splat cannot be used with raw varargs.");
			}
			Expr **new_args = sema_splat_arraylike_insert(context, args, inner, len, i);
			if (!new_args) return false;
			args = new_args;
			i--;
			num_args = vec_size(args);
			continue;
		}
		if (arg->expr_kind == EXPR_NAMED_ARGUMENT)
		{
			// Find the location of the parameter.
			int index = sema_call_find_index_of_named_parameter(context, params, arg);

			// If it's not found then this is an error. Let's not invoke nomatch
			if (index < 0) return false;

			// We have named parameters, that will add some restrictions.
			has_named = true;

			Decl *param = params[index];
			// 8d. We might actually be finding the typed vararg at the end,
			//     this is an error.
			if (param->var.vararg)
			{
				RETURN_SEMA_FUNC_ERROR(callee->definition, arg, "Vararg parameters may not be named parameters, "
																"use normal parameters instead.", param->name);
			}

			// 8e. We might have already set this parameter, that is not allowed.
			if (actual_args[index] && actual_args[index]->expr_kind != EXPR_DEFAULT_ARG)
			{
				RETURN_SEMA_ERROR(arg, "The parameter '%s' was already set.", param->name);
			}

			if (last_index > index)
			{
				SEMA_ERROR(arg, "Named arguments must always be declared in order.");
				SEMA_NOTE(last_named_arg, "Place it before this argument.");
				return false;
			}
			if (last_index == -1) last_index = i - 1;
			for (int j = last_index + 1; j < index; j++)
			{
				if (j == vaarg_index) continue;
				if (!sema_set_default_argument(context, callee, call,
											   params[j], no_match_ref,
											   &actual_args[j],
											   variadic, has_named,
											   j > vaarg_index, needed, last, optional))
				{
					return false;
				}
			}
			last_index = index;
			last_named_arg = arg;

			actual_args[index] = arg->named_argument_expr.value;
			if (!sema_analyse_parameter(context, actual_args[index], param, callee->definition, optional, no_match_ref,
										callee->macro, false)) return false;
			continue;
		}
		if (call->call_expr.va_is_splat)
		{
			if (no_match_ref) goto NO_MATCH_REF;
			RETURN_SEMA_FUNC_ERROR(callee->definition, arg,
								   "This looks like an argument after a splatted variable, which "
								   "isn't allowed. Did you add too many arguments?");
		}
		if (has_named)
		{
			RETURN_SEMA_FUNC_ERROR(callee->definition, args[i - 1],
								   "Named arguments must be placed after positional arguments.");
		}

		// 11. We might have a typed variadic argument.
		if (variadic == VARIADIC_NONE && i >= func_param_count)
		{
			// 15. We have too many parameters...
			if (no_match_ref) goto NO_MATCH_REF;
			RETURN_SEMA_FUNC_ERROR(callee->definition, arg,
								   "This argument would exceed the number of parameters, "
								   "did you add too many arguments?");
		}

		// 10. If we exceed the function parameter count (remember we reduced this by one
		//     in the case of typed vararg) we're now in a variadic list.
		if (variadic != VARIADIC_NONE && i >= vaarg_index)
		{
			switch (variadic)
			{
				case VARIADIC_RAW:
					// Only analyse for non-macro
					if (!callee->macro)
					{
						if (!sema_analyse_expr(context, arg)) return false;
						switch (sema_resolve_storage_type(context, arg->type))
						{
							case STORAGE_ERROR:
								return false;
							case STORAGE_NORMAL:
								break;
							default:
								RETURN_SEMA_ERROR(arg, "A value of type %s cannot be passed as a raw variadic argument.",
								                  type_quoted_error_string(arg->type));
						}
						cast_promote_vararg(context, arg);
					}
					// Set the argument at the location.
					*optional |= IS_OPTIONAL(arg);
					break;
				case VARIADIC_ANY:
					if (!sema_analyse_expr(context, arg)) return false;
					Type *type = arg->type;
					switch (sema_resolve_storage_type(context, arg->type))
					{
						case STORAGE_ERROR:
							return false;
						case STORAGE_NORMAL:
							break;
						default:
							RETURN_SEMA_ERROR(arg, "A value of type %s cannot be passed as a variadic argument.",
							                  type_quoted_error_string(type));
					}
					expr_insert_addr(arg);
					FALLTHROUGH;
				case VARIADIC_TYPED:
					if (!sema_analyse_expr_rhs(context, variadic_type, arg, true, no_match_ref, false)) return false;
					*optional |= IS_OPTIONAL(arg);
					break;
				case VARIADIC_NONE:
					UNREACHABLE
			}
			vec_add(call->call_expr.varargs, arg);
			continue;
		}
		if (!sema_analyse_parameter(context, arg, params[i], callee->definition, optional, no_match_ref, callee->macro, callee->struct_var && i == 0)) return false;
		actual_args[i] = arg;
	}
	if (num_args) last = args[num_args - 1];
	call->call_expr.arguments = args;
	// 17. Set default values.
	for (unsigned i = 0; i < func_param_count; i++)
	{
		// 17a. Assigned a value - skip
		if (actual_args[i]) continue;
		if (i == vaarg_index && variadic != VARIADIC_NONE) continue;

		if (!sema_set_default_argument(context, callee, call, params[i], no_match_ref, &actual_args[i],
									   variadic, has_named, i > vaarg_index, needed,
									   last, optional)) return false;
	}
	for (int i = 0; i < func_param_count; i++)
	{
		if (i == vaarg_index) continue;
		if (actual_args[i]) continue;
		// Argument missing, that's bad.
		Decl *param = params[i];
		if (no_match_ref)
		{
			*no_match_ref = true;
			return true;
		}
		if (!has_named || !param->name)
		{
			if (vec_size(callee->params) == 1)
			{
				if (param->type)
				{
					RETURN_SEMA_FUNC_ERROR(callee->definition, call,
										   "This call expected a parameter of type %s, did you forget it?",
										   type_quoted_error_string(param->type));
				}
				RETURN_SEMA_FUNC_ERROR(callee->definition, call, "This call expected a parameter, did you forget it?");
			}
			if (variadic != VARIADIC_NONE && i > vaarg_index)
			{
				print_error_after(last->span, "Expected '%s: ...' after this argument.", param->name);
				RETURN_NOTE_FUNC_DEFINITION;
			}
			if (!num_args)
			{
				RETURN_SEMA_FUNC_ERROR(callee->definition, call, "'%s' expects %d parameter(s), but none was provided.",
									   callee->name, needed);
			}
			if (!last) last = args[0];
			int more_needed = func_param_count - i;
			RETURN_SEMA_FUNC_ERROR(callee->definition, last,
								   "Expected %d more %s after this one, did you forget %s?",
								   more_needed, more_needed == 1 ? "argument" : "arguments", more_needed == 1 ? "it" : "them");
		}
		RETURN_SEMA_FUNC_ERROR(callee->definition, call, "The parameter '%s' must be set, did you forget it?", param->name);
	}
	call->call_expr.arguments = actual_args;
	return true;
NO_MATCH_REF:
	*no_match_ref = true;
	return false;
}

static inline bool sema_call_check_contract_param_match(SemaContext *context, Decl *param, Expr *expr)
{
	if (param->var.not_null && expr_is_const_pointer(expr) && !expr->const_expr.ptr)
	{
		RETURN_SEMA_ERROR(expr, "You may not pass null to a '&' parameter.");
	}
	if (expr->expr_kind == EXPR_UNARY && expr->unary_expr.expr->expr_kind == EXPR_IDENTIFIER)
	{
		if (expr->unary_expr.expr->identifier_expr.decl->var.kind == VARDECL_CONST && param->var.out_param)
		{
			SEMA_ERROR(expr, "A const parameter may not be passed into a function or macro as an 'out' argument.");
			return false;
		}
	}
	if (expr->expr_kind != EXPR_IDENTIFIER) return true;
	Decl *ident = expr->identifier_expr.decl;
	if (ident->decl_kind != DECL_VAR) return true;
	if (ident->var.out_param && param->var.in_param)
	{
		SEMA_ERROR(expr, "An 'out' parameter may not be passed into a function or macro as an 'in' argument.");
		return false;
	}
	if (ident->var.in_param && param->var.out_param)
	{
		SEMA_ERROR(expr, "An 'in' parameter may not be passed into a function or macro as an 'out' argument.");
		return false;
	}
	return true;
}

static inline bool sema_call_analyse_func_invocation(SemaContext *context, Decl *decl,
													 Type *type, Expr *expr, Expr *struct_var,
													 bool optional, const char *name, bool *no_match_ref)
{
	Signature *sig = type->function.signature;
	CalledDecl callee = {
			.macro = false,
			.definition = decl,
			.call_location = expr->span,
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

	if (sig->attrs.noreturn) expr->call_expr.no_return = true;

	if (!sema_call_evaluate_arguments(context, &callee, expr, &optional, no_match_ref)) return false;

	Type *rtype = type->function.prototype->rtype;

	expr->call_expr.has_optional_arg = optional;

	if (rtype != type_void)
	{
		bool is_optional_return = type_is_optional(rtype);
		expr->call_expr.is_optional_return = is_optional_return;
		expr->call_expr.must_use = sig->attrs.nodiscard || (is_optional_return && !sig->attrs.maydiscard);
	}
	expr->type = type_add_optional(rtype, optional);

	return true;
}

static inline bool sema_expr_analyse_var_call(SemaContext *context, Expr *expr, Type *func_ptr_type, bool optional, bool *no_match_ref)
{
	Decl *decl = NULL;
	func_ptr_type = type_flat_distinct_inline(func_ptr_type);
	if (func_ptr_type->type_kind != TYPE_FUNC_PTR)
	{
		if (no_match_ref)
		{
			*no_match_ref = true;
			return false;
		}
		RETURN_SEMA_ERROR(expr, "Only macros, functions and function pointers may be invoked, this is of type '%s'.",
						  type_to_error_string(func_ptr_type));
	}
	Type *pointee = func_ptr_type->pointer;
	expr->call_expr.is_pointer_call = true;
	return sema_call_analyse_func_invocation(context, pointee->function.decl, pointee, expr, NULL, optional,
											 func_ptr_type->pointer->name,
											 no_match_ref);
}

// Unify returns in a macro or expression block.
static inline Type *context_unify_returns(SemaContext *context)
{
	bool all_returns_need_casts = false;
	Type *common_type = NULL;

	// 1. Loop through the returns.
	bool optional = false;
	unsigned returns = vec_size(context->returns);
	if (!returns) return type_void;
	for (unsigned i = 0; i < returns; i++)
	{
		Ast *return_stmt = context->returns[i];
		Type *rtype;
		if (!return_stmt)
		{
			optional = true;
			rtype = type_wildcard;
		}
		else
		{
			Expr *ret_expr = return_stmt->return_stmt.expr;
			rtype = ret_expr ? ret_expr->type : type_void;
			if (type_is_optional(rtype))
			{
				optional = true;
				rtype = type_no_optional(rtype);
			}
		}
		// 2. If we have no common type, set to the return type.
		if (!common_type)
		{
			common_type = rtype;
			continue;
		}

		// 3. Same type -> we're done.
		if (common_type == rtype || (type_is_void(common_type) && rtype == type_wildcard)) continue;

		// 4. Find the max of the old and new.
		Type *max = type_find_max_type(common_type, rtype);

		// 5. No match -> error.
		if (!max)
		{
			ASSERT0(return_stmt);
			SEMA_ERROR(return_stmt, "Cannot find a common parent type of %s and %s",
					   type_quoted_error_string(rtype), type_quoted_error_string(common_type));
			Ast *prev = context->returns[i - 1];
			ASSERT0(prev);
			SEMA_NOTE(prev, "The previous return was here.");
			return NULL;
		}

		// 6. Set the new max, mark as needing a cast on all returns.
		common_type = max;
		all_returns_need_casts = true;
	}

	ASSERT0(common_type);

	// 7. Insert casts.
	if (all_returns_need_casts)
	{
		ASSERT0(common_type != type_wildcard);
		FOREACH(Ast *, return_stmt, context->returns)
		{
			if (!return_stmt) continue;
			Expr *ret_expr = return_stmt->return_stmt.expr;
			if (!ret_expr)
			{
				if (type_is_void(common_type)) continue;
				context_unify_returns(context);
			}
			// 8. All casts should work.
			if (!cast_implicit(context, ret_expr, common_type, false))
			{
				return NULL;
			}
		}
	}

	return type_add_optional(common_type, optional);
}

static inline bool sema_expr_analyse_func_call(SemaContext *context, Expr *expr, Decl *decl, Expr *struct_var,
											   bool optional, bool *no_match_ref)
{
	expr->call_expr.is_pointer_call = false;
	if (decl->func_decl.attr_test)
	{
		SEMA_ERROR(expr, "@test functions may not be directly called.");
		return false;
	}

	if (decl->func_decl.attr_benchmark)
	{
		SEMA_ERROR(expr, "@benchmark functions may not be directly called.");
		return false;
	}

	sema_display_deprecated_warning_on_use(context, decl, expr->span);

	// Tag dynamic dispatch.
	if (struct_var && decl->func_decl.attr_interface_method) expr->call_expr.is_dynamic_dispatch = true;

	return sema_call_analyse_func_invocation(context, decl,
											 decl->type,
											 expr,
											 struct_var,
											 optional,
											 decl->name, no_match_ref);
}


bool sema_expr_analyse_macro_call(SemaContext *context, Expr *call_expr, Expr *struct_var, Decl *decl,
								  bool call_var_optional, bool *no_match_ref)
{
	bool is_always_const = decl->func_decl.signature.attrs.always_const;
	ASSERT_SPAN(call_expr, decl->decl_kind == DECL_MACRO);

	if (context->macro_call_depth > 256)
	{
		decl->decl_kind = DECL_POISONED;
		RETURN_SEMA_ERROR(call_expr, "Failure evaluating macro, max call depth reached, "
									 "possibly due non-terminating macro recursion.");
	}

	sema_display_deprecated_warning_on_use(context, decl, call_expr->span);

	copy_begin();
	Decl **params = copy_decl_list_macro(decl->func_decl.signature.params);
	Ast *body = copy_ast_macro(astptr(decl->func_decl.body));
	AstId docs = decl->func_decl.docs;
	if (docs) docs = astid(copy_ast_macro(astptr(docs)));
	Signature *sig = &decl->func_decl.signature;
	copy_end();
	CalledDecl callee = {
			.macro = true,
			.call_location = call_expr->span,
			.name = decl->name,
			.params = params,
			.definition = decl,
			.block_parameter = decl->func_decl.body_param ? declptr(decl->func_decl.body_param)->name : NULL,
			.signature = sig,
			.struct_var = struct_var
	};

	bool has_optional_arg = call_var_optional;
	if (!sema_call_evaluate_arguments(context, &callee, call_expr, &has_optional_arg, no_match_ref)) return false;

	unsigned vararg_index = sig->vararg_index;
	Expr **args = call_expr->call_expr.arguments;
	FOREACH_IDX(i, Decl *, param, params)
	{
		if (i == vararg_index)
		{
			if (!param) continue;
			// Splat? That's the simple case.
			if (call_expr->call_expr.va_is_splat)
			{
				if (!sema_analyse_expr(context, args[i] = call_expr->call_expr.vasplat)) return false;
			}
			else
			{
				Expr **exprs = call_expr->call_expr.varargs;
				Expr *literal = expr_new(EXPR_COMPOUND_LITERAL, exprs ? exprs[0]->span : param->span);
				Expr *initializer_list = expr_new_expr(EXPR_INITIALIZER_LIST, literal);
				initializer_list->initializer_list = exprs;
				literal->expr_compound_literal.type_info = vartype(param);
				literal->expr_compound_literal.initializer = initializer_list;
				if (!sema_analyse_expr(context, args[i] = literal)) return false;
			}
		}
		param->var.init_expr = args[i];
		// Ref arguments doesn't affect optional arg.
		if (param->var.kind == VARDECL_PARAM_EXPR) continue;
		has_optional_arg = has_optional_arg || IS_OPTIONAL(args[i]);
	}

	Expr *macro_body = exprptrzero(call_expr->call_expr.macro_body);
	Decl **body_params = macro_body ? macro_body->macro_body_expr.body_arguments : NULL;
	unsigned body_params_count = vec_size(body_params);
	Decl **macro_body_params = decl->func_decl.body_param ? declptr(decl->func_decl.body_param)->body_params : NULL;
	unsigned expected_body_params = vec_size(macro_body_params);
	if (expected_body_params > body_params_count)
	{
		if (no_match_ref) goto NO_MATCH_REF;
		RETURN_SEMA_ERROR(call_expr, "Not enough parameters for the macro body, expected %d.", expected_body_params);
	}
	if (expected_body_params < body_params_count)
	{
		if (no_match_ref) goto NO_MATCH_REF;
		RETURN_SEMA_ERROR(call_expr, "Too many parameters for the macro body, expected %d.", expected_body_params);
	}
	for (unsigned j = 0; j < expected_body_params; j++)
	{
		Decl *body_param = macro_body_params[j];
		ASSERT_SPAN(call_expr, body_param->resolve_status == RESOLVE_DONE);
		Decl *body_arg = body_params[j];
		VarDeclKind kind_of_expected = body_param->var.kind;
		if (kind_of_expected != body_arg->var.kind)
		{
			switch (kind_of_expected)
			{
				case VARDECL_PARAM_CT_TYPE:
					RETURN_SEMA_FUNC_ERROR(decl, body_arg, "Expected a type argument.");
				case VARDECL_PARAM_EXPR:
					RETURN_SEMA_FUNC_ERROR(decl, body_arg, "Expected an expression argument.");
				case VARDECL_PARAM_REF:
					RETURN_SEMA_FUNC_ERROR(decl, body_arg, "Expected a reference ('&') argument.");
				case VARDECL_PARAM_CT:
					RETURN_SEMA_FUNC_ERROR(decl, body_arg, "Expected a compile time ('$') argument.");
				case VARDECL_PARAM:
					RETURN_SEMA_FUNC_ERROR(decl, body_arg, "Expected a regular argument.");
				default:
					UNREACHABLE
			}
		}
		// No type checking
		switch (kind_of_expected)
		{
			case VARDECL_PARAM_CT_TYPE:
				continue;
			case VARDECL_PARAM_CT:
			case VARDECL_PARAM_EXPR:
			case VARDECL_PARAM_REF:
				// Optional typing
				break;
			case VARDECL_PARAM:
				// Mandatory typing
				if (!body_arg->var.type_info)
				{
					RETURN_SEMA_ERROR(body_arg, "Expected a type parameter before this variable name.");
				}
				break;
			default:
				UNREACHABLE
		}
		TypeInfo *expected_type_info = vartype(body_param);
		TypeInfo *type_info = vartype(body_arg);
		if (type_info && !sema_resolve_type_info(context, type_info, RESOLVE_TYPE_DEFAULT)) return false;
		Type *type = type_info ? type_info->type : NULL;
		if (type && expected_type_info && type->canonical != expected_type_info->type->canonical)
		{
			if (no_match_ref) goto NO_MATCH_REF;
			RETURN_SEMA_ERROR(type_info, "This parameter should be %s but was %s",
								  type_quoted_error_string(expected_type_info->type),
								  type_quoted_error_string(type));
		}
		if (type && kind_of_expected == VARDECL_PARAM_REF && !type_is_pointer(type_info->type))
		{
			RETURN_SEMA_ERROR(type_info, "A pointer type was expected for a ref argument, did you mean %s?",
							  type_quoted_error_string(type_get_ptr(type_info->type)));
		}
		body_arg->type = type;

		if (type_info)
		{
			switch (sema_resolve_storage_type(context, type_info->type))
			{
				case STORAGE_ERROR:
					return false;
				case STORAGE_NORMAL:
					if (!sema_set_alloca_alignment(context, body_arg->type, &body_arg->alignment)) return false;
					break;
				default:
					break;
			}
		}
	}


	DynamicScope old_scope = context->active_scope;
	context_change_scope_with_flags(context, SCOPE_NONE);

	SemaContext macro_context;

	Type *rtype = NULL;
	sema_context_init(&macro_context, decl->unit);
	macro_context.compilation_unit = context->unit;
	macro_context.macro_call_depth = context->macro_call_depth + 1;
	macro_context.call_env = context->call_env;
	rtype = typeget(sig->rtype);
	bool optional_return = rtype && type_is_optional(rtype);
	bool may_be_optional = !rtype || optional_return;
	if (rtype) rtype = type_no_optional(rtype);
	macro_context.expected_block_type = rtype;

	context_change_scope_with_flags(&macro_context, SCOPE_MACRO);

	macro_context.block_return_defer = macro_context.active_scope.defer_last;
	InliningSpan span = { call_expr->span, context->inlined_at };
	macro_context.inlined_at = &span;
	macro_context.current_macro = decl;
	macro_context.yield_body = macro_body ? macro_body->macro_body_expr.body : NULL;
	macro_context.yield_params = body_params;
	macro_context.yield_context = context;
	FOREACH(Expr *, expr, call_expr->call_expr.varargs)
	{
		if (expr->resolve_status == RESOLVE_DONE) continue;
		Expr *expr_inner = expr_copy(expr);
		expr->expr_kind = EXPR_OTHER_CONTEXT;
		expr->expr_other_context.inner = expr_inner;
		expr->expr_other_context.context = context;
	}
	macro_context.macro_varargs = call_expr->call_expr.varargs;
	macro_context.original_inline_line = context->original_inline_line ? context->original_inline_line : call_expr->span.row;
	macro_context.original_module = context->original_module ? context->original_module : context->compilation_unit->module;
	macro_context.macro_params = params;
	BlockExit** block_exit_ref = CALLOCS(BlockExit*);
	macro_context.block_exit_ref = block_exit_ref;

	FOREACH(Decl *, param, params)
	{
		// Skip raw vararg
		if (!param) continue;
		if (!sema_add_local(&macro_context, param)) goto EXIT_FAIL;
	}

	AstId assert_first = 0;
	AstId* next = &assert_first;

	bool has_ensures = false;
	if (!sema_analyse_contracts(&macro_context, docs, &next, call_expr->span, &has_ensures)) goto EXIT_FAIL;
	sema_append_contract_asserts(assert_first, body);
	macro_context.macro_has_ensures = has_ensures;

	if (!sema_analyse_statement(&macro_context, body)) goto EXIT_FAIL;
	ASSERT_SPAN(call_expr, macro_context.active_scope.depth == 1);
	bool implicit_void_return = !macro_context.active_scope.jump_end;
	params = macro_context.macro_params;
	bool is_no_return = sig->attrs.noreturn;

	if (!vec_size(macro_context.returns))
	{
		if (rtype && rtype != type_void && !macro_context.active_scope.jump_end)
		{
			SEMA_ERROR(decl,
					   "Missing return in macro that should evaluate to %s.",
					   type_quoted_error_string(rtype));
			goto EXIT_FAIL;
		}
	}
	else if (is_no_return)
	{
		SEMA_ERROR(context->returns[0], "Return used despite macro being marked '@noreturn'.");
		goto EXIT_FAIL;
	}

	if (rtype)
	{
		bool inferred_len = type_len_is_inferred(rtype);
		FOREACH(Ast *, return_stmt, macro_context.returns)
		{
			if (!return_stmt)
			{
				ASSERT_SPAN(call_expr, may_be_optional);
				continue;
			}
			Expr *ret_expr = return_stmt->return_stmt.expr;
			if (!ret_expr)
			{
				if (type_is_void(rtype)) continue;
				SEMA_ERROR(return_stmt, "Expected returning a value of type %s.", type_quoted_error_string(rtype));
				goto EXIT_FAIL;
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
			bool success = cast_implicit_silent(context, ret_expr, rtype, false);
			if (inferred_len || (!may_be_optional && IS_OPTIONAL(ret_expr)) || !success)
			{
				SEMA_ERROR(ret_expr, "Expected %s, not %s.", type_quoted_error_string(rtype),
						   type_quoted_error_string(type));
				goto EXIT_FAIL;
			}
			ret_expr->type = type_add_optional(ret_expr->type, optional_return);
		}
	}
	else
	{
		rtype = context_unify_returns(&macro_context);
		if (!rtype) goto EXIT_FAIL;
		optional_return = type_is_optional(rtype);
	}

	// Handle the implicit return.
	if (implicit_void_return)
	{
		if (type_is_wildcard(rtype))
		{
			optional_return = optional_return || type_is_optional(rtype);
			rtype = type_void;
		}
		else
		{
			Type *flat = type_flatten(rtype);
			if (flat != type_void)
			{
				RETURN_SEMA_ERROR(decl, "Macro implicitly returns 'void' at the end, which cannot be cast to the inferred %s.",
								  type_quoted_error_string(rtype));
			}
		}
	}

	call_expr->type = type_add_optional(rtype, optional_return || has_optional_arg);

	ASSERT_SPAN(call_expr, call_expr->type);
	bool must_use = false;
	if (rtype != type_void || optional_return)
	{
		must_use = sig->attrs.nodiscard || (optional_return && !sig->attrs.maydiscard);
	}
	unsigned returns_found = vec_size(macro_context.returns);
	// We may have zero normal macro returns but the active scope still has a "jump end".
	// In this case it is triggered by the @body()
	if (!returns_found && macro_context.active_scope.jump_end)
	{
		is_no_return = true;
	}
	if (returns_found == 1 && !implicit_void_return)
	{
		Ast *ret = macro_context.returns[0];
		Expr *result = ret ? ret->return_stmt.expr : NULL;
		if (!result) goto NOT_CT;
		if (!expr_is_runtime_const(result)) goto NOT_CT;
		bool only_ct_params = true;
		FOREACH(Decl *, param, params)
		{
			// Skip raw vararg
			if (!param) continue;
			switch (param->var.kind)
			{
				case VARDECL_PARAM_CT:
				case VARDECL_PARAM_CT_TYPE:
				case VARDECL_PARAM_EXPR:
					break;
				default:
					goto NOT_CT;
			}
		}
		if (ast_is_compile_time(body))
		{
			expr_replace(call_expr, result);
			goto EXIT;
		}
	}
NOT_CT:
	call_expr->expr_kind = EXPR_MACRO_BLOCK;
	call_expr->macro_block.had_optional_arg = has_optional_arg;
	call_expr->macro_block.is_must_use = must_use;
	call_expr->macro_block.is_optional_return = optional_return;
	call_expr->macro_block.first_stmt = body->compound_stmt.first_stmt;
	call_expr->macro_block.macro = decl;
	call_expr->macro_block.params = params;
	call_expr->macro_block.block_exit = block_exit_ref;
	call_expr->macro_block.is_noreturn = is_no_return;
EXIT:
	ASSERT_SPAN(call_expr, context->active_scope.defer_last == context->active_scope.defer_start);
	context->active_scope = old_scope;
	if (is_no_return) context->active_scope.jump_end = true;
	sema_context_destroy(&macro_context);
	call_expr->resolve_status = RESOLVE_DONE;
	if (is_always_const && !expr_is_runtime_const(call_expr))
	{
		SEMA_ERROR(call_expr, "The macro failed to fold to a constant value, despite being '@const'.");
		SEMA_NOTE(decl, "The macro was declared here.");
		return false;
	}
	return true;
EXIT_FAIL:
	sema_context_destroy(&macro_context);
	return SCOPE_POP_ERROR();
NO_MATCH_REF:
	*no_match_ref = true;
	return false;
}

static bool sema_call_analyse_body_expansion(SemaContext *macro_context, Expr *call)
{
	Decl *macro = macro_context->current_macro;
	ASSERT_SPAN(call, macro && macro->func_decl.body_param);
	Decl *body_decl = declptr(macro->func_decl.body_param);
	ExprCall *call_expr = &call->call_expr;
	Expr *macro_body = exprptrzero(call_expr->macro_body);
	if (macro_body && vec_size(macro_body->macro_body_expr.body_arguments))
	{
		PRINT_ERROR_AT(call, "Nested expansion is not possible.");
		return false;
	}
	if (call_expr->va_is_splat)
	{
		PRINT_ERROR_AT(call, "Expanding parameters is not allowed for macro invocations.");
	}
	// Theoretically we could support named arguments, but that's unnecessary.
	unsigned expressions = vec_size(call_expr->arguments);
	Decl **body_parameters = body_decl->body_params;
	if (expressions != vec_size(body_parameters))
	{
		PRINT_ERROR_AT(call, "Expected %d parameter(s) to %s.", vec_size(body_parameters), body_decl->name);
		return false;
	}
	Expr **args = call_expr->arguments;

	Decl **params = macro_context->yield_params;

	bool has_optional_arg = false;

	// Evaluate the expressions.
	for (unsigned i = 0; i < expressions; i++)
	{
		Decl *param = params[i];
		Expr *expr = args[i];
		if (!sema_analyse_parameter(macro_context, expr, param, body_decl, &has_optional_arg, NULL, true, false))
		{
			return false;
		}
		if (has_optional_arg)
		{
			sema_error_at(macro_context, expr->span, "Optional arguments are not permitted in a body invocation.");
			return false;
		}
		switch (param->var.kind)
		{
			case VARDECL_PARAM_EXPR:
			case VARDECL_PARAM_CT:
			case VARDECL_PARAM_CT_TYPE:
				param->var.init_expr = args[i];
				args[i] = NULL;
				break;
			default:
				break;
		}
	}

	AstId macro_defer = macro_context->active_scope.defer_last;
	Ast *first_defer = NULL;
	SemaContext *context = macro_context->yield_context;
	Expr *func_expr = exprptr(call_expr->function);
	ASSERT_SPAN(call, func_expr->expr_kind == EXPR_MACRO_BODY_EXPANSION);
	expr_replace(call, func_expr);
	call->body_expansion_expr.values = args;
	call->body_expansion_expr.declarations = macro_context->yield_params;
	AstId last_defer = context->active_scope.defer_last;
	bool success;
	bool ends_in_jump;
	SCOPE_START
		unsigned ct_context = sema_context_push_ct_stack(context);
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
		FOREACH(Decl *, param, params)
		{
			if (!sema_add_local(context, param))
			{
				sema_context_pop_ct_stack(context, ct_context);
				return SCOPE_POP_ERROR();
			}
		}
		Ast *ast = copy_ast_single(macro_context->yield_body);
		call->body_expansion_expr.first_stmt = astid(ast);
		if (!sema_analyse_statement(context, ast))
		{
			sema_context_pop_ct_stack(context, ct_context);
			return SCOPE_POP_ERROR();
		}
		ASSERT_SPAN(call, ast->ast_kind == AST_COMPOUND_STMT);
		if (context->active_scope.jump_end)
		{
			macro_context->active_scope.jump_end = true;
		}
		if (first_defer)
		{
			first_defer->defer_stmt.prev_defer = 0;
			context->active_scope.defer_last = last_defer;
		}
		sema_context_pop_ct_stack(context, ct_context);
	SCOPE_END;

	return true;

}

void sema_expr_convert_enum_to_int(SemaContext *context, Expr *expr)
{
	ASSERT0(type_flatten(expr->type)->type_kind == TYPE_ENUM);
	Type *underlying_type = type_base(expr->type);
	if (sema_cast_const(expr))
	{
		ASSERT0(expr->const_expr.const_kind == CONST_ENUM);
		expr_rewrite_const_int(expr, underlying_type, expr->const_expr.enum_err_val->enum_constant.ordinal);
	}
	if (expr->expr_kind == EXPR_ENUM_FROM_ORD)
	{
		*expr = *expr->inner_expr;
	}
	expr->type = type_add_optional(underlying_type, IS_OPTIONAL(expr));
}

bool sema_expr_analyse_general_call(SemaContext *context, Expr *expr, Decl *decl, Expr *struct_var, bool optional,
									bool *no_match_ref)
{
	expr->call_expr.is_type_method = struct_var != NULL;
	if (decl == NULL)
	{
		return sema_expr_analyse_var_call(context, expr,
										  type_flatten(exprptr(expr->call_expr.function)->type), optional,
										  no_match_ref);
	}
	if (!sema_analyse_decl(context, decl)) return false;
	switch (decl->decl_kind)
	{
		case DECL_MACRO:
			expr->call_expr.func_ref = declid(decl);
			expr->call_expr.is_func_ref = true;
			return sema_expr_analyse_macro_call(context, expr, struct_var, decl, optional, no_match_ref);
		case DECL_VAR:
			ASSERT_SPAN(expr, struct_var == NULL);
			return sema_expr_analyse_var_call(context, expr, decl->type->canonical, optional || IS_OPTIONAL(decl),
											  no_match_ref);
		case DECL_FUNC:
			expr->call_expr.func_ref = declid(decl);
			expr->call_expr.is_func_ref = true;
			return sema_expr_analyse_func_call(context, expr, decl, struct_var, optional, no_match_ref);
		case DECL_POISONED:
			return false;
		default:
			if (no_match_ref)
			{
				*no_match_ref = true;
				return false;
			}
			RETURN_SEMA_ERROR(expr, "This expression cannot be called.");
	}
}

INLINE bool sema_expr_analyse_from_ordinal(SemaContext *context, Expr *expr, Expr *tag)
{
	Expr **args = expr->call_expr.arguments;
	unsigned arg_count = vec_size(args);
	Decl *decl = tag->type_call_expr.type;
	if (arg_count != 1) RETURN_SEMA_ERROR(expr, "Expected a single string argument to 'from_ordinal'.");
	Expr *key = args[0];
	if (!sema_analyse_expr(context, key)) return false;
	if (!type_is_integer(key->type))
	{
		RETURN_SEMA_ERROR(key, "The ordinal should be an integer.");
	}

	if (sema_cast_const(key))
	{
		Int to_convert = key->const_expr.ixx;
		if (int_is_neg(to_convert))
		{
			RETURN_SEMA_ERROR(key, "'from_ordinal' doesn't work on negative numbers.");
		}
		unsigned max_enums = vec_size(decl->enums.values);
		Int max = {.i.low = max_enums, .type = TYPE_U32};
		if (int_comp(to_convert, max, BINARYOP_GE))
		{
			RETURN_SEMA_ERROR(key, "The ordinal '%s' exceeds the max ordinal '%u'.", int_to_str(max, 10, false), max_enums - 1);
		}
		expr->expr_kind = EXPR_CONST;
		expr->const_expr = (ExprConst) {
				.enum_err_val = decl->enums.values[to_convert.i.low],
				.const_kind = decl->decl_kind == DECL_FAULT ? CONST_ERR : CONST_ENUM
		};
		expr->type = decl->type;
		return true;
	}
	if (decl->decl_kind == DECL_FAULT) RETURN_SEMA_ERROR(key, "For faults you can only use 'from_ordinal' with constant arguments.", decl->name);

	expr->expr_kind = EXPR_ENUM_FROM_ORD;
	expr->inner_expr = key;
	expr->type = decl->type;
	return true;
}

static inline bool sema_expr_analyse_typecall(SemaContext *context, Expr *expr)
{
	Expr *tag = exprptr(expr->call_expr.function);
	expr->call_expr.arguments = sema_expand_vasplat_exprs(context, expr->call_expr.arguments);
	if (tag->type_call_expr.property == TYPE_PROPERTY_FROM_ORDINAL)
	{
		return sema_expr_analyse_from_ordinal(context, expr, tag);
	}
	Expr **args = expr->call_expr.arguments;
	unsigned arg_count = vec_size(args);
	Decl *decl = tag->type_call_expr.type;
	bool is_has = tag->type_call_expr.property == TYPE_PROPERTY_HAS_TAGOF;
	const char *name = is_has ? "has_tagof" : "tagof";
	if (arg_count != 1) RETURN_SEMA_ERROR(expr, "Expected a single string argument to '%s'.", name);
	Expr *key = args[0];
	if (!sema_analyse_expr(context, key)) return false;
	if (!sema_cast_const(key) || !expr_is_const_string(key))
	{
		RETURN_SEMA_ERROR(key, "The tag name should be a string constant.");
	}
	ASSERT_SPAN(expr, decl->resolved_attributes);
	ResolvedAttrData *attrs = decl->attrs_resolved;
	const char *tagname = key->const_expr.bytes.ptr;
	if (!attrs || !attrs->tags) goto NOT_FOUND;
	Expr *value = NULL;
	FOREACH(Attr *, attr, attrs->tags)
	{
		if (str_eq(attr->exprs[0]->const_expr.bytes.ptr, tagname)) value = attr->exprs[1];
	}
	if (!value) goto NOT_FOUND;
	if (is_has)
	{
		expr_rewrite_const_bool(expr, type_bool, true);
		return true;
	}
	expr_replace(expr, expr_copy(value));
	return true;
NOT_FOUND:
	if (is_has)
	{
		expr_rewrite_const_bool(expr, type_bool, false);
		return true;
	}
	RETURN_SEMA_ERROR(expr, "The tag '%s' is not defined, always check with '.has_tagof'.", tagname);
}

INLINE bool sema_call_may_not_have_attributes(SemaContext *context, Expr *expr)
{
	if (expr->call_expr.attr_force_inline)
	{
		RETURN_SEMA_ERROR(expr, "'@inline' is not allowed here.");
	}
	if (expr->call_expr.attr_force_inline)
	{
		RETURN_SEMA_ERROR(expr, "'@noinline' is not allowed here.");
	}
	if (expr->call_expr.attr_force_inline)
	{
		RETURN_SEMA_ERROR(expr, "'@pure' is not allowed here.");
	}
	return true;
}
static inline bool sema_call_analyse_member_get(SemaContext *context, Expr *expr)
{
	if (vec_size(expr->call_expr.arguments) != 1)
	{
		RETURN_SEMA_ERROR(expr, "Expected a single argument to '.get'.");
	}
	if (!sema_call_may_not_have_attributes(context, expr)) return false;
	Expr *get = exprptr(expr->call_expr.function);
	Expr *inner = expr->call_expr.arguments[0];
	if (!sema_analyse_expr(context, inner)) return false;
	Decl *decl = get->member_get_expr;
	Type *type = type_flatten(inner->type);
	if (type->type_kind != TYPE_STRUCT && type->type_kind != TYPE_UNION)
	{
		RETURN_SEMA_ERROR(inner, "This value does not match the member.");
	}
	Decl **members = type->decl->strukt.members;
	ArrayIndex index = -1;
	FOREACH_IDX(i, Decl *, member, members)
	{
		if (member == decl)
		{
			index = i;
			break;
		}
	}
	if (index == -1)
	{
		RETURN_SEMA_ERROR(inner, "This value does not match the member.");
	}
	expr->expr_kind = EXPR_ACCESS;
	expr->access_expr = (ExprAccess) { .parent = inner, .ref = decl };
	expr->type = decl->type;
	return true;
}
static inline bool sema_expr_analyse_call(SemaContext *context, Expr *expr, bool *no_match_ref)
{
	if (no_match_ref) *no_match_ref = true;
	Expr *func_expr = exprptr(expr->call_expr.function);
	if (!sema_analyse_expr_value(context, func_expr)) return false;
	if (func_expr->expr_kind == EXPR_MACRO_BODY_EXPANSION)
	{
		return sema_call_analyse_body_expansion(context, expr);
	}
	if (func_expr->expr_kind == EXPR_MEMBER_GET)
	{
		return sema_call_analyse_member_get(context, expr);
	}
	bool optional = func_expr->type && IS_OPTIONAL(func_expr);
	Decl *decl;
	Expr *struct_var = NULL;
	switch (func_expr->expr_kind)
	{
		case EXPR_TYPECALL:
			return sema_expr_analyse_typecall(context, expr);
		case EXPR_BUILTIN:
			return sema_expr_analyse_builtin_call(context, expr);
		case EXPR_IDENTIFIER:
			decl = func_expr->identifier_expr.decl;
			if (!sema_analyse_decl(context, decl)) return false;
			break;
		case EXPR_ACCESS:
			decl = func_expr->access_expr.ref;
			if (!sema_analyse_decl(context, decl)) return false;
			switch (decl->decl_kind)
			{
				case DECL_MACRO:
					struct_var = func_expr->access_expr.parent;
					if (decl->func_decl.signature.params[0]->var.kind == VARDECL_PARAM_REF) break;
					if (decl->func_decl.signature.params[0]->type->canonical != struct_var->type->canonical
						&& decl->func_decl.signature.params[0]->type->type_kind == TYPE_POINTER)
					{
						expr_insert_addr(struct_var);
					}
					break;
				case DECL_FUNC:
					struct_var = func_expr->access_expr.parent;
					if (decl->func_decl.signature.params[0]->type->type_kind == TYPE_POINTER )
					{
						if (decl->func_decl.signature.params[0]->type->canonical != struct_var->type->canonical
							&& !decl->func_decl.attr_interface_method)
						{
							expr_insert_addr(struct_var);
						}
					}
					break;
				default:
					break;
			}
			break;
		case EXPR_TYPEINFO:
			if (func_expr->type_expr->resolve_status == RESOLVE_DONE)
			{
				SEMA_ERROR(expr, "A type cannot be followed by (), if you intended a cast, use '(type) expression'.");
			}
			else
			{
				SEMA_ERROR(expr, "A type cannot be followed by (), did you mean to use 'type {}'?");
			}
			return false;
		default:
		{
			Type *type = type_flatten(func_expr->type);
			if (type->type_kind == TYPE_FUNC_PTR)
			{
				decl = NULL;
				break;
			}
			if (no_match_ref)
			{
				*no_match_ref = true;
				return false;
			}
			RETURN_SEMA_ERROR(expr, "This value cannot be invoked, did you accidentally add ()?");
		}
	}
	decl = decl ? decl_flatten(decl) : NULL;
	return sema_expr_analyse_general_call(context, expr, decl, struct_var, optional, no_match_ref);
}

static bool sema_slice_index_is_in_range(SemaContext *context, Type *type, Expr *index_expr, bool end_index,
                                         bool from_end, bool *remove_from_end, bool check_valid)
{
	ASSERT_SPAN(index_expr, type == type->canonical);
	if (!sema_cast_const(index_expr)) return true;

	Int index = index_expr->const_expr.ixx;
	if (!int_fits(index, TYPE_I64))
	{
		RETURN_SEMA_ERROR(index_expr, "The index cannot be stored in a 64-signed integer, which isn't supported.");
		return false;
	}
	if (from_end && int_is_neg(index))
	{
		RETURN_SEMA_ERROR(index_expr, "Negative numbers are not allowed when indexing from the end.");
	}
	ArrayIndex idx = (ArrayIndex)index.i.low;
RETRY:;
	switch (type->type_kind)
	{
		case TYPE_POINTER:
			ASSERT_SPAN(index_expr, !from_end);
			return true;
		case TYPE_FLEXIBLE_ARRAY:
			ASSERT_SPAN(index_expr, !from_end);
			break;
		case TYPE_UNTYPED_LIST:
		case TYPE_ARRAY:
		case TYPE_VECTOR:
		{
			ArrayIndex len = (ArrayIndex)type->array.len;
			if (from_end)
			{
				idx = len - idx;
				index_expr->const_expr.ixx.i.low = idx;
				*remove_from_end = true;
			}
			// Checking end can only be done for arrays.
			if (end_index && idx >= len)
			{
				if (check_valid) return false;
				RETURN_SEMA_ERROR(index_expr, "End index out of bounds, was %lld, exceeding %lld.", (long long)idx, (long long)len);
			}
			if (!end_index && idx >= len)
			{
				if (len == 0)
				{
					if (check_valid) return false;
					RETURN_SEMA_ERROR(index_expr, "Cannot index into a zero size list.");
				}
				if (check_valid) return false;
				RETURN_SEMA_ERROR(index_expr, "Index out of bounds, was %lld, exceeding maximum (%lld).", (long long)idx, (long long)len - 1);
			}
			break;
		}
		case TYPE_SLICE:
			// If not from end, just check the negative values.
			if (!from_end) break;
			// From end we can only do sanity checks ^0 is invalid for non-end index. ^-1 and less is invalid for all.
			if (idx == 0 && !end_index)
			{
				if (check_valid) return false;
				RETURN_SEMA_ERROR(index_expr,
				                  "Array index out of bounds, index from end (%lld) must be greater than zero or it will exceed the max array index.",
				                  (long long) idx);
			}
			return true;
		case TYPE_STRUCT:
		{
			Decl *decl = type->decl;
			ASSERT_SPAN(index_expr, decl->is_substruct);
			type = decl->strukt.members[0]->type->canonical;
			goto RETRY;
		}
		default:
			UNREACHABLE
	}
	if (idx < 0)
	{
		RETURN_SEMA_ERROR(index_expr, "Index out of bounds, using a negative index is only allowed for pointers.");
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

static bool sema_subscript_rewrite_index_const_list(Expr *const_list, ArraySize index, bool from_back, Expr *result)
{
	ASSERT_SPAN(const_list, const_list->const_expr.const_kind == CONST_INITIALIZER);
	return expr_rewrite_to_const_initializer_index(const_list->type, const_list->const_expr.initializer, result, index, from_back);
}

/**
 * Find subscript type or overload for subscript.
 */
static Expr *sema_expr_find_subscript_type_or_overload_for_subscript(SemaContext *context, Expr *current_expr,
                                                                     CheckType check, Type **subscript_type_ptr,
                                                                     Decl **overload_ptr)
{
	Decl *overload = NULL;
	switch (check)
	{
		case CHECK_ADDRESS:
			overload = sema_find_operator(context, current_expr->type, OVERLOAD_ELEMENT_REF);
			break;
		case CHECK_VALUE:
			overload = sema_find_operator(context, current_expr->type, OVERLOAD_ELEMENT_AT);
			break;
		case CHECK_LVALUE:
			overload = sema_find_operator(context, current_expr->type, OVERLOAD_ELEMENT_SET);
			if (overload)
			{
				*overload_ptr = overload;
				ASSERT0(vec_size(overload->func_decl.signature.params) == 3);
				*subscript_type_ptr = overload->func_decl.signature.params[2]->type;
				return current_expr;
			}
			break;

	}
	// Overload found for [] and &[]
	if (overload)
	{
		*overload_ptr = overload;
		ASSERT0(overload->func_decl.signature.rtype);
		*subscript_type_ptr = type_infoptr(overload->func_decl.signature.rtype)->type;
		return current_expr;
	}
	// Otherwise, see if we have an indexed type.
	Type *inner_type = type_get_indexed_type(current_expr->type);
	if (inner_type)
	{
		*subscript_type_ptr = inner_type;
		*overload_ptr = NULL;
		return current_expr;
	}
	if (type_is_substruct(current_expr->type))
	{
		Expr *embedded_struct = expr_access_inline_member(current_expr, current_expr->type->decl);
		return sema_expr_find_subscript_type_or_overload_for_subscript(context, embedded_struct, check,
		                                                               subscript_type_ptr,
		                                                               overload_ptr);
	}
	return NULL;
}

static inline bool sema_expr_analyse_subscript(SemaContext *context, Expr *expr, CheckType check, bool check_valid)
{
	ASSERT0(expr->expr_kind == EXPR_SUBSCRIPT || expr->expr_kind == EXPR_SUBSCRIPT_ADDR);
	bool is_eval_ref = expr->expr_kind == EXPR_SUBSCRIPT_ADDR;

	// Evaluate the expression to index.
	Expr *subscripted = exprptr(expr->subscript_expr.expr);
	if (!sema_analyse_expr_check(context, subscripted, CHECK_VALUE)) return false;

	// If it is an lvalue then check that it is assignable.
	if (check == CHECK_LVALUE && !sema_expr_check_assign(context, expr, NULL)) return false;

	// 2. Evaluate the index.
	Expr *index = exprptr(expr->subscript_expr.index.expr);
	if (!sema_analyse_expr(context, index)) return false;

	// 3. Check failability due to value.
	bool optional = IS_OPTIONAL(subscripted);

	Decl *overload = NULL;
	Type *subscript_type = NULL;
	Expr *current_expr;
	Type *current_type = subscripted->type->canonical;
	if (current_type == type_untypedlist)
	{
		current_expr = subscripted;
	}
	else
	{
		current_expr = sema_expr_find_subscript_type_or_overload_for_subscript(context, subscripted, check,
		                                                                       &subscript_type,
		                                                                       &overload);
		if (!overload && !subscript_type && is_eval_ref)
		{
			// Maybe there is a [] overload?
			if (sema_expr_find_subscript_type_or_overload_for_subscript(context, subscripted, check, &subscript_type,
			                                                            &overload))
			{
				if (check_valid) goto VALID_FAIL_POISON;
				RETURN_SEMA_ERROR(expr, "A function or macro with '@operator(&[])' is not defined for %s, "
										"so you need && to take the address of the temporary.",
								  type_quoted_error_string(subscripted->type));
			}
		}
		if (!subscript_type)
		{
			if (check_valid) goto VALID_FAIL_POISON;
			RETURN_SEMA_ERROR(expr, "Indexing a value of type %s is not possible.", type_quoted_error_string(subscripted->type));
		}
		if (!overload) current_type = type_flatten(current_expr->type);
	}

	ASSERT0(current_type == current_type->canonical);
	int64_t index_value = -1;
	bool start_from_end = expr->subscript_expr.index.start_from_end;
	if (start_from_end && (current_type->type_kind == TYPE_POINTER || current_type->type_kind == TYPE_FLEXIBLE_ARRAY))
	{
		if (check_valid) goto VALID_FAIL_POISON;
		RETURN_SEMA_ERROR(index, "Indexing from the end is not allowed for pointers "
								 "and flexible array members.");
	}

	ArrayIndex size;
	bool check_len = !context->call_env.in_no_eval || current_type == type_untypedlist;
	if (check_len && expr_is_const_int(index) && (size = sema_len_from_expr(current_expr)) >= 0)
	{
		// 4c. And that it's in range.
		if (int_is_neg(index->const_expr.ixx))
		{
			if (check_valid) goto VALID_FAIL_POISON;
			RETURN_SEMA_ERROR(index, "The index may not be negative.");
		}
		if (!int_fits(index->const_expr.ixx, TYPE_I64) || size == 0)
		{
			if (check_valid) goto VALID_FAIL_POISON;
			RETURN_SEMA_ERROR(index, "The index is out of range.", size);
		}
		index_value = int_to_i64(index->const_expr.ixx);
		if (start_from_end)
		{
			index_value = size - index_value;
		}
		if (index_value < 0 || index_value >= size)
		{
			if (check_valid) goto VALID_FAIL_POISON;
			if (start_from_end)
			{
				RETURN_SEMA_ERROR(index,
								  size > 1
								  ? "An index of '%lld' from the end is out of range, a value between 1 and %lld was expected."
								  : "An index of '%lld' from the end is out of range, a value of %lld was expected.",
								  (long long) (size - index_value),
								  (long long) size);
			}
			RETURN_SEMA_ERROR(index,
							  size > 1
							  ? "An index of '%lld' is out of range, a value between 0 and %lld was expected."
							  : "An index of '%lld' is out of range, a value of %lld was expected.",
							  (long long) index_value,
							  (long long) size - 1);
		}
	}

	// 4. If we are indexing into a complist
	if (current_type == type_untypedlist)
	{
		if (is_eval_ref)
		{
			if (check_valid) goto VALID_FAIL_POISON;
			RETURN_SEMA_ERROR(subscripted, "You need to use && to take the address of a temporary.");
		}
		// 4a. This may either be an initializer list or a CT value
		while (current_expr->expr_kind == EXPR_CT_IDENT) current_expr = current_expr->ct_ident_expr.decl->var.init_expr;

		// 4b. Now we need to check that we actually have a valid type.
		if (index_value < 0)
		{
			if (check_valid) goto VALID_FAIL_POISON;
			RETURN_SEMA_ERROR(index, "To subscript an untyped list a compile time integer index is needed.");
		}
		if (check == CHECK_LVALUE)
		{
			REMINDER("Fix LVALUE");
		}
		expr_replace(expr, current_expr->const_expr.untyped_list[index_value]);
		return true;
	}
	if (!sema_cast_rvalue(context, subscripted, true)) return false;

	if (overload)
	{
		if (start_from_end)
		{
			Decl *len = sema_find_operator(context, current_expr->type, OVERLOAD_LEN);
			if (!len)
			{
				if (check_valid) goto VALID_FAIL_POISON;
				RETURN_SEMA_ERROR(subscripted, "Cannot index '%s' from the end, since there is no 'len' overload.", type_to_error_string(subscripted->type));
			}
			if (!sema_analyse_expr(context, current_expr)) return false;
			Decl *temp = decl_new_generated_var(current_expr->type, VARDECL_PARAM, current_expr->span);
			Expr *decl = expr_generate_decl(temp, expr_copy(current_expr));
			current_expr->expr_kind = EXPR_EXPRESSION_LIST;
			current_expr->expression_list = NULL;
			vec_add(current_expr->expression_list, decl);
			vec_add(current_expr->expression_list, expr_variable(temp));
			if (!sema_analyse_expr(context, current_expr)) return false;
			Expr *var_for_len = expr_variable(temp);
			Expr *len_expr = expr_new(EXPR_CALL, expr->span);
			if (!sema_insert_method_call(context, len_expr, len, var_for_len, NULL)) return false;
			if (!sema_analyse_expr(context, len_expr)) return false;
			Expr *index_copy = expr_copy(index);
			if (!sema_analyse_expr(context, index_copy)) return false;
			if (!cast_explicit(context, index_copy, len_expr->type)) return false;
			expr_rewrite_to_binary(index, len_expr, index_copy, BINARYOP_SUB);
			index->resolve_status = RESOLVE_NOT_DONE;
			if (!sema_analyse_expr(context, index)) return false;
		}
		if (check == CHECK_LVALUE)
		{
			expr->expr_kind = EXPR_SUBSCRIPT_ASSIGN;
			expr->type = subscript_type;
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
	if (!cast_to_index(context, index, subscripted->type)) return false;

	optional |= IS_OPTIONAL(index);
	// Check range
	bool remove_from_back = false;
	if (!sema_slice_index_is_in_range(context, current_type, index, false, start_from_end, &remove_from_back,
	                                  check_valid))
	{
		if (check_valid) goto VALID_FAIL_POISON;
		return false;
	}
	if (remove_from_back)
	{
		start_from_end = expr->subscript_expr.index.start_from_end = false;
	}

	if (is_eval_ref)
	{
		subscript_type = type_get_ptr(subscript_type);
	}
	else
	{
		if (sema_cast_const(index))
		{
			ASSERT_SPAN(index, expr_is_const_int(index));
			sema_cast_const(current_expr);
			bool is_const_initializer = expr_is_const_initializer(current_expr);
			if (is_const_initializer || expr_is_const_string(current_expr) || expr_is_const_bytes(current_expr))
			{
				if (!int_fits(index->const_expr.ixx, TYPE_U32))
				{
					if (check_valid) goto VALID_FAIL_POISON;
					RETURN_SEMA_ERROR(index, "Index is out of range.");
				}
				ArraySize idx = index->const_expr.ixx.i.low;
				if (!is_const_initializer)
				{
					// Handle bytes / String
					ArraySize len = current_expr->const_expr.bytes.len;
					if (idx > len || (idx == len && !start_from_end) || (idx == 0 && start_from_end))
					{
						if (check_valid) goto VALID_FAIL_POISON;
						RETURN_SEMA_ERROR(index, "The index (%s%llu) is out of range, the length is just %llu.",
										  start_from_end ? "^" : "",
										  (unsigned long long)idx,
										  (unsigned long long)current_expr->const_expr.bytes.len);
					}
					if (start_from_end) idx = len - idx;
					unsigned char c = current_expr->const_expr.bytes.ptr[idx];
					expr_rewrite_const_int(expr, type_char, c);
					expr->type = type_char;
					return true;
				}
				if (sema_subscript_rewrite_index_const_list(current_expr, idx, start_from_end, expr)) return true;
			}
		}
	}
	expr->subscript_expr.expr = exprid(current_expr);
	if (is_eval_ref && type_flatten(subscripted->type)->type_kind == TYPE_POINTER)
	{
		expr->type = type_add_optional(subscripted->type, optional);
	}
	else
	{
		expr->type = type_add_optional(subscript_type, optional);
	}
	return true;
VALID_FAIL_POISON:
	expr_poison(expr);
	return true;
}

static inline bool sema_expr_analyse_pointer_offset(SemaContext *context, Expr *expr)
{
	ASSERT_SPAN(expr, expr->expr_kind == EXPR_POINTER_OFFSET);

	// 1. Evaluate the pointer
	Expr *pointer = exprptr(expr->pointer_offset_expr.ptr);
	if (!sema_analyse_expr(context, pointer)) return false;

	// 2. Evaluate the offset.
	Expr *offset = exprptr(expr->pointer_offset_expr.offset);
	if (!sema_analyse_expr(context, offset)) return false;
	Type *flat = type_flatten(pointer->type);
	unsigned vec_len = flat->type_kind == TYPE_VECTOR ? flat->array.len : 0;

	if (!cast_implicit_binary(context, offset, vec_len ? type_get_vector(type_isz, vec_len) : type_isz, false)) return false;

	// 3. Store optionality
	bool is_optional = IS_OPTIONAL(pointer) || IS_OPTIONAL(offset);

	// 4. Possibly constant fold
	if (!vec_len && sema_cast_const(pointer) && expr_is_const_pointer(pointer) && sema_cast_const(offset))
	{
		ASSERT_SPAN(expr, !is_optional);
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

typedef enum RangeEnv
{
	RANGE_ARRAY,
	RANGE_SLICE,
	RANGE_PTR,
	RANGE_FLEXIBLE,
} RangeEnv;

INLINE bool sema_expr_analyse_range_internal(SemaContext *context, Range *range, Type *indexed_type, ArrayIndex len, RangeEnv env)
{
	Expr *start = exprptr(range->start);
	ASSERT0(start);
	Expr *end = exprptrzero(range->end);

	if (!sema_analyse_expr(context, start)) return false;
	if (end && !sema_analyse_expr(context, end)) return false;

	if (!cast_to_index(context, start, indexed_type)) return false;
	if (end && !cast_to_index(context, end, indexed_type)) return false;
	Type *end_type = end ? type_no_optional(end->type) : NULL;
	Type *start_type = type_no_optional(start->type);
	if (end && IS_OPTIONAL(end)) range->is_optional = true;
	if (IS_OPTIONAL(start)) range->is_optional = true;
	if (end && end_type != start_type)
	{
		Type *common = type_find_max_type(start_type, end_type);
		if (!common)
		{
			SourceSpan span = start->span;
			span = extend_span_with_token(span, end->span);
			sema_error_at(context, span, "No common type can be found between start and end index.");
			return false;
		}
		if (!cast_implicit(context, start, common, false) || !cast_implicit(context, end, common, false)) return false;
	}
	// Check range
	if (env != RANGE_ARRAY && env != RANGE_SLICE)
	{
		if (range->start_from_end)
		{
			RETURN_SEMA_ERROR(start, "Indexing from the end is not allowed for pointers or flexible array members.");
		}
		if (!end)
		{
			RETURN_SEMA_ERROR(start, "Omitting end index is not allowed for pointers or flexible array members.");
		}
		if (end && range->end_from_end)
		{
			RETURN_SEMA_ERROR(end, "Indexing from the end is not allowed for pointers or flexible array members.");
		}
	}
	bool end_is_const = !end || sema_cast_const(end);
	if (end && sema_cast_const(end))
	{
		// Only ArrayIndex sized
		if (!int_fits(end->const_expr.ixx, TYPE_I64))
		{
			RETURN_SEMA_ERROR(end, "The index cannot be stored in a 64-signed integer, which isn't supported.");
		}

		int64_t end_index = int_to_i64(end->const_expr.ixx);

		if (range->end_from_end)
		{
			if (end_index < 0) RETURN_SEMA_ERROR(end, "Negative numbers are not allowed when indexing from the end.");
			// Something like  1 .. ^4 with an unknown length.
			if (len < 0) return true;
			// Otherwise we fold the "from end"
			end_index = len - end_index;
			if (end_index < 0)
			{
				RETURN_SEMA_ERROR(end, "An index may only be negative for pointers (it was: %lld).", end_index);
			}
			range->end_from_end = false;
		}
		if (end_index < 0 && env != RANGE_PTR)
		{
			RETURN_SEMA_ERROR(end, "An index may only be negative for pointers (it was: %lld).", end_index);
		}
		// No more analysis
		if (end_index > MAX_ARRAYINDEX || end_index < -MAX_ARRAYINDEX) return true;
		range->const_end = end_index;
		range->range_type = range->is_len ? RANGE_CONST_LEN : RANGE_CONST_END;
	}
	else if (!end && len > 0)
	{
		range->is_len = false;
		range->const_end = len - 1;
		range->range_type = RANGE_CONST_END;
	}

	if (sema_cast_const(start))
	{
		// Only ArrayIndex sized
		if (!int_fits(start->const_expr.ixx, TYPE_I64))
		{
			RETURN_SEMA_ERROR(end, "The index cannot be stored in a 64-signed integer, which isn't supported.");
		}
		// Only ArrayIndex sized
		int64_t start_index = int_to_i64(start->const_expr.ixx);
		if (range->start_from_end)
		{
			if (start_index < 0) RETURN_SEMA_ERROR(end, "Negative numbers are not allowed when indexing from the end.");
			// Something like  ^1 .. 4 with an unknown length.
			if (len < 0) return true;
			// Otherwise we fold the "from end"
			start_index = len - start_index;
			if (start_index < 0)
			{
				RETURN_SEMA_ERROR(start, "An index may only be negative for pointers (it was: %lld).", start_index);
			}
			if (start_index > MAX_ARRAYINDEX || start_index < -MAX_ARRAYINDEX) return true;
			range->start_from_end = false;
		}
		if (start_index < 0 && env != RANGE_PTR)
		{
			RETURN_SEMA_ERROR(start, "An index may only be negative for pointers (it was: %lld).", start_index);
		}
		if (len > -1 && start_index >= len)
		{
			RETURN_SEMA_ERROR(start, "Index out of bounds: the start index was %lld, exceeding the maximum (%lld)",
							  start_index, len - 1);
		}
		if (range->range_type == RANGE_CONST_END)
		{
			int64_t end_index = range->const_end;
			if (end_index < start_index) RETURN_SEMA_ERROR(start, "The start index (%lld) should not be greater than the end index (%lld).",
														   start_index, end_index);
			if (start_index > MAX_ARRAYINDEX) return true;
			range->const_end = end_index + 1 - start_index;
			range->range_type = RANGE_CONST_LEN;
			range->is_len = true;
		}
		if (range->range_type == RANGE_CONST_LEN)
		{
			int64_t end_index = range->const_end;
			range->range_type = RANGE_CONST_RANGE;
			range->start_index = start_index;
			range->len_index = end_index;
		}
	}
	if (len > -1)
	{
		switch (range->range_type)
		{
			case RANGE_CONST_END:
				if (range->const_end >= len)
				{
					RETURN_SEMA_ERROR(end ? end : start, "End index out of bounds, was %d, exceeding max index %d.", range->const_end, len - 1);
				}
				break;
			case RANGE_CONST_LEN:
				if (range->const_end > len)
				{
					RETURN_SEMA_ERROR(end ? end : start, "Length out of bounds, was %d, exceeding max length %d.", range->const_end, len);
				}
				break;
			case RANGE_CONST_RANGE:
				if (range->len_index > len)
				{
					RETURN_SEMA_ERROR(end ? end : start, "End index out of bounds, was %d, exceeding max index %d.", range->len_index - 1, len - 1);
				}
				break;
			default:
				break;
		}
	}
	return true;
}


static inline bool sema_expr_analyse_range(SemaContext *context, Range *range, Type *indexed_type, ArrayIndex len, RangeEnv env)
{
	switch (range->status)
	{
		case RESOLVE_DONE:
			return true;
		case RESOLVE_NOT_DONE:
			range->status = RESOLVE_RUNNING;
			if (!sema_expr_analyse_range_internal(context, range, indexed_type, len, env))
			{
				range->status = RESOLVE_NOT_DONE;
				return false;
			}
			range->status = RESOLVE_DONE;
			return true;
		case RESOLVE_RUNNING:
		{
			SourceSpan span = exprptr(range->start)->span;
			if (range->end) span = extend_span_with_token(span, exprptr(range->end)->span);
			sema_error_at(context, span, "Recursive definition of range.");
			range->status = RESOLVE_NOT_DONE;
			return false;
		}
		default:
			UNREACHABLE
	}
}

static inline void sema_slice_initializer(SemaContext *context, Expr *expr, Expr *subscripted, Range *range)
{
	ConstInitializer *initializer = subscripted->const_expr.initializer;
	ASSERT0(type_is_arraylike(initializer->type));
	Type *new_type = type_get_slice(type_get_indexed_type(subscripted->type));
	// Turn zero length into an untyped list.
	if (range->len_index == 0)
	{
		expr_rewrite_const_empty_slice(expr, new_type);
		return;
	}
	bool is_vec = initializer->type->type_kind == TYPE_VECTOR;
	Type *inner_type = is_vec
			? type_get_vector(new_type->array.base, range->len_index)
			: type_get_array(new_type->array.base, range->len_index);
	initializer->type = inner_type;
	switch (initializer->kind)
	{
		case CONST_INIT_ZERO:
			break;
		case CONST_INIT_ARRAY_FULL:
			vec_erase_front(initializer->init_array_full, range->start_index);
			vec_resize(initializer->init_array_full, range->len_index);
			break;
		case CONST_INIT_ARRAY:
		{
			unsigned elements = vec_size(initializer->init_array.elements);
			for (unsigned i = 0; i < elements; i++)
			{
				ConstInitializer *element = initializer->init_array.elements[i];
				ArrayIndex index = element->init_array_value.index;
				if (index < range->start_index || index >= range->start_index + range->len_index)
				{
					vec_erase_at(initializer->init_array.elements, i);
					elements--;
					i--;
					continue;
				}
			}
			if (vec_size(initializer->init_array.elements) == 0)
			{
				initializer->kind = CONST_INIT_ZERO;
				break;
			}
			break;
		}
		case CONST_INIT_STRUCT:
		case CONST_INIT_UNION:
		case CONST_INIT_VALUE:
		case CONST_INIT_ARRAY_VALUE:
			UNREACHABLE
	}
	subscripted->const_expr.const_kind = CONST_SLICE;
	expr_replace(expr, subscripted);
	expr->type = new_type;
}

static inline bool sema_expr_analyse_slice(SemaContext *context, Expr *expr, CheckType check)
{
	ASSERT_SPAN(expr, expr->expr_kind == EXPR_SLICE);
	Expr *subscripted = exprptr(expr->slice_expr.expr);
	if (check == CHECK_LVALUE) check = CHECK_ADDRESS;
	if (!sema_analyse_expr_check(context, subscripted, check)) return false;
	bool optional = IS_OPTIONAL(subscripted);
	Type *type = type_flatten(subscripted->type);
	Type *original_type = type_no_optional(subscripted->type);
	RangeEnv env;
	switch (type->type_kind)
	{
		case TYPE_POINTER:
			env = RANGE_PTR;
			break;
		case TYPE_FLEXIBLE_ARRAY:
			env = RANGE_FLEXIBLE;
			break;
		case TYPE_SLICE:
			env = RANGE_SLICE;
			break;
		default:
			env = RANGE_ARRAY;
			break;
	}
	ArrayIndex length = sema_len_from_expr(subscripted);
	Range *range = &expr->slice_expr.range;
	if (!sema_expr_analyse_range(context, range, subscripted->type, length, env)) return false;
	if (range->is_optional) optional = true;
	if (check == CHECK_VALUE && sema_cast_const(subscripted) && range->range_type == RANGE_CONST_RANGE)
	{
		switch (subscripted->const_expr.const_kind)
		{
			case CONST_STRING:
			{
				const char *data = str_copy(subscripted->const_expr.bytes.ptr + range->start_index, range->len_index);
				expr_rewrite_const_string(expr, data);
				return true;
			}
			case CONST_BYTES:
			{
				char *data = range->len_index ? malloc_arena(range->len_index) : NULL;
				if (data)
				{
					memcpy(data, subscripted->const_expr.bytes.ptr + range->start_index, range->len_index);
				}
				subscripted->const_expr.bytes.ptr = data;
				subscripted->const_expr.bytes.len = range->len_index;
				if (type->type_kind != TYPE_SLICE)
				{
					Type *index = type_get_indexed_type(type);
					ASSERT0(index);
					original_type = type_get_slice(index);
				}
				subscripted->type = original_type;
				expr_replace(expr, subscripted);
				return true;
			}
			case CONST_UNTYPED_LIST:
				ASSERT0(!type_is_arraylike(subscripted->type));
				vec_erase_front(subscripted->const_expr.untyped_list, range->start_index);
				vec_resize(subscripted->const_expr.untyped_list, range->len_index);
				expr_replace(expr, subscripted);
				return true;
			case CONST_INITIALIZER:
				sema_slice_initializer(context, expr, subscripted, range);
				return true;
			case CONST_SLICE:
				if (!subscripted->const_expr.slice_init)
				{
					ASSERT0(range->len_index == 0);
					expr_replace(expr, subscripted);
					return true;
				}
				sema_slice_initializer(context, expr, subscripted, range);
				return true;
			case CONST_POINTER:
			case CONST_FLOAT:
			case CONST_INTEGER:
			case CONST_BOOL:
			case CONST_ENUM:
			case CONST_ERR:
			case CONST_TYPEID:
			case CONST_REF:
			case CONST_MEMBER:
				break;
		}
	}
	Expr *current_expr = subscripted;
	Type *inner_type = sema_subscript_find_indexable_type_recursively(&type, &current_expr);
	if (type == type_voidptr) inner_type = type_char;

	if (!inner_type || !type_is_valid_for_array(inner_type))
	{
		RETURN_SEMA_ERROR(subscripted, "Cannot index %s.", type_quoted_error_string(subscripted->type));
	}
	if (current_expr != subscripted)
	{
		expr->slice_expr.expr = exprid(current_expr);
	}
	// Retain the original type when doing distinct slices.
	Type *result_type = type_get_slice(inner_type);
	Type *original_type_canonical = original_type->canonical;
	if (original_type_canonical->type_kind == TYPE_DISTINCT && type_base(original_type_canonical) == result_type)
	{
		result_type = original_type;
	}
	expr->type = type_add_optional(result_type, optional);
	return true;
}


/**
 * 1. .A -> It is an enum constant.
 * 2. .foo -> It is a function.
 * 3. .@foo -> It is a macro.
 * 4. .#bar -> It is an identifier to resolve as a member or a function
 * 5. .@#bar -> It is an identifier to resolve as a macro
 * 6. .$eval(...) -> resolve the eval and retry.
 * 7. .$ident -> It is a child to resolve as CT param
 * 8. .$Type -> It is a child to resolve as CT type param
 */
 Expr *sema_expr_resolve_access_child(SemaContext *context, Expr *child, bool *missing)
{
	 SourceSpan span = child->span;
	 bool in_hash = false;
RETRY:
	switch (child->expr_kind)
	{
		case EXPR_HASH_IDENT:
			if (!sema_expr_fold_hash(context, child)) return false;
			in_hash = true;
			goto RETRY;
		case EXPR_OTHER_CONTEXT:
		{
			Expr *inner = child->expr_other_context.inner;
			context = child->expr_other_context.context;
			child = inner;
			goto RETRY;
		}
		case EXPR_IDENTIFIER:
			if (child->resolve_status == RESOLVE_DONE) goto ALREADY_RESOLVED;
			// A path is not allowed.
			if (child->identifier_expr.path) break;
			return child;
		case EXPR_CT_IDENT:
			if (child->resolve_status == RESOLVE_DONE) goto ALREADY_RESOLVED;
			return child;
		case EXPR_TYPEINFO:
			if (child->type_expr->kind == TYPE_INFO_CT_IDENTIFIER) return child;
			break;
		case EXPR_CT_EVAL:
		{
			ASSERT_SPAN(child, child->resolve_status != RESOLVE_DONE);
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
	sema_error_at(context, span, "Expected an identifier here.");
	return NULL;
ALREADY_RESOLVED:
	if (in_hash)
	{
		sema_error_at(context, span, "An expression cannot already be resolved when used as '.foo'. "
		                             "One way this might happen is if you pass a '#foo' style "
		                             "parameter that is already assigned a type when declared: 'macro @test(int #foo) { ... }'.");
		return NULL;
	}
	sema_error_at(context, span, "This expression was already resolved to an identifier before it was used.");
	return NULL;
}

static inline bool sema_expr_replace_with_enum_array(SemaContext *context, Expr *enum_array_expr, Decl *enum_decl)
{
	if (!sema_analyse_decl(context, enum_decl)) return false;
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
		ASSERT_SPAN(enum_array_expr, enum_decl->resolve_status == RESOLVE_DONE);
		expr->const_expr.enum_err_val = decl;
		ASSERT_SPAN(enum_array_expr, decl_ok(decl));
		expr->type = kind;
		expr->resolve_status = RESOLVE_DONE;
		vec_add(element_values, expr);
	}
	initializer->initializer_list = element_values;
	enum_array_expr->expr_kind = EXPR_COMPOUND_LITERAL;
	enum_array_expr->expr_compound_literal.initializer = initializer;
	enum_array_expr->expr_compound_literal.type_info = type_info_new_base(type_get_array(kind, elements), span);
	enum_array_expr->resolve_status = RESOLVE_NOT_DONE;
	return sema_analyse_expr(context, enum_array_expr);
}

static inline bool sema_expr_replace_with_enum_name_array(SemaContext *context, Expr *enum_array_expr, Decl *enum_decl)
{
	if (!sema_analyse_decl(context, enum_decl)) return false;
	Decl **values = enum_decl->enums.values;
	SourceSpan span = enum_array_expr->span;
	Expr *initializer = expr_new(EXPR_INITIALIZER_LIST, span);
	ArraySize elements = vec_size(values);
	Expr **element_values = elements > 0 ? VECNEW(Expr*, elements) : NULL;
	for (ArraySize i = 0; i < elements; i++)
	{
		Decl *decl = values[i];
		Expr *expr = expr_new(EXPR_CONST, span);
		expr_rewrite_const_string(expr, decl->name);
		vec_add(element_values, expr);
	}
	initializer->initializer_list = element_values;
	enum_array_expr->expr_kind = EXPR_COMPOUND_LITERAL;
	enum_array_expr->expr_compound_literal.initializer = initializer;
	enum_array_expr->expr_compound_literal.type_info = type_info_new_base(type_get_slice(type_string), span);
	enum_array_expr->resolve_status = RESOLVE_NOT_DONE;
	return sema_analyse_expr(context, enum_array_expr);
}

static inline bool sema_analyse_macro_func_access(SemaContext *context, Expr *expr, Decl *parent, Expr *identifier, const char *kw, bool *missing_ref)
{
	 if (kw == type_property_list[TYPE_PROPERTY_HAS_TAGOF])
	 {
		 expr->expr_kind = EXPR_TYPECALL;
		 expr->type_call_expr = (ExprTypeCall) { .type = parent, .property = TYPE_PROPERTY_HAS_TAGOF };
		 return true;
	 }
	if (kw == type_property_list[TYPE_PROPERTY_TAGOF])
	{
		expr->expr_kind = EXPR_TYPECALL;
		expr->type_call_expr = (ExprTypeCall) { .type = parent, .property = TYPE_PROPERTY_TAGOF };
		return true;
	}
	if (parent->decl_kind == DECL_MACRO)
	{
		if (missing_ref)
		{
			*missing_ref = true;
			return false;
		}
		RETURN_SEMA_ERROR(identifier, "The property '%s' is not valid on the macro '%s'.", kw, parent->name);
	}
	return sema_expr_analyse_type_access(context, expr, parent->type, identifier, missing_ref);
}

static inline Decl *sema_check_for_type_method(SemaContext *context, Expr *expr, Type *parent_type, const char *name, bool *missing_ref)
{
	ASSERT0(parent_type == parent_type->canonical);
	Decl *ambiguous = NULL;
	Decl *private = NULL;
	Decl *member = sema_resolve_type_method(context->unit, parent_type, name, &ambiguous, &private);
	if (private)
	{
		if (missing_ref)
		{
			*missing_ref = true;
		}
		else
		{
			SEMA_ERROR(expr, "The method '%s' has private visibility.", name);
		}
		return poisoned_decl;
	}
	if (ambiguous)
	{
		SEMA_ERROR(expr, "'%s' is an ambiguous name and so cannot be resolved, "
		                 "it may refer to method defined in '%s' or one in '%s'",
		           name, member->unit->module->name->module, ambiguous->unit->module->name->module);
		return poisoned_decl;
	}
	return member;
}

static inline bool sema_expr_analyse_type_access(SemaContext *context, Expr *expr, Type *parent_type, Expr *identifier, bool *missing_ref)
{
	ASSERT_SPAN(expr, identifier->expr_kind == EXPR_IDENTIFIER);
	Type *canonical = parent_type->canonical;
	const char *name = identifier->identifier_expr.ident;
	bool is_const = identifier->identifier_expr.is_const;

	if (!is_const)
	{
		TypeProperty property = type_property_by_name(name);
		if (sema_type_property_is_valid_for_type(canonical, property))
		{
			return sema_expr_rewrite_to_type_property(context, expr, canonical, type_property_by_name(name), parent_type);
		}
	}

	if (!type_may_have_sub_elements(canonical))
	{
		Decl *member = sema_check_for_type_method(context, expr, parent_type->canonical, name, missing_ref);
		if (!decl_ok(member)) return false;
		if (!member)
		{
			if (missing_ref) goto MISSING_REF;
			RETURN_SEMA_ERROR(expr, "'%s' does not have a property or method '%s'.", type_to_error_string(parent_type), name);
		}
		expr->expr_kind = EXPR_IDENTIFIER;
		expr_resolve_ident(expr, member);
		return true;
	}
	Decl *decl = canonical->decl;
	if (!decl_ok(decl)) return false;
	switch (decl->decl_kind)
	{
		case DECL_ENUM:
			if (is_const)
			{
				if (!sema_expr_analyse_enum_constant(context, expr, name, decl))
				{
					if (missing_ref) goto MISSING_REF;
					if (!decl_ok(decl)) return false;
					SEMA_ERROR(expr, "'%s' has no enumeration value '%s'.", decl->name, name);
					return false;
				}
				return true;
			}
			break;
		case DECL_FAULT:
			unit_register_external_symbol(context, decl);
			if (is_const)
			{
				if (!sema_expr_analyse_enum_constant(context, expr, name, decl))
				{
					if (missing_ref) goto MISSING_REF;
					if (decl_poison(decl)) return false;
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
		case DECL_INTERFACE:
			break;
		default:
			UNREACHABLE
	}

	Decl *member = sema_decl_stack_find_decl_member(context, decl, name, METHODS_AND_FIELDS);
	if (!decl_ok(member)) return false;
	if (!member)
	{
		member = sema_check_for_type_method(context, expr, decl->type, name, missing_ref);
		if (!decl_ok(member)) return false;
	}
	if (!member)
	{
		if (missing_ref) goto MISSING_REF;
		RETURN_SEMA_ERROR(expr, "No method or inner struct/union '%s.%s' found.", type_to_error_string(decl->type), name);
	}

	if (member->decl_kind == DECL_VAR || member->decl_kind == DECL_UNION || member->decl_kind == DECL_STRUCT || member->decl_kind == DECL_BITSTRUCT)
	{
		expr->expr_kind = EXPR_CONST;
		expr->resolve_status = RESOLVE_DONE;
		AlignSize align;
		if (!sema_set_abi_alignment(context, decl->type, &align)) return false;
		expr->const_expr = (ExprConst) {
			.member.decl = member,
			.member.align = align,
			.member.offset = decl_find_member_offset(decl, member),
			.const_kind = CONST_MEMBER
		};
		expr->type = type_member;
		return true;
	}

	expr->expr_kind = EXPR_IDENTIFIER;
	expr_resolve_ident(expr, member);
	return true;
MISSING_REF:
	*missing_ref = true;
	return false;
}

static inline bool sema_expr_analyse_member_access(SemaContext *context, Expr *expr, Expr *parent, Expr *identifier, bool *missing_ref)
{
	ASSERT_SPAN(expr, identifier->expr_kind == EXPR_IDENTIFIER);

	Decl *decl = parent->const_expr.member.decl;
	const char *name = identifier->identifier_expr.ident;
	bool is_const = identifier->identifier_expr.is_const;

	if (is_const)
	{
		if (missing_ref) goto MISSING_REF;
		RETURN_SEMA_ERROR(expr, "There is no member '%s' for %s.", name, type_to_error_string(decl->type));
	}

	if (name == kw_offsetof)
	{
		expr_rewrite_const_int(expr, type_usz, parent->const_expr.member.offset);
		return true;
	}
	if (!sema_analyse_decl(context, decl)) return false;

	TypeProperty type_property = type_property_by_name(name);
	switch (type_property)
	{
		case TYPE_PROPERTY_TAGOF:
		case TYPE_PROPERTY_HAS_TAGOF:
		case TYPE_PROPERTY_FROM_ORDINAL:
			expr->expr_kind = EXPR_TYPECALL;
			expr->type_call_expr = (ExprTypeCall) { .type = decl, .property = type_property };
			return true;
		case TYPE_PROPERTY_GET:
			expr->expr_kind = EXPR_MEMBER_GET;
			expr->member_get_expr = decl;
			expr->type = type_void;
			return true;
		case TYPE_PROPERTY_NONE:
			break;
		case TYPE_PROPERTY_QNAMEOF:
			break;
		case TYPE_PROPERTY_NAMEOF:
			expr_rewrite_const_string(expr, decl->name ? decl->name : "");
			return true;
		case TYPE_PROPERTY_ALIGNOF:
			expr_rewrite_const_int(expr, type_usz,
								   type_min_alignment(parent->const_expr.member.offset,
													  parent->const_expr.member.align));
			return true;
		case TYPE_PROPERTY_MEMBERSOF:
			sema_create_const_membersof(context, expr, decl->type->canonical, parent->const_expr.member.align, parent->const_expr.member.offset);
			return true;
		case TYPE_PROPERTY_METHODSOF:
		case TYPE_PROPERTY_KINDOF:
		case TYPE_PROPERTY_SIZEOF:
			return sema_expr_rewrite_to_type_property(context, expr, decl->type->canonical, type_property, decl->type->canonical);
		case TYPE_PROPERTY_ELEMENTS:
		case TYPE_PROPERTY_EXTNAMEOF:
		case TYPE_PROPERTY_PARAMS:
		case TYPE_PROPERTY_PARAMSOF:
		case TYPE_PROPERTY_RETURNS:
		case TYPE_PROPERTY_INF:
		case TYPE_PROPERTY_LEN:
		case TYPE_PROPERTY_MAX:
		case TYPE_PROPERTY_MIN:
		case TYPE_PROPERTY_NAN:
		case TYPE_PROPERTY_INNER:
		case TYPE_PROPERTY_NAMES:
		case TYPE_PROPERTY_VALUES:
		case TYPE_PROPERTY_ASSOCIATED:
		case TYPE_PROPERTY_PARENTOF:
		case TYPE_PROPERTY_IS_EQ:
		case TYPE_PROPERTY_IS_ORDERED:
		case TYPE_PROPERTY_IS_SUBSTRUCT:
			break;
	}

	Type *underlying_type = type_flatten(decl->type);

	if (!type_is_union_or_strukt(underlying_type) && underlying_type->type_kind != TYPE_BITSTRUCT)
	{
		if (missing_ref) goto MISSING_REF;
		RETURN_SEMA_ERROR(parent, "No member or property '%s' was found.", name);
	}

	Decl *underlying_type_decl = underlying_type->decl;
	Decl *member = sema_decl_stack_find_decl_member(context, underlying_type_decl, name, METHODS_AND_FIELDS);
	if (!decl_ok(member)) return false;
	if (!member || !(decl_is_struct_type(member) || member->decl_kind == DECL_VAR || member->decl_kind == DECL_BITSTRUCT))
	{
		if (missing_ref) goto MISSING_REF;
		RETURN_SEMA_ERROR(expr, "No member '%s' found.", name);
	}


	expr->expr_kind = EXPR_CONST;
	expr->resolve_status = RESOLVE_DONE;
	expr->const_expr = (ExprConst) {
		.member.decl = member,
		.member.align = parent->const_expr.member.align,
		.member.offset = parent->const_expr.member.offset + decl_find_member_offset(decl, member),
		.const_kind = CONST_MEMBER
	};
	expr->type = type_member;
	return true;
MISSING_REF:
	*missing_ref = true;
	return false;
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


static inline bool sema_create_const_kind(SemaContext *context, Expr *expr, Type *type)
{
	Module *module = global_context_find_module(kw_std__core__types);
	Decl *type_kind = module ? module_find_symbol(module, kw_typekind) : NULL;
	unsigned val = type_get_introspection_kind(type->type_kind);
	if (!type_kind)
	{
		// No TypeKind defined, fallback to char.
		expr_rewrite_const_int(expr, type_char, val);
		return true;
	}
	if (type_kind->resolve_status == RESOLVE_NOT_DONE && !sema_analyse_decl(context, type_kind)) return false;
	Decl **values = type_kind->enums.values;
	ASSERT_SPAN(expr, vec_size(values) > val);
	expr->type = type_kind->type;
	expr->expr_kind = EXPR_CONST;
	expr->resolve_status = RESOLVE_DONE;
	ASSERT_SPAN(expr, type_kind->resolve_status == RESOLVE_DONE);
	expr->const_expr = (ExprConst) {
		.const_kind = CONST_ENUM,
		.enum_err_val = values[val]
	};
	return true;
}

static inline bool sema_create_const_len(SemaContext *context, Expr *expr, Type *type)
{
	ASSERT_SPAN(expr, type == type_flatten(type) && "Should be flattened already.");

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
		case TYPE_INFERRED_ARRAY:
		case TYPE_FLEXIBLE_ARRAY:
		case TYPE_SLICE:
		default:
			UNREACHABLE
	}
	expr_rewrite_const_int(expr, type_usz, len);
	return true;
}

static inline bool sema_create_const_inner(SemaContext *context, Expr *expr, Type *type)
{
	if (!sema_resolve_type_decl(context, type)) return false;
	Type *inner = NULL;
	switch (type->type_kind)
	{
		case TYPE_POINTER:
			inner = type->pointer;
			break;
		case TYPE_OPTIONAL:
			inner = type->optional;
			break;
		case TYPE_DISTINCT:
			inner = type->decl->distinct->type->canonical;
			break;
		case TYPE_ENUM:
			inner = type->decl->enums.type_info->type->canonical;
			break;
		case TYPE_BITSTRUCT:
			inner = type->decl->strukt.container_type->type->canonical;
			break;
		case TYPE_ARRAY:
		case TYPE_FLEXIBLE_ARRAY:
		case TYPE_SLICE:
		case TYPE_INFERRED_ARRAY:
		case TYPE_INFERRED_VECTOR:
		case TYPE_VECTOR:
			inner = type->array.base;
			break;
		default:
			UNREACHABLE
	}
	expr_rewrite_const_typeid(expr, inner);
	return true;
}

static inline bool sema_create_const_parent(SemaContext *context, Expr *expr, Type *type)
{
	if (!sema_resolve_type_decl(context, type)) return false;
	Type *parent = type_find_parent_type(type->canonical);
	if (!parent) parent = type_void;
	expr_rewrite_const_typeid(expr, parent);
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
			case TYPE_BF16:
				expr->const_expr.fxx.f = -338953138925153547590470800371487866880.0;
				break;
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
		expr->const_expr.is_character = false;
		expr->type = type;
		expr->resolve_status = RESOLVE_DONE;
		expr->const_expr.ixx.type = flat->type_kind;
		switch (flat->type_kind)
		{
			case TYPE_I8:
				expr->const_expr.ixx.i = (Int128){ 0xFFFFFFFFFFFFFFFF, 0xFFFFFFFFFFFFFF80 };
				break;
			case TYPE_I16:
				expr->const_expr.ixx.i = (Int128){ 0xFFFFFFFFFFFFFFFF, 0xFFFFFFFFFFFF8000 };
				break;
			case TYPE_I32:
				expr->const_expr.ixx.i = (Int128){ 0xFFFFFFFFFFFFFFFF, 0xFFFFFFFF80000000 };
				break;
			case TYPE_I64:
				expr->const_expr.ixx.i = (Int128){ 0xFFFFFFFFFFFFFFFF, 1ULL << 63 };
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
	UNREACHABLE
}

static inline bool sema_create_const_params(SemaContext *context, Expr *expr, Type *type)
{
	ASSERT_SPAN(expr, type->type_kind == TYPE_FUNC_PTR);
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

static inline bool sema_create_const_paramsof(SemaContext *context, Expr *expr, Type *type)
{
	ASSERT_SPAN(expr, type->type_kind == TYPE_FUNC_PTR);
	type = type->pointer;
	Signature *sig = type->function.signature;
	unsigned params = vec_size(sig->params);
	Expr **param_exprs = params ? VECNEW(Expr*, params) : NULL;
	SourceSpan span = expr->span;
	for (unsigned i = 0; i < params; i++)
	{
		Decl *decl = sig->params[i];
		Expr *name_expr = expr_new_const_string(span, decl->name ? decl->name : "");
		Expr *type_expr = expr_new_const_typeid(span, decl->type->canonical);
		Expr **values = NULL;
		vec_add(values, name_expr);
		vec_add(values, type_expr);
		Expr *struct_value = sema_create_struct_from_expressions(type_reflected_param->decl, expr->span, values);
		vec_add(param_exprs, struct_value);
	}
	expr_rewrite_const_untyped_list(expr, param_exprs);
	return true;
}

static inline bool sema_create_const_associated(SemaContext *context, Expr *expr, Type *type)
{
	ASSERT_SPAN(expr, type->type_kind == TYPE_ENUM);
	if (!sema_analyse_decl(context, type->decl)) return false;
	Decl **associated = type->decl->enums.parameters;
	unsigned count = vec_size(associated);
	Expr **associated_exprs = count ? VECNEW(Expr*, count) : NULL;
	for (unsigned i = 0; i < count; i++)
	{
		Decl *decl = associated[i];
		Expr *expr_element = expr_new_const_typeid(expr->span, decl->type->canonical);
		vec_add(associated_exprs, expr_element);
	}
	expr_rewrite_const_untyped_list(expr, associated_exprs);
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
		members = type->decl->strukt.members;
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
		expr_element->resolve_status = RESOLVE_DONE;
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


static inline void sema_create_const_methodsof(SemaContext *context, Expr *expr, Type *type)
{
	Decl **methods = type->decl->methods;
	unsigned method_count = vec_size(methods);
	
	if (!method_count)
	{
		expr_rewrite_const_untyped_list(expr, NULL);
		return;
	}

	Expr **method_exprs = method_count ? VECNEW(Expr*, method_count) : NULL;
	for (unsigned i = 0; i < method_count; i++)
	{
		Decl *method = methods[i];
		if (method->decl_kind == DECL_FUNC)
		{
			Decl *decl = methods[i];
			size_t namestr_len = strlen(decl->name);
			const char *namestr = str_copy(decl->name, namestr_len);
			Expr *expr_element = expr_new(EXPR_CONST, expr->span);
			expr_element->resolve_status = RESOLVE_DONE;
			expr_element->type = type_string;
			expr_element->const_expr = (ExprConst) {
				.const_kind = CONST_STRING,
				.bytes.ptr = namestr,
				.bytes.len = namestr_len,
			};
			vec_add(method_exprs, expr_element);
		}
	}
	expr_rewrite_const_untyped_list(expr, method_exprs);
}


static inline bool sema_create_const_max(SemaContext *context, Expr *expr, Type *type, Type *flat)
{
	if (type_is_integer(flat))
	{
		expr->expr_kind = EXPR_CONST;
		expr->const_expr.const_kind = CONST_INTEGER;
		expr->const_expr.is_character = false;
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
			case TYPE_BF16:
				expr->const_expr.fxx.f = 338953138925153547590470800371487866880.0;
				break;
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
	UNREACHABLE
}

static bool sema_expr_rewrite_typeid_call(Expr *expr, Expr *typeid, TypeIdInfoKind kind, Type *result_type)
{
	expr->expr_kind = EXPR_TYPEID_INFO;
	expr->typeid_info_expr.parent = exprid(typeid);
	expr->typeid_info_expr.kind = kind;
	expr->type = result_type;
	return true;
}

static bool sema_expr_rewrite_to_typeid_property(SemaContext *context, Expr *expr, Expr *typeid, const char *kw, bool *was_error)
{
	TypeProperty property = type_property_by_name(kw);

	if (sema_cast_const(typeid))
	{
		Type *type = typeid->const_expr.typeid;
		if (type == NULL) return false;
		if (!sema_type_property_is_valid_for_type(type, property)) return false;
		*was_error = !sema_expr_rewrite_to_type_property(context, expr, type, property, type);
		return true;
	}
	switch (property)
	{
		case TYPE_PROPERTY_SIZEOF:
			return sema_expr_rewrite_typeid_call(expr, typeid, TYPEID_INFO_SIZEOF, type_usz);
		case TYPE_PROPERTY_LEN:
			return sema_expr_rewrite_typeid_call(expr, typeid, TYPEID_INFO_LEN, type_usz);
		case TYPE_PROPERTY_INNER:
			return sema_expr_rewrite_typeid_call(expr, typeid, TYPEID_INFO_INNER, type_typeid);
		case TYPE_PROPERTY_KINDOF:
			sema_expr_rewrite_typeid_kind(expr, typeid);
			return true;
		case TYPE_PROPERTY_PARENTOF:
			return sema_expr_rewrite_typeid_call(expr, typeid, TYPEID_INFO_PARENTOF, type_typeid);
		case TYPE_PROPERTY_NAMES:
			return sema_expr_rewrite_typeid_call(expr, typeid, TYPEID_INFO_NAMES, type_get_slice(type_string));
		case TYPE_PROPERTY_ALIGNOF:
		case TYPE_PROPERTY_ASSOCIATED:
		case TYPE_PROPERTY_ELEMENTS:
		case TYPE_PROPERTY_EXTNAMEOF:
		case TYPE_PROPERTY_FROM_ORDINAL:
		case TYPE_PROPERTY_GET:
		case TYPE_PROPERTY_HAS_TAGOF:
		case TYPE_PROPERTY_INF:
		case TYPE_PROPERTY_IS_EQ:
		case TYPE_PROPERTY_IS_ORDERED:
		case TYPE_PROPERTY_IS_SUBSTRUCT:
		case TYPE_PROPERTY_MAX:
		case TYPE_PROPERTY_MEMBERSOF:
		case TYPE_PROPERTY_METHODSOF:
		case TYPE_PROPERTY_MIN:
		case TYPE_PROPERTY_NAMEOF:
		case TYPE_PROPERTY_NAN:
		case TYPE_PROPERTY_PARAMS:
		case TYPE_PROPERTY_PARAMSOF:
		case TYPE_PROPERTY_QNAMEOF:
		case TYPE_PROPERTY_RETURNS:
		case TYPE_PROPERTY_TAGOF:
		case TYPE_PROPERTY_VALUES:
			// Not supported by dynamic typeid
		case TYPE_PROPERTY_NONE:
			return false;
	}
	UNREACHABLE
	return false;
}

static inline bool sema_expr_fold_to_index(Expr *expr, Expr *parent, SubscriptIndex index_expr)
{
	ConstInitializer *init = parent->const_expr.initializer;
	ConstInitializer *result = INVALID_PTR;
	ASSERT_SPAN(expr, !index_expr.start_from_end);
	ArrayIndex index = exprptr(index_expr.expr)->const_expr.ixx.i.low;
	switch (init->kind)
	{
		case CONST_INIT_ZERO:
			expr_rewrite_to_const_zero(expr, expr->type);
			return true;
		case CONST_INIT_STRUCT:
		case CONST_INIT_UNION:
		case CONST_INIT_VALUE:
		case CONST_INIT_ARRAY_VALUE:
			UNREACHABLE
		case CONST_INIT_ARRAY:
			result = NULL;
			FOREACH(ConstInitializer *, e, init->init_array.elements)
			{
				ArrayIndex idx = e->init_array_value.index;
				if (index != idx) continue;
				result = e->init_array_value.element;
				break;
			}
			if (!result)
			{
				expr_rewrite_to_const_zero(expr, expr->type);
				return true;
			}
			break;
		case CONST_INIT_ARRAY_FULL:
			result = init->init_array_full[index];
			break;
	}
	switch (result->kind)
	{
		case CONST_INIT_ZERO:
			expr_rewrite_to_const_zero(expr, expr->type);
			break;
		case CONST_INIT_ARRAY:
		case CONST_INIT_ARRAY_FULL:
		case CONST_INIT_STRUCT:
		case CONST_INIT_UNION:
			expr->expr_kind = EXPR_CONST;
			expr->const_expr.const_kind = CONST_INITIALIZER;
			expr->const_expr.initializer = result;
			expr->type = type_get_indexed_type(parent->type);
			break;
		case CONST_INIT_ARRAY_VALUE:
			UNREACHABLE
		case CONST_INIT_VALUE:
			expr_replace(expr, result->init_value);
			break;
	}
	return true;
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
			FOREACH_IDX(i, Decl *, other_member, type_flatten(parent->type)->decl->strukt.members)
			{
				if (other_member == member)
				{
					result = init->init_struct[i];
					goto EVAL;
				}
			}
			UNREACHABLE
		}
		case CONST_INIT_UNION:
			if (type_flatten(parent->type)->decl->strukt.members[init->init_union.index] != member) return false;
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
			expr_rewrite_to_const_zero(expr, member->type);
			break;
		case CONST_INIT_STRUCT:
		case CONST_INIT_UNION:
		case CONST_INIT_ARRAY:
		case CONST_INIT_ARRAY_FULL:
			expr->expr_kind = EXPR_CONST;
			expr->const_expr.const_kind = CONST_INITIALIZER;
			expr->const_expr.initializer = result;
			expr->type = member->type;
			break;
		case CONST_INIT_ARRAY_VALUE:
			UNREACHABLE
		case CONST_INIT_VALUE:
			expr_replace(expr, result->init_value);
			break;
	}
	return true;
}


static bool sema_type_property_is_valid_for_type(Type *original_type, TypeProperty property)
{
	Type *type = type_flatten(original_type);
	switch (property)
	{
		case TYPE_PROPERTY_NONE:
			return false;
		case TYPE_PROPERTY_GET:
			return type == type_member;
		case TYPE_PROPERTY_INF:
		case TYPE_PROPERTY_NAN:
			return type_is_float(type);
		case TYPE_PROPERTY_INNER:
			switch (original_type->type_kind)
			{
				case TYPE_POINTER:
				case TYPE_OPTIONAL:
				case TYPE_DISTINCT:
				case TYPE_ENUM:
				case TYPE_BITSTRUCT:
				case TYPE_ARRAY:
				case TYPE_FLEXIBLE_ARRAY:
				case TYPE_SLICE:
				case TYPE_INFERRED_ARRAY:
				case TYPE_INFERRED_VECTOR:
				case TYPE_VECTOR:
					return true;
				default:
					return false;
			}
		case TYPE_PROPERTY_KINDOF:
		case TYPE_PROPERTY_SIZEOF:
		case TYPE_PROPERTY_ALIGNOF:
		case TYPE_PROPERTY_NAMEOF:
		case TYPE_PROPERTY_QNAMEOF:
		case TYPE_PROPERTY_PARENTOF:
		case TYPE_PROPERTY_IS_ORDERED:
		case TYPE_PROPERTY_IS_EQ:
			return true;
		case TYPE_PROPERTY_IS_SUBSTRUCT:
			return type->type_kind == TYPE_STRUCT;
		case TYPE_PROPERTY_LEN:
			switch (type->type_kind)
			{
				case TYPE_ARRAY:
				case TYPE_VECTOR:
				case TYPE_ENUM:
				case TYPE_FAULTTYPE:
					return true;
				default:
					return false;
			}
		case TYPE_PROPERTY_FROM_ORDINAL:
			return type_kind_is_enumlike(type->canonical->type_kind);
		case TYPE_PROPERTY_MIN:
		case TYPE_PROPERTY_MAX:
			return type_is_float(type) || type_is_integer(type);
		case TYPE_PROPERTY_ELEMENTS:
		case TYPE_PROPERTY_NAMES:
		case TYPE_PROPERTY_VALUES:
			return type_kind_is_enumlike(type->type_kind);
		case TYPE_PROPERTY_ASSOCIATED:
			return type->type_kind == TYPE_ENUM;
		case TYPE_PROPERTY_MEMBERSOF:
			switch (type->type_kind)
			{
				case TYPE_STRUCT:
				case TYPE_UNION:
				case TYPE_BITSTRUCT:
					return true;
				default:
					return false;
			}
		case TYPE_PROPERTY_METHODSOF:
			switch (type->type_kind)
			{
				case TYPE_POISONED:
				case TYPE_VOID:
				case TYPE_FUNC_RAW:
				case TYPE_TYPEDEF:
				case TYPE_UNTYPED_LIST:
				case TYPE_FLEXIBLE_ARRAY:
				case TYPE_OPTIONAL:
				case TYPE_WILDCARD:
				case TYPE_TYPEINFO:
				case TYPE_MEMBER:
					return false;
				default:
					return true;
			}
		case TYPE_PROPERTY_PARAMSOF:
		case TYPE_PROPERTY_PARAMS:
		case TYPE_PROPERTY_RETURNS:
			return type_is_func_ptr(type);
		case TYPE_PROPERTY_TAGOF:
		case TYPE_PROPERTY_HAS_TAGOF:
		case TYPE_PROPERTY_EXTNAMEOF:
			return !type_is_builtin(type->type_kind);
	}
	UNREACHABLE
}

static bool sema_expr_rewrite_to_type_property(SemaContext *context, Expr *expr, Type *type, TypeProperty property,
											   Type *parent_type)
{
	ASSERT_SPAN(expr, type == type->canonical);
	ASSERT_SPAN(expr, sema_type_property_is_valid_for_type(type, property));
	Type *flat = type_flatten(type);
	switch (property)
	{
		case TYPE_PROPERTY_INF:
			ASSERT_SPAN(expr, type_is_float(flat));
			expr->expr_kind = EXPR_CONST;
			expr->const_expr.const_kind = CONST_FLOAT;
			expr->const_expr.fxx = (Float) { INFINITY, flat->type_kind };
			expr->type = type;
			expr->resolve_status = RESOLVE_DONE;
			return true;
		case TYPE_PROPERTY_IS_ORDERED:
			expr_rewrite_const_bool(expr, type_bool, type_is_ordered(flat));
			return true;
		case TYPE_PROPERTY_IS_EQ:
			expr_rewrite_const_bool(expr, type_bool, type_is_comparable(flat));
			return true;
		case TYPE_PROPERTY_IS_SUBSTRUCT:
			expr_rewrite_const_bool(expr, type_bool, type_is_substruct(flat));
			return true;
		case TYPE_PROPERTY_INNER:
			return sema_create_const_inner(context, expr, type);
		case TYPE_PROPERTY_PARENTOF:
			return sema_create_const_parent(context, expr, type);
		case TYPE_PROPERTY_KINDOF:
			return sema_create_const_kind(context, expr, type);
		case TYPE_PROPERTY_LEN:
			return sema_create_const_len(context, expr, flat);
		case TYPE_PROPERTY_MIN:
			return sema_create_const_min(context, expr, type, flat);
		case TYPE_PROPERTY_MAX:
			return sema_create_const_max(context, expr, type, flat);
		case TYPE_PROPERTY_NAMES:
			ASSERT_SPAN(expr, type_kind_is_enumlike(flat->type_kind));
			return sema_expr_replace_with_enum_name_array(context, expr, flat->decl);
		case TYPE_PROPERTY_ASSOCIATED:
			return sema_create_const_associated(context, expr, flat);
		case TYPE_PROPERTY_ELEMENTS:
			ASSERT_SPAN(expr, type_kind_is_enumlike(flat->type_kind));
			if (!sema_analyse_decl(context, type->decl)) return false;
			expr_rewrite_const_int(expr, type_isz, vec_size(flat->decl->enums.values));
			return true;
		case TYPE_PROPERTY_VALUES:
			ASSERT_SPAN(expr, type_kind_is_enumlike(flat->type_kind));
			return sema_expr_replace_with_enum_array(context, expr, flat->decl);
		case TYPE_PROPERTY_NAN:
			ASSERT_SPAN(expr, type_is_float(flat));
			expr->expr_kind = EXPR_CONST;
			expr->const_expr.const_kind = CONST_FLOAT;
			expr->const_expr.fxx = (Float) { nan(""), flat->type_kind };
			expr->type = type;
			expr->resolve_status = RESOLVE_DONE;
			return true;
		case TYPE_PROPERTY_GET:
			UNREACHABLE
		case TYPE_PROPERTY_MEMBERSOF:
		{
			AlignSize align;
			if (!sema_set_abi_alignment(context, parent_type, &align)) return false;
			sema_create_const_membersof(context, expr, flat, align, 0);
			return true;
		}
		case TYPE_PROPERTY_METHODSOF:
			sema_create_const_methodsof(context, expr, flat);
			return true;
		case TYPE_PROPERTY_PARAMSOF:
			return sema_create_const_paramsof(context, expr, flat);
		case TYPE_PROPERTY_PARAMS:
			return sema_create_const_params(context, expr, flat);
		case TYPE_PROPERTY_RETURNS:
			expr_rewrite_const_typeid(expr, type_infoptr(flat->pointer->function.signature->rtype)->type);
			return true;
		case TYPE_PROPERTY_SIZEOF:
			if (!sema_resolve_type_decl(context, type)) return false;
			expr_rewrite_const_int(expr, type_usz, type_size(type));
			return true;
		case TYPE_PROPERTY_NAMEOF:
			if (!sema_resolve_type_decl(context, type)) return false;
			sema_expr_rewrite_to_type_nameof(expr, type, TOKEN_CT_NAMEOF);
			return true;
		case TYPE_PROPERTY_QNAMEOF:
			if (!sema_resolve_type_decl(context, type)) return false;
			sema_expr_rewrite_to_type_nameof(expr, type, TOKEN_CT_QNAMEOF);
			return true;
		case TYPE_PROPERTY_ALIGNOF:
		{
			AlignSize align;
			if (!sema_set_abi_alignment(context, type, &align)) return false;
			expr_rewrite_const_int(expr, type_usz, align);
			return true;
		}
		case TYPE_PROPERTY_EXTNAMEOF:
			ASSERT_SPAN(expr, !type_is_builtin(type->type_kind));
			if (!sema_resolve_type_decl(context, type)) return false;
			sema_expr_rewrite_to_type_nameof(expr, type, TOKEN_CT_EXTNAMEOF);
			return true;
		case TYPE_PROPERTY_TAGOF:
		case TYPE_PROPERTY_HAS_TAGOF:
		case TYPE_PROPERTY_FROM_ORDINAL:
			expr->expr_kind = EXPR_TYPECALL;
			expr->type_call_expr = (ExprTypeCall) {
				.type = type->type_kind == TYPE_FUNC_PTR
						? type->pointer->function.decl
						: type->decl,
				.property = property };
			return true;
		case TYPE_PROPERTY_NONE:
			return false;
	}
	UNREACHABLE
}

static inline bool sema_expr_analyse_swizzle(SemaContext *context, Expr *expr, Expr *parent, Type *flat_type, const char *kw, unsigned len, CheckType check)
{
	unsigned vec_len = flat_type->array.len;
	Type *indexed_type = type_get_indexed_type(parent->type);
	ASSERT_SPAN(expr, len > 0);
	int index;
	for (unsigned i = 0; i < len; i++)
	{
		char val = swizzle[(int)kw[i]] - 1;
		if ((val & 0xF) >= vec_len)
		{
			RETURN_SEMA_ERROR(expr, "The '%c' component is not present in a vector of length %d, did you assume a longer vector?", kw[i], vec_len);
		}
		if (i == 0)
		{
			index = val;
		}
		if ((index ^ val) & 0x10)
		{
			RETURN_SEMA_ERROR(expr, "Mixing [xyzw] and [rgba] is not permitted, you will need to select one of them.");
		}
	}
	index &= 0xF;
	if (len == 1)
	{
		switch (check)
		{
			case CHECK_ADDRESS:
				expr->expr_kind = EXPR_SUBSCRIPT_ADDR;
				break;
			case CHECK_LVALUE:
			case CHECK_VALUE:
				expr->expr_kind = EXPR_SUBSCRIPT;
				break;
		}
		expr->subscript_expr = (ExprSubscript) {
				.index.expr = exprid(expr_new_const_int(expr->span, type_usz, index)),
				.expr = exprid(parent)
		};
		if (!sema_expr_analyse_subscript(context, expr, check, false)) return false;
		expr->resolve_status = RESOLVE_DONE;
		if (check == CHECK_ADDRESS)
		{
			expr_rewrite_insert_deref(expr);
		}
		return true;
	}
	Type *result = type_get_vector(indexed_type, len);
	expr->expr_kind = EXPR_SWIZZLE;
	expr->swizzle_expr = (ExprSwizzle) { exprid(parent), kw };
	expr->type = result;
	return true;
}

static inline bool sema_analyse_maybe_dead_expr(SemaContext *context, Expr *expr, bool is_dead, Type *infer_type)
{
	if (!is_dead || context->active_scope.is_dead)
	{
		return infer_type ? sema_analyse_inferred_expr(context, infer_type, expr) : sema_analyse_expr(context, expr);
	}
	context->active_scope.is_dead = true;
	bool success = infer_type ? sema_analyse_inferred_expr(context, infer_type, expr) : sema_analyse_expr(context, expr);
	context->active_scope.is_dead = false;
	return success;
}

static inline void sema_expr_flatten_const_ident(Expr *expr)
{
	if (expr->expr_kind != EXPR_IDENTIFIER) return;
	Decl *ident = expr->identifier_expr.decl;
	if (ident->decl_kind != DECL_VAR) return;
	switch (ident->var.kind)
	{
		case VARDECL_CONST:
		case VARDECL_LOCAL_CT:
		case VARDECL_PARAM_CT:
			break;
		case VARDECL_GLOBAL:
		case VARDECL_LOCAL:
		case VARDECL_PARAM:
		case VARDECL_MEMBER:
		case VARDECL_BITMEMBER:
		case VARDECL_PARAM_REF:
		case VARDECL_PARAM_EXPR:
		case VARDECL_UNWRAPPED:
		case VARDECL_ERASE:
		case VARDECL_REWRAPPED:
		case VARDECL_PARAM_CT_TYPE:
		case VARDECL_LOCAL_CT_TYPE:
			return;
	}
	Expr *init_expr = ident->var.init_expr;
	if (!init_expr) return;
	sema_expr_flatten_const_ident(init_expr);
	if (expr_is_const(init_expr))
	{
		expr_replace(expr, expr_copy(init_expr));
	}
}

/**
 * Analyse "x.y"
 */
static inline bool sema_expr_analyse_access(SemaContext *context, Expr *expr, bool *missing_ref, CheckType check)
{
	Expr *parent = expr->access_expr.parent;
	if (missing_ref) *missing_ref = false;

	// 1. Resolve the left hand
	if (!sema_analyse_expr_check(context, parent, check != CHECK_VALUE ? CHECK_ADDRESS : CHECK_VALUE)) return false;

	// 2. The right hand side may be a @ident or ident
	Expr *child = expr->access_expr.child;

	// 3. Handle xxxxxx.typeid
	if (child->expr_kind == EXPR_TYPEINFO)
	{
		if (child->type_expr->resolve_status != RESOLVE_DONE || child->type_expr->type != type_typeid)
		{
			RETURN_SEMA_ERROR(child, "A type can't appear here.");
		}
		if (parent->expr_kind == EXPR_IDENTIFIER && parent->type->type_kind == TYPE_FUNC_RAW)
		{
			expr->type = type_typeid;
			expr->expr_kind = EXPR_CONST;
			expr->const_expr.const_kind = CONST_TYPEID;
			expr->const_expr.typeid = parent->type;
			expr->resolve_status = RESOLVE_DONE;
			return true;
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
	Expr *identifier = sema_expr_resolve_access_child(context, child, missing_ref);
	if (!identifier) return false;
	const char *kw = identifier->identifier_expr.ident;

	// 2. If our left-hand side is a type, e.g. MyInt.abc, handle this here.
	if (parent->expr_kind == EXPR_TYPEINFO)
	{
		return sema_expr_analyse_type_access(context, expr, parent->type_expr->type, identifier, missing_ref);
	}
	if (parent->expr_kind == EXPR_IDENTIFIER)
	{
		Decl *decl = parent->identifier_expr.decl;
		switch (decl->decl_kind)
		{
			case DECL_FUNC:
			case DECL_MACRO:
				return sema_analyse_macro_func_access(context, expr, decl, identifier, kw, missing_ref);
			default:
				break;
		}
		if (parent->type->type_kind == TYPE_FUNC_RAW)
		{
			return sema_expr_analyse_type_access(context, expr, parent->type, identifier, missing_ref);
		}
	}
	if (expr_is_const_member(parent))
	{
		return sema_expr_analyse_member_access(context, expr, parent, identifier, missing_ref);
	}

	// 6. Copy failability
	bool optional = IS_OPTIONAL(parent);

	ASSERT_SPAN(expr, expr->expr_kind == EXPR_ACCESS);
	ASSERT_SPAN(expr, parent->resolve_status == RESOLVE_DONE);

	// 7. Is this a pointer? If so we insert a deref.
	Type *underlying_type = type_no_optional(parent->type)->canonical;
	if (underlying_type->type_kind == TYPE_POINTER && underlying_type != type_voidptr)
	{
		if (!sema_cast_rvalue(context, parent, true)) return false;
		expr_rewrite_insert_deref(expr->access_expr.parent);
		parent = expr->access_expr.parent;
	}

	// 8. Depending on parent type, we have some hard coded types
	Expr *current_parent = parent;

	Type *type = type_no_optional(parent->type)->canonical;
	Type *flat_type = type_flatten(type);
	if (kw_type == kw)
	{
		if (type_is_any_raw(flat_type))
		{
			expr_rewrite_to_builtin_access(expr, parent, ACCESS_TYPEOFANY, type_typeid);
			return true;
		}
		if (flat_type->type_kind == TYPE_ANYFAULT)
		{
			expr_rewrite_to_builtin_access(expr, parent, ACCESS_TYPEOFANYFAULT, type_typeid);
			return true;
		}
	}

CHECK_DEEPER:

	// 9. Fix hard coded function `len` on slices and arrays
	if (kw == kw_len)
	{
		if (flat_type->type_kind == TYPE_SLICE)
		{
			// Handle literal "foo".len which is now a slice.
			sema_expr_flatten_const_ident(parent);
			if (expr_is_const_string(parent))
			{
				expr_rewrite_const_int(expr, type_isz, parent->const_expr.bytes.len);
				return true;
			}
			expr_rewrite_slice_len(expr, parent, type_usz);
			return true;
		}
		if (flat_type->type_kind == TYPE_ARRAY || flat_type->type_kind == TYPE_VECTOR)
		{
			expr_rewrite_const_int(expr, type_isz, flat_type->array.len);
			return true;
		}
		if (flat_type->type_kind == TYPE_UNTYPED_LIST)
		{
			expr_rewrite_const_int(expr, type_isz, vec_size(current_parent->const_expr.untyped_list));
			return true;
		}
	}
	if (flat_type->type_kind == TYPE_TYPEID)
	{
		bool was_error = false;
		if (sema_expr_rewrite_to_typeid_property(context, expr, parent, kw, &was_error)) return !was_error;
	}
	if (flat_type->type_kind == TYPE_VECTOR)
	{
		unsigned len = strlen(kw);
		if (len <= 4)
		{
			for (unsigned i = 0; i < len; i++)
			{
				if (!swizzle[(int)kw[i]]) goto NOT_SWIZZLE;
			}
			// TODO should we do a missing for this as well?
			return sema_expr_analyse_swizzle(context, expr, parent, flat_type, kw, len, check);
			NOT_SWIZZLE:;
		}
	}
	// Hard coded ptr on slices and any
	if (kw == kw_ptr)
	{
		if (flat_type->type_kind == TYPE_SLICE)
		{
			expr_rewrite_ptr_access(expr, current_parent, type_get_ptr(flat_type->array.base));
			return true;
		}
		if (type_is_any_raw(flat_type))
		{
			expr_rewrite_ptr_access(expr, current_parent, type_voidptr);
			return true;
		}
	}
	if (kw == kw_ordinal)
	{
		if (flat_type->type_kind == TYPE_ENUM)
		{
			sema_expr_convert_enum_to_int(context, current_parent);
			expr_replace(expr, current_parent);
			return true;
		}
		if (flat_type->type_kind == TYPE_FAULTTYPE)
		{
			ASSERT_SPAN(expr, flat_type->decl->resolve_status == RESOLVE_DONE);

			if (sema_cast_const(current_parent))
			{
				if (current_parent->const_expr.const_kind == CONST_POINTER)
				{
					ASSERT_SPAN(expr, !current_parent->const_expr.ptr);
					expr_rewrite_const_int(expr, type_usz, 0);
					return true;
				}
				expr_rewrite_const_int(expr, type_usz, current_parent->const_expr.enum_err_val->enum_constant.ordinal + 1);
				return true;
			}
			expr_rewrite_to_builtin_access(expr, current_parent, ACCESS_FAULTORDINAL, type_usz);
			return true;
		}
	}
	if (kw == kw_nameof)
	{
		if (flat_type->type_kind == TYPE_ENUM)
		{
			if (sema_cast_const(current_parent))
			{
				expr_rewrite_const_string(expr, current_parent->const_expr.enum_err_val->name);
				return true;
			}
			else
			{
				expr_rewrite_to_builtin_access(expr, current_parent, ACCESS_ENUMNAME, type_string);
				return true;
			}
		}
		if (type_is_fault_raw(flat_type))
		{
			if (sema_cast_const(current_parent))
			{
				expr_rewrite_const_string(expr, current_parent->const_expr.enum_err_val->name);
				return true;
			}
			expr_rewrite_to_builtin_access(expr, current_parent, ACCESS_FAULTNAME, type_string);
			return true;
		}
	}

	// 9. At this point we may only have distinct, struct, union, error, enum, interface
	if (!type_may_have_sub_elements(type))
	{
		Decl *ambiguous = NULL;
		Decl *private = NULL;
		Decl *method = sema_resolve_type_method(context->unit, type, kw, &ambiguous, &private);
		if (private)
		{
			if (missing_ref) goto MISSING_REF;
			RETURN_SEMA_ERROR(expr, "The method '%s' has private visibility.", kw);
		}
		if (ambiguous)
		{
			RETURN_SEMA_ERROR(expr, "'%s' is an ambiguous name and so cannot be resolved, "
									"it may refer to method defined in '%s' or one in '%s'",
									kw, method->unit->module->name->module, ambiguous->unit->module->name->module);
		}
		if (!method)
		{
			if (missing_ref) goto MISSING_REF;
			RETURN_SEMA_ERROR(expr, "There is no member or method '%s' on '%s'", kw, type_to_error_string(parent->type));
		}
		expr->access_expr.parent = current_parent;
		expr->type = method->type ? type_add_optional(method->type, optional) : NULL;
		expr->access_expr.ref = method;
		if (method->decl_kind == DECL_FUNC) unit_register_external_symbol(context, method);
		return true;
	}

	// 10. Dump all members and methods into a decl stack.
	Decl *decl = type->decl;

	Decl *member = sema_decl_stack_find_decl_member(context, decl, kw, METHODS_AND_FIELDS);
	if (!decl_ok(member)) return false;
	if (member && decl_is_enum_kind(decl) && member->decl_kind == DECL_VAR && sema_cast_const(parent))
	{
		if (!sema_analyse_decl(context, decl)) return false;
		ASSERT_SPAN(expr, parent->const_expr.const_kind == CONST_ENUM);
		Expr *copy_init = copy_expr_single(current_parent->const_expr.enum_err_val->enum_constant.args[member->var.index]);
		expr_replace(expr, copy_init);
		ASSERT_SPAN(expr, copy_init->resolve_status == RESOLVE_DONE);
		return true;
	}
	Decl *private = NULL;
	if (!member)
	{
		Decl *ambiguous = NULL;
		member = sema_resolve_method(context->unit, decl, kw, &ambiguous, &private);
		// Look at interface parents
		if (!member && decl->decl_kind == DECL_INTERFACE)
		{
			FOREACH(TypeInfo *, parent_interface, decl->interfaces)
			{
				member = sema_resolve_method(context->unit, parent_interface->type->decl, kw, &ambiguous, &private);
				if (member) break;
			}
		}
		if (ambiguous)
		{
			ASSERT0(member);
			RETURN_SEMA_ERROR(expr, "'%s' is an ambiguous name and so cannot be resolved, it may refer to method defined in '%s' or one in '%s'",
					   kw, member->unit->module->name->module, ambiguous->unit->module->name->module);
		}
	}

	if (member && member->decl_kind == DECL_FUNC)
	{
		unit_register_external_symbol(context, member);
	}

	// 11. If we didn't find a match...
	if (!member)
	{
		// 11a. We have a potential embedded struct check:
		Expr *substruct = sema_enter_inline_member(current_parent, type);
		if (substruct)
		{
			current_parent = substruct;
			type = current_parent->type->canonical;
			flat_type = type_flatten(type);
			goto CHECK_DEEPER;
		}

		// 11b. Otherwise we give up.
		if (private)
		{
			if (missing_ref) goto MISSING_REF;
			RETURN_SEMA_ERROR(expr, "The method '%s' has private visibility.", kw);
		}
		if (parent->type->canonical->type_kind == TYPE_INTERFACE)
		{
			if (missing_ref) goto MISSING_REF;
			RETURN_SEMA_ERROR(expr, "The '%s' interface has no method '%s', did you spell it correctly?", parent->type->canonical->name, kw);
		}
		if (missing_ref) goto MISSING_REF;
		RETURN_SEMA_ERROR(expr, "There is no field or method '%s.%s'.", type_to_error_string(parent->type), kw);
	}

	if (!sema_analyse_decl(context, member)) return false;

	ASSERT_SPAN(expr, member->type);
	if (member->decl_kind == DECL_VAR)
	{
		if (member->var.kind == VARDECL_BITMEMBER)
		{
			// Transform bitstruct access to expr_bitaccess.
			expr->expr_kind = EXPR_BITACCESS;
		}
		else if (member->var.kind == VARDECL_MEMBER && expr_is_const_initializer(current_parent))
		{
			if (!sema_expr_fold_to_member(expr, current_parent, member))
			{
				RETURN_SEMA_ERROR(expr, "Could not fold to member '%s' – it wasn't the last assigned member.", member->name);
			}
			return true;
		}
	}
	// 13. Copy properties.
	expr->access_expr.parent = current_parent;
	expr->type = type_add_optional(member->type, optional);
	expr->access_expr.ref = member;
	return true;
MISSING_REF:
	*missing_ref = true;
	return false;
}

static inline Expr **sema_prepare_splat_insert(Expr **exprs, unsigned added, unsigned insert_point)
{
	if (added == 0)
	{
		vec_erase_at(exprs, insert_point);
		return exprs;
	}
	unsigned size = vec_size(exprs);
	ASSERT0(size);
	for (unsigned i = 1; i < added; i++)
	{
		vec_add(exprs, NULL);
	}
	// Move everything upwards.
	for (unsigned i = size - 1; i > insert_point; i--)
	{
		exprs[i + added - 1] = exprs[i];
	}
	return exprs;
}
static Expr **sema_vasplat_insert(SemaContext *context, Expr **init_expressions, Expr *expr, unsigned insert_point)
{
	Expr **args = context->macro_varargs;
	unsigned param_count = vec_size(args);
	Range *range = &expr->vasplat_expr;
	Expr *start = exprptrzero(range->start);
	unsigned start_idx = 0;
	if (start)
	{
		if (!range->is_range)
		{
			SEMA_ERROR(expr, "$vasplat expected a range.");
			return NULL;
		}
		if (!sema_analyse_expr(context, start)) return NULL;
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
		if (!sema_analyse_expr(context, end)) return NULL;
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

	unsigned added = end_idx - start_idx;

	// Zero splat
	if (!added)
	{
		vec_erase_at(init_expressions, insert_point);
		return init_expressions;
	}

	init_expressions = sema_prepare_splat_insert(init_expressions, added, insert_point);
	for (unsigned i = start_idx; i < end_idx; i++)
	{
		init_expressions[insert_point + i - start_idx] = copy_expr_single(args[i]);
	}
	return init_expressions;
}

Expr **sema_expand_vasplat_exprs(SemaContext *context, Expr **exprs)
{
	if (!context) return exprs;
	bool in_macro = context->current_macro;
	unsigned count = vec_size(exprs);
	bool expand;
	do
	{
		expand = false;
		for (unsigned i = 0; i < count; i++)
		{
			Expr *arg = exprs[i];
			ExprKind kind = arg->expr_kind;
			if (in_macro && kind == EXPR_VASPLAT)
			{
				exprs = sema_vasplat_insert(context, exprs, arg, i);
				// If we have null back it failed.
				if (!exprs) return NULL;
				count = vec_size(exprs);
				expand = true;
				break;
			}
			if (kind == EXPR_SPLAT)
			{
				Expr *inner = arg->inner_expr;
				if (!sema_analyse_expr(context, inner)) return false;
				Type *flat = type_flatten(inner->type);
				switch (flat->type_kind)
				{
					case TYPE_VECTOR:
					case TYPE_ARRAY:
					case TYPE_SLICE:
					case TYPE_UNTYPED_LIST:
						// These may be splatted
						break;
					default:
						SEMA_ERROR(arg, "An argument of type %s cannot be splatted.",
						           type_quoted_error_string(inner->type));
						return NULL;
				}
				ArrayIndex len = sema_len_from_expr(inner);
				if (len == -1)
				{
					SEMA_ERROR(arg,
					           "Splat may not be used with if the length is not known, but if you slice it to a constant length it will work (e.g '...val[:2]')");
					return NULL;
				}
				if (len == 0 && !expr_is_const(arg))
				{
					SEMA_ERROR(arg, "A non-constant zero size splat is not allowed.");
					return NULL;
				}
				Expr **new_args = sema_splat_arraylike_insert(context, exprs, inner, len, i);
				if (!new_args) return false;
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
	for (unsigned i = 0; i <= last; i++)
	{
		Expr *checked_expr = expr->expression_list[i];
		if (!sema_analyse_expr(context, checked_expr)) return false;
		if (i != last && !sema_expr_check_discard(context, checked_expr)) return false;
	}
	expr->type = expr->expression_list[last]->type;
	return success;
}


static inline bool sema_expr_analyse_cast(SemaContext *context, Expr *expr, bool *invalid_cast_ref)
{
	if (invalid_cast_ref) *invalid_cast_ref = false;
	Expr *inner = exprptr(expr->cast_expr.expr);
	TypeInfo *type_info = type_infoptr(expr->cast_expr.type_info);
	bool success = sema_resolve_type_info(context, type_info, RESOLVE_TYPE_DEFAULT);
	if (!sema_analyse_expr(context, inner) || !success) return false;

	Type *target_type = type_info->type;
	if (type_is_optional(target_type))
	{
		RETURN_SEMA_ERROR(type_info, "Casting to an optional type is not allowed.");
	}

	if (invalid_cast_ref)
	{
		if (!cast_explicit_silent(context, inner, target_type))
		{
			*invalid_cast_ref = true;
			return false;
		}
	}
	else
	{
		if (!cast_explicit(context, inner, target_type)) return expr_poison(expr);
	}
	expr_replace(expr, inner);
	return true;
}

static bool sema_expr_analyse_slice_assign(SemaContext *context, Expr *expr, Type *left_type, Expr *right, bool is_unwrapped)
{
	Expr *left = exprptr(expr->binary_expr.left);
	if (!sema_analyse_expr(context, right)) return false;
	if (IS_OPTIONAL(right))
	{
		RETURN_SEMA_ERROR(right, "The right hand side may not be optional when using slice assign.");
	}
	Type *base = type_flatten(left_type)->array.base;
	Type *rhs_type = type_flatten(right->type);

	switch (rhs_type->type_kind)
	{
		case TYPE_UNTYPED_LIST:
			break;
		case TYPE_VECTOR:
		case TYPE_ARRAY:
		case TYPE_SLICE:
			if (base == rhs_type->array.base) goto SLICE_COPY;
			break;
		default:
			if (!cast_implicit(context, right, base, false)) return false;
			goto SLICE_ASSIGN;
	}

	// By default we need to make a silent attempt.
	bool could_cast = cast_implicit_silent(context, right, base, false);

	// Failed, so let's go back to the original
	if (!could_cast) goto SLICE_COPY;
	// Use the copy

SLICE_ASSIGN:
	expr->expr_kind = EXPR_SLICE_ASSIGN;
	expr->type = right->type;
	expr->slice_assign_expr.left = exprid(left);
	expr->slice_assign_expr.right = exprid(right);
	return true;

SLICE_COPY:;
	Range *left_range = &left->slice_expr.range;
	IndexDiff left_len = range_const_len(left_range);
	switch (rhs_type->type_kind)
	{
		case TYPE_ARRAY:
		case TYPE_SLICE:
		case TYPE_VECTOR:
			if (rhs_type->array.base != base) goto EXPECTED;
			break;
		case TYPE_UNTYPED_LIST:
		{
			ASSERT_SPAN(expr, right->const_expr.const_kind == CONST_UNTYPED_LIST);
			// Zero sized lists cannot be right.
			unsigned count = vec_size(right->const_expr.untyped_list);
			if (!count) goto EXPECTED;
			// Cast to an array of the length.
			rhs_type = type_get_array(base, count);
			if (!cast_implicit(context, right, rhs_type, false)) return false;
			break;
		}
		default:
			goto EXPECTED;
	}
	ASSERT_SPAN(expr, right->expr_kind != EXPR_SLICE || rhs_type->type_kind == TYPE_SLICE);

	// If we have a slice operation on the right hand side, check the ranges.
	if (right->expr_kind == EXPR_SLICE)
	{
		Range *right_range = &right->slice_expr.range;
		IndexDiff right_len = range_const_len(right_range);
		if (left_len >= 0 && right_len >= 0 && left_len != right_len)
		{
			RETURN_SEMA_ERROR(expr, "Length mismatch between slices.");
		}
	}
	else
	{
		// Otherwise we want to make a slice.
		ArraySize len = rhs_type->array.len;
		if (len > 0 && left_len > 0 && left_len != len)
		{
			RETURN_SEMA_ERROR(left, "Length mismatch, expected left hand slice to be %d elements.", left_len);
		}
		Expr *inner = expr_copy(right);
		right->expr_kind = EXPR_SLICE;
		Expr *const_zero = expr_new_const_int(inner->span, type_uint, 0);
		Range range;
		if (len > 0)
		{
			range = (Range) { .status = RESOLVE_DONE, .range_type = RANGE_CONST_RANGE, .is_range = true, .is_len = true, .start_index = 0, .len_index = len };
		}
		else
		{
			range = (Range) { .start = exprid(const_zero), .is_range = true, .is_len = true };
		}
		right->slice_expr = (ExprSlice ) { .range = range, .expr = exprid(inner) };
		right->resolve_status = RESOLVE_NOT_DONE;
		if (!sema_analyse_expr(context, right)) return false;
	}
	expr->expr_kind = EXPR_SLICE_COPY;
	expr->type = left->type;
	expr->slice_assign_expr.left = exprid(left);
	expr->slice_assign_expr.right = exprid(right);
	return true;
EXPECTED:
	RETURN_SEMA_ERROR(right, "Expected an array, vector or slice with element type %s.",
					  type_quoted_error_string(base));
}

bool sema_expr_analyse_assign_right_side(SemaContext *context, Expr *expr, Type *left_type, Expr *right,
										 bool is_unwrapped, bool is_declaration)
{
	if (expr && exprptr(expr->binary_expr.left)->expr_kind == EXPR_SLICE)
	{
		return sema_expr_analyse_slice_assign(context, expr, left_type, right, is_unwrapped);
	}

	// 1. Evaluate right side to required type.
	bool to_optional = left_type && type_is_optional(left_type);
	if (!sema_analyse_expr_rhs(context, left_type, right, is_unwrapped || to_optional, NULL, is_declaration)) return false;
	if (IS_OPTIONAL(right) && !to_optional)
	{
		if (is_unwrapped)
		{
			SEMA_ERROR(exprptr(expr->binary_expr.left), "The variable is unwrapped in this context, if you don't want to unwrap it, use () around the variable to suppress unwrapping, like 'catch err = (x)' and 'try (x)'.");
			return false;
		}
		if (!left_type) left_type = type_no_optional(right->type);
		return sema_error_failed_cast(context, right, right->type, left_type);
	}

	// 3. Set the result to the type on the right side.
	if (expr) expr->type = right->type;

	return true;
}

static bool sema_expr_analyse_ct_identifier_assign(SemaContext *context, Expr *expr, Expr *left, Expr *right)
{
	// Do regular lvalue evaluation of the identifier
	if (!sema_analyse_expr_lvalue(context, left)) return false;

	// Evaluate right side to using inference from last type.
	if (!sema_analyse_inferred_expr(context, left->type, right)) return false;

	if (context->call_env.in_other)
	{
		RETURN_SEMA_ERROR(left, "Compile time variables may only be modified in the scope they are defined in.");
	}

	left->ct_ident_expr.decl->var.init_expr = right;
	expr_replace(expr, right);
	left->ct_ident_expr.decl->type = right->type;
	return true;
}

static bool sema_expr_analyse_ct_type_identifier_assign(SemaContext *context, Expr *expr, Expr *left, Expr *right)
{
	TypeInfo *info = left->type_expr;
	if (info->kind != TYPE_INFO_CT_IDENTIFIER)
	{
		RETURN_SEMA_ERROR(left, "A type cannot be assigned to.");
	}

	if (!sema_analyse_expr_value(context, right)) return false;
	if (right->expr_kind != EXPR_TYPEINFO) RETURN_SEMA_ERROR(right, "Expected a type here.");

	Decl *decl = sema_find_symbol(context, info->unresolved.name);
	if (!decl) RETURN_SEMA_ERROR(info, "'%s' is not defined in this scope yet.", info->unresolved.name);

	if (context->call_env.in_other)
	{
		RETURN_SEMA_ERROR(left, "Compile time variables may only be modified in the scope they are defined in.");
	}

	decl->var.init_expr = right;
	expr->expr_kind = EXPR_NOP;
	expr->type = type_void;

	return true;
}

static bool sema_expr_fold_hash(SemaContext *context, Expr *expr)
{
	assert(expr->expr_kind == EXPR_HASH_IDENT);
	while (expr->expr_kind == EXPR_HASH_IDENT)
	{
		ASSERT0(expr && expr->hash_ident_expr.identifier);
		DEBUG_LOG("Resolving identifier '%s'", expr->hash_ident_expr.identifier);
		Decl *decl = sema_resolve_symbol(context, expr->hash_ident_expr.identifier, NULL, expr->span);

		// Already handled
		if (!decl) return expr_poison(expr);

		ASSERT_SPAN(expr, decl->decl_kind == DECL_VAR);
		DEBUG_LOG("Replacing expr (%p) with '%s' (%p) expression resolve: %d", expr, expr_kind_to_string(decl->var.init_expr->expr_kind), decl->var.init_expr, decl->var.init_expr->resolve_status);
		expr_replace(expr, copy_expr_single(decl->var.init_expr));
		REMINDER("Handle inlining at");
	}
	return expr_ok(expr);
}
/**
 * Analyse a = b
 * @return true if analysis works
 */
static bool sema_expr_analyse_assign(SemaContext *context, Expr *expr, Expr *left, Expr *right, bool *failed_ref)
{
RETRY:;
	// 1. Evaluate left side
	switch (left->expr_kind)
	{
		case EXPR_HASH_IDENT:
			if (!sema_expr_fold_hash(context, left)) return false;
			goto RETRY;
		case EXPR_CT_IDENT:
			// $foo = ...
			return sema_expr_analyse_ct_identifier_assign(context, expr, left, right);
		case EXPR_TYPEINFO:
			// $Foo = ...
			return sema_expr_analyse_ct_type_identifier_assign(context, expr, left, right);
		case EXPR_SUBSCRIPT:
			// abc[...] = ...
			if (!sema_analyse_expr_lvalue(context, left)) return false;
			break;
		default:
			if (!sema_analyse_expr_lvalue(context, left)) return false;
			break;
	}
	// 2. Check assignability
	if (!sema_expr_check_assign(context, left, failed_ref)) return false;

	bool is_unwrapped_var = expr_is_unwrapped_ident(left);

	// 3. Evaluate right side to required type.
	if (!sema_expr_analyse_assign_right_side(context, expr, left->type, right, is_unwrapped_var, false)) return false;

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
		if (!sema_bit_assignment_check(context, right, left->access_expr.ref)) return false;
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
	if (!sema_analyse_expr_lvalue(context, left)) return false;

	Decl *left_var = left->ct_ident_expr.decl;
	if (!sema_cast_ct_ident_rvalue(context, left)) return false;

	if (context->call_env.in_other)
	{
		RETURN_SEMA_ERROR(left, "Compile time variables may only be modified in the scope they are defined in.");
	}
	expr->binary_expr.operator = binaryop_assign_base_op(expr->binary_expr.operator);

	if (!sema_expr_analyse_binary(context, expr, NULL)) return false;
	expr->resolve_status = RESOLVE_DONE;

	if (!sema_cast_const(expr))
	{
		RETURN_SEMA_ERROR(exprptr(expr->binary_expr.right), "Expected a constant expression.");
	}

	left_var->var.init_expr = expr;
	left->type = expr->type;
	return true;
}

/**
 * Analyse *= /= %= ^= |= &= += -=
 *
 * @return true if analysis worked.
 */
static bool sema_expr_analyse_op_assign(SemaContext *context, Expr *expr, Expr *left, Expr *right, bool int_only,
                                        bool allow_bitstruct, bool is_add_sub)
{
	if (left->expr_kind == EXPR_CT_IDENT)
	{
		return sema_binary_analyse_ct_common_assign(context, expr, left);
	}

	// 1. Analyse left side.
	if (!sema_analyse_expr_lvalue(context, left)) return false;

	// 2. Verify that the left side is assignable.
	if (!sema_expr_check_assign(context, left, NULL)) return false;

	Type *left_type_canonical = left->type->canonical;

	// 3. Check that it is readable
	if (!sema_cast_rvalue(context, left, false)) return false;

	Type *no_fail = type_no_optional(left->type);
	Type *flat = type_flatten(no_fail);

	// 3. If this is only defined for ints (*%, ^= |= &= %=) verify that this is an int.
	if (int_only && !type_flat_is_intlike(flat))
	{
		if (allow_bitstruct && flat->type_kind == TYPE_BITSTRUCT) goto BITSTRUCT_OK;
		RETURN_SEMA_ERROR(left, "Expected an integer here, not a value of type %s.", type_quoted_error_string(left->type));
	}

	// 4. In any case, these ops are only defined on numbers.
	if (!type_underlying_is_numeric(flat) && !(is_add_sub && type_underlying_may_add_sub(left->type)))
	{
		RETURN_SEMA_ERROR(left, "Expected a numeric type here, not a value of type %s.", type_quoted_error_string(left->type));
	}

BITSTRUCT_OK:

	// 5. Analyse RHS
	if (!sema_analyse_expr(context, right)) return false;

	// 3. Copy type & set properties.
	if (IS_OPTIONAL(right) && !IS_OPTIONAL(left))
	{
		RETURN_SEMA_ERROR(right, "Cannot assign an optional value to a non-optional.");
	}

	expr->type = left->type;
	bool optional = IS_OPTIONAL(left) || IS_OPTIONAL(right);

	// 5. In the pointer case we have to treat this differently.
	if (flat->type_kind == TYPE_ENUM)
	{
		if (type_flat_distinct_inline(no_fail)->type_kind != TYPE_ENUM)
		{
			RETURN_SEMA_ERROR(expr, "A value of type %s cannot be added to or subtracted from.", type_quoted_error_string(left->type));
		}
		// 7. Finally, check that the right side is indeed an integer.
		if (!type_is_integer(right->type->canonical))
		{
			RETURN_SEMA_ERROR(right,
			                  "The right side was '%s' but only integers are valid on the right side of %s when the left side is an enum.",
			                  type_to_error_string(right->type),
			                  token_type_to_string(binaryop_to_token(expr->binary_expr.operator)));
		}
		if (!cast_implicit(context, right, flat->decl->enums.type_info->type, false)) return false;
		goto END;
	}
	if (type_is_pointer_like(flat))
	{
		// Not inline pointer-like
		if (!type_is_pointer_like(no_fail))
		{
			RETURN_SEMA_ERROR(expr, "A value of type %s cannot be added to or subtracted from.", type_quoted_error_string(left->type));
		}
		// 7. Finally, check that the right side is indeed an integer.
		if (!type_is_integer(right->type->canonical))
		{
			RETURN_SEMA_ERROR(right,
			                  "The right side was '%s' but only integers are valid on the right side of %s when the left side is a pointer.",
			                  type_to_error_string(right->type),
			                  token_type_to_string(binaryop_to_token(expr->binary_expr.operator)));
		}
		goto END;
	}

	if (flat->type_kind == TYPE_ENUM)
	{
		if (!cast_implicit(context, right, type_base(flat), false)) return false;
		goto END;
	}

	// Otherwise cast left to right.
	if (!cast_implicit_binary(context, right, no_fail, false)) return false;

	// 6. Check for zero in case of div or mod.
	if (sema_cast_const(right))
	{
		if (expr->binary_expr.operator == BINARYOP_DIV_ASSIGN)
		{
			switch (right->const_expr.const_kind)
			{
				case CONST_INTEGER:
					if (int_is_zero(right->const_expr.ixx)) RETURN_SEMA_ERROR(right, "Division by zero not allowed.");
					break;
				case CONST_FLOAT:
					if (right->const_expr.fxx.f == 0) RETURN_SEMA_ERROR(right, "Division by zero not allowed.");
					break;
				default:
					break;
			}
		}
		else if (expr->binary_expr.operator == BINARYOP_MOD_ASSIGN)
		{
			switch (right->const_expr.const_kind)
			{
				case CONST_INTEGER:
					if (int_is_zero(right->const_expr.ixx))
					{
						RETURN_SEMA_ERROR(right, "% by zero not allowed.");
					}
					break;
				default:
					break;
			}
		}
	}

	if (left->expr_kind == EXPR_BITACCESS)
	{
		expr->expr_kind = EXPR_BITASSIGN;
	}

END:
	// Handle the subscript assign variant.
	if (left->expr_kind == EXPR_SUBSCRIPT_ASSIGN)
	{
		return sema_analyse_assign_mutate_overloaded_subscript(context, expr, left, left_type_canonical);
	}
	// 7. Assign type
	expr->type = type_add_optional(left->type, optional);
	return true;
}



static bool sema_binary_arithmetic_promotion(SemaContext *context, Expr *left, Expr *right, Type *left_type, Type *right_type,
											 Expr *parent, const char *error_message, bool allow_bool_vec)
{
	Type *max = cast_numeric_arithmetic_promotion(type_find_max_type(left_type, right_type));
	if (!max || (!type_underlying_is_numeric(max) && !(allow_bool_vec && type_flat_is_bool_vector(max))))
	{
		if (!error_message)
		{
			return sema_type_error_on_binop(context, parent);
		}
		SEMA_ERROR(parent, error_message, type_quoted_error_string(left->type), type_quoted_error_string(right->type));
		return false;
	}
	return cast_implicit_binary(context, left, max, false) &&
		   cast_implicit_binary(context, right, max, false);
}

static void sema_binary_unify_voidptr(SemaContext *context, Expr *left, Expr *right, Type **left_type_ref, Type **right_type_ref)
{
	if (*left_type_ref == *right_type_ref) return;
	if (*left_type_ref == type_voidptr)
	{
		cast_no_check(context, left, *right_type_ref, IS_OPTIONAL(left));
		*left_type_ref = *right_type_ref;
	}
	if (*right_type_ref == type_voidptr)
	{
		cast_no_check(context, right, *left_type_ref, IS_OPTIONAL(right));
		*right_type_ref = *left_type_ref;
	}
}

static Type *defer_iptr_cast(Expr *maybe_pointer, Expr *maybe_diff)
{
	// Do we have (iptr)(ptr) +- rhs? If so we change it to
	// (iptr)((char*)(ptr) +- 1)
	if (maybe_pointer->expr_kind == EXPR_PTR_TO_INT
		&& type_flatten(maybe_pointer->type) == type_flatten(type_iptr))
	{
		maybe_pointer->expr_kind = EXPR_RVALUE;
		Type *cast_to_iptr = maybe_pointer->type;
		maybe_pointer->type = type_get_ptr(type_char);
		return cast_to_iptr;
	}
	return NULL;
}

static bool sema_expr_analyse_enum_add_sub(SemaContext *context, Expr *expr, Expr *left, Expr *right)
{
	Type *left_type = type_no_optional(left->type)->canonical;
	bool is_sub = expr->binary_expr.operator == BINARYOP_SUB;
	bool swapped = false;
	if (left_type->type_kind != TYPE_ENUM)
	{
		if (is_sub)
		{
			RETURN_SEMA_ERROR(right, "You can't subtract an enum from a value.");
		}
		Expr *temp = right;
		right = left;
		left = temp;
		left_type = type_no_optional(left->type)->canonical;
	}
	Type *right_type = type_no_optional(right->type)->canonical;

	// Enum - Enum / Enum + Enum
	if (right_type->type_kind == TYPE_ENUM)
	{
		if (!is_sub) SEMA_DEPRECATED(expr, "Adding two enums is deprecated.");
		if (left_type != right_type)
		{

			RETURN_SEMA_ERROR(expr, is_sub ? "Cannot subtract %s from %s" : "Cannot add %s to %s",
			                  type_quoted_error_string(left->type),
			                  type_quoted_error_string(right->type));
		}
		Type *underlying_type = left_type->decl->enums.type_info->type;
		sema_expr_convert_enum_to_int(context, left);
		sema_expr_convert_enum_to_int(context, right);
		expr->type = type_add_optional(underlying_type, IS_OPTIONAL(left) || IS_OPTIONAL(right));
		if (expr_both_const(left, right))
		{
			if (is_sub)
			{
				expr->const_expr.ixx = int_sub(left->const_expr.ixx, right->const_expr.ixx);
			}
			else
			{
				expr->const_expr.ixx = int_add(left->const_expr.ixx, right->const_expr.ixx);
			}
			expr->const_expr.const_kind = CONST_INTEGER;
			expr->const_expr.is_character = false;
			expr->expr_kind = EXPR_CONST;
			expr->resolve_status = RESOLVE_DONE;
		}
		return true;
	}

	// Enum - value / Enum + value
	sema_expr_convert_enum_to_int(context, left);
	if (!cast_implicit(context, right, left->type, true)) return false;
	expr->type = type_add_optional(left_type, IS_OPTIONAL(left) || IS_OPTIONAL(right));
	if (expr_both_const(left, right))
	{
		Int i;
		if (is_sub)
		{
			i = int_sub(left->const_expr.ixx, right->const_expr.ixx);
		}
		else
		{
			i = int_add(left->const_expr.ixx, right->const_expr.ixx);
		}
		Decl **enums = left_type->decl->enums.values;
		if (int_is_neg(i) || int_ucomp(i, vec_size(enums), BINARYOP_GE))
		{
			SEMA_ERROR(expr, "This does not result in a valid enum. If you want to do the %s, cast the enum to an integer.",
					   is_sub ? "subtraction" : "addition");
			return false;
		}
		ASSERT_SPAN(expr, left_type->decl->resolve_status == RESOLVE_DONE);
		expr->const_expr = (ExprConst) { .const_kind = CONST_ENUM, .enum_err_val = enums[int_to_i64(i)] };
		expr->expr_kind = EXPR_CONST;
		expr->resolve_status = RESOLVE_DONE;
	}
	return true;

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

	bool left_is_pointer_vector = type_is_pointer_vector(left_type);
	bool left_is_pointer = left_is_pointer_vector || left_type->type_kind == TYPE_POINTER;
	// 2. Handle the ptr - x and ptr - other_pointer
	if (left_is_pointer)
	{
		ArraySize vec_len = left_is_pointer_vector ? left_type->array.len : 0;
		// We restore the type to ensure distinct types are tested against each other.
		left_type = type_no_optional(left->type)->canonical;

		bool right_is_pointer_vector = type_is_pointer_vector(right_type);
		bool right_is_pointer = right_is_pointer_vector || right_type->type_kind == TYPE_POINTER;

		Type *offset_type = vec_len ? type_get_vector(type_isz, vec_len) : type_isz;

		// 3. ptr - other pointer
		if (right_is_pointer)
		{

			// Restore the type
			right_type = type_no_optional(right->type)->canonical;

			// 3a. Require that both types are the same.
			sema_binary_unify_voidptr(context, left, right, &left_type, &right_type);
			if (left_type != right_type)
			{
				SEMA_ERROR(expr, "'%s' - '%s' is not allowed. Subtracting pointers of different types is not allowed.", type_to_error_string(left_type), type_to_error_string(right_type));
				return false;
			}

			if (!right_is_pointer_vector && !left_is_pointer_vector
				&& expr_both_const(left, right) && sema_constant_fold_ops(left) && sema_constant_fold_ops(right))
			{
				expr_rewrite_const_int(expr, type_isz, (left->const_expr.ptr - right->const_expr.ptr) /
													   type_size(left_type->pointer));
				return true;
			}
			// 3b. Set the type
			expr->type = offset_type;
			return true;
		}

		right_type = right->type->canonical;

		bool right_is_vector = right_type->type_kind == TYPE_VECTOR;
		// 4. Check that the right hand side is an integer.
		if (!type_flat_is_intlike(right_type))
		{
			SEMA_ERROR(expr, "Cannot subtract '%s' from '%s'", type_to_error_string(right_type), type_to_error_string(left_type));
			return false;
		}

		// 5. Make sure that the integer does not exceed isz in size.
		ArraySize max_size = right_is_vector ? type_size(offset_type) : type_size(type_isz);
		if (type_size(right_type) > max_size)
		{
			RETURN_SEMA_ERROR(expr, "Cannot subtract %s from a %s, you need to add an explicit a narrowing cast to %s.",
							  type_quoted_error_string(right->type),
							  left_is_pointer_vector ? "pointer vector" : "pointer",
							  type_quoted_error_string(right_is_vector ? offset_type : type_isz));
		}

		// 6. Convert to isz
		if (!cast_implicit_binary(context, right, offset_type, false)) return true;

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
			return cast_explicit(context, expr, cast_to_iptr);
		}

		return true;
	}

	// Enum - Enum and Enum - int
	if (left_type->type_kind == TYPE_ENUM)
	{
		return sema_expr_analyse_enum_add_sub(context, expr, left, right);
	}

	left_type = type_no_optional(left->type)->canonical;
	right_type = type_no_optional(right->type)->canonical;

	// 7. Attempt arithmetic promotion, to promote both to a common type.
	if (!sema_binary_arithmetic_promotion(context,
										  left,
										  right,
										  left_type,
										  right_type,
										  expr,
										  "The subtraction %s - %s is not possible.", false))
	{
		return false;
	}

	left_type = left->type->canonical;

	expr->type = type_add_optional(left->type, IS_OPTIONAL(right));

	// 8. Handle constant folding.
	if (expr_both_const(left, right) && sema_constant_fold_ops(left))
	{
		Type *type = expr->type;
		expr_replace(expr, left);
		expr->type = type;
		switch (left->const_expr.const_kind)
		{
			case CONST_INTEGER:
				expr->const_expr.ixx = int_sub(left->const_expr.ixx, right->const_expr.ixx);
				break;
			case CONST_FLOAT:
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

	bool right_is_pointer = type_is_pointer_like(right_type);
	bool left_is_pointer = type_is_pointer_like(left_type);
	// 2. To detect pointer additions, reorder if needed
	if (right_is_pointer && !left_is_pointer)
	{
		Expr *temp = right;
		right = left;
		left = temp;
		right_type = left_type;
		left_type = left->type->canonical;
		left_is_pointer = true;
		right_is_pointer = false;
		(void)right_is_pointer;
		expr->binary_expr.left = exprid(left);
		expr->binary_expr.right = exprid(right);
	}

	// 3. The "left" will now always be the pointer.
	//    so check if we want to do the normal pointer add special handling.
	if (left_is_pointer)
	{
		bool left_is_vec = left_type->type_kind == TYPE_VECTOR;
		bool right_is_vec = right_type->type_kind == TYPE_VECTOR;
		ArraySize vec_len = left_is_vec ? left_type->array.len : 0;
		// 3a. Check that the other side is an integer of some sort.
		if (!type_is_integer(right_type))
		{
			if (!left_is_vec || !right_is_vec || !type_is_integer(right_type->array.base))
			{
				RETURN_SEMA_ERROR(right, "A value of type '%s' cannot be added to '%s', an integer was expected here.",
								  type_to_error_string(right->type),
								  type_to_error_string(left->type));
			}
		}

		// 3b. Cast it to usz or isz depending on underlying type.
		//     Either is fine, but it looks a bit nicer if we actually do this and keep the sign.
		bool success = cast_explicit(context, right, left_is_vec ? type_get_vector(type_isz, vec_len) : type_isz);

		// No need to check the cast we just ensured it was an integer.
		ASSERT_SPAN(expr, success && "This should always work");
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
			return cast_explicit(context, expr, cast_to_iptr);
		}
		return true;
	}

	if (left_type->type_kind == TYPE_ENUM || right_type->type_kind == TYPE_ENUM)
	{
		return sema_expr_analyse_enum_add_sub(context, expr, left, right);
	}

	left_type = type_no_optional(left->type)->canonical;
	right_type = type_no_optional(right->type)->canonical;

	ASSERT_SPAN(expr, !cast_to_iptr);
	// 4. Do a binary arithmetic promotion
	if (!sema_binary_arithmetic_promotion(context,
										  left,
										  right,
										  left_type,
										  right_type,
										  expr,
										  "Cannot do the addition %s + %s.", false))
	{
		return false;
	}

	// 5. Handle the "both const" case. We should only see ints and floats at this point.
	if (expr_both_const(left, right) && sema_constant_fold_ops(left))
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
	if (expr_both_const(left, right) && sema_constant_fold_ops(left))
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
	if (sema_cast_const(right))
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
			case CONST_INITIALIZER:
				// Do not analyse
				break;
			default:
				UNREACHABLE
		}
	}

	// 3. Perform constant folding.
	if (expr_both_const(left, right) && sema_constant_fold_ops(left))
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

	Type *flat = type_flatten(left->type);
	if (type_is_float(flat))
	{
		// 3. a % 0 is not valid, so detect it.
		if (sema_cast_const(right) && right->const_expr.fxx.f == 0.0)
		{
			RETURN_SEMA_ERROR(right, "Cannot perform %% with a constant zero.");
		}

		// 4. Constant fold
		if (expr_both_const(left, right) && sema_constant_fold_ops(left))
		{
			expr_replace(expr, left);
			// 4a. Remember this is remainder.
			expr->const_expr.fxx = float_rem(left->const_expr.fxx, right->const_expr.fxx);
		}
	}
	else if (type_is_integer(flat))
	{
		// 3. a % 0 is not valid, so detect it.
		if (sema_cast_const(right) && int_is_zero(right->const_expr.ixx)) RETURN_SEMA_ERROR(right, "Cannot perform %% with a constant zero.");

		// 4. Constant fold
		if (expr_both_const(left, right) && sema_constant_fold_ops(left))
		{
			expr_replace(expr, left);
			// 4a. Remember this is remainder.
			expr->const_expr.ixx = int_rem(left->const_expr.ixx, right->const_expr.ixx);
		}
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
	bool is_bitstruct = left->type->canonical->type_kind == TYPE_BITSTRUCT;

	if (!is_bool && !is_bitstruct && !expr_both_any_integer_or_integer_bool_vector(left, right))
	{
		return sema_type_error_on_binop(context, expr);
	}

	// 3. Do constant folding if both sides are constant.
	if (expr_both_const(left, right) && (sema_constant_fold_ops(left) || is_bitstruct))
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
		else if (is_bitstruct)
		{
			ConstInitializer *merged = sema_merge_bitstruct_const_initializers(left->const_expr.initializer,
																			   right->const_expr.initializer, op);
			expr->const_expr.initializer = merged;
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
		return sema_type_error_on_binop(context, expr);
	}

	// 3. Promote lhs using the usual numeric promotion.
	if (!cast_implicit_binary(context, left, cast_numeric_arithmetic_promotion(type_no_optional(left->type)), false)) return false;

	// 4. For a constant rhs side we will make a series of checks.
	if (sema_cast_const(right))
	{
		// 4a. Make sure the value does not exceed the bitsize of
		//     the left hand side. We ignore this check for lhs being a constant.
		Type *left_type_flat = type_flatten(left->type);
		ASSERT_SPAN(expr, type_kind_is_any_integer(left_type_flat->type_kind));
		if (int_ucomp(right->const_expr.ixx, left_type_flat->builtin.bitsize, BINARYOP_GE))
		{
			SEMA_ERROR(right, "The shift is not less than the bitsize of %s.", type_quoted_error_string(type_no_optional(left->type)));
			return false;
		}

		// 4b. Make sure that the RHS is positive.
		if (int_is_neg(right->const_expr.ixx))
		{
			SEMA_ERROR(right, "A shift must be a positive number.");
			return false;
		}

		// 5. Fold constant expressions.
		if (sema_cast_const(left))
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

	bool optional = IS_OPTIONAL(left) || IS_OPTIONAL(right);

	// 2. Ensure the lhs side is assignable
	if (!sema_expr_check_assign(context, left, NULL)) return false;

	// 3. Only integers may be shifted.
	if (!expr_both_any_integer_or_integer_vector(left, right)) return sema_type_error_on_binop(context, expr);

	// 4. For a constant right hand side we will make a series of checks.
	if (sema_cast_const(right))
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
	expr->type = type_add_optional(left->type, optional);
	return true;
}


static bool sema_expr_analyse_and_or(SemaContext *context, Expr *expr, Expr *left, Expr *right)
{
	if (!sema_binary_analyse_subexpr(context, expr, left, right)) return false;
	if (!cast_explicit(context, left, type_bool) || !cast_explicit(context, right, type_bool)) return false;

	if (expr_both_const(left, right) && sema_constant_fold_ops(left))
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
	expr->type = type_add_optional(type_bool, IS_OPTIONAL(left) || IS_OPTIONAL(right));
	return true;
}




static bool sema_binary_is_unsigned_always_same_comparison(SemaContext *context, Expr *expr, Expr *left, Expr *right,
														   Type *lhs_type, Type *rhs_type)
{
	if (context->active_scope.flags & (SCOPE_MACRO | SCOPE_ENSURE | SCOPE_ENSURE_MACRO)) return true;
	if (!sema_cast_const(left) && !sema_cast_const(right)) return true;
	if (!type_is_integer(left->type)) return true;
	if (expr_is_const(left) && type_is_unsigned(rhs_type))
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
				RETURN_SEMA_ERROR(right,
								  "Comparing '0 > unsigned expression' can never be true, and is only allowed inside of macro expansions.");
				return false;
			case BINARYOP_GE:
				RETURN_SEMA_ERROR(right,
								  "Comparing '0 >= unsigned expression' is the same as 0 == expr and is a common bug, "
								  "for this reason it is only allowed inside of macro expansions.");
			case BINARYOP_LE:
				RETURN_SEMA_ERROR(right,
								  "Comparing '0 <= unsigned expression' is always true and is a common bug, "
								  "for this reason it is only allowed inside of macro expansions.");
			default:
				return true;
		}
	}
	if (!expr_is_const(right) || !type_is_unsigned(lhs_type)) return true;
	if (int_is_neg(right->const_expr.ixx))
	{
		SEMA_ERROR(right, "Comparing an unsigned value with a negative constant is only allowed inside of macros.");
		return false;
	}
	if (!int_is_zero(right->const_expr.ixx)) return true;
	switch (expr->binary_expr.operator)
	{
		case BINARYOP_LT:
			RETURN_SEMA_ERROR(right,
							  "Comparing 'unsigned expression < 0' can never be true, and is only allowed inside of macro expansions.");
			return false;
		case BINARYOP_LE:
			RETURN_SEMA_ERROR(right,
							  "Comparing 'unsigned expression <= 0' is the same as expr == 0 and is a common bug, "
							  "for this reason it is only allowed inside of macro expansions.");
		case BINARYOP_GE:
			RETURN_SEMA_ERROR(right,
							  "Comparing 'unsigned expression >= 0' is always true and is a common bug, "
							  "for this reason it is only allowed inside of macro expansions.");
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

	// Flatten distinct/optional
	Type *left_type = type_flat_distinct_inline(type_no_optional(left->type)->canonical)->canonical;
	Type *right_type = type_flat_distinct_inline(type_no_optional(right->type)->canonical)->canonical;

	// 2. Handle the case of signed comparisons.
	//    This happens when either side has a definite integer type
	//    and those are either signed or unsigned.
	//    If either side is compint, then this does not happen.
	if ((type_is_unsigned(left_type) && type_is_signed(right_type))
		|| (type_is_signed(left_type) && type_is_unsigned(right_type)))
	{
		// 2a. Resize so that both sides have the same bit width. This will always work.
		cast_to_int_to_max_bit_size(context, left, right, left_type, right_type);
		goto DONE;
	}

	// 3. In the normal case, treat this as a binary op, finding the max type.
	Type *max = type_find_max_type(left_type, right_type);

	// 4. If no common type, then that's an error:
	if (!max)
	{
		RETURN_SEMA_ERROR(expr, "%s and %s are different types and cannot be compared.",
						  type_quoted_error_string(left->type), type_quoted_error_string(right->type));
	}

	max = max->canonical;

	if (max->type_kind == TYPE_VECTOR && !is_equality_type_op)
	{
		RETURN_SEMA_ERROR(expr, "Vector types can only be tested for equality, for other comparison, use vector comparison functions.");
	}

	if (!type_is_comparable(max))
	{
		if (type_is_user_defined(max))
		{
			RETURN_SEMA_ERROR(expr,
							  "%s does not support comparisons, you need to manually implement a comparison if you need it.",
							  type_quoted_error_string(left->type));
		}
		RETURN_SEMA_ERROR(expr, "%s does not support comparisons.",
						  type_quoted_error_string(left->type));
	}

	if (!is_equality_type_op)
	{
		if (!type_is_ordered(max))
		{
			RETURN_SEMA_ERROR(expr, "%s can only be compared using '!=' and '==' it "
									"cannot be ordered, did you make a mistake?",
							  type_quoted_error_string(left->type));
		}
		if (type_is_pointer_type(max))
		{

			// Only comparisons between the same type is allowed. Subtypes not allowed.
			if (left_type != right_type && left_type != type_voidptr && right_type != type_voidptr)
			{
				RETURN_SEMA_ERROR(expr, "You are not allowed to compare pointers of different types, "
										"if you need to do, first convert all pointers to void*.");
			}
		}
	}

	// 6. Do the explicit cast.
	if (!cast_implicit(context, left, max, false) || !cast_implicit(context, right, max, false)) return false;
	bool success = cast_explicit(context, left, max) && cast_explicit(context, right, max);
	ASSERT_SPAN(expr, success);
DONE:

	// 7. Do constant folding.
	if (expr_both_const(left, right) && sema_constant_fold_ops(left))
	{
		expr->const_expr.b = expr_const_compare(&left->const_expr, &right->const_expr, expr->binary_expr.operator);
		expr->const_expr.const_kind = CONST_BOOL;
		expr->expr_kind = EXPR_CONST;
		expr->resolve_status = RESOLVE_DONE;
	}
	else
	{
		if (!sema_binary_is_unsigned_always_same_comparison(context, expr, left, right, left_type, right_type)) return false;
	}

	// 8. Set the type to bool
	expr->type = type_add_optional(type_bool, IS_OPTIONAL(left) || IS_OPTIONAL(right));

	return true;
}

/**
 * Analyse *a
 * @return true if analysis succeeds.
 */
static inline bool sema_expr_analyse_deref(SemaContext *context, Expr *expr, bool *failed_ref)
{
	// 1. Check the inner expression
	Expr *inner = expr->unary_expr.expr;
	if (!sema_analyse_expr(context, inner)) return false;

	Type *inner_type_nofail = type_no_optional(inner->type);
	Type *canonical = inner_type_nofail->canonical;

	// 2. Check that we have a pointer, or dereference is not allowed.
	if (canonical->type_kind != TYPE_POINTER)
	{
		if (failed_ref) goto ON_FAILED;
		RETURN_SEMA_ERROR(inner, "Cannot dereference a value of type %s, it must be a pointer.",
						  type_quoted_error_string(inner_type_nofail));
	}
	if (type_is_void(canonical->pointer))
	{
		if (failed_ref) goto ON_FAILED;
		RETURN_SEMA_ERROR(inner, "A 'void*' cannot be dereferenced, you need to first cast it to a concrete type.");
	}

	// 3. This could be a constant, in which case we can inline any *&foo to "foo"
	//    and check for null.
	if (sema_cast_const(inner))
	{
		switch (inner->const_expr.const_kind)
		{
			case CONST_POINTER:
				if (!inner->const_expr.ptr)
				{
					if (failed_ref) goto ON_FAILED;
					RETURN_SEMA_ERROR(inner, "Dereferencing null is not allowed, did you do it by mistake?");
				}
				break;
			case CONST_REF:
				expr_replace(expr, expr_variable(inner->const_expr.global_ref));
				break;
			default:
				UNREACHABLE
		}
	}

	// 4. Now the type might not be a pointer because of a typedef,
	//    otherwise we need to use the canonical representation.
	Type *deref_type = inner_type_nofail->type_kind == TYPE_POINTER ? inner_type_nofail : canonical;

	// 5. And... set the type.
	expr->type = type_add_optional(deref_type->pointer, IS_OPTIONAL(inner));
	return true;
ON_FAILED:
	*failed_ref = true;
	return false;
}

static inline const char *sema_addr_may_take_of_var(Expr *expr, Decl *decl)
{
	if (decl->decl_kind != DECL_VAR) return "This is not a regular variable.";
	decl->var.is_addr = true;
	bool is_void = type_flatten(decl->type) == type_void;
	switch (decl->var.kind)
	{
		case VARDECL_GLOBAL:
			if (is_void) return "You cannot take the address of a global of type 'void'";
			return NULL;
		case VARDECL_LOCAL:
			if (is_void) return "You cannot take the address of a variable of type 'void'";
			return NULL;
		case VARDECL_PARAM_REF:
			return "You may not take the address of a ref parameter";
		case VARDECL_PARAM:
			if (is_void) return "You cannot take the address of a parameter of type 'void'";
			return NULL;
		case VARDECL_CONST:
			if (!decl->var.type_info)
			{
				return "The constant is not typed, either type it or use && to take the reference to a temporary.";
			}
			ASSERT_SPAN(expr, decl->type != type_void);
			return NULL;
		case VARDECL_PARAM_EXPR:
			return "It is not possible to take the address of a captured expression, but you can use && to take a reference to the temporary value.";
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

static inline const char *sema_addr_may_take_of_ident(Expr *inner)
{
	Decl *decl = decl_raw(inner->identifier_expr.decl);
	switch (decl->decl_kind)
	{
		case DECL_POISONED:
			expr_poison(inner);
			return NULL;
		case DECL_FUNC:
			if (decl->func_decl.attr_test)
			{
				return "You may not take the address of a '@test' function.";
			}
			if (decl->func_decl.attr_benchmark)
			{
				return "You may not take the address of a '@benchmark' function.";
			}
			return NULL;
		case DECL_VAR:
			return sema_addr_may_take_of_var(inner, decl);
		case DECL_MACRO:
			return "It is not possible to take the address of a macro.";
		default:
			UNREACHABLE
	}
	UNREACHABLE
}

static const char *sema_addr_check_may_take(Expr *inner)
{
	switch (inner->expr_kind)
	{
		case EXPR_CT_IDENT:
			return "It's not possible to take the address of a compile time value.";
		case EXPR_IDENTIFIER:
			return sema_addr_may_take_of_ident(inner);
		case EXPR_UNARY:
			if (inner->unary_expr.operator == UNARYOP_DEREF) return NULL;
			break;
		case EXPR_ACCESS:
		{
			Decl *decl = inner->access_expr.ref;
			if (decl->decl_kind == DECL_FUNC)
			{
				if (decl->func_decl.attr_interface_method) return NULL;
				return "Taking the address of a method should be done through the type e.g. '&Foo.method' not through the value.";
			}
			return sema_addr_check_may_take(inner->access_expr.parent);
		}
		case EXPR_SUBSCRIPT_ADDR:
			return NULL;
		case EXPR_SUBSCRIPT:
			return sema_addr_check_may_take(exprptr(inner->subscript_expr.expr));
		case EXPR_TYPEINFO:
			return "It is not possible to take the address of a type.";
		case EXPR_BITACCESS:
			return "You cannot take the address of a bitstruct member.";
		default:
			break;
	}
	return "To take the address of a temporary value, use '&&' instead of '&'.";
}

/**
 * Analyse &a
 * @return true if analysis succeeds.
 */
static inline bool sema_expr_analyse_addr(SemaContext *context, Expr *expr, bool *failed_ref, CheckType check)
{
RETRY:;
	// 1. Evaluate the expression
	Expr *inner = expr->unary_expr.expr;
	if (inner->resolve_status == RESOLVE_DONE) goto RESOLVED;
	switch (inner->expr_kind)
	{
		case EXPR_POISONED:
			return false;
		case EXPR_OTHER_CONTEXT:
		{
			Expr *inner_c = inner->expr_other_context.inner;
			SemaContext *c2 = inner->expr_other_context.context;
			expr_replace(inner, inner_c);
			return sema_expr_analyse_addr(c2, expr, failed_ref, check);
		}
		case EXPR_HASH_IDENT:
			if (!sema_expr_fold_hash(context, inner)) return false;
			goto RETRY;
		case EXPR_SUBSCRIPT:
			inner->expr_kind = EXPR_SUBSCRIPT_ADDR;
			if (failed_ref)
			{
				if (!sema_expr_analyse_subscript(context, inner, CHECK_ADDRESS, true)) return false;
				if (!expr_ok(inner))
				{
					*failed_ref = true;
					return false;
				}
			}
			if (!sema_analyse_expr_address(context, inner)) return false;
			expr_replace(expr, inner);
			return true;
		case EXPR_ACCESS:
		{
			Expr *parent = inner->access_expr.parent;
			if (parent->expr_kind == EXPR_TYPEINFO) break;
			if (!sema_analyse_expr_address(context, parent)) return false;
			break;
		}
		default:
			break;
	}
RESOLVED:
	if (inner->expr_kind == EXPR_CT_IDENT)
	{
		RETURN_SEMA_ERROR(expr, "It's not possible to take the address of a compile time value.");
	}

	if (!sema_analyse_expr_address(context, inner)) return false;

	// 2. Take the address.
	const char *error = sema_addr_check_may_take(inner);
	if (error)
	{
		if (failed_ref)
		{
			*failed_ref = true;
			return false;
		}
		RETURN_SEMA_ERROR(inner, error);
	}

	// 3. Get the pointer of the underlying type.
	if (inner->type->type_kind == TYPE_FUNC_RAW)
	{
		expr->type = type_get_func_ptr(inner->type);
		return true;
	}
	expr->type = type_get_ptr_recurse(inner->type);
	if (inner->expr_kind == EXPR_IDENTIFIER)
	{
		Decl *ident = inner->identifier_expr.decl;
		if (decl_is_global(ident))
		{
			expr_rewrite_const_ref(expr, ident);
			return true;
		}
	}
	return true;
}

/**
 * Test -a / +a
 */
static inline bool sema_expr_analyse_neg_plus(SemaContext *context, Expr *expr)
{
	// 1. Check the inner expression
	Expr *inner = expr->unary_expr.expr;
	bool is_plus = expr->unary_expr.operator == UNARYOP_PLUS;
	if (!sema_analyse_expr(context, inner)) return false;

	// 2. Check if it's possible to negate this (i.e. is it an int, float or vector)
	Type *no_fail = type_no_optional(inner->type);
	if (!type_may_negate(no_fail))
	{
		if (is_plus)
		{
			RETURN_SEMA_ERROR(expr, "Cannot use '+' with an expression of type %s.", type_quoted_error_string(no_fail));
		}
		RETURN_SEMA_ERROR(expr, "Cannot negate an expression of type %s.", type_quoted_error_string(no_fail));
	}
	// 3. Promote the type
	Type *result_type = cast_numeric_arithmetic_promotion(no_fail);
	if (!cast_implicit(context, inner, result_type, false)) return false;

	// If it's a plus, we simply replace the inner with the outer or with a recast.
	if (is_plus)
	{
		if (expr_is_const(expr))
		{
			expr_replace(expr, inner);
			return true;
		}
		expr->expr_kind = EXPR_RECAST;
		expr->inner_expr = inner;
		expr->type = inner->type;
		return true;
	}
	// 4. If it's non-const, we're done.
	if (!sema_constant_fold_ops(inner))
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

	// 2. Check that it's a vector, bool
	Type *canonical = type_no_optional(inner->type)->canonical;
	Type *flat = type_flatten(canonical);
	bool is_bitstruct = flat->type_kind == TYPE_BITSTRUCT;
	if (!type_is_integer_or_bool_kind(flat) && !is_bitstruct)
	{
		Type *vector_type = type_vector_type(canonical);
		if (vector_type && (type_is_integer(vector_type) || vector_type == type_bool)) goto VALID_VEC;
		RETURN_SEMA_ERROR(expr, "Cannot bit negate '%s'.", type_to_error_string(inner->type));
	}

VALID_VEC:
	if (is_bitstruct && sema_cast_const(inner))
	{
		expr_replace(expr, inner);
		sema_invert_bitstruct_const_initializer(expr->const_expr.initializer);
		return true;
	}

	// Arithmetic promotion
	Type *result_type = cast_numeric_arithmetic_promotion(type_no_optional(inner->type));
	if (!cast_implicit(context, inner, result_type, false)) return false;

	// 3. The simple case, non-const.
	if (!sema_constant_fold_ops(inner))
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
	Type *flat = type_flatten(type);
	if (type_kind_is_any_vector(flat->type_kind))
	{
		// This may be some form of bool vector.
		if (type_flatten(flat->array.base) == type_bool)
		{
			// If so then we're done.
			expr->type = type;
			return true;
		}
		Type *canonical = type->canonical;
		switch (canonical->type_kind)
		{
			case TYPE_VECTOR:
				expr->type = type_get_vector(type_bool, canonical->array.len);
				return true;
			case TYPE_INFERRED_VECTOR:
				UNREACHABLE;
			default:
				break;
		}
	}

	if (!cast_explicit_silent(context, inner, type_add_optional(type_bool, IS_OPTIONAL(inner))))
	{
		RETURN_SEMA_ERROR(expr, "The %s can't be converted to a boolean value.", type_quoted_error_string(inner->type));
	}

	expr->type = type_add_optional(type_bool, IS_OPTIONAL(inner));

	if (inner->expr_kind == EXPR_INT_TO_BOOL)
	{
		inner->int_to_bool_expr.negate = !inner->int_to_bool_expr.negate;
		expr_replace(expr, inner);
		return true;
	}

	if (sema_cast_const(inner))
	{
		ASSERT_SPAN(expr, inner->const_expr.const_kind == CONST_BOOL);
		expr->const_expr.const_kind = CONST_BOOL;
		expr->expr_kind = EXPR_CONST;
		expr->resolve_status = RESOLVE_DONE;
		expr->const_expr.b = !inner->const_expr.b;
		return true;
	}

	return true;
}

static inline bool sema_expr_analyse_ct_incdec(SemaContext *context, Expr *expr, Expr *inner)
{
	ASSERT_SPAN(expr, inner->expr_kind == EXPR_CT_IDENT);

	if (!sema_analyse_expr_lvalue(context, inner)) return false;

	Decl *var = inner->ct_ident_expr.decl;
	Expr *start_value = var->var.init_expr;
	if (!expr_is_const_int(start_value))
	{
		RETURN_SEMA_ERROR(expr, "The compile time variable '%s' does not hold an integer.", var->name);
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

static bool sema_analyse_assign_mutate_overloaded_subscript(SemaContext *context, Expr *main, Expr *subscript_expr, Type *type)
{
	Expr *increased = exprptr(subscript_expr->subscript_assign_expr.expr);
	Type *type_check = increased->type->canonical;
	Expr *index = exprptr(subscript_expr->subscript_assign_expr.index);
	Decl *operator = sema_find_operator(context, type_check, OVERLOAD_ELEMENT_REF);
	Expr **args = NULL;
	if (operator)
	{
		vec_add(args, exprptr(subscript_expr->subscript_assign_expr.index));
		if (!sema_insert_method_call(context, subscript_expr, operator, exprptr(subscript_expr->subscript_assign_expr.expr), args)) return false;
		expr_rewrite_insert_deref(subscript_expr);
		main->type = subscript_expr->type;
		return true;
	}
	operator = sema_find_operator(context, type_check, OVERLOAD_ELEMENT_AT);
	if (!operator)
	{
		RETURN_SEMA_ERROR(main, "There is no overload for [] for %s.", type_quoted_error_string(increased->type));
	}
	Type *return_type = typeget(operator->func_decl.signature.rtype);
	if (type_no_optional(return_type->canonical) != type->canonical)
	{
		RETURN_SEMA_ERROR(main, "There is a type mismatch between overload for [] and []= for %s.", type_quoted_error_string(increased->type));
	}
	bool is_optional_result = type_is_optional(increased->type) || type_is_optional(return_type);
	Type *result_type = type_add_optional(subscript_expr->type, is_optional_result);
	expr_insert_addr(increased);
	Decl *temp_val = decl_new_generated_var(increased->type, VARDECL_LOCAL, increased->span);
	Decl *index_val = decl_new_generated_var(index->type, VARDECL_LOCAL, index->span);
	Decl *value_val = decl_new_generated_var(return_type, VARDECL_LOCAL, main->span);
	Decl *result_val = decl_new_generated_var(result_type, VARDECL_LOCAL, main->span);
	Expr *decl_expr = expr_generate_decl(temp_val, increased);
	Expr *decl_index_expr = expr_generate_decl(index_val, index);
	Expr *mutate = expr_copy(main);
	mutate->resolve_status = RESOLVE_NOT_DONE;
	mutate->type = NULL;
	switch (main->expr_kind)
	{
		case EXPR_UNARY:
		case EXPR_POST_UNARY:
			mutate->unary_expr.expr = expr_variable(value_val);
			break;
		case EXPR_BINARY:
			mutate->binary_expr.left = exprid(expr_variable(value_val));
			break;
		default:
			UNREACHABLE
	}
	main->expr_kind = EXPR_EXPRESSION_LIST;
	main->expression_list = NULL;
	// temp = indexed
	vec_add(main->expression_list, decl_expr);
	// temp_index = index
	vec_add(main->expression_list, decl_index_expr);
	Expr *get_expr = expr_new(EXPR_ACCESS, increased->span);
	vec_add(args, expr_variable(index_val));
	Expr *temp_val_1 = expr_variable(temp_val);
	expr_rewrite_insert_deref(temp_val_1);
	if (!sema_insert_method_call(context, get_expr, operator, temp_val_1, args)) return false;
	Expr *value_val_expr = expr_generate_decl(value_val, get_expr);
	// temp_value = func(temp, temp_index)
	vec_add(main->expression_list, value_val_expr);
	// temp_result = temp_value++, temp_result *= temp_value etc
	vec_add(main->expression_list, expr_generate_decl(result_val, mutate));

	args = NULL;
	vec_add(args, expr_variable(index_val));
	vec_add(args, expr_variable(value_val));
	Expr *temp_val_2 = expr_variable(temp_val);
	expr_rewrite_insert_deref(temp_val_2);
	if (!sema_insert_method_call(context, subscript_expr, declptr(subscript_expr->subscript_assign_expr.method), temp_val_2, args)) return false;
	ASSERT0(subscript_expr->expr_kind == EXPR_CALL);
	subscript_expr->call_expr.has_optional_arg = false;
	vec_add(main->expression_list, subscript_expr);
	vec_add(main->expression_list, expr_variable(result_val));
	return sema_expr_analyse_expr_list(context, main);

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
	if (!sema_expr_check_assign(context, inner, NULL)) return false;

	// 3. This might be a $foo, if to handle it.
	if (inner->expr_kind == EXPR_CT_IDENT)
	{
		return sema_expr_analyse_ct_incdec(context, expr, inner);
	}

	// 4. Flatten typedef, enum, distinct, optional
	Type *type = type_flatten(inner->type);

	// 5. We can only inc/dec numbers or pointers.
	if (!type_underlying_may_add_sub(type))
	{
		RETURN_SEMA_ERROR(inner, "The expression must be a number or a pointer.");
	}

	if (inner->expr_kind == EXPR_SUBSCRIPT_ASSIGN)
	{
		return sema_analyse_assign_mutate_overloaded_subscript(context, expr, inner, type);
	}
	// 6. Done, the result is same as the inner type.
	expr->type = inner->type;
	return true;
}

/**
 * Take an address of a temporary &&x.
 */
static inline bool sema_expr_analyse_taddr(SemaContext *context, Expr *expr, bool *failed_ref)
{
	Expr *inner = expr->unary_expr.expr;
	if (!sema_analyse_expr(context, inner)) return false;

	Type *type = inner->type;

	switch (sema_resolve_storage_type(context, type))
	{
		case STORAGE_ERROR:
			return false;
		case STORAGE_NORMAL:
			break;
		default:
			if (failed_ref)
			{
				*failed_ref = true;
				return false;
			}
			RETURN_SEMA_ERROR(expr, "It is not possible to take the address from a value of type %s.",
							  type_quoted_error_string(type));
	}
	// 2. The type is the resulting type of the expression.
	expr->type = type_get_ptr_recurse(inner->type);
	return true;
}

INLINE bool expr_is_ungrouped_binary(Expr *expr)
{
	return expr->expr_kind == EXPR_BINARY && !expr->binary_expr.grouped;
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

	BinaryOp main_op = main_expr->binary_expr.operator;
	int precedence_main = BINOP_PREC_REQ[main_op];
	if (expr_is_ungrouped_binary(left_side))
	{
		int left_op = left_side->binary_expr.operator;
		int precedence_left = BINOP_PREC_REQ[left_op];
		if (precedence_left && (precedence_left == precedence_main))
		{
			// ^|& has special rules
			if (precedence_left != 1 || left_op != main_op) return true;
		}
	}
	if (expr_is_ungrouped_binary(right_side))
	{
		int right_op = right_side->binary_expr.operator;
		int precedence_right = BINOP_PREC_REQ[right_op];
		if (precedence_right && (precedence_right == precedence_main))
		{
			// ^|& has special rules
			if (precedence_right != 1 || right_op != main_op) return true;
		}
	}
	return false;
}


INLINE bool expr_is_ungrouped_ternary(Expr *expr)
{
	return expr->expr_kind == EXPR_TERNARY && !expr->ternary_expr.grouped;
}

static inline bool sema_expr_analyse_or_error(SemaContext *context, Expr *expr)
{
	Expr *lhs = exprptr(expr->binary_expr.left);
	bool lhs_is_embed = lhs->expr_kind == EXPR_EMBED;
	Expr *rhs = exprptr(expr->binary_expr.right);
	if (expr_is_ungrouped_ternary(lhs) || expr_is_ungrouped_ternary(rhs))
	{
		SEMA_ERROR(expr, "Unclear precedence using ternary with ??, please use () to remove ambiguity.");
		return false;
	}
	if (lhs_is_embed)
	{
		if (!sema_expr_analyse_embed(context, lhs, true)) return false;
	}
	else
	{
		if (!sema_analyse_expr(context, lhs)) return false;
	}

	Type *type = lhs->type;
	if (!type_is_optional(type))
	{
		if (lhs_is_embed)
		{
			expr_replace(expr, lhs);
			return true;
		}
		SEMA_ERROR(lhs, "No optional to use '\?\?' with, please remove the '\?\?'.");
		return false;
	}

	// First we analyse the "else" and try to implictly cast.
	if (!sema_analyse_expr(context, rhs)) return false;

	if (lhs->expr_kind == EXPR_OPTIONAL)
	{
		expr_replace(expr, rhs);
		return true;
	}

	// Here we might need to insert casts.
	Type *else_type = rhs->type;


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
	if (!cast_implicit(context, lhs, common, false)) return false;
	if (!cast_implicit(context, rhs, common, false)) return false;
	expr->type = type_add_optional(common, add_optional);
	return true;
}

static inline bool sema_expr_analyse_ct_and_or(SemaContext *context, Expr *expr, Expr *left, Expr *right)
{
	ASSERT_SPAN(expr, expr->resolve_status == RESOLVE_RUNNING);
	bool is_and = expr->binary_expr.operator == BINARYOP_CT_AND;
	if (!sema_analyse_expr(context, left)) return false;
	if (!expr_is_const_bool(left)) RETURN_SEMA_ERROR(left, "Expected this to evaluate to a constant boolean.");
	if (left->const_expr.b != is_and)
	{
		expr_rewrite_const_bool(expr, type_bool, !is_and);
		return true;
	}
	if (!sema_analyse_expr(context, right)) return false;
	if (!expr_is_const_bool(right)) RETURN_SEMA_ERROR(right, "Expected this to evaluate to a constant boolean.");
	expr_rewrite_const_bool(expr, type_bool, right->const_expr.b);
	return true;
}

static inline bool sema_expr_analyse_binary(SemaContext *context, Expr *expr, bool *failed_ref)
{
	if (expr->binary_expr.operator == BINARYOP_ELSE) return sema_expr_analyse_or_error(context, expr);
	ASSERT_SPAN(expr, expr->resolve_status == RESOLVE_RUNNING);
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
		case BINARYOP_CT_CONCAT:
			return sema_expr_analyse_ct_concat(context, expr, left, right);
		case BINARYOP_CT_OR:
		case BINARYOP_CT_AND:
			return sema_expr_analyse_ct_and_or(context, expr, left, right);
		case BINARYOP_ASSIGN:
			return sema_expr_analyse_assign(context, expr, left, right, failed_ref);
		case BINARYOP_MULT:
			return sema_expr_analyse_mult(context, expr, left, right);
		case BINARYOP_ADD:
			return sema_expr_analyse_add(context, expr, left, right);
		case BINARYOP_ADD_ASSIGN:
		case BINARYOP_SUB_ASSIGN:
			return sema_expr_analyse_op_assign(context, expr, left, right, false, false, true);
		case BINARYOP_SUB:
			return sema_expr_analyse_sub(context, expr, left, right);
		case BINARYOP_DIV:
			return sema_expr_analyse_div(context, expr, left, right);
		case BINARYOP_MULT_ASSIGN:
		case BINARYOP_DIV_ASSIGN:
			return sema_expr_analyse_op_assign(context, expr, left, right, false, false, false);
		case BINARYOP_BIT_AND_ASSIGN:
		case BINARYOP_BIT_OR_ASSIGN:
		case BINARYOP_BIT_XOR_ASSIGN:
			return sema_expr_analyse_op_assign(context, expr, left, right, true, true, false);
		case BINARYOP_MOD_ASSIGN:
			return sema_expr_analyse_op_assign(context, expr, left, right, true, false, false);
		case BINARYOP_MOD:
			return sema_expr_analyse_mod(context, expr, left, right);
		case BINARYOP_AND:
		case BINARYOP_OR:
			return sema_expr_analyse_and_or(context, expr, left, right);
		case BINARYOP_BIT_OR:
		case BINARYOP_BIT_XOR:
		case BINARYOP_BIT_AND:
			return sema_expr_analyse_bit(context, expr, left, right);
		case BINARYOP_VEC_NE:
		case BINARYOP_VEC_EQ:
		case BINARYOP_VEC_GT:
		case BINARYOP_VEC_GE:
		case BINARYOP_VEC_LT:
		case BINARYOP_VEC_LE:
			UNREACHABLE
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
static inline bool sema_expr_analyse_unary(SemaContext *context, Expr *expr, bool *failed_ref, CheckType check)
{
	if (failed_ref) *failed_ref = false;
	ASSERT_SPAN(expr, expr->resolve_status == RESOLVE_RUNNING);
	switch (expr->unary_expr.operator)
	{
		case UNARYOP_DEREF:
			return sema_expr_analyse_deref(context, expr, failed_ref);
		case UNARYOP_ADDR:
			return sema_expr_analyse_addr(context, expr, failed_ref, check);
		case UNARYOP_NEG:
		case UNARYOP_PLUS:
			return sema_expr_analyse_neg_plus(context, expr);
		case UNARYOP_BITNEG:
			return sema_expr_analyse_bit_not(context, expr);
		case UNARYOP_NOT:
			return sema_expr_analyse_not(context, expr);
		case UNARYOP_DEC:
		case UNARYOP_INC:
			return sema_expr_analyse_incdec(context, expr);
		case UNARYOP_TADDR:
			return sema_expr_analyse_taddr(context, expr, failed_ref);
		case UNARYOP_ERROR:
			return false;
	}
	UNREACHABLE
}


static inline bool sema_expr_analyse_rethrow(SemaContext *context, Expr *expr)
{
	if (context->call_env.kind != CALL_ENV_FUNCTION)
	{
		if (CALL_ENV_FUNCTION_STATIC)
		{
			RETURN_SEMA_ERROR(expr, "Rethrow cannot be used for a static initializer.");
		}
		RETURN_SEMA_ERROR(expr, "Rethrow cannot be used outside of a function.");
	}

	Expr *inner = expr->rethrow_expr.inner;
	if (!sema_analyse_expr(context, inner)) return false;

	if (context->active_scope.in_defer) RETURN_SEMA_ERROR(expr, "Rethrows are not allowed inside of defers.");

	expr->type = type_no_optional(inner->type);

	if (!IS_OPTIONAL(inner))
	{
		RETURN_SEMA_ERROR(expr, "No optional to rethrow before '!' in the expression, please remove '!'.");
	}

	if (context->active_scope.flags & (SCOPE_EXPR_BLOCK | SCOPE_MACRO))
	{
		TypeInfoId rtype = context->active_scope.flags & SCOPE_MACRO ? context->current_macro->func_decl.signature.rtype : 0;
		if (rtype && !type_is_optional(typeget(rtype)))
		{
			RETURN_SEMA_ERROR(expr, "Rethrow is only allowed in macros with an optional or inferred return type. "
									"Did you mean to use '!!' instead?");
		}
		vec_add(context->returns, NULL);
		expr->rethrow_expr.in_block = context->block_exit_ref;
		expr->rethrow_expr.cleanup = context_get_defers(context, context->active_scope.defer_last, context->block_return_defer, false);
	}
	else
	{
		expr->rethrow_expr.cleanup = context_get_defers(context, context->active_scope.defer_last, 0, false);
		expr->rethrow_expr.in_block = NULL;
		if (context->rtype && context->rtype->type_kind != TYPE_OPTIONAL)
		{
			RETURN_SEMA_ERROR(expr, "This expression implicitly returns with an optional result, "
									"but the function does not allow optional results. Did you mean to use '!!' instead?");
		}
	}
	return true;
}


static inline bool sema_expr_analyse_force_unwrap(SemaContext *context, Expr *expr)
{
	Expr *inner = expr->inner_expr;
	if (!sema_analyse_expr(context, inner)) return false;
	expr->type = type_no_optional(inner->type);
	if (!IS_OPTIONAL(inner))
	{
		SEMA_ERROR(expr, "No optional to rethrow before '!!' in the expression, please remove '!!'.");
		return false;
	}
	return true;
}

static inline bool sema_expr_analyse_typeid(SemaContext *context, Expr *expr)
{
	if (!sema_resolve_type_info(context, expr->typeid_expr, RESOLVE_TYPE_DEFAULT)) return expr_poison(expr);
	Type *type = expr->type_expr->type;
	expr->expr_kind = EXPR_CONST;
	expr->resolve_status = RESOLVE_DONE;
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
	BlockExit **ref = CALLOCS(BlockExit*);
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
		Type *return_no_optional = type_no_optional(sum_returns);
		if (return_no_optional != type_wildcard && return_no_optional != type_void && !context->active_scope.jump_end)
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



static inline bool sema_expr_analyse_optional(SemaContext *context, Expr *expr, bool *failed_ref)
{
	Expr *inner = expr->inner_expr;
	if (failed_ref) *failed_ref = false;
	if (!sema_analyse_expr(context, inner)) return false;

	if (IS_OPTIONAL(inner))
	{
		if (failed_ref) goto ON_FAILED;
		RETURN_SEMA_ERROR(inner, "The inner expression is already an optional.");
	}

	if (inner->expr_kind == EXPR_OPTIONAL)
	{
		if (failed_ref) goto ON_FAILED;
		RETURN_SEMA_ERROR(inner, "It looks like you added one too many '?' after the error.");
	}

	Type *type = inner->type->canonical;

	if (!type_is_fault_raw(type))
	{
		if (failed_ref) goto ON_FAILED;
		RETURN_SEMA_ERROR(inner, "You cannot use the '?' operator on expressions of type %s",
						  type_quoted_error_string(type));
	}
	ASSERT_SPAN(expr, type->type_kind == TYPE_ANYFAULT || type->decl->resolve_status == RESOLVE_DONE);
	expr->type = type_wildcard_optional;
	return true;
ON_FAILED:
	*failed_ref = true;
	return false;
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
			expr_rewrite_const_string(expr, time_get());
			return true;
		case BUILTIN_DEF_DATE:
			expr_rewrite_const_string(expr, date_get());
			return true;
		case BUILTIN_DEF_FILE:
			if (context->call_env.current_function)
			{
				expr_rewrite_const_string(expr, context->call_env.current_function->unit->file->name);
				return true;
			}
			expr_rewrite_const_string(expr, context->compilation_unit->file->name);
			return true;
		case BUILTIN_DEF_FILEPATH:
			if (context->call_env.current_function)
			{
				expr_rewrite_const_string(expr, context->call_env.current_function->unit->file->full_path);
				return true;
			}
			expr_rewrite_const_string(expr, context->compilation_unit->file->full_path);
			return true;
		case BUILTIN_DEF_MODULE:
			if (context->original_module)
			{
				expr_rewrite_const_string(expr, context->original_module->name->module);
			}
			else
			{
				expr_rewrite_const_string(expr, context->compilation_unit->module->name->module);
			}
			return true;
		case BUILTIN_DEF_LINE:
			if (context->original_inline_line)
			{
				expr_rewrite_const_int(expr, type_isz, context->original_inline_line);
			}
			else
			{
				expr_rewrite_const_int(expr, type_isz, expr->span.row);
			}
			return true;
		case BUILTIN_DEF_LINE_RAW:
			expr_rewrite_const_int(expr, type_isz, expr->span.row);
			return true;
		case BUILTIN_DEF_FUNCTION:
			switch (context->call_env.kind)
			{
				case CALL_ENV_GLOBAL_INIT:
				case CALL_ENV_ATTR:
					if (report_missing)
					{
						SEMA_ERROR(expr, "$$FUNCEXPR is not defined outside of a function.");
					}
					return false;
				case CALL_ENV_FUNCTION:
				case CALL_ENV_FUNCTION_STATIC:
					expr->expr_kind = EXPR_IDENTIFIER;
					expr_resolve_ident(expr, context->call_env.current_function);
					return true;
			}
			UNREACHABLE
		case BUILTIN_DEF_FUNC:
			switch (context->call_env.kind)
			{
				case CALL_ENV_GLOBAL_INIT:
					expr_rewrite_const_string(expr, "<GLOBAL>");
					return true;
				case CALL_ENV_FUNCTION_STATIC:
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
						expr_rewrite_const_string(expr, scratch_buffer_copy());
						return true;
					}
					expr_rewrite_const_string(expr, current_func->name);
					return true;
				}
				case CALL_ENV_ATTR:
					expr_rewrite_const_string(expr, "<attribute>");
					return true;
			}
			UNREACHABLE
		case BUILTIN_DEF_NONE:
		{
			Expr *value = htable_get(&compiler.context.compiler_defines, (void *)string);
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

		case BUILTIN_DEF_BENCHMARK_NAMES:
			if (!compiler.build.benchmarking)
			{
				expr_rewrite_const_empty_slice(expr, type_get_slice(type_string));
				return true;
			}
			expr->type = type_get_slice(type_string);
			expr->benchmark_hook_expr = BUILTIN_DEF_BENCHMARK_NAMES;
			expr->expr_kind = EXPR_BENCHMARK_HOOK;
			return true;
		case BUILTIN_DEF_BENCHMARK_FNS:
			if (!compiler.build.benchmarking)
			{
				expr_rewrite_const_empty_slice(expr, type_get_slice(type_voidptr));
				return true;
			}
			expr->type = type_get_slice(type_voidptr);
			expr->benchmark_hook_expr = BUILTIN_DEF_BENCHMARK_FNS;
			expr->expr_kind = EXPR_BENCHMARK_HOOK;
			return true;
		case BUILTIN_DEF_TEST_NAMES:
			if (!compiler.build.testing)
			{
				expr_rewrite_const_empty_slice(expr, type_get_slice(type_string));
				return true;
			}
			expr->type = type_get_slice(type_string);
			expr->test_hook_expr = BUILTIN_DEF_TEST_NAMES;
			expr->expr_kind = EXPR_TEST_HOOK;
			return true;
		case BUILTIN_DEF_TEST_FNS:
			if (!compiler.build.testing)
			{
				expr_rewrite_const_empty_slice(expr, type_get_slice(type_voidptr));
				return true;
			}
			expr->type = type_get_slice(type_voidptr);
			expr->test_hook_expr = BUILTIN_DEF_TEST_FNS;
			expr->expr_kind = EXPR_TEST_HOOK;
			return true;
	}
	UNREACHABLE
}



static Decl *sema_expr_analyse_var_path(SemaContext *context, Expr *expr)
{
	if (!sema_analyse_expr_value(context, expr)) return NULL;
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

static inline bool sema_expr_analyse_decl_element(SemaContext *context, DesignatorElement *element, Type *type, Decl **member_ref, ArraySize *index_ref, Type **return_type, unsigned i, SourceSpan loc,
												  bool *is_missing)
{
	DesignatorType kind = element->kind;
	if (kind == DESIGNATOR_RANGE) RETURN_SEMA_ERROR(element->index_expr, "Ranges are not allowed.");
	Type *actual_type = type_flatten(type);
	if (kind == DESIGNATOR_ARRAY)
	{
		Expr *inner = element->index_expr;
		if (!type_is_arraylike(actual_type) && actual_type->type_kind != TYPE_POINTER)
		{
			if (is_missing)
			{
				*is_missing = true;
				return false;
			}
			RETURN_SEMA_ERROR(inner, "It's not possible to constant index into something that is not an array nor vector.");
		}
		if (!sema_analyse_expr(context, inner)) return false;
		if (!type_is_integer(inner->type))
		{
			RETURN_SEMA_ERROR(inner, "Expected an integer index.");
		}
		if (!sema_cast_const(inner))
		{
			RETURN_SEMA_ERROR(inner, "Expected a constant index.");
		}
		Int value = inner->const_expr.ixx;
		if (!int_fits(value, type_isz->canonical->type_kind))
		{
			RETURN_SEMA_ERROR(inner, "The index is out of range for a %s.", type_quoted_error_string(type_isz));
		}
		if (int_is_neg(value)) RETURN_SEMA_ERROR(inner, "The index must be zero or greater.");

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
			RETURN_SEMA_ERROR(inner, "Index exceeds array bounds.");
		}
		*return_type = type;
		*index_ref = index;
		*member_ref = NULL;
		return true;
	}
	Expr *field = sema_expr_resolve_access_child(context, element->field_expr, is_missing);
	if (!field) return false;
	if (field->expr_kind != EXPR_IDENTIFIER) RETURN_SEMA_ERROR(field, "Expected an identifier here.");
	const char *kw = field->identifier_expr.ident;
	if (kw == kw_ptr)
	{
		switch (actual_type->type_kind)
		{
			case TYPE_SLICE:
				*member_ref = NULL;
				*return_type = actual_type->array.base;
				return true;
			case TYPE_INTERFACE:
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
		if (type_is_arraylike(actual_type) || actual_type->type_kind == TYPE_SLICE)
		{
			*member_ref = NULL;
			*return_type = type_usz;
			return true;
		}
	}
	if (actual_type->type_kind == TYPE_POINTER && actual_type->pointer->type_kind != TYPE_POINTER)
	{
		actual_type = actual_type->pointer;
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
			sema_error_at(context, loc, "%s has no members.", type_quoted_error_string(type));
		}
		else
		{
			sema_error_at(context, loc, "There is no such member in %s.", type_quoted_error_string(type));
		}
		return false;
	}
	Decl *member = sema_decl_stack_find_decl_member(context, actual_type->decl, kw, METHODS_AND_FIELDS);
	if (!decl_ok(member)) return false;
	if (!member)
	{
		Decl *ambiguous = NULL;
		Decl *private = NULL;
		member = sema_resolve_method(context->unit, actual_type->decl, kw, &ambiguous, &private);
		if (ambiguous)
		{
			sema_error_at(context, loc, "'%s' is an ambiguous name and so cannot be resolved, it may refer to method defined in '%s' or one in '%s'",
					   kw, member->unit->module->name->module, ambiguous->unit->module->name->module);
			return false;
		}
		if (is_missing)
		{
			*is_missing = true;
			return false;
		}
		sema_error_at(context, loc, "There is no such member in %s.", type_quoted_error_string(type));
		return false;
	}
	*member_ref = member;
	*return_type = member->type;
	return true;
}

static inline bool sema_expr_analyse_ct_alignof(SemaContext *context, Expr *expr)
{
	Expr *main_var = expr->ct_call_expr.main_var;
	DesignatorElement **path = expr->ct_call_expr.flat_path;
	Decl *decl = sema_expr_analyse_var_path(context, main_var);
	if (!decl) return false;
	Type *type = decl->type;
	switch (sema_resolve_storage_type(context, type))
	{
		case STORAGE_ERROR:
			return false;
		case STORAGE_NORMAL:
			break;
		default:
			RETURN_SEMA_ERROR(main_var, "Cannot use '$alignof' on type %s.", type_quoted_error_string(type));
	}
	AlignSize align;
	if (decl && !decl_is_user_defined_type(decl))
	{
		align = decl->alignment;
	}
	else
	{
		if (!sema_set_abi_alignment(context, type, &align)) return false;
	}
	FOREACH_IDX(i, DesignatorElement *, element, path)
	{
		Decl *member;
		ArraySize index = 0;
		Type *result_type;
		if (!sema_expr_analyse_decl_element(context,
											element,
											type,
											&member,
											&index,
											&result_type,
											i,
											i == 0 ? main_var->span : expr->span,
											NULL)) return false;
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
	expr_rewrite_const_int(expr, type_isz, align);
	return true;
}

static inline void sema_expr_rewrite_to_type_nameof(Expr *expr, Type *type, TokenType name_type)
{
	if (type_is_func_ptr(type)) type = type->pointer->function.prototype->raw_type;
	if (name_type == TOKEN_CT_EXTNAMEOF)
	{
		if (type_is_user_defined(type))
		{
			scratch_buffer_set_extern_decl_name(type->decl, true);
			expr_rewrite_const_string(expr, scratch_buffer_copy());
		}
		else
		{
			expr_rewrite_const_string(expr, type->name);
		}
		return;
	}

	if (name_type == TOKEN_CT_NAMEOF || type_is_builtin(type->type_kind))
	{
		expr_rewrite_const_string(expr, type->name);
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
	expr_rewrite_const_string(expr, scratch_buffer_copy());
}

static inline bool sema_expr_analyse_ct_nameof(SemaContext *context, Expr *expr)
{
	Expr *main_var = expr->ct_call_expr.main_var;
	Decl *decl = sema_expr_analyse_var_path(context, main_var);
	if (!decl) return false;

	TokenType name_type = expr->ct_call_expr.token_type;

	if (vec_size(expr->ct_call_expr.flat_path))
	{
		SEMA_ERROR(main_var, "You can only take the name of types and variables, not their sub elements.");
		return false;
	}

	if (name_type == TOKEN_CT_EXTNAMEOF)
	{
		switch (decl->decl_kind)
		{
			case DECL_VAR:
				switch (decl->var.kind)
				{
					case VARDECL_CONST:
					case VARDECL_GLOBAL:
						goto RETURN_CT;
					case VARDECL_LOCAL:
					case VARDECL_PARAM:
					case VARDECL_MEMBER:
					case VARDECL_BITMEMBER:
					case VARDECL_PARAM_REF:
					case VARDECL_PARAM_EXPR:
					case VARDECL_UNWRAPPED:
					case VARDECL_ERASE:
					case VARDECL_REWRAPPED:
					case VARDECL_PARAM_CT:
					case VARDECL_PARAM_CT_TYPE:
					case VARDECL_LOCAL_CT:
					case VARDECL_LOCAL_CT_TYPE:
						// TODO verify that all of these are correct.
						break;
				}
				FALLTHROUGH;
			case DECL_POISONED:
			case DECL_ATTRIBUTE:
			case DECL_BODYPARAM:
			case DECL_CT_ASSERT:
			case DECL_CT_ECHO:
			case DECL_CT_EXEC:
			case DECL_CT_INCLUDE:
			case DECL_DECLARRAY:
			case DECL_ERASED:
			case DECL_GLOBALS:
			case DECL_IMPORT:
			case DECL_LABEL:
			case DECL_MACRO:
			case DECL_DEFINE:
				RETURN_SEMA_ERROR(main_var, "'%s' does not have an external name.", decl->name);
			case DECL_BITSTRUCT:
			case DECL_DISTINCT:
			case DECL_ENUM:
			case DECL_ENUM_CONSTANT:
			case DECL_FAULT:
			case DECL_FAULTVALUE:
			case DECL_FNTYPE:
			case DECL_FUNC:
			case DECL_INTERFACE:
			case DECL_STRUCT:
			case DECL_TYPEDEF:
			case DECL_UNION:
				// TODO verify that all of these are correct
				goto RETURN_CT;
		}
RETURN_CT:
		scratch_buffer_set_extern_decl_name(decl, true);
		expr_rewrite_const_string(expr, scratch_buffer_to_string());
		return true;
	}
	if (!decl->unit || name_type == TOKEN_CT_NAMEOF || decl_is_var_local(decl))
	{
		expr_rewrite_const_string(expr, decl->name);
		return true;
	}
	scratch_buffer_clear();
	scratch_buffer_append(decl->unit->module->name->module);
	scratch_buffer_append("::");
	scratch_buffer_append(decl->name);
	expr_rewrite_const_string(expr, scratch_buffer_copy());
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
		case TYPE_INFO_GENERIC:
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
						   "%s is not of a vectorizable type.",
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
			if (!type_is_valid_for_array(type))
			{
				SEMA_ERROR(type_info->array.base,
						   "You cannot form an array with elements of type %s.",
						   type_quoted_error_string(type));
				return poisoned_type;
			}
			return type_get_array(type, size);
		}
		case TYPE_INFO_CT_IDENTIFIER:
		case TYPE_INFO_IDENTIFIER:
		{
			Decl *decl = sema_find_path_symbol(context, type_info->unresolved.name, type_info->unresolved.path);
			if (!decl) return NULL;
			if (!decl_ok(decl)) return poisoned_type;
			if (type_info->kind == TYPE_INFO_CT_IDENTIFIER) return decl->var.init_expr->type_expr->type->canonical;
			return decl->type->canonical;
		}
		case TYPE_INFO_VATYPE:
		{
			if (!context->current_macro) return NULL;
			Expr *arg_expr = sema_expr_analyse_ct_arg_index(context, type_info->unresolved_type_expr, NULL);
			if (!expr_ok(arg_expr)) return poisoned_type;
			if (!sema_analyse_expr_value(context, arg_expr)) return poisoned_type;
			if (arg_expr->expr_kind != EXPR_TYPEINFO) return NULL;
			return arg_expr->type_expr->type->canonical;
		}
		case TYPE_INFO_TYPEFROM:
		case TYPE_INFO_TYPEOF:
			if (!sema_resolve_type_info(context, type_info, RESOLVE_TYPE_DEFAULT)) return poisoned_type;
			return type_info->type;
		case TYPE_INFO_EVALTYPE:
		{
			Expr *expr = type_info->unresolved_type_expr;
			expr = sema_ct_eval_expr(context, true, expr, false);
			if (!expr) return NULL;
			if (expr->expr_kind != EXPR_TYPEINFO)
			{
				SEMA_ERROR(expr, "Only type names may be resolved with $evaltype.");
				return poisoned_type;
			}
			type_info = expr->type_expr;
			goto RETRY;
		}
		case TYPE_INFO_SLICE:
		{
			// If it's an array, make sure we can resolve the length
			Type *type = sema_expr_check_type_exists(context, type_info->array.base);
			if (!type) return NULL;
			if (!type_ok(type)) return type;
			return type_get_slice(type);
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


static inline bool sema_may_reuse_lambda(SemaContext *context, Decl *lambda, Type **types)
{
	Signature *sig = &lambda->func_decl.signature;
	if (typeget(sig->rtype)->canonical != types[0]) return false;
	FOREACH_IDX(i, Decl *, param, sig->params)
	{
		TypeInfo *info = vartype(param);
		ASSERT_SPAN(lambda, info && types[i + 1]); // NOLINT
		if (info->type->canonical != types[i + 1]) return false;
	}
	return true;
}

static inline Type *sema_evaluate_type_copy(SemaContext *context, TypeInfo *type_info)
{
	if (type_info->resolve_status == RESOLVE_DONE) return type_info->type;
	type_info = copy_type_info_single(type_info);
	if (!sema_resolve_type_info(context, type_info, RESOLVE_TYPE_DEFAULT)) return NULL;
	return type_info->type;
}

INLINE bool lambda_parameter_match(Decl **ct_lambda_params, Decl *candidate)
{
	unsigned param_count = vec_size(ct_lambda_params);
	ASSERT0(vec_size(candidate->func_decl.lambda_ct_parameters) == param_count);
	if (!param_count) return true;
	FOREACH_IDX(i, Decl *, param, candidate->func_decl.lambda_ct_parameters)
	{
		Decl *ct_param = ct_lambda_params[i];
		if (!param->var.is_read) continue;
		ASSERT0(ct_param->resolve_status == RESOLVE_DONE || param->resolve_status == RESOLVE_DONE);
		ASSERT0(ct_param->var.kind == param->var.kind);
		switch (ct_param->var.kind)
		{
			case VARDECL_LOCAL_CT_TYPE:
			case VARDECL_PARAM_CT_TYPE:
				if (ct_param->var.init_expr->type_expr->type->canonical !=
					param->var.init_expr->type_expr->type->canonical)
					return false;
				break;
			case VARDECL_LOCAL_CT:
			case VARDECL_PARAM_CT:
				if (!expr_is_const(ct_param->var.init_expr)) return false;
				if (!expr_is_const(param->var.init_expr)) return false;
				if (!expr_const_compare(&ct_param->var.init_expr->const_expr,
										&param->var.init_expr->const_expr, BINARYOP_EQ)) return false;
				break;
			default:
				UNREACHABLE
		}
	}
	return true;
}

static inline Decl *sema_find_cached_lambda(SemaContext *context, Type *func_type, Decl *original, Decl **ct_lambda_parameters)
{

	unsigned cached = vec_size(original->func_decl.generated_lambda);
	if (!cached) return NULL;
	// If it has a function type, then we just use that for comparison.
	if (func_type)
	{
		Type *raw = func_type->canonical->pointer->function.prototype->raw_type;
		FOREACH(Decl *, candidate, original->func_decl.generated_lambda)
		{
			if (raw == candidate->type->function.prototype->raw_type &&
				lambda_parameter_match(ct_lambda_parameters, candidate))
				return candidate;
		}
		return NULL;
	}
	Signature *sig = &original->func_decl.signature;
	if (!sig->rtype) return NULL;
	Type *rtype = sema_evaluate_type_copy(context, type_infoptr(sig->rtype));
	if (!rtype) return NULL;
	Type *types[200];
	types[0] = rtype;
	FOREACH_IDX(i, Decl *, param, sig->params)
	{
		TypeInfo *info = vartype(param);
		if (!info) return NULL;
		Type *type = sema_evaluate_type_copy(context, info);
		if (!type) return NULL;
		ASSERT0(i < 198);
		types[i + 1] = type;
	}

	FOREACH(Decl *, candidate, original->func_decl.generated_lambda)
	{
		if (sema_may_reuse_lambda(context, candidate, types) &&
			lambda_parameter_match(ct_lambda_parameters, candidate))
			return candidate;
	}
	return NULL;
}

static inline bool sema_expr_analyse_embed(SemaContext *context, Expr *expr, bool allow_fail)
{
	static File no_file;
	Expr *filename = expr->embed_expr.filename;
	if (!sema_analyse_ct_expr(context, filename)) return false;
	Expr *len_expr = expr->embed_expr.len;
	size_t len = ~((size_t)0);
	if (len_expr)
	{
		if (!sema_analyse_ct_expr(context, len_expr)) return false;
		if (!expr_is_const_int(len_expr)) RETURN_SEMA_ERROR(len_expr, "Expected an integer value.");
		Int i = len_expr->const_expr.ixx;
		if (int_is_neg(i) || int_is_zero(i)) RETURN_SEMA_ERROR(len_expr, "Expected a positive value for the limit.");
		Int max = { .i.low = len, .type = TYPE_U64 };
		if (int_comp(i, max, BINARYOP_LT))
		{
			len = int_to_u64(i);
		}
	}
	if (!expr_is_const_string(filename)) RETURN_SEMA_ERROR(filename, "A compile time string was expected.");

	CompilationUnit *unit = context->unit;
	const char *string = filename->const_expr.bytes.ptr;
	bool loaded;
	const char *error;
	char *path;
	char *name;
	if (file_namesplit(unit->file->full_path, &name, &path))
	{
		string = file_append_path(path, string);
	}
	char *content = file_read_binary(string, &len);
	if (!content)
	{
		if (!allow_fail) RETURN_SEMA_ERROR(expr, "Failed to load '%s'.", string);
		if (!compiler.context.io_error_file_not_found)
		{
			Module *module = global_context_find_module(kw_std__io);
			Decl *io_error = module ? module_find_symbol(module, kw_IoError) : NULL;
			Decl *fault = poisoned_decl;
			if (io_error && io_error->decl_kind == DECL_FAULT)
			{
				FOREACH(Decl *, f, io_error->enums.values)
				{
					if (f->name == kw_FILE_NOT_FOUND)
					{
						fault = f;
						break;
					}
				}
			}
			compiler.context.io_error_file_not_found = fault;
		}
		if (!decl_ok(compiler.context.io_error_file_not_found))
		{
			RETURN_SEMA_ERROR(expr, "Cannot generate an optional result, no IoError.FILE_NOT_FOUND could be located.");
		}
		expr->expr_kind = EXPR_OPTIONAL;
		expr->inner_expr = filename;
		filename->expr_kind = EXPR_CONST;
		filename->const_expr.const_kind = CONST_ERR;
		expr->type = type_wildcard_optional;
		filename->const_expr.enum_err_val = compiler.context.io_error_file_not_found;
		filename->resolve_status = RESOLVE_DONE;
		expr->resolve_status = RESOLVE_DONE;
		return true;
	}
	expr->const_expr = (ExprConst){
			.const_kind = CONST_BYTES,
			.bytes.ptr = content,
			.bytes.len = len,
	};
	expr->expr_kind = EXPR_CONST;
	expr->resolve_status = RESOLVE_DONE;
	expr->type = type_get_slice(type_char);
	return true;
}

static inline bool sema_expr_analyse_generic_ident(SemaContext *context, Expr *expr)
{
	Expr *parent = exprptr(expr->generic_ident_expr.parent);
	if (parent->expr_kind != EXPR_IDENTIFIER)
	{
		SEMA_ERROR(parent, "Expected an identifier to parameterize.");
		return false;
	}
	Decl *symbol = sema_analyse_parameterized_identifier(context, parent->identifier_expr.path,
														 parent->identifier_expr.ident, parent->span,
														 expr->generic_ident_expr.parmeters, NULL);
	if (!decl_ok(symbol)) return false;
	expr->expr_kind = EXPR_IDENTIFIER;
	expr_resolve_ident(expr, symbol);
	return true;
}

static inline bool sema_expr_analyse_lambda(SemaContext *context, Type *target_type, Expr *expr)
{
	Decl *decl = expr->lambda_expr;
	if (!decl_ok(decl)) return false;
	if (decl->resolve_status == RESOLVE_DONE)
	{
		expr->type = type_get_func_ptr(decl->type);
		return true;
	}
	Type *flat = target_type ? type_flatten(target_type) : NULL;
	if (flat)
	{
		if (!type_is_func_ptr(flat))
		{
			RETURN_SEMA_ERROR(expr, "Can't convert a lambda to %s.", type_quoted_error_string(target_type));
		}
		if (!sema_resolve_type_decl(context, flat->pointer)) return false;
	}

	bool multiple = context->current_macro || context->ct_locals;

	// Capture CT variables
	Decl **ct_lambda_parameters = copy_decl_list_single(context->ct_locals);

	if (multiple && decl->resolve_status != RESOLVE_DONE)
	{
		Decl *decl_cached = sema_find_cached_lambda(context, flat, decl, ct_lambda_parameters);
		if (decl_cached)
		{
			expr->type = type_get_func_ptr(decl_cached->type);
			expr_rewrite_const_ref(expr, decl_cached);
			return true;
		}
	}
	Decl *original = decl;
	if (multiple) decl = expr->lambda_expr = copy_lambda_deep(decl);
	Signature *sig = &decl->func_decl.signature;
	Signature *to_sig = flat ? flat->canonical->pointer->function.signature : NULL;
	if (!sig->rtype)
	{
		if (!to_sig) goto FAIL_NO_INFER;
		sig->rtype = type_info_id_new_base(typeget(to_sig->rtype), expr->span);
	}
	if (to_sig && vec_size(to_sig->params) != vec_size(sig->params))
	{
		RETURN_SEMA_ERROR(expr, "The lambda doesn't match the required type %s.", type_quoted_error_string(target_type));
	}
	FOREACH_IDX(i, Decl *, param, sig->params)
	{
		if (param->var.type_info) continue;
		if (!to_sig) goto FAIL_NO_INFER;
		param->var.type_info = type_info_id_new_base(to_sig->params[i]->type, param->span);
	}
	CompilationUnit *unit = decl->unit = context->unit;
	ASSERT_SPAN(expr, !decl->name);
	scratch_buffer_clear();
	switch (context->call_env.kind)
	{
		case CALL_ENV_GLOBAL_INIT:
			scratch_buffer_append(unit->module->name->module);
			scratch_buffer_append(".$global");
			break;
		case CALL_ENV_FUNCTION:
		case CALL_ENV_FUNCTION_STATIC:
			if (context->current_macro)
			{
				scratch_buffer_append(unit->module->name->module);
				scratch_buffer_append(".");
				scratch_buffer_append(context->current_macro->name);
			}
			else
			{
				scratch_buffer_append(context->call_env.current_function->unit->module->name->module);
				scratch_buffer_append(".");
				scratch_buffer_append(context->call_env.current_function->name);
			}
			break;
		case CALL_ENV_ATTR:
			scratch_buffer_append(unit->module->name->module);
			scratch_buffer_append(".$attr");
			break;
	}
	scratch_buffer_append("$lambda");
	scratch_buffer_append_unsigned_int(++unit->lambda_count);
	decl->name = scratch_buffer_copy();
	decl->extname = decl->name;
	decl->type = type_new_func(decl, sig);
	if (!sema_analyse_function_signature(context, decl, NULL, sig->abi, sig)) return false;
	if (flat && flat->pointer->function.prototype->raw_type != decl->type->function.prototype->raw_type)
	{
		RETURN_SEMA_ERROR(expr, "The lambda has type %s, which doesn't match the required type %s.",
						  type_quoted_error_string(decl->type),
						  type_quoted_error_string(target_type));
	}
	decl->func_decl.lambda_ct_parameters = ct_lambda_parameters;
	decl->func_decl.is_lambda = true;
	if (context_is_macro(context) || !context->call_env.current_function)
	{
		decl->func_decl.in_macro = true;
	}
	decl->alignment = type_alloca_alignment(decl->type);
	// We will actually compile this into any module using it (from a macro) by necessity,
	// so we'll declare it as weak and externally visible.
	unit_register_external_symbol(context, decl);

	// Before function analysis, lambda evaluation is deferred
	if (unit->module->stage < ANALYSIS_FUNCTIONS)
	{
		vec_add(unit->module->lambdas_to_evaluate, decl);
	}
	else
	{
		SemaContext lambda_context;
		sema_context_init(&lambda_context, context->unit);
		if (sema_analyse_function_body(&lambda_context, decl))
		{
			vec_add(unit->lambdas, decl);
		}
		sema_context_destroy(&lambda_context);
	}

	expr->type = type_get_func_ptr(decl->type);
	// If it's a distinct type we have to make a cast.
	expr->resolve_status = RESOLVE_DONE;
	if (target_type && expr->type != target_type && !cast_explicit(context, expr, target_type)) return false;
	if (multiple)
	{
		vec_add(original->func_decl.generated_lambda, decl);
	}
	decl->resolve_status = RESOLVE_DONE;
	expr_rewrite_const_ref(expr, decl);
	return true;
FAIL_NO_INFER:
	SEMA_ERROR(expr, "Inferred lambda expressions cannot be used unless the type can be determined.");
	return false;
}

static inline bool sema_expr_analyse_ct_feature(SemaContext *context, Expr *expr)
{
	if (expr->resolve_status == RESOLVE_DONE) return expr_ok(expr);

	Expr *inner = expr->ct_call_expr.main_var;
	if (expr->ct_call_expr.flat_path) goto ERROR;
	if (inner->expr_kind != EXPR_IDENTIFIER) goto ERROR;
	if (inner->resolve_status != RESOLVE_NOT_DONE) goto ERROR;
	if (!inner->identifier_expr.is_const) goto ERROR;

	const char *name = inner->identifier_expr.ident;
	void *value = htable_get(&compiler.context.features, (void *)name);
	ASSERT_SPAN(expr, !value || value == name);
	expr_rewrite_const_bool(expr, type_bool, value != NULL);
	return true;
ERROR:
	RETURN_SEMA_ERROR(inner, "Expected a feature name here, e.g. $feature(MY_FEATURE).");
}

static inline bool sema_expr_analyse_ct_is_const(SemaContext *context, Expr *expr)
{
	ASSERT_SPAN(expr, expr->resolve_status == RESOLVE_RUNNING);
	Expr *inner = expr->inner_expr;
	if (!sema_analyse_expr(context, inner)) return false;
	expr_rewrite_const_bool(expr, type_bool, sema_cast_const(inner));
	return true;
}

static inline bool sema_expr_analyse_ct_defined(SemaContext *context, Expr *expr)
{
	if (expr->resolve_status == RESOLVE_DONE) return expr_ok(expr);

	Expr **list = expr->expression_list;

	bool success = true;
	bool failed = false;
	unsigned list_len = vec_size(list);
	if (!list_len) RETURN_SEMA_ERROR(expr, "Expected at least one expression to test.");
	for (unsigned i = 0; i < list_len; i++)
	{
		Expr *main_expr = list[i];
		SemaContext *active_context = context;
		bool in_no_eval = active_context->call_env.in_no_eval;
		active_context->call_env.in_no_eval = true;
	RETRY:
		switch (main_expr->expr_kind)
		{
			case EXPR_OTHER_CONTEXT:
				active_context->call_env.in_no_eval = in_no_eval;
				active_context = expr->expr_other_context.context;
				in_no_eval = active_context->call_env.in_no_eval;
				active_context->call_env.in_no_eval = true;
				main_expr = expr->expr_other_context.inner;
				goto RETRY;
			case EXPR_ACCESS:
				if (!sema_expr_analyse_access(active_context, main_expr, &failed, CHECK_VALUE))
				{
					if (!failed) goto FAIL;
					success = false;
				}
				break;
			case EXPR_IDENTIFIER:
			{
				Decl *decl = sema_find_path_symbol(active_context, main_expr->identifier_expr.ident, main_expr->identifier_expr.path);
				if (!decl_ok(decl)) goto FAIL;
				success = decl != NULL;
				break;
			}
			case EXPR_COMPILER_CONST:
				success = sema_expr_analyse_compiler_const(active_context, main_expr, false);
				break;
			case EXPR_BUILTIN:
				success = sema_expr_analyse_builtin(active_context, main_expr, false);
				break;
			case EXPR_UNARY:
				main_expr->resolve_status = RESOLVE_RUNNING;
				if (!sema_expr_analyse_unary(active_context, main_expr, &failed, CHECK_VALUE))
				{
					if (!failed) goto FAIL;
					success = false;
				}
				break;
			case EXPR_TYPEINFO:
			{
				Type *type = sema_expr_check_type_exists(active_context, main_expr->type_expr);
				if (!type_ok(type)) goto FAIL;
				success = type != NULL;
				break;
			}
			case EXPR_CT_EVAL:
				success = sema_ct_eval_expr(active_context, "$eval", main_expr->inner_expr, false);
				break;
			case EXPR_HASH_IDENT:
			{
				Decl *decl = sema_resolve_symbol(active_context, main_expr->hash_ident_expr.identifier, NULL, main_expr->span);
				if (!decl) goto FAIL;
				main_expr = copy_expr_single(decl->var.init_expr);
				goto RETRY;
			}
			case EXPR_SUBSCRIPT:
			{
				if (!sema_expr_analyse_subscript(active_context, main_expr, CHECK_VALUE, true))
				{
					goto FAIL;
				}
				if (!expr_ok(main_expr))
				{
					success = false;
				}
				break;
			}
			case EXPR_CAST:
				if (!sema_expr_analyse_cast(active_context, main_expr, &failed))
				{
					if (!failed) goto FAIL;
					success = false;
				}
				break;
			case EXPR_CT_IDENT:
			{
				Decl *decl = sema_resolve_symbol(active_context, main_expr->ct_ident_expr.identifier, NULL, main_expr->span);
				if (!decl) goto FAIL;
				break;
			}
			case EXPR_CALL:
			{
				bool no_match;
				if (!sema_expr_analyse_call(active_context, main_expr, &no_match))
				{
					if (!no_match) goto FAIL;
					success = false;
				}
				break;
			}
			case EXPR_FORCE_UNWRAP:
				if (!sema_analyse_expr(active_context, main_expr->inner_expr)) goto FAIL;
				success = IS_OPTIONAL(main_expr->inner_expr);
				break;
			case EXPR_RETHROW:
				if (!sema_analyse_expr(active_context, main_expr->rethrow_expr.inner)) goto FAIL;
				success = IS_OPTIONAL(main_expr->rethrow_expr.inner);
				break;
			case EXPR_OPTIONAL:
				if (!sema_expr_analyse_optional(active_context, main_expr, &failed))
				{
					if (!failed) goto FAIL;
					success = false;
				}
				break;
			case EXPR_POISONED:
				success = false;
				break;
			case EXPR_COND:
			case EXPR_TEST_HOOK:
			case EXPR_CATCH_UNWRAP:
			case EXPR_DESIGNATOR:
			case EXPR_BENCHMARK_HOOK:
			case EXPR_TRY_UNWRAP:
			case EXPR_TRY_UNWRAP_CHAIN:
			case EXPR_ANYSWITCH:
			case EXPR_OPERATOR_CHARS:
			case EXPR_MACRO_BODY_EXPANSION:
			case EXPR_BUILTIN_ACCESS:
			case EXPR_DECL:
			case EXPR_LAST_FAULT:
			case EXPR_DEFAULT_ARG:
			case EXPR_NAMED_ARGUMENT:
				UNREACHABLE
			case EXPR_BINARY:
				main_expr->resolve_status = RESOLVE_RUNNING;
				if (!sema_expr_analyse_binary(context, main_expr, &failed))
				{
					if (!failed) goto FAIL;
					success = false;
				}
				break;
			case EXPR_CT_ARG:
			case EXPR_BITACCESS:
			case EXPR_BITASSIGN:
			case EXPR_COMPOUND_LITERAL:
			case EXPR_EMBED:
			case EXPR_GENERIC_IDENT:
			case EXPR_MACRO_BODY:
			case EXPR_POINTER_OFFSET:
			case EXPR_RETVAL:
			case EXPR_SLICE:
			case EXPR_SLICE_ASSIGN:
			case EXPR_SLICE_COPY:
			case EXPR_SUBSCRIPT_ADDR:
			case EXPR_SUBSCRIPT_ASSIGN:
			case EXPR_SWIZZLE:
			case EXPR_VASPLAT:
				REMINDER("Check if these should be analysed");
				FALLTHROUGH;
				// Above needs to be analysed
			case EXPR_INITIALIZER_LIST:
			case EXPR_DESIGNATED_INITIALIZER_LIST:
			case EXPR_ASM:
			case EXPR_CONST:
			case EXPR_NOP:
			case EXPR_MACRO_BLOCK:
			case EXPR_LAMBDA:
			case EXPR_EXPR_BLOCK:
			case EXPR_CT_IS_CONST:
			case EXPR_CT_APPEND:
			case EXPR_CT_DEFINED:
			case EXPR_CT_AND_OR:
			case EXPR_CT_CONCAT:
			case EXPR_STRINGIFY:
			case EXPR_TERNARY:
			case EXPR_CT_CASTABLE:
			case EXPR_CT_CALL:
			case EXPR_EXPRESSION_LIST:
			case EXPR_POST_UNARY:
			case EXPR_TYPEID:
			case EXPR_TYPEID_INFO:
			case EXPR_TYPECALL:
			case EXPR_MEMBER_GET:
			case EXPR_SPLAT:
			case EXPR_EXT_TRUNC:
			case EXPR_INT_TO_BOOL:
			case EXPR_VECTOR_TO_ARRAY:
			case EXPR_SLICE_TO_VEC_ARRAY:
			case EXPR_SCALAR_TO_VECTOR:
			case EXPR_PTR_ACCESS:
			case EXPR_ENUM_FROM_ORD:
			case EXPR_SLICE_LEN:
			case EXPR_ANYFAULT_TO_FAULT:
			case EXPR_VECTOR_FROM_ARRAY:
			case EXPR_RVALUE:
			case EXPR_RECAST:
			case EXPR_MAKE_ANY:
			case EXPR_DISCARD:
			case EXPR_ADDR_CONVERSION:
			case EXPR_FLOAT_TO_INT:
			case EXPR_INT_TO_FLOAT:
			case EXPR_INT_TO_PTR:
			case EXPR_PTR_TO_INT:
			case EXPR_MAKE_SLICE:
				if (!sema_analyse_expr(active_context, main_expr)) goto FAIL;
				break;
		}
		active_context->call_env.in_no_eval = in_no_eval;
		if (success) continue;
		break;
FAIL:
		active_context->call_env.in_no_eval = in_no_eval;
		return false;
	}
	expr_rewrite_const_bool(expr, type_bool, success);
	return true;
}

static inline bool sema_expr_analyse_ct_arg(SemaContext *context, Type *infer_type, Expr *expr)
{
	ASSERT_SPAN(expr, expr->resolve_status == RESOLVE_RUNNING);
	TokenType type = expr->ct_arg_expr.type;
	if (!context->current_macro)
	{
		SEMA_ERROR(expr, "'%s' can only be used inside of a macro.", token_type_to_string(type));
		return false;
	}
	switch (type)
	{
		case TOKEN_CT_VACOUNT:
			expr_rewrite_const_int(expr, type_usz, vec_size(context->macro_varargs));
			return true;
		case TOKEN_CT_VAARG:
		{
			unsigned index = 0;
			// A normal argument, this means we only evaluate it once.
			ASSIGN_EXPR_OR_RET(Expr *arg_expr, sema_expr_analyse_ct_arg_index(context, exprptr(expr->ct_arg_expr.arg), &index), false);

			index++;
			ASSERT_SPAN(expr, index < 0x10000);
			Decl *decl = NULL;
			// Try to find the original param.
			FOREACH(Decl *, val, context->macro_params)
			{
				if (!val) continue;
				if (val->var.va_index == index && val->var.kind == VARDECL_PARAM)
				{
					decl = val;
					break;
				}
			}
			// Not found, so generate a new.
			if (!decl)
			{
				if (!sema_analyse_inferred_expr(context, infer_type, arg_expr)) return false;
				switch (sema_resolve_storage_type(context, arg_expr->type))
				{
					case STORAGE_ERROR:
						return false;
					case STORAGE_NORMAL:
						break;
					default:
						RETURN_SEMA_ERROR(expr, "The vararg doesn't have a valid runtime type.");
				}
				decl = decl_new_generated_var(arg_expr->type, VARDECL_PARAM, arg_expr->span);
				decl->var.init_expr = arg_expr;
				decl->var.va_index = (uint16_t)index;
				vec_add(context->macro_params, decl);
			}
			// Replace with the identifier.
			expr->expr_kind = EXPR_IDENTIFIER;
			expr_resolve_ident(expr, decl);
			ASSERT_SPAN(expr, expr->type);
			return true;
		}
		case TOKEN_CT_VAEXPR:
		{
			// An expr argument, this means we copy and evaluate.
			ASSIGN_EXPR_OR_RET(Expr *arg_expr, sema_expr_analyse_ct_arg_index(context, exprptr(expr->ct_arg_expr.arg), NULL), false);
			expr_replace(expr, copy_expr_single(arg_expr));
			return sema_analyse_inferred_expr(context, infer_type, expr);
		}
		case TOKEN_CT_VACONST:
		{
			// An expr argument, this means we copy and evaluate.
			ASSIGN_EXPR_OR_RET(Expr *arg_expr, sema_expr_analyse_ct_arg_index(context, exprptr(expr->ct_arg_expr.arg), NULL), false);
			arg_expr = copy_expr_single(arg_expr);
			if (!sema_analyse_inferred_expr(context, infer_type, arg_expr)) return false;
			if (!sema_cast_const(arg_expr))
			{
				RETURN_SEMA_ERROR(arg_expr, "This argument needs to be a compile time constant.");
			}
			expr_replace(expr, arg_expr);
			return true;
		}
		case TOKEN_CT_VAREF:
		{
			SEMA_DEPRECATED(expr, "'$varef' is deprecated together with '&' arguments.");
			// A normal argument, this means we only evaluate it once.
			unsigned index = 0;
			ASSIGN_EXPR_OR_RET(Expr *arg_expr, sema_expr_analyse_ct_arg_index(context, exprptr(expr->ct_arg_expr.arg), &index), false);
			index++;
			ASSERT_SPAN(expr, index < 0x10000);

			Decl *decl = NULL;
			// Try to find the original param.
			FOREACH(Decl *, val, context->macro_params)
			{
				if (!val) continue;
				if (val->var.kind == VARDECL_PARAM_REF && val->var.va_index == index)
				{
					decl = val;
					break;
				}
			}
			// Not found, so generate a new.
			if (!decl)
			{
				arg_expr = copy_expr_single(arg_expr);
				if (!sema_analyse_expr_address(context, arg_expr)) return false;
				expr_insert_addr(arg_expr);
				decl = decl_new_generated_var(arg_expr->type, VARDECL_PARAM_REF, arg_expr->span);
				decl->var.init_expr = arg_expr;
				decl->var.va_index = (uint16_t)index;
				vec_add(context->macro_params, decl);
			}
			// Replace with the identifier.
			expr->expr_kind = EXPR_IDENTIFIER;
			expr_resolve_ident(expr, decl);
			ASSERT_SPAN(expr, expr->type);
			return true;
		}
		case TOKEN_CT_VATYPE:
		default:
			UNREACHABLE;
	}
}

static inline bool sema_expr_analyse_castable(SemaContext *context, Expr *expr)
{
	ASSERT_SPAN(expr, expr->resolve_status == RESOLVE_RUNNING);
	TypeInfo *type_info = type_infoptr(expr->castable_expr.type);
	bool in_no_eval = context->call_env.in_no_eval;
	context->call_env.in_no_eval = true;
	if (!sema_resolve_type_info(context, type_info, RESOLVE_TYPE_ALLOW_INFER)) goto FAILED;
	Type *type = type_info->type;
	Expr *inner = exprptr(expr->castable_expr.expr);
	if (!sema_analyse_inferred_expr(context, type, inner)) goto FAILED;
	bool ok = may_cast(context, inner, type, !expr->castable_expr.is_assign, true);
	expr_rewrite_const_bool(expr, type_bool, ok);
	context->call_env.in_no_eval = in_no_eval;
	return true;
FAILED:
	context->call_env.in_no_eval = in_no_eval;
	return false;
}


static inline bool sema_expr_analyse_ct_and_or_fn(SemaContext *context, Expr *expr)
{
	ASSERT_SPAN(expr, expr->resolve_status == RESOLVE_RUNNING);
	bool is_and = expr->ct_and_or_expr.is_and;
	Expr **exprs = expr->ct_and_or_expr.args;
	FOREACH(Expr *, single_expr, exprs)
	{
		if (!sema_analyse_expr(context, single_expr)) return false;
		if (!expr_is_const_bool(single_expr))
			RETURN_SEMA_ERROR(single_expr, "Expected this to evaluate to a constant boolean.");
		if (single_expr->const_expr.b != is_and)
		{
			expr_rewrite_const_bool(expr, type_bool, !is_and);
			return true;
		}
	}
	expr_rewrite_const_bool(expr, type_bool, is_and);
	return true;
}

static inline bool sema_expr_analyse_ct_append(SemaContext *context, Expr *append_expr)
{
	ASSERT_SPAN(append_expr, append_expr->resolve_status == RESOLVE_RUNNING);
	if (!sema_expand_vasplat_exprs(context, append_expr->ct_concat)) return false;
	Expr **exprs = append_expr->ct_concat;
	unsigned expr_count = vec_size(exprs);
	if (!expr_count) RETURN_SEMA_ERROR(append_expr, "No list given.");
	Expr *list = exprs[0];
	if (expr_count < 2)
	{
		expr_replace(append_expr, list);
		return true;
	}
	for (unsigned i = 1; i < expr_count; i++)
	{
		Expr binary = { .expr_kind = EXPR_BINARY, .span = append_expr->span };
		binary.binary_expr.left = exprid(list);
		binary.binary_expr.right = exprid(exprs[i]);
		binary.binary_expr.operator = BINARYOP_CT_CONCAT;
		if (!sema_expr_analyse_ct_concat(context, &binary, list, exprs[i])) return false;
		*list = binary;
	}
	expr_replace(append_expr, list);
	return true;
}

static inline bool sema_expr_analyse_ct_stringify(SemaContext *context, Expr *expr)
{
	Expr *inner = expr->inner_expr;
	// Only hash ident style stringify reaches here.
	ASSERT_SPAN(expr, inner->expr_kind == EXPR_HASH_IDENT);
	while (true)
	{
		Decl *decl = sema_resolve_symbol(context, inner->ct_ident_expr.identifier, NULL, inner->span);
		if (!decl) return false;
		inner = decl->var.init_expr;
		while (inner->expr_kind == EXPR_OTHER_CONTEXT)
		{
			context = inner->expr_other_context.context;
			inner = inner->expr_other_context.inner;
		}
		if (inner->expr_kind != EXPR_HASH_IDENT) break;
	}
	const char *desc = span_to_string(inner->span);
	if (!desc)
	{
		RETURN_SEMA_ERROR(expr, "Failed to stringify hash variable contents - they must be a single line and not exceed 255 characters.");
	}
	expr_rewrite_const_string(expr, desc);
	return true;
}

static inline bool sema_expr_analyse_ct_eval(SemaContext *context, Expr *expr, CheckType check)
{
	TokenType type;
	Path *path = NULL;
	Expr *result = sema_ct_eval_expr(context, false, expr->inner_expr, true);
	if (!result) return false;
	if (result->expr_kind == EXPR_TYPEINFO)
	{
		RETURN_SEMA_ERROR(result, "Evaluation to a type requires the use of '$evaltype' rather than '$eval'.");
	}
	expr_replace(expr, result);
	return sema_analyse_expr_dispatch(context, expr, check);
}


static inline bool sema_expr_analyse_ct_offsetof(SemaContext *context, Expr *expr)
{
	Expr *main_var = expr->ct_call_expr.main_var;
	Decl *decl = sema_expr_analyse_var_path(context, main_var);
	if (!decl) return false;
	DesignatorElement **path = expr->ct_call_expr.flat_path;
	if (!vec_size(path))
	{
		SEMA_ERROR(expr, "Expected a path to get the offset of.");
		return false;
	}

	ByteSize offset = 0;
	Type *type = decl->type;
	FOREACH_IDX(i, DesignatorElement *, element, path)
	{
		Decl *member;
		ArraySize index = 0;
		Type *result_type;
		if (!sema_expr_analyse_decl_element(context,
											element,
											type,
											&member,
											&index,
											&result_type,
											i,
											i == 0 ? main_var->span : expr->span,
											NULL)) return false;
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

	expr_rewrite_const_int(expr, type_isz, offset);

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
		case TOKEN_CT_FEATURE:
			return sema_expr_analyse_ct_feature(context, expr);
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


static inline bool sema_expr_analyse_retval(SemaContext *context, Expr *expr)
{
	if ((context->active_scope.flags & (SCOPE_ENSURE | SCOPE_ENSURE_MACRO)) == 0)
	{
		RETURN_SEMA_ERROR(expr, "'return' as a value can only be used inside of an '@ensure'.");
	}
	Expr *return_value = context->return_expr;
	bool is_macro_ensure = (context->active_scope.flags & SCOPE_ENSURE_MACRO) != 0;
	if (is_macro_ensure)
	{
		expr->type = type_no_optional(return_value->type);
	}
	else
	{
		expr->type = type_no_optional(context->rtype);
		if (type_is_void(expr->type))
		{
			RETURN_SEMA_ERROR(expr, "'return' cannot be used on void functions.");
		}
	}
	ASSERT_SPAN(expr, return_value);
	if (expr_is_runtime_const(return_value))
	{
		expr_replace(expr, copy_expr_single(return_value));
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
	TypeInfo *type_info = expr->expr_compound_literal.type_info;
	// We allow infering the size of arrays.
	if (!sema_resolve_type_info(context, type_info, RESOLVE_TYPE_ALLOW_INFER)) return false;
	Type *type = type_info->type;
	if (type_is_optional(type))
	{
		RETURN_SEMA_ERROR(type_info, "The type here should always be written as a plain type and not an optional, please remove the '!'.");
	}
	if (!sema_resolve_type_structure(context, type, type_info->span)) return false;
	if (!sema_expr_analyse_initializer_list(context, type, expr->expr_compound_literal.initializer)) return false;
	expr_replace(expr, expr->expr_compound_literal.initializer);
	return true;
}


static inline bool sema_analyse_expr_dispatch(SemaContext *context, Expr *expr, CheckType check)
{
	switch (expr->expr_kind)
	{
		case EXPR_ANYSWITCH:
		case EXPR_ASM:
		case EXPR_BENCHMARK_HOOK:
		case EXPR_CATCH_UNWRAP:
		case EXPR_COND:
		case EXPR_DEFAULT_ARG:
		case EXPR_DESIGNATOR:
		case EXPR_MACRO_BODY:
		case EXPR_MACRO_BODY_EXPANSION:
		case EXPR_MEMBER_GET:
		case EXPR_NAMED_ARGUMENT:
		case EXPR_NOP:
		case EXPR_OPERATOR_CHARS:
		case EXPR_SWIZZLE:
		case EXPR_TEST_HOOK:
		case EXPR_TRY_UNWRAP:
		case EXPR_TRY_UNWRAP_CHAIN:
		case EXPR_TYPEID_INFO:
		case EXPR_FLOAT_TO_INT:
		case EXPR_INT_TO_FLOAT:
		case EXPR_INT_TO_PTR:
		case EXPR_PTR_TO_INT:
		case EXPR_ENUM_FROM_ORD:
		case EXPR_VECTOR_TO_ARRAY:
		case EXPR_SLICE_TO_VEC_ARRAY:
		case EXPR_SCALAR_TO_VECTOR:
		case EXPR_MAKE_SLICE:
			UNREACHABLE
		case EXPR_MAKE_ANY:
			if (!sema_analyse_expr(context, expr->make_any_expr.typeid)) return false;
			return sema_analyse_expr(context, expr->make_any_expr.inner);
		case EXPR_RVALUE:
		case EXPR_RECAST:
		case EXPR_ADDR_CONVERSION:
		case EXPR_DISCARD:
			return sema_analyse_expr(context, expr->inner_expr);
		case EXPR_PTR_ACCESS:
		case EXPR_SLICE_LEN:
		case EXPR_ANYFAULT_TO_FAULT:
		case EXPR_VECTOR_FROM_ARRAY:
			return sema_analyse_expr(context, expr->inner_expr);
		case EXPR_INT_TO_BOOL:
			return sema_analyse_expr(context, expr->int_to_bool_expr.inner);
		case EXPR_EXT_TRUNC:
			return sema_analyse_expr(context, expr->ext_trunc_expr.inner);
		case EXPR_SPLAT:
			RETURN_SEMA_ERROR(expr, "Splat ('...') may only appear in initializers and calls.");
		case EXPR_TYPECALL:
			RETURN_SEMA_ERROR(expr, "Expected '()' after this.");
		case EXPR_OTHER_CONTEXT:
		{
			bool in_no_eval = context->call_env.in_no_eval;
			context = expr->expr_other_context.context;
			expr_replace(expr, expr->expr_other_context.inner);
			if (expr->resolve_status == RESOLVE_DONE) return expr_ok(expr);
			ASSERT_SPAN(expr, expr->resolve_status == RESOLVE_NOT_DONE);
			expr->resolve_status = RESOLVE_RUNNING;
			bool in_other = context->call_env.in_other;
			bool was_in_no_eval = context->call_env.in_no_eval;
			context->call_env.in_other = true;
			context->call_env.in_no_eval = in_no_eval;
			bool success = sema_analyse_expr_dispatch(context, expr, check);
			context->call_env.in_other = in_other;
			context->call_env.in_no_eval = was_in_no_eval;
			return success;
		}
		case EXPR_CT_CASTABLE:
			return sema_expr_analyse_castable(context, expr);
		case EXPR_EMBED:
			return sema_expr_analyse_embed(context, expr, false);
		case EXPR_VASPLAT:
			RETURN_SEMA_ERROR(expr, "'$vasplat' can only be used inside of macros.");
		case EXPR_GENERIC_IDENT:
			return sema_expr_analyse_generic_ident(context, expr);
		case EXPR_LAMBDA:
			return sema_expr_analyse_lambda(context, NULL, expr);
		case EXPR_CT_IS_CONST:
			return sema_expr_analyse_ct_is_const(context, expr);
		case EXPR_CT_DEFINED:
			return sema_expr_analyse_ct_defined(context, expr);
		case EXPR_CT_APPEND:
		case EXPR_CT_CONCAT:
			return sema_expr_analyse_ct_append(context, expr);
		case EXPR_CT_AND_OR:
			return sema_expr_analyse_ct_and_or_fn(context, expr);
		case EXPR_CT_ARG:
			return sema_expr_analyse_ct_arg(context, NULL, expr);
		case EXPR_STRINGIFY:
			if (!sema_expr_analyse_ct_stringify(context, expr)) return false;
			return true;
		case EXPR_DECL:
			if (!sema_analyse_var_decl(context, expr->decl_expr, true)) return false;
			expr->type = expr->decl_expr->type;
			return true;
		case EXPR_LAST_FAULT:
			expr->type = type_anyfault;
			return true;
		case EXPR_RETVAL:
			return sema_expr_analyse_retval(context, expr);
		case EXPR_BUILTIN:
			return sema_expr_analyse_builtin(context, expr, true);
		case EXPR_CT_CALL:
			return sema_expr_analyse_ct_call(context, expr);
		case EXPR_HASH_IDENT:
			if (!sema_expr_fold_hash(context, expr)) return false;
			return sema_analyse_expr_check(context, expr, check);
		case EXPR_CT_IDENT:
			return sema_expr_analyse_ct_identifier(context, expr, check);
		case EXPR_OPTIONAL:
			return sema_expr_analyse_optional(context, expr, NULL);
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
			return sema_resolve_type_info(context, expr->type_expr, RESOLVE_TYPE_DEFAULT);
		case EXPR_SLICE:
			return sema_expr_analyse_slice(context, expr, check);
		case EXPR_FORCE_UNWRAP:
			return sema_expr_analyse_force_unwrap(context, expr);
		case EXPR_COMPOUND_LITERAL:
			return sema_expr_analyse_compound_literal(context, expr);
		case EXPR_EXPR_BLOCK:
			return sema_expr_analyse_expr_block(context, NULL, expr);
		case EXPR_RETHROW:
			return sema_expr_analyse_rethrow(context, expr);
		case EXPR_CONST:
			return true;
		case EXPR_CT_EVAL:
			return sema_expr_analyse_ct_eval(context, expr, check);
		case EXPR_BINARY:
			return sema_expr_analyse_binary(context, expr, NULL);
		case EXPR_TERNARY:
			return sema_expr_analyse_ternary(context, NULL, expr);
		case EXPR_UNARY:
		case EXPR_POST_UNARY:
			return sema_expr_analyse_unary(context, expr, NULL, check);
		case EXPR_TYPEID:
			return sema_expr_analyse_typeid(context, expr);
		case EXPR_IDENTIFIER:
			return sema_expr_analyse_identifier(context, NULL, expr);
		case EXPR_CALL:
			return sema_expr_analyse_call(context, expr, NULL);
		case EXPR_SUBSCRIPT:
		case EXPR_SUBSCRIPT_ADDR:
			return sema_expr_analyse_subscript(context, expr, check, false);
		case EXPR_BITACCESS:
		case EXPR_SUBSCRIPT_ASSIGN:
			UNREACHABLE
		case EXPR_ACCESS:
			return sema_expr_analyse_access(context, expr, NULL, check);
		case EXPR_INITIALIZER_LIST:
		case EXPR_DESIGNATED_INITIALIZER_LIST:
			return sema_expr_analyse_initializer_list(context, type_untypedlist, expr);
		case EXPR_CAST:
			return sema_expr_analyse_cast(context, expr, NULL);
		case EXPR_EXPRESSION_LIST:
			return sema_expr_analyse_expr_list(context, expr);
	}
	UNREACHABLE
}


bool sema_analyse_cond_expr(SemaContext *context, Expr *expr, CondResult *result)
{
	if (expr_is_ungrouped_binary(expr) && expr->binary_expr.operator == BINARYOP_ASSIGN)
	{
		RETURN_SEMA_ERROR(expr, "Assignment expressions must be enclosed in an extra () in conditionals.");
	}
	if (!sema_analyse_expr(context, expr)) return false;
	if (IS_OPTIONAL(expr))
	{
		RETURN_SEMA_ERROR(expr, "An optional %s cannot be implicitly converted to a regular boolean value, "
								"use '@ok(<expr>)' and '@catch(<expr>)' to conditionally execute on success "
								"or failure.",
								type_quoted_error_string(expr->type));
	}
	if (!cast_explicit(context, expr, type_bool)) return false;
	if (expr_is_const_bool(expr))
	{
		*result = expr->const_expr.b ? COND_TRUE : COND_FALSE;
	}
	return true;
}


bool sema_analyse_expr_rhs(SemaContext *context, Type *to, Expr *expr, bool allow_optional, bool *no_match_ref,
						   bool as_binary)
{
	if (to && type_is_optional(to))
	{
		to = to->optional;
		ASSERT_SPAN(expr, allow_optional);
	}
	if (expr->expr_kind == EXPR_EMBED && allow_optional)
	{
		if (!sema_expr_analyse_embed(context, expr, true)) return false;
	}
	else
	{
		if (!sema_analyse_inferred_expr(context, to, expr)) return false;
	}
	if (!sema_cast_rvalue(context, expr, true)) return false;
	Type *to_canonical = to ? to->canonical : NULL;
	Type *rhs_type = expr->type;
	Type *rhs_type_canonical = rhs_type->canonical;
	if (to && allow_optional && to_canonical != rhs_type_canonical && rhs_type_canonical->type_kind == TYPE_FAULTTYPE)
	{
		Type *flat = type_flatten(to);
		if (!type_is_fault_raw(flat) && sema_cast_const(expr))
		{
			if (no_match_ref) goto NO_MATCH_REF;
			print_error_after(expr->span, "You need to add a trailing '?' here to make this an optional.");
			return false;
		}
	}
	// Let's see if we have a fixed slice.
	if (to && type_is_arraylike(to_canonical) && expr->expr_kind == EXPR_SLICE && rhs_type_canonical->type_kind == TYPE_SLICE)
	{
		Type *element = type_get_indexed_type(rhs_type_canonical)->canonical;
		if (element != type_get_indexed_type(to_canonical)->canonical) goto NO_SLICE;
		IndexDiff len = range_const_len(&expr->slice_expr.range);
		if (len < 1) goto NO_SLICE;
		if (len != to_canonical->array.len)
		{
			if (no_match_ref) goto NO_MATCH_REF;
			RETURN_SEMA_ERROR(expr, "Slice length mismatch, expected %u but got %u.", to_canonical->array.len, len);
		}
		// Given x[3..7] -> (int[5]*)x[3..7]
		cast_no_check(context, expr, type_get_ptr(type_get_array(element, len)), IS_OPTIONAL(expr));
		// Deref
		expr_rewrite_insert_deref(expr);
		cast_no_check(context, expr, to, IS_OPTIONAL(expr));
		return true;
	}
	NO_SLICE:;
	if (to)
	{
		if (as_binary)
		{
			if (!cast_implicit_binary(context, expr, to, no_match_ref != NULL)) return false;
		}
		else
		{
			bool cast_works = no_match_ref ? cast_implicit_silent(context, expr, to, false) : cast_implicit(context, expr, to, false);
			if (!cast_works) return false;
		}
	}
	if (!allow_optional && IS_OPTIONAL(expr))
	{
		if (no_match_ref) goto NO_MATCH_REF;
		RETURN_SEMA_ERROR(expr, "It is not possible to cast from %s to %s.", type_quoted_error_string(expr->type),
				   type_quoted_error_string(type_no_optional(expr->type)));
	}
	return true;
NO_MATCH_REF:
	*no_match_ref = true;
	return false;
}

static inline bool sema_cast_ct_ident_rvalue(SemaContext *context, Expr *expr)
{
	Decl *decl = expr->ct_ident_expr.decl;
	Expr *copy = copy_expr_single(decl->var.init_expr);
	if (!copy)
	{
		RETURN_SEMA_ERROR(expr, "'%s' was not yet initialized to any value, assign a value to it before use.", decl->name);
	}
	if (!sema_analyse_expr(context, copy)) return false;
	sema_cast_const(copy);
	expr_replace(expr, copy);
	return true;
}

static inline bool sema_cast_rvalue(SemaContext *context, Expr *expr, bool mutate)
{
	if (!expr_ok(expr)) return false;
	switch (expr->expr_kind)
	{
		case EXPR_MEMBER_GET:
			RETURN_SEMA_ERROR(expr, "Expected a parameter to 'get', e.g. '$member.get(value)'.");
		case EXPR_MACRO_BODY_EXPANSION:
			if (!expr->body_expansion_expr.first_stmt)
			{
				SEMA_ERROR(expr, "'@%s' must be followed by ().", declptr(context->current_macro->func_decl.body_param)->name);
				return false;
			}
			break;
		case EXPR_TYPECALL:
			RETURN_SEMA_ERROR(expr, "A tag name must be given.");
		case EXPR_BUILTIN:
			RETURN_SEMA_ERROR(expr, "A builtin must be followed by ().");
		case EXPR_ACCESS:
			if (expr->access_expr.ref->decl_kind == DECL_FUNC)
			{
				RETURN_SEMA_ERROR(expr, "A function name must be followed by '(' or preceded by '&'.");
			}
			if (expr->access_expr.ref->decl_kind == DECL_MACRO)
			{
				RETURN_SEMA_ERROR(expr, "A macro name must be followed by '('.");
			}
			// We may have kept FOO.x.y as a reference, fold it now if y is not an aggregate.
			if (mutate) sema_expr_flatten_const_ident(expr->access_expr.parent);
			return true;
		case EXPR_TYPEINFO:
			RETURN_SEMA_ERROR(expr, "A type must be followed by either (...) or '.' unless passed as a macro type argument or assigned to a compile time type variable.");
		case EXPR_CT_IDENT:
			if (mutate && !sema_cast_ct_ident_rvalue(context, expr)) return false;
			break;
		case EXPR_IDENTIFIER:
			if (mutate && !sema_cast_ident_rvalue(context, expr)) return false;
			break;
		case EXPR_SUBSCRIPT:
		case EXPR_SLICE:
		{
			Expr *inner = exprptr(expr->expr_kind == EXPR_SUBSCRIPT ? expr->subscript_expr.expr : expr->slice_expr.expr);
			if (inner->expr_kind != EXPR_IDENTIFIER) break;
			Decl *decl = inner->identifier_expr.decl;
			if (decl->decl_kind != DECL_VAR) break;
			if (!decl->var.out_param) break;
			RETURN_SEMA_ERROR(expr, "'out' parameters may not be read.");
		}
		case EXPR_UNARY:
		{
			if (expr->unary_expr.operator != UNARYOP_DEREF) break;
			Expr *inner = expr->inner_expr;
			if (inner->expr_kind != EXPR_IDENTIFIER) break;
			Decl *decl = inner->identifier_expr.decl;
			if (decl->decl_kind != DECL_VAR) break;
			if (!decl->var.out_param) break;
			RETURN_SEMA_ERROR(expr, "'out' parameters may not be read.");
		}
		default:
			break;
	}
	return true;
}

bool sema_analyse_ct_expr(SemaContext *context, Expr *expr)
{
	if (!sema_analyse_expr_value(context, expr)) return false;
	if (expr->expr_kind == EXPR_TYPEINFO)
	{
		Type *cond_val = expr->type_expr->type;
		expr->expr_kind = EXPR_CONST;
		expr->const_expr.const_kind = CONST_TYPEID;
		expr->const_expr.typeid = cond_val->canonical;
		expr->type = type_typeid;
	}
	if (!sema_cast_rvalue(context, expr, true)) return false;
	if (!sema_cast_const(expr))
	{
		RETURN_SEMA_ERROR(expr, "Expected a compile time expression.");
	}
	return true;
}

bool sema_analyse_expr_value(SemaContext *context, Expr *expr)
{
	ASSERT0(expr);
	switch (expr->resolve_status)
	{
		case RESOLVE_NOT_DONE:
			expr->resolve_status = RESOLVE_RUNNING;
			if (!sema_analyse_expr_dispatch(context, expr, CHECK_VALUE)) return expr_poison(expr);
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

#define RESOLVE(expr__, check__) \
  do { \
  Expr *expr_temp__ = expr__; \
  switch (expr_temp__->resolve_status) { \
    case RESOLVE_NOT_DONE: \
        expr_temp__->resolve_status = RESOLVE_RUNNING; \
        if (!check__) return expr_poison(expr_temp__); \
		expr_temp__->resolve_status = RESOLVE_DONE; \
		return true; \
	case RESOLVE_RUNNING: \
		SEMA_ERROR(expr, "Recursive resolution of expression"); \
		return expr_poison(expr_temp__); \
	case RESOLVE_DONE: \
		return expr_ok(expr_temp__); \
	default: \
		UNREACHABLE \
	} } while (0);


static inline bool sema_analyse_expr_check(SemaContext *context, Expr *expr, CheckType check)
{
	RESOLVE(expr, sema_analyse_expr_dispatch(context, expr, check));
}

bool sema_analyse_expr_address(SemaContext *context, Expr *expr)
{
	return sema_analyse_expr_check(context, expr, CHECK_ADDRESS);
}

bool sema_analyse_expr_lvalue(SemaContext *context, Expr *expr)
{
	ASSERT0(expr);
	return sema_analyse_expr_check(context, expr, CHECK_LVALUE);
}

bool sema_expr_check_discard(SemaContext *context, Expr *expr)
{
	if (expr->expr_kind == EXPR_EXPRESSION_LIST)
	{
		FOREACH(Expr *, expr_element, expr->expression_list)
		{
			if (!sema_expr_check_discard(context, expr_element)) return false;
		}
		return true;
	}
	if (expr->expr_kind == EXPR_DECL) return true;
	if (expr->expr_kind == EXPR_SUBSCRIPT_ASSIGN || expr->expr_kind == EXPR_SLICE_ASSIGN) return true;
	if (expr->expr_kind == EXPR_BINARY && expr->binary_expr.operator >= BINARYOP_ASSIGN) return true;
	if (expr->expr_kind == EXPR_UNARY || expr->expr_kind == EXPR_POST_UNARY)
	{
		switch (expr->unary_expr.operator)
		{
			case UNARYOP_DEC:
			case UNARYOP_INC:
				return true;
			default:
				break;
		}
	}
	if (expr->expr_kind == EXPR_EXPR_BLOCK)
	{
		if (type_is_void(expr->type)) return true;
		RETURN_SEMA_ERROR(expr, "The block returns a value of type %s, which must be handled – did you forget to assign it to something?",
								type_quoted_error_string(expr->type));
	}
	if (expr->expr_kind == EXPR_MACRO_BLOCK)
	{
		if (expr->macro_block.is_must_use)
		{
			if (expr->macro_block.is_optional_return)
			{
				RETURN_SEMA_ERROR(expr, "The macro returns %s, which is an optional and must be handled. "
										"You can either assign it to a variable, rethrow it using '!', "
										"panic with '!!', use if-catch etc. You can also silence the error using a void cast (e.g. '(void)the_call()') to ignore the error.",
								  type_quoted_error_string(expr->type));
			}
			RETURN_SEMA_ERROR(expr, "The called macro is marked `@nodiscard` meaning the result should be kept. You can still discard it using a void cast (e.g. '(void)the_call()') if you want.");
		}
		if (expr->macro_block.had_optional_arg) goto ERROR_ARGS;
		return true;
	}
	if (expr->expr_kind == EXPR_CALL)
	{
		if (expr->call_expr.must_use)
		{
			if (expr->call_expr.is_optional_return)
			{
				RETURN_SEMA_ERROR(expr, "The function returns %s, which is an optional and must be handled. "
										"You can either assign it to a variable, rethrow it using '!', "
										"panic with '!!', use if-catch etc. You can also silence the error using a void cast (e.g. '(void)the_call()') to ignore the error.",
										type_quoted_error_string(expr->type));
			}
			RETURN_SEMA_ERROR(expr, "The called function is marked `@nodiscard` meaning the result should be kept. You can still discard it using a void cast (e.g. '(void)the_call()') if you want.");
		}
		if (expr->call_expr.has_optional_arg) goto ERROR_ARGS;
		return true;
	}
	if (!IS_OPTIONAL(expr)) return true;
	RETURN_SEMA_ERROR(expr, "An optional value may not be discarded, you can ignore it with a void cast '(void)', rethrow on optional with '!' or panic '!!' to avoid this error.");
ERROR_ARGS:
	RETURN_SEMA_ERROR(expr, "The result of this call is optional due to its argument(s). The optional result may not be implicitly discarded. Consider using '(void)', '!' or '!!' to handle this.");
}

bool sema_analyse_expr(SemaContext *context, Expr *expr)
{
	return sema_analyse_expr_value(context, expr) && sema_cast_rvalue(context, expr, true);
}

bool sema_cast_const(Expr *expr)
{
	ASSERT_SPAN(expr, expr->resolve_status == RESOLVE_DONE);
	switch (expr->expr_kind)
	{
		case EXPR_RECAST:
			if (sema_cast_const(expr->inner_expr))
			{
				Type *type = expr->type;
				expr_replace(expr, expr->inner_expr);
				expr->type = type;
				return true;
			}
			return false;
		case EXPR_ACCESS:
		case EXPR_BITACCESS:
		{
			Expr *parent = expr->access_expr.parent;
			Type *flat = type_flatten(parent->type);
			switch (flat->type_kind)
			{
				case TYPE_UNION:
				case TYPE_UNTYPED_LIST:
				case TYPE_BITSTRUCT:
				case TYPE_STRUCT:
					break;
				default:
					return false;
			}
			if (!sema_cast_const(expr->access_expr.parent)) return false;
			if (!sema_expr_fold_to_member(expr, expr->access_expr.parent, expr->access_expr.ref)) return false;
			return true;
		}
		case EXPR_SLICE:
			return false;
		case EXPR_SUBSCRIPT:
		{
			if (!expr_is_const(exprptr(expr->subscript_expr.index.expr))) return false;
			Expr *parent = exprptr(expr->subscript_expr.expr);
			Type *flat_type = type_flatten(parent->type);
			if (!type_is_any_arraylike(flat_type) || flat_type->type_kind == TYPE_UNTYPED_LIST) return false;
			if (!sema_cast_const(parent)) return false;
			return sema_expr_fold_to_index(expr, parent, expr->subscript_expr.index);
		}
		case EXPR_IDENTIFIER:
			sema_expr_flatten_const_ident(expr);
			return expr_is_const(expr);
		case EXPR_CONST:
			return true;
		default:
			return false;
	}
	UNREACHABLE
}

bool sema_analyse_inferred_expr(SemaContext *context, Type *infer_type, Expr *expr)
{
	infer_type = type_no_optional(infer_type);
RETRY:
	switch (expr->resolve_status)
	{
		case RESOLVE_NOT_DONE:
			break;
		case RESOLVE_RUNNING:
			SEMA_ERROR(expr, "Recursive resolution of list.");
			return expr_poison(expr);
		case RESOLVE_DONE:
			if (infer_type && expr->type != infer_type)
			{
				cast_implicit_silent(context, expr, infer_type, false);
			}
			return expr_ok(expr);
		default:
			UNREACHABLE
	}

	expr->resolve_status = RESOLVE_RUNNING;
	switch (expr->expr_kind)
	{
		case EXPR_HASH_IDENT:
			if (!sema_expr_fold_hash(context, expr)) return false;
			goto RETRY;
		case EXPR_OTHER_CONTEXT:
		{
			InliningSpan *new_span = context->inlined_at;
			context = expr->expr_other_context.context;
			InliningSpan *old_span = context->inlined_at;
			context->inlined_at = new_span;
			expr_replace(expr, expr->expr_other_context.inner);
			bool success = sema_analyse_inferred_expr(context, infer_type, expr);
			context->inlined_at = old_span;
			return success;
		}
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
		case EXPR_LAMBDA:
			if (!sema_expr_analyse_lambda(context, infer_type, expr)) return expr_poison(expr);
			break;
		case EXPR_TERNARY:
			if (!sema_expr_analyse_ternary(context, infer_type, expr)) return expr_poison(expr);
			break;
		case EXPR_CT_ARG:
			if (!sema_expr_analyse_ct_arg(context, infer_type, expr)) return expr_poison(expr);
			break;
		default:
			if (!sema_analyse_expr_dispatch(context, expr, CHECK_VALUE)) return expr_poison(expr);
			break;
	}
	if (!sema_cast_rvalue(context, expr, true)) return false;
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
	TokenType type = TOKEN_INVALID_TOKEN;
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
	SourceSpan original_span = method_call->span;
	*method_call = (Expr) { .expr_kind = EXPR_CALL,
			.span = original_span,
			.resolve_status = RESOLVE_RUNNING,
			.call_expr.func_ref = declid(method_decl),
			.call_expr.arguments = arguments,
			.call_expr.is_func_ref = true,
			.call_expr.is_type_method = true };
	Type *type = parent->type->canonical;
	Decl *first_param = method_decl->func_decl.signature.params[0];
	Type *first = first_param->type;
	// Deref / addr as needed.
	if (first_param->var.kind == VARDECL_PARAM_REF)
	{
		// DEPRECATED
		ASSERT_SPAN(method_call, first->type_kind == TYPE_POINTER);
		first = first->pointer;
	}
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
	else if (first_param->var.kind == VARDECL_PARAM_REF || !expr_may_ref(parent))
	{
		// DEPRECATED
		Expr *inner = expr_copy(parent);
		parent->expr_kind = EXPR_UNARY;
		Type *inner_type = inner->type;
		bool optional = type_is_optional(inner->type);
		parent->type = type_add_optional(type_get_ptr(type_no_optional(inner_type)), optional);
		parent->unary_expr.operator = UNARYOP_TADDR;
		parent->unary_expr.expr = inner;
		parent->resolve_status = RESOLVE_NOT_DONE;
		if (!sema_analyse_expr(context, parent)) return false;
		expr_rewrite_insert_deref(parent);
	}
	ASSERT_SPAN(method_call, parent && parent->type && first == parent->type->canonical);
	if (!sema_expr_analyse_general_call(context, method_call, method_decl, parent, false,
										NULL)) return expr_poison(method_call);
	method_call->resolve_status = RESOLVE_DONE;
	return true;
}

// Check if the assignment fits
bool sema_bit_assignment_check(SemaContext *context, Expr *right, Decl *member)
{
	// Don't check non-consts and non integers.
	if (!sema_cast_const(right) || !type_is_integer(right->type)) return true;

	unsigned bits = member->var.end_bit - member->var.start_bit + 1;

	// If we have enough bits to fit, then we're done.
	if (bits >= type_bit_size(right->type) || int_is_zero(right->const_expr.ixx)) return true;

	if (int_bits_needed(right->const_expr.ixx) > bits)
	{
		RETURN_SEMA_ERROR(right, "This constant would be truncated if stored in the "
								 "bitstruct, do you need a wider bit range?");
	}
	return true;
}
