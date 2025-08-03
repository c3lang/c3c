// Copyright (c) 2019-2025 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "sema_internal.h"
#include <math.h>

#define RETURN_SEMA_FUNC_ERROR(_decl, _node, ...) do { sema_error_at(context, (_node)->span, __VA_ARGS__); SEMA_NOTE(_decl, "The definition was here."); return false; } while (0)
#define RETURN_NOTE_FUNC_DEFINITION do { SEMA_NOTE(callee->definition, "The definition was here."); return false; } while (0)
#define RESOLVE(expr__, check__) \
  do { \
  Expr *expr_temp__ = expr__; \
  switch (expr_temp__->resolve_status) { \
    case RESOLVE_NOT_DONE: \
        expr_temp__->resolve_status = RESOLVE_RUNNING; \
        if (!(check__)) return expr_poison(expr_temp__); \
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


typedef enum
{
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
static inline bool sema_expr_analyse_subscript(SemaContext *context, Expr *expr, CheckType check, bool check_valid);
static inline bool sema_expr_analyse_pointer_offset(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_slice(SemaContext *context, Expr *expr, CheckType check);
static inline bool sema_expr_analyse_access(SemaContext *context, Expr *expr, bool *missing_ref, CheckType check,
                                            bool lvalue);
static inline bool sema_expr_analyse_compound_literal(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_builtin(SemaContext *context, Expr *expr, bool throw_error);
static inline bool sema_expr_analyse_binary(SemaContext *context, Type *infer_type, Expr *expr, bool *failed_ref);
static inline bool sema_expr_resolve_ct_eval(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_identifier(SemaContext *context, Type *to, Expr *expr);
static inline bool sema_expr_analyse_ct_identifier(SemaContext *context, Expr *expr);

static inline bool sema_expr_analyse_ternary(SemaContext *context, Type *infer_type, Expr *expr);
static inline bool sema_expr_analyse_cast(SemaContext *context, Expr *expr, bool *invalid_cast_ref);
static inline bool sema_expr_analyse_or_error(SemaContext *context, Expr *expr, Expr *left, Expr *right, Type *infer_type, bool *failed_ref);
static inline bool sema_expr_analyse_unary(SemaContext *context, Expr *expr, bool *failed_ref, CheckType check);
static inline bool sema_expr_analyse_embed(SemaContext *context, Expr *expr, bool allow_fail);

static inline bool sema_expr_analyse_rethrow(SemaContext *context, Expr *expr, Type *to);
static inline bool sema_expr_analyse_force_unwrap(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_typeid(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_call(SemaContext *context, Expr *expr, bool *no_match_ref);

static inline bool sema_expr_analyse_optional(SemaContext *context, Expr *expr, bool *failed_ref);
static inline bool sema_expr_analyse_compiler_const(SemaContext *context, Expr *expr, bool report_missing);
static inline bool sema_expr_analyse_ct_arg(SemaContext *context, Type *infer_type, Expr *expr);
static inline bool sema_expr_analyse_ct_stringify(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_ct_offsetof(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_ct_call(SemaContext *context, Expr *expr);

static inline bool sema_expr_analyse_retval(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_expr_list(SemaContext *context, Expr *expr);

// -- binary
static bool sema_expr_analyse_sub(SemaContext *context, Expr *expr, Expr *left, Expr *right, bool *failed_ref);
static bool sema_expr_analyse_add(SemaContext *context, Expr *expr, Expr *left, Expr *right, bool *failed_ref);
static bool sema_expr_analyse_mult(SemaContext *context, Expr *expr, Expr *left, Expr *right, bool *failed_ref);
static bool sema_expr_analyse_div(SemaContext *context, Expr *expr, Expr *left, Expr *right, bool *failed_ref);
static bool sema_expr_analyse_mod(SemaContext *context, Expr *expr, Expr *left, Expr *right, bool *failed_ref);
static bool sema_expr_analyse_bit(SemaContext *context, Expr *expr, Expr *left, Expr *right, OperatorOverload overload, bool *failed_ref);
static bool sema_expr_analyse_enum_add_sub(SemaContext *context, Expr *expr, Expr *left, Expr *right, bool *failed_ref);
static bool sema_expr_analyse_shift(SemaContext *context, Expr *expr, Expr *left, Expr *right, bool *failed_ref);
static bool sema_expr_check_shift_rhs(SemaContext *context, Expr *expr, Expr *left, Type *left_type_flat, Expr *right, Type *right_type_flat, bool *failed_ref,bool
                                      is_assign);
static bool sema_expr_analyse_and_or(SemaContext *context, Expr *expr, Expr *left, Expr *right, bool *failed_ref);
static bool sema_expr_analyse_slice_assign(SemaContext *context, Expr *expr, Type *left_type, Expr *right, bool *failed_ref);
static bool sema_expr_analyse_ct_identifier_assign(SemaContext *context, Expr *expr, Expr *left, Expr *right, bool *failed_ref);
static bool sema_expr_analyse_assign(SemaContext *context, Expr *expr, Expr *left, Expr *right, bool *failed_ref);
static bool sema_expr_analyse_comp(SemaContext *context, Expr *expr, Expr *left, Expr *right, bool *failed_ref);
static bool sema_expr_analyse_op_assign(SemaContext *context, Expr *expr, Expr *left, Expr *right, BinaryOp operator);

// -- unary
static inline bool sema_expr_analyse_addr(SemaContext *context, Expr *expr, bool *failed_ref, CheckType check);
static inline bool sema_expr_analyse_neg_plus(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_bit_not(SemaContext *context, Expr *expr, bool *failed_ref);
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

static inline bool sema_binary_analyse_subexpr(SemaContext *context, Expr *left, Expr *right);
static inline bool sema_binary_analyse_arithmetic_subexpr(SemaContext *context, Expr *expr, const char *error,
                                                          bool bool_and_bitstruct_is_allowed, OperatorOverload *operator_overload_ref, bool *failed_ref);
static bool sema_binary_check_unclear_op_precedence(Expr *left_side, Expr * main_expr, Expr *right_side);
static bool sema_binary_analyse_ct_op_assign(SemaContext *context, Expr *expr, Expr *left);
static bool sema_binary_arithmetic_promotion(SemaContext *context, Expr *left, Expr *right, Type *left_type, Type *right_type,
								 Expr *parent, const char *error_message, bool allow_bool_vec,
								 OperatorOverload *operator_overload_ref, bool *failed_ref);
static bool sema_binary_is_unsigned_always_same_comparison(SemaContext *context, Expr *expr, Expr *left, Expr *right,
														   Type *lhs_type, Type *rhs_type);
static bool sema_binary_is_expr_lvalue(SemaContext *context, Expr *top_expr, Expr *expr, bool *failed_ref);
static void sema_binary_unify_voidptr(Expr *left, Expr *right, Type **left_type_ref, Type **right_type_ref);

// -- function helper functions
static inline bool sema_expr_analyse_var_call(SemaContext *context, Expr *expr, Type *func_ptr_type,
											  bool optional, bool *no_match_ref);
static inline bool sema_expr_analyse_func_call(SemaContext *context, Expr *expr, Decl *decl,
											   Expr *struct_var, bool optional, bool *no_match_ref);

static inline bool sema_expr_setup_call_analysis(SemaContext *context, CalledDecl *
                                                 callee,
                                                 SemaContext *macro_context, Expr *call_expr, Type *rtype,
                                                 Ast *yield_body,
                                                 Decl **yield_params, Decl **params, BlockExit **block_exit_ref, InliningSpan *span_ref);
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
static inline bool sema_expr_analyse_decl_element(SemaContext *context, DesignatorElement *element, Type *type, Decl **member_ref, ArraySize *index_ref, Type **return_type, unsigned i, SourceSpan loc, bool *is_missing);
static Type *sema_expr_check_type_exists(SemaContext *context, TypeInfo *type_info);
static inline bool sema_cast_ct_ident_rvalue(SemaContext *context, Expr *expr);
static bool sema_expr_rewrite_to_typeid_property(SemaContext *context, Expr *expr, Expr *typeid, const char *kw, bool *was_error);
static bool sema_expr_rewrite_to_type_property(SemaContext *context, Expr *expr, Type *type, TypeProperty property, Type *parent_type);
static bool sema_type_property_is_valid_for_type(CanonicalType *original_type, TypeProperty property);
static bool sema_expr_rewrite_typeid_call(Expr *expr, Expr *typeid, TypeIdInfoKind kind, Type *result_type);
static inline void sema_expr_rewrite_typeid_kind(Expr *expr, Expr *parent);
static inline bool sema_expr_replace_with_enum_array(SemaContext *context, Expr *enum_array_expr, Decl *enum_decl);
static inline bool sema_expr_replace_with_enum_name_array(SemaContext *context, Expr *enum_array_expr, Decl *enum_decl);
static inline void sema_expr_rewrite_to_type_nameof(Expr *expr, Type *type, TokenType name_type);

static inline bool sema_create_const_kind(SemaContext *context, Expr *expr, Type *type);
static inline bool sema_create_const_len(Expr *expr, Type *type, Type *flat);
static inline bool sema_create_const_inner(SemaContext *context, Expr *expr, Type *type);
static inline bool sema_create_const_min(Expr *expr, Type *type, Type *flat);
static inline bool sema_create_const_max(Expr *expr, Type *type, Type *flat);
static inline bool sema_create_const_params(Expr *expr, Type *type);
static inline void sema_create_const_membersof(Expr *expr, Type *type, AlignSize alignment, AlignSize offset);
static inline void sema_create_const_methodsof(SemaContext *context, Expr *expr, Type *type);

static inline bool expr_both_any_integer_or_integer_bool_vector(Expr *left, Expr *right);
static inline bool expr_both_const_foldable(Expr *left, Expr *right, BinaryOp op);
static inline bool expr_const_foldable_unary(Expr *expr, UnaryOp unary);
static inline bool sema_identifier_find_possible_inferred(SemaContext *context, Type *to, Expr *expr);
static inline bool sema_expr_analyse_enum_constant(SemaContext *context, Expr *expr, const char *name, Decl *decl);

static inline bool sema_cast_ident_rvalue(SemaContext *context, Expr *expr);
static inline bool sema_cast_rvalue(SemaContext *context, Expr *expr, bool mutate);

static inline bool sema_expr_analyse_type_access(SemaContext *context, Expr *expr, Type *parent_type, Expr *identifier, bool *missing_ref);
static inline bool sema_expr_analyse_member_access(SemaContext *context, Expr *expr, Expr *parent, Expr *identifier, bool *missing_ref);
static inline bool sema_expr_fold_to_member(Expr *expr, Expr *parent, Decl *member);
static inline bool sema_expr_fold_to_index(Expr *expr, Expr *parent, SubscriptIndex index_expr);
static inline bool sema_expr_fold_hash(SemaContext *context, Expr *expr);

static inline void sema_expr_flatten_const_ident(Expr *expr);
static inline bool sema_analyse_expr_check(SemaContext *context, Expr *expr, CheckType check);

static inline Expr **sema_prepare_splat_insert(Expr **exprs, unsigned added, unsigned insert_point);
static inline bool sema_analyse_maybe_dead_expr(SemaContext *, Expr *expr, bool is_dead, Type *infer_type);
static inline bool sema_insert_binary_overload(SemaContext *context, Expr *expr, Decl *overload, Expr *lhs, Expr *rhs, bool reverse);
static bool sema_replace_with_overload(SemaContext *context, Expr *expr, Expr *left, Expr *right, Type *left_type, OperatorOverload* operator_overload_ref);

INLINE bool sema_expr_analyse_ptr_add(SemaContext *context, Expr *expr, Expr *left, Expr *right, CanonicalType *left_type, CanonicalType *right_type, Type *cast_to_iptr, bool *failed_ref);
static Type *defer_iptr_cast(Expr *maybe_pointer);

// -- implementations

typedef struct
{
	bool in_no_eval;
} ContextSwitchState;

static inline ContextSwitchState context_switch_state_push(SemaContext *context, SemaContext *new_context)
{

	ContextSwitchState state = { .in_no_eval = new_context->call_env.in_no_eval, };
	new_context->call_env.in_no_eval = context->call_env.in_no_eval;
	return state;
}

static inline void context_switch_stat_pop(SemaContext *swapped, ContextSwitchState state)
{
	swapped->call_env.in_no_eval = state.in_no_eval;
}

Expr *sema_enter_inline_member(Expr *parent, CanonicalType *type)
{
	Expr *expr;
	switch (type->type_kind)
	{
		case TYPE_STRUCT:
		{
			Decl *decl = type->decl;
			if (!decl->is_substruct) return NULL;
			expr = expr_access_inline_member(parent, decl);
			break;
		}
		case TYPE_DISTINCT:
		{
			Decl *decl = type->decl;
			if (!decl->is_substruct) return NULL;
			expr = expr_copy(parent);
			type = type->decl->distinct->type;
			expr->type = type;
			break;
		}
		case TYPE_ENUM:
		{
			Decl *decl = type->decl;
			if (!decl->is_substruct) return NULL;

			if (parent->expr_kind == EXPR_CONST)
			{
				if (decl->enums.inline_value)
				{
					expr = expr_new_expr(EXPR_CONST, parent);
					expr_rewrite_const_int(expr, decl->enums.type_info->type, parent->const_expr.enum_val->enum_constant.inner_ordinal);
					return expr;
				}
				expr = copy_expr_single(parent->const_expr.enum_val->enum_constant.associated[decl->enums.inline_index]);
				break;
			}
			if (decl->enums.inline_value)
			{
				expr = copy_expr_single(parent);
				expr->type = type_add_optional(decl->enums.type_info->type, IS_OPTIONAL(parent));
				break;
			}
			expr = expr_new(EXPR_ACCESS_RESOLVED, parent->span);
			expr->resolve_status = RESOLVE_DONE;
			expr->access_resolved_expr.parent = parent;
			expr->access_resolved_expr.ref = decl->enums.parameters[decl->enums.inline_value];
			expr->type = expr->access_resolved_expr.ref->type;
			break;
		}
		default:
			return NULL;
	}
	if (IS_OPTIONAL(parent)) expr->type = type_add_optional(expr->type, true);
	return expr;
}

Expr *sema_expr_analyse_ct_arg_index(SemaContext *context, Expr *index_expr, unsigned *index_ref)
{
	unsigned args = vec_size(context->macro_varargs);
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

Expr *sema_resolve_string_ident(SemaContext *context, Expr *inner, bool report_missing)
{
	ASSERT_SPAN(inner, expr_is_const_string(inner));
	Path *path = NULL;
	const char *interned_version = NULL;
	TokenType token = sema_splitpathref(inner->const_expr.bytes.ptr, inner->const_expr.bytes.len, &path, &interned_version);
	switch (token)
	{
		case TOKEN_CONST_IDENT:
			inner->unresolved_ident_expr.is_const = true;
			break;
		case TOKEN_HASH_IDENT:
			if (path) goto NO_PATH;
			inner->expr_kind = EXPR_HASH_IDENT;
			inner->ct_ident_expr.identifier = interned_version;
			inner->resolve_status = RESOLVE_NOT_DONE;
			return inner;
		case TOKEN_CT_IDENT:
			if (path) goto NO_PATH;
			inner->expr_kind = EXPR_CT_IDENT;
			inner->ct_ident_expr.identifier = interned_version;
			inner->resolve_status = RESOLVE_NOT_DONE;
			return inner;
		case TOKEN_AT_IDENT:
		case TOKEN_IDENT:
			if (!interned_version)
			{
				if (report_missing)
				{
					SEMA_ERROR(inner, "'%.*s' could not be found, did you spell it right?", (int)inner->const_expr.bytes.len, inner->const_expr.bytes.ptr);
				}
				return NULL;
			}
			inner->unresolved_ident_expr.is_const = false;
			break;
		case TYPE_TOKENS:
		{
			if (path) goto NO_PATH;
			TypeInfo *info = type_info_new_base(type_from_token(token), inner->span);
			inner->expr_kind = EXPR_TYPEINFO;
			inner->resolve_status = RESOLVE_NOT_DONE;
			inner->type_expr = info;
			return inner;
		}
		case TOKEN_CT_TYPE_IDENT:
			if (path) goto NO_PATH;
			FALLTHROUGH;
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
			SEMA_ERROR(inner, "'%.*s' could not be resolved to a valid symbol.", (int)inner->const_expr.bytes.len, inner->const_expr.bytes.ptr);
			return poisoned_expr;
	}
	inner->expr_kind = EXPR_UNRESOLVED_IDENTIFIER;
	inner->resolve_status = RESOLVE_NOT_DONE;
	inner->unresolved_ident_expr.ident = interned_version;
	inner->unresolved_ident_expr.path = path;
	return inner;
NO_PATH:
	if (report_missing)
	{
		SEMA_ERROR(inner, "Unexpected path in '%.*s'.", (int)inner->const_expr.bytes.len, inner->const_expr.bytes.ptr);
	}
	return NULL;
}

Expr *sema_ct_eval_expr(SemaContext *context, bool is_type_eval, Expr *inner, bool report_missing)
{
	if (!sema_analyse_ct_expr(context, inner)) return NULL;
	if (!expr_is_const_string(inner))
	{
		SEMA_ERROR(inner, "'%s' expects a constant string as the argument.", is_type_eval ? "$evaltype" : "$eval");
		return poisoned_expr;
	}
	return sema_resolve_string_ident(context, inner, report_missing);
}

Expr *expr_access_inline_member(Expr *parent, Decl *parent_decl)
{
	Expr *embedded_struct = expr_new(EXPR_ACCESS_RESOLVED, parent->span);
	embedded_struct->resolve_status = RESOLVE_DONE;
	embedded_struct->access_resolved_expr.parent = parent;
	embedded_struct->access_resolved_expr.ref = parent_decl->strukt.members[0];
	embedded_struct->type = type_add_optional(embedded_struct->access_resolved_expr.ref->type, IS_OPTIONAL(parent));
	return embedded_struct;
}

static inline bool expr_const_foldable_unary(Expr *expr, UnaryOp unary)
{
	if (!sema_cast_const(expr) || !expr_is_const(expr)) return false;
	ConstKind kind = expr->const_expr.const_kind;
	switch (unary)
	{
		case UNARYOP_INC:
		case UNARYOP_DEC:
			return false;
		case UNARYOP_PLUS:
		case UNARYOP_NEG:
			return kind == CONST_FLOAT || kind == CONST_INTEGER;
		case UNARYOP_BITNEG:
			return kind == CONST_BOOL || kind == CONST_INTEGER;
		case UNARYOP_NOT:
			return kind == CONST_BOOL;
		case UNARYOP_ERROR:
			UNREACHABLE
		case UNARYOP_DEREF:
		case UNARYOP_ADDR:
		case UNARYOP_TADDR:
			UNREACHABLE
	}
	UNREACHABLE
}
static inline bool expr_both_const_foldable(Expr *left, Expr *right, BinaryOp op)
{
	if (!sema_cast_const(left) || !sema_cast_const(right) || !expr_is_const(left) || !expr_is_const(right)) return false;
	ConstKind a = left->const_expr.const_kind;
	ConstKind b = right->const_expr.const_kind;
	switch (a)
	{
		case CONST_BOOL:
			if (a != b) return false;
			switch (op)
			{
				case BINARYOP_BIT_AND:
				case BINARYOP_BIT_OR:
				case BINARYOP_BIT_XOR:
				case BINARYOP_NE:
				case BINARYOP_EQ:
				case BINARYOP_AND:
				case BINARYOP_OR:
					return true;
				default:
					return false;
			}
		case CONST_FLOAT:
		case CONST_INTEGER:
			if (a != b) return false;
			return op != BINARYOP_AND && op != BINARYOP_OR;
		case CONST_BYTES:
		case CONST_STRING:
			if (op != BINARYOP_EQ && op != BINARYOP_NE) return false;
			return a == b || b == CONST_SLICE || b == CONST_BYTES || b == CONST_STRING;
		case CONST_ENUM:
		case CONST_FAULT:
		case CONST_TYPEID:
		case CONST_REF:
			break;
		case CONST_POINTER:
			if (op == BINARYOP_SUB) return true;
			break;
		case CONST_SLICE:
			if (op != BINARYOP_EQ && op != BINARYOP_NE) return false;
			return b == CONST_BYTES || b == CONST_STRING;
		case CONST_INITIALIZER:
			switch (type_flatten(left->type)->type_kind)
			{
				case TYPE_VECTOR:
					if (a != b) return false;
					return op == BINARYOP_EQ || op == BINARYOP_NE;
				case TYPE_BITSTRUCT:
					if (a != b) return false;
					switch (op)
					{
						case BINARYOP_EQ:
						case BINARYOP_NE:
						case BINARYOP_BIT_AND:
						case BINARYOP_BIT_XOR:
						case BINARYOP_BIT_OR:
							return true;
						default:
							return false;
					}
				default:
					return false;
			}
		case CONST_UNTYPED_LIST:
			return false;
		case CONST_MEMBER:
			return true;
	}
	return (a == b) && (op == BINARYOP_EQ || op == BINARYOP_NE);
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

bool sema_expr_analyse_sprintf(SemaContext *context, Expr *expr, Expr *format_string, Expr **args, unsigned num_args)
{
	if (!sema_analyse_expr(context, format_string)) return false;
	if (!sema_cast_const(format_string))
	{
		RETURN_SEMA_ERROR(format_string, "Expected a constant format string expression.");
	}
	for (unsigned i = 0; i < num_args; i++)
	{
		Expr *e = args[i];
		if (!sema_analyse_expr(context, e)) return false;
		if (!sema_cast_const(e))
		{
			RETURN_SEMA_ERROR(e, "Expected a constant expression.");
		}
	}
	if (!expr_is_const_string(format_string))
	{
		RETURN_SEMA_ERROR(format_string, "Expected a constant format string.");
	}
	const char *inner_str = format_string->const_expr.bytes.ptr;
	ArraySize len = format_string->const_expr.bytes.len;
	scratch_buffer_clear();
	ArrayIndex current_index = 0;
	for (ArraySize i = 0; i < len; i++)
	{
		char c = inner_str[i];
		if (c == '%')
		{
			i++;
			switch (inner_str[i])
			{
				case 's':
					if (current_index == num_args) RETURN_SEMA_ERROR(format_string, "Too many arguments in format string.");
					expr_const_to_scratch_buffer(&(args[current_index++]->const_expr));
					continue;
				case '%':
					scratch_buffer_append_char('%');
					continue;
				default:
					RETURN_SEMA_ERROR(format_string, "Only '%%s' is supported for compile time sprintf.");
			}
		}
		scratch_buffer_append_char(c);
	}
	if (current_index != num_args)
	{
		RETURN_SEMA_ERROR(expr, "Too many arguments to sprintf.");
	}
	expr_rewrite_const_string(expr, scratch_buffer_copy());
	return true;
}

static bool sema_binary_is_expr_lvalue(SemaContext *context, Expr *top_expr, Expr *expr, bool *failed_ref)
{
	if (expr->expr_kind == EXPR_CT_SUBSCRIPT) return true;
	switch (expr->expr_kind)
	{
		case UNRESOLVED_EXPRS:
			UNREACHABLE
		case EXPR_SWIZZLE:
			if (failed_ref) goto FAILED_REF;
			if (expr->swizzle_expr.is_overlapping)
			{
				RETURN_SEMA_ERROR(expr, "You cannot use swizzling to assign to multiple elements if they are overlapping");
			}
			return sema_binary_is_expr_lvalue(context, top_expr, exprptr(expr->swizzle_expr.parent), failed_ref);
		case EXPR_LAMBDA:
			if (failed_ref) goto FAILED_REF;
			RETURN_SEMA_ERROR(expr, "This expression is a value and cannot be assigned to.");
		case EXPR_CT_IDENT:
			return true;
		case EXPR_EXT_TRUNC:
		case EXPR_INT_TO_BOOL:
		case EXPR_DISCARD:
			goto ERR;
		case EXPR_IDENTIFIER:
		{
			Decl *decl = expr->ident_expr;
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
		case EXPR_ACCESS_RESOLVED:
			if (!sema_binary_is_expr_lvalue(context, top_expr, expr->access_resolved_expr.parent, failed_ref)) return false;
			goto CHECK_OPTIONAL;
		case EXPR_SUBSCRIPT:
		case EXPR_SLICE:
		case EXPR_SUBSCRIPT_ASSIGN:
			goto CHECK_OPTIONAL;
		case EXPR_CONST:
			if (failed_ref) goto FAILED_REF;
			RETURN_SEMA_ERROR(top_expr, "You cannot assign to a constant expression.");
		case EXPR_CT_ARG:
		case EXPR_HASH_IDENT:
		case EXPR_POISONED:
		case EXPR_ADDR_CONVERSION:
		case EXPR_ASM:
		case EXPR_BENCHMARK_HOOK:
		case EXPR_BINARY:
		case EXPR_BITASSIGN:
		case EXPR_BUILTIN:
		case EXPR_BUILTIN_ACCESS:
		case EXPR_CALL:
		case EXPR_CATCH:
		case EXPR_COMPILER_CONST:
		case EXPR_COND:
		case EXPR_CT_CALL:
		case EXPR_CT_ASSIGNABLE:
		case EXPR_CT_DEFINED:
		case EXPR_CT_EVAL:
		case EXPR_CT_IS_CONST:
		case EXPR_DECL:
		case EXPR_DEFAULT_ARG:
		case EXPR_DESIGNATED_INITIALIZER_LIST:
		case EXPR_DESIGNATOR:
		case EXPR_FORCE_UNWRAP:
		case EXPR_INITIALIZER_LIST:
		case EXPR_LAST_FAULT:
		case EXPR_MACRO_BLOCK:
		case EXPR_MACRO_BODY_EXPANSION:
		case EXPR_MAKE_ANY:
		case EXPR_MAKE_SLICE:
		case EXPR_MEMBER_GET:
		case EXPR_MEMBER_SET:
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
		case EXPR_TRY:
		case EXPR_TRY_UNWRAP_CHAIN:
		case EXPR_TYPECALL:
		case EXPR_TYPEID_INFO:
		case EXPR_TYPEINFO:
		case EXPR_VECTOR_FROM_ARRAY:
		case EXPR_VECTOR_TO_ARRAY:
		case EXPR_SLICE_TO_VEC_ARRAY:
		case EXPR_SCALAR_TO_VECTOR:
		case EXPR_SUBSCRIPT_ADDR:
		case EXPR_EXPRESSION_LIST:
		case EXPR_TWO:
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
		case EXPR_DEFAULT_ARG:
		case EXPR_TYPECALL:
		case EXPR_MEMBER_GET:
		case EXPR_MEMBER_SET:
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
		case EXPR_INT_TO_BOOL:
		case EXPR_RVALUE:
		case EXPR_RECAST:
		case EXPR_DISCARD:
		case EXPR_ADDR_CONVERSION:
		case EXPR_MAKE_SLICE:
			return false;
		case EXPR_SUBSCRIPT_ASSIGN:
			return true;
		case UNRESOLVED_EXPRS:
			UNREACHABLE
		case EXPR_IDENTIFIER:
		{
			Decl *decl = expr->ident_expr;
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
		case EXPR_ACCESS_RESOLVED:
			return expr_may_ref(expr->access_resolved_expr.parent);
		case EXPR_SUBSCRIPT:
		case EXPR_SLICE:
		case EXPR_SUBSCRIPT_ADDR:
			return true;
		case EXPR_HASH_IDENT:
			return false;
		case EXPR_TWO:
			return expr_may_ref(expr->two_expr.last);
		case EXPR_EXPRESSION_LIST:
			if (!vec_size(expr->expression_list)) return false;
			return expr_may_ref(VECLAST(expr->expression_list));
		case EXPR_ASM:
		case EXPR_BENCHMARK_HOOK:
		case EXPR_BINARY:
		case EXPR_BITASSIGN:
		case EXPR_BUILTIN:
		case EXPR_BUILTIN_ACCESS:
		case EXPR_CALL:
		case EXPR_CATCH:
		case EXPR_COMPILER_CONST:
		case EXPR_COND:
		case EXPR_CONST:
		case EXPR_CT_ARG:
		case EXPR_CT_CALL:
		case EXPR_CT_ASSIGNABLE:
		case EXPR_CT_DEFINED:
		case EXPR_CT_EVAL:
		case EXPR_CT_IS_CONST:
		case EXPR_DECL:
		case EXPR_DESIGNATED_INITIALIZER_LIST:
		case EXPR_DESIGNATOR:
		case EXPR_FORCE_UNWRAP:
		case EXPR_INITIALIZER_LIST:
		case EXPR_LAST_FAULT:
		case EXPR_MACRO_BLOCK:
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
		case EXPR_TRY:
		case EXPR_TRY_UNWRAP_CHAIN:
		case EXPR_TYPEID_INFO:
		case EXPR_TYPEINFO:
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
		if (inner->expr_kind == EXPR_IDENTIFIER) inner->ident_expr->var.is_written = true;
		goto CHECK_INNER;
	}
	if (expr->expr_kind == EXPR_BITACCESS || expr->expr_kind == EXPR_ACCESS_RESOLVED) expr = expr->access_resolved_expr.parent;
	if (expr->expr_kind == EXPR_IDENTIFIER)
	{
		expr->ident_expr->var.is_written = true;
	}
	if (expr->expr_kind != EXPR_UNARY) return true;
	inner = expr->inner_expr;
CHECK_INNER:
	if (inner->expr_kind != EXPR_IDENTIFIER) return true;
	Decl *decl = inner->ident_expr;
	if (decl->decl_kind != DECL_VAR) return true;
	if (!decl->var.in_param || decl->var.out_param) return true;
	RETURN_SEMA_ERROR(inner, "'in' parameters may not be assigned to.");
}

static inline bool sema_cast_ident_rvalue(SemaContext *context, Expr *expr)
{
	Decl *decl = expr->ident_expr;
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
		case DECL_ENUM_CONSTANT:
			// This can't happen, inferred identifiers are folded to consts they are never identifiers.
			UNREACHABLE
		case DECL_VAR:
			break;
		case DECL_FAULT:
			expr_rewrite_const_fault(expr, decl);
			return true;
		case DECL_ALIAS:
		case DECL_ALIAS_PATH:
		case DECL_ATTRIBUTE:
		case DECL_BODYPARAM:
		case DECL_CT_ASSERT:
		case DECL_CT_ECHO:
		case DECL_CT_EXEC:
		case DECL_CT_INCLUDE:
		case DECL_DECLARRAY:
		case DECL_DISTINCT:
		case DECL_ERASED:
		case DECL_GROUP:
		case DECL_IMPORT:
		case DECL_TYPEDEF:
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
		case DECL_CONST_ENUM:
			SEMA_ERROR(expr, "Expected enum name followed by '.' and an enum value.");
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
		case VARDECL_LOCAL:
			if (decl->var.copy_const && decl->var.init_expr && expr_is_const(decl->var.init_expr))
			{
				assert(decl->var.init_expr->resolve_status == RESOLVE_DONE);
				expr_replace(expr, copy_expr_single(decl->var.init_expr));
				return true;
			}
			FALLTHROUGH;
		case VARDECL_PARAM:
		case VARDECL_GLOBAL:
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
			cast_no_check(copy, type_bool, false);
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

	Type *left_canonical = left->type->canonical;
	Type *right_canonical = right->type->canonical;
	if (left_canonical != right_canonical)
	{
		Type *max = type_find_max_type(type_no_optional(left_canonical), type_no_optional(right_canonical));
		if (!max)
		{
			SEMA_ERROR(expr, "Cannot find a common parent type of '%s' and '%s'",
					   type_to_error_string(left->type), type_to_error_string(right->type));
			return false;
		}
		Type *no_fail_max = type_no_optional(max);
		if (!cast_implicit_binary(context, left, no_fail_max, NULL)
			|| !cast_implicit_binary(context, right, no_fail_max, NULL)) return false;
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

	ASSERT_SPAN(expr, enum_constant->resolve_status != RESOLVE_NOT_DONE);
	expr->type = decl->type;
	if (enum_constant->enum_constant.is_raw)
	{
		expr_replace(expr, copy_expr_single(enum_constant->enum_constant.value));
		return true;
	}
	expr->expr_kind = EXPR_CONST;
	expr->const_expr.const_kind = CONST_ENUM;
	expr->const_expr.enum_val = enum_constant;
	expr->resolve_status = RESOLVE_DONE;
	return true;
}


static inline bool sema_identifier_find_possible_inferred(SemaContext *context, Type *to, Expr *expr)
{
	to = to->canonical;
	switch (to->type_kind)
	{
		case TYPE_CONST_ENUM:
		case TYPE_ENUM:
			if (!decl_ok(to->decl)) return expr_poison(expr), true;
			return sema_expr_analyse_enum_constant(context, expr, expr->unresolved_ident_expr.ident, to->decl);
		default:
			return false;
	}
}

static inline bool sema_expr_analyse_identifier(SemaContext *context, Type *to, Expr *expr)
{
	ASSERT(expr);
	ASSERT_SPAN(expr, expr->unresolved_ident_expr.ident);
	DEBUG_LOG("Resolving identifier '%s'", expr->unresolved_ident_expr.ident);

	ASSERT_SPAN(expr, expr->resolve_status != RESOLVE_DONE);
	DeclId body_param;
	if (!expr->unresolved_ident_expr.path && context->current_macro && (body_param = context->current_macro->func_decl.body_param)) // NOLINT
	{
		if (expr->unresolved_ident_expr.ident == declptr(body_param)->name)
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
	if (!expr->unresolved_ident_expr.path && to)
	{
		if (sema_identifier_find_possible_inferred(context, to, expr)) return expr_ok(expr);
	}

	Decl *decl = sema_find_path_symbol(context, expr->unresolved_ident_expr.ident, expr->unresolved_ident_expr.path);

	// Is this a broken decl?
	if (!decl_ok(decl)) return false;

	// Rerun if we can't do inference.
	if (!decl)
	{
		if (!expr->unresolved_ident_expr.path && expr->unresolved_ident_expr.is_const && (!to || to->canonical->type_kind != TYPE_ENUM))
		{
			CompilationUnit **units = context->unit->module->units;
			FOREACH (CompilationUnit *, unit, units)
			{
				FOREACH(Decl *, decl, unit->enums)
				{
					FOREACH(Decl *, enum_val, decl->enums.values)
					{
						if (enum_val->name == expr->unresolved_ident_expr.ident)
						{
							RETURN_SEMA_ERROR(expr, "No constant named '%s' was found in the current scope. Did you "
							   "mean the value '%s' of the enum '%s'? The enum type cannot be inferred%s, so in that case you need to use "
								"the qualified name: '%s.%s'.",
								enum_val->name, enum_val->name, decl->name, to ? " correctly" : "", decl->name, enum_val->name);
						}
					}
				}
			}
		}
		decl = sema_resolve_symbol(context, expr->unresolved_ident_expr.ident, expr->unresolved_ident_expr.path, expr->span);
		(void)decl;
		ASSERT_SPAN(expr, !decl);
		return false;
	}

	if (decl_needs_prefix(decl))
	{
		if (!sema_analyse_decl(context, decl)) return false;
		if (decl->unit->module != context->unit->module && !expr->unresolved_ident_expr.path)
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
				case DECL_ALIAS:
					message = "Aliases from other modules must be prefixed with the module name.";
					break;
				case DECL_FAULT:
					message = "Faults from other modules must be prefixed with the module name.";
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
	decl = decl_flatten(decl);
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
						RETURN_SEMA_ERROR(expr, "Constant value did not evaluate to a constant.");
					}
					expr_replace(expr, copy);
					return true;
				}
				break;
			case VARDECL_PARAM:
				if (context->call_env.is_naked_fn && !(context->active_scope.flags & SCOPE_MACRO))
				{
					RETURN_SEMA_ERROR(expr, "Parameters may not be directly accessed in '@naked' functions.");
				}
				break;
			case VARDECL_GLOBAL:
				if (context->call_env.pure)
				{
					RETURN_SEMA_ERROR(expr, "'@pure' functions may not access globals.");
				}
				break;
			default:
				break;
		}
	}
	else if (decl->decl_kind == DECL_FAULT)
	{
		expr_rewrite_const_fault(expr, decl);
		return true;
	}
	expr_resolve_ident(expr, decl);
	return true;
}

static inline bool sema_expr_resolve_ct_identifier(SemaContext *context, Expr *expr)
{
	ASSERT_SPAN(expr, expr->resolve_status != RESOLVE_DONE);
	ASSERT(expr && expr->ct_ident_expr.identifier);

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

	return true;
}


static inline bool sema_expr_analyse_ct_identifier(SemaContext *context, Expr *expr)
{
	if (!sema_expr_resolve_ct_identifier(context, expr)) return false;
	return sema_cast_ct_ident_rvalue(context, expr);
}


static inline bool sema_binary_analyse_with_inference(SemaContext *context, Expr *left, Expr *right, BinaryOp op)
{
	const static int op_table[BINARYOP_LAST + 1] = {
		[BINARYOP_AND] = 1, [BINARYOP_OR] = 1, [BINARYOP_CT_AND] = 1, [BINARYOP_CT_OR] = 1,
		[BINARYOP_EQ] = 2, [BINARYOP_NE] = 2, [BINARYOP_GT] = 2, [BINARYOP_GE] = 2,
		[BINARYOP_LE] = 2, [BINARYOP_LT] = 2 };
	int op_result = op_table[op];
	if (op_result == 1) return true;
	// If lhs or rhs is an initializer list, infer
	bool is_init_rhs = right->expr_kind == EXPR_INITIALIZER_LIST;
	bool is_init_lhs = left->expr_kind == EXPR_INITIALIZER_LIST;
	if (is_init_rhs && is_init_lhs) goto EVAL_BOTH;

	if (is_init_rhs)
	{
		if (!sema_analyse_expr(context, left)) return false;
		if (type_kind_is_any_vector(type_flatten(left->type)->type_kind))
		{
			return sema_analyse_inferred_expr(context, left->type, right);
		}
		return sema_analyse_expr(context, right);
	}
	if (is_init_lhs)
	{
		if (!sema_analyse_expr(context, right)) return false;
		if (type_kind_is_any_vector(type_flatten(right->type)->type_kind))
		{
			return sema_analyse_inferred_expr(context, right->type, left);
		}
		return sema_analyse_expr(context, left);
	}

	if (op_result != 2) goto EVAL_BOTH;

	if (!sema_analyse_expr(context, left)) return false;
	switch (left->type->canonical->type_kind)
	{
		case TYPE_ENUM:
		case TYPE_CONST_ENUM:
			return sema_analyse_inferred_expr(context, left->type, right);
		default:
			return sema_analyse_expr(context, right);
	}

EVAL_BOTH:
	return sema_analyse_expr(context, left) && sema_analyse_expr(context, right);
}

static inline bool sema_binary_analyse_subexpr(SemaContext *context, Expr *left, Expr *right)
{
	// Special handling of f = FOO_BAR
	if (right->expr_kind == EXPR_UNRESOLVED_IDENTIFIER && right->unresolved_ident_expr.is_const)
	{
		if (!sema_analyse_expr(context, left)) return false;
		switch (type_flatten(left->type)->type_kind)
		{
			case TYPE_ENUM:
			case TYPE_CONST_ENUM:
				return sema_analyse_inferred_expr(context, left->type, right);
			default:
				break;
		}
	}
	// Special handling of f = FOO_BAR
	if (left->expr_kind == EXPR_UNRESOLVED_IDENTIFIER && left->unresolved_ident_expr.is_const)
	{
		if (!sema_analyse_expr(context, right)) return false;
		switch (type_flatten(right->type)->type_kind)
		{
			case TYPE_ENUM:
			case TYPE_CONST_ENUM:
				return sema_analyse_inferred_expr(context, right->type, left);
			default:
				break;
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

static inline bool sema_binary_analyse_arithmetic_subexpr(SemaContext *context, Expr *expr, const char *error,
                                                          bool bool_and_bitstruct_is_allowed,
                                                          OperatorOverload *operator_overload_ref, bool *failed_ref)
{
	Expr *left = exprptr(expr->binary_expr.left);
	Expr *right = exprptr(expr->binary_expr.right);

	// 1. Analyse both sides.
	ASSERT_SPAN(expr, left->resolve_status == RESOLVE_DONE);
	ASSERT_SPAN(expr, right->resolve_status == RESOLVE_DONE);

	Type *left_type = type_no_optional(left->type)->canonical;
	Type *right_type = type_no_optional(right->type)->canonical;

	if (bool_and_bitstruct_is_allowed)
	{
		if (left_type->type_kind == TYPE_BITSTRUCT && left_type == right_type) return true;
		if (left_type == type_bool && right_type == type_bool) return true;
	}
	// 2. Perform promotion to a common type.
	return sema_binary_arithmetic_promotion(context, left, right, left_type, right_type, expr, error,
		bool_and_bitstruct_is_allowed, operator_overload_ref, failed_ref);
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

#define RETURN_ERR_WITH_DEFINITION do { if (compiler.context.errors_found != errors) SEMA_NOTE(definition, "The definition is here."); return false; } while (0)

static bool sema_analyse_parameter(SemaContext *context, Expr *arg, Decl *param, Decl *definition, bool *optional_ref,
								   bool *no_match_ref, bool macro, bool is_method_target)
{
	VarDeclKind kind = param->var.kind;
	Type *type = param->type;
	// 16. Analyse a regular argument.
	unsigned errors = compiler.context.errors_found;
	switch (kind)
	{
		case VARDECL_PARAM:
			// foo
			if (arg->expr_kind == EXPR_NAMED_ARGUMENT)
			{
				// This only happens in body arguments
				RETURN_SEMA_ERROR(arg, "Named arguments are not supported for body parameters.");
			}
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
			if (!sema_call_check_contract_param_match(context, param, arg)) RETURN_ERR_WITH_DEFINITION;
			if (!param->alignment)
			{
				ASSERT(macro && "Only in the macro case should we need to insert the alignment.");
				if (!sema_set_alloca_alignment(context, arg->type, &param->alignment)) return false;
			}
			break;
		case VARDECL_PARAM_EXPR:
			if (param->type)
			{
				if (!sema_analyse_expr_rhs(context, param->type, arg, true, NULL, false))
				{
					RETURN_ERR_WITH_DEFINITION;
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
			ASSERT(macro);
			if (!sema_analyse_expr_rhs(context, type, arg, true, no_match_ref, false))
			{
				RETURN_ERR_WITH_DEFINITION;
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
			if (!sema_analyse_expr_value(context, arg)) RETURN_ERR_WITH_DEFINITION;

			if (arg->expr_kind == EXPR_TYPEINFO)
			{
				assert(arg->type_expr->resolve_status == RESOLVE_DONE);
				expr_rewrite_const_typeid(arg, arg->type_expr->type);
			}
			if (!sema_cast_const(arg) || !expr_is_const_typeid(arg))
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
                                      bool *no_match_ref, Expr **expr_ref, bool *optional)
{
	Expr *init_expr = param->var.init_expr;
	if (!init_expr) return true;
	Expr *arg = copy_expr_single(init_expr);
	bool parameter_checked = false;
	if (arg->resolve_status != RESOLVE_DONE)
	{
		SemaContext default_context;
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
		if (!success) RETURN_NOTE_FUNC_DEFINITION;
		parameter_checked = true;
	}
	switch (param->var.kind)
	{
		case VARDECL_PARAM_CT:
		case VARDECL_PARAM_CT_TYPE:
		case VARDECL_PARAM_EXPR:
			*expr_ref = arg;
			param->var.defaulted = true;
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
	if (sema_cast_const(arg))
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
	if (context->call_env.kind != CALL_ENV_FUNCTION)
	{
		SEMA_ERROR(arg, "Cannot splat a non-constant value in a global context.");
		return NULL;
	}
	Decl *temp = decl_new_generated_var(arg->type, VARDECL_LOCAL, arg->span);
	Expr *decl_expr = expr_generate_decl(temp, arg);
	Expr *two = expr_new_expr(EXPR_TWO, arg);
	two->two_expr.first = decl_expr;
	Expr *subscript = expr_new_expr(EXPR_SUBSCRIPT, arg);
	subscript->subscript_expr.index.expr = exprid(expr_new_const_int(arg->span, type_usz, 0));
	subscript->subscript_expr.expr = exprid(expr_variable(temp));
	two->two_expr.last = subscript;
	if (!sema_analyse_expr(context, two)) return NULL;
	args[index] = two;
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
			return (ArrayIndex)type->array.len;
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

INLINE Type *sema_get_va_type(SemaContext *context, Expr *expr, Variadic variadic)
{
	if (variadic == VARIADIC_RAW)
	{
		if (expr->resolve_status != RESOLVE_DONE)
		{
			expr = copy_expr_single(expr);
			if (!sema_analyse_expr(context, expr)) return poisoned_type;
		}
		return type_flatten(expr->type);
	}
	assert(expr->expr_kind == EXPR_MAKE_ANY);
	return type_flatten(expr->make_any_expr.typeid->const_expr.typeid);
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
	int format_index = (int)sig->attrs.format - 1;
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

	ASSERT(func_param_count < MAX_PARAMS);
	Expr **actual_args = VECNEW(Expr*, func_param_count);
	for (unsigned i = 0; i < func_param_count; i++)
	{
		vec_add(actual_args, NULL);
	}

	// 2. Loop through the parameters.
	bool has_named = false;
	ArrayIndex last_index = -1;
	Expr *last_named_arg = INVALID_PTR;
	Expr *last = NULL;
	ArrayIndex needed = (ArrayIndex)func_param_count - (callee->struct_var ? 1 : 0);
	for (ArrayIndex i = 0; i < num_args; i++)
	{
		Expr *arg = args[i];
		if (i > 0) last = args[i - 1];
		ASSERT(expr_ok(arg));
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
			if (actual_args[index] && actual_args[index]->expr_kind != EXPR_DEFAULT_ARG && !param->var.defaulted)
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
				                               optional))
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
						cast_promote_vararg(arg);
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
		                               optional)) return false;
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
			int missing = 1;
			for (int j = i + 1; j < func_param_count; j++) if (!actual_args[j]) missing++;
			int implicit_args = callee->struct_var ? 1 : 0;
			if (vec_size(callee->params) == 1 + implicit_args)
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
			if (num_args == implicit_args)
			{
				if (missing != needed)
				{
					RETURN_SEMA_FUNC_ERROR(callee->definition, call, "'%s' expects %d-%d parameters, but none was provided.",
										   callee->name, missing, needed);
				}
				RETURN_SEMA_FUNC_ERROR(callee->definition, call, "'%s' expects %d parameter(s), but none was provided.",
									   callee->name, needed);
			}
			if (!last) last = args[0];
			int more_needed = (ArrayIndex)func_param_count - i;
			if (missing != more_needed)
			{
				RETURN_SEMA_FUNC_ERROR(callee->definition, last,
									   "%d-%d additional arguments were expected after this one, did you forget them?",
									   missing, more_needed);
			}
			RETURN_SEMA_FUNC_ERROR(callee->definition, last,
								   "%d more %s expected after this one, did you forget %s?",
								   more_needed, more_needed == 1 ? "argument was" : "arguments were", more_needed == 1 ? "it" : "them");
		}
		RETURN_SEMA_FUNC_ERROR(callee->definition, call, "The parameter '%s' must be set, did you forget it?", param->name);
	}
	call->call_expr.arguments = actual_args;
	if (format_index >= 0) goto CHECK_FORMAT;
	return true;
CHECK_FORMAT:;
	// Check
	Expr *expr = actual_args[format_index];
	if (!sema_cast_const(expr) || call->call_expr.va_is_splat) return true;
	assert(expr_is_const_string(expr));
	const char *data = expr->const_expr.bytes.ptr;
	size_t len = expr->const_expr.bytes.len;
	size_t idx = 0;
	Expr **vaargs = call->call_expr.varargs;
	unsigned vacount = vec_size(vaargs);
	for (size_t i = 0; i < len; i++)
	{
		if (data[i] != '%') continue;
		i++;
		char c = data[i];
NEXT_FLAG:
		switch (c)
		{
			case '-':
			case '+':
			case '0':
			case '#':
			case ' ':
				if (++i == len) goto UNEXPECTED_END;
				c = data[i];
				goto NEXT_FLAG;
			case '%':
				continue;
			default:
				break;
		}
		if (idx == vacount) goto TOO_FEW_ARGUMENTS;
		expr = vaargs[idx];
		Type *type = sema_get_va_type(context, expr, variadic);
		if (!type_ok(type)) return false;

		// Possible variable width
		if (c == '*')
		{
			if (!type_is_integer(type))
			{
				RETURN_SEMA_ERROR(vaargs[idx], "Expected an integer for the format width.");
			}
			if (++i == len) goto UNEXPECTED_END;
			c = data[i];
			if (++idx == vacount) goto TOO_FEW_ARGUMENTS;
			expr = vaargs[idx];
			type = sema_get_va_type(context, expr, variadic);
			if (!type_ok(type)) return false;
		}
		else
		{
			while (char_is_digit(c))
			{
				if (++i == len) goto UNEXPECTED_END;
				c = data[i];
			}
		}
		if (c == '.')
		{
			if (++i == len) goto UNEXPECTED_END;
			c = data[i];
			if (c == '*')
			{
				if (!type_is_integer(type))
				{
					RETURN_SEMA_ERROR(vaargs[idx], "Expected an integer for the format width.");
				}
				if (++i == len) goto UNEXPECTED_END;
				c = data[i];
				if (++idx == vacount) goto TOO_FEW_ARGUMENTS;
				expr = vaargs[idx];
				if (!type_ok(type)) return false;
			}
			else
			{
				if (!char_is_digit(c))
				{
					RETURN_SEMA_ERROR(actual_args[format_index], "Expected a format width or '*' in the format string but got another character");
				}
				while (char_is_digit(c))
				{
					if (++i == len) goto UNEXPECTED_END;
					c = data[i];
				}
			}
		}
		switch (c)
		{
			case 's':
				goto NEXT;
			case 'c':
				if (!type_is_integer(type))
				{
					RETURN_SEMA_ERROR(vaargs[idx], "Expected an integer here.");
				}
				goto NEXT;
			case 'd':
			case 'X':
			case 'x':
			case 'B':
			case 'b':
			case 'o':
			case 'a':
			case 'A':
			case 'F':
			case 'f':
			case 'e':
			case 'E':
			case 'g':
			case 'G':
				if (!type_is_number_or_bool(type) && !type_is_pointer_type(type))
				{
					if (type->type_kind == TYPE_ENUM)
					{
						RETURN_SEMA_ERROR(vaargs[idx], "An enum cannot directly be turned into a number. Use '.ordinal' to convert it to its value.", type_quoted_error_string(type));
					}
					RETURN_SEMA_ERROR(vaargs[idx], "Expected a number here, but was %s", type_quoted_error_string(type));
				}
				goto NEXT;
			case 'p':
				if (!type_is_pointer_type(type) && !type_is_integer(type))
				{
					RETURN_SEMA_ERROR(vaargs[idx], "Expected a pointer here.");
				}
				goto NEXT;
			case 'H':
			case 'h':
				if (!type_flat_is_valid_for_arg_h(type))
				{
					RETURN_SEMA_ERROR(vaargs[idx], "Expected a pointer, char array or slice here.");
				}
				goto NEXT;
			default:
				if (c > 31 && c < 127)
				{
					RETURN_SEMA_ERROR(actual_args[format_index], "Unexpected character '%c' in format declaration.", c);
				}
				RETURN_SEMA_ERROR(actual_args[format_index], "Unexpected character in format declaration.", c);
		}
NEXT:
		idx++;
	}
	if (idx < vacount)
	{
		RETURN_SEMA_FUNC_ERROR(callee->definition, call, "Too many arguments were provided for the formatting string.");
	}
	return true;
NO_MATCH_REF:
	*no_match_ref = true;
	return false;
UNEXPECTED_END:
	RETURN_SEMA_FUNC_ERROR(callee->definition, call, "Unexpected end of formatting string mid format declaration.");
TOO_FEW_ARGUMENTS:
	RETURN_SEMA_FUNC_ERROR(callee->definition, call, "Too few arguments provided for the formatting string.");
}

static inline bool sema_call_check_contract_param_match(SemaContext *context, Decl *param, Expr *expr)
{
	if (param->var.not_null && expr_is_const_pointer(expr) && !expr->const_expr.ptr)
	{
		RETURN_SEMA_ERROR(expr, "You may not pass null to the '&' parameter.");
	}
	if (expr->expr_kind == EXPR_UNARY && expr->unary_expr.expr->expr_kind == EXPR_IDENTIFIER)
	{
		if (expr->unary_expr.expr->ident_expr->var.kind == VARDECL_CONST && param->var.out_param)
		{
			SEMA_ERROR(expr, "A const parameter may not be passed into a function or macro as an 'out' or 'inout' argument.");
			return false;
		}
	}
	if (expr->expr_kind != EXPR_IDENTIFIER) return true;
	Decl *ident = expr->ident_expr;
	if (ident->decl_kind != DECL_VAR) return true;
	if (ident->var.out_param && !ident->var.in_param && param->var.in_param)
	{
		RETURN_SEMA_ERROR(expr, "An 'out' parameter may not be passed into a function or macro as an 'in' or 'inout' argument.");
	}
	if (ident->var.in_param && !ident->var.out_param && param->var.out_param)
	{
		RETURN_SEMA_ERROR(expr, "An 'in' parameter may not be passed into a function or macro as an 'out' or 'inout' argument.");
	}
	return true;
}

static inline bool sema_has_require(AstId doc_id)
{
	if (!doc_id) return false;
	Ast *docs = astptr(doc_id);
	while (docs)
	{
		switch (docs->contract_stmt.kind)
		{
			case CONTRACT_UNKNOWN:
			case CONTRACT_COMMENT:
			case CONTRACT_PURE:
			case CONTRACT_PARAM:
			case CONTRACT_OPTIONALS:
			case CONTRACT_ENSURE:
				docs = astptrzero(docs->next);
				continue;
			case CONTRACT_REQUIRE:
				return true;
		}
		UNREACHABLE
	}
	return false;
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
	if (expr->call_expr.is_dynamic_dispatch)
	{
		Expr *any_val = expr->call_expr.arguments[0];
		ASSERT(any_val->expr_kind == EXPR_PTR_ACCESS);
		*any_val = *(any_val->inner_expr);
	}
	expr->call_expr.function_contracts = 0;
	AstId docs = decl->func_decl.docs;
	if (!safe_mode_enabled() || !sema_has_require(docs)) goto SKIP_CONTRACTS;
	SemaContext temp_context;
	bool success = false;
	if (!sema_expr_setup_call_analysis(context, &callee, &temp_context,
	                                            expr, NULL, NULL, NULL, NULL,
	                                            NULL, NULL))
	{
		goto END_CONTRACT;
	}
	FOREACH_IDX(i, Decl *, param, sig->params)
	{
		if (!param || !param->name) continue;
		Expr *arg = expr->call_expr.arguments[i];
		if (!arg)
		{
			assert(i == sig->vararg_index);
			if (expr->call_expr.va_is_splat)
			{
				arg = expr->call_expr.vasplat;
			}
			else
			{
				Expr **exprs = expr->call_expr.varargs;
				Expr *init_list = expr_new_expr(EXPR_INITIALIZER_LIST, expr);
				init_list->initializer_list = exprs;
				init_list->type = param->type;
				Expr *compound_init = expr_new_expr(EXPR_COMPOUND_LITERAL, expr);
				compound_init->expr_compound_literal.initializer = init_list;
				compound_init->expr_compound_literal.type_info = type_info_new_base(param->type, param->span);
				if (!sema_analyse_expr(context, compound_init)) goto END_CONTRACT;
				arg = compound_init;
				expr->call_expr.va_is_splat = true;
			}
		}
		Decl *new_param = decl_new_generated_var(arg->type, VARDECL_LOCAL, expr->span);
		new_param->name = param->name;
		new_param->unit = context->unit;
		new_param->var.copy_const = true;
		Expr *new_arg = expr_generate_decl(new_param, arg);
		new_arg->resolve_status = RESOLVE_DONE;
		new_arg->type = arg->type;
		if (!sema_add_local(&temp_context, new_param)) goto END_CONTRACT;
		if (IS_OPTIONAL(new_param))
		{
			sema_unwrap_var(&temp_context, new_param);
		}
		if (i == sig->vararg_index)
		{
			expr->call_expr.vasplat = new_arg;
			continue;
		}
		expr->call_expr.arguments[i] = new_arg;

	}
	AstId assert_first = 0;
	AstId* next = &assert_first;

	if (!sema_analyse_contracts(&temp_context, docs, &next, expr->span, NULL)) return false;

	expr->call_expr.function_contracts = assert_first;

	success = true;
END_CONTRACT:
	sema_context_destroy(&temp_context);
	if (!success) return false;

SKIP_CONTRACTS:
	expr->call_expr.has_optional_arg = optional;

	if (!type_is_void(rtype))
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
	unsigned returns = vec_size(context->block_returns);
	if (!returns) return type_void;
	for (unsigned i = 0; i < returns; i++)
	{
		Ast *return_stmt = context->block_returns[i];
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
		Type *max = type_find_max_type(common_type, rtype); // NOLINT

		// 5. No match -> error.
		if (!max)
		{
			ASSERT(return_stmt);
			SEMA_ERROR(return_stmt, "Cannot find a common parent type of %s and %s",
					   type_quoted_error_string(rtype), type_quoted_error_string(common_type));
			Ast *prev = context->block_returns[i - 1];
			ASSERT(prev);
			SEMA_NOTE(prev, "The previous return was here.");
			return NULL;
		}

		// 6. Set the new max, mark as needing a cast on all returns.
		common_type = max;
		all_returns_need_casts = true;
	}

	ASSERT(common_type);

	// 7. Insert casts.
	if (all_returns_need_casts)
	{
		ASSERT(common_type != type_wildcard);
		FOREACH(Ast *, return_stmt, context->block_returns)
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

static inline bool sema_expr_setup_call_analysis(SemaContext *context, CalledDecl *callee,
                                                 SemaContext *macro_context, Expr *call_expr,
                                                 Type *rtype,
                                                 Ast *yield_body,
                                                 Decl **yield_params, Decl **params,
                                                 BlockExit **block_exit_ref, InliningSpan *span_ref)
{

	Decl *decl = callee->definition;
	sema_context_init(macro_context, decl->unit);
	macro_context->compilation_unit = context->unit;
	macro_context->macro_call_depth = context->macro_call_depth + 1;
	macro_context->call_env = context->call_env;
	macro_context->expected_block_type = rtype;
	if (span_ref)
	{
		*span_ref = (InliningSpan){ call_expr->span, context->inlined_at };
	}
	else
	{
		span_ref = context->inlined_at;
	}
	macro_context->inlined_at = span_ref;
	macro_context->current_macro = callee->macro ? decl : NULL;
	macro_context->yield_body = yield_body;
	macro_context->yield_params = yield_params;
	macro_context->yield_context = context;
	FOREACH(Expr *, expr, call_expr->call_expr.varargs)
	{
		if (expr->resolve_status == RESOLVE_DONE) continue;
		Expr *expr_inner = expr_copy(expr);
		expr->expr_kind = EXPR_OTHER_CONTEXT;
		expr->expr_other_context.inner = expr_inner;
		expr->expr_other_context.context = context;
	}
	macro_context->macro_varargs = callee->macro ? call_expr->call_expr.varargs : NULL;
	macro_context->original_inline_line = context->original_inline_line ? context->original_inline_line : call_expr->span.row;
	macro_context->original_module = context->original_module ? context->original_module : context->compilation_unit->module;
	macro_context->macro_params = params;

	macro_context->block_exit_ref = block_exit_ref;

	context_change_scope_with_flags(macro_context, SCOPE_MACRO);
	macro_context->block_return_defer = macro_context->active_scope.defer_last;

	return true;
}
bool sema_expr_analyse_macro_call(SemaContext *context, Expr *call_expr, Expr *struct_var, Decl *decl,
								  bool call_var_optional, bool *no_match_ref)
{
	bool is_always_const = decl->func_decl.signature.attrs.always_const;
	if (decl->resolved_attributes && decl->attrs_resolved && decl->attrs_resolved->links)
	{
		if (context->call_env.kind != CALL_ENV_FUNCTION && context->call_env.kind != CALL_ENV_FUNCTION_STATIC)
		{
			goto SKIP_LINK;
		}
		Decl *func = context->call_env.current_function;
		ASSERT_SPAN(func, func);
		ASSERT_SPAN(func, func->resolved_attributes);
		if (!func->attrs_resolved)
		{
			func->attrs_resolved = MALLOCS(ResolvedAttrData);
			*func->attrs_resolved = (ResolvedAttrData) { .overload = INVALID_SPAN };
		}
		const char **updated = func->attrs_resolved->links;
		FOREACH(const char *, link, decl->attrs_resolved->links)
		{
			vec_add(updated, link);
		}
		func->attrs_resolved->links = updated;
	}
	SKIP_LINK:;
	bool is_outer = call_expr->call_expr.is_outer_call;
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
		// Lazy arguments doesn't affect optional arg.
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
			case VARDECL_PARAM:
				// Optional typing
				break;
			default:
				UNREACHABLE
		}
		if (body_arg->var.init_expr)
		{
			RETURN_SEMA_ERROR(body_arg->var.init_expr, "Macro body parameters should never have default values.");
		}
		TypeInfo *expected_type_info = vartype(body_param);
		TypeInfo *type_info = vartype(body_arg);
		if (type_info && !sema_resolve_type_info(context, type_info, RESOLVE_TYPE_DEFAULT)) return false;
		Type *type = type_info ? type_info->type : NULL;
		if (!type && expected_type_info)
		{
			type = expected_type_info->type;
		}
		if (type && expected_type_info && type->canonical != expected_type_info->type->canonical)
		{
			if (no_match_ref) goto NO_MATCH_REF;
			RETURN_SEMA_ERROR(type_info, "This parameter should be %s but was %s",
								  type_quoted_error_string(expected_type_info->type),
								  type_quoted_error_string(type));
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
	// Create a scope, since the macro itself will not.
	context_change_scope_with_flags(context, SCOPE_NONE);
	SemaContext macro_context;

	Type *rtype = typeget(sig->rtype);
	bool optional_return = rtype && type_is_optional(rtype);
	bool may_be_optional = !rtype || optional_return;
	if (rtype) rtype = type_no_optional(rtype);

	BlockExit** block_exit_ref = CALLOCS(BlockExit*);

	InliningSpan span;
	if (!sema_expr_setup_call_analysis(context, &callee, &macro_context,
	                                   call_expr, rtype, macro_body ? macro_body->macro_body_expr.body : NULL, body_params, params, block_exit_ref,
	                                   &span))
	{
		goto EXIT_FAIL;
	}

	AstId assert_first = 0;
	AstId* next = &assert_first;

	FOREACH_IDX(idx, Decl *, param, params)
	{
		// Skip raw vararg
		if (!param) continue;
		if (!sema_add_local(&macro_context, param)) goto EXIT_FAIL;
		if (param->var.init_expr)
		{
			Type *param_type = param->type;
			if (param_type && (param->var.out_param || param->var.not_null))
			{
				param_type = type_flatten(param_type);
				if (param_type->type_kind != TYPE_POINTER && param_type->type_kind != TYPE_SLICE && param_type->type_kind != TYPE_INTERFACE && param_type->type_kind != TYPE_ANY)
				{
					SEMA_ERROR(param->var.init_expr, "Expected a pointer, slice or interface value for '%s'.", param->name);
					goto EXIT_FAIL;
				}
			}
			if (param->var.not_null)
			{
				Expr *expr = expr_variable(param);
				Expr *binary = expr_new_expr(EXPR_BINARY, expr);
				binary->binary_expr.left = exprid(expr);
				binary->binary_expr.right = exprid(expr_new_const_null(expr->span, type_voidptr));
				binary->binary_expr.operator = BINARYOP_NE;
				if (!sema_analyse_expr_rhs(context, type_bool, binary, false, NULL, false)) goto EXIT_FAIL;
				const char *string = struct_var && idx == 0 ? "Called a method on a null value." : "Passed null to a ref ('&') parameter.";
				if (expr_is_const_bool(binary) && !binary->const_expr.b)
				{
					sema_error_at(context, param->var.init_expr->span, string);
					goto EXIT_FAIL;
				}

				Ast *assert = new_ast(AST_ASSERT_STMT, param->var.init_expr->span);
				assert->assert_stmt.is_ensure = true;
				assert->assert_stmt.expr = exprid(binary);
				Expr *comment_expr = expr_new_const_string(expr->span, string);
				assert->assert_stmt.message = exprid(comment_expr);
				ast_append(&next, assert);
			}
		}
	}

	bool has_ensures = false;
	if (!sema_analyse_contracts(&macro_context, docs, &next, call_expr->span, &has_ensures)) return false;
	macro_context.macro_has_ensures = has_ensures;

	sema_append_contract_asserts(assert_first, body);
	if (!sema_analyse_statement(&macro_context, body)) goto EXIT_FAIL;
	ASSERT_SPAN(call_expr, macro_context.active_scope.depth == 1);
	bool implicit_void_return = !macro_context.active_scope.end_jump.active;
	params = macro_context.macro_params;
	bool is_no_return = sig->attrs.noreturn;

	if (!vec_size(macro_context.block_returns))
	{
		if (rtype && rtype != type_void && !macro_context.active_scope.end_jump.active)
		{
			SEMA_ERROR(decl,
					   "Missing return in macro that should evaluate to %s.",
					   type_quoted_error_string(rtype));
			goto EXIT_FAIL;
		}
	}
	else if (is_no_return)
	{
		SEMA_ERROR(macro_context.block_returns[0], "Return used despite macro being marked '@noreturn'.");
		goto EXIT_FAIL;
	}

	if (rtype)
	{
		bool inferred_len = type_len_is_inferred(rtype);
		FOREACH(Ast *, return_stmt, macro_context.block_returns)
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
	if (is_no_return && type_is_void(rtype))
	{
		call_expr->type = type_wildcard;
	}

	ASSERT_SPAN(call_expr, call_expr->type);
	bool must_use = false;
	if (rtype != type_void || optional_return)
	{
		must_use = sig->attrs.nodiscard || (optional_return && !sig->attrs.maydiscard);
	}
	unsigned returns_found = vec_size(macro_context.block_returns);
	// We may have zero normal macro returns but the active scope still has a "jump end".
	// In this case it is triggered by the @body()
	if (!returns_found && macro_context.active_scope.end_jump.active)
	{
		is_no_return = true;
	}
	if (returns_found == 1 && !implicit_void_return)
	{
		Ast *ret = macro_context.block_returns[0];
		Expr *result = ret ? ret->return_stmt.expr : NULL;
		if (!result) goto NOT_CT;
		if (!expr_is_runtime_const(result)) goto NOT_CT;
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
	if (is_outer && !type_is_void(call_expr->type))
	{
		RETURN_SEMA_ERROR(call_expr, "The macro itself returns %s here, but only 'void' is permitted "
							   "when a macro with trailing body is used directly after '=>'.",
							   type_quoted_error_string(rtype));
	}
	ASSERT_SPAN(call_expr, context->active_scope.defer_last == context->active_scope.defer_start);
	context->active_scope = old_scope;
	if (is_no_return)
	{
		SET_JUMP_END(context, call_expr);
	}
	sema_context_destroy(&macro_context);
	call_expr->resolve_status = RESOLVE_DONE;
	if (!expr_is_runtime_const(call_expr))
	{
		if (is_always_const)
		{
			SEMA_ERROR(call_expr, "The macro failed to fold to a constant value, despite being '@const'.");
			SEMA_NOTE(decl, "The macro was declared here.");
			return false;
		}
		if (call_expr->type == type_untypedlist)
		{
			SEMA_ERROR(call_expr, "The macro returns an untyped list, but the macro did not evaluate to a constant. The macro needs to explicitly cast the return value to the expected type.");
			SEMA_NOTE(decl, "The macro was declared here.");
			return false;
		}
	}
	return true;
EXIT_FAIL:
	context->active_scope = old_scope;
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
	expr_replace(call, func_expr); // NOLINT
	call->body_expansion_expr.values = args;
	call->body_expansion_expr.declarations = macro_context->yield_params;
	AstId last_defer = context->active_scope.defer_last;
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
		if (context->active_scope.end_jump.active)
		{
			macro_context->active_scope.end_jump = context->active_scope.end_jump;
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

void sema_expr_convert_enum_to_int(Expr *expr)
{
	ASSERT(type_flatten(expr->type)->type_kind == TYPE_ENUM);
	Type *underlying_type = type_base(expr->type);
	if (sema_cast_const(expr))
	{
		ASSERT(expr->const_expr.const_kind == CONST_ENUM);
		expr_rewrite_const_int(expr, underlying_type, expr->const_expr.enum_val->enum_constant.inner_ordinal);
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
	if (arg_count != 1) RETURN_SEMA_ERROR(expr, "Expected a single integer argument to 'from_ordinal'.");
	Expr *key = args[0];
	if (!sema_analyse_expr(context, key)) return false;
	if (!type_is_integer(key->type))
	{
		RETURN_SEMA_ERROR(key, "The ordinal should be an integer.");
	}
	bool is_const_enum = decl->decl_kind == DECL_CONST_ENUM;
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
		if (is_const_enum)
		{
			Expr *val = decl->enums.values[to_convert.i.low]->enum_constant.value;
			*expr = *copy_expr_single(val);
			return true;
		}
		expr->expr_kind = EXPR_CONST;
		expr->const_expr = (ExprConst) {
				.enum_val = decl->enums.values[to_convert.i.low],
				.const_kind = CONST_ENUM
		};
		expr->type = decl->type;
		return true;
	}
	if (is_const_enum)
	{
		RETURN_SEMA_ERROR(key, ".from_ordinal on const enums is only valid with compile time constant arguments, maybe you can try using regular enums?");
	}
	expr->expr_kind = EXPR_ENUM_FROM_ORD;
	expr->inner_expr = key;
	expr->type = decl->type;
	return true;
}

INLINE bool sema_expr_analyse_lookup(SemaContext *context, Expr *expr, Expr *tag, bool inline_field)
{
	Expr **args = expr->call_expr.arguments;
	unsigned arg_count = vec_size(args);
	Decl *decl = tag->type_call_expr.type;
	if (inline_field)
	{
		if (arg_count != 1) RETURN_SEMA_ERROR(expr, "Expected one (1) argument to 'lookup', did you want 'lookup_field' perhaps?");
	}
	else
	{
		if (arg_count != 2) RETURN_SEMA_ERROR(expr, "'lookup_field' requires two arguments: the name of the field and the value to search for.");
	}
	Expr *key = inline_field ? args[0] : args[1];
	if (!sema_analyse_expr(context, key)) return false;
	ArrayIndex index;
	if (inline_field)
	{
		if (!decl->is_substruct || decl->enums.inline_value)
		{
			RETURN_SEMA_ERROR(expr, "'lookup' requires an inline associated value, use 'Enum.lookup_field(fieldname, value)' instead.");
		}
	}
	else
	{
		Expr *ident = sema_expr_resolve_access_child(context, args[0], NULL);
		if (!ident) return false;
		const char *child = ident->unresolved_ident_expr.ident;
		FOREACH_IDX(i, Decl *, param, decl->enums.parameters)
		{
			if (param->name && param->name == child)
			{
				index = (ArrayIndex)i;
				goto FOUND;
			}
		}
		RETURN_SEMA_ERROR(args[0], "There is no associated value of %s with the name '%s'.", type_quoted_error_string(decl->type), child);
	}
	index = decl->enums.inline_index;
FOUND:;
	Decl *match = decl->enums.parameters[index];
	if (!cast_implicit(context, key, match->type, false)) return false;
	Decl *d = sema_find_symbol(context, kw_at_enum_lookup);
	if (!d || d->unit->module->name->module != kw_std__core__runtime)
	{
		RETURN_SEMA_ERROR(expr, "Missing main enum lookup macro '%s' in '%s'.", kw_at_enum_lookup, kw_std__core__runtime);
	}
	Expr *type = expr_new_expr(EXPR_TYPEINFO, expr);
	type->type_expr = type_info_new_base(decl->type, tag->span);
	expr->expr_kind = EXPR_CALL;
	while (vec_size(args) < 3) vec_add(args, NULL);
	args[0] = type;

	args[1] = expr_new_const_string(expr->span, match->name);
	args[2] = key;
	Expr *call = expr_new_expr(EXPR_UNRESOLVED_IDENTIFIER, expr);
	Path *new_path = CALLOCS(Path);
	new_path->module = kw_std__core__runtime;
	new_path->span = expr->span;
	new_path->len = strlen(kw_std__core__runtime);
	call->unresolved_ident_expr = (ExprUnresolvedIdentifier) { .ident = kw_at_enum_lookup, .path = new_path };
	expr->call_expr = (ExprCall) { .arguments = args, .function = exprid(call) };
	expr->resolve_status = RESOLVE_NOT_DONE;
	return sema_analyse_expr(context, expr);
}

static inline bool sema_expr_analyse_typecall(SemaContext *context, Expr *expr)
{
	Expr *tag = exprptr(expr->call_expr.function);
	expr->call_expr.arguments = sema_expand_vasplat_exprs(context, expr->call_expr.arguments);
	switch (tag->type_call_expr.property)
	{
		case TYPE_PROPERTY_FROM_ORDINAL:
			return sema_expr_analyse_from_ordinal(context, expr, tag);
		case TYPE_PROPERTY_LOOKUP:
			return sema_expr_analyse_lookup(context, expr, tag, true);
		case TYPE_PROPERTY_LOOKUP_FIELD:
			return sema_expr_analyse_lookup(context, expr, tag, false);
		default:
			break;
	}
	Expr **args = expr->call_expr.arguments;
	unsigned arg_count = vec_size(args);
	bool is_has = tag->type_call_expr.property == TYPE_PROPERTY_HAS_TAGOF;
	const char *name = is_has ? "has_tagof" : "tagof";
	if (arg_count != 1) RETURN_SEMA_ERROR(expr, "Expected a single string argument to '%s'.", name);
	Expr *key = args[0];
	if (!sema_analyse_expr(context, key)) return false;
	if (!sema_cast_const(key) || !expr_is_const_string(key))
	{
		RETURN_SEMA_ERROR(key, "The tag name should be a string constant.");
	}
	Decl *decl = tag->type_call_expr.type;
	const char *tagname = key->const_expr.bytes.ptr;
	if (!decl) goto NOT_FOUND;
	ASSERT_SPAN(expr, decl->resolved_attributes);
	ResolvedAttrData *attrs = decl->attrs_resolved;
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

INLINE bool sema_analyse_member_get_set_common(SemaContext *context, Decl *decl, Expr *inner, bool *is_bitrstruct)
{
	if (!sema_analyse_expr(context, inner)) return false;
	Type *type = type_flatten(inner->type);
	bool type_is_bitstruct = type->type_kind == TYPE_BITSTRUCT;
	if (!type_is_bitstruct && type->type_kind != TYPE_STRUCT && type->type_kind != TYPE_UNION)
	{
		RETURN_SEMA_ERROR(inner, "The member does not belong to the type %s.", type_quoted_error_string(inner->type));
	}
	Decl **members = type->decl->strukt.members;
	ArrayIndex index = -1;
	FOREACH_IDX(i, Decl *, member, members)
	{
		if (member == decl)
		{
			index = (ArrayIndex)i;
			break;
		}
	}
	if (index == -1)
	{
		RETURN_SEMA_ERROR(inner, "The member does not belong to the type %s.", type_quoted_error_string(inner->type));
	}
	*is_bitrstruct = type_is_bitstruct;
	return true;
}

static inline bool sema_call_analyse_member_set(SemaContext *context, Expr *expr)
{
	if (vec_size(expr->call_expr.arguments) != 2)
	{
		RETURN_SEMA_ERROR(expr, "Expected two arguments to '.set'.");
	}
	if (!sema_call_may_not_have_attributes(context, expr)) return false;
	Expr *get = exprptr(expr->call_expr.function);
	Decl *decl = get->member_get_expr;
	Expr *inner = expr->call_expr.arguments[0];
	bool is_bitstruct;
	if (!sema_analyse_member_get_set_common(context, decl, inner, &is_bitstruct)) return false;
	Expr *arg = expr->call_expr.arguments[1];
	Expr *access = expr_new_expr(is_bitstruct ? EXPR_BITACCESS : EXPR_ACCESS_RESOLVED, expr);
	access->access_resolved_expr = (ExprResolvedAccess) { .parent = inner, .ref = decl };
	access->type = decl->type;
	access->resolve_status = RESOLVE_DONE;
	expr->expr_kind = EXPR_BINARY;
	expr->binary_expr = (ExprBinary) { .left =  exprid(access), .right = exprid(arg), .operator = BINARYOP_ASSIGN };
	return sema_expr_analyse_binary(context, NULL, expr, NULL);
}

static inline bool sema_call_analyse_member_get(SemaContext *context, Expr *expr)
{
	if (vec_size(expr->call_expr.arguments) != 1)
	{
		RETURN_SEMA_ERROR(expr, "Expected a single argument to '.get'.");
	}
	if (!sema_call_may_not_have_attributes(context, expr)) return false;
	Expr *get = exprptr(expr->call_expr.function);
	Decl *decl = get->member_get_expr;
	Expr *inner = expr->call_expr.arguments[0];
	bool is_bitstruct;
	if (!sema_analyse_member_get_set_common(context, decl, inner, &is_bitstruct)) return false;
	expr->expr_kind = is_bitstruct ? EXPR_BITACCESS : EXPR_ACCESS_RESOLVED;
	expr->access_resolved_expr = (ExprResolvedAccess) { .parent = inner, .ref = decl };
	expr->type = decl->type;
	return true;
}
static inline bool sema_expr_analyse_call(SemaContext *context, Expr *expr, bool *no_match_ref)
{
	if (no_match_ref) *no_match_ref = true;
	Expr *func_expr = exprptr(expr->call_expr.function);
	if (!sema_analyse_expr_value(context, func_expr)) return false;
	bool optional = func_expr->type && IS_OPTIONAL(func_expr);
	Decl *decl;
	Expr *struct_var = NULL;
	switch (func_expr->expr_kind)
	{
		case EXPR_MACRO_BODY_EXPANSION:
			return sema_call_analyse_body_expansion(context, expr);
		case EXPR_MEMBER_GET:
			return sema_call_analyse_member_get(context, expr);
		case EXPR_MEMBER_SET:
			return sema_call_analyse_member_set(context, expr);
		case EXPR_TYPECALL:
			return sema_expr_analyse_typecall(context, expr);
		case EXPR_BUILTIN:
			return sema_expr_analyse_builtin_call(context, expr);
		case EXPR_IDENTIFIER:
			decl = func_expr->ident_expr;
			if (!sema_analyse_decl(context, decl)) return false;
			break;
		case EXPR_ACCESS_RESOLVED:
			decl = func_expr->access_resolved_expr.ref;
			if (!sema_analyse_decl(context, decl)) return false;
			switch (decl->decl_kind)
			{
				case DECL_MACRO:
					struct_var = func_expr->access_resolved_expr.parent;
					if (decl->func_decl.signature.params[0]->type->canonical != struct_var->type->canonical
						&& decl->func_decl.signature.params[0]->type->type_kind == TYPE_POINTER)
					{
						expr_insert_addr(struct_var);
					}
					break;
				case DECL_FUNC:
					struct_var = func_expr->access_resolved_expr.parent;
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
	if (const_list->const_expr.const_kind == CONST_SLICE)
	{
		ASSERT_SPAN(const_list, const_list->const_expr.slice_init);
		return expr_rewrite_to_const_initializer_index(const_list->type, const_list->const_expr.slice_init, result, index, from_back);
	}
	ASSERT_SPAN(const_list, const_list->const_expr.const_kind == CONST_INITIALIZER);
	return expr_rewrite_to_const_initializer_index(const_list->type, const_list->const_expr.initializer, result, index, from_back);
}

/**
 * Find subscript type or overload for subscript.
 */
static Expr *sema_expr_find_subscript_type_or_overload_for_subscript(SemaContext *context, Expr *current_expr,
                                                                     OperatorOverload overload_type,
																	 Type **subscript_type_ptr,
                                                                     Decl **overload_ptr)
{
	Decl *overload = NULL;
	overload = sema_find_untyped_operator(context, current_expr->type, overload_type, NULL);
	if (overload)
	{
		// Overload for []=
		if (overload_type == OVERLOAD_ELEMENT_SET)
		{
			*overload_ptr = overload;
			ASSERT(vec_size(overload->func_decl.signature.params) == 3);
			*subscript_type_ptr = overload->func_decl.signature.params[2]->type;
			return current_expr;
		}
		// Overload found for [] and &[]
		*overload_ptr = overload;
		ASSERT(overload->func_decl.signature.rtype);
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
		return sema_expr_find_subscript_type_or_overload_for_subscript(context, embedded_struct, overload_type,
		                                                               subscript_type_ptr,
		                                                               overload_ptr);
	}
	return NULL;
}

static inline bool sema_expr_resolve_subscript_index(SemaContext *context, Expr *expr, Expr *subscripted, Expr *index, Type **current_type_ref, Expr **current_expr_ref, Type **subscript_type_ref, Decl **overload_ref, int64_t *index_ref, bool is_ref, OperatorOverload overload_type, bool check_valid)
{
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
		current_expr = sema_expr_find_subscript_type_or_overload_for_subscript(context,
																			   subscripted,
																			   overload_type,
		                                                                       &subscript_type,
		                                                                       &overload);
		if (!overload && !subscript_type && is_ref)
		{
			// Maybe there is a [] overload?
			if (sema_expr_find_subscript_type_or_overload_for_subscript(context, subscripted, overload_type, &subscript_type,
			                                                            &overload))
			{
				if (check_valid) return false;
				RETURN_SEMA_ERROR(expr, "A function or macro with '@operator(&[])' is not defined for %s, "
				                        "so you need && to take the address of the temporary.",
				                  type_quoted_error_string(subscripted->type));
			}
		}
		if (!subscript_type)
		{
			if (check_valid) return false;
			RETURN_SEMA_ERROR(expr, "Indexing a value of type %s is not possible.", type_quoted_error_string(subscripted->type));
		}
		if (!overload) current_type = type_flatten(current_expr->type);
	}
	ASSERT(current_type == current_type->canonical);
	Type *index_type = NULL;
	if (overload)
	{
		index_type = overload->func_decl.signature.params[1]->type;
		if (!sema_analyse_inferred_expr(context, index_type, index))
		{
			expr_poison(index);
			return false;
		}
	}
	else
	{
		if (!sema_analyse_expr_value(context, index))
		{
			expr_poison(index);
			return false;
		}
	}

	// Analyse the index.

	int64_t index_value = -1;
	bool start_from_end = expr->subscript_expr.index.start_from_end;
	if (start_from_end && (current_type->type_kind == TYPE_POINTER || current_type->type_kind == TYPE_FLEXIBLE_ARRAY))
	{
		if (check_valid) return false;
		RETURN_SEMA_ERROR(index, "Indexing from the end is not allowed for pointers "
		                         "and flexible array members.");
	}
	ArrayIndex size;
	bool check_len = !context->call_env.in_no_eval || current_type == type_untypedlist;
	Expr *len_expr = current_expr->expr_kind == EXPR_CT_IDENT ? current_expr->ct_ident_expr.decl->var.init_expr : current_expr;
	if (check_len && expr_is_const_int(index) && (size = sema_len_from_expr(len_expr)) >= 0)
	{
		// 4c. And that it's in range.
		if (int_is_neg(index->const_expr.ixx))
		{
			if (check_valid) return false;
			RETURN_SEMA_ERROR(index, "The index may not be negative.");
		}
		if (!int_fits(index->const_expr.ixx, TYPE_I64) || size == 0)
		{
			if (check_valid) return false;
			RETURN_SEMA_ERROR(index, "The index is out of range.", size);
		}
		index_value = int_to_i64(index->const_expr.ixx);
		if (start_from_end)
		{
			index_value = size - index_value;
		}
		if (index_value < 0 || index_value >= size)
		{
			if (check_valid) return false;
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
	*index_ref = index_value;
	*current_type_ref = current_type;
	*current_expr_ref = current_expr;
	*overload_ref = overload;
	*subscript_type_ref = subscript_type;
	return true;
}

static inline bool sema_expr_analyse_subscript_lvalue(SemaContext *context, Expr *expr, bool check_valid)
{
	// Evaluate the expression to index.
	Expr *subscripted = exprptr(expr->subscript_expr.expr);
	if (subscripted->expr_kind == EXPR_CT_IDENT)
	{
		if (!sema_analyse_expr_lvalue(context, subscripted, NULL)) return false;
	}
	else
	{
		if (!sema_analyse_expr(context, subscripted)) return false;
	}

	if (!sema_expr_check_assign(context, expr, NULL)) return false;

	Expr *index = exprptr(expr->subscript_expr.index.expr);

	// 3. Check failability due to value.
	bool optional = IS_OPTIONAL(subscripted);

	Type *current_type;
	Expr *current_expr;
	Decl *overload;
	Type *subscript_type;
	int64_t index_value;
	if (!sema_expr_resolve_subscript_index(context, expr, subscripted, index, &current_type, &current_expr, &subscript_type, &overload, &index_value, false, OVERLOAD_ELEMENT_SET, check_valid))
	{
		if (check_valid && expr_ok(index))
		{
			expr_poison(expr);
			return true;
		}
		return false;
	}

	// 4. If we are indexing into a complist
	if (current_expr->expr_kind == EXPR_CT_IDENT)
	{
		if (index_value == -1)
		{
			if (check_valid) goto VALID_FAIL_POISON;
			RETURN_SEMA_ERROR(index, "Assigning to a compile time constant requires a constant index.");
		}
		expr->expr_kind = EXPR_CT_SUBSCRIPT;
		expr->ct_subscript_expr = (ExprCtSubscript) { .var = current_expr->ct_ident_expr.decl, .index = (ArrayIndex)index_value };
		expr->type = NULL;
		return true;
	}

	if (!sema_cast_rvalue(context, subscripted, true)) return false;

	bool start_from_end = expr->subscript_expr.index.start_from_end;
	if (overload)
	{
		if (start_from_end)
		{
			Decl *len = sema_find_untyped_operator(context, current_expr->type, OVERLOAD_LEN, NULL);
			if (!len)
			{
				if (check_valid) goto VALID_FAIL_POISON;
				RETURN_SEMA_ERROR(subscripted, "Cannot index '%s' from the end, since there is no 'len' overload.", type_to_error_string(subscripted->type));
			}
			if (!sema_analyse_expr(context, current_expr)) return false;
			Decl *temp = decl_new_generated_var(current_expr->type, VARDECL_PARAM, current_expr->span);
			Expr *decl = expr_generate_decl(temp, expr_copy(current_expr));
			expr_rewrite_two(current_expr, decl, expr_variable(temp));
			if (!sema_analyse_expr(context, current_expr)) return false;
			Expr *var_for_len = expr_variable(temp);
			Expr *len_expr = expr_new(EXPR_CALL, expr->span);
			if (!sema_insert_method_call(context, len_expr, len, var_for_len, NULL, false)) return false;
			if (!sema_analyse_expr(context, len_expr)) return false;
			Expr *index_copy = expr_copy(index);
			if (!sema_analyse_expr(context, index_copy)) return false;
			if (!cast_explicit(context, index_copy, len_expr->type)) return false;
			expr_rewrite_to_binary(index, len_expr, index_copy, BINARYOP_SUB);
			index->resolve_status = RESOLVE_NOT_DONE;
			if (!sema_analyse_expr(context, index)) return false;
		}
		expr->expr_kind = EXPR_SUBSCRIPT_ASSIGN;
		expr->type = subscript_type;
		expr->subscript_assign_expr.expr = exprid(current_expr);
		expr->subscript_assign_expr.index = exprid(index);
		expr->subscript_assign_expr.method = declid(overload);
		return true;
	}

	// Cast to an appropriate type for index.
	if (!cast_to_index_len(context, index, false)) return false;

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
		expr->subscript_expr.index.start_from_end = false;
	}

	expr->subscript_expr.expr = exprid(current_expr);
	expr->type = type_add_optional(subscript_type, optional);
	return true;
VALID_FAIL_POISON:
	expr_poison(expr);
	return true;
}

static inline bool sema_expr_analyse_subscript(SemaContext *context, Expr *expr, CheckType check, bool check_valid)
{
	ASSERT(expr->expr_kind == EXPR_SUBSCRIPT || expr->expr_kind == EXPR_SUBSCRIPT_ADDR);
	bool is_eval_ref = expr->expr_kind == EXPR_SUBSCRIPT_ADDR;

	// Evaluate the expression to index.
	Expr *subscripted = exprptr(expr->subscript_expr.expr);
	if (!sema_analyse_expr_check(context, subscripted, CHECK_VALUE)) return false;

	// 3. Check failability due to value.
	bool optional = IS_OPTIONAL(subscripted);

	// 2. Evaluate the index.
	Expr *index = exprptr(expr->subscript_expr.index.expr);

	Decl *overload = NULL;
	Type *subscript_type = NULL;
	Expr *current_expr;
	Type *current_type = subscripted->type->canonical;
	int64_t index_value;
	OperatorOverload overload_type = check == CHECK_VALUE ? OVERLOAD_ELEMENT_AT : OVERLOAD_ELEMENT_REF;

	if (!sema_expr_resolve_subscript_index(context, expr, subscripted, index, &current_type, &current_expr, &subscript_type, &overload, &index_value, is_eval_ref, overload_type, check_valid))
	{
		if (check_valid && expr_ok(index))
		{
			expr_poison(expr);
			return true;
		}
		return false;
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
		expr_replace(expr, current_expr->const_expr.untyped_list[index_value]);
		return true;
	}
	if (!sema_cast_rvalue(context, subscripted, true)) return false;


	bool start_from_end = expr->subscript_expr.index.start_from_end;
	if (overload)
	{
		if (start_from_end)
		{
			Decl *len = sema_find_untyped_operator(context, current_expr->type, OVERLOAD_LEN, NULL);
			if (!len)
			{
				if (check_valid) goto VALID_FAIL_POISON;
				RETURN_SEMA_ERROR(subscripted, "Cannot index '%s' from the end, since there is no 'len' overload.", type_to_error_string(subscripted->type));
			}
			if (!sema_analyse_expr(context, current_expr)) return false;
			Decl *temp = decl_new_generated_var(current_expr->type, VARDECL_PARAM, current_expr->span);
			Expr *decl = expr_generate_decl(temp, expr_copy(current_expr));
			expr_rewrite_two(current_expr, decl, expr_variable(temp));
			if (!sema_analyse_expr(context, current_expr)) return false;
			Expr *var_for_len = expr_variable(temp);
			Expr *len_expr = expr_new(EXPR_CALL, expr->span);
			if (!sema_insert_method_call(context, len_expr, len, var_for_len, NULL, false)) return false;
			if (!sema_analyse_expr(context, len_expr)) return false;
			Expr *index_copy = expr_copy(index);
			if (!sema_analyse_expr(context, index_copy)) return false;
			if (!cast_explicit(context, index_copy, len_expr->type)) return false;
			expr_rewrite_to_binary(index, len_expr, index_copy, BINARYOP_SUB);
			index->resolve_status = RESOLVE_NOT_DONE;
			if (!sema_analyse_expr(context, index)) return false;
		}
		Expr **args = NULL;
		vec_add(args, index);
		return sema_insert_method_call(context, expr, overload, current_expr, args, false);
	}

	// Cast to an appropriate type for index.
	if (!cast_to_index_len(context, index, false)) return false;

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
			bool is_const_initializer = expr_is_const_initializer(current_expr) || (expr_is_const_slice(current_expr) && current_expr->const_expr.slice_init);
			if (is_const_initializer || expr_is_const_string(current_expr) || expr_is_const_bytes(current_expr))
			{
				if (!int_fits(index->const_expr.ixx, TYPE_U32))
				{
					if (check_valid) goto VALID_FAIL_POISON;
					RETURN_SEMA_ERROR(index, "Index is out of range.");
				}
				ArraySize idx = index->const_expr.ixx.i.low;
				ArrayIndex len = sema_len_from_const(current_expr);
				if (idx > len || (idx == len && !start_from_end) || (idx == 0 && start_from_end))
				{
					if (check_valid) goto VALID_FAIL_POISON;
					RETURN_SEMA_ERROR(index, "The index (%s%llu) is out of range, the length is just %llu.",
									  start_from_end ? "^" : "",
									  (unsigned long long)idx,
									  (unsigned long long)len);
				}
				if (!is_const_initializer)
				{
					// Handle bytes / String
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

	if (!cast_implicit_binary(context, offset, vec_len ? type_get_vector(type_isz, vec_len) : type_isz, NULL)) return false;

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

INLINE bool sema_expr_analyse_range_internal(SemaContext *context, Range *range, ArrayIndex len, RangeEnv env)
{
	Expr *start = exprptr(range->start);
	ASSERT(start);
	Expr *end = exprptrzero(range->end);

	if (!sema_analyse_expr(context, start)) return false;
	if (end && !sema_analyse_expr(context, end)) return false;

	if (!cast_to_index_len(context, start, false)) return false;
	if (end && !cast_to_index_len(context, end, false)) return false;
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
			RETURN_SEMA_ERROR_AT(span, "No common type can be found between start and end index.");
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
	if (end && sema_cast_const(end))
	{
		// Only ArrayIndex sized
		if (!expr_is_valid_index(end))
		{
			RETURN_SEMA_ERROR(end, "The index cannot be stored in a 64-signed integer, which isn't supported.");
		}

		ArrayIndex end_index = int_to_i64(end->const_expr.ixx);

		if (range->end_from_end)
		{
			if (end_index < 0) RETURN_SEMA_ERROR(end, "Negative numbers are not allowed when indexing from the end.");
			// Something like  1 .. ^4 with an unknown length.
			if (len < 0) return true;
			// Otherwise we fold the "from end"
			if (end_index > len)
			{
				RETURN_SEMA_ERROR(end, "An index may only be negative for pointers (it was: %lld).", len - end_index);
			}
			end_index = len - end_index;
			range->end_from_end = false;
		}
		if (end_index < 0 && env != RANGE_PTR)
		{
			RETURN_SEMA_ERROR(end, "An index may only be negative for pointers (it was: %lld).", end_index);
		}
		// No more analysis
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
		if (!expr_is_valid_index(start))
		{
			RETURN_SEMA_ERROR(end, "The index cannot be stored in a 64-signed integer, which isn't supported.");
		}
		// Only ArrayIndex sized
		ArrayIndex start_index = int_to_i64(start->const_expr.ixx);
		if (range->start_from_end)
		{
			if (start_index < 0) RETURN_SEMA_ERROR(end, "Negative numbers are not allowed when indexing from the end.");
			// Something like  ^1 .. 4 with an unknown length.
			if (len < 0) return true;
			// Otherwise we fold the "from end"
			if (len < start_index)
			{
				RETURN_SEMA_ERROR(start, "An index may only be negative for pointers (it was: %lld).", len - start_index);
			}
			start_index = len - start_index;
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
			ArrayIndex end_index = range->const_end;
			if (end_index < start_index) RETURN_SEMA_ERROR(start, "The start index (%lld) should not be greater than the end index (%lld).",
														   start_index, end_index);
			range->const_end = end_index + 1 - start_index;
			range->range_type = RANGE_CONST_LEN;
			range->is_len = true;
		}
		if (range->range_type == RANGE_CONST_LEN)
		{
			ArrayIndex end_index = range->const_end;
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
				if (range->start_index + range->len_index > len)
				{
					RETURN_SEMA_ERROR(end ? end : start, "End index out of bounds, was %d, exceeding max index %d.", range->start_index + range->len_index - 1, len - 1);
				}
				break;
			default:
				break;
		}
	}
	return true;
}


static inline bool sema_expr_analyse_range(SemaContext *context, Range *range, ArrayIndex len, RangeEnv env)
{
	switch (range->status)
	{
		case RESOLVE_DONE:
			return true;
		case RESOLVE_NOT_DONE:
			range->status = RESOLVE_RUNNING;
			if (!sema_expr_analyse_range_internal(context, range, len, env))
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

static inline void sema_slice_initializer(Expr *expr, Expr *subscripted, Range *range)
{
	ConstInitializer *initializer = subscripted->const_expr.initializer;
	ASSERT(type_is_arraylike(initializer->type));
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
	if (!sema_expr_analyse_range(context, range, length, env)) return false;
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
					ASSERT(index);
					original_type = type_get_slice(index);
				}
				subscripted->type = original_type;
				expr_replace(expr, subscripted);
				return true;
			}
			case CONST_UNTYPED_LIST:
				ASSERT(!type_is_arraylike(subscripted->type));
				vec_erase_front(subscripted->const_expr.untyped_list, range->start_index);
				vec_resize(subscripted->const_expr.untyped_list, range->len_index);
				expr_replace(expr, subscripted);
				return true;
			case CONST_INITIALIZER:
				sema_slice_initializer(expr, subscripted, range);
				return true;
			case CONST_SLICE:
				if (!subscripted->const_expr.slice_init)
				{
					ASSERT(range->len_index == 0);
					expr_replace(expr, subscripted);
					return true;
				}
				sema_slice_initializer(expr, subscripted, range);
				return true;
			case CONST_POINTER:
			case CONST_FLOAT:
			case CONST_INTEGER:
			case CONST_BOOL:
			case CONST_ENUM:
			case CONST_FAULT:
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
 * 5. .$eval(...) -> resolve the eval and retry.
 * 6. .$ident -> It is a child to resolve as CT param
 * 7. .$Type -> It is a child to resolve as CT type param
 */
 Expr *sema_expr_resolve_access_child(SemaContext *context, Expr *child, bool *missing)
{
	 SourceSpan span = child->span;
	 bool in_hash = false;
RETRY:
	switch (child->expr_kind)
	{
		case EXPR_HASH_IDENT:
			SEMA_DEPRECATED(child, "Using 'abc.#foo' access style is deprecated. Use 'abc.eval($foo)' instead.");
			if (!sema_expr_fold_hash(context, child)) return NULL;
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
			goto ALREADY_RESOLVED;
		case EXPR_UNRESOLVED_IDENTIFIER:
			// A path is not allowed.
			if (child->unresolved_ident_expr.path) break;
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
			// Only report missing if missing var is NULL
			Expr *result = sema_ct_eval_expr(context, false, child->inner_expr, missing == NULL);
			if (!expr_ok(result)) return NULL;
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
	ConstKind const_kind = CONST_ENUM;
	for (ArraySize i = 0; i < elements; i++)
	{
		Decl *decl = values[i];
		Expr *expr = expr_new(EXPR_CONST, span);
		expr->const_expr.const_kind = const_kind;
		ASSERT_SPAN(enum_array_expr, enum_decl->resolve_status == RESOLVE_DONE);
		expr->const_expr.enum_val = decl;
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

static inline bool sema_expr_replace_with_const_enum_array(SemaContext *context, Expr *enum_array_expr, Decl *enum_decl)
{
	if (!sema_analyse_decl(context, enum_decl)) return false;
	Decl **values = enum_decl->enums.values;
	SourceSpan span = enum_array_expr->span;
	Expr *initializer = expr_new(EXPR_INITIALIZER_LIST, span);
	ArraySize elements = vec_size(values);
	Expr **element_values = elements > 0 ? VECNEW(Expr*, elements) : NULL;
	Type *kind = enum_decl->type;
	for (ArraySize i = 0; i < elements; i++)
	{
		Decl *decl = values[i];
		Expr *expr = copy_expr_single(decl->enum_constant.value);
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
	ASSERT(parent_type == parent_type->canonical);
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
	ASSERT_SPAN(expr, identifier->expr_kind == EXPR_UNRESOLVED_IDENTIFIER);
	Type *canonical = parent_type->canonical;
	const char *name = identifier->unresolved_ident_expr.ident;
	bool is_const = identifier->unresolved_ident_expr.is_const;

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
		expr_resolve_ident(expr, member);
		return true;
	}
	Decl *decl = canonical->decl;
	if (!decl_ok(decl)) return false;
	switch (decl->decl_kind)
	{
		case DECL_ENUM:
		case DECL_CONST_ENUM:
			if (is_const)
			{
				if (!sema_expr_analyse_enum_constant(context, expr, name, decl))
				{
					if (missing_ref) goto MISSING_REF;
					if (!decl_ok(decl)) return false;
					RETURN_SEMA_ERROR(expr, "'%s' has no enumeration value '%s'.", decl->name, name);
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
	if (!member->unit)
	{
		if (!sema_analyse_decl(context, decl)) return false;
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
	expr_resolve_ident(expr, member);
	return true;
MISSING_REF:
	*missing_ref = true;
	return false;
}

static inline bool sema_expr_analyse_member_access(SemaContext *context, Expr *expr, Expr *parent, Expr *identifier, bool *missing_ref)
{
	ASSERT_SPAN(expr, identifier->expr_kind == EXPR_UNRESOLVED_IDENTIFIER);

	Decl *decl = parent->const_expr.member.decl;
	const char *name = identifier->unresolved_ident_expr.ident;
	bool is_const = identifier->unresolved_ident_expr.is_const;

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
		case TYPE_PROPERTY_LOOKUP:
		case TYPE_PROPERTY_LOOKUP_FIELD:
			expr->expr_kind = EXPR_TYPECALL;
			expr->type_call_expr = (ExprTypeCall) { .type = decl, .property = type_property };
			return true;
		case TYPE_PROPERTY_SET:
			expr->expr_kind = EXPR_MEMBER_SET;
			expr->member_get_expr = decl;
			expr->type = type_void;
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
			sema_create_const_membersof(expr, decl->type->canonical, parent->const_expr.member.align, parent->const_expr.member.offset);
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

	ASSERT_SPAN(expr, member->unit);

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
		.enum_val = values[val]
	};
	return true;
}

static inline bool sema_create_const_len(Expr *expr, Type *type, Type *flat)
{
	ASSERT_SPAN(expr, flat == type_flatten(flat) && "Should be flattened already.");

	size_t len;
	if (type->type_kind == TYPE_CONST_ENUM)
	{
		len = vec_size(type->decl->enums.values);
		expr_rewrite_const_int(expr, type_usz, len);
		return true;
	}
	switch (flat->type_kind)
	{
		case TYPE_ARRAY:
		case TYPE_VECTOR:
			len = flat->array.len;
			break;
		case TYPE_ENUM:
			len = vec_size(flat->decl->enums.values);
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
		case TYPE_CONST_ENUM:
		case TYPE_ENUM:
			inner = enum_inner_type(type)->canonical;
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

static inline bool sema_create_const_min(Expr *expr, Type *type, Type *flat)
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
				UNREACHABLE
		}
		return true;
	}
	if (type_is_integer(flat))
	{
		expr->expr_kind = EXPR_CONST;
		expr->const_expr.const_kind = CONST_INTEGER;
		expr->const_expr.is_character = false;
		expr->const_expr.is_hex = false;
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
				expr->const_expr.is_hex = true;
				break;
			case TYPE_I128:
				expr->const_expr.ixx.i = (Int128){ 1ULL << 63, 0 };
				expr->const_expr.is_hex = true;
				break;
			default:
				expr->const_expr.ixx.i = (Int128){ 0, 0 };
				break;
		}
		return true;
	}
	UNREACHABLE
}

static inline bool sema_create_const_params(Expr *expr, Type *type)
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

static inline bool sema_create_const_paramsof(Expr *expr, Type *type)
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

static inline void sema_create_const_membersof(Expr *expr, Type *type, AlignSize alignment, AlignSize offset)
{
	Decl **members = NULL;
	assert(type->canonical == type);
	switch (type->type_kind)
	{
		case TYPE_UNION:
		case TYPE_STRUCT:
		case TYPE_BITSTRUCT:
			members = type->decl->strukt.members;
			break;
		default:
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

static inline Expr *create_method_copy(Decl *method, SourceSpan span)
{
	size_t namestr_len = strlen(method->name);
	const char *namestr = str_copy(method->name, namestr_len);
	Expr *expr_element = expr_new(EXPR_CONST, span);
	expr_element->resolve_status = RESOLVE_DONE;
	expr_element->type = type_string;
	expr_element->const_expr = (ExprConst) {
		.const_kind = CONST_STRING,
		.bytes.ptr = namestr,
		.bytes.len = namestr_len,
	};
	return expr_element;
}

static inline void append_to_method_list(Decl **methods, Expr ***method_exprs_ref, SourceSpan span)
{
	FOREACH(Decl *, method, methods)
	{
		if (method->decl_kind == DECL_FUNC)
		{
			vec_add(*method_exprs_ref, create_method_copy(method, span));
		}
	}
}

static inline void sema_append_interface_methods(Decl *interface, Expr ***method_exprs_ref, SourceSpan span)
{
	append_to_method_list(interface->interface_methods, method_exprs_ref, span);
	FOREACH(TypeInfo *, type_info, interface->interfaces)
	{
		sema_append_interface_methods(type_info->type->decl, method_exprs_ref, span);
	}
}
static inline void append_extension_methods(Type *type, Decl **extensions, Expr ***method_exprs_ref, SourceSpan span)
{
	FOREACH(Decl *, method, extensions)
	{
		if (method->decl_kind == DECL_FUNC && typeget(method->func_decl.type_parent) == type)
		{
			vec_add(*method_exprs_ref, create_method_copy(method, span));
		}
	}
}

static inline void sema_create_const_methodsof(SemaContext *context, Expr *expr, Type *type)
{
	Expr **method_exprs = NULL;
CONTINUE:
	if (type_is_user_defined(type))
	{
		Decl *decl = type->decl;
		// Interface, prefer interface methods.
		if (decl->decl_kind == DECL_INTERFACE)
		{
			sema_append_interface_methods(decl, &method_exprs, expr->span);
		}
		// Look through natively defined methods.
		append_to_method_list(decl->methods, &method_exprs, expr->span);
	}
	append_extension_methods(type, context->unit->local_method_extensions, &method_exprs, expr->span);
	append_extension_methods(type, context->unit->module->private_method_extensions, &method_exprs, expr->span);
	append_extension_methods(type, compiler.context.method_extensions, &method_exprs, expr->span);

	type = type_find_parent_type(type);
	if (type) goto CONTINUE;
	expr_rewrite_const_untyped_list(expr, method_exprs);
}


static inline bool sema_create_const_max(Expr *expr, Type *type, Type *flat)
{
	if (type_is_integer(flat))
	{
		expr->expr_kind = EXPR_CONST;
		expr->const_expr.const_kind = CONST_INTEGER;
		expr->const_expr.is_character = false;
		expr->type = type;
		expr->const_expr.is_hex = false;
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
				expr->const_expr.is_hex = true;
				break;
			case TYPE_I128:
				expr->const_expr.ixx.i = (Int128){ 0x7FFFFFFFFFFFFFFFLL, 0xFFFFFFFFFFFFFFFFLL };
				expr->const_expr.is_hex = true;
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
				expr->const_expr.is_hex = true;
				break;
			case TYPE_U128:
				expr->const_expr.ixx.i = (Int128){ 0xFFFFFFFFFFFFFFFFLL, 0xFFFFFFFFFFFFFFFFLL };
				expr->const_expr.is_hex = true;
				break;
			default:
				UNREACHABLE
		}
		return true;
	}
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
				UNREACHABLE
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
		case TYPE_PROPERTY_SET:
		case TYPE_PROPERTY_HAS_TAGOF:
		case TYPE_PROPERTY_INF:
		case TYPE_PROPERTY_IS_EQ:
		case TYPE_PROPERTY_IS_ORDERED:
		case TYPE_PROPERTY_IS_SUBSTRUCT:
		case TYPE_PROPERTY_LOOKUP:
		case TYPE_PROPERTY_LOOKUP_FIELD:
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
	Int128 i = exprptr(index_expr.expr)->const_expr.ixx.i;
	if (i.high || i.low > MAX_ARRAYINDEX) return false;
	ArrayIndex index = (ArrayIndex)i.low;
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
			UNREACHABLE
	}
	UNREACHABLE
EVAL:
	switch (result->kind)
	{
		case CONST_INIT_ZERO:
			if (member->type->type_kind == TYPE_FLEXIBLE_ARRAY) return false;
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


static bool sema_type_property_is_valid_for_type(CanonicalType *original_type, TypeProperty property)
{
	switch (original_type->type_kind)
	{
		case CT_TYPES:
			return false;
		default:
			break;
	}
	CanonicalType *type = type_flatten(original_type);
	switch (property)
	{
		case TYPE_PROPERTY_NONE:
			return false;
		case TYPE_PROPERTY_GET:
		case TYPE_PROPERTY_SET:
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
				case TYPE_CONST_ENUM:
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
			if (original_type->type_kind == TYPE_CONST_ENUM) return true;
			switch (type->type_kind)
			{
				case TYPE_ARRAY:
				case TYPE_VECTOR:
				case TYPE_ENUM:
					return true;
				default:
					return false;
			}
		case TYPE_PROPERTY_LOOKUP:
		case TYPE_PROPERTY_LOOKUP_FIELD:
			return type->type_kind == TYPE_ENUM;
		case TYPE_PROPERTY_MIN:
		case TYPE_PROPERTY_MAX:
			return type_is_float(type) || type_is_integer(type);
		case TYPE_PROPERTY_FROM_ORDINAL:
		case TYPE_PROPERTY_NAMES:
		case TYPE_PROPERTY_VALUES:
			return type->type_kind == TYPE_ENUM || original_type->canonical->type_kind == TYPE_CONST_ENUM;
		case TYPE_PROPERTY_ELEMENTS:
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
			return true;
		case TYPE_PROPERTY_EXTNAMEOF:
			return !type_is_builtin(original_type->canonical->type_kind);
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
			return sema_create_const_len(expr, type, flat);
		case TYPE_PROPERTY_MIN:
			return sema_create_const_min(expr, type, flat);
		case TYPE_PROPERTY_MAX:
			return sema_create_const_max(expr, type, flat);
		case TYPE_PROPERTY_NAMES:
			if (type->type_kind == TYPE_CONST_ENUM)
			{
				return sema_expr_replace_with_enum_name_array(context, expr, type->decl);
			}
			ASSERT_SPAN(expr, flat->type_kind == TYPE_ENUM);
			return sema_expr_replace_with_enum_name_array(context, expr, flat->decl);
		case TYPE_PROPERTY_ASSOCIATED:
			return sema_create_const_associated(context, expr, flat);
		case TYPE_PROPERTY_ELEMENTS:
			ASSERT_SPAN(expr, flat->type_kind == TYPE_ENUM);
			if (!sema_analyse_decl(context, type->decl)) return false;
			SEMA_DEPRECATED(expr, ".elements is deprecated. Use .values.len instead.");
			expr_rewrite_const_int(expr, type_isz, vec_size(flat->decl->enums.values));
			return true;
		case TYPE_PROPERTY_VALUES:
			if (type->type_kind == TYPE_CONST_ENUM)
			{
				return sema_expr_replace_with_const_enum_array(context, expr, type->decl);
			}
			ASSERT_SPAN(expr, flat->type_kind == TYPE_ENUM);
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
		case TYPE_PROPERTY_SET:
			UNREACHABLE
		case TYPE_PROPERTY_MEMBERSOF:
		{
			AlignSize align;
			if (!sema_set_abi_alignment(context, parent_type, &align)) return false;
			sema_create_const_membersof(expr, flat, align, 0);
			return true;
		}
		case TYPE_PROPERTY_METHODSOF:
			sema_create_const_methodsof(context, expr, type);
			return true;
		case TYPE_PROPERTY_PARAMSOF:
			return sema_create_const_paramsof(expr, flat);
		case TYPE_PROPERTY_PARAMS:
			SEMA_DEPRECATED(expr, "'params' is deprecated, use 'paramsof' instead.");
			return sema_create_const_params(expr, flat);
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
			if (!type_is_user_defined(type))
			{
				RETURN_SEMA_ERROR(expr, "'tagof' is not defined for builtin types like %s.", type_quoted_error_string(type));
			}
			FALLTHROUGH;
		case TYPE_PROPERTY_HAS_TAGOF:
			if (!type_is_user_defined(type))
			{

				expr->expr_kind = EXPR_TYPECALL;
				expr->type_call_expr = (ExprTypeCall) {
					.type = NULL,
					.property = property };
				return true;
			}
			goto TYPE_CALL;;
		case TYPE_PROPERTY_LOOKUP:
			if (!compiler.build.old_enums)
			{
				SEMA_DEPRECATED(expr, ".lookup is deprecated.");
			}
			goto TYPE_CALL;
		case TYPE_PROPERTY_FROM_ORDINAL:
		case TYPE_PROPERTY_LOOKUP_FIELD:
			goto TYPE_CALL;
		case TYPE_PROPERTY_NONE:
			return false;
	}
	UNREACHABLE
TYPE_CALL:
	expr->expr_kind = EXPR_TYPECALL;
	expr->type_call_expr = (ExprTypeCall) {
		.type = type->type_kind == TYPE_FUNC_PTR
				? type->pointer->function.decl
				: type->decl,
		.property = property };
	return true;
}


bool sema_expr_rewrite_insert_deref(SemaContext *context, Expr *original)
{
	if (expr_is_const_pointer(original) && !original->const_expr.ptr)
	{
		RETURN_SEMA_ERROR(original, "This value is known to be null so you cannot dereference it.");
	}
	// Assume *(&x) => x
	if (original->expr_kind == EXPR_UNARY && original->unary_expr.operator == UNARYOP_ADDR)
	{
		*original = *original->unary_expr.expr;
		return true;
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
		ASSERT(no_fail->canonical->type_kind == TYPE_POINTER);

		// Only fold to the canonical type if it wasn't a pointer.
		Type *pointee = no_fail->type_kind == TYPE_POINTER ? no_fail->pointer : no_fail->canonical->pointer;
		original->type = type_add_optional(pointee, IS_OPTIONAL(inner));
	}
	return true;
}

static inline bool sema_expr_analyse_swizzle(SemaContext *context, Expr *expr, Expr *parent, Type *flat_type,
                                             const char *kw, unsigned len, CheckType check, bool is_lvalue)
{
	unsigned vec_len = flat_type->array.len;
	Type *indexed_type = type_get_indexed_type(parent->type);
	assert(indexed_type);
	if (is_lvalue) check = CHECK_VALUE;
	ASSERT_SPAN(expr, len > 0);
	int index = 0;
	bool is_overlapping = false;
	for (unsigned i = 0; i < len; i++)
	{
		char val = (char)(swizzle[(int)kw[i]] - 1);
		if ((val & 0xF) >= vec_len)
		{
			RETURN_SEMA_ERROR(expr, "The '%c' component is not present in a vector of length %d, did you assume a longer vector?", kw[i], vec_len);
		}
		if (i == 0)
		{
			index = (int)val;
		}
		if ((index ^ val) & 0x10)
		{
			RETURN_SEMA_ERROR(expr, "Mixing [xyzw] and [rgba] is not permitted, you will need to select one of them.");
		}
		if (!is_overlapping)
		{
			for (int j = 0; j < i; j++)
			{
				char prev = (char)(swizzle[(int)kw[j]] - 1);
				if (val == prev)
				{
					is_overlapping = true;
					break;
				}
			}
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
			case CHECK_VALUE:
				expr->expr_kind = EXPR_SUBSCRIPT;
				break;
		}
		expr->subscript_expr = (ExprSubscript) {
				.index.expr = exprid(expr_new_const_int(expr->span, type_usz, index)),
				.expr = exprid(parent)
		};
		if (is_lvalue)
		{
			if (!sema_expr_analyse_subscript_lvalue(context, expr, false)) return false;
		}
		else
		{
			if (!sema_expr_analyse_subscript(context, expr, check, false)) return false;
		}
		expr->resolve_status = RESOLVE_DONE;
		if (check == CHECK_ADDRESS)
		{
			if (!sema_expr_rewrite_insert_deref(context, expr)) return false;
		}
		return true;
	}
	Type *result = type_get_vector(indexed_type, len);
	expr->expr_kind = EXPR_SWIZZLE;
	expr->swizzle_expr = (ExprSwizzle) { .parent = exprid(parent), .swizzle = kw, .is_overlapping = is_overlapping };

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
	Decl *ident = expr->ident_expr;
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
static inline bool sema_expr_analyse_access(SemaContext *context, Expr *expr, bool *missing_ref, CheckType check, bool lvalue)
{
	Expr *parent = expr->access_unresolved_expr.parent;
	if (missing_ref) *missing_ref = false;

	// 1. Resolve the left hand
	if (!sema_analyse_expr_check(context, parent, check != CHECK_VALUE ? CHECK_ADDRESS : CHECK_VALUE)) return false;

	// 2. The right hand side may be a @ident or ident
	Expr *child = expr->access_unresolved_expr.child;

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
			Type *type = parent->type_expr->type->canonical;
			switch (type->type_kind)
			{
				case CT_TYPES:
					RETURN_SEMA_ERROR(parent, "You cannot take the typeid of a compile time type.");
				default:
					expr->type = type_typeid;
					expr->expr_kind = EXPR_CONST;
					expr->const_expr.const_kind = CONST_TYPEID;
					expr->const_expr.typeid = parent->type_expr->type->canonical;
					expr->resolve_status = RESOLVE_DONE;
					return true;
			}
			UNREACHABLE
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
	Expr *identifier = sema_expr_resolve_access_child(context, child, missing_ref);
	if (!identifier) return false;
	const char *kw = identifier->unresolved_ident_expr.ident;

	// 2. If our left-hand side is a type, e.g. MyInt.abc, handle this here.
	if (parent->expr_kind == EXPR_TYPEINFO)
	{
		return sema_expr_analyse_type_access(context, expr, parent->type_expr->type, identifier, missing_ref);
	}
	if (parent->expr_kind == EXPR_IDENTIFIER)
	{
		Decl *decl = parent->ident_expr;
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

	ASSERT_SPAN(expr, expr->expr_kind == EXPR_ACCESS_UNRESOLVED);
	ASSERT_SPAN(expr, parent->resolve_status == RESOLVE_DONE);

	// 7. Is this a pointer? If so we insert a deref.
	Type *underlying_type = type_no_optional(parent->type)->canonical;
	if (underlying_type->type_kind == TYPE_POINTER && underlying_type != type_voidptr)
	{
		if (!sema_cast_rvalue(context, parent, true)) return false;
		if (!sema_expr_rewrite_insert_deref(context, expr->access_unresolved_expr.parent)) return false;
		parent = expr->access_unresolved_expr.parent;
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
		ArrayIndex index = sema_len_from_expr(current_parent);
		if (index > -1)
		{
			expr_rewrite_const_int(expr, type_isz, index);
			return true;
		}
		if (flat_type->type_kind == TYPE_SLICE)
		{
			expr_rewrite_slice_len(expr, current_parent, type_usz);
			return true;
		}
		assert(flat_type->type_kind != TYPE_ARRAY && flat_type->type_kind != TYPE_VECTOR);
	}
	if (flat_type->type_kind == TYPE_TYPEID)
	{
		bool was_error = false;
		if (sema_expr_rewrite_to_typeid_property(context, expr, current_parent, kw, &was_error)) return !was_error;
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
			return sema_expr_analyse_swizzle(context, expr, current_parent, flat_type, kw, len, check, lvalue);
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
	if (kw == kw_ordinal && flat_type->type_kind == TYPE_ENUM)
	{
		sema_expr_convert_enum_to_int(current_parent);
		expr_replace(expr, current_parent);
		return true;
	}
	if (kw == kw_nameof)
	{
		if (flat_type->type_kind == TYPE_CONST_ENUM)
		{
			if (sema_cast_const(current_parent))
			{
				expr_rewrite_const_string(expr, current_parent->const_expr.enum_val->name);
				return true;
			}
		}
		if (flat_type->type_kind == TYPE_ENUM)
		{
			if (sema_cast_const(current_parent))
			{
				expr_rewrite_const_string(expr, current_parent->const_expr.enum_val->name);
				return true;
			}
			expr_rewrite_to_builtin_access(expr, current_parent, ACCESS_ENUMNAME, type_string);
			return true;
		}
		if (flat_type == type_fault)
		{
			if (sema_cast_const(current_parent))
			{
				expr_rewrite_const_string(expr, current_parent->const_expr.fault ? current_parent->const_expr.fault->name : "null");
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

		ASSERT_SPAN(expr, expr->expr_kind == EXPR_ACCESS_UNRESOLVED);
		expr->type = method->type ? type_add_optional(method->type, optional) : NULL;
		expr->access_resolved_expr = (ExprResolvedAccess) { .parent = current_parent, .ref = method };
		expr->expr_kind = EXPR_ACCESS_RESOLVED;
		if (method->decl_kind == DECL_FUNC) unit_register_external_symbol(context, method);
		return true;
	}

	// 10. Dump all members and methods into a decl stack.
	Decl *decl = type->decl;

	Decl *member = sema_decl_stack_find_decl_member(context, decl, kw, METHODS_AND_FIELDS);
	if (!decl_ok(member)) return false;

	if (member && decl->decl_kind == DECL_ENUM && member->decl_kind == DECL_VAR && sema_cast_const(parent))
	{
		if (!sema_analyse_decl(context, decl)) return false;
		Decl *ref = current_parent->const_expr.enum_val;
		if (!sema_analyse_decl(context, ref)) return false;
		ASSERT_SPAN(expr, current_parent->const_expr.const_kind == CONST_ENUM);
		Expr *copy_init = copy_expr_single(ref->enum_constant.associated[member->var.index]);
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
			ASSERT(member);
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

	if (!member->unit && !sema_analyse_decl(context, decl)) return false;
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
				if (member->type->type_kind == TYPE_FLEXIBLE_ARRAY)
				{
					RETURN_SEMA_ERROR(expr, "Could not fold to member '%s', it's a flexible array member which is always empty.", member->name);
				}
				RETURN_SEMA_ERROR(expr, "Could not fold to member '%s' – it wasn't the last assigned member.", member->name);
			}
			return true;
		}
	}
	// 13. Copy properties.

	expr->access_resolved_expr = (ExprResolvedAccess) { .parent = current_parent, .ref = member };
	if (expr->expr_kind == EXPR_ACCESS_UNRESOLVED) expr->expr_kind = EXPR_ACCESS_RESOLVED;
	expr->type = type_add_optional(member->type, optional);
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
	ASSERT(size);
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
		for (ArrayIndex i = 0; i < (ArrayIndex)count; i++)
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
				exprs = new_args;
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
	if (inner->expr_kind == EXPR_INITIALIZER_LIST || inner->expr_kind == EXPR_DESIGNATED_INITIALIZER_LIST)
	{
		expr->expr_kind = EXPR_COMPOUND_LITERAL;
		expr->expr_compound_literal = (ExprCompoundLiteral) { .initializer = inner, .type_info = type_info };
		return sema_expr_analyse_compound_literal(context, expr);
	}
	bool success = sema_resolve_type_info(context, type_info, RESOLVE_TYPE_ALLOW_INFER);
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

static bool sema_expr_analyse_slice_assign(SemaContext *context, Expr *expr, Type *left_type, Expr *right, bool *failed_ref)
{
	Expr *left = exprptr(expr->binary_expr.left);
	Type *left_flat = type_flatten(left_type);
	Type *base = left_flat->array.base;
	if (right->expr_kind == EXPR_SLICE || (compiler.build.old_slice_copy && right->expr_kind == EXPR_INITIALIZER_LIST && right->initializer_list))
	{
		if (!sema_analyse_inferred_expr(context, left_type, right)) return false;
		if (left_flat == type_flatten(right->type) || right->type == type_untypedlist) goto SLICE_COPY;
	}
	else
	{
		if (right->expr_kind == EXPR_INITIALIZER_LIST && right->initializer_list)
		{
			Type *flat = type_flatten(base);
			switch (flat->type_kind)
			{
				case TYPE_STRUCT:
				case TYPE_ARRAY:
				case TYPE_VECTOR:
				case TYPE_UNION:
				case TYPE_INFERRED_ARRAY:
				case TYPE_INFERRED_VECTOR:
				case TYPE_BITSTRUCT:
				case TYPE_SLICE:
					break;
				default:
					RETURN_SEMA_ERROR(right, "You trying to assign this expression to each element in the slice, but the expression can't be cast to a value of type %s. Maybe you wanted to do a slice copy and forgot to add [..] at the end? Rather than 'a[..] = { ... }', try 'a[..] = { ... }[..]'.",
						type_quoted_error_string(base));
			}
		}
		if (!sema_analyse_inferred_expr(context, base, right)) return false;
	}
	Type *right_type = right->type->canonical;
	if (base->canonical != right_type && (type_is_arraylike(right_type) || right_type->type_kind == TYPE_SLICE))
	{
		if (right_type->array.base->canonical == base->canonical)
		{
			CHECK_ON_DEFINED(failed_ref);
			RETURN_SEMA_ERROR(right, "You cannot assign a slice, vector or array to a slicing without making an explicit [..] operation, e.g. 'foo[..] = my_array[..]', so you can try adding an explicit slicing to this expression.");
		}
	}
	if (!cast_implicit_checked(context, right, base, false, failed_ref)) return false;
	if (IS_OPTIONAL(right))
	{
		RETURN_SEMA_ERROR(right, "The right hand side may not be optional when using slice assign.");
	}
	expr->expr_kind = EXPR_SLICE_ASSIGN;
	expr->type = right->type;
	expr->slice_assign_expr.left = exprid(left);
	expr->slice_assign_expr.right = exprid(right);
	return true;

SLICE_COPY:;
	if (!sema_analyse_expr_rhs(context, left_type, right, false, NULL, false)) return false;
	Range *left_range = &left->slice_expr.range;
	IndexDiff left_len = range_const_len(left_range);
	IndexDiff right_len = 0;
	if (!expr_is_const(right))
	{
		ASSERT_SPAN(right, right->expr_kind == EXPR_SLICE);
		Range *right_range = &right->slice_expr.range;
		right_len = range_const_len(right_range);
	}
	else
	{
		right_len = sema_len_from_const(right);
	}
	if (left_len >= 0 && right_len >= 0 && left_len != right_len)
	{
		CHECK_ON_DEFINED(failed_ref);
		RETURN_SEMA_ERROR(expr, "Length mismatch between slices.");
	}
	expr->expr_kind = EXPR_SLICE_COPY;
	expr->type = left->type;
	expr->slice_assign_expr.left = exprid(left);
	expr->slice_assign_expr.right = exprid(right);
	return true;
}

bool sema_expr_analyse_assign_right_side(SemaContext *context, Expr *expr, Type *left_type, Expr *right,
                                         bool is_unwrapped_var, bool is_declaration, bool *failed_ref)
{
	if (expr && exprptr(expr->binary_expr.left)->expr_kind == EXPR_SLICE)
	{
		return sema_expr_analyse_slice_assign(context, expr, left_type, right, failed_ref);
	}

	// 1. Evaluate right side to required type.
	bool to_optional = left_type && type_is_optional(left_type);
	if (!sema_analyse_expr_rhs(context, left_type, right, is_unwrapped_var || to_optional, failed_ref, is_declaration)) return false;
	if (IS_OPTIONAL(right) && !to_optional)
	{
		if (is_unwrapped_var)
		{
			RETURN_SEMA_ERROR(exprptr(expr->binary_expr.left), "The variable is unwrapped in this context, if you don't want to unwrap it, use () around the variable to suppress unwrapping, like 'catch err = (x)' and 'try (x)'.");
		}
		if (!left_type) left_type = type_no_optional(right->type);
		CHECK_ON_DEFINED(failed_ref);
		return sema_error_failed_cast(context, right, right->type, left_type);
	}

	// 3. Set the result to the type on the right side.
	if (expr) expr->type = right->type;

	return true;
}

static bool sema_expr_analyse_ct_identifier_assign(SemaContext *context, Expr *expr, Expr *left, Expr *right, bool *failed_ref)
{
	ASSERT_SPAN(left, left->resolve_status == RESOLVE_DONE);

	// Evaluate right side to using inference from last type.
	if (!sema_analyse_inferred_expr(context, left->type, right)) return false;

	if (!expr_is_runtime_const(right))
	{
		if (failed_ref) return *failed_ref = true, false;
		RETURN_SEMA_ERROR(right, "You can only assign constants to a compile time variable.");
	}
	Decl *ident = left->ct_ident_expr.decl;

	ident->var.init_expr = right;
	expr_replace(expr, right);
	ident->type = right->type;
	return true;
}

static bool sema_expr_analyse_ct_subscript_rhs(SemaContext *context, Decl *ct_var, Expr *right)
{
	if (ct_var->type == type_untypedlist)
	{
		if (!sema_analyse_expr(context, right)) return false;
	}
	else
	{
		if (!sema_analyse_expr_rhs(context, type_get_indexed_type(ct_var->type), right, false, NULL, false)) return false;
	}
	if (!expr_is_runtime_const(right))
	{
		RETURN_SEMA_ERROR(right, "The argument must be a constant value.");
	}
	return true;
}

static bool sema_expr_analyse_ct_subscript_set_value(SemaContext *context, Expr *left, Decl *ct_var, Expr *right)
{
	ArrayIndex index = left->ct_subscript_expr.index;
	Expr *original_value = ct_var->var.init_expr;
	ASSERT_SPAN(original_value, original_value->expr_kind == EXPR_CONST);
	ExprConst *expr_const = &original_value->const_expr;

	switch (expr_const->const_kind)
	{
		case CONST_FLOAT:
		case CONST_INTEGER:
		case CONST_BOOL:
		case CONST_ENUM:
		case CONST_FAULT:
		case CONST_POINTER:
		case CONST_TYPEID:
		case CONST_REF:
		case CONST_MEMBER:
			UNREACHABLE
		case CONST_BYTES:
		case CONST_STRING:
		{
			unsigned char *copy = MALLOC(expr_const->bytes.len + 1);
			memcpy(copy, expr_const->bytes.ptr, expr_const->bytes.len + 1);
			assert(right->const_expr.const_kind == CONST_INTEGER);
			copy[index] = (unsigned char)right->const_expr.ixx.i.low;
			expr_const->bytes.ptr = (char *)copy;
			break;
		}
		case CONST_SLICE:
			const_init_rewrite_array_at(expr_const->slice_init, right, index);
		break;
		case CONST_INITIALIZER:
			const_init_rewrite_array_at(expr_const->initializer, right, index);
		break;
		case CONST_UNTYPED_LIST:
			expr_const->untyped_list[index] = right;
		break;
	}
	return true;
}

static bool sema_expr_analyse_ct_subscript_assign(SemaContext *context, Expr *expr, Expr *left, Expr *right)
{
	Decl *ct_var = left->ct_subscript_expr.var;
	if (!sema_expr_analyse_ct_subscript_rhs(context, ct_var, right)) return false;
	if (!sema_expr_analyse_ct_subscript_set_value(context, left, ct_var, right)) return false;
	expr_replace(expr, right);
	return true;
}


static bool sema_expr_fold_hash(SemaContext *context, Expr *expr)
{
	assert(expr->expr_kind == EXPR_HASH_IDENT);
	while (expr->expr_kind == EXPR_HASH_IDENT)
	{
		ASSERT(expr && expr->hash_ident_expr.identifier);
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
	if (!sema_analyse_expr_lvalue(context, left, failed_ref)) return false;
	switch (left->expr_kind)
	{
		case EXPR_CT_IDENT:
			// $foo = ...
			return sema_expr_analyse_ct_identifier_assign(context, expr, left, right, failed_ref);
		case EXPR_CT_SUBSCRIPT:
			return sema_expr_analyse_ct_subscript_assign(context, expr, left, right);
		default:
			break;
	}

	// 2. Check assignability
	if (!sema_expr_check_assign(context, left, failed_ref)) return false;

	bool is_unwrapped_var = expr_is_unwrapped_ident(left);

	// 3. Evaluate right side to required type.
	if (!sema_expr_analyse_assign_right_side(context, expr, left->type, right, is_unwrapped_var, false, failed_ref)) return false;

	if (is_unwrapped_var && IS_OPTIONAL(right))
	{
		sema_rewrap_var(context, left->ident_expr);
		return true;
	}
	if (left->expr_kind == EXPR_SUBSCRIPT_ASSIGN)
	{
		Decl *temp = decl_new_generated_var(right->type, VARDECL_LOCAL, right->span);
		Expr *expr_rh = expr_generate_decl(temp, right);
		Expr **args = NULL;
		vec_add(args, exprptr(left->subscript_assign_expr.index));
		vec_add(args, expr_variable(temp));
		if (!sema_insert_method_call(context, expr, declptr(left->subscript_assign_expr.method), exprptr(left->subscript_assign_expr.expr), args, false)) return false;
		expr_rewrite_two(expr, expr_rh, expr_copy(expr));
		return true;
	}
	if (left->expr_kind == EXPR_BITACCESS)
	{
		if (!sema_bit_assignment_check(context, right, left->access_resolved_expr.ref)) return false;
		expr->expr_kind = EXPR_BITASSIGN;
	}
	return true;
}


/**
 * Analyse define $foo = ...
 *
 * @return true if analysis worked.
 */
static bool sema_binary_analyse_ct_op_assign(SemaContext *context, Expr *expr, Expr *left)
{
	ASSERT_SPAN(left, left->expr_kind == EXPR_CT_IDENT);

	Decl *left_var = left->ct_ident_expr.decl;
	if (!sema_cast_ct_ident_rvalue(context, left)) return false;

	expr->binary_expr.operator = binaryop_assign_base_op(expr->binary_expr.operator);

	if (!sema_expr_analyse_binary(context, NULL, expr, NULL)) return false;
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
 * Analyse $foo[1] += ...
 *
 * @return true if analysis worked.
 */
static bool sema_binary_analyse_ct_subscript_op_assign(SemaContext *context, Expr *expr, Expr *left)
{
	ASSERT_SPAN(left, left->expr_kind == EXPR_CT_SUBSCRIPT);

	Decl *left_var = left->ct_subscript_expr.var;
	ArrayIndex idx = left->ct_subscript_expr.index;
	Expr *lhs_expr = left_var->var.init_expr;
	ASSERT_SPAN(lhs_expr, lhs_expr->expr_kind == EXPR_CONST);
	Expr *value = expr_from_const_expr_at_index(lhs_expr, idx);

	BinaryOp op = binaryop_assign_base_op(expr->binary_expr.operator);
	expr->binary_expr = (ExprBinary) { .left = exprid(value), .right = expr->binary_expr.right, .operator = op };
	if (!sema_expr_analyse_binary(context, NULL, expr, NULL)) return false;
	if (!sema_expr_analyse_ct_subscript_set_value(context, left, left_var, expr)) return false;
	return true;

}

static BoolErr sema_insert_overload_in_op_assign_or_error(SemaContext *context, Expr *expr, Expr *left, Expr *right, BinaryOp operator, Type *lhs_type)
{
	assert(type_is_user_defined(lhs_type));
	if (!sema_analyse_inferred_expr(context, lhs_type, right)) return BOOL_ERR;
	static OperatorOverload MAP[BINARYOP_LAST + 1] = {
		[BINARYOP_ADD_ASSIGN] = OVERLOAD_PLUS_ASSIGN,
		[BINARYOP_SUB_ASSIGN] = OVERLOAD_MINUS_ASSIGN,
		[BINARYOP_MULT_ASSIGN] = OVERLOAD_MULTIPLY_ASSIGN,
		[BINARYOP_DIV_ASSIGN] = OVERLOAD_DIVIDE_ASSIGN,
		[BINARYOP_MOD_ASSIGN] = OVERLOAD_REMINDER_ASSIGN,
		[BINARYOP_BIT_XOR_ASSIGN] = OVERLOAD_XOR_ASSIGN,
		[BINARYOP_BIT_OR_ASSIGN] = OVERLOAD_OR_ASSIGN,
		[BINARYOP_BIT_AND_ASSIGN] = OVERLOAD_AND_ASSIGN,
		[BINARYOP_SHL_ASSIGN] = OVERLOAD_SHL_ASSIGN,
		[BINARYOP_SHR_ASSIGN] = OVERLOAD_SHR_ASSIGN,
	};
	OperatorOverload overload = MAP[operator];
	assert(overload && "Overload not mapped");
	if (!sema_replace_with_overload(context, expr, left, right, lhs_type, &overload)) return BOOL_ERR;
	if (!overload)
	{
		return BOOL_TRUE;
	}
	return BOOL_FALSE;
}

INLINE bool sema_rewrite_op_assign(SemaContext *context, Expr *expr, Expr *left, Expr *right, BinaryOp new_op)
{
	// Simple case: f += a => f = f + a
	if (left->expr_kind == EXPR_IDENTIFIER)
	{
		Expr *lvalue = expr_copy(left);
		Expr *rvalue = expr_copy(left);
		expr->expr_kind = EXPR_BINARY;
		left->expr_kind = EXPR_BINARY;
		left->binary_expr = (ExprBinary) { .left = exprid(rvalue), .right = exprid(right), .operator = new_op };
		expr->binary_expr = (ExprBinary) { .left = exprid(lvalue), .right = exprid(left), .operator = BINARYOP_ASSIGN, .grouped = false };
		expr->resolve_status = RESOLVE_NOT_DONE;
		left->resolve_status = RESOLVE_NOT_DONE;
		return sema_analyse_expr(context, expr);
	}

	Type *lhs_type = type_no_optional(left->type)->canonical;
	Decl *variable = decl_new_generated_var(type_get_ptr(left->type), VARDECL_LOCAL, left->span);
	Expr *left_copy = expr_copy(left);

	// If we have a &[] overload, then replace left copy with that one.
	if (left->expr_kind == EXPR_SUBSCRIPT_ASSIGN)
	{
		Expr *parent = exprptr(left->subscript_assign_expr.expr);
		Type *parent_type = type_no_optional(parent->type)->canonical;
		Decl *operator = sema_find_untyped_operator(context, parent_type, OVERLOAD_ELEMENT_REF, NULL);
		Expr *index = exprptr(left->subscript_assign_expr.index);
		if (operator)
		{
			Expr **args = NULL;
			vec_add(args, index);
			if (!sema_insert_method_call(context, left_copy, operator, parent, args, false)) return false;
			goto AFTER_ADDR;
		}
		// If we only have []=, then we need []
		operator = sema_find_untyped_operator(context, parent_type, OVERLOAD_ELEMENT_AT, NULL);
		if (!operator)
		{
			RETURN_SEMA_ERROR(left, "There is no overload for [] for %s.", type_quoted_error_string(type_no_optional(left->type)));
		}
		Type *return_type = typeget(operator->func_decl.signature.rtype);
		if (type_no_optional(return_type->canonical) != lhs_type->canonical)
		{
			RETURN_SEMA_ERROR(expr, "There is a type mismatch between the overload for [] and []= for %s.", type_quoted_error_string(type_no_optional(left->type)));
		}
		// First we want to create the indexed value and the index:
		Decl *index_val = decl_new_generated_var(index->type, VARDECL_LOCAL, index->span);
		// We need to take the address of the parent here, otherwise this might fail,
		expr_insert_addr(parent);
		Decl *parent_val = decl_new_generated_var(parent->type, VARDECL_LOCAL, parent->span);
		Expr **list = NULL;
		// temp = parent, temp_2 = index
		vec_add(list, expr_generate_decl(parent_val, parent));
		vec_add(list, expr_generate_decl(index_val, index));
		// Now, create a lhs of the binary add:
		Expr *lhs = expr_new_expr(EXPR_SUBSCRIPT, left);
		Expr *parent_by_variable = expr_variable(parent_val);
		if (!sema_expr_rewrite_insert_deref(context, parent_by_variable)) return false;
		lhs->subscript_expr = (ExprSubscript) { .expr = exprid(parent_by_variable), .index.expr = exprid(expr_variable(index_val)) };
		// Now create the binary expression
		Expr *binary = expr_new_expr(EXPR_BINARY, expr);
		binary->binary_expr = (ExprBinary) { .left = exprid(lhs), .right = exprid(right), .operator = new_op };
		// Finally, we need the assign, and here we just need to replace
		Expr *assign = expr_new_expr(EXPR_BINARY, expr);
		assign->binary_expr = (ExprBinary) { .left = exprid(left), .right = exprid(binary), .operator = BINARYOP_ASSIGN };
		// Now we need to patch the values in `left`:
		parent_by_variable = expr_variable(parent_val);
		if (!sema_expr_rewrite_insert_deref(context, parent_by_variable)) return false;
		left->subscript_assign_expr.expr = exprid(parent_by_variable);
		left->subscript_assign_expr.index = exprid(expr_variable(index_val));
		// We add the assign
		vec_add(list, assign);
		// And rewrite the expression to an expression list:
		expr->expr_kind = EXPR_EXPRESSION_LIST;
		expr->expression_list = list;
		return sema_expr_analyse_expr_list(context, expr);
	}

	// f => &f
	expr_insert_addr(left_copy);

AFTER_ADDR:;

	// temp = &f
	Expr *init = expr_generate_decl(variable, left_copy);
	// lvalue = temp, rvalue = temp
	Expr *left_rvalue = expr_variable(variable);
	Expr *left_lvalue = expr_variable(variable);

	// lvalue = *temp, rvalue = *temp
	if (!sema_expr_rewrite_insert_deref(context, left_lvalue)) return false;
	if (!sema_expr_rewrite_insert_deref(context, left_rvalue)) return false;

	// init, expr -> lvalue = rvalue + a
	expr->expr_kind = EXPR_BINARY;
	left->expr_kind = EXPR_BINARY;
	left->binary_expr = (ExprBinary) { .left = exprid(left_lvalue), .right = exprid(right), new_op };
	expr->binary_expr = (ExprBinary) { .left = exprid(left_rvalue), .right = exprid(left), .operator = BINARYOP_ASSIGN, .grouped = false };
	expr->resolve_status = RESOLVE_NOT_DONE;
	left->resolve_status = RESOLVE_NOT_DONE;
	Expr *binary = expr_copy(expr);
	expr_rewrite_two(expr, init, binary);
	return sema_analyse_expr(context, expr);
}

static bool sema_expr_analyse_op_assign_enum_ptr(SemaContext *context, Expr *rhs, Type *flat, Type *base, Type *flat_rhs, bool is_enum, BinaryOp op)
{
	if (flat == base)
	{
		if (!type_is_integer(flat_rhs))
		{
			RETURN_SEMA_ERROR(rhs,
							  "The right side was '%s' but only integers are valid on the right side of %s when the left side is %s.",
							  type_to_error_string(rhs->type),
							  token_type_to_string(binaryop_to_token(op)), is_enum ? "an enum" : "a pointer");
		}
		Type *to = is_enum ? enum_inner_type(flat) : type_isz;
		if (!cast_implicit(context, rhs, to, true)) return false;
	}
	else
	{
		Type *real_type = type_get_vector(is_enum ? enum_inner_type(base) : type_isz, flat->array.len);
		if (flat_rhs == type_untypedlist)
		{
			if (!cast_implicit(context, rhs, real_type, true)) return false;
			flat_rhs = type_flat_for_arithmethics(rhs->type);
		}
		if (!type_is_integer(flat_rhs) && (flat_rhs->type_kind != TYPE_VECTOR || !type_is_integer(flat_rhs->array.base)))
		{
			RETURN_SEMA_ERROR(rhs,
							  "The right side was '%s' but only integers or integer vectors are valid on the right side of %s when the left side is %s.",
							  type_to_error_string(rhs->type),
							  token_type_to_string(binaryop_to_token(op)), is_enum ? "an enum vector" : "a pointer vector");
		}
		if (!cast_implicit(context, rhs, real_type, true)) return false;
	}
	return true;
}
/**
 * Analyse *= /= %= ^= |= &= += -= <<= >>=
 *
 * @return true if analysis worked.
 */
static bool sema_expr_analyse_op_assign(SemaContext *context, Expr *expr, Expr *left, Expr *right, BinaryOp operator)
{
	bool *failed_ref = NULL;
	bool is_bit_op = false;
	bool is_shift = false;
	bool is_add_sub = false;
	bool int_only = false;
	switch (operator)
	{
		case BINARYOP_MULT_ASSIGN:
		case BINARYOP_DIV_ASSIGN:
			break;
		case BINARYOP_BIT_AND_ASSIGN:
		case BINARYOP_BIT_OR_ASSIGN:
		case BINARYOP_BIT_XOR_ASSIGN:
			int_only = true;
			is_bit_op = true;
			break;
		case BINARYOP_ADD_ASSIGN:
		case BINARYOP_SUB_ASSIGN:
			is_add_sub = true;
			break;
		case BINARYOP_MOD_ASSIGN:
			int_only = true;
			break;
		case BINARYOP_SHL_ASSIGN:
		case BINARYOP_SHR_ASSIGN:
			is_shift = true;
			int_only = true;
			break;
		default:
			UNREACHABLE

	}
	// 1. Analyse left side.
	if (!sema_analyse_expr_lvalue(context, left, NULL)) return false;

	switch (left->expr_kind)
	{
		case EXPR_CT_IDENT:
			return sema_binary_analyse_ct_op_assign(context, expr, left);
		case EXPR_CT_SUBSCRIPT:
			return sema_binary_analyse_ct_subscript_op_assign(context, expr, left);
		default:
			break;
	}

	// 2. Verify that the left side is assignable.
	if (!sema_expr_check_assign(context, left, NULL)) return false;

	Type *left_type_canonical = left->type->canonical;

	// 3. Check that it is readable
	if (!sema_cast_rvalue(context, left, false)) return false;

	Type *no_fail = type_no_optional(left->type);

	Type *canonical = no_fail->canonical;
	if (type_is_user_defined(canonical))
	{
		if (canonical->type_kind == TYPE_BITSTRUCT)
		{
			if (operator == BINARYOP_BIT_OR_ASSIGN
				|| operator == BINARYOP_BIT_AND_ASSIGN
				|| operator == BINARYOP_BIT_XOR_ASSIGN) goto SKIP_OVERLOAD_CHECK;
		}
		BoolErr b = sema_insert_overload_in_op_assign_or_error(context, expr, left, right, operator, no_fail->canonical);
		if (b == BOOL_ERR) return false;
		if (b == BOOL_TRUE) return true;
		// Maybe we have the corresponding implemented % * etc
		// The right hand side is now already checked.
		BinaryOp underlying_op = binaryop_assign_base_op(operator);
		static OperatorOverload MAP[BINARYOP_LAST + 1] = {
			[BINARYOP_ADD] = OVERLOAD_PLUS,
			[BINARYOP_SUB] = OVERLOAD_MINUS,
			[BINARYOP_DIV] = OVERLOAD_DIVIDE,
			[BINARYOP_MOD] = OVERLOAD_REMINDER,
			[BINARYOP_MULT] = OVERLOAD_MULTIPLY,
			[BINARYOP_BIT_XOR] = OVERLOAD_XOR,
			[BINARYOP_BIT_OR] = OVERLOAD_OR,
			[BINARYOP_BIT_AND] = OVERLOAD_AND,
			[BINARYOP_SHL] = OVERLOAD_SHL,
			[BINARYOP_SHR] = OVERLOAD_SHR,
		};
		OperatorOverload mapped_overload = MAP[underlying_op];
		bool reverse = false;
		Decl *candidate = expr_may_ref(left) ? sema_find_typed_operator(context, mapped_overload, expr->span, left, right, &reverse) : NULL;
		if (!decl_ok(candidate)) return false;
		if (candidate && typeget(candidate->func_decl.signature.rtype)->canonical == canonical)
		{
			return sema_rewrite_op_assign(context, expr, left, right, underlying_op);
		}
	}
SKIP_OVERLOAD_CHECK:;
	// 3. If this is only defined for ints (^= |= &= %=) verify that this is an int.
	Type *flat = type_flat_for_arithmethics(no_fail);
	Type *base = flat->type_kind == TYPE_VECTOR ? type_flat_for_arithmethics(flat->array.base) : flat;

	if (int_only && !type_is_integer(base))
	{
		if (is_bit_op && (flat->type_kind == TYPE_BITSTRUCT || flat == type_bool || type_flat_is_bool_vector(flat))) goto BITSTRUCT_OK;
		RETURN_SEMA_ERROR(left, "Expected an integer here, not a value of type %s.", type_quoted_error_string(left->type));
	}

	// 4. In any case, these ops are only defined on numbers.
	if (!type_is_numeric(base) && !(is_add_sub && type_underlying_may_add_sub(base)))
	{
		RETURN_SEMA_ERROR(left, "Expected a numeric type here, not a value of type %s.", type_quoted_error_string(left->type));
	}


BITSTRUCT_OK:

	// 5. Analyse RHS
	if (base->type_kind == TYPE_ENUM || base->type_kind == TYPE_POINTER )
	{
		if (!sema_analyse_expr(context, right)) return false;
	}
	else
	{
		if (!sema_analyse_inferred_expr(context, left->type, right)) return false;
	}

	// 3. Copy type & set properties.
	if (IS_OPTIONAL(right) && !IS_OPTIONAL(left))
	{
		RETURN_SEMA_ERROR(right, "Cannot assign an optional value to a non-optional.");
	}

	expr->type = left->type;
	bool optional = IS_OPTIONAL(left) || IS_OPTIONAL(right);

	Type *type_rhs_inline = type_flat_for_arithmethics(right->type);


	// 5. In the enum case we have to treat this differently.
	if (base->type_kind == TYPE_ENUM)
	{
		if (!sema_expr_analyse_op_assign_enum_ptr(context, right, flat, base, type_rhs_inline, true, expr->binary_expr.operator)) return false;
		goto END;
	}
	if (base->type_kind == TYPE_POINTER)
	{
		if (!sema_expr_analyse_op_assign_enum_ptr(context, right, flat, base, type_rhs_inline, false, expr->binary_expr.operator)) return false;
		goto END;
	}

	if (is_shift)
	{
		if (!sema_expr_check_shift_rhs(context, expr, left, type_flatten_and_inline(left->type), right, type_flatten_and_inline(right->type), failed_ref, true))
		{
			return false;
		}
	}
	else
	{
		// Otherwise cast left to right.
		if (!cast_implicit_binary(context, right, no_fail, failed_ref)) return false;
	}

	// 6. Check for zero in case of div or mod.
	if (sema_cast_const(right))
	{
		switch (operator)
		{
			case BINARYOP_DIV_ASSIGN:
			case BINARYOP_MOD_ASSIGN:
				switch (right->const_expr.const_kind)
				{
					case CONST_INTEGER:
						if (int_is_zero(right->const_expr.ixx)) RETURN_SEMA_ERROR(right, operator == BINARYOP_MOD_ASSIGN ? "% by zero not allowed." : "Division by zero not allowed.");
						break;
					case CONST_FLOAT:
						if (right->const_expr.fxx.f == 0) RETURN_SEMA_ERROR(right, operator == BINARYOP_MOD_ASSIGN ? "% by zero not allowed." : "% by zero not allowed.");
						break;
					default:
						break;
				}
				break;
			case BINARYOP_SHL_ASSIGN:
			case BINARYOP_SHR_ASSIGN:
				if (expr_is_const_int(right))
				{
					//  Make sure the value does not exceed the bitsize of
					//     the left hand side.
					if (int_ucomp(right->const_expr.ixx, left->type->canonical->builtin.bitsize, BINARYOP_GT))
					{
						RETURN_SEMA_ERROR(right, "The shift exceeds bitsize of '%s'.", type_to_error_string(left->type));
					}

					// 4b. Make sure that the right hand side is positive.
					if (int_is_neg(right->const_expr.ixx))
					{
						RETURN_SEMA_ERROR(right, "A shift must be a positive number.");
					}
				}
				break;
			default:
				break;
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

static bool sema_replace_with_overload(SemaContext *context, Expr *expr, Expr *left, Expr *right, Type *left_type, OperatorOverload* operator_overload_ref)
{
	assert(!type_is_optional(left_type) && left_type->canonical == left_type);
	bool reverse;
	Decl *overload = sema_find_typed_operator(context, *operator_overload_ref, expr->span, left, right, &reverse);
	if (!decl_ok(overload)) return false;
	if (overload)
	{
		*operator_overload_ref = (OperatorOverload)0; // NOLINT
		return sema_insert_binary_overload(context, expr, overload, left, right, reverse);
	}
	return true;
}

static inline bool sema_check_untyped_promotion(SemaContext *context, Expr *expr, bool is_left, CanonicalType *max_flat, Type *max)
{
	Type *flat = type_flatten(expr->type);
	if (!type_is_unsigned(flat) || type_size(max_flat) != type_size(flat)) return true;
	if (sema_cast_const(expr) && expr_is_const_int(expr) && expr_const_will_overflow(&expr->const_expr, max_flat->type_kind))
	{
		RETURN_SEMA_ERROR(expr,
			"This expression (%s) will be implicitly converted to a signed type due to the %s-hand side being signed, but the value does not fit %s. "
			"To fix this, either cast the value explicitly, or make the %s-hand side an unsigned type.",
			expr_const_to_error_string(&expr->const_expr), is_left ? "right" : "left", type_quoted_error_string(max),
			is_left ? "right" : "left");
	}

	return true;

}
static bool sema_binary_arithmetic_promotion(SemaContext *context, Expr *left, Expr *right, Type *left_type, Type *right_type,
											 Expr *parent, const char *error_message, bool allow_bool_vec,
											 OperatorOverload *operator_overload_ref,
											 bool *failed_ref)
{
	OperatorOverload overload = *operator_overload_ref;
	if (type_is_user_defined(left_type) || type_is_user_defined(right_type))
	{
		if (!sema_replace_with_overload(context, parent, left, right, left_type, operator_overload_ref)) return false;
		if (!*operator_overload_ref) return true;
	}

	if (overload == OVERLOAD_PLUS)
	{
		Type *cast_to_iptr = defer_iptr_cast(left);
		if (!cast_to_iptr) cast_to_iptr = defer_iptr_cast(right);
		left_type = type_no_optional(left->type)->canonical;
		right_type = type_no_optional(right->type)->canonical;
		if (type_is_pointer_like(left_type))
		{
			*operator_overload_ref = OVERLOAD_NONE;
			return sema_expr_analyse_ptr_add(context, parent, left, right, left_type, right_type, cast_to_iptr, failed_ref);
		}
		if (type_is_pointer_like(right_type))
		{
			*operator_overload_ref = OVERLOAD_NONE;
			return sema_expr_analyse_ptr_add(context, parent, right, left, right_type, left_type, cast_to_iptr, failed_ref);
		}
	}

	Type *max = cast_numeric_arithmetic_promotion(type_find_max_type(left_type, right_type));
	if (!max || (!type_underlying_is_numeric(max) && !(allow_bool_vec && type_flat_is_bool_vector(max))))
	{
		CHECK_ON_DEFINED(failed_ref);
		if (!error_message)
		{
			return sema_type_error_on_binop(context, parent);
		}
		RETURN_SEMA_ERROR(parent, error_message, type_quoted_error_string(left->type), type_quoted_error_string(right->type));
	}
	Type *flat_max = type_flatten(max);
	if (type_is_signed(flat_max))
	{
		if (!sema_check_untyped_promotion(context, left, true, flat_max, max)) return false;
		if (!sema_check_untyped_promotion(context, right, false, flat_max, max)) return false;
	}
	return cast_implicit_binary(context, left, max, failed_ref) &&
		   cast_implicit_binary(context, right, max, failed_ref);
}

static void sema_binary_unify_voidptr(Expr *left, Expr *right, Type **left_type_ref, Type **right_type_ref)
{
	if (*left_type_ref == *right_type_ref) return;
	if (*left_type_ref == type_voidptr)
	{
		cast_no_check(left, *right_type_ref, IS_OPTIONAL(left));
		*left_type_ref = *right_type_ref;
	}
	if (*right_type_ref == type_voidptr)
	{
		cast_no_check(right, *left_type_ref, IS_OPTIONAL(right));
		*right_type_ref = *left_type_ref;
	}
}

static Type *defer_iptr_cast(Expr *maybe_pointer)
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

static bool sema_expr_analyse_enum_add_sub(SemaContext *context, Expr *expr, Expr *left, Expr *right, bool *failed_ref)
{
	Type *left_type = type_no_optional(left->type)->canonical;
	bool is_sub = expr->binary_expr.operator == BINARYOP_SUB;
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

	// Enum - value / Enum + value
	sema_expr_convert_enum_to_int(left);
	if (!cast_implicit(context, right, left->type, true)) return false;
	expr->type = type_add_optional(left_type, IS_OPTIONAL(left) || IS_OPTIONAL(right));
	if (expr_both_const_foldable(left, right, BINARYOP_ADD))
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
		expr->const_expr = (ExprConst) { .const_kind = CONST_ENUM, .enum_val = enums[int_to_i64(i)] };
		expr->expr_kind = EXPR_CONST;
		expr->resolve_status = RESOLVE_DONE;
	}
	return true;

}

INLINE bool sema_expr_analyse_ptr_sub(SemaContext *context, Expr *expr, Expr *left, Expr *right, CanonicalType *left_type, Type *cast_to_iptr, bool *failed_ref)
{
	Type *right_type = type_no_optional(right->type)->canonical;
	bool left_is_ptr_vector = left_type->type_kind != TYPE_POINTER;
	ArraySize vec_len = left_is_ptr_vector ? left_type->array.len : 0;
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
		sema_binary_unify_voidptr(left, right, &left_type, &right_type);
		if (left_type != right_type)
		{
			CHECK_ON_DEFINED(failed_ref);
			RETURN_SEMA_ERROR(expr, "'%s' - '%s' is not allowed. Subtracting pointers of different types is not allowed.",
				type_to_error_string(left_type), type_to_error_string(right_type));
		}

		if (expr_both_const_foldable(left, right, BINARYOP_SUB))
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
		CHECK_ON_DEFINED(failed_ref);
		RETURN_SEMA_ERROR(expr, "Cannot subtract '%s' from '%s'", type_to_error_string(right_type),
		                  type_to_error_string(left_type));
	}

	// 5. Make sure that the integer does not exceed isz in size.
	ArraySize max_size = right_is_vector ? type_size(offset_type) : type_size(type_isz);
	if (type_size(right_type) > max_size)
	{
		CHECK_ON_DEFINED(failed_ref);
		RETURN_SEMA_ERROR(expr, "Cannot subtract %s from a %s, you need to add an explicit a narrowing cast to %s.",
		                  type_quoted_error_string(right->type),
		                  left_is_ptr_vector ? "pointer vector" : "pointer",
		                  type_quoted_error_string(right_is_vector ? offset_type : type_isz));
	}

	// 6. Convert to isz
	if (!cast_implicit_binary(context, right, offset_type, failed_ref)) return false;

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
/**
 * Analyse a - b
 * @return true if analysis succeeded
 */
static bool sema_expr_analyse_sub(SemaContext *context, Expr *expr, Expr *left, Expr *right, bool *failed_ref)
{
	// 1. Analyse a and b.
	if (!sema_binary_analyse_subexpr(context, left, right)) return false;

	// Do we have (iptr)(ptr) - rhs? If so we change it to
	// (iptr)((char*)(ptr) - 1)
	Type *cast_to_iptr = defer_iptr_cast(left);

	Type *left_type = type_no_optional(left->type)->canonical;
	Type *right_type = type_no_optional(right->type)->canonical;

	bool left_is_pointer_vector = type_is_pointer_vector(left_type);
	bool left_is_pointer = left_is_pointer_vector || left_type->type_kind == TYPE_POINTER;

	// 2. Handle the ptr - x and ptr - other_pointer
	if (left_is_pointer)
	{
		return sema_expr_analyse_ptr_sub(context, expr, left, right, left_type, cast_to_iptr, failed_ref);
	}

	// Enum - Enum and Enum - int
	if (left_type->type_kind == TYPE_ENUM)
	{
		return sema_expr_analyse_enum_add_sub(context, expr, left, right, failed_ref);
	}

	// 7. Attempt arithmetic promotion, to promote both to a common type.
	OperatorOverload overload = OVERLOAD_MINUS;
	if (!sema_binary_arithmetic_promotion(context, left, right, left_type, right_type, expr,
										  "The subtraction %s - %s is not possible.", false, &overload, failed_ref))
	{
		return false;
	}

	if (!overload) return true;

	// 8. Handle constant folding.
	if (expr_both_const_foldable(left, right, BINARYOP_SUB))
	{
		expr_replace(expr, left);
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

	expr_binary_unify_failability(expr, left, right);

	return true;

}

INLINE bool sema_expr_analyse_ptr_add(SemaContext *context, Expr *expr, Expr *left, Expr *right, CanonicalType *left_type, CanonicalType *right_type, Type *cast_to_iptr, bool *failed_ref)
{
	bool left_is_vec = left_type->type_kind == TYPE_VECTOR;
	bool right_is_vec = right_type->type_kind == TYPE_VECTOR;
	ArraySize vec_len = left_is_vec ? left_type->array.len : 0;

	// 3a. Check that the other side is an integer of some sort.
	if (!type_is_integer(right_type))
	{
		if (!left_is_vec || !right_is_vec || !type_is_integer(right_type->array.base))
		{
			CHECK_ON_DEFINED(failed_ref);
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
	(void) success;

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
/**
 * Analyse a + b
 * @return true if it succeeds.
 */
static bool sema_expr_analyse_add(SemaContext *context, Expr *expr, Expr *left, Expr *right, bool *failed_ref)
{
	// 1. Promote everything to the recipient type – if possible
	//    this is safe in the pointer case actually.
	if (!sema_binary_analyse_subexpr(context, left, right)) return false;


	Type *left_type = type_no_optional(left->type)->canonical;
	Type *right_type = type_no_optional(right->type)->canonical;

	if (left_type->type_kind == TYPE_ENUM || right_type->type_kind == TYPE_ENUM)
	{
		return sema_expr_analyse_enum_add_sub(context, expr, left, right, failed_ref);
	}

	// 4. Do a binary arithmetic promotion
	OperatorOverload overload = OVERLOAD_PLUS;
	if (!sema_binary_arithmetic_promotion(context, left, right, left_type, right_type, expr,
		"Cannot do the addition %s + %s.", false, &overload, failed_ref))
	{
		return false;
	}
	if (!overload) return true;

	// 5. Handle the "both const" case. We should only see ints and floats at this point.
	if (expr_both_const_foldable(left, right, BINARYOP_ADD))
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
static bool sema_expr_analyse_mult(SemaContext *context, Expr *expr, Expr *left, Expr *right, bool *failed_ref)
{

	// 1. Analyse the sub expressions and promote to a common type
	OperatorOverload overload = OVERLOAD_MULTIPLY;
	if (!sema_binary_analyse_arithmetic_subexpr(context, expr, "It is not possible to multiply %s by %s.", false, &overload, failed_ref)) return false;

	if (!overload) return true;

	// 2. Handle constant folding.
	if (expr_both_const_foldable(left, right, BINARYOP_MULT))
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
static bool sema_expr_analyse_div(SemaContext *context, Expr *expr, Expr *left, Expr *right, bool *failed_ref)
{
	// 1. Analyse sub expressions and promote to a common type
	OperatorOverload overload = OVERLOAD_DIVIDE;
	if (!sema_binary_analyse_arithmetic_subexpr(context, expr, "Cannot divide %s by %s.", false, &overload, failed_ref)) return false;
	if (!overload) return true;

	// 2. Check for a constant 0 on the rhs.
	if (sema_cast_const(right))
	{
		switch (right->const_expr.const_kind)
		{
			case CONST_INTEGER:
				if (int_is_zero(right->const_expr.ixx))
				{
					RETURN_SEMA_ERROR(right, "This expression evaluates to zero and division by zero is not allowed.");
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
	if (expr_both_const_foldable(left, right, BINARYOP_DIV))
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
static bool sema_expr_analyse_mod(SemaContext *context, Expr *expr, Expr *left, Expr *right, bool *failed_ref)
{
	// 1. Analyse both sides and promote to a common type
	OperatorOverload overload = OVERLOAD_REMINDER;
	if (!sema_binary_analyse_arithmetic_subexpr(context, expr, "Cannot calculate the reminder %s %% %s",
		false, &overload, failed_ref)) return false;
	if (!overload) return true;

	Type *flat = type_flatten(left->type);
	if (type_is_float(flat))
	{
		// 3. a % 0 is not valid, so detect it.
		if (sema_cast_const(right) && right->const_expr.fxx.f == 0.0)
		{
			RETURN_SEMA_ERROR(right, "Cannot perform %% with a constant zero.");
		}

		// 4. Constant fold
		if (expr_both_const_foldable(left, right, BINARYOP_MOD))
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
		if (expr_both_const_foldable(left, right, BINARYOP_MOD))
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
static bool sema_expr_analyse_bit(SemaContext *context, Expr *expr, Expr *left, Expr *right, OperatorOverload overload, bool *failed_ref)
{
	// 1. Convert to common type if possible.
	if (!sema_binary_analyse_arithmetic_subexpr(context, expr, NULL, true, &overload, failed_ref)) return false;
	if (!overload) return true;

	// 2. Check that both are integers or bools.
	bool is_bool = left->type->canonical == type_bool;
	bool is_bitstruct = left->type->canonical->type_kind == TYPE_BITSTRUCT;

	if (!is_bool && !is_bitstruct && !expr_both_any_integer_or_integer_bool_vector(left, right))
	{
		CHECK_ON_DEFINED(failed_ref);
		return sema_type_error_on_binop(context, expr);
	}

	// 3. Do constant folding if both sides are constant.
	if (expr_both_const_foldable(left, right, BINARYOP_BIT_AND))
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
					UNREACHABLE
			}
		}
		else if (is_bitstruct)
		{
			// Avoid merging with value casts, eg (Bitstruct)1
			if (!expr_is_const_initializer(left) || !expr_is_const_initializer(right)) goto DONE;
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
					UNREACHABLE
			}
		}
	}
DONE:
	// 5. Assign the type
	expr_binary_unify_failability(expr, left, right);
	return true;
}

static bool sema_expr_check_shift_rhs(SemaContext *context, Expr *expr, Expr *left,
                                      Type *left_type_flat, Expr *right, Type *right_type_flat, bool *failed_ref,bool is_assign)
{
	// For a constant rhs side we will make a series of checks.
	// We could extend this by checking vectors
	if (sema_cast_const(right) && expr_is_const_int(right))
	{
		// Make sure the value does not exceed the bitsize of
		// the left hand side. We ignore this check for lhs being a constant.


		Type *base = type_vector_base(left_type_flat);
		ASSERT_SPAN(expr, type_kind_is_any_integer(base->type_kind));
		if (int_ucomp(right->const_expr.ixx, base->builtin.bitsize, BINARYOP_GE))
		{
			RETURN_SEMA_ERROR(right, "The shift is not less than the bitsize of %s.", type_quoted_error_string(type_no_optional(left->type)));
		}

		// Make sure that the RHS is positive.
		if (int_is_neg(right->const_expr.ixx))
		{
			RETURN_SEMA_ERROR(right, "A shift must be a positive number.");
		}
	}

	// If LHS is vector but RHS isn't? Promote.
	bool lhs_is_vec = left_type_flat->type_kind == TYPE_VECTOR;
	if (lhs_is_vec && right_type_flat->type_kind != TYPE_VECTOR)
	{
		// Create a vector from the right hand side.
		Type *right_vec = type_get_vector(right->type, left_type_flat->array.len);
		if (!cast_explicit_checkable(context, right, right_vec, failed_ref)) return false;
	}

	bool rhs_is_vec = right_type_flat->type_kind == TYPE_VECTOR;
	if (!lhs_is_vec && rhs_is_vec)
	{
		if (is_assign)
		{
			if (failed_ref) return *failed_ref = true, false;
			RETURN_SEMA_ERROR(right, "The shift cannot be a vector of type %s when shifting a variable of type %s.",
				left->type, right->type);
		}
		Type *left_vec = type_get_vector(left->type, right_type_flat->array.len);
		if (!cast_explicit_checkable(context, left, left_vec, failed_ref)) return false;

	}
	// Same type is always ok
	Type *right_type = type_no_optional(right->type)->canonical;
	if (type_no_optional(left->type)->canonical == right_type) return true;

	Type *base = right_type;
	if (right_type->type_kind == TYPE_VECTOR)
	{
		base = right_type->array.base->canonical;
		base = type_flat_distinct_enum_inline(base);
		right_type = type_get_vector(base, right_type->array.len);
	}
	else
	{
		base = type_flat_distinct_enum_inline(base);
		right_type = base;
	}
	if (!type_is_integer(base))
	{
		if (failed_ref) return *failed_ref = true, false;
		RETURN_SEMA_ERROR(right, "The right hand shift must be an integer type, or match the left hand side type, but it was %s",
			type_quoted_error_string(right->type), type_quoted_error_string(left->type));
	}
	return cast_implicit_binary(context, right, right_type, failed_ref);
}
/**
 * Analyse >> and << operations.
 * @return true if the analysis succeeded.
 */
static bool sema_expr_analyse_shift(SemaContext *context, Expr *expr, Expr *left, Expr *right, bool *failed_ref)
{
	// 1. Analyze both sides.
	if (!sema_binary_analyse_subexpr(context, left, right)) return false;

	Type *lhs_type = type_no_optional(left->type)->canonical;
	bool shr = expr->binary_expr.operator == BINARYOP_SHR;

	if (type_is_user_defined(lhs_type))
	{
		OperatorOverload overload = shr ? OVERLOAD_SHR : OVERLOAD_SHL;
		if (!sema_replace_with_overload(context, expr, left, right, lhs_type, &overload)) return false;
		if (!overload) return true;
	}

	// 3. Promote lhs using the usual numeric promotion.
	if (!cast_implicit_binary(context, left, cast_numeric_arithmetic_promotion(lhs_type), failed_ref)) return false;

	Type *flat_left = type_flatten_and_inline(left->type);
	Type *flat_right = type_flatten_and_inline(right->type);
	if (flat_left->type_kind == TYPE_VECTOR)
	{
		Type *left_base = type_flatten_and_inline(flat_left->array.base);
		if (!type_is_integer(left_base)) goto FAIL;
	}
	else
	{
		if (!type_is_integer(flat_left)) goto FAIL;
	}

	if (!sema_expr_check_shift_rhs(context, expr, left, flat_left, right, flat_right, failed_ref, false)) return false;

	// Fold constant expressions.
	if (expr_both_const_foldable(left, right, BINARYOP_SHL))
	{
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

	// 6. Set the type
	expr->type = type_add_optional(left->type, IS_OPTIONAL(right));
	return true;
FAIL:
	CHECK_ON_DEFINED(failed_ref);
	return sema_type_error_on_binop(context, expr);
}


static bool sema_expr_analyse_and_or(SemaContext *context, Expr *expr, Expr *left, Expr *right, bool *failed_ref)
{
	if (!sema_binary_analyse_subexpr(context, left, right)) return false;
	if (!cast_explicit_checkable(context, left, type_bool, failed_ref) || !cast_explicit_checkable(context, right, type_bool, failed_ref)) return false;

	if (expr_both_const_foldable(left, right, BINARYOP_AND))
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
	if (expr_is_const_int(left) && type_is_unsigned(rhs_type))
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
	if (!expr_is_const_int(right) || !type_is_unsigned(lhs_type)) return true;
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
static bool sema_expr_analyse_comp(SemaContext *context, Expr *expr, Expr *left, Expr *right, bool *failed_ref)
{
	// 1. Analyse left and right side without any conversions.
	if (!sema_binary_analyse_subexpr(context, left, right)) return false;

	bool is_equality_type_op = expr->binary_expr.operator == BINARYOP_NE || expr->binary_expr.operator == BINARYOP_EQ;

	Type *left_type = type_no_optional(left->type)->canonical;
	Type *right_type = type_no_optional(right->type)->canonical;

	if (is_equality_type_op && (type_is_user_defined(left_type) || type_is_user_defined(right_type)))
	{
		Decl *overload = NULL;
		bool negated_overload = false;
		bool reverse = false;
		switch (expr->binary_expr.operator)
		{
			case BINARYOP_NE:
				overload = sema_find_typed_operator(context, OVERLOAD_NOT_EQUAL, expr->span, left, right, &reverse);
				if (!overload)
				{
					negated_overload = true;
					overload = sema_find_typed_operator(context, OVERLOAD_EQUAL, expr->span, left, right, &reverse);
				}
				if (!decl_ok(overload)) return false;
				if (!overload) goto NEXT;
				break;
			case BINARYOP_EQ:
				overload = sema_find_typed_operator(context, OVERLOAD_EQUAL, expr->span, left, right, &reverse);
				if (!overload)
				{
					negated_overload = true;
					overload = sema_find_typed_operator(context, OVERLOAD_NOT_EQUAL, expr->span, left, right, &reverse);
				}
				if (!decl_ok(overload)) return false;
				if (!overload) goto NEXT;
				break;
			default:
				UNREACHABLE
		}
		Expr **args = NULL;
		if (overload->func_decl.signature.params[1]->type->canonical->type_kind == TYPE_POINTER)
		{
			expr_insert_addr(right);
		}
		vec_add(args, right);
		if (!sema_insert_method_call(context, expr, overload, left, args, reverse)) return false;
		if (!negated_overload) return true;
		assert(expr->resolve_status == RESOLVE_DONE);
		Expr *inner = expr_copy(expr);
		expr->expr_kind = EXPR_UNARY;
		expr->unary_expr = (ExprUnary) { .expr = inner, .operator = UNARYOP_NOT };
		expr->resolve_status = RESOLVE_NOT_DONE;
		return sema_analyse_expr(context, expr);
	}
NEXT:
	// Flatten distinct/optional
	left_type = type_flat_distinct_inline(left_type)->canonical;
	right_type = type_flat_distinct_inline(right_type)->canonical;

	// 2. Handle the case of signed comparisons.
	//    This happens when either side has a definite integer type
	//    and those are either signed or unsigned.
	//    If either side is compint, then this does not happen.
	if ((type_is_unsigned(left_type) && type_is_signed(right_type))
		|| (type_is_signed(left_type) && type_is_unsigned(right_type)))
	{
		// 2a. Resize so that both sides have the same bit width. This will always work.
		cast_to_int_to_max_bit_size(left, right, left_type, right_type);
		goto DONE;
	}


	// 3. In the normal case, treat this as a binary op, finding the max type.
	Type *max = type_find_max_type(left_type, right_type);

	// 4. If no common type, then that's an error:
	if (!max)
	{
		CHECK_ON_DEFINED(failed_ref);
		RETURN_SEMA_ERROR(expr, "%s and %s are different types and cannot be compared.",
						  type_quoted_error_string(left->type), type_quoted_error_string(right->type));
	}

	max = max->canonical;

	if (max->type_kind == TYPE_VECTOR && !is_equality_type_op)
	{
		RETURN_SEMA_ERROR(expr, "Vector types can only be tested for equality, for other comparison, use vector comparison functions.");
	}

	if (max == type_untypedlist)
	{
		RETURN_SEMA_ERROR(expr, "Both sides are untyped and cannot be compared. Please cast one or both sides to a type, e.g. (Foo){ 1, 2 } == { 1, 2 }.");
	}
	if (!type_is_comparable(max))
	{
		CHECK_ON_DEFINED(failed_ref);
		RETURN_SEMA_ERROR(expr, "%s does not support comparisons.",
						  type_quoted_error_string(left->type));
	}

	if (!is_equality_type_op)
	{
		if (!type_is_ordered(max))
		{
			CHECK_ON_DEFINED(failed_ref);
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
	if (!cast_implicit_checked(context, left, max, false, failed_ref) || !cast_implicit_checked(context, right, max, false, failed_ref)) return false;
	bool success = cast_explicit_checkable(context, left, max, failed_ref) && cast_explicit_checkable(context, right, max, failed_ref);
	ASSERT_SPAN(expr, success);


DONE:

	// 7. Do constant folding.
	if (expr_both_const_foldable(left, right, BINARYOP_EQ))
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
			case CONST_BYTES:
			case CONST_STRING:
				expr_rewrite_const_int(expr, type_get_indexed_type(inner->type), inner->const_expr.bytes.len ? inner->const_expr.bytes.ptr[0] : 0);
				return true;
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
			UNREACHABLE
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
	Decl *decl = decl_raw(inner->ident_expr);
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
		case UNRESOLVED_EXPRS:
			UNREACHABLE
		case EXPR_CT_IDENT:
			return "It's not possible to take the address of a compile time value.";
		case EXPR_IDENTIFIER:
			return sema_addr_may_take_of_ident(inner);
		case EXPR_UNARY:
			if (inner->unary_expr.operator == UNARYOP_DEREF) return NULL;
			break;
		case EXPR_ACCESS_RESOLVED:
		{
			Decl *decl = inner->access_resolved_expr.ref;
			switch (decl->decl_kind)
			{
				case DECL_FUNC:
					if (decl->func_decl.attr_interface_method) return NULL;
					return "Taking the address of a method should be done through the type e.g. '&Foo.method' not through the value.";
				case DECL_MACRO:
					return "It's not possible to take the address of a macro.";
				default:
					return sema_addr_check_may_take(inner->access_resolved_expr.parent);
			}
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
		case EXPR_ACCESS_RESOLVED:
		case EXPR_ACCESS_UNRESOLVED:
		{
			Expr *parent = inner->access_resolved_expr.parent;
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

	Type *no_optional = type_no_optional(inner->type)->canonical;
	// 3. Get the pointer of the underlying type.
	if (no_optional->type_kind == TYPE_FUNC_RAW)
	{
		expr->type = type_add_optional(type_get_func_ptr(no_optional), IS_OPTIONAL((inner)));
	}
	else
	{
		expr->type = type_get_ptr_recurse(inner->type);
	}
	if (inner->expr_kind == EXPR_IDENTIFIER)
	{
		Decl *ident = inner->ident_expr;
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
	Type *canonical = no_fail->canonical;

	// Check for overload
	if (type_is_user_defined(canonical))
	{
		Decl *overload = sema_find_untyped_operator(context, canonical, OVERLOAD_UNARY_MINUS, NULL);
		if (overload)
		{
			// Plus just returns inner
			if (is_plus)
			{
				expr_replace(expr, inner);
				return true;
			}
			return sema_insert_method_call(context, expr, overload, inner, NULL, false);
		}
	}
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
	if (!expr_const_foldable_unary(inner, UNARYOP_NEG))
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
static inline bool sema_expr_analyse_bit_not(SemaContext *context, Expr *expr, bool *failed_ref)
{
	// 1. Analyse the inner expression.
	Expr *inner = expr->unary_expr.expr;
	if (!sema_analyse_expr(context, inner)) return false;

	// 2. Check that it's a vector, bool
	Type *canonical = type_no_optional(inner->type)->canonical;

	if (type_is_user_defined(canonical) && canonical->type_kind != TYPE_BITSTRUCT)
	{
		Decl *overload = sema_find_untyped_operator(context, canonical, OVERLOAD_NEGATE, NULL);
		if (overload) return sema_insert_method_call(context, expr, overload, inner, NULL, false);
	}

	Type *flat = type_flatten(canonical);
	bool is_bitstruct = flat->type_kind == TYPE_BITSTRUCT;
	if (!type_is_integer_or_bool_kind(flat) && !is_bitstruct)
	{
		Type *vector_type = type_vector_type(canonical);
		if (vector_type && (type_is_integer(vector_type) || vector_type == type_bool)) goto VALID_VEC;
		CHECK_ON_DEFINED(failed_ref);
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
	if (!cast_implicit_checked(context, inner, result_type, false, failed_ref)) return false;

	// 3. The simple case, non-const.
	if (!expr_const_foldable_unary(inner, UNARYOP_BITNEG))
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
				UNREACHABLE
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
		ASSERT_SPAN(expr, expr_const_foldable_unary(inner, UNARYOP_NOT));
		ASSERT_SPAN(expr, inner->const_expr.const_kind == CONST_BOOL);
		expr->const_expr.const_kind = CONST_BOOL;
		expr->expr_kind = EXPR_CONST;
		expr->resolve_status = RESOLVE_DONE;
		expr->const_expr.b = !inner->const_expr.b;
		return true;
	}

	return true;
}

static inline bool sema_expr_analyse_ct_subscript_incdec(SemaContext *context, Expr *expr, Expr *inner)
{
	Decl *ct_var = inner->ct_subscript_expr.var;
	Expr *init = ct_var->var.init_expr;
	ArrayIndex index = inner->ct_subscript_expr.index;
	bool post = expr->expr_kind == EXPR_POST_UNARY;
	bool dec = expr->unary_expr.operator == UNARYOP_DEC;
	ASSIGN_EXPR_OR_RET(Expr *value, expr_from_const_expr_at_index(init, index), false);
	if (!expr_is_const_int(value))
	{
		RETURN_SEMA_ERROR(expr, "The indexed type is not an integer.");
	}
	if (post)
	{
		expr_replace(expr, copy_expr_single(value));
	}
	if (dec)
	{
		value->const_expr.ixx = int_sub64(value->const_expr.ixx, 1);
	}
	else
	{
		value->const_expr.ixx = int_add64(value->const_expr.ixx, 1);
	}
	if (!post)
	{
		expr_replace(expr, value);
	}
	return sema_expr_analyse_ct_subscript_set_value(context, inner, ct_var, value);
}

static inline bool sema_expr_analyse_ct_incdec(SemaContext *context, Expr *expr, Expr *inner)
{
	ASSERT_SPAN(expr, inner->expr_kind == EXPR_CT_IDENT);

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
	Decl *operator = sema_find_untyped_operator(context, type_check, OVERLOAD_ELEMENT_REF, NULL);
	Expr **args = NULL;
	// The simple case: we have &[] so just replace it by that.
	if (operator)
	{
		vec_add(args, exprptr(subscript_expr->subscript_assign_expr.index));
		if (!sema_insert_method_call(context, subscript_expr, operator, exprptr(subscript_expr->subscript_assign_expr.expr), args, false)) return false;
		if (!sema_expr_rewrite_insert_deref(context, subscript_expr)) return false;
		main->type = subscript_expr->type;
		return true;
	}
	// We need []= and [] now.
	operator = sema_find_untyped_operator(context, type_check, OVERLOAD_ELEMENT_AT, NULL);
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
	Expr *index = exprptr(subscript_expr->subscript_assign_expr.index);
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
	Expr *get_expr = expr_new(EXPR_ACCESS_RESOLVED, increased->span);
	vec_add(args, expr_variable(index_val));
	Expr *temp_val_1 = expr_variable(temp_val);
	if (!sema_expr_rewrite_insert_deref(context, temp_val_1)) return false;
	if (!sema_insert_method_call(context, get_expr, operator, temp_val_1, args, false)) return false;
	Expr *value_val_expr = expr_generate_decl(value_val, get_expr);
	// temp_value = func(temp, temp_index)
	vec_add(main->expression_list, value_val_expr);
	// temp_result = temp_value++, temp_result *= temp_value etc
	vec_add(main->expression_list, expr_generate_decl(result_val, mutate));

	args = NULL;
	vec_add(args, expr_variable(index_val));
	vec_add(args, expr_variable(value_val));
	Expr *temp_val_2 = expr_variable(temp_val);
	if (!sema_expr_rewrite_insert_deref(context, temp_val_2)) return false;
	if (!sema_insert_method_call(context, subscript_expr, declptr(subscript_expr->subscript_assign_expr.method), temp_val_2, args, false)) return false;
	ASSERT(subscript_expr->expr_kind == EXPR_CALL);
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
	if (!sema_analyse_expr_lvalue(context, inner, NULL)) return false;

	// 2. Assert it's an l-value
	if (!sema_expr_check_assign(context, inner, NULL)) return false;

	// 3. This might be a $foo, if to handle it.
	if (inner->expr_kind == EXPR_CT_IDENT)
	{
		return sema_expr_analyse_ct_incdec(context, expr, inner);
	}

	if (inner->expr_kind == EXPR_CT_SUBSCRIPT)
	{
		return sema_expr_analyse_ct_subscript_incdec(context, expr, inner);
	}

	// 4. Flatten typedef, enum, distinct, optional
	Type *type = type_flatten(inner->type);

	// 5. We can only inc/dec numbers or pointers.
	if (!type_underlying_may_add_sub(type) && type->type_kind != TYPE_VECTOR)
	{
		RETURN_SEMA_ERROR(inner, "The expression must be a vector, enum, number or a pointer.");
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

static inline bool sema_expr_analyse_or_error(SemaContext *context, Expr *expr, Expr *left, Expr *right, Type *infer_type, bool *failed_ref)
{
	bool lhs_is_embed = left->expr_kind == EXPR_EMBED;
	if (expr_is_ungrouped_ternary(left) || expr_is_ungrouped_ternary(right))
	{
		RETURN_SEMA_ERROR(expr, "Unclear precedence using ternary with ??, please use () to remove ambiguity.");
	}
	if (lhs_is_embed)
	{
		if (!sema_expr_analyse_embed(context, left, true)) return false;
	}
	else
	{
		if (!sema_analyse_inferred_expr(context, infer_type, left)) return false;
	}

	Type *type = left->type;
	if (!type_is_optional(type))
	{
		if (lhs_is_embed)
		{
			expr_replace(expr, left);
			return true;
		}
		CHECK_ON_DEFINED(failed_ref);
		RETURN_SEMA_ERROR(left, "No optional to use '\?\?' with, please remove the '\?\?'.");
	}

	EndJump active_scope_jump = context->active_scope.end_jump;

	// First we analyse the "else" and try to implictly cast.
	if (!sema_analyse_inferred_expr(context, infer_type, right)) return false;

	if (left->expr_kind == EXPR_OPTIONAL)
	{
		expr_replace(expr, right);
		return true;
	}

	// Ignore the jump here.
	context->active_scope.end_jump = active_scope_jump;

	// Here we might need to insert casts.
	Type *else_type = right->type;


	// Remove any possible optional of the else type.
	bool add_optional = type_is_optional(else_type);
	type = type_no_optional(type);
	else_type = type_no_optional(else_type);
	Type *common = type_find_max_type(type, else_type);
	if (!common)
	{
		CHECK_ON_DEFINED(failed_ref);
		if (else_type == type_fault)
		{
			RETURN_SEMA_ERROR(right, "There is no common type for %s and %s, did you perhaps forget a '?' after the last expression?", type_quoted_error_string(type), type_quoted_error_string(else_type));
		}
		RETURN_SEMA_ERROR(right, "Cannot find a common type for %s and %s.", type_quoted_error_string(type), type_quoted_error_string(else_type));
	}
	if (!cast_both_implicit(context, left, right, common, false, failed_ref)) return false;
	expr->type = type_add_optional(common, add_optional);
	return true;
}

static inline bool sema_expr_analyse_ct_and_or(SemaContext *context, Expr *expr, Expr *left, Expr *right)
{
	ASSERT_SPAN(expr, expr->resolve_status == RESOLVE_RUNNING);
	bool is_and = expr->binary_expr.operator == BINARYOP_CT_AND;
	if (!sema_analyse_expr(context, left)) return false;
	if (!sema_cast_const(left) || !expr_is_const_bool(left)) RETURN_SEMA_ERROR(left, "Expected this to evaluate to a constant boolean.");
	if (left->const_expr.b != is_and)
	{
		expr_rewrite_const_bool(expr, type_bool, !is_and);
		return true;
	}
	if (!sema_analyse_expr(context, right)) return false;
	if (sema_cast_const(right) && expr_is_const_bool(right))
	{
		expr_rewrite_const_bool(expr, type_bool, right->const_expr.b);
		return true;
	}
	if (!cast_implicit(context, right, type_bool, false)) return false;
	expr_replace(expr, right);
	return true;
}

static inline bool sema_expr_analyse_binary(SemaContext *context, Type *infer_type, Expr *expr, bool *failed_ref)
{
	ASSERT_SPAN(expr, expr->resolve_status == RESOLVE_RUNNING);
	Expr *left = exprptr(expr->binary_expr.left);
	Expr *right = exprptr(expr->binary_expr.right);
	// check if both sides have a binary operation where the precedence is unclear. Example: a ^ b | c
	if (sema_binary_check_unclear_op_precedence(left, expr, right))
	{
		RETURN_SEMA_ERROR(expr, "You need to add explicit parentheses to clarify precedence.");
	}
	BinaryOp operator = expr->binary_expr.operator;
	if (operator >= BINARYOP_ASSIGN)
	{
		if (left->expr_kind != EXPR_TYPEINFO)
		{
			if (!sema_analyse_expr_lvalue(context, left, NULL)) return false;
		}
	}
	else
	{
		if (operator == BINARYOP_ELSE) return sema_expr_analyse_or_error(context, expr, left, right, infer_type, failed_ref);
		if (!sema_binary_analyse_with_inference(context, left, right, operator)) return false;
	}
	switch (operator)
	{
		case BINARYOP_ELSE:
			UNREACHABLE
		case BINARYOP_CT_CONCAT:
			return sema_expr_analyse_ct_concat(context, expr, left, right, failed_ref);
		case BINARYOP_CT_OR:
		case BINARYOP_CT_AND:
			return sema_expr_analyse_ct_and_or(context, expr, left, right);
		case BINARYOP_ASSIGN:
			return sema_expr_analyse_assign(context, expr, left, right, failed_ref);
		case BINARYOP_MULT:
			return sema_expr_analyse_mult(context, expr, left, right, failed_ref);
		case BINARYOP_ADD:
			return sema_expr_analyse_add(context, expr, left, right, failed_ref);
		case BINARYOP_SUB:
			return sema_expr_analyse_sub(context, expr, left, right, failed_ref);
		case BINARYOP_DIV:
			return sema_expr_analyse_div(context, expr, left, right, failed_ref);
		case BINARYOP_MOD:
			return sema_expr_analyse_mod(context, expr, left, right, failed_ref);
		case BINARYOP_AND:
		case BINARYOP_OR:
			return sema_expr_analyse_and_or(context, expr, left, right, failed_ref);
		case BINARYOP_BIT_OR:
			return sema_expr_analyse_bit(context, expr, left, right, OVERLOAD_OR, failed_ref);
		case BINARYOP_BIT_XOR:
			return sema_expr_analyse_bit(context, expr, left, right, OVERLOAD_XOR, failed_ref);
		case BINARYOP_BIT_AND:
			return sema_expr_analyse_bit(context, expr, left, right, OVERLOAD_AND, failed_ref);
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
			return sema_expr_analyse_comp(context, expr, left, right, failed_ref);
		case BINARYOP_SHR:
		case BINARYOP_SHL:
			return sema_expr_analyse_shift(context, expr, left, right, failed_ref);
		case BINARYOP_ADD_ASSIGN:
		case BINARYOP_SUB_ASSIGN:
		case BINARYOP_BIT_AND_ASSIGN:
		case BINARYOP_BIT_OR_ASSIGN:
		case BINARYOP_BIT_XOR_ASSIGN:
		case BINARYOP_MOD_ASSIGN:
		case BINARYOP_MULT_ASSIGN:
		case BINARYOP_DIV_ASSIGN:
		case BINARYOP_SHR_ASSIGN:
		case BINARYOP_SHL_ASSIGN:
			return sema_expr_analyse_op_assign(context, expr, left, right, operator);
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
			return sema_expr_analyse_bit_not(context, expr, failed_ref);
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


static inline bool sema_expr_analyse_rethrow(SemaContext *context, Expr *expr, Type *to)
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

	if (context->active_scope.flags & SCOPE_MACRO)
	{
		TypeInfoId rtype = context->current_macro->func_decl.signature.rtype;
		if (rtype && !type_is_optional(typeget(rtype)))
		{
			RETURN_SEMA_ERROR(expr, "Rethrow is only allowed in macros with an optional or inferred return type. "
									"Did you mean to use '!!' instead?");
		}
		vec_add(context->block_returns, NULL);
		expr->rethrow_expr.in_block = context->block_exit_ref;
		expr->rethrow_expr.cleanup = context_get_defers(context, context->block_return_defer, false);
	}
	else
	{
		expr->rethrow_expr.cleanup = context_get_defers(context, 0, false);
		expr->rethrow_expr.in_block = NULL;
		if (context->rtype && context->rtype->type_kind != TYPE_OPTIONAL)
		{
			// Sometimes people write "int? foo = bar()!;" which is likely a mistake.
			if (to && type_is_optional(to))
			{
				RETURN_SEMA_ERROR(expr, "This expression is doing a rethrow, "
										"but '%s' returns %s, which isn't an optional type. Since you are assigning to "
				                        "an optional, maybe you added '!' by mistake?",
										context->call_env.current_function->name,
										type_quoted_error_string(context->rtype));
			}
			RETURN_SEMA_ERROR(expr, "This expression is doing a rethrow, "
									"but '%s' returns %s, which isn't an optional type. Did you intend to use '!!' instead?",
									context->call_env.current_function->name,
									type_quoted_error_string(context->rtype));
		}
		if (context->call_env.is_naked_fn)
		{
			RETURN_SEMA_ERROR(expr, "Rethrow is not allowed in a '@naked' function.");
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

	if (type != type_fault)
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
			current = current->ident_expr->var.init_expr;
			goto RETRY;
		case EXPR_IDENTIFIER:
			decl = current->ident_expr;
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
	if (field->expr_kind != EXPR_UNRESOLVED_IDENTIFIER) RETURN_SEMA_ERROR(field, "Expected an identifier here.");
	const char *kw = field->unresolved_ident_expr.ident;
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
			case DECL_ALIAS:
			case DECL_ALIAS_PATH:
			case DECL_ATTRIBUTE:
			case DECL_BODYPARAM:
			case DECL_CT_ASSERT:
			case DECL_CT_ECHO:
			case DECL_CT_EXEC:
			case DECL_CT_INCLUDE:
			case DECL_DECLARRAY:
			case DECL_ERASED:
			case DECL_GROUP:
			case DECL_IMPORT:
			case DECL_LABEL:
			case DECL_MACRO:
			case DECL_POISONED:
				RETURN_SEMA_ERROR(main_var, "'%s' does not have an external name.", decl->name);
			case DECL_FAULT:
				goto RETURN_CT;
			case DECL_BITSTRUCT:
			case DECL_DISTINCT:
			case DECL_ENUM:
			case DECL_CONST_ENUM:
			case DECL_ENUM_CONSTANT:
			case DECL_FNTYPE:
			case DECL_FUNC:
			case DECL_INTERFACE:
			case DECL_STRUCT:
			case DECL_TYPEDEF:
			case DECL_UNION:
				// TODO verify that all of these are correct
				goto RETURN_CT; // NOLINT
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
		{
			TypeInfo *base = type_info->generic.base;
			if (base->kind == TYPE_INFO_IDENTIFIER)
			{
				if (!sema_parameterized_type_is_found(context, base->unresolved.path, base->unresolved.name, type_info->span)) return NULL;
			}
			if (!sema_resolve_type_info(context, type_info, RESOLVE_TYPE_DEFAULT)) return poisoned_type;
			return type_info->type;
		}
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
			if (!expr_ok(expr)) return poisoned_type;
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


static inline bool sema_may_reuse_lambda(Decl *lambda, Type **types)
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
	ASSERT(vec_size(candidate->func_decl.lambda_ct_parameters) == param_count);
	if (!param_count) return true;
	FOREACH_IDX(i, Decl *, param, candidate->func_decl.lambda_ct_parameters)
	{
		Decl *ct_param = ct_lambda_params[i];
		if (!param->var.is_read) continue;
		ASSERT(ct_param->resolve_status == RESOLVE_DONE || param->resolve_status == RESOLVE_DONE);
		ASSERT(ct_param->var.kind == param->var.kind);
		switch (ct_param->var.kind)
		{
			case VARDECL_LOCAL_CT_TYPE:
			case VARDECL_PARAM_CT_TYPE:
				if (ct_param->var.init_expr->const_expr.typeid->canonical !=
					param->var.init_expr->const_expr.typeid->canonical)
					return false;
				break;
			case VARDECL_LOCAL_CT:
			case VARDECL_PARAM_CT:
				if (!expr_is_const(ct_param->var.init_expr)) return false;
				if (!expr_is_const(param->var.init_expr)) return false;
				if (!expr_both_const_foldable(ct_param->var.init_expr, param->var.init_expr, BINARYOP_EQ)) return false;
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
		ASSERT(i < 198);
		types[i + 1] = type;
	}

	FOREACH(Decl *, candidate, original->func_decl.generated_lambda)
	{
		if (sema_may_reuse_lambda(candidate, types) &&
			lambda_parameter_match(ct_lambda_parameters, candidate))
			return candidate;
	}
	return NULL;
}

static inline bool sema_expr_analyse_embed(SemaContext *context, Expr *expr, bool allow_fail)
{
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
	if (!filename->const_expr.bytes.len) RETURN_SEMA_ERROR(filename, "Expected a non-empty string.");
	CompilationUnit *unit = context->unit;
	const char *string = filename->const_expr.bytes.ptr;
	char *path;
	char *name;
	if (file_path_is_relative(string) && file_namesplit(unit->file->full_path, &name, &path))
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
			Decl *io_error = module ? module_find_symbol(module, kw_FILE_NOT_FOUND) : NULL;
			Decl *fault = poisoned_decl;
			if (io_error && io_error->decl_kind == DECL_FAULT)
			{
				fault = io_error;
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
		filename->const_expr.const_kind = CONST_FAULT;
		expr->type = type_wildcard_optional;
		filename->const_expr.fault = compiler.context.io_error_file_not_found;
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
	if (parent->expr_kind != EXPR_UNRESOLVED_IDENTIFIER)
	{
		RETURN_SEMA_ERROR(parent, "Expected an identifier to parameterize.");
	}
	Decl *symbol = sema_analyse_parameterized_identifier(context, parent->unresolved_ident_expr.path,
														 parent->unresolved_ident_expr.ident, parent->span,
														 expr->generic_ident_expr.parmeters, NULL);
	if (!decl_ok(symbol)) return false;
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
	if (sig->variadic == VARIADIC_RAW)
	{
		if (to_sig)
		{
			if (to_sig->variadic == VARIADIC_RAW)
			{
				RETURN_SEMA_ERROR(decl, "A lambda may not use C-style vaargs. Consequently it can never implement the %s function type.", type_quoted_error_string(flat));
			}
			RETURN_SEMA_ERROR(expr, "The lambda doesn't match the required type %s.", type_quoted_error_string(target_type));
		}
		RETURN_SEMA_ERROR(decl, "A lambda may not use C-style vaargs.");
	}
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
	bool erase_decl = false;
	if (!sema_analyse_func_macro(context, decl, ATTR_FUNC, &erase_decl)) return false;
	if (erase_decl)
	{
		RETURN_SEMA_ERROR(decl, "`@if` can't be placed on a lambda.");
	}
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
		// Because we cannot check if the parameter is used before everything, set them all as read.
		FOREACH(Decl *, decl, ct_lambda_parameters)
		{
			decl->var.is_read = true;
		}
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
	if (inner->expr_kind != EXPR_UNRESOLVED_IDENTIFIER) goto ERROR;
	if (!inner->unresolved_ident_expr.is_const) goto ERROR;

	const char *name = inner->unresolved_ident_expr.ident;
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
				active_context = main_expr->expr_other_context.context;
				in_no_eval = active_context->call_env.in_no_eval;
				active_context->call_env.in_no_eval = true;
				main_expr = main_expr->expr_other_context.inner;
				goto RETRY;
			case EXPR_ACCESS_UNRESOLVED:
				if (!sema_expr_analyse_access(active_context, main_expr, &failed, CHECK_VALUE, false))
				{
					if (!failed) goto FAIL;
					success = false;
				}
				break;
			case EXPR_UNRESOLVED_IDENTIFIER:
			{
				Decl *decl = sema_find_path_symbol(active_context, main_expr->unresolved_ident_expr.ident, main_expr->unresolved_ident_expr.path);
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
			{
				Expr *eval = sema_ct_eval_expr(active_context, "$eval", main_expr->inner_expr, false);
				if (!expr_ok(eval)) return false;
				success = eval != NULL;
				break;
			}
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
			case EXPR_CATCH_UNRESOLVED:
			case EXPR_CATCH:
			case EXPR_COND:
			case EXPR_TEST_HOOK:
			case EXPR_DESIGNATOR:
			case EXPR_BENCHMARK_HOOK:
			case EXPR_TRY_UNRESOLVED:
			case EXPR_TRY:
			case EXPR_TRY_UNWRAP_CHAIN:
			case EXPR_OPERATOR_CHARS:
			case EXPR_MACRO_BODY_EXPANSION:
			case EXPR_BUILTIN_ACCESS:
			case EXPR_DECL:
			case EXPR_LAST_FAULT:
			case EXPR_DEFAULT_ARG:
			case EXPR_IDENTIFIER:
			case EXPR_NAMED_ARGUMENT:
			case EXPR_ACCESS_RESOLVED:
			case EXPR_CT_SUBSCRIPT:
			case EXPR_IOTA_DECL:
				UNREACHABLE
			case EXPR_BINARY:
				main_expr->resolve_status = RESOLVE_RUNNING;
				if (!sema_expr_analyse_binary(active_context, NULL, main_expr, &failed))
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
			case EXPR_CT_IS_CONST:
			case EXPR_CT_DEFINED:
			case EXPR_STRINGIFY:
			case EXPR_TERNARY:
			case EXPR_CT_ASSIGNABLE:
			case EXPR_CT_CALL:
			case EXPR_EXPRESSION_LIST:
			case EXPR_POST_UNARY:
			case EXPR_TYPEID:
			case EXPR_TYPEID_INFO:
			case EXPR_TYPECALL:
			case EXPR_MEMBER_GET:
			case EXPR_MEMBER_SET:
			case EXPR_SPLAT:
			case EXPR_EXT_TRUNC:
			case EXPR_INT_TO_BOOL:
			case EXPR_VECTOR_TO_ARRAY:
			case EXPR_SLICE_TO_VEC_ARRAY:
			case EXPR_SCALAR_TO_VECTOR:
			case EXPR_PTR_ACCESS:
			case EXPR_ENUM_FROM_ORD:
			case EXPR_SLICE_LEN:
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
			case EXPR_TWO:
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
		case TOKEN_CT_VATYPE:
		default:
			UNREACHABLE
	}
}

static inline bool sema_expr_analyse_iota_decl(SemaContext *context, Expr *expr)
{
	Decl *decl = expr->iota_decl_expr;
	if (!decl_ok(decl)) return false;
	if (decl->resolve_status != RESOLVE_DONE)
	{
		RETURN_SEMA_ERROR(expr, "Enum constants values should never need to be resolved out of order.");
	}
	Expr *iota_expr = decl->enum_constant.value;
	assert(expr_is_const_int(iota_expr));

	Int value = iota_expr->const_expr.ixx;
	Int add = int_add64(value, 1);
	if (int_comp(add, value, BINARYOP_LT))
	{
		RETURN_SEMA_ERROR(expr, "Enum value would overflow the maximum value of the container type %s.", type_quoted_error_string(type_flatten(decl->type)));
	}
	expr_rewrite_const_integer(expr, iota_expr->type, add.i);
	return true;
}

static inline bool sema_expr_analyse_assignable(SemaContext *context, Expr *expr)
{
	SEMA_DEPRECATED(expr, "$assignable is deprecated, use the '@assignable_to' macro instead.");
	ASSERT_SPAN(expr, expr->resolve_status == RESOLVE_RUNNING);
	Expr *type_expr = exprptr(expr->assignable_expr.type);
	bool in_no_eval = context->call_env.in_no_eval;
	context->call_env.in_no_eval = true;
	Type *type;
	if (type_expr->expr_kind == EXPR_TYPEINFO)
	{
		TypeInfo *type_info = type_expr->type_expr;
		if (!sema_resolve_type_info(context, type_info, RESOLVE_TYPE_ALLOW_INFER)) goto FAILED;
		type = type_info->type;
	}
	else
	{
		if (!sema_analyse_expr_value(context, type_expr)) goto FAILED;
		switch (type_expr->expr_kind)
		{
			case EXPR_TYPEINFO:
				ASSERT_SPAN(expr, type_expr->type_expr->resolve_status == RESOLVE_DONE);
				type = type_expr->type_expr->type;
				break;
			case EXPR_CONST:
				if (type_expr->const_expr.const_kind == CONST_TYPEID)
				{
					type = type_expr->const_expr.typeid;
					break;
				}
				type = NULL;
				break;
			default:
				type = NULL;
				break;
		}
	}
	if (!type) RETURN_SEMA_ERROR(type_expr, "Expected a type or constant typeid here.");
	Expr *inner = exprptr(expr->assignable_expr.expr);
	if (!sema_analyse_inferred_expr(context, type, inner)) goto FAILED;
	bool ok = may_cast(context, inner, type, false, true);
	expr_rewrite_const_bool(expr, type_bool, ok);
	context->call_env.in_no_eval = in_no_eval;
	return true;
FAILED:
	context->call_env.in_no_eval = in_no_eval;
	return false;
}


static inline bool sema_expr_analyse_ct_stringify(SemaContext *context, Expr *expr)
{
	Expr *inner = expr->inner_expr;
	// Only hash ident style stringify reaches here.
	while (true)
	{
		switch (inner->expr_kind)
		{
			case EXPR_CT_ARG:
			{
				ASSIGN_EXPR_OR_RET(Expr *arg_expr, sema_expr_analyse_ct_arg_index(context, exprptr(inner->ct_arg_expr.arg), NULL), false);
				inner = arg_expr;
				continue;
			}
			case EXPR_OTHER_CONTEXT:
				context = inner->expr_other_context.context;
				inner = inner->expr_other_context.inner;
				continue;
			case EXPR_HASH_IDENT:
			{
				Decl *decl = sema_resolve_symbol(context, inner->ct_ident_expr.identifier, NULL, inner->span);
				if (!decl) return false;
				inner = decl->var.init_expr;
				continue;
			}
			default:
				break;
		}
		break;
	}
	const char *desc = span_to_string(inner->span);
	if (!desc)
	{
		RETURN_SEMA_ERROR(expr, "Failed to stringify hash variable contents - they must be a single line and not exceed 255 characters.");
	}
	expr_rewrite_const_string(expr, desc);
	return true;
}

static inline bool sema_expr_resolve_ct_eval(SemaContext *context, Expr *expr)
{
	Expr *result = sema_ct_eval_expr(context, false, expr->inner_expr, true);
	if (!result) return false;
	if (result->expr_kind == EXPR_TYPEINFO)
	{
		RETURN_SEMA_ERROR(result, "Evaluation to a type requires the use of '$typefrom' rather than '$eval'.");
	}
	expr_replace(expr, result);
	return true;
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
			offset += (ByteSize)(type_size(result_type) * index);
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
		RETURN_SEMA_ERROR(type_info, "The type here should always be written as a plain type and not an optional, please remove the '?'.");
	}
	if (!sema_resolve_type_structure(context, type)) return false;
	if (!sema_expr_analyse_initializer_list(context, type, expr->expr_compound_literal.initializer)) return false;
	expr_replace(expr, expr->expr_compound_literal.initializer);
	return true;
}


static inline bool sema_analyse_expr_dispatch(SemaContext *context, Expr *expr, CheckType check)
{
	switch (expr->expr_kind)
	{
		case EXPR_ASM:
		case EXPR_BENCHMARK_HOOK:
		case EXPR_CATCH_UNRESOLVED:
		case EXPR_CATCH:
		case EXPR_COND:
		case EXPR_DEFAULT_ARG:
		case EXPR_DESIGNATOR:
		case EXPR_MACRO_BODY:
		case EXPR_MACRO_BODY_EXPANSION:
		case EXPR_MEMBER_GET:
		case EXPR_MEMBER_SET:
		case EXPR_NAMED_ARGUMENT:
		case EXPR_NOP:
		case EXPR_OPERATOR_CHARS:
		case EXPR_SWIZZLE:
		case EXPR_TEST_HOOK:
		case EXPR_TRY_UNRESOLVED:
		case EXPR_TRY:
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
		case EXPR_CT_SUBSCRIPT:
			UNREACHABLE
		case EXPR_IOTA_DECL:
			return sema_expr_analyse_iota_decl(context, expr);
		case EXPR_TWO:
			if (!sema_analyse_expr(context, expr->two_expr.first)) return false;
			if (!sema_analyse_expr_check(context, expr->two_expr.last, check)) return false;
			expr->type = expr->two_expr.last->type;
			return true;
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
			SemaContext *new_context = expr->expr_other_context.context;
			expr_replace(expr, expr->expr_other_context.inner);
			if (expr->resolve_status == RESOLVE_DONE) return expr_ok(expr);
			ContextSwitchState state = context_switch_state_push(context, new_context);
			expr->resolve_status = RESOLVE_RUNNING;
			bool success = sema_analyse_expr_dispatch(new_context, expr, check);
			context_switch_stat_pop(new_context, state);
			return success;
		}
		case EXPR_CT_ASSIGNABLE:
			return sema_expr_analyse_assignable(context, expr);
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
		case EXPR_CT_ARG:
			return sema_expr_analyse_ct_arg(context, NULL, expr);
		case EXPR_STRINGIFY:
			if (!sema_expr_analyse_ct_stringify(context, expr)) return false;
			return true;
		case EXPR_DECL:
		{
			Decl *decl = expr->decl_expr;
			bool erase = decl->var.kind == VARDECL_LOCAL_CT_TYPE || decl->var.kind == VARDECL_LOCAL_CT;
			if (!sema_analyse_var_decl(context, decl, true)) return false;
			if (decl->decl_kind == DECL_ERASED)
			{
				expr->expr_kind = EXPR_NOP;
				expr->type = type_void;
				return true;
			}
			if (erase)
			{
				Expr *init = decl->var.init_expr;
				if (init)
				{
					expr_replace(expr, copy_expr_single(decl->var.init_expr));
					return true;
				}
				expr->expr_kind = EXPR_NOP;
				expr->type = type_void;
				return true;
			}
			expr->type = decl->type;
			return true;
		}
		case EXPR_LAST_FAULT:
			expr->type = type_fault;
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
			return sema_expr_analyse_ct_identifier(context, expr);
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
		case EXPR_RETHROW:
			return sema_expr_analyse_rethrow(context, expr, NULL);
		case EXPR_CONST:
			return true;
		case EXPR_CT_EVAL:
			if (!sema_expr_resolve_ct_eval(context, expr)) return false;
			return sema_analyse_expr_dispatch(context, expr, check);
		case EXPR_BINARY:
			return sema_expr_analyse_binary(context, NULL, expr, NULL);
		case EXPR_TERNARY:
			return sema_expr_analyse_ternary(context, NULL, expr);
		case EXPR_UNARY:
		case EXPR_POST_UNARY:
			return sema_expr_analyse_unary(context, expr, NULL, check);
		case EXPR_TYPEID:
			return sema_expr_analyse_typeid(context, expr);
		case EXPR_IDENTIFIER:
			UNREACHABLE
		case EXPR_UNRESOLVED_IDENTIFIER:
			return sema_expr_analyse_identifier(context, NULL, expr);
		case EXPR_CALL:
			return sema_expr_analyse_call(context, expr, NULL);
		case EXPR_SUBSCRIPT:
		case EXPR_SUBSCRIPT_ADDR:
			return sema_expr_analyse_subscript(context, expr, check, false);
		case EXPR_BITACCESS:
		case EXPR_SUBSCRIPT_ASSIGN:
		case EXPR_ACCESS_RESOLVED:
			UNREACHABLE
		case EXPR_ACCESS_UNRESOLVED:
			return sema_expr_analyse_access(context, expr, NULL, check, false);
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
	if (sema_cast_const(expr) && expr_is_const_bool(expr))
	{
		*result = expr->const_expr.b ? COND_TRUE : COND_FALSE;
	}
	return true;
}


bool sema_analyse_expr_rhs(SemaContext *context, Type *to, Expr *expr, bool allow_optional, bool *no_match_ref,
						   bool as_binary)
{
	if (expr->expr_kind == EXPR_EMBED && allow_optional)
	{
		if (!sema_expr_analyse_embed(context, expr, true)) return false;
	}
	else
	{
		if (!sema_analyse_inferred_expr(context, to, expr)) return false;
	}
	if (!sema_cast_rvalue(context, expr, true)) return false;
	if (to) to = type_no_optional(to);
	Type *to_canonical = to ? type_no_optional(to)->canonical : NULL;
	Type *rhs_type = expr->type;
	Type *rhs_type_canonical = rhs_type->canonical;
	// Let's have a better error on `return io::FILE_NOT_FOUND;` when the return type is not fault.
	if (to && allow_optional && to_canonical != rhs_type_canonical && rhs_type_canonical == type_fault)
	{
		Type *flat = type_flatten(to);
		if (flat != type_fault && sema_cast_const(expr))
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
		cast_no_check(expr, type_get_ptr(type_get_array(element, len)), IS_OPTIONAL(expr));
		// Deref
		if (!sema_expr_rewrite_insert_deref(context, expr)) return false;
		cast_no_check(expr, to, IS_OPTIONAL(expr));
		return true;
	}
	NO_SLICE:;
	if (to)
	{
		if (as_binary)
		{
			if (!cast_implicit_binary(context, expr, to, no_match_ref)) return false;
		}
		else
		{
			if (!cast_implicit_checked(context, expr, to, false, no_match_ref)) return false;
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
		case EXPR_MEMBER_SET:
			RETURN_SEMA_ERROR(expr, "Expected two parameters to 'set', e.g. '$member.set(v, new_value)'.");
		case EXPR_MEMBER_GET:
			RETURN_SEMA_ERROR(expr, "Expected a parameter to 'get', e.g. '$member.get(v)'.");
		case EXPR_MACRO_BODY_EXPANSION:
			if (!expr->body_expansion_expr.first_stmt)
			{
				RETURN_SEMA_ERROR(expr, "'@%s' must be followed by ().", declptr(context->current_macro->func_decl.body_param)->name); // NOLINT
			}
			break;
		case EXPR_TYPECALL:
			RETURN_SEMA_ERROR(expr, "A tag name must be given.");
		case EXPR_BUILTIN:
			RETURN_SEMA_ERROR(expr, "A builtin must be followed by ().");
		case EXPR_ACCESS_RESOLVED:
			if (expr->access_resolved_expr.ref->decl_kind == DECL_FUNC)
			{
				RETURN_SEMA_ERROR(expr, "A function name must be followed by '(' or preceded by '&'.");
			}
			if (expr->access_resolved_expr.ref->decl_kind == DECL_MACRO)
			{
				RETURN_SEMA_ERROR(expr, "A macro name must be followed by '('.");
			}
			// We may have kept FOO.x.y as a reference, fold it now if y is not an aggregate.
			if (mutate) sema_expr_flatten_const_ident(expr->access_resolved_expr.parent);
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
			Decl *decl = inner->ident_expr;
			if (decl->decl_kind != DECL_VAR) break;
			if (!decl->var.out_param || decl->var.in_param) break;
			if (context->active_scope.flags & (SCOPE_ENSURE | SCOPE_ENSURE_MACRO)) break;
			RETURN_SEMA_ERROR(expr, "'out' parameters may not be read.");
		}
		case EXPR_UNARY:
		{
			if (expr->unary_expr.operator != UNARYOP_DEREF) break;
			Expr *inner = expr->inner_expr;
			if (inner->expr_kind != EXPR_IDENTIFIER) break;
			Decl *decl = inner->ident_expr;
			if (decl->decl_kind != DECL_VAR) break;
			if (!decl->var.out_param || decl->var.in_param) break;
			if (context->active_scope.flags & (SCOPE_ENSURE | SCOPE_ENSURE_MACRO)) return true;
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
	ASSERT(expr);
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


static inline bool sema_analyse_expr_check(SemaContext *context, Expr *expr, CheckType check)
{
	RESOLVE(expr, sema_analyse_expr_dispatch(context, expr, check))
}

bool sema_analyse_expr_address(SemaContext *context, Expr *expr)
{
	return sema_analyse_expr_check(context, expr, CHECK_ADDRESS);
}


static inline bool sema_analyse_expr_lvalue_dispatch(SemaContext *context, Expr *expr, bool *failed_ref)
{
RETRY:
	switch (expr->expr_kind)
	{
		case EXPR_HASH_IDENT:
			DEBUG_LOG("Expand hash ident");
			if (!sema_expr_fold_hash(context, expr)) return false;
			goto RETRY;
		case EXPR_CT_IDENT:
			return sema_expr_resolve_ct_identifier(context, expr);
		case EXPR_SUBSCRIPT:
			return sema_expr_analyse_subscript_lvalue(context, expr, false);
		case EXPR_OTHER_CONTEXT:
		{
			DEBUG_LOG("Switch context");
			SemaContext *new_context = expr->expr_other_context.context;
			expr_replace(expr, expr->expr_other_context.inner);
			if (expr->resolve_status == RESOLVE_DONE) return expr_ok(expr);
			ContextSwitchState state = context_switch_state_push(context, new_context);
			expr->resolve_status = RESOLVE_RUNNING;
			bool success = sema_analyse_expr_lvalue_dispatch(new_context, expr, failed_ref);
			context_switch_stat_pop(new_context, state);
			return success;
		}
		case EXPR_SWIZZLE:
			if (failed_ref) goto FAILED_REF;
			RETURN_SEMA_ERROR(expr, "You cannot use swizzling to assign to multiple elements, use element-wise assign instead.");
		case EXPR_LAMBDA:
			if (failed_ref) goto FAILED_REF;
			RETURN_SEMA_ERROR(expr, "This expression is a value and cannot be assigned to.");
		case EXPR_CONST:
			if (failed_ref) goto FAILED_REF;
			RETURN_SEMA_ERROR(expr, "You cannot assign to a constant expression.");
		case EXPR_GENERIC_IDENT:
			if (!sema_analyse_expr_dispatch(context, expr, CHECK_VALUE)) return false;
			goto IDENT_CHECK;
		case EXPR_UNRESOLVED_IDENTIFIER:
			if (!sema_analyse_expr_dispatch(context, expr, CHECK_VALUE)) return false;
			FALLTHROUGH;
		case EXPR_IDENTIFIER:
IDENT_CHECK:;
		{
			Decl *decl = expr->ident_expr;
			if (decl->decl_kind != DECL_VAR)
			{
				if (failed_ref) goto FAILED_REF;
				RETURN_SEMA_ERROR(expr, "You cannot assign a value to %s.", decl_to_a_name(decl));
			}
			if (decl->var.kind == VARDECL_CONST)
			{
				if (failed_ref) goto FAILED_REF;
				RETURN_SEMA_ERROR(expr, "You cannot assign to a constant.");
			}
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
				case VARDECL_MEMBER:
				case VARDECL_BITMEMBER:
					break;
				case VARDECL_CONST:
				case VARDECL_PARAM_EXPR:
				case VARDECL_UNWRAPPED:
				case VARDECL_ERASE:
				case VARDECL_REWRAPPED:
					UNREACHABLE
			}
			UNREACHABLE
		}
		case EXPR_POISONED:
			return false;
		case EXPR_UNARY:
			if (!sema_analyse_expr_dispatch(context, expr, CHECK_VALUE)) return false;
			if (expr->unary_expr.operator != UNARYOP_DEREF) break;
			if (IS_OPTIONAL(expr))
			{
				if (failed_ref) goto FAILED_REF;
				RETURN_SEMA_ERROR(expr, "You cannot assign to a dereferenced optional.");
			}
			return true;
		case EXPR_CT_ARG:
			if (expr->ct_arg_expr.type == TOKEN_CT_VAEXPR)
			{
				if (!context->current_macro)
				{
					RETURN_SEMA_ERROR(expr, "'$vaexpr' can only be used inside of a macro.");
				}
				// An expr argument, this means we copy and evaluate.
				ASSIGN_EXPR_OR_RET(Expr *arg_expr, sema_expr_analyse_ct_arg_index(context, exprptr(expr->ct_arg_expr.arg), NULL), false);
				expr_replace(expr, copy_expr_single(arg_expr));
				return sema_analyse_expr_lvalue(context, expr, failed_ref);
			}
			break;
		case EXPR_RECAST:
		case EXPR_CAST:
			if (failed_ref) goto FAILED_REF;
			RETURN_SEMA_ERROR(expr, "Assignment to casts is not possible (this sub-expression is a value, not an address), maybe you put the order of the operators wrong?");
		case EXPR_CT_EVAL:
			if (!sema_expr_resolve_ct_eval(context, expr)) return false;
			goto RETRY;
		case EXPR_SLICE:
			return sema_expr_analyse_slice(context, expr, CHECK_ADDRESS);
		case EXPR_ACCESS_UNRESOLVED:
			return sema_expr_analyse_access(context, expr, failed_ref, CHECK_ADDRESS, true);
		case EXPR_EXT_TRUNC:
		case EXPR_INT_TO_BOOL:
		case EXPR_DISCARD:
		case EXPR_ADDR_CONVERSION:
		case EXPR_ASM:
		case EXPR_BENCHMARK_HOOK:
		case EXPR_BINARY:
		case EXPR_BITASSIGN:
		case EXPR_BUILTIN:
		case EXPR_BUILTIN_ACCESS:
		case EXPR_CALL:
		case EXPR_CATCH:
		case EXPR_COMPILER_CONST:
		case EXPR_COND:
		case EXPR_CT_CALL:
		case EXPR_CT_ASSIGNABLE:
		case EXPR_CT_DEFINED:
		case EXPR_CT_IS_CONST:
		case EXPR_DECL:
		case EXPR_DEFAULT_ARG:
		case EXPR_DESIGNATED_INITIALIZER_LIST:
		case EXPR_DESIGNATOR:
		case EXPR_FORCE_UNWRAP:
		case EXPR_INITIALIZER_LIST:
		case EXPR_IOTA_DECL:
		case EXPR_LAST_FAULT:
		case EXPR_MACRO_BLOCK:
		case EXPR_MACRO_BODY_EXPANSION:
		case EXPR_MAKE_ANY:
		case EXPR_MAKE_SLICE:
		case EXPR_MEMBER_GET:
		case EXPR_MEMBER_SET:
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

		case EXPR_RETHROW:
		case EXPR_RETVAL:
		case EXPR_RVALUE:
		case EXPR_SLICE_ASSIGN:
		case EXPR_SLICE_COPY:
		case EXPR_SPLAT:
		case EXPR_STRINGIFY:
		case EXPR_TERNARY:
		case EXPR_TEST_HOOK:
		case EXPR_TRY:
		case EXPR_TRY_UNWRAP_CHAIN:
		case EXPR_TYPECALL:
		case EXPR_TYPEID_INFO:
		case EXPR_TYPEINFO:
		case EXPR_VECTOR_FROM_ARRAY:
		case EXPR_VECTOR_TO_ARRAY:
		case EXPR_SLICE_TO_VEC_ARRAY:
		case EXPR_SCALAR_TO_VECTOR:
		case EXPR_SUBSCRIPT_ADDR:
		case EXPR_EXPRESSION_LIST:
		case EXPR_MACRO_BODY:
		case EXPR_CATCH_UNRESOLVED:
		case EXPR_COMPOUND_LITERAL:
		case EXPR_EMBED:
		case EXPR_TYPEID:
		case EXPR_VASPLAT:
		case EXPR_TRY_UNRESOLVED:
		case EXPR_TWO:
			break;
		case EXPR_BITACCESS:
		case EXPR_SUBSCRIPT_ASSIGN:
		case EXPR_ACCESS_RESOLVED:
		case EXPR_CT_SUBSCRIPT:
			UNREACHABLE
	}
	if (failed_ref) goto FAILED_REF;
	RETURN_SEMA_ERROR(expr, "An assignable expression, like a variable, was expected here.");

FAILED_REF:
	*failed_ref = true;
	return false;
}

bool sema_analyse_expr_lvalue(SemaContext *context, Expr *expr, bool *failed_ref)
{
	ASSERT(expr);
	RESOLVE(expr, sema_analyse_expr_lvalue_dispatch(context, expr, failed_ref))
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
	if (!IS_OPTIONAL(expr))
	{
		if (expr->expr_kind == EXPR_BINARY && expr->binary_expr.operator == BINARYOP_EQ)
		{
			RETURN_SEMA_ERROR(expr, "This equals check was discarded, which isn't allowed. You can assign it to a variable or explicitly ignore it with a void cast '(void)' if this is what you want.");
		}
		return true;
	}
	RETURN_SEMA_ERROR(expr, "An optional value was discarded, you can assign it to a variable, ignore it with a void cast '(void)', rethrow on optional with '!' or panic '!!' to avoid this error.");
ERROR_ARGS:
	RETURN_SEMA_ERROR(expr, "The result of this call is optional due to its argument(s). This optional result may not be implicitly discarded. Please assign it to a variable, ignore it with '(void)', rethrow with '!' or panic with '!!'.");
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
		case UNRESOLVED_EXPRS:
			UNREACHABLE
		case EXPR_RECAST:
			if (sema_cast_const(expr->inner_expr))
			{
				Type *type = expr->type;
				expr_replace(expr, expr->inner_expr);
				expr->type = type;
				return true;
			}
			return false;
		case EXPR_ACCESS_RESOLVED:
		case EXPR_BITACCESS:
		{
			Expr *parent = expr->access_resolved_expr.parent;
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
			if (!sema_cast_const(expr->access_resolved_expr.parent)) return false;
			if (!sema_expr_fold_to_member(expr, expr->access_resolved_expr.parent, expr->access_resolved_expr.ref)) return false;
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

bool sema_analyse_inferred_expr(SemaContext *context, Type *to, Expr *expr)
{
	Type *original_type = to;
	to = type_no_optional(to);
RETRY:
	switch (expr->resolve_status)
	{
		case RESOLVE_NOT_DONE:
			break;
		case RESOLVE_RUNNING:
			SEMA_ERROR(expr, "Recursive resolution of list.");
			return expr_poison(expr);
		case RESOLVE_DONE:
			if (to && expr->type != to)
			{
				cast_implicit_silent(context, expr, to, false);
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
			bool success = sema_analyse_inferred_expr(context, original_type, expr);
			context->inlined_at = old_span;
			return success;
		}
		case EXPR_DESIGNATED_INITIALIZER_LIST:
		case EXPR_INITIALIZER_LIST:
			if (!sema_expr_analyse_initializer_list(context, to, expr)) return expr_poison(expr);
			break;
		case EXPR_UNRESOLVED_IDENTIFIER:
			if (!sema_expr_analyse_identifier(context, to, expr)) return expr_poison(expr);
			break;
		case EXPR_LAMBDA:
			if (!sema_expr_analyse_lambda(context, to, expr)) return expr_poison(expr);
			break;
		case EXPR_BINARY:
			if (!sema_expr_analyse_binary(context, to, expr, NULL)) return expr_poison(expr);
			break;
		case EXPR_TERNARY:
			if (!sema_expr_analyse_ternary(context, to, expr)) return expr_poison(expr);
			break;
		case EXPR_CT_ARG:
			if (!sema_expr_analyse_ct_arg(context, to, expr)) return expr_poison(expr);
			break;
		case EXPR_RETHROW:
			if (!sema_expr_analyse_rethrow(context, expr, original_type)) return expr_poison(expr);
			break;
		case EXPR_UNARY:
			if (to && expr->unary_expr.operator == UNARYOP_TADDR && to->canonical->type_kind == TYPE_POINTER && to->canonical != type_voidptr)
			{
				if (!sema_analyse_inferred_expr(context, type_get_indexed_type(to), expr->unary_expr.expr)) return expr_poison(expr);
			}
			FALLTHROUGH;
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
				if (ch == '$' || ch == '@' || ch == '#') break; // $foo / @foo

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
	size_t start = 0;
	switch (string[0])
	{
		case '@':
		case '$':
		case '#':
			hash = FNV1a(string[0], hash);
			start = 1;
			break;
		default:
			break;
	}

	for (size_t i = start; i < len; i++)
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
		case TOKEN_AT_IDENT:
		case TOKEN_CT_TYPE_IDENT:
		case TOKEN_CT_IDENT:
		case TOKEN_HASH_IDENT:
		case TYPE_TOKENS:
			return type;
		default:
			*ident_ref = NULL;
			return TOKEN_INVALID_TOKEN;
	}
}

bool sema_insert_method_call(SemaContext *context, Expr *method_call, Decl *method_decl, Expr *parent, Expr **arguments, bool reverse_overload)
{
	SourceSpan original_span = method_call->span;
	Expr *resolve = method_call;
	if (reverse_overload)
	{
		if (!expr_is_const(parent))
		{
			Decl *temp = decl_new_generated_var(method_decl->func_decl.signature.params[1]->type, VARDECL_LOCAL, parent->span);
			Expr *generate = expr_generate_decl(temp, expr_copy(parent));
			parent->expr_kind = EXPR_IDENTIFIER;
			parent->ident_expr = temp;
			parent->resolve_status = RESOLVE_DONE;
			parent->type = temp->type;
			if (!sema_analyse_expr(context, generate)) return false;
			Expr *copied_method = expr_copy(method_call);
			expr_rewrite_two(method_call, generate, copied_method);
			method_call = copied_method;
		}
		Expr *arg0 = arguments[0];
		arguments[0] = parent;
		parent = arg0;
	}
	*method_call = (Expr) { .expr_kind = EXPR_CALL,
			.span = original_span,
			.resolve_status = RESOLVE_RUNNING,
			.call_expr.func_ref = declid(method_decl),
			.call_expr.arguments = arguments,
			.call_expr.is_func_ref = true,
			.call_expr.is_type_method = true,
	};
	Type *type = parent->type->canonical;
	Decl *first_param = method_decl->func_decl.signature.params[0];
	Type *first = first_param->type->canonical;
	// Deref / addr as needed.
	if (type != first)
	{
		if (first->type_kind == TYPE_POINTER && first->pointer == type)
		{
			expr_insert_addr(parent);
		}
		else if (type->type_kind == TYPE_POINTER && type->pointer == first)
		{
			if (!sema_expr_rewrite_insert_deref(context, parent)) return false;
		}
	}
	ASSERT_SPAN(method_call, parent && parent->type && first == parent->type->canonical);
	unit_register_external_symbol(context, method_decl);
	if (!sema_expr_analyse_general_call(context, method_call, method_decl, parent, false,
										NULL)) return expr_poison(method_call);
	method_call->resolve_status = RESOLVE_DONE;
	if (resolve != method_call)
	{
		resolve->resolve_status = RESOLVE_DONE;
		resolve->type = method_call->type;
	}
	return true;
}

static inline bool sema_insert_binary_overload(SemaContext *context, Expr *expr, Decl *overload, Expr *lhs, Expr *rhs, bool reverse)
{
	Expr **args = NULL;
	vec_add(args, rhs);
	return sema_insert_method_call(context, expr, overload, lhs, args, reverse);
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
