#pragma once

// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.


#include "compiler_internal.h"

#define INT5_MAX         15
#define INT12_MAX        2047
#define INT20_MAX        524287
#define INT5_MIN         -16
#define INT12_MIN        -2048
#define INT20_MIN        (-INT20_MAX-1)
#define UINT5_MAX         31
#define UINT12_MAX        4095
#define UINT20_MAX        1048575U

#define SEMA_WARN(_node, ...) (sema_warn_at(context, (_node)->span, __VA_ARGS__))
#define SEMA_ERROR(_node, ...) sema_error_at(context, (_node)->span, __VA_ARGS__)
#define RETURN_SEMA_ERROR(_node, ...) do { sema_error_at(context, (_node)->span, __VA_ARGS__); return false; } while (0)
#define RETURN_SEMA_ERROR_AT(span__, ...) do { sema_error_at(context, span__, __VA_ARGS__); return false; } while (0)
#ifdef NDEBUG
#define ASSERT_SPANF(node__, check__, format__, ...) do { } while(0)
#define ASSERT_SPAN(node__, check__) do { } while(0)
#else
#define ASSERT_SPANF(node__, check__, format__, ...) do { if (!(check__)) { assert_print_line((node__)->span); eprintf(format__, __VA_ARGS__); ASSERT0(check__); } } while(0)
#define ASSERT_SPAN(node__, check__) do { if (!(check__)) { assert_print_line((node__)->span); ASSERT0(check__); } } while(0)
#endif
#define SCOPE_OUTER_START do { DynamicScope stored_scope = context->active_scope; context_change_scope_with_flags(context, SCOPE_NONE);
#define SCOPE_OUTER_END ASSERT0(context->active_scope.defer_last == context->active_scope.defer_start); context->active_scope = stored_scope; } while(0)
#define SCOPE_START SCOPE_START_WITH_FLAGS(SCOPE_NONE)
#define SCOPE_START_WITH_FLAGS(flags) do { DynamicScope old_scope = context->active_scope; context_change_scope_with_flags(context, flags);
#define SCOPE_START_WITH_LABEL(label) do { DynamicScope old_scope = context->active_scope; context_change_scope_for_label(context, label);
#define SCOPE_END ASSERT0(context->active_scope.defer_last == context->active_scope.defer_start); context->active_scope = old_scope; } while(0)
#define SCOPE_POP_ERROR() ((bool)(context->active_scope = old_scope, false))
#define SCOPE_ERROR_END_OUTER() do { context->active_scope = stored_scope; } while(0)
#define PUSH_X(ast, X) AstId _old_##X##_defer = context->X##_defer; Ast *_old_##X = context->X##_target; context->X##_target = ast; context->X##_defer = context->active_scope.defer_last
#define POP_X(X) context->X##_target = _old_##X; context->X##_defer = _old_##X##_defer
#define PUSH_CONTINUE(ast) PUSH_X(ast, continue)
#define POP_CONTINUE() POP_X(continue)
#define PUSH_BREAK(ast) PUSH_X(ast, break)
#define POP_BREAK() POP_X(break)
#define PUSH_NEXT(ast, sast) PUSH_X(ast, next); Ast *_old_next_switch = context->next_switch; context->next_switch = sast
#define POP_NEXT() POP_X(next); context->next_switch = _old_next_switch
#define PUSH_BREAKCONT(ast) PUSH_CONTINUE(ast); PUSH_BREAK(ast)
#define POP_BREAKCONT() POP_CONTINUE(); POP_BREAK()

Decl **global_context_acquire_locals_list(void);
void generic_context_release_locals_list(Decl **);
const char *context_filename(SemaContext *context);

AstId context_get_defers(SemaContext *context, AstId defer_top, AstId defer_bottom, bool is_success);
void context_pop_defers(SemaContext *context, AstId *next);
void context_pop_defers_and_replace_ast(SemaContext *context, Ast *ast);
void context_change_scope_for_label(SemaContext *context, DeclId label);
void context_change_scope_with_flags(SemaContext *context, ScopeFlags flags);
SemaContext *context_transform_for_eval(SemaContext *context, SemaContext *temp_context, CompilationUnit *eval_unit);

TokenType sema_splitpathref(const char *string, ArraySize len, Path **path_ref, const char **ident_ref);
void sema_print_inline(SemaContext *context);
void sema_error_at(SemaContext *context, SourceSpan span, const char *message, ...);
bool sema_warn_at(SemaContext *context, SourceSpan span, const char *message, ...);

void sema_context_init(SemaContext *context, CompilationUnit *unit);
void sema_context_destroy(SemaContext *context);
unsigned sema_context_push_ct_stack(SemaContext *context);
void sema_context_pop_ct_stack(SemaContext *context, unsigned old_state);

bool sema_analyse_function_body(SemaContext *context, Decl *func);
bool sema_analyse_contracts(SemaContext *context, AstId doc, AstId **asserts, SourceSpan span, bool *has_ensures);
void sema_append_contract_asserts(AstId assert_first, Ast* compound_stmt);

Decl *sema_create_runner_main(SemaContext *context, Decl *decl);

void sema_analyse_pass_top(Module *module);
void sema_analyse_pass_module_hierarchy(Module *module);
void sema_analysis_pass_process_imports(Module *module);
void sema_analysis_pass_register_global_declarations(Module *module);
void sema_analysis_pass_process_includes(Module *module);
void sema_analysis_pass_register_conditional_units(Module *module);
void sema_analysis_pass_register_conditional_declarations(Module *module);
void sema_analysis_pass_process_methods(Module *module, bool process_generic);
void sema_analysis_pass_decls(Module *module);
void sema_analysis_pass_ct_assert(Module *module);
void sema_analysis_pass_ct_echo(Module *module);
void sema_analysis_pass_functions(Module *module);
void sema_analysis_pass_interface(Module *module);

void sema_analysis_pass_lambda(Module *module);
void sema_analyze_stage(Module *module, AnalysisStage stage);
void sema_trace_liveness(void);

Expr *sema_expr_resolve_access_child(SemaContext *context, Expr *child, bool *missing);
bool sema_analyse_expr_address(SemaContext *context, Expr *expr);
bool sema_analyse_expr_lvalue(SemaContext *context, Expr *expr);

bool sema_analyse_expr_value(SemaContext *context, Expr *expr);
Expr *expr_access_inline_member(Expr *parent, Decl *parent_decl);
bool sema_analyse_ct_expr(SemaContext *context, Expr *expr);
Decl *sema_find_operator(SemaContext *context, Type *type, OperatorOverload operator_overload);
bool sema_insert_method_call(SemaContext *context, Expr *method_call, Decl *method_decl, Expr *parent, Expr **arguments);
bool sema_expr_analyse_builtin_call(SemaContext *context, Expr *expr);

bool sema_expr_analyse_macro_call(SemaContext *context, Expr *call_expr, Expr *struct_var, Decl *decl, bool optional,
                                  bool *no_match_ref);
Expr *sema_expr_analyse_ct_arg_index(SemaContext *context, Expr *index_expr, unsigned *index_ref);
Expr *sema_ct_eval_expr(SemaContext *context, bool is_type, Expr *inner, bool report_missing);
bool sema_analyse_asm(SemaContext *context, AsmInlineBlock *block, Ast *asm_stmt);

bool sema_bit_assignment_check(SemaContext *context, Expr *right, Decl *member);
CondResult sema_check_comp_time_bool(SemaContext *context, Expr *expr);

bool sema_expr_check_assign(SemaContext *context, Expr *expr, bool *failed_ref);
bool sema_analyse_function_signature(SemaContext *context, Decl *func_decl, TypeInfo *parent, CallABI abi, Signature *signature);
Expr *sema_create_struct_from_expressions(Decl *struct_decl, SourceSpan span, Expr **exprs);
ConstInitializer *sema_merge_bitstruct_const_initializers(ConstInitializer *lhs, ConstInitializer *rhs, BinaryOp op);
void sema_invert_bitstruct_const_initializer(ConstInitializer *initializer);
ArrayIndex sema_len_from_const(Expr *expr);
void cast_promote_vararg(SemaContext *context, Expr *arg);
Type *cast_numeric_arithmetic_promotion(Type *type);
void cast_to_int_to_max_bit_size(SemaContext *context, Expr *lhs, Expr *rhs, Type *left_type, Type *right_type);
bool sema_decl_if_cond(SemaContext *context, Decl *decl);
Decl *sema_analyse_parameterized_identifier(SemaContext *c, Path *decl_path, const char *name, SourceSpan span,
                                            Expr **params, bool *was_recursive_ref);
Type *sema_resolve_type_get_func(Signature *signature, CallABI abi);
INLINE bool sema_set_abi_alignment(SemaContext *context, Type *type, AlignSize *result);
INLINE bool sema_set_alloca_alignment(SemaContext *context, Type *type, AlignSize *result);
INLINE void sema_display_deprecated_warning_on_use(SemaContext *context, Decl *decl, SourceSpan use);
bool sema_expr_analyse_ct_concat(SemaContext *context, Expr *concat_expr, Expr *left, Expr *right);

INLINE void assert_print_line(SourceSpan span)
{
	if (span.row == 0)
	{
		eprintf("Assert analysing code at unknown location:\n");
		return;
	}
	File *file = source_file_by_id(span.file_id);
	eprintf("Assert analysing '%s' at row %d, col %d.\n", file->name, span.row, span.col);
}


INLINE bool sema_set_abi_alignment(SemaContext *context, Type *type, AlignSize *result)
{
	if (type_is_func_ptr(type))
	{
		*result = type_abi_alignment(type_voidptr);
		return true;
	}
	if (!sema_resolve_type_decl(context, type)) return false;
	*result = type_abi_alignment(type);
	return true;
}

INLINE bool sema_set_alloca_alignment(SemaContext *context, Type *type, AlignSize *result)
{
	if (!sema_resolve_type_decl(context, type)) return false;
	*result = type_alloca_alignment(type);
	return true;
}

INLINE Attr* attr_find_kind(Attr **attrs, AttributeType attr_type)
{
	FOREACH(Attr *, attr, attrs)
	{
		if (attr->attr_kind == attr_type) return attr;
	}
	return NULL;
}

INLINE void sema_display_deprecated_warning_on_use(SemaContext *context, Decl *decl, SourceSpan span)
{
	ASSERT0(decl->resolve_status == RESOLVE_DONE);
	if (!decl->resolved_attributes || !decl->attrs_resolved || !decl->attrs_resolved->deprecated) return;
	const char *msg = decl->attrs_resolved->deprecated;

	// Prevent multiple reports
	decl->attrs_resolved->deprecated = NULL;

	if (compiler.build.silence_deprecation) return;
	if (msg[0])
	{
		sema_warning_at(span, "'%s' is deprecated: %s.", decl->name, msg);
		return;
	}
	sema_warning_at(span, "'%s' is deprecated.", decl->name);
}

static inline IndexDiff range_const_len(Range *range)
{
	switch (range->range_type)
	{
		case RANGE_CONST_LEN:
			return range->const_end;
		case RANGE_CONST_END:
			return -1;
		case RANGE_CONST_RANGE:
			return range->len_index;
		case RANGE_DYNAMIC:
			break;
	}
	Expr *start = exprptr(range->start);
	Expr *end = exprptrzero(range->end);
	if (!end || !expr_is_const_int(end)) return -1;
	if (!int_fits(end->const_expr.ixx, TYPE_I32)) return -1;
	IndexDiff end_val = (IndexDiff)int_to_i64(end->const_expr.ixx);
	if (range->is_len) return end_val;
	if (!expr_is_const_int(start)) return -1;
	if (!int_fits(start->const_expr.ixx, TYPE_I32)) return -1;
	IndexDiff start_val = (IndexDiff)int_to_i64(start->const_expr.ixx);
	if (range->start_from_end && range->end_from_end) return start_val - end_val + 1;
	if (range->start_from_end != range->end_from_end) return -1;
	return end_val - start_val + 1;
}

static inline StorageType sema_resolve_storage_type(SemaContext *context, Type *type)
{
	if (!type) return STORAGE_NORMAL;
	bool is_distinct = false;
	RETRY:
	if (type == type_wildcard_optional) return STORAGE_WILDCARD;
	switch (type->type_kind)
	{
		case TYPE_VOID:
			return is_distinct ? STORAGE_UNKNOWN : STORAGE_VOID;
		case TYPE_WILDCARD:
			return STORAGE_WILDCARD;
		case TYPE_MEMBER:
		case TYPE_UNTYPED_LIST:
		case TYPE_TYPEINFO:
		case TYPE_FUNC_RAW:
			return STORAGE_COMPILE_TIME;
		case TYPE_OPTIONAL:
			type = type->optional;
			goto RETRY;
		case TYPE_TYPEDEF:
			if (!sema_analyse_decl(context, type->decl)) return false;
			type = type->canonical;
			goto RETRY;
		case TYPE_DISTINCT:
			is_distinct = true;
			if (!sema_analyse_decl(context, type->decl)) return false;
			type = type->decl->distinct->type;
			goto RETRY;
		default:
			return STORAGE_NORMAL;
	}
}

static inline TypeProperty type_property_by_name(const char *name)
{
	for (unsigned i = 0; i < NUMBER_OF_TYPE_PROPERTIES; i++)
	{
		if (type_property_list[i] == name) return (TypeProperty)i;
	}
	return TYPE_PROPERTY_NONE;
}
