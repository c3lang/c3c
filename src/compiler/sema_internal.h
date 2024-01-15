#pragma once

// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.


#include "compiler_internal.h"

#define SCOPE_OUTER_START do { DynamicScope stored_scope = context->active_scope; context_change_scope_with_flags(context, SCOPE_NONE);
#define SCOPE_OUTER_END assert(context->active_scope.defer_last == context->active_scope.defer_start); context->active_scope = stored_scope; } while(0)
#define SCOPE_START SCOPE_START_WITH_FLAGS(SCOPE_NONE)
#define SCOPE_START_WITH_FLAGS(flags) do { DynamicScope old_scope = context->active_scope; context_change_scope_with_flags(context, flags);
#define SCOPE_START_WITH_LABEL(label) do { DynamicScope old_scope = context->active_scope; context_change_scope_for_label(context, label);
#define SCOPE_END assert(context->active_scope.defer_last == context->active_scope.defer_start); context->active_scope = old_scope; } while(0)
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

extern const char *ct_eval_error;

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

void sema_context_init(SemaContext *context, CompilationUnit *unit);
void sema_context_destroy(SemaContext *context);
unsigned sema_context_push_ct_stack(SemaContext *context);
void sema_context_pop_ct_stack(SemaContext *context, unsigned old_state);

bool sema_analyse_function_body(SemaContext *context, Decl *func);
bool sema_analyse_contracts(SemaContext *context, AstId doc, AstId **asserts, SourceSpan span, bool *has_ensures);
void sema_append_contract_asserts(AstId assert_first, Ast* compound_stmt);

void sema_analyse_pass_top(Module *module);
void sema_analyse_pass_module_hierarchy(Module *module);
void sema_analysis_pass_process_imports(Module *module);
void sema_analysis_pass_register_global_declarations(Module *module);
void sema_analysis_pass_register_conditional_units(Module *module);
void sema_analysis_pass_register_conditional_declarations(Module *module);
void sema_analysis_pass_decls(Module *module);
void sema_analysis_pass_ct_assert(Module *module);
void sema_analysis_pass_ct_echo(Module *module);
void sema_analysis_pass_functions(Module *module);
void sema_analysis_pass_interface(Module *module);

void sema_analysis_pass_lambda(Module *module);
void sema_analyze_stage(Module *module, AnalysisStage stage);
void sema_trace_liveness(void);

Expr *sema_expr_resolve_access_child(SemaContext *context, Expr *child, bool *missing);
bool sema_analyse_expr_lvalue(SemaContext *context, Expr *expr);
bool sema_analyse_expr_lvalue_fold_const(SemaContext *context, Expr *expr);
Expr *expr_access_inline_member(Expr *parent, Decl *parent_decl);
bool sema_analyse_ct_expr(SemaContext *context, Expr *expr);
Decl *sema_find_operator(SemaContext *context, Type *type, OperatorOverload operator_overload);
bool sema_insert_method_call(SemaContext *context, Expr *method_call, Decl *method_decl, Expr *parent, Expr **arguments);
bool sema_expr_analyse_builtin_call(SemaContext *context, Expr *expr);

bool sema_expr_analyse_macro_call(SemaContext *context, Expr *call_expr, Expr *struct_var, Decl *decl, bool optional,
                                  bool *no_match_ref);
Expr *sema_expr_analyse_ct_arg_index(SemaContext *context, Expr *index_expr, unsigned *index_ref, bool report_error);
Expr *sema_ct_eval_expr(SemaContext *c, bool is_type, Expr *inner, bool report_missing);
bool sema_analyse_asm(SemaContext *context, AsmInlineBlock *block, Ast *asm_stmt);
bool sema_bit_assignment_check(Expr *right, Decl *member);
int sema_check_comp_time_bool(SemaContext *context, Expr *expr);
bool sema_expr_check_assign(SemaContext *c, Expr *expr);
bool sema_analyse_function_signature(SemaContext *context, Decl *func_decl, CallABI abi, Signature *signature);
ConstInitializer *sema_merge_bitstruct_const_initializers(ConstInitializer *lhs, ConstInitializer *rhs, BinaryOp op);
void sema_invert_bitstruct_const_initializer(ConstInitializer *initializer);

MemberIndex sema_len_from_const(Expr *expr);

void cast_promote_vararg(SemaContext *context, Expr *arg);
Type *cast_numeric_arithmetic_promotion(Type *type);
void cast_to_int_to_max_bit_size(SemaContext *context, Expr *lhs, Expr *rhs, Type *left_type, Type *right_type);
bool sema_decl_if_cond(SemaContext *context, Decl *decl);
bool sema_flattened_expr_is_const(SemaContext *context, Expr *expr);
Decl *sema_analyse_parameterized_identifier(SemaContext *c, Path *decl_path, const char *name, SourceSpan span, Expr **params);

INLINE bool sema_set_abi_alignment(SemaContext *context, Type *type, AlignSize *result);
INLINE bool sema_set_alloca_alignment(SemaContext *context, Type *type, AlignSize *result);
void sema_display_deprecated_warning_on_use(SemaContext *context, Decl *decl, SourceSpan use);

INLINE bool sema_set_abi_alignment(SemaContext *context, Type *type, AlignSize *result)
{
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
