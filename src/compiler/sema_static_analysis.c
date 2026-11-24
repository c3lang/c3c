// Copyright (c) 2026 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "sema_internal.h"

#define MAX_SLOTS 64
#define BIT_SET(_x, _bit) (((_x) >> (_bit)) & 1)

typedef uint64_t FlagSlot;
typedef int SASlotId;

typedef struct
{
	FlagSlot is_null;
	FlagSlot not_null;
} SAState;

typedef struct
{
	bool bail;
	DeclId slots[MAX_SLOTS];
	int slot_count;
	SAState state;
} SAContext;

typedef enum
{
	NULLABILITY_UNKNOWN,
	NULLABILITY_NULL,
	NULLABILITY_NOT_NULL,
} SANullability;

static void sa_analyse_functions(Decl **functions);
static void sa_analyse_function(SAContext *c, Decl *func);
static void sa_analyse_stmt(SAContext *c, Ast *stmt);
static void sa_analyse_stmt_chain(SAContext *c, AstId ast_id);
static void sa_analyse_expr_stmt(SAContext *c, Expr *expr);
static void sa_analyse_declare(SAContext *c, Decl *decl);
static void sa_analyse_return(SAContext *c, Ast *stmt);
static void sa_analyse_block_exit(SAContext *c, Ast *stmt);
static void sa_analyse_assert(SAContext *c, Ast *stmt);
static void sa_analyse_if(SAContext *c, Ast *stmt);
static void sa_analyse_for(SAContext *c, Ast *stmt);
static void sa_analyse_switch(SAContext *c, Ast *stmt);
static void sa_analyse_nextcase(SAContext *c, Ast *stmt);
static void sa_analyse_break_continue(SAContext *c, Ast *stmt);

static SANullability sa_analyse_expr(SAContext *c, Expr *expr);
static SANullability sa_analyse_const_expr(SAContext *c, Expr *expr);
static SANullability sa_analyse_binary_expr(SAContext *c, Expr *expr);
static SANullability sa_analyse_call_expr(SAContext *c, Expr *expr);
static SANullability sa_analyse_ident_expr(SAContext *c, Expr *expr);
static SANullability sa_analyse_expr_list(SAContext *c, Expr **expr);
static SANullability sa_analyse_unary_expr(SAContext *c, Expr *expr);

static SAState sa_state_join(SAState a, SAState b);
static SASlotId sa_allocate_slot(SAContext *c, Decl *decl);
static void sa_set_slot_nullability(SAContext *c, SASlotId slot, SANullability nullability);
static SASlotId sa_find_slot(SAContext *c, Decl *decl);

void sema_static_analysis(void)
{
	FOREACH(Module *, module, compiler.context.module_list)
	{
		FOREACH(CompilationUnit *, unit, module->units)
		{
			sa_analyse_functions(unit->functions);
			sa_analyse_functions(unit->methods);
			sa_analyse_functions(unit->lambdas);
		}
	}
}

static void sa_analyse_functions(Decl **functions)
{
	SAContext context;
	FOREACH(Decl *, func, functions)
	{
		if (func->decl_kind != DECL_FUNC || !func->func_decl.body || !func->is_body_checked) continue;
		if (!func->is_live) continue;
		sa_analyse_function(&context, func);
	}
}

static void sa_analyse_function(SAContext *c, Decl *func)
{
	c->bail = false;
	c->state.is_null = 0;    // Empty null flags
	c->state.not_null = 0;   // Empty not null flags
	c->slot_count = 0; // No slots in use

	Ast *body = astptr(func->func_decl.body);
	ASSERT(body->ast_kind == AST_COMPOUND_STMT);
	AstId next = body->compound_stmt.first_stmt;
	while (next)
	{
		Ast *stmt = astptr(next);
		next = stmt->next;
		sa_analyse_stmt(c, stmt);
		if (c->bail) break;
	}
}

static void sa_analyse_stmt(SAContext *c, Ast *stmt)
{
	if (!stmt || c->bail) return;
	switch (stmt->ast_kind)
	{
		case AST_DEFER_STMT:
		case AST_NOP_STMT:
			return;
		case AST_CT_COMPOUND_STMT:
			sa_analyse_stmt_chain(c, stmt->ct_compound_stmt);
			return;
		case AST_COMPOUND_STMT:
			sa_analyse_stmt_chain(c, stmt->compound_stmt.first_stmt);
			return;
		case AST_EXPR_STMT:
			sa_analyse_expr_stmt(c, stmt->expr_stmt);
			return;
		case AST_DECLARE_STMT:
			sa_analyse_declare(c, stmt->declare_stmt);
			return;
		case AST_DECLS_STMT:
		{
			FOREACH(Decl *, decl, stmt->decls_stmt) sa_analyse_declare(c, decl);
			return;
		}
		case AST_RETURN_STMT:
			sa_analyse_return(c, stmt);
			return;
		case AST_BLOCK_EXIT_STMT:
			sa_analyse_block_exit(c, stmt);
			return;
		case AST_ASSERT_STMT:
			sa_analyse_assert(c, stmt);
			return;
		case AST_IF_STMT:
			sa_analyse_if(c, stmt);
			return;
		case AST_FOR_STMT:
			sa_analyse_for(c, stmt);
			return;
		case AST_SWITCH_STMT:
			sa_analyse_switch(c, stmt);
			return;
		case AST_NEXTCASE_STMT:
			sa_analyse_nextcase(c, stmt);
			return;
		case AST_BREAK_STMT:
		case AST_CONTINUE_STMT:
			sa_analyse_break_continue(c, stmt);
			return;
		case AST_CT_ASSERT:
		case AST_CT_ECHO_STMT:
		case AST_CT_ELSE_STMT:
		case AST_CT_EXPAND_STMT:
		case AST_CT_FOREACH_STMT:
		case AST_CT_FOR_STMT:
		case AST_CT_IF_STMT:
		case AST_CT_SWITCH_STMT:
		case AST_CT_TYPE_ASSIGN_STMT:
		case AST_POISONED:
		case AST_FOREACH_STMT:
		case AST_CASE_STMT:
		case AST_DEFAULT_STMT:
			UNREACHABLE_VOID;
		case AST_ASM_BLOCK_STMT:
		case AST_ASM_LABEL:
		case AST_ASM_STMT:
			c->bail = true;
			return;
	}
	UNREACHABLE_VOID;
}

static void sa_analyse_stmt_chain(SAContext *c, AstId id)
{
	while (id)
	{
		Ast *ast = astptr(id);
		sa_analyse_stmt(c, ast);
		id = ast->next;
	}
}

static void sa_analyse_expr_stmt(SAContext *c, Expr *expr)
{
	sa_analyse_expr(c, expr);
}

static void sa_analyse_declare(SAContext *c, Decl *decl)
{
	if (!decl || decl->decl_kind != DECL_VAR) return;
	// Only handle locals, excluding tlocal and static.
	if (decl->var.kind != VARDECL_LOCAL || decl->var.is_static || decl->var.is_threadlocal) return;
	Expr *init = decl->var.init_expr;
	if (init && IS_OPTIONAL(init))
	{
		// Skip optional
		c->bail = true;
		return;
	}
	SANullability state;
	if (init)
	{
		state = sa_analyse_expr(c, init);
	}
	else
	{
		state = decl->var.no_init ? NULLABILITY_UNKNOWN : NULLABILITY_NULL;
	}
	SASlotId slot = sa_allocate_slot(c, decl);
	if (slot < 0) return;
	sa_set_slot_nullability(c, slot, state);
}

static void sa_analyse_return(SAContext *c, Ast *stmt)
{
	c->bail = true;
}

static void sa_analyse_block_exit(SAContext *c, Ast *stmt)
{
	c->bail = true;
}

static void sa_analyse_assert(SAContext *c, Ast *stmt)
{
	c->bail = true;
}

static void sa_analyse_if(SAContext *c, Ast *stmt)
{
	if (stmt->if_stmt.flow.label)
	{
		c->bail = true;
		return;
	}
	sa_analyse_expr(c, exprptr(stmt->if_stmt.cond));
	SAState stored = c->state;
	sa_analyse_stmt(c, astptrzero(stmt->if_stmt.then_body));
	SAState then_state = c->state;
	c->state = stored;
	sa_analyse_stmt(c, astptrzero(stmt->if_stmt.else_body));
	c->state = sa_state_join(then_state, c->state);
}

static void sa_analyse_for(SAContext *c, Ast *stmt)
{
	c->bail = true;
}
static void sa_analyse_switch(SAContext *c, Ast *stmt)
{
	c->bail = true;
}
static void sa_analyse_nextcase(SAContext *c, Ast *stmt)
{
	c->bail = true;
}
static void sa_analyse_break_continue(SAContext *c, Ast *stmt)
{
	c->bail = true;
}

static void sa_assign_slot(SAContext *c, Expr *lhs, SANullability value)
{
	if (lhs->expr_kind != EXPR_IDENTIFIER) return;
	SASlotId slot = sa_allocate_slot(c, lhs->ident_expr);
	if (slot >= 0) sa_set_slot_nullability(c, slot, value);
}

static void sa_set_slot_nullability(SAContext *c, SASlotId slot, SANullability nullability)
{
	ASSERT(slot >= 0 && slot < MAX_SLOTS);
	uint32_t bit = 1ULL << slot;
	switch (nullability)
	{
		case NULLABILITY_NOT_NULL:
			c->state.not_null |= bit;
			c->state.is_null &= ~bit;
			return;
		case NULLABILITY_NULL:
			c->state.not_null &= ~bit;
			c->state.is_null |= bit;
			return;
		case NULLABILITY_UNKNOWN:
			c->state.not_null &= ~bit;
			c->state.is_null &= ~bit;
			return;
	}
	UNREACHABLE_VOID;
}

static SASlotId sa_find_slot(SAContext *c, Decl *decl)
{
	DeclId id = declid(decl);
	for (int i = 0; i < c->slot_count; i++)
	{
		if (c->slots[i] == id) return i;
	}
	return -1;
}

static SASlotId sa_allocate_slot(SAContext *c, Decl *decl)
{
	ASSERT(decl);
	if (!decl || decl->decl_kind != DECL_VAR) return -1;
	if (decl->var.kind != VARDECL_LOCAL || decl->var.is_static || decl->var.is_threadlocal) return -1;
	DeclId id = declid(decl);
	for (int i = 0; i < c->slot_count; i++)
	{
		if (c->slots[i] == id) return i;
	}
	if (c->slot_count == MAX_SLOTS) return -1;
	c->slots[c->slot_count] = id;
	return c->slot_count++;
}

static SAState sa_state_join(SAState a, SAState b)
{
	return (SAState){ .is_null = a.is_null && b.is_null, .not_null = a.not_null && b.not_null };
}

static SANullability sa_analyse_expr(SAContext *c, Expr *expr)
{
	if (!expr) return NULLABILITY_UNKNOWN;
RETRY:;
	switch (expr->expr_kind)
	{
		case EXPR_NOP:
			return NULLABILITY_UNKNOWN;
		case NON_RUNTIME_EXPR:
		case UNRESOLVED_EXPRS:
		case EXPR_SUBSCRIPT_ASSIGN:
		case EXPR_OPERATOR_CHARS:
			UNREACHABLE
		case EXPR_CONST:
			return sa_analyse_const_expr(c, expr);
		case EXPR_CALL:
			return sa_analyse_call_expr(c, expr);
		case EXPR_BINARY:
			return sa_analyse_binary_expr(c, expr);
		case EXPR_IDENTIFIER:
			return sa_analyse_ident_expr(c, expr);
		case EXPR_COND:
			return sa_analyse_expr_list(c, expr->cond_expr);
		case EXPR_UNARY:
		case EXPR_POST_UNARY:
			return sa_analyse_unary_expr(c, expr);
		case EXPR_RVALUE:
		case EXPR_RECAST:
		case EXPR_DISCARD:
			expr = expr->inner_expr;
			goto RETRY;
		case EXPR_TWO:
			sa_analyse_expr(c, expr->two_expr.first);
			expr = expr->two_expr.last;
			goto RETRY;
		case EXPR_ACCESS_RESOLVED:
		case EXPR_ADDR_CONVERSION:
		case EXPR_ASM:
		case EXPR_BENCHMARK_HOOK:
		case EXPR_BITACCESS:
		case EXPR_BITASSIGN:
		case EXPR_BUILTIN:
		case EXPR_BUILTIN_ACCESS:
		case EXPR_CATCH:
		case EXPR_DECL:
		case EXPR_DEFAULT_ARG:
		case EXPR_DESIGNATED_INITIALIZER_LIST:
		case EXPR_DESIGNATOR:
		case EXPR_ENUM_FROM_ORD:
		case EXPR_EXPRESSION_LIST:
		case EXPR_EXT_TRUNC:
		case EXPR_FLOAT_TO_INT:
		case EXPR_FORCE_UNWRAP:
		case EXPR_INITIALIZER_LIST:
		case EXPR_INT_TO_BOOL:
		case EXPR_INT_TO_FLOAT:
		case EXPR_INT_TO_PTR:
		case EXPR_LAMBDA:
		case EXPR_LAST_FAULT:
		case EXPR_MACRO_BLOCK:
		case EXPR_MACRO_BODY_EXPANSION:
		case EXPR_MAKE_ANY:
		case EXPR_MAKE_SLICE:
		case EXPR_MEMBER_GET:
		case EXPR_MEMBER_SET:
		case EXPR_NAMED_ARGUMENT:
		case EXPR_NAMED_EVAL_ARGUMENT:
		case EXPR_OPTIONAL:
		case EXPR_POINTER_OFFSET:
		case EXPR_PTR_ACCESS:
		case EXPR_PTR_TO_INT:
		case EXPR_RETHROW:
		case EXPR_RETVAL:
		case EXPR_SCALAR_TO_VECTOR:
		case EXPR_SLICE:
		case EXPR_SLICE_ASSIGN:
		case EXPR_SLICE_COPY:
		case EXPR_SLICE_LEN:
		case EXPR_SLICE_TO_VEC_ARRAY:
		case EXPR_SUBSCRIPT:
		case EXPR_SUBSCRIPT_ADDR:
		case EXPR_SWIZZLE:
		case EXPR_TERNARY:
		case EXPR_TEST_HOOK:
		case EXPR_TRY:
		case EXPR_TRY_UNWRAP_CHAIN:
		case EXPR_TYPEID_INFO:
		case EXPR_VECTOR_FROM_ARRAY:
		case EXPR_VECTOR_TO_ARRAY:
			c->bail = true;
			return NULLABILITY_UNKNOWN;
	}
	UNREACHABLE
}

static SANullability sa_analyse_const_expr(SAContext *c, Expr *expr)
{
	switch (expr->const_expr.const_kind)
	{
		case CONST_POINTER:
			return expr->const_expr.ptr ? NULLABILITY_NOT_NULL : NULLABILITY_NULL;
		case CONST_REF:
			return NULLABILITY_NOT_NULL;
		default:
			return NULLABILITY_UNKNOWN;
	}
	UNREACHABLE
}

static SANullability sa_analyse_binary_expr(SAContext *c, Expr *expr)
{
	Expr *left = exprptr(expr->binary_expr.left);
	Expr *right = exprptr(expr->binary_expr.right);
	BinaryOp op = expr->binary_expr.operator;
	switch (op)
	{
		case BINARYOP_ASSIGN:
		{
			SANullability value = sa_analyse_expr(c, right);
			sa_analyse_expr(c, left);
			sa_assign_slot(c, left, value);
			return value;
		}
		case BINARYOP_AND:
		case BINARYOP_OR:
		{
			sa_analyse_expr(c, left);
			SAState skipped = c->state;
			sa_analyse_expr(c, right);
			c->state = sa_state_join(skipped, c->state);
			return NULLABILITY_UNKNOWN;
		}
		case BINARYOP_ELSE:
			c->bail = true;
			return NULLABILITY_UNKNOWN;
		default:
			sa_analyse_expr(c, left);
			sa_analyse_expr(c, right);
			if (op > BINARYOP_ASSIGN) sa_assign_slot(c, left, NULLABILITY_UNKNOWN);
			return NULLABILITY_UNKNOWN;
	}
}

static SANullability sa_analyse_call_expr(SAContext *c, Expr *expr)
{
	if (expr->call_expr.is_builtin || expr->call_expr.varargs || expr->call_expr.va_is_splat
		|| expr->call_expr.function_contracts || expr->call_expr.no_return)
	{
		c->bail = true;
		return NULLABILITY_UNKNOWN;
	}
	if (!expr->call_expr.is_func_ref) sa_analyse_expr(c, exprptr(expr->call_expr.function));
	Decl *callee = expr->call_expr.is_func_ref ? declptr(expr->call_expr.func_ref) : NULL;
	Decl **params = callee && callee->decl_kind == DECL_FUNC ? callee->func_decl.signature.params : NULL;
	int param_count = vec_size(params);
	SANullability values[MAX_PARAMS];
	FOREACH_IDX(i, Expr *, arg, expr->call_expr.arguments)
	{
		SANullability v = sa_analyse_expr(c, arg);
		if (i < MAX_PARAMS) values[i] = v;
	}
	if (c->bail) return NULLABILITY_UNKNOWN;
	FOREACH_IDX(j, Expr *, arg, expr->call_expr.arguments)
	{
		if (j >= param_count || j >= MAX_PARAMS || !params[j] || !params[j]->var.not_null) continue;
		if (values[j] != NULLABILITY_NULL) continue;
		print_error_at(arg->loc, params[j]->var.self_addr
			? "This method may not be called on a value known to be null."
			: "This value is null, but this parameter must be non-null.");
		sema_note_prev_at(params[j]->loc, "The definition is here.");
	}
	return NULLABILITY_UNKNOWN;
}

static SANullability sa_analyse_ident_expr(SAContext *c, Expr *expr)
{
	SASlotId slot = sa_find_slot(c, expr->ident_expr);
	if (slot < 0) return NULLABILITY_UNKNOWN;
	if (BIT_SET(c->state.is_null, slot)) return NULLABILITY_NULL;
	if (BIT_SET(c->state.not_null, slot)) return NULLABILITY_NOT_NULL;
	return NULLABILITY_UNKNOWN;
}

static SANullability sa_analyse_expr_list(SAContext *c, Expr **exprs)
{
	SANullability value = NULLABILITY_UNKNOWN;
	FOREACH(Expr *, arg, exprs) value = sa_analyse_expr(c, arg);
	return value;
}

static SANullability sa_analyse_unary_expr(SAContext *c, Expr *expr)
{
	Expr *inner = expr->unary_expr.expr;
	switch (expr->unary_expr.operator)
	{
		case UNARYOP_ERROR:
			UNREACHABLE;
		case UNARYOP_NOT:
		case UNARYOP_NEG:
		case UNARYOP_BITNEG:
		case UNARYOP_PLUS:
			sa_analyse_expr(c, inner);
			return NULLABILITY_UNKNOWN;
		case UNARYOP_INC:
		case UNARYOP_DEC:
			sa_analyse_expr(c, inner);
			sa_assign_slot(c, inner, NULLABILITY_UNKNOWN);
			return NULLABILITY_UNKNOWN;
		case UNARYOP_TADDR:
			sa_analyse_expr(c, inner);
			return NULLABILITY_NOT_NULL;
		case UNARYOP_ADDR:
			c->bail = true;
			return NULLABILITY_UNKNOWN;
		case UNARYOP_DEREF:
			if (sa_analyse_expr(c, inner) == NULLABILITY_NULL && !c->bail)
			{
				print_error_at(expr->loc, "This would dereference a null pointer.");
			}
			return NULLABILITY_UNKNOWN;
	}
	UNREACHABLE
}
