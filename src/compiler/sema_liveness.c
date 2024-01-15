// Copyright (c) 2023 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "sema_internal.h"

static void sema_trace_expr_liveness(Expr *expr);
static void sema_trace_stmt_liveness(Ast *ast);
static void sema_trace_decl_liveness(Decl *decl);

INLINE void sema_trace_type_liveness(Type *type)
{
	if (!type) return;
RETRY:
	if (!type_is_user_defined(type))
	{
		type = type->canonical;
		switch (type->type_kind)
		{
			case TYPE_INFPTR:
			case TYPE_POINTER:
				type = type->pointer;
				goto RETRY;
			case TYPE_SUBARRAY:
			case TYPE_ARRAY:
			case TYPE_INFERRED_ARRAY:
			case TYPE_FLEXIBLE_ARRAY:
				type = type->array.base;
				goto RETRY;
			default:
				return;
		}
	}
	sema_trace_decl_liveness(type->decl);
}

INLINE void sema_trace_exprid_liveness(ExprId expr)
{
	if (expr) sema_trace_expr_liveness(exprptr(expr));
}

INLINE void sema_trace_astid_liveness(AstId astid)
{
	if (astid) sema_trace_stmt_liveness(astptr(astid));
}

static void sema_trace_expr_list_liveness(Expr **exprlist)
{
	FOREACH_BEGIN(Expr *arg, exprlist)
		sema_trace_expr_liveness(arg);
	FOREACH_END();
}

static void sema_trace_stmt_chain_liveness(AstId astid)
{
	AstId current = astid;
	while (current)
	{
		Ast *stmt = ast_next(&current);
		sema_trace_stmt_liveness(stmt);
		switch (stmt->ast_kind)
		{
			case AST_RETURN_STMT:
			case AST_BREAK_STMT:
			case AST_CONTINUE_STMT:
			case AST_NEXTCASE_STMT:
				return;
			default:
				break;
		}
	}
	REMINDER("Optimize to ignore after return");
	return;
}
static void sema_trace_asm_arg_list(ExprAsmArg **list)
{
	FOREACH_BEGIN(ExprAsmArg *asm_arg, list)
		switch (asm_arg->kind)
		{
			case ASM_ARG_ADDR:
			case ASM_ARG_ADDROF:
				TODO
			case ASM_ARG_REG:
			case ASM_ARG_INT:
				continue;
			case ASM_ARG_MEMVAR:
			case ASM_ARG_REGVAR:
				sema_trace_decl_liveness(asm_arg->ident.ident_decl);
				continue;
			case ASM_ARG_VALUE:
				sema_trace_exprid_liveness(asm_arg->expr_id);
				continue;
		}
		UNREACHABLE
	FOREACH_END();
}

static void sema_trace_stmt_liveness(Ast *ast)
{
	if (!ast) return;
	switch (ast->ast_kind)
	{
		case AST_POISONED:
		case AST_CT_ECHO_STMT:
		case AST_CT_ELSE_STMT:
		case AST_CT_FOREACH_STMT:
		case AST_CT_FOR_STMT:
		case AST_CT_IF_STMT:
		case AST_CT_ASSERT:
		case AST_CT_SWITCH_STMT:
		case AST_CONTRACT:
		case AST_FOREACH_STMT:
		case AST_CONTRACT_FAULT:
			UNREACHABLE
		case AST_ASM_STMT:
			sema_trace_expr_list_liveness(ast->asm_stmt.args);
			return;
		case AST_DEFER_STMT:
			sema_trace_stmt_liveness(astptr(ast->defer_stmt.body));
			return;
		case AST_NOP_STMT:
			return;
		case AST_COMPOUND_STMT:
			sema_trace_stmt_chain_liveness(ast->compound_stmt.first_stmt);
			return;
		case AST_EXPR_STMT:
			sema_trace_expr_liveness(ast->expr_stmt);
			return;
		case AST_DECLARE_STMT:
			sema_trace_decl_liveness(ast->declare_stmt);
			return;
		case AST_RETURN_STMT:
		case AST_BLOCK_EXIT_STMT:
			sema_trace_expr_liveness(ast->return_stmt.expr);
			sema_trace_astid_liveness(ast->return_stmt.cleanup);
			if (ast->return_stmt.cleanup != ast->return_stmt.cleanup_fail)
			{
				sema_trace_astid_liveness(ast->return_stmt.cleanup_fail);
			}
			return;
		case AST_ASM_BLOCK_STMT:
			if (ast->asm_block_stmt.is_string)
			{
				sema_trace_exprid_liveness(ast->asm_block_stmt.asm_string);
				return;
			}
			sema_trace_stmt_chain_liveness(ast->asm_block_stmt.block->asm_stmt);
			sema_trace_asm_arg_list(ast->asm_block_stmt.block->input);
			sema_trace_asm_arg_list(ast->asm_block_stmt.block->output_vars);
			return;
		case AST_ASSERT_STMT:
		{
			Expr *e = exprptr(ast->assert_stmt.expr);
			if (safe_mode_enabled() || expr_is_pure(e))
			{
				sema_trace_expr_liveness(e);
			}
			return;
		}
		case AST_DECLS_STMT:
		{
			FOREACH_BEGIN(Decl *decl, ast->decls_stmt)
				sema_trace_decl_liveness(decl);
			FOREACH_END();
			return;
		}
		case AST_FOR_STMT:
			sema_trace_exprid_liveness(ast->for_stmt.cond);
			sema_trace_exprid_liveness(ast->for_stmt.init);
			sema_trace_exprid_liveness(ast->for_stmt.incr);
			sema_trace_astid_liveness(ast->for_stmt.body);
			return;
		case AST_IF_STMT:
			sema_trace_exprid_liveness(ast->if_stmt.cond);
			sema_trace_astid_liveness(ast->if_stmt.then_body);
			sema_trace_astid_liveness(ast->if_stmt.else_body);
			return;
		case AST_SWITCH_STMT:
		case AST_IF_CATCH_SWITCH_STMT:
			sema_trace_exprid_liveness(ast->switch_stmt.cond);
			{
				FOREACH_BEGIN(Ast *casestm, ast->switch_stmt.cases)
					sema_trace_stmt_liveness(casestm);
				FOREACH_END();
			}
			return;
		case AST_CASE_STMT:
			sema_trace_exprid_liveness(ast->case_stmt.expr);
			sema_trace_exprid_liveness(ast->case_stmt.to_expr);
			sema_trace_stmt_liveness(ast->case_stmt.body);
			return;
		case AST_DEFAULT_STMT:
			sema_trace_stmt_liveness(ast->case_stmt.body);
			return;
		case AST_NEXTCASE_STMT:
			sema_trace_stmt_chain_liveness(ast->nextcase_stmt.defer_id);
			sema_trace_expr_liveness(ast->nextcase_stmt.switch_expr);
			return;
		case AST_BREAK_STMT:
		case AST_CONTINUE_STMT:
			sema_trace_stmt_chain_liveness(ast->contbreak_stmt.defers);
			return;
	}
	UNREACHABLE
}

static void sema_trace_const_initializer_liveness(ConstInitializer *const_init)
{
	RETRY:
	switch (const_init->kind)
	{
		case CONST_INIT_ZERO:
			return;
		case CONST_INIT_ARRAY_VALUE:
			const_init = const_init->init_array_value.element;
			goto RETRY;
		case CONST_INIT_ARRAY_FULL:
		{
			Type *array_type = const_init->type;
			ConstInitializer **elements = const_init->init_array_full;
			ArraySize size = array_type->array.len;
			for (MemberIndex i = 0; i < (MemberIndex)size; i++)
			{
				sema_trace_const_initializer_liveness(elements[i]);
			}
			return;
		}
		case CONST_INIT_ARRAY:
		{
			FOREACH_BEGIN(ConstInitializer *i, const_init->init_array.elements)
				sema_trace_const_initializer_liveness(i);
			FOREACH_END();
			return;
		}
		case CONST_INIT_UNION:
			const_init = const_init->init_union.element;
			goto RETRY;
		case CONST_INIT_STRUCT:
		{
			Decl *decl = const_init->type->decl;
			Decl **members = decl->strukt.members;
			uint32_t count = vec_size(members);
			if (decl->decl_kind == DECL_UNION && count) count = 1;
			for (MemberIndex i = 0; i < count; i++)
			{
				sema_trace_const_initializer_liveness(const_init->init_struct[i]);
			}
			return;
		}
		case CONST_INIT_VALUE:
			sema_trace_expr_liveness(const_init->init_value);
			return;
	}
	UNREACHABLE
}


static void sema_trace_expr_liveness(Expr *expr)
{
RETRY:
	if (!expr) return;
	sema_trace_type_liveness(expr->type);
	switch (expr->expr_kind)
	{
		case EXPR_SUBSCRIPT_ASSIGN:
		case EXPR_GROUP:
		case EXPR_OPERATOR_CHARS:
		case EXPR_VASPLAT:
		case EXPR_POISONED:
		case EXPR_COMPILER_CONST:
		case EXPR_CT_ARG:
		case EXPR_CT_CALL:
		case EXPR_CT_DEFINED:
		case EXPR_CT_IS_CONST:
		case EXPR_CT_EVAL:
		case EXPR_CT_IDENT:
		case EXPR_ANYSWITCH:
		case EXPR_GENERIC_IDENT:
		case EXPR_EMBED:
		case EXPR_CT_CASTABLE:
		case EXPR_CT_AND_OR:
		case EXPR_MACRO_BODY:
		case EXPR_OTHER_CONTEXT:
			UNREACHABLE
		case EXPR_DESIGNATOR:
			sema_trace_expr_liveness(expr->designator_expr.value);
			return;
		case EXPR_HASH_IDENT:
		case EXPR_STRINGIFY:
		case EXPR_TYPEINFO:
		case EXPR_BUILTIN:
			return;
		case EXPR_ACCESS:
		case EXPR_BITACCESS:
			sema_trace_decl_liveness(expr->access_expr.ref);
			expr = expr->access_expr.parent;
			goto RETRY;
		case EXPR_ASM:
			switch (expr->expr_asm_arg.kind)
			{
				case ASM_ARG_REG:
				case ASM_ARG_ADDROF:
				case ASM_ARG_REGVAR:
				case ASM_ARG_INT:
				case ASM_ARG_MEMVAR:
					return;
				case ASM_ARG_VALUE:
				case ASM_ARG_ADDR:
					sema_trace_expr_liveness(exprptr(expr->expr_asm_arg.expr_id));
					return;
			}
			UNREACHABLE
		case EXPR_BINARY:
		case EXPR_BITASSIGN:
			sema_trace_expr_liveness(exprptr(expr->binary_expr.left));
			sema_trace_expr_liveness(exprptr(expr->binary_expr.right));
			return;
		case EXPR_CALL:
		{
			sema_trace_expr_list_liveness(expr->call_expr.arguments);
			if (expr->call_expr.varargs)
			{
				if (expr->call_expr.splat_vararg)
				{
					sema_trace_expr_liveness(expr->call_expr.splat);
				}
				else
				{
					sema_trace_expr_list_liveness(expr->call_expr.varargs);
				}
			}
			if (expr->call_expr.is_builtin) return;
			if (!expr->call_expr.is_func_ref)
			{
				sema_trace_expr_liveness(exprptr(expr->call_expr.function));
				return;
			}
			sema_trace_decl_liveness(declptr(expr->call_expr.func_ref));
			return;
		}
		case EXPR_CAST:
			expr = exprptr(expr->cast_expr.expr);
			goto RETRY;
		case EXPR_FORCE_UNWRAP:
		case EXPR_RETHROW:
		case EXPR_OPTIONAL:
			expr = expr->inner_expr;
			goto RETRY;
		case EXPR_BUILTIN_ACCESS:
			expr = exprptr(expr->builtin_access_expr.inner);
			goto RETRY;
		case EXPR_CATCH_UNWRAP:
		{
			if (expr->catch_unwrap_expr.lhs)
			{
				sema_trace_expr_liveness(expr->catch_unwrap_expr.lhs);
			}
			else if (expr->catch_unwrap_expr.decl)
			{
				sema_trace_decl_liveness(expr->catch_unwrap_expr.decl);
			}
			FOREACH_BEGIN(Expr *e, expr->catch_unwrap_expr.exprs)
				sema_trace_expr_liveness(e);
			FOREACH_END();
			return;
		}
		case EXPR_CONST:
			if (expr->const_expr.const_kind != CONST_INITIALIZER) return;
			{
				sema_trace_const_initializer_liveness(expr->const_expr.initializer);
			}
			return;
		case EXPR_COMPOUND_LITERAL:
			sema_trace_expr_liveness(expr->expr_compound_literal.initializer);
			return;
		case EXPR_COND:
		{
			FOREACH_BEGIN(Expr *e, expr->cond_expr)
				sema_trace_expr_liveness(e);
			FOREACH_END();
			return;
		}
		case EXPR_DECL:
			sema_trace_decl_liveness(expr->decl_expr);
			return;
		case EXPR_EXPRESSION_LIST:
			sema_trace_expr_list_liveness(expr->expression_list);
			return;
		case EXPR_DESIGNATED_INITIALIZER_LIST:
			sema_trace_expr_list_liveness(expr->designated_init_list);
			return;
		case EXPR_EXPR_BLOCK:
		{
			AstId current = expr->expr_block.first_stmt;
			if (!current) return;
			do
			{
				Ast *value = ast_next(&current);
				sema_trace_stmt_liveness(value);
			} while (current);
			return;
		}
		case EXPR_IDENTIFIER:
			sema_trace_decl_liveness(expr->identifier_expr.decl);
			return;
		case EXPR_INITIALIZER_LIST:
		{
			FOREACH_BEGIN(Expr *e, expr->initializer_list)
				sema_trace_expr_liveness(e);
			FOREACH_END();
			return;
		}
		case EXPR_LAMBDA:
			sema_trace_decl_liveness(expr->lambda_expr);
			return;
		case EXPR_MACRO_BLOCK:
		{
			FOREACH_BEGIN(Decl *val, expr->macro_block.params)
				sema_trace_decl_liveness(val);
			FOREACH_END();
			sema_trace_stmt_chain_liveness(expr->macro_block.first_stmt);
			return;
		}
		case EXPR_MACRO_BODY_EXPANSION:
		{
			FOREACH_BEGIN(Decl *arg, expr->body_expansion_expr.declarations)
				sema_trace_decl_liveness(arg);
			FOREACH_END();
			FOREACH_BEGIN(Expr *arg, expr->body_expansion_expr.values)
				sema_trace_expr_liveness(arg);
			FOREACH_END();
			sema_trace_stmt_liveness(astptrzero(expr->body_expansion_expr.first_stmt));
			return;
		}
		case EXPR_NOP:
			return;
		case EXPR_POINTER_OFFSET:
			sema_trace_expr_liveness(exprptr(expr->pointer_offset_expr.ptr));
			expr = exprptr(expr->pointer_offset_expr.offset);
			goto RETRY;
		case EXPR_POST_UNARY:
		case EXPR_UNARY:
			expr = expr->unary_expr.expr;
			goto RETRY;
		case EXPR_RETVAL:
			return;
		case EXPR_SLICE_ASSIGN:
		case EXPR_SLICE_COPY:
			sema_trace_expr_liveness(exprptr(expr->slice_assign_expr.left));
			sema_trace_expr_liveness(exprptr(expr->slice_assign_expr.right));
			return;
		case EXPR_SLICE:
		case EXPR_SUBSCRIPT:
		case EXPR_SUBSCRIPT_ADDR:
			sema_trace_expr_liveness(exprptr(expr->subscript_expr.expr));
			sema_trace_expr_liveness(exprptr(expr->subscript_expr.range.start));
			sema_trace_expr_liveness(exprptrzero(expr->subscript_expr.range.end));
			return;
		case EXPR_SWIZZLE:
			sema_trace_expr_liveness(exprptr(expr->swizzle_expr.parent));
			return;
		case EXPR_TERNARY:
		{
			REMINDER("Tracing ternary can be optimized");
			sema_trace_expr_liveness(exprptr(expr->ternary_expr.cond));
			if (expr->ternary_expr.then_expr)
			{
				sema_trace_expr_liveness(exprptr(expr->ternary_expr.then_expr));
			}
			expr = exprptr(expr->ternary_expr.else_expr);
			goto RETRY;
		}
		case EXPR_BENCHMARK_HOOK:
		case EXPR_TEST_HOOK:
			return;
		case EXPR_TYPEID_INFO:
			sema_trace_exprid_liveness(expr->typeid_info_expr.parent);
			return;
		case EXPR_TRY_UNWRAP:
			sema_trace_expr_liveness(expr->try_unwrap_expr.optional);
			if (expr->try_unwrap_expr.assign_existing)
			{
				sema_trace_expr_liveness(expr->try_unwrap_expr.lhs);
			}
			else
			{
				sema_trace_decl_liveness(expr->try_unwrap_expr.decl);
			}
			return;
		case EXPR_TRY_UNWRAP_CHAIN:
			sema_trace_expr_list_liveness(expr->try_unwrap_chain_expr);
			return;
		case EXPR_TYPEID:
			return;
	}
	UNREACHABLE
}

void sema_trace_liveness(void)
{
	if (global_context.main)
	{
		sema_trace_decl_liveness(global_context.main);
	}
	bool keep_tests = active_target.testing;
	bool keep_benchmarks = active_target.benchmarking;
	FOREACH_BEGIN(Decl *function, global_context.method_extensions)
		if (function->func_decl.attr_dynamic) function->no_strip = true;
		if (function->is_export || function->no_strip) sema_trace_decl_liveness(function);
	FOREACH_END();
	FOREACH_BEGIN(Module *module, global_context.module_list)
		FOREACH_BEGIN(CompilationUnit *unit, module->units)
			FOREACH_BEGIN(Decl *function, unit->functions)
				if (function->is_export || function->no_strip || function->func_decl.attr_finalizer || function->func_decl.attr_init ||
				   (function->func_decl.attr_test && keep_tests) ||
				   (function->func_decl.attr_benchmark && keep_benchmarks)) sema_trace_decl_liveness(function);
			FOREACH_END();
			FOREACH_BEGIN(Decl *method, unit->methods)
				if (method->is_export || method->no_strip) sema_trace_decl_liveness(method);
			FOREACH_END();
			FOREACH_BEGIN(Decl *var, unit->vars)
				if (var->is_export || var->no_strip) sema_trace_decl_liveness(var);
			FOREACH_END();
			FOREACH_BEGIN(Decl *method, unit->local_method_extensions)
				if (method->is_export || method->no_strip) sema_trace_decl_liveness(method);
			FOREACH_END();
		FOREACH_END();
	FOREACH_END();
}


INLINE void sema_trace_decl_dynamic_methods(Decl *decl)
{
	Decl **methods = decl->methods;
	unsigned method_count = vec_size(methods);
	if (!method_count) return;
	for (unsigned i = 0; i < method_count; i++)
	{
		Decl *method = methods[i];
		if (method->decl_kind == DECL_MACRO) continue;
		if (method->func_decl.attr_dynamic || method->func_decl.attr_default)
		{
			sema_trace_decl_liveness(method);
		}
	}
}
static void sema_trace_func_liveness(Signature *sig)
{
	FOREACH_BEGIN(Decl *param, sig->params)
		sema_trace_decl_liveness(param);
	FOREACH_END();
	sema_trace_type_liveness(typeget(sig->rtype));
}

static void sema_trace_decl_liveness(Decl *decl)
{
RETRY:
	if (!decl || decl->is_live) return;
	decl->is_live = true;
	switch (decl->decl_kind)
	{
		case DECL_ERASED:
			return;
		case DECL_TYPEDEF:
			sema_trace_type_liveness(decl->type);
			return;
		case DECL_DEFINE:
			decl = decl->define_decl.alias;
			goto RETRY;
		case DECL_DISTINCT:
			sema_trace_type_liveness(decl->distinct->type);
			FALLTHROUGH;
		case DECL_ENUM:
		case DECL_BITSTRUCT:
		case DECL_FAULT:
		case DECL_STRUCT:
		case DECL_UNION:
		case DECL_INTERFACE:
			sema_trace_decl_dynamic_methods(decl);
			return;
		case DECL_POISONED:
		case DECL_ATTRIBUTE:
		case DECL_ENUM_CONSTANT:
		case DECL_FAULTVALUE:
			return;
		case DECL_CT_ASSERT:
		case DECL_CT_ECHO:
		case DECL_CT_EXEC:
		case DECL_IMPORT:
		case DECL_CT_INCLUDE:
		case DECL_LABEL:
		case DECL_MACRO:
		case DECL_BODYPARAM:
		case DECL_GLOBALS:
			UNREACHABLE
		case DECL_FNTYPE:;
			sema_trace_func_liveness(&decl->fntype_decl);
			return;
		case DECL_FUNC:;
			sema_trace_func_liveness(&decl->func_decl.signature);
			sema_trace_stmt_liveness(astptrzero(decl->func_decl.body));
			return;
		case DECL_VAR:
			switch (decl->var.kind)
			{
				case VARDECL_REWRAPPED:
				case VARDECL_UNWRAPPED:
					break;
				case VARDECL_PARAM_EXPR:
				case VARDECL_PARAM_CT:
				case VARDECL_PARAM_REF:
				case VARDECL_PARAM:
					sema_trace_type_liveness(decl->type);
					if (decl->var.init_expr && decl->var.init_expr->resolve_status == RESOLVE_DONE)
					{
						sema_trace_expr_liveness(decl->var.init_expr);
					}
					break;
				default:
					sema_trace_type_liveness(decl->type);
					sema_trace_expr_liveness(decl->var.init_expr);
					break;
			}
			return;
		case DECL_DECLARRAY:
			UNREACHABLE
	}
}