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
			case TYPE_POINTER:
				type = type->pointer;
				goto RETRY;
			case TYPE_SLICE:
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
	FOREACH(Expr *, arg, exprlist) sema_trace_expr_liveness(arg);
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
}
static void sema_trace_asm_arg_list(ExprAsmArg **list)
{
	FOREACH(ExprAsmArg *, asm_arg, list)
	{
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
	}
}

static void sema_trace_stmt_liveness(Ast *ast)
{
	if (!ast) return;
	switch (ast->ast_kind)
	{
		case AST_POISONED:
		case CT_AST:
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
		case AST_ASM_LABEL:
			return;
		case AST_CT_COMPOUND_STMT:
			sema_trace_stmt_chain_liveness(ast->ct_compound_stmt);
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
			if (compile_asserts() || expr_is_pure(e))
			{
				sema_trace_expr_liveness(e);
			}
			if (compile_asserts())
			{
				sema_trace_exprid_liveness(ast->assert_stmt.message);
				sema_trace_expr_list_liveness(ast->assert_stmt.args);
			}
			return;
		}
		case AST_DECLS_STMT:
		{
			FOREACH(Decl *, decl, ast->decls_stmt) sema_trace_decl_liveness(decl);
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
			sema_trace_exprid_liveness(ast->switch_stmt.cond);
			{
				FOREACH(Ast *, casestm, ast->switch_stmt.cases) sema_trace_stmt_liveness(casestm);
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
	if (!const_init) return;
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
			for (ArrayIndex i = 0; i < (ArrayIndex)size; i++)
			{
				sema_trace_const_initializer_liveness(elements[i]);
			}
			return;
		}
		case CONST_INIT_ARRAY:
		{
			FOREACH(ConstInitializer *, i, const_init->init_array.elements) sema_trace_const_initializer_liveness(i);
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
			for (ArrayIndex i = 0; i < count; i++)
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
		case NON_RUNTIME_EXPR:
		case EXPR_SUBSCRIPT_ASSIGN:
		case EXPR_OPERATOR_CHARS:
		case EXPR_MEMBER_GET:
		case EXPR_MEMBER_SET:
		case EXPR_NAMED_ARGUMENT:
		case UNRESOLVED_EXPRS:
			UNREACHABLE
		case EXPR_TWO:
			sema_trace_expr_liveness(expr->two_expr.first);
			sema_trace_expr_liveness(expr->two_expr.last);
			return;
		case EXPR_DESIGNATOR:
			sema_trace_expr_liveness(expr->designator_expr.value);
			return;
		case EXPR_BUILTIN:
			return;
		case EXPR_MAKE_SLICE:
			expr = expr->make_slice_expr.ptr;
			goto RETRY;
		case EXPR_MAKE_ANY:
			sema_trace_expr_liveness(expr->make_any_expr.typeid);
			expr = expr->make_any_expr.inner;
			goto RETRY;
		case EXPR_ACCESS_RESOLVED:
		case EXPR_BITACCESS:
			sema_trace_decl_liveness(expr->access_resolved_expr.ref);
			expr = expr->access_resolved_expr.parent;
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
				if (expr->call_expr.va_is_splat)
				{
					sema_trace_expr_liveness(expr->call_expr.vasplat);
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
			sema_trace_astid_liveness(expr->call_expr.function_contracts);
			sema_trace_decl_liveness(declptr(expr->call_expr.func_ref));
			return;
		}
		case EXPR_FORCE_UNWRAP:
		case EXPR_RETHROW:
		case EXPR_OPTIONAL:
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
		case EXPR_VECTOR_FROM_ARRAY:
		case EXPR_RVALUE:
		case EXPR_RECAST:
		case EXPR_DISCARD:
		case EXPR_ADDR_CONVERSION:
			expr = expr->inner_expr;
			goto RETRY;
		case EXPR_DEFAULT_ARG:
			expr = expr->default_arg_expr.inner;
			goto RETRY;
		case EXPR_BUILTIN_ACCESS:
			expr = exprptr(expr->builtin_access_expr.inner);
			goto RETRY;
		case EXPR_CATCH:
		{
			if (expr->catch_expr.decl)
			{
				sema_trace_decl_liveness(expr->catch_expr.decl);
			}
			FOREACH(Expr *, e, expr->catch_expr.exprs) sema_trace_expr_liveness(e);
			return;
		}
		case EXPR_CONST:
			switch (expr->const_expr.const_kind)
			{
				case CONST_TYPEID:
					sema_trace_type_liveness(expr->const_expr.typeid);
					return;
				case CONST_FLOAT:
				case CONST_INTEGER:
				case CONST_BOOL:
				case CONST_BYTES:
				case CONST_STRING:
				case CONST_POINTER:
				case CONST_UNTYPED_LIST:
				case CONST_MEMBER:
					return;
				case CONST_FAULT:
					sema_trace_decl_liveness(expr->const_expr.fault);
					return;
				case CONST_ENUM:
					sema_trace_decl_liveness(expr->const_expr.enum_val);
					return;
				case CONST_REF:
					sema_trace_decl_liveness(expr->const_expr.global_ref);
					return;
				case CONST_SLICE:
					sema_trace_const_initializer_liveness(expr->const_expr.slice_init);
					return;
				case CONST_INITIALIZER:
					sema_trace_const_initializer_liveness(expr->const_expr.initializer);
					return;
			}
		case EXPR_COND:
		{
			FOREACH(Expr *, e, expr->cond_expr) sema_trace_expr_liveness(e);
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
		case EXPR_IDENTIFIER:
			sema_trace_decl_liveness(expr->ident_expr);
			return;
		case EXPR_INITIALIZER_LIST:
		{
			FOREACH(Expr *, e, expr->initializer_list) sema_trace_expr_liveness(e);
			return;
		}
		case EXPR_LAMBDA:
			UNREACHABLE
		case EXPR_MACRO_BLOCK:
		{
			FOREACH(Decl *, val, expr->macro_block.params) sema_trace_decl_liveness(val);
			sema_trace_stmt_chain_liveness(expr->macro_block.first_stmt);
			return;
		}
		case EXPR_MACRO_BODY_EXPANSION:
		{
			FOREACH(Decl *, arg, expr->body_expansion_expr.declarations) sema_trace_decl_liveness(arg);
			FOREACH(Expr *, arg, expr->body_expansion_expr.values) sema_trace_expr_liveness(arg);
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
			sema_trace_expr_liveness(exprptr(expr->slice_expr.expr));
			switch (expr->slice_expr.range.range_type)
			{
				case RANGE_SINGLE_ELEMENT:
					UNREACHABLE
				case RANGE_CONST_RANGE:
					return;
				case RANGE_DYNAMIC:
					sema_trace_expr_liveness(exprptr(expr->slice_expr.range.start));
					sema_trace_expr_liveness(exprptrzero(expr->slice_expr.range.end));
					return;
				case RANGE_CONST_END:
				case RANGE_CONST_LEN:
					sema_trace_expr_liveness(exprptr(expr->slice_expr.range.start));
					return;
			}
			UNREACHABLE
		case EXPR_SUBSCRIPT:
		case EXPR_SUBSCRIPT_ADDR:
			sema_trace_expr_liveness(exprptr(expr->subscript_expr.expr));
			sema_trace_expr_liveness(exprptr(expr->subscript_expr.index.expr));
			return;
		case EXPR_SWIZZLE:
			sema_trace_expr_liveness(exprptr(expr->swizzle_expr.parent));
			return;
		case EXPR_TERNARY:
			sema_trace_expr_liveness(exprptr(expr->ternary_expr.cond));
			if (expr->ternary_expr.then_expr)
			{
				sema_trace_expr_liveness(exprptr(expr->ternary_expr.then_expr));
			}
			expr = exprptr(expr->ternary_expr.else_expr);
			goto RETRY;
		case EXPR_BENCHMARK_HOOK:
		case EXPR_TEST_HOOK:
			return;
		case EXPR_TYPEID_INFO:
			sema_trace_exprid_liveness(expr->typeid_info_expr.parent);
			return;
		case EXPR_TRY:
			sema_trace_expr_liveness(expr->try_expr.optional);
			if (expr->try_expr.assign_existing)
			{
				sema_trace_expr_liveness(expr->try_expr.lhs);
			}
			else
			{
				sema_trace_decl_liveness(expr->try_expr.decl);
			}
			return;
		case EXPR_TRY_UNWRAP_CHAIN:
			sema_trace_expr_list_liveness(expr->try_unwrap_chain_expr);
			return;
		case EXPR_INT_TO_BOOL:
			sema_trace_expr_liveness(expr->int_to_bool_expr.inner);
			return;
		case EXPR_EXT_TRUNC:
			sema_trace_expr_liveness(expr->ext_trunc_expr.inner);
			return;
		case EXPR_LAST_FAULT:
			return;
	}
	UNREACHABLE
}

void sema_trace_liveness(void)
{
	if (compiler.context.main)
	{
		sema_trace_decl_liveness(compiler.context.main);
	}
	bool keep_tests = compiler.build.testing;
	bool keep_benchmarks = compiler.build.benchmarking;
	FOREACH(Decl *, function, compiler.context.method_extensions)
	{
		if (function->func_decl.attr_dynamic) function->no_strip = true;
		if (function->is_export || function->no_strip) sema_trace_decl_liveness(function);
	}
	FOREACH(Module *, module, compiler.context.module_list)
	{
		FOREACH(CompilationUnit *, unit, module->units)
		{
			FOREACH(Decl *, function, unit->functions)
			{
				if (function->is_export || function->no_strip || function->func_decl.attr_finalizer ||
				    function->func_decl.attr_init ||
				    (function->func_decl.attr_test && keep_tests) ||
				    (function->func_decl.attr_benchmark && keep_benchmarks))
					sema_trace_decl_liveness(function);
			}
			FOREACH(Decl *, method, unit->methods)
			{
				if (method->is_export || method->no_strip) sema_trace_decl_liveness(method);
			}
			FOREACH(Decl *, var, unit->vars)
			{
				if (var->is_export || var->no_strip) sema_trace_decl_liveness(var);
			}
			FOREACH(Decl *, method, unit->local_method_extensions)
			{
				if (method->is_export || method->no_strip) sema_trace_decl_liveness(method);
			}
		}
	}
}

INLINE void sema_trace_enum_associated(Decl *decl)
{
	sema_trace_type_liveness(decl->enums.type_info->type);
	FOREACH(Decl *, enum_constant, decl->enums.values)
	{
		sema_trace_decl_liveness(enum_constant);
	}
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
		if (method->func_decl.attr_dynamic)
		{
			sema_trace_decl_liveness(method);
		}
	}
}
static void sema_trace_func_liveness(Signature *sig)
{
	FOREACH(Decl *, param, sig->params) sema_trace_decl_liveness(param);
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
		case DECL_ALIAS:
			decl = decl->define_decl.alias;
			goto RETRY;
		case DECL_ENUM:
			sema_trace_decl_dynamic_methods(decl);
			sema_trace_enum_associated(decl);
			return;
		case DECL_DISTINCT:
			sema_trace_type_liveness(decl->distinct->type);
			FALLTHROUGH;
		case DECL_CONST_ENUM:
		case DECL_BITSTRUCT:
		case DECL_STRUCT:
		case DECL_UNION:
		case DECL_INTERFACE:
			sema_trace_decl_dynamic_methods(decl);
			return;
		case DECL_ENUM_CONSTANT:
			if (!decl->enum_constant.is_raw)
			{
				sema_trace_expr_list_liveness(decl->enum_constant.associated);
			}
			return;
		case DECL_POISONED:
		case DECL_ATTRIBUTE:
		case DECL_FAULT:
			return;
		case DECL_ALIAS_PATH:
		case DECL_BODYPARAM:
		case DECL_CT_ASSERT:
		case DECL_CT_ECHO:
		case DECL_CT_EXEC:
		case DECL_CT_INCLUDE:
		case DECL_GROUP:
		case DECL_IMPORT:
		case DECL_LABEL:
		case DECL_MACRO:
			UNREACHABLE
		case DECL_FNTYPE:
			sema_trace_func_liveness(&decl->fntype_decl);
			return;
		case DECL_FUNC:
			sema_trace_func_liveness(&decl->func_decl.signature);
			sema_trace_stmt_liveness(astptrzero(decl->func_decl.body));
			return;
		case DECL_VAR:
			switch (decl->var.kind)
			{
				case VARDECL_PARAM_CT_TYPE:
				case VARDECL_LOCAL_CT_TYPE:
				case VARDECL_REWRAPPED:
				case VARDECL_UNWRAPPED:
					break;
				case VARDECL_PARAM_EXPR:
					// These are never traced, they are folded in use.
					break;
				case VARDECL_PARAM_CT:
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