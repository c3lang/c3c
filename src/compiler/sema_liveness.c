// Copyright (c) 2023 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "sema_internal.h"

static void sema_trace_expr_liveness(Module *m, Expr *expr);
static void sema_trace_stmt_liveness(Module *m, Ast *ast);
static void sema_trace_decl_liveness(Module *m, Decl *decl);

INLINE void sema_trace_type_liveness(Module *m, Type *type)
{
	if (!type) return;
	if (type->is_live && type->traced_module == m) return;
RETRY:
	type->is_live = true;
	type->traced_module = m;
	switch (type->type_kind)
	{
		case TYPE_POISONED:
		case TYPE_VOID:
		case TYPE_BOOL:
		case ALL_INTS:
		case ALL_FLOATS:
		case TYPE_ANY:
			return;
		case TYPE_INTERFACE:
			break;
		case TYPE_ANYFAULT:
		case TYPE_TYPEID:
			return;
		case TYPE_FUNC_PTR:
		case TYPE_POINTER:
			type = type->pointer;
			goto RETRY;
		case TYPE_TYPEDEF:
		case TYPE_CONSTSET:
		case TYPE_ENUM:
		case TYPE_FUNC_RAW:
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_BITSTRUCT:
			break;
		case TYPE_ALIAS:
			type = type->canonical;
			goto RETRY;
		case TYPE_UNTYPEDLIST:
			return;
		case TYPE_SLICE:
		case TYPE_ARRAY:
		case TYPE_FLEXIBLE_ARRAY:
		case TYPE_INFERRED_ARRAY:
		case TYPE_VECTOR:
		case TYPE_SIMD_VECTOR:
		case TYPE_INFERRED_VECTOR:
			type = type->array.base;
			goto RETRY;
		case TYPE_OPTIONAL:
			type = type->optional;
			goto RETRY;
		case TYPE_WILDCARD:
		case TYPE_TYPEINFO:
		case TYPE_MEMBER:
		case TYPE_REFLECTION:
			return;
	}
	sema_trace_decl_liveness(m, type->decl);
}

INLINE void sema_trace_exprid_liveness(Module *m, ExprId expr)
{
	if (expr) sema_trace_expr_liveness(m, exprptr(expr));
}

INLINE void sema_trace_astid_liveness(Module *m, AstId astid)
{
	if (astid) sema_trace_stmt_liveness(m, astptr(astid));
}

static void sema_trace_expr_list_liveness(Module *m, Expr **exprlist)
{
	FOREACH(Expr *, arg, exprlist) sema_trace_expr_liveness(m, arg);
}

static void sema_trace_stmt_chain_liveness(Module *m, AstId astid)
{
	AstId current = astid;
	while (current)
	{
		Ast *stmt = ast_next(&current);
		sema_trace_stmt_liveness(m, stmt);
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
static void sema_trace_expr_asm_arg(Module *m, ExprAsmArg *arg)
{
	switch (arg->kind)
	{
		case ASM_ARG_ADDR:
			sema_trace_exprid_liveness(m, arg->base);
			sema_trace_exprid_liveness(m, arg->idx);
			return;
		case ASM_ARG_REG:
		case ASM_ARG_INT:
			return;
		case ASM_ARG_MEMADDR:
		case ASM_ARG_MEMVAR:
		case ASM_ARG_REGVAR:
			sema_trace_decl_liveness(m, arg->ident.ident_decl);
			return;
		case ASM_ARG_VALUE:
			sema_trace_exprid_liveness(m, arg->expr_id);
			return;
	}
	UNREACHABLE_VOID
}
static void sema_trace_asm_arg_list(Module *m, ExprAsmArg **list)
{
	FOREACH(ExprAsmArg *, asm_arg, list)
	{
		sema_trace_expr_asm_arg(m, asm_arg);
	}
}

static void sema_trace_stmt_liveness(Module *m, Ast *ast)
{
	if (!ast) return;
	switch (ast->ast_kind)
	{
		case AST_POISONED:
		case CT_AST:
		case AST_FOREACH_STMT:
			assert_print_line(ast->loc);
			error_exit("Unexpected liveness checking of AST node %d.", ast->ast_kind);
		case AST_ASM_STMT:
			sema_trace_expr_list_liveness(m, ast->asm_stmt.args);
			return;
		case AST_DEFER_STMT:
			sema_trace_stmt_liveness(m, astptr(ast->defer_stmt.body));
			return;
		case AST_NOP_STMT:
		case AST_ASM_LABEL:
			return;
		case AST_CT_COMPOUND_STMT:
			sema_trace_stmt_chain_liveness(m, ast->ct_compound_stmt);
			return;
		case AST_COMPOUND_STMT:
			sema_trace_stmt_chain_liveness(m, ast->compound_stmt.first_stmt);
			return;
		case AST_EXPR_STMT:
			sema_trace_expr_liveness(m, ast->expr_stmt);
			return;
		case AST_DECLARE_STMT:
			sema_trace_decl_liveness(m, ast->declare_stmt);
			return;
		case AST_RETURN_STMT:
		case AST_BLOCK_EXIT_STMT:
			sema_trace_expr_liveness(m, ast->return_stmt.expr);
			sema_trace_stmt_chain_liveness(m, ast->return_stmt.cleanup);
			if (ast->return_stmt.cleanup != ast->return_stmt.cleanup_fail)
			{
				sema_trace_astid_liveness(m, ast->return_stmt.cleanup_fail);
			}
			return;
		case AST_ASM_BLOCK_STMT:
			if (ast->asm_block_stmt.is_string)
			{
				sema_trace_exprid_liveness(m, ast->asm_block_stmt.asm_string);
				return;
			}
			sema_trace_stmt_chain_liveness(m, ast->asm_block_stmt.block->asm_stmt);
			sema_trace_asm_arg_list(m, ast->asm_block_stmt.block->input);
			sema_trace_asm_arg_list(m, ast->asm_block_stmt.block->output_vars);
			return;
		case AST_ASSERT_STMT:
		{
			Expr *e = exprptr(ast->assert_stmt.expr);
			if (compile_asserts() || expr_is_pure(e))
			{
				sema_trace_expr_liveness(m, e);
			}
			if (compile_asserts())
			{
				sema_trace_exprid_liveness(m, ast->assert_stmt.message);
				sema_trace_expr_list_liveness(m, ast->assert_stmt.args);
			}
			return;
		}
		case AST_DECLS_STMT:
		{
			FOREACH(Decl *, decl, ast->decls_stmt) sema_trace_decl_liveness(m, decl);
			return;
		}
		case AST_FOR_STMT:
			sema_trace_exprid_liveness(m, ast->for_stmt.cond);
			sema_trace_exprid_liveness(m, ast->for_stmt.init);
			sema_trace_exprid_liveness(m, ast->for_stmt.incr);
			sema_trace_astid_liveness(m, ast->for_stmt.body);
			return;
		case AST_IF_STMT:
			sema_trace_exprid_liveness(m, ast->if_stmt.cond);
			sema_trace_astid_liveness(m, ast->if_stmt.then_body);
			sema_trace_astid_liveness(m, ast->if_stmt.else_body);
			return;
		case AST_SWITCH_STMT:
			sema_trace_exprid_liveness(m, ast->switch_stmt.cond);
			{
				FOREACH(Ast *, casestm, ast->switch_stmt.cases) sema_trace_stmt_liveness(m, casestm);
			}
			return;
		case AST_CASE_STMT:
			sema_trace_exprid_liveness(m, ast->case_stmt.expr);
			sema_trace_exprid_liveness(m, ast->case_stmt.to_expr);
			sema_trace_stmt_liveness(m, ast->case_stmt.body);
			return;
		case AST_DEFAULT_STMT:
			sema_trace_stmt_liveness(m, ast->case_stmt.body);
			return;
		case AST_NEXTCASE_STMT:
			sema_trace_stmt_chain_liveness(m, ast->nextcase_stmt.defer_id);
			if (ast->nextcase_stmt.is_expr) sema_trace_expr_liveness(m, ast->nextcase_stmt.nextcase_value);
			return;
		case AST_BREAK_STMT:
		case AST_CONTINUE_STMT:
			sema_trace_stmt_chain_liveness(m, ast->contbreak_stmt.defers);
			return;
	}
	UNREACHABLE_VOID
}

static void sema_trace_const_initializer_liveness(Module *m, ConstInitializer *const_init)
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
			ArrayIndex size = array_type->array.len;
			for (ArrayIndex i = 0; i < (ArrayIndex)size; i++)
			{
				sema_trace_const_initializer_liveness(m, elements[i]);
			}
			return;
		}
		case CONST_INIT_ARRAY:
		{
			FOREACH(ConstInitializer *, i, const_init->init_array.elements) sema_trace_const_initializer_liveness(m, i);
			return;
		}
		case CONST_INIT_UNION:
			const_init = const_init->init_union.element;
			goto RETRY;
		case CONST_INIT_STRUCT:
		{
			Decl *decl = const_init->type->decl;
			Decl **members = decl->strukt.members;
			int count = vec_size(members);
			if (decl->decl_kind == DECL_UNION && count) count = 1;
			for (int i = 0; i < count; i++)
			{
				sema_trace_const_initializer_liveness(m, const_init->init_struct[i]);
			}
			return;
		}
		case CONST_INIT_VALUE:
			sema_trace_expr_liveness(m, const_init->init_value);
			return;
	}
	UNREACHABLE_VOID
}


static void sema_trace_expr_liveness(Module *m, Expr *expr)
{
RETRY:
	if (!expr) return;
	sema_trace_type_liveness(m, expr->type);
	switch (expr->expr_kind)
	{
		case NON_RUNTIME_EXPR:
		case EXPR_SUBSCRIPT_ASSIGN:
		case EXPR_OPERATOR_CHARS:
		case EXPR_MEMBER_GET:
		case EXPR_MEMBER_SET:
		case EXPR_NAMED_ARGUMENT:
		case EXPR_NAMED_EVAL_ARGUMENT:
		case UNRESOLVED_EXPRS:
		case EXPR_LAMBDA:
			assert_print_line(expr->loc);
			error_exit("Unexpected liveness checking of expr node %d.", expr->expr_kind);
		case EXPR_TWO:
			sema_trace_expr_liveness(m, expr->two_expr.first);
			sema_trace_expr_liveness(m, expr->two_expr.last);
			return;
		case EXPR_DESIGNATOR:
			sema_trace_expr_liveness(m, expr->designator_expr.value);
			return;
		case EXPR_BUILTIN:
			return;
		case EXPR_MAKE_SLICE:
			expr = expr->make_slice_expr.ptr;
			goto RETRY;
		case EXPR_MAKE_ANY:
			sema_trace_expr_liveness(m, expr->make_any_expr.typeid);
			expr = expr->make_any_expr.inner;
			goto RETRY;
		case EXPR_ACCESS_RESOLVED:
		case EXPR_BITACCESS:
			sema_trace_decl_liveness(m, expr->access_resolved_expr.ref);
			expr = expr->access_resolved_expr.parent;
			goto RETRY;
		case EXPR_ASM:
			sema_trace_expr_asm_arg(m, &expr->expr_asm_arg);
			return;
		case EXPR_BINARY:
		case EXPR_BITASSIGN:
			sema_trace_expr_liveness(m, exprptr(expr->binary_expr.left));
			sema_trace_expr_liveness(m, exprptr(expr->binary_expr.right));
			return;
		case EXPR_CALL:
		{
			sema_trace_expr_list_liveness(m, expr->call_expr.arguments);
			if (expr->call_expr.varargs)
			{
				if (expr->call_expr.va_is_splat)
				{
					sema_trace_expr_liveness(m, expr->call_expr.vasplat);
				}
				else
				{
					sema_trace_expr_list_liveness(m, expr->call_expr.varargs);
				}
			}
			if (expr->call_expr.is_builtin) return;
			if (!expr->call_expr.is_func_ref)
			{
				sema_trace_expr_liveness(m, exprptr(expr->call_expr.function));
				return;
			}
			sema_trace_stmt_chain_liveness(m, expr->call_expr.function_contracts);
			sema_trace_decl_liveness(m, declptr(expr->call_expr.func_ref));
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
				sema_trace_decl_liveness(m, expr->catch_expr.decl);
			}
			FOREACH(Expr *, e, expr->catch_expr.exprs) sema_trace_expr_liveness(m, e);
			return;
		}
		case EXPR_CONST:
			switch (expr->const_expr.const_kind)
			{
				case CONST_TYPEID:
					sema_trace_type_liveness(m, expr->const_expr.typeid);
					return;
				case CONST_FLOAT:
				case CONST_INTEGER:
				case CONST_BOOL:
				case CONST_BYTES:
				case CONST_STRING:
				case CONST_POINTER:
				case CONST_UNTYPED_LIST:
				case CONST_MEMBER:
				case CONST_REFLECTION:
					return;
				case CONST_FAULT:
					sema_trace_decl_liveness(m, expr->const_expr.fault);
					return;
				case CONST_ENUM:
					sema_trace_decl_liveness(m, expr->const_expr.enum_val);
					return;
				case CONST_REF:
					sema_trace_decl_liveness(m, expr->const_expr.global_ref);
					return;
				case CONST_SLICE:
					sema_trace_const_initializer_liveness(m, expr->const_expr.slice_init);
					return;
				case CONST_INITIALIZER:
					sema_trace_const_initializer_liveness(m, expr->const_expr.initializer);
					return;
			}
		case EXPR_COND:
		{
			FOREACH(Expr *, e, expr->cond_expr) sema_trace_expr_liveness(m, e);
			return;
		}
		case EXPR_DECL:
			sema_trace_decl_liveness(m, expr->decl_expr);
			return;
		case EXPR_EXPRESSION_LIST:
			sema_trace_expr_list_liveness(m, expr->expression_list);
			return;
		case EXPR_DESIGNATED_INITIALIZER_LIST:
			sema_trace_expr_liveness(m, expr->designated_init.splat);
			sema_trace_expr_list_liveness(m, expr->designated_init.list);
			return;
		case EXPR_IDENTIFIER:
			sema_trace_decl_liveness(m, expr->ident_expr);
			return;
		case EXPR_INITIALIZER_LIST:
		{
			FOREACH(Expr *, e, expr->initializer_list) sema_trace_expr_liveness(m, e);
			return;
		}
		case EXPR_MACRO_BLOCK:
		{
			FOREACH(Decl *, val, expr->macro_block.params) sema_trace_decl_liveness(m, val);
			sema_trace_stmt_chain_liveness(m, expr->macro_block.first_stmt);
			return;
		}
		case EXPR_MACRO_BODY_EXPANSION:
		{
			FOREACH(Decl *, arg, expr->body_expansion_expr.declarations) sema_trace_decl_liveness(m, arg);
			FOREACH(Expr *, arg, expr->body_expansion_expr.values) sema_trace_expr_liveness(m, arg);
			sema_trace_stmt_liveness(m, astptrzero(expr->body_expansion_expr.first_stmt));
			return;
		}
		case EXPR_NOP:
			return;
		case EXPR_POINTER_OFFSET:
			sema_trace_expr_liveness(m, exprptr(expr->pointer_offset_expr.ptr));
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
			sema_trace_expr_liveness(m, exprptr(expr->slice_assign_expr.left));
			sema_trace_expr_liveness(m, exprptr(expr->slice_assign_expr.right));
			return;
		case EXPR_SLICE:
			sema_trace_expr_liveness(m, exprptr(expr->slice_expr.expr));
			switch (expr->slice_expr.range.range_type)
			{
				case RANGE_SINGLE_ELEMENT:
					UNREACHABLE_VOID
				case RANGE_CONST_RANGE:
					return;
				case RANGE_DYNAMIC:
					sema_trace_expr_liveness(m, exprptr(expr->slice_expr.range.start));
					sema_trace_expr_liveness(m, exprptrzero(expr->slice_expr.range.end));
					return;
				case RANGE_CONST_END:
				case RANGE_CONST_LEN:
					sema_trace_expr_liveness(m, exprptr(expr->slice_expr.range.start));
					return;
			}
			UNREACHABLE_VOID
		case EXPR_SUBSCRIPT:
		case EXPR_SUBSCRIPT_ADDR:
			sema_trace_expr_liveness(m, exprptr(expr->subscript_expr.expr));
			sema_trace_expr_liveness(m, exprptr(expr->subscript_expr.index.expr));
			return;
		case EXPR_SWIZZLE:
			sema_trace_expr_liveness(m, exprptr(expr->swizzle_expr.parent));
			return;
		case EXPR_TERNARY:
			sema_trace_expr_liveness(m, exprptr(expr->ternary_expr.cond));
			if (expr->ternary_expr.then_expr)
			{
				sema_trace_expr_liveness(m, exprptr(expr->ternary_expr.then_expr));
			}
			expr = exprptr(expr->ternary_expr.else_expr);
			goto RETRY;
		case EXPR_BENCHMARK_HOOK:
		case EXPR_TEST_HOOK:
			return;
		case EXPR_TYPEID_INFO:
			sema_trace_exprid_liveness(m, expr->typeid_info_expr.parent);
			return;
		case EXPR_TRY:
			sema_trace_expr_liveness(m, expr->try_expr.optional);
			if (expr->try_expr.assign_existing)
			{
				sema_trace_expr_liveness(m, expr->try_expr.lhs);
			}
			else
			{
				sema_trace_decl_liveness(m, expr->try_expr.decl);
			}
			return;
		case EXPR_TRY_UNWRAP_CHAIN:
			sema_trace_expr_list_liveness(m, expr->try_unwrap_chain_expr);
			return;
		case EXPR_INT_TO_BOOL:
			sema_trace_expr_liveness(m, expr->int_to_bool_expr.inner);
			return;
		case EXPR_EXT_TRUNC:
			sema_trace_expr_liveness(m, expr->ext_trunc_expr.inner);
			return;
		case EXPR_LAST_FAULT:
			return;
	}
	UNREACHABLE_VOID
}

void sema_trace_liveness(void)
{
	if (compiler.context.main)
	{
		sema_trace_decl_liveness(NULL, compiler.context.main);
	}
	sema_trace_decl_liveness(NULL, compiler.context.panic_var);
	bool keep_all = !strip_unused();
	bool keep_tests = compiler.build.build_test;
	bool keep_benchmarks = compiler.build.build_benchmark;
	FOREACH(Decl *, function, compiler.context.method_extension_list)
	{
		if (function->decl_kind == DECL_MACRO) continue;
		if (function->func_decl.attr_dynamic) function->no_strip = true;
		if (function->is_export || function->no_strip) sema_trace_decl_liveness(function->unit->module, function);
	}
	FOREACH(Module *, m, compiler.context.module_list)
	{
		FOREACH(CompilationUnit *, unit, m->units)
		{
			FOREACH(Decl *, function, unit->functions)
			{
				if (function->func_decl.attr_test && !keep_tests) continue;
				if (function->func_decl.attr_benchmark && !keep_benchmarks) continue;
				if (keep_all ||
					function->is_export ||
					function->no_strip ||
					function->func_decl.attr_finalizer ||
					function->func_decl.attr_init ||
					(function->func_decl.attr_test) ||
					(function->func_decl.attr_benchmark))
				{
					sema_trace_decl_liveness(m, function);
				}
			}
			FOREACH(Decl *, method, unit->methods)
			{
				if (keep_all || method->is_export || method->no_strip) sema_trace_decl_liveness(m, method);
			}
			FOREACH(Decl *, var, unit->vars)
			{
				if (keep_all || var->is_export || var->no_strip) sema_trace_decl_liveness(m, var);
			}
			if (!keep_all) continue;
			FOREACH(Decl *, lambda, unit->lambdas) sema_trace_decl_liveness(m, lambda);
			FOREACH(Decl *, type, unit->types) sema_trace_decl_liveness(m, type);
			FOREACH(Decl *, enum_decl, unit->enums) sema_trace_decl_liveness(m, enum_decl);
		}
	}
}

INLINE void sema_trace_enum_associated(Module *m, Decl *decl)
{
	sema_trace_type_liveness(m, decl->enums.type_info->type);
	FOREACH(Decl *, enum_constant, decl->enums.values)
	{
		sema_trace_decl_liveness(m, enum_constant);
	}
}
INLINE void sema_trace_decl_dynamic_methods(Module *m, Decl *decl)
{
	Methods *table = decl->method_table;
	if (!table) return;
	Decl **methods = table->methods;
	int method_count = vec_size(methods);
	if (!method_count) return;
	for (int i = 0; i < method_count; i++)
	{
		Decl *method = methods[i];
		if (method->decl_kind == DECL_MACRO) continue;
		if (method->func_decl.attr_dynamic)
		{
			sema_trace_decl_liveness(m, method);
		}
	}
}
static void sema_trace_func_liveness(Module *m, Signature *sig)
{
	FOREACH(Decl *, param, sig->params) sema_trace_decl_liveness(m, param);
	sema_trace_type_liveness(m, typeget(sig->rtype));
}

static void sema_trace_decl_internal(Module *m, Decl *decl)
{
	switch (decl->decl_kind)
	{
		case DECL_ERASED:
			return;
		case DECL_TYPE_ALIAS:
			sema_trace_type_liveness(m, decl->type);
			return;
		case DECL_ENUM:
			sema_trace_decl_dynamic_methods(m, decl);
			sema_trace_enum_associated(m, decl);
			return;
		case DECL_TYPEDEF:
			sema_trace_type_liveness(m, decl->distinct->type);
			FALLTHROUGH;
		case DECL_CONSTSET:
		case DECL_BITSTRUCT:
		case DECL_INTERFACE:
		case DECL_UNION:
			sema_trace_decl_dynamic_methods(m, decl);
			return;
		case DECL_STRUCT:
			if (decl->is_substruct)
			{
				sema_trace_type_liveness(m, decl->strukt.members[0]->type);
			}
			sema_trace_decl_dynamic_methods(m, decl);
			return;
		case DECL_ENUM_CONSTANT:
			if (!decl->enum_constant.is_raw)
			{
				sema_trace_expr_list_liveness(m, decl->enum_constant.associated);
			}
			return;
		case DECL_POISONED:
		case DECL_ATTRIBUTE:
		case DECL_FAULT:
			return;
		case DECL_ALIAS:
		case DECL_ALIAS_PATH:
		case DECL_BODYPARAM:
		case DECL_CT_ASSERT:
		case DECL_CT_ECHO:
		case DECL_CT_EXEC:
		case DECL_CT_EXPAND:
		case DECL_CT_INCLUDE:
		case DECL_GROUP:
		case DECL_IMPORT:
		case DECL_LABEL:
		case DECL_MACRO:
		case DECL_GENERIC:
		case DECL_GENERIC_INSTANCE:
		case DECL_CONTRACT:
			UNREACHABLE_VOID;
		case DECL_DECLARRAY:
		{
			FOREACH(Decl *, d, decl->decls)
			{
				sema_trace_decl_liveness(m, d);
			}
			return;
		}
		case DECL_FNTYPE:
			sema_trace_func_liveness(m, &decl->fntype_decl.signature);
			return;
		case DECL_FUNC:
			sema_trace_func_liveness(m, &decl->func_decl.signature);
			sema_trace_stmt_liveness(m, astptrzero(decl->func_decl.body));
			return;
		case DECL_VAR:
			switch (decl->var.kind)
			{
				case VARDECL_PARAM_EXPR:
					// These are never traced, they are folded in use.
				case VARDECL_PARAM_CT_TYPE:
				case VARDECL_LOCAL_CT_TYPE:
				case VARDECL_REWRAPPED:
				case VARDECL_UNWRAPPED:
					break;
				case VARDECL_PARAM_CT:
				case VARDECL_PARAM:
					sema_trace_type_liveness(m, decl->type);
					if (decl->var.init_expr && decl->var.init_expr->resolve_status == RESOLVE_DONE)
					{
						sema_trace_expr_liveness(m, decl->var.init_expr);
					}
					break;
				default:
					sema_trace_type_liveness(m, decl->type);
					sema_trace_expr_liveness(m, decl->var.init_expr);
					break;
			}
	}
}

INLINE bool decl_is_emitted_in_own_module(Decl *decl)
{
	if (decl->decl_kind != DECL_VAR) return true;
	return !decl->var.is_static && (decl->var.kind == VARDECL_GLOBAL || decl->var.kind == VARDECL_CONST);
}

static void sema_trace_decl_used_externally(Module *m, Decl *decl)
{
	switch (decl->decl_kind)
	{
		case DECL_ENUM:
		{
			FOREACH(Decl *, value, decl->enums.values)
			{
				if (!value->enum_constant.is_raw) sema_trace_expr_list_liveness(NULL, value->enum_constant.associated);
			}
			break;
		}
		case DECL_TYPEDEF:
			sema_trace_type_liveness(m, decl->distinct->type);
			break;
		case DECL_STRUCT:
			if (decl->is_substruct) sema_trace_type_liveness(m, decl->strukt.members[0]->type);
			break;
		default:
			break;
	}
}

static void sema_trace_decl_liveness(Module *m, Decl *decl)
{
	if (!decl) return;
	if (decl->decl_kind == DECL_ALIAS)
	{
		sema_trace_decl_liveness(m, decl->define_decl.alias);
		return;
	}
	Module *module = decl->unit && decl_is_emitted_in_own_module(decl) ? decl->unit->module : m;
	if (module != m && !decl->is_external_visible)
	{
		decl->is_external_visible = true;
		sema_trace_decl_used_externally(m, decl);
	}
	if (decl->is_live) return;
	decl->is_live = true;
	sema_trace_decl_internal(module, decl);
}
