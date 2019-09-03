// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "compiler_internal.h"


#define PRINTF(...) fprintf(context->codegen_output, __VA_ARGS__)
#define PRINTTYPE(_t) print_typename(context->codegen_output, _t)
#define INDENT() indent_line(context, indent)

static void codegen_ast(Context *context, Ast *ast, int indent);
static int codegen_emit_expr(Context *context, Expr *expr, int indent);

static void print_typename(FILE *file, Type *type)
{
	assert(type->resolve_status == RESOLVE_DONE);
	type = type->canonical;
	switch (type->type_kind)
	{
		case TYPE_POISONED:
		case TYPE_INC_ARRAY:
			UNREACHABLE
		case TYPE_USER_DEFINED:
			switch (type->decl->decl_kind)
			{
				case DECL_STRUCT:
					fprintf(file, "struct");
					break;
				case DECL_UNION:
					fprintf(file, "union");
					break;
				case DECL_ENUM:
					fprintf(file, "enum");
					break;
				case DECL_ERROR:
					fprintf(file, "int32_t");
					return;
				default:
					UNREACHABLE
			}
			fprintf(file, " _%s_%s", type->decl->module->name, type->decl->name.string);
			break;
		case TYPE_VOID:
			fprintf(file, "void");
			break;
		case TYPE_BOOL:
			fprintf(file, "bool");
			break;
		case TYPE_I8:
			fprintf(file, "int8_t");
			break;
		case TYPE_I16:
			fprintf(file, "int16_t");
			break;
		case TYPE_I32:
			fprintf(file, "int32_t");
			break;
		case TYPE_I64:
			fprintf(file, "int64_t");
			break;
		case TYPE_IXX:
			UNREACHABLE
		case TYPE_U8:
			fprintf(file, "uint8_t");
			break;
		case TYPE_U16:
			fprintf(file, "uint16_t");
			break;
		case TYPE_U32:
			fprintf(file, "int32_t");
			break;
		case TYPE_U64:
			fprintf(file, "uint64_t");
			break;
		case TYPE_UXX:
			UNREACHABLE
		case TYPE_F32:
			fprintf(file, "float");
			break;
		case TYPE_F64:
			fprintf(file, "double");
			break;
		case TYPE_FXX:
			UNREACHABLE
		case TYPE_STRING:
			UNREACHABLE;
		case TYPE_VARARRAY:
		case TYPE_POINTER:
			print_typename(file, type->base);
			fprintf(file, "*");
			break;
		case TYPE_ARRAY:
			print_typename(file, type->base);
			fprintf(file, "[%zu]", type->len);
			break;
		case TYPE_EXPRESSION:
			UNREACHABLE
	}
}

static void indent_line(Context *context, int indent)
{
	for (int i = 0; i < indent; i++)
	{
		PRINTF("   ");
	}
}

static inline void codegen_compound_stmt(Context *context, Ast *ast, int indent)
{
	indent_line(context, indent);
	PRINTF("{\n");
	VECEACH(ast->compound_stmt.stmts, i)
	{
		codegen_ast(context, ast->compound_stmt.stmts[i], indent + 1);
	}
	indent_line(context, indent);
	PRINTF("}\n");
}

static inline void codegen_emit_cast(Context *context, Type *canonical)
{
	assert(canonical == canonical->canonical);
	PRINTF("(");
	TODO
}

static inline int codegen_emit_call_expr(Context *context, Expr *expr, int indent)
{
	// TODO properly
	INDENT();
	PRINTTYPE(expr->type);
	PRINTF(" _%d = %s();\n", ++context->unique_index, expr->call_expr.function->identifier_expr.decl->name.string);
	return context->unique_index;
}

static inline int codegen_emit_identifier_expr(Context *context, Expr *expr, int indent)
{
	// TODO properly
	INDENT();
	PRINTTYPE(expr->type);
	Decl *decl = expr->identifier_expr.decl;
	PRINTF(" _%d = _%d_%s;\n", ++context->unique_index, decl->var.id, decl->name.string);
	return context->unique_index;
}


static inline int codegen_emit_const_expr(Context *context, Expr *expr, int indent)
{
	assert(expr->expr_kind == EXPR_CONST);
	Type *canonical = expr->type->canonical;
	INDENT();
	switch (canonical->type_kind)
	{
		case TYPE_POISONED:
		case TYPE_USER_DEFINED:
		case TYPE_VOID:
			UNREACHABLE
			break;
		case TYPE_BOOL:
			assert(expr->const_expr.type == CONST_BOOL);
			PRINTTYPE(expr->type);
			PRINTF(" _%d = %s;\n", ++context->unique_index, expr->const_expr.b ? "true" : "false");
			return context->unique_index;
		case TYPE_I8:
		case TYPE_I16:
		case TYPE_I32:
		case TYPE_I64:
		case TYPE_IXX:
			assert(expr->const_expr.type == CONST_INT);
			PRINTTYPE(expr->type);
			PRINTF(" _%d = (", ++context->unique_index);
			PRINTTYPE(canonical);
			PRINTF(")");
			PRINTF("%lld;\n", (long long)expr->const_expr.i);
			return context->unique_index;
		case TYPE_U8:
		case TYPE_U16:
		case TYPE_U32:
		case TYPE_U64:
		case TYPE_UXX:
			assert(expr->const_expr.type == CONST_INT);
			PRINTTYPE(expr->type);
			PRINTF(" _%d = (", ++context->unique_index);
			PRINTTYPE(canonical);
			PRINTF(")");
			PRINTF("%llu;\n", expr->const_expr.i);
			return context->unique_index;
		case TYPE_F32:
		case TYPE_F64:
		case TYPE_FXX:
			assert(expr->const_expr.type == CONST_FLOAT);
			PRINTTYPE(expr->type);
			PRINTF(" _%d = (", ++context->unique_index);
			PRINTTYPE(canonical);
			PRINTF(")");
			PRINTF("%Lf;\n", expr->const_expr.f);
			return context->unique_index;
		case TYPE_POINTER:
			assert(expr->const_expr.type == CONST_NIL);
			PRINTTYPE(expr->type);
			PRINTF(" _%d = ((", ++context->unique_index);
			PRINTTYPE(canonical);
			PRINTF("0);\n");
			return context->unique_index;
		case TYPE_STRING:
			TODO
		case TYPE_ARRAY:
		case TYPE_VARARRAY:
		case TYPE_INC_ARRAY:
		case TYPE_EXPRESSION:
			UNREACHABLE
	}
}

static inline int codegen_emit_cast_expr(Context *context, Expr *expr, int indent)
{
	int index = codegen_emit_expr(context, expr->expr_cast.expr, indent);
	INDENT();
	PRINTTYPE(expr->type);
	PRINTF(" _%d = (", ++context->unique_index);
	PRINTTYPE(expr->type);
	PRINTF(") _%d;\n", index);
	return context->unique_index;
}

static int codegen_emit_simple_binary_expr(Context *context, Expr *expr, int indent)
{
	int l_index = codegen_emit_expr(context, expr->binary_expr.left, indent);
	int r_index = codegen_emit_expr(context, expr->binary_expr.right, indent);
	INDENT();
	PRINTTYPE(expr->type);
	PRINTF(" _%d = _%d %s _%d;\n", ++context->unique_index, l_index, token_type_to_string(expr->binary_expr.operator), r_index);
	return context->unique_index;
}

static int codegen_emit_assign_expr(Context *context, Expr *expr, int indent)
{
	Expr *left = expr->binary_expr.left;
	Expr *right = expr->binary_expr.right;
	int index = codegen_emit_expr(context, right, indent);
	if (expr->binary_expr.left->expr_kind == EXPR_IDENTIFIER)
	{
		INDENT();
		PRINTF("%s = _%d;\n", left->identifier_expr.identifier.string, index);
		return index;
	}
	else
	{
		TODO
	}
}

static int codegen_emit_binary_expr(Context *context, Expr *expr, int indent)
{
	switch (expr->binary_expr.operator)
	{
		case TOKEN_EQ:
			return codegen_emit_assign_expr(context, expr, indent);
		case TOKEN_PLUS:
		case TOKEN_MINUS:
		case TOKEN_GREATER:
		case TOKEN_GREATER_EQ:
		case TOKEN_LESS_EQ:
		case TOKEN_LESS:
		case TOKEN_EQEQ:
		case TOKEN_NOT_EQUAL:
		case TOKEN_MOD:
		case TOKEN_DIV:
		case TOKEN_STAR:
			return codegen_emit_simple_binary_expr(context, expr, indent);
		case TOKEN_BIT_OR:
		case TOKEN_AMP:
		case TOKEN_BIT_XOR:
		case TOKEN_SHL:
		case TOKEN_SHR:
			TODO
		case TOKEN_MULT_ASSIGN:
		case TOKEN_DIV_ASSIGN:
		case TOKEN_MINUS_ASSIGN:
		case TOKEN_PLUS_ASSIGN:
		case TOKEN_MOD_ASSIGN:
		case TOKEN_AND_ASSIGN:
		case TOKEN_OR_ASSIGN:
		case TOKEN_SHR_ASSIGN:
		case TOKEN_SHL_ASSIGN:
		case TOKEN_BIT_XOR_ASSIGN:
		case TOKEN_BIT_OR_ASSIGN:
		case TOKEN_BIT_AND_ASSIGN:
		case TOKEN_ELVIS:
			UNREACHABLE
		default:
			UNREACHABLE
	}
}
static int codegen_emit_expr(Context *context, Expr *expr, int indent)
{
	switch (expr->expr_kind)
	{
		case EXPR_CONST:
			return codegen_emit_const_expr(context, expr, indent);
		case EXPR_CALL:
			return codegen_emit_call_expr(context, expr, indent);
		case EXPR_IDENTIFIER:
			return codegen_emit_identifier_expr(context, expr, indent);
		case EXPR_CAST:
			return codegen_emit_cast_expr(context, expr, indent);
		case EXPR_BINARY:
			return codegen_emit_binary_expr(context, expr, indent);
		default:
			TODO
	}
}

static inline int codegen_var_decl(Context *context, Decl *decl, int indent)
{
	assert(decl->decl_kind == DECL_VAR);
	if (!decl->var.init_expr) return -1;
	int index = codegen_emit_expr(context, decl->var.init_expr, indent);
	INDENT();
	PRINTF("_%d_%s = _%d;\n", decl->var.id, decl->name.string, index);
	return index;
}


static inline void codegen_declare_stmt(Context *context, Ast *ast, int indent)
{
	Decl *decl = ast->declare_stmt;
	if (decl->decl_kind == DECL_MULTI_DECL)
	{
		VECEACH(decl->multi_decl, i)
		{
			codegen_var_decl(context, decl->multi_decl[i], indent);
		}
	}
	else
	{
		codegen_var_decl(context, decl, indent);
	}
}

static inline void codegen_emit_for_smt(Context *context, Ast *ast, int indent)
{
	INDENT();
	int loop = ++context->unique_index;
	PRINTF("// --- Begin for id:%d ---\n", loop);
	Ast **stmts = ast->for_stmt.cond->cond_stmt.stmts;
	if (stmts)
	{
		INDENT();
		PRINTF("// --- Prelude ---\n");
		VECEACH(stmts, i)
		{
			codegen_ast(context, stmts[i], indent);
		}
	}
	INDENT();
	PRINTF("// --- Loop condition ---\n");
	INDENT();
	PRINTF("_FOR_%d:\n", loop);
	if (ast->for_stmt.cond->cond_stmt.expr)
	{
		int res = codegen_emit_expr(context, ast->for_stmt.cond->cond_stmt.expr, indent);
		INDENT();
		PRINTF("if (!_%i) goto _FOR_EXIT_%d;\n", res, loop);
	}
	INDENT();
	PRINTF("// --- Body ---\n");
	codegen_ast(context, ast->for_stmt.body, indent);
	INDENT();
	PRINTF("// --- End ---\n");
	INDENT();
	PRINTF("goto _FOR_%d;\n", loop);
	INDENT();
	PRINTF("_FOR_EXIT_%d:;\n", loop);
	INDENT();
	PRINTF("// --- End for id:%d --- \n", loop);
}

static inline void codegen_emit_do_smt(Context *context, Ast *ast, int indent)
{
	INDENT();
	int loop = ++context->unique_index;
	PRINTF("// --- Begin do id:%d ---\n", loop);
	INDENT();
	PRINTF("_DO_%d_BEGIN:\n", loop);
	INDENT();
	PRINTF("// --- Body ---\n");
	codegen_ast(context, ast->do_stmt.body, indent);
	INDENT();
	PRINTF("// --- End ---\n");
	INDENT();
	PRINTF("_DO_%d_CONTINUE:\n", loop);
	INDENT();
	PRINTF("// --- Loop condition ---\n");
	int res = codegen_emit_expr(context, ast->do_stmt.expr, indent);
	INDENT();
	PRINTF("if (_%i) goto _DO_%d_BEGIN;\n", res, loop);
	INDENT();
	PRINTF("_DO_%d_EXIT:;\n", loop);
	INDENT();
	PRINTF("// --- End do id:%d --- \n", loop);
}

static inline void codegen_emit_stmt_list(Context *context, Ast *ast, int indent)
{
	VECEACH(ast->stmt_list, i)
	{
		codegen_ast(context, ast->stmt_list[i], indent);
	}
}

static void codegen_ast(Context *context, Ast *ast, int indent)
{
	switch (ast->ast_kind)
	{
		case AST_POISONED:
			UNREACHABLE
		case AST_ASM_STMT:
			break;
		case AST_ATTRIBUTE:
			break;
		case AST_BREAK_STMT:
			break;
		case AST_CASE_STMT:
			break;
		case AST_CATCH_STMT:
			UNREACHABLE
		case AST_COMPOUND_STMT:
			codegen_compound_stmt(context, ast, indent);
			return;
		case AST_COND_STMT:
			break;
		case AST_CONTINUE_STMT:
			break;
		case AST_CT_IF_STMT:
			break;
		case AST_CT_ELIF_STMT:
			break;
		case AST_CT_ELSE_STMT:
			break;
		case AST_DECLARE_STMT:
			codegen_declare_stmt(context, ast, indent);
			return;
		case AST_DEFAULT_STMT:
			break;
		case AST_DEFER_STMT:
			break;
		case AST_DO_STMT:
			codegen_emit_do_smt(context, ast, indent);
			return;
		case AST_EXPR_STMT:
			codegen_emit_expr(context, ast->expr_stmt, indent);
			return;
		case AST_FOR_STMT:
			codegen_emit_for_smt(context, ast, indent);
			return;
		case AST_GOTO_STMT:
			break;
		case AST_IF_STMT:
			break;
		case AST_LABEL:
			break;
		case AST_NOP_STMT:
			break;
		case AST_RETURN_STMT:
			if (ast->return_stmt.expr)
			{
				int index = codegen_emit_expr(context, ast->return_stmt.expr, indent);
				INDENT();
				PRINTF("return _%d", index);
				PRINTF(";\n");
			}
			else
			{
				INDENT();
				PRINTF("return;\n");
			}
			return;
		case AST_SWITCH_STMT:
			break;
		case AST_THROW_STMT:
			break;
		case AST_TRY_STMT:
			break;
		case AST_NEXT_STMT:
			break;
		case AST_VOLATILE_STMT:
			break;
		case AST_WHILE_STMT:
			UNREACHABLE
		case AST_GENERIC_CASE_STMT:
			break;
		case AST_GENERIC_DEFAULT_STMT:
			break;
		case AST_STMT_LIST:
			codegen_emit_stmt_list(context, ast, indent);
			return;
	}
	TODO
}

static inline void codegen_func_decl(Context *context, Decl *decl)
{
	if (decl->visibility != VISIBLE_PUBLIC)
	{
		PRINTF("static ");
	}
	print_typename(context->codegen_output, decl->func.function_signature.rtype);
	PRINTF(" %s__%s()", decl->module->name, decl->name.string);
}

static inline void codegen_func(Context *context, Decl *decl)
{
	codegen_func_decl(context, decl);
	Ast *const body = decl->func.body;
	assert(body->ast_kind == AST_COMPOUND_STMT);
	PRINTF("\n{\n");
	Decl **const vars = decl->func.annotations->vars;
	Type *type = NULL;
	VECEACH(vars, i)
	{
		Decl *var = vars[i];
		assert(var->decl_kind == DECL_VAR);
		Type *current = var->var.type->canonical;
		if (type == current)
		{
			PRINTF(", ");
		}
		else
		{
			if (type) PRINTF(";\n");
			indent_line(context, 1);
			print_typename(context->codegen_output, var->var.type);
			PRINTF(" ");
		}
		type = current;
		PRINTF("_%u_%s", var->var.id, var->name.string);
	}
	if (type) PRINTF(";\n");
	VECEACH(body->compound_stmt.stmts, i)
	{
		codegen_ast(context, body->compound_stmt.stmts[i], 1);
	}
	PRINTF("}\n");
}

static void codegen_struct_member(Context *context, Decl *decl, int indent)
{
	indent_line(context, indent);
	switch (decl->decl_kind)
	{
		case DECL_VAR:
			PRINTTYPE(decl->var.type);
			PRINTF(" %s;\n", decl->name.string);
			return;
		case DECL_STRUCT:
		case DECL_UNION:
			break;
		default:
			UNREACHABLE
	}
	const char* type = decl->decl_kind == DECL_UNION ? "union" : "struct";
	PRINTF("%s {\n", type);
	VECEACH(decl->strukt.members, i)
	{
		codegen_struct_member(context, decl->strukt.members[i], indent + 1);
	}
	indent_line(context, indent);
	PRINTF("};\n");
}

static inline void codegen_struct_union(Context *context, Decl *decl)
{
	const char* type = decl->decl_kind == DECL_UNION ? "union" : "struct";
	PRINTF("typedef %s _%s_%s\n", type, decl->module->name, decl->name.string);
	PRINTF("{\n");
	VECEACH(decl->strukt.members, i)
	{
		codegen_struct_member(context, decl->strukt.members[i], 1);
	}
	PRINTF("} %s_%s;\n\n", decl->module->name, decl->name.string);
}


static inline void codegen_top_level_func(Context *context, Decl *decl)
{
	codegen_func_decl(context, decl);
	PRINTF(";\n");
}

static inline void codegen_top_level_struct_union(Context *context, Decl *decl)
{
	const char* type = decl->decl_kind == DECL_UNION ? "union" : "struct";
	PRINTF("typedef %s _%s_%s %s_%s;\n", type, decl->module->name, decl->name.string, decl->module->name, decl->name.string);
}

static inline void codegen_top_level_decl_header(Context *context, Decl *decl)
{
	switch (decl->decl_kind)
	{
		case DECL_POISONED:
			FATAL_ERROR("Tried to codegen broken code");
			return;
		case DECL_FUNC:
			codegen_top_level_func(context, decl);
			return;
		case DECL_VAR:
			break;
		case DECL_TYPEDEF:
			TODO
		case DECL_STRUCT:
		case DECL_UNION:
			codegen_top_level_struct_union(context, decl);
			return;
		case DECL_ENUM:
			TODO
			// codegen_top_level_enum(context, decl);
			return;
		case DECL_ERROR:
			TODO
		case DECL_MULTI_DECL:
			TODO
			break;
		case DECL_CT_IF:
		case DECL_CT_ELSE:
		case DECL_CT_ELIF:
		case DECL_ENUM_CONSTANT:
		case DECL_ARRAY_VALUE:
		case DECL_IMPORT:
		case DECL_ERROR_CONSTANT:
			UNREACHABLE
		case DECL_MACRO:
		case DECL_GENERIC:
			break;
	}
	TODO

}

static inline void codegen_top_level_decl(Context *context, Decl *decl)
{
	switch (decl->decl_kind)
	{
		case DECL_POISONED:
			FATAL_ERROR("Tried to codegen broken code");
			return;
		case DECL_FUNC:
			codegen_func(context, decl);
			return;
		case DECL_VAR:
			break;
		case DECL_ENUM_CONSTANT:
			break;
		case DECL_TYPEDEF:
			break;
		case DECL_STRUCT:
		case DECL_UNION:
			codegen_struct_union(context, decl);
			return;
		case DECL_ENUM:
			break;
		case DECL_ERROR:
			break;
		case DECL_ERROR_CONSTANT:
			break;
		case DECL_ARRAY_VALUE:
			break;
		case DECL_IMPORT:
			break;
			return;
		case DECL_MULTI_DECL:
			break;
			break;
		case DECL_CT_IF:
		case DECL_CT_ELSE:
		case DECL_CT_ELIF:
			UNREACHABLE
		case DECL_MACRO:
		case DECL_GENERIC:
			break;
	}
	TODO
}
void codegen(Context *context)
{
	VECEACH(context->declarations, i)
	{
		codegen_top_level_decl_header(context, context->declarations[i]);
	}
	VECEACH(context->declarations, i)
	{
		codegen_top_level_decl(context, context->declarations[i]);
	}

}