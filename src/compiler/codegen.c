// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "compiler_internal.h"


#define PRINTF(...) fprintf(context->codegen_output, __VA_ARGS__)
#define PRINTTYPE(_t) print_typename(context->codegen_output, _t)
static void codegen_ast(Context *context, Ast *ast, int indent);

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
		PRINTF("  ");
	}
}

static void codegen_expr(Context *context, Expr *expr)
{
	switch (expr->expr_kind)
	{
		case EXPR_POISONED:
			UNREACHABLE;
		case EXPR_TRY:
			break;
		case EXPR_CONST:
			break;
		case EXPR_BINARY:
			break;
		case EXPR_CONDITIONAL:
			break;
		case EXPR_UNARY:
			break;
		case EXPR_POST_UNARY:
			break;
		case EXPR_TYPE:
			break;
		case EXPR_IDENTIFIER:
			break;
		case EXPR_METHOD_REF:
			break;
		case EXPR_CALL:
			break;
		case EXPR_SIZEOF:
			break;
		case EXPR_SUBSCRIPT:
			break;
		case EXPR_ACCESS:
			break;
		case EXPR_STRUCT_VALUE:
			break;
		case EXPR_STRUCT_INIT_VALUES:
			break;
		case EXPR_INITIALIZER_LIST:
			break;
		case EXPR_EXPRESSION_LIST:
			break;
		case EXPR_DEFERRED_TOKENS:
			break;
		case EXPR_CAST:
			break;
	}
	TODO
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
static inline void codegen_emit_const_expr(Context *context, Expr *expr)
{
	assert(expr->expr_kind == EXPR_CONST);
	Type *canonical = expr->type->canonical;
	switch (canonical->type_kind)
	{
		case TYPE_POISONED:
		case TYPE_USER_DEFINED:
		case TYPE_VOID:
			UNREACHABLE
			break;
		case TYPE_BOOL:
			assert(expr->const_expr.type == CONST_BOOL);
			PRINTF(expr->const_expr.b ? "true" : "false");
			break;
		case TYPE_I8:
		case TYPE_I16:
		case TYPE_I32:
		case TYPE_I64:
			assert(expr->const_expr.type == CONST_INT);
			PRINTF("((");
			print_typename(context->codegen_output, canonical);
			PRINTF(")");
			PRINTF("%lld)", (long long)expr->const_expr.i);
			break;
		case TYPE_IXX:
			UNREACHABLE
		case TYPE_U8:
		case TYPE_U16:
		case TYPE_U32:
		case TYPE_U64:
			assert(expr->const_expr.type == CONST_INT);
			PRINTF("((");
			print_typename(context->codegen_output, canonical);
			PRINTF(")");
			PRINTF("%llu)", expr->const_expr.i);
			break;
		case TYPE_UXX:
			UNREACHABLE
		case TYPE_F32:
		case TYPE_F64:
			assert(expr->const_expr.type == CONST_FLOAT);
			PRINTF("((");
			print_typename(context->codegen_output, canonical);
			PRINTF(")");
			PRINTF("%Lf)", expr->const_expr.f);
			break;
		case TYPE_FXX:
			UNREACHABLE
		case TYPE_POINTER:
			assert(expr->const_expr.type == CONST_NIL);
			PRINTF("((");
			print_typename(context->codegen_output, canonical);
			PRINTF("0)");
			break;
		case TYPE_STRING:
			TODO
		case TYPE_ARRAY:
		case TYPE_VARARRAY:
		case TYPE_INC_ARRAY:
		case TYPE_EXPRESSION:
			UNREACHABLE
	}
}

static inline void codegen_emit_expr(Context *context, Expr *expr)
{
	switch (expr->expr_kind)
	{
		case EXPR_CONST:
			codegen_emit_const_expr(context, expr);
			break;
		default:
			TODO
	}
}

static inline void codegen_var_decl(Context *context, Decl *decl, int indent)
{
	assert(decl->decl_kind == DECL_VAR);
	indent_line(context, indent);
	print_typename(context->codegen_output, decl->var.type);
	PRINTF(" ");
	PRINTF("%s", decl->name.string);
	PRINTF(";\n");
	if (!decl->var.init_expr) return;
	indent_line(context, indent);
	PRINTF("%s = ", decl->name.string);
	codegen_emit_expr(context, decl->var.init_expr);
	PRINTF(";\n");
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
		case AST_DECL_EXPR_LIST:
			break;
		case AST_DEFAULT_STMT:
			break;
		case AST_DEFER_STMT:
			break;
		case AST_DO_STMT:
			break;
		case AST_EXPR_STMT:
			break;
		case AST_FOR_STMT:
			break;
		case AST_GOTO_STMT:
			break;
		case AST_IF_STMT:
			break;
		case AST_LABEL:
			break;
		case AST_NOP_STMT:
			break;
		case AST_RETURN_STMT:
			indent_line(context, indent);
			if (ast->return_stmt.expr)
			{
				PRINTF("return ");
				codegen_expr(context, ast->return_stmt.expr);
				PRINTF(";\n");
			}
			else
			{
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
			break;
		case AST_GENERIC_CASE_STMT:
			break;
		case AST_GENERIC_DEFAULT_STMT:
			break;
	}
	TODO
}
static inline void codegen_func(Context *context, Decl *decl)
{
	print_typename(context->codegen_output, decl->func.function_signature.rtype);
	PRINTF(" %s__%s()\n", decl->module->name, decl->name.string);
	codegen_ast(context, decl->func.body, 0);
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

static inline void codegen_decl(Context *context, Decl *decl)
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
		case DECL_MACRO:
			break;
		case DECL_MULTI_DECL:
			break;
		case DECL_GENERIC:
			break;
		case DECL_CT_IF:
			break;
		case DECL_CT_ELSE:
			break;
		case DECL_CT_ELIF:
			break;
	}
	TODO
}
void codegen(Context *context)
{
	VECEACH(context->declarations, i)
	{
		codegen_decl(context, context->declarations[i]);
	}
}