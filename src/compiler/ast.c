// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include <utils/malloc.h>
#include <build/build_options.h>
#include <utils/lib.h>
#include "ast.h"

Decl *decl_new_in_module(Module *module, DeclKind decl_kind, Token name, Visibility visibility)
{
	assert(name.string);
	Decl *decl = malloc_arena(sizeof(Decl));
	memset(decl, 0, sizeof(Decl));
	decl->decl_kind = decl_kind;
	decl->name = name;
	decl->visibility = visibility;
	decl->module = module;
	return decl;
}

Type poisoned_type = { .type_kind = TYPE_POISONED };

char *type_to_string(Type *type)
{
	switch (type->type_kind)
	{
		case TYPE_VOID:
			return "void";
		case TYPE_UNRESOLVED:
			TODO
		case TYPE_STRING:
			return "string";
		case TYPE_UNRESOLVED_EXPR:
			TODO
		case TYPE_NIL:
			return "nil";
		case TYPE_BUILTIN:
			return "TODO";
		default:
			TODO
	}
}

Type *type_poisoned()
{
	static Type poison = { .type_kind = TYPE_POISONED };
	return &poison;
}
Type *type_new(TypeKind type_kind)
{
	Type *type = malloc_arena(sizeof(Type));
	memset(type, 0, sizeof(Type));
	type->type_kind = type_kind;
	return type;
}

Decl *decl_new_self_type(struct _Module *module, Token name, DeclKind decl_type, TypeKind type_kind, Visibility visibility)
{
	Decl *decl = decl_new_in_module(module, decl_type, name, visibility);
	Type *type = type_new(type_kind);
	type->canonical_type = type;
	type->decl = decl;
	decl->type = type;
	return decl;
}

Decl poisoned_decl = { .decl_kind = DECL_POISONED, .resolve_status = RESOLVE_DONE };

Decl *decl_new_var(struct _Module *module, Token name, Type *type, VarDeclKind kind, Visibility visibility)
{
	Decl *decl = decl_new_in_module(module, DECL_VAR, name, visibility);
	decl->var.kind = kind;
	decl->var.type = type;
	return decl;
}

Decl *decl_new_enum_const(Decl *parent, Token name, DeclKind kind)
{
	Decl *decl = decl_new_in_module(parent->module, DECL_VAR, name, parent->visibility);
	decl->decl_kind = kind;
	assert(parent->type);
	decl->type = parent->type;
	return decl;
}

Decl *struct_find_name(Decl *decl, const char* name)
{
    Decl** compare_members = decl->strukt.members;
    VECEACH(compare_members, i)
    {
        Decl *member = compare_members[i];
        if (member->name.type == TOKEN_INVALID_TOKEN)
        {
            Decl *found = struct_find_name(member, name);
            if (found) return found;
        }
        else if (member->name.string == name) return member;
    }
    return NULL;
}


Expr *expr_new(ExprKind kind, Token start)
{
	Expr *expr = malloc_arena(sizeof(Expr));
	expr->expr_kind = kind;
	expr->loc = start;
	expr->type = NULL;
	return expr;
}

Expr poisoned_expr = { .expr_kind = EXPR_POISONED };

Type type_bool;
Type type_void, type_nil, type_string;

Type type_half, type_float, type_double, type_quad;
Type type_char, type_short, type_int, type_long, type_isize;
Type type_byte, type_ushort, type_uint, type_ulong, type_usize;
Type type_compint, type_compfloat;

void type_setup(int pointer_size)
{
	type_void = (Type) { .type_kind = TYPE_VOID, .canonical_type = &type_void };
	type_nil = (Type) { .type_kind = TYPE_NIL, .canonical_type = &type_nil };
	type_string = (Type) { .type_kind = TYPE_STRING, .canonical_type = &type_string };

#define DEF_TYPE(name, bits, _bytes, type) name = (Type) { .type_kind = TYPE_BUILTIN, .bitsize = bits, .bytes = _bytes, .num_type = type, .canonical_type = &name }

	DEF_TYPE(type_compint, 0, 0, NUMBER_TYPE_SIGNED_INT);
	DEF_TYPE(type_compfloat, 0, 0, NUMBER_TYPE_FLOAT);

	DEF_TYPE(type_bool, 1, 8, NUMBER_TYPE_BOOL);

	DEF_TYPE(type_half, 2, 16, NUMBER_TYPE_FLOAT);
	DEF_TYPE(type_float, 4, 32, NUMBER_TYPE_FLOAT);
	DEF_TYPE(type_double, 8, 64, NUMBER_TYPE_FLOAT);
	DEF_TYPE(type_quad, 16, 128, NUMBER_TYPE_FLOAT);

	DEF_TYPE(type_char, 1, 8, NUMBER_TYPE_SIGNED_INT);
	DEF_TYPE(type_short, 2, 16, NUMBER_TYPE_SIGNED_INT);
	DEF_TYPE(type_int, 4, 32, NUMBER_TYPE_SIGNED_INT);
	DEF_TYPE(type_long, 8, 64, NUMBER_TYPE_SIGNED_INT);
	DEF_TYPE(type_isize, pointer_size / 8, pointer_size, NUMBER_TYPE_SIGNED_INT);

	DEF_TYPE(type_byte, 1, 8, NUMBER_TYPE_UNSIGNED_INT);
	DEF_TYPE(type_ushort, 2, 16, NUMBER_TYPE_UNSIGNED_INT);
	DEF_TYPE(type_uint, 4, 32, NUMBER_TYPE_UNSIGNED_INT);
	DEF_TYPE(type_ulong, 8, 64, NUMBER_TYPE_UNSIGNED_INT);
	DEF_TYPE(type_usize, pointer_size / 8, pointer_size, NUMBER_TYPE_UNSIGNED_INT);

#undef DEF_TYPE

}

Type* type_int_max_type(bool is_signed)
{
	return is_signed ? &type_long : &type_ulong;
}

Type *type_get_signed(Type *type)
{
	assert(type->type_kind == TYPE_BUILTIN);
	if (type->num_type == NUMBER_TYPE_SIGNED_INT) return type;
	assert(type->num_type == NUMBER_TYPE_UNSIGNED_INT);
	switch (type->bytes)
	{
		case 8:
			return &type_long;
		case 4:
			return &type_int;
		case 2:
			return &type_short;
		case 1:
			return &type_char;
		default:
			UNREACHABLE
	}
}

Type* type_get_unsigned(Type *type)
{
	assert(type->type_kind == TYPE_BUILTIN);
	if (type->num_type == NUMBER_TYPE_UNSIGNED_INT) return type;
	assert(type->num_type == NUMBER_TYPE_SIGNED_INT);
	switch (type->bytes)
	{
		case 8:
			return &type_ulong;
		case 4:
			return &type_uint;
		case 2:
			return &type_ushort;
		case 1:
			return &type_byte;
		default:
			UNREACHABLE
	}
}



BinOp bin_op[256] = {
		[TOKEN_EQ] = BINOP_ASSIGN,
		[TOKEN_STAR] = BINOP_MULT,
		[TOKEN_PLUS] = BINOP_ADD,
		[TOKEN_MINUS] = BINOP_SUB,
		[TOKEN_DIV] = BINOP_DIV,
		[TOKEN_MOD] = BINOP_MOD,
		[TOKEN_NOT_EQUAL] = BINOP_NE,
		[TOKEN_AND] = BINOP_AND,
		[TOKEN_OR] = BINOP_OR,
		[TOKEN_AMP] = BINOP_BIT_AND,
		[TOKEN_BIT_OR] = BINOP_BIT_OR,
		[TOKEN_BIT_XOR] = BINOP_BIT_XOR,
		[TOKEN_EQEQ] = BINOP_EQ,
		[TOKEN_GREATER] = BINOP_GT,
		[TOKEN_GREATER_EQ] = BINOP_GE,
		[TOKEN_LESS] = BINOP_LT,
		[TOKEN_LESS_EQ] = BINOP_LE,
		[TOKEN_SHR] = BINOP_SHR,
		[TOKEN_SHL] = BINOP_SHL,
		[TOKEN_ELVIS] = BINOP_ELVIS
};

AssignOp assign_op[256] = {
		[TOKEN_EQ] = ASSIGNOP_ASSIGN,
		[TOKEN_MULT_ASSIGN] = ASSIGNOP_MULT_ASSIGN,
		[TOKEN_PLUS_ASSIGN] = ASSIGNOP_ADD_ASSIGN,
		[TOKEN_MINUS_ASSIGN] = ASSIGNOP_SUB_ASSIGN,
		[TOKEN_DIV_ASSIGN] = ASSIGNOP_DIV_ASSIGN,
		[TOKEN_MOD_ASSIGN] = ASSIGNOP_MOD_ASSIGN,
		[TOKEN_AND_ASSIGN] = ASSIGNOP_AND_ASSIGN,
		[TOKEN_OR_ASSIGN] = ASSIGNOP_OR_ASSIGN,
		[TOKEN_BIT_AND_ASSIGN] = ASSIGNOP_BIT_AND_ASSIGN,
		[TOKEN_BIT_OR_ASSIGN] = ASSIGNOP_BIT_OR_ASSIGN,
		[TOKEN_BIT_XOR_ASSIGN] = ASSIGNOP_BIT_XOR_ASSIGN,
		[TOKEN_SHR_ASSIGN] = ASSIGNOP_SHR_ASSIGN,
		[TOKEN_SHL_ASSIGN] = ASSIGNOP_SHL_ASSIGN,
};

UnaryOp unary_op[256] = {
		[TOKEN_STAR] = UNARYOP_DEREF,
		[TOKEN_AMP] = UNARYOP_ADDR,
		[TOKEN_BIT_NOT] = UNARYOP_BITNEG,
		[TOKEN_NOT] = UNARYOP_NOT,
		[TOKEN_MINUS] = UNARYOP_NEG,
		[TOKEN_PLUSPLUS] = UNARYOP_INC,
		[TOKEN_MINUSMINUS] = UNARYOP_DEC,
};


BinOp binop_from_token(TokenType type)
{
	return bin_op[type];
}

AssignOp assignop_from_token(TokenType type)
{
	return assign_op[type];
}

TokenType assignop_to_token(AssignOp type)
{
	for (unsigned i = 0; i < 256; i++)
	{
		if (assign_op[i] == type) return (TokenType)i;
	}
	return TOKEN_INVALID_TOKEN;
}

TokenType binop_to_token(BinOp type)
{
	for (unsigned i = 0; i < 256; i++)
	{
		if (bin_op[i] == type) return (TokenType)i;
	}
	return TOKEN_INVALID_TOKEN;
}


UnaryOp unaryop_from_token(TokenType type)
{
	return unary_op[type];
}

TokenType unaryop_to_token(UnaryOp type)
{
	for (unsigned i = 0; i < 256; i++)
	{
		if (unary_op[i] == type) return (TokenType)i;
	}
	return TOKEN_INVALID_TOKEN;
}

Ast poisoned_ast = { .ast_kind = AST_POISONED };

void fprint_indent(FILE *file, int indent)
{
	for (int i = 0; i < indent * 2; i++) fprintf(file, " ");
}

void fprint_endparen(FILE *file, int indent)
{
	fprint_indent(file, indent);
	fprintf(file, ")\n");
}

void fprint_type_recursive(FILE *file, Type *type, int indent)
{
	fprint_indent(file, indent);
	if (!type)
	{
		fprintf(file, "(none)\n");
		return;
	}
	switch (type->type_kind)
	{
		case TYPE_POISONED:
			fprintf(file, "(POISON)\n");
			return;
		case TYPE_UNRESOLVED:
			if (type->unresolved.module.string)
			{
				fprintf(file, "(unresolved %s::%s)\n", type->unresolved.module.string, type->unresolved.name.string);
			}
			else
			{
				fprintf(file, "(unresolved %s)\n", type->unresolved.name.string);
			}
			return;
		case TYPE_UNRESOLVED_EXPR:
			fprintf(file, "(unresolved\n");
			fprint_expr_recursive(file, type->unresolved_type_expr, indent + 1);
			fprint_endparen(file, indent);
			return;
		case TYPE_VOID:
			fprintf(file, "(void)\n");
			return;
		case TYPE_OPAQUE:
			break;
		case TYPE_BUILTIN:
			fprintf(file, "(builtin)\n");
			return;
		case TYPE_NIL:
			fprintf(file, "(nil)\n");
			return;
		case TYPE_POINTER:
			fprintf(file, "(pointer\n");
			fprint_type_recursive(file, type->base, indent + 1);
			fprint_endparen(file, indent);
			return;
		case TYPE_STRING:
			fprintf(file, "(string)\n");
			return;
		case TYPE_ARRAY:
			fprintf(file, "(array [%zu]\n", type->len);
			fprint_type_recursive(file, type->base, indent + 1);
			fprint_endparen(file, indent);
			return;
		case TYPE_INC_ARRAY:
			break;
		case TYPE_UNRESOLVED_ARRAY:
			fprintf(file, "(array\n");
			fprint_type_recursive(file, type->base, indent + 1);
			fprint_expr_recursive(file, type->unresolved_len, indent + 1);
			fprint_endparen(file, indent);
			return;
		case TYPE_TYPEDEF:
			break;
		case TYPE_MACRO:
			break;
		case TYPE_FUNC_TYPE:
			break;
		case TYPE_ENUM:
			break;
		case TYPE_ERROR:
			break;
		case TYPE_FUNC:
			break;
		case TYPE_STRUCT:
			break;
		case TYPE_UNION:
			break;
		case TYPE_GENERIC:
			break;
	}
	fprintf(file, "(TYPETODO)\n");
}
void fprint_expr_recursive(FILE *file, Expr *expr, int indent)
{
	fprint_indent(file, indent);
	switch (expr->expr_kind)
	{
		case EXPR_IDENTIFIER:
			fprintf(file, "(ident %s)\n", expr->identifier_expr.identifier.string);
			return;
		case EXPR_CONST:
			fprintf(file, "(const ");
			value_fprint(file, expr->const_expr);
			fprintf(file, ")\n");
			return;
		case EXPR_BINARY:
			fprintf(file, "(binary %s\n", token_type_to_string(binop_to_token(expr->binary_expr.operator)));
			fprint_expr_recursive(file, expr->binary_expr.left, indent + 1);
			fprint_expr_recursive(file, expr->binary_expr.right, indent + 1);
			break;
		case EXPR_UNARY:
			fprintf(file, "(unary %s\n", token_type_to_string(unaryop_to_token(expr->unary_expr.operator)));
			fprint_expr_recursive(file, expr->unary_expr.expr, indent + 1);
			break;
		case EXPR_POST_UNARY:
			fprintf(file, "(postunary %s\n", token_type_to_string(unaryop_to_token(expr->post_expr.operator)));
			fprint_expr_recursive(file, expr->post_expr.expr, indent + 1);
			break;
		case EXPR_METHOD_REF:
			fprintf(file, "(methodref .%s\n", expr->method_ref_expr.method.string);
			fprint_type_recursive(file, expr->method_ref_expr.type, indent + 1);
			break;
		case EXPR_STRUCT_VALUE:
			fprintf(file, "(structvalue\n");
			fprint_type_recursive(file, expr->struct_value_expr.type, indent + 1);
			fprint_expr_recursive(file, expr->struct_value_expr.init_expr, indent + 1);
			break;
		case EXPR_ACCESS:
			fprintf(file, "(access .%s\n", expr->access_expr.sub_element.string);
			fprint_expr_recursive(file, expr->access_expr.parent, indent + 1);
			break;
		case EXPR_TYPE:
			fprintf(file, "(type\n");
			fprint_type_recursive(file, expr->type_expr.type, indent + 1);
			break;
		case EXPR_CALL:
			fprintf(file, "(call\n");
			fprint_expr_recursive(file, expr->call_expr.function, indent + 1);
			{
				VECEACH(expr->call_expr.parameters, i)
				{
					fprint_expr_recursive(file, expr->call_expr.parameters[i], indent + 1);
				}
			}
			break;
		case EXPR_CONDITIONAL:
			if (!expr->conditional_expr.then_expr)
			{
				fprintf(file, "(elvis\n");
				fprint_expr_recursive(file, expr->conditional_expr.cond, indent + 1);
			}
			else
			{
				fprintf(file, "(cond\n");
				fprint_expr_recursive(file, expr->conditional_expr.cond, indent + 1);
				fprint_expr_recursive(file, expr->conditional_expr.then_expr, indent + 1);
			}
			fprint_expr_recursive(file, expr->conditional_expr.else_expr, indent + 1);
			break;
		case EXPR_INITIALIZER_LIST:
			fprintf(file, "(initializerlist\n");
			{
				VECEACH(expr->initializer_expr, i)
				{
					fprint_expr_recursive(file, expr->initializer_expr[i], indent + 1);
				}
			}
			break;
		case EXPR_SUBSCRIPT:
			fprintf(file, "(subscript\n");
			fprint_expr_recursive(file, expr->subscript_expr.expr, indent + 1);
			fprint_expr_recursive(file, expr->subscript_expr.index, indent + 1);
			break;
		case EXPR_EXPRESSION_LIST:
			fprintf(file, "(expressionlist\n");
			{
				VECEACH(expr->expression_list, i)
				{
					fprint_expr_recursive(file, expr->expression_list[i], indent + 1);
				}
			}
			break;
		case EXPR_TRY:
			if (!expr->try_expr.else_expr)
			{
				fprintf(file, "(try\n");
				fprint_expr_recursive(file, expr->try_expr.expr, indent + 1);
			}
			else
			{
				fprintf(file, "(try-else\n");
				fprint_expr_recursive(file, expr->try_expr.expr, indent + 1);
				fprint_expr_recursive(file, expr->try_expr.else_expr, indent + 1);
			}
			break;
		default:
			fprintf(file, "(TODOEXPR)\n");
			return;
	}
	fprint_endparen(file, indent);
}

static void fprint_decl_list(FILE *file, Decl **decls, int indent);
static void fprint_ast_recursive(FILE *file, Ast *ast, int indent);

void fprint_func_signature(FILE *file, FunctionSignature *signature, int indent)
{
	fprint_type_recursive(file, signature->rtype, indent);
	fprint_indent(file, indent);
	fprintf(file, "(params\n");
	fprint_decl_list(file, signature->params, indent + 1);
	fprint_endparen(file, indent);
	// TODO throws, variable
}
void fprint_decl_recursive(FILE *file, Decl *decl, int indent)
{
	fprint_indent(file, indent);
	switch (decl->decl_kind)
	{
		case DECL_MULTI_DECL:
			fprintf(file, "(multi-decl\n");
			fprint_decl_list(file, decl->multi_decl, indent + 1);
			break;
		case DECL_VAR:
			if (!decl->var.init_expr)
			{
				fprintf(file, "(var %s)\n", decl->name.string);
				return;
			}
			fprintf(file, "(var %s\n", decl->name.string);
			fprint_expr_recursive(file, decl->var.init_expr, indent + 1);
			break;
		case DECL_MACRO:
			fprintf(file, "(macro %s\n", decl->name.string);
			fprint_type_recursive(file, decl->macro_decl.rtype, indent + 1);
			fprint_indent(file, indent + 1);
			fprintf(file, "(params\n");
			fprint_decl_list(file, decl->macro_decl.parameters, indent + 2);
			fprint_endparen(file, indent + 1);
			fprint_ast_recursive(file, decl->macro_decl.body, indent + 1);
			break;
		case DECL_FUNC:
			fprintf(file, "(func %s\n", decl->name.string);
			fprint_type_recursive(file, decl->func.struct_parent, indent + 1);
			fprint_func_signature(file, &decl->func.function_signature, indent + 1);
			fprint_ast_recursive(file, decl->func.body, indent + 1);
			break;
		case DECL_STRUCT:
			fprintf(file, "(struct %s\n", decl->name.string);
			fprint_decl_list(file, decl->strukt.members, indent + 1);
			break;
		case DECL_UNION:
			fprintf(file, "(union %s\n", decl->name.string);
			fprint_decl_list(file, decl->strukt.members, indent + 1);
			break;
		case DECL_ENUM:
			fprintf(file, "(enum %s\n", decl->name.string);
			fprint_type_recursive(file, decl->enums.type, indent + 1);
			fprint_decl_list(file, decl->enums.values, indent + 1);
			break;
		case DECL_ERROR:
			fprintf(file, "(error %s\n", decl->name.string);
			fprint_decl_list(file, decl->error.error_constants, indent + 1);
			break;
		case DECL_ENUM_CONSTANT:
			if (!decl->enum_constant.expr)
			{
				fprintf(file, "(enum-constant %s)\n", decl->name.string);
				return;
			}
			fprintf(file, "(enum-constant %s\n", decl->name.string);
			fprint_expr_recursive(file, decl->enum_constant.expr, indent + 1);
			break;
		case DECL_ERROR_CONSTANT:
			fprintf(file, "(error-constant %s)\n", decl->name.string);
			return;
		case DECL_GENERIC:
			fprintf(file, "(generic %s\n", decl->name.string);
			fprint_indent(file, indent + 1);
			fprintf(file, "(params\n");
			{
				VECEACH(decl->generic_decl.parameters, i)
				{
					fprint_indent(file, indent + 2);
					fprintf(file, "%s\n", decl->generic_decl.parameters[i].string);
				}
			}
			fprint_endparen(file, indent + 1);
			fprint_indent(file, indent + 1);
			fprintf(file, "(cases\n");
			{
				VECEACH(decl->generic_decl.cases, i)
				{
					fprint_ast_recursive(file, decl->generic_decl.cases[i], indent + 2);
				}
			}
			fprint_endparen(file, indent + 2);
			break;
		case DECL_TYPEDEF:
			fprintf(file, "(typedef %s\n", decl->name.string);
			if (decl->typedef_decl.is_func)
			{
				fprint_func_signature(file, &decl->typedef_decl.function_signature, indent + 1);
			}
			else
			{
				fprint_type_recursive(file, decl->typedef_decl.type, indent + 1);
			}
			break;
		case DECL_CT_IF:
			fprintf(file, "(ct-if\n");
			fprint_expr_recursive(file, decl->ct_if_decl.expr, indent + 1);
			fprint_decl_list(file, decl->ct_if_decl.then, indent + 1);
			if (decl->ct_if_decl.elif)
			{
				fprint_decl_recursive(file, decl->ct_if_decl.elif, indent + 1);
			}
			break;
		case DECL_CT_ELIF:
			fprintf(file, "(ct-elif\n");
			fprint_expr_recursive(file, decl->ct_elif_decl.expr, indent + 1);
			fprint_decl_list(file, decl->ct_elif_decl.then, indent + 1);
			if (decl->ct_elif_decl.elif)
			{
				fprint_decl_recursive(file, decl->ct_elif_decl.elif, indent + 1);
			}
			break;
		case DECL_CT_ELSE:
			fprintf(file, "(ct-else\n");
			fprint_decl_list(file, decl->ct_else_decl, indent + 1);
			break;
		case DECL_POISONED:
			fprintf(file, "(poisoned-decl)\n");
			return;
		case DECL_BUILTIN:
			fprintf(file, "(builtin %s)\n", decl->name.string);
			break;
		case DECL_FUNC_TYPE:
			TODO
			break;
		case DECL_ARRAY_VALUE:
			TODO
			break;
		case DECL_IMPORT:
			fprintf(file, "(import %s", decl->name.string);
			TODO
			break;
	}
	fprint_endparen(file, indent);
}

static void fprint_decl_list(FILE *file, Decl **decls, int indent)
{
	VECEACH(decls, i)
	{
		fprint_decl_recursive(file, decls[i], indent);
	}
}

static void fprint_ast_recursive(FILE *file, Ast *ast, int indent)
{
	fprint_indent(file, indent);
	switch (ast->ast_kind)
	{
		case AST_COMPOUND_STMT:
			if (!ast->compound_stmt.stmts)
			{
				fprintf(file, "(compound)\n");
				return;
			}
			fprintf(file, "(compound\n");
			{
				VECEACH(ast->compound_stmt.stmts, i)
				{
					fprint_ast_recursive(file, ast->compound_stmt.stmts[i], indent + 1);
				}
			}
			break;
		case AST_DECLARE_STMT:
			fprintf(file, "(declare\n");
			fprint_decl_recursive(file, ast->declare_stmt, indent + 1);
			break;
		case AST_EXPR_STMT:
			fprintf(file, "(exprstmt\n");
			fprint_expr_recursive(file, ast->expr_stmt, indent + 1);
			break;
		case AST_WHILE_STMT:
			fprintf(file, "(while\n");
			fprint_ast_recursive(file, ast->while_stmt.cond, indent + 1);
			fprint_ast_recursive(file, ast->while_stmt.body, indent + 1);
			break;
		case AST_DO_STMT:
			fprintf(file, "(do\n");
			fprint_ast_recursive(file, ast->do_stmt.body, indent + 1);
			fprint_expr_recursive(file, ast->do_stmt.expr, indent + 1);
			break;
		case AST_RETURN_STMT:
			if (ast->return_stmt.expr)
			{
				fprintf(file, "(return\n");
				fprint_expr_recursive(file, ast->expr_stmt, indent + 1);
				break;
			}
			else
			{
				fprintf(file, "(return)\n");
				return;
			}
		case AST_BREAK_STMT:
			fprintf(file, "(break)\n");
			return;
		case AST_NEXT_STMT:
			fprintf(file, "(next)\n");
			return;
		case AST_CONTINUE_STMT:
			fprintf(file, "(continue)\n");
			return;
		case AST_DEFAULT_STMT:
			fprintf(file, "(default)\n");
			return;
		case AST_FOR_STMT:
			fprintf(file, "(for\n");
			if (ast->for_stmt.init)
			{
				fprint_ast_recursive(file, ast->for_stmt.init, indent + 1);
			}
			else
			{
				fprint_indent(file, indent + 1);
				fprintf(file, "(noinit)\n");
			}
			if (ast->for_stmt.cond)
			{
				fprint_expr_recursive(file, ast->for_stmt.cond, indent + 1);
			}
			else
			{
				fprint_indent(file, indent + 1);
				fprintf(file, "(nocond)\n");
			}
			if (ast->for_stmt.incr)
			{
				fprint_expr_recursive(file, ast->for_stmt.incr, indent + 1);
			}
			else
			{
				fprint_indent(file, indent + 1);
				fprintf(file, "(noincr)\n");
			}
			fprint_ast_recursive(file, ast->for_stmt.body, indent + 1);
			break;
		case AST_IF_STMT:
			fprintf(file, "(if\n");
			fprint_ast_recursive(file, ast->if_stmt.cond, indent + 1);
			fprint_ast_recursive(file, ast->if_stmt.then_body, indent + 1);
			if (ast->if_stmt.else_body)
			{
				fprint_ast_recursive(file, ast->if_stmt.else_body, indent + 1);
			}
			break;
		case AST_DECL_EXPR_LIST:
			fprintf(file, "(declexprlist\n");
			if (ast->decl_expr_list.list_type == DECLEXPR_EXPR)
			{
				fprint_expr_recursive(file, ast->decl_expr_list.expr, indent + 1);
			}
			else
			{
				fprint_decl_recursive(file, ast->decl_expr_list.decl, indent + 1);
			}
			break;
		case AST_COND_STMT:
			fprintf(file, "(condstmt\n");
			fprint_decl_recursive(file, ast->cond_stmt.decl, indent + 1);
			fprint_ast_recursive(file, ast->cond_stmt.decl_expr, indent + 1);
			break;
	    case AST_SWITCH_STMT:
            fprintf(file, "(condstmt\n");
            fprint_ast_recursive(file, ast->switch_stmt.cond, indent + 1);
            fprint_ast_recursive(file, ast->switch_stmt.body, indent + 1);
            break;
		case AST_CASE_STMT:
			fprintf(file, "(case\n");
			fprint_expr_recursive(file, ast->case_stmt.expr, indent + 1);
			break;
	    case AST_DEFER_STMT:
            fprintf(file, "(defer\n");
            fprint_ast_recursive(file, ast->defer_stmt.body, indent + 1);
            break;
		case AST_GENERIC_CASE_STMT:
			fprintf(file, "(generic-case\n");
			fprint_indent(file, indent + 1);
			fprintf(file, "(match\n");
			{
				VECEACH(ast->generic_case_stmt.types, i)
				{
					fprint_type_recursive(file, ast->generic_case_stmt.types[i], indent + 2);
				}
			}
			fprint_endparen(file, indent + 1);
			fprint_ast_recursive(file, ast->generic_case_stmt.body, indent + 1);
			break;
		case AST_GENERIC_DEFAULT_STMT:
			fprintf(file, "(generic-default\n");
			fprint_ast_recursive(file, ast->generic_default_stmt, indent + 1);
			break;
		case AST_POISONED:
			fprintf(file, "(ast-poisoned)\n");
			return;
		case AST_ASM_STMT:
			TODO
			break;
		case AST_ATTRIBUTE:
			TODO
			break;
		case AST_CATCH_STMT:
			TODO
			break;
		case AST_CT_IF_STMT:
			TODO
			break;
		case AST_CT_ELIF_STMT:
			TODO
			break;
		case AST_CT_ELSE_STMT:
			TODO
			break;
		case AST_GOTO_STMT:
			fprintf(file, "(goto %s)\n", ast->token.string);
			return;
		case AST_LABEL:
			fprintf(file, "(label %s)\n", ast->token.string);
			return;
		case AST_NOP_STMT:
			TODO
			break;
		case AST_THROW_STMT:
			fprintf(file, "(throw\n");
			fprint_expr_recursive(file, ast->throw_stmt, indent + 1);
			break;
		case AST_TRY_STMT:
			TODO
			break;
		case AST_VOLATILE_STMT:
			TODO
			break;
	}
	fprint_endparen(file, indent);
}
void fprint_ast(FILE *file, Ast *ast)
{
	fprint_ast_recursive(file, ast, 0);
}
void fprint_decl(FILE *file, Decl *dec)
{
	fprint_decl_recursive(file, dec, 0);
}
Module module_poisoned = { .name = "INVALID" };