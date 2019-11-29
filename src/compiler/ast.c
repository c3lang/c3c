// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "compiler_internal.h"

Decl *decl_new(DeclKind decl_kind, Token name, Visibility visibility)
{
	assert(name.string);
	Decl *decl = CALLOCS(Decl);
	decl->decl_kind = decl_kind;
	decl->name = name;
	decl->visibility = visibility;
	return decl;
}

Type poisoned_type = { .type_kind = TYPE_POISONED };

TypeInfo poisoned_type_info = { .kind = TYPE_INFO_POISON };


Decl *decl_new_with_type(Token name, DeclKind decl_type, Visibility visibility)
{
	Decl *decl = decl_new(decl_type, name, visibility);
	TypeKind kind = TYPE_POISONED;
	switch (decl_type)
	{
		case DECL_FUNC:
			kind = TYPE_FUNC;
			break;
		case DECL_UNION:
			kind = TYPE_UNION;
			break;
		case DECL_STRUCT:
			kind = TYPE_STRUCT;
			break;
		case DECL_ERROR:
			kind = TYPE_ERROR;
			break;
		case DECL_ENUM:
			kind = TYPE_ENUM;
			break;
		case DECL_TYPEDEF:
			kind = TYPE_TYPEDEF;
			break;
		case DECL_THROWS:
		case DECL_POISONED:
		case DECL_VAR:
		case DECL_ENUM_CONSTANT:
		case DECL_ERROR_CONSTANT:
		case DECL_ARRAY_VALUE:
		case DECL_IMPORT:
		case DECL_MACRO:
		case DECL_MULTI_DECL:
		case DECL_GENERIC:
		case DECL_CT_IF:
		case DECL_CT_ELSE:
		case DECL_CT_ELIF:
		case DECL_ATTRIBUTE:
			UNREACHABLE
	}
	Type *type = type_new(kind, name.string);
	type->canonical = type;
	type->decl = decl;
	decl->type = type;
	return decl;
}

const char *decl_var_to_string(VarDeclKind kind)
{
	switch (kind)
	{
		case VARDECL_CONST:
			return "const";
		case VARDECL_GLOBAL:
			return "global";
		case VARDECL_LOCAL:
			return "local";
		case VARDECL_PARAM:
			return "param";
		case VARDECL_MEMBER:
			return "member";
	}
	UNREACHABLE
}

Decl poisoned_decl = { .decl_kind = DECL_POISONED, .resolve_status = RESOLVE_DONE };

Decl *decl_new_var(Token name, TypeInfo *type, VarDeclKind kind, Visibility visibility)
{
	Decl *decl = decl_new(DECL_VAR, name, visibility);
	decl->var.kind = kind;
	decl->var.type_info = type;
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

Expr poisoned_expr = { .expr_kind = EXPR_POISONED, .resolve_status = RESOLVE_DONE };



Type* type_int_max_type(bool is_signed)
{
	return is_signed ? type_long : type_ulong;
}

/*
Type* type_get_unsigned(Type *type)
{
	assert(type->type_kind == TYPE_BUILTIN);
	Decl *decl = type->decl;
	if (decl->builtin.num_type == NUMBER_TYPE_UNSIGNED_INT) return type;
	assert(decl->builtin.num_type == NUMBER_TYPE_SIGNED_INT);
	switch (decl->builtin.bytes)
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

*/


BinaryOp binary_op[256] = {
		[TOKEN_STAR] = BINARYOP_MULT,
		[TOKEN_DIV] = BINARYOP_DIV,
		[TOKEN_PLUS] = BINARYOP_ADD,
		[TOKEN_MINUS] = BINARYOP_SUB,
		[TOKEN_MOD] = BINARYOP_MOD,
		[TOKEN_SHL] = BINARYOP_SHL,
		[TOKEN_SHR] = BINARYOP_SHR,
		[TOKEN_AND] = BINARYOP_AND,
		[TOKEN_OR] = BINARYOP_OR,
		[TOKEN_AMP] = BINARYOP_BIT_AND,
		[TOKEN_BIT_OR] = BINARYOP_BIT_OR,
		[TOKEN_BIT_XOR] = BINARYOP_BIT_XOR,
		[TOKEN_EQEQ] = BINARYOP_EQ,
		[TOKEN_NOT_EQUAL] = BINARYOP_NE,
		[TOKEN_LESS] = BINARYOP_LT,
		[TOKEN_LESS_EQ] = BINARYOP_LE,
		[TOKEN_GREATER] = BINARYOP_GT,
		[TOKEN_GREATER_EQ] = BINARYOP_GE,
		[TOKEN_EQ] = BINARYOP_ASSIGN,
		[TOKEN_MULT_ASSIGN] = BINARYOP_MULT_ASSIGN,
		[TOKEN_PLUS_ASSIGN] = BINARYOP_ADD_ASSIGN,
		[TOKEN_MINUS_ASSIGN] = BINARYOP_SUB_ASSIGN,
		[TOKEN_DIV_ASSIGN] = BINARYOP_DIV_ASSIGN,
		[TOKEN_MOD_ASSIGN] = BINARYOP_MOD_ASSIGN,
		[TOKEN_AND_ASSIGN] = BINARYOP_AND_ASSIGN,
		[TOKEN_OR_ASSIGN] = BINARYOP_OR_ASSIGN,
		[TOKEN_BIT_AND_ASSIGN] = BINARYOP_BIT_AND_ASSIGN,
		[TOKEN_BIT_OR_ASSIGN] = BINARYOP_BIT_OR_ASSIGN,
		[TOKEN_BIT_XOR_ASSIGN] = BINARYOP_BIT_XOR_ASSIGN,
		[TOKEN_SHR_ASSIGN] = BINARYOP_SHR_ASSIGN,
		[TOKEN_SHL_ASSIGN] = BINARYOP_SHL_ASSIGN,
};


static BinaryOp assign_binop[256] = {
		[BINARYOP_MULT_ASSIGN] = BINARYOP_MULT,
		[BINARYOP_ADD_ASSIGN] = BINARYOP_ADD,
		[BINARYOP_SUB_ASSIGN] = BINARYOP_SUB,
		[BINARYOP_DIV_ASSIGN] = BINARYOP_DIV,
		[BINARYOP_MOD_ASSIGN] = BINARYOP_MOD,
		[BINARYOP_AND_ASSIGN] = BINARYOP_AND,
		[BINARYOP_OR_ASSIGN] = BINARYOP_OR,
		[BINARYOP_BIT_AND_ASSIGN] = BINARYOP_BIT_AND,
		[BINARYOP_BIT_OR_ASSIGN] = BINARYOP_BIT_OR,
		[BINARYOP_BIT_XOR_ASSIGN] = BINARYOP_BIT_XOR,
		[BINARYOP_SHR_ASSIGN] = BINARYOP_SHR,
		[BINARYOP_SHL_ASSIGN] = BINARYOP_SHL,
};

BinaryOp binaryop_assign_base_op(BinaryOp assign_binary_op)
{
	return assign_binop[(int)assign_binary_op];
}

AssignOp assign_op[256] = {
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

BinaryOp binaryop_from_token(TokenType type)
{
	return binary_op[type];
}

TokenType binaryop_to_token(BinaryOp type)
{
	for (unsigned i = 0; i < 256; i++)
	{
		if (binary_op[i] == type) return (TokenType)i;
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

static void fprintf_indented(FILE *file, int indent, const char *string, ...)
{
	fprint_indent(file, indent);
	va_list list;
	va_start(list, string);
	vfprintf(file, string, list);
	va_end(list);
}

void fprint_endparen(FILE *file, int indent)
{
	fprint_indent(file, indent);
	fprintf(file, ")\n");
}

void fprint_decl_recursive(FILE *file, Decl *decl, int indent);

void fprint_type_recursive(FILE *file, Type *type, int indent)
{
	if (!type)
	{
		fprintf_indented(file, indent, "(none)\n");
		return;
	}
	switch (type->type_kind)
	{
		case TYPE_POISONED:
			fprintf_indented(file, indent, "(type poison)\n");
			return;
		case TYPE_FUNC:
			fprintf_indented(file, indent, "(type-func %s)\n", type->func.signature->mangled_signature);
			return;
		case TYPE_STRUCT:
			fprintf_indented(file, indent, "(struct %s::%s)\n", type->decl->module->name, type->decl->name.string);
			return;
		case TYPE_UNION:
			fprintf_indented(file, indent, "(union %s::%s)\n", type->decl->module->name, type->decl->name.string);
			return;
		case TYPE_ENUM:
			fprintf_indented(file, indent, "(enum %s::%s)\n", type->decl->module->name, type->decl->name.string);
			return;
		case TYPE_ERROR:
			fprintf_indented(file, indent, "(error %s::%s)\n", type->decl->module->name, type->decl->name.string);
			return;
		case TYPE_TYPEDEF:
			if (type->canonical != type)
			{
				fprintf_indented(file, indent, "(user-defined %s::%s\n", type->decl->module->name, type->decl->name.string);
				fprint_type_recursive(file, type->canonical, indent + 1);
				fprint_endparen(file, indent);
				break;
			}
			break;
		case TYPE_POINTER:
			fprintf_indented(file, indent, "(pointer\n");
			fprint_type_recursive(file, type->pointer, indent + 1);
			fprint_endparen(file, indent);
			break;
		case TYPE_SUBARRAY:
			fprintf_indented(file, indent, "(subarray\n");
			fprint_type_recursive(file, type->array.base, indent + 1);
			fprint_endparen(file, indent);
			break;
		case TYPE_VARARRAY:
			fprintf_indented(file, indent, "(vararray\n");
			fprint_type_recursive(file, type->array.base, indent + 1);
			fprint_endparen(file, indent);
			break;
		case TYPE_ARRAY:
			fprintf_indented(file, indent, "(array [%zu]\n", type->array.len);
			fprint_type_recursive(file, type->array.base, indent + 1);
			fprint_endparen(file, indent);
			break;
		case TYPE_VOID:
		case TYPE_BOOL:
		case TYPE_I8:
		case TYPE_I16:
		case TYPE_I32:
		case TYPE_I64:
		case TYPE_U8:
		case TYPE_U16:
		case TYPE_U32:
		case TYPE_U64:
		case TYPE_F32:
		case TYPE_F64:
			fprintf_indented(file, indent, "(type %s)\n", type->name);
			break;
		case TYPE_IXX:
			fprintf_indented(file, indent, "(comp time int)\n");
			break;
		case TYPE_UXX:
			fprintf_indented(file, indent, "(comp time uint)\n");
			break;
		case TYPE_FXX:
			fprintf_indented(file, indent, "(comp time float)\n");
			break;
		case TYPE_STRING:
			fprintf_indented(file, indent, "(string)\n");
			break;
		case TYPE_ERROR_UNION:
			TODO
	}
}

const char *resolve_status_to_string(ResolveStatus status)
{
	switch (status)
	{
		case RESOLVE_NOT_DONE:
			return "not_done";
		case RESOLVE_DONE:
			return "done";
		case RESOLVE_RUNNING:
			return "running";
		default:
			UNREACHABLE
	}
}

void fprint_type_info_recursive(FILE *file, TypeInfo *type_info, int indent)
{
	if (!type_info)
	{
		fprintf_indented(file, indent, "(type_info missing)\n");
		return;
	}
	fprintf_indented(file, indent, "(type_info\n");
	fprintf_indented(file, indent + 1, "(resolve_status %s)\n", resolve_status_to_string(type_info->resolve_status));
	if (type_info->resolve_status == RESOLVE_DONE)
	{
		fprint_type_recursive(file, type_info->type, indent + 1);
		fprint_endparen(file, indent);
		return;
	}
	switch (type_info->kind)
	{
		case TYPE_INFO_POISON:
			fprintf_indented(file, indent + 1, "(POISON)\n");
			break;
		case TYPE_INFO_IDENTIFIER:
			if (type_info->unresolved.path)
			{
				if (type_info->unresolved.path->module.string)
				{
					fprintf_indented(file, indent + 1, "(unresolved %s::%s::%s)\n", type_info->unresolved.path->module.string, type_info->unresolved.path->module.string, type_info->unresolved.name_loc.string);
				}
				else
				{
					fprintf_indented(file, indent + 1, "(unresolved %s::%s)\n", type_info->unresolved.path->module.string, type_info->unresolved.name_loc.string);
				}
				return;
			}
			fprintf_indented(file, indent + 1, "(unresolved %s)\n", type_info->unresolved.name_loc.string);
			break;
		case TYPE_INFO_ARRAY:
			fprintf_indented(file, indent + 1, "(unresolved-array\n");
			fprint_type_info_recursive(file, type_info->array.base, indent + 2);
			if (type_info->array.len) fprint_expr_recursive(file, type_info->array.len, indent + 1);
			fprint_endparen(file, indent + 1);
			break;
		case TYPE_INFO_POINTER:
			fprintf_indented(file, indent + 1, "(pointer\n");
			fprint_type_info_recursive(file, type_info->pointer, indent + 2);
			fprint_endparen(file, indent + 1);
			break;
		case TYPE_INFO_INC_ARRAY:
			fprintf_indented(file, indent + 1, "(incarray\n");
			fprint_type_info_recursive(file, type_info->array.base, indent + 2);
			fprint_endparen(file, indent + 1);
			break;
		case TYPE_INFO_EXPRESSION:
			fprintf_indented(file, indent + 1, "(typexpr\n");
			fprint_expr_recursive(file, type_info->unresolved_type_expr, indent + 2);
			fprint_endparen(file, indent + 1);
			break;
	}
	fprint_endparen(file, indent);
}

void fprint_expr_common(FILE *file, Expr *expr, int indent)
{
	switch (expr->resolve_status)
	{
		case RESOLVE_NOT_DONE:
			fprintf_indented(file, indent, "(unresolved)\n");
			break;
		case RESOLVE_RUNNING:
			fprintf_indented(file, indent, "(resolving)\n");
			break;
		case RESOLVE_DONE:
			fprint_type_recursive(file, expr->type, indent);
			break;
	}
}

void fprint_expr_recursive(FILE *file, Expr *expr, int indent)
{
	switch (expr->expr_kind)
	{
		case EXPR_IDENTIFIER:
			fprintf_indented(file, indent, "(ident %s\n", expr->identifier_expr.identifier.string);
			fprint_expr_common(file, expr, indent + 1);
			break;
		case EXPR_CONST:
			fprintf_indented(file, indent, "(const ");
			switch (expr->const_expr.type)
			{
				case CONST_NIL:
					fprintf(file, "nil\n");
					break;
				case CONST_BOOL:
					fprintf(file, expr->const_expr.b ? "true\n" : "false\n");
					break;
				case CONST_INT:
					if (expr->type->type_kind >= TYPE_U8 && expr->type->type_kind <= TYPE_UXX)
					{
						fprintf(file, "%llu\n", expr->const_expr.i);
					}
					else
					{
						fprintf(file, "%lld\n", (int64_t)expr->const_expr.i);
					}
					break;
				case CONST_FLOAT:
					fprintf(file, "%Lf\n", expr->const_expr.f);
					break;
				case CONST_STRING:
					fprintf(file, "%s\n", expr->const_expr.string.chars);
					break;
			}
			fprint_expr_common(file, expr, indent + 1);
			break;
		case EXPR_BINARY:
			fprintf_indented(file, indent, "(binary %s\n", token_type_to_string(expr->binary_expr.operator));
			fprint_expr_common(file, expr, indent + 1);
			fprint_expr_recursive(file, expr->binary_expr.left, indent + 1);
			fprint_expr_recursive(file, expr->binary_expr.right, indent + 1);
			break;
		case EXPR_UNARY:
			fprintf_indented(file, indent, "(unary %s\n", token_type_to_string(expr->unary_expr.operator));
			fprint_expr_common(file, expr, indent + 1);
			fprint_expr_recursive(file, expr->unary_expr.expr, indent + 1);
			break;
		case EXPR_POST_UNARY:
			fprintf_indented(file, indent, "(postunary %s\n", token_type_to_string(expr->post_expr.operator));
			fprint_expr_common(file, expr, indent + 1);
			fprint_expr_recursive(file, expr->post_expr.expr, indent + 1);
			break;
		case EXPR_TYPE_ACCESS:
			fprintf_indented(file, indent, "(typeaccess .%s\n", expr->type_access.name.string);
			fprint_expr_common(file, expr, indent + 1);
			fprint_type_info_recursive(file, expr->type_access.type, indent + 1);
			break;
		case EXPR_STRUCT_VALUE:
			fprintf_indented(file, indent, "(structvalue\n");
			fprint_expr_common(file, expr, indent + 1);
			fprint_type_info_recursive(file, expr->struct_value_expr.type, indent + 1);
			fprint_expr_recursive(file, expr->struct_value_expr.init_expr, indent + 1);
			break;
		case EXPR_ACCESS:
			fprintf_indented(file, indent, "(access .%s\n", expr->access_expr.sub_element.string);
			fprint_expr_common(file, expr, indent + 1);
			fprint_expr_recursive(file, expr->access_expr.parent, indent + 1);
			break;
		case EXPR_TYPE:
			fprintf_indented(file, indent, "(type\n");
			fprint_expr_common(file, expr, indent + 1);
			fprint_type_info_recursive(file, expr->type_expr.type, indent + 1);
			break;
		case EXPR_CALL:
			fprintf_indented(file, indent, "(call\n");
			fprint_expr_common(file, expr, indent + 1);
			fprint_expr_recursive(file, expr->call_expr.function, indent + 1);
			{
				VECEACH(expr->call_expr.arguments, i)
				{
					fprint_expr_recursive(file, expr->call_expr.arguments[i], indent + 1);
				}
			}
			break;
		case EXPR_CONDITIONAL:
			if (!expr->conditional_expr.then_expr)
			{
				fprintf_indented(file, indent, "(elvis\n");
				fprint_expr_common(file, expr, indent + 1);
				fprint_expr_recursive(file, expr->conditional_expr.cond, indent + 1);
			}
			else
			{
				fprintf_indented(file, indent, "(cond\n");
				fprint_expr_common(file, expr, indent + 1);
				fprint_expr_recursive(file, expr->conditional_expr.cond, indent + 1);
				fprint_expr_recursive(file, expr->conditional_expr.then_expr, indent + 1);
			}
			fprint_expr_recursive(file, expr->conditional_expr.else_expr, indent + 1);
			break;
		case EXPR_INITIALIZER_LIST:
			fprintf_indented(file, indent, "(initializerlist\n");
			fprint_expr_common(file, expr, indent + 1);
			{
				VECEACH(expr->initializer_expr, i)
				{
					fprint_expr_recursive(file, expr->initializer_expr[i], indent + 1);
				}
			}
			break;
		case EXPR_SUBSCRIPT:
			fprintf_indented(file, indent, "(subscript\n");
			fprint_expr_common(file, expr, indent + 1);
			fprint_expr_recursive(file, expr->subscript_expr.expr, indent + 1);
			fprint_expr_recursive(file, expr->subscript_expr.index, indent + 1);
			break;
		case EXPR_TRY:
			if (!expr->try_expr.else_expr)
			{
				fprintf_indented(file, indent, "(try\n");
				fprint_expr_common(file, expr, indent + 1);
				fprint_expr_recursive(file, expr->try_expr.expr, indent + 1);
			}
			else
			{
				fprintf_indented(file, indent, "(try-else\n");
				fprint_expr_common(file, expr, indent + 1);
				fprint_expr_recursive(file, expr->try_expr.expr, indent + 1);
				fprint_expr_recursive(file, expr->try_expr.else_expr, indent + 1);
			}
			break;
		case EXPR_CAST:
			fprintf_indented(file, indent, "cast\n");
			fprint_expr_common(file, expr, indent + 1);
			fprint_type_info_recursive(file, expr->cast_expr.type_info, indent + 1);
			fprint_expr_recursive(file, expr->cast_expr.expr, indent + 1);
			break;
		default:
			fprintf_indented(file, indent, "(TODOEXPR)\n");
			return;
	}
	fprint_endparen(file, indent);
}

static void fprint_decl_list(FILE *file, Decl **decls, int indent);
static void fprint_ast_recursive(FILE *file, Ast *ast, int indent);

void fprint_func_signature(FILE *file, FunctionSignature *signature, int indent)
{
	fprint_type_info_recursive(file, signature->rtype, indent);
	if (!vec_size(signature->params))
	{
		fprintf_indented(file, indent, "(params none)\n");
		return;
	}
	fprintf_indented(file, indent, "(params\n");
	fprint_decl_list(file, signature->params, indent + 1);
	fprint_endparen(file, indent);
	fprintf_indented(file, indent, "(throws\n");
	fprint_decl_list(file, signature->throws, indent + 1);
	fprint_endparen(file, indent);
}
void fprint_decl_recursive(FILE *file, Decl *decl, int indent)
{
	switch (decl->decl_kind)
	{
		case DECL_MULTI_DECL:
			fprintf_indented(file, indent, "(multi-decl\n");
			fprint_decl_list(file, decl->multi_decl, indent + 1);
			fprint_endparen(file, indent);
			break;
		case DECL_VAR:
			fprintf_indented(file, indent, "(var-%s %s\n", decl_var_to_string(decl->var.kind), decl->name.string ?: "");
			fprint_type_info_recursive(file, decl->var.type_info, indent + 1);
			if (decl->var.init_expr)
			{
				fprint_expr_recursive(file, decl->var.init_expr, indent + 1);
			}
			fprint_endparen(file, indent);
			break;
		case DECL_MACRO:
			fprintf_indented(file, indent, "(macro %s\n", decl->name.string);
			fprint_type_info_recursive(file, decl->macro_decl.rtype, indent + 1);
			fprint_indent(file, indent + 1);
			fprintf(file, "(params\n");
			fprint_decl_list(file, decl->macro_decl.parameters, indent + 2);
			fprint_endparen(file, indent + 1);
			fprint_ast_recursive(file, decl->macro_decl.body, indent + 1);
			fprint_endparen(file, indent);
			break;
		case DECL_FUNC:
			fprintf_indented(file, indent, "(func %s\n", decl->name.string);
			if (decl->func.type_parent)
			{
				fprint_indent(file, indent + 1);
				fprintf(file, "(parent_type\n");
				fprint_type_info_recursive(file, decl->func.type_parent, indent + 2);
				fprint_indent(file, indent + 1);
				fprintf(file, ")\n");
			}
			fprint_func_signature(file, &decl->func.function_signature, indent + 1);
			if (decl->func.body) fprint_ast_recursive(file, decl->func.body, indent + 1);
			fprint_endparen(file, indent);
			break;
		case DECL_STRUCT:
			fprintf_indented(file, indent, "(struct %s\n", decl->name.string);
			fprint_decl_list(file, decl->strukt.members, indent + 1);
			fprint_endparen(file, indent);
			break;
		case DECL_UNION:
			fprintf_indented(file, indent, "(union %s\n", decl->name.string);
			fprint_decl_list(file, decl->strukt.members, indent + 1);
			fprint_endparen(file, indent);
			break;
		case DECL_ENUM:
			fprintf_indented(file, indent, "(enum %s\n", decl->name.string);
			fprint_type_info_recursive(file, decl->enums.type_info, indent + 1);
			fprint_decl_list(file, decl->enums.values, indent + 1);
			fprint_endparen(file, indent);
			break;
		case DECL_ERROR:
			fprintf_indented(file, indent, "(error %s\n", decl->name.string);
			fprint_decl_list(file, decl->error.error_constants, indent + 1);
			fprint_endparen(file, indent);
			break;
		case DECL_ENUM_CONSTANT:
			if (!decl->enum_constant.expr)
			{
				fprintf_indented(file, indent, "(enum-constant %s)\n", decl->name.string);
				return;
			}
			fprintf_indented(file, indent, "(enum-constant %s\n", decl->name.string);
			fprint_expr_recursive(file, decl->enum_constant.expr, indent + 1);
			fprint_endparen(file, indent);
			break;
		case DECL_ERROR_CONSTANT:
			fprintf_indented(file, indent, "(error-constant %s)\n", decl->name.string);
			break;
		case DECL_GENERIC:
			fprintf_indented(file, indent, "(generic %s\n", decl->name.string);
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
			fprint_endparen(file, indent);
			break;
		case DECL_TYPEDEF:
			fprintf_indented(file, indent, "(typedef %s\n", decl->name.string);
			if (decl->typedef_decl.is_func)
			{
				fprint_func_signature(file, &decl->typedef_decl.function_signature, indent + 1);
			}
			else
			{
				fprint_type_info_recursive(file, decl->typedef_decl.type_info, indent + 1);
			}
			fprint_endparen(file, indent);
			break;
		case DECL_CT_IF:
			fprintf_indented(file, indent, "(ct-if\n");
			fprint_expr_recursive(file, decl->ct_if_decl.expr, indent + 1);
			fprint_decl_list(file, decl->ct_if_decl.then, indent + 1);
			if (decl->ct_if_decl.elif)
			{
				fprint_decl_recursive(file, decl->ct_if_decl.elif, indent + 1);
			}
			fprint_endparen(file, indent);
			break;
		case DECL_CT_ELIF:
			fprintf_indented(file, indent, "(ct-elif\n");
			fprint_expr_recursive(file, decl->ct_elif_decl.expr, indent + 1);
			fprint_decl_list(file, decl->ct_elif_decl.then, indent + 1);
			if (decl->ct_elif_decl.elif)
			{
				fprint_decl_recursive(file, decl->ct_elif_decl.elif, indent + 1);
			}
			fprint_endparen(file, indent);
			break;
		case DECL_CT_ELSE:
			fprintf_indented(file, indent, "(ct-else\n");
			fprint_decl_list(file, decl->ct_else_decl, indent + 1);
			fprint_endparen(file, indent);
			break;
		case DECL_POISONED:
			fprintf_indented(file, indent, "(poisoned-decl)\n");
			return;
		case DECL_ARRAY_VALUE:
			fprintf_indented(file, indent, "(array value");
			fprint_expr_recursive(file, decl->incr_array_decl, indent + 1);
			fprint_endparen(file, indent);
			return;
		case DECL_IMPORT:
			fprintf_indented(file, indent, "(import %s", decl->name.string);
			TODO
			fprint_endparen(file, indent);
			break;
		case DECL_ATTRIBUTE:
			TODO
		case DECL_THROWS:
			TODO
	}
}

static void fprint_decl_list(FILE *file, Decl **decls, int indent)
{
	VECEACH(decls, i)
	{
		fprint_decl_recursive(file, decls[i], indent);
	}
}

static void fprint_asts_recursive(FILE *file, Ast **asts, int indent)
{
	VECEACH(asts, i)
	{
		fprint_ast_recursive(file, asts[i], indent);
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
				fprint_asts_recursive(file, ast->compound_stmt.stmts, indent + 1);
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
		case AST_CT_FOR_STMT:
			if (ast->ct_for_stmt.index.string)
			{
				fprintf(file, "($for %s, %s\n", ast->ct_for_stmt.index.string, ast->ct_for_stmt.value.string);
			}
			else
			{
				fprintf(file, "($for  %s\n", ast->ct_for_stmt.value.string);
			}
			fprint_expr_recursive(file, ast->ct_for_stmt.expr, indent + 1);
			fprint_ast_recursive(file, ast->ct_for_stmt.body, indent + 1);
			break;
		case AST_DO_STMT:
			fprintf(file, "(do\n");
			fprint_ast_recursive(file, ast->do_stmt.body, indent + 1);
			fprint_expr_recursive(file, ast->do_stmt.expr, indent + 1);
			break;
		case AST_STMT_LIST:
			fprintf(file, "(stmtlist\n");
			fprint_asts_recursive(file, ast->stmt_list, indent + 1);
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
		case AST_COND_STMT:
			fprintf(file, "(cond\n");
			if (ast->cond_stmt.expr)
			{
				fprint_expr_recursive(file, ast->cond_stmt.expr, indent + 1);
			}
			else
			{
				fprint_indent(file, indent);
				fprintf(file, "(noexpr)");
			}
			fprint_asts_recursive(file, ast->cond_stmt.stmts, indent + 1);
			break;
		case AST_FOR_STMT:
			fprintf(file, "(for\n");
			fprint_ast_recursive(file, ast->for_stmt.cond, indent + 1);
			if (ast->for_stmt.incr)
			{
				fprint_ast_recursive(file, ast->for_stmt.incr, indent + 1);
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
	    case AST_SWITCH_STMT:
            fprintf(file, "(switchstmt\n");
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
					fprint_type_info_recursive(file, ast->generic_case_stmt.types[i], indent + 2);
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
		case AST_CT_SWITCH_STMT:
			TODO
			break;
		case AST_CT_CASE_STMT:
			TODO
			break;
		case AST_CT_DEFAULT_STMT:
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
Module poisoned_module = { .name = "INVALID" };
Decl all_error = { .decl_kind = DECL_ERROR, .name = { .type = TOKEN_INVALID_TOKEN, .string = NULL } };
