// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"


Decl *decl_new(DeclKind decl_kind, TokenId name, Visibility visibility)
{
	Decl *decl = decl_calloc();
	decl->decl_kind = decl_kind;
	decl->name_token = name;
	decl->span = source_span_from_token_id(name);
	if (name.index)
	{
		decl->name = TOKSTR(name);
	}
	else
	{
		decl->name = NULL;
	}
	decl->visibility = visibility;
	return decl;
}


static Type poison_type = { .type_kind = TYPE_POISONED };
static TypeInfo poison_type_info = { .kind = TYPE_INFO_POISON };
Type *poisoned_type = &poison_type;
TypeInfo *poisoned_type_info = &poison_type_info;

const char *decl_to_name(Decl *decl)
{
	switch (decl->decl_kind)
	{
		case DECL_BITSTRUCT:
			return "bitstruct";
		case DECL_POISONED:
			return "poisoned decl";
		case DECL_CT_ASSERT:
			return "compile time assert";
		case DECL_CT_CASE:
			return "compile time case";
		case DECL_CT_ELIF:
			return "compile time else if";
		case DECL_CT_ELSE:
			return "compile time else";
		case DECL_CT_IF:
			return "compile time if";
		case DECL_CT_SWITCH:
			return "compile time switch";
		case DECL_IMPORT:
			return "import";
		case DECL_LABEL:
			return "label";
		case DECL_ATTRIBUTE:
			return "attribute";
		case DECL_DEFINE:
		case DECL_TYPEDEF:
			return "define";
		case DECL_DISTINCT:
			return "distinct type";
		case DECL_ENUM:
			return "enum";
		case DECL_ENUM_CONSTANT:
			return "enum value";
		case DECL_ERRVALUE:
			return "err value";
		case DECL_ERRTYPE:
			return "errtype";
		case DECL_FUNC:
			return "function";
		case DECL_GENERIC:
			return "generic";
		case DECL_MACRO:
			return "macro";
		case DECL_STRUCT:
			return "struct";
		case DECL_UNION:
			return "union";
		case DECL_VAR:
			switch (decl->var.kind)
			{
				case VARDECL_ERASE:
				case VARDECL_REWRAPPED:
					UNREACHABLE
				case VARDECL_CONST:
					return "constant";
				case VARDECL_GLOBAL:
					return "global variable";
				case VARDECL_LOCAL:
					return "variable";
				case VARDECL_PARAM:
					return "parameter";
				case VARDECL_MEMBER:
				case VARDECL_BITMEMBER:
					return "member";
				case VARDECL_PARAM_CT:
					return "compile time parameter";
				case VARDECL_PARAM_CT_TYPE:
					return "compile time type parameter";
				case VARDECL_PARAM_REF:
					return "ref parameter";
				case VARDECL_PARAM_EXPR:
					return "extpression parameter";
				case VARDECL_LOCAL_CT:
					return "compile time variable";
				case VARDECL_LOCAL_CT_TYPE:
					return "compile time type variable";
				case VARDECL_UNWRAPPED:
					return "unwrapped";
			}
			UNREACHABLE
	}
	UNREACHABLE
}
void decl_set_external_name(Decl *decl)
{
	if (decl->visibility == VISIBLE_EXTERN)
	{
		assert(decl->name);
		decl->external_name = decl->name;
		return;
	}
	scratch_buffer_clear();
	scratch_buffer_append(decl->module->name->module);
	scratch_buffer_append(".");
	scratch_buffer_append(decl->name ? decl->name : "anon");
	decl->external_name = scratch_buffer_interned();
}

Decl *decl_new_with_type(TokenId name, DeclKind decl_type, Visibility visibility)
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
		case DECL_ERRTYPE:
			kind = TYPE_ERRTYPE;
			break;
		case DECL_ENUM:
			kind = TYPE_ENUM;
			break;
		case DECL_DISTINCT:
			kind = TYPE_DISTINCT;
			break;
		case DECL_TYPEDEF:
			kind = TYPE_TYPEDEF;
			break;
		case DECL_BITSTRUCT:
			kind = TYPE_BITSTRUCT;
			break;
		case DECL_POISONED:
		case DECL_VAR:
		case DECL_ENUM_CONSTANT:
		case DECL_ERRVALUE:
		case DECL_IMPORT:
		case DECL_MACRO:
		case DECL_GENERIC:
		case DECL_CT_IF:
		case DECL_CT_ELSE:
		case DECL_CT_ELIF:
		case DECL_ATTRIBUTE:
		case DECL_LABEL:
		case DECL_CT_SWITCH:
		case DECL_CT_CASE:
		case DECL_DEFINE:
		case DECL_CT_ASSERT:
			UNREACHABLE
	}
	Type *type = type_new(kind, !name.index ? "anon" : TOKSTR(name));
	type->canonical = type;
	type->decl = decl;
	decl->type = type;
	return decl;
}

static Decl poison_decl = { .decl_kind = DECL_POISONED, .resolve_status = RESOLVE_DONE };
Decl *poisoned_decl = &poison_decl;

Decl *decl_new_var(TokenId name, TypeInfo *type, VarDeclKind kind, Visibility visibility)
{
	Decl *decl = decl_new(DECL_VAR, name, visibility);
	decl->var.kind = kind;
	decl->var.type_info = type;
	return decl;
}

Decl *decl_new_generated_var(const char *name, Type *type, VarDeclKind kind, SourceSpan span)
{
	Decl *decl = decl_calloc();
	decl->decl_kind = DECL_VAR;
	decl->name_token = NO_TOKEN_ID;
	decl->span = span;
	decl->name = name;
	decl->visibility = VISIBLE_LOCAL;
	decl->var.kind = kind;
	decl->type = type;
	decl->var.type_info = type_info_new_base(type, span);
	decl->resolve_status = RESOLVE_DONE;
	return decl;
}

bool expr_is_pure(Expr *expr)
{
	if (!expr) return true;
	switch (expr->expr_kind)
	{
		case EXPR_BUILTIN:
			return false;
		case EXPR_CONST:
		case EXPR_CONST_IDENTIFIER:
		case EXPR_IDENTIFIER:
		case EXPR_NOP:
			return true;
		case EXPR_BITASSIGN:
			return false;
		case EXPR_BINARY:
			if (expr->binary_expr.operator >= BINARYOP_ASSIGN) return false;
			return expr_is_pure(expr->binary_expr.right) && expr_is_pure(expr->binary_expr.left);
		case EXPR_UNARY:
			switch (expr->unary_expr.operator)
			{
				case UNARYOP_INC:
				case UNARYOP_DEC:
				case UNARYOP_TADDR:
					return false;
				default:
					return expr_is_pure(expr->unary_expr.expr);
			}
		case EXPR_BITACCESS:
		case EXPR_ACCESS:
			return expr_is_pure(expr->access_expr.parent);
		case EXPR_POISONED:
		case EXPR_CT_IDENT:
		case EXPR_TYPEID:
		case EXPR_CT_CALL:
			UNREACHABLE
		case EXPR_MACRO_BODY_EXPANSION:
		case EXPR_CALL:
		case EXPR_CATCH_UNWRAP:
		case EXPR_COMPOUND_LITERAL:
		case EXPR_COND:
		case EXPR_DESIGNATOR:
		case EXPR_DECL:
		case EXPR_OR_ERROR:
		case EXPR_EXPR_BLOCK:
		case EXPR_FAILABLE:
		case EXPR_RETHROW:
		case EXPR_HASH_IDENT:
		case EXPR_MACRO_BLOCK:
		case EXPR_MACRO_EXPANSION:
		case EXPR_FLATPATH:
		case EXPR_INITIALIZER_LIST:
		case EXPR_DESIGNATED_INITIALIZER_LIST:
		case EXPR_PLACEHOLDER:
		case EXPR_POST_UNARY:
		case EXPR_SCOPED_EXPR:
		case EXPR_SLICE_ASSIGN:
		case EXPR_TRY_UNWRAP:
		case EXPR_TRY_UNWRAP_CHAIN:
		case EXPR_UNDEF:
		case EXPR_TYPEINFO:
		case EXPR_FORCE_UNWRAP:
			return false;
		case EXPR_CAST:
			return expr_is_pure(expr->cast_expr.expr);
		case EXPR_EXPRESSION_LIST:
			VECEACH(expr->expression_list, i)
			{
				if (!expr_is_pure(expr->expression_list[i])) return false;
			}
			return true;
		case EXPR_LEN:
			return expr_is_pure(expr->len_expr.inner);
		case EXPR_SLICE:
			return expr_is_pure(expr->slice_expr.expr)
			       && expr_is_pure(expr->slice_expr.start)
			       && expr_is_pure(expr->slice_expr.end);
		case EXPR_SUBSCRIPT:
		case EXPR_SUBSCRIPT_ADDR:
			return expr_is_pure(expr->subscript_expr.expr)
			       && expr_is_pure(expr->subscript_expr.index);
		case EXPR_TERNARY:
			return expr_is_pure(expr->ternary_expr.cond)
			       && expr_is_pure(expr->ternary_expr.else_expr)
			       && expr_is_pure(expr->ternary_expr.then_expr);
		case EXPR_TRY:
		case EXPR_GROUP:
		case EXPR_CATCH:
			return expr_is_pure(expr->inner_expr);
	}
	UNREACHABLE
}

bool expr_is_simple(Expr *expr)
{
	RETRY:
	switch (expr->expr_kind)
	{
		case EXPR_GROUP:
			expr = expr->inner_expr;
			goto RETRY;
		case EXPR_OR_ERROR:
		case EXPR_TERNARY:
			return false;
		case EXPR_RETHROW:
			expr = expr->rethrow_expr.inner;
			goto RETRY;
		default:
			return true;
		case EXPR_BINARY:
			switch (expr->binary_expr.operator)
			{
				case BINARYOP_AND:
				case BINARYOP_OR:
				case BINARYOP_GT:
				case BINARYOP_GE:
				case BINARYOP_LT:
				case BINARYOP_LE:
				case BINARYOP_NE:
				case BINARYOP_EQ:
				case BINARYOP_ASSIGN:
				case BINARYOP_ADD_ASSIGN:
				case BINARYOP_BIT_AND_ASSIGN:
				case BINARYOP_BIT_OR_ASSIGN:
				case BINARYOP_BIT_XOR_ASSIGN:
				case BINARYOP_DIV_ASSIGN:
				case BINARYOP_MOD_ASSIGN:
				case BINARYOP_MULT_ASSIGN:
				case BINARYOP_SHR_ASSIGN:
				case BINARYOP_SHL_ASSIGN:
				case BINARYOP_SUB_ASSIGN:
					return true;
				default:
					return false;
			}
			UNREACHABLE
		case EXPR_UNARY:
			switch (expr->unary_expr.operator)
			{
				case UNARYOP_NEG:
				case UNARYOP_BITNEG:
					return false;
				default:
					return true;
			}
			UNREACHABLE
	}
	UNREACHABLE
}


Expr *expr_new(ExprKind kind, SourceSpan start)
{
	Expr *expr = expr_calloc();
	expr->expr_kind = kind;
	expr->span = start;
	expr->type = NULL;
	return expr;
}

static Expr poison_expr = { .expr_kind = EXPR_POISONED, .resolve_status = RESOLVE_DONE };
Expr *poisoned_expr = &poison_expr;

BinaryOp binary_op[TOKEN_LAST + 1] = {
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
		[TOKEN_BIT_AND_ASSIGN] = BINARYOP_BIT_AND_ASSIGN,
		[TOKEN_BIT_OR_ASSIGN] = BINARYOP_BIT_OR_ASSIGN,
		[TOKEN_BIT_XOR_ASSIGN] = BINARYOP_BIT_XOR_ASSIGN,
		[TOKEN_SHR_ASSIGN] = BINARYOP_SHR_ASSIGN,
		[TOKEN_SHL_ASSIGN] = BINARYOP_SHL_ASSIGN,
};


static BinaryOp assign_binop[BINARYOP_LAST + 1] = {
		[BINARYOP_MULT_ASSIGN] = BINARYOP_MULT,
		[BINARYOP_ADD_ASSIGN] = BINARYOP_ADD,
		[BINARYOP_SUB_ASSIGN] = BINARYOP_SUB,
		[BINARYOP_DIV_ASSIGN] = BINARYOP_DIV,
		[BINARYOP_MOD_ASSIGN] = BINARYOP_MOD,
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

UnaryOp unary_op[TOKEN_LAST + 1] = {
		[TOKEN_STAR] = UNARYOP_DEREF,
		[TOKEN_AMP] = UNARYOP_ADDR,
		[TOKEN_AND] = UNARYOP_TADDR,
		[TOKEN_BIT_NOT] = UNARYOP_BITNEG,
		[TOKEN_BANG] = UNARYOP_NOT,
		[TOKEN_MINUS] = UNARYOP_NEG,
		[TOKEN_PLUSPLUS] = UNARYOP_INC,
		[TOKEN_MINUSMINUS] = UNARYOP_DEC,
};





BinaryOp binaryop_from_token(TokenType type)
{
	return binary_op[type];
}

TokenType binaryop_to_token(BinaryOp type)
{
	for (unsigned i = 0; i <= TOKEN_LAST; i++)
	{
		if (binary_op[i] == type) return (TokenType)i;
	}
	return TOKEN_INVALID_TOKEN;
}

UnaryOp unaryop_from_token(TokenType type)
{
	return unary_op[type];
}

static Ast poison_ast = { .ast_kind = AST_POISONED };
Ast *poisoned_ast = &poison_ast;


