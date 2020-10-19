// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"

static void fprint_asts_recursive(Context *context, FILE *file, Ast **asts, int indent);
static void fprint_decl_list(Context *context, FILE *file, Decl **decls, int indent);
static void fprint_ast_recursive(Context *context, FILE *file, Ast *ast, int indent);

#define DUMP(text) do { fprintf_indented(file, indent, text); fprintf(file, "\n"); } while(0)
#define DUMPF(text, ...) do { fprintf_indented(file, indent, text, __VA_ARGS__); fprintf(file, "\n"); } while(0)
#define DUMPI(text) do { fprintf_indented(file, indent + 1, text); fprintf(file, "\n"); } while(0)
#define DUMPFI(text, ...) do { fprintf_indented(file, indent + 1, text, __VA_ARGS__); fprintf(file, "\n"); } while(0)
#define DUMPE() fprint_endparen(file, indent)
#define DUMPEND() fprint_endparen(file, indent); return
#define DUMPEXPR(_expr) fprint_expr_recursive(context, file, _expr, indent + 1)
#define DUMPAST(_ast) fprint_ast_recursive(context, file, _ast, indent + 1)
#define DUMPASTS(_asts) fprint_asts_recursive(context, file, _asts, indent + 1)
#define DUMPTI(_type_info) fprint_type_info_recursive(context, file, _type_info, indent + 1)
#define DUMPTYPE(_type) fprint_type_recursive(context, file, _type, indent + 1)
#define DUMPDECLS(_decls) fprint_decl_list(context, file, _decls, indent + 1)
#define DUMPDECL(_decl) fprint_decl_recursive(context, file, _decl, indent + 1)

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

unsigned decl_abi_alignment(Decl *decl)
{
	return decl->alignment ?: type_abi_alignment(decl->type);
}

void decl_set_external_name(Decl *decl)
{
	if (decl->visibility == VISIBLE_EXTERN)
	{
		assert(decl->name);
		decl->external_name = decl->name;
		return;
	}
	char buffer[1024];
	uint32_t len = sprintf(buffer, "%s.%s", decl->module->name->module, decl->name ? decl->name : "anon");
	assert(len);
	TokenType type = TOKEN_INVALID_TOKEN;
	decl->external_name = symtab_add(buffer, len, fnv1a(buffer, len), &type);
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
		case DECL_ERR:
			kind = TYPE_ERRTYPE;
			break;
		case DECL_ENUM:
			kind = TYPE_ENUM;
			break;
		case DECL_TYPEDEF:
			kind = TYPE_TYPEDEF;
			break;
		case DECL_POISONED:
		case DECL_VAR:
		case DECL_ENUM_CONSTANT:
		case DECL_ARRAY_VALUE:
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
			UNREACHABLE
	}
	Type *type = type_new(kind, !name.index ? "anon" : TOKSTR(name));
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
		case VARDECL_MEMBER:
			return "member";
		case VARDECL_PARAM:
			return "param";
		case VARDECL_ALIAS:
			return "alias";
		case VARDECL_PARAM_CT:
			return "$param";
		case VARDECL_PARAM_CT_TYPE:
			return "$Param";
		case VARDECL_PARAM_EXPR:
			return "#param";
		case VARDECL_PARAM_REF:
			return "&param";
		case VARDECL_LOCAL_CT:
			return "$local";
		case VARDECL_LOCAL_CT_TYPE:
			return "$Local";
	}
	UNREACHABLE
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
		[TOKEN_MULT_MOD] = BINARYOP_MULT_MOD,
		[TOKEN_DIV] = BINARYOP_DIV,
		[TOKEN_PLUS] = BINARYOP_ADD,
		[TOKEN_PLUS_MOD] = BINARYOP_ADD_MOD,
		[TOKEN_MINUS] = BINARYOP_SUB,
		[TOKEN_MINUS_MOD] = BINARYOP_SUB_MOD,
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
		[TOKEN_MULT_MOD_ASSIGN] = BINARYOP_MULT_MOD_ASSIGN,
		[TOKEN_PLUS_ASSIGN] = BINARYOP_ADD_ASSIGN,
		[TOKEN_PLUS_MOD_ASSIGN] = BINARYOP_ADD_MOD_ASSIGN,
		[TOKEN_MINUS_ASSIGN] = BINARYOP_SUB_ASSIGN,
		[TOKEN_MINUS_MOD_ASSIGN] = BINARYOP_SUB_MOD_ASSIGN,
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
		[BINARYOP_MULT_MOD_ASSIGN] = BINARYOP_MULT_MOD,
		[BINARYOP_ADD_ASSIGN] = BINARYOP_ADD,
		[BINARYOP_ADD_MOD_ASSIGN] = BINARYOP_ADD_MOD,
		[BINARYOP_SUB_ASSIGN] = BINARYOP_SUB,
		[BINARYOP_SUB_MOD_ASSIGN] = BINARYOP_SUB_MOD,
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
		[TOKEN_MINUS_MOD] = UNARYOP_NEGMOD,
		[TOKEN_PLUSPLUS] = UNARYOP_INC,
		[TOKEN_MINUSMINUS] = UNARYOP_DEC,
};


PostUnaryOp post_unary_op[TOKEN_LAST + 1] = {
		[TOKEN_PLUSPLUS] = POSTUNARYOP_INC,
		[TOKEN_MINUSMINUS] = POSTUNARYOP_DEC,
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

TokenType unaryop_to_token(UnaryOp type)
{
	for (unsigned i = 0; i <= TOKEN_LAST; i++)
	{
		if (unary_op[i] == type) return (TokenType)i;
	}
	return TOKEN_INVALID_TOKEN;
}

PostUnaryOp post_unaryop_from_token(TokenType type)
{
	return post_unary_op[type];
}

TokenType postunaryop_to_token(PostUnaryOp type)
{
	for (unsigned i = 0; i <= TOKEN_LAST; i++)
	{
		if (post_unary_op[i] == type) return (TokenType)i;
	}
	return TOKEN_INVALID_TOKEN;
}

static Ast poison_ast = { .ast_kind = AST_POISONED };
Ast *poisoned_ast = &poison_ast;

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

void fprint_decl_recursive(Context *context, FILE *file, Decl *decl, int indent);

void fprint_type_recursive(Context *context, FILE *file, Type *type, int indent)
{
	if (!type)
	{
		DUMP("(none)");
		return;
	}
	switch (type->type_kind)
	{

		case TYPE_COMPLEX:
			DUMP("(type complex");
			return;
		case TYPE_VECTOR:
			DUMP("(type vector");
			return;
		case TYPE_TYPEINFO:
			DUMP("(type typeinfo)");
			return;
		case TYPE_MEMBER:
			DUMP("(type member)");
			return;
		case TYPE_POISONED:
			DUMP("(type poison)");
			return;
		case TYPE_TYPEID:
			DUMP("(typeid)");
			return;
		case TYPE_FUNC:
			DUMPF("(func-type %s)", type->name);
			return;
		case TYPE_STRUCT:
			DUMPF("(struct %s)", type->name);
			return;
		case TYPE_UNION:
			DUMPF("(union %s)", type->name);
			return;
		case TYPE_ENUM:
			DUMPF("(enum %s)", type->name);
			return;
		case TYPE_ERRTYPE:
			DUMPF("(errtype %s)", type->name);
			return;
		case TYPE_TYPEDEF:
			DUMPF("(typedef %s", type->name);
			DUMPTYPE(type->canonical);
			DUMPEND();
		case TYPE_POINTER:
			DUMP("(pointer");
			DUMPTYPE(type->pointer);
			DUMPEND();
		case TYPE_SUBARRAY:
			DUMP("(subarray");
			DUMPTYPE(type->array.base);
			DUMPEND();
		case TYPE_VARARRAY:
			DUMP("(vararray");
			DUMPTYPE(type->array.base);
			DUMPEND();
		case TYPE_ARRAY:
			DUMPF("(array [%zu]", type->array.len);
			DUMPTYPE(type->array.base);
			DUMPEND();
		case TYPE_VOID:
		case TYPE_BOOL:
		case ALL_SIGNED_INTS:
		case ALL_UNSIGNED_INTS:
		case ALL_REAL_FLOATS:
			DUMPF("(%s)", type->name);
			return;
		case TYPE_IXX:
			DUMP("(ct int)");
			return;
		case TYPE_FXX:
			DUMP("(ct float)");
			return;
		case TYPE_STRING:
			DUMP("(string)");
			return;
		case TYPE_ERR_UNION:
			DUMP("(any-error)");
			return;
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


void fprint_type_info_recursive(Context *context, FILE *file, TypeInfo *type_info, int indent)
{
	if (!type_info)
	{
		DUMP("(type_info missing)");
		return;
	}
	DUMP("(type_info");
	DUMPFI("(resolve_status %s)", resolve_status_to_string(type_info->resolve_status));
	if (type_info->resolve_status == RESOLVE_DONE)
	{
		DUMPTYPE(type_info->type);
		DUMPEND();
	}
	indent++;
	switch (type_info->kind)
	{
		case TYPE_INFO_POISON:
			DUMP("(POISON)");
			break;
		case TYPE_INFO_IDENTIFIER:
			if (type_info->unresolved.path)
			{
				DUMPF("(unresolved %s::%s)\n", type_info->unresolved.path->module, TOKSTR(type_info->unresolved.name_loc));
				break;
			}
			DUMPF("(unresolved %s)", TOKSTR(type_info->unresolved.name_loc));
			break;
		case TYPE_INFO_VARARRAY:
			DUMP("(vararray");
			DUMPTI(type_info->array.base);
			DUMPE();
			break;
		case TYPE_INFO_SUBARRAY:
			DUMP("(subarray");
			DUMPTI(type_info->array.base);
			DUMPE();
			break;
		case TYPE_INFO_ARRAY:
			DUMP("(unresolved-array");
			DUMPTI(type_info->array.base);
			DUMPEXPR(type_info->array.len);
			DUMPE();
			break;
		case TYPE_INFO_POINTER:
			DUMP("(pointer");
			DUMPTI(type_info->pointer);
			DUMPE();
			break;
		case TYPE_INFO_INC_ARRAY:
			DUMP("(incarray");
			DUMPTI(type_info->array.base);
			DUMPE();
			break;
		case TYPE_INFO_EXPRESSION:
			DUMP("(typexpr");
			DUMPEXPR(type_info->unresolved_type_expr);
			DUMPE();
			break;
	}
	indent--;
	DUMPEND();
}

void fprint_expr_common(Context *context, FILE *file, Expr *expr, int indent)
{
	switch (expr->resolve_status)
	{
		case RESOLVE_NOT_DONE:
			DUMP("(unresolved)");
			return;
		case RESOLVE_RUNNING:
			DUMP("(resolving)");
			return;
		case RESOLVE_DONE:
			DUMPTYPE(expr->type);
			return;
	}
	UNREACHABLE
}

#define DUMPEXPC(_expr) fprint_expr_common(context, file, _expr, indent + 1)

void fprint_expr_recursive(Context *context, FILE *file, Expr *expr, int indent)
{
	if (!expr) return;
	switch (expr->expr_kind)
	{
		case EXPR_MEMBER_ACCESS:
			DUMP("(member access)");
			return;
		case EXPR_UNDEF:
			DUMP("(undef)");
			return;
		case EXPR_ENUM_CONSTANT:
			DUMP("(enumconstant)");
			return;
		case EXPR_TYPEINFO:
			TODO;
		case EXPR_SLICE_ASSIGN:
			DUMP("(sliceassign");
			DUMPEXPC(expr);
			DUMPEXPR(expr->slice_assign_expr.left);
			DUMPEXPR(expr->slice_assign_expr.right);
			DUMPEND();
		case EXPR_LEN:
			DUMP("(len");
			DUMPEXPC(expr);
			DUMPEXPR(expr->len_expr.inner);
			DUMPEND();
		case EXPR_DECL_LIST:
			DUMP("(decllist");
			DUMPASTS(expr->dexpr_list_expr);
			DUMPEND();
		case EXPR_FAILABLE:
			DUMP("(failable");
			DUMPEXPC(expr);
			DUMPEXPR(expr->failable_expr);
			DUMPEND();
		case EXPR_MACRO_IDENTIFIER:
			DUMPF("(ident @%s", expr->macro_identifier_expr.identifier);
			DUMPEXPC(expr);
			DUMPEND();
		case EXPR_IDENTIFIER:
			DUMPF("(ident %s", expr->identifier_expr.identifier);
			DUMPEXPC(expr);
			DUMPEND();
		case EXPR_CT_IDENT:
			DUMPF("(ctident %s", expr->ct_ident_expr.identifier);
			DUMPEXPC(expr);
			DUMPEND();
		case EXPR_HASH_IDENT:
			DUMPF("(hashident %s", expr->hash_ident_expr.identifier);
			DUMPEXPC(expr);
			DUMPEND();
		case EXPR_MACRO_CT_IDENTIFIER:
			DUMPF("(macroctident @%s", expr->ct_ident_expr.identifier);
			DUMPEXPC(expr);
			DUMPEND();
		case EXPR_CONST_IDENTIFIER:
			DUMPF("(ident %s", expr->identifier_expr.identifier);
			DUMPEXPC(expr);
			DUMPEND();
		case EXPR_MACRO_BLOCK:
			DUMP("(macro_block");
			DUMPASTS(expr->macro_block.stmts);
			DUMPDECLS(expr->macro_block.params);
			DUMPEND();
		case EXPR_EXPR_BLOCK:
			if (!expr->expr_block.stmts)
			{
				DUMP("(expr_block)");
				return;
			}
			DUMP("(expr_block");
			DUMPASTS(expr->expr_block.stmts);
			DUMPEND();
		case EXPR_CONST:
			fprintf_indented(file, indent, "(const ");
			expr_const_fprint(file, &expr->const_expr);
			fprintf(file, "\n");
			DUMPEXPC(expr);
			DUMPEND();
		case EXPR_BINARY:
			DUMPF("(binary %s", token_type_to_string(binaryop_to_token(expr->binary_expr.operator)));
			DUMPEXPC(expr);
			DUMPEXPR(expr->binary_expr.left);
			DUMPEXPR(expr->binary_expr.right);
			DUMPEND();
		case EXPR_UNARY:
			DUMPF("(unary %s", token_type_to_string(unaryop_to_token(expr->unary_expr.operator)));
			DUMPEXPC(expr);
			DUMPEXPR(expr->unary_expr.expr);
			DUMPEND();
		case EXPR_POST_UNARY:
			DUMPF("(postunary %s", token_type_to_string(postunaryop_to_token(expr->post_expr.operator)));
			DUMPEXPC(expr);
			DUMPEXPR(expr->post_expr.expr);
			DUMPEND();
		case EXPR_CATCH:
			DUMP("(catch");
			DUMPEXPC(expr);
			DUMPEXPR(expr->trycatch_expr);
			DUMPEND();
		case EXPR_TRY:
			DUMP("(try");
			DUMPEXPC(expr);
			DUMPEXPR(expr->trycatch_expr);
			DUMPEND();
		case EXPR_ACCESS:
			DUMPF("(access .%s", TOKSTR(expr->access_expr.sub_element));
			DUMPEXPC(expr);
			DUMPEXPR(expr->access_expr.parent);
			DUMPEND();
		case EXPR_TYPEID:
			DUMP("(typeid");
			DUMPEXPC(expr);
			DUMPTI(expr->typeid_expr);
			DUMPEND();
		case EXPR_GROUP:
			DUMP("(group");
			DUMPEXPR(expr->group_expr);
			DUMPEND();
		case EXPR_CALL:
			DUMP("(call");
			DUMPEXPC(expr);
			DUMPEXPR(expr->call_expr.function);
			indent++;
			DUMP("(args");
			VECEACH(expr->call_expr.arguments, i)
			{
				DUMPEXPR(expr->call_expr.arguments[i]);
			}
			DUMPE();
			indent--;
			DUMPEND();
		case EXPR_TERNARY:
			if (!expr->ternary_expr.then_expr)
			{
				DUMP("(elvis");
				DUMPEXPC(expr);
				DUMPEXPR(expr->ternary_expr.cond);
			}
			else
			{
				DUMP("(cond");
				DUMPEXPC(expr);
				DUMPEXPR(expr->ternary_expr.cond);
				DUMPEXPR(expr->ternary_expr.then_expr);
			}
			DUMPEXPR(expr->ternary_expr.else_expr);
			DUMPEND();
		case EXPR_INITIALIZER_LIST:
			fprintf_indented(file, indent, "(initializerlist ");
			switch (expr->expr_initializer.init_type)
			{
				case INITIALIZER_UNKNOWN:
					fprintf(file, "not-analyzed\n");
					break;
				case INITIALIZER_ZERO:
					fprintf(file, "zero\n");
					break;
				case INITIALIZER_NORMAL:
					fprintf(file, "normal\n");
					break;
				case INITIALIZER_DESIGNATED:
					fprintf(file, "designated\n");
					break;
			}
			DUMPEXPC(expr);
			VECEACH(expr->expr_initializer.initializer_expr, i)
			{
				DUMPEXPR(expr->expr_initializer.initializer_expr[i]);
			}
			DUMPEND();
		case EXPR_SUBSCRIPT:
			DUMP("(subscript");
			DUMPEXPC(expr);
			DUMPEXPR(expr->subscript_expr.expr);
			DUMPEXPC(expr->subscript_expr.index);
			DUMPEND();
		case EXPR_SLICE:
			DUMP("(slice");
			DUMPEXPC(expr);
			DUMPEXPR(expr->slice_expr.expr);
			DUMPEXPR(expr->slice_expr.start);
			DUMPEXPR(expr->slice_expr.end);
			DUMPEND();
		case EXPR_GUARD:
			DUMP("(guard");
			DUMPEXPR(expr->guard_expr.inner);
			DUMPEND();
		case EXPR_ELSE:
			if (expr->else_expr.is_jump)
			{
				DUMP("(else-jump");
				DUMPEXPC(expr);
				DUMPEXPR(expr->else_expr.expr);
				DUMPAST(expr->else_expr.else_stmt);
				DUMPEND();
			}
			else
			{
				DUMP("(else-expr");
				DUMPEXPC(expr);
				DUMPEXPR(expr->else_expr.expr);
				DUMPEXPR(expr->else_expr.else_expr);
				DUMPEND();
			}
			UNREACHABLE
		case EXPR_CAST:
			DUMP("(cast\n");
			DUMPEXPC(expr);
			DUMPTI(expr->cast_expr.type_info);
			DUMPEXPR(expr->cast_expr.expr);
			DUMPEND();
		case EXPR_EXPRESSION_LIST:
			DUMP("(expression-list");
			DUMPEXPC(expr);
			DUMPTI(expr->struct_value_expr.type);
			VECEACH(expr->expression_list, i)
			{
				fprint_expr_recursive(context, file, expr->expression_list[i], indent + 1);
			}
			DUMPEND();
		case EXPR_POISONED:
			DUMP("(POISONED)");
			return;
		case EXPR_TYPEOF:
			DUMP("(typeof");
			DUMPEXPR(expr->typeof_expr);
			DUMPEND();
		case EXPR_SCOPED_EXPR:
			DUMP("(scopedexpr");
			DUMPEXPR(expr->expr_scope.expr);
			// TODO defers.
			DUMPEND();
		case EXPR_DESIGNATED_INITIALIZER:
			DUMP("(designated-initializer");
			// TODO path
			DUMPEXPR(expr->designated_init_expr.value);
			DUMPEND();
		case EXPR_COMPOUND_LITERAL:
			DUMP("(compound-literal");
			DUMPTI(expr->expr_compound_literal.type_info);
			DUMPEXPR(expr->expr_compound_literal.initializer);
			DUMPEND();
	}
	UNREACHABLE
}


void fprint_func_signature(Context *context, FILE *file, FunctionSignature *signature, int indent)
{
	DUMP("(func-sig");
	DUMPTI(signature->rtype);
	do
	{
		if (!vec_size(signature->params))
		{
			DUMPI("(params none)");
			break;
		}
		indent++;
		DUMP("(params");
		DUMPDECLS(signature->params);
		DUMPE();
		indent--;
	} while (false);
	DUMPEND();
}

void fprint_decl_recursive(Context *context, FILE *file, Decl *decl, int indent)
{
	if (!decl) return;
	switch (decl->decl_kind)
	{
		case DECL_VAR:
			DUMPF("(var-%s %s", decl_var_to_string(decl->var.kind), decl->name ? decl->name : "anon");
			DUMPTI(decl->var.type_info);
			switch (decl->var.kind)
			{
				case VARDECL_CONST:
				case VARDECL_GLOBAL:
				case VARDECL_LOCAL:
				case VARDECL_PARAM:
				case VARDECL_PARAM_CT:
				case VARDECL_PARAM_EXPR:
				case VARDECL_PARAM_REF:
				case VARDECL_MEMBER:
				case VARDECL_LOCAL_CT:
					DUMPEXPR(decl->var.init_expr);
					break;
				case VARDECL_PARAM_CT_TYPE:
				case VARDECL_LOCAL_CT_TYPE:
					DUMPTI(decl->var.type_info);
					break;
				case VARDECL_ALIAS:
					DUMPDECL(decl->var.alias);
					break;
			}
			DUMPEND();
		case DECL_DEFINE:
			DUMPF("(define %s", decl->name);
			DUMPEND();
		case DECL_LABEL:
			DUMPF("(label %s", decl->name);
			DUMPEND();
		case DECL_MACRO:
			DUMPF("(macro %s", decl->name);
			DUMPTI(decl->macro_decl.rtype);
			indent++;
			DUMP("(params");
			DUMPDECLS(decl->macro_decl.parameters);
			DUMPE();
			indent--;
			DUMPAST(decl->macro_decl.body);
			DUMPEND();
		case DECL_FUNC:
			DUMPF("(func %s", decl->name);
			if (decl->func.type_parent)
			{
				indent++;
				DUMP("(parent_type");
				DUMPTI(decl->func.type_parent);
				DUMPE();
				indent--;
			}
			fprint_func_signature(context, file, &decl->func.function_signature, indent + 1);
			if (decl->func.body) DUMPAST(decl->func.body);
			DUMPEND();
		case DECL_STRUCT:
			DUMPF("(struct %s", decl->name ? decl->name : "anon");
			DUMPDECLS(decl->strukt.members);
			DUMPEND();
		case DECL_UNION:
			DUMPF("(union %s", decl->name ? decl->name : "anon");
			DUMPDECLS(decl->strukt.members);
			DUMPEND();
		case DECL_ENUM:
			DUMPF("(enum %s", decl->name);
			DUMPTI(decl->enums.type_info);
			DUMPDECLS(decl->enums.values);
			DUMPEND();
		case DECL_ERR:
			DUMPF("(error %s", decl->name);
			DUMPDECLS(decl->strukt.members);
			DUMPEND();
		case DECL_ENUM_CONSTANT:
			if (!decl->enum_constant.expr)
			{
				DUMPF("(enum-constant %s)", decl->name);
				return;
			}
			DUMPF("(enum-constant %s", decl->name);
			DUMPEXPR(decl->enum_constant.expr);
			DUMPEND();
		case DECL_GENERIC:
			DUMPF("(generic %s\n", decl->name);
			indent++;
			DUMP("(params");
			VECEACH(decl->generic_decl.parameters, i)
			{
				DUMPFI("%s", TOKSTR(decl->generic_decl.parameters[i]));
			}
			DUMPE();
			DUMP("(cases");
			DUMPASTS(decl->generic_decl.cases);
			DUMPE();
			indent--;
			DUMPEND();
		case DECL_TYPEDEF:
			DUMPF("(typedef %s", decl->name);
			if (decl->typedef_decl.is_func)
			{
				fprint_func_signature(context, file, &decl->typedef_decl.function_signature, indent + 1);
			}
			else
			{
				DUMPTI(decl->typedef_decl.type_info);
			}
			DUMPEND();
		case DECL_CT_IF:
			DUMP("(ct-if");
			DUMPEXPR(decl->ct_if_decl.expr);
			DUMPDECLS(decl->ct_if_decl.then);
			if (decl->ct_if_decl.elif)
			{
				DUMPDECL(decl->ct_if_decl.elif);
			}
			DUMPEND();
		case DECL_CT_ELIF:
			DUMP("(ct-elif");
			DUMPEXPR(decl->ct_elif_decl.expr);
			DUMPDECLS(decl->ct_elif_decl.then);
			DUMPDECL(decl->ct_elif_decl.elif);
			DUMPEND();
		case DECL_CT_ELSE:
			DUMP("(ct-else");
			DUMPDECLS(decl->ct_else_decl);
			DUMPEND();
		case DECL_POISONED:
			DUMP("(poisoned-decl)");
			return;
		case DECL_ARRAY_VALUE:
			DUMP("(array value");
			DUMPEXPR(decl->incr_array_decl);
			DUMPEND();
		case DECL_IMPORT:
			DUMPF("(import %s", decl->name);
			// TODO
			DUMPEND();
		case DECL_CT_CASE:
			if (decl->ct_case_decl.type)
			{
				DUMP("($case");
				DUMPTI(decl->ct_case_decl.type);
			}
			else
			{
				DUMP("($default");
			}
			DUMPDECLS(decl->ct_case_decl.body);
			DUMPEND();
		case DECL_CT_SWITCH:
			DUMP("($switch");
			DUMPEXPR(decl->ct_switch_decl.expr);
			DUMPDECLS(decl->ct_switch_decl.cases);
			DUMPEND();
		case DECL_ATTRIBUTE:
			DUMPF("(attribute %s)", decl->name);
			if (decl->attr.domains & ATTR_FUNC)
			{
				DUMPI("(func)");
			}
			if (decl->attr.domains & ATTR_VAR)
			{
				DUMPI("(var)");
			}
			if (decl->attr.domains & ATTR_ENUM)
			{
				DUMPI("(enum)");
			}
			if (decl->attr.domains & ATTR_STRUCT)
			{
				DUMPI("(struct)");
			}
			if (decl->attr.domains & ATTR_UNION)
			{
				DUMPI("(union)");
			}
			if (decl->attr.domains & ATTR_CONST)
			{
				DUMPI("(const)");
			}
			if (decl->attr.domains & ATTR_ERROR)
			{
				DUMPI("(error)");
			}
			if (decl->attr.domains & ATTR_TYPEDEF)
			{
				DUMPI("(typedef)");
			}
			// TODO attribute
			DUMPEND();
	}
	UNREACHABLE
}

static void fprint_decl_list(Context *context, FILE *file, Decl **decls, int indent)
{
	VECEACH(decls, i)
	{
		fprint_decl_recursive(context, file, decls[i], indent);
	}
}

static void fprint_asts_recursive(Context *context, FILE *file, Ast **asts, int indent)
{
	VECEACH(asts, i)
	{
		fprint_ast_recursive(context, file, asts[i], indent);
	}
}

static void fprint_ast_recursive(Context *context, FILE *file, Ast *ast, int indent)
{
	if (!ast) return;
	switch (ast->ast_kind)
	{
		case AST_CT_ASSERT:
			DUMP("($assert");
			DUMPEXPR(ast->ct_assert_stmt.expr);
			DUMPEXPR(ast->ct_assert_stmt.message);
			DUMPEND();
		case AST_ASSERT_STMT:
			DUMP("(assert");
			DUMPEXPR(ast->assert_stmt.expr);
			DUMPEXPR(ast->assert_stmt.message);
			DUMPEND();
		case AST_TRY_STMT:
			DUMP("(try");
			DUMPEXPR(ast->try_stmt.decl_expr);
			DUMPAST(ast->try_stmt.body);
			DUMPEND();
		case AST_COMPOUND_STMT:
			if (!ast->compound_stmt.stmts)
			{
				DUMP("(compound)");
				return;
			}
			DUMP("(compound\n");
			fprint_asts_recursive(context, file, ast->compound_stmt.stmts, indent + 1);
			DUMPEND();
		case AST_CT_COMPOUND_STMT:
			if (!ast->compound_stmt.stmts)
			{
				DUMP("(ct-compound)");
				return;
			}
			DUMP("(ct-compound\n");
			fprint_asts_recursive(context, file, ast->ct_compound_stmt, indent + 1);
			DUMPEND();
		case AST_DEFINE_STMT:
			DUMP("(define");
			DUMPDECL(ast->define_stmt);
			DUMPEND();
		case AST_DECLARE_STMT:
			DUMP("(declare");
			DUMPDECL(ast->declare_stmt);
			DUMPEND();
		case AST_EXPR_STMT:
			DUMP("expr");
			DUMPEXPR(ast->expr_stmt);
			DUMPEND();
		case AST_WHILE_STMT:
			DUMP("(while");
			DUMPEXPR(ast->while_stmt.cond);
			DUMPAST(ast->while_stmt.body);
			DUMPEND();
		case AST_SCOPED_STMT:
			DUMP("(scoped");
			DUMPAST(ast->scoped_stmt.stmt);
			DUMPEND();
		case AST_CT_FOR_STMT:
			if (ast->ct_for_stmt.index.index)
			{
				DUMPF("($for %s, %s\n", TOKSTR(ast->ct_for_stmt.index), TOKSTR(ast->ct_for_stmt.value));
			}
			else
			{
				DUMPF("($for  %s\n", TOKSTR(ast->ct_for_stmt.value));
			}
			DUMPEXPR(ast->ct_for_stmt.expr);
			DUMPAST(ast->ct_for_stmt.body);
			DUMPEND();
		case AST_DO_STMT:
			DUMP("(do");
			DUMPAST(ast->do_stmt.body);
			DUMPEXPR(ast->do_stmt.expr);
			DUMPEND();
		case AST_RETURN_STMT:
			if (ast->return_stmt.expr)
			{
				DUMP("(return");
				DUMPEXPR(ast->expr_stmt);
				DUMPEND();
			}
			DUMP("(return)");
			return;
		case AST_BREAK_STMT:
			DUMP("(break)");
			return;
		case AST_NEXT_STMT:
			DUMP("(next)");
			return;
		case AST_CONTINUE_STMT:
			DUMP("(continue)");
			return;
		case AST_DEFAULT_STMT:
			DUMP("(default)");
			return;
		case AST_FOR_STMT:
			DUMP("(for");
			DUMPEXPR(ast->for_stmt.init);
			DUMPEXPR(ast->for_stmt.cond);
			if (ast->for_stmt.incr)
			{
				DUMPEXPR(ast->for_stmt.incr);
			}
			else
			{
				DUMPI("(noincr)");
			}
			DUMPAST(ast->for_stmt.body);
			DUMPEND();
		case AST_IF_STMT:
			DUMP("(if");
			DUMPEXPR(ast->if_stmt.cond);
			DUMPAST(ast->if_stmt.then_body);
			DUMPAST(ast->if_stmt.else_body);
			DUMPEND();
	    case AST_SWITCH_STMT:
	    	DUMP("(switch");
	    	DUMPEXPR(ast->switch_stmt.cond);
	    	DUMPASTS(ast->switch_stmt.cases);
            DUMPEND();
		case AST_CASE_STMT:
			DUMP("(case");
			if (ast->case_stmt.is_type)
			{
				DUMPTI(ast->case_stmt.type_info);
			}
			else
			{
				DUMPEXPR(ast->case_stmt.expr);
			}
			DUMPAST(ast->case_stmt.body);
			DUMPEND();
	    case AST_DEFER_STMT:
	    	DUMP("(defer");
	    	DUMPAST(ast->defer_stmt.body);
	    	DUMPEND();
		case AST_POISONED:
			DUMP("(ast-poisoned)");
			return;
		case AST_ASM_STMT:
			DUMP("(asm");
			// TODO
			DUMPEND();
		case AST_CATCH_STMT:
			DUMP("(catch");
			DUMPEXPR(ast->catch_stmt.catchable);
			DUMPAST(ast->catch_stmt.body);
			DUMPEND();
		case AST_CT_IF_STMT:
			DUMP("(ct-if");
			DUMPEXPR(ast->ct_if_stmt.expr);
			DUMPAST(ast->ct_if_stmt.then);
			DUMPAST(ast->ct_if_stmt.elif);
			DUMPEND();
		case AST_CT_ELIF_STMT:
			DUMP("(ct-elif");
			DUMPEXPR(ast->ct_elif_stmt.expr);
			DUMPAST(ast->ct_elif_stmt.then);
			DUMPAST(ast->ct_elif_stmt.elif);
			DUMPEND();
		case AST_CT_ELSE_STMT:
			DUMP("(ct-else");
			DUMPAST(ast->ct_else_stmt);
			DUMPEND();
		case AST_CT_SWITCH_STMT:
			DUMP("(ct-switch");
			DUMPEXPR(ast->ct_switch_stmt.cond);
			DUMPASTS(ast->ct_switch_stmt.body);
			DUMPEND();
		case AST_NOP_STMT:
			DUMP("(nop)");
			return;
		case AST_UNREACHABLE_STMT:
			DUMP("(unreachable)");
			return;
		case AST_VOLATILE_STMT:
			DUMP("(volatile");
			DUMPAST(ast->volatile_stmt);
			DUMPEND();
	}
	UNREACHABLE
}
void fprint_decl(Context *context, FILE *file, Decl *dec)
{
	fprint_decl_recursive(context, file, dec, 0);
}

