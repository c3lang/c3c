// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"

static Type poison_type = { .type_kind = TYPE_POISONED };
static TypeInfo poison_type_info = { .kind = TYPE_INFO_POISON };
static Decl poison_decl = { .decl_kind = DECL_POISONED, .resolve_status = RESOLVE_DONE };
static Expr poison_expr = { .expr_kind = EXPR_POISONED, .resolve_status = RESOLVE_DONE };
static Ast poison_ast = { .ast_kind = AST_POISONED };

Type *poisoned_type = &poison_type;
TypeInfo *poisoned_type_info = &poison_type_info;
Decl *poisoned_decl = &poison_decl;
Expr *poisoned_expr = &poison_expr;
Ast *poisoned_ast = &poison_ast;


Decl *decl_new_ct(DeclKind kind, SourceSpan span)
{
	Decl *decl = decl_calloc();
	decl->decl_kind = kind;
	decl->span = span;
	return decl;
}

Decl *decl_new(DeclKind decl_kind, const char *name, SourceSpan span, Visibility visibility)
{
	Decl *decl = decl_calloc();
	decl->decl_kind = decl_kind;
	decl->span = span;
	decl->name = name;
	decl->visibility = visibility;
	return decl;
}

Decl *decl_new_with_type(const char *name, SourceSpan loc, DeclKind decl_type, Visibility visibility)
{
	Decl *decl = decl_calloc();
	decl->decl_kind = decl_type;
	decl->name = name;
	decl->span = loc;
	decl->visibility = visibility;
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
		case DECL_FAULT:
			kind = TYPE_FAULTTYPE;
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
		case DECL_FAULTVALUE:
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
		case DECL_DECLARRAY:
		case DECL_BODYPARAM:
			UNREACHABLE
	}
	Type *type = type_new(kind, name ? name : "$anon");
	type->canonical = type;
	type->decl = decl;
	decl->type = type;
	return decl;
}

const char *decl_to_name(Decl *decl)
{
	const char *name = decl_to_a_name(decl);
	if (name[1] == 'n') return &name[3];
	return &name[2];
}

const char *decl_to_a_name(Decl *decl)
{
	switch (decl->decl_kind)
	{
		case DECL_BODYPARAM:
			return "a bodyparam";
		case DECL_DECLARRAY:
			return "a declarray";
		case DECL_BITSTRUCT:
			return "a bitstruct";
		case DECL_POISONED:
			return "a poisoned decl";
		case DECL_CT_ASSERT:
			return "a compile time assert";
		case DECL_CT_CASE:
			return "a compile time case";
		case DECL_CT_ELIF:
			return "a compile time else if";
		case DECL_CT_ELSE:
			return "a compile time else";
		case DECL_CT_IF:
			return "a compile time if";
		case DECL_CT_SWITCH:
			return "a compile time switch";
		case DECL_IMPORT:
			return "an import";
		case DECL_LABEL:
			return "a label";
		case DECL_ATTRIBUTE:
			return "an attribute";
		case DECL_DEFINE:
		case DECL_TYPEDEF:
			return "a define";
		case DECL_DISTINCT:
			return "a distinct type";
		case DECL_ENUM:
			return "an enum";
		case DECL_ENUM_CONSTANT:
			return "an enum value";
		case DECL_FAULTVALUE:
			return "a fault value";
		case DECL_FAULT:
			return "a fault";
		case DECL_FUNC:
			return "a function";
		case DECL_GENERIC:
			return "a generic";
		case DECL_MACRO:
			return "a macro";
		case DECL_STRUCT:
			return "a struct";
		case DECL_UNION:
			return "a union";
		case DECL_VAR:
			switch (decl->var.kind)
			{
				case VARDECL_ERASE:
				case VARDECL_REWRAPPED:
					UNREACHABLE
				case VARDECL_CONST:
					return "a constant";
				case VARDECL_GLOBAL:
					return "a global variable";
				case VARDECL_LOCAL:
					return "a variable";
				case VARDECL_PARAM:
					return "a parameter";
				case VARDECL_MEMBER:
				case VARDECL_BITMEMBER:
					return "a member";
				case VARDECL_PARAM_CT:
					return "a compile time parameter";
				case VARDECL_PARAM_CT_TYPE:
					return "a compile time type parameter";
				case VARDECL_PARAM_REF:
					return "a ref parameter";
				case VARDECL_PARAM_EXPR:
					return "a expression parameter";
				case VARDECL_LOCAL_CT:
					return "a compile time variable";
				case VARDECL_LOCAL_CT_TYPE:
					return "a compile time type variable";
				case VARDECL_UNWRAPPED:
					return "an unwrapped variable";
			}
			UNREACHABLE
	}
	UNREACHABLE
}


// Set the external name of a declaration
void decl_set_external_name(Decl *decl)
{
	// Already has the extname set using an attribute?
	// if so we're done.
	if (decl->has_extname) return;

	const char *name = decl->name;
	if (!name) name = "$anon";

	// "extern" or the module has no prefix?
	if (decl->visibility == VISIBLE_EXTERN || decl->unit->module->no_extprefix)
	{
		assert(decl->name || decl->unit->module->no_extprefix);
		decl->extname = name;
		return;
	}

	// Otherwise, first put the module name into the scratch buffer
	scratch_buffer_clear();
	Module *module = decl->unit->module;
	const char *module_name = module->extname ? module->extname : module->name->module;
	char c;
	while ((c = *(module_name++)) != 0)
	{
		switch (c)
		{
			case ':':
				scratch_buffer_append_char('_');
				module_name++;
				break;
			default:
				scratch_buffer_append_char(c);
				break;
		}
	}
	// Concat with the name
	scratch_buffer_append("_");
	scratch_buffer_append(name);

	// Copy it to extname
	decl->extname = scratch_buffer_copy();
}

Decl *decl_new_var(const char *name, SourceSpan loc, TypeInfo *type, VarDeclKind kind, Visibility visibility)
{
	Decl *decl = decl_new(DECL_VAR, name, loc, visibility);
	decl->var.kind = kind;
	decl->var.type_info = type;
	return decl;
}

Decl *decl_new_generated_var(Type *type, VarDeclKind kind, SourceSpan span)
{
	Decl *decl = decl_calloc();
	decl->decl_kind = DECL_VAR;
	decl->span = span;
	decl->name = NULL;
	decl->visibility = VISIBLE_LOCAL;
	decl->var.kind = kind;
	decl->type = type;
	decl->alignment = type ? type_alloca_alignment(type) : 0;
	decl->var.type_info = type_info_new_base(type, span);
	decl->resolve_status = RESOLVE_DONE;
	return decl;
}

// Determine if the expression has side effects
// Note! This is not the same as it being const.
bool expr_is_pure(Expr *expr)
{
	if (!expr) return true;
	switch (expr->expr_kind)
	{
		case EXPR_BUILTIN:
			return false;
		case EXPR_BUILTIN_ACCESS:
			return exprid_is_pure(expr->builtin_access_expr.inner);
		case EXPR_VARIANT:
			return exprid_is_pure(expr->variant_expr.type_id) && exprid_is_pure(expr->variant_expr.ptr);
		case EXPR_POINTER_OFFSET:
			return exprid_is_pure(expr->pointer_offset_expr.ptr) && exprid_is_pure(expr->pointer_offset_expr.offset);
		case EXPR_COMPILER_CONST:
		case EXPR_CONST:
		case EXPR_IDENTIFIER:
		case EXPR_NOP:
		case EXPR_STRINGIFY:
		case EXPR_RETVAL:
		case EXPR_TYPEINFO:
		case EXPR_CT_EVAL:
		case EXPR_CT_IDENT:
		case EXPR_CT_CALL:
		case EXPR_TYPEID:
		case EXPR_CT_ARG:
		case EXPR_OPERATOR_CHARS:
		case EXPR_CT_CHECKS:
			return true;
		case EXPR_VASPLAT:
			return true;
		case EXPR_ARGV_TO_SUBARRAY:
		case EXPR_BITASSIGN:
			return false;
		case EXPR_VARIANTSWITCH:
			return false;
		case EXPR_BINARY:
			// Anything with assignment is impure, otherwise true if sub expr are pure.
			if (expr->binary_expr.operator >= BINARYOP_ASSIGN) return false;
			return exprid_is_pure(expr->binary_expr.right) && exprid_is_pure(expr->binary_expr.left);
		case EXPR_UNARY:
			switch (expr->unary_expr.operator)
			{
				case UNARYOP_INC:
				case UNARYOP_DEC:
				case UNARYOP_TADDR:
					// ++ -- &&1
					return false;
				case UNARYOP_ERROR:
				case UNARYOP_DEREF:
				case UNARYOP_ADDR:
				case UNARYOP_NEG:
				case UNARYOP_BITNEG:
				case UNARYOP_NOT:
					return expr_is_pure(expr->unary_expr.expr);
			}
			UNREACHABLE
		case EXPR_BITACCESS:
		case EXPR_ACCESS:
			// All access is pure if the parent is pure.
			return expr_is_pure(expr->access_expr.parent);
		case EXPR_POISONED:
			UNREACHABLE
		case EXPR_MACRO_BODY_EXPANSION:
		case EXPR_CALL:
		case EXPR_CATCH_UNWRAP:
		case EXPR_COMPOUND_LITERAL:
		case EXPR_COND:
		case EXPR_DESIGNATOR:
		case EXPR_DECL:
		case EXPR_EXPR_BLOCK:
		case EXPR_FAILABLE:
		case EXPR_RETHROW:
		case EXPR_HASH_IDENT:
		case EXPR_MACRO_BLOCK:
		case EXPR_FLATPATH:
		case EXPR_INITIALIZER_LIST:
		case EXPR_DESIGNATED_INITIALIZER_LIST:
		case EXPR_POST_UNARY:
		case EXPR_SLICE_ASSIGN:
		case EXPR_SLICE_COPY:
		case EXPR_TRY_UNWRAP:
		case EXPR_TRY_UNWRAP_CHAIN:
		case EXPR_FORCE_UNWRAP:
			return false;
		case EXPR_CAST:
			return exprid_is_pure(expr->cast_expr.expr);
		case EXPR_EXPRESSION_LIST:
			VECEACH(expr->expression_list, i)
			{
				if (!expr_is_pure(expr->expression_list[i])) return false;
			}
			return true;
		case EXPR_TYPEID_INFO:
			return exprid_is_pure(expr->typeid_info_expr.parent);
		case EXPR_SLICE:
			return exprid_is_pure(expr->subscript_expr.expr)
			       && exprid_is_pure(expr->subscript_expr.range.start)
			       && exprid_is_pure(expr->subscript_expr.range.end);
		case EXPR_SUBSCRIPT:
		case EXPR_SUBSCRIPT_ADDR:
			return exprid_is_pure(expr->subscript_expr.expr)
			       && exprid_is_pure(expr->subscript_expr.range.start);
		case EXPR_TERNARY:
			return exprid_is_pure(expr->ternary_expr.cond)
			       && exprid_is_pure(expr->ternary_expr.else_expr)
			       && exprid_is_pure(expr->ternary_expr.then_expr);
		case EXPR_ASM:
			return false;
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
		case EXPR_TERNARY:
			return expr_is_simple(exprptr(expr->ternary_expr.else_expr)) && expr_is_simple(exprptr(expr->ternary_expr.then_expr));
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
	return expr;
}

Expr *expr_new_const_int(SourceSpan span, Type *type, uint64_t v, bool narrowable)
{
	Expr *expr = expr_calloc();
	expr->expr_kind = EXPR_CONST;
	expr->span = span;
	expr->type = type;
	TypeKind kind = type_flatten(type)->type_kind;
	expr->const_expr.ixx.i.high = 0;
	if (type_kind_is_signed(kind))
	{
		if (v > (uint64_t)INT64_MAX) expr->const_expr.ixx.i.high = UINT64_MAX;
	}
	expr->const_expr.ixx.i.low = v;
	expr->const_expr.ixx.type = kind;
	expr->const_expr.const_kind = CONST_INTEGER;
	expr->const_expr.narrowable = narrowable;
	expr->resolve_status = RESOLVE_DONE;
	return expr;
}

Expr *expr_new_const_bool(SourceSpan span, Type *type, bool value)
{
	Expr *expr = expr_calloc();
	expr->expr_kind = EXPR_CONST;
	expr->span = span;
	expr->type = type;
	assert(type_flatten(type)->type_kind == TYPE_BOOL);
	expr->const_expr.b = value;
	expr->const_expr.const_kind = CONST_BOOL;
	expr->resolve_status = RESOLVE_DONE;
	return expr;
}


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
		[TOKEN_QUESTQUEST] = BINARYOP_ELSE,
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

bool ast_is_not_empty(Ast *ast)
{
	if (!ast) return false;
	if (ast->ast_kind != AST_COMPOUND_STMT) return true;
	AstId first = ast->compound_stmt.first_stmt;
	if (first)
	{
		Ast *stmt = astptr(first);
		if (stmt->next) return true;
		return ast_is_not_empty(stmt);
	}
	return false;
}

AttributeType attribute_by_name(const char *name)
{
	for (unsigned i = 0; i < NUMBER_OF_ATTRIBUTES; i++)
	{
		if (attribute_list[i] == name) return (AttributeType)i;
	}
	return ATTRIBUTE_NONE;
}

static inline ConstInitializer *initializer_for_index(ConstInitializer *initializer, uint32_t index)
{
	switch (initializer->kind)
	{
		case CONST_INIT_ZERO:
		case CONST_INIT_STRUCT:
		case CONST_INIT_UNION:
		case CONST_INIT_VALUE:
			return initializer;
		case CONST_INIT_ARRAY_FULL:
			return initializer->init_array_full[index];
		case CONST_INIT_ARRAY:
		{
			ConstInitializer **sub_values = initializer->init_array.elements;
			VECEACH(sub_values, i)
			{
				ConstInitializer *init = sub_values[i];
				assert(init->kind == CONST_INIT_ARRAY_VALUE);
				if (init->init_array_value.index == index) return init->init_array_value.element;
			}
			return NULL;
		}
		case CONST_INIT_ARRAY_VALUE:
			UNREACHABLE
	}
	UNREACHABLE
}

void expr_rewrite_to_const_zero(Expr *expr, Type *type)
{
	expr->expr_kind = EXPR_CONST;
	expr->const_expr.narrowable = true;
	switch (type->canonical->type_kind)
	{
		case TYPE_POISONED:
		case TYPE_VOID:
		case TYPE_INFERRED_VECTOR:
			UNREACHABLE
		case ALL_INTS:
			expr_rewrite_const_int(expr, type, 0, true);
			return;
		case ALL_FLOATS:
			expr_rewrite_const_float(expr, type, 0);
			break;
		case TYPE_BOOL:
			expr_rewrite_const_bool(expr, type, false);
			return;
		case TYPE_POINTER:
		case TYPE_FAULTTYPE:
		case TYPE_ANY:
		case TYPE_ANYERR:
		case TYPE_TYPEID:
			expr_rewrite_const_null(expr, type);
			return;
		case TYPE_ENUM:
			expr->const_expr.const_kind = CONST_ENUM;
			expr->const_expr.enum_val = type->decl->enums.values[0];
			break;
		case TYPE_FUNC:
		case TYPE_TYPEDEF:
		case TYPE_FAILABLE_ANY:
		case TYPE_FAILABLE:
		case TYPE_TYPEINFO:
			UNREACHABLE
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_BITSTRUCT:
		case TYPE_ARRAY:
		case TYPE_SUBARRAY:
		case TYPE_INFERRED_ARRAY:
		case TYPE_FLEXIBLE_ARRAY:
		case TYPE_UNTYPED_LIST:
		case TYPE_SCALED_VECTOR:
		case TYPE_VECTOR:
		{
			ConstInitializer *init = CALLOCS(ConstInitializer);
			init->kind = CONST_INIT_ZERO;
			init->type = type;
			expr_rewrite_const_list(expr, type, init);
			return;
		}
		case TYPE_DISTINCT:
			expr_rewrite_to_const_zero(expr, type->decl->distinct_decl.base_type);
			break;
	}
	expr->type = type;
}

bool expr_rewrite_to_const_initializer_index(Type *list_type, ConstInitializer *list, Expr *result, unsigned index)
{
	ConstInitializer *initializer = initializer_for_index(list, index);
	ConstInitType kind = initializer ? initializer->kind : CONST_INIT_ZERO;
	switch (kind)
	{
		case CONST_INIT_ZERO:
			expr_rewrite_to_const_zero(result, type_get_indexed_type(list_type));
			return true;
		case CONST_INIT_STRUCT:
		case CONST_INIT_UNION:
		case CONST_INIT_ARRAY:
		case CONST_INIT_ARRAY_FULL:
		case CONST_INIT_ARRAY_VALUE:
			return false;
		case CONST_INIT_VALUE:
			expr_replace(result, initializer->init_value);
			return true;
	}
	UNREACHABLE
}
