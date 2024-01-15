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

// Standard decl creation, used by compile time constructs, since they have no need for neither type nor name.
Decl *decl_new_ct(DeclKind kind, SourceSpan span)
{
	Decl *decl = decl_calloc();
	decl->decl_kind = kind;
	decl->span = span;
	return decl;
}

// Named declaration without type.
Decl *decl_new(DeclKind decl_kind, const char *name, SourceSpan span)
{
	Decl *decl = decl_calloc();
	decl->decl_kind = decl_kind;
	decl->span = span;
	decl->name = name;
	return decl;
}


// Check if local or parameter $foo/$Foo
bool decl_is_ct_var(Decl *decl)
{
	if (decl->decl_kind != DECL_VAR) return false;
	return decl_var_kind_is_ct(decl->var.kind);
}

Decl *decl_new_with_type(const char *name, SourceSpan loc, DeclKind decl_type)
{
	Decl *decl = decl_calloc();
	decl->decl_kind = decl_type;
	decl->name = name;
	decl->span = loc;
	TypeKind kind = TYPE_POISONED;
	switch (decl_type)
	{
		case DECL_FNTYPE:
		case DECL_FUNC:
			UNREACHABLE
		case DECL_INTERFACE:
			kind = TYPE_INTERFACE;
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
		case NON_TYPE_DECLS:
		case DECL_ERASED:
			UNREACHABLE
	}
	Type *type = type_new(kind, name ? name : "$anon");
	type->canonical = type;
	type->decl = decl;
	decl->type = type;
	return decl;
}

const char *decl_safe_name(Decl *decl)
{
	if (!decl) return "<no decl>";
	if (decl->name) return decl->name;
	return decl_to_name(decl);
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
		case DECL_ATTRIBUTE: return "an attribute";
		case DECL_BITSTRUCT: return "a bitstruct";
		case DECL_BODYPARAM: return "a body parameter";
		case DECL_CT_ASSERT: return "a compile time assert";
		case DECL_CT_ECHO: return "a compile time echo";
		case DECL_CT_EXEC: return "compile time exec include";
		case DECL_CT_INCLUDE: return "an include";
		case DECL_DECLARRAY: return "a declarray";
		case DECL_DEFINE: case DECL_TYPEDEF: return "a define";
		case DECL_DISTINCT: return "a distinct type";
		case DECL_ENUM: return "an enum";
		case DECL_ENUM_CONSTANT: return "an enum value";
		case DECL_ERASED: return "an erased declaration";
		case DECL_FAULT: return "a fault";
		case DECL_FAULTVALUE: return "a fault value";
		case DECL_FNTYPE: return "a function type";
		case DECL_FUNC: return "a function";
		case DECL_GLOBALS: return "globals";
		case DECL_IMPORT: return "an import";
		case DECL_LABEL: return "a label";
		case DECL_MACRO: return "a macro";
		case DECL_POISONED: return "a poisoned decl";
		case DECL_INTERFACE: return "an interface";
		case DECL_STRUCT: return "a struct";
		case DECL_UNION: return "a union";
		case DECL_VAR:
			switch (decl->var.kind)
			{
				case VARDECL_BITMEMBER: return "a bitstruct member";
				case VARDECL_CONST: return "a constant";
				case VARDECL_ERASE: UNREACHABLE
				case VARDECL_GLOBAL: return "a global variable";
				case VARDECL_LOCAL: return "a variable";
				case VARDECL_LOCAL_CT: return "a compile time variable";
				case VARDECL_LOCAL_CT_TYPE: return "a compile time type variable";
				case VARDECL_MEMBER: return "a member";
				case VARDECL_PARAM: return "a parameter";
				case VARDECL_PARAM_CT: return "a compile time parameter";
				case VARDECL_PARAM_CT_TYPE: return "a compile time type parameter";
				case VARDECL_PARAM_EXPR: return "a expression parameter";
				case VARDECL_PARAM_REF: return "a ref parameter";
				case VARDECL_REWRAPPED: UNREACHABLE
				case VARDECL_UNWRAPPED: return "an unwrapped variable";
			}
			UNREACHABLE
	}
	UNREACHABLE
}


// Set the external name of a declaration
void decl_set_external_name(Decl *decl)
{
	if (decl->decl_kind == DECL_ERASED) return;

	// Already has the extname set using an attribute?
	// if so we're done.
	if (decl->has_extname) return;

	const char *name = decl->name;
	if (!name) name = "$anon";

	// "extern" or the module has no prefix?
	if (decl->is_extern || decl_module(decl)->no_extprefix)
	{
		assert(decl->name || decl_module(decl)->no_extprefix);
		decl->extname = name;
		return;
	}

	// Otherwise, first put the module name into the scratch buffer
	scratch_buffer_clear();
	Module *module = decl_module(decl);
	const char *module_name = module->extname ? module->extname : module->name->module;
	char c;
	while ((c = *(module_name++)) != 0)
	{
		switch (c)
		{
			case ':':
				scratch_buffer_append_char(decl->is_export ? '_' : '.');
				module_name++;
				break;
			default:
				scratch_buffer_append_char(c);
				break;
		}
	}
	// Concat with the name
	scratch_buffer_append(decl->is_export ? "_" : ".");
	scratch_buffer_append(name);

	// Copy it to extname
	decl->extname = scratch_buffer_copy();
}

Decl *decl_new_var(const char *name, SourceSpan loc, TypeInfo *type, VarDeclKind kind)
{
	Decl *decl = decl_new(DECL_VAR, name, loc);
	decl->var.kind = kind;
	decl->var.type_info = type ? type_infoid(type) : 0;
	return decl;
}

Decl *decl_new_generated_var(Type *type, VarDeclKind kind, SourceSpan span)
{
	Decl *decl = decl_calloc();
	decl->decl_kind = DECL_VAR;
	decl->span = span;
	decl->name = NULL;
	decl->var.kind = kind;
	decl->type = type;
	decl->alignment = type ? type_alloca_alignment(type) : 0;
	assert(!type || !type_is_user_defined(type) || type->decl->resolve_status == RESOLVE_DONE);
	decl->var.type_info = type_info_id_new_base(type, span);
	decl->resolve_status = RESOLVE_DONE;
	return decl;
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
		[TOKEN_PLUS] = UNARYOP_PLUS,
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



int decl_count_elements(Decl *structlike)
{
	int elements = 0;
	Decl **members = structlike->strukt.members;
	unsigned member_size = vec_size(members);
	if (member_size == 0) return 0;
	if (structlike->decl_kind == DECL_UNION) member_size = 1;
	for (unsigned i = 0; i < member_size; i++)
	{
		Decl *member = members[i];
		if (member->decl_kind != DECL_VAR && !member->name)
		{
			elements += decl_count_elements(member);
			continue;
		}
		elements++;
	}
	return elements;
}

bool ast_is_compile_time(Ast *ast)
{
	switch (ast->ast_kind)
	{
		case AST_NOP_STMT:
			return true;
		case AST_RETURN_STMT:
		case AST_BLOCK_EXIT_STMT:
			if (!ast->return_stmt.expr) return true;
			return expr_is_constant_eval(ast->return_stmt.expr, CONSTANT_EVAL_CONSTANT_VALUE);
		case AST_EXPR_STMT:
			return expr_is_compile_time(ast->expr_stmt);
		case AST_COMPOUND_STMT:
		{
			AstId current = ast->compound_stmt.first_stmt;
			while (current)
			{
				if (!ast_is_compile_time(ast_next(&current))) return false;
			}
			return true;
		}
		default:
			return false;
	}
}

bool decl_is_externally_visible(Decl *decl)
{
	return decl->is_external_visible || decl->visibility == VISIBLE_PUBLIC || decl->is_export;
}

bool decl_is_local(Decl *decl)
{
	return !decl->is_external_visible && decl->visibility != VISIBLE_PUBLIC && !decl->is_export;
}

bool decl_needs_prefix(Decl *decl)
{
	switch (decl->decl_kind)
	{
		case DECL_VAR:
		case DECL_FUNC:
		case DECL_MACRO:
			return !decl->is_autoimport;
		default:
			return false;
	}
}

Decl *decl_find_enum_constant(Decl *decl, const char *name)
{
	VECEACH(decl->enums.values, i)
	{
		Decl *enum_constant = decl->enums.values[i];
		if (enum_constant->name == name)
		{
			return enum_constant;
		}
	}
	return NULL;
}

AlignSize decl_find_member_offset(Decl *decl, Decl *member)
{
	static const AlignSize NO_MATCH = ~(AlignSize)0;
	while (decl->decl_kind == DECL_DISTINCT) decl = decl->distinct->type->decl;
	Decl **members = NULL;
	switch (decl->decl_kind)
	{
		case DECL_BITSTRUCT:
			members = decl->bitstruct.members;
			break;
		case DECL_STRUCT:
		case DECL_UNION:
			members = decl->strukt.members;
			break;
		default:
			return NO_MATCH;
	}
	assert(members);
	unsigned list = vec_size(members);
	for (unsigned i = 0; i < list; i++)
	{
		Decl *m = members[i];
		if (m == member)
		{
			return member->offset;
		}
		if (m->decl_kind != DECL_VAR)
		{
			AlignSize possible_offset = decl_find_member_offset(m, member);
			if (possible_offset != NO_MATCH) return possible_offset + m->offset;
		}
	}
	return NO_MATCH;
}

bool ast_supports_continue(Ast *stmt)
{
	if (stmt->ast_kind != AST_FOR_STMT) return false;
	return stmt->for_stmt.cond || !stmt->flow.skip_first;
}
