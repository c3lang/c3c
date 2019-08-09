#pragma once

// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.


#include "compiler_common.h"
#include "symtab.h"
#include "value.h"

typedef enum _NumberType
{
	NUMBER_TYPE_BOOL,
	NUMBER_TYPE_FLOAT,
	NUMBER_TYPE_SIGNED_INT,
	NUMBER_TYPE_UNSIGNED_INT,
} NumberType;

// IF ORDER IS CHANGED, rewrite type_implicit_convert_ordered
typedef enum _TypeKind
{
	TYPE_POISONED,
	TYPE_UNRESOLVED,
	TYPE_UNRESOLVED_EXPR,
	TYPE_VOID,
	TYPE_OPAQUE,
	TYPE_BUILTIN,
	TYPE_NIL,
	TYPE_POINTER,
	TYPE_STRING,
	TYPE_ARRAY,
	TYPE_INC_ARRAY,
	TYPE_UNRESOLVED_ARRAY,
	TYPE_TYPEDEF,
	TYPE_MACRO,
	TYPE_FUNC_TYPE,
	TYPE_ENUM,
	TYPE_ERROR,
	TYPE_FUNC,
	TYPE_STRUCT,
	TYPE_UNION,
	TYPE_GENERIC,
} TypeKind;

struct _Type
{
	TypeKind type_kind;
	union
	{
		struct
		{
			unsigned char bytes;
			unsigned char bitsize;
			NumberType num_type : 8;
		};
		Decl *decl;
		struct
		{
			Token module;
			Token name;
		} unresolved;
		Expr *unresolved_type_expr;
		struct
		{
			union
			{
				Expr *unresolved_len;
				size_t len;
			};
			Type *base;
			bool nullable : 1;
		};
	};
	Type *canonical_type;
};

void type_setup(int pointer_size);
Type *type_new(TypeKind type_kind);
Type *type_poisoned();
char *type_to_string(Type *type);

static bool type_is_poison(Type *type)
{
	return type->type_kind == TYPE_POISONED;
}

static bool type_ok(Type *type)
{
	return !type || type->type_kind != TYPE_POISONED;
}

extern Type poisoned_type;

extern Type type_bool, type_void, type_nil, type_string;

extern Type type_half, type_float, type_double, type_quad;
extern Type type_char, type_short, type_int, type_long, type_isize;
extern Type type_byte, type_ushort, type_uint, type_ulong, type_usize;

extern Type type_compint, type_compfloat;

typedef enum _AttrKind
{
	ATTR_INVALID,
	ATTR_UNRESOLVED,
} AttrKind;

typedef struct _Attr
{
	Token module;
	Token name;
	union
	{
		Expr *expr;
	};
} Attr;

typedef struct _ErrorDecl
{
	Decl **error_constants;
} ErrorDecl;

typedef struct _ImportDecl
{
	ImportType type : 3;
	Token alias;
	Expr** generic_parameters;
	struct _Module *module;
} ImportDecl;

typedef struct _StructDecl
{
	Decl **members;
	Decl **method_functions;
} StructDecl;

typedef enum _VarDeclKind {
	VARDECL_CONST = 0,
	VARDECL_GLOBAL = 1,
	VARDECL_LOCAL = 2,
	VARDECL_PARAM = 3,
	VARDECL_MEMBER = 4,
	VARDECL_MULTI = 5,
} VarDeclKind;

typedef struct _VarDecl
{
	VarDeclKind kind : 3;
	Type *type;
	Expr *init_expr;
} VarDecl;


typedef struct _CtIfDecl
{
	Expr *expr;
	Decl **then;
	Decl *elif;
} CtIfDecl;


typedef enum _DeclKind
{
	DECL_POISONED = 0,
	DECL_BUILTIN,
	DECL_FUNC,
	DECL_VAR,
	DECL_ENUM_CONSTANT,
	DECL_TYPEDEF,
	DECL_STRUCT,
	DECL_UNION,
	DECL_ENUM,
	DECL_ERROR,
	DECL_ERROR_CONSTANT,
	DECL_FUNC_TYPE,
	DECL_ARRAY_VALUE,
	DECL_IMPORT,
	DECL_MACRO,
	DECL_MULTI_DECL,
	DECL_GENERIC,
	DECL_CT_IF,
	DECL_CT_ELSE,
	DECL_CT_ELIF,
} DeclKind;

static inline bool decl_may_be_type(DeclKind kind)
{
	switch (kind)
	{
		case DECL_TYPEDEF:
		case DECL_STRUCT:
		case DECL_UNION:
		case DECL_ENUM:
		case DECL_ERROR:
		case DECL_FUNC_TYPE:
		case DECL_BUILTIN:
			return true;
		default:
			return false;
	}
}
typedef struct _EnumConstantDecl
{
	Expr *expr;
} EnumConstantDecl;

typedef struct _ErrorConstantDecl
{
	uint32_t value;
} ErrorConstantDecl;

typedef struct _EnumDecl
{
	Decl** values;
	Type *type;
} EnumDecl;


typedef struct _FunctionSignature
{
    bool variadic : 1;
    Type *rtype;
    Decl** params;
    Token *throws;
} FunctionSignature;

typedef struct _FuncDecl
{
    const char *full_name;
    Type *struct_parent;
    FunctionSignature function_signature;
    Ast *body;
} FuncDecl;

typedef struct _TypedefDecl
{
    bool is_func : 1;
    union
    {
        FunctionSignature function_signature;
        Type *type;
    };
} TypedefDecl;

typedef struct _MacroDecl
{
    Decl **parameters;
    Type *rtype; // May be null!
    Ast *body;
} MacroDecl;

typedef struct _GenericDecl
{
    Ast **cases;
    Token *parameters;
    Type *rtype; // May be null!
} GenericDecl;


struct _Decl
{
	DeclKind decl_kind : 6;
	bool is_exported : 1;
	Visibility visibility : 2;
	ResolveStatus resolve_status : 2;
	bool is_used : 1;
	bool is_used_public : 1;
	bool has_cname : 1;
	uint32_t alignment : 5;
	union
	{
		uint32_t offset;
		uint32_t counter;
	};
	uint32_t size;
	Token name;
	struct _Module *module;
	Attr** attributes;
	Type *type;
	Type *pointer_types[2]; // Up to three stars
	union
	{
		ErrorDecl error;
		ErrorConstantDecl error_constant;
		ImportDecl import;
		StructDecl strukt;
		VarDecl var;
		EnumDecl enums;
		EnumConstantDecl enum_constant;
		FuncDecl func;
		TypedefDecl typedef_decl;
		Decl** multi_decl;
		MacroDecl macro_decl;
		GenericDecl generic_decl;
		CtIfDecl ct_if_decl;
		CtIfDecl ct_elif_decl;
		Decl** ct_else_decl;
		/*
		QualifiedType alias;
		SourceRange  unparsed_alias;
		EnumConstantDecl enum_constant;
		MacroDecl macro_decl;
		FuncTypeDecl func_type;
		ArrayDecl array_decl;
		MacroParmDecl macro_param;*/
	};
};

Decl *decl_new_in_module(struct _Module *module, DeclKind decl_kind, Token name, Visibility visibility);
Decl *decl_new_self_type(struct _Module *module, Token name, DeclKind decl_type, TypeKind type_kind, Visibility visibility);
Decl *decl_new_var(struct _Module *module, Token name, Type *type, VarDeclKind kind, Visibility visibility);
Decl *decl_new_enum_const(Decl *parent, Token name, DeclKind kind);
Decl *struct_find_name(Decl *decl, const char* name);

static inline Decl *decl_poison(Decl *decl)
{
	decl->decl_kind = DECL_POISONED;
	return decl;
}

static inline DeclKind decl_from_token(TokenType type)
{
	if (type == TOKEN_STRUCT) return DECL_STRUCT;
	if (type == TOKEN_UNION) return DECL_UNION;
	UNREACHABLE
}
static inline bool decl_ok(Decl *decl)
{
	return decl->decl_kind != DECL_POISONED;
}

extern Decl poisoned_decl;

typedef enum _BinOp
{
	BINOP_ERROR,
	BINOP_ASSIGN,
	BINOP_MULT,
	BINOP_MULT_ASSIGN,
	BINOP_ADD,
	BINOP_ADD_ASSIGN,
	BINOP_SUB,
	BINOP_SUB_ASSIGN,
	BINOP_DIV,
	BINOP_DIV_ASSIGN,
	BINOP_MOD,
	BINOP_MOD_ASSIGN,
	BINOP_AND,
	BINOP_AND_ASSIGN,
	BINOP_OR,
	BINOP_OR_ASSIGN,
	BINOP_BIT_AND,
	BINOP_BIT_AND_ASSIGN,
	BINOP_BIT_OR,
	BINOP_BIT_OR_ASSIGN,
	BINOP_BIT_XOR,
	BINOP_BIT_XOR_ASSIGN,
	BINOP_NE,
	BINOP_EQ,
	BINOP_GE,
	BINOP_GT,
	BINOP_LE,
	BINOP_LT,
	BINOP_SHR,
	BINOP_SHR_ASSIGN,
	BINOP_SHL,
	BINOP_SHL_ASSIGN,
	BINOP_ELVIS
} BinOp;

typedef enum _AssignOp
{
	ASSIGNOP_ERROR,
	ASSIGNOP_ASSIGN,
	ASSIGNOP_MULT_ASSIGN,
	ASSIGNOP_ADD_ASSIGN,
	ASSIGNOP_SUB_ASSIGN,
	ASSIGNOP_DIV_ASSIGN,
	ASSIGNOP_MOD_ASSIGN,
	ASSIGNOP_AND_ASSIGN,
	ASSIGNOP_OR_ASSIGN,
	ASSIGNOP_BIT_AND_ASSIGN,
	ASSIGNOP_BIT_OR_ASSIGN,
	ASSIGNOP_BIT_XOR_ASSIGN,
	ASSIGNOP_SHR_ASSIGN,
	ASSIGNOP_SHL_ASSIGN,
} AssignOp;

typedef enum _UnaryOp
{
	UNARYOP_ERROR,
	UNARYOP_DEREF,
	UNARYOP_ADDR,
	UNARYOP_NEG,
	UNARYOP_BITNEG,
	UNARYOP_NOT,
	UNARYOP_INC,
	UNARYOP_DEC,
} UnaryOp;

typedef enum _ExprKind
{
	EXPR_POISONED,
	EXPR_TRY,
	EXPR_CONST,
	EXPR_BINARY,
	EXPR_CONDITIONAL,
	EXPR_UNARY,
	EXPR_POST_UNARY,
	EXPR_TYPE,
	EXPR_IDENTIFIER,
	EXPR_METHOD_REF,
	EXPR_CALL,
	EXPR_SIZEOF,
	EXPR_SUBSCRIPT,
	EXPR_ACCESS,
	EXPR_STRUCT_VALUE,
	EXPR_STRUCT_INIT_VALUES,
	EXPR_INITIALIZER_LIST,
	EXPR_EXPRESSION_LIST,
	EXPR_DEFERRED_TOKENS,
} ExprKind;

typedef struct _ExprTry
{
	Expr *expr;
	Expr *else_expr;
} ExprTry;

typedef struct _ExprMethodRef
{
	Type *type;
	Token method;
} ExprMethodRef;

typedef struct _ExprStructValue
{
	Type *type;
	Expr *init_expr;
} ExprStructValue;

typedef struct _ExprTernary
{
	Expr *cond;
	Expr *then_expr; // May be null for elvis!
	Expr *else_expr;
} ExprTernary;

typedef struct _ExprBinary
{
	Expr *left;
	Expr *right;
	BinOp operator;
} ExprBinary;

typedef struct _ExprAssign
{
	Expr *left;
	Expr *right;
	AssignOp operator;
} ExprAssign;

typedef struct _ExprUnary
{
	Expr* expr;
	UnaryOp operator;
} ExprUnary;

typedef struct _ExprCall
{
	bool is_struct_function;
	Expr *function;
	Expr **parameters;
} ExprCall;

typedef struct _ExprSubscript
{
	Expr *expr;
	Expr *index;
} ExprSubscript;

typedef struct _ExprAccess
{
	Expr *parent;
	Token sub_element;
} ExprAccess;

typedef struct _ExprIdentifier
{
	Token module;
	Token identifier;
	bool is_ref;
	Decl *decl;
} ExprIdentifier;

typedef struct _ExprType
{
	Type *type;
} ExprType;

struct _Expr
{
	ExprKind expr_kind : 8;
	Token loc;
	Type *type;

	union {
		Token* deferred_tokens;
		Token deferred_token;
		Value const_expr;
		ExprStructValue struct_value_expr;
		ExprMethodRef method_ref_expr;
		ExprTry try_expr;
		ExprBinary binary_expr;
		ExprAssign assign_expr;
		ExprTernary conditional_expr;
		ExprUnary unary_expr;
		ExprUnary post_expr;
		ExprCall call_expr;
		ExprSubscript subscript_expr;
		ExprAccess access_expr;
		ExprIdentifier identifier_expr;
		ExprType type_expr;
		Expr** initializer_expr;
		Expr** expression_list;
		/*
		Value const_expr;
		ExprPost post_expr;
		ExprStructInitValues struct_init_values_expr;
		ExprDesignatedInitializer designated_initializer_expr;
		ExprSizeof sizeof_expr;
		ExprCast cast_expr;*/
	};
};

Expr *expr_new(ExprKind kind, Token start);
#define EXPR_NEW_EXPR(_kind, _expr) expr_new(_kind, _expr->loc)
#define EXPR_NEW_TOKEN(_kind, _tok) expr_new(_kind, _tok)

AssignOp assignop_from_token(TokenType type);
BinOp binop_from_token(TokenType type);
UnaryOp unaryop_from_token(TokenType type);

extern Expr poisoned_expr;

static inline bool expr_ok(Expr *expr)
{
	return expr == NULL || expr->expr_kind != EXPR_POISONED;
}

typedef struct _AstAttribute
{

} AstAttribute;

typedef enum _AstKind
{
	AST_POISONED,
	AST_ASM_STMT,
	AST_ATTRIBUTE,
	AST_BREAK_STMT,
	AST_CASE_STMT,
	AST_CATCH_STMT,
	AST_COMPOUND_STMT,
	AST_COND_STMT,
	AST_CONTINUE_STMT,
	AST_CT_IF_STMT,
	AST_CT_ELIF_STMT,
	AST_CT_ELSE_STMT,
	AST_DECLARE_STMT,
	AST_DECL_EXPR_LIST,
	AST_DEFAULT_STMT,
	AST_DEFER_STMT,
	AST_DO_STMT,
	AST_EXPR_STMT,
	AST_FOR_STMT,
	AST_GOTO_STMT,
	AST_IF_STMT,
	AST_LABEL,
	AST_NOP_STMT,
	AST_RETURN_STMT,
	AST_SWITCH_STMT,
	AST_THROW_STMT,
	AST_TRY_STMT,
	AST_NEXT_STMT,
	AST_VOLATILE_STMT,
	AST_WHILE_STMT,
	AST_GENERIC_CASE_STMT,
	AST_GENERIC_DEFAULT_STMT,
} AstKind;

// Ordering here is in priority if two branches should have the same exit.
typedef enum _ExitType
{
	EXIT_NONE,
	EXIT_BREAK,
	EXIT_GOTO,
	EXIT_CONTINUE,
	EXIT_RETURN,
} ExitType;

typedef struct _AstCompoundStmt
{
	Ast **stmts;
//	DeferList defer_list; TODO
} AstCompoundStmt;

typedef struct _AstLabel
{
	uint16_t last_goto;
	bool is_used : 1;
	Ast *defer;
	Ast *in_defer;
} AstLabelStmt;

typedef struct _AstReturnStmt
{
	Expr *expr; // May be NULL
	Ast *defer;
} AstReturnStmt;

typedef struct _AstWhileStmt
{
	Ast *cond;
	Ast *body;
} AstWhileStmt;

typedef struct _AstDoStmt
{
	Expr *expr;
	Ast *body;
} AstDoStmt;

typedef struct _AstIfStmt
{
	Ast *cond;
	Ast *then_body;
	Ast *else_body;
} AstIfStmt;


typedef struct _AstCaseStmt
{
	Expr *expr;
} AstCaseStmt;

typedef struct _AstSwitchStmt
{
	Ast *cond;
	Ast *body;
} AstSwitchStmt;

typedef struct _AstForStmt
{
	Ast *init;
	Expr *cond;
	Expr *incr;
	Ast *body;
} AstForStmt;

typedef enum DeclExprType
{
    DECLEXPR_DECL,
    DECLEXPR_EXPR,
} DeclExprType;

typedef struct _AstCondStmt
{
    Decl *decl;
    Ast *decl_expr;
} AstCondStmt;


typedef struct _AstDeclExprList
{
    DeclExprType list_type : 2;
    union
    {
        Decl *decl;
        Expr *expr;
    };
} AstDeclExprList;

typedef enum _GotoType
{
	GOTO_NOT_ANALYSED,
	GOTO_JUMP_FORWARD,
	GOTO_JUMP_BACK
} GotoType;

typedef struct _AstGotoStmt
{
	GotoType type : 2;
	Ast *defer;
	union
	{
		Ast *in_defer;
		Ast *defer_end;
	};
} AstGotoStmt;

typedef struct _AstDeferStmt
{
	bool emit_boolean : 1;
	Ast *body; // Compound statement
	Ast *prev_defer;
} AstDeferStmt;

typedef struct _AstCatchStmt
{
    Decl *error_param;
	Ast *body;
} AstCatchStmt;

typedef struct _AstCtIfStmt
{
	Expr *expr;
	Ast *then;
	Ast *elif;
} AstCtIfStmt;


typedef struct _AstGenericCaseStmt
{
    Type **types;
    Ast *body;
} AstGenericCaseStmt;

typedef struct _Ast
{
	AstKind ast_kind : 8;
	ExitType exit : 3;
	Token token;

	union
	{
		AstAttribute attribute;
		AstCompoundStmt compound_stmt;
		Decl *declare_stmt;
		Expr *expr_stmt;
		Expr *throw_stmt;
		Ast *volatile_stmt;
		Ast *try_stmt;
		AstLabelStmt label_stmt;
		AstReturnStmt return_stmt;
		AstWhileStmt while_stmt;
		AstDoStmt do_stmt;
		AstIfStmt if_stmt;
		AstDeferStmt defer_stmt;
		AstSwitchStmt switch_stmt;
		AstCaseStmt case_stmt;
		AstCatchStmt catch_stmt;
		AstGotoStmt goto_stmt;
		AstForStmt for_stmt;
		AstCondStmt cond_stmt;
		AstCtIfStmt ct_if_stmt;
		AstCtIfStmt ct_elif_stmt;
		Ast* ct_else_stmt;
		AstDeclExprList decl_expr_list;
		AstGenericCaseStmt generic_case_stmt;
		Ast* generic_default_stmt;
	};
} Ast;


#define NEW_AST(_kind, _token) new_ast(_kind, _token)

void *malloc_arena(size_t mem);

static inline Ast *new_ast(AstKind kind, Token token)
{
	Ast *ast = malloc_arena(sizeof(Ast));
	memset(ast, 0, sizeof(Ast));
	ast->token = token;
	ast->ast_kind = kind;
	ast->exit = EXIT_NONE;
	return ast;
}

extern Ast poisoned_ast;
static inline bool ast_ok(Ast *ast)
{
	return ast == NULL || ast->ast_kind != AST_POISONED;
}
static inline void ast_poison(Ast *ast)
{
	ast->ast_kind = AST_POISONED;
}

typedef struct _Module
{
	const char *name;

	bool is_external;
	bool is_c_library;
	bool is_exported;

	Ast **files; // Asts

	Decl** functions;
	STable struct_functions;
	STable symbols;
	STable public_symbols;
} Module;

extern Module module_poisoned;

void fprint_ast(FILE *file, Ast *ast);
void fprint_decl(FILE *file, Decl *dec);
void fprint_type_recursive(FILE *file, Type *type, int indent);
void fprint_expr_recursive(FILE *file, Expr *expr, int indent);
