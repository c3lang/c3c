#pragma once
// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "../utils/common.h"
#include "../utils/errors.h"
#include "../utils/lib.h"
#include "../build/build_options.h"
#include "compiler.h"
#include "enums.h"
#include "target.h"

typedef uint32_t SourceLoc;
#define INVALID_LOC UINT32_MAX
#define INVALID_RANGE ((SourceRange){ .loc = UINT32_MAX })
#define EMPTY_TOKEN ((Token) { .string = NULL })
#define MAX_LOCALS 0xFFFF
#define MAX_SCOPE_DEPTH 0xFF
#define MAX_PATH 1024
#define MAX_DEFERS 0xFFFF
#define MAX_FUNCTION_SIGNATURE_SIZE 2048
#define MAX_PARAMS 512
#define MAX_ERRORS 0xFFFF

typedef struct _Ast Ast;
typedef struct _Decl Decl;
typedef struct _TypeInfo TypeInfo;
typedef struct _Expr Expr;
typedef struct _Module Module;
typedef struct _Type Type;

typedef struct _BigInt
{
	unsigned digit_count;
	bool is_negative;
	union {
		uint64_t digit;
		uint64_t *digits;
	};
} BigInt;


typedef struct
{
	union
	{
		long double f;
		BigInt i;
		bool b;
		struct
		{
			char* chars;
			int len;
		} string;
		Decl *enum_constant;
		Decl *error_constant;
	};
	// Valid type kinds:
	// bool, ints, floats, string
	TypeKind kind;
} ExprConst;

typedef struct
{
	SourceLoc loc;
	SourceLoc end_loc;
} SourceRange;

typedef struct
{
	Ast *start;
	Ast *end;
} DeferList;

typedef struct
{
	SourceRange span;
	TokenType type : 8;
	union
	{
		const char *string;
		const char* start;
	};
} Token;

typedef struct _Diagnostics
{
	bool panic_mode;
	unsigned errors;
	unsigned warnings;
} Diagnostics;

typedef struct
{
	const char *key;
	void *value;
} SEntry;

typedef struct
{
	uint32_t count;
	uint32_t capacity;
	SEntry *entries;
} STable;


typedef struct
{
	const char *contents;
	char *name;
	char *dir_path;
	const char *full_path;
	SourceLoc start_id;
	SourceLoc end_id;
	SourceLoc *lines;
	SourceLoc current_line_start;
} File;

typedef struct
{
	File *file;
	uint32_t line;
	uint32_t col;
	SourceLoc loc;
	const char *start;
} SourcePosition;

typedef struct _Path
{
	SourceRange span;
	const char *module;
	uint32_t len;
} Path;

typedef enum
{
	DESIGNATED_IDENT,
	DESIGNATED_SUBSCRIPT,
} DesignatedPathKind;

typedef struct _DesignatedPath
{
	DesignatedPathKind kind;
	struct _DesignatedPath *sub_path;
	Type *type;
	union
	{
		unsigned index;
		Expr *index_expr;
	};
} DesignatedPath;

typedef struct
{
	unsigned char bitsize;
	unsigned char bytesize;
	unsigned char abi_alignment;
	unsigned char pref_alignment;
}  TypeBuiltin;

typedef struct
{
	Token name_loc;
	Path *path;
} TypeUnresolved;

typedef struct
{
	Type *base;
	size_t len;
} TypeArray;

typedef struct
{
	struct _FunctionSignature *signature;
} TypeFunc;

struct _Type
{
	TypeKind type_kind : 8;
	Type *canonical;
	const char *name;
	Type **type_cache;
	void *backend_type;
	void *backend_typeid;
	void *backend_debug_type;
	union
	{
		// Error, Struct, Union, Typedef
		Decl *decl;
		// int, float, bool
		TypeBuiltin builtin;
		// Type[], Type[*], Type[123]
		TypeArray array;
		// func Type1(Type2, Type3, ...) throws Err1, Err2, ...
		TypeFunc func;
		// Type*
		Type *pointer;
		// Type.type
		Type *child;
	};
};

struct _TypeInfo
{
	ResolveStatus resolve_status : 2;
	Type *type;
	TypeInfoKind kind;
	SourceRange span;
	union
	{
		TypeUnresolved unresolved;
		Expr *unresolved_type_expr;
		struct
		{
			TypeInfo *base;
			Expr *len;
		} array;
		TypeInfo *pointer;
	};
};

typedef struct
{
	Path *path;
	Token name;
	union
	{
		Expr *expr;
	};
} Attr;

typedef struct
{
	Decl **error_constants;
	void *start_value;
} ErrorDecl;

typedef struct
{
	Path *path;
	Token symbol;
	bool aliased;
} ImportDecl;

typedef struct
{
	uint32_t abi_alignment;
	uint32_t id;
	uint64_t size;
	Decl **members;
} StructDecl;


typedef struct _VarDecl
{
	unsigned id : 16;
	VarDeclKind kind : 3;
	TypeInfo *type_info;
	union
	{
		Expr *init_expr;
		Decl *parent;
	};
	void *backend_ref;
	void *backend_debug_ref;
} VarDecl;


typedef struct
{
	Expr *expr;
	Decl **then;
	Decl *elif;
} CtIfDecl;


typedef struct
{
	Expr *expr;
	Expr **args;
	uint64_t ordinal;
} EnumConstantDecl;

typedef struct
{
	Decl *parent;
	uint32_t value;
} ErrorConstantDecl;

typedef struct
{
	Decl** values;
	Decl** parameters;
	TypeInfo *type_info;
} EnumDecl;

typedef enum
{
	ERROR_RETURN_NONE = 0,
	ERROR_RETURN_ONE = 1,
	ERROR_RETURN_MANY = 2,
	ERROR_RETURN_ANY = 3,
} ErrorReturn;
typedef struct _FunctionSignature
{
	CallConvention convention : 4;
	bool variadic : 1;
	bool has_default : 1;
	bool throw_any : 1;
	bool return_param : 1;
	ErrorReturn error_return : 4;
	TypeInfo *rtype;
	Decl** params;
	Decl** throws;
	const char *mangled_signature;
} FunctionSignature;

typedef struct
{
	Decl **vars;
} FuncAnnotations;

typedef struct
{
	TypeInfo *type_parent;
	FunctionSignature function_signature;
	Ast *body;
	FuncAnnotations *annotations;
	Decl **locals;
	Ast **labels;
	void *backend_value;
} FuncDecl;

typedef struct
{
	AttributeDomains domains;
	FunctionSignature attr_signature;
} AttrDecl;

typedef struct
{
	Decl *import;
} AliasDecl;

typedef struct
{
	bool is_func : 1;
	union
	{
		FunctionSignature function_signature;
		TypeInfo *type_info;
	};
} TypedefDecl;

typedef struct
{
	Decl **parameters;
	TypeInfo *rtype; // May be null!
	struct _Ast *body;
} MacroDecl;

typedef struct
{
	struct _Ast **cases;
	Token *parameters;
	TypeInfo *rtype; // May be null!
	Path *path; // For redefinition
} GenericDecl;


typedef struct _Decl
{
	const char *name;
	SourceRange name_span;
	SourceRange span;
	const char *external_name;
	DeclKind decl_kind : 6;
	Visibility visibility : 2;
	ResolveStatus resolve_status : 2;
	bool is_packed : 1;
/*	bool is_exported : 1;
	bool is_used : 1;
	bool is_used_public : 1;
	bool has_cname : 1;
	uint32_t alignment : 5;
	union
	{
		uint32_t offset;
		uint32_t counter;
	};
	uint32_t size;*/
	Module *module;
	Attr** attributes;
	Type *type;
	union
	{
		struct
		{
			union
			{
				Decl* parent_struct;
				Decl** method_functions;
			};
			union
			{
				ErrorDecl error;
				StructDecl strukt;
				EnumDecl enums;
			};
		};
		ErrorConstantDecl error_constant;
		ImportDecl import;
		VarDecl var;
		EnumConstantDecl enum_constant;
		FuncDecl func;
		AttrDecl attr;
		TypedefDecl typedef_decl;
		MacroDecl macro_decl;
		GenericDecl generic_decl;
		CtIfDecl ct_if_decl;
		CtIfDecl ct_elif_decl;
		Decl** ct_else_decl;
		Expr *incr_array_decl;
		TypeInfo *throws;
	};
} Decl;

typedef enum
{
	TRY_EXPR_ELSE_EXPR,
	TRY_EXPR_ELSE_JUMP,
	TRY_STMT,
} TryType;

typedef struct
{
	TryType type;
	union
	{
		Expr *expr;
		Ast *stmt;
	};
	union
	{
		Expr *else_expr;
		Ast *else_stmt;
	};
	void *jump_target;
} ExprTry;

typedef struct
{
	TypeInfo *type;
	union
	{
		Token name;
		Decl *method;
	};
} ExprTypeAccess;

typedef struct
{
	TypeInfo *type;
	Expr *init_expr;
} ExprStructValue;

typedef struct
{
	Expr *cond;
	Expr *then_expr; // May be null for elvis!
	Expr *else_expr;
} ExprTernary;

typedef struct
{
	Expr *left;
	Expr *right;
	BinaryOp operator;
} ExprBinary;

typedef struct
{
	Expr* expr;
	UnaryOp operator;
} ExprUnary;

typedef struct
{
	Expr* expr;
	PostUnaryOp operator;
} ExprPostUnary;


typedef enum
{
	CATCH_TRY_ELSE,
	CATCH_REGULAR,
	CATCH_RETURN_ANY,
	CATCH_RETURN_MANY,
	CATCH_RETURN_ONE
} CatchKind;

typedef struct
{
	CatchKind kind;
	union
	{
		Expr *try_else;
		Ast *catch;
		Decl *error;
	};
} CatchInfo;

typedef struct
{
	bool is_completely_handled;
	DeferList defers;
	CatchInfo *catches;
} ThrowInfo;

typedef struct
{
	bool is_struct_function : 1;
	Expr *function;
	Expr **arguments;
	ThrowInfo *throw_info;
} ExprCall;

typedef struct
{
	Expr *expr;
	Expr *index;
} ExprSubscript;

typedef struct
{
	Expr *parent;
	union
	{
		Token sub_element;
		Decl *ref;
	};
} ExprAccess;

typedef struct
{
	Path *path;
	const char *identifier;
	bool is_ref;
	Decl *decl;
} ExprIdentifier;


typedef struct
{
	CastKind kind;
	Expr *expr;
	TypeInfo *type_info;
	union
	{
		size_t truncated_size;
	};
} ExprCast;

typedef struct
{
	Expr *expr;
	DeferList defers;
} ExprScope;

typedef struct
{
	Ast **stmts;
} ExprFuncBlock;

typedef struct
{
	Expr *left;
	Expr *right;
} ExprRange;

typedef enum
{
	INITIALIZER_UNKNOWN,
	INITIALIZER_ZERO,
	INITIALIZER_DESIGNATED,
	INITIALIZER_NORMAL
} InitializerType;

typedef struct
{
	InitializerType init_type;
	Expr** initializer_expr;
} ExprInitializer;

typedef struct
{
	Expr *initializer;
	TypeInfo *type_info;
} ExprCompoundLiteral;

typedef struct
{
	DesignatedPath *path;
	Expr* value;
} ExprDesignatedInit;

struct _Expr
{
	ExprKind expr_kind : 8;
	ResolveStatus resolve_status : 3;
	SourceRange span;
	Type *type;
	union {
		ExprDesignatedInit designated_init_expr;
		Expr *group_expr;
		ExprCast cast_expr;
		Expr *typeof_expr;
		ExprConst const_expr;
		ExprRange range_expr;
		ExprStructValue struct_value_expr;
		ExprTypeAccess type_access;
		ExprTry try_expr;
		Expr* macro_expr;
		ExprBinary binary_expr;
		ExprTernary ternary_expr;
		ExprUnary unary_expr;
		ExprPostUnary post_expr;
		ExprCall call_expr;
		ExprSubscript subscript_expr;
		ExprAccess access_expr;
		ExprIdentifier identifier_expr;
		TypeInfo *typeid_expr;
		ExprInitializer expr_initializer;
		ExprCompoundLiteral expr_compound_literal;
		Expr** expression_list;
		ExprScope expr_scope;
		ExprFuncBlock expr_block;
	};
};

typedef struct
{

} AstAttribute;


typedef struct
{
	struct _Ast **stmts;
	DeferList defer_list;
} AstCompoundStmt;

typedef struct
{
	const char *name;
	uint16_t last_goto;
	bool is_used : 1;
	Ast *defer;
	struct _Ast *in_defer;
	void *backend_value;
} AstLabelStmt;

typedef struct
{
	Expr *expr; // May be NULL
	Ast *defer;
} AstReturnStmt;

typedef struct
{
	Ast *decl;
	Ast *cond;
	Ast *body;
} AstWhileStmt;

typedef struct
{
	Expr *expr;
	Ast *body;
	DeferList expr_defer;
	DeferList body_defer;
} AstDoStmt;

typedef struct
{
	Ast *decl;
	Ast *cond;
	Ast *then_body;
	Ast *else_body;
} AstIfStmt;


typedef struct
{
	Expr *expr; // NULL == DEFAULT
	Ast *body;
	void *backend_value;
} AstCaseStmt;


typedef struct
{
	Ast *decl;
	Ast *cond;
	Ast **cases;
} AstSwitchStmt;

typedef struct
{
	Ast *init;
	Expr *cond;
	Expr *incr;
	Ast *body;
} AstForStmt;



typedef struct
{
	const char *label_name;
	Ast *label;
	DeferList defer;
	union
	{
		struct _Ast *in_defer;
	};
} AstGotoStmt;

typedef struct _AstDeferStmt
{
	bool emit_boolean : 1;
	Ast *body; // Compound statement
	Ast *prev_defer;
	void *bool_var;
} AstDeferStmt;

typedef struct _AstCatchStmt
{
	Decl *error_param;
	struct _Ast *body;
	void *block;
} AstCatchStmt;

typedef struct _AstCtIfStmt
{
	Expr *expr;
	struct _Ast *then;
	struct _Ast *elif;
} AstCtIfStmt;


typedef struct _AstGenericCaseStmt
{
	TypeInfo **types;
	struct _Ast *body;
} AstGenericCaseStmt;

typedef struct
{
	Ast *stmt;
	DeferList defers;
} AstScopedStmt;

typedef struct
{
	Expr *cond;
	Ast **body;
} AstCtSwitchStmt;

typedef struct
{
	TypeInfo **types;
	Ast *body;
} AstCtCaseStmt;

typedef struct
{
	Token index;
	Token value;
	Expr *expr;
	Ast *body;
} AstCtForStmt;

typedef struct
{
	DeferList defers;
} AstContinueBreakStmt;

typedef struct
{
	Ast *prev;
	DeferList defers;
} AstNextStmt;

typedef struct
{
	Expr *throw_value;
} AstThrowStmt;

typedef struct
{
	Token name;
	Token constraints;
	Token token;
} AsmOperand;

typedef struct
{
	bool is_volatile : 1;
	bool is_inline : 1;
	bool is_goto : 1;
	Expr *asm_template;
	AsmOperand** output_operands;
	AsmOperand** input_operands;
	AsmOperand** clobbers;
	Token* labels;
} AstAsmStmt;

typedef struct _Ast
{
	SourceRange span;
	AstKind ast_kind : 8;
	union
	{
		AstAttribute attribute;
		AstAsmStmt asm_stmt;
		AstCompoundStmt compound_stmt;
		Decl *declare_stmt;
		Expr *expr_stmt;
		AstThrowStmt throw_stmt;
		Ast *volatile_stmt;
		AstLabelStmt label_stmt;
		AstReturnStmt return_stmt;
		AstWhileStmt while_stmt;
		AstDoStmt do_stmt;
		AstIfStmt if_stmt;
		AstDeferStmt defer_stmt;
		AstSwitchStmt switch_stmt;
		AstCaseStmt case_stmt;
		AstCtSwitchStmt ct_switch_stmt;
		AstCtCaseStmt ct_case_stmt;
		AstContinueBreakStmt continue_stmt;
		AstContinueBreakStmt break_stmt;
		Ast* ct_default_stmt;
		AstNextStmt next_stmt;
		AstCatchStmt catch_stmt;
		AstGotoStmt goto_stmt;
		AstForStmt for_stmt;
		AstCtIfStmt ct_if_stmt;
		AstCtIfStmt ct_elif_stmt;
		Ast *ct_else_stmt;
		AstCtForStmt ct_for_stmt;
		AstGenericCaseStmt generic_case_stmt;
		Ast *generic_default_stmt;
		Ast** decl_expr_stmt;
		AstScopedStmt scoped_stmt;
	};
} Ast;


typedef struct _Module
{
	Path *name;

	bool is_external;
	bool is_c_library;
	bool is_exported;

	Ast **files; // Asts

	Decl** functions;
	STable struct_functions;
	STable symbols;
	STable public_symbols;
	Module **sub_modules;
} Module;


typedef struct _DynamicScope
{
	ScopeFlags flags;
	ScopeFlags flags_created;
	unsigned throws;
	Decl **local_decl_start;
	DeferList defers;
	ExitType exit;
} DynamicScope;


typedef struct
{
	const char *file_begin;
	const char *lexing_start;
	const char *current;
	uint16_t source_file;
	File *current_file;
	SourceLoc last_in_range;
	Token tok;
	Token next_tok;
} Lexer;

typedef enum
{
	THROW_TYPE_CALL_ANY,
	THROW_TYPE_CALL_THROW_MANY,
	THROW_TYPE_CALL_THROW_ONE,
} ThrowType;

typedef struct
{
	SourceRange span;
	ThrowType kind : 4;
	ThrowInfo *throw_info;
	// The error type of the throw.
	union
	{
		Type *throw;
		Decl **throws;
	};
} Throw;

typedef struct _Context
{
	BuildTarget *target;
	Path *module_name;
	Token* module_parameters;
	File* file;
	Decl** imports;
	Decl *specified_imports;
	Module *module;
	STable local_symbols;
	Decl **enums;
	Decl **error_types;
	Decl **types;
	Decl **functions;
	Decl **struct_functions;
	Decl **vars;
	Decl **ct_ifs;
	Ast **defers;
	Decl *active_function_for_analysis;
	Decl *active_type_for_analysis;
	Decl **last_local;
	Ast **labels;
	Ast **gotos;
	Token *comments;
	Token *lead_comment;
	Token *trailing_comment;
	Token *next_lead_comment;
	DynamicScope *current_scope;
	struct
	{
		Type *expected_block_type;
		Ast **returns;
		// Reusable returns cache.
		Ast **returns_cache;
	};
	Decl *evaluating_macro;
	// Error handling
	struct
	{
		Ast **throw;
		Throw *error_calls;
		int try_nesting;
	};
	Type *rtype;
	int in_volatile_section;
	Decl *locals[MAX_LOCALS];
	DynamicScope scopes[MAX_SCOPE_DEPTH];
	char path_scratch[MAX_PATH];
	struct {
		STable external_symbols;
		Decl **external_symbol_list;
	};
	STable scratch_table;
	Lexer lexer;
	SourceLoc prev_tok_end;
	Token tok;
	Token next_tok;
	struct
	{
		bool in_lookahead;
		const char *current;
		const char *start;
		Token tok;
		Token next_tok;
		Token *lead_comment;
		Token *trailing_comment;
		Token *next_lead_comment;
	} stored;
} Context;

typedef struct
{
	STable modules;
	STable global_symbols;
	STable qualified_symbols;
	Type **type;
} Compiler;

extern Compiler compiler;
extern Ast *poisoned_ast;
extern Decl *poisoned_decl;
extern Expr *poisoned_expr;
extern Type *poisoned_type;
extern TypeInfo *poisoned_type_info;
extern Module poisoned_module;
extern Diagnostics diagnostics;

extern Decl all_error;

extern Type *type_bool, *type_void, *type_string, *type_voidptr;
extern Type *type_float, *type_double;
extern Type *type_char, *type_short, *type_int, *type_long, *type_isize;
extern Type *type_byte, *type_ushort, *type_uint, *type_ulong, *type_usize;
extern Type *type_compint, *type_compfloat;
extern Type *type_c_short, *type_c_int, *type_c_long, *type_c_longlong;
extern Type *type_c_ushort, *type_c_uint, *type_c_ulong, *type_c_ulonglong;
extern Type *type_typeid, *type_error_union, *type_error_base;


extern const char *main_name;

#define AST_NEW_TOKEN(_kind, _token) new_ast(_kind, _token.span)
#define AST_NEW(_kind, _loc) new_ast(_kind, _loc)

static inline bool ast_ok(Ast *ast) { return ast == NULL || ast->ast_kind != AST_POISONED; }
static inline bool ast_poison(Ast *ast) { ast->ast_kind = AST_POISONED; return false; }
Ast *ast_from_expr(Expr *expr);

static inline Ast *new_ast(AstKind kind, SourceRange range)
{
	Ast *ast = malloc_arena(sizeof(Ast));
	memset(ast, 0, sizeof(Ast));
	ast->span = range;
	ast->ast_kind = kind;
	return ast;
}

static inline Ast *extend_ast(Ast *ast, Token token)
{
	ast->span.end_loc = token.span.end_loc;
	return ast;
}

static inline Ast *extend_ast_with_prev_token(Context *context, Ast *ast)
{
	ast->span.end_loc = context->prev_tok_end;
	return ast;
}



void builtin_setup(Target *target);

static inline bool builtin_may_negate(Type *canonical)
{
	assert(canonical->canonical == canonical);
	switch (canonical->type_kind)
	{
		case TYPE_FXX:
		case TYPE_F32:
		case TYPE_F64:
		case TYPE_I8:
		case TYPE_I16:
		case TYPE_I32:
		case TYPE_I64:
		case TYPE_IXX:
			return true;
		default:
			return false;
	}
}

static inline bool builtin_may_bit_negate(Type *canonical)
{
	assert(canonical->canonical == canonical);
	switch (canonical->type_kind)
	{
		case TYPE_BOOL:
		case TYPE_I8:
		case TYPE_I16:
		case TYPE_I32:
		case TYPE_I64:
		case TYPE_IXX:
		case TYPE_U8:
		case TYPE_U16:
		case TYPE_U32:
		case TYPE_U64:
			return true;
		default:
			return false;
	}
}


bool cast_implicit(Expr *expr, Type *to_type);
bool cast(Expr *expr, Type *to_type, CastType cast_type);
bool cast_binary_arithmetic(Expr *left, Expr *right, const char *action);
CastKind cast_to_bool_kind(Type *type);
bool cast_to_runtime(Expr *expr);
void cast_to_smallest_runtime(Expr *expr);

void llvm_codegen(Context *context);
void llvm_codegen_setup();


bool sema_analyse_expr_of_required_type(Context *context, Type *to, Expr *expr);
bool sema_analyse_expr(Context *context, Type *to, Expr *expr);
bool sema_analyse_decl(Context *context, Decl *decl);

void compiler_add_type(Type *type);
Decl *compiler_find_symbol(Token token);
Module *compiler_find_or_create_module(Path *module_name);
void compiler_register_public_symbol(Decl *decl);

Context *context_create(File *file, BuildTarget *target);
void context_register_global_decl(Context *context, Decl *decl);
void context_register_external_symbol(Context *context, Decl *decl);
bool context_add_import(Context *context, Path *path, Token symbol, Token alias);
bool context_set_module_from_filename(Context *context);
bool context_set_module(Context *context, Path *path, Token *generic_parameters);
void context_print_ast(Context *context, FILE *file);

#pragma mark --- Decl functions

Decl *decl_new(DeclKind decl_kind, Token name, Visibility visibility);
Decl *decl_new_with_type(Token name, DeclKind decl_type, Visibility visibility);
Decl *decl_new_var(Token name, TypeInfo *type, VarDeclKind kind, Visibility visibility);
void decl_set_external_name(Decl *decl);
const char *decl_var_to_string(VarDeclKind kind);

static inline bool decl_ok(Decl *decl) { return decl->decl_kind != DECL_POISONED; }
static inline bool decl_poison(Decl *decl) { decl->decl_kind = DECL_POISONED; decl->resolve_status = RESOLVE_DONE; return false; }
static inline bool decl_is_struct_type(Decl *decl) { return decl->decl_kind == DECL_UNION || decl->decl_kind == DECL_STRUCT; }
static inline DeclKind decl_from_token(TokenType type)
{
	if (type == TOKEN_STRUCT) return DECL_STRUCT;
	if (type == TOKEN_UNION) return DECL_UNION;
	UNREACHABLE
}

#pragma mark --- Diag functions

void diag_reset(void);
void diag_error_range(SourceRange span, const char *message, ...);
void diag_verror_range(SourceRange span, const char *message, va_list args);


#define EXPR_NEW_EXPR(_kind, _expr) expr_new(_kind, _expr->span)
#define EXPR_NEW_TOKEN(_kind, _tok) expr_new(_kind, _tok.span)
Expr *expr_new(ExprKind kind, SourceRange start);
static inline bool expr_ok(Expr *expr) { return expr == NULL || expr->expr_kind != EXPR_POISONED; }
static inline bool expr_poison(Expr *expr) { expr->expr_kind = EXPR_POISONED; expr->resolve_status = RESOLVE_DONE; return false; }
static inline void expr_replace(Expr *expr, Expr *replacement)
{
	SourceRange loc = expr->span;
	*expr = *replacement;
	expr->span = loc;
}
void expr_const_set_int(ExprConst *expr, uint64_t v, TypeKind kind);
void expr_const_set_float(ExprConst *expr, long double d, TypeKind kind);
void expr_const_set_bool(ExprConst *expr, bool b);
void expr_const_set_nil(ExprConst *expr);
void expr_const_fprint(FILE *__restrict file, ExprConst *expr);
bool expr_const_int_overflowed(const ExprConst *expr);
bool expr_const_compare(const ExprConst *left, const ExprConst *right, BinaryOp op);
const char *expr_const_to_error_string(const ExprConst *expr);

void fprint_ast(FILE *file, Ast *ast);
void fprint_decl(FILE *file, Decl *dec);
void fprint_type_info_recursive(FILE *file, TypeInfo *type_info, int indent);
void fprint_expr_recursive(FILE *file, Expr *expr, int indent);


#pragma mark --- Lexer functions

Token lexer_scan_token(Lexer *lexer);
Token lexer_scan_ident_test(Lexer *lexer, const char *scan);
void lexer_init_for_test(Lexer *lexer, const char *text, size_t len);
void lexer_init_with_file(Lexer *lexer, File *file);
File* lexer_current_file(Lexer *lexer);


typedef enum
{
	MODULE_SYMBOL_SEARCH_EXTERNAL,
	MODULE_SYMBOL_SEARCH_PARENT,
	MODULE_SYMBOL_SEARCH_THIS
} ModuleSymbolSearch;

Decl *module_find_symbol(Module *module, const char *symbol, ModuleSymbolSearch search);

void parse_file(Context *context);
Path *path_create_from_string(const char *string, size_t len, SourceRange span);
Path *path_find_parent_path(Path *path);

const char *resolve_status_to_string(ResolveStatus status);

#define SEMA_TOKEN_ERROR(_tok, ...) sema_error_range(_tok.span, __VA_ARGS__)
#define SEMA_ERROR(_node, ...) sema_error_range(_node->span, __VA_ARGS__)
#define SEMA_PREV(_node, ...) sema_prev_at_range(_node->span, __VA_ARGS__)
void sema_analysis_pass_process_imports(Context *context);
void sema_analysis_pass_conditional_compilation(Context *context);
void sema_analysis_pass_decls(Context *context);

bool sema_add_local(Context *context, Decl *decl);
bool sema_analyse_statement(Context *context, Ast *statement);
Decl *sema_resolve_symbol(Context *context, const char *symbol, Path *path, Decl **ambiguous_other_decl);
bool sema_resolve_type_info(Context *context, TypeInfo *type_info);
bool sema_resolve_type_shallow(Context *context, TypeInfo *type_info);
void sema_error_at(SourceLoc loc, const char *message, ...);
void sema_error_range(SourceRange range, const char *message, ...);
void sema_verror_at(SourceLoc loc, const char *message, va_list args);
void sema_verror_range(SourceRange range, const char *message, va_list args);
void sema_error(Context *context, const char *message, ...);
void sema_prev_at_range(SourceRange span, const char *message, ...);
void sema_prev_at(SourceLoc loc, const char *message, ...);
void sema_shadow_error(Decl *decl, Decl *old);

File *source_file_load(const char *filename, bool *already_loaded);
File *source_file_from_position(SourceLoc loc);
void source_file_append_line_end(File *file, SourceLoc loc);
SourcePosition source_file_find_position_in_file(File *file, SourceLoc loc);
SourcePosition source_file_find_position(SourceLoc loc);
SourceRange source_range_from_ranges(SourceRange first, SourceRange last);
#define RANGE_EXTEND_PREV(x) (x)->span.end_loc = context->prev_tok_end

static inline unsigned source_range_len(SourceRange range) { return range.end_loc - range.loc; }

void stable_init(STable *table, uint32_t initial_size);
void *stable_set(STable *table, const char *key, void *value);
void *stable_get(STable *table, const char *key);
void *stable_delete(STable *table, const char *key);
void stable_clear(STable *table);

const char *symtab_add(const char *symbol, uint32_t len, uint32_t fnv1hash, TokenType *type);

void target_setup();
int target_alloca_addr_space();
void *target_data_layout();
void *target_machine();
void *target_target();

bool throw_completely_caught(Decl *throw, CatchInfo *catches);
static inline Throw throw_new_single(SourceRange range, ThrowType type, ThrowInfo *info, Type *throw)
{
	return (Throw) { .kind = type, .span = range, .throw_info = info, .throw = throw };
}
static inline Throw throw_new_union(SourceRange range, ThrowType type, ThrowInfo *info)
{
	return (Throw) { .kind = type, .span = range, .throw_info = info, .throw = type_error_union };
}
static inline Throw throw_new_multiple(SourceRange range, ThrowInfo *info, Decl **throws)
{
	return (Throw) { .kind = THROW_TYPE_CALL_THROW_MANY, .span = range, .throw_info = info, .throws = throws };
}

#define TOKEN_MAX_LENGTH 0xFFFF
#define TOK2VARSTR(_token) _token.span.length, _token.start
bool token_is_type(TokenType type);
bool token_is_symbol(TokenType type);
const char *token_type_to_string(TokenType type);
static inline Token wrap(const char *string)
{
	return (Token) { .span = INVALID_RANGE, .type = TOKEN_IDENT, .string = string };
}

Type *type_get_ptr(Type *ptr_type);
Type *type_get_meta(Type *meta_type);
Type *type_get_indexed_type(Type *type);
Type *type_get_array(Type *arr_type, uint64_t len);
Type *type_signed_int_by_bitsize(unsigned bytesize);
Type *type_unsigned_int_by_bitsize(unsigned bytesize);
bool type_is_subtype(Type *type, Type *possible_subtype);
Type *type_find_common_ancestor(Type *left, Type *right);
const char *type_to_error_string(Type *type);
size_t type_size(Type *canonical);
unsigned int type_abi_alignment(Type *canonical);
void type_append_signature_name(Type *type, char *dst, size_t *offset);
Type *type_find_max_type(Type *type, Type *other);

static inline bool type_is_builtin(TypeKind kind) { return kind >= TYPE_VOID && kind <= TYPE_FXX; }
static inline bool type_kind_is_signed(TypeKind kind) { return kind >= TYPE_I8 && kind <= TYPE_I64; }
static inline bool type_kind_is_unsigned(TypeKind kind) { return kind >= TYPE_U8 && kind <= TYPE_U64; }
static inline bool type_kind_is_any_integer(TypeKind kind) { return kind >= TYPE_I8 && kind <= TYPE_IXX; }
static inline bool type_is_signed(Type *type) { return type->type_kind >= TYPE_I8 && type->type_kind <= TYPE_I64; }
static inline bool type_is_unsigned(Type *type) { return type->type_kind >= TYPE_U8 && type->type_kind <= TYPE_U64; }
static inline bool type_ok(Type *type) { return !type || type->type_kind != TYPE_POISONED; }
static inline bool type_info_ok(TypeInfo *type_info) { return !type_info || type_info->kind != TYPE_INFO_POISON; }
bool type_may_have_method_functions(Type *type);

static inline Type *type_reduced(Type *type)
{
	Type *canonical = type->canonical;
	if (canonical->type_kind == TYPE_ENUM) return canonical->decl->enums.type_info->type->canonical;
	if (canonical->type_kind == TYPE_ERROR) return type_error_base;
	return canonical;
}

static inline bool type_is_structlike(Type *type)
{
	assert(type->canonical == type);
	switch (type->type_kind)
	{
		case TYPE_UNION:
		case TYPE_STRUCT:
			return true;
		default:
			return false;

	}
}

static inline Type *type_reduced_from_expr(Expr *expr)
{
	return type_reduced(expr->type);
}


static inline bool type_is_integer(Type *type)
{
	assert(type == type->canonical);
	return type->type_kind >= TYPE_I8 && type->type_kind <= TYPE_U64;
}

static inline bool type_is_any_integer(Type *type)
{
	assert(type == type->canonical);
	return type->type_kind >= TYPE_I8 && type->type_kind <= TYPE_IXX;
}

static inline bool type_is_signed_integer(Type *type)
{
	assert(type == type->canonical);
	return type->type_kind >= TYPE_I8 && type->type_kind <= TYPE_I64;
}

static inline bool type_is_unsigned_integer(Type *type)
{
	assert(type == type->canonical);
	return type->type_kind >= TYPE_U8 && type->type_kind <= TYPE_U64;
}

static inline bool type_info_poison(TypeInfo *type)
{
	type->type = poisoned_type;
	type->resolve_status = RESOLVE_DONE;
	return false;
}

static inline bool type_is_float(Type *type)
{
	assert(type == type->canonical);
	return type->type_kind >= TYPE_F32 && type->type_kind <= TYPE_FXX;
}

static inline TypeInfo *type_info_new(TypeInfoKind kind, SourceRange range)
{
	TypeInfo *type_info = malloc_arena(sizeof(TypeInfo));
	memset(type_info, 0, sizeof(TypeInfo));
	type_info->kind = kind;
	type_info->span = range;
	type_info->resolve_status = RESOLVE_NOT_DONE;
	return type_info;
}

static inline TypeInfo *type_info_new_base(Type *type, SourceRange range)
{
	TypeInfo *type_info = malloc_arena(sizeof(TypeInfo));
	memset(type_info, 0, sizeof(TypeInfo));
	type_info->kind = TYPE_INFO_IDENTIFIER;
	type_info->resolve_status = RESOLVE_DONE;
	type_info->type = type;
	type_info->span = range;
	return type_info;
}

static inline Type *type_new(TypeKind kind, const char *name)
{
	Type *type = malloc_arena(sizeof(Type));
	memset(type, 0, sizeof(Type));
	type->type_kind = kind;
	type->name = name;
	compiler_add_type(type);
	return type;
}


static inline bool type_convert_will_trunc(Type *destination, Type *source)
{
	assert(type_is_builtin(destination->canonical->type_kind));
	assert(type_is_builtin(source->canonical->type_kind));
	return (unsigned)destination->canonical->builtin.bitsize < (unsigned)source->canonical->builtin.bitsize;
}

static inline bool type_is_numeric(Type *type)
{
	assert(type == type->canonical);
	return type->type_kind >= TYPE_I8 && type->type_kind <= TYPE_FXX;
}


#define TYPE_MODULE_UNRESOLVED(_module, _name) ({ Type *__type = type_new(TYPE_USER_DEFINED); \
 __type->name_loc = _name; __type->unresolved.module = _module; __type; })
#define TYPE_UNRESOLVED(_name) ({ TypeInfo *__type = type_new(TYPE_USER_DEFINED); __type->name_loc = _name; __type; })

UnaryOp unaryop_from_token(TokenType type);
TokenType unaryop_to_token(UnaryOp type);
PostUnaryOp post_unaryop_from_token(TokenType type);
TokenType postunaryop_to_token(PostUnaryOp type);
BinaryOp binaryop_from_token(TokenType type);
BinaryOp binaryop_assign_base_op(BinaryOp assign_binary_op);
TokenType binaryop_to_token(BinaryOp type);


Decl *struct_find_name(Decl *decl, const char* name);


static inline const char* struct_union_name_from_token(TokenType type)
{
	return type == TOKEN_STRUCT ? "struct" : "union";
}


void advance(Context *context);

// Useful sanity check function.
static inline void advance_and_verify(Context *context, TokenType token_type)
{
	assert(context->tok.type == token_type);
	advance(context);
}