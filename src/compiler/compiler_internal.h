#pragma once
// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "../utils/common.h"
#include "../utils/errors.h"
#include "../utils/lib.h"
#include "../build/build_options.h"
#include "compiler.h"
#include "enums.h"

typedef uint32_t SourceLoc;
#define INVALID_LOC UINT32_MAX
#define INVALID_RANGE ((SourceRange){ .loc = UINT32_MAX })
#define EMPTY_TOKEN ((Token) { .string = NULL })
#define MAX_LOCALS 0xFFFF
#define MAX_SCOPE_DEPTH 0xFF

typedef struct _Ast Ast;
typedef struct _Decl Decl;
typedef struct _Type Type;
typedef struct _Expr Expr;
typedef struct _Module Module;

typedef bool(*CastFunc)(Expr*, Type*, Type*, CastType cast_type);

typedef struct
{
	SourceLoc loc;
	uint32_t length;
} SourceRange;


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
	const char *name;
	const char *full_path;
	SourceLoc start_id;
	SourceLoc end_id;
} File;


typedef struct _Path
{
	Token package;
	Token module;
} Path;

struct _Type
{
	TypeKind type_kind : 6;
	ResolveStatus resolve_status : 2;
	Type *canonical;
	Token name_loc;
	Type **ptr_like_canonical;
	union
	{
		Decl *decl;
		struct
		{
			unsigned bitsize : 16;
			unsigned char bytesize;
		} builtin;
		struct
		{
			Path *path;
		} unresolved;
		Expr *unresolved_type_expr;
		struct
		{
			Type *base;
			union
			{
				Expr *unresolved_len;
				size_t len;
				bool nullable;
			};
		};
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
} ErrorDecl;

typedef struct
{
	ImportType type : 3;
	Token alias;
	Expr** generic_parameters;
	struct _Module *module;
} ImportDecl;

typedef struct
{
	Decl **members;
	Decl **method_functions;
} StructDecl;


typedef struct _VarDecl
{
	unsigned id : 16;
	VarDeclKind kind : 3;
	Type *type;
	Expr *init_expr;
} VarDecl;


typedef struct
{
	Expr *expr;
	Decl **then;
	Decl *elif;
} CtIfDecl;


typedef struct
{
	Decl *parent;
	Expr *expr;
} EnumConstantDecl;

typedef struct
{
	Decl *parent;
	uint32_t value;
} ErrorConstantDecl;

typedef struct
{
	Decl** values;
	Type *type;
} EnumDecl;


typedef struct
{
	bool variadic : 1;
	Type *rtype;
	Decl** params;
	Token *throws;
} FunctionSignature;

typedef struct
{
	Decl **vars;
} FuncAnnotations;

typedef struct
{
	const char *full_name;
	Type *struct_parent;
	FunctionSignature function_signature;
	Ast *body;
	FuncAnnotations *annotations;
} FuncDecl;

typedef struct
{
	AttributeDomains domains;
	FunctionSignature attr_signature;
} AttrDecl;

typedef struct
{
	bool is_func : 1;
	union
	{
		FunctionSignature function_signature;
		Type *type;
	};
} TypedefDecl;

typedef struct
{
	Decl **parameters;
	Type *rtype; // May be null!
	struct _Ast *body;
} MacroDecl;

typedef struct
{
	struct _Ast **cases;
	Token *parameters;
	Type *rtype; // May be null!
} GenericDecl;


typedef struct _Decl
{
	Token name;
	DeclKind decl_kind : 6;
	Visibility visibility : 2;
	ResolveStatus resolve_status : 2;
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
	Type *self_type;
	struct _Module *module;
	Attr** attributes;
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
		AttrDecl attr;
		TypedefDecl typedef_decl;
		Decl** multi_decl;
		MacroDecl macro_decl;
		GenericDecl generic_decl;
		CtIfDecl ct_if_decl;
		CtIfDecl ct_elif_decl;
		Decl** ct_else_decl;
		Expr *incr_array_decl;
	};
} Decl;

typedef struct
{
	Expr *expr;
	Expr *else_expr;
} ExprTry;

typedef struct
{
	Type *type;
	Token method;
} ExprMethodRef;

typedef struct
{
	Type *type;
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
	TokenType operator;
} ExprBinary;

typedef struct
{
	Expr *left;
	Expr *right;
	AssignOp operator;
} ExprAssign;

typedef struct
{
	Expr* expr;
	TokenType operator;
} ExprUnary;


typedef struct
{
	union
	{
		long double f;
		uint64_t i;
		bool b;
		struct
		{
			char* chars;
			int len;
		} string;
	};
	ConstType type : 3;
} ExprConst;

typedef struct
{
	bool is_struct_function;
	Expr *function;
	Expr **arguments;
} ExprCall;

typedef struct
{
	Expr *expr;
	Expr *index;
} ExprSubscript;

typedef struct
{
	Expr *parent;
	Token sub_element;
} ExprAccess;

typedef struct
{
	Path *path;
	Token identifier;
	bool is_ref;
	Decl *decl;
} ExprIdentifier;

typedef struct
{
	Type *type;
} ExprType;


typedef struct
{
	CastKind kind;
	Expr *expr;
	union
	{
		size_t truncated_size;
	};
} ExprCast;

struct _Expr
{
	ExprKind expr_kind : 8;
	ResolveStatus resolve_status : 3;
	Token loc;
	Type *type;

	union {
		Token* deferred_tokens;
		Token deferred_token;
		ExprCast expr_cast;
		ExprConst const_expr;
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
	};
};

typedef struct
{

} AstAttribute;


typedef struct
{
	struct _Ast **stmts;
//	DeferList defer_list; TODO
} AstCompoundStmt;

typedef struct
{
	uint16_t last_goto;
	bool is_used : 1;
	struct _Ast *defer;
	struct _Ast *in_defer;
} AstLabelStmt;

typedef struct
{
	Expr *expr; // May be NULL
	struct _Ast *defer;
} AstReturnStmt;

typedef struct
{
	Ast *cond;
	Ast *body;
} AstWhileStmt;

typedef struct
{
	Expr *expr;
	Ast *body;
} AstDoStmt;

typedef struct
{
	Ast *cond;
	Ast *then_body;
	Ast *else_body;
} AstIfStmt;


typedef struct
{
	union
	{
		Expr *expr;
		struct
		{
			uint64_t val;
			CaseValueType value_type : 3;
			bool has_next;
		};
	};
	Ast *block;
} AstCaseStmt;

typedef struct
{
	Ast *cond;
	Ast *body;
	Ast **cases;
} AstSwitchStmt;

typedef struct
{
	Ast *cond;
	Ast *incr;
	Ast *body;
} AstForStmt;


typedef struct
{
	Ast **stmts;
	Expr *expr;
} AstCondStmt;


typedef struct
{
	GotoType type : 2;
	Ast *label;
	struct _Ast *defer;
	union
	{
		struct _Ast *in_defer;
		struct _Ast *defer_end;
	};
} AstGotoStmt;

typedef struct _AstDeferStmt
{
	bool emit_boolean : 1;
	struct _Ast *body; // Compound statement
	struct _Ast *prev_defer;
} AstDeferStmt;

typedef struct _AstCatchStmt
{
	Decl *error_param;
	struct _Ast *body;
} AstCatchStmt;

typedef struct _AstCtIfStmt
{
	Expr *expr;
	struct _Ast *then;
	struct _Ast *elif;
} AstCtIfStmt;


typedef struct _AstGenericCaseStmt
{
	Type **types;
	struct _Ast *body;
} AstGenericCaseStmt;

typedef struct
{
	Expr *cond;
	Ast **body;
} AstCtSwitchStmt;

typedef struct
{
	Type **types;
	Ast *body;
} AstCtCaseStmt;

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
		struct _Ast *volatile_stmt;
		struct _Ast *try_stmt;
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
		Ast* ct_default_stmt;
		Ast* next_stmt;
		AstCatchStmt catch_stmt;
		AstGotoStmt goto_stmt;
		AstForStmt for_stmt;
		AstCondStmt cond_stmt;
		AstCtIfStmt ct_if_stmt;
		AstCtIfStmt ct_elif_stmt;
		Ast *ct_else_stmt;
		AstGenericCaseStmt generic_case_stmt;
		Ast *generic_default_stmt;
		Ast** stmt_list;
	};
} Ast;



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


typedef struct _DynamicScope
{
	ScopeFlags flags;
	ScopeFlags flags_created;
	unsigned errors;
	Decl **local_decl_start;
	Ast *defer_stack_start;
	Ast *active_defer;
	ExitType exit;
} DynamicScope;


typedef struct _Context
{
	Token module_name;
	Token* module_parameters;
	File * file;
	Decl** imports;
	Module *module;
	STable local_symbols;
	Decl **header_declarations;
	Decl **enums;
	Decl **types;
	Decl **functions;
	Decl **vars;
	Decl **ct_ifs;
	Decl *active_function_for_analysis;
	Type *left_type_in_assignment;
	FILE *codegen_output;
	Decl **last_local;
	Ast **labels;
	Ast **gotos;
	DynamicScope *current_scope;
	int unique_index;
	Decl *evaluating_macro;
	Type *rtype;
	int in_volatile_section;
	Decl *locals[MAX_LOCALS];
	DynamicScope scopes[MAX_SCOPE_DEPTH];
} Context;

extern Context *current_context;

extern Ast poisoned_ast;
extern Decl poisoned_decl;
extern Expr poisoned_expr;
extern Type poisoned_type;
extern Module poisoned_module;
extern Diagnostics diagnostics;

extern Token next_tok;
extern Token tok;

extern Type *type_bool, *type_void, *type_string;
extern Type *type_float, *type_double;
extern Type *type_char, *type_short, *type_int, *type_long, *type_isize;
extern Type *type_byte, *type_ushort, *type_uint, *type_ulong, *type_usize;
extern Type *type_compint, *type_compuint, *type_compfloat;
extern Type *type_c_short, *type_c_int, *type_c_long, *type_c_longlong;
extern Type *type_c_ushort, *type_c_uint, *type_c_ulong, *type_c_ulonglong;

extern Type t_i8, t_i16, t_i32, t_i64, t_isz, t_ixx;
extern Type t_u1, t_u8, t_u16, t_u32, t_u64, t_usz, t_uxx;
extern Type t_f32, t_f64, t_fxx;
extern Type t_u0, t_str;
extern Type t_cus, t_cui, t_cul, t_cull;
extern Type t_cs, t_ci, t_cl, t_cll;

#define AST_NEW(_kind, _token) new_ast(_kind, _token)

static inline bool ast_ok(Ast *ast) { return ast == NULL || ast->ast_kind != AST_POISONED; }
static inline bool ast_poison(Ast *ast) { ast->ast_kind = AST_POISONED; return false; }
static inline Ast *new_ast(AstKind kind, Token token)
{
	Ast *ast = malloc_arena(sizeof(Ast));
	memset(ast, 0, sizeof(Ast));
	ast->token = token;
	ast->ast_kind = kind;
	ast->exit = EXIT_NONE;
	return ast;
}


void builtin_setup();

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
		case TYPE_UXX:
			return true;
		default:
			return false;
	}
}

static inline ConstType sign_from_type(Type *type)
{
	assert(type->canonical == type);
	TODO //	return (type->type_kind >= TYPE_I8 && type->type_kind <= TYPE_IXX) ? CONST_INT : CONST_UINT;
}

bool cast(Expr *expr, Type *to_type, CastType cast_type);
bool cast_arithmetic(Expr *expr, Expr *other, const char *action);
bool cast_to_runtime(Expr *expr);


void codegen(Context *context);

bool sema_analyse_expr(Context *context, Expr *expr);

Context *context_create(File *file);
void context_push(Context *context);
void context_register_global_decl(Context *context, Decl *decl);
bool context_add_import(Context *context, Token module_name, Token alias, ImportType import_type, Expr** generic_parameters);
bool context_set_module_from_filename(Context *context);
bool context_set_module(Context *context, Token module_name, Token *generic_parameters);
void context_print_ast(Context *context, FILE *file);
Decl *context_find_ident(Context *context, const char *symbol);
void context_add_header_decl(Context *context, Decl *decl);
bool context_add_local(Context *context, Decl *decl);

Decl *decl_new(DeclKind decl_kind, Token name, Visibility visibility);
Decl *decl_new_user_defined_type(Token name, DeclKind decl_type, Visibility visibility);
Decl *decl_new_var(Token name, Type *type, VarDeclKind kind, Visibility visibility);
static inline bool decl_ok(Decl *decl) { return decl->decl_kind != DECL_POISONED; }
static inline bool decl_poison(Decl *decl) { decl->decl_kind = DECL_POISONED; decl->resolve_status = RESOLVE_DONE; return false; }
static inline bool decl_is_struct_type(Decl *decl) { return decl->decl_kind == DECL_UNION || decl->decl_kind == DECL_STRUCT; }
static inline DeclKind decl_from_token(TokenType type)
{
	if (type == TOKEN_STRUCT) return DECL_STRUCT;
	if (type == TOKEN_UNION) return DECL_UNION;
	UNREACHABLE
}

void diag_reset(void);
void diag_error_range(SourceRange span, const char *message, ...);
void diag_verror_range(SourceRange span, const char *message, va_list args);

#define EXPR_NEW_EXPR(_kind, _expr) expr_new(_kind, _expr->loc)
#define EXPR_NEW_TOKEN(_kind, _tok) expr_new(_kind, _tok)
Expr *expr_new(ExprKind kind, Token start);
static inline bool expr_ok(Expr *expr) { return expr == NULL || expr->expr_kind != EXPR_POISONED; }
static inline bool expr_poison(Expr *expr) { expr->expr_kind = EXPR_POISONED; expr->resolve_status = RESOLVE_DONE; return false; }
static inline void expr_replace(Expr *expr, Expr *replacement)
{
	Token loc = expr->loc;
	*expr = *replacement;
	expr->loc = loc;
}

void fprint_ast(FILE *file, Ast *ast);
void fprint_decl(FILE *file, Decl *dec);
void fprint_type_recursive(FILE *file, Type *type, int indent);
void fprint_expr_recursive(FILE *file, Expr *expr, int indent);


Token lexer_scan_token(void);
Token lexer_scan_ident_test(const char *scan);
void lexer_test_setup(const char *text, size_t len);
void lexer_add_file_for_lexing(File *file);
File* lexer_current_file(void);
void lexer_check_init(void);
void lexer_store_state(void);
void lexer_restore_state(void);

static inline void advance(void)
{
	tok = next_tok;
	while (1)
	{
		next_tok = lexer_scan_token();
		// printf(">>> %.*s => %s\n", tok.length, tok.start, token_type_to_string(tok.type));
		if (next_tok.type != TOKEN_INVALID_TOKEN) break;
	}
}

static inline void advance_and_verify(TokenType token_type)
{
	assert(tok.type == token_type);
	advance();
}

Decl *module_find_symbol(Module *module, const char *symbol);

void parse_file(File *file);

#define SEMA_ERROR(_tok, ...) sema_error_range(_tok.span, __VA_ARGS__)
void sema_init(File *file);
void sema_analysis(Context *context);

bool sema_analyse_statement(Context *context, Ast *statement);
bool sema_resolve_type(Context *context, Type *type);
bool sema_resolve_type_shallow(Context *context, Type *type);
void sema_error_at(SourceLoc loc, const char *message, ...);
void sema_error_range(SourceRange range, const char *message, ...);
void sema_verror_at(SourceLoc loc, const char *message, va_list args);
void sema_verror_range(SourceRange range, const char *message, va_list args);
void sema_error(const char *message, ...);
void sema_prev_at_range(SourceRange span, const char *message, ...);
void sema_prev_at(SourceLoc loc, const char *message, ...);
void sema_shadow_error(Decl *decl, Decl *old);

File *source_file_load(const char *filename, bool *already_loaded);
File *source_file_from_position(SourceLoc loc);


void stable_init(STable *table, uint32_t initial_size);
void *stable_set(STable *table, const char *key, void *value);
void *stable_get(STable *table, const char *key);
void *stable_delete(STable *table, const char *key);
void stable_clear(STable *table);

void symtab_init(uint32_t max_size);
const char *symtab_add(const char *symbol, uint32_t len, uint32_t fnv1hash, TokenType *type);

#define TOKEN_MAX_LENGTH 0xFFFF
#define TOK2VARSTR(_token) _token.span.length, _token.start
bool token_is_type(TokenType type);
const char *token_type_to_string(TokenType type);
static inline Token wrap(const char *string)
{
	return (Token) { .span = INVALID_RANGE, .type = TOKEN_IDENT, .string = string };
}

Type *type_new(TypeKind type_kind);
Type *type_get_canonical_ptr(Type *ptr_type);
Type *type_get_canonical_array(Type *arr_type);
Type *type_signed_int_by_size(int bitsize);
Type *type_unsigned_int_by_size(int bitsize);
bool type_is_subtype(Type *type, Type *possible_subtype);
const char *type_to_error_string(Type *type);
size_t type_size(Type *canonical);
static inline bool type_is_builtin(TypeKind kind) { return kind >= TYPE_VOID && kind <= TYPE_FXX; }
static inline bool type_is_signed(Type *type) { return type->type_kind >= TYPE_I8 && type->type_kind <= TYPE_IXX; }
static inline bool type_is_unsigned(Type *type) { return type->type_kind >= TYPE_U8 && type->type_kind <= TYPE_UXX; }
static inline bool type_ok(Type *type) { return !type || type->type_kind != TYPE_POISONED; }
static inline void type_poison(Type *type) { type->type_kind = TYPE_POISONED; type->resolve_status = RESOLVE_DONE; }
static inline bool type_is_integer(Type *type)
{
	assert(type == type->canonical);
	return type->type_kind >= TYPE_I8 && type->type_kind <= TYPE_UXX;
}

static inline bool type_is_number(Type *type)
{
	assert(type == type->canonical);
	return type->type_kind >= TYPE_I8 && type->type_kind <= TYPE_FXX;
}

#define TYPE_MODULE_UNRESOLVED(_module, _name) ({ Type *__type = type_new(TYPE_USER_DEFINED); \
 __type->name_loc = _name; __type->unresolved.module = _module; __type; })
#define TYPE_UNRESOLVED(_name) ({ Type *__type = type_new(TYPE_USER_DEFINED); __type->name_loc = _name; __type; })

AssignOp assignop_from_token(TokenType type);
UnaryOp unaryop_from_token(TokenType type);
Decl *struct_find_name(Decl *decl, const char* name);


static inline const char* struct_union_name_from_token(TokenType type)
{
	return type == TOKEN_STRUCT ? "struct" : "union";
}





