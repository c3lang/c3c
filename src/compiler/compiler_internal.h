#pragma once
#pragma clang diagnostic push
#pragma ide diagnostic ignored "bugprone-reserved-identifier"

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
#include "utils/malloc.h"

typedef uint64_t ByteSize;
typedef int64_t ArrayIndex;
typedef int32_t MemberIndex;
typedef int32_t AlignSize;
typedef int32_t ScopeId;



typedef uint32_t SourceLoc;
typedef struct
{
	unsigned index;
} TokenId;

#define NO_TOKEN_ID ((TokenId) { 0 })
#define NO_TOKEN ((Token) { .type = TOKEN_INVALID_TOKEN })
#define INVALID_TOKEN_ID ((TokenId) { UINT32_MAX })
#define INVALID_RANGE ((SourceSpan){ INVALID_TOKEN_ID, INVALID_TOKEN_ID })
#define MAX_LOCALS 0xFFFF
#define MAX_SCOPE_DEPTH 0xFF
#define MAX_PATH 1024
#define MAX_MACRO_NESTING 1024
#define MAX_FUNCTION_SIGNATURE_SIZE 2048
#define MAX_PARAMS 512
#define MAX_MEMBERS ((MemberIndex)(((uint64_t)2) << 28))
#define MAX_ALIGNMENT ((ArrayIndex)(((uint64_t)2) << 28))
#define MAX_OFFSET ((ArrayIndex)(((uint64_t)2) << 60))

typedef struct _Ast Ast;
typedef struct _Decl Decl;
typedef struct _TypeInfo TypeInfo;
typedef struct _Expr Expr;
typedef struct _Module Module;
typedef struct _Type Type;

typedef unsigned AstId;

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
		struct
		{
			long double i;
			long double r;
		} complex;
	};
	// Valid type kinds:
	// bool, ints, floats, string
	TypeKind kind;
} ExprConst;

typedef struct
{
	TokenId loc;
	TokenId end_loc;
} SourceSpan;

typedef struct
{
	AstId start;
	AstId end;
} DeferList;


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
	unsigned token_start_id;
} File;

typedef struct
{
	File *file;
	uint32_t line;
	uint32_t col;
	uint32_t start;
	uint32_t length;
} SourceLocation;


typedef struct
{
	TokenId id;
	TokenType type : 8;
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
	File *file;
	uint32_t line;
	uint32_t col;
	SourceLoc loc;
	const char *start;
} SourcePosition;



typedef struct _Path
{
	SourceSpan span;
	const char *module;
	uint32_t len;
} Path;


typedef struct
{
	unsigned char bitsize;
	unsigned char bytesize;
	unsigned char abi_alignment;
	unsigned char pref_alignment;
}  TypeBuiltin;

typedef struct
{
	TokenId name_loc;
	Path *path;
} TypeUnresolved;

typedef struct
{
	Type *base;
	ByteSize len;
} TypeArray;

typedef struct
{
	Type *base;
	ByteSize len;
} TypeVector;

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
		// Error, Struct, Union, Typedef, Member
		Decl *decl;
		// int, float, bool
		TypeBuiltin builtin;
		// Type[], Type[*], Type[123]
		TypeArray array;
		// func Type1(Type2, Type3, ...) throws Err1, Err2, ...
		TypeFunc func;
		// Type*
		Type *pointer;
		// Type[<123>] or Type<[123]>
		TypeVector vector;
		Type *complex;
	};
};

struct _TypeInfo
{
	ResolveStatus resolve_status : 2;
	Type *type;
	TypeInfoKind kind;
	SourceSpan span;
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
	TokenId name;
	union
	{
		Expr *expr;
		uint32_t alignment;
	};
} Attr;

typedef struct
{
	Path *path;
	TokenId symbol;
	bool aliased;
} ImportDecl;

typedef struct
{
	uint64_t size;
	Decl **members;
	MemberIndex union_rep;
} StructDecl;


typedef struct _VarDecl
{
	VarDeclKind kind : 4;
	bool constant : 1;
	bool failable : 1;
	bool unwrap : 1;
	TypeInfo *type_info;
	union
	{
		Expr *init_expr;
		Decl *alias;
	};
	union
	{
		void *backend_debug_ref;
		void *scope;
	};
	union
	{
		void *failable_ref;
		struct ABIArgInfo_ *abi_info;
	};
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
	Decl **cases;
} CtSwitchDecl;

typedef struct
{
	TypeInfo *type;
	Decl **body;
} CtCaseDecl;

typedef struct
{
	Expr *expr;
	Expr **args;
	uint64_t ordinal;
} EnumConstantDecl;


typedef struct
{
	Decl** values;
	Decl** parameters;
	TypeInfo *type_info;
} EnumDecl;

typedef struct _FunctionSignature
{
	CallConvention convention : 4;
	bool variadic : 1;
	bool has_default : 1;
	bool failable : 1;
	TypeInfo *rtype;
	struct ABIArgInfo_ *ret_abi_info;
	struct ABIArgInfo_ *failable_abi_info;
	Decl** params;
	const char *mangled_signature;
} FunctionSignature;

typedef struct
{
	Decl **vars;
} FuncAnnotations;

typedef struct
{
	struct
	{
		bool is_builtin;
		bool attr_weak : 1;
		bool attr_noreturn : 1;
		bool attr_inline : 1;
		bool attr_noinline : 1;
		bool attr_cname : 1;
		bool attr_stdcall : 1;
	};

	TypeInfo *type_parent;
	FunctionSignature function_signature;
	Ast *body;
	FuncAnnotations *annotations;
	Decl **locals;
	Ast **labels;
} FuncDecl;

typedef struct
{
	AttributeDomain domains;
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
	bool failable : 1;
	Decl **parameters;
	TypeInfo *rtype; // May be null!
	struct _Ast *body;
} MacroDecl;

typedef struct
{
	struct _Ast **cases;
	TokenId *parameters;
	TypeInfo *rtype; // May be null!
	Path *path; // For redefinition
} GenericDecl;

typedef struct
{
	Path *path;
	Expr **params;
	Token alias;
} DefineDecl;

typedef struct
{
	AstId defer;
	bool next_target : 1;
	void *break_target;
	void *continue_target;
	ScopeId scope_id;
	AstId parent;
} LabelDecl;


typedef struct _Decl
{
	const char *name;
	TokenId name_token;
	SourceSpan span;
	const char *external_name;
	DeclKind decl_kind : 6;
	Visibility visibility : 2;
	ResolveStatus resolve_status : 2;
	bool is_packed : 1;
	bool is_opaque : 1;
	bool needs_additional_pad : 1;
	bool is_substruct : 1;
	void *backend_ref;
	const char *cname;
	AlignSize alignment;
	const char *section;
	ArrayIndex offset;
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
				Decl** methods;
			};
			union
			{
				// Unions, Errtype and Struct use strukt
				StructDecl strukt;
				EnumDecl enums;
			};
		};
		ImportDecl import;
		VarDecl var;
		LabelDecl label;
		EnumConstantDecl enum_constant;
		FuncDecl func;
		AttrDecl attr;
		TypedefDecl typedef_decl;
		MacroDecl macro_decl;
		GenericDecl generic_decl;
		DefineDecl define_decl;
		CtIfDecl ct_if_decl;
		CtIfDecl ct_elif_decl;
		CtSwitchDecl ct_switch_decl;
		CtCaseDecl ct_case_decl;
		Decl** ct_else_decl;
		Expr *incr_array_decl;
	};
} Decl;



typedef struct
{
	bool is_jump : 1;
	Expr *expr;
	union
	{
		Expr *else_expr;
		Ast *else_stmt;
	};
} ExprElse;


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
	bool left_maybe : 1;
	bool right_maybe : 1;
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




typedef struct
{
	bool is_struct_function : 1;
	bool is_pointer_call : 1;
	Expr *function;
	Expr **arguments;
} ExprCall;

typedef struct
{
	bool from_back : 1;
	Expr *expr;
	Expr *index;
} ExprSubscript;

typedef struct
{
	Expr *expr;
	bool start_from_back : 1;
	bool end_from_back : 1;
	Expr *start;
	Expr *end;
} ExprSlice;

typedef struct
{
	Expr *left;
	Expr *right;
} ExprSliceAssign;


typedef struct
{
	Expr *parent;
	union
	{
		TokenId sub_element;
		Decl *ref;
	};
} ExprAccess;

typedef struct DesignatorElement_
{
	DesignatorType kind : 4;
	union
	{
		const char *field;
		struct
		{
			Expr *index_expr;
			Expr *index_end_expr;
		};
	};
	ArrayIndex index;
	ArrayIndex index_end;
} DesignatorElement;

typedef enum
{
	CONST_INIT_ZERO,
	CONST_INIT_EXPANDED,
	CONST_SELECTED,
	CONST_VALUE,
	CONST_INIT_ARRAY_SPLIT,
	CONST_INIT_ARRAY_RANGE_ZERO,
	CONST_INIT_ARRAY_VALUE_FRAGMENT
} ConstInitType;


typedef struct ConstInitializer_
{
	Type *type;
	ConstInitType kind;
	union
	{
		struct ConstInitializer_ **elements;
		Expr *value;
		struct
		{
			struct ConstInitializer_ *element;
			MemberIndex index;
		} union_const;
		struct
		{
			struct ConstInitializer_ *low;
			struct ConstInitializer_ *mid;
			struct ConstInitializer_ *hi;
		} split_const;
		struct
		{
			ArrayIndex low;
			ArrayIndex high;
		} array_range_zero;
		struct
		{
			struct ConstInitializer_ *element;
			ArrayIndex index;
		} single_array_index;
	};
} ConstInitializer;

typedef struct
{
	DesignatorElement **path;
	Expr *value;
} ExprDesignator;

typedef struct
{
	Path *path;
	const char *identifier;
	bool is_ref : 1;
	bool is_rvalue : 1;
	Decl *decl;
} ExprIdentifier;

typedef struct
{
	const char *identifier;
	bool is_ref : 1;
	bool is_rvalue : 1;
	Decl *decl;
} ExprIdentifierRaw;

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
	Ast **stmts;
	Expr **args;
	Decl **params;
} ExprMacroBlock;

typedef struct
{
	Expr *left;
	Expr *right;
} ExprRange;

typedef enum
{
	INITIALIZER_UNKNOWN,
	INITIALIZER_DESIGNATED,
	INITIALIZER_NORMAL,
	INITIALIZER_CONST,
} InitializerType;

typedef struct
{
	InitializerType init_type;
	union
	{
		Expr** initializer_expr;
		ConstInitializer *initializer;
	};
} ExprInitializer;

typedef struct
{
	Expr *initializer;
	TypeInfo *type_info;
} ExprCompoundLiteral;

typedef struct
{
	const char *name;
	TokenId span;
} Label;

typedef struct
{
	Expr *inner;
	AstId defer;
} ExprGuard;

typedef struct
{
	Expr *inner;
} ExprLen;


struct _Expr
{
	ExprKind expr_kind : 8;
	ResolveStatus resolve_status : 3;
	bool failable : 1;
	bool pure : 1;
	bool constant : 1;
	bool reeval : 1;
	SourceSpan span;
	Type *type;
	union {
		Expr *group_expr;
		ExprLen len_expr;
		ExprCast cast_expr;
		Expr *typeof_expr;
		TypeInfo *type_expr;
		ExprConst const_expr;
		ExprRange range_expr;
		ExprStructValue struct_value_expr;
		ExprGuard guard_expr;
		Expr *trycatch_expr;
		ExprElse else_expr;
		ExprSliceAssign slice_assign_expr;
		ExprBinary binary_expr;
		ExprTernary ternary_expr;
		ExprUnary unary_expr;
		ExprPostUnary post_expr;
		ExprCall call_expr;
		ExprSlice slice_expr;
		ExprSubscript subscript_expr;
		ExprAccess access_expr;
		ExprDesignator designator_expr;
		ExprIdentifier identifier_expr;
		ExprIdentifier macro_identifier_expr;
		ExprIdentifierRaw ct_ident_expr;
		ExprIdentifierRaw ct_macro_ident_expr;
		ExprIdentifierRaw hash_ident_expr;
		TypeInfo *typeid_expr;
		ExprInitializer initializer_expr;
		Decl *expr_enum;
		ExprCompoundLiteral expr_compound_literal;
		Expr** expression_list;
		ExprScope expr_scope;
		ExprFuncBlock expr_block;
		ExprMacroBlock macro_block;
		Expr* failable_expr;
		Ast** dexpr_list_expr;
	};
};


typedef struct
{
	struct _Ast **stmts;
	DeferList defer_list;
} AstCompoundStmt;


typedef struct
{
	Expr *expr; // May be NULL
	AstId defer;
} AstReturnStmt;

typedef struct
{
	bool has_break : 1;
	bool no_exit : 1;
	Decl *label;
} FlowCommon;

typedef struct
{
	FlowCommon flow;
	Expr *cond;
	Ast *body;
	void *break_block;
	void *continue_block;
} AstWhileStmt;

typedef struct
{
	FlowCommon flow;
	DeferList expr_defer;
	DeferList body_defer;
	union
	{
		struct
		{
			void *break_block;
			void *continue_block;
		};
		struct
		{
			Expr *expr;
			Ast *body;
		};
	};
} AstDoStmt;

typedef struct
{
	FlowCommon flow;
	Expr *cond;
	Ast *then_body;
	Ast *else_body;
	void *break_block;
} AstIfStmt;


typedef struct
{
	bool is_type;
	union
	{
		TypeInfo *type_info;
		Expr *expr;
	};
	Ast *body;
	void *backend_block;
} AstCaseStmt;


typedef struct
{
	FlowCommon flow;
	AstId defer;
	Expr *cond;
	Ast **cases;
	union
	{
		struct
		{
			ScopeId scope_id;
		};
		struct
		{
			void *retry_block;
			void *exit_block;
			void *retry_var;
		} codegen;
	};
} AstSwitchStmt;

typedef struct
{
	FlowCommon flow;
	Expr *init;
	Expr *cond;
	Expr *incr;
	Ast *body;
	void *continue_block;
	void *exit_block;
} AstForStmt;

typedef struct
{
	AstId prev_defer;
	Ast *body; // Compound statement
	struct
	{
		void *exit_block;
	} codegen;
} AstDeferStmt;

typedef struct
{
	FlowCommon flow;
	bool is_switch : 1;
	bool has_err_var : 1;
	ScopeId scope_id;
	AstId defer;
	union
	{
		Expr *catchable;
		Decl *err_var;
	};
	union
	{
		Ast *body;
		Ast **cases;
	};
	void *block;
} AstCatchStmt;


typedef struct _AstCtIfStmt
{
	Expr *expr;
	struct _Ast *then;
	struct _Ast *elif;
} AstCtIfStmt;


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
	TokenId index;
	TokenId value;
	Expr *expr;
	Ast *body;
} AstCtForStmt;

typedef struct
{
	bool is_label;
	union
	{
		Label label;
		AstId ast;
	};
	DeferList defers;
} AstContinueBreakStmt;

typedef struct
{
	DeferList defers;
	union
	{
		struct
		{
			Label label;
			bool is_type;
			union
			{
				Expr *target;
				TypeInfo *type_info;
			};
		};
		struct
		{
			AstId case_switch_stmt;
			Expr *switch_expr;
		};
	};
} AstNextStmt;


typedef struct
{
	Expr *expr;
	TokenId alias;
	TokenId constraints;
} AsmOperand;

typedef struct
{
	Expr *decl_expr;
	Ast *body;
} AstTryStmt;

typedef struct
{
	AsmOperand *inputs;
	AsmOperand *outputs;
	TokenId **clobbers;
	TokenId **labels;
} AsmParams;

typedef struct
{
	bool is_volatile : 1;
	bool is_inline : 1;
	bool is_goto : 1;
	AsmParams *params;
	TokenId **instructions;
} AstAsmStmt;

typedef struct
{
	Expr *message;
	Expr *expr;
} AstAssertStmt;

typedef struct _Ast
{
	SourceSpan span;
	AstKind ast_kind : 8;
	union
	{
		FlowCommon flow;                // Shared struct
		AstAsmStmt asm_stmt;            // 24
		AstCompoundStmt compound_stmt;  // 16
		Ast** ct_compound_stmt;
		Decl *declare_stmt;             // 8
		Expr *expr_stmt;                // 8
		AstTryStmt try_stmt;
		Decl *define_stmt;              // 8
		Ast *volatile_stmt;             // 8
		AstReturnStmt return_stmt;      // 16
		AstWhileStmt while_stmt;        // 24
		AstDoStmt do_stmt;              // 32
		AstIfStmt if_stmt;              // 32
		AstDeferStmt defer_stmt;        // 32
		AstSwitchStmt switch_stmt;      // 24
		AstCaseStmt case_stmt;          // 32
		AstCtSwitchStmt ct_switch_stmt; // 16
		AstContinueBreakStmt contbreak_stmt; // 8
		AstNextStmt next_stmt;              // 16
		AstCatchStmt catch_stmt;            // 32
		AstForStmt for_stmt;                // 32
		AstCtIfStmt ct_if_stmt;             // 24
		AstCtIfStmt ct_elif_stmt;           // 24
		Ast *ct_else_stmt;                  // 8
		AstCtForStmt ct_for_stmt;           // 64
		AstScopedStmt scoped_stmt;          // 16
		AstAssertStmt ct_assert_stmt;
		AstAssertStmt assert_stmt;
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
	STable symbols;
	STable public_symbols;
	Module **sub_modules;
} Module;



typedef struct _DynamicScope
{
	ScopeId scope_id;
	bool allow_dead_code : 1;
	bool jump_end : 1;
	ScopeFlags flags;
	Decl **local_decl_start;
	AstId defer_last;
	Ast *in_defer;
} DynamicScope;


typedef union
{
	const char *string;
	long double value;
	struct
	{
		union
		{
			uint8_t b[8];
			uint8_t u8;
			uint16_t u16;
			uint32_t u32;
			uint64_t u64;
		} char_lit;
		char width;
	};
} TokenData;

typedef struct
{
	uint32_t lexer_index;
	const char *file_begin;
	const char *lexing_start;
	const char *current;
	uint16_t source_file;
	uint32_t current_line;
	const char *line_start;
	File *current_file;
	SourceLoc last_in_range;
} Lexer;


typedef struct _Context
{
	BuildTarget *target;
	Path *module_name;
	TokenId* module_parameters;
	File* file;
	Decl** imports;
	Module *module;
	STable local_symbols;
	Decl **global_decls;
	Decl **enums;
	Decl **types;
	Decl **generic_defines;
	Decl **functions;
	Decl **macros;
	Decl **methods;
	Decl **vars;
	Decl **incr_array;
	Decl **ct_ifs;
	Ast **ct_asserts;
	Decl *active_function_for_analysis;
	Token *comments;
	Token *lead_comment;
	Token *trailing_comment;
	Token *next_lead_comment;
	ScopeId scope_id;
	AstId break_target;
	AstId break_defer;
	AstId continue_target;
	AstId continue_defer;
	AstId next_target;
	Ast *next_switch;
	AstId next_defer;
	DynamicScope *current_scope;
	struct
	{
		Type *expected_block_type;
		Ast **returns;
		bool expr_failable_return;
		// Reusable returns cache.
		Ast **returns_cache;
	};
	Decl *evaluating_macro;
	Type *rtype;
	bool failable_return;
	int in_volatile_section;
	struct
	{
		Decl **macro_locals_start;
		int macro_counter;
		int macro_nesting;
	};
	Decl* locals[MAX_LOCALS];
	Decl **last_local;
	DynamicScope scopes[MAX_SCOPE_DEPTH];
	char path_scratch[MAX_PATH];
	struct {
		STable external_symbols;
		Decl **external_symbol_list;
	};
	STable scratch_table;
	Lexer lexer;
	Token tok;
	TokenId prev_tok;
	Token next_tok;
} Context;

typedef struct
{
	STable modules;
	STable global_symbols;
	STable qualified_symbols;
	Type **type;
	const char *lib_dir;
} Compiler;

typedef enum
{
	MODULE_SYMBOL_SEARCH_EXTERNAL,
	MODULE_SYMBOL_SEARCH_PARENT,
	MODULE_SYMBOL_SEARCH_THIS
} ModuleSymbolSearch;

typedef enum
{
	ABI_ARG_IGNORE,
	ABI_ARG_DIRECT_PAIR,
	ABI_ARG_DIRECT_COERCE,
	ABI_ARG_EXPAND_COERCE,
	ABI_ARG_INDIRECT,
	ABI_ARG_EXPAND,
}  ABIKind;

typedef enum
{
	ABI_TYPE_PLAIN,
	ABI_TYPE_INT_BITS
} AbiTypeKind;

typedef struct
{
	AbiTypeKind kind : 2;
	union
	{
		Type *type;
		uint32_t int_bits;
	};
} AbiType;

typedef struct ABIArgInfo_
{
	MemberIndex param_index_start : 16;
	MemberIndex param_index_end : 16;
	ABIKind kind : 6;
	struct
	{
		bool by_reg : 1;
		bool zeroext : 1;
		bool signext : 1;
	} attributes;
	union
	{
		struct
		{
			bool padding_by_reg : 1;
			Type *padding_type;
		} expand;
		struct
		{
			AbiType *lo;
			AbiType *hi;
		} direct_pair;
		struct
		{
			uint8_t offset_lo;
			uint8_t padding_hi;
			uint8_t lo_index;
			uint8_t hi_index;
			uint8_t offset_hi;
			bool packed : 1;
			AbiType *lo;
			AbiType *hi;
		} coerce_expand;
		struct
		{
			AbiType *partial_type;
		};
		struct
		{
			AbiType *type;
			uint8_t elements : 3;
			bool prevent_flatten : 1;
		} direct_coerce;
		struct
		{
			// We may request a certain alignment of the parameters.
			AlignSize realignment;
			bool by_val : 1;
		} indirect;
	};

} ABIArgInfo;

extern Compiler compiler;
extern Ast *poisoned_ast;
extern Decl *poisoned_decl;
extern Expr *poisoned_expr;
extern Type *poisoned_type;
extern TypeInfo *poisoned_type_info;
extern Diagnostics diagnostics;


extern Type *type_bool, *type_void, *type_string, *type_voidptr;
extern Type *type_half, *type_float, *type_double, *type_quad;
extern Type *type_char, *type_short, *type_int, *type_long, *type_isize;
extern Type *type_byte, *type_ushort, *type_uint, *type_ulong, *type_usize;
extern Type *type_u128, *type_i128;
extern Type *type_compint, *type_compfloat;
extern Type *type_c_short, *type_c_int, *type_c_long, *type_c_longlong;
extern Type *type_c_ushort, *type_c_uint, *type_c_ulong, *type_c_ulonglong;
extern Type *type_typeid, *type_error, *type_typeinfo;

extern const char *attribute_list[NUMBER_OF_ATTRIBUTES];

extern const char *kw_main;
extern const char *kw_sizeof;
extern const char *kw_alignof;
extern const char *kw_align;
extern const char *kw_offsetof;
extern const char *kw_kindof;
extern const char *kw_nameof;
extern const char *kw_qnameof;
extern const char *kw_len;
extern const char *kw_inline;
extern const char *kw_ordinal;
extern const char *kw___round;
extern const char *kw___ceil;
extern const char *kw___trunc;
extern const char *kw___sqrt;

#define AST_NEW_TOKEN(_kind, _token) new_ast(_kind, source_span_from_token_id(_token.id))
#define AST_NEW(_kind, _loc) new_ast(_kind, _loc)

ARENA_DEF(ast, Ast);
ARENA_DEF(expr, Expr);
ARENA_DEF(sourceloc, SourceLocation);
ARENA_DEF(toktype, char);
ARENA_DEF(tokdata, TokenData);
ARENA_DEF(decl, Decl);
ARENA_DEF(type_info, TypeInfo);

static inline bool ast_ok(Ast *ast) { return ast == NULL || ast->ast_kind != AST_POISONED; }
static inline bool ast_poison(Ast *ast) { ast->ast_kind = AST_POISONED; return false; }

static inline Ast *new_ast(AstKind kind, SourceSpan range)
{
	Ast *ast = ast_calloc();
	ast->span = range;
	ast->ast_kind = kind;
	return ast;
}



static inline Ast *extend_ast_with_prev_token(Context *context, Ast *ast)
{
	ast->span.end_loc = context->prev_tok;
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



bool cast_implicit(Context *context, Expr *expr, Type *to_type);
bool cast(Context *context, Expr *expr, Type *to_type, CastType cast_type);
CastKind cast_to_bool_kind(Type *type);

bool cast_implicitly_to_runtime(Context *context, Expr *expr);

void llvm_codegen(void *module);
void *llvm_gen(Context *context);
void llvm_codegen_setup();

void header_gen(Context *context);

void compiler_add_type(Type *type);
Decl *compiler_find_symbol(const char *name);
Module *compiler_find_or_create_module(Path *module_name);
void compiler_register_public_symbol(Decl *decl);

Context *context_create(File *file, BuildTarget *target);
void context_register_global_decl(Context *context, Decl *decl);
void context_register_external_symbol(Context *context, Decl *decl);
bool context_add_import(Context *context, Path *path, Token symbol, Token alias);
bool context_set_module_from_filename(Context *context);
bool context_set_module(Context *context, Path *path, TokenId *generic_parameters);
void context_print_ast(Context *context, FILE *file);

#pragma mark --- Decl functions

Decl *decl_new(DeclKind decl_kind, TokenId name, Visibility visibility);
Decl *decl_new_with_type(TokenId name, DeclKind decl_type, Visibility visibility);
Decl *decl_new_var(TokenId name, TypeInfo *type, VarDeclKind kind, Visibility visibility);
#define DECL_NEW(_kind, _vis) decl_new(_kind, context->tok.id, _vis)
#define DECL_NEW_WITH_TYPE(_kind, _vis) decl_new_with_type(context->tok.id, _kind, _vis)
#define DECL_NEW_VAR(_type, _kind, _vis) decl_new_var(context->tok.id, _type, _kind, _vis)
void decl_set_external_name(Decl *decl);
const char *decl_var_to_string(VarDeclKind kind);
static inline Decl *decl_raw(Decl *decl);
static inline bool decl_ok(Decl *decl) { return !decl || decl->decl_kind != DECL_POISONED; }
static inline bool decl_poison(Decl *decl) { decl->decl_kind = DECL_POISONED; decl->resolve_status = RESOLVE_DONE; return false; }
static inline bool decl_is_struct_type(Decl *decl);
AlignSize decl_abi_alignment(Decl *decl);
static inline DeclKind decl_from_token(TokenType type);

#pragma mark --- Diag functions

void diag_reset(void);

void diag_verror_range(SourceLocation *location, const char *message, va_list args);

#define EXPR_NEW_EXPR(_kind, _expr) expr_new(_kind, _expr->span)
#define EXPR_NEW_TOKEN(_kind, _tok) expr_new(_kind, source_span_from_token_id(_tok.id))
Expr *expr_new(ExprKind kind, SourceSpan start);
static inline bool expr_ok(Expr *expr) { return expr == NULL || expr->expr_kind != EXPR_POISONED; }
static inline bool expr_poison(Expr *expr) { expr->expr_kind = EXPR_POISONED; expr->resolve_status = RESOLVE_DONE; return false; }
static inline void expr_replace(Expr *expr, Expr *replacement)
{
	SourceSpan loc = expr->span;
	*expr = *replacement;
	expr->span = loc;
}
void expr_const_set_int(ExprConst *expr, uint64_t v, TypeKind kind);
void expr_const_set_float(ExprConst *expr, long double d, TypeKind kind);
void expr_const_set_bool(ExprConst *expr, bool b);
void expr_const_set_null(ExprConst *expr);
void expr_const_fprint(FILE *__restrict file, ExprConst *expr);
bool expr_const_int_overflowed(const ExprConst *expr);
bool expr_const_compare(const ExprConst *left, const ExprConst *right, BinaryOp op);
bool expr_is_constant_eval(Expr *expr);
const char *expr_const_to_error_string(const ExprConst *expr);

void fprint_decl(Context *context, FILE *file, Decl *dec);
void fprint_type_info_recursive(Context *context, FILE *file, TypeInfo *type_info, int indent);
void fprint_expr_recursive(Context *context, FILE *file, Expr *expr, int indent);


#pragma mark --- Lexer functions


Token lexer_advance(Lexer *lexer);
bool lexer_scan_ident_test(Lexer *lexer, const char *scan);
void lexer_init_for_test(Lexer *lexer, const char *text, size_t len);
void lexer_init_with_file(Lexer *lexer, File *file);
File* lexer_current_file(Lexer *lexer);

static inline SourceLocation *TOKILOC(TokenId token) { return sourcelocptr(token.index); }
static inline SourceLocation *TOKKLOC(Token token) { return sourcelocptr(token.id.index); }
static inline TokenData *tokendata_from_id(TokenId token) { return tokdataptr(token.index); }

#define TOKLOC(T) _Generic((T), TokenId: TOKILOC, Token: TOKKLOC)(T)

static inline const char *TOKISTR(TokenId token) { return tokendata_from_id(token)->string; }
static inline const char *TOKKSTR(Token token) { return tokendata_from_id(token.id)->string; }
#define TOKSTR(T) _Generic((T), TokenId: TOKISTR, Token: TOKKSTR)(T)

static inline double TOKIREAL(TokenId token) { return tokendata_from_id(token)->value; }
static inline double TOKKREAL(Token token) { return tokendata_from_id(token.id)->value; }
#define TOKREAL(T) _Generic((T), TokenId: TOKIREAL, Token: TOKKREAL)(T)

static inline TokenType TOKITYPE(TokenId token) { return toktypeptr(token.index)[0]; }
static inline TokenType TOKKTYPE(Token token) { return toktypeptr(token.id.index)[0]; }
#define TOKTYPE(T) _Generic((T), TokenId: TOKITYPE, Token: TOKKTYPE)(T)

static inline uint32_t TOKILEN(TokenId token) { return TOKILOC(token)->length; }
static inline uint32_t TOKKLEN(Token token) { return TOKKLOC(token)->length; }
#define TOKLEN(T) _Generic((T), TokenId: TOKILEN, Token:TOKKLEN)(T)

#define TOKVALID(_tok) (_tok.index != 0)
Decl *module_find_symbol(Module *module, const char *symbol, ModuleSymbolSearch search, Decl **private_decl);

void parse_file(Context *context);
Path *path_create_from_string(Context *context, const char *string, size_t len, SourceSpan span);
Path *path_find_parent_path(Context *context, Path *path);

const char *resolve_status_to_string(ResolveStatus status);

#define SEMA_TOKEN_ERROR(_tok, ...) sema_error_range3(source_span_from_token_id(_tok.id), __VA_ARGS__)
#define SEMA_TOKID_ERROR(_tok_id, ...) sema_error_range3(source_span_from_token_id(_tok_id), __VA_ARGS__)
#define SEMA_ERROR(_node, ...) sema_error_range3((_node)->span, __VA_ARGS__)
#define SEMA_PREV(_node, ...) sema_prev_at_range3((_node)->span, __VA_ARGS__)

void sema_analysis_pass_process_imports(Context *context);
void sema_analysis_pass_register_globals(Context *context);
void sema_analysis_pass_conditional_compilation(Context *context);
void sema_analysis_pass_decls(Context *context);
void sema_analysis_pass_ct_assert(Context *context);
void sema_analysis_pass_functions(Context *context);

bool sema_add_member(Context *context, Decl *decl);
bool sema_add_local(Context *context, Decl *decl);
bool sema_unwrap_var(Context *context, Decl *decl);
bool sema_rewrap_var(Context *context, Decl *decl);

bool sema_analyse_expr_of_required_type(Context *context, Type *to, Expr *expr, bool may_be_failable);
bool sema_analyse_expr(Context *context, Type *to, Expr *expr);
bool sema_analyse_decl(Context *context, Decl *decl);
bool sema_analyse_ct_assert_stmt(Context *context, Ast *statement);
bool sema_analyse_statement(Context *context, Ast *statement);
bool sema_expr_analyse_assign_right_side(Context *context, Expr *expr, Type *left_type, Expr *right, ExprFailableStatus lhs_is_failable);

Decl *sema_resolve_symbol_in_current_dynamic_scope(Context *context, const char *symbol);
Decl *sema_resolve_symbol(Context *context, const char *symbol, Path *path, Decl **ambiguous_other_decl, Decl **private_decl);
bool sema_resolve_type_info(Context *context, TypeInfo *type_info);
bool sema_resolve_type_shallow(Context *context, TypeInfo *type_info);

void sema_error_at_prev_end(Token token, const char *message, ...);

void sema_error_range3(SourceSpan span, const char *message, ...);

void sema_verror_range(SourceLocation *location, const char *message, va_list args);
void sema_error(Context *context, const char *message, ...);
void sema_prev_at_range3(SourceSpan span, const char *message, ...);
void sema_shadow_error(Decl *decl, Decl *old);

File *source_file_load(const char *filename, bool *already_loaded);
void source_file_append_line_end(File *file, SourceLoc loc);
SourcePosition source_file_find_position_in_file(File *file, SourceLoc loc);

static inline SourceSpan source_span_from_token_id(TokenId id)
{
	return (SourceSpan) { id, id };
}


#define RANGE_EXTEND_PREV(x) ((x)->span.end_loc = context->prev_tok)

void stable_init(STable *table, uint32_t initial_size);
void *stable_set(STable *table, const char *key, void *value);
void *stable_get(STable *table, const char *key);
void *stable_delete(STable *table, const char *key);
void stable_clear(STable *table);

const char *symtab_add(const char *symbol, uint32_t len, uint32_t fnv1hash, TokenType *type);

void target_setup(void);
int target_alloca_addr_space();
void *target_data_layout();
void *target_machine();
void *target_target();

#define TOK2VARSTR(_token) _token.span.length, _token.start
bool token_is_type(TokenType type);
bool token_is_any_type(TokenType type);
bool token_is_symbol(TokenType type);
const char *token_type_to_string(TokenType type);

AlignSize type_abi_alignment(Type *type);
AlignSize type_alloca_alignment(Type *type);
void type_append_signature_name(Type *type, char *dst, size_t *offset);
static inline bool type_convert_will_trunc(Type *destination, Type *source);
Type *type_find_common_ancestor(Type *left, Type *right);
Type *type_find_largest_union_element(Type *type);
Type *type_find_max_type(Type *type, Type *other);
Type *type_abi_find_single_struct_element(Type *type);
const char *type_generate_qname(Type *type);
Type *type_get_array(Type *arr_type, uint64_t len);
Type *type_get_indexed_type(Type *type);
Type *type_get_ptr(Type *ptr_type);
Type *type_get_subarray(Type *arr_type);
Type *type_get_vararray(Type *arr_type);
Type *type_get_vector(Type *vector_type, unsigned len);
Type *type_int_signed_by_bitsize(unsigned bytesize);
Type *type_int_unsigned_by_bitsize(unsigned bytesize);
bool type_is_abi_aggregate(Type *type);
static inline bool type_is_any_integer(Type *type);
static inline bool type_is_builtin(TypeKind kind);
static inline bool type_is_ct(Type *type);
bool type_is_empty_union_struct(Type *type, bool allow_array);
bool type_is_empty_field(Type *type, bool allow_array);
static inline bool type_is_float(Type *type);
bool type_is_homogenous_aggregate(Type *type, Type **base, unsigned *elements);
bool type_is_int128(Type *type);
static inline bool type_is_integer(Type *type);
static inline bool type_is_integer_unsigned(Type *type);
static inline bool type_is_integer_signed(Type *type);
static inline bool type_is_integer_kind(Type *type);
static inline bool type_is_numeric(Type *type);
static inline bool type_is_pointer(Type *type);
static inline bool type_is_promotable_float(Type *type);
static inline bool type_is_promotable_integer(Type *type);
static inline bool type_is_signed(Type *type);
static inline bool type_is_structlike(Type *type);
static inline size_t type_min_alignment(size_t a, size_t b);
bool type_is_subtype(Type *type, Type *possible_subtype);
bool type_is_union_struct(Type *type);
bool type_is_user_defined(Type *type);
static inline Type *type_lowering(Type *type);
bool type_may_have_sub_elements(Type *type);
static inline bool type_ok(Type *type);
static inline Type *type_reduced_from_expr(Expr *expr);
ByteSize type_size(Type *type);
const char *type_to_error_string(Type *type);

static inline TypeInfo *type_info_new(TypeInfoKind kind, SourceSpan span);
static inline TypeInfo *type_info_new_base(Type *type, SourceSpan span);
static inline bool type_info_ok(TypeInfo *type_info);
static inline bool type_info_poison(TypeInfo *type);

static inline bool type_kind_is_signed(TypeKind kind);
static inline bool type_kind_is_unsigned(TypeKind kind);
static inline bool type_kind_is_any_integer(TypeKind kind);
static inline bool type_kind_is_derived(TypeKind kind);


// ---- static inline function implementations.
static inline Type *type_reduced_from_expr(Expr *expr)
{
	return type_lowering(expr->type);
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

static inline bool type_is_integer_signed(Type *type)
{
	assert(type == type->canonical);
	return type->type_kind >= TYPE_I8 && type->type_kind <= TYPE_I64;
}

static inline bool type_is_integer_kind(Type *type)
{
	assert(type == type->canonical);
	return type->type_kind >= TYPE_BOOL && type->type_kind <= TYPE_U64;
}

static inline bool type_is_integer_unsigned(Type *type)
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

static inline bool type_is_ct(Type *type)
{
	RETRY:
	switch (type->type_kind)
	{
		case TYPE_FXX:
		case TYPE_IXX:
			return true;
		case TYPE_TYPEDEF:
			type = type->canonical;
			goto RETRY;
		default:
			return false;
	}
}

static inline bool type_is_pointer(Type *type)
{
	type = type->canonical;
	return type->type_kind == TYPE_POINTER || type->type_kind == TYPE_VARARRAY;
}

static inline uint64_t aligned_offset(uint64_t offset, uint64_t alignment)
{
	return ((offset + alignment - 1) / alignment) * alignment;
}

static inline bool type_is_substruct(Type *type)
{
	assert(type == type->canonical);
	return type->type_kind == TYPE_STRUCT && type->decl->is_substruct;
}

static inline bool type_is_float(Type *type)
{
	assert(type == type->canonical);
	return type->type_kind >= TYPE_F32 && type->type_kind <= TYPE_FXX;
}

static inline TypeInfo *type_info_new(TypeInfoKind kind, SourceSpan span)
{
	TypeInfo *type_info = type_info_calloc();
	type_info->kind = kind;
	type_info->span = span;
	type_info->resolve_status = RESOLVE_NOT_DONE;
	return type_info;
}

static inline TypeInfo *type_info_new_base(Type *type, SourceSpan span)
{
	TypeInfo *type_info = type_info_calloc();
	type_info->kind = TYPE_INFO_IDENTIFIER;
	type_info->resolve_status = RESOLVE_DONE;
	type_info->type = type;
	type_info->span = span;
	return type_info;
}

static inline Type *type_new(TypeKind kind, const char *name)
{
	Type *type = malloc_arena(sizeof(Type));
	memset(type, 0, sizeof(Type));
	type->type_kind = kind;
	assert(name);
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
	if (type->type_kind == TYPE_TYPEDEF) type = type->canonical;
	return type->type_kind >= TYPE_I8 && type->type_kind <= TYPE_FXX;
}

UnaryOp unaryop_from_token(TokenType type);
TokenType unaryop_to_token(UnaryOp type);
PostUnaryOp post_unaryop_from_token(TokenType type);
TokenType postunaryop_to_token(PostUnaryOp type);
BinaryOp binaryop_from_token(TokenType type);
BinaryOp binaryop_assign_base_op(BinaryOp assign_binary_op);
TokenType binaryop_to_token(BinaryOp type);


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


static inline bool type_is_builtin(TypeKind kind) { return kind >= TYPE_VOID && kind <= TYPE_TYPEID; }
static inline bool type_kind_is_signed(TypeKind kind) { return kind >= TYPE_I8 && kind <= TYPE_I64; }
static inline bool type_kind_is_unsigned(TypeKind kind) { return kind >= TYPE_U8 && kind <= TYPE_U64; }
static inline bool type_kind_is_any_integer(TypeKind kind) { return kind >= TYPE_I8 && kind <= TYPE_IXX; }
static inline bool type_is_signed(Type *type) { return type->type_kind >= TYPE_I8 && type->type_kind <= TYPE_I64; }
static inline bool type_is_unsigned(Type *type) { return type->type_kind >= TYPE_U8 && type->type_kind <= TYPE_U64; }
static inline bool type_ok(Type *type) { return !type || type->type_kind != TYPE_POISONED; }
static inline bool type_info_ok(TypeInfo *type_info) { return !type_info || type_info->kind != TYPE_INFO_POISON; }
bool type_is_scalar(Type *type);
static inline bool type_kind_is_derived(TypeKind kind)
{
	switch (kind)
	{
		case TYPE_ARRAY:
		case TYPE_POINTER:
		case TYPE_VARARRAY:
		case TYPE_SUBARRAY:
			return true;
		default:
			return false;
	}
}

static inline bool type_is_structlike(Type *type)
{
	assert(type->canonical == type);
	switch (type->type_kind)
	{
		case TYPE_UNION:
		case TYPE_STRUCT:
		case TYPE_ERRTYPE:
			return true;
		default:
			return false;

	}
}

static inline Type *type_lowering(Type *type)
{
	Type *canonical = type->canonical;
	if (canonical->type_kind == TYPE_ENUM) return canonical->decl->enums.type_info->type->canonical;
	if (canonical->type_kind == TYPE_TYPEID) return type_usize->canonical;
	return canonical;
}

static inline Decl *decl_raw(Decl *decl)
{
	if (decl->decl_kind != DECL_VAR || decl->var.kind != VARDECL_ALIAS) return decl;
	decl = decl->var.alias;
	assert(decl->decl_kind != DECL_VAR || decl->var.kind != VARDECL_ALIAS);
	return decl;
}

static inline bool decl_is_struct_type(Decl *decl)
{
	DeclKind kind = decl->decl_kind;
	return (kind == DECL_UNION) | (kind == DECL_STRUCT) | (kind == DECL_ERR);
}

static inline DeclKind decl_from_token(TokenType type)
{
	if (type == TOKEN_STRUCT) return DECL_STRUCT;
	if (type == TOKEN_UNION) return DECL_UNION;
	UNREACHABLE
}

static inline bool type_is_promotable_integer(Type *type)
{
	// If we support other architectures, update this.
	return type_is_integer_kind(type) && type->builtin.bytesize < type_c_int->canonical->builtin.bytesize;
}

static inline bool type_is_promotable_float(Type *type)
{
	// If we support other architectures, update this.
	return type_is_float(type->canonical) && type->builtin.bytesize < type_double->builtin.bytesize;
}

/**
 * Minimum alignment, values are either offsets or alignments.
 * @return
 */
static inline size_t type_min_alignment(size_t a, size_t b)
{
	return (a | b) & (1 + ~(a | b));
}

#define TRY_AST_OR(_ast_stmt, _res) ({ Ast* _ast = (_ast_stmt); if (!ast_ok(_ast)) return _res; _ast; })
#define TRY_EXPR_OR(_expr_stmt, _res) ({ Expr* _expr = (_expr_stmt); if (!expr_ok(_expr)) return _res; _expr; })
#define TRY_TYPE_OR(_type_stmt, _res) ({ TypeInfo* _type = (_type_stmt); if (!type_info_ok(_type)) return _res; _type; })
#define TRY_TYPE_REAL_OR(_type_stmt, _res) ({ Type* _type = (_type_stmt); if (!type_ok(_type)) return _res; _type; })
#define TRY_DECL_OR(_decl_stmt, _res) ({ Decl* _decl = (_decl_stmt); if (!decl_ok(_decl)) return _res; _decl; })

#pragma clang diagnostic pop


