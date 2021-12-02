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
#include <float.h>


typedef double Real;

#define MAX_ARRAYINDEX INT32_MAX
typedef uint64_t ByteSize;
typedef uint32_t TypeSize;
typedef int32_t IndexDiff;
typedef int32_t MemberIndex;
typedef uint32_t AlignSize;
typedef int32_t ScopeId;
typedef uint32_t ArraySize;
typedef uint64_t BitSize;


#if PLATFORM_WINDOWS
#define DEFAULT_EXE "a.exe"
#else
#define DEFAULT_EXE "a.out"
#endif

#if PLATFORM_WINDOWS
#define DEFAULT_OBJ_FILE_EXT ".obj"
#else
#define DEFAULT_OBJ_FILE_EXT ".o"
#endif


typedef uint32_t SourceLoc;
typedef struct
{
	unsigned index;
} TokenId;

#define NO_TOKEN_ID ((TokenId) { 0 })
#define NO_TOKEN ((Token) { .type = TOKEN_INVALID_TOKEN })
#define INVALID_TOKEN_ID ((TokenId) { UINT32_MAX })
#define INVALID_RANGE ((SourceSpan){ INVALID_TOKEN_ID, INVALID_TOKEN_ID })
#define TOKEN_IS_INVALID(_token_id) ((_token_id).index == INVALID_TOKEN_ID.index)
#define MAX_LOCALS 0xFFF
#define MAX_SCOPE_DEPTH 0x100
#define MAX_STRING_BUFFER 0x10000
#define MAX_MACRO_NESTING 1024
#define MAX_FUNCTION_SIGNATURE_SIZE 2048
#define MAX_PARAMS 512
#define MAX_MEMBERS ((MemberIndex)(((uint64_t)2) << 28))
#define MAX_ALIGNMENT ((MemberIndex)(((uint64_t)2) << 28))
#define MAX_TYPE_SIZE UINT32_MAX
#define MAX_OFFSET ((ArrayIndex)(((uint64_t)2) << 60))

typedef struct Ast_ Ast;
typedef struct Decl_ Decl;
typedef struct TypeInfo_ TypeInfo;
typedef struct Expr_ Expr;
typedef struct Module_ Module;
typedef struct Type_ Type;

typedef unsigned AstId;

typedef struct Int128_
{
	uint64_t high;
	uint64_t low;
} Int128;

typedef struct
{
	Int128 i;
	TypeKind type;
} Int;

typedef struct
{
	Real f;
	TypeKind type;
} Float;

#define UINT128_MAX ((Int128) { UINT64_MAX, UINT64_MAX })
#define INT128_MAX ((Int128) { INT64_MAX, UINT64_MAX })
#define INT128_MIN ((Int128) { (uint64_t)INT64_MIN, 0 })

typedef enum
{
	CONST_INIT_ZERO,
	CONST_INIT_STRUCT,
	CONST_INIT_UNION,
	CONST_INIT_VALUE,
	CONST_INIT_ARRAY,
	CONST_INIT_ARRAY_FULL,
	CONST_INIT_ARRAY_VALUE,
	} ConstInitType;


typedef struct ConstInitializer_
{
	ConstInitType kind;
	// Type initialized
	Type *type;
	union
	{
		struct ConstInitializer_ **init_struct;
		Expr *init_value;
		struct
		{
			struct ConstInitializer_ *element;
			MemberIndex index;
		} init_union;
		struct
		{
			struct ConstInitializer_ **elements;
		} init_array;
		struct ConstInitializer_ **init_array_full;
		struct
		{
			struct ConstInitializer_ *element;
			MemberIndex index;
		} init_array_value;
	};
} ConstInitializer;





typedef struct
{
	ConstKind const_kind : 8;
	bool narrowable : 1;
	bool is_character : 1;
	bool is_hex : 1;
	union
	{
		Float fxx;
		Int ixx;
		bool b;
		struct
		{
			const char *chars;
			uint32_t len;
		} string;
		Decl *enum_val;
		Decl *err_val;
		struct
		{
			const char *ptr;
			TypeSize len;
		} bytes;
		Type *typeid;
		ConstInitializer *list;
	};
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
	uint32_t token_start_id;
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
	TokenType type : 16;
} Token;


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



typedef struct Path_
{
	SourceSpan span;
	const char *module;
	uint32_t len;
} Path;


typedef struct
{
	unsigned bitsize : 8;
	unsigned bytesize : 8;
	unsigned abi_alignment : 8;
	unsigned pref_alignment : 8;
}  TypeBuiltin;

typedef struct
{
	TokenId name_loc;
	Path *path;
} TypeUnresolved;

typedef struct
{
	Type *base;
	ArraySize len;
} TypeArray;

typedef struct
{
	Type *base;
	ArraySize len;
} TypeVector;

typedef struct
{
	struct FunctionSignature_ *signature;
	const char *mangled_function_signature;
} TypeFunc;

struct Type_
{
	TypeKind type_kind;
	Type *canonical;
	const char *name;
	Type **type_cache;
	void *backend_type;
	void *backend_aux_type;
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
		// Failable
		Type *failable;
		// Bitstruct
		Type *bitstruct;
	};
};

struct TypeInfo_
{
	ResolveStatus resolve_status : 3;
	bool virtual_type : 1;
	bool failable : 1;
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
	bool private;
	bool aliased;
} ImportDecl;

typedef struct
{
	TypeSize size;
	Decl **members;
	MemberIndex union_rep;
	AlignSize padding : 16;
} StructDecl;


typedef struct
{
	TypeInfo *base_type;
	Decl **members;
	bool msb0 : 1;
	bool big_endian : 1;
	bool little_endian : 1;
	bool overlap : 1;
} BitStructDecl;

typedef struct VarDecl_
{
	VarDeclKind kind : 8;
	bool constant : 1;
	bool unwrap : 1;
	bool vararg : 1;
	bool is_static : 1;
	bool is_threadlocal : 1;
	TypeInfo *type_info;
	union
	{
		Expr *init_expr;
		Decl *alias;
	};
	union
	{
		void *backend_debug_ref;
		unsigned scope_depth;
		Expr *start;
		unsigned start_bit;
	};
	union
	{
		void *failable_ref;
		struct ABIArgInfo_ *abi_info;
		Expr *end;
		unsigned end_bit;
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

typedef enum
{
	VARIADIC_NONE,
	VARIADIC_TYPED,
	VARIADIC_RAW,
} Variadic;

typedef struct FunctionSignature_
{
	CallABI call_abi : 4;
	Variadic variadic : 3;
	bool has_default : 1;
	bool use_win64 : 1;
	TypeInfo *rtype;
	struct ABIArgInfo_ *ret_abi_info;
	struct ABIArgInfo_ *failable_abi_info;
	Decl** params;
} FunctionSignature;

typedef struct
{
	Decl **vars;
} FuncAnnotations;

typedef struct
{
	struct
	{
		bool attr_weak : 1;
		bool attr_noreturn : 1;
		bool attr_inline : 1;
		bool attr_noinline : 1;
		bool attr_extname : 1;
		bool attr_naked : 1;
	};

	TypeInfo *type_parent;
	FunctionSignature function_signature;
	Ast *body;
	FuncAnnotations *annotations;
} FuncDecl;

typedef struct
{
	AttributeDomain domains;
	FunctionSignature attr_signature;
} AttrDecl;

typedef struct
{
	bool is_func : 1;
	bool is_distinct : 1;
	union
	{
		FunctionSignature function_signature;
		TypeInfo *type_info;
	};
} TypedefDecl;


typedef struct
{
	Decl **functions;
	Decl **members;
} InterfaceDecl;

typedef struct
{
	union
	{
		TypedefDecl typedef_decl;
		Type *base_type;
	};
} DistinctDecl;

typedef struct
{
	Decl **parameters;
	TypeInfo *type_parent; // May be null
	TypeInfo *rtype; // May be null!
	struct Ast_ *body;
	Decl **body_parameters;
	TokenId block_parameter;
	struct Context_ *context;
} MacroDecl;

typedef struct
{
	struct Ast_ **cases;
	Decl **parameters;
	Decl **body_parameters;
	TypeInfo *rtype; // May be null!
	Path *path; // For redefinition
} GenericDecl;

typedef enum
{
	DEFINE_TYPE_GENERIC,
	DEFINE_IDENT_ALIAS,
	DEFINE_IDENT_GENERIC,
	DEFINE_ATTRIBUTE,
} DefineType;

typedef struct
{
	DefineType define_kind: 5;
	union
	{
		struct
		{
			Decl **params;
			Attr **attrs;
		} attributes;
		struct
		{
			union
			{
				TypeInfo *type_info;
				struct
				{
					Path *path;
					TokenId identifier;
				};
			};
			TypeInfo **generic_params;
		};
		Decl *alias;
	};
} DefineDecl;

typedef struct
{
	AstId defer;
	bool next_target : 1;
	void *break_target;
	void *continue_target;
	AstId scope_defer;
	AstId parent;
} LabelDecl;


typedef struct Decl_
{
	const char *name;
	TokenId name_token;
	SourceSpan span;
	const char *external_name;
	Ast *docs;
	DeclKind decl_kind : 7;
	Visibility visibility : 3;
	ResolveStatus resolve_status : 3;
	bool is_packed : 1;
	bool is_opaque : 1;
	bool needs_additional_pad : 1;
	bool is_substruct : 1;
	bool has_variable_array : 1;
	void *backend_ref;
	const char *extname;
	AlignSize alignment;
	const char *section;
	AlignSize offset : 32;
	AlignSize padding : 32;
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
			Decl **methods;
			union
			{
				// Unions, Errtype and Struct use strukt
				StructDecl strukt;
				EnumDecl enums;
				DistinctDecl distinct_decl;
				BitStructDecl bitstruct;
			};
		};
		ImportDecl import;
		VarDecl var;
		LabelDecl label;
		EnumConstantDecl enum_constant;
		FuncDecl func_decl;
		AttrDecl attr;
		TypedefDecl typedef_decl;
		InterfaceDecl interface_decl;
		MacroDecl macro_decl;
		GenericDecl generic_decl;
		DefineDecl define_decl;
		CtIfDecl ct_if_decl;
		CtIfDecl ct_elif_decl;
		CtSwitchDecl ct_switch_decl;
		CtCaseDecl ct_case_decl;
		Ast *ct_assert_decl;
		Decl** ct_else_decl;
	};
} Decl;



typedef struct
{
	bool is_jump : 1;
	bool widen : 1;
	Expr *expr;
	union
	{
		Expr *or_error_expr;
		Ast *or_error_stmt;
	};
} ExprOrError;


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
	bool widen : 1;
} ExprTernary;

typedef struct
{
	Expr *left;
	Expr *right;
	BinaryOp operator : 8;
	bool widen : 1;
} ExprBinary;

typedef struct
{
	Expr* expr;
	UnaryOp operator : 8;
	bool widen : 1;
} ExprUnary;





typedef struct
{
	bool is_type_method : 1;
	bool is_pointer_call : 1;
	bool unsplat_last : 1;
	bool force_inline : 1;
	bool force_noinline : 1;
	bool is_builtin : 1;
	union
	{
		Expr *function;
		Decl *func_ref;
	};
	Expr **arguments;
	Decl **body_arguments;
	Ast *body;
	Attr **attributes;
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
		Expr *child;
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
	MemberIndex index;
	MemberIndex index_end;
} DesignatorElement;

typedef struct
{
	DesignatorElement **path;
	Expr *value;
} ExprDesignator;

typedef struct
{
	Path *path;
	TokenId identifier;
	Decl *decl;
} ExprIdentifier;

typedef struct
{
	Path *path;
	TokenId identifier;
} ExprPlaceholder;

typedef struct
{
	TokenId identifier;
	bool is_ref : 1;
	bool is_rvalue : 1;
	Decl *decl;
} ExprIdentifierRaw;

typedef struct
{
	bool array : 1;
	union
	{
		MemberIndex index;
		const char *ident;
	};
} ExprFlatElement;

typedef struct
{
	TokenType token_type;
	struct
	{
		Expr *main_var;
		ExprFlatElement *flat_path;
	};
} ExprCtCall;

typedef struct
{
	Expr *inner;
	Decl *decl;
} ExprMacroExpansion;

typedef struct
{
	CastKind kind;
	bool implicit;
	Expr *expr;
	TypeInfo *type_info;
	union
	{
		size_t truncated_size;
	};
} ExprCast;

typedef struct
{
	Expr **values;
	Decl **declarations;
	Ast *ast;
} ExprBodyExpansion;

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
	bool is_try : 1;
	Decl *decl;
} ExprTryDecl;

typedef struct
{
	union
	{
		struct
		{
			Expr *variable;
			TypeInfo *type;
		};
		struct
		{
			Decl *decl;
			Expr *lhs;
		};
	};
	Expr **exprs;
} ExprCatchUnwrap;

typedef struct
{
	union
	{
		struct
		{
			Expr *variable;
			TypeInfo *type;
			Expr *init;
		};
		struct
		{
			bool assign_existing : 1;
			Expr *failable;
			union
			{
				Decl *decl;
				Expr *lhs;
			};
		};
	};
} ExprTryUnwrap;

typedef struct
{
	Expr *inner;
} ExprLen;


typedef struct
{
	Token identifier;
	BuiltinFunction builtin;
} ExprBuiltin;
struct Expr_
{
	ExprKind expr_kind : 8;
	ResolveStatus resolve_status : 4;
	SourceSpan span;
	Type *type;
	union {
		ExprLen len_expr;
		ExprCast cast_expr;
		TypeInfo *type_expr;
		ExprConst const_expr;
		ExprGuard rethrow_expr;
		Decl *decl_expr;
		ExprOrError or_error_expr;
		ExprSliceAssign slice_assign_expr;
		ExprBinary binary_expr;
		ExprTernary ternary_expr;
		ExprUnary unary_expr;
		Expr** try_unwrap_chain_expr;
		ExprTryUnwrap try_unwrap_expr;
		ExprCall call_expr;
		ExprSlice slice_expr;
		Expr *inner_expr;
		ExprCatchUnwrap catch_unwrap_expr;
		ExprSubscript subscript_expr;
		ExprAccess access_expr;
		ExprDesignator designator_expr;
		ExprIdentifier identifier_expr;
		ExprPlaceholder placeholder_expr;
		ExprIdentifierRaw ct_ident_expr;
		ExprCtCall ct_call_expr;
		ExprIdentifierRaw ct_macro_ident_expr;
		ExprMacroExpansion macro_expansion_expr;
		ExprIdentifierRaw hash_ident_expr;
		TypeInfo *typeid_expr;
		ExprBodyExpansion body_expansion_expr;
		ExprCompoundLiteral expr_compound_literal;
		Expr** expression_list;
		Expr** initializer_list;
		Expr** designated_init_list;
		ExprScope expr_scope;
		ExprFuncBlock expr_block;
		ExprMacroBlock macro_block;
		Expr** cond_expr;
		ExprBuiltin builtin_expr;
	};
};


typedef struct
{
	struct Ast_ **stmts;
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
	bool skip_first : 1;
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
	Expr *expr;
	Expr *to_expr;
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
			Ast* scope_defer;
			bool if_chain;
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
	FlowCommon flow;
	bool index_by_ref : 1;
	bool value_by_ref : 1;
	bool iterator : 1;
	CastKind cast;
	Decl *index;
	Decl *variable;
	Expr *enumeration;
	Ast *body;
	void *continue_block;
	void *exit_block;
} AstForeachStmt;

typedef struct
{
	AstId prev_defer;
	Ast *body; // Compound statement
	struct
	{
		void *exit_block;
	} codegen;
} AstDeferStmt;



typedef struct AstCtIfStmt_
{
	Expr *expr;
	struct Ast_ *then;
	struct Ast_ *elif;
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
			void *expr_or_type_info;
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
	Expr *body;
} AstAsmStmt;

typedef struct
{
	Expr *scoped;
	Ast *stmt;
} AstScopingStmt;
typedef struct
{
	Expr *message;
	Expr *expr;
} AstAssertStmt;


typedef struct
{
	DocDirectiveKind kind;
	union
	{
		struct
		{
			TokenId param;
			TokenId rest_of_line;
		} param;
		struct
		{
			Expr *decl_exprs;
			Expr *comment;
		} contract;
		struct
		{
			TokenId rest_of_line;
		} pure;
		struct
		{
			const char *directive_name;
			TokenId rest_of_line;
		} generic;

	};
} AstDocDirective;

typedef struct Ast_
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
		Decl *var_stmt;              // 8
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
		AstForStmt for_stmt;                // 32
		AstForeachStmt foreach_stmt;
		AstCtIfStmt ct_if_stmt;             // 24
		AstCtIfStmt ct_elif_stmt;           // 24
		Ast *ct_else_stmt;                  // 8
		AstCtForStmt ct_for_stmt;           // 64
		AstScopedStmt scoped_stmt;          // 16
		AstScopingStmt scoping_stmt;
		AstAssertStmt ct_assert_stmt;
		AstAssertStmt assert_stmt;
		Ast **directives;
		AstDocDirective doc_directive;
	};
} Ast;



typedef struct Module_
{
	Path *name;
	TokenId *parameters;

	bool is_external : 1;
	bool is_c_library : 1;
	bool is_exported : 1;
	bool is_generic : 1;
	bool is_private : 1;
	AnalysisStage stage : 6;

	Ast **files; // Asts

	Decl** method_extensions;
	Decl** generic_cache;
	STable symbols;
	STable public_symbols;
	struct Context_ **contexts;
} Module;



typedef struct DynamicScope_
{
	ScopeId scope_id;
	bool allow_dead_code : 1;
	bool jump_end : 1;
	ScopeFlags flags;
	Decl **local_decl_start;
	Decl **current_local;
	AstId defer_last;
	AstId defer_start;
	Ast *in_defer;
	unsigned depth;
} DynamicScope;

typedef struct MacroScope_
{
	Decl *macro;
	uint32_t inline_line;
	uint32_t original_inline_line;
	Decl **locals_start;
	unsigned depth;
	Decl **yield_symbol_start;
	Decl **yield_symbol_end;
	Decl **yield_args;
	Ast *yield_body;
	bool in_yield;
	const char *body_param;
} MacroScope;

typedef union
{
	struct
	{
		const char *string;
		size_t strlen;
	};
	Float value;
	struct
	{
		bool is_base64 : 1;
		uint64_t len : 63;
	};
	struct
	{
		Int128 char_value;
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
	TokenData *latest_token_data;
	SourceLocation *latest_token_loc;
	unsigned char *latest_token_type;
} Lexer;


typedef struct Context_
{
	Path *module_name;
	File* file;
	Decl** imports;
	Module *module;
	STable local_symbols;
	Decl **global_decls;
	Decl **enums;
	Decl **errtypes;
	Decl **types;
	Decl **generic_defines;
	Decl **functions;
	Decl **macros;
	Decl **generics;
	Decl **generic_methods;
	Decl **templates;
	Decl **methods;
	Decl **macro_methods;
	Decl **vars;
	Decl **ct_ifs;
	Decl **ct_asserts;
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
	struct
	{
		Type *expected_block_type;
		Ast **returns;
		bool expr_failable_return;
		// Reusable returns cache.
		Ast **returns_cache;
	};
	Type *rtype;
	MacroScope macro_scope;
	struct {
		STable external_symbols;
		Decl **external_symbol_list;
	};
	Decl* locals[MAX_LOCALS];
	DynamicScope active_scope;
	Lexer *lexer;
	Token tok;
	TokenId prev_tok;
	Token next_tok;
	TokenId docs_start;
	TokenId docs_end;
	void *llvm_debug_file;
	void *llvm_debug_compile_unit;
} Context;

typedef struct
{
	STable modules;
	Module **module_list;
	Module **generic_module_list;
	STable global_symbols;
	STable qualified_symbols;
	Type **type;
	const char *lib_dir;
	const char **sources;
	File **loaded_sources;
	bool in_panic_mode : 1;
	bool in_test_mode : 1;
	unsigned errors_found;
	unsigned warnings_found;
	char scratch_buffer[MAX_STRING_BUFFER];
	uint32_t scratch_buffer_len;
	STable scratch_table;
	STable compiler_defines;
	Module std_module;
	Path std_module_path;
} GlobalContext;


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
		bool realign : 1;
		bool by_val : 1;
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
			uint8_t elements : 7;
			bool prevent_flatten : 1;
		} direct_coerce;
		struct
		{
			// We may request a certain alignment of the parameters.
			AlignSize alignment;
			Type *type;
		} indirect;
	};

} ABIArgInfo;

extern GlobalContext global_context;
extern BuildTarget active_target;
extern Ast *poisoned_ast;
extern Decl *poisoned_decl;
extern Expr *poisoned_expr;
extern Type *poisoned_type;
extern TypeInfo *poisoned_type_info;


extern Type *type_bool, *type_void, *type_compstr, *type_voidptr;
extern Type *type_half, *type_float, *type_double, *type_quad;
extern Type *type_ichar, *type_short, *type_int, *type_long, *type_isize;
extern Type *type_char, *type_ushort, *type_uint, *type_ulong, *type_usize;
extern Type *type_iptr, *type_uptr, *type_iptrdiff, *type_uptrdiff;
extern Type *type_u128, *type_i128;
extern Type *type_typeid, *type_anyerr, *type_typeinfo;
extern Type *type_any;
extern Type *type_complist;
extern Type *type_anyfail;

extern const char *attribute_list[NUMBER_OF_ATTRIBUTES];
extern const char *builtin_list[NUMBER_OF_BUILTINS];
extern const char *kw_std;
extern const char *kw_max;
extern const char *kw_min;
extern const char *kw_elements;
extern const char *kw_align;
extern const char *kw_deprecated;
extern const char *kw_distinct;
extern const char *kw_ensure;
extern const char *kw_inline;
extern const char *kw_inf;
extern const char *kw_iterator;
extern const char *kw_operator_element_at;
extern const char *kw_operator_element_at_ref;
extern const char *kw_operator_len;
extern const char *kw_len;
extern const char *kw_next;
extern const char *kw_nan;
extern const char *kw_main;
extern const char *kw_ordinal;
extern const char *kw_reqparse;
extern const char *kw_require;
extern const char *kw_pure;
extern const char *kw_param;
extern const char *kw_errors;
extern const char *kw___ceil;
extern const char *kw___round;
extern const char *kw___sqrt;
extern const char *kw___trunc;
extern const char *kw_FILE;
extern const char *kw_FUNC;
extern const char *kw_LINE;
extern const char *kw_LINEREAL;
extern const char *kw_default_iterator;
extern const char *kw_incr;
extern const char *kw_check_assign;
extern const char *kw_builtin_ceil;
extern const char *kw_builtin_trunc;
extern const char *kw_builtin_sqrt;
extern const char *kw_builtin_cos;
extern const char *kw_builtin_sin;
extern const char *kw_builtin_log;
extern const char *kw_builtin_log2;
extern const char *kw_builtin_log10;
extern const char *kw_builtin_max;
extern const char *kw_builtin_min;
extern const char *kw_builtin_pow;
extern const char *kw_builtin_exp;
extern const char *kw_builtin_fabs;
extern const char *kw_builtin_fma;
extern const char *kw_builtin_cmpxchg;


#define AST_NEW_TOKEN(_kind, _token) new_ast(_kind, source_span_from_token_id((_token).id))
#define AST_NEW(_kind, _loc) new_ast(_kind, _loc)

typedef unsigned char TokenTypeChar;
ARENA_DEF(ast, Ast)
ARENA_DEF(expr, Expr)
ARENA_DEF(sourceloc, SourceLocation)
ARENA_DEF(toktype, TokenTypeChar)
ARENA_DEF(tokdata, TokenData)
ARENA_DEF(decl, Decl)
ARENA_DEF(type_info, TypeInfo)

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


typedef enum CmpRes_
{
	CMP_LT = -1,
	CMP_EQ = 0,
	CMP_GT = 1,
} CmpRes;

void type_setup(PlatformTarget *target);
Float float_add(Float op1, Float op2);
Float float_sub(Float op1, Float op2);
Float float_mul(Float op1, Float op2);
Float float_div(Float op1, Float op2);
Float float_neg(Float op);
Float float_from_string(const char *string, char **error);
Float float_from_hex(const char *string, char **error);
Int128 i128_from_double(double x);
bool int_ucomp(Int op1, uint64_t op2, BinaryOp op);
bool int_icomp(Int op1, int64_t op2, BinaryOp op);
bool int_comp(Int op1, Int op2, BinaryOp op);
uint64_t int_to_u64(Int op);
int64_t int_to_i64(Int op);
bool int_is_zero(Int op);
bool int_fits(Int op1, TypeKind kind);
Int int_rightmost_bits(Int op, unsigned to_bits, TypeKind result_type);
Int int_conv(Int op, TypeKind to_type);
Int int_div(Int op1, Int op2);
Int int_mul(Int op1, Int op2);
Int int_sub(Int op1, Int op2);
Int int_sub64(Int op1, uint64_t op2);
Int int_add(Int op1, Int op2);
Int int_add64(Int op1, uint64_t op2);
Int int_rem(Int op1, Int op2);
Int int_and(Int op1, Int op2);
Int int_or(Int op1, Int op2);
Int int_xor(Int op1, Int op2);
Int int_neg(Int op);
Int int_not(Int op);
bool int_is_neg(Int op);
Int int_shr64(Int op, uint64_t);
Int int_shl64(Int op, uint64_t);
Real int_to_real(Int op);
Int int_from_real(Real d, TypeKind type);
char *int_to_str(Int i, int radix);
bool i128_can_convert_from_double(double x);
bool i128_can_convert_from_double_signed(double x);
Int128 i128_from_double(double x);
Int128 i128_from_double_signed(double x);
Int128 i128_extend(Int128 op, TypeKind type);
Int128 i128_add(Int128 op1, Int128 op2);
Int128 i128_add64(Int128 op1, uint64_t op2);
Int128 i128_add_swrap64(Int128 op1, int64_t op2, bool *wrapped);
Int128 i128_add_uwrap64(Int128 op1, uint64_t op2, bool *wrapped);
Int128 i128_sub(Int128 op1, Int128 op2);
Int128 i128_sub64(Int128 op1, uint64_t op2);
Int128 i128_and(Int128 op1, Int128 op2);
Int128 i128_or(Int128 op1, Int128 op2);
Int128 i128_xor(Int128 op1, Int128 op2);
Int128 i128_neg(Int128 op1);
Int128 i128_not(Int128 op1);
Int128 i128_mult(Int128 op1, Int128 op2);
Int128 i128_mult64(Int128 op1, uint64_t op2);
Int128 i128_from_str(const char *str);
char *i128_to_string(Int128 op, uint64_t base, bool is_signed);
bool i128_is_neg(Int128 op);
CmpRes i128_ucomp(Int128 op1, Int128 op2);
CmpRes i128_scomp(Int128 op1, Int128 op2);
CmpRes i128_comp(Int128 op1, Int128 op2, Type *type);
Int128 i128_shl64(Int128 op1, uint64_t amount);
Int128 i128_shl(Int128 op1, Int128 op);
Int128 i128_ashr64(Int128 op1, uint64_t amount);
Int128 i128_ashr(Int128 op1, Int128 op2);
Int128 i128_lshr64(Int128 op1, uint64_t amount);
Int128 i128_lshr(Int128 op1, Int128 op2);
Int128 i128_udiv(Int128 op1, Int128 op2);
Int128 i128_sdiv(Int128 op1, Int128 op2);
Int128 i128_urem(Int128 op1, Int128 op2);
Int128 i128_srem(Int128 op1, Int128 op2);
void i128_udivrem(Int128 op1, Int128 op2, Int128 *div, Int128 *rem);
Real i128_to_float(Int128 op);
Real i128_to_float_signed(Int128 op);
bool i128_is_zero(Int128 op);
uint32_t i128_clz(const Int128 *op);
uint32_t i128_ctz(const Int128 *op);
int i128_lsb(const Int128 *op);
int i128_msb(const Int128 *op);
Int128 i128_from_float_signed(Real d);
Int128 i128_from_float_unsigned(Real d);
Int128 i128_from_signed(int64_t i);
Int128 i128_from_unsigned(uint64_t i);
bool i128_get_bit(const Int128 *op, int bit);

static inline bool type_may_negate(Type *type)
{
	RETRY:
	switch (type->type_kind)
	{
		case TYPE_VECTOR:
			type = type->vector.base;
			goto RETRY;
		case ALL_FLOATS:
		case ALL_INTS:
			return true;
		case TYPE_DISTINCT:
			type = type->decl->distinct_decl.base_type;
			goto RETRY;
		case TYPE_TYPEDEF:
			type = type->canonical;
			goto RETRY;
		case TYPE_FAILABLE:
			type = type->failable;
			goto RETRY;
		default:
			return false;
	}
}


bool cast_implicit(Expr *expr, Type *to_type);
bool cast(Expr *expr, Type *to_type);

bool cast_may_implicit(Type *from_type, Type *to_type, bool is_simple_expr, bool failable_allowed);

bool cast_may_explicit(Type *from_type, Type *to_type, bool ignore_failability, bool is_const);
bool cast_implicit_bit_width(Expr *expr, Type *to_type);

CastKind cast_to_bool_kind(Type *type);

const char *llvm_codegen(void *context);
void *llvm_gen(Module *module);
void llvm_codegen_setup();

const char *tinybackend_codegen(void *context);
void *tinybackend_gen(Module *module);
void tinybackend_codegen_setup();

void header_gen(Module *module);

void global_context_add_type(Type *type);
Decl *compiler_find_symbol(const char *name);
Module *compiler_find_or_create_module(Path *module_name, TokenId *parameters, bool is_private);
Module *global_context_find_module(const char *name);
void compiler_register_public_symbol(Decl *decl);

Context *context_create(File *file);
void context_register_global_decl(Context *context, Decl *decl);
void context_register_external_symbol(Context *context, Decl *decl);
bool context_add_import(Context *context, Path *path, Token symbol, Token alias, bool private_import);
bool context_set_module_from_filename(Context *context);
bool context_set_module(Context *context, Path *path, TokenId *generic_parameters, bool is_private);

// --- Decl functions

Decl *decl_new(DeclKind decl_kind, TokenId name, Visibility visibility);
Decl *decl_new_with_type(TokenId name, DeclKind decl_type, Visibility visibility);
Decl *decl_new_var(TokenId name, TypeInfo *type, VarDeclKind kind, Visibility visibility);
Decl *decl_new_generated_var(const char *name, Type *type, VarDeclKind kind, SourceSpan span);
#define DECL_NEW(_kind, _vis) decl_new(_kind, context->tok.id, _vis)
#define DECL_NEW_WITH_TYPE(_kind, _vis) decl_new_with_type(context->tok.id, _kind, _vis)
#define DECL_NEW_VAR(_type, _kind, _vis) decl_new_var(context->tok.id, _type, _kind, _vis)
void decl_set_external_name(Decl *decl);
const char *decl_to_name(Decl *decl);

static inline Decl *decl_raw(Decl *decl);
static inline bool decl_ok(Decl *decl) { return !decl || decl->decl_kind != DECL_POISONED; }
static inline bool decl_poison(Decl *decl) { decl->decl_kind = DECL_POISONED; decl->resolve_status = RESOLVE_DONE; return false; }
static inline bool decl_is_struct_type(Decl *decl);
static inline bool decl_is_callable_type(Decl *decl);
static inline bool decl_is_user_defined_type(Decl *decl);
static inline DeclKind decl_from_token(TokenType type);
static inline bool decl_var_is_assignable(Decl *decl)
{
	switch (decl->var.kind)
	{
		case VARDECL_GLOBAL:
		case VARDECL_LOCAL:
		case VARDECL_PARAM:
		case VARDECL_PARAM_CT:
		case VARDECL_PARAM_CT_TYPE:
		case VARDECL_PARAM_REF:
		case VARDECL_LOCAL_CT:
		case VARDECL_LOCAL_CT_TYPE:
		case VARDECL_UNWRAPPED:
			return true;
		case VARDECL_BITMEMBER:
		case VARDECL_CONST:
		case VARDECL_MEMBER:
		case VARDECL_PARAM_EXPR:
			return false;
		case VARDECL_REWRAPPED:
		case VARDECL_ERASE:
			UNREACHABLE
	}
}
static inline Decl *decl_flatten(Decl *decl)
{
	if (decl->decl_kind == DECL_DEFINE && decl->define_decl.define_kind != DEFINE_TYPE_GENERIC)
	{
		return decl->define_decl.alias;
	}
	return decl;
}

// --- Diag functions


void diag_verror_range(SourceLocation *location, const char *message, va_list args);

#define EXPR_NEW_EXPR(kind_, expr_) expr_new(kind_, (expr_)->span)
#define EXPR_NEW_TOKEN(kind_, tok_) expr_new(kind_, source_span_from_token_id((tok_).id))
Expr *expr_new(ExprKind kind, SourceSpan start);
bool expr_is_simple(Expr *expr);
bool expr_is_pure(Expr *expr);
static inline bool expr_ok(Expr *expr) { return expr == NULL || expr->expr_kind != EXPR_POISONED; }
static inline bool expr_poison(Expr *expr) { expr->expr_kind = EXPR_POISONED; expr->resolve_status = RESOLVE_DONE; return false; }
static inline void expr_replace(Expr *expr, Expr *replacement)
{
	SourceSpan loc = expr->span;
	*expr = *replacement;
	expr->span = loc;
}

void expr_const_set_int(ExprConst *expr, uint64_t v, TypeKind kind);
void expr_const_set_float(ExprConst *expr, Real d, TypeKind kind);
void expr_const_set_bool(ExprConst *expr, bool b);
void expr_const_set_null(ExprConst *expr);

bool expr_const_compare(const ExprConst *left, const ExprConst *right, BinaryOp op);
bool expr_const_will_overflow(const ExprConst *expr, TypeKind kind);
ArraySize expr_const_list_size(const ConstInitializer *list);

Expr *expr_generate_decl(Decl *decl, Expr *assign);
void expr_insert_addr(Expr *original);
void expr_insert_deref(Expr *expr);
Expr *expr_variable(Decl *decl);
typedef enum
{
	CONSTANT_EVAL_ANY,
	CONSTANT_EVAL_FOLDABLE,
	CONSTANT_EVAL_NO_LINKTIME,
} ConstantEvalKind;
bool expr_is_constant_eval(Expr *expr, ConstantEvalKind eval_kind);
const char *expr_const_to_error_string(const ExprConst *expr);
static inline bool expr_is_init_list(Expr *expr)
{
	ExprKind kind = expr->expr_kind;
	return kind == EXPR_DESIGNATED_INITIALIZER_LIST || kind == EXPR_INITIALIZER_LIST;
}
bool float_const_fits_type(const ExprConst *expr_const, TypeKind kind);


// --- Lexer functions


Token lexer_advance(Lexer *lexer);
bool lexer_scan_ident_test(Lexer *lexer, const char *scan);
void lexer_init_for_test(Lexer *lexer, const char *text, size_t len);
void lexer_init_with_file(Lexer *lexer, File *file);
File* lexer_current_file(Lexer *lexer);


static inline SourceLocation *tokenid_loc(TokenId token) { return sourcelocptr(token.index); }
static inline SourceLocation *token_loc(Token token) { return sourcelocptr(token.id.index); }
static inline TokenData *tokendata_from_id(TokenId token) { return tokdataptr(token.index); }
static inline TokenData *tokendata_from_token(Token token) { return tokdataptr(token.id.index); }

#define TOKDATA(T) _Generic((T), TokenId: tokendata_from_id, Token: tokendata_from_token)(T)
#define TOKLOC(T) _Generic((T), TokenId: tokenid_loc, Token: token_loc)(T)

#define TOKSTR(T) TOKDATA(T)->string

#define TOKREAL(T) TOKDATA(T)->value

static inline TokenType tokenid_type(TokenId token) { return (TokenType)toktypeptr(token.index)[0]; }
static inline TokenType token_type(Token token) { return (TokenType)toktypeptr(token.id.index)[0]; }
#define TOKTYPE(T) _Generic((T), TokenId: tokenid_type, Token: token_type)(T)

#define TOKLEN(T) TOKLOC(T)->length

#define TOKVALID(_tok) (_tok.index != 0)
Decl *module_find_symbol(Module *module, const char *symbol);

bool parse_file(File *file);
Path *path_create_from_string(const char *string, uint32_t len, SourceSpan span);
Path *path_find_parent_path(Path *path);

#define SEMA_TOKEN_ERROR(_tok, ...) sema_error_range(source_span_from_token_id(_tok.id), __VA_ARGS__)
#define SEMA_TOKID_ERROR(_tok_id, ...) sema_error_range(source_span_from_token_id(_tok_id), __VA_ARGS__)
#define SEMA_ERROR(_node, ...) sema_error_range((_node)->span, __VA_ARGS__)
#define SEMA_PREV(_node, ...) sema_prev_at_range3((_node)->span, __VA_ARGS__)
#define SEMA_TOKID_PREV(_tok_id, ...) sema_prev_at_range3(source_span_from_token_id(_tok_id), __VA_ARGS__)

void sema_analysis_pass_process_imports(Module *module);
void sema_analysis_pass_register_globals(Module *module);
void sema_analysis_pass_conditional_compilation(Module *module);
void sema_analysis_pass_decls(Module *module);
void sema_analysis_pass_ct_assert(Module *module);
void sema_analysis_pass_functions(Module *module);
void sema_analyze_stage(Module *module, AnalysisStage stage);

bool sema_failed_cast(Expr *expr, Type *from, Type *to);
bool sema_add_member(Context *context, Decl *decl);
bool sema_add_local(Context *context, Decl *decl);
bool sema_unwrap_var(Context *context, Decl *decl);
bool sema_rewrap_var(Context *context, Decl *decl);
bool sema_erase_var(Context *context, Decl *decl);
bool sema_erase_unwrapped(Context *context, Decl *decl);
bool sema_analyse_cond_expr(Context *context, Expr *expr);

bool sema_analyse_expr_rhs(Context *context, Type *to, Expr *expr, bool allow_failable);
MemberIndex sema_get_initializer_const_array_size(Context *context, Expr *initializer, bool *may_be_array, bool *is_const_size);
bool sema_analyse_expr(Context *context, Expr *expr);
bool sema_analyse_inferred_expr(Context *context, Type *to, Expr *expr);
bool sema_analyse_decl(Context *context, Decl *decl);

bool sema_analyse_var_decl(Context *context, Decl *decl, bool local);
bool sema_analyse_ct_assert_stmt(Context *context, Ast *statement);
bool sema_analyse_statement(Context *context, Ast *statement);
bool sema_expr_analyse_assign_right_side(Context *context, Expr *expr, Type *left_type, Expr *right, bool is_unwrapped_var);

bool sema_expr_analyse_general_call(Context *context, Expr *expr, Decl *decl, Expr *struct_var, bool is_macro, bool failable);
Decl *sema_resolve_symbol_in_current_dynamic_scope(Context *context, const char *symbol);
Decl *sema_resolve_parameterized_symbol(Context *context, TokenId symbol, Path *path);
Decl *sema_resolve_method(Context *context, Decl *type, const char *method_name, Decl **ambiguous_ref, Decl **private_ref);
Decl *sema_find_extension_method_in_module(Module *module, Type *type, const char *method_name);
Decl *sema_resolve_normal_symbol(Context *context, TokenId symbol, Path *path, bool handle_error);
Decl *sema_resolve_string_symbol(Context *context, const char *symbol, SourceSpan span, Path *path, bool report_error);

bool sema_resolve_type(Context *context, Type *type);
bool sema_resolve_array_like_len(Context *context, TypeInfo *type_info, ArraySize *len_ref);
bool sema_resolve_type_info(Context *context, TypeInfo *type_info);
bool sema_resolve_type_info_maybe_inferred(Context *context, TypeInfo *type_info, bool allow_inferred_type);
bool sema_resolve_type_shallow(Context *context, TypeInfo *type_info, bool allow_inferred_type, bool in_shallow);
Type *sema_type_lower_by_size(Type *type, ArraySize element_size);

void sema_error_at_prev_end(Token token, const char *message, ...);

void sema_error_range(SourceSpan span, const char *message, ...);

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

void scratch_buffer_clear(void);
void scratch_buffer_append(const char *string);
void scratch_buffer_append_len(const char *string, size_t len);
void scratch_buffer_append_char(char c);
void scratch_buffer_append_signed_int(int64_t i);
void scratch_buffer_append_unsigned_int(uint64_t i);
char *scratch_buffer_to_string(void);
const char *scratch_buffer_interned(void);

const char *symtab_add(const char *symbol, uint32_t len, uint32_t fnv1hash, TokenType *type);
const char *symtab_find(const char *symbol, uint32_t len, uint32_t fnv1hash, TokenType *type);
void *llvm_target_machine_create(void);
void target_setup(BuildTarget *build_target);
int target_alloca_addr_space();


bool token_is_type(TokenType type);
bool token_is_any_type(TokenType type);
bool token_is_symbol(TokenType type);
bool token_is_ident_keyword(TokenType token_type);
bool token_is_ct_ident_keyword(TokenType token_type);
const char *token_type_to_string(TokenType type);
static inline TokenType advance_token(TokenId *token)
{
	TokenType tok;
	while (1)
	{
		token->index += 1;
		tok = TOKTYPE(*token);
		if (tok != TOKEN_COMMENT) return tok;
	}
}


AlignSize type_abi_alignment(Type *type);
AlignSize type_alloca_alignment(Type *type);
void type_append_signature_name(Type *type, char *dst, size_t *offset);
static inline bool type_convert_will_trunc(Type *destination, Type *source);
bool type_is_comparable(Type *type);
bool type_is_ordered(Type *type);
Type *type_find_common_ancestor(Type *left, Type *right);
Type *type_find_largest_union_element(Type *type);
Type *type_find_max_type(Type *type, Type *other);
Type *type_abi_find_single_struct_element(Type *type);
const char *type_generate_qname(Type *type);
bool type_is_valid_for_vector(Type *type);
Type *type_get_array(Type *arr_type, ArraySize len);
Type *type_get_indexed_type(Type *type);
Type *type_get_ptr(Type *ptr_type);
Type *type_get_ptr_recurse(Type *ptr_type);
Type *type_get_subarray(Type *arr_type);
Type *type_get_inferred_array(Type *arr_type);
Type *type_get_failable(Type *failable_type);
Type *type_get_vector(Type *vector_type, unsigned len);
Type *type_get_vector_bool(Type *original_type);
Type *type_cint(void);
Type *type_cuint(void);
Type *type_int_signed_by_bitsize(unsigned bitsize);
Type *type_int_unsigned_by_bitsize(unsigned bytesize);
static inline bool type_is_builtin(TypeKind kind);
bool type_is_abi_aggregate(Type *type);
static inline bool type_is_float(Type *type);
bool type_is_int128(Type *type);
Type *type_find_function_type(FunctionSignature *signature);
static inline bool type_is_integer(Type *type);
static inline bool type_is_integer_unsigned(Type *type);
static inline bool type_is_integer_signed(Type *type);
static inline bool type_is_integer_or_bool_kind(Type *type);
static inline bool type_is_numeric(Type *type);
static inline bool type_underlying_is_numeric(Type *type);
static inline bool type_is_pointer(Type *type);
static inline bool type_is_promotable_float(Type *type);
static inline bool type_is_promotable_integer(Type *type);
static inline bool type_is_signed(Type *type);
static inline bool type_is_structlike(Type *type);
static inline AlignSize type_min_alignment(AlignSize a, AlignSize b);
bool type_is_subtype(Type *type, Type *possible_subtype);
Type *type_from_token(TokenType type);
bool type_is_union_struct(Type *type);
bool type_is_user_defined(Type *type);
bool type_is_structurally_equivalent(Type *type1, Type *type);
static inline Type *type_flatten(Type *type);
static inline bool type_is_vector(Type *type) { return type_flatten(type)->type_kind == TYPE_VECTOR; };
bool type_is_float_or_float_vector(Type *type);
bool type_may_have_sub_elements(Type *type);
static inline bool type_ok(Type *type);
TypeSize type_size(Type *type);
const char *type_to_error_string(Type *type);
const char *type_quoted_error_string(Type *type);

static inline TypeInfo *type_info_new(TypeInfoKind kind, SourceSpan span);
static inline TypeInfo *type_info_new_base(Type *type, SourceSpan span);
static inline bool type_info_ok(TypeInfo *type_info);
static inline bool type_info_poison(TypeInfo *type);

static inline bool type_kind_is_signed(TypeKind kind);
static inline bool type_kind_is_unsigned(TypeKind kind);
static inline bool type_kind_is_any_integer(TypeKind kind);
static inline bool type_kind_is_derived(TypeKind kind);


// ---- static inline function implementations.


static inline Type *type_no_fail(Type *type)
{
	if (!type) return NULL;
	if (type->type_kind == TYPE_FAILABLE) return type->failable;
	if (type->type_kind == TYPE_FAILABLE_ANY) return type_void;
	return type;
}

static inline bool type_is_pointer_sized_or_more(Type *type)
{
	return type_is_integer(type) && type_size(type) >= type_size(type_iptr);
}

static inline bool type_is_pointer_sized(Type *type)
{
	return type_is_integer(type) && type_size(type) == type_size(type_iptr);
}

#define DECL_TYPE_KIND_REAL(k_, t_) \
 TypeKind k_ = (t_)->type_kind; \
 if (k_ == TYPE_TYPEDEF) k_ = (t_)->canonical->type_kind;

#define IS_FAILABLE(element_) (type_is_failable((element_)->type))
#define type_is_failable(type_) ((type_)->type_kind == TYPE_FAILABLE || (type_)->canonical->type_kind == TYPE_FAILABLE_ANY)
#define type_is_failable_type(type_) ((type_)->type_kind == TYPE_FAILABLE)
#define type_is_failable_any(type_) ((type_)->canonical->type_kind == TYPE_FAILABLE_ANY)
#define type_is_void(type_) (type_->canonical->type_kind == TYPE_VOID)

static inline Type *type_with_added_failability(Expr *expr, bool add_failable)
{
	Type *type = expr->type;
	if (!add_failable || type->type_kind == TYPE_FAILABLE) return type;
	return type_get_failable(type);
}

static inline Type *type_get_opt_fail(Type *type, bool add_failable)
{
	if (!add_failable || type->type_kind == TYPE_FAILABLE) return type;
	return type_get_failable(type);
}

static inline bool type_is_integer(Type *type)
{
	DECL_TYPE_KIND_REAL(kind, type);
	return kind >= TYPE_INTEGER_FIRST && kind <= TYPE_INTEGER_LAST;
}

static inline bool type_is_integer_signed(Type *type)
{
	DECL_TYPE_KIND_REAL(kind, type);
	return kind >= TYPE_INT_FIRST && kind <= TYPE_INT_LAST;
}

static inline bool type_is_integer_or_bool_kind(Type *type)
{
	DECL_TYPE_KIND_REAL(kind, type);
	return kind >= TYPE_BOOL && kind <= TYPE_U128;
}

static inline bool type_is_integer_unsigned(Type *type)
{
	DECL_TYPE_KIND_REAL(kind, type);
	return kind >= TYPE_UINT_FIRST && kind <= TYPE_UINT_LAST;
}

static inline bool type_info_poison(TypeInfo *type)
{
	type->type = poisoned_type;
	type->resolve_status = RESOLVE_DONE;
	return false;
}


static inline bool type_is_pointer(Type *type)
{
	DECL_TYPE_KIND_REAL(kind, type);
	return kind == TYPE_POINTER;
}

static inline AlignSize aligned_offset(AlignSize offset, AlignSize alignment)
{
	return ((offset + alignment - 1) / alignment) * alignment;
}

static inline bool type_is_substruct(Type *type)
{
	DECL_TYPE_KIND_REAL(kind, type);
	return kind == TYPE_STRUCT && type->decl->is_substruct;
}

static inline bool type_is_float(Type *type)
{
	DECL_TYPE_KIND_REAL(kind, type);
	return kind >= TYPE_FLOAT_FIRST && kind <= TYPE_FLOAT_LAST;
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
	global_context_add_type(type);
	return type;
}


static inline bool type_convert_will_trunc(Type *destination, Type *source)
{
	assert(type_is_builtin(destination->canonical->type_kind));
	assert(type_is_builtin(source->canonical->type_kind));
	return (unsigned)destination->canonical->builtin.bitsize < (unsigned)source->canonical->builtin.bitsize;
}


UnaryOp unaryop_from_token(TokenType type);
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

static inline Type *type_flatten_for_bitstruct(Type *type)
{
	type = type->canonical;
	RETRY:
	while (type->type_kind == TYPE_DISTINCT)
	{
		type = type->decl->distinct_decl.base_type;
	}
	if (type->type_kind == TYPE_ENUM)
	{
		type = type->decl->enums.type_info->type->canonical;
		goto RETRY;
	}
	return type;
}

static inline Type *type_flatten_distinct_failable(Type *type)
{
	while (1)
	{
		switch (type->type_kind)
		{
			case TYPE_TYPEDEF:
				type = type->canonical;
				continue;
			case TYPE_FAILABLE:
				type = type->failable;
				continue;
			case TYPE_DISTINCT:
				type = type->decl->distinct_decl.base_type;
				continue;
			default:
				return type;
		}
	}
}
static inline Type *type_flatten_distinct(Type *type)
{
	type = type->canonical;
	while (type->type_kind == TYPE_DISTINCT)
	{
		type = type->decl->distinct_decl.base_type;
	}
	return type;
}

static inline Type *type_flatten(Type *type)
{
	type = type->canonical;
	while (1)
	{
		if (type->type_kind == TYPE_DISTINCT)
		{
			type = type->decl->distinct_decl.base_type;
			continue;
		}
		if (type->type_kind == TYPE_ENUM)
		{
			type = type->decl->enums.type_info->type;
			continue;
		}
		if (type->type_kind == TYPE_FAILABLE)
		{
			type = type->failable;
			continue;
		}
		if (type->type_kind == TYPE_FAILABLE_ANY)
		{
			return type_void;
		}
		return type;
	}
}

static Type *type_vector_type(Type *type)
{
	Type *flatten = type_flatten(type);
	return flatten->type_kind == TYPE_VECTOR ? flatten->vector.base : NULL;
}

static inline bool type_is_builtin(TypeKind kind) { return kind >= TYPE_VOID && kind <= TYPE_TYPEID; }
static inline bool type_kind_is_signed(TypeKind kind) { return kind >= TYPE_I8 && kind < TYPE_U8; }
static inline bool type_kind_is_unsigned(TypeKind kind) { return kind >= TYPE_U8 && kind <= TYPE_U128; }
static inline bool type_kind_is_any_integer(TypeKind kind) { return kind >= TYPE_I8 && kind <= TYPE_U128; }
static inline bool type_is_signed(Type *type) { return type->type_kind >= TYPE_I8 && type->type_kind < TYPE_U8; }
static inline bool type_is_unsigned(Type *type) { return type->type_kind >= TYPE_U8 && type->type_kind <= TYPE_U128; }
static inline bool type_ok(Type *type) { return !type || type->type_kind != TYPE_POISONED; }
static inline bool type_info_ok(TypeInfo *type_info) { return !type_info || type_info->kind != TYPE_INFO_POISON; }
int type_kind_bitsize(TypeKind kind);
bool type_is_scalar(Type *type);

static inline bool type_is_numeric(Type *type)
{
	TypeKind kind = type->type_kind;
	return (kind >= TYPE_I8 && kind <= TYPE_FLOAT_LAST) || kind == TYPE_VECTOR;
}

static inline bool type_underlying_is_numeric(Type *type)
{
	return type_is_numeric(type_flatten(type));
}

static inline bool type_kind_is_derived(TypeKind kind)
{
	switch (kind)
	{
		case TYPE_ARRAY:
		case TYPE_POINTER:
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
			return true;
		default:
			return false;

	}
}


static inline Decl *decl_raw(Decl *decl)
{
	while (decl->decl_kind == DECL_DEFINE && decl->define_decl.define_kind == DEFINE_IDENT_ALIAS)
	{
		decl = decl->define_decl.alias;
	}
	if (decl->decl_kind != DECL_VAR || decl->var.kind != VARDECL_UNWRAPPED) return decl;
	decl = decl->var.alias;
	assert(decl->decl_kind != DECL_VAR || decl->var.kind != VARDECL_UNWRAPPED);
	return decl;
}

static inline bool decl_is_struct_type(Decl *decl)
{
	DeclKind kind = decl->decl_kind;
	return (kind == DECL_UNION) | (kind == DECL_STRUCT);
}

static inline bool decl_is_enum_kind(Decl *decl)
{
	DeclKind kind = decl->decl_kind;
	return (kind == DECL_ENUM) | (kind == DECL_ERRTYPE);
}

static inline bool decl_is_callable_type(Decl *decl)
{
	DeclKind kind = decl->decl_kind;
	return (kind == DECL_MACRO) | (kind == DECL_FUNC) | (kind == DECL_GENERIC);
}

static inline bool decl_is_user_defined_type(Decl *decl)
{
	DeclKind kind = decl->decl_kind;
	return (kind == DECL_UNION) | (kind == DECL_STRUCT) | (kind == DECL_BITSTRUCT)
			| (kind == DECL_ENUM) | (kind == DECL_DISTINCT);
}

static inline DeclKind decl_from_token(TokenType type)
{
	if (type == TOKEN_STRUCT) return DECL_STRUCT;
	if (type == TOKEN_UNION) return DECL_UNION;
	if (type == TOKEN_BITSTRUCT) return DECL_BITSTRUCT;
	UNREACHABLE
}

static inline bool type_is_promotable_integer(Type *type)
{
	// If we support other architectures, update this.
	return type_is_integer_or_bool_kind(type) && type->builtin.bitsize < platform_target.width_c_int;
}

static inline bool type_is_promotable_float(Type *type)
{
	// If we support other architectures, update this.
	return type_is_float(type->canonical) && type->builtin.bytesize < type_double->builtin.bytesize;
}

#define MACRO_COPY_DECL(x) x = copy_decl(x)
#define MACRO_COPY_DECL_LIST(x) x = copy_decl_list(x)
#define MACRO_COPY_EXPR(x) x = copy_expr(x)
#define MACRO_COPY_TYPE(x) x = copy_type_info(x)
#define MACRO_COPY_TYPE_LIST(x) x = type_info_copy_list_from_macro(x)
#define MACRO_COPY_EXPR_LIST(x) x = copy_expr_list(x)
#define MACRO_COPY_AST_LIST(x) x = copy_ast_list(x)
#define MACRO_COPY_AST(x) x = copy_ast(x)

Expr **copy_expr_list(Expr **expr_list);
Expr *copy_expr(Expr *source_expr);
Ast *copy_ast(Ast *source);
Ast **copy_ast_list(Ast **to_copy);
Decl *decl_copy_local_from_macro(Decl *to_copy);
Decl *copy_decl(Decl *decl);
Decl **copy_decl_list(Decl **decl_list);
TypeInfo *copy_type_info(TypeInfo *source);

/**
 * Minimum alignment, values are either offsets or alignments.
 * @return
 */
static inline AlignSize type_min_alignment(AlignSize a, AlignSize b)
{
	return (a | b) & (1 + ~(a | b));
}

bool obj_format_linking_supported(ObjectFormatType format_type);
bool linker(const char *output_file, const char **files, unsigned file_count);
void platform_linker(const char *output_file, const char **files, unsigned file_count);

#define CAT(a,b) CAT2(a,b) // force expand
#define CAT2(a,b) a##b // actually concatenate
#define TEMP(X) CAT(X, __LINE__)
#define ASSIGN_AST_ELSE(_assign, _ast_stmt, _res) Ast* TEMP(_ast) = (_ast_stmt); if (!ast_ok(TEMP(_ast))) return _res; _assign = TEMP(_ast)
#define ASSIGN_EXPR_ELSE(_assign, _expr_stmt, _res) Expr* TEMP(_expr) = (_expr_stmt); if (!expr_ok(TEMP(_expr))) return _res; _assign = TEMP(_expr)
#define ASSIGN_TYPE_ELSE(_assign, _type_stmt, _res) TypeInfo* TEMP(_type) = (_type_stmt); if (!type_info_ok(TEMP(_type))) return _res; _assign = TEMP(_type)
#define ASSIGN_DECL_ELSE(_assign, _decl_stmt, _res) Decl* TEMP(_decl) = (_decl_stmt); if (!decl_ok(TEMP(_decl))) return _res; _assign = TEMP(_decl)

static inline Type *abi_rtype(FunctionSignature *signature)
{
	Type *type = signature->rtype->type;
	if (type->type_kind == TYPE_FAILABLE) return type->failable;
	return type;
}

static inline Type *abi_returntype(FunctionSignature *signature)
{
	Type *type = signature->rtype->type;
	return type->type_kind == TYPE_FAILABLE ? type_anyerr : type;
}
