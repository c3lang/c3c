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

#define MAX_FIXUPS 0xFFFFF
#define MAX_HASH_SIZE (512 * 1024 * 1024)
#define INVALID_SPAN ((SourceSpan){ .row = 0 })
#define MAX_SCOPE_DEPTH 0x100
#define MAX_STRING_BUFFER 0x10000
#define INITIAL_SYMBOL_MAP 0x10000
#define INITIAL_GENERIC_SYMBOL_MAP 0x1000
#define MAX_MACRO_ITERATIONS 0xFFFFFF
#define MAX_PARAMS 127
#define MAX_BITSTRUCT 0x1000
#define MAX_MEMBERS ((MemberIndex)(((uint64_t)2) << 28))
#define MAX_ALIGNMENT ((MemberIndex)(((uint64_t)2) << 28))
#define MAX_PRIORITY 0xFFFF
#define MAX_TYPE_SIZE UINT32_MAX
#define MAX_GLOBAL_DECL_STACK (65536)
#define MAX_ASM_INSTRUCTION_PARAMS 6
#define CLOBBER_FLAG_ELEMENTS 4
#define MAX_CLOBBER_FLAGS (64 * CLOBBER_FLAG_ELEMENTS)

extern const char *project_default_keys[];
extern const int project_default_keys_count;
extern const char* project_target_keys[];
extern const int project_target_keys_count;

typedef struct Ast_ Ast;
typedef struct Decl_ Decl;
typedef struct TypeInfo_ TypeInfo;
typedef struct Expr_ Expr;
typedef struct Module_ Module;
typedef struct Type_ Type;
typedef Type CanonicalType;
typedef struct Signature_ Signature;
typedef struct ConstInitializer_ ConstInitializer;
typedef struct CompilationUnit_ CompilationUnit;
typedef unsigned AstId;
typedef unsigned ExprId;
typedef unsigned DeclId;
typedef unsigned TypeInfoId;


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



struct ConstInitializer_
{
	ConstInitType kind;
	// Type initialized
	Type *type;
	union
	{
		ConstInitializer **init_struct;
		Expr *init_value;
		struct
		{
			ConstInitializer *element;
			MemberIndex index;
		} init_union;
		struct
		{
			ConstInitializer **elements;
		} init_array;
		ConstInitializer **init_array_full;
		struct
		{
			ConstInitializer *element;
			MemberIndex index;
		} init_array_value;
	};
};

typedef struct
{
	char string[1024];
	unsigned constraint_len;
} ClobberList;


typedef struct
{
	bool is_write : 1;
	bool is_readwrite : 1;
	bool is_address : 1;
	AsmArgBits imm_arg_ubits : 16;
	AsmArgBits imm_arg_ibits : 16;
	AsmArgBits ireg_bits : 16;
	AsmArgBits float_bits : 16;
	AsmArgBits vec_bits : 16;
} AsmArgType;

typedef struct
{
	const char *name;
	AsmRegisterType type;
	AsmArgBits bits;
	int clobber_index;
} AsmRegister;

typedef struct
{
	uint64_t mask[CLOBBER_FLAG_ELEMENTS];
} Clobbers;

typedef struct
{
	const char *name;
	AsmArgType param[MAX_ASM_INSTRUCTION_PARAMS];
	unsigned param_count;
	Clobbers mask;
} AsmInstruction;

#define ASM_INSTRUCTION_MAX 0x1000
#define ASM_INSTRUCTION_MASK (ASM_INSTRUCTION_MAX - 1)
#define ASM_REGISTER_MAX 4096
#define ASM_REGISTER_MASK (ASM_REGISTER_MAX - 1)

typedef struct
{
	bool initialized;
	const char **clobber_name_list;
	const char *extra_clobbers;
	AsmRegister registers[ASM_REGISTER_MAX];
	AsmInstruction instructions[ASM_INSTRUCTION_MAX];
	unsigned register_count;
} AsmTarget;

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
		uint64_t ptr;
		bool b;
		struct
		{
			const char *chars;
			ArraySize len;
		} string;
		Decl *enum_val;
		Decl *err_val;
		struct
		{
			const char *ptr;
			TypeSize len;
		} bytes;
		Type *typeid;
		ConstInitializer *initializer;
		Expr **untyped_list;
		struct
		{
			AlignSize offset;
			AlignSize align;
			Decl *decl;
		} member;
	};
} ExprConst;


typedef uint16_t FileId;
typedef struct
{
	FileId file_id;
	const char *contents;
	char *name;
	char *dir_path;
	const char *full_path;
} File;

typedef union
{
	struct
	{
		FileId file_id;
		unsigned char length;
		unsigned char col;
		uint32_t row;
	};
	uint64_t a;
} SourceSpan;

extern File stdin_file;
#define stdin_file_id 0xFFFF

static_assert(sizeof(SourceSpan) == 8, "Expected 8 bytes");

typedef struct
{
	const char *key;
	void *value;
} SEntry;

typedef struct
{
	uint32_t count;
	uint32_t capacity;
	uint32_t max_load;
	SEntry *entries;
} STable;

typedef struct SEntry2_
{
	const char *key;
	void *value;
	struct SEntry2_ *next;
} HTEntry;

typedef struct
{
	uint32_t mask;
	HTEntry **entries;
} HTable;


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
	Type *base;
	ArraySize len;
} TypeArray;

typedef struct
{
	bool nodiscard : 1;
	bool maydiscard : 1;
	bool is_pure : 1;
	bool noreturn : 1;
} CalleeAttributes;

typedef struct
{
	Module *module;
	Signature *signature;
	struct FunctionPrototype_ *prototype;
} TypeFunction;

struct Type_
{
	TypeKind type_kind;
	CanonicalType *canonical;
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
		// Type[], Type[*], Type[123], Type[<123>] or Type<[123]>
		TypeArray array;
		// fn TypeR Type1(Type2, Type3, ...)
		TypeFunction function;
		// Type*
		Type *pointer;
		// Failable
		Type *failable;
		// Bitstruct
		Type *bitstruct;
	};
};

typedef enum
{
	TYPE_COMPRESSED_NONE = 0,
	TYPE_COMPRESSED_PTR = 1,
	TYPE_COMPRESSED_SUB = 2,
	TYPE_COMPRESSED_SUBPTR = 3,
	TYPE_COMPRESSED_PTRPTR = 4,
	TYPE_COMPRESSED_PTRSUB = 5,
	TYPE_COMPRESSED_SUBSUB = 6,
} TypeInfoCompressedKind;

struct TypeInfo_
{
	ResolveStatus resolve_status : 3;
	TypeInfoKind kind : 6;
	bool failable : 1;
	TypeInfoCompressedKind subtype : 4;
	Type *type;
	SourceSpan span;
	union
	{
		struct
		{
			const char *name;
			Path *path;
		} unresolved;
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
	const char *name;
	SourceSpan span;
	AttributeType attr_kind : 8;
	bool is_custom : 1;
	Expr **exprs;
} Attr;

typedef struct
{
	Path *path;
	bool private;
	Module *module;
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
	bool unwrap : 1;
	bool shadow : 1;
	bool vararg : 1;
	bool is_static : 1;
	bool is_read : 1;
	bool not_null : 1;
	bool out_param : 1;
	bool in_param : 1;
	bool is_written : 1;
	bool is_addr : 1;
	bool is_threadlocal : 1;
	bool no_init : 1;
	TypeInfo *type_info;
	union
	{
		Expr *init_expr;
		Decl *alias;
	};
	union
	{
		int32_t index;
		struct
		{
			struct SemaContext_ *context;
			SourceSpan span;
		} hash_var;
		unsigned scope_depth; // CT var
		struct
		{
			void *backend_debug_ref;
			union
			{
				// Variable
				void *failable_ref;
				int tb_failable_reg;
			};
		};
		struct
		{
			union
			{
				struct
				{
					Expr *start;
					Expr *end;
				};
				struct
				{
					unsigned start_bit;
					unsigned end_bit;
				};
			};
		};
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
	Expr *expr;
	Expr *to_expr;
	Decl **body;
} CtCaseDecl;

typedef struct
{
	Expr **args;
	uint32_t ordinal;
	DeclId parent;
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
	VARIADIC_ANY,
	VARIADIC_RAW,
} Variadic;



struct Signature_
{
	CalleeAttributes attrs;
	bool is_macro : 1;
	bool is_at_macro : 1;
	Variadic variadic : 3;
	CallABI abi : 8;
	unsigned vararg_index;
	TypeInfoId rtype;
	Decl** params;
};



typedef struct
{
	TypeInfoId type_parent;
	Signature signature;
	AstId body;
	AstId docs;
	union
	{
		struct
		{
			bool attr_inline : 1;
			bool attr_noinline : 1;
			bool attr_extname : 1;
			bool attr_naked : 1;
		};
		struct
		{
			DeclId body_param;
			CompilationUnit *unit;
		};
	};
} FuncDecl;


typedef struct
{
	Decl **params;
	Attr **attrs;
} AttrDecl;

typedef struct
{
	bool is_func : 1;
	bool is_distinct : 1;
	union
	{
		Signature function_signature;
		TypeInfo *type_info;
	};
} TypedefDecl;


typedef struct
{
	union
	{
		TypedefDecl typedef_decl;
		Type *base_type;
	};
} DistinctDecl;



typedef enum
{
	DEFINE_TYPE_GENERIC,
	DEFINE_IDENT_ALIAS,
	DEFINE_IDENT_GENERIC,
} DefineType;

typedef struct
{
	DefineType define_kind: 5;
	union
	{
		struct
		{
			union
			{
				TypeInfo *type_info;
				struct
				{
					Path *path;
					const char *ident;
					SourceSpan span;
				};
			};
			Expr **generic_params;
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

typedef struct
{
	unsigned priority;
	AstId init;
} InitializerDecl;



typedef struct Decl_
{
	const char *name;
	const char *extname;
	SourceSpan span;
	DeclKind decl_kind : 7;
	Visibility visibility : 3;
	ResolveStatus resolve_status : 3;
	bool is_packed : 1;
	bool is_substruct : 1;
	bool has_variable_array : 1;
	bool is_value : 1;
	bool is_autoimport : 1;
	bool has_extname : 1;
	bool is_external_visible : 1;
	bool is_weak : 1;
	bool is_maybe_unused : 1;
	bool is_must_use : 1;
	bool will_reflect : 1;
	bool obfuscate : 1;
	bool is_dynamic : 1;
	bool is_synthetic : 1;
	OperatorOverload operator : 4;
	union
	{
		void *backend_ref;
		int tb_register;
		void *backend_value;
	};
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
	struct CompilationUnit_ *unit;
	Attr** attributes;
	Type *type;
	union
	{
		InitializerDecl xxlizer;
		Decl** decl_list;
		struct
		{
			Decl **methods;
			union
			{
				// Unions, Struct use strukt
				StructDecl strukt;
				// Enums and Fault
				EnumDecl enums;
				DistinctDecl distinct_decl;
				BitStructDecl bitstruct;
			};
		};
		Decl** body_params;
		ImportDecl import;
		VarDecl var;
		LabelDecl label;
		EnumConstantDecl enum_constant;
		FuncDecl func_decl;
		AttrDecl attr_decl;
		TypedefDecl typedef_decl;
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
	bool start_from_end : 1;
	bool end_from_end : 1;
	bool is_len : 1;
	bool is_range : 1;
	ExprId start;
	ExprId end;
} Range;

typedef struct
{
	ExprId cond;
	ExprId then_expr; // May be null for elvis!
	ExprId else_expr;
	bool widen : 1;
} ExprTernary;

typedef struct
{
	ExprId left;
	ExprId right;
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
	bool splat_vararg : 1;
	bool attr_force_inline : 1;
	bool attr_force_noinline : 1;
	bool is_builtin : 1;
	bool is_func_ref : 1;
	bool attr_pure : 1;
	bool result_unused : 1;
	AstId body;
	union
	{
		ExprId function;
		DeclId func_ref;
	};
	Expr **arguments;
	union
	{
		Expr **varargs;
		Expr *splat;
	};
	Decl **body_arguments;
} ExprCall;

typedef struct
{
	ExprId expr;
	Range range;
} ExprSubscript;

typedef struct
{
	ExprId expr;
	DeclId method;
	ExprId index;
} ExprSubscriptAssign;

typedef struct
{
	ExprId left;
	ExprId right;
} ExprSliceAssign;

typedef struct
{
	ExprId ptr;
	ExprId type_id;
} ExprVariant;

typedef struct
{
	bool raw_offset : 1;
	ExprId ptr;
	ExprId offset;
} ExprPointerOffset;

typedef enum
{
	ACCESS_LEN,
	ACCESS_PTR,
	ACCESS_TYPEOFANY,
	ACCESS_ENUMNAME,
	ACCESS_FAULTNAME,
} BuiltinAccessKind;

typedef struct
{
	BuiltinAccessKind kind : 8;
	ExprId inner;
} ExprBuiltinAccess;
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
	union
	{
		struct
		{
			Path *path;
			const char *ident;
			bool is_const;
		};
		Decl *decl;
	};
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
	bool array : 1;
	Expr *inner;
} ExprFlatElement;

typedef struct
{
	TokenType token_type;
	union
	{
		struct
		{
			Expr *main_var;
			ExprFlatElement *flat_path;
		};
		struct
		{
			TypeInfoId type_from;
			TypeInfoId type_to;
		};
	};
} ExprCtCall;


typedef enum
{
	ASM_SCALE_1,
	ASM_SCALE_2,
	ASM_SCALE_4,
	ASM_SCALE_8,
	ASM_SCALE_SHR,
	ASM_SCALE_SHL,
	ASM_SCALE_ASHL,
	ASM_SCALE_ROR,
	ASM_SCALE_RRX,
} AsmOffsetType;
typedef struct
{
	AsmArgKind kind : 8;
	unsigned short index : 16;
	AsmOffsetType offset_type : 6;
	bool neg_offset : 1;
	union
	{
		struct {
			union
			{
				const char *name;
				Decl *ident_decl;
			};
			bool copy_output : 1;
			bool early_clobber : 1;
			bool is_input : 1;
		} ident;
		ExprId expr_id;
		struct {
			ExprId base;
			ExprId idx;
			uint64_t offset;
		};
		uint64_t value;
		union
		{
			const char *name;
			AsmRegister *ref;
		} reg;
	};
} ExprAsmArg;


typedef struct
{
	TokenType type : 16;
	ExprId arg;
} ExprCtArg;

typedef struct
{
	CastKind kind : 8;
	bool implicit : 1;
	ExprId expr;
	TypeInfoId type_info;
} ExprCast;

typedef struct
{
	Expr **values;
	Decl **declarations;
	AstId first_stmt;
} ExprBodyExpansion;


typedef struct
{
	void *block_return_exit;
	void *block_failable_exit;
	void *block_error_var;
	void *block_return_out;
} BlockExit;

typedef struct
{
	AstId first_stmt;
	BlockExit **block_exit_ref;
} ExprFuncBlock;


typedef struct
{
	AstId first_stmt;
	Decl **params;
	BlockExit **block_exit;
} ExprMacroBlock;



typedef struct
{
	Expr *initializer;
	TypeInfo *type_info;
} ExprCompoundLiteral;

typedef struct
{
	const char *name;
	SourceSpan span;
} Label;

typedef struct
{
	Expr *inner;
	AstId cleanup;
} ExprGuard;


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
	const char *ident;
	BuiltinFunction builtin;
} ExprBuiltin;

typedef struct
{
	bool is_assign : 1;
	bool is_deref : 1;
	union
	{
		struct
		{
			const char *new_ident;
			SourceSpan span;
			Expr *variant_expr;
		};
		Decl *variable;
	};
} ExprVariantSwitch;

typedef struct
{
	ExprId parent;
	TypeIdInfoKind kind;
} ExprTypeidInfo;

typedef struct
{
	Decl *argc;
	Decl *argv;
} ExprArgv;


struct Expr_
{
	Type *type;
	SourceSpan span;
	ExprKind expr_kind : 8;
	ResolveStatus resolve_status : 4;
	union {
		Range vasplat_expr;
		ExprTypeidInfo typeid_info_expr;
		ExprVariantSwitch variant_switch;           // 32
		ExprCast cast_expr;                         // 12
		ExprVariant variant_expr;
		ExprPointerOffset pointer_offset_expr;
		ExprAsmArg expr_asm_arg;
		OperatorOverload expr_operator_chars;
		TypeInfo *type_expr;                        // 8
		ExprConst const_expr;                       // 32
		ExprArgv argv_expr;                         // 16
		ExprGuard rethrow_expr;                     // 16
		Decl *decl_expr;                            // 8
		ExprSliceAssign slice_assign_expr;          // 8
		ExprBinary binary_expr;                     // 12
		ExprTernary ternary_expr;                   // 16
		ExprUnary unary_expr;                       // 16
		Expr** try_unwrap_chain_expr;               // 8
		ExprTryUnwrap try_unwrap_expr;              // 24
		ExprCall call_expr;                         // 32
		Expr *inner_expr;                           // 8
		ExprBuiltinAccess builtin_access_expr;
		ExprCatchUnwrap catch_unwrap_expr;          // 24
		ExprSubscript subscript_expr;               // 12
		ExprSubscriptAssign subscript_assign_expr;
		ExprAccess access_expr;                     // 16
		ExprDesignator designator_expr;             // 16
		ExprIdentifier identifier_expr;             // 24
		ExprIdentifierRaw ct_ident_expr;            // 24
		ExprCtCall ct_call_expr;                    // 24
		ExprCtArg ct_arg_expr;
		ExprIdentifierRaw hash_ident_expr;          // 24
		TypeInfo *typeid_expr;                      // 8
		ExprBodyExpansion body_expansion_expr;      // 24
		ExprCompoundLiteral expr_compound_literal;  // 16
		Expr** expression_list;                     // 8
		Expr** initializer_list;                    // 8
		Expr** designated_init_list;                // 8
		ExprFuncBlock expr_block;                   // 4
		ExprMacroBlock macro_block;                 // 24
		Expr** cond_expr;                           // 8
		ExprBuiltin builtin_expr;                   // 16
	};
};
//static_assert(sizeof(ExprConst) == 32, "Not expected size");

//static_assert(sizeof(Expr) == 56, "Expr not expected size");

typedef struct
{
	AstId first_stmt;
} AstCompoundStmt;


typedef struct
{
	Expr *expr; // May be NULL
	AstId cleanup;
	BlockExit** block_exit_ref; // For block exits
} AstReturnStmt;

typedef struct
{
	bool has_break : 1;
	bool no_exit : 1;
	bool skip_first : 1;
	bool if_chain : 1;
	Decl *label;
} FlowCommon;



typedef struct
{
	FlowCommon flow;
	union
	{
		struct
		{
			ExprId cond;
			AstId then_body;
			AstId else_body;
		};
		struct
		{
			void *break_block;
		} codegen;
	};
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
	union
	{
		struct
		{
			ExprId cond;
			AstId defer;
			Ast **cases;
			Ast *scope_defer;
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
	union
	{
		struct
		{
			ExprId cond;
			ExprId incr;
			ExprId init;
			AstId body;
		};
		struct
		{
			void *continue_block;
			void *exit_block;
		} codegen;
	};
} AstForStmt;

typedef struct
{
	FlowCommon flow;
	bool index_by_ref : 1;
	bool value_by_ref : 1;
	bool iterator : 1;
	bool is_reverse : 1;
	CastKind cast : 8;
	ExprId enumeration;
	AstId body;
	DeclId index;
	DeclId variable;
} AstForeachStmt;

typedef struct
{
	AstId prev_defer;
	AstId body; // Compound statement
} AstDeferStmt;



typedef struct AstCtIfStmt_
{
	Expr *expr;
	AstId elif;
	AstId then;
} AstCtIfStmt;


typedef struct
{
	ExprId cond;
	Ast **body;
} AstCtSwitchStmt;


typedef struct
{
	const char *index_name;
	const char *value_name;
	SourceSpan index_span;
	SourceSpan value_span;
	AstId body;
	ExprId expr;
} AstCtForeachStmt;


typedef struct
{
	bool is_label : 1;
	bool is_resolved : 1;
	AstId defers;
	union
	{
		Label label;
		AstId ast;
	};
} AstContinueBreakStmt;

typedef struct
{
	AstId defer_id;
	union
	{
		struct
		{
			Label label;
			Expr *expr;
		};
		struct
		{
			AstId case_switch_stmt;
			Expr *switch_expr;
		};
	};
} AstNextcaseStmt;

typedef struct
{
	const char *instruction;
	const char *variant;
	Expr **args;
} AstAsmStmt;

typedef struct
{
	Clobbers clobbers;
	const char *asm_block;
	AstId asm_stmt;
	ExprAsmArg **output_vars;
	ExprAsmArg **input;
} AsmInlineBlock;

typedef struct
{
	bool is_volatile : 1;
	bool is_inline : 1;
	bool is_goto : 1;
	bool is_string : 1;
	union
	{
		AsmInlineBlock *block;
		ExprId asm_string;
	};

} AstAsmBlock;

typedef struct
{
	bool is_ensure;
	ExprId message;
	ExprId expr;
} AstAssertStmt;


typedef struct
{
	union
	{
		struct
		{
			SourceSpan span;
			TypeInfo *type;
			const char *ident;
		};
		Decl *decl;
	};
} DocOptReturn;

typedef struct AstDocDirective_
{
	SourceSpan span;
	DocDirectiveKind kind : 4;
	union
	{
		struct
		{
			const char *name;
			SourceSpan span;
			InOutModifier modifier : 4;
			bool by_ref : 1;
		} param;
		DocOptReturn *optreturns;
		struct
		{
			Expr *decl_exprs;
			const char *comment;
			const char *expr_string;
		} contract;
		struct
		{
			const char *directive_name;
			const char *rest_of_line;
		} generic;
	};
} AstDocStmt;

typedef struct Ast_
{
	SourceSpan span;
	AstId next;
	AstKind ast_kind : 8;
	union
	{
		FlowCommon flow;                    // Shared struct
		AstAsmBlock asm_block_stmt;
		AstAsmStmt asm_stmt;
		AstCompoundStmt compound_stmt;      // 12
		Decl *declare_stmt;                 // 8
		Expr *expr_stmt;                    // 8
		Decl *var_stmt;                     // 8
		AstReturnStmt return_stmt;          // 16
		AstIfStmt if_stmt;                  // 32
		AstDeferStmt defer_stmt;            // 8
		AstSwitchStmt switch_stmt;          // 40
		AstCaseStmt case_stmt;              // 32
		AstCtSwitchStmt ct_switch_stmt;     // 16
		AstContinueBreakStmt contbreak_stmt;// 24
		AstNextcaseStmt nextcase_stmt;      // 32
		AstForStmt for_stmt;                // 32
		AstForeachStmt foreach_stmt;        // 40
		AstCtIfStmt ct_if_stmt;             // 24
		AstId ct_else_stmt;                 // 4
		AstCtForeachStmt ct_foreach_stmt;   // 40
		AstAssertStmt assert_stmt;          // 16
		AstDocStmt doc_stmt;
	};
} Ast;

//static_assert(sizeof(AstContinueBreakStmt) == 24, "Ooops");
//static_assert(sizeof(Ast) == 56, "Not expected size on 64 bit");

typedef struct Module_
{
	Path *name;
	// Extname in case a module is renamed externally
	const char *extname;

	const char **parameters;
	bool is_external : 1;
	bool is_c_library : 1;
	bool is_exported : 1;
	bool is_generic : 1;
	bool is_private : 1;
	bool no_extprefix : 1;
	AnalysisStage stage : 6;

	AstId docs;
	Decl** method_extensions;
	HTable symbols;
	struct CompilationUnit_ **units;
	Module *parent_module;
	Module *top_module;
	Module **sub_modules;
} Module;



typedef struct DynamicScope_
{
	ScopeId scope_id;
	bool allow_dead_code : 1;
	bool jump_end : 1;
	ScopeFlags flags;
	unsigned label_start;
	unsigned current_local;
	AstId defer_last;
	AstId defer_start;
	Ast *in_defer;
	unsigned depth;
} DynamicScope;


typedef struct
{
	const char *lex_start;
	size_t lex_len;
	union
	{
		struct
		{
			const char *string;
			size_t strlen;
		};
		struct
		{
			Float value;
		};
		struct
		{
			bool is_base64 : 1;
			uint64_t bytes_len : 63;
		};
		struct
		{
			Int128 char_value;
			char width;
		};
	};
} TokenData;

typedef struct
{
	struct ParseContext_ *context;
	const char *file_begin;
	const char *lexing_start;
	const char *current;
	uint32_t current_row;
	uint32_t start_row;
	const char *line_start;
	const char *start_row_start;
	File *file;
	TokenData data;
	SourceSpan tok_span;
	TokenType token_type;
	LexMode mode;
} Lexer;

typedef struct
{
	uint32_t count;
	uint32_t capacity;
	uint32_t max_load;
	DeclId *entries;
} DeclTable;

struct CompilationUnit_
{
	Module *module;
	File* file;
	Decl** imports;
	Decl **types;
	Decl **functions;
	Decl **enums;
	Decl **attributes;
	Decl **faulttypes;
	struct
	{
		// Not properly implemented
		Decl **generic_methods;
		Decl **generics;
		Decl **generic_defines;
	};
	Decl **ct_ifs;
	Decl **ct_asserts;
	Decl **xxlizers;
	Decl **vars;
	Decl **macros;
	Decl **methods;
	Decl **macro_methods;
	Decl **global_decls;
	Decl *main_function;
	HTable local_symbols;
	struct
	{
		void *debug_file;
		void *debug_compile_unit;
	} llvm;
};

typedef struct ParseContext_
{
	TokenData data;
	TokenType tok;
	SourceSpan span;
	SourceSpan prev_span;
	CompilationUnit *unit;
	Lexer lexer;
} ParseContext;

typedef enum
{
	CALL_ENV_GLOBAL_INIT,
	CALL_ENV_FUNCTION,
	CALL_ENV_INITIALIZER,
	CALL_ENV_FINALIZER,
	CALL_ENV_CHECKS,
	CALL_ENV_ATTR,
} CallEnvKind;

typedef struct
{
	CallEnvKind kind : 8;
	bool ensures : 1;
	bool pure : 1;
	union
	{
		Decl *attr_declaration;
		Decl *current_function;
	};
} CallEnv;

typedef struct SemaContext_
{
	Module *core_module;
	// Evaluated in this.
	CompilationUnit *unit;
	// Compiled in this unit.
	CompilationUnit *compilation_unit;
	CallEnv call_env;
	Decl *current_macro;
	ScopeId scope_id;
	Ast *break_target;
	AstId break_defer;
	Ast *continue_target;
	AstId continue_defer;
	AstId block_return_defer;
	Ast *next_target;
	Ast *next_switch;
	AstId next_defer;
	struct
	{
		uint32_t original_inline_line;
		Decl **yield_params;
		Ast *yield_body;
		BlockExit** block_exit_ref;
		Type *expected_block_type;
		Ast **returns;
		// Reusable returns cache.
		Ast **returns_cache;
		Expr **macro_varargs;
		Decl **macro_params;
	};
	Type *rtype;
	struct SemaContext_ *yield_context;
	Decl** locals;
	DynamicScope active_scope;
	Expr *return_expr;
} SemaContext;


typedef struct
{
	HTable modules;
	Module *core_module;
	Module **module_list;
	Module **generic_module_list;
	Type **type;
	const char *lib_dir;
	const char **sources;
	File **loaded_sources;
	bool in_panic_mode : 1;
	bool in_test_mode : 1;
	unsigned errors_found;
	unsigned warnings_found;
	bool suppress_errors;
	Decl ***locals_list;
	HTable compiler_defines;
	Module std_module;
	DeclTable symbols;
	DeclTable generic_symbols;
	Path std_module_path;
	Decl *panic_fn;
	Decl *main;
	Decl *decl_stack[MAX_GLOBAL_DECL_STACK];
	Decl** decl_stack_bottom;
	Decl** decl_stack_top;
} GlobalContext;


typedef enum
{
	ABI_ARG_IGNORE,
	ABI_ARG_DIRECT,
	ABI_ARG_DIRECT_PAIR,
	ABI_ARG_DIRECT_COERCE,
	ABI_ARG_DIRECT_COERCE_INT,
	ABI_ARG_DIRECT_SPLIT_STRUCT,
	ABI_ARG_EXPAND_COERCE,
	ABI_ARG_INDIRECT,
	ABI_ARG_EXPAND,
}  ABIKind;

typedef struct
{
	union
	{
		Type *type;
		uintptr_t int_bits_plus_1;
	};
} AbiType;

#define ABI_TYPE_EMPTY ((AbiType) { .type = NULL })

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
			AbiType lo;
			AbiType hi;
		} direct_pair;
		struct
		{
			uint8_t offset_lo;
			uint8_t padding_hi;
			uint8_t lo_index;
			uint8_t hi_index;
			uint8_t offset_hi;
			bool packed : 1;
			AbiType lo;
			AbiType hi;
		} coerce_expand;
		Type *direct_coerce_type;
		struct
		{
			Type *type;
			uint8_t elements;
		} direct_struct_expand;
		struct
		{
			// We may request a certain alignment of the parameters.
			AlignSize alignment;
			Type *type;
		} indirect;
	};

} ABIArgInfo;

typedef struct FunctionPrototype_
{
	CallABI call_abi : 4;
	Variadic variadic : 3;
	bool use_win64 : 1;
	bool is_failable : 1;
	bool ret_by_ref : 1;
	unsigned short vararg_index;
	Type *rtype;
	Type **param_types;
	Decl **param_copy;
	Type **varargs;
	Type *ret_by_ref_type;
	Type *abi_ret_type;
	ABIArgInfo *ret_abi_info;
	ABIArgInfo *ret_by_ref_abi_info;
	ABIArgInfo **abi_args;
	ABIArgInfo **abi_varargs;
	union
	{
		void *tb_prototype;
		void *llvm_prototype;
	};
	Type *raw_type;
} FunctionPrototype;

typedef struct
{
	Decl *ambiguous_other_decl;
	Decl *private_decl;
	Decl *maybe_decl;
	Path *path;
	SourceSpan span;
	const char *symbol;
	bool path_found;
	bool suppress_error;
} NameResolve;

typedef struct
{
	void *original;
	void *new_ptr;
} CopyFixup;

typedef struct CopyStruct_
{
	CopyFixup fixups[MAX_FIXUPS];
	CopyFixup *current_fixup;
	bool single_static;
	bool copy_in_use;
} CopyStruct;


extern GlobalContext global_context;
extern AsmTarget asm_target;
extern BuildTarget active_target;
extern Ast *poisoned_ast;
extern Decl *poisoned_decl;
extern Expr *poisoned_expr;
extern Type *poisoned_type;
extern TypeInfo *poisoned_type_info;


extern Type *type_bool, *type_void, *type_voidptr;
extern Type *type_float16, *type_float, *type_double, *type_f128;
extern Type *type_ichar, *type_short, *type_int, *type_long, *type_isize, *type_isz;
extern Type *type_char, *type_ushort, *type_uint, *type_ulong, *type_usize, *type_usz;
extern Type *type_iptr, *type_uptr, *type_iptrdiff, *type_uptrdiff;
extern Type *type_u128, *type_i128;
extern Type *type_typeid, *type_anyerr, *type_typeinfo, *type_member;
extern Type *type_any;
extern Type *type_untypedlist;
extern Type *type_anyfail;
extern Type *type_cint;
extern Type *type_cuint;
extern Type *type_chars;


extern const char *attribute_list[NUMBER_OF_ATTRIBUTES];
extern const char *builtin_list[NUMBER_OF_BUILTINS];
extern const char *builtin_defines[NUMBER_OF_BUILTIN_DEFINES];
extern const char *type_property_list[NUMBER_OF_TYPE_PROPERTIES];
extern const char *kw_std__core;
extern const char *kw_std__core__types;
extern const char *kw_typekind;

extern const char *kw_argc;
extern const char *kw_argv;
extern const char *kw_at_checked;
extern const char *kw_at_ensure;
extern const char *kw_at_optreturn;
extern const char *kw_at_param;
extern const char *kw_at_pure;
extern const char *kw_at_require;
extern const char *kw_at_return;
extern const char *kw_check_assign;
extern const char *kw_deprecated;
extern const char *kw_distinct;
extern const char *kw_finalize;
extern const char *kw_in;
extern const char *kw_incr;
extern const char *kw_initialize;
extern const char *kw_inline;
extern const char *kw_inout;
extern const char *kw_kind;
extern const char *kw_len;
extern const char *kw_main;
extern const char *kw_mainstub;
extern const char *kw_nameof;
extern const char *kw_noinline;
extern const char *kw_offsetof;
extern const char *kw_ordinal;
extern const char *kw_out;
extern const char *kw_ptr;
extern const char *kw_pure;
extern const char *kw_return;
extern const char *kw_std;
extern const char *kw_type;
extern ArchOsTarget default_target;

ARENA_DEF(chars, char)
ARENA_DEF(ast, Ast)
ARENA_DEF(expr, Expr)
ARENA_DEF(decl, Decl)
ARENA_DEF(type_info, TypeInfo)


INLINE Type *typeinfotype(TypeInfoId id_)
{
	return type_infoptr(id_)->type;
}

bool ast_is_not_empty(Ast *ast);
bool ast_is_compile_time(Ast *ast);
bool ast_supports_continue(Ast *stmt);
INLINE void ast_append(AstId **succ, Ast *next);
INLINE void ast_prepend(AstId *first, Ast *ast);
INLINE bool ast_ok(Ast *ast);
INLINE bool ast_poison(Ast *ast);
INLINE Ast *ast_last(Ast *ast);
INLINE Ast *new_ast(AstKind kind, SourceSpan range);
INLINE Ast *ast_next(AstId *current_ptr);

const char *span_to_string(SourceSpan span);
void span_to_scratch(SourceSpan span);

static inline SourceSpan extend_span_with_token(SourceSpan loc, SourceSpan after)
{
	if (loc.row != after.row) return loc;
	loc.length = after.col + after.length - loc.col;
	return loc;
}


typedef enum CmpRes_
{
	CMP_LT = -1,
	CMP_EQ = 0,
	CMP_GT = 1,
} CmpRes;

AttributeType attribute_by_name(const char *name);

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
unsigned int_bits_needed(Int op);
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
UNUSED int i128_lsb(const Int128 *op);
UNUSED int i128_msb(const Int128 *op);
Int128 i128_from_float_signed(Real d);
Int128 i128_from_float_unsigned(Real d);
Int128 i128_from_signed(int64_t i);
UNUSED Int128 i128_from_unsigned(uint64_t i);
UNUSED bool i128_get_bit(const Int128 *op, int bit);

// copy ---

#define MACRO_COPY_DECL(x) x = copy_decl(c, x)
#define MACRO_COPY_DECLID(x) x = declid_copy_deep(c, x)
#define MACRO_COPY_DECL_LIST(x) x = copy_decl_list(c, x)
#define MACRO_COPY_EXPR(x) x = copy_expr(c, x)
#define MACRO_COPY_EXPRID(x) x = exprid_copy_deep(c, x)
#define MACRO_COPY_TYPE(x) x = copy_type_info(c, x)
#define MACRO_COPY_TYPEID(x) x = type_info_id_copy_deep(c, x)
#define MACRO_COPY_TYPE_LIST(x) x = type_info_copy_list_from_macro(c, x)
#define MACRO_COPY_EXPR_LIST(x) x = copy_expr_list(c, x)
#define MACRO_COPY_AST_LIST(x) x = copy_ast_list(c, x)
#define MACRO_COPY_AST(x) x = ast_copy_deep(c, x)
#define MACRO_COPY_ASTID(x) x = astid_copy_deep(c, x)


void copy_begin(void);
void copy_end(void);
Expr *copy_expr_single(Expr *source_expr);
Decl **copy_decl_list_single(Decl **decl_list);
Ast *copy_ast_single(Ast *source_ast);
Decl **copy_decl_list_macro(Decl **decl_list);
Ast *copy_ast_macro(Ast *source_ast);
Ast *copy_ast_defer(Ast *source_ast);

void init_asm(void);
AsmRegister *asm_reg_by_name(const char *name);
AsmInstruction *asm_instr_by_name(const char *name);
INLINE const char *asm_clobber_by_index(unsigned index);
INLINE AsmRegister *asm_reg_by_index(unsigned index);

AsmRegister *asm_reg_by_index(unsigned index);

bool cast_implicit(SemaContext *context, Expr *expr, Type *to_type);
bool cast(Expr *expr, Type *to_type);
bool cast_may_implicit(Type *from_type, Type *to_type, CastOption option);
bool cast_may_explicit(Type *from_type, Type *to_type, bool ignore_failability, bool is_const);
Type *cast_infer_len(Type *to_infer, Type *actual_type);
bool cast_to_index(Expr *index);

bool cast_untyped_to_type(SemaContext *context, Expr *expr, Type *to_type);

CastKind cast_to_bool_kind(Type *type);

const char *llvm_codegen(void *context);
void **llvm_gen(Module** modules, unsigned module_count);

const char *tinybackend_codegen(void *context);
void *tinybackend_gen(Module *module);
void tinybackend_codegen_setup();

void header_gen(Module *module);

void global_context_clear_errors(void);
void global_context_add_type(Type *type);
void global_context_add_decl(Decl *type_decl);
void global_context_add_generic_decl(Decl *decl);

Module *compiler_find_or_create_module(Path *module_name, const char **parameters, bool is_private);
Module *global_context_find_module(const char *name);
const char *get_object_extension(void);

CompilationUnit * unit_create(File *file);
void unit_register_global_decl(CompilationUnit *unit, Decl *decl);
void unit_register_external_symbol(CompilationUnit *unit, Decl *decl);

bool unit_add_import(CompilationUnit *unit, Path *path, bool private_import);
bool context_set_module_from_filename(ParseContext *context);
bool context_set_module(ParseContext *context, Path *path, const char **generic_parameters, bool is_private);

// --- Decl functions

Decl *decl_new(DeclKind decl_kind, const char *name, SourceSpan span, Visibility visibility);
Decl *decl_new_ct(DeclKind kind, SourceSpan span);
Decl *decl_new_with_type(const char *name, SourceSpan span, DeclKind decl_type, Visibility visibility);
Decl *decl_new_var(const char *name, SourceSpan span, TypeInfo *type, VarDeclKind kind, Visibility visibility);
Decl *decl_new_generated_var(Type *type, VarDeclKind kind, SourceSpan span);
void decl_set_external_name(Decl *decl);
const char *decl_to_name(Decl *decl);
const char *decl_to_a_name(Decl *decl);
int decl_count_elements(Decl *structlike);

INLINE bool decl_ok(Decl *decl);
INLINE bool decl_poison(Decl *decl);
INLINE bool decl_is_struct_type(Decl *decl);
INLINE bool decl_is_user_defined_type(Decl *decl);
INLINE Decl *decl_flatten(Decl *decl);
INLINE const char *decl_get_extname(Decl *decl);
static inline Decl *decl_raw(Decl *decl);
static inline DeclKind decl_from_token(TokenType type);
static inline bool decl_is_local(Decl *decl);
Decl *decl_find_enum_constant(Decl *decl, const char *name);
AlignSize decl_find_member_offset(Decl *decl, Decl *member);

// --- Expression functions

#define EXPR_NEW_TOKEN(kind_) expr_new(kind_, c->span)
Expr *expr_new(ExprKind kind, SourceSpan start);
Expr *expr_new_const_int(SourceSpan span, Type *type, uint64_t v, bool narrowable);
Expr *expr_new_const_bool(SourceSpan span, Type *type, bool value);
Expr *expr_new_const_typeid(SourceSpan span, Type *type);
bool expr_is_simple(Expr *expr);
bool expr_is_pure(Expr *expr);
bool expr_is_constant_eval(Expr *expr, ConstantEvalKind eval_kind);
bool expr_is_compile_time(Expr *ast);
Expr *expr_generate_decl(Decl *decl, Expr *assign);
void expr_insert_addr(Expr *original);
void expr_rewrite_insert_deref(Expr *original);
Expr *expr_generate_decl(Decl *decl, Expr *assign);
Expr *expr_variable(Decl *decl);
Expr *expr_negate_expr(Expr *expr);
bool expr_may_addr(Expr *expr);
bool expr_in_int_range(Expr *expr, int64_t low, int64_t high);
bool expr_is_unwrapped_ident(Expr *expr);
bool expr_may_splat_as_vararg(Expr *expr, Type *variadic_base_type);
INLINE Expr *expr_new_expr(ExprKind kind, Expr *expr);
INLINE bool expr_ok(Expr *expr);
INLINE bool exprid_is_simple(ExprId expr_id);
INLINE bool exprid_is_pure(ExprId expr_id);
INLINE Type *exprtype(ExprId expr_id);
INLINE void expr_replace(Expr *expr, Expr *replacement);
INLINE bool expr_poison(Expr *expr);
INLINE bool exprid_is_constant_eval(ExprId expr, ConstantEvalKind eval_kind);
INLINE bool expr_is_init_list(Expr *expr);
INLINE bool expr_is_neg(Expr *expr);
INLINE bool expr_is_mult(Expr *expr);
INLINE bool expr_is_deref(Expr *expr);
INLINE bool expr_is_const(Expr *expr);
INLINE bool expr_is_const_int(Expr *expr);
INLINE bool expr_is_const_string(Expr *expr);
INLINE bool expr_is_const_initializer(Expr *expr);
INLINE bool expr_is_const_untyped_list(Expr *expr);
INLINE bool expr_is_const_member(Expr *expr);

INLINE void expr_rewrite_const_null(Expr *expr, Type *type);
INLINE void expr_rewrite_const_bool(Expr *expr, Type *type, bool b);
INLINE void expr_rewrite_const_float(Expr *expr, Type *type, Real d);
INLINE void expr_rewrite_const_int(Expr *expr, Type *type, uint64_t v, bool narrowable);
INLINE void expr_rewrite_const_typeid(Expr *expr, Type *type);
INLINE void expr_rewrite_const_initializer(Expr *expr, Type *type, ConstInitializer *initializer);
INLINE void expr_rewrite_const_untyped_list(Expr *expr, Expr **elements);

void expr_rewrite_to_builtin_access(Expr *expr, Expr *parent, BuiltinAccessKind kind, Type *type);
void expr_rewrite_to_string(Expr *expr_to_rewrite, const char *string);
void expr_rewrite_to_const_zero(Expr *expr, Type *type);
bool expr_rewrite_to_const_initializer_index(Type *list_type, ConstInitializer *list, Expr *result, unsigned index);

bool expr_const_in_range(const ExprConst *left, const ExprConst *right, const ExprConst *right_to);
bool expr_const_compare(const ExprConst *left, const ExprConst *right, BinaryOp op);
bool expr_const_will_overflow(const ExprConst *expr, TypeKind kind);
const char *expr_const_to_error_string(const ExprConst *expr);
bool expr_const_float_fits_type(const ExprConst *expr_const, TypeKind kind);

// --- Lexer functions

void lexer_init(Lexer *lexer);
bool lexer_next_token(Lexer *lexer);

// --- Module functions

Decl *module_find_symbol(Module *module, const char *symbol);
const char *module_create_object_file_name(Module *module);

bool parse_file(File *file);
bool parse_stdin(void);
Path *path_create_from_string(const char *string, uint32_t len, SourceSpan span);

#define SEMA_ERROR_HERE(...) sema_error_at(c->span, __VA_ARGS__)
#define SEMA_ERROR_LAST(...) sema_error_at(c->prev_span, __VA_ARGS__)
#define SEMA_ERROR(_node, ...) sema_error_at((_node)->span, __VA_ARGS__)
#define SEMA_NOTE(_node, ...) sema_error_prev_at((_node)->span, __VA_ARGS__)
#define EXPAND_EXPR_STRING(str_) (str_)->const_expr.string.len, (str_)->const_expr.string.chars
#define TABLE_MAX_LOAD 0.5

void sema_analysis_run(void);
Decl **sema_decl_stack_store(void);
Decl *sema_decl_stack_find_decl_member(Decl *decl_owner, const char *symbol);
Decl *sema_decl_stack_resolve_symbol(const char *symbol);
void sema_decl_stack_restore(Decl **state);
void sema_decl_stack_push(Decl *decl);
bool sema_error_failed_cast(Expr *expr, Type *from, Type *to);
bool sema_add_local(SemaContext *context, Decl *decl);
void sema_unwrap_var(SemaContext *context, Decl *decl);
void sema_rewrap_var(SemaContext *context, Decl *decl);
void sema_erase_var(SemaContext *context, Decl *decl);
void sema_erase_unwrapped(SemaContext *context, Decl *decl);
bool sema_analyse_cond_expr(SemaContext *context, Expr *expr);

bool sema_analyse_expr_rhs(SemaContext *context, Type *to, Expr *expr, bool allow_failable);
MemberIndex sema_get_initializer_const_array_size(SemaContext *context, Expr *initializer, bool *may_be_array, bool *is_const_size);
bool sema_analyse_expr(SemaContext *context, Expr *expr);
bool sema_analyse_inferred_expr(SemaContext *context, Type *to, Expr *expr);
bool sema_analyse_decl(SemaContext *context, Decl *decl);
bool sema_analyse_var_decl_ct(SemaContext *context, Decl *decl);
bool sema_analyse_var_decl(SemaContext *context, Decl *decl, bool local);
bool sema_analyse_ct_assert_stmt(SemaContext *context, Ast *statement);
bool sema_analyse_statement(SemaContext *context, Ast *statement);
bool sema_expr_analyse_assign_right_side(SemaContext *context, Expr *expr, Type *left_type, Expr *right, bool is_unwrapped_var);
bool sema_expr_analyse_initializer_list(SemaContext *context, Type *to, Expr *expr);
Expr **sema_expand_vasplat_exprs(SemaContext *c, Expr **exprs);
bool sema_expr_analyse_general_call(SemaContext *context, Expr *expr, Decl *decl, Expr *struct_var, bool failable);
Decl *sema_resolve_symbol_in_current_dynamic_scope(SemaContext *context, const char *symbol);
Decl *sema_decl_stack_resolve_symbol(const char *symbol);
Decl *sema_find_decl_in_modules(Module **module_list, Path *path, const char *interned_name);
Decl *unit_resolve_parameterized_symbol(CompilationUnit *unit, NameResolve *name_resolve);
Decl *sema_resolve_type_method(CompilationUnit *unit, Type *type, const char *method_name, Decl **ambiguous_ref, Decl **private_ref);
Decl *sema_resolve_method(CompilationUnit *unit, Decl *type, const char *method_name, Decl **ambiguous_ref, Decl **private_ref);
Decl *sema_find_extension_method_in_module(Module *module, Type *type, const char *method_name);

Decl *sema_find_symbol(SemaContext *context, const char *symbol);
Decl *sema_find_path_symbol(SemaContext *context, const char *symbol, Path *path);
Decl *sema_find_label_symbol(SemaContext *context, const char *symbol);
Decl *sema_find_label_symbol_anywhere(SemaContext *context, const char *symbol);
Decl *sema_resolve_symbol(SemaContext *context, const char *symbol, Path *path, SourceSpan span);
bool sema_symbol_is_defined_in_scope(SemaContext *c, const char *symbol);

bool sema_resolve_array_like_len(SemaContext *context, TypeInfo *type_info, ArraySize *len_ref);
bool sema_resolve_type_info(SemaContext *context, TypeInfo *type_info);
bool sema_resolve_type_info_maybe_inferred(SemaContext *context, TypeInfo *type_info, bool allow_inferred_type);

void sema_error_at(SourceSpan loc, const char *message, ...);
void sema_error_at_after(SourceSpan loc, const char *message, ...);
void sema_error_prev_at(SourceSpan loc, const char *message, ...);
void sema_verror_range(SourceSpan location, const char *message, va_list args);
void sema_error(ParseContext *context, const char *message, ...);

void sema_shadow_error(Decl *decl, Decl *old);
bool sema_type_error_on_binop(Expr *expr);

File *source_file_by_id(FileId file);
File *source_file_load(const char *filename, bool *already_loaded);


#define RANGE_EXTEND_PREV(x)  do { (x)->span = extend_span_with_token((x)->span, c->prev_span); } while (0)

void stable_init(STable *table, uint32_t initial_size);
void *stable_set(STable *table, const char *key, void *value);
void *stable_get(STable *table, const char *key);

void htable_init(HTable *table, uint32_t initial_size);
void *htable_set(HTable *table, const char *key, void *value);
void *htable_get(HTable *table, const char *key);

UNUSED void stable_clear(STable *table);

void decltable_init(DeclTable *table, uint32_t initial_size);
DeclId decltable_get(DeclTable *table, const char *name);
void decltable_set(DeclTable *table, Decl *decl);

const char *scratch_buffer_interned(void);

const char *symtab_add(const char *symbol, uint32_t len, uint32_t fnv1hash, TokenType *type);
const char *symtab_find(const char *symbol, uint32_t len, uint32_t fnv1hash, TokenType *type);
void *llvm_target_machine_create(void);
void target_setup(BuildTarget *build_target);
int target_alloca_addr_space();
bool os_is_apple(OsType os_type);

const char *macos_sysroot(void);
MacSDK *macos_sysroot_sdk_information(const char *sdk_path);
WindowsSDK *windows_get_sdk(void);
const char *windows_cross_compile_library(void);
INLINE bool visible_external(Visibility  visibility);

void c_abi_func_create(FunctionPrototype *proto);

bool token_is_any_type(TokenType type);
const char *token_type_to_string(TokenType type);

#define IS_OPTIONAL(element_) (type_is_optional((element_)->type))
bool type_is_comparable(Type *type);
bool type_is_ordered(Type *type);
unsigned type_get_introspection_kind(TypeKind kind);
void type_mangle_introspect_name_to_buffer(Type *type);
AlignSize type_abi_alignment(Type *type);
AlignSize type_alloca_alignment(Type *type);
Type *type_find_common_ancestor(Type *left, Type *right);
Type *type_find_largest_union_element(Type *type);
Type *type_find_max_type(Type *type, Type *other);
Type *type_find_max_type_may_fail(Type *type, Type *other);
Type *type_abi_find_single_struct_element(Type *type);
Module *type_base_module(Type *type);
bool type_is_valid_for_vector(Type *type);
Type *type_get_array(Type *arr_type, ArraySize len);
Type *type_get_indexed_type(Type *type);
Type *type_get_ptr(Type *ptr_type);
Type *type_get_ptr_recurse(Type *ptr_type);
Type *type_get_subarray(Type *arr_type);
Type *type_get_inferred_array(Type *arr_type);
Type *type_get_inferred_vector(Type *arr_type);
Type *type_get_flexible_array(Type *arr_type);
Type *type_get_scaled_vector(Type *arr_type);
Type *type_get_optional(Type *failable_type);
Type *type_get_vector(Type *vector_type, unsigned len);
Type *type_get_vector_bool(Type *original_type);
Type *type_int_signed_by_bitsize(BitSize bitsize);
Type *type_int_unsigned_by_bitsize(BitSize bit_size);
TypeSize type_size(Type *type);
void type_init_cint(void);
void type_func_prototype_init(uint32_t capacity);
bool type_is_subtype(Type *type, Type *possible_subtype);
bool type_is_abi_aggregate(Type *type);
bool type_is_int128(Type *type);
Type *type_get_func(Signature *signature, CallABI abi);
Type *type_from_token(TokenType type);
bool type_is_user_defined(Type *type);
bool type_is_structurally_equivalent(Type *type1, Type *type);
bool type_flat_is_floatlike(Type *type);
bool type_flat_is_intlike(Type *type);
bool type_flat_is_numlike(Type *type);
bool type_may_have_sub_elements(Type *type);
bool type_may_have_method(Type *type);
const char *type_to_error_string(Type *type);
const char *type_quoted_error_string(Type *type);
INLINE bool type_may_negate(Type *type);
INLINE bool type_is_builtin(TypeKind kind);
INLINE bool type_convert_will_trunc(Type *destination, Type *source);
INLINE Type *type_no_optional(Type *type);
INLINE Type *type_new(TypeKind kind, const char *name);
INLINE bool type_is_pointer_sized(Type *type);
INLINE bool type_is_pointer_sized_or_more(Type *type);
INLINE Type *type_add_optional(Type *type, bool make_optional);
INLINE Type *type_from_inferred(Type *flattened, Type *element_type, unsigned count);
INLINE bool type_len_is_inferred(Type *type);
INLINE bool type_is_substruct(Type *type);
INLINE Type *type_flatten_for_bitstruct(Type *type);
INLINE const char *type_invalid_storage_type_name(Type *type);
INLINE bool type_is_float(Type *type);
INLINE bool type_is_optional(Type *type);
INLINE bool type_is_optional_type(Type *type);
INLINE bool type_is_optional_any(Type *type);
INLINE bool type_is_void(Type *type);
INLINE bool type_is_integer(Type *type);
INLINE bool type_is_integer_unsigned(Type *type);
INLINE bool type_is_integer_signed(Type *type);
INLINE bool type_is_integer_or_bool_kind(Type *type);
INLINE bool type_is_numeric(Type *type);
INLINE bool type_is_len_inferred(Type *type);
INLINE bool type_underlying_is_numeric(Type *type);
INLINE bool type_is_pointer(Type *type);
INLINE bool type_is_arraylike(Type *type);
INLINE bool type_is_any_arraylike(Type *type);
INLINE bool type_is_promotable_float(Type *type);
INLINE bool type_is_promotable_integer(Type *type);
INLINE bool type_is_signed(Type *type);
INLINE bool type_ok(Type *type);
INLINE bool type_is_unsigned(Type *type);
INLINE bool type_is_union_or_strukt(Type *type);
INLINE bool type_flat_is_vector(Type *type);
INLINE AlignSize type_min_alignment(AlignSize a, AlignSize b);
INLINE AlignSize type_max_alignment(AlignSize a, AlignSize b);
INLINE BitSize type_bit_size(Type *type);
INLINE Type *type_vector_type(Type *type);

static inline CanonicalType *type_pointer_type(Type *type);
static inline Type *type_flatten(Type *type);
static inline Type *type_flatten_distinct(Type *type);
static inline Type *type_flatten_distinct_optional(Type *type);
static inline bool type_flat_is_char_array(Type *type);

INLINE TypeInfo *type_info_new(TypeInfoKind kind, SourceSpan span);
INLINE TypeInfo *type_info_new_base(Type *type, SourceSpan span);
INLINE bool type_info_ok(TypeInfo *type_info);
INLINE bool type_info_poison(TypeInfo *type);

int type_kind_bitsize(TypeKind kind);
INLINE bool type_kind_is_signed(TypeKind kind);
INLINE bool type_kind_is_unsigned(TypeKind kind);
INLINE bool type_kind_is_any_integer(TypeKind kind);

void advance(ParseContext *c);
INLINE void advance_and_verify(ParseContext *context, TokenType token_type);

UnaryOp unaryop_from_token(TokenType type);
BinaryOp binaryop_from_token(TokenType type);
BinaryOp binaryop_assign_base_op(BinaryOp assign_binary_op);
TokenType binaryop_to_token(BinaryOp type);


// ---- static inline function implementations.

INLINE Type *type_no_optional(Type *type)
{
	if (!type) return NULL;
	if (type->type_kind == TYPE_OPTIONAL) return type->failable;
	if (type->type_kind == TYPE_FAILABLE_ANY) return type_void;
	return type;
}

INLINE bool type_is_pointer_sized_or_more(Type *type)
{
	return type_is_integer(type) && type_size(type) >= type_size(type_iptr);
}

INLINE bool type_is_pointer_sized(Type *type)
{
	return type_is_integer(type) && type_size(type) == type_size(type_iptr);
}

#define DECL_TYPE_KIND_REAL(k_, t_) \
 TypeKind k_ = (t_)->type_kind; \
 if (k_ == TYPE_TYPEDEF) k_ = (t_)->canonical->type_kind;


INLINE Type *type_add_optional(Type *type, bool make_optional)
{
	if (!make_optional || type->type_kind == TYPE_OPTIONAL || type->type_kind == TYPE_FAILABLE_ANY) return type;
	return type_get_optional(type);
}

INLINE Type *type_from_inferred(Type *flattened, Type *element_type, unsigned count)
{
	switch (flattened->type_kind)
	{
		case TYPE_INFERRED_VECTOR:
			return type_get_vector(element_type, count);
			break;
		case TYPE_INFERRED_ARRAY:
			return type_get_array(element_type, count);
			break;
		default:
			UNREACHABLE
	}
}
INLINE bool type_len_is_inferred(Type *type)
{
	if (!type) return true;
	DECL_TYPE_KIND_REAL(kind, type);
	while (1)
	{
		switch (type->type_kind)
		{
			case TYPE_TYPEDEF:
				type = type->canonical;
				continue;
			case TYPE_ARRAY:
			case TYPE_SUBARRAY:
			case TYPE_FLEXIBLE_ARRAY:
			case TYPE_VECTOR:
			case TYPE_SCALED_VECTOR:
				type = type->array.base;
				continue;
			case TYPE_INFERRED_ARRAY:
			case TYPE_INFERRED_VECTOR:
				return true;
			case TYPE_POINTER:
				type = type->pointer;
				continue;
			default:
				return false;
		}
		UNREACHABLE;
	}
}

INLINE bool type_is_optional(Type *type)
{
	DECL_TYPE_KIND_REAL(kind, type);
	return kind == TYPE_OPTIONAL || kind == TYPE_FAILABLE_ANY;
}

INLINE bool type_is_optional_type(Type *type)
{
	DECL_TYPE_KIND_REAL(kind, type);
	return kind == TYPE_OPTIONAL;
}

INLINE bool type_is_optional_any(Type *type)
{
	return type->canonical == type_anyfail;
}
INLINE bool type_is_void(Type *type)
{
	return type->canonical == type_void;
}

INLINE bool type_is_integer(Type *type)
{
	DECL_TYPE_KIND_REAL(kind, type);
	return kind >= TYPE_INTEGER_FIRST && kind <= TYPE_INTEGER_LAST;
}

INLINE bool type_is_integer_signed(Type *type)
{
	DECL_TYPE_KIND_REAL(kind, type);
	return kind >= TYPE_INT_FIRST && kind <= TYPE_INT_LAST;
}

INLINE bool type_is_integer_or_bool_kind(Type *type)
{
	DECL_TYPE_KIND_REAL(kind, type);
	return kind >= TYPE_BOOL && kind <= TYPE_U128;
}

INLINE bool type_is_integer_unsigned(Type *type)
{
	DECL_TYPE_KIND_REAL(kind, type);
	return kind >= TYPE_UINT_FIRST && kind <= TYPE_UINT_LAST;
}

INLINE bool type_info_poison(TypeInfo *type)
{
	type->type = poisoned_type;
	type->resolve_status = RESOLVE_DONE;
	return false;
}

INLINE bool type_is_arraylike(Type *type)
{
	DECL_TYPE_KIND_REAL(kind, type);
	return kind == TYPE_ARRAY || kind == TYPE_VECTOR || kind == TYPE_FLEXIBLE_ARRAY || kind == TYPE_SCALED_VECTOR;
}

INLINE bool type_is_any_arraylike(Type *type)
{
	DECL_TYPE_KIND_REAL(kind, type);
	return kind == TYPE_ARRAY || kind == TYPE_VECTOR
		|| kind == TYPE_FLEXIBLE_ARRAY || kind == TYPE_SCALED_VECTOR
		|| kind == TYPE_INFERRED_VECTOR || kind == TYPE_INFERRED_ARRAY;
}

INLINE CanonicalType *type_pointer_type(Type *type)
{
	CanonicalType *res = type->canonical;
	if (res->type_kind != TYPE_POINTER) return NULL;
	return res->pointer ;
}

INLINE bool type_is_pointer(Type *type)
{
	DECL_TYPE_KIND_REAL(kind, type);
	return kind == TYPE_POINTER;
}

static inline AlignSize aligned_offset(AlignSize offset, AlignSize alignment)
{
	return ((offset + alignment - 1) / alignment) * alignment;
}

INLINE bool type_is_substruct(Type *type)
{
	DECL_TYPE_KIND_REAL(kind, type);
	return kind == TYPE_STRUCT && type->decl->is_substruct;
}

INLINE bool type_may_negate(Type *type)
{
	RETRY:
	switch (type->type_kind)
	{
		case TYPE_VECTOR:
			type = type->array.base;
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
		case TYPE_OPTIONAL:
			type = type->failable;
			goto RETRY;
		default:
			return false;
	}
}


INLINE bool type_is_float(Type *type)
{
	DECL_TYPE_KIND_REAL(kind, type);
	return kind >= TYPE_FLOAT_FIRST && kind <= TYPE_FLOAT_LAST;
}

INLINE const char *type_invalid_storage_type_name(Type *type)
{
	switch (type->type_kind)
	{
		case TYPE_MEMBER:
			return "a member reference";
		case TYPE_UNTYPED_LIST:
			return "an untyped list";
		case TYPE_TYPEINFO:
			return "a type";
		default:
			UNREACHABLE;
	}
}

INLINE bool type_is_invalid_storage_type(Type *type)
{
	if (!type) return false;
	switch (type->type_kind)
	{
		case TYPE_MEMBER:
		case TYPE_UNTYPED_LIST:
		case TYPE_TYPEINFO:
			return true;
		default:
			return false;
	}
}

INLINE TypeInfo *type_info_new(TypeInfoKind kind, SourceSpan span)
{
	TypeInfo *type_info = type_info_calloc();
	type_info->kind = kind;
	type_info->span = span;
	type_info->resolve_status = RESOLVE_NOT_DONE;
	return type_info;
}

INLINE TypeInfo *type_info_new_base(Type *type, SourceSpan span)
{
	TypeInfo *type_info = type_info_calloc();
	type_info->kind = TYPE_INFO_IDENTIFIER;
	type_info->resolve_status = RESOLVE_DONE;
	type_info->type = type;
	type_info->span = span;
	return type_info;
}

INLINE Type *type_new(TypeKind kind, const char *name)
{
	Type *type = CALLOCS(Type);
	type->type_kind = kind;
	assert(name);
	type->name = name;
	global_context_add_type(type);
	return type;
}


INLINE bool type_convert_will_trunc(Type *destination, Type *source)
{
	assert(type_is_builtin(destination->canonical->type_kind));
	assert(type_is_builtin(source->canonical->type_kind));
	return (unsigned)destination->canonical->builtin.bitsize < (unsigned)source->canonical->builtin.bitsize;
}



// Useful sanity check function.
INLINE void advance_and_verify(ParseContext *context, TokenType token_type)
{
	assert(context->tok == token_type);
	advance(context);
}


INLINE Type *type_flatten_for_bitstruct(Type *type)
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

static inline Type *type_flatten_distinct_optional(Type *type)
{
	while (1)
	{
		switch (type->type_kind)
		{
			case TYPE_TYPEDEF:
				type = type->canonical;
				continue;
			case TYPE_OPTIONAL:
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
	while (1)
	{
		type = type->canonical;
		switch (type->type_kind)
		{
			case TYPE_DISTINCT:
				type = type->decl->distinct_decl.base_type;
				break;
			case TYPE_ENUM:
				type = type->decl->enums.type_info->type;
				break;
			case TYPE_OPTIONAL:
				type = type->failable;
				break;
			case TYPE_FAILABLE_ANY:
				return type_void;
			case TYPE_TYPEDEF:
				UNREACHABLE
			default:
				return type;
		}
	}
}

static inline bool type_flat_is_char_array(Type *type)
{
	type = type_flatten_distinct(type);
	if (type->type_kind != TYPE_ARRAY) return false;
	switch (type->array.base->type_kind)
	{
		case TYPE_I8:
		case TYPE_U8:
			return true;
		default:
			return false;
	}
}

INLINE Type *type_vector_type(Type *type)
{
	Type *flatten = type_flatten(type);
	return flatten->type_kind == TYPE_VECTOR ? flatten->array.base : NULL;
}

INLINE bool type_is_builtin(TypeKind kind) { return kind >= TYPE_VOID && kind <= TYPE_TYPEID; }
INLINE bool type_kind_is_signed(TypeKind kind) { return kind >= TYPE_I8 && kind < TYPE_U8; }
INLINE bool type_kind_is_unsigned(TypeKind kind) { return kind >= TYPE_U8 && kind <= TYPE_U128; }
INLINE bool type_kind_is_any_integer(TypeKind kind) { return kind >= TYPE_I8 && kind <= TYPE_U128; }
INLINE bool type_is_unsigned(Type *type) { return type->type_kind >= TYPE_U8 && type->type_kind <= TYPE_U128; }
INLINE bool type_ok(Type *type) { return !type || type->type_kind != TYPE_POISONED; }
INLINE bool type_info_ok(TypeInfo *type_info) { return !type_info || type_info->kind != TYPE_INFO_POISON; }
bool type_is_scalar(Type *type);

INLINE bool type_is_signed(Type *type) { return type->type_kind >= TYPE_I8 && type->type_kind < TYPE_U8; }

INLINE bool type_is_len_inferred(Type *type)
{
	TypeKind kind = type->type_kind;
	return kind == TYPE_INFERRED_VECTOR || kind == TYPE_INFERRED_ARRAY;
}

INLINE bool type_is_numeric(Type *type)
{
	DECL_TYPE_KIND_REAL(kind, type);
	return (kind >= TYPE_I8 && kind <= TYPE_FLOAT_LAST) || kind == TYPE_VECTOR;
}

INLINE bool type_underlying_is_numeric(Type *type)
{
	return type_is_numeric(type_flatten(type));
}

INLINE bool type_flat_is_vector(Type *type)
{
	return type_flatten(type)->type_kind == TYPE_VECTOR;
}

INLINE bool type_is_union_or_strukt(Type *type)
{
	DECL_TYPE_KIND_REAL(kind, type);
	return kind == TYPE_UNION || kind == TYPE_STRUCT;
}


INLINE bool decl_ok(Decl *decl)
{
	return !decl || decl->decl_kind != DECL_POISONED;
}

INLINE bool decl_poison(Decl *decl)
{
	decl->decl_kind = DECL_POISONED; decl->resolve_status = RESOLVE_DONE; return false;
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

INLINE bool decl_is_struct_type(Decl *decl)
{
	DeclKind kind = decl->decl_kind;
	return (kind == DECL_UNION) | (kind == DECL_STRUCT);
}

static inline bool decl_is_enum_kind(Decl *decl)
{
	DeclKind kind = decl->decl_kind;
	return (kind == DECL_ENUM) | (kind == DECL_FAULT);
}

INLINE bool decl_is_user_defined_type(Decl *decl)
{
	DeclKind kind = decl->decl_kind;
	return (kind == DECL_UNION) | (kind == DECL_STRUCT) | (kind == DECL_BITSTRUCT)
			| (kind == DECL_ENUM) | (kind == DECL_DISTINCT);
}

INLINE Decl *decl_flatten(Decl *decl)
{
	if (decl->decl_kind == DECL_DEFINE && decl->define_decl.define_kind != DEFINE_TYPE_GENERIC)
	{
		return decl->define_decl.alias;
	}
	return decl;
}

static inline DeclKind decl_from_token(TokenType type)
{
	if (type == TOKEN_STRUCT) return DECL_STRUCT;
	if (type == TOKEN_UNION) return DECL_UNION;
	if (type == TOKEN_BITSTRUCT) return DECL_BITSTRUCT;
	UNREACHABLE
}

INLINE bool expr_is_deref(Expr *expr)
{
	return expr->expr_kind == EXPR_UNARY && expr->unary_expr.operator == UNARYOP_DEREF;
}

INLINE bool expr_is_mult(Expr *expr)
{
	return expr->expr_kind == EXPR_BINARY && expr->binary_expr.operator == BINARYOP_MULT;
}

INLINE bool expr_is_neg(Expr *expr)
{
	return expr->expr_kind == EXPR_UNARY && expr->unary_expr.operator == UNARYOP_NEG;
}

INLINE bool expr_is_init_list(Expr *expr)
{
	ExprKind kind = expr->expr_kind;
	return kind == EXPR_DESIGNATED_INITIALIZER_LIST || kind == EXPR_INITIALIZER_LIST;
}

INLINE bool exprid_is_constant_eval(ExprId expr, ConstantEvalKind eval_kind)
{
	return expr ? expr_is_constant_eval(exprptr(expr), eval_kind) : true;
}

INLINE bool expr_poison(Expr *expr) { expr->expr_kind = EXPR_POISONED; expr->resolve_status = RESOLVE_DONE; return false; }

INLINE void expr_replace(Expr *expr, Expr *replacement)
{
	SourceSpan loc = expr->span;
	*expr = *replacement;
	expr->span = loc;
}

INLINE bool expr_ok(Expr *expr) { return expr == NULL || expr->expr_kind != EXPR_POISONED; }

INLINE bool exprid_is_simple(ExprId expr_id) { return expr_is_simple(exprptr(expr_id)); }

INLINE Type *exprtype(ExprId expr_id)
{
	assert(expr_id);
	return exprptr(expr_id)->type;
}

INLINE bool exprid_is_pure(ExprId expr_id)
{
	return expr_id ? expr_is_pure(exprptr(expr_id)) : false;
}

INLINE Expr *expr_new_expr(ExprKind kind, Expr *expr)
{
	return expr_new(kind, expr->span);
}

INLINE bool type_is_promotable_integer(Type *type)
{
	// If we support other architectures, update this.
	return type_is_integer_or_bool_kind(type) && type->builtin.bitsize < platform_target.width_c_int;
}

INLINE bool type_is_promotable_float(Type *type)
{
	// If we support other architectures, update this.
	return type_is_float(type->canonical) && type->builtin.bytesize < type_double->builtin.bytesize;
}


/**
 * Minimum alignment, values are either offsets or alignments.
 * @return
 */
INLINE AlignSize type_min_alignment(AlignSize a, AlignSize b)
{
	return (a | b) & (1 + ~(a | b));
}

/**
 * Max highest required alignment
 */
INLINE AlignSize type_max_alignment(AlignSize a, AlignSize b)
{
	return a < b ? b : a;
}

INLINE BitSize type_bit_size(Type *type)
{
	return type_size(type) * 8;
}

bool obj_format_linking_supported(ObjectFormatType format_type);
bool static_lib_linker(const char *output_file, const char **files, unsigned file_count);
bool dynamic_lib_linker(const char *output_file, const char **files, unsigned file_count);
bool linker(const char *output_file, const char **files, unsigned file_count);
void platform_linker(const char *output_file, const char **files, unsigned file_count);
void platform_compiler(const char **files, unsigned file_count, const char* flags);
const char *arch_to_linker_arch(ArchType arch);

#define CAT(a,b) CAT2(a,b) // force expand
#define CAT2(a,b) a##b // actually concatenate
#define TEMP(X) CAT(X, __LINE__)
#define ASSIGN_AST_OR_RET(_assign, _ast_stmt, _res) Ast* TEMP(_ast) = (_ast_stmt); if (!ast_ok(TEMP(_ast))) return _res; _assign = TEMP(_ast)
#define ASSIGN_ASTID_OR_RET(_assign, _ast_stmt, _res) Ast* TEMP(_ast) = (_ast_stmt); if (!ast_ok(TEMP(_ast))) return _res; _assign = astid(TEMP(_ast))
#define ASSIGN_EXPR_OR_RET(_assign, _expr_stmt, _res) Expr* TEMP(_expr) = (_expr_stmt); if (!expr_ok(TEMP(_expr))) return _res; _assign = TEMP(_expr)
#define ASSIGN_EXPRID_OR_RET(_assign, _expr_stmt, _res) Expr* TEMP(_expr) = (_expr_stmt); if (!expr_ok(TEMP(_expr))) return _res; _assign = exprid(TEMP(_expr))
#define ASSIGN_TYPE_OR_RET(_assign, _type_stmt, _res) TypeInfo* TEMP(_type) = (_type_stmt); if (!type_info_ok(TEMP(_type))) return _res; _assign = TEMP(_type)
#define ASSIGN_TYPEID_OR_RET(_assign, _type_stmt, _res) TypeInfo* TEMP(_type) = (_type_stmt); if (!type_info_ok(TEMP(_type))) return _res; _assign = type_infoid(TEMP(_type))
#define ASSIGN_DECL_OR_RET(_assign, _decl_stmt, _res) Decl* TEMP(_decl) = (_decl_stmt); if (!decl_ok(TEMP(_decl))) return _res; _assign = TEMP(_decl)



INLINE void ast_append(AstId **succ, Ast *next)
{
	**succ = astid(next);
	*succ = &next->next;
}

INLINE bool ast_ok(Ast *ast) { return ast == NULL || ast->ast_kind != AST_POISONED; }

INLINE bool ast_poison(Ast *ast) { ast->ast_kind = AST_POISONED; return false; }

INLINE Ast *new_ast(AstKind kind, SourceSpan range)
{
	Ast *ast = ast_calloc();
	ast->span = range;
	ast->ast_kind = kind;
	return ast;
}

INLINE Ast *ast_last(Ast *ast)
{
	while (ast->next) ast = astptr(ast->next);
	return ast;
}

INLINE void ast_prepend(AstId *first, Ast *ast)
{
	Ast *end = ast;
	while (end->next)
	{
		end = astptr(end->next);
	}
	end->next = *first;
	*first = astid(ast);
}

INLINE bool visible_external(Visibility  visibility)
{
	return visibility == VISIBLE_PUBLIC || visibility == VISIBLE_EXTERN;
}

INLINE Ast *ast_next(AstId *current_ptr)
{
	Ast *ast = astptr(*current_ptr);
	*current_ptr = ast->next;
	return ast;
}


INLINE const char *decl_get_extname(Decl *decl)
{
	return decl->extname;
}


INLINE void expr_rewrite_const_bool(Expr *expr, Type *type, bool b)
{
	expr->expr_kind = EXPR_CONST;
	expr->type = type;
	expr->const_expr = (ExprConst) { .b = b, .const_kind = CONST_BOOL };
	expr->resolve_status = RESOLVE_DONE;
}

INLINE void expr_rewrite_const_null(Expr *expr, Type *type)
{
	expr->expr_kind = EXPR_CONST;
	expr->type = type;
	expr->const_expr = (ExprConst) { .ptr = 0, .const_kind = CONST_POINTER };
	expr->resolve_status = RESOLVE_DONE;
}

INLINE void expr_rewrite_const_untyped_list(Expr *expr, Expr **elements)
{
	expr->expr_kind = EXPR_CONST;
	expr->type = type_untypedlist;
	expr->const_expr = (ExprConst) { .untyped_list = elements, .const_kind = CONST_UNTYPED_LIST };
	expr->resolve_status = RESOLVE_DONE;
}

INLINE void expr_rewrite_const_initializer(Expr *expr, Type *type, ConstInitializer *initializer)
{
	expr->expr_kind = EXPR_CONST;
	expr->type = type;
	expr->const_expr = (ExprConst) { .initializer = initializer, .const_kind = CONST_INITIALIZER };
	expr->resolve_status = RESOLVE_DONE;
}

INLINE void expr_rewrite_const_typeid(Expr *expr, Type *type)
{
	expr->expr_kind = EXPR_CONST;
	expr->const_expr.const_kind = CONST_TYPEID;
	expr->const_expr.typeid = type->canonical;
	expr->type = type_typeid;
	expr->resolve_status = RESOLVE_DONE;
}


INLINE void expr_rewrite_const_int(Expr *expr, Type *type, uint64_t v, bool narrowable)
{
	expr->expr_kind = EXPR_CONST;
	expr->type = type;
	TypeKind kind = type_flatten(type)->type_kind;
	(&expr->const_expr)->ixx.i.high = 0;
	if (type_kind_is_signed(kind))
	{
		if (v > (uint64_t)INT64_MAX) (&expr->const_expr)->ixx.i.high = UINT64_MAX;
	}
	(&expr->const_expr)->ixx.i.low = v;
	(&expr->const_expr)->ixx.type = kind;
	(&expr->const_expr)->const_kind = CONST_INTEGER;
	expr->const_expr.narrowable = narrowable;
}

INLINE void expr_rewrite_const_float(Expr *expr, Type *type, Real d)
{
	expr->expr_kind = EXPR_CONST;
	expr->type = type;
	TypeKind kind = type_flatten(type)->type_kind;
	switch (kind)
	{
		case TYPE_F32:
			expr->const_expr.fxx = (Float){ (float)d, TYPE_F32 };
			break;
		case TYPE_F64:
			expr->const_expr.fxx = (Float){ (double)d, TYPE_F64 };
			break;
		default:
			expr->const_expr.fxx = (Float){ d, kind };
			break;
	}
	expr->const_expr.const_kind = CONST_FLOAT;
	expr->resolve_status = RESOLVE_DONE;
}

INLINE const char *asm_clobber_by_index(unsigned index)
{
	return asm_target.clobber_name_list[index];
}

INLINE AsmRegister *asm_reg_by_index(unsigned index)
{
	return &asm_target.registers[index];
}

INLINE void clobbers_add(Clobbers *clobbers, unsigned index)
{
	assert(index < MAX_CLOBBER_FLAGS);
	unsigned bit = index % 64;
	unsigned element = index / 64;
	clobbers->mask[element] |= (1ull << bit);
}

static inline Clobbers clobbers_make_from(Clobbers clobbers, ...)
{
	va_list list;
	va_start(list, clobbers);
	int i;
	while ((i = va_arg(list, int)) > -1)
	{
		assert(i < MAX_CLOBBER_FLAGS);
		unsigned bit = i % 64;
		unsigned element = i / 64;
		clobbers.mask[element] |= (1ull << bit);
	}
	va_end(list);
	return clobbers;
}

static inline Clobbers clobbers_make(unsigned index, ...)
{
	Clobbers clobbers = { .mask[0] = 0 };
	assert(index < MAX_CLOBBER_FLAGS);
	unsigned bit = index % 64;
	unsigned element = index / 64;
	clobbers.mask[element] |= (1ull << bit);
	va_list list;
	va_start(list, index);
	int i;
	while ((i = va_arg(list, int)) > -1)
	{
		assert(i < MAX_CLOBBER_FLAGS);
		bit = i % 64;
		element = i / 64;
		clobbers.mask[element] |= (1ull << bit);
	}
	va_end(list);
	return clobbers;
}


INLINE unsigned arg_bits_max(AsmArgBits bits, unsigned limit)
{
	if (limit == 0) limit = ~(0u);
	if (limit >= 128 && (bits & ARG_BITS_128)) return 128;
	if (limit >= 80 && (bits & ARG_BITS_80)) return 80;
	if (limit >= 64 && (bits & ARG_BITS_64)) return 64;
	if (limit >= 32 && (bits & ARG_BITS_32)) return 32;
	if (limit >= 16 && (bits & ARG_BITS_16)) return 16;
	if (limit >= 8 && (bits & ARG_BITS_8)) return 8;
	return 0;
}

INLINE bool expr_is_const(Expr *expr)
{
	return expr->expr_kind == EXPR_CONST;
}

static inline bool decl_is_local(Decl *decl)
{
	if (decl->decl_kind != DECL_VAR) return false;
	VarDeclKind kind = decl->var.kind;
	return kind == VARDECL_PARAM_CT_TYPE
	       || kind == VARDECL_PARAM
	       || kind == VARDECL_PARAM_CT
	       || kind == VARDECL_LOCAL
	       || kind == VARDECL_LOCAL_CT_TYPE
	       || kind == VARDECL_LOCAL_CT
	       || kind == VARDECL_PARAM_REF
	       || kind == VARDECL_PARAM_EXPR
	       || kind == VARDECL_BITMEMBER
	       || kind == VARDECL_MEMBER;
}

INLINE bool expr_is_const_string(Expr *expr)
{
	return expr->expr_kind == EXPR_CONST && expr->const_expr.const_kind == CONST_STRING;
}

INLINE bool expr_is_const_initializer(Expr *expr)
{
	return expr->expr_kind == EXPR_CONST && expr->const_expr.const_kind == CONST_INITIALIZER;
}

INLINE bool expr_is_const_untyped_list(Expr *expr)
{
	return expr->expr_kind == EXPR_CONST && expr->const_expr.const_kind == CONST_UNTYPED_LIST;
}

INLINE bool expr_is_const_int(Expr *expr)
{
	return expr->expr_kind == EXPR_CONST && expr->const_expr.const_kind == CONST_INTEGER;
}

INLINE bool expr_is_const_member(Expr *expr)
{
	return expr->expr_kind == EXPR_CONST && expr->const_expr.const_kind == CONST_MEMBER;
}
