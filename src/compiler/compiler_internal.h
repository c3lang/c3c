// Copyright (c) 2019-2023 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "../utils/common.h"
#include "../utils/lib.h"
#include "../build/build.h"
#include "compiler.h"
#include "enums.h"
#include "target.h"
#include "utils/malloc.h"
#include "subprocess.h"
#include <float.h>

typedef double Real;
typedef uint64_t ByteSize;
typedef uint32_t TypeSize;
typedef int32_t IndexDiff;
typedef int64_t ArrayIndex;
typedef uint16_t StructIndex;
typedef uint32_t AlignSize;
typedef uint64_t ArraySize;
typedef uint64_t BitSize;
typedef uint16_t FileId;

#define INT5_MAX         15
#define INT12_MAX        2047
#define INT20_MAX        524287
#define INT5_MIN         -16
#define INT12_MIN        -2048
#define INT20_MIN        (-INT20_MAX-1)
#define UINT5_MAX         31
#define UINT12_MAX        4095
#define UINT20_MAX        1048575U

#define MAX_ARRAYINDEX INT64_MAX
#define MAX_FIXUPS 0xFFFFF
#define MAX_HASH_SIZE (512 * 1024 * 1024)
#define INVALID_SPAN ((SourceSpan){ .row = 0 })
#define MAX_SCOPE_DEPTH 0x100
#define INITIAL_SYMBOL_MAP 0x10000
#define INITIAL_GENERIC_SYMBOL_MAP 0x1000
#define MAX_INCLUDE_DIRECTIVES 2048
#define MAX_PARAMS 255
#define MAX_VAARGS 512
#define MAX_BITSTRUCT 0x1000
#define MAX_MEMBERS ((StructIndex)1) << 15
#define MAX_ALIGNMENT ((ArrayIndex)(((uint64_t)2) << 28))
#define MAX_GENERIC_DEPTH 32
#define MAX_PRIORITY 0xFFFF
#define MAX_TYPE_SIZE UINT32_MAX
#define MAX_GLOBAL_DECL_STACK (65536)
#define MAX_MODULE_NAME 31
#define MAX_MODULE_PATH 63
#define MAX_MACRO_RECURSION_DEPTH 128
#define MEMCMP_INLINE_REGS 8
#define UINT128_MAX ((Int128) { UINT64_MAX, UINT64_MAX })
#define INT128_MAX ((Int128) { INT64_MAX, UINT64_MAX })
#define INT128_MIN ((Int128) { (uint64_t)INT64_MIN, 0 })
#define STDIN_FILE_ID 0xFFFF
#define ABI_TYPE_EMPTY ((AbiType) { .type = NULL })
#define RANGE_EXTEND_PREV(x)  do { (x)->span = extend_span_with_token((x)->span, c->prev_span); } while (0)
#define PRINT_ERROR_AT(_node, ...) print_error_at((_node)->span, __VA_ARGS__)
#define RETURN_PRINT_ERROR_AT(_val, _node, ...) do { print_error_at((_node)->span, __VA_ARGS__); return _val; } while (0)
#define PRINT_ERROR_HERE(...) print_error_at(c->span, __VA_ARGS__)
#define RETURN_PRINT_ERROR_HERE(...) do { print_error_at(c->span, __VA_ARGS__); return false; } while (0)
#define PRINT_ERROR_LAST(...) print_error_at(c->prev_span, __VA_ARGS__)
#define RETURN_PRINT_ERROR_LAST(...) do { print_error_at(c->prev_span, __VA_ARGS__); return false; } while (0)
#define SEMA_NOTE(_node, ...) sema_note_prev_at((_node)->span, __VA_ARGS__)
#define SEMA_DEPRECATED(_node, ...) do { if (compiler.build.test_output && !compiler.build.silence_deprecation) print_error_at((_node)->span, __VA_ARGS__); if (!compiler.build.silence_deprecation) \
 sema_note_prev_at((_node)->span, __VA_ARGS__); } while (0)
#define PRINT_DEPRECATED_AT(span__, ...) do { if (compiler.build.test_output && !compiler.build.silence_deprecation) print_error_at(span__, __VA_ARGS__); if (!compiler.build.silence_deprecation) \
sema_note_prev_at(span__, __VA_ARGS__); } while (0)

#define EXPAND_EXPR_STRING(str_) (str_)->const_expr.bytes.len, (str_)->const_expr.bytes.ptr
#define TABLE_MAX_LOAD 0.5
#define OUTF(...) do { if (!compiler.build.silent) printf(__VA_ARGS__); } while(0)
#define OUTN(str__) do { if (!compiler.build.silent) puts(str__); } while(0)
#ifdef NDEBUG
#define ASSERT_SPANF(node__, check__, format__, ...) do { (void)(check__); } while(0)
#define ASSERT_SPAN(node__, check__) do { (void)(check__); } while(0)
#define ASSERT_AT(span__, check__) do { (void)(check__);} while(0)
#else
#define ASSERT_SPANF(node__, check__, format__, ...) do { if (!(check__)) { assert_print_line((node__)->span); eprintf(format__, __VA_ARGS__); ASSERT(check__); } } while(0)
#define ASSERT_SPAN(node__, check__) do { if (!(check__)) { assert_print_line((node__)->span); ASSERT(check__); } } while(0)
#define ASSERT_AT(span__, check__) do { if (!(check__)) { assert_print_line(span__); ASSERT(check__); } } while(0)
#endif

#define INVALID_PTR ((void*)(uintptr_t)0xAAAAAAAAAAAAAAAA)

typedef struct Ast_ Ast;
typedef struct Decl_ Decl;
typedef struct TypeInfo_ TypeInfo;
typedef struct Expr_ Expr;
typedef struct Module_ Module;
typedef struct Type_ Type;
typedef Type CanonicalType;
typedef Type FlatType;
typedef Type LoweredType;
typedef struct Signature_ Signature;
typedef struct ConstInitializer_ ConstInitializer;
typedef struct CompilationUnit_ CompilationUnit;
typedef unsigned AstId;
typedef unsigned ExprId;
typedef unsigned DeclId;
typedef unsigned TypeInfoId;
typedef struct SemaContext_ SemaContext;

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

typedef struct
{
	uint32_t count;
	uint32_t capacity;
	uint32_t max_load;
	DeclId *methods;
} MethodTable;

typedef struct
{
	uint32_t count;
	uint32_t capacity;
	uint32_t max_load;
	DeclId *entries;
} DeclTable;

struct ConstInitializer_
{
	ConstInitType kind;
	// Type, should always be flattened
	Type *type;
	union
	{
		ConstInitializer **init_struct;
		Expr *init_value;
		struct
		{
			ConstInitializer *element;
			ArrayIndex index;
		} init_union;
		struct
		{
			ConstInitializer **elements;
		} init_array;
		ConstInitializer **init_array_full;
		struct
		{
			ConstInitializer *element;
			ArrayIndex index;
		} init_array_value;
	};
};

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

static_assert(sizeof(SourceSpan) == 8, "Expected 8 bytes");

typedef struct InliningSpan_
{
	SourceSpan span;
	struct InliningSpan_ *prev;
} InliningSpan;

typedef struct
{
	ConstKind const_kind : 8;
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
			const char *ptr;
			ArraySize len;
		} bytes;
		Expr *expr_ref;
		Decl *global_ref;
		Decl *enum_val;
		Decl *fault;
		Type *typeid;
		ConstInitializer *initializer;
		ConstInitializer *slice_init;
		Expr **untyped_list;
		struct
		{
			AlignSize offset;
			AlignSize align;
			Decl *decl;
		} member;
	};
} ExprConst;


typedef struct
{
	FileId file_id;
	const char *contents;
	size_t content_len;
	char *name;
	char *dir_path;
	const char *full_path;
} File;



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

typedef struct HEntry_
{
	void *key;
	void *value;
	struct HEntry_ *next;
} HTEntry;

typedef struct PathTableEntry_
{
	const char *short_path;
	const char *name;
	Decl *value;
	struct PathTableEntry_ *next;
} PathTableEntry;

typedef struct
{
	uint32_t mask;
	HTEntry **entries;
} HTable;

typedef struct
{
	uint32_t mask;
	PathTableEntry **entries;
} PathTable;

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
	bool always_const : 1;
	bool is_simd : 1;
	uint8_t format : 8;
} CalleeAttributes;

typedef struct
{
	Decl *decl;
	Signature *signature;
	struct FunctionPrototype_ *prototype;
} TypeFunction;

struct Type_
{
	TypeKind type_kind;
	CanonicalType *canonical;
	const char *name;
	Type **type_cache;
	Type *func_ptr;
	union
	{
		void *backend_type;
		struct
		{
			uint16_t tb_set;
			uint16_t tb_type;
		};
	};
	ByteSize size;
	void *backend_typeid;
	void *backend_debug_type;
	union
	{
		// Error, Struct, Union, Typedef, Member, Bitstruct
		Decl *decl;
		// int, float, bool
		TypeBuiltin builtin;
		// Type[], Type[*], Type[123] or Type[<123>]
		TypeArray array;
		// fn TypeR Type1(Type2, Type3, ...)
		TypeFunction function;
		// Type*
		Type *pointer;
		// Optional
		Type *optional;
	};
};


struct TypeInfo_
{
	ResolveStatus resolve_status : 3;
	TypeInfoKind kind : 6;
	bool optional : 1;
	bool in_def : 1;
	bool is_simd : 1;
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
		struct
		{
			TypeInfo *base;
			Expr **params;
		} generic;
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
	Attr **tags;
	const char *deprecated;
	const char **links;
	const char *section;
	const char *wasm_module;
	SourceSpan overload;
} ResolvedAttrData;

typedef struct
{
	Path *path;
	bool import_private_as_public;
	bool is_non_recurse;
	Module *module;
} ImportDecl;

typedef struct
{
	Expr *filename;
} IncludeDecl;

typedef struct
{
	Expr *filename;
	Expr **args;
	Expr *stdin_string;
} ExecDecl;

typedef struct
{
	Decl **members;
	DeclId parent;
	union
	{
		struct
		{
			TypeSize size;
			DeclId padded_decl_id;
			StructIndex union_rep;
			AlignSize padding : 16;
		};
		struct
		{
			TypeInfo *container_type;
			bool msb0 : 1;
			bool big_endian : 1;
			bool little_endian : 1;
			bool overlap : 1;
			bool consecutive : 1;
		};
	};
} StructDecl;



typedef struct VarDecl_
{
	TypeInfoId type_info;
	uint16_t va_index;
	VarDeclKind kind : 8;
	bool shadow : 1;
	bool vararg : 1;
	bool is_static : 1;
	bool is_read : 1;
	bool not_null : 1;
	bool out_param : 1;
	bool in_param : 1;
	bool is_written : 1;
	bool is_addr : 1;
	bool self_addr : 1;
	bool is_threadlocal : 1;
	bool no_init : 1;
	bool no_alias : 1;
	bool bit_is_expr : 1;
	bool is_self : 1;
	bool is_temp : 1;
	bool copy_const : 1;
	bool defaulted : 1;
	bool safe_infer : 1;
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
			void *backend_debug_ref;
			union
			{
				// Variable
				void *optional_ref;
				int optional_id;
				int tb_optional_reg;
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
	bool is_raw;
	union
	{
		struct
		{
			Expr **associated;
			uint32_t inner_ordinal;
		};
		Expr *value;
	};
} EnumConstantDecl;


typedef struct
{
	Decl** values;
	Decl** parameters;
	TypeInfo *type_info;
	int16_t inline_index;
	bool inline_value;
} EnumDecl;

struct Signature_
{
	CalleeAttributes attrs;
	bool is_macro : 1;
	bool is_at_macro : 1;
	bool is_safemacro : 1;
	Variadic variadic : 3;
	CallABI abi : 8;
	unsigned vararg_index;
	TypeInfoId rtype;
	Decl** params;
};

typedef struct
{
	TypeInfoId type_parent;
	unsigned overload_type : 2;
	unsigned is_wildcard_overload : 1;
	OperatorOverload operator : 6;
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
			bool attr_benchmark : 1;
			bool attr_test : 1;
			bool attr_winmain : 1;
			bool attr_optional : 1;
			bool attr_init : 1;
			bool attr_finalizer : 1;
			bool attr_interface_method : 1;
			bool attr_dynamic : 1;
			bool attr_nosanitize_address : 1;
			bool attr_nosanitize_memory : 1;
			bool attr_nosanitize_thread : 1;
			bool is_lambda : 1;
			bool in_macro : 1;
			union
			{
				uint32_t priority;
				DeclId interface_method;
				DeclId default_method;
				Decl **generated_lambda;
				Decl **lambda_ct_parameters;
			};
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
	Signature signature;
	AstId docs;
} FnTypeDecl;


typedef struct
{
	Decl **params;
	Attr **attrs;
} AttrDecl;

typedef struct
{
	bool is_func : 1;
	bool is_redef : 1;
	union
	{
		Decl *decl;
		TypeInfo *type_info;
	};
} TypeAliasDecl;

typedef struct
{
	union
	{
		Expr *alias_expr;
		Decl *alias;
	};
} DefineDecl;

typedef struct
{
	Path *alias_path;
	Module *module;
} ModuleAliasDecl;

typedef struct
{
	AstId defer;
	bool next_target : 1;
	union
	{
		void *break_target;
		int tb_break_target;
	};
	void *continue_target;
	AstId scope_defer;
	AstId parent;
} LabelDecl;

typedef struct
{
	DeclId overloads[OVERLOADS_COUNT + 1];
	DeclTable method_table;
	Decl **methods;
} Methods;

typedef struct Decl_
{
	const char *name;
	const char *extname;
	SourceSpan span;
	DeclKind decl_kind : 7;
	ResolveStatus resolve_status : 3;
	Visibility visibility : 3;
	bool has_tag : 1;
	bool is_packed : 1;
	bool is_extern : 1;
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
	bool is_export : 1;
	bool is_live : 1;
	bool no_strip : 1;
	bool is_cond : 1;
	bool is_if : 1;
	bool attr_nopadding : 1;
	bool attr_compact : 1;
	bool resolved_attributes : 1;
	bool allow_deprecated : 1;
	bool attr_structlike : 1;
	union
	{
		void *backend_ref;
		int backend_id;
		int tb_register;
		void *backend_value;
		void *tb_symbol;
		bool in_init;
	};
	AlignSize offset;
	AlignSize padding;
	AlignSize alignment;
	struct CompilationUnit_ *unit;
	union
	{
		Attr **attributes;
		ResolvedAttrData *attrs_resolved;
	};
	Type *type;
	union
	{
		Decl** decl_list;
		struct
		{
			TypeInfo **interfaces;
			Methods *method_table;
			union
			{
				// Enums and Fault
				EnumDecl enums;
				struct
				{
					TypeInfo *distinct;
					Expr *distinct_align;
				};
				// Unions, Struct, Bitstruct use strukt
				StructDecl strukt;
				Decl **interface_methods;
			};
		};
		AttrDecl attr_decl;
		Decl** body_params;
		Ast *ct_assert_decl;
		Ast *ct_echo_decl;
		Decl** ct_else_decl;
		Decl** decls;
		DefineDecl define_decl;
		ModuleAliasDecl module_alias_decl;
		EnumConstantDecl enum_constant;
		ExecDecl exec_decl;
		Expr* expand_decl;
		FnTypeDecl fntype_decl;
		FuncDecl func_decl;
		ImportDecl import;
		IncludeDecl include;
		LabelDecl label;
		TypeAliasDecl type_alias_decl;
		VarDecl var;
	};
} Decl;

static_assert(sizeof(void*) != 8 || sizeof(Decl) == 136, "Decl has unexpected size.");

typedef enum RangeType
{
	RANGE_DYNAMIC,
	RANGE_CONST_END,
	RANGE_CONST_LEN,
	RANGE_CONST_RANGE,
	RANGE_SINGLE_ELEMENT,
} RangeType;


typedef struct
{
	ResolveStatus status : 3;
	RangeType range_type : 4;
	bool start_from_end : 1;
	bool end_from_end : 1;
	bool is_len : 1;
	bool is_optional : 1;
	union
	{
		struct
		{
			ExprId start;
			union
			{
				ExprId end;
				ArrayIndex const_end;
			};
		};
		struct
		{
			ArrayIndex start_index;
			ArrayIndex len_index;
		};
	};
} Range;

typedef struct
{
	ExprId cond;
	ExprId then_expr; // May be null for elvis!
	ExprId else_expr;
	bool grouped : 1;
	bool is_const : 1;
} ExprTernary;

typedef struct
{
	ExprId left;
	ExprId right;
	BinaryOp operator : 8;
	bool grouped : 1;
} ExprBinary;

typedef struct
{
	Expr* expr;
	UnaryOp operator : 8;
	bool no_wrap : 1;
	bool no_read : 1;
} ExprUnary;



typedef struct
{
	union
	{
		ExprId function;
		DeclId func_ref;
	};
	union
	{
		ExprId macro_body;
		AstId function_contracts;
	};
	bool is_type_method : 1;
	bool is_pointer_call : 1;
	bool attr_force_inline : 1;
	bool attr_force_noinline : 1;
	bool is_builtin : 1;
	bool is_func_ref : 1;
	bool attr_pure : 1;
	bool no_return : 1;
	bool is_dynamic_dispatch : 1;
	bool has_optional_arg : 1;
	bool must_use : 1;
	bool is_optional_return : 1;
	bool va_is_splat : 1;
	bool is_outer_call : 1;
	Expr **arguments;
	union {
		Expr **varargs;
		Expr *vasplat;
	};
} ExprCall;

typedef struct
{
	Ast *body;
	Decl **body_arguments;
} ExprMacroBody;

typedef struct {
	bool start_from_end : 1;
	ExprId expr;
} SubscriptIndex;

typedef struct
{
	ExprId expr;
	SubscriptIndex index;
	bool no_check;
	bool ref;
} ExprSubscript;

typedef struct
{
	Decl *var;
	ArrayIndex index;
} ExprCtSubscript;

typedef struct
{
	ExprId expr;
	Range range;
} ExprSlice;

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
	ExprId offset;
} ExprPointerOffset;


typedef struct
{
	BuiltinAccessKind kind : 8;
	ExprId inner;
} ExprBuiltinAccess;

typedef struct
{
	Expr *filename;
	Expr *len;
} ExprEmbedExpr;

typedef struct
{
	ExprId parent;
	Expr **parameters;
} ExprGenericIdent;

typedef struct
{
	Expr *parent;
	Decl *ref;
} ExprResolvedAccess;

typedef struct
{
	Expr *parent;
	Expr *child;
	bool is_lvalue;
	bool is_ref;
} ExprUnresolvedAccess;

typedef struct DesignatorElement_
{
	DesignatorType kind : 4;
	union
	{
		Expr *field_expr;
		struct
		{
			Expr *index_expr;
			Expr *index_end_expr;
		};
	};
	ArrayIndex index;
	ArrayIndex index_end;
} DesignatorElement;

typedef struct
{
	DesignatorElement **path;
	Expr *value;
} ExprDesignator;

typedef struct
{
	const char *name;
	SourceSpan name_span;
	Expr *value;
} ExprNamedArgument;

typedef struct
{
	Decl *type;
	TypeProperty property;
} ExprTypeCall;

typedef struct
{
	Path *path;
	const char *ident;
	bool is_const;
} ExprUnresolvedIdentifier;

typedef struct
{
	SourceSpan loc;
	Expr *inner;
} ExprDefaultArg;

typedef struct
{
	const char *identifier;
	bool is_ref : 1;
	bool is_rvalue : 1;
	Decl *decl;
} ExprIdentifierRaw;

typedef struct
{
	TokenType token_type;
	union
	{
		struct
		{
			Expr *main_var;
			DesignatorElement **flat_path;
		};
		struct
		{
			TypeInfoId type_from;
			TypeInfoId type_to;
		};
	};
} ExprCtCall;

typedef struct
{
	AsmArgKind kind : 8;
	unsigned short index : 16;
	AsmOffsetType offset_type : 6;
	bool neg_offset : 1;
	bool resolved : 1;
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
		struct {
			uint64_t value;
			unsigned bits;
			bool is_neg;
		};
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
	bool is_and;
	Expr** args;
} ExprCtAndOr;


typedef struct
{
	ExprId expr;
	TypeInfoId type_info;
} ExprCast;

typedef struct
{
	Expr *first;
	Expr *last;
} ExprTwo;

typedef struct
{
	Expr **values;
	Decl **declarations;
	AstId first_stmt;
} ExprBodyExpansion;


typedef struct
{
	void *block_return_exit;
	void *block_optional_exit;
	void *block_error_var;
	void *block_return_out;
} BlockExit;



typedef struct
{
	AstId first_stmt;
	bool is_noreturn : 1;
	bool is_must_use : 1;
	bool is_optional_return : 1;
	bool had_optional_arg : 1;
	Decl **params;
	Decl *macro;
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
	BlockExit **in_block;
} ExprGuard;


typedef struct
{
	Decl *decl;
	Expr **exprs;
} ExprCatch;

typedef struct
{
	Expr *variable;
	TypeInfo *type;
	Expr **exprs;
} ExprUnresolvedCatch;

typedef struct
{
	Expr *variable;
	TypeInfo *type;
	Expr *init;
} ExprUnresolvedTry;

typedef struct
{
	bool assign_existing : 1;
	Expr *optional;
	union
	{
		Decl *decl;
		Expr *lhs;
	};
} ExprTry;


typedef struct
{
	const char *ident;
	BuiltinFunction builtin;
} ExprBuiltin;

typedef struct
{
	ExprId parent;
	TypeIdInfoKind kind;
} ExprTypeidInfo;

typedef struct
{
	ExprId parent;
	bool is_overlapping;
	const char *swizzle;
} ExprSwizzle;

typedef struct
{
	ExprId expr;
	ExprId type;
} ExprCastable;

typedef struct
{
	Expr *inner;
	SemaContext *context;
	SourceSpan inline_at;
	bool is_ref;
} ExprOtherContext;

typedef struct
{
	Expr **list;
	Expr *splat;
} ExprDesignatedInit;

typedef struct
{
	Expr *inner;
	bool is_signed;
} ExprExtTrunc;

typedef struct
{
	Expr *inner;
	bool negate;
} ExprIntToBool;

typedef struct
{
	Expr *inner;
	Expr *typeid;
} ExprMakeAny;

typedef struct
{
	Expr *ptr;
	ArraySize len;
} ExprMakeSlice;

struct Expr_
{
	Type *type;
	SourceSpan span;
	ExprKind expr_kind : 8;
	ResolveStatus resolve_status : 4;
	union {
		ExprResolvedAccess access_resolved_expr;
		ExprUnresolvedAccess access_unresolved_expr;// 16
		ExprBinary binary_expr;                     // 12
		ExprBinary veccomp_expr;
		ExprBodyExpansion body_expansion_expr;      // 24
		ExprBuiltinAccess builtin_access_expr;      // 8
		ExprBuiltin builtin_expr;                   // 16
		ExprCall call_expr;                         // 40
		ExprCast cast_expr;                         // 12
		ExprUnresolvedCatch unresolved_catch_expr;  // 24
		ExprCatch catch_expr;                       // 24
		Expr** cond_expr;                           // 8
		ExprConst const_expr;                       // 32
		ExprCtArg ct_arg_expr;
		Expr** ct_concat;
		ExprOtherContext expr_other_context;
		ExprCastable assignable_expr;
		ExprCtCall ct_call_expr;                    // 24
		ExprIdentifierRaw ct_ident_expr;            // 24
		Decl *decl_expr;                            // 8
		Decl *iota_decl_expr;                       // 8
		ExprDesignatedInit designated_init;         // 16
		ExprDesignator designator_expr;             // 16
		ExprNamedArgument named_argument_expr;
		ExprEmbedExpr embed_expr;                   // 16
		Expr **exec_expr;                           // 8
		ExprAsmArg expr_asm_arg;                    // 24
		ExprCompoundLiteral expr_compound_literal;  // 16
		Expr **expression_list;                     // 8
		ExprIntToBool int_to_bool_expr;
		ExprExtTrunc ext_trunc_expr;
		ExprGenericIdent generic_ident_expr;
		ExprDefaultArg default_arg_expr;
		ExprIdentifierRaw hash_ident_expr;          // 24
		ExprUnresolvedIdentifier unresolved_ident_expr;// 24
		Decl *ident_expr;                           // 8
		Expr** initializer_list;                    // 8
		Expr *inner_expr;                           // 8
		ExprMakeAny make_any_expr;
		ExprMakeSlice make_slice_expr;
		Decl *lambda_expr;                          // 8
		ExprMacroBlock macro_block;                 // 24
		ExprMacroBody macro_body_expr;              // 16
		Decl *member_get_expr;                      // 8
		OperatorOverload overload_expr;             // 4
		ExprPointerOffset pointer_offset_expr;
		ExprGuard rethrow_expr;                     // 16
		ExprSliceAssign slice_assign_expr;          // 8
		ExprSubscriptAssign subscript_assign_expr;
		ExprSubscript subscript_expr;               // 12
		ExprCtSubscript ct_subscript_expr;
		ExprSlice slice_expr;
		ExprSwizzle swizzle_expr;
		ExprTernary ternary_expr;                   // 16
		ExprTwo two_expr;
		BuiltinDefine benchmark_hook_expr;
		ExprTypeCall type_call_expr;
		BuiltinDefine test_hook_expr;
		Expr** try_unwrap_chain_expr;               // 8
		ExprTry try_expr;                          // 24
		ExprUnresolvedTry unresolved_try_expr;     // 24
		TypeInfo *type_expr;                        // 8
		TypeInfo *typeid_expr;                      // 8
		ExprTypeidInfo typeid_info_expr;            // 8
		ExprUnary unary_expr;                       // 16
		Range vasplat_expr;
	};
};

static_assert(sizeof(void*) != 8 || sizeof(Expr) == 56, "Expr not expected size");

typedef struct
{
	AstId first_stmt;
	AstId parent_defer;
} AstCompoundStmt;

typedef struct
{
	Expr *expr; // May be NULL
	AstId cleanup;
	AstId cleanup_fail;
	bool cleanup_catch;
	BlockExit** block_exit_ref; // For block exits
} AstReturnStmt;

typedef struct
{
	DeclId label;
	bool has_break : 1;
	bool no_exit : 1;
	bool skip_first : 1;
	bool if_chain : 1;
	bool jump : 1;
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
			union
			{
				void *break_block;
				int tb_break_block;
			};
		} codegen;
	};
} AstIfStmt;


typedef struct
{
	ExprId expr;
	ExprId to_expr;
	Ast *body;
	void *backend_block;
} AstCaseStmt;


typedef struct
{
	FlowCommon flow;
	Ast **cases;
	union
	{
		struct
		{
			ExprId cond;
			AstId defer;
			Ast *scope_defer;
		};
		struct
		{
			void *exit_block;
			union
			{
				struct {
					void *block;
					void *var;
				} retry;
				struct {
					uint16_t count;
					int16_t min_index;
					int16_t default_index;
					void *jmptable;
				} jump;
			};
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
	ExprId enumeration;
	AstId body;
	DeclId index;
	DeclId variable;
} AstForeachStmt;

typedef struct
{
	AstId prev_defer;
	AstId body; // Compound statement
	void *scope;
	bool is_try : 1;
	bool is_catch : 1;
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
	const char *var_name;
	Expr *type_expr;
} AstCtTypeAssignStmt;

typedef struct
{
	DeclId index;
	DeclId value;
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
			ExprId expr;
			bool is_default;
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
	AstId asm_stmt;
	Ast **labels;
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
	Expr **args;
} AstAssertStmt;


typedef struct
{
	bool resolved;
	bool expanding;
	union
	{
		Expr *expr;
		Decl *decl;
	};
} AstDocFault;

typedef struct AstDocDirective_
{
	ContractKind kind : 4;
	union
	{
		struct
		{
			const char *name;
			SourceSpan span;
			InOutModifier modifier : 4;
			bool by_ref : 1;
		} param;
		Ast **faults;
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
		struct
		{
			const char *string;
			size_t strlen;
		};
	};
} AstContractStmt;

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
		const char *asm_label;
		AstAssertStmt assert_stmt;          // 16
		AstCaseStmt case_stmt;              // 32
		AstCompoundStmt compound_stmt;      // 12
		AstId ct_compound_stmt;
		AstContinueBreakStmt contbreak_stmt;// 24
		AstContractStmt contract_stmt;      // 32
		AstDocFault contract_fault;         // 24
		AstId ct_else_stmt;                 // 4
		AstCtTypeAssignStmt ct_type_assign_stmt;
		AstCtForeachStmt ct_foreach_stmt;   // 40
		AstCtIfStmt ct_if_stmt;             // 24
		AstCtSwitchStmt ct_switch_stmt;     // 16
		Decl *declare_stmt;                 // 8
		Decl **decls_stmt;
		AstDeferStmt defer_stmt;            // 8
		Expr *expr_stmt;                    // 8
		Expr *expand_stmt;                  // 8
		AstForStmt for_stmt;                // 32
		AstForeachStmt foreach_stmt;        // 40
		AstIfStmt if_stmt;                  // 32
		AstNextcaseStmt nextcase_stmt;      // 32
		AstReturnStmt return_stmt;          // 16
		AstSwitchStmt switch_stmt;          // 40
		Decl *var_stmt;                     // 8
	};
} Ast;


static_assert(sizeof(void*) != 8 || sizeof(Ast) == 56, "Not expected Ast size");

typedef struct Module_
{
	Path *name;
	const char *short_path;
	// Extname in case a module is renamed externally
	const char *extname;

	const char **parameters;
	bool is_external : 1;
	bool is_c_library : 1;
	bool is_exported : 1;
	bool is_generic : 1;
	bool no_extprefix : 1;
	AnalysisStage stage : 6;

	AstId contracts;
	HTable symbols;
	struct CompilationUnit_ **units;
	Module *generic_module;
	Module *parent_module;
	Module *top_module;
	Module **sub_modules;
	Decl **benchmarks;
	Decl **tests;
	Decl **lambdas_to_evaluate;
	const char *generic_suffix;
	InliningSpan inlined_at;
} Module;


typedef struct EndJump_
{
	bool active;
	SourceSpan span;
} EndJump;

typedef struct DynamicScope_
{
	bool allow_dead_code : 1;
	bool is_dead : 1;
	bool is_poisoned : 1;
	EndJump end_jump;
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
            uint64_t is_base64 : 1;
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

struct CompilationUnit_
{
	Module *module;
	File *file;
	Decl **imports;
	Decl **module_aliases;
	Decl **public_imports;
	Decl **types;
	Decl **functions;
	Decl **lambdas;
	Decl **enums;
	Decl **attributes;
	Decl **faulttypes;
	Decl **faults;
	const char **links;
	Visibility default_visibility;
	Attr *if_attr;
	bool export_by_default;
	bool is_interface_file;
	bool benchmark_by_default;
	bool test_by_default;
	bool module_generated;
	Attr **attr_links;
	Decl **generic_defines;
	Decl **ct_asserts;
	Decl **ct_echos;
	Decl **ct_includes;
	Decl **vars;
	Decl **macros;
	Decl **methods_to_register;
	Decl **generic_methods_to_register;
	Decl **methods;
	Decl **macro_methods;
	Decl **global_decls;
	Decl **global_cond_decls;
	Decl *main_function;
	Decl *error_import;
	HTable local_symbols;
	int lambda_count;
	TypeInfo **check_type_variable_array;
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

typedef struct
{
	CallEnvKind kind : 8;
	bool ensures : 1;
	bool pure : 1;
	bool in_no_eval : 1;
	bool is_naked_fn : 1;
	bool ignore_deprecation : 1;
	SourceSpan in_if_resolution;
	Decl **opt_returns;
	union
	{
		Decl *attr_declaration;
		Decl *current_function;
	};
} CallEnv;

typedef struct JumpTarget_
{
	Ast *target;
	AstId defer;
} JumpTarget;



struct SemaContext_
{
	// Evaluated in this.
	CompilationUnit *unit;
	// Compiled in this unit.
	CompilationUnit *compilation_unit;
	CallEnv call_env;
	Decl *current_macro;
	InliningSpan *inlined_at;
	unsigned macro_call_depth;
	// Jump tracking
	JumpTarget break_jump;
	JumpTarget continue_jump;
	JumpTarget next_jump;
	AstId block_return_defer;
	Ast *next_switch;
	struct
	{
		uint32_t original_inline_line;
		Module *original_module;
		Decl **yield_params;
		Ast *yield_body;
		BlockExit** block_exit_ref;
		Type *expected_block_type;
		Ast **block_returns;
		Expr **macro_varargs;
		bool macro_has_vaargs;
		Decl **macro_params;
		bool macro_has_ensures;
		Decl** ct_locals;
	};
	Type *rtype;
	SemaContext *yield_context;
	Decl** locals;
	DynamicScope active_scope;
	Expr *return_expr;
	bool is_temp;
	struct
	{
		Module *infer;
	} generic;
};

typedef enum
{
	ABI_TYPE_INT_24 = 1,
	ABI_TYPE_INT_40 = 3,
	ABI_TYPE_INT_48 = 5,
	ABI_TYPE_INT_56 = 7,
	ABI_TYPE_INT_VEC_2 = 9,
	ABI_TYPE_INT_VEC_4 = 11,
	ABI_TYPE_FLOAT_VEC_2 = 13,
	ABI_TYPE_FLOAT_VEC_4 = 15,
	ABI_TYPE_LONG_VEC_2 = 17,
	ABI_TYPE_FLOAT16_VEC_2 = 19,
	ABI_TYPE_FLOAT16_VEC_4 = 21,
	ABI_TYPE_BFLOAT16_VEC_2 = 23,
	ABI_TYPE_BFLOAT16_VEC_4 = 25,
	ABI_TYPE_DOUBLE_VEC_2 = 27,
	ABI_TYPE_DOUBLE_VEC_4 = 29,
	ABI_TYPE_DOUBLE_VEC_8 = 31,
} AbiSpecType;

typedef struct
{
	union
	{
		Type *type;
		AbiSpecType abi_type;
	};
} AbiType;

typedef struct ABIArgInfo_
{
	ArrayIndex param_index_start : 16;
	ArrayIndex param_index_end : 16;
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
			AbiType lo;
			AbiType hi;
		} direct_pair;
		struct
		{
			uint8_t offset_hi;
			bool packed;
			Type *lo;
			Type *hi;
		} coerce_expand;
		AbiType direct_coerce_type;
		uint8_t direct_struct_expand;
		struct
		{
			// We may request a certain alignment of the parameters.
			AlignSize alignment;
			Type *type;
		} indirect;
	};
	Type *original_type;
	ParamRewrite rewrite;
} ABIArgInfo;

typedef struct ParamInfo
{
	Type *type;
	ParamRewrite rewrite;
} ParamInfo;

typedef struct FunctionPrototype_
{
	CallABI call_abi : 4;
	bool raw_variadic : 1;
	bool use_win64 : 1;
	bool is_resolved : 1;
	unsigned short vararg_index;
	RetValType ret_rewrite : 8;
	ParamRewrite return_rewrite : 3;
	ParamInfo return_info;
	Type *return_result;
	unsigned param_count;
	unsigned short param_vacount;
	ABIArgInfo *ret_abi_info;
	ABIArgInfo **abi_args;
	ABIArgInfo **abi_varargs;
	Type *raw_type;
} FunctionPrototype;

typedef struct
{
	Decl *ambiguous_other_decl;
	Decl *private_decl;
	Decl *maybe_decl;
	Decl *found;
	Path *path;
	SourceSpan span;
	const char *symbol;
	Module *path_found;
	bool suppress_error;
	bool is_parameterized;
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
	bool is_template;
} CopyStruct;

typedef struct
{
	const char **links;
	bool link_math;
} Linking;

typedef struct
{
	bool should_print_environment;
	bool should_print_asm;
	Ansi ansi;
	HTable modules;
	Module *core_module;
	CompilationUnit *core_unit;
	Module **module_list;
	Module **generic_module_list;
	Type **type;
	const char *lib_dir;
	const char **sources;
	File **loaded_sources;
	bool in_panic_mode : 1;
	unsigned errors_found;
	unsigned warnings_found;
	unsigned includes_used;
	Decl ***locals_list;
	HTable compiler_defines;
	HTable features;
	Module std_module;
	MethodTable method_extensions;
	Decl **method_extension_list;
	DeclTable symbols;
	PathTable path_symbols;
	Path std_module_path;
	Type *string_type;
	Decl *panic_var;
	Decl *panicf;
	Decl *io_error_file_not_found;
	Decl *main;
	Decl *decl_stack[MAX_GLOBAL_DECL_STACK];
	Decl **decl_stack_bottom;
	Decl **decl_stack_top;
} GlobalContext;

typedef struct
{
	BuildTarget build;
	PlatformTarget platform;
	Linking linking;
	GlobalContext context;
	const char *obj_output;
	int generic_depth;
	double exec_time;
	double script_time;
} CompilerState;

extern CompilerState compiler;
extern Ast *poisoned_ast;
extern Decl *poisoned_decl;
extern Expr *poisoned_expr;
extern Type *poisoned_type;
extern TypeInfo *poisoned_type_info;

extern Type *type_bool, *type_void, *type_voidptr;
extern Type *type_float16, *type_bfloat, *type_float, *type_double, *type_f128;
extern Type *type_ichar, *type_short, *type_int, *type_long, *type_isz;
extern Type *type_char, *type_ushort, *type_uint, *type_ulong, *type_usz;
extern Type *type_iptr, *type_uptr;
extern Type *type_u128, *type_i128;
extern Type *type_typeid, *type_fault, *type_any, *type_typeinfo, *type_member;
extern Type *type_untypedlist;
extern Type *type_wildcard;
extern Type *type_cint;
extern Type *type_cuint;
extern Type *type_chars;
extern Type *type_wildcard_optional;
extern Type *type_string;
extern Type *type_reflected_param;
extern File stdin_file;

extern const char *attribute_list[NUMBER_OF_ATTRIBUTES];
extern const char *builtin_list[NUMBER_OF_BUILTINS];
extern const char *builtin_defines[NUMBER_OF_BUILTIN_DEFINES];
extern const char *type_property_list[NUMBER_OF_TYPE_PROPERTIES];
extern const char *kw_std__core;
extern const char *kw_std__core__types;
extern const char *kw_std__core__runtime;
extern const char *kw_std__io;
extern const char *kw_typekind;
extern const char *kw_FILE_NOT_FOUND;
extern const char *kw_IoError;

extern const char *kw_at_align;
extern const char *kw_at_deprecated;
extern const char *kw_at_ensure;
extern const char *kw_at_enum_lookup;
extern const char *kw_at_jump;
extern const char *kw_at_param;
extern const char *kw_at_pure;
extern const char *kw_at_require;
extern const char *kw_at_return;
extern const char *kw_at_simd;
extern const char *kw_in;
extern const char *kw_inout;
extern const char *kw_len;
extern const char *kw_libc;
extern const char *kw_main;
extern const char *kw_mainstub;
extern const char *kw_memcmp;
extern const char *kw_nameof;
extern const char *kw_offsetof;
extern const char *kw_ordinal;
extern const char *kw_out;
extern const char *kw_ptr;
extern const char *kw_return;
extern const char *kw_self;
extern const char *kw_std;
extern const char *kw_type;
extern const char *kw_winmain;
extern const char *kw_wmain;
extern ArchOsTarget default_target;

ARENA_DEF(chars, char)
ARENA_DEF(ast, Ast)
ARENA_DEF(expr, Expr)
ARENA_DEF(decl, Decl)
ARENA_DEF(type_info, TypeInfo)

INLINE Ast *ast_new(AstKind kind, SourceSpan span)
{
	Ast *ast = ast_calloc();
	ast->ast_kind = kind;
	ast->span = span;
	return ast;
}

INLINE TypeInfo *vartype(Decl *var)
{
	return type_infoptrzero(var->var.type_info);
}

INLINE Type *typeget(TypeInfoId id_)
{
	return id_ ? type_infoptr(id_)->type : NULL;
}

INLINE bool no_panic(void)
{
	return compiler.build.feature.panic_level == PANIC_OFF;
}

INLINE bool safe_mode_enabled(void)
{
	return compiler.build.feature.safe_mode != SAFETY_OFF;
}

INLINE bool link_libc(void)
{
	return compiler.build.link_libc != LINK_LIBC_OFF;
}

INLINE bool strip_unused(void)
{
	return compiler.build.strip_unused != STRIP_UNUSED_OFF;
}

INLINE bool no_stdlib(void)
{
	return compiler.build.use_stdlib == USE_STDLIB_OFF;
}

INLINE bool compile_asserts(void)
{
	return safe_mode_enabled() || compiler.build.testing;
}

void assert_print_line(SourceSpan span);

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

AttributeType attribute_by_name(const char *name);

void type_setup(PlatformTarget *target);
Float float_add(Float op1, Float op2);
Float float_sub(Float op1, Float op2);
Float float_mul(Float op1, Float op2);
Float float_div(Float op1, Float op2);
Float float_neg(Float op);
Float float_rem(Float op1, Float op2);
Float float_from_string(const char *string, char **error);
Float float_from_hex(const char *string, char **error);
Int128 i128_from_double_unsigned(double x);
bool int_ucomp(Int op1, uint64_t op2, BinaryOp op);
bool int_icomp(Int op1, int64_t op2, BinaryOp op);
bool int_comp(Int op1, Int op2, BinaryOp op);
uint64_t int_to_u64(Int op);
int64_t int_to_i64(Int op);
bool int_is_zero(Int op);
unsigned int_bits_needed(Int op);
bool int_fits(Int op1, TypeKind kind);
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
char *int_to_str(Int i, int radix, bool use_prefix);
bool i128_can_convert_from_double(double x);
bool i128_can_convert_from_double_signed(double x);
Int128 i128_from_double_unsigned(double x);
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
char *i128_to_string(Int128 op, uint64_t base, bool is_signed, bool use_prefix);
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
Int128 i128_from_signed(int64_t i);
UNUSED Int128 i128_from_unsigned(uint64_t i);
UNUSED bool i128_get_bit(const Int128 *op, int bit);

// copy ---

#define MACRO_COPY_DECL(x) x = copy_decl(c, x)
#define MACRO_COPY_DECLID(x) x = declid_copy_deep(c, x)
#define MACRO_COPY_DECL_LIST(x) x = copy_decl_list(c, x)
#define MACRO_COPY_DECL_METHODS(x) x = copy_decl_methods(c, x)
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
Decl **copy_decl_list_single_for_unit(Decl **decl_list);
Attr **copy_attributes_single(Attr** attr_list);
Decl *copy_lambda_deep(Decl *decl);
Ast *copy_ast_single(Ast *source_ast);
Decl **copy_decl_list_macro(Decl **decl_list);
Ast *copy_ast_macro(Ast *source_ast);
Ast *copy_ast_defer(Ast *source_ast);
TypeInfo *copy_type_info_single(TypeInfo *type_info);
InliningSpan *copy_inlining_span(InliningSpan *span);

void init_asm(PlatformTarget *target);
void print_asm_list(PlatformTarget *target);
AsmRegister *asm_reg_by_name(PlatformTarget *target, const char *name);
AsmInstruction *asm_instr_by_name(const char *name);
INLINE const char *asm_clobber_by_index(unsigned index);
INLINE AsmRegister *asm_reg_by_index(unsigned index);

AsmRegister *asm_reg_by_index(unsigned index);
bool asm_is_supported(ArchType arch);

bool cast_implicit_silent(SemaContext *context, Expr *expr, Type *to_type, bool is_binary_conversion);

bool cast_implicit_binary(SemaContext *context, Expr *expr, Type *to_type, bool *failed_ref);
bool cast_implicit(SemaContext *context, Expr *expr, Type *to_type, bool is_binary);
bool cast_implicit_checked(SemaContext *context, Expr *expr, Type *to_type, bool is_binary, bool *failed_ref);
bool cast_explicit_silent(SemaContext *context, Expr *expr, Type *to_type);
bool cast_explicit(SemaContext *context, Expr *expr, Type *to_type);
bool cast_explicit_checkable(SemaContext *context, Expr *expr, Type *to_type, bool *failed_ref);
INLINE bool cast_both_implicit(SemaContext *context, Expr *expr1, Expr *expr2, Type *to_type, bool is_binary, bool *failed_ref)
{
	return cast_implicit_checked(context, expr1, to_type, is_binary, failed_ref) && cast_implicit_checked(context, expr2, to_type, is_binary, failed_ref);
}


bool may_cast(SemaContext *context, Expr *expr, Type *to_type, bool is_explicit, bool is_silent);

void cast_no_check(Expr *expr, Type *to_type, bool add_optional);


bool cast_to_index_len(SemaContext *context, Expr *index, bool is_len);

const char *llvm_codegen(void *context);
const char *tilde_codegen(void *context);
void **c_gen(Module** modules, unsigned module_count);
void **llvm_gen(Module** modules, unsigned module_count);
void **tilde_gen(Module** modules, unsigned module_count);

void header_gen(Module **modules, unsigned module_count);
const char *build_base_name(void);

void global_context_clear_errors(void);
void global_context_add_type(Type *type);
void global_context_add_decl(Decl *type_decl);

void linking_add_link(Linking *linker, const char *link);

Module *compiler_find_or_create_module(Path *module_name, const char **parameters);
Module *global_context_find_module(const char *name);
const char *get_object_extension(void);
const char *get_exe_extension(void);

CompilationUnit * unit_create(File *file);
void unit_register_global_decl(CompilationUnit *unit, Decl *decl);
void unit_register_external_symbol(SemaContext *context, Decl *decl);

bool unit_add_import(CompilationUnit *unit, Path *path, bool private_import, bool is_non_recursive);
bool unit_add_alias(CompilationUnit *unit, Decl *decl);
bool context_set_module_from_filename(ParseContext *context);
bool context_set_module(ParseContext *context, Path *path, const char **generic_parameters);
bool context_is_macro(SemaContext *context);

// --- Decl functions

Decl *decl_new(DeclKind decl_kind, const char *name, SourceSpan span);
Decl *decl_new_ct(DeclKind kind, SourceSpan span);
Decl *decl_new_with_type(const char *name, SourceSpan span, DeclKind decl_type);
Decl *decl_new_var(const char *name, SourceSpan span, TypeInfo *type, VarDeclKind kind);
Decl *decl_new_generated_var(Type *type, VarDeclKind kind, SourceSpan span);

const char *decl_safe_name(Decl *decl);
const char *decl_to_name(Decl *decl);
const char *decl_to_a_name(Decl *decl);
int decl_count_elements(Decl *structlike);
bool decl_is_defaulted_var(Decl *decl);
void decl_append_links_to_global_during_codegen(Decl *decl);

INLINE bool decl_ok(Decl *decl);
INLINE bool decl_poison(Decl *decl);
INLINE bool decl_is_struct_type(Decl *decl);
INLINE bool decl_is_user_defined_type(Decl *decl);
INLINE Decl *decl_flatten(Decl *decl);
static inline Decl *decl_raw(Decl *decl);
static inline DeclKind decl_from_token(TokenType type);
static inline bool decl_is_var_local(Decl *decl);
bool decl_is_ct_var(Decl *decl);
bool decl_is_deprecated(Decl *decl);
Decl *decl_find_enum_constant(Decl *decl, const char *name);
bool decl_needs_prefix(Decl *decl);
AlignSize decl_find_member_offset(Decl *decl, Decl *member);
bool decl_is_externally_visible(Decl *decl);
bool decl_is_local(Decl *decl);
bool decl_is_global(Decl *decl);
void scratch_buffer_set_extern_decl_name(Decl *decl, bool clear);

// --- Expression functions

#define EXPR_NEW_TOKEN(kind_) expr_new(kind_, c->span)
Expr *expr_new(ExprKind kind, SourceSpan start);
Expr *expr_new_const_int(SourceSpan span, Type *type, uint64_t v);
Expr *expr_new_const_bool(SourceSpan span, Type *type, bool value);
Expr *expr_new_const_typeid(SourceSpan span, Type *type);
Expr *expr_new_const_string(SourceSpan span, const char *string);
Expr *expr_new_const_null(SourceSpan span, Type *type);
Expr *expr_new_const_initializer(SourceSpan span, Type *type, ConstInitializer *initializer);
Expr *expr_new_expr_list_resolved(SourceSpan span, Type *type, Expr **expressions);
Expr *expr_new_binary(SourceSpan span, Expr *left, Expr *right, BinaryOp op);
Expr *expr_new_cond(Expr *expr);
const char *expr_kind_to_string(ExprKind kind);
bool expr_is_simple(Expr *expr, bool to_float);
bool expr_is_pure(Expr *expr);
bool expr_is_runtime_const(Expr *expr);
Expr *expr_new_two(Expr *first, Expr *second);
void expr_rewrite_two(Expr *original, Expr *first, Expr *second);
void expr_insert_addr(Expr *original);
bool sema_expr_rewrite_insert_deref(SemaContext *context, Expr *original);
Expr *expr_generate_decl(Decl *decl, Expr *assign);
Expr *expr_generated_local(Expr *assign, Decl **decl_ref);
Expr *expr_variable(Decl *decl);
Expr *expr_negate_expr(Expr *expr);
bool expr_may_addr(Expr *expr);
bool expr_in_int_range(Expr *expr, int64_t low, int64_t high);
bool expr_is_unwrapped_ident(Expr *expr);
bool expr_is_zero(Expr *expr);
INLINE Expr *expr_new_expr(ExprKind kind, Expr *expr);
INLINE bool expr_ok(Expr *expr);
INLINE void expr_resolve_ident(Expr *expr, Decl *decl);
INLINE bool exprid_is_simple(ExprId expr_id, bool to_float);
INLINE bool exprid_is_pure(ExprId expr_id);
INLINE Type *exprtype(ExprId expr_id);
INLINE void expr_replace(Expr *expr, Expr *replacement);
INLINE bool expr_poison(Expr *expr);
INLINE bool exprid_is_runtime_const(ExprId expr);
INLINE bool expr_is_init_list(Expr *expr);
INLINE bool expr_is_neg(Expr *expr);
INLINE bool expr_is_mult(Expr *expr);
INLINE bool expr_is_deref(Expr *expr);
INLINE bool expr_is_const(Expr *expr);
INLINE bool expr_is_const_int(Expr *expr);
INLINE bool expr_is_const_float(Expr *expr);
INLINE bool expr_is_const_string(Expr *expr);
INLINE bool expr_is_const_initializer(Expr *expr);
INLINE bool expr_is_const_untyped_list(Expr *expr);
INLINE bool expr_is_const_member(Expr *expr);

INLINE void expr_rewrite_const_null(Expr *expr, Type *type);
INLINE void expr_rewrite_const_bool(Expr *expr, Type *type, bool b);
INLINE void expr_rewrite_const_float(Expr *expr, Type *type, Real d);
INLINE void expr_rewrite_const_int(Expr *expr, Type *type, uint64_t v);
INLINE void expr_rewrite_const_typeid(Expr *expr, Type *type);
INLINE void expr_rewrite_const_initializer(Expr *expr, Type *type, ConstInitializer *initializer);
INLINE void expr_rewrite_const_untyped_list(Expr *expr, Expr **elements);
void expr_rewrite_const_string(Expr *expr_to_rewrite, const char *string);
void expr_rewrite_const_ref(Expr *expr_to_rewrite, Decl *decl);

void expr_rewrite_to_builtin_access(Expr *expr, Expr *parent, BuiltinAccessKind kind, Type *type);
void expr_rewrite_to_const_zero(Expr *expr, Type *type);
bool expr_rewrite_to_const_initializer_index(Type *list_type, ConstInitializer *list, Expr *result, unsigned index, bool from_back);

void expr_rewrite_to_binary(Expr *expr_to_rewrite, Expr *left, Expr *right, BinaryOp op);

Expr *expr_from_const_expr_at_index(Expr *expr, ArrayIndex index);
bool expr_const_in_range(const ExprConst *left, const ExprConst *right, const ExprConst *right_to);
bool expr_const_compare(const ExprConst *left, const ExprConst *right, BinaryOp op);
void expr_contract_array(ExprConst *expr_const, ConstKind contract_type);
bool expr_const_will_overflow(const ExprConst *expr, TypeKind kind);
const char *expr_const_to_error_string(const ExprConst *expr);
bool expr_const_float_fits_type(const ExprConst *expr_const, TypeKind kind);
void expr_const_to_scratch_buffer(const ExprConst *expr);

// --- Lexer functions

void lexer_init(Lexer *lexer);
bool lexer_next_token(Lexer *lexer);

// --- Module functions

void scratch_buffer_append_module(Module *module, bool is_export);
Decl *module_find_symbol(Module *module, const char *symbol);
const char *module_create_object_file_name(Module *module);

bool parse_file(File *file);
Decl **parse_include_file(File *file, CompilationUnit *unit);
Ast *parse_include_file_stmts(File *file, CompilationUnit *unit);
bool parse_stdin(void);
Path *path_create_from_string(const char *string, uint32_t len, SourceSpan span);


typedef enum FindMember
{
	METHODS_AND_FIELDS,
	METHODS_INTERFACES_AND_FIELDS,
	FIELDS_ONLY
} FindMember;

void sema_analysis_run(void);
Decl **sema_decl_stack_store(void);
Decl *sema_decl_stack_find_decl_member(SemaContext *context, Decl *decl_owner, const char *symbol, FindMember find);
Decl *sema_decl_stack_resolve_symbol(const char *symbol);
void sema_decl_stack_restore(Decl **state);
void sema_decl_stack_push(Decl *decl);

bool sema_error_failed_cast(SemaContext *context, Expr *expr, Type *from, Type *to);
bool sema_add_local(SemaContext *context, Decl *decl);
void sema_unwrap_var(SemaContext *context, Decl *decl);
void sema_rewrap_var(SemaContext *context, Decl *decl);
void sema_erase_var(SemaContext *context, Decl *decl);
void sema_erase_unwrapped(SemaContext *context, Decl *decl);
bool sema_analyse_cond_expr(SemaContext *context, Expr *expr, CondResult *result);

bool sema_analyse_expr_rhs(SemaContext *context, Type *to, Expr *expr, bool allow_optional, bool *no_match_ref,
                           bool as_binary);

bool sema_analyse_expr_rvalue(SemaContext *context, Expr *expr);
bool sema_cast_const(Expr *expr);

bool sema_expr_check_discard(SemaContext *context, Expr *expr);
bool sema_analyse_inferred_expr(SemaContext *context, Type *to, Expr *expr, bool *no_match_ref);
bool sema_analyse_decl(SemaContext *context, Decl *decl);

bool sema_analyse_method_register(SemaContext *context, Decl *method);
bool sema_resolve_type_structure(SemaContext *context, Type *type);
bool sema_analyse_var_decl_ct(SemaContext *context, Decl *decl, bool *check_defined);
bool sema_analyse_var_decl(SemaContext *context, Decl *decl, bool local, bool *check_defined);
bool sema_analyse_ct_assert_stmt(SemaContext *context, Ast *statement);
bool sema_analyse_ct_echo_stmt(SemaContext *context, Ast *statement);
bool sema_analyse_statement(SemaContext *context, Ast *statement);

bool sema_expr_analyse_assign_right_side(SemaContext *context, Expr *expr, Type *left_type, Expr *right,
                                         bool is_unwrapped_var, bool is_declaration, bool *failed_ref);
bool sema_expr_analyse_initializer_list(SemaContext *context, Type *to, Expr *expr, bool *no_match_ref);
Expr **sema_expand_vasplat_exprs(SemaContext *context, Expr **exprs);

bool sema_expr_analyse_general_call(SemaContext *context, Expr *expr, Decl *decl, Expr *struct_var, bool optional,
                                    bool *no_match_ref);

void sema_expr_convert_enum_to_int(Expr *expr);
Decl *sema_decl_stack_resolve_symbol(const char *symbol);
Decl *sema_find_decl_in_modules(Module **module_list, Path *path, const char *interned_name);
Decl *sema_resolve_type_method(SemaContext *context, CanonicalType *type, const char *method_name);
Decl *sema_resolve_method(Decl *type, const char *method_name);
Decl *sema_resolve_method_only(Decl *type, const char *method_name);
Decl *sema_find_extension_method_in_list(Decl **extensions, Type *type, const char *method_name);
bool sema_resolve_type_decl(SemaContext *context, Type *type);
bool sema_check_type_variable_array(SemaContext *context, TypeInfo *type);
Decl *sema_find_symbol(SemaContext *context, const char *symbol);
Decl *sema_find_path_symbol(SemaContext *context, const char *symbol, Path *path);
Decl *sema_find_label_symbol(SemaContext *context, const char *symbol);
Decl *sema_find_label_symbol_anywhere(SemaContext *context, const char *symbol);
Decl *sema_find_local(SemaContext *context, const char *symbol);
Decl *sema_resolve_symbol(SemaContext *context, const char *symbol, Path *path, SourceSpan span);
Decl *sema_resolve_parameterized_symbol(SemaContext *context, const char *symbol, Path *path, SourceSpan span);
BoolErr sema_symbol_is_defined_in_scope(SemaContext *c, const char *symbol);

bool sema_resolve_array_like_len(SemaContext *context, TypeInfo *type_info, ArraySize *len_ref);

bool sema_resolve_type_info(SemaContext *context, TypeInfo *type_info, ResolveTypeKind kind);
bool sema_unresolved_type_is_generic(SemaContext *context, TypeInfo *type_info);

bool use_ansi(void);
void print_error_at(SourceSpan loc, const char *message, ...);
void print_error_after(SourceSpan loc, const char *message, ...);
void sema_note_prev_at(SourceSpan loc, const char *message, ...);
void sema_verror_range(SourceSpan location, const char *message, va_list args);
void sema_vwarn_range(SourceSpan location, const char *message, va_list args);
void print_error(ParseContext *context, const char *message, ...);

void sema_warning_at(SourceSpan loc, const char *message, ...);
void sema_shadow_error(SemaContext *context, Decl *decl, Decl *old);

bool sema_type_error_on_binop(SemaContext *context, Expr *expr);

File *source_file_by_id(FileId file);
File *source_file_load(const char *filename, bool *already_loaded, const char **error);
File *source_file_generate(const char *filename);
File *source_file_text_load(const char *filename, char *content);

File *compile_and_invoke(const char *file, const char *args, const char *stdin_data, size_t limit);
void compiler_parse(void);
bool compiler_should_output_file(const char *file);
void emit_json(void);

void stable_init(STable *table, uint32_t initial_size);
void *stable_set(STable *table, const char *key, void *value);
void *stable_get(STable *table, const char *key);

void htable_init(HTable *table, uint32_t initial_size);
void *htable_set(HTable *table, void *key, void *value);
void *htable_get(HTable *table, void *key);

void pathtable_init(PathTable *table, uint32_t initial_size);
void pathtable_set(PathTable *table, Decl *value);
Decl *pathtable_get(PathTable *table, const char *short_path, const char *name);

UNUSED void stable_clear(STable *table);

void decltable_init(DeclTable *table, uint32_t initial_size);
DeclId decltable_get(DeclTable *table, const char *name);
void decltable_set(DeclTable *table, Decl *decl);

void methodtable_init(MethodTable *table, uint32_t initial_size);
DeclId methodtable_get(MethodTable *table, Type *type, const char *name);
DeclId methodtable_set(MethodTable *table, Decl *method);

const char *scratch_buffer_interned(void);
const char *scratch_buffer_interned_as(TokenType *type);

const char *symtab_preset(const char *data, TokenType type);
const char *symtab_add(const char *symbol, uint32_t len, uint32_t fnv1hash, TokenType *type);
const char *symtab_find(const char *symbol, uint32_t len, uint32_t fnv1hash, TokenType *type);
void *llvm_target_machine_create(void);
void codegen_setup_object_names(Module *module, const char **base_name, const char **ir_filename, const char **asm_filename, const char **object_filename);
void target_setup(BuildTarget *build_target);
int target_alloca_addr_space();
bool os_is_apple(OsType os_type);
bool os_supports_stacktrace(OsType os_type);
bool arch_is_wasm(ArchType type);

const char *macos_sysroot(void);
MacSDK *macos_sysroot_sdk_information(const char *sdk_path);
WindowsSDK *windows_get_sdk(void);
const char *windows_cross_compile_library(void);

void c_abi_func_create(Signature *sig, FunctionPrototype *proto, Expr **vaargs);

bool token_is_any_type(TokenType type);
const char *token_type_to_string(TokenType type);

#define IS_OPTIONAL(element_) (type_is_optional((element_)->type))
#define IS_RESOLVED(element_) ((element_)->resolve_status == RESOLVE_DONE)
bool type_is_comparable(Type *type);
bool type_is_ordered(Type *type);
unsigned type_get_introspection_kind(TypeKind kind);
void type_mangle_introspect_name_to_buffer(Type *type);
AlignSize type_alloca_alignment(Type *type);
AlignSize type_abi_alignment(Type *type);
bool type_func_match(Type *fn_type, Type *rtype, unsigned arg_count, ...);
Type *type_find_largest_union_element(Type *type);
Type *type_find_max_type(Type *type, Type *other, Expr *first, Expr *second);
Type *type_find_max_type_may_fail(Type *type, Type *other);
Type *type_abi_find_single_struct_element(Type *type, bool in_abi);
Module *type_base_module(Type *type);
bool type_is_valid_for_vector(Type *type);
bool type_is_valid_for_array(Type *type);
Type *type_get_array(Type *arr_type, ArraySize len);
Type *type_array_from_vector(Type *vec_type);
Type *type_vector_from_array(Type *vec_type);
Type *type_get_indexed_type(Type *type);
Type *type_get_ptr(Type *ptr_type);
Type *type_get_func_ptr(Type *func_type);
Type *type_get_ptr_recurse(Type *ptr_type);
Type *type_get_slice(Type *arr_type);
Type *type_get_inferred_array(Type *arr_type);
Type *type_get_inferred_vector(Type *arr_type);
Type *type_get_flexible_array(Type *arr_type);
Type *type_get_optional(Type *optional_type);
Type *type_get_vector(Type *vector_type, TypeKind kind, unsigned len);
Type *type_get_vector_from_vector(Type *base_type, Type *orginal_vector);
Type *type_get_simd_from_vector(Type *orginal_vector);
Type *type_get_vector_bool(Type *original_type, TypeKind kind);

Type *type_int_signed_by_bitsize(BitSize bitsize);
Type *type_int_unsigned_by_bitsize(BitSize bit_size);
bool type_is_matching_int(CanonicalType *type1, CanonicalType *type2);
TypeSize type_size(Type *type); // Only call after all types are resolved.
void type_init_cint(void);
Type *type_new_func(Decl *decl, Signature *sig);
void type_func_prototype_init(uint32_t capacity);
Type *type_find_parent_type(Type *type);
bool type_is_subtype(Type *type, Type *possible_subtype);
bool type_is_abi_aggregate(Type *type);
bool type_is_aggregate(Type *type);
bool type_is_int128(Type *type);

Type *type_from_token(TokenType type);
bool type_is_structurally_equivalent(Type *type1, Type *type);
bool type_flat_is_floatlike(Type *type);
bool type_flat_is_intlike(Type *type);
bool type_flat_is_boolintlike(Type *type);
bool type_flat_is_numlike(Type *type);
bool type_may_have_sub_elements(Type *type);
bool type_may_have_method(Type *type);
void type_append_name_to_scratch(Type *type);

TypeCmpResult type_is_pointer_equivalent(SemaContext *context, Type *to_pointer, Type *from_pointer, bool flatten_distinct);
TypeCmpResult type_array_element_is_equivalent(SemaContext *context, Type *element1, Type *element2, bool is_explicit);
FunctionPrototype *type_get_resolved_prototype(Type *type);
bool type_is_inner_type(Type *type);
const char *type_to_error_string(Type *type);
const char *type_quoted_error_string(Type *type);
const char *type_quoted_error_string_with_path(Type *type);
const char *type_error_string_maybe_with_path(Type *type, Type *other_type);
const char *type_quoted_error_string_maybe_with_path(Type *type, Type *other_type);
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
INLINE bool type_is_floatlike(Type *type);
INLINE bool type_is_optional(Type *type);
INLINE bool type_is_wildcard(Type *type);
INLINE bool type_is_void(Type *type);
INLINE bool type_is_integer(Type *type);
INLINE bool type_is_integer_unsigned(Type *type);
INLINE bool type_is_integer_signed(Type *type);
INLINE bool type_is_integer_or_bool_kind(Type *type);
INLINE bool type_is_numeric(Type *type);
INLINE bool type_is_inferred(Type *type);
INLINE bool type_underlying_is_numeric(Type *type);
INLINE bool type_underlying_may_add_sub(CanonicalType *type);
INLINE bool type_is_pointer(Type *type);
INLINE bool type_is_arraylike(Type *type);
INLINE bool type_is_any_arraylike(Type *type);
INLINE bool type_is_promotable_float(Type *type);
INLINE bool type_is_promotable_int_bool(Type *type);
INLINE bool type_is_signed(Type *type);
INLINE bool type_ok(Type *type);
INLINE bool type_is_unsigned(Type *type);
INLINE bool type_is_union_or_strukt(Type *type);
INLINE bool type_flat_is_vector(Type *type);
INLINE bool type_flat_is_vector_bitstruct(Type *type);
INLINE AlignSize type_min_alignment(AlignSize a, AlignSize b);
INLINE AlignSize type_max_alignment(AlignSize a, AlignSize b);
INLINE BitSize type_bit_size(Type *type);
INLINE Type *type_vector_type(Type *type);

static inline CanonicalType *type_pointer_type(Type *type);
static inline FlatType *type_flatten(Type *type);
static inline Type *type_base(Type *type);

INLINE TypeInfo *type_info_new(TypeInfoKind kind, SourceSpan span);
INLINE TypeInfo *type_info_new_base(Type *type, SourceSpan span);
INLINE bool type_info_ok(TypeInfo *type_info);
INLINE bool type_info_poison(TypeInfo *type);
INLINE bool type_is_user_defined(Type *type);

int type_kind_bitsize(TypeKind kind);
INLINE bool type_kind_is_signed(TypeKind kind);
INLINE bool type_kind_is_unsigned(TypeKind kind);
INLINE bool type_kind_is_any_integer(TypeKind kind);
INLINE bool type_kind_is_real_vector(TypeKind kind);

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
	if (type->type_kind == TYPE_OPTIONAL) return type->optional;
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
 if (k_ == TYPE_ALIAS) k_ = (t_)->canonical->type_kind;


INLINE Type *type_add_optional(Type *type, bool make_optional)
{
	if (!make_optional || type->type_kind == TYPE_OPTIONAL) return type;
	return type_get_optional(type);
}

INLINE Type *type_from_inferred(Type *flattened, Type *element_type, unsigned count)
{
	switch (flattened->type_kind)
	{
		case TYPE_POINTER:
			ASSERT(count == 0);
			return type_get_ptr(element_type);
		case TYPE_SIMD_VECTOR:
			ASSERT(flattened->array.len == count);
			return type_get_vector(element_type, TYPE_SIMD_VECTOR, count);
		case TYPE_VECTOR:
			ASSERT(flattened->array.len == count);
			FALLTHROUGH;
		case TYPE_INFERRED_VECTOR:
			return type_get_vector(element_type, TYPE_VECTOR, count);
		case TYPE_ARRAY:
			ASSERT(flattened->array.len == count);
			FALLTHROUGH;
		case TYPE_INFERRED_ARRAY:
			return type_get_array(element_type, count);
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
			case TYPE_ALIAS:
				type = type->canonical;
				continue;
			case TYPE_OPTIONAL:
				type = type->optional;
				continue;
			case TYPE_ARRAY:
			case TYPE_SLICE:
			case VECTORS:
				type = type->array.base;
				continue;
			case TYPE_INFERRED_ARRAY:
			case TYPE_INFERRED_VECTOR:
				return true;
			case TYPE_POINTER:
				type = type->pointer;
				if (type->canonical->type_kind == TYPE_FLEXIBLE_ARRAY) return false;
				continue;
			case TYPE_FLEXIBLE_ARRAY:
			default:
				return false;
		}
		UNREACHABLE;
	}
}

INLINE bool type_is_wildcard(Type *type)
{
	return type == type_wildcard || type == type_wildcard_optional;
}

INLINE bool type_is_any_raw(Type *type)
{
	switch (type->type_kind)
	{
		case TYPE_ANY:
		case TYPE_INTERFACE:
			return true;
		default:
			return false;
	}
}


INLINE bool type_is_any(Type *type)
{
	return type->canonical == type_any;
}



INLINE bool type_is_optional(Type *type)
{
	if (!type) return false;
	DECL_TYPE_KIND_REAL(kind, type);
	return kind == TYPE_OPTIONAL;
}

INLINE bool type_may_implement_interface(Type *type)
{
	DECL_TYPE_KIND_REAL(kind, type);
	switch (kind)
	{
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_ENUM:
		case TYPE_CONST_ENUM:
		case TYPE_TYPEDEF:
		case TYPE_BITSTRUCT:
			return true;
		default:
			return false;
	}
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
	type->kind = TYPE_INFO_POISON;
	type->type = poisoned_type;
	type->resolve_status = RESOLVE_DONE;
	return false;
}

INLINE bool type_is_arraylike(Type *type)
{
	DECL_TYPE_KIND_REAL(kind, type);
	return kind == TYPE_ARRAY || kind == TYPE_VECTOR || kind == TYPE_FLEXIBLE_ARRAY || kind == TYPE_SIMD_VECTOR;
}

INLINE bool type_is_any_arraylike(Type *type)
{
	DECL_TYPE_KIND_REAL(kind, type);
	return kind >= TYPE_FIRST_ARRAYLIKE && kind <= TYPE_LAST_ARRAYLIKE;
}

INLINE CanonicalType *type_pointer_type(Type *type)
{
	CanonicalType *res = type->canonical;
	if (res->type_kind != TYPE_POINTER) return NULL;
	return res->pointer;
}

static inline Type *type_flat_distinct_inline(Type *type);

static inline bool type_is_pointer_like(Type *type)
{
	TypeKind kind = type->type_kind;
	if (kind == TYPE_TYPEDEF)
	{
		type = type_flat_distinct_inline(type);
		kind = type->type_kind;
	}
	switch (kind)
	{
		case TYPE_POINTER:
			return true;
		case VECTORS:
			return type_is_pointer_like(type->array.base->canonical);
		default:
			return false;
	}
}

INLINE bool type_is_pointer_vector(Type *type)
{
	return type_kind_is_real_vector(type->type_kind) && type->array.base->canonical->type_kind == TYPE_POINTER;
}

INLINE bool type_is_atomic(Type *type_flat)
{
	switch (type_flat->type_kind)
	{
		case ALL_UNSIGNED_INTS:
		case ALL_SIGNED_INTS:
		case ALL_FLOATS:
		case TYPE_ENUM:
		case TYPE_CONST_ENUM:
		case TYPE_ANYFAULT:
		case TYPE_TYPEID:
		case TYPE_BOOL:
			break;
		default:
			return false;
	}
	return type_size(type_flat) <= type_size(type_iptr);
}

INLINE bool type_is_pointer(Type *type)
{
	DECL_TYPE_KIND_REAL(kind, type);
	return kind == TYPE_POINTER;
}

INLINE bool type_is_pointer_type(Type *type)
{
	DECL_TYPE_KIND_REAL(kind, type);
	return kind == TYPE_POINTER || kind == TYPE_FUNC_PTR;
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
		case VECTORS:
			type = type->array.base;
			goto RETRY;
		case ALL_FLOATS:
		case ALL_INTS:
			return true;
		case TYPE_TYPEDEF:
			type = type->decl->distinct->type;
			goto RETRY;
		case TYPE_ALIAS:
			type = type->canonical;
			goto RETRY;
		case TYPE_OPTIONAL:
			type = type->optional;
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

INLINE bool type_is_16bit_float(Type *type)
{
	DECL_TYPE_KIND_REAL(kind, type);
	return kind == TYPE_BF16 || kind == TYPE_F16;
}

INLINE bool type_is_floatlike(Type *type)
{
	type = type->canonical;
	TypeKind kind = type->type_kind;
	if (type_kind_is_real_vector(kind) && type_is_float(type->array.base)) return true;
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
			return "a typeinfo";
		case TYPE_WILDCARD:
			return "an empty value";
		default:
			UNREACHABLE;
	}
}

INLINE Type *enum_inner_type(Type *enum_type)
{
	assert(enum_type->type_kind == TYPE_ENUM || enum_type->type_kind == TYPE_CONST_ENUM);
	return enum_type->decl->enums.type_info->type;
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


INLINE TypeInfoId type_info_id_new_base(Type *type, SourceSpan span)
{
	return type_infoid(type_info_new_base(type, span));
}


INLINE Type *type_new(TypeKind kind, const char *name)
{
	Type *type = CALLOCS(Type);
	type->type_kind = kind;
	type->size = ~(ByteSize)0;
	ASSERT(name);
	type->name = name;
	global_context_add_type(type);
	return type;
}


INLINE bool type_convert_will_trunc(Type *destination, Type *source)
{
	ASSERT(type_flat_is_vector(destination) || type_is_builtin(destination->canonical->type_kind));
	ASSERT(type_flat_is_vector(destination) || type_is_builtin(source->canonical->type_kind));
	return type_size(destination) < type_size(source);
}



// Useful sanity check function.
INLINE void advance_and_verify(ParseContext *context, TokenType token_type)
{
	ASSERT_SPAN(context, context->tok == token_type);
	advance(context);
}


INLINE Type *type_flatten_for_bitstruct(Type *type)
{
	type = type->canonical;
	RETRY:
	while (type->type_kind == TYPE_TYPEDEF)
	{
		type = type->decl->distinct->type;
	}
	if (type->type_kind == TYPE_ENUM || type->type_kind == TYPE_CONST_ENUM)
	{
		type = enum_inner_type(type)->canonical;
		goto RETRY;
	}
	return type;
}

static inline void methods_add(Methods *methods, Decl *method)
{
	vec_add(methods->methods, method);
	OperatorOverload operator = method->func_decl.operator;
	if (operator)
	{
		unsigned len = vec_size(method->func_decl.signature.params);
		if (operator == OVERLOAD_MINUS && len == 1)
		{
			method->func_decl.operator = operator = OVERLOAD_UNARY_MINUS;
		}

		if (len > 1 && !method->func_decl.signature.params[1]->var.type_info)
		{
			method->func_decl.is_wildcard_overload = true;
		}
		DeclId *decl = &methods->overloads[operator];
		if (!*decl)
		{
			*decl = declid(method);
		}
		else
		{
			Decl *current = declptr(*decl);
			if (current->decl_kind != DECL_DECLARRAY)
			{
				Decl *decl_array = decl_new(DECL_DECLARRAY, NULL,  INVALID_SPAN);
				vec_add(decl_array->decls, declptr(*decl));
				vec_add(decl_array->decls, method);
				*decl = declid(decl_array);
			}
			else
			{
				vec_add(current->decls, method);
			}
		}
	}
	decltable_set(&methods->method_table, method);
}

static inline Type *type_base(Type *type)
{
	while (1)
	{
		type = type->canonical;
		switch (type->type_kind)
		{
			case TYPE_TYPEDEF:
				type = type->decl->distinct->type;
				break;
			case TYPE_ENUM:
			case TYPE_CONST_ENUM:
				type = enum_inner_type(type);
				break;
			case TYPE_OPTIONAL:
				type = type->optional;
				break;
			case TYPE_ALIAS:
				UNREACHABLE
			default:
				return type;
		}
	}
}


static const bool is_distinct_like[TYPE_LAST + 1] = {
	[TYPE_ENUM] = true,
	[TYPE_CONST_ENUM] = true,
	[TYPE_TYPEDEF] = true
};

INLINE bool typekind_is_distinct_like(TypeKind kind)
{
	return is_distinct_like[kind];
}

INLINE bool type_is_distinct_like(Type *type)
{
	return is_distinct_like[type->type_kind];
}

static bool type_has_inline(Type *type)
{
	return is_distinct_like[type->type_kind] && type->decl->is_substruct;
}

static inline Type *type_inline(Type *type)
{
	assert(type_is_distinct_like(type));
	return type->type_kind == TYPE_TYPEDEF ? type->decl->distinct->type : type->decl->enums.type_info->type;
}


static inline Type *type_flat_distinct_inline(Type *type)
{
	while (1)
	{
		type = type->canonical;
		if (!type_has_inline(type)) return type;
		type = type_inline(type);
	}
}

static inline CanonicalType *type_vector_base(CanonicalType *type)
{
	return type_kind_is_real_vector(type->type_kind) ? type->array.base->canonical : type;
}

static inline Type *type_flatten_and_inline(Type *type)
{
	while (1)
	{
		type = type->canonical;
		Decl *decl;
		switch (type->type_kind)
		{
			case TYPE_OPTIONAL:
				type = type->optional;
				continue;
			case TYPE_TYPEDEF:
				type = type->decl->distinct->type;
				continue;
			case TYPE_CONST_ENUM:
				type = type->decl->enums.type_info->type;
				continue;
			case TYPE_ENUM:
				decl = type->decl;
				if (!decl->is_substruct) return type;
				if (!compiler.build.old_enums || decl->enums.inline_value)
				{
					type = decl->enums.type_info->type;
					continue;
				}
				type = decl->enums.parameters[decl->enums.inline_index]->type;
				continue;
			default:
				return type;
		}
	}
}

static inline Type *type_flat_distinct_enum_inline(Type *type)
{
	while (1)
	{
		type = type->canonical;
		Decl *decl;
		switch (type->type_kind)
		{
			case TYPE_TYPEDEF:
				decl = type->decl;
				if (!decl->is_substruct) return type;;
				type = decl->distinct->type;
				continue;
			case TYPE_CONST_ENUM:
				decl = type->decl;
				if (!decl->is_substruct) return type;
				type = decl->enums.type_info->type;
				continue;
			case TYPE_ENUM:
				decl = type->decl;
				if (!decl->is_substruct) return type;
				if (!compiler.build.old_enums || decl->enums.inline_value)
				{
					type = decl->enums.type_info->type;
					continue;
				}
				type = decl->enums.parameters[decl->enums.inline_index]->type;
				continue;
			default:
				return type;
		}
	}
}

INLINE bool type_is_user_defined(Type *type)
{
	static const bool user_defined_types[TYPE_LAST + 1] = {
		[TYPE_ENUM]       = true,
		[TYPE_CONST_ENUM] = true,
		[TYPE_STRUCT]     = true,
		[TYPE_FUNC_RAW]   = true,
		[TYPE_UNION]      = true,
		[TYPE_TYPEDEF]    = true,
		[TYPE_BITSTRUCT]  = true,
		[TYPE_ALIAS]      = true,
		[TYPE_INTERFACE]  = true,
	};
	return user_defined_types[type->type_kind];
}


static inline Module *type_find_generic(Type *type)
{
	Type *canonical = type->canonical;
	if (canonical != type && type_is_user_defined(canonical))
	{
		Module *module = canonical->decl->unit->module;
		if (module->generic_module) return module;
	}
	if (!type_is_user_defined(type)) return NULL;
	Module *module = type->decl->unit->module;
	if (module->generic_module) return module;
	return module->generic_module ? module : NULL;
}
static inline Type *type_flatten_to_int(Type *type)
{
	while (1)
	{
		type = type->canonical;
		switch (type->type_kind)
		{
			case TYPE_TYPEDEF:
				type = type->decl->distinct->type;
				break;
			case TYPE_OPTIONAL:
				type = type->optional;
				break;
			case TYPE_BITSTRUCT:
				type = type->decl->strukt.container_type->type;
				break;
			case TYPE_CONST_ENUM:
				type = type->decl->enums.type_info->type;
				break;
			case TYPE_ENUM:
				return type;
			case VECTORS:
				ASSERT(type_is_integer(type->array.base));
				return type;
			case TYPE_ALIAS:
				UNREACHABLE
			default:
				ASSERT(type_is_integer(type));
				return type;
		}
	}
}

static inline CanonicalType *type_distinct_inline(Type *type)
{
	while (1)
	{
		type = type->canonical;
		switch (type->type_kind)
		{
			case TYPE_ENUM:
				if (!type->decl->is_substruct) return type;
				FALLTHROUGH;
			case TYPE_CONST_ENUM:
				type = enum_inner_type(type);
				break;
			case TYPE_TYPEDEF:
				type = type->decl->distinct->type;
				break;
			case TYPE_OPTIONAL:
				type = type->optional;
				break;
			case TYPE_ALIAS:
				UNREACHABLE
			default:
				return type;
		}
	}
}
static inline FlatType *type_flatten(Type *type)
{
	while (1)
	{
		type = type->canonical;
		switch (type->type_kind)
		{
			case TYPE_CONST_ENUM:
				type = enum_inner_type(type);
				break;
			case TYPE_TYPEDEF:
				type = type->decl->distinct->type;
				break;
			case TYPE_OPTIONAL:
				type = type->optional;
				break;
			case TYPE_ALIAS:
				UNREACHABLE
			default:
				return type;
		}
	}
}

static inline Type *type_flatten_no_export(Type *type)
{
	while (1)
	{
		switch (type->type_kind)
		{
			case TYPE_ALIAS:
				if (type->decl->is_export) return type;
				type = type->canonical;
				break;
			case TYPE_TYPEDEF:
				if (type->decl->is_export) return type;
				type = type->decl->distinct->type;
				break;
			case TYPE_CONST_ENUM:
				if (type->decl->is_export) return type;
				type = enum_inner_type(type);
				break;
			case TYPE_OPTIONAL:
				type = type->optional;
				break;
			default:
				return type;
		}
	}
}

static inline bool type_flat_is_valid_for_arg_h(Type *type)
{
	type = type_flatten(type);
	if (type->type_kind == TYPE_POINTER) return true;
	if (type->type_kind != TYPE_ARRAY && type->type_kind != TYPE_SLICE) return false;
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
	return type_kind_is_real_vector(flatten->type_kind) ? flatten->array.base : NULL;
}

INLINE bool type_is_builtin(TypeKind kind) { return kind >= TYPE_VOID && kind <= TYPE_TYPEID; }
INLINE bool type_kind_is_signed(TypeKind kind) { return kind >= TYPE_I8 && kind < TYPE_U8; }
INLINE bool type_kind_is_unsigned(TypeKind kind) { return kind >= TYPE_U8 && kind <= TYPE_U128; }
INLINE bool type_kind_is_any_integer(TypeKind kind) { return kind >= TYPE_I8 && kind <= TYPE_U128; }
INLINE bool type_kind_is_enum_or_fault(TypeKind kind) { return kind == TYPE_ENUM || kind == TYPE_ANYFAULT; }
INLINE bool type_is_unsigned(Type *type) { return type->type_kind >= TYPE_U8 && type->type_kind <= TYPE_U128; }
INLINE bool type_ok(Type *type) { return !type || type->type_kind != TYPE_POISONED; }
INLINE bool type_info_ok(TypeInfo *type_info) { return !type_info || type_info->kind != TYPE_INFO_POISON; }
bool type_is_scalar(Type *type);

INLINE bool type_is_signed(Type *type)
{
	TypeKind kind = type->type_kind;
	if (kind >= TYPE_I8 && kind < TYPE_U8) return true;
	if (!type_kind_is_real_vector(kind)) return false;
	kind = type->array.base->type_kind;
	return kind >= TYPE_I8 && kind < TYPE_U8;
}

INLINE bool type_is_func_ptr(Type *fn_type)
{
	return fn_type->canonical->type_kind == TYPE_FUNC_PTR;
}

INLINE bool type_is_infer_type(Type *type)
{
	TypeKind kind = type->type_kind;
	return kind == TYPE_INFERRED_VECTOR || kind == TYPE_INFERRED_ARRAY;
}

INLINE bool type_is_inferred(Type *type)
{
RETRY:;
	TypeKind kind = type->type_kind;
	switch (kind)
	{
		case TYPE_INFERRED_ARRAY:
		case TYPE_INFERRED_VECTOR:
			return true;
		case TYPE_ARRAY:
			type = type->array.base->canonical;
			goto RETRY;
		case TYPE_POINTER:
			type = type->pointer->canonical;
			goto RETRY;
		default:
			return false;
	}
}

INLINE bool type_is_number_or_bool(Type *type)
{
	DECL_TYPE_KIND_REAL(kind, type);
	return (kind >= TYPE_BOOL) && (kind <= TYPE_FLOAT_LAST);
}

INLINE bool type_is_number(Type *type)
{
	DECL_TYPE_KIND_REAL(kind, type);
	return (kind >= TYPE_I8) && (kind <= TYPE_FLOAT_LAST);
}


static inline Type *type_flat_for_arithmethics(Type *type)
{
	while (true)
	{
		type = type->canonical;
		Type *inner;
		switch (type->type_kind)
		{
			case TYPE_OPTIONAL:
				type = type->optional;
				continue;
			case TYPE_CONST_ENUM:
			case TYPE_TYPEDEF:
				inner = type_inline(type);
				if (type->decl->is_substruct)
				{
					type = inner;
					continue;
				}
				break;
			default:
				return type;
		}
		inner = type_flat_for_arithmethics(inner);
		if (type_is_number_or_bool(inner)) return inner;
		return type;
	}
}

INLINE bool type_is_numeric(Type *type)
{
	RETRY:;
	DECL_TYPE_KIND_REAL(kind, type);
	if ((kind >= TYPE_I8) & (kind <= TYPE_FLOAT_LAST)) return true;
	if (type_kind_is_real_vector(type->type_kind))
	{
		type = type->array.base;
		goto RETRY;
	}
	return false;
}

INLINE bool type_underlying_is_numeric(Type *type)
{
	return type_is_numeric(type_flatten(type));
}

INLINE bool type_underlying_may_add_sub(CanonicalType *type)
{
	return type->type_kind == TYPE_ENUM || type->type_kind == TYPE_POINTER || type_is_numeric(type);
}

INLINE bool type_is_vec(FlatType *type)
{
	ASSERT(type_flatten(type) == type);
	TypeKind kind = type->type_kind;
	return kind == TYPE_VECTOR || kind == TYPE_SIMD_VECTOR;
}

INLINE bool type_kind_is_real_vector(TypeKind kind)
{
	return kind == TYPE_VECTOR || kind == TYPE_SIMD_VECTOR;
}

INLINE bool type_flat_is_vector(Type *type)
{
	return type_kind_is_real_vector(type_flatten(type)->type_kind);
}

INLINE bool type_flat_is_vector_bitstruct(Type *type)
{
	TypeKind kind = type_flatten(type)->type_kind;
	return kind == TYPE_VECTOR || kind == TYPE_BITSTRUCT || kind == TYPE_SIMD_VECTOR;
}

INLINE bool type_kind_is_any_non_simd_vector(TypeKind kind)
{
	return kind == TYPE_VECTOR || kind == TYPE_INFERRED_VECTOR;
}

INLINE bool type_kind_is_any_vector(TypeKind kind)
{
	return kind == TYPE_VECTOR || kind == TYPE_INFERRED_VECTOR || kind == TYPE_SIMD_VECTOR;
}

INLINE bool type_flat_is_bool_vector(Type *type)
{
	Type *flat = type_flatten(type);
	return type_kind_is_real_vector(flat->type_kind) && type_flatten(flat->array.base) == type_bool;
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
	while (decl->decl_kind == DECL_ALIAS)
	{
		decl = decl->define_decl.alias;
	}
	if (decl->decl_kind != DECL_VAR || decl->var.kind != VARDECL_UNWRAPPED) return decl;
	decl = decl->var.alias;
	ASSERT(decl->decl_kind != DECL_VAR || decl->var.kind != VARDECL_UNWRAPPED);
	return decl;
}

INLINE bool decl_is_struct_type(Decl *decl)
{
	DeclKind kind = decl->decl_kind;
	return (kind == DECL_UNION) | (kind == DECL_STRUCT);
}

INLINE bool decl_is_user_defined_type(Decl *decl)
{
	DeclKind kind = decl->decl_kind;
	return (kind == DECL_UNION) | (kind == DECL_STRUCT) | (kind == DECL_BITSTRUCT)
			| (kind == DECL_ENUM) | (kind == DECL_TYPE_ALIAS) | (kind == DECL_TYPEDEF)
			| (kind == DECL_INTERFACE)
			;
}

INLINE Decl *decl_flatten(Decl *decl)
{
	if (decl->decl_kind == DECL_ALIAS)
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

INLINE bool expr_is_addr(Expr *expr)
{
	return expr->expr_kind == EXPR_UNARY && expr->unary_expr.operator == UNARYOP_ADDR;
}

INLINE bool expr_is_any_addr(Expr *expr)
{
	if (expr->expr_kind != EXPR_UNARY) return false;
	switch (expr->unary_expr.operator)
	{
		case UNARYOP_ADDR:
		case UNARYOP_TADDR:
			return true;
		default:
			return false;
	}
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

INLINE bool exprid_is_runtime_const(ExprId expr)
{
	return expr ? expr_is_runtime_const(exprptr(expr)) : true;
}

INLINE bool expr_poison(Expr *expr) { expr->expr_kind = EXPR_POISONED; expr->resolve_status = RESOLVE_DONE; return false; }

static inline void expr_list_set_span(Expr **expr, SourceSpan loc);
static inline void exprid_set_span(ExprId expr_id, SourceSpan loc);
static inline void expr_set_span(Expr *expr, SourceSpan loc);

bool const_init_local_init_may_be_global(ConstInitializer *init);
ConstInitializer *const_init_new_zero(Type *type);
ConstInitializer *const_init_new_value(Expr *value);
ConstInitializer *const_init_new_array(Type *type, ConstInitializer **elements) UNUSED;
ConstInitializer *const_init_new_struct(Type *type, Expr **elements);
ConstInitializer *const_init_new_array_full(Type *type, ConstInitializer **elements);
ConstInitializer *const_init_new_zero_array_value(Type *type, ArrayIndex index);
ConstInitializer *const_init_new_array_value(Expr *expr, ArrayIndex index);
bool const_init_is_zero(ConstInitializer *init);
void const_init_rewrite_to_value(ConstInitializer *const_init, Expr *value);
void const_init_rewrite_array_at(ConstInitializer *const_init, Expr *value, ArrayIndex index);
void const_init_rewrite_to_zero(ConstInitializer *init, Type *type);

static inline void const_init_set_span(ConstInitializer *init, SourceSpan loc)
{
	RETRY:
	switch (init->kind)
	{
		case CONST_INIT_ZERO:
			return;
		case CONST_INIT_STRUCT:
		{
			FOREACH(ConstInitializer *, init_val, init->init_struct)
			{
				const_init_set_span(init_val, loc);
			}
			return;
		}
		case CONST_INIT_UNION:
			init = init->init_union.element;
			goto RETRY;
		case CONST_INIT_VALUE:
			expr_set_span(init->init_value, loc);
			return;
		case CONST_INIT_ARRAY:
		{
			FOREACH(ConstInitializer *, init2, init->init_array.elements) const_init_set_span(init2, loc);
			return;
		}
		case CONST_INIT_ARRAY_FULL:
		{
			FOREACH(ConstInitializer *, init2, init->init_array_full) const_init_set_span(init2, loc);
			return;
		}
		case CONST_INIT_ARRAY_VALUE:
			const_init_set_span(init->init_array_value.element, loc);
			return;
	}
	UNREACHABLE_VOID
}

static inline void expr_list_set_span(Expr **expr, SourceSpan loc);
static inline void exprid_set_span(ExprId expr_id, SourceSpan loc);

static inline void expr_set_span(Expr *expr, SourceSpan loc)
{
	if (!expr) return;
	expr->span = loc;
	switch (expr->expr_kind)
	{
		case EXPR_TWO:
			expr_set_span(expr->two_expr.first, loc);
			expr_set_span(expr->two_expr.last, loc);
			return;
		case EXPR_INT_TO_BOOL:
			expr_set_span(expr->int_to_bool_expr.inner, loc);
			return;
		case EXPR_EXT_TRUNC:
			expr_set_span(expr->ext_trunc_expr.inner, loc);
			return;
		case EXPR_NAMED_ARGUMENT:
			expr->named_argument_expr.name_span = loc;
			expr_set_span(expr->named_argument_expr.value, loc);
			return;
		case EXPR_CONST:
			switch (expr->const_expr.const_kind)
			{
				case CONST_INITIALIZER:
					const_init_set_span(expr->const_expr.initializer, loc);
					return;
				case CONST_UNTYPED_LIST:
					expr_list_set_span(expr->const_expr.untyped_list, loc);
					return;
				default:
					return;
			}
		case EXPR_CAST:
			exprid_set_span(expr->cast_expr.expr, loc);
			return;
		case EXPR_INITIALIZER_LIST:
			expr_list_set_span(expr->initializer_list, loc);
			return;
		case EXPR_DESIGNATED_INITIALIZER_LIST:
			expr_set_span(expr->designated_init.splat, loc);
			expr_list_set_span(expr->designated_init.list, loc);
			return;
		case EXPR_MAKE_ANY:
			expr_set_span(expr->make_any_expr.inner, loc);
			expr_set_span(expr->make_any_expr.typeid, loc);
			return;
		case EXPR_MAKE_SLICE:
			if (expr->make_slice_expr.ptr) expr_set_span(expr->make_slice_expr.ptr, loc);
			return;
		case EXPR_SPLAT:
		case EXPR_PTR_ACCESS:
		case EXPR_VECTOR_TO_ARRAY:
		case EXPR_SLICE_TO_VEC_ARRAY:
		case EXPR_SCALAR_TO_VECTOR:
		case EXPR_ENUM_FROM_ORD:
		case EXPR_INT_TO_FLOAT:
		case EXPR_INT_TO_PTR:
		case EXPR_PTR_TO_INT:
		case EXPR_FLOAT_TO_INT:
		case EXPR_SLICE_LEN:
		case EXPR_DISCARD:
		case EXPR_VECTOR_FROM_ARRAY:
		case EXPR_ADDR_CONVERSION:
		case EXPR_RECAST:
		case EXPR_LENGTHOF:
		case EXPR_MAYBE_DEREF:
			expr_set_span(expr->inner_expr, loc);
			return;
		case EXPR_EXPRESSION_LIST:
		case EXPR_ACCESS_RESOLVED:
		case EXPR_ACCESS_UNRESOLVED:
		case EXPR_BITACCESS:
		case EXPR_POISONED:
		case EXPR_ASM:
		case EXPR_BUILTIN:
		case EXPR_BINARY:
		case EXPR_BITASSIGN:
		case EXPR_BUILTIN_ACCESS:
		case EXPR_CALL:
		case EXPR_CATCH_UNRESOLVED:
		case EXPR_CATCH:
		case EXPR_COMPILER_CONST:
		case EXPR_COMPOUND_LITERAL:
		case EXPR_COND:
		case EXPR_CT_ARG:
		case EXPR_CT_CALL:
		case EXPR_CT_ASSIGNABLE:
		case EXPR_CT_IS_CONST:
		case EXPR_CT_DEFINED:
		case EXPR_CT_EVAL:
		case EXPR_CT_IDENT:
		case EXPR_DECL:
		case EXPR_DESIGNATOR:
		case EXPR_EMBED:
		case EXPR_OPTIONAL:
		case EXPR_FORCE_UNWRAP:
		case EXPR_GENERIC_IDENT:
		case EXPR_HASH_IDENT:
		case EXPR_IDENTIFIER:
		case EXPR_LAMBDA:
		case EXPR_LAST_FAULT:
		case EXPR_MACRO_BLOCK:
		case EXPR_MACRO_BODY_EXPANSION:
		case EXPR_NOP:
		case EXPR_OPERATOR_CHARS:
		case EXPR_OTHER_CONTEXT:
		case EXPR_POINTER_OFFSET:
		case EXPR_POST_UNARY:
		case EXPR_RETHROW:
		case EXPR_RETVAL:
		case EXPR_SLICE:
		case EXPR_SLICE_ASSIGN:
		case EXPR_SLICE_COPY:
		case EXPR_STRINGIFY:
		case EXPR_SUBSCRIPT:
		case EXPR_SWIZZLE:
		case EXPR_SUBSCRIPT_ADDR:
		case EXPR_SUBSCRIPT_ASSIGN:
		case EXPR_TERNARY:
		case EXPR_BENCHMARK_HOOK:
		case EXPR_TEST_HOOK:
		case EXPR_TRY:
		case EXPR_TRY_UNRESOLVED:
		case EXPR_TRY_UNWRAP_CHAIN:
		case EXPR_TYPEID:
		case EXPR_TYPEID_INFO:
		case EXPR_TYPEINFO:
		case EXPR_UNARY:
		case EXPR_UNRESOLVED_IDENTIFIER:
		case EXPR_VASPLAT:
		case EXPR_MACRO_BODY:
		case EXPR_DEFAULT_ARG:
		case EXPR_TYPECALL:
		case EXPR_MEMBER_GET:
		case EXPR_MEMBER_SET:
		case EXPR_RVALUE:
		case EXPR_CT_SUBSCRIPT:
		case EXPR_IOTA_DECL:
			break;
	}
}
static inline void exprid_set_span(ExprId expr_id, SourceSpan loc)
{
	if (expr_id) expr_set_span(exprptr(expr_id), loc);
}
static inline void expr_list_set_span(Expr **exprs, SourceSpan loc)
{
	FOREACH(Expr *, expr, exprs) expr_set_span(expr, loc);
}
INLINE void expr_replace(Expr *expr, Expr *replacement)
{
	SourceSpan loc = expr->span;
	*expr = *replacement;
	expr_set_span(expr, loc);
}

INLINE bool expr_ok(Expr *expr) { return expr == NULL || expr->expr_kind != EXPR_POISONED; }

INLINE bool exprid_is_simple(ExprId expr_id, bool to_float) { return expr_is_simple(exprptr(expr_id), to_float); }

INLINE void expr_resolve_ident(Expr *expr, Decl *decl)
{
	expr->expr_kind = EXPR_IDENTIFIER;
	expr->ident_expr = decl;
	expr->type = decl->type ? decl->type : type_void;
	expr->resolve_status = RESOLVE_DONE;
}

INLINE Type *exprtype(ExprId expr_id)
{
	ASSERT(expr_id);
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

INLINE bool type_is_promotable_int_bool(Type *type)
{
	// If we support other architectures, update this.
	return type_is_integer_or_bool_kind(type) && type->builtin.bitsize < compiler.platform.width_c_int;
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
const char *cc_compiler(const char *cc, const char *file, const char *flags, const char **include_dirs, const char *output_subdir);
const char *arch_to_linker_arch(ArchType arch);
extern char swizzle[256];
#define SWIZZLE_INDEX(c) ((swizzle[(int)(c)] - 1) & 0xF)
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
#define ASSIGN_DECLID_OR_RET(_assign, _decl_stmt, _res) Decl* TEMP(_decl) = (_decl_stmt); if (!decl_ok(TEMP(_decl))) return _res; _assign = TEMP(_decl) ? declid(TEMP(_decl)) : 0



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


INLINE Ast *ast_next(AstId *current_ptr)
{
	Ast *ast = astptr(*current_ptr);
	*current_ptr = ast->next;
	return ast;
}


INLINE void expr_rewrite_recast(Expr *expr, Type *type)
{
	if (expr->expr_kind == EXPR_RECAST || expr->expr_kind == EXPR_ADDR_CONVERSION || expr->expr_kind == EXPR_RVALUE)
	{
		expr->type = type;
		return;
	}

	Expr *inner = expr_copy(expr);
	expr->expr_kind = EXPR_RECAST;
	expr->inner_expr = inner;
	expr->type = type;
}


INLINE void expr_rewrite_rvalue(Expr *expr, Type *type)
{
	switch (expr->expr_kind)
	{
		case EXPR_RECAST:
			expr->expr_kind = EXPR_RVALUE;
			expr->type = type;
			return;
		case EXPR_RVALUE:
			expr->type = type;
			return;
		default:
			break;
	}
	Expr *inner = expr_copy(expr);
	expr->expr_kind = EXPR_RVALUE;
	expr->inner_expr = inner;
	expr->type = type;
}

INLINE void expr_rewrite_addr_conversion(Expr *expr, Type *type)
{
	switch (expr->expr_kind)
	{
		case EXPR_RECAST:
			expr->expr_kind = EXPR_ADDR_CONVERSION;
			expr->type = type;
			return;
		case EXPR_ADDR_CONVERSION:
			expr->type = type;
			return;
		default:
			break;
	}
	Expr *inner = expr_copy(expr);
	expr->expr_kind = EXPR_ADDR_CONVERSION;
	expr->inner_expr = inner;
	expr->type = type;
}

INLINE void expr_rewrite_discard(Expr *expr)
{
	Expr *inner = expr_copy(expr);
	expr->expr_kind = EXPR_DISCARD;
	expr->inner_expr = inner;
	expr->type = type_void;
}

INLINE void expr_rewrite_const_bool(Expr *expr, Type *type, bool b)
{
	expr->expr_kind = EXPR_CONST;
	expr->type = type;
	expr->const_expr = (ExprConst) { .b = b, .const_kind = CONST_BOOL };
	expr->resolve_status = RESOLVE_DONE;
}

INLINE void expr_rewrite_const_fault(Expr *expr, Decl *fault)
{
	expr->expr_kind = EXPR_CONST;
	expr->type = type_fault;
	expr->const_expr = (ExprConst) { .fault = fault, .const_kind = CONST_FAULT };
	expr->resolve_status = RESOLVE_DONE;
}

INLINE void expr_rewrite_const_null(Expr *expr, Type *type)
{
	expr->expr_kind = EXPR_CONST;
	expr->type = type;
	expr->const_expr = (ExprConst) { .ptr = 0, .const_kind = CONST_POINTER };
	expr->resolve_status = RESOLVE_DONE;
}

INLINE void expr_rewrite_const_empty_slice(Expr *expr, Type *type)
{
	ASSERT(type_flatten(type)->type_kind == TYPE_SLICE);
	expr->const_expr = (ExprConst) { .const_kind = CONST_SLICE, .initializer = NULL };
	expr->expr_kind = EXPR_CONST;
	expr->type = type;
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
	ASSERT(type_flatten(type)->type_kind != TYPE_SLICE);
	ASSERT(type != type_untypedlist);
	expr->expr_kind = EXPR_CONST;
	expr->type = type;
	expr->const_expr = (ExprConst) { .initializer = initializer, .const_kind = CONST_INITIALIZER };
	expr->resolve_status = RESOLVE_DONE;
}

INLINE void expr_rewrite_const_slice(Expr *expr, Type *type, ConstInitializer *initializer)
{
	ASSERT(type_flatten(type)->type_kind == TYPE_SLICE);
	ASSERT(type != type_untypedlist);
	expr->expr_kind = EXPR_CONST;
	expr->type = type;
	expr->const_expr = (ExprConst) { .slice_init = initializer, .const_kind = CONST_SLICE };
	expr->resolve_status = RESOLVE_DONE;
}

INLINE void expr_rewrite_const_typeid(Expr *expr, Type *type)
{
	ASSERT(type->type_kind != TYPE_UNTYPED_LIST);
	expr->expr_kind = EXPR_CONST;
	expr->const_expr.const_kind = CONST_TYPEID;
	expr->const_expr.typeid = type->canonical;
	expr->type = type_typeid;
	expr->resolve_status = RESOLVE_DONE;
}

INLINE void expr_rewrite_ptr_access(Expr *expr, Expr *inner, Type *type)
{
	ASSERT(inner->resolve_status == RESOLVE_DONE);
	expr->expr_kind = EXPR_PTR_ACCESS;
	expr->inner_expr = inner;
	expr->type = type_add_optional(type, IS_OPTIONAL(inner));
	expr->resolve_status = RESOLVE_DONE;
}

INLINE void expr_rewrite_enum_from_ord(Expr *expr, Type *type)
{
	Expr *inner = expr_copy(expr);
	ASSERT(inner->resolve_status == RESOLVE_DONE);
	expr->expr_kind = EXPR_ENUM_FROM_ORD;
	expr->inner_expr = inner;
	expr->type = type_add_optional(type, IS_OPTIONAL(expr));
	expr->resolve_status = RESOLVE_DONE;
}

INLINE void expr_rewrite_slice_len(Expr *expr, Expr *inner, Type *type)
{
	ASSERT(inner->resolve_status == RESOLVE_DONE);
	expr->expr_kind = EXPR_SLICE_LEN;
	expr->inner_expr = inner;
	expr->type = type_add_optional(type, IS_OPTIONAL(inner));
	expr->resolve_status = RESOLVE_DONE;
}

INLINE void expr_rewrite_int_to_bool(Expr *expr, bool negate)
{
	if (expr_is_const(expr))
	{
		bool is_zero = expr_is_zero(expr);
		expr_rewrite_const_bool(expr, type_bool, negate ? is_zero : !is_zero);
		return;
	}
	Expr *inner = expr_copy(expr);
	expr->expr_kind = EXPR_INT_TO_BOOL;
	expr->int_to_bool_expr = (ExprIntToBool) { .inner = inner, .negate = negate };
	expr->type = type_add_optional(type_bool, IS_OPTIONAL(expr));
}

INLINE void expr_rewrite_ext_trunc(Expr *expr, Type *type, bool is_signed)
{
	Expr *inner = expr_copy(expr);
	expr->expr_kind = EXPR_EXT_TRUNC;
	expr->ext_trunc_expr = (ExprExtTrunc) { .inner = inner, .is_signed = is_signed };
	expr->type = type_add_optional(type, IS_OPTIONAL(inner));
}

INLINE void expr_rewrite_const_integer(Expr *expr, Type *type, Int128 i)
{
	expr->expr_kind = EXPR_CONST;
	expr->type = type;
	expr->resolve_status = RESOLVE_DONE;
	expr->const_expr.ixx = (Int) {
		.i =  i,
		.type = type_flatten_to_int(type)->type_kind
	};
	expr->const_expr.is_character = false;
	expr->const_expr.const_kind = CONST_INTEGER;
}

INLINE void expr_rewrite_const_int(Expr *expr, Type *type, uint64_t v)
{
	expr->expr_kind = EXPR_CONST;
	expr->type = type;
	expr->resolve_status = RESOLVE_DONE;
	TypeKind kind = type_flatten(type)->type_kind;
	(&expr->const_expr)->ixx.i.high = 0;
	if (type_kind_is_signed(kind))
	{
		if (v > (uint64_t)INT64_MAX) (&expr->const_expr)->ixx.i.high = UINT64_MAX;
	}
	else
	{
		switch (kind)
		{
			case TYPE_U8:
				v &= 0xFF;
				break;
			case TYPE_U16:
				v &= 0xFFFF;
				break;
			case TYPE_U32:
				v &= 0xFFFFFFFF;
				break;
			default:
				break;
		}
	}
	(&expr->const_expr)->ixx.i.low = v;
	(&expr->const_expr)->ixx.type = kind;
	(&expr->const_expr)->is_character = false;
	(&expr->const_expr)->const_kind = CONST_INTEGER;
}

INLINE void expr_rewrite_to_int_to_float(Expr *expr, Type *type)
{
	Expr *inner = expr_copy(expr);
	expr->expr_kind = EXPR_INT_TO_FLOAT;
	expr->inner_expr = inner;
	expr->type = type_add_optional(type, IS_OPTIONAL(inner));
}

INLINE void expr_rewrite_to_int_to_ptr(Expr *expr, Type *type)
{
	Expr *inner = expr_copy(expr);
	expr->expr_kind = EXPR_INT_TO_PTR;
	expr->inner_expr = inner;
	expr->type = type_add_optional(type, IS_OPTIONAL(inner));
}

INLINE void expr_rewrite_to_ptr_to_int(Expr *expr, Type *type)
{
	Expr *inner = expr_copy(expr);
	expr->expr_kind = EXPR_PTR_TO_INT;
	expr->inner_expr = inner;
	expr->type = type_add_optional(type, IS_OPTIONAL(inner));
}

INLINE void expr_rewrite_to_float_to_int(Expr *expr, Type *type)
{
	Expr *inner = expr_copy(expr);
	expr->expr_kind = EXPR_FLOAT_TO_INT;
	expr->inner_expr = inner;
	expr->type = type_add_optional(type, IS_OPTIONAL(inner));
}

INLINE void expr_rewrite_const_float(Expr *expr, Type *type, Real d)
{
	expr->expr_kind = EXPR_CONST;
	expr->type = type;
	TypeKind kind = type_flatten(type)->type_kind;
	Real real;
	switch (kind)
	{
		case TYPE_F16:
		case TYPE_BF16:
		case TYPE_F32:
			real = (float)d;
			break;
		case TYPE_F64:
			real = (double)d;
			break;
		default:
			REMINDER("Handling of float type may not be accurate");
			real = d;
			break;
	}
	expr->const_expr = (ExprConst) {
			.fxx = (Float){ real, kind },
			.const_kind = CONST_FLOAT,
		};
	expr->resolve_status = RESOLVE_DONE;
}

INLINE const char *asm_clobber_by_index(unsigned index)
{
	return compiler.platform.clobber_name_list[index];
}

INLINE AsmRegister *asm_reg_by_index(unsigned index)
{
	return &compiler.platform.registers[index];
}

INLINE void clobbers_add(Clobbers *clobbers, unsigned index)
{
	ASSERT(index < MAX_CLOBBER_FLAGS);
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
		ASSERT(i < MAX_CLOBBER_FLAGS);
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
	ASSERT(index < MAX_CLOBBER_FLAGS);
	unsigned bit = index % 64;
	unsigned element = index / 64;
	clobbers.mask[element] |= (1ull << bit);
	va_list list;
	va_start(list, index);
	int i;
	while ((i = va_arg(list, int)) > -1)
	{
		ASSERT(i < MAX_CLOBBER_FLAGS);
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
	if (limit >= 20 && (bits & ARG_BITS_20)) return 20;
	if (limit >= 16 && (bits & ARG_BITS_16)) return 16;
	if (limit >= 12 && (bits & ARG_BITS_12)) return 12;
	if (limit >= 8 && (bits & ARG_BITS_8)) return 8;
	if (limit >= 5 && (bits & ARG_BITS_5)) return 5;
	return 0;
}

INLINE bool expr_is_empty_const_slice(Expr *expr)
{
	ASSERT(expr->resolve_status == RESOLVE_DONE);
	return expr->expr_kind == EXPR_CONST
		&& expr->const_expr.const_kind == CONST_SLICE
		&& expr->const_expr.slice_init == NULL;
}

INLINE bool expr_is_const(Expr *expr)
{
	return expr->expr_kind == EXPR_CONST;
}

INLINE bool decl_var_kind_is_ct(VarDeclKind kind)
{
	return kind >= VARDECL_FIRST_CT && kind <= VARDECL_LAST_CT;
}


static inline bool decl_is_var_local(Decl *decl)
{
	if (decl->decl_kind != DECL_VAR) return false;
	VarDeclKind kind = decl->var.kind;
	return kind == VARDECL_PARAM_CT_TYPE
		   || kind == VARDECL_PARAM
		   || kind == VARDECL_PARAM_CT
		   || kind == VARDECL_LOCAL
		   || kind == VARDECL_LOCAL_CT_TYPE
		   || kind == VARDECL_LOCAL_CT
		   || kind == VARDECL_PARAM_EXPR
		   || kind == VARDECL_BITMEMBER
		   || kind == VARDECL_MEMBER;
}

INLINE bool expr_is_const_string(Expr *expr)
{
	ASSERT(expr->resolve_status == RESOLVE_DONE);
	return expr->expr_kind == EXPR_CONST && expr->const_expr.const_kind == CONST_STRING;
}

INLINE bool expr_is_const_enum(Expr *expr)
{
	ASSERT(expr->resolve_status == RESOLVE_DONE);
	return expr->expr_kind == EXPR_CONST && expr->const_expr.const_kind == CONST_ENUM;
}

INLINE bool expr_is_const_number(Expr *expr)
{
	ASSERT(expr->resolve_status == RESOLVE_DONE);
	return expr->expr_kind == EXPR_CONST && (expr->const_expr.const_kind == CONST_INTEGER || expr->const_expr.const_kind == CONST_FLOAT);
}

INLINE bool expr_is_const_fault(Expr *expr)
{
	ASSERT(expr->resolve_status == RESOLVE_DONE);
	return expr->expr_kind == EXPR_CONST && expr->const_expr.const_kind == CONST_FAULT;
}

INLINE bool expr_is_const_ref(Expr *expr)
{
	ASSERT(expr->resolve_status == RESOLVE_DONE);
	return expr->expr_kind == EXPR_CONST && expr->const_expr.const_kind == CONST_REF;
}

INLINE bool expr_is_const_pointer(Expr *expr)
{
	ASSERT(expr->resolve_status == RESOLVE_DONE);
	return expr->expr_kind == EXPR_CONST && expr->const_expr.const_kind == CONST_POINTER;
}

INLINE bool expr_is_const_bool(Expr *expr)
{
	ASSERT(expr->resolve_status == RESOLVE_DONE);
	return expr->expr_kind == EXPR_CONST && expr->const_expr.const_kind == CONST_BOOL;
}

INLINE bool expr_is_const_initializer(Expr *expr)
{
	ASSERT(expr->resolve_status == RESOLVE_DONE);
	return expr->expr_kind == EXPR_CONST && expr->const_expr.const_kind == CONST_INITIALIZER;
}

INLINE bool expr_is_const_slice(Expr *expr)
{
	ASSERT(expr->resolve_status == RESOLVE_DONE);
	return expr->expr_kind == EXPR_CONST && expr->const_expr.const_kind == CONST_SLICE;
}

INLINE bool expr_is_const_bytes(Expr *expr)
{
	ASSERT(expr->resolve_status == RESOLVE_DONE);
	return expr->expr_kind == EXPR_CONST && expr->const_expr.const_kind == CONST_BYTES;
}

INLINE bool expr_is_const_untyped_list(Expr *expr)
{
	ASSERT(expr->resolve_status == RESOLVE_DONE);
	return expr->expr_kind == EXPR_CONST && expr->const_expr.const_kind == CONST_UNTYPED_LIST;
}

INLINE bool expr_is_const_int(Expr *expr)
{
	ASSERT(expr->resolve_status == RESOLVE_DONE);
	return expr->expr_kind == EXPR_CONST && expr->const_expr.const_kind == CONST_INTEGER;
}

INLINE bool expr_is_const_float(Expr *expr)
{
	ASSERT(expr->resolve_status == RESOLVE_DONE);
	return expr->expr_kind == EXPR_CONST && expr->const_expr.const_kind == CONST_FLOAT;
}

INLINE bool expr_is_const_typeid(Expr *expr)
{
	ASSERT(expr->resolve_status == RESOLVE_DONE);
	return expr->expr_kind == EXPR_CONST && expr->const_expr.const_kind == CONST_TYPEID;
}

INLINE bool expr_is_const_member(Expr *expr)
{
	ASSERT(expr->resolve_status == RESOLVE_DONE);
	return expr->expr_kind == EXPR_CONST && expr->const_expr.const_kind == CONST_MEMBER;
}

INLINE bool check_module_name(Path *path)
{
	if (!str_is_valid_module_name(path->module))
	{
		RETURN_PRINT_ERROR_AT(false, path, "A module name may not have any uppercase characters, trailing, leading or double '_'");
	}
	return true;
}

INLINE void const_init_set_type(ConstInitializer *init, Type *type)
{
	init->type = type_flatten(type);
}

INLINE bool expr_is_valid_index(Expr *expr)
{
	ASSERT_SPAN(expr, expr_is_const_int(expr));
	return int_fits(expr->const_expr.ixx, TYPE_I64);
}


const char *default_c_compiler(void);

void print_build_env(void);
void print_asm(PlatformTarget *target);
const char *os_type_to_string(OsType os);
