// Copyright (c) 2019-2023 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

// Only include this from compiler_common.h


#if IS_CLANG
#define FLAG_ATTR __attribute__((flag_enum))
#else
#define FLAG_ATTR
#endif

typedef enum
{
	COND_MISSING = -1,
	COND_TRUE = 1,
	COND_FALSE = 0,
} CondResult;

typedef enum
{
	BINARYOP_ERROR,
	BINARYOP_MULT,
	BINARYOP_SUB,
	BINARYOP_ADD,
	BINARYOP_DIV,
	BINARYOP_MOD,
	BINARYOP_SHR,
	BINARYOP_SHL,
	BINARYOP_BIT_OR,
	BINARYOP_BIT_XOR,
	BINARYOP_BIT_AND,
	BINARYOP_AND,
	BINARYOP_OR,
	BINARYOP_ELSE,
	// Don't change the ordering for GT to EQ or things will break
	BINARYOP_GT,
	BINARYOP_GE,
	BINARYOP_LT,
	BINARYOP_LE,
	BINARYOP_NE,
	BINARYOP_EQ,
	// Only "assign" BINOPS after this point
	BINARYOP_ASSIGN,
	BINARYOP_ADD_ASSIGN,
	BINARYOP_BIT_AND_ASSIGN,
	BINARYOP_BIT_OR_ASSIGN,
	BINARYOP_BIT_XOR_ASSIGN,
	BINARYOP_DIV_ASSIGN,
	BINARYOP_MOD_ASSIGN,
	BINARYOP_MULT_ASSIGN,
	BINARYOP_SHR_ASSIGN,
	BINARYOP_SHL_ASSIGN,
	BINARYOP_SUB_ASSIGN,
	BINARYOP_LAST = BINARYOP_SUB_ASSIGN
} BinaryOp;

typedef enum
{
	AST_POISONED,
	AST_ASM_STMT,
	AST_ASM_BLOCK_STMT,
	AST_ASSERT_STMT,
	AST_BREAK_STMT,
	AST_CASE_STMT,
	AST_COMPOUND_STMT,
	AST_CONTINUE_STMT,
	AST_CT_ASSERT,
	AST_CT_ECHO_STMT,
	AST_CT_ELSE_STMT,
	AST_CT_FOREACH_STMT,
	AST_CT_FOR_STMT,
	AST_CT_IF_STMT,
	AST_CT_SWITCH_STMT,
	AST_DECLARE_STMT,
	AST_DECLS_STMT,
	AST_DEFAULT_STMT,
	AST_DEFER_STMT,
	AST_EXPR_STMT,
	AST_FOR_STMT,
	AST_FOREACH_STMT,
	AST_IF_CATCH_SWITCH_STMT,
	AST_IF_STMT,
	AST_NOP_STMT,
	AST_RETURN_STMT,
	AST_BLOCK_EXIT_STMT,
	AST_SWITCH_STMT,
	AST_NEXTCASE_STMT,
	AST_CONTRACT,
	AST_CONTRACT_FAULT,
} AstKind;


typedef enum
{
	CAST_ANYPTR,
	CAST_ANYBOOL,
	CAST_APTSA,
	CAST_ARRVEC,
	CAST_BOOLBOOL,
	CAST_BOOLFP,
	CAST_BOOLINT,
	CAST_BOOLVECINT,
	CAST_BSINTARR,
	CAST_INTARRBS,
	CAST_EREU,
	CAST_ERINT,
	CAST_ERPTR,
	CAST_ERROR,
	CAST_EUBOOL,
	CAST_EUER,
	CAST_FPBOOL,
	CAST_FPFP,
	CAST_FPINT,
	CAST_INTINT,
	CAST_INTBOOL,
	CAST_INTENUM,
	CAST_INTFP,
	CAST_IDPTR,
	CAST_IDBOOL,
	CAST_IDINT,
	CAST_PTRANY,
	CAST_PTRBOOL,
	CAST_PTRPTR,
	CAST_PTRINT,
	CAST_SLBOOL,
	CAST_SAPTR,
	CAST_SLSL,
	CAST_SLARR,
	CAST_STRPTR,
	CAST_STINLINE,
	CAST_VECARR,
	CAST_VOID,
	CAST_INTERR,
	CAST_INTPTR,
	CAST_EXPVEC,
} CastKind;


typedef enum
{
	DECL_POISONED = 0,
	DECL_ATTRIBUTE,
	DECL_BITSTRUCT,
	DECL_BODYPARAM,
	DECL_CT_ASSERT,
	DECL_CT_ECHO,
	DECL_CT_EXEC,
	DECL_CT_INCLUDE,
	DECL_DECLARRAY,
	DECL_DEFINE,
	DECL_DISTINCT,
	DECL_ENUM,
	DECL_ENUM_CONSTANT,
	DECL_ERASED,
	DECL_FAULT,
	DECL_FAULTVALUE,
	DECL_FNTYPE,
	DECL_FUNC,
	DECL_GLOBALS,
	DECL_IMPORT,
	DECL_LABEL,
	DECL_MACRO,
	DECL_INTERFACE,
	DECL_STRUCT,
	DECL_TYPEDEF,
	DECL_UNION,
	DECL_VAR,
} DeclKind;

#define NON_TYPE_DECLS DECL_IMPORT: case DECL_MACRO: \
	case DECL_DECLARRAY: case DECL_ATTRIBUTE: case DECL_LABEL: \
	case DECL_DEFINE: case DECL_CT_ASSERT: case DECL_CT_EXEC: \
	case DECL_CT_ECHO: case DECL_CT_INCLUDE: case DECL_GLOBALS: \
	case DECL_BODYPARAM: case DECL_VAR: case DECL_ENUM_CONSTANT: case DECL_FAULTVALUE: \
	case DECL_POISONED

#define NON_RUNTIME_EXPR EXPR_DESIGNATOR: case EXPR_POISONED: \
		case EXPR_CT_DEFINED: case EXPR_CT_AND_OR:\
		case EXPR_CT_CASTABLE: case EXPR_CT_IS_CONST: \
		case EXPR_CT_ARG: case EXPR_TYPEINFO: case EXPR_CT_IDENT: case EXPR_HASH_IDENT: \
		case EXPR_COMPILER_CONST: case EXPR_CT_CALL: \
		case EXPR_ANYSWITCH: case EXPR_STRINGIFY: \
		case EXPR_CT_EVAL: case EXPR_CT_CONCAT: case EXPR_CT_APPEND

typedef enum
{
	CONTRACT_UNKNOWN,
	CONTRACT_PURE,
	CONTRACT_REQUIRE,
	CONTRACT_PARAM,
	CONTRACT_OPTIONALS,
	CONTRACT_ENSURE,
} ContractKind;

typedef enum
{
	INTROSPECT_TYPE_VOID = 0,
	INTROSPECT_TYPE_BOOL = 1,
	INTROSPECT_TYPE_SIGNED_INT = 2,
	INTROSPECT_TYPE_UNSIGNED_INT = 3,
	INTROSPECT_TYPE_FLOAT = 4,
	INTROSPECT_TYPE_TYPEID = 5,
	INTROSPECT_TYPE_ANYFAULT = 6,
	INTROSPECT_TYPE_ANY = 7,
	INTROSPECT_TYPE_ENUM = 8,
	INTROSPECT_TYPE_FAULT = 9,
	INTROSPECT_TYPE_STRUCT = 10,
	INTROSPECT_TYPE_UNION = 11,
	INTROSPECT_TYPE_BITSTRUCT = 12,
	INTROSPECT_TYPE_FUNC = 13,
	INTROSPECT_TYPE_OPTIONAL = 14,
	INTROSPECT_TYPE_ARRAY = 15,
	INTROSPECT_TYPE_SLICE = 16,
	INTROSPECT_TYPE_VECTOR = 17,
	INTROSPECT_TYPE_DISTINCT = 18,
	INTROSPECT_TYPE_POINTER = 19,
	INTROSPECT_TYPE_INTERFACE = 20,
} IntrospectType;

typedef enum
{
	EXPR_ACCESS,
	EXPR_ANYSWITCH,
	EXPR_ASM,
	EXPR_BENCHMARK_HOOK,
	EXPR_BINARY,
	EXPR_BITACCESS,
	EXPR_BITASSIGN,
	EXPR_BUILTIN,
	EXPR_BUILTIN_ACCESS,
	EXPR_CALL,
	EXPR_CAST,
	EXPR_CATCH_UNWRAP,
	EXPR_COMPILER_CONST,
	EXPR_COMPOUND_LITERAL,
	EXPR_COND,
	EXPR_CONST,
	EXPR_CT_AND_OR,
	EXPR_CT_ARG,
	EXPR_CT_APPEND,
	EXPR_CT_CALL,
	EXPR_CT_CASTABLE,
	EXPR_CT_CONCAT,
	EXPR_CT_DEFINED,
	EXPR_CT_EVAL,
	EXPR_CT_IDENT,
	EXPR_CT_IS_CONST,
	EXPR_DECL,
	EXPR_DEFAULT_ARG,
	EXPR_DESIGNATED_INITIALIZER_LIST,
	EXPR_DESIGNATOR,
	EXPR_EMBED,
	EXPR_EXPRESSION_LIST,
	EXPR_EXPR_BLOCK,
	EXPR_FORCE_UNWRAP,
	EXPR_GENERIC_IDENT,
	EXPR_GROUP,
	EXPR_HASH_IDENT,
	EXPR_IDENTIFIER,
	EXPR_INITIALIZER_LIST,
	EXPR_LAMBDA,
	EXPR_LAST_FAULT,
	EXPR_MACRO_BLOCK,
	EXPR_MACRO_BODY,
	EXPR_MACRO_BODY_EXPANSION,
	EXPR_NOP,
	EXPR_OPERATOR_CHARS,
	EXPR_OPTIONAL,
	EXPR_OTHER_CONTEXT,
	EXPR_POINTER_OFFSET,
	EXPR_POISONED,
	EXPR_POST_UNARY,
	EXPR_RETHROW,
	EXPR_RETVAL,
	EXPR_SLICE,
	EXPR_SLICE_ASSIGN,
	EXPR_SLICE_COPY,
	EXPR_STRINGIFY,
	EXPR_SUBSCRIPT,
	EXPR_SUBSCRIPT_ADDR,
	EXPR_SUBSCRIPT_ASSIGN,
	EXPR_SWIZZLE,
	EXPR_TERNARY,
	EXPR_TEST_HOOK,
	EXPR_TRY_UNWRAP,
	EXPR_TRY_UNWRAP_CHAIN,
	EXPR_TYPEID,
	EXPR_TYPEID_INFO,
	EXPR_TYPEINFO,
	EXPR_UNARY,
	EXPR_VASPLAT,
} ExprKind;

typedef enum
{
	ASM_ARG_REG,
	ASM_ARG_ADDR,
	ASM_ARG_REGVAR,
	ASM_ARG_ADDROF,
	ASM_ARG_MEMVAR,
	ASM_ARG_VALUE,
	ASM_ARG_INT,
} AsmArgKind;

typedef enum
{
	ASM_REG_INT,
	ASM_REG_FLOAT,
	ASM_REG_IVEC,
	ASM_REF_FVEC,
} AsmRegisterType;

typedef enum FLAG_ATTR
{
	ARG_BITS_8 = 1 << 0,
	ARG_BITS_16 = 1 << 1,
	ARG_BITS_32 = 1 << 2,
	ARG_BITS_64 = 1 << 3,
	ARG_BITS_128 = 1 << 4,
	ARG_BITS_256 = 1 << 5,
	ARG_BITS_512 = 1 << 6,
	ARG_BITS_80 = 1 << 7,
} AsmArgBits;

typedef enum
{
	TYPEID_INFO_KIND,
	TYPEID_INFO_PARENTOF,
	TYPEID_INFO_INNER,
	TYPEID_INFO_LEN,
	TYPEID_INFO_SIZEOF,
	TYPEID_INFO_NAMES,
} TypeIdInfoKind;

typedef enum
{
	CONST_FLOAT,
	CONST_INTEGER,
	CONST_BOOL,
	CONST_ENUM,
	CONST_ERR,
	CONST_BYTES,
	CONST_STRING,
	CONST_POINTER,
	CONST_TYPEID,
	CONST_INITIALIZER,
	CONST_UNTYPED_LIST,
	CONST_MEMBER,
} ConstKind;

typedef enum
{
	DESIGNATOR_FIELD,
	DESIGNATOR_ARRAY,
	DESIGNATOR_RANGE
} DesignatorType;



typedef enum
{
	PREC_NONE,
	PREC_ASSIGNMENT,        // =, *=, /=, %=, +=, etc
	PREC_TERNARY,           // ?: ? ??
	PREC_OR,                // ||
	PREC_AND,               // &&
	PREC_RELATIONAL,        // < > <= >= == !=
	PREC_ADDITIVE,          // + -
	PREC_BIT,               // ^ | &
	PREC_SHIFT,             // << >>
	PREC_MULTIPLICATIVE,    // * / %
	PREC_UNARY,             // ! - + ~ * & prefix ++/-- (type)
	PREC_CALL,              // . () [] postfix ++ -- !! !
	PREC_PRIMARY,
} Precedence;

typedef enum FLAG_ATTR
{
	SCOPE_NONE = 0,
	SCOPE_ENSURE = 1 << 1,
	SCOPE_ENSURE_MACRO = 1 << 2,
	SCOPE_EXPR_BLOCK = 1 << 3,
	SCOPE_MACRO = 1 << 4,
	SCOPE_COND = 1 << 5,
} ScopeFlags;

typedef enum
{
	RESOLVE_NOT_DONE = 0,
	RESOLVE_RUNNING,
	RESOLVE_DONE
} ResolveStatus;

typedef enum
{
	TYPE_INFO_POISON,
	TYPE_INFO_IDENTIFIER,
	TYPE_INFO_CT_IDENTIFIER,
	TYPE_INFO_TYPEOF,
	TYPE_INFO_VATYPE,
	TYPE_INFO_EVALTYPE,
	TYPE_INFO_TYPEFROM,
	TYPE_INFO_ARRAY,
	TYPE_INFO_VECTOR,
	TYPE_INFO_INFERRED_ARRAY,
	TYPE_INFO_INFERRED_VECTOR,
	TYPE_INFO_SLICE,
	TYPE_INFO_POINTER,
	TYPE_INFO_GENERIC,
} TypeInfoKind;

typedef enum
{
	TOKEN_INVALID_TOKEN = 0,

	// Single-character tokens.
	TOKEN_AMP,              // &
	TOKEN_AT,               // @
	TOKEN_BANG,             // !
	TOKEN_BIT_NOT,          // ~
	TOKEN_BIT_OR,           // =
	TOKEN_BIT_XOR,          // ^
	TOKEN_COLON,            // :
	TOKEN_COMMA,            // ,
	TOKEN_EOS,              // ;
	TOKEN_EQ,               // =
	TOKEN_GREATER,          // >
	TOKEN_DIV,              // /
	TOKEN_DOLLAR,           // $
	TOKEN_DOT,              // .
	TOKEN_HASH,             // #
	TOKEN_LESS,             // <
	TOKEN_LBRACE,           // {
	TOKEN_LBRACKET,         // [
	TOKEN_LPAREN,           // (
	TOKEN_MINUS,            // -
	TOKEN_MOD,              // %
	TOKEN_PLUS,             // +
	TOKEN_QUESTION,         // ?
	TOKEN_RBRACE,           // }
	TOKEN_RBRACKET,         // ]
	TOKEN_RPAREN,           // )
	TOKEN_STAR,             // *
	TOKEN_UNDERSCORE,       // _

	// two character tokens.
	TOKEN_AND,              // &&
	TOKEN_ARROW,            // -> // Not used but reserved
	TOKEN_BANGBANG,         // !!
	TOKEN_BIT_AND_ASSIGN,   // &=
	TOKEN_BIT_OR_ASSIGN,    // |=
	TOKEN_BIT_XOR_ASSIGN,   // ^=
	TOKEN_DIV_ASSIGN,       // /=
	TOKEN_DOTDOT,           // ..
	TOKEN_BUILTIN,          // $$
	TOKEN_ELVIS,            // ?:
	TOKEN_EQEQ,             // ==
	TOKEN_GREATER_EQ,       // >=
	TOKEN_IMPLIES,          // =>
	TOKEN_LESS_EQ,          // <=
	TOKEN_LBRAPIPE,         // {|
	TOKEN_LGENPAR,          // (<
	TOKEN_LVEC,             // [<
	TOKEN_MINUS_ASSIGN,     // -=
	TOKEN_MINUSMINUS,       // --
	TOKEN_MOD_ASSIGN,       // %=
	TOKEN_MULT_ASSIGN,      // *=
	TOKEN_NOT_EQUAL,        // !=
	TOKEN_OR,               // ||
	TOKEN_PLUS_ASSIGN,      // +=
	TOKEN_PLUSPLUS,         // ++
	TOKEN_RBRAPIPE,         // |}
	TOKEN_RGENPAR,          // >)
	TOKEN_RVEC,             // >]
	TOKEN_QUESTQUEST,       // ??
	TOKEN_SCOPE,            // ::
	TOKEN_SHL,              // <<
	TOKEN_SHR,              // >>

	// Three or more
	TOKEN_ELLIPSIS,         // ...
	TOKEN_SHL_ASSIGN,       // <<=
	TOKEN_SHR_ASSIGN,       // >>=

	// Literals.
	TOKEN_IDENT,            // Any normal ident.
	TOKEN_CONST_IDENT,      // Any purely uppercase ident,
	TOKEN_TYPE_IDENT,       // Any ident on the format FooBar or __FooBar

	// We want to parse $foo separately,
	// otherwise we allow things like "$ foo" which would be pretty bad.
	TOKEN_CT_IDENT,         // $foobar
	TOKEN_CT_CONST_IDENT,   // $FOOBAR
	TOKEN_CT_TYPE_IDENT,    // $Foobar

	// We want to parse #foo separately.
	TOKEN_HASH_IDENT,       // #foobar
	TOKEN_HASH_CONST_IDENT, // #FOOBAR
	TOKEN_HASH_TYPE_IDENT,  // #Foobar

	TOKEN_AT_IDENT,         // @macro
	TOKEN_AT_CONST_IDENT,   // @MACRO
	TOKEN_AT_TYPE_IDENT,    // @Macro

	TOKEN_STRING,           // "Teststring"
	TOKEN_INTEGER,          // 123 0x23 0b10010 0o327
	TOKEN_CHAR_LITERAL,        // 'a' 'FO' 'BARS' '\u1232'
	TOKEN_REAL,             // 0x23.2p-2a 43.23e23
	TOKEN_BYTES,            // Base64 or Hex

	TOKEN_DOC_COMMENT,      // Doc Comment start

	// Basic types names
	TOKEN_VOID,
	TOKEN_FIRST_KEYWORD = TOKEN_VOID,
	TOKEN_BOOL,
	TOKEN_CHAR,
	TOKEN_DOUBLE,
	TOKEN_FLOAT,
	TOKEN_FLOAT16,
	TOKEN_BFLOAT,
	TOKEN_INT128,
	TOKEN_ICHAR,
	TOKEN_INT,
	TOKEN_IPTR,
	TOKEN_ISZ,
	TOKEN_LONG,
	TOKEN_SHORT,
	TOKEN_UINT128,
	TOKEN_UINT,
	TOKEN_ULONG,
	TOKEN_UPTR,
	TOKEN_USHORT,
	TOKEN_USZ,
	TOKEN_FLOAT128,
	TOKEN_ANY,
	TOKEN_ANYFAULT,
	TOKEN_TYPEID,

	// Keywords
	TOKEN_ASSERT,
	TOKEN_ASM,
	TOKEN_BITSTRUCT,
	TOKEN_BREAK,
	TOKEN_CASE,
	TOKEN_CATCH,
	TOKEN_CONST,
	TOKEN_CONTINUE,
	TOKEN_DEF,
	TOKEN_DEFAULT,
	TOKEN_DEFER,
	TOKEN_DISTINCT,
	TOKEN_DO,
	TOKEN_ELSE,
	TOKEN_ENUM,
	TOKEN_EXTERN,
	TOKEN_FALSE,
	TOKEN_FAULT,
	TOKEN_FOR,
	TOKEN_FOREACH,
	TOKEN_FOREACH_R,
	TOKEN_FN,
	TOKEN_TLOCAL,
	TOKEN_IF,
	TOKEN_INLINE,
	TOKEN_IMPORT,
	TOKEN_MACRO,
	TOKEN_MODULE,
	TOKEN_NEXTCASE,
	TOKEN_NULL,
	TOKEN_INTERFACE,
	TOKEN_RETURN,
	TOKEN_STATIC,
	TOKEN_STRUCT,
	TOKEN_SWITCH,
	TOKEN_TRUE,
	TOKEN_TRY,
	TOKEN_UNION,
	TOKEN_VAR,
	TOKEN_WHILE,
	TOKEN_LAST_NON_CT_KEYWORD = TOKEN_WHILE,

	TOKEN_CT_ALIGNOF,           // $alignof
	TOKEN_CT_AND,               // $and
	TOKEN_CT_APPEND,            // $append
	TOKEN_CT_ASSERT,            // $assert
	TOKEN_CT_ASSIGNABLE,        // $assignable
	TOKEN_CT_CASE,              // $case
	TOKEN_CT_CONCAT,            // $concat
	TOKEN_CT_DEFAULT,           // $default
	TOKEN_CT_DEFINED,           // $defined
	TOKEN_CT_ECHO,              // $echo
	TOKEN_CT_ELSE,              // $else
	TOKEN_CT_EMBED,             // $embed
	TOKEN_CT_ENDFOR,            // $endfor
	TOKEN_CT_ENDFOREACH,        // $endforeach
	TOKEN_CT_ENDIF,             // $endif
	TOKEN_CT_ENDSWITCH,         // $endswitch
	TOKEN_CT_EVAL,              // $eval
	TOKEN_CT_EVALTYPE,          // $evaltype
	TOKEN_CT_ERROR,             // $error
	TOKEN_CT_EXEC,              // $exec
	TOKEN_CT_EXTNAMEOF,         // $extnameof
	TOKEN_CT_FEATURE,           // $feature
	TOKEN_CT_FOR,               // $for
	TOKEN_CT_FOREACH,           // $foreach
	TOKEN_CT_IF,                // $if
	TOKEN_CT_INCLUDE,           // $include
	TOKEN_CT_IS_CONST,          // $is_const
	TOKEN_CT_NAMEOF,            // $nameof
	TOKEN_CT_OFFSETOF,          // $offsetof
	TOKEN_CT_OR,                // $or
	TOKEN_CT_QNAMEOF,           // $qnameof
	TOKEN_CT_SIZEOF,            // $sizeof
	TOKEN_CT_STRINGIFY,         // $stringify
	TOKEN_CT_SWITCH,            // $switch
	TOKEN_CT_TYPEFROM,          // $typefrom
	TOKEN_CT_TYPEOF,            // $typeof
	TOKEN_CT_VACOUNT,           // $vacount
	TOKEN_CT_VATYPE,            // $vatype
	TOKEN_CT_VACONST,           // $vaconst,
	TOKEN_CT_VAREF,             // $varef,
	TOKEN_CT_VAARG,             // $vaarg,
	TOKEN_CT_VAEXPR,            // $vaexpr,
	TOKEN_CT_VASPLAT,           // $vasplat,
	TOKEN_LAST_KEYWORD = TOKEN_CT_VASPLAT,
	TOKEN_DOCS_START,       // /**
	TOKEN_DOCS_END,         // */ (may start with an arbitrary number of `*`
	TOKEN_DOC_DIRECTIVE,    // Any doc directive

	TOKEN_EOF,              // \n - SHOULD ALWAYS BE THE LAST TOKEN.

	TOKEN_LAST = TOKEN_EOF,
} TokenType;

#define NON_VOID_TYPE_TOKENS \
  TOKEN_BOOL: case TOKEN_CHAR: case TOKEN_DOUBLE: case TOKEN_FLOAT: \
  case TOKEN_FLOAT16: case TOKEN_BFLOAT: case TOKEN_INT128: case TOKEN_ICHAR: case TOKEN_INT: \
  case TOKEN_IPTR: case TOKEN_LONG: \
  case TOKEN_SHORT: case TOKEN_UINT128: case TOKEN_UINT: case TOKEN_ULONG:  \
  case TOKEN_UPTR: case TOKEN_USHORT: case TOKEN_USZ: \
  case TOKEN_ISZ: case TOKEN_FLOAT128: case TOKEN_TYPEID: case TOKEN_ANYFAULT: case TOKEN_ANY
#define TYPE_TOKENS NON_VOID_TYPE_TOKENS: case TOKEN_VOID
#define CT_TYPE_TOKENS TOKEN_CT_TYPE_IDENT: case TOKEN_CT_TYPEOF: case TOKEN_CT_EVALTYPE: \
	case TOKEN_CT_VATYPE: case TOKEN_CT_TYPEFROM
#define TYPELIKE_TOKENS TYPE_TOKENS: case TOKEN_TYPE_IDENT: case CT_TYPE_TOKENS

// Note that ordering matters here. If ordering is changed,
// So must type_find_max_type and friends.
// The reason is that for binary expressions we can simplify
// by knowing the type_kind of left <= type kind of right
typedef enum
{
	TYPE_POISONED,
	TYPE_VOID,
	TYPE_BOOL,
	TYPE_I8,
	TYPE_INTEGER_FIRST = TYPE_I8,
	TYPE_NUM_FIRST = TYPE_INTEGER_FIRST,
	TYPE_INT_FIRST = TYPE_INTEGER_FIRST,
	TYPE_I16,
	TYPE_I32,
	TYPE_I64,
	TYPE_I128,
	TYPE_INT_LAST = TYPE_I128,
	TYPE_U8,
	TYPE_UINT_FIRST = TYPE_U8,
	TYPE_U16,
	TYPE_U32,
	TYPE_U64,
	TYPE_U128,
	TYPE_UINT_LAST = TYPE_U128,
	TYPE_INTEGER_LAST = TYPE_U128,
	TYPE_F16,
	TYPE_FLOAT_FIRST = TYPE_F16,
	TYPE_BF16,
	TYPE_F32,
	TYPE_F64,
	TYPE_F128,
	TYPE_FLOAT_LAST = TYPE_F128,
	TYPE_NUM_LAST = TYPE_FLOAT_LAST,
	TYPE_DISTINCT,
	TYPE_ANY,
	TYPE_INTERFACE,
	TYPE_ANYFAULT,
	TYPE_TYPEID,
	TYPE_FUNC_PTR,
	TYPE_POINTER,
	TYPE_ENUM,
	TYPE_FUNC_RAW,
	TYPE_STRUCT,
	TYPE_UNION,
	TYPE_BITSTRUCT,
	TYPE_FAULTTYPE,
	TYPE_TYPEDEF,
	TYPE_SLICE,
	TYPE_ARRAY,
	TYPE_FIRST_ARRAYLIKE = TYPE_ARRAY,
	TYPE_FLEXIBLE_ARRAY,
	TYPE_INFERRED_ARRAY,
	TYPE_VECTOR,
	TYPE_INFERRED_VECTOR,
	TYPE_LAST_ARRAYLIKE = TYPE_INFERRED_VECTOR,
	TYPE_UNTYPED_LIST,
	TYPE_OPTIONAL,
	TYPE_WILDCARD,
	TYPE_TYPEINFO,
	TYPE_MEMBER,
	TYPE_LAST = TYPE_MEMBER
} TypeKind;

#define FLATTENED_TYPES TYPE_DISTINCT: case TYPE_OPTIONAL: case TYPE_TYPEDEF
#define LOWERED_TYPES CT_TYPES: case TYPE_ENUM: case TYPE_TYPEDEF: case TYPE_TYPEID: \
  case TYPE_DISTINCT: case TYPE_ANYFAULT: case TYPE_FAULTTYPE: case TYPE_BITSTRUCT: \
  case TYPE_OPTIONAL: case TYPE_INTERFACE
#define CT_TYPES TYPE_TYPEINFO: case TYPE_INFERRED_ARRAY: case TYPE_INFERRED_VECTOR: case TYPE_UNTYPED_LIST: \
case TYPE_POISONED: case TYPE_MEMBER: case TYPE_WILDCARD
#define ALL_INTS TYPE_I8: case TYPE_I16: case TYPE_I32: case TYPE_I64: case TYPE_I128: \
case TYPE_U8: case TYPE_U16: case TYPE_U32: case TYPE_U64: case TYPE_U128
#define ALL_SIGNED_INTS TYPE_I8: case TYPE_I16: case TYPE_I32: case TYPE_I64: case TYPE_I128
#define ALL_UNSIGNED_INTS TYPE_U8: case TYPE_U16: case TYPE_U32: case TYPE_U64: case TYPE_U128
#define ALL_FLOATS TYPE_BF16: case TYPE_F16: case TYPE_F32: case TYPE_F64: case TYPE_F128

typedef enum
{
	UNARYOP_ERROR,
	UNARYOP_DEREF,
	UNARYOP_ADDR,
	UNARYOP_NEG,
	UNARYOP_PLUS,
	UNARYOP_BITNEG,
	UNARYOP_NOT,
	UNARYOP_INC,
	UNARYOP_DEC,
	UNARYOP_TADDR,
} UnaryOp;

typedef enum
{
	VARDECL_CONST = 0,
	VARDECL_GLOBAL,
	VARDECL_LOCAL,
	VARDECL_PARAM,
	VARDECL_MEMBER,
	VARDECL_BITMEMBER,
	VARDECL_PARAM_REF,
	VARDECL_PARAM_EXPR,
	VARDECL_UNWRAPPED,
	VARDECL_ERASE,
	VARDECL_REWRAPPED,
	VARDECL_PARAM_CT,
	VARDECL_PARAM_CT_TYPE,
	VARDECL_LOCAL_CT,
	VARDECL_LOCAL_CT_TYPE,
	VARDECL_FIRST_CT = VARDECL_PARAM_CT,
	VARDECL_LAST_CT = VARDECL_LOCAL_CT_TYPE,
} VarDeclKind;


typedef enum
{
	VISIBLE_PUBLIC,
	VISIBLE_PRIVATE,
	VISIBLE_LOCAL,
} Visibility;

typedef enum FLAG_ATTR AttributeDomain_
{
	ATTR_FUNC = 1 << 0,
	ATTR_GLOBAL = 1 << 1,
	ATTR_LOCAL = 1 << 2,
	ATTR_ENUM = 1 << 3,
	ATTR_STRUCT = 1 << 4,
	ATTR_UNION = 1 << 5,
	ATTR_CONST = 1 << 6,
	ATTR_FAULT = 1 << 7,
	ATTR_DEF = 1 << 8,
	ATTR_MEMBER = 1 << 9,
	ATTR_BITSTRUCT_MEMBER = 1 << 10,
	ATTR_INTERFACE = 1 << 11,
	ATTR_CALL = 1 << 12,
	ATTR_BITSTRUCT = 1 << 13,
	ATTR_MACRO = 1 << 14,
	ATTR_DISTINCT = 1 << 15,
	ATTR_ENUM_VALUE = 1 << 16,
	ATTR_INTERFACE_METHOD = 1 << 17,
	ATTR_PARAM = 1 << 18,
} AttributeDomain;

typedef enum
{
	OVERLOAD_ELEMENT_AT = 1,
	OVERLOAD_ELEMENT_REF,
	OVERLOAD_ELEMENT_SET,
	OVERLOAD_LEN,
} OperatorOverload;

typedef enum
{
	ATTRIBUTE_ALIGN,
	ATTRIBUTE_BENCHMARK,
	ATTRIBUTE_BIGENDIAN,
	ATTRIBUTE_BUILTIN,
	ATTRIBUTE_CALLCONV,
	ATTRIBUTE_DEPRECATED,
	ATTRIBUTE_DYNAMIC,
	ATTRIBUTE_EXPORT,
	ATTRIBUTE_EXTERN,
	ATTRIBUTE_FINALIZER,
	ATTRIBUTE_IF,
	ATTRIBUTE_INLINE,
	ATTRIBUTE_INIT,
	ATTRIBUTE_LINK,
	ATTRIBUTE_LITTLEENDIAN,
	ATTRIBUTE_LOCAL,
	ATTRIBUTE_MAYDISCARD,
	ATTRIBUTE_NAKED,
	ATTRIBUTE_NODISCARD,
	ATTRIBUTE_NOINIT,
	ATTRIBUTE_NOINLINE,
	ATTRIBUTE_NOPADDING,
	ATTRIBUTE_NOPADDING_RECURSIVE,
	ATTRIBUTE_NORETURN,
	ATTRIBUTE_NOSTRIP,
	ATTRIBUTE_OBFUSCATE,
	ATTRIBUTE_OPERATOR,
	ATTRIBUTE_OPTIONAL,
	ATTRIBUTE_OVERLAP,
	ATTRIBUTE_PACKED,
	ATTRIBUTE_PRIVATE,
	ATTRIBUTE_PUBLIC,
	ATTRIBUTE_PURE,
	ATTRIBUTE_REFLECT,
	ATTRIBUTE_SAFEMACRO,
	ATTRIBUTE_SECTION,
	ATTRIBUTE_TEST,
	ATTRIBUTE_UNUSED,
	ATTRIBUTE_USED,
	ATTRIBUTE_WASM,
	ATTRIBUTE_WEAK,
	ATTRIBUTE_WINMAIN,
	ATTRIBUTE_NONE,
	NUMBER_OF_ATTRIBUTES = ATTRIBUTE_NONE,
} AttributeType;


typedef enum
{
	CALL_C,
	CALL_X64_VECTOR,
	CALL_AAPCS,
	CALL_AAPCS_VFP,
} CallABI;

typedef enum
{
	ANALYSIS_NOT_BEGUN,
	ANALYSIS_MODULE_HIERARCHY,
	ANALYSIS_MODULE_TOP,
	ANALYSIS_IMPORTS,
	ANALYSIS_REGISTER_GLOBAL_DECLARATIONS,
	ANALYSIS_REGISTER_CONDITIONAL_UNITS,
	ANALYSIS_REGISTER_CONDITIONAL_DECLARATIONS,
	ANALYSIS_DECLS,
	ANALYSIS_CT_ECHO,
	ANALYSIS_CT_ASSERT,
	ANALYSIS_FUNCTIONS,
	ANALYSIS_INTERFACE,
	ANALYSIS_FINALIZE,
	ANALYSIS_LAST = ANALYSIS_FINALIZE
} AnalysisStage;

typedef enum
{
	BUILTIN_ABS,
	BUILTIN_ANY_MAKE,
	BUILTIN_ATOMIC_LOAD,
	BUILTIN_ATOMIC_STORE,
	BUILTIN_ATOMIC_FETCH_EXCHANGE,
	BUILTIN_ATOMIC_FETCH_ADD,
	BUILTIN_ATOMIC_FETCH_SUB,
	BUILTIN_ATOMIC_FETCH_AND,
	BUILTIN_ATOMIC_FETCH_NAND,
	BUILTIN_ATOMIC_FETCH_OR,
	BUILTIN_ATOMIC_FETCH_XOR,
	BUILTIN_ATOMIC_FETCH_MAX,
	BUILTIN_ATOMIC_FETCH_MIN,
	BUILTIN_ATOMIC_FETCH_INC_WRAP,
	BUILTIN_ATOMIC_FETCH_DEC_WRAP,
	BUILTIN_BITREVERSE,
	BUILTIN_BREAKPOINT,
	BUILTIN_BSWAP,
	BUILTIN_CEIL,
	BUILTIN_COMPARE_EXCHANGE,
	BUILTIN_COPYSIGN,
	BUILTIN_COS,
	BUILTIN_CTLZ,
	BUILTIN_CTTZ,
	BUILTIN_EXACT_ADD,
	BUILTIN_EXACT_DIV,
	BUILTIN_EXACT_MOD,
	BUILTIN_EXACT_MUL,
	BUILTIN_EXACT_NEG,
	BUILTIN_EXACT_SUB,
	BUILTIN_EXP,
	BUILTIN_EXP2,
	BUILTIN_EXPECT,
	BUILTIN_EXPECT_WITH_PROBABILITY,
	BUILTIN_FLOOR,
	BUILTIN_FMA,
	BUILTIN_FMULADD,
	BUILTIN_FRAMEADDRESS,
	BUILTIN_FSHL,
	BUILTIN_FSHR,
	BUILTIN_GATHER,
	BUILTIN_GET_ROUNDING_MODE,
	BUILTIN_LOG,
	BUILTIN_LOG10,
	BUILTIN_LOG2,
	BUILTIN_MASKED_LOAD,
	BUILTIN_MASKED_STORE,
	BUILTIN_MAX,
	BUILTIN_MEMCOPY,
	BUILTIN_MEMCOPY_INLINE,
	BUILTIN_MEMMOVE,
	BUILTIN_MEMSET,
	BUILTIN_MEMSET_INLINE,
	BUILTIN_MIN,
	BUILTIN_NEARBYINT,
	BUILTIN_OVERFLOW_ADD,
	BUILTIN_OVERFLOW_MUL,
	BUILTIN_OVERFLOW_SUB,
	BUILTIN_POPCOUNT,
	BUILTIN_POW,
	BUILTIN_POW_INT,
	BUILTIN_PREFETCH,
	BUILTIN_REDUCE_ADD,
	BUILTIN_REDUCE_AND,
	BUILTIN_REDUCE_FADD,
	BUILTIN_REDUCE_FMUL,
	BUILTIN_REDUCE_MAX,
	BUILTIN_REDUCE_MIN,
	BUILTIN_REDUCE_MUL,
	BUILTIN_REDUCE_OR,
	BUILTIN_REDUCE_XOR,
	BUILTIN_REVERSE,
	BUILTIN_RETURNADDRESS,
	BUILTIN_RINT,
	BUILTIN_ROUND,
	BUILTIN_ROUNDEVEN,
	BUILTIN_SAT_ADD,
	BUILTIN_SAT_SHL,
	BUILTIN_SAT_SUB,
	BUILTIN_SCATTER,
	BUILTIN_SELECT,
	BUILTIN_SET_ROUNDING_MODE,
	BUILTIN_STR_HASH,
	BUILTIN_STR_UPPER,
	BUILTIN_STR_LOWER,
	BUILTIN_STR_FIND,
	BUILTIN_SWIZZLE,
	BUILTIN_SWIZZLE2,
	BUILTIN_SIN,
	BUILTIN_SQRT,
	BUILTIN_SYSCALL,
	BUILTIN_SYSCLOCK,
	BUILTIN_TRAP,
	BUILTIN_TRUNC,
	BUILTIN_UNALIGNED_LOAD,
	BUILTIN_UNALIGNED_STORE,
	BUILTIN_UNREACHABLE,
	BUILTIN_VECCOMPLT,
	BUILTIN_VECCOMPLE,
	BUILTIN_VECCOMPGT,
	BUILTIN_VECCOMPGE,
	BUILTIN_VECCOMPEQ,
	BUILTIN_VECCOMPNE,
	BUILTIN_VOLATILE_LOAD,
	BUILTIN_VOLATILE_STORE,
	BUILTIN_WASM_MEMORY_SIZE,
	BUILTIN_WASM_MEMORY_GROW,
	BUILTIN_NONE,
	NUMBER_OF_BUILTINS = BUILTIN_NONE,

// Disabled for now!
	BUILTIN_LLRINT,
	BUILTIN_LLROUND,
	BUILTIN_LRINT,
	BUILTIN_LROUND,
} BuiltinFunction;

typedef enum
{
	BUILTIN_DEF_DATE,
	BUILTIN_DEF_FILE,
	BUILTIN_DEF_FILEPATH,
	BUILTIN_DEF_FUNC,
	BUILTIN_DEF_FUNCTION,
	BUILTIN_DEF_LINE,
	BUILTIN_DEF_LINE_RAW,
	BUILTIN_DEF_MODULE,
	BUILTIN_DEF_BENCHMARK_NAMES,
	BUILTIN_DEF_BENCHMARK_FNS,
	BUILTIN_DEF_TEST_NAMES,
	BUILTIN_DEF_TEST_FNS,
	BUILTIN_DEF_TIME,
	BUILTIN_DEF_NONE,
	NUMBER_OF_BUILTIN_DEFINES = BUILTIN_DEF_NONE
} BuiltinDefine;

typedef enum
{
	TYPE_PROPERTY_ALIGNOF,
	TYPE_PROPERTY_ASSOCIATED,
	TYPE_PROPERTY_ELEMENTS,
	TYPE_PROPERTY_EXTNAMEOF,
	TYPE_PROPERTY_INF,
	TYPE_PROPERTY_IS_EQ,
	TYPE_PROPERTY_IS_ORDERED,
	TYPE_PROPERTY_LEN,
	TYPE_PROPERTY_MAX,
	TYPE_PROPERTY_MEMBERSOF,
	TYPE_PROPERTY_MIN,
	TYPE_PROPERTY_NAN,
	TYPE_PROPERTY_INNER,
	TYPE_PROPERTY_KINDOF,
	TYPE_PROPERTY_NAMES,
	TYPE_PROPERTY_NAMEOF,
	TYPE_PROPERTY_PARAMS,
	TYPE_PROPERTY_PARENTOF,
	TYPE_PROPERTY_QNAMEOF,
	TYPE_PROPERTY_RETURNS,
	TYPE_PROPERTY_SIZEOF,
	TYPE_PROPERTY_VALUES,
	TYPE_PROPERTY_NONE,
	NUMBER_OF_TYPE_PROPERTIES = TYPE_PROPERTY_NONE
} TypeProperty;
typedef enum
{
	ATOMIC_NONE,
	ATOMIC_UNORDERED,
	ATOMIC_RELAXED,
	ATOMIC_ACQUIRE,
	ATOMIC_RELEASE,
	ATOMIC_ACQUIRE_RELEASE,
	ATOMIC_SEQ_CONSISTENT,
} Atomicity;


typedef enum
{
	LEX_NORMAL,
	LEX_DOCS,
} LexMode;

typedef enum
{
	PARAM_ANY,
	PARAM_IN,
	PARAM_OUT,
	PARAM_INOUT,
} InOutModifier;

typedef enum
{
	CONSTANT_EVAL_NO_SIDE_EFFECTS,
	CONSTANT_EVAL_GLOBAL_INIT,
	CONSTANT_EVAL_LOCAL_INIT,
	CONSTANT_EVAL_CONSTANT_VALUE,
} ConstantEvalKind;

typedef enum
{
	COND_TYPE_UNWRAP_BOOL,
	COND_TYPE_UNWRAP,
	COND_TYPE_EVALTYPE_VALUE,
} CondType;


typedef enum
{
	MAIN_TYPE_ERROR,
	MAIN_TYPE_RAW,
	MAIN_TYPE_NO_ARGS,
	MAIN_TYPE_ARGS,
	MAIN_TYPE_WIN,
} MainType;

typedef enum
{
	CONV_NO = -1,
	CONV_VOID = 0,
	CONV_WILDCARD,
	CONV_BOOL,
	CONV_INT,
	CONV_FLOAT,
	CONV_POINTER,
	CONV_SLICE,
	CONV_VECTOR,
	CONV_BITSTRUCT,
	CONV_DISTINCT,
	CONV_ARRAY,
	CONV_STRUCT,
	CONV_UNION,
	CONV_ANY,
	CONV_INTERFACE,
	CONV_FAULT,
	CONV_ENUM,
	CONV_FUNC,
	CONV_TYPEID,
	CONV_ANYFAULT,
	CONV_VOIDPTR,
	CONV_VAPTR,
	CONV_INFERRED,
	CONV_UNTYPED_LIST,
	CONV_LAST = CONV_UNTYPED_LIST
} ConvGroup;
