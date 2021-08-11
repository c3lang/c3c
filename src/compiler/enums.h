// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

// Only include this from compiler_common.h

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
	AST_ASSERT_STMT,
	AST_BREAK_STMT,
	AST_CASE_STMT,
	AST_CATCH_STMT,
	AST_COMPOUND_STMT,
	AST_CONTINUE_STMT,
	AST_DEFINE_STMT,
	AST_CT_ASSERT,
	AST_CT_COMPOUND_STMT,
	AST_CT_IF_STMT,
	AST_CT_ELIF_STMT,
	AST_CT_ELSE_STMT,
	AST_CT_FOR_STMT,
	AST_CT_SWITCH_STMT,
	AST_DECLARE_STMT,
	AST_DEFAULT_STMT,
	AST_DEFER_STMT,
	AST_DO_STMT,
	AST_DOC_DIRECTIVE,
	AST_DOCS,
	AST_EXPR_STMT,
	AST_TRY_STMT,
	AST_FOR_STMT,
	AST_FOREACH_STMT,
	AST_IF_STMT,
	AST_NOP_STMT,
	AST_RETURN_STMT,
	AST_SWITCH_STMT,
	AST_NEXT_STMT,
	AST_UNREACHABLE_STMT,
	AST_VOLATILE_STMT,
	AST_WHILE_STMT,
	AST_SCOPED_STMT,
} AstKind;


typedef enum
{
	CAST_ERROR,
	CAST_ERBOOL,
	CAST_EUBOOL,
	CAST_EUER,
	CAST_EUINT,
	CAST_EREU,
	CAST_ERINT,
	CAST_XIERR,
	CAST_PTRPTR,
	CAST_PTRXI,
	CAST_ARRPTR,
	CAST_STRPTR,
	CAST_PTRBOOL,
	CAST_BOOLINT,
	CAST_BOOLFP,
	CAST_BOOLBOOL,
	CAST_FPBOOL,
	CAST_INTBOOL,
	CAST_FPFP,
	CAST_FPSI,
	CAST_FPUI,
	CAST_SISI,
	CAST_SIUI,
	CAST_SIFP,
	CAST_XIPTR,
	CAST_UISI,
	CAST_UIUI,
	CAST_UIFP,
	CAST_ENUMLOW,
	CAST_APTSA,
	CAST_SAPTR,
	CAST_SABOOL,
	CAST_STST,
	CAST_VRPTR,
	CAST_PTRVR,
	CAST_VRBOOL,
} CastKind;

typedef enum
{
	CAST_TYPE_EXPLICIT,
	CAST_TYPE_IMPLICIT,
	CAST_TYPE_OPTIONAL_IMPLICIT,
} CastType;



typedef enum
{
	DECL_POISONED = 0,
	DECL_ATTRIBUTE,
	DECL_ARRAY_VALUE,
	DECL_BITSTRUCT,
	DECL_CT_CASE,
	DECL_CT_ELIF,
	DECL_CT_ELSE,
	DECL_CT_IF,
	DECL_CT_SWITCH,
	DECL_CT_ASSERT,
	DECL_DEFINE,
	DECL_DISTINCT,
	DECL_ENUM,
	DECL_ENUM_CONSTANT,
	DECL_ERRTYPE,
	DECL_ERRVALUE,
	DECL_FUNC,
	DECL_GENERIC,
	DECL_IMPORT,
	DECL_INTERFACE,
	DECL_LABEL,
	DECL_MACRO,
	DECL_STRUCT,
	DECL_TYPEDEF,
	DECL_UNION,
	DECL_VAR,
} DeclKind;

#define NON_TYPE_DECLS DECL_ARRAY_VALUE: case DECL_IMPORT: case DECL_MACRO: \
	case DECL_CT_IF: case DECL_CT_ELSE: case DECL_CT_ELIF: \
	case DECL_CT_SWITCH: case DECL_CT_CASE: case DECL_ATTRIBUTE: case DECL_LABEL: \
    case DECL_DEFINE: case DECL_CT_ASSERT: case DECL_GENERIC

typedef enum
{
	DOC_DIRECTIVE_UNKNOWN,
	DOC_DIRECTIVE_PURE,
	DOC_DIRECTIVE_REQUIRE,
	DOC_DIRECTIVE_PARAM,
	DOC_DIRECTIVE_ERRORS,
	DOC_DIRECTIVE_ENSURE,
} DocDirectiveKind;

typedef enum
{
	EXPR_POISONED,
	EXPR_ACCESS,
	EXPR_BINARY,
	EXPR_MACRO_BODY_EXPANSION,
	EXPR_CALL,
	EXPR_CAST,
	EXPR_CATCH_UNWRAP,
	EXPR_COMPOUND_LITERAL,
	EXPR_CONST,
	EXPR_CONST_IDENTIFIER,
	EXPR_CT_IDENT,
	EXPR_COND,
	EXPR_DECL,
	EXPR_DESIGNATOR,
	EXPR_ELSE,
	EXPR_ENUM_CONSTANT,
	EXPR_EXPR_BLOCK,
	EXPR_EXPRESSION_LIST,
	EXPR_FAILABLE,
	EXPR_GROUP,
	EXPR_GUARD,
	EXPR_HASH_IDENT,
	EXPR_MACRO_BLOCK,
	EXPR_MACRO_EXPANSION,
	EXPR_MEMBER_ACCESS,
	EXPR_IDENTIFIER,
	EXPR_FLATPATH,
	EXPR_INITIALIZER_LIST,
	EXPR_LEN,
	EXPR_PLACEHOLDER,
	EXPR_POST_UNARY,
	EXPR_SCOPED_EXPR,
	EXPR_SLICE,
	EXPR_SLICE_ASSIGN,
	EXPR_SUBSCRIPT,
	EXPR_TERNARY,
	EXPR_TRY,
	EXPR_TRY_UNWRAP,
	EXPR_TRY_UNWRAP_CHAIN,
	EXPR_TRY_ASSIGN,
	EXPR_TRY_DECL,
	EXPR_TYPEID,
	EXPR_TYPEINFO,
	EXPR_TYPEOF,
	EXPR_UNARY,
	EXPR_UNDEF,
	EXPR_CT_CALL,
	EXPR_NOP,
} ExprKind;

typedef enum
{
	DESIGNATOR_FIELD,
	DESIGNATOR_ARRAY,
	DESIGNATOR_RANGE
} DesignatorType;

typedef enum
{
	FAILABLE_NO,
	FAILABLE_YES,
	FAILABLE_UNWRAPPED
} ExprFailableStatus;


typedef enum
{
	PREC_NONE,
	PREC_ASSIGNMENT,        // =, *=, /=, %=, +=, etc
	PREC_ELSE,              // else
	PREC_TERNARY,           // ?:
	PREC_OR,                // ||
	PREC_AND,               // &&
	PREC_RELATIONAL,        // < > <= >= == !=
	PREC_ADDITIVE,          // + -
	PREC_BIT,               // ^ | &
	PREC_SHIFT,             // << >>
	PREC_MULTIPLICATIVE,    // * / %
	PREC_UNARY,             // ! - + ~ * & prefix ++/-- try catch
	PREC_CALL,              // . () [] postfix ++/--
	PREC_MACRO,
	PREC_FIRST = PREC_MACRO
} Precedence;

typedef enum
{
	SCOPE_NONE = 0,
	SCOPE_DEFER = 1 << 4,
	SCOPE_EXPR_BLOCK = 1 << 5,
	SCOPE_MACRO = 1 << 6,
	SCOPE_COND = 1 << 7,
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
	TYPE_INFO_EXPRESSION,
	TYPE_INFO_ARRAY,
	TYPE_INFO_INC_ARRAY,
	TYPE_INFO_INFERRED_ARRAY,
	TYPE_INFO_SUBARRAY,
	TYPE_INFO_POINTER,
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
	TOKEN_ELVIS,            // ?:
	TOKEN_EQEQ,             // ==
	TOKEN_GREATER_EQ,       // >=
	TOKEN_LESS_EQ,          // <=
	TOKEN_LBRAPIPE,         // {|
	TOKEN_MINUS_ASSIGN,     // -=
	TOKEN_MINUSMINUS,       // --
	TOKEN_MOD_ASSIGN,       // %=
	TOKEN_MULT_ASSIGN,      // *=
	TOKEN_NOT_EQUAL,        // !=
	TOKEN_OR,               // ||
	TOKEN_PLACEHOLDER,      // ${
	TOKEN_PLUS_ASSIGN,      // +=
	TOKEN_PLUSPLUS,         // ++
	TOKEN_RBRAPIPE,         // |}
	TOKEN_SCOPE,            // ::
	TOKEN_SHL,              // <<
	TOKEN_SHR,              // >>

	// Three or more
	TOKEN_ELLIPSIS,         // ...
	TOKEN_SHL_ASSIGN,       // <<=
	TOKEN_SHR_ASSIGN,       // >>=

	// Basic types names
	TOKEN_VOID,
	TOKEN_BOOL,
	TOKEN_CHAR,
	TOKEN_DOUBLE,
	TOKEN_FLOAT,
	TOKEN_FLOAT16,
	TOKEN_INT128,
	TOKEN_ICHAR,
	TOKEN_INT,
	TOKEN_IPTR,
	TOKEN_IPTRDIFF,
	TOKEN_ISIZE,
	TOKEN_LONG,
	TOKEN_SHORT,
	TOKEN_UINT128,
	TOKEN_UINT,
	TOKEN_ULONG,
	TOKEN_UPTR,
	TOKEN_UPTRDIFF,
	TOKEN_USHORT,
	TOKEN_USIZE,
	TOKEN_FLOAT128,
	TOKEN_ANYERR,
	TOKEN_TYPEID,

	// Literals.
	TOKEN_IDENT,            // Any normal ident.
	TOKEN_CONST_IDENT,      // Any purely upper case ident,
	TOKEN_TYPE_IDENT,       // Any ident on the format FooBar or __FooBar

	// Asm
	TOKEN_ASM_STRING,
	TOKEN_ASM_CONSTRAINT,

	// We want to parse $foo separately.
	// Otherwise we allow things like "# foo" which would be pretty bad.
	TOKEN_CT_IDENT,         // $foobar
	TOKEN_CT_CONST_IDENT,   // $FOOBAR
	TOKEN_CT_TYPE_IDENT,    // $Foobar

	// We want to parse #foo separately.
	TOKEN_HASH_IDENT,       // #foobar
	TOKEN_HASH_CONST_IDENT, // #FOOBAR
	TOKEN_HASH_TYPE_IDENT,  // #Foobar

	TOKEN_STRING,           // "Teststring"
	TOKEN_INTEGER,          // 123 0x23 0b10010 0o327
	TOKEN_CHAR_LITERAL,        // 'a' 'FO' 'BARS' '\u1232'
	TOKEN_REAL,             // 0x23.2p-2a 43.23e23

	TOKEN_COMMENT,          // Comment
	TOKEN_DOC_COMMENT,      // Doc Comment

	// Keywords
	TOKEN_ALIAS,            // Reserved
	TOKEN_AS,
	TOKEN_ASSERT,
	TOKEN_ASM,
	TOKEN_ATTRIBUTE,
	TOKEN_BITSTRUCT,
	TOKEN_BREAK,
	TOKEN_CASE,
	TOKEN_CATCH_OLD,
	TOKEN_CATCH,
	TOKEN_CONST,
	TOKEN_CONTINUE,
	TOKEN_DEFINE,
	TOKEN_DEFAULT,
	TOKEN_DEFER,
	TOKEN_DO,
	TOKEN_ELSE,
	TOKEN_ENUM,
	TOKEN_ERRTYPE,
	TOKEN_EXTERN,
	TOKEN_FALSE,
	TOKEN_FOR,
	TOKEN_FOREACH,
	TOKEN_FUNC,
	TOKEN_GENERIC,
	TOKEN_IF,
	TOKEN_IMPORT,
	TOKEN_INTERFACE,
	TOKEN_MACRO,
	TOKEN_MODULE,
	TOKEN_NEXTCASE,
	TOKEN_NULL,
	TOKEN_PRIVATE,
	TOKEN_RETURN,
	TOKEN_STATIC,
	TOKEN_STRUCT,
	TOKEN_SWITCH,
	TOKEN_TRUE,
	TOKEN_TRY_OLD,
	TOKEN_TRY,
	TOKEN_UNION,
	TOKEN_VAR,              // Reserved
	TOKEN_VIRTUAL,
	TOKEN_VOLATILE,
	TOKEN_WHILE,

	TOKEN_CT_ALIGNOF,       // $alignof
	TOKEN_CT_ASSERT,        // $assert
	TOKEN_CT_CASE,          // $case
	TOKEN_CT_DEFAULT,       // $default
	TOKEN_CT_FOR,           // $for
	TOKEN_CT_ELIF,          // $elif
	TOKEN_CT_ELSE,          // $else
	TOKEN_CT_ENDIF,         // $endif
	TOKEN_CT_ENDSWITCH,     // $endswitch
	TOKEN_CT_IF,            // $if
	TOKEN_CT_NAMEOF,        // $nameof
	TOKEN_CT_OFFSETOF,      // $offsetof
	TOKEN_CT_QNAMEOF,       // $qnameof
	TOKEN_CT_SIZEOF,        // $sizeof
	TOKEN_CT_SWITCH,        // $switch
	TOKEN_CT_TYPEOF,        // $typeof
	TOKEN_CT_UNREACHABLE,   // $unreachable

	TOKEN_DOCS_START,       // /**
	TOKEN_DOCS_END,         // */ (may start with an arbitrary number of `*`
	TOKEN_DOCS_EOL,         // "\n" only seen in directives.
	TOKEN_DOCS_DIRECTIVE,   // @ in the directive
	TOKEN_DOCS_LINE,        // Any line within /** **/

	TOKEN_EOF,              // \n - SHOULD ALWAYS BE THE LAST TOKEN.

	TOKEN_LAST = TOKEN_EOF,
} TokenType;

#define NON_VOID_TYPE_TOKENS \
  TOKEN_BOOL: case TOKEN_CHAR: case TOKEN_DOUBLE: case TOKEN_FLOAT: \
  case TOKEN_FLOAT16: case TOKEN_INT128: case TOKEN_ICHAR: case TOKEN_INT: \
  case TOKEN_IPTR: case TOKEN_IPTRDIFF: case TOKEN_ISIZE: case TOKEN_LONG: \
  case TOKEN_SHORT: case TOKEN_UINT128: case TOKEN_UINT: case TOKEN_ULONG:  \
  case TOKEN_UPTR: case TOKEN_UPTRDIFF: case TOKEN_USHORT: case TOKEN_USIZE:  \
  case TOKEN_FLOAT128: case TOKEN_TYPEID: case TOKEN_ANYERR
#define TYPE_TOKENS NON_VOID_TYPE_TOKENS: case TOKEN_VOID
#define TYPELIKE_TOKENS TYPE_TOKENS: case TOKEN_TYPE_IDENT: \
	case TOKEN_CT_TYPE_IDENT: case TOKEN_VIRTUAL: case TOKEN_CT_TYPEOF

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
	TYPE_I16,
	TYPE_I32,
	TYPE_I64,
	TYPE_I128,
	TYPE_U8,
	TYPE_U16,
	TYPE_U32,
	TYPE_U64,
	TYPE_U128,
	TYPE_IXX,
	TYPE_F16,
	TYPE_F32,
	TYPE_F64,
	TYPE_F128,
	TYPE_FXX,
	TYPE_TYPEID,
	TYPE_POINTER,
	TYPE_ENUM,
	TYPE_FUNC,
	TYPE_STRUCT,
	TYPE_UNION,
	TYPE_BITSTRUCT,
	TYPE_ERRTYPE,
	TYPE_ANYERR,
	TYPE_TYPEDEF,
	TYPE_STRLIT,
	TYPE_DISTINCT,
	TYPE_ARRAY,
	TYPE_SUBARRAY,
	TYPE_INFERRED_ARRAY,
	TYPE_TYPEINFO,
	TYPE_VECTOR,
	TYPE_VIRTUAL,
	TYPE_VIRTUAL_ANY,
	TYPE_LAST = TYPE_VIRTUAL_ANY
} TypeKind;

#define ALL_INTS TYPE_I8: case TYPE_I16: case TYPE_I32: case TYPE_I64: case TYPE_I128: \
case TYPE_U8: case TYPE_U16: case TYPE_U32: case TYPE_U64: case TYPE_U128: case TYPE_IXX
#define ALL_SIGNED_INTS TYPE_I8: case TYPE_I16: case TYPE_I32: case TYPE_I64: case TYPE_I128
#define ALL_UNSIGNED_INTS TYPE_U8: case TYPE_U16: case TYPE_U32: case TYPE_U64: case TYPE_U128
#define ALL_REAL_FLOATS TYPE_F16: case TYPE_F32: case TYPE_F64: case TYPE_F128
#define ALL_FLOATS ALL_REAL_FLOATS: case TYPE_FXX

#define TYPE_KINDS (TYPE_LAST + 1)

typedef enum
{
	UNARYOP_ERROR,
	UNARYOP_DEREF,
	UNARYOP_ADDR,
	UNARYOP_NEG,
	UNARYOP_BITNEG,
	UNARYOP_NOT,
	UNARYOP_INC,
	UNARYOP_DEC,
	UNARYOP_TADDR,
	UNARYOP_LAST = UNARYOP_DEC
} UnaryOp;

typedef enum
{
	POSTUNARYOP_INC,
	POSTUNARYOP_DEC,
} PostUnaryOp;

typedef enum
{
	VARDECL_CONST = 0,
	VARDECL_GLOBAL = 1,
	VARDECL_LOCAL = 2,
	VARDECL_PARAM = 3,
	VARDECL_MEMBER = 4,
	VARDECL_PARAM_CT = 5,
	VARDECL_PARAM_CT_TYPE = 6,
	VARDECL_PARAM_REF = 7,
	VARDECL_PARAM_EXPR = 8,
	VARDECL_LOCAL_CT = 9,
	VARDECL_LOCAL_CT_TYPE = 10,
	VARDECL_UNWRAPPED = 11,
	VARDECL_ERASE = 12,
	VARDECL_REWRAPPED = 13,
} VarDeclKind;

typedef enum
{
	VISIBLE_LOCAL,
	VISIBLE_MODULE,
	VISIBLE_PUBLIC,
	VISIBLE_EXTERN,
} Visibility;


typedef enum
{
	ATTR_FUNC = 1 << 0,
	ATTR_VAR = 1 << 1,
	ATTR_ENUM = 1 << 2,
	ATTR_STRUCT = 1 << 3,
	ATTR_UNION = 1 << 4,
	ATTR_CONST = 1 << 5,
	ATTR_ERROR = 1 << 6,
	ATTR_TYPEDEF = 1 << 7,
	ATTR_MEMBER = 1 << 8,
	ATTR_INTERFACE = 1 << 9,
	ATTR_CALL = 1 << 10,
	ATTR_BITSTRUCT = 1 << 11,
} AttributeDomain;

typedef enum
{
	ATTRIBUTE_INLINE,
	ATTRIBUTE_NOINLINE,
	ATTRIBUTE_OPAQUE,
	ATTRIBUTE_NORETURN,
	ATTRIBUTE_SECTION,
	ATTRIBUTE_EXTNAME,
	ATTRIBUTE_WEAK,
	ATTRIBUTE_ALIGN,
	ATTRIBUTE_PACKED,
	ATTRIBUTE_UNUSED,
	ATTRIBUTE_USED,
	ATTRIBUTE_NAKED,
	ATTRIBUTE_CDECL,
	ATTRIBUTE_STDCALL,
	ATTRIBUTE_VECCALL,
	ATTRIBUTE_REGCALL,
	ATTRIBUTE_FASTCALL,
	ATTRIBUTE_OVERLAP,
	ATTRIBUTE_NONE,
	NUMBER_OF_ATTRIBUTES = ATTRIBUTE_NONE,
} AttributeType;


typedef enum
{
	CALL_C,
	CALL_X86_STD,
	CALL_X86_FAST,
	CALL_X86_THIS,
	CALL_X86_VECTOR,
	CALL_X86_REG,
	CALL_AAPCS,
	CALL_AAPCS_VFP,
} CallABI;

typedef enum
{
	ANALYSIS_NOT_BEGUN,
	ANALYSIS_IMPORTS,
	ANALYSIS_REGISTER_GLOBALS,
	ANALYSIS_CONDITIONAL_COMPILATION,
	ANALYSIS_DECLS,
	ANALYSIS_CT_ASSERT,
	ANALYSIS_FUNCTIONS,
	ANALYSIS_LAST = ANALYSIS_FUNCTIONS
} AnalysisStage;