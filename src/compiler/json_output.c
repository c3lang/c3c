#include "compiler_internal.h"

#define FOREACH_DECL(a__, modules__) \
    unsigned module_count_ = vec_size(modules__); \
	for (unsigned i_ = 0; i_ < module_count_; i_++) { \
    Module *module = modules__[i_]; \
    unsigned unit_count_ = vec_size(module->units); \
	for (unsigned j_ = 0; j_ < unit_count_; j_++) { \
	CompilationUnit *unit = module->units[j_]; \
	unsigned decl_count_ = vec_size(unit->global_decls); \
	for (unsigned k_ = 0; k_ < decl_count_; k_++) { \
	a__ = unit->global_decls[k_];
#define PRINTF(string, ...) fprintf(file, string, ##__VA_ARGS__) /* NOLINT */
#define FOREACH_DECL_END } } }
#define INSERT_COMMA do { if (first) { first = false; } else { fputs(",\n", file); } } while(0)

static inline void emit_modules(FILE *file)
{

	fputs("\t\"modules\": [\n", file);
	FOREACH_IDX(i, Module *, module, compiler.context.module_list)
	{
		if (i != 0) fputs(",\n", file);
		PRINTF("\t\t\"%s\"", module->name->module);
	}
	fputs("\n\t],\n", file);
	fputs("\t\"generic_modules\": [\n", file);
	FOREACH_IDX(j, Module *, module, compiler.context.generic_module_list)
	{
		if (j != 0) fputs(",\n", file);
		PRINTF("\t\t\"%s\"", module->name->module);
	}
	fputs("\n\t],\n", file);
}

static inline const char *decl_type_to_string(Decl *type)
{
	switch (type->decl_kind)
	{
		case DECL_ATTRIBUTE: return "attribute";
		case DECL_BITSTRUCT: return "bitstruct";
		case DECL_CT_ASSERT: return "$assert";
		case DECL_CT_ECHO: return "$echo";
		case DECL_CT_EXEC: return "$exec";
		case DECL_CT_INCLUDE: return "$include";
		case DECL_DEFINE: return "def";
		case DECL_DISTINCT: return "distinct";
		case DECL_ENUM: return "enum";
		case DECL_ENUM_CONSTANT: return "enum_const";
		case DECL_FAULT: return "fault";
		case DECL_FAULTVALUE: return "fault_val";
		case DECL_FNTYPE: return "fntype";
		case DECL_FUNC: return "function";
		case DECL_GLOBALS: return "global";
		case DECL_IMPORT: return "import";
		case DECL_MACRO: return "macro";
		case DECL_INTERFACE: return "interface";
		case DECL_STRUCT: return "struct";
		case DECL_UNION: return "union";
 		case DECL_TYPEDEF: return "typedef";
		case DECL_BODYPARAM:
		case DECL_DECLARRAY:
		case DECL_ERASED:
		case DECL_LABEL:
		case DECL_POISONED:
		case DECL_VAR:
			UNREACHABLE
	}
	UNREACHABLE
}

void print_type(FILE *file, TypeInfo *type)
{
	if (type->resolve_status == RESOLVE_DONE)
	{
		fputs(type->type->name, file);
		return;
	}
	switch (type->kind)
	{
		case TYPE_INFO_POISON:
			UNREACHABLE;
		case TYPE_INFO_IDENTIFIER:
		case TYPE_INFO_CT_IDENTIFIER:
			if (type->unresolved.path)
			{
				PRINTF("%s::", type->unresolved.path->module);
			}
			fputs(type->unresolved.name, file);
			break;
		case TYPE_INFO_TYPEOF:
			scratch_buffer_clear();
			span_to_scratch(type->unresolved_type_expr->span);
			PRINTF("$typeof(%s)", scratch_buffer_to_string());
			break;
		case TYPE_INFO_VATYPE:
			PRINTF("$vatype[...]");
			break;
		case TYPE_INFO_EVALTYPE:
			PRINTF("$evaltype(...)");
			break;
		case TYPE_INFO_TYPEFROM:
			PRINTF("$typefrom(...)");
			break;
		case TYPE_INFO_ARRAY:
			print_type(file, type->array.base);
			scratch_buffer_clear();
			span_to_scratch(type->array.len->span);
			PRINTF("[%s]", scratch_buffer_to_string());
			break;
		case TYPE_INFO_VECTOR:
			print_type(file, type->array.base);
			scratch_buffer_clear();
			span_to_scratch(type->array.len->span);
			PRINTF("[<%s>]", scratch_buffer_to_string());
			break;
		case TYPE_INFO_INFERRED_ARRAY:
			print_type(file, type->array.base);
			fputs("[*]", file);
			break;
		case TYPE_INFO_INFERRED_VECTOR:
			print_type(file, type->array.base);
			fputs("[<>]", file);
			break;
		case TYPE_INFO_SLICE:
			print_type(file, type->array.base);
			fputs("[]", file);
			break;
		case TYPE_INFO_POINTER:
			print_type(file, type->array.base);
			fputs("*", file);
			break;
		case TYPE_INFO_GENERIC:
			print_type(file, type->array.base);
			fputs("(<...>)", file);
			break;
	}
	switch (type->subtype)
	{
		case TYPE_COMPRESSED_NONE:
			break;
		case TYPE_COMPRESSED_PTR:
			fputs("*", file);
			break;
		case TYPE_COMPRESSED_SUB:
			fputs("[]", file);
			break;
		case TYPE_COMPRESSED_SUBPTR:
			fputs("[]*", file);
			break;
		case TYPE_COMPRESSED_PTRPTR:
			fputs("**", file);
			break;
		case TYPE_COMPRESSED_PTRSUB:
			fputs("*[]", file);
			break;
		case TYPE_COMPRESSED_SUBSUB:
			fputs("[][]", file);
			break;
	}
}

void print_expr(FILE *file, Expr *expr);

void print_exprunary(FILE *file, ExprUnary *expr, bool is_post)
{
    if (is_post) print_expr(file, expr->expr);
    switch (expr->operator)
    {
        case UNARYOP_ERROR:
            UNREACHABLE
        case UNARYOP_DEREF:
            fputs("*", file);
            break;
        case UNARYOP_ADDR:
            fputs("&", file);
            break;
        case UNARYOP_NEG:
            fputs("-", file);
            break;
        case UNARYOP_PLUS:
            fputs("+", file);
            break;
        case UNARYOP_BITNEG:
            fputs("~", file);
            break;
        case UNARYOP_NOT:
            fputs("!", file);
            break;
        case UNARYOP_INC:
            fputs("++", file);
            break;
        case UNARYOP_DEC:
            fputs("--", file);
            break;
        case UNARYOP_TADDR:
            fputs("&&", file);
            break;
    }
    if (!is_post) print_expr(file, expr->expr);
}

void print_expr(FILE *file, Expr *expr)
{
    if (!expr)
    {
        fputs("NULL EXPR", file);
        return;
    }

    switch (expr->expr_kind)
    {
        case EXPR_ACCESS:
            print_expr(file, expr->access_expr.parent);
            fputs(".", file);
            print_expr(file, expr->access_expr.child);
            break;
        case EXPR_ANYSWITCH:
            fputs("TODO: EXPR_ANYSWITCH", file);
            break;
        case EXPR_ASM:
            fputs("TODO: EXPR_ASM", file);
            break;
        case EXPR_BENCHMARK_HOOK:
            fputs("TODO: EXPR_BENCHMARK_HOOK", file);
            break;
        case EXPR_BINARY:
            print_expr(file, exprptr(expr->binary_expr.left));
            switch (expr->binary_expr.operator)
            {
                case BINARYOP_ERROR:
                    UNREACHABLE
                case BINARYOP_MULT:
                    fputs(" * ", file);
                    break;
                case BINARYOP_SUB:
                    fputs(" - ", file);
                    break;
                case BINARYOP_ADD:
                    fputs(" + ", file);
                    break;
                case BINARYOP_DIV:
                    fputs(" / ", file);
                    break;
                case BINARYOP_MOD:
                    fputs(" % ", file);
                    break;
                case BINARYOP_SHR:
                    fputs(" >> ", file);
                    break;
                case BINARYOP_SHL:
                    fputs(" << ", file);
                    break;
                case BINARYOP_BIT_OR:
                    fputs(" | ", file);
                    break;
                case BINARYOP_BIT_XOR:
                    fputs(" ^ ", file);
                    break;
                case BINARYOP_BIT_AND:
                    fputs(" & ", file);
                    break;
                case BINARYOP_AND:
                case BINARYOP_CT_AND:
                    fputs(" && ", file);
                    break;
                case BINARYOP_OR:
                case BINARYOP_CT_OR:
                    fputs(" || ", file);
                    break;
                case BINARYOP_ELSE:
                    fputs(" : ", file);
                    break;
                case BINARYOP_CT_CONCAT:
                    fputs(" +++ ", file);
                    break;
                case BINARYOP_GT:
                    fputs(" > ", file);
                    break;
                case BINARYOP_GE:
                    fputs(" >= ", file);
                    break;
                case BINARYOP_LT:
                    fputs(" < ", file);
                    break;
                case BINARYOP_LE:
                    fputs(" <= ", file);
                    break;
                case BINARYOP_NE:
                    fputs(" != ", file);
                    break;
                case BINARYOP_EQ:
                    fputs(" == ", file);
                    break;
                case BINARYOP_ASSIGN:
                    fputs(" = ", file);
                    break;
                case BINARYOP_ADD_ASSIGN:
                    fputs(" += ", file);
                    break;
                case BINARYOP_BIT_AND_ASSIGN:
                    fputs(" &= ", file);
                    break;
                case BINARYOP_BIT_OR_ASSIGN:
                    fputs(" |= ", file);
                    break;
                case BINARYOP_BIT_XOR_ASSIGN:
                    fputs(" ^= ", file);
                    break;
                case BINARYOP_DIV_ASSIGN:
                    fputs(" /= ", file);
                    break;
                case BINARYOP_MOD_ASSIGN:
                    fputs(" %= ", file);
                    break;
                case BINARYOP_MULT_ASSIGN:
                    fputs(" *= ", file);
                    break;
                case BINARYOP_SHR_ASSIGN:
                    fputs(" >>= ", file);
                    break;
                case BINARYOP_SHL_ASSIGN:
                    fputs(" <<= ", file);
                    break;
                case BINARYOP_SUB_ASSIGN:
                    fputs(" -= ", file);
                    break;
            }
            print_expr(file, exprptr(expr->binary_expr.right));
            break;
        case EXPR_BITACCESS:
            fputs("TODO: EXPR_BITACCESS", file);
            break;
        case EXPR_BITASSIGN:
            fputs("TODO: EXPR_BITASSIGN", file);
            break;
        case EXPR_BUILTIN_ACCESS:
            fputs("TODO: EXPR_BUILTIN_ACCESS", file);
            break;
        case EXPR_CALL:
            {
                if (!expr->call_expr.is_pointer_call) print_expr(file, exprptr(expr->call_expr.function));
                fputs("(", file);
                FOREACH_IDX(i, Expr *, e, expr->call_expr.arguments)
                {
                    print_expr(file, e);
                    if (i != vec_size(expr->call_expr.arguments) - 1) fputs(", ", file);
                }
                fputs(")", file);
            }
            break;
        case EXPR_CAST:
            fputs("(", file);
            print_type(file, type_infoptr(expr->cast_expr.type_info));
            fputs(")", file);
            print_expr(file, exprptr(expr->cast_expr.expr));
            break;
        case EXPR_BUILTIN:
        case EXPR_COMPILER_CONST:
            PRINTF("$$%s", expr->builtin_expr.ident);
            break;
        case EXPR_COMPOUND_LITERAL:
            print_type(file, expr->expr_compound_literal.type_info);
            print_expr(file, expr->expr_compound_literal.initializer);
            break;
        case EXPR_CONST:
            switch (expr->const_expr.const_kind)
            {
                case CONST_FLOAT:
                    PRINTF("%f", expr->const_expr.fxx.f);
                    break;
                case CONST_INTEGER:
                    fputs(i128_to_string(expr->const_expr.ixx.i, 10, true, false), file);
                    break;
                case CONST_BOOL:
                    fputs(expr->const_expr.b ? "true" : "false", file);
                    break;
                case CONST_ENUM:
                    PRINTF("%u", expr->const_expr.enum_err_val->enum_constant.ordinal);
                    break;
                case CONST_ERR:
                    fputs("TODO: CONST_ERR", file);
                    break;
                case CONST_BYTES:
                case CONST_STRING:
                    PRINTF("\\\"%.*s\\\"", expr->const_expr.bytes.len, expr->const_expr.bytes.ptr);
                    break;
                case CONST_POINTER:
                    PRINTF("%lld", expr->const_expr.ptr);
                    break;
                case CONST_TYPEID:
                    fputs(expr->const_expr.typeid->name, file);
                    break;
                case CONST_SLICE:
                    fputs("TODO: CONST_SLICE", file);
                    break;
                case CONST_INITIALIZER:
                    fputs("TODO: CONST_INITIALIZER", file);
                    break;
                case CONST_UNTYPED_LIST:
                    fputs("TODO: CONST_UNTYPED_LIST", file);
                    break;
                case CONST_REF:
                    fputs("TODO: CONST_REF", file);
                    break;
                case CONST_MEMBER:
                    fputs("TODO: CONST_MEMBER", file);
                    break;
            }
            break;
        case EXPR_TYPECALL:
            fputs("TODO: EXPR_TYPECALL", file);
            break;
        case EXPR_CT_AND_OR:
            {
                if (expr->ct_and_or_expr.is_and)
                {
                    fputs("$and(", file);
                }
                else
                {
                    fputs("$or(", file);
                }
                FOREACH_IDX(i, Expr *, e, expr->ct_and_or_expr.args)
                {
                    print_expr(file, e);
                    if (i != vec_size(expr->ct_and_or_expr.args) - 1) fputs(", ", file);
                }
                fputs(")", file);
            }
            break;
        case EXPR_CT_ARG:
            fputs("$", file);
            print_expr(file, exprptr(expr->ct_arg_expr.arg));
            break;
        case EXPR_CT_APPEND:
            fputs("$append", file);
            break;
        case EXPR_CT_CALL:
            switch (expr->ct_call_expr.token_type)
            {
                case TOKEN_CT_ALIGNOF:           // $alignof
                    fputs("$alignof(", file);
                    break;
                case TOKEN_CT_APPEND:            // $append
                    fputs("$append(", file);
                    break;
                case TOKEN_CT_ASSERT:            // $assert
                    fputs("$assert(", file);
                    break;
                case TOKEN_CT_ASSIGNABLE:        // $assignable
                    fputs("$assignable(", file);
                    break;
                case TOKEN_CT_DEFINED:           // $defined
                    fputs("$defined(", file);
                    break;
                case TOKEN_CT_ECHO:              // $echo
                    fputs("$echo(", file);
                    break;
                case TOKEN_CT_EMBED:             // $embed
                    fputs("$embed(", file);
                    break;
                case TOKEN_CT_EVAL:              // $eval
                    fputs("$eval(", file);
                    break;
                case TOKEN_CT_EVALTYPE:          // $evaltype
                    fputs("$evaltype(", file);
                    break;
                case TOKEN_CT_ERROR:             // $error
                    fputs("$error(", file);
                    break;
                case TOKEN_CT_EXEC:              // $exec
                    fputs("$exec(", file);
                    break;
                case TOKEN_CT_EXTNAMEOF:         // $extnameof
                    fputs("$extnameof(", file);
                    break;
                case TOKEN_CT_FEATURE:           // $feature
                    fputs("$feature(", file);
                    break;
                case TOKEN_CT_IS_CONST:          // $is_const
                    fputs("$is_const(", file);
                    break;
                case TOKEN_CT_NAMEOF:            // $nameof
                    fputs("$nameof(", file);
                    break;
                case TOKEN_CT_OFFSETOF:          // $offsetof
                    fputs("$offsetof(", file);
                    break;
                case TOKEN_CT_QNAMEOF:           // $qnameof
                    fputs("$qnameof(", file);
                    break;
                case TOKEN_CT_SIZEOF:            // $sizeof
                    fputs("$sizeof(", file);
                    break;
                case TOKEN_CT_STRINGIFY:         // $stringify
                    fputs("$stringify(", file);
                    break;
                case TOKEN_CT_TYPEFROM:          // $typefrom
                    fputs("$typefrom(", file);
                    break;
                case TOKEN_CT_TYPEOF:            // $typeof
                    fputs("$typeof(", file);
                    break;
                case TOKEN_CT_VACOUNT:           // $vacount
                    fputs("$vacount(", file);
                    break;
                case TOKEN_CT_VATYPE:            // $vatype
                    fputs("$vatype(", file);
                    break;
                case TOKEN_CT_VACONST:           // $vaconst,
                    fputs("$vaconst(,", file);
                    break;
                case TOKEN_CT_VAREF:             // $varef,
                    fputs("$varef(,", file);
                    break;
                case TOKEN_CT_VAARG:             // $vaarg,
                    fputs("$vaarg(,", file);
                    break;
                case TOKEN_CT_VAEXPR:            // $vaexpr,
                    fputs("$vaexpr(,", file);
                    break;
                case TOKEN_CT_VASPLAT:           // $vasplat,
                    fputs("$vasplat(,", file);
                    break;
                default:
                    UNREACHABLE
            }
            print_expr(file, expr->ct_call_expr.main_var);
            fputs(")", file);
            break;
        case EXPR_CT_CASTABLE:
            fputs("TODO: EXPR_CT_CASTABLE", file);
            break;
        case EXPR_CT_CONCAT:
            {
                FOREACH_IDX(i, Expr *, e, expr->ct_concat)
                {
                    print_expr(file, e);
                    if (i != vec_size(expr->ct_concat) - 1) fputs(" +++ ", file);
                }
            }
            break;
        case EXPR_CT_DEFINED:
            fputs("TODO: EXPR_CT_DEFINED", file);
            break;
        case EXPR_CT_EVAL:
            fputs("$eval", file);
            break;
        case EXPR_CT_IDENT:
            fputs(expr->ct_ident_expr.identifier, file);
            break;
        case EXPR_CT_IS_CONST:
            fputs("$is_const", file);
            break;
        case EXPR_DECL:
            fputs("TODO: EXPR_DECL", file);
            break;
        case EXPR_DESIGNATOR:
            {
                FOREACH(DesignatorElement *, d, expr->designator_expr.path)
                {
                    switch (d->kind)
                    {
                        case DESIGNATOR_FIELD:
                            fputs(".", file);
                            print_expr(file, d->field_expr);
                            break; 
                        case DESIGNATOR_ARRAY:
                        case DESIGNATOR_RANGE:
                            fputs("[", file);
                            if (d->index_expr) print_expr(file, d->index_expr);
                            if (d->index_end_expr)
                            {
                                fputs("..", file);
                                print_expr(file, d->index_end_expr);
                            }
                            fputs("]", file);
                            break;
                    }
                }
                fputs(" = ", file);
                print_expr(file, expr->designator_expr.value);
            }
            break;
        case EXPR_EMBED:
            fputs("$embed(", file);
            print_expr(file, expr->embed_expr.filename);
            if (expr->embed_expr.len)
            {
                fputs(", ", file);
                print_expr(file, expr->embed_expr.len);
            }
            break;
        case EXPR_EXPRESSION_LIST:
            fputs("TODO: EXPR_EXPRESSION_LIST", file);
            break;
        case EXPR_EXPR_BLOCK:
            fputs("TODO: EXPR_EXPR_BLOCK", file);
            break;
        case EXPR_FORCE_UNWRAP:
            fputs("TODO: EXPR_FORCE_UNWRAP", file);
            break;
        case EXPR_GENERIC_IDENT:
            fputs("TODO: EXPR_GENERIC_IDENT", file);
            break;
        case EXPR_HASH_IDENT:
            PRINTF("#%s", expr->hash_ident_expr.identifier);
            break;
        case EXPR_IDENTIFIER:
            if (expr->identifier_expr.path)
            {
                if (expr->identifier_expr.path->module)
                {
                    PRINTF("%s::", expr->identifier_expr.path->module);
                }
            }
            if (expr->identifier_expr.ident)
            {
                fputs(expr->identifier_expr.ident, file);
            }
        break;
        case EXPR_DESIGNATED_INITIALIZER_LIST:
        case EXPR_INITIALIZER_LIST:
            {
                fputs("{ ", file);
                FOREACH_IDX(i, Expr *, e, expr->initializer_list)
                {
                    print_expr(file, e);
                    if (i != vec_size(expr->initializer_list) - 1) fputs(", ", file);
                }
                fputs(" }", file);
            }
            break;
        case EXPR_LAMBDA:
            fputs("TODO: EXPR_LAMBDA", file);
            break;
        case EXPR_LAST_FAULT:
            fputs("TODO: EXPR_LAST_FAULT", file);
            break;
        case EXPR_MACRO_BLOCK:
            fputs("TODO: EXPR_MACRO_BLOCK", file);
            break;
        case EXPR_MACRO_BODY:
            fputs("TODO: EXPR_MACRO_BODY", file);
            break;
        case EXPR_MACRO_BODY_EXPANSION:
            fputs("TODO: EXPR_MACRO_BODY_EXPANSION", file);
            break;
        case EXPR_MEMBER_GET:
            fputs("TODO: EXPR_MEMBER_GET", file);
            break;
        case EXPR_NAMED_ARGUMENT:
            PRINTF("%s: ", expr->named_argument_expr.name);
            print_expr(file, expr->named_argument_expr.value);
            break;
        case EXPR_NOP:
            fputs("TODO: EXPR_NOP", file);
            break;
        case EXPR_OPERATOR_CHARS:
            fputs("TODO: EXPR_OPERATOR_CHARS", file);
            break;
        case EXPR_OPTIONAL:
            fputs("TODO: EXPR_OPTIONAL", file);
            break;
        case EXPR_OTHER_CONTEXT:
            fputs("TODO: EXPR_OTHER_CONTEXT", file);
            break;
        case EXPR_POINTER_OFFSET:
            fputs("TODO: EXPR_POINTER_OFFSET", file);
            break;
        case EXPR_POST_UNARY:
            print_exprunary(file, &expr->unary_expr, true);
            break;
        case EXPR_RETHROW:
            fputs("TODO: EXPR_RETHROW", file);
            break;
        case EXPR_RETVAL:
            fputs("TODO: EXPR_RETVAL", file);
            break;
        case EXPR_SLICE:
            {
                print_expr(file, exprptr(expr->slice_expr.expr));
                fputs("[", file);
                if (expr->slice_expr.range.is_len)
                {
                    if (expr->slice_expr.range.start_from_end) fputs("^", file);
                    PRINTF("%d", expr->slice_expr.range.start_index);
                    fputs(" : ", file);
                    if (expr->slice_expr.range.end_from_end) fputs("^", file);
                    PRINTF("%d", expr->slice_expr.range.len_index);
                }
                else
                {
                    switch (expr->slice_expr.range.range_type)
                    {
                        case RANGE_DYNAMIC:
                            if (expr->slice_expr.range.start_from_end) fputs("^", file);
                            print_expr(file, exprptr(expr->slice_expr.range.start));
                            fputs("..", file);
                            if (expr->slice_expr.range.end) print_expr(file, exprptr(expr->slice_expr.range.end));
                            break;
                        case RANGE_CONST_END:
                            fputs("TODO: RANGE_CONST_END", file);
                            break;
                        case RANGE_CONST_LEN:
                            fputs("TODO: RANGE_CONST_LEN", file);
                            break;
                        case RANGE_CONST_RANGE:
                            fputs("TODO: RANGE_CONST_RANGE", file);
                            break;
                    }
                }
                fputs("]", file);
            }
            break;
        case EXPR_SLICE_ASSIGN:
            fputs("TODO: EXPR_SLICE_ASSIGN", file);
            break;
        case EXPR_SLICE_COPY:
            fputs("TODO: EXPR_SLICE_COPY", file);
            break;
        case EXPR_SPLAT:
            fputs("TODO: EXPR_SPLAT", file);
            break;
        case EXPR_STRINGIFY:
            fputs("$stringify", file);
            break;
        case EXPR_SUBSCRIPT:
            fputs("TODO: EXPR_SUBSCRIPT", file);
            break;
        case EXPR_SUBSCRIPT_ADDR:
            fputs("TODO: EXPR_SUBSCRIPT_ADDR", file);
            break;
        case EXPR_SUBSCRIPT_ASSIGN:
            fputs("TODO: EXPR_SUBSCRIPT_ASSIGN", file);
            break;
        case EXPR_SWIZZLE:
            fputs("TODO: EXPR_SWIZZLE", file);
            break;
        case EXPR_TERNARY:
            {
                print_expr(file, exprptr(expr->ternary_expr.cond));
                fputs(" ? ", file);
                Expr *then = exprptr(expr->ternary_expr.then_expr);
                if (then) print_expr(file, then);
                fputs(" : ", file);
                print_expr(file, exprptr(expr->ternary_expr.else_expr));
            }
            break;
        case EXPR_TEST_HOOK:
            fputs("TODO: EXPR_TEST_HOOK", file);
            break;
        case EXPR_TYPEID:
            fputs("TODO: EXPR_TYPEID", file);
            break;
        case EXPR_TYPEID_INFO:
            fputs("TODO: EXPR_TYPEID_INFO", file);
            break;
        case EXPR_TYPEINFO:
            print_type(file, expr->type_expr);
            break;
        case EXPR_UNARY:
            print_exprunary(file, &expr->unary_expr, false);
            break;
        case EXPR_VASPLAT:
            fputs("TODO: EXPR_VASPLAT", file);
            break;
        case EXPR_COND:
        case EXPR_POISONED:
        case EXPR_TRY_UNWRAP:
        case EXPR_DEFAULT_ARG:
        case EXPR_CATCH_UNWRAP:
        case EXPR_TRY_UNWRAP_CHAIN:
            UNREACHABLE
    }
}

static inline void emit_type_data(FILE *file, Module *module, Decl *type)
{
	PRINTF("\t\t\"%s::%s\": {\n", module->name->module, type->name);
	PRINTF("\t\t\t\"kind\": \"%s\"", decl_type_to_string(type));
	if (type->decl_kind == DECL_STRUCT || type->decl_kind == DECL_UNION)
	{
		fputs(",\n\t\t\t\"members\": [\n", file);
		FOREACH_IDX(i, Decl *, member, type->strukt.members)
		{
			if (i != 0) fputs(",\n", file);
			PRINTF("\t\t\t\t{\n");
			if (member->name)
			{
				PRINTF("\t\t\t\t\t\"name\": \"%s\",\n", member->name);
			}
			// TODO, extend this
			PRINTF("\t\t\t\t\t\"type\": \"");
			if (member->var.type_info)
			{
				print_type(file, type_infoptr(member->var.type_info));
			}
			else
			{
				fputs("", file);
			}
			PRINTF("\"\n\t\t\t\t}");
		}
		fputs("\n\t\t\t]", file);
	}
	fputs("\n\t\t}", file);
}

static inline void emit_func_data(FILE *file, Module *module, Decl *func)
{
	PRINTF("\t\t\"%s::%s\": {\n", module->name->module, func->name);
	PRINTF("\t\t\t\"rtype\": \"");
	print_type(file, type_infoptr(func->func_decl.signature.rtype));
	PRINTF("\",\n");
	fputs("\t\t\t\"params\": [\n", file);
	FOREACH_IDX(i, Decl *, decl, func->func_decl.signature.params)
	{
		if (!decl) continue;
		if (i != 0) fputs(",\n", file);
		fputs("\t\t\t\t{\n", file);
		PRINTF("\t\t\t\t\t\"name\": \"%s\",\n", decl->name ? decl->name : "");
		PRINTF("\t\t\t\t\t\"type\": \"");
		if (decl->var.type_info)
		{
			print_type(file, type_infoptr(decl->var.type_info));
		}
		else
		{
			fputs("", file);
		}
		fputs("\"\n", file);
		fputs("\t\t\t\t}", file);
	}
	fputs("\n\t\t\t]\n", file);

	fputs("\n\t\t}", file);
}

static inline bool decl_is_hidden(Decl *decl)
{
	return decl->visibility > VISIBLE_PUBLIC;
}

static inline void emit_types(FILE *file)
{
	fputs("\t\"types\": {\n", file);
	{
		bool first = true;
		FOREACH_DECL(Decl *type, compiler.context.module_list)
					if (!decl_is_user_defined_type(type) && type->decl_kind != DECL_TYPEDEF) continue;
					if (decl_is_hidden(type)) continue;
					INSERT_COMMA;
					emit_type_data(file, module, type);
		FOREACH_DECL_END;
	}

	fputs("\n\t},\n", file);
	fputs("\t\"generic_types\": {\n", file);
	{
		bool first = true;
		FOREACH_DECL(Decl *type, compiler.context.generic_module_list)
					if (!decl_is_user_defined_type(type) && type->decl_kind != DECL_TYPEDEF) continue;
					if (decl_is_hidden(type)) continue;
					INSERT_COMMA;
					emit_type_data(file, module, type);
		FOREACH_DECL_END;
	}
	fputs("\n\t},\n", file);
}

static inline void emit_globals(FILE *file)
{
	fputs("\t\"globals\": [\n", file);
	{
		bool first = true;
		FOREACH_DECL(Decl *decl, compiler.context.module_list)
					if (decl->decl_kind != DECL_VAR || decl->var.kind != VARDECL_GLOBAL) continue;
					if (decl_is_hidden(decl)) continue;
					INSERT_COMMA;
                    PRINTF("\t\t\"%s::%s\": {\n", module->name->module, decl->name);
                    fputs("\t\t\t\"type\": \"", file);
                    if (decl->var.type_info)
                    {
                        print_type(file, type_infoptr(decl->var.type_info));
                    }
                    else
                    {
                        fputs("", file);
                    }
                    fputs("\",\n", file);
                    fputs("\t\t\t\"value\": \"", file);
                    if (decl->var.init_expr) print_expr(file, decl->var.init_expr);
                    fputs("\"\n\t\t}", file);
		FOREACH_DECL_END;
	}
	fputs("\n\t],\n", file);
}

static inline void emit_constants(FILE *file)
{
	fputs("\t\"constants\": [\n", file);
	{
		bool first = true;
		FOREACH_DECL(Decl *decl, compiler.context.module_list)
                    if (decl->decl_kind != DECL_VAR || decl->var.kind != VARDECL_CONST) continue;
					if (decl_is_hidden(decl)) continue;
					INSERT_COMMA;
                    PRINTF("\t\t\"%s::%s\": {\n", module->name->module, decl->name);
                    fputs("\t\t\t\"type\": \"", file);
                    if (decl->var.type_info)
                    {
                        print_type(file, type_infoptr(decl->var.type_info));
                    }
                    else
                    {
                        fputs("", file);
                    }
                    fputs("\",\n", file);
                    fputs("\t\t\t\"value\": \"", file);
                    print_expr(file, decl->var.init_expr);
                    fputs("\"\n\t\t}", file);
		FOREACH_DECL_END;
	}
	fputs("\n\t]", file);
}

static inline void emit_functions(FILE *file)
{
	fputs("\t\"functions\": {\n", file);
	{
		bool first = true;
		FOREACH_DECL(Decl *func, compiler.context.module_list)
					if (func->decl_kind != DECL_FUNC) continue;
					if (decl_is_hidden(func)) continue;
					INSERT_COMMA;
					emit_func_data(file, module, func);
		FOREACH_DECL_END;
	}
	fputs("\n\t},\n", file);

	fputs("\t\"generic_functions\": {\n", file);
	{
		bool first = true;
		FOREACH_DECL(Decl *func, compiler.context.generic_module_list)
					if (func->decl_kind != DECL_FUNC) continue;
					if (decl_is_hidden(func)) continue;
					INSERT_COMMA;
					emit_func_data(file, module, func);
		FOREACH_DECL_END;
	}
	fputs("\n\t},\n", file);
}

static inline void emit_json_to_file(FILE *file)
{
	fputs("{\n", file);
	emit_modules(file);
	emit_types(file);
	emit_functions(file);
	emit_globals(file);
	emit_constants(file);
	fputs("\n}", file);
}

void emit_json(void)
{
	emit_json_to_file(stdout);
}
