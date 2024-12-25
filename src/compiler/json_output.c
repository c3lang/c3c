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

void print_var_expr(FILE *file, Expr *expr);

void print_var_expr_unary(FILE *file, ExprUnary *expr, bool is_post)
{
    if (is_post) print_var_expr(file, expr->expr);
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
    if (!is_post) print_var_expr(file, expr->expr);
}

void print_var_expr_range(FILE *file, Range *r)
{
    if (r->is_len)
    {
        if (r->start_from_end) fputs("^", file);
        PRINTF("%d", r->start_index);
        fputs(" : ", file);
        if (r->end_from_end) fputs("^", file);
        PRINTF("%d", r->len_index);
    }
    else
    {
        switch (r->range_type)
        {
            case RANGE_DYNAMIC:
                if (r->start_from_end) fputs("^", file);
                print_var_expr(file, exprptr(r->start));
                fputs("..", file);
                if (r->end) print_var_expr(file, exprptr(r->end));
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
}

void print_var_expr(FILE *file, Expr *expr)
{
    if (!expr)
    {
        fputs("NULL EXPR", file);
        return;
    }

    switch (expr->expr_kind)
    {
        case EXPR_BITASSIGN:
        case EXPR_BITACCESS:
        case EXPR_ACCESS:
            print_var_expr(file, expr->access_expr.parent);
            fputs(".", file);
            print_var_expr(file, expr->access_expr.child);
            break;
        case EXPR_BINARY:
            print_var_expr(file, exprptr(expr->binary_expr.left));
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
                    fputs(" ?? ", file);
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
            print_var_expr(file, exprptr(expr->binary_expr.right));
            break;
        case EXPR_TYPECALL:
        case EXPR_CALL:
            {
                if (!expr->call_expr.is_pointer_call) print_var_expr(file, exprptr(expr->call_expr.function));
                fputs("(", file);
                FOREACH_IDX(i, Expr *, e, expr->call_expr.arguments)
                {
                    print_var_expr(file, e);
                    if (i != vec_size(expr->call_expr.arguments) - 1) fputs(", ", file);
                }
                fputs(")", file);
            }
            break;
        case EXPR_CAST:
            fputs("(", file);
            print_type(file, type_infoptr(expr->cast_expr.type_info));
            fputs(")", file);
            print_var_expr(file, exprptr(expr->cast_expr.expr));
            break;
        case EXPR_BUILTIN:
        case EXPR_COMPILER_CONST:
            PRINTF("$$%s", expr->builtin_expr.ident);
            break;
        case EXPR_COMPOUND_LITERAL:
            print_type(file, expr->expr_compound_literal.type_info);
            print_var_expr(file, expr->expr_compound_literal.initializer);
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
                    {
                        fputs("x\\\"", file);
                        ArraySize len = expr->const_expr.bytes.len;
                        const unsigned char *ptr = expr->const_expr.bytes.ptr;
                        char *res = malloc_string((size_t)(len * 3 + 1));
                        char *c = res;
                        for (ArraySize i = 0; i < len; ++i)
                        {
                            unsigned char curr = ptr[i];
                            char h = (curr & 0xF0) >> 4;
                            if (h > 0x09) *(c++) = h - 10 + 'A';
                            else *(c++) = h + '0';
                            char l = curr & 0x0F;
                            if (l > 0x09) *(c++) = l - 10 + 'A';
                            else *(c++) = l + '0';
                            *(c++) = ' ';
                        }
                        *(c - 1) = '\\';
                        *c = 0;
                        fputs(res, file);
                        fputs("\"", file);
                    }
                    break;
                case CONST_STRING:
                    {
                        fputs("\\\"", file);
                        ArraySize len = expr->const_expr.bytes.len;
                        const char *ptr = expr->const_expr.bytes.ptr;
                        char *res = malloc_string((size_t)(len * 6 + 1));
                        char *c = res;
                        for (ArraySize i = 0; i < len; ++i)
                        {
                            char curr = ptr[i];
                            if (curr <= 0x1F && curr >= 0x00)
                            {
                                *(c++) = '\\';
                                *(c++) = 'u';
                                *(c++) = '0';
                                *(c++) = '0';
                                *(c++) = ((curr & 0x10) >> 4) + '0';
                                char b = curr & 0x0F;
                                if (b > 0x09) *(c++) = b - 10 + 'A';
                                else *(c++) = b + '0';
                            }
                            else
                            {
                                if (curr == '\\' || curr == '\"') *(c++) = '\\';
                                *(c++) = curr;
                            }
                        }
                        *c = 0;
                        fputs(res, file);
                        fputs("\\\"", file);
                    }
                    break;
                case CONST_POINTER:
                    PRINTF("%llu", (unsigned long long) expr->const_expr.ptr);
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
                    fputs("{", file);
                    {
                        FOREACH_IDX(i, Expr *, e, expr->const_expr.untyped_list)
                        {
                            print_var_expr(file, e);
                            if (i != vec_size(expr->const_expr.untyped_list) - 1) fputs(", ", file);
                        }
                    }
                    fputs("}", file);
                    break;
                case CONST_REF:
                    fputs("TODO: CONST_REF", file);
                    break;
                case CONST_MEMBER:
                    fputs("TODO: CONST_MEMBER", file);
                    break;
            }
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
                    print_var_expr(file, e);
                    if (i != vec_size(expr->ct_and_or_expr.args) - 1) fputs(", ", file);
                }
                fputs(")", file);
            }
            break;
        case EXPR_CT_ARG:
            fputs("$", file);
            print_var_expr(file, exprptr(expr->ct_arg_expr.arg));
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
                case TOKEN_CT_ECHO:              // $echo
                    fputs("$echo(", file);
                    break;
                case TOKEN_CT_EMBED:             // $embed
                    fputs("$embed(", file);
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
                    fputs("$vaconst(", file);
                    break;
                case TOKEN_CT_VAREF:             // $varef,
                    fputs("$varef(", file);
                    break;
                case TOKEN_CT_VAARG:             // $vaarg,
                    fputs("$vaarg(", file);
                    break;
                case TOKEN_CT_VAEXPR:            // $vaexpr,
                    fputs("$vaexpr(", file);
                    break;
                default:
                    UNREACHABLE
            }
            print_var_expr(file, expr->ct_call_expr.main_var);
            fputs(")", file);
            break;
        case EXPR_CT_CASTABLE:
            fputs("$assignable(", file);
            print_var_expr(file, exprptr(expr->castable_expr.expr));
            fputs(", ", file);
            print_type(file, type_infoptr(expr->castable_expr.type));
            fputs(")", file);
            break;
        case EXPR_CT_APPEND:
        case EXPR_CT_CONCAT:
            {
                FOREACH_IDX(i, Expr *, e, expr->ct_concat)
                {
                    print_var_expr(file, e);
                    if (i != vec_size(expr->ct_concat) - 1) fputs(" +++ ", file);
                }
            }
            break;
        case EXPR_CT_DEFINED:
            fputs("$defined(", file);
            {
                FOREACH_IDX(i, Expr *, e, expr->expression_list)
                {
                    print_var_expr(file, e);
                    if (i != vec_size(expr->ct_concat) - 1) fputs(", ", file);
                }
            }
            fputs(")", file);
            break;
        case EXPR_CT_EVAL:
            fputs("$eval(", file);
            print_var_expr(file, expr->inner_expr);
            fputs(")", file);
            break;
        case EXPR_CT_IDENT:
            fputs(expr->ct_ident_expr.identifier, file);
            break;
        case EXPR_CT_IS_CONST:
            fputs("$is_const(", file);
            print_var_expr(file, expr->inner_expr);
            fputs(")", file);
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
                            print_var_expr(file, d->field_expr);
                            break; 
                        case DESIGNATOR_ARRAY:
                        case DESIGNATOR_RANGE:
                            fputs("[", file);
                            if (d->index_expr) print_var_expr(file, d->index_expr);
                            if (d->index_end_expr)
                            {
                                fputs("..", file);
                                print_var_expr(file, d->index_end_expr);
                            }
                            fputs("]", file);
                            break;
                    }
                }
                fputs(" = ", file);
                print_var_expr(file, expr->designator_expr.value);
            }
            break;
        case EXPR_EMBED:
            fputs("$embed(", file);
            print_var_expr(file, expr->embed_expr.filename);
            if (expr->embed_expr.len)
            {
                fputs(", ", file);
                print_var_expr(file, expr->embed_expr.len);
            }
            break;
        case EXPR_EXPRESSION_LIST:
            {
                FOREACH_IDX(i, Expr *, e, expr->expression_list)
                {
                    print_var_expr(file, e);
                    if (i != vec_size(expr->ct_concat) - 1) fputs(", ", file);
                }
            }
            break;
        case EXPR_EXPR_BLOCK:
            fputs("TODO: EXPR_EXPR_BLOCK", file);
            break;
        case EXPR_FORCE_UNWRAP:
            print_var_expr(file, expr->inner_expr);
            fputs("!!", file);
            break;
        case EXPR_GENERIC_IDENT:
            print_var_expr(file, exprptr(expr->generic_ident_expr.parent));
            fputs("(<", file);
            {
                FOREACH_IDX(i, Expr *, e, expr->generic_ident_expr.parmeters)
                {
                    print_var_expr(file, e);
                    if (i != vec_size(expr->call_expr.arguments) - 1) fputs(", ", file);
                }
            }
            fputs(">)", file);
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
                    print_var_expr(file, e);
                    if (i != vec_size(expr->initializer_list) - 1) fputs(", ", file);
                }
                fputs(" }", file);
            }
            break;
        case EXPR_LAMBDA:
            fputs("TODO: EXPR_LAMBDA", file);
            break;
        case EXPR_MACRO_BLOCK:
            fputs("TODO: EXPR_MACRO_BLOCK", file);
            break;
        case EXPR_MACRO_BODY_EXPANSION:
            fputs("TODO: EXPR_MACRO_BODY_EXPANSION", file);
            break;
        case EXPR_MEMBER_GET:
            fputs("TODO: EXPR_MEMBER_GET", file);
            break;
        case EXPR_NAMED_ARGUMENT:
            PRINTF("%s: ", expr->named_argument_expr.name);
            print_var_expr(file, expr->named_argument_expr.value);
            break;
        case EXPR_OPERATOR_CHARS:
            fputs("TODO: EXPR_OPERATOR_CHARS", file);
            break;
        case EXPR_OPTIONAL:
            print_var_expr(file, expr->inner_expr);
            fputs("?", file);
            break;
        case EXPR_OTHER_CONTEXT:
            fputs("TODO: EXPR_OTHER_CONTEXT", file);
            break;
        case EXPR_POINTER_OFFSET:
            fputs("TODO: EXPR_POINTER_OFFSET", file);
            break;
        case EXPR_POST_UNARY:
            print_var_expr_unary(file, &expr->unary_expr, true);
            break;
        case EXPR_RETHROW:
            print_var_expr(file, expr->rethrow_expr.inner);
            fputs("!", file);
            break;
        case EXPR_SLICE:
            {
                print_var_expr(file, exprptr(expr->slice_expr.expr));
                fputs("[", file);
                print_var_expr_range(file, &expr->slice_expr.range);
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
            fputs("$stringify(", file);
            print_var_expr(file, expr->inner_expr);
            fputs(")", file);
            break;
        case EXPR_SUBSCRIPT:
            fputs("TODO: EXPR_SUBSCRIPT", file);
            break;
        case EXPR_SUBSCRIPT_ADDR:
            fputs("TODO: EXPR_SUBSCRIPT_ADDR", file);
            break;
        case EXPR_SUBSCRIPT_ASSIGN:
			UNREACHABLE
        case EXPR_SWIZZLE:
            fputs("TODO: EXPR_SWIZZLE", file);
            break;
        case EXPR_TERNARY:
            {
                print_var_expr(file, exprptr(expr->ternary_expr.cond));
                fputs(" ? ", file);
                Expr *then = exprptr(expr->ternary_expr.then_expr);
                if (then) print_var_expr(file, then);
                fputs(" : ", file);
                print_var_expr(file, exprptr(expr->ternary_expr.else_expr));
            }
            break;
        case EXPR_TYPEID:
            print_type(file, expr->typeid_expr);
            break;
        case EXPR_TYPEID_INFO:
            print_var_expr(file, exprptr(expr->typeid_info_expr.parent));
            break;
        case EXPR_TYPEINFO:
            print_type(file, expr->type_expr);
            break;
        case EXPR_UNARY:
            print_var_expr_unary(file, &expr->unary_expr, false);
            break;
        case EXPR_VASPLAT:
            fputs("$vasplat[", file);
            print_var_expr_range(file, &expr->vasplat_expr);
            fputs("]", file);
            break;
        case EXPR_NOP:
        case EXPR_ASM:
        case EXPR_COND:
        case EXPR_RETVAL:
            // if not in doc lexing "mode", this is unreachable
        case EXPR_POISONED:
        case EXPR_TEST_HOOK:
        case EXPR_ANYSWITCH:
        case EXPR_TRY_UNWRAP:
        case EXPR_LAST_FAULT:
        case EXPR_MACRO_BODY:
            // nested inside EXPR_CALL, avoid macro substitution
        case EXPR_DEFAULT_ARG:
        case EXPR_CATCH_UNWRAP:
        case EXPR_BENCHMARK_HOOK:
        case EXPR_BUILTIN_ACCESS:
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
	fputs("\t\"globals\": {\n", file);
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
                    if (decl->var.init_expr) print_var_expr(file, decl->var.init_expr);
                    fputs("\"\n\t\t}", file);
		FOREACH_DECL_END;
	}
	fputs("\n\t},\n", file);
}

static inline void emit_constants(FILE *file)
{
	fputs("\t\"constants\": {\n", file);
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
                    print_var_expr(file, decl->var.init_expr);
                    fputs("\"\n\t\t}", file);
		FOREACH_DECL_END;
	}
	fputs("\n\t}", file);
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
