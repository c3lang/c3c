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


void print_var_expr(FILE *file, Expr *expr)
{
	scratch_buffer_clear();
	span_to_scratch(expr->span);
	const char *str = scratch_buffer_to_string();
	while (*str != 0)
	{
		char c = *str;
		switch (c)
		{
			case '\r':
				break;
			case '\\':
				fputs("\\\\", file);
				break;
			case '\n':
				fputs("\\n", file);
				break;
			case '\"':
				fputs("\\\"", file);
				break;
			default:
				fputc(c, file);
				break;
		}
		++str;
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
