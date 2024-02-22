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

#define FOREACH_DECL_END } } }
#define INERT_COMMA do { if (first) { first = false; } else { fputs(",\n", file); } } while(0)

static inline void emit_modules(FILE *file)
{

	fputs("\t\"modules\": {\n", file);
	FOREACH_BEGIN_IDX(i, Module *module, global_context.module_list)
		if (i != 0) fputs(",\n", file);
		fprintf(file, "\t\t\"%s\"", module->name->module);
	FOREACH_END();
	fputs("\n\t},\n", file);
	fputs("\t\"generic_modules\": {\n", file);
	FOREACH_BEGIN_IDX(i, Module *module, global_context.generic_module_list)
		if (i != 0) fputs(",\n", file);
		fprintf(file, "\t\t\"%s\"", module->name->module);
	FOREACH_END();
	fputs("\n\t}\n", file);
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
static inline void emit_type_data(FILE *file, Module *module, Decl *type)
{
	fprintf(file, "\t\t\"%s::%s\": {\n", module->name->module, type->name);
	fprintf(file, "\t\t\t\"kind\": \"%s\"", decl_type_to_string(type));
	if (type->decl_kind == DECL_STRUCT || type->decl_kind == DECL_UNION)
	{
		fputs(",\n\t\t\t\"members\": {\n", file);
		FOREACH_BEGIN_IDX(i, Decl *member, type->strukt.members)
			if (i != 0) fputs(",\n", file);
			fprintf(file, "\t\t\t\t\"%s\"", member->name);
		FOREACH_END();
		fputs("\n\t\t\t}", file);
	}
	fputs("\n\t\t}", file);
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
				fprintf(file, "%s::", type->unresolved.path->module);
			}
			fputs(type->unresolved.name, file);
			break;
		case TYPE_INFO_TYPEOF:
			scratch_buffer_clear();
			span_to_scratch(type->unresolved_type_expr->span);
			fprintf(file, "$typeof(%s)", scratch_buffer_to_string());
			break;
		case TYPE_INFO_VATYPE:
			fprintf(file, "$vatype(...)");
			break;
		case TYPE_INFO_EVALTYPE:
			fprintf(file, "$evaltype(...)");
			break;
		case TYPE_INFO_TYPEFROM:
			fprintf(file, "$typefrom(...)");
			break;
		case TYPE_INFO_ARRAY:
			print_type(file, type->array.base);
			scratch_buffer_clear();
			span_to_scratch(type->array.len->span);
			fprintf(file, "[%s]", scratch_buffer_to_string());
			break;
		case TYPE_INFO_VECTOR:
			print_type(file, type->array.base);
			scratch_buffer_clear();
			span_to_scratch(type->array.len->span);
			fprintf(file, "[<%s>]", scratch_buffer_to_string());
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
static inline void emit_func_data(FILE *file, Module *module, Decl *func)
{
	fprintf(file, "\t\t\"%s::%s\": {\n", module->name->module, func->name);
	fprintf(file, "\t\t\t\"rtype\": \"");
	print_type(file, type_infoptr(func->func_decl.signature.rtype));
	fprintf(file, "\",\n");
	fputs("\t\t\t\"params\": [\n", file);
	FOREACH_BEGIN_IDX(i, Decl *decl, func->func_decl.signature.params)
		if (i != 0) fputs(",\n", file);
		if (!decl) continue;
		fputs("\t\t\t\t{\n", file);
		fprintf(file, "\t\t\t\t\t\"name\": \"%s\",\n", decl->name ? decl->name : "");
		fprintf(file, "\t\t\t\t\t\"type\": \"");
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
	FOREACH_END();
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
		FOREACH_DECL(Decl *type, global_context.module_list)
					if (!decl_is_user_defined_type(type) && type->decl_kind != DECL_TYPEDEF) continue;
					if (decl_is_hidden(type)) continue;
					INERT_COMMA;
					emit_type_data(file, module, type);
		FOREACH_DECL_END;
	}

	fputs("\n\t},\n", file);
	fputs("\t\"generic_types\": {\n", file);
	{
		bool first = true;
		FOREACH_DECL(Decl *type, global_context.generic_module_list)
					if (!decl_is_user_defined_type(type) && type->decl_kind != DECL_TYPEDEF) continue;
					if (decl_is_hidden(type)) continue;
					INERT_COMMA;
					emit_type_data(file, module, type);
		FOREACH_DECL_END;
	}
	fputs("\n\t}\n", file);
}

static inline void emit_functions(FILE *file)
{
	fputs("\t\"functions\": {\n", file);
	{
		bool first = true;
		FOREACH_DECL(Decl *func, global_context.module_list)
					if (func->decl_kind != DECL_FUNC) continue;
					if (decl_is_hidden(func)) continue;
					INERT_COMMA;
					emit_func_data(file, module, func);
		FOREACH_DECL_END;
	}
	fputs("\n\t},\n", file);

	fputs("\t\"generic_functions\": {\n", file);
	{
		bool first = true;
		FOREACH_DECL(Decl *func, global_context.generic_module_list)
					if (func->decl_kind != DECL_FUNC) continue;
					if (decl_is_hidden(func)) continue;
					INERT_COMMA;
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
	fputs("\n}", file);
}


void emit_json(void)
{
	emit_json_to_file(stdout);
}