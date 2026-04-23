#include "compiler_internal.h"
#include "../utils/json.h"
#include "../build/build.h"
#include <stdio.h>
#include "docs_template.h"

typedef enum
{
	DOC_CAT_FUNCTIONS,
	DOC_CAT_MACROS,
	DOC_CAT_TYPES,
	DOC_CAT_VARIABLES,
	DOC_CAT_COUNT
} DocCategory;

static const char *category_names[DOC_CAT_COUNT] = {"functions", "macros", "types", "variables"};
static Module **all_modules = NULL;

static void write_decl_uid(FILE *file, Module *module, Decl *decl);
static void emit_type_name_to_scratch(TypeInfo *type);
static void print_doc_type(FILE *file, Module *module, TypeInfo *type);
static void emit_params_json(FILE *file, Module *module, Decl **params);
static void write_expr_source_json(FILE *file, Expr *expr)
{
	if (!expr)
	{
		fputs("null", file);
		return;
	}
	scratch_buffer_clear();
	loc_to_scratch(expr->loc);
	json_write_string(file, scratch_buffer_to_string());
}

static void get_unit_lists(CompilationUnit *unit, DocCategory cat, Decl ***lists)
{
	lists[0] = lists[1] = lists[2] = NULL;
	int i = 0;
	switch (cat)
	{
		case DOC_CAT_FUNCTIONS:
			lists[i++] = unit->functions;
			lists[i++] = unit->methods;
			break;
		case DOC_CAT_MACROS:
			lists[i++] = unit->macros;
			lists[i++] = unit->macro_methods;
			break;
		case DOC_CAT_TYPES:
			lists[i++] = unit->types;
			lists[i++] = unit->enums;
			lists[i++] = unit->faults;
			break;
		case DOC_CAT_VARIABLES:
			lists[i++] = unit->vars;
			break;
		case DOC_CAT_COUNT:
			break;
	}
}

static const char *get_visibility_name(Visibility vis)
{
	switch (vis)
	{
		case VISIBLE_PUBLIC:
			return "public";
		case VISIBLE_PRIVATE:
			return "private";
		case VISIBLE_LOCAL:
			return "local";
		default:
			return "unknown";
	}
}

static const char *get_inout_modifier_name(InOutModifier mod)
{
	switch (mod)
	{
		case INOUT_IN:
			return "in";
		case INOUT_OUT:
			return "out";
		case INOUT_INOUT:
			return "inout";
		default:
			return NULL;
	}
}

static void emit_param_json(FILE *file, Module *module, Decl *p)
{
	fprintf(file, "{\"name\": \"%s\"", p->name ? p->name : "");
	if (p->decl_kind == DECL_BODYPARAM)
	{
		fputs(", \"kind\": \"body_param\", \"params\": ", file);
		emit_params_json(file, module, p->body_params);
	}
	else
	{
		fputs(", \"type\": ", file);
		print_doc_type(file, module, p->var.type_info ? type_infoptr(p->var.type_info) : NULL);
		if (p->var.init_expr)
		{
			fputs(", \"default_value\": ", file);
			write_expr_source_json(file, p->var.init_expr);
		}
		if (p->is_maybe_unused || p->is_must_use || p->var.no_alias)
		{
			fputs(", \"attributes\": [", file);
			bool first_attr = true;
			if (p->is_maybe_unused)
			{
				fputs("\"@unused\"", file);
				first_attr = false;
			}
			if (p->is_must_use)
			{
				if (!first_attr) fputs(", ", file);
				fputs("\"@used\"", file);
				first_attr = false;
			}
			if (p->var.no_alias)
			{
				if (!first_attr) fputs(", ", file);
				fputs("\"@noalias\"", file);
				first_attr = false;
			}
			fputs("]", file);
		}
		if (p->var.self_addr)
		{
			fputs(", \"is_ref\": true", file);
		}
	}
	fputs("}", file);
}

static void emit_params_json(FILE *file, Module *module, Decl **params)
{
	fputs("[", file);
	bool first = true;
	for (unsigned i = 0; i < vec_size(params); i++)
	{
		Decl *p = params[i];
		if (!p) continue;
		if (!first) fputs(", ", file);
		first = false;
		emit_param_json(file, module, p);
	}
	fputs("]", file);
}
static void write_decl_uid(FILE *file, Module *module, Decl *decl)
{
	if (!decl || !decl->name)
	{
		fputs("null", file);
		return;
	}
	fputs("\"", file);
	fprintf(file, "%s::", module->name->module);
	if ((decl->decl_kind == DECL_FUNC || decl->decl_kind == DECL_MACRO) && decl->func_decl.type_parent)
	{
		TypeInfo *parent = type_infoptr(decl->func_decl.type_parent);
		if (parent)
		{
			scratch_buffer_clear();
			emit_type_name_to_scratch(parent);
			fprintf(file, "%s.", scratch_buffer_to_string());
		}
	}
	fprintf(file, "%s\"", decl->name);
}

static void emit_type_name_to_scratch(TypeInfo *type)
{
	if (!type) return;
	if (type->type && type->type->name)
	{
		scratch_buffer_append(type->type->name);
		if (type->optional && !strstr(type->type->name, "?")) scratch_buffer_append("?");
		return;
	}
	if (type->kind == TYPE_INFO_IDENTIFIER || type->kind == TYPE_INFO_CT_IDENTIFIER)
	{
		scratch_buffer_append(type->unresolved.name);
	}
	else if (type->kind == TYPE_INFO_POINTER)
	{
		emit_type_name_to_scratch(type->pointer);
		scratch_buffer_append("*");
	}
	else if (type->kind == TYPE_INFO_ARRAY || type->kind == TYPE_INFO_INFERRED_ARRAY)
	{
		emit_type_name_to_scratch(type->array.base);
		scratch_buffer_append("[");
		if (type->array.len)
		{
			loc_to_scratch(type->array.len->loc);
		}
		scratch_buffer_append("]");
	}
	else if (type->kind == TYPE_INFO_VECTOR || type->kind == TYPE_INFO_INFERRED_VECTOR)
	{
		emit_type_name_to_scratch(type->array.base);
		scratch_buffer_append("[<");
		if (type->array.len)
		{
			loc_to_scratch(type->array.len->loc);
		}
		scratch_buffer_append(">]");
	}
	else if (type->kind == TYPE_INFO_SLICE)
	{
		emit_type_name_to_scratch(type->array.base);
		scratch_buffer_append("[*]");
	}
	if (type->optional) scratch_buffer_append("?");
}

static void emit_decl_uid_json(FILE *file, Decl *d)
{
	if (d && d->name && d->unit && d->unit->module)
	{
		fputs(", \"uid\": ", file);
		write_decl_uid(file, d->unit->module, d);
	}
}

static void print_doc_type(FILE *file, Module *module, TypeInfo *type)
{
	if (!type)
	{
		fputs("null", file);
		return;
	}
	fputs("{\"name\": \"", file);
	scratch_buffer_clear();
	emit_type_name_to_scratch(type);
	fputs(scratch_buffer_to_string(), file);
	fputs("\"", file);

	TypeInfo *base_info = type;
	while (base_info && (base_info->kind == TYPE_INFO_POINTER || base_info->kind == TYPE_INFO_ARRAY || base_info->kind == TYPE_INFO_INFERRED_ARRAY || base_info->kind == TYPE_INFO_SLICE || base_info->kind == TYPE_INFO_VECTOR || base_info->kind == TYPE_INFO_INFERRED_VECTOR))
	{
		if (base_info->kind == TYPE_INFO_POINTER)
			base_info = base_info->pointer;
		else
			base_info = base_info->array.base;
	}

	Type *t = base_info ? base_info->type : NULL;
	while (t && (t->type_kind == TYPE_POINTER || t->type_kind == TYPE_ARRAY || t->type_kind == TYPE_SLICE || t->type_kind == TYPE_VECTOR || t->type_kind == TYPE_SIMD_VECTOR || t->type_kind == TYPE_OPTIONAL))
	{
		if (t->type_kind == TYPE_POINTER)
			t = t->pointer;
		else if (t->type_kind == TYPE_OPTIONAL)
			t = t->optional;
		else
			t = t->array.base;
	}

	if (t)
	{
		if (t->type_kind == TYPE_INTERFACE || t->type_kind == TYPE_STRUCT || t->type_kind == TYPE_UNION || t->type_kind == TYPE_ENUM || t->type_kind == TYPE_BITSTRUCT || (t->type_kind >= TYPE_TYPEDEF && t->type_kind <= TYPE_ALIAS) || t->type_kind == TYPE_MEMBER)
		{
			emit_decl_uid_json(file, t->decl);
		}
	}
	else if (base_info && (base_info->kind == TYPE_INFO_IDENTIFIER || base_info->kind == TYPE_INFO_CT_IDENTIFIER) && module)
	{
		Decl *d = htable_get(&module->symbols, (void *)base_info->unresolved.name);
		if (!d && all_modules)
		{
			// Search in other modules if not found locally
			for (unsigned i = 0; i < vec_size(all_modules); i++)
			{
				if (all_modules[i] == module) continue;
				d = htable_get(&all_modules[i]->symbols, (void *)base_info->unresolved.name);
				if (d) break;
			}
		}
		emit_decl_uid_json(file, d);
	}
	fputs("}", file);
}

static void emit_doc_struct_members(FILE *file, Decl *decl, bool *first)
{
	if (decl->decl_kind != DECL_STRUCT && decl->decl_kind != DECL_UNION && decl->decl_kind != DECL_BITSTRUCT) return;
	for (unsigned i = 0; i < vec_size(decl->strukt.members); i++)
	{
		Decl *p = decl->strukt.members[i];
		if (!p) continue;

		if (p->decl_kind == DECL_VAR)
		{
			if (!*first) fputs(", ", file);
			*first = false;
			fprintf(file, "{\"name\": \"%s\", \"type\": ", p->name ? p->name : "");
			print_doc_type(file, decl->unit ? decl->unit->module : NULL, p->var.type_info ? type_infoptr(p->var.type_info) : NULL);
			if (decl->decl_kind == DECL_BITSTRUCT && p->var.kind == VARDECL_BITMEMBER)
			{
				fprintf(file, ", \"bit_range\": [%u, %u]", p->var.start_bit, p->var.end_bit);
			}
			fputs("}", file);
		}
		else if (p->decl_kind == DECL_STRUCT || p->decl_kind == DECL_UNION)
		{
			if (!*first) fputs(", ", file);
			*first = false;
			fprintf(file, "{\"kind\": \"%s\"", p->decl_kind == DECL_UNION ? "union" : "struct");
			if (p->name) fprintf(file, ", \"name\": \"%s\"", p->name);
			fputs(", \"members\": [", file);
			bool sub_first = true;
			emit_doc_struct_members(file, p, &sub_first);
			fputs("]}", file);
		}
	}
}

static void emit_doc_members(FILE *file, Module *module, Decl *decl)
{
	if (decl->decl_kind == DECL_FUNC || decl->decl_kind == DECL_MACRO)
	{
		fputs("[", file);
		bool first = true;
		for (unsigned i = 0; i < vec_size(decl->func_decl.signature.params); i++)
		{
			Decl *p = decl->func_decl.signature.params[i];
			if (!p) continue;
			if (!first) fputs(", ", file);
			first = false;
			emit_param_json(file, module, p);
		}
		if (decl->decl_kind == DECL_MACRO && decl->func_decl.body_param)
		{
			Decl *p = declptr(decl->func_decl.body_param);
			if (p)
			{
				if (!first) fputs(", ", file);
				emit_param_json(file, module, p);
			}
		}
		fputs("]", file);
		return;
	}
	if (decl->decl_kind == DECL_TYPE_ALIAS && decl->type_alias_decl.is_func)
	{
		Decl *fntype = decl->type_alias_decl.decl;
		if (fntype && fntype->decl_kind == DECL_FNTYPE)
		{
			emit_params_json(file, module, fntype->fntype_decl.signature.params);
			return;
		}
	}

	fputs("[", file);
	bool first = true;
	if (decl->decl_kind == DECL_ENUM || decl->decl_kind == DECL_CONSTDEF)
	{
		if (decl->decl_kind == DECL_ENUM)
		{
			for (unsigned i = 0; i < vec_size(decl->enums.parameters); i++)
			{
				Decl *p = decl->enums.parameters[i];
				if (!p || !p->name) continue;
				if (!first) fputs(", ", file);
				first = false;
				emit_param_json(file, module, p);
			}
		}

		for (unsigned i = 0; i < vec_size(decl->enums.values); i++)
		{
			Decl *p = decl->enums.values[i];
			if (!p || !p->name) continue;
			if (!first) fputs(", ", file);
			first = false;
			fprintf(file, "{\"name\": \"%s\", \"type\": null}", p->name);
		}
	}
	else if (decl->decl_kind == DECL_STRUCT || decl->decl_kind == DECL_UNION || decl->decl_kind == DECL_BITSTRUCT)
	{
		emit_doc_struct_members(file, decl, &first);
	}
	else if (decl->decl_kind == DECL_INTERFACE)
	{
		for (unsigned i = 0; i < vec_size(decl->interface_methods); i++)
		{
			Decl *p = decl->interface_methods[i];
			if (!p || !p->name) continue;
			if (!first) fputs(", ", file);
			first = false;

			fputs("{", file);
			fprintf(file, "\"name\": \"%s\", \"type\": ", p->name);
			if (p->func_decl.signature.rtype)
				print_doc_type(file, module, type_infoptr(p->func_decl.signature.rtype));
			else
				fputs("null", file);

			// Emit the parameter list so the HTML can reconstruct the full signature
			fputs(", \"params\": ", file);
			emit_params_json(file, module, p->func_decl.signature.params);

			fputs("}", file);
		}
	}
	fputs("]", file);
}

static void emit_custom_attrs(FILE *file, Decl *decl)
{
	if (!decl->resolved_attributes || !decl->attrs_resolved) return;
	if (vec_size(decl->attrs_resolved->tags) == 0) return;

	fputs(", \"custom_attrs\": [", file);
	for (unsigned i = 0; i < vec_size(decl->attrs_resolved->tags); i++)
	{
		if (i > 0) fputs(", ", file);
		Attr *attr = decl->attrs_resolved->tags[i];
		fputs("{", file);
		fputs("\"name\": ", file);
		json_write_string(file, attr->name);
		if (vec_size(attr->exprs) > 0)
		{
			fputs(", \"args\": [", file);
			for (unsigned j = 0; j < vec_size(attr->exprs); j++)
			{
				if (j > 0) fputs(", ", file);
				Expr *e = attr->exprs[j];
				if (e->expr_kind == EXPR_CONST && e->const_expr.const_kind == CONST_STRING)
				{
					json_write_string(file, e->const_expr.bytes.ptr);
				}
				else
				{
					fputs("null", file);
				}
			}
			fputs("]", file);
		}
		fputs("}", file);
	}
	fputs("]", file);
}

static void emit_normal_attrs(FILE *file, Decl *decl)
{
	bool has_attrs = false;

#define EMIT_ATTR(flag, name)                   \
	if (flag)                                   \
	{                                           \
		if (has_attrs)                          \
			fputs(", ", file);                  \
		else                                    \
		{                                       \
			fputs(", \"attributes\": [", file); \
			has_attrs = true;                   \
		}                                       \
		fputs("\"@" name "\"", file);           \
	}

	EMIT_ATTR(decl->is_packed, "packed")
	EMIT_ATTR(decl->is_export, "export")
	EMIT_ATTR(decl->is_weak, "weak")
	EMIT_ATTR(decl->is_weak_link, "weaklink")
	EMIT_ATTR(decl->is_maybe_unused, "unused")
	EMIT_ATTR(decl->is_must_use, "nodiscard")
	EMIT_ATTR(decl->will_reflect, "reflect")
	EMIT_ATTR(decl->obfuscate, "obfuscate")
	EMIT_ATTR(decl->is_dynamic, "dynamic")
	EMIT_ATTR(decl->no_strip, "nostrip")
	EMIT_ATTR(decl->attr_nopadding, "nopadding")
	EMIT_ATTR(decl->attr_compact, "compact")
	EMIT_ATTR(decl->attr_constinit, "constinit")
	EMIT_ATTR(decl->attr_mustinit, "mustinit")

	if (decl->decl_kind == DECL_FUNC || decl->decl_kind == DECL_MACRO)
	{
		if (decl->decl_kind == DECL_FUNC)
		{
			EMIT_ATTR(decl->func_decl.attr_inline, "inline")
			EMIT_ATTR(decl->func_decl.attr_noinline, "noinline")
			EMIT_ATTR(decl->func_decl.attr_naked, "naked")
			EMIT_ATTR(decl->func_decl.attr_benchmark, "benchmark")
			EMIT_ATTR(decl->func_decl.attr_test, "test")
			EMIT_ATTR(decl->func_decl.attr_winmain, "winmain")
			EMIT_ATTR(decl->func_decl.attr_optional, "optional")
			EMIT_ATTR(decl->func_decl.attr_init, "init")
			EMIT_ATTR(decl->func_decl.attr_finalizer, "finalizer")
		}
		EMIT_ATTR(decl->func_decl.signature.attrs.noreturn, "noreturn")
		EMIT_ATTR(decl->func_decl.signature.attrs.nodiscard, "nodiscard")
		EMIT_ATTR(decl->func_decl.signature.attrs.always_const, "const")
	}

	if (has_attrs) fputs("]", file);
#undef EMIT_ATTR
}

static Decl *get_contract_decl(DeclId id)
{
	if (!id) return NULL;
	Decl *d = declptr(id);
	if (d && d->decl_kind == DECL_CONTRACT) return d;
	return NULL;
}

static void emit_doc_comments(FILE *file, Decl *decl)
{
	if (!decl)
	{
		fputs("\"docs\": null", file);
		return;
	}

	Decl *contract = (decl->decl_kind == DECL_CONTRACT) ? decl : get_contract_decl(decl->docs);
	const char *deprecated = (decl->resolved_attributes && decl->attrs_resolved) ? decl->attrs_resolved->deprecated : NULL;

	if (!contract && !deprecated)
	{
		fputs("\"docs\": null", file);
		return;
	}

	fputs("\"docs\": {", file);
	bool first = true;

	if (deprecated)
	{
		fputs("\"deprecated\": ", file);
		json_write_string(file, deprecated);
		first = false;
	}

	if (contract)
	{
		if (contract->contracts_decl.comment)
		{
			if (!first) fputs(", ", file);
			fputs("\"text\": ", file);
			json_write_string(file, contract->contracts_decl.comment);
			first = false;
		}

		if (contract->contracts_decl.return_desc)
		{
			if (!first) fputs(", ", file);
			fputs("\"return\": ", file);
			json_write_string(file, contract->contracts_decl.return_desc);
			first = false;
		}

		if (contract->contracts_decl.pure)
		{
			if (!first) fputs(", ", file);
			fputs("\"pure\": true", file);
			first = false;
		}

		if (vec_size(contract->contracts_decl.params) > 0)
		{
			if (!first) fputs(", ", file);
			fputs("\"params\": [", file);
			bool first_p = true;
			for (unsigned i = 0; i < vec_size(contract->contracts_decl.params); i++)
			{
				ContractParam *p = &contract->contracts_decl.params[i];
				if (!p || !p->name) continue;
				if (!first_p) fputs(", ", file);
				first_p = false;

				fputs("{", file);
				fputs("\"name\": ", file);
				json_write_string(file, p->name);
				const char *mod = get_inout_modifier_name(p->modifier);
				if (mod) fprintf(file, ", \"modifier\": \"%s\"", mod);
				if (p->by_ref) fputs(", \"by_ref\": true", file);
				if (p->description)
				{
					fputs(", \"description\": ", file);
					json_write_string(file, p->description);
				}
				fputs("}", file);
			}
			fputs("]", file);
		}
	}

	fputs("}", file);
}

static const char *get_decl_kind_name(DeclKind kind)
{
	switch (kind)
	{
		case DECL_FUNC:
			return "function";
		case DECL_MACRO:
			return "macro";
		case DECL_STRUCT:
			return "struct";
		case DECL_UNION:
			return "union";
		case DECL_ENUM:
			return "enum";
		case DECL_BITSTRUCT:
			return "bitstruct";
		case DECL_TYPEDEF:
			return "typedef";
		case DECL_TYPE_ALIAS:
			return "type_alias";
		case DECL_CONSTDEF:
			return "constdef";
		case DECL_INTERFACE:
			return "interface";
		case DECL_VAR:
			return "variable";
		case DECL_FAULT:
			return "faultdef";
		default:
			return "other";
	}
}

static void emit_decl_json(FILE *file, Module *module, Decl *decl)
{
	fputs("\t\t\t{\n", file);
	fputs("\t\t\t\t\"name\": ", file);
	json_write_string(file, decl->name);
	fputs(",\n", file);
	fputs("\t\t\t\t\"kind\": \"", file);
	fputs(get_decl_kind_name(decl->decl_kind), file);
	fputs("\",\n", file);
	fputs("\t\t\t\t\"uid\": ", file);
	write_decl_uid(file, module, decl);
	fputs(",\n", file);
	fprintf(file, "\t\t\t\t\"visibility\": \"%s\",\n", get_visibility_name(decl->visibility));

	if (decl->decl_kind == DECL_STRUCT || decl->decl_kind == DECL_UNION || decl->decl_kind == DECL_BITSTRUCT || decl->decl_kind == DECL_ENUM || decl->decl_kind == DECL_TYPEDEF || decl->decl_kind == DECL_INTERFACE || decl->decl_kind == DECL_CONSTDEF)
	{
		if (decl->interfaces)
		{
			unsigned iface_count = vec_size(decl->interfaces);
			if (iface_count > 0)
			{
				fputs("\t\t\t\t\"interfaces\": [", file);
				for (unsigned i = 0; i < iface_count; i++)
				{
					if (i > 0) fputs(", ", file);
					print_doc_type(file, module, decl->interfaces[i]);
				}
				fputs("],\n", file);
			}
		}
	}

	if (decl->decl_kind == DECL_FUNC || decl->decl_kind == DECL_MACRO)
	{
		fputs("\t\t\t\t\"return_type\": ", file);
		if (decl->func_decl.signature.rtype)
		{
			print_doc_type(file, module, type_infoptr(decl->func_decl.signature.rtype));
		}
		else
		{
			fputs("null", file);
		}
		fputs(",\n", file);
		if (decl->decl_kind == DECL_MACRO)
		{
			fputs("\t\t\t\t\"is_at_macro\": ", file);
			fputs(decl->func_decl.signature.is_at_macro ? "true" : "false", file);
			fputs(",\n", file);
			fputs("\t\t\t\t\"is_safemacro\": ", file);
			fputs(decl->func_decl.signature.is_safemacro ? "true" : "false", file);
			fputs(",\n", file);
		}
	}
	else if (decl->decl_kind == DECL_TYPE_ALIAS && decl->type_alias_decl.is_func)
	{
		Decl *fntype = decl->type_alias_decl.decl;
		if (fntype && fntype->decl_kind == DECL_FNTYPE)
		{
			fputs("\t\t\t\t\"return_type\": ", file);
			if (fntype->fntype_decl.signature.rtype)
			{
				print_doc_type(file, module, type_infoptr(fntype->fntype_decl.signature.rtype));
			}
			else
			{
				fputs("null", file);
			}
			fputs(",\n", file);
		}
	}
	else if (decl->decl_kind == DECL_ENUM || decl->decl_kind == DECL_CONSTDEF || decl->decl_kind == DECL_BITSTRUCT || decl->decl_kind == DECL_TYPEDEF || decl->decl_kind == DECL_TYPE_ALIAS)
	{
		TypeInfo *base = NULL;
		if (decl->decl_kind == DECL_ENUM || decl->decl_kind == DECL_CONSTDEF)
			base = decl->enums.type_info;
		else if (decl->decl_kind == DECL_BITSTRUCT && decl->strukt.parent)
			base = type_infoptr(decl->strukt.parent);
		else if (decl->decl_kind == DECL_TYPEDEF)
			base = decl->distinct;
		else if (decl->decl_kind == DECL_TYPE_ALIAS)
			base = decl->type_alias_decl.type_info;

		if (base)
		{
			fputs("\t\t\t\t\"base_type\": ", file);
			print_doc_type(file, module, base);
			fputs(",\n", file);
		}
	}
	else if (decl->decl_kind == DECL_VAR)
	{
		TypeInfo *base = type_infoptrzero(decl->var.type_info);
		if (base)
		{
			fputs("\t\t\t\t\"type\": ", file);
			print_doc_type(file, module, base);
			fputs(",\n", file);
		}
		fputs("\t\t\t\t\"is_const\": ", file);
		fputs(decl->var.kind == VARDECL_CONST ? "true" : "false", file);
		fputs(",\n", file);
	}

	fputs("\t\t\t\t\"members\": ", file);
	emit_doc_members(file, module, decl);
	fputs(",\n", file);
	fputs("\t\t\t\t", file);
	emit_doc_comments(file, decl);
	emit_custom_attrs(file, decl);
	emit_normal_attrs(file, decl);
	fputs("\n", file);
	fputs("\t\t\t}", file);
}

static DocCategory get_category_for_decl(Decl *decl)
{
	switch (decl->decl_kind)
	{
		case DECL_FUNC:
			return DOC_CAT_FUNCTIONS;
		case DECL_MACRO:
			return DOC_CAT_MACROS;
		case DECL_STRUCT:
		case DECL_UNION:
		case DECL_ENUM:
		case DECL_BITSTRUCT:
		case DECL_TYPEDEF:
		case DECL_TYPE_ALIAS:
		case DECL_FAULT:
		case DECL_INTERFACE:
		case DECL_CONSTDEF:
			return DOC_CAT_TYPES;
		case DECL_VAR:
			return DOC_CAT_VARIABLES;
		default:
			return DOC_CAT_COUNT;
	}
}

static bool category_has_content(Module *module, DocCategory cat)
{
	unsigned unit_count = vec_size(module->units);
	for (unsigned j = 0; j < unit_count; j++)
	{
		CompilationUnit *unit = module->units[j];
		Decl **lists[3];
		get_unit_lists(unit, cat, lists);

		for (int l = 0; l < 3; l++)
		{
			Decl **list = lists[l];
			if (list && vec_size(list) > 0) return true;
		}

		unsigned generic_count = vec_size(unit->generic_decls);
		for (unsigned k = 0; k < generic_count; k++)
		{
			Decl *gdecl = unit->generic_decls[k];
			if (gdecl->decl_kind != DECL_GENERIC) continue;
			unsigned sub_count = vec_size(gdecl->generic_decl.decls);
			for (unsigned m = 0; m < sub_count; m++)
			{
				Decl *decl = gdecl->generic_decl.decls[m];
				if (get_category_for_decl(decl) == cat) return true;
			}
			sub_count = vec_size(gdecl->generic_decl.conditional_decls);
			for (unsigned m = 0; m < sub_count; m++)
			{
				Decl *decl = gdecl->generic_decl.conditional_decls[m];
				if (get_category_for_decl(decl) == cat) return true;
			}
		}
	}
	return false;
}

void compiler_docgen(BuildTarget *target)
{
	bool json_only = compiler.build.docgen_json_out;
	bool append = compiler.build.docgen_append;
	const char *out_name = json_only ? "stdout" : "docs.html";

	const char *data_start_marker = "/*DATA_START*/";
	const char *data_end_marker = "/*DATA_END*/";
	const char *target_str = (target && target->arch_os_target != ARCH_OS_TARGET_DEFAULT)
	                             ? arch_os_target[target->arch_os_target]
	                             : "default";

	char *existing = NULL;
	size_t existing_len = 0;
	if (!json_only && append && file_exists(out_name))
	{
		existing = file_read_all(out_name, &existing_len);
	}

	FILE *file = json_only ? stdout : fopen(out_name, "wb");
	if (!file)
	{
		error_exit("Could not open output file %s", out_name);
	}

	if (!json_only)
	{
		if (existing)
		{
			char *pos = strstr(existing, data_end_marker);
			if (!pos) error_exit("Could not find /*DATA_END*/ in existing docs.html for append.");
			fwrite(existing, 1, pos - existing, file);
		}
		else
		{
			char *pos = (char *)strstr((const char *)docs_html, data_start_marker);
			if (!pos) error_exit("Internal error: Could not find /*DATA_START*/ in the docs.html template.");
			fwrite(docs_html, 1, (pos - (const char *)docs_html) + strlen(data_start_marker), file);
		}
		fprintf(file, "\n\t\tEMBEDDED_JSON_LIST.push({ target: \"%s\", data: ", target_str);
	}

	fputs("{\n", file);
	all_modules = compiler.context.module_list;
	fputs("  \"modules\": {\n", file);

	unsigned module_count = vec_size(all_modules);
	bool first_module = true;
	for (unsigned i = 0; i < module_count; i++)
	{
		Module *module = all_modules[i];

		DeclId module_doc = 0;
		unsigned unit_count = vec_size(module->units);
		for (unsigned j = 0; j < unit_count; j++)
		{
			if (module->units[j]->module_doc)
			{
				module_doc = module->units[j]->module_doc;
				break;
			}
		}

		bool has_any_content = (module_doc != 0);
		bool cat_has_content[DOC_CAT_COUNT];
		for (int cat = 0; cat < DOC_CAT_COUNT; cat++)
		{
			cat_has_content[cat] = category_has_content(module, (DocCategory)cat);
			if (cat_has_content[cat]) has_any_content = true;
		}

		if (!has_any_content) continue;

		if (!first_module) fputs(",\n", file);
		first_module = false;

		fprintf(file, "    \"%s\": {\n", module->name->module);

		if (module_doc)
		{
			fputs("      ", file);
			emit_doc_comments(file, declptr(module_doc));
		}
		else
		{
			fputs("      \"docs\": null", file);
		}

		for (int cat = 0; cat < DOC_CAT_COUNT; cat++)
		{
			if (!cat_has_content[cat]) continue;

			fputs(",\n", file);
			fprintf(file, "      \"%s\": [\n", category_names[cat]);

			bool first_in_cat = true;
			for (unsigned j = 0; j < unit_count; j++)
			{
				CompilationUnit *unit = module->units[j];
				Decl **lists[3];
				get_unit_lists(unit, (DocCategory)cat, lists);

				for (int l = 0; l < 3; l++)
				{
					Decl **list = lists[l];
					if (!list) continue;
					unsigned decl_count = vec_size(list);
					for (unsigned k = 0; k < decl_count; k++)
					{
						Decl *decl = list[k];
						if (!first_in_cat) fputs(",\n", file);
						first_in_cat = false;
						emit_decl_json(file, module, decl);
					}
				}

				unsigned generic_count = vec_size(unit->generic_decls);
				for (unsigned k = 0; k < generic_count; k++)
				{
					Decl *gdecl = unit->generic_decls[k];
					if (gdecl->decl_kind != DECL_GENERIC) continue;
					unsigned sub_count = vec_size(gdecl->generic_decl.decls);
					for (unsigned m = 0; m < sub_count; m++)
					{
						Decl *decl = gdecl->generic_decl.decls[m];
						if (get_category_for_decl(decl) == cat)
						{
							if (!first_in_cat) fputs(",\n", file);
							first_in_cat = false;
							emit_decl_json(file, module, decl);
						}
					}
					sub_count = vec_size(gdecl->generic_decl.conditional_decls);
					for (unsigned m = 0; m < sub_count; m++)
					{
						Decl *decl = gdecl->generic_decl.conditional_decls[m];
						if (get_category_for_decl(decl) == cat)
						{
							if (!first_in_cat) fputs(",\n", file);
							first_in_cat = false;
							emit_decl_json(file, module, decl);
						}
					}
				}
			}
			fputs("\n      ]", file);
		}
		fputs("\n    }", file);
	}
	fputs("\n  }\n}\n", file);

	if (!json_only)
	{
		fputs(" });\n\t\t", file);
		if (existing)
		{
			char *pos = strstr(existing, data_end_marker);
			fwrite(pos, 1, strlen(pos), file);
		}
		else
		{
			char *pos = (char *)strstr((const char *)docs_html, data_end_marker);
			if (!pos) error_exit("Internal error: Could not find /*DATA_END*/ in the docs.html template.");
			fwrite(pos, 1, docs_html_len - (pos - (const char *)docs_html), file);
		}
		fclose(file);
		printf("Documentation generated to %s\n", out_name);
	}

	exit_compiler(COMPILER_SUCCESS_EXIT);
}
