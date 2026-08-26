#include "compiler_internal.h"
#include "../utils/json.h"
#include "../build/build.h"
#include <stdio.h>
#include "docs_template.h"

typedef enum
{
	DOC_CAT_FUNCTIONS,
	DOC_CAT_METHODS,
	DOC_CAT_MACROS,
	DOC_CAT_ATTRDEFS,
	DOC_CAT_MACRO_METHODS,
	DOC_CAT_TYPES,
	DOC_CAT_VARIABLES,
	DOC_CAT_COUNT
} DocCategory;

static const char *category_names[DOC_CAT_COUNT] = {"functions", "methods", "macros", "attrdefs", "macro_methods", "types", "variables"};
static Module **all_modules = NULL;

#define JSON_MAX_DEPTH 32
#define TRUNCATE_MAX_LEN 512
#define TRUNCATE_HALF_LEN (TRUNCATE_MAX_LEN / 2)

typedef struct
{
	FILE *file;
	bool first_stack[JSON_MAX_DEPTH];
	int depth;
} JsonEmitter;

static inline void json_init(JsonEmitter *e, FILE *file)
{
	e->file = file;
	e->depth = 0;
	e->first_stack[0] = true;
}

static inline void json_comma(JsonEmitter *e)
{
	if (!e->first_stack[e->depth])
	{
		fputs(",", e->file);
	}
	e->first_stack[e->depth] = false;
}

static inline void json_start_object_val(JsonEmitter *e)
{
	fputs("{", e->file);
	if (e->depth >= JSON_MAX_DEPTH - 1)
	{
		fflush(e->file);
		error_exit("\nError: JSON emitter exceeded maximum nesting depth.");
	}
	e->depth++;
	e->first_stack[e->depth] = true;
}

static inline void json_start_array_val(JsonEmitter *e)
{
	fputs("[", e->file);
	if (e->depth >= JSON_MAX_DEPTH - 1)
	{
		fflush(e->file);
		error_exit("\nError: JSON emitter exceeded maximum nesting depth.");
	}
	e->depth++;
	e->first_stack[e->depth] = true;
}

static inline void json_start_object(JsonEmitter *e)
{
	json_comma(e);
	json_start_object_val(e);
}

static inline void json_write_prop_key(JsonEmitter *e, const char *key)
{
	json_comma(e);
	fputs("\"", e->file);
	fputs(key, e->file);
	fputs("\":", e->file);
}

static inline void json_start_object_prop(JsonEmitter *e, const char *key)
{
	json_write_prop_key(e, key);
	json_start_object_val(e);
}

static inline void json_end_object(JsonEmitter *e)
{
	fputs("}", e->file);
	if (e->depth == 0)
	{
		fflush(e->file);
		error_exit("\nError: JSON emitter depth underflow.");
	}
	e->depth--;
}

static inline void json_start_array_prop(JsonEmitter *e, const char *key)
{
	json_write_prop_key(e, key);
	json_start_array_val(e);
}

static inline void json_start_array(JsonEmitter *e)
{
	json_comma(e);
	json_start_array_val(e);
}

static inline void json_end_array(JsonEmitter *e)
{
	fputs("]", e->file);
	if (e->depth == 0)
	{
		fflush(e->file);
		error_exit("\nError: JSON emitter depth underflow.");
	}
	e->depth--;
}

static inline void json_write_prop_string(JsonEmitter *e, const char *key, const char *str)
{
	if (!str) return;
	json_write_prop_key(e, key);
	json_write_string(e->file, str);
}

static inline void json_write_prop_bool(JsonEmitter *e, const char *key, bool val)
{
	json_write_prop_key(e, key);
	fputs(val ? "true" : "false", e->file);
}

static void write_decl_uid(JsonEmitter *e, Module *module, Decl *decl);
static void emit_type_name_to_scratch(TypeInfo *type);
static void print_doc_type(JsonEmitter *e, Module *module, TypeInfo *type, bool is_vararg);
static void emit_params_json(JsonEmitter *e, Module *module, Decl **params);
static bool emit_doc_comments(JsonEmitter *e, Decl *decl);
static void emit_param_json(JsonEmitter *e, Module *module, Decl *p);

static void truncate_scratch_buffer_middle(void)
{
	const char *str = scratch_buffer_to_string();
	size_t len = strlen(str);
	if (len <= TRUNCATE_MAX_LEN) return;

	size_t head_len = TRUNCATE_HALF_LEN;
	while (head_len > 0 && ((unsigned char)str[head_len] & 0xC0) == 0x80)
	{
		head_len--;
	}
	if (head_len < TRUNCATE_HALF_LEN && ((unsigned char)str[head_len] & 0x80) != 0)
	{
		unsigned char c = (unsigned char)str[head_len];
		size_t char_len = 1;
		if      ((c & 0xE0) == 0xC0) char_len = 2;
		else if ((c & 0xF0) == 0xE0) char_len = 3;
		else if ((c & 0xF8) == 0xF0) char_len = 4;
		if (head_len + char_len <= TRUNCATE_HALF_LEN)
		{
			head_len += char_len;
		}
	}

	size_t tail_start = len - TRUNCATE_HALF_LEN;
	while (tail_start < len && ((unsigned char)str[tail_start] & 0xC0) == 0x80)
	{
		tail_start++;
	}

	char head[TRUNCATE_HALF_LEN + 1];
	char tail[TRUNCATE_HALF_LEN + 1];
	memcpy(head, str, head_len);
	head[head_len] = '\0';
	size_t tail_len = len - tail_start;
	memcpy(tail, str + tail_start, tail_len);
	tail[tail_len] = '\0';

	scratch_buffer_clear();
	scratch_buffer_append_len(head, head_len);
	scratch_buffer_append("\n...\n");
	scratch_buffer_append_len(tail, tail_len);
}

static void write_expr_source_json(FILE *file, Expr *expr)
{
	if (!expr)
	{
		fputs("null", file);
		return;
	}
	scratch_buffer_clear();
	loc_to_scratch(expr->loc);
	truncate_scratch_buffer_middle();
	json_write_string(file, scratch_buffer_to_string());
}

static void write_const_value_json(FILE *file, Expr *expr)
{
	if (!expr)
	{
		fputs("null", file);
		return;
	}
	if (expr->expr_kind == EXPR_CONST)
	{
		switch (expr->const_expr.const_kind)
		{
			case CONST_INTEGER:
			case CONST_FLOAT:
			case CONST_BOOL:
			case CONST_STRING:
			case CONST_ENUM:
			case CONST_FAULT:
			case CONST_TYPEID:
			case CONST_POINTER:
			case CONST_REF:
				scratch_buffer_clear();
				expr_const_to_scratch_buffer(&expr->const_expr);
				truncate_scratch_buffer_middle();
				json_write_string(file, scratch_buffer_to_string());
				return;
			default:
				break;
		}
	}
	write_expr_source_json(file, expr);
}

static void get_unit_lists(CompilationUnit *unit, DocCategory cat, Decl ***lists)
{
	lists[0] = lists[1] = lists[2] = NULL;
	int i = 0;
	switch (cat)
	{
		case DOC_CAT_FUNCTIONS:
			lists[i++] = unit->functions;
			break;
		case DOC_CAT_METHODS:
			lists[i++] = unit->methods;
			break;
		case DOC_CAT_MACROS:
			lists[i++] = unit->macros;
			break;
		case DOC_CAT_ATTRDEFS:
			lists[i++] = unit->attributes;
			break;
		case DOC_CAT_MACRO_METHODS:
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
		case INOUT_IN:    return "in";
		case INOUT_OUT:   return "out";
		case INOUT_INOUT: return "inout";
		case INOUT_INIT:  return "init";
		case INOUT_OWN:   return "own";
		case INOUT_DROP:  return "drop";
		default:
			return NULL;
	}
}

static void emit_param_json(JsonEmitter *e, Module *module, Decl *p)
{
	json_start_object(e);
	if (p->name && p->name[0])
	{
		json_write_prop_string(e, "name", p->name);
	}
	if (p->decl_kind == DECL_BODYPARAM)
	{
		json_write_prop_string(e, "kind", "body_param");
		json_write_prop_key(e, "params");
		emit_params_json(e, module, p->body_params);
	}
	else
	{
		if (p->var.type_info)
		{
			json_write_prop_key(e, "type");
			print_doc_type(e, module, type_infoptr(p->var.type_info), p->var.vararg);
		}
		if (p->var.init_expr)
		{
			json_write_prop_key(e, "default_value");
			write_expr_source_json(e->file, p->var.init_expr);
		}
		if (p->is_maybe_unused || p->is_must_use || p->var.no_alias)
		{
			json_start_array_prop(e, "attributes");
			if (p->is_maybe_unused) { json_comma(e); fputs("\"@unused\"", e->file); }
			if (p->is_must_use)     { json_comma(e); fputs("\"@used\"", e->file); }
			if (p->var.no_alias)     { json_comma(e); fputs("\"@noalias\"", e->file); }
			json_end_array(e);
		}
		if (p->var.self_addr) json_write_prop_bool(e, "is_ref", true);
		if (p->var.vararg)    json_write_prop_bool(e, "is_vararg", true);
	}
	json_end_object(e);
}

static void emit_params_json(JsonEmitter *e, Module *module, Decl **params)
{
	json_start_array_val(e);
	for (int i = 0; i < vec_size(params); i++)
	{
		Decl *p = params[i];
		if (!p) continue;
		emit_param_json(e, module, p);
	}
	json_end_array(e);
}

static void write_decl_uid(JsonEmitter *e, Module *module, Decl *decl)
{
	if (!decl || !decl->name)
	{
		fputs("null", e->file);
		return;
	}
	fputs("\"", e->file);
	fprintf(e->file, "%s::", module->name->module);
	if ((decl->decl_kind == DECL_FUNC || decl->decl_kind == DECL_MACRO))
	{
		TypeInfo *parent = decl_find_target_if_method(decl);
		if (parent)
		{
			scratch_buffer_clear();
			emit_type_name_to_scratch(parent);
			fprintf(e->file, "%s.", scratch_buffer_to_string());
		}
	}
	fprintf(e->file, "%s\"", decl->name);
}

static void emit_type_name_to_scratch(TypeInfo *type)
{
	if (!type) return;
	if (type->kind != TYPE_INFO_TYPEOF && type->kind != TYPE_INFO_TYPEFROM && type->type && type->type->name)
	{
		scratch_buffer_append(type->type->name);
		if (type->optional && !strstr(type->type->name, "?")) scratch_buffer_append("?");
		return;
	}
	switch (type->kind)
	{
		case TYPE_INFO_CT_IDENTIFIER:
		case TYPE_INFO_IDENTIFIER:
			scratch_buffer_append(type->unresolved.name);
			break;
		case TYPE_INFO_POINTER:
			emit_type_name_to_scratch(type->pointer);
			scratch_buffer_append("*");
			break;
		case TYPE_INFO_ARRAY:
		case TYPE_INFO_INFERRED_ARRAY:
			emit_type_name_to_scratch(type->array.base);
			scratch_buffer_append("[");
			if (type->array.len)
			{
				loc_to_scratch(type->array.len->loc);
			}
			scratch_buffer_append("]");
			break;
		case TYPE_INFO_INFERRED_VECTOR:
		case TYPE_INFO_VECTOR:
			emit_type_name_to_scratch(type->array.base);
			scratch_buffer_append("[<");
			if (type->array.len)
			{
				loc_to_scratch(type->array.len->loc);
			}
			scratch_buffer_append(">]");
			break;
		case TYPE_INFO_SLICE:
			emit_type_name_to_scratch(type->array.base);
			scratch_buffer_append("[*]");
			break;
		case TYPE_INFO_GENERIC:
			emit_type_name_to_scratch(type->generic.base);
			scratch_buffer_append("{");
			int param_count = vec_size(type->generic.params);
			for (int i = 0; i < param_count; i++)
			{
				if (i > 0) scratch_buffer_append(", ");
				Expr *param = type->generic.params[i];
				if (param->expr_kind == EXPR_TYPEINFO)
				{
					emit_type_name_to_scratch(param->type_expr);
				}
				else
				{
					loc_to_scratch(param->loc);
				}
			}
			scratch_buffer_append("}");
			break;
		case TYPE_INFO_TYPEOF:
			scratch_buffer_append("$Typeof(");
			if (type->unresolved_type_expr) loc_to_scratch(type->unresolved_type_expr->loc);
			scratch_buffer_append(")");
			break;
		case TYPE_INFO_TYPEFROM:
			scratch_buffer_append("$Typefrom(");
			if (type->unresolved_type_expr) loc_to_scratch(type->unresolved_type_expr->loc);
			scratch_buffer_append(")");
			break;
		case TYPE_INFO_POISON:
			scratch_buffer_append("*INVALID*");
			break;
	}
	switch (type->subtype)
	{
		case TYPE_COMPRESSED_NONE:
			break;
		case TYPE_COMPRESSED_PTR:
			scratch_buffer_append("*");
			break;
		case TYPE_COMPRESSED_SUB:
			scratch_buffer_append("[]");
			break;
		case TYPE_COMPRESSED_SUBPTR:
			scratch_buffer_append("[]*");
			break;
		case TYPE_COMPRESSED_PTRPTR:
			scratch_buffer_append("**");
			break;
		case TYPE_COMPRESSED_PTRSUB:
			scratch_buffer_append("*[]");
			break;
		case TYPE_COMPRESSED_SUBSUB:
			scratch_buffer_append("[][]");
			break;
	}
	if (type->optional) scratch_buffer_append("?");
}

static void emit_decl_uid_json(JsonEmitter *e, Decl *d)
{
	if (d && d->name && d->unit && d->unit->module)
	{
		json_write_prop_key(e, "uid");
		write_decl_uid(e, d->unit->module, d);
	}
}

static void emit_return_type_json(JsonEmitter *e, Module *module, TypeInfo *rtype)
{
	if (rtype)
	{
		json_write_prop_key(e, "return_type");
		print_doc_type(e, module, rtype, false);
	}
}

static void print_doc_type(JsonEmitter *e, Module *module, TypeInfo *type, bool is_vararg)
{
	if (!type)
	{
		fputs("null", e->file);
		return;
	}
	json_start_object_val(e);
	json_write_prop_key(e, "name");
	scratch_buffer_clear();
	if (is_vararg && type->type)
	{
		int declared_dims = 0;
		for (TypeInfo *ti = type; ti; )
		{
			switch (ti->kind)
			{
				case TYPE_INFO_POINTER:
					ti = ti->pointer;
					break;
				case TYPE_INFO_SLICE:
					declared_dims++;
					ti = ti->array.base;
					break;
				case TYPE_INFO_ARRAY:
				case TYPE_INFO_INFERRED_ARRAY:
				case TYPE_INFO_INFERRED_VECTOR:
				case TYPE_INFO_VECTOR:
					ti = ti->array.base;
					break;
				case TYPE_INFO_IDENTIFIER:
				case TYPE_INFO_CT_IDENTIFIER:
					switch (ti->subtype)
					{
						case TYPE_COMPRESSED_SUB:
						case TYPE_COMPRESSED_SUBPTR:
						case TYPE_COMPRESSED_PTRSUB:
							declared_dims += 1;
							break;
						case TYPE_COMPRESSED_SUBSUB:
							declared_dims += 2;
							break;
						case TYPE_COMPRESSED_NONE:
						case TYPE_COMPRESSED_PTR:
						case TYPE_COMPRESSED_PTRPTR:
							break;
					}
					ti = NULL;
					break;
				default:
					ti = NULL;
					break;
			}
		}

		int resolved_dims = 0;
		Type *rt = type->type;
		while (rt && rt->type_kind == TYPE_SLICE)
		{
			resolved_dims++;
			rt = rt->array.base;
		}

		Type *t = type->type;
		if (resolved_dims > declared_dims && t->type_kind == TYPE_SLICE)
		{
			t = t->array.base;
		}
		scratch_buffer_append(t->name);
		scratch_buffer_append("...");
	}
	else
	{
		emit_type_name_to_scratch(type);
		if (is_vararg) scratch_buffer_append("...");
	}

	json_write_string(e->file, scratch_buffer_to_string());

	TypeInfo *base_info = type;

RETRY:
	switch (base_info->kind)
	{
		case TYPE_INFO_POISON:
			break;
		case TYPE_INFO_POINTER:
			base_info = base_info->pointer;
			goto RETRY;
		case TYPE_INFO_ARRAY:
		case TYPE_INFO_INFERRED_ARRAY:
		case TYPE_INFO_INFERRED_VECTOR:
		case TYPE_INFO_SLICE:
		case TYPE_INFO_VECTOR:
			base_info = base_info->array.base;
			goto RETRY;
		case TYPE_INFO_IDENTIFIER:
		case TYPE_INFO_CT_IDENTIFIER:
		case TYPE_INFO_TYPEOF:
		case TYPE_INFO_TYPEFROM:
		case TYPE_INFO_GENERIC:
			break;
	}

	Type *t = base_info->type;
	if (!t) t = poisoned_type;
RETRY2:
	switch (t->type_kind)
	{
		case TYPE_POINTER:
			t = t->pointer;
			goto RETRY2;
		case TYPE_SLICE:
		case TYPE_ARRAY:
		case TYPE_VECTOR:
		case TYPE_SIMD_VECTOR:
			t = t->array.base;
			goto RETRY2;
		case TYPE_OPTIONAL:
			t = t->optional;
			goto RETRY2;
		default:
			break;
	}
	switch (t->type_kind)
	{
		case TYPE_INTERFACE:
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_ENUM:
		case TYPE_BITSTRUCT:
		case TYPE_CONSTDEF:
		case TYPE_FUNC_RAW:
		case TYPE_ALIAS:
		case TYPE_MEMBER:
			emit_decl_uid_json(e, t->decl);
			break;
		default:
			if ((base_info->kind == TYPE_INFO_IDENTIFIER || base_info->kind == TYPE_INFO_CT_IDENTIFIER) && module)
			{
				Decl *d = htable_get(&module->symbols, (void *)base_info->unresolved.name);
				if (!d && all_modules)
				{
					// Search in other modules if not found locally
					FOREACH(Module *, m, all_modules)
					{
						if (m == module) continue;
						d = htable_get(&m->symbols, (void *)base_info->unresolved.name);
						if (d) break;
					}
				}
				emit_decl_uid_json(e, d);
			}

			break;
	}
	json_end_object(e);
}

static void emit_doc_struct_members(JsonEmitter *e, Decl *decl)
{
	if (!decl_has_members(decl)) return;
	FOREACH(Decl *, p, decl->strukt.members)
	{
		if (!p) continue;

		if (p->decl_kind == DECL_VAR)
		{
			json_start_object(e);
			if (p->name && p->name[0])
			{
				json_write_prop_string(e, "name", p->name);
			}
			json_write_prop_key(e, "type");
			print_doc_type(e, decl->unit ? decl->unit->module : NULL, p->var.type_info ? type_infoptr(p->var.type_info) : NULL, false);
			if (decl->decl_kind == DECL_BITSTRUCT && p->var.kind == VARDECL_BITMEMBER)
			{
				json_write_prop_key(e, "bit_range");
				fprintf(e->file, "[%u,%u]", p->var.start_bit, p->var.end_bit);
			}
			json_end_object(e);
			continue;
		}
		json_start_object(e);
		json_write_prop_string(e, "kind", decl_to_name(p));
		if (p->name && p->name[0])
		{
			json_write_prop_string(e, "name", p->name);
		}
		json_start_array_prop(e, "members");
		emit_doc_struct_members(e, p);
		json_end_array(e);
		json_end_object(e);
	}
}

static bool emit_doc_members_json(JsonEmitter *e, Module *module, Decl *decl)
{
	if (decl_is_fn_macro(decl))
	{
		int count = 0;
		FOREACH(Decl *, p, decl->func_decl.signature.params) if (p) count++;
		if (decl->decl_kind == DECL_MACRO && decl->func_decl.body_param && declptr(decl->func_decl.body_param)) count++;
		if (!count) return false;
		json_start_array_prop(e, "members");
		FOREACH(Decl *, p, decl->func_decl.signature.params)
		{
			if (p) emit_param_json(e, module, p);
		}
		if (decl->decl_kind == DECL_MACRO && decl->func_decl.body_param)
		{
			Decl *p = declptr(decl->func_decl.body_param);
			if (p) emit_param_json(e, module, p);
		}
		json_end_array(e);
		return true;
	}
	if (decl->decl_kind == DECL_TYPE_ALIAS && decl->type_alias_decl.is_func)
	{
		Decl *fntype = decl->type_alias_decl.decl;
		if (fntype && fntype->decl_kind == DECL_FNTYPE)
		{
			Decl **params = fntype->fntype_decl.signature.params;
			if (!vec_size(params)) return false;
			json_write_prop_key(e, "members");
			emit_params_json(e, module, params);
			return true;
		}
		return false;
	}
	if (decl->decl_kind == DECL_ATTRIBUTE)
	{
		Decl **params = decl->attr_decl.params;
		if (!vec_size(params)) return false;
		json_write_prop_key(e, "members");
		emit_params_json(e, module, params);
		return true;
	}
	if (decl->decl_kind == DECL_ENUM || decl->decl_kind == DECL_CONSTDEF)
	{
		if (!vec_size(decl->enums.values)) return false;
		json_start_array_prop(e, "members");
		FOREACH_IDX(i, Decl *, p, decl->enums.values)
		{
			json_start_object(e);
			json_write_prop_string(e, "name", p->name ? p->name : "");
			json_start_object_prop(e, "type");
			json_write_prop_string(e, "name", decl->name ? decl->name : "");
			emit_decl_uid_json(e, decl);
			json_end_object(e);
			if (decl->decl_kind == DECL_ENUM && vec_size(decl->enums.parameters) > 0)
			{
				json_start_array_prop(e, "value");
				FOREACH_IDX(j, Expr *, expr, p->enum_constant.associated)
				{
					json_comma(e);
					write_const_value_json(e->file, expr);
				}
				json_end_array(e);
			}
			else if (p->enum_constant.value)
			{
				json_write_prop_key(e, "value");
				write_const_value_json(e->file, p->enum_constant.value);
			}
			emit_doc_comments(e, p);
			json_end_object(e);
		}
		json_end_array(e);
		return true;
	}
	if (decl_has_members(decl))
	{
		if (!vec_size(decl->strukt.members)) return false;
		json_start_array_prop(e, "members");
		emit_doc_struct_members(e, decl);
		json_end_array(e);
		return true;
	}
	if (decl->decl_kind == DECL_INTERFACE)
	{
		if (!vec_size(decl->interface_methods)) return false;
		json_start_array_prop(e, "members");
		FOREACH(Decl *, p, decl->interface_methods)
		{
			json_start_object(e);
			json_write_prop_string(e, "name", p->name);
			json_write_prop_key(e, "type");
			if (p->func_decl.signature.rtype)
			{
				print_doc_type(e, module, type_infoptr(p->func_decl.signature.rtype), false);
			}
			else
			{
				fputs("null", e->file);
			}
			json_start_array_prop(e, "params");
			for (int i = 1; i < vec_size(p->func_decl.signature.params); i++)
			{
				Decl *param = p->func_decl.signature.params[i];
				if (!param) continue;
				emit_param_json(e, module, param);
			}
			json_end_array(e);

			if (p->func_decl.attr_optional)
			{
				json_write_prop_bool(e, "is_optional", true);
			}
			json_end_object(e);
		}
		json_end_array(e);
		return true;
	}
	return false;
}

static void emit_custom_attrs(JsonEmitter *e, Decl *decl)
{
	if (!decl->resolved_attributes || !decl->attrs_resolved) return;
	if (vec_size(decl->attrs_resolved->tags) == 0) return;

	json_start_array_prop(e, "custom_attrs");
	FOREACH_IDX(i, Attr *, attr, decl->attrs_resolved->tags)
	{
		json_start_object(e);
		json_write_prop_string(e, "name", attr->name);
		if (vec_size(attr->exprs) > 0)
		{
			json_start_array_prop(e, "args");
			FOREACH(Expr *, ex, attr->exprs)
			{
				json_comma(e);
				if (expr_is_const_string(ex))
				{
					json_write_string(e->file, ex->const_expr.bytes.ptr);
				}
				else
				{
					fputs("null", e->file);
				}
			}
			json_end_array(e);
		}
		json_end_object(e);
	}
	json_end_array(e);
}

static void emit_normal_attrs(JsonEmitter *e, Decl *decl)
{
	bool has_attrs = false;

#define EMIT_ATTR(flag, name)                               \
	if (flag)                                               \
	{                                                       \
		if (!has_attrs)                                     \
		{                                                   \
			json_start_array_prop(e, "attributes");         \
			has_attrs = true;                               \
		}                                                   \
		json_comma(e);                                      \
		json_write_string(e->file, "@" name);               \
	}

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
	EMIT_ATTR(decl->attr_constinit, "constinit")
	EMIT_ATTR(decl->attr_mustinit, "mustinit")

	if (decl->decl_kind == DECL_STRUCT || decl->decl_kind == DECL_UNION)
	{
		EMIT_ATTR(decl->strukt.is_compact, "compact")
		EMIT_ATTR(decl->strukt.is_packed, "packed")
	}
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

	if (has_attrs) json_end_array(e);
#undef EMIT_ATTR
}

static Decl *get_contract_decl(DeclId id)
{
	if (!id) return NULL;
	Decl *d = declptr(id);
	if (d->decl_kind == DECL_CONTRACT) return d;
	return NULL;
}

static bool emit_doc_comments(JsonEmitter *e, Decl *decl)
{
	if (!decl) return false;

	bool is_func_alias = decl->decl_kind == DECL_TYPE_ALIAS && decl->type_alias_decl.is_func;
	DeclId docs_id = decl->docs;
	if (!docs_id && is_func_alias && decl->type_alias_decl.decl)
	{
		docs_id = decl->type_alias_decl.decl->docs;
	}
	Decl *contract = (decl->decl_kind == DECL_CONTRACT) ? decl : get_contract_decl(docs_id);
	const char *deprecated = (decl->resolved_attributes && decl->attrs_resolved) ? decl->attrs_resolved->deprecated : NULL;

	bool is_callable = decl->decl_kind == DECL_FUNC || decl->decl_kind == DECL_MACRO || decl->decl_kind == DECL_FNTYPE || is_func_alias;
	bool has_docs = (deprecated != NULL);
	if (contract)
	{
		if (contract->contracts_decl.comment || (is_callable && contract->contracts_decl.return_desc) || contract->contracts_decl.pure || vec_size(contract->contracts_decl.params) > 0)
		{
			has_docs = true;
		}
	}

	if (!has_docs) return false;

	json_start_object_prop(e, "docs");

	if (deprecated)
	{
		json_write_prop_string(e, "deprecated", deprecated);
	}

	if (contract)
	{
		if (contract->contracts_decl.comment)
		{
			json_write_prop_string(e, "text", contract->contracts_decl.comment);
		}

		if (is_callable && contract->contracts_decl.return_desc)
		{
			json_write_prop_string(e, "return", contract->contracts_decl.return_desc);
		}

		if (contract->contracts_decl.pure)
		{
			json_write_prop_bool(e, "pure", true);
		}

		if (vec_size(contract->contracts_decl.params) > 0)
		{
			json_start_array_prop(e, "params");
			FOREACH_REF(ContractParam, p, contract->contracts_decl.params)
			{
				if (!p || !p->name) continue;

				json_start_object(e);
				json_write_prop_string(e, "name", p->name);
				const char *mod = get_inout_modifier_name(p->modifier);
				if (mod) json_write_prop_string(e, "modifier", mod);
				if (p->by_ref) json_write_prop_bool(e, "by_ref", true);
				if (p->description) json_write_prop_string(e, "description", p->description);
				json_end_object(e);
			}
			json_end_array(e);
		}
	}

	json_end_object(e);
	return true;
}

static void emit_attrdef_target_json(JsonEmitter *e, Decl *decl)
{
	Attr **attrs = decl->attr_decl.attrs;
	if (vec_size(attrs) == 0) return;

	scratch_buffer_clear();
	FOREACH_IDX(i, Attr *, attr, attrs)
	{
		if (!attr) continue;
		if (i > 0) scratch_buffer_append(" ");
		if (attr->name)
		{
			if (attr->name[0] != '@') scratch_buffer_append("@");
			scratch_buffer_append(attr->name);
		}
		if (vec_size(attr->exprs) > 0)
		{
			scratch_buffer_append("(");
			FOREACH_IDX(j, Expr *, ex, attr->exprs)
			{
				if (j > 0) scratch_buffer_append(", ");
				if (ex) loc_to_scratch(ex->loc);
			}
			scratch_buffer_append(")");
		}
	}
	json_write_prop_string(e, "target", scratch_buffer_to_string());
}

static const char *get_decl_kind_name(Decl *decl)
{
	switch (decl->decl_kind)
	{
		case DECL_FUNC:
			if (decl->func_decl.type_parent) return "method";
			return "function";
		case DECL_MACRO:
			if (decl->func_decl.type_parent) return "macro_method";
			return "macro";
		default:
			return decl_to_name(decl);
	}
}

static void emit_decl_json(JsonEmitter *e, Module *module, Decl *decl, const char **generic_params)
{
	json_start_object(e);

	json_write_prop_string(e, "name", decl->name);
	json_write_prop_string(e, "kind", get_decl_kind_name(decl));
	json_write_prop_key(e, "uid");
	write_decl_uid(e, module, decl);

	if (decl->loc)
	{
		SourceLoc *loc_info = sourcelocptr(decl->loc);
		if (loc_info && loc_info->file_id)
		{
			File *f = source_file_by_id(loc_info->file_id);
			if (f && f->full_path)
			{
				scratch_buffer_clear();
				const char *path = f->full_path;
				char cwd_buf[PATH_MAX + 1];
				const char *cwd = getcwd(cwd_buf, sizeof(cwd_buf));
				if (cwd)
				{
					for (char *p = cwd_buf; *p; p++) if (*p == '\\') *p = '/';
					size_t cwd_len = strlen(cwd);
					if (strncmp(path, cwd, cwd_len) == 0 && path[cwd_len] == '/')
					{
						path = path + cwd_len + 1;
					}
				}
				scratch_buffer_printf("%s:%u:%u", path, loc_info->row, loc_info->col);
				json_write_prop_string(e, "file", scratch_buffer_to_string());
			}
		}
	}
	if (decl->visibility != VISIBLE_PUBLIC)
	{
		json_write_prop_string(e, "visibility", get_visibility_name(decl->visibility));
	}
	if (decl->is_template)
	{
		json_write_prop_bool(e, "is_generic", true);
	}
	if (generic_params)
	{
		int param_count = vec_size(generic_params);
		if (param_count > 0)
		{
			json_start_array_prop(e, "generic_parameters");
			for (int i = 0; i < param_count; i++)
			{
				json_comma(e);
				json_write_string(e->file, generic_params[i]);
			}
			json_end_array(e);
		}
	}
	if (decl_has_interface(decl))
	{
		int iface_count = vec_size(decl->interfaces);
		if (iface_count > 0)
		{
			json_start_array_prop(e, "interfaces");
			for (int i = 0; i < iface_count; i++)
			{
				json_comma(e);
				print_doc_type(e, module, decl->interfaces[i], false);
			}
			json_end_array(e);
		}
	}

	TypeInfo *base = NULL;
	switch (decl->decl_kind)
	{
		case DECL_FUNC:
		case DECL_MACRO:
			emit_return_type_json(e, module, type_infoptrzero(decl->func_decl.signature.rtype));
			if (decl->decl_kind == DECL_MACRO)
			{
				if (decl->func_decl.signature.is_at_macro)  json_write_prop_bool(e, "is_at_macro", true);
				if (decl->func_decl.signature.is_safemacro) json_write_prop_bool(e, "is_safemacro", true);
			}
			break;
		case DECL_TYPE_ALIAS:
			if (decl->type_alias_decl.is_func)
			{
				Decl *fntype = decl->type_alias_decl.decl;
				if (fntype && fntype->decl_kind == DECL_FNTYPE)
				{
					emit_return_type_json(e, module, type_infoptrzero(fntype->fntype_decl.signature.rtype));
				}
				break;
			}
			if (decl->type_alias_decl.type_expr->expr_kind != EXPR_TYPEINFO)
			{
				Expr *expr = decl->type_alias_decl.type_expr;
				base = type_info_new(TYPE_INFO_TYPEFROM, expr->loc);
				base->unresolved_type_expr = expr;
				goto PRINT_BASE;
			}
			base = decl->type_alias_decl.type_expr->type_expr;
			goto PRINT_BASE;
		case DECL_ENUM:
		case DECL_CONSTDEF:
			base = decl->enums.type_info;
			goto PRINT_BASE;
		case DECL_BITSTRUCT:
			base = decl->strukt.container_type;
			goto PRINT_BASE;
		case DECL_TYPEDEF:
			if (decl->is_substruct) json_write_prop_bool(e, "is_inline", true);
			base = decl->distinct;
			goto PRINT_BASE;
		PRINT_BASE:
			json_write_prop_key(e, "base_type");
			print_doc_type(e, module, base, false);
			break;
		case DECL_VAR:
			base = type_infoptrzero(decl->var.type_info);
			if (base)
			{
				json_write_prop_key(e, "type");
				print_doc_type(e, module, base, false);
			}
			if (decl->var.kind == VARDECL_CONST)
			{
				json_write_prop_bool(e, "is_const", true);
			}
			if (decl->var.init_expr)
			{
				json_write_prop_key(e, "value");
				write_const_value_json(e->file, decl->var.init_expr);
			}
			break;
		case DECL_ATTRIBUTE:
			emit_attrdef_target_json(e, decl);
			break;
		case DECL_POISONED:
		case DECL_BODYPARAM:
		case DECL_CT_ASSERT:
		case DECL_CT_ECHO:
		case DECL_CT_EXEC:
		case DECL_CT_EXPAND:
		case DECL_CT_INCLUDE:
		case DECL_DECLARRAY:
		case DECL_CONTRACT:
		case DECL_ALIAS:
		case DECL_ALIAS_PATH:
		case DECL_ENUM_CONSTANT:
		case DECL_ERASED:
		case DECL_FAULT:
		case DECL_FNTYPE:
		case DECL_GROUP:
		case DECL_GENERIC:
		case DECL_GENERIC_INSTANCE:
		case DECL_IMPORT:
		case DECL_LABEL:
		case DECL_INTERFACE:
		case DECL_STRUCT:
		case DECL_UNION:
			break;
	}

	if (decl->decl_kind == DECL_ENUM)
	{
		if (vec_size(decl->enums.parameters) > 0)
		{
			json_start_array_prop(e, "associated_values");
			FOREACH_IDX(i, Decl *, p, decl->enums.parameters)
			{
				if (p) emit_param_json(e, module, p);
			}
			json_end_array(e);
		}
	}
	emit_doc_members_json(e, module, decl);
	emit_doc_comments(e, decl);
	emit_custom_attrs(e, decl);
	emit_normal_attrs(e, decl);
	json_end_object(e);
}

static DocCategory get_category_for_decl(Decl *decl)
{
	switch (decl->decl_kind)
	{
		case DECL_FUNC:
			if (decl->func_decl.type_parent) return DOC_CAT_METHODS;
			return DOC_CAT_FUNCTIONS;
		case DECL_MACRO:
			if (decl->func_decl.type_parent) return DOC_CAT_MACRO_METHODS;
			return DOC_CAT_MACROS;
		case DECL_ATTRIBUTE:
			return DOC_CAT_ATTRDEFS;
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

static bool emit_category_decls(JsonEmitter *e, Module *module, DocCategory cat)
{
	bool found = false;
	int unit_count = vec_size(module->units);
	for (int j = 0; j < unit_count; j++)
	{
		CompilationUnit *unit = module->units[j];
		Decl **lists[3];
		get_unit_lists(unit, cat, lists);

		for (int l = 0; l < 3; l++)
		{
			Decl **list = lists[l];
			if (!list) continue;
			FOREACH(Decl *, decl, list)
			{
				if (decl->is_templated || decl->decl_kind == DECL_GENERIC_INSTANCE) continue;
				if (get_category_for_decl(decl) != cat) continue;
				found = true;
				if (e) emit_decl_json(e, module, decl, NULL);
				else return true;
			}
		}

		int generic_count = vec_size(unit->generic_decls);
		for (int k = 0; k < generic_count; k++)
		{
			Decl *gdecl = unit->generic_decls[k];
			if (gdecl->decl_kind != DECL_GENERIC) continue;
			Decl **sub_lists[2] = {gdecl->generic_decl.decls, gdecl->generic_decl.conditional_decls};
			for (int list_idx = 0; list_idx < 2; list_idx++)
			{
				FOREACH(Decl *, decl, sub_lists[list_idx])
				{
					if (decl->is_templated || decl->decl_kind == DECL_GENERIC_INSTANCE) continue;
					if (get_category_for_decl(decl) != cat) continue;
					found = true;
					if (e) emit_decl_json(e, module, decl, (const char **)gdecl->generic_decl.parameters);
					else return true;
				}
			}
		}
	}
	return found;
}

static bool category_has_content(Module *module, DocCategory cat)
{
	return emit_category_decls(NULL, module, cat);
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
			const char *pos = strstr(existing, data_end_marker);
			if (!pos) error_exit("Could not find /*DATA_END*/ in existing docs.html for append.");
			fwrite(existing, 1, pos - existing, file);
		}
		else
		{
			const char *pos = strstr((const char *)docs_html, data_start_marker);
			if (!pos) error_exit("Internal error: Could not find /*DATA_START*/ in the docs.html template.");
			fwrite(docs_html, 1, (pos - (const char *)docs_html) + strlen(data_start_marker), file);
		}
		fprintf(file, "\n\t\tEMBEDDED_JSON_LIST.push({ target: \"%s\", data: ", target_str);
	}

	JsonEmitter emitter;
	json_init(&emitter, file);

	json_start_object(&emitter);
	all_modules = compiler.context.module_list;
	json_start_object_prop(&emitter, "modules");

	FOREACH(Module *, module, all_modules)
	{
		if (target->emit_stdlib == EMIT_STDLIB_OFF &&
			(module_is_stdlib(module) ||
			 (module->name->len == 11 && strcmp(module->name->module, "compiler_rt") == 0) ||
			 (module->name->len > 13 && memcmp(module->name->module, "compiler_rt::", 13) == 0))) continue;

		DeclId module_doc = 0;
		int unit_count = vec_size(module->units);
		FOREACH(CompilationUnit *, unit, module->units)
		{
			if (unit->module_doc)
			{
				module_doc = unit->module_doc;
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

		json_start_object_prop(&emitter, module->name->module);
		Decl *module_generic = NULL;
		FOREACH(CompilationUnit *, unit, module->units)
		{
			if (unit->default_generic_section)
			{
				module_generic = unit->default_generic_section;
				break;
			}
		}

		bool is_module_generic = module_generic != NULL;
		json_write_prop_bool(&emitter, "is_generic", is_module_generic);
		if (is_module_generic)
		{
			json_start_array_prop(&emitter, "generic_parameters");
			GenericDecl *g = &module_generic->generic_decl;
			FOREACH(const char *, name, g->parameters)
			{
				json_comma(&emitter);
				json_write_string(emitter.file, name);
			}
			json_end_array(&emitter);
		}

		if (module_doc) emit_doc_comments(&emitter, declptr(module_doc));

		for (int cat = 0; cat < DOC_CAT_COUNT; cat++)
		{
			if (!cat_has_content[cat]) continue;

			json_start_array_prop(&emitter, category_names[cat]);
			emit_category_decls(&emitter, module, (DocCategory)cat);
			json_end_array(&emitter);
		}
		json_end_object(&emitter);
	}
	json_end_object(&emitter);
	json_end_object(&emitter);

	if (!json_only)
	{
		fputs("});", file);
		if (existing)
		{
			const char *pos = strstr(existing, data_end_marker);
			fwrite(pos, 1, strlen(pos), file);
		}
		else
		{
			const char *pos = strstr((const char *)docs_html, data_end_marker);
			if (!pos) error_exit("Internal error: Could not find /*DATA_END*/ in the docs.html template.");
			fwrite(pos, 1, docs_html_len - (pos - (const char *)docs_html), file);
		}
		fclose(file);
		printf("Documentation generated to %s\n", out_name);
	}

	exit_compiler(COMPILER_SUCCESS_EXIT);
}
