// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"

#define OUTPUT(x, ...) fprintf(file, x, ## __VA_ARGS__)
#define INDENT() indent_line(file, indent)

static void indent_line(FILE *file, int indent)
{
	for (int i = 0; i < indent * 3; i++)
	{
		fputc(' ', file);
	}
}
static void header_gen_decl(FILE *file, int indent, Decl *decl);

static void header_gen_method(FILE *file, Context *c, Decl *decl)
{
	fprintf(file, "/* method */\n");
}

static void header_gen_function(FILE *file, Context *c, Decl *decl)
{
	fprintf(file, "/* function */\n");
}

static void header_print_type(FILE *file, Type *type)
{
	switch (type->type_kind)
	{
		case CT_TYPES:
			UNREACHABLE
		case TYPE_BITSTRUCT:
			TODO
		case TYPE_FAILABLE:
		case TYPE_FAILABLE_ANY:
			// If this is reachable then we are not doing the proper lowering.
			UNREACHABLE
		case TYPE_VOID:
			OUTPUT("void");
			return;
		case TYPE_BOOL:
			OUTPUT("bool");
			return;
		case TYPE_I8:
			OUTPUT("int8_t");
			return;
		case TYPE_I16:
			OUTPUT("int16_t");
			return;
		case TYPE_I32:
			OUTPUT("int32_t");
			return;
		case TYPE_I64:
			OUTPUT("int64_t");
			return;
		case TYPE_I128:
			OUTPUT("__int128");
			return;
		case TYPE_U8:
			OUTPUT("uint8_t");
			return;
		case TYPE_U16:
			OUTPUT("uint16_t");
			return;
		case TYPE_U32:
			OUTPUT("uint32_t");
			return;
		case TYPE_U64:
			OUTPUT("uint64_t");
			return;
		case TYPE_U128:
			OUTPUT("unsigned __int128");
			return;
		case TYPE_F16:
			OUTPUT("__fp16");
			return;
		case TYPE_F32:
			OUTPUT("float");
			return;
		case TYPE_F64:
			OUTPUT("double");
			return;
		case TYPE_F128:
			OUTPUT("__float128");
			return;
		case TYPE_TYPEID:
			OUTPUT("c3typeid_t");
			return;
		case TYPE_POINTER:
			header_print_type(file, type->pointer);
			OUTPUT("*");
			return;
		case TYPE_ENUM:
		case TYPE_ERRTYPE:
			OUTPUT("enum %s__", type->decl->external_name);
			return;
		case TYPE_FLEXIBLE_ARRAY:
			TODO
		case TYPE_FUNC:
			TODO
		case TYPE_STRUCT:
			OUTPUT("struct %s__", type->decl->external_name);
			return;
		case TYPE_UNION:
			OUTPUT("union %s__", type->decl->external_name);
			return;
		case TYPE_DISTINCT:
			header_print_type(file, type->decl->distinct_decl.base_type);
			return;
		case TYPE_ANYERR:
			break;
		case TYPE_TYPEDEF:
			break;
		case TYPE_ARRAY:
			break;
		case TYPE_ANY:
			TODO
		case TYPE_SUBARRAY:
			break;
		case TYPE_VECTOR:
			break;
	}
	TODO
}

static void header_gen_members(FILE *file, int indent, Decl **members)
{
	VECEACH(members, i)
	{
		Decl *member = members[i];
		if (member->decl_kind == DECL_VAR)
		{
			INDENT();
			header_print_type(file, member->type);
			OUTPUT(" %s;\n", member->name);
		}
		else
		{
			TODO
		}
	}
}
static void header_gen_struct(FILE *file, int indent, Decl *decl)
{
	if (!indent)
	{
		OUTPUT("typedef struct %s__ %s;\n", decl->external_name, decl->external_name);
	}
	INDENT();
	if (decl->name)
	{
		OUTPUT("struct %s__\n{\n", decl->external_name);
	}
	else
	{
		OUTPUT("struct\n{\n");
	}
	header_gen_members(file, indent + 1, decl->strukt.members);
	INDENT();
	OUTPUT("};\n");
}

static void header_gen_union(FILE *file, int indent, Decl *decl)
{
	OUTPUT("typedef union %s__ %s;\n", decl->external_name, decl->external_name);
	OUTPUT("union %s__\n{\n", decl->external_name);
	header_gen_members(file, indent, decl->strukt.members);
	OUTPUT("};\n");
}

static void header_gen_enum(FILE *file, int indent, Decl *decl)
{
	TODO
}

static void header_gen_err(FILE *file, int indent, Decl *decl)
{
	OUTPUT("typedef struct %s_error__ %s_error;\n", decl->external_name, decl->external_name);
	OUTPUT("struct %s_error__\n{\n", decl->external_name);
	header_gen_members(file, indent, decl->strukt.members);
	OUTPUT("};\n");
}


static void header_gen_decl(FILE *file, int indent, Decl *decl)
{
	switch (decl->decl_kind)
	{
		case NON_TYPE_DECLS:
		case DECL_ENUM_CONSTANT:
		case DECL_ERRVALUE:
		case DECL_POISONED:
		case DECL_VAR:
			UNREACHABLE
		case DECL_FUNC:
		case DECL_BITSTRUCT:
			TODO
		case DECL_TYPEDEF:
		case DECL_DISTINCT:
			TODO
		case DECL_STRUCT:
			header_gen_struct(file, indent, decl);
			return;
		case DECL_UNION:
			header_gen_union(file, indent, decl);
			return;
		case DECL_ENUM:
			header_gen_enum(file, indent, decl);
			return;
		case DECL_ERRTYPE:
			header_gen_err(file, indent, decl);
			return;
	}
	UNREACHABLE
}

static void header_gen_var(FILE *file, Context *c, Decl *decl)
{
	fprintf(file, "/* vars */\n");
}

void header_gen(Module *module)
{
	TODO
	Context *context = module->contexts[0];
	const char *filename = strcat_arena(context->file->name, ".h");
	FILE *file = fopen(filename, "w");
	OUTPUT("#include <stdint.h>\n");
	OUTPUT("#ifndef __c3__\n");
	OUTPUT("#define __c3__\n");
	OUTPUT("typedef ");
	header_print_type(file, type_flatten(type_typeid));
	OUTPUT(" c3typeid_t;\n");
	OUTPUT("#endif\n");
	VECEACH(context->types, i)
	{
		header_gen_decl(file, 0, context->types[i]);
	}

	VECEACH(context->vars, i)
	{
		header_gen_var(file, context, context->vars[i]);
	}
	VECEACH(context->methods, i)
	{
		header_gen_method(file, context, context->methods[i]);
	}
	VECEACH(context->functions, i)
	{
		header_gen_function(file, context, context->functions[i]);
	}
	fclose(file);
}
