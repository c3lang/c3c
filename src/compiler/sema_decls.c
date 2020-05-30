// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "sema_internal.h"
#include "bigint.h"


static inline bool sema_analyse_error(Context *context __unused, Decl *decl)
{
	Decl **constants = decl->error.error_constants;
	unsigned size = vec_size(constants);
	if (size > MAX_ERRORS)
	{
		SEMA_ERROR(decl, "More than %d errors declared in a single error type.", MAX_ERRORS);
		return false;
	}
	bool success = true;
	for (unsigned i = 0; i < size; i++)
	{
		Decl *constant = constants[i];
		for (unsigned j = 0; j < i; j++)
		{
			if (constant->name == constants[j]->name)
			{
				SEMA_ERROR(constant, "Duplicate error names, please remove one of them.");
				SEMA_PREV(constants[j], "The previous declaration was here.");
				decl_poison(constant);
				decl_poison(constants[j]);
				success = false;
				break;
			}
		}
		constant->type = decl->type;
		constant->error_constant.value = i + 1;
		constant->resolve_status = RESOLVE_DONE;
	}
	return success;
}


static inline void sema_set_struct_size(Decl *decl)
{
	// TODO packed
	uint64_t size = 0;
	uint64_t alignment = 0;
	VECEACH(decl->strukt.members, i)
	{
		Decl *member = decl->strukt.members[i];
		Type *canonical = member->type->canonical;
		uint64_t member_size = type_size(canonical);
		uint64_t member_alignment = type_abi_alignment(canonical);
		assert(member_size > 0);
		// Add padding.
		if (member_alignment && (size % member_alignment))
		{
			size += member_alignment - size % member_alignment;
		}
		// Add size.
		size += member_size;
		if (member_alignment > alignment) alignment = member_alignment;
	}
	decl->strukt.abi_alignment = alignment;
	if (alignment && size % alignment)
	{
		size += alignment - size % alignment;
	}
	decl->strukt.size = size;
}

static inline void sema_set_union_size(Decl *decl)
{
	uint64_t size = 0;
	uint64_t alignment = 0;
	VECEACH(decl->strukt.members, i)
	{
		Decl *member = decl->strukt.members[i];
		Type *canonical = member->type->canonical;
		uint64_t member_size = type_size(canonical);
		uint64_t member_alignment = type_abi_alignment(canonical);
		if (member_size > size) size = member_size;
		if (member_alignment > alignment) alignment = member_alignment;
	}
	decl->strukt.abi_alignment = alignment;
	decl->strukt.size = size;
}


static inline bool sema_analyse_struct_member(Context *context, Decl *decl)
{
	assert(decl->resolve_status == RESOLVE_NOT_DONE);
	decl->resolve_status = RESOLVE_RUNNING;
	if (decl->decl_kind == DECL_STRUCT || decl->decl_kind == DECL_UNION)
	{
		DEBUG_LOG("Beginning analysis of inner struct/union");
		VECEACH(decl->strukt.members, i)
		{
			Decl *member = decl->strukt.members[i];
			if (!decl_ok(member))
			{
				decl_poison(decl);
				continue;
			}
			if (!sema_analyse_struct_member(context, decl->strukt.members[i]))
			{
				if (decl_ok(decl))
				{
					decl_poison(decl);
					continue;
				}
				decl_poison(decl);
			}
		}
		if (decl->decl_kind == DECL_UNION)
		{
			sema_set_union_size(decl);
		}
		else
		{
			sema_set_struct_size(decl);
		}
		DEBUG_LOG("Analysis complete.");
		decl->resolve_status = RESOLVE_DONE;
		return decl_ok(decl);
	}
	assert(decl->decl_kind == DECL_VAR);
	assert(decl->var.kind == VARDECL_MEMBER);
	if (!sema_resolve_type_info(context, decl->var.type_info))
	{
		decl_poison(decl);
		return false;
	}
	decl->type = decl->var.type_info->type;
	assert(decl->var.type_info->type);
	decl->resolve_status = RESOLVE_DONE;
	return true;
}

static inline bool sema_analyse_struct_union(Context *context, Decl *decl)
{
	DEBUG_LOG("Beginning analysis of %s.", decl->name);
	assert(decl->decl_kind == DECL_STRUCT || decl->decl_kind == DECL_UNION);
	VECEACH(decl->strukt.members, i)
	{
		Decl *member = decl->strukt.members[i];
		if (!decl_ok(member))
		{
			decl_poison(decl);
			continue;
		}
		if (!sema_analyse_struct_member(context, member))
		{
			if (decl_ok(decl))
			{
				decl_poison(decl);
				continue;
			}
			decl_poison(decl);
		}
	}
	DEBUG_LOG("Analysis complete.");
	return decl_ok(decl);
}


static inline bool sema_analyse_function_param(Context *context, Decl *param, bool is_function, bool *has_default)
{
	*has_default = false;
	assert(param->decl_kind == DECL_VAR);
	assert(param->var.kind == VARDECL_PARAM);
	if (!sema_resolve_type_info(context, param->var.type_info))
	{
		return false;
	}
	param->type = param->var.type_info->type;
	if (param->var.init_expr && !is_function)
	{
		SEMA_ERROR(param->var.init_expr, "Function types may not have default arguments.");
		return false;
	}
	if (param->var.init_expr)
	{
		Expr *expr = param->var.init_expr;
		if (!sema_analyse_expr_of_required_type(context, param->type, expr)) return false;
		if (expr->expr_kind != EXPR_CONST)
		{
			SEMA_ERROR(expr, "Only constant expressions may be used as default values.");
			return false;
		}
		*has_default = true;
	}
	return true;
}

static inline Type *sema_analyse_function_signature(Context *context, FunctionSignature *signature, bool is_function)
{
	char buffer[MAX_FUNCTION_SIGNATURE_SIZE + 200];
	size_t buffer_write_offset = 0;
	bool all_ok = true;
	all_ok = sema_resolve_type_info(context, signature->rtype) && all_ok;
	if (all_ok)
	{
		type_append_signature_name(signature->rtype->type, buffer, &buffer_write_offset);
		buffer[buffer_write_offset++] = '(';
	}
	if (vec_size(signature->params) > MAX_PARAMS)
	{
		SEMA_ERROR(signature->params[MAX_PARAMS], "Number of params exceeds %d which is unsupported.", MAX_PARAMS);
		return false;
	}
	STable *names = &context->scratch_table;
	stable_clear(names);

	VECEACH(signature->params, i)
	{
		Decl *param = signature->params[i];
		assert(param->resolve_status == RESOLVE_NOT_DONE);
		param->resolve_status = RESOLVE_RUNNING;
		bool has_default;
		if (!sema_analyse_function_param(context, param, is_function, &has_default))
		{
			decl_poison(param);
			all_ok = false;
			continue;
		}
		signature->has_default = signature->has_default || has_default;
		param->resolve_status = RESOLVE_DONE;
		if (i > 0 && all_ok)
		{
			buffer[buffer_write_offset++] = ',';
		}
		type_append_signature_name(param->var.type_info->type, buffer, &buffer_write_offset);
		if (param->name)
		{
			Decl *prev = stable_set(names, param->name, param);
			if (prev)
			{
				SEMA_ERROR(param, "Duplicate parameter name %s.", param->name);
				SEMA_PREV(prev, "Previous use of the name was here.");
				decl_poison(prev);
				decl_poison(param);
				all_ok = false;
			}
		}
	}
	if (signature->variadic)
	{
		buffer[buffer_write_offset++] = ',';
		buffer[buffer_write_offset++] = '.';
		buffer[buffer_write_offset++] = '.';
		buffer[buffer_write_offset++] = '.';
	}
	buffer[buffer_write_offset++] = ')';
	if (signature->throw_any)
	{
		assert(!signature->throws);
		buffer[buffer_write_offset++] = '!';
	}
	if (vec_size(signature->throws))
	{
		buffer[buffer_write_offset++] = '!';
		VECEACH(signature->throws, i)
		{
			TypeInfo *err_decl = signature->throws[i];
			if (!sema_resolve_type_info(context, err_decl))
			{
				return false;
			}
			if (i > 0 && all_ok)
			{
				buffer[buffer_write_offset++] = '|';
			}
			type_append_signature_name(err_decl->type, buffer, &buffer_write_offset);
		}
	}

	unsigned error_types = vec_size(signature->throws);
	ErrorReturn error_return = ERROR_RETURN_NONE;
	if (signature->throw_any)
	{
		error_return = ERROR_RETURN_ANY;
	}
	else if (error_types)
	{
		 error_return = error_types > 1 ? ERROR_RETURN_MANY : ERROR_RETURN_ONE;
	}
	signature->error_return = error_return;

	Type *return_type = signature->rtype->type->canonical;
	signature->return_param = false;
	if (return_type->type_kind != TYPE_VOID)
	{
		// TODO fix this number with ABI compatibility
		if (signature->error_return != ERROR_RETURN_NONE || type_size(return_type) > 8 * 2)
		{
			signature->return_param = true;
		}
	}

	if (!all_ok) return NULL;
	TokenType type = TOKEN_INVALID_TOKEN;
	signature->mangled_signature = symtab_add(buffer, buffer_write_offset, fnv1a(buffer, buffer_write_offset), &type);
	Type *func_type = stable_get(&context->local_symbols, signature->mangled_signature);
	if (!func_type)
	{
		func_type = type_new(TYPE_FUNC, signature->mangled_signature);
		func_type->canonical = func_type;
		func_type->func.signature = signature;
		stable_set(&context->local_symbols, signature->mangled_signature, func_type);
	}
	return func_type;

}

static inline bool sema_analyse_typedef(Context *context, Decl *decl)
{
	if (decl->typedef_decl.is_func)
	{
		Type *func_type = sema_analyse_function_signature(context, &decl->typedef_decl.function_signature, false);
		if (!func_type) return false;
		decl->type->canonical = type_get_ptr(func_type);
		return true;
	}
	if (!sema_resolve_type_info(context, decl->typedef_decl.type_info)) return false;
	decl->type->canonical = decl->typedef_decl.type_info->type->canonical;
	// Do we need anything else?
	return true;
}

static inline bool sema_analyse_enum(Context *context, Decl *decl)
{
	// Resolve the type of the enum.
	if (!sema_resolve_type_info(context, decl->enums.type_info)) return false;

	Type *type = decl->enums.type_info->type;
	Type *canonical = type->canonical;

	// Require an integer type
	if (!type_is_integer(canonical))
	{
		SEMA_ERROR(decl->enums.type_info, "The enum type must be an integer type not '%s'.", type_to_error_string(type));
		return false;
	}

	DEBUG_LOG("* Enum type resolved to %s.", type->name);
	bool success = true;
	unsigned enums = vec_size(decl->enums.values);
	BigInt value;
	BigInt add;
	bigint_init_unsigned(&add, 1);
	bigint_init_unsigned(&value, 0);

	for (unsigned i = 0; i < enums; i++)
	{
		Decl *enum_value = decl->enums.values[i];
		enum_value->type = decl->type;
		DEBUG_LOG("* Checking enum constant %s.", enum_value->name);
		enum_value->enum_constant.ordinal = i;
		DEBUG_LOG("* Ordinal: %d", i);
		assert(enum_value->resolve_status == RESOLVE_NOT_DONE);
		assert(enum_value->decl_kind == DECL_ENUM_CONSTANT);

		// Start evaluating the constant
		enum_value->resolve_status = RESOLVE_RUNNING;
		Expr *expr = enum_value->enum_constant.expr;

		// Create a "fake" expression.
		// This will be evaluated later to catch the case
		if (!expr)
		{
			expr = expr_new(EXPR_CONST, enum_value->name_span);
			expr->type = type;
			expr->resolve_status = RESOLVE_NOT_DONE;
			bigint_init_bigint(&expr->const_expr.i, &value);
			expr->const_expr.kind = TYPE_IXX;
			expr->type = type_compint;
			enum_value->enum_constant.expr = expr;
		}

		// We try to convert to the desired type.
		if (!sema_analyse_expr_of_required_type(context, type, expr))
		{
			success = false;
			enum_value->resolve_status = RESOLVE_DONE;
			decl_poison(enum_value);
			// Reset!
			bigint_init_unsigned(&value, 0);
			continue;
		}

		assert(type_is_integer(expr->type->canonical));

		// Here we might have a non-constant value,
		if (expr->expr_kind != EXPR_CONST)
		{
			SEMA_ERROR(expr, "Expected a constant expression for enum.");
			decl_poison(enum_value);
			success = false;
			// Skip one value.
			continue;
		}

		// Update the value
		bigint_add(&value, &expr->const_expr.i, &add);
		DEBUG_LOG("* Value: %s", expr_const_to_error_string(&expr->const_expr));
		enum_value->resolve_status = RESOLVE_DONE;
	}
	return success;
}



static inline bool sema_analyse_method(Context *context, Decl *decl)
{
	TypeInfo *parent_type = decl->func.type_parent;
	if (!sema_resolve_type_info(context, parent_type)) return false;
	if (!type_may_have_method(parent_type->type))
	{
		SEMA_ERROR(decl,
		           "Methods can not be associated with '%s'",
		           type_to_error_string(decl->func.type_parent->type));
		return false;
	}
	Decl *parent = parent_type->type->decl;
	VECEACH(parent->methods, i)
	{
		Decl *function = parent->methods[i];
		if (function->name == decl->name)
		{
			SEMA_ERROR(decl, "Duplicate name '%s' for method.", function->name);
			SEMA_PREV(function, "Previous definition here.");
			return false;
		}
	}
	DEBUG_LOG("Method '%s.%s' analysed.", parent->name, decl->name);
	vec_add(parent->methods, decl);
	return true;
}

static inline AttributeType attribute_by_name(Attr *attr)
{
	const char *attribute = attr->name.string;
	for (unsigned i = 0; i < NUMBER_OF_ATTRIBUTES; i++)
	{
		if (attribute_list[i] == attribute) return (AttributeType)i;
	}
	return ATTRIBUTE_NONE;
}

static const char *attribute_domain_to_string(AttributeDomain domain)
{
	switch (domain)
	{
		case ATTR_FUNC:
			return "function";
		case ATTR_VAR:
			return "variable";
		case ATTR_ENUM:
			return "enum";
		case ATTR_STRUCT:
			return "struct";
		case ATTR_UNION:
			return "union";
		case ATTR_CONST:
			return "constant";
		case ATTR_ERROR:
			return "error type";
		case ATTR_TYPEDEF:
			return "typedef";
	}
	UNREACHABLE
}
static AttributeType sema_analyse_attribute(Context *context, Attr *attr, AttributeDomain domain)
{
	AttributeType type = attribute_by_name(attr);
	if (type == ATTRIBUTE_NONE)
	{
		SEMA_TOKEN_ERROR(attr->name, "There is no attribute with the name '%s', did you mistype?", attr->name.string);
		return ATTRIBUTE_NONE;
	}
	static AttributeDomain attribute_domain[NUMBER_OF_ATTRIBUTES] = {
			[ATTRIBUTE_WEAK] = ATTR_FUNC | ATTR_CONST | ATTR_VAR,
			[ATTRIBUTE_CNAME] = 0xFF,
			[ATTRIBUTE_SECTION] = ATTR_FUNC | ATTR_CONST | ATTR_VAR,
			[ATTRIBUTE_PACKED] = ATTR_STRUCT | ATTR_UNION,
			[ATTRIBUTE_NORETURN] = ATTR_FUNC,
			[ATTRIBUTE_ALIGN] = ATTR_FUNC | ATTR_CONST | ATTR_VAR | ATTR_STRUCT | ATTR_UNION,
			[ATTRIBUTE_INLINE] = ATTR_FUNC,
			[ATTRIBUTE_OPAQUE] = ATTR_STRUCT | ATTR_UNION
	};

	if ((attribute_domain[type] & domain) != domain)
	{
		SEMA_TOKEN_ERROR(attr->name, "'%s' is not a valid %s attribute.", attr->name.string, attribute_domain_to_string(domain));
		return ATTRIBUTE_NONE;
	}
	switch (type)
	{
		case ATTRIBUTE_ALIGN:
			if (!attr->expr)
			{
				SEMA_TOKEN_ERROR(attr->name, "'align' requires an power-of-2 argument, e.g. align(8).");
				return ATTRIBUTE_NONE;
			}
			if (!sema_analyse_expr(context, type_usize, attr->expr)) return false;
			if (attr->expr->expr_kind != EXPR_CONST || !type_is_any_integer(attr->expr->type->canonical))
			{
				SEMA_ERROR(attr->expr, "Expected a constant integer value as argument.");
				return ATTRIBUTE_NONE;
			}
			{
				BigInt comp;
				bigint_init_unsigned(&comp, MAX_ALIGNMENT);
				if (bigint_cmp(&attr->expr->const_expr.i, &comp) == CMP_GT)
				{
					SEMA_ERROR(attr->expr, "Alignment must be less or equal to %ull.", MAX_ALIGNMENT);
					return ATTRIBUTE_NONE;
				}
				if (bigint_cmp_zero(&attr->expr->const_expr.i) != CMP_GT)
				{
					SEMA_ERROR(attr->expr, "Alignment must be greater than zero.");
					return ATTRIBUTE_NONE;
				}
				uint64_t align = bigint_as_unsigned(&attr->expr->const_expr.i);
				if (!is_power_of_two(align))
				{
					SEMA_ERROR(attr->expr, "Alignment must be a power of two.");
					return ATTRIBUTE_NONE;
				}
				attr->alignment = align;
			}
			return type;
		case ATTRIBUTE_SECTION:
		case ATTRIBUTE_CNAME:
			if (!attr->expr)
			{
				SEMA_TOKEN_ERROR(attr->name, "'%s' requires a string argument, e.g. %s(\"foo\").", attr->name.string, attr->name.string);
				return ATTRIBUTE_NONE;
			}
			if (!sema_analyse_expr(context, NULL, attr->expr)) return false;
			if (attr->expr->expr_kind != EXPR_CONST || attr->expr->type->canonical != type_string)
			{
				SEMA_ERROR(attr->expr, "Expected a constant string value as argument.");
				return ATTRIBUTE_NONE;
			}
			return type;
		default:
			if (attr->expr)
			{
				SEMA_ERROR(attr->expr, "'%s' should not have any arguments.", attr->name.string);
				return ATTRIBUTE_NONE;
			}
			return type;
	}

}

static inline bool sema_analyse_func(Context *context, Decl *decl)
{
	DEBUG_LOG("----Analysing function %s", decl->name);
	Type *func_type = sema_analyse_function_signature(context, &decl->func.function_signature, true);
	decl->type = func_type;
	if (!func_type) return decl_poison(decl);
	if (decl->func.type_parent)
	{
		if (!sema_analyse_method(context, decl)) return decl_poison(decl);
	}
	VECEACH(decl->attributes, i)
	{
		Attr *attr = decl->attributes[i];

		AttributeType attribute = sema_analyse_attribute(context, attr, ATTR_FUNC);
		if (attribute == ATTRIBUTE_NONE) return decl_poison(decl);

		bool had = false;
#define SET_ATTR(_X) had = decl->func._X; decl->func._X = true; break
		switch (attribute)
		{
			case ATTRIBUTE_CNAME:
				had = decl->cname != NULL;
				decl->cname = attr->expr->const_expr.string.chars;
				break;
			case ATTRIBUTE_SECTION:
				had = decl->section != NULL;
				decl->section = attr->expr->const_expr.string.chars;
				break;
			case ATTRIBUTE_ALIGN:
				had = decl->alignment != 0;
				decl->alignment = attr->alignment;
				break;
			case ATTRIBUTE_NOINLINE: SET_ATTR(attr_noinline);
			case ATTRIBUTE_STDCALL: SET_ATTR(attr_stdcall);
			case ATTRIBUTE_INLINE: SET_ATTR(attr_inline);
			case ATTRIBUTE_NORETURN: SET_ATTR(attr_noreturn);
			case ATTRIBUTE_WEAK: SET_ATTR(attr_weak);
			default:
				UNREACHABLE
		}
#undef SET_ATTR
		if (had)
		{
			SEMA_TOKEN_ERROR(attr->name, "Attribute occurred twice, please remove one.");
			return decl_poison(decl);
		}
		if (decl->func.attr_inline && decl->func.attr_noinline)
		{
			SEMA_TOKEN_ERROR(attr->name, "A function cannot be 'inline' and 'noinline' at the same time.");
			return decl_poison(decl);
		}
	}
	if (decl->name == kw_main)
	{
		if (decl->visibility == VISIBLE_LOCAL)
		{
			SEMA_ERROR(decl, "'main' cannot have local visibility.");
			return false;
		}
		decl->visibility = VISIBLE_EXTERN;
	}
	DEBUG_LOG("Function analysis done.");
	return true;
}

static inline bool sema_analyse_macro(Context *context, Decl *decl)
{
	TypeInfo *rtype = decl->macro_decl.rtype;
	if (decl->macro_decl.rtype && !sema_resolve_type_info(context, rtype)) return false;
	VECEACH(decl->macro_decl.parameters, i)
	{
		Decl *param = decl->macro_decl.parameters[i];
		assert(param->decl_kind == DECL_VAR);
		assert(param->var.kind == VARDECL_PARAM);
		if (param->var.type_info && !sema_resolve_type_info(context, param->var.type_info)) return false;
	}
	return true;
}




static inline bool sema_analyse_global(Context *context, Decl *decl)
{
	if (!sema_resolve_type_info(context, decl->var.type_info)) return false;
	decl->type = decl->var.type_info->type;
	if (decl->var.init_expr)
	{
		if (!sema_analyse_expr_of_required_type(context, decl->type, decl->var.init_expr)) return false;
		if (decl->var.init_expr->expr_kind != EXPR_CONST)
		{
			SEMA_ERROR(decl->var.init_expr, "The expression must be a constant value.");
			return false;
		}
	}
	AttributeDomain domain = decl->var.kind == VARDECL_CONST ? ATTR_CONST : ATTR_FUNC;
	VECEACH(decl->attributes, i)
	{
		Attr *attr = decl->attributes[i];

		AttributeType attribute = sema_analyse_attribute(context, attr, domain);
		if (attribute == ATTRIBUTE_NONE) return decl_poison(decl);

		bool had = false;
#define SET_ATTR(_X) had = decl->func._X; decl->func._X = true; break
		switch (attribute)
		{
			case ATTRIBUTE_CNAME:
				had = decl->cname != NULL;
				decl->cname = attr->expr->const_expr.string.chars;
				break;
			case ATTRIBUTE_SECTION:
				had = decl->section != NULL;
				decl->section = attr->expr->const_expr.string.chars;
				break;
			case ATTRIBUTE_ALIGN:
				had = decl->alignment != 0;
				decl->alignment = attr->alignment;
				break;
			case ATTRIBUTE_WEAK: SET_ATTR(attr_weak);
			default:
				UNREACHABLE
		}
#undef SET_ATTR
		if (had)
		{
			SEMA_TOKEN_ERROR(attr->name, "Attribute occurred twice, please remove one.");
			return decl_poison(decl);
		}
	}

	switch (decl->var.kind)
	{
		case VARDECL_CONST:
			assert(decl->var.init_expr);
			return true;
		case VARDECL_GLOBAL:
			return true;
		default:
			UNREACHABLE
			break;
	}
}

static inline bool sema_analyse_generic(Context *context, Decl *decl)
{
	TODO
	return true;
}


bool sema_analyse_decl(Context *context, Decl *decl)
{
	if (decl->resolve_status == RESOLVE_DONE) return decl_ok(decl);

	DEBUG_LOG(">>> Analysing %s.", decl->name);
	if (decl->resolve_status == RESOLVE_RUNNING)
	{
		SEMA_ERROR(decl, "Recursive dependency on %s.", decl->name);
		decl_poison(decl);
		return false;
	}

	decl->resolve_status = RESOLVE_RUNNING;
	decl->module = context->module;
	switch (decl->decl_kind)
	{
		case DECL_STRUCT:
			if (!sema_analyse_struct_union(context, decl)) return decl_poison(decl);
			sema_set_struct_size(decl);
			decl_set_external_name(decl);
			break;
		case DECL_UNION:
			if (!sema_analyse_struct_union(context, decl)) return decl_poison(decl);
			sema_set_union_size(decl);
			decl_set_external_name(decl);
			break;
		case DECL_FUNC:
			if (!sema_analyse_func(context, decl)) return decl_poison(decl);
			decl_set_external_name(decl);
			break;
		case DECL_MACRO:
			if (!sema_analyse_macro(context, decl)) return decl_poison(decl);
			break;
		case DECL_VAR:
			if (!sema_analyse_global(context, decl)) return decl_poison(decl);
			decl_set_external_name(decl);
			break;
		case DECL_TYPEDEF:
			if (!sema_analyse_typedef(context, decl)) return decl_poison(decl);
			break;
		case DECL_ENUM:
			if (!sema_analyse_enum(context, decl)) return decl_poison(decl);
			decl_set_external_name(decl);
			break;
		case DECL_ERROR:
			if (!sema_analyse_error(context, decl)) return decl_poison(decl);
			decl_set_external_name(decl);
			break;
		case DECL_GENERIC:
			if (!sema_analyse_generic(context, decl)) return decl_poison(decl);
			break;
		case DECL_ATTRIBUTE:
			TODO
		case DECL_POISONED:
		case DECL_IMPORT:
		case DECL_ENUM_CONSTANT:
		case DECL_ERROR_CONSTANT:
		case DECL_ARRAY_VALUE:
		case DECL_CT_ELSE:
		case DECL_CT_ELIF:
			UNREACHABLE
		case DECL_CT_IF:
			// Handled elsewhere
			UNREACHABLE
	}
	decl->resolve_status = RESOLVE_DONE;
	DEBUG_LOG("<<< Analysis of %s successful.", decl->name);
	return true;
}
