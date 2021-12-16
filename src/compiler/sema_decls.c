// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "sema_internal.h"




static bool sema_analyse_struct_union(Context *context, Decl *decl);

static bool sema_check_section(Context *context, Decl *decl, Attr *attr)
{
	const char *section_string = attr->expr->const_expr.string.chars;
	decl->section = section_string;
	// No restrictions except for MACH-O
	if (platform_target.object_format != OBJ_FORMAT_MACHO)
	{
		return true;
	}
	scratch_buffer_clear();
	StringSlice slice = strtoslice(section_string);
	StringSlice segment = strnexttok(&slice, ',');
	StringSlice section = strnexttok(&slice, ',');
	StringSlice attrs = strnexttok(&slice, ',');
	StringSlice stub_size_str = strnexttok(&slice, ',');

	if (slice.len)
	{
		SEMA_ERROR(attr->expr, "Too many parts to the Mach-o section description.");
	}
	slicetrim(&segment);
	if (segment.len == 0)
	{
		SEMA_ERROR(attr->expr, "The segment is missing, did you type it correctly?");
		return false;
	}
	slicetrim(&section);
	if (section.len == 0)
	{
		SEMA_ERROR(attr->expr, "Mach-o requires 'segment,section' as the format, did you type it correctly?");
		return false;
	}
	if (section.len > 16)
	{
		SEMA_ERROR(attr->expr, "Mach-o requires the section to be at the most 16 characters, can you shorten it?");
		return false;
	}
	// TODO improve checking
	return true;
}
static bool sema_check_unique_parameters(Decl **decls)
{
	STable *table = &global_context.scratch_table;
	stable_clear(table);

	VECEACH(decls, i)
	{
		Decl *param = decls[i];
		if (param->name)
		{
			Decl *old = stable_set(table, param->name, param);
			if (old)
			{
				SEMA_ERROR(param, "Parameter name occurred twice, try renaming one of them.");
				return false;
			}
		}
	}
	return true;
}

static inline bool sema_analyse_struct_member(Context *context, Decl *decl)
{
	if (decl->name)
	{
		Decl *other = sema_resolve_symbol_in_current_dynamic_scope(context, decl->name);
		if (other)
		{
			SEMA_ERROR(decl, "Duplicate member name '%s'.", other->name);
			SEMA_PREV(other, "Previous declaration was here.");
			return false;
		}
		if (decl->name) sema_add_member(context, decl);
	}

	switch (decl->decl_kind)
	{
		case DECL_VAR:
			assert(decl->var.kind == VARDECL_MEMBER);
			decl->resolve_status = RESOLVE_RUNNING;
			if (!sema_resolve_type_info_maybe_inferred(context, decl->var.type_info, true)) return decl_poison(decl);
			decl->type = decl->var.type_info->type;
			decl->resolve_status = RESOLVE_DONE;
			Type *member_type = type_flatten_distinct(decl->type);
			if (member_type->type_kind == TYPE_ARRAY)
			{
				if (member_type->array.len == 0)
				{
					SEMA_ERROR(decl, "Zero length arrays are not valid members.");
					return false;
				}
			}
			return true;
		case DECL_STRUCT:
		case DECL_UNION:
		case DECL_BITSTRUCT:
			if (!sema_analyse_decl(context, decl)) return false;
			return true;
		default:
			UNREACHABLE
	}
}

static bool sema_analyse_union_members(Context *context, Decl *decl, Decl **members)
{
	AlignSize max_size = 0;
	MemberIndex max_alignment_element = 0;
	AlignSize max_alignment = 0;

	bool has_named_parameter = false;
	VECEACH(members, i)
	{
		Decl *member = members[i];
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
			continue;
		}
		if (member->type->type_kind == TYPE_INFERRED_ARRAY)
		{
			SEMA_ERROR(member, "Flexible array members not allowed in unions.");
			return false;
		}
		AlignSize member_alignment = type_abi_alignment(member->type);
		ByteSize member_size = type_size(member->type);
		assert(member_size <= MAX_TYPE_SIZE);
		// Update max alignment
		if (member_alignment > max_alignment)
		{
			max_alignment = member_alignment;
			max_alignment_element = (MemberIndex)i;
		}
		// Update max size
		if (member_size > max_size)
		{
			//max_size_element = i;
			max_size = (AlignSize)member_size;
			// If this is bigger than the previous with max
			// alignment, pick this as the maximum size field.
			if (max_alignment_element != (MemberIndex)i && max_alignment == member_alignment)
			{
				max_alignment_element = (MemberIndex)i;
			}
		}
		// Offset is always 0
		member->offset = 0;
	}

	if (!decl_ok(decl)) return false;

	// 1. If packed, then the alignment is zero, unless previously given
	if (decl->is_packed && !decl->alignment) decl->alignment = 1;

	// 2. Otherwise pick the highest of the natural alignment and the given alignment.
	if (!decl->is_packed) decl->alignment = MAX(decl->alignment, max_alignment);

	// We're only packed if the max alignment is > 1
	decl->is_packed = decl->is_packed && max_alignment > 1;

	decl->strukt.union_rep = max_alignment_element;

	// All members share the same alignment
	VECEACH(members, i)
	{
		members[i]->alignment = decl->alignment;
	}

	if (!max_size)
	{
		decl->strukt.size = 0;
		decl->alignment = 1;
		return true;
	}

	// The actual size might be larger than the max size due to alignment.
	AlignSize size = aligned_offset(max_size, decl->alignment);

	ByteSize rep_size = type_size(members[max_alignment_element]->type);

	// If the actual size is bigger than the real size, add
	// padding.
	if (size > rep_size)
	{
		decl->strukt.padding = (AlignSize)(size - rep_size);
	}

	decl->strukt.size = size;

	return true;
}

static bool sema_analyse_struct_members(Context *context, Decl *decl, Decl **members)
{
	// Default alignment is 1 even if it is empty.
	AlignSize natural_alignment = 1;
	bool is_unaligned = false;
	AlignSize size = 0;
	AlignSize offset = 0;
	bool is_packed = decl->is_packed;
	unsigned member_count = vec_size(members);
	for (unsigned i = 0; i < member_count; i++)
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
			continue;
		}
		Type *member_type = type_flatten_distinct(member->type);
		if (member_type->type_kind == TYPE_STRUCT && member_type->decl->has_variable_array)
		{
			if (i != member_count - 1)
			{
				SEMA_ERROR(member, "A struct member with a flexible array must be the last element.");
				return false;
			}
			decl->has_variable_array = true;
		}
		if (member_type->type_kind == TYPE_INFERRED_ARRAY)
		{
			if (i != member_count - 1)
			{
				SEMA_ERROR(member, "The flexible array member must be the last element.");
				return false;
			}
			if (i == 0)
			{
				SEMA_ERROR(member, "The flexible array member cannot be the only element.");
				return false;
			}
			member->type = type_get_flexible_array(member->type->array.base);
			decl->has_variable_array = true;
		}

		if (!decl_ok(decl)) return false;

		AlignSize member_natural_alignment = type_abi_alignment(member->type);
		AlignSize member_alignment = is_packed ? 1 : member_natural_alignment;
		Attr **attributes = member->attributes;
		unsigned count = vec_size(attributes);
		for (unsigned j = 0; j < count; j++)
		{
			Attr *attribute = attributes[j];
			if (!sema_analyse_attribute(context, attribute, ATTR_GLOBAL)) return false;
			if (TOKSTR(attribute->name) == kw_align)
			{
				member_alignment = attribute->alignment;
				// Update total alignment if we have a member that has bigger alignment.
				if (member_alignment > decl->alignment) decl->alignment = member_alignment;
			}
		}

		// If the member alignment is higher than the currently detected alignment,
		// then we update the natural alignment
		if (member_natural_alignment > natural_alignment)
		{
			natural_alignment = member_natural_alignment;
		}

		// In the case of a struct, we will align this to the next offset,
		// using the alignment of the member.
		unsigned align_offset = aligned_offset(offset, member_alignment);

		unsigned natural_align_offset = aligned_offset(offset, member_natural_alignment);

		// If the natural align is different from the aligned offset we have two cases:
		if (natural_align_offset != align_offset)
		{
			// If the natural alignment is greater, in this case the struct is unaligned.
			if (member_natural_alignment > member_alignment)
			{
				assert(natural_align_offset > align_offset);
				is_unaligned = true;
			}
			else
			{
				// Otherwise we have a greater offset, and in this case
				// we add padding for the difference.
				assert(natural_align_offset < align_offset);
				member->padding = align_offset - offset;
			}
		}

		member->alignment = member_alignment;

		offset = align_offset;
		member->offset = offset;
		offset += type_size(member->type);
	}

	// Set the alignment:

	// 1. If packed, use the alignment given, otherwise set to 1.
	if (decl->is_packed && !decl->alignment) decl->alignment = 1;

	// 2. Otherwise pick the highest of the natural alignment and the given alignment.
	if (!decl->is_packed) decl->alignment = MAX(decl->alignment, natural_alignment);

	// We must now possibly add the end padding.
	// First we calculate the actual size
	size = aligned_offset(offset, decl->alignment);

	// We might get a size that is greater than the natural alignment
	// in this case we need an additional padding
	if (size > aligned_offset(offset, natural_alignment))
	{
		decl->strukt.padding = (AlignSize)(size - offset);
	}

	// If the size is smaller the naturally aligned struct, then it is also unaligned
	if (size < aligned_offset(offset, natural_alignment))
	{
		is_unaligned = true;
	}
	if (is_unaligned && size > offset)
	{
		assert(!decl->strukt.padding);
		decl->strukt.padding = (AlignSize)(size - offset);
	}
	decl->is_packed = is_unaligned;
	decl->strukt.size = size;
	return true;
}

static bool sema_analyse_struct_union(Context *context, Decl *decl)
{
	AttributeDomain domain;
	switch (decl->decl_kind)
	{
		case DECL_STRUCT:
			domain = ATTR_STRUCT;
			break;
		case DECL_UNION:
			domain = ATTR_UNION;
			break;
		case DECL_ERRTYPE:
			domain = ATTR_ERROR;
			break;
		default:
			UNREACHABLE
	}
	VECEACH(decl->attributes, i)
	{
		Attr *attr = decl->attributes[i];

		AttributeType attribute = sema_analyse_attribute(context, attr, domain);
		if (attribute == ATTRIBUTE_NONE) return decl_poison(decl);

		bool had = false;
#define SET_ATTR(_X) had = decl->func_decl._X; decl->func_decl._X = true; break
		switch (attribute)
		{
			case ATTRIBUTE_EXTNAME:
				had = decl->extname != NULL;
				decl->extname = attr->expr->const_expr.string.chars;
				break;
			case ATTRIBUTE_ALIGN:
				had = decl->alignment != 0;
				decl->alignment = attr->alignment;
				break;
			case ATTRIBUTE_PACKED:
				had = decl->is_packed;
				decl->is_packed = true;
				break;
			case ATTRIBUTE_OPAQUE:
				had = decl->is_opaque;
				decl->is_opaque = true;
				break;
			default:
				UNREACHABLE
		}
#undef SET_ATTR
		if (had)
		{
			SEMA_TOKID_ERROR(attr->name, "Attribute occurred twice, please remove one.");
			return decl_poison(decl);
		}
	}

	DEBUG_LOG("Beginning analysis of %s.", decl->name ? decl->name : "anon");
	bool success;
	Decl **members = decl->strukt.members;
	if (!vec_size(members))
	{
		SEMA_ERROR(decl, decl->decl_kind == DECL_UNION ? "Zero sized unions are not permitted." : "Zero sized structs are not permitted.");
		return false;
	}
	if (decl->name)
	{
		SCOPE_START
			if (decl->decl_kind == DECL_UNION)
			{
				success = sema_analyse_union_members(context, decl, decl->strukt.members);
			}
			else
			{
				success = sema_analyse_struct_members(context, decl, decl->strukt.members);
			}
		SCOPE_END;
	}
	else
	{
		if (decl->decl_kind == DECL_UNION)
		{
			success = sema_analyse_union_members(context, decl, decl->strukt.members);
		}
		else
		{
			success = sema_analyse_struct_members(context, decl, decl->strukt.members);
		}
	}
	DEBUG_LOG("Struct/union size %d, alignment %d.", (int)decl->strukt.size, (int)decl->alignment);
	DEBUG_LOG("Analysis complete.");
	if (!success) return decl_poison(decl);
	return decl_ok(decl);
}

static inline bool sema_analyse_bitstruct_member(Context *context, Decl *decl, unsigned index, bool allow_overlap)
{
	Decl **members = decl->strukt.members;
	Decl *member = members[index];
	if (!sema_resolve_type_info(context, member->var.type_info)) return false;
	Type *member_type = type_flatten_for_bitstruct(member->var.type_info->type);
	member->type = member->var.type_info->type;
	if (!type_is_integer(member_type) && member_type != type_bool)
	{
		SEMA_ERROR(member->var.type_info, "%s is not supported in a bitstruct, only enums, integer and boolean values may be used.",
				   type_quoted_error_string(member->var.type_info->type));
		return false;
	}
	BitSize bits = type_size(decl->bitstruct.base_type->type) * 8;
	Int max_bits = (Int) { .type = TYPE_I64, .i = { .low =  bits } };
	Expr *start = member->var.start;
	Expr *end = member->var.end;
	if (!sema_analyse_expr(context, start)) return false;
	unsigned start_bit;
	unsigned end_bit;
	if (start->expr_kind != EXPR_CONST || !type_is_integer(start->type) || int_is_neg(start->const_expr.ixx))
	{
		SEMA_ERROR(start, "This must be a constant non-negative integer value.");
		return false;
	}
	if (int_comp(start->const_expr.ixx, max_bits, BINARYOP_GE))
	{
		SEMA_ERROR(start, "Expected at the most a bit index of %d\n", bits - 1);
		return false;
	}
	end_bit = start_bit = (unsigned)start->const_expr.ixx.i.low;
	if (end)
	{
		if (!sema_analyse_expr(context, start)) return false;
		if (end->expr_kind != EXPR_CONST || !type_is_integer(end->type) || int_is_neg(end->const_expr.ixx))
		{
			SEMA_ERROR(end, "This must be a constant non-negative integer value.");
			return false;
		}
		if (int_comp(end->const_expr.ixx, max_bits, BINARYOP_GE))
		{
			SEMA_ERROR(end, "Expected at the most a bit index of %d.", bits - 1);
			return false;
		}
		end_bit = (unsigned)end->const_expr.ixx.i.low;
	}
	else
	{
		if (member_type->type_kind != TYPE_BOOL)
		{
			SEMA_ERROR(member, "Only booleans may use non-range indices, try using %d..%d instead.", start_bit, start_bit);
			return false;
		}
	}
	if (start_bit > end_bit)
	{
		SEMA_ERROR(start, "The start bit must be smaller than the end bit index.");
		return false;
	}
	TypeSize bitsize_type = member_type == type_bool ? 1 : type_size(member_type) * 8;
	TypeSize bits_available = end_bit + 1 - start_bit;
	if (bitsize_type < bits_available)
	{
		SEMA_ERROR(member, "The bit width of %s (%d) is less than the assigned bits (%d), try reducing the range.",
		           type_quoted_error_string(member->type), (int)bitsize_type, (int)bits_available);
		return false;
	}
	member->var.start_bit = start_bit;
	member->var.end_bit = end_bit;
	for (unsigned i = 0; i < index; i++)
	{
		Decl *other_member = members[i];
		if (member->name == other_member->name)
		{
			SEMA_ERROR(member, "Duplicate members with the name '%s'.", member->name);
			SEMA_PREV(other_member, "The other member was declared here.");
			return false;
		}
		if (allow_overlap) continue;
		// Check for overlap.
		if ((start_bit >= other_member->var.start_bit || end_bit >= other_member->var.start_bit)
			&& start_bit <= other_member->var.end_bit)
		{
			SEMA_ERROR(member, "Overlapping members, please use '@overlap' if this is intended.");
			SEMA_PREV(other_member, "The other member was declared here.");
			return false;
		}
	}
	member->resolve_status = RESOLVE_DONE;
	return true;
}

static bool sema_analyse_bitstruct(Context *context, Decl *decl)
{
	VECEACH(decl->attributes, i)
	{
		Attr *attr = decl->attributes[i];

		AttributeType attribute = sema_analyse_attribute(context, attr, ATTR_BITSTRUCT);
		if (attribute == ATTRIBUTE_NONE) return decl_poison(decl);

		bool had = false;
#define SET_ATTR(_X) had = decl->bitstruct._X; decl->bitstruct._X = true; break
		switch (attribute)
		{
			case ATTRIBUTE_OVERLAP:
				SET_ATTR(overlap);
				break;
			case ATTRIBUTE_OPAQUE:
				had = decl->is_opaque;
				decl->is_opaque = true;
				break;
			case ATTRIBUTE_BIGENDIAN:
				if (decl->bitstruct.little_endian)
				{
					SEMA_TOKID_ERROR(attr->name, "Attribute cannot be combined with @littleendian");
					return decl_poison(decl);
				}
				SET_ATTR(big_endian);
			case ATTRIBUTE_LITTLEENDIAN:
				if (decl->bitstruct.big_endian)
				{
					SEMA_TOKID_ERROR(attr->name, "Attribute cannot be combined with @bigendian");
					return decl_poison(decl);
				}
				SET_ATTR(little_endian);
			default:
				UNREACHABLE
		}
#undef SET_ATTR
		if (had)
		{
			SEMA_TOKID_ERROR(attr->name, "Attribute occurred twice, please remove one.");
			return decl_poison(decl);
		}
	}

	DEBUG_LOG("Beginning analysis of %s.", decl->name ? decl->name : "anon");
	if (!sema_resolve_type_info(context, decl->bitstruct.base_type)) return false;
	Type *type = decl->bitstruct.base_type->type->canonical;
	Type *base_type = type->type_kind == TYPE_ARRAY ? type->array.base : type;
	if (!type_is_integer(base_type))
	{
		SEMA_ERROR(decl->bitstruct.base_type, "The type of the bitstruct cannot be %s but must be an integer or an array of integers.",
		           type_quoted_error_string(decl->bitstruct.base_type->type));
		return false;
	}
	Decl **members = decl->bitstruct.members;
	bool success = true;
	SCOPE_START
		VECEACH(members, i)
		{
			if (!sema_analyse_bitstruct_member(context, decl, i, decl->bitstruct.overlap))
			{
				success = false;
				break;
			}
		}
	SCOPE_END;
	if (!success) return decl_poison(decl);
	return decl_ok(decl);
}


static inline bool sema_analyse_function_param(Context *context, Decl *param, bool is_function, bool *has_default)
{
	*has_default = false;
	assert(param->decl_kind == DECL_VAR);
	// We need to check that the parameters are not typeless nor are of any macro parameter kind.
	if (param->var.kind != VARDECL_PARAM)
	{
		SEMA_ERROR(param, "Only regular parameters are allowed for functions.");
		return false;
	}
	if (!param->var.type_info)
	{
		SEMA_ERROR(param, "Only typed parameters are allowed for functions.");
		return false;
	}
	if (!sema_resolve_type_info(context, param->var.type_info)) return false;

	if (param->var.vararg)
	{
		param->var.type_info->type = type_get_subarray(param->var.type_info->type);
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

		if (!sema_analyse_expr_rhs(context, param->type, expr, true)) return false;
		if (IS_FAILABLE(expr))
		{
			SEMA_ERROR(expr, "Default arguments may not be failable.");
			return false;
		}
		if (!expr_is_constant_eval(expr, CONSTANT_EVAL_ANY))
		{
			SEMA_ERROR(expr, "Only constant expressions may be used as default values.");
			return false;
		}
		*has_default = true;
	}
	param->alignment = type_abi_alignment(param->type);
	return true;
}

static inline Type *sema_analyse_function_signature(Context *context, FunctionSignature *signature, bool is_real_function)
{
	bool all_ok = true;
	all_ok = sema_resolve_type_info(context, signature->rtype) && all_ok;
	if (vec_size(signature->params) > MAX_PARAMS)
	{
		SEMA_ERROR(signature->params[MAX_PARAMS], "Number of params exceeds %d which is unsupported.", MAX_PARAMS);
		return NULL;
	}
	STable *names = &global_context.scratch_table;
	stable_clear(names);

	VECEACH(signature->params, i)
	{
		Decl *param = signature->params[i];
		assert(param->resolve_status == RESOLVE_NOT_DONE);
		param->resolve_status = RESOLVE_RUNNING;
		bool has_default;
		if (!sema_analyse_function_param(context, param, is_real_function, &has_default))
		{
			decl_poison(param);
			all_ok = false;
			continue;
		}
		signature->has_default = signature->has_default || has_default;
		param->resolve_status = RESOLVE_DONE;
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

	if (!all_ok) return NULL;
	c_abi_func_create(signature);
	return type_find_function_type(signature);
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
	Type *type = decl->typedef_decl.type_info->type->canonical;
	if (type == type_anyerr || type == type_any)
	{
		SEMA_ERROR(decl->typedef_decl.type_info, "%s may not be aliased.", type_quoted_error_string(type));
		return false;
	}
	decl->type->canonical = type;
	// Do we need anything else?
	return true;
}

static inline bool sema_analyse_distinct(Context *context, Decl *decl)
{
	if (decl->distinct_decl.typedef_decl.is_func)
	{
		Type *func_type = sema_analyse_function_signature(context, &decl->distinct_decl.typedef_decl.function_signature, false);
		if (!func_type) return false;
		decl->distinct_decl.base_type = type_get_ptr(func_type);
		return true;
	}
	TypeInfo *info = decl->distinct_decl.typedef_decl.type_info;
	if (!sema_resolve_type_info(context, info)) return false;
	Type *base = type_flatten_distinct(info->type);
	decl->distinct_decl.base_type = base;
	switch (base->type_kind)
	{
		case TYPE_FUNC:
		case TYPE_TYPEDEF:
		case TYPE_DISTINCT:
		case CT_TYPES:
		case TYPE_FLEXIBLE_ARRAY:
			UNREACHABLE
			return false;
		case TYPE_FAILABLE_ANY:
		case TYPE_FAILABLE:
			SEMA_ERROR(decl, "You cannot create a distinct type from a failable.");
			return false;
		case TYPE_ERRTYPE:
			SEMA_ERROR(decl, "You cannot create a distinct type from an error type.");
			return false;
		case TYPE_ANYERR:
			SEMA_ERROR(decl, "You cannot create a distinct type from an error union.");
			return false;
			case TYPE_ANY:
		case TYPE_VOID:
		case TYPE_TYPEID:
			SEMA_ERROR(decl, "Cannot create a distinct type from %s.", type_quoted_error_string(base));
		case TYPE_BOOL:
		case ALL_INTS:
		case ALL_FLOATS:
		case TYPE_POINTER:
		case TYPE_ENUM:
		case TYPE_BITSTRUCT:
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_ARRAY:
		case TYPE_SUBARRAY:
		case TYPE_VECTOR:
			break;
	}
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
	Int128 value = { 0, 0 };

	Decl **enum_values = decl->enums.values;
	for (unsigned i = 0; i < enums; i++)
	{
		Decl *enum_value = enum_values[i];
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
			expr = expr_new(EXPR_CONST, source_span_from_token_id(enum_value->name_token));
			expr->type = type;
			expr->resolve_status = RESOLVE_NOT_DONE;
			expr->const_expr.ixx = (Int) { value, canonical->type_kind };
			if (expr_const_will_overflow(&expr->const_expr, canonical->type_kind))
			{
				SEMA_ERROR(enum_value,
				           "The enum value would implicitly be %s which does not fit in %s.",
				           i128_to_string(value, 10, type_is_signed(canonical)),
				           type_quoted_error_string(type));
				return false;
			}
			expr->const_expr.const_kind = CONST_INTEGER;
			expr->const_expr.narrowable = true;
			expr->type = canonical;
			enum_value->enum_constant.expr = expr;
		}

		// We try to convert to the desired type.
		if (!sema_analyse_expr_rhs(context, type, expr, false))
		{
			success = false;
			enum_value->resolve_status = RESOLVE_DONE;
			decl_poison(enum_value);
			// Reset!
			value = (Int128) { 0, 0 };
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
		value = i128_add64(expr->const_expr.ixx.i, 1);
		DEBUG_LOG("* Value: %s", expr_const_to_error_string(&expr->const_expr));
		enum_value->resolve_status = RESOLVE_DONE;
	}
	return success;
}

static inline bool sema_analyse_error(Context *context, Decl *decl)
{
	bool success = true;
	unsigned enums = vec_size(decl->enums.values);

	for (unsigned i = 0; i < enums; i++)
	{
		Decl *enum_value = decl->enums.values[i];
		enum_value->type = decl->type;
		DEBUG_LOG("* Checking error value %s.", enum_value->name);
		enum_value->enum_constant.ordinal = i;
		DEBUG_LOG("* Ordinal: %d", i);
		assert(enum_value->resolve_status == RESOLVE_NOT_DONE);
		assert(enum_value->decl_kind == DECL_ERRVALUE);

		// Start evaluating the constant
		enum_value->resolve_status = RESOLVE_DONE;
	}
	return success;
}

static inline const char *name_by_decl(Decl *method_like)
{
	switch (method_like->decl_kind)
	{
		case DECL_MACRO:
			return "macro method";
		case DECL_FUNC:
			return "method";
		case DECL_GENERIC:
			return "generic method";
		default:
			UNREACHABLE
	}
}


static inline void find_operator_parameters(Decl *method, TypeInfo **rtype_ptr, Decl ***params_ptr)
{
	switch (method->decl_kind)
	{
		case DECL_GENERIC:
			*params_ptr = method->generic_decl.parameters;
			*rtype_ptr = method->generic_decl.rtype;
			break;
		case DECL_MACRO:
			*params_ptr = method->macro_decl.parameters;
			*rtype_ptr = method->macro_decl.rtype;
			break;
		case DECL_FUNC:
			*params_ptr = method->func_decl.function_signature.params;
			*rtype_ptr = method->func_decl.function_signature.rtype;
			break;
		default:
			UNREACHABLE
	}

}


static bool sema_analyse_operator_common(Context *context, Decl *method, TypeInfo **rtype_ptr, Decl ***params_ptr, uint32_t parameters)
{
	find_operator_parameters(method, rtype_ptr, params_ptr);
	Decl **params = *params_ptr;
	uint32_t param_count = vec_size(params);
	if (param_count > parameters)
	{
		SEMA_ERROR(params[parameters], "Too many parameters, '%s' expects only %u.", method->name, (unsigned)parameters);
		return false;
	}
	if (param_count < parameters)
	{
		SEMA_TOKID_ERROR(method->name_token, "Not enough parameters, '%s' requires %u.", method->name, (unsigned)parameters);
		return false;
	}

	if (*rtype_ptr == NULL)
	{
		SEMA_TOKID_ERROR(method->name_token, "The return value must be explicitly typed for '%s'.", method->name);
		return false;
	}
	VECEACH(params, i)
	{
		Decl *param = params[i];
		if (!params[i]->var.type_info)
		{
			SEMA_TOKID_ERROR(param->name_token, "All parameters must be explicitly typed for '%s'.", method->name);
			return false;
		}
	}
	return true;
}

Decl *sema_find_operator(Context *context, Expr *expr, const char *kw)
{
	Decl *ambiguous = NULL;
	Decl *private = NULL;
	Decl *method = type_may_have_sub_elements(expr->type) ? sema_resolve_method(context, expr->type->decl, kw, &ambiguous, &private) : NULL;
	if (!decl_ok(method)) return NULL;
	if (!method)
	{
		if (ambiguous)
		{
			SEMA_ERROR(expr,
					   "It's not possible to find a definition for '%s' on %s that is not ambiguous.",
					   kw, type_quoted_error_string(expr->type));
			return NULL;
		}
		if (private)
		{
			SEMA_ERROR(expr,
					   "It's not possible to find a public definition for '%s' with '%s'.",
					   kw, type_quoted_error_string(expr->type));
			return NULL;
		}
		return NULL;
	}
	return method;
}

static inline bool sema_analyse_operator_element_at(Context *context, Decl *method)
{
	TypeInfo *rtype;
	Decl **params;
	if (!sema_analyse_operator_common(context, method, &rtype, &params, 2)) return false;
	if (rtype->type->canonical == type_void)
	{
		SEMA_ERROR(rtype, "The return type cannot be 'void'.");
		return false;
	}
	return true;
}

static inline bool sema_analyse_operator_len(Context *context, Decl *method)
{
	TypeInfo *rtype;
	Decl **params;
	if (!sema_analyse_operator_common(context, method, &rtype, &params, 1)) return false;
	if (!type_is_integer(rtype->type))
	{
		SEMA_ERROR(rtype, "The return type must be an integer type.");
		return false;
	}
	return true;
}

static bool sema_check_operator_method_validity(Context *context, Decl *method)
{
	if (method->name == kw_operator_element_at || method->name == kw_operator_element_at_ref)
	{
		return sema_analyse_operator_element_at(context, method);
	}
	if (method->name == kw_operator_len)
	{
		return sema_analyse_operator_len(context, method);
	}
	return true;
}

static inline bool sema_add_method_like(Context *context, Type *parent_type, Decl *method_like)
{
	assert(parent_type->canonical == parent_type);
	Decl *parent = parent_type->decl;
	const char *name = method_like->name;
	Decl *method = sema_find_extension_method_in_module(context->module, parent_type, name);
	if (method)
	{
		SEMA_TOKID_ERROR(method_like->name_token, "This %s is already defined in this module.", name_by_decl(method_like));
		SEMA_TOKID_PREV(method->name_token, "The previous definition was here.");
		return false;
	}
	Decl *ambiguous = NULL;
	Decl *private = NULL;
	method = sema_resolve_method(context, parent, name, &ambiguous, &private);
	if (method)
	{
		SEMA_TOKID_ERROR(method_like->name_token, "This %s is already defined for '%s'.", name_by_decl(method_like), parent_type->name);
		SEMA_TOKID_PREV(method->name_token, "The previous definition was here.");
		return false;
	}
	if (!sema_check_operator_method_validity(context, method_like)) return false;
	scratch_buffer_clear();
	if (method_like->visibility <= VISIBLE_MODULE)
	{
		scratch_buffer_append(parent->name);
		scratch_buffer_append_char('.');
		scratch_buffer_append(method_like->name);
	}
	else
	{
		scratch_buffer_append(parent->external_name);
		scratch_buffer_append("__");
		scratch_buffer_append(method_like->name);
	}
	method_like->external_name = scratch_buffer_interned();
	DEBUG_LOG("Method-like '%s.%s' analysed.", parent->name, method_like->name);
	if (parent->module == context->module)
	{
		vec_add(parent->methods, method_like);
	}
	else
	{
		vec_add(context->module->method_extensions, method_like);
	}
	return true;

}

static inline bool sema_analyse_method(Context *context, Decl *decl)
{
	TypeInfo *parent_type = decl->func_decl.type_parent;
	if (!sema_resolve_type_info(context, parent_type)) return false;
	if (!type_may_have_sub_elements(parent_type->type))
	{
		SEMA_ERROR(decl,
				   "Methods can not be associated with '%s'",
		           type_to_error_string(decl->func_decl.type_parent->type));
		return false;
	}
	Type *type = parent_type->type->canonical;
	return sema_add_method_like(context, type, decl);
}

static inline AttributeType attribute_by_name(Attr *attr)
{
	const char *attribute = TOKSTR(attr->name);
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
		case ATTR_LOCAL:
			return "local variable";
		case ATTR_BITSTRUCT:
			return "bitstruct";
		case ATTR_INTERFACE:
			return "interface";
		case ATTR_MEMBER:
			return "member";
		case ATTR_FUNC:
			return "function";
		case ATTR_GLOBAL:
			return "global variable";
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
		case ATTR_CALL:
			return "call";
	}
	UNREACHABLE
}
AttributeType sema_analyse_attribute(Context *context, Attr *attr, AttributeDomain domain)
{
	AttributeType type = attribute_by_name(attr);
	if (type == ATTRIBUTE_NONE)
	{
		SEMA_TOKID_ERROR(attr->name, "There is no attribute with the name '%s', did you mistype?", TOKSTR(attr->name));
		return ATTRIBUTE_NONE;
	}
	static AttributeDomain attribute_domain[NUMBER_OF_ATTRIBUTES] = {
			[ATTRIBUTE_WEAK] = ATTR_FUNC | ATTR_CONST | ATTR_GLOBAL,
			[ATTRIBUTE_EXTNAME] = (AttributeDomain)~ATTR_CALL,
			[ATTRIBUTE_SECTION] = ATTR_FUNC | ATTR_CONST | ATTR_GLOBAL,
			[ATTRIBUTE_PACKED] = ATTR_STRUCT | ATTR_UNION,
			[ATTRIBUTE_NORETURN] = ATTR_FUNC,
			[ATTRIBUTE_ALIGN] = ATTR_FUNC | ATTR_CONST | ATTR_LOCAL | ATTR_GLOBAL | ATTR_STRUCT | ATTR_UNION | ATTR_MEMBER,
			[ATTRIBUTE_INLINE] = ATTR_FUNC | ATTR_CALL,
			[ATTRIBUTE_NOINLINE] = ATTR_FUNC | ATTR_CALL,
			[ATTRIBUTE_OPAQUE] = ATTR_STRUCT | ATTR_UNION | ATTR_BITSTRUCT,
			[ATTRIBUTE_BIGENDIAN] = ATTR_BITSTRUCT,
			[ATTRIBUTE_LITTLEENDIAN] = ATTR_BITSTRUCT,
			[ATTRIBUTE_USED] = (AttributeDomain)~ATTR_CALL,
			[ATTRIBUTE_UNUSED] = (AttributeDomain)~ATTR_CALL,
			[ATTRIBUTE_NAKED] = ATTR_FUNC,
			[ATTRIBUTE_CDECL] = ATTR_FUNC,
			[ATTRIBUTE_STDCALL] = ATTR_FUNC,
			[ATTRIBUTE_VECCALL] = ATTR_FUNC,
			[ATTRIBUTE_REGCALL] = ATTR_FUNC,
			[ATTRIBUTE_FASTCALL] = ATTR_FUNC,
			[ATTRIBUTE_OVERLAP] = ATTR_BITSTRUCT
	};

	if ((attribute_domain[type] & domain) != domain)
	{
		SEMA_TOKID_ERROR(attr->name, "'%s' is not a valid %s attribute.", TOKSTR(attr->name), attribute_domain_to_string(domain));
		return ATTRIBUTE_NONE;
	}
	switch (type)
	{
		case ATTRIBUTE_CDECL:
		case ATTRIBUTE_FASTCALL:
		case ATTRIBUTE_STDCALL:
		case ATTRIBUTE_VECCALL:
		case ATTRIBUTE_REGCALL:
			return type;
		case ATTRIBUTE_ALIGN:
			if (!attr->expr)
			{
				SEMA_TOKID_ERROR(attr->name, "'align' requires an power-of-2 argument, e.g. align(8).");
				return ATTRIBUTE_NONE;
			}
			if (!sema_analyse_expr(context, attr->expr)) return false;
			if (attr->expr->expr_kind != EXPR_CONST || !type_is_integer(attr->expr->type->canonical))
			{
				SEMA_ERROR(attr->expr, "Expected a constant integer value as argument.");
				return ATTRIBUTE_NONE;
			}
			{
				if (int_ucomp(attr->expr->const_expr.ixx, MAX_ALIGNMENT, BINARYOP_GT))
				{
					SEMA_ERROR(attr->expr, "Alignment must be less or equal to %ull.", MAX_ALIGNMENT);
					return ATTRIBUTE_NONE;
				}
				if (int_ucomp(attr->expr->const_expr.ixx, 0, BINARYOP_LE))
				{
					SEMA_ERROR(attr->expr, "Alignment must be greater than zero.");
					return ATTRIBUTE_NONE;
				}
				uint64_t align = int_to_u64(attr->expr->const_expr.ixx);
				if (!is_power_of_two(align))
				{
					SEMA_ERROR(attr->expr, "Alignment must be a power of two.");
					return ATTRIBUTE_NONE;
				}

				attr->alignment = (AlignSize)align;
			}
			return type;
		case ATTRIBUTE_SECTION:
		case ATTRIBUTE_EXTNAME:
			if (context->module->is_generic)
			{
				SEMA_TOKID_ERROR(attr->name, "'extname' attributes are not allowed in generic modules.");
				return false;
			}
			if (!attr->expr)
			{
				SEMA_TOKID_ERROR(attr->name, "'%s' requires a string argument, e.g. %s(\"foo\").", TOKSTR(attr->name), TOKSTR(attr->name));
				return ATTRIBUTE_NONE;
			}
			if (!sema_analyse_expr(context, attr->expr)) return false;
			if (attr->expr->expr_kind != EXPR_CONST || attr->expr->const_expr.const_kind != CONST_STRING)
			{
				SEMA_ERROR(attr->expr, "Expected a constant string value as argument.");
				return ATTRIBUTE_NONE;
			}
			return type;
		default:
			if (attr->expr)
			{
				SEMA_ERROR(attr->expr, "'%s' should not have any arguments.", TOKSTR(attr->name));
				return ATTRIBUTE_NONE;
			}
			return type;
	}

}

static inline bool sema_analyse_doc_header(Ast *docs, Decl **params, Decl **extra_params)
{
	if (!docs) return true;
	assert(docs->ast_kind == AST_DOCS);
	Ast **doc_directives = docs->directives;
	VECEACH(doc_directives, i)
	{
		Ast *directive = doc_directives[i];
		if (directive->doc_directive.kind != DOC_DIRECTIVE_PARAM) continue;
		TokenId param = directive->doc_directive.param.param;
		const char *param_name = TOKSTR(param);
		VECEACH(params, j)
		{
			if (params[j]->name == param_name) goto NEXT;
		}
		VECEACH(extra_params, j)
		{
			if (extra_params[j]->name == param_name) goto NEXT;
		}
		SEMA_TOKID_ERROR(param, "There is no parameter '%s', did you misspell it?", param_name);
		return false;
		NEXT:
		continue;
	}
	return true;
}
static inline bool sema_update_call_convention(Decl *decl, CallABI abi)
{
	bool had = decl->func_decl.function_signature.call_abi > 0;
	decl->func_decl.function_signature.call_abi = abi;
	return had;
}

static inline bool sema_analyse_main_function(Context *context, Decl *decl)
{
	if (decl == context->main_function) return true;

	if (decl->visibility == VISIBLE_LOCAL)
	{
		SEMA_ERROR(decl, "A main function may not have local visibility.");
		return false;
	}
	FunctionSignature *signature = &decl->func_decl.function_signature;
	Type *rtype = type_flatten_distinct(signature->rtype->type);
	bool is_int_return = true;
	bool is_err_return = false;
	if (rtype->type_kind == TYPE_FAILABLE_ANY) is_err_return = true;
	if (!is_err_return && type_is_failable(rtype))
	{
		if (rtype->failable->type_kind != TYPE_VOID)
		{
			SEMA_ERROR(signature->rtype, "The return type of 'main' cannot be a failable, unless it is 'void!'.");
			return false;
		}
		is_int_return = false;
		is_err_return = true;
	}
	if (rtype->type_kind == TYPE_VOID) is_int_return = false;

	if (type_is_integer(rtype) && rtype != type_cint())
	{
		SEMA_ERROR(signature->rtype, "Expected a return type of 'void' or %s.", type_quoted_error_string(type_cint()));
		return false;
	}
	// At this point the style is either MAIN_INT_VOID, MAIN_VOID_VOID or MAIN_ERR_VOID
	Decl **params = signature->params;
	unsigned param_count = vec_size(params);
	bool subarray_param = false;
	bool cparam = false;
	switch (param_count)
	{
		case 0:
			// This is the default style already set
			break;
		case 1:
			if (type_flatten_distinct(params[0]->type) != type_get_subarray(type_get_subarray(type_char)))
			{
				SEMA_ERROR(params[0], "Expected a parameter of type 'char[][]'");
				return false;
			}
			subarray_param = true;
			break;
		case 2:
			if (type_flatten_distinct(params[0]->type) != type_cint())
			{
				SEMA_ERROR(params[0], "Expected a parameter of type %s for a C-style main.", type_quoted_error_string(type_cint()));
				return false;
			}
			if (type_flatten_distinct(params[1]->type) != type_get_ptr(type_get_ptr(type_char)))
			{
				SEMA_ERROR(params[1], "Expected a parameter of type 'char**' for a C-style main.");
				return false;
			}
			cparam = true;
			break;
		default:
			SEMA_ERROR(params[0], "Expected zero, 1 or 2 parameters for main.");
			return false;
	}
	if (!subarray_param && is_int_return)
	{
		// Int return is pass-through at the moment.
		decl->visibility = VISIBLE_EXTERN;
		return true;
	}
	Decl *function = decl_new(DECL_FUNC, decl->name_token, VISIBLE_EXTERN);
	function->name = kw_mainstub;
	function->extname = kw_main;
	function->func_decl.function_signature.rtype = type_info_new_base(type_cint(), decl->span);
	Decl *param1 = decl_new_generated_var(kw_argv, type_cint(), VARDECL_PARAM, decl->span);
	Decl *param2 = decl_new_generated_var(kw_argc, type_get_ptr(type_get_ptr(type_char)), VARDECL_PARAM, decl->span);
	Decl **main_params = NULL;
	vec_add(main_params, param1);
	vec_add(main_params, param2);
	function->func_decl.function_signature.params = main_params;
	Ast *body = new_ast(AST_COMPOUND_STMT, decl->span);
	Ast *ret_stmt = new_ast(AST_RETURN_STMT, decl->span);
	Expr *call = expr_new(EXPR_CALL, decl->span);
	call->call_expr.function = expr_variable(decl);
	if (subarray_param)
	{
		Expr *subarray = expr_new(EXPR_ARGV_TO_SUBARRAY, decl->span);
		subarray->argv_expr.argc = param1;
		subarray->argv_expr.argv = param2;
		vec_add(call->call_expr.arguments, subarray);
	}
	else if (cparam)
	{
		vec_add(call->call_expr.arguments, expr_variable(param1));
		vec_add(call->call_expr.arguments, expr_variable(param2));
	}
	// Unresolve them or the params cannot be resolved later.
	param1->resolve_status = RESOLVE_NOT_DONE;
	param2->resolve_status = RESOLVE_NOT_DONE;
	if (is_int_return)
	{
		ret_stmt->return_stmt.expr = call;
	}
	else if (is_err_return)
	{
		Expr *try_expr = expr_new(EXPR_TRY, decl->span);
		try_expr->inner_expr = call;
		Expr *not_expr = expr_new(EXPR_UNARY, decl->span);
		not_expr->unary_expr.expr = try_expr;
		not_expr->unary_expr.operator = UNARYOP_NOT;
		Expr *cast_expr = expr_new(EXPR_CAST, decl->span);
		cast_expr->cast_expr.expr = not_expr;
		cast_expr->cast_expr.type_info = type_info_new_base(type_cint(), decl->span);
		ret_stmt->return_stmt.expr = cast_expr;
	}
	else
	{
		Ast *stmt = new_ast(AST_EXPR_STMT, decl->span);
		stmt->expr_stmt = call;
		vec_add(body->compound_stmt.stmts, stmt);
		Expr *c = expr_new(EXPR_CONST, decl->span);
		c->type = type_cint();
		expr_const_set_int(&c->const_expr, 0, c->type->type_kind);
		c->resolve_status = RESOLVE_DONE;
		ret_stmt->expr_stmt = c;
	}
	vec_add(body->compound_stmt.stmts, ret_stmt);
	function->func_decl.body = body;
	context->main_function = function;
	return true;
}
static inline bool sema_analyse_func(Context *context, Decl *decl)
{
	DEBUG_LOG("----Analysing function %s", decl->name);

	VECEACH(decl->attributes, i)
	{
		Attr *attr = decl->attributes[i];

		AttributeType attribute = sema_analyse_attribute(context, attr, ATTR_FUNC);
		if (attribute == ATTRIBUTE_NONE) return decl_poison(decl);

		bool had = false;
#define SET_ATTR(_X) had = decl->func_decl._X; decl->func_decl._X = true; break

		switch (attribute)
		{
			case ATTRIBUTE_EXTNAME:
				had = decl->extname != NULL;
				decl->extname = attr->expr->const_expr.string.chars;
				break;
			case ATTRIBUTE_SECTION:
				had = decl->section != NULL;
				if (!sema_check_section(context, decl, attr)) return decl_poison(decl);
				break;
			case ATTRIBUTE_ALIGN:
				had = decl->alignment != 0;
				decl->alignment = attr->alignment;
				break;
			case ATTRIBUTE_NOINLINE: SET_ATTR(attr_noinline);
			case ATTRIBUTE_STDCALL:
				if (platform_target.arch == ARCH_TYPE_X86 || platform_target.arch == ARCH_TYPE_X86_64)
				{
					had = sema_update_call_convention(decl, CALL_X86_STD);
				}
				else if (platform_target.arch == ARCH_TYPE_ARM || platform_target.arch == ARCH_TYPE_ARMB)
				{
					had = sema_update_call_convention(decl, CALL_AAPCS);
				}
				break;
			case ATTRIBUTE_CDECL:
				had = sema_update_call_convention(decl, CALL_C);
				break;
			case ATTRIBUTE_VECCALL:
				switch (platform_target.arch)
				{
					case ARCH_TYPE_X86_64:
					case ARCH_TYPE_X86:
						had = sema_update_call_convention(decl, CALL_X86_VECTOR);
						break;
					case ARCH_TYPE_ARM:
					case ARCH_TYPE_ARMB:
					case ARCH_TYPE_AARCH64:
					case ARCH_TYPE_AARCH64_32:
					case ARCH_TYPE_AARCH64_BE:
						had = sema_update_call_convention(decl, CALL_AAPCS_VFP);
						break;
					default:
						break;
				}
				break;
			case ATTRIBUTE_FASTCALL:
				if (platform_target.arch == ARCH_TYPE_X86 || (platform_target.arch == ARCH_TYPE_X86_64 && platform_target.os == OS_TYPE_WIN32))
				{
					had = sema_update_call_convention(decl, CALL_X86_FAST);
				}
				break;
			case ATTRIBUTE_REGCALL:
				had = decl->func_decl.function_signature.call_abi > 0;
				if (platform_target.arch == ARCH_TYPE_X86 || (platform_target.arch == ARCH_TYPE_X86_64 && platform_target.os == OS_TYPE_WIN32))
				{
					had = sema_update_call_convention(decl, CALL_X86_REG);
				}
				break;
			case ATTRIBUTE_INLINE: SET_ATTR(attr_inline);
			case ATTRIBUTE_NORETURN: SET_ATTR(attr_noreturn);
			case ATTRIBUTE_WEAK: SET_ATTR(attr_weak);
			case ATTRIBUTE_NAKED: SET_ATTR(attr_naked);
			default:
				UNREACHABLE
		}
#undef SET_ATTR
		if (had)
		{
			SEMA_TOKID_ERROR(attr->name, "Attribute occurred twice, please remove one.");
			return decl_poison(decl);
		}
		if (decl->func_decl.attr_inline && decl->func_decl.attr_noinline)
		{
			SEMA_TOKID_ERROR(attr->name, "A function cannot be 'inline' and 'noinline' at the same time.");
			return decl_poison(decl);
		}
	}

	Type *func_type = sema_analyse_function_signature(context, &decl->func_decl.function_signature, true);
	
	decl->type = func_type;
	if (!func_type) return decl_poison(decl);
	if (decl->func_decl.type_parent)
	{
		if (!sema_analyse_method(context, decl)) return decl_poison(decl);
	}
	else
	{
		if (decl->name == kw_main)
		{
			if (!sema_analyse_main_function(context, decl)) return decl_poison(decl);
		}
		decl_set_external_name(decl);
	}
	if (!sema_analyse_doc_header(decl->docs, decl->func_decl.function_signature.params, NULL)) return decl_poison(decl);
	DEBUG_LOG("Function analysis done.");
	return true;
}

static bool sema_analyse_macro_method(Context *context, Decl *decl)
{
	if (!sema_resolve_type_info(context, decl->macro_decl.type_parent)) return false;
	Type *parent_type = decl->macro_decl.type_parent->type;
	if (!type_may_have_sub_elements(parent_type))
	{
		SEMA_ERROR(decl->macro_decl.type_parent,
		           "Methods can not be associated with '%s'",
		           type_to_error_string(parent_type));
		return false;
	}
	if (!vec_size(decl->macro_decl.parameters))
	{
		SEMA_ERROR(decl, "Expected at least one parameter - of type &%s.", type_to_error_string(parent_type));
		return false;
	}
	Decl *first_param = decl->macro_decl.parameters[0];
	if (!first_param->type || first_param->type->canonical != parent_type->canonical)
	{
		SEMA_ERROR(first_param, "The first parameter must be &%s.", type_to_error_string(parent_type));
		return false;
	}
	if (first_param->var.kind != VARDECL_PARAM_REF)
	{
		SEMA_ERROR(first_param, "The first parameter must be a ref (&) parameter.");
		return false;
	}
	return sema_add_method_like(context, parent_type, decl);
}

static inline bool sema_analyse_macro(Context *context, Decl *decl)
{
	bool is_generic = decl->decl_kind == DECL_GENERIC;
	TypeInfo *rtype = decl->macro_decl.rtype;
	if (decl->macro_decl.rtype && !sema_resolve_type_info(context, rtype)) return decl_poison(decl);
	decl->macro_decl.context = context;
	VECEACH(decl->macro_decl.parameters, i)
	{
		Decl *param = decl->macro_decl.parameters[i];
		param->resolve_status = RESOLVE_RUNNING;
		assert(param->decl_kind == DECL_VAR);
		switch (param->var.kind)
		{
			case VARDECL_PARAM_EXPR:
			case VARDECL_PARAM_CT:
			case VARDECL_PARAM_REF:
				if (is_generic)
				{
					SEMA_ERROR(param, "Only regular parameters and type parameters are allowed for generic functions.");
					return decl_poison(decl);
				}
				FALLTHROUGH;
			case VARDECL_PARAM:
				if (param->var.type_info)
				{
					if (!sema_resolve_type_info(context, param->var.type_info)) return decl_poison(decl);
					param->type = param->var.type_info->type;
				}
				break;
			case VARDECL_PARAM_CT_TYPE:
				if (param->var.type_info)
				{
					SEMA_ERROR(param->var.type_info, "A compile time type parameter cannot have a type itself.");
					return decl_poison(decl);
				}
				break;
			case VARDECL_CONST:
			case VARDECL_GLOBAL:
			case VARDECL_LOCAL:
			case VARDECL_MEMBER:
			case VARDECL_BITMEMBER:
			case VARDECL_LOCAL_CT:
			case VARDECL_LOCAL_CT_TYPE:
			case VARDECL_UNWRAPPED:
			case VARDECL_REWRAPPED:
			case VARDECL_ERASE:
				UNREACHABLE
		}
		param->resolve_status = RESOLVE_DONE;
	}
	if (is_generic && vec_size(decl->macro_decl.body_parameters))
	{
		SEMA_ERROR(decl->macro_decl.body_parameters[0], "Trailing block syntax is not allowed for generic functions.");
		return decl_poison(decl);
	}
	VECEACH(decl->macro_decl.body_parameters, i)
	{
		Decl *param = decl->macro_decl.body_parameters[i];
		param->resolve_status = RESOLVE_RUNNING;
		assert(param->decl_kind == DECL_VAR);
		switch (param->var.kind)
		{
			case VARDECL_PARAM:
				if (param->var.type_info && !sema_resolve_type_info(context, param->var.type_info)) return decl_poison(decl);
				break;
			case VARDECL_PARAM_EXPR:
			case VARDECL_PARAM_CT:
			case VARDECL_PARAM_REF:
			case VARDECL_PARAM_CT_TYPE:
				SEMA_ERROR(param, "Only plain variables are allowed as body parameters.");
				return decl_poison(decl);
			case VARDECL_CONST:
			case VARDECL_GLOBAL:
			case VARDECL_LOCAL:
			case VARDECL_MEMBER:
			case VARDECL_BITMEMBER:
			case VARDECL_LOCAL_CT:
			case VARDECL_LOCAL_CT_TYPE:
			case VARDECL_UNWRAPPED:
			case VARDECL_REWRAPPED:
			case VARDECL_ERASE:
				UNREACHABLE
		}
		param->resolve_status = RESOLVE_DONE;
	}
	if (!sema_check_unique_parameters(decl->macro_decl.parameters)) return decl_poison(decl);
	if (!sema_check_unique_parameters(decl->macro_decl.body_parameters)) return decl_poison(decl);
	if (!sema_analyse_doc_header(decl->docs, decl->macro_decl.parameters, decl->macro_decl.body_parameters)) return decl_poison(decl);
	if (decl->macro_decl.type_parent)
	{
		if (!sema_analyse_macro_method(context, decl)) return decl_poison(decl);
	}
	decl->type = type_void;
	return true;
}


bool sema_analyse_attributes_for_var(Context *context, Decl *decl)
{

	AttributeDomain domain;
	switch (decl->var.kind)
	{
		case VARDECL_CONST:
			domain = ATTR_CONST;
			break;
		case VARDECL_GLOBAL:
			domain = ATTR_GLOBAL;
			break;
		default:
			domain = ATTR_LOCAL;
			break;
	}
	VECEACH(decl->attributes, i)
	{
		Attr *attr = decl->attributes[i];

		AttributeType attribute = sema_analyse_attribute(context, attr, domain);
		if (attribute == ATTRIBUTE_NONE) return decl_poison(decl);

		bool had = false;
#define SET_ATTR(_X) had = decl->func_decl._X; decl->func_decl._X = true; break
		switch (attribute)
		{
			case ATTRIBUTE_EXTNAME:
				had = decl->extname != NULL;
				decl->extname = attr->expr->const_expr.string.chars;
				break;
			case ATTRIBUTE_SECTION:
				had = decl->section != NULL;
				if (!sema_check_section(context, decl, attr)) return decl_poison(decl);
				break;
			case ATTRIBUTE_ALIGN:
				had = decl->alignment != 0;
				decl->alignment = attr->alignment;
				break;
			case ATTRIBUTE_WEAK:
				SET_ATTR(attr_weak);
			default:
				UNREACHABLE
		}
#undef SET_ATTR
		if (had)
		{
			SEMA_TOKID_ERROR(attr->name, "Attribute occurred twice, please remove one.");
			return decl_poison(decl);
		}
	}
	return true;
}

bool sema_analyse_decl_type(Context *context, Type *type, SourceSpan span)
{
	if (type == type_void)
	{
		sema_error_range(span, "The use of 'void' as a variable type is not permitted.");
		return false;
	}

	if (!type_is_failable(type)) return true;
	if (type_is_failable_any(type) || type_flatten_distinct(type->failable) == type_void)
	{
		sema_error_range(span, "The use of 'void!' as a variable type is not permitted, use %s instead.",
		                 type_quoted_error_string(type_anyerr));
		return false;
	}
	return true;
}
/**
 * Analyse a regular global or local declaration, e.g. int x = 123
 */
bool sema_analyse_var_decl(Context *context, Decl *decl, bool local)
{
	assert(decl->decl_kind == DECL_VAR && "Unexpected declaration type");

	// We expect a constant to actually be parsed correctly so that it has a value, so
	// this should always be true.
	assert(decl->var.type_info || decl->var.kind == VARDECL_CONST);

	bool is_global = decl->var.kind == VARDECL_GLOBAL || !local;

	if (!sema_analyse_attributes_for_var(context, decl)) return false;

	if (is_global)
	{

	}
	else
	{
		// Add a local to the current context, will throw error on shadowing.
		if (!sema_add_local(context, decl)) return decl_poison(decl);
	}

	// 1. Local constants: const int FOO = 123.
	if (decl->var.kind == VARDECL_CONST)
	{
		Expr *init_expr = decl->var.init_expr;
		// 1a. We require an init expression.
		if (!init_expr)
		{
			SEMA_ERROR(decl, "Constants need to have an initial value.");
			return false;
		}
		// 1b. We require defined constants
		if (init_expr->expr_kind == EXPR_UNDEF)
		{
			SEMA_ERROR(decl, "Constants cannot be undefined.");
			return false;
		}
		if (!decl->var.type_info)
		{
			if (!sema_analyse_expr(context, init_expr)) return false;
			decl->type = init_expr->type;
			if (!decl->alignment) decl->alignment = type_alloca_alignment(decl->type);
			if (!sema_analyse_decl_type(context, decl->type, init_expr->span)) return false;
			// Skip further evaluation.
			goto EXIT_OK;
		}
	}

	if (!sema_resolve_type_info_maybe_inferred(context, decl->var.type_info, decl->var.init_expr != NULL)) return decl_poison(decl);

	decl->type = decl->var.type_info->type;
	if (!sema_analyse_decl_type(context, decl->type, decl->var.type_info->span)) return false;
	if (decl->var.is_static)
	{
		scratch_buffer_clear();
		scratch_buffer_append(context->active_function_for_analysis->name);
		scratch_buffer_append_char('.');
		scratch_buffer_append(decl->name);
		decl->external_name = scratch_buffer_interned();
	}

	if (decl->var.init_expr)
	{
		bool type_is_inferred = decl->type->type_kind == TYPE_INFERRED_ARRAY;
		Expr *init = decl->var.init_expr;
		// Handle explicit undef
		if (init->expr_kind == EXPR_UNDEF)
		{
			if (type_is_inferred)
			{
				SEMA_ERROR(decl->var.type_info, "Size of the array cannot be inferred with explicit undef.");
				return false;
			}
			goto EXIT_OK;
		}

		if (!type_is_inferred)
		{
			// Pre resolve to avoid problem with recursive definitions.
			decl->resolve_status = RESOLVE_DONE;
			if (!decl->alignment) decl->alignment = type_alloca_alignment(decl->type);
		}

		if (!sema_expr_analyse_assign_right_side(context, NULL, decl->type, init, false)) return decl_poison(decl);

		if (type_is_inferred)
		{
			Type *right_side_type = init->type->canonical;
			assert(right_side_type->type_kind == TYPE_ARRAY);
			decl->type = type_get_array(decl->type->array.base, right_side_type->array.len);
		}

		Expr *init_expr = decl->var.init_expr;

		// 2. Check const-ness
		if ((is_global || decl->var.is_static) && !expr_is_constant_eval(init_expr, CONSTANT_EVAL_ANY))
		{
			SEMA_ERROR(init_expr, "The expression must be a constant value.");
		}
		else
		{
			if (decl->var.unwrap && IS_FAILABLE(init))
			{
				SEMA_ERROR(decl->var.init_expr, "A failable expression was expected here.");
				return decl_poison(decl);
			}
		}
		if (init_expr->expr_kind == EXPR_CONST)
		{
			init_expr->const_expr.narrowable = false;
			init_expr->const_expr.is_hex = false;
		}
	}
	EXIT_OK:
	if (!decl->alignment) decl->alignment = type_alloca_alignment(decl->type);
	return true;
}



static Context *copy_context(Module *module, Context *c)
{
	Context *copy = context_create(c->file);
	copy->imports = copy_decl_list(c->imports);
	copy->global_decls = copy_decl_list(c->global_decls);
	copy->module = module;
	assert(!c->functions && !c->macro_methods && !c->methods && !c->enums && !c->ct_ifs && !c->types && !c->external_symbol_list);
	return copy;
}

static Module *sema_instantiate_module(Context *context, Module *module, Path *path, TypeInfo **parms)
{
	Module *new_module = compiler_find_or_create_module(path, NULL, module->is_private);
	new_module->is_generic = true;
	Context **contexts = module->contexts;
	VECEACH(contexts, i)
	{
		vec_add(new_module->contexts, copy_context(new_module, contexts[i]));
	}
	Context *first_context = new_module->contexts[0];
	VECEACH(module->parameters, i)
	{
		TokenId param = module->parameters[i];
		Decl *decl = decl_new_with_type(param, DECL_TYPEDEF, VISIBLE_PUBLIC);
		decl->resolve_status = RESOLVE_DONE;
		TypeInfo *type_info = parms[i];
		assert(type_info->resolve_status == RESOLVE_DONE);
		decl->typedef_decl.type_info = type_info;
		decl->type->name = decl->name;
		decl->type->canonical = type_info->type->canonical;
		vec_add(first_context->global_decls, decl);
	}
	return new_module;
}

static bool sema_analyse_parameterized_define(Context *c, Decl *decl)
{
	Path *decl_path;
	TokenId name;
	switch (decl->define_decl.define_kind)
	{
		case DEFINE_IDENT_GENERIC:
			decl_path = decl->define_decl.path;
			name = decl->define_decl.identifier;
			break;
		case DEFINE_TYPE_GENERIC:
		{
			TypeInfo *define_type = decl->define_decl.type_info;
			if (define_type->resolve_status == RESOLVE_DONE && type_is_user_defined(define_type->type))
			{
				SEMA_ERROR(define_type, "Expected a user defined type for parameterization.");
				return decl_poison(decl);
			}
			decl_path = define_type->unresolved.path;
			name = define_type->unresolved.name_loc;
			break;
		}
		case DEFINE_ATTRIBUTE:
			TODO
		default:
			UNREACHABLE
	}
	Decl *alias = sema_resolve_parameterized_symbol(c, name, decl_path);
	if (!decl_ok(alias))
	{
		return decl_poison(decl);
	}

	Module *module = alias->module;
	TypeInfo **params = decl->define_decl.generic_params;
	unsigned parameter_count = vec_size(module->parameters);
	assert(parameter_count > 0);
	if (parameter_count != vec_size(params))
	{
		sema_error_range((SourceSpan) { params[0]->span.loc, VECLAST(params)->span.end_loc }, "The generic module expected %d arguments, but you only supplied %d, did you make a mistake?",
		           parameter_count, vec_size(decl->define_decl.generic_params));
		return decl_poison(decl);
	}
	scratch_buffer_clear();
	scratch_buffer_append_len(module->name->module, module->name->len);
	scratch_buffer_append_char('.');
	VECEACH(decl->define_decl.generic_params, i)
	{
		TypeInfo *type_info = decl->define_decl.generic_params[i];
		if (!sema_resolve_type_info(c, type_info)) return decl_poison(decl);
		if (i != 0) scratch_buffer_append_char('.');
		const char *type_name = type_info->type->canonical->name;
		scratch_buffer_append(type_name);
	}
	TokenType ident_type = TOKEN_IDENT;
	const char *path_string = scratch_buffer_interned();
	Module *instantiated_module = global_context_find_module(path_string);
	if (!instantiated_module)
	{
		Path *path = CALLOCS(Path);
		path->module = path_string;
		path->span = module->name->span;
		path->len = global_context.scratch_buffer_len;
		instantiated_module = sema_instantiate_module(c, module, path, decl->define_decl.generic_params);
		sema_analyze_stage(instantiated_module, c->module->stage);
	}
	if (global_context.errors_found) return decl_poison(decl);
	const char *name_str = TOKSTR(name);
	Decl *symbol = module_find_symbol(instantiated_module, name_str);
	assert(symbol);
	context_register_external_symbol(c, symbol);
	switch (decl->define_decl.define_kind)
	{
		case DEFINE_IDENT_GENERIC:
			decl->define_decl.alias = symbol;
			return true;
		case DEFINE_TYPE_GENERIC:
		{
			Type *type = type_new(TYPE_TYPEDEF, decl->name);
			decl->type = type;
			decl->decl_kind = DECL_TYPEDEF;
			type->canonical = symbol->type->canonical;
			return true;
		}
		case DEFINE_ATTRIBUTE:
			TODO
		default:
			UNREACHABLE
	}
}

static inline bool sema_analyse_define(Context *c, Decl *decl)
{
	// 1. The plain define
	if (decl->define_decl.define_kind == DEFINE_IDENT_ALIAS)
	{
		Decl *symbol = sema_resolve_normal_symbol(c,
		                                   decl->define_decl.identifier,
		                                   decl->define_decl.path,
		                                   true);
		if (!decl_ok(symbol)) return false;
		decl->type = symbol->type;
		decl->define_decl.alias = symbol;
		return true;
	}

	// 2. Handle type generics.
	return sema_analyse_parameterized_define(c, decl);
}




bool sema_analyse_decl(Context *context, Decl *decl)
{
	if (decl->resolve_status == RESOLVE_DONE) return decl_ok(decl);

	DEBUG_LOG(">>> Analysing %s.", decl->name ? decl->name : "anon");
	if (decl->resolve_status == RESOLVE_RUNNING)
	{
		SEMA_ERROR(decl, "Recursive definition of '%s'.", decl->name ? decl->name : "anon");
		decl_poison(decl);
		return false;
	}

	decl->resolve_status = RESOLVE_RUNNING;
	decl->module = context->module;
	switch (decl->decl_kind)
	{
		case DECL_BITSTRUCT:
			if (!sema_analyse_bitstruct(context, decl)) return decl_poison(decl);
			decl_set_external_name(decl);
			break;
		case DECL_STRUCT:
		case DECL_UNION:
			if (!sema_analyse_struct_union(context, decl)) return decl_poison(decl);
			decl_set_external_name(decl);
			break;
		case DECL_FUNC:
			if (!sema_analyse_func(context, decl)) return decl_poison(decl);
			break;
		case DECL_MACRO:
		case DECL_GENERIC:
			if (!sema_analyse_macro(context, decl)) return decl_poison(decl);
			break;
		case DECL_VAR:
			if (!sema_analyse_var_decl(context, decl, false)) return decl_poison(decl);
			decl_set_external_name(decl);
			break;
		case DECL_DISTINCT:
			if (!sema_analyse_distinct(context, decl)) return decl_poison(decl);
			decl_set_external_name(decl);
			break;
		case DECL_TYPEDEF:
			if (!sema_analyse_typedef(context, decl)) return decl_poison(decl);
			break;
		case DECL_ENUM:
			if (!sema_analyse_enum(context, decl)) return decl_poison(decl);
			decl_set_external_name(decl);
			break;
		case DECL_ERRTYPE:
			if (!sema_analyse_error(context, decl)) return decl_poison(decl);
			decl_set_external_name(decl);
			break;
		case DECL_DEFINE:
			if (!sema_analyse_define(context, decl)) return decl_poison(decl);
			break;
		case DECL_ATTRIBUTE:
			TODO
		case DECL_POISONED:
		case DECL_IMPORT:
		case DECL_ENUM_CONSTANT:
		case DECL_CT_ELSE:
		case DECL_CT_ELIF:
		case DECL_LABEL:
		case DECL_CT_SWITCH:
		case DECL_CT_CASE:
		case DECL_CT_IF:
		case DECL_CT_ASSERT:
		case DECL_ERRVALUE:
			UNREACHABLE
	}
	decl->resolve_status = RESOLVE_DONE;
	return true;
}
