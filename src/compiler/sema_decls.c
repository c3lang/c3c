// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "sema_internal.h"


static inline bool sema_analyse_func_macro(SemaContext *context, Decl *decl, bool is_func);
static inline bool sema_analyse_func(SemaContext *context, Decl *decl);
static inline bool sema_analyse_macro(SemaContext *context, Decl *decl);
static inline bool sema_analyse_signature(SemaContext *context, Signature *sig);
static inline Type *sema_analyse_function_signature(SemaContext *context, Decl *parent, CallABI abi, Signature *signature, bool is_real_function);
static inline bool sema_analyse_main_function(SemaContext *context, Decl *decl);
static inline bool sema_check_param_uniqueness_and_type(Decl **decls, Decl *current, unsigned current_index, unsigned count);

static inline bool sema_analyse_method(SemaContext *context, Decl *decl);
static inline bool sema_is_valid_method_param(SemaContext *context, Decl *param, Type *parent_type);
static inline bool sema_analyse_macro_method(SemaContext *context, Decl *decl);
static inline bool unit_add_base_extension_method(CompilationUnit *unit, Type *parent_type, Decl *method_like);
static inline bool unit_add_method_like(CompilationUnit *unit, Type *parent_type, Decl *method_like);

static bool sema_analyse_operator_common(Decl *method, TypeInfo **rtype_ptr, Decl ***params_ptr, uint32_t parameters);
static inline Decl *operator_in_module(SemaContext *c, Module *module, OperatorOverload operator_overload);
static inline bool sema_analyse_operator_element_at(Decl *method);
static inline bool sema_analyse_operator_element_set(Decl *method);
static inline bool sema_analyse_operator_len(Decl *method);
static bool sema_check_operator_method_validity(Decl *method);
static inline const char *method_name_by_decl(Decl *method_like);

static bool sema_analyse_struct_union(SemaContext *context, Decl *decl);
static bool sema_analyse_bitstruct(SemaContext *context, Decl *decl);
static bool sema_analyse_union_members(SemaContext *context, Decl *decl, Decl **members);
static bool sema_analyse_struct_members(SemaContext *context, Decl *decl, Decl **members);
static inline bool sema_analyse_struct_member(SemaContext *context, Decl *parent, Decl *decl);
static inline bool sema_analyse_bitstruct_member(SemaContext *context, Decl *decl, unsigned index, bool allow_overlap);

static inline bool sema_analyse_doc_header(AstId doc, Decl **params, Decl **extra_params, bool *pure_ref);

static const char *attribute_domain_to_string(AttributeDomain domain);
static bool sema_analyse_attribute(SemaContext *context, Decl *decl, Attr *attr, AttributeDomain domain);
static bool sema_analyse_attributes_inner(SemaContext *context, Decl *decl, Attr** attrs, AttributeDomain domain, Decl *top, int counter);
static bool sema_analyse_attributes(SemaContext *context, Decl *decl, Attr** attrs, AttributeDomain domain);
static bool sema_analyse_attributes_for_var(SemaContext *context, Decl *decl);
static bool sema_check_section(SemaContext *context, Attr *attr);
static inline bool sema_analyse_attribute_decl(SemaContext *c, Decl *decl);

static inline bool sema_analyse_typedef(SemaContext *context, Decl *decl);
bool sema_analyse_decl_type(SemaContext *context, Type *type, SourceSpan span);
static inline bool sema_analyse_define(SemaContext *c, Decl *decl);
static inline bool sema_analyse_distinct(SemaContext *context, Decl *decl);

static CompilationUnit *unit_copy(Module *module, CompilationUnit *unit);
static bool sema_analyse_parameterized_define(SemaContext *c, Decl *decl);
static Module *module_instantiate_generic(Module *module, Path *path, TypeInfo **parms);

static inline bool sema_analyse_enum_param(SemaContext *context, Decl *param, bool *has_default);
static inline bool sema_analyse_enum(SemaContext *context, Decl *decl);
static inline bool sema_analyse_error(SemaContext *context, Decl *decl);


static bool sema_check_section(SemaContext *context, Attr *attr)
{
	const char *section_string = attr->exprs[0]->const_expr.string.chars;
	// No restrictions except for MACH-O
	if (platform_target.object_format != OBJ_FORMAT_MACHO)
	{
		return true;
	}
	scratch_buffer_clear();
	StringSlice slice = slice_from_string(section_string);
	StringSlice segment = slice_next_token(&slice, ',');
	StringSlice section = slice_next_token(&slice, ',');
	StringSlice attrs = slice_next_token(&slice, ',');
	StringSlice stub_size_str = slice_next_token(&slice, ',');
	(void)attrs;
	(void)stub_size_str;

	if (slice.len)
	{
		SEMA_ERROR(attr->exprs[0], "Too many parts to the Mach-o section description.");
	}
	slice_trim(&segment);
	if (segment.len == 0)
	{
		SEMA_ERROR(attr->exprs[0], "The segment is missing, did you type it correctly?");
		return false;
	}
	slice_trim(&section);
	if (section.len == 0)
	{
		SEMA_ERROR(attr->exprs[0], "Mach-o requires 'segment,section' as the format, did you type it correctly?");
		return false;
	}
	if (section.len > 16)
	{
		SEMA_ERROR(attr->exprs[0], "Mach-o requires the section to be at the most 16 characters, can you shorten it?");
		return false;
	}
	REMINDER("Improve section type checking for Mach-o like Clang has.");
	return true;
}

static inline bool sema_check_param_uniqueness_and_type(Decl **decls, Decl *current, unsigned current_index, unsigned count)
{
	if (current->type && current->type == type_void)
	{
		if (count == 1 && !current->name && current->var.kind == VARDECL_PARAM)
		{
			SEMA_ERROR(current, "C-style 'foo(void)' style argument declarations are not valid, please remove 'void'.");
		}
		else
		{
			SEMA_ERROR(current, "Parameters may not be of type 'void'.");
		}
		return false;
	}
	const char *name = current->name;
	if (!name) return true;
	for (int i = 0; i < current_index; i++)
	{
		if (name == decls[i]->name)
		{
			SEMA_ERROR(current, "Duplicate parameter name '%s'.", name);
			SEMA_NOTE(decls[i], "Previous use of the name was here.");
			decl_poison(decls[i]);
			decl_poison(current);
			return false;
		}
	}
	return true;
}

static inline bool sema_analyse_struct_member(SemaContext *context, Decl *parent, Decl *decl)
{
	assert(!decl->unit || decl->unit->module->is_generic);
	decl->unit = parent->unit;
	if (decl->name)
	{
		Decl *other = sema_decl_stack_resolve_symbol(decl->name);
		if (other)
		{
			SEMA_ERROR(decl, "Duplicate member name '%s'.", other->name);
			SEMA_NOTE(other, "Previous declaration was here.");
			return false;
		}
		if (decl->name) sema_decl_stack_push(decl);
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

static bool sema_analyse_union_members(SemaContext *context, Decl *decl, Decl **members)
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
		if (!sema_analyse_struct_member(context, decl, member))
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
		if (member->type->type_kind == TYPE_SCALED_VECTOR)
		{
			SEMA_ERROR(member, "Scaled vector members not allowed in unions / structs.");
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
	if (!decl->is_packed && decl->alignment < max_alignment) decl->alignment = max_alignment;

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
		REMINDER("Check if this should really be allowed.");
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

static bool sema_analyse_struct_members(SemaContext *context, Decl *decl, Decl **members)
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
		if (!sema_analyse_struct_member(context, decl, member))
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
		if (member_type->type_kind == TYPE_SCALED_VECTOR)
		{
			SEMA_ERROR(member, "Scaled vectors may not be used in structs and unions.");
			return false;
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

		if (!sema_analyse_attributes(context, member, attributes, ATTR_MEMBER)) return decl_poison(decl);

		if (member->alignment)
		{
			member_alignment = member->alignment;
			// Update total alignment if we have a member that has bigger alignment.
			if (member_alignment > decl->alignment) decl->alignment = member_alignment;
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
	if (!decl->is_packed && decl->alignment < natural_alignment) decl->alignment = natural_alignment;

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

static bool sema_analyse_struct_union(SemaContext *context, Decl *decl)
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
		case DECL_FAULT:
			domain = ATTR_ERROR;
			break;
		default:
			UNREACHABLE
	}

	if (!sema_analyse_attributes(context, decl, decl->attributes, domain)) return decl_poison(decl);

	DEBUG_LOG("Beginning analysis of %s.", decl->name ? decl->name : ".anon");
	bool success;
	Decl **members = decl->strukt.members;
	if (!vec_size(members))
	{
		SEMA_ERROR(decl, decl->decl_kind == DECL_UNION ? "Zero sized unions are not permitted." : "Zero sized structs are not permitted.");
		return false;
	}
	if (decl->name)
	{
		Decl** state = sema_decl_stack_store();
			if (decl->decl_kind == DECL_UNION)
			{
				success = sema_analyse_union_members(context, decl, decl->strukt.members);
			}
			else
			{
				success = sema_analyse_struct_members(context, decl, decl->strukt.members);
			}
		sema_decl_stack_restore(state);
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

static inline bool sema_analyse_bitstruct_member(SemaContext *context, Decl *decl, unsigned index, bool allow_overlap)
{
	Decl **members = decl->strukt.members;
	Decl *member = members[index];

	// Resolve the type.
	if (!sema_resolve_type_info(context, member->var.type_info)) return false;
	member->type = member->var.type_info->type;

	// Flatten the distinct and enum types.
	Type *member_type = type_flatten_for_bitstruct(member->type);

	// Only accept (flattened) integer and bool types
	if (!type_is_integer(member_type) && member_type != type_bool)
	{
		SEMA_ERROR(member->var.type_info, "%s is not supported in a bitstruct, only enums, integer and boolean values may be used.",
		           type_quoted_error_string(member->type));
		return false;
	}

	// Grab the underlying bit type size.
	BitSize bits = type_size(decl->bitstruct.base_type->type) * (BitSize)8;

	if (bits > MAX_BITSTRUCT)
	{
		SEMA_ERROR(decl->bitstruct.base_type, "Bitstruct size may not exceed %d bits.", MAX_BITSTRUCT);
		return false;
	}
	Int max_bits = (Int) { .type = TYPE_I64, .i = { .low =  bits } };

	// Resolve the bit range, starting with the beginning
	Expr *start = member->var.start;
	if (!sema_analyse_expr(context, start)) return false;

	// Check for negative, non integer or non const values.
	if (start->expr_kind != EXPR_CONST || !type_is_integer(start->type) || int_is_neg(start->const_expr.ixx))
	{
		SEMA_ERROR(start, "This must be a constant non-negative integer value.");
		return false;
	}

	// Check that we didn't exceed max bits.
	if (int_comp(start->const_expr.ixx, max_bits, BINARYOP_GE))
	{
		SEMA_ERROR(start, "Expected at the most a bit index of %d\n", bits - 1);
		return false;
	}

	unsigned start_bit, end_bit;
	end_bit = start_bit = (unsigned)start->const_expr.ixx.i.low;

	// Handle the end
	Expr *end = member->var.end;
	if (end)
	{
		// Analyse the end
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
		// No end bit, this is only ok if the type is bool, otherwise a range is needed.
		// This prevents confusion with C style bits.
		if (member_type->type_kind != TYPE_BOOL)
		{
			SEMA_ERROR(member, "Only booleans may use non-range indices, try using %d..%d instead.", start_bit, start_bit);
			return false;
		}
	}

	// Check that we actually have a positive range.
	if (start_bit > end_bit)
	{
		SEMA_ERROR(start, "The start bit must be smaller than the end bit index.");
		return false;
	}

	// Check how many bits we need.
	TypeSize bitsize_type = member_type == type_bool ? 1 : type_size(member_type) * 8;

	// And how many we have.
	TypeSize bits_available = end_bit + 1 - start_bit;

	// Assigning more than needed is not allowed.
	if (bitsize_type < bits_available)
	{
		SEMA_ERROR(member, "The bit width of %s (%d) is less than the assigned bits (%d), try reducing the range.",
		           type_quoted_error_string(member->type), (int)bitsize_type, (int)bits_available);
		return false;
	}

	// Store the bits.
	member->var.start_bit = start_bit;
	member->var.end_bit = end_bit;

	// Check for duplicate members.
	for (unsigned i = 0; i < index; i++)
	{
		Decl *other_member = members[i];
		if (member->name == other_member->name)
		{
			SEMA_ERROR(member, "Duplicate members with the name '%s'.", member->name);
			SEMA_NOTE(other_member, "The other member was declared here.");
			return false;
		}
		// And possibly overlap.
		if (allow_overlap) continue;

		// Check for overlap.
		if ((start_bit >= other_member->var.start_bit || end_bit >= other_member->var.start_bit)
			&& start_bit <= other_member->var.end_bit)
		{
			SEMA_ERROR(member, "Overlapping members, please use '@overlap' if this is intended.");
			SEMA_NOTE(other_member, "The other member was declared here.");
			return false;
		}
	}
	member->resolve_status = RESOLVE_DONE;
	return true;
}

static bool sema_analyse_bitstruct(SemaContext *context, Decl *decl)
{
	if (!sema_analyse_attributes(context, decl, decl->attributes, ATTR_BITSTRUCT)) return decl_poison(decl);

	DEBUG_LOG("Beginning analysis of %s.", decl->name ? decl->name : ".anon");
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
	VECEACH(members, i)
	{
		if (!sema_analyse_bitstruct_member(context, decl, i, decl->bitstruct.overlap))
		{
			return decl_poison(decl);
		}
	}
	return true;
}


static inline bool sema_analyse_signature(SemaContext *context, Signature *sig)
{
	Variadic variadic_type = sig->variadic;
	Decl **params = sig->params;
	unsigned param_count = vec_size(params);
	unsigned vararg_index = sig->vararg_index;
	bool is_macro = sig->is_macro;
	bool is_macro_at_name = sig->is_at_macro;

	// Check return type
	assert(sig->rtype || sig->is_macro);
	Type *rtype = NULL;
	if (sig->rtype)
	{
		TypeInfo *rtype_info = type_infoptr(sig->rtype);
		if (!sema_resolve_type_info_maybe_inferred(context, type_infoptr(sig->rtype), is_macro)) return false;
		rtype = rtype_info->type;
		if (sig->attrs.nodiscard)
		{
			if (rtype == type_void)
			{
				SEMA_ERROR(rtype_info, "@nodiscard cannot be used on %s returning 'void'.", is_macro ? "macros" : "functions");
				return false;
			}
		}
		if (sig->attrs.maydiscard)
		{
			if (!type_is_optional(rtype))
			{
				SEMA_ERROR(rtype_info, "@maydiscard can only be used on %s returning optional values.", is_macro ? "macros" : "functions");
				return false;
			}
		}
	}

	// We don't support more than MAX_PARAMS number of params. This makes everything sane.
	if (param_count > MAX_PARAMS)
	{
		if (variadic_type != VARIADIC_NONE)
		{
			SEMA_ERROR(params[MAX_PARAMS], "The number of params exceeded the max of %d.", MAX_PARAMS);
			return false;
		}
		SEMA_ERROR(params[MAX_PARAMS], "The number of params exceeded the max of %d. To accept more arguments, consider using varargs.", MAX_PARAMS);
		return false;
	}

	// Check parameters
	for (unsigned i = 0; i < param_count; i++)
	{
		Decl *param = params[i];
		if (!param)
		{
			assert(variadic_type == VARIADIC_RAW);
			assert(i == vararg_index);
			continue;
		}
		if (vararg_index < i)
		{
			if (!is_macro && variadic_type == VARIADIC_RAW)
			{
				SEMA_ERROR(param, "C-style varargs cannot be followed by regular parameters.");
				return decl_poison(param);
			}

			// If we already reached a vararg (as a previous argument) and we have a
			// parameter without a name then that is an error.
			if (!param->name)
			{
				SEMA_ERROR(param, "A parameter name was expected, as parameters after varargs must always be named.");
				return decl_poison(param);
			}
		}

		assert(param->resolve_status == RESOLVE_NOT_DONE && "The param shouldn't have been resolved yet.");
		param->resolve_status = RESOLVE_RUNNING;
		param->unit = context->unit;
		assert(param->decl_kind == DECL_VAR);
		VarDeclKind var_kind = param->var.kind;
		switch (var_kind)
		{
			case VARDECL_PARAM_EXPR:
			case VARDECL_PARAM_REF:
				if (!is_macro)
				{
					SEMA_ERROR(param, "Only regular parameters are allowed for functions.");
					return decl_poison(param);
				}
				if (!is_macro_at_name)
				{
					SEMA_ERROR(param, "Ref and expression parameters are not allowed in function-like macros. Prefix the macro name with '@'.");
					return decl_poison(param);
				}
				FALLTHROUGH;
			case VARDECL_PARAM_CT:
				if (!is_macro)
				{
					SEMA_ERROR(param, "Only regular parameters are allowed for functions.");
					return decl_poison(param);
				}
				FALLTHROUGH;
			case VARDECL_PARAM:
				if (param->var.type_info)
				{
					if (!sema_resolve_type_info_maybe_inferred(context, param->var.type_info, is_macro)) return decl_poison(param);
					param->type = param->var.type_info->type;
				}
				else if (!is_macro)
				{
					SEMA_ERROR(param, "Only typed parameters are allowed for functions.");
					return false;
				}
				if (!sema_analyse_attributes_for_var(context, param)) return false;
				break;
			case VARDECL_PARAM_CT_TYPE:
				if (!is_macro)
				{
					SEMA_ERROR(param, "Only regular parameters are allowed for functions.");
					return decl_poison(param);
				}
				if (param->var.type_info)
				{
					SEMA_ERROR(param->var.type_info, "A compile time type parameter cannot have a type itself.");
					return decl_poison(param);
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
		if (param->var.vararg)
		{
			if (var_kind != VARDECL_PARAM)
			{
				SEMA_ERROR(param, "Only regular parameters may be vararg.");
				return decl_poison(param);
			}
			if (!param->var.type_info)
			{
				SEMA_ERROR(param, "Only typed parameters may be vararg.");
				return decl_poison(param);
			}
			// Update whether this was a vararg, and update "default" for the signature.
			if (param->var.vararg)
			{
				if (vararg_index != i)
				{
					SEMA_ERROR(param, "A %s may not have more than one vararg.", is_macro ? "macro" : "function");
					return decl_poison(param);
				}
			}
			param->var.type_info->type = type_get_subarray(param->var.type_info->type);
		}

		if (param->var.type_info)
		{
			param->type = param->var.type_info->type;
			param->alignment = type_abi_alignment(param->type);
		}
		if (param->var.init_expr)
		{
			Expr *expr = param->var.init_expr;
			if (expr->expr_kind == EXPR_CONST)
			{
				if (!sema_analyse_expr_rhs(context, param->type, expr, true)) return decl_poison(param);
			}
		}
		if (!sema_check_param_uniqueness_and_type(params, param, i, param_count)) return decl_poison(param);
		param->resolve_status = RESOLVE_DONE;
	}
	return true;
}


static inline Type *sema_analyse_function_signature(SemaContext *context, Decl *parent, CallABI abi, Signature *signature, bool is_real_function)
{
	// Get param count and variadic type
	Decl **params = signature->params;

	if (!sema_analyse_signature(context, signature)) return NULL;

	Variadic variadic_type = signature->variadic;
	// Remove the last empty value.
	if (variadic_type == VARIADIC_RAW)
	{
		assert(!params[signature->vararg_index] && "The last parameter must have been a raw variadic.");
		assert(signature->vararg_index == vec_size(params) - 1);
		vec_pop(params);
	}

	Type **types = NULL;
	bool all_ok = true;
	unsigned param_count = vec_size(params);

	for (unsigned i = 0; i < param_count; i++)
	{
		vec_add(types, params[i]->type);
	}

	if (!all_ok) return NULL;
	Type *raw_type = type_get_func(signature, abi);
	Type *type = type_new(TYPE_FUNC, parent->name);
	type->canonical = type;
	type->function.signature = signature;
	type->function.module = parent->unit->module;
	type->function.prototype = raw_type->function.prototype;
	return type;
}

static inline bool sema_analyse_typedef(SemaContext *context, Decl *decl)
{
	if (decl->typedef_decl.is_func)
	{
		Type *func_type = sema_analyse_function_signature(context, decl, CALL_C, &decl->typedef_decl.function_signature, false);
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

static inline bool sema_analyse_distinct(SemaContext *context, Decl *decl)
{
	if (decl->distinct_decl.typedef_decl.is_func)
	{
		Type *func_type = sema_analyse_function_signature(context, decl, CALL_C, &decl->distinct_decl.typedef_decl.function_signature, false);
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
		case TYPE_OPTIONAL:
			SEMA_ERROR(decl, "You cannot create a distinct type from a failable.");
			return false;
		case TYPE_FAULTTYPE:
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
		case TYPE_SCALED_VECTOR:
			break;
	}
	// Do we need anything else?
	return true;
}

static inline bool sema_analyse_enum_param(SemaContext *context, Decl *param, bool *has_default)
{
	*has_default = false;
	assert(param->decl_kind == DECL_VAR);
	// We need to check that the parameters are not typeless nor are of any macro parameter kind.
	if (param->var.kind != VARDECL_PARAM && !param->var.type_info)
	{
		SEMA_ERROR(param, "An associated value must be a normal typed parameter.");
		return false;
	}
	if (vec_size(param->attributes))
	{
		SEMA_ERROR(param->attributes[0], "There are no valid attributes for associated values.");
		return false;
	}
	if (!sema_resolve_type_info(context, param->var.type_info)) return false;
	if (param->var.vararg)
	{
		param->var.type_info->type = type_get_subarray(param->var.type_info->type);
	}
	param->type = param->var.type_info->type;
	assert(param->name);
	if (param->name == kw_nameof)
	{
		SEMA_ERROR(param, "'nameof' is not a valid parameter name for enums.");
		return false;
	}
	Decl *other = sema_decl_stack_resolve_symbol(param->name);
	if (other)
	{
		SEMA_ERROR(param, "Duplicate parameter name '%s'.", param->name);
		return false;
	}
	sema_decl_stack_push(param);
	if (param->var.init_expr)
	{
		Expr *expr = param->var.init_expr;

		if (!sema_analyse_expr_rhs(context, param->type, expr, true)) return false;
		if (IS_OPTIONAL(expr))
		{
			SEMA_ERROR(expr, "Default arguments may not be failable.");
			return false;
		}
		if (!expr_is_constant_eval(expr, CONSTANT_EVAL_GLOBAL_INIT))
		{
			SEMA_ERROR(expr, "Only constant expressions may be used as default values.");
			return false;
		}
		*has_default = true;
	}
	param->alignment = type_abi_alignment(param->type);
	return true;
}

static inline bool sema_analyse_enum(SemaContext *context, Decl *decl)
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

	if (!sema_analyse_attributes(context, decl, decl->attributes, ATTR_ENUM)) return decl_poison(decl);

	Decl **associated_values = decl->enums.parameters;
	unsigned associated_value_count = vec_size(associated_values);
	unsigned mandatory_count = 0;
	bool default_values_used = false;
	Decl** state = sema_decl_stack_store();
	for (unsigned i = 0; i < associated_value_count; i++)
	{
		Decl *value = associated_values[i];
		switch (value->resolve_status)
		{
			case RESOLVE_DONE:
				continue;
			case RESOLVE_RUNNING:
				SEMA_ERROR(value, "Recursive definition found.");
				goto ERR;
			case RESOLVE_NOT_DONE:
				value->resolve_status = RESOLVE_RUNNING;
				break;
		}
		bool has_default = false;
		if (!sema_analyse_enum_param(context, value, &has_default)) goto ERR;
		if (!has_default)
		{
			mandatory_count++;
			if (default_values_used && !value->var.vararg)
			{
				SEMA_ERROR(value, "Non-default parameters cannot appear after default parameters.");
				goto ERR;
			}
		}
		default_values_used |= has_default;
		value->resolve_status = RESOLVE_DONE;
	}
	sema_decl_stack_restore(state);

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

		// Create a "fake" expression.
		// This will be evaluated later to catch the case
		Int val = (Int){ value, canonical->type_kind };
		if (!int_fits(val, canonical->type_kind))
		{
			SEMA_ERROR(enum_value,
			           "The enum value would implicitly be %s which does not fit in %s.",
			           i128_to_string(value, 10, type_is_signed(canonical)),
			           type_quoted_error_string(type));
			return false;
		}
		enum_value->enum_constant.ordinal = value.low;

		// Update the value
		value.low++;

		Expr **args = enum_value->enum_constant.args;
		unsigned arg_count = vec_size(args);
		if (arg_count > associated_value_count)
		{
			if (!associated_value_count)
			{
				SEMA_ERROR(args[0], "No associated values are defined for this enum.");
				return false;
			}
			SEMA_ERROR(args[associated_value_count], "Only %d associated value(s) may be defined for this enum.");
			return false;
		}
		if (arg_count < mandatory_count)
		{
			SEMA_ERROR(enum_value, "Expected associated value(s) defined for this enum.");
			return false;
		}
		for (unsigned j = 0; j < arg_count; j++)
		{
			Expr *arg = args[j];

			if (!sema_analyse_expr_rhs(context, associated_values[j]->type, arg, false)) return false;
			if (!expr_is_constant_eval(arg, CONSTANT_EVAL_GLOBAL_INIT))
			{
				SEMA_ERROR(arg, "Expected a constant expression as parameter.");
				return false;
			}
		}
		REMINDER("named parameters and defaults do not work");
		enum_value->resolve_status = RESOLVE_DONE;
	}
	return success;
ERR:
	sema_decl_stack_restore(state);
	return false;
}

static inline bool sema_analyse_error(SemaContext *context, Decl *decl)
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
		assert(enum_value->decl_kind == DECL_FAULTVALUE);

		// Start evaluating the constant
		enum_value->resolve_status = RESOLVE_DONE;
	}
	return success;
}

static inline const char *method_name_by_decl(Decl *method_like)
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



static bool sema_analyse_operator_common(Decl *method, TypeInfo **rtype_ptr, Decl ***params_ptr, uint32_t parameters)
{
	Signature *signature = &method->func_decl.signature;
	Decl **params = *params_ptr = signature->params;
	uint32_t param_count = vec_size(params);
	if (param_count > parameters)
	{
		SEMA_ERROR(params[parameters], "Too many parameters, '%s' expects only %u.", method->name, (unsigned)parameters);
		return false;
	}
	if (param_count < parameters)
	{
		SEMA_ERROR(method, "Not enough parameters, '%s' requires %u.", method->name, (unsigned)parameters);
		return false;
	}

	if (!signature->rtype)
	{
		SEMA_ERROR(method, "The return value must be explicitly typed for '%s'.", method->name);
		return false;
	}
	VECEACH(params, i)
	{
		Decl *param = params[i];
		if (!params[i]->var.type_info)
		{
			SEMA_ERROR(param, "All parameters must be explicitly typed for '%s'.", method->name);
			return false;
		}
	}
	*rtype_ptr = type_infoptr(signature->rtype);
	return true;
}

static inline Decl *operator_in_module(SemaContext *c, Module *module, OperatorOverload operator_overload)
{
	if (module->is_generic) return NULL;
	Decl **extensions = module->method_extensions;
	VECEACH(extensions, j)
	{
		Decl *extension = extensions[j];
		if (extension->operator == operator_overload)
		{
			unit_register_external_symbol(c->compilation_unit, extension);
			return extension;
		}
	}
	VECEACH(module->sub_modules, i)
	{
		return operator_in_module(c, module->sub_modules[i], operator_overload);
	}
	return NULL;
}

Decl *sema_find_operator(SemaContext *context, Expr *expr, OperatorOverload operator_overload)
{
	Decl *ambiguous = NULL;
	Decl *private = NULL;
	Type *type = expr->type->canonical;
	if (!type_may_have_sub_elements(type)) return NULL;
	Decl *def = type->decl;
	Decl **funcs = def->methods;
	VECEACH(funcs, i)
	{
		Decl *func = funcs[i];
		if (func->operator == operator_overload)
		{
			unit_register_external_symbol(context->compilation_unit, func);
			return func;
		}
	}
	Decl *extension = operator_in_module(context, context->compilation_unit->module, operator_overload);
	if (extension) return extension;

	Decl **imports = context->unit->imports;
	VECEACH(imports, i)
	{
		extension = operator_in_module(context, imports[i]->import.module, operator_overload);
		if (extension) return extension;
	}
	return NULL;
}


static inline bool sema_analyse_operator_element_at(Decl *method)
{
	TypeInfo *rtype;
	Decl **params;
	if (!sema_analyse_operator_common(method, &rtype, &params, 2)) return false;
	if (rtype->type->canonical == type_void)
	{
		SEMA_ERROR(rtype, "The return type cannot be 'void'.");
		return false;
	}
	return true;
}

static inline bool sema_analyse_operator_element_set(Decl *method)
{
	TypeInfo *rtype;
	Decl **params;
	return sema_analyse_operator_common(method, &rtype, &params, 3);
}

static inline bool sema_analyse_operator_len(Decl *method)
{
	TypeInfo *rtype;
	Decl **params;
	if (!sema_analyse_operator_common(method, &rtype, &params, 1)) return false;
	if (!type_is_integer(rtype->type))
	{
		SEMA_ERROR(rtype, "The return type must be an integer type.");
		return false;
	}
	return true;
}

static bool sema_check_operator_method_validity(Decl *method)
{
	switch (method->operator)
	{
		case OVERLOAD_ELEMENT_SET:
			return sema_analyse_operator_element_set(method);
		case OVERLOAD_ELEMENT_AT:
		case OVERLOAD_ELEMENT_REF:
			return sema_analyse_operator_element_at(method);
		case OVERLOAD_LEN:
			return sema_analyse_operator_len(method);
	}
	UNREACHABLE
}

static inline bool unit_add_base_extension_method(CompilationUnit *unit, Type *parent_type, Decl *method_like)
{
	if (!method_like->has_extname)
	{
		scratch_buffer_clear();
		if (method_like->visibility <= VISIBLE_MODULE)
		{
			scratch_buffer_append(parent_type->name);
			scratch_buffer_append_char('$');
			scratch_buffer_append(method_like->name);
		}
		else
		{
			scratch_buffer_append(parent_type->name);
			scratch_buffer_append("_");
			scratch_buffer_append(method_like->name);
		}
		method_like->extname = scratch_buffer_copy();
	}
	DEBUG_LOG("Method-like '%s.%s' analysed.", parent_type->name, method_like->name);
	vec_add(unit->module->method_extensions, method_like);
	return true;
}

static inline bool unit_add_method_like(CompilationUnit *unit, Type *parent_type, Decl *method_like)
{
	assert(parent_type->canonical == parent_type);
	const char *name = method_like->name;
	Decl *method = sema_find_extension_method_in_module(unit->module, parent_type, name);
	if (method)
	{
		SEMA_ERROR(method_like, "This %s is already defined in this module.", method_name_by_decl(method_like));
		SEMA_NOTE(method, "The previous definition was here.");
		return false;
	}
	if (!type_is_user_defined(parent_type)) return unit_add_base_extension_method(unit, parent_type, method_like);
	Decl *parent = parent_type->decl;
	Decl *ambiguous = NULL;
	Decl *private = NULL;
	method = sema_resolve_method(unit, parent, name, &ambiguous, &private);
	if (method)
	{
		SEMA_ERROR(method_like, "This %s is already defined for '%s'.",
		           method_name_by_decl(method_like), parent_type->name);
		SEMA_NOTE(method, "The previous definition was here.");
		return false;
	}
	if (method_like->operator && !sema_check_operator_method_validity(method_like)) return false;
	REMINDER("Check multiple operator");
	if (!method_like->has_extname)
	{
		scratch_buffer_clear();
		if (method_like->visibility <= VISIBLE_MODULE)
		{
			scratch_buffer_append(parent->extname);
			scratch_buffer_append_char('$');
			scratch_buffer_append(method_like->name);
		}
		else
		{
			scratch_buffer_append(parent->extname);
			scratch_buffer_append("_");
			scratch_buffer_append(method_like->name);
		}
		method_like->extname = scratch_buffer_copy();
	}
	DEBUG_LOG("Method-like '%s.%s' analysed.", parent->name, method_like->name);
	if (parent->unit->module == unit->module)
	{
		vec_add(parent->methods, method_like);
	}
	else
	{
		vec_add(unit->module->method_extensions, method_like);
	}
	return true;

}

static inline bool sema_analyse_method(SemaContext *context, Decl *decl)
{
	TypeInfo *parent_type = type_infoptr(decl->func_decl.type_parent);
	if (!sema_resolve_type_info(context, parent_type)) return false;
	Type *type = parent_type->type->canonical;
	if (!type_may_have_sub_elements(type) && !type_underlying_is_numeric(type) && !type_is_arraylike(type))
	{
		SEMA_ERROR(decl,
		           "Methods can not be associated with '%s'",
		           type_to_error_string(parent_type->type));
		return false;
	}
	Decl **params = decl->func_decl.signature.params;
	if (!vec_size(params))
	{
		SEMA_ERROR(decl, "A method must start with a parameter of type %s or %s.",
		           type_quoted_error_string(parent_type->type), type_quoted_error_string(type_get_ptr(parent_type->type)));
		return false;
	}
	if (!sema_is_valid_method_param(context, params[0], type)) return false;
	return unit_add_method_like(context->unit, type, decl);
}


static const char *attribute_domain_to_string(AttributeDomain domain)
{
	switch (domain)
	{
		case ATTR_MACRO:
			return "macro";
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
		case ATTR_INITIALIZER:
			return "static initializer";
		case ATTR_FINALIZER:
			return "static finalizer";
		case ATTR_XXLIZER:
			UNREACHABLE
	}
	UNREACHABLE
}

static bool sema_analyse_attribute(SemaContext *context, Decl *decl, Attr *attr, AttributeDomain domain)
{
	AttributeType type = attr->attr_kind;
	assert(type >= 0 && type < NUMBER_OF_ATTRIBUTES);
	static AttributeDomain attribute_domain[NUMBER_OF_ATTRIBUTES] = {
			[ATTRIBUTE_WEAK] = ATTR_FUNC | ATTR_CONST | ATTR_GLOBAL,
			[ATTRIBUTE_EXTNAME] = (AttributeDomain)~(ATTR_CALL | ATTR_BITSTRUCT | ATTR_MACRO | ATTR_XXLIZER),
			[ATTRIBUTE_SECTION] = ATTR_FUNC | ATTR_CONST | ATTR_GLOBAL,
			[ATTRIBUTE_PACKED] = ATTR_STRUCT | ATTR_UNION,
			[ATTRIBUTE_NORETURN] = ATTR_FUNC | ATTR_MACRO,
			[ATTRIBUTE_ALIGN] = ATTR_FUNC | ATTR_CONST | ATTR_LOCAL | ATTR_GLOBAL | ATTR_STRUCT | ATTR_UNION |
			                    ATTR_MEMBER,
			[ATTRIBUTE_INLINE] = ATTR_FUNC | ATTR_CALL,
			[ATTRIBUTE_NOINLINE] = ATTR_FUNC | ATTR_CALL,
			[ATTRIBUTE_NODISCARD] = ATTR_FUNC | ATTR_MACRO,
			[ATTRIBUTE_MAYDISCARD] = ATTR_FUNC | ATTR_MACRO,
			[ATTRIBUTE_BIGENDIAN] = ATTR_BITSTRUCT,
			[ATTRIBUTE_LITTLEENDIAN] = ATTR_BITSTRUCT,
			[ATTRIBUTE_USED] = (AttributeDomain)~(ATTR_CALL | ATTR_XXLIZER ),
			[ATTRIBUTE_UNUSED] = (AttributeDomain)~(ATTR_CALL | ATTR_XXLIZER),
			[ATTRIBUTE_NAKED] = ATTR_FUNC,
			[ATTRIBUTE_CDECL] = ATTR_FUNC,
			[ATTRIBUTE_STDCALL] = ATTR_FUNC,
			[ATTRIBUTE_VECCALL] = ATTR_FUNC,
			[ATTRIBUTE_REGCALL] = ATTR_FUNC,
			[ATTRIBUTE_FASTCALL] = ATTR_FUNC,
			[ATTRIBUTE_OVERLAP] = ATTR_BITSTRUCT,
			[ATTRIBUTE_BUILTIN] = ATTR_MACRO | ATTR_FUNC,
			[ATTRIBUTE_OPERATOR] = ATTR_MACRO | ATTR_FUNC,
			[ATTRIBUTE_REFLECT] = ATTR_ENUM,
			[ATTRIBUTE_OBFUSCATE] = ATTR_ENUM,
			[ATTRIBUTE_PURE] = ATTR_CALL,
			[ATTRIBUTE_PRIORITY] = ATTR_XXLIZER,
	};

	if ((attribute_domain[type] & domain) != domain)
	{
		sema_error_at(attr->span, "'%s' is not a valid %s attribute.", attr->name, attribute_domain_to_string(domain));
		return false;
	}
	unsigned args = vec_size(attr->exprs);
	if (args > 1)
	{
		SEMA_ERROR(attr->exprs[1], "Too many arguments for the attribute.");
		return false;
	}
	Expr *expr = args ? attr->exprs[0] : NULL;
	switch (type)
	{
		case ATTRIBUTE_CDECL:
			decl->func_decl.signature.abi = CALL_C;
			break;
		case ATTRIBUTE_VECCALL:
			switch (platform_target.arch)
			{
				case ARCH_TYPE_X86_64:
				case ARCH_TYPE_X86:
					decl->func_decl.signature.abi = CALL_X86_VECTOR;
					break;
				case ARCH_TYPE_ARM:
				case ARCH_TYPE_ARMB:
				case ARCH_TYPE_AARCH64:
				case ARCH_TYPE_AARCH64_32:
				case ARCH_TYPE_AARCH64_BE:
					decl->func_decl.signature.abi = CALL_AAPCS_VFP;
					break;
				default:
					break;
			}
			break;
		case ATTRIBUTE_STDCALL:
			assert(decl->decl_kind == DECL_FUNC);
			if (platform_target.arch == ARCH_TYPE_X86 || platform_target.arch == ARCH_TYPE_X86_64)
			{
				decl->func_decl.signature.abi = CALL_X86_STD;
			}
			else if (platform_target.arch == ARCH_TYPE_ARM || platform_target.arch == ARCH_TYPE_ARMB)
			{
				decl->func_decl.signature.abi = CALL_AAPCS;
			}
			break; // Check args
		case ATTRIBUTE_FASTCALL:
			if (platform_target.arch == ARCH_TYPE_X86 ||
			    (platform_target.arch == ARCH_TYPE_X86_64 && platform_target.os == OS_TYPE_WIN32))
			{
				decl->func_decl.signature.abi = CALL_X86_FAST;
			}
			break;
		case ATTRIBUTE_REGCALL:
			if (platform_target.arch == ARCH_TYPE_X86 ||
			    (platform_target.arch == ARCH_TYPE_X86_64 && platform_target.os == OS_TYPE_WIN32))
			{
				decl->func_decl.signature.abi = CALL_X86_REG;
			}
			break;
		case ATTRIBUTE_OPERATOR:
		{
			assert(decl->decl_kind == DECL_FUNC || decl->decl_kind == DECL_MACRO);
			if (!expr) goto FAILED_OP_TYPE;
			switch (expr->expr_kind)
			{
				case EXPR_IDENTIFIER:
					if (expr->identifier_expr.path) goto FAILED_OP_TYPE;
					if (expr->identifier_expr.ident != kw_len) goto FAILED_OP_TYPE;
					decl->operator = OVERLOAD_LEN;
					break;
				case EXPR_OPERATOR_CHARS:
					decl->operator = expr->expr_operator_chars;
					break;
				default:
					goto FAILED_OP_TYPE;
			}
			if (!decl->func_decl.type_parent)
			{
				SEMA_ERROR(expr, "@operator(...) can only be used with methods.");
				return false;
			}
			return true;
			FAILED_OP_TYPE:
			SEMA_ERROR(attr, "'operator' requires an operator type argument: '[]', '[]=', '&[]' or 'len'.");
			return false;
		}
		case ATTRIBUTE_ALIGN:
			if (!expr)
			{
				sema_error_at(attr->span, "'align' requires an power-of-2 argument, e.g. align(8).");
				return false;
			}
			if (!sema_analyse_expr(context, expr)) return false;
			if (!expr_is_const_int(expr))
			{
				SEMA_ERROR(expr, "Expected a constant integer value as argument.");
				return false;
			}
			{
				if (int_ucomp(expr->const_expr.ixx, MAX_ALIGNMENT, BINARYOP_GT))
				{
					SEMA_ERROR(expr, "Alignment must be less or equal to %ull.", MAX_ALIGNMENT);
					return false;
				}
				if (int_ucomp(expr->const_expr.ixx, 0, BINARYOP_LE))
				{
					SEMA_ERROR(expr, "Alignment must be greater than zero.");
					return false;
				}
				uint64_t align = int_to_u64(expr->const_expr.ixx);
				if (!is_power_of_two(align))
				{
					SEMA_ERROR(expr, "Alignment must be a power of two.");
					return false;
				}
				decl->alignment = (AlignSize)align;
				return true;
			}
		case ATTRIBUTE_SECTION:
		case ATTRIBUTE_EXTNAME:
			if (context->unit->module->is_generic)
			{
				sema_error_at(attr->span, "'extname' attributes are not allowed in generic modules.");
				return false;
			}
			if (!expr)
			{
				sema_error_at(attr->span, "'%s' requires a string argument, e.g. %s(\"foo\").", attr->name, attr->name);
				return false;
			}
			if (!sema_analyse_expr(context, expr)) return false;
			if (!expr_is_const_string(expr))
			{
				SEMA_ERROR(expr, "Expected a constant string value as argument.");
				return false;
			}
			if (type == ATTRIBUTE_SECTION)
			{
				if (!sema_check_section(context, attr)) return false;
				decl->section = expr->const_expr.string.chars;
			}
			else
			{
				decl->has_extname = true;
				decl->extname = expr->const_expr.string.chars;
			}
			return true;
		case ATTRIBUTE_NOINLINE:
			decl->func_decl.attr_noinline = true;
			decl->func_decl.attr_inline = false;
			break;
		case ATTRIBUTE_NODISCARD:
			decl->func_decl.signature.attrs.nodiscard = true;
			break;
		case ATTRIBUTE_MAYDISCARD:
			decl->func_decl.signature.attrs.maydiscard = true;
			break;
		case ATTRIBUTE_INLINE:
			decl->func_decl.attr_inline = true;
			decl->func_decl.attr_noinline = false;
			break;
		case ATTRIBUTE_NORETURN:
			decl->func_decl.signature.attrs.noreturn = true;
			break;
		case ATTRIBUTE_WEAK:
			decl->is_weak = true;
			break;
		case ATTRIBUTE_NAKED:
			assert(domain == ATTR_FUNC);
			decl->func_decl.attr_naked = true;
			break;
		case ATTRIBUTE_BUILTIN:
			decl->is_autoimport = true;
			break;
		case ATTRIBUTE_OVERLAP:
			decl->bitstruct.overlap = true;
			break;
		case ATTRIBUTE_BIGENDIAN:
			if (decl->bitstruct.little_endian)
			{
				sema_error_at(attr->span, "Attribute cannot be combined with @littleendian");
				return decl_poison(decl);
			}
			decl->bitstruct.big_endian = true;
			break;
		case ATTRIBUTE_LITTLEENDIAN:
			if (decl->bitstruct.big_endian)
			{
				sema_error_at(attr->span, "Attribute cannot be combined with @bigendian");
				return decl_poison(decl);
			}
			decl->bitstruct.little_endian = true;
			break;
		case ATTRIBUTE_PACKED:
			decl->is_packed = true;
			break;
		case ATTRIBUTE_UNUSED:
			decl->is_maybe_unused = true;
			break;
		case ATTRIBUTE_USED:
			decl->is_must_use = true;
			break;
		case ATTRIBUTE_PRIORITY:
			if (!expr || !expr_is_const_int(expr)) goto ERROR_PRIORITY;
			{
				Int i = expr->const_expr.ixx;
				if (!int_fits(i, TYPE_I64)) goto ERROR_PRIORITY;
				int64_t priority = int_to_i64(i);
				if (priority < 1 || priority > MAX_PRIORITY) goto ERROR_PRIORITY;
				decl->xxlizer.priority = priority;
				return true;
			}
		ERROR_PRIORITY:
			SEMA_ERROR(attr, "Expected an argument to '@priority' between 1 and %d.", MAX_PRIORITY);
			return decl_poison(decl);
		case ATTRIBUTE_PURE:
			// Only used for calls.
			UNREACHABLE
		case ATTRIBUTE_REFLECT:
			decl->will_reflect = true;
			break;
		case ATTRIBUTE_OBFUSCATE:
			decl->obfuscate = true;
			break;
		case ATTRIBUTE_NONE:
			UNREACHABLE
	}
	if (expr)
	{
		SEMA_ERROR(expr, "'%s' should not have any arguments.", attr->name);
		return false;
	}
	return true;

}

// TODO consider doing this evaluation early, it should be possible.
static bool sema_analyse_attributes_inner(SemaContext *context, Decl *decl, Attr** attrs, AttributeDomain domain, Decl *top, int counter)
{
	// Detect cycles of the type @Foo = @BarCyclic, @BarCyclic = @BarCyclic
	if (counter > 1000)
	{
		SEMA_ERROR(top, "Recursive declaration of attribute '%s'.", top->name);
		return false;
	}
	int count = vec_size(attrs);
	for (int i = 0; i < count; i++)
	{
		Attr *attr = attrs[i];
		// The simple case,
		if (!attr->is_custom)
		{
			// Analyse it and move on.
			if (!sema_analyse_attribute(context, decl, attr, domain)) return false;
			continue;
		}

		// Custom attributes.
		// First find it.
		Decl *attr_decl = sema_find_symbol(context, attr->name);
		if (!attr_decl || attr_decl->decl_kind != DECL_ATTRIBUTE)
		{
			SEMA_ERROR(attr, "The attribute '%s' could not be found.", attr->name);
			return false;
		}

		// Detect direct cycles @Foo = @Bar @Bar = @Foo
		if (attr_decl == top)
		{
			SEMA_ERROR(top, "Recursive declaration of attribute '%s'.", top->name);
			return false;
		}

		// Handle the case where the current function is the declaration itself.
		if (context->call_env.kind == CALL_ENV_ATTR && context->call_env.attr_declaration == attr_decl)
		{
			SEMA_ERROR(attr_decl, "Recursive declaration of attribute '%s' – it contains itself.", attr_decl->name);
			return false;
		}

		// Grab all the parameters.
		Decl **params = attr_decl->attr_decl.params;
		unsigned param_count = vec_size(params);
		Expr **args = attr->exprs;

		// Check that the parameters match. No varargs, little use for it.
		if (param_count != vec_size(args))
		{
			SEMA_ERROR(attr, "Expected %d parameter(s).", param_count);
			return false;
		}

		// Ok, we have the list of inner attributes.
		Attr **attributes = attr_decl->attr_decl.attrs;

		// Now we need to evaluate these attributes in the attribute definition
		// context.
		SemaContext eval_context;
		sema_context_init(&eval_context, attr_decl->unit);
		eval_context.call_env = (CallEnv) { .kind = CALL_ENV_ATTR, .attr_declaration = decl };
		// We copy the compilation unit.
		eval_context.compilation_unit = context->unit;

		// First we need to analyse each expression in the current scope
		for (int j = 0; j < param_count; j++)
		{
			if (!sema_analyse_ct_expr(context, args[j])) goto ERR;
			params[i]->var.init_expr = args[j];
			params[i]->var.kind = VARDECL_CONST;
			// Then add them to the evaluation context.
			// (Yes this is messy)
			sema_add_local(&eval_context, params[i]);
		}
		// Now we've added everything to the evaluation context, so we can (recursively)
		// apply it to the contained attributes, which in turn may be derived attributes.
		if (!sema_analyse_attributes_inner(&eval_context, decl, attributes, domain, top ? top : attr_decl, counter + 1)) goto ERR;
		// Then destroy the eval context.
		sema_context_destroy(&eval_context);
		continue;
ERR:
		sema_context_destroy(&eval_context);
		return false;
	}
	return true;
}

static bool sema_analyse_attributes(SemaContext *context, Decl *decl, Attr** attrs, AttributeDomain domain)
{
	return sema_analyse_attributes_inner(context, decl, attrs, domain, NULL, 0);
}

static inline bool sema_analyse_doc_header(AstId doc, Decl **params, Decl **extra_params, bool *pure_ref)
{
	while (doc)
	{
		Ast *directive = astptr(doc);
		doc = directive->next;
		DocDirectiveKind directive_kind = directive->doc_stmt.kind;
		if (directive_kind == DOC_DIRECTIVE_PURE)
		{
			if (*pure_ref)
			{
				SEMA_ERROR(directive, "Multiple '@pure' declarations, please remove one.");
				return false;
			}
			*pure_ref = true;
			continue;
		}
		if (directive_kind != DOC_DIRECTIVE_PARAM) continue;
		const char *param_name = directive->doc_stmt.param.name;
		Decl *extra_param = NULL;
		Decl *param = NULL;
		VECEACH(params, j)
		{
			param = params[j];
			if (param->name == param_name) goto NEXT;
		}
		VECEACH(extra_params, j)
		{
			param = extra_params[j];
			if (param->name == param_name) goto NEXT;
		}
		SEMA_ERROR(&directive->doc_stmt.param, "There is no parameter '%s', did you misspell it?", param_name);
		return false;
	NEXT:;
		Type *type = param->type;
		if (type) type = type_flatten(type);
		bool may_be_pointer = !type || type_is_pointer(type);
		if (directive->doc_stmt.param.by_ref)
		{
			if (!may_be_pointer)
			{
				SEMA_ERROR(directive, "'&' can only be added to pointer type parameters.");
				return false;
			}
			param->var.not_null = true;
		}
		switch (directive->doc_stmt.param.modifier)
		{
			case PARAM_ANY:
				goto ADDED;
			case PARAM_IN:
				param->var.in_param = true;
				break;
			case PARAM_OUT:
				param->var.out_param = true;
				break;
			case PARAM_INOUT:
				break;
		}
		if (!may_be_pointer && type->type_kind != TYPE_SUBARRAY)
		{
			SEMA_ERROR(directive, "'in', 'out' and 'inout' may only be added to pointers and subarrays.");
			return false;
		}
ADDED:;
	}
	return true;
}

static inline bool sema_analyse_main_function(SemaContext *context, Decl *decl)
{
	assert(decl != context->unit->main_function);

	if (decl->visibility == VISIBLE_LOCAL)
	{
		SEMA_ERROR(decl, "A main function may not have local visibility.");
		return false;
	}
	Signature *signature = &decl->func_decl.signature;
	TypeInfo *rtype_info = type_infoptr(signature->rtype);
	Type *rtype = type_flatten_distinct(rtype_info->type);
	bool is_int_return = true;
	bool is_err_return = false;
	if (rtype->type_kind == TYPE_FAILABLE_ANY) is_err_return = true;
	if (!is_err_return && type_is_optional(rtype))
	{
		if (rtype->failable->type_kind != TYPE_VOID)
		{
			SEMA_ERROR(rtype_info, "The return type of 'main' cannot be a failable, unless it is 'void!'.");
			return false;
		}
		is_int_return = false;
		is_err_return = true;
	}
	if (type_is_void(rtype)) is_int_return = false;

	if (type_is_integer(rtype) && rtype != type_cint)
	{
		SEMA_ERROR(rtype_info, "Expected a return type of 'void' or %s.", type_quoted_error_string(type_cint));
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
			if (type_flatten_distinct(params[0]->type) != type_cint)
			{
				SEMA_ERROR(params[0], "Expected a parameter of type %s for a C-style main.", type_quoted_error_string(type_cint));
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
	Decl *function;
	if (!subarray_param && is_int_return)
	{
		// Int return is pass-through at the moment.
		decl->visibility = VISIBLE_EXTERN;
		function = decl;
		goto REGISTER_MAIN;
	}
	function = decl_new(DECL_FUNC, NULL, decl->span, VISIBLE_EXTERN);
	function->name = kw_mainstub;
	function->unit = decl->unit;
	function->extname = kw_main;
	function->has_extname = true;
	function->func_decl.signature.rtype = type_infoid(type_info_new_base(type_cint, decl->span));
	function->func_decl.signature.vararg_index = 2;
	Decl *param1 = decl_new_generated_var(type_cint, VARDECL_PARAM, decl->span);
	Decl *param2 = decl_new_generated_var(type_get_ptr(type_get_ptr(type_char)), VARDECL_PARAM, decl->span);
	Decl **main_params = NULL;
	vec_add(main_params, param1);
	vec_add(main_params, param2);
	function->func_decl.signature.params = main_params;
	Ast *body = new_ast(AST_COMPOUND_STMT, decl->span);
	AstId *next = &body->compound_stmt.first_stmt;
	Ast *ret_stmt = new_ast(AST_RETURN_STMT, decl->span);
	Expr *call = expr_new(EXPR_CALL, decl->span);
	call->call_expr.function = exprid(expr_variable(decl));
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
		cast_expr->cast_expr.expr = exprid(not_expr);
		cast_expr->cast_expr.type_info = type_infoid(type_info_new_base(type_cint, decl->span));
		ret_stmt->return_stmt.expr = cast_expr;
	}
	else
	{
		Ast *stmt = new_ast(AST_EXPR_STMT, decl->span);
		stmt->expr_stmt = call;
		ast_append(&next, stmt);
		ret_stmt->expr_stmt = expr_new_const_int(decl->span, type_cint, 0, true);
	}
	ast_append(&next, ret_stmt);
	assert(body);
	function->func_decl.body = astid(body);
	function->is_synthetic = true;
REGISTER_MAIN:
	context->unit->main_function = function;
	if (global_context.main)
	{
		SEMA_ERROR(function, "Duplicate main functions found.");
		SEMA_NOTE(global_context.main, "The first one was found here.");
		return false;
	}
	else
	{
		global_context.main = function;
	}
	return true;
}

static inline bool sema_analyse_func_macro(SemaContext *context, Decl *decl, bool is_func)
{
	if (!sema_analyse_attributes(context, decl, decl->attributes, is_func ? ATTR_FUNC : ATTR_MACRO)) return decl_poison(decl);
	return true;
}

static inline bool sema_analyse_xxlizer(SemaContext *context, Decl *decl)
{
	if (!sema_analyse_attributes(context, decl, decl->attributes, decl->decl_kind == DECL_INITIALIZE ? ATTR_INITIALIZER : ATTR_FINALIZER)) return decl_poison(decl);
	if (decl->xxlizer.priority == 0) decl->xxlizer.priority = MAX_PRIORITY;
	context->call_env = (CallEnv) { .kind = decl->decl_kind == DECL_INITIALIZE ? CALL_ENV_INITIALIZER : CALL_ENV_FINALIZER };
	context->rtype = type_void;
	context->active_scope = (DynamicScope) {
			.scope_id = 0,
			.depth = 0,
			.label_start = 0,
			.current_local = 0
	};

	// Clear returns
	vec_resize(context->returns, 0);
	context->scope_id = 0;
	context->continue_target = NULL;
	context->next_target = 0;
	context->next_switch = 0;
	context->break_target = 0;
	Ast *body = astptr(decl->xxlizer.init);

	// Insert an implicit return
	AstId *next_id = &body->compound_stmt.first_stmt;
	if (!*next_id)
	{
		decl->xxlizer.init = 0;
	}
	SourceSpan span = body->span;
	if (*next_id)
	{
		Ast *last = ast_last(astptr(*next_id));
		// Cleanup later
		if (last->ast_kind == AST_RETURN_STMT) goto SKIP_NEW_RETURN;
		span = last->span;
		next_id = &last->next;
	}
	Ast *ret = new_ast(AST_RETURN_STMT, span);
	ast_append(&next_id, ret);
SKIP_NEW_RETURN:
	return sema_analyse_statement(context, body);
}

static inline bool sema_analyse_func(SemaContext *context, Decl *decl)
{
	DEBUG_LOG("----Analysing function %s", decl->name);

	if (!sema_analyse_func_macro(context, decl, true)) return false;

	Type *func_type = sema_analyse_function_signature(context, decl, decl->func_decl.signature.abi, &decl->func_decl.signature, true);
	decl->type = func_type;
	if (!func_type) return decl_poison(decl);
	TypeInfo *rtype_info = type_infoptr(decl->func_decl.signature.rtype);
	assert(rtype_info);
	Type *rtype = rtype_info->type->canonical;
	if (decl->func_decl.signature.attrs.nodiscard)
	{
		if (rtype == type_void)
		{
			SEMA_ERROR(rtype_info, "@nodiscard cannot be used on functions returning 'void'.");
			return decl_poison(decl);
		}
	}
	if (decl->func_decl.signature.attrs.maydiscard)
	{
		if (!type_is_optional(rtype))
		{
			SEMA_ERROR(rtype_info, "@maydiscard can only be used on functions returning optional values.");
			return decl_poison(decl);
		}
	}
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
	bool pure = false;
	if (!sema_analyse_doc_header(decl->func_decl.docs, decl->func_decl.signature.params, NULL, &pure)) return decl_poison(decl);
	decl->func_decl.signature.attrs.is_pure = pure;
	decl->alignment = type_alloca_alignment(decl->type);
	DEBUG_LOG("Function analysis done.");
	return true;
}

static inline bool sema_is_valid_method_param(SemaContext *context, Decl *param, Type *parent_type)
{
	assert(parent_type->canonical == parent_type && "Expected already the canonical version.");
	Type *param_type = param->type;

	if (!param_type) goto ERROR;
	param_type = param_type->canonical;
	// 1. Same type ok!
	if (param_type == parent_type) return true;
	// 2. A pointer is ok!
	if (param_type->type_kind == TYPE_POINTER && param_type->pointer == parent_type) return true;
ERROR:
	SEMA_ERROR(param, "The first parameter must be of type %s or %s.", type_quoted_error_string(parent_type),
	           type_quoted_error_string(type_get_ptr(parent_type)));
	return false;
}

static bool sema_analyse_macro_method(SemaContext *context, Decl *decl)
{
	TypeInfo *parent_type_info = type_infoptr(decl->func_decl.type_parent);
	if (!sema_resolve_type_info_maybe_inferred(context, parent_type_info, true)) return false;
	Type *parent_type = parent_type_info->type;
	if (!type_may_have_method(parent_type))
	{
		SEMA_ERROR(parent_type_info,
		           "Methods can not be associated with '%s'",
		           type_to_error_string(parent_type));
		return false;
	}
	if (!vec_size(decl->func_decl.signature.params))
	{
		SEMA_ERROR(decl, "Expected at least one parameter - of type %s.", type_to_error_string(parent_type));
		return false;
	}
	Decl *first_param = decl->func_decl.signature.params[0];

	if (!sema_is_valid_method_param(context, first_param, parent_type->canonical)) return false;

	if (first_param->var.kind != VARDECL_PARAM_REF && first_param->var.kind != VARDECL_PARAM)
	{
		SEMA_ERROR(first_param, "The first parameter must be a regular or ref (&) type.");
		return false;
	}
	return unit_add_method_like(context->unit, parent_type->canonical, decl);
}

static inline bool sema_analyse_macro(SemaContext *context, Decl *decl)
{
	bool is_generic = decl->decl_kind == DECL_GENERIC;
	decl->func_decl.unit = context->unit;

	if (!sema_analyse_func_macro(context, decl, false)) return false;
	if (!sema_analyse_signature(context, &decl->func_decl.signature)) return decl_poison(decl);

	if (!decl->func_decl.signature.is_at_macro && decl->func_decl.body_param)
	{
		SEMA_ERROR(decl, "Names of macros with a trailing body must start with '@'.");
		return decl_poison(decl);
	}

	DeclId body_param = decl->func_decl.body_param;

	Decl **body_parameters = body_param ? declptr(body_param)->body_params : NULL;
	unsigned body_param_count = vec_size(body_parameters);
	for (unsigned i = 0; i < body_param_count; i++)
	{
		Decl *param = body_parameters[i];
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
		if (!sema_check_param_uniqueness_and_type(body_parameters, param, i, body_param_count)) return decl_poison(decl);
		param->resolve_status = RESOLVE_DONE;
	}
	bool pure = false;
	if (!sema_analyse_doc_header(decl->func_decl.docs, decl->func_decl.signature.params, body_parameters, &pure)) return decl_poison(decl);

	if (decl->func_decl.type_parent)
	{
		if (!sema_analyse_macro_method(context, decl)) return decl_poison(decl);
	}
	decl->type = type_void;
	return true;
}


static bool sema_analyse_attributes_for_var(SemaContext *context, Decl *decl)
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
	if (!sema_analyse_attributes(context, decl, decl->attributes, domain)) return decl_poison(decl);
	return true;
}

bool sema_analyse_decl_type(SemaContext *context, Type *type, SourceSpan span)
{
	switch (type->type_kind)
	{
		case TYPE_VOID:
			sema_error_at(span, "The use of 'void' as a variable type is not permitted.");
			return false;
		case TYPE_UNTYPED_LIST:
		case TYPE_MEMBER:
		case TYPE_TYPEINFO:
			sema_error_at(span, "The variable cannot have an compile time %s type.",
			              type_quoted_error_string(type));
			return false;
		default:
			break;
	}
	if (!type_is_optional(type)) return true;
	if (type_is_optional_any(type) || type_flatten_distinct(type->failable) == type_void)
	{
		sema_error_at(span, "The use of 'void!' as a variable type is not permitted, use %s instead.",
		                 type_quoted_error_string(type_anyerr));
		return false;
	}
	return true;
}

bool sema_analyse_var_decl_ct(SemaContext *context, Decl *decl)
{
	Expr *init;
	assert(decl->decl_kind == DECL_VAR);
	switch (decl->var.kind)
	{
		case VARDECL_LOCAL_CT_TYPE:
			// Locally declared compile time type.
			if (decl->var.type_info)
			{
				SEMA_ERROR(decl->var.type_info, "Compile time type variables may not have a type.");
				return false;
			}
			if ((init = decl->var.init_expr))
			{
				if (!sema_analyse_expr_lvalue_fold_const(context, init)) return false;
				if (init->expr_kind != EXPR_TYPEINFO)
				{
					SEMA_ERROR(decl->var.init_expr, "Expected a type assigned to %s.", decl->name);
					return false;
				}
			}
			break;
		case VARDECL_LOCAL_CT:
			if (decl->var.type_info && !sema_resolve_type_info(context, decl->var.type_info)) return false;
			if (decl->var.type_info)
			{
				decl->type = decl->var.type_info->type->canonical;
				if (!type_is_builtin(decl->type->type_kind))
				{
					SEMA_ERROR(decl->var.type_info, "Compile time variables may only be built-in types.");
					return false;
				}
				if ((init = decl->var.init_expr))
				{
					if (!sema_analyse_expr_rhs(context, decl->type, init, false)) return false;
					if (!expr_is_constant_eval(init, CONSTANT_EVAL_CONSTANT_VALUE))
					{
						SEMA_ERROR(init, "Expected a constant expression assigned to %s.", decl->name);
						return false;
					}
				}
				else
				{
					TODO // generate.
					// decl->var.init_expr =
				}
			}
			else
			{
				if ((init = decl->var.init_expr))
				{
					if (!sema_analyse_expr(context, init)) return false;
					if (!expr_is_constant_eval(init, CONSTANT_EVAL_CONSTANT_VALUE))
					{
						SEMA_ERROR(init, "Expected a constant expression assigned to %s.", decl->name);
						return false;
					}
					decl->type = init->type;
				}
				else
				{
					decl->type = type_void;
				}
			}
			break;
		default:
			UNREACHABLE
	}

	decl->var.scope_depth = context->active_scope.depth;
	return sema_add_local(context, decl);

}
/**
 * Analyse a regular global or local declaration, e.g. int x = 123
 */
bool sema_analyse_var_decl(SemaContext *context, Decl *decl, bool local)
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
		if (context->call_env.kind == CALL_ENV_GLOBAL_INIT)
		{
			if (context->current_macro)
			{
				SEMA_ERROR(decl, "Macros with declarations may not be used outside of functions.");
				return false;
			}
			SEMA_ERROR(decl, "Variable declarations may not be used outside of functions.");
			return false;
		}
		// Add a local to the current context, will throw error on shadowing.
		if (!sema_add_local(context, decl)) return decl_poison(decl);
	}

	// 1. Local or global constants: const int FOO = 123.
	if (decl->var.kind == VARDECL_CONST)
	{
		Expr *init_expr = decl->var.init_expr;
		// 1a. We require an init expression.
		if (!init_expr)
		{
			SEMA_ERROR(decl, "Constants need to have an initial value.");
			return false;
		}
		assert(!decl->var.no_init);
		if (!decl->var.type_info)
		{
			if (!sema_analyse_expr(context, init_expr)) return false;
			if (is_global && !expr_is_constant_eval(init_expr, CONSTANT_EVAL_GLOBAL_INIT))
			{
				SEMA_ERROR(init_expr, "This expression cannot be evaluated at compile time.");
				return false;
			}
			decl->type = init_expr->type;
			if (type_is_invalid_storage_type(init_expr->type))
			{
				SEMA_ERROR(init_expr, "A value of type '%s' cannot be used as a constant.", type_quoted_error_string(init_expr->type));
				return false;
			}
			if (!decl->alignment) decl->alignment = type_alloca_alignment(decl->type);
			if (!sema_analyse_decl_type(context, decl->type, init_expr->span)) return false;
			// Skip further evaluation.
			goto EXIT_OK;
		}
	}

	if (!sema_resolve_type_info_maybe_inferred(context, decl->var.type_info, decl->var.init_expr != NULL)) return decl_poison(decl);

	decl->type = decl->var.type_info->type;
	if (!sema_analyse_decl_type(context, decl->type, decl->var.type_info->span)) return false;
	bool is_static = decl->var.is_static;
	if (is_static && context->call_env.pure)
	{
		SEMA_ERROR(decl, "'@pure' functions may not have static variables.");
		return false;
	}
	if (is_static && !decl->has_extname)
	{
		scratch_buffer_clear();
		scratch_buffer_append(context->call_env.kind == CALL_ENV_FUNCTION ? context->call_env.current_function->name : ".global");
		scratch_buffer_append_char('$');
		scratch_buffer_append(decl->name);
		decl->extname = scratch_buffer_copy();
	}

	bool infer_len = type_len_is_inferred(decl->type);
	if (!decl->var.init_expr && infer_len)
	{
		SEMA_ERROR(decl->var.type_info, "The length cannot be inferred without an initializer.");
		return false;
	}
	if (decl->var.init_expr)
	{
		Expr *init = decl->var.init_expr;

		if (!infer_len)
		{
			// Pre resolve to avoid problem with recursive definitions.
			decl->resolve_status = RESOLVE_DONE;
			if (!decl->alignment) decl->alignment = type_alloca_alignment(decl->type);
		}

		CallEnvKind env_kind = context->call_env.kind;
		if (is_static) context->call_env.kind = CALL_ENV_GLOBAL_INIT;
		if (!sema_expr_analyse_assign_right_side(context, NULL, decl->type, init, false))
		{
			context->call_env.kind = env_kind;
			return decl_poison(decl);
		}
		context->call_env.kind = env_kind;

		if (infer_len)
		{
			if (type_is_len_inferred(init->type))
			{
				SEMA_ERROR(decl->var.type_info, "You cannot use [*] and [<*>] underlying types with initializers.");
				return false;
			}
			decl->type = cast_infer_len(decl->type,  init->type);
		}

		Expr *init_expr = decl->var.init_expr;

		// 2. Check const-ness
		if ((is_global || decl->var.is_static) && !expr_is_constant_eval(init_expr, CONSTANT_EVAL_GLOBAL_INIT))
		{
			SEMA_ERROR(init_expr, "The expression must be a constant value.");
		}
		else
		{
			if (decl->var.unwrap && IS_OPTIONAL(init))
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



static CompilationUnit *unit_copy(Module *module, CompilationUnit *unit)
{
	CompilationUnit *copy = unit_create(unit->file);
	copy->imports = copy_decl_list_single(unit->imports);
	copy->global_decls = copy_decl_list_single(unit->global_decls);
	copy->module = module;
	assert(!unit->functions && !unit->macro_methods && !unit->methods && !unit->enums && !unit->ct_ifs && !unit->types);
	return copy;
}

static Module *module_instantiate_generic(Module *module, Path *path, TypeInfo **parms)
{
	Module *new_module = compiler_find_or_create_module(path, NULL, module->is_private);
	new_module->is_generic = true;
	CompilationUnit **units = module->units;
	VECEACH(units, i)
	{
		vec_add(new_module->units, unit_copy(new_module, units[i]));
	}
	CompilationUnit *first_context = new_module->units[0];
	VECEACH(module->parameters, i)
	{
		const char *param = module->parameters[i];
		Decl *decl = decl_new_with_type(param, parms[i]->span, DECL_TYPEDEF, VISIBLE_PUBLIC);
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

static bool sema_analyse_parameterized_define(SemaContext *c, Decl *decl)
{
	Path *decl_path;
	const char *name;
	SourceSpan span;
	switch (decl->define_decl.define_kind)
	{
		case DEFINE_IDENT_GENERIC:
			decl_path = decl->define_decl.path;
			name = decl->define_decl.ident;
			span = decl->define_decl.span;
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
			name = define_type->unresolved.name;
			span = define_type->span;
			break;
		}
		default:
			UNREACHABLE
	}
	NameResolve name_resolve = {
			.path = decl_path,
			.span = span,
			.symbol = name
	};
	Decl *alias = unit_resolve_parameterized_symbol(c->unit, &name_resolve);
	if (!decl_ok(alias))
	{
		return decl_poison(decl);
	}

	Module *module = alias->unit->module;
	TypeInfo **params = decl->define_decl.generic_params;
	unsigned parameter_count = vec_size(module->parameters);
	assert(parameter_count > 0);
	if (parameter_count != vec_size(params))
	{
		assert(vec_size(params));
		sema_error_at(extend_span_with_token(params[0]->span, vectail(params)->span),
		              "The generic module expected %d arguments, but you supplied %d, did you make a mistake?",
		              parameter_count,
		              vec_size(decl->define_decl.generic_params));
		return decl_poison(decl);
	}
	scratch_buffer_clear();
	scratch_buffer_append_len(module->name->module, module->name->len);
	scratch_buffer_append("$$");
	FOREACH_BEGIN_IDX(i, TypeInfo *type_info, decl->define_decl.generic_params)
		if (!sema_resolve_type_info(c, type_info)) return decl_poison(decl);
		if (i != 0) scratch_buffer_append_char('.');
		type_mangle_introspect_name_to_buffer(type_info->type->canonical);
	FOREACH_END();
	TokenType ident_type = TOKEN_IDENT;
	const char *path_string = scratch_buffer_interned();
	Module *instantiated_module = global_context_find_module(path_string);
	if (!instantiated_module)
	{
		Path *path = CALLOCS(Path);
		path->module = path_string;
		path->span = module->name->span;
		path->len = scratch_buffer.len;
		instantiated_module = module_instantiate_generic(module, path, decl->define_decl.generic_params);
		sema_analyze_stage(instantiated_module, c->unit->module->stage);
	}
	if (global_context.errors_found) return decl_poison(decl);
	Decl *symbol = module_find_symbol(instantiated_module, name);
	assert(symbol);
	unit_register_external_symbol(c->compilation_unit, symbol);
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
		default:
			UNREACHABLE
	}
}

static inline bool sema_analyse_attribute_decl(SemaContext *c, Decl *decl)
{
	Decl **params = decl->attr_decl.params;
	unsigned param_count = vec_size(params);
	for (unsigned i = 0; i < param_count; i++)
	{
		Decl *param = params[i];
		if (param->var.kind != VARDECL_PARAM)
		{
			SEMA_ERROR(param, "Expected a simple replacement parameter e.g. 'val' here.");
			return false;
		}
		if (param->var.type_info)
		{
			SEMA_ERROR(param, "Type is not allowed on attribute parameters.");
			return false;
		}
		if (param->var.init_expr)
		{
			SEMA_ERROR(param, "Attribute parameters may not have default values.");
			return false;
		}
		param->resolve_status = RESOLVE_DONE;
		for (int j = 0; j < i; j++)
		{
			if (param[j].name == param->name)
			{
				SEMA_ERROR(param, "Duplicate parameter name '%s'.", param->name);
				return false;
			}
		}
	}
	return true;
}

static inline bool sema_analyse_define(SemaContext *c, Decl *decl)
{
	// 1. The plain define
	if (decl->define_decl.define_kind == DEFINE_IDENT_ALIAS)
	{
		Decl *symbol = sema_resolve_symbol(c, decl->define_decl.ident, decl->define_decl.path, decl->define_decl.span);
		if (!decl_ok(symbol)) return false;
		decl->type = symbol->type;
		decl->define_decl.alias = symbol;
		return true;
	}

	// 2. Handle type generics.
	return sema_analyse_parameterized_define(c, decl);
}

bool sema_analyse_decl(SemaContext *context, Decl *decl)
{
	if (decl->resolve_status == RESOLVE_DONE) return decl_ok(decl);

	SemaContext temp_context;
	context = context_transform_for_eval(context, &temp_context, decl->unit);
	DEBUG_LOG(">>> Analysing %s.", decl->name ? decl->name : ".anon");
	if (decl->resolve_status == RESOLVE_RUNNING)
	{
		SEMA_ERROR(decl, "Recursive definition of '%s'.", decl->name ? decl->name : ".anon");
		goto FAILED;
	}
	decl->resolve_status = RESOLVE_RUNNING;
	assert(decl->unit);
	switch (decl->decl_kind)
	{
		case DECL_BITSTRUCT:
			if (!sema_analyse_bitstruct(context, decl)) goto FAILED;
			decl_set_external_name(decl);
			break;
		case DECL_STRUCT:
		case DECL_UNION:
			if (!sema_analyse_struct_union(context, decl)) goto FAILED;
			decl_set_external_name(decl);
			break;
		case DECL_FUNC:
			if (!sema_analyse_func(context, decl)) goto FAILED;
			break;
		case DECL_MACRO:
		case DECL_GENERIC:
			if (!sema_analyse_macro(context, decl)) goto FAILED;
			break;
		case DECL_VAR:
			if (!sema_analyse_var_decl(context, decl, false)) goto FAILED;
			decl_set_external_name(decl);
			break;
		case DECL_ATTRIBUTE:
			if (!sema_analyse_attribute_decl(context, decl)) goto FAILED;
			break;
		case DECL_DISTINCT:
			if (!sema_analyse_distinct(context, decl)) goto FAILED;
			decl_set_external_name(decl);
			break;
		case DECL_TYPEDEF:
			if (!sema_analyse_typedef(context, decl)) goto FAILED;
			break;
		case DECL_ENUM:
			if (!sema_analyse_enum(context, decl)) goto FAILED;
			decl_set_external_name(decl);
			break;
		case DECL_FAULT:
			if (!sema_analyse_error(context, decl)) goto FAILED;
			decl_set_external_name(decl);
			break;
		case DECL_DEFINE:
			if (!sema_analyse_define(context, decl)) goto FAILED;
			break;
		case DECL_INITIALIZE:
		case DECL_FINALIZE:
			if (!sema_analyse_xxlizer(context, decl)) goto FAILED;
			break;
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
		case DECL_FAULTVALUE:
		case DECL_DECLARRAY:
		case DECL_BODYPARAM:
			UNREACHABLE
	}
	decl->resolve_status = RESOLVE_DONE;
	sema_context_destroy(&temp_context);
	return true;
FAILED:
	sema_context_destroy(&temp_context);
	return decl_poison(decl);
}
