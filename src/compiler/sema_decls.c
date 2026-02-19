// Copyright (c) 2019-2025 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "sema_internal.h"

static inline bool sema_analyse_func(SemaContext *context, Decl *decl, bool *erase_decl);
static inline bool sema_analyse_macro(SemaContext *context, Decl *decl, bool *erase_decl);
static inline bool sema_analyse_signature(SemaContext *context, Signature *sig, TypeInfo *method_parent, Decl *decl);
static inline bool sema_analyse_main_function(SemaContext *context, Decl *decl);
static inline bool sema_check_param_uniqueness_and_type(SemaContext *context, Decl **decls, Decl *current,
                                                        unsigned current_index);
static inline bool sema_analyse_method(SemaContext *context, Decl *decl);
static inline bool sema_is_valid_method_param(SemaContext *context, Decl *param, Type *parent_type, bool is_dynamic);
static inline bool sema_analyse_macro_method(SemaContext *context, Decl *decl);
static inline bool unit_add_base_extension_method(SemaContext *context, Type *type, Decl *method);
static inline bool type_add_method(SemaContext *context, Type *parent_type, Decl *method);
static bool sema_analyse_operator_common(SemaContext *context, Decl *method, TypeInfo **rtype_ptr, Decl ***params_ptr, uint32_t parameters);
static inline bool sema_analyse_operator_element_at(SemaContext *context, Decl *method);
static inline bool sema_analyse_operator_element_set(SemaContext *context, Decl *method);
static inline bool sema_analyse_operator_len(SemaContext *context, Decl *method);
static bool sema_check_operator_method_validity(SemaContext *context, Decl *method);

static inline const char *method_name_by_decl(Decl *method_like);

static bool sema_analyse_struct_union(SemaContext *context, Decl *decl, bool *erase_decl);
static bool sema_analyse_bitstruct(SemaContext *context, Decl *decl, bool *erase_decl);
static bool sema_analyse_union_members(SemaContext *context, Decl *decl);
static bool sema_analyse_struct_members(SemaContext *context, Decl *decl);
static inline bool sema_analyse_struct_member(SemaContext *context, Decl *parent, Decl *decl, bool *erase_decl);
static inline bool sema_check_struct_holes(SemaContext *context, Decl *decl, Decl *member);
static inline bool sema_analyse_bitstruct_member(SemaContext *context, Decl *parent, Decl *member, unsigned index, bool allow_overlap, bool *erase_decl);

static inline bool sema_analyse_doc_header(SemaContext *context, DeclId doc, Decl **params, Decl **extra_params, bool *pure_ref, bool is_raw_vaarg);

static const char *attribute_domain_to_string(AttributeDomain domain);
static bool sema_analyse_attribute(SemaContext *context, ResolvedAttrData *attr_data, Decl *decl, Attr *attr, AttributeDomain domain, bool *erase_decl);
static bool sema_analyse_attributes_inner(SemaContext *context, ResolvedAttrData *attr_data_ref, Decl *decl, Attr **attrs, AttributeDomain domain,
										  Decl *top, bool *erase_decl);
static bool sema_analyse_attributes_for_var(SemaContext *context, Decl *decl, bool *erase_decl);
static bool sema_check_section(SemaContext *context, Attr *attr);
static inline bool sema_analyse_attribute_decl(SemaContext *context, SemaContext *c, Decl *decl, bool *erase_decl);

static inline bool sema_analyse_type_alias(SemaContext *context, Decl *decl, bool *erase_decl);
static bool sema_analyse_variable_type(SemaContext *context, Type *type, SourceSpan span);
static inline bool sema_analyse_alias(SemaContext *context, Decl *decl, bool *erase_decl);
static inline bool sema_analyse_typedef(SemaContext *context, Decl *decl, bool *erase_decl);

static inline bool sema_resolve_align_expr(SemaContext *context, Expr *expr, AlignSize *result)
{
	if (!sema_analyse_expr_rvalue(context, expr)) return false;
	if (!expr_is_const_int(expr))
	{
		RETURN_SEMA_ERROR(expr, "Expected a constant integer value as argument.");
	}
	if (int_ucomp(expr->const_expr.ixx, MAX_ALIGNMENT, BINARYOP_GT))
	{
		RETURN_SEMA_ERROR(expr, "Alignment must be less or equal to %ull.", MAX_ALIGNMENT);
	}
	if (int_ucomp(expr->const_expr.ixx, 0, BINARYOP_LE))
	{
		RETURN_SEMA_ERROR(expr, "Alignment must be greater than zero.");
	}
	uint64_t align = int_to_u64(expr->const_expr.ixx);
	if (!is_power_of_two(align))
	{
		RETURN_SEMA_ERROR(expr, "Alignment must be a power of two.");
	}
	*result = (AlignSize)align;
	return true;
}
static inline bool sema_analyse_enum_param(SemaContext *context, Decl *param);
static inline bool sema_analyse_enum(SemaContext *context, Decl *decl, bool *erase_decl);
static inline bool sema_analyse_constdef(SemaContext *context, Decl *decl, bool *erase_decl);

static bool sema_check_section(SemaContext *context, Attr *attr)
{
	Expr *expr = attr->exprs[0];
	const char *section_string = expr->const_expr.bytes.ptr;
	// No restrictions except for MACH-O
	if (compiler.platform.object_format != OBJ_FORMAT_MACHO)
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

	if (slice.len) RETURN_SEMA_ERROR(expr, "Too many parts to the Mach-o section description.");

	slice_trim(&segment);
	if (!segment.len) RETURN_SEMA_ERROR(expr, "The segment is missing, did you type it correctly?");

	slice_trim(&section);
	if (!section.len) RETURN_SEMA_ERROR(expr, "Mach-o requires 'segment,section' as the format, did you type it correctly?");

	if (section.len > 16) RETURN_SEMA_ERROR(expr, "Mach-o requires the section to be at the most 16 characters, can you shorten it?");

	REMINDER("Improve section type checking for Mach-o like Clang has.");
	return true;
}

/**
 * Check parameter name uniqueness and that the type is not void.
 */
static inline bool sema_check_param_uniqueness_and_type(SemaContext *context, Decl **decls, Decl *current,
                                                        unsigned current_index)
{

	const char *name = current->name;
	// There is no need to do a check if it is anonymous.
	if (!name) return true;

	// Check for a duplicate name, this algorithm is O(n^2),
	// but is fine as parameters are typically few, and doing something like
	// a hash map would be more expensive to set up.
	for (int i = 0; i < current_index; i++)
	{
		if (decls[i] && name == decls[i]->name)
		{
			SEMA_ERROR(current, "Duplicate parameter name '%s'.", name);
			SEMA_NOTE(decls[i], "Previous use of the name was here.");
			decl_poison(current);
			return false;
		}
	}
	return true;
}

/**
 * Look at all the interface declarations, optionally type check the interfaces completely if needed.
 */
static inline bool sema_resolve_implemented_interfaces(SemaContext *context, Decl *decl, bool deep)
{
	TypeInfo **interfaces = decl->interfaces;
	unsigned count = vec_size(interfaces);
	// No interfaces? Then we're done. This is the most common case.
	if (!count) return true;

	for (unsigned i = 0; i < count; i++)
	{
		TypeInfo *inf_info = interfaces[i];
		// Resolve the name
		if (!sema_resolve_type_info(context, inf_info, RESOLVE_TYPE_DEFAULT)) return false;
		Type *inf_type = inf_info->type->canonical;
		if (inf_type->type_kind != TYPE_INTERFACE)
		{
			RETURN_SEMA_ERROR(inf_info, "Expected an interface name, but %s is not an interface.",
							  type_quoted_error_string(inf_type));
		}
		// We don't permit duplicates as this would affect the implementation ordering.
		for (unsigned j = 0; j < i; j++)
		{
			if (interfaces[j]->type->canonical == inf_type)
			{
				RETURN_SEMA_ERROR(inf_info, "The interface '%s' was included more than once, "
											"this is not allowed, so please remove the duplicates.",
								  inf_type->name);
			}
		}
		// If this is a deep check, then we also resolve the interface itself (this would mean resolving function types)
		if (deep && !sema_resolve_type_decl(context, inf_type)) return false;
	}
	return true;
}

/**
 * Analyse a struct or union field.
 *
 * This will also push the member to the current declaration stack.
 */
static inline bool sema_analyse_struct_member(SemaContext *context, Decl *parent, Decl *decl, bool *erase_decl)
{
	// Resolution might already have been completed in some cases due to $checks
	if (decl->resolve_status == RESOLVE_DONE)
	{
		if (!decl_ok(decl)) return false;
		if (decl->name) sema_decl_stack_push(decl);
		return true;
	}

	// Detect circular dependency.
	if (decl->resolve_status == RESOLVE_RUNNING) RETURN_SEMA_ERROR(decl, "Circular dependency resolving member.");

	// Mark the unit, it should not have been assigned at this point.
	ASSERT_SPAN(decl, !decl->unit || decl->is_templated || decl->unit == parent->unit);
	decl->unit = parent->unit;

	// Pick the domain for attribute analysis.
	AttributeDomain domain = ATTR_MEMBER;
	switch (decl->decl_kind)
	{
		case DECL_BITSTRUCT:
			domain = ATTR_BITSTRUCT;
			break;
		case DECL_UNION:
			domain = ATTR_UNION;
			break;
		case DECL_STRUCT:
			domain = ATTR_STRUCT;
			break;
		case DECL_VAR:
			break;
		default:
			UNREACHABLE
	}
	// Check attributes.
	if (!sema_analyse_attributes(context, decl, decl->attributes, domain, erase_decl)) return decl_poison(decl);

	// If we should erase this declaration due to an @if, exit here.
	if (*erase_decl) return true;

	// If it has a name, place it in the decl stack to ensure it is unique.
	if (decl->name)
	{
		Decl *other = sema_decl_stack_resolve_symbol(decl->name);
		if (other)
		{
			SEMA_ERROR(decl, "Duplicate member name '%s'.", other->name);
			SEMA_NOTE(other, "Previous declaration was here.");
			return false;
		}
		// Push the name onto the stack.
		sema_decl_stack_push(decl);
	}

	bool is_export = parent->is_export;

	// Analysis depends on the underlying type.
	switch (decl->decl_kind)
	{
		// A regular member
		case DECL_VAR:
		{
			ASSERT_SPAN(decl, decl->var.kind == VARDECL_MEMBER);
			decl->resolve_status = RESOLVE_RUNNING;

			// Resolve the type
			// Inferred types are not strictly allowed, but we use the int[*] for the flexible array member.
			ASSERT_SPAN(decl, decl->var.type_info);
			TypeInfo *type_info = type_infoptr(decl->var.type_info);
			if (!sema_resolve_type_info(context, type_info, RESOLVE_TYPE_ALLOW_FLEXIBLE)) return false;

			// Check the resulting storage type:
			Type *type = type_info->type;
			switch (sema_resolve_storage_type(context, type))
			{
				case STORAGE_ERROR:
					return false;
				case STORAGE_NORMAL:
					break;
				case STORAGE_VOID:
					RETURN_SEMA_ERROR(type_info, "Members cannot be %s.", type_quoted_error_string(type));
				case STORAGE_COMPILE_TIME:
					RETURN_SEMA_ERROR(type_info, "Members cannot be %s.", type_invalid_storage_type_name(type));
				case STORAGE_WILDCARD:
				case STORAGE_UNKNOWN:
					RETURN_SEMA_ERROR(type_info, "%s has unknown size and cannot be used as a member.", type_quoted_error_string(type));
			}
			decl->type = type;
			decl->resolve_status = RESOLVE_DONE;
			return true;
		}
		// It might be a struct of union
		case DECL_STRUCT:
		case DECL_UNION:
			// Extend the nopadding attributes to nested structs.
			if (parent->attr_nopadding) decl->attr_nopadding = true;
			if (parent->attr_compact) decl->attr_compact = true;
			FALLTHROUGH;
			// The common case for all inline types:
		case DECL_BITSTRUCT:
			// Set the nested type as export if this one is exported.
			decl->is_export = is_export;
			// Perform the analysis
			if (!sema_analyse_decl(context, decl)) return false;
			return true;
		default:
			UNREACHABLE
	}
}

static inline bool sema_check_struct_holes(SemaContext *context, Decl *decl, Decl *member)
{
	Type* member_type = type_flatten(member->type);
	if (!type_is_union_or_strukt(member_type)) return true;
	ASSERT(decl_is_struct_type(member_type->decl));
	if (!member_type->decl->strukt.padded_decl_id) return true;
	if (!decl->strukt.padded_decl_id) decl->strukt.padded_decl_id = member_type->decl->strukt.padded_decl_id;
	if (decl->attr_compact)
	{
		SEMA_ERROR(member, "%s has padding and can't be used as the type of '%s', because members of a `@compact` type must all have zero padding.", type_quoted_error_string(member_type), member->name);
		Decl* padded_decl = declptr(member_type->decl->strukt.padded_decl_id);
		if (decl_is_struct_type(padded_decl))
		{
			SEMA_NOTE(padded_decl, "The first padding in %s would be added to the end of this type.",
			          type_quoted_error_string(member_type));
		}
		else
		{
			SEMA_NOTE(padded_decl, "The first padded field in %s is here.", type_quoted_error_string(member_type));
		}
		return false;
	}
	return true;
}


/**
 * Analyse union members, calculating alignment.
 */
static bool sema_analyse_union_members(SemaContext *context, Decl *decl)
{
	AlignSize max_size = 0;
	ArrayIndex max_alignment_element = 0;
	AlignSize max_alignment = 0;

	Decl **members = decl->strukt.members;
	unsigned member_count = vec_size(members);
	ASSERT(member_count > 0);

	// Check all members
	for (unsigned i = 0; i < member_count; i++)
	{
		AGAIN:;
		Decl *member = members[i];

		// The member may already have been resolved if it is poisoned, then exit.
		if (!decl_ok(member)) return false;
		bool erase_decl = false;

		// Check the member
		if (!sema_analyse_struct_member(context, decl, member, &erase_decl)) // NOLINT
		{
			// Failed
			return decl_poison(member);
		}

		// If we need to erase it then do so.
		if (erase_decl)
		{
			vec_erase_at(members, i);
			member_count--;
			// Go back and take the next one.
			if (i < member_count) goto AGAIN;
			break;
		}
		if (member->type->type_kind == TYPE_INFERRED_ARRAY)
		{
			decl_poison(member);
			RETURN_SEMA_ERROR(member, "Flexible array members not allowed in unions.");
		}
		AlignSize member_alignment;
		if (!sema_set_alignment(context, member->type, &member_alignment, false)) return false;
		if (!sema_check_struct_holes(context, decl, member)) return false;

		ByteSize member_size = type_size(member->type);
		if (member_size > MAX_STRUCT_SIZE) RETURN_SEMA_ERROR(member, "Union member '%s' would cause the union to become too large (exceeding 2 GB).", member->name);

		ASSERT(member_size <= MAX_TYPE_SIZE);
		// Update max alignment
		if (member->alignment > member_alignment) member_alignment = member->alignment;
		if (member_alignment > max_alignment)
		{
			max_alignment = member_alignment;
			max_alignment_element = (ArrayIndex)i;
		}
		// Update max size
		if (member_size > max_size)
		{
			//max_size_element = i;
			max_size = (AlignSize)member_size;
			// If this is bigger than the previous with max
			// alignment, pick this as the maximum size field.
			if (max_alignment_element != (ArrayIndex)i && max_alignment == member_alignment)
			{
				max_alignment_element = (ArrayIndex)i;
			}
		}
		// Offset is always 0
		member->offset = 0;
	}
	if (!member_count)
	{
		RETURN_SEMA_ERROR(decl, "No union members exist after processing attributes, this is not allowed. Please make sure it has at least one member.");
	}
	ASSERT(decl_ok(decl));

	// 1. If packed, then the alignment is zero, unless previously given
	if (decl->is_packed && !decl->alignment) decl->alignment = 1;

	// 2. otherwise pick the highest of the natural alignment and the given alignment.
	if (!decl->is_packed && decl->alignment < max_alignment) decl->alignment = max_alignment;

	// We're only packed if the max alignment is > 1
	decl->is_packed = decl->is_packed && max_alignment > 1;

	// "Representative" type is the one with the maximum alignment.
	ASSERT(max_alignment_element >= 0);
	decl->strukt.union_rep = max_alignment_element;

	// All members share the same alignment
	FOREACH(Decl *, member, members)
	{
		member->alignment = decl->alignment;
	}

	ASSERT(max_size);

	// The actual size might be larger than the max size due to alignment.
	AlignSize size = aligned_offset(max_size, decl->alignment);

	ByteSize rep_size = type_size(members[max_alignment_element]->type);

	// If the actual size is bigger than the real size, add
	// padding – typically used with LLVM lowering.
	if (size > rep_size)
	{
		decl->strukt.padding = (AlignSize)(size - rep_size);
	}

	decl->strukt.size = size;
	return true;
}

AlignSize sema_get_max_natural_alignment(Type *type)
{
RETRY:;
	switch (type->type_kind)
	{
		case TYPE_OPTIONAL:
			type = type->optional;
			goto RETRY;
		case TYPE_TYPEDEF:
			type = type->decl->distinct->type;
			goto RETRY;
		case TYPE_ALIAS:
			type = type->canonical;
			goto RETRY;
		case TYPE_POISONED:
		case TYPE_UNTYPED_LIST:
		case TYPE_WILDCARD:
		case TYPE_TYPEINFO:
		case TYPE_MEMBER:
			UNREACHABLE
		case TYPE_VOID:
			return 1;
		case TYPE_BOOL:
			return 1;
		case ALL_INTS:
		case ALL_FLOATS:
			return type->builtin.abi_alignment;
		case TYPE_ANY:
		case TYPE_INTERFACE:
		case TYPE_ANYFAULT:
		case TYPE_TYPEID:
		case TYPE_FUNC_PTR:
		case TYPE_POINTER:
		case TYPE_FUNC_RAW:
		case TYPE_SLICE:
			return compiler.platform.align_pointer.align / 8;
		case TYPE_ENUM:
		case TYPE_CONST_ENUM:
			type = enum_inner_type(type);
			goto RETRY;
		case TYPE_STRUCT:
		case TYPE_UNION:
		{
			AlignSize max = 0;
			FOREACH(Decl *, member, type->decl->strukt.members)
			{
				AlignSize member_max = sema_get_max_natural_alignment(member->type);
				if (member_max > max) max = member_max;
			}
			return max;
		}
		case TYPE_BITSTRUCT:
			type = type->decl->strukt.container_type->type;
			goto RETRY;
		case TYPE_SIMD_VECTOR:
			return type_abi_alignment(type);
		case TYPE_ARRAY:
		case TYPE_FLEXIBLE_ARRAY:
		case TYPE_INFERRED_ARRAY:
		case TYPE_VECTOR:
		case TYPE_INFERRED_VECTOR:
			type = type->array.base;
			goto RETRY;
	}
	UNREACHABLE
}
/*
 * Analyse the members of a struct, assumed to be 1 or more (should already be checked before calling the function)
 */
static bool sema_analyse_struct_members(SemaContext *context, Decl *decl)
{
	// Default alignment is 1 even if it is empty.
	AlignSize natural_alignment = 1;
	AlignSize actual_alignment = 1;
	bool is_unaligned = false;
	AlignSize size = 0;
	AlignSize offset = 0;
	bool is_packed = decl->is_packed;
	Decl **struct_members = decl->strukt.members;
	unsigned member_count = vec_size(struct_members);
	ASSERT(member_count > 0 && "This analysis should only be called on member_count > 0");
	bool is_naturally_aligned = !is_packed;
	for (unsigned i = 0; i < member_count; i++)
	{
	AGAIN:;
		Decl *member = struct_members[i];
		// We might have already analysed and poisoned this decl, if so, exit.
		if (!decl_ok(member)) return decl_poison(decl);
		bool erase_decl = false;
		// Check the member
		if (!sema_analyse_struct_member(context, decl, member, &erase_decl)) // NOLINT
		{
			return decl_poison(decl);
		}
		// If we should erase it, do so.
		if (erase_decl)
		{
			vec_erase_at(struct_members, i);
			member_count--;
			if (i < member_count) goto AGAIN;
			break;
		}

		Type *member_type = type_flatten(member->type);
		// If this is a struct and it has a variable array ending, then it must also be the last struct.
		// So this is ok:
		// struct Foo { int x; struct { int x; int[*] y; } }
		// But not this:
		// struct Bar { struct { int x; int[*] y; } int x; }
		if (member_type->type_kind == TYPE_STRUCT && member_type->decl->has_variable_array)
		{
			if (i != member_count - 1)
			{
				decl_poison(member);
				RETURN_SEMA_ERROR(member, "A struct member with a flexible array must be the last element.");
			}
			// Mark it as a variable array.
			decl->has_variable_array = true;
		}
		else if (member_type->type_kind == TYPE_INFERRED_ARRAY)
		{
			// Check chat it is the last element.
			if (i != member_count - 1)
			{
				decl_poison(member);
				RETURN_SEMA_ERROR(member, "The flexible array member must be the last element.");
			}
			// And that it isn't the only element.
			if (i == 0)
			{
				decl_poison(member);
				RETURN_SEMA_ERROR(member, "The flexible array member cannot be the only element.");
			}
			// Now replace the type, because we want a TYPE_FLEXIBLE_ARRAY rather than the assumed TYPE_INFERRED_ARRAY
			member->type = type_get_flexible_array(member->type->array.base);
			// And mark as variable array
			decl->has_variable_array = true;
		}

		ASSERT(decl_ok(decl) && "The declaration should be fine at this point.");

		// Grab the alignment of the member type
		AlignSize member_type_alignment;
		if (type_is_user_defined(member_type) && member_type->decl->resolve_status == RESOLVE_RUNNING)
		{
			SEMA_ERROR(member, "Recursive definition of %s.", type_quoted_error_string(member_type));
			return decl_poison(decl);
		}
		if (!sema_set_alignment(context, member->type, &member_type_alignment, false)) return decl_poison(decl);
		// And get the natural alignment
		AlignSize member_natural_alignment = sema_get_max_natural_alignment(member->type);

		// If packed, then the alignment is 1
		AlignSize member_alignment = is_packed ? 1 : member_type_alignment;
		// If a member has an assigned alignment, then we use that one, even if it is packed.
		if (member->alignment)
		{
			member_alignment = member->alignment;
			// Update total alignment if we have a member that has bigger alignment.
			if (member_alignment > decl->alignment) decl->alignment = member_alignment;
		}

		// If the natural alignment is higher than the currently detected alignment,
		// then we update the natural alignment
		if (member_natural_alignment > natural_alignment)
		{
			natural_alignment = member_natural_alignment;
		}
		if (member_alignment > actual_alignment) actual_alignment = member_alignment;

		// In the case of a struct, we will align this to the next offset,
		// using the alignment of the member.
		unsigned align_offset = aligned_offset(offset, member_alignment);

		unsigned natural_align_offset = aligned_offset(offset, member_natural_alignment);

		// If the natural align is different from the aligned offset we have two cases:
		if (natural_align_offset != align_offset)
		{
			is_naturally_aligned = false;
			// If the natural alignment is greater, in this case the struct is unaligned.
			if (member_natural_alignment > member_alignment)
			{
				ASSERT(natural_align_offset > align_offset);
				is_unaligned = true;
			}
			else
			{
				// Otherwise we have a greater offset, and in this case
				// we add padding for the difference.
				ASSERT(natural_align_offset < align_offset);
				member->padding = align_offset - offset;
			}
		}

		member->alignment = member_alignment;

		if (align_offset - offset != 0)
		{
			ASSERT(decl_is_struct_type(decl));
			if (!decl->strukt.padded_decl_id) decl->strukt.padded_decl_id = declid(member);
			if (decl->attr_nopadding || member->attr_nopadding)
			{
				RETURN_SEMA_ERROR(member, "%d bytes of padding would be added to align this member which is not allowed with `@nopadding` and `@compact`.", align_offset - offset);
			}
			member->padding = align_offset - offset;
		}

		if (!sema_check_struct_holes(context, decl, member)) return false;

		offset = align_offset;
		member->offset = offset;
		AlignSize sz = offset;
		offset += type_size(member->type);
		if (offset < sz || offset > MAX_STRUCT_SIZE) RETURN_SEMA_ERROR(member, "Struct member '%s' would cause the struct to become too large (exceeding 2 GB).", member->name);
	}

	if (!member_count)
	{
		RETURN_SEMA_ERROR(decl, "No members exist for this struct after processing attributes, creating an invalid empty struct. Please make sure every struct/inner struct has at least one member.");
	}

	// Set the alignment:

	// 1. If packed, use the alignment given, otherwise set to 1.
	if (decl->is_packed && !decl->alignment) decl->alignment = 1;

	// 2. otherwise pick the highest of the actual alignment and the given alignment.
	if (!decl->is_packed && decl->alignment < actual_alignment) decl->alignment = actual_alignment;

	// We must now possibly add the end padding.
	// First we calculate the actual size
	size = aligned_offset(offset, decl->alignment);

	// We might get a size that is greater than the natural alignment
	// in this case we need an additional padding
	AlignSize natural_size = aligned_offset(offset, natural_alignment);
	if (size > natural_size)
	{
		decl->strukt.padding = size - offset;
	}

	// If the size is smaller the naturally aligned struct, then it is also unaligned
	if (size < natural_size)
	{
		is_unaligned = true;
	}
	if (is_unaligned && size > offset)
	{
		ASSERT(!decl->strukt.padding || decl->strukt.padding == size - offset);
		decl->strukt.padding = size - offset;
	}

	if (decl->attr_nopadding && type_is_substruct(decl->type))
	{
		Decl *first_member = struct_members[0];
		Type *type = type_flatten(first_member->type);
		if (type_is_union_or_strukt(type) && !type->decl->attr_nopadding)
		{
			RETURN_SEMA_ERROR(first_member, "An inlined struct or union type also requires the `@nopadding` attribute.");
		}
	}

	if (size != offset)
	{
		ASSERT(decl_is_struct_type(decl));
		if (!decl->strukt.padded_decl_id) decl->strukt.padded_decl_id = declid(decl);
		if (decl->attr_nopadding)
		{
			RETURN_SEMA_ERROR(decl, "%d bytes of padding would be added to the end this struct which is not allowed with `@nopadding` and `@compact`.", size - offset);
		}
	}

	decl->is_packed |= is_unaligned;
	// Strip padding if we are aligned.
	if (!decl->is_packed && is_naturally_aligned)
	{
		for (unsigned i = 0; i < member_count; i++)
		{
			Decl *member = struct_members[i];
			member->padding = 0;
		}
	}
	decl->strukt.size = size;
	return true;
}

static bool sema_analyse_struct_union(SemaContext *context, Decl *decl, bool *erase_decl)
{
	// Begin by analysing attributes
	bool is_union = decl->decl_kind == DECL_UNION;
	AttributeDomain domain = is_union ? ATTR_UNION : ATTR_STRUCT;
	if (!sema_analyse_attributes(context, decl, decl->attributes, domain, erase_decl)) return decl_poison(decl);
	if (decl_is_deprecated(decl)) context->call_env.ignore_deprecation = true;

	// If an @if attribute erases it, end here
	if (*erase_decl) return true;

	// Resolve any implemented interfaces shallowly (i.e. functions are not resolved)
	if (!sema_resolve_implemented_interfaces(context, decl, false)) return decl_poison(decl);

	DEBUG_LOG("Beginning analysis of %s.", decl->name ? decl->name : ".anon");
	Decl **members = decl->strukt.members;

	// We require at least one member in C3. This simplifies semantics in corner cases.
	if (!vec_size(members))
	{
		RETURN_SEMA_ERROR(decl, "Zero sized %s are not permitted.", is_union ? "unions" : "structs");
	}

	// If we have a name, we need to create a new decl stack
	Decl** state = decl->name ? sema_decl_stack_store() : NULL;

	bool success = is_union
			? sema_analyse_union_members(context, decl)
			: sema_analyse_struct_members(context, decl);

	// Restore if needed.
	if (decl->name) sema_decl_stack_restore(state);

	DEBUG_LOG("Struct/union size %d, alignment %d.", (int)decl->strukt.size, (int)decl->alignment);
	DEBUG_LOG("Analysis complete.");

	// Failed, so exit.
	if (!success) return decl_poison(decl);

	ASSERT(decl_ok(decl));
	return true;
}

static inline bool sema_analyse_bitstruct_member(SemaContext *context, Decl *parent, Decl *member, unsigned index, bool allow_overlap, bool *erase_decl)
{

	if (member->resolve_status == RESOLVE_DONE)
	{
		if (!decl_ok(member)) return false;
		if (member->name) sema_decl_stack_push(member);
		return true;
	}

	TypeInfo *type_info = type_infoptr(member->var.type_info);
	if (member->resolve_status == RESOLVE_RUNNING)
	{
		RETURN_SEMA_ERROR(member, "Circular dependency resolving member.");
	}

	if (!sema_analyse_attributes(context, member, member->attributes, ATTR_BITSTRUCT_MEMBER, erase_decl)) return decl_poison(member);
	if (*erase_decl) return true;

	if (member->name)
	{
		Decl *other = sema_decl_stack_resolve_symbol(member->name);
		if (other)
		{
			SEMA_ERROR(member, "Duplicate member name '%s'.", other->name);
			SEMA_NOTE(other, "Previous declaration was here.");
			return false;
		}
		if (member->name) sema_decl_stack_push(member);
	}

	bool is_consecutive = parent->strukt.consecutive;

	// Resolve the type.
	if (!sema_resolve_type_info(context, type_info, RESOLVE_TYPE_DEFAULT)) return false;
	member->type = type_info->type;

	// Flatten the distinct and enum types.
	Type *member_type = type_flatten_for_bitstruct(member->type);

	// Only accept (flattened) integer and bool types
	if (!type_is_integer(member_type) && member_type != type_bool)
	{
		SEMA_ERROR(type_info, "%s is not supported in a bitstruct, only enums, integer and boolean values may be used.",
				   type_quoted_error_string(member->type));
		return false;
	}

	// Grab the underlying bit type size.
	BitSize bits = type_size(parent->strukt.container_type->type) * (BitSize)8;

	if (bits > MAX_BITSTRUCT)
	{
		SEMA_ERROR(parent->strukt.container_type, "Bitstruct size may not exceed %d bits.", MAX_BITSTRUCT);
		return false;
	}
	Int max_bits = (Int) { .type = TYPE_I64, .i = { .low =  bits } };

	// Resolve the bit range, starting with the beginning

	unsigned start_bit, end_bit;

	if (is_consecutive)
	{
		ASSERT(!member->var.bit_is_expr && "Should always me inferred");
		if (member_type != type_bool)
		{
			SEMA_ERROR(type_info, "For bitstructs without bit ranges, the types must all be 'bool'.");
			return false;
		}
		start_bit = end_bit = member->var.start_bit;
		if (start_bit >= bits)
		{
			SEMA_ERROR(member, "This element would overflow the bitstruct size (%d bits).", bits);
			return false;
		}
		goto AFTER_BIT_CHECK;
	}

	if (member->var.bit_is_expr)
	{
		Expr *start = member->var.start;
		if (!sema_analyse_expr_rvalue(context, start)) return false;

		// Check for negative, non integer or non const values.
		if (!sema_cast_const(start) || !type_is_integer(start->type) || int_is_neg(start->const_expr.ixx))
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

		end_bit = start_bit = (unsigned)start->const_expr.ixx.i.low;

		// Handle the end
		Expr *end = member->var.end;
		if (end)
		{
			// Analyse the end
			if (!sema_analyse_expr_rvalue(context, end)) return false;
			if (!sema_cast_const(end) || !type_is_integer(end->type) || int_is_neg(end->const_expr.ixx))
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
	}
	else
	{
		start_bit = member->var.start_bit;
		end_bit = member->var.end_bit;
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
	member->var.bit_is_expr = false;

AFTER_BIT_CHECK:
	// Check for overlap
	if (!allow_overlap)
	{
		Decl **members = parent->strukt.members;
		for (unsigned i = 0; i < index; i++)
		{
			Decl *other_member = members[i];
			// Check for overlap.
			if ((start_bit >= other_member->var.start_bit || end_bit >= other_member->var.start_bit)
			    && start_bit <= other_member->var.end_bit)
			{
				SEMA_ERROR(member, "Overlapping members, please use '@overlap' if this is intended.");
				SEMA_NOTE(other_member, "The other member was declared here.");
				return false;
			}
		}
	}
	member->resolve_status = RESOLVE_DONE;
	return true;
}

/*
 * Analyse an interface declaration, because it is only called from analyse_decl, it is safe
 * to just return false (and not poison explicitly), if it should be called from elsewhere,
 * then this needs to be handled.
 */
static bool sema_analyse_interface(SemaContext *context, Decl *decl, bool *erase_decl)
{
	// Begin with analysing attributes.
	if (!sema_analyse_attributes(context, decl, decl->attributes, ATTR_INTERFACE, erase_decl)) return false;

	if (decl_is_deprecated(decl)) context->call_env.ignore_deprecation = true;

	// If erased using @if, we exit.
	if (*erase_decl) return true;

	// Resolve the inherited interfaces deeply
	if (!sema_resolve_implemented_interfaces(context, decl, true)) return false;

	// Walk through the methods.
	Decl **functions = decl->interface_methods;
	unsigned count = vec_size(functions);

	// Note that zero functions are allowed, this is useful for creating combinations of interfaces.
	for (unsigned i = 0; i < count; i++)
	{
		RETRY:;
		Decl *method = functions[i];
		if (method->name == kw_ptr || method->name == kw_type)
		{
			RETURN_SEMA_ERROR(method, "The method name '%s' would shadow the built-in property '.%s', "
							 "please select a different name.", method->name, method->name);
		}
		// The method might have been resolved earlier, if so we either exit or go to the next.
		// This might happen for example if it was resolved using $defined
		if (method->resolve_status == RESOLVE_DONE)
		{
			if (!decl_ok(method)) return false;
			continue;
		}
		if (method->resolve_status == RESOLVE_RUNNING)
		{
			SEMA_ERROR(method, "Recursive definition of method, this is not allowed.");
			return decl_poison(method);
		}
		method->unit = decl->unit;
		method->resolve_status = RESOLVE_RUNNING;
		if (method->decl_kind != DECL_FUNC)
		{
			SEMA_ERROR(method, "Only functions are allowed here.");
			return decl_poison(method);
		}
		if (method->func_decl.type_parent)
		{
			SEMA_ERROR(type_infoptr(method->func_decl.type_parent), "Interfaces should not be declared as methods.");
			return decl_poison(method);
		}
		bool erase = false;

		// Insert the first parameter, which is the implicit `void*`
		Decl *first = decl_new_var(NULL, decl->span, NULL, VARDECL_PARAM);
		first->type = type_voidptr;
		first->var.kind = VARDECL_PARAM;
		first->unit = context->unit;
		first->resolve_status = RESOLVE_DONE;
		first->alignment = type_alloca_alignment(type_voidptr);
		vec_insert_first(method->func_decl.signature.params, first);
		method->unit = context->unit;
		method->func_decl.signature.vararg_index += 1;

		// Now we analyse the function as a regular function.
		if (!sema_analyse_func(context, method, &erase))
		{
			// This is necessary in order to allow this check to run again.
			decl_poison(method);
			vec_erase_at(method->func_decl.signature.params, 0);
			return false;
		}

		// We might need to erase the function.
		if (erase)
		{
			vec_erase_at(functions, i);
			count--;
			if (i >= count) break;
			goto RETRY;
		}

		const char *name = method->name;
		// Do a simple check to ensure the same function isn't defined twice.
		// note that this doesn't check if it's overlapping an inherited method, this is deliberate.
		for (unsigned j = 0; j < i; j++)
		{
			if (functions[j]->name == name)
			{
				SEMA_ERROR(method, "Duplicate definition of method '%s'.", name);
				SEMA_NOTE(functions[j], "The previous definition was here.");
				return decl_poison(method);
			}
		}
		method->resolve_status = RESOLVE_DONE;
	}
	return true;
}

static bool sema_deep_resolve_function_ptr(SemaContext *context, TypeInfo *type_to_resolve)
{
	Type *type = type_to_resolve->type;
RETRY:
	switch (type->type_kind) // NOLINT
	{
		case TYPE_POISONED:
		case TYPE_VOID:
		case TYPE_BOOL:
		case ALL_INTS:
		case ALL_FLOATS:
		case ALL_VECTORS:
		case TYPE_ANY:
		case TYPE_INTERFACE:
		case TYPE_ANYFAULT:
		case TYPE_TYPEID:
		case TYPE_ENUM:
		case TYPE_CONST_ENUM:
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_BITSTRUCT:
		case TYPE_TYPEDEF:
		case TYPE_UNTYPED_LIST:
		case TYPE_WILDCARD:
		case TYPE_TYPEINFO:
		case TYPE_MEMBER:
			return true;
		case TYPE_ALIAS:
			type = type->canonical;
			goto RETRY;
		case TYPE_POINTER:
		case TYPE_FUNC_PTR:
			type = type->pointer;
			goto RETRY;
		case TYPE_FUNC_RAW:
			return sema_analyse_decl(context, type->decl);
		case TYPE_OPTIONAL:
			type = type->optional;
			goto RETRY;
		case TYPE_ARRAY:
		case TYPE_SLICE:
		case TYPE_INFERRED_ARRAY:
		case TYPE_FLEXIBLE_ARRAY:
			type = type->array.base;
			goto RETRY;
	}
	UNREACHABLE
}

static bool sema_analyse_bitstruct(SemaContext *context, Decl *decl, bool *erase_decl)
{
	if (!sema_analyse_attributes(context, decl, decl->attributes, ATTR_BITSTRUCT, erase_decl)) return decl_poison(decl);
	if (decl_is_deprecated(decl)) context->call_env.ignore_deprecation = true;

	if (!sema_resolve_implemented_interfaces(context, decl, false)) return decl_poison(decl);
	if (decl_is_deprecated(decl)) context->call_env.ignore_deprecation = true;

	if (*erase_decl) return true;
	DEBUG_LOG("Beginning analysis of %s.", decl->name ? decl->name : ".anon");
	if (!sema_resolve_type_info(context, decl->strukt.container_type, RESOLVE_TYPE_DEFAULT)) return false;
	Type *type = type_flatten(decl->strukt.container_type->type->canonical);
	if (!sema_resolve_type_decl(context, type)) return false;
	if (type_size(type) == 1)
	{
		decl->strukt.big_endian = false;
		decl->strukt.little_endian = false;
	}
	Type *base_type = type->type_kind == TYPE_ARRAY ? type_flatten(type->array.base) : type;
	if (!type_is_integer(base_type) || (type->type_kind == TYPE_ARRAY && type_size(type->array.base) > 1))
	{
		RETURN_SEMA_ERROR(decl->strukt.container_type, "The type of the bitstruct cannot be %s but must be an integer or an array of chars.",
				type_quoted_error_string(decl->strukt.container_type->type));
	}
	Decl **members = decl->strukt.members;
	unsigned member_count = vec_size(members);

	Decl **state = decl->name ? sema_decl_stack_store() : NULL;
	for (unsigned i = 0; i < member_count; i++)
	{
		AGAIN:;
		Decl *member = members[i];
		if (!decl_ok(member)) goto ERROR;
		bool erase_decl_member = false;
		if (!sema_analyse_bitstruct_member(context, decl, member, i, decl->strukt.overlap, &erase_decl_member)) goto ERROR;
		if (erase_decl_member)
		{
			vec_erase_at(members, i);
			member_count--;
			if (i < member_count) goto AGAIN;
			break;
		}
	}
	if (state) sema_decl_stack_restore(state);
	return true;
ERROR:
	if (state) sema_decl_stack_restore(state);
	return decl_poison(decl);
}

static inline bool sema_analyse_signature(SemaContext *context, Signature *sig, TypeInfo *method_parent, Decl *decl)
{
	Variadic variadic_type = sig->variadic;
	Decl **params = sig->params;

	unsigned param_count = vec_size(params);
	unsigned vararg_index = sig->vararg_index;
	bool is_macro = sig->is_macro;
	bool is_macro_at_name = sig->is_at_macro || sig->is_safemacro;
	// Check return type
	ASSERT(sig->rtype || sig->is_macro);
	Type *rtype = NULL;
	int format_index = (int)sig->attrs.format - 1;
	if (sig->rtype)
	{
		TypeInfo *rtype_info = type_infoptr(sig->rtype);
		if (!sema_resolve_type_info(context, type_infoptr(sig->rtype),
		                            is_macro ? RESOLVE_TYPE_ALLOW_INFER
		                                     : RESOLVE_TYPE_DEFAULT)) return false;
		rtype = rtype_info->type;
		switch (sema_resolve_storage_type(context, rtype))
		{
			case STORAGE_ERROR:
				return false;
			case STORAGE_VOID:
			case STORAGE_NORMAL:
				break;
			case STORAGE_UNKNOWN:
				RETURN_SEMA_ERROR(rtype_info, "This type is of unknown size, and cannot be a return value. However, you can pass it by pointer.");
			case STORAGE_WILDCARD:
				RETURN_SEMA_ERROR(rtype_info, "The type cannot be determined for this return value.");
			case STORAGE_COMPILE_TIME:
				if (is_macro) break;
				RETURN_SEMA_ERROR(rtype_info, "A function may not return %s, only macros may do so.",  type_invalid_storage_type_name(rtype));
		}
		if (sig->attrs.noreturn && !type_is_void(rtype))
		{
			RETURN_SEMA_ERROR(rtype_info, "@noreturn cannot be used on %s not returning 'void'.", is_macro ? "macros" : "functions");
		}
		if (sig->attrs.nodiscard)
		{
			if (type_is_void(rtype))
			{
				RETURN_SEMA_ERROR(rtype_info, "@nodiscard cannot be used on %s returning 'void'.",
								  is_macro ? "macros" : "functions");
			}
		}
		if (sig->attrs.maydiscard)
		{
			if (!type_is_optional(rtype))
			{
				RETURN_SEMA_ERROR(rtype_info,
				                  "@maydiscard can only be used on %s returning optional values.",
				                  is_macro ? "macros" : "functions");
			}
		}
		if (!sema_deep_resolve_function_ptr(context, rtype_info)) return false;
	}

	// We don't support more than MAX_PARAMS number of params. This makes everything sane.
	if (param_count > MAX_PARAMS)
	{
		if (variadic_type != VARIADIC_NONE)
		{
			RETURN_SEMA_ERROR(params[MAX_PARAMS], "The number of params exceeded the max of %d.", MAX_PARAMS);
		}
		RETURN_SEMA_ERROR(params[MAX_PARAMS], "The number of params exceeded the max of %d. To accept more arguments, "
											  "consider using varargs.", MAX_PARAMS);
	}

	if (method_parent)
	{
		if (!sema_resolve_type_info(context, method_parent,
		                            is_macro ? RESOLVE_TYPE_MACRO_METHOD : RESOLVE_TYPE_FUNC_METHOD)) return false;
	}

	if (params && params[0] && params[0]->var.self_addr)
	{
		if (!method_parent)
		{
			RETURN_SEMA_ERROR(params[0], "Self parameters are only allowed on methods.");
		}
		if (params[0]->var.type_info)
		{
			RETURN_SEMA_ERROR(type_infoptr(params[0]->var.type_info), "A self parameter should always be untyped, please remove the type here.");
		}
	}
	// Fill in the type if the first parameter is lacking a type.
	if (method_parent && params && params[0] && !params[0]->var.type_info)
	{
		Decl *param = params[0];
		Type *inferred_type = NULL;
		switch (param->var.kind)
		{
			case VARDECL_PARAM:
				if (param->var.self_addr)
				{
					inferred_type = type_get_ptr(method_parent->type);
					param->var.not_null = true;
					break;
				}
				FALLTHROUGH;
			case VARDECL_PARAM_EXPR:
			case VARDECL_PARAM_CT:
				inferred_type = method_parent->type;
				break;
			case VARDECL_PARAM_CT_TYPE:
				RETURN_SEMA_ERROR(param, "Expected a parameter of type %s here.", type_quoted_error_string(method_parent->type));
			default:
				UNREACHABLE
		}
		param->var.type_info = type_info_id_new_base(inferred_type, param->span);
		param->var.is_self = true;
	}

	// Ensure it has at least one parameter if method.
	if (method_parent && !vec_size(params))
	{
		RETURN_SEMA_ERROR(decl, "A method must start with an argument of the type "
								"it is a method of, e.g. 'fn void %s.%s(%s* self)'.",
								type_to_error_string(method_parent->type), decl->name, type_to_error_string(method_parent->type));
	}

	if (format_index >= 0)
	{
		if (format_index >= param_count)
		{
			RETURN_SEMA_ERROR(decl, "The format '@format()' index was out of range.");
		}
		if (sig->variadic != VARIADIC_ANY && !is_macro)
		{
			RETURN_SEMA_ERROR(decl, "'@format()' is only valid for a function or macro with 'args...' style vaargs.");
		}
		if (sig->vararg_index == format_index)
		{
			RETURN_SEMA_ERROR(decl, "The format string cannot be a vaarg parameter.");
		}
	}

	// Check parameters
	for (unsigned i = 0; i < param_count; i++)
	{
		Decl *param = params[i];
		if (!param)
		{
			ASSERT(variadic_type == VARIADIC_RAW);
			ASSERT(i == vararg_index);
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
		if (i == 0 && param->resolve_status == RESOLVE_DONE)
		{
			ASSERT(param->type == type_voidptr && "Expected the first parameter of an interface method.");
			continue;
		}

		ASSERT(param->resolve_status == RESOLVE_NOT_DONE && "The param shouldn't have been resolved yet.");
		param->resolve_status = RESOLVE_RUNNING;
		bool erase = false;
		if (!sema_analyse_attributes(context, param, param->attributes, ATTR_PARAM, &erase))
		{
			return decl_poison(param);
		}
		ASSERT(!erase);
		param->unit = context->unit;
		ASSERT(param->decl_kind == DECL_VAR);
		VarDeclKind var_kind = param->var.kind;
		TypeInfo *type_info = type_infoptrzero(param->var.type_info);
		if (type_info)
		{
			if (!sema_resolve_type_info(context, type_info,
			                            is_macro ? RESOLVE_TYPE_ALLOW_INFER
			                                     : RESOLVE_TYPE_DEFAULT)) return decl_poison(param);
			param->type = type_info->type;
			switch (sema_resolve_storage_type(context, type_info->type))
			{
				case STORAGE_ERROR:
					return false;
				case STORAGE_VOID:
					// We may have `void` arguments. They are not allowed in C3. Theoretically they could
					// be permitted, but it would complicate semantics in general.
					if (param_count == 1 && !param->name && param->var.kind == VARDECL_PARAM)
					{
						RETURN_SEMA_ERROR(param, "C-style 'foo(void)' style argument declarations are not valid, please remove 'void'.");
					}
					RETURN_SEMA_ERROR(type_info, "Parameters may not be of type 'void'.");
				case STORAGE_NORMAL:
					break;
				case STORAGE_UNKNOWN:
					RETURN_SEMA_ERROR(type_info, "This type is of unknown size, and cannot be a parameter. However, you can pass it by pointer.");
				case STORAGE_WILDCARD:
					RETURN_SEMA_ERROR(type_info, "The type cannot be determined for this parameter.");
				case STORAGE_COMPILE_TIME:
					RETURN_SEMA_ERROR(type_info, "The type of a parameter may not %s.", type_invalid_storage_type_name(type_info->type));
			}
		}
		if (i == format_index)
		{
			if (!type_info || type_info->type->canonical != type_string)
			{
				SourceSpan span = type_info ? type_info->span : param->span;
				sema_error_at(context, span, "The '@format()' format string must be be of type 'String'.");
				return decl_poison(param);
			}
		}
		if (type_info && param->var.no_alias && !type_is_pointer(param->type) && type_flatten(param->type)->type_kind != TYPE_SLICE)
		{
			SEMA_ERROR(param, "The parameter was set to @noalias, but it was neither a slice nor a pointer. You need to either remove '@noalias' or use pointer/slice type.");
			return decl_poison(param);
		}
		switch (var_kind)
		{
			case VARDECL_PARAM_EXPR:
				if (i == format_index)
				{
					SEMA_ERROR(param, "'@format()' cannot be used with lazy arguments, please remove '@format' or make this a regular parameter.");
					return decl_poison(param);
				}
				if (!is_macro)
				{
					SEMA_ERROR(param, "Only regular parameters are allowed for functions.");
					return decl_poison(param);
				}
				if (!is_macro_at_name)
				{
					SEMA_ERROR(param, "Expression parameters are not allowed in function-like macros. Prefix the macro name with '@'.");
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
				if (!param->type && !is_macro)
				{
					RETURN_SEMA_ERROR(param, "Only typed parameters are allowed for functions.");
				}
				bool erase_decl = false;
				if (!sema_analyse_attributes_for_var(context, param, &erase_decl)) return false;
				ASSERT(!erase_decl);
				break;
			case VARDECL_PARAM_CT_TYPE:
				if (type_info)
				{
					SEMA_ERROR(type_info, "A compile time type parameter cannot have a type itself.");
					return decl_poison(param);
				}
				if (!is_macro)
				{
					SEMA_ERROR(param, "Only regular parameters are allowed for functions.");
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
			if (var_kind != VARDECL_PARAM && var_kind != VARDECL_PARAM_CT)
			{
				SEMA_ERROR(param, "Only regular parameters may be vararg.");
				return decl_poison(param);
			}
			if (!type_info)
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
			type_info->type = type_get_slice(type_info->type);
		}

		if (type_info)
		{
			if (!sema_deep_resolve_function_ptr(context, type_info)) return false;
			param->type = type_info->type;
			if (!is_macro && !sema_set_alignment(context, param->type, &param->alignment, true)) return false;
		}

		if (param->var.init_expr)
		{
			Expr *expr = param->var.init_expr;
			if (expr_is_const(expr))
			{
				if (!sema_analyse_expr_rhs(context, param->type, expr, true, NULL, false)) return decl_poison(param);
			}
		}
		if (!sema_check_param_uniqueness_and_type(context, params, param, i)) return decl_poison(param);
		param->resolve_status = RESOLVE_DONE;
	}
	return true;
}

bool sema_analyse_function_signature(SemaContext *context, Decl *func_decl, TypeInfo *parent, CallABI abi, Signature *signature)
{
	// Get param count and variadic type
	Decl **params = signature->params;

	if (!sema_analyse_signature(context, signature, parent, func_decl)) return false;

	Variadic variadic_type = signature->variadic;

	// Remove the last empty value.
	if (variadic_type == VARIADIC_RAW)
	{
		ASSERT(params && !params[signature->vararg_index] && "The last parameter must have been a raw variadic.");
		ASSERT(signature->vararg_index == vec_size(params) - 1);
		vec_pop(params);
	}

	Type **types = NULL;
	unsigned param_count = vec_size(params);

	for (unsigned i = 0; i < param_count; i++)
	{
		ASSERT(IS_RESOLVED(params[i]));
		ASSERT(params[i]->type->canonical);
		vec_add(types, params[i]->type);
	}

	Type *raw_type = sema_resolve_type_get_func(signature, abi);
	ASSERT(func_decl->type->type_kind == TYPE_FUNC_RAW);
	ASSERT(raw_type->function.prototype);
	func_decl->type->function.prototype = raw_type->function.prototype;
	return true;
}

static inline bool sema_analyse_fntype(SemaContext *context, Decl *decl, bool *erase_decl)
{
	if (!sema_analyse_attributes(context, decl, decl->attributes, ATTR_FNTYPE, erase_decl)) return decl_poison(decl);
	if (decl_is_deprecated(decl)) context->call_env.ignore_deprecation = true;

	if (*erase_decl) return true;
	Signature *sig = &decl->fntype_decl.signature;
	if (!sema_analyse_function_signature(context, decl, NULL, sig->abi, sig)) return false;
	bool pure = false;
	if (!sema_analyse_doc_header(context, decl->fntype_decl.docs, sig->params, NULL, &pure, sig->variadic == VARIADIC_RAW)) return false;
	sig->attrs.is_pure = pure;
	return true;
}

static inline bool sema_analyse_type_alias(SemaContext *context, Decl *decl, bool *erase_decl)
{
	if (!sema_analyse_attributes(context, decl, decl->attributes, ATTR_ALIAS, erase_decl)) return decl_poison(decl);
	if (decl_is_deprecated(decl)) context->call_env.ignore_deprecation = true;

	if (*erase_decl) return true;

	bool is_export = decl->is_export;
	if (decl->type_alias_decl.is_func)
	{
		Decl *fn_decl = decl->type_alias_decl.decl;
		fn_decl->is_export = is_export;
		fn_decl->unit = decl->unit;
		fn_decl->type = type_new_func(fn_decl, &fn_decl->fntype_decl.signature);
		decl->type->canonical = type_get_func_ptr(fn_decl->type);
		return true;
	}
	TypeInfo *info = decl->type_alias_decl.type_info;
	info->in_def = true;
	if (!sema_resolve_type_info(context, info, RESOLVE_TYPE_DEFAULT)) return false;
	if (type_is_optional(info->type))
	{
		RETURN_SEMA_ERROR(info, "You cannot create an alias for an optional type like %s.", type_quoted_error_string(info->type));
	}
	switch (sema_resolve_storage_type(context, info->type))
	{
		case STORAGE_ERROR:
			return false;
		case STORAGE_NORMAL:
		case STORAGE_UNKNOWN:
		case STORAGE_VOID:
			break;
		case STORAGE_WILDCARD:
			RETURN_SEMA_ERROR(info, "You cannot create an alias for the wildcard type.");
		case STORAGE_COMPILE_TIME:
			RETURN_SEMA_ERROR(info, "You cannot create an alias for %s as it is a compile time type.",
							  type_invalid_storage_type_name(info->type));
	}
	decl->type->canonical = info->type->canonical;
	// Do we need anything else?
	return true;
}

/**
 * Analyse a distinct type.
 */
static inline bool sema_analyse_typedef(SemaContext *context, Decl *decl, bool *erase_decl)
{
	// Check the attributes on the distinct type.
	if (!sema_analyse_attributes(context, decl, decl->attributes, ATTR_TYPEDEF, erase_decl)) return false;
	if (decl_is_deprecated(decl)) context->call_env.ignore_deprecation = true;

	// Erase it?
	if (*erase_decl) return true;

	// Check the interfaces.
	if (!sema_resolve_implemented_interfaces(context, decl, false)) return false;

	// Infer the underlying type normally.
	TypeInfo *info = decl->distinct;
	info->in_def = true;
	if (!sema_resolve_type_info(context, info, RESOLVE_TYPE_DEFAULT)) return false;
	if (!sema_resolve_type_decl(context, info->type)) return false;
	Type *inner_type = info->type;
	// Optional isn't allowed of course.
	if (type_is_optional(inner_type)) RETURN_SEMA_ERROR(decl, "You cannot create a distinct type from an optional.");
	switch (sema_resolve_storage_type(context, inner_type))
	{
		case STORAGE_ERROR:
			return false;
		case STORAGE_NORMAL:
		case STORAGE_UNKNOWN:
		case STORAGE_VOID:
			break;
		case STORAGE_WILDCARD:
			RETURN_SEMA_ERROR(info, "You cannot create a distinct type from the wildcard type.");
		case STORAGE_COMPILE_TIME:
			RETURN_SEMA_ERROR(info, "You cannot create a distinct type for %s as it is a compile time type.",
							  type_invalid_storage_type_name(inner_type));
	}

	if (decl->distinct_align)
	{
		if (!sema_resolve_align_expr(context, decl->distinct_align, &decl->alignment)) return false;
		AlignSize default_size = type_abi_alignment(inner_type);
		// Remove "alignment"
		if (default_size == decl->alignment) decl->distinct_align = NULL;
	}
	if (!decl->alignment)
	{
		decl->alignment = type_abi_alignment(inner_type);
	}
	// Distinct types drop the canonical part.
	info->type = info->type->canonical;
	return true;
}

static inline bool sema_analyse_enum_param(SemaContext *context, Decl *param)
{
	ASSERT(param->decl_kind == DECL_VAR && param->var.kind == VARDECL_PARAM && param->var.type_info);
	if (vec_size(param->attributes))
	{
		RETURN_SEMA_ERROR(param->attributes[0], "There are no valid attributes for associated values.");
	}
	TypeInfo *type_info = type_infoptrzero(param->var.type_info);
	if (!sema_resolve_type_info(context, type_info, RESOLVE_TYPE_DEFAULT)) return false;
	ASSERT(!param->var.vararg);
	param->type = type_info->type;
	ASSERT(param->name);
	if (param->name == kw_nameof || param->name == kw_ordinal)
	{
		RETURN_SEMA_ERROR(param, "'%s' is not a valid parameter name for enums.", param->name);
	}
	Decl *other = sema_decl_stack_resolve_symbol(param->name);
	if (other)
	{
		RETURN_SEMA_ERROR(param, "Duplicate parameter name '%s'.", param->name);
	}
	sema_decl_stack_push(param);
	ASSERT(!param->var.init_expr);
	return true;
}

static inline void sema_print_enum_to_cenum_error(SemaContext *context, Decl *decl, Expr *arg)
{
	TypeInfo *type_info = decl->enums.type_info;
	if (type_info->type == type_int)
	{
		SEMA_ERROR(arg, "Assigning a value requires the declaration of associated values for the enum. Did you perhaps want C-style enums? In that case use constdef, defined like 'constdef %s : { ... }'", decl->name);
	}
	else
	{
		SEMA_ERROR(arg, "Assigning a value requires the declaration of associated values for the enum. Did you perhaps want C-style enums? In that case use constdef, defined like 'constdef %s : %s { ... }'", decl->name, type_to_error_string(type_info->type));
	}
}
static inline bool sema_analyse_enum(SemaContext *context, Decl *decl, bool *erase_decl)
{
	if (!sema_analyse_attributes(context, decl, decl->attributes, ATTR_ENUM, erase_decl)) return decl_poison(decl);
	if (decl_is_deprecated(decl)) context->call_env.ignore_deprecation = true;

	if (*erase_decl) return true;
	if (!sema_resolve_implemented_interfaces(context, decl, false)) return decl_poison(decl);

	// Resolve the type of the enum.
	if (!sema_resolve_type_info(context, decl->enums.type_info, RESOLVE_TYPE_DEFAULT)) return false;

	if (decl->enums.inline_index > -1 || decl->enums.inline_value) decl->is_substruct = true;
	Type *type = decl->enums.type_info->type;
	ASSERT(!type_is_optional(type) && "Already stopped when parsing.");

	Type *flat_underlying_type = type_flatten(type);
	// Require an integer type
	if (!type_is_integer(flat_underlying_type))
	{
		RETURN_SEMA_ERROR(decl->enums.type_info, "The enum type must be an integer type not '%s'.", type_to_error_string(type));
	}

	DEBUG_LOG("* Enum type resolved to %s.", type->name);

	Decl **associated_values = decl->enums.parameters;
	unsigned associated_value_count = vec_size(associated_values);
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
		if (!sema_analyse_enum_param(context, value)) goto ERR;
	}
	sema_decl_stack_restore(state);

	bool success = true;
	unsigned enums = vec_size(decl->enums.values);
	Int128 value = { 0, 0 };

	Decl **enum_values = decl->enums.values;

	for (unsigned i = 0; i < enums; i++)
	{

		Decl *enum_value = enum_values[i];

		bool erase_val = false;
		if (!sema_analyse_attributes(context, enum_value, enum_value->attributes, ATTR_ENUM_VALUE, &erase_val)) return decl_poison(decl);

		if (erase_val)
		{
			if (enums == 1)
			{
				RETURN_SEMA_ERROR(decl, "No enum values left in enum after @if resolution, there must be at least one.");
			}
			vec_erase_at(enum_values, i);
			enums--;
			i--;
			continue;
		}
		enum_value->type = decl->type;
		DEBUG_LOG("* Checking enum constant %s.", enum_value->name);
		enum_value->enum_constant.inner_ordinal = i;
		DEBUG_LOG("* Ordinal: %d", i);
		ASSERT(enum_value->resolve_status == RESOLVE_NOT_DONE);
		ASSERT(enum_value->decl_kind == DECL_ENUM_CONSTANT);

		// Start evaluating the constant
		enum_value->resolve_status = RESOLVE_RUNNING;

		// Create a "fake" expression.
		// This will be evaluated later to catch the case

		Int val = (Int){ value, flat_underlying_type->type_kind };
		if (!int_fits(val, flat_underlying_type->type_kind))
		{
			RETURN_SEMA_ERROR(enum_value,
			                  "The enum value would implicitly be %s which does not fit in %s.",
			                  i128_to_string(value, 10, type_is_signed(flat_underlying_type), false),
			                  type_quoted_error_string(type));
		}
		enum_value->enum_constant.inner_ordinal = value.low;

		// Update the value
		value.low++;

		Expr **args = enum_value->enum_constant.associated;
		unsigned arg_count = vec_size(args);
		if (arg_count > associated_value_count)
		{
			if (!associated_value_count)
			{
				sema_print_enum_to_cenum_error(context, decl, args[0]);
				return false;
			}
			RETURN_SEMA_ERROR(args[associated_value_count], "You're adding too many values, only %d associated value%s are defined for '%s'.", associated_value_count, associated_value_count != 1 ? "s" : "", decl->name);
		}
		if (arg_count < associated_value_count)
		{
			RETURN_SEMA_ERROR(enum_value, "Expected %d associated value%s for this enum value.", associated_value_count, associated_value_count != 1 ? "s" : "");
		}
	}
	decl->resolve_status = RESOLVE_DONE;
	for (unsigned i = 0; i < associated_value_count; i++)
	{
		Decl *param = associated_values[i];
		if (!sema_set_alignment(context, param->type, &param->alignment, true)) return false;
		param->resolve_status = RESOLVE_DONE;
	}
	for (unsigned i = 0; i < enums; i++)
	{
		Decl *enum_value = enum_values[i];
		Expr **args = enum_value->enum_constant.associated;
		unsigned arg_count = vec_size(args);
		if (arg_count > associated_value_count)
		{
			if (!associated_value_count)
			{
				sema_print_enum_to_cenum_error(context, decl, args[0]);
				return false;
			}
			RETURN_SEMA_ERROR(args[associated_value_count], "You're adding too many values, only %d associated value%s are defined for '%s'.", associated_value_count, associated_value_count != 1 ? "s" : "", decl->name);
		}
		if (arg_count < associated_value_count)
		{
			RETURN_SEMA_ERROR(enum_value, "Expected %d associated value%s for this enum value.", associated_value_count, associated_value_count != 1 ? "s" : "");
		}
		for (unsigned j = 0; j < arg_count; j++)
		{
			Expr *arg = args[j];

			if (!sema_analyse_expr_rhs(context, associated_values[j]->type, arg, false, NULL, false)) return false;
			if (!expr_is_runtime_const(arg))
			{
				RETURN_SEMA_ERROR(arg, "Associated values must be constant expressions.");
			}
		}
		enum_value->resolve_status = RESOLVE_DONE;
	}
	return success;
ERR:
	sema_decl_stack_restore(state);
	return false;
}

static bool sema_analyse_const_enum_constant_val(SemaContext *context, Decl *decl, Type *underlying_type)
{
	Expr *value = decl->enum_constant.value;
	if (!sema_analyse_expr_rhs(context, decl->type, value, false, NULL, true)) return decl_poison(decl);
	if (!expr_is_runtime_const(value))
	{
		SEMA_ERROR(value, "Expected an constant enum value.");
		return decl_poison(decl);
	}
	if (value->type != decl->type)
	{
		SEMA_ERROR(value, "Expected an constant enum value of type %s, was %s", type_quoted_error_string(decl->type), type_quoted_error_string(value->type));
		return decl_poison(decl);
	}
	return true;
}
static inline bool sema_analyse_constdef(SemaContext *context, Decl *decl, bool *erase_decl)
{
	if (!sema_analyse_attributes(context, decl, decl->attributes, ATTR_ENUM, erase_decl)) return decl_poison(decl);
	if (decl_is_deprecated(decl)) context->call_env.ignore_deprecation = true;

	if (*erase_decl) return true;
	if (!sema_resolve_implemented_interfaces(context, decl, false)) return decl_poison(decl);

	// Resolve the type of the enum.
	if (!sema_resolve_type_info(context, decl->enums.type_info, RESOLVE_TYPE_DEFAULT)) return false;

	Type *type = decl->enums.type_info->type;
	if (!sema_resolve_type_decl(context, type)) return false;
	Type *flat = type_flatten(type);
	ASSERT_SPAN(decl, !type_is_optional(type) && "Already stopped when parsing.");

	switch (sema_resolve_storage_type(context, type))
	{
		case STORAGE_ERROR:
			return decl_poison(decl);
		case STORAGE_NORMAL:
			break;
		case STORAGE_WILDCARD:
			SEMA_ERROR(decl->enums.type_info, "No type can be inferred from the optional result.");
			return decl_poison(decl);
		case STORAGE_VOID:
			SEMA_ERROR(decl->enums.type_info, "A constdef may not have a void type.");
			return decl_poison(decl);
		case STORAGE_COMPILE_TIME:
			SEMA_ERROR(decl->enums.type_info, "A constdef may not be %s.", type_invalid_storage_type_name(type));
			return decl_poison(decl);
		case STORAGE_UNKNOWN:
			SEMA_ERROR(decl->enums.type_info, "A constdef may not be %s, as it has an unknown size.",
				type_quoted_error_string(type));
			return decl_poison(decl);
	}

	DEBUG_LOG("* Constdef type resolved to %s.", type->name);

	ASSERT_SPAN(decl, !decl->enums.parameters);

	bool success = true;
	unsigned enums = vec_size(decl->enums.values);

	Decl **enum_values = decl->enums.values;
	for (unsigned i = 0; i < enums; i++)
	{
		Decl *enum_value = enum_values[i];

		bool erase_val = false;
		if (!sema_analyse_attributes(context, enum_value, enum_value->attributes, ATTR_ENUM_VALUE, &erase_val)) return decl_poison(decl);
		if (erase_val)
		{
			if (enums == 1)
			{
				RETURN_SEMA_ERROR(decl, "No constdef values left in constdef after @if resolution, there must be at least one.");
			}
			vec_erase_at(enum_values, i);
			enums--;
			i--;
			continue;
		}
		enum_value->type = decl->type;
		DEBUG_LOG("* Checking constdef constant %s.", enum_value->name);
		if (!enum_value->enum_constant.value)
		{
			if (!type_is_integer(flat))
			{
				RETURN_SEMA_ERROR(enum_value, "Constdefs with missing values must be of integer type.");
			}
			if (i == 0)
			{
				enum_value->enum_constant.value = expr_new_const_int(enum_value->span, type, 0);
			}
			else
			{
				Expr *expr = expr_new(EXPR_IOTA_DECL, enum_value->span);
				expr->decl_expr = enum_values[i - 1];
				enum_value->enum_constant.value = expr;
			}
		}
		ASSERT(enum_value->resolve_status == RESOLVE_NOT_DONE);
		ASSERT(enum_value->decl_kind == DECL_ENUM_CONSTANT);
	}
	decl->resolve_status = RESOLVE_DONE;
	for (unsigned i = 0; i < enums; i++)
	{
		Decl *enum_value = enum_values[i];
		enum_value->resolve_status = RESOLVE_RUNNING;
		if (!sema_analyse_const_enum_constant_val(context, enum_value, type)) return decl_poison(decl);
		enum_value->resolve_status = RESOLVE_DONE;
	}
	return success;
}

static inline bool sema_analyse_fault(SemaContext *context, Decl *decl, bool *erase_decl)
{
	if (!sema_analyse_attributes(context, decl, decl->attributes, ATTR_FAULT, erase_decl)) return decl_poison(decl);
	if (*erase_decl) return true;
	if (decl_is_deprecated(decl)) context->call_env.ignore_deprecation = true;

	decl->type = type_fault;
	decl->alignment = type_abi_alignment(type_string);
	return true;
}

static inline const char *method_name_by_decl(Decl *method_like)
{
	switch (method_like->decl_kind)
	{
		case DECL_MACRO:
			return "macro method";
		case DECL_FUNC:
			return "method";
		default:
			UNREACHABLE
	}
}


static bool sema_analyse_operator_common(SemaContext *context, Decl *method, TypeInfo **rtype_ptr, Decl ***params_ptr,
                                         uint32_t parameters)
{
	Signature *signature = &method->func_decl.signature;
	Decl **params = *params_ptr = signature->params;
	uint32_t param_count = vec_size(params);
	if (param_count > parameters)
	{
		RETURN_SEMA_ERROR(params[parameters], "Too many parameters, '%s' expects only %u.", method->name, (unsigned)parameters);
	}
	if (param_count < parameters)
	{
		RETURN_SEMA_ERROR(method, "Not enough parameters, '%s' requires %u.", method->name, (unsigned)parameters);
	}

	if (!signature->rtype) RETURN_SEMA_ERROR(method, "The return value must be explicitly typed for '%s'.", method->name);

	FOREACH(Decl *, param, params)
	{
		if (!param->var.type_info)
		{
			RETURN_SEMA_ERROR(param, "All parameters must be explicitly typed for '%s'.", method->name);
		}
	}
	*rtype_ptr = type_infoptr(signature->rtype);
	return true;
}


Decl *sema_find_untyped_operator(Type *type, OperatorOverload operator_overload, Decl *skipped)
{
	type = type_no_optional(type)->canonical;
	assert(operator_overload < OVERLOAD_TYPED_START);
	if (!type_may_have_sub_elements(type)) return NULL;
	Decl *def = type->decl;
	if (!def->method_table) return NULL;
	Decl *decl = declptrzero(def->method_table->overloads[operator_overload]);
	if (!decl) return NULL;
	if (decl->decl_kind != DECL_DECLARRAY)
	{
		return decl == skipped ? NULL : decl;
	}
	FOREACH(Decl *, candidate, decl->decls)
	{
		return candidate == skipped ? NULL : candidate;
	}
	return NULL;
}



OverloadMatch sema_find_typed_operator_type(SemaContext *context, OperatorOverload operator_overload, OverloadType overload_type, Type *lhs_type, Type *rhs_type, Expr *rhs, Decl **candidate_ref, OverloadMatch last_match, Decl **ambiguous_ref)
{
	Methods *methods = lhs_type->decl->method_table;
	if (!methods || last_match == OVERLOAD_MATCH_AMBIGUOUS_EXACT || last_match == OVERLOAD_MATCH_ERROR) return last_match;
	Decl *decl = declptrzero(methods->overloads[operator_overload]);
	if (!decl) return last_match;
	Decl **decls;
	unsigned count;
	Decl *candidate = *candidate_ref;
	if (decl->decl_kind == DECL_DECLARRAY)
	{
		decls = decl->decls;
		count = vec_size(decls);
	}
	else
	{
		decls = &decl;
		count = 1;
	}
	for (unsigned i = 0; i < count; i++)
	{
		Decl *func = decls[i];
		if (!sema_analyse_decl(context, func)) return OVERLOAD_MATCH_ERROR;
		ASSERT_SPAN(func, func->resolve_status == RESOLVE_DONE);
		if (candidate == func) continue;
		if ((overload_type & func->func_decl.overload_type) == 0) continue;
		OverloadMatch match = OVERLOAD_MATCH_WILDCARD;
		if (!func->func_decl.is_wildcard_overload)
		{
			Type *first_arg = func->func_decl.signature.params[1]->type->canonical;
			match = OVERLOAD_MATCH_EXACT;
			if (first_arg != rhs_type)
			{
				if (!rhs) continue;
				if (!may_cast(context, rhs, first_arg, false, true)) continue;
				match = OVERLOAD_MATCH_CONVERSION;
			}
		}
		if (last_match >= OVERLOAD_MATCH_AMBIGUOUS_START)
		{
			assert(last_match != OVERLOAD_MATCH_AMBIGUOUS_EXACT);
			if (match == OVERLOAD_MATCH_EXACT)
			{
				*ambiguous_ref = candidate;
				goto MATCH;
			}
			if (match == OVERLOAD_MATCH_CONVERSION && last_match == OVERLOAD_MATCH_AMBIGUOUS_WILDCARD)
			{
				*ambiguous_ref = candidate;
				goto MATCH;
			}
			continue;
		}
		if (candidate)
		{
			if (last_match > match) goto MATCH;
			if (last_match < match) continue;
			*ambiguous_ref = func;
			*candidate_ref = candidate;
			switch (match)
			{
				case OVERLOAD_MATCH_EXACT:
					return OVERLOAD_MATCH_AMBIGUOUS_EXACT;
				case OVERLOAD_MATCH_CONVERSION:
					last_match = OVERLOAD_MATCH_AMBIGUOUS_CONVERSION;
					continue;
				case OVERLOAD_MATCH_WILDCARD:
					last_match = OVERLOAD_MATCH_AMBIGUOUS_WILDCARD;
					continue;
				default:
					UNREACHABLE;
			}
			UNREACHABLE;
		}
		MATCH:
		candidate = func;
		last_match = match;
	}
	*candidate_ref = candidate;
	return last_match;
}

const char *operator_overload_to_string(OperatorOverload operator_overload)
{
	switch (operator_overload)
	{
		case OVERLOAD_ELEMENT_AT:
		case OVERLOAD_ELEMENT_REF:
		case OVERLOAD_ELEMENT_SET:
		case OVERLOAD_LEN:				UNREACHABLE
		case OVERLOAD_NEGATE:			return "~";
		case OVERLOAD_UNARY_MINUS:		return "-";
		case OVERLOAD_PLUS:				return "+";
		case OVERLOAD_MINUS:			return "-";
		case OVERLOAD_MULTIPLY:			return "*";
		case OVERLOAD_DIVIDE:			return "/";
		case OVERLOAD_REMINDER:			return "%";
		case OVERLOAD_AND:				return "&";
		case OVERLOAD_OR:				return "|";
		case OVERLOAD_XOR:				return "^";
		case OVERLOAD_SHL:				return "<<";
		case OVERLOAD_SHR:				return ">>";
		case OVERLOAD_EQUAL:			return "==";
		case OVERLOAD_NOT_EQUAL:		return "!=";
		case OVERLOAD_PLUS_ASSIGN:		return "+=";
		case OVERLOAD_MINUS_ASSIGN:		return "-=";
		case OVERLOAD_MULTIPLY_ASSIGN:	return "*=";
		case OVERLOAD_DIVIDE_ASSIGN:	return "/=";
		case OVERLOAD_REMINDER_ASSIGN:	return "%=";
		case OVERLOAD_AND_ASSIGN:		return "&=";
		case OVERLOAD_OR_ASSIGN:		return "|=";
		case OVERLOAD_XOR_ASSIGN:		return "^=";
		case OVERLOAD_SHL_ASSIGN:		return "<<=";
		case OVERLOAD_SHR_ASSIGN:		return ">>=";
	}
	UNREACHABLE
}

Decl *sema_find_typed_operator(SemaContext *context, OperatorOverload operator_overload, SourceSpan span, Expr *lhs, Expr *rhs, bool *reverse)
{
	assert(operator_overload >= OVERLOAD_TYPED_START);
	assert(lhs && rhs);
	Type *left_type = type_no_optional(lhs->type)->canonical;
	Type *right_type = type_no_optional(rhs->type)->canonical;
	Decl *candidate = NULL;
	Decl *ambiguous = NULL;
	OverloadMatch current_match = OVERLOAD_MATCH_NONE;
	*reverse = false;
	Decl *first = NULL;
	if (type_is_user_defined(left_type))
	{
		current_match = sema_find_typed_operator_type(context, operator_overload, OVERLOAD_TYPE_LEFT, left_type, right_type, rhs, &candidate, current_match, &ambiguous);
		first = candidate;
	}
	if (type_is_user_defined(right_type))
	{
		current_match = sema_find_typed_operator_type(context, operator_overload, OVERLOAD_TYPE_RIGHT, right_type, left_type, lhs, &candidate, current_match, &ambiguous);
	}

	if (current_match >= OVERLOAD_MATCH_AMBIGUOUS_START)
	{
		sema_error_at(context, span, "%s %s %s has more than one candidate method, and is ambiguous. One solution is to call the desired method directly, instead of using operators, to select the right one.",
		              type_quoted_error_string(lhs->type),
		              operator_overload_to_string(operator_overload),
		              type_quoted_error_string(rhs->type));
		SEMA_NOTE(candidate, "One candidate was here.");
		SEMA_NOTE(ambiguous, "Another candidate was found here.");
		return poisoned_decl;
	}

	if (current_match == OVERLOAD_MATCH_ERROR) return poisoned_decl;
	if (candidate != first) *reverse = true;

	return candidate;
}

static inline bool sema_analyse_operator_unary(SemaContext *context, Decl *method, OperatorOverload operator_overload)
{
	assert(operator_overload == OVERLOAD_NEGATE || operator_overload == OVERLOAD_UNARY_MINUS);
	TypeInfo *rtype;
	Decl **params;
	if (!sema_analyse_operator_common(context, method, &rtype, &params, 1)) return false;
	if (!rtype) RETURN_SEMA_ERROR(method, "The return value must be explicitly typed for '%s'.", method->name);
	if (rtype->type->canonical != typeget(method->func_decl.type_parent)->canonical)
	{
		RETURN_SEMA_ERROR(rtype, "The return value must be %s but was %s.", type_quoted_error_string(typeget(method->func_decl.type_parent)),
			type_quoted_error_string(rtype->type));
	}
	return true;
}

static inline bool sema_analyse_operator_arithmetics(SemaContext *context, Decl *method, OperatorOverload operator_overload)
{
	Signature *signature = &method->func_decl.signature;
	Decl **params = signature->params;
	uint32_t param_count = vec_size(params);
	if (param_count > 2)
	{
		RETURN_SEMA_ERROR(params[2], "Too many parameters, '%s' expects only 2 parameters.", method->name);
	}
	if (param_count < 2)
	{
		RETURN_SEMA_ERROR(method, "Not enough parameters, '%s' requires 2 parameters.", method->name);
	}
	if (!signature->rtype) RETURN_SEMA_ERROR(method, "The return value must be explicitly typed for '%s'.", method->name);
	TypeInfo *rtype = type_infoptr(signature->rtype);
	if (IS_OPTIONAL(rtype))
	{
		RETURN_SEMA_ERROR(rtype, "The return type may not be an optional.");
	}
	if (operator_overload == OVERLOAD_EQUAL || operator_overload == OVERLOAD_NOT_EQUAL)
	{
		if (rtype->type->canonical != type_bool)
		{
			RETURN_SEMA_ERROR(rtype, "The return type was %s, but it must be bool for comparisons.", type_quoted_error_string(rtype->type));
		}
	}
	if (operator_overload < OVERLOAD_PLUS_ASSIGN && type_is_void(rtype->type))
	{
		RETURN_SEMA_ERROR(rtype, "The return type may not be %s.", type_quoted_error_string(rtype->type));
	}
	// Set it as pure
	method->func_decl.signature.attrs.is_pure = true;
	return true;
}

static inline bool sema_analyse_operator_element_at(SemaContext *context, Decl *method)
{
	TypeInfo *rtype;
	Decl **params;
	if (!sema_analyse_operator_common(context, method, &rtype, &params, 2)) return false;
	switch (sema_resolve_storage_type(context, rtype->type))
	{
		case STORAGE_ERROR:
			return false;
		case STORAGE_NORMAL:
			break;
		case STORAGE_VOID:
		case STORAGE_WILDCARD:
			RETURN_SEMA_ERROR(rtype, "The return type cannot be 'void'.");
		case STORAGE_COMPILE_TIME:
			RETURN_SEMA_ERROR(rtype, "The return type is %s which is a compile time type, which isn't allowed.",
			                  type_invalid_storage_type_name(rtype->type));
		case STORAGE_UNKNOWN:
			RETURN_SEMA_ERROR(rtype, "%s has unknown size and cannot be used as a return type.",
			                  type_quoted_error_string(rtype->type));
	}
	if (method->func_decl.operator == OVERLOAD_ELEMENT_REF && !type_is_pointer(type_no_optional(rtype->type)))
	{
		RETURN_SEMA_ERROR(rtype, "The return type must be a pointer, but it is returning %s, did you mean to overload [] instead?",
		                  type_quoted_error_string(rtype->type));
	}
	return true;
}

static inline bool sema_analyse_operator_element_set(SemaContext *context, Decl *method)
{
	TypeInfo *rtype;
	Decl **params;
	return sema_analyse_operator_common(context, method, &rtype, &params, 3);
}

static inline bool sema_analyse_operator_len(SemaContext *context, Decl *method)
{
	TypeInfo *rtype;
	Decl **params;
	if (!sema_analyse_operator_common(context, method, &rtype, &params, 1)) return false;
	if (!type_is_integer(rtype->type))
	{
		RETURN_SEMA_ERROR(rtype, "The return type must be an integer type.");
	}
	return true;
}

static bool sema_check_operator_method_validity(SemaContext *context, Decl *method)
{
	OperatorOverload operator = method->func_decl.operator;
	switch (operator)
	{
		case OVERLOAD_ELEMENT_SET:
			return sema_analyse_operator_element_set(context, method);
		case OVERLOAD_ELEMENT_AT:
		case OVERLOAD_ELEMENT_REF:
			return sema_analyse_operator_element_at(context, method);
		case OVERLOAD_LEN:
			return sema_analyse_operator_len(context, method);
		case OVERLOAD_PLUS:
		case OVERLOAD_MULTIPLY:
		case OVERLOAD_MINUS:
		case OVERLOAD_DIVIDE:
		case OVERLOAD_REMINDER:
		case OVERLOAD_EQUAL:
		case OVERLOAD_NOT_EQUAL:
		case OVERLOAD_AND:
		case OVERLOAD_OR:
		case OVERLOAD_XOR:
		case OVERLOAD_SHL:
		case OVERLOAD_SHR:
		case OVERLOAD_PLUS_ASSIGN:
		case OVERLOAD_MINUS_ASSIGN:
		case OVERLOAD_MULTIPLY_ASSIGN:
		case OVERLOAD_DIVIDE_ASSIGN:
		case OVERLOAD_REMINDER_ASSIGN:
		case OVERLOAD_AND_ASSIGN:
		case OVERLOAD_OR_ASSIGN:
		case OVERLOAD_XOR_ASSIGN:
		case OVERLOAD_SHR_ASSIGN:
		case OVERLOAD_SHL_ASSIGN:
			return sema_analyse_operator_arithmetics(context, method, operator);
		case OVERLOAD_NEGATE:
		case OVERLOAD_UNARY_MINUS:
			return sema_analyse_operator_unary(context, method, operator);
			UNREACHABLE
	}
	ASSERT_SPANF(method, false, "Method had unexpected operator %d", operator);
	UNREACHABLE
}

bool sema_decl_if_cond(SemaContext *context, Decl *decl)
{
	Attr *attr = attr_find_kind(decl->attributes, ATTRIBUTE_IF);
	decl->is_if = true;
	ASSERT(attr);
	if (vec_size(attr->exprs) != 1)
	{
		RETURN_SEMA_ERROR(attr, "Expected an argument to '@if'.");
	}
	Expr *expr = attr->exprs[0];
	context->call_env.in_if_resolution = attr->span;
	bool success = sema_analyse_ct_expr(context, expr);
	context->call_env.in_if_resolution.a = 0;
	if (!success) return false;
	if (expr->type->canonical != type_bool)
	{
		RETURN_SEMA_ERROR(expr, "Expected a boolean value not %s.", type_quoted_error_string(expr->type));
	}
	if (expr->const_expr.b) return true;
	decl->decl_kind = DECL_ERASED;
	context->call_env.in_if_resolution.a = 0;
	return false;
}

INLINE SourceSpan method_find_overload_span(Decl *method)
{
	ASSERT(method->resolved_attributes && method->attrs_resolved);
	return method->attrs_resolved->overload;
}

static inline bool unit_add_base_extension_method(SemaContext *context, Type *type, Decl *method)
{
	Decl *other = declptrzero(methodtable_set(&compiler.context.method_extensions, method));
	if (other)
	{
		SEMA_ERROR(method, "This %s is already defined.", method_name_by_decl(method));
		SEMA_NOTE(other, "The previous definition was here.");
		return false;
	}
	FOREACH(Type *, t, compiler.context.types_with_failed_methods)
	{
		if (t == type)
		{
			RETURN_SEMA_ERROR(method, "A method was added to %s which already was checked for method availability, declarations must be reordered.", type_quoted_error_string(type));
		}
	}
	vec_add(compiler.context.method_extension_list, method);
	DEBUG_LOG("Builtin type method '%s' analysed.", method->name);
	return true;
}

static inline void sema_get_overload_arguments(Decl *method, Type **value_ref, Type **index_ref)
{
	switch (method->func_decl.operator)
	{
		case OVERLOAD_LEN:
			UNREACHABLE_VOID
		case OVERLOAD_ELEMENT_AT:
			*value_ref = type_no_optional(typeget(method->func_decl.signature.rtype)->canonical);
			*index_ref = method->func_decl.signature.params[1]->type->canonical;
			return;
		case OVERLOAD_ELEMENT_REF:
			*value_ref = type_no_optional(typeget(method->func_decl.signature.rtype))->canonical->pointer;
			*index_ref = method->func_decl.signature.params[1]->type->canonical;
			return;
		case OVERLOAD_ELEMENT_SET:
			*value_ref = method->func_decl.signature.params[2]->type->canonical;
			*index_ref = method->func_decl.signature.params[1]->type->canonical;
			return;
		default:
			UNREACHABLE_VOID
	}
}

static const char *OVERLOAD_NAME[OVERLOADS_COUNT + 1] =
{
	[OVERLOAD_ELEMENT_AT] = "[]",
	[OVERLOAD_ELEMENT_REF] = "&[]",
	[OVERLOAD_ELEMENT_SET] = "[]=",
	[OVERLOAD_LEN] = ".len",
	[OVERLOAD_NEGATE] = "~",
	[OVERLOAD_UNARY_MINUS] = "-",
	[OVERLOAD_PLUS] = "+",
	[OVERLOAD_MINUS] = "-",
	[OVERLOAD_MULTIPLY] = "*",
	[OVERLOAD_DIVIDE] = "/",
	[OVERLOAD_REMINDER] = "%",
	[OVERLOAD_AND] = "&",
	[OVERLOAD_OR] = "|",
	[OVERLOAD_XOR] = "^",
	[OVERLOAD_SHL] = "<<",
	[OVERLOAD_SHR] = ">>",
	[OVERLOAD_EQUAL] = "==",
	[OVERLOAD_NOT_EQUAL] = "!=",
	[OVERLOAD_PLUS_ASSIGN] = "+=",
	[OVERLOAD_MINUS_ASSIGN] = "-=",
	[OVERLOAD_MULTIPLY_ASSIGN] = "*=",
	[OVERLOAD_DIVIDE_ASSIGN] = "/=",
	[OVERLOAD_REMINDER_ASSIGN] = "%=",
	[OVERLOAD_AND_ASSIGN] = "&=",
	[OVERLOAD_OR_ASSIGN] = "|=",
	[OVERLOAD_XOR_ASSIGN] = "^=",
	[OVERLOAD_SHL_ASSIGN] = "<<=",
	[OVERLOAD_SHR_ASSIGN] = ">>=",
};

static bool OVERLOAD_MAY_BE_REVERSE[OVERLOADS_COUNT + 1] =
{
	[OVERLOAD_PLUS] = true,
	[OVERLOAD_MINUS] = true,
	[OVERLOAD_MULTIPLY] = true,
	[OVERLOAD_DIVIDE] = true,
	[OVERLOAD_REMINDER] = true,
	[OVERLOAD_AND] = true,
	[OVERLOAD_OR] = true,
	[OVERLOAD_XOR] = true,
	[OVERLOAD_SHL] = true,
	[OVERLOAD_SHR] = true,
	[OVERLOAD_EQUAL] = true,
	[OVERLOAD_NOT_EQUAL] = true,
};
/**
 * Do checks on an operator method:
 *
 * 1. Are the arguments valid for the operator?
 * 2. Check if the operator was already defined.
 * 3. See if another operator was defined and if the results match up.
 */
INLINE bool sema_analyse_operator_method(SemaContext *context, Type *parent_type, Decl *method)
{
	// Check it's valid for the operator type.
	if (!sema_check_operator_method_validity(context, method)) return false;

	// See if the operator has already been defined.
	OperatorOverload operator = method->func_decl.operator;

	Type *second_param = NULL;
	bool is_wildcard = false;
	if (vec_size(method->func_decl.signature.params) > 1)
	{
		second_param = method->func_decl.signature.params[1]->type;
		if (!second_param)
		{
			if (method->func_decl.overload_type & OVERLOAD_TYPE_RIGHT)
			{
				RETURN_SEMA_ERROR(method, "Only regular overloads can have untyped right hand parameters");
			}
			is_wildcard = (method->func_decl.is_wildcard_overload = true);
			second_param = type_void;
		}
		second_param = second_param->canonical;
	}

	if (!type_is_user_defined(parent_type))
	{
		RETURN_SEMA_ERROR(method, "Only user-defined types may have overloads.");
	}

	Decl *other = NULL;
	if (operator >= OVERLOAD_TYPED_START)
	{
		Methods *methods = parent_type->decl->method_table;
		if (!methods) goto NONE;
		Decl *decl = declptrzero(methods->overloads[operator]);
		if (!decl) goto NONE;
		Decl **decls;
		unsigned count;
		if (decl->decl_kind == DECL_DECLARRAY)
		{
			decls = decl->decls;
			count = vec_size(decls);
		}
		else
		{
			decls = &decl;
			count = 1;
		}
		OverloadType type = method->func_decl.overload_type;
		assert(type);
		for (unsigned i = 0; i < count; i++)
		{
			Decl *func = decls[i];
			if (func == method) continue;
			assert(func->func_decl.operator);
			if ((func->func_decl.overload_type & type) == 0) continue;
			if (!sema_analyse_decl(context, func)) return false;
			ASSERT_SPAN(func, func->decl_kind == DECL_FUNC || func->decl_kind == DECL_MACRO);
			if (is_wildcard && func->func_decl.is_wildcard_overload)
			{
				other = func;
				break;
			}
			if (is_wildcard || func->func_decl.is_wildcard_overload) continue;
			if (func->func_decl.signature.params[1]->type->canonical != second_param) continue;
			other = func;
			break;
		}
	}
	else
	{
		other = sema_find_untyped_operator(parent_type, operator, method);
	}
	if (other)
	{
		SourceSpan span = method_find_overload_span(method);
		sema_error_at(context, span, "This operator is already defined for '%s'.", parent_type->name);
		SEMA_NOTE(other, "The previous definition was here.");
		return false;
	}
NONE:
	if (parent_type->canonical->type_kind == TYPE_BITSTRUCT)
	{
		switch (operator)
		{
			case OVERLOAD_XOR:
			case OVERLOAD_AND:
			case OVERLOAD_OR:
			case OVERLOAD_XOR_ASSIGN:
			case OVERLOAD_AND_ASSIGN:
			case OVERLOAD_OR_ASSIGN:
			case OVERLOAD_NEGATE:
			{
				SourceSpan span = method_find_overload_span(method);
				RETURN_SEMA_ERROR_AT(span, "Bitstructs do not support overloading the '%s' operator.",
								  operator_overload_to_string(operator));
			}
			default:
				break;
		}
	}
	// Check that actual types match up
	Type *value;
	Type *index_type;
	switch (operator)
	{
		case OVERLOAD_LEN:
			// No dependency
			return true;
		case OVERLOAD_ELEMENT_AT:
			// [] compares &[]
			other = sema_find_untyped_operator(parent_type, OVERLOAD_ELEMENT_REF, method);
			if (other)
			{
				if (!sema_analyse_decl(context, other)) return false;
				sema_get_overload_arguments(other, &value, &index_type);
				break;
			}
			// And []=
			other = sema_find_untyped_operator(parent_type, OVERLOAD_ELEMENT_SET, method);
			if (other)
			{
				if (!sema_analyse_decl(context, other)) return false;
				sema_get_overload_arguments(other, &value, &index_type);
				break;
			}
			return true;
		case OVERLOAD_ELEMENT_REF:
			// &[] compares []
			other = sema_find_untyped_operator(parent_type, OVERLOAD_ELEMENT_AT, method);
			if (other)
			{
				if (!sema_analyse_decl(context, other)) return false;
				sema_get_overload_arguments(other, &value, &index_type);
				break;
			}
			// And []=
			other = sema_find_untyped_operator(parent_type, OVERLOAD_ELEMENT_SET, method);
			if (other)
			{
				if (!sema_analyse_decl(context, other)) return false;
				sema_get_overload_arguments(other, &value, &index_type);
				break;
			}
			return true;
		case OVERLOAD_ELEMENT_SET:
			// []= compares &[]
			other = sema_find_untyped_operator(parent_type, OVERLOAD_ELEMENT_REF, method);
			if (other)
			{
				if (!sema_analyse_decl(context, other)) return false;
				sema_get_overload_arguments(other, &value, &index_type);
				break;
			}
			// And []
			other = sema_find_untyped_operator(parent_type, OVERLOAD_ELEMENT_AT, method);
			if (other)
			{
				if (!sema_analyse_decl(context, other)) return false;
				sema_get_overload_arguments(other, &value, &index_type);
				break;
			}
			return true;
		case OVERLOAD_SHL:
		case OVERLOAD_SHR:
		case OVERLOAD_PLUS:
		case OVERLOAD_DIVIDE:
		case OVERLOAD_REMINDER:
		case OVERLOAD_UNARY_MINUS:
		case OVERLOAD_AND:
		case OVERLOAD_OR:
		case OVERLOAD_XOR:
		case OVERLOAD_NEGATE:
		case OVERLOAD_MULTIPLY:
		case OVERLOAD_MINUS:
		case OVERLOAD_EQUAL:
		case OVERLOAD_NOT_EQUAL:
		case OVERLOAD_PLUS_ASSIGN:
		case OVERLOAD_MINUS_ASSIGN:
		case OVERLOAD_MULTIPLY_ASSIGN:
		case OVERLOAD_DIVIDE_ASSIGN:
		case OVERLOAD_REMINDER_ASSIGN:
		case OVERLOAD_AND_ASSIGN:
		case OVERLOAD_OR_ASSIGN:
		case OVERLOAD_XOR_ASSIGN:
		case OVERLOAD_SHL_ASSIGN:
		case OVERLOAD_SHR_ASSIGN:
			return true;
		default:
			UNREACHABLE
	}

	// We have an operator to compare with.
	Type *this_value;
	Type *this_index_type;
	sema_get_overload_arguments(method, &this_value, &this_index_type);

	// So compare the value
	if (this_value != value)
	{
		if (operator == OVERLOAD_ELEMENT_REF)
		{
			value = type_get_ptr(value);
			this_value = type_get_ptr(this_value);
		}
		SEMA_ERROR(method, "There is a mismatch of the 'value' type compared to that of another operator: expected %s but got %s.",
		           type_quoted_error_string(value), type_quoted_error_string(this_value));
		SEMA_NOTE(other, "The other definition is here.");
		return false;
	}

	// And the index.
	if (this_index_type != index_type)
	{
		SEMA_ERROR(method, "There is a mismatch of the 'index' type compared to that of another operator: expected %s but got %s.",
		           type_quoted_error_string(index_type), type_quoted_error_string(this_index_type));
		SEMA_NOTE(other, "The other definition is here.");
		return false;
	}

	// Everything worked! Done.
	return true;
}

/**
 * Attempt to add a method to the type:
 *
 * 1. See if it is already defined as an extension method => if so show error
 * 2. If it is a non-user defined => delegate to unit_add_base_extension_method
 * 3. If it is check if it is defined on the type.
 * 4. If it is an operator method call sema_analyse_operator_method to check it.
 * 5. Register its external name.
 * 6. Add the module according to visibility.
 */
static inline bool type_add_method(SemaContext *context, Type *parent_type, Decl *method)
{
	ASSERT(parent_type->canonical == parent_type);
	const char *name = method->name;

	// Attributes needs to be resolved early
	bool erase_decl = false;
	if (!sema_analyse_attributes(context, method, method->attributes,
		method->decl_kind == DECL_MACRO ? ATTR_MACRO : ATTR_FUNC, &erase_decl)) return decl_poison(method);
	if (erase_decl)
	{
		method->decl_kind = DECL_ERASED;
		return true;
	}
	// Is it a base extension?
	if (!type_is_user_defined(parent_type)) return unit_add_base_extension_method(context, parent_type, method);

	// Resolve it as a user-defined type extension.
	Decl *parent = parent_type->decl;

	// If we found it, issue an error.
	Decl *other = sema_resolve_method(parent, name);
	if (!decl_ok(other)) return false;
	if (other)
	{
		// We might sometimes end up adding the same method twice.
		if (other == method) return false;
		SEMA_ERROR(method, "This %s is already defined for '%s'.",
		           method_name_by_decl(method), parent_type->name);
		SEMA_NOTE(other, "The previous definition was here.");
		return decl_poison(method);
	}
	if (parent->is_method_checked)
	{
		RETURN_SEMA_ERROR(method, "A method was added to %s which already was checked for method availability, declarations must be reordered.", type_quoted_error_string(parent_type));
	}
	DEBUG_LOG("Method-like '%s.%s' analysed.", parent->name, method->name);

	Methods *table = parent->method_table;
	if (!table)
	{
		table = parent->method_table = CALLOCS(Methods);
		decltable_init(&table->method_table, 64);
	}
	methods_add(table, method);
	return true;
}

/**
 * Find an interface name by searching through its own interfaces as well as any
 * parents to the interface.
 */
static Decl *sema_interface_method_by_name(SemaContext *context, Decl *interface, const char *name)
{
	if (!sema_analyse_decl(context, interface)) return poisoned_decl;
	FOREACH(Decl *, method, interface->interface_methods)
	{
		if (method->name == name) return method;
	}
	FOREACH(TypeInfo *, parent_type, interface->interfaces)
	{
		if (!sema_resolve_type_info(context, parent_type, RESOLVE_TYPE_DEFAULT)) return poisoned_decl;
		Decl *res = sema_interface_method_by_name(context, parent_type->type->decl, name);
		if (res) return res;
	}
	return NULL;
}

/**
 * Given a method, find the interface it implements.
 *
 * 1. Check that it can implement an interface (structs, unions, distinct, faults and enums can)
 * 2. Find the method in the interfaces implemented. If more than one interface has the method then they must match
 * 3. If no match is found we return.
 * 4. Otherwise we fully resolve the matching interface.
 * 5. And return the interface method.
 *
 * If (2) or (4) fails, then this is an error returning a poisoned decl. Not finding a method is fine,
 * this returns NULL.
 */
static inline Decl *sema_find_interface_for_method(SemaContext *context, CanonicalType *parent_type, Decl *method)
{
	// Can the parent even implement a interface?
	switch (parent_type->type_kind)
	{
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_TYPEDEF:
		case TYPE_ENUM:
			break;
		case TYPE_ALIAS:
			UNREACHABLE
		default:
			return NULL;
	}
	const char *name = method->name;
	Decl *first_match = NULL;
	Decl *first_interface = NULL;

	// Walk through all implemented interfaces.
	FOREACH(TypeInfo *, proto, parent_type->decl->interfaces)
	{
		if (!sema_resolve_type_info(context, proto, RESOLVE_TYPE_DEFAULT)) return poisoned_decl;
		Decl *interface = proto->type->decl;
		Decl *match = sema_interface_method_by_name(context, interface, name);
		if (!decl_ok(match)) return poisoned_decl;
		if (!match) continue;
		ASSERT(interface->resolve_status == RESOLVE_DONE);

		// Is there a already a match?
		if (first_match)
		{
			if (first_match->type->function.prototype->raw_type == match->type->function.prototype->raw_type) continue;
			SEMA_ERROR(method,
			           "Both '%s' and '%s' interfaces have a method matching '%s' but their signatures are different, "
			           "which prevents it from being implemented.",
			           first_interface->name, interface->name, name);
			return poisoned_decl;
		}

		// Update the match.
		first_match = match;
		first_interface = interface;
	}

	// No match => return NULL.
	if (!first_match) return NULL;

	// Analyse the interface.
	if (!sema_analyse_decl(context, first_interface)) return poisoned_decl;

	// Return the match.
	return first_match;
}


/**
 * Check that an interface matches the implementing method.
 *
 * 1. Check return types.
 * 2. Check parameter count.
 * 3. Check each parameter for matching types
 *
 * @return true if it matches, false otherwise.
 */
static inline bool sema_compare_method_with_interface(SemaContext *context, Decl *decl, Decl *implemented_method)
{
	Signature interface_sig = implemented_method->func_decl.signature;
	Signature this_sig = decl->func_decl.signature;
	Type *any_rtype = typeget(interface_sig.rtype);
	Type *this_rtype = typeget(this_sig.rtype);

	// Do the return types match?
	if (any_rtype->canonical != this_rtype->canonical)
	{
		SEMA_ERROR(type_infoptr(this_sig.rtype), "The prototype method has a return type %s, but this function returns %s, they need to match.",
		           type_quoted_error_string(any_rtype), type_quoted_error_string(this_rtype));
		SEMA_NOTE(type_infoptr(interface_sig.rtype), "The interface definition is here.");
		return false;
	}

	Decl **any_params = interface_sig.params;
	Decl **this_params = this_sig.params;
	unsigned any_param_count = vec_size(any_params);
	unsigned this_param_count = vec_size(this_params);

	// Do the param counts match?
	if (any_param_count != this_param_count)
	{
		if (any_param_count > this_param_count)
		{
			SEMA_ERROR(decl, "This function is missing parameters, %d parameters were expected.", any_param_count);
			SEMA_NOTE(any_params[this_param_count], "Compare with the interface definition.");
			return false;
		}
		SEMA_ERROR(this_params[any_param_count], "This function has too many parameters (%d).", this_param_count);
		SEMA_NOTE(decl, "Compare with the interface, which has only %d parameter%s.",
		          any_param_count, any_param_count == 1 ? "" : "s");
		return false;
	}

	// Check each param.
	FOREACH_IDX(i, Decl *, param, this_params)
	{
		if (i == 0) continue;
		if (param->type->canonical != any_params[i]->type->canonical)
		{
			SEMA_ERROR(vartype(param),
			           "The prototype argument has type %s, but in this function it has type %s. Please make them match.",
			           type_quoted_error_string(any_params[i]->type), type_quoted_error_string(param->type));
			SEMA_NOTE(vartype(any_params[i]), "The interface definition is here.");
			return false;
		}
	}
	return true;
}

/**
 * Analyse a method.
 *
 * 1. Check that it has no init/finalizer attributes.
 * 2. Check that it has no test/benchmark attributes.
 * 3. Resolve the declaration of the parent (as needed).
 * 4. Check that it has at least one parameter.
 * 5. Check that this parameter is correct.
 * 6. If it is dynamic, the type may not be an interface or any
 * 7. If it is dynamic, make sure that it implements an interface correctly if available
 * 8. Try adding the method
 */
static inline bool sema_analyse_method(SemaContext *context, Decl *decl)
{
	ASSERT_SPAN(decl, decl->decl_kind == DECL_FUNC);
	// Check for @init, @finalizer, @test and @benchmark
	if (decl->func_decl.attr_init | decl->func_decl.attr_finalizer)
	{
		SEMA_ERROR(decl, "Methods may not have '@init' or '@finalizer' attributes.");
		return decl_poison(decl);
	}
	if (decl->func_decl.attr_test || decl->func_decl.attr_benchmark)
	{
		SEMA_ERROR(decl, "Methods may not be annotated %s.", decl->func_decl.attr_test ? "@test" : "@benchmark");
		return decl_poison(decl);
	}

	// Resolve the parent type.
	TypeInfo *parent_type = type_infoptr(decl->func_decl.type_parent);
	ASSERT(parent_type->resolve_status == RESOLVE_DONE);
	Type *par_type = parent_type->type->canonical;

	// Resolve declaration of parent as needed.
	if (!sema_resolve_type_decl(context, par_type)) return false;
	const char *kw = decl->name;
	const char *errname = NULL;
	switch (par_type->canonical->type_kind)
	{
		case TYPE_ENUM:
			if (kw == kw_ordinal || kw == kw_nameof)
			{
				errname = "an enum";
				goto NOT_VALID_NAME;
			}
			FALLTHROUGH;
		case TYPE_CONST_ENUM:
		case TYPE_STRUCT:
		case TYPE_UNION:
		{
			Decl *d = sema_decl_stack_find_decl_member(context, par_type->canonical->decl, kw, FIELDS_ONLY);
			if (!decl_ok(d)) return false;
			if (d) RETURN_SEMA_ERROR(decl, "%s already has a field with the same name.", type_quoted_error_string(par_type));
			break;
		}
		case TYPE_ANYFAULT:
			if (kw == kw_type || kw == kw_nameof)
			{
				errname = "'fault'";
				goto NOT_VALID_NAME;
			}
			break;
		case TYPE_ANY:
		case TYPE_INTERFACE:
			if (kw == kw_ptr || kw == kw_type)
			{
				errname = "an interface or 'any'";
				goto NOT_VALID_NAME;
			}
			break;
		case TYPE_SLICE:
			if (kw == kw_ptr || kw == kw_len)
			{
				errname = "a slice";
				goto NOT_VALID_NAME;
			}
			break;
		case ALL_VECTORS:
			if (sema_kw_is_swizzle(decl->name, strlen(decl->name)))
			{
				RETURN_SEMA_ERROR(decl, "\"%s\" is not a valid method name for a vector, since it matches a swizzle combination.", kw);
			}
			FALLTHROUGH;
		case TYPE_ARRAY:
		case TYPE_INFERRED_ARRAY:
		case TYPE_FLEXIBLE_ARRAY:
			if (kw == kw_len)
			{
				errname = "a vector or array";
				goto NOT_VALID_NAME;
			}
			break;
		case TYPE_POISONED:
		case TYPE_VOID:
		case ALL_FLOATS:
		case ALL_INTS:
		case TYPE_BOOL:
		case TYPE_FUNC_PTR:
		case TYPE_POINTER:
		case TYPE_FUNC_RAW:
		case TYPE_UNTYPED_LIST:
		case TYPE_BITSTRUCT:
		case TYPE_TYPEDEF:
			break;
		case TYPE_TYPEID:
			if (type_property_by_name(kw) != TYPE_PROPERTY_NONE)
			{
				RETURN_SEMA_ERROR(decl, "\"%s\" is not a valid method name for a typeid as this is the name of a type property.", decl->name);
			}
			break;
		case TYPE_ALIAS:
		case TYPE_OPTIONAL:
		case TYPE_WILDCARD:
		case TYPE_TYPEINFO:
		case TYPE_MEMBER:
			UNREACHABLE
	}
	Decl **params = decl->func_decl.signature.params;
	bool is_dynamic = decl->func_decl.attr_dynamic;

	// Ensure that the first parameter is valid.
	if (!sema_is_valid_method_param(context, params[0], par_type, is_dynamic)) return false;

	// Make dynamic checks.
	if (is_dynamic)
	{
		if (par_type->type_kind == TYPE_INTERFACE)
		{
			RETURN_SEMA_ERROR(decl, "Interfaces may not implement '@dynamic' methods, only regular methods.");
		}
		if (par_type == type_any)
		{
			RETURN_SEMA_ERROR(decl, "'any' may not implement '@dynamic' methods, only regular methods.");
		}
		// Retrieve the implemented method.
		Decl *implemented_method = sema_find_interface_for_method(context, par_type, decl);
		if (!decl_ok(implemented_method)) return false;

		// If it's implementing a method, check it.
		if (implemented_method)
		{
			ASSERT(implemented_method->resolve_status == RESOLVE_DONE);
			if (!sema_compare_method_with_interface(context, decl, implemented_method)) return false;
			decl->func_decl.interface_method = declid(implemented_method);
		}
		else
		{
			decl->func_decl.interface_method = 0;
		}
	}
	// Is it an operator?
	if (decl->func_decl.operator)
	{
		// We must resolve it here to avoid recursion.
		decl->resolve_status = RESOLVE_DONE;
		if (!sema_analyse_operator_method(context, par_type, decl)) return false;
	}

	return true;
NOT_VALID_NAME:
	RETURN_SEMA_ERROR(decl, "\"%s\" is not a valid method name for %s, as this would shadow the built-in property '.%s'.", kw, errname, kw);
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
		case ATTR_BITSTRUCT_MEMBER:
			return "bitstruct member";
		case ATTR_FUNC:
			return "function";
		case ATTR_PARAM:
			return "parameter";
		case ATTR_ENUM_VALUE:
			return "enum value";
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
		case ATTR_FAULT:
			return "faultdef";
		case ATTR_ALIAS:
			return "alias";
		case ATTR_CALL:
			return "call";
		case ATTR_TYPEDEF:
			return "typedef";
		case ATTR_INTERFACE_METHOD:
			return "interface method";
		case ATTR_FNTYPE:
			return "function type";
	}
	UNREACHABLE
}

// Helper method
INLINE bool update_abi(Decl *decl, CallABI abi)
{
	if (decl->decl_kind == DECL_FNTYPE)
	{
		decl->fntype_decl.signature.abi = abi;
		return true;
	}
	decl->func_decl.signature.abi = abi;
	return true;
}

/**
 * Given a @callconv string, update the call convention.
 *
 * Test for:
 * 1. cdecl
 * 2. veccall
 * 3. stdcall
 *
 * If the platform is unsupported on the platform, it is ignored.
 */
static bool update_call_abi_from_string(SemaContext *context, Decl *decl, Expr *expr)
{
	const char *str = expr->const_expr.bytes.ptr;
	// C decl is easy
	if (str_eq(str, "cdecl")) return update_abi(decl, CALL_C);

	// Check veccall
	if (str_eq(str, "veccall"))
	{
		switch (compiler.platform.arch)
		{
			case ARCH_TYPE_X86_64:
				return update_abi(decl, CALL_X64_VECTOR);
			case ARCH_TYPE_ARM:
			case ARCH_TYPE_ARMB:
			case ARCH_TYPE_AARCH64:
			case ARCH_TYPE_AARCH64_32:
			case ARCH_TYPE_AARCH64_BE:
				return update_abi(decl, CALL_AAPCS_VFP);
			case ARCH_TYPE_X86:
				// Unsupported
				FALLTHROUGH;
			default:
				return true;
		}
	}
	// Finally check stdcall.
	if (strcmp(str, "stdcall") == 0)
	{
		if (compiler.platform.arch == ARCH_TYPE_ARM || compiler.platform.arch == ARCH_TYPE_ARMB)
		{
			return update_abi(decl, CALL_AAPCS);
		}
		return true;
	}
	RETURN_SEMA_ERROR(expr, "Unknown call convention, only 'cdecl', 'stdcall' and 'veccall' are supported");
}


/**
 * Analyse almost all attributes.
 */
static bool sema_analyse_attribute(SemaContext *context, ResolvedAttrData *attr_data, Decl *decl, Attr *attr, AttributeDomain domain, bool *erase_decl)
{
	AttributeType type = attr->attr_kind;
	ASSERT(type >= 0 && type < NUMBER_OF_ATTRIBUTES);
	// NOLINTBEGIN(*.EnumCastOutOfRange)
	static AttributeDomain attribute_domain[NUMBER_OF_ATTRIBUTES] = {
			[ATTRIBUTE_ALIGN] = ATTR_FUNC | ATTR_CONST | ATTR_LOCAL | ATTR_GLOBAL | ATTR_BITSTRUCT | ATTR_STRUCT | ATTR_UNION | ATTR_MEMBER, // NOLINT
			[ATTRIBUTE_ALLOW_DEPRECATED] = ATTR_FUNC,
			[ATTRIBUTE_BENCHMARK] = ATTR_FUNC,
			[ATTRIBUTE_BIGENDIAN] = ATTR_BITSTRUCT,
			[ATTRIBUTE_BUILTIN] = ATTR_MACRO | ATTR_FUNC | ATTR_GLOBAL | ATTR_CONST,
			[ATTRIBUTE_CALLCONV] = ATTR_FUNC | ATTR_INTERFACE_METHOD | ATTR_FNTYPE,
			[ATTRIBUTE_CNAME] = ATTR_FUNC | ATTR_GLOBAL | ATTR_CONST | USER_DEFINED_TYPES,
			[ATTRIBUTE_COMPACT] = ATTR_STRUCT | ATTR_UNION,
			[ATTRIBUTE_CONST] = ATTR_MACRO,
			[ATTRIBUTE_CONSTINIT] = ATTR_TYPEDEF | ATTR_ENUM,
			[ATTRIBUTE_DEPRECATED] = (AttributeDomain)~(ATTR_CALL | ATTR_PARAM),
			[ATTRIBUTE_DYNAMIC] = ATTR_FUNC,
			[ATTRIBUTE_EXPORT] = ATTR_FUNC | ATTR_GLOBAL | ATTR_CONST | USER_DEFINED_TYPES | ATTR_ALIAS,
			[ATTRIBUTE_EXTERN] = ATTR_FUNC | ATTR_GLOBAL | ATTR_CONST | USER_DEFINED_TYPES,
			[ATTRIBUTE_FINALIZER] = ATTR_FUNC,
			[ATTRIBUTE_FORMAT] = ATTR_FUNC | ATTR_MACRO | ATTR_FNTYPE,
			[ATTRIBUTE_IF] = (AttributeDomain)~(ATTR_CALL | ATTR_PARAM),
			[ATTRIBUTE_INIT] = ATTR_FUNC,
			[ATTRIBUTE_INLINE] = ATTR_FUNC | ATTR_CALL,
			[ATTRIBUTE_JUMP] = 0, // Special, used for switch only
			[ATTRIBUTE_LINK] = ATTR_FUNC | ATTR_MACRO | ATTR_CONST | ATTR_GLOBAL,
			[ATTRIBUTE_LITTLEENDIAN] = ATTR_BITSTRUCT,
			[ATTRIBUTE_LOCAL] = ATTR_FUNC | ATTR_MACRO | ATTR_GLOBAL | ATTR_CONST | USER_DEFINED_TYPES | ATTR_ALIAS | ATTR_INTERFACE,
			[ATTRIBUTE_MAYDISCARD] = CALLABLE_TYPE,
			[ATTRIBUTE_NAKED] = ATTR_FUNC,
			[ATTRIBUTE_NOALIAS] = ATTR_PARAM,
			[ATTRIBUTE_NODISCARD] = CALLABLE_TYPE,
			[ATTRIBUTE_NOINIT] = ATTR_GLOBAL | ATTR_LOCAL,
			[ATTRIBUTE_NOINLINE] = ATTR_FUNC | ATTR_CALL,
			[ATTRIBUTE_NOPADDING] = ATTR_STRUCT | ATTR_UNION | ATTR_MEMBER,
			[ATTRIBUTE_NORETURN] = CALLABLE_TYPE,
			[ATTRIBUTE_NOSANITIZE] = ATTR_FUNC,
			[ATTRIBUTE_NOSTRIP] = ATTR_FUNC | ATTR_GLOBAL | ATTR_CONST | EXPORTED_USER_DEFINED_TYPES,
			[ATTRIBUTE_OBFUSCATE] = ATTR_ENUM | ATTR_FAULT,
			[ATTRIBUTE_OPERATOR] = ATTR_MACRO | ATTR_FUNC,
			[ATTRIBUTE_OPERATOR_R] = ATTR_MACRO | ATTR_FUNC,
			[ATTRIBUTE_OPERATOR_S] = ATTR_MACRO | ATTR_FUNC,
			[ATTRIBUTE_OPTIONAL] = ATTR_INTERFACE_METHOD,
			[ATTRIBUTE_OVERLAP] = ATTR_BITSTRUCT,
			[ATTRIBUTE_PACKED] = ATTR_STRUCT | ATTR_UNION,
			[ATTRIBUTE_PRIVATE] = ATTR_FUNC | ATTR_MACRO | ATTR_GLOBAL | ATTR_CONST | USER_DEFINED_TYPES | ATTR_ALIAS | ATTR_INTERFACE,
			[ATTRIBUTE_PUBLIC] = ATTR_FUNC | ATTR_MACRO | ATTR_GLOBAL | ATTR_CONST | USER_DEFINED_TYPES | ATTR_ALIAS | ATTR_INTERFACE,
			[ATTRIBUTE_PURE] = ATTR_CALL,
			[ATTRIBUTE_REFLECT] = ATTR_FUNC | ATTR_GLOBAL | ATTR_CONST | USER_DEFINED_TYPES,
			[ATTRIBUTE_SAFEMACRO] = ATTR_MACRO,
			[ATTRIBUTE_SAFEINFER] = ATTR_GLOBAL | ATTR_LOCAL,
			[ATTRIBUTE_SECTION] = ATTR_FUNC | ATTR_CONST | ATTR_GLOBAL,
			[ATTRIBUTE_SIMD] = 0,
			[ATTRIBUTE_STRUCTLIKE] = ATTR_TYPEDEF,
			[ATTRIBUTE_TAG] = ATTR_BITSTRUCT_MEMBER | ATTR_MEMBER | USER_DEFINED_TYPES | CALLABLE_TYPE,
			[ATTRIBUTE_TEST] = ATTR_FUNC,
			[ATTRIBUTE_UNUSED] = (AttributeDomain)~(ATTR_CALL),
			[ATTRIBUTE_USED] = (AttributeDomain)~(ATTR_CALL),
			[ATTRIBUTE_WASM] = ATTR_FUNC,
			[ATTRIBUTE_WEAK] = ATTR_FUNC | ATTR_CONST | ATTR_GLOBAL | ATTR_ALIAS,
			[ATTRIBUTE_WINMAIN] = ATTR_FUNC,
	};
	// NOLINTEND(*.EnumCastOutOfRange)
	// First check if it doesn't match the domain.
	if ((attribute_domain[type] & domain) != domain)
	{
		RETURN_SEMA_ERROR(attr, "'%s' is not a valid %s attribute.", attr->name, attribute_domain_to_string(domain));
	}

	// No attribute has more than one argument right now.
	unsigned args = vec_size(attr->exprs);
	if (args > 1 && type != ATTRIBUTE_LINK && type != ATTRIBUTE_TAG && type != ATTRIBUTE_WASM)
	{
		RETURN_SEMA_ERROR(attr->exprs[1], "Too many arguments for the attribute.");
	}
	Expr *expr = args ? attr->exprs[0] : NULL;

	switch (type)
	{
		case ATTRIBUTE_PRIVATE:
		case ATTRIBUTE_PUBLIC:
		case ATTRIBUTE_LOCAL:
		case ATTRIBUTE_BUILTIN:
		case ATTRIBUTE_NORECURSE:
			// These are pseudo-attributes and are processed separately.
			UNREACHABLE
		case ATTRIBUTE_DEPRECATED:
			if (attr_data->deprecated)
			{
				RETURN_SEMA_ERROR(attr, "There can't be more than a single '@deprecated' tag.");
			}
			// We expect an optional string.
			attr_data->deprecated = "";
			if (expr)
			{
				if (!sema_analyse_expr_rvalue(context, expr)) return false;
				if (!expr_is_const_string(expr))
				{
					RETURN_SEMA_ERROR(expr, "Expected a constant string value as argument.");
				}
				attr_data->deprecated = expr->const_expr.bytes.ptr;
			}
			return true;
		case ATTRIBUTE_ALLOW_DEPRECATED:
			decl->allow_deprecated = true;
			return true;
		case ATTRIBUTE_OPTIONAL:
			decl->func_decl.attr_optional = true;
			return true;
		case ATTRIBUTE_WINMAIN:
			if (decl->name != kw_main)
			{
				SEMA_ERROR(attr, "'@winmain' can only be used on the 'main' function.");
				return false;
			}
			decl->func_decl.attr_winmain = true;
			break;
		case ATTRIBUTE_CALLCONV:
			if (!expr) RETURN_SEMA_ERROR(decl, "Expected a string argument.");
			if (expr && !sema_analyse_expr_rvalue(context, expr)) return false;
			if (!expr_is_const_string(expr)) RETURN_SEMA_ERROR(expr, "Expected a constant string value as argument.");
			if (!update_call_abi_from_string(context, decl, expr)) return false;
			return true;
		case ATTRIBUTE_BENCHMARK:
			decl->func_decl.attr_benchmark = true;
			break;
		case ATTRIBUTE_TAG:
		{
			decl->has_tag = true;
			if (args != 2) RETURN_SEMA_ERROR(attr, "'@tag' requires two arguments.");
			Expr *string = attr->exprs[0];
			Expr *val = attr->exprs[1];
			if (!sema_analyse_expr_rvalue(context, string)) return false;
			if (!sema_cast_const(string) || !expr_is_const_string(string)) RETURN_SEMA_ERROR(string, "Expected a constant string here, usage is: '@tag(name, value)'.");
			if (!sema_analyse_expr_rvalue(context, val)) return false;
			if (!sema_cast_const(val)) RETURN_SEMA_ERROR(val, "Expected a constant value here, usage is: '@tag(name, value)'.");
			const char *name = string->const_expr.bytes.ptr;
			FOREACH_IDX(i, Attr *, tag, attr_data->tags)
			{
				// Overwrite if already found
				if (str_eq(tag->exprs[0]->const_expr.bytes.ptr, name))
				{
					attr_data->tags[i] = attr;
					return true;
				}
			}
			vec_add(attr_data->tags, attr);
			return true;
		}
		case ATTRIBUTE_TEST:
			decl->func_decl.attr_test = true;
			break;
		case ATTRIBUTE_OPERATOR_R:
		case ATTRIBUTE_OPERATOR_S:
		case ATTRIBUTE_OPERATOR:
		{
			ASSERT(decl->decl_kind == DECL_FUNC || decl->decl_kind == DECL_MACRO);
			if (!expr) goto FAILED_OP_TYPE;
			if (decl->func_decl.operator)
			{
				RETURN_SEMA_ERROR(attr, "This method already has overload, it can't match multiple ones.");
			}
			switch (expr->expr_kind)
			{
				case EXPR_UNRESOLVED_IDENTIFIER:
					if (expr->unresolved_ident_expr.path) goto FAILED_OP_TYPE;
					if (expr->unresolved_ident_expr.ident != kw_len) goto FAILED_OP_TYPE;
					decl->func_decl.operator = OVERLOAD_LEN;
					break;
				case EXPR_OPERATOR_CHARS:
					decl->func_decl.operator = expr->overload_expr;
					switch (type)
					{
						case ATTRIBUTE_OPERATOR:
							decl->func_decl.overload_type = OVERLOAD_TYPE_LEFT;
							break;
						case ATTRIBUTE_OPERATOR_R:
							if (!OVERLOAD_MAY_BE_REVERSE[expr->overload_expr])
							{
								RETURN_SEMA_ERROR(attr, "'%s' may not be used with @operator_r(), only @operator().", OVERLOAD_NAME[expr->overload_expr]);
							}
							decl->func_decl.overload_type = OVERLOAD_TYPE_RIGHT;
							break;
						case ATTRIBUTE_OPERATOR_S:
							if (!OVERLOAD_MAY_BE_REVERSE[expr->overload_expr])
							{
								RETURN_SEMA_ERROR(attr, "'%s' may not be used with @operator_s(), only @operator().", OVERLOAD_NAME[expr->overload_expr]);
							}
							decl->func_decl.overload_type = OVERLOAD_TYPE_SYMMETRIC;
							break;
						default:
							UNREACHABLE
					}
					break;
				default:
					goto FAILED_OP_TYPE;
			}
			attr_data->overload = attr->span;
			if (!decl->func_decl.type_parent)
			{
				RETURN_SEMA_ERROR(expr, "@operator(...) can only be used with methods.");
			}
			return true;
			FAILED_OP_TYPE:
			RETURN_SEMA_ERROR(attr, "'operator' requires an operator type argument. It should be any of the arithmetic and bit operators, equality operators, '[]', '[]=', '&[]' or 'len'.");
		}
		case ATTRIBUTE_ALIGN:
			if (!expr)
			{
				RETURN_SEMA_ERROR(attr, "'align' requires an power-of-2 argument, e.g. align(8).");
			}
			return sema_resolve_align_expr(context, expr, &decl->alignment);
		case ATTRIBUTE_WASM:
			if (args > 2) RETURN_SEMA_ERROR(attr->exprs[2], "Too many arguments to '@wasm', expected 0, 1 or 2 arguments");
			decl->is_export = true;
			if (decl->is_templated)
			{
				RETURN_SEMA_ERROR(attr, "'@wasm' is not allowed on generic declarations.");
			}
			if (args == 0) return true;
			if (decl->has_extname)
			{
				RETURN_SEMA_ERROR(expr, "An external name is already defined, please use '@wasm` without an argument.");
			}
			if (args == 2)
			{
				if (!decl->is_extern)
				{
					RETURN_SEMA_ERROR(expr, "Specifying a wasm import module name is only valid for extern declarations.");
				}
				Expr *module = expr;
				expr = attr->exprs[1];
				if (!sema_analyse_expr_rvalue(context, module)) return false;
				if (!expr_is_const_string(module))
				{
					RETURN_SEMA_ERROR(module, "Expected a constant string value as argument.");
				}
				attr_data->wasm_module = module->const_expr.bytes.ptr;
			}

			if (!sema_analyse_expr_rvalue(context, expr)) return false;
			if (!expr_is_const_string(expr))
			{
				RETURN_SEMA_ERROR(expr, "Expected a constant string value as argument.");
			}
			decl->extname = expr->const_expr.bytes.ptr;
			decl->has_extname = true;
			return true;
		case ATTRIBUTE_EXPORT:
			if (decl->is_templated)
			{
				RETURN_SEMA_ERROR(attr, "'@export' is not allowed on generic declarations.");
			}
			if (expr)
			{
				if (!sema_analyse_expr_rvalue(context, expr)) return false;
				if (!expr_is_const_string(expr))
				{
					RETURN_SEMA_ERROR(expr, "Expected a constant string value as argument.");
				}
				if (decl->has_extname)
				{
					RETURN_SEMA_ERROR(expr, "An external name is already defined, please use '@export` without an argument.");
				}
				decl->has_extname = true;
				decl->extname = expr->const_expr.bytes.ptr;
			}
			decl->is_export = true;
			return true;
		case ATTRIBUTE_NOSTRIP:
			decl->no_strip = true;
			return true;
		case ATTRIBUTE_NOALIAS:
			decl->var.no_alias = true;
			return true;
		case ATTRIBUTE_IF:
			if (!expr) RETURN_SEMA_ERROR(attr, "'@if' requires a boolean argument.");
			if (!sema_analyse_expr_rvalue(context, expr)) return false;
			if (!cast_explicit_silent(context, expr, type_bool) || !sema_cast_const(expr))
			{
				RETURN_SEMA_ERROR(expr, "Expected a boolean compile time constant value.");
			}
			if (!expr->const_expr.b) *erase_decl = true;
			return true;
		case ATTRIBUTE_FINALIZER:
			decl->func_decl.attr_finalizer = true;
			// Ugly
			goto PARSE;
		case ATTRIBUTE_FORMAT:
			if (args != 1) RETURN_SEMA_ERROR(attr, "'@format' expects the index of the format string as the argument, e.g. '@format(1)'.");
			if (!sema_analyse_expr_rvalue(context, expr)) return false;
			if (!type_is_integer(expr->type) || !sema_cast_const(expr))
			{
				RETURN_SEMA_ERROR(expr, "Expected an integer compile time constant value.");
			}
			{
				Int i = expr->const_expr.ixx;
				if (int_is_neg(i) || int_icomp(i, 127, BINARYOP_GT))
				{
					RETURN_SEMA_ERROR(expr, "The index must be between 0 and 127.");
				}
				uint16_t val = (uint16_t)i.i.low;
				switch (decl->decl_kind)
				{
					case DECL_FUNC:
					case DECL_MACRO:
						if (decl->func_decl.signature.attrs.format) break;
						decl->func_decl.signature.attrs.format = val + 1;
						return true;
					case DECL_FNTYPE:
						if (decl->fntype_decl.signature.attrs.format) break;
						decl->fntype_decl.signature.attrs.format = val + 1;
						return true;
					default:
						UNREACHABLE
				}
				RETURN_SEMA_ERROR(attr, "'@format' may not appear twice.");
			}
		case ATTRIBUTE_LINK:
			if (args < 1) RETURN_SEMA_ERROR(attr, "'@link' requires at least one argument.");
			Expr *cond = args > 1 ? attr->exprs[0] : NULL;
			if (cond && !sema_analyse_expr_rvalue(context, cond)) return false;
			int start = 0;
			bool has_link = true;
			if (cond && expr_is_const_bool(cond))
			{
				start = 1;
				has_link = cond->const_expr.b;
			}
			for (unsigned i = start; i < args; i++)
			{
				Expr *string = attr->exprs[i];
				if (!sema_analyse_expr_rvalue(context, string)) return false;
				if (!expr_is_const_string(string)) RETURN_SEMA_ERROR(string, "Expected a constant string here, usage is: '@link(cond1, link1, link2, ...)'.");
				if (has_link) vec_add(attr_data->links, string->const_expr.bytes.ptr);
			}
			return true;
		case ATTRIBUTE_INIT:
			decl->func_decl.attr_init = true;
		PARSE:;
			if (expr)
			{
				if (!sema_analyse_expr_rvalue(context, expr)) return false;
				if (!expr_is_const_int(expr))
				{
					RETURN_SEMA_ERROR(attr, "Expected an integer value.");
				}
				uint64_t prio = decl->func_decl.priority = expr->const_expr.ixx.i.low;
				if (expr_const_will_overflow(&expr->const_expr, TYPE_U16) || prio > MAX_PRIORITY || prio < 1)
				{
					RETURN_SEMA_ERROR(attr, "The priority must be a value between 1 and %d", MAX_PRIORITY);
				}
			}
			if (!decl->func_decl.priority) decl->func_decl.priority = MAX_PRIORITY;
			return true;
		case ATTRIBUTE_STRUCTLIKE:
			decl->attr_structlike = true;
			return true;
		case ATTRIBUTE_CONSTINIT:
			decl->attr_constinit = true;
			return true;
		case ATTRIBUTE_SIMD:
			RETURN_SEMA_ERROR(attr, "'@simd' is only allowed on typedef types.");
		case ATTRIBUTE_SECTION:
		case ATTRIBUTE_CNAME:
		case ATTRIBUTE_EXTERN:
			if (decl->is_templated)
			{
				RETURN_SEMA_ERROR(attr, "'%s' attributes are not allowed for generic declarations.", attr->name);
			}
			if (!expr)
			{
				RETURN_SEMA_ERROR(attr, "'%s' requires a string argument, e.g. %s(\"foo\").", attr->name, attr->name);
			}
			if (!sema_analyse_expr_rvalue(context, expr)) return false;
			if (!expr_is_const_string(expr))
			{
				RETURN_SEMA_ERROR(expr, "Expected a constant string value as argument.");
			}
			switch (type)
			{
				case ATTRIBUTE_SECTION:
					if (!sema_check_section(context, attr)) return false;
					attr_data->section = expr->const_expr.bytes.ptr;
					break;
				case ATTRIBUTE_CNAME:
				case ATTRIBUTE_EXTERN:
					decl->has_extname = true;
					decl->extname = expr->const_expr.bytes.ptr;
					break;
				default:
					UNREACHABLE
			}
			return true;
		case ATTRIBUTE_NOINLINE:
			decl->func_decl.attr_noinline = true;
			decl->func_decl.attr_inline = false;
			break;
		case ATTRIBUTE_NOPADDING:
			decl->attr_nopadding = true;
			break;
		case ATTRIBUTE_COMPACT:
			decl->attr_nopadding = true;
			decl->attr_compact = true;
			break;
		case ATTRIBUTE_SAFEINFER:
			decl->var.safe_infer = true;
			break;
		case ATTRIBUTE_NOINIT:
			decl->var.no_init = true;
			break;
		case ATTRIBUTE_CONST:
			decl->func_decl.signature.attrs.always_const = true;
			break;
		case ATTRIBUTE_NODISCARD:
			if (decl->func_decl.signature.attrs.nodiscard)
			{
				RETURN_SEMA_ERROR(attr, "@nodiscard cannot be combined with @noreturn.");
			}
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
			if (decl->func_decl.signature.attrs.nodiscard)
			{
				RETURN_SEMA_ERROR(attr, "@noreturn cannot be combined with @nodiscard.");
			}
			decl->func_decl.signature.attrs.noreturn = true;
			break;
		case ATTRIBUTE_NOSANITIZE:
			if (!expr)
			{
				RETURN_SEMA_ERROR(attr, "'%s' requires a string argument, e.g. %s(\"address\").", attr->name, attr->name);
			}
			if (!sema_analyse_expr_rvalue(context, expr)) return false;
			if (!expr_is_const_string(expr))
			{
				RETURN_SEMA_ERROR(expr, "Expected a constant string value as argument.");
			}
			const char *str = expr->const_expr.bytes.ptr;
			if (str_eq(str, "address"))
			{
				decl->func_decl.attr_nosanitize_address = true;
			}
			else if (str_eq(str, "memory"))
			{
				decl->func_decl.attr_nosanitize_memory = true;
			}
			else if (str_eq(str, "thread"))
			{
				decl->func_decl.attr_nosanitize_thread = true;
			}
			else
			{
				RETURN_SEMA_ERROR(expr, "Expected \"address\", \"memory\" or \"thread\" as argument.");
			}
			return true;
		case ATTRIBUTE_WEAK:
			if (domain == ATTR_ALIAS)
			{
				if (decl->decl_kind != DECL_TYPE_ALIAS) RETURN_SEMA_ERROR(attr, "'@weak' can only be used on type aliases.");
				if (!decl->type_alias_decl.is_redef)
				{
					RETURN_SEMA_ERROR(attr, "'@weak' is only allowed on type aliases with the same name, eg 'def Foo = bar::def::Foo @weak'.");
				}
			}
			decl->is_weak = true;
			break;
		case ATTRIBUTE_NAKED:
			ASSERT(domain == ATTR_FUNC);
			decl->func_decl.attr_naked = true;
			break;
		case ATTRIBUTE_OVERLAP:
			decl->strukt.overlap = true;
			break;
		case ATTRIBUTE_DYNAMIC:
			decl->func_decl.attr_dynamic = true;
			break;
		case ATTRIBUTE_BIGENDIAN:
			if (decl->strukt.little_endian)
			{
				SEMA_ERROR(attr, "Attribute cannot be combined with @littleendian");
				return decl_poison(decl);
			}
			decl->strukt.big_endian = true;
			break;
		case ATTRIBUTE_LITTLEENDIAN:
			if (decl->strukt.big_endian)
			{
				SEMA_ERROR(attr, "Attribute cannot be combined with @bigendian");
				return decl_poison(decl);
			}
			decl->strukt.little_endian = true;
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
		case ATTRIBUTE_PURE:
			// Only used for calls.
			UNREACHABLE
		case ATTRIBUTE_SAFEMACRO:
			decl->func_decl.signature.is_safemacro = true;
			break;
		case ATTRIBUTE_REFLECT:
			decl->will_reflect = true;
			break;
		case ATTRIBUTE_OBFUSCATE:
			decl->obfuscate = true;
			break;
		case ATTRIBUTE_JUMP:
			break;
		case ATTRIBUTE_NONE:
			UNREACHABLE
	}
	if (expr) RETURN_SEMA_ERROR(expr, "'%s' should not have any arguments.", attr->name);
	return true;

}


static inline bool sema_analyse_custom_attribute(SemaContext *context, ResolvedAttrData *attr_data_ref, Decl *decl, Attr *attr, AttributeDomain domain,
                                            Decl *top, bool *erase_decl)
{
	// Custom attributes.
	// First find it.
	Decl *attr_decl = sema_resolve_symbol(context, attr->name, attr->path, attr->span);
	if (!attr_decl) return false;

	// Detect direct cycles @Foo = @Bar @Bar = @Foo
	if (attr_decl == top)
	{
		SEMA_ERROR(top, "Recursive declaration of attribute '%s'.", top->name);
		return decl_poison(attr_decl);
	}

	// Handle the case where the current function is the declaration itself.
	if (context->call_env.kind == CALL_ENV_ATTR && context->call_env.attr_declaration == attr_decl)
	{
		decl_poison(attr_decl);
		RETURN_SEMA_ERROR(attr_decl, "Recursive declaration of attribute '%s' – it contains itself.",
		                  attr_decl->name);
	}

	// Grab all the parameters to the attribute and copy them.
	Decl **params = attr_decl->attr_decl.params;
	unsigned param_count = vec_size(params);
	params = copy_decl_list_single(params);

	// Get the arguments
	Expr **args = attr->exprs;

	// Check that the parameters match. No varargs are allowed on attributes,
	// there seems to be little use for it.
	if (param_count != vec_size(args))
	{
		SEMA_ERROR(attr, "Expected %d parameter(s).", param_count);
		SEMA_NOTE(attr_decl, "The declaration was here.");
		return false;
	}

	// Ok, we have the list of inner attributes.
	Attr **attributes = attr_decl->attr_decl.attrs;
	attributes = copy_attributes_single(attributes);

	// Now we need to evaluate these attributes in the attribute definition
	// context.
	SemaContext eval_context;
	sema_context_init(&eval_context, attr_decl->unit);
	eval_context.macro_call_depth = context->macro_call_depth + 1;
	CallEnv eval_env = { .kind = CALL_ENV_ATTR, .attr_declaration = decl,  };
	eval_context.call_env = eval_env;
	// We copy the compilation unit.
	eval_context.compilation_unit = context->unit;

	// First we need to analyse each expression in the current scope
	for (int j = 0; j < param_count; j++)
	{
		Decl *param = params[j];
		Expr *expr = args[j];
		if (param->var.type_info)
		{
			if (!sema_resolve_type_info(context, type_infoptr(param->var.type_info), RESOLVE_TYPE_DEFAULT)) return false;
			Type *type = typeget(param->var.type_info);
			ASSERT_SPAN(decl, type);
			eval_context.rtype = type;
			if (!sema_analyse_inferred_expr(context, type, expr, NULL)) goto ERR;
			if (!cast_implicit(context, expr, type, false)) goto ERR;
			if (!sema_cast_const(expr))
			{
				SEMA_ERROR(expr, "Expected a compile time value.");
				goto ERR;
			}
		}
		else
		{
			eval_context.rtype = type_void;
			if (!sema_analyse_ct_expr(context, args[j])) goto ERR;
		}
		params[j]->var.init_expr = expr;
		params[j]->var.kind = VARDECL_CONST;
		// Then add them to the evaluation context.
		// (Yes this is messy)
		sema_add_local(&eval_context, params[j]);
	}
	eval_context.rtype = type_void;
	// Now we've added everything to the evaluation context, so we can (recursively)
	// apply it to the contained attributes, which in turn may be derived attributes.
	if (!sema_analyse_attributes_inner(&eval_context, attr_data_ref, decl, attributes, domain, top ? top : attr_decl, erase_decl)) goto ERR;
	// Then destroy the eval context.
	sema_context_destroy(&eval_context);
	// Stop evaluating on erase.
	return true;
ERR:
	sema_context_destroy(&eval_context);
	return false;
}

// TODO consider doing this evaluation early, it should be possible.
static bool sema_analyse_attributes_inner(SemaContext *context, ResolvedAttrData *attr_data_ref, Decl *decl, Attr **attrs, AttributeDomain domain,
                                          Decl *top, bool *erase_decl)
{
	// Detect cycles of the type @Foo = @BarCyclic, @BarCyclic = @BarCyclic
	if (context->macro_call_depth > 1024)
	{
		if (!top) top = decl;
		RETURN_SEMA_ERROR(top, "Recursive declaration of attribute '%s'.", top->name);
	}

	// Walk through all of the attributes.
	FOREACH(Attr *, attr, attrs)
	{
		if (attr->is_custom)
		{
			if (!sema_analyse_custom_attribute(context, attr_data_ref, decl, attr, domain, top, erase_decl)) return false;
		}
		else
		{
			// The simple case, we have a built in attribute:
			if (!sema_analyse_attribute(context, attr_data_ref, decl, attr, domain, erase_decl)) return false;
		}
		if (*erase_decl) return true;
	}
	return true;
}

bool sema_analyse_attributes(SemaContext *context, Decl *decl, Attr **attrs, AttributeDomain domain, bool *erase_decl)
{
	if (decl->resolved_attributes) return true;
	ResolvedAttrData data = { .tags = NULL, .overload = INVALID_SPAN };
	if (!sema_analyse_attributes_inner(context, &data, decl, attrs, domain, NULL, erase_decl)) return false;
	if (*erase_decl) return true;
	decl->resolved_attributes = true;
	if (data.tags || data.deprecated || data.links || data.section || data.overload.row || data.wasm_module )
	{
		ResolvedAttrData *copy = MALLOCS(ResolvedAttrData);
		*copy = data;
		decl->attrs_resolved = copy;
	}
	else
	{
		decl->attrs_resolved = NULL;
	}
	return true;
}

bool sema_analyse_optional_returns(SemaContext *context, Decl *contracts)
{
	if (contracts->resolve_status != RESOLVE_NOT_DONE) return true;
	contracts->resolve_status = RESOLVE_RUNNING;

	Decl **result = NULL;
	FOREACH(Expr *, expr, contracts->contracts_decl.opt_returns)
	{
		if (expr->expr_kind == EXPR_RETHROW)
		{
			Expr *inner = expr->rethrow_expr.inner;
			if (!sema_analyse_expr(context, inner)) goto FAIL;
			Decl *decl;
			switch (inner->expr_kind)
			{
				case EXPR_IDENTIFIER:
					decl = inner->ident_expr;
					break;
				case EXPR_TYPEINFO:
				{
					Type *type = inner->type_expr->type;
					if (type->type_kind != TYPE_ALIAS) goto IS_FAULT;
					decl = type->decl;
					ASSERT(decl->decl_kind == DECL_TYPE_ALIAS);
					if (!decl->type_alias_decl.is_func) goto IS_FAULT;
					decl = decl->type_alias_decl.decl;
					break;
				}
				default:
					goto IS_FAULT;;
			}
			decl = decl_flatten(decl);
			if (decl->decl_kind != DECL_FNTYPE && decl->decl_kind != DECL_FUNC) goto IS_FAULT;
			if (!sema_analyse_decl(context, decl)) goto FAIL;
			DeclId contract_id = decl->decl_kind == DECL_FNTYPE ? decl->fntype_decl.docs : decl->func_decl.docs;
			if (!contract_id) continue;
			Decl *sub_contracts = declptr(contract_id);
			if (!sub_contracts->contracts_decl.opt_returns) continue;
			switch (sub_contracts->resolve_status)
			{
				case RESOLVE_DONE:
				case RESOLVE_RUNNING:
					break;
				case RESOLVE_NOT_DONE:
					if (!sema_analyse_optional_returns(context, sub_contracts)) goto FAIL;
			}
			FOREACH (Decl *, decl, sub_contracts->contracts_decl.opt_returns_resolved) vec_add(result, decl);
			continue;
		}
IS_FAULT:;
		if (!sema_analyse_expr_rvalue(context, expr)) goto FAIL;
		if (expr->type->canonical != type_fault)
		{
			SEMA_ERROR(expr, "Expected a fault here.");
			goto FAIL;
		}
		if (!expr_is_const_fault(expr))
		{
			SEMA_ERROR(expr, "A constant fault is required.");
			goto FAIL;
		}
		Decl *decl = expr->const_expr.fault;
		if (!decl)
		{
			SEMA_ERROR(expr, "A non-null fault is required.");
			goto FAIL;
		}
		vec_add(result, decl);
	}
	contracts->resolve_status = RESOLVE_DONE;
	contracts->contracts_decl.opt_returns_resolved = result;
	return true;
FAIL:
	contracts->resolve_status = RESOLVE_DONE;
	contracts->contracts_decl.opt_returns_resolved = NULL;
	return false;
}

static inline bool sema_analyse_doc_header(SemaContext *context, DeclId docs,
                                           Decl **params, Decl **extra_params, bool *pure_ref, bool is_raw_vaarg)
{
	bool va_param_found = false;
	if (!docs) return true;
	Decl *contracts = declptr(docs);
	if (!sema_analyse_optional_returns(context, contracts)) return false;
	if (contracts->contracts_decl.pure) *pure_ref = true;

	FOREACH_REF(ContractParam, param_contract, contracts->contracts_decl.params)
	{
		if (!param_contract->name)
		{
			if (va_param_found)
			{
				RETURN_SEMA_ERROR_AT(param_contract->span, "The '...' @param may not be repeated.");
			}
			if (param_contract->modifier != INOUT_ANY)
			{
				RETURN_SEMA_ERROR(param_contract, "'...' @params may not have any in-out modifiers.");
			}
			if (!is_raw_vaarg)
			{
				RETURN_SEMA_ERROR(param_contract, "'...' @params are only allowed macros and functions with a '...' parameter.");
			}
			va_param_found = true;
			continue;
		}
		Decl *param = NULL;
		FOREACH(Decl *, other_param, params)
		{
			param = other_param;
			if (param && param->name == param_contract->name) goto NEXT;
		}
		FOREACH(Decl *, extra, extra_params)
		{
			param = extra;
			if (param && param->name == param_contract->name) goto NEXT;
		}
		RETURN_SEMA_ERROR(param_contract, "There is no parameter '%s', did you misspell it?", param_contract->name);
	NEXT:;
		Type *type = param->type;
		if (type) type = type_flatten(type);
		bool may_be_pointer = !type || type_is_pointer(type) || type_is_any_raw(type);
		if (param_contract->by_ref)
		{
			if (!may_be_pointer)
			{
				RETURN_SEMA_ERROR(param_contract, "'&' can only be added to pointer type parameters.");
			}
			param->var.not_null = true;
		}
		switch (param_contract->modifier)
		{
			case INOUT_ANY:
				goto ADDED;
			case INOUT_IN:
				param->var.in_param = true;
				break;
			case INOUT_OUT:
				param->var.out_param = true;
				break;
			case INOUT_INOUT:
				param->var.out_param = true;
				param->var.in_param = true;
				break;
		}
		if (!may_be_pointer && type->type_kind != TYPE_SLICE)
		{
			RETURN_SEMA_ERROR(param_contract, "'in', 'out' and 'inout' may only be added to pointers and slices.");
		}
ADDED:;
	}
	return true;
}

static inline MainType sema_find_main_type(SemaContext *context, Signature *sig, bool is_winmain)
{
	Decl **params = sig->params;
	unsigned param_count = vec_size(params);
	bool is_win32 = compiler.platform.os == OS_TYPE_WIN32;
	Type *arg_type, *arg_type2;
	switch (param_count)
	{
		case 0:
			return MAIN_TYPE_NO_ARGS;
		case 1:
			arg_type = type_flatten(params[0]->type);
			if (arg_type == type_get_slice(type_string)) return MAIN_TYPE_ARGS;
			SEMA_ERROR(params[0], "Expected a parameter of type 'String[]'.");
			return MAIN_TYPE_ERROR;
		case 2:
			arg_type = type_flatten(params[0]->type);
			arg_type2 = type_flatten(params[1]->type);
			if (arg_type != type_cint)
			{
				SEMA_ERROR(params[0],
						   "Expected a parameter of type %s for a C-style main.",
						   type_quoted_error_string(type_cint));
				return MAIN_TYPE_ERROR;
			}
			if (arg_type2->type_kind != TYPE_POINTER
				|| type_flatten(arg_type2->pointer) != type_get_ptr(type_char))
			{
				SEMA_ERROR(params[1], "Expected a parameter of type 'char**' for a C-style main.");
				return MAIN_TYPE_ERROR;
			}
			if (is_winmain)
			{
				SEMA_ERROR(params[0], "For '@winmain' functions, C-style 'main' with argc + argv isn't valid. It compiles if you remove the '@winmain' attribute.");
				return MAIN_TYPE_ERROR;
			}
			return MAIN_TYPE_RAW;
		case 4:
			if (!is_win32 || !is_winmain) break;
			for (int i = 0; i < 2; i++)
			{
				arg_type = type_flatten(params[i]->type);
				if (arg_type != type_voidptr)
				{
					SEMA_ERROR(params[i], "Expected a parameter of type 'void*' (Win32_HINSTANCE)");
					return MAIN_TYPE_ERROR;
				}
			}
			arg_type2 = type_flatten(params[2]->type);
			if (arg_type2 != type_get_slice(type_string))
			{
				SEMA_ERROR(params[1], "Expected a parameter of type 'String[]'.");
				return MAIN_TYPE_ERROR;
			}
			if (type_flatten(params[3]->type) != type_cint)
			{
				SEMA_ERROR(params[3], "Expected a parameter of type %s for the 'showCmd' parameter.",
						   type_quoted_error_string(type_cint));
				return MAIN_TYPE_ERROR;
			}
			if (!is_win32)
			{
				SEMA_ERROR(params[0], "'main(Win32_HINSTANCE, Win32_HINSTANCE, String[], int) is only valid for Windows.");
				return MAIN_TYPE_ERROR;
			}
			return MAIN_TYPE_WIN;
		default:
			break;
	}
	SEMA_ERROR(params[0], (is_win32 & is_winmain)
		? "Expected zero, 1 or 4 parameters for main."
		: "Expected zero or 1 parameters for main.");
	return MAIN_TYPE_ERROR;

}

Decl *sema_create_runner_main(SemaContext *context, Decl *decl)
{
	bool is_win32 = compiler.platform.os == OS_TYPE_WIN32;
	Decl *function = decl_new(DECL_FUNC, NULL, decl->span);
	function->is_export = true;
	function->has_extname = true;
	function->extname = kw_mainstub;
	function->name = kw_mainstub;
	function->unit = decl->unit;

	// Pick wWinMain, main or wmain
	Decl *params[4] = { NULL, NULL, NULL, NULL };
	int param_count;
	if (is_win32)
	{
		function->extname = kw_wmain;
		params[0] = decl_new_generated_var(type_cint, VARDECL_PARAM, decl->span);
		params[1] = decl_new_generated_var(type_get_ptr(type_get_ptr(type_ushort)), VARDECL_PARAM, decl->span);
		param_count = 2;
	}
	else
	{
		function->extname = kw_main;
		params[0] = decl_new_generated_var(type_cint, VARDECL_PARAM, decl->span);
		params[1] = decl_new_generated_var(type_get_ptr(type_get_ptr(type_char)), VARDECL_PARAM, decl->span);
		param_count = 2;
	}

	function->has_extname = true;
	function->func_decl.signature.rtype = type_infoid(type_info_new_base(type_cint, decl->span));
	function->func_decl.signature.vararg_index = param_count;
	Decl **main_params = NULL;
	for (int i = 0; i < param_count; i++) vec_add(main_params, params[i]);
	function->func_decl.signature.params = main_params;
	Ast *body = new_ast(AST_COMPOUND_STMT, decl->span);
	AstId *next = &body->compound_stmt.first_stmt;
	Ast *ret_stmt = new_ast(AST_RETURN_STMT, decl->span);
	const char *kw_main_invoker = symtab_preset(is_win32 ? "@_wmain_runner" : "@_main_runner", TOKEN_AT_IDENT);
	Decl *d = sema_find_symbol(context, kw_main_invoker);
	if (!d)
	{
		SEMA_ERROR(decl, "Missing main forwarding function '%s'.", kw_main_invoker);
		return poisoned_decl;
	}
	Expr *invoker = expr_new(EXPR_IDENTIFIER, decl->span);
	expr_resolve_ident(invoker, d);
	Expr *call = expr_new(EXPR_CALL, decl->span);
	Expr *fn_ref = expr_variable(decl);
	vec_add(call->call_expr.arguments, fn_ref);
	for (int i = 0; i < param_count; i++)
	{
		Expr *arg = expr_variable(params[i]);
		vec_add(call->call_expr.arguments, arg);
	}
	call->call_expr.function = exprid(invoker);
	for (int i = 0; i < param_count; i++) params[i]->resolve_status = RESOLVE_NOT_DONE;
	ast_append(&next, ret_stmt);
	ret_stmt->return_stmt.expr = call;
	function->func_decl.body = astid(body);
	function->is_synthetic = true;
	return function;
}

typedef enum
{
	MAIN_SUBTYPE_NORMAL,
	MAIN_SUBTYPE_WINMAIN,
	MAIN_SUBTYPE_WMAIN
} MainSubType;

static inline Decl *sema_create_synthetic_main(SemaContext *context, Decl *decl, MainType main, MainSubType sub_type)
{
	Decl *function = decl_new(DECL_FUNC, NULL, decl->span);
	function->is_export = true;
	function->has_extname = true;
	function->extname = kw_mainstub;
	function->name = kw_mainstub;
	function->unit = decl->unit;

	// Pick wWinMain, main or wmain
	Decl *params[4] = { NULL, NULL, NULL, NULL };
	int param_count;
	switch (sub_type)
	{
		case MAIN_SUBTYPE_WINMAIN:
			function->extname = kw_winmain;
			params[0] = decl_new_generated_var(type_voidptr, VARDECL_PARAM, decl->span);
			params[1]  = decl_new_generated_var(type_voidptr, VARDECL_PARAM, decl->span);
			params[2]  = decl_new_generated_var(type_get_ptr(type_ushort), VARDECL_PARAM, decl->span);
			params[3]  = decl_new_generated_var(type_cint, VARDECL_PARAM, decl->span);
			param_count = 4;
			break;
		case MAIN_SUBTYPE_WMAIN:
			function->extname = kw_wmain;
			params[0] = decl_new_generated_var(type_cint, VARDECL_PARAM, decl->span);
			params[1] = decl_new_generated_var(type_get_ptr(type_get_ptr(type_ushort)), VARDECL_PARAM, decl->span);
			param_count = 2;
			break;
		case MAIN_SUBTYPE_NORMAL:
			function->extname = kw_main;
			params[0] = decl_new_generated_var(type_cint, VARDECL_PARAM, decl->span);
			params[1] = decl_new_generated_var(type_get_ptr(type_get_ptr(type_char)), VARDECL_PARAM, decl->span);
			param_count = 2;
			break;
		default:
			UNREACHABLE;
	}

	function->has_extname = true;
	function->func_decl.signature.rtype = type_infoid(type_info_new_base(type_cint, decl->span));
	function->func_decl.signature.vararg_index = param_count;
	Decl **main_params = NULL;
	for (int i = 0; i < param_count; i++) vec_add(main_params, params[i]);
	function->func_decl.signature.params = main_params;
	Ast *body = new_ast(AST_COMPOUND_STMT, decl->span);
	AstId *next = &body->compound_stmt.first_stmt;
	Ast *ret_stmt = new_ast(AST_RETURN_STMT, decl->span);
	const char *main_invoker;
	switch (main)
	{
		case MAIN_TYPE_ARGS:
			switch (sub_type)
			{
				case MAIN_SUBTYPE_WINMAIN: main_invoker = "@win_main_args"; break;
				case MAIN_SUBTYPE_WMAIN: main_invoker = "@wmain_main"; break;
				default: main_invoker = "@main_args"; break;
			}
			break;
		case MAIN_TYPE_NO_ARGS:
			switch (sub_type)
			{
				case MAIN_SUBTYPE_WINMAIN: main_invoker = "@win_main_no_args"; break;
				case MAIN_SUBTYPE_WMAIN: main_invoker = "@wmain_main_no_args"; break;
				default: main_invoker = "@main_no_args"; break;
			}
			break;
		case MAIN_TYPE_WIN:
			ASSERT(sub_type == MAIN_SUBTYPE_WINMAIN);
			main_invoker = "@win_main";
			break;
		default:
			UNREACHABLE
	}
	const char *kw_main_invoker = symtab_preset(main_invoker, TOKEN_AT_IDENT);
	Decl *d = sema_find_symbol(context, kw_main_invoker);
	if (!d)
	{
		SEMA_ERROR(decl, "Missing main forwarding function '%s'.", kw_main_invoker);
		return poisoned_decl;
	}
	Expr *invoker = expr_new(EXPR_IDENTIFIER, decl->span);
	expr_resolve_ident(invoker, d);
	Expr *call = expr_new(EXPR_CALL, decl->span);
	Expr *fn_ref = expr_variable(decl);
	vec_add(call->call_expr.arguments, fn_ref);
	for (int i = 0; i < param_count; i++)
	{
		Expr *arg = expr_variable(params[i]);
		vec_add(call->call_expr.arguments, arg);
	}
	call->call_expr.function = exprid(invoker);
	for (int i = 0; i < param_count; i++) params[i]->resolve_status = RESOLVE_NOT_DONE;
	ast_append(&next, ret_stmt);
	ret_stmt->return_stmt.expr = call;
	function->func_decl.body = astid(body);
	function->is_synthetic = true;
	return function;
}

static inline bool sema_analyse_main_function(SemaContext *context, Decl *decl)
{
	ASSERT(decl != context->unit->main_function);
	bool is_win32 = compiler.platform.os == OS_TYPE_WIN32;
	MainSubType sub_type = decl->func_decl.attr_winmain && is_win32 ? MAIN_SUBTYPE_WINMAIN : MAIN_SUBTYPE_NORMAL;
	if (decl->visibility != VISIBLE_PUBLIC)
	{
		RETURN_SEMA_ERROR(decl, "A main function must be public.");
	}
	Signature *signature = &decl->func_decl.signature;
	TypeInfo *rtype_info = type_infoptr(signature->rtype);
	Type *rtype = rtype_info->type;
	bool is_int_return = true;
	if (type_is_optional(rtype))
	{
		RETURN_SEMA_ERROR(rtype_info, "The return type of 'main' cannot be an optional.");
	}

	if (type_is_void(rtype)) is_int_return = false;

	if (is_int_return && type_flatten(rtype) != type_cint)
	{
		RETURN_SEMA_ERROR(rtype_info, "Expected a return type of 'void' or %s.", type_quoted_error_string(type_cint));
	}
	MainType type = sema_find_main_type(context, signature, sub_type == MAIN_SUBTYPE_WINMAIN);
	if (type == MAIN_TYPE_ERROR) return false;
	if (compiler.build.type == TARGET_TYPE_TEST || compiler.build.type == TARGET_TYPE_BENCHMARK) return true;
	Decl *function;
	if (compiler.build.no_entry)
	{
		function = decl;
		goto REGISTER_MAIN;
	}
	if (type == MAIN_TYPE_RAW && !is_int_return)
	{
		RETURN_SEMA_ERROR(rtype_info, "Int return is required for a C style main.");
	}

	if ((type == MAIN_TYPE_RAW || (type == MAIN_TYPE_NO_ARGS && !is_win32)) && is_int_return)
	{
		// Int return is pass-through at the moment.
		decl->is_export = true;
		decl->has_extname = true;
		decl->extname = kw_main;
		function = decl;
		goto REGISTER_MAIN;
	}
	if (is_win32 && sub_type != MAIN_SUBTYPE_WINMAIN) sub_type = MAIN_SUBTYPE_WMAIN;
	compiler.build.win.use_win_subsystem = sub_type == MAIN_SUBTYPE_WINMAIN;
	function = sema_create_synthetic_main(context, decl, type, sub_type);
	if (!decl_ok(function)) return false;
REGISTER_MAIN:
	context->unit->main_function = function;
	if (compiler.context.main)
	{
		SEMA_ERROR(function, "Duplicate main functions found.");
		SEMA_NOTE(compiler.context.main, "The first one was found here.");
		return false;
	}
	compiler.context.main = function;
	return true;
}


static inline bool sema_analyse_func(SemaContext *context, Decl *decl, bool *erase_decl)
{
	DEBUG_LOG(">>> Analyse function [%s] in %s", decl_safe_name(decl), context->unit->file->full_path);

	bool is_interface_method = decl->func_decl.attr_interface_method;
	if (!sema_analyse_func_macro(context, decl, is_interface_method ? ATTR_INTERFACE_METHOD : ATTR_FUNC, erase_decl)) return false;
	if (*erase_decl) return true;

	bool is_test = decl->func_decl.attr_test;
	bool is_benchmark = decl->func_decl.attr_benchmark;
	bool is_init_finalizer = decl->func_decl.attr_init || decl->func_decl.attr_finalizer;
	Signature *sig = &decl->func_decl.signature;
	if (is_init_finalizer && (is_test || is_benchmark))
	{
		RETURN_SEMA_ERROR(decl, "Test and benchmark functions may not also be marked '@init' or '@finalizer'.");
	}
	if (is_test || is_benchmark || is_init_finalizer)
	{
		ASSERT(!is_interface_method);
		unsigned params = vec_size(sig->params);
		if (params)
		{
			SourceSpan span = sig->params[0] ? sig->params[0]->span : decl->span;
			RETURN_SEMA_ERROR_AT(span, "%s functions may not take any parameters.",
							  is_init_finalizer ? "'@init' and '@finalizer'" : "'@test' and '@benchmark'");
		}
		TypeInfo *rtype_info = type_infoptr(sig->rtype);
		if (!sema_resolve_type_info(context, rtype_info, RESOLVE_TYPE_DEFAULT)) return false;
		Type *rtype = rtype_info->type;
		if (is_init_finalizer)
		{
			if (!type_is_void(rtype))
			{
				RETURN_SEMA_ERROR(rtype_info, "'@init' and '@finalizer' functions may only return 'void'.");
			}
			goto CHECK_DONE;
		}
		if (!type_is_void(rtype))
		{
			RETURN_SEMA_ERROR(rtype_info, "'@test' and '@benchmark' function may only return 'void' you can allow 'void!' by passing in '--old-test-bench=yes'.");
		}
	}
CHECK_DONE:
	decl->type = type_new_func(decl, sig);
	if (!sema_analyse_function_signature(context, decl, type_infoptrzero(decl->func_decl.type_parent), sig->abi, sig)) return decl_poison(decl);
	TypeInfo *rtype_info = type_infoptr(sig->rtype);
	ASSERT(rtype_info);
	Type *rtype = rtype_info->type->canonical;
	if (sig->attrs.nodiscard)
	{
		if (type_is_void(rtype))
		{
			RETURN_SEMA_ERROR(rtype_info, "@nodiscard cannot be used on functions returning 'void'.");
		}
	}
	if (sig->attrs.maydiscard && !type_is_optional(rtype))
	{
		RETURN_SEMA_ERROR(rtype_info, "@maydiscard can only be used on functions returning optional values.");
	}

	if (decl->func_decl.type_parent)
	{
		if (!sema_analyse_method(context, decl)) return false;
	}
	else if (!is_interface_method)
	{
		if (decl->func_decl.attr_dynamic) RETURN_SEMA_ERROR(decl, "Only methods may implement interfaces.");
		if (decl->name == kw_main)
		{
			if (is_test || is_benchmark)
			{
				RETURN_SEMA_ERROR(decl, "The main function may not be annotated %s.", is_test ? "@test" : "@benchmark");
			}
			if (!sema_analyse_main_function(context, decl)) return false;
		}
	}

	// Do we have fn void any.foo(void*) { ... }?
	if (!decl->func_decl.body && !decl->is_extern && !decl->unit->is_interface_file && !is_interface_method)
	{
		RETURN_SEMA_ERROR(decl, "Expected a function body, if you want to declare an extern function use "
								"'extern' or place it in an .c3i file.");
	}
	bool pure = false;

	if (!sema_analyse_doc_header(context, decl->func_decl.docs, decl->func_decl.signature.params, NULL,
	                             &pure, sig->variadic == VARIADIC_RAW)) return false;
	decl->func_decl.signature.attrs.is_pure = pure;
	if (!sema_set_alloca_alignment(context, decl->type, &decl->alignment)) return false;
	DEBUG_LOG("<<< Function analysis of [%s] successful.", decl_safe_name(decl));
	return true;
}

static inline bool sema_is_valid_method_param(SemaContext *context, Decl *param, Type *parent_type, bool is_dynamic)
{
	ASSERT(parent_type->canonical == parent_type && "Expected already the canonical version.");
	Type *param_type = param->type;

	if (!param_type) goto ERROR;
	param_type = param_type->canonical;

	if (is_dynamic)
	{
		if (param_type->type_kind != TYPE_POINTER || param_type->pointer != parent_type)
		{
			RETURN_SEMA_ERROR(type_infoptr(param->var.type_info),
							  "The first parameter must always be a pointer for '@dynamic' methods, "
							  "so please change its type to %s if possible.",
							  type_quoted_error_string(type_get_ptr(parent_type)));
		}
		return true;
	}

	// 1. Same type ok!
	if (param_type == parent_type) return true;

	switch (param_type->type_kind)
	{
		case TYPE_POINTER:
			if (param_type->pointer == parent_type) return true;
			break;
		default:
			break;
	}
ERROR:
	RETURN_SEMA_ERROR(type_infoptr(param->var.type_info),
					  "The first parameter of a method must be of type %s or %s.",
					  type_quoted_error_string(parent_type),
	                  type_quoted_error_string(type_get_ptr(parent_type)));
}

static bool sema_analyse_macro_method(SemaContext *context, Decl *decl)
{
	// Resolve the type of the method.
	TypeInfo *parent_type_info = type_infoptr(decl->func_decl.type_parent);
	ASSERT(parent_type_info->resolve_status == RESOLVE_DONE);
	Type *parent_type = parent_type_info->type->canonical;

	// Check the first argument.
	Decl *first_param = decl->func_decl.signature.params[0];
	if (!first_param)
	{
		RETURN_SEMA_ERROR(decl, "The first parameter to this method must be of type %s or %s.", type_quoted_error_string(parent_type),
		                  type_quoted_error_string(type_get_ptr(parent_type)));
	}

	if (!sema_is_valid_method_param(context, first_param, parent_type, false)) return false;

	if (first_param->var.kind != VARDECL_PARAM_EXPR && first_param->var.kind != VARDECL_PARAM_CT && first_param->var.kind != VARDECL_PARAM)
	{
		RETURN_SEMA_ERROR(first_param, "The first parameter must be a compile time, regular or ref (&) type.");
	}
	// Is it an operator?
	if (decl->func_decl.operator)
	{
		decl->resolve_status = RESOLVE_DONE;
		if (!sema_analyse_operator_method(context, parent_type, decl)) return false;
	}

	return true;
}

INLINE bool sema_analyse_macro_body(SemaContext *context, Decl **body_parameters)
{
	unsigned body_param_count = vec_size(body_parameters);
	for (unsigned i = 0; i < body_param_count; i++)
	{
		ASSERT(body_parameters);
		Decl *param = body_parameters[i];
		ASSERT(param);
		param->resolve_status = RESOLVE_RUNNING;
		ASSERT(param->decl_kind == DECL_VAR);
		TypeInfo *type_info = type_infoptrzero(param->var.type_info);
		VarDeclKind kind = param->var.kind;
		if (param->var.init_expr)
		{
			RETURN_SEMA_ERROR(param->var.init_expr, "Body parameters cannot use default values.");
		}
		switch (kind)
		{
			case VARDECL_PARAM:
			case VARDECL_PARAM_EXPR:
			case VARDECL_PARAM_CT:
			case VARDECL_PARAM_CT_TYPE:
				if (!type_info) break;
				if (!sema_resolve_type_info(context, type_info, RESOLVE_TYPE_DEFAULT)) return false;
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
		if (!sema_check_param_uniqueness_and_type(context, body_parameters, param, i)) return false;
		param->resolve_status = RESOLVE_DONE;
	}
	return true;
}

static inline bool sema_check_body_const(SemaContext *context, Ast *body)
{
	while (body)
	{
		switch (body->ast_kind)
		{
			case AST_CT_ASSERT:
			case AST_CT_ECHO_STMT:
			case AST_CT_FOREACH_STMT:
			case AST_CT_FOR_STMT:
			case AST_CT_IF_STMT:
			case AST_CT_ELSE_STMT:
			case AST_CT_SWITCH_STMT:
			case AST_CT_TYPE_ASSIGN_STMT:
			case AST_DECLARE_STMT:
			case AST_DECLS_STMT:
			case AST_NOP_STMT:
				body = astptrzero(body->next);
				continue;
			case AST_CT_COMPOUND_STMT:
				if (!sema_check_body_const(context, body)) return false;
				body = astptrzero(body->next);
				continue;
			case AST_RETURN_STMT:
				if (!body->return_stmt.expr) RETURN_SEMA_ERROR(body, "The 'return' in an `@const` must provide a value, e.g. 'return Foo.typeid;'");
				if (body->next) RETURN_SEMA_ERROR(body, "There should not be any statements after 'return'.");
				body = NULL;
				continue;
			case AST_EXPR_STMT:
				// For some cases we KNOW it's not correct.
				switch (body->expr_stmt->expr_kind)
				{
					case EXPR_IDENTIFIER:
					case EXPR_UNRESOLVED_IDENTIFIER:
					case EXPR_LAMBDA:
					case EXPR_FORCE_UNWRAP:
					case EXPR_ASM:
					case EXPR_TERNARY:
					case EXPR_RETHROW:
						break;
					default:
						body = astptrzero(body->next);
						continue;
				}
				FALLTHROUGH;
			case AST_POISONED:
			case AST_ASM_STMT:
			case AST_ASM_LABEL:
			case AST_ASM_BLOCK_STMT:
			case AST_ASSERT_STMT:
			case AST_BREAK_STMT:
			case AST_CASE_STMT:
			case AST_COMPOUND_STMT:
			case AST_CONTINUE_STMT:
			case AST_DEFAULT_STMT:
			case AST_DEFER_STMT:
			case AST_FOR_STMT:
			case AST_FOREACH_STMT:
			case AST_IF_STMT:
			case AST_BLOCK_EXIT_STMT:
			case AST_SWITCH_STMT:
			case AST_NEXTCASE_STMT:
				RETURN_SEMA_ERROR(body, "Only 'return' and compile time statements are allowed in an '@const' macro.");
		}
		UNREACHABLE
	}
	return true;
}
static inline bool sema_analyse_macro(SemaContext *context, Decl *decl, bool *erase_decl)
{
	decl->func_decl.unit = context->unit;

	if (!sema_analyse_func_macro(context, decl, ATTR_MACRO, erase_decl)) return false;
	if (*erase_decl) return true;

	Signature *sig = &decl->func_decl.signature;
	if (!sema_analyse_signature(context, sig, type_infoptrzero(decl->func_decl.type_parent), decl)) return false;

	DeclId body_param = decl->func_decl.body_param;
	if (!decl->func_decl.signature.is_at_macro && body_param && !sig->is_safemacro)
	{
		RETURN_SEMA_ERROR(decl, "Names of macros with a trailing body must start with '@'.");
	}
	Decl **body_parameters = body_param ? declptr(body_param)->body_params : NULL;
	if (!sema_analyse_macro_body(context, body_parameters)) return false;
	bool pure = false;
	if (!sema_analyse_doc_header(context, decl->func_decl.docs, sig->params, body_parameters,
	                             &pure, sig->variadic == VARIADIC_RAW)) return false;
	if (decl->func_decl.type_parent)
	{
		if (!sema_analyse_macro_method(context, decl)) return false;
	}
	bool always_const = sig->attrs.always_const;
	// Sanity check "always const"
	if (always_const)
	{
		if (typeget(sig->rtype) == type_void) RETURN_SEMA_ERROR(decl, "'@const' macros may not return 'void', they should always return a constant value.");
		if (body_parameters) RETURN_SEMA_ERROR(decl, "'@const' macros cannot have body parameters.");
		Ast *body = astptr(decl->func_decl.body);
		ASSERT(body->ast_kind == AST_COMPOUND_STMT);
		body = astptrzero(body->compound_stmt.first_stmt);
		if (!body) RETURN_SEMA_ERROR(decl, "'@const' macros cannot have an empty body.");
		sema_check_body_const(context, body);
	}
	decl->type = type_void;
	return true;
}


static bool sema_analyse_attributes_for_var(SemaContext *context, Decl *decl, bool *erase_decl)
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
		case VARDECL_PARAM:
		case VARDECL_PARAM_CT_TYPE:
		case VARDECL_PARAM_CT:
			domain = ATTR_PARAM;
			break;
		default:
			domain = ATTR_LOCAL;
			break;
	}
	if (!sema_analyse_attributes(context, decl, decl->attributes, domain, erase_decl)) return decl_poison(decl);
	return true;
}
static bool sema_type_is_valid_size(SemaContext *context, Type *type, SourceSpan span)
{
	Int128 size = i128_from_unsigned(1);
RETRY:
	if (size.high || size.low > (uint64_t)MAX_TYPE_SIZE)
	{
		RETURN_SEMA_ERROR_AT(span, "This type would exceed max type size of %u GB.", MAX_TYPE_SIZE >> 30);
	}
	switch (type->type_kind)
	{
		case TYPE_BITSTRUCT:
			ASSERT(type->decl->resolve_status == RESOLVE_DONE);
			type = type->decl->strukt.container_type->type;
			goto RETRY;
		case TYPE_TYPEDEF:
			type = type->decl->distinct->type;
			goto RETRY;
		case TYPE_ALIAS:
			type = type->canonical;
			goto RETRY;
		case CT_TYPES:
		case TYPE_FUNC_RAW:
		case TYPE_FLEXIBLE_ARRAY:
			return true;
		case TYPE_OPTIONAL:
			type = type->optional;
			goto RETRY;
		case TYPE_VOID:
			return true;
		case TYPE_BOOL:
		case TYPE_TYPEID:
		case ALL_INTS:
		case ALL_FLOATS:
		case TYPE_ANYFAULT:
		case TYPE_INTERFACE:
		case TYPE_ANY:
		case TYPE_FUNC_PTR:
		case TYPE_POINTER:
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_ENUM:
		case TYPE_CONST_ENUM:
		case TYPE_SLICE:
			size = i128_mult64(size, type_size(type));
			break;
		case VECTORS:
		case TYPE_ARRAY:
			size = i128_mult64(size, type->array.len);
			type = type->array.base;
			goto RETRY;
	}
	if (size.high || size.low > (uint64_t)MAX_TYPE_SIZE)
	{
		RETURN_SEMA_ERROR_AT(span, "This type would exceed max type size of %u GB.", MAX_TYPE_SIZE >> 30);
	}
	return true;
}
static bool sema_analyse_variable_type(SemaContext *context, Type *type, SourceSpan span)
{
	switch (sema_resolve_storage_type(context, type))
	{
		case STORAGE_ERROR:
			return false;
		case STORAGE_NORMAL:
			return true;
		case STORAGE_VOID:
		case STORAGE_WILDCARD:
			if (type_is_optional(type))
			{
				RETURN_SEMA_ERROR_AT(span, "The use of %s as a variable type is not permitted, "
				                           "catch the error using 'if (catch err = foo) { ... }',"
										   " or use '@catch(foo)' to convert it to a 'fault'.",
				                     type_quoted_error_string(type));
			}
			RETURN_SEMA_ERROR_AT(span, "The use of %s as a variable type is not permitted.", type_quoted_error_string(type));
		case STORAGE_COMPILE_TIME:
			RETURN_SEMA_ERROR_AT(span, "The variable cannot be %s which is an compile time type.",
			                     type_invalid_storage_type_name(type));
		case STORAGE_UNKNOWN:
			RETURN_SEMA_ERROR_AT(span, "%s has unknown size, and so it cannot be a variable type.",
			                     type_quoted_error_string(type));
	}
	UNREACHABLE
}

/**
 * Analyse $foo and $Foo variables.
 */
bool sema_analyse_var_decl_ct(SemaContext *context, Decl *decl, bool *check_defined)
{
	Expr *init;
	ASSERT(decl->decl_kind == DECL_VAR && "Should only be called on variables.");

	// Grab the optional type_info.
	TypeInfo *type_info = vartype(decl);
	switch (decl->var.kind)
	{
		case VARDECL_LOCAL_CT_TYPE:
			// Locally declared compile time type.
			if (type_info)
			{
				SEMA_ERROR(type_info, "Compile time type variables may not have a type.");
				goto FAIL;
			}
			// Check initialization.
			if ((init = decl->var.init_expr))
			{
				// Try to fold any constant into an lvalue.
				if (!sema_analyse_expr(context, init)) goto FAIL;

				if (init->expr_kind == EXPR_TYPEINFO)
				{
					Type *type = init->type_expr->type;
					expr_rewrite_const_typeid(init, type);
				}
				// If this isn't a type, it's an error.
				if (!expr_is_const_typeid(init))
				{
					if (check_defined) goto FAIL_CHECK;
					SEMA_ERROR(decl->var.init_expr, "Expected a type assigned to %s.", decl->name);
					goto FAIL;
				}
			}
			// Otherwise we're done.
			break;
		case VARDECL_LOCAL_CT:
			// Resolve the type if it's present.
			if (type_info)
			{
				if (!sema_resolve_type_info(context, type_info, RESOLVE_TYPE_ALLOW_INFER)) goto FAIL;
				// Set the type of the declaration.
				decl->type = type_info->type;
				init = decl->var.init_expr;
				// If there is no init, set it to zero.
				if (!init)
				{
					if (type_is_inferred(decl->type))
					{
						if (check_defined) goto FAIL_CHECK;
						SEMA_ERROR(type_info, "No size could be inferred.");
						goto FAIL;
					}
					switch (sema_resolve_storage_type(context, decl->type))
					{
						case STORAGE_NORMAL:
						case STORAGE_COMPILE_TIME:
							break;
						case STORAGE_ERROR:
							goto FAIL;
						case STORAGE_VOID:
						case STORAGE_WILDCARD:
						case STORAGE_UNKNOWN:
							SEMA_ERROR(type_info, "Expected a runtime or compile time type with a well-defined zero value.");
							goto FAIL;
					}
					decl->var.init_expr = init = expr_new(EXPR_POISONED, decl->span);
					expr_rewrite_to_const_zero(init, type_no_optional(decl->type));
				}

				// Analyse the expression.
				if (!sema_analyse_expr_rhs(context, decl->type, init, false, NULL, false)) goto FAIL;

				if (type_is_inferred(decl->type))
				{
					decl->type = init->type;
				}
				// Check that it is constant.
				if (!expr_is_runtime_const(init))
				{
					if (check_defined) goto FAIL_CHECK;
					SEMA_ERROR(init, "Expected a constant expression assigned to %s.", decl->name);
					goto FAIL;
				}
				break;
			}
			// If we don't have a type, resolve the expression.
			if ((init = decl->var.init_expr))
			{
				if (init->expr_kind == EXPR_TYPEINFO)
				{
					if (check_defined) goto FAIL_CHECK;
					SEMA_ERROR(init, "You can't assign a type to a regular compile time variable like '%s', but it would be allowed if the variable was a compile time type variable. Such a variable needs to have a type-like name, e.g. '$MyType'.", decl->name);
					goto FAIL;
				}
				if (!sema_analyse_expr_rvalue(context, init)) goto FAIL;
				// Check it is constant.
				if (!expr_is_runtime_const(init))
				{
					if (check_defined) goto FAIL_CHECK;
					SEMA_ERROR(init, "Expected a constant expression assigned to %s.", decl->name);
					goto FAIL;
				}
				// Update the type.
				decl->type = init->type;
				break;
			}
			// Here we set a void type if both init and type is missing.
			decl->type = type_void;
			break;
		default:
			UNREACHABLE
	}
	if (check_defined) return true;
	return sema_add_local(context, decl);
FAIL_CHECK:
	if (check_defined)
	{
		*check_defined = true;
		return false;
	}
FAIL:
	sema_add_local(context, decl);
	return decl_poison(decl);
}
/**
 * Analyse a regular global or local declaration, e.g. int x = 123
 */
bool sema_analyse_var_decl(SemaContext *context, Decl *decl, bool local, bool *check_defined)
{
	ASSERT(decl->decl_kind == DECL_VAR && "Unexpected declaration type");

	VarDeclKind kind = decl->var.kind;

	bool success = true;
	bool is_global = !local;
	switch (kind)
	{
		case VARDECL_LOCAL_CT:
		case VARDECL_LOCAL_CT_TYPE:
			return sema_analyse_var_decl_ct(context, decl, check_defined);
		case VARDECL_GLOBAL:
			is_global = true;
			break;
		case VARDECL_CONST:
			if (local) decl->var.is_static = true;
			break;
		default:
			break;
	}

	TypeInfo *type_info = vartype(decl);
	// We expect a constant to actually be parsed correctly so that it has a value, so
	// this should always be true.
	if (!type_info && decl->is_extern)
	{
		SEMA_ERROR(decl, "A type is needed for the extern %s '%s'.", decl_to_name(decl), decl->name);
		return decl_poison(decl);
	}
	ASSERT(type_info || decl->var.init_expr);

	bool erase_decl = false;
	if (!sema_analyse_attributes_for_var(context, decl, &erase_decl)) return decl_poison(decl);

	bool is_static = decl->var.is_static;
	bool global_level_var = is_static || decl->var.kind == VARDECL_CONST || is_global;

	if (decl->is_extern && decl->var.init_expr)
	{
		ASSERT(is_global);
		SEMA_ERROR(decl->var.init_expr, "Extern globals may not have initializers.");
		return decl_poison(decl);
	}

	if (decl->var.no_init && decl->var.init_expr)
	{
		SEMA_ERROR(decl->var.init_expr, "'@noinit' variables may not have initializers.");
		return decl_poison(decl);
	}
	if (erase_decl)
	{
		decl->decl_kind = DECL_ERASED;
		decl->resolve_status = RESOLVE_DONE;
		return true;
	}

	if (is_global)
	{

	}
	else if (!check_defined)
	{
		if (context->call_env.kind == CALL_ENV_GLOBAL_INIT && !context->call_env.in_no_eval)
		{
			if (context->current_macro)
			{
				SEMA_ERROR(decl, "Macros with declarations may not be used outside of functions.");
				return decl_poison(decl);
			}
			SEMA_ERROR(decl, "Variable declarations may not be used outside of functions.");
			return decl_poison(decl);
		}
		// Add a local to the current context, will throw error on shadowing.
		if (!sema_add_local(context, decl)) return decl_poison(decl);
	}


	// 1. Local or global constants: const int FOO = 123.
	if (!type_info)
	{
		Expr *init_expr = decl->var.init_expr;
		// 1a. We require an init expression.
		if (!init_expr)
		{
			ASSERT(kind == VARDECL_CONST);
			SEMA_ERROR(decl, "Constants need to have an initial value.");
			return decl_poison(decl);
		}
		ASSERT(!decl->var.no_init);
		if (!check_defined && kind == VARDECL_LOCAL && !context_is_macro(context) && init_expr->expr_kind != EXPR_LAMBDA && !decl->var.safe_infer)
		{
			SEMA_ERROR(decl, "Defining a variable using 'var %s = ...' is only allowed inside a macro, or when defining a lambda. You can override this by adding the attribute '@safeinfer' to the declaration.", decl->name);
			return decl_poison(decl);
		}
		if (!sema_analyse_expr_rvalue(context, init_expr)) return decl_poison(decl);
		if (check_defined || global_level_var || !type_is_aggregate(init_expr->type)) sema_cast_const(init_expr);
		if (global_level_var && !expr_is_runtime_const(init_expr))
		{
			if (check_defined) return *check_defined = true, false;
			SEMA_ERROR(init_expr, "This expression cannot be evaluated at compile time.");
			return decl_poison(decl);
		}
		decl->type = init_expr->type;
		switch (sema_resolve_storage_type(context, init_expr->type))
		{
			case STORAGE_ERROR:
				return decl_poison(decl);
			case STORAGE_NORMAL:
				break;
			case STORAGE_WILDCARD:
				if (check_defined) return *check_defined = true, false;
				SEMA_ERROR(init_expr, "No type can be inferred from the optional result.");
				return decl_poison(decl);
			case STORAGE_VOID:
				if (check_defined) return *check_defined = true, false;
				SEMA_ERROR(init_expr, "You cannot initialize a value to 'void'.");
				return decl_poison(decl);
			case STORAGE_COMPILE_TIME:
				if (check_defined) return *check_defined = true, false;
				if (init_expr->type == type_untypedlist)
				{
					if (check_defined) return *check_defined = true, false;
					SEMA_ERROR(init_expr,
					           "The type of an untyped list cannot be inferred, you can try adding an explicit type to solve this.");
					return decl_poison(decl);
				}
				if (decl->var.kind == VARDECL_CONST)
				{
					SEMA_ERROR(init_expr,
					           "You cannot initialize a constant to %s, but you can assign the expression to a compile time variable.",
					           type_invalid_storage_type_name(init_expr->type));
					return decl_poison(decl);
				}
				SEMA_ERROR(init_expr, "You can't store a compile time type in a variable.");
				return decl_poison(decl);
			case STORAGE_UNKNOWN:
				if (check_defined) return *check_defined = true, false;
				SEMA_ERROR(init_expr, "You cannot initialize a value to %s as it has unknown size.",
				           type_quoted_error_string(init_expr->type));
				return decl_poison(decl);
		}
		if (!decl->alignment)
		{
			if (!sema_set_alloca_alignment(context, decl->type, &decl->alignment)) return false;
		}
		if (!sema_analyse_variable_type(context, decl->type, init_expr->span)) return decl_poison(decl);
		// Skip further evaluation.
		goto EXIT_OK;
	}

	if (!sema_resolve_type_info(context, type_info,
	                            decl->var.init_expr ? RESOLVE_TYPE_ALLOW_INFER
	                                                : RESOLVE_TYPE_DEFAULT)) return decl_poison(decl);

	Type *type = decl->type = type_info->type;
	if (!sema_analyse_variable_type(context, type, type_info->span)) return decl_poison(decl);

	type = type_no_optional(type);
	if (type_is_user_defined(type) && type->decl)
	{
		if (!sema_analyse_decl(context, type->decl)) return false;
		sema_display_deprecated_warning_on_use(context, type->decl, type_info->span);
	}

	if (is_static && context->call_env.pure)
	{
		SEMA_ERROR(decl, "'@pure' functions may not have static variables.");
		return decl_poison(decl);
	}

	bool infer_len = type_len_is_inferred(decl->type);
	if (!decl->var.init_expr && infer_len)
	{
		SEMA_ERROR(type_info, "The length cannot be inferred without an initializer.");
		return decl_poison(decl);
	}
	if (decl->var.init_expr)
	{
		Expr *init = decl->var.init_expr;

		if (!infer_len)
		{
			// Pre resolve to avoid problem with recursive definitions.
			decl->resolve_status = RESOLVE_DONE;
			if (!decl->alignment)
			{
				if (!sema_set_alloca_alignment(context, decl->type, &decl->alignment)) return false;
			}
		}

		CallEnvKind env_kind = context->call_env.kind;
		if (is_static) context->call_env.kind = CALL_ENV_FUNCTION_STATIC;
		decl->in_init = true;

		Decl *generic = declptrzero(type_find_generic(decl->type));
		if (generic)
		{
			Decl *temp = context->generic_infer;
			context->generic_infer = generic;
			generic = temp;
		}
		success = sema_expr_analyse_assign_right_side(context, NULL, decl->type, init, false, true, check_defined);
		context->generic_infer = generic;
		if (!success && check_defined) return false;
		decl->in_init = false;
		context->call_env.kind = env_kind;
		if (infer_len)
		{
			if (!success) return decl_poison(decl);
			decl->type = type_add_optional(init->type, IS_OPTIONAL(decl));
		}

		// 2. Check const-ness
		if (global_level_var)
		{
			if (!success) return decl_poison(decl);
			if (!expr_is_runtime_const(init))
			{
				if (check_defined) return *check_defined = true, false;
				SEMA_ERROR(init, "The expression must be a constant value.");
				return decl_poison(decl);
			}
		}
		if (!success) goto EXIT_OK;
		if (global_level_var || !type_is_aggregate(init->type)) sema_cast_const(init);
		if (expr_is_const(init))
		{
			init->const_expr.is_hex = false;
		}
	}
	EXIT_OK:;
	// Patch the external name for local consts and static variables.
	if ((decl->var.kind == VARDECL_CONST || is_static) && !decl->extname && context->call_env.kind == CALL_ENV_FUNCTION)
	{
		scratch_buffer_clear();
		scratch_buffer_append(context->call_env.current_function->name);
		scratch_buffer_append_char('.');
		scratch_buffer_append(decl->name);
		decl->extname = scratch_buffer_copy();
	}
	if (!decl->alignment)
	{
		if (!sema_set_alloca_alignment(context, decl->type, &decl->alignment)) return false;
	}
	if (decl->type && !sema_type_is_valid_size(context, decl->type, decl->var.type_info ? type_infoptr(decl->var.type_info)->span : decl->span)) return false;
	if (decl->var.kind == VARDECL_LOCAL && !is_static && type_size(decl->type) > compiler.build.max_stack_object_size * 1024)
	{
		size_t size = type_size(decl->type);
		RETURN_SEMA_ERROR(
			decl, "The size of this local variable (%s%d Kb) exceeds the maximum allowed stack object size (%d Kb), "
			"you can increase this limit with --max-stack-object-size, but be aware that too large objects "
			"allocated on the stack may lead to the stack running out of memory.", size % 1024 == 0 ? "" : "over ",
			size / 1024,
			compiler.build.max_stack_object_size);
	}
	return success;
}

static inline void mangle_type_param(Type *type, bool mangled)
{
	type = type->canonical;
	if (mangled)
	{
		type_mangle_introspect_name_to_buffer(type);
	}
	else
	{
		scratch_buffer_append(type->name);
	}
}


static bool sema_generate_parameter_suffix_to_scratch(Expr **params, bool mangled)
{
	scratch_buffer_clear();
	scratch_buffer_append(mangled ? "$" : "{");
	FOREACH_IDX(j, Expr *, param, params)
	{
		if (j != 0)
		{
			scratch_buffer_append(mangled ? "$" : ", ");
		}
		if (expr_is_const_typeid(param))
		{
			mangle_type_param(param->const_expr.typeid, mangled);
		}
		else
		{
			Type *type = param->type->canonical;
			if (type->type_kind == TYPE_TYPEDEF)
			{
				mangle_type_param(type, mangled);
				scratch_buffer_append(mangled ? "$" : ", ");
				type = type_flatten(type);
			}
			if (type == type_bool)
			{
				if (mangled)
				{
					scratch_buffer_append_char(param->const_expr.b ? 't' : 'f');
				}
				else
				{
					scratch_buffer_append(param->const_expr.b ? "true" : "false");
				}
			}
			else if (type->type_kind == TYPE_ENUM)
			{
				Decl *enumm = param->const_expr.enum_val;
				type_mangle_introspect_name_to_buffer(enumm->type->canonical);
				scratch_buffer_append(mangled ? "_" : ":");
				scratch_buffer_append(enumm->name);
			}
			else if (type->type_kind == TYPE_ANYFAULT)
			{
				Decl *fault = param->const_expr.fault;
				if (fault)
				{
					type_mangle_introspect_name_to_buffer(fault->type->canonical);
					scratch_buffer_append(mangled ? "_" : ":");
					scratch_buffer_append(fault->name);
				}
				else
				{
					scratch_buffer_append("null");
				}
			}
			else
			{
				char *maybe_neg = &scratch_buffer.str[scratch_buffer.len];
				if (type->type_kind == TYPE_I128 || type->type_kind == TYPE_U128)
				{
					char *str = int_to_str(param->const_expr.ixx, 10, false);
					scratch_buffer_append(str);
				}
				else
				{
					if (type_is_signed(type))
					{
						scratch_buffer_append_signed_int((int64_t)param->const_expr.ixx.i.low);
					}
					else
					{
						scratch_buffer_append_unsigned_int(param->const_expr.ixx.i.low);
					}
				}
				if (mangled)
				{
					// Replace - with _
					if (maybe_neg[0] == '-') maybe_neg[0] = '_';
				}
			}
		}
	}
	scratch_buffer_append(mangled ? "$" : "}");
	return true;
}

static bool sema_analyse_generic_module_contracts(SemaContext *c, Module *module, Decl *instance, Expr **contracts, SourceSpan param_span, SourceSpan invocation_span)
{
	ASSERT(contracts);
	FOREACH(Expr *, contract, contracts)
	{
		SemaContext temp_context;
		InliningSpan *old_span = c->inlined_at;
		InliningSpan new_span = { .prev = old_span, .span = invocation_span };
		SemaContext *new_context = context_transform_for_eval(c, &temp_context, module->units[0]);
		Decl *old_generic_instance = new_context->generic_instance;
		InliningSpan *old_inlined_at = new_context->inlined_at;
		new_context->generic_instance = instance;
		new_context->inlined_at = &new_span;

		FOREACH(Expr *, expr, contract->contract_expr.decl_exprs->expression_list)
		{
			CondResult res = sema_check_comp_time_bool(new_context, expr);
			if (res == COND_MISSING) goto FAIL;
			if (res == COND_TRUE) continue;
			if (contract->contract_expr.comment)
			{
				sema_error_at(c, param_span,
				              "Parameter(s) would violate constraint: %s.",
				              contract->contract_expr.comment);
			}
			else
			{
				sema_error_at(c, param_span, "Parameter(s) failed validation: %s",
				              contract->contract_expr.expr_string);
			}
			FAIL:
			new_context->inlined_at = old_inlined_at;
			sema_context_destroy(&temp_context);
			return false;
		}
		new_context->inlined_at = old_inlined_at;
		new_context->generic_instance = old_generic_instance;
		sema_context_destroy(&temp_context);
	}
	return true;
}

Decl *sema_generate_parameterized_identifier(SemaContext *context, Decl *generic, Decl *alias, Expr **params, Decl **param_decls, const char *suffix, const char *csuffix, SourceSpan invocation_span, SourceSpan span)
{
	Module *module = alias->unit->module;
	Decl *instance = NULL;
	unsigned id = generic->generic_decl.id;
	FOREACH(Decl *, g, alias->unit->module->generic_sections)
	{
		if (g->generic_decl.id != id) continue;
		FOREACH (Decl *, candidate, g->generic_decl.instances)
		{
			if (candidate->name == csuffix)
			{
				instance = candidate;
				goto FOUND;
			}
		}
	}
FOUND:;
	if (!instance)
	{
		DEBUG_LOG("Generate generic instance %s", csuffix);
		if (compiler.context.errors_found) return poisoned_decl;
		instance = decl_new(DECL_GENERIC_INSTANCE, csuffix, generic->span);
		FOREACH_IDX(i, const char *, param_name, generic->generic_decl.parameters)
		{
			Decl *decl;
			Expr *param = params ? params[i] : copy_expr_single(param_decls[i]->var.init_expr);
			ASSERT_SPAN(param, param->expr_kind == EXPR_CONST);
			if (expr_is_const_typeid(param))
			{
				decl = decl_new_var(param_name, param->span, NULL, VARDECL_PARAM_CT_TYPE);
			}
			else
			{
				ASSERT(param->expr_kind == EXPR_CONST);
				decl = decl_new_var(param_name, param->span, NULL, VARDECL_CONST);
			}
			decl->var.init_expr = param;
			decl->unit = alias->unit;
			decl->resolve_status = RESOLVE_DONE;
			decl->type = param->type;
			vec_add(instance->instance_decl.params, decl);
		}
		instance->unit = alias->unit;
		Decl **copied = NULL;
		Decl **copied_cond = NULL;
		if (!suffix)
		{
			if (!sema_generate_parameter_suffix_to_scratch(params, false)) return poisoned_decl;
			suffix = scratch_buffer_interned();
		}
		instance->instance_decl.name_suffix = suffix;
		instance->instance_decl.cname_suffix = csuffix;
		instance->instance_decl.id = id;
		FOREACH(Decl *, g, module->generic_sections)
		{
			if (g->generic_decl.id == generic->generic_decl.id)
			{
				vec_add(instance->instance_decl.templates, g);
				Decl **decls = g->generic_decl.decls;
				Decl **cond_decls = g->generic_decl.conditional_decls;
				decls = decls ? copy_decl_list_single_for_generic(decls, instance) : NULL;
				cond_decls = cond_decls ? copy_decl_list_single_for_generic(cond_decls, instance) : NULL;
				FOREACH(Decl *, d, decls) vec_add(copied, d);
				FOREACH(Decl *, d, cond_decls) vec_add(copied_cond, d);
			}
		}
		vec_add(generic->generic_decl.instances, instance);
		AnalysisStage stage = module->stage;
		ASSERT(stage > ANALYSIS_IMPORTS);
		if (compiler.context.errors_found) return poisoned_decl;

		// Check contracts
		FOREACH(Decl *, decl, module->generic_sections)
		{
			if (decl->generic_decl.id == generic->generic_decl.id)
			{
				Expr **requires = decl->generic_decl.requires;
				if (!requires) continue;
				copy_begin();
				Expr **contract = copy_exprlist_macro(requires);
				copy_end();
				SourceSpan param_span = extend_span_with_token(params[0]->span, VECLAST(params)->span); // NOLINT
				if (!sema_analyse_generic_module_contracts(context, module, instance, contract, param_span, invocation_span))
				{
					decl_poison(instance);
					decl_poison(alias);
					return alias;
				}
			}
		}

		// Add all the normal top level declarations
		FOREACH(Decl *, decl, copied) unit_register_global_decl(decl->unit, decl);
		// Add all the conditional declarations
		FOREACH(Decl *, decl, copied_cond)
		{
			unit_register_optional_global_decl(decl->unit, decl);
			if (decl->decl_kind != DECL_ERASED) vec_add(copied, decl);
		}

		if (stage < ANALYSIS_METHODS_REGISTER) goto EXIT;
		FOREACH(Decl *, decl, copied)
		{
			if (decl->decl_kind != DECL_FUNC && decl->decl_kind != DECL_MACRO) continue;
			if (!decl->func_decl.type_parent) continue;
			SemaContext gen_context;
			sema_context_init(&gen_context, decl->unit);
			gen_context.generic_instance = instance;
			if (sema_analyse_method_register(&gen_context, decl) && decl->decl_kind != DECL_ERASED)
			{
				if (decl->decl_kind == DECL_MACRO)
				{
					vec_add(decl->unit->macro_methods, decl);
				}
				else
				{
					vec_add(decl->unit->methods, decl);
				}
			}
		}
		if (stage < ANALYSIS_DECLS) goto EXIT;
		FOREACH(Decl *, decl, copied)
		{
			SemaContext context_gen;
			sema_context_init(&context_gen, decl->unit);
			DynamicScope empty = { .depth = 0 };
			context_gen.active_scope = empty;
			sema_analyse_decl(&context_gen, decl);
			context_gen.generic_instance = instance;
			sema_analyse_inner_func_ptr(&context_gen, decl);
			FOREACH(TypeInfo *, info, decl->unit->check_type_variable_array)
			{
				sema_check_type_variable_array(&context_gen, info);
			}
			sema_context_destroy(&context_gen);
		}
		if (stage < ANALYSIS_FUNCTIONS) goto EXIT;
		if (compiler.context.errors_found) return poisoned_decl;
		FOREACH(Decl *, decl, copied)
		{
			SemaContext context_gen;
			switch (decl->decl_kind)
			{
				case DECL_FUNC:
					sema_context_init(&context_gen, decl->unit);
					analyse_func_body(&context_gen, decl);
					sema_context_destroy(&context_gen);
					break;
				default:
					break;
			}
		}
		if (stage < ANALYSIS_INTERFACE) goto EXIT;
		if (compiler.context.errors_found) return poisoned_decl;
		FOREACH(Decl *, decl, copied)
		{
			SemaContext context_gen;
			switch (decl->decl_kind)
			{
				case DECL_TYPEDEF:
				case DECL_STRUCT:
				case DECL_UNION:
				case DECL_ENUM:
				case DECL_BITSTRUCT:
					break;
				default:
					continue;
			}
			if (decl->interfaces)
			{
				sema_context_init(&context_gen, decl->unit);
				sema_check_interfaces(&context_gen, decl);
				sema_context_destroy(&context_gen);
			}
		}
EXIT:;
		if (compiler.context.errors_found) return poisoned_decl;
	}
	Decl *symbol = sema_find_generic_instance(context, module, generic, instance, alias->name);
	if (!symbol)
	{
		sema_error_at(context, span, "The generic '%s' does not exist for this parameterization.", alias->name);
		return poisoned_decl;
	}

	CompilationUnit *unit = symbol->unit;
	if (unit->module->stage < ANALYSIS_POST_REGISTER)
	{
		vec_add(unit->global_decls, symbol);
	}
	else
	{
		if (!sema_analyse_decl(context, symbol)) return poisoned_decl;
	}
	unit_register_external_symbol(context, symbol);
	return symbol;

}
Decl *sema_analyse_parameterized_identifier(SemaContext *context, Path *decl_path, const char *name, SourceSpan span,
                                            Expr **params, SourceSpan invocation_span)
{
	Decl *alias = sema_resolve_parameterized_symbol(context, name, decl_path, span);
	if (!alias) return poisoned_decl;
	ASSERT_AT(invocation_span, alias->is_template && alias->generic_id);
	Decl *generic = declptr(alias->generic_id);
	unsigned parameter_count = vec_size(generic->generic_decl.parameters);
	ASSERT(parameter_count > 0);
	unsigned count = vec_size(params);
	if (parameter_count != count)
	{
		if (!count)
		{
			sema_error_at(context, invocation_span,
						  "'%s' requires generic arguments inside the '{}', did you forget them?", name, (int)parameter_count);

		}
		else
		{
			// 'Foo' expects 2 generic arguments, but you supplied 1, did you make a mistake?
			sema_error_at(context, extend_span_with_token(params[0]->span, vectail(params)->span),
						  "'%s' expects %d %s, but you supplied %d, did you make a mistake?",
						  name,
						  parameter_count,
						  parameter_count == 1 ? "argument" : "arguments",
						  vec_size(params));
		}
		return poisoned_decl;
	}
	// First resolve
	FOREACH_IDX(i, Expr *, param, params)
	{
		if (!sema_analyse_expr_rvalue(context, param)) return poisoned_decl;
		const char *param_name = generic->generic_decl.parameters[i];
		bool is_type = str_is_type(param_name);
		if (expr_is_const_typeid(param))
		{
			if (!is_type)
			{
				SEMA_ERROR(param, "Expected a value, not a type, for parameter '%s'.", param_name);
				return poisoned_decl;
			}
			Type *type = param->const_expr.typeid;
			if (type_is_optional(type))
			{
				RETURN_VAL_SEMA_ERROR(poisoned_decl, param, "The generic type can never be an optional, please use only non-optional types.");
			}
			if (type_is_func_ptr(type))
			{
				if (!sema_resolve_type_decl(context, type->pointer)) return poisoned_decl;
			}
		}
		else
		{
			if (is_type)
			{
				RETURN_VAL_SEMA_ERROR(poisoned_decl, param, "Expected a type, not a value, for parameter '%s'.", param_name);
			}
			if (!sema_analyse_ct_expr(context, param)) return poisoned_decl;
			Type *type = param->type->canonical;
			if (type->type_kind == TYPE_TYPEDEF) type = type_flatten(type);
			if (IS_OPTIONAL(param))
			{
				RETURN_VAL_SEMA_ERROR(poisoned_decl, param, "The parameter may never be an optional value.");
			}

			bool is_enum_or_fault = type_kind_is_enum_or_fault(type->type_kind);
			if (!type_is_integer_or_bool_kind(type) && !is_enum_or_fault)
			{
				RETURN_VAL_SEMA_ERROR(poisoned_decl, param, "Only integer, bool, fault and enum values may be generic arguments.");
			}
			ASSERT(expr_is_const(param));
		}
	}
	if (!sema_generate_parameter_suffix_to_scratch(params, true)) return poisoned_decl;
	const char *suffix = scratch_buffer_interned();
	return sema_generate_parameterized_identifier(context, generic, alias, params, NULL, NULL, suffix, invocation_span, span);
}

static inline bool sema_analyse_attribute_decl(SemaContext *context, SemaContext *c, Decl *decl, bool *erase_decl)
{
	if (!sema_analyse_attributes(c, decl, decl->attributes, ATTR_ALIAS, erase_decl)) return decl_poison(decl);
	if (decl_is_deprecated(decl)) context->call_env.ignore_deprecation = true;

	if (*erase_decl) return true;

	Decl **params = decl->attr_decl.params;
	unsigned param_count = vec_size(params);
	for (unsigned i = 0; i < param_count; i++)
	{
		Decl *param = params[i];
		if (param->var.kind != VARDECL_PARAM) RETURN_SEMA_ERROR(param, "Expected a simple replacement parameter e.g. 'val' here.");
		if (param->var.init_expr) RETURN_SEMA_ERROR(param, "Attribute parameters may not have default values.");
		if (param->var.type_info && !sema_resolve_type_info(c, type_infoptr(param->var.type_info), RESOLVE_TYPE_DEFAULT)) return false;
		param->resolve_status = RESOLVE_DONE;
		for (int j = 0; j < i; j++)
		{
			if (params[j]->name == param->name)
			{
				RETURN_SEMA_ERROR(param, "Duplicate parameter name '%s'.", param->name);
			}
		}
	}
	return true;
}


static inline bool sema_analyse_alias(SemaContext *context, Decl *decl, bool *erase_decl)
{
	if (!sema_analyse_attributes(context, decl, decl->attributes, ATTR_ALIAS, erase_decl)) return decl_poison(decl);
	if (decl_is_deprecated(decl)) context->call_env.ignore_deprecation = true;

	if (*erase_decl) return true;

	Expr *expr = decl->define_decl.alias_expr;
	if (!sema_analyse_expr(context, expr)) return false;
	if (expr->expr_kind == EXPR_TYPEINFO)
	{
		RETURN_SEMA_ERROR(decl, "To alias a type, the alias name must start with uppercase and contain at least one lowercase letter.");
	}
	if (expr->expr_kind != EXPR_IDENTIFIER)
	{
		RETURN_SEMA_ERROR(expr, "A global variable or function name was expected here.");
	}
	Decl *symbol = expr->ident_expr;
	while (true)
	{
		if (!sema_analyse_decl(context, symbol)) return false;
		if (symbol->decl_kind != DECL_ALIAS) break;
		symbol = symbol->define_decl.alias;
	}
	bool should_be_const = char_is_upper(decl->name[0]);
	if (should_be_const)
	{
		if (!char_is_upper(symbol->name[0]))
		{
			RETURN_SEMA_ERROR(decl, "An uppercase alias is expected to alias a constant. "
									"If you want to alias a non-constant, make sure the alias name "
									"starts with a lower case letter.");
		}
	}
	else
	{
		if (char_is_upper(symbol->name[0]))
		{
			RETURN_SEMA_ERROR(expr, "An alias starting with a lowercase letter is expected to alias a non-constant. "
			                        "If you want to alias a constant, make sure the "
									"alias name is all uppercase letters.", decl->name);
		}
	}
	if (symbol->name[0] == '@' && decl->name[0] != '@')
	{
		RETURN_SEMA_ERROR(expr, "An at-macro like '%s' must be aliased with an identifier also starting with '@'.", symbol->name);
	}
	decl->type = symbol->type;
	decl->define_decl.alias = symbol;
	if (symbol->visibility == VISIBLE_LOCAL && decl->visibility < VISIBLE_LOCAL)
	{
		RETURN_SEMA_ERROR(decl, "A local symbol like '%s' may not be aliased to 'private' or 'public' visibility, please change it to at least be '@private'", symbol->name);
	}
	// If the symbol is more hidden than the alias, then we must increase the visibility.
	if (decl_is_externally_visible(decl) && !decl_is_externally_visible(symbol))
	{
		symbol->is_external_visible = true;
	}
	return true;
}

bool sema_resolve_type_structure(SemaContext *context, Type *type)
{
RETRY:
	switch (type->type_kind)
	{
		case TYPE_INTERFACE:
		case TYPE_ANY:
		case TYPE_POISONED:
		case TYPE_VOID:
		case TYPE_BOOL:
		case ALL_INTS:
		case ALL_FLOATS:
		case TYPE_ANYFAULT:
		case TYPE_TYPEID:
		case TYPE_UNTYPED_LIST:
		case TYPE_WILDCARD:
		case TYPE_TYPEINFO:
		case TYPE_MEMBER:
			return true;
		case TYPE_FUNC_RAW:
		case TYPE_ENUM:
		case TYPE_CONST_ENUM:
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_BITSTRUCT:
			return sema_analyse_decl(context, type->decl);
		case TYPE_POINTER:
		case TYPE_FUNC_PTR:
			type = type->pointer;
			goto RETRY;
		case TYPE_ALIAS:
			type = type->canonical;
			goto RETRY;
		case TYPE_TYPEDEF:
			if (!sema_analyse_decl(context, type->decl)) return false;
			type = type->decl->distinct->type;
			goto RETRY;
		case TYPE_ARRAY:
		case TYPE_SLICE:
		case TYPE_FLEXIBLE_ARRAY:
		case TYPE_INFERRED_ARRAY:
		case ALL_VECTORS:
			type = type->array.base;
			goto RETRY;
		case TYPE_OPTIONAL:
			type = type->optional;
			goto RETRY;
	}
	UNREACHABLE
}

INLINE Decl *type_is_possible_template(SemaContext *context, TypeInfo *type_info)
{
	if (type_info->resolve_status == RESOLVE_DONE) return NULL;
	if (type_info->kind != TYPE_INFO_IDENTIFIER) return NULL;
	if (type_info->subtype != TYPE_COMPRESSED_NONE) return NULL;
	Decl *candidate = sema_find_template_symbol(context, type_info->unresolved.name, type_info->unresolved.path);
	return candidate && candidate->is_template ? candidate : NULL;
}
bool sema_analyse_method_register(SemaContext *context, Decl *method)
{
	TypeInfo *parent_type_info = type_infoptr(method->func_decl.type_parent);
	Decl *decl = method->is_templated ? NULL : type_is_possible_template(context, parent_type_info);
	if (decl)
	{
		Decl *generic_section = declptr(decl->generic_id);
		vec_add(generic_section->generic_decl.decls, method);
		method->is_template = true;
		method->generic_id = decl->generic_id;
		return false;
	}
	if (!sema_resolve_type_info(context, parent_type_info, method->decl_kind == DECL_MACRO ? RESOLVE_TYPE_MACRO_METHOD : RESOLVE_TYPE_FUNC_METHOD)) return false;

	// Can the type have methods?
	Type *parent_type = parent_type_info->type = parent_type_info->type->canonical;
	if (!type_may_have_method(parent_type))
	{
		RETURN_SEMA_ERROR(parent_type_info, "Methods can not be associated with '%s'", type_to_error_string(parent_type));
	}

	return type_add_method(context, parent_type->canonical, method);
}

bool sema_analyse_decl(SemaContext *context, Decl *decl)
{
	if (decl->resolve_status == RESOLVE_DONE) return decl_ok(decl);
	DEBUG_LOG(">>> Analyse declaration [%s] in %s.", decl_safe_name(decl), context_filename(context));

	SemaContext temp_context;
	context = context_transform_for_eval(context, &temp_context, decl->unit);

	Decl *old_generic_instance = context->generic_instance;
	context->generic_instance = decl->is_templated ? declptr(decl->instance_id) : NULL;
	if (decl->resolve_status == RESOLVE_RUNNING)
	{
		SEMA_ERROR(decl, decl->name
			? "Recursive definition of '%s'."
			: "Recursive definition of anonymous declaration.", decl->name);
		goto FAILED;
	}
	decl->resolve_status = RESOLVE_RUNNING;
	ASSERT_SPAN(decl, decl->unit);
	bool erase_decl = false;
	switch (decl->decl_kind)
	{
		case DECL_ERASED:
			break;
		case DECL_INTERFACE:
			if (!sema_analyse_interface(context, decl, &erase_decl)) goto FAILED;
			break;
		case DECL_BITSTRUCT:
			if (!sema_analyse_bitstruct(context, decl, &erase_decl)) goto FAILED;
			break;
		case DECL_STRUCT:
		case DECL_UNION:
			if (!sema_analyse_struct_union(context, decl, &erase_decl)) goto FAILED;
			break;
		case DECL_FNTYPE:
			if (!sema_analyse_fntype(context, decl, &erase_decl)) goto FAILED;
			break;
		case DECL_FUNC:
			if (!sema_analyse_func(context, decl, &erase_decl)) goto FAILED;
			break;
		case DECL_MACRO:
			if (!sema_analyse_macro(context, decl, &erase_decl)) goto FAILED;
			break;
		case DECL_VAR:
			if (!sema_analyse_var_decl(context, decl, false, NULL)) goto FAILED;
			break;
		case DECL_ATTRIBUTE:
			if (!sema_analyse_attribute_decl(context, context, decl, &erase_decl)) goto FAILED;
			break;
		case DECL_TYPEDEF:
			if (!sema_analyse_typedef(context, decl, &erase_decl)) goto FAILED;
			break;
		case DECL_TYPE_ALIAS:
			if (!sema_analyse_type_alias(context, decl, &erase_decl)) goto FAILED;
			break;
		case DECL_ENUM:
			if (!sema_analyse_enum(context, decl, &erase_decl)) goto FAILED;
			break;
		case DECL_CONSTDEF:
			if (!sema_analyse_constdef(context, decl, &erase_decl)) goto FAILED;
			break;
		case DECL_FAULT:
			if (!sema_analyse_fault(context, decl, &erase_decl)) goto FAILED;
			break;
		case DECL_ALIAS:
			if (!sema_analyse_alias(context, decl, &erase_decl)) goto FAILED;
			break;
		case DECL_ALIAS_PATH:
		case DECL_BODYPARAM:
		case DECL_CT_ASSERT:
		case DECL_CT_ECHO:
		case DECL_CT_EXEC:
		case DECL_CT_INCLUDE:
		case DECL_DECLARRAY:
		case DECL_ENUM_CONSTANT:
		case DECL_GROUP:
		case DECL_IMPORT:
		case DECL_LABEL:
		case DECL_POISONED:
		case DECL_GENERIC:
		case DECL_GENERIC_INSTANCE:
		case DECL_CONTRACT:
			UNREACHABLE
	}
	if (erase_decl)
	{
		decl->decl_kind = DECL_ERASED;
	}
	decl->resolve_status = RESOLVE_DONE;
	context->generic_instance = old_generic_instance;
	sema_context_destroy(&temp_context);

	DEBUG_LOG("<<< Analysis of [%s] successful.", decl_safe_name(decl));
	return true;
FAILED:
	context->generic_instance = old_generic_instance;
	sema_context_destroy(&temp_context);
	DEBUG_LOG("<<< Analysis of [%s] failed.", decl_safe_name(decl));
	return decl_poison(decl);
}
