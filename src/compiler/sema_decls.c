// Copyright (c) 2019-2024 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "sema_internal.h"

#define EXPORTED_USER_DEFINED_TYPES (ATTR_ENUM | ATTR_UNION | ATTR_STRUCT | ATTR_FAULT)
#define CALLABLE_TYPE (ATTR_FUNC | ATTR_INTERFACE_METHOD | ATTR_MACRO)
#define USER_DEFINED_TYPES EXPORTED_USER_DEFINED_TYPES | ATTR_BITSTRUCT | ATTR_DISTINCT

static inline bool sema_analyse_func_macro(SemaContext *context, Decl *decl, AttributeDomain domain, bool *erase_decl);
static inline bool sema_analyse_func(SemaContext *context, Decl *decl, bool *erase_decl);
static inline bool sema_analyse_macro(SemaContext *context, Decl *decl, bool *erase_decl);
static inline bool sema_analyse_signature(SemaContext *context, Signature *sig, TypeInfoId type_parent, bool is_export);
static inline bool sema_analyse_main_function(SemaContext *context, Decl *decl);
static inline bool sema_check_param_uniqueness_and_type(SemaContext *context, Decl **decls, Decl *current,
                                                        unsigned current_index, unsigned count);
static inline bool sema_analyse_method(SemaContext *context, Decl *decl);
static inline bool sema_is_valid_method_param(SemaContext *context, Decl *param, Type *parent_type, bool is_dynamic);
static inline bool sema_analyse_macro_method(SemaContext *context, Decl *decl);
static inline bool unit_add_base_extension_method(SemaContext *context, CompilationUnit *unit, Type *parent_type,
                                                  Decl *method);
static inline bool unit_add_method(SemaContext *context, Type *parent_type, Decl *method);
static bool sema_analyse_operator_common(SemaContext *context, Decl *method, TypeInfo **rtype_ptr, Decl ***params_ptr,
                                         uint32_t parameters);
static inline Decl *operator_in_module(SemaContext *c, Module *module, OperatorOverload operator_overload);
static inline bool sema_analyse_operator_element_at(SemaContext *context, Decl *method);
static inline bool sema_analyse_operator_element_set(SemaContext *context, Decl *method);
static inline bool sema_analyse_operator_len(Decl *method, SemaContext *context);
static bool sema_check_operator_method_validity(SemaContext *context, Decl *method);

static inline const char *method_name_by_decl(Decl *method_like);

static bool sema_analyse_struct_union(SemaContext *context, Decl *decl, bool *erase_decl);
static bool sema_analyse_bitstruct(SemaContext *context, Decl *decl, bool *erase_decl);
static bool sema_analyse_union_members(SemaContext *context, Decl *decl);
static bool sema_analyse_struct_members(SemaContext *context, Decl *decl);
static inline bool sema_analyse_struct_member(SemaContext *context, Decl *parent, Decl *decl, bool *erase_decl);
static inline bool sema_check_struct_holes(SemaContext *context, Decl *decl, Decl *member);
static inline bool sema_analyse_bitstruct_member(SemaContext *context, Decl *parent, Decl *member, unsigned index, bool allow_overlap, bool *erase_decl);

static inline bool sema_analyse_doc_header(SemaContext *context, AstId doc, Decl **params, Decl **extra_params, bool *pure_ref);

static const char *attribute_domain_to_string(AttributeDomain domain);
static bool sema_analyse_attribute(SemaContext *context, Decl *decl, Attr *attr, AttributeDomain domain, bool *erase_decl);
static bool sema_analyse_attributes_inner(SemaContext *context, Decl *decl, Attr **attrs, AttributeDomain domain,
										  Decl *top, bool *erase_decl);
static bool sema_analyse_attributes(SemaContext *context, Decl *decl, Attr **attrs, AttributeDomain domain,
									bool *erase_decl);
static bool sema_analyse_attributes_for_var(SemaContext *context, Decl *decl, bool *erase_decl);
static bool sema_check_section(SemaContext *context, Attr *attr);
static inline bool sema_analyse_attribute_decl(SemaContext *context, SemaContext *c, Decl *decl, bool *erase_decl);

static inline bool sema_analyse_typedef(SemaContext *context, Decl *decl, bool *erase_decl);
static bool sema_analyse_decl_type(SemaContext *context, Type *type, SourceSpan span);
static inline bool sema_analyse_define(SemaContext *c, Decl *decl, bool *erase_decl);
static inline bool sema_analyse_distinct(SemaContext *context, Decl *decl, bool *erase_decl);

static CompilationUnit *unit_copy(Module *module, CompilationUnit *unit);
static bool sema_analyse_parameterized_define(SemaContext *c, Decl *decl);
static Module *module_instantiate_generic(SemaContext *context, Module *module, Path *path, Expr **params,
										  SourceSpan from);

static inline bool sema_analyse_enum_param(SemaContext *context, Decl *param);
static inline bool sema_analyse_enum(SemaContext *context, Decl *decl, bool *erase_decl);
static inline bool sema_analyse_error(SemaContext *context, Decl *decl, bool *erase_decl);

static bool sema_check_section(SemaContext *context, Attr *attr)
{
	Expr *expr = attr->exprs[0];
	const char *section_string = expr->const_expr.bytes.ptr;
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
                                                        unsigned current_index, unsigned count)
{
	// We may have `void` arguments. They are not allowed in C3. Theoretically they could
	// be permitted, but it would complicate semantics in general.
	if (current->type && type_flatten(current->type) == type_void)
	{
		if (count == 1 && !current->name && current->var.kind == VARDECL_PARAM)
		{
			RETURN_SEMA_ERROR(current, "C-style 'foo(void)' style argument declarations are not valid, please remove 'void'.");
		}
		// Some languages would allow this, because it is a way to do overloading
		RETURN_SEMA_ERROR(current, "Parameters may not be of type 'void'.");
	}

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
	assert(!decl->unit || decl->unit->module->is_generic || decl->unit == parent->unit);
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
		case DECL_VAR:
		{
			assert(decl->var.kind == VARDECL_MEMBER);
			decl->resolve_status = RESOLVE_RUNNING;
			// Inferred types are not strictly allowed, but we use the int[*] for the flexible array member.
			assert(type_infoptrzero(decl->var.type_info));
			TypeInfo *type_info = type_infoptr(decl->var.type_info);
			if (!sema_resolve_type_info(context, type_info, RESOLVE_TYPE_ALLOW_FLEXIBLE)) return decl_poison(decl);
			Type *type = type_info->type;
			switch (type_storage_type(type))
			{
				case STORAGE_NORMAL:
					break;
				case STORAGE_VOID:
				case STORAGE_COMPILE_TIME:
					RETURN_SEMA_ERROR(type_info, "Members cannot be of type %s.", type_quoted_error_string(type));
				case STORAGE_WILDCARD:
				case STORAGE_UNKNOWN:
					RETURN_SEMA_ERROR(type_info, "%s has unknown size and cannot be used as a member.", type_quoted_error_string(type));
			}
			decl->type = type;
			decl->resolve_status = RESOLVE_DONE;
			return true;
		}
		case DECL_STRUCT:
		case DECL_UNION:
			// Extend the nopadding attributes to nested structs.
			if (parent->attr_nopadding) decl->attr_nopadding = true;
			if (parent->attr_compact) decl->attr_compact = true;
			FALLTHROUGH;
		case DECL_BITSTRUCT:
			decl->is_export = is_export;
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
	assert(decl_is_struct_type(member_type->decl));
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

	bool has_named_parameter = false;
	Decl **members = decl->strukt.members;
	unsigned member_count = vec_size(members);
	assert(member_count > 0);

	// Check all members
	for (unsigned i = 0; i < member_count; i++)
	{
		AGAIN:;
		Decl *member = members[i];

		// The member may already have been resolved if it is poisoned, then exit.
		if (!decl_ok(member)) return false;
		bool erase_decl = false;

		// Check the member
		if (!sema_analyse_struct_member(context, decl, member, &erase_decl))
		{
			// Failed
			return decl_poison(member);
		}

		// If we need to erase it then do so.
		if (erase_decl)
		{
			vec_erase_ptr_at(members, i);
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
		if (!sema_set_abi_alignment(context, member->type, &member_alignment)) return false;

		if (!sema_check_struct_holes(context, decl, member)) return false;

		ByteSize member_size = type_size(member->type);
		assert(member_size <= MAX_TYPE_SIZE);
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

	assert(decl_ok(decl));

	// 1. If packed, then the alignment is zero, unless previously given
	if (decl->is_packed && !decl->alignment) decl->alignment = 1;

	// 2. otherwise pick the highest of the natural alignment and the given alignment.
	if (!decl->is_packed && decl->alignment < max_alignment) decl->alignment = max_alignment;

	// We're only packed if the max alignment is > 1
	decl->is_packed = decl->is_packed && max_alignment > 1;

	// "Representative" type is the one with the maximum alignment.
	assert(max_alignment_element >= 0);
	decl->strukt.union_rep = max_alignment_element;

	// All members share the same alignment
	FOREACH(Decl *, member, members)
	{
		member->alignment = decl->alignment;
	}

	assert(max_size);

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
	type = type_flatten(type);
	switch (type->type_kind)
	{
		case TYPE_DISTINCT:
		case TYPE_POISONED:
		case TYPE_TYPEDEF:
		case TYPE_UNTYPED_LIST:
		case TYPE_OPTIONAL:
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
		case TYPE_FAULTTYPE:
		case TYPE_SLICE:
			return platform_target.align_pointer.align / 8;
		case TYPE_ENUM:
			type = type->decl->enums.type_info->type;
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
			type = type->decl->bitstruct.base_type->type;
			goto RETRY;
		case TYPE_ARRAY:
		case TYPE_FLEXIBLE_ARRAY:
		case TYPE_INFERRED_ARRAY:
			type = type->array.base;
			goto RETRY;
		case TYPE_VECTOR:
		case TYPE_INFERRED_VECTOR:
			return type_abi_alignment(type);
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
	assert(member_count > 0 && "This analysis should only be called on member_count > 0");

	for (unsigned i = 0; i < member_count; i++)
	{
	AGAIN:;
		Decl *member = struct_members[i];
		// We might have already analysed and poisoned this decl, if so, exit.
		if (!decl_ok(member)) return decl_poison(decl);
		bool erase_decl = false;
		// Check the member
		if (!sema_analyse_struct_member(context, decl, member, &erase_decl))
		{
			return decl_poison(decl);
		}
		// If we should erase it, do so.
		if (erase_decl)
		{
			vec_erase_ptr_at(struct_members, i);
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

		assert(decl_ok(decl) && "The declaration should be fine at this point.");

		// Grab the alignment of the member type
		AlignSize member_type_alignment;
		if (!sema_set_abi_alignment(context, member->type, &member_type_alignment)) return decl_poison(decl);
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

		if (align_offset - offset != 0)
		{
			assert(decl_is_struct_type(decl));
			if (!decl->strukt.padded_decl_id) decl->strukt.padded_decl_id = declid(member);
			if (decl->attr_nopadding || member->attr_nopadding)
			{
				RETURN_SEMA_ERROR(member, "%d bytes of padding would be added to align this member which is not allowed with `@nopadding` and `@compact`.", align_offset - offset);
			}
		}

		if (!sema_check_struct_holes(context, decl, member)) return false;

		offset = align_offset;
		member->offset = offset;
		offset += type_size(member->type);
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
		decl->strukt.padding = (AlignSize)(size - offset);
	}

	// If the size is smaller the naturally aligned struct, then it is also unaligned
	if (size < natural_size)
	{
		is_unaligned = true;
	}
	if (is_unaligned && size > offset)
	{
		assert(!decl->strukt.padding);
		decl->strukt.padding = (AlignSize)(size - offset);
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
		assert(decl_is_struct_type(decl));
		if (!decl->strukt.padded_decl_id) decl->strukt.padded_decl_id = declid(decl);
		if (decl->attr_nopadding)
		{
			RETURN_SEMA_ERROR(decl, "%d bytes of padding would be added to the end this struct which is not allowed with `@nopadding` and `@compact`.", size - offset);
		}
	}

	decl->is_packed = is_unaligned;
	decl->strukt.size = size;
	return true;
}

static bool sema_analyse_struct_union(SemaContext *context, Decl *decl, bool *erase_decl)
{
	// Begin by analysing attributes
	bool is_union = decl->decl_kind == DECL_UNION;
	AttributeDomain domain = is_union ? ATTR_UNION : ATTR_STRUCT;
	if (!sema_analyse_attributes(context, decl, decl->attributes, domain, erase_decl)) return decl_poison(decl);

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

	assert(decl_ok(decl));
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

	bool ease_decl = false;
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

	bool is_consecutive = parent->bitstruct.consecutive;

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
	BitSize bits = type_size(parent->bitstruct.base_type->type) * (BitSize)8;

	if (bits > MAX_BITSTRUCT)
	{
		SEMA_ERROR(parent->bitstruct.base_type, "Bitstruct size may not exceed %d bits.", MAX_BITSTRUCT);
		return false;
	}
	Int max_bits = (Int) { .type = TYPE_I64, .i = { .low =  bits } };

	// Resolve the bit range, starting with the beginning

	unsigned start_bit, end_bit;

	if (is_consecutive)
	{
		assert(!member->var.bit_is_expr && "Should always me inferred");
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
		if (!sema_analyse_expr(context, start)) return false;

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
			if (!sema_analyse_expr(context, start)) return false;
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
		Decl **members = parent->bitstruct.members;
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
		// The method might have been resolved earlier, if so we either exit or go to the next.
		// This might happen for example if it was resolved using $checks
		if (method->resolve_status == RESOLVE_DONE)
		{
			if (!decl_ok(method)) return false;
			continue;
		}

		if (method->decl_kind != DECL_FUNC) RETURN_SEMA_ERROR(method, "Only functions are allowed here.");
		if (method->func_decl.type_parent)
		{
			RETURN_SEMA_ERROR(type_infoptr(method->func_decl.type_parent), "Interfaces should not be declared as methods.");
		}
		method->func_decl.attr_interface_method = true;
		bool erase = false;

		// Insert the first parameter, which is the implicit `void*`
		Decl *first = decl_new_var(kw_self, decl->span, NULL, VARDECL_PARAM);
		first->type = type_voidptr;
		first->var.kind = VARDECL_PARAM;
		first->unit = context->unit;
		first->resolve_status = RESOLVE_DONE;
		first->alignment = type_abi_alignment(type_voidptr);
		vec_insert_first(method->func_decl.signature.params, first);
		method->unit = context->unit;
		method->func_decl.signature.vararg_index += 1;

		// Now we analyse the function as a regular function.
		if (!sema_analyse_func(context, method, &erase))
		{
			// This is necessary in order to allow this check to run again.
			decl_poison(method);
			vec_erase_ptr_at(method->func_decl.signature.params, 0);
			return false;
		}

		// We might need to erase the function.
		if (erase)
		{
			vec_erase_ptr_at(functions, i);
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
				decl_poison(method);
				return false;
			}
		}
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
		case TYPE_ANY:
		case TYPE_INTERFACE:
		case TYPE_ANYFAULT:
		case TYPE_TYPEID:
		case TYPE_ENUM:
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_BITSTRUCT:
		case TYPE_FAULTTYPE:
		case TYPE_DISTINCT:
		case TYPE_VECTOR:
		case TYPE_INFERRED_VECTOR:
		case TYPE_UNTYPED_LIST:
		case TYPE_WILDCARD:
		case TYPE_TYPEINFO:
		case TYPE_MEMBER:
			return true;
		case TYPE_TYPEDEF:
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
	if (!sema_resolve_implemented_interfaces(context, decl, false)) return decl_poison(decl);
	if (*erase_decl) return true;
	DEBUG_LOG("Beginning analysis of %s.", decl->name ? decl->name : ".anon");
	if (!sema_resolve_type_info(context, decl->bitstruct.base_type, RESOLVE_TYPE_DEFAULT)) return false;
	Type *type = decl->bitstruct.base_type->type->canonical;
	Type *base_type = type->type_kind == TYPE_ARRAY ? type->array.base : type;
	if (!type_is_integer(base_type))
	{
		SEMA_ERROR(decl->bitstruct.base_type, "The type of the bitstruct cannot be %s but must be an integer or an array of integers.",
				   type_quoted_error_string(decl->bitstruct.base_type->type));
		return false;
	}
	Decl **members = decl->bitstruct.members;
	unsigned member_count = vec_size(members);

	Decl **state = decl->name ? sema_decl_stack_store() : NULL;
	for (unsigned i = 0; i < member_count; i++)
	{
		AGAIN:;
		Decl *member = members[i];
		if (!decl_ok(member)) goto ERROR;
		bool erase_decl_member = false;
		if (!sema_analyse_bitstruct_member(context, decl, member, i, decl->bitstruct.overlap, &erase_decl_member)) goto ERROR;
		if (erase_decl_member)
		{
			vec_erase_ptr_at(members, i);
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

static inline bool sema_analyse_signature(SemaContext *context, Signature *sig, TypeInfoId type_parent, bool is_export)
{
	Variadic variadic_type = sig->variadic;
	Decl **params = sig->params;
	unsigned param_count = vec_size(params);
	unsigned vararg_index = sig->vararg_index;
	bool is_macro = sig->is_macro;
	bool is_macro_at_name = sig->is_at_macro || sig->is_safemacro;
	// Check return type
	assert(sig->rtype || sig->is_macro);
	Type *rtype = NULL;
	if (sig->rtype)
	{
		TypeInfo *rtype_info = type_infoptr(sig->rtype);
		if (!sema_resolve_type_info(context, type_infoptr(sig->rtype),
		                            is_macro ? RESOLVE_TYPE_ALLOW_INFER
		                                     : RESOLVE_TYPE_DEFAULT)) return false;
		rtype = rtype_info->type;
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

	TypeInfo *method_parent = type_infoptrzero(type_parent);
	if (method_parent)
	{
		if (!sema_resolve_type_info(context, method_parent,
		                            is_macro ? RESOLVE_TYPE_MACRO_METHOD : RESOLVE_TYPE_FUNC_METHOD)) return false;
	}
	// Fill in the type if the first parameter is lacking a type.
	if (method_parent && params && params[0] && !params[0]->var.type_info)
	{
		Decl *param = params[0];
		Type *inferred_type = NULL;
		switch (param->var.kind)
		{
			case VARDECL_PARAM_REF:
				inferred_type = type_get_ptr(method_parent->type);
				if (!is_macro) param->var.kind = VARDECL_PARAM;
				break;
			case VARDECL_PARAM:
				inferred_type = method_parent->type;
				break;
			default:
				goto CHECK_PARAMS;
		}
		param->var.type_info = type_info_id_new_base(inferred_type, param->span);
	}

	CHECK_PARAMS:

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
		if (i == 0 && param->resolve_status == RESOLVE_DONE)
		{
			assert(param->type == type_voidptr && "Expected the first parameter of an interface method.");
			continue;
		}

		assert(param->resolve_status == RESOLVE_NOT_DONE && "The param shouldn't have been resolved yet.");
		param->resolve_status = RESOLVE_RUNNING;
		bool erase = false;
		if (!sema_analyse_attributes(context, param, param->attributes, ATTR_PARAM, &erase))
		{
			return decl_poison(param);
		}
		assert(!erase);
		param->unit = context->unit;
		assert(param->decl_kind == DECL_VAR);
		VarDeclKind var_kind = param->var.kind;
		TypeInfo *type_info = type_infoptrzero(param->var.type_info);
		if (type_info)
		{
			if (!sema_resolve_type_info(context, type_info,
			                            is_macro ? RESOLVE_TYPE_ALLOW_INFER
			                                     : RESOLVE_TYPE_DEFAULT)) return decl_poison(param);
			param->type = type_info->type;
		}
		if (type_info && param->var.no_alias && !type_is_pointer(param->type) && type_flatten(param->type)->type_kind != TYPE_SLICE)
		{
			SEMA_ERROR(param, "The parameter was set to @noalias, but it was neither a slice nor a pointer. You need to either remove '@noalias' or use pointer/slice type.");
			return decl_poison(param);
		}
		switch (var_kind)
		{
			case VARDECL_PARAM_REF:
				if (type_info && !type_is_pointer(param->type))
				{
					SEMA_ERROR(type_info, "A pointer type was expected for a ref argument, did you mean %s?",
							   type_quoted_error_string(type_get_ptr(param->type)));
					return decl_poison(param);
				}
				FALLTHROUGH;
			case VARDECL_PARAM_EXPR:
				if (!is_macro)
				{
					SEMA_ERROR(param, "Only regular parameters are allowed for functions.");
					return decl_poison(param);
				}
				if (!is_macro_at_name && (!type_parent || i != 0 || var_kind != VARDECL_PARAM_REF))
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
				if (!param->type && !is_macro)
				{
					RETURN_SEMA_ERROR(param, "Only typed parameters are allowed for functions.");
				}
				bool erase_decl = false;
				if (!sema_analyse_attributes_for_var(context, param, &erase_decl)) return false;
				assert(!erase_decl);
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
			if (var_kind != VARDECL_PARAM)
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
			if (!sema_set_abi_alignment(context, param->type, &param->alignment)) return false;
		}

		if (param->var.init_expr)
		{
			Expr *expr = param->var.init_expr;
			if (expr_is_const(expr))
			{
				if (!sema_analyse_expr_rhs(context, param->type, expr, true, NULL, false)) return decl_poison(param);
			}
		}
		if (!sema_check_param_uniqueness_and_type(context, params, param, i, param_count)) return decl_poison(param);
		param->resolve_status = RESOLVE_DONE;
	}
	return true;
}

bool sema_analyse_function_signature(SemaContext *context, Decl *func_decl, CallABI abi, Signature *signature)
{
	// Get param count and variadic type
	Decl **params = signature->params;

	if (!sema_analyse_signature(context, signature, func_decl->func_decl.type_parent, func_decl->is_export)) return false;

	Variadic variadic_type = signature->variadic;

	// Remove the last empty value.
	if (variadic_type == VARIADIC_RAW)
	{
		assert(params && !params[signature->vararg_index] && "The last parameter must have been a raw variadic.");
		assert(signature->vararg_index == vec_size(params) - 1);
		vec_pop(params);
	}

	Type **types = NULL;
	unsigned param_count = vec_size(params);

	for (unsigned i = 0; i < param_count; i++)
	{
		assert(IS_RESOLVED(params[i]));
		assert(params[i]->type->canonical);
		vec_add(types, params[i]->type);
	}

	Type *raw_type = sema_resolve_type_get_func(signature, abi);
	assert(func_decl->type->type_kind == TYPE_FUNC_RAW);
	assert(raw_type->function.prototype);
	func_decl->type->function.prototype = raw_type->function.prototype;
	return true;
}

static inline bool sema_analyse_fntype(SemaContext *context, Decl *decl, bool *erase_decl)
{
	if (!sema_analyse_attributes(context, decl, decl->attributes, ATTR_DEF, erase_decl)) return decl_poison(decl);
	if (*erase_decl) return true;
	Signature *sig = &decl->fntype_decl;
	return sema_analyse_function_signature(context, decl, sig->abi, sig);
}

static inline bool sema_analyse_typedef(SemaContext *context, Decl *decl, bool *erase_decl)
{
	if (!sema_analyse_attributes(context, decl, decl->attributes, ATTR_DEF, erase_decl)) return decl_poison(decl);
	if (*erase_decl) return true;

	bool is_export = decl->is_export;
	if (decl->typedef_decl.is_func)
	{
		Decl *fn_decl = decl->typedef_decl.decl;
		fn_decl->is_export = is_export;
		fn_decl->unit = decl->unit;
		fn_decl->type = type_new_func(fn_decl, &fn_decl->fntype_decl);
		decl->type->canonical = type_get_func_ptr(fn_decl->type);
		return true;
	}
	TypeInfo *info = decl->typedef_decl.type_info;
	if (!sema_resolve_type_info(context, info, RESOLVE_TYPE_DEFAULT)) return false;
	decl->type->canonical = info->type->canonical;
	// Do we need anything else?
	return true;
}

/**
 * Analyse a distinct type.
 */
static inline bool sema_analyse_distinct(SemaContext *context, Decl *decl, bool *erase)
{
	// Check the attributes on the distinct type.
	if (!sema_analyse_attributes(context, decl, decl->attributes, ATTR_DISTINCT, erase)) return false;

	// Erase it?
	if (*erase) return true;

	// Check the interfaces.
	if (!sema_resolve_implemented_interfaces(context, decl, false)) return false;

	// Infer the underlying type normally.
	TypeInfo *info = decl->distinct;
	if (!sema_resolve_type_info(context, info, RESOLVE_TYPE_DEFAULT)) return false;

	// Optional isn't allowed of course.
	if (type_is_optional(info->type)) RETURN_SEMA_ERROR(decl, "You cannot create a distinct type from an optional.");

	// Distinct types drop the canonical part.
	info->type = info->type->canonical;
	return true;
}

static inline bool sema_analyse_enum_param(SemaContext *context, Decl *param)
{
	assert(param->decl_kind == DECL_VAR && param->var.kind == VARDECL_PARAM && param->var.type_info);
	if (vec_size(param->attributes))
	{
		RETURN_SEMA_ERROR(param->attributes[0], "There are no valid attributes for associated values.");
	}
	TypeInfo *type_info = type_infoptrzero(param->var.type_info);
	if (!sema_resolve_type_info(context, type_info, RESOLVE_TYPE_DEFAULT)) return false;
	assert(!param->var.vararg);
	param->type = type_info->type;
	assert(param->name);
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
	assert(!param->var.init_expr);
	return sema_set_abi_alignment(context, param->type, &param->alignment);
}

static inline bool sema_analyse_enum(SemaContext *context, Decl *decl, bool *erase_decl)
{
	if (!sema_analyse_attributes(context, decl, decl->attributes, ATTR_ENUM, erase_decl)) return decl_poison(decl);
	if (*erase_decl) return true;
	if (!sema_resolve_implemented_interfaces(context, decl, false)) return decl_poison(decl);

	// Resolve the type of the enum.
	if (!sema_resolve_type_info(context, decl->enums.type_info, RESOLVE_TYPE_DEFAULT)) return false;

	Type *type = decl->enums.type_info->type;
	assert(!type_is_optional(type) && "Already stopped when parsing.");

	Type *flat_underlying_type = type_flatten(type);
	// Require an integer type
	if (!type_is_integer(flat_underlying_type))
	{
		SEMA_ERROR(decl->enums.type_info, "The enum type must be an integer type not '%s'.", type_to_error_string(type));
		return false;
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

		bool erase_val = false;
		if (!sema_analyse_attributes(context, decl, enum_value->attributes, ATTR_ENUM, &erase_val)) return decl_poison(decl);

		if (erase_val)
		{
			if (enums == 1)
			{
				SEMA_ERROR(decl, "No enum values left in enum after @if resolution, there must be at least one.");
				return decl_poison(decl);
			}
			vec_erase_ptr_at(enum_values, i);
			enums--;
			i--;
			continue;
		}
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

		Int val = (Int){ value, flat_underlying_type->type_kind };
		if (!int_fits(val, flat_underlying_type->type_kind))
		{
			SEMA_ERROR(enum_value,
					   "The enum value would implicitly be %s which does not fit in %s.",
					   i128_to_string(value, 10, type_is_signed(flat_underlying_type), false),
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
				RETURN_SEMA_ERROR(args[0], "No associated values are defined for this enum.");
			}
			RETURN_SEMA_ERROR(args[associated_value_count], "Only %d associated value(s) may be defined for this enum.");
		}
		if (arg_count < associated_value_count)
		{
			RETURN_SEMA_ERROR(enum_value, "Expected %d associated value(s) for this enum.", associated_value_count);
			return false;
		}
		for (unsigned j = 0; j < arg_count; j++)
		{
			Expr *arg = args[j];

			if (!sema_analyse_expr_rhs(context, associated_values[j]->type, arg, false, NULL, false)) return false;
			if (!expr_is_constant_eval(arg, CONSTANT_EVAL_GLOBAL_INIT))
			{
				SEMA_ERROR(arg, "Expected a constant expression as parameter.");
				return false;
			}
		}
		enum_value->resolve_status = RESOLVE_DONE;
	}
	return success;
ERR:
	sema_decl_stack_restore(state);
	return false;
}

static inline bool sema_analyse_error(SemaContext *context, Decl *decl, bool *erase_decl)
{
	if (!sema_analyse_attributes(context, decl, decl->attributes, ATTR_FAULT, erase_decl)) return decl_poison(decl);
	if (*erase_decl) return true;
	if (!sema_resolve_implemented_interfaces(context, decl, false)) return decl_poison(decl);

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

static inline Decl *operator_in_module(SemaContext *c, Module *module, OperatorOverload operator_overload)
{
	if (module->is_generic) return NULL;
	Decl **extensions = module->private_method_extensions;
	FOREACH(Decl *, extension, extensions)
	{
		if (extension->operator == operator_overload)
		{
			unit_register_external_symbol(c, extension);
			return extension;
		}
	}
	FOREACH(Module *, sub_module, module->sub_modules)
	{
		return operator_in_module(c, sub_module, operator_overload);
	}
	return NULL;
}

Decl *sema_find_operator(SemaContext *context, Type *type, OperatorOverload operator_overload)
{
	type = type->canonical;
	if (!type_may_have_sub_elements(type)) return NULL;
	Decl *def = type->decl;
	FOREACH(Decl *, func, def->methods)
	{
		if (func->operator == operator_overload)
		{
			unit_register_external_symbol(context, func);
			return func;
		}
	}
	Decl *extension = operator_in_module(context, context->compilation_unit->module, operator_overload);
	if (extension) return extension;

	FOREACH(Decl *, import, context->unit->imports)
	{
		extension = operator_in_module(context, import->import.module, operator_overload);
		if (extension) return extension;
	}
	return NULL;
}


static inline bool sema_analyse_operator_element_at(SemaContext *context, Decl *method)
{
	TypeInfo *rtype;
	Decl **params;
	if (!sema_analyse_operator_common(context, method, &rtype, &params, 2)) return false;
	switch (type_storage_type(rtype->type))
	{
		case STORAGE_NORMAL:
			break;
		case STORAGE_VOID:
		case STORAGE_WILDCARD:
			RETURN_SEMA_ERROR(rtype, "The return type cannot be 'void'.");
		case STORAGE_COMPILE_TIME:
			RETURN_SEMA_ERROR(rtype, "The return type is %s which is a compile time type, which isn't allowed.",
			                  type_quoted_error_string(rtype->type));
		case STORAGE_UNKNOWN:
			RETURN_SEMA_ERROR(rtype, "%s has unknown size and cannot be used as a return type.",
			                  type_quoted_error_string(rtype->type));
	}
	if (method->operator == OVERLOAD_ELEMENT_REF && !type_is_pointer(rtype->type))
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

static inline bool sema_analyse_operator_len(Decl *method, SemaContext *context)
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
	switch (method->operator)
	{
		case OVERLOAD_ELEMENT_SET:
			return sema_analyse_operator_element_set(context, method);
		case OVERLOAD_ELEMENT_AT:
		case OVERLOAD_ELEMENT_REF:
			return sema_analyse_operator_element_at(context, method);
		case OVERLOAD_LEN:
			return sema_analyse_operator_len(method, context);
	}
	UNREACHABLE
}

bool sema_decl_if_cond(SemaContext *context, Decl *decl)
{
	Attr *attr = attr_find_kind(decl->attributes, ATTRIBUTE_IF);
	decl->is_if = true;
	assert(attr);
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

INLINE Attr* method_find_overload_attribute(Decl *method)
{
	FOREACH(Attr *, attr, method->attributes)
	{
		if (attr->attr_kind == ATTRIBUTE_OPERATOR) return attr;
	}
	UNREACHABLE
}

static inline bool
unit_add_base_extension_method(SemaContext *context, CompilationUnit *unit, Type *parent_type, Decl *method)
{
	// We don't support operator overloading on base types, because
	// there seems little use for it frankly.
	if (method->operator)
	{
		RETURN_SEMA_ERROR(method_find_overload_attribute(method),
						  "Only user-defined types support operator oveloading.");
	}

	// Add it to the right list of extensions.
	switch (method->visibility)
	{
		case VISIBLE_PUBLIC:
			vec_add(global_context.method_extensions, method);
			break;
		case VISIBLE_PRIVATE:
			vec_add(unit->module->private_method_extensions, method);
			break;
		case VISIBLE_LOCAL:
			vec_add(unit->local_method_extensions, method);
			break;
	}
	DEBUG_LOG("Method-like '%s.%s' analysed.", parent_type->name, method->name);
	return true;
}

static inline void sema_get_overload_arguments(Decl *method, Type **value_ref, Type **index_ref)
{
	switch (method->operator)
	{
		case OVERLOAD_LEN:
			UNREACHABLE
		case OVERLOAD_ELEMENT_AT:
			*value_ref = type_no_optional(typeget(method->func_decl.signature.rtype)->canonical);
			*index_ref = method->func_decl.signature.params[1]->type->canonical;
			return;
		case OVERLOAD_ELEMENT_REF:
			*value_ref = type_no_optional(typeget(method->func_decl.signature.rtype)->canonical->pointer);
			*index_ref = method->func_decl.signature.params[1]->type->canonical;
			return;
		case OVERLOAD_ELEMENT_SET:
			*value_ref = method->func_decl.signature.params[2]->type->canonical;
			*index_ref = method->func_decl.signature.params[1]->type->canonical;
			return;
		default:
			UNREACHABLE
	}
}

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
	OperatorOverload operator = method->operator;
	Decl *other = sema_find_operator(context, parent_type, operator);
	if (other)
	{
		Attr *attr = method_find_overload_attribute(method);
		SEMA_ERROR(attr->exprs[0], "This operator is already defined for '%s'.", parent_type->name);
		SEMA_NOTE(other, "The previous definition was here.");
		return false;
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
			other = sema_find_operator(context, parent_type, OVERLOAD_ELEMENT_REF);
			if (other && decl_ok(other))
			{
				sema_get_overload_arguments(other, &value, &index_type);
				break;
			}
			// And []=
			other = sema_find_operator(context, parent_type, OVERLOAD_ELEMENT_SET);
			if (other && decl_ok(other))
			{
				sema_get_overload_arguments(other, &value, &index_type);
				break;
			}
			return true;
		case OVERLOAD_ELEMENT_REF:
			// &[] compares []
			other = sema_find_operator(context, parent_type, OVERLOAD_ELEMENT_AT);
			if (other && decl_ok(other))
			{
				sema_get_overload_arguments(other, &value, &index_type);
				break;
			}
			// And []=
			other = sema_find_operator(context, parent_type, OVERLOAD_ELEMENT_SET);
			if (other && decl_ok(other))
			{
				sema_get_overload_arguments(other, &value, &index_type);
				break;
			}
			return true;
		case OVERLOAD_ELEMENT_SET:
			// []= compares &[]
			other = sema_find_operator(context, parent_type, OVERLOAD_ELEMENT_REF);
			if (other && decl_ok(other))
			{
				sema_get_overload_arguments(other, &value, &index_type);
				break;
			}
			// And []
			other = sema_find_operator(context, parent_type, OVERLOAD_ELEMENT_AT);
			if (other && decl_ok(other))
			{
				sema_get_overload_arguments(other, &value, &index_type);
				break;
			}
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
static inline bool unit_add_method(SemaContext *context, Type *parent_type, Decl *method)
{
	CompilationUnit *unit = context->unit;
	assert(parent_type->canonical == parent_type);
	const char *name = method->name;

	// Did we already define it externally?
	Decl *other = sema_find_extension_method_in_list(unit->local_method_extensions, parent_type, name);
	if (!other) sema_find_extension_method_in_list(unit->module->private_method_extensions, parent_type, name);
	if (!other) sema_find_extension_method_in_list(global_context.method_extensions, parent_type, name);
	if (other)
	{
		SEMA_ERROR(method, "This %s is already defined.", method_name_by_decl(method));
		SEMA_NOTE(other, "The previous definition was here.");
		return false;
	}

	// Is it a base extension?
	if (!type_is_user_defined(parent_type)) return unit_add_base_extension_method(context, unit, parent_type, method);

	// Resolve it as a user-defined type extension.
	Decl *parent = parent_type->decl;
	Decl *ambiguous = NULL;
	Decl *private = NULL;

	// If we found it, issue an error.
	other = sema_resolve_method(unit, parent, name, &ambiguous, &private);
	if (other)
	{
		SEMA_ERROR(method, "This %s is already defined for '%s'.",
		           method_name_by_decl(method), parent_type->name);
		SEMA_NOTE(other, "The previous definition was here.");
		return false;
	}

	// Is it an operator?
	if (method->operator)
	{
		if (!sema_analyse_operator_method(context, parent_type, method)) return false;
	}

	DEBUG_LOG("Method-like '%s.%s' analysed.", parent->name, method->name);

	// Add it to the correct place: type methods, private extensions, local method extensions
	switch (method->visibility)
	{
		case VISIBLE_PUBLIC:
			vec_add(parent->methods, method);
			break;
		case VISIBLE_PRIVATE:
			if (parent->unit->module == unit->module && parent->visibility >= VISIBLE_PRIVATE)
			{
				vec_add(parent->methods, method);
				break;
			}
			vec_add(unit->module->private_method_extensions, method);
			break;
		case VISIBLE_LOCAL:
			if (parent->unit == unit && parent->visibility >= VISIBLE_LOCAL)
			{
				vec_add(parent->methods, method);
				break;
			}
			vec_add(unit->local_method_extensions, method);
			break;
		default:
			UNREACHABLE
	}
	return true;
}

/**
 * Find an interface name by searching through its own interfaces as well as any
 * parents to the interface.
 */
static Decl *sema_interface_method_by_name(Decl *interface, const char *name)
{
	FOREACH(Decl *, method, interface->interface_methods)
	{
		if (method->name == name) return method;
	}
	FOREACH(TypeInfo *, parent_type, interface->interfaces)
	{
		Decl *res = sema_interface_method_by_name(parent_type->type->decl, name);
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
		case TYPE_DISTINCT:
		case TYPE_FAULTTYPE:
		case TYPE_ENUM:
			break;
		case TYPE_TYPEDEF:
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
		Decl *interface = proto->type->decl;
		Decl *match = sema_interface_method_by_name(interface, name);
		if (!match) continue;

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
		else
		{
			SEMA_ERROR(this_params[any_param_count], "This function has too many parameters (%d).", this_param_count);
			SEMA_NOTE(decl, "Compare with the interface, which has only %d parameter%s.",
			          any_param_count, any_param_count == 1 ? "" : "s");
		}
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
 * 3. Resolve the parent type.
 * 4. Resolve the declaration of the parent (as needed).
 * 5. Check that it has at least one parameter.
 * 6. Check that this parameter is correct.
 * 7. If it is dynamic, the type may not be an interface or any
 * 8. If it is dynamic, make sure that it implements an interface correctly if available
 * 9. Try adding the method
 */
static inline bool sema_analyse_method(SemaContext *context, Decl *decl)
{
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
	if (!sema_resolve_type_info(context, parent_type, RESOLVE_TYPE_FUNC_METHOD)) return false;
	Type *par_type = parent_type->type->canonical;

	// Resolve declaration of parent as needed.
	if (!sema_resolve_type_decl(context, par_type)) return false;
	Decl **params = decl->func_decl.signature.params;
	bool is_dynamic = decl->func_decl.attr_dynamic;

	// Ensure it has at least one parameter.
	if (!vec_size(params)) RETURN_SEMA_ERROR(decl, "A method must start with an argument of the type "
												   "it is a method of, e.g. 'fn void %s.%s(%s* self)'.",
	                                         type_to_error_string(par_type), decl->name, type_to_error_string(par_type));

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
			if (!sema_compare_method_with_interface(context, decl, implemented_method)) return false;
			decl->func_decl.interface_method = declid(implemented_method);
		}
		else
		{
			decl->func_decl.interface_method = 0;
		}
	}
	return unit_add_method(context, par_type, decl);
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
			return "fault";
		case ATTR_DEF:
			return "def";
		case ATTR_CALL:
			return "call";
		case ATTR_DISTINCT:
			return "distinct";
		case ATTR_INTERFACE_METHOD:
			return "interface method";
	}
	UNREACHABLE
}

// Helper method
INLINE bool update_abi(Decl *decl, CallABI abi)
{
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
	CallABI abi;
	// C decl is easy
	if (str_eq(str, "cdecl")) return update_abi(decl, CALL_C);

	// Check veccall
	if (str_eq(str, "veccall"))
	{
		switch (platform_target.arch)
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
		if (platform_target.arch == ARCH_TYPE_ARM || platform_target.arch == ARCH_TYPE_ARMB)
		{
			return update_abi(decl, CALL_AAPCS);
		}
		return true;
	}
	RETURN_SEMA_ERROR(expr, "Unknown call convention, only 'cdecl', 'stdcall' and 'veccall' are supported");
}

#define EXPORTED_USER_DEFINED_TYPES (ATTR_ENUM | ATTR_UNION | ATTR_STRUCT | ATTR_FAULT)
#define CALLABLE_TYPE (ATTR_FUNC | ATTR_INTERFACE_METHOD | ATTR_MACRO)
#define USER_DEFINED_TYPES EXPORTED_USER_DEFINED_TYPES | ATTR_BITSTRUCT | ATTR_DISTINCT

/**
 * Analyse almost all attributes.
 */
static bool sema_analyse_attribute(SemaContext *context, Decl *decl, Attr *attr, AttributeDomain domain, bool *erase_decl)
{
	AttributeType type = attr->attr_kind;
	assert(type >= 0 && type < NUMBER_OF_ATTRIBUTES);
	// NOLINTBEGIN(*.EnumCastOutOfRange)
	static AttributeDomain attribute_domain[NUMBER_OF_ATTRIBUTES] = {
			[ATTRIBUTE_ALIGN] = ATTR_FUNC | ATTR_CONST | ATTR_LOCAL | ATTR_GLOBAL | ATTR_BITSTRUCT | ATTR_STRUCT | ATTR_UNION | ATTR_MEMBER, // NOLINT
			[ATTRIBUTE_BENCHMARK] = ATTR_FUNC,
			[ATTRIBUTE_BIGENDIAN] = ATTR_BITSTRUCT,
			[ATTRIBUTE_BUILTIN] = ATTR_MACRO | ATTR_FUNC | ATTR_GLOBAL | ATTR_CONST,
			[ATTRIBUTE_CALLCONV] = ATTR_FUNC | ATTR_INTERFACE_METHOD,
			[ATTRIBUTE_COMPACT] = ATTR_STRUCT | ATTR_UNION,
			[ATTRIBUTE_CONST] = ATTR_MACRO,
			[ATTRIBUTE_DEPRECATED] = USER_DEFINED_TYPES | CALLABLE_TYPE | ATTR_CONST | ATTR_GLOBAL | ATTR_MEMBER | ATTR_BITSTRUCT_MEMBER | ATTR_INTERFACE,
			[ATTRIBUTE_DYNAMIC] = ATTR_FUNC,
			[ATTRIBUTE_EXPORT] = ATTR_FUNC | ATTR_GLOBAL | ATTR_CONST | USER_DEFINED_TYPES | ATTR_DEF,
			[ATTRIBUTE_EXTERN] = ATTR_FUNC | ATTR_GLOBAL | ATTR_CONST | USER_DEFINED_TYPES,
			[ATTRIBUTE_FINALIZER] = ATTR_FUNC,
			[ATTRIBUTE_IF] = (AttributeDomain)~(ATTR_CALL | ATTR_LOCAL | ATTR_PARAM),
			[ATTRIBUTE_INIT] = ATTR_FUNC,
			[ATTRIBUTE_INLINE] = ATTR_FUNC | ATTR_CALL,
			[ATTRIBUTE_LINK] = ATTR_FUNC | ATTR_MACRO | ATTR_CONST | ATTR_GLOBAL,
			[ATTRIBUTE_LITTLEENDIAN] = ATTR_BITSTRUCT,
			[ATTRIBUTE_LOCAL] = ATTR_FUNC | ATTR_MACRO | ATTR_GLOBAL | ATTR_CONST | USER_DEFINED_TYPES | ATTR_DEF | ATTR_INTERFACE,
			[ATTRIBUTE_MAYDISCARD] = CALLABLE_TYPE,
			[ATTRIBUTE_NAKED] = ATTR_FUNC,
			[ATTRIBUTE_NOALIAS] = ATTR_PARAM,
			[ATTRIBUTE_NODISCARD] = CALLABLE_TYPE,
			[ATTRIBUTE_NOINIT] = ATTR_GLOBAL | ATTR_LOCAL,
			[ATTRIBUTE_NOINLINE] = ATTR_FUNC | ATTR_CALL,
			[ATTRIBUTE_NOPADDING] = ATTR_STRUCT | ATTR_UNION | ATTR_MEMBER,
			[ATTRIBUTE_NORETURN] = CALLABLE_TYPE,
			[ATTRIBUTE_NOSTRIP] = ATTR_FUNC | ATTR_GLOBAL | ATTR_CONST | EXPORTED_USER_DEFINED_TYPES,
			[ATTRIBUTE_OBFUSCATE] = ATTR_ENUM | ATTR_FAULT,
			[ATTRIBUTE_OPERATOR] = ATTR_MACRO | ATTR_FUNC,
			[ATTRIBUTE_OPTIONAL] = ATTR_INTERFACE_METHOD,
			[ATTRIBUTE_OVERLAP] = ATTR_BITSTRUCT,
			[ATTRIBUTE_PACKED] = ATTR_STRUCT | ATTR_UNION,
			[ATTRIBUTE_PRIVATE] = ATTR_FUNC | ATTR_MACRO | ATTR_GLOBAL | ATTR_CONST | USER_DEFINED_TYPES | ATTR_DEF | ATTR_INTERFACE,
			[ATTRIBUTE_PUBLIC] = ATTR_FUNC | ATTR_MACRO | ATTR_GLOBAL | ATTR_CONST | USER_DEFINED_TYPES | ATTR_DEF | ATTR_INTERFACE,
			[ATTRIBUTE_PURE] = ATTR_CALL,
			[ATTRIBUTE_REFLECT] = ATTR_FUNC | ATTR_GLOBAL | ATTR_CONST | USER_DEFINED_TYPES,
			[ATTRIBUTE_SAFEMACRO] = ATTR_MACRO,
			[ATTRIBUTE_SECTION] = ATTR_FUNC | ATTR_CONST | ATTR_GLOBAL,
			[ATTRIBUTE_TEST] = ATTR_FUNC,
			[ATTRIBUTE_UNUSED] = (AttributeDomain)~(ATTR_CALL),
			[ATTRIBUTE_USED] = (AttributeDomain)~(ATTR_CALL),
			[ATTRIBUTE_WASM] = ATTR_FUNC,
			[ATTRIBUTE_WEAK] = ATTR_FUNC | ATTR_CONST | ATTR_GLOBAL | ATTR_DEF,
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
	if (args > 1 && type != ATTRIBUTE_LINK)
	{
		SEMA_ERROR(attr->exprs[1], "Too many arguments for the attribute.");
		return false;
	}
	Expr *expr = args ? attr->exprs[0] : NULL;

	switch (type)
	{
		case ATTRIBUTE_PRIVATE:
		case ATTRIBUTE_PUBLIC:
		case ATTRIBUTE_LOCAL:
		case ATTRIBUTE_BUILTIN:
			// These are pseudo-attributes and are processed separately.
			UNREACHABLE;
		case ATTRIBUTE_DEPRECATED:
			// We expect an optional string.
			if (expr)
			{
				if (!sema_analyse_expr(context, expr)) return false;
				if (!expr_is_const_string(expr))
				{
					RETURN_SEMA_ERROR(expr, "Expected a constant string value as argument.");
				}
			}
			decl->is_deprecated = true;
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
			if (expr && !sema_analyse_expr(context, expr)) return false;
			if (!expr_is_const_string(expr)) RETURN_SEMA_ERROR(expr, "Expected a constant string value as argument.");
			if (!update_call_abi_from_string(context, decl, expr)) return false;
			return true;
		case ATTRIBUTE_BENCHMARK:
			decl->func_decl.attr_benchmark = true;
			break;
		case ATTRIBUTE_TEST:
			decl->func_decl.attr_test = true;
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
					decl->operator = expr->overload_expr;
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
			RETURN_SEMA_ERROR(attr, "'operator' requires an operator type argument: '[]', '[]=', '&[]' or 'len'.");
		}
		case ATTRIBUTE_ALIGN:
			if (!expr)
			{
				RETURN_SEMA_ERROR(attr, "'align' requires an power-of-2 argument, e.g. align(8).");
			}
			if (!sema_analyse_expr(context, expr)) return false;
			if (!expr_is_const_int(expr))
			{
				RETURN_SEMA_ERROR(expr, "Expected a constant integer value as argument.");
			}
			{
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
				decl->alignment = (AlignSize)align;
				return true;
			}
		case ATTRIBUTE_EXPORT:
			if (context->unit->module->is_generic)
			{
				RETURN_SEMA_ERROR(attr, "'@export' is not allowed in generic modules.");
			}
			if (expr)
			{
				if (!sema_analyse_expr(context, expr)) return false;
				if (!expr_is_const_string(expr))
				{
					RETURN_SEMA_ERROR(expr, "Expected a constant string value as argument.");
				}
				if (decl->has_extname)
				{
					RETURN_SEMA_ERROR(expr, "An external name is already defined, please use '@extern` without an argument.");
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
			if (!sema_analyse_expr(context, expr)) return false;
			if (expr->type->canonical != type_bool || !sema_cast_const(expr))
			{
				RETURN_SEMA_ERROR(expr, "Expected a boolean compile time constant value.");
			}
			if (!expr->const_expr.b) *erase_decl = true;
			return true;
		case ATTRIBUTE_FINALIZER:
			decl->func_decl.attr_finalizer = true;
			// Ugly
			goto PARSE;
		case ATTRIBUTE_LINK:
			if (args < 1) RETURN_SEMA_ERROR(attr, "'@link' requires at least one argument.");
			Expr *cond = args > 1 ? attr->exprs[0] : NULL;
			if (cond && !sema_analyse_expr(context, cond)) return false;
			int start = 0;
			decl->has_link = true;
			if (cond && expr_is_const_bool(cond))
			{
				start = 1;
				decl->has_link = cond->const_expr.b;
			}
			for (unsigned i = start; i < args; i++)
			{
				Expr *string = attr->exprs[i];
				if (!sema_analyse_expr(context, string)) return false;
				if (!expr_is_const_string(string)) RETURN_SEMA_ERROR(string, "Expected a constant string here, usage is: '@link(cond1, link1, link2, ...)'.");
			}
			// Erase if not applicable.
			if (start == 1) attr->exprs[0] = NULL;
			if (!decl->has_link) attr->exprs = NULL;
			return true;
		case ATTRIBUTE_INIT:
			decl->func_decl.attr_init = true;
		PARSE:;
			if (expr)
			{
				if (!sema_analyse_expr(context, expr)) return false;
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
		case ATTRIBUTE_SECTION:
		case ATTRIBUTE_EXTERN:
			if (context->unit->module->is_generic)
			{
				RETURN_SEMA_ERROR(attr, "'%s' attributes are not allowed in generic modules.", attr->name);
			}
			if (!expr)
			{
				RETURN_SEMA_ERROR(attr, "'%s' requires a string argument, e.g. %s(\"foo\").", attr->name, attr->name);
			}
			if (!sema_analyse_expr(context, expr)) return false;
			if (!expr_is_const_string(expr))
			{
				RETURN_SEMA_ERROR(expr, "Expected a constant string value as argument.");
			}
			switch (type)
			{
				case ATTRIBUTE_SECTION:
					if (!sema_check_section(context, attr)) return false;
					decl->section_id = global_context_register_section(expr->const_expr.bytes.ptr);
					break;
				case ATTRIBUTE_EXTERN:
					decl->has_extname = true;
					decl->extname = expr->const_expr.bytes.ptr;
					break;
				default:
					UNREACHABLE;
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
		case ATTRIBUTE_NOINIT:
			decl->var.no_init = true;
			break;
		case ATTRIBUTE_CONST:
			decl->func_decl.signature.attrs.always_const = true;
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
			if (domain == ATTR_DEF)
			{
				if (decl->decl_kind != DECL_TYPEDEF) RETURN_SEMA_ERROR(attr, "'@weak' can only be used on type aliases.");
				if (!decl->typedef_decl.is_redef)
				{
					RETURN_SEMA_ERROR(attr, "'@weak' is only allowed on type aliases with the same name, eg 'def Foo = bar::def::Foo @weak'.");
				}
			}
			decl->is_weak = true;
			break;
		case ATTRIBUTE_WASM:
			decl->is_export = true;
			break;
		case ATTRIBUTE_NAKED:
			assert(domain == ATTR_FUNC);
			decl->func_decl.attr_naked = true;
			break;
		case ATTRIBUTE_OVERLAP:
			decl->bitstruct.overlap = true;
			break;
		case ATTRIBUTE_DYNAMIC:
			decl->func_decl.attr_dynamic = true;
			break;
		case ATTRIBUTE_BIGENDIAN:
			if (decl->bitstruct.little_endian)
			{
				SEMA_ERROR(attr, "Attribute cannot be combined with @littleendian");
				return decl_poison(decl);
			}
			decl->bitstruct.big_endian = true;
			break;
		case ATTRIBUTE_LITTLEENDIAN:
			if (decl->bitstruct.big_endian)
			{
				SEMA_ERROR(attr, "Attribute cannot be combined with @bigendian");
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
		case ATTRIBUTE_NONE:
			UNREACHABLE
	}
	if (expr) RETURN_SEMA_ERROR(expr, "'%s' should not have any arguments.", attr->name);
	return true;

}


static inline bool sema_analyse_custom_attribute(SemaContext *context, Decl *decl, Attr *attr, AttributeDomain domain,
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
	eval_context.call_env = (CallEnv) { .kind = CALL_ENV_ATTR, .attr_declaration = decl };

	// We copy the compilation unit.
	eval_context.compilation_unit = context->unit;

	// First we need to analyse each expression in the current scope
	for (int j = 0; j < param_count; j++)
	{
		if (!sema_analyse_ct_expr(context, args[j])) goto ERR;

		params[j]->var.init_expr = args[j];
		params[j]->var.kind = VARDECL_CONST;
		// Then add them to the evaluation context.
		// (Yes this is messy)
		sema_add_local(&eval_context, params[j]);
	}
	// Now we've added everything to the evaluation context, so we can (recursively)
	// apply it to the contained attributes, which in turn may be derived attributes.
	if (!sema_analyse_attributes_inner(&eval_context, decl, attributes, domain, top ? top : attr_decl, erase_decl)) goto ERR;
	// Then destroy the eval context.
	sema_context_destroy(&eval_context);
	// Stop evaluating on erase.
	return true;
ERR:
	sema_context_destroy(&eval_context);
	return false;
}

// TODO consider doing this evaluation early, it should be possible.
static bool sema_analyse_attributes_inner(SemaContext *context, Decl *decl, Attr **attrs, AttributeDomain domain,
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
			if (!sema_analyse_custom_attribute(context, decl, attr, domain, top, erase_decl)) return false;
		}
		else
		{
			// The simple case, we have a built in attribute:
			if (!sema_analyse_attribute(context, decl, attr, domain, erase_decl)) return false;
		}
		if (*erase_decl) return true;
	}
	return true;
}

static bool sema_analyse_attributes(SemaContext *context, Decl *decl, Attr **attrs, AttributeDomain domain,
									bool *erase_decl)
{
	return sema_analyse_attributes_inner(context, decl, attrs, domain, NULL, erase_decl);
}

static inline bool sema_analyse_doc_header(SemaContext *context, AstId doc,
										   Decl **params, Decl **extra_params, bool *pure_ref)
{
	while (doc)
	{
		Ast *directive = astptr(doc);
		doc = directive->next;
		ContractKind directive_kind = directive->contract_stmt.kind;
		if (directive_kind == CONTRACT_PURE)
		{
			if (*pure_ref)
			{
				SEMA_ERROR(directive, "Multiple '@pure' declarations, please remove one.");
				return false;
			}
			*pure_ref = true;
			continue;
		}
		if (directive_kind != CONTRACT_PARAM) continue;
		const char *param_name = directive->contract_stmt.param.name;
		Decl *extra_param = NULL;
		Decl *param = NULL;
		FOREACH(Decl *, other_param, params)
		{
			param = other_param;
			if (param && param->name == param_name) goto NEXT;
		}
		FOREACH(Decl *, extra, extra_params)
		{
			param = extra;
			if (param && param->name == param_name) goto NEXT;
		}
		RETURN_SEMA_ERROR(&directive->contract_stmt.param, "There is no parameter '%s', did you misspell it?", param_name);
	NEXT:;
		Type *type = param->type;
		if (type) type = type_flatten(type);
		bool may_be_pointer = !type || type_is_pointer(type) || type_is_any_raw(type);
		if (directive->contract_stmt.param.by_ref)
		{
			if (!may_be_pointer)
			{
				RETURN_SEMA_ERROR(directive, "'&' can only be added to pointer type parameters.");
			}
			param->var.not_null = true;
		}
		switch (directive->contract_stmt.param.modifier)
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
		if (!may_be_pointer && type->type_kind != TYPE_SLICE)
		{
			RETURN_SEMA_ERROR(directive, "'in', 'out' and 'inout' may only be added to pointers and slices.");
		}
ADDED:;
	}
	return true;
}

static inline MainType sema_find_main_type(SemaContext *context, Signature *sig, bool is_winmain)
{
	Decl **params = sig->params;
	unsigned param_count = vec_size(params);
	bool is_win32 = platform_target.os == OS_TYPE_WIN32;
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
			if (arg_type2 != type_get_ptr(type_get_ptr(type_char)))
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

static inline Decl *sema_create_synthetic_main(SemaContext *context, Decl *decl, MainType main, bool int_return, bool err_return, bool is_winmain, bool is_wmain)
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
	if (is_winmain)
	{
		function->extname = kw_winmain;
		params[0] = decl_new_generated_var(type_voidptr, VARDECL_PARAM, decl->span);
		params[1]  = decl_new_generated_var(type_voidptr, VARDECL_PARAM, decl->span);
		params[2]  = decl_new_generated_var(type_get_ptr(type_ushort), VARDECL_PARAM, decl->span);
		params[3]  = decl_new_generated_var(type_cint, VARDECL_PARAM, decl->span);
		param_count = 4;
	}
	else if (is_wmain)
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
	int type = int_return ? 1 : (err_return ? 2 : 0);
	const char *main_invoker;
	switch (main)
	{
		case MAIN_TYPE_ARGS:
			if (is_winmain)
			{
				switch (type)
				{
					case 0 : main_invoker = "@win_to_void_main_args"; goto NEXT;
					case 1 : main_invoker = "@win_to_int_main_args"; goto NEXT;
					case 2 : main_invoker = "@win_to_err_main_args"; goto NEXT;
					default: UNREACHABLE
				}
			}
			else if (is_wmain)
			{
				switch (type)
				{
					case 0 : main_invoker = "@wmain_to_void_main_args"; goto NEXT;
					case 1 : main_invoker = "@wmain_to_int_main_args"; goto NEXT;
					case 2 : main_invoker = "@wmain_to_err_main_args"; goto NEXT;
					default: UNREACHABLE
				}
			}
			else
			{
				switch (type)
				{
					case 0 : main_invoker = "@main_to_void_main_args"; goto NEXT;
					case 1 : main_invoker = "@main_to_int_main_args"; goto NEXT;
					case 2 : main_invoker = "@main_to_err_main_args"; goto NEXT;
					default: UNREACHABLE
				}
			}
		case MAIN_TYPE_NO_ARGS:
			assert(!is_wmain);
			if (is_winmain)
			{
				switch (type)
				{
					case 0 : main_invoker = "@win_to_void_main_noargs"; goto NEXT;
					case 1 : main_invoker = "@win_to_int_main_noargs"; goto NEXT;
					case 2 : main_invoker = "@win_to_err_main_noargs"; goto NEXT;
					default: UNREACHABLE
				}
			}
			switch (type)
			{
				case 0 : main_invoker = "@main_to_void_main"; goto NEXT;
				case 1 : main_invoker = "@main_to_int_main"; goto NEXT;
				case 2 : main_invoker = "@main_to_err_main"; goto NEXT;
				default: UNREACHABLE
			}
		case MAIN_TYPE_WIN:
			assert(is_winmain);
			switch (type)
			{
				case 0 : main_invoker = "@win_to_void_main"; goto NEXT;
				case 1 : main_invoker = "@win_to_int_main"; goto NEXT;
				case 2 : main_invoker = "@win_to_err_main"; goto NEXT;
				default: UNREACHABLE
			}
		default:
			UNREACHABLE;
	}
NEXT:;
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
	assert(decl != context->unit->main_function);
	bool is_winmain = decl->func_decl.attr_winmain;
	bool is_win32 = platform_target.os == OS_TYPE_WIN32;
	if (decl->visibility != VISIBLE_PUBLIC)
	{
		SEMA_ERROR(decl, "A main function must be public.");
		return false;
	}
	Signature *signature = &decl->func_decl.signature;
	TypeInfo *rtype_info = type_infoptr(signature->rtype);
	Type *rtype = rtype_info->type;
	bool is_int_return = true;
	bool is_err_return = false;
	if (!is_err_return && type_is_optional(rtype))
	{
		if (rtype->optional->type_kind != TYPE_VOID)
		{
			SEMA_ERROR(rtype_info, "The return type of 'main' cannot be an optional, unless it is 'void!'.");
			return false;
		}
		is_int_return = false;
		is_err_return = true;
	}

	if (type_is_void(rtype)) is_int_return = false;

	if (is_int_return && type_flatten(rtype) != type_cint)
	{
		RETURN_SEMA_ERROR(rtype_info, "Expected a return type of 'void' or %s.", type_quoted_error_string(type_cint));
	}
	// At this point the style is either MAIN_INT_VOID, MAIN_VOID_VOID or MAIN_ERR_VOID
	MainType type = sema_find_main_type(context, signature, is_winmain);
	if (type == MAIN_TYPE_ERROR) return false;
	if (active_target.type == TARGET_TYPE_TEST || active_target.type == TARGET_TYPE_BENCHMARK) return true;
	Decl *function;
	if (active_target.no_entry)
	{
		function = decl;
		goto REGISTER_MAIN;
	}
	if (type == MAIN_TYPE_RAW && !is_int_return)
	{
		RETURN_SEMA_ERROR(rtype_info, "Int return is required for a C style main.");
	}
	// Suppress winmain on non-win32
	if (platform_target.os != OS_TYPE_WIN32) is_winmain = false;

	if ((type == MAIN_TYPE_RAW || type == MAIN_TYPE_NO_ARGS) && is_int_return && !is_winmain)
	{
		// Int return is pass-through at the moment.
		decl->is_export = true;
		decl->has_extname = true;
		decl->extname = kw_main;
		function = decl;
		goto REGISTER_MAIN;
	}
	bool use_wmain = is_win32 && !is_winmain && type != MAIN_TYPE_NO_ARGS;
	active_target.win.use_win_subsystem = is_winmain && is_win32;
	function = sema_create_synthetic_main(context, decl, type, is_int_return, is_err_return, is_winmain, use_wmain);
	if (!decl_ok(function)) return false;
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

static inline bool sema_analyse_func_macro(SemaContext *context, Decl *decl, AttributeDomain domain, bool *erase_decl)
{
	assert((domain & CALLABLE_TYPE) == domain);
	if (!sema_analyse_attributes(context, decl, decl->attributes, domain,
								 erase_decl)) return decl_poison(decl);
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
		assert(!is_interface_method);
		if (vec_size(sig->params))
		{
			RETURN_SEMA_ERROR(sig->params[0], "%s functions may not take any parameters.",
			           is_init_finalizer ? "'@init' and '@finalizer'" : "'@test' and '@benchmark'");
		}
		TypeInfo *rtype_info = type_infoptr(sig->rtype);
		if (!sema_resolve_type_info(context, rtype_info, RESOLVE_TYPE_DEFAULT)) return false;
		Type *rtype = rtype_info->type;
		if (is_init_finalizer)
		{
			if (rtype->canonical != type_void)
			{
				RETURN_SEMA_ERROR(rtype_info, "'@init' and '@finalizer' functions may only return 'void'.");
			}
		}
		else
		{
			if (type_no_optional(rtype) != type_void)
			{
				RETURN_SEMA_ERROR(rtype_info, "'@test' and '@benchmark' functions may only return 'void' or 'void!'.");
			}
			if (type_is_void(rtype))
			{
				rtype_info->type = type_get_optional(rtype);
			}
		}
	}

	decl->type = type_new_func(decl, sig);
	if (!sema_analyse_function_signature(context, decl, sig->abi, sig)) return decl_poison(decl);
	TypeInfo *rtype_info = type_infoptr(sig->rtype);
	assert(rtype_info);
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
	                             &pure)) return false;
	decl->func_decl.signature.attrs.is_pure = pure;
	if (!sema_set_alloca_alignment(context, decl->type, &decl->alignment)) return false;
	DEBUG_LOG("<<< Function analysis of [%s] successful.", decl_safe_name(decl));
	return true;
}

static inline bool sema_is_valid_method_param(SemaContext *context, Decl *param, Type *parent_type, bool is_dynamic)
{
	assert(parent_type->canonical == parent_type && "Expected already the canonical version.");
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
	if (!sema_resolve_type_info(context, parent_type_info, RESOLVE_TYPE_MACRO_METHOD)) return false;

	// Can the type have methods?
	Type *parent_type = parent_type_info->type;
	if (!type_may_have_method(parent_type))
	{
		RETURN_SEMA_ERROR(parent_type_info, "Methods can not be associated with '%s'", type_to_error_string(parent_type));
	}

	// We need at least one argument (the parent type)
	if (!vec_size(decl->func_decl.signature.params))
	{
		RETURN_SEMA_ERROR(decl, "Expected at least one parameter - of type '%s'.", type_to_error_string(parent_type));
	}

	// Check the first argument.
	Decl *first_param = decl->func_decl.signature.params[0];
	if (!first_param)
	{
		SEMA_ERROR(decl, "The first parameter to this method must be of type '%s'.", type_to_error_string(parent_type));
		return false;
	}
	if (!sema_is_valid_method_param(context, first_param, parent_type->canonical, false)) return false;

	if (first_param->var.kind != VARDECL_PARAM_REF && first_param->var.kind != VARDECL_PARAM)
	{
		RETURN_SEMA_ERROR(first_param, "The first parameter must be a regular or ref (&) type.");
	}
	return unit_add_method(context, parent_type->canonical, decl);
}

INLINE bool sema_analyse_macro_body(SemaContext *context, Decl **body_parameters)
{
	unsigned body_param_count = vec_size(body_parameters);
	for (unsigned i = 0; i < body_param_count; i++)
	{
		assert(body_parameters);
		Decl *param = body_parameters[i];
		assert(param);
		param->resolve_status = RESOLVE_RUNNING;
		assert(param->decl_kind == DECL_VAR);
		TypeInfo *type_info = type_infoptrzero(param->var.type_info);
		VarDeclKind kind = param->var.kind;
		switch (kind)
		{
			case VARDECL_PARAM:
			case VARDECL_PARAM_EXPR:
			case VARDECL_PARAM_CT:
			case VARDECL_PARAM_REF:
			case VARDECL_PARAM_CT_TYPE:
				if (!type_info) break;
				if (!sema_resolve_type_info(context, type_info, RESOLVE_TYPE_DEFAULT)) return false;
				if (kind != VARDECL_PARAM_REF || type_is_pointer(type_info->type)) break;
				RETURN_SEMA_ERROR(type_info, "A pointer type was expected for a ref argument, did you mean %s?",
				                  type_quoted_error_string(type_get_ptr(type_info->type)));
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
		if (!sema_check_param_uniqueness_and_type(context, body_parameters, param, i, body_param_count)) return false;
		param->resolve_status = RESOLVE_DONE;
	}
	return true;
}
static inline bool sema_analyse_macro(SemaContext *context, Decl *decl, bool *erase_decl)
{
	decl->func_decl.unit = context->unit;

	if (!sema_analyse_func_macro(context, decl, ATTR_MACRO, erase_decl)) return false;
	if (*erase_decl) return true;
	if (!sema_analyse_signature(context, &decl->func_decl.signature, decl->func_decl.type_parent,
	                            false)) return false;

	if (!decl->func_decl.signature.is_at_macro && decl->func_decl.body_param && !decl->func_decl.signature.is_safemacro)
	{
		RETURN_SEMA_ERROR(decl, "Names of macros with a trailing body must start with '@'.");
	}
	DeclId body_param = decl->func_decl.body_param;
	Decl **body_parameters = body_param ? declptr(body_param)->body_params : NULL;
	if (!sema_analyse_macro_body(context, body_parameters)) return false;
	bool pure = false;
	if (!sema_analyse_doc_header(context, decl->func_decl.docs, decl->func_decl.signature.params, body_parameters,
	                             &pure)) return false;
	if (decl->func_decl.type_parent)
	{
		if (!sema_analyse_macro_method(context, decl)) return false;
	}
	bool always_const = decl->func_decl.signature.attrs.always_const;
	// Sanity check "always const"
	if (always_const)
	{
		if (typeget(decl->func_decl.signature.rtype) == type_void) RETURN_SEMA_ERROR(decl, "'@const' macros may not return 'void', they should always return a constant value.");
		if (body_parameters) RETURN_SEMA_ERROR(decl, "'@const' macros cannot have body parameters.");
		Ast *body = astptr(decl->func_decl.body);
		assert(body->ast_kind == AST_COMPOUND_STMT);
		body = astptrzero(body->compound_stmt.first_stmt);
		if (!body) RETURN_SEMA_ERROR(decl, "'@const' macros cannot have an empty body.");
		while (body)
		{
			switch (body->ast_kind)
			{
				case AST_CT_ASSERT:
				case AST_CT_ECHO_STMT:
				case AST_CT_FOREACH_STMT:
				case AST_CT_FOR_STMT:
				case AST_CT_IF_STMT:
				case AST_CT_SWITCH_STMT:
				case AST_CT_ELSE_STMT:
				case AST_DECLARE_STMT:
				case AST_DECLS_STMT:
				case AST_NOP_STMT:
					body = astptrzero(body->next);
					continue;
				case AST_RETURN_STMT:
					if (!body->return_stmt.expr) RETURN_SEMA_ERROR(body, "The 'return' in an `@const` must provide a value, e.g. 'return Foo.typeid;'");
					if (body->next) RETURN_SEMA_ERROR(body, "There should not be any statements after 'return'.");
					body = NULL;
					continue;
				case AST_POISONED:
				case AST_ASM_STMT:
				case AST_ASM_BLOCK_STMT:
				case AST_ASSERT_STMT:
				case AST_BREAK_STMT:
				case AST_CASE_STMT:
				case AST_COMPOUND_STMT:
				case AST_CONTINUE_STMT:
				case AST_DEFAULT_STMT:
				case AST_DEFER_STMT:
				case AST_EXPR_STMT:
				case AST_FOR_STMT:
				case AST_FOREACH_STMT:
				case AST_IF_CATCH_SWITCH_STMT:
				case AST_IF_STMT:
				case AST_BLOCK_EXIT_STMT:
				case AST_SWITCH_STMT:
				case AST_NEXTCASE_STMT:
				case AST_CONTRACT:
				case AST_CONTRACT_FAULT:
					RETURN_SEMA_ERROR(body, "Only 'return' and compile time statements are allowed in an '@const' macro.");
			}
			UNREACHABLE
		}
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
		case VARDECL_PARAM_REF:
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

static bool sema_analyse_decl_type(SemaContext *context, Type *type, SourceSpan span)
{
	switch (type_storage_type(type))
	{
		case STORAGE_NORMAL:
			return true;
		case STORAGE_VOID:
		case STORAGE_WILDCARD:
			if (type_is_optional(type))
			{
				sema_error_at(context, span, "The use of 'void!' as a variable type is not permitted, use %s instead.",
				              type_quoted_error_string(type_anyfault));
			} else
			{
				sema_error_at(context, span, "The use of 'void' as a variable type is not permitted.");
			}
			return false;
		case STORAGE_COMPILE_TIME:
			sema_error_at(context, span, "The variable cannot have an compile time %s type.",
			              type_quoted_error_string(type));
			return false;
		case STORAGE_UNKNOWN:
			sema_error_at(context, span, "%s has unknown size, and so it cannot be a variable type.",
			              type_quoted_error_string(type));
			return false;
	}
	UNREACHABLE
}

/**
 * Analyse $foo and $Foo variables.
 */
bool sema_analyse_var_decl_ct(SemaContext *context, Decl *decl)
{
	Expr *init;
	assert(decl->decl_kind == DECL_VAR && "Should only be called on variables.");

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
				if (!sema_analyse_expr_lvalue_fold_const(context, init)) goto FAIL;

				// If this isn't a type, it's an error.
				if (init->expr_kind != EXPR_TYPEINFO)
				{
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
				if (!sema_resolve_type_info(context, type_info, RESOLVE_TYPE_DEFAULT)) goto FAIL;
				// Set the type of the declaration.
				decl->type = type_info->type->canonical;
				init = decl->var.init_expr;
				// If there is no init, set it to zero.
				if (!init)
				{
					decl->var.init_expr = init = expr_new(EXPR_POISONED, decl->span);
					expr_rewrite_to_const_zero(init, decl->type);
				}

				// Analyse the expression.
				if (!sema_analyse_expr_rhs(context, decl->type, init, false, NULL, false)) goto FAIL;

				// Check that it is constant.
				if (!expr_is_constant_eval(init, CONSTANT_EVAL_CONSTANT_VALUE))
				{
					SEMA_ERROR(init, "Expected a constant expression assigned to %s.", decl->name);
					goto FAIL;
				}
				break;
			}
			// If we don't have a type, resolve the expression.
			if ((init = decl->var.init_expr))
			{
				if (!sema_analyse_expr(context, init)) goto FAIL;
				// Check it is constant.
				if (!expr_is_constant_eval(init, CONSTANT_EVAL_CONSTANT_VALUE))
				{
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

	return sema_add_local(context, decl);
FAIL:
	sema_add_local(context, decl);
	return decl_poison(decl);
}
/**
 * Analyse a regular global or local declaration, e.g. int x = 123
 */
bool sema_analyse_var_decl(SemaContext *context, Decl *decl, bool local)
{
	assert(decl->decl_kind == DECL_VAR && "Unexpected declaration type");

	VarDeclKind kind = decl->var.kind;

	bool is_global = !local;
	switch (kind)
	{
		case VARDECL_LOCAL_CT:
		case VARDECL_LOCAL_CT_TYPE:
			return sema_analyse_var_decl_ct(context, decl);
		case VARDECL_GLOBAL:
			is_global = true;
			break;
		case VARDECL_CONST:
		default:
			break;
	}

	TypeInfo *type_info = vartype(decl);
	// We expect a constant to actually be parsed correctly so that it has a value, so
	// this should always be true.
	assert(type_info || decl->var.init_expr);

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
				return decl_poison(decl);
			}
			SEMA_ERROR(decl, "Variable declarations may not be used outside of functions.");
			return decl_poison(decl);
		}
		// Add a local to the current context, will throw error on shadowing.
		if (!sema_add_local(context, decl)) return decl_poison(decl);
	}

	bool erase_decl = false;
	if (!sema_analyse_attributes_for_var(context, decl, &erase_decl)) return decl_poison(decl);

	bool is_static = decl->var.is_static;
	bool global_level_var = is_static || decl->var.kind == VARDECL_CONST || is_global;

	if (decl->is_extern && decl->var.init_expr)
	{
		assert(is_global);
		SEMA_ERROR(decl->var.init_expr, "Extern globals may not have initializers.");
		return decl_poison(decl);
	}
	if (erase_decl)
	{
		decl->decl_kind = DECL_ERASED;
		decl->resolve_status = RESOLVE_DONE;
		return true;
	}
	// 1. Local or global constants: const int FOO = 123.
	if (!type_info)
	{
		Expr *init_expr = decl->var.init_expr;
		// 1a. We require an init expression.
		if (!init_expr)
		{
			assert(kind == VARDECL_CONST);
			SEMA_ERROR(decl, "Constants need to have an initial value.");
			return decl_poison(decl);
		}
		if (kind == VARDECL_LOCAL && !context->current_macro)
		{
			SEMA_ERROR(decl, "Defining a variable using 'var %s = ...' is only allowed inside a macro.", decl->name);
			return decl_poison(decl);
		}
		assert(!decl->var.no_init);
		if (!type_info)
		{
			if (!sema_analyse_expr(context, init_expr)) return decl_poison(decl);
			if (global_level_var || !type_is_abi_aggregate(init_expr->type)) sema_cast_const(init_expr);
			if (global_level_var && !expr_is_constant_eval(init_expr, CONSTANT_EVAL_GLOBAL_INIT))
			{
				SEMA_ERROR(init_expr, "This expression cannot be evaluated at compile time.");
				return decl_poison(decl);
			}
			decl->type = init_expr->type;
			switch (type_storage_type(init_expr->type))
			{
				case STORAGE_NORMAL:
					break;
				case STORAGE_WILDCARD:
					SEMA_ERROR(init_expr, "No type can be inferred from the optional result.");
					return decl_poison(decl);
				case STORAGE_VOID:
					SEMA_ERROR(init_expr, "You cannot initialize a value to 'void'.");
					return decl_poison(decl);
				case STORAGE_COMPILE_TIME:
					if (init_expr->type == type_untypedlist)
					{
						SEMA_ERROR(init_expr, "The type of an untyped list cannot be inferred, you can try adding an explicit type to solve this.");
						return decl_poison(decl);
					}
					if (decl->var.kind == VARDECL_CONST)
					{
						SEMA_ERROR(init_expr, "You cannot initialize a constant to %s, but you can assign the expression to a compile time variable.", type_invalid_storage_type_name(init_expr->type));
						return decl_poison(decl);
					}
					SEMA_ERROR(init_expr, "You can't store a compile time type in a variable.");
					return decl_poison(decl);
				case STORAGE_UNKNOWN:
					SEMA_ERROR(init_expr, "You cannot initialize a value to %s as it has unknown size.",
					           type_quoted_error_string(init_expr->type));
					return decl_poison(decl);
			}
			if (!decl->alignment)
			{
				if (!sema_set_alloca_alignment(context, decl->type, &decl->alignment)) return false;
			}
			if (!sema_analyse_decl_type(context, decl->type, init_expr->span)) return decl_poison(decl);
			// Skip further evaluation.
			goto EXIT_OK;
		}
	}

	if (!sema_resolve_type_info(context, type_info,
	                            decl->var.init_expr ? RESOLVE_TYPE_ALLOW_INFER
	                                                : RESOLVE_TYPE_DEFAULT)) return decl_poison(decl);

	Type *type = decl->type = type_info->type;
	if (!sema_analyse_decl_type(context, decl->type, type_info->span)) return decl_poison(decl);

	type = type_no_optional(type);
	if (type_is_user_defined(type) && type->decl)
	{
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
		if (!sema_expr_analyse_assign_right_side(context, NULL, decl->type, init, false, true))
		{
			context->call_env.kind = env_kind;
			return decl_poison(decl);
		}
		context->call_env.kind = env_kind;

		if (infer_len)
		{
			decl->type = type_add_optional(init->type, IS_OPTIONAL(decl));
		}

		Expr *init_expr = decl->var.init_expr;

		// 2. Check const-ness
		if (global_level_var && !expr_is_constant_eval(init_expr, CONSTANT_EVAL_GLOBAL_INIT))
		{
			SEMA_ERROR(init_expr, "The expression must be a constant value.");
			return decl_poison(decl);
		}
		if (global_level_var || !type_is_abi_aggregate(init_expr->type)) sema_cast_const(init_expr);
		if (expr_is_const(init_expr))
		{
			init_expr->const_expr.is_hex = false;
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
	return true;
}


static CompilationUnit *unit_copy(Module *module, CompilationUnit *unit)
{
	CompilationUnit *copy = unit_create(unit->file);
	copy->imports = copy_decl_list_single(unit->imports);
	copy->global_decls = copy_decl_list_single_for_unit(unit->global_decls);
	copy->global_cond_decls = copy_decl_list_single_for_unit(unit->global_cond_decls);
	copy->module = module;
	assert(!unit->functions && !unit->macro_methods && !unit->methods && !unit->enums && !unit->ct_includes && !unit->types);
	return copy;
}

static Module *module_instantiate_generic(SemaContext *context, Module *module, Path *path, Expr **params,
										  SourceSpan from)
{
	unsigned decls = 0;
	Decl* params_decls[MAX_PARAMS];
	unsigned count = vec_size(module->parameters);
	for (unsigned i = 0; i < count; i++)
	{
		const char *param_name = module->parameters[i];
		bool is_value = str_is_valid_constant(param_name);
		Expr *param = params[i];
		if (param->expr_kind != EXPR_TYPEINFO)
		{
			if (!is_value)
			{
				SEMA_ERROR(param, "Expected a type, not a value.");
				return NULL;
			}
			Decl *decl = decl_new_var(param_name, param->span, NULL, VARDECL_CONST);
			decl->var.init_expr = param;
			decl->type = param->type;
			decl->resolve_status = RESOLVE_NOT_DONE;
			params_decls[decls++] = decl;
			continue;
		}
		if (is_value)
		{
			SEMA_ERROR(param, "Expected a value, not a type.");
			return NULL;
		}
		TypeInfo *type_info = param->type_expr;
		if (!sema_resolve_type_info(context, type_info, RESOLVE_TYPE_DEFAULT)) return false;
		Decl *decl = decl_new_with_type(param_name, params[i]->span, DECL_TYPEDEF);
		decl->resolve_status = RESOLVE_DONE;
		assert(type_info->resolve_status == RESOLVE_DONE);
		decl->typedef_decl.type_info = type_info;
		decl->type->name = decl->name;
		decl->type->canonical = type_info->type->canonical;
		params_decls[decls++] = decl;
	}

	Module *new_module = compiler_find_or_create_module(path, NULL);
	new_module->is_generic = false;
	new_module->generic_module = module;
	FOREACH(CompilationUnit *, unit, module->units)
	{
		vec_add(new_module->units, unit_copy(new_module, unit));
	}
	CompilationUnit *first_context = new_module->units[0];
	for (unsigned  i = 0; i < decls; i++)
	{
		vec_add(first_context->global_decls, params_decls[i]);
	}

	if (module->contracts)
	{
		copy_begin();
		new_module->contracts = astid(copy_ast_macro(astptr(module->contracts)));
		copy_end();
	}

	return new_module;
}

static bool sema_generate_parameterized_name_to_scratch(SemaContext *context, Module *module, Expr **params, bool mangled)
{
	// First resolve
	FOREACH_IDX(i, Expr *, param, params)
	{
		if (param->expr_kind == EXPR_TYPEINFO)
		{
			TypeInfo *type_info = param->type_expr;
			if (!sema_resolve_type_info(context, type_info, RESOLVE_TYPE_DEFAULT)) return false;
			Type *type = type_info->type->canonical;
			if (type->type_kind == TYPE_OPTIONAL) RETURN_SEMA_ERROR(type_info, "Expected a non-optional type.");
			switch (type_storage_type(type))
			{
				case STORAGE_NORMAL:
					break;
				case STORAGE_VOID:
					RETURN_SEMA_ERROR(type_info, "A 'void' type cannot be used as a parameter type.");
				case STORAGE_WILDCARD:
					RETURN_SEMA_ERROR(type_info, "The type is undefined and cannot be used as a parameter type.");
				case STORAGE_COMPILE_TIME:
					RETURN_SEMA_ERROR(type_info, "Expected a runtime type.");
				case STORAGE_UNKNOWN:
					RETURN_SEMA_ERROR(type_info,
					                  "%s doesn't have a well defined size and cannot be used as a parameter type.",
					                  type_quoted_error_string(type));
			}
			if (type_is_func_ptr(type))
			{
				if (!sema_resolve_type_decl(context, type->pointer)) return false;
			}
		}
		else
		{
			if (!sema_analyse_ct_expr(context, param)) return false;
			Type *type = param->type->canonical;
			bool is_enum_like = type_kind_is_enumlike(type->type_kind);
			if (!type_is_integer_or_bool_kind(type) && !is_enum_like)
			{
				SEMA_ERROR(param, "Only integer, bool, fault and enum values may be generic arguments.");
				return poisoned_decl;
			}
			assert(expr_is_const(param));
		}
	}

	scratch_buffer_clear();
	if (mangled)
	{
		scratch_buffer_append_module(module, true);
		scratch_buffer_append("$");
	}
	else
	{
		scratch_buffer_append("(<");
	}
	FOREACH_IDX(j, Expr *, param, params)
	{
		if (j != 0)
		{
			scratch_buffer_append(mangled ? "$" : ", ");
		}
		if (param->expr_kind == EXPR_TYPEINFO)
		{
			Type *type = param->type_expr->type->canonical;
			if (mangled)
			{
				type_mangle_introspect_name_to_buffer(type);
			}
			else
			{
				scratch_buffer_append(type->name);
			}
		}
		else
		{
			Type *type = param->type->canonical;
			bool is_enum_like = type_kind_is_enumlike(type->type_kind);
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
			else if (is_enum_like)
			{
				Decl *enum_like = param->const_expr.enum_err_val;
				type_mangle_introspect_name_to_buffer(enum_like->type->canonical);
				scratch_buffer_append(mangled ? "_" : ":");
				scratch_buffer_append(enum_like->name);
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
						scratch_buffer_append_signed_int(param->const_expr.ixx.i.low);
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
	scratch_buffer_append(mangled ? "$" : ">)");
	return true;
}

static bool sema_analyse_generic_module_contracts(SemaContext *c, Module *module, SourceSpan error_span)
{
	assert(module->contracts);
	AstId contract = module->contracts;
	while (contract)
	{
		Ast *ast = astptr(contract);
		contract = ast->next;
		assert(ast->ast_kind == AST_CONTRACT);
		SemaContext temp_context;
		assert(ast->contract_stmt.kind == CONTRACT_REQUIRE);
		SemaContext *new_context = context_transform_for_eval(c, &temp_context, module->units[0]);
		FOREACH(Expr *, expr, ast->contract_stmt.contract.decl_exprs->expression_list)
		{
			CondResult res = sema_check_comp_time_bool(new_context, expr);
			if (res == COND_MISSING) goto FAIL;
			if (res == COND_TRUE) continue;
			if (ast->contract_stmt.contract.comment)
			{
				sema_error_at(c, error_span,
				              "Parameter(s) would violate constraint: %s.",
				              ast->contract_stmt.contract.comment);
			}
			else
			{
				sema_error_at(c, error_span, "Parameter(s) failed validation: %s",
				              ast->contract_stmt.contract.expr_string);
			}
			FAIL:
			sema_context_destroy(&temp_context);
			return false;
		}
		sema_context_destroy(&temp_context);
	}
	return true;
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
		default:
			UNREACHABLE
	}
	Expr **params = decl->define_decl.generic_params;
	Decl *symbol = sema_analyse_parameterized_identifier(c, decl_path, name, span, params);
	if (!decl_ok(symbol)) return decl_poison(decl);
	switch (decl->define_decl.define_kind)
	{
		case DEFINE_IDENT_GENERIC:
			decl->define_decl.alias = symbol;
			decl->type = symbol->type;
			return true;
		default:
			UNREACHABLE
	}
}

Decl *sema_analyse_parameterized_identifier(SemaContext *c, Path *decl_path, const char *name, SourceSpan span, Expr **params)
{
	NameResolve name_resolve = {
			.path = decl_path,
			.span = span,
			.symbol = name
	};

	if (!unit_resolve_parameterized_symbol(c, &name_resolve)) return poisoned_decl;
	Decl *alias = name_resolve.found;
	assert(alias);
	Module *module = alias->unit->module;
	unsigned parameter_count = vec_size(module->parameters);
	assert(parameter_count > 0);
	if (parameter_count != vec_size(params))
	{
		assert(vec_size(params));
		sema_error_at(c, extend_span_with_token(params[0]->span, vectail(params)->span),
					  "The generic module expected %d arguments, but you supplied %d, did you make a mistake?",
					  parameter_count,
					  vec_size(params));
		return poisoned_decl;
	}
	if (!sema_generate_parameterized_name_to_scratch(c, module, params, true)) return poisoned_decl;
	TokenType ident_type = TOKEN_IDENT;
	const char *path_string = scratch_buffer_interned();
	Module *instantiated_module = global_context_find_module(path_string);

	bool was_initiated = false;
	if (!instantiated_module)
	{
		was_initiated = true;
		Path *path = CALLOCS(Path);
		path->module = path_string;
		path->span = module->name->span;
		path->len = scratch_buffer.len;
		instantiated_module = module_instantiate_generic(c, module, path, params, span);
		if (!sema_generate_parameterized_name_to_scratch(c, module, params, false)) return poisoned_decl;
		if (!instantiated_module) return poisoned_decl;
		instantiated_module->generic_suffix = scratch_buffer_copy();
		if (c->unit->module->generic_module)
		{
			sema_analyze_stage(instantiated_module, c->unit->module->stage);
		}
		else
		{
			sema_analyze_stage(instantiated_module, c->unit->module->stage - 1);
		}
	}
	if (global_context.errors_found) return poisoned_decl;
	Decl *symbol = module_find_symbol(instantiated_module, name);
	if (!symbol)
	{
		sema_error_at(c, span, "The generic module '%s' does not have '%s' for this parameterization.", module->name->module, name);
		return poisoned_decl;
	}
	if (was_initiated && instantiated_module->contracts)
	{
		SourceSpan error_span = extend_span_with_token(params[0]->span, params[parameter_count - 1]->span);
		if (!sema_analyse_generic_module_contracts(c, instantiated_module, error_span))
		{
			return poisoned_decl;
		}
	}
	if (!sema_analyse_decl(c, symbol)) return poisoned_decl;
	unit_register_external_symbol(c, symbol);
	return symbol;
}

static inline bool sema_analyse_attribute_decl(SemaContext *context, SemaContext *c, Decl *decl, bool *erase_decl)
{
	if (!sema_analyse_attributes(c, decl, decl->attributes, ATTR_DEF, erase_decl)) return decl_poison(decl);
	if (*erase_decl) return true;

	Decl **params = decl->attr_decl.params;
	unsigned param_count = vec_size(params);
	for (unsigned i = 0; i < param_count; i++)
	{
		Decl *param = params[i];
		if (param->var.kind != VARDECL_PARAM) RETURN_SEMA_ERROR(param, "Expected a simple replacement parameter e.g. 'val' here.");
		if (param->var.type_info) RETURN_SEMA_ERROR(param, "Type is not allowed on attribute parameters.");
		if (param->var.init_expr) RETURN_SEMA_ERROR(param, "Attribute parameters may not have default values.");
		param->resolve_status = RESOLVE_DONE;
		for (int j = 0; j < i; j++)
		{
			if (param[j].name == param->name)
			{
				RETURN_SEMA_ERROR(param, "Duplicate parameter name '%s'.", param->name);
			}
		}
	}
	return true;
}

static inline bool sema_analyse_define(SemaContext *c, Decl *decl, bool *erase_decl)
{
	if (!sema_analyse_attributes(c, decl, decl->attributes, ATTR_DEF, erase_decl)) return decl_poison(decl);
	if (*erase_decl) return true;

	// 1. The plain define
	if (decl->define_decl.define_kind == DEFINE_IDENT_ALIAS)
	{
		Decl *symbol = sema_resolve_symbol(c, decl->define_decl.ident, decl->define_decl.path, decl->define_decl.span);
		if (!symbol) return false;
		if (!sema_analyse_decl(c, symbol)) return false;
		decl->type = symbol->type;
		decl->define_decl.alias = symbol;
		return true;
	}

	// 2. Handle type generics.
	return sema_analyse_parameterized_define(c, decl);
}

bool sema_resolve_type_structure(SemaContext *context, Type *type, SourceSpan span)
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
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_BITSTRUCT:
		case TYPE_FAULTTYPE:
			return sema_analyse_decl(context, type->decl);
		case TYPE_POINTER:
		case TYPE_FUNC_PTR:
			type = type->pointer;
			goto RETRY;
		case TYPE_TYPEDEF:
			type = type->canonical;
			goto RETRY;
		case TYPE_DISTINCT:
			if (!sema_analyse_decl(context, type->decl)) return false;
			type = type->decl->distinct->type;
			goto RETRY;
		case TYPE_ARRAY:
		case TYPE_SLICE:
		case TYPE_FLEXIBLE_ARRAY:
		case TYPE_INFERRED_ARRAY:
		case TYPE_VECTOR:
		case TYPE_INFERRED_VECTOR:
			type = type->array.base;
			goto RETRY;
		case TYPE_OPTIONAL:
			type = type->optional;
			goto RETRY;
	}
	UNREACHABLE
}

bool sema_analyse_decl(SemaContext *context, Decl *decl)
{
	if (decl->resolve_status == RESOLVE_DONE) return decl_ok(decl);

	DEBUG_LOG(">>> Analyse declaration [%s] in %s.", decl_safe_name(decl), context_filename(context));

	SemaContext temp_context;
	context = context_transform_for_eval(context, &temp_context, decl->unit);

	if (decl->resolve_status == RESOLVE_RUNNING)
	{
		SEMA_ERROR(decl, decl->name
			? "Recursive definition of '%s'."
			: "Recursive definition of anonymous declaration.", decl->name);
		goto FAILED;
	}
	decl->resolve_status = RESOLVE_RUNNING;
	assert(decl->unit);
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
			if (!sema_analyse_var_decl(context, decl, false)) goto FAILED;
			break;
		case DECL_ATTRIBUTE:
			if (!sema_analyse_attribute_decl(context, context, decl, &erase_decl)) goto FAILED;
			break;
		case DECL_DISTINCT:
			if (!sema_analyse_distinct(context, decl, &erase_decl)) goto FAILED;
			break;
		case DECL_TYPEDEF:
			if (!sema_analyse_typedef(context, decl, &erase_decl)) goto FAILED;
			break;
		case DECL_ENUM:
			if (!sema_analyse_enum(context, decl, &erase_decl)) goto FAILED;
			break;
		case DECL_FAULT:
			if (!sema_analyse_error(context, decl, &erase_decl)) goto FAILED;
			break;
		case DECL_DEFINE:
			if (!sema_analyse_define(context, decl, &erase_decl)) goto FAILED;
			break;
		case DECL_POISONED:
		case DECL_IMPORT:
		case DECL_ENUM_CONSTANT:
		case DECL_LABEL:
		case DECL_CT_ASSERT:
		case DECL_CT_ECHO:
		case DECL_FAULTVALUE:
		case DECL_DECLARRAY:
		case DECL_BODYPARAM:
		case DECL_CT_INCLUDE:
		case DECL_CT_EXEC:
		case DECL_GLOBALS:
			UNREACHABLE
	}
	if (erase_decl)
	{
		decl->decl_kind = DECL_ERASED;
	}
	decl->resolve_status = RESOLVE_DONE;
	sema_context_destroy(&temp_context);

	DEBUG_LOG("<<< Analysis of [%s] successful.", decl_safe_name(decl));

	return true;
FAILED:
	sema_context_destroy(&temp_context);
	DEBUG_LOG("<<< Analysis of [%s] failed.", decl_safe_name(decl));
	return decl_poison(decl);
}

