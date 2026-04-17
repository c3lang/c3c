// Copyright (c) 2019-2025 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_internal.h"

static unsigned id_counter = 0;

static inline LLVMMetadataRef llvm_get_debug_type_internal(GenContext *c, Type *type, LLVMMetadataRef scope);
static inline LLVMMetadataRef llvm_get_debug_member(GenContext *c, Type *type, const char *name, unsigned offset, SourceLocId loc, LLVMMetadataRef scope, LLVMDIFlags flags);
static inline LLVMMetadataRef llvm_get_debug_struct(GenContext *c, Type *type, const char *external_name, LLVMMetadataRef *elements, unsigned element_count, SourceLocId
                                                    loc, LLVMMetadataRef scope, LLVMDIFlags flags);
static LLVMMetadataRef llvm_debug_forward_comp(GenContext *c, Type *type, const char *external_name, SourceLocId loc, LLVMMetadataRef scope, LLVMDIFlags flags);
static LLVMMetadataRef llvm_debug_simple_type(GenContext *context, Type *type, int dwarf_code);
static LLVMMetadataRef llvm_debug_func_type(GenContext *c, Type *type);
static LLVMMetadataRef llvm_debug_structlike_type(GenContext *c, Type *type, LLVMMetadataRef scope);
static LLVMMetadataRef llvm_debug_pointer_type(GenContext *c, Type *type);
static LLVMMetadataRef llvm_debug_vector_type(GenContext *c, Type *type);
static LLVMMetadataRef llvm_debug_typedef_type(GenContext *c, Type *type);
static LLVMMetadataRef llvm_debug_array_type(GenContext *c, Type *type);
static LLVMMetadataRef llvm_debug_errunion_type(GenContext *c, Type *type);
static LLVMMetadataRef llvm_debug_slice_type(GenContext *c, Type *type);
static LLVMMetadataRef llvm_debug_any_type(GenContext *c, Type *type);
static LLVMMetadataRef llvm_debug_enum_type(GenContext *c, Type *type, LLVMMetadataRef scope);
static LLVMMetadataRef llvm_debug_raw_enum_type(GenContext *c, Type *type, LLVMMetadataRef scope);

#define SOURCELOC_ROW_COL(loc__) uint32_t row, col; do { SourceLoc *loc_ref__ = sourceloc_safe(loc__); row = loc_ref__->row; col = loc_ref__->col; } while (0)

INLINE SourceLoc *sourceloc_safe(SourceLocId loc)
{
	static SourceLoc dummy = { .row = 1, .col = 1 };
	if (loc) return sourcelocptr(loc);
	return &dummy;
}

INLINE uint32_t sourceloc_row(SourceLocId loc)
{
	if (loc) return sourcelocptr(loc)->row;
	return 0;
}

INLINE uint32_t sourceloc_row_1(SourceLocId loc)
{
	if (!loc) return 1;
	uint32_t row = sourcelocptr(loc)->row;
	return row ? row : 1;
}

INLINE LLVMMetadataRef llvm_create_debug_location_with_inline(GenContext *c, unsigned row, unsigned col, LLVMMetadataRef scope)
{
	if (!c->debug.emit_expr_loc) col = 0;
	return LLVMDIBuilderCreateDebugLocation(c->context, row, col,
	                                 scope, c->debug.block_stack->inline_loc ? c->debug.block_stack->inline_loc : NULL);
}
static inline LLVMMetadataRef llvm_get_debug_struct(GenContext *c, Type *type, const char *external_name, LLVMMetadataRef *elements, unsigned element_count, SourceLocId
                                                    loc, LLVMMetadataRef scope, LLVMDIFlags flags)
{
	LLVMMetadataRef file = NULL;
	unsigned row = 0;
	size_t external_name_len = strlen(external_name);
	if (loc)
	{
		file = c->debug.file.debug_file;
		row = sourceloc_row_1(loc);
	}
	LLVMMetadataRef real = LLVMDIBuilderCreateStructType(c->debug.builder,
														 scope,
														 type->name ? type->name : "", type->name ? strlen(type->name) : 0,
														 file,
														 row,
														 type_size(type) * 8,
														 (uint32_t)(type_abi_alignment(type) * 8),
														 flags,
														 NULL, // Derived from
														 elements, element_count,
														 c->debug.runtime_version,
														 NULL, // VTable
														 external_name, external_name_len);
	if (type->backend_debug_type)
	{
		LLVMMetadataReplaceAllUsesWith(type->backend_debug_type, real);
	}
	return real;
}

static inline LLVMMetadataRef llvm_get_debug_member(GenContext *c, Type *type, const char *name, unsigned offset, SourceLocId loc, LLVMMetadataRef scope, LLVMDIFlags flags)
{
	ASSERT(name && scope);
	return LLVMDIBuilderCreateMemberType(
			c->debug.builder,
			scope,
			name, strlen(name),
			loc ? c->debug.file.debug_file : NULL,
			sourceloc_row(loc),
			type_size(type) * 8,
			(uint32_t)(type_abi_alignment(type) * 8),
			offset * 8, flags, llvm_get_debug_type_internal(c, type, scope));
}



LLVMMetadataRef llvm_debug_current_scope(GenContext *context)
{
	if (context->debug.block_stack) return context->debug.block_stack->lexical_block;
	return context->debug.compile_unit;
}

void llvm_emit_debug_function(GenContext *c, Decl *decl)
{
	if (!decl->func_decl.body) return;
	LLVMDIFlags flags = LLVMDIFlagZero;
	flags |= LLVMDIFlagPrototyped;
	if (decl->func_decl.signature.attrs.noreturn) flags |= LLVMDIFlagNoReturn;

	uint32_t row = sourceloc_row_1(decl->loc);
	ASSERT(decl->name);
	ASSERT(c->debug.file.debug_file);
	LLVMMetadataRef debug_type = llvm_get_debug_type(c, decl->type);
	scratch_buffer_set_extern_decl_name(decl, true);
	c->debug.function = LLVMDIBuilderCreateFunction(c->debug.builder,
													c->debug.file.debug_file,
													decl->name, strlen(decl->name),
													scratch_buffer_to_string(), scratch_buffer.len,
													c->debug.file.debug_file,
													row,
													debug_type,
													decl_is_local(decl),
													true,
													row,
													flags,
													compiler.build.optlevel != OPTIMIZATION_NONE);
	LLVMSetSubprogram(decl->backend_ref, c->debug.function);

}

static void llvm_emit_debug_value(GenContext *c, LLVMValueRef value, LLVMMetadataRef debug_val, unsigned row, unsigned col, LLVMMetadataRef scope)
{
	LLVMDIBuilderInsertDbgValueRecordAtEnd(c->debug.builder, value, debug_val,
									 LLVMDIBuilderCreateExpression(c->debug.builder, NULL, 0),
									 llvm_create_debug_location_with_inline(c, row, col, c->debug.function),
									 LLVMGetInsertBlock(c->builder));
}


static void llvm_emit_debug_declare(GenContext *c, LLVMValueRef var, LLVMMetadataRef debug_var, unsigned row, unsigned col, LLVMMetadataRef scope)
{
	LLVMDIBuilderInsertDeclareRecordAtEnd(c->debug.builder,
	                                var, debug_var,
	                                LLVMDIBuilderCreateExpression(c->debug.builder, NULL, 0),
	                                llvm_create_debug_location_with_inline(c, row, col, scope),
	                                LLVMGetInsertBlock(c->builder));
}

void llvm_emit_debug_local_var(GenContext *c, Decl *decl)
{
	ASSERT(llvm_is_local_eval(c));
	EMIT_EXPR_LOC(c, decl);
	SourceLoc *loc = sourceloc_safe(decl->loc);
	uint32_t row = loc->row;
	uint32_t col = loc->col;
	const char *name = decl->name;
	if (!name) name = ".temp";
	LLVMMetadataRef scope = llvm_debug_current_scope(c);
	LLVMMetadataRef var = LLVMDIBuilderCreateAutoVariable(
			c->debug.builder,
			scope,
			name,
			strlen(name),
			c->debug.file.debug_file,
			row,
			llvm_get_debug_type(c, decl->type),
			compiler.build.optlevel != OPTIMIZATION_NONE,
			LLVMDIFlagZero,
			decl->alignment * 8);
	decl->var.backend_debug_ref = var;

	ASSERT(!decl->is_value);
	llvm_emit_debug_declare(c, decl->backend_ref, var, row, col, scope);
}

/**
 * Setup a debug parameter for a given index.
 * @param c
 * @param parameter
 * @param index
 */
void llvm_emit_debug_parameter(GenContext *c, Decl *parameter, unsigned index)
{
	ASSERT(!llvm_is_global_eval(c));
	const char *name = parameter->name ? parameter->name : ".anon";
	bool always_preserve = false;

	SOURCELOC_ROW_COL(parameter->loc);

	parameter->var.backend_debug_ref = LLVMDIBuilderCreateParameterVariable(
			c->debug.builder,
			c->debug.function,
			name,
			strlen(name),
			index + 1,
			c->debug.file.debug_file,
			row,
			llvm_get_debug_type(c, parameter->type),
			always_preserve,
			LLVMDIFlagZero);
	if (parameter->is_value)
	{
		llvm_emit_debug_value(c, parameter->backend_value, parameter->var.backend_debug_ref, row, col, c->debug.function);
		return;
	}
	llvm_emit_debug_declare(c, parameter->backend_ref, parameter->var.backend_debug_ref,
	                        row, col, c->debug.function);

}

LLVMMetadataRef llvm_create_debug_location(GenContext *c, SourceLocId location)
{
	LLVMMetadataRef scope = llvm_debug_current_scope(c);
	SOURCELOC_ROW_COL(location);
	return llvm_create_debug_location_with_inline(c, row ? row : 1, col ? col : 1, scope);
}

void llvm_emit_debug_location(GenContext *c, SourceLocId location)
{
	if (llvm_is_global_eval(c)) return;
	// Avoid re-emitting the same location.
	LLVMMetadataRef oldloc = LLVMGetCurrentDebugLocation2(c->builder);
	if (oldloc && c->last_emitted_loc == location) return;
	LLVMMetadataRef loc = c->last_loc = llvm_create_debug_location(c, location);
	c->last_emitted_loc = location;
	LLVMSetCurrentDebugLocation2(c->builder, loc);

}

static LLVMMetadataRef llvm_debug_forward_comp(GenContext *c, Type *type, const char *external_name, SourceLocId loc, LLVMMetadataRef scope, LLVMDIFlags flags)
{
	unsigned row = sourceloc_row_1(loc);
	return LLVMDIBuilderCreateReplaceableCompositeType(c->debug.builder, id_counter++,
													   type->name, strlen(type->name),
													   scope,
													   c->debug.file.debug_file, row,
													   c->debug.runtime_version,
													   type_size(type) * 8,
													   type_abi_alignment(type) * 8,
													   flags,
													   "temp",
													   4);

}

LLVMMetadataRef llvm_debug_create_macro(GenContext *c, Decl *macro)
{
	const char *name = macro->name;
	size_t namelen = strlen(name);
	SourceLoc *location = sourceloc_safe(macro->loc);
	LLVMMetadataRef file = llvm_get_debug_file(c, location->file_id);
	LLVMMetadataRef macro_type = NULL;
	return LLVMDIBuilderCreateFunction(c->debug.builder, file, name, namelen, name, namelen,
	                            file, location->row, macro_type, true, true, location->row, LLVMDIFlagZero, false);
}

DebugScope llvm_debug_create_lexical_scope(GenContext *context, SourceLocId location)
{
	LLVMMetadataRef scope;
	LLVMMetadataRef inline_at;
	DebugScope *outline_at;
	if (context->debug.block_stack)
	{
		scope = context->debug.block_stack->lexical_block;
		inline_at = context->debug.block_stack->inline_loc;
		outline_at = context->debug.block_stack->outline_loc;
	}
	else
	{
		scope = context->debug.compile_unit;
		inline_at = NULL;
		outline_at = NULL;
	}

	SourceLoc *loc = sourceloc_safe(location);
	unsigned row = loc->row;
	unsigned col = loc->col;
	LLVMMetadataRef debug_file = context->debug.file.debug_file;
	if (loc->file_id != context->debug.file.file_id)
	{
		debug_file = llvm_get_debug_file(context, loc->file_id);
	}
	LLVMMetadataRef block = LLVMDIBuilderCreateLexicalBlock(context->debug.builder, scope, debug_file, row ? row : 1, col ? col : 1);

	return (DebugScope) { .lexical_block = block, .inline_loc = inline_at, .outline_loc = outline_at };
}



static LLVMMetadataRef llvm_debug_typeid_type(GenContext *context, Type *type)
{
	return type->backend_debug_type = LLVMDIBuilderCreateBasicType(context->debug.builder,
	                                                               "typeid",
	                                                               strlen("typeid"),
	                                                               type_bit_size(type_voidptr),
	                                                               (LLVMDWARFTypeEncoding)DW_ATE_address,
	                                                               LLVMDIFlagZero);

}
static LLVMMetadataRef llvm_debug_simple_type(GenContext *context, Type *type, int dwarf_code)
{
	return type->backend_debug_type = LLVMDIBuilderCreateBasicType(context->debug.builder,
																   type->name,
																   strlen(type->name),
																   type->builtin.bitsize,
																   (LLVMDWARFTypeEncoding)dwarf_code,
																   LLVMDIFlagZero);

}

static LLVMMetadataRef llvm_debug_pointer_type(GenContext *c, Type *type)
{
	LLVMMetadataRef inner = llvm_get_debug_type(c, type->pointer);
	if (type->backend_debug_type) return type->backend_debug_type;
	return LLVMDIBuilderCreatePointerType(c->debug.builder,
										  inner,
										  type_size(type) * 8,
										  type_abi_alignment(type) * 8, 0,
										  "", 0);
}

static LLVMMetadataRef llvm_debug_enum_type(GenContext *c, Type *type, LLVMMetadataRef scope)
{
	Decl *decl = type->decl;

	LLVMMetadataRef forward = llvm_debug_forward_comp(c, type, "temp_enum", decl->loc, scope, LLVMDIFlagZero);
	type->backend_debug_type = forward;

	Type *enum_real_type = decl->enums.type_info->type->canonical;

	LLVMMetadataRef *elements = NULL;
	Decl **enums = decl->enums.values;

	bool is_unsigned = type_is_unsigned(enum_real_type);
	FOREACH(Decl *, enum_constant, enums)
	{
		int64_t val = enum_constant->enum_constant.inner_ordinal;
		LLVMMetadataRef debug_info = LLVMDIBuilderCreateEnumerator(
				c->debug.builder,
				enum_constant->name, strlen(enum_constant->name),
				val,
				is_unsigned);
		vec_add(elements, debug_info);
	}

	LLVMMetadataRef real = LLVMDIBuilderCreateEnumerationType(c->debug.builder,
															  scope,
															  type->decl->name, strlen(type->decl->name),
															  c->debug.file.debug_file, sourceloc_row_1(decl->loc), type_size(type) * 8,
															  type_abi_alignment(type) * 8,
															  elements, vec_size(elements),
															  llvm_get_debug_type(c, enum_real_type));

	LLVMMetadataReplaceAllUsesWith(forward, real);
	return real;
}

static LLVMMetadataRef llvm_debug_raw_enum_type(GenContext *c, Type *type, LLVMMetadataRef scope)
{
	Decl *decl = type->decl;
	// FIXME TODO

	LLVMMetadataRef forward = llvm_debug_forward_comp(c, type, "temp_enum", decl->loc, scope, LLVMDIFlagZero);
	type->backend_debug_type = forward;

	Type *enum_real_type = decl->enums.type_info->type->canonical;

	LLVMMetadataRef *elements = NULL;
	Decl **enums = decl->enums.values;

	bool is_unsigned = type_is_unsigned(enum_real_type);
	/*if ()
	FOREACH(Decl *, enum_constant, enums)
	{
		// TODO, support other 128
		if (type_size(enum_real_type) > 8)
		{
			TODO
		}
		LLVMMetadataRef debug_info = LLVMDIBuilderCreateEnumerator(
				c->debug.builder,
				enum_constant->name, strlen(enum_constant->name),
				enum_constant->enum_constant.const_value.low,
				is_unsigned);
		vec_add(elements, debug_info);
	}*/

	LLVMMetadataRef real = LLVMDIBuilderCreateEnumerationType(c->debug.builder,
															  scope,
															  type->decl->name, strlen(type->decl->name),
															  c->debug.file.debug_file, sourceloc_row_1(decl->loc), type_size(type) * 8,
															  type_abi_alignment(type) * 8,
															  elements, vec_size(elements),
															  llvm_get_debug_type(c, enum_real_type));

	LLVMMetadataReplaceAllUsesWith(forward, real);
	return real;
}

static LLVMMetadataRef llvm_debug_structlike_type(GenContext *c, Type *type, LLVMMetadataRef scope)
{
	Decl *decl = type->decl;

	LLVMDIFlags flags = 0;

	// Create a forward reference in case of recursive data.
	LLVMMetadataRef forward = llvm_debug_forward_comp(c, type, "temp", decl->loc, scope, flags);
	type->backend_debug_type = forward;

	LLVMMetadataRef *elements = NULL;
	Decl **members = decl->strukt.members;
	unsigned count = vec_size(members);
	for (unsigned i = 0; i < count; i++)
	{
		Decl *member = members[i];
		LLVMMetadataRef debug_info = llvm_get_debug_member(c,
														   member->type,
														   member->name ? member->name : "",
														   member->offset,
														   member->loc,
														   forward,
														   LLVMDIFlagZero);
		vec_add(elements, debug_info);
	}

	LLVMMetadataRef real;

	const char *extname = "";
	unsigned extname_len = 0;
	if (decl->name)
	{
		scratch_buffer_set_extern_decl_name(decl, true);
		extname = scratch_buffer_to_string();
		extname_len = scratch_buffer.len;
	}
	if (type->type_kind == TYPE_UNION)
	{
		real = LLVMDIBuilderCreateUnionType(c->debug.builder,
											scope,
											decl->name ? decl->name : "",
											decl->name ? strlen(decl->name) : 0,
											c->debug.file.debug_file,
											sourceloc_row_1(decl->loc),
											type_size(type) * 8,
											type_abi_alignment(type) * 8,
											LLVMDIFlagZero,
											elements, vec_size(members),
											c->debug.runtime_version,
											extname,
											extname_len);
		LLVMMetadataReplaceAllUsesWith(forward, real);
		return real;
	}
	return llvm_get_debug_struct(c, type, extname, elements, vec_size(elements), decl->loc, scope, LLVMDIFlagZero);
}

static LLVMMetadataRef llvm_debug_slice_type(GenContext *c, Type *type)
{
	LLVMMetadataRef forward = llvm_debug_forward_comp(c, type, type->name, 0, NULL, LLVMDIFlagZero);
	type->backend_debug_type = forward;

	LLVMMetadataRef elements[2] = {
			llvm_get_debug_member(c, type_get_ptr(type->array.base), "ptr", 0, 0, forward, LLVMDIFlagZero),
			llvm_get_debug_member(c, type_sz, "len", type_size(type_voidptr), 0, forward, LLVMDIFlagZero)
	};
	return llvm_get_debug_struct(c, type, type->name, elements, 2, 0, NULL, LLVMDIFlagZero);
}

static LLVMMetadataRef llvm_debug_any_type(GenContext *c, Type *type)
{
	LLVMMetadataRef forward = llvm_debug_forward_comp(c, type, type->name, 0, NULL, LLVMDIFlagZero);

	type->backend_debug_type = forward;

	LLVMMetadataRef elements[2] = {
			llvm_get_debug_member(c, type_voidptr, "ptr", 0, 0, forward, LLVMDIFlagZero),
			llvm_get_debug_member(c, type_typeid, "type", type_size(type_voidptr), 0, forward, LLVMDIFlagZero)
	};
	return llvm_get_debug_struct(c, type, type->name, elements, 2, 0, NULL, LLVMDIFlagZero);
}

static LLVMMetadataRef llvm_debug_errunion_type(GenContext *c, Type *type)
{
	return LLVMDIBuilderCreateTypedef(c->debug.builder,
									  llvm_get_debug_type(c, type_uptr->canonical),
									  type->name, strlen(type->name),
									  NULL, 0, NULL, 0);
}

static LLVMMetadataRef llvm_debug_array_type(GenContext *c, Type *type)
{
	LLVMMetadataRef *ranges = NULL;
	Type *current_type = type;
	while (current_type->canonical->type_kind == TYPE_ARRAY || current_type->canonical->type_kind == TYPE_FLEXIBLE_ARRAY)
	{
		vec_add(ranges, LLVMDIBuilderGetOrCreateSubrange(c->debug.builder, 0, current_type->canonical->array.len));
		current_type = current_type->canonical->array.base;
	}
	if (!current_type->backend_debug_type)
	{
		type->backend_debug_type = llvm_debug_forward_comp(c, type, type->name, 0, NULL, LLVMDIFlagZero);
	}
	LLVMMetadataRef real = LLVMDIBuilderCreateArrayType(
			c->debug.builder,
			type_size(type) * 8,
			type_abi_alignment(current_type) * 8,
			llvm_get_debug_type(c, current_type),
			ranges, vec_size(ranges));

	if (type->backend_debug_type)
	{
		LLVMMetadataReplaceAllUsesWith(type->backend_debug_type, real);
	}
	return real;
}

static LLVMMetadataRef llvm_debug_typedef_type(GenContext *c, Type *type)
{
	Decl *decl = type->decl;

	// Is this a primitive typedef? If so, we create it without reference.
	if (decl->unit == compiler.context.core_unit)
	{
		return LLVMDIBuilderCreateTypedef(c->debug.builder,
										  llvm_get_debug_type(c, type_lowering(type)),
										  type->name, strlen(type->name),
										  NULL, 0, NULL, 0);
	}

	Type *original_type = type->type_kind == TYPE_ALIAS ? type->canonical : decl->distinct->type;

	// Use forward references in case we haven't resolved the original type, since we could have this:
	if (!type->canonical->backend_debug_type)
	{
		type->backend_debug_type = llvm_debug_forward_comp(c, type, type->name, decl->loc, NULL, LLVMDIFlagZero);
	}
	LLVMMetadataRef real = LLVMDIBuilderCreateTypedef(c->debug.builder,
													  llvm_get_debug_type(c, original_type),
													  decl->name, strlen(decl->name),
													  c->debug.file.debug_file, sourceloc_row_1(decl->loc),
													  c->debug.file.debug_file, type_abi_alignment(type) * 8);
	if (type->backend_debug_type)
	{
		LLVMMetadataReplaceAllUsesWith(type->backend_debug_type, real);
		type->backend_debug_type = real;
	}
	return real;
}

static LLVMMetadataRef llvm_debug_vector_type(GenContext *c, Type *type)
{
	LLVMMetadataRef *ranges = NULL;
	Type *current_type = type;
	while (type_kind_is_any_vector(current_type->canonical->type_kind))
	{
		vec_add(ranges, LLVMDIBuilderGetOrCreateSubrange(c->debug.builder, 0, current_type->canonical->array.len));
		current_type = current_type->canonical->array.base;
	}
	return LLVMDIBuilderCreateVectorType(
			c->debug.builder,
			type_size(type) * 8,
			type_abi_alignment(current_type) * 8,
			llvm_get_debug_type(c, current_type),
			ranges, vec_size(ranges));
}

static LLVMMetadataRef llvm_debug_func_type(GenContext *c, Type *type)
{
	FunctionPrototype *prototype = type_get_resolved_prototype(type);
	Signature *sig = prototype->raw_type->function.signature;
	// 1. Generate all the parameter types, this may cause this function to be called again!
	FOREACH(Decl *, param, sig->params)
	{
		llvm_get_debug_type(c, param->type);
	}
	// 2. We might be done!
	if (type->backend_debug_type) return type->backend_debug_type;

	// 3. Otherwise generate:
	LLVMMetadataRef params[MAX_PARAMS + 1];
	int index = 0;
	params[index++] = llvm_get_debug_type(c, typeget(sig->rtype));
	FOREACH(Decl *, param, sig->params)
	{
		params[index++] = llvm_get_debug_type(c, param->type);
	}
	if (prototype->raw_variadic)
	{
		params[index++] = LLVMDIBuilderCreateUnspecifiedType(c->debug.builder, "", 0);
	}

	// 4. We might be done again!
	if (type->backend_debug_type) return type->backend_debug_type;

	return LLVMDIBuilderCreateSubroutineType(c->debug.builder,
	                                         c->debug.file.debug_file,
	                                         params,
	                                         index, 0);
}


static inline LLVMMetadataRef llvm_get_debug_type_internal(GenContext *c, Type *type, LLVMMetadataRef scope)
{
	if (type->backend_debug_type)
	{
		return type->backend_debug_type;
	}
	// Consider special handling of UTF8 arrays.
	switch (type->type_kind)
	{
		case CT_TYPES:
		case TYPE_UNTYPEDLIST:
			UNREACHABLE
		case TYPE_BITSTRUCT:
		case TYPE_OPTIONAL:
			return type->backend_debug_type = llvm_get_debug_type(c, type_lowering(type));
		case TYPE_BOOL:
			return llvm_debug_simple_type(c, type, DW_ATE_boolean);
		case TYPE_I8:
			return llvm_debug_simple_type(c, type, DW_ATE_signed_char); // DW_ATE_UTF?
		case TYPE_U8:
			return llvm_debug_simple_type(c, type, DW_ATE_unsigned_char);
		case TYPE_I16:
		case TYPE_I32:
		case TYPE_I64:
		case TYPE_I128:
			return llvm_debug_simple_type(c, type, DW_ATE_signed);
		case TYPE_U16:
		case TYPE_U32:
		case TYPE_U64:
		case TYPE_U128:
			return llvm_debug_simple_type(c, type, DW_ATE_unsigned);
		case TYPE_F16:
		case TYPE_BF16:
		case TYPE_F32:
		case TYPE_F64:
		case TYPE_F128:
			return llvm_debug_simple_type(c, type, DW_ATE_float);
		case VECTORS:
			return type->backend_debug_type = llvm_debug_vector_type(c, type);
		case TYPE_VOID:
			return NULL;
		case TYPE_TYPEID:
			return type->backend_debug_type = llvm_debug_typeid_type(c, type);
		case TYPE_POINTER:
		case TYPE_FUNC_PTR:
			return type->backend_debug_type = llvm_debug_pointer_type(c, type);
		case TYPE_ENUM:
			return type->backend_debug_type = llvm_debug_enum_type(c, type, scope);
		case TYPE_CONSTDEF:
			return type->backend_debug_type = llvm_debug_raw_enum_type(c, type, scope);
		case TYPE_FUNC_RAW:
			return type->backend_debug_type = llvm_debug_func_type(c, type);
		case TYPE_STRUCT:
		case TYPE_UNION:
			return type->backend_debug_type = llvm_debug_structlike_type(c, type, scope);
		case TYPE_TYPEDEF:
		case TYPE_ALIAS:
			return type->backend_debug_type = llvm_debug_typedef_type(c, type);
		case TYPE_FLEXIBLE_ARRAY:
		case TYPE_ARRAY:
			return type->backend_debug_type = llvm_debug_array_type(c, type);
		case TYPE_SLICE:
			return type->backend_debug_type = llvm_debug_slice_type(c, type);
		case TYPE_ANYFAULT:
			return type->backend_debug_type = llvm_debug_errunion_type(c, type);
		case TYPE_INTERFACE:
		case TYPE_ANY:
			return type->backend_debug_type = llvm_debug_any_type(c, type);
	}
	UNREACHABLE
}

LLVMMetadataRef llvm_get_debug_type(GenContext *c, Type *type)
{
	// All types should be generated in the outer scope.
	return llvm_get_debug_type_internal(c, type, c->debug.file.debug_file);
}
