// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_internal.h"

#define LINE_ZERO 0

static unsigned id_counter = 0;

static inline LLVMMetadataRef llvm_get_debug_type_internal(GenContext *c, Type *type, LLVMMetadataRef scope);
static inline LLVMMetadataRef llvm_get_debug_member(GenContext *c, Type *type, const char *name, unsigned offset, SourceLocation *loc, LLVMMetadataRef scope, LLVMDIFlags flags);
static inline LLVMMetadataRef llvm_get_debug_struct(GenContext *c, Type *type, const char *external_name, LLVMMetadataRef *elements, unsigned element_count, SourceLocation *loc, LLVMMetadataRef scope, LLVMDIFlags flags);
static LLVMMetadataRef llvm_debug_forward_comp(GenContext *c, Type *type, const char *external_name, SourceLocation *loc, LLVMMetadataRef scope, LLVMDIFlags flags);


static inline LLVMMetadataRef llvm_get_debug_struct(GenContext *c, Type *type, const char *external_name, LLVMMetadataRef *elements, unsigned element_count, SourceLocation *loc, LLVMMetadataRef scope, LLVMDIFlags flags)
{
	size_t external_name_len = strlen(external_name);
	LLVMMetadataRef real = LLVMDIBuilderCreateStructType(c->debug.builder,
	                                                     scope,
	                                                     external_name_len ? type->name : "", external_name_len ? strlen(type->name) : 0,
	                                                     loc ? c->debug.file : NULL,
	                                                     loc ? loc->line : 0,
	                                                     type_size(type) * 8,
	                                                     type_abi_alignment(type) * 8,
	                                                     flags, NULL,
	                                                     elements, element_count,
	                                                     c->debug.runtime_version,
	                                                     NULL, // VTable
	                                                     external_name, strlen(external_name));
	if (type->backend_debug_type)
	{
		LLVMMetadataReplaceAllUsesWith(type->backend_debug_type, real);
	}
	return real;
}

static inline LLVMMetadataRef llvm_get_debug_member(GenContext *c, Type *type, const char *name, unsigned offset, SourceLocation *loc, LLVMMetadataRef scope, LLVMDIFlags flags)
{
	return LLVMDIBuilderCreateMemberType(
			c->debug.builder,
			scope,
			name, strlen(name),
			loc ? c->debug.file : NULL,
			loc ? loc->line : 0,
			type_size(type) * 8,
			type_abi_alignment(type) * 8,
			offset * 8, flags, llvm_get_debug_type_internal(c, type, scope));
}


void llvm_debug_scope_push(GenContext *context, LLVMMetadataRef debug_scope)
{
	vec_add(context->debug.lexical_block_stack, debug_scope);
}

void llvm_debug_scope_pop(GenContext *context)
{
	vec_pop(context->debug.lexical_block_stack);
}

LLVMMetadataRef llvm_debug_current_scope(GenContext *context)
{
	if (vec_size(context->debug.lexical_block_stack) > 0)
	{
		return VECLAST(context->debug.lexical_block_stack);
	}
	return context->debug.compile_unit;
}

void llvm_emit_debug_global_var(GenContext *c, Decl *global)
{
	SourceLocation *loc = TOKLOC(global->span.loc);
	global->var.backend_debug_ref = LLVMDIBuilderCreateGlobalVariableExpression(
			c->debug.builder,
			c->debug.file,
			global->name,
			TOKLEN(global->name_token),
			global->external_name,
			strlen(global->external_name),
			c->debug.file,
			loc->line,
			llvm_get_debug_type(c, global->type),
			global->visibility == VISIBLE_LOCAL,
			LLVMDIBuilderCreateExpression(c->debug.builder, NULL, 0),
			NULL,
			global->alignment);
}

void llvm_emit_debug_function(GenContext *c, Decl *decl)
{
	LLVMDIFlags flags = LLVMDIFlagZero;
	if (!decl->func_decl.body) return;
	switch (decl->visibility)
	{
		case VISIBLE_LOCAL:
			flags |= LLVMDIFlagPrivate;
			break;
		case VISIBLE_MODULE:
			flags |= LLVMDIFlagProtected;
			break;
		case VISIBLE_PUBLIC:
		case VISIBLE_EXTERN:
			flags |= LLVMDIFlagPublic;
			break;
		default:
			UNREACHABLE
	}
	flags |= LLVMDIFlagPrototyped;
	if (decl->func_decl.attr_noreturn) flags |= LLVMDIFlagNoReturn;

	SourceLocation *loc = TOKLOC(decl->span.loc);
	c->debug.function = LLVMDIBuilderCreateFunction(c->debug.builder,
	                                                c->debug.file,
	                                                decl->name, TOKLEN(decl->name_token),
	                                                decl->external_name, strlen(decl->external_name),
	                                                c->debug.file,
	                                                loc->line,
	                                                llvm_get_debug_type(c, decl->type),
	                                                decl->visibility == VISIBLE_LOCAL,
	                                                true,
	                                                loc->line,
	                                                flags,
	                                                active_target.optimization_level != OPTIMIZATION_NONE);
	LLVMSetSubprogram(decl->backend_ref, c->debug.function);
}

void llvm_emit_debug_local_var(GenContext *c, Decl *decl)
{
	EMIT_LOC(c, decl);
	SourceLocation *location = TOKLOC(decl->span.loc);
	LLVMMetadataRef var = LLVMDIBuilderCreateAutoVariable(
			c->debug.builder,
			c->debug.function,
			decl->name,
			TOKLEN(decl->name_token),
			c->debug.file,
			location->line,
			llvm_get_debug_type(c, decl->type),
			active_target.optimization_level != OPTIMIZATION_NONE,
			LLVMDIFlagZero,
			decl->alignment);
	decl->var.backend_debug_ref = var;

	LLVMMetadataRef inline_at = NULL;
	LLVMDIBuilderInsertDeclareAtEnd(c->debug.builder,
	                                decl->backend_ref, var,
	                                LLVMDIBuilderCreateExpression(c->debug.builder, NULL, 0),
	                                LLVMDIBuilderCreateDebugLocation(c->context, location->line, location->col,
	                                                                 c->debug.function, inline_at),
	                                LLVMGetInsertBlock(c->builder));
}

/**
 * Setup a debug parameter for a given index.
 * @param c
 * @param parameter
 * @param index
 */
void llvm_emit_debug_parameter(GenContext *c, Decl *parameter, unsigned index)
{
	SourceLocation *loc = TOKLOC(parameter->span.loc);
	const char *name = parameter->name ? parameter->name : "anon";
	bool always_preserve = false;

	parameter->var.backend_debug_ref = LLVMDIBuilderCreateParameterVariable(
			c->debug.builder,
			c->debug.function,
			name,
			strlen(name),
			index,
			c->debug.file,
			loc->line,
			llvm_get_debug_type(c, parameter->type),
			always_preserve,
			LLVMDIFlagZero);
	LLVMMetadataRef inline_at = NULL;

	LLVMDIBuilderInsertDeclareAtEnd(c->debug.builder,
	                                parameter->backend_ref,
	                                parameter->var.backend_debug_ref,
	                                LLVMDIBuilderCreateExpression(c->debug.builder, NULL, 0),
	                                LLVMDIBuilderCreateDebugLocation(c->context, loc->line, loc->col, c->debug.function,
																  inline_at),
	                                LLVMGetInsertBlock(c->builder));


}

void llvm_emit_debug_location(GenContext *context, SourceSpan location)
{
	static TokenId last = { 0 };

	// Avoid re-emitting the same location.
	if (last.index == location.loc.index) return;
	if (!context->builder) return;

	SourceLocation *source_loc = TOKLOC(location.loc);
	LLVMMetadataRef scope = llvm_debug_current_scope(context);

	LLVMMetadataRef loc = LLVMDIBuilderCreateDebugLocation(context->context,
	                                                       source_loc->line,
	                                                       source_loc->col,
	                                                       scope, /* inlined at */ 0);

	LLVMSetCurrentDebugLocation2(context->builder, loc);
}

static LLVMMetadataRef llvm_debug_forward_comp(GenContext *c, Type *type, const char *external_name, SourceLocation *loc, LLVMMetadataRef scope, LLVMDIFlags flags)
{
	return LLVMDIBuilderCreateReplaceableCompositeType(c->debug.builder, id_counter++,
	                                                   type->name, strlen(type->name),
	                                                   scope,
	                                                   c->debug.file, loc ? loc->line : 0,
	                                                   1 /* version TODO */,
	                                                   type_size(type) * 8,
	                                                   type_abi_alignment(type) * 8,
	                                                   flags,
	                                                   external_name,
	                                                   strlen(external_name));

}
void llvm_debug_push_lexical_scope(GenContext *context, SourceSpan location)
{
	SourceLocation *source_loc = TOKLOC(location.loc);

	LLVMMetadataRef scope;
	if (vec_size(context->debug.lexical_block_stack) > 0)
	{
		scope = VECLAST(context->debug.lexical_block_stack);
	}
	else
	{
		scope = context->debug.compile_unit;
	}

	LLVMMetadataRef block =
			LLVMDIBuilderCreateLexicalBlock(context->debug.builder, scope, context->debug.file,
			                                source_loc->line,
			                                source_loc->col);

	llvm_debug_scope_push(context, block);
}


static LLVMMetadataRef llvm_debug_simple_type(GenContext *context, Type *type, int dwarf_code)
{
	return type->backend_debug_type = LLVMDIBuilderCreateBasicType(context->debug.builder,
	                                                               type->name,
	                                                               strlen(type->name),
	                                                               type->builtin.bitsize,
	                                                               dwarf_code, 0);

}

static LLVMMetadataRef llvm_debug_pointer_type(GenContext *c, Type *type)
{
	if (!type->pointer->backend_debug_type)
	{
		type->backend_debug_type = llvm_debug_forward_comp(c, type, type->name, NULL, NULL, LLVMDIFlagZero);
	}
	LLVMMetadataRef real = LLVMDIBuilderCreatePointerType(c->debug.builder,
	                                                      llvm_get_debug_type(c, type->pointer),
	                                                      type_size(type) * 8,
	                                                      type_abi_alignment(type) * 8, 0,
	                                                      type->name, strlen(type->name));
	if (type->backend_debug_type)
	{
		LLVMMetadataReplaceAllUsesWith(type->backend_debug_type, real);
	}
	return real;
}

static LLVMMetadataRef llvm_debug_enum_type(GenContext *c, Type *type, LLVMMetadataRef scope)
{
	Decl *decl = type->decl;
	SourceLocation *location = TOKLOC(decl->span.loc);

	LLVMMetadataRef forward = llvm_debug_forward_comp(c, type, decl->external_name, location, scope, LLVMDIFlagZero);
	type->backend_debug_type = forward;

	Type *enum_real_type = decl->enums.type_info->type->canonical;

	LLVMMetadataRef *elements = NULL;
	Decl **enums = decl->enums.values;

	bool is_unsigned = type_is_unsigned(enum_real_type);
	VECEACH(enums, i)
	{
		Decl *enum_constant = enums[i];
		uint64_t val = int_to_u64(enum_constant->enum_constant.expr->const_expr.ixx);
		LLVMMetadataRef debug_info = LLVMDIBuilderCreateEnumerator(
				c->debug.builder,
				enum_constant->name, TOKLEN(enum_constant->name_token),
				val,
				is_unsigned);
		vec_add(elements, debug_info);
	}

	LLVMMetadataRef real = LLVMDIBuilderCreateEnumerationType(c->debug.builder,
	                                                          scope,
	                                                          type->decl->name, TOKLEN(type->decl->name_token),
	                                                          c->debug.file, location->line, type_size(type) * 8,
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
	SourceLocation *location = TOKLOC(decl->span.loc);

	// Create a forward reference in case of recursive data.
	LLVMMetadataRef forward = llvm_debug_forward_comp(c, type, decl->external_name, location, scope, flags);
	type->backend_debug_type = forward;

	LLVMMetadataRef *elements = NULL;
	Decl **members = decl->strukt.members;
	VECEACH(members, i)
	{
		Decl *member = members[i];
		SourceLocation *member_loc = TOKLOC(member->span.loc);
		LLVMMetadataRef debug_info = llvm_get_debug_member(c,
		                                                   member->type,
		                                                   member->name ? member->name : "",
		                                                   member->offset,
		                                                   member_loc,
		                                                   forward,
		                                                   LLVMDIFlagZero);
		vec_add(elements, debug_info);
	}

	LLVMMetadataRef real;
	if (type->type_kind == TYPE_UNION)
	{
		real = LLVMDIBuilderCreateUnionType(c->debug.builder,
		                                    scope,
		                                    type->decl->name ? type->decl->name : "",
		                                    type->decl->name ? TOKLEN(type->decl->name_token) : 0,
		                                    c->debug.file, location->line, type_size(type) * 8,
		                                    type_abi_alignment(type) * 8,
		                                    LLVMDIFlagZero,
		                                    elements, vec_size(members),
		                                    c->debug.runtime_version,
		                                    type->decl->name ? decl->external_name : "",
		                                    type->decl->name ? strlen(decl->external_name) : 0);
		LLVMMetadataReplaceAllUsesWith(forward, real);
		return real;
	}
	return llvm_get_debug_struct(c, type, decl->name ? decl->external_name : "", elements, vec_size(elements), location, scope, LLVMDIFlagZero);
}

static LLVMMetadataRef llvm_debug_subarray_type(GenContext *c, Type *type)
{
	LLVMMetadataRef forward = llvm_debug_forward_comp(c, type, type->name, NULL, NULL, LLVMDIFlagZero);
	type->backend_debug_type = forward;

	LLVMMetadataRef elements[2] = {
			llvm_get_debug_member(c, type_get_ptr(type->array.base), "ptr", 0, NULL, forward, LLVMDIFlagZero),
			llvm_get_debug_member(c, type_usize, "len", 0, NULL, forward, LLVMDIFlagZero)
	};
	return llvm_get_debug_struct(c, type, type->name, elements, 2, NULL, NULL, LLVMDIFlagZero);
}


static LLVMMetadataRef llvm_debug_errunion_type(GenContext *c, Type *type)
{
	return LLVMDIBuilderCreateTypedef(c->debug.builder,
									  llvm_get_debug_type(c, type_iptr->canonical),
									  type->name, strlen(type->name),
									  NULL, 0, NULL, 0);
}

static LLVMMetadataRef llvm_debug_array_type(GenContext *c, Type *type)
{
	LLVMMetadataRef *ranges = NULL;
	Type *current_type = type;
	while (current_type->canonical->type_kind == TYPE_ARRAY)
	{
		vec_add(ranges, LLVMDIBuilderGetOrCreateSubrange(c->debug.builder, 0, current_type->canonical->array.len));
		current_type = current_type->canonical->array.base;
	}
	if (!current_type->backend_debug_type)
	{
		type->backend_debug_type = llvm_debug_forward_comp(c, type, type->name, NULL, NULL, LLVMDIFlagZero);
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
	if (!decl)
	{
		return LLVMDIBuilderCreateTypedef(c->debug.builder,
		                                  llvm_get_debug_type(c, type->canonical),
		                                  type->name, strlen(type->name),
		                                  NULL, 0, NULL, 0);
	}

	Type *original_type = type->type_kind == TYPE_TYPEDEF ? type->canonical : decl->distinct_decl.base_type;

	SourceLocation *location = TOKLOC(decl->span.loc);
	// Use forward references in case we haven't resolved the original type, since we could have this:
	if (!type->canonical->backend_debug_type)
	{
		type->backend_debug_type = llvm_debug_forward_comp(c, type, type->name, location, NULL, LLVMDIFlagZero);
	}
	LLVMMetadataRef real = LLVMDIBuilderCreateTypedef(c->debug.builder,
	                                                  llvm_get_debug_type(c, original_type),
	                                                  decl->name, TOKLEN(decl->name_token),
	                                                  c->debug.file, location->line,
	                                                  c->debug.file, type_abi_alignment(type));
	if (type->backend_debug_type)
	{
		LLVMMetadataReplaceAllUsesWith(type->backend_debug_type, real);
	}
	return real;
}

static LLVMMetadataRef llvm_debug_vector_type(GenContext *c, Type *type)
{
	LLVMMetadataRef *ranges = NULL;
	Type *current_type = type;
	REMINDER("Handle Recursive Vector type");
	while (current_type->canonical->type_kind == TYPE_VECTOR)
	{
		vec_add(ranges, LLVMDIBuilderGetOrCreateSubrange(c->debug.builder, 0, current_type->canonical->vector.len));
		current_type = current_type->canonical->vector.base;
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
	FunctionSignature *sig = type->func.signature;
	static LLVMMetadataRef *buffer = NULL;
	vec_resize(buffer, 0);
	vec_add(buffer, llvm_get_debug_type(c, sig->rtype->type));
	VECEACH(sig->params, i)
	{
		vec_add(buffer, llvm_get_debug_type(c, sig->params[i]->type));
	}
	return LLVMDIBuilderCreateSubroutineType(c->debug.builder,
	                                         c->debug.file,
	                                         buffer,
	                                         vec_size(buffer), 0);
}


static inline LLVMMetadataRef llvm_get_debug_type_internal(GenContext *c, Type *type, LLVMMetadataRef scope)
{
	if (type->backend_debug_type) return type->backend_debug_type;
	// Consider special handling of UTF8 arrays.
	switch (type->type_kind)
	{
		case TYPE_TYPEID:
		case TYPE_STRLIT:
		case CT_TYPES:
			UNREACHABLE
		case TYPE_FAILABLE:
			// If this is reachable then we're not doing the proper lowering.
			UNREACHABLE
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
		case TYPE_F32:
		case TYPE_F64:
		case TYPE_F128:
			return llvm_debug_simple_type(c, type, DW_ATE_float);
		case TYPE_VECTOR:
			return type->backend_debug_type = llvm_debug_vector_type(c, type);
		case TYPE_VOID:
			return NULL;
		case TYPE_POINTER:
			return type->backend_debug_type = llvm_debug_pointer_type(c, type);
		case TYPE_ENUM:
			return type->backend_debug_type = llvm_debug_enum_type(c, type, scope);
		case TYPE_ERRTYPE:
			return type->backend_debug_type = llvm_debug_enum_type(c, type, scope);
		case TYPE_FUNC:
			return type->backend_debug_type = llvm_debug_func_type(c, type);
		case TYPE_BITSTRUCT:
			TODO
		case TYPE_STRUCT:
		case TYPE_UNION:
			return type->backend_debug_type = llvm_debug_structlike_type(c, type, scope);
		case TYPE_DISTINCT:
		case TYPE_TYPEDEF:
			return type->backend_debug_type = llvm_debug_typedef_type(c, type);
		case TYPE_ARRAY:
			return type->backend_debug_type = llvm_debug_array_type(c, type);
		case TYPE_SUBARRAY:
			return type->backend_debug_type = llvm_debug_subarray_type(c, type);
		case TYPE_ANYERR:
			return type->backend_debug_type = llvm_debug_errunion_type(c, type);
		case TYPE_VIRTUAL:
		case TYPE_VIRTUAL_ANY:
			TODO
	}
	UNREACHABLE
}

LLVMMetadataRef llvm_get_debug_type(GenContext *c, Type *type)
{
	// All types should be generated in the outer scope.
	return llvm_get_debug_type_internal(c, type, c->debug.file);
}
