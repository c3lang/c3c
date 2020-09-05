// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_internal.h"

static inline LLVMMetadataRef gencontext_create_debug_type_from_decl(GenContext *context, Decl *decl)
{
	static LLVMMetadataRef debug_params[512];
	switch (decl->decl_kind)
	{
		case DECL_POISONED:
		case DECL_VAR:
		case DECL_ENUM_CONSTANT:
		case NON_TYPE_DECLS:
			UNREACHABLE;
		case DECL_FUNC:
		{
			VECEACH(decl->func.function_signature.params, i)
			{
				Type *param_type = decl->func.function_signature.params[i]->type;
				debug_params[i + 1] = gencontext_get_debug_type(context, param_type);
			}
			unsigned param_size = vec_size(decl->func.function_signature.params);
			debug_params[0] = decl->func.function_signature.rtype->type->backend_debug_type;
			return LLVMDIBuilderCreateSubroutineType(context->debug.builder,
			                                                                  context->debug.file,
			                                                                  debug_params, param_size + 1,
					/** TODO **/ 0);
		}
		case DECL_TYPEDEF:
			TODO
		case DECL_STRUCT:
			TODO
		case DECL_UNION:
			TODO
		case DECL_ENUM:
			TODO
		case DECL_ERR:
			TODO
	}
	UNREACHABLE
}

void gencontext_push_debug_scope(GenContext *context, LLVMMetadataRef debug_scope)
{
	VECADD(context->debug.lexical_block_stack, debug_scope);
}

void gencontext_pop_debug_scope(GenContext *context)
{
	vec_pop(context->debug.lexical_block_stack);
}

void gencontext_emit_debug_location(GenContext *context, SourceSpan location)
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

	LLVMMetadataRef loc = LLVMDIBuilderCreateDebugLocation(context->context,
	                                                       source_loc->line,
	                                                       source_loc->col,
	                                                       scope, /* inlined at */ 0);

	LLVMSetCurrentDebugLocation2(context->builder, loc);
}

void gencontext_debug_push_lexical_scope(GenContext *context, SourceSpan location)
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

	gencontext_push_debug_scope(context, block);
}


static LLVMMetadataRef gencontext_simple_debug_type(GenContext *context, Type *type, int dwarf_code)
{
	return type->backend_debug_type = LLVMDIBuilderCreateBasicType(context->debug.builder,
	                                                               type->name,
	                                                               strlen(type->name),
	                                                               type->builtin.bitsize,
	                                                               dwarf_code, 0);

}

static LLVMMetadataRef gencontext_func_debug_type(GenContext *context, Type *type)
{
	FunctionSignature *sig = type->func.signature;
	static LLVMMetadataRef *buffer = NULL;
	vec_resize(buffer, 0);
	vec_add(buffer, llvm_debug_type(sig->rtype->type));
	VECEACH(sig->params, i)
	{
		vec_add(buffer, llvm_debug_type(sig->params[i]->type));
	}
	return LLVMDIBuilderCreateSubroutineType(context->debug.builder,
	                                         context->debug.file,
	                                         buffer,
	                                         vec_size(buffer), 0);
}

LLVMMetadataRef gencontext_get_debug_type(GenContext *context, Type *type)
{
	if (type->backend_debug_type) return type->backend_debug_type;
	// Consider special handling of UTF8 arrays.
	switch (type->type_kind)
	{
		case TYPE_POISONED:
		case TYPE_IXX:
		case TYPE_FXX:
		case TYPE_TYPEID:
		case TYPE_TYPEINFO:
		case TYPE_MEMBER:
			UNREACHABLE
		case TYPE_BOOL:
			return gencontext_simple_debug_type(context, type, DW_ATE_boolean);
		case TYPE_I8:
			return gencontext_simple_debug_type(context, type, DW_ATE_signed_char); // DW_ATE_UTF?
		case TYPE_U8:
			return gencontext_simple_debug_type(context, type, DW_ATE_unsigned_char);
		case TYPE_I16:
		case TYPE_I32:
		case TYPE_I64:
			return gencontext_simple_debug_type(context, type, DW_ATE_signed);
		case TYPE_U16:
		case TYPE_U32:
		case TYPE_U64:
			return gencontext_simple_debug_type(context, type, DW_ATE_unsigned);
		case TYPE_F32:
		case TYPE_F64:
			return gencontext_simple_debug_type(context, type, DW_ATE_float);
		case TYPE_VOID:
			return NULL;
		case TYPE_POINTER:
			return type->backend_debug_type = LLVMDIBuilderCreatePointerType(context->debug.builder, type->pointer->backend_debug_type, type_size(type->canonical->pointer), 0, 0, type->name, strlen(type->name));
		case TYPE_ENUM:
			TODO
		case TYPE_ERRTYPE:
			TODO
		case TYPE_FUNC:
			return type->backend_debug_type = gencontext_func_debug_type(context, type);
		case TYPE_STRUCT:
//			LLVMDIBuilderCreateStructType(context->debug.builder, NULL, type->decl->name, strlen(type->decl->name), type->decl->module->)
			TODO
		case TYPE_UNION:
			TODO
		case TYPE_TYPEDEF:
			TODO
		case TYPE_STRING:
			TODO
		case TYPE_ARRAY:
			{
				LLVMMetadataRef *ranges = NULL;
				Type *current_type = type;
				while (current_type->canonical->type_kind == TYPE_ARRAY)
				{
					VECADD(ranges, LLVMDIBuilderGetOrCreateSubrange(context->debug.builder, 0, current_type->canonical->array.len));
					current_type = current_type->canonical->array.base;
				}
				return type->backend_debug_type = LLVMDIBuilderCreateArrayType(
						context->debug.builder,
						type->array.len,
						type_abi_alignment(type->array.base),
						llvm_debug_type(type->array.base),
						ranges, vec_size(ranges));
			}
		case TYPE_VARARRAY:
			TODO
		case TYPE_SUBARRAY:
			TODO
		case TYPE_ERR_UNION:
			TODO
	}
	UNREACHABLE
}
