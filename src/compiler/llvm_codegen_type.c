// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "llvm_codegen_internal.h"



static inline LLVMTypeRef gencontext_create_llvm_type_from_decl(GenContext *context, Decl *decl)
{
	static LLVMTypeRef params[512];
	static LLVMMetadataRef debug_params[512];
	switch (decl->decl_kind)
	{
		case DECL_ATTRIBUTE:
		case DECL_POISONED:
		case DECL_GENERIC:
		case DECL_MULTI_DECL:
		case DECL_MACRO:
		case DECL_CT_IF:
		case DECL_CT_ELSE:
		case DECL_CT_ELIF:
			UNREACHABLE;
		case DECL_FUNC:
		{
			VECEACH(decl->func.function_signature.params, i)
			{
				Type *param_type = decl->func.function_signature.params[i]->var.type;
				params[i] = gencontext_get_llvm_type(context, param_type);
				if (build_options.debug_info)
				{
					assert(param_type->backend_debug_type);
					debug_params[i + 1] = param_type->backend_debug_type;
				}
			}
			unsigned param_size = vec_size(decl->func.function_signature.params);
			if (gencontext_use_debug(context))
			{
				gencontext_get_llvm_type(context, decl->func.function_signature.rtype);
				debug_params[0] = decl->func.function_signature.rtype->backend_debug_type;
				LLVMMetadataRef function_type = LLVMDIBuilderCreateSubroutineType(context->debug.builder,
				                                                                  context->debug.file,
				                                                                  debug_params, param_size + 1,
						/** TODO **/ 0);
				decl->self_type->backend_debug_type = function_type;
			}
			return LLVMFunctionType(gencontext_get_llvm_type(context, decl->func.function_signature.rtype),
			                        params,
			                        param_size,
			                        decl->func.function_signature.variadic);

		}

		case DECL_VAR:
			break;
		case DECL_ENUM_CONSTANT:
			break;
		case DECL_TYPEDEF:
			break;
		case DECL_STRUCT:
		{
			LLVMTypeRef *types = NULL;
			VECEACH(decl->strukt.members, i)
			{
				VECADD(types, gencontext_get_llvm_type(context, decl->strukt.members[i]->var.type));
			}
			LLVMTypeRef type = LLVMStructCreateNamed(context->context, decl->name.string);
			LLVMStructSetBody(type, types, vec_size(types), decl->is_packed);
			return type;
		}
		case DECL_UNION:
			break;
		case DECL_ENUM:
			break;
		case DECL_ERROR:
			break;
		case DECL_ERROR_CONSTANT:
			break;
		case DECL_ARRAY_VALUE:
			break;
		case DECL_IMPORT:
			UNREACHABLE
	}
	UNREACHABLE
}
static inline LLVMTypeRef gencontext_create_llvm_type_from_ptr(GenContext *context, Type *type)
{
	LLVMTypeRef base_llvm_type = gencontext_get_llvm_type(context, type->base);

	if (gencontext_use_debug(context))
	{
		type->backend_debug_type = LLVMDIBuilderCreatePointerType(context->debug.builder, type->base->backend_debug_type, type_size(type->canonical->base), 0, 0, /* TODO */ "TODO", 4);
	}

	if (type->canonical != type)
	{
		return type->backend_type = gencontext_get_llvm_type(context, type->canonical);
	}

	return type->backend_type = LLVMPointerType(base_llvm_type, /** TODO **/0);
}

static inline LLVMTypeRef gencontext_create_llvm_type_from_array(GenContext *context, Type *type)
{
	LLVMTypeRef base_llvm_type = gencontext_get_llvm_type(context, type->base);

	if (gencontext_use_debug(context))
	{
		LLVMMetadataRef *ranges = NULL;
		Type *current_type = type;
		while (current_type->canonical->type_kind == TYPE_ARRAY)
		{
			VECADD(ranges, LLVMDIBuilderGetOrCreateSubrange(context->debug.builder, 0, current_type->canonical->len));
			current_type = current_type->canonical->base;
		}
		type->backend_debug_type = LLVMDIBuilderCreateArrayType(
				context->debug.builder,
				type->len,
				0 /* ALIGN */,
				type->base->backend_debug_type,
				ranges, vec_size(ranges));
	}

	if (type->canonical != type)
	{
		return type->backend_type = gencontext_get_llvm_type(context, type->canonical);
	}

	return type->backend_type = LLVMPointerType(base_llvm_type, /** TODO **/0);
}

LLVMTypeRef gencontext_create_llvm_func_type(GenContext *context, Type *type)
{
	LLVMTypeRef *params = NULL;
	FunctionSignature *signature = type->func.signature;
	// TODO throws
	if (vec_size(signature->params))
	{
		params = malloc_arena(sizeof(LLVMTypeRef) * vec_size(signature->params));
		VECEACH(signature->params, i)
		{
			params[i] = gencontext_get_llvm_type(context, signature->params[i]->var.type->canonical);
		}
	}
	return LLVMFunctionType(
			gencontext_get_llvm_type(context, type->func.signature->rtype),
			params, vec_size(signature->params), signature->variadic);
}

LLVMTypeRef gencontext_get_llvm_type(GenContext *context, Type *type)
{
	if (type->backend_type) return type->backend_type;
	switch (type->type_kind)
	{
		case TYPE_USER_DEFINED:
			return type->backend_type = gencontext_create_llvm_type_from_decl(context, type->decl);
		case TYPE_FUNC:
			return type->backend_type = gencontext_create_llvm_func_type(context, type);
		case TYPE_VOID:
		case TYPE_F64:
		case TYPE_F32:
		case TYPE_U64:
		case TYPE_POISONED:
		case TYPE_BOOL:
		case TYPE_I8:
		case TYPE_I16:
		case TYPE_I32:
		case TYPE_I64:
		case TYPE_IXX:
		case TYPE_U8:
		case TYPE_U16:
		case TYPE_U32:
		case TYPE_UXX:
		case TYPE_FXX:
		case TYPE_EXPRESSION:
		case TYPE_INC_ARRAY:
			UNREACHABLE;
		case TYPE_POINTER:
			return type->backend_type = gencontext_create_llvm_type_from_ptr(context, type);
		case TYPE_STRING:
			TODO;
		case TYPE_ARRAY:
			return type->backend_type = gencontext_create_llvm_type_from_array(context, type);
		case TYPE_VARARRAY:
			TODO
	}
	UNREACHABLE;
}
