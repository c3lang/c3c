// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_internal.h"

#define LLVMCONTEXT(gen_context) (gen_context ? gen_context->context : LLVMGetGlobalContext())

static inline LLVMTypeRef gencontext_create_llvm_type_from_decl(GenContext *context, Decl *decl)
{
	static LLVMTypeRef params[512];
	switch (decl->decl_kind)
	{
		case DECL_ATTRIBUTE:
		case DECL_ENUM_CONSTANT:
		case DECL_POISONED:
		case DECL_GENERIC:
		case DECL_MACRO:
		case DECL_CT_IF:
		case DECL_CT_ELSE:
		case DECL_CT_ELIF:
		case DECL_VAR:
		case DECL_ERROR_CONSTANT:
		case DECL_ARRAY_VALUE:
		case DECL_IMPORT:
			UNREACHABLE;
		case DECL_FUNC:
		{
			VECEACH(decl->func.function_signature.params, i)
			{
				params[i] = BACKEND_TYPE(decl->func.function_signature.params[i]->type);
			}
			unsigned param_size = vec_size(decl->func.function_signature.params);
			return LLVMFunctionType(BACKEND_TYPE(decl->func.function_signature.rtype->type),
			                        params,
			                        param_size,
			                        decl->func.function_signature.variadic);

		}
		case DECL_TYPEDEF:
			return BACKEND_TYPE(decl->typedef_decl.type);
		case DECL_STRUCT:
		{
			LLVMTypeRef *types = NULL;
			VECEACH(decl->strukt.members, i)
			{
				VECADD(types, BACKEND_TYPE(decl->strukt.members[i]->type));
			}
			// TODO fix name.
			LLVMTypeRef type = LLVMStructCreateNamed(LLVMCONTEXT(context), decl->external_name);
			LLVMStructSetBody(type, types, vec_size(types), decl->is_packed);
			return type;
		}
		case DECL_UNION:
		{
			LLVMTypeRef max_type = NULL;
			unsigned long long max_size = 0;
			VECEACH(decl->strukt.members, i)
			{
				LLVMTypeRef type = BACKEND_TYPE(decl->strukt.members[i]->type);
				unsigned long long size = LLVMStoreSizeOfType(target_data_layout(), type);
				if (size > max_size || !max_type)
				{
					max_size = size;
					max_type = type;
				}
			}
			LLVMTypeRef type = LLVMStructCreateNamed(LLVMCONTEXT(context), decl->external_name);
			LLVMStructSetBody(type, &max_type, 1, false);
			return type;
		}
		case DECL_ENUM:
			return BACKEND_TYPE(decl->type);
		case DECL_ERROR:
			if (!context->error_type)
			{
				LLVMTypeRef domain_type = LLVMInt64TypeInContext(LLVMCONTEXT(context));
				LLVMTypeRef pointer_type = BACKEND_TYPE(type_voidptr);
				LLVMTypeRef error_type = LLVMStructCreateNamed(LLVMCONTEXT(context), "error");
				LLVMTypeRef types[2] = { domain_type, pointer_type };
				LLVMStructSetBody(error_type, types, 2, false);
				context->error_type = error_type;
			}
			return context->error_type;
		case DECL_THROWS:
			UNREACHABLE
	}
	UNREACHABLE
}
static inline LLVMTypeRef gencontext_create_llvm_type_from_ptr(GenContext *context, Type *type)
{
	LLVMTypeRef base_llvm_type = BACKEND_TYPE(type->pointer);
	vec_add(context->generated_types, type);

	if (type->canonical != type)
	{
		return type->backend_type = BACKEND_TYPE(type->canonical);
	}

	return type->backend_type = LLVMPointerType(base_llvm_type, /** TODO **/0);
}

static inline LLVMTypeRef gencontext_create_llvm_type_from_array(GenContext *context, Type *type)
{
	LLVMTypeRef base_llvm_type = BACKEND_TYPE(type->array.base);

	vec_add(context->generated_types, type);

	if (type->canonical != type)
	{
		return type->backend_type = BACKEND_TYPE(type->canonical);
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
			params[i] = BACKEND_TYPE(signature->params[i]->type->canonical);
		}
	}
	return LLVMFunctionType(
			BACKEND_TYPE(type->func.signature->rtype->type),
			params, vec_size(signature->params), signature->variadic);
}


LLVMTypeRef gencontext_get_llvm_type(GenContext *context, Type *type)
{
	if (type->backend_type)
	{
		assert(LLVMGetTypeContext(type->backend_type) == context->context);
		return type->backend_type;
	}
	vec_add(context->generated_types, type);

	DEBUG_LOG("Generating type %s", type->name);
	switch (type->type_kind)
	{
		case TYPE_TYPEDEF:
			return type->backend_type = BACKEND_TYPE(type->canonical);
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_ENUM:
		case TYPE_ERROR:
		case TYPE_ERROR_UNION:
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
			UNREACHABLE;
		case TYPE_POINTER:
			return type->backend_type = gencontext_create_llvm_type_from_ptr(context, type);
		case TYPE_STRING:
			// TODO
			return type->backend_type = LLVMPointerType(LLVMTYPE(type_char), 0);
		case TYPE_ARRAY:
			return type->backend_type = gencontext_create_llvm_type_from_array(context, type);
		case TYPE_SUBARRAY:
		{
			LLVMTypeRef base_type = BACKEND_TYPE(type->array.base);
			LLVMTypeRef size_type = BACKEND_TYPE(type_usize);
			assert(type->array.base->canonical->type_kind == TYPE_POINTER);
			LLVMTypeRef array_type = LLVMStructCreateNamed(LLVMCONTEXT(context), type->name);
			LLVMTypeRef types[2] = { base_type, size_type };
			LLVMStructSetBody(array_type, types, 2, false);
			return type->backend_type = array_type;
		}
		case TYPE_VARARRAY:
			return type->backend_type = LLVMPointerType(BACKEND_TYPE(type->array.base), 0);
	}
	UNREACHABLE;
}

