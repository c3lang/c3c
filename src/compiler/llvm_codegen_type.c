// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_internal.h"

LLVMTypeRef llvm_get_type(LLVMContextRef context, Type *type);

static inline LLVMTypeRef llvm_type_from_decl(LLVMContextRef context, Decl *decl)
{
	static LLVMTypeRef params[MAX_PARAMS];
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
				params[i] = llvm_get_type(context, decl->func.function_signature.params[i]->type);
			}
			unsigned param_size = vec_size(decl->func.function_signature.params);
			return LLVMFunctionType(llvm_get_type(context, decl->func.function_signature.rtype->type),
			                        params,
			                        param_size,
			                        decl->func.function_signature.variadic);

		}
		case DECL_TYPEDEF:
			return llvm_get_type(context, decl->typedef_decl.type_info->type);
		case DECL_STRUCT:
		{
			LLVMTypeRef *types = NULL;
			VECEACH(decl->strukt.members, i)
			{
				vec_add(types, llvm_get_type(context, decl->strukt.members[i]->type));
			}
			LLVMTypeRef type = LLVMStructCreateNamed(context, decl->external_name);
			LLVMStructSetBody(type, types, vec_size(types), decl->is_packed);
			return type;
		}
		case DECL_UNION:
		{
			Decl *max_type = NULL;
			unsigned long long max_size = 0;
			VECEACH(decl->strukt.members, i)
			{
				Decl *member = decl->strukt.members[i];
				unsigned size = type_size(member->type);
				if (size > max_size || !max_type)
				{
					max_size = size;
					max_type = member;
				}
			}
			LLVMTypeRef type = LLVMStructCreateNamed(context, decl->external_name);
			if (max_type)
			{
				LLVMTypeRef type_ref = llvm_get_type(context, max_type->type);
				LLVMStructSetBody(type, &type_ref, 1, false);
			}
			else
			{
				LLVMStructSetBody(type, NULL, 0, true);
			}
			return type;
		}
		case DECL_ENUM:
			return llvm_get_type(context, decl->type);
		case DECL_ERROR:
			TODO
			/*
			if (!context->error_type)
			{
				LLVMTypeRef domain_type = LLVMInt64TypeInContext(LLVMCONTEXT(context));
				LLVMTypeRef pointer_type = BACKEND_TYPE(type_voidptr);
				LLVMTypeRef error_type = LLVMStructCreateNamed(LLVMCONTEXT(context), "error");
				LLVMTypeRef types[2] = { domain_type, pointer_type };
				LLVMStructSetBody(error_type, types, 2, false);
				context->error_type = error_type;
			}
			return context->error_type;*/
		case DECL_THROWS:
			UNREACHABLE
	}
	UNREACHABLE
}
static inline LLVMTypeRef llvm_type_from_ptr(LLVMContextRef context, Type *type)
{
	LLVMTypeRef base_llvm_type = llvm_get_type(context, type->pointer);

	if (type->canonical != type)
	{
		return type->backend_type = llvm_get_type(context, type->canonical);
	}

	return type->backend_type = LLVMPointerType(base_llvm_type, /** TODO **/0);
}

static inline LLVMTypeRef llvm_type_from_array(LLVMContextRef context, Type *type)
{
	if (type->canonical != type)
	{
		return type->backend_type = llvm_get_type(context, type->canonical);
	}

	LLVMTypeRef base_llvm_type = llvm_get_type(context, type->array.base);
	return type->backend_type = LLVMArrayType(base_llvm_type, type->array.len);
}

LLVMTypeRef llvm_func_type(LLVMContextRef context, Type *type)
{
	LLVMTypeRef *params = NULL;
	FunctionSignature *signature = type->func.signature;
	unsigned parameters = vec_size(signature->params);
	if (signature->return_param) parameters++;
	if (signature->error_return == ERROR_RETURN_PARAM) parameters++;
	if (parameters)
	{
		params = malloc_arena(sizeof(LLVMTypeRef) * parameters);
		unsigned index = 0;
		if (signature->return_param)
		{
			params[index++] = llvm_get_type(context, type_get_ptr(signature->rtype->type));
		}
		if (signature->error_return == ERROR_RETURN_PARAM)
		{
			params[index++] = llvm_get_type(context, type_get_ptr(type_error_union));
		}
		VECEACH(signature->params, i)
		{
			params[index++] = llvm_get_type(context, signature->params[i]->type->canonical);
		}
	}
	LLVMTypeRef ret_type;
	if (signature->error_return == ERROR_RETURN_RETURN)
	{
		ret_type = llvm_get_type(context, type_ulong);
	}
	else
	{
		ret_type = signature->return_param ? llvm_get_type(context, type_void) : llvm_get_type(context, type->func.signature->rtype->type);
	}
	LLVMTypeRef functype = LLVMFunctionType(ret_type, params, parameters, signature->variadic);
	return functype;
}


LLVMTypeRef llvm_get_type(LLVMContextRef context, Type *type)
{
	if (type->backend_type && LLVMGetTypeContext(type->backend_type) == context)
	{
		return type->backend_type;
	}
	DEBUG_LOG("Generating type %s", type->name);
	switch (type->type_kind)
	{
		case TYPE_POISONED:
		case TYPE_META_TYPE:
		case TYPE_ERROR:
			UNREACHABLE;
		case TYPE_TYPEDEF:
			return type->backend_type = llvm_get_type(context, type->canonical);
		case TYPE_ENUM:
			return type->backend_type = llvm_get_type(context, type->decl->enums.type_info->type->canonical);
		case TYPE_ERROR_UNION:
		{
			LLVMTypeRef types[2];
			types[0] = llvm_get_type(context, type_typeid->canonical);
			types[1] = llvm_get_type(context, type_error->canonical);
			return type->backend_type = LLVMStructType(types, 2, false);
		}
		case TYPE_STRUCT:
		case TYPE_UNION:
			return type->backend_type = llvm_type_from_decl(context, type->decl);
		case TYPE_FUNC:
			return type->backend_type = llvm_func_type(context, type);
		case TYPE_VOID:
			return type->backend_type = LLVMVoidTypeInContext(context);
		case TYPE_F64:
		case TYPE_FXX:
			return type->backend_type = LLVMDoubleTypeInContext(context);
		case TYPE_F32:
			return type->backend_type = LLVMFloatTypeInContext(context);
		case TYPE_U64:
		case TYPE_I64:
			return type->backend_type = LLVMIntTypeInContext(context, 64U);
		case TYPE_U32:
		case TYPE_I32:
		case TYPE_IXX:
			return type->backend_type = LLVMIntTypeInContext(context, 32U);
		case TYPE_U16:
		case TYPE_I16:
			return type->backend_type = LLVMIntTypeInContext(context, 16U);
		case TYPE_U8:
		case TYPE_I8:
			return type->backend_type = LLVMIntTypeInContext(context, 8U);
		case TYPE_BOOL:
			return type->backend_type = LLVMIntTypeInContext(context, 1U);
		case TYPE_POINTER:
			return type->backend_type = llvm_type_from_ptr(context, type);
		case TYPE_STRING:
			// TODO
			return type->backend_type = LLVMPointerType(llvm_get_type(context, type_char), 0);
		case TYPE_ARRAY:
			return type->backend_type = llvm_type_from_array(context, type);
		case TYPE_SUBARRAY:
		{
			LLVMTypeRef base_type = llvm_get_type(context, type->array.base);
			LLVMTypeRef size_type = llvm_get_type(context, type_usize);
			assert(type->array.base->canonical->type_kind == TYPE_POINTER);
			LLVMTypeRef array_type = LLVMStructCreateNamed(context, type->name);
			LLVMTypeRef types[2] = { base_type, size_type };
			LLVMStructSetBody(array_type, types, 2, false);
			return type->backend_type = array_type;
		}
		case TYPE_VARARRAY:
			return type->backend_type = LLVMPointerType(llvm_get_type(context, type->array.base), 0);
	}
	UNREACHABLE;
}

LLVMTypeRef gencontext_get_llvm_type(GenContext *context, Type *type)
{
	return llvm_get_type(context->context, type);
}

