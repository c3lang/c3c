// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_internal.h"

LLVMTypeRef llvm_get_type(LLVMContextRef context, Type *any_type);

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
		case DECL_ARRAY_VALUE:
		case DECL_IMPORT:
		case DECL_MEMBER:
		case DECL_LABEL:
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
			LLVMTypeRef type = LLVMStructCreateNamed(context, decl->name);
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
		case DECL_ERR:
		{
			LLVMTypeRef *types = NULL;
			vec_add(types, llvm_get_type(context, type_typeid));
			unsigned size = type_size(type_typeid);
			VECEACH(decl->strukt.members, i)
			{
				Type *type = decl->strukt.members[i]->type->canonical;
				unsigned alignment = type_abi_alignment(type);
				if (size % alignment != 0)
				{
					size += alignment - size % alignment;
				}
				size += type_size(type);
				vec_add(types, llvm_get_type(context, type));
			}
			unsigned padding = type_size(type_error) - size;
			if (padding > 0)
			{
				vec_add(types, LLVMIntTypeInContext(context, padding * 8));
			}
			LLVMTypeRef type = LLVMStructCreateNamed(context, decl->name);
			LLVMStructSetBody(type, types, vec_size(types), false);
			return type;
		}
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
	if (parameters)
	{
		params = malloc_arena(sizeof(LLVMTypeRef) * parameters);
		unsigned index = 0;
		if (signature->return_param)
		{
			params[index++] = llvm_get_type(context, type_get_ptr(signature->rtype->type));
		}
		VECEACH(signature->params, i)
		{
			params[index++] = llvm_get_type(context, signature->params[i]->type->canonical);
		}
	}
	LLVMTypeRef ret_type;
	if (signature->failable)
	{
		ret_type = llvm_get_type(context, type_error);
	}
	else
	{
		ret_type = signature->return_param
				? llvm_get_type(context, type_void)
				: llvm_get_type(context, type->func.signature->rtype->type);
	}
	return LLVMFunctionType(ret_type, params, parameters, signature->variadic);
}


LLVMTypeRef llvm_get_type(LLVMContextRef context, Type *any_type)
{
	if (any_type->backend_type && LLVMGetTypeContext(any_type->backend_type) == context)
	{
		return any_type->backend_type;
	}
	DEBUG_LOG("Generating type %s", any_type->name);
	switch (any_type->type_kind)
	{
		case TYPE_MEMBER:
		case TYPE_POISONED:
			UNREACHABLE
		case TYPE_TYPEID:
			return any_type->backend_type = LLVMIntTypeInContext(context, any_type->builtin.bitsize);
		case TYPE_TYPEDEF:
			return any_type->backend_type = llvm_get_type(context, any_type->canonical);
		case TYPE_ENUM:
			return any_type->backend_type = llvm_get_type(context, any_type->decl->enums.type_info->type->canonical);
		case TYPE_ERR_UNION:
			return any_type->backend_type = LLVMIntTypeInContext(context, any_type->builtin.bitsize);
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_ERRTYPE:
			return any_type->backend_type = llvm_type_from_decl(context, any_type->decl);
		case TYPE_FUNC:
			return any_type->backend_type = llvm_func_type(context, any_type);
		case TYPE_VOID:
			return any_type->backend_type = LLVMVoidTypeInContext(context);
		case TYPE_F64:
		case TYPE_FXX:
			return any_type->backend_type = LLVMDoubleTypeInContext(context);
		case TYPE_F32:
			return any_type->backend_type = LLVMFloatTypeInContext(context);
		case TYPE_U64:
		case TYPE_I64:
			return any_type->backend_type = LLVMIntTypeInContext(context, 64U);
		case TYPE_U32:
		case TYPE_I32:
		case TYPE_IXX:
			return any_type->backend_type = LLVMIntTypeInContext(context, 32U);
		case TYPE_U16:
		case TYPE_I16:
			return any_type->backend_type = LLVMIntTypeInContext(context, 16U);
		case TYPE_U8:
		case TYPE_I8:
			return any_type->backend_type = LLVMIntTypeInContext(context, 8U);
		case TYPE_BOOL:
			return any_type->backend_type = LLVMIntTypeInContext(context, 1U);
		case TYPE_POINTER:
			return any_type->backend_type = llvm_type_from_ptr(context, any_type);
		case TYPE_STRING:
			// TODO
			return any_type->backend_type = LLVMPointerType(llvm_get_type(context, type_char), 0);
		case TYPE_ARRAY:
			return any_type->backend_type = llvm_type_from_array(context, any_type);
		case TYPE_SUBARRAY:
		{
			LLVMTypeRef base_type = llvm_get_type(context, type_get_ptr(any_type->array.base));
			LLVMTypeRef size_type = llvm_get_type(context, type_usize);
			LLVMTypeRef array_type = LLVMStructCreateNamed(context, any_type->name);
			LLVMTypeRef types[2] = { base_type, size_type };
			LLVMStructSetBody(array_type, types, 2, false);
			return any_type->backend_type = array_type;
		}
		case TYPE_VARARRAY:
			return any_type->backend_type = llvm_get_type(context, type_get_ptr(any_type->array.base));
	}
	UNREACHABLE;
}

LLVMTypeRef gencontext_get_llvm_type(GenContext *context, Type *type)
{
	// gencontext_get_debug_type(context, type);
	return llvm_get_type(context->context, type);
}

