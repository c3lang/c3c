// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_internal.h"


static inline LLVMTypeRef llvm_type_from_decl(GenContext *context, Decl *decl)
{
	static LLVMTypeRef params[MAX_PARAMS];
	switch (decl->decl_kind)
	{
		case DECL_VAR:
		case DECL_ENUM_CONSTANT:
		case DECL_POISONED:
		case NON_TYPE_DECLS:
			UNREACHABLE
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
			LLVMTypeRef type = LLVMStructCreateNamed(context->context, decl->external_name);
			// Avoid recursive issues.
			decl->type->backend_type = type;
			Decl **members = decl->strukt.members;
			VECEACH(members, i)
			{
				vec_add(types, llvm_get_type(context, members[i]->type));
			}
			if (decl->needs_additional_pad)
			{
				Decl *last_member = VECLAST(members);
				unsigned member_end = last_member->offset + type_size(last_member->type);
				unsigned bytes = decl->strukt.size - member_end;
				assert(bytes > 0);
				if (bytes == 1)
				{
					vec_add(types, llvm_get_type(context, type_byte));
				}
				else
				{
					vec_add(types, LLVMArrayType(llvm_get_type(context, type_byte), bytes));
				}
			}
			LLVMStructSetBody(type, types, vec_size(types), decl->is_packed);
			return type;
		}
		case DECL_UNION:
		{
			LLVMTypeRef type = LLVMStructCreateNamed(context->context, decl->external_name);
			// Avoid recursive issues.
			decl->type->backend_type = type;
			Decl **members = decl->strukt.members;
			if (vec_size(members))
			{

				Decl *rep_type = members[decl->strukt.union_rep];
				LLVMTypeRef type_ref[2] = {
						llvm_get_type(context, rep_type->type),
						NULL
				};
				unsigned elements = 1;
				if (decl->needs_additional_pad)
				{
					type_ref[elements++] = LLVMArrayType(llvm_get_type(context, type_bool), type_size(decl->type) - type_size(rep_type->type));

				}
				LLVMStructSetBody(type, type_ref, elements, decl->is_packed);
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
			LLVMTypeRef err_type = LLVMStructCreateNamed(context->context, decl->external_name);
			// Avoid recursive issues.
			decl->type->backend_type = err_type;
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
				vec_add(types, LLVMIntTypeInContext(context->context, padding * 8));
			}
			LLVMStructSetBody(err_type, types, vec_size(types), false);
			return err_type;
		}
	}
	UNREACHABLE
}
static inline LLVMTypeRef llvm_type_from_ptr(GenContext *context, Type *type)
{
	if (type->canonical != type)
	{
		return type->backend_type = llvm_get_type(context, type->canonical);
	}
	if (type == type_voidptr)
	{
		return type->backend_type = llvm_get_ptr_type(context, type_byte);
	}
	return type->backend_type = LLVMPointerType(llvm_get_type(context, type->pointer), /** TODO **/0);
}

static inline LLVMTypeRef llvm_type_from_array(GenContext *context, Type *type)
{
	if (type->canonical != type)
	{
		return type->backend_type = llvm_get_type(context, type->canonical);
	}

	return type->backend_type = LLVMArrayType(llvm_get_type(context, type->array.base), type->array.len);
}


static void param_expand(GenContext *context, LLVMTypeRef** params_ref, Type *type)
{
	switch (type->type_kind)
	{
		case TYPE_TYPEDEF:
			UNREACHABLE
		case TYPE_ARRAY:
			for (ByteSize i = type->array.len; i > 0; i--)
			{
				param_expand(context, params_ref, type->array.base);
			}
			return;
		case TYPE_STRUCT:
		{
			Decl **members = type->decl->strukt.members;
			VECEACH(members, i)
			{
				param_expand(context, params_ref, members[i]->type);
			}
			return;
		}
		case TYPE_ENUM:
			param_expand(context, params_ref, type_lowering(type));
			return;
		case TYPE_ERR_UNION:
			param_expand(context, params_ref, type_usize->canonical);
			param_expand(context, params_ref, type_usize->canonical);
			return;
		case TYPE_ERRTYPE:
			// TODO
			param_expand(context, params_ref, type_usize->canonical);
			return;
		case TYPE_UNION:
		{
			ByteSize largest = 0;
			Type *largest_type = NULL;
			Decl **members = type->decl->strukt.members;
			VECEACH(members, i)
			{
				if (type_size(type) > largest)
				{
					largest = type_size(type);
					type = type->canonical;
				}
			}
			if (!largest) return;
			param_expand(context, params_ref, largest_type);
			return;
		}
		default:
			// Type complex: return 2;
			vec_add(*params_ref, llvm_get_type(context, type));
			return;
	}

}

static inline void add_func_type_param(GenContext *context, Type *param_type, ABIArgInfo *arg_info, LLVMTypeRef **params)
{
	arg_info->param_index_start = vec_size(*params);
	switch (arg_info->kind)
	{
		case ABI_ARG_IGNORE:
			break;
		case ABI_ARG_INDIRECT:
			vec_add(*params, llvm_get_ptr_type(context, param_type));
			break;
		case ABI_ARG_EXPAND_COERCE:
			vec_add(*params, llvm_abi_type(context, arg_info->coerce_expand.lo));
			if (arg_info->coerce_expand.hi)
			{
				vec_add(*params, llvm_abi_type(context, arg_info->coerce_expand.hi));
			}
			break;
		case ABI_ARG_EXPAND:
			// Expanding a structs
			param_expand(context, params, param_type->canonical);
			// If we have padding, add it here.
			if (arg_info->expand.padding_type)
			{
				vec_add(*params, llvm_get_type(context, arg_info->expand.padding_type));
			}
			break;
		case ABI_ARG_DIRECT_COERCE:
		{
			// Normal direct.
			if (!arg_info->direct_coerce.type)
			{
				vec_add(*params, llvm_get_type(context, param_type));
				break;
			}
			LLVMTypeRef coerce_type = llvm_abi_type(context, arg_info->direct_coerce.type);
			if (!abi_info_should_flatten(arg_info))
			{
				vec_add(*params, coerce_type);
				break;
			}
			for (unsigned idx = 0; idx < arg_info->direct_coerce.elements; idx++)
			{
				vec_add(*params, coerce_type);
			}
			break;
		}
		case ABI_ARG_DIRECT_PAIR:
			// Pairs are passed by param.
			vec_add(*params, llvm_abi_type(context, arg_info->direct_pair.lo));
			vec_add(*params, llvm_abi_type(context, arg_info->direct_pair.hi));
			break;
	}
	arg_info->param_index_end = vec_size(*params);
}

LLVMTypeRef llvm_func_type(GenContext *context, Type *type)
{
	LLVMTypeRef *params = NULL;
	FunctionSignature *signature = type->func.signature;

	LLVMTypeRef return_type = NULL;

	Type *real_return_type = signature->failable ? type_error : signature->rtype->type->canonical;
	ABIArgInfo *ret_arg_info = signature->failable ? signature->failable_abi_info : signature->ret_abi_info;

	ret_arg_info->param_index_end = 0;
	ret_arg_info->param_index_start = 0;

	switch (ret_arg_info->kind)
	{
		case ABI_ARG_EXPAND:
			UNREACHABLE;
		case ABI_ARG_INDIRECT:
			vec_add(params, llvm_get_ptr_type(context, real_return_type));
			FALLTHROUGH;
		case ABI_ARG_EXPAND_COERCE:
		{
			LLVMTypeRef lo = llvm_abi_type(context, ret_arg_info->direct_pair.lo);
			if (!ret_arg_info->direct_pair.hi)
			{
				return_type = lo;
				break;
			}
			LLVMTypeRef hi = llvm_abi_type(context, ret_arg_info->direct_pair.hi);
			return_type = llvm_get_twostruct(context, lo, hi);
			break;
		}
		case ABI_ARG_IGNORE:
			return_type = llvm_get_type(context, type_void);
			break;
		case ABI_ARG_DIRECT_PAIR:
		{
			LLVMTypeRef lo = llvm_abi_type(context, ret_arg_info->direct_pair.lo);
			LLVMTypeRef hi = llvm_abi_type(context, ret_arg_info->direct_pair.hi);
			return_type = llvm_get_twostruct(context, lo, hi);
			break;
		}
		case ABI_ARG_DIRECT_COERCE:
			assert(!abi_info_should_flatten(ret_arg_info));
			return_type = llvm_get_coerce_type(context, ret_arg_info) ?: llvm_get_type(context, real_return_type);
			break;
	}

	// If it's failable and it's not void (meaning ret_abi_info will be NULL)
	if (signature->failable && signature->ret_abi_info)
	{
		add_func_type_param(context, type_get_ptr(signature->rtype->type), signature->ret_abi_info, &params);
	}

	// Add in all of the required arguments.
	VECEACH(signature->params, i)
	{
		add_func_type_param(context, signature->params[i]->type, signature->params[i]->var.abi_info, &params);
	}

	return LLVMFunctionType(return_type, params, vec_size(params), signature->variadic);
}



LLVMTypeRef llvm_get_type(GenContext *c, Type *any_type)
{
	if (any_type->backend_type && LLVMGetTypeContext(any_type->backend_type) == c->context)
	{
		return any_type->backend_type;
	}
	DEBUG_LOG("Generating type %s", any_type->name);
	switch (any_type->type_kind)
	{
		case TYPE_POISONED:
		case TYPE_TYPEINFO:
		case TYPE_MEMBER:
			UNREACHABLE
		case TYPE_TYPEID:
			return any_type->backend_type = LLVMIntTypeInContext(c->context, any_type->builtin.bitsize);
		case TYPE_TYPEDEF:
			return any_type->backend_type = llvm_get_type(c, any_type->canonical);
		case TYPE_ENUM:
			return any_type->backend_type = llvm_get_type(c, any_type->decl->enums.type_info->type->canonical);
		case TYPE_ERR_UNION:
		{
			LLVMTypeRef elements[2] = { llvm_get_type(c, type_usize->canonical), llvm_get_type(c, type_usize->canonical) };
			return any_type->backend_type = LLVMStructTypeInContext(c->context, elements, 2, false);
		}
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_ERRTYPE:
			return any_type->backend_type = llvm_type_from_decl(c, any_type->decl);
		case TYPE_FUNC:
			return any_type->backend_type = llvm_func_type(c, any_type);
		case TYPE_VOID:
			return any_type->backend_type = LLVMVoidTypeInContext(c->context);
		case TYPE_F64:
		case TYPE_FXX:
			return any_type->backend_type = LLVMDoubleTypeInContext(c->context);
		case TYPE_F16:
			return any_type->backend_type = LLVMHalfTypeInContext(c->context);
		case TYPE_F32:
			return any_type->backend_type = LLVMFloatTypeInContext(c->context);
		case TYPE_F128:
			return any_type->backend_type = LLVMFP128TypeInContext(c->context);
		case ALL_SIGNED_INTS:
		case ALL_UNSIGNED_INTS:
			return any_type->backend_type = LLVMIntTypeInContext(c->context, any_type->builtin.bitsize);
		case TYPE_IXX:
			return any_type->backend_type = LLVMIntTypeInContext(c->context, 32U);
		case TYPE_BOOL:
			return any_type->backend_type = LLVMIntTypeInContext(c->context, 8U);
		case TYPE_POINTER:
			return any_type->backend_type = llvm_type_from_ptr(c, any_type);
		case TYPE_STRING:
			// TODO
			return any_type->backend_type = LLVMPointerType(llvm_get_type(c, type_char), 0);
		case TYPE_ARRAY:
			return any_type->backend_type = llvm_type_from_array(c, any_type);
		case TYPE_SUBARRAY:
		{
			LLVMTypeRef base_type = llvm_get_type(c, type_get_ptr(any_type->array.base));
			LLVMTypeRef size_type = llvm_get_type(c, type_usize);
			LLVMTypeRef array_type = LLVMStructCreateNamed(c->context, any_type->name);
			LLVMTypeRef types[2] = { base_type, size_type };
			LLVMStructSetBody(array_type, types, 2, false);
			return any_type->backend_type = array_type;
		}
		case TYPE_VARARRAY:
			return any_type->backend_type = llvm_get_type(c, type_get_ptr(any_type->array.base));
		case TYPE_VECTOR:
			return any_type->backend_type = LLVMVectorType(llvm_get_type(c, any_type->vector.base), any_type->vector.len);
		case TYPE_COMPLEX:
			return any_type->backend_type = llvm_get_twostruct(c,
			                                                   llvm_get_type(c, any_type->complex),
			                                                   llvm_get_type(c, any_type->complex));
	}
	UNREACHABLE;
}


LLVMTypeRef llvm_get_coerce_type(GenContext *c, ABIArgInfo *arg_info)
{
	if (arg_info->kind == ABI_ARG_EXPAND_COERCE)
	{
		unsigned element_index = 0;
		LLVMTypeRef elements[4];
		// Add padding if needed.
		if (arg_info->coerce_expand.offset_lo)
		{
			elements[element_index++] = LLVMArrayType(llvm_get_type(c, type_byte), arg_info->coerce_expand.offset_lo);
		}
		elements[element_index++] = llvm_abi_type(c, arg_info->coerce_expand.lo);
		if (arg_info->coerce_expand.padding_hi)
		{
			elements[element_index++] = LLVMArrayType(llvm_get_type(c, type_byte), arg_info->coerce_expand.padding_hi);
		}
		if (arg_info->coerce_expand.hi)
		{
			elements[element_index++] = llvm_abi_type(c, arg_info->coerce_expand.hi);
		}
		return LLVMStructType(elements, element_index, arg_info->coerce_expand.packed);
	}

	if (arg_info->kind == ABI_ARG_DIRECT_COERCE)
	{
		if (!arg_info->direct_coerce.type) return NULL;
		LLVMTypeRef coerce_type = llvm_abi_type(c, arg_info->direct_coerce.type);
		if (arg_info->direct_coerce.elements < 2U) return coerce_type;
		LLVMTypeRef *refs = MALLOC(sizeof(LLVMValueRef) * arg_info->direct_coerce.elements);
		for (unsigned i = 0; i < arg_info->direct_coerce.elements; i++)
		{
			refs[i] = coerce_type;
		}
		return LLVMStructTypeInContext(c->context, refs, arg_info->direct_coerce.elements, false);
	}
	if (arg_info->kind == ABI_ARG_DIRECT_PAIR)
	{
		LLVMTypeRef lo = llvm_abi_type(c, arg_info->direct_pair.lo);
		LLVMTypeRef hi = llvm_abi_type(c, arg_info->direct_pair.hi);
		return llvm_get_twostruct(c, lo, hi);
	}
	UNREACHABLE
}

LLVMTypeRef llvm_get_twostruct(GenContext *context, LLVMTypeRef lo, LLVMTypeRef hi)
{
	LLVMTypeRef types[2] = { lo, hi };
	return LLVMStructTypeInContext(context->context, types, 2, false);
}

LLVMTypeRef llvm_abi_type(GenContext *c, AbiType *type)
{
	switch (type->kind)
	{
		case ABI_TYPE_PLAIN:
			return llvm_get_type(c, type->type);
		case ABI_TYPE_INT_BITS:
			return LLVMIntTypeInContext(c->context, type->int_bits);
	}
	UNREACHABLE
}
