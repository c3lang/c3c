// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_internal.h"


static inline LLVMTypeRef llvm_type_from_decl(GenContext *c, Decl *decl)
{
	static LLVMTypeRef params[MAX_PARAMS];
	switch (decl->decl_kind)
	{
		case DECL_VAR:
		case DECL_ENUM_CONSTANT:
		case DECL_ERRVALUE:
		case DECL_POISONED:
		case NON_TYPE_DECLS:
			UNREACHABLE
		case DECL_BITSTRUCT:
			return llvm_get_type(c, decl->bitstruct.base_type->type);
		case DECL_FUNC:
		{
			VECEACH(decl->func_decl.function_signature.params, i)
			{
				params[i] = llvm_get_type(c, decl->func_decl.function_signature.params[i]->type);
			}
			unsigned param_size = vec_size(decl->func_decl.function_signature.params);
			return LLVMFunctionType(llvm_get_type(c, decl->func_decl.function_signature.rtype->type),
			                        params,
			                        param_size,
			                        decl->func_decl.function_signature.variadic == VARIADIC_RAW);

		}
		case DECL_TYPEDEF:
			return llvm_get_type(c, decl->typedef_decl.type_info->type);
		case DECL_DISTINCT:
			return llvm_get_type(c, decl->distinct_decl.base_type);
		case DECL_STRUCT:
		{
			LLVMTypeRef *types = NULL;
			LLVMTypeRef type = LLVMStructCreateNamed(c->context, decl->name ? decl->name : "anon");
			// Avoid recursive issues.
			decl->type->backend_type = type;
			Decl **members = decl->strukt.members;
			VECEACH(members, i)
			{
				Decl *member = members[i];
				if (member->padding)
				{
					vec_add(types, llvm_const_padding_type(c, member->padding));
				}
				vec_add(types, llvm_get_type(c, members[i]->type));
			}
			if (decl->strukt.padding)
			{
				vec_add(types, llvm_const_padding_type(c, decl->strukt.padding));
			}
			LLVMStructSetBody(type, types, vec_size(types), decl->is_packed);
			return type;
		}
		case DECL_UNION:
		{
			LLVMTypeRef type = LLVMStructCreateNamed(c->context, decl->name ? decl->name : "anon");
			// Avoid recursive issues.
			decl->type->backend_type = type;
			Decl **members = decl->strukt.members;
			if (vec_size(members))
			{

				Decl *rep_type = members[decl->strukt.union_rep];
				LLVMTypeRef type_ref[2] = {
						llvm_get_type(c, rep_type->type),
						NULL
				};
				unsigned elements = 1;
				if (decl->strukt.padding)
				{
					type_ref[elements++] = llvm_const_padding_type(c, decl->strukt.padding);
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
			return llvm_get_type(c, decl->type);
		case DECL_ERRTYPE:
			return llvm_get_type(c, type_iptr);
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
		return type->backend_type = llvm_get_ptr_type(context, type_char);
	}
	return type->backend_type = LLVMPointerType(llvm_get_type(context, type->pointer), /** TODO **/0);
}


static inline LLVMTypeRef llvm_type_from_array(GenContext *context, Type *type)
{
	if (type->canonical != type)
	{
		return type->backend_type = llvm_get_type(context, type->canonical);
	}

	return type->backend_type = LLVMArrayType(llvm_get_type(context, type->array.base), (unsigned)type->array.len);
}


static void param_expand(GenContext *context, LLVMTypeRef** params_ref, Type *type)
{
	switch (type->type_kind)
	{
		case TYPE_TYPEDEF:
			UNREACHABLE
		case TYPE_ARRAY:
			for (ArraySize i = type->array.len; i > 0; i--)
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
		case TYPE_ANYERR:
		case TYPE_ERRTYPE:
			param_expand(context, params_ref, type_lowering(type));
			return;
		case TYPE_UNION:
		{
			ByteSize largest = 0;
			Type *largest_type = NULL;
			Decl **members = type->decl->strukt.members;
			// Clang: Unions can be here only in degenerative cases - all the fields are same
			// after flattening. Thus we have to use the "largest" field.
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
	arg_info->param_index_start = (MemberIndex)vec_size(*params);
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
	arg_info->param_index_end = (MemberIndex)vec_size(*params);
}

LLVMTypeRef llvm_func_type(GenContext *context, Type *type)
{
	LLVMTypeRef *params = NULL;
	FunctionSignature *signature = type->func.signature;

	LLVMTypeRef return_type = NULL;

	Type *rtype = signature->rtype->type;
	bool is_failable = type_is_failable(rtype);
	if (is_failable) rtype = rtype->failable;
	Type *real_return_type = is_failable ? type_anyerr : rtype;
	ABIArgInfo *ret_arg_info = is_failable ? signature->failable_abi_info : signature->ret_abi_info;

	ret_arg_info->param_index_end = 0;
	ret_arg_info->param_index_start = 0;

	switch (ret_arg_info->kind)
	{
		case ABI_ARG_EXPAND:
			UNREACHABLE;
		case ABI_ARG_INDIRECT:
			vec_add(params, llvm_get_ptr_type(context, real_return_type));
			return_type = llvm_get_type(context, type_void);
			break;
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
			return_type = llvm_get_coerce_type(context, ret_arg_info);
			if (!return_type) return_type = llvm_get_type(context, real_return_type);
			break;
	}

	// If it's failable and it's not void (meaning ret_abi_info will be NULL)
	if (is_failable && signature->ret_abi_info)
	{
		add_func_type_param(context, type_get_ptr(rtype), signature->ret_abi_info, &params);
	}

	// Add in all of the required arguments.
	VECEACH(signature->params, i)
	{
		add_func_type_param(context, signature->params[i]->type, signature->params[i]->var.abi_info, &params);
	}

	return LLVMFunctionType(return_type, params, vec_size(params), signature->variadic == VARIADIC_RAW);
}


LLVMTypeRef llvm_get_pointee_type(GenContext *c, Type *any_type)
{
	any_type = any_type->canonical;
	assert(any_type->type_kind == TYPE_POINTER);
	if (any_type == type_voidptr) return llvm_get_type(c, type_char);
	return llvm_get_type(c, any_type->pointer);
}

LLVMTypeRef llvm_get_type(GenContext *c, Type *any_type)
{
	if (any_type->backend_type)
	{
		assert(LLVMGetTypeContext(any_type->backend_type) == c->context && "Should have been purged");
		return any_type->backend_type;
	}
	switch (any_type->type_kind)
	{
		case CT_TYPES:
			UNREACHABLE
		case TYPE_FAILABLE:
		case TYPE_FAILABLE_ANY:
			// If this is reachable, then we're not doing the proper lowering.
			UNREACHABLE
		case TYPE_TYPEID:
		case TYPE_ANYERR:
		case TYPE_ERRTYPE:
			return any_type->backend_type = llvm_get_type(c, type_iptr->canonical);
		case TYPE_TYPEDEF:
			return any_type->backend_type = llvm_get_type(c, any_type->canonical);
		case TYPE_DISTINCT:
			return any_type->backend_type = llvm_get_type(c, any_type->decl->distinct_decl.base_type);
		case TYPE_ENUM:
			return any_type->backend_type = llvm_get_type(c, any_type->decl->enums.type_info->type->canonical);
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_BITSTRUCT:
			return any_type->backend_type = llvm_type_from_decl(c, any_type->decl);
		case TYPE_FUNC:
			return any_type->backend_type = llvm_func_type(c, any_type);
		case TYPE_VOID:
			return any_type->backend_type = LLVMVoidTypeInContext(c->context);
		case TYPE_F64:
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
		case TYPE_BOOL:
			return any_type->backend_type = LLVMIntTypeInContext(c->context, 8U);
		case TYPE_POINTER:
			return any_type->backend_type = llvm_type_from_ptr(c, any_type);
		case TYPE_ARRAY:
		case TYPE_FLEXIBLE_ARRAY:
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
		case TYPE_ANY:
		{
			LLVMTypeRef pointer_type = llvm_get_type(c, type_voidptr);
			LLVMTypeRef type_type = llvm_get_type(c, type_typeid);
			LLVMTypeRef virtual_type = LLVMStructCreateNamed(c->context, any_type->name);
			LLVMTypeRef types[2] = { pointer_type, type_type };
			LLVMStructSetBody(virtual_type, types, 2, false);
			return any_type->backend_type = virtual_type;
		}
		case TYPE_VECTOR:
			return any_type->backend_type = LLVMVectorType(llvm_get_type(c, any_type->vector.base), any_type->vector.len);
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
			elements[element_index++] = LLVMArrayType(llvm_get_type(c, type_char), arg_info->coerce_expand.offset_lo);
		}
		elements[element_index++] = llvm_abi_type(c, arg_info->coerce_expand.lo);
		if (arg_info->coerce_expand.padding_hi)
		{
			elements[element_index++] = LLVMArrayType(llvm_get_type(c, type_char), arg_info->coerce_expand.padding_hi);
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
