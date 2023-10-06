// Copyright (c) 2022 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "tilde_internal.h"

TB_CallingConv calling_conv_from(CallABI abi)
{
	switch (abi)
	{
		case CALL_X86_STD:
			return TB_STDCALL;
		case CALL_C:
			return TB_CDECL;
		case CALL_X86_FAST:
		case CALL_X86_THIS:
		case CALL_X86_VECTOR:
		case CALL_X86_REG:
		case CALL_AAPCS:
		case CALL_AAPCS_VFP:
			REMINDER("Using C decl even though actual calling convention is different.");
			return TB_CDECL;
	}
}

TB_DebugType *tilde_get_debug_type(Type *type)
{
	return NULL;
}

TB_FunctionPrototype *tilde_get_func_prototype(TildeContext *c, FunctionPrototype *prototype)
{
	if (prototype->tb_prototype) return prototype->tb_prototype;
	int actual_arg_count = 0;
	ABIArgInfo **abi_args = prototype->abi_args;
	for (unsigned i = vec_size(prototype->param_types); i > 0; i--)
	{
		ABIArgInfo *info = abi_args[i - 1];
		switch (info->kind)
		{
			case ABI_ARG_IGNORE:
				continue;
			case ABI_ARG_INDIRECT:
			case ABI_ARG_DIRECT:
				actual_arg_count++;
				break;
			case ABI_ARG_DIRECT_PAIR:
				actual_arg_count += 2;
			default:
				TODO
		}
	}
	TB_FunctionPrototype *proto =
			tb_prototype_create(c->module,
			                    calling_conv_from(prototype->call_abi),
			                    tildetype(prototype->abi_ret_type),
			                    tilde_get_debug_type(prototype->rtype),
			                    actual_arg_count,
			                    prototype->variadic == VARIADIC_RAW);
	FOREACH_BEGIN_IDX(i, Decl *param, prototype->param_copy)
		ABIArgInfo *abi_info = prototype->abi_args[i];
		switch (abi_info->kind)
		{
			case ABI_ARG_DIRECT:
				if (param->name)
				{
					tb_prototype_add_param_named(proto, tildetype(param->type), param->name, NULL);
				}
				else
				{
					tb_prototype_add_param(proto, tildetype(param->type));
				}
				break;
			case ABI_ARG_IGNORE:
				continue;
			case ABI_ARG_INDIRECT:
				tb_prototype_add_param(proto, TB_TYPE_PTR);
				break;
			default:
				TODO
		}
	FOREACH_END();
	prototype->tb_prototype = proto;
	return proto;
}

TB_DataType tilde_abi_type(AbiType type)
{
	if (abi_type_is_type(type)) return tildetype(type.type);
	return tilde_get_int_type_of_bytesize((type.int_bits_plus_1 - 1) / 8);
}

static void param_expand(TildeContext *context, TB_DataType** params_ref, Type *type)
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
		case TYPE_ANYFAULT:
		case TYPE_FAULTTYPE:
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
			vec_add(*params_ref, tildetype(type));
			return;
	}

}

static inline void add_func_type_param(TildeContext *context, Type *param_type, ABIArgInfo *arg_info, TB_DataType **params)
{
	arg_info->param_index_start = (MemberIndex)vec_size(*params);
	switch (arg_info->kind)
	{
		case ABI_ARG_IGNORE:
			break;
		case ABI_ARG_INDIRECT:
			vec_add(*params, TB_TYPE_PTR);
			break;
		case ABI_ARG_EXPAND_COERCE:
			vec_add(*params, tilde_abi_type(arg_info->coerce_expand.lo));
			if (abi_type_is_valid(arg_info->coerce_expand.hi))
			{
				vec_add(*params, tilde_abi_type(arg_info->coerce_expand.hi));
			}
			break;
		case ABI_ARG_EXPAND:
			// Expanding a structs
			param_expand(context, params, param_type->canonical);
			// If we have padding, add it here.
			if (arg_info->expand.padding_type)
			{
				vec_add(*params, tildetype(arg_info->expand.padding_type));
			}
			break;
		case ABI_ARG_DIRECT:
			vec_add(*params, tildetype(param_type));
			break;
		case ABI_ARG_DIRECT_SPLIT_STRUCT:
		{
			// Normal direct.
			TB_DataType coerce_type = tildetype(arg_info->direct_struct_expand.type);
			for (unsigned idx = 0; idx < arg_info->direct_struct_expand.elements; idx++)
			{
				vec_add(*params, coerce_type);
			}
			break;
		}
		case ABI_ARG_DIRECT_COERCE_INT:
		{
			// Normal direct.
			TB_DataType coerce_type = tilde_get_int_type_of_bytesize(type_size(param_type));
			vec_add(*params, coerce_type);
			break;
		}
		case ABI_ARG_DIRECT_COERCE:
		{
			// Normal direct.
			TB_DataType coerce_type = tildetype(arg_info->direct_coerce_type);
			vec_add(*params, coerce_type);
			break;
		}
		case ABI_ARG_DIRECT_PAIR:
			// Pairs are passed by param.
			vec_add(*params, tilde_abi_type(arg_info->direct_pair.lo));
			vec_add(*params, tilde_abi_type(arg_info->direct_pair.hi));
			break;
	}
	arg_info->param_index_end = (MemberIndex)vec_size(*params);
}

TB_DataType tilde_update_prototype_abi(TildeContext *context, FunctionPrototype *prototype, TB_DataType **params)
{
	TB_DataType retval;
	Type *call_return_type = prototype->abi_ret_type;
	ABIArgInfo *ret_arg_info = prototype->ret_abi_info;

	ret_arg_info->param_index_end = 0;
	ret_arg_info->param_index_start = 0;

	switch (ret_arg_info->kind)
	{
		case ABI_ARG_EXPAND:
			UNREACHABLE;
		case ABI_ARG_INDIRECT:
			vec_add(*params, TB_TYPE_PTR);
			retval = TB_TYPE_VOID;
			break;
		case ABI_ARG_EXPAND_COERCE:
		{
			TB_DataType lo = tilde_abi_type(ret_arg_info->direct_pair.lo);
			if (!abi_type_is_valid(ret_arg_info->direct_pair.hi))
			{
				retval = lo;
				break;
			}
			TB_DataType hi = tilde_abi_type(ret_arg_info->direct_pair.hi);
			TODO // retval = llvm_get_twostruct(context, lo, hi);
			break;
		}
		case ABI_ARG_IGNORE:
			retval = TB_TYPE_VOID;
			break;
		case ABI_ARG_DIRECT_PAIR:
		{
			TODO /*---
			LLVMTypeRef lo = llvm_abi_type(context, ret_arg_info->direct_pair.lo);
			LLVMTypeRef hi = llvm_abi_type(context, ret_arg_info->direct_pair.hi);
			retval = llvm_get_twostruct(context, lo, hi);*/
			break;
		}
		case ABI_ARG_DIRECT:
			retval = tildetype(call_return_type);
			break;
		case ABI_ARG_DIRECT_SPLIT_STRUCT:
			UNREACHABLE
		case ABI_ARG_DIRECT_COERCE_INT:
			retval = tilde_get_int_type_of_bytesize(type_size(call_return_type));
			break;
		case ABI_ARG_DIRECT_COERCE:
			retval = tildetype(ret_arg_info->direct_coerce_type);
			break;
	}

	// If it's optional and it's not void (meaning ret_abi_info will be NULL)
	if (prototype->ret_by_ref)
	{
		add_func_type_param(context, type_get_ptr(type_lowering(prototype->ret_by_ref_type)), prototype->ret_by_ref_abi_info, params);
	}

	// Add in all of the required arguments.
	VECEACH(prototype->param_types, i)
	{
		add_func_type_param(context, prototype->param_types[i], prototype->abi_args[i], params);
	}

	VECEACH(prototype->varargs, i)
	{
		add_func_type_param(context, prototype->varargs[i], prototype->abi_varargs[i], params);
	}
	return retval;
}

TB_Global *tilde_get_typeid(TildeContext *c, Type *type)
{
	return NULL;
	/*
	if (type->backend_typeid) return type->backend_typeid;

	switch (type->type_kind)
	{
		case TYPE_OPTIONAL:
			return tilde_generate_introspection_global(c, NULL, type, INTROSPECT_TYPE_OPTIONAL, type->optional, 0, NULL, false);
		case TYPE_FLEXIBLE_ARRAY:
			return tilde_generate_introspection_global(c, NULL, type, INTROSPECT_TYPE_ARRAY, type->array.base, 0, NULL, false);
		case TYPE_VECTOR:
			return tilde_generate_introspection_global(c, NULL, type, INTROSPECT_TYPE_VECTOR, type->array.base, type->array.len, NULL, false);
		case TYPE_ARRAY:
			return tilde_generate_introspection_global(c, NULL, type, INTROSPECT_TYPE_ARRAY, type->array.base, type->array.len, NULL, false);
		case TYPE_SUBARRAY:
			return tilde_generate_introspection_global(c, NULL, type, INTROSPECT_TYPE_SUBARRAY, type->array.base, 0, NULL, false);
		case TYPE_POINTER:
			return tilde_generate_introspection_global(c, NULL, type, INTROSPECT_TYPE_POINTER, type->pointer, 0, NULL, false);
		case TYPE_DISTINCT:
			return tilde_generate_introspection_global(c, NULL, type, INTROSPECT_TYPE_DISTINCT, type->decl->distinct->type, 0, NULL, false);
		case TYPE_ENUM:
			return tilde_generate_introspection_global(c, type);
		case TYPE_FAULTTYPE:
			return tilde_generate_introspection_global(c, type);
		case TYPE_STRUCT:
		case TYPE_UNION:
			return tilde_generate_introspection_global(c, type);
		case TYPE_FUNC:
			if (type->function.prototype->raw_type == type)
			{
				LLVMValueRef ref = llvm_generate_temp_introspection_global(c, type);
				return tilde_generate_introspection_global(c, ref, type, INTROSPECT_TYPE_FUNC, NULL, 0, NULL, false);
			}
			return llvm_get_typeid(c, type->function.prototype->raw_type);
		case TYPE_BITSTRUCT:
		{
			LLVMValueRef ref = llvm_generate_temp_introspection_global(c, type);
			return tilde_generate_introspection_global(c, ref, type, INTROSPECT_TYPE_BITSTRUCT, NULL, 0, NULL, false);
		}
		case TYPE_TYPEDEF:
			return tilde_get_typeid(c, type->canonical);
		case TYPE_INFERRED_ARRAY:
		case TYPE_INFERRED_VECTOR:
		case TYPE_UNTYPED_LIST:
		case TYPE_OPTIONAL_ANY:
		case TYPE_TYPEINFO:
		case TYPE_MEMBER:
			UNREACHABLE
		case TYPE_VOID:
			return tilde_get_introspection_for_builtin_type(c, type, INTROSPECT_TYPE_VOID, 0);
		case TYPE_BOOL:
			return tilde_get_introspection_for_builtin_type(c, type, INTROSPECT_TYPE_BOOL, 0);
		case ALL_SIGNED_INTS:
			return tilde_get_introspection_for_builtin_type(c, type, INTROSPECT_TYPE_SIGNED_INT,
			                                               type_kind_bitsize(type->type_kind));
		case ALL_UNSIGNED_INTS:
			return tilde_get_introspection_for_builtin_type(c,
			                                               type,
			                                               INTROSPECT_TYPE_UNSIGNED_INT,
			                                               type_kind_bitsize(type->type_kind));
		case ALL_FLOATS:
			return tilde_get_introspection_for_builtin_type(c,
			                                               type,
			                                               INTROSPECT_TYPE_FLOAT,
			                                               type_kind_bitsize(type->type_kind));
		case TYPE_ANYERR:
			return tilde_get_introspection_for_builtin_type(c, type, INTROSPECT_TYPE_ANYERR, 0);
		case TYPE_ANY:
			return tilde_get_introspection_for_builtin_type(c, type, INTROSPECT_TYPE_ANY, 0);
		case TYPE_TYPEID:
			return tilde_get_introspection_for_builtin_type(c, type, INTROSPECT_TYPE_TYPEID, 0);
		case TYPE_POISONED:
			UNREACHABLE
		case TYPE_SCALED_VECTOR:
			TODO
	}
	UNREACHABLE*/
}

void tilde_emit_function_decl(TildeContext *c, Decl *decl)
{
	assert(decl->decl_kind == DECL_FUNC);
	// Resolve function backend type for function.

	TB_Function *fn = tilde_get_function(c, decl);
	(void)fn;
///	FunctionPrototype *prototype = decl->type->function.prototype;

	/*
	ABIArgInfo *ret_abi_info = prototype->ret_abi_info;
	llvm_emit_param_attributes(c, function, ret_abi_info, true, 0, 0);
	unsigned params = vec_size(prototype->param_types);
	if (prototype->ret_by_ref)
	{
		ABIArgInfo *info = prototype->ret_by_ref_abi_info;
		llvm_emit_param_attributes(c, function, prototype->ret_by_ref_abi_info, false, info->param_index_start + 1, info->param_index_end);
	}
	for (unsigned i = 0; i < params; i++)
	{
		ABIArgInfo *info = prototype->abi_args[i];
		llvm_emit_param_attributes(c, function, info, false, info->param_index_start + 1, info->param_index_end);
	}
	// We ignore decl->func_decl.attr_inline and place it in every call instead.
	if (decl->func_decl.attr_noinline)
	{
		llvm_attribute_add(c, function, attribute_id.noinline, -1);
	}
	if (decl->func_decl.signature.attrs.noreturn)
	{
		llvm_attribute_add(c, function, attribute_id.noreturn, -1);
	}
	if (decl->alignment != type_abi_alignment(decl->type))
	{
		llvm_set_alignment(function, decl->alignment);
	}
	if (decl->section)
	{
		LLVMSetSection(function, decl->section);
	}
	llvm_attribute_add(c, function, attribute_id.nounwind, -1);
	if (decl->func_decl.attr_naked)
	{
		llvm_attribute_add(c, function, attribute_id.naked, -1);
	}
	LLVMSetFunctionCallConv(function, llvm_call_convention_from_call(prototype->call_abi));

	Visibility visibility = decl->visibility;
	if (decl->is_external_visible) visibility = VISIBLE_PUBLIC;
	switch (visibility)
	{
		case VISIBLE_EXTERN:
			if (decl->is_weak)
			{
				LLVMSetLinkage(function, LLVMExternalWeakLinkage);
				llvm_set_comdat(c, function);
			}
			else
			{
				LLVMSetLinkage(function, LLVMExternalLinkage);
			}
			LLVMSetVisibility(function, LLVMDefaultVisibility);
			if (prototype->call_abi == CALL_X86_STD && platform_target.os == OS_TYPE_WIN32)
			{
				LLVMSetDLLStorageClass(function, LLVMDLLImportStorageClass);
			}
			break;
		case VISIBLE_PUBLIC:
		case VISIBLE_MODULE:
			if (decl->is_weak) llvm_set_weak(c, function);
			break;
		case VISIBLE_LOCAL:
			LLVMSetLinkage(function, decl->is_weak ? LLVMLinkerPrivateWeakLinkage : LLVMInternalLinkage);
			LLVMSetVisibility(function, LLVMDefaultVisibility);
			break;;
	}
	if (llvm_use_debug(c))
	{
		llvm_emit_debug_function(c, decl);
	}*/
}

TB_DataType tildetype(Type *type)
{
	type = type_lowering(type);
	if (type->tb_set) return (TB_DataType) { .raw = type->tb_type };
	TB_DataType tb_type;
	switch (type->type_kind)
	{
		case TYPE_TYPEID:
		case TYPE_ANYFAULT:
			UNREACHABLE;
		case TYPE_FUNC:
			TODO
		case TYPE_VECTOR:
			tb_type = tildetype(type->array.base);
			tb_type.width = next_highest_power_of_2(type->array.len);
			break;
		case TYPE_F32:
			tb_type = TB_TYPE_F32;
			break;
		case TYPE_F64:
			tb_type = TB_TYPE_F64;
			break;
		case TYPE_VOID:
			tb_type = TB_TYPE_VOID;
			break;
		case TYPE_I8:
		case TYPE_U8:
			tb_type = TB_TYPE_I8;
			break;
		case TYPE_I16:
		case TYPE_U16:
			tb_type = TB_TYPE_I16;
			break;
		case TYPE_I32:
		case TYPE_U32:
			tb_type = TB_TYPE_I32;
			break;
		case TYPE_I64:
		case TYPE_U64:
			tb_type = TB_TYPE_I64;
			break;
		case TYPE_I128:
		case TYPE_U128:
			tb_type = (TB_DataType) { { TB_INT, 0, 128 } };
			break;
		case TYPE_POINTER:
			tb_type = TB_TYPE_PTR;
			break;
		case TYPE_BOOL:
			tb_type = TB_TYPE_BOOL;
			break;
		case TYPE_ANY:
		default:
			TODO
	}
	type->tb_set = 1;
	type->tb_type = tb_type.raw;
	return tb_type;
}
