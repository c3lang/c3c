// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler/c_abi_internal.h"


static ABIArgInfo *abi_arg_new(ABIKind kind, ParamInfo param)
{
	ABIArgInfo *info = CALLOCS(ABIArgInfo);
	info->kind = kind;
	info->original_type = param.type;
	info->rewrite = param.rewrite;
	return info;
}

ABIArgInfo *abi_arg_ignore(void)
{
	static ABIArgInfo info = { .kind = ABI_ARG_IGNORE };
	return &info;
}

bool abi_type_is_integer(AbiType type)
{
	return !abi_type_is_type(type) || type_is_integer(type.type);
}

bool abi_type_is_float(AbiType type)
{
	return abi_type_is_type(type) && type_is_float(type.type);
}

TypeSize abi_type_size(AbiType type)
{
	if (abi_type_is_type(type)) return type_size(type.type);
	switch (type.abi_type)
	{
		case ABI_TYPE_INT_24:         return 3;
		case ABI_TYPE_INT_40:         return 5;
		case ABI_TYPE_INT_48:         return 6;
		case ABI_TYPE_INT_56:         return 7;
		case ABI_TYPE_INT_VEC_2:      return 8;
		case ABI_TYPE_FLOAT_VEC_2:    return 8;
		case ABI_TYPE_FLOAT16_VEC_2:  return 4;
		case ABI_TYPE_FLOAT16_VEC_4:  return 8;
		case ABI_TYPE_BFLOAT16_VEC_2: return 4;
		case ABI_TYPE_BFLOAT16_VEC_4: return 8;
		case ABI_TYPE_INT_VEC_4:      return 16;
		case ABI_TYPE_FLOAT_VEC_4:    return 16;
		case ABI_TYPE_DOUBLE_VEC_2:   return 16;
		case ABI_TYPE_LONG_VEC_2:     return 16;
		case ABI_TYPE_DOUBLE_VEC_4:   return 32;
		case ABI_TYPE_DOUBLE_VEC_8:   return 64;
	}
	UNREACHABLE
}

AlignSize abi_type_abi_alignment(AbiType type)
{
	if (abi_type_is_type(type)) return type_abi_alignment(type.type);
	switch (type.abi_type)
	{
		case ABI_TYPE_FLOAT16_VEC_2:
		case ABI_TYPE_BFLOAT16_VEC_2:
		case ABI_TYPE_INT_24:       return 4;
		case ABI_TYPE_INT_40:
		case ABI_TYPE_INT_48:
		case ABI_TYPE_INT_56:
		case ABI_TYPE_FLOAT16_VEC_4:
		case ABI_TYPE_BFLOAT16_VEC_4:
		case ABI_TYPE_FLOAT_VEC_2:
		case ABI_TYPE_INT_VEC_2:    return 8;
		case ABI_TYPE_FLOAT_VEC_4:
		case ABI_TYPE_LONG_VEC_2:
		case ABI_TYPE_DOUBLE_VEC_2:
		case ABI_TYPE_INT_VEC_4:    return 16;
		case ABI_TYPE_DOUBLE_VEC_4: return 32;
		case ABI_TYPE_DOUBLE_VEC_8: return 64;
	}
	UNREACHABLE;
}

bool abi_arg_is_indirect(ABIArgInfo *info)
{
	switch (info->kind)
	{
		case ABI_ARG_IGNORE:
		case ABI_ARG_DIRECT:
		case ABI_ARG_DIRECT_COERCE:
		case ABI_ARG_DIRECT_COERCE_INT:
		case ABI_ARG_DIRECT_SPLIT_STRUCT_I32:
		case ABI_ARG_EXPAND:
		case ABI_ARG_DIRECT_PAIR:
		case ABI_ARG_EXPAND_COERCE:
			return false;
		case ABI_ARG_INDIRECT:
			return true;
	}
	UNREACHABLE
}

ABIArgInfo *abi_arg_new_indirect_realigned(AlignSize alignment, Type *by_val_type, ParamInfo param)
{
	ASSERT(alignment > 0);
	ABIArgInfo *info = abi_arg_new(ABI_ARG_INDIRECT, param);
	info->indirect.alignment = alignment;
	ASSERT(info->indirect.alignment);
	info->attributes.realign = true;
	info->indirect.type = by_val_type;
	info->attributes.by_val = true;
	return info;
}

ABIArgInfo *abi_arg_new_indirect_by_val(Type *by_val_type, ParamInfo param)
{
	ABIArgInfo *info = abi_arg_new(ABI_ARG_INDIRECT, param);
	info->indirect.alignment = type_abi_alignment(by_val_type);
	info->indirect.type = by_val_type;
	info->attributes.by_val = true;
	ASSERT(info->indirect.alignment);
	return info;
}

ABIArgInfo *abi_arg_new_indirect_not_by_val(Type *type, ParamInfo param)
{
	ABIArgInfo *info = abi_arg_new(ABI_ARG_INDIRECT, param);
	info->indirect.alignment = type_abi_alignment(type);
	ASSERT(info->indirect.alignment);
	info->indirect.type = type;
	info->attributes.by_val = false;
	return info;
}

ABIArgInfo *abi_arg_new_direct_int_ext(Type *int_to_extend, ParamInfo param)
{
	return abi_arg_new_direct_int_ext_by_reg(int_to_extend, false, param);
}

ABIArgInfo *abi_arg_new_direct_coerce_int_ext(Type *int_to_extend, ParamInfo param)
{
	return abi_arg_new_direct_coerce_int_ext_by_reg(int_to_extend, false, param);
}

ABIArgInfo *abi_arg_new_direct_coerce_int_ext_by_reg(Type *int_to_extend, bool by_reg, ParamInfo param)
{
	ABIArgInfo *info = abi_arg_new_direct_coerce_type(abi_type_get(int_to_extend), param);
	if (type_is_signed(int_to_extend))
	{
		info->attributes.signext = true;
	}
	else
	{
		info->attributes.zeroext = true;
	}
	info->attributes.by_reg = by_reg;
	return info;
}

ABIArgInfo *abi_arg_new_direct_int_ext_by_reg(Type *int_to_extend, bool by_reg, ParamInfo param)
{
	ABIArgInfo *info = abi_arg_new(ABI_ARG_DIRECT, param);
	if (type_is_signed(int_to_extend))
	{
		info->attributes.signext = true;
	}
	else
	{
		info->attributes.zeroext = true;
	}
	info->attributes.by_reg = by_reg;
	return info;
}

ABIArgInfo *abi_arg_new_direct_pair(AbiType low_type, AbiType high_type, ParamInfo param)
{
	ABIArgInfo *arg_info = abi_arg_new(ABI_ARG_DIRECT_PAIR, param);
	arg_info->direct_pair.hi = high_type;
	arg_info->direct_pair.lo = low_type;
	return arg_info;
}

ABIArgInfo *abi_arg_new_direct_by_reg(bool by_reg, ParamInfo param)
{
	ABIArgInfo *info = abi_arg_new(ABI_ARG_DIRECT, param);
	info->attributes.by_reg = by_reg;
	return info;
}

ABIArgInfo *abi_arg_new_direct(ParamInfo param)
{
	return abi_arg_new_direct_by_reg(false, param);
}


ABIArgInfo *abi_arg_new_expand(ParamInfo param)
{
	return abi_arg_new(ABI_ARG_EXPAND, param);
}


ABIArgInfo *abi_arg_new_expand_coerce_pair(Type *first_element, Type *second_element, unsigned hi_offset, bool packed, ParamInfo param)
{
	ABIArgInfo *arg = abi_arg_new(ABI_ARG_EXPAND_COERCE, param);
	arg->coerce_expand.lo = first_element;
	arg->coerce_expand.hi = second_element;
	arg->coerce_expand.offset_hi = hi_offset;
	arg->coerce_expand.packed = packed;
	return arg;
}

ABIArgInfo *abi_arg_new_direct_coerce_int(ParamInfo param)
{
	return abi_arg_new(ABI_ARG_DIRECT_COERCE_INT, param);
}

ABIArgInfo *abi_arg_new_direct_coerce_type_spec(AbiSpecType type, ParamInfo param)
{
	ABIArgInfo *info = abi_arg_new(ABI_ARG_DIRECT_COERCE, param);
	info->direct_coerce_type = (AbiType){ .abi_type = type };
	return info;
}

ABIArgInfo *abi_arg_new_direct_coerce_type(AbiType type, ParamInfo param)
{
	ABIArgInfo *info = abi_arg_new(ABI_ARG_DIRECT_COERCE, param);
	info->direct_coerce_type = type;
	return info;
}

ABIArgInfo *abi_arg_new_direct_coerce_type_bits(int bits, ParamInfo param)
{
	ABIArgInfo *info = abi_arg_new(ABI_ARG_DIRECT_COERCE, param);
	info->direct_coerce_type = abi_type_get_int_bits(bits);
	return info;
}

ABIArgInfo *abi_arg_new_direct_struct_expand_i32(uint8_t elements, ParamInfo param)
{
	ABIArgInfo *info = abi_arg_new(ABI_ARG_DIRECT_SPLIT_STRUCT_I32, param);
	info->direct_struct_expand = elements;
	return info;
}

// Fully set up the prototype correctly
void c_abi_func_create(Signature *sig, FunctionPrototype *proto, Expr **vaargs)
{
	ASSERT(!proto->is_resolved);
	ParamInfo vaarg_params[512];
	ParamInfo params[512];
	proto->raw_variadic = sig->variadic == VARIADIC_RAW;
	proto->vararg_index = sig->vararg_index;
	Type *rtype = type_infoptr(sig->rtype)->type;
	Type *rtype_flat = type_flatten(rtype);
	unsigned param_count = 0;
	if (rtype_flat->type_kind == TYPE_VECTOR)
	{
		rtype_flat = type_array_from_vector(rtype_flat);
		proto->return_rewrite = PARAM_RW_VEC_TO_ARRAY;
	}
	if (type_is_optional(rtype))
	{
		proto->return_info = (ParamInfo){ .type = type_fault };
		proto->return_result = type_no_optional(rtype);
		if (type_is_void(rtype_flat))
		{
			proto->ret_rewrite = RET_OPTIONAL_VOID;
		}
		else
		{
			proto->ret_rewrite = RET_OPTIONAL_VALUE;
			params[param_count++] = (ParamInfo){ .type = type_get_ptr(rtype_flat) };
		}
	}
	else
	{
		proto->return_info = (ParamInfo){ .type = rtype_flat };
		proto->return_result = rtype;
		proto->ret_rewrite = RET_NORMAL;
	}
	proto->call_abi = sig->abi;

	unsigned param_decl_count = vec_size(sig->params);
	for (unsigned i = 0; i < param_decl_count; i++)
	{
		Decl *decl = sig->params[i];
		Type *flat_type = type_flatten(decl->type);
		ParamInfo param_info = (ParamInfo) { .type = flat_type };
		if (flat_type->type_kind == TYPE_VECTOR)
		{
			param_info.rewrite = PARAM_RW_VEC_TO_ARRAY;
			param_info.type = type_array_from_vector(flat_type);
		}
		params[param_count++] = param_info;
	}
	unsigned vaarg_count = 0;
	FOREACH(Expr *, val, vaargs)
	{
		vaarg_params[vaarg_count++] = (ParamInfo) { .type = type_flatten(val->type) };
	}
	proto->param_vacount = vaarg_count;
	proto->param_count = param_count;
	proto->is_resolved = true;
	switch (compiler.platform.abi)
	{
		case ABI_X64:
			c_abi_func_create_x64(proto, params, param_count, vaarg_params, vaarg_count);
			return;
		case ABI_X86:
			c_abi_func_create_x86(proto, params, param_count, vaarg_params, vaarg_count);
			return;
		case ABI_WIN64:
			c_abi_func_create_win64(proto, params, param_count, vaarg_params, vaarg_count);
			return;
		case ABI_AARCH64:
			c_abi_func_create_aarch64(proto, params, param_count, vaarg_params, vaarg_count);
			return;
		case ABI_RISCV:
			c_abi_func_create_riscv(proto, params, param_count, vaarg_params, vaarg_count);
			return;
		case ABI_WASM:
			c_abi_func_create_wasm(proto, params, param_count, vaarg_params, vaarg_count);
			return;
		case ABI_XTENSA:
			c_abi_func_create_default(proto, params, param_count, vaarg_params, vaarg_count);
			return;
		case ABI_UNKNOWN:
		case ABI_ARM:
		case ABI_PPC32:
		case ABI_PPC64_SVR4:
			break;
	}
	FATAL_ERROR("Unsupported ABI");
}


ABIArgInfo *c_abi_classify_return_type_default(ParamInfo param)
{
	Type *type = type_lowering(param.type);
	if (type_is_void(type)) return abi_arg_ignore();
	return c_abi_classify_argument_type_default(param);
}

ABIArgInfo *c_abi_classify_argument_type_default(ParamInfo param)
{
	// Perform general lowering.
	Type *type = type_lowering(param.type);

	// Struct-likes are returned by sret
	if (type_is_abi_aggregate(type)) return abi_arg_new_indirect_by_val(type, param);

	if (type_is_int128(type) && !compiler.platform.int128) return abi_arg_new_indirect_by_val(type, param);

	// Otherwise do we have a type that needs promotion?
	if (type_is_promotable_int_bool(type)) return abi_arg_new_direct_int_ext(type, param);

	// No, then do a direct pass.
	return abi_arg_new_direct(param);
}

void c_abi_func_create_default(FunctionPrototype *prototype, ParamInfo *params, unsigned param_count, ParamInfo *vaargs, unsigned vaarg_count)
{
	prototype->ret_abi_info = c_abi_classify_return_type_default(prototype->return_info);

	if (param_count)
	{
		ABIArgInfo **args = MALLOC(sizeof(ABIArgInfo) * param_count);
		for (unsigned i = 0; i < param_count; i++)
		{
			args[i] = c_abi_classify_argument_type_default(params[i]);
		}
		prototype->abi_args = args;
	}
	if (vaarg_count)
	{
		ABIArgInfo **args = MALLOC(sizeof(ABIArgInfo) * vaarg_count);
		for (unsigned i = 0; i < vaarg_count; i++)
		{
			args[i] = c_abi_classify_argument_type_default(vaargs[i]);
		}
		prototype->abi_varargs = args;
	}
}

