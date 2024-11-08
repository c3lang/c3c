// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler/c_abi_internal.h"


static ABIArgInfo *abi_arg_new(ABIKind kind)
{
	ABIArgInfo *info = CALLOCS(ABIArgInfo);
	info->kind = kind;
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
	return (type.int_bits_plus_1 - 1) / 8;
}

AlignSize abi_type_abi_alignment(AbiType type)
{
	if (abi_type_is_type(type)) return type_abi_alignment(type.type);
	return type_abi_alignment(type_int_unsigned_by_bitsize(next_highest_power_of_2(type.int_bits_plus_1 - 1)));
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

ABIArgInfo *abi_arg_new_indirect_realigned(AlignSize alignment, Type *by_val_type)
{
	ASSERT0(alignment > 0);
	ABIArgInfo *info = abi_arg_new(ABI_ARG_INDIRECT);
	info->indirect.alignment = alignment;
	ASSERT0(info->indirect.alignment);
	info->attributes.realign = true;
	info->indirect.type = by_val_type;
	info->attributes.by_val = true;
	return info;
}

ABIArgInfo *abi_arg_new_indirect_by_val(Type *by_val_type)
{
	ABIArgInfo *info = abi_arg_new(ABI_ARG_INDIRECT);
	info->indirect.alignment = type_abi_alignment(by_val_type);
	info->indirect.type = by_val_type;
	info->attributes.by_val = true;
	ASSERT0(info->indirect.alignment);
	return info;
}

ABIArgInfo *abi_arg_new_indirect_not_by_val(Type *type)
{
	ABIArgInfo *info = abi_arg_new(ABI_ARG_INDIRECT);
	info->indirect.alignment = type_abi_alignment(type);
	ASSERT0(info->indirect.alignment);
	info->indirect.type = type;
	info->attributes.by_val = false;
	return info;
}

ABIArgInfo *abi_arg_new_direct_int_ext(Type *int_to_extend)
{
	return abi_arg_new_direct_int_ext_by_reg(int_to_extend, false);
}

ABIArgInfo *abi_arg_new_direct_coerce_int_ext(Type *int_to_extend)
{
	return abi_arg_new_direct_coerce_int_ext_by_reg(int_to_extend, false);
}

ABIArgInfo *abi_arg_new_direct_coerce_int_ext_by_reg(Type *int_to_extend, bool by_reg)
{
	ABIArgInfo *info = abi_arg_new_direct_coerce_type(int_to_extend);
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

ABIArgInfo *abi_arg_new_direct_int_ext_by_reg(Type *int_to_extend, bool by_reg)
{
	ABIArgInfo *info = abi_arg_new(ABI_ARG_DIRECT);
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

ABIArgInfo *abi_arg_new_direct_pair(AbiType low_type, AbiType high_type)
{
	ABIArgInfo *arg_info = abi_arg_new(ABI_ARG_DIRECT_PAIR);
	arg_info->direct_pair.hi = high_type;
	arg_info->direct_pair.lo = low_type;
	return arg_info;
}

ABIArgInfo *abi_arg_new_direct_by_reg(bool by_reg)
{
	ABIArgInfo *info = abi_arg_new(ABI_ARG_DIRECT);
	info->attributes.by_reg = by_reg;
	return info;
}

ABIArgInfo *abi_arg_new_direct(void)
{
	return abi_arg_new_direct_by_reg(false);
}


ABIArgInfo *abi_arg_new_expand(void)
{
	return abi_arg_new(ABI_ARG_EXPAND);
}


ABIArgInfo *abi_arg_new_expand_coerce_pair(Type *first_element, Type *second_element, unsigned hi_offset, bool packed)
{
	ABIArgInfo *arg = abi_arg_new(ABI_ARG_EXPAND_COERCE);
	arg->coerce_expand.lo = first_element;
	arg->coerce_expand.hi = second_element;
	arg->coerce_expand.offset_hi = hi_offset;
	arg->coerce_expand.packed = packed;
	return arg;
}

ABIArgInfo *abi_arg_new_direct_coerce_int(void)
{
	return abi_arg_new(ABI_ARG_DIRECT_COERCE_INT);
}

ABIArgInfo *abi_arg_new_direct_coerce_type(Type *type)
{
	ASSERT0(type);
	ABIArgInfo *info = abi_arg_new(ABI_ARG_DIRECT_COERCE);
	info->direct_coerce_type = type->canonical;
	return info;
}

ABIArgInfo *abi_arg_new_direct_struct_expand_i32(uint8_t elements)
{
	ABIArgInfo *info = abi_arg_new(ABI_ARG_DIRECT_SPLIT_STRUCT_I32);
	info->direct_struct_expand = elements;
	return info;
}


void c_abi_func_create(FunctionPrototype *proto)
{
	ASSERT0(!proto->is_resolved);
	proto->is_resolved = true;
	switch (compiler.platform.abi)
	{
		case ABI_X64:
			c_abi_func_create_x64(proto);
			return;
		case ABI_X86:
			c_abi_func_create_x86(proto);
			return;
		case ABI_WIN64:
			c_abi_func_create_win64(proto);
			return;
		case ABI_AARCH64:
			c_abi_func_create_aarch64(proto);
			return;
		case ABI_RISCV:
			c_abi_func_create_riscv(proto);
			return;
		case ABI_WASM:
			c_abi_func_create_wasm(proto);
			return;
		case ABI_XTENSA:
			c_abi_func_create_default(proto);
			return;
		case ABI_UNKNOWN:
		case ABI_ARM:
		case ABI_PPC32:
		case ABI_PPC64_SVR4:
			break;
	}
	FATAL_ERROR("Unsupported ABI");
}


ABIArgInfo *c_abi_classify_return_type_default(Type *type)
{
	if (type_is_void(type)) return abi_arg_ignore();
	return c_abi_classify_argument_type_default(type);
}

ABIArgInfo *c_abi_classify_argument_type_default(Type *type)
{
	// Perform general lowering.
	type = type_lowering(type);

	// Struct-likes are returned by sret
	if (type_is_abi_aggregate(type)) return abi_arg_new_indirect_by_val(type);

	if (type_is_int128(type) && !compiler.platform.int128) return abi_arg_new_indirect_by_val(type);

	// Otherwise do we have a type that needs promotion?
	if (type_is_promotable_int_bool(type)) return abi_arg_new_direct_int_ext(type);

	// No, then do a direct pass.
	return abi_arg_new_direct();
}

void c_abi_func_create_default(FunctionPrototype *prototype)
{
	prototype->ret_abi_info = c_abi_classify_return_type_default(prototype->abi_ret_type);
	if (prototype->ret_by_ref)
	{
		prototype->ret_by_ref_abi_info = c_abi_classify_return_type_default(type_get_ptr(type_flatten(prototype->ret_by_ref_type)));
	}

	Type **params = prototype->param_types;
	unsigned param_count = vec_size(prototype->param_types);
	if (param_count)
	{
		ABIArgInfo **args = MALLOC(sizeof(ABIArgInfo) * param_count);
		for (unsigned i = 0; i < param_count; i++)
		{
			args[i] = c_abi_classify_argument_type_default(params[i]);
		}
		prototype->abi_args = args;
	}
	Type **va_params = prototype->varargs;
	unsigned va_param_count = vec_size(va_params);
	if (va_param_count)
	{
		ABIArgInfo **args = MALLOC(sizeof(ABIArgInfo) * va_param_count);
		for (unsigned i = 0; i < va_param_count; i++)
		{
			args[i] = c_abi_classify_argument_type_default(va_params[i]);
		}
		prototype->abi_varargs = args;
	}
}