// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "c_abi_internal.h"


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

AbiType *abi_type_new_plain(Type *type)
{
	AbiType *abi_type = CALLOCS(AbiType);
	abi_type->kind = ABI_TYPE_PLAIN;
	abi_type->type = type;
	return abi_type;
}

AbiType *abi_type_new_int_bits(ByteSize bits)
{
	assert(bits < UINT32_MAX);
	AbiType *abi_type = CALLOCS(AbiType);
	abi_type->kind = ABI_TYPE_INT_BITS;
	abi_type->int_bits = (uint32_t)bits;
	return abi_type;
}

bool abi_type_is_integer(AbiType *type)
{
	return type->kind == ABI_TYPE_INT_BITS || type_is_integer(type->type);
}

bool abi_type_is_float(AbiType *type)
{
	return type->kind != ABI_TYPE_INT_BITS && type_is_float(type->type);
}

TypeSize abi_type_size(AbiType *type)
{
	switch (type->kind)
	{
		case ABI_TYPE_INT_BITS:
			return type->int_bits / 8;
		case ABI_TYPE_PLAIN:
			return type_size(type->type);
	}
	UNREACHABLE;
}

AlignSize abi_type_abi_alignment(AbiType *type)
{
	switch (type->kind)
	{
		case ABI_TYPE_INT_BITS:
			return type_abi_alignment(type_int_unsigned_by_bitsize(next_highest_power_of_2(type->int_bits)));
		case ABI_TYPE_PLAIN:
			return type_abi_alignment(type->type);
	}
	UNREACHABLE;
}

bool abi_arg_is_indirect(ABIArgInfo *info)
{
	switch (info->kind)
	{
		case ABI_ARG_IGNORE:
		case ABI_ARG_DIRECT_COERCE:
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
	assert(alignment > 0);
	ABIArgInfo *info = abi_arg_new(ABI_ARG_INDIRECT);
	info->indirect.alignment = alignment;
	assert(info->indirect.alignment);
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
	assert(info->indirect.alignment);
	return info;
}

ABIArgInfo *abi_arg_new_indirect_not_by_val(Type *type)
{
	ABIArgInfo *info = abi_arg_new(ABI_ARG_INDIRECT);
	info->indirect.alignment = type_abi_alignment(type);
	assert(info->indirect.alignment);
	info->indirect.type = type;
	info->attributes.by_val = false;
	return info;
}


ABIArgInfo *abi_arg_new_direct_int_ext(Type *int_to_extend)
{
	ABIArgInfo *arg_info = abi_arg_new(ABI_ARG_DIRECT_COERCE);
	if (type_is_signed(int_to_extend))
	{
		arg_info->attributes.signext = true;
	}
	else
	{
		arg_info->attributes.zeroext = true;
	}
	return arg_info;
}

ABIArgInfo *abi_arg_new_direct_pair(AbiType *low_type, AbiType *high_type)
{
	ABIArgInfo *arg_info = abi_arg_new(ABI_ARG_DIRECT_PAIR);
	arg_info->direct_pair.hi = high_type;
	arg_info->direct_pair.lo = low_type;
	return arg_info;
}

ABIArgInfo *abi_arg_new_direct(void)
{
	return abi_arg_new(ABI_ARG_DIRECT_COERCE);
}

ABIArgInfo *abi_arg_new_expand(void)
{
	return abi_arg_new(ABI_ARG_EXPAND);
}

ABIArgInfo *abi_arg_new_expand_coerce(AbiType *target_type, unsigned offset)
{
	ABIArgInfo *arg = abi_arg_new(ABI_ARG_EXPAND_COERCE);
	arg->coerce_expand.packed = offset > 0;
	assert(offset <= 0xFF);
	arg->coerce_expand.offset_lo = (unsigned char)offset;
	arg->coerce_expand.lo_index = offset > 0 ? 1 : 0;
	arg->coerce_expand.lo = target_type;
	return arg;
}

ABIArgInfo *abi_arg_new_expand_coerce_pair(AbiType *first_element, unsigned initial_offset, AbiType *second_element, unsigned padding, bool is_packed)
{
	ABIArgInfo *arg = abi_arg_new(ABI_ARG_EXPAND_COERCE);
	arg->coerce_expand.packed = is_packed;
	assert(initial_offset <= 0xFF && padding <= 0xFF);
	arg->coerce_expand.offset_lo = (unsigned char)initial_offset;
	arg->coerce_expand.lo_index = initial_offset > 0 ? 1 : 0;
	arg->coerce_expand.lo = first_element;
	arg->coerce_expand.hi = second_element;
	arg->coerce_expand.padding_hi = (uint8_t)padding;
	arg->coerce_expand.offset_hi = (uint8_t)(padding + initial_offset + abi_type_size(first_element));
	arg->coerce_expand.hi_index = arg->coerce_expand.lo_index + (padding > 0 ? 1U : 0U);
	return arg;
}

ABIArgInfo *abi_arg_new_direct_coerce(AbiType *target_type)
{
	assert(target_type);
	ABIArgInfo *info = abi_arg_new(ABI_ARG_DIRECT_COERCE);
	info->direct_coerce.type = target_type;
	info->direct_coerce.elements = 0;
	return info;
}

ABIArgInfo *abi_arg_new_expand_padded(Type *padding)
{
	ABIArgInfo *info = abi_arg_new(ABI_ARG_EXPAND);
	info->expand.padding_type = padding;
	return info;
}


void c_abi_func_create(FunctionSignature *signature)
{
	switch (platform_target.abi)
	{
		case ABI_X64:
			c_abi_func_create_x64(signature);
			break;
		case ABI_X86:
			c_abi_func_create_x86(signature);
			break;
		case ABI_WIN64:
			c_abi_func_create_win64(signature);
			break;
		case ABI_AARCH64:
			c_abi_func_create_aarch64(signature);
			break;
		case ABI_RISCV:
			c_abi_func_create_riscv(signature);
			break;
		case ABI_WASM:
			c_abi_func_create_wasm(signature);
			break;
		default:
			FATAL_ERROR("Unsupported ABI");
	}
}


ABIArgInfo *c_abi_classify_return_type_default(Type *type)
{
	if (type->type_kind == TYPE_VOID) return abi_arg_ignore();
	return c_abi_classify_argument_type_default(type);
}

ABIArgInfo *c_abi_classify_argument_type_default(Type *type)
{
	// Perform general lowering.
	type = type_lowering(type);

	// Struct-likes are returned by sret
	if (type_is_abi_aggregate(type)) return abi_arg_new_indirect_by_val(type);

	if (type_is_int128(type) && !platform_target.int128) return abi_arg_new_indirect_by_val(type);

	// Otherwise do we have a type that needs promotion?
	if (type_is_promotable_integer(type)) return abi_arg_new_direct_int_ext(type);

	// No, then do a direct pass.
	return abi_arg_new_direct();
}

