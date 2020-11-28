// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_c_abi_internal.h"


ABIArgInfo *abi_arg_new(ABIKind kind)
{
	ABIArgInfo *info = CALLOCS(ABIArgInfo);
	info->kind = kind;
	return info;
}

AbiType *abi_type_new_plain(Type *type)
{
	AbiType *abi_type = CALLOCS(AbiType);
	abi_type->kind = ABI_TYPE_PLAIN;
	abi_type->type = type;
	return abi_type;
}

AbiType *abi_type_new_int_bits(unsigned bits)
{
	AbiType *abi_type = CALLOCS(AbiType);
	abi_type->kind = ABI_TYPE_INT_BITS;
	abi_type->int_bits = bits;
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

size_t abi_type_size(AbiType *type)
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

size_t abi_type_abi_alignment(AbiType *type)
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

ABIArgInfo *abi_arg_new_indirect_realigned(unsigned alignment)
{
	assert(alignment > 0);
	ABIArgInfo *info = abi_arg_new(ABI_ARG_INDIRECT);
	info->indirect.realignment = alignment;
	info->indirect.by_val = true;
	return info;
}

ABIArgInfo *abi_arg_new_indirect_by_val(void)
{
	ABIArgInfo *info = abi_arg_new(ABI_ARG_INDIRECT);
	info->indirect.by_val = true;
	return info;
}

ABIArgInfo *abi_arg_new_indirect_not_by_val(void)
{
	ABIArgInfo *info = abi_arg_new(ABI_ARG_INDIRECT);
	info->indirect.by_val = false;
	return info;
}

size_t abi_arg_expanded_size(ABIArgInfo *type_info, Type *type)
{
	switch (type->type_kind)
	{
		case TYPE_TYPEDEF:
			return abi_arg_expanded_size(type_info, type->canonical);
		case TYPE_ARRAY:
			return abi_arg_expanded_size(type_info, type->array.base) * type->array.len;
		case TYPE_STRUCT:
		{
			Decl **members = type->decl->strukt.members;
			size_t result = 0;
			VECEACH(members, i)
			{
				members += abi_arg_expanded_size(type_info, members[i]->type);
			}
			return result;
		}
		case TYPE_UNION:
		{
			Type *max_union = type_find_largest_union_element(type);
			if (!max_union) return 0;
			return abi_arg_expanded_size(type_info, max_union);
		}
		case TYPE_COMPLEX:
		case TYPE_SUBARRAY:
		case TYPE_STRING:
			// Complex is { real, real }, Sub array { pointer, len } = String?
			return 2;
		case TYPE_ERR_UNION:
		case TYPE_VOID:
		case TYPE_BOOL:
		case ALL_FLOATS:
		case ALL_INTS:
		case TYPE_TYPEID:
		case TYPE_POINTER:
		case TYPE_ENUM:
		case TYPE_ERRTYPE:
		case TYPE_VARARRAY:
		case TYPE_VECTOR:
			return 1;
		case TYPE_POISONED:
		case TYPE_FUNC:
		case TYPE_TYPEINFO:
		case TYPE_MEMBER:
			UNREACHABLE
	}
	UNREACHABLE
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

ABIArgInfo *abi_arg_new_expand_coerce(AbiType *target_type, unsigned offset)
{
	ABIArgInfo *arg = abi_arg_new(ABI_ARG_EXPAND_COERCE);
	arg->coerce_expand.packed = offset > 0;
	arg->coerce_expand.offset_lo = offset;
	arg->coerce_expand.lo_index = offset > 0 ? 1 : 0;
	arg->coerce_expand.lo = target_type;
	return arg;
}

ABIArgInfo *abi_arg_new_expand_coerce_pair(AbiType *first_element, unsigned initial_offset, AbiType *second_element, unsigned padding, bool is_packed)
{
	ABIArgInfo *arg = abi_arg_new(ABI_ARG_EXPAND_COERCE);
	arg->coerce_expand.packed = is_packed;
	arg->coerce_expand.offset_lo = initial_offset;
	arg->coerce_expand.lo_index = initial_offset > 0 ? 1 : 0;
	arg->coerce_expand.lo = first_element;
	arg->coerce_expand.hi = second_element;
	arg->coerce_expand.padding_hi = padding;
	arg->coerce_expand.offset_hi = padding + initial_offset + abi_type_size(first_element);
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

ABIArgInfo *classify_return_type_default(Type *type)
{
	if (type == type_void)
	{
		return abi_arg_new(ABI_ARG_IGNORE);
	}

	// Struct-likes are returned by sret
	if (type_is_abi_aggregate(type))
	{
		return abi_arg_new(ABI_ARG_INDIRECT);
	}

	// Otherwise do we have a type that needs promotion?
	if (type_is_promotable_integer(type_lowering(type)))
	{
		ABIArgInfo *arg_info = abi_arg_new(ABI_ARG_DIRECT_COERCE);
		if (type_is_signed(type))
		{
			arg_info->attributes.signext = true;
		}
		else
		{
			arg_info->attributes.zeroext = true;
		}
		return arg_info;
	}

	// No, then do a direct pass.
	return abi_arg_new_direct();
}

void c_abi_func_create(GenContext *context, FunctionSignature *signature)
{
	switch (build_target.abi)
	{
		case ABI_X64:
			c_abi_func_create_x64(context, signature);
			break;
		case ABI_X86:
			c_abi_func_create_x86(context, signature);
			break;
		case ABI_WIN64:
			c_abi_func_create_win64(context, signature);
			break;
		case ABI_AARCH64:
			c_abi_func_create_aarch64(context, signature);
			break;
		default:
			FATAL_ERROR("Unsupported ABI");
	}
}

ABIArgInfo *c_abi_classify_argument_type_default(Type *type)
{
	type = type_lowering(type);

	// Struct-likes are returned by sret
	if (type_is_abi_aggregate(type))
	{
		return abi_arg_new(ABI_ARG_INDIRECT);
	}

	if (type_is_int128(type) && !build_target.int_128)
	{
		return abi_arg_new_indirect_by_val();
	}

	// Otherwise do we have a type that needs promotion?
	if (type_is_promotable_integer(type))
	{
		return abi_arg_new_direct_int_ext(type);
	}

	// No, then do a direct pass.
	return abi_arg_new_direct();
}
