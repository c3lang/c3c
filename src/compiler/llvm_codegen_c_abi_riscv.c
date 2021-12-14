// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "c_abi_internal.h"


static ABIArgInfo *riscv_coerce_and_expand_fpcc_struct(AbiType *field1, unsigned field1_offset, AbiType *field2, unsigned field2_offset)
{
	if (!field2)
	{
		return abi_arg_new_expand_coerce(field1, field1_offset);
	}

	unsigned field2_alignment = abi_type_abi_alignment(field2);
	unsigned field1_size = abi_type_size(field1);
	unsigned field2_offset_no_pad = aligned_offset(field1_size, field2_alignment);

	unsigned padding = 0;

	if (field2_offset > field2_offset_no_pad)
	{
		padding = field2_offset - field2_offset_no_pad;
	}
	else if (field2_offset != field2_alignment && field2_offset > field1_size)
	{
		padding = field2_offset - field1_size;
	}

	bool is_packed = field2_offset % field2_alignment != 0;

	return abi_arg_new_expand_coerce_pair(field1, field1_offset, field2, padding, is_packed);
}

static bool riscv_detect_fpcc_struct_internal(Type *type, unsigned current_offset, AbiType **field1, unsigned *field1_offset, AbiType **field2, unsigned *field2_offset)
{
	bool is_int = type_is_integer(type);
	bool is_float = type_is_float(type);
	unsigned flen = platform_target.riscv.flen;
	ByteSize size = type_size(type);
	if (is_int || is_float)
	{
		if (is_int && size > platform_target.riscv.xlen) return false;
		// Can't be eligible if larger than the FP registers. Half precision isn't
		// currently supported on RISC-V and the ABI hasn't been confirmed, so
		// default to the integer ABI in that case.
		if (is_float && (size > flen || size < 4)) return false;
		// Can't be eligible if an integer type was already found (int+int pairs
		// are not eligible).
		if (is_int && *field1 && abi_type_is_integer(*field1)) return false;
		if (!*field1)
		{
			*field1 = abi_type_new_plain(type);
			*field1_offset = current_offset;
			return true;
		}
		if (!*field2)
		{
			*field2 = abi_type_new_plain(type);
			*field2_offset = current_offset;
			return true;
		}
		return false;
	}

	if (type->type_kind == TYPE_ARRAY)
	{
		ByteSize array_len = type->array.len;
		Type *element_type = type->array.base;
		ByteSize element_size = type_size(element_type);
		for (ByteSize i = 0; i < array_len; i++)
		{
			if (!riscv_detect_fpcc_struct_internal(element_type,
			                                       current_offset,
			                                       field1,
			                                       field1_offset,
			                                       field2,
			                                       field2_offset)) return false;
			current_offset += (unsigned)element_size;
		}
		return true;
	}

	if (type_is_structlike(type))
	{
		// Unions aren't eligible unless they're empty (which is caught above).
		if (type->type_kind == TYPE_UNION) return false;
		Decl **members = type->decl->strukt.members;
		VECEACH(members, i)
		{
			Decl *member = members[i];
			if (!riscv_detect_fpcc_struct_internal(member->type,
												   (unsigned)(current_offset + member->offset),
			                                       field1,
			                                       field1_offset,
			                                       field2,
			                                       field2_offset)) return false;

		}
		return *field1 != NULL;
	}
	return false;
}

static bool riscv_detect_fpcc_struct(Type *type, AbiType **field1, unsigned *field1_offset, AbiType **field2, unsigned *field2_offset, unsigned *gprs, unsigned *fprs)
{
	*field1 = NULL;
	*field2 = NULL;
	*gprs = 0;
	*fprs = 0;

	bool is_candidate = riscv_detect_fpcc_struct_internal(type, 0, field1, field1_offset, field2, field2_offset);

	// Not really a candidate if we have a single int but no float.
	if (*field1 && !*field2 && !abi_type_is_float(*field1)) return false;
	if (!is_candidate) return false;
	if (*field1)
	{
		if (abi_type_is_float(*field1))
		{
			(*fprs)++;
		}
		else
		{
			(*gprs)++;
		}
	}
	if (*field2)
	{
		if (abi_type_is_float(*field2))
		{
			(*fprs)++;
		}
		else
		{
			(*gprs)++;
		}
	}
	return true;
}

static ABIArgInfo *riscv_classify_argument_type(Type *type, bool is_fixed, unsigned *gprs, unsigned *fprs)
{

	assert(type == type->canonical);

	unsigned xlen = platform_target.riscv.xlen;

	ByteSize size = type_size(type);

	// Pass floating point values via FPRs if possible.
	if (is_fixed && type_is_float(type) && platform_target.riscv.flen >= size && *fprs)
	{
		(*fprs)--;
		return abi_arg_new_direct();
	}


	if (is_fixed && platform_target.riscv.flen && type->type_kind == TYPE_STRUCT)
	{
		AbiType *field1 = NULL;
		AbiType *field2 = NULL;
		unsigned offset1 = 0;
		unsigned offset2 = 0;
		unsigned needed_gprs;
		unsigned needed_fprs;
		bool is_candidate = riscv_detect_fpcc_struct(type,
		                                             &field1,
		                                             &offset1,
		                                             &field2,
		                                             &offset2,
		                                             &needed_gprs,
		                                             &needed_fprs);
		if (is_candidate && needed_gprs <= *gprs && needed_fprs <= *fprs)
		{
			*gprs -= needed_gprs;
			*fprs -= needed_fprs;
			return riscv_coerce_and_expand_fpcc_struct(field1, offset1, field2, offset2);
		}
	}

	unsigned alignment = type_abi_alignment(type);
	bool must_use_stack = false;
	// Clang: Determine the number of GPRs needed to pass the current argument
	// according to the ABI. 2*XLen-aligned varargs are passed in "aligned"
	// register pairs, so may consume 3 registers.
	unsigned needed_gprs = 1;
	if (!is_fixed && alignment == 2 * xlen)
	{
		needed_gprs = 2 + (*gprs % 2U);
	}
	else if (size > xlen && size <= 2 * xlen)
	{
		needed_gprs = 2;
	}
	if (needed_gprs > *gprs)
	{
		must_use_stack = true;
		needed_gprs = *gprs;
	}

	*gprs -= needed_gprs;

	if (!type_is_abi_aggregate(type) && type->type_kind != TYPE_VECTOR)
	{
		// All integral types are promoted to XLen width, unless passed on the
		// stack.
		if (size < xlen && type_is_integer(type) && !must_use_stack)
		{
			return abi_arg_new_expand_padded(type_int_unsigned_by_bitsize(xlen * 8));
		}
		if (size > 16 || (size > 8 && !platform_target.int128))
		{
			return abi_arg_new_indirect_not_by_val(type);
		}
		return abi_arg_new_direct();
	}

	// Aggregates which are <= 2*XLen will be passed in registers if possible,
	// so coerce to integers.
	if (size <= 2 * xlen)
	{
		// Use a single XLen int if possible, 2*XLen if 2*XLen alignment is
		// required, and a 2-field XLen array if only XLen alignment is required.
		if (size <= xlen)
		{
			return abi_arg_new_direct_coerce(abi_type_new_int_bits(xlen * 8));
		}
		if (alignment == 2 * platform_target.riscv.xlen)
		{
			return abi_arg_new_direct_coerce(abi_type_new_int_bits(xlen * 16));
		}
		ABIArgInfo *info = abi_arg_new_direct_coerce(abi_type_new_int_bits(xlen));
		info->direct_coerce.elements = 2;
		return info;
	}
	return abi_arg_new_indirect_not_by_val(type);
}

static ABIArgInfo *riscv_classify_return(Type *return_type)
{
	if (return_type->type_kind == TYPE_VOID) return abi_arg_ignore();

	unsigned arg_gpr_left = 2;
	unsigned arg_fpr_left = platform_target.riscv.flen ? 2 : 0;

	// The rules for return and argument types are the same, so defer to
	// classifyArgumentType.
	return riscv_classify_argument_type(return_type, true, &arg_gpr_left, &arg_fpr_left);
}

void c_abi_func_create_riscv(FunctionSignature *signature)
{
	// Registers
	unsigned gpr = 8;
	unsigned fpr = 8;

	bool failable = IS_FAILABLE(signature->rtype);
	Type *rtype = abi_rtype(signature);
	Type *return_type = abi_returntype(signature);
	return_type = type_lowering(return_type);
	ABIArgInfo *return_abi = riscv_classify_return(return_type);
	if (failable)
	{
		signature->failable_abi_info = return_abi;
	}
	else
	{
		signature->ret_abi_info = return_abi;
	}

	// IsRetIndirect is true if classifyArgumentType indicated the value should
	// be passed indirect, or if the type size is a scalar greater than 2*XLen
	// and not a complex type with elements <= FLen. e.g. fp128 is passed direct
	// in LLVM IR, relying on the backend lowering code to rewrite the argument
	// list and pass indirectly on RV32.
	bool is_ret_indirect = abi_arg_is_indirect(return_abi);
	if (type_is_scalar(return_type) && type_size(return_type) > 2 * platform_target.riscv.xlen)
	{
		// Normal scalar > 2 * XLen, e.g. f128 on RV32
		is_ret_indirect = true;
	}
	// Clang: We must track the number of GPRs used in order to conform to the RISC-V
	// ABI, as integer scalars passed in registers should have signext/zeroext
	// when promoted, but are anyext if passed on the stack. As GPR usage is
	// different for variadic arguments, we must also track whether we are
	// examining a vararg or not.
	unsigned arg_gprs_left = is_ret_indirect ? gpr - 1 : gpr;
	unsigned arg_fprs_left = platform_target.riscv.flen ? fpr : 0;

	// If we have a failable, then the return type is a parameter.
	if (IS_FAILABLE(signature->rtype) && rtype != type_void)
	{
		signature->ret_abi_info = riscv_classify_argument_type(type_get_ptr(type_lowering(rtype)),
														 true, &arg_gprs_left, &arg_fprs_left);
	}

	Decl **params = signature->params;
	VECEACH(params, i)
	{
		bool is_fixed = true;
		params[i]->var.abi_info = riscv_classify_argument_type(type_lowering(params[i]->type), is_fixed, &arg_gprs_left, &arg_fprs_left);
	}
}
