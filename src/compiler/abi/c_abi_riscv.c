// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "compiler/c_abi_internal.h"


static ABIArgInfo *riscv_coerce_and_expand_fpcc_struct(AbiType field1, unsigned field1_offset, AbiType field2, unsigned field2_offset)
{
	ASSERT0(abi_type_is_type(field1));
	if (!abi_type_is_valid(field2))
	{
		return abi_arg_new_direct_coerce_type(field1.type);
	}

	ASSERT0(abi_type_is_type(field2));
	Type *type2 = field2.type;
	ByteSize abi_type_size = type_size(type2);
	// Not on even offset, use packed semantics.
	if (field2_offset % abi_type_size != 0)
	{
		return abi_arg_new_expand_coerce_pair(field1.type, field2.type, field2_offset, true);
	}
	return abi_arg_new_expand_coerce_pair(field1.type, field2.type, field2_offset / abi_type_size, false);
}

static bool riscv_detect_fpcc_struct_internal(Type *type, unsigned current_offset, AbiType *field1_ref, unsigned *field1_offset, AbiType *field2_ref, unsigned *field2_offset)
{
	bool is_int = type_is_integer_or_bool_kind(type);
	bool is_float = type_is_float(type);
	unsigned flen = compiler.platform.riscv.flen;
	ByteSize size = type_size(type);
	if (is_int || is_float)
	{
		if (is_int && size > compiler.platform.riscv.xlen) return false;
		// Can't be eligible if larger than the FP registers. Half precision isn't
		// currently supported on RISC-V and the ABI hasn't been confirmed, so
		// default to the integer ABI in that case.
		if (is_float && (size > flen || size < 4)) return false;
		// Can't be eligible if an integer type was already found (int+int pairs
		// are not eligible).
		if (is_int && abi_type_is_valid(*field1_ref) && abi_type_is_integer(*field1_ref)) return false;
		if (!abi_type_is_valid(*field1_ref))
		{
			abi_type_set_type(field1_ref, type);
			*field1_offset = current_offset;
			return true;
		}
		if (!abi_type_is_valid(*field2_ref))
		{
			abi_type_set_type(field2_ref, type);
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
												   field1_ref,
												   field1_offset,
												   field2_ref,
												   field2_offset)) return false;
			current_offset += (unsigned)element_size;
		}
		return true;
	}

	if (type_is_union_or_strukt(type))
	{
		// Unions aren't eligible unless they're empty (which is caught above).
		if (type->type_kind == TYPE_UNION) return false;
		FOREACH(Decl *, member, type->decl->strukt.members)
		{
			if (!riscv_detect_fpcc_struct_internal(member->type,
												   (unsigned)(current_offset + member->offset),
												   field1_ref,
												   field1_offset,
												   field2_ref,
												   field2_offset)) return false;

		}
		return abi_type_is_valid(*field1_ref);
	}
	return false;
}

static bool riscv_detect_fpcc_struct(Type *type, AbiType *field1_ref, unsigned *field1_offset, AbiType *field2_ref, unsigned *field2_offset, unsigned *gprs, unsigned *fprs)
{
	*field1_ref = ABI_TYPE_EMPTY;
	*field2_ref = ABI_TYPE_EMPTY;
	*gprs = 0;
	*fprs = 0;

	bool is_candidate = riscv_detect_fpcc_struct_internal(type, 0, field1_ref, field1_offset, field2_ref, field2_offset);

	// Not really a candidate if we have a single int but no float.
	if (abi_type_is_valid(*field1_ref) && !abi_type_is_valid(*field2_ref) && !abi_type_is_float(*field1_ref)) return false;
	if (!is_candidate) return false;
	if (abi_type_is_valid(*field1_ref))
	{
		if (abi_type_is_float(*field1_ref))
		{
			(*fprs)++;
		}
		else
		{
			(*gprs)++;
		}
	}
	if (abi_type_is_valid(*field2_ref))
	{
		if (abi_type_is_float(*field2_ref))
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

	ASSERT0(type == type->canonical);

	unsigned xlen = compiler.platform.riscv.xlen;
	ASSERT0(is_power_of_two(xlen));

	ByteSize size = type_size(type);

	// Pass floating point values via FPRs if possible.
	if (is_fixed && type_is_float(type) && compiler.platform.riscv.flen >= size && *fprs)
	{
		(*fprs)--;
		return abi_arg_new_direct();
	}

	if (is_fixed && compiler.platform.riscv.flen && type->type_kind == TYPE_STRUCT)
	{
		AbiType field1, field2;
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
		if (size < xlen && type_is_integer_or_bool_kind(type) && !must_use_stack)
		{
			// Clang: RV64 ABI requires unsigned 32-bit integers to be sign extended.
			if (xlen == 8 && type == type_uint)
			{
				return abi_arg_new_direct_int_ext(type_int);
			}
			return abi_arg_new_direct_int_ext(type);
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
			return abi_arg_new_direct_coerce_type(type_int_unsigned_by_bitsize(xlen * 8));
		}
		if (alignment == 2 * compiler.platform.riscv.xlen)
		{
			return abi_arg_new_direct_coerce_type(type_int_unsigned_by_bitsize(xlen * 16));
		}
		Type *ret_type = type_int_unsigned_by_bitsize(xlen * 8);
		return abi_arg_new_direct_coerce_type(type_get_array(ret_type, 2));
	}
	return abi_arg_new_indirect_not_by_val(type);
}

static ABIArgInfo *riscv_classify_return(Type *return_type)
{
	if (type_is_void(return_type)) return abi_arg_ignore();

	unsigned arg_gpr_left = 2;
	unsigned arg_fpr_left = compiler.platform.riscv.flen ? 2 : 0;

	// The rules for return and argument types are the same, so defer to
	// classifyArgumentType.
	return riscv_classify_argument_type(return_type, true, &arg_gpr_left, &arg_fpr_left);
}
ABIArgInfo **riscv_create_params(Type** params, bool is_fixed, unsigned *arg_gprs_left, unsigned *arg_fprs_left)
{
	unsigned param_count = vec_size(params);
	if (!param_count) return NULL;
	ABIArgInfo **args = MALLOC(sizeof(ABIArgInfo) * param_count);
	for (unsigned i = 0; i < param_count; i++)
	{
		args[i] = riscv_classify_argument_type(type_lowering(params[i]), is_fixed, arg_gprs_left, arg_fprs_left);
	}
	return args;
}
void c_abi_func_create_riscv(FunctionPrototype *prototype)
{
	// Registers
	unsigned gpr = 8;
	unsigned fpr = 8;

	Type *ret_type = type_lowering(prototype->abi_ret_type);
	ABIArgInfo *ret_abi = prototype->ret_abi_info = riscv_classify_return(ret_type);

	// IsRetIndirect is true if classifyArgumentType indicated the value should
	// be passed indirect, or if the type size is a scalar greater than 2*XLen
	// and not a complex type with elements <= FLen. e.g. fp128 is passed direct
	// in LLVM IR, relying on the backend lowering code to rewrite the argument
	// list and pass indirectly on RV32.
	bool is_ret_indirect = abi_arg_is_indirect(ret_abi);
	if (type_is_scalar(ret_type) && type_size(ret_type) > 2 * compiler.platform.riscv.xlen)
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
	unsigned arg_fprs_left = compiler.platform.riscv.flen ? fpr : 0;

	// If we have an optional, then the return type is a parameter.
	if (prototype->ret_by_ref)
	{
		prototype->ret_by_ref_abi_info = riscv_classify_argument_type(type_get_ptr(type_lowering(prototype->ret_by_ref_type)),
																	  true, &arg_gprs_left, &arg_fprs_left);
	}

	prototype->abi_args = riscv_create_params(prototype->param_types, true, &arg_gprs_left, &arg_fprs_left);
	prototype->abi_varargs = riscv_create_params(prototype->varargs, false, &arg_gprs_left, &arg_fprs_left);
}
