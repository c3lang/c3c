#pragma once
// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "codegen_internal.h"

typedef enum
{
	BY_VAL,
	BY_VAL_SKIP
} ByVal;

bool abi_arg_is_indirect(ABIArgInfo *info);
ABIArgInfo *abi_arg_ignore(void);
ABIArgInfo *abi_arg_new_direct_pair(AbiType low_type, AbiType high_type, ParamInfo param);
ABIArgInfo *abi_arg_new_direct(ParamInfo param);
ABIArgInfo *abi_arg_new_direct_by_reg(bool by_reg, ParamInfo param);
ABIArgInfo *abi_arg_new_expand(ParamInfo param);
ABIArgInfo *abi_arg_new_direct_int_ext(Type *type_to_extend, ParamInfo param);
ABIArgInfo *abi_arg_new_direct_int_ext_by_reg(Type *int_to_extend, bool by_reg, ParamInfo param);
ABIArgInfo *abi_arg_new_direct_coerce_int_ext_by_reg(Type *int_to_extend, bool by_reg, ParamInfo param);
ABIArgInfo *abi_arg_new_direct_coerce_int_ext(Type *int_to_extend, ParamInfo param);
ABIArgInfo *abi_arg_new_direct_coerce_int(ParamInfo param);
ABIArgInfo *abi_arg_new_direct_coerce_type(AbiType type, ParamInfo param);
ABIArgInfo *abi_arg_new_direct_coerce_type_spec(AbiSpecType type, ParamInfo param);
ABIArgInfo *abi_arg_new_direct_coerce_type_bits(int bits, ParamInfo param);
ABIArgInfo *abi_arg_new_direct_struct_expand_i32(uint8_t elements, ParamInfo param);
ABIArgInfo *abi_arg_new_expand_coerce_pair(Type *first_element, Type *second_element, unsigned hi_offset, bool packed, ParamInfo param);
ABIArgInfo *abi_arg_new_indirect_realigned(AlignSize alignment, Type *by_val_type, ParamInfo param);
ABIArgInfo *abi_arg_new_indirect_by_val(Type *by_val_type, ParamInfo param);
ABIArgInfo *abi_arg_new_indirect_not_by_val(Type *type, ParamInfo param);

AlignSize abi_type_abi_alignment(AbiType type);
bool abi_type_is_integer(AbiType type);
bool abi_type_is_float(AbiType type);
static inline void abi_type_set_type(AbiType *abi_type, Type *type);
static inline AbiType abi_type_get(Type *type);

TypeSize abi_type_size(AbiType type);

typedef struct
{
	unsigned int_regs;
	unsigned float_regs;
} Regs;



ABIArgInfo *c_abi_classify_return_type_default(ParamInfo param);
ABIArgInfo *c_abi_classify_argument_type_default(ParamInfo param);
void c_abi_func_create_win64(FunctionPrototype *prototype, ParamInfo *params, unsigned param_count, ParamInfo *vaargs, unsigned vaarg_count);
void c_abi_func_create_x86(FunctionPrototype *prototype, ParamInfo *params, unsigned param_count, ParamInfo *vaargs, unsigned vaarg_count);
void c_abi_func_create_x64(FunctionPrototype *prototype, ParamInfo *params, unsigned param_count, ParamInfo *vaargs, unsigned vaarg_count);
void c_abi_func_create_aarch64(FunctionPrototype *prototype, ParamInfo *params, unsigned param_count, ParamInfo *vaargs, unsigned vaarg_count);
void c_abi_func_create_riscv(FunctionPrototype *prototype, ParamInfo *params, unsigned param_count, ParamInfo *vaargs, unsigned vaarg_count);
void c_abi_func_create_wasm(FunctionPrototype *prototype, ParamInfo *params, unsigned param_count, ParamInfo *vaargs, unsigned vaarg_count);
void c_abi_func_create_default(FunctionPrototype *prototype, ParamInfo *params, unsigned param_count, ParamInfo *vaargs, unsigned vaarg_count);


static inline AbiType abi_type_get(Type *type)
{
	return (AbiType) { .type = type };
}

static inline AbiType abi_type_spec(AbiSpecType type)
{
	return (AbiType) { .abi_type = type };
}

static inline AbiType abi_type_get_int_bits(BitSize bits)
{
	switch (bits)
	{
		case 24:
			return (AbiType) { .abi_type = ABI_TYPE_INT_24 };
		case 40:
			return (AbiType) { .abi_type = ABI_TYPE_INT_40 };
		case 48:
			return (AbiType) { .abi_type = ABI_TYPE_INT_48 };
		case 56:
			return (AbiType) { .abi_type = ABI_TYPE_INT_56 };
		case 8:
		case 16:
		case 32:
		case 64:
		case 128:
			return (AbiType) { .type = type_int_unsigned_by_bitsize(bits) };
		default:
			UNREACHABLE_VOID;
			return (AbiType) { .type = NULL };
	}
}

static inline void abi_type_set_type(AbiType *abi_type, Type *type)
{
	abi_type->type = type;
}

