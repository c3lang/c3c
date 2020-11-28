#pragma once
// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_internal.h"

typedef enum
{
	WIN32_CDECL,
	WIN32_VEC,
	WIN32_STD,
	WIN32_SYS,
	UNIX_CDECL,
	WIN64_CC,
	AMD64_CC,
} CABIKind;

typedef enum
{
	BY_VAL,
	BY_VAL_SKIP
} ByVal;

static inline ABIArgInfo *abi_arg_by_reg_attr(ABIArgInfo *info);
size_t abi_arg_expanded_size(ABIArgInfo *type_info, Type *type);
bool abi_arg_is_indirect(ABIArgInfo *info);
ABIArgInfo *abi_arg_new(ABIKind kind);
ABIArgInfo *abi_arg_new_direct_pair(AbiType *low_type, AbiType *high_type);
ABIArgInfo *abi_arg_new_direct(void);
ABIArgInfo *abi_arg_new_direct_int_ext(Type *type_to_extend);
ABIArgInfo *abi_arg_new_direct_coerce(AbiType *target_type);
ABIArgInfo *abi_arg_new_expand_coerce(AbiType *target_type, unsigned offset);
ABIArgInfo *abi_arg_new_expand_coerce_pair(AbiType *first_element, unsigned initial_offset, AbiType *second_element, unsigned padding, bool is_packed);
ABIArgInfo *abi_arg_new_expand_padded(Type *padding);
ABIArgInfo *abi_arg_new_indirect_realigned(unsigned alignment);
ABIArgInfo *abi_arg_new_indirect_by_val(void);
ABIArgInfo *abi_arg_new_indirect_not_by_val(void);

size_t abi_type_abi_alignment(AbiType *type);
bool abi_type_is_integer(AbiType *type);
bool abi_type_is_float(AbiType *type);
AbiType *abi_type_new_plain(Type *type);
AbiType *abi_type_new_int_bits(unsigned bits);
size_t abi_type_size(AbiType *type);

ABIArgInfo *c_abi_classify_argument_type_default(Type *type);
void c_abi_func_create_win64(GenContext *context, FunctionSignature *signature);
void c_abi_func_create_x86(GenContext *context, FunctionSignature *signature);
void c_abi_func_create_x64(GenContext *context, FunctionSignature *signature);
void c_abi_func_create_aarch64(FunctionSignature *signature);
void c_abi_func_create_riscv(GenContext *context, FunctionSignature *signature);

// Implementation
static inline ABIArgInfo *abi_arg_by_reg_attr(ABIArgInfo *info)
{
	info->attributes.by_reg = true;
	return info;
}

