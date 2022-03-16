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
ABIArgInfo *abi_arg_new_direct_pair(AbiType low_type, AbiType high_type);
ABIArgInfo *abi_arg_new_direct(void);
ABIArgInfo *abi_arg_new_direct_by_reg(bool by_reg);
ABIArgInfo *abi_arg_new_expand(void);
ABIArgInfo *abi_arg_new_direct_int_ext(Type *type_to_extend);
ABIArgInfo *abi_arg_new_direct_int_ext_by_reg(Type *int_to_extend, bool by_reg);
ABIArgInfo *abi_arg_new_direct_coerce_int_ext_by_reg(Type *int_to_extend, bool by_reg);
ABIArgInfo *abi_arg_new_direct_coerce_int_ext(Type *int_to_extend);
ABIArgInfo *abi_arg_new_direct_coerce_int(void);
ABIArgInfo *abi_arg_new_direct_coerce_type(Type *type);
ABIArgInfo *abi_arg_new_direct_struct_expand(Type *type, int8_t elements);
ABIArgInfo *abi_arg_new_expand_coerce(AbiType target_type, unsigned offset);
ABIArgInfo *abi_arg_new_expand_coerce_pair(AbiType first_element, unsigned initial_offset, AbiType second_element,
                                           unsigned padding, bool is_packed);
ABIArgInfo *abi_arg_new_expand_padded(Type *padding);
ABIArgInfo *abi_arg_new_indirect_realigned(AlignSize alignment, Type *by_val_type);
ABIArgInfo *abi_arg_new_indirect_by_val(Type *by_val_type);
ABIArgInfo *abi_arg_new_indirect_not_by_val(Type *type);

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

ABIArgInfo *c_abi_classify_return_type_default(Type *type);
ABIArgInfo *c_abi_classify_argument_type_default(Type *type);
void c_abi_func_create_win64(FunctionPrototype *prototype);
void c_abi_func_create_x86(FunctionPrototype *prototype);
void c_abi_func_create_x64(FunctionPrototype *prototype);
void c_abi_func_create_aarch64(FunctionPrototype *prototype);
void c_abi_func_create_riscv(FunctionPrototype *prototype);
void c_abi_func_create_wasm(FunctionPrototype *prototype);

static inline AbiType abi_type_get(Type *type)
{
    return (AbiType){.type = type};
}

static inline AbiType abi_type_get_int_bits(BitSize bits)
{
    switch (bits)
    {
    case 8:
    case 16:
    case 32:
    case 64:
        return (AbiType){.type = type_int_unsigned_by_bitsize(bits)};
    default:
        return (AbiType){.int_bits_plus_1 = bits + 1};
    }
}

static inline void abi_type_set_type(AbiType *abi_type, Type *type)
{
    abi_type->type = type;
}
