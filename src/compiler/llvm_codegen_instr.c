// Copyright (c) 2022 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_internal.h"

void llvm_emit_cond_br_raw(GenContext *context, LLVMValueRef b, LLVMBasicBlockRef then_block,
                           LLVMBasicBlockRef else_block)
{
    assert(context->current_block);
    LLVMBuildCondBr(context->builder, b, then_block, else_block);
    LLVMClearInsertionPosition(context->builder);
    context->current_block = NULL;
    context->current_block_is_target = false;
}

void llvm_emit_cond_br(GenContext *context, BEValue *value, LLVMBasicBlockRef then_block, LLVMBasicBlockRef else_block)
{
    assert(context->current_block);
    assert(value->kind == BE_BOOLEAN);
    LLVMBuildCondBr(context->builder, value->value, then_block, else_block);
    LLVMClearInsertionPosition(context->builder);
    context->current_block = NULL;
    context->current_block_is_target = false;
}

LLVMValueRef llvm_emit_lshr_fixed(GenContext *c, LLVMValueRef data, int shift)
{
    assert(shift >= 0);
    if (shift == 0)
        return data;
    LLVMTypeRef type = LLVMTypeOf(data);
    BitSize bit_width = llvm_bitsize(c, type);
    if (shift >= bit_width)
        return LLVMConstNull(type);
    if (LLVMIsAConstant(data))
    {
        return LLVMConstLShr(data, LLVMConstInt(type, (unsigned)shift, false));
    }
    return LLVMBuildLShr(c->builder, data, LLVMConstInt(type, (unsigned)shift, false), "");
}

LLVMValueRef llvm_emit_shl_fixed(GenContext *c, LLVMValueRef data, int shift)
{
    assert(shift >= 0);
    if (shift == 0)
        return data;
    LLVMTypeRef type = LLVMTypeOf(data);
    BitSize bit_width = llvm_bitsize(c, type);
    if (shift >= bit_width)
        return LLVMConstNull(type);
    if (LLVMIsAConstant(data))
    {
        return LLVMConstShl(data, LLVMConstInt(type, (unsigned)shift, false));
    }
    return LLVMBuildShl(c->builder, data, LLVMConstInt(type, (unsigned)shift, false), "");
}
