// Copyright (c) 2022 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_internal.h"

void llvm_emit_cond_br_raw(GenContext *context, LLVMValueRef b, LLVMBasicBlockRef then_block, LLVMBasicBlockRef else_block)
{
	ASSERT0(context->current_block);
	LLVMBuildCondBr(context->builder, b, then_block, else_block);
	context->current_block = NULL;
}

void llvm_emit_cond_br(GenContext *context, BEValue *value, LLVMBasicBlockRef then_block, LLVMBasicBlockRef else_block)
{
	ASSERT0(context->current_block);
	ASSERT0(value->kind == BE_BOOLEAN);
	LLVMBuildCondBr(context->builder, value->value, then_block, else_block);
	context->current_block = NULL;
}

LLVMValueRef llvm_emit_lshr_fixed(GenContext *c, LLVMValueRef data, int shift)
{
	ASSERT0(shift >= 0);
	if (shift == 0) return data;
	LLVMTypeRef type = LLVMTypeOf(data);
	BitSize bit_width = llvm_bitsize(c, type);
	if (shift >= bit_width) return llvm_get_zero_raw(type);
	return llvm_emit_lshr(c, data, LLVMConstInt(type, (unsigned)shift, false));
}

LLVMValueRef llvm_emit_ashr_fixed(GenContext *c, LLVMValueRef data, int shift)
{
	ASSERT0(shift >= 0);
	if (shift == 0) return data;
	LLVMTypeRef type = LLVMTypeOf(data);
	BitSize bit_width = llvm_bitsize(c, type);
	if (shift >= bit_width) shift = (int)bit_width;
	return llvm_emit_ashr(c, data, LLVMConstInt(type, (unsigned)shift, false));
}

LLVMValueRef llvm_emit_shl_fixed(GenContext *c, LLVMValueRef data, int shift)
{
	ASSERT0(shift >= 0);
	if (shift == 0) return data;
	LLVMTypeRef type = LLVMTypeOf(data);
	BitSize bit_width = llvm_bitsize(c, type);
	if (shift >= bit_width) return llvm_get_zero_raw(type);
	return llvm_emit_shl(c, data, LLVMConstInt(type, (unsigned)shift, false));
}

LLVMAtomicOrdering llvm_atomic_ordering(Atomicity atomicity)
{
	switch (atomicity)
	{
		case ATOMIC_NONE: return LLVMAtomicOrderingNotAtomic;
		case ATOMIC_UNORDERED: return LLVMAtomicOrderingUnordered;
		case ATOMIC_RELAXED: return LLVMAtomicOrderingMonotonic;
		case ATOMIC_ACQUIRE: return LLVMAtomicOrderingAcquire;
		case ATOMIC_RELEASE: return LLVMAtomicOrderingRelease;
		case ATOMIC_ACQUIRE_RELEASE: return LLVMAtomicOrderingAcquireRelease;
		case ATOMIC_SEQ_CONSISTENT: return LLVMAtomicOrderingSequentiallyConsistent;
	}
	UNREACHABLE
}
