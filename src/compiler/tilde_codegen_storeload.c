// Copyright (c) 2022 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "tilde_internal.h"



void tilde_store_to_ptr_raw_aligned(TildeContext *c, Type *type, TB_Reg addr, TB_Reg value, AlignSize alignment)
{
	tilde_store_internal(c, tildetype(type), addr, value, alignment);
}

void tilde_store_to_ptr_raw(TildeContext *c, TB_Reg addr, TB_Reg value, Type *type)
{
	tilde_store_internal(c, tildetype(type), addr, value, type_abi_alignment(type));
}


void tilde_store_value_raw(TildeContext *c, TBEValue *destination, TB_Reg value)
{
	assert(value_is_addr(destination));
	tilde_store_internal(c, tildetype(destination->type), destination->reg, value, destination->alignment);
}


void tilde_store_decl_raw(TildeContext *c, Decl *decl, TB_Reg value)
{
	assert(!decl->is_value);
	tilde_store_internal(c, tildetype(decl->type), decl->tb_register, value, decl->alignment);
}

TB_Reg tilde_load_value_store(TildeContext *c, TBEValue *value)
{
	TB_Reg val = tilde_load_value(c, value);
	return val;
	/*
	if (value->kind == BE_BOOLVECTOR)
	{
		return LLVMBuildSExt(c->builder, val, llvm_get_type(c, type_get_vector_bool(value->type)), "");
	}
	if (value->kind != BE_BOOLEAN) return val;
	return LLVMBuildZExt(c->builder, val, c->byte_type, "");*/
}

void tilde_store_to_ptr_aligned(TildeContext *c, TB_Reg destination, TBEValue *value, AlignSize alignment)
{
	// If we have an address but not an aggregate, do a load.
	assert(alignment);
	value_fold_optional(c, value);
	if (value->kind == TBE_ADDRESS && !type_is_abi_aggregate(value->type))
	{
		value->reg = tilde_load_value_store(c, value);
		value->kind = TBE_VALUE;
	}
	switch (value->kind)
	{
		/*
		case BE_BOOLVECTOR:
			value->value = LLVMBuildSExt(c->builder, value->value, llvm_get_type(c, value->type), "");
			value->kind = BE_VALUE;
			return llvm_store_to_ptr_raw_aligned(c, destination, value->value, alignment);
		case BE_BOOLEAN:
			value->value = LLVMBuildZExt(c->builder, value->value, c->byte_type, "");
			value->kind = BE_VALUE;
					FALLTHROUGH;*/
		case TBE_VALUE:
			tilde_store_to_ptr_raw_aligned(c, value->type, destination, value->reg, alignment);
			return;
		case TBE_ADDRESS_OPTIONAL:
			UNREACHABLE
		case TBE_ADDRESS:
			tb_inst_memcpy(c->f, destination, value->reg, tb_inst_uint(c->f, TB_TYPE_I32, type_size(value->type)), alignment);
			return;
	}
	UNREACHABLE
}

void tilde_store_value_aligned(TildeContext *c, TB_Reg destination, TBEValue *value, AlignSize alignment)
{
	assert(alignment);
	value_fold_optional(c, value);
	// If we have an address but not an aggregate, do a load.
	if (value->kind == TBE_ADDRESS && !type_is_abi_aggregate(value->type))
	{
		value_rvalue(c, value);
	}
	switch (value->kind)
	{
		case TBE_VALUE:
			tilde_store_value_raw(c, value, value->reg);
			return;
		case TBE_ADDRESS_OPTIONAL:
			UNREACHABLE
		case TBE_ADDRESS:
		{
			ByteSize size = type_size(value->type);
			TB_Reg copy_size = tb_inst_uint(c->f, size <= UINT32_MAX ? TB_TYPE_I32 : TB_TYPE_I64, size);
			tb_inst_memcpy(c->f, destination, value->reg, copy_size, type_min_alignment(alignment, value->alignment));
		}
	}
	UNREACHABLE
}

void tilde_store(TildeContext *c, TBEValue *dst, TBEValue *value)
{
	if (value->type == type_void) return;
	assert(value_is_addr(dst));
	tilde_store_to_ptr_aligned(c, dst->reg, value, dst->alignment);
}

TB_Reg tilde_load_abi_alignment(TildeContext *c, Type *type, TB_Reg pointer)
{
	return tilde_load(c, tildetype(type), pointer, type_abi_alignment(type));
}

TB_Reg tilde_load_value(TildeContext *c, TBEValue *value)
{
	value_fold_optional(c, value);
	switch (value->kind)
	{
		case TBE_VALUE:
			return value->reg;
		case TBE_ADDRESS_OPTIONAL:
			UNREACHABLE
		case TBE_ADDRESS:
			return tilde_load(c, tildetype(value->type), value->reg, value->alignment);
	}
	UNREACHABLE
}

void tilde_store_zero(TildeContext *c, Type *type, TB_Reg addr, AlignSize alignment)
{
	type = type_lowering(type);
	if (alignment == 0) alignment = type_alloca_alignment(type);
	if (type_is_builtin(type->type_kind) || type->type_kind == TYPE_POINTER)
	{
		tilde_store_to_ptr_raw_aligned(c, type, addr, tilde_get_zero(c, type), alignment);
		return;
	}
	ByteSize size = type_size(type);
	ByteSize min = type_min_alignment(alignment, size);
	TB_Register zero = tb_inst_uint(c->f, TB_TYPE_I8, 0);
	TB_Register elements = tb_inst_uint(c->f, tildetype(type_usz), size);
	tb_inst_memset(c->f, addr, zero, elements, min);
}

void tilde_store_value_zero(TildeContext *c, TBEValue *to)
{
	assert(to->kind == TBE_ADDRESS);
	tilde_store_zero(c, to->type, to->reg, to->alignment);
}


void tilde_emit_and_set_decl_alloca(TildeContext *c, Decl *decl)
{
	Type *type = type_lowering(decl->type);
	if (type == type_void) return;
	decl->tb_register = tilde_emit_alloca(c, type, decl->alignment);
}
