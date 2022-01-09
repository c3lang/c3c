// Copyright (c) 2022 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "tilde_internal.h"

#if TB_BACKEND

void tilde_store_raw(TbContext *c, Type *type, TB_Reg addr, TB_Reg value, AlignSize alignment)
{
	tilde_store(c, tbtype(type), addr, value, alignment);
}

void tilde_store_raw_abi_alignment(TbContext *c, Type *type, TB_Reg addr, TB_Reg value)
{
	tilde_store(c, tbtype(type), addr, value, type_abi_alignment(type));
}

void tilde_store_value_raw(TbContext *c, TBEValue *destination, TB_Reg value)
{
	assert(value_is_addr(destination));
	tilde_store(c, tbtype(destination->type), destination->reg, value, destination->alignment);
}


void tilde_store_decl_raw(TbContext *c, Decl *decl, TB_Reg value)
{
	assert(!decl->is_value);
	tilde_store(c, tbtype(decl->type), decl->tb_register, value, decl->alignment);
}

void tilde_store_value_aligned(TbContext *c, TB_Reg destination, TBEValue *value, AlignSize alignment)
{
	assert(alignment);
	value_fold_failable(c, value);
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
		case TBE_ADDRESS_FAILABLE:
			UNREACHABLE
		case TBE_ADDRESS:
		{
			ByteSize size = type_size(value->type);
			TB_Reg copy_size = tb_inst_iconst(c->f, size <= UINT32_MAX ? TB_TYPE_I32 : TB_TYPE_I64, size);
			tb_inst_memcpy(c->f, destination, value->reg, copy_size, type_min_alignment(alignment, value->alignment));
		}
	}
	UNREACHABLE
}

void tilde_store_value(TbContext *c, TBEValue *dst, TBEValue *value)
{
	assert(dst->kind == TBE_ADDRESS);
	tilde_store_value_aligned(c, dst->reg, value, dst->alignment);
}

TB_Reg tilde_load_abi_alignment(TbContext *c, Type *type, TB_Reg pointer)
{
	return tilde_load(c, tbtype(type), pointer, type_abi_alignment(type));
}

TB_Reg tilde_load_value(TbContext *c, TBEValue *value)
{
	value_fold_failable(c, value);
	switch (value->kind)
	{
		case TBE_VALUE:
			return value->reg;
		case TBE_ADDRESS_FAILABLE:
			UNREACHABLE
		case TBE_ADDRESS:
			return tilde_load(c, tbtype(value->type), value->reg, value->alignment);
	}
	UNREACHABLE
}

void tilde_store_zero(TbContext *c, Type *type, TB_Reg addr, AlignSize alignment)
{
	type = type_lowering(type);
	if (type_is_builtin(type->type_kind) || type->type_kind == TYPE_POINTER)
	{
		tilde_store_raw(c, type, addr, tilde_get_zero(c, type), alignment);
		return;
	}
	ByteSize size = type_size(type);
	ByteSize min = type_min_alignment(alignment, size);
	TB_Register zero = tb_inst_iconst(c->f, TB_TYPE_I8, 0);
	TB_Register elements = tb_inst_iconst(c->f, tbtype(type_usize), size);
	tb_inst_memset(c->f, addr, zero, elements, min);
}

void tilde_store_value_zero(TbContext *c, TBEValue *to)
{
	assert(to->kind == TBE_ADDRESS);
	tilde_store_zero(c, to->type, to->reg, to->alignment);
}

#endif
