#include "tilde_internal.h"

#if TB_BACKEND

void tilde_emit_memclear_size_align(TbContext *c, TB_Register ref, uint64_t size, AlignSize align)
{
	ByteSize min = type_min_alignment(align, size);
	TB_Register zero = tb_inst_iconst(c->f, TB_TYPE_I8, 0);
	TB_Register elements = tb_inst_iconst(c->f, size <= UINT32_MAX ? TB_TYPE_I32 : TB_TYPE_I64, size);
	tb_inst_memset(c->f, ref, zero, elements, min);
}

void tilde_emit_cond_br(TbContext *c, TBEValue *value, TB_Label then_block, TB_Label else_block)
{
	tb_inst_if(c->f, tilde_load_value(c, value), then_block, else_block);
}

TB_Reg tilde_emit_shl_fixed(TbContext *c, Type *type, TB_Reg reg, int shift)
{
	assert(shift >= 0);
	if (shift == 0) return reg;
	BitSize bit_width = type_kind_bitsize(type->type_kind);
	if (shift >= bit_width) return tilde_get_zero(c, type);
	TB_DataType int_type = tbtype(type);
	return tb_inst_shl(c->f, int_type, reg, tb_inst_iconst(c->f, int_type, (unsigned)shift), type_is_signed(type) ? TB_ASSUME_NSW : TB_ASSUME_NUW);
}

TB_Reg tilde_emit_lshr_fixed(TbContext *c, Type *type, TB_Reg reg, int shift)
{
	assert(shift >= 0);
	if (shift == 0) return reg;
	BitSize bit_width = type_kind_bitsize(type->type_kind);
	if (shift >= bit_width) return tilde_get_zero(c, type);
	TB_DataType int_type = tbtype(type);
	return tb_inst_shr(c->f, int_type, reg, tb_inst_iconst(c->f, int_type, (unsigned)shift));
}

TB_Reg tilde_emit_alloca(TbContext *c, Type *type)
{
	return tb_inst_local(c->f, type_size(type), type_alloca_alignment(type));
}

#endif