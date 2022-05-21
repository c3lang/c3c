#include "tilde_internal.h"

#if TB_BACKEND

void value_set(TBEValue *value, TB_Reg val, Type *type)
{
	type = type_lowering(type);
	assert(!val || type == type_void);
	value->reg = val;
	value->alignment = type_abi_alignment(type);
	value->kind = TBE_VALUE;
	value->type = type;
}

void value_set_address(TBEValue *value, TB_Reg addr, Type *type, AlignSize alignment)
{
	value->reg = addr;
	value->alignment = alignment;
	value->kind = TBE_ADDRESS;
	value->type = type_lowering(type);
}

void value_set_address_abi_aligned(TBEValue *value, TB_Reg val, Type *type)
{
	value_set_address(value, val, type, type_abi_alignment(type));
}

void value_addr(TbContext *c, TBEValue *value)
{
	value_fold_failable(c, value);
	if (value->kind == TBE_ADDRESS) return;
	if (!c->f)
	{
		TODO
		// TB_Register val = value_rvalue_get(c, value);
		// TODO check whether new names must be added
		TB_Register val = tb_global_create(c->module, ".taddr", TB_STORAGE_DATA, TB_LINKAGE_PRIVATE);
		TB_InitializerID initializer_id = tb_initializer_create(c->module, type_size(value->type),
		                                                        type_alloca_alignment(value->type), 0);
		tb_global_set_initializer(c->module, val, initializer_id);
		TODO
		// TODO set linkage
		/*
		llvm_set_private_linkage(ref);
		llvm_emit_bitcast(c, ref, type_get_ptr(value->type));
		llvm_value_set_address(value, ref, value->type);*/
	}
	else
	{
		TODO
		/*
		 *
		tilde_emit_alloca_aligned(c, value->type);
		TB_Register reg = llvm_emit_alloca_aligned(c, value->type, "taddr");
		tilde_store_bevalue_dest_aligned(c, temp, value);
		llvm_value_set_address(value, temp, value->type);*/
	}
}

void value_rvalue(TbContext *c, TBEValue *value)
{
	if (value->kind == TBE_VALUE) return;
	value_fold_failable(c, value);
	value->reg = tilde_load_value(c, value);
	value->kind = TBE_VALUE;
}

void value_fold_failable(TbContext *c, TBEValue *value)
{
	if (value->kind != TBE_ADDRESS_FAILABLE) return;
	TB_Label after_block = tb_inst_new_label_id(c->f);
	TB_Reg error_val = tilde_load_abi_alignment(c, type_anyerr, value->failable);
	TB_Reg comp = tilde_emit_is_no_error(c, error_val);
	assert(c->catch_block);
	if (c->error_var)
	{
		TB_Label err_block = tb_inst_new_label_id(c->f);
		tb_inst_if(c->f, comp, after_block, err_block);
		tb_inst_label(c->f, err_block);
		tilde_store_raw_abi_alignment(c, type_anyerr, c->error_var, error_val);
		tb_inst_goto(c->f, c->catch_block);
	}
	else
	{
		tb_inst_if(c->f, comp, after_block, c->catch_block);
	}
	tb_inst_label(c->f, after_block);
	value->kind = TBE_ADDRESS;
}

void value_set_decl(TBEValue *value, Decl *decl)
{
	decl = decl_flatten(decl);
	value_set_address(value, decl_reg(decl), decl->type, decl->alignment);

	if (decl->decl_kind == DECL_VAR && IS_FAILABLE(decl))
	{
		value->kind = TBE_ADDRESS_FAILABLE;
		value->failable = decl->var.tb_failable_reg;
	}
}


#endif