#include "llvm_codegen_internal.h"

void llvm_value_set(BEValue *value, LLVMValueRef llvm_value, Type *type)
{
	type = type_lowering(type);
	assert(llvm_value || type == type_void);
	value->value = llvm_value;
	value->alignment = type_abi_alignment(type);
	value->kind = BE_VALUE;
	value->type = type;
}

void llvm_value_set_address(BEValue *value, LLVMValueRef llvm_value, Type *type, AlignSize alignment)
{
	assert(alignment > 0);
	value->value = llvm_value;
	value->alignment = alignment;
	value->kind = BE_ADDRESS;
	value->type = type_lowering(type);
}

void llvm_value_set_address_abi_aligned(BEValue *value, LLVMValueRef llvm_value, Type *type)
{
	llvm_value_set_address(value, llvm_value, type_lowering(type), type_abi_alignment(type));
}

void llvm_value_addr(GenContext *c, BEValue *value)
{
	llvm_value_fold_failable(c, value);
	if (value->kind == BE_ADDRESS) return;
	if (!c->builder)
	{
		LLVMValueRef val = llvm_load_value_store(c, value);
		LLVMValueRef ref = LLVMAddGlobal(c->module, LLVMTypeOf(val), ".taddr");
		llvm_set_alignment(ref, llvm_abi_alignment(c, LLVMTypeOf(val)));
		llvm_set_private_linkage(ref);
		LLVMSetInitializer(ref, val);
		llvm_emit_bitcast(c, ref, type_get_ptr(value->type));
		llvm_value_set_address_abi_aligned(value, ref, value->type);
	}
	else
	{
		LLVMValueRef temp = llvm_emit_alloca_aligned(c, value->type, "taddr");
		llvm_store_value_dest_aligned(c, temp, value);
		llvm_value_set_address_abi_aligned(value, temp, value->type);
	}
}

void llvm_value_rvalue(GenContext *c, BEValue *value)
{
	if (value->kind != BE_ADDRESS && value->kind != BE_ADDRESS_FAILABLE)
	{
		if (value->type->type_kind == TYPE_BOOL && value->kind != BE_BOOLEAN)
		{
			value->value = LLVMBuildTrunc(c->builder, value->value, c->bool_type, "");
			value->kind = BE_BOOLEAN;
		}
		return;
	}
	llvm_value_fold_failable(c, value);
	value->value = llvm_load(c,
	                         llvm_get_type(c, value->type),
	                         value->value,
	                         value->alignment ? value->alignment : type_abi_alignment(value->type),
	                         "");
	if (value->type->type_kind == TYPE_BOOL)
	{
		value->value = LLVMBuildTrunc(c->builder, value->value, c->bool_type, "");
		value->kind = BE_BOOLEAN;
		return;
	}
	value->kind = BE_VALUE;
}

void llvm_value_fold_failable(GenContext *c, BEValue *value)
{
	if (value->kind == BE_ADDRESS_FAILABLE)
	{
		LLVMBasicBlockRef after_block = llvm_basic_block_new(c, "after_check");
		LLVMValueRef err_value = llvm_load_natural_alignment(c, type_anyerr, value->failable, "");
		LLVMValueRef was_ok = llvm_emit_is_no_error(c, err_value);
		if (c->error_var)
		{
			LLVMBasicBlockRef error_block = llvm_basic_block_new(c, "error");
			llvm_emit_cond_br_raw(c, was_ok, after_block, error_block);
			llvm_emit_block(c, error_block);
			llvm_store_raw_abi_alignment(c, c->error_var, err_value, type_anyerr);
			llvm_emit_br(c, c->catch_block);
		}
		else
		{
			assert(c->catch_block);
			llvm_emit_cond_br_raw(c, was_ok, after_block, c->catch_block);
		}
		llvm_emit_block(c, after_block);
		value->kind = BE_ADDRESS;
	}
}

void llvm_value_set_decl_address(BEValue *value, Decl *decl)
{
	decl = decl_flatten(decl);
	llvm_value_set_address(value, decl_ref(decl), decl->type, decl->alignment);

	if (decl->decl_kind == DECL_VAR && IS_FAILABLE(decl))
	{
		value->kind = BE_ADDRESS_FAILABLE;
		value->failable = decl->var.failable_ref;
	}
}
