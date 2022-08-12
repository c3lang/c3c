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
	llvm_value_fold_optional(c, value);
	if (value->kind == BE_ADDRESS) return;
	if (llvm_is_global_eval(c))
	{
		LLVMValueRef val = llvm_load_value_store(c, value);
		LLVMValueRef ref = llvm_add_global_raw(c, ".taddr", LLVMTypeOf(val), 0);
		llvm_set_private_linkage(ref);
		LLVMSetInitializer(ref, val);
		llvm_value_set_address_abi_aligned(value, llvm_emit_bitcast_ptr(c, ref, value->type), value->type);
	}
	else
	{
		LLVMValueRef temp = llvm_emit_alloca_aligned(c, value->type, "taddr");
		llvm_store_to_ptr(c, temp, value);
		llvm_value_set_address_abi_aligned(value, temp, value->type);
	}
}

void llvm_value_rvalue(GenContext *c, BEValue *value)
{
	if (!llvm_value_is_addr(value))
	{
		if (value->type->type_kind == TYPE_BOOL && value->kind != BE_BOOLEAN)
		{
			value->value = LLVMBuildTrunc(c->builder, value->value, c->bool_type, "");
			value->kind = BE_BOOLEAN;
		}
		return;
	}
	llvm_value_fold_optional(c, value);
	value->value = llvm_load(c,
	                         llvm_get_type(c, value->type),
	                         value->value,
	                         value->alignment ? value->alignment : type_abi_alignment(value->type),
	                         "");
	if (value->type->type_kind == TYPE_BOOL)
	{
		value->value = llvm_emit_trunc_bool(c, value->value);
		value->kind = BE_BOOLEAN;
		return;
	}
	value->kind = BE_VALUE;
}

void llvm_emit_jump_to_optional_exit(GenContext *c, LLVMValueRef err_value)
{
	assert(c->catch_block && "unexpected emit");
	bool is_constant_err = LLVMIsConstant(err_value);

	// Maybe we don't need to emit anything?
	if (is_constant_err && LLVMIsNull(err_value)) return;

	LLVMBasicBlockRef after_block = llvm_basic_block_new(c, "after_check");
	// No error variable
	if (!c->opt_var)
	{
		// No error var and a constant error means jumping to the "catch" block
		if (is_constant_err)
		{
			llvm_emit_br(c, c->catch_block);
		}
		else
		{
			llvm_emit_cond_br_raw(c, llvm_emit_is_no_opt(c, err_value), after_block, c->catch_block);
		}
		llvm_emit_block(c, after_block);
		return;
	}

	// If it's not a constant, then jump conditionally
	if (!is_constant_err)
	{
		LLVMValueRef was_ok = llvm_emit_is_no_opt(c, err_value);
		LLVMBasicBlockRef error_block = llvm_basic_block_new(c, "assign_optional");
		llvm_emit_cond_br_raw(c, was_ok, after_block, error_block);
		llvm_emit_block(c, error_block);
	}

	llvm_store_to_ptr_raw(c, c->opt_var, err_value, type_anyerr);
	llvm_emit_br(c, c->catch_block);
	llvm_emit_block(c, after_block);
}

void llvm_value_fold_optional(GenContext *c, BEValue *value)
{
	if (value->kind == BE_ADDRESS_FAILABLE)
	{
		llvm_emit_jump_to_optional_exit(c, llvm_load_natural_alignment(c, type_anyerr, value->failable, "optval"));
		value->kind = BE_ADDRESS;
	}
}

void llvm_value_set_decl_address(GenContext *c, BEValue *value, Decl *decl)
{
	LLVMValueRef backend_ref = llvm_get_ref(c, decl);
	llvm_value_set_address(value, backend_ref, decl->type, decl->alignment);

	if ((value->failable = llvm_get_opt_ref(c, decl)))
	{
		value->kind = BE_ADDRESS_FAILABLE;
	}
}
