#include "llvm_codegen_internal.h"

void llvm_value_deref(GenContext *c, BEValue *value)
{
	llvm_value_rvalue(c, value);
	value->kind = BE_ADDRESS;
	Type *type = value->type = type_lowering(type_get_indexed_type(value->type));
	value->alignment = type_abi_alignment(type);
}

void llvm_value_set(BEValue *value, LLVMValueRef llvm_value, Type *type)
{
	type = type_lowering(type);
	ASSERT0(llvm_value || type == type_void);
	value->value = llvm_value;
	value->alignment = type_abi_alignment(type);
	value->kind = BE_VALUE;
	value->type = type;

	if (type == type_bool)
	{
		LLVMTypeRef llvm_type = LLVMTypeOf(llvm_value);
		LLVMContextRef context = LLVMGetTypeContext(llvm_type);
		if (llvm_type == LLVMIntTypeInContext(context, 1))
		{
			value->kind = BE_BOOLEAN;
		}
	}
	if (type_kind_is_any_vector(type->type_kind) && type->array.base == type_bool)
	{
		LLVMTypeRef llvm_type = LLVMTypeOf(llvm_value);
		LLVMTypeRef element = LLVMGetElementType(llvm_type);
		LLVMContextRef context = LLVMGetTypeContext(llvm_type);
		if (element == LLVMIntTypeInContext(context, 1))
		{
			value->kind = BE_BOOLVECTOR;
		}
	}
}

void llvm_value_set_address(BEValue *value, LLVMValueRef llvm_value, Type *type, AlignSize alignment)
{
	ASSERT0(alignment > 0);
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
		llvm_set_private_declaration(ref);
		LLVMSetInitializer(ref, val);
		llvm_value_set_address_abi_aligned(value, ref, value->type);
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
			value->value = llvm_emit_trunc_bool(c, value->value);
			value->kind = BE_BOOLEAN;
		}
		if (type_flat_is_bool_vector(value->type) && value->kind != BE_BOOLVECTOR)
		{
			value->value = llvm_emit_trunc_bool(c, value->value);
			value->kind = BE_BOOLVECTOR;
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
	if (type_flat_is_bool_vector(value->type))
	{
		value->value = llvm_emit_trunc_bool(c, value->value);
		value->kind = BE_BOOLVECTOR;
		return;
	}
	value->kind = BE_VALUE;
}

void llvm_emit_jump_to_optional_exit(GenContext *c, LLVMValueRef opt_value)
{
	ASSERT0(c->catch.block && "unexpected emit");
	bool is_constant_opt = llvm_is_const(opt_value);

	// Maybe we don't need to emit anything?
	if (is_constant_opt && llvm_is_const_null(opt_value)) return;

	LLVMBasicBlockRef after_block = llvm_basic_block_new(c, "after_check");
	// No error variable
	if (!c->catch.fault)
	{
		// No error var and a constant error means jumping to the "catch" block
		if (is_constant_opt)
		{
			llvm_emit_br(c, c->catch.block);
		}
		else
		{
			llvm_emit_cond_br_raw(c, llvm_emit_is_no_opt(c, opt_value), after_block, c->catch.block);
		}
		llvm_emit_block(c, after_block);
		return;
	}

	// If it's not a constant, then jump conditionally
	if (!is_constant_opt)
	{
		LLVMValueRef was_ok = llvm_emit_is_no_opt(c, opt_value);
		LLVMBasicBlockRef error_block = llvm_basic_block_new(c, "assign_optional");
		llvm_emit_cond_br_raw(c, was_ok, after_block, error_block);
		llvm_emit_block(c, error_block);
	}

	llvm_store_to_ptr_raw(c, c->catch.fault, opt_value, type_anyfault);
	llvm_emit_br(c, c->catch.block);
	llvm_emit_block(c, after_block);
}

void llvm_value_fold_optional(GenContext *c, BEValue *value)
{
	if (value->kind == BE_ADDRESS_OPTIONAL)
	{
		llvm_emit_jump_to_optional_exit(c, llvm_load_abi_alignment(c, type_anyfault, value->optional, "optval"));
		value->kind = BE_ADDRESS;
	}
}

void llvm_value_set_decl_address(GenContext *c, BEValue *value, Decl *decl)
{
	ASSERT0(!decl->is_value);
	LLVMValueRef backend_ref = llvm_get_ref(c, decl);
	llvm_value_set_address(value, backend_ref, decl->type, decl->alignment);

	if ((value->optional = llvm_get_opt_ref(c, decl)))
	{
		value->kind = BE_ADDRESS_OPTIONAL;
	}
}
