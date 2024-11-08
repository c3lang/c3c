// Copyright (c) 2022 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_internal.h"

LLVMValueRef llvm_store_to_ptr_raw_aligned(GenContext *context, LLVMValueRef pointer, LLVMValueRef value, AlignSize alignment)
{
	ASSERT0(alignment > 0);
	LLVMValueRef ref = LLVMBuildStore(context->builder, value, pointer);
	llvm_set_alignment(ref, alignment);
	return ref;
}


void llvm_store_to_ptr_zero(GenContext *context, LLVMValueRef pointer, Type *type)
{
	llvm_store_to_ptr_raw_aligned(context, pointer, llvm_get_zero(context, type), type_abi_alignment(type));
}

bool llvm_temp_as_address(GenContext *c, Type *type)
{
	if (type_size(type) <= 2) return false;
	switch (type_lowering(type)->type_kind)
	{
		case TYPE_SLICE:
		case TYPE_ANY:
		case TYPE_INTERFACE:
			// Ok by value.
			return false;
		default:
			return type_is_abi_aggregate(type);
	}
}

LLVMValueRef llvm_store_to_ptr_aligned(GenContext *c, LLVMValueRef destination, BEValue *value, AlignSize alignment)
{
	// If we have an address but not an aggregate, do a load.
	ASSERT0(alignment);
	llvm_value_fold_optional(c, value);
	if (value->kind == BE_ADDRESS && !type_is_abi_aggregate(value->type))
	{
		value->value = llvm_load_value_store(c, value);
		value->kind = BE_VALUE;
	}
	switch (value->kind)
	{
		case BE_BOOLVECTOR:
			value->value = LLVMBuildSExt(c->builder, value->value, llvm_get_type(c, value->type), "");
			value->kind = BE_VALUE;
			return llvm_store_to_ptr_raw_aligned(c, destination, value->value, alignment);
		case BE_BOOLEAN:
			value->value = LLVMBuildZExt(c->builder, value->value, c->byte_type, "");
			value->kind = BE_VALUE;
			FALLTHROUGH;
		case BE_VALUE:
			return llvm_store_to_ptr_raw_aligned(c, destination, value->value, alignment);
		case BE_ADDRESS_OPTIONAL:
			UNREACHABLE
		case BE_ADDRESS:
			return llvm_emit_memcpy(c, destination, alignment, value->value, value->alignment ? value->alignment : type_abi_alignment(value->type), type_size(value->type));
	}
	UNREACHABLE
}

LLVMValueRef llvm_store(GenContext *c, BEValue *destination, BEValue *value)
{
	if (value->type == type_void) return NULL;
	ASSERT0(!type_is_void(value->type));
	ASSERT0(llvm_value_is_addr(destination));
	return llvm_store_to_ptr_aligned(c, destination->value, value, destination->alignment);
}

LLVMValueRef llvm_load(GenContext *c, LLVMTypeRef type, LLVMValueRef pointer, AlignSize alignment, const char *name)
{
	ASSERT0(alignment > 0);
	ASSERT0(!llvm_is_global_eval(c));
	ASSERT0(LLVMGetTypeContext(type) == c->context);
	LLVMValueRef value = LLVMBuildLoad2(c->builder, type, pointer, name);
	llvm_set_alignment(value, alignment ? alignment : llvm_abi_alignment(c, type));
	return value;
}

LLVMValueRef llvm_load_abi_alignment(GenContext *c, Type *type, LLVMValueRef pointer, const char *name)
{
	return llvm_load(c, llvm_get_type(c, type), pointer, type_abi_alignment(type), name);
}


LLVMValueRef llvm_load_value(GenContext *c, BEValue *value)
{
	llvm_value_fold_optional(c, value);
	switch (value->kind)
	{
		case BE_BOOLVECTOR:
		case BE_BOOLEAN:
		case BE_VALUE:
			return value->value;
		case BE_ADDRESS_OPTIONAL:
			UNREACHABLE
		case BE_ADDRESS:
			return llvm_load(c, llvm_get_type(c, value->type), value->value, value->alignment, "");
	}
	UNREACHABLE
}

LLVMValueRef llvm_load_value_store(GenContext *c, BEValue *value)
{
	LLVMValueRef val = llvm_load_value(c, value);
	if (value->kind == BE_BOOLVECTOR)
	{
		return LLVMBuildSExt(c->builder, val, llvm_get_type(c, type_get_vector_bool(value->type)), "");
	}
	if (value->kind != BE_BOOLEAN) return val;
	return LLVMBuildZExt(c->builder, val, c->byte_type, "");
}


LLVMValueRef llvm_store_zero(GenContext *c, BEValue *ref)
{
	llvm_value_addr(c, ref);
	Type *type = ref->type;
	if (!type_is_abi_aggregate(type) || type_is_builtin(type->type_kind))
	{
		return llvm_store_raw(c, ref, llvm_get_zero(c, type));
	}
	Type *single_type = type_abi_find_single_struct_element(type);

	if (single_type && !type_is_abi_aggregate(single_type))
	{
		BEValue element = *ref;
		llvm_value_bitcast(c, &element, single_type);
		return llvm_store_zero(c, &element);
	}
	if (type_size(type) <= 16)
	{
		if (type->type_kind == TYPE_STRUCT)
		{
			Decl *decl = type->decl;
			FOREACH_IDX(i, Decl *, member, decl->strukt.members)
			{
				if (!type_size(member->type)) continue;
				BEValue member_ref;
				llvm_emit_struct_member_ref(c, ref, &member_ref, i);
				llvm_store_zero(c, &member_ref);
			}
			return NULL;
		}
		if (type->type_kind == TYPE_ARRAY)
		{
			LLVMTypeRef array_type = llvm_get_type(c, type);
			for (unsigned i = 0; i < type->array.len; i++)
			{
				AlignSize align;
				LLVMValueRef element_ptr = llvm_emit_array_gep_raw(c, ref->value, array_type, i, ref->alignment, &align);
				BEValue be_value;
				llvm_value_set_address(&be_value, element_ptr, type->array.base, align);
				llvm_store_zero(c, &be_value);
			}
			return NULL;
		}
	}
	return llvm_emit_memclear_size_align(c, ref->value, type_size(ref->type), ref->alignment);
}
