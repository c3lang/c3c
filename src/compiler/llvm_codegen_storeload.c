// Copyright (c) 2022 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_internal.h"

LLVMValueRef llvm_store(GenContext *context, LLVMValueRef pointer, LLVMValueRef value, AlignSize alignment)
{
	assert(alignment > 0);
	LLVMValueRef ref = LLVMBuildStore(context->builder, value, pointer);
	llvm_set_alignment(ref, alignment);
	return ref;
}

void llvm_store_raw_abi_alignment(GenContext *context, LLVMValueRef pointer, LLVMValueRef value, Type *type)
{
	llvm_store(context, pointer, value, type_abi_alignment(type));
}

void llvm_store_value_raw(GenContext *c, BEValue *destination, LLVMValueRef raw_value)
{
	assert(llvm_value_is_addr(destination));
	llvm_store(c, destination->value, raw_value, destination->alignment);
}

void llvm_store_decl_raw(GenContext *context, Decl *decl, LLVMValueRef value)
{
	assert(!decl->is_value);
	llvm_store(context, decl->backend_ref, value, decl->alignment);
}

void llvm_store_value_dest_aligned(GenContext *c, LLVMValueRef destination, BEValue *value)
{
	llvm_store_value_aligned(c, destination, value, LLVMGetAlignment(destination));
}

LLVMValueRef llvm_store_value_aligned(GenContext *c, LLVMValueRef destination, BEValue *value, AlignSize alignment)
{
	// If we have an address but not an aggregate, do a load.
	assert(alignment);
	llvm_value_fold_failable(c, value);
	if (value->kind == BE_ADDRESS && !type_is_abi_aggregate(value->type))
	{
		value->value = llvm_load_value_store(c, value);
		value->kind = BE_VALUE;
	}
	switch (value->kind)
	{
		case BE_BOOLEAN:
			value->value = LLVMBuildZExt(c->builder, value->value, c->byte_type, "");
			value->kind = BE_VALUE;
			FALLTHROUGH;
		case BE_VALUE:
			return llvm_store(c, destination, value->value, alignment);
		case BE_ADDRESS_FAILABLE:
			UNREACHABLE
		case BE_ADDRESS:
		{
			// Here we do an optimized(?) memcopy.
			ByteSize size = type_size(value->type);
			LLVMValueRef copy_size = llvm_const_int(c, size <= UINT32_MAX ? type_uint : type_usize, size);
			destination = LLVMBuildBitCast(c->builder, destination, llvm_get_ptr_type(c, type_char), "");
			LLVMValueRef source = LLVMBuildBitCast(c->builder, value->value, llvm_get_ptr_type(c, type_char), "");
			LLVMValueRef copy = LLVMBuildMemCpy(c->builder,
			                                    destination,
			                                    alignment,
			                                    source,
			                                    value->alignment ? value->alignment : type_abi_alignment(value->type),
			                                    copy_size);
			return copy;
		}
	}
	UNREACHABLE
}

LLVMValueRef llvm_store_value(GenContext *c, BEValue *destination, BEValue *value)
{
	if (value->type == type_void) return NULL;
	assert(llvm_value_is_addr(destination));
	return llvm_store_value_aligned(c, destination->value, value, destination->alignment);
}

LLVMValueRef llvm_load(GenContext *c, LLVMTypeRef type, LLVMValueRef pointer, AlignSize alignment, const char *name)
{
	assert(alignment > 0);
	assert(c->builder);
	assert(LLVMGetTypeContext(type) == c->context);
	LLVMValueRef value = LLVMBuildLoad2(c->builder, type, pointer, name);
	llvm_set_alignment(value, alignment ? alignment : llvm_abi_alignment(c, type));
	return value;
}

LLVMValueRef llvm_load_natural_alignment(GenContext *c, Type *type, LLVMValueRef pointer, const char *name)
{
	return llvm_load(c, llvm_get_type(c, type), pointer, type_abi_alignment(type), name);
}


LLVMValueRef llvm_load_value(GenContext *c, BEValue *value)
{
	llvm_value_fold_failable(c, value);
	switch (value->kind)
	{
		case BE_BOOLEAN:
		case BE_VALUE:
			return value->value;
		case BE_ADDRESS_FAILABLE:
			UNREACHABLE
		case BE_ADDRESS:
			return llvm_load(c, llvm_get_type(c, value->type), value->value, value->alignment, "");
	}
	UNREACHABLE
}

LLVMValueRef llvm_load_value_store(GenContext *c, BEValue *value)
{
	LLVMValueRef val = llvm_load_value(c, value);
	if (value->kind != BE_BOOLEAN) return val;
	return LLVMIsConstant(val) ? LLVMConstZExt(val, c->byte_type) : LLVMBuildZExt(c->builder, val, c->byte_type, "");
}


void llvm_store_zero(GenContext *c, BEValue *ref)
{
	llvm_value_addr(c, ref);
	Type *type = ref->type;
	if (!type_is_abi_aggregate(type))
	{
		llvm_store_value_raw(c, ref, llvm_get_zero(c, type));
		return;
	}
	Type *single_type = type_abi_find_single_struct_element(type);

	if (single_type && !type_is_abi_aggregate(single_type))
	{
		BEValue element;
		llvm_value_set_address(&element,
							   llvm_emit_bitcast(c, ref->value, type_get_ptr(single_type)),
							   single_type,
							   (unsigned)ref->alignment);
		llvm_store_zero(c, &element);
		return;
	}
	if (type_size(type) <= 16)
	{
		if (type->type_kind == TYPE_STRUCT)
		{
			Decl *decl = type->decl;
			Decl **members = decl->strukt.members;
			VECEACH(members, i)
			{
				if (!type_size(members[i]->type)) continue;
				BEValue member_ref;
				llvm_emit_struct_member_ref(c, ref, &member_ref, i);
				llvm_store_zero(c, &member_ref);
			}
			return;
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
			return;
		}
	}
	llvm_emit_memclear_size_align(c, ref->value, type_size(ref->type), ref->alignment);
}
