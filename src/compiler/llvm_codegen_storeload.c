// Copyright (c) 2022-2025 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_internal.h"

INLINE bool llvm_should_expand_vector_store(GenContext *c, ArrayIndex len, LLVMValueRef pointer, AlignSize alignment, LLVMTypeRef type)
{
	ByteSize size = llvm_store_size(c, type);
	return !is_power_of_two(len)
		&& LLVMIsAAllocaInst(pointer)
		&& LLVMGetTypeKind(LLVMGetAllocatedType(pointer)) == LLVMVectorTypeKind
		&& size < aligned_offset(size, alignment);
}

static bool llvm_is_small_integer_struct(GenContext *c, LLVMTypeRef strukt, unsigned member_count)
{
	if (member_count > 4) return false;
	for (unsigned i = 0; i < member_count; i++)
	{
		LLVMTypeRef field_type = LLVMStructGetTypeAtIndex(strukt, i);
		LLVMTypeKind kind = LLVMGetTypeKind(field_type);
		if (kind != LLVMPointerTypeKind
			&& (kind != LLVMIntegerTypeKind || field_type == c->bool_type)) return false;
	}
	return true;
}

LLVMValueRef llvm_store_split_fields(GenContext *c, LLVMValueRef pointer, LLVMValueRef value, AlignSize alignment)
{
	LLVMTypeRef type = LLVMTypeOf(value);
	if (LLVMGetTypeKind(type) != LLVMStructTypeKind) return NULL;
	unsigned count = LLVMCountStructElementTypes(type);
	if (!llvm_is_small_integer_struct(c, type, count)) return NULL;
	LLVMValueRef fields[4] = { NULL };
	if (LLVMIsConstant(value))
	{
		for (unsigned i = 0; i < count; i++)
		{
			if (!(fields[i] = LLVMGetAggregateElement(value, i))) return NULL;
		}
	}
	else
	{
		LLVMValueRef base = value;
		while (LLVMIsAInsertValueInst(base))
		{
			if (LLVMGetNumIndices(base) != 1) return NULL;
			unsigned index = LLVMGetIndices(base)[0];
			if (!fields[index]) fields[index] = LLVMGetOperand(base, 1);
			base = LLVMGetOperand(base, 0);
		}
		if (!LLVMIsUndef(base) && !LLVMIsPoison(base)) return NULL;
	}
	LLVMValueRef store = NULL;
	for (unsigned i = 0; i < count; i++)
	{
		if (!fields[i] || LLVMIsUndef(fields[i])) continue;
		AlignSize field_alignment;
		LLVMValueRef ptr = llvm_emit_struct_gep_raw(c, pointer, type, (int)i, alignment, &field_alignment);
		store = llvm_store_to_ptr_raw_aligned_single(c, ptr, fields[i], field_alignment);
	}
	return store;
}

LLVMValueRef llvm_store_to_ptr_raw_aligned(GenContext *c, LLVMValueRef pointer, LLVMValueRef value, AlignSize alignment)
{
	if (c->split_aggregates && LLVMIsConstant(value))
	{
		LLVMValueRef store = llvm_store_split_fields(c, pointer, value, alignment);
		if (store) return store;
	}
	return llvm_store_to_ptr_raw_aligned_single(c, pointer, value, alignment);
}

LLVMValueRef llvm_store_to_ptr_raw_aligned_single(GenContext *c, LLVMValueRef pointer, LLVMValueRef value, AlignSize alignment)
{
	ASSERT(alignment > 0);
	LLVMTypeRef type = LLVMTypeOf(value);
	ASSERT(type != c->bool_type);
	if (LLVMIsAAllocaInst(pointer) || LLVMIsAGlobalVariable(pointer))
	{
		ASSERT(alignment <= (AlignSize)LLVMGetAlignment(pointer));
		alignment = (AlignSize)LLVMGetAlignment(pointer);
	}
	if (LLVMGetTypeKind(type) == LLVMVectorTypeKind)
	{
		int len = LLVMGetVectorSize(LLVMTypeOf(value));
		if (llvm_should_expand_vector_store(c, len, pointer, alignment, type))
		{
			int npot = next_highest_power_of_2(len);
			static LLVMValueRef vec[MAX_VECTOR_WIDTH];
			LLVMTypeRef mask_type = llvm_get_type(c, type_uint);
			for (int i = 0; i < len; i++)
			{
				vec[i] = LLVMConstInt(mask_type, i, 0);
			}
			for (int i = len; i < npot; i++)
			{
				vec[i] = LLVMGetPoison(mask_type);
			}
			value = LLVMBuildShuffleVector(c->builder, value, LLVMGetPoison(type), LLVMConstVector(vec, npot), "expandvec");
		}
	}
	LLVMValueRef ref = LLVMBuildStore(c->builder, value, pointer);
	llvm_set_alignment(ref, alignment);
	return ref;
}


void llvm_store_to_ptr_zero(GenContext *context, LLVMValueRef pointer, Type *type)
{
	llvm_store_to_ptr_raw_aligned(context, pointer, llvm_get_zero(context, type), type_abi_alignment(type));
}

bool llvm_temp_as_address(Type *type)
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
			return type_is_aggregate(type);
	}
}

static LLVMValueRef llvm_store_to_ptr_aligned_internal(GenContext *c, LLVMValueRef destination, BEValue *value, AlignSize alignment, bool split)
{
	// If we have an address but not an aggregate, do a load.
	ASSERT(alignment);
	llvm_value_fold_optional(c, value);
	if (value->kind == BE_ADDRESS && !type_is_aggregate(value->type))
	{
		value->value = llvm_load_value_store(c, value);
		value->kind = BE_VALUE;
	}
	switch (value->kind)
	{
		case BE_BOOLVECTOR:
			value->value = LLVMBuildSExt(c->builder, value->value, llvm_get_type(c, value->type), "");
			value->kind = BE_VALUE;
			return llvm_store_to_ptr_raw_aligned_single(c, destination, value->value, alignment);
		case BE_BOOLEAN:
			value->value = LLVMBuildZExt(c->builder, value->value, c->byte_type, "");
			value->kind = BE_VALUE;
			FALLTHROUGH;
		case BE_VALUE:
			return split ? llvm_store_to_ptr_raw_aligned(c, destination, value->value, alignment)
			             : llvm_store_to_ptr_raw_aligned_single(c, destination, value->value, alignment);
		case BE_ADDRESS_OPTIONAL:
			UNREACHABLE
		case BE_ADDRESS:
			return llvm_emit_memcpy(c, destination, alignment, value->value, value->alignment ? value->alignment : type_abi_alignment(value->type), type_size(value->type));
	}
	UNREACHABLE
}

LLVMValueRef llvm_store_to_ptr_aligned(GenContext *c, LLVMValueRef destination, BEValue *value, AlignSize alignment)
{
	return llvm_store_to_ptr_aligned_internal(c, destination, value, alignment, true);
}

LLVMValueRef llvm_store_to_ptr_aligned_single(GenContext *c, LLVMValueRef destination, BEValue *value, AlignSize alignment)
{
	return llvm_store_to_ptr_aligned_internal(c, destination, value, alignment, false);
}

LLVMValueRef llvm_store(GenContext *c, BEValue *destination, BEValue *value)
{
	if (value->type == type_void) return NULL;
	ASSERT(!type_is_void(value->type));
	ASSERT(llvm_value_is_addr(destination));
	return llvm_store_to_ptr_aligned(c, destination->value, value, destination->alignment);
}

LLVMValueRef llvm_store_single(GenContext *c, BEValue *destination, BEValue *value)
{
	if (value->type == type_void) return NULL;
	ASSERT(!type_is_void(value->type));
	ASSERT(llvm_value_is_addr(destination));
	return llvm_store_to_ptr_aligned_single(c, destination->value, value, destination->alignment);
}

LLVMValueRef llvm_load(GenContext *c, LLVMTypeRef type, LLVMValueRef pointer, AlignSize alignment, const char *name)
{
	ASSERT(alignment > 0);
	ASSERT(!llvm_is_global_eval(c));
	ASSERT(LLVMGetTypeContext(type) == c->context);
	if (LLVMGetTypeKind(type) == LLVMVectorTypeKind)
	{
		int len = (int)LLVMGetVectorSize(type);
		if (!is_power_of_two(len) && alignment > llvm_store_size(c, type))
		{
			int npot = next_highest_power_of_2(len);
			LLVMTypeRef t = LLVMVectorType(LLVMGetElementType(type), npot);
			LLVMValueRef value = LLVMBuildLoad2(c->builder, t, pointer, name);
			llvm_set_alignment(value, alignment);
			LLVMValueRef poison = LLVMGetPoison(t);
			static LLVMValueRef vec[MAX_VECTOR_WIDTH];
			for (int i = 0; i < len; i++)
			{
				vec[i] = llvm_const_int(c, type_uint, i);
			}
			value = LLVMBuildShuffleVector(c->builder, value, poison, LLVMConstVector(vec, len), "extractvec");
			return value;
		}
	}
	LLVMValueRef value = LLVMBuildLoad2(c->builder, type, pointer, name);
	llvm_set_alignment(value, alignment);
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
		return LLVMBuildSExt(c->builder, val, llvm_get_type(c, type_get_vector_bool(value->type, TYPE_SIMD_VECTOR)), "");
	}
	if (value->kind != BE_BOOLEAN) return val;
	return LLVMBuildZExt(c->builder, val, c->byte_type, "");
}

void llvm_store_no_fault(GenContext *c, BEValue *ref)
{
	if (ref->kind == BE_ADDRESS_OPTIONAL)
	{
		llvm_store_to_ptr_raw_aligned(c, ref->optional, llvm_get_zero(c, type_fault), type_abi_alignment(type_fault));
	}
}

LLVMValueRef llvm_store_zero(GenContext *c, BEValue *ref)
{
	if (!llvm_value_is_addr(ref)) llvm_value_addr(c, ref);
	Type *type = ref->type;
	llvm_store_no_fault(c, ref);
	if (!type_is_aggregate(type) || type_is_builtin(type->type_kind))
	{
		if (type_kind_is_real_vector(type->type_kind))
		{
			int len = type->array.len;
			if (llvm_should_expand_vector_store(c, len, ref->value, ref->alignment, llvm_get_type(c, type)))
			{
				return llvm_store_raw(c, ref, llvm_emit_const_vector_pot(llvm_get_zero(c, type->array.base), len));
			}
		}

		return llvm_store_raw(c, ref, llvm_get_zero(c, type));
	}
	Type *single_type = type_abi_find_single_struct_element(type, false);

	if (single_type && !type_is_aggregate(single_type))
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
			Type *base = type->array.base;
			for (int i = 0; i < type->array.len; i++)
			{
				AlignSize align;
				LLVMValueRef element_ptr = llvm_emit_array_gep_raw(c, ref->value, base, i, ref->alignment, &align);
				BEValue be_value;
				llvm_value_set_address(c, &be_value, element_ptr, base, align);
				llvm_store_zero(c, &be_value);
			}
			return NULL;
		}
	}
	return llvm_emit_memclear_size_align(c, ref->value, type_size(ref->type), ref->alignment);
}
