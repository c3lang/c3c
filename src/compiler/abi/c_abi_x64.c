// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "compiler/c_abi_internal.h"

typedef enum
{
	UNNAMED,
	NAMED
} NamedArgument;

typedef struct
{
	unsigned sse_registers;
	unsigned int_registers;
} Registers;

bool try_use_registers(Registers *available, Registers *used)
{
	if (available->sse_registers < used->sse_registers) return false;
	if (available->int_registers < used->int_registers) return false;
	available->int_registers -= used->int_registers;
	available->sse_registers -= used->sse_registers;
	return true;
}

typedef enum
{
	// No not change ordering.
	CLASS_NO_CLASS,
	CLASS_MEMORY,
	CLASS_INTEGER,
	CLASS_SSE,
	CLASS_SSEUP,
} X64Class;

static ABIArgInfo *x64_classify_argument_type(Type *type, unsigned free_int_regs, Registers *needed_registers, NamedArgument is_named);
static bool x64_type_is_structure(Type *type);
static void x64_classify(Type *type, ByteSize offset_base, X64Class *lo_class, X64Class *hi_class, NamedArgument named);

ABIArgInfo *x64_indirect_return_result(Type *type)
{
	if (type_is_abi_aggregate(type))
	{
		return abi_arg_new_indirect_not_by_val(type);
	}
	type = type_lowering(type);
	if (type_is_promotable_int_bool(type))
	{
		return abi_arg_new_direct_int_ext(type);
	}
	return abi_arg_new_direct();
}


static bool x64_type_is_illegal_vector(Type *type)
{
	// Only check vectors.
	if (type->type_kind != TYPE_VECTOR) return false;
	ByteSize size = type_size(type);
	// Less than 64 bits or larger than the avx native size => not allowed.
	if (size <= 8 || size > compiler.platform.x64.native_vector_size_avx) return true;
	// If we pass i128 in mem, then check for that.
	if (compiler.platform.x64.pass_int128_vector_in_mem)
	{
		// Illegal if i128/u128
		TypeKind kind = type->array.base->type_kind;
		return kind == TYPE_I128 || kind == TYPE_U128;
	}
	// Otherwise fine!
	return false;
}

ABIArgInfo *x64_indirect_result(Type *type, unsigned free_int_regs)
{
	// If this is a scalar LLVM value then assume LLVM will pass it in the right
	// place naturally.
	//
	// This assumption is optimistic, as there could be free registers available
	// when we need to pass this argument in memory, and LLVM could try to pass
	// the argument in the free register. This does not seem to happen currently,
	// but this code would be much safer if we could mark the argument with
	// 'onstack'. See PR12193.
	type = type_lowering(type);
	if (!type_is_abi_aggregate(type) && !x64_type_is_illegal_vector(type))
	{
		if (type_is_promotable_int_bool(type))
		{
			return abi_arg_new_direct_int_ext(type);
		}
		// No change, just put it on the stack
		return abi_arg_new_direct();
	}

	// The byval alignment
	unsigned align = type_abi_alignment(type);

	// Pass as arguments if there are no more free int regs
	// (if 'onstack' appears, change this code)
	if (!free_int_regs)
	{
		ByteSize size = type_size(type);
		if (align <= 8 && size <= 8)
		{
			return abi_arg_new_direct_coerce_int();
		}
	}
	if (align < 8)
	{
		return abi_arg_new_indirect_realigned(8, type);
	}
	return abi_arg_new_indirect_by_val(type);
}


/**
 * Based on X86_64ABIInfo::classifyRegCallStructTypeImpl in Clang
 * @param type
 * @param needed_registers
 * @return
 */
ABIArgInfo *x64_classify_reg_call_struct_type_check(Type *type, Registers *needed_registers)
{
	ASSERT(x64_type_is_structure(type));

	// These are all passed in two registers.
	if (type->type_kind == TYPE_SLICE || type->type_kind == TYPE_ANY)
	{
		needed_registers->int_registers += 2;
		return abi_arg_new_direct();
	}

	// Struct, err type handled =>
	ASSERT(type->type_kind == TYPE_STRUCT);

	// Variable array structs are always passed by pointer.
	if (type->decl->has_variable_array) return x64_indirect_return_result(type);

	FOREACH(Decl *, member, type->decl->strukt.members)
	{
		Type *member_type = type_lowering(member->type->canonical);
		ABIArgInfo *member_info;
		Registers temp_needed_registers = { 0, 0 };
		if (x64_type_is_structure(member_type))
		{
			// Recursively check the structure.
			member_info = x64_classify_reg_call_struct_type_check(member_type, &temp_needed_registers);
		}
		else
		{
			// Pass as single argument.
			member_info = x64_classify_argument_type(member_type, ~(0U), &temp_needed_registers, NAMED);
		}
		if (abi_arg_is_indirect(member_info))
		{
			*needed_registers = (Registers) { 0, 0 };
			return x64_indirect_return_result(type);
		}
		needed_registers->sse_registers += temp_needed_registers.sse_registers;
		needed_registers->int_registers += temp_needed_registers.int_registers;
	}
	// Send as direct.
	return abi_arg_new_direct();
}



static X64Class x64_merge(X64Class accum, X64Class field)
{
	// 1. Same => result
	// 2. no class + something => something
	// 3. mem + something => mem
	// 4. int + something => int
	// 6. SSE

	// Accum should never be memory (we should have returned) or
	ASSERT(accum != CLASS_MEMORY);
	if (accum == field) return accum;

	// Swap
	if (accum > field)
	{
		X64Class temp = field;
		field = accum;
		accum = temp;
	}
	switch (accum)
	{
		case CLASS_NO_CLASS:
			return field;
		case CLASS_MEMORY:
			return CLASS_MEMORY;
		case CLASS_INTEGER:
			// Other can only be non MEM and non NO_CLASS
			return CLASS_INTEGER;
		case CLASS_SSEUP:
		case CLASS_SSE:
			// Other can only be SSE type
			return CLASS_SSE;
	}
	UNREACHABLE
}
void x64_classify_post_merge(ByteSize size, X64Class *lo_class, X64Class *hi_class)
{
	// If one is MEM => both is mem
	// If X87UP is not before X87 => mem
	// If size > 16 && first isn't SSE or any other is not SSEUP => mem
	// If SSEUP is not preceded by SSE/SSEUP => convert to SSE.
	if (*hi_class == CLASS_MEMORY) *lo_class = CLASS_MEMORY;
	if (size > 16 && (*lo_class != CLASS_SSE || *hi_class != CLASS_SSEUP)) *lo_class = CLASS_MEMORY;
	if (*hi_class == CLASS_SSEUP && *lo_class != CLASS_SSE) *hi_class = CLASS_SSE;
}

void x64_classify_struct_union(Type *type, ByteSize offset_base, X64Class *current, X64Class *lo_class, X64Class *hi_class, NamedArgument named_arg)
{
	ByteSize size = type_size(type);
	// 64 byte max.
	if (size > 64) return;

	// Variable sized member is passed in memory.
	if (type->decl->has_variable_array) return;

	// Re-classify
	*current = CLASS_NO_CLASS;
	bool is_union = type->type_kind == TYPE_UNION;

	Decl **members = type->decl->strukt.members;
	FOREACH(Decl *, member, members)
	{
		ByteSize offset = offset_base + member->offset;
		// The only case a 256-bit or a 512-bit wide vector could be used is when
		// the struct contains a single 256-bit or 512-bit field. Early check
		// and fallback to memory.
		if (size > 16 &&
			((!is_union && size != type_size(member->type))
			|| size > compiler.platform.x64.native_vector_size_avx))
		{
			*lo_class = CLASS_MEMORY;
			x64_classify_post_merge(size, lo_class, hi_class);
			return;
		}
		// Not aligned?
		if (offset % type_abi_alignment(member->type))
		{
			*lo_class = CLASS_MEMORY;
			x64_classify_post_merge(size, lo_class, hi_class);
			return;
		}

		X64Class field_lo;
		X64Class field_hi;
		x64_classify(member->type, offset, &field_lo, &field_hi, named_arg);
		*lo_class = x64_merge(*lo_class, field_lo);
		*hi_class = x64_merge(*hi_class, field_hi);
		if (*lo_class == CLASS_MEMORY || *hi_class == CLASS_MEMORY) break;
	}

	x64_classify_post_merge(size, lo_class, hi_class);
}

void x64_classify_array(Type *type, ByteSize offset_base, X64Class *current, X64Class *lo_class, X64Class *hi_class, NamedArgument named_arg)
{
	ByteSize size = type_size(type);
	Type *element = type->array.base;
	ByteSize element_size = type_size(element);
	// Bigger than 64 bytes => MEM
	if (size > 64) return;

	if (offset_base % type_abi_alignment(element))
	{
		*lo_class = CLASS_MEMORY;
		x64_classify_post_merge(size, lo_class, hi_class);
		return;
	}

	// Re-classify
	*current = CLASS_NO_CLASS;
	// The only case a 256-bit or a 512-bit wide vector could be used is when
	// the struct contains a single 256-bit or 512-bit field. Early check
	// and fallback to memory.
	if (size > 16 && (size != type_size(element) || size > compiler.platform.x64.native_vector_size_avx))
	{
		*lo_class = CLASS_MEMORY;
		return;
	}

	ByteSize offset = offset_base;
	for (ArraySize i = 0; i < type->array.len; i++)
	{
		X64Class field_lo;
		X64Class field_hi;
		x64_classify(element, offset, &field_lo, &field_hi, named_arg);
		offset += element_size;
		*lo_class = x64_merge(*lo_class, field_lo);
		*hi_class = x64_merge(*hi_class, field_hi);
		if (*lo_class == CLASS_MEMORY || *hi_class == CLASS_MEMORY) break;
	}
	x64_classify_post_merge(size, lo_class, hi_class);
	ASSERT(*hi_class != CLASS_SSEUP || *lo_class == CLASS_SSE);
}

void x64_classify_vector(Type *type, ByteSize offset_base, X64Class *current, X64Class *lo_class, X64Class *hi_class,
						 NamedArgument named_arg)
{
	unsigned size = type_size(type);
	// Pass as int
	if (size == 1 || size == 2 || size == 4)
	{
		*current = CLASS_INTEGER;
		// Check boundary crossing
		ByteSize lo = offset_base / 8;
		ByteSize hi = (offset_base + size - 1) / 8;

		// If it crosses boundary, split it.
		if (hi != lo)
		{
			*hi_class = *lo_class;
		}
		return;
	}
	Type *element = type->array.base;
	if (size == 8)
	{
		// 1 x double passed in memory (by gcc)
		if (element->type_kind == TYPE_F64) return;

		// 1 x "long long" is passed different on older clang and
		// gcc, we pick SSE which is the GCC and later Clang standard.
		*current = CLASS_SSE;
		// Split if crossing boundary.
		if (offset_base && offset_base != 8)
		{
			*hi_class = *lo_class;
		}
		return;
	}
	if (size == 16 || (named_arg == NAMED && size <= compiler.platform.x64.native_vector_size_avx))
	{
		if (compiler.platform.x64.pass_int128_vector_in_mem
			&& size != 16 && type_is_int128(element)) return;

		*lo_class = CLASS_SSE;
		*hi_class = CLASS_SSEUP;
	}
	// Default pass by mem
}


static Decl *x64_get_member_at_offset(Decl *decl, unsigned offset)
{
	if (type_size(decl->type) <= offset) return NULL;
	Decl **members = decl->strukt.members;
	Decl *last_match = NULL;
	FOREACH(Decl *, member, members)
	{
		if (member->offset > (ArrayIndex)offset) break;
		last_match = member;
	}
	ASSERT(last_match);
	return last_match;
}

static void x64_classify(Type *type, ByteSize offset_base, X64Class *lo_class, X64Class *hi_class, NamedArgument named)
{
	*lo_class = CLASS_NO_CLASS;
	*hi_class = CLASS_NO_CLASS;
	X64Class *current = offset_base < 8 ? lo_class : hi_class;
	*current = CLASS_MEMORY;
	type = type_lowering(type);
	switch (type->type_kind)
	{
		case LOWERED_TYPES:
		case TYPE_FUNC_RAW:
			UNREACHABLE
		case TYPE_VOID:
			*current = CLASS_NO_CLASS;
			break;
		case TYPE_I128:
		case TYPE_U128:
		case TYPE_SLICE:
		case TYPE_ANY:
			*lo_class = CLASS_INTEGER;
			*hi_class = CLASS_INTEGER;
			break;
		case TYPE_BOOL:
		case TYPE_U8:
		case TYPE_U16:
		case TYPE_U32:
		case TYPE_U64:
		case TYPE_I8:
		case TYPE_I16:
		case TYPE_I32:
		case TYPE_I64:
			*current = CLASS_INTEGER;
			break;
		case TYPE_BF16:
		case TYPE_F16:
		case TYPE_F32:
		case TYPE_F64:
			*current = CLASS_SSE;
			break;
		case TYPE_F128:
			*lo_class = CLASS_SSE;
			*hi_class = CLASS_SSEUP;
			break;
		case TYPE_POINTER:
		case TYPE_FUNC_PTR:
			*current = CLASS_INTEGER;
			break;
		case TYPE_STRUCT:
		case TYPE_UNION:
			x64_classify_struct_union(type, offset_base, current, lo_class, hi_class, named);
			break;
		case TYPE_FLEXIBLE_ARRAY:
		case TYPE_ARRAY:
			x64_classify_array(type, offset_base, current, lo_class, hi_class, named);
			break;
		case TYPE_VECTOR:
			x64_classify_vector(type, offset_base, current, lo_class, hi_class, named);
			break;
	}
}

bool x64_bits_contain_no_user_data(Type *type, unsigned start, unsigned end)
{
	// If the bytes being queried are off the end of the type, there is no user
	// data hiding here.  This handles analysis of builtins, vectors and other
	// types that don't contain interesting padding.
	TypeSize size = type_size(type);
	if (size <= start) return true;
	if (type->type_kind == TYPE_ARRAY)
	{
		// Check each field to see if the field overlaps with the queried range.
		TypeSize element_size = type_size(type->array.base);
		for (unsigned i = 0; i < type->array.len; i++)
		{
			// If the field is after the span we care about, then we're done..
			TypeSize offset = i * element_size;
			if (offset >= end) break;
			unsigned element_start = offset < start ? start - offset : 0;
			if (!x64_bits_contain_no_user_data(type->array.base, element_start, end - offset)) return false;
		}
		// No overlap
		return true;
	}
	if (type->type_kind == TYPE_STRUCT || type->type_kind == TYPE_UNION)
	{
		FOREACH(Decl *, member, type->decl->strukt.members)
		{
			unsigned offset = member->offset;
			if (offset >= end) break;
			unsigned field_start = offset < start ? start - offset : 0;
			if (!x64_bits_contain_no_user_data(member->type, field_start, end - offset)) return false;
		}
		// No overlap
		return true;
	}
	return false;
}

bool x64_contains_float_at_offset(Type *type, unsigned offset)
{
	if (offset == 0 && type->type_kind == TYPE_F32) return true;

	// If this is a struct, recurse into the field at the specified offset.
	if (type->type_kind == TYPE_STRUCT)
	{
		Decl *member = x64_get_member_at_offset(type->decl, offset);
		offset -= member->offset;
		return x64_contains_float_at_offset(member->type, offset);
	}
	if (type->type_kind == TYPE_ARRAY)
	{
		Type *element_type = type->array.base;
		unsigned element_size = type_size(element_type);
		offset -= (offset / element_size) * element_size;
		return x64_contains_float_at_offset(element_type, offset);
	}
	return false;
}

static Type *x64_get_fp_type_at_offset(Type *type, unsigned ir_offset)
{
	if (!ir_offset && type_is_float(type)) return type;
	if (type->type_kind == TYPE_STRUCT || type->type_kind == TYPE_UNION)
	{
		Decl *element = x64_get_member_at_offset(type->decl, ir_offset);
		return x64_get_fp_type_at_offset(element->type, ir_offset - element->offset);
	}
	if (type->type_kind == TYPE_ARRAY)
	{
		Type *element_type = type_lowering(type->array.base);
		ByteSize size = type_size(element_type);
		return x64_get_fp_type_at_offset(element_type, ir_offset - size * (ir_offset / size));
	}
	return NULL;
}

static Type *x64_get_sse_type_at_offset(Type *type, unsigned ir_offset, Type *source_type, unsigned source_offset)
{
	Type *float_type = x64_get_fp_type_at_offset(type, ir_offset);
	if (!float_type || float_type == type_double) return type_double;

	ByteSize size = type_size(float_type);
	ByteSize source_size = type_size(source_type) - source_offset;

	Type *float_type2 = NULL;
	if (source_size > size)
	{
		float_type2 = x64_get_fp_type_at_offset(type, ir_offset + size);
	}

	if (!float_type2)
	{
		// Check if type is half / bf16 + float, float will be at offset + 4 due
		// to alignment.
		if (type_is_16bit_float(float_type) && source_size > 4)
		{
			float_type2 = x64_get_fp_type_at_offset(type, ir_offset + 4);
		}
		// If we can't get a second FP type, return a simple half or float.
		if (!float_type2) return float_type;
	}

	if (float_type == type_float && float_type == float_type2)
	{
		return type_get_vector(float_type, 2);
	}

	if (type_is_16bit_float(float_type) && type_is_16bit_float(float_type2))
	{
		bool has_following_float = source_size > 4
				&& x64_get_fp_type_at_offset(type, ir_offset + 4) != NULL;
		return type_get_vector(float_type, has_following_float ? 4 : 2);
	}
	if (type_is_16bit_float(float_type) || type_is_16bit_float(float_type2))
	{
		return type_get_vector(type_float16, 4);
	}
	return type_double;
}

/**
 * Based off X86_64ABIInfo::GetINTEGERTypeAtOffset in Clang
 */
AbiType x64_get_int_type_at_offset(Type *type, unsigned offset, Type *source_type, unsigned source_offset)
{
	type = type_lowering(type);
	switch (type->type_kind)
	{
		case LOWERED_TYPES:
		case TYPE_VOID:
		case TYPE_FUNC_RAW:
			UNREACHABLE
		case TYPE_U64:
		case TYPE_I64:
		case TYPE_POINTER:
		case TYPE_FUNC_PTR:
			if (!offset) return abi_type_get(type);
			break;
		case TYPE_BOOL:
		case TYPE_U8:
		case TYPE_I8:
		case TYPE_I16:
		case TYPE_U16:
		case TYPE_U32:
		case TYPE_I32:
			if (offset) break;
			if (x64_bits_contain_no_user_data(source_type,
											  source_offset + type_size(type),
											  source_offset + 8))
			{
				return abi_type_get(type);
			}
			break;
		case TYPE_STRUCT:
		{
			Decl *member = x64_get_member_at_offset(type->decl, offset);
			if (member)
			{
				return x64_get_int_type_at_offset(member->type, offset - member->offset, source_type, source_offset);
			}
			break;
		}
		case TYPE_ANY:
			if (offset < 8) return abi_type_get(type_ulong);
			if (offset < 16) return abi_type_get(type_voidptr);
			break;
		case TYPE_SLICE:
			if (offset < 8) return abi_type_get(type_voidptr);
			if (offset < 16) return abi_type_get(type_ulong);
			break;
		case TYPE_FLEXIBLE_ARRAY:
			UNREACHABLE
		case TYPE_ARRAY:
		{
			Type *element = type->array.base;
			TypeSize element_size = type_size(element);
			TypeSize element_offset = (offset / element_size) * element_size;
			return x64_get_int_type_at_offset(element, offset - element_offset, source_type, source_offset);
		}
		case TYPE_I128:
		case TYPE_U128:
		case ALL_FLOATS:
		case TYPE_UNION:
		case TYPE_VECTOR:
			break;
	}
	ByteSize size = type_size(source_type);
	ASSERT(size != source_offset);
	if (size - source_offset > 8) return abi_type_get(type_ulong);
	return abi_type_get_int_bits((size - source_offset) * 8);
}


/**
 * This is only called on SSE.
 */
static AbiType x64_get_byte_vector_type(Type *type)
{
	// Wrapper structs/arrays that only contain vectors are passed just like
	// vectors; strip them off if present.
	Type *inner_type = type_abi_find_single_struct_element(type);
	if (inner_type) type = inner_type;
	type = type_lowering(type);

	// If vector
	if (type->type_kind == TYPE_VECTOR)
	{
		Type *element = type->array.base->canonical;
		if (compiler.platform.x64.pass_int128_vector_in_mem && type_is_int128(element))
		{
			// Convert to u64
			return abi_type_get(type_get_vector(type_ulong, type_size(type) / 8));
		}
		return abi_type_get(type);
	}

	if (type->type_kind == TYPE_F128) return abi_type_get(type);

	unsigned size = type_size(type);

	ASSERT(size == 16 || size == 32 || size == 64);

	// Return a vector type based on the size.
	return abi_type_get(type_get_vector(type_double, size / 8));
}

static ABIArgInfo *x64_get_argument_pair_return(AbiType low_type, AbiType high_type)
{
	TypeSize low_size = abi_type_size(low_type);
	unsigned hi_start = aligned_offset(low_size, abi_type_abi_alignment(high_type));
	ASSERT(hi_start == 8 && "Expected aligned with C-style structs.");
	return abi_arg_new_direct_pair(low_type, high_type);
}


ABIArgInfo *x64_classify_return(Type *return_type)
{
	// AMD64-ABI 3.2.3p4: Rule 1. Classify the return type with the
	// classification algorithm.
	X64Class hi_class;
	X64Class lo_class;
	x64_classify(return_type, 0, &lo_class, &hi_class, NAMED);

	// Invariants
	ASSERT(hi_class != CLASS_MEMORY || lo_class == CLASS_MEMORY);
	ASSERT(hi_class != CLASS_SSEUP || lo_class == CLASS_SSE);

	AbiType result_type = ABI_TYPE_EMPTY;
	switch (lo_class)
	{
		case CLASS_NO_CLASS:
			if (hi_class == CLASS_NO_CLASS)
			{
				return abi_arg_ignore();
			}
			// If low part is padding, keep type null
			ASSERT(hi_class == CLASS_SSE || hi_class == CLASS_INTEGER);
			break;
		case CLASS_SSEUP:
			UNREACHABLE
		case CLASS_MEMORY:
			// AMD64-ABI 3.2.3p4: Rule 2. Types of class memory are returned via
			// hidden argument.
			return x64_indirect_return_result(return_type);
		case CLASS_INTEGER:
			// AMD64-ABI 3.2.3p4: Rule 3. If the class is INTEGER, the next
			// available register of the sequence %rax, %rdx is used.
			result_type = x64_get_int_type_at_offset(return_type, 0, return_type, 0);
			if (hi_class == CLASS_NO_CLASS && type_is_promotable_int_bool(return_type))
			{
				return abi_arg_new_direct_coerce_int_ext(return_type);
			}
			break;
		case CLASS_SSE:
			result_type = abi_type_get(x64_get_sse_type_at_offset(return_type, 0, return_type, 0));
			break;
		default:
			UNREACHABLE
	}

	AbiType high_part = ABI_TYPE_EMPTY;
	switch (hi_class)
	{
		case CLASS_MEMORY:
		case CLASS_NO_CLASS:
			// Previously handled.
			break;
		case CLASS_INTEGER:
			ASSERT(lo_class != CLASS_NO_CLASS);
			high_part = x64_get_int_type_at_offset(return_type, 8, return_type, 8);
			break;
		case CLASS_SSE:
			ASSERT(lo_class != CLASS_NO_CLASS);
			high_part = abi_type_get(x64_get_sse_type_at_offset(return_type, 8, return_type, 8));
			break;
		case CLASS_SSEUP:
			// AMD64-ABI 3.2.3p4: Rule 5. If the class is SSEUP, the eightbyte
			// is passed in the next available eightbyte chunk if the last used
			// vector register.
			//
			// SSEUP should always be preceded by SSE, just widen.
			ASSERT(lo_class == CLASS_SSE && "Unexpected SSEUp classification.");
			result_type = x64_get_byte_vector_type(return_type);
			break;
	}

	// If a high part was specified, merge it together with the low part.  It is
	// known to pass in the high eightbyte of the result.  We do this by forming a
	// first class struct aggregate with the high and low part: {low, high}
	if (abi_type_is_valid(high_part)) return x64_get_argument_pair_return(result_type, high_part);

	if (abi_type_is_type(result_type))
	{
		if (return_type->canonical == result_type.type->canonical)
		{
			return abi_arg_new_direct();
		}
		return abi_arg_new_direct_coerce_type(result_type.type->canonical);
	}
	ASSERT(result_type.int_bits_plus_1 - 1 == type_size(return_type) * 8);
	return abi_arg_new_direct_coerce_int();
}

/**
 * Based off X86_64ABIInfo::classifyArgumentType in Clang.
 * It completely ignores the x87 type, which C3 does not use.
 *
 * @param type the type to classify, it should already have been flattened.
 * @param free_int_regs
 * @param needed_registers
 * @param is_named
 * @return
 */
static ABIArgInfo *x64_classify_argument_type(Type *type, unsigned free_int_regs, Registers *needed_registers, NamedArgument is_named)
{
	ASSERT(type == type_lowering(type));
	X64Class hi_class;
	X64Class lo_class;
	x64_classify(type, 0, &lo_class, &hi_class, is_named);

	// Invariants
	ASSERT(hi_class != CLASS_MEMORY || lo_class == CLASS_MEMORY);
	ASSERT(hi_class != CLASS_SSEUP || lo_class == CLASS_SSE);

	AbiType result_type;
	*needed_registers = (Registers) { 0, 0 };

	// Start by checking the lower class.
	switch (lo_class)
	{
		case CLASS_NO_CLASS:
			// Only C++ would leave 8 bytes of padding, so we can ignore that case.
			ASSERT(hi_class == CLASS_NO_CLASS);
			return abi_arg_ignore();
		case CLASS_SSEUP:
			UNREACHABLE
		case CLASS_MEMORY:
			return x64_indirect_result(type, free_int_regs);
		case CLASS_INTEGER:
			needed_registers->int_registers++;
			result_type = x64_get_int_type_at_offset(type, 0, type, 0);
			if (hi_class == CLASS_NO_CLASS && type_is_promotable_int_bool(type))
			{
				ASSERT(abi_type_is_type(result_type));
				return abi_arg_new_direct_coerce_int_ext(result_type.type);
			}
			break;
		case CLASS_SSE:
			result_type = abi_type_get(x64_get_sse_type_at_offset(type, 0, type, 0));
			needed_registers->sse_registers++;
			break;
	}

	// At this point we know it's not MEMORY, since that's always handled.
	AbiType high_part = ABI_TYPE_EMPTY;
	switch (hi_class)
	{
		case CLASS_MEMORY:
			UNREACHABLE
		case CLASS_NO_CLASS:
			break;
		case CLASS_INTEGER:
			needed_registers->int_registers++;
			high_part = x64_get_int_type_at_offset(type, 8, type, 8);
			// Return directly into high part.
			ASSERT(lo_class != CLASS_NO_CLASS && "empty first 8 bytes not allowed, this is C++ stuff.");
			break;
		case CLASS_SSE:
			needed_registers->sse_registers++;
			high_part = abi_type_get(x64_get_sse_type_at_offset(type, 8, type, 8));
			ASSERT(lo_class != CLASS_NO_CLASS && "empty first 8 bytes not allowed, this is C++ stuff");
			break;
		case CLASS_SSEUP:
			ASSERT(lo_class == CLASS_SSE && "Unexpected SSEUp classification.");
			result_type = x64_get_byte_vector_type(type);
			break;
	}

	// If a high part was specified, merge it together with the low part.  It is
	// known to pass in the high eightbyte of the result.  We do this by forming a
	// first class struct aggregate with the high and low part: {low, high}
	if (abi_type_is_valid(high_part)) return x64_get_argument_pair_return(result_type, high_part);

	if (abi_type_is_type(result_type))
	{
		Type *result = result_type.type->canonical;
		type = type->canonical;
		if (type == result) return abi_arg_new_direct();
		if (type_is_integer(type) && type_is_integer(result) && type->builtin.bytesize == result->builtin.bytesize)
		{
			return abi_arg_new_direct();
		}
		return abi_arg_new_direct_coerce_type(result);
	}
	ASSERT(result_type.int_bits_plus_1 - 1 == type_size(type) * 8);
	return abi_arg_new_direct_coerce_int();
}

bool x64_type_is_structure(Type *type)
{
	switch (type->type_kind)
	{
		case TYPE_STRUCT:
		case TYPE_SLICE:
		case TYPE_ANY:
			return true;
		default:
			return false;
	}
}

/**
 * This code is based on the loop operations in X86_64ABIInfo::computeInfo in Clang
 * @param type
 * @param available_registers to update
 * @param is_regcall true if this is a regcall
 * @param named whether this is a named (non-vararg) parameter or not.
 * @return the calculated ABI
 */
static ABIArgInfo *x64_classify_parameter(Type *type, Registers *available_registers, NamedArgument named)
{
	Registers needed_registers = { 0, 0 };
	type = type_lowering(type);
	ABIArgInfo *info = x64_classify_argument_type(type, available_registers->int_registers, &needed_registers, named);

	// Check if we can fit in a register, we're golden.
	if (try_use_registers(available_registers, &needed_registers)) return info;

	// The rest needs to be passed indirectly.
	return x64_indirect_result(type, available_registers->int_registers);

}

void c_abi_func_create_x64(FunctionPrototype *prototype)
{
	if (prototype->use_win64)
	{
		c_abi_func_create_win64(prototype);
		return;
	}

	Registers available_registers = {
			.int_registers = 6,
			.sse_registers = 8
	};

	prototype->ret_abi_info = x64_classify_return(type_lowering(prototype->abi_ret_type));
	if (abi_arg_is_indirect(prototype->ret_abi_info)) available_registers.int_registers--;

	if (prototype->ret_by_ref)
	{
		prototype->ret_by_ref_abi_info = x64_classify_parameter(type_get_ptr(type_lowering(prototype->ret_by_ref_type)),
																&available_registers,
																NAMED);
	}

	Type **params = prototype->param_types;
	unsigned param_count = vec_size(prototype->param_types);
	if (param_count)
	{
		ABIArgInfo **args = MALLOC(sizeof(ABIArgInfo) * param_count);
		for (unsigned i = 0; i < param_count; i++)
		{
			args[i] = x64_classify_parameter(params[i], &available_registers, NAMED);
		}
		prototype->abi_args = args;
	}
	unsigned vararg_count = vec_size(prototype->varargs);
	if (vararg_count)
	{
		ABIArgInfo **args = MALLOC(sizeof(ABIArgInfo) * vararg_count);
		for (unsigned i = 0; i < vararg_count; i++)
		{
			args[i] = x64_classify_parameter(prototype->varargs[i], &available_registers, UNNAMED);
		}
		prototype->abi_varargs = args;
	}

}
