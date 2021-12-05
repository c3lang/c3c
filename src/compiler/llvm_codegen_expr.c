// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_internal.h"
#include <math.h>

void gencontext_emit_binary(GenContext *c, BEValue *be_value, Expr *expr, BEValue *lhs_loaded, BinaryOp binary_op);
static void llvm_emit_any_pointer(GenContext *c, BEValue *any, BEValue *pointer);
static void llvm_emit_const_expr(GenContext *c, BEValue *be_value, Expr *expr);
static void gencontext_emit_unary_expr(GenContext *context, BEValue *value, Expr *expr);
static inline void llvm_emit_post_inc_dec(GenContext *c, BEValue *value, Expr *expr, int diff, bool use_mod);
static inline void llvm_emit_pre_inc_dec(GenContext *c, BEValue *value, Expr *expr, int diff, bool use_mod);
static inline void llvm_emit_inc_dec_change(GenContext *c, bool use_mod, BEValue *addr, BEValue *after, BEValue *before, Expr *expr, int diff);
static void llvm_emit_post_unary_expr(GenContext *context, BEValue *be_value, Expr *expr);
static inline void llvm_emit_subscript_addr_with_base(GenContext *c, BEValue *result, BEValue *parent, BEValue *index, SourceLocation *loc);
static void llvm_emit_initialize_designated(GenContext *c, BEValue *ref, AlignSize offset, DesignatorElement** current, DesignatorElement **last, Expr *expr, BEValue *emitted_value);
static inline void llvm_emit_const_initialize_reference(GenContext *c, BEValue *ref, Expr *expr);
LLVMValueRef llvm_emit_is_no_error_value(GenContext *c, BEValue *value)
{
	llvm_value_rvalue(c, value);
	return LLVMBuildICmp(c->builder, LLVMIntEQ, value->value, llvm_get_zero(c, type_anyerr), "not_err");
}

static inline LLVMValueRef llvm_emit_extract_value(GenContext *c, LLVMValueRef agg, unsigned index)
{
	if (LLVMIsConstant(agg))
	{
		return LLVMConstExtractValue(agg, &index, 1);
	}
	else
	{
		return LLVMBuildExtractValue(c->builder, agg, index, "");
	}
}

static inline LLVMValueRef llvm_emit_extract_element(GenContext *c, LLVMValueRef vector, unsigned index)
{
	if (LLVMIsConstant(vector))
	{
		return LLVMConstExtractElement(vector, llvm_const_int(c, type_usize, index));
	}
	else
	{
		return LLVMBuildExtractElement(c->builder, vector, llvm_const_int(c, type_usize, index), "");
	}
}

static inline LLVMValueRef llvm_emit_insert_value(GenContext *c, LLVMValueRef agg, LLVMValueRef new_value, unsigned index)
{
	if (LLVMIsConstant(agg) && LLVMIsConstant(new_value))
	{
		return LLVMConstInsertValue(agg, new_value, &index, 1);
	}
	else
	{
		return LLVMBuildInsertValue(c->builder, agg, new_value, index, "");
	}
}

static inline LLVMValueRef llvm_zext_trunc(GenContext *c, LLVMValueRef data, LLVMTypeRef type)
{
	LLVMTypeRef current_type = LLVMTypeOf(data);
	if (current_type == type) return data;
	assert(LLVMGetTypeKind(type) == LLVMIntegerTypeKind);
	assert(LLVMGetTypeKind(current_type) == LLVMIntegerTypeKind);
	if (llvm_bitsize(c, current_type) < llvm_bitsize(c, type))
	{
		if (LLVMIsConstant(data))
		{
			return LLVMConstZExt(data, type);
		}
		return LLVMBuildZExt(c->builder, data, type, "");
	}
	assert(llvm_bitsize(c, current_type) > llvm_bitsize(c, type));
	if (LLVMIsConstant(data))
	{
		return LLVMConstTrunc(data, type);
	}
	return LLVMBuildTrunc(c->builder, data, type, "");
}

static inline LLVMValueRef llvm_emit_lshr(GenContext *c, LLVMValueRef data, int shift)
{
	assert(shift >= 0);
	if (shift == 0) return data;
	LLVMTypeRef type = LLVMTypeOf(data);
	BitSize bit_width = llvm_bitsize(c, type);
	if (shift >= bit_width) return LLVMConstNull(type);
	if (LLVMIsAConstant(data))
	{
		return LLVMConstLShr(data, LLVMConstInt(type, (unsigned)shift, false));
	}
	return LLVMBuildLShr(c->builder, data, LLVMConstInt(type, (unsigned)shift, false), "");
}

static inline LLVMValueRef llvm_emit_shl(GenContext *c, LLVMValueRef data, int shift)
{
	assert(shift >= 0);
	if (shift == 0) return data;
	LLVMTypeRef type = LLVMTypeOf(data);
	BitSize bit_width = llvm_bitsize(c, type);
	if (shift >= bit_width) return LLVMConstNull(type);
	if (LLVMIsAConstant(data))
	{
		return LLVMConstShl(data, LLVMConstInt(type, (unsigned)shift, false));
	}
	return LLVMBuildShl(c->builder, data, LLVMConstInt(type, (unsigned)shift, false), "");
}

void llvm_convert_vector_comparison(GenContext *c, BEValue *be_value, LLVMValueRef val, Type *vector_type)
{
	Type *result_type = type_get_vector_bool(vector_type);
	val = LLVMBuildSExt(c->builder, val, llvm_get_type(c, result_type), "");
	llvm_value_set(be_value, val, result_type);
}

LLVMValueRef llvm_emit_coerce_alignment(GenContext *c, BEValue *be_value, LLVMTypeRef coerce_type, AlignSize target_alignment, AlignSize *resulting_alignment)
{
	// If we are loading something with greater alignment than what we have, we cannot directly memcpy.
	if (!llvm_value_is_addr(be_value) || be_value->alignment < target_alignment)
	{
		LLVMValueRef cast = llvm_emit_alloca(c, llvm_get_type(c, be_value->type), target_alignment, "coerce");
		LLVMValueRef target = LLVMBuildBitCast(c->builder, cast, LLVMPointerType(coerce_type, 0), "");
		llvm_store_bevalue_aligned(c, target, be_value, target_alignment);
		*resulting_alignment = target_alignment;
		return cast;
	}
	*resulting_alignment = be_value->alignment;
	return LLVMBuildBitCast(c->builder, be_value->value, LLVMPointerType(coerce_type, 0), "");
}


LLVMValueRef llvm_emit_aggregate_value(GenContext *c, Type *type, ...)
{
	LLVMValueRef result = LLVMGetUndef(llvm_get_type(c, type));
	va_list args;
	va_start(args, type);
	LLVMValueRef val;
	bool is_constant = true;
	while (is_constant && (val = va_arg(args, LLVMValueRef)) != NULL)
	{
		if (!LLVMIsConstant(val)) is_constant = false;
	}
	va_end(args);
	va_start(args, type);
	unsigned index = 0;
	if (is_constant)
	{
		while ((val = va_arg(args, LLVMValueRef)) != NULL)
		{
			result = LLVMConstInsertValue(result, val, &index, 1);
			index++;
		}
	}
	else
	{
		assert(c->builder);
		while ((val = va_arg(args, LLVMValueRef)) != NULL)
		{
			result = LLVMBuildInsertValue(c->builder, result, val, index++, "");
		}
	}
	va_end(args);
	return result;
}

LLVMValueRef llvm_const_low_bitmask(LLVMTypeRef type, int type_bits, int low_bits)
{
	if (low_bits < 1) return LLVMConstNull(type);
	if (type_bits <= low_bits) return LLVMConstAllOnes(type);
	return LLVMConstLShr(LLVMConstAllOnes(type), LLVMConstInt(type, (unsigned long long)(type_bits - low_bits), 0));
}

LLVMValueRef llvm_const_high_bitmask(LLVMTypeRef type, int type_bits, int high_bits)
{
	if (high_bits < 1) return LLVMConstNull(type);
	if (type_bits <= high_bits) return LLVMConstAllOnes(type);
	return LLVMConstNot(LLVMConstLShr(LLVMConstAllOnes(type), LLVMConstInt(type, (unsigned long long)high_bits, 0)));
}

LLVMValueRef llvm_mask_low_bits(GenContext *c, LLVMValueRef value, unsigned low_bits)
{
	LLVMTypeRef type = LLVMTypeOf(value);
	if (low_bits < 1) return LLVMConstNull(type);
	BitSize type_bits = llvm_bitsize(c, type);
	if (type_bits <= low_bits) return value;
	LLVMValueRef mask = LLVMConstLShr(LLVMConstAllOnes(type), LLVMConstInt(type, type_bits - low_bits, 0));
	if (LLVMIsConstant(value))
	{
		return LLVMConstAnd(mask, value);
	}
	return LLVMBuildAnd(c->builder, mask, value, "");
}

LLVMTypeRef llvm_const_padding_type(GenContext *c, AlignSize size)
{
	assert(size > 0);
	if (size == 1) return llvm_get_type(c, type_char);
	return LLVMArrayType(llvm_get_type(c, type_char), (unsigned)size);
}

LLVMValueRef llvm_emit_const_padding(GenContext *c, AlignSize size)
{
	return LLVMGetUndef(llvm_const_padding_type(c, size));
}

static inline LLVMValueRef llvm_emit_add_int(GenContext *c, Type *type, LLVMValueRef left, LLVMValueRef right, SourceLocation *loc)
{
	if (active_target.feature.trap_on_wrap)
	{
		LLVMTypeRef type_to_use = llvm_get_type(c, type->canonical);
		LLVMValueRef args[2] = { left, right };
		assert(type->canonical == type);
		LLVMValueRef add_res;
		if (type_is_unsigned(type))
		{
			add_res = llvm_emit_call_intrinsic(c, intrinsic_id.uadd_overflow, &type_to_use, 1, args, 2);
		}
		else
		{
			add_res = llvm_emit_call_intrinsic(c, intrinsic_id.sadd_overflow, &type_to_use, 1, args, 2);
		}
		LLVMValueRef result = LLVMBuildExtractValue(c->builder, add_res, 0, "");
		LLVMValueRef ok = LLVMBuildExtractValue(c->builder, add_res, 1, "");
		llvm_emit_panic_on_true(c, ok, "Addition overflow", loc);
		return result;
	}

	return LLVMBuildAdd(c->builder, left, right, "add");
}

void llvm_enter_struct_for_coerce(GenContext *c, LLVMValueRef *struct_ptr, LLVMTypeRef *type, ByteSize dest_size)
{
	while (1)
	{
		if (LLVMGetTypeKind(*type) != LLVMStructTypeKind) return;
		if (!LLVMCountStructElementTypes(*type)) return;
		LLVMTypeRef first_element = LLVMStructGetTypeAtIndex(*type, 0);
		ByteSize first_element_size = llvm_store_size(c, first_element);
		// If the size is smaller than the total size and smaller than the destination size
		// then we're done.
		if (first_element_size < dest_size && first_element_size < llvm_store_size(c, *type))
		{
			return;
		}
		AlignSize dummy;
		LLVMValueRef ref = llvm_emit_struct_gep_raw(c, *struct_ptr, *type, 0, llvm_abi_alignment(c, *type), &dummy);

		*struct_ptr = ref;
		*type = first_element;
	}
}

LLVMValueRef llvm_int_resize(GenContext *c, LLVMValueRef value, LLVMTypeRef from, LLVMTypeRef to)
{
	if (llvm_store_size(c, from) >= llvm_store_size(c, to))
	{
		return LLVMBuildTruncOrBitCast(c->builder, value, to, "trunc");
	}
	return LLVMBuildZExt(c->builder, value, to, "ext");
}

/**
 * General functionality to convert int <-> int ptr <-> int
 */
LLVMValueRef llvm_coerce_int_ptr(GenContext *c, LLVMValueRef value, LLVMTypeRef from, LLVMTypeRef to)
{
	// 1. Are they the same?
	if (from == to) return value;

	// 2. If the source is a pointer, then.
	bool to_is_pointer = LLVMGetTypeKind(to) == LLVMPointerTypeKind;
	if (LLVMGetTypeKind(from) == LLVMPointerTypeKind)
	{
		// 2a. Destination is a pointer, perform a bitcast.
		if (to_is_pointer)
		{
			return LLVMBuildBitCast(c->builder, value, to, "coerce.val");
		}
		// 2b. Otherwise perform ptr -> int
		from = llvm_get_type(c, type_iptr);
		value = LLVMBuildPtrToInt(c->builder, value, from, "");
	}

	// 3. Find the to int type to convert to.
	LLVMTypeRef to_int_type = to_is_pointer ? llvm_get_type(c, type_iptr) : to;

	// 4. Are int types not matching?
	if (to_int_type != from)
	{
		if (platform_target.big_endian)
		{
			// Big endian, preserve the high bits.
			ByteSize to_size = llvm_abi_size(c, to_int_type);
			ByteSize from_size = llvm_abi_size(c, from);
			if (from_size > to_size)
			{
				value = LLVMBuildLShr(c->builder, value, LLVMConstInt(from, (from_size - to_size) * 8, false), "");
				value = LLVMBuildTrunc(c->builder, value, to_int_type, "");
			}
			else
			{
				value = LLVMBuildZExt(c->builder, value, to_int_type, "");
				value = LLVMBuildShl(c->builder, value, LLVMConstInt(from, (to_size - from_size) * 8, false), "");
			}
		}
		else
		{
			// Little-endian targets preserve the low bits. No shifts required.
			value = LLVMBuildIntCast2(c->builder, value, to_int_type, false, "");
		}
	}
	if (to_is_pointer)
	{
		value = LLVMBuildIntToPtr(c->builder, value, to, "");
	}
	return value;
}

LLVMValueRef llvm_emit_coerce(GenContext *c, LLVMTypeRef coerced, BEValue *value, Type *original_type)
{
	assert(original_type->canonical == value->type->canonical);
	LLVMTypeRef llvm_source_type = llvm_get_type(c, value->type);

	// 1. If the types match then we're done, just load.
	if (llvm_source_type == coerced)
	{
		llvm_value_rvalue_store(c, value);
		return value->value;
	}

	// 2. Both are integer types and values, then just truncate / extend
	if (!llvm_value_is_addr(value)
		&& LLVMGetTypeKind(coerced) == LLVMIntegerTypeKind
		&& LLVMGetTypeKind(llvm_source_type) == LLVMIntegerTypeKind)
	{
		return llvm_int_resize(c, value->value, llvm_source_type, coerced);
	}

	// 2. From now on we need th address.
	llvm_value_addr(c, value);
	LLVMValueRef addr = value->value;

	ByteSize target_size = llvm_store_size(c, coerced);

	// 3. If this is a struct, we index into it.
	if (LLVMGetTypeKind(llvm_source_type) == LLVMStructTypeKind)
	{
		llvm_enter_struct_for_coerce(c, &addr, &llvm_source_type, target_size);
	}
	// --> from now on we only use LLVM types.

	ByteSize source_size = llvm_store_size(c, llvm_source_type);

	LLVMTypeKind source_type_kind = LLVMGetTypeKind(llvm_source_type);
	LLVMTypeKind coerced_type_kind = LLVMGetTypeKind(coerced);

	if ((coerced_type_kind == LLVMPointerTypeKind || coerced_type_kind == LLVMIntegerTypeKind)
		&& (source_type_kind == LLVMPointerTypeKind || source_type_kind == LLVMIntegerTypeKind))
	{
		LLVMValueRef val = llvm_emit_load_aligned(c, llvm_source_type, addr, value->alignment, "");
		return llvm_coerce_int_ptr(c, val, llvm_source_type, coerced);
	}

	// TODO for scalable vectors this is not true.
	if (source_size > target_size)
	{
		LLVMValueRef val = LLVMBuildBitCast(c->builder, addr, LLVMPointerType(coerced, 0), "");
		return llvm_emit_load_aligned(c, coerced, val, value->alignment, "");
	}

	// Otherwise, do it through memory.
	AlignSize max_align = MAX(value->alignment, llvm_abi_alignment(c, coerced));

	LLVMValueRef temp = llvm_emit_alloca(c, coerced, max_align, "tempcoerce");
	llvm_emit_memcpy(c, temp, max_align, addr, value->alignment, source_size);
	return llvm_emit_load_aligned(c, coerced, temp, max_align, "");
}


void llvm_emit_coerce_store(GenContext *c, LLVMValueRef addr, AlignSize alignment, LLVMTypeRef coerced, LLVMValueRef value, LLVMTypeRef target_type)
{

	// 1. Simplest case, the underlying types match.
	if (coerced == target_type)
	{
		llvm_store_aligned(c, addr, value, alignment);
		return;
	}

	ByteSize src_size = llvm_store_size(c, coerced);

	// 3. Enter into a struct in case the result is a struct.
	if (LLVMGetTypeKind(target_type) == LLVMStructTypeKind)
	{
		llvm_enter_struct_for_coerce(c, &addr, &target_type, src_size);
	}

	// 4. If we are going from int/ptr <-> ptr/int
	LLVMTypeKind source_type_kind = LLVMGetTypeKind(target_type);
	LLVMTypeKind coerced_type_kind = LLVMGetTypeKind(coerced);
	if ((coerced_type_kind == LLVMPointerTypeKind || coerced_type_kind == LLVMIntegerTypeKind)
	    && (source_type_kind == LLVMPointerTypeKind || source_type_kind == LLVMIntegerTypeKind))
	{
		value = llvm_coerce_int_ptr(c, value, coerced, target_type);
		llvm_store_aligned(c, addr, value, alignment);
		return;
	}

	// TODO for scalable vectors this is not true.
	ByteSize target_size = llvm_store_size(c, target_type);
	if (src_size <= target_size)
	{
		LLVMValueRef val = LLVMBuildBitCast(c->builder, addr, LLVMPointerType(coerced, 0), "");
		llvm_store_aligned(c, val, value, alignment);
		return;
	}

	// Otherwise, do it through memory.
	AlignSize coerce_align = llvm_abi_alignment(c, coerced);
	LLVMValueRef temp = llvm_emit_alloca(c, coerced, coerce_align, "tempcoerce");
	llvm_store_aligned(c, temp, value, coerce_align);
	llvm_emit_memcpy(c, addr, alignment, temp, coerce_align, target_size);
}

void llvm_emit_convert_value_from_coerced(GenContext *c, BEValue *result, LLVMTypeRef coerced, LLVMValueRef value, Type *original_type)
{
	LLVMTypeRef target_type = llvm_get_type(c, original_type);
	LLVMValueRef addr = llvm_emit_alloca(c, target_type, type_abi_alignment(original_type), "result");
	llvm_emit_coerce_store(c, addr, type_abi_alignment(original_type), coerced, value, target_type);
	llvm_value_set_address(result, addr, original_type);
}

static inline LLVMValueRef llvm_emit_sub_int(GenContext *c, Type *type, LLVMValueRef left, LLVMValueRef right, SourceLocation *loc)
{
	if (active_target.feature.trap_on_wrap)
	{
		LLVMTypeRef type_to_use = llvm_get_type(c, type);
		LLVMValueRef args[2] = { left, right };
		assert(type->canonical == type);
		LLVMValueRef add_res;
		if (type_is_unsigned(type))
		{
			add_res = llvm_emit_call_intrinsic(c, intrinsic_id.usub_overflow, &type_to_use, 1, args, 2);
		}
		else
		{
			add_res = llvm_emit_call_intrinsic(c, intrinsic_id.ssub_overflow, &type_to_use, 1, args, 2);
		}
		LLVMValueRef result = LLVMBuildExtractValue(c->builder, add_res, 0, "");
		LLVMValueRef ok = LLVMBuildExtractValue(c->builder, add_res, 1, "");
		llvm_emit_panic_on_true(c, ok, "Subtraction overflow", loc);
		return result;
	}

	return LLVMBuildSub(c->builder, left, right, "sub");
}

static inline void llvm_emit_subscript_addr_base(GenContext *context, BEValue *value, Expr *parent)
{
	llvm_emit_expr(context, value, parent);
	llvm_emit_ptr_from_array(context, value);
}

static void llvm_emit_array_bounds_check(GenContext *c, BEValue *index, LLVMValueRef array_max_index, SourceLocation *loc)
{
	BEValue result;
	llvm_value_rvalue(c, index);

	// Negative values are not allowed.
	if (type_is_signed(index->type))
	{
		llvm_emit_int_comp(c, &result, index->type, index->type, index->value,
						   llvm_get_zero(c, index->type), BINARYOP_LT);
		llvm_emit_panic_if_true(c, &result, "Negative array indexing", loc);
	}

	llvm_emit_int_comp(c, &result, index->type, index->type,
	                   index->value, array_max_index,
	                   BINARYOP_GE);
	llvm_emit_panic_if_true(c, &result, "Array index out of bounds", loc);
}

static inline void llvm_emit_subscript_addr_with_base(GenContext *c, BEValue *result, BEValue *parent, BEValue *index, SourceLocation *loc)
{
	assert(llvm_value_is_addr(parent));
	Type *type = type_lowering(parent->type);
	switch (type->type_kind)
	{
		case TYPE_POINTER:
			llvm_value_set_address(result, llvm_emit_pointer_inbounds_gep_raw(c, llvm_get_pointee_type(c, parent->type), parent->value, index->value), type->pointer);
			return;
		case TYPE_ARRAY:
		case TYPE_VECTOR:
			// TODO vector
			if (active_target.feature.safe_mode)
			{
				llvm_emit_array_bounds_check(c, index, llvm_const_int(c, index->type, type->array.len), loc);
			}
			{
				AlignSize alignment;
				LLVMValueRef ptr = llvm_emit_array_gep_raw_index(c, parent->value, llvm_get_type(c, type), index->value, parent->alignment, &alignment);
				llvm_value_set_address_align(result, ptr, type->array.base, alignment);
			}
			return;
		case TYPE_SUBARRAY:
			if (active_target.feature.safe_mode)
			{
				// TODO insert trap on overflow.
			}
			{
				LLVMValueRef ptr = llvm_emit_pointer_inbounds_gep_raw(c, llvm_get_type(c, type->array.base), parent->value, index->value);
				llvm_value_set_address_align(result, ptr, type->array.base, type_abi_alignment(type->array.base));
			}
			return;
		case TYPE_STRLIT:
			// TODO insert trap on overflow.
			llvm_value_set_address_align(result, llvm_emit_pointer_inbounds_gep_raw(c, llvm_get_type(c, type_char), parent->value, index->value),
										 type_char, type_abi_alignment(type_char));
			return;
		default:
			UNREACHABLE

	}
}

static inline void llvm_emit_vector_subscript(GenContext *c, BEValue *value, Expr *expr)
{
	llvm_emit_expr(c, value, expr->subscript_expr.expr);
	llvm_value_rvalue(c, value);
	Type *element = value->type->array.base;
	LLVMValueRef vector = value->value;
	llvm_emit_expr(c, value, expr->subscript_expr.index);
	llvm_value_rvalue(c, value);
	LLVMValueRef index = value->value;
	if (LLVMIsAConstant(index) && LLVMIsAConstant(vector))
	{
		llvm_value_set(value, LLVMConstExtractElement(vector, index), element);
	}
	else
	{
		llvm_value_set(value, LLVMBuildExtractElement(c->builder, vector, index, ""), element);
	}
}

/**
 * Expand foo[123] or someCall()[n] or some such.
 * Evaluation order is left to right.
 */
static inline void gencontext_emit_subscript(GenContext *c, BEValue *value, Expr *expr)
{
	bool is_value = expr->expr_kind == EXPR_SUBSCRIPT;
	if (is_value && type_lowering(expr->subscript_expr.expr->type)->type_kind == TYPE_VECTOR)
	{
		llvm_emit_vector_subscript(c, value, expr);
		return;
	}
	BEValue ref;
	// First, get thing being subscripted.
	llvm_emit_subscript_addr_base(c, &ref, expr->subscript_expr.expr);
	// It needs to be an address.
	llvm_value_addr(c, &ref);

	// Now calculate the index:
	BEValue index;
	llvm_emit_expr(c, &index, expr->subscript_expr.index);
	// It needs to be an rvalue.
	llvm_value_rvalue(c, &index);

	llvm_emit_subscript_addr_with_base(c, value, &ref, &index, TOKLOC(expr->subscript_expr.index->span.loc));
	if (!is_value)
	{
		assert(llvm_value_is_addr(value));
		llvm_value_fold_failable(c, value);
		value->kind = BE_VALUE;
		value->type = type_get_ptr(value->type);
	}
}


static MemberIndex find_member_index(Decl *parent, Decl *member)
{
	VECEACH(parent->strukt.members, i)
	{
		Decl *maybe_member = parent->strukt.members[i];
		if (member == maybe_member)
		{
			return (int)i;
		}
		if (!maybe_member->name)
		{
			if (find_member_index(maybe_member, member) != -1) return (int)i;
		}
	}
	return -1;
}

static void gencontext_emit_member_addr(GenContext *c, BEValue *value, Decl *parent, Decl *member)
{
	assert(member->resolve_status == RESOLVE_DONE);

	Decl *found = NULL;
	do
	{
		MemberIndex index = find_member_index(parent, member);
		assert(index > -1);
		found = parent->strukt.members[index];
		switch (parent->type->canonical->type_kind)
		{
			case TYPE_UNION:
				llvm_value_addr(c, value);
				llvm_value_set_address_align(value, llvm_emit_bitcast(c, value->value, type_get_ptr(found->type)), found->type, value->alignment);
				break;
			case TYPE_STRUCT:
				llvm_value_struct_gep(c, value, value, (unsigned)index);
				break;
			default:
				UNREACHABLE
		}
		parent = found;
	} while (found != member);
}

static void llvm_emit_bitstruct_member(GenContext *c, BEValue *value, Decl *parent, Decl *member)
{
	assert(member->resolve_status == RESOLVE_DONE);
	Decl *found = NULL;
	do
	{
		MemberIndex index = find_member_index(parent, member);
		assert(index > -1);
		found = parent->strukt.members[index];
		switch (parent->type->canonical->type_kind)
		{
			case TYPE_UNION:
				llvm_value_addr(c, value);
				llvm_value_set_address_align(value,
				                             llvm_emit_bitcast(c, value->value, type_get_ptr(found->type)),
				                             found->type,
				                             value->alignment);
				break;
			case TYPE_STRUCT:
				llvm_value_struct_gep(c, value, value, (unsigned)index);
				break;
			case TYPE_BITSTRUCT:
				break;
			default:
				UNREACHABLE
		}
		parent = found;
	} while (found != member);
}

static LLVMValueRef llvm_emit_bswap(GenContext *c, LLVMValueRef value)
{
	if (LLVMIsConstant(value))
	{
		return LLVMConstBswap(value);
	}
	LLVMTypeRef type = LLVMTypeOf(value);
	return llvm_emit_call_intrinsic(c, intrinsic_id.bswap, &type, 1, &value, 1);
}




/**
 * The super simple case is extracting a bool from a char array:
 * 1. Grab the byte
 * 2. Rightshift
 * 3. Truncate to 1 bit
 */
static inline void llvm_extract_bool_bit_from_array(GenContext *c, BEValue *be_value, Decl *member)
{
	AlignSize alignment;
	LLVMValueRef array_ptr = be_value->value;
	LLVMTypeRef array_type = llvm_get_type(c, type_char);
	unsigned start_bit = member->var.start_bit;
	// Grab the byte
	LLVMValueRef byte_ptr = llvm_emit_array_gep_raw(c, array_ptr, llvm_get_type(c, be_value->type),
	                                                start_bit / 8, be_value->alignment, &alignment);
	LLVMValueRef element = llvm_emit_load_aligned(c, array_type, byte_ptr, alignment, "");
	// Shift the bit to the zero position.
	element = llvm_emit_lshr(c, element, start_bit % 8);
	// Truncate to i1.
	element = LLVMBuildTrunc(c->builder, element, c->bool_type, "");
	// Done!
	llvm_value_set_bool(be_value, element);
}

static inline LLVMValueRef llvm_bswap_non_integral(GenContext *c, LLVMValueRef value, unsigned bitsize)
{
	if (bitsize <= 8) return value;
	LLVMValueRef shifted = llvm_emit_shl(c, value, (int)llvm_bitsize(c, LLVMTypeOf(value)) - (int)bitsize);
	return llvm_emit_bswap(c, shifted);
}

static inline void llvm_extract_bitvalue_from_array(GenContext *c, BEValue *be_value, Decl *member, Decl *parent_decl)
{
	llvm_value_addr(c, be_value);
	if (type_lowering(member->type) == type_bool)
	{
		llvm_extract_bool_bit_from_array(c, be_value, member);
		return;
	}
	bool big_endian = platform_target.big_endian;
	if (parent_decl->bitstruct.big_endian) big_endian = true;
	if (parent_decl->bitstruct.little_endian) big_endian = false;
	unsigned start = member->var.start_bit;
	unsigned end = member->var.end_bit;
	LLVMValueRef array_ptr = be_value->value;
	LLVMTypeRef array_type = llvm_get_type(c, type_char);
	LLVMValueRef result = NULL;
	int start_byte = start / 8;
	int end_byte = end / 8;
	Type *member_type = type_lowering(member->type);
	LLVMTypeRef llvm_member_type = llvm_get_type(c, member_type);
	TypeSize bitsize = type_size(member_type) * 8;
	LLVMValueRef res = NULL;
	int offset = start % 8;
	for (int i = start_byte; i <= end_byte; i++)
	{
		AlignSize alignment;
		LLVMValueRef byte_ptr = llvm_emit_array_gep_raw(c, array_ptr, llvm_get_type(c, be_value->type),
		                                                (unsigned)i, be_value->alignment, &alignment);
		LLVMValueRef element = llvm_emit_load_aligned(c, array_type, byte_ptr, alignment, "");
		element = llvm_zext_trunc(c, element, llvm_member_type);
		int current_offset = 8 * (i - start_byte) - offset;
		if (current_offset < 0)
		{
			element = llvm_emit_lshr(c, element, -current_offset);
		}
		else if (current_offset > 0)
		{
			element = llvm_emit_shl(c, element, current_offset);
		}
		if (res == NULL)
		{
			res = element;
			continue;
		}
		res = LLVMBuildOr(c->builder, element, res, "");
	}
	if (big_endian)
	{
		res = llvm_bswap_non_integral(c, res, end - start + 1);
	}
	if (type_is_signed(member_type))
	{
		TypeSize top_bits_to_clear = bitsize - end + start - 1;
		if (top_bits_to_clear)
		{
			LLVMValueRef shift = LLVMConstInt(llvm_member_type, top_bits_to_clear, false);
			res = LLVMBuildShl(c->builder, res, shift, "");
			res = LLVMBuildAShr(c->builder, res, shift, "");
		}
	}
	else
	{
		res = llvm_mask_low_bits(c, res, end - start + 1);
	}
	llvm_value_set(be_value, res, member_type);
}


static inline void llvm_extract_bitvalue(GenContext *c, BEValue *be_value, Expr *parent, Decl *member)
{
	Decl *parent_decl = type_flatten(parent->type)->decl;
	if (be_value->type->type_kind == TYPE_ARRAY)
	{
		llvm_extract_bitvalue_from_array(c, be_value, member, parent_decl);
		return;
	}
	bool bswap = false;
	if (parent_decl->bitstruct.big_endian && !platform_target.big_endian) bswap = true;
	if (parent_decl->bitstruct.little_endian && platform_target.big_endian) bswap = true;
	LLVMValueRef value = llvm_value_rvalue_store(c, be_value);
	if (bswap) value = llvm_emit_bswap(c, value);
	LLVMTypeRef container_type = LLVMTypeOf(value);
	BitSize container_size = type_size(be_value->type);
	BitSize container_bit_size = container_size * 8;
	unsigned start = (unsigned)member->var.start_bit;
	unsigned end = (unsigned)member->var.end_bit;
	Type *member_type = type_lowering(member->type);
	ByteSize member_type_size = type_size(member_type);
	if (type_is_signed(member_type))
	{
		// Shift all the way left, so top bit is to the top.
		uint64_t left_shift = container_bit_size - end - 1;
		if (left_shift)
		{
			value = LLVMBuildShl(c->builder, value, LLVMConstInt(container_type, left_shift, 0), "");
		}
		uint64_t right_shift = left_shift + start;
		if (right_shift)
		{
			value = LLVMBuildAShr(c->builder, value, LLVMConstInt(container_type, right_shift, 0), "");
		}
		if (member_type_size < container_bit_size)
		{
			value = LLVMBuildTrunc(c->builder, value, llvm_get_type(c, member_type), "");
		}
		else if (member_type_size > container_bit_size)
		{
			value = LLVMBuildSExt(c->builder, value, llvm_get_type(c, member_type), "");
		}
	}
	else
	{
		// Shift away bottom:
		if (start)
		{
			value = LLVMBuildLShr(c->builder, value, LLVMConstInt(container_type, start, 0), "");
		}
		TypeSize bits_needed = end - start + 1;
		value = llvm_mask_low_bits(c, value, bits_needed);
		value = llvm_zext_trunc(c, value, llvm_get_type(c, member_type));
	}
	llvm_value_set(be_value, value, member_type);
}

static inline void llvm_emit_bitassign_array(GenContext *c, BEValue *result, BEValue parent, Decl *parent_decl, Decl *member)
{
	// We could possibly do this on a value as well. However, this
	// is unlikely to be the common case, so use addr.
	llvm_value_addr(c, &parent);
	LLVMValueRef array_ptr = parent.value;
	assert(parent.type->type_kind == TYPE_ARRAY);
	LLVMTypeRef array_type = llvm_get_type(c, parent.type);

	unsigned start_bit = member->var.start_bit;
	unsigned end_bit = member->var.end_bit;

	Type *member_type = type_flatten(member->type);
	LLVMValueRef value = llvm_value_rvalue_store(c, result);
	if (member_type == type_bool)
	{
		assert(start_bit == end_bit);
		value = llvm_emit_shl(c, value, start_bit % 8);
		AlignSize alignment;
		LLVMValueRef byte_ptr = llvm_emit_array_gep_raw(c, array_ptr, array_type, start_bit / 8, parent.alignment, &alignment);
		LLVMValueRef current = llvm_emit_load_aligned(c, c->byte_type, byte_ptr, alignment, "");
		LLVMValueRef bit = llvm_emit_shl(c, LLVMConstInt(c->byte_type, 1, 0), start_bit % 8);
		current = LLVMBuildAnd(c->builder, current, LLVMConstNot(bit), "");
		current = LLVMBuildOr(c->builder, current, value, "");
		llvm_store_aligned(c, byte_ptr, current, alignment);
		return;
	}

	bool big_endian = platform_target.big_endian;
	if (parent_decl->bitstruct.big_endian) big_endian = true;
	if (parent_decl->bitstruct.little_endian) big_endian = false;

	unsigned bit_size = end_bit - start_bit + 1;
	if (big_endian)
	{
		value = llvm_bswap_non_integral(c, value, bit_size);
	}
	assert(bit_size > 0 && bit_size <= 128);
	int start_byte = start_bit / 8;
	int end_byte = end_bit / 8;
	int start_mod = start_bit % 8;
	int end_mod = end_bit % 8;
	ByteSize member_type_bitsize = type_size(member_type) * 8;
	for (int i = start_byte; i <= end_byte; i++)
	{
		AlignSize alignment;
		LLVMValueRef byte_ptr = llvm_emit_array_gep_raw(c, array_ptr, array_type,
		                                                (unsigned)i, parent.alignment, &alignment);
		if (i == start_byte && start_mod != 0)
		{
			int skipped_bits = start_mod;
			// Shift the lower bits into the top of the byte.
			LLVMValueRef res = llvm_emit_shl(c, value, skipped_bits);
			// Then truncate.
			if (member_type_bitsize > 8)
			{
				res = llvm_zext_trunc(c, res, c->byte_type);
			}
			// Create a mask for the lower bits.
			LLVMValueRef mask = llvm_const_low_bitmask(c->byte_type, 8, skipped_bits);

			// We might need to mask the top bits
			if (i == end_byte && end_mod != 7)
			{
				res = LLVMBuildAnd(c->builder, res, llvm_const_low_bitmask(c->byte_type, 8, end_mod + 1), "");
				mask = LLVMConstOr(mask, llvm_const_high_bitmask(c->byte_type, 8, 7 - (int)end_bit));
			}
			// Load the current value.
			LLVMValueRef current = llvm_emit_load_aligned(c, c->byte_type, byte_ptr, alignment, "");
			// Empty the top bits.
			current = LLVMBuildAnd(c->builder, current, mask, "");
			// Use *or* with the top bits from "res":
			current = LLVMBuildOr(c->builder, current, res, "");
			// And store it back.
			llvm_store_aligned(c, byte_ptr, current, alignment);
			// We now shift the value by the number of bits we used.
			value = llvm_emit_lshr(c, value, 8 - skipped_bits);
			// ... and we're done with the first byte.
			continue;
		}
		if (i == end_byte && end_mod != 7)
		{
			// What remains is end_mod + 1 bits to copy.
			value = llvm_zext_trunc(c, value, c->byte_type);
			// Create a mask for the lower bits.
			LLVMValueRef mask = llvm_const_low_bitmask(c->byte_type, 8, end_mod + 1);
			value = LLVMBuildAnd(c->builder, value, mask, "");
			// Load the current value.
			LLVMValueRef current = llvm_emit_load_aligned(c, c->byte_type, byte_ptr, alignment, "");
			// Clear the lower bits.
			current = LLVMBuildAnd(c->builder, current, LLVMConstNot(mask), "");
			// Use *or* with the bottom bits from "value":
			current = LLVMBuildOr(c->builder, current, value, "");
			// And store it back.
			llvm_store_aligned(c, byte_ptr, current, alignment);
			continue;
		}
		// All others are simple: truncate & store
		llvm_store_aligned(c, byte_ptr, llvm_zext_trunc(c, value, c->byte_type), alignment);
		// Then shift
		value = llvm_emit_lshr(c, value, 8);
	}
}
static inline void llvm_emit_bitassign_expr(GenContext *c, BEValue *be_value, Expr *expr)
{
	Expr *lhs = expr->binary_expr.left;
	Expr *parent_expr = lhs->access_expr.parent;

	// Grab the parent
	BEValue parent;
	llvm_emit_expr(c, &parent, parent_expr);

	Decl *member = lhs->access_expr.ref;

	// If we have assign + op, load the current value, perform the operation.
	if (expr->binary_expr.operator != BINARYOP_ASSIGN)
	{
		// Grab the current value.
		BEValue value = parent;
		llvm_extract_bitvalue(c, &value, parent_expr, member);
		// Perform the operation and place it in be_value
		gencontext_emit_binary(c, be_value, expr, &value, binaryop_assign_base_op(expr->binary_expr.operator));
	}
	else
	{
		// Otherwise just resolve the rhs and place it in be_value
		llvm_emit_expr(c, be_value, expr->binary_expr.right);
	}

	if (type_lowering(parent_expr->type)->type_kind == TYPE_ARRAY)
	{
		llvm_emit_bitassign_array(c, be_value, parent, type_flatten_distinct(parent_expr->type)->decl, member);
		return;
	}

	// To start the assign, pull out the current value.
	LLVMValueRef current_value = llvm_value_rvalue_store(c, &parent);

	// Get the type.
	LLVMTypeRef struct_type = LLVMTypeOf(current_value);

	// We now need to create a mask, a very naive algorithm:
	LLVMValueRef mask = LLVMConstAllOnes(struct_type);
	TypeSize bits = type_size(parent.type) * 8;
	int start_bit = (int)member->var.start_bit;
	int end_bit = (int)member->var.end_bit;
	// Let's say we want to create 00111000 => start: 3 end: 5
	int left_shift = (int)bits - end_bit - 1;
	mask = llvm_emit_shl(c, mask, left_shift);
	// => shift 2: 11111100
	mask = llvm_emit_lshr(c, mask, left_shift + start_bit);
	// => shift 5: 00000111
	mask = llvm_emit_shl(c, mask, start_bit);
	// => shift 3: 00111000

	// Now we might need to truncate or widen the value to insert:
	LLVMValueRef value = llvm_value_rvalue_store(c, be_value);
	value = llvm_zext_trunc(c, value, struct_type);
	// Shift to the correct location.
	value = llvm_emit_shl(c, value, start_bit);
	// And combine using ((current_value & ~mask) | (value & mask))
	value = LLVMBuildAnd(c->builder, value, mask, "");
	current_value = LLVMBuildAnd(c->builder, current_value, LLVMConstNot(mask), "");
	current_value = LLVMBuildOr(c->builder, current_value, value, "");
	llvm_store_bevalue_raw(c, &parent, current_value);
}
static inline void llvm_emit_bitaccess(GenContext *c, BEValue *be_value, Expr *expr)
{
	Expr *parent = expr->access_expr.parent;
	llvm_emit_expr(c, be_value, parent);

	Decl *member = expr->access_expr.ref;
	assert(be_value && be_value->type);

	llvm_emit_bitstruct_member(c, be_value, type_flatten(parent->type)->decl, member);
	llvm_extract_bitvalue(c, be_value, parent, expr->access_expr.ref);
}

static inline void gencontext_emit_access_addr(GenContext *context, BEValue *be_value, Expr *expr)
{
	Expr *parent = expr->access_expr.parent;
	llvm_emit_expr(context, be_value, parent);
	Decl *member = expr->access_expr.ref;

	gencontext_emit_member_addr(context, be_value, type_lowering(parent->type)->decl, member);
}

static void gencontext_emit_scoped_expr(GenContext *context, BEValue *value, Expr *expr)
{
	llvm_emit_expr(context, value, expr->expr_scope.expr);
	llvm_emit_defer(context, expr->expr_scope.defers.start, expr->expr_scope.defers.end);
}


static inline void llvm_emit_initialize_reference(GenContext *c, BEValue *value, Expr *expr);

/**
 * Here we are converting an array to a subarray.
 * int[] x = &the_array;
 * @param c
 * @param value
 * @param to_type
 * @param from_type
 */
static void llvm_emit_arr_to_subarray_cast(GenContext *c, BEValue *value, Type *to_type)
{
	llvm_value_rvalue(c, value);
	ByteSize size = value->type->pointer->array.len;
	Type *array_type = value->type->pointer->array.base;
	LLVMTypeRef subarray_type = llvm_get_type(c, to_type);
	LLVMValueRef pointer = llvm_emit_bitcast(c, value->value, type_get_ptr(array_type));
	LLVMValueRef len = llvm_const_int(c, type_usize, size);
	value->type = to_type;
	value->value = llvm_emit_aggregate_value(c, to_type, pointer, len, NULL);
}


LLVMValueRef gencontext_emit_value_bitcast(GenContext *context, LLVMValueRef value, Type *to_type, Type *from_type)
{
	LLVMValueRef ptr = llvm_emit_alloca_aligned(context, from_type, "");
	LLVMBuildStore(context->builder, value, ptr);
	LLVMValueRef ptr_cast = llvm_emit_bitcast(context, ptr, type_get_ptr(to_type));
	return gencontext_emit_load(context, to_type, ptr_cast);
}

void llvm_emit_vector_to_array_cast(GenContext *c, BEValue *value, Type *to_type, Type *from_type)
{
	llvm_value_rvalue(c, value);
	LLVMTypeRef array_type = llvm_get_type(c, to_type);
	LLVMValueRef array = LLVMGetUndef(array_type);
	bool is_const = LLVMIsConstant(value->value);
	for (unsigned i = 0; i < to_type->array.len; i++)
	{
		LLVMValueRef element = llvm_emit_extract_element(c, value->value, i);
		if (is_const)
		{
			array = LLVMConstInsertValue(array, element, &i, 1);
			continue;
		}
		array = LLVMBuildInsertValue(c->builder, array, element, i, "");
	}
	llvm_value_set(value, array, to_type);
}

void llvm_emit_array_to_vector_cast(GenContext *c, BEValue *value, Type *to_type, Type *from_type)
{
	llvm_value_rvalue(c, value);
	LLVMTypeRef vector_type = llvm_get_type(c, to_type);
	LLVMValueRef vector = LLVMGetUndef(vector_type);
	bool is_const = LLVMIsConstant(value->value);
	for (unsigned i = 0; i < to_type->vector.len; i++)
	{
		LLVMValueRef element = llvm_emit_extract_value(c, value->value, i);
		if (is_const)
		{
			vector = LLVMConstInsertElement(vector, element, llvm_const_int(c, type_usize, i));
			continue;
		}
		vector = LLVMBuildInsertElement(c->builder, vector, element, llvm_const_int(c, type_usize, i), "");
	}
	llvm_value_set(value, vector, to_type);
}

void llvm_emit_cast(GenContext *c, CastKind cast_kind, BEValue *value, Type *to_type, Type *from_type)
{
	to_type = type_flatten(to_type);
	from_type = type_flatten(from_type);

	switch (cast_kind)
	{
		case CAST_ARRVEC:
			llvm_emit_array_to_vector_cast(c, value, to_type, from_type);
			return;
		case CAST_PTRANY:
		{
			llvm_value_rvalue(c, value);
			LLVMTypeRef any = llvm_get_type(c, to_type);
			LLVMValueRef pointer = llvm_emit_bitcast(c, value->value, type_voidptr);
			BEValue typeid;
			llvm_emit_typeid(c, &typeid, from_type->pointer);
			llvm_value_set(value, llvm_emit_aggregate_value(c, to_type, pointer, typeid.value, NULL), to_type);
			return;
		}
		case CAST_BSARRY:
			llvm_value_addr(c, value);
			value->value = llvm_emit_bitcast(c, value->value, type_get_ptr(to_type));
			value->type = to_type;
			llvm_value_rvalue(c, value);
			return;
		case CAST_BSINT:
			llvm_value_addr(c, value);
			value->value = llvm_emit_bitcast(c, value->value, type_get_ptr(to_type));
			value->type = to_type;
			llvm_value_rvalue(c, value);
			return;
		case CAST_EUINT:
		case CAST_ERINT:
			to_type = type_lowering(to_type);
			from_type = type_lowering(from_type);
			llvm_value_rvalue(c, value);
			if (type_convert_will_trunc(to_type, from_type))
			{
				value->value = LLVMBuildTrunc(c->builder, value->value, llvm_get_type(c, to_type), "errinttrunc");
			}
			else
			{
				value->value = type_is_signed(to_type)
						? LLVMBuildSExt(c->builder, value->value, llvm_get_type(c, to_type), "errsiext")
						: LLVMBuildZExt(c->builder, value->value, llvm_get_type(c, to_type), "erruiext");

			}
			break;
		case CAST_ANYPTR:
			llvm_value_fold_failable(c, value);
			if (llvm_value_is_addr(value))
			{
				llvm_emit_any_pointer(c, value, value);
			}
			else
			{
				value->value = LLVMBuildExtractValue(c->builder, value->value, 0, "");
			}
			break;
		case CAST_XIERR:
			to_type = type_lowering(to_type);
			llvm_value_rvalue(c, value);
			value->value = llvm_zext_trunc(c, value->value, llvm_get_type(c, to_type));
			break;
		case CAST_ERROR:
			UNREACHABLE
		case CAST_STRPTR:
		case CAST_PTRPTR:
			llvm_value_rvalue(c, value);
			if (c->builder)
			{
				value->value = LLVMBuildPointerCast(c->builder, value->value, llvm_get_type(c, to_type), "ptrptr");
			}
			else
			{
				value->value = LLVMConstPointerCast(value->value, llvm_get_type(c, to_type));
			}
			break;
		case CAST_PTRXI:
			llvm_value_rvalue(c, value);
			if (c->builder)
			{
				value->value = LLVMBuildPtrToInt(c->builder, value->value, llvm_get_type(c, to_type), "ptrxi");
			}
			else
			{
				value->value = LLVMConstPtrToInt(value->value, llvm_get_type(c, to_type));
			}
			break;
		case CAST_APTSA:
			llvm_emit_arr_to_subarray_cast(c, value, to_type);
			break;
		case CAST_SAPTR:
			llvm_value_fold_failable(c, value);
			if (llvm_value_is_addr(value))
			{
				llvm_emit_subarray_pointer(c, value, value);
			}
			else
			{
				value->value = LLVMBuildExtractValue(c->builder, value->value, 0, "");
			}
			break;
		case CAST_ARRPTR:
			TODO
		case CAST_EREU:
			// This is a no op.
			assert(type_lowering(to_type) == type_lowering(from_type));
			break;
		case CAST_VECARR:
			llvm_emit_vector_to_array_cast(c, value, to_type, from_type);
			break;
		case CAST_EUER:
			TODO // gencontext_emit_value_bitcast(c, value->value, to_type, from_type);
		case CAST_ERBOOL:
		case CAST_EUBOOL:
		{
			BEValue zero;
			llvm_value_set_int(c, &zero, type_anyerr, 0);
			llvm_emit_int_comparison(c, value, value, &zero, BINARYOP_NE);
			break;
		}
		case CAST_PTRBOOL:
			llvm_value_rvalue(c, value);
			value->value = LLVMBuildIsNotNull(c->builder, value->value, "ptrbool");
			value->kind = BE_BOOLEAN;
			break;
		case CAST_BOOLINT:
			llvm_value_rvalue(c, value);
			value->value =  LLVMBuildZExt(c->builder, value->value, llvm_get_type(c, to_type), "boolsi");
			value->kind = BE_VALUE;
			break;
		case CAST_FPBOOL:
			llvm_value_rvalue(c, value);
			value->value =  LLVMBuildFCmp(c->builder, LLVMRealUNE, value->value, llvm_get_zero(c, from_type), "fpbool");
			value->kind = BE_BOOLEAN;
			break;
		case CAST_BOOLBOOL:
			value->value = LLVMBuildTrunc(c->builder, value->value, c->bool_type, "boolbool");
			value->kind = BE_BOOLEAN;
			break;
		case CAST_BOOLFP:
			llvm_value_rvalue(c, value);
			value->value =  LLVMBuildUIToFP(c->builder, value->value, llvm_get_type(c, to_type), "boolfp");
			value->kind = BE_VALUE;
			break;
		case CAST_INTBOOL:
			llvm_value_rvalue(c, value);
			value->value = LLVMBuildICmp(c->builder, LLVMIntNE, value->value, llvm_get_zero(c, from_type), "intbool");
			value->kind = BE_BOOLEAN;
			break;
		case CAST_FPFP:
			llvm_value_rvalue(c, value);
			value->value = type_convert_will_trunc(to_type, from_type)
			       ? LLVMBuildFPTrunc(c->builder, value->value, llvm_get_type(c, to_type), "fpfptrunc")
			       : LLVMBuildFPExt(c->builder, value->value, llvm_get_type(c, to_type), "fpfpext");
			break;
		case CAST_FPSI:
			llvm_value_rvalue(c, value);
			value->value = LLVMBuildFPToSI(c->builder, value->value, llvm_get_type(c, to_type), "fpsi");
			break;
		case CAST_FPUI:
			llvm_value_rvalue(c, value);
			value->value = LLVMBuildFPToUI(c->builder, value->value, llvm_get_type(c, to_type), "fpui");
			break;
		case CAST_SISI:
			llvm_value_rvalue(c, value);
			value->value = type_convert_will_trunc(to_type, from_type)
			       ? LLVMBuildTrunc(c->builder, value->value, llvm_get_type(c, to_type), "sisitrunc")
			       : LLVMBuildSExt(c->builder, value->value, llvm_get_type(c, to_type), "sisiext");
			break;
		case CAST_SIUI:
			llvm_value_rvalue(c, value);
			value->value = type_convert_will_trunc(to_type, from_type)
			       ? LLVMBuildTrunc(c->builder, value->value, llvm_get_type(c, to_type), "siuitrunc")
			       : LLVMBuildZExt(c->builder, value->value, llvm_get_type(c, to_type), "siuiext");
			break;
		case CAST_SIFP:
			llvm_value_rvalue(c, value);
			value->value = LLVMBuildSIToFP(c->builder, value->value, llvm_get_type(c, to_type), "sifp");
			break;
		case CAST_XIPTR:
			llvm_value_rvalue(c, value);
			if (LLVMIsConstant(value->value))
			{
				value->value = LLVMConstIntToPtr(value->value, llvm_get_type(c, to_type));
				break;
			}
			assert(c->builder);
			value->value = LLVMBuildIntToPtr(c->builder, value->value, llvm_get_type(c, to_type), "xiptr");
			break;
		case CAST_UISI:
			llvm_value_rvalue(c, value);
			value->value = type_convert_will_trunc(to_type, from_type)
			       ? LLVMBuildTrunc(c->builder, value->value, llvm_get_type(c, to_type), "uisitrunc")
			       : LLVMBuildZExt(c->builder, value->value, llvm_get_type(c, to_type), "uisiext");
			break;
		case CAST_UIUI:
			llvm_value_rvalue(c, value);
			value->value = type_convert_will_trunc(to_type, from_type)
			       ? LLVMBuildTrunc(c->builder, value->value, llvm_get_type(c, to_type), "uiuitrunc")
			       : LLVMBuildZExt(c->builder, value->value, llvm_get_type(c, to_type), "uiuiext");
			break;
		case CAST_UIFP:
			llvm_value_rvalue(c, value);
			value->value = LLVMBuildUIToFP(c->builder, value->value, llvm_get_type(c, to_type), "uifp");
			break;
		case CAST_ENUMLOW:
			llvm_value_rvalue(c, value);
			value->value = value->value;
			break;
		case CAST_STST:
			llvm_value_addr(c, value);
			value->value = LLVMBuildBitCast(c->builder, value->value, llvm_get_ptr_type(c, to_type), "");
			value->type = to_type;
			return;
		case CAST_SABOOL:
			llvm_value_fold_failable(c, value);
			if (llvm_value_is_addr(value))
			{
				value->value = llvm_emit_struct_gep_raw(c,
				                                        value->value,
				                                        llvm_get_type(c, value->type),
				                                        1,
				                                        value->alignment,
				                                        &value->alignment);
			}
			else
			{
				value->value = LLVMBuildExtractValue(c->builder, value->value, 1, "");
			}
			value->type = type_usize->canonical;
			llvm_value_rvalue(c, value);
			llvm_emit_int_comp_zero(c, value, value, BINARYOP_NE);
			break;
	}
	value->type = to_type;
}

static inline void gencontext_emit_cast_expr(GenContext *context, BEValue *be_value, Expr *expr)
{
	llvm_emit_expr(context, be_value, expr->cast_expr.expr);
	llvm_emit_cast(context,
	               expr->cast_expr.kind,
	               be_value,
	               expr->type,
	               expr->cast_expr.expr->type);
}


static LLVMValueRef llvm_recursive_set_value(GenContext *c, DesignatorElement **current_element_ptr, LLVMValueRef parent, DesignatorElement **last_element_ptr, Expr *value)
{
	DesignatorElement *current_element = current_element_ptr[0];
	if (current_element_ptr == last_element_ptr)
	{
		BEValue res;
		llvm_emit_expr(c, &res, value);
		unsigned index = (unsigned)current_element->index;
		LLVMValueRef val = llvm_value_rvalue_store(c, &res);
		switch (current_element->kind)
		{
			case DESIGNATOR_FIELD:
				return LLVMConstInsertValue(parent, val, &index, 1);
			case DESIGNATOR_ARRAY:
				return LLVMConstInsertElement(parent, val, llvm_const_int(c, type_isize, (unsigned)current_element->index));
			case DESIGNATOR_RANGE:
				for (MemberIndex i = current_element->index; i <= current_element->index_end; i++)
				{
					parent = LLVMConstInsertElement(parent, val, llvm_const_int(c, type_isize, (uint64_t)i));
				}
				return parent;
		}
		UNREACHABLE
	}
	LLVMValueRef current_val;
	switch (current_element->kind)
	{
		case DESIGNATOR_FIELD:
		{
			unsigned index = (unsigned)current_element->index;
			current_val = LLVMConstExtractValue(parent, &index, 1);
			current_val = llvm_recursive_set_value(c, current_element_ptr + 1, current_val, last_element_ptr, value);
			return LLVMConstInsertValue(parent, current_val, &index, 1);
		}
		case DESIGNATOR_ARRAY:
		{
			LLVMValueRef index = llvm_const_int(c, type_isize, (uint64_t)current_element->index);
			current_val = LLVMConstExtractElement(parent, index);
			current_val = llvm_recursive_set_value(c, current_element_ptr + 1, current_val, last_element_ptr, value);
			return LLVMConstInsertElement(parent, current_val, index);
		}
		case DESIGNATOR_RANGE:
			for (MemberIndex i = current_element->index; i <= current_element->index_end; i++)
			{
				LLVMValueRef index = llvm_const_int(c, type_isize, (uint64_t)i);
				current_val = LLVMConstExtractElement(parent, index);
				current_val = llvm_recursive_set_value(c, current_element_ptr + 1, current_val, last_element_ptr, value);
				parent = LLVMConstInsertElement(parent, current_val, index);
			}
			return parent;
		default:
			UNREACHABLE
	}
}


void llvm_emit_initialize_reference_temporary_const(GenContext *c, BEValue *ref, Expr *expr)
{
	bool modified = false;
	// First create the constant value.

	Type *canonical = expr->type->canonical;

	assert(expr->expr_kind == EXPR_CONST && expr->const_expr.const_kind == CONST_LIST);
	LLVMValueRef value = llvm_emit_const_initializer(c, expr->const_expr.list);

	LLVMTypeRef expected_type = llvm_get_type(c, canonical);
	// Create a global const.
	LLVMTypeRef type = LLVMTypeOf(value);
	LLVMValueRef global_copy = LLVMAddGlobal(c->module, type, ".__const");
	llvm_set_private_linkage(global_copy);

	// Set a nice alignment
	AlignSize alignment = type_alloca_alignment(expr->type);
	llvm_set_alignment(global_copy, alignment);

	// Set the value and make it constant
	LLVMSetInitializer(global_copy, value);
	LLVMSetGlobalConstant(global_copy, true);

	// Ensure we have a reference.
	llvm_value_addr(c, ref);

	if (expected_type != type)
	{
		global_copy = LLVMConstBitCast(global_copy, LLVMPointerType(expected_type, 0));
	}

	// Perform the memcpy.
	llvm_emit_memcpy(c, ref->value, ref->alignment, global_copy, alignment, type_size(expr->type));
}

static void llvm_emit_inititialize_reference_const(GenContext *c, BEValue *ref, ConstInitializer *const_init)
{
	switch (const_init->kind)
	{
		case CONST_INIT_ZERO:
			if (type_is_builtin(ref->type->type_kind) || ref->type->type_kind == TYPE_ARRAY)
			{
				llvm_store_bevalue_raw(c, ref, llvm_get_zero(c, ref->type));
				return;
			}
			llvm_emit_memclear(c, ref);
			return;
		case CONST_INIT_ARRAY_VALUE:
			UNREACHABLE
		case CONST_INIT_ARRAY_FULL:
		{
			LLVMValueRef array_ref = ref->value;
			Type *array_type = const_init->type;
			Type *element_type = array_type->array.base;
			MemberIndex size = (MemberIndex)array_type->array.len;
			LLVMTypeRef array_type_llvm = llvm_get_type(c, array_type);
			assert(size <= UINT32_MAX);
			for (MemberIndex i = 0; i < size; i++)
			{
				AlignSize alignment;
				LLVMValueRef array_pointer = llvm_emit_array_gep_raw(c, array_ref, array_type_llvm, (unsigned)i, ref->alignment, &alignment);
				BEValue value;
				llvm_value_set_address_align(&value, array_pointer, element_type, alignment);
				llvm_emit_inititialize_reference_const(c, &value, const_init->init_array_full[i]);
			}
			return;
		}
		case CONST_INIT_ARRAY:
		{
			LLVMValueRef array_ref = ref->value;
			llvm_emit_memclear(c, ref);
			Type *array_type = const_init->type;
			Type *element_type = array_type->array.base;
			LLVMTypeRef array_type_llvm = llvm_get_type(c, array_type);
			ConstInitializer **elements = const_init->init_array.elements;
			MemberIndex current_index = 0;
			LLVMValueRef *parts = NULL;
			VECEACH(elements, i)
			{
				ConstInitializer *element = elements[i];
				assert(element->kind == CONST_INIT_ARRAY_VALUE);
				MemberIndex element_index = element->init_array_value.index;
				AlignSize alignment;
				LLVMValueRef array_pointer = llvm_emit_array_gep_raw(c, array_ref, array_type_llvm, (unsigned)element_index, ref->alignment, &alignment);
				BEValue value;
				llvm_value_set_address_align(&value, array_pointer, element_type, alignment);
				llvm_emit_inititialize_reference_const(c, &value, element->init_array_value.element);
			}
			return;
		}
		case CONST_INIT_UNION:
		{
			Decl *decl = const_init->type->decl;
			MemberIndex index = const_init->init_union.index;
			Type *type = decl->strukt.members[index]->type->canonical;
			// Bitcast.
			BEValue value = *ref;
			llvm_value_set_address(&value, llvm_emit_bitcast(c, ref->value, type_get_ptr(type)), type);
			// Emit our value.
			llvm_emit_inititialize_reference_const(c, &value, const_init->init_union.element);
			return;
		}
		case CONST_INIT_STRUCT:
		{
			Decl *decl = const_init->type->decl;
			Decl **members = decl->strukt.members;
			MemberIndex count = (MemberIndex)vec_size(members);
			for (MemberIndex i = 0; i < count; i++)
			{
				BEValue value;
				llvm_value_struct_gep(c, &value, ref, (unsigned)i);
				llvm_emit_inititialize_reference_const(c, &value, const_init->init_struct[i]);
			}
			return;
		}
		case CONST_INIT_VALUE:
		{
			BEValue value;
			llvm_emit_expr(c, &value, const_init->init_value);
			llvm_store_bevalue(c, ref, &value);
			return;
		}
	}
	UNREACHABLE

}
static inline void llvm_emit_initialize_reference_const(GenContext *c, BEValue *ref, Expr *expr)
{
	assert(expr->expr_kind == EXPR_CONST && expr->const_expr.const_kind == CONST_LIST);
	ConstInitializer *initializer = expr->const_expr.list;

	// Make sure we have an address.
	llvm_value_addr(c, ref);

	llvm_emit_inititialize_reference_const(c, ref, initializer);

}

static inline void llvm_emit_initialize_reference_list(GenContext *c, BEValue *ref, Expr *expr)
{
	// Getting ready to initialize, get the real type.
	Type *real_type = type_lowering(ref->type);
	Expr **elements = expr->initializer_list;

	// Make sure we have an address.
	llvm_value_addr(c, ref);
	LLVMValueRef value = ref->value;

	// If this is a union, we assume it's initializing the first element.
	if (real_type->type_kind == TYPE_UNION)
	{
		assert(vec_size(elements) == 1);
		real_type = type_lowering(real_type->decl->strukt.members[0]->type);
		value = LLVMBuildBitCast(c->builder, ref->value, llvm_get_ptr_type(c, real_type), "");
	}

	LLVMTypeRef llvm_type = llvm_get_type(c, real_type);
	bool is_struct = type_is_structlike(real_type);
	bool is_array = real_type->type_kind == TYPE_ARRAY;
	// Now walk through the elements.
	VECEACH(elements, i)
	{
		Expr *element = elements[i];
		BEValue pointer;
		if (is_struct)
		{
			llvm_value_struct_gep(c, &pointer, ref, i);
		}
		else if (is_array)
		{
			// Todo optimize
			AlignSize alignment;
			LLVMValueRef ptr = llvm_emit_array_gep_raw(c, value, llvm_type, i, ref->alignment, &alignment);
			llvm_value_set_address_align(&pointer, ptr, element->type, alignment);
		}
		else
		{
			llvm_value_set_address_align(&pointer, value, element->type, ref->alignment);
		}
		// If this is an initializer, we want to actually run the initialization recursively.
		if (element->expr_kind == EXPR_CONST && element->const_expr.const_kind == CONST_LIST)
		{
			llvm_emit_const_initialize_reference(c, &pointer, element);
			continue;
		}
		if (expr_is_init_list(element))
		{
			llvm_emit_initialize_reference(c, &pointer, element);
			continue;
		}
		BEValue init_value;
		llvm_emit_expr(c, &init_value, element);
		llvm_store_bevalue(c, &pointer, &init_value);
	}
}

static void llvm_emit_initialize_designated_const_range(GenContext *c, BEValue *ref, AlignSize offset, DesignatorElement** current, DesignatorElement **last, Expr *expr, BEValue *emitted_value)
{
	DesignatorElement *curr = current[0];
	llvm_value_addr(c, ref);

	assert(curr->kind == DESIGNATOR_RANGE);

	BEValue emitted_local;
	if (!emitted_value)
	{
		llvm_emit_expr(c, &emitted_local, expr);
		emitted_value = &emitted_local;
	}
	LLVMTypeRef ref_type = llvm_get_type(c, ref->type);
	for (MemberIndex i = curr->index; i <= curr->index_end; i++)
	{
		BEValue new_ref;
		AlignSize alignment;
		LLVMValueRef ptr = llvm_emit_array_gep_raw(c, ref->value, ref_type, (unsigned)i, ref->alignment, &alignment);
		llvm_value_set_address_align(&new_ref, ptr, type_get_indexed_type(ref->type), alignment);
		llvm_emit_initialize_designated(c, &new_ref, offset, current + 1, last, expr, emitted_value);
	}
}

static void llvm_emit_initialize_designated(GenContext *c, BEValue *ref, AlignSize offset, DesignatorElement** current,
											DesignatorElement **last, Expr *expr, BEValue *emitted_value)
{
	BEValue value;
	if (current > last)
	{
		if (emitted_value)
		{
			llvm_store_bevalue(c, ref, emitted_value);
			return;
		}
		if (expr->expr_kind == EXPR_CONST && expr->const_expr.const_kind == CONST_LIST)
		{
			llvm_emit_const_initialize_reference(c, ref, expr);
			return;
		}
		if (expr_is_init_list(expr))
		{
			llvm_emit_initialize_reference(c, ref, expr);
			return;
		}
		BEValue val;
		llvm_emit_expr(c, &val, expr);
		llvm_store_bevalue(c, ref, &val);
		return;
	}
	DesignatorElement *curr = current[0];
	switch (curr->kind)
	{
		case DESIGNATOR_FIELD:
		{
			Decl *decl = ref->type->canonical->decl->strukt.members[curr->index];
			offset += decl->offset;
			Type *type = decl->type->canonical;
			unsigned decl_alignment = decl->alignment;
			if (ref->type->type_kind == TYPE_UNION)
			{
				llvm_value_set_address_align(&value, llvm_emit_bitcast(c, ref->value, type_get_ptr(type)), type, type_min_alignment(offset, decl_alignment));
			}
			else
			{
				llvm_value_struct_gep(c, &value, ref, (unsigned)curr->index);
			}
			llvm_emit_initialize_designated(c, &value, offset, current + 1, last, expr, emitted_value);
			break;
		}
		case DESIGNATOR_ARRAY:
		{
			Type *type = ref->type->array.base;
			offset += (unsigned)curr->index * type_size(type);
			AlignSize alignment;
			LLVMValueRef ptr = llvm_emit_array_gep_raw(c, ref->value, llvm_get_type(c, ref->type), (unsigned)curr->index, ref->alignment, &alignment);
			llvm_value_set_address_align(&value, ptr, type, alignment);
			llvm_emit_initialize_designated(c, &value, offset, current + 1, last, expr, emitted_value);
			break;
		}
		case DESIGNATOR_RANGE:
			llvm_emit_initialize_designated_const_range(c, ref, offset, current, last, expr, emitted_value);
			break;
		default:
			UNREACHABLE
	}
}

static inline void llvm_emit_initialize_reference_designated(GenContext *c, BEValue *ref, Expr *expr)
{
	// Getting ready to initialize, get the real type.
	Type *real_type = type_lowering(ref->type);
	Expr **elements = expr->designated_init_list;
	assert(vec_size(elements));

	// Make sure we have an address.
	llvm_value_addr(c, ref);

	// Clear the memory if not union.
	if (real_type->type_kind != TYPE_UNION) llvm_emit_memclear(c, ref);

	// Now walk through the elements.
	VECEACH(elements, i)
	{
		Expr *designator = elements[i];
		DesignatorElement **last_element = designator->designator_expr.path + vec_size(designator->designator_expr.path) - 1;
		llvm_emit_initialize_designated(c, ref, 0, designator->designator_expr.path, last_element, designator->designator_expr.value, NULL);
	}
}
LLVMValueRef llvm_emit_const_bitstruct_array(GenContext *c, ConstInitializer *initializer)
{
	Decl *decl = initializer->type->decl;
	Type *base_type = decl->bitstruct.base_type->type;
	bool big_endian = platform_target.big_endian;
	if (decl->bitstruct.big_endian) big_endian = true;
	if (decl->bitstruct.little_endian) big_endian = false;
	LLVMValueRef data = LLVMConstNull(llvm_get_type(c, base_type));
	Decl **members = decl->strukt.members;
	MemberIndex count = (MemberIndex)vec_size(members);
	for (MemberIndex i = 0; i < count; i++)
	{
		Decl *member = members[i];
		unsigned start_bit = member->var.start_bit;
		unsigned end_bit = member->var.end_bit;
		Type *member_type = type_flatten(member->type);
		assert(initializer->init_struct[i]->kind == CONST_INIT_VALUE);
		Expr *expr = initializer->init_struct[i]->init_value;

		// Special case for bool
		if (member_type == type_bool)
		{
			assert(expr->expr_kind == EXPR_CONST && expr->const_expr.const_kind == CONST_BOOL);
			assert(start_bit == end_bit);

			// Completely skip zero.
			if (!expr->const_expr.b) continue;

			LLVMValueRef bit = llvm_emit_shl(c, LLVMConstInt(c->byte_type, 1, 0), start_bit % 8);
			unsigned byte = start_bit / 8;
			LLVMValueRef current_value = llvm_emit_extract_value(c, data, byte);
			data = llvm_emit_insert_value(c, data, LLVMConstOr(current_value, bit), byte);
			continue;
		}
		unsigned bit_size = end_bit - start_bit + 1;
		assert(bit_size > 0 && bit_size <= 128);
		BEValue val;
		llvm_emit_const_expr(c, &val, initializer->init_struct[i]->init_value);
		assert(val.kind == BE_VALUE);
		LLVMValueRef value = val.value;
		int start_byte = start_bit / 8;
		int end_byte = end_bit / 8;
		ByteSize member_type_bitsize = type_size(member_type) * 8;
		value = llvm_mask_low_bits(c, value, bit_size);
		if (big_endian && bit_size > 8)
		{
			value = llvm_bswap_non_integral(c, value, bit_size);
		}
		int bit_offset = start_bit % 8;
		for (int j = start_byte; j <= end_byte; j++)
		{
			LLVMValueRef to_or;
			if (j == start_byte)
			{
				to_or = llvm_emit_shl(c, value, bit_offset);
			}
			else
			{
				to_or = llvm_emit_lshr(c, value, j * 8 - (int)start_bit);
			}
			if (j == end_byte)
			{
				to_or = llvm_mask_low_bits(c, to_or, end_bit % 8 + 1);
			}
			if (member_type_bitsize > 8) to_or = LLVMConstTrunc(to_or, c->byte_type);
			LLVMValueRef current_value = llvm_emit_extract_value(c, data, (unsigned)j);
			data = llvm_emit_insert_value(c, data, LLVMConstOr(to_or, current_value), (unsigned)j);
		}
	}
	return data;
}

LLVMValueRef llvm_emit_const_bitstruct(GenContext *c, ConstInitializer *initializer)
{
	Decl *decl = initializer->type->decl;
	Type *base_type = decl->bitstruct.base_type->type;
	if (initializer->kind == CONST_INIT_ZERO) return llvm_get_zero(c, base_type);
	bool char_array = base_type->type_kind == TYPE_ARRAY;
	if (char_array)
	{
		return llvm_emit_const_bitstruct_array(c, initializer);
	}
	LLVMTypeRef llvm_base_type = llvm_get_type(c, base_type);
	LLVMValueRef result = LLVMConstNull(llvm_base_type);
	Decl **members = decl->strukt.members;
	MemberIndex count = (MemberIndex)vec_size(members);
	TypeSize base_type_size = type_size(base_type);
	TypeSize base_type_bitsize = base_type_size * 8;
	for (MemberIndex i = 0; i < count; i++)
	{
		Decl *member = members[i];
		unsigned start_bit = member->var.start_bit;
		unsigned end_bit = member->var.end_bit;
		unsigned bit_size = end_bit - start_bit + 1;
		assert(bit_size > 0 && bit_size <= 128);
		assert(initializer->init_struct[i]->kind == CONST_INIT_VALUE);
		BEValue entry;
		llvm_emit_const_expr(c, &entry, initializer->init_struct[i]->init_value);
		LLVMValueRef value = llvm_value_rvalue_store(c, &entry);
		value = llvm_zext_trunc(c, value, llvm_base_type);
		if (bit_size < base_type_bitsize)
		{
			LLVMValueRef mask = LLVMConstAllOnes(llvm_base_type);
			mask = LLVMConstLShr(mask, LLVMConstInt(llvm_base_type, base_type_bitsize - bit_size, 0));
			value = LLVMConstAnd(mask, value);
		}
		if (start_bit > 0)
		{
			value = LLVMConstShl(value, LLVMConstInt(llvm_base_type, start_bit, 0));
		}
		result = LLVMConstOr(value, result);
	}
	if (decl->bitstruct.little_endian && platform_target.big_endian)
	{
		return LLVMConstBswap(result);
	}
	if (decl->bitstruct.big_endian && !platform_target.big_endian)
	{
		return LLVMConstBswap(result);
	}
	return result;
}

static inline void llvm_emit_const_initialize_bitstruct_ref(GenContext *c, BEValue *ref, ConstInitializer *initializer)
{
	if (initializer->kind == CONST_INIT_ZERO)
	{
		llvm_emit_memclear(c, ref);
		return;
	}
	assert(initializer->kind == CONST_INIT_STRUCT);
	llvm_store_bevalue_raw(c, ref, llvm_emit_const_bitstruct(c, initializer));
}

/**
 * Initialize a constant aggregate type.
 */
static inline void llvm_emit_const_initialize_reference(GenContext *c, BEValue *ref, Expr *expr)
{
	assert(expr->expr_kind == EXPR_CONST && expr->const_expr.const_kind == CONST_LIST);
	ConstInitializer *initializer = expr->const_expr.list;
	assert(!type_is_vector(initializer->type) && "Vectors should be handled elsewhere.");
	if (initializer->type->type_kind == TYPE_BITSTRUCT)
	{
		llvm_emit_const_initialize_bitstruct_ref(c, ref, initializer);
		return;
	}
	if (initializer->kind == CONST_INIT_ZERO)
	{
		// In case of a zero, optimize.
		llvm_emit_memclear(c, ref);
		return;
	}
	// In case of small const initializers, or full arrays - use copy.
	if (initializer->kind == CONST_INIT_ARRAY_FULL || type_size(expr->type) <= 32)
	{
		llvm_emit_initialize_reference_temporary_const(c, ref, expr);
		return;
	}
	llvm_emit_initialize_reference_const(c, ref, expr);
	return;
}

/**
 * Initialize an aggregate type.
 *
 * There are three methods:
 * 1. Create a constant and store it in a global, followed by a memcopy from this global.
 *    this is what Clang does for elements up to 4 pointers wide.
 * 2. For empty elements, we do a memclear.
 * 3. For the rest use GEP into the appropriate elements.
 */
static inline void llvm_emit_initialize_reference(GenContext *c, BEValue *ref, Expr *expr)
{
	switch (expr->expr_kind)
	{
		case EXPR_INITIALIZER_LIST:
			llvm_emit_initialize_reference_list(c, ref, expr);
			break;
		case EXPR_DESIGNATED_INITIALIZER_LIST:
			llvm_emit_initialize_reference_designated(c, ref, expr);
			break;
		default:
			UNREACHABLE
	}
}

static inline void llvm_emit_inc_dec_change(GenContext *c, bool use_mod, BEValue *addr, BEValue *after, BEValue *before, Expr *expr, int diff)
{
	EMIT_LOC(c, expr);
	Type *type = type_reduced_from_expr(expr);

	// Copy the address and make it a value.
	BEValue value = *addr;
	llvm_value_rvalue(c, &value);

	// Store the original value if we want it
	if (before) *before = value;

	LLVMValueRef after_value;

	switch (type->type_kind)
	{
		case TYPE_POINTER:
		{
			// Use byte here, we don't need a big offset.
			LLVMValueRef add = LLVMConstInt(diff < 0 ? llvm_get_type(c, type_ichar) : llvm_get_type(c, type_char), (unsigned long long)diff, diff < 0);
			after_value = llvm_emit_pointer_gep_raw(c, llvm_get_pointee_type(c, type), value.value, add);
			break;
		}
		case ALL_FLOATS:
		{
			// We allow inc/dec on floats, which is same as f += 1.0 or f -= 1.0
			LLVMTypeRef llvm_type = llvm_get_type(c, type);
			LLVMValueRef add = LLVMConstReal(llvm_type, (double)diff);
			after_value = LLVMBuildFAdd(c->builder, value.value, add, "fincdec");
			break;
		}
		case ALL_INTS:
		{
			// Instead of negative numbers do dec/inc with a positive number.
			LLVMTypeRef llvm_type = llvm_get_type(c, type);
			LLVMValueRef diff_value = LLVMConstInt(llvm_type, 1, false);
			after_value = diff > 0
					? llvm_emit_add_int(c, type, value.value, diff_value, TOKLOC(expr->span.loc))
					: llvm_emit_sub_int(c, type, value.value, diff_value, TOKLOC(expr->span.loc));
			break;
		}
		case TYPE_VECTOR:
		{
			Type *element = type->vector.base;
			LLVMValueRef diff_value;
			bool is_integer = type_is_integer(element);
			if (is_integer)
			{
				diff_value = LLVMConstInt(llvm_get_type(c, element), 1, false);
			}
			else
			{
				diff_value = LLVMConstReal(llvm_get_type(c, element), diff);
			}
			ArraySize width = type->vector.len;
			LLVMValueRef val = LLVMGetUndef(llvm_get_type(c, type));
			for (ArraySize i = 0; i < width; i++)
			{
				val = LLVMConstInsertElement(val, diff_value, llvm_const_int(c, type_usize, i));
			}
			if (is_integer)
			{
				after_value = diff > 0
						? llvm_emit_add_int(c, type, value.value, val, TOKLOC(expr->span.loc))
						: llvm_emit_sub_int(c, type, value.value, val, TOKLOC(expr->span.loc));
			}
			else
			{
				after_value = LLVMBuildFAdd(c->builder, value.value, val, "fincdec");
			}
			break;
		}
		default:
			UNREACHABLE
	}

	// Store the result aligned.
	llvm_store_bevalue_raw(c, addr, after_value);
	if (after) llvm_value_set(after, after_value, addr->type);
}

/**
 * This method implements the common ++x and --x operators, as well as the --%x and ++%x
 * that have wrapping behaviour. See llvm_emit_post_inc_dec for more discussion.
 */
static inline void llvm_emit_pre_inc_dec(GenContext *c, BEValue *value, Expr *expr, int diff, bool use_mod)
{
	// Pull out the address, also allowing temporaries.
	BEValue addr;
	llvm_emit_expr(c, &addr, expr);
	llvm_value_addr(c, &addr);

	// Set the value to the new value.
	llvm_emit_inc_dec_change(c, use_mod, &addr, value, NULL, expr, diff);
}


/**
 * Emit the common x++ and x-- operations.
 * Normally overflow/underflow is considered UB, which is why C3 provides x%++ and x%-- for wrapping.
 * We could also provide methods for the same but where it would cap on overflow.
 */
static inline void llvm_emit_post_inc_dec(GenContext *c, BEValue *value, Expr *expr, int diff, bool use_mod)
{
	// Retrieve the address, creating a temp in case this is
	// a temporary value (this gives us a lot of flexibility for temporaries)
	BEValue addr;
	llvm_emit_expr(c, &addr, expr);
	llvm_value_addr(c, &addr);

	// Perform the actual dec/inc to generate the new value.
	llvm_emit_inc_dec_change(c, use_mod, &addr, NULL, value, expr, diff);
}


static void gencontext_emit_unary_expr(GenContext *c, BEValue *value, Expr *expr)
{
	Type *type = type_reduced_from_expr(expr->unary_expr.expr);
	Expr *inner = expr->unary_expr.expr;
	LLVMValueRef llvm_value;
	switch (expr->unary_expr.operator)
	{
		case UNARYOP_ERROR:
			FATAL_ERROR("Illegal unary op %s", expr->unary_expr.operator);
		case UNARYOP_NOT:
			llvm_emit_expr(c, value, inner);
			if (type_is_vector(type))
			{
				llvm_value_rvalue(c, value);
				Type *vec_type = type_vector_type(type);
				if (type_is_float(vec_type))
				{
					llvm_value = LLVMBuildFCmp(c->builder, LLVMRealUNE, value->value, llvm_get_zero(c, type), "not");
				}
				else
				{
					llvm_value = LLVMBuildICmp(c->builder, LLVMIntEQ, value->value, llvm_get_zero(c, type), "not");
				}
				Type *res_type = type_get_vector_bool(type);
				llvm_value = LLVMBuildSExt(c->builder, llvm_value, llvm_get_type(c, res_type), "");
				llvm_value_set(value, llvm_value, res_type);
				return;
			}
			switch (type->type_kind)
			{
				case ALL_FLOATS:
					llvm_value_rvalue(c, value);
					llvm_value = LLVMBuildFCmp(c->builder, LLVMRealUNE, value->value, llvm_get_zero(c, type), "not");
					break;
				case TYPE_BOOL:
					llvm_value_rvalue(c, value);
					llvm_value = LLVMBuildNot(c->builder, value->value, "not");
					break;
				case TYPE_SUBARRAY:
					if (value->kind != BE_VALUE)
					{
						llvm_emit_len_for_expr(c, value, value);
						llvm_value_rvalue(c, value);
						llvm_value = value->value;
					}
					else
					{
						llvm_value = LLVMBuildExtractValue(c->builder, value->value, 1, "len");
					}
					llvm_value = LLVMBuildIsNull(c->builder, llvm_value, "not");
					break;
				case ALL_INTS:
				case TYPE_POINTER:
					llvm_value_rvalue(c, value);
					llvm_value = LLVMBuildIsNull(c->builder, value->value, "not");
					break;
				default:
					DEBUG_LOG("Unexpectedly tried to not %s", type_quoted_error_string(inner->type));
					UNREACHABLE
			}
			llvm_value_set_bool(value, llvm_value);
			return;
		case UNARYOP_BITNEG:
			llvm_emit_expr(c, value, inner);
			llvm_value_rvalue(c, value);
			value->value = LLVMBuildNot(c->builder, value->value, "bnot");
			return;
		case UNARYOP_NEG:
			llvm_emit_expr(c, value, inner);
			llvm_value_rvalue(c, value);
			if (type_is_float(type))
			{
				value->value = LLVMBuildFNeg(c->builder, value->value, "fneg");
				return;
			}
			assert(type->canonical != type_bool);
			assert(!type_is_unsigned(type));
			llvm_emit_expr(c, value, expr->unary_expr.expr);
			llvm_value_rvalue(c, value);
			if (active_target.feature.trap_on_wrap)
			{
				LLVMValueRef zero = llvm_get_zero(c, expr->unary_expr.expr->type);
				LLVMTypeRef type_to_use = llvm_get_type(c, type->canonical);
				LLVMValueRef args[2] = { zero, value->value };
				LLVMValueRef call_res = llvm_emit_call_intrinsic(c, intrinsic_id.ssub_overflow,
				                                                 &type_to_use, 1, args, 2);
				value->value = LLVMBuildExtractValue(c->builder, call_res, 0, "");
				LLVMValueRef ok = LLVMBuildExtractValue(c->builder, call_res, 1, "");
				llvm_emit_panic_on_true(c, ok, "Signed negation overflow", TOKLOC(expr->span.loc));
				return;
			}
			value->value = LLVMBuildNeg(c->builder, value->value, "neg");
			return;
		case UNARYOP_TADDR:
		case UNARYOP_ADDR:
			llvm_emit_expr(c, value, inner);
			// Create an addr
			llvm_value_addr(c, value);
			// Transform to value
			value->kind = BE_VALUE;
			value->type = type_lowering(expr->type);
			return;
		case UNARYOP_DEREF:
			llvm_emit_expr(c, value, inner);
			// Load the pointer value.
			llvm_value_rvalue(c, value);
			// Convert pointer to address
			value->kind = BE_ADDRESS;
			value->type = type_lowering(expr->type);
			return;
		case UNARYOP_INC:
			llvm_emit_pre_inc_dec(c, value, inner, 1, false);
			return;
		case UNARYOP_DEC:
			llvm_emit_pre_inc_dec(c, value, inner, -1, false);
			return;
	}
	UNREACHABLE
}

void llvm_emit_len_for_expr(GenContext *c, BEValue *be_value, BEValue *expr_to_len)
{
	llvm_value_addr(c, expr_to_len);
	switch (expr_to_len->type->type_kind)
	{
		case TYPE_SUBARRAY:
		{
			LLVMTypeRef subarray_type = llvm_get_type(c, expr_to_len->type);
			AlignSize alignment;
			LLVMValueRef len_addr = llvm_emit_struct_gep_raw(c,
			                                                 expr_to_len->value,
			                                                 subarray_type,
			                                                 1,
			                                                 expr_to_len->alignment,
			                                                 &alignment);
			llvm_value_set_address_align(be_value, len_addr, type_usize, alignment);
			break;
		}
		case TYPE_ARRAY:
			llvm_value_set(be_value, llvm_const_int(c, type_usize, expr_to_len->type->array.len), type_usize);
			break;
		case TYPE_STRLIT:
			TODO
			break;
		default:
			UNREACHABLE
	}
}
static void llvm_emit_len(GenContext *c, BEValue *be_value, Expr *expr)
{
	llvm_emit_expr(c, be_value, expr->len_expr.inner);
	llvm_emit_len_for_expr(c, be_value, be_value);
}

static void llvm_emit_trap_negative(GenContext *c, Expr *expr, LLVMValueRef value, const char *error)
{
	if (!active_target.feature.safe_mode) return;
	if (type_is_integer_unsigned(expr->type->canonical)) return;

	LLVMValueRef zero = llvm_const_int(c, expr->type, 0);
	LLVMValueRef ok = LLVMBuildICmp(c->builder, LLVMIntSLT, value, zero, "underflow");
	llvm_emit_panic_on_true(c, ok, error, TOKLOC(expr->span.loc));
}

static void llvm_emit_trap_zero(GenContext *c, Type *type, LLVMValueRef value, const char *error, SourceLocation *loc)
{
	if (!active_target.feature.safe_mode) return;

	LLVMValueRef zero = llvm_get_zero(c, type);
	LLVMValueRef ok = type_is_integer(type) ? LLVMBuildICmp(c->builder, LLVMIntEQ, value, zero, "zero") : LLVMBuildFCmp(c->builder, LLVMRealUEQ, value, zero, "zero");
	llvm_emit_panic_on_true(c, ok, error, loc);
}


static void llvm_emit_trap_invalid_shift(GenContext *c, LLVMValueRef value, Type *type, const char *error, SourceLocation *loc)
{
	if (!active_target.feature.safe_mode) return;
	unsigned type_bit_size = type_size(type) * 8;
	LLVMValueRef max = llvm_const_int(c, type, type_bit_size);
	if (type_is_unsigned(type))
	{
		LLVMValueRef equal_or_greater = LLVMBuildICmp(c->builder, LLVMIntUGE, value, max, "shift_exceeds");
		llvm_emit_panic_on_true(c, equal_or_greater, error, loc);
		return;
	}
	LLVMValueRef zero = llvm_const_int(c, type, 0);
	LLVMValueRef negative = LLVMBuildICmp(c->builder, LLVMIntSLT, value, zero, "shift_underflow");
	llvm_emit_panic_on_true(c, negative, error, loc);
	LLVMValueRef equal_or_greater = LLVMBuildICmp(c->builder, LLVMIntSGE, value, max, "shift_exceeds");
	llvm_emit_panic_on_true(c, equal_or_greater, error, loc);
}

static void llvm_emit_slice_values(GenContext *c, Expr *slice, BEValue *parent_ref, BEValue *start_ref, BEValue *end_ref)
{
	assert(slice->expr_kind == EXPR_SLICE);

	Expr *parent_expr = slice->slice_expr.expr;
	Type *parent_type = parent_expr->type->canonical;
	BEValue parent_addr_x;
	llvm_emit_expr(c, &parent_addr_x, parent_expr);
	llvm_value_addr(c, &parent_addr_x);
	LLVMValueRef parent_addr = parent_addr_x.value;
	LLVMValueRef parent_load_value = NULL;
	LLVMValueRef parent_base;
	switch (parent_type->type_kind)
	{
		case TYPE_POINTER:
			parent_load_value = parent_base = gencontext_emit_load(c, parent_type, parent_addr);
			break;
		case TYPE_SUBARRAY:
			parent_load_value = gencontext_emit_load(c, parent_type, parent_addr);
			parent_base = LLVMBuildExtractValue(c->builder, parent_load_value, 0, "");
			break;
		case TYPE_ARRAY:
			parent_base = parent_addr;
			break;
		case TYPE_STRLIT:
			TODO
		default:
			UNREACHABLE
	}
	// Endpoints
	Expr *start = slice->slice_expr.start;
	Expr *end = slice->slice_expr.end;

	// Emit the start and end
	Type *start_type = start->type->canonical;
	BEValue start_index;
	llvm_emit_expr(c, &start_index, start);
	llvm_value_rvalue(c, &start_index);

	BEValue len = { .value = NULL };
	if (!end || slice->slice_expr.start_from_back || slice->slice_expr.end_from_back || active_target.feature.safe_mode)
	{
		switch (parent_type->type_kind)
		{
			case TYPE_POINTER:
				len.value = NULL;
				break;
			case TYPE_SUBARRAY:
				assert(parent_load_value);
				llvm_value_set(&len, LLVMBuildExtractValue(c->builder, parent_load_value, 1, ""), type_usize);
				break;
			case TYPE_ARRAY:
				llvm_value_set_int(c, &len, type_usize, parent_type->array.len);
				break;
			case TYPE_STRLIT:
				TODO
			default:
				UNREACHABLE
		}
	}

	// Walk from end if it is slice from the back.
	if (slice->slice_expr.start_from_back)
	{
		start_index.value = llvm_emit_sub_int(c, start_type, len.value, start_index.value, TOKLOC(slice->span.loc));
	}

	// Check that index does not extend beyond the length.
	if (parent_type->type_kind != TYPE_POINTER && active_target.feature.safe_mode)
	{

		assert(len.value);
		BEValue exceeds_size;
		llvm_emit_int_comparison(c, &exceeds_size, &start_index, &len, BINARYOP_GE);
		llvm_emit_panic_if_true(c, &exceeds_size, "Index exceeds array length.", TOKLOC(slice->span.loc));
	}

	// Insert trap for negative start offset for non pointers.
	if (parent_type->type_kind != TYPE_POINTER)
	{
		llvm_emit_trap_negative(c, start, start_index.value, "Negative index");
	}

	Type *end_type;
	BEValue end_index;

	if (end)
	{
		// Get the index.
		llvm_emit_expr(c, &end_index, end);
		llvm_value_rvalue(c, &end_index);
		end_type = end->type->canonical;

		// Reverse if it is "from back"
		if (slice->slice_expr.end_from_back)
		{
			end_index.value = llvm_emit_sub_int(c, end_type, len.value, end_index.value, TOKLOC(slice->span.loc));
			llvm_value_rvalue(c, &end_index);
		}

		// This will trap any bad negative index, so we're fine.
		if (active_target.feature.safe_mode)
		{
			BEValue excess;
			llvm_emit_int_comparison(c, &excess, &start_index, &end_index, BINARYOP_GT);
			llvm_emit_panic_if_true(c, &excess, "Negative size", TOKLOC(slice->span.loc));

			if (len.value)
			{

				llvm_emit_int_comparison(c, &excess, &len, &end_index, BINARYOP_LT);
				llvm_emit_panic_if_true(c, &excess, "Size exceeds index", TOKLOC(slice->span.loc));
			}
		}
	}
	else
	{
		assert(len.value && "Pointer should never end up here.");
		// Otherwise everything is fine and dandy. Our len - 1 is our end index.
		end_index.value = LLVMBuildSub(c->builder, len.value, LLVMConstInt(LLVMTypeOf(len.value), 1, false), "");
		end_type = type_usize;
	}

	llvm_value_set(end_ref, end_index.value, end_type);
	llvm_value_set(start_ref, start_index.value, start_type);
	llvm_value_set_address_align(parent_ref, parent_base, parent_type, type_abi_alignment(parent_type));
}

static void gencontext_emit_slice(GenContext *c, BEValue *be_value, Expr *expr)
{
	// Use general function to get all the values we need (a lot!)
	BEValue parent;
	BEValue start;
	BEValue end;
	llvm_emit_slice_values(c, expr, &parent, &start, &end);
	llvm_value_rvalue(c, &start);
	llvm_value_rvalue(c, &end);


	// Calculate the size
	LLVMValueRef size = LLVMBuildSub(c->builder, LLVMBuildAdd(c->builder, end.value, llvm_const_int(c, start.type, 1), ""), start.value, "size");
	LLVMValueRef start_pointer;
	switch (parent.type->type_kind)
	{
		case TYPE_ARRAY:
		{
			Type *pointer_type = type_get_ptr(parent.type->array.base);
			// Move pointer
			AlignSize alignment;
			start_pointer = llvm_emit_array_gep_raw_index(c, parent.value, llvm_get_type(c, parent.type), start.value, type_abi_alignment(parent.type), &alignment);
			start_pointer = llvm_emit_bitcast(c, start_pointer, pointer_type);
			break;
		}
		case TYPE_SUBARRAY:
			start_pointer = llvm_emit_pointer_inbounds_gep_raw(c, llvm_get_type(c, parent.type->array.base), parent.value, start.value);
			break;
		case TYPE_POINTER:
			start_pointer = llvm_emit_pointer_inbounds_gep_raw(c, llvm_get_pointee_type(c, parent.type), parent.value, start.value);
			break;
		default:
			TODO
	}

	// Create a new subarray type
	llvm_value_set(be_value, llvm_emit_aggregate_value(c, expr->type, start_pointer, size, NULL), expr->type);
}

static void llvm_emit_slice_assign(GenContext *c, BEValue *be_value, Expr *expr)
{
	// We will be replacing the slice assign with code that roughly looks like this:
	// size_t end = slice_end;
	// size_t slice_current = slice_start;
	// while (slice_current <= end) pointer[slice_current++] = value;

	// First, find the value assigned.
	Expr *assigned_value = expr->slice_assign_expr.right;
	llvm_emit_expr(c, be_value, assigned_value);

	BEValue parent;
	BEValue start;
	BEValue end;
	// Use general function to get all the values we need (a lot!)
	llvm_emit_slice_values(c, expr->slice_assign_expr.left, &parent, &start, &end);
	llvm_value_rvalue(c, &start);
	llvm_value_rvalue(c, &end);

	if (LLVMIsConstant(start.value) && LLVMIsConstant(end.value))
	{
		assert(type_is_integer(start.type) && type_is_integer(end.type));
		bool signed_start = type_is_signed(start.type);
		bool signed_end = type_is_signed(end.type);
		uint64_t start_val = signed_start ? (uint64_t)LLVMConstIntGetSExtValue(start.value)
		                                  : (uint64_t)LLVMConstIntGetZExtValue(start.value);
		uint64_t end_val = signed_end ? (uint64_t)LLVMConstIntGetSExtValue(end.value)
		                              : (uint64_t)LLVMConstIntGetZExtValue(end.value);
		assert(start_val <= INT64_MAX);
		assert(end_val <= INT64_MAX);
		if (start_val > end_val) return;
		if (end_val - start_val < SLICE_MAX_UNROLL)
		{
			BEValue addr;
			BEValue offset_val;
			for (uint64_t i = start_val; i <= end_val; i++)
			{
				llvm_value_set_int(c, &offset_val, type_usize, i);
				llvm_emit_subscript_addr_with_base(c, &addr, &parent, &offset_val, TOKLOC(expr->span.loc));

				// And store the value.
				llvm_store_bevalue(c, &addr, be_value);
			}
			return;
		}
	}

	// We will need to iterate for the general case.
	LLVMBasicBlockRef start_block = c->current_block;
	LLVMBasicBlockRef cond_block = llvm_basic_block_new(c, "cond");
	LLVMBasicBlockRef exit_block = llvm_basic_block_new(c, "exit");
	LLVMBasicBlockRef assign_block = llvm_basic_block_new(c, "assign");

	// First jump to the cond block.
	llvm_emit_br(c, cond_block);
	llvm_emit_block(c, cond_block);

	// We emit a phi here: value is either the start value (start_offset) or the next value (next_offset)
	// but we haven't generated the latter yet, so we defer that.
	EMIT_LOC(c, expr);
	LLVMValueRef offset = LLVMBuildPhi(c->builder, llvm_get_type(c, start.type), "");
	BEValue offset_val;
	llvm_value_set(&offset_val, offset, start.type);

	// Check if we're not at the end.
	BEValue value;
	llvm_emit_int_comp(c, &value, start.type, end.type, offset, end.value, BINARYOP_LE);

	// If jump to the assign block if we're not at the end index.
	EMIT_LOC(c, expr);
	llvm_emit_cond_br(c, &value, assign_block, exit_block);

	// Emit the assign.
	llvm_emit_block(c, assign_block);
	// Reuse this calculation
	BEValue addr;
	llvm_emit_subscript_addr_with_base(c, &addr, &parent, &offset_val, TOKLOC(expr->span.loc));

	// And store the value.
	llvm_store_bevalue(c, &addr, be_value);

	// Create the new offset
	LLVMValueRef next_offset = llvm_emit_add_int(c, start.type, offset, llvm_const_int(c, start.type, 1), TOKLOC(expr->span.loc));

	// And jump back
	llvm_emit_br(c, cond_block);

	// Finally set up our phi
	LLVMValueRef logic_values[2] = { start.value, next_offset };
	LLVMBasicBlockRef blocks[2] = { start_block, assign_block };
	LLVMAddIncoming(offset, logic_values, blocks, 2);

	// And emit the exit block.
	llvm_emit_block(c, exit_block);

}

static void gencontext_emit_logical_and_or(GenContext *c, BEValue *be_value, Expr *expr, BinaryOp op)
{
	// Value *ScalarExprEmitter::VisitBinLAnd(const BinaryOperator *E)
	// For vector implementation.

	// Set up basic blocks, following Cone
	LLVMBasicBlockRef start_block;
	LLVMBasicBlockRef phi_block = llvm_basic_block_new(c, op == BINARYOP_AND ? "and.phi" : "or.phi");
	LLVMBasicBlockRef rhs_block = llvm_basic_block_new(c, op == BINARYOP_AND ? "and.rhs" : "or.rhs");

	// Generate left-hand condition and conditional branch
	llvm_emit_expr(c, be_value, expr->binary_expr.left);
	llvm_value_rvalue(c, be_value);

	start_block = c->current_block;

	if (op == BINARYOP_AND)
	{
		llvm_emit_cond_br(c, be_value, rhs_block, phi_block);
	}
	else
	{
		llvm_emit_cond_br(c, be_value, phi_block, rhs_block);
	}

	llvm_emit_block(c, rhs_block);
	BEValue rhs_value;
	llvm_emit_expr(c, &rhs_value, expr->binary_expr.right);
	llvm_value_rvalue(c, &rhs_value);

	LLVMBasicBlockRef end_block = c->current_block;
	llvm_emit_br(c, phi_block);

	// Generate phi
	llvm_emit_block(c, phi_block);

	// Simplify for LLVM by entering the constants we already know of.
	LLVMValueRef result_on_skip = LLVMConstInt(c->bool_type, op == BINARYOP_AND ? 0 : 1, 0);

	// One possibility here is that a return happens inside of the expression.
	if (!end_block)
	{
		llvm_value_set_bool(be_value, result_on_skip);
		return;
	}
	LLVMValueRef phi = LLVMBuildPhi(c->builder, c->bool_type, "val");
	LLVMValueRef logic_values[2] = { result_on_skip, rhs_value.value };
	LLVMBasicBlockRef blocks[2] = { start_block, end_block };
	LLVMAddIncoming(phi, logic_values, blocks, 2);

	llvm_value_set_bool(be_value, phi);
}

void llvm_emit_int_comp_zero(GenContext *c, BEValue *result, BEValue *lhs, BinaryOp binary_op)
{
	BEValue zero;
	llvm_value_set_int(c, &zero, lhs->type, 0);
	llvm_emit_int_comparison(c, result, lhs, &zero, binary_op);
}

void llvm_emit_int_comparison(GenContext *c, BEValue *result, BEValue *lhs, BEValue *rhs, BinaryOp binary_op)
{
	llvm_value_rvalue(c, lhs);
	llvm_value_rvalue(c, rhs);
	llvm_emit_int_comp(c, result, lhs->type, rhs->type, lhs->value, rhs->value, binary_op);
}
void llvm_emit_int_comp(GenContext *c, BEValue *result, Type *lhs_type, Type *rhs_type, LLVMValueRef lhs_value, LLVMValueRef rhs_value, BinaryOp binary_op)
{
	bool lhs_signed, rhs_signed;
	Type *vector_type = type_vector_type(lhs_type);
	if (vector_type)
	{
		lhs_signed = type_is_signed(vector_type);
		rhs_signed = type_is_signed(type_vector_type(rhs_type));
	}
	else
	{
		assert(type_is_integer_or_bool_kind(lhs_type));
		lhs_signed = type_is_signed(lhs_type);
		rhs_signed = type_is_signed(rhs_type);
	}
	if (lhs_signed != rhs_signed)
	{
		// Swap sides if needed.
		if (!lhs_signed)
		{
			Type *temp = lhs_type;
			lhs_type = rhs_type;
			rhs_type = temp;
			LLVMValueRef temp_val = lhs_value;
			lhs_value = rhs_value;
			rhs_value = temp_val;
			switch (binary_op)
			{
				case BINARYOP_GE:
					binary_op = BINARYOP_LE;
					break;
				case BINARYOP_GT:
					binary_op = BINARYOP_LT;
					break;
				case BINARYOP_LE:
					binary_op = BINARYOP_GE;
					break;
				case BINARYOP_LT:
					binary_op = BINARYOP_GT;
					break;
				default:
					break;
			}
			lhs_signed = true;
			rhs_signed = false;
		}
	}
	if (lhs_signed && !rhs_signed && !vector_type && LLVMIsConstant(lhs_value) && type_size(lhs_type) <= 64)
	{
		long long val = LLVMConstIntGetSExtValue(lhs_value);
		if (val < 0)
		{
			switch (binary_op)
			{
				case BINARYOP_EQ:
				case BINARYOP_GE:
				case BINARYOP_GT:
					llvm_value_set(result, llvm_const_int(c, type_bool, 0), type_bool);
					return;
				case BINARYOP_NE:
				case BINARYOP_LE:
				case BINARYOP_LT:
					llvm_value_set(result, llvm_const_int(c, type_bool, 1), type_bool);
					return;
				default:
					UNREACHABLE
			}
		}
		lhs_signed = false;
	}

	if (!lhs_signed)
	{
		assert(lhs_signed == rhs_signed);
		// Right and left side are both unsigned.
		LLVMValueRef value;
		switch (binary_op)
		{
			case BINARYOP_EQ:
				value = LLVMBuildICmp(c->builder, LLVMIntEQ, lhs_value, rhs_value, "eq");
				break;
			case BINARYOP_NE:
				value = LLVMBuildICmp(c->builder, LLVMIntNE, lhs_value, rhs_value, "neq");
				break;
			case BINARYOP_GE:
				value = LLVMBuildICmp(c->builder, LLVMIntUGE, lhs_value, rhs_value, "ge");
				break;
			case BINARYOP_GT:
				value = LLVMBuildICmp(c->builder, LLVMIntUGT, lhs_value, rhs_value, "gt");
				break;
			case BINARYOP_LE:
				value = LLVMBuildICmp(c->builder, LLVMIntULE, lhs_value, rhs_value, "le");
				break;
			case BINARYOP_LT:
				value = LLVMBuildICmp(c->builder, LLVMIntULT, lhs_value, rhs_value, "lt");
				break;
			default:
				UNREACHABLE
		}
		if (vector_type)
		{
			llvm_convert_vector_comparison(c, result, value, lhs_type);
			return;
		}
		llvm_value_set_bool(result, value);
		return;
	}


	// Left side is signed.
	LLVMValueRef comp_value;
	LLVMValueRef check_value;

	switch (binary_op)
	{
		case BINARYOP_EQ:
			comp_value = LLVMBuildICmp(c->builder, LLVMIntEQ, lhs_value, rhs_value, "eq");
			break;
		case BINARYOP_NE:
			comp_value = LLVMBuildICmp(c->builder, LLVMIntNE, lhs_value, rhs_value, "neq");
			break;
		case BINARYOP_GE:
			comp_value = LLVMBuildICmp(c->builder, LLVMIntSGE, lhs_value, rhs_value, "ge");
			break;
		case BINARYOP_GT:
			comp_value = LLVMBuildICmp(c->builder, LLVMIntSGT, lhs_value, rhs_value, "gt");
			break;
		case BINARYOP_LE:
			comp_value = LLVMBuildICmp(c->builder, LLVMIntSLE, lhs_value, rhs_value, "le");
			break;
		case BINARYOP_LT:
			comp_value = LLVMBuildICmp(c->builder, LLVMIntSLT, lhs_value, rhs_value, "lt");
			break;
		default:
			UNREACHABLE
	}

	// If right side is also signed then this is fine.
	if (rhs_signed)
	{
		if (vector_type)
		{
			llvm_convert_vector_comparison(c, result, comp_value, lhs_type);
			return;
		}
		llvm_value_set_bool(result, comp_value);
		return;
	}

	// Otherwise, special handling for left side signed, right side unsigned.
	LLVMValueRef zero = llvm_get_zero(c, lhs_type);
	switch (binary_op)
	{
		case BINARYOP_EQ:
			// Only true if lhs >= 0
			check_value = LLVMBuildICmp(c->builder, LLVMIntSGE, lhs_value, zero, "check");
			comp_value = LLVMBuildAnd(c->builder, check_value, comp_value, "siui-eq");
			break;
		case BINARYOP_NE:
			// Always true if lhs < 0
			check_value = LLVMBuildICmp(c->builder, LLVMIntSLT, lhs_value, zero, "check");
			comp_value = LLVMBuildOr(c->builder, check_value, comp_value, "siui-ne");
			break;
		case BINARYOP_GE:
			// Only true if rhs >= 0 when regarded as a signed integer
			check_value = LLVMBuildICmp(c->builder, LLVMIntSGE, rhs_value, zero, "check");
			comp_value = LLVMBuildAnd(c->builder, check_value, comp_value, "siui-ge");
			break;
		case BINARYOP_GT:
			// Only true if rhs >= 0 when regarded as a signed integer
			check_value = LLVMBuildICmp(c->builder, LLVMIntSGE, rhs_value, zero, "check");
			comp_value = LLVMBuildAnd(c->builder, check_value, comp_value, "siui-gt");
			break;
		case BINARYOP_LE:
			// Always true if rhs < 0 when regarded as a signed integer
			check_value = LLVMBuildICmp(c->builder, LLVMIntSLT, rhs_value, zero, "check");
			comp_value = LLVMBuildOr(c->builder, check_value, comp_value, "siui-le");
			break;
		case BINARYOP_LT:
			// Always true if rhs < 0 when regarded as a signed integer
			check_value = LLVMBuildICmp(c->builder, LLVMIntSLT, rhs_value, zero, "check");
			comp_value = LLVMBuildOr(c->builder, check_value, comp_value, "siui-lt");
			break;
		default:
			UNREACHABLE
	}
	if (vector_type)
	{
		llvm_convert_vector_comparison(c, result, comp_value, lhs_type);
		return;
	}
	llvm_value_set_bool(result, comp_value);
}

static void llvm_emit_ptr_comparison(GenContext *c, BEValue *result, BEValue *lhs, BEValue *rhs, BinaryOp binary_op)
{
	llvm_value_rvalue(c, lhs);
	llvm_value_rvalue(c, rhs);
	LLVMValueRef lhs_value = lhs->value;
	LLVMValueRef rhs_value = rhs->value;
	LLVMValueRef val;
	switch (binary_op)
	{
		case BINARYOP_EQ:
			val = LLVMBuildICmp(c->builder, LLVMIntEQ, lhs_value, rhs_value, "eq");
			break;
		case BINARYOP_NE:
			val = LLVMBuildICmp(c->builder, LLVMIntNE, lhs_value, rhs_value, "neq");
			break;
		case BINARYOP_GE:
			val = LLVMBuildICmp(c->builder, LLVMIntUGE, lhs_value, rhs_value, "ge");
			break;
		case BINARYOP_GT:
			val = LLVMBuildICmp(c->builder, LLVMIntUGT, lhs_value, rhs_value, "gt");
			break;
		case BINARYOP_LE:
			val = LLVMBuildICmp(c->builder, LLVMIntULE, lhs_value, rhs_value, "le");
			break;
		case BINARYOP_LT:
			val = LLVMBuildICmp(c->builder, LLVMIntULT, lhs_value, rhs_value, "lt");
			break;
		default:
			UNREACHABLE
	}
	llvm_value_set_bool(result, val);
}

static inline LLVMValueRef llvm_fixup_shift_rhs(GenContext *c, LLVMValueRef left, LLVMValueRef right, SourceLocation *loc)
{
	LLVMTypeRef left_type = LLVMTypeOf(left);
	LLVMTypeRef right_type = LLVMTypeOf(right);
	if (left_type == right_type) return right;
	if (LLVMStoreSizeOfType(c->target_data, left_type) < LLVMStoreSizeOfType(c->target_data, right_type))
	{
		return LLVMBuildTrunc(c->builder, right, left_type, "");
	}
	return LLVMBuildZExt(c->builder, right, left_type, "");
}

static inline LLVMValueRef llvm_emit_mult_int(GenContext *c, Type *type, LLVMValueRef left, LLVMValueRef right, SourceLocation *loc)
{
	if (active_target.feature.trap_on_wrap)
	{
		LLVMTypeRef type_to_use = llvm_get_type(c, type);
		LLVMValueRef args[2] = { left, right };
		LLVMTypeRef types[2] = { type_to_use, type_to_use };
		unsigned operation = type_is_integer_unsigned(type) ? intrinsic_id.umul_overflow
				: intrinsic_id.smul_overflow;
		LLVMValueRef call_res = llvm_emit_call_intrinsic(c,
		                                                 operation,
		                                                 types,
		                                                 1,
		                                                 args,
		                                                 2);
		LLVMValueRef val = LLVMBuildExtractValue(c->builder, call_res, 0, "mul");
		LLVMValueRef ok = LLVMBuildExtractValue(c->builder, call_res, 1, "");
		llvm_emit_panic_on_true(c, ok, "Integer multiplication overflow", loc);
		return val;
	}
	return LLVMBuildMul(c->builder, left, right, "mul");
}

static void llvm_emit_subarray_comp(GenContext *c, BEValue *be_value, BEValue *lhs, BEValue *rhs, BinaryOp binary_op)
{
	bool want_match = binary_op == BINARYOP_EQ;

	Type *array_base_type = type_lowering(lhs->type->array.base);
	Type *array_base_pointer = type_get_ptr(array_base_type);
	LLVMTypeRef llvm_base_type = llvm_get_type(c, array_base_type);

	LLVMBasicBlockRef exit = llvm_basic_block_new(c, "subarray_cmp_exit");
	LLVMBasicBlockRef value_cmp = llvm_basic_block_new(c, "subarray_cmp_values");
	LLVMBasicBlockRef loop_begin = llvm_basic_block_new(c, "subarray_loop_start");
	LLVMBasicBlockRef comparison = llvm_basic_block_new(c, "subarray_loop_comparison");
	LLVMBasicBlockRef no_match_block;
	LLVMBasicBlockRef all_match_block;
	LLVMBasicBlockRef match_fail_block;

	llvm_value_rvalue(c, lhs);
	llvm_value_rvalue(c, rhs);
	BEValue lhs_len;
	BEValue rhs_len;
	llvm_value_set(&lhs_len, LLVMBuildExtractValue(c->builder, lhs->value, 1, ""), type_usize);
	llvm_value_set(&rhs_len, LLVMBuildExtractValue(c->builder, rhs->value, 1, ""), type_usize);
	BEValue lhs_value;
	BEValue rhs_value;
	llvm_value_set(&lhs_value, LLVMBuildExtractValue(c->builder, lhs->value, 0, ""), array_base_pointer);
	llvm_value_set(&rhs_value, LLVMBuildExtractValue(c->builder, rhs->value, 0, ""), array_base_pointer);
	BEValue len_match;
	llvm_emit_comparison(c, &len_match, &lhs_len, &rhs_len, BINARYOP_EQ);

	no_match_block = c->current_block;
	llvm_emit_cond_br(c, &len_match, value_cmp, exit);

	llvm_emit_block(c, value_cmp);
	BEValue index_var;
	llvm_value_set_address(&index_var, llvm_emit_alloca_aligned(c, type_usize, "cmp.idx"), type_usize);
	LLVMValueRef one = llvm_const_int(c, type_usize, 1);
	llvm_store_bevalue_raw(c, &index_var, llvm_get_zero(c, type_usize));
	llvm_emit_br(c, loop_begin);

	llvm_emit_block(c, loop_begin);
	BEValue current_index = index_var;
	llvm_value_rvalue(c, &current_index);
	BEValue cmp;
	llvm_emit_comparison(c, &cmp, &current_index, &lhs_len, BINARYOP_LT);
	all_match_block = c->current_block;
	llvm_emit_cond_br(c, &cmp, comparison, exit);

	llvm_emit_block(c, comparison);
	BEValue lhs_to_compare;
	BEValue rhs_to_compare;
	llvm_value_set_address(&lhs_to_compare,
						   llvm_emit_pointer_inbounds_gep_raw(c, llvm_base_type, lhs_value.value, current_index.value),
						   array_base_type);
	llvm_value_set_address(&rhs_to_compare,
						   llvm_emit_pointer_inbounds_gep_raw(c, llvm_base_type, rhs_value.value, current_index.value),
						   array_base_type);
	llvm_emit_comparison(c, &cmp, &lhs_to_compare, &rhs_to_compare, BINARYOP_EQ);
	match_fail_block = c->current_block;
	llvm_store_bevalue_raw(c, &index_var, LLVMBuildAdd(c->builder, current_index.value, one, ""));
	llvm_emit_cond_br(c, &cmp, loop_begin, exit);
	llvm_emit_block(c, exit);

	LLVMValueRef phi = LLVMBuildPhi(c->builder, c->bool_type, "subarray_cmp_phi");

	LLVMValueRef success = LLVMConstInt(c->bool_type, want_match ? 1 : 0, false);
	LLVMValueRef failure = LLVMConstInt(c->bool_type, want_match ? 0 : 1, false);
	LLVMValueRef logic_values[3] = { success, failure, failure };
	LLVMBasicBlockRef blocks[3] = { all_match_block, no_match_block, match_fail_block };
	LLVMAddIncoming(phi, logic_values, blocks, 3);

	llvm_value_set_bool(be_value, phi);


}
static void llvm_emit_float_comp(GenContext *c, BEValue *be_value, BEValue *lhs, BEValue *rhs, BinaryOp binary_op, Type *vector_type)
{
	llvm_value_rvalue(c, lhs);
	llvm_value_rvalue(c, rhs);
	LLVMValueRef lhs_value = lhs->value;
	LLVMValueRef rhs_value = rhs->value;
	LLVMValueRef val;
	switch (binary_op)
	{
		case BINARYOP_EQ:
			// Unordered?
			val = LLVMBuildFCmp(c->builder, LLVMRealOEQ, lhs_value, rhs_value, "eq");
			break;
		case BINARYOP_NE:
			// Unordered?
			val = LLVMBuildFCmp(c->builder, LLVMRealONE, lhs_value, rhs_value, "neq");
			break;
		case BINARYOP_GE:
			val = LLVMBuildFCmp(c->builder, LLVMRealOGE, lhs_value, rhs_value, "ge");
			break;
		case BINARYOP_GT:
			val = LLVMBuildFCmp(c->builder, LLVMRealOGT, lhs_value, rhs_value, "gt");
			break;
		case BINARYOP_LE:
			val = LLVMBuildFCmp(c->builder, LLVMRealOLE, lhs_value, rhs_value, "le");
			break;
		case BINARYOP_LT:
			val = LLVMBuildFCmp(c->builder, LLVMRealOLT, lhs_value, rhs_value, "lt");
			break;
		default:
			UNREACHABLE
	}
	if (vector_type)
	{
		llvm_convert_vector_comparison(c, be_value, val, vector_type);
		return;
	}
	llvm_value_set_bool(be_value, val);
}

void llvm_emit_comparison(GenContext *c, BEValue *be_value, BEValue *lhs, BEValue *rhs, BinaryOp binary_op)
{
	assert(binary_op >= BINARYOP_GT && binary_op <= BINARYOP_EQ);
	llvm_value_rvalue(c, lhs);
	llvm_value_rvalue(c, rhs);
	if (type_is_integer_or_bool_kind(lhs->type))
	{
		llvm_emit_int_comp(c, be_value, lhs->type, rhs->type, lhs->value, rhs->value, binary_op);
		return;
	}
	if (type_is_pointer(lhs->type))
	{
		llvm_emit_ptr_comparison(c, be_value, lhs, rhs, binary_op);
		return;
	}
	if (type_is_float(lhs->type))
	{
		llvm_emit_float_comp(c, be_value, lhs, rhs, binary_op, NULL);
		return;
	}
	if (lhs->type->type_kind == TYPE_SUBARRAY)
	{
		llvm_emit_subarray_comp(c, be_value, lhs, rhs, binary_op);
		return;
	}
	if (lhs->type->type_kind == TYPE_VECTOR)
	{
		Type *type = type_vector_type(lhs->type);
		if (type_is_float(type))
		{
			llvm_emit_float_comp(c, be_value, lhs, rhs, binary_op, lhs->type);
		}
		else
		{
			llvm_emit_int_comp(c, be_value, lhs->type, rhs->type, lhs->value, rhs->value, binary_op);
		}
		return;
	}
	TODO
}
void gencontext_emit_binary(GenContext *c, BEValue *be_value, Expr *expr, BEValue *lhs_loaded, BinaryOp binary_op)
{

	if (binary_op == BINARYOP_AND || binary_op == BINARYOP_OR)
	{
		gencontext_emit_logical_and_or(c, be_value, expr, binary_op);
		return;
	}
	BEValue lhs;
	if (lhs_loaded)
	{
		lhs = *lhs_loaded;
	}
	else
	{
		llvm_emit_expr(c, &lhs, expr->binary_expr.left);
	}
	llvm_value_rvalue(c, &lhs);

	BEValue rhs;
	llvm_emit_expr(c, &rhs, expr->binary_expr.right);
	llvm_value_rvalue(c, &rhs);

	EMIT_LOC(c, expr);
	if (binary_op >= BINARYOP_GT && binary_op <= BINARYOP_EQ)
	{
		llvm_emit_comparison(c, be_value, &lhs, &rhs, binary_op);
		return;
	}

	Type *lhs_type = lhs.type;
	Type *rhs_type = rhs.type;
	Type *vector_type = lhs_type->type_kind == TYPE_VECTOR ? lhs_type->vector.base : NULL;
	bool is_float = type_is_float(lhs_type) || (vector_type && type_is_float(vector_type));
	LLVMValueRef val = NULL;
	LLVMValueRef lhs_value = lhs.value;
	LLVMValueRef rhs_value = rhs.value;
	switch (binary_op)
	{
		case BINARYOP_ERROR:
			UNREACHABLE
		case BINARYOP_MULT:
			if (is_float)
			{
				val = LLVMBuildFMul(c->builder, lhs_value, rhs_value, "fmul");
				break;
			}
			val = llvm_emit_mult_int(c, lhs_type, lhs_value, rhs_value, TOKLOC(expr->span.loc));
			break;
		case BINARYOP_SUB:
			if (lhs_type->type_kind == TYPE_POINTER)
			{
				bool is_constant = LLVMIsConstant(lhs_value) && LLVMIsConstant(rhs_value);
				if (lhs_type == rhs_type)
				{
					LLVMTypeRef int_type = llvm_get_type(c, type_iptrdiff);
					val = LLVMBuildSub(c->builder, LLVMBuildPtrToInt(c->builder, lhs_value, int_type, ""),
					                   LLVMBuildPtrToInt(c->builder, rhs_value, int_type, ""), "");
					val = LLVMBuildExactSDiv(c->builder, val, llvm_const_int(c, type_iptrdiff, type_abi_alignment(lhs_type->pointer)), "");
					break;
				}
				rhs_value = LLVMConstNeg(rhs_value);
				val = llvm_emit_pointer_gep_raw(c, llvm_get_pointee_type(c, lhs_type), lhs_value, rhs_value);
				break;
			}
			if (is_float)
			{
				val = LLVMBuildFSub(c->builder, lhs_value, rhs_value, "fsub");
				break;
			}
			val = llvm_emit_sub_int(c, lhs_type, lhs_value, rhs_value, TOKLOC(expr->span.loc));
			break;
		case BINARYOP_ADD:
			if (lhs_type->type_kind == TYPE_POINTER)
			{
				assert(type_is_integer(rhs_type));
				val = llvm_emit_pointer_gep_raw(c, llvm_get_pointee_type(c, lhs_type), lhs_value, rhs_value);
				break;
			}
			if (is_float)
			{
				val = LLVMBuildFAdd(c->builder, lhs_value, rhs_value, "fadd");
				break;
			}
			val = llvm_emit_add_int(c, lhs_type, lhs_value, rhs_value, TOKLOC(expr->span.loc));
			break;
		case BINARYOP_DIV:
			llvm_emit_trap_zero(c, rhs_type, rhs_value, "% by zero", TOKLOC(expr->span.loc));
			if (is_float)
			{
				val = LLVMBuildFDiv(c->builder, lhs_value, rhs_value, "fdiv");
				break;
			}
			val = type_is_unsigned(lhs_type)
			      ? LLVMBuildUDiv(c->builder, lhs_value, rhs_value, "udiv")
			      : LLVMBuildSDiv(c->builder, lhs_value, rhs_value, "sdiv");
			break;
		case BINARYOP_MOD:
			llvm_emit_trap_zero(c, rhs_type, rhs_value, "% by zero", TOKLOC(expr->span.loc));
			val = type_is_unsigned(lhs_type)
			      ? LLVMBuildURem(c->builder, lhs_value, rhs_value, "umod")
			      : LLVMBuildSRem(c->builder, lhs_value, rhs_value, "smod");
			break;
		case BINARYOP_SHR:
			rhs_value = llvm_fixup_shift_rhs(c, lhs_value, rhs_value, TOKLOC(expr->span.loc));
			llvm_emit_trap_invalid_shift(c, rhs_value, lhs_type, "Shift amount out of range.", TOKLOC(expr->span.loc));
			val = type_is_unsigned(lhs_type)
			      ? LLVMBuildLShr(c->builder, lhs_value, rhs_value, "lshr")
			      : LLVMBuildAShr(c->builder, lhs_value, rhs_value, "ashr");
			val = LLVMBuildFreeze(c->builder, val, "");
			break;
		case BINARYOP_SHL:
			rhs_value = llvm_fixup_shift_rhs(c, lhs_value, rhs_value, TOKLOC(expr->span.loc));
			llvm_emit_trap_invalid_shift(c, rhs_value, lhs_type, "Shift amount out of range.", TOKLOC(expr->span.loc));
			val = LLVMBuildShl(c->builder, lhs_value, rhs_value, "shl");
			val = LLVMBuildFreeze(c->builder, val, "");
			break;
		case BINARYOP_BIT_AND:
			val = LLVMBuildAnd(c->builder, lhs_value, rhs_value, "and");
			break;
		case BINARYOP_BIT_OR:
			val = LLVMBuildOr(c->builder, lhs_value, rhs_value, "or");
			break;
		case BINARYOP_BIT_XOR:
			val = LLVMBuildXor(c->builder, lhs_value, rhs_value, "xor");
			break;
		case BINARYOP_EQ:
		case BINARYOP_NE:
		case BINARYOP_GE:
		case BINARYOP_GT:
		case BINARYOP_LE:
		case BINARYOP_LT:
			UNREACHABLE
		case BINARYOP_AND:
		case BINARYOP_OR:
		case BINARYOP_ASSIGN:
		case BINARYOP_MULT_ASSIGN:
		case BINARYOP_ADD_ASSIGN:
		case BINARYOP_SUB_ASSIGN:
		case BINARYOP_DIV_ASSIGN:
		case BINARYOP_MOD_ASSIGN:
		case BINARYOP_BIT_AND_ASSIGN:
		case BINARYOP_BIT_OR_ASSIGN:
		case BINARYOP_BIT_XOR_ASSIGN:
		case BINARYOP_SHR_ASSIGN:
		case BINARYOP_SHL_ASSIGN:
			UNREACHABLE
	}
	assert(val);
	llvm_value_set(be_value, val, expr->type);
}


static void llvm_emit_post_unary_expr(GenContext *context, BEValue *be_value, Expr *expr)
{

	llvm_emit_post_inc_dec(context,
	                       be_value,
	                       expr->unary_expr.expr,
	                       expr->unary_expr.operator == UNARYOP_INC ? 1 : -1,
	                       false);
}

void llvm_emit_derived_backend_type(GenContext *c, Type *type)
{
	llvm_get_type(c, type);
	LLVMValueRef global_name = LLVMAddGlobal(c->module, llvm_get_type(c, type_char), type->name);
	LLVMSetGlobalConstant(global_name, 1);
	LLVMSetInitializer(global_name, LLVMConstInt(llvm_get_type(c, type_char), 1, false));
	type->backend_typeid = LLVMConstPointerCast(global_name, llvm_get_type(c, type_typeid));
	Decl *origin = NULL;
	Type *original_type = type;
	while (!origin)
	{
		switch (original_type->type_kind)
		{
			case TYPE_FAILABLE:
				original_type = type->failable;
				continue;
			case TYPE_VECTOR:
				original_type = type->vector.base;
				continue;
			case TYPE_ARRAY:
			case TYPE_SUBARRAY:
				original_type = original_type->array.base;
				continue;
			case TYPE_POINTER:
				original_type = original_type->pointer;
				continue;
			case TYPE_ENUM:
			case TYPE_FUNC:
			case TYPE_STRUCT:
			case TYPE_UNION:
			case TYPE_BITSTRUCT:
			case TYPE_ERRTYPE:
			case TYPE_DISTINCT:
				origin = type->decl;
				continue;
			case TYPE_TYPEDEF:
				original_type = original_type->canonical;
				continue;
			case TYPE_STRLIT:
			case TYPE_INFERRED_ARRAY:
			case TYPE_UNTYPED_LIST:
			case TYPE_FAILABLE_ANY:
			case TYPE_TYPEINFO:
				UNREACHABLE
			default:
				goto PRIMITIVE;
		}
	}
	llvm_set_linkage(c, origin, global_name);
	return;

	PRIMITIVE:
	LLVMSetLinkage(global_name, LLVMWeakAnyLinkage);
	LLVMSetVisibility(global_name, LLVMDefaultVisibility);
}

void llvm_emit_typeid(GenContext *c, BEValue *be_value, Type *type)
{
	LLVMValueRef value;
	type = type->canonical;
	if (type_is_builtin(type->type_kind))
	{
		value = llvm_const_int(c, type_usize, type->type_kind);
	}
	else
	{
		if (!type->backend_typeid)
		{
			llvm_emit_derived_backend_type(c, type);
		}
		value = type->backend_typeid;
	}
	llvm_value_set(be_value, value, type_typeid);
}

void llvm_emit_try_assign_try_catch(GenContext *c, bool is_try, BEValue *be_value, BEValue *var_addr, BEValue *catch_addr, Expr *rhs)
{
	assert(!catch_addr || llvm_value_is_addr(catch_addr));
	assert(!var_addr || llvm_value_is_addr(var_addr));

	// 1.  Create after try/catch block
	LLVMBasicBlockRef catch_block = llvm_basic_block_new(c, "catch_landing");
	LLVMBasicBlockRef phi_catch = llvm_basic_block_new(c, "phi_try_catch");

	// 2. Push the error state.
	PUSH_ERROR();

	// 3. If we have a catch *and* we want to store it, set the catch variable
	c->error_var = catch_addr ? catch_addr->value : NULL;

	// 4. After catch we want to end up in the landing, because otherwise we don't know the value for the phi.
	c->catch_block = catch_block;

	// 5. Emit the init part.
	llvm_emit_expr(c, be_value, rhs);

	// 6. If we haven't jumped yet, do it here (on error) to the catch block.
	llvm_value_fold_failable(c, be_value);

	// 7. If we have a variable, then we make the store.
	if (var_addr)
	{
		assert(is_try && "Storing will only happen on try.");
		llvm_store_bevalue(c, var_addr, be_value);
	}

	// 8. Restore the error stack.
	POP_ERROR();

	// 9. Store the success block.
	LLVMBasicBlockRef success_block = c->current_block;

	// 10. Jump to the phi
	llvm_emit_br(c, phi_catch);

	// 11. Emit the catch and jump.
	llvm_emit_block(c, catch_block);
	llvm_emit_br(c, phi_catch);

	// 12. Emit the phi
	llvm_emit_block(c, phi_catch);

	// 13. Use a phi to pick true / false.
	LLVMValueRef phi = LLVMBuildPhi(c->builder, c->bool_type, "val");
	LLVMValueRef from_try = LLVMConstInt(c->bool_type, is_try ? 1 : 0, false);
	LLVMValueRef from_catch = LLVMConstInt(c->bool_type, is_try ? 0 : 1, false);
	LLVMValueRef logic_values[2] = { from_try, from_catch };
	LLVMBasicBlockRef blocks[2] = { success_block, catch_block };
	LLVMAddIncoming(phi, logic_values, blocks, 2);

	llvm_value_set_bool(be_value, phi);

}


static inline void llvm_emit_catch_expr(GenContext *c, BEValue *value, Expr *expr)
{
	Expr *inner = expr->inner_expr;

	if (inner->expr_kind == EXPR_IDENTIFIER)
	{
		Decl *decl = inner->identifier_expr.decl;
		assert(IS_FAILABLE(decl));
		llvm_value_set_address(value, decl_failable_ref(decl), type_anyerr);
		return;
	}

	if (inner->expr_kind == EXPR_FAILABLE)
	{
		llvm_emit_expr(c, value, inner->inner_expr);
		return;
	}

	LLVMBasicBlockRef end_block = llvm_basic_block_new(c, "noerr_block");

	// Store catch/error var
	PUSH_ERROR();

	LLVMValueRef error_var = llvm_emit_alloca_aligned(c, type_anyerr, "error_var");
	llvm_value_set_address(value, error_var, type_anyerr);
	llvm_store_bevalue_raw(c, value, llvm_get_zero(c, type_anyerr));
	c->error_var = error_var;
	c->catch_block = end_block;

	BEValue expr_value;
	llvm_emit_expr(c, &expr_value, inner);
	llvm_value_fold_failable(c, &expr_value);

	// Restore.
	POP_ERROR();

	// Emit success and jump to end block.
	llvm_emit_br(c, end_block);

	llvm_emit_block(c, end_block);

}
void llvm_emit_try_expr(GenContext *c, BEValue *value, Expr *expr)
{

	LLVMBasicBlockRef error_block = llvm_basic_block_new(c, "error_block");
	LLVMBasicBlockRef no_err_block = llvm_basic_block_new(c, "noerr_block");
	LLVMBasicBlockRef phi_block = llvm_basic_block_new(c, "phi_trycatch_block");

	// Store catch/error var
	PUSH_ERROR();

	// Set the catch/error var
	c->error_var = NULL;
	c->catch_block = error_block;

	llvm_emit_expr(c, value, expr->inner_expr);
	llvm_value_fold_failable(c, value);

	// Restore.
	POP_ERROR();

	// Emit success and jump to phi.
	llvm_emit_br(c, no_err_block);
	llvm_emit_block(c, no_err_block);
	llvm_emit_br(c, phi_block);

	// Emit error and jump to phi
	llvm_emit_block(c, error_block);
	llvm_emit_br(c, phi_block);

	llvm_emit_block(c, phi_block);

	LLVMValueRef	phi = LLVMBuildPhi(c->builder, llvm_get_type(c, expr->type), "val");
	LLVMValueRef	lhs = llvm_const_int(c, type_bool, 1);
	LLVMValueRef	rhs = llvm_const_int(c, type_bool, 0);

	LLVMValueRef logic_values[2] = { lhs, rhs };
	LLVMBasicBlockRef blocks[2] = { no_err_block, error_block };
	LLVMAddIncoming(phi, logic_values, blocks, 2);

	llvm_value_set(value, phi, expr->type);
}

static inline void gencontext_emit_else_jump_expr(GenContext *c, BEValue *be_value, Expr *expr)
{
	LLVMBasicBlockRef else_block = llvm_basic_block_new(c, "else_block");
	LLVMBasicBlockRef no_err_block = llvm_basic_block_new(c, "noerr_block");

	// Store catch/error var
	PUSH_ERROR();

	// Set the catch/error var
	c->error_var = NULL;
	c->catch_block = else_block;


	llvm_emit_expr(c, be_value, expr->or_error_expr.expr);
	llvm_value_rvalue(c, be_value);

	// Restore.
	POP_ERROR();

	// Emit success and to end.
	llvm_emit_br(c, no_err_block);

	// Emit else
	llvm_emit_block(c, else_block);
	llvm_emit_stmt(c, expr->or_error_expr.or_error_stmt);
	llvm_emit_br(c, no_err_block);

	llvm_emit_block(c, no_err_block);
}


static void gencontext_emit_or_error(GenContext *c, BEValue *be_value, Expr *expr)
{
	if (expr->or_error_expr.is_jump)
	{
		gencontext_emit_else_jump_expr(c, be_value, expr);
		return;
	}
	LLVMBasicBlockRef else_block = llvm_basic_block_new(c, "else_block");
	LLVMBasicBlockRef phi_block = llvm_basic_block_new(c, "phi_block");

	// Store catch/error var
	PUSH_ERROR();

	// Set the catch/error var
	c->error_var = NULL;
	c->catch_block = else_block;

	BEValue normal_value;
	llvm_emit_expr(c, &normal_value, expr->or_error_expr.expr);
	llvm_value_rvalue(c, &normal_value);

	// Restore.
	POP_ERROR();

	// Emit success and jump to phi.
	LLVMBasicBlockRef success_end_block = llvm_get_current_block_if_in_use(c);

	if (success_end_block) llvm_emit_br(c, phi_block);

	// Emit else
	llvm_emit_block(c, else_block);

	BEValue else_value;
	llvm_emit_expr(c, &else_value, expr->or_error_expr.or_error_expr);
	llvm_value_rvalue(c, &else_value);

	LLVMBasicBlockRef else_block_exit = llvm_get_current_block_if_in_use(c);

	if (else_block_exit) llvm_emit_br(c, phi_block);

	llvm_emit_block(c, phi_block);

	if (!else_block_exit)
	{
		*be_value = normal_value;
		return;
	}
	if (!success_end_block)
	{
		*be_value = else_value;
		return;
	}

	LLVMValueRef phi = LLVMBuildPhi(c->builder, llvm_get_type(c, expr->type), "val");

	LLVMValueRef logic_values[2] = { normal_value.value, else_value.value };
	LLVMBasicBlockRef blocks[2] = { success_end_block, else_block_exit };
	LLVMAddIncoming(phi, logic_values, blocks, 2);

	llvm_value_set(be_value, phi, expr->type);

}

/**
 * This is the foo? instruction.
 */
static inline void llvm_emit_rethrow_expr(GenContext *c, BEValue *be_value, Expr *expr)
{
	LLVMBasicBlockRef guard_block = llvm_basic_block_new(c, "guard_block");
	LLVMBasicBlockRef no_err_block = llvm_basic_block_new(c, "noerr_block");

	// Store catch/error var
	PUSH_ERROR();

	// Set the catch/error var
	LLVMValueRef error_var = llvm_emit_alloca_aligned(c, type_anyerr, "error_var");

	c->error_var = error_var;
	c->catch_block = guard_block;

	llvm_emit_expr(c, be_value, expr->rethrow_expr.inner);
	// Fold the failable.
	llvm_value_fold_failable(c, be_value);

	// Restore.
	POP_ERROR();

	// Emit success and to end.
	llvm_emit_br(c, no_err_block);

	// Emit else
	llvm_emit_block(c, guard_block);

	// Ensure we are on a branch that is non empty.
	if (llvm_emit_check_block_branch(c))
	{
		llvm_emit_defer(c, expr->rethrow_expr.defer, 0);
		BEValue value;
		llvm_value_set_address(&value, error_var, type_anyerr);
		llvm_emit_return_abi(c, NULL, &value);
		c->current_block = NULL;
		c->current_block_is_target = NULL;
	}

	llvm_emit_block(c, no_err_block);

}

static inline void llvm_emit_typeofany(GenContext *c, BEValue *be_value, Expr *expr)
{
	llvm_emit_expr(c, be_value, expr->inner_expr);
	llvm_value_fold_failable(c, be_value);
	if (llvm_value_is_addr(be_value))
	{
		AlignSize alignment = 0;
		LLVMValueRef pointer_addr = llvm_emit_struct_gep_raw(c,
															 be_value->value,
															 llvm_get_type(c, type_any),
															 1,
															 be_value->alignment,
															 &alignment);
		llvm_value_set_address_align(be_value, pointer_addr, type_typeid, alignment);
	}
	else
	{
		llvm_value_set(be_value, LLVMBuildExtractValue(c->builder, be_value->value, 1, ""), type_typeid);
	}
}

/**
 * This is the foo? instruction.
 */
static inline void llvm_emit_force_unwrap_expr(GenContext *c, BEValue *be_value, Expr *expr)
{
	LLVMBasicBlockRef panic_block = llvm_basic_block_new(c, "panic_block");
	LLVMBasicBlockRef no_err_block = llvm_basic_block_new(c, "noerr_block");

	// Store catch/error var
	PUSH_ERROR();

	// Set the catch/error var
	LLVMValueRef error_var = llvm_emit_alloca_aligned(c, type_anyerr, "error_var");

	c->error_var = error_var;
	c->catch_block = panic_block;

	llvm_emit_expr(c, be_value, expr->inner_expr);
	llvm_value_rvalue(c, be_value);

	// Restore.
	POP_ERROR();

	// Emit success and to end.
	llvm_emit_br(c, no_err_block);

	POP_ERROR();

	// Emit panic
	llvm_emit_block(c, panic_block);

	// Ensure we are on a branch that is non-empty.
	if (llvm_emit_check_block_branch(c))
	{
		// TODO, we should add info about the error.
		SourceLocation *loc = TOKLOC(expr->span.loc);
		llvm_emit_debug_output(c, "Runtime error force unwrap!", loc->file->name, c->cur_func_decl->external_name, loc->line);
		llvm_emit_call_intrinsic(c, intrinsic_id.trap, NULL, 0, NULL, 0);
		LLVMBuildUnreachable(c->builder);
		c->current_block = NULL;
		c->current_block_is_target = NULL;
	}

	llvm_emit_block(c, no_err_block);

}

static bool expr_is_vector_index(Expr *expr)
{
	return expr->expr_kind == EXPR_SUBSCRIPT
		&& type_lowering(expr->subscript_expr.expr->type)->type_kind == TYPE_VECTOR;
}

static void llvm_emit_vector_assign_expr(GenContext *c, BEValue *be_value, Expr *expr)
{
	Expr *left = expr->binary_expr.left;
	BinaryOp binary_op = expr->binary_expr.operator;
	BEValue addr;
	BEValue index;

	// Emit the variable
	llvm_emit_expr(c, &addr, left->subscript_expr.expr);
	llvm_value_addr(c, &addr);
	LLVMValueRef vector_value = llvm_value_rvalue_store(c, &addr);

	// Emit the index
	llvm_emit_expr(c, &index, left->subscript_expr.index);
	LLVMValueRef index_val = llvm_value_rvalue_store(c, &index);

	if (binary_op > BINARYOP_ASSIGN)
	{
		BinaryOp base_op = binaryop_assign_base_op(binary_op);
		assert(base_op != BINARYOP_ERROR);
		BEValue lhs;
		llvm_value_set(&lhs, LLVMBuildExtractElement(c->builder, vector_value, index_val, "elem"), expr->type);
		gencontext_emit_binary(c, be_value, expr, &lhs, base_op);
	}
	else
	{
		llvm_emit_expr(c, be_value, expr->binary_expr.right);
	}

	LLVMValueRef new_value = LLVMBuildInsertElement(c->builder, vector_value, llvm_value_rvalue_store(c, be_value), index_val, "elemset");
	llvm_store_bevalue_raw(c, &addr, new_value);
}

static void llvm_emit_binary_expr(GenContext *c, BEValue *be_value, Expr *expr)
{
	BinaryOp binary_op = expr->binary_expr.operator;
	if (binary_op >= BINARYOP_ASSIGN && expr_is_vector_index(expr->binary_expr.left))
	{
		llvm_emit_vector_assign_expr(c, be_value, expr);
		return;
	}
	if (binary_op > BINARYOP_ASSIGN)
	{
		BinaryOp base_op = binaryop_assign_base_op(binary_op);
		assert(base_op != BINARYOP_ERROR);
		BEValue addr;
		llvm_emit_expr(c, &addr, expr->binary_expr.left);
		llvm_value_addr(c, &addr);
		gencontext_emit_binary(c, be_value, expr, &addr, base_op);
		llvm_store_bevalue(c, &addr, be_value);
		return;
	}
	if (binary_op == BINARYOP_ASSIGN)
	{
		Expr *left = expr->binary_expr.left;
		llvm_emit_expr(c, be_value, left);
		assert(llvm_value_is_addr(be_value));
		LLVMValueRef failable_ref = NULL;
		if (expr->binary_expr.left->expr_kind == EXPR_IDENTIFIER)
		{
			failable_ref = decl_failable_ref(left->identifier_expr.decl);
			be_value->kind = BE_ADDRESS;
		}
		*be_value = llvm_emit_assign_expr(c, be_value, expr->binary_expr.right, failable_ref);
		return;
	}

	gencontext_emit_binary(c, be_value, expr, NULL, binary_op);
}

void gencontext_emit_elvis_expr(GenContext *c, BEValue *value, Expr *expr)
{
	LLVMBasicBlockRef current_block = c->current_block;
	LLVMBasicBlockRef phi_block = llvm_basic_block_new(c, "cond.phi");
	LLVMBasicBlockRef rhs_block = llvm_basic_block_new(c, "cond.rhs");

	// Generate condition and conditional branch
	llvm_emit_expr(c, value, expr->ternary_expr.cond);
	// Get the Rvalue version (in case we have an address)
	llvm_value_rvalue(c, value);

	LLVMValueRef lhs = value->value;
	Type *cond_type = expr->ternary_expr.cond->type;

	// If the cond is not a boolean, we need to do the cast.
	if (value->kind != BE_BOOLEAN)
	{
		CastKind cast = cast_to_bool_kind(cond_type);
		llvm_emit_cast(c, cast, value, type_bool, cond_type);
		assert(value->kind == BE_BOOLEAN);
	}

	llvm_emit_cond_br(c, value, phi_block, rhs_block);

	llvm_emit_block(c, rhs_block);
	// Emit right side:
	llvm_emit_expr(c, value, expr->ternary_expr.else_expr);
	// Lower to value.
	llvm_value_rvalue(c, value);

	LLVMBasicBlockRef end_block = c->current_block;
	llvm_emit_br(c, phi_block);

	// Generate phi
	llvm_emit_block(c, phi_block);

	// If both sides are bool we produce a bool as well.
	LLVMTypeRef phi_type = expr->type->canonical->type_kind == TYPE_BOOL ? c->bool_type : llvm_get_type(c, expr->type);
	LLVMValueRef phi = LLVMBuildPhi(c->builder, phi_type, "val");

	LLVMValueRef logic_values[2] = { lhs, value->value };
	LLVMBasicBlockRef blocks[2] = { current_block, end_block };
	LLVMAddIncoming(phi, logic_values, blocks, 2);

	// The rest of value should now be set to the right value.
	value->value = phi;
}

void gencontext_emit_ternary_expr(GenContext *c, BEValue *value, Expr *expr)
{
	if (expr->ternary_expr.then_expr == NULL) return gencontext_emit_elvis_expr(c, value, expr);

	// Set up basic blocks, following Cone
	LLVMBasicBlockRef phi_block = llvm_basic_block_new(c, "cond.phi");
	LLVMBasicBlockRef lhs_block = llvm_basic_block_new(c, "cond.lhs");
	LLVMBasicBlockRef rhs_block = llvm_basic_block_new(c, "cond.rhs");

	// Generate condition and conditional branch

	llvm_emit_expr(c, value, expr->ternary_expr.cond);
	llvm_value_rvalue(c, value);

	assert(value->kind == BE_BOOLEAN);

	llvm_emit_cond_br(c, value, lhs_block, rhs_block);

	llvm_emit_block(c, lhs_block);
	BEValue lhs;
	llvm_emit_expr(c, &lhs, expr->ternary_expr.then_expr);
	LLVMValueRef lhs_value = llvm_value_rvalue_store(c, &lhs);

	LLVMBasicBlockRef lhs_exit = llvm_get_current_block_if_in_use(c);
	if (lhs_exit) llvm_emit_br(c, phi_block);

	llvm_emit_block(c, rhs_block);
	BEValue rhs;
	llvm_emit_expr(c, &rhs, expr->ternary_expr.else_expr);
	LLVMValueRef rhs_value = llvm_value_rvalue_store(c, &rhs);

	LLVMBasicBlockRef rhs_exit = llvm_get_current_block_if_in_use(c);
	if (rhs_exit) llvm_emit_br(c, phi_block);

	// Generate phi
	llvm_emit_block(c, phi_block);
	if (!rhs_exit)
	{
		llvm_value_set(value, lhs_value, lhs.type);
		return;
	}
	if (!lhs_exit)
	{
		llvm_value_set(value, rhs_value, rhs.type);
		return;
	}
	LLVMValueRef phi = LLVMBuildPhi(c->builder, llvm_get_type(c, expr->type), "val");
	LLVMValueRef logic_values[2] = { lhs_value, rhs_value };
	LLVMBasicBlockRef blocks[2] = { lhs_exit, rhs_exit };
	LLVMAddIncoming(phi, logic_values, blocks, 2);

	llvm_value_set(value, phi, expr->type);
}
static LLVMValueRef llvm_emit_real(LLVMTypeRef type, Float f)
{
	if (isnan(f.f))
	{
		return LLVMConstRealOfString(type, "nan");
	}
	if (isinf(f.f))
	{
		return LLVMConstRealOfString(type, f.f < 0 ? "-inf" : "inf");
	}
	scratch_buffer_clear();
#if LONG_DOUBLE
	global_context.scratch_buffer_len = sprintf(global_context.scratch_buffer, "%La", f.f);
#else
	global_context.scratch_buffer_len = (uint32_t)sprintf(global_context.scratch_buffer, "%a", f.f);
#endif
	return LLVMConstRealOfStringAndSize(type, global_context.scratch_buffer, global_context.scratch_buffer_len);
}

static inline void llvm_emit_const_initializer_list_expr(GenContext *c, BEValue *value, Expr *expr)
{
	if (!c->builder || type_is_vector(expr->type) || type_flatten_distinct(expr->type)->type_kind == TYPE_BITSTRUCT)
	{
		llvm_value_set(value, llvm_emit_const_initializer(c, expr->const_expr.list), expr->type);
		return;
	}
	llvm_value_set_address(value, llvm_emit_alloca_aligned(c, expr->type, "literal"), expr->type);
	llvm_emit_initialize_reference_const(c, value, expr);
}

static void llvm_emit_const_expr(GenContext *c, BEValue *be_value, Expr *expr)
{
	Type *type = type_reduced_from_expr(expr)->canonical;
	switch (expr->const_expr.const_kind)
	{
		case CONST_BYTES:
			assert(type->array.base == type_char);
			{
				LLVMValueRef global_name = LLVMAddGlobal(c->module, LLVMArrayType(llvm_get_type(c, type_char), expr->const_expr.bytes.len), ".bytes");
				llvm_set_private_linkage(global_name);
				LLVMSetGlobalConstant(global_name, 1);

				LLVMSetInitializer(global_name, LLVMConstStringInContext(c->context,
																		 expr->const_expr.bytes.ptr,
																		 expr->const_expr.bytes.len,
																		 1));
				llvm_set_alignment(global_name, 1);
				global_name = LLVMConstBitCast(global_name, LLVMPointerType(llvm_get_type(c, type_char), 0));
				llvm_value_set_address(be_value, global_name, type);
				return;
			}
		case CONST_INTEGER:
		{
			LLVMValueRef value;
			Int128 i = expr->const_expr.ixx.i;
			switch (expr->const_expr.ixx.type)
			{
				case TYPE_I128:
				case TYPE_U128:
				{
					uint64_t words[2] = { i.low, i.high };
					value = LLVMConstIntOfArbitraryPrecision(llvm_get_type(c, type), 2, words);
					break;
				}
				default:
					value = llvm_const_int(c, type, i.low);
					break;
			}
			llvm_value_set(be_value, value, type);
			return;
		}
		case CONST_LIST:
			llvm_emit_const_initializer_list_expr(c, be_value, expr);
			return;
		case CONST_FLOAT:
			llvm_value_set(be_value, llvm_emit_real(llvm_get_type(c, type), expr->const_expr.fxx), type);
			return;
		case CONST_POINTER:
			llvm_value_set(be_value, LLVMConstNull(llvm_get_type(c, type)), type);
			return;
		case CONST_BOOL:
			llvm_value_set_bool(be_value, LLVMConstInt(c->bool_type, expr->const_expr.b ? 1 : 0, 0));
			return;
		case CONST_STRING:
		{
			LLVMValueRef global_name = LLVMAddGlobal(c->module, LLVMArrayType(llvm_get_type(c, type_char), expr->const_expr.string.len + 1), ".str");
			llvm_set_private_linkage(global_name);
			LLVMSetGlobalConstant(global_name, 1);
			LLVMSetInitializer(global_name, LLVMConstStringInContext(c->context,
			                                                         expr->const_expr.string.chars,
			                                                         expr->const_expr.string.len,
			                                                         0));
			llvm_set_alignment(global_name, 1);
			global_name = LLVMConstBitCast(global_name, LLVMPointerType(llvm_get_type(c, type_char), 0));
			llvm_value_set(be_value, global_name, type);
			return;
		}
		case CONST_TYPEID:
			llvm_emit_typeid(c, be_value, expr->const_expr.typeid);
			return;
		case CONST_ERR:
		{
			Decl *decl = expr->const_expr.err_val;

			LLVMValueRef value;
			if (decl)
			{
				value = LLVMBuildPtrToInt(c->builder, decl->backend_ref, llvm_get_type(c, type_anyerr), "");
			}
			else
			{
				value = llvm_get_zero(c, type_anyerr);
			}
			llvm_value_set(be_value, value, type_anyerr);
			return;
		}
		case CONST_ENUM:
		{
			Decl *decl = expr->const_expr.enum_val;
			return llvm_emit_const_expr(c, be_value, expr->const_expr.err_val->enum_constant.expr);
			assert(decl->decl_kind == DECL_ERRVALUE);
			llvm_value_set(be_value,
						   LLVMBuildPtrToInt(c->builder, decl->backend_ref, llvm_get_type(c, type_anyerr), ""),
						   type_anyerr);
			return;
		}
		default:
			UNREACHABLE
	}
}

static void llvm_expand_type_to_args(GenContext *context, Type *param_type, LLVMValueRef expand_ptr, LLVMValueRef **values, AlignSize alignment);


static void llvm_expand_array_to_args(GenContext *c, Type *param_type, LLVMValueRef expand_ptr, LLVMValueRef **values, AlignSize alignment)
{
	LLVMTypeRef array_type = llvm_get_type(c, param_type);
	for (ByteSize i = 0; i < param_type->array.len; i++)
	{
		AlignSize load_align;
		LLVMValueRef element_ptr = llvm_emit_array_gep_raw(c, expand_ptr, array_type, (unsigned)i, alignment, &load_align);
		llvm_expand_type_to_args(c, param_type->array.base, element_ptr, values, load_align);
	}
}

static void llvm_expand_struct_to_args(GenContext *context, Type *param_type, LLVMValueRef expand_ptr, LLVMValueRef **values, AlignSize alignment)
{
	Decl **members = param_type->decl->strukt.members;
	VECEACH(members, i)
	{
		Type *member_type = members[i]->type;
		if (type_is_empty_field(member_type, true)) continue;
		AlignSize load_align;
		LLVMValueRef member_ptr = llvm_emit_struct_gep_raw(context,
		                                                   expand_ptr,
		                                                   llvm_get_type(context, param_type),
		                                                   i,
		                                                   alignment,
		                                                   &load_align);
		llvm_expand_type_to_args(context, member_type, member_ptr, values, load_align);
	}
}

static void llvm_expand_type_to_args(GenContext *context, Type *param_type, LLVMValueRef expand_ptr, LLVMValueRef **values, AlignSize alignment)
{
	REDO:
	switch (type_lowering(param_type)->type_kind)
	{
		case TYPE_VOID:
		case TYPE_TYPEID:
		case TYPE_FUNC:
		case TYPE_DISTINCT:
		case TYPE_STRLIT:
		case TYPE_ENUM:
		case TYPE_ERRTYPE:
		case TYPE_ANYERR:
		case TYPE_BITSTRUCT:
		case TYPE_FAILABLE:
		case CT_TYPES:
		case TYPE_FAILABLE_ANY:
			UNREACHABLE
			break;
		case TYPE_BOOL:
		case ALL_INTS:
		case ALL_FLOATS:
		case TYPE_POINTER:
			vec_add(*values, llvm_emit_load_aligned(context, llvm_get_type(context, param_type), expand_ptr, alignment, "loadexpanded"));
			return;
		case TYPE_TYPEDEF:
			param_type = param_type->canonical;
			goto REDO;
		case TYPE_STRUCT:
			llvm_expand_struct_to_args(context, param_type, expand_ptr, values, alignment);
			break;
		case TYPE_ARRAY:
			llvm_expand_array_to_args(context, param_type, expand_ptr, values, alignment);
			break;
		case TYPE_UNION:
		case TYPE_SUBARRAY:
		case TYPE_VECTOR:
		case TYPE_ANY:
			TODO
			break;
	}
}

void llvm_emit_struct_member_ref(GenContext *c, BEValue *struct_ref, BEValue *member_ref, unsigned member_id)
{
	assert(llvm_value_is_addr(struct_ref));
	llvm_value_fold_failable(c, struct_ref);
	assert(struct_ref->type->type_kind == TYPE_STRUCT);
	AlignSize align;
	LLVMValueRef ptr = llvm_emit_struct_gep_raw(c, struct_ref->value, llvm_get_type(c, struct_ref->type), member_id, struct_ref->alignment, &align);
	llvm_value_set_address_align(member_ref, ptr, struct_ref->type->decl->strukt.members[member_id]->type, align);
}

LLVMValueRef llvm_emit_struct_gep_raw(GenContext *context, LLVMValueRef ptr, LLVMTypeRef struct_type, unsigned index,
                                      unsigned struct_alignment, AlignSize *alignment)
{
	*alignment = type_min_alignment((AlignSize)LLVMOffsetOfElement(context->target_data, struct_type, index), struct_alignment);
	if (LLVMIsConstant(ptr))
	{
		LLVMValueRef idx[2] = { llvm_get_zero(context, type_int), llvm_const_int(context, type_int, index) };
		return LLVMConstInBoundsGEP2(struct_type, ptr, idx, 2);
	}
	return LLVMBuildStructGEP2(context->builder, struct_type, ptr, index, "");
}


LLVMValueRef llvm_emit_array_gep_raw_index(GenContext *c, LLVMValueRef ptr, LLVMTypeRef array_type, LLVMValueRef index, AlignSize array_alignment, AlignSize *alignment)
{
	*alignment = type_min_alignment(llvm_store_size(c, LLVMGetElementType(array_type)), array_alignment);
	LLVMValueRef idx[2] = { LLVMConstNull(LLVMTypeOf(index)), index };
	if (LLVMIsConstant(ptr))
	{
		return LLVMConstInBoundsGEP2(array_type, ptr, idx, 2);
	}
	return LLVMBuildInBoundsGEP2(c->builder, array_type, ptr, idx, 2, "");
}

LLVMValueRef llvm_emit_array_gep_raw(GenContext *c, LLVMValueRef ptr, LLVMTypeRef array_type, unsigned index, AlignSize array_alignment, AlignSize *alignment)
{
	return llvm_emit_array_gep_raw_index(c, ptr, array_type, llvm_const_int(c, type_usize, index), array_alignment, alignment);
}

LLVMValueRef llvm_emit_array_load(GenContext *c, LLVMValueRef ptr, LLVMTypeRef array_type, unsigned index, AlignSize array_alignment)
{
	AlignSize alignment;
	LLVMValueRef element_ptr = llvm_emit_array_gep_raw_index(c, ptr, array_type, llvm_const_int(c, type_usize, index), array_alignment, &alignment);
	return llvm_emit_load_aligned(c, LLVMGetElementType(array_type), element_ptr, alignment, "");
}

LLVMValueRef llvm_emit_pointer_gep_raw(GenContext *c, LLVMTypeRef pointee_type, LLVMValueRef ptr, LLVMValueRef offset)
{
	if (LLVMIsConstant(ptr) && LLVMIsConstant(offset))
	{
		return LLVMConstGEP2(pointee_type, ptr, &offset, 1);
	}
	return LLVMBuildGEP2(c->builder, pointee_type, ptr, &offset, 1, "ptroffset");
}

LLVMValueRef llvm_emit_pointer_inbounds_gep_raw(GenContext *c, LLVMTypeRef pointee_type, LLVMValueRef ptr, LLVMValueRef offset)
{
	if (LLVMIsConstant(ptr) && LLVMIsConstant(offset))
	{
		return LLVMConstInBoundsGEP2(pointee_type, ptr, &offset, 1);
	}
	return LLVMBuildInBoundsGEP2(c->builder, pointee_type, ptr, &offset, 1, "ptroffset");
}

void llvm_emit_subarray_len(GenContext *c, BEValue *subarray, BEValue *len)
{
	llvm_value_addr(c, subarray);
	AlignSize alignment = 0;
	LLVMValueRef len_addr = llvm_emit_struct_gep_raw(c,
	                                                 subarray->value,
	                                                 llvm_get_type(c, subarray->type),
	                                                 1,
	                                                 subarray->alignment,
	                                                 &alignment);
	llvm_value_set_address_align(len, len_addr, type_usize, alignment);
}

void llvm_emit_subarray_pointer(GenContext *c, BEValue *subarray, BEValue *pointer)
{
	llvm_value_addr(c, subarray);
	AlignSize alignment = 0;
	LLVMValueRef pointer_addr = llvm_emit_struct_gep_raw(c,
	                                                     subarray->value,
	                                                     llvm_get_type(c, subarray->type),
	                                                     0,
	                                                     subarray->alignment,
	                                                     &alignment);
	llvm_value_set_address_align(pointer, pointer_addr, type_get_ptr(subarray->type->array.base), alignment);
}

static void llvm_emit_any_pointer(GenContext *c, BEValue *any, BEValue *pointer)
{
	llvm_value_addr(c, any);
	AlignSize alignment = 0;
	LLVMValueRef pointer_addr = llvm_emit_struct_gep_raw(c,
														 any->value,
														 llvm_get_type(c, type_voidptr),
														 0,
														 any->alignment,
														 &alignment);
	llvm_value_set_address_align(pointer, pointer_addr, type_voidptr, alignment);
}

void llvm_value_struct_gep(GenContext *c, BEValue *element, BEValue *struct_pointer, unsigned index)
{
	llvm_value_fold_failable(c, struct_pointer);
	MemberIndex actual_index = -1;
	Decl *member;
	for (MemberIndex i = 0; i <= index; i++)
	{
		member = struct_pointer->type->decl->strukt.members[i];
		if (member->padding)
		{
			actual_index++;
		}
		actual_index++;
	}
	AlignSize alignment;
	LLVMValueRef ref = llvm_emit_struct_gep_raw(c,
	                                            struct_pointer->value,
	                                            llvm_get_type(c, struct_pointer->type),
	                                            (unsigned)actual_index,
	                                            struct_pointer->alignment,
	                                            &alignment);
	llvm_value_set_address(element, ref, member->type);
	element->alignment = alignment;
}

static void llvm_emit_intrinsic_expr(GenContext *c, unsigned intrinsic, BEValue *be_value, Expr *expr)
{
	unsigned arguments = vec_size(expr->call_expr.arguments);
	assert(arguments < 5 && "Only has room for 4");
	LLVMValueRef arg_results[4];
	for (unsigned i = 0; i < arguments; i++)
	{
		llvm_emit_expr(c, be_value, expr->call_expr.arguments[0]);
		llvm_value_rvalue(c, be_value);
		arg_results[i] = be_value->value;
	}
	LLVMTypeRef call_type = llvm_get_type(c, expr->type);
	LLVMValueRef result = llvm_emit_call_intrinsic(c, intrinsic, &call_type, 1, arg_results, arguments);
	llvm_value_set(be_value, result, expr->type);
}


void llvm_emit_parameter(GenContext *c, LLVMValueRef **args, ABIArgInfo *info, BEValue *be_value, Type *type)
{
	type = type_lowering(type);
	assert(be_value->type->canonical == type);
	switch (info->kind)
	{
		case ABI_ARG_IGNORE:
			// Skip.
			return;
		case ABI_ARG_INDIRECT:
		{
			// If we want we could optimize for structs by doing it by reference here.
			assert(info->indirect.alignment == type_abi_alignment(type) || info->attributes.realign);
			LLVMValueRef indirect = llvm_emit_alloca(c,
			                                         llvm_get_type(c, type),
			                                         info->indirect.alignment,
			                                         "indirectarg");
			llvm_store_bevalue_aligned(c, indirect, be_value, info->indirect.alignment);
			vec_add(*args, indirect);
			return;
		}
		case ABI_ARG_DIRECT_COERCE:
		{
			LLVMTypeRef coerce_type = llvm_get_coerce_type(c, info);
			if (!coerce_type || coerce_type == llvm_get_type(c, type))
			{
				vec_add(*args, llvm_value_rvalue_store(c, be_value));
				return;
			}
			if (!abi_info_should_flatten(info))
			{
				vec_add(*args, llvm_emit_coerce(c, coerce_type, be_value, type));
				return;
			}
			AlignSize target_alignment = llvm_abi_alignment(c, coerce_type);

			AlignSize alignment;
			LLVMValueRef cast = llvm_emit_coerce_alignment(c, be_value, coerce_type, target_alignment, &alignment);
			LLVMTypeRef element = llvm_abi_type(c, info->direct_coerce.type);
			for (unsigned idx = 0; idx < info->direct_coerce.elements; idx++)
			{
				AlignSize load_align;
				LLVMValueRef element_ptr = llvm_emit_struct_gep_raw(c, cast, coerce_type, idx, alignment, &load_align);
				vec_add(*args, llvm_emit_load_aligned(c, element, element_ptr, load_align, ""));
			}
			return;
		}
		case ABI_ARG_DIRECT_PAIR:
		{
			llvm_value_addr(c, be_value);
			REMINDER("Handle invalid alignment");
			// Here we do the following transform:
			// struct -> { lo, hi } -> lo, hi
			LLVMTypeRef lo = llvm_abi_type(c, info->direct_pair.lo);
			LLVMTypeRef hi = llvm_abi_type(c, info->direct_pair.hi);
			LLVMTypeRef struct_type = llvm_get_coerce_type(c, info);

			AlignSize struct_align;
			LLVMValueRef cast = llvm_emit_coerce_alignment(c, be_value, struct_type, llvm_abi_alignment(c, struct_type), &struct_align);
			// Get the lo value.

			AlignSize alignment;
			LLVMValueRef lo_ptr = llvm_emit_struct_gep_raw(c, cast, struct_type, 0, struct_align, &alignment);
			vec_add(*args, llvm_emit_load_aligned(c, lo, lo_ptr, alignment, "lo"));
			// Get the hi value.
			LLVMValueRef hi_ptr = llvm_emit_struct_gep_raw(c, cast, struct_type, 1, struct_align, &alignment);
			vec_add(*args, llvm_emit_load_aligned(c, hi, hi_ptr, alignment, "hi"));
			return;
		}
		case ABI_ARG_EXPAND_COERCE:
		{
			// Move this to an address (if needed)
			llvm_value_addr(c, be_value);
			LLVMTypeRef coerce_type = llvm_get_coerce_type(c, info);
			AlignSize alignment;
			LLVMValueRef temp = llvm_emit_coerce_alignment(c, be_value, coerce_type, llvm_abi_alignment(c, coerce_type), &alignment);

			AlignSize align;
			LLVMValueRef gep_first = llvm_emit_struct_gep_raw(c, temp, coerce_type, info->coerce_expand.lo_index, alignment, &align);
			vec_add(*args, llvm_emit_load_aligned(c, llvm_abi_type(c, info->coerce_expand.lo), gep_first, align, ""));
			if (info->coerce_expand.hi)
			{
				LLVMValueRef gep_second = llvm_emit_struct_gep_raw(c, temp, coerce_type, info->coerce_expand.hi_index, alignment, &align);
				vec_add(*args, llvm_emit_load_aligned(c, llvm_abi_type(c, info->coerce_expand.hi), gep_second, align, ""));
			}
			return;
		}
		case ABI_ARG_EXPAND:
		{
			// Move this to an address (if needed)
			llvm_value_addr(c, be_value);
			llvm_expand_type_to_args(c, type, be_value->value, args, be_value->alignment);
			// Expand the padding here.
			if (info->expand.padding_type)
			{
				vec_add(*args, LLVMGetUndef(llvm_get_type(c, info->expand.padding_type)));
			}
			return;
		}
	}

}
static void llvm_emit_unpacked_variadic_arg(GenContext *c, Expr *expr, BEValue *subarray)
{
	BEValue value;
	llvm_emit_expr(c, &value, expr);
	BEValue len_addr;
	BEValue pointer_addr;
	llvm_emit_subarray_len(c, subarray, &len_addr);
	llvm_emit_subarray_pointer(c, subarray, &pointer_addr);
	Type *type = expr->type->canonical;
	switch (type->type_kind)
	{
		case TYPE_ARRAY:
		{
			llvm_store_bevalue_raw(c, &len_addr, llvm_const_int(c, type_usize, type->array.len));
			llvm_value_addr(c, &value);
			llvm_store_bevalue_raw(c, &pointer_addr, llvm_emit_bitcast(c, value.value, type_get_ptr(type->array.base)));
			return;
		}
		case TYPE_POINTER:
			// Load the pointer
			llvm_value_rvalue(c, &value);
			llvm_store_bevalue_raw(c, &len_addr, llvm_const_int(c, type_usize, type->pointer->array.len));
			llvm_store_bevalue_raw(c, &pointer_addr, llvm_emit_bitcast(c, value.value, type_get_ptr(type->pointer->array.base)));
			return;
		case TYPE_SUBARRAY:
			*subarray = value;
			return;
		default:
			UNREACHABLE
	}
}

unsigned llvm_get_intrinsic(BuiltinFunction func)
{
	switch (func)
	{
		case BUILTIN_NONE:
			UNREACHABLE
		case BUILTIN_CEIL:
			return intrinsic_id.ceil;
		case BUILTIN_TRUNC:
			return intrinsic_id.trunc;
		case BUILTIN_SQRT:
			return intrinsic_id.sqrt;
		case BUILTIN_COS:
			return intrinsic_id.cos;
		case BUILTIN_SIN:
			return intrinsic_id.sin;
		case BUILTIN_LOG:
			return intrinsic_id.log;
		case BUILTIN_LOG10:
			return intrinsic_id.log10;
		case BUILTIN_MAX:
			return intrinsic_id.maxnum;
		case BUILTIN_MIN:
			return intrinsic_id.minnum;
		case BUILTIN_FABS:
			return intrinsic_id.fabs;
		case BUILTIN_FMA:
			return intrinsic_id.fma;
		case BUILTIN_LOG2:
			return intrinsic_id.log2;
		case BUILTIN_POW:
			return intrinsic_id.pow;
		case BUILTIN_EXP:
			return intrinsic_id.exp;
		case BUILTIN_VOLATILE_STORE:
		case BUILTIN_VOLATILE_LOAD:
			UNREACHABLE
	}
	UNREACHABLE
}

LLVMAtomicOrdering llvm_atomic_ordering(Atomicity atomicity)
{
	switch (atomicity)
	{
		case ATOMIC_NONE: return LLVMAtomicOrderingNotAtomic;
		case ATOMIC_UNORDERED: return LLVMAtomicOrderingUnordered;
		case ATOMIC_RELAXED: return LLVMAtomicOrderingMonotonic;
		case ATOMIC_ACQUIRE: return LLVMAtomicOrderingAcquire;
		case ATOMIC_RELEASE: return LLVMAtomicOrderingRelease;
		case ATOMIC_ACQUIRE_RELEASE: return LLVMAtomicOrderingAcquireRelease;
		case ATOMIC_SEQ_CONSISTENT: return LLVMAtomicOrderingSequentiallyConsistent;
	}
	UNREACHABLE
}

void llvm_emit_builtin_call(GenContext *c, BEValue *result_value, Expr *expr)
{
	BuiltinFunction func = expr->call_expr.function->builtin_expr.builtin;
	if (func == BUILTIN_VOLATILE_STORE)
	{
		BEValue value;
		llvm_emit_expr(c, &value, expr->call_expr.arguments[0]);
		llvm_emit_expr(c, result_value, expr->call_expr.arguments[1]);
		llvm_value_rvalue(c, &value);
		value.kind = BE_ADDRESS;
		BEValue store_value = *result_value;
		LLVMValueRef store = llvm_store_bevalue(c, &value, &store_value);
		if (store) LLVMSetVolatile(store, true);
		return;
	}
	if (func == BUILTIN_VOLATILE_LOAD)
	{
		llvm_emit_expr(c, result_value, expr->call_expr.arguments[0]);
		llvm_value_rvalue(c, result_value);
		result_value->kind = BE_ADDRESS;
		result_value->type = type_lowering(result_value->type->pointer);
		llvm_value_rvalue(c, result_value);
		LLVMSetVolatile(result_value->value, true);
		return;
	}
	llvm_emit_intrinsic_expr(c, llvm_get_intrinsic(func), result_value, expr);
}

void llvm_emit_call_expr(GenContext *c, BEValue *result_value, Expr *expr)
{
	if (expr->call_expr.is_builtin)
	{
		llvm_emit_builtin_call(c, result_value, expr);
		return;
	}
	FunctionSignature *signature;
	LLVMTypeRef func_type;
	LLVMValueRef func;
	BEValue temp_value;

	bool always_inline = false;

	// 1. Call through a pointer.
	if (expr->call_expr.is_pointer_call)
	{
		Expr *function = expr->call_expr.function;

		// 1a. Find the pointee type for the function pointer:
		Type *type = function->type->canonical->pointer;

		// 1b. Find the type signature using the underlying pointer.
		signature = type->func.signature;

		// 1c. Evaluate the pointer expression.
		BEValue func_value;
		llvm_emit_expr(c, &func_value, expr->call_expr.function);

		// 1d. Load it as a value
		func = llvm_value_rvalue_store(c, &func_value);

		// 1e. Calculate the function type
		func_type = llvm_get_type(c, type);
	}
	else
	{
		// 2a. Get the function declaration

		Decl *function_decl = expr->call_expr.func_ref;
		always_inline = function_decl->func_decl.attr_inline;

		// 2b. Set signature, function and function type
		signature = &function_decl->func_decl.function_signature;
		func = function_decl->backend_ref;
		assert(func);
		func_type = llvm_get_type(c, function_decl->type);
	}

	LLVMValueRef *values = NULL;

	// 4. Prepare the return abi data.
	ABIArgInfo *ret_info = signature->ret_abi_info;
	Type *return_type = abi_returntype(signature);
	bool is_failable = IS_FAILABLE(signature->rtype);

	// 5. In the case of a failable, the error is replacing the regular return abi.
	LLVMValueRef error_var = NULL;
	if (is_failable)
	{
		ret_info = signature->failable_abi_info;
	}

	*result_value = (BEValue){ .kind = BE_VALUE, .value = NULL };
	// 6. Generate data for the return value.
	switch (ret_info->kind)
	{
		case ABI_ARG_INDIRECT:
			// 6a. We can use the stored error var if there is no redirect.
			if (is_failable && c->error_var && !ret_info->attributes.realign)
			{
				error_var = c->error_var;
				vec_add(values, error_var);
				break;
			}
			// 6b. Return true is indirect, in this case we allocate a local, using the desired alignment on the caller side.
			assert(ret_info->attributes.realign || ret_info->indirect.alignment == type_abi_alignment(return_type));
			AlignSize alignment = ret_info->indirect.alignment;
			llvm_value_set_address_align(result_value, llvm_emit_alloca(c, llvm_get_type(c, return_type), alignment, "sretparam"), return_type, alignment);

			// 6c. Add the pointer to the list of arguments.
			vec_add(values, result_value->value);
			break;
		case ABI_ARG_EXPAND:
			UNREACHABLE
		case ABI_ARG_DIRECT_PAIR:
		case ABI_ARG_IGNORE:
		case ABI_ARG_DIRECT_COERCE:
		case ABI_ARG_EXPAND_COERCE:
			break;
	}


	// 7. We might have a failable indirect return and a normal return.
	//    In this case we need to add it by hand.
	BEValue synthetic_return_param = { 0 };
	if (is_failable && signature->ret_abi_info)
	{
		// 7b. Create the address to hold the return.
		Type *actual_return_type = type_lowering(signature->rtype->type);
		llvm_value_set(&synthetic_return_param, llvm_emit_alloca_aligned(c, actual_return_type, "retparam"), type_get_ptr(actual_return_type));
		// 7c. Emit it as a parameter as a pointer (will implicitly add it to the value list)
		llvm_emit_parameter(c, &values, signature->ret_abi_info, &synthetic_return_param, synthetic_return_param.type);
		// 7d. Update the be_value to actually be an address.
		llvm_value_set_address(&synthetic_return_param, synthetic_return_param.value, actual_return_type);
	}


	// 8. Add all other arguments.
	unsigned arguments = vec_size(expr->call_expr.arguments);
	unsigned non_variadic_params = vec_size(signature->params);
	if (signature->variadic == VARIADIC_TYPED) non_variadic_params--;
	assert(arguments >= non_variadic_params);
	for (unsigned i = 0; i < non_variadic_params; i++)
	{
		// 8a. Evaluate the expression.
		Expr *arg_expr = expr->call_expr.arguments[i];
		llvm_emit_expr(c, &temp_value, arg_expr);

		// 8b. Emit the parameter according to ABI rules.
		Decl *param = signature->params[i];
		ABIArgInfo *info = param->var.abi_info;
		llvm_emit_parameter(c, &values, info, &temp_value, param->type);
	}

	// 9. Typed varargs
	if (signature->variadic == VARIADIC_TYPED)
	{
		REMINDER("All varargs should be called with non-alias!");
		Decl *vararg_param = signature->params[non_variadic_params];

		BEValue subarray;

		llvm_value_set_address(&subarray, llvm_emit_alloca_aligned(c, vararg_param->type, "vararg"), vararg_param->type);

		// 9a. Special case, empty argument
		if (arguments == non_variadic_params)
		{
			// Just set the size to zero.
			BEValue len_addr;
			llvm_emit_subarray_len(c, &subarray, &len_addr);
			llvm_store_bevalue_raw(c, &len_addr, llvm_get_zero(c, type_usize));
		}
		else if (arguments == non_variadic_params + 1 && expr->call_expr.unsplat_last)
		{
			// 9b. We unpack the last type which is either a slice, an array or a dynamic array.
			llvm_emit_unpacked_variadic_arg(c, expr->call_expr.arguments[non_variadic_params], &subarray);
		}
		else
		{
			// 9b. Otherwise we also need to allocate memory for the arguments:
			Type *pointee_type = vararg_param->type->array.base;
			LLVMTypeRef llvm_pointee = llvm_get_type(c, pointee_type);
			Type *array = type_get_array(pointee_type, arguments - non_variadic_params);
			LLVMTypeRef llvm_array_type = llvm_get_type(c, array);
			AlignSize alignment = type_alloca_alignment(array);
			LLVMValueRef array_ref = llvm_emit_alloca(c, llvm_array_type, alignment, "varargslots");
			for (unsigned i = non_variadic_params; i < arguments; i++)
			{
				Expr *arg_expr = expr->call_expr.arguments[i];
				llvm_emit_expr(c, &temp_value, arg_expr);
				AlignSize store_alignment;
				LLVMValueRef slot = llvm_emit_array_gep_raw(c, array_ref, llvm_array_type, i - non_variadic_params, alignment, &store_alignment);
				llvm_store_bevalue_aligned(c, slot, &temp_value, store_alignment);
			}
			BEValue len_addr;
			llvm_emit_subarray_len(c, &subarray, &len_addr);
			llvm_store_bevalue_raw(c, &len_addr, llvm_const_int(c, type_usize, arguments - non_variadic_params));
			BEValue pointer_addr;
			llvm_emit_subarray_pointer(c, &subarray, &pointer_addr);
			Type *array_as_pointer_type = type_get_ptr(pointee_type);
			llvm_store_bevalue_raw(c, &pointer_addr, llvm_emit_bitcast(c, array_ref, array_as_pointer_type));
		}
		ABIArgInfo *info = vararg_param->var.abi_info;
		llvm_emit_parameter(c, &values, info, &subarray, vararg_param->type);
	}
	else
	{
		// 9. Emit varargs.
		for (unsigned i = vec_size(signature->params); i < arguments; i++)
		{
			Expr *arg_expr = expr->call_expr.arguments[i];
			llvm_emit_expr(c, &temp_value, arg_expr);
			REMINDER("Varargs should be expanded correctly");
			vec_add(values, llvm_value_rvalue_store(c, &temp_value));
		}
	}


	// 10. Create the actual call (remember to emit a loc, because we might have shifted loc emitting the params)
	EMIT_LOC(c, expr);
	LLVMValueRef call_value = LLVMBuildCall2(c->builder, func_type, func, values, vec_size(values), "");
	if (signature->call_abi)
	{
		LLVMSetInstructionCallConv(call_value, llvm_call_convention_from_call(signature->call_abi, platform_target.arch, platform_target.os));
	}
	if (expr->call_expr.force_noinline)
	{
		llvm_attribute_add_call(c, call_value, attribute_id.noinline, -1, 0);
	}
	else
	{
		if (expr->call_expr.force_inline || always_inline)
		{
			llvm_attribute_add_call(c, call_value, attribute_id.alwaysinline, -1, 0);
		}
	}

	for (unsigned i = 0; i < non_variadic_params; i++)
	{
		Decl *param = signature->params[i];
		ABIArgInfo *info = param->var.abi_info;
		switch (info->kind)
		{
			case ABI_ARG_INDIRECT:
				if (info->attributes.by_val)
				{
					llvm_attribute_add_call_type(c, call_value, attribute_id.byval, (int)i + 1, llvm_get_type(c, info->indirect.type));
				}
				llvm_attribute_add_call(c, call_value, attribute_id.align, (int)i + 1, info->indirect.alignment);
				break;
			default:
				break;
		}
	}

	// 11. Process the return value.
	switch (ret_info->kind)
	{
		case ABI_ARG_EXPAND:
			UNREACHABLE
		case ABI_ARG_IGNORE:
			// 12. Basically void returns or empty structs.
			//     Here we know we don't have a failable or any return value that can be used.
			assert(!is_failable && "Failable should have produced a return value.");
			*result_value = (BEValue) { .type = type_void, .kind = BE_VALUE };
			return;
		case ABI_ARG_INDIRECT:
			llvm_attribute_add_call_type(c, call_value, attribute_id.sret, 1, llvm_get_type(c, ret_info->indirect.type));
			llvm_attribute_add_call(c, call_value, attribute_id.align, 1, ret_info->indirect.alignment);
			// 13. Indirect, that is passing the result through an out parameter.

			// 13a. In the case of an already present error_var, we don't need to do a load here.
			if (error_var) break;

			// 13b. Otherwise it will be contained in a be_value that is an address
			//      so we don't need to do anything more.
			assert(result_value->kind == BE_ADDRESS);

			break;
		case ABI_ARG_DIRECT_PAIR:
		{
			// 14. A direct pair, in this case the data is stored like { lo, hi }
			//     For example we might have { int, int, short, short, int },
			//     this then gets bitcast to { long, long }, so we recover it by loading
			//     { long, long } into memory, then performing a bitcast to { int, int, short, short, int }

			// 14a. Generate the type.
			LLVMTypeRef lo = llvm_abi_type(c, ret_info->direct_pair.lo);
			LLVMTypeRef hi = llvm_abi_type(c, ret_info->direct_pair.hi);
			LLVMTypeRef struct_type = llvm_get_twostruct(c, lo, hi);

			// 14b. Use the coerce method to go from the struct to the actual type
			//      by storing the { lo, hi } struct to memory, then loading it
			//      again using a bitcast.
			llvm_emit_convert_value_from_coerced(c, result_value, struct_type, call_value, return_type);
			break;
		}
		case ABI_ARG_EXPAND_COERCE:
		{
			// 15. Expand-coerce, this is similar to "direct pair", but looks like this:
			//     { lo, hi } set into { pad, lo, pad, hi } -> original type.

			// 15a. Create memory to hold the return type.
			LLVMValueRef ret = llvm_emit_alloca_aligned(c, return_type, "");
			llvm_value_set_address(result_value, ret, return_type);

			// 15b. "Convert" this return type pointer in memory to our coerce type which is { pad, lo, pad, hi }
			LLVMTypeRef coerce_type = llvm_get_coerce_type(c, ret_info);
			LLVMValueRef coerce = LLVMBuildBitCast(c->builder, ret, coerce_type, "");

			// 15d. Find the address to the low value
			AlignSize alignment;
			LLVMValueRef lo = llvm_emit_struct_gep_raw(c, coerce, coerce_type, ret_info->coerce_expand.lo_index,
			                                           type_abi_alignment(return_type), &alignment);

			// 15e. If there is only a single field, we simply store the value,
			//      so { lo } set into { pad, lo, pad } -> original type.
			if (!ret_info->coerce_expand.hi)
			{
				// Here we do a store to call -> lo (leaving the rest undefined)
				llvm_store_aligned(c, lo, call_value, alignment);
				break;
			}

			// 15g. We can now extract { lo, hi } to lo_value and hi_value.
			LLVMValueRef lo_value = LLVMBuildExtractValue(c->builder, call_value, 0, "");
			LLVMValueRef hi_value = LLVMBuildExtractValue(c->builder, call_value, 1, "");

			// 15h. Store lo_value into the { pad, lo, pad, hi } struct.
			llvm_store_aligned(c, lo, lo_value, alignment);

			// 15i. Calculate the address to the high value (like for the low in 15d.
			LLVMValueRef hi = llvm_emit_struct_gep_raw(c, coerce, coerce_type, ret_info->coerce_expand.hi_index,
			                                           type_abi_alignment(return_type), &alignment);

			// 15h. Store the high value.
			llvm_store_aligned(c, hi, hi_value, alignment);

			break;
		}
		case ABI_ARG_DIRECT_COERCE:
		{
			// 16. A direct coerce, this is basically "call result" bitcast return type.

			// 16a. Get the type of the return.
			LLVMTypeRef coerce = llvm_get_coerce_type(c, ret_info);

			// 16b. If we don't have any coerce type, or the actual LLVM types are the same, we're done.
			if (!coerce || coerce == llvm_get_type(c, return_type))
			{
				// 16c. We just set as a value in be_value.
				llvm_value_set(result_value, call_value, return_type);
				break;
			}
			// 16c. We use a normal bitcast coerce.
			assert(!abi_info_should_flatten(ret_info) && "Did not expect flattening on return types.");
			llvm_emit_convert_value_from_coerced(c, result_value, coerce, call_value, return_type);
			break;
		}
	}

	// 17. Handle failables.
	if (is_failable)
	{
		BEValue no_err;

		// 17a. If we used the error var as the indirect recipient, then that will hold the error.
		//      otherwise it's whatever value in be_value.
		BEValue error_holder = *result_value;
		if (error_var)
		{
			llvm_value_set_address(&error_holder, c->error_var, type_anyerr);
		}
		// 17b. Generate a boolean switch.
		llvm_value_set_bool(&no_err, llvm_emit_is_no_error_value(c, &error_holder));

		// 17c. If we have an error var, or we aren't interested in the error variable
		//      - then it's straightforward. We just jump to the catch block.
		LLVMBasicBlockRef after_block = llvm_basic_block_new(c, "after.errcheck");
		if (error_var || !c->error_var)
		{
			llvm_emit_cond_br(c, &no_err, after_block, c->catch_block);
		}
		else
		{
			// 17d. If we have an error var we need to assign, then we need to
			//      first jump to an error block, where we do the copy.
			LLVMBasicBlockRef error_block = llvm_basic_block_new(c, "error");
			llvm_emit_cond_br(c, &no_err, after_block, error_block);
			llvm_emit_block(c, error_block);
			llvm_store_bevalue_aligned(c, c->error_var, result_value, 0);
			// 17e. Then jump to the catch.
			llvm_emit_br(c, c->catch_block);
		}

		// 17f. Emit the "after" block.
		llvm_emit_block(c, after_block);

		// 17g. If void, be_value contents should be skipped.
		if (!signature->ret_abi_info)
		{
			*result_value = (BEValue) { .type = type_void, .kind = BE_VALUE };
			return;
		}

		// 17h. Assign the return param to be_value.
		*result_value = synthetic_return_param;
		return;
	}

	// 17i. The simple case here is where there is a normal return.
	//      In this case be_value already holds the result
	return;
}




static inline void gencontext_emit_expression_list_expr(GenContext *context, BEValue *be_value, Expr *expr)
{
	VECEACH(expr->expression_list, i)
	{
		llvm_emit_expr(context, be_value, expr->expression_list[i]);
	}
}

static inline void llvm_emit_return_block(GenContext *context, BEValue *be_value, Type *type, Ast **stmts)
{
	// First case - an empty block
	unsigned count = vec_size(stmts);
	if (!count)
	{
		llvm_value_set(be_value, NULL, type_void);
		return;
	}

	Type *type_lowered = type_lowering(type);
	LLVMValueRef old_ret_out = context->return_out;
	LLVMBasicBlockRef saved_block_return_exit = context->block_return_exit;
	LLVMBasicBlockRef saved_block_failable_exit = context->block_failable_exit;
	LLVMValueRef saved_block_error = context->block_error_var;
	context->in_block++;

	LLVMBasicBlockRef expr_block = llvm_basic_block_new(context, "expr_block.exit");
	context->block_return_exit = expr_block;

	LLVMValueRef return_out = NULL;
	LLVMValueRef error_out = context->error_var;
	LLVMBasicBlockRef error_block = context->catch_block;

	if (type_lowered != type_void)
	{
		return_out = llvm_emit_alloca_aligned(context, type_lowered, "blockret");
	}
	context->block_error_var = error_out;
	context->block_failable_exit = error_block;
	context->return_out = return_out;
	context->error_var = NULL;
	context->catch_block = NULL;

	// Process all but the last statement.
	for (unsigned i = 0; i < count - 1; i++)
	{
		llvm_emit_stmt(context, stmts[i]);
	}

	Ast *last_stmt = stmts[count - 1];

	do
	{
		// Do we have more than one exit?
		// Then follow the normal path.
		if (!llvm_basic_block_is_unused(expr_block)) break;

		// Do we have a void function? That's the only
		// possible case if the last statement isn't return.
		if (last_stmt->ast_kind != AST_RETURN_STMT) break;

		// Defers? In that case we also use the default behaviour.
		// We might optimize this later.
		if (last_stmt->return_stmt.defer) break;

		Expr *ret_expr = last_stmt->return_stmt.expr;

		// If this is a void return, we can just skip here!
		if (!ret_expr)
		{
			llvm_value_set(be_value, NULL, type_void);
			goto DONE;
		}

		// Failable? Then we use the normal path
		if (IS_FAILABLE(ret_expr)) break;

		// Optimization, emit directly to value
		llvm_emit_expr(context, be_value, ret_expr);
		// And remove the alloca
		LLVMInstructionEraseFromParent(context->return_out);
		goto DONE;

	} while (0);

	// Emit the last statement
	llvm_emit_stmt(context, last_stmt);

	// In the case of a void with no return, then this may be true.
	if (llvm_basic_block_is_unused(expr_block))
	{
		// Skip the expr block.
		assert(!context->return_out);
		llvm_value_set(be_value, NULL, type_void);
		goto DONE;
	}

	llvm_emit_br(context, expr_block);

	// Emit the exit block.
	llvm_emit_block(context, expr_block);

	if (return_out)
	{
		llvm_value_set_address(be_value, return_out, type_lowered);
	}
	else
	{
		llvm_value_set(be_value, NULL, type_void);
	}

DONE:
	context->return_out = old_ret_out;
	context->catch_block = error_block;
	context->error_var = error_out;
	context->block_return_exit = saved_block_return_exit;
	context->block_failable_exit = saved_block_failable_exit;
	context->block_error_var = saved_block_error;
	context->in_block--;

}

static inline void llvm_emit_expr_block(GenContext *context, BEValue *be_value, Expr *expr)
{
	llvm_emit_return_block(context, be_value, expr->type, expr->expr_block.stmts);
}

static inline void llvm_emit_macro_block(GenContext *context, BEValue *be_value, Expr *expr)
{
	VECEACH(expr->macro_block.params, i)
	{
		// In case we have a constant, we never do an emit. The value is already folded.
		Decl *decl = expr->macro_block.params[i];
		switch (decl->var.kind)
		{
			case VARDECL_CONST:
			case VARDECL_GLOBAL:
			case VARDECL_LOCAL:
			case VARDECL_MEMBER:
			case VARDECL_LOCAL_CT:
			case VARDECL_LOCAL_CT_TYPE:
			case VARDECL_UNWRAPPED:
			case VARDECL_REWRAPPED:
			case VARDECL_ERASE:
			case VARDECL_BITMEMBER:
				UNREACHABLE
			case VARDECL_PARAM_REF:
			{
				BEValue addr;
				llvm_emit_expr(context, &addr, decl->var.init_expr);
				decl->backend_ref = addr.value;
				continue;
			}
			case VARDECL_PARAM_CT:
			case VARDECL_PARAM_CT_TYPE:
			case VARDECL_PARAM_EXPR:
				continue;
			case VARDECL_PARAM:
				break;
		}
		llvm_emit_and_set_decl_alloca(context, decl);
		BEValue value;

		llvm_emit_expr(context, &value, expr->macro_block.args[i]);
		llvm_store_aligned_decl(context, decl, llvm_value_rvalue_store(context, &value));
	}

	llvm_emit_return_block(context, be_value, expr->type, expr->macro_block.stmts);
}

LLVMValueRef llvm_emit_call_intrinsic(GenContext *context, unsigned intrinsic, LLVMTypeRef *types, unsigned type_count,
                                      LLVMValueRef *values, unsigned arg_count)
{
	LLVMValueRef decl = LLVMGetIntrinsicDeclaration(context->module, intrinsic, types, type_count);
	LLVMTypeRef type = LLVMIntrinsicGetType(context->context, intrinsic, types, arg_count);
	return LLVMBuildCall2(context->builder, type, decl, values, arg_count, "");
}

BEValue llvm_emit_assign_expr(GenContext *c, BEValue *ref, Expr *expr, LLVMValueRef failable)
{
	assert(ref->kind == BE_ADDRESS || ref->kind == BE_ADDRESS_FAILABLE);
	LLVMBasicBlockRef assign_block = NULL;

	PUSH_ERROR();

	if (failable)
	{
		if (IS_FAILABLE(expr))
		{
			if (expr->expr_kind == EXPR_FAILABLE)
			{
				c->error_var = NULL;
				c->catch_block = NULL;
				BEValue result;
				llvm_emit_expr(c, &result, expr->inner_expr);
				llvm_store_bevalue_dest_aligned(c, failable, &result);
				llvm_value_set(&result, LLVMGetUndef(llvm_get_type(c, ref->type)), ref->type);
				POP_ERROR();
				return result;
			}
			assign_block = llvm_basic_block_new(c, "after_assign");
			c->error_var = failable;
			c->catch_block = assign_block;
		}
		else
		{
			c->error_var = NULL;
			c->catch_block = NULL;
		}
	}
	BEValue value;
	if (type_is_vector(expr->type))
	{
		llvm_emit_expr(c, &value, expr);
		llvm_store_bevalue(c, ref, &value);
	}
	else if (expr->expr_kind == EXPR_CONST && expr->const_expr.const_kind == CONST_LIST)
	{
		llvm_emit_const_initialize_reference(c, ref, expr);
		value = *ref;
	}
	else if (expr_is_init_list(expr))
	{
		llvm_emit_initialize_reference(c, ref, expr);
		value = *ref;
	}
	else
	{
		llvm_emit_expr(c, &value, expr);
		llvm_store_bevalue(c, ref, &value);
	}

	if (failable)
	{
		llvm_store_self_aligned(c, failable, llvm_get_zero(c, type_anyerr), type_anyerr);
	}
	POP_ERROR();

	if (failable && IS_FAILABLE(expr))
	{
		llvm_emit_br(c, assign_block);
		llvm_emit_block(c, assign_block);
	}

	return value;
}



static inline void llvm_emit_failable(GenContext *c, BEValue *be_value, Expr *expr)
{
	Expr *fail = expr->inner_expr;
	// If there is an error value, assign to it.
	if (c->error_var)
	{
		assert(c->error_var);
		llvm_emit_expr(c, be_value, fail);
		llvm_store_bevalue_dest_aligned(c, c->error_var, be_value);
	}
	// Branch to the catch
	llvm_emit_br(c, c->catch_block);
	// Create an empty block
	LLVMBasicBlockRef ignored_block = llvm_basic_block_new(c, "postfailed");
	llvm_emit_block(c, ignored_block);

	// Finally we need to replace the result with something undefined here.
	// It will be optimized away.
	Type *type = type_no_fail(expr->type);
	if (type->canonical == type_void)
	{
		llvm_value_set(be_value, NULL, type_void);
		return;
	}
	llvm_value_set(be_value, LLVMGetUndef(llvm_get_type(c, type)), type);
}

static inline LLVMValueRef llvm_update_vector(GenContext *c, LLVMValueRef vector, LLVMValueRef value, MemberIndex index, bool *is_const)
{
	LLVMValueRef index_value = llvm_const_int(c, type_usize, (uint64_t)index);
	if (*is_const && LLVMIsConstant(value))
	{
		return LLVMConstInsertElement(vector, value, index_value);
	}
	else
	{
		*is_const = false;
		return LLVMBuildInsertElement(c->builder, vector, value, index_value, "");
	}

}
static inline void llvm_emit_vector_initializer_list(GenContext *c, BEValue *value, Expr *expr)
{
	Type *type = type_lowering(expr->type);
	Type *element_type = type->vector.base;

	LLVMTypeRef llvm_type = llvm_get_type(c, type);

	BEValue val;
	LLVMValueRef vec_value;

	bool is_const = true;
	if (expr->expr_kind == EXPR_INITIALIZER_LIST)
	{
		vec_value = LLVMGetUndef(llvm_type);
		Expr **elements = expr->initializer_list;

		// Now walk through the elements.
		VECEACH(elements, i)
		{
			Expr *element = elements[i];
			llvm_emit_expr(c, &val, element);
			llvm_value_rvalue(c, &val);
			vec_value = llvm_update_vector(c, vec_value, val.value, (MemberIndex)i, &is_const);
		}
	}
	else
	{
		vec_value = LLVMConstNull(llvm_type);
		Expr **elements = expr->designated_init_list;

		VECEACH(elements, i)
		{
			Expr *designator = elements[i];
			assert(vec_size(designator->designator_expr.path) == 1);
			DesignatorElement *element = designator->designator_expr.path[0];
			llvm_emit_expr(c, &val, designator->designator_expr.value);
			llvm_value_rvalue(c, &val);
			switch (element->kind)
			{
				case DESIGNATOR_ARRAY:
				{
					vec_value = llvm_update_vector(c, vec_value, val.value, element->index, &is_const);
					break;
				}
				case DESIGNATOR_RANGE:
					for (MemberIndex idx = element->index; idx <= element->index_end; idx++)
					{
						vec_value = llvm_update_vector(c, vec_value, val.value, idx, &is_const);
					}
					break;
				case DESIGNATOR_FIELD:
				default:
					UNREACHABLE
			}
		}
	}
	llvm_value_set(value, vec_value, type);
}

static inline void llvm_emit_initializer_list_expr(GenContext *c, BEValue *value, Expr *expr)
{
	Type *type = type_lowering(expr->type);
	if (type_is_vector(type))
	{
		llvm_emit_vector_initializer_list(c, value, expr);
		return;
	}
	assert(!IS_FAILABLE(expr) || c->catch_block);
	llvm_value_set_address(value, llvm_emit_alloca_aligned(c, type, "literal"), type);
	llvm_emit_initialize_reference(c, value, expr);
}

static void llvm_emit_macro_body_expansion(GenContext *c, BEValue *value, Expr *body_expr)
{
	Decl **declarations = body_expr->body_expansion_expr.declarations;
	Expr **values = body_expr->body_expansion_expr.values;
	// Create backend refs on demand.
	foreach(declarations, i)
	{
		Decl *decl = declarations[i];
		if (!decl->backend_ref) llvm_emit_local_var_alloca(c, decl);
	}
	// Set the values
	foreach(values, i)
	{
		Expr *expr = values[i];
		llvm_emit_expr(c, value, expr);
		llvm_store_bevalue_aligned(c, declarations[i]->backend_ref, value, declarations[i]->alignment);
	}
	llvm_emit_stmt(c, body_expr->body_expansion_expr.ast);
}

void llvm_emit_try_unwrap(GenContext *c, BEValue *value, Expr *expr)
{
	if (!expr->try_unwrap_expr.failable)
	{
		LLVMValueRef fail_ref = decl_failable_ref(expr->try_unwrap_expr.decl);
		LLVMValueRef errv = llvm_emit_load_aligned(c, llvm_get_type(c, type_anyerr), fail_ref, 0, "load.err");
		LLVMValueRef result = LLVMBuildICmp(c->builder, LLVMIntEQ, errv, llvm_get_zero(c, type_anyerr), "result");
		llvm_value_set_bool(value, result);
		return;
	}
	BEValue addr;
	if (expr->try_unwrap_expr.assign_existing)
	{
		llvm_emit_expr(c, &addr, expr->try_unwrap_expr.lhs);
	}
	else
	{
		llvm_emit_local_decl(c, expr->try_unwrap_expr.decl);
		llvm_value_set_decl_address(&addr, expr->try_unwrap_expr.decl);
	}
	assert(llvm_value_is_addr(&addr));
	llvm_emit_try_assign_try_catch(c, true, value, &addr, NULL, expr->try_unwrap_expr.failable);
}

void llvm_emit_catch_unwrap(GenContext *c, BEValue *value, Expr *expr)
{
	BEValue addr;
	if (expr->catch_unwrap_expr.lhs)
	{
		llvm_emit_expr(c, &addr, expr->catch_unwrap_expr.lhs);
	}
	else if (expr->catch_unwrap_expr.decl)
	{
		llvm_emit_local_decl(c, expr->catch_unwrap_expr.decl);
		llvm_value_set_decl_address(&addr, expr->catch_unwrap_expr.decl);
	}
	else
	{
		LLVMValueRef temp_err = llvm_emit_alloca_aligned(c, type_anyerr, "temp_err");
		llvm_value_set_address(&addr, temp_err, type_anyerr);
	}

	PUSH_ERROR();

	LLVMBasicBlockRef catch_block = llvm_basic_block_new(c, "end_block");

	c->error_var = addr.value;
	c->catch_block = catch_block;

	VECEACH(expr->catch_unwrap_expr.exprs, i)
	{
		BEValue val;
		LLVMBasicBlockRef block = llvm_basic_block_new(c, "testblock");
		llvm_emit_br(c, block);
		llvm_emit_block(c, block);
		llvm_emit_expr(c, &val, expr->catch_unwrap_expr.exprs[i]);
		llvm_value_fold_failable(c, &val);
	}

	POP_ERROR();

	llvm_store_bevalue_raw(c, &addr, llvm_get_zero(c, type_anyerr));
	llvm_emit_br(c, catch_block);
	llvm_emit_block(c, catch_block);
	llvm_value_rvalue(c, &addr);
	llvm_value_set(value, addr.value, type_anyerr);
}

void llvm_emit_try_unwrap_chain(GenContext *c, BEValue *value, Expr *expr)
{
	Expr **exprs = expr->try_unwrap_chain_expr;
	unsigned elements = vec_size(exprs);
	assert(elements > 0);

	LLVMBasicBlockRef next_block = NULL;
	LLVMBasicBlockRef end_block = llvm_basic_block_new(c, "end_chain");
	LLVMBasicBlockRef fail_block = llvm_basic_block_new(c, "fail_chain");

	if (elements == 1)
	{
		llvm_emit_expr(c, value, exprs[0]);
		assert(llvm_value_is_bool(value));
		return;
	}
	else
	{
		for (unsigned i = 0; i < elements; i++)
		{
			if (next_block)
			{
				llvm_emit_br(c, next_block);
				llvm_emit_block(c, next_block);
			}
			next_block = llvm_basic_block_new(c, "chain_next");
			Expr *link = exprs[i];
			BEValue res;
			llvm_emit_expr(c, &res, link);
			assert(llvm_value_is_bool(&res));
			llvm_emit_cond_br(c, &res, next_block, fail_block);
		}
		llvm_emit_block(c, next_block);
		llvm_emit_br(c, end_block);
		llvm_emit_block(c, fail_block);
		llvm_emit_br(c, end_block);
	}

	// Finally set up our phi
	llvm_emit_block(c, end_block);
	LLVMValueRef chain_result = LLVMBuildPhi(c->builder, c->bool_type, "chain.phi");
	LLVMValueRef logic_values[2] = { LLVMConstInt(c->bool_type, 1, 0), LLVMConstNull(c->bool_type) };
	LLVMBasicBlockRef blocks[2] = { next_block, fail_block };
	LLVMAddIncoming(chain_result, logic_values, blocks, 2);

	llvm_value_set_bool(value, chain_result);

}

void llvm_emit_expr(GenContext *c, BEValue *value, Expr *expr)
{
	EMIT_LOC(c, expr);
	switch (expr->expr_kind)
	{
		case EXPR_DESIGNATOR:
		case EXPR_POISONED:
		case EXPR_COND:
		case EXPR_TYPEINFO:
		case EXPR_MACRO_EXPANSION:
		case EXPR_CT_IDENT:
		case EXPR_HASH_IDENT:
		case EXPR_PLACEHOLDER:
		case EXPR_CT_CALL:
		case EXPR_FLATPATH:
			UNREACHABLE
		case EXPR_TRY_UNWRAP_CHAIN:
			llvm_emit_try_unwrap_chain(c, value, expr);
			return;
		case EXPR_TRY_UNWRAP:
			llvm_emit_try_unwrap(c, value, expr);
			return;
		case EXPR_CATCH_UNWRAP:
			llvm_emit_catch_unwrap(c, value, expr);
			return;
		case EXPR_UNDEF:
			// Should never reach this.
			UNREACHABLE
		case EXPR_BUILTIN:
			TODO
		case EXPR_DECL:
			llvm_emit_local_decl(c, expr->decl_expr);
			return;
		case EXPR_SLICE_ASSIGN:
			llvm_emit_slice_assign(c, value, expr);
			return;
		case EXPR_SLICE:
			gencontext_emit_slice(c, value, expr);
			return;
		case EXPR_LEN:
			llvm_emit_len(c, value, expr);
			return;
		case EXPR_FAILABLE:
			llvm_emit_failable(c, value, expr);
			return;
		case EXPR_TRY:
			llvm_emit_try_expr(c, value, expr);
			return;
		case EXPR_CATCH:
			llvm_emit_catch_expr(c, value, expr);
			return;
		case EXPR_NOP:
			llvm_value_set(value, NULL, type_void);
			return;
		case EXPR_OR_ERROR:
			gencontext_emit_or_error(c, value, expr);
			return;
		case EXPR_MACRO_BLOCK:
			llvm_emit_macro_block(c, value, expr);
			return;
		case EXPR_COMPOUND_LITERAL:
			UNREACHABLE
		case EXPR_INITIALIZER_LIST:
		case EXPR_DESIGNATED_INITIALIZER_LIST:
			llvm_emit_initializer_list_expr(c, value, expr);
			return;
		case EXPR_EXPR_BLOCK:
			llvm_emit_expr_block(c, value, expr);
			return;
		case EXPR_SCOPED_EXPR:
			gencontext_emit_scoped_expr(c, value, expr);
			return;
		case EXPR_UNARY:
			gencontext_emit_unary_expr(c, value, expr);
			return;
		case EXPR_CONST:
			llvm_emit_const_expr(c, value, expr);
			return;
		case EXPR_MACRO_BODY_EXPANSION:
			llvm_emit_macro_body_expansion(c, value, expr);
			return;
		case EXPR_BITASSIGN:
			llvm_emit_bitassign_expr(c, value, expr);
			return;
		case EXPR_BINARY:
			llvm_emit_binary_expr(c, value, expr);
			return;
		case EXPR_TERNARY:
			gencontext_emit_ternary_expr(c, value, expr);
			return;
		case EXPR_POST_UNARY:
			llvm_emit_post_unary_expr(c, value, expr);
			return;
		case EXPR_FORCE_UNWRAP:
			llvm_emit_force_unwrap_expr(c, value, expr);
			return;
		case EXPR_RETHROW:
			llvm_emit_rethrow_expr(c, value, expr);
			return;
		case EXPR_TYPEOFANY:
			llvm_emit_typeofany(c, value, expr);
			return;
		case EXPR_TYPEID:
		case EXPR_GROUP:
			// These are folded in the semantic analysis step.
			UNREACHABLE
		case EXPR_IDENTIFIER:
		case EXPR_CONST_IDENTIFIER:
			llvm_value_set_decl_address(value, expr->identifier_expr.decl);
			return;
		case EXPR_SUBSCRIPT:
		case EXPR_SUBSCRIPT_ADDR:
			gencontext_emit_subscript(c, value, expr);
			return;
		case EXPR_ACCESS:
			gencontext_emit_access_addr(c, value, expr);
			return;
		case EXPR_CALL:
			llvm_emit_call_expr(c, value, expr);
			return;
		case EXPR_EXPRESSION_LIST:
			gencontext_emit_expression_list_expr(c, value, expr);
			return;
		case EXPR_CAST:
			gencontext_emit_cast_expr(c, value, expr);
			return;
		case EXPR_BITACCESS:
			llvm_emit_bitaccess(c, value, expr);
			return;
	}
	UNREACHABLE
}

