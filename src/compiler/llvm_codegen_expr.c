// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_internal.h"
#include <math.h>


static LLVMValueRef llvm_emit_coerce_alignment(GenContext *c, BEValue *be_value, LLVMTypeRef coerce_type, AlignSize target_alignment, AlignSize *resulting_alignment);
static bool bitstruct_requires_bitswap(Decl *decl);
static inline LLVMValueRef llvm_const_high_bitmask(GenContext *c, LLVMTypeRef type, int type_bits, int high_bits);
static inline LLVMValueRef llvm_const_low_bitmask(GenContext *c, LLVMTypeRef type, int type_bits, int low_bits);
static inline LLVMValueRef llvm_update_vector(GenContext *c, LLVMValueRef vector, LLVMValueRef value, ArrayIndex index);
static inline void llvm_emit_expression_list_expr(GenContext *c, BEValue *be_value, Expr *expr);
static LLVMValueRef llvm_emit_dynamic_search(GenContext *c, LLVMValueRef type_id_ptr, LLVMValueRef selector);
static inline void llvm_emit_bitassign_array(GenContext *c, LLVMValueRef result, BEValue parent, Decl *parent_decl, Decl *member);
static inline void llvm_emit_builtin_access(GenContext *c, BEValue *be_value, Expr *expr);
static inline void llvm_emit_const_initialize_reference(GenContext *c, BEValue *ref, Expr *expr);
static inline void llvm_emit_expr_block(GenContext *context, BEValue *be_value, Expr *expr);
static inline void llvm_emit_optional(GenContext *c, BEValue *be_value, Expr *expr);
static inline void llvm_emit_inc_dec_change(GenContext *c, BEValue *addr, BEValue *after, BEValue *before, Expr *expr, int diff,
                         bool allow_wrap);
static inline void llvm_emit_initialize_reference(GenContext *c, BEValue *ref, Expr *expr);
static inline void llvm_emit_initialize_reference_bitstruct(GenContext *c, BEValue *ref, Decl *bitstruct, Expr** elements);
static inline void llvm_emit_initialize_reference_list(GenContext *c, BEValue *ref, Expr *expr);
static inline void llvm_emit_initialize_reference_vector(GenContext *c, BEValue *ref, Type *real_type, Expr **elements);
static inline void llvm_emit_initializer_list_expr(GenContext *c, BEValue *value, Expr *expr);
static inline void llvm_emit_macro_block(GenContext *c, BEValue *be_value, Expr *expr);
static inline void llvm_emit_post_inc_dec(GenContext *c, BEValue *value, Expr *expr, int diff, bool allow_wrap);
static inline void llvm_emit_pre_inc_dec(GenContext *c, BEValue *value, Expr *expr, int diff, bool allow_wrap);
static inline void llvm_emit_return_block(GenContext *c, BEValue *be_value, Type *type, AstId current, BlockExit **block_exit);
static inline void llvm_emit_subscript_addr_with_base(GenContext *c, BEValue *result, BEValue *parent, BEValue *index, SourceSpan loc);
static inline void llvm_emit_try_unwrap(GenContext *c, BEValue *value, Expr *expr);
static inline void llvm_emit_vector_initializer_list(GenContext *c, BEValue *value, Expr *expr);
static inline void llvm_extract_bitvalue_from_array(GenContext *c, BEValue *be_value, Decl *member, Decl *parent_decl);
static inline void llvm_emit_type_from_any(GenContext *c, BEValue *be_value);
static void llvm_convert_vector_comparison(GenContext *c, BEValue *be_value, LLVMValueRef val, Type *vector_type,
										   bool is_equals);
static void llvm_emit_any_pointer(GenContext *c, BEValue *any, BEValue *pointer);
static void llvm_emit_binary(GenContext *c, BEValue *be_value, Expr *expr, BEValue *lhs_loaded, BinaryOp binary_op);
static void llvm_emit_call_expr(GenContext *c, BEValue *result_value, Expr *expr, BEValue *target);
static void llvm_emit_const_expr(GenContext *c, BEValue *be_value, Expr *expr);
static void llvm_emit_initialize_designated_element(GenContext *c, BEValue *ref, AlignSize offset, DesignatorElement** current, DesignatorElement **last, Expr *expr, BEValue *emitted_value);
static void llvm_emit_macro_body_expansion(GenContext *c, BEValue *value, Expr *body_expr);
static void llvm_emit_post_unary_expr(GenContext *context, BEValue *be_value, Expr *expr);
static void llvm_emit_unary_expr(GenContext *c, BEValue *value, Expr *expr);
static inline void llvm_emit_memcmp(GenContext *c, BEValue *be_value, LLVMValueRef ptr, LLVMValueRef other_ptr, LLVMValueRef size);
static LLVMTypeRef llvm_find_inner_struct_type_for_coerce(GenContext *c, LLVMTypeRef struct_type, ByteSize dest_size);
static void llvm_expand_type_to_args(GenContext *context, Type *param_type, LLVMValueRef expand_ptr, LLVMValueRef *args, unsigned *arg_count_ref, AlignSize alignment);
static inline void llvm_emit_initialize_reference_designated_bitstruct(GenContext *c, BEValue *ref, Decl *bitstruct, Expr **elements);
INLINE LLVMValueRef llvm_emit_bitstruct_value_update(GenContext *c, LLVMValueRef current_val, TypeSize bits, LLVMTypeRef bitstruct_type, Decl *member, LLVMValueRef val);
INLINE void llvm_emit_initialize_reference_bitstruct_array(GenContext *c, BEValue *ref, Decl *bitstruct, Expr** elements);
#define MAX_AGG 16

/**
 * Emit an expression into a value (as opposed to the address)
 * @param c the current context
 * @param expr the expression to emit
 * @return the LLVM value.
 */
LLVMValueRef llvm_emit_expr_to_rvalue(GenContext *c, Expr *expr)
{
	BEValue value;
	llvm_emit_expr(c, &value, expr);
	llvm_value_rvalue(c, &value);
	return value.value;
}

LLVMValueRef llvm_emit_exprid_to_rvalue(GenContext *c, ExprId expr_id)
{
	BEValue value;
	llvm_emit_exprid(c, &value, expr_id);
	llvm_value_rvalue(c, &value);
	return value.value;
}

void llvm_emit_assume_true(GenContext *c, BEValue *assume_true)
{
	assert(llvm_value_is_bool(assume_true));
	LLVMValueRef value = assume_true->value;
	llvm_emit_call_intrinsic(c, intrinsic_id.assume, NULL, 0, &value, 1);
}

LLVMValueRef llvm_emit_expect_false(GenContext *c, BEValue *expect_false)
{
	assert(llvm_value_is_bool(expect_false));
	LLVMValueRef values[2] = { expect_false->value, LLVMConstNull(c->bool_type) };
	return llvm_emit_call_intrinsic(c, intrinsic_id.expect, &c->bool_type, 1, values, 2);
}

LLVMValueRef llvm_emit_expect_raw(GenContext *c, LLVMValueRef expect_true)
{
	LLVMValueRef values[2] = { expect_true, LLVMConstInt(c->bool_type, 1, false) };
	return llvm_emit_call_intrinsic(c, intrinsic_id.expect, &c->bool_type, 1, values, 2);
}

BEValue llvm_emit_assign_expr(GenContext *c, BEValue *ref, Expr *expr, LLVMValueRef optional, bool is_init)
{
	assert(llvm_value_is_addr(ref));

	assert((optional || !IS_OPTIONAL(expr)) && "Assumed an optional address if it's an optional expression.");

	// Special optimization of handling of optional
	if (expr->expr_kind == EXPR_OPTIONAL)
	{
		PUSH_CLEAR_CATCH();

		BEValue result;
		// Emit the fault type.
		llvm_emit_expr(c, &result, expr->inner_expr);
		llvm_value_rvalue(c, &result);

		LLVMValueRef err_val = result.value;
		// Store it in the optional
		llvm_store_to_ptr(c, optional, &result);
		// Set the result to an undef value
		llvm_value_set(&result, llvm_get_undef(c, ref->type), ref->type);

		POP_CATCH();

		// If we had a catch block outside then we want to jump to that exit.
		if (c->catch.block) llvm_emit_jump_to_optional_exit(c, err_val);

		// This return value will not be used.
		return result;
	}

	PUSH_CATCH();

	LLVMBasicBlockRef assign_block = NULL;
	LLVMBasicBlockRef rejump_block = NULL;

	if (IS_OPTIONAL(expr))
	{
		assign_block = llvm_basic_block_new(c, "after_assign");
		assert(optional);
		if (c->catch.fault)
		{
			c->catch.block = rejump_block = llvm_basic_block_new(c, "optional_assign_jump");
		}
		else
		{
			c->catch.block = assign_block;
		}
		c->catch.fault = optional;
	}
	else
	{
		c->catch = NO_CATCH;
	}

	BEValue value;
	if (type_flat_is_vector(expr->type))
	{
		llvm_emit_expr(c, &value, expr);
		llvm_store(c, ref, &value);
	}
	else if (expr_is_const_initializer(expr))
	{
		llvm_emit_const_initialize_reference(c, ref, expr);
		value = *ref;
	}
	else if (expr_is_init_list(expr))
	{
		if (is_init)
		{
			llvm_emit_initialize_reference(c, ref, expr);
		}
		else
		{
			BEValue val;
			AlignSize alignment = type_alloca_alignment(ref->type);
			LLVMValueRef temp = llvm_emit_alloca(c, llvm_get_type(c, ref->type), alignment, ".assign_list");
			llvm_value_set_address(&val, temp, ref->type, alignment);
			llvm_emit_initialize_reference(c, &val, expr);
			llvm_store(c, ref, &val);
		}
		value = *ref;
	}
	else
	{
		if (expr->expr_kind == EXPR_CALL)
		{
			llvm_emit_call_expr(c, &value, expr, ref);
		}
		else
		{
			llvm_emit_expr(c, &value, expr);
		}

		if (value.type != type_void) llvm_store(c, ref, &value);
	}

	if (optional)
	{
		llvm_store_to_ptr_raw(c, optional, llvm_get_zero(c, type_anyfault), type_anyfault);
	}
	POP_CATCH();

	if (assign_block)
	{
		llvm_emit_br(c, assign_block);
		if (rejump_block)
		{
			llvm_emit_block(c, rejump_block);
			LLVMValueRef error = llvm_load_abi_alignment(c, type_anyfault, optional, "reload_err");
			llvm_store_to_ptr_raw(c, c->catch.fault, error, type_anyfault);
			llvm_emit_br(c, c->catch.block);
		}
		llvm_emit_block(c, assign_block);
	}

	return value;
}

static void llvm_convert_vector_comparison(GenContext *c, BEValue *be_value, LLVMValueRef val, Type *vector_type,
										   bool is_equals)
{
	unsigned bits = vector_type->array.len;
	LLVMTypeRef llvm_type = LLVMTypeOf(val);
	if (bits <= 64)
	{
	}
	unsigned intrinsic = is_equals ? intrinsic_id.vector_reduce_and : intrinsic_id.vector_reduce_or;
	LLVMValueRef result = llvm_emit_call_intrinsic(c, intrinsic, &llvm_type, 1, &val, 1);
	llvm_value_set(be_value, result, type_bool);
}

static LLVMValueRef llvm_emit_coerce_alignment(GenContext *c, BEValue *be_value, LLVMTypeRef coerce_type, AlignSize target_alignment, AlignSize *resulting_alignment)
{
	// If we are loading something with greater alignment than what we have, we cannot directly memcpy.
	if (!llvm_value_is_addr(be_value) || be_value->alignment < target_alignment)
	{
		// COERCE UPDATE bitcast removed, check for ways to optimize
		LLVMValueRef target = llvm_emit_alloca(c, llvm_get_type(c, be_value->type), target_alignment, "coerce");
		llvm_store_to_ptr_aligned(c, target, be_value, target_alignment);
		*resulting_alignment = target_alignment;
		return target;
	}
	*resulting_alignment = be_value->alignment;
	return be_value->value;
}

/**
 * Emit a two value aggregate { value1, value2 }. If the values are constant, emit this
 * as a constant, otherwise generate two inserts.
 *
 * @param c the context
 * @param type the type of the aggregate
 * @param value1 the first value
 * @param value2 the second value
 * @return the resulting aggregate
 */
LLVMValueRef llvm_emit_aggregate_two(GenContext *c, Type *type, LLVMValueRef value1, LLVMValueRef value2)
{
	bool is_constant = llvm_is_const(value1) && llvm_is_const(value2);
	if (is_constant)
	{
		LLVMValueRef two[2] = { value1, value2 };
		return llvm_get_struct_of_type(c, type, two, 2);
	}
	LLVMValueRef result = llvm_get_undef(c, type);
	result = llvm_emit_insert_value(c, result, value1, 0);
	return llvm_emit_insert_value(c, result, value2, 1);
}

/**
 * Return a value as an aggregate,
 * @param c the context
 * @param value the BEValue to set.
 * @param type the type of the aggregate
 * @param value1 the first value
 * @param value2 the second value
 * @return the resulting aggregate
 */
void llvm_value_aggregate_two(GenContext *c, BEValue *value, Type *type, LLVMValueRef value1, LLVMValueRef value2)
{
	llvm_value_set(value, llvm_emit_aggregate_two(c, type, value1, value2), type);
}

static inline LLVMValueRef llvm_const_low_bitmask(GenContext *c, LLVMTypeRef type, int type_bits, int low_bits)
{
	if (low_bits < 1) return llvm_get_zero_raw(type);
	if (type_bits <= low_bits) return llvm_get_ones_raw(type);
	return llvm_emit_lshr_fixed(c, llvm_get_ones_raw(type), (type_bits - low_bits));
}

static inline LLVMValueRef llvm_const_high_bitmask(GenContext *c, LLVMTypeRef type, int type_bits, int high_bits)
{
	if (high_bits < 1) return llvm_get_zero_raw(type);
	if (type_bits <= high_bits) return llvm_get_ones_raw(type);
	return LLVMBuildNot(c->builder, llvm_emit_lshr_fixed(c, llvm_get_ones_raw(type), high_bits), "");
}

/**
 * Given an integer value, return the n lowest bits. This function is
 * valid even for low_bits == 0.
 *
 * @param c the context
 * @param value the value to mask
 * @param low_bits the number of bits to retain
 * @return the resulting masked value.
 */
static inline LLVMValueRef llvm_mask_low_bits(GenContext *c, LLVMValueRef value, unsigned low_bits)
{
	LLVMTypeRef type = LLVMTypeOf(value);
	if (low_bits < 1) return llvm_get_zero_raw(type);
	BitSize type_bits = llvm_bitsize(c, type);
	if (type_bits <= low_bits) return value;
	LLVMValueRef mask = llvm_emit_lshr_fixed(c, llvm_get_ones_raw(type), type_bits - low_bits);
	return llvm_emit_and_raw(c, mask, value);
}

/**
 * Return the desired padding type for n number of bytes, returning an i8 for size = 1
 * otherwise returning [i8 x size] for
 * larger padding. The size must be at least 1.
 *
 * @param c the context
 * @param size the size of the padding 1 or higher
 * @return the resulting padding type
 */
LLVMTypeRef llvm_const_padding_type(GenContext *c, AlignSize size)
{
	assert(size > 0);
	if (size == 1) return c->byte_type;
	return LLVMArrayType(c->byte_type, (unsigned)size);
}

/**
 * Return an undefined constant with a given padding.
 *
 * @param c the context
 * @param size the size of the padding, must be 1 or more.
 * @return the resulting padding.
 */
LLVMValueRef llvm_emit_const_padding(GenContext *c, AlignSize size)
{
	return llvm_get_undef_raw(llvm_const_padding_type(c, size));
}

static inline LLVMValueRef llvm_emit_add_int(GenContext *c, Type *type, LLVMValueRef left, LLVMValueRef right, SourceSpan loc)
{
	if (compiler.build.feature.trap_on_wrap && type->type_kind != TYPE_VECTOR)
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
		LLVMValueRef result = llvm_emit_extract_value(c, add_res, 0);
		LLVMValueRef ok = llvm_emit_extract_value(c, add_res, 1);
		llvm_emit_panic_on_true(c, ok, "Addition overflow", loc, NULL, NULL, NULL);
		return result;
	}

	return LLVMBuildAdd(c->builder, left, right, "add");
}

/**
 * Recursively find the largest, non-trivial inner type that is the dest_size or smaller.
 *
 * @param c context
 * @param type the LLVM type to step into
 * @param dest_size the min size
 * @return the type containing this inner type.
 */
static LLVMTypeRef llvm_find_inner_struct_type_for_coerce(GenContext *c, LLVMTypeRef type, ByteSize dest_size)
{
	ByteSize container_size = llvm_store_size(c, type);
	while (1)
	{
		if (LLVMGetTypeKind(type) != LLVMStructTypeKind) break;
		// This should strictly speaking never happen because we don't have zero size elements.
		if (!LLVMCountStructElementTypes(type)) break;

		LLVMTypeRef first_element = LLVMStructGetTypeAtIndex(type, 0);
		ByteSize first_element_size = llvm_store_size(c, first_element);
		// If the size is smaller than the desired size, the previous type is what we wanted.
		// then we're done if this type is actually smaller than our previous.
		// The reason for the second check is to avoid the case when one isn't stepping into the sub
		// structs, e.g. struct { struct { int } }. Here, even if dest_size > 4, we want struct { int }.
		if (first_element_size < dest_size && first_element_size < container_size) break;
		type = first_element;
		container_size = first_element_size;
	}
	return type;
}

LLVMTypeRef llvm_coerce_expand_hi_offset(GenContext *c, LLVMValueRef *addr, ABIArgInfo *info, AlignSize *align)
{
	LLVMTypeRef type2 = llvm_get_type(c, info->coerce_expand.hi);
	if (info->coerce_expand.packed)
	{
		*align = type_min_alignment(*align, *align + info->coerce_expand.offset_hi);
		llvm_emit_const_ptradd_inbounds_raw(c, *addr, info->coerce_expand.offset_hi);
		return type2;
	}
	*align = type_min_alignment(*align, *align + llvm_store_size(c, type2) * info->coerce_expand.offset_hi);
	llvm_emit_const_ptradd_inbounds_raw(c, *addr, type_size(info->coerce_expand.hi) * info->coerce_expand.offset_hi);
	return type2;
}
/**
 * General functionality to convert ptr <-> int
 */
LLVMValueRef llvm_coerce_int_ptr(GenContext *c, LLVMValueRef value, LLVMTypeRef from, LLVMTypeRef to)
{
	// 1. Are they the same?
	if (from == to) return value;

	// 2. If the source is a pointer, then.
	bool to_is_pointer = LLVMGetTypeKind(to) == LLVMPointerTypeKind;
	if (LLVMGetTypeKind(from) == LLVMPointerTypeKind)
	{
		assert(!to_is_pointer && "ptr<->ptr should never happen in LLVM 15+");
		from = llvm_get_type(c, type_iptr);
		value = LLVMBuildPtrToInt(c->builder, value, from, "");
	}

	// 3. Find the "to" int type to convert to.
	LLVMTypeRef to_int_type = to_is_pointer ? llvm_get_type(c, type_iptr) : to;

	// 4. Are int types not matching?
	if (to_int_type != from)
	{
		if (compiler.platform.big_endian)
		{
			// Big endian, preserve the high bits.
			ByteSize to_size = llvm_abi_size(c, to_int_type);
			ByteSize from_size = llvm_abi_size(c, from);
			if (from_size > to_size)
			{
				value = llvm_emit_lshr_fixed(c, value, (from_size - to_size) * 8);
				value = LLVMBuildTrunc(c->builder, value, to_int_type, "");
			}
			else
			{
				value = LLVMBuildZExt(c->builder, value, to_int_type, "");
				value = llvm_emit_shl_fixed(c, value, (to_size - from_size) * 8);
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
	LLVMTypeRef llvm_source_type = llvm_get_type(c, value->type);

	// 1. If the types match then we're done, just load.
	if (llvm_source_type == coerced)
	{
		return llvm_load_value(c, value);
	}

	// 2. Both are integer types and values, then just truncate / extend
	if (!llvm_value_is_addr(value)
		&& LLVMGetTypeKind(coerced) == LLVMIntegerTypeKind
		&& LLVMGetTypeKind(llvm_source_type) == LLVMIntegerTypeKind)
	{
		return llvm_zext_trunc(c, value->value, coerced);
	}

	// 2. From now on we need th address.
	llvm_value_addr(c, value);
	LLVMValueRef addr = value->value;

	ByteSize target_size = llvm_alloc_size(c, coerced);

	// 3. If this is a struct, we index into it.
	if (LLVMGetTypeKind(llvm_source_type) == LLVMStructTypeKind)
	{
		llvm_source_type = llvm_find_inner_struct_type_for_coerce(c, llvm_source_type, target_size);
	}
	// --> from now on we only use LLVM types.

	ByteSize source_size = llvm_alloc_size(c, llvm_source_type);

	LLVMTypeKind source_type_kind = LLVMGetTypeKind(llvm_source_type);
	LLVMTypeKind coerced_type_kind = LLVMGetTypeKind(coerced);

	if ((coerced_type_kind == LLVMPointerTypeKind || coerced_type_kind == LLVMIntegerTypeKind)
		&& (source_type_kind == LLVMPointerTypeKind || source_type_kind == LLVMIntegerTypeKind))
	{
		LLVMValueRef val = llvm_load(c, llvm_source_type, addr, value->alignment, "");
		return llvm_coerce_int_ptr(c, val, llvm_source_type, coerced);
	}

	if (source_size >= target_size && source_type_kind != LLVMScalableVectorTypeKind && coerced_type_kind != LLVMScalableVectorTypeKind)
	{
		// COERCE UPDATE bitcast removed, check for ways to optimize
		return llvm_load(c, coerced, addr, value->alignment, "");
	}

	if (coerced_type_kind == LLVMScalableVectorTypeKind)
	{
		UNSUPPORTED;
	}

	// Otherwise, do it through memory.
	AlignSize max_align = type_max_alignment(value->alignment, llvm_abi_alignment(c, coerced));
	LLVMValueRef temp = llvm_emit_alloca(c, coerced, max_align, "tempcoerce");
	llvm_emit_memcpy(c, temp, max_align, addr, value->alignment, source_size);
	return llvm_load(c, coerced, temp, max_align, "");
}


void llvm_emit_coerce_store(GenContext *c, LLVMValueRef addr, AlignSize alignment, LLVMTypeRef coerced, LLVMValueRef value, LLVMTypeRef target_type)
{

	// 1. Simplest case, the underlying types match.
	if (coerced == target_type)
	{
		llvm_store_to_ptr_raw_aligned(c, addr, value, alignment);
		return;
	}

	ByteSize src_size = llvm_alloc_size(c, coerced);

	// 3. Enter into a struct in case the result is a struct.
	if (LLVMGetTypeKind(target_type) == LLVMStructTypeKind)
	{
		target_type = llvm_find_inner_struct_type_for_coerce(c, target_type, src_size);
	}

	// 4. If we are going from int/ptr <-> ptr/int
	LLVMTypeKind source_type_kind = LLVMGetTypeKind(target_type);
	LLVMTypeKind coerced_type_kind = LLVMGetTypeKind(coerced);
	if ((coerced_type_kind == LLVMPointerTypeKind || coerced_type_kind == LLVMIntegerTypeKind)
		&& (source_type_kind == LLVMPointerTypeKind || source_type_kind == LLVMIntegerTypeKind))
	{
		value = llvm_coerce_int_ptr(c, value, coerced, target_type);
		llvm_store_to_ptr_raw_aligned(c, addr, value, alignment);
		return;
	}

	assert(coerced_type_kind != LLVMScalableVectorTypeKind && "Scalable vectors are not supported.");

	ByteSize target_size = llvm_alloc_size(c, target_type);
	if (src_size <= target_size && coerced_type_kind != LLVMScalableVectorTypeKind && source_type_kind != LLVMScalableVectorTypeKind)
	{
		// COERCE UPDATE bitcast removed, check for ways to optimize
		llvm_store_to_ptr_raw_aligned(c, addr, value, alignment);
		return;
	}

	// Otherwise, do it through memory.
	AlignSize coerce_align = llvm_abi_alignment(c, coerced);
	LLVMValueRef temp = llvm_emit_alloca(c, coerced, coerce_align, "tempcoerce");
	llvm_store_to_ptr_raw_aligned(c, temp, value, coerce_align);
	llvm_emit_memcpy(c, addr, alignment, temp, coerce_align, target_size);
}

void llvm_emit_convert_value_from_coerced(GenContext *c, BEValue *result, LLVMTypeRef coerced, LLVMValueRef value, Type *original_type)
{
	LLVMTypeRef target_type = llvm_get_type(c, original_type);
	LLVMValueRef addr = llvm_emit_alloca(c, target_type, type_abi_alignment(original_type), "result");
	llvm_emit_coerce_store(c, addr, type_abi_alignment(original_type), coerced, value, target_type);
	llvm_value_set_address_abi_aligned(result, addr, original_type);
}

static inline LLVMValueRef llvm_emit_sub_int(GenContext *c, Type *type, LLVMValueRef left, LLVMValueRef right, SourceSpan loc)
{
	if (compiler.build.feature.trap_on_wrap && type->type_kind != TYPE_VECTOR)
	{
		LLVMTypeRef type_to_use = llvm_get_type(c, type);
		LLVMValueRef args[2] = { left, right };
		assert(type_lowering(type) == type);
		LLVMValueRef add_res;
		if (type_is_unsigned(type))
		{
			add_res = llvm_emit_call_intrinsic(c, intrinsic_id.usub_overflow, &type_to_use, 1, args, 2);
		}
		else
		{
			add_res = llvm_emit_call_intrinsic(c, intrinsic_id.ssub_overflow, &type_to_use, 1, args, 2);
		}
		LLVMValueRef result = llvm_emit_extract_value(c, add_res, 0);
		LLVMValueRef ok = llvm_emit_extract_value(c, add_res, 1);
		llvm_emit_panic_on_true(c, ok, "Subtraction overflow", loc, NULL, NULL, NULL);
		return result;
	}

	return LLVMBuildSub(c->builder, left, right, "sub");
}


static void llvm_emit_array_bounds_check(GenContext *c, BEValue *index, LLVMValueRef array_max_index, SourceSpan loc)
{
	BEValue result;
	llvm_value_rvalue(c, index);

	// Negative values are not allowed.
	if (type_is_signed(index->type))
	{
		llvm_emit_int_comp_raw(c, &result, index->type, index->type, index->value,
		                       llvm_get_zero(c, index->type), BINARYOP_LT);
		llvm_emit_panic_if_true(c, &result, "Negative array indexing", loc, "Negative array indexing (index was %d)", index, NULL);
	}

	llvm_emit_int_comp_raw(c, &result, index->type, index->type,
	                       index->value, array_max_index,
	                       BINARYOP_GE);
	BEValue max;
	llvm_value_set(&max, array_max_index, index->type);
	llvm_emit_panic_if_true(c, &result, "Array index out of bounds", loc, "Array index out of bounds (array had size %d, index was %d)", &max, index);
}

static inline void llvm_emit_subscript_addr_with_base(GenContext *c, BEValue *result, BEValue *parent, BEValue *index, SourceSpan loc)
{
	assert(llvm_value_is_addr(parent));
	Type *type = type_lowering(parent->type);
	switch (type->type_kind)
	{
		case TYPE_POINTER:
			llvm_value_set_address_abi_aligned(result, llvm_emit_pointer_inbounds_gep_raw(
					c,
					llvm_get_pointee_type(c, parent->type),
					parent->value,
					index->value), type->pointer);
			return;
		case TYPE_ARRAY:
		case TYPE_FLEXIBLE_ARRAY:
		case TYPE_VECTOR:
		{
			AlignSize alignment;
			LLVMValueRef ptr = llvm_emit_array_gep_raw_index(c, parent->value, llvm_get_type(c, type), index, parent->alignment, &alignment);
			llvm_value_set_address(result, ptr, type->array.base, alignment);
			return;
		}
		case TYPE_SLICE:
			{
				LLVMValueRef ptr = llvm_emit_pointer_inbounds_gep_raw(c, llvm_get_type(c, type->array.base), parent->value, index->value);
				llvm_value_set_address(result, ptr, type->array.base, type_abi_alignment(type->array.base));
			}
			return;
		default:
			UNREACHABLE

	}
}

static inline void llvm_emit_vector_subscript(GenContext *c, BEValue *value, Expr *expr)
{
	llvm_emit_exprid(c, value, expr->subscript_expr.expr);
	llvm_value_rvalue(c, value);
	Type *vec = value->type;
	assert(vec->type_kind == TYPE_VECTOR);
	Type *element = vec->array.base;
	LLVMValueRef vector = value->value;
	llvm_emit_exprid(c, value, expr->subscript_expr.index.expr);
	llvm_value_rvalue(c, value);
	LLVMValueRef index = value->value;
	if (expr->subscript_expr.index.start_from_end)
	{
		index = LLVMBuildNUWSub(c->builder, llvm_const_int(c, value->type, vec->array.len), index, "");
	}
	llvm_value_set(value, LLVMBuildExtractElement(c->builder, vector, index, ""), element);
}


/**
 * Expand &foo[123] or &someCall()[n] or some such.
 * Evaluation order is left to right.
 */
static inline void llvm_emit_subscript_addr(GenContext *c, BEValue *value, Expr *expr)
{
	Expr *parent_expr = exprptr(expr->subscript_expr.expr);
	Expr *index_expr = exprptr(expr->subscript_expr.index.expr);
	Type *parent_type = type_lowering(parent_expr->type);
	BEValue ref;
	// First, get thing being subscripted.
	llvm_emit_expr(c, value, parent_expr);
	BEValue len = { .value = NULL };
	TypeKind parent_type_kind = parent_type->type_kind;

	// See if we need the length.
	bool needs_len = false;
	bool start_from_end = expr->subscript_expr.index.start_from_end;
	if (parent_type_kind == TYPE_SLICE)
	{
		needs_len = safe_mode_enabled() || start_from_end;
	}
	else if (parent_type_kind == TYPE_ARRAY)
	{
		// From back should always be folded.
		assert(!expr_is_const(expr) || !start_from_end);
		needs_len = (safe_mode_enabled() && !expr_is_const(expr)) || start_from_end;
	}
	if (needs_len)
	{
		llvm_emit_len_for_expr(c, &len, value);
		llvm_value_rvalue(c, &len);
	}

	llvm_emit_ptr_from_array(c, value);
	llvm_value_addr(c, value);
	// Now calculate the index:
	BEValue index;
	llvm_emit_expr(c, &index, index_expr);
	// It needs to be an rvalue.
	llvm_value_rvalue(c, &index);

	if (start_from_end)
	{
		assert(needs_len);
		index.value = LLVMBuildNUWSub(c->builder, llvm_zext_trunc(c, len.value, llvm_get_type(c, index.type)), index.value, "");
	}
	if (needs_len && safe_mode_enabled() && !llvm_is_global_eval(c))
	{
		llvm_emit_array_bounds_check(c, &index, len.value, index_expr->span);
	}
	llvm_emit_subscript_addr_with_base(c, value, value, &index, index_expr->span);
	assert(llvm_value_is_addr(value));
	llvm_value_fold_optional(c, value);
}

/**
 * Expand foo[123] or someCall()[n] or some such.
 * Evaluation order is left to right.
 */
static inline void llvm_emit_subscript(GenContext *c, BEValue *value, Expr *expr)
{
	Expr *parent_expr = exprptr(expr->subscript_expr.expr);
	Type *parent_type = type_lowering(parent_expr->type);
	if (parent_type->type_kind == TYPE_VECTOR)
	{
		llvm_emit_vector_subscript(c, value, expr);
		return;
	}
	llvm_emit_subscript_addr(c, value, expr);
}

static inline void llvm_emit_pointer_offset(GenContext *c, BEValue *value, Expr *expr)
{
	Expr *pointer = exprptr(expr->pointer_offset_expr.ptr);
	Expr *offset_expr = exprptr(expr->pointer_offset_expr.offset);

	// Emit the pointer
	llvm_emit_expr(c, value, pointer);
	llvm_value_rvalue(c, value);

	// Now calculate the offset:
	BEValue offset;
	llvm_emit_expr(c, &offset, offset_expr);
	llvm_value_rvalue(c, &offset);

	LLVMTypeRef element_type;
	ArraySize vec_len = pointer->type->type_kind == TYPE_VECTOR ? pointer->type->array.len : 0;
	if (expr->pointer_offset_expr.raw_offset)
	{
		element_type = vec_len ? LLVMVectorType(c->byte_type, vec_len) : c->byte_type;
	}
	else
	{
		element_type = llvm_get_pointee_type(c, vec_len ? pointer->type->array.base : pointer->type);
	}
	value->value = llvm_emit_pointer_gep_raw(c, element_type, value->value, offset.value);
}


static ArrayIndex find_member_index(Decl *parent, Decl *member)
{
	FOREACH_IDX(i, Decl *, maybe_member, parent->strukt.members)
	{
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

static void llvm_emit_member_addr(GenContext *c, BEValue *value, Decl *parent, Decl *member)
{
	assert(member->resolve_status == RESOLVE_DONE);
	Decl *found = NULL;
	do
	{
		ArrayIndex index = find_member_index(parent, member);
		assert(index > -1);
		found = parent->strukt.members[index];
		switch (parent->type->canonical->type_kind)
		{
			case TYPE_UNION:
				llvm_value_addr(c, value);
				llvm_value_bitcast(c, value, found->type);
				break;
			case TYPE_STRUCT:
				llvm_value_addr(c, value);
				llvm_value_struct_gep(c, value, value, (unsigned)index);
				break;
			default:
				UNREACHABLE
		}
		parent = found;
	} while (found != member);
}

static Decl *llvm_emit_bitstruct_member(GenContext *c, BEValue *value, Decl *parent, Decl *member)
{
	assert(member->resolve_status == RESOLVE_DONE);
	Decl *found = parent;
	Decl *last = NULL;
	do
	{
		ArrayIndex index = find_member_index(parent, member);
		assert(index > -1);
		last = found;
		found = parent->strukt.members[index];
		switch (parent->type->canonical->type_kind)
		{
			case TYPE_UNION:
				llvm_value_addr(c, value);
				llvm_value_bitcast(c, value, found->type);
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
	return last ? last : parent;
}

static LLVMValueRef llvm_emit_bswap(GenContext *c, LLVMValueRef value)
{
	if (llvm_is_const(value))
	{
		return LLVMConstBswap(value);
	}
	LLVMTypeRef type = LLVMTypeOf(value);
	assert(type != c->byte_type);
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
	LLVMValueRef element = llvm_load(c, array_type, byte_ptr, alignment, "");
	// Shift the bit to the zero position.
	element = llvm_emit_lshr_fixed(c, element, start_bit % 8);
	// Truncate to i1.
	element = LLVMBuildTrunc(c->builder, element, c->bool_type, "");
	// Done!
	llvm_value_set(be_value, element, type_bool);
}

static inline LLVMValueRef llvm_bswap_non_integral(GenContext *c, LLVMValueRef value, unsigned bitsize)
{
	if (bitsize <= 8) return value;
	assert(is_power_of_two(bitsize));
	LLVMValueRef shifted = llvm_emit_shl_fixed(c, value, (int)llvm_bitsize(c, LLVMTypeOf(value)) - (int)bitsize);
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
	bool bswap = bitstruct_requires_bitswap(parent_decl);
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
		LLVMValueRef element = llvm_load(c, array_type, byte_ptr, alignment, "");
		element = llvm_zext_trunc(c, element, llvm_member_type);
		int current_offset = 8 * (i - start_byte) - offset;
		if (current_offset < 0)
		{
			element = llvm_emit_lshr_fixed(c, element, -current_offset);
		}
		else if (current_offset > 0)
		{
			element = llvm_emit_shl_fixed(c, element, current_offset);
		}
		if (res == NULL)
		{
			res = element;
			continue;
		}

		res = llvm_emit_or_raw(c, element, res);
	}
	if (bswap)
	{
		res = llvm_bswap_non_integral(c, res, end - start + 1);
	}
	if (type_is_signed(member_type))
	{
		TypeSize top_bits_to_clear = bitsize - end + start - 1;
		if (top_bits_to_clear)
		{
			LLVMValueRef shift = LLVMConstInt(llvm_member_type, top_bits_to_clear, false);
			res = llvm_emit_shl(c, res, shift);
			res = llvm_emit_ashr(c, res, shift);
		}
	}
	else
	{
		res = llvm_mask_low_bits(c, res, end - start + 1);
	}
	llvm_value_set(be_value, res, member_type);
}


static inline void llvm_extract_bitvalue(GenContext *c, BEValue *be_value, Decl *parent_decl, Decl *member)
{
	if (be_value->type->type_kind == TYPE_ARRAY)
	{
		llvm_extract_bitvalue_from_array(c, be_value, member, parent_decl);
		return;
	}
	LLVMValueRef value = llvm_load_value_store(c, be_value);
	if (bitstruct_requires_bitswap(parent_decl)) value = llvm_emit_bswap(c, value);
	BitSize container_size = type_size(be_value->type);
	BitSize container_bit_size = container_size * 8;
	unsigned start = (unsigned)member->var.start_bit;
	unsigned end = (unsigned)member->var.end_bit;
	Type *member_type = type_lowering(member->type);
	if (type_is_signed(member_type))
	{
		// Shift all the way left, so top bit is to the top.
		uint64_t left_shift = container_bit_size - end - 1;
		if (left_shift)
		{
			value = llvm_emit_shl_fixed(c, value, left_shift);
		}
		uint64_t right_shift = left_shift + start;
		if (right_shift)
		{
			value = llvm_emit_ashr_fixed(c, value, right_shift);
		}
		value = llvm_sext_trunc(c, value, llvm_get_type(c, member_type));
	}
	else
	{
		// Shift away bottom:
		if (start)
		{
			value = llvm_emit_lshr_fixed(c, value, start);
		}
		TypeSize bits_needed = end - start + 1;
		value = llvm_mask_low_bits(c, value, bits_needed);
		value = llvm_zext_trunc(c, value, llvm_get_type(c, member_type));
	}
	llvm_value_set(be_value, value, member_type);
}

static inline void llvm_emit_update_bitstruct_array(GenContext *c,
													LLVMValueRef array_ptr,
													AlignSize array_alignment,
													LLVMTypeRef array_type,
													bool need_bitswap,
													Decl *member,
													LLVMValueRef value)
{
	unsigned start_bit = member->var.start_bit;
	unsigned end_bit = member->var.end_bit;

	Type *member_type = type_flatten(member->type);
	if (member_type == type_bool)
	{
		assert(start_bit == end_bit);
		value = llvm_emit_shl_fixed(c, value, start_bit % 8);
		AlignSize alignment;
		LLVMValueRef byte_ptr = llvm_emit_array_gep_raw(c, array_ptr, array_type, start_bit / 8, array_alignment, &alignment);
		LLVMValueRef current = llvm_load(c, c->byte_type, byte_ptr, alignment, "");
		LLVMValueRef bit = llvm_emit_shl_fixed(c, LLVMConstInt(c->byte_type, 1, 0), start_bit % 8);
		current = llvm_emit_and_raw(c, current, LLVMBuildNot(c->builder, bit, ""));
		current = llvm_emit_or_raw(c, current, value);
		llvm_store_to_ptr_raw_aligned(c, byte_ptr, current, alignment);
		return;
	}

	unsigned bit_size = end_bit - start_bit + 1;
	if (need_bitswap)
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
														(unsigned)i, array_alignment, &alignment);
		if (i == start_byte && start_mod != 0)
		{
			int skipped_bits = start_mod;
			// Shift the lower bits into the top of the byte.
			LLVMValueRef res = llvm_emit_shl_fixed(c, value, skipped_bits);
			// Then truncate.
			if (member_type_bitsize > 8)
			{
				res = llvm_zext_trunc(c, res, c->byte_type);
			}
			// Create a mask for the lower bits.
			LLVMValueRef mask = llvm_const_low_bitmask(c, c->byte_type, 8, skipped_bits);

			// We might need to mask the top bits
			if (i == end_byte && end_mod != 7)
			{
				res = llvm_emit_and_raw(c, res, llvm_const_low_bitmask(c, c->byte_type, 8, end_mod + 1));
				mask = llvm_emit_or_raw(c, mask, llvm_const_high_bitmask(c, c->byte_type, 8, 7 - (int)end_bit));
			}
			// Load the current value.
			LLVMValueRef current = llvm_load(c, c->byte_type, byte_ptr, alignment, "");
			// Empty the top bits.
			current = llvm_emit_and_raw(c, current, mask);
			// Use *or* with the top bits from "res":
			current = llvm_emit_or_raw(c, current, res);
			// And store it back.
			llvm_store_to_ptr_raw_aligned(c, byte_ptr, current, alignment);
			// We now shift the value by the number of bits we used.
			value = llvm_emit_lshr_fixed(c, value, 8 - skipped_bits);
			// ... and we're done with the first byte.
			continue;
		}
		if (i == end_byte && end_mod != 7)
		{
			// What remains is end_mod + 1 bits to copy.
			value = llvm_zext_trunc(c, value, c->byte_type);
			// Create a mask for the lower bits.
			LLVMValueRef mask = llvm_const_low_bitmask(c, c->byte_type, 8, end_mod + 1);
			value = llvm_emit_and_raw(c, value, mask);
			// Load the current value.
			LLVMValueRef current = llvm_load(c, c->byte_type, byte_ptr, alignment, "");
			// Clear the lower bits.
			current = llvm_emit_and_raw(c, current, LLVMBuildNot(c->builder, mask, ""));
			// Use *or* with the bottom bits from "value":
			llvm_emit_or_raw(c, current, value);
			// And store it back.
			llvm_store_to_ptr_raw_aligned(c, byte_ptr, current, alignment);
			continue;
		}
		// All others are simple: truncate & store
		llvm_store_to_ptr_raw_aligned(c, byte_ptr, llvm_zext_trunc(c, value, c->byte_type), alignment);
		// Then shift
		value = llvm_emit_lshr_fixed(c, value, 8);
	}
}

static inline void llvm_emit_bitassign_array(GenContext *c, LLVMValueRef result, BEValue parent, Decl *parent_decl, Decl *member)
{
	llvm_value_addr(c, &parent);
	llvm_emit_update_bitstruct_array(c, parent.value, parent.alignment, llvm_get_type(c, parent.type),
									 bitstruct_requires_bitswap(parent_decl), member, result);
}

INLINE LLVMValueRef llvm_emit_bitstruct_value_update(GenContext *c, LLVMValueRef current_val, TypeSize bits, LLVMTypeRef bitstruct_type, Decl *member, LLVMValueRef val)
{
	// We now need to create a mask, a very naive algorithm:
	LLVMValueRef mask = llvm_get_ones_raw(bitstruct_type);
	int start_bit = (int)member->var.start_bit;
	int end_bit = (int)member->var.end_bit;
	// Let's say we want to create 00111000 => start: 3 end: 5
	int left_shift = (int)bits - end_bit - 1;
	mask = llvm_emit_shl_fixed(c, mask, left_shift);
	// => shift 2: 11111100
	mask = llvm_emit_lshr_fixed(c, mask, left_shift + start_bit);
	// => shift 5: 00000111
	mask = llvm_emit_shl_fixed(c, mask, start_bit);
	// => shift 3: 00111000

	val = llvm_zext_trunc(c, val, bitstruct_type);
	// Shift to the correct location.
	val = llvm_emit_shl_fixed(c, val, start_bit);
	// And combine using ((current_value & ~mask) | (value & mask))
	val = llvm_emit_and_raw(c, val, mask);
	current_val = llvm_emit_and_raw(c, current_val, LLVMBuildNot(c->builder, mask, ""));
	// Skip this op for LLVM14 if zero.
	current_val = llvm_emit_or_raw(c, current_val, val);
	return current_val;
}

static inline void llvm_emit_bitassign_expr(GenContext *c, BEValue *be_value, Expr *expr)
{
	Expr *lhs = exprptr(expr->binary_expr.left);
	Expr *parent_expr = lhs->access_expr.parent;

	// Grab the parent
	BEValue parent;
	Decl *member = lhs->access_expr.ref;
	llvm_emit_expr(c, &parent, parent_expr);
	Decl *parent_decl = llvm_emit_bitstruct_member(c, &parent, type_flatten(parent_expr->type)->decl, member);

	// If we have assign + op, load the current value, perform the operation.
	if (expr->binary_expr.operator != BINARYOP_ASSIGN)
	{
		// Grab the current value.
		BEValue value = parent;
		llvm_extract_bitvalue(c, &value, parent_decl, member);
		// Perform the operation and place it in be_value
		llvm_emit_binary(c, be_value, expr, &value, binaryop_assign_base_op(expr->binary_expr.operator));
	}
	else
	{
		// Otherwise just resolve the rhs and place it in be_value
		llvm_emit_expr(c, be_value, exprptr(expr->binary_expr.right));
	}

	Type *parent_type = type_flatten(parent_expr->type);
	if (type_lowering(parent_type)->type_kind == TYPE_ARRAY)
	{
		llvm_emit_bitassign_array(c, llvm_load_value_store(c, be_value), parent, parent_type->decl, member);
		return;
	}

	// To start the assign, pull out the current value.
	LLVMValueRef current_value = llvm_load_value_store(c, &parent);
	bool bswap = bitstruct_requires_bitswap(parent_decl);
	if (bswap) current_value = llvm_emit_bswap(c, current_value);
	LLVMValueRef value = llvm_load_value_store(c, be_value);
	current_value = llvm_emit_bitstruct_value_update(c, current_value, type_size(parent_decl->type) * 8, LLVMTypeOf(current_value), member, value);
	if (bswap) current_value = llvm_emit_bswap(c, current_value);
	llvm_store_raw(c, &parent, current_value);
}
static inline void llvm_emit_bitaccess(GenContext *c, BEValue *be_value, Expr *expr)
{
	Expr *parent = expr->access_expr.parent;
	llvm_emit_expr(c, be_value, parent);

	Decl *member = expr->access_expr.ref;
	assert(be_value && be_value->type);

	Decl *parent_decl = llvm_emit_bitstruct_member(c, be_value, type_flatten(parent->type)->decl, member);
	llvm_extract_bitvalue(c, be_value, parent_decl, expr->access_expr.ref);
}

static inline void llvm_emit_access_addr(GenContext *c, BEValue *be_value, Expr *expr)
{
	Expr *parent = expr->access_expr.parent;
	llvm_emit_expr(c, be_value, parent);
	Decl *member = expr->access_expr.ref;

	Type *flat_type = type_flatten(parent->type);
	if (flat_type->type_kind == TYPE_ENUM)
	{
		llvm_value_rvalue(c, be_value);
		if (!flat_type->decl->backend_ref) llvm_get_typeid(c, parent->type);
		assert(member->backend_ref);
		LLVMTypeRef value_type = llvm_get_type(c, type_get_array(member->type, vec_size(flat_type->decl->enums.values)));
		AlignSize align = LLVMGetAlignment(member->backend_ref);
		AlignSize alignment;
		LLVMValueRef ptr = llvm_emit_array_gep_raw_index(c, member->backend_ref, value_type, be_value, align, &alignment);
		llvm_value_set_address(be_value, ptr, member->type, alignment);
		return;
	}
	llvm_emit_member_addr(c, be_value, type_lowering(parent->type)->decl, member);
}

void llvm_set_phi(LLVMValueRef phi, LLVMValueRef val1, LLVMBasicBlockRef block1, LLVMValueRef val2, LLVMBasicBlockRef block2)
{
	LLVMValueRef vals[2] = { val1, val2 };
	LLVMBasicBlockRef blocks[2] = { block1, block2 };
	LLVMAddIncoming(phi, vals, blocks, 2);
}

void llvm_new_phi(GenContext *c, BEValue *value, const char *name, Type *type, LLVMValueRef val1, LLVMBasicBlockRef block1, LLVMValueRef val2, LLVMBasicBlockRef block2)
{
	LLVMValueRef phi = LLVMBuildPhi(c->builder, LLVMTypeOf(val1), name);
	llvm_set_phi(phi, val1, block1, val2, block2);
	llvm_value_set(value, phi, type);
}


static inline void llvm_emit_initialize_reference(GenContext *c, BEValue *value, Expr *expr);

/**
 * Here we are converting an array to a slice.
 * int[] x = &the_array;
 * @param c
 * @param value
 * @param to_type
 * @param from_type
 */
static void llvm_emit_arr_to_slice_cast(GenContext *c, BEValue *value, Type *to_type)
{
	ByteSize size = value->type->pointer->array.len;
	LLVMValueRef pointer;
	Type *array_type = value->type->pointer->array.base;
	if (size)
	{
		llvm_value_rvalue(c, value);
		pointer = value->value;
	}
	else
	{
		pointer = llvm_get_zero(c, type_get_ptr(array_type));
	}
	LLVMValueRef len = llvm_const_int(c, type_usz, size);
	llvm_value_aggregate_two(c, value, to_type, pointer, len);
}


void llvm_emit_vector_to_array_cast(GenContext *c, BEValue *value, Type *to_type, Type *from_type)
{
	llvm_value_rvalue(c, value);
	LLVMValueRef array = llvm_get_undef(c, to_type);
	for (unsigned i = 0; i < to_type->array.len; i++)
	{
		LLVMValueRef element = llvm_emit_extract_value(c, value->value, i);
		array = llvm_emit_insert_value(c, array, element, i);
	}
	llvm_value_set(value, array, to_type);
}

void llvm_emit_array_to_vector_cast(GenContext *c, BEValue *value, Type *to_type, Type *from_type)
{
	llvm_value_rvalue(c, value);
	LLVMValueRef vector = llvm_get_undef(c, to_type);
	for (unsigned i = 0; i < to_type->array.len; i++)
	{
		LLVMValueRef element = llvm_emit_extract_value(c, value->value, i);
		vector = llvm_emit_insert_value(c, vector, element, i);
	}
	llvm_value_set(value, vector, to_type);
}


void llvm_emit_slice_to_vec_array_cast(GenContext *c, BEValue *value, Type *to_type, Type *from_type)
{
	BEValue pointer;
	Type *base = type_lowering(from_type)->array.base;
	AlignSize element_alignment = type_abi_alignment(base);
	llvm_emit_slice_pointer(c, value, &pointer);
	llvm_value_rvalue(c, &pointer);
	LLVMTypeRef type = llvm_get_type(c, to_type);
	AlignSize alignment = llvm_abi_alignment(c, type);
	LLVMValueRef temp = llvm_emit_alloca(c, type, alignment, ".temp");
	llvm_emit_memcpy(c, temp, alignment, pointer.value, element_alignment, llvm_abi_size(c, type));
	llvm_value_set_address(value, temp, to_type, alignment);
}

void llvm_emit_expand_to_vec_cast(GenContext *c, BEValue *value, Type *to_type, Type *from_type)
{
	llvm_value_rvalue(c, value);
	LLVMTypeRef type = llvm_get_type(c, to_type);
	unsigned elements = LLVMGetVectorSize(type);
	LLVMValueRef res = LLVMGetUndef(type);
	for (unsigned i = 0; i < elements; i++)
	{
		res = LLVMBuildInsertElement(c->builder, res, value->value, llvm_const_int(c, type_usz, i), "");
	}
	llvm_value_set(value, res, to_type);
}

static void llvm_emit_bool_to_intvec_cast(GenContext *c, BEValue *value, Type *to_type, Type *from_type)
{
	llvm_value_rvalue(c, value);
	LLVMTypeRef type = llvm_get_type(c, to_type);
	LLVMValueRef res = LLVMBuildSExt(c->builder, value->value, type, "");
	llvm_value_set(value, res, to_type);
}

// Prune the common occurrence where the optional is not used.
static void llvm_prune_optional(GenContext *c, LLVMBasicBlockRef discard_fail)
{
	// Replace discard with the current block,
	// this removes the jump in the case:
	// br i1 %not_err, label %after_check, label %voiderr
	//after_check:
	// br label %voiderr
	//voiderr:
	// <insert point>
	LLVMValueRef block_value = LLVMBasicBlockAsValue(c->current_block);
	LLVMReplaceAllUsesWith(LLVMBasicBlockAsValue(discard_fail), block_value);

	// We now have:
	// br i1 %not_err, label %after_check, label %after_check
	//after_check:
	// <insert point>

	// Find the use of this block.
	LLVMUseRef use = LLVMGetFirstUse(block_value);
	if (!use) return;

	LLVMValueRef maybe_br = LLVMGetUser(use);
	// Expect a br instruction.
	if (!LLVMIsAInstruction(maybe_br) || LLVMGetInstructionOpcode(maybe_br) != LLVMBr) return;
	if (LLVMGetNumOperands(maybe_br) != 3) return;
	// We expect a single user.
	LLVMUseRef other_use = LLVMGetNextUse(use);
	while (other_use)
	{
		if (LLVMGetUser(other_use) != maybe_br) return;
		other_use = LLVMGetNextUse(other_use);
	}
	// Both operands same block value
	if (LLVMGetOperand(maybe_br, 1) != block_value || LLVMGetOperand(maybe_br, 2) != block_value) return;

	// Grab the compared value
	LLVMValueRef compared = LLVMGetOperand(maybe_br, 0);

	// Remove the block and the br
	LLVMBasicBlockRef prev_block = LLVMGetInstructionParent(maybe_br);
	LLVMRemoveBasicBlockFromParent(c->current_block);
	LLVMInstructionEraseFromParent(maybe_br);

	// Optionally remove the comparison
	if (!LLVMGetFirstUse(compared))
	{
		LLVMValueRef operand = NULL;
		if (LLVMGetInstructionOpcode(compared) == LLVMCall)
		{
			operand = LLVMGetOperand(compared, 0);
		}
		LLVMInstructionEraseFromParent(compared);
		if (operand) LLVMInstructionEraseFromParent(operand);
	}
	// Update the context
	c->current_block = prev_block;
	LLVMPositionBuilderAtEnd(c->builder, prev_block);
}

void llvm_emit_ignored_expr(GenContext *c, Expr *expr)
{
	BEValue value;
	// For a standalone catch, we can ignore storing the value.
	if (IS_OPTIONAL(expr))
	{
		LLVMBasicBlockRef discard_fail = llvm_basic_block_new(c, "voiderr");
		PUSH_CATCH_VAR_BLOCK(NULL, discard_fail);
		llvm_emit_expr(c, &value, expr);
		llvm_value_fold_optional(c, &value);
		EMIT_LOC(c, expr);
		// We only optimize if there is no instruction the current block
		if (!LLVMGetFirstInstruction(c->current_block))
		{
			llvm_prune_optional(c, discard_fail);
		}
		else
		{
			llvm_emit_br(c, discard_fail);
			llvm_emit_block(c, discard_fail);
		}
		POP_CATCH();
		return;
	}
	llvm_emit_expr(c, &value, expr);

}

static LLVMValueRef llvm_emit_char_array_zero(GenContext *c, BEValue *value, bool find_zero)
{
	llvm_value_addr(c, value);
	unsigned len = type_size(value->type);
	assert(len > 0);
	LLVMValueRef total = NULL;
	for (int i = 0; i < len; i++)
	{
		LLVMValueRef ref = llvm_emit_const_ptradd_inbounds_raw(c, value->value, i);
		LLVMValueRef val = llvm_zext_trunc(c, llvm_load(c, c->byte_type, ref, 1, ""), llvm_get_type(c, type_cint));
		total = total ? LLVMBuildAdd(c->builder, total, val, "") : val;
	}
	return LLVMBuildICmp(c->builder, find_zero ? LLVMIntEQ : LLVMIntNE, total, llvm_get_zero(c, type_cint), "");
}
static void llvm_emit_bitstruct_to_bool(GenContext *c, BEValue *value, Type *to_type, Type *from_type)
{
	Type *base_type = type_flatten(from_type->decl->strukt.container_type->type);
	if (base_type->type_kind != TYPE_ARRAY)
	{
		llvm_emit_int_comp_zero(c, value, value, BINARYOP_NE);
		return;
	}
	llvm_value_set(value, llvm_emit_char_array_zero(c, value, false), to_type);
}

void llvm_emit_cast(GenContext *c, CastKind cast_kind, Expr *expr, BEValue *value, Type *to_type, Type *from_type)
{
	Type *to_type_original = to_type;
	to_type = type_flatten(to_type);
	from_type = type_flatten(from_type);
	switch (cast_kind)
	{
		case CAST_BSBOOL:
			llvm_emit_bitstruct_to_bool(c, value, to_type, from_type);
			return;
		case CAST_SLARR:
			llvm_emit_slice_to_vec_array_cast(c, value, to_type, from_type);
			return;
		case CAST_EXPVEC:
			llvm_emit_expand_to_vec_cast(c, value, to_type, from_type);
			return;
		case CAST_BOOLVECINT:
			llvm_emit_bool_to_intvec_cast(c, value, to_type, from_type);
			return;
		case CAST_ARRVEC:
			llvm_emit_array_to_vector_cast(c, value, to_type, from_type);
			return;
		case CAST_PTRANY:
		{
			llvm_value_rvalue(c, value);
			BEValue typeid;
			llvm_emit_typeid(c, &typeid, from_type->pointer);
			llvm_value_aggregate_two(c, value, to_type, value->value, typeid.value);
			return;
		}
		case CAST_VOID:
			UNREACHABLE;
		case CAST_ERINT:
		case CAST_IDINT:
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
		case CAST_ANYBOOL:
			llvm_emit_any_pointer(c, value, value);
			llvm_value_rvalue(c, value);
			value->value = LLVMBuildIsNotNull(c->builder, value->value, "anybool");
			value->kind = BE_BOOLEAN;
			break;
		case CAST_ANYPTR:
			llvm_emit_any_pointer(c, value, value);
			break;
		case CAST_INTERR:
			to_type = type_lowering(to_type);
			llvm_value_rvalue(c, value);
			value->value = llvm_zext_trunc(c, value->value, llvm_get_type(c, to_type));
			break;
		case CAST_ERROR:
			UNREACHABLE
		case CAST_STRPTR:
		case CAST_PTRPTR:
			llvm_value_rvalue(c, value);
			break;
		case CAST_PTRINT:
			llvm_value_rvalue(c, value);
			value->value = LLVMBuildPtrToInt(c->builder, value->value, llvm_get_type(c, to_type), "ptrxi");
			break;
		case CAST_APTSA:
			llvm_emit_arr_to_slice_cast(c, value, to_type);
			break;
		case CAST_SAPTR:
			llvm_value_fold_optional(c, value);
			llvm_emit_slice_pointer(c, value, value);
			break;
		case CAST_EREU:
			// This is a no op.
			assert(type_lowering(to_type) == type_lowering(from_type));
			break;
		case CAST_VECARR:
			llvm_emit_vector_to_array_cast(c, value, to_type, from_type);
			break;
		case CAST_EUER:
			REMINDER("Improve fault to err comparison");
			break;
		case CAST_EUBOOL:
		case CAST_IDBOOL:
		{
			BEValue zero;
			llvm_value_set_int(c, &zero, type_iptr, 0);
			llvm_emit_int_comp(c, value, value, &zero, BINARYOP_NE);
			break;
		}
		case CAST_PTRBOOL:
			llvm_value_rvalue(c, value);
			value->value = LLVMBuildIsNotNull(c->builder, value->value, "ptrbool");
			value->kind = BE_BOOLEAN;
			break;
		case CAST_BSINTARR:
		case CAST_INTARRBS:
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
			value->kind = type_kind_is_any_vector(value->type->type_kind) ? BE_BOOLVECTOR : BE_BOOLEAN;
			break;
		case CAST_FPFP:
			llvm_value_rvalue(c, value);
			value->value = type_convert_will_trunc(to_type, from_type)
				   ? LLVMBuildFPTrunc(c->builder, value->value, llvm_get_type(c, to_type), "fpfptrunc")
				   : LLVMBuildFPExt(c->builder, value->value, llvm_get_type(c, to_type), "fpfpext");
			break;
		case CAST_FPINT:
			llvm_value_rvalue(c, value);
			if (type_is_signed(to_type))
			{
				value->value = LLVMBuildFPToSI(c->builder, value->value, llvm_get_type(c, to_type), "fpsi");
				break;
			}
			value->value = LLVMBuildFPToUI(c->builder, value->value, llvm_get_type(c, to_type), "fpui");
			break;
		case CAST_INTINT:
			llvm_value_ext_trunc(c, value, to_type);
			break;
		case CAST_INTFP:
			llvm_value_rvalue(c, value);
			if (type_is_signed(value->type))
			{
				value->value = LLVMBuildSIToFP(c->builder, value->value, llvm_get_type(c, to_type), "sifp");
				break;
			}
			value->value = LLVMBuildUIToFP(c->builder, value->value, llvm_get_type(c, to_type), "uifp");
			break;
		case CAST_IDPTR:
		case CAST_ERPTR:
		case CAST_INTPTR:
			llvm_value_rvalue(c, value);
			value->value = LLVMBuildIntToPtr(c->builder, value->value, llvm_get_type(c, to_type), "intptr");
			break;
		case CAST_SLSL:
			// Improve this
			break;
		case CAST_STINLINE:
			llvm_value_addr(c, value);
			value->type = to_type;
			return;
		case CAST_INTENUM:
			if (safe_mode_enabled() && c->builder != c->global_builder)
			{
				llvm_value_rvalue(c, value);
				BEValue check;
				Decl *decl = to_type_original->canonical->decl;
				unsigned max = vec_size(decl->enums.values);
				if (type_is_signed(value->type))
				{
					scratch_buffer_clear();
					scratch_buffer_printf("Attempt to convert a negative value (%%d) to enum '%s' failed.", decl->name);
					llvm_emit_int_comp_zero(c, &check, value, BINARYOP_LT);
					BEValue val;
					llvm_emit_panic_on_true(c, check.value, "Attempt to convert negative value to enum failed.", expr->span, scratch_buffer_copy(), value, NULL);
				}
				scratch_buffer_clear();
				scratch_buffer_printf("Attempting to convert %%d to enum '%s' failed as the value exceeds the max ordinal (%u).", decl->name, max - 1);
				LLVMValueRef val = llvm_const_int(c, value->type, max);
				llvm_emit_int_comp_raw(c, &check, value->type, value->type, value->value, val, BINARYOP_GE);
				llvm_emit_panic_on_true(c, check.value, "Failed integer to enum conversion", expr->span, scratch_buffer_copy(), value, NULL);
			}
			// We might need to extend or truncate.
			if (type_size(to_type) != type_size(from_type))
			{
				llvm_value_rvalue(c, value);
				llvm_value_set(value, llvm_zext_trunc(c, value->value, llvm_get_type(c, to_type)), to_type);
				return;
			}
			value->type = type_lowering(to_type);
			return;
		case CAST_SLBOOL:
			llvm_value_fold_optional(c, value);
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
				value->value = llvm_emit_extract_value(c, value->value, 1);
			}
			value->type = type_lowering(type_usz);
			llvm_value_rvalue(c, value);
			llvm_emit_int_comp_zero(c, value, value, BINARYOP_NE);
			break;
	}
	value->type = type_lowering(to_type);
}

static inline void llvm_emit_cast_expr(GenContext *context, BEValue *be_value, Expr *expr)
{
	if (expr->cast_expr.kind == CAST_VOID)
	{
		llvm_value_set(be_value, NULL, type_void);
		llvm_emit_ignored_expr(context, exprptr(expr->cast_expr.expr));
		return;
	}
	llvm_emit_exprid(context, be_value, expr->cast_expr.expr);
	llvm_emit_cast(context,
				   expr->cast_expr.kind,
				   expr,
				   be_value,
				   expr->type,
				   exprtype(expr->cast_expr.expr));
}


static LLVMValueRef llvm_recursive_set_value(GenContext *c, DesignatorElement **current_element_ptr, LLVMValueRef parent, DesignatorElement **last_element_ptr, Expr *value)
{
	DesignatorElement *current_element = current_element_ptr[0];
	if (current_element_ptr == last_element_ptr)
	{
		BEValue res;
		llvm_emit_expr(c, &res, value);
		ArraySize index = (ArraySize)current_element->index;
		LLVMValueRef val = llvm_load_value_store(c, &res);
		switch (current_element->kind)
		{
			case DESIGNATOR_FIELD:
			case DESIGNATOR_ARRAY:
				return llvm_emit_insert_value(c, parent, val, index);
			case DESIGNATOR_RANGE:
				for (ArrayIndex i = current_element->index; i <= current_element->index_end; i++)
				{
					parent = llvm_emit_insert_value(c, parent, val, i);
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
			current_val = llvm_emit_extract_value(c, parent, index);
			current_val = llvm_recursive_set_value(c, current_element_ptr + 1, current_val, last_element_ptr, value);
			return llvm_emit_insert_value(c, parent, current_val, index);
		}
		case DESIGNATOR_ARRAY:
			current_val = llvm_emit_extract_value(c, parent, current_element->index);
			current_val = llvm_recursive_set_value(c, current_element_ptr + 1, current_val, last_element_ptr, value);
			return llvm_emit_insert_value(c, parent, current_val, current_element->index);
		case DESIGNATOR_RANGE:
			for (ArrayIndex i = current_element->index; i <= current_element->index_end; i++)
			{
				current_val = llvm_emit_extract_value(c, parent, i);
				current_val = llvm_recursive_set_value(c, current_element_ptr + 1, current_val, last_element_ptr, value);
				parent = llvm_emit_insert_value(c, parent, current_val, i);
			}
			return parent;
		default:
			UNREACHABLE
	}
}


void llvm_emit_initialize_reference_temporary_const(GenContext *c, BEValue *ref, ConstInitializer *initializer)
{
	bool modified = false;
	// First create the constant value.

	LLVMValueRef value = llvm_emit_const_initializer(c, initializer);

	// Create a global const.
	AlignSize alignment = type_alloca_alignment(initializer->type);
	LLVMTypeRef type = LLVMTypeOf(value);
	LLVMValueRef global_copy = llvm_add_global_raw(c, ".__const", type, alignment);
	llvm_set_private_linkage(global_copy);
	LLVMSetUnnamedAddress(global_copy, LLVMGlobalUnnamedAddr);

	// Set the value and make it constant
	LLVMSetInitializer(global_copy, value);
	LLVMSetGlobalConstant(global_copy, true);

	// Ensure we have a reference.
	llvm_value_addr(c, ref);

	// Perform the memcpy.
	llvm_emit_memcpy(c, ref->value, ref->alignment, global_copy, alignment, type_size(initializer->type));
}

static inline void llvm_emit_const_initialize_bitstruct_ref(GenContext *c, BEValue *ref, ConstInitializer *initializer)
{
	if (initializer->kind == CONST_INIT_ZERO)
	{
		llvm_store_zero(c, ref);
		return;
	}
	assert(initializer->kind == CONST_INIT_STRUCT);
	llvm_store_raw(c, ref, llvm_emit_const_bitstruct(c, initializer));
}

static void llvm_emit_const_init_ref(GenContext *c, BEValue *ref, ConstInitializer *const_init, bool top)
{
	if (const_init->type->type_kind == TYPE_VECTOR)
	{
		LLVMValueRef val = llvm_emit_const_initializer(c, const_init);
		llvm_store_raw(c, ref, val);
		return;
	}
	if (const_init->type->type_kind == TYPE_BITSTRUCT)
	{
		llvm_emit_const_initialize_bitstruct_ref(c, ref, const_init);
		return;
	}
	if (const_init->kind == CONST_INIT_ZERO)
	{
		// In case of a zero, optimize.
		llvm_store_zero(c, ref);
		return;
	}
	// In case of small const initializers, or full arrays - use copy.
	if (const_init->kind == CONST_INIT_ARRAY_FULL || type_size(const_init->type) <= 32)
	{
		if (top && const_init_local_init_may_be_global(const_init))
		{
			llvm_emit_initialize_reference_temporary_const(c, ref, const_init);
			return;
		}
	}

	// Make sure we have an address.
	llvm_value_addr(c, ref);

	switch (const_init->kind)
	{
		case CONST_INIT_ZERO:
			if (type_is_builtin(ref->type->type_kind) || ref->type->type_kind == TYPE_ARRAY)
			{
				llvm_store_raw(c, ref, llvm_get_zero(c, ref->type));
				return;
			}
			llvm_store_zero(c, ref);
			return;
		case CONST_INIT_ARRAY_VALUE:
			UNREACHABLE
		case CONST_INIT_ARRAY_FULL:
		{
			LLVMValueRef array_ref = ref->value;
			Type *array_type = const_init->type;
			Type *element_type = array_type->array.base;
			ArrayIndex size = (ArrayIndex)array_type->array.len;
			LLVMTypeRef array_type_llvm = llvm_get_type(c, array_type);
			assert(size <= UINT32_MAX);
			for (ArrayIndex i = 0; i < size; i++)
			{
				AlignSize alignment;
				LLVMValueRef array_pointer = llvm_emit_array_gep_raw(c, array_ref, array_type_llvm, (unsigned)i, ref->alignment, &alignment);
				BEValue value;
				llvm_value_set_address(&value, array_pointer, element_type, alignment);
				llvm_emit_const_init_ref(c, &value, const_init->init_array_full[i], false);
			}
			return;
		}
		case CONST_INIT_ARRAY:
		{
			LLVMValueRef array_ref = ref->value;
			llvm_store_zero(c, ref);
			Type *array_type = const_init->type;
			Type *element_type = array_type->array.base;
			LLVMTypeRef array_type_llvm = llvm_get_type(c, array_type);
			ConstInitializer **elements = const_init->init_array.elements;
			ArrayIndex current_index = 0;
			LLVMValueRef *parts = NULL;
			FOREACH(ConstInitializer *, element, elements)
			{
				assert(element->kind == CONST_INIT_ARRAY_VALUE);
				ArrayIndex element_index = element->init_array_value.index;
				AlignSize alignment;
				LLVMValueRef array_pointer = llvm_emit_array_gep_raw(c, array_ref, array_type_llvm, (unsigned)element_index, ref->alignment, &alignment);
				BEValue value;
				llvm_value_set_address(&value, array_pointer, element_type, alignment);
				llvm_emit_const_init_ref(c, &value, element->init_array_value.element, false);
			}
			return;
		}
		case CONST_INIT_UNION:
		{
			Decl *decl = const_init->type->decl;
			ArrayIndex index = const_init->init_union.index;
			Type *type = decl->strukt.members[index]->type->canonical;
			// Bitcast.
			BEValue value = *ref;
			llvm_value_bitcast(c, &value, type);
//			llvm_value_set_address_abi_aligned(&value, llvm_emit_bitcast_ptr(c, ref->value, type), type);
			// Emit our value.
			llvm_emit_const_init_ref(c, &value, const_init->init_union.element, false);
			return;
		}
		case CONST_INIT_STRUCT:
		{
			Decl *decl = const_init->type->decl;
			assert(vec_size(decl->strukt.members) == vec_size(const_init->init_struct));
			FOREACH_IDX(i, ConstInitializer *, init, const_init->init_struct)
			{
				BEValue value;
				llvm_value_struct_gep(c, &value, ref, (unsigned)i);
				llvm_emit_const_init_ref(c, &value, init, false);
			}
			return;
		}
		case CONST_INIT_VALUE:
		{
			BEValue value;
			llvm_emit_expr(c, &value, const_init->init_value);
			llvm_store(c, ref, &value);
			return;
		}
	}
	UNREACHABLE
}

static inline void llvm_emit_initialize_reference_vector(GenContext *c, BEValue *ref, Type *real_type, Expr **elements)
{
	llvm_value_addr(c, ref);
	LLVMTypeRef llvm_type = llvm_get_type(c, real_type);
	LLVMValueRef vector_val = LLVMGetUndef(llvm_type);
	BEValue element_val;
	FOREACH_IDX(i, Expr *, element, elements)
	{
		llvm_emit_expr(c, &element_val, element);
		llvm_value_rvalue(c, &element_val);
		vector_val = LLVMBuildInsertElement(c->builder, vector_val, element_val.value, llvm_const_int(c, type_usz, i), "");
	}
	llvm_store_raw(c, ref, vector_val);
}

INLINE void llvm_emit_initialize_reference_bitstruct_array(GenContext *c, BEValue *ref, Decl *bitstruct, Expr** elements)
{
	LLVMTypeRef type = llvm_get_type(c, ref->type);
	bool is_bitswap = bitstruct_requires_bitswap(bitstruct);
	llvm_value_addr(c, ref);
	llvm_store_zero(c, ref);
	AlignSize alignment = ref->alignment;
	LLVMValueRef array_ptr = ref->value;
	// Now walk through the elements.
	FOREACH_IDX(i, Expr *, init, elements)
	{
		Decl *member = bitstruct->strukt.members[i];
		BEValue val;
		llvm_emit_expr(c, &val, init);
		llvm_emit_update_bitstruct_array(c, array_ptr, alignment, type, is_bitswap, member,
		                                 llvm_load_value_store(c, &val));
	}
}

static inline void llvm_emit_initialize_reference_bitstruct(GenContext *c, BEValue *ref, Decl *bitstruct, Expr** elements)
{
	Type *underlying_type = type_lowering(ref->type);
	if (underlying_type->type_kind == TYPE_ARRAY)
	{
		llvm_emit_initialize_reference_bitstruct_array(c, ref, bitstruct, elements);
		return;
	}
	LLVMTypeRef type = llvm_get_type(c, underlying_type);
	LLVMValueRef data = LLVMConstNull(type);
	TypeSize bits = type_bit_size(underlying_type);

	// Now walk through the elements.
	FOREACH_IDX(i, Expr *, init, elements)
	{
		Decl *member = bitstruct->strukt.members[i];
		BEValue val;
		llvm_emit_expr(c, &val, init);
		data = llvm_emit_bitstruct_value_update(c, data, bits, type, member, llvm_load_value_store(c, &val));
	}
	if (bitstruct_requires_bitswap(bitstruct))
	{
		data = llvm_emit_bswap(c, data);
	}
	llvm_store_raw(c, ref, data);
}

static inline void llvm_emit_initialize_reference_list(GenContext *c, BEValue *ref, Expr *expr)
{
	Type *type = type_flatten(expr->type);
	Expr **elements = expr->initializer_list;
	assert(type->type_kind != TYPE_SLICE);

	if (type->type_kind == TYPE_BITSTRUCT)
	{
		llvm_emit_initialize_reference_bitstruct(c, ref, type->decl, elements);
		return;
	}
	// Getting ready to initialize, get the real type.
	Type *real_type = type_lowering(ref->type);

	if (real_type->type_kind == TYPE_VECTOR)
	{
		llvm_emit_initialize_reference_vector(c, ref, real_type, elements);
		return;
	}

	// Make sure we have an address.
	llvm_value_addr(c, ref);
	LLVMValueRef value = ref->value;

	LLVMTypeRef llvm_type = llvm_get_type(c, real_type);
	bool is_struct = type_is_union_or_strukt(real_type);
	bool is_array = real_type->type_kind == TYPE_ARRAY;

	bool is_vector = real_type->type_kind == TYPE_VECTOR;
	// Now walk through the elements.
	FOREACH_IDX(i, Expr *, element, elements)
	{
		BEValue pointer;
		if (is_struct)
		{
			llvm_value_struct_gep(c, &pointer, ref, i);
		}
		else if (is_array)
		{
			REMINDER("Optimize array reference list init");
			AlignSize alignment;
			LLVMValueRef ptr = llvm_emit_array_gep_raw(c, value, llvm_type, i, ref->alignment, &alignment);
			llvm_value_set_address(&pointer, ptr, element->type, alignment);
		}
		else
		{
			llvm_value_set_address(&pointer, value, element->type, ref->alignment);
		}
		// If this is an initializer, we want to actually run the initialization recursively.
		if (expr_is_const_initializer(element))
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
		llvm_store(c, &pointer, &init_value);
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
	for (ArrayIndex i = curr->index; i <= curr->index_end; i++)
	{
		BEValue new_ref;
		AlignSize alignment;
		LLVMValueRef ptr = llvm_emit_array_gep_raw(c, ref->value, ref_type, (unsigned)i, ref->alignment, &alignment);
		llvm_value_set_address(&new_ref, ptr, type_get_indexed_type(ref->type), alignment);
		llvm_emit_initialize_designated_element(c, &new_ref, offset, current + 1, last, expr, emitted_value);
	}
}

static void llvm_emit_initialize_designated_element(GenContext *c, BEValue *ref, AlignSize offset, DesignatorElement** current,
                                                    DesignatorElement **last, Expr *expr, BEValue *emitted_value)
{
	BEValue value;
	if (current > last)
	{
		if (emitted_value)
		{
			llvm_store(c, ref, emitted_value);
			return;
		}
		if (expr_is_const_initializer(expr))
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
		llvm_store(c, ref, &val);
		return;
	}
	DesignatorElement *curr = current[0];
	switch (curr->kind)
	{
		case DESIGNATOR_FIELD:
		{
			Decl *decl = ref->type->canonical->decl->strukt.members[curr->index];
			offset += decl->offset;
			Type *type = type_flatten(decl->type);
			unsigned decl_alignment = decl->alignment;
			if (ref->type->type_kind == TYPE_UNION)
			{
				llvm_value_set_address(&value,
									   ref->value,
									   type,
									   type_min_alignment(offset, decl_alignment));
			}
			else
			{
				llvm_value_struct_gep(c, &value, ref, (unsigned) curr->index);
			}
			if (type->type_kind == TYPE_BITSTRUCT && last == current + 1)
			{
				assert(llvm_value_is_addr(&value));
				Decl *member = type->decl->strukt.members[last[0]->index];
				// Special handling of bitstructs.
				Type *underlying_type = value.type;
				assert(!emitted_value);
				BEValue exprval;
				llvm_emit_expr(c, &exprval, expr);
				LLVMValueRef val = llvm_load_value_store(c, &exprval);
				LLVMTypeRef bitstruct_type = llvm_get_type(c, underlying_type);
				bool is_bitswap = bitstruct_requires_bitswap(type->decl);
				if (underlying_type->type_kind == TYPE_ARRAY)
				{
					llvm_emit_update_bitstruct_array(c, value.value, value.alignment, bitstruct_type, is_bitswap, member, val);
					break;
				}
				LLVMValueRef current_val = llvm_load_value(c, &value);
				current_val = llvm_emit_bitstruct_value_update(c, current_val, type_bit_size(underlying_type), bitstruct_type, member, val);
				llvm_store_raw(c, &value, current_val);
				break;
			}
			llvm_emit_initialize_designated_element(c, &value, offset, current + 1, last, expr, emitted_value);
			break;
		}
		case DESIGNATOR_ARRAY:
		{
			Type *type = ref->type->array.base;
			offset += (unsigned)curr->index * type_size(type);
			AlignSize alignment;
			LLVMValueRef ptr = llvm_emit_array_gep_raw(c, ref->value, llvm_get_type(c, ref->type), (unsigned)curr->index, ref->alignment, &alignment);
			llvm_value_set_address(&value, ptr, type, alignment);
			llvm_emit_initialize_designated_element(c, &value, offset, current + 1, last, expr, emitted_value);
			break;
		}
		case DESIGNATOR_RANGE:
			llvm_emit_initialize_designated_const_range(c, ref, offset, current, last, expr, emitted_value);
			break;
		default:
			UNREACHABLE
	}
}

static inline void llvm_emit_initialize_reference_designated_bitstruct_array(GenContext *c, BEValue *ref, Decl *bitstruct, Expr **elements)
{
	LLVMTypeRef type = llvm_get_type(c, ref->type);
	bool is_bitswap = bitstruct_requires_bitswap(bitstruct);
	llvm_value_addr(c, ref);
	llvm_store_zero(c, ref);
	AlignSize alignment = ref->alignment;
	LLVMValueRef array_ptr = ref->value;
	// Now walk through the elements.
	FOREACH(Expr *, designator, elements)
	{
		assert(vec_size(designator->designator_expr.path) == 1);
		DesignatorElement *element = designator->designator_expr.path[0];
		assert(element->kind == DESIGNATOR_FIELD);
		Decl *member = bitstruct->strukt.members[element->index];
		BEValue val;
		llvm_emit_expr(c, &val, designator->designator_expr.value);
		llvm_emit_update_bitstruct_array(c, array_ptr, alignment, type, is_bitswap, member, llvm_load_value_store(c, &val));
	}
}

static inline void llvm_emit_initialize_reference_designated_bitstruct(GenContext *c, BEValue *ref, Decl *bitstruct, Expr **elements)
{
	Type *underlying_type = type_lowering(ref->type);
	if (underlying_type->type_kind == TYPE_ARRAY)
	{
		llvm_emit_initialize_reference_designated_bitstruct_array(c, ref, bitstruct, elements);
		return;
	}
	LLVMTypeRef type = llvm_get_type(c, underlying_type);
	LLVMValueRef data = LLVMConstNull(type);
	TypeSize bits = type_bit_size(underlying_type);

	// Now walk through the elements.
	FOREACH(Expr *, designator, elements)
	{
		assert(vec_size(designator->designator_expr.path) == 1);
		DesignatorElement *element = designator->designator_expr.path[0];
		assert(element->kind == DESIGNATOR_FIELD);
		Decl *member = bitstruct->strukt.members[element->index];
		BEValue val;
		llvm_emit_expr(c, &val, designator->designator_expr.value);
		data = llvm_emit_bitstruct_value_update(c, data, bits, type, member, llvm_load_value_store(c, &val));
	}
	if (bitstruct_requires_bitswap(bitstruct))
	{
		data = llvm_emit_bswap(c, data);
	}
	llvm_store_raw(c, ref, data);
}

static inline void llvm_emit_initialize_reference_designated(GenContext *c, BEValue *ref, Expr *expr)
{
	Expr **elements = expr->designated_init_list;
	assert(vec_size(elements));
	Type *type = type_flatten(expr->type);
	assert(type->type_kind != TYPE_SLICE);
	if (type->type_kind == TYPE_BITSTRUCT)
	{
		llvm_emit_initialize_reference_designated_bitstruct(c, ref, type->decl, elements);
		return;
	}

	// Make sure we have an address.
	llvm_value_addr(c, ref);

	// Clear the memory
	llvm_store_zero(c, ref);

	// Now walk through the elements.
	FOREACH(Expr *, designator, elements)
	{
		DesignatorElement **last_element = designator->designator_expr.path + vec_size(designator->designator_expr.path) - 1;
		llvm_emit_initialize_designated_element(c, ref, 0, designator->designator_expr.path, last_element,
		                                        designator->designator_expr.value, NULL);
	}
}


static bool bitstruct_requires_bitswap(Decl *decl)
{
	assert(decl->decl_kind == DECL_BITSTRUCT);
	bool big_endian = compiler.platform.big_endian;
	if (decl->strukt.big_endian) return !big_endian;
	if (decl->strukt.little_endian) return big_endian;
	return false;
}

LLVMValueRef llvm_emit_const_bitstruct_array(GenContext *c, ConstInitializer *initializer)
{
	Decl *decl = initializer->type->decl;
	Type *base_type = decl->strukt.container_type->type;
	unsigned elements = base_type->array.len;
	LLVMValueRef stack_data[MAX_AGG];
	LLVMValueRef* slots = elements > MAX_AGG ? MALLOC(elements * sizeof(LLVMValueRef)) : stack_data;
	for (unsigned i = 0; i < elements; i++)
	{
		slots[i] = llvm_get_zero_raw(c->byte_type);
	}
	Decl **members = decl->strukt.members;
	assert(vec_size(members) == vec_size(initializer->init_struct));
	FOREACH_IDX(i, ConstInitializer *, init, initializer->init_struct)
	{
		Decl *member = members[i];
		unsigned start_bit = member->var.start_bit;
		unsigned end_bit = member->var.end_bit;
		Type *member_type = type_flatten(member->type);
		if (init->kind == CONST_INIT_ZERO) continue;
		assert(init->kind == CONST_INIT_VALUE);
		Expr *expr = init->init_value;

		// Special case for bool
		if (member_type == type_bool)
		{
			assert(expr_is_const_bool(expr));
			assert(start_bit == end_bit);

			// Completely skip zero.
			if (!expr->const_expr.b) continue;

			LLVMValueRef bit = llvm_emit_shl_fixed(c, LLVMConstInt(c->byte_type, 1, 0), start_bit % 8);
			unsigned byte = start_bit / 8;
			LLVMValueRef current_value = slots[byte];
			slots[byte] = llvm_emit_or_raw(c, current_value, bit);
			continue;
		}
		unsigned bit_size = end_bit - start_bit + 1;
		assert(bit_size > 0 && bit_size <= 128);
		BEValue val;
		llvm_emit_const_expr(c, &val, init->init_value);
		assert(val.kind == BE_VALUE);
		LLVMValueRef value = val.value;
		int start_byte = start_bit / 8;
		int end_byte = end_bit / 8;
		ByteSize member_type_bitsize = type_size(member_type) * 8;
		value = llvm_mask_low_bits(c, value, bit_size);
		if (bitstruct_requires_bitswap(decl) && bit_size > 8)
		{
			value = llvm_bswap_non_integral(c, value, bit_size);
		}
		int bit_offset = start_bit % 8;
		for (int j = start_byte; j <= end_byte; j++)
		{
			LLVMValueRef to_or;
			if (j == start_byte)
			{
				to_or = llvm_emit_shl_fixed(c, value, bit_offset);
			}
			else
			{
				to_or = llvm_emit_lshr_fixed(c, value, j * 8 - (int)start_bit);
			}
			if (j == end_byte)
			{
				to_or = llvm_mask_low_bits(c, to_or, end_bit % 8 + 1);
			}
			if (member_type_bitsize > 8) to_or = LLVMBuildTrunc(c->builder, to_or, c->byte_type, "");
			LLVMValueRef current_value = slots[(unsigned)j];
			slots[(unsigned)j] = llvm_emit_or_raw(c, to_or, current_value);
		}
	}
	return llvm_get_array(c->byte_type, slots, elements);
}

LLVMValueRef llvm_emit_const_bitstruct(GenContext *c, ConstInitializer *initializer)
{
	Decl *decl = initializer->type->decl;
	Type *base_type = decl->strukt.container_type->type;
	if (initializer->kind == CONST_INIT_ZERO) return llvm_get_zero(c, base_type);
	bool char_array = base_type->type_kind == TYPE_ARRAY;
	if (char_array)
	{
		return llvm_emit_const_bitstruct_array(c, initializer);
	}
	LLVMTypeRef llvm_base_type = llvm_get_type(c, base_type);
	LLVMValueRef result = llvm_get_zero_raw(llvm_base_type);
	Decl **members = decl->strukt.members;
	TypeSize base_type_size = type_size(base_type);
	TypeSize base_type_bitsize = base_type_size * 8;
	assert(vec_size(members) == vec_size(initializer->init_struct));
	FOREACH_IDX(i, ConstInitializer *, val, initializer->init_struct)
	{
		Decl *member = members[i];
		unsigned start_bit = member->var.start_bit;
		unsigned end_bit = member->var.end_bit;
		unsigned bit_size = end_bit - start_bit + 1;
		assert(bit_size > 0 && bit_size <= 128);
		LLVMValueRef value;
		if (val->kind == CONST_INIT_ZERO)
		{
			value = val->type == type_bool ? llvm_get_zero_raw(c->byte_type) : llvm_get_zero(c, val->type);
		}
		else
		{
			BEValue entry;
			assert(val->kind == CONST_INIT_VALUE);
			llvm_emit_const_expr(c, &entry, val->init_value);
			value = llvm_load_value_store(c, &entry);
		}
		value = llvm_zext_trunc(c, value, llvm_base_type);
		if (bit_size < base_type_bitsize)
		{
			LLVMValueRef mask = llvm_emit_lshr_fixed(c, llvm_get_ones_raw(llvm_base_type), base_type_bitsize - bit_size);
			value = llvm_emit_and_raw(c, mask, value);
		}
		if (start_bit > 0)
		{
			value = llvm_emit_shl_fixed(c, value, start_bit);
		}
		result = llvm_emit_or_raw(c, value, result);
	}
	if (bitstruct_requires_bitswap(decl))
	{
		return LLVMConstBswap(result);
	}
	return result;
}


/**
 * Initialize a constant aggregate type.
 */
static inline void llvm_emit_const_initialize_reference(GenContext *c, BEValue *ref, Expr *expr)
{
	assert(expr_is_const_initializer(expr));
	assert(type_flatten(expr->type)->type_kind != TYPE_SLICE);
	llvm_emit_const_init_ref(c, ref, expr->const_expr.initializer, true);
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

static inline LLVMValueRef llvm_emit_inc_dec_value(GenContext *c, SourceSpan span, BEValue *original, int diff, bool allow_wrap)
{
	assert(!llvm_value_is_addr(original));

	Type *type = original->type;
	switch (type->type_kind)
	{
		case TYPE_POINTER:
		{
			// Use byte here, we don't need a big offset.
			LLVMValueRef add = LLVMConstInt(diff < 0 ? llvm_get_type(c, type_ichar) : llvm_get_type(c, type_char), (unsigned long long)diff, diff < 0);
			return llvm_emit_pointer_gep_raw(c, llvm_get_pointee_type(c, type), original->value, add);
		}
		case ALL_FLOATS:
		{
			// We allow inc/dec on floats, which is same as f += 1.0 or f -= 1.0
			LLVMTypeRef llvm_type = llvm_get_type(c, type);
			LLVMValueRef add = LLVMConstReal(llvm_type, (double)diff);
			return LLVMBuildFAdd(c->builder, original->value, add, "fincdec");
		}
		case ALL_INTS:
		{
			// Instead of negative numbers do dec/inc with a positive number.
			LLVMTypeRef llvm_type = llvm_get_type(c, type);
			LLVMValueRef diff_value = LLVMConstInt(llvm_type, 1, false);
			if (!allow_wrap)
			{
				if (type_is_signed(type))
				{
					return diff > 0
					       ? LLVMBuildNSWAdd(c->builder, original->value, diff_value, "addnsw")
						   : LLVMBuildNSWSub(c->builder, original->value, diff_value, "subnsw");
				}
				return diff > 0
				       ? LLVMBuildNUWAdd(c->builder, original->value, diff_value, "addnuw")
				       : LLVMBuildNUWSub(c->builder, original->value, diff_value, "subnuw");
			}
			return diff > 0
			       ? llvm_emit_add_int(c, original->type, original->value, diff_value, span)
			       : llvm_emit_sub_int(c, original->type, original->value, diff_value, span);
		}
		case TYPE_VECTOR:
		{
			Type *element = type->array.base;
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
			ArraySize width = type->array.len;
			LLVMValueRef val = llvm_get_undef(c, type);
			for (ArraySize i = 0; i < width; i++)
			{
				val = llvm_emit_insert_value(c, val, diff_value, i);
			}
			if (is_integer)
			{
				return diff > 0
					   ? llvm_emit_add_int(c, original->type, original->value, val, span)
					   : llvm_emit_sub_int(c, original->type, original->value, val, span);
			}
			else
			{
				return LLVMBuildFAdd(c->builder, original->value, val, "fincdec");
			}
		}
		default:
			UNREACHABLE
	}
}
static inline void llvm_emit_inc_dec_change(GenContext *c, BEValue *addr, BEValue *after, BEValue *before,
											Expr *expr, int diff, bool allow_wrap)
{
	EMIT_LOC(c, expr);

	// Copy the address and make it a value.
	BEValue value = *addr;
	llvm_value_rvalue(c, &value);

	// Store the original value if we want it
	if (before) *before = value;

	LLVMValueRef after_value = llvm_emit_inc_dec_value(c, expr->span, &value, diff, allow_wrap);

	// Store the result aligned.
	llvm_store_raw(c, addr, after_value);
	if (after) llvm_value_set(after, after_value, addr->type);
}

static inline bool expr_is_vector_subscript(Expr *expr)
{
	if (expr->expr_kind != EXPR_SUBSCRIPT) return false;
	Type *type = type_lowering(exprptr(expr->subscript_expr.expr)->type);
	return type->type_kind == TYPE_VECTOR;
}

/**
 * This method implements the common ++x and --x operators on bitstructs
 */
static inline void llvm_emit_pre_post_inc_dec_bitstruct(GenContext *c, BEValue *be_value, Expr *lhs, int diff, bool pre)
{
	Expr *parent_expr = lhs->access_expr.parent;

	// Grab the parent
	BEValue parent;
	Decl *member = lhs->access_expr.ref;
	llvm_emit_expr(c, &parent, parent_expr);
	Decl *parent_decl = llvm_emit_bitstruct_member(c, &parent, type_flatten(parent_expr->type)->decl, member);

	BEValue value = parent;
	llvm_extract_bitvalue(c, &value, parent_decl, member);
	LLVMValueRef value_start = llvm_load_value_store(c, &value);
	LLVMValueRef result = llvm_emit_add_int(c, value.type, value_start, llvm_const_int(c, value.type, diff), lhs->span);

	llvm_value_set(be_value, pre ? result : value_start, value.type);

	if (type_lowering(parent_decl->type)->type_kind == TYPE_ARRAY)
	{
		llvm_emit_bitassign_array(c, result, parent, parent_decl, member);
		return;
	}

	// To start the assign, pull out the current value.
	LLVMValueRef current_value = llvm_load_value_store(c, &parent);
	bool bswap = bitstruct_requires_bitswap(parent_decl);
	if (bswap) current_value = llvm_emit_bswap(c, current_value);
	current_value = llvm_emit_bitstruct_value_update(c, current_value, type_size(parent_decl->type) * 8, LLVMTypeOf(current_value), member, result);
	if (bswap) current_value = llvm_emit_bswap(c, current_value);
	llvm_store_raw(c, &parent, current_value);
}

/**
 * This method implements the common ++x and --x operators on vector elements
 */
static inline void llvm_emit_pre_post_inc_dec_vector(GenContext *c, BEValue *value, Expr *expr, int diff, bool pre)
{
	// First grab the address
	BEValue addr;
	llvm_emit_exprid(c, &addr, expr->subscript_expr.expr);
	*value = addr;
	llvm_value_addr(c, &addr);

	// But we also want the value (of the full vector)
	llvm_value_rvalue(c, value);
	Type *vec = value->type;
	assert(vec->type_kind == TYPE_VECTOR);
	Type *element = vec->array.base;
	LLVMValueRef vector = value->value;

	// Now let's get the subscript and store it in value
	bool start_from_end = expr->subscript_expr.index.start_from_end;
	llvm_emit_exprid(c, value, expr->subscript_expr.index.expr);
	llvm_value_rvalue(c, value);
	LLVMValueRef index = value->value;
	if (start_from_end)
	{
		index = LLVMBuildNUWSub(c->builder, llvm_const_int(c, value->type, vec->array.len), index, "");
	}

	// We're now done, we can extract the current value:
	BEValue current_res;
	llvm_value_set(&current_res, LLVMBuildExtractElement(c->builder, vector, index, ""), element);

	// Calculate the new value.
	LLVMValueRef new_value = llvm_emit_inc_dec_value(c, expr->span, &current_res, diff, false);

	// We update the vector value.
	vector = LLVMBuildInsertElement(c->builder, vector, new_value, index, "");

	// And store it.
	llvm_store_raw(c, &addr, vector);

	// And set the return value.
	llvm_value_set(value, pre ? current_res.value : new_value, element);
}

/**
 * This method implements the common ++x and --x operators
 */
static inline void llvm_emit_pre_inc_dec(GenContext *c, BEValue *value, Expr *expr, int diff, bool allow_wrap)
{
	if (expr_is_vector_subscript(expr))
	{
		llvm_emit_pre_post_inc_dec_vector(c, value, expr, diff, true);
		return;
	}
	if (expr->expr_kind == EXPR_BITACCESS)
	{
		llvm_emit_pre_post_inc_dec_bitstruct(c, value, expr, diff, true);
		return;
	}
	// Pull out the address, also allowing temporaries.
	BEValue addr;
	llvm_emit_expr(c, &addr, expr);
	llvm_value_addr(c, &addr);

	// Set the value to the new value.
	llvm_emit_inc_dec_change(c, &addr, value, NULL, expr, diff, allow_wrap);
}

static inline void llvm_emit_deref(GenContext *c, BEValue *value, Expr *inner, Type *type)
{
	switch (inner->expr_kind)
	{
		case EXPR_UNARY:
			switch (inner->unary_expr.operator)
			{
				case UNARYOP_ADDR:
					llvm_emit_expr(c, value, inner->unary_expr.expr);
					return;
				case UNARYOP_TADDR:
					llvm_emit_expr(c, value, inner->unary_expr.expr);
					llvm_value_addr(c, value);
					return;
				default:
					break;
			}
			break;
		case EXPR_SUBSCRIPT_ADDR:
			llvm_emit_subscript_addr(c, value, inner);
			return;
		default:
			break;
	}
	llvm_emit_expr(c, value, inner);
	llvm_value_rvalue(c, value);
	if (safe_mode_enabled())
	{
		LLVMValueRef check = LLVMBuildICmp(c->builder, LLVMIntEQ, value->value, llvm_get_zero(c, inner->type), "checknull");
		scratch_buffer_clear();
		scratch_buffer_append("Dereference of null pointer, '");
		span_to_scratch(inner->span);
		scratch_buffer_append("' was null.");
		llvm_emit_panic_on_true(c, check, scratch_buffer_to_string(), inner->span, NULL, NULL, NULL);
	}
	// Convert pointer to address
	value->kind = BE_ADDRESS;
	value->type = type;
	value->alignment = type_abi_alignment(type);
}

/**
 * Emit the common x++ and x-- operations.
 */
static inline void llvm_emit_post_inc_dec(GenContext *c, BEValue *value, Expr *expr, int diff, bool allow_wrap)
{
	if (expr_is_vector_subscript(expr))
	{
		llvm_emit_pre_post_inc_dec_vector(c, value, expr, diff, false);
		return;
	}
	if (expr->expr_kind == EXPR_BITACCESS)
	{
		llvm_emit_pre_post_inc_dec_bitstruct(c, value, expr, diff, false);
		return;
	}

	// Retrieve the address, creating a temp in case this is
	// a temporary value (this gives us a lot of flexibility for temporaries)
	BEValue addr;
	llvm_emit_expr(c, &addr, expr);
	llvm_value_addr(c, &addr);

	// Perform the actual dec/inc to generate the new value.
	llvm_emit_inc_dec_change(c, &addr, NULL, value, expr, diff, allow_wrap);
}

static void llvm_emit_dynamic_method_addr(GenContext *c, BEValue *value, Expr *expr)
{
	llvm_emit_expr(c, value, expr->access_expr.parent);
	llvm_emit_type_from_any(c, value);
	llvm_value_rvalue(c, value);
	LLVMValueRef introspect = LLVMBuildIntToPtr(c->builder, value->value, c->ptr_type, "");

	AlignSize align;
	Decl *dyn_fn = expr->access_expr.ref;
	LLVMValueRef func = llvm_emit_dynamic_search(c, introspect, llvm_get_ref(c, dyn_fn));

	llvm_value_set(value, func, type_get_func_ptr(dyn_fn->type));
}

static void llvm_emit_unary_expr(GenContext *c, BEValue *value, Expr *expr)
{
	Type *type = type_reduced_from_expr(expr->unary_expr.expr);
	Expr *inner = expr->unary_expr.expr;
	LLVMValueRef llvm_value;
	switch (expr->unary_expr.operator)
	{
		case UNARYOP_ERROR:
			FATAL_ERROR("Illegal unary op %s", expr->unary_expr.operator);
		case UNARYOP_PLUS:
			// Folded
			UNREACHABLE
		case UNARYOP_NOT:
			llvm_emit_expr(c, value, inner);
			if (type_flat_is_vector(type))
			{
				llvm_value_rvalue(c, value);
				Type *vec_type = type_vector_type(type);
				if (type_is_float(vec_type))
				{
					llvm_value = LLVMBuildFCmp(c->builder, LLVMRealUEQ, value->value, llvm_get_zero(c, type), "not");
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
					llvm_value = LLVMBuildFCmp(c->builder, LLVMRealUEQ, value->value, llvm_get_zero(c, type), "not");
					break;
				case TYPE_BOOL:
					llvm_value_rvalue(c, value);
					llvm_value = LLVMBuildNot(c->builder, value->value, "not");
					break;
				case TYPE_SLICE:
					if (value->kind != BE_VALUE)
					{
						llvm_emit_len_for_expr(c, value, value);
						llvm_value_rvalue(c, value);
						llvm_value = value->value;
					}
					else
					{
						llvm_value = llvm_emit_extract_value(c, value->value, 1);
					}
					llvm_value = LLVMBuildIsNull(c->builder, llvm_value, "not");
					break;
				case ALL_INTS:
				case TYPE_FUNC_PTR:
				case TYPE_POINTER:
					llvm_value_rvalue(c, value);
					llvm_value = LLVMBuildIsNull(c->builder, value->value, "not");
					break;
				case TYPE_ANY:
				case TYPE_INTERFACE:
					llvm_emit_any_pointer(c, value, value);
					llvm_value_rvalue(c, value);
					llvm_value = LLVMBuildIsNull(c->builder, value->value, "not");
					break;
				case TYPE_ARRAY:
					// Handle the bitstruct to bool case.
					if (type->array.base == type_char)
					{
						llvm_value = llvm_emit_char_array_zero(c, value, true);
						break;
					}
					FALLTHROUGH;
				default:
					DEBUG_LOG("Unexpectedly tried to not %s", type_quoted_error_string(inner->type));
					UNREACHABLE
			}
			llvm_value_set(value, llvm_value, type_bool);
			return;
		case UNARYOP_BITNEG:
			llvm_emit_expr(c, value, inner);
			if (value->type->type_kind == TYPE_ARRAY)
			{
				llvm_value_addr(c, value);
				LLVMTypeRef big_int = LLVMIntTypeInContext(c->context, type_size(value->type) * 8);
				LLVMValueRef val = llvm_load(c, big_int, value->value, value->alignment, "");
				val = LLVMBuildNot(c->builder, val, "bnot");
				LLVMValueRef store = llvm_emit_alloca(c, big_int, value->alignment, "");
				llvm_store_to_ptr_raw_aligned(c, store, val, value->alignment);
				llvm_value_set_address(value, store, value->type, value->alignment);
				return;
			}
			llvm_value_rvalue(c, value);
			value->value = LLVMBuildNot(c->builder, value->value, "bnot");
			return;
		case UNARYOP_NEG:
			llvm_emit_expr(c, value, inner);
			llvm_value_rvalue(c, value);
			if (type_is_floatlike(type))
			{
				value->value = LLVMBuildFNeg(c->builder, value->value, "fneg");
				return;
			}
			assert(type->canonical != type_bool);
			if (compiler.build.feature.trap_on_wrap && !type_flat_is_vector(value->type))
			{
				LLVMValueRef zero = llvm_get_zero(c, expr->unary_expr.expr->type);
				LLVMTypeRef type_to_use = llvm_get_type(c, type->canonical);
				LLVMValueRef args[2] = { zero, value->value };
				LLVMValueRef call_res = llvm_emit_call_intrinsic(c, intrinsic_id.ssub_overflow,
																 &type_to_use, 1, args, 2);
				value->value = llvm_emit_extract_value(c, call_res, 0);
				LLVMValueRef ok = llvm_emit_extract_value(c, call_res, 1);
				llvm_emit_panic_on_true(c, ok, "Signed negation overflow", expr->span, NULL, NULL, NULL);
				return;
			}
			value->value = LLVMBuildNeg(c->builder, value->value, "neg");
			return;
		case UNARYOP_ADDR:
			if (inner->expr_kind == EXPR_ACCESS && inner->access_expr.ref->decl_kind == DECL_FUNC)
			{
				llvm_emit_dynamic_method_addr(c, value, inner);
				return;
			}
			FALLTHROUGH;
		case UNARYOP_TADDR:
			llvm_emit_expr(c, value, inner);
			// Create an addr
			llvm_value_addr(c, value);
			// Transform to value
			value->kind = BE_VALUE;
			value->type = type_lowering(expr->type);
			return;
		case UNARYOP_DEREF:
			llvm_emit_deref(c, value, inner, type_lowering(expr->type));
			return;
		case UNARYOP_INC:
			llvm_emit_pre_inc_dec(c, value, inner, 1, !expr->unary_expr.no_wrap);
			return;
		case UNARYOP_DEC:
			llvm_emit_pre_inc_dec(c, value, inner, -1, !expr->unary_expr.no_wrap);
			return;
	}
	UNREACHABLE
}

void llvm_emit_len_for_expr(GenContext *c, BEValue *be_value, BEValue *expr_to_len)
{
	switch (expr_to_len->type->type_kind)
	{
		case TYPE_SLICE:
			llvm_value_fold_optional(c, be_value);
			if (expr_to_len->kind == BE_VALUE)
			{
				llvm_value_set(be_value, llvm_emit_extract_value(c, expr_to_len->value, 1), type_usz);
			}
			else
			{
				LLVMTypeRef slice_type = llvm_get_type(c, expr_to_len->type);
				AlignSize alignment;
				LLVMValueRef len_addr = llvm_emit_struct_gep_raw(c,
				                                                 expr_to_len->value,
				                                                 slice_type,
				                                                 1,
				                                                 expr_to_len->alignment,
				                                                 &alignment);
				llvm_value_set_address(be_value, len_addr, type_usz, alignment);
			}
			break;
		case TYPE_ARRAY:
		case TYPE_VECTOR:
			llvm_value_set(be_value, llvm_const_int(c, type_usz, expr_to_len->type->array.len), type_usz);
			break;
		default:
			UNREACHABLE
	}
}

static void llvm_emit_trap_negative(GenContext *c, Expr *expr, LLVMValueRef value, const char *error,
									BEValue *index_val)
{
	if (!safe_mode_enabled()) return;
	if (type_is_integer_unsigned(expr->type->canonical)) return;

	LLVMValueRef zero = llvm_const_int(c, expr->type, 0);
	LLVMValueRef ok = LLVMBuildICmp(c->builder, LLVMIntSLT, value, zero, "underflow");
	llvm_emit_panic_on_true(c, ok, "Negative value", expr->span, error, index_val, NULL);
}

static void llvm_emit_trap_zero(GenContext *c, Type *type, LLVMValueRef value, const char *error, SourceSpan loc)
{
	if (!safe_mode_enabled()) return;

	assert(type == type_flatten(type));

	if (type_flat_is_vector(type))
	{
		Type *base_type = type->array.base;
		LLVMTypeRef llvm_type = llvm_get_type(c, type);
		if (type_is_float(base_type))
		{
			value = llvm_emit_call_intrinsic(c, intrinsic_id.fabs, &llvm_type, 1, &value, 1);
			value = llvm_emit_call_intrinsic(c, intrinsic_id.vector_reduce_fmin, &llvm_type, 1, &value, 1);
		}
		else
		{
			value = llvm_emit_call_intrinsic(c, intrinsic_id.vector_reduce_umin, &llvm_type, 1, &value, 1);
		}
		// Set the value to the base type.
		type = base_type;
	}

	LLVMValueRef zero = llvm_get_zero(c, type);
	LLVMValueRef ok = type_is_integer(type) ? LLVMBuildICmp(c->builder, LLVMIntEQ, value, zero, "zero") : LLVMBuildFCmp(c->builder, LLVMRealUEQ, value, zero, "zero");
	llvm_emit_panic_on_true(c, ok, error, loc, NULL, NULL, NULL);
}


static void llvm_emit_trap_invalid_shift(GenContext *c, LLVMValueRef value, Type *type, const char *error, SourceSpan loc)
{
	if (!safe_mode_enabled()) return;
	BEValue val;
	type = type_flatten(type);
	llvm_value_set(&val, value, type);
	if (type_flat_is_vector(type))
	{
		Type *vec_base = type->array.base;
		unsigned type_bit_size = type_size(vec_base) * 8;
		LLVMTypeRef llvm_type = llvm_get_type(c, type);
		LLVMValueRef max = llvm_const_int(c, vec_base, type_bit_size);
		if (type_is_unsigned(vec_base))
		{
			LLVMValueRef flat_max = llvm_emit_call_intrinsic(c, intrinsic_id.vector_reduce_umax, &llvm_type, 1, &value, 1);
			LLVMValueRef equal_or_greater = LLVMBuildICmp(c->builder, LLVMIntUGE, flat_max, max, "shift_exceeds");
			llvm_emit_panic_on_true(c, equal_or_greater, "Invalid shift", loc, error, &val, NULL);
			return;
		}
		LLVMValueRef flat_min = llvm_emit_call_intrinsic(c, intrinsic_id.vector_reduce_smin, &llvm_type, 1, &value, 1);
		LLVMValueRef zero = llvm_const_int(c, vec_base, 0);
		LLVMValueRef negative = LLVMBuildICmp(c->builder, LLVMIntSLT, flat_min, zero, "shift_underflow");
		llvm_emit_panic_on_true(c, negative, "Invalid shift", loc, error, &val, NULL);
		LLVMValueRef flat_max = llvm_emit_call_intrinsic(c, intrinsic_id.vector_reduce_smax, &llvm_type, 1, &value, 1);
		LLVMValueRef equal_or_greater = LLVMBuildICmp(c->builder, LLVMIntSGE, flat_max, max, "shift_exceeds");
		llvm_emit_panic_on_true(c, equal_or_greater, "Invalid shift", loc, error, &val, NULL);
		return;

	}
	else
	{
		unsigned type_bit_size = type_size(type) * 8;
		LLVMValueRef max = llvm_const_int(c, type, type_bit_size);
		if (type_is_unsigned(type))
		{
			LLVMValueRef equal_or_greater = LLVMBuildICmp(c->builder, LLVMIntUGE, value, max, "shift_exceeds");
			llvm_emit_panic_on_true(c, equal_or_greater, "Invalid shift", loc, error, &val, NULL);
			return;
		}
		LLVMValueRef zero = llvm_const_int(c, type, 0);
		LLVMValueRef negative = LLVMBuildICmp(c->builder, LLVMIntSLT, value, zero, "shift_underflow");
		llvm_emit_panic_on_true(c, negative, "Invalid shift", loc, error, &val, NULL);
		LLVMValueRef equal_or_greater = LLVMBuildICmp(c->builder, LLVMIntSGE, value, max, "shift_exceeds");
		llvm_emit_panic_on_true(c, equal_or_greater, "Invalid shift", loc, error, &val, NULL);
	}
}

static void llvm_emit_slice_values(GenContext *c, Expr *slice, BEValue *parent_ref, BEValue *start_ref, BEValue *end_ref, bool *is_exclusive)
{
	assert(slice->expr_kind == EXPR_SLICE);

	Expr *parent_expr = exprptr(slice->subscript_expr.expr);

	Type *parent_type = type_flatten(parent_expr->type);
	BEValue parent_addr_x;
	llvm_emit_expr(c, &parent_addr_x, parent_expr);
	llvm_value_addr(c, &parent_addr_x);
	LLVMValueRef parent_addr = parent_addr_x.value;
	LLVMValueRef parent_load_value = NULL;
	LLVMValueRef parent_base;
	bool is_optional = type_is_optional(parent_type);
	parent_type = type_no_optional(parent_type);
	switch (parent_type->type_kind)
	{
		case TYPE_POINTER:
			parent_load_value = parent_base = LLVMBuildLoad2(c->builder, llvm_get_type(c, parent_type), parent_addr, "");
			break;
		case TYPE_SLICE:
			parent_load_value = LLVMBuildLoad2(c->builder, llvm_get_type(c, parent_type), parent_addr, "");
			parent_base = llvm_emit_extract_value(c, parent_load_value, 0);
			break;
		case TYPE_FLEXIBLE_ARRAY:
		case TYPE_ARRAY:
		case TYPE_VECTOR:
			parent_base = parent_addr;
			break;
		default:
			UNREACHABLE
	}

	// Emit the start and end

	Type *start_type;
	Range range = slice->slice_expr.range;
	BEValue start_index;
	switch (range.range_type)
	{
		case RANGE_DYNAMIC:
		case RANGE_CONST_LEN:
		case RANGE_CONST_END:
			llvm_emit_exprid(c, &start_index, range.start);
			llvm_value_rvalue(c, &start_index);
			start_type = start_index.type;
			break;
		case RANGE_CONST_RANGE:
			start_type = type_isz;
			llvm_value_set_int(c, &start_index, type_isz, range.start_index);
			break;
	}

	BEValue len = { .value = NULL };
	bool check_end = true;
	bool start_from_end = range.start_from_end;
	bool end_from_end = range.end_from_end;
	bool has_end = range.range_type != RANGE_DYNAMIC || range.end;
	if (!has_end || start_from_end || end_from_end || safe_mode_enabled())
	{
		switch (parent_type->type_kind)
		{
			case TYPE_POINTER:
			case TYPE_FLEXIBLE_ARRAY:
				len.value = NULL;
				check_end = false;
				break;
			case TYPE_SLICE:
				assert(parent_load_value);
				llvm_value_set(&len, llvm_emit_extract_value(c, parent_load_value, 1), start_type);
				break;
			case TYPE_ARRAY:
			case TYPE_VECTOR:
				llvm_value_set_int(c, &len, start_type, parent_type->array.len);
				break;
			default:
				UNREACHABLE
		}
	}

	// Walk from end if it is a slice from the back.
	if (start_from_end)
	{
		start_index.value = llvm_emit_sub_int(c, start_index.type, len.value, start_index.value, slice->span);
	}

	// Check that index does not extend beyond the length.
	if (check_end && safe_mode_enabled())
	{
		assert(len.value);
		BEValue exceeds_size;
		llvm_emit_int_comp(c, &exceeds_size, &start_index, &len, BINARYOP_GT);
		llvm_emit_panic_if_true(c, &exceeds_size, "Index exceeds array len", slice->span, "Index exceeds array length (array had size %d, index was %d).", &len, &start_index);
	}

	// Insert trap for negative start offset for non pointers.
	if (parent_type->type_kind != TYPE_POINTER && range.range_type != RANGE_CONST_RANGE)
	{
		llvm_emit_trap_negative(c, exprptr(range.start), start_index.value, "Negative indexing (%d)", &start_index);
	}

	BEValue end_index;
	bool is_len_range = *is_exclusive = range.is_len;
	Type *end_type = start_type;
	if (has_end)
	{
		// Get the index.
		switch (range.range_type)
		{
			case RANGE_DYNAMIC:
				llvm_emit_exprid(c, &end_index, range.end);
				llvm_value_rvalue(c, &end_index);
				end_type = end_index.type;
				break;
			case RANGE_CONST_LEN:
				assert(range.is_len);
				llvm_value_set_int(c, &end_index, end_type, range.const_end);
				break;
			case RANGE_CONST_END:
				assert(!range.is_len);
				llvm_value_set_int(c, &end_index, end_type, range.const_end);
				break;
			case RANGE_CONST_RANGE:
				llvm_value_set_int(c, &end_index, end_type, range.len_index);
				break;
		}

		// Reverse if it is "from back"
		if (end_from_end)
		{
			assert(range.range_type == RANGE_DYNAMIC);
			end_index.value = llvm_emit_sub_int(c, end_index.type, len.value, end_index.value, slice->span);
			llvm_value_rvalue(c, &end_index);
		}
		if (is_len_range)
		{
			end_index.value = llvm_emit_add_int(c, end_index.type, start_index.value, end_index.value, slice->span);
		}

		// This will trap any bad negative index, so we're fine.
		if (safe_mode_enabled())
		{
			if (is_len_range)
			{
				if (len.value)
				{
					BEValue excess;
					llvm_emit_int_comp(c, &excess, &len, &end_index, BINARYOP_LT);
					BEValue actual_end_index = end_index;
					actual_end_index.value = llvm_emit_sub_int(c, end_index.type, end_index.value, llvm_const_int(c, type_isz, 1), slice->span);
					llvm_emit_panic_if_true(c, &excess, "End index out of bounds", slice->span, "End index out of bounds (end index of %d exceeds size of %d)", &actual_end_index, &len);
				}
			}
			else
			{
				BEValue excess;
				llvm_emit_int_comp(c, &excess, &start_index, &end_index, BINARYOP_GT);
				llvm_emit_panic_if_true(c, &excess, "Negative size", slice->span, "Negative size (start %d is less than end %d)", &start_index, &end_index);

				if (len.value)
				{
					llvm_emit_int_comp(c, &excess, &len, &end_index, BINARYOP_LE);
					llvm_emit_panic_if_true(c, &excess, "End index out of bounds", slice->span, "End index out of bounds (end index of %d exceeds size of %d)", &end_index, &len);
				}
			}
		}
	}
	else
	{
		assert(len.value && "Pointer should never end up here.");
		end_index.value = len.value;
		end_type = start_type;
		// Use "len-range" when implicit, this avoids len - 1 here.
		*is_exclusive = true;
	}

	llvm_value_set(end_ref, end_index.value, end_type);
	llvm_value_set(start_ref, start_index.value, end_type);
	llvm_value_set_address(parent_ref, parent_base, parent_type, type_abi_alignment(parent_type));
}

static void gencontext_emit_slice(GenContext *c, BEValue *be_value, Expr *expr)
{
	// Use general function to get all the values we need (a lot!)
	BEValue parent;
	BEValue start;
	BEValue end;
	bool is_exclusive;
	llvm_emit_slice_values(c, expr, &parent, &start, &end, &is_exclusive);
	llvm_value_rvalue(c, &start);
	llvm_value_rvalue(c, &end);


	// Calculate the size
	LLVMValueRef size;
	if (is_exclusive)
	{
		size = LLVMBuildSub(c->builder, end.value, start.value, "size");
	}
	else
	{
		size = LLVMBuildSub(c->builder, LLVMBuildAdd(c->builder, end.value, llvm_const_int(c, start.type, 1), ""), start.value, "size");
	}
	LLVMValueRef start_pointer;
	Type *type = type_lowering(parent.type);
	switch (type->type_kind)
	{
		case TYPE_FLEXIBLE_ARRAY:
		case TYPE_ARRAY:
		case TYPE_VECTOR:
		{
			// Move pointer
			AlignSize alignment;
			start_pointer = llvm_emit_array_gep_raw_index(c, parent.value, llvm_get_type(c, parent.type), &start, type_abi_alignment(parent.type), &alignment);
			break;
		}
		case TYPE_SLICE:
			start_pointer = llvm_emit_pointer_inbounds_gep_raw(c, llvm_get_type(c, parent.type->array.base), parent.value, start.value);
			break;
		case TYPE_POINTER:
			start_pointer = llvm_emit_pointer_inbounds_gep_raw(c, llvm_get_pointee_type(c, parent.type), parent.value, start.value);
			break;
		default:
			UNREACHABLE
	}

	// Create a new slice type
	llvm_value_aggregate_two(c, be_value, type_lowering(expr->type), start_pointer, size);
}

static void llvm_emit_slice_copy(GenContext *c, BEValue *be_value, Expr *expr)
{
	llvm_emit_exprid(c, be_value, expr->slice_assign_expr.right);
	llvm_value_rvalue(c, be_value);

	BEValue assigned_to;
	llvm_emit_exprid(c, &assigned_to, expr->slice_assign_expr.left);
	llvm_value_rvalue(c, &assigned_to);

	BEValue to_pointer;
	llvm_emit_slice_pointer(c, &assigned_to, &to_pointer);
	llvm_value_rvalue(c, &to_pointer);

	BEValue from_pointer;
	BEValue from_len;
	llvm_emit_slice_pointer(c, be_value, &from_pointer);
	llvm_value_rvalue(c, &from_pointer);
	llvm_emit_slice_len(c, be_value, &from_len);
	llvm_value_rvalue(c, &from_len);

	if (safe_mode_enabled())
	{
		BEValue to_len;
		llvm_emit_slice_len(c, &assigned_to, &to_len);
		BEValue comp;
		llvm_emit_int_comp(c, &comp, &to_len, &from_len, BINARYOP_NE);
		llvm_emit_panic_if_true(c, &comp, "Length mismatch", expr->span, "Slice copy length mismatch (%d != %d).", &to_len, &from_len);
	}


	Type *pointer_type = to_pointer.type->pointer;
	unsigned alignment = type_abi_alignment(pointer_type);
	LLVMValueRef bytes = LLVMBuildMul(c->builder, from_len.value, llvm_const_int(c, from_len.type, type_size(pointer_type)), "");
	LLVMBuildMemMove(c->builder, to_pointer.value, alignment, from_pointer.value, alignment, bytes);
}


static void llvm_emit_slice_assign(GenContext *c, BEValue *be_value, Expr *expr)
{
	// We will be replacing the slice assign with code that roughly looks like this:
	// size_t end = slice_end;
	// size_t slice_current = slice_start;
	// while (slice_current <= end) pointer[slice_current++] = value;

	// First, find the value assigned.
	Expr *assigned_value = exprptr(expr->slice_assign_expr.right);
	llvm_emit_expr(c, be_value, assigned_value);
	assert(!IS_OPTIONAL(assigned_value));
	// If this is copying a big value, then first store it in a variable, this is to
	// ensure value semantics even in special cases.
	if (llvm_value_is_addr(be_value) && type_size(assigned_value->type) > 16)
	{
		LLVMValueRef address = llvm_emit_alloca(c, llvm_get_type(c, be_value->type), be_value->alignment, "tempval");
		llvm_store_to_ptr(c, address, be_value);
		// Replace the old value with this temp
		llvm_value_set_address(be_value, address, be_value->type, be_value->alignment);
	}
	else
	{
		llvm_value_rvalue(c, be_value);
	}

	BEValue parent;
	BEValue start;
	BEValue end;
	// Use general function to get all the values we need (a lot!)
	bool is_exclusive;
	llvm_emit_slice_values(c, exprptr(expr->slice_assign_expr.left), &parent, &start, &end, &is_exclusive);
	llvm_value_rvalue(c, &start);
	llvm_value_rvalue(c, &end);

	if (llvm_is_const(start.value) && llvm_is_const(end.value))
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
		if (is_exclusive)
		{
			if (start_val == end_val) return;
			end_val--;
		}
		if (end_val - start_val < SLICE_MAX_UNROLL)
		{
			BEValue addr;
			BEValue offset_val;
			for (uint64_t i = start_val; i <= end_val; i++)
			{
				llvm_value_set_int(c, &offset_val, type_usz, i);
				llvm_emit_subscript_addr_with_base(c, &addr, &parent, &offset_val, expr->span);

				// And store the value.
				llvm_store(c, &addr, be_value);
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
	BinaryOp op = is_exclusive ? BINARYOP_LT : BINARYOP_LE;
	llvm_emit_int_comp_raw(c, &value, start.type, end.type, offset, end.value, op);

	// If jump to the assign block if we're not at the end index.
	EMIT_LOC(c, expr);
	llvm_emit_cond_br(c, &value, assign_block, exit_block);

	// Emit the assign.
	llvm_emit_block(c, assign_block);
	// Reuse this calculation
	BEValue addr;
	llvm_emit_subscript_addr_with_base(c, &addr, &parent, &offset_val, expr->span);

	// And store the value.
	llvm_store(c, &addr, be_value);

	// Create the new offset
	LLVMValueRef next_offset = llvm_emit_add_int(c, start.type, offset, llvm_const_int(c, start.type, 1), expr->span);
	LLVMBasicBlockRef assign_block_end = llvm_get_current_block_if_in_use(c);

	// And jump back
	llvm_emit_br(c, cond_block);

	// Finally set up our phi
	if (!assign_block_end)
	{
		offset = start.value;
	}
	else
	{
		llvm_set_phi(offset, start.value, start_block, next_offset, assign_block_end);
	}

	// And emit the exit block.
	llvm_emit_block(c, exit_block);

}

static void llvm_emit_logical_and_or(GenContext *c, BEValue *be_value, Expr *expr, BinaryOp op)
{
	LLVMBasicBlockRef lhs_end_block;

	// Generate left-hand condition and conditional branch
	llvm_emit_expr(c, be_value, exprptr(expr->binary_expr.left));
	llvm_value_rvalue(c, be_value);

	lhs_end_block = llvm_get_current_block_if_in_use(c);

	LLVMValueRef result_on_skip = LLVMConstInt(c->bool_type, op == BINARYOP_AND ? 0 : 1, 0);

	// We might end this with a jump, eg (foo()? || bar()) where foo() is a macro and guaranteed not to exit.
	if (!lhs_end_block)
	{
		// Just set any value.
		llvm_value_set(be_value, result_on_skip, type_bool);
		return;
	}

	// Set up basic blocks, following Cone
	LLVMBasicBlockRef phi_block = llvm_basic_block_new(c, op == BINARYOP_AND ? "and.phi" : "or.phi");
	LLVMBasicBlockRef rhs_block = llvm_basic_block_new(c, op == BINARYOP_AND ? "and.rhs" : "or.rhs");

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
	llvm_emit_expr(c, &rhs_value, exprptr(expr->binary_expr.right));
	llvm_value_rvalue(c, &rhs_value);

	LLVMBasicBlockRef rhs_end_block = llvm_get_current_block_if_in_use(c);

	if (rhs_end_block)
	{
		llvm_emit_br(c, phi_block);
	}

	// Generate phi
	llvm_emit_block(c, phi_block);

	// One possibility here is that a return happens inside of the expression.
	if (!rhs_end_block)
	{
		llvm_value_set(be_value, result_on_skip, type_bool);
		return;
	}
	llvm_new_phi(c, be_value, "val", type_bool, result_on_skip, lhs_end_block, rhs_value.value, rhs_end_block);
}

void llvm_emit_int_comp_zero(GenContext *c, BEValue *result, BEValue *lhs, BinaryOp binary_op)
{
	BEValue zero;
	llvm_value_set_int(c, &zero, lhs->type, 0);
	llvm_emit_int_comp(c, result, lhs, &zero, binary_op);
}

void llvm_emit_int_comp(GenContext *c, BEValue *result, BEValue *lhs, BEValue *rhs, BinaryOp binary_op)
{
	llvm_value_rvalue(c, lhs);
	llvm_value_rvalue(c, rhs);
	llvm_emit_int_comp_raw(c, result, lhs->type, rhs->type, lhs->value, rhs->value, binary_op);
}
void llvm_emit_int_comp_raw(GenContext *c, BEValue *result, Type *lhs_type, Type *rhs_type, LLVMValueRef lhs_value, LLVMValueRef rhs_value, BinaryOp binary_op)
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
			(void)rhs_type;
			lhs_signed = true;
			rhs_signed = false;
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
		}
	}
	if (lhs_signed && !rhs_signed && !vector_type && llvm_is_const(lhs_value) && type_size(lhs_type) <= 8)
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
			llvm_convert_vector_comparison(c, result, value, lhs_type, binary_op == BINARYOP_EQ);
			return;
		}
		llvm_value_set(result, value, type_bool);
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
			llvm_convert_vector_comparison(c, result, comp_value, lhs_type, binary_op == BINARYOP_EQ);
			return;
		}
		llvm_value_set(result, comp_value, type_bool);
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
		llvm_convert_vector_comparison(c, result, comp_value, lhs_type, BINARYOP_EQ == binary_op);
		return;
	}
	llvm_value_set(result, comp_value, type_bool);
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
	llvm_value_set(result, val, type_bool);
}

static void llvm_emit_any_comparison(GenContext *c, BEValue *result, BEValue *lhs, BEValue *rhs, BinaryOp binary_op)
{
	BEValue pointer_lhs;
	BEValue pointer_rhs;
	llvm_emit_any_pointer(c, lhs, &pointer_lhs);
	llvm_emit_type_from_any(c, lhs);
	llvm_value_rvalue(c, &pointer_lhs);
	llvm_value_rvalue(c, lhs);
	llvm_emit_any_pointer(c, rhs, &pointer_rhs);
	llvm_emit_type_from_any(c, rhs);
	llvm_value_rvalue(c, &pointer_rhs);
	llvm_value_rvalue(c, rhs);

	LLVMValueRef val;
	LLVMValueRef val2;
	LLVMValueRef res;
	LLVMIntPredicate comparison;
	const char *desc;
	switch (binary_op)
	{
		case BINARYOP_EQ:
			val = LLVMBuildICmp(c->builder, LLVMIntEQ, pointer_lhs.value, pointer_rhs.value, "ptr_eq");
			val2 = LLVMBuildICmp(c->builder, LLVMIntEQ, lhs->value, rhs->value, "type_eq");
			res = LLVMBuildAnd(c->builder, val, val2, "any_eq");
			break;
		case BINARYOP_NE:
			val = LLVMBuildICmp(c->builder, LLVMIntNE, pointer_lhs.value, pointer_rhs.value, "ptr_ne");
			val2 = LLVMBuildICmp(c->builder, LLVMIntNE, lhs->value, rhs->value, "type_ne");
			res = LLVMBuildOr(c->builder, val, val2, "any_ne");
			break;
		default:
			UNREACHABLE
	}
	llvm_value_set(result, res, type_bool);
}

static void llvm_emit_struct_comparison(GenContext *c, BEValue *result, BEValue *lhs, BEValue *rhs, BinaryOp binary_op)
{
	llvm_value_fold_optional(c, lhs);
	llvm_value_fold_optional(c, rhs);
	llvm_value_addr(c, lhs);
	llvm_value_addr(c, rhs);
	llvm_emit_memcmp(c, result, lhs->value, rhs->value, llvm_const_int(c, type_usz, type_size(lhs->type)));
	llvm_emit_int_comp_zero(c, result, result, binary_op);
}

static inline LLVMValueRef llvm_emit_mult_int(GenContext *c, Type *type, LLVMValueRef left, LLVMValueRef right, SourceSpan loc)
{
	if (compiler.build.feature.trap_on_wrap && type->type_kind != TYPE_VECTOR)
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
		LLVMValueRef val = llvm_emit_extract_value(c, call_res, 0);
		LLVMValueRef ok = llvm_emit_extract_value(c, call_res, 1);
		llvm_emit_panic_on_true(c, ok, "Integer multiplication overflow", loc, NULL, NULL, NULL);
		return val;
	}
	return LLVMBuildMul(c->builder, left, right, "mul");
}

static void llvm_emit_slice_comp(GenContext *c, BEValue *be_value, BEValue *lhs, BEValue *rhs, BinaryOp binary_op)
{
	bool want_match = binary_op == BINARYOP_EQ;

	Type *array_base_type = type_lowering(lhs->type->array.base);
	Type *array_base_pointer = type_get_ptr(array_base_type);
	LLVMTypeRef llvm_base_type = llvm_get_type(c, array_base_type);

	LLVMBasicBlockRef exit = llvm_basic_block_new(c, "slice_cmp_exit");
	LLVMBasicBlockRef value_cmp = llvm_basic_block_new(c, "slice_cmp_values");
	LLVMBasicBlockRef loop_begin = llvm_basic_block_new(c, "slice_loop_start");
	LLVMBasicBlockRef comparison = llvm_basic_block_new(c, "slice_loop_comparison");
	LLVMBasicBlockRef no_match_block;
	LLVMBasicBlockRef all_match_block;
	LLVMBasicBlockRef match_fail_block;

	llvm_value_rvalue(c, lhs);
	llvm_value_rvalue(c, rhs);
	BEValue lhs_len;
	BEValue rhs_len;
	llvm_value_set(&lhs_len, llvm_emit_extract_value(c, lhs->value, 1), type_isz);
	llvm_value_set(&rhs_len, llvm_emit_extract_value(c, rhs->value, 1), type_isz);
	BEValue lhs_value;
	BEValue rhs_value;
	llvm_value_set(&lhs_value, llvm_emit_extract_value(c, lhs->value, 0), array_base_pointer);
	llvm_value_set(&rhs_value, llvm_emit_extract_value(c, rhs->value, 0), array_base_pointer);
	BEValue len_match;
	llvm_emit_comp(c, &len_match, &lhs_len, &rhs_len, BINARYOP_EQ);

	no_match_block = c->current_block;
	llvm_emit_cond_br(c, &len_match, value_cmp, exit);

	llvm_emit_block(c, value_cmp);
	BEValue index_var;
	llvm_value_set_address_abi_aligned(&index_var, llvm_emit_alloca_aligned(c, type_isz, "cmp.idx"), type_isz);
	LLVMValueRef one = llvm_const_int(c, type_isz, 1);
	llvm_store_raw(c, &index_var, llvm_get_zero(c, type_isz));
	llvm_emit_br(c, loop_begin);

	llvm_emit_block(c, loop_begin);
	BEValue current_index = index_var;
	llvm_value_rvalue(c, &current_index);
	BEValue cmp;
	llvm_emit_comp(c, &cmp, &current_index, &lhs_len, BINARYOP_LT);
	all_match_block = c->current_block;
	llvm_emit_cond_br(c, &cmp, comparison, exit);

	llvm_emit_block(c, comparison);
	BEValue lhs_to_compare;
	BEValue rhs_to_compare;
	llvm_value_set_address_abi_aligned(&lhs_to_compare,
									   llvm_emit_pointer_inbounds_gep_raw(c,
																		  llvm_base_type,
																		  lhs_value.value,
																		  current_index.value),
									   array_base_type);
	llvm_value_set_address_abi_aligned(&rhs_to_compare,
									   llvm_emit_pointer_inbounds_gep_raw(c,
																		  llvm_base_type,
																		  rhs_value.value,
																		  current_index.value),
									   array_base_type);
	llvm_emit_comp(c, &cmp, &lhs_to_compare, &rhs_to_compare, BINARYOP_EQ);
	match_fail_block = c->current_block;
	llvm_store_raw(c, &index_var, LLVMBuildAdd(c->builder, current_index.value, one, ""));
	llvm_emit_cond_br(c, &cmp, loop_begin, exit);
	llvm_emit_block(c, exit);

	LLVMValueRef phi = LLVMBuildPhi(c->builder, c->bool_type, "slice_cmp_phi");

	LLVMValueRef success = LLVMConstInt(c->bool_type, want_match ? 1 : 0, false);
	LLVMValueRef failure = LLVMConstInt(c->bool_type, want_match ? 0 : 1, false);
	LLVMValueRef logic_values[3] = { success, failure, failure };
	LLVMBasicBlockRef blocks[3] = { all_match_block, no_match_block, match_fail_block };
	LLVMAddIncoming(phi, logic_values, blocks, 3);

	llvm_value_set(be_value, phi, type_bool);

}

INLINE bool should_inline_array_comp(ArraySize len, Type *base_type_lowered)
{
	RETRY:
	switch (base_type_lowered->type_kind)
	{
		case TYPE_ARRAY:
			len *= base_type_lowered->array.len;
			base_type_lowered = type_lowering(base_type_lowered->array.base);
			goto RETRY;
		case TYPE_SLICE:
			return len <= 4;
		default:
			return len <= 16;
	}
}

static inline void llvm_emit_memcmp(GenContext *c, BEValue *be_value, LLVMValueRef ptr, LLVMValueRef other_ptr, LLVMValueRef size)
{
	if (!c->memcmp_function)
	{
		c->memcmp_function = LLVMGetNamedFunction(c->module, kw_memcmp);
		if (!c->memcmp_function)
		{
			c->memcmp_function = LLVMAddFunction(c->module, kw_memcmp, c->memcmp_function_type);
		}
	}
	LLVMValueRef args[3] = { ptr, other_ptr, size };
	LLVMValueRef function = LLVMBuildCall2(c->builder, c->memcmp_function_type, c->memcmp_function, args, 3, "cmp");
	llvm_value_set(be_value, function, type_cint);
}

static inline void llvm_emit_fp_vector_compare(GenContext *c, BEValue *be_value, BEValue *lhs, BEValue *rhs, BinaryOp binary_op, Type *base_type, unsigned len)
{
	LLVMTypeRef fp_vec = LLVMVectorType(llvm_get_type(c, base_type), len);
	LLVMTypeRef bool_vec = LLVMVectorType(c->bool_type, len);
	llvm_value_addr(c, lhs);
	llvm_value_addr(c, rhs);
	LLVMValueRef left = llvm_load(c, fp_vec, lhs->value, lhs->alignment, "lhs");
	LLVMValueRef right = llvm_load(c, fp_vec, rhs->value, rhs->alignment, "rhs");
	LLVMValueRef cmp = LLVMBuildFCmp(c->builder, binary_op == BINARYOP_EQ ? LLVMRealOEQ : LLVMRealONE, left, right, "cmp");
	if (binary_op == BINARYOP_EQ)
	{
		cmp = llvm_emit_call_intrinsic(c, intrinsic_id.vector_reduce_and, &bool_vec, 1, &cmp, 1);
	}
	else
	{
		cmp = llvm_emit_call_intrinsic(c, intrinsic_id.vector_reduce_or, &bool_vec, 1, &cmp, 1);
	}
	llvm_value_set(be_value, cmp, type_bool);
}

static inline void llvm_emit_bool_vector_compare(GenContext *c, BEValue *be_value, BEValue *lhs, BEValue *rhs, BinaryOp binary_op, unsigned len)
{
	LLVMTypeRef bool_vec = LLVMVectorType(c->bool_type, len);
	LLVMTypeRef load_vec = LLVMVectorType(c->byte_type, len);
	llvm_value_addr(c, lhs);
	llvm_value_addr(c, rhs);
	LLVMValueRef left = llvm_load(c, load_vec, lhs->value, lhs->alignment, "lhs");
	LLVMValueRef right = llvm_load(c, load_vec, rhs->value, rhs->alignment, "rhs");
	left = LLVMBuildTrunc(c->builder, left, bool_vec, "");
	right = LLVMBuildTrunc(c->builder, right, bool_vec, "");
	LLVMValueRef cmp = LLVMBuildICmp(c->builder, binary_op == BINARYOP_EQ ? LLVMIntEQ : LLVMIntNE, left, right, "cmp");
	if (binary_op == BINARYOP_EQ)
	{
		cmp = llvm_emit_call_intrinsic(c, intrinsic_id.vector_reduce_and, &bool_vec, 1, &cmp, 1);
	}
	else
	{
		cmp = llvm_emit_call_intrinsic(c, intrinsic_id.vector_reduce_or, &bool_vec, 1, &cmp, 1);
	}
	llvm_value_set(be_value, cmp, type_bool);
}

static void llvm_emit_array_comp(GenContext *c, BEValue *be_value, BEValue *lhs, BEValue *rhs, BinaryOp binary_op)
{
	Type *array_base = type_flatten(lhs->type->array.base);
	switch (array_base->type_kind)
	{

		case ALL_INTS:
		case TYPE_POINTER:
		case TYPE_ENUM:
		case TYPE_FUNC_PTR:
		case TYPE_INTERFACE:
		case TYPE_ANY:
		case TYPE_ANYFAULT:
		case TYPE_FAULTTYPE:
		case TYPE_TYPEID:
MEMCMP:
			llvm_value_addr(c, lhs);
			llvm_value_addr(c, rhs);
			llvm_emit_memcmp(c, be_value, lhs->value, rhs->value, llvm_const_int(c, type_usz, type_size(lhs->type)));
			llvm_emit_int_comp_zero(c, be_value, be_value, binary_op);
			return;
		case TYPE_VECTOR:
			if (is_power_of_two(array_base->array.len)) goto MEMCMP;
			break;
		case TYPE_UNION:
		case TYPE_STRUCT:
		case TYPE_BITSTRUCT:
			if (array_base->decl->attr_compact) goto MEMCMP;
			break;
		case TYPE_POISONED:
		case TYPE_VOID:
		case TYPE_DISTINCT:
		case TYPE_FUNC_RAW:
		case TYPE_TYPEDEF:
		case TYPE_INFERRED_ARRAY:
		case TYPE_INFERRED_VECTOR:
		case TYPE_UNTYPED_LIST:
		case TYPE_OPTIONAL:
		case TYPE_WILDCARD:
		case TYPE_TYPEINFO:
		case TYPE_MEMBER:
			UNREACHABLE
		case TYPE_BOOL:
		case ALL_FLOATS:
		case TYPE_SLICE:
		case TYPE_ARRAY:
		case TYPE_FLEXIBLE_ARRAY:
			break;
	}

	bool want_match = binary_op == BINARYOP_EQ;
	ArraySize len = lhs->type->array.len;
	Type *array_base_type = type_lowering(array_base);
	LLVMTypeRef array_type = llvm_get_type(c, lhs->type);
	if (should_inline_array_comp(len, array_base_type))
	{
		if (array_base_type == type_bool)
		{
			llvm_emit_bool_vector_compare(c, be_value, lhs, rhs, binary_op, len);
			return;
		}
		if (type_is_float(array_base_type))
		{
			llvm_emit_fp_vector_compare(c, be_value, lhs, rhs, binary_op, array_base_type, len);
			return;
		}
		LLVMBasicBlockRef blocks[17];
		LLVMValueRef value_block[17];
		LLVMBasicBlockRef ok_block = llvm_basic_block_new(c, "match");
		LLVMBasicBlockRef exit_block = llvm_basic_block_new(c, "exit");
		llvm_value_addr(c, lhs);
		llvm_value_addr(c, rhs);
		LLVMValueRef success = LLVMConstInt(c->bool_type, want_match ? 1 : 0, false);
		LLVMValueRef failure = LLVMConstInt(c->bool_type, want_match ? 0 : 1, false);

		for (unsigned i = 0; i < len; i++)
		{
			value_block[i] = failure;
			AlignSize align_lhs;
			BEValue lhs_v;
			LLVMValueRef lhs_ptr = llvm_emit_array_gep_raw(c, lhs->value, array_type, i, lhs->alignment, &align_lhs);
			llvm_value_set_address(&lhs_v, lhs_ptr, array_base_type, align_lhs);
			AlignSize align_rhs;
			BEValue rhs_v;
			LLVMValueRef rhs_ptr = llvm_emit_array_gep_raw(c, rhs->value, array_type, i, rhs->alignment, &align_rhs);
			llvm_value_set_address(&rhs_v, rhs_ptr, array_base_type, align_rhs);
			BEValue comp;
			llvm_emit_comp(c, &comp, &lhs_v, &rhs_v, BINARYOP_EQ);
			blocks[i] = c->current_block;
			LLVMBasicBlockRef block = ok_block;
			block = i < len - 1 ? llvm_basic_block_new(c, "next_check") : block;
			llvm_emit_cond_br(c, &comp, block, exit_block);
			llvm_emit_block(c, block);
		}
		llvm_emit_br(c, exit_block);
		llvm_emit_block(c, exit_block);
		value_block[len] = success;
		blocks[len] = ok_block;
		LLVMValueRef phi = LLVMBuildPhi(c->builder, c->bool_type, "array_cmp_phi");
		LLVMAddIncoming(phi, value_block, blocks, len + 1);
		llvm_value_set(be_value, phi, type_bool);
		return;
	}

	LLVMBasicBlockRef exit = llvm_basic_block_new(c, "array_cmp_exit");
	LLVMBasicBlockRef loop_begin = llvm_basic_block_new(c, "array_loop_start");
	LLVMBasicBlockRef comparison = llvm_basic_block_new(c, "array_loop_comparison");
	LLVMBasicBlockRef comparison_phi;
	LLVMBasicBlockRef loop_begin_phi;
	LLVMValueRef len_val = llvm_const_int(c, type_isz, len);
	LLVMValueRef one = llvm_const_int(c, type_isz, 1);
	BEValue index_var;
	llvm_value_set_address_abi_aligned(&index_var, llvm_emit_alloca_aligned(c, type_isz, "cmp.idx"), type_isz);
	llvm_store_raw(c, &index_var, llvm_get_zero(c, type_isz));

	llvm_emit_br(c, loop_begin);
	llvm_emit_block(c, loop_begin);

	AlignSize align_lhs;
	BEValue lhs_v;
	BEValue index_copy = index_var;
	llvm_value_rvalue(c, &index_copy);
	LLVMValueRef lhs_ptr = llvm_emit_array_gep_raw_index(c, lhs->value, array_type, &index_copy, lhs->alignment, &align_lhs);
	llvm_value_set_address(&lhs_v, lhs_ptr, array_base_type, align_lhs);
	AlignSize align_rhs;
	BEValue rhs_v;
	LLVMValueRef rhs_ptr = llvm_emit_array_gep_raw_index(c, rhs->value, array_type, &index_copy, rhs->alignment, &align_rhs);
	llvm_value_set_address(&rhs_v, rhs_ptr, array_base_type, align_rhs);
	BEValue comp;
	llvm_emit_comp(c, &comp, &lhs_v, &rhs_v, BINARYOP_EQ);
	loop_begin_phi = c->current_block;
	llvm_emit_cond_br(c, &comp, comparison, exit);
	llvm_emit_block(c, comparison);

	LLVMValueRef new_index = LLVMBuildAdd(c->builder, index_copy.value, one, "inc");
	llvm_store_raw(c, &index_var, new_index);
	llvm_emit_int_comp_raw(c, &comp, type_isz, type_isz, new_index, len_val, BINARYOP_LT);
	comparison_phi = c->current_block;
	llvm_emit_cond_br(c, &comp, loop_begin, exit);
	llvm_emit_block(c, exit);
	LLVMValueRef success = LLVMConstInt(c->bool_type, want_match ? 1 : 0, false);
	LLVMValueRef failure = LLVMConstInt(c->bool_type, want_match ? 0 : 1, false);
	llvm_new_phi(c, be_value, "array_cmp_phi", type_bool, success, comparison_phi, failure, loop_begin_phi);
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
		llvm_convert_vector_comparison(c, be_value, val, vector_type, BINARYOP_EQ == binary_op);
		return;
	}
	llvm_value_set(be_value, val, type_bool);
}

void llvm_emit_lhs_is_subtype(GenContext *c, BEValue *result, BEValue *lhs, BEValue *rhs)
{
	Type *canonical_typeid = type_lowering(type_typeid);
	llvm_value_rvalue(c, lhs);
	llvm_value_rvalue(c, rhs);
	LLVMValueRef switch_val = lhs->value;
	LLVMBasicBlockRef start_block = c->current_block;
	LLVMBasicBlockRef retry_block = llvm_basic_block_new(c, "check_subtype");
	LLVMBasicBlockRef result_block = llvm_basic_block_new(c, "result_block");
	LLVMBasicBlockRef parent_type_block = llvm_basic_block_new(c, "parent_type_block");
	llvm_emit_br(c, retry_block);
	llvm_emit_block(c, retry_block);
	LLVMValueRef phi = LLVMBuildPhi(c->builder, c->typeid_type, "");
	BEValue cond;
	llvm_emit_int_comp_raw(c, &cond, canonical_typeid, canonical_typeid, switch_val, phi, BINARYOP_EQ);
	llvm_emit_cond_br(c, &cond, result_block, parent_type_block);
	llvm_emit_block(c, parent_type_block);
	LLVMValueRef introspect_ptr = LLVMBuildIntToPtr(c->builder, phi, c->ptr_type, "");
	AlignSize alignment;
	LLVMValueRef parent = llvm_emit_struct_gep_raw(c, introspect_ptr, c->introspect_type, INTROSPECT_INDEX_PARENTOF,
	                                               type_abi_alignment(type_voidptr), &alignment);
	LLVMValueRef parent_value = llvm_load(c, c->typeid_type, parent, alignment, "typeid.parent");
	LLVMValueRef is_zero = LLVMBuildICmp(c->builder, LLVMIntEQ, parent_value, LLVMConstNull(c->typeid_type), "");
	llvm_emit_cond_br_raw(c, is_zero, result_block, retry_block);
	llvm_set_phi(phi, rhs->value, start_block, parent_value, parent_type_block);
	llvm_emit_block(c, result_block);
	llvm_new_phi(c, result, "", type_bool, LLVMConstNull(c->bool_type), parent_type_block, LLVMConstAllOnes(c->bool_type), retry_block);
}

void llvm_emit_comp(GenContext *c, BEValue *result, BEValue *lhs, BEValue *rhs, BinaryOp binary_op)
{
	assert(type_lowering(lhs->type) == lhs->type);
	assert(binary_op >= BINARYOP_GT && binary_op <= BINARYOP_EQ);
	switch (lhs->type->type_kind)
	{
		case TYPE_VOID:
			UNREACHABLE;
		case TYPE_BOOL:
		case ALL_INTS:
			llvm_value_rvalue(c, lhs);
			llvm_value_rvalue(c, rhs);
			llvm_emit_int_comp_raw(c, result, lhs->type, rhs->type, lhs->value, rhs->value, binary_op);
			return;
		case ALL_FLOATS:
			llvm_emit_float_comp(c, result, lhs, rhs, binary_op, NULL);
			return;
		case TYPE_POINTER:
		case TYPE_FUNC_PTR:
			llvm_emit_ptr_comparison(c, result, lhs, rhs, binary_op);
			return;
		case TYPE_ARRAY:
			llvm_emit_array_comp(c, result, lhs, rhs, binary_op);
			return;
		case TYPE_FUNC_RAW:
			break;
		case TYPE_ANY:
			llvm_emit_any_comparison(c, result, lhs, rhs, binary_op);
			return;
		case LOWERED_TYPES:
		case TYPE_FLEXIBLE_ARRAY:
			UNREACHABLE
		case TYPE_STRUCT:
		case TYPE_UNION:
			llvm_emit_struct_comparison(c, result, lhs, rhs, binary_op);
			return;
		case TYPE_SLICE:
			llvm_emit_slice_comp(c, result, lhs, rhs, binary_op);
			return;
		case TYPE_VECTOR:
			if (type_is_float(type_vector_type(lhs->type)))
			{
				llvm_emit_float_comp(c, result, lhs, rhs, binary_op, lhs->type);
			}
			else
			{
				llvm_value_rvalue(c, lhs);
				llvm_value_rvalue(c, rhs);
				llvm_emit_int_comp_raw(c, result, lhs->type, rhs->type, lhs->value, rhs->value, binary_op);
			}
			return;
	}
	TODO // When updated, also update tilde_codegen_expr
}

static void llvm_emit_else(GenContext *c, BEValue *be_value, Expr *expr)
{
	LLVMBasicBlockRef else_block = llvm_basic_block_new(c, "else_block");
	LLVMBasicBlockRef phi_block = llvm_basic_block_new(c, "phi_block");

	// Store catch/opt var
	PUSH_CATCH_VAR_BLOCK(NULL, else_block);

	// Fold the real value, this will cause an implicit jump to the else block on failure.
	BEValue real_value;
	llvm_emit_exprid(c, &real_value, expr->binary_expr.left);
	bool was_address = llvm_value_is_addr(&real_value);
	llvm_value_fold_optional(c, &real_value);
	if (was_address && !llvm_temp_as_address(c, real_value.type))
	{
		was_address = false;
		llvm_value_rvalue(c, &real_value);
	}

	// Restore.
	POP_CATCH();

	// Emit success and jump to phi.
	LLVMBasicBlockRef success_end_block = llvm_get_current_block_if_in_use(c);

	// Only jump to phi if we didn't have an immediate jump. That would
	// for example happen on "{| defer foo(); return Foo.ERR?; |} ?? 123"
	if (success_end_block) llvm_emit_br(c, phi_block);

	// Emit else
	llvm_emit_block(c, else_block);

	// Emit the value here
	BEValue else_value;
	llvm_emit_exprid(c, &else_value, expr->binary_expr.right);
	llvm_value_fold_optional(c, &else_value);

	// If else is a non-void, then fold as needed
	if (else_value.type != type_void)
	{
		if (was_address)
		{
			llvm_value_addr(c, &else_value);
		}
		else
		{
			llvm_value_rvalue(c, &else_value);
		}
	}

	LLVMBasicBlockRef else_block_exit = llvm_get_current_block_if_in_use(c);

	// While the value may not be an optional, we may get a jump
	// from this construction: foo() ?? (bar()?)
	// In this case the else block is empty.
	if (!else_block_exit)
	{
		llvm_emit_block(c, phi_block);
		*be_value = real_value;
		return;
	}

	llvm_emit_br(c, phi_block);
	llvm_emit_block(c, phi_block);

	// Was there never a success, if so the result is the be_value.
	if (!success_end_block)
	{
		*be_value = else_value;
		return;
	}

	// Emit an address if the phi is was by address
	if (was_address)
	{
		llvm_new_phi(c, be_value, "val", type_get_ptr(else_value.type), real_value.value, success_end_block, else_value.value, else_block_exit);
		be_value->kind = BE_ADDRESS;
		be_value->type = else_value.type;
		return;
	}

	llvm_new_phi(c, be_value, "val", expr->type, real_value.value, success_end_block, else_value.value, else_block_exit);
}

typedef enum
{
	FMUL_NONE,
	FMUL_LHS_MULT,
	FMUL_LHS_NEG_MULT,
	FMUL_RHS_MULT,
	FMUL_RHS_NEG_MULT
} FmulTransformation;

INLINE FmulTransformation llvm_get_fmul_transformation(Expr *lhs, Expr *rhs)
{
	if (compiler.build.feature.fp_math <= FP_STRICT) return FMUL_NONE;
	// x * y + z
	if (expr_is_mult(lhs)) return FMUL_LHS_MULT;
	// -(x * y) + z
	if (expr_is_neg(lhs) && expr_is_mult(lhs->unary_expr.expr)) return FMUL_LHS_NEG_MULT;
	// x + y * z
	if (expr_is_mult(rhs)) return FMUL_RHS_MULT;
	// x - (y * z)
	if (expr_is_neg(rhs) && expr_is_mult(rhs->unary_expr.expr)) return FMUL_RHS_NEG_MULT;
	return FMUL_NONE;
}
INLINE bool llvm_emit_fmuladd_maybe(GenContext *c, BEValue *be_value, Expr *expr, BinaryOp op)
{
	Expr *lhs = exprptr(expr->binary_expr.left);
	Expr *rhs = exprptr(expr->binary_expr.right);
	FmulTransformation transformation = llvm_get_fmul_transformation(lhs, rhs);
	bool negate_rhs = op == BINARYOP_SUB;
	bool negate_result = false;
	if (negate_rhs && transformation == FMUL_RHS_NEG_MULT)
	{
		negate_rhs = false;
		rhs = rhs->unary_expr.expr;
		transformation = FMUL_RHS_MULT;
	}
	LLVMValueRef args[3];
	switch (transformation)
	{
		case FMUL_NONE:
			return false;
		case FMUL_LHS_MULT:
			args[0] = llvm_emit_exprid_to_rvalue(c, lhs->binary_expr.left);
			args[1] = llvm_emit_exprid_to_rvalue(c, lhs->binary_expr.right);
			args[2] = llvm_emit_expr_to_rvalue(c, rhs);
			if (negate_rhs) args[2] = LLVMBuildFNeg(c->builder, args[2], "");
			break;
		case FMUL_LHS_NEG_MULT:
			lhs = lhs->unary_expr.expr;
			args[0] = llvm_emit_exprid_to_rvalue(c, lhs->binary_expr.left);
			args[1] = llvm_emit_exprid_to_rvalue(c, lhs->binary_expr.right);
			if (expr_is_neg(rhs))
			{
				args[2] = llvm_emit_expr_to_rvalue(c, negate_rhs ? rhs : rhs->unary_expr.expr);
			}
			else
			{
				args[2] = llvm_emit_expr_to_rvalue(c, rhs);
				if (!negate_rhs) args[2] = LLVMBuildFNeg(c->builder, args[2], "");
			}
			negate_result = true;
			break;
		case FMUL_RHS_MULT:
			args[2] = llvm_emit_expr_to_rvalue(c, lhs);
			args[0] = llvm_emit_exprid_to_rvalue(c, rhs->binary_expr.left);
			args[1] = llvm_emit_exprid_to_rvalue(c, rhs->binary_expr.right);
			if (negate_rhs)
			{
				args[1] = LLVMBuildFNeg(c->builder, args[1], "");
				negate_result = false;
			}
			break;
		case FMUL_RHS_NEG_MULT:
			rhs = rhs->unary_expr.expr;
			assert(!negate_rhs);
			args[0] = llvm_emit_exprid_to_rvalue(c, rhs->binary_expr.left);
			args[1] = llvm_emit_exprid_to_rvalue(c, rhs->binary_expr.right);

			if (expr_is_neg(lhs))
			{
				// -x - (y * z) => -(x + y * z)
				args[2] = llvm_emit_expr_to_rvalue(c, lhs->unary_expr.expr);
				negate_result = true;
			}
			else
			{
				// x - (y * z) => x + (-y) * z
				args[1] = LLVMBuildFNeg(c->builder, args[1], "");
				args[2] = llvm_emit_expr_to_rvalue(c, lhs->unary_expr.expr);
			}
			break;
		default:
			UNREACHABLE
	}
	LLVMTypeRef call_type[1] = { LLVMTypeOf(args[0]) };
	LLVMValueRef result = llvm_emit_call_intrinsic(c, intrinsic_id.fmuladd, call_type, 1, args, 3);
	if (negate_result)
	{
		result = LLVMBuildFNeg(c->builder, result, "");
	}
	llvm_value_set(be_value, result, expr->type);
	return true;
}


void llvm_emit_bitstruct_binary_op(GenContext *c, BEValue *be_value, BEValue *lhs, BEValue *rhs, BinaryOp binary_op)
{
	llvm_value_addr(c, lhs);
	llvm_value_addr(c, rhs);
	LLVMTypeRef big_int = LLVMIntTypeInContext(c->context, type_size(lhs->type) * 8);
	LLVMValueRef l = llvm_load(c, big_int, lhs->value, lhs->alignment, "");
	LLVMValueRef r = llvm_load(c, big_int, rhs->value, rhs->alignment, "");
	LLVMValueRef val;
	switch (binary_op)
	{
		case BINARYOP_BIT_AND:
			val = LLVMBuildAnd(c->builder, l, r, "and");
			break;
		case BINARYOP_BIT_OR:
			val = LLVMBuildOr(c->builder, l, r, "or");
			break;
		case BINARYOP_BIT_XOR:
			val = LLVMBuildXor(c->builder, l, r, "xor");
			break;
		default:
			UNREACHABLE
	}
	LLVMValueRef store = llvm_emit_alloca(c, big_int, lhs->alignment, "");
	llvm_store_to_ptr_raw_aligned(c, store, val, lhs->alignment);
	llvm_value_set_address(be_value, store, lhs->type, lhs->alignment);
}

INLINE void llvm_fold_for_compare(GenContext *c, BEValue *be_value)
{
	switch (be_value->type->type_kind)
	{
		case TYPE_ARRAY:
		case TYPE_STRUCT:
		case TYPE_UNION:
			break;
		default:
			llvm_value_rvalue(c, be_value);
			break;
	}
}
void llvm_emit_binary(GenContext *c, BEValue *be_value, Expr *expr, BEValue *lhs_loaded, BinaryOp binary_op)
{
	// foo ?? bar
	if (binary_op == BINARYOP_ELSE)
	{
		llvm_emit_else(c, be_value, expr);
		return;
	}

	// foo || bar and foo && bar
	if (binary_op == BINARYOP_AND || binary_op == BINARYOP_OR)
	{
		llvm_emit_logical_and_or(c, be_value, expr, binary_op);
		return;
	}

	// Load if needed, otherwise use the already loaded.
	BEValue lhs;
	if (lhs_loaded)
	{
		lhs = *lhs_loaded;
	}
	else
	{
		if (type_is_float(type_flatten(expr->type)) && (binary_op == BINARYOP_ADD || binary_op == BINARYOP_SUB))
		{
			if (llvm_emit_fmuladd_maybe(c, be_value, expr, binary_op)) return;
		}
		llvm_emit_expr(c, &lhs, exprptr(expr->binary_expr.left));
	}
	// We need the rvalue.
	llvm_fold_for_compare(c, &lhs);

	// Evaluate rhs
	BEValue rhs;
	llvm_emit_expr(c, &rhs, exprptr(expr->binary_expr.right));
	llvm_fold_for_compare(c, &rhs);
	EMIT_LOC(c, expr);
	// Comparison <=>
	if (binary_op >= BINARYOP_GT && binary_op <= BINARYOP_EQ)
	{
		llvm_emit_comp(c, be_value, &lhs, &rhs, binary_op);
		return;
	}

	Type *lhs_type = lhs.type;
	Type *rhs_type = rhs.type;
	Type *vector_type = lhs_type->type_kind == TYPE_VECTOR ? lhs_type->array.base : NULL;
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
			val = llvm_emit_mult_int(c, lhs_type, lhs_value, rhs_value, expr->span);
			break;
		case BINARYOP_SUB:
			if (type_is_pointer_vector(lhs_type))
			{
				Type *element_type = lhs_type->array.base->pointer;
				unsigned len = lhs_type->array.len;
				LLVMTypeRef int_vec_type = llvm_get_type(c, type_get_vector(type_isz, len));
				if (lhs_type == rhs_type)
				{
					val = LLVMBuildSub(c->builder, LLVMBuildPtrToInt(c->builder, lhs_value, int_vec_type, ""),
					                   LLVMBuildPtrToInt(c->builder, rhs_value, int_vec_type, ""), "");
					LLVMValueRef divisor = llvm_emit_const_vector(llvm_const_int(c, type_isz, type_size(element_type)), len);
					val = LLVMBuildExactSDiv(c->builder, val, divisor, "");
					break;
				}
				rhs_value = LLVMBuildNeg(c->builder, rhs_value, "");
				val = llvm_emit_pointer_gep_raw(c, llvm_get_type(c, element_type), lhs_value, rhs_value);
				break;
			}
			else if (lhs_type->type_kind == TYPE_POINTER)
			{
				if (lhs_type == rhs_type)
				{
					LLVMTypeRef int_type = llvm_get_type(c, type_isz);
					val = LLVMBuildSub(c->builder, LLVMBuildPtrToInt(c->builder, lhs_value, int_type, ""),
									   LLVMBuildPtrToInt(c->builder, rhs_value, int_type, ""), "");
					val = LLVMBuildExactSDiv(c->builder, val, llvm_const_int(c, type_isz, type_size(lhs_type->pointer)), "");
					break;
				}
				rhs_value = LLVMBuildNeg(c->builder, rhs_value, "");
				val = llvm_emit_pointer_gep_raw(c, llvm_get_pointee_type(c, lhs_type), lhs_value, rhs_value);
				break;
			}
			if (is_float)
			{
				val = LLVMBuildFSub(c->builder, lhs_value, rhs_value, "fsub");
				break;
			}
			val = llvm_emit_sub_int(c, lhs_type, lhs_value, rhs_value, expr->span);
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
			val = llvm_emit_add_int(c, lhs_type, lhs_value, rhs_value, expr->span);
			break;
		case BINARYOP_DIV:
			llvm_emit_trap_zero(c, rhs_type, rhs_value, "Division by zero.", expr->span);
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
			llvm_emit_trap_zero(c, rhs_type, rhs_value, "% by zero.", expr->span);
			if (type_is_float(lhs_type))
			{
				val = LLVMBuildFRem(c->builder, lhs_value, rhs_value, "fmod");
				break;
			}
			val = type_is_unsigned(lhs_type)
				  ? LLVMBuildURem(c->builder, lhs_value, rhs_value, "umod")
				  : LLVMBuildSRem(c->builder, lhs_value, rhs_value, "smod");
			break;
		case BINARYOP_SHR:
			rhs_value = llvm_zext_trunc(c, rhs_value, LLVMTypeOf(lhs_value));
			llvm_emit_trap_invalid_shift(c, rhs_value, lhs_type, "Shift amount out of range (was %s).", expr->span);
			val = type_is_unsigned(lhs_type)
				  ? LLVMBuildLShr(c->builder, lhs_value, rhs_value, "lshr")
				  : LLVMBuildAShr(c->builder, lhs_value, rhs_value, "ashr");
			val = LLVMBuildFreeze(c->builder, val, "");
			break;
		case BINARYOP_SHL:
			rhs_value = llvm_zext_trunc(c, rhs_value, LLVMTypeOf(lhs_value));
			llvm_emit_trap_invalid_shift(c, rhs_value, lhs_type, "Shift amount out of range (was %s).", expr->span);
			val = LLVMBuildShl(c->builder, lhs_value, rhs_value, "shl");
			val = LLVMBuildFreeze(c->builder, val, "");
			break;
		case BINARYOP_BIT_AND:
			if (lhs.type->type_kind == TYPE_ARRAY)
			{
				llvm_emit_bitstruct_binary_op(c, be_value, &lhs, &rhs, binary_op);
				return;
			}
			val = LLVMBuildAnd(c->builder, lhs_value, rhs_value, "and");
			break;
		case BINARYOP_BIT_OR:
			if (lhs.type->type_kind == TYPE_ARRAY)
			{
				llvm_emit_bitstruct_binary_op(c, be_value, &lhs, &rhs, binary_op);
				return;
			}
			val = LLVMBuildOr(c->builder, lhs_value, rhs_value, "or");
			break;
		case BINARYOP_BIT_XOR:
			if (lhs.type->type_kind == TYPE_ARRAY)
			{
				llvm_emit_bitstruct_binary_op(c, be_value, &lhs, &rhs, binary_op);
				return;
			}
			val = LLVMBuildXor(c->builder, lhs_value, rhs_value, "xor");
			break;
		case BINARYOP_ELSE:
		case BINARYOP_EQ:
		case BINARYOP_NE:
		case BINARYOP_GE:
		case BINARYOP_GT:
		case BINARYOP_LE:
		case BINARYOP_LT:
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
		case BINARYOP_CT_AND:
		case BINARYOP_CT_OR:
		case BINARYOP_CT_CONCAT:
			// Handled elsewhere.
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
	                       expr->unary_expr.operator == UNARYOP_INC ? 1 : -1, !expr->unary_expr.no_wrap);
}

void llvm_emit_typeid(GenContext *c, BEValue *be_value, Type *type)
{
	LLVMValueRef value;
	type = type->canonical;
	llvm_value_set(be_value, llvm_get_typeid(c, type), type_typeid);
}

void llvm_emit_try_assign_try_catch(GenContext *c, bool is_try, BEValue *be_value, BEValue *var_addr, BEValue *catch_addr, Expr *rhs)
{
	assert(!catch_addr || llvm_value_is_addr(catch_addr));
	assert(!var_addr || llvm_value_is_addr(var_addr));

	// 1.  Create after try/catch block
	LLVMBasicBlockRef catch_block = llvm_basic_block_new(c, "catch_landing");
	LLVMBasicBlockRef phi_catch = llvm_basic_block_new(c, "phi_try_catch");

	// 2. If we have a catch *and* we want to store it, set the catch variable
	LLVMValueRef catch_var = catch_addr ? catch_addr->value : NULL;

	// 3. After catch, we want to end up in the landing, because otherwise we don't know the value for the phi.
	PUSH_CATCH_VAR_BLOCK(catch_var, catch_block);

	// 5. Emit the init part.
	llvm_emit_expr(c, be_value, rhs);

	// 6. If we haven't jumped yet, do it here (on error) to the catch block.
	llvm_value_fold_optional(c, be_value);

	// 7. If we have a variable, then we make the store.
	if (var_addr)
	{
		assert(is_try && "Storing will only happen on try.");
		llvm_store(c, var_addr, be_value);
	}

	// 8. Restore the error stack.
	POP_CATCH();

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
	LLVMValueRef from_try = LLVMConstInt(c->bool_type, is_try ? 1 : 0, false);
	LLVMValueRef from_catch = LLVMConstInt(c->bool_type, is_try ? 0 : 1, false);
	llvm_new_phi(c, be_value, "val", type_bool, from_try, success_block, from_catch, catch_block);
}


/**
 * This is the foo! instruction.
 */
static inline void llvm_emit_rethrow_expr(GenContext *c, BEValue *be_value, Expr *expr)
{
	LLVMBasicBlockRef guard_block = llvm_basic_block_new(c, "guard_block");
	LLVMBasicBlockRef no_err_block = llvm_basic_block_new(c, "noerr_block");

	// Set the catch/error var
	LLVMValueRef error_var = llvm_emit_alloca_aligned(c, type_anyfault, "error_var");

	PUSH_CATCH_VAR_BLOCK(error_var, guard_block);

	llvm_emit_expr(c, be_value, expr->rethrow_expr.inner);

	// Fold the optional.
	llvm_value_fold_optional(c, be_value);

	// Restore.
	POP_CATCH();

	// Emit success and to end.
	llvm_emit_br(c, no_err_block);

	// Emit else
	llvm_emit_block(c, guard_block);

	// Ensure we are on a branch that is non-empty.
	if (llvm_emit_check_block_branch(c))
	{
		PUSH_DEFER_ERROR(error_var);
		llvm_emit_statement_chain(c, expr->rethrow_expr.cleanup);
		POP_DEFER_ERROR();
		BEValue value;
		llvm_value_set_address_abi_aligned(&value, error_var, type_anyfault);
		if (expr->rethrow_expr.in_block)
		{
			BlockExit *exit = *expr->rethrow_expr.in_block;
			if (exit->block_error_var)
			{
				llvm_store_to_ptr(c, exit->block_error_var, &value);
			}
			llvm_emit_br(c, exit->block_optional_exit);
		}
		else
		{
			llvm_emit_return_abi(c, NULL, &value);
		}
	}

	llvm_emit_block(c, no_err_block);

}


/**
 * This is the foo? instruction.
 */
static inline void llvm_emit_force_unwrap_expr(GenContext *c, BEValue *be_value, Expr *expr)
{
	LLVMBasicBlockRef panic_block = llvm_basic_block_new(c, "panic_block");
	LLVMBasicBlockRef no_err_block = llvm_basic_block_new(c, "noerr_block");

	// Set the catch/error var
	LLVMValueRef error_var = llvm_emit_alloca_aligned(c, type_anyfault, "error_var");

	PUSH_CATCH_VAR_BLOCK(error_var, panic_block);

	llvm_emit_expr(c, be_value, expr->inner_expr);
	llvm_value_fold_optional(c, be_value);

	// Restore.
	POP_CATCH();

	// Emit success and to end.
	llvm_emit_br(c, no_err_block);

	POP_CATCH();

	// Emit panic
	llvm_emit_block(c, panic_block);

	// Ensure we are on a branch that is non-empty.
	if (llvm_emit_check_block_branch(c))
	{
		// TODO, we should add info about the error.
		SourceSpan loc = expr->span;
		BEValue *varargs = NULL;
		BEValue fault_arg;
		llvm_value_set_address(&fault_arg, error_var, type_anyfault, type_abi_alignment(type_anyfault));
		llvm_emit_any_from_value(c, &fault_arg, type_anyfault);
		vec_add(varargs, fault_arg);
		llvm_emit_panic(c, "Force unwrap failed!", loc, "Unexpected fault '%s' was unwrapped!", varargs);
	}
	llvm_emit_block(c, no_err_block);
	EMIT_LOC(c, expr);
}


static void llvm_emit_vector_assign_expr(GenContext *c, BEValue *be_value, Expr *expr)
{
	Expr *left = exprptr(expr->binary_expr.left);
	BinaryOp binary_op = expr->binary_expr.operator;
	BEValue addr;
	BEValue index;

	// Emit the variable
	llvm_emit_exprid(c, &addr, left->subscript_expr.expr);
	llvm_value_addr(c, &addr);
	LLVMValueRef vector_value = llvm_load_value_store(c, &addr);

	// Emit the index
	llvm_emit_exprid(c, &index, left->subscript_expr.index.expr);
	LLVMValueRef index_val = llvm_load_value_store(c, &index);

	if (binary_op > BINARYOP_ASSIGN)
	{
		BinaryOp base_op = binaryop_assign_base_op(binary_op);
		assert(base_op != BINARYOP_ERROR);
		BEValue lhs;
		llvm_value_set(&lhs, LLVMBuildExtractElement(c->builder, vector_value, index_val, "elem"), expr->type);
		llvm_emit_binary(c, be_value, expr, &lhs, base_op);
	}
	else
	{
		llvm_emit_expr(c, be_value, exprptr(expr->binary_expr.right));
	}

	LLVMValueRef new_value = LLVMBuildInsertElement(c->builder, vector_value, llvm_load_value_store(c, be_value), index_val, "elemset");
	llvm_store_raw(c, &addr, new_value);
}

static void llvm_emit_binary_expr(GenContext *c, BEValue *be_value, Expr *expr)
{
	BinaryOp binary_op = expr->binary_expr.operator;
	// Vector assign is handled separately.
	if (binary_op >= BINARYOP_ASSIGN && expr_is_vector_index(exprptr(expr->binary_expr.left)))
	{
		llvm_emit_vector_assign_expr(c, be_value, expr);
		return;
	}

	// Operation + assign
	if (binary_op > BINARYOP_ASSIGN)
	{
		// Finde the base op.
		BinaryOp base_op = binaryop_assign_base_op(binary_op);
		assert(base_op != BINARYOP_ERROR);

		// Get the left hand side, which must be an address.
		BEValue addr;
		llvm_emit_expr(c, &addr, exprptr(expr->binary_expr.left));
		assert(llvm_value_is_addr(&addr));

		// Fold the optional.
		llvm_value_fold_optional(c, &addr);

		// Perform the binary operation, using the already loaded LHS.
		llvm_emit_binary(c, be_value, expr, &addr, base_op);

		// Store it.
		llvm_store(c, &addr, be_value);
		return;
	}

	if (binary_op == BINARYOP_ASSIGN)
	{
		Expr *left = exprptr(expr->binary_expr.left);
		llvm_emit_expr(c, be_value, left);
		assert(llvm_value_is_addr(be_value));
		LLVMValueRef optional_ref = NULL;

		// If the LHS is an identifier, then we're assigning the optional value to that.
		if (left->expr_kind == EXPR_IDENTIFIER)
		{
			optional_ref = decl_optional_ref(left->identifier_expr.decl);
			be_value->kind = BE_ADDRESS;
		}

		// Emit the result.
		*be_value = llvm_emit_assign_expr(c, be_value, exprptr(expr->binary_expr.right), optional_ref, false);
		return;
	}

	// Emit binary, LHS is not loaded.
	llvm_emit_binary(c, be_value, expr, NULL, binary_op);
}

static inline void llvm_emit_elvis_expr(GenContext *c, BEValue *value, Expr *expr)
{

	// Generate condition and conditional branch
	Expr *cond = exprptr(expr->ternary_expr.cond);
	llvm_emit_expr(c, value, cond);
	llvm_value_rvalue(c, value);

	LLVMValueRef lhs_value = value->value;
	if (value->kind != BE_BOOLEAN)
	{
		CastKind cast = cast_to_bool_kind(value->type);
		llvm_emit_cast(c, cast, cond, value, type_bool, value->type);
	}

	Expr *else_expr = exprptr(expr->ternary_expr.else_expr);
	if (!IS_OPTIONAL(expr) && expr_is_const(else_expr))
	{
		BEValue right;
		llvm_emit_expr(c, &right, else_expr);
		llvm_value_rvalue(c, &right);
		LLVMValueRef val = LLVMBuildSelect(c->builder, value->value, lhs_value, right.value, "elvis");
		llvm_value_set(value, val, right.type);
		return;
	}

	LLVMBasicBlockRef lhs_exit = llvm_get_current_block_if_in_use(c);
	if (!lhs_exit) return;

	// Set up basic blocks, following Cone
	LLVMBasicBlockRef phi_block = llvm_basic_block_new(c, "cond.phi");
	LLVMBasicBlockRef rhs_block = llvm_basic_block_new(c, "cond.rhs");

	llvm_emit_cond_br(c, value, phi_block, rhs_block);
	llvm_emit_block(c, rhs_block);
	BEValue rhs;
	llvm_emit_expr(c, &rhs, else_expr);
	llvm_value_rvalue(c, &rhs);
	LLVMValueRef rhs_value = rhs.value;
	if (rhs.type == type_bool && LLVMTypeOf(rhs_value) != c->bool_type)
	{
		llvm_emit_trunc_bool(c, rhs_value);
	}
	LLVMBasicBlockRef rhs_exit = llvm_get_current_block_if_in_use(c);
	if (rhs_exit) llvm_emit_br(c, phi_block);

	// Generate phi
	llvm_emit_block(c, phi_block);
	if (!rhs_exit)
	{
		if (!lhs_value) lhs_value = LLVMGetUndef(llvm_get_type(c, expr->type));
		llvm_value_set(value, lhs_value, expr->type);
		return;
	}
	if (!lhs_exit)
	{
		if (!rhs_value) rhs_value = LLVMGetUndef(llvm_get_type(c, expr->type));
		llvm_value_set(value, rhs_value, expr->type);
		return;
	}
	llvm_new_phi(c, value, "val", expr->type, lhs_value, lhs_exit, rhs_value, rhs_exit);
}

void gencontext_emit_ternary_expr(GenContext *c, BEValue *value, Expr *expr)
{
	if (!expr->ternary_expr.then_expr)
	{
		llvm_emit_elvis_expr(c, value, expr);
		return;
	}


	bool is_elvis = false;

	// Generate condition and conditional branch
	Expr *cond = exprptr(expr->ternary_expr.cond);
	llvm_emit_expr(c, value, cond);
	llvm_value_rvalue(c, value);

	LLVMValueRef lhs_value = is_elvis ? value->value : NULL;
	if (value->kind != BE_BOOLEAN)
	{
		assert(is_elvis);
		CastKind cast = cast_to_bool_kind(value->type);
		llvm_emit_cast(c, cast, cond, value, type_bool, value->type);
	}

	Expr *else_expr;
	Expr *then_expr;

	if (is_elvis)
	{
		then_expr = NULL;
		else_expr = exprptr(expr->ternary_expr.else_expr);
	}
	else
	{
		else_expr = exprptr(expr->ternary_expr.else_expr);
		then_expr = exprptr(expr->ternary_expr.then_expr);

	}

	if (!IS_OPTIONAL(expr) && expr_is_const(else_expr)
		&& (is_elvis || expr_is_const(then_expr)))
	{
		if (!lhs_value)
		{
			BEValue left;
			llvm_emit_expr(c, &left, then_expr);
			llvm_value_rvalue(c, &left);
			lhs_value = left.value;
		}
		BEValue right;
		llvm_emit_expr(c, &right, else_expr);
		llvm_value_rvalue(c, &right);
		LLVMValueRef val = LLVMBuildSelect(c->builder, value->value, lhs_value, right.value, "ternary");
		llvm_value_set(value, val, right.type);
		return;
	}

	// Set up basic blocks, following Cone
	LLVMBasicBlockRef phi_block = llvm_basic_block_new(c, "cond.phi");
	LLVMBasicBlockRef rhs_block = llvm_basic_block_new(c, "cond.rhs");

	LLVMBasicBlockRef lhs_exit;
	if (is_elvis)
	{
		lhs_exit = llvm_get_current_block_if_in_use(c);
		if (!lhs_exit) return;
		llvm_emit_cond_br(c, value, phi_block, rhs_block);
	}
	else
	{
		LLVMBasicBlockRef lhs_block = llvm_basic_block_new(c, "cond.lhs");
		llvm_emit_cond_br(c, value, lhs_block, rhs_block);
		llvm_emit_block(c, lhs_block);
		BEValue lhs;
		llvm_emit_expr(c, &lhs, then_expr);
		llvm_value_rvalue(c, &lhs);
		lhs_value = lhs.value;
		lhs_exit = llvm_get_current_block_if_in_use(c);
		if (lhs.type == type_bool && LLVMTypeOf(lhs_value) != c->bool_type)
		{
			llvm_emit_trunc_bool(c, lhs_value);
		}
		if (lhs_exit) llvm_emit_br(c, phi_block);
	}

	llvm_emit_block(c, rhs_block);
	BEValue rhs;
	llvm_emit_expr(c, &rhs, else_expr);
	llvm_value_rvalue(c, &rhs);
	LLVMValueRef rhs_value = rhs.value;
	if (rhs.type == type_bool && LLVMTypeOf(rhs_value) != c->bool_type)
	{
		llvm_emit_trunc_bool(c, rhs_value);
	}

	LLVMBasicBlockRef rhs_exit = llvm_get_current_block_if_in_use(c);
	if (rhs_exit) llvm_emit_br(c, phi_block);

	// Generate phi
	llvm_emit_block(c, phi_block);
	if (!rhs_exit)
	{
		if (!lhs_value) lhs_value = LLVMGetUndef(llvm_get_type(c, expr->type));
		llvm_value_set(value, lhs_value, expr->type);
		return;
	}

	if (!lhs_exit)
	{
		if (!rhs_value) rhs_value = LLVMGetUndef(llvm_get_type(c, expr->type));
		llvm_value_set(value, rhs_value, expr->type);
		return;
	}

	if (expr->type == type_void)
	{
		llvm_value_set(value, NULL, expr->type);
		return;
	}
	llvm_new_phi(c, value, "val", expr->type, lhs_value, lhs_exit, rhs_value, rhs_exit);
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
	scratch_buffer_printf("%a", f.f);
	return LLVMConstRealOfStringAndSize(type, scratch_buffer.str, scratch_buffer.len);
}

static inline void llvm_emit_const_initializer_list_expr(GenContext *c, BEValue *value, Expr *expr)
{
	if (llvm_is_global_eval(c) || type_flat_is_vector(expr->type) || type_flatten(expr->type)->type_kind == TYPE_BITSTRUCT)
	{
		assert(type_flatten(expr->type)->type_kind != TYPE_SLICE);
		llvm_value_set(value, llvm_emit_const_initializer(c, expr->const_expr.initializer), expr->type);
		return;
	}
	llvm_value_set_address_abi_aligned(value, llvm_emit_alloca_aligned(c, expr->type, "literal"), expr->type);
	llvm_emit_const_initialize_reference(c, value, expr);
}

static void llvm_emit_const_expr(GenContext *c, BEValue *be_value, Expr *expr)
{
	Type *type = type_reduced_from_expr(expr)->canonical;
	bool is_bytes = false;
	switch (expr->const_expr.const_kind)
	{
		case CONST_REF:
		{
			Decl *decl = expr->const_expr.global_ref;
			LLVMValueRef backend_ref = llvm_get_ref(c, decl);
			llvm_value_set(be_value, backend_ref, expr->type);
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
		case CONST_INITIALIZER:
			llvm_emit_const_initializer_list_expr(c, be_value, expr);
			return;
		case CONST_FLOAT:
			llvm_value_set(be_value, llvm_emit_real(llvm_get_type(c, type), expr->const_expr.fxx), type);
			return;
		case CONST_SLICE:
			if (!expr->const_expr.slice_init)
			{
				llvm_value_set(be_value, llvm_get_zero(c, type), type);
				return;
			}
			else
			{
				ConstInitializer *init = expr->const_expr.slice_init;
				if (llvm_is_global_eval(c) || type_flat_is_vector(expr->type) || type_flatten(expr->type)->type_kind == TYPE_BITSTRUCT)
				{
					LLVMValueRef value = llvm_emit_const_initializer(c, init);
					AlignSize alignment = type_alloca_alignment(init->type);
					LLVMTypeRef val_type = llvm_get_type(c, init->type);
					LLVMValueRef global_copy = llvm_add_global_raw(c, ".__const_slice", val_type, alignment);
					LLVMSetInitializer(global_copy, value);
					llvm_set_private_linkage(global_copy);
					LLVMSetUnnamedAddress(global_copy, LLVMGlobalUnnamedAddr);
					assert(type_is_arraylike(init->type));
					LLVMValueRef val = llvm_emit_aggregate_two(c, type, global_copy,
					                                           llvm_const_int(c, type_usz, init->type->array.len));
					llvm_value_set(be_value, val, type);
				}
				else
				{
					assert(type_is_arraylike(init->type));
					llvm_value_set_address_abi_aligned(be_value, llvm_emit_alloca_aligned(c, init->type, "literal"), init->type);
					llvm_emit_const_init_ref(c, be_value, init, true);
					LLVMValueRef val = llvm_emit_aggregate_two(c, type, be_value->value, llvm_const_int(c, type_usz, init->type->array.len));
					llvm_value_set(be_value, val, type);
				}
			}
			return;
		case CONST_POINTER:
			if (!expr->const_expr.ptr)
			{
				llvm_value_set(be_value, llvm_get_zero(c, type), type);
			}
			else
			{
				llvm_value_set(be_value, LLVMBuildIntToPtr(c->builder, llvm_const_int(c, type_uptr, expr->const_expr.ptr), llvm_get_type(c, type), ""), type);
			}
			return;
		case CONST_BOOL:
			llvm_value_set(be_value, LLVMConstInt(c->bool_type, expr->const_expr.b ? 1 : 0, 0), type_bool);
			return;
		case CONST_BYTES:
			is_bytes = true;
			FALLTHROUGH;
		case CONST_STRING:
		{
			Type *str_type = type_lowering(expr->type);
			bool is_array = str_type->type_kind == TYPE_ARRAY;
			if (is_array && llvm_is_global_eval(c))
			{
				// In the global alloc case, create the byte array.
				ArraySize array_len = str_type->array.len;
				ArraySize size = expr->const_expr.bytes.len;
				if (!is_bytes) size += 1;
				LLVMValueRef string;
				if (array_len == size)
				{
					string = is_bytes
							? llvm_get_bytes(c, expr->const_expr.bytes.ptr, size)
							: llvm_get_zstring(c, expr->const_expr.bytes.ptr, expr->const_expr.bytes.len);
				}
				else if (array_len < size)
				{
					string = llvm_get_bytes(c, expr->const_expr.bytes.ptr, array_len);
				}
				else
				{
					char *buffer = ccalloc(1, array_len);
					memcpy(buffer, expr->const_expr.bytes.ptr, expr->const_expr.bytes.len);
					string = llvm_get_bytes(c, buffer, array_len);
				}
				llvm_value_set(be_value, string, type);
				return;
			}
			// local case or creating a pointer / slice.
			// In this case we first create the constant.
			ArraySize len = expr->const_expr.bytes.len;
			if (len == 0 && str_type->type_kind == TYPE_SLICE)
			{
				llvm_value_set(be_value, llvm_get_zero(c, expr->type), expr->type);
				return;
			}
			ArraySize size = expr->const_expr.bytes.len;
			if (!is_bytes) size++;
			if (is_array && type->array.len > size) size = type->array.len;
			LLVMValueRef global_name = llvm_add_global_raw(c, is_bytes ? ".bytes" : ".str", LLVMArrayType(llvm_get_type(c, type_char), size), 1);
			llvm_set_private_linkage(global_name);
			LLVMSetUnnamedAddress(global_name, LLVMGlobalUnnamedAddr);
			LLVMSetGlobalConstant(global_name, 1);
			LLVMValueRef data = is_bytes
					? llvm_get_bytes(c, expr->const_expr.bytes.ptr, expr->const_expr.bytes.len)
					: llvm_get_zstring(c, expr->const_expr.bytes.ptr, expr->const_expr.bytes.len);
			LLVMValueRef trailing_zeros = NULL;
			if (is_bytes)
			{
				if (size > len)
				{
					trailing_zeros = llvm_get_zero_raw(LLVMArrayType(c->byte_type, size - len));
				}
			}
			else if (size > len + 1)
			{
				trailing_zeros = llvm_get_zero_raw(LLVMArrayType(c->byte_type, size - len - 1));
			}
			if (trailing_zeros)
			{
				LLVMValueRef values[2] = { data, trailing_zeros };
				data = llvm_get_packed_struct(c, values, 2);
			}
			LLVMSetInitializer(global_name, data);
			if (is_array)
			{
				llvm_value_set_address(be_value, global_name, type, 1);
			}
			else
			{
				if (str_type->type_kind == TYPE_SLICE)
				{
					LLVMValueRef len_value = llvm_const_int(c, type_usz, len);
					llvm_value_aggregate_two(c, be_value, str_type, global_name, len_value);
				}
				else
				{
					llvm_value_set(be_value, global_name, type);
				}
			}
			return;
		}
		case CONST_TYPEID:
			llvm_emit_typeid(c, be_value, expr->const_expr.typeid);
			return;
		case CONST_ERR:
		{
			Decl *decl = expr->const_expr.enum_err_val;
			assert(decl);
			LLVMValueRef value = LLVMBuildPtrToInt(c->builder, llvm_get_ref(c, decl), llvm_get_type(c, type_anyfault), "");
			llvm_value_set(be_value, value, type_anyfault);
			return;
		}
		case CONST_ENUM:
			llvm_value_set(be_value, llvm_const_int(c, type, expr->const_expr.enum_err_val->enum_constant.ordinal), type);
			return;
		case CONST_MEMBER:
		case CONST_UNTYPED_LIST:
			UNREACHABLE
	}
	UNREACHABLE
}




static void llvm_expand_array_to_args(GenContext *c, Type *param_type, LLVMValueRef expand_ptr, LLVMValueRef *args, unsigned *arg_count_ref, AlignSize alignment)
{
	LLVMTypeRef array_type = llvm_get_type(c, param_type);
	for (ByteSize i = 0; i < param_type->array.len; i++)
	{
		AlignSize load_align;
		LLVMValueRef element_ptr = llvm_emit_array_gep_raw(c, expand_ptr, array_type, (unsigned)i, alignment, &load_align);
		llvm_expand_type_to_args(c, param_type->array.base, element_ptr, args, arg_count_ref, load_align);
	}
}

static void llvm_expand_struct_to_args(GenContext *context, Type *param_type, LLVMValueRef expand_ptr, LLVMValueRef *args, unsigned *arg_count_ref, AlignSize alignment)
{
	FOREACH_IDX(i, Decl *, member, param_type->decl->strukt.members)
	{
		Type *member_type = member->type;
		AlignSize load_align;
		LLVMValueRef member_ptr = llvm_emit_struct_gep_raw(context,
														   expand_ptr,
														   llvm_get_type(context, param_type),
														   i,
														   alignment,
														   &load_align);
		llvm_expand_type_to_args(context, member_type, member_ptr, args, arg_count_ref, load_align);
	}
}

static void llvm_expand_type_to_args(GenContext *context, Type *param_type, LLVMValueRef expand_ptr, LLVMValueRef *args, unsigned *arg_count_ref, AlignSize alignment)
{
	switch (type_lowering(param_type)->type_kind)
	{
		case LOWERED_TYPES:
		case TYPE_VOID:
		case TYPE_FUNC_RAW:
		case TYPE_FLEXIBLE_ARRAY:
			UNREACHABLE
			break;
		case TYPE_BOOL:
		case ALL_INTS:
		case ALL_FLOATS:
		case TYPE_FUNC_PTR:
		case TYPE_POINTER:
			args[(*arg_count_ref)++] = llvm_load(context,
												 llvm_get_type(context, param_type),
												 expand_ptr,
												 alignment,
												 "loadexpanded");
			return;
		case TYPE_STRUCT:
			llvm_expand_struct_to_args(context, param_type, expand_ptr, args, arg_count_ref, alignment);
			break;
		case TYPE_ARRAY:
			llvm_expand_array_to_args(context, param_type, expand_ptr, args, arg_count_ref, alignment);
			break;
		case TYPE_UNION:
		case TYPE_SLICE:
		case TYPE_VECTOR:
		case TYPE_ANY:
			TODO
			break;
	}
}

void llvm_emit_struct_member_ref(GenContext *c, BEValue *struct_ref, BEValue *member_ref, unsigned member_id)
{
	assert(llvm_value_is_addr(struct_ref));
	llvm_value_fold_optional(c, struct_ref);
	assert(struct_ref->type->type_kind == TYPE_STRUCT);
	AlignSize align;
	LLVMValueRef ptr = llvm_emit_struct_gep_raw(c, struct_ref->value, llvm_get_type(c, struct_ref->type), member_id, struct_ref->alignment, &align);
	llvm_value_set_address(member_ref, ptr, struct_ref->type->decl->strukt.members[member_id]->type, align);
}

LLVMValueRef llvm_emit_struct_gep_raw(GenContext *c, LLVMValueRef ptr, LLVMTypeRef struct_type, unsigned index,
                                      unsigned struct_alignment, AlignSize *alignment)
{
	*alignment = type_min_alignment((AlignSize)LLVMOffsetOfElement(c->target_data, struct_type, index), struct_alignment);
	if (!index) return ptr;
	ByteSize offset = LLVMOffsetOfElement(c->target_data, struct_type, index);
	return llvm_emit_const_ptradd_inbounds_raw(c, ptr, offset);
}


LLVMValueRef llvm_emit_array_gep_raw_index(GenContext *c, LLVMValueRef ptr, LLVMTypeRef array_type, BEValue *index, AlignSize array_alignment, AlignSize *alignment)
{
	LLVMValueRef index_val = llvm_load_value(c, index);
	LLVMTypeRef element_type = LLVMGetElementType(array_type);
	Type *index_type = index->type;
	assert(type_is_integer(index_type));
	LLVMTypeRef idx_type = llvm_get_type(c, index_type);
	if (type_is_unsigned(index_type) && type_size(index_type) < type_size(type_usz))
	{
		index_val = llvm_zext_trunc(c, index_val, idx_type);
	}
	*alignment = type_min_alignment(llvm_abi_size(c, element_type), array_alignment);
	return llvm_emit_pointer_inbounds_gep_raw(c, element_type, ptr, index_val);
}

LLVMValueRef llvm_emit_array_gep_raw(GenContext *c, LLVMValueRef ptr, LLVMTypeRef array_type, unsigned index, AlignSize array_alignment, AlignSize *alignment)
{
	BEValue index_value;
	llvm_value_set(&index_value, llvm_const_int(c, type_usz, index), type_usz);
	return llvm_emit_array_gep_raw_index(c, ptr, array_type, &index_value, array_alignment, alignment);
}

LLVMValueRef llvm_emit_ptradd_raw(GenContext *c, LLVMValueRef ptr, LLVMValueRef offset, ByteSize mult)
{
	if (LLVMIsConstant(offset) && LLVMIsNull(offset))
	{
		return ptr;
	}
	if (mult == 1) return LLVMBuildGEP2(c->builder, c->byte_type, ptr, &offset, 1, "ptradd_any");
	return LLVMBuildGEP2(c->builder, LLVMArrayType(c->byte_type, mult), ptr, &offset, 1, "ptroffset_any");
}

LLVMValueRef llvm_emit_ptradd_inbounds_raw(GenContext *c, LLVMValueRef ptr, LLVMValueRef offset, ByteSize mult)
{
	if (LLVMIsConstant(offset) && LLVMIsNull(offset)) return ptr;
	if (mult == 1) return LLVMBuildInBoundsGEP2(c->builder, c->byte_type, ptr, &offset, 1, "ptradd");
	return LLVMBuildInBoundsGEP2(c->builder, LLVMArrayType(c->byte_type, mult), ptr, &offset, 1, "ptroffset");
}

LLVMValueRef llvm_emit_const_vector(LLVMValueRef value, ArraySize len)
{
	LLVMValueRef slots[256];
	LLVMValueRef *ptr = slots;
	if (len > 256)
	{
		ptr = MALLOC(len * sizeof(LLVMValueRef));
	}
	for (ArraySize i = 0; i < len; i++)
	{
		ptr[i] = value;
	}
	return LLVMConstVector(ptr, len);
}


LLVMValueRef llvm_ptr_mult(GenContext *c, LLVMValueRef offset, LLVMTypeRef pointee_type)
{
	ByteSize size = llvm_abi_size(c, pointee_type);
	if (size == 1) return offset;

	LLVMTypeRef offset_type = LLVMTypeOf(offset);
	LLVMValueRef mult;
	if (LLVMGetTypeKind(offset_type) == LLVMVectorTypeKind)
	{
		mult = llvm_emit_const_vector(LLVMConstInt(LLVMGetElementType(offset_type), size, false), LLVMGetVectorSize(offset_type));
	}
	else
	{
		mult = LLVMConstInt(offset_type, size, false);
	}
	return LLVMBuildMul(c->builder, offset, mult, "");
}
LLVMValueRef llvm_emit_pointer_gep_raw(GenContext *c, LLVMTypeRef pointee_type, LLVMValueRef ptr, LLVMValueRef offset)
{
	if (LLVMIsConstant(offset))
	{
		return llvm_emit_ptradd_raw(c, ptr, llvm_ptr_mult(c, offset, pointee_type), 1);
	}
	return llvm_emit_ptradd_raw(c, ptr, offset, llvm_abi_size(c, pointee_type));
}

LLVMValueRef llvm_emit_pointer_inbounds_gep_raw(GenContext *c, LLVMTypeRef pointee_type, LLVMValueRef ptr, LLVMValueRef offset)
{
	if (LLVMIsConstant(offset))
	{
		return llvm_emit_ptradd_inbounds_raw(c, ptr, llvm_ptr_mult(c, offset, pointee_type), 1);
	}
	return llvm_emit_ptradd_inbounds_raw(c, ptr, offset, llvm_abi_size(c, pointee_type));
}

LLVMValueRef llvm_emit_const_ptradd_inbounds_raw(GenContext *c, LLVMValueRef ptr, ByteSize offset)
{
	return llvm_emit_ptradd_inbounds_raw(c, ptr, LLVMConstInt(c->size_type, offset, false), 1);
}

void llvm_emit_slice_len(GenContext *c, BEValue *slice, BEValue *len)
{
	llvm_value_addr(c, slice);
	AlignSize alignment = 0;
	LLVMValueRef len_addr = llvm_emit_struct_gep_raw(c,
	                                                 slice->value,
	                                                 llvm_get_type(c, slice->type),
	                                                 1,
	                                                 slice->alignment,
	                                                 &alignment);
	llvm_value_set_address(len, len_addr, type_usz, alignment);
}

void llvm_emit_slice_pointer(GenContext *context, BEValue *slice, BEValue *pointer)
{
	assert(slice->type->type_kind == TYPE_SLICE);
	Type *ptr_type = type_get_ptr(slice->type->array.base);
	if (slice->kind == BE_ADDRESS)
	{
		AlignSize alignment;
		LLVMValueRef ptr = llvm_emit_struct_gep_raw(context, slice->value, llvm_get_type(context, slice->type), 0, slice->alignment, &alignment);
		llvm_value_set_address(pointer, ptr, ptr_type, alignment);
		return;
	}
	LLVMValueRef ptr = llvm_emit_extract_value(context, slice->value, 0);
	llvm_value_set(pointer, ptr, ptr_type);
}

static void llvm_emit_any_pointer(GenContext *c, BEValue *value, BEValue *pointer)
{
	llvm_value_fold_optional(c, value);
	if (value->kind == BE_ADDRESS)
	{
		AlignSize alignment;
		LLVMValueRef ptr = llvm_emit_struct_gep_raw(c, value->value, llvm_get_type(c, value->type), 0, value->alignment, &alignment);
		llvm_value_set_address(pointer, ptr, type_voidptr, alignment);
		return;
	}
	LLVMValueRef ptr = llvm_emit_extract_value(c, value->value, 0);
	llvm_value_set(pointer, ptr, type_voidptr);
}

void llvm_value_struct_gep(GenContext *c, BEValue *element, BEValue *struct_pointer, unsigned index)
{
	llvm_value_fold_optional(c, struct_pointer);
	ArrayIndex actual_index = -1;
	Decl *member;
	for (ArrayIndex i = 0; i <= index; i++)
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
	llvm_value_set_address_abi_aligned(element, ref, member->type);
	element->alignment = alignment;
}


void llvm_emit_parameter(GenContext *c, LLVMValueRef *args, unsigned *arg_count_ref, ABIArgInfo *info, BEValue *be_value, Type *type)
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
			if (info->attributes.by_val && llvm_value_is_addr(be_value) && info->indirect.alignment <= be_value->alignment)
			{
				llvm_value_fold_optional(c, be_value);
				args[(*arg_count_ref)++] = be_value->value;
				return;
			}
			LLVMValueRef indirect = llvm_emit_alloca(c, llvm_get_type(c, type), info->indirect.alignment, "indirectarg");
			llvm_store_to_ptr_aligned(c, indirect, be_value, info->indirect.alignment);
			args[(*arg_count_ref)++] = indirect;
			return;
		}
		case ABI_ARG_DIRECT:
			args[(*arg_count_ref)++] = llvm_load_value_store(c, be_value);
			return;
		case ABI_ARG_DIRECT_SPLIT_STRUCT_I32:
		{
			LLVMTypeRef coerce_type = llvm_get_coerce_type(c, info);
			assert(coerce_type && coerce_type != llvm_get_type(c, type));
			AlignSize target_alignment = llvm_abi_alignment(c, coerce_type);

			AlignSize alignment;
			LLVMValueRef cast = llvm_emit_coerce_alignment(c, be_value, coerce_type, target_alignment, &alignment);
			LLVMTypeRef element = llvm_get_type(c, type_uint);
			for (unsigned idx = 0; idx < info->direct_struct_expand; idx++)
			{
				AlignSize load_align;
				LLVMValueRef element_ptr = llvm_emit_struct_gep_raw(c, cast, coerce_type, idx, alignment, &load_align);
				args[(*arg_count_ref)++] = llvm_load(c, element, element_ptr, load_align, "");
			}
			return;
		}
		case ABI_ARG_DIRECT_COERCE:
		{
			LLVMTypeRef coerce_type = llvm_get_type(c, info->direct_coerce_type);
			if (coerce_type == llvm_get_type(c, type))
			{
				args[(*arg_count_ref)++] = llvm_load_value_store(c, be_value);
				return;
			}
			args[(*arg_count_ref)++] = llvm_emit_coerce(c, coerce_type, be_value, type);
			return;
		}
		case ABI_ARG_DIRECT_COERCE_INT:
		{
			LLVMTypeRef coerce_type = LLVMIntTypeInContext(c->context, type_size(type) * 8);
			if (coerce_type == llvm_get_type(c, type))
			{
				args[(*arg_count_ref)++] = llvm_load_value_store(c, be_value);
				return;
			}
			args[(*arg_count_ref)++] = llvm_emit_coerce(c, coerce_type, be_value, type);
			return;
		}
		case ABI_ARG_DIRECT_PAIR:
		{
			assert(type_flatten(be_value->type) == be_value->type);
			LLVMTypeRef original_type = llvm_get_type(c, be_value->type);
			LLVMTypeRef struct_type = llvm_get_coerce_type(c, info);
			AlignSize alignment;
			if (llvm_types_are_similar(original_type, struct_type))
			{
				// Optimization
				assert(LLVMGetTypeKind(original_type) == LLVMStructTypeKind && LLVMCountStructElementTypes(original_type) == 2);
				if (llvm_value_is_addr(be_value))
				{
					LLVMValueRef ptr = llvm_emit_struct_gep_raw(c, be_value->value, original_type, 0, be_value->alignment, &alignment);
					args[(*arg_count_ref)++] = llvm_load(c, LLVMStructGetTypeAtIndex(original_type, 0), ptr, alignment, "lo");
					ptr = llvm_emit_struct_gep_raw(c, be_value->value, original_type, 1, be_value->alignment, &alignment);
					args[(*arg_count_ref)++] = llvm_load(c, LLVMStructGetTypeAtIndex(original_type, 1), ptr, alignment, "hi");
					return;
				}
				LLVMValueRef val = be_value->value;
				// Maybe it's just created? Let's optimize codegen.
				if (!LLVMGetFirstUse(val) && LLVMIsAInsertValueInst(val) && LLVMIsAInsertValueInst(
						LLVMGetPreviousInstruction(val)))
				{
					LLVMValueRef prev = LLVMGetPreviousInstruction(val);
					// Isn't this a second insert?
					if (LLVMGetOperand(val, 0) != prev) goto NO_OPT;
					// Is it used in between?
					if (LLVMGetNextUse(LLVMGetFirstUse(prev))) goto NO_OPT;
					// No, then we can replace the instructions with the values.
					LLVMValueRef first_val = LLVMGetOperand(prev, 1);
					LLVMValueRef second_val = LLVMGetOperand(val, 1);
					const char *name = LLVMGetValueName(val);
					if (name && strncmp(name, temp_name, 6) == 0)
					{
						LLVMInstructionEraseFromParent(val);
						LLVMInstructionEraseFromParent(prev);
					}
					args[(*arg_count_ref)++] = first_val;
					args[(*arg_count_ref)++] = second_val;
					return;
				}
				NO_OPT:
				args[(*arg_count_ref)++] = llvm_emit_extract_value(c, be_value->value, 0);
				args[(*arg_count_ref)++] = llvm_emit_extract_value(c, be_value->value, 1);
				return;
			}
			llvm_value_addr(c, be_value);
			REMINDER("Handle invalid alignment");
			// Here we do the following transform:
			// struct -> { lo, hi } -> lo, hi
			LLVMTypeRef lo = llvm_abi_type(c, info->direct_pair.lo);
			LLVMTypeRef hi = llvm_abi_type(c, info->direct_pair.hi);

			AlignSize struct_align;
			LLVMValueRef cast = llvm_emit_coerce_alignment(c, be_value, struct_type, llvm_abi_alignment(c, struct_type), &struct_align);
			// Get the lo value.

			LLVMValueRef lo_ptr = llvm_emit_struct_gep_raw(c, cast, struct_type, 0, struct_align, &alignment);
			args[(*arg_count_ref)++] = llvm_load(c, lo, lo_ptr, alignment, "lo");
			// Get the hi value.
			LLVMValueRef hi_ptr = llvm_emit_struct_gep_raw(c, cast, struct_type, 1, struct_align, &alignment);
			args[(*arg_count_ref)++] = llvm_load(c, hi, hi_ptr, alignment, "hi");
			return;
		}
		case ABI_ARG_EXPAND_COERCE:
		{
			// Move this to an address (if needed)
			llvm_value_addr(c, be_value);
			LLVMValueRef addr = be_value->value;
			AlignSize align = be_value->alignment;
			args[(*arg_count_ref)++] = llvm_load(c, llvm_get_type(c, info->coerce_expand.lo), addr, align, "");
			LLVMTypeRef type2 = llvm_coerce_expand_hi_offset(c, &addr, info, &align);
			args[(*arg_count_ref)++] = llvm_load(c, type2, addr, align, "");
			return;
		}
		case ABI_ARG_EXPAND:
		{
			// Move this to an address (if needed)
			llvm_value_addr(c, be_value);
			llvm_expand_type_to_args(c, type, be_value->value, args, arg_count_ref, be_value->alignment);
			return;
		}
	}
}



void llvm_add_abi_call_attributes(GenContext *c, LLVMValueRef call_value, int count, ABIArgInfo **infos)
{
	for (unsigned i = 0; i < count; i++)
	{
		ABIArgInfo *info = infos[i];
		if (info->attributes.signext)
		{
			llvm_attribute_add_call(c, call_value, attribute_id.sext, (int)info->param_index_start + 1, 0);
		}
		if (info->attributes.zeroext)
		{
			llvm_attribute_add_call(c, call_value, attribute_id.zext, (int)info->param_index_start + 1, 0);
		}
		switch (info->kind)
		{
			case ABI_ARG_INDIRECT:
				if (info->attributes.by_val)
				{
					llvm_attribute_add_call_type(c,
												 call_value,
												 attribute_id.byval,
												 (int)info->param_index_start + 1,
												 llvm_get_type(c, info->indirect.type));
				}
				llvm_attribute_add_call(c, call_value, attribute_id.align, (int)info->param_index_start + 1, info->indirect.alignment);
				break;
			default:
				break;
		}
	}

}


void llvm_emit_raw_call(GenContext *c, BEValue *result_value, FunctionPrototype *prototype, LLVMTypeRef func_type, LLVMValueRef func, LLVMValueRef *args, unsigned arg_count, int inline_flag, LLVMValueRef error_var, bool sret_return, BEValue *synthetic_return_param)
{
	ABIArgInfo *ret_info = prototype->ret_abi_info;
	Type *call_return_type = prototype->abi_ret_type;

	LLVMValueRef call_value = LLVMBuildCall2(c->builder, func_type, func, args, arg_count, "");
	if (prototype->call_abi)
	{
		LLVMSetInstructionCallConv(call_value, llvm_call_convention_from_call(prototype->call_abi));
	}
	switch (inline_flag)
	{
		case -1:
			llvm_attribute_add_call(c, call_value, attribute_id.noinline, -1, 0);
			break;
		case 1:
			llvm_attribute_add_call(c, call_value, attribute_id.alwaysinline, -1, 0);
			break;
		default:
			break;
	}

	assert(!prototype->ret_by_ref || prototype->ret_by_ref_abi_info->kind != ABI_ARG_INDIRECT);

	llvm_add_abi_call_attributes(c, call_value, vec_size(prototype->param_types), prototype->abi_args);
	if (prototype->abi_varargs)
	{
		llvm_add_abi_call_attributes(c,
									 call_value,
									 vec_size(prototype->varargs),
									 prototype->abi_varargs);
	}

	// 11. Process the return value.
	switch (ret_info->kind)
	{
		case ABI_ARG_EXPAND:
		case ABI_ARG_DIRECT_SPLIT_STRUCT_I32:
			UNREACHABLE
		case ABI_ARG_IGNORE:
			// 12. Basically void returns or empty structs.
			//     Here we know we don't have an optional or any return value that can be used.
			assert(!prototype->is_optional && "Optional should have produced a return value.");
			*result_value = (BEValue) { .type = type_void, .kind = BE_VALUE };
			return;
		case ABI_ARG_INDIRECT:
			llvm_attribute_add_call_type(c, call_value, attribute_id.sret, 1, llvm_get_type(c, ret_info->indirect.type));
			llvm_attribute_add_call(c, call_value, attribute_id.align, 1, ret_info->indirect.alignment);
			// 13. Indirect, that is passing the result through an out parameter.

			// 13a. In the case of an already present error_var, we don't need to do a load here.
			if (error_var || sret_return) break;

			// 13b. If not it will be contained in a be_value that is an address,
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
			llvm_emit_convert_value_from_coerced(c, result_value, struct_type, call_value, call_return_type);
			break;
		}
		case ABI_ARG_EXPAND_COERCE:
		{
			// 15. Expand-coerce, this is similar to "direct pair", but looks like this:
			//     { lo, hi } set into { pad, lo, pad, hi } -> original type.

			// 15a. Create memory to hold the return type.
			// COERCE UPDATE bitcast removed, check for ways to optimize

			LLVMValueRef addr = llvm_emit_alloca_aligned(c, call_return_type, "");
			llvm_value_set_address_abi_aligned(result_value, addr, call_return_type);

			// Store lower
			AlignSize align = result_value->alignment;
			llvm_store_to_ptr_raw_aligned(c, addr, llvm_emit_extract_value(c, call_value, 0), align);

			// Store upper
			(void)llvm_coerce_expand_hi_offset(c, &addr, ret_info, &align);
			llvm_store_to_ptr_raw_aligned(c, addr, llvm_emit_extract_value(c, call_value, 1), align);
			break;
		}
		case ABI_ARG_DIRECT:
			llvm_value_set(result_value, call_value, call_return_type);
			break;
		case ABI_ARG_DIRECT_COERCE_INT:
		{
			// 16. A direct coerce, this is basically "call result" bitcast return type.

			// 16a. Get the type of the return.
			LLVMTypeRef coerce = LLVMIntTypeInContext(c->context, type_size(call_return_type) * 8);

			// 16b. If we don't have any coerce type, or the actual LLVM types are the same, we're done.
			if (coerce == llvm_get_type(c, call_return_type))
			{
				// 16c. We just set as a value in be_value.
				llvm_value_set(result_value, call_value, call_return_type);
				break;
			}
			// 16c. We use a normal bitcast coerce.
			llvm_emit_convert_value_from_coerced(c, result_value, coerce, call_value, call_return_type);
			break;
		}
		case ABI_ARG_DIRECT_COERCE:
		{
			// 16. A direct coerce, this is basically "call result" bitcast return type.

			// 16a. Get the type of the return.
			LLVMTypeRef coerce = llvm_get_type(c, ret_info->direct_coerce_type);

			// 16b. If we don't have any coerce type, or the actual LLVM types are the same, we're done.
			if (coerce == llvm_get_type(c, call_return_type))
			{
				// 16c. We just set as a value in be_value.
				llvm_value_set(result_value, call_value, call_return_type);
				break;
			}
			// 16c. We use a normal bitcast coerce.
			llvm_emit_convert_value_from_coerced(c, result_value, coerce, call_value, call_return_type);
			break;
		}
	}

	// 17. Handle optionals.
	if (sret_return)
	{
		*result_value = (BEValue) { .type = type_void, .kind = BE_VALUE };
		return;
	}
	if (prototype->is_optional)
	{
		BEValue no_err;

		// 17a. If we used the error var as the indirect recipient, then that will hold the error.
		//      otherwise it's whatever value in be_value.
		BEValue error_holder = *result_value;
		if (error_var)
		{
			llvm_value_set_address_abi_aligned(&error_holder, c->catch.fault, type_anyfault);
		}

		LLVMValueRef stored_error;

		if (error_var)
		{
			stored_error = c->catch.fault;
			c->catch.fault = NULL;
		}
		llvm_emit_jump_to_optional_exit(c, llvm_load_value(c, &error_holder));
		if (error_var)
		{
			c->catch.fault = stored_error;
		}


		// 17g. If void, be_value contents should be skipped.
		if (!prototype->ret_by_ref)
		{
			*result_value = (BEValue) { .type = type_void, .kind = BE_VALUE };
			return;
		}

		// 17h. Assign the return param to be_value.
		*result_value = *synthetic_return_param;
		return;
	}

	// 17i. The simple case here is where there is a normal return.
	//      In this case be_value already holds the result
}

static LLVMValueRef llvm_emit_dynamic_search(GenContext *c, LLVMValueRef type_id_ptr, LLVMValueRef selector)
{
	LLVMTypeRef type = c->dyn_find_function_type;
	LLVMValueRef func = c->dyn_find_function;
	if (!c->dyn_find_function)
	{
		LLVMTypeRef types[2] = { c->ptr_type, c->ptr_type };
		type = c->dyn_find_function_type = LLVMFunctionType(c->ptr_type, types, 2, false);
		func = c->dyn_find_function = LLVMAddFunction(c->module, ".dyn_search", c->dyn_find_function_type);

		LLVMSetUnnamedAddress(func, LLVMGlobalUnnamedAddr);
		LLVMSetLinkage(func, LLVMWeakODRLinkage);
		llvm_set_comdat(c, func);

		LLVMBasicBlockRef entry;
		LLVMBuilderRef builder = llvm_create_function_entry(c, func, &entry);

		AlignSize align;
		LLVMValueRef dtable_ptr_in = LLVMGetParam(func, 0);
		LLVMValueRef func_ref = LLVMGetParam(func, 1);

		LLVMBasicBlockRef check = llvm_basic_block_new(c, "check");
		LLVMBasicBlockRef missing_function = llvm_basic_block_new(c, "missing_function");
		LLVMBasicBlockRef compare = llvm_basic_block_new(c, "compare");
		LLVMBasicBlockRef match = llvm_basic_block_new(c, "match");
		LLVMBasicBlockRef no_match = llvm_basic_block_new(c, "no_match");

		LLVMBuildBr(builder, check);

		// check: dtable_ptr = phi
		LLVMAppendExistingBasicBlock(func, check);
		LLVMPositionBuilderAtEnd(builder, check);
		LLVMValueRef dtable_ptr = LLVMBuildPhi(builder, c->ptr_type, "");

		// dtable_ptr == null
		LLVMValueRef cmp = LLVMBuildICmp(builder, LLVMIntEQ, dtable_ptr, LLVMConstNull(c->ptr_type), "");

		// if (cmp) goto missing_function else compare
		LLVMBuildCondBr(builder, cmp, missing_function, compare);

		// missing_function: return null
		LLVMAppendExistingBasicBlock(func, missing_function);
		LLVMPositionBuilderAtEnd(builder, missing_function);
		LLVMBuildRet(builder, LLVMConstNull(c->ptr_type));

		// function_type = dtable_ptr.function_type
		LLVMAppendExistingBasicBlock(func, compare);
		LLVMPositionBuilderAtEnd(builder, compare);

		LLVMValueRef function_type = LLVMBuildStructGEP2(builder, c->dtable_type, dtable_ptr, 1, "");
		function_type = LLVMBuildLoad2(builder, c->ptr_type, function_type, "");
		LLVMSetAlignment(function_type, llvm_abi_alignment(c, c->ptr_type));

		// function_type == func_ref
		cmp = LLVMBuildICmp(builder, LLVMIntEQ, function_type, func_ref, "");

		// if (cmp) goto match else no_match
		LLVMBuildCondBr(builder, cmp, match, no_match);

		// match: function_ptr = dtable_ptr.function_ptr
		LLVMAppendExistingBasicBlock(func, match);
		LLVMPositionBuilderAtEnd(builder, match);

		// Offset = 0
		LLVMValueRef function_ptr = LLVMBuildLoad2(builder, c->ptr_type, dtable_ptr, "");
		LLVMSetAlignment(function_ptr, llvm_abi_alignment(c, c->ptr_type));
		LLVMBuildRet(builder, function_ptr);

		// no match: next = dtable_ptr.next
		LLVMAppendExistingBasicBlock(func, no_match);
		LLVMPositionBuilderAtEnd(builder, no_match);
		LLVMValueRef next = LLVMBuildStructGEP2(builder, c->dtable_type, dtable_ptr, 2, "");
		next = LLVMBuildLoad2(builder, c->ptr_type, next, "");
		LLVMSetAlignment(next, llvm_abi_alignment(c, c->ptr_type));

		// goto check
		LLVMBuildBr(builder, check);

		llvm_set_phi(dtable_ptr, dtable_ptr_in, entry, next, no_match);
		LLVMDisposeBuilder(builder);
	}
	// Insert cache.
	LLVMValueRef cache_fn_ptr = llvm_emit_alloca_aligned(c, type_voidptr, ".inlinecache");
	LLVMValueRef cache_type_id_ptr = llvm_emit_alloca_aligned(c, type_voidptr, ".cachedtype");
	LLVMBasicBlockRef current_block = LLVMGetInsertBlock(c->builder);
	LLVMValueRef next_after_alloca = LLVMGetNextInstruction(c->alloca_point);
	if (next_after_alloca)
	{
		LLVMPositionBuilderBefore(c->builder, next_after_alloca);
	}
	else
	{
		LLVMPositionBuilderAtEnd(c->builder, LLVMGetInstructionParent(c->alloca_point));
	}
	llvm_store_to_ptr_zero(c, cache_type_id_ptr, type_voidptr);
	LLVMPositionBuilderAtEnd(c->builder, current_block);
	LLVMBasicBlockRef cache_miss = llvm_basic_block_new(c, "cache_miss");
	LLVMBasicBlockRef cache_hit = llvm_basic_block_new(c, "cache_hit");
	LLVMBasicBlockRef exit = llvm_basic_block_new(c, "");
	LLVMValueRef cached_type_id = llvm_load_abi_alignment(c, type_voidptr, cache_type_id_ptr, "type");
	LLVMValueRef compare = LLVMBuildICmp(c->builder, LLVMIntEQ, type_id_ptr, cached_type_id, "");
	llvm_emit_cond_br_raw(c, compare, cache_hit, cache_miss);
	llvm_emit_block(c, cache_miss);
	AlignSize align;
	LLVMValueRef dtable_ref = llvm_emit_struct_gep_raw(c,
													   type_id_ptr,
													   c->introspect_type,
													   INTROSPECT_INDEX_DTABLE,
													   llvm_abi_alignment(c, c->introspect_type),
													   &align);
	LLVMValueRef dtable_ptr = llvm_load(c, c->ptr_type, dtable_ref, align, "");
	LLVMValueRef params[2] = { dtable_ptr, selector };
	LLVMValueRef call = LLVMBuildCall2(c->builder, type, func, params, 2, "");
	// Store in cache.
	llvm_store_to_ptr_raw(c, cache_fn_ptr, call, type_voidptr);
	llvm_store_to_ptr_raw(c, cache_type_id_ptr, type_id_ptr, type_voidptr);
	llvm_emit_br(c, exit);
	llvm_emit_block(c, cache_hit);
	LLVMValueRef cached_val = llvm_load_abi_alignment(c, type_voidptr, cache_fn_ptr, "cache_hit_fn");
	llvm_emit_br(c, exit);
	llvm_emit_block(c, exit);
	LLVMValueRef phi = LLVMBuildPhi(c->builder, c->ptr_type, "fn_phi");
	llvm_set_phi(phi, cached_val, cache_hit, call, cache_miss);
	return phi;
}

/**
 * We assume all optionals are already folded for the arguments.
 */
INLINE void llvm_emit_call_invocation(GenContext *c, BEValue *result_value,
									  BEValue *target,
									  SourceSpan span,
									  FunctionPrototype *prototype,
									  Expr **args,
									  BEValue *values,
									  int inline_flag,
									  LLVMValueRef func,
									  LLVMTypeRef func_type,
									  Expr **varargs)
{
	LLVMValueRef arg_values[512];
	unsigned arg_count = 0;
	Type **params = prototype->param_types;
	ABIArgInfo **abi_args = prototype->abi_args;
	unsigned param_count = vec_size(params);
	FunctionPrototype copy;
	if (prototype->raw_variadic)
	{
		if (varargs)
		{
			copy = *prototype;
			copy.varargs = NULL;

			FOREACH(Expr *, val, varargs)
			{
				vec_add(copy.varargs, type_flatten(val->type));
			}
			copy.is_resolved = false;
			copy.ret_abi_info = NULL;
			copy.ret_by_ref_abi_info = NULL;
			copy.abi_args = NULL;
			c_abi_func_create(&copy);
			prototype = &copy;
			LLVMTypeRef *params_type = NULL;
			llvm_update_prototype_abi(c, prototype, &params_type);
		}
	}
	ABIArgInfo *ret_info = prototype->ret_abi_info;
	Type *call_return_type = prototype->abi_ret_type;

	// 5. In the case of an optional, the error is replacing the regular return abi.
	LLVMValueRef error_var = NULL;

	*result_value = (BEValue){ .kind = BE_VALUE, .value = NULL };
	// 6. Generate data for the return value.
	bool sret_return = false;
	switch (ret_info->kind)
	{
		case ABI_ARG_INDIRECT:
			// 6a. We can use the stored error var if there is no redirect.
			if (prototype->is_optional && c->catch.fault && !ret_info->attributes.realign)
			{
				error_var = c->catch.fault;
				arg_values[arg_count++] = error_var;
				break;
			}
			// 6b. Return true is indirect, in this case we allocate a local, using the desired alignment on the caller side.
			assert(ret_info->attributes.realign || ret_info->indirect.alignment == type_abi_alignment(call_return_type));
			AlignSize alignment = ret_info->indirect.alignment;
			// If we have a target, then use it.
			if (target && alignment <= target->alignment)
			{
				assert(target->kind == BE_ADDRESS);
				arg_values[arg_count++] = target->value;
				sret_return = true;
				break;
			}
			llvm_value_set_address(result_value,
			                       llvm_emit_alloca(c, llvm_get_type(c, call_return_type), alignment, "sretparam"),
			                       call_return_type,
			                       alignment);

			// 6c. Add the pointer to the list of arguments.
			arg_values[arg_count++] = result_value->value;
			break;
		case ABI_ARG_EXPAND:
		case ABI_ARG_DIRECT_SPLIT_STRUCT_I32:
			UNREACHABLE
		case ABI_ARG_DIRECT_PAIR:
		case ABI_ARG_IGNORE:
		case ABI_ARG_DIRECT_COERCE_INT:
		case ABI_ARG_DIRECT_COERCE:
		case ABI_ARG_DIRECT:
		case ABI_ARG_EXPAND_COERCE:
			break;
	}


	// 7. We might have an optional indirect return and a normal return.
	//    In this case we need to add it by hand.
	BEValue synthetic_return_param = { 0 };
	if (prototype->ret_by_ref)
	{
		// 7b. Create the address to hold the return.
		Type *actual_return_type = type_lowering(prototype->ret_by_ref_type);
		llvm_value_set(&synthetic_return_param, llvm_emit_alloca_aligned(c, actual_return_type, "retparam"), type_get_ptr(actual_return_type));
		// 7c. Emit it as a parameter as a pointer (will implicitly add it to the value list)
		llvm_emit_parameter(c, arg_values, &arg_count, prototype->ret_by_ref_abi_info, &synthetic_return_param, synthetic_return_param.type);
		// 7d. Update the be_value to actually be an address.
		llvm_value_set_address_abi_aligned(&synthetic_return_param, synthetic_return_param.value, actual_return_type);
	}

	BEValue temp_value;

	// 8. Add all other arguments.
	for (unsigned i = 0; i < param_count; i++)
	{
		// 8a. Evaluate the expression.
		Type *param = params[i];
		ABIArgInfo *info = abi_args[i];

		// 8b. Emit the parameter according to ABI rules.
		BEValue value_copy = values[i];
		llvm_emit_parameter(c, arg_values, &arg_count, info, &value_copy, param);
	}

	// 9. Typed varargs

	if (prototype->raw_variadic)
	{
		unsigned vararg_count = vec_size(varargs);
		if (prototype->abi_varargs)
		{
			// 9. Emit varargs.
			unsigned index = 0;
			ABIArgInfo **abi_varargs = prototype->abi_varargs;
			for (unsigned i = 0; i < vararg_count; i++)
			{
				ABIArgInfo *info = abi_varargs[index];
				BEValue value_copy = values[i + param_count];
				llvm_emit_parameter(c, arg_values, &arg_count, info, &value_copy, prototype->varargs[index]);
				index++;
			}
		}
		else
		{
			// 9. Emit varargs.
			for (unsigned i = 0; i < vararg_count; i++)
			{
				REMINDER("Varargs should be expanded correctly");
				arg_values[arg_count++] = llvm_load_value_store(c, &values[i + param_count]);
			}
		}
	}


	// 10. Create the actual call (remember to emit a loc, because we might have shifted loc emitting the params)
	EMIT_SPAN(c, span);

	llvm_emit_raw_call(c, result_value, prototype, func_type, func, arg_values, arg_count, inline_flag, error_var, sret_return, &synthetic_return_param);

	// 17i. The simple case here is where there is a normal return.
	//      In this case be_value already holds the result
}

INLINE void llvm_emit_varargs_expr(GenContext *c, BEValue *value_ref, Expr **varargs, Type *param)
{
	BEValue inner_temp;
	// 9b. Otherwise, we also need to allocate memory for the arguments:
	Type *pointee_type = param->array.base;
	unsigned elements = vec_size(varargs);
	Type *array = type_get_array(pointee_type, elements);
	LLVMTypeRef llvm_array_type = llvm_get_type(c, array);
	AlignSize alignment = type_alloca_alignment(array);
	LLVMValueRef array_ref = llvm_emit_alloca(c, llvm_array_type, alignment, varargslots_name);
	FOREACH_IDX(foreach_index, Expr *, val, varargs)
	{
		llvm_emit_expr(c, &inner_temp, val);
		llvm_value_fold_optional(c, &inner_temp);
		AlignSize store_alignment;
		LLVMValueRef slot = llvm_emit_array_gep_raw(c,
		                                            array_ref,
		                                            llvm_array_type,
		                                            foreach_index,
		                                            alignment,
		                                            &store_alignment);
		llvm_store_to_ptr_aligned(c, slot, &inner_temp, store_alignment);
	}
	llvm_value_aggregate_two(c, value_ref, param, array_ref, llvm_const_int(c, type_usz, elements));
	LLVMSetValueName2(value_ref->value, temp_name, 6);
}

INLINE void llvm_emit_vasplat_expr(GenContext *c, BEValue *value_ref, Expr *vasplat, Type *param)
{
	llvm_emit_expr(c, value_ref, vasplat);
	llvm_value_fold_optional(c, value_ref);
	Type *type = value_ref->type;
	switch (type->type_kind)
	{
		case TYPE_ARRAY:
			llvm_value_addr(c, value_ref);
			llvm_value_bitcast(c, value_ref, type->array.base);
			llvm_value_aggregate_two(c, value_ref, param, value_ref->value, llvm_const_int(c, type_usz, type->array.len));
			return;
		case TYPE_POINTER:
			// Load the pointer
			llvm_value_rvalue(c, value_ref);
			llvm_value_aggregate_two(c, value_ref, param, value_ref->value, llvm_const_int(c, type_usz, type->pointer->array.len));
			return;
		case TYPE_SLICE:
			return;
		default:
			UNREACHABLE
	}

}
static void llvm_emit_call_expr(GenContext *c, BEValue *result_value, Expr *expr, BEValue *target)
{
	if (expr->call_expr.is_builtin)
	{
		llvm_emit_builtin_call(c, result_value, expr);
		return;
	}

	LLVMTypeRef func_type;
	LLVMValueRef func;

	BEValue values[256];
	Expr **args = expr->call_expr.arguments;
	unsigned arg_count = vec_size(args);
	bool always_inline = false;
	FunctionPrototype *prototype;

	// 1. Dynamic dispatch.
	if (expr->call_expr.is_dynamic_dispatch)
	{
		assert(arg_count);
		Expr *any_val = args[0];
		assert(any_val->expr_kind == EXPR_CAST);
		args[0] = exprptr(any_val->cast_expr.expr);
	}

	if (!expr->call_expr.is_func_ref)
	{
		// Call through a pointer.
		Expr *function = exprptr(expr->call_expr.function);

		// 1a. Find the pointee type for the function pointer:
		assert(type_flatten(function->type)->type_kind == TYPE_FUNC_PTR);
		Type *type = type_flatten(function->type)->pointer;

		// 1b. Find the type signature using the underlying pointer.
		prototype = type_get_resolved_prototype(type);

		// 1c. Evaluate the pointer expression.
		BEValue func_value;
		llvm_emit_expr(c, &func_value, function);

		// 1d. Load it as a value
		func = llvm_load_value_store(c, &func_value);

		// 1e. Calculate the function type
		func_type = llvm_get_type(c, type);
	}
	else
	{
		// 2a. Get the function declaration
		Decl *function_decl = declptr(expr->call_expr.func_ref);
		always_inline = function_decl->func_decl.attr_inline;

		// 2b. Set signature, function and function type
		prototype = type_get_resolved_prototype(function_decl->type);
		func = llvm_get_ref(c, function_decl);
		assert(func);
		func_type = llvm_get_type(c, function_decl->type);
	}
	int inline_flag = 0;
	if (expr->call_expr.attr_force_noinline)
	{
		inline_flag = -1;
	}
	else
	{
		inline_flag = expr->call_expr.attr_force_inline || always_inline ? 1 : 0;
	}

	Expr *vararg_splat = NULL;
	Expr **varargs = NULL;
	if (expr->call_expr.va_is_splat)
	{
		vararg_splat = expr->call_expr.vasplat;
	}
	else
	{
		varargs = expr->call_expr.varargs;
	}

	for (unsigned i = 0; i < arg_count; i++)
	{
		BEValue *value_ref = &values[i];
		Expr *arg = args[i];
		if (arg)
		{
			llvm_emit_expr(c, value_ref, arg);
			llvm_value_fold_optional(c, value_ref);
			continue;
		}
		Type *param = prototype->param_types[i];
		if (vararg_splat)
		{
			llvm_emit_vasplat_expr(c, value_ref, vararg_splat, param);
			continue;
		}
		if (varargs)
		{
			llvm_emit_varargs_expr(c, value_ref, varargs, param);
			continue;
		}
		// Just set the size to zero.
		llvm_value_set(value_ref, llvm_get_zero(c, param), param);
	}
	// Emit raw varargs
	if (prototype->raw_variadic && varargs)
	{
		FOREACH_IDX(i, Expr *, vararg, varargs)
		{
			BEValue *value_ref = &values[arg_count + i];
			llvm_emit_expr(c, value_ref, vararg);
			llvm_value_fold_optional(c, value_ref);
		}
	}

	// 1. Dynamic dispatch.
	if (expr->call_expr.is_dynamic_dispatch)
	{
		assert(arg_count);
		BEValue result = values[0];
		BEValue typeid = result;
		llvm_emit_type_from_any(c, &typeid);
		llvm_value_rvalue(c, &typeid);
		llvm_emit_any_pointer(c, &result, &result);
		LLVMValueRef introspect = LLVMBuildIntToPtr(c->builder, typeid.value, c->ptr_type, "");

		LLVMBasicBlockRef missing_function = llvm_basic_block_new(c, "missing_function");
		LLVMBasicBlockRef match = llvm_basic_block_new(c, "match");

		AlignSize align;
		Decl *dyn_fn = declptr(expr->call_expr.func_ref);
		prototype = type_get_resolved_prototype(dyn_fn->type);
		func_type = llvm_get_type(c, dyn_fn->type);
		func = llvm_emit_dynamic_search(c, introspect, llvm_get_ref(c, dyn_fn));
		LLVMValueRef cmp = LLVMBuildICmp(c->builder, LLVMIntEQ, func, LLVMConstNull(c->ptr_type), "");
		llvm_emit_cond_br_raw(c, cmp, missing_function, match);
		llvm_emit_block(c, missing_function);
		Decl *default_method = declptrzero(dyn_fn->func_decl.interface_method);
		if (default_method)
		{
			LLVMBasicBlockRef after = llvm_basic_block_new(c, "after_call");
			FunctionPrototype *default_prototype = type_get_resolved_prototype(default_method->type);
			BEValue default_res;
			llvm_emit_call_invocation(c, &default_res, target, expr->span, default_prototype, args, values, inline_flag,
			                          llvm_get_ref(c, default_method),
			                          llvm_get_type(c, default_method->type),
			                          varargs);
			LLVMValueRef default_val = llvm_load_value(c, &default_res);
			LLVMBasicBlockRef default_block = c->current_block;
			llvm_emit_br(c, after);
			llvm_emit_block(c, match);
			prototype = type_get_resolved_prototype(dyn_fn->type);
			func_type = llvm_get_type(c, dyn_fn->type);
			BEValue normal_res;
			values[0] = result;
			llvm_emit_call_invocation(c, &normal_res, target, expr->span, prototype, args, values, inline_flag, func, func_type,
			                          varargs);
			LLVMValueRef normal_val = llvm_load_value(c, &normal_res);
			LLVMBasicBlockRef normal_block = c->current_block;
			llvm_emit_br(c, after);
			llvm_emit_block(c, after);
			if (normal_val)
			{
				llvm_new_phi(c, result_value, "result", default_res.type, default_val, default_block, normal_val, normal_block);
			}
			else
			{
				*result_value = (BEValue) { .value = NULL };
			}
			return;
		}
		scratch_buffer_clear();
		scratch_buffer_printf("No method '%s' could be found on target", dyn_fn->name);
		llvm_emit_panic(c, scratch_buffer_to_string(), expr->span, NULL, NULL);
		llvm_emit_block(c, match);
		EMIT_LOC(c, expr);
		values[0] = result;

	}

	llvm_emit_call_invocation(c, result_value, target, expr->span, prototype, args, values, inline_flag, func, func_type,
							  varargs);
}




static inline void llvm_emit_expression_list_expr(GenContext *c, BEValue *be_value, Expr *expr)
{
	FOREACH(Expr *, e, expr->expression_list)
	{
		llvm_emit_expr(c, be_value, e);
	}
}

static inline void llvm_emit_return_block(GenContext *c, BEValue *be_value, Type *type, AstId current, BlockExit **block_exit)
{
	Type *type_lowered = type_lowering(type);

	// First case - an empty block
	if (!current)
	{
		llvm_value_set(be_value, llvm_get_undef(c, type_lowered), type_lowered);
		return;
	}

	Ast *value = ast_next(&current);

	// Just a return statement, in this case we can just do the expression.
	if (value->ast_kind == AST_BLOCK_EXIT_STMT && !value->return_stmt.cleanup && !value->return_stmt.cleanup_fail)
	{
		Expr *expr = value->return_stmt.expr;
		if (!expr)
		{
			llvm_value_set(be_value, NULL, type_void);
			return;
		}
		// We can only do this if there is no potential optional
		if (!type_is_optional(type))
		{
			llvm_emit_expr(c, be_value, expr);
			return;
		}
	}

	LLVMValueRef old_ret_out = c->return_out;
	LLVMValueRef error_out = c->catch.fault;
	LLVMBasicBlockRef error_block = c->catch.block;
	LLVMValueRef return_out = NULL;
	LLVMBasicBlockRef expr_block = llvm_basic_block_new(c, "expr_block.exit");
	LLVMBasicBlockRef cleanup_error_block = error_block;
	BlockExit exit = {
			.block_return_exit = expr_block,
			.block_optional_exit = cleanup_error_block,
			.block_error_var = error_out,
			.block_return_out = NULL,
	};

	*block_exit= &exit;

	if (type_lowered != type_void)
	{
		exit.block_return_out = llvm_emit_alloca_aligned(c, type_lowered, "blockret");
	}
	c->catch.fault = NULL;
	c->catch.block = NULL;

	// Process all but the last statement.
	while (value->next)
	{
		llvm_emit_stmt(c, value);
		value = ast_next(&current);
	}

	do
	{
		// Do we have more than one exit?
		// Then follow the normal path.
		if (!llvm_basic_block_is_unused(expr_block)) break;

		// Do we have a void function? That's the only
		// possible case if the last statement isn't return.
		if (value->ast_kind != AST_BLOCK_EXIT_STMT) break;

		// Defers? In that case we also use the default behaviour.
		// We might optimize this later.
		if (value->return_stmt.cleanup || value->return_stmt.cleanup_fail) break;

		Expr *ret_expr = value->return_stmt.expr;

		// If this is a void return, we can just skip here!
		if (!ret_expr)
		{
			llvm_value_set(be_value, NULL, type_void);
			goto DONE;
		}

		// return foo() where foo() is a void!
		if (type_lowering(ret_expr->type) == type_void) break;

		LLVMInstructionEraseFromParent(exit.block_return_out);

		// Restore
		c->return_out = old_ret_out;
		c->catch.block = error_block;
		c->catch.fault = error_out;

		// Output directly to a value
		llvm_emit_expr(c, be_value, ret_expr);
		return;

	} while (0);

	bool has_current_block = llvm_get_current_block_if_in_use(c) != NULL;
	if (has_current_block)
	{
		// Emit the last statement
		llvm_emit_stmt(c, value);
	}

	// In the case of a void with no return, then this may be true.
	if (llvm_basic_block_is_unused(expr_block))
	{
		// Skip the expr block.
		llvm_value_set(be_value, llvm_get_undef(c, type_lowered), type_lowered);
		goto DONE;
	}

	if (has_current_block)
	{
		llvm_emit_br(c, expr_block);
	}

	// Emit the exit block.
	llvm_emit_block(c, expr_block);

	if (exit.block_return_out)
	{
		llvm_value_set_address_abi_aligned(be_value, exit.block_return_out, type_lowered);
	}
	else
	{
		llvm_value_set(be_value, NULL, type_void);
	}

DONE:

	c->return_out = old_ret_out;
	c->catch.block = error_block;
	c->catch.fault = error_out;

}

static inline void llvm_emit_expr_block(GenContext *c, BEValue *be_value, Expr *expr)
{
	DEBUG_PUSH_LEXICAL_SCOPE(c, astptr(expr->expr_block.first_stmt)->span);
	llvm_emit_return_block(c, be_value, expr->type, expr->expr_block.first_stmt, expr->expr_block.block_exit_ref);
	DEBUG_POP_LEXICAL_SCOPE(c);
}

static inline void llvm_emit_macro_block(GenContext *c, BEValue *be_value, Expr *expr)
{
	DebugScope *old_inline_location = c->debug.block_stack;
	DebugScope updated;
	if (llvm_use_debug(c))
	{
		SourceSpan span = expr->span;
		Decl *macro = expr->macro_block.macro;
		LLVMMetadataRef macro_def = llvm_debug_create_macro(c, macro);
		LLVMMetadataRef loc = llvm_create_debug_location(c, span);

		updated = (DebugScope) { .lexical_block = macro_def, .inline_loc = loc, .outline_loc = old_inline_location };
	}
	FOREACH(Decl *, val, expr->macro_block.params)
	{
		// Skip vararg
		if (!val) continue;
		// In case we have a constant, we never do an emit. The value is already folded.
		switch (val->var.kind)
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
			case VARDECL_PARAM_CT:
			case VARDECL_PARAM_CT_TYPE:
			case VARDECL_PARAM_EXPR:
				continue;
			case VARDECL_PARAM_REF:
			case VARDECL_PARAM:
				break;
		}

		Expr *init_expr = val->var.init_expr;
		BEValue value;
		c->debug.block_stack = old_inline_location;
		llvm_emit_expr(c, &value, init_expr);
		if (llvm_value_is_addr(&value) || val->var.is_written || val->var.is_addr || llvm_use_accurate_debug_info(c))
		{
			c->debug.block_stack = &updated;
			llvm_emit_and_set_decl_alloca(c, val);
			llvm_store_decl(c, val, &value);
			continue;
		}
		val->is_value = true;
		val->backend_value = value.value;
	}

	c->debug.block_stack = &updated;
	llvm_emit_return_block(c, be_value, expr->type, expr->macro_block.first_stmt, expr->macro_block.block_exit);
	bool is_unreachable = expr->macro_block.is_noreturn && c->current_block;
	if (is_unreachable)
	{
		llvm_emit_unreachable(c);
	}
	c->debug.block_stack = old_inline_location;
}


LLVMValueRef llvm_emit_call_intrinsic(GenContext *context, unsigned intrinsic, LLVMTypeRef *types, unsigned type_count,
									  LLVMValueRef *values, unsigned arg_count)
{
	LLVMValueRef decl = LLVMGetIntrinsicDeclaration(context->module, intrinsic, types, type_count);
	LLVMTypeRef type = LLVMIntrinsicGetType(context->context, intrinsic, types, type_count);
	return LLVMBuildCall2(context->builder, type, decl, values, arg_count, "");
}

static inline void llvm_emit_optional(GenContext *c, BEValue *be_value, Expr *expr)
{
	Expr *fail = expr->inner_expr;
	// If there is an error value, assign to it.
	if (c->catch.fault)
	{
		assert(c->catch.fault);
		llvm_emit_expr(c, be_value, fail);
		llvm_store_to_ptr(c, c->catch.fault, be_value);
	}
	// Branch to the catch
	llvm_emit_br(c, c->catch.block);
	// Create an empty block
	LLVMBasicBlockRef ignored_block = llvm_basic_block_new(c, "postfailed");
	llvm_emit_block(c, ignored_block);

	// Finally we need to replace the result with something undefined here.
	// It will be optimized away.
	Type *type = type_lowering(expr->type);
	if (type == type_void)
	{
		llvm_value_set(be_value, NULL, type_void);
		return;
	}
	llvm_value_set(be_value, llvm_get_undef(c, type), type);
}

static inline LLVMValueRef llvm_update_vector(GenContext *c, LLVMValueRef vector, LLVMValueRef value, ArrayIndex index)
{
	LLVMValueRef index_value = llvm_const_int(c, type_usz, (uint64_t)index);
	return LLVMBuildInsertElement(c->builder, vector, value, index_value, "");
}

static inline void llvm_emit_vector_initializer_list(GenContext *c, BEValue *value, Expr *expr)
{
	Type *type = type_lowering(expr->type);

	LLVMTypeRef llvm_type = llvm_get_type(c, type);

	BEValue val;
	LLVMValueRef vec_value;

	if (expr->expr_kind == EXPR_INITIALIZER_LIST)
	{
		vec_value = llvm_get_undef_raw(llvm_type);
		Expr **elements = expr->initializer_list;

		// Now walk through the elements.
		FOREACH_IDX(i, Expr *, element, elements)
		{
			llvm_emit_expr(c, &val, element);
			llvm_value_rvalue(c, &val);
			vec_value = llvm_update_vector(c, vec_value, val.value, (ArrayIndex)i);
		}
	}
	else
	{
		vec_value = llvm_get_zero_raw(llvm_type);
		Expr **elements = expr->designated_init_list;

		FOREACH(Expr *, designator, elements)
		{
			assert(vec_size(designator->designator_expr.path) == 1);
			DesignatorElement *element = designator->designator_expr.path[0];
			llvm_emit_expr(c, &val, designator->designator_expr.value);
			llvm_value_rvalue(c, &val);
			switch (element->kind)
			{
				case DESIGNATOR_ARRAY:
				{
					vec_value = llvm_update_vector(c, vec_value, val.value, element->index);
					break;
				}
				case DESIGNATOR_RANGE:
					for (ArrayIndex idx = element->index; idx <= element->index_end; idx++)
					{
						vec_value = llvm_update_vector(c, vec_value, val.value, idx);
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
	if (type_flat_is_vector(type))
	{
		llvm_emit_vector_initializer_list(c, value, expr);
		return;
	}
	assert(!IS_OPTIONAL(expr) || c->catch.block);
	llvm_value_set_address_abi_aligned(value, llvm_emit_alloca_aligned(c, type, "literal"), type);
	llvm_emit_initialize_reference(c, value, expr);
}

static void llvm_emit_macro_body_expansion(GenContext *c, BEValue *value, Expr *body_expr)
{
	Decl **declarations = body_expr->body_expansion_expr.declarations;
	Expr **values = body_expr->body_expansion_expr.values;

	DEBUG_PUSH_LEXICAL_SCOPE(c, body_expr->span);

	DebugScope *old_inline_loc = c->debug.block_stack;
	DebugScope *outline = llvm_use_debug(c) ? old_inline_loc->outline_loc : NULL;
	c->debug.block_stack = outline;

	// Create backend refs on demand.
	FOREACH_IDX(i, Decl *, decl, declarations)
	{
		if (!values[i]) continue;
		if (!decl->backend_ref) llvm_emit_local_var_alloca(c, decl);
	}

	c->debug.block_stack = old_inline_loc;

	// Set the values
	FOREACH_IDX(j, Expr *, expr, values)
	{
		if (!expr) continue;
		llvm_emit_expr(c, value, expr);
		llvm_store_to_ptr_aligned(c, declarations[j]->backend_ref, value, declarations[j]->alignment);
	}

	c->debug.block_stack = outline;
	AstId body = body_expr->body_expansion_expr.first_stmt;
	if (body) llvm_emit_stmt(c, astptr(body));

	c->debug.block_stack = old_inline_loc;

	DEBUG_POP_LEXICAL_SCOPE(c);
}

static inline void llvm_emit_try_unwrap(GenContext *c, BEValue *value, Expr *expr)
{
	if (!expr->try_unwrap_expr.optional)
	{
		LLVMValueRef fail_ref = decl_optional_ref(expr->try_unwrap_expr.decl);
		LLVMValueRef errv = llvm_load(c, llvm_get_type(c, type_anyfault), fail_ref, type_abi_alignment(type_anyfault), "load.err");
		LLVMValueRef result = LLVMBuildICmp(c->builder, LLVMIntEQ, errv, llvm_get_zero(c, type_anyfault), "result");
		llvm_value_set(value, result, type_bool);
		return;
	}
	BEValue addr;
	if (expr->try_unwrap_expr.assign_existing)
	{
		Expr *lhs = expr->try_unwrap_expr.lhs;
		if (lhs)
		{
			llvm_emit_expr(c, &addr, lhs);
		}
		else
		{
			llvm_emit_try_assign_try_catch(c, true, value, NULL, NULL, expr->try_unwrap_expr.optional);
			return;
		}
	}
	else
	{
		llvm_emit_local_decl(c, expr->try_unwrap_expr.decl, &addr);
		llvm_value_set_decl_address(c, &addr, expr->try_unwrap_expr.decl);
	}
	assert(llvm_value_is_addr(&addr));
	llvm_emit_try_assign_try_catch(c, true, value, &addr, NULL, expr->try_unwrap_expr.optional);
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
		llvm_emit_local_decl(c, expr->catch_unwrap_expr.decl, &addr);
		llvm_value_set_decl_address(c, &addr, expr->catch_unwrap_expr.decl);
	}
	else
	{
		LLVMValueRef temp_err = llvm_emit_alloca_aligned(c, type_anyfault, "temp_err");
		llvm_value_set_address_abi_aligned(&addr, temp_err, type_anyfault);
	}

	LLVMBasicBlockRef catch_block = llvm_basic_block_new(c, "end_block");

	PUSH_CATCH_VAR_BLOCK(addr.value, catch_block);

	FOREACH(Expr *, e, expr->catch_unwrap_expr.exprs)
	{
		BEValue val;
		LLVMBasicBlockRef block = llvm_basic_block_new(c, "testblock");
		llvm_emit_br(c, block);
		llvm_emit_block(c, block);
		llvm_emit_expr(c, &val, e);
		llvm_value_fold_optional(c, &val);
	}

	POP_CATCH();

	llvm_store_raw(c, &addr, llvm_get_zero(c, type_anyfault));
	llvm_emit_br(c, catch_block);
	llvm_emit_block(c, catch_block);
	llvm_value_rvalue(c, &addr);
	llvm_value_set(value, addr.value, type_anyfault);
}


static inline void llvm_emit_typeid_info(GenContext *c, BEValue *value, Expr *expr)
{
	llvm_emit_exprid(c, value, expr->typeid_info_expr.parent);
	llvm_value_rvalue(c, value);

	LLVMValueRef kind = NULL;
	LLVMValueRef ref = LLVMBuildIntToPtr(c->builder, value->value, c->ptr_type, "introspect*");
	AlignSize align = llvm_abi_alignment(c, c->introspect_type);
	AlignSize alignment;
	TypeIdInfoKind info_kind = expr->typeid_info_expr.kind;
	if (info_kind == TYPEID_INFO_PARENTOF)
	{
		LLVMValueRef parent = llvm_emit_struct_gep_raw(c, ref, c->introspect_type, INTROSPECT_INDEX_PARENTOF, align, &alignment);
		LLVMValueRef parent_value = llvm_load(c, c->typeid_type, parent, alignment, "typeid.parent");
		LLVMValueRef is_zero = LLVMBuildICmp(c->builder, LLVMIntEQ, parent_value, LLVMConstNull(c->typeid_type), "");
		parent_value = LLVMBuildSelect(c->builder, is_zero, llvm_get_typeid(c, type_void), parent_value, "");
		llvm_value_set(value, parent_value, expr->type);
		return;
	}
	bool safe_mode = safe_mode_enabled();
	if (safe_mode || info_kind == TYPEID_INFO_KIND)
	{
		kind = llvm_emit_struct_gep_raw(c, ref, c->introspect_type, INTROSPECT_INDEX_KIND, align, &alignment);
		kind = llvm_load(c, c->byte_type, kind, alignment, "typeid.kind");
	}
	switch (info_kind)
	{
		case TYPEID_INFO_KIND:
			llvm_value_set(value, kind, expr->type);
			return;
		case TYPEID_INFO_INNER:
			if (safe_mode)
			{
				BEValue check;
				LLVMBasicBlockRef exit = llvm_basic_block_new(c, "check_type_ok");
				IntrospectType checks[8] = { INTROSPECT_TYPE_ARRAY, INTROSPECT_TYPE_POINTER,
											 INTROSPECT_TYPE_VECTOR, INTROSPECT_TYPE_ENUM,
											 INTROSPECT_TYPE_SLICE, INTROSPECT_TYPE_DISTINCT,
											 INTROSPECT_TYPE_OPTIONAL, INTROSPECT_TYPE_BITSTRUCT };
				for (int i = 0; i < 8; i++)
				{
					llvm_emit_int_comp_raw(c,
										   &check,
										   type_char,
										   type_char,
										   kind,
										   llvm_const_int(c, type_char, checks[i]),
										   BINARYOP_EQ);
					LLVMBasicBlockRef next = llvm_basic_block_new(c, "check_next");
					llvm_emit_cond_br(c, &check, exit, next);
					llvm_emit_block(c, next);
				}
				llvm_emit_panic(c, "Attempted to access 'inner' on non composite type", expr->span, NULL, NULL);
				llvm_emit_block(c, exit);
				EMIT_LOC(c, expr);
			}
			{
				LLVMValueRef val = llvm_emit_struct_gep_raw(c, ref, c->introspect_type, INTROSPECT_INDEX_INNER, align, &alignment);
				val = llvm_load(c, c->typeid_type, val, alignment, "typeid.inner");
				llvm_value_set(value, val, expr->type);
				return;
			}
		case TYPEID_INFO_NAMES:
			if (safe_mode)
			{
				BEValue check;
				LLVMBasicBlockRef exit = llvm_basic_block_new(c, "check_type_ok");
				IntrospectType checks[1] = { INTROSPECT_TYPE_ENUM };
				for (int i = 0; i < 1; i++)
				{
					llvm_emit_int_comp_raw(c,
										   &check,
										   type_char,
										   type_char,
										   kind,
										   llvm_const_int(c, type_char, checks[i]),
										   BINARYOP_EQ);
					LLVMBasicBlockRef next = llvm_basic_block_new(c, "check_next");
					llvm_emit_cond_br(c, &check, exit, next);
					llvm_emit_block(c, next);
				}
				llvm_emit_panic(c, "Attempted to access 'names' on non enum/fault type.", expr->span, NULL, NULL);
				llvm_emit_block(c, exit);
				EMIT_LOC(c, expr);
			}
			{
				LLVMValueRef len = llvm_emit_struct_gep_raw(c, ref, c->introspect_type, INTROSPECT_INDEX_LEN, align, &alignment);
				len = llvm_load(c, c->size_type, len, alignment, "namelen");
				LLVMValueRef val = llvm_emit_struct_gep_raw(c, ref, c->introspect_type, INTROSPECT_INDEX_ADDITIONAL, align, &alignment);
				Type *slice = type_get_slice(type_chars);
				llvm_value_set(value, llvm_emit_aggregate_two(c, slice, val, len), slice);
				return;
			}
		case TYPEID_INFO_LEN:
			if (safe_mode)
			{
				BEValue check;
				LLVMBasicBlockRef exit = llvm_basic_block_new(c, "check_type_ok");
				IntrospectType checks[4] = { INTROSPECT_TYPE_ARRAY, INTROSPECT_TYPE_VECTOR,
											 INTROSPECT_TYPE_ENUM,
											 INTROSPECT_TYPE_SLICE };
				for (int i = 0; i < 4; i++)
				{
					llvm_emit_int_comp_raw(c,
										   &check,
										   type_char,
										   type_char,
										   kind,
										   llvm_const_int(c, type_char, checks[i]),
										   BINARYOP_EQ);
					LLVMBasicBlockRef next = llvm_basic_block_new(c, "check_next");
					llvm_emit_cond_br(c, &check, exit, next);
					llvm_emit_block(c, next);
				}
				llvm_emit_panic(c, "Attempted to access 'len' on non array type", expr->span, NULL, NULL);
				llvm_emit_block(c, exit);
				EMIT_LOC(c, expr);
			}
			{
				LLVMValueRef val = llvm_emit_struct_gep_raw(c, ref, c->introspect_type, INTROSPECT_INDEX_LEN, align, &alignment);
				val = llvm_load(c, c->size_type, val, alignment, "typeid.len");
				llvm_value_set(value, val, expr->type);
				return;
			}
		case TYPEID_INFO_SIZEOF:
			{
				LLVMValueRef val = llvm_emit_struct_gep_raw(c, ref, c->introspect_type, INTROSPECT_INDEX_SIZEOF, align, &alignment);
				val = llvm_load(c, c->size_type, val, alignment, "typeid.size");
				llvm_value_set(value, val, expr->type);
				return;
			}
		case TYPEID_INFO_PARENTOF:
			UNREACHABLE
	}
	UNREACHABLE
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
			llvm_value_rvalue(c, &res);
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
	llvm_new_phi(c, value, "chain.phi", type_bool, LLVMConstInt(c->bool_type, 1, 0),
				 next_block, llvm_get_zero_raw(c->bool_type), fail_block);
}

void llvm_emit_any_from_value(GenContext *c, BEValue *value, Type *type)
{
	llvm_value_addr(c, value);
	BEValue typeid;
	llvm_emit_typeid(c, &typeid, type);
	llvm_value_rvalue(c, &typeid);
	LLVMValueRef var = llvm_get_undef(c, type_any);
	var = llvm_emit_insert_value(c, var, value->value, 0);
	var = llvm_emit_insert_value(c, var, typeid.value, 1);
	llvm_value_set(value, var, type_any);
}


static inline void llvm_emit_type_from_any(GenContext *c, BEValue *be_value)
{
	if (llvm_value_is_addr(be_value))
	{
		AlignSize alignment = 0;
		LLVMValueRef pointer_addr = llvm_emit_struct_gep_raw(c,
															 be_value->value,
															 llvm_get_type(c, type_any),
															 1,
															 be_value->alignment,
															 &alignment);
		llvm_value_set_address(be_value, pointer_addr, type_typeid, alignment);
	}
	else
	{
		llvm_value_set(be_value, llvm_emit_extract_value(c, be_value->value, 1), type_typeid);
	}
}

static inline void llvm_emit_builtin_access(GenContext *c, BEValue *be_value, Expr *expr)
{
	Expr *inner = exprptr(expr->builtin_access_expr.inner);
	llvm_emit_expr(c, be_value, inner);
	llvm_value_fold_optional(c, be_value);
	switch (expr->builtin_access_expr.kind)
	{
		case ACCESS_LEN:
			llvm_emit_len_for_expr(c, be_value, be_value);
			return;
		case ACCESS_PTR:
			if (type_is_any(be_value->type))
			{
				llvm_emit_any_pointer(c, be_value, be_value);
				return;
			}
			assert(be_value->type->type_kind == TYPE_SLICE);
			llvm_emit_slice_pointer(c, be_value, be_value);
			return;
		case ACCESS_FAULTORDINAL:
		{
			LLVMBasicBlockRef current_block = llvm_get_current_block_if_in_use(c);
			if (!current_block)
			{
				llvm_value_set(be_value, llvm_get_zero(c, type_usz), type_usz);
				return;
			}
			assert(type_flatten(inner->type)->type_kind == TYPE_FAULTTYPE);
			llvm_value_rvalue(c, be_value);
			BEValue zero;
			LLVMBasicBlockRef exit_block = llvm_basic_block_new(c, "faultordinal_exit");
			LLVMBasicBlockRef ok_block = llvm_basic_block_new(c, "faultordinal_found");
			BEValue check;
			llvm_emit_int_comp_zero(c, &check, be_value, BINARYOP_EQ);
			llvm_emit_cond_br(c, &check, exit_block, ok_block);
			llvm_emit_block(c, ok_block);
			LLVMValueRef fault_data = LLVMBuildIntToPtr(c->builder, be_value->value,
														c->ptr_type, "");
			LLVMValueRef ptr = LLVMBuildStructGEP2(c->builder, c->fault_type, fault_data, 2, "");
			LLVMValueRef ordinal = llvm_load_abi_alignment(c, type_usz, ptr, "");
			llvm_emit_br(c, exit_block);
			llvm_emit_block(c, exit_block);
			llvm_new_phi(c, be_value, "faultname", type_usz, llvm_const_int(c, type_usz, 0), current_block, ordinal, ok_block);
			return;
		}
		case ACCESS_FAULTNAME:
		{
			Type *inner_type = type_no_optional(inner->type)->canonical;
			(void)inner_type;
			assert(inner_type->type_kind == TYPE_FAULTTYPE || inner_type->type_kind == TYPE_ANYFAULT);
			llvm_value_rvalue(c, be_value);
			LLVMValueRef val = llvm_emit_alloca_aligned(c, type_chars, "faultname_zero");
			BEValue zero;
			llvm_value_set_address_abi_aligned(&zero, val, type_chars);
			LLVMBasicBlockRef exit_block = llvm_basic_block_new(c, "faultname_exit");
			LLVMBasicBlockRef zero_block = llvm_basic_block_new(c, "faultname_no");
			LLVMBasicBlockRef ok_block = llvm_basic_block_new(c, "faultname_ok");
			BEValue check;
			llvm_emit_int_comp_zero(c, &check, be_value, BINARYOP_EQ);
			llvm_emit_cond_br(c, &check, zero_block, ok_block);
			llvm_emit_block(c, zero_block);
			llvm_store_zero(c, &zero);
			llvm_emit_br(c, exit_block);
			llvm_emit_block(c, ok_block);
			LLVMValueRef fault_data = LLVMBuildIntToPtr(c->builder, be_value->value,
														c->ptr_type, "");
			LLVMValueRef ptr = LLVMBuildStructGEP2(c->builder, c->fault_type, fault_data, 1, "");
			llvm_emit_br(c, exit_block);
			llvm_emit_block(c, exit_block);
			LLVMValueRef phi = LLVMBuildPhi(c->builder, c->ptr_type, "faultname");
			llvm_set_phi(phi, zero.value, zero_block, ptr, ok_block);
			llvm_value_set_address_abi_aligned(be_value, phi, type_chars);
			return;
		}
		case ACCESS_ENUMNAME:
		{
			Type *inner_type = type_no_optional(inner->type)->canonical;
			assert(inner_type->canonical->type_kind == TYPE_ENUM);
			llvm_value_rvalue(c, be_value);
			LLVMTypeRef slice = llvm_get_type(c, type_chars);
			LLVMValueRef to_introspect = LLVMBuildIntToPtr(c->builder, llvm_get_typeid(c, inner_type),
														  c->ptr_type, "");
			LLVMValueRef ptr = LLVMBuildStructGEP2(c->builder, c->introspect_type, to_introspect, INTROSPECT_INDEX_ADDITIONAL, "");
			LLVMValueRef val = llvm_zext_trunc(c, be_value->value, c->size_type);
			llvm_value_set_address(be_value, llvm_emit_pointer_gep_raw(c, slice, ptr, val),
								   type_chars, llvm_abi_alignment(c, slice));
			return;
		}
		case ACCESS_TYPEOFANYFAULT:
		{
			llvm_value_addr(c, be_value);
			LLVMValueRef value = llvm_load(c, c->ptr_type, be_value->value, be_value->alignment, "");
			llvm_value_set_address(be_value, value, type_typeid, type_alloca_alignment(type_typeid));
			return;
		}
		case ACCESS_TYPEOFANY:
			llvm_emit_type_from_any(c, be_value);
			return;
	}
	UNREACHABLE
}

static LLVMValueRef llvm_get_benchmark_hook_global(GenContext *c, Expr *expr)
{
	const char *name;
	switch (expr->benchmark_hook_expr)
	{
		case BUILTIN_DEF_BENCHMARK_FNS:
			name = benchmark_fns_var_name;
			break;
		case BUILTIN_DEF_BENCHMARK_NAMES:
			name = benchmark_names_var_name;
			break;
		default:
			UNREACHABLE
	}
	LLVMValueRef global = LLVMGetNamedGlobal(c->module, name);
	if (global) return global;
	global = LLVMAddGlobal(c->module, llvm_get_type(c, expr->type), name);
	LLVMSetExternallyInitialized(global, true);
	LLVMSetGlobalConstant(global, true);
	return global;
}

INLINE void llvm_emit_last_fault(GenContext *c, BEValue *value)
{
	assert(c->defer_error_var);
	llvm_value_set_address_abi_aligned(value, c->defer_error_var, type_anyfault);
}

INLINE void llmv_emit_benchmark_hook(GenContext *c, BEValue *value, Expr *expr)
{
	LLVMValueRef get_global = llvm_get_benchmark_hook_global(c, expr);
	llvm_value_set_address_abi_aligned(value, get_global, expr->type);
}

static LLVMValueRef llvm_get_test_hook_global(GenContext *c, Expr *expr)
{
	const char *name;
	switch (expr->test_hook_expr)
	{
		case BUILTIN_DEF_TEST_FNS:
			name = test_fns_var_name;
			break;
		case BUILTIN_DEF_TEST_NAMES:
			name = test_names_var_name;
			break;
		default:
			UNREACHABLE
	}
	LLVMValueRef global = LLVMGetNamedGlobal(c->module, name);
	if (global) return global;
	global = LLVMAddGlobal(c->module, llvm_get_type(c, expr->type), name);
	LLVMSetExternallyInitialized(global, true);
	LLVMSetGlobalConstant(global, true);
	return global;
}

static void llmv_emit_test_hook(GenContext *c, BEValue *value, Expr *expr)
{
	LLVMValueRef get_global = llvm_get_test_hook_global(c, expr);
	llvm_value_set_address_abi_aligned(value, get_global, expr->type);
}


static void llvm_emit_swizzle(GenContext *c, BEValue *value, Expr *expr)
{
	llvm_emit_exprid(c, value, expr->swizzle_expr.parent);
	llvm_value_rvalue(c, value);
	LLVMValueRef parent = value->value;
	LLVMTypeRef result_type = llvm_get_type(c, expr->type);
	unsigned vec_len = LLVMGetVectorSize(result_type);
	LLVMValueRef mask_val[4];
	assert(vec_len <= 4);
	const char *sw_ptr = expr->swizzle_expr.swizzle;
	for (unsigned i = 0; i < vec_len; i++)
	{
		int index = (swizzle[(int)sw_ptr[i]] - 1) & 0xF;
		mask_val[i] = llvm_const_int(c, type_uint, index);
	}
	LLVMValueRef res = LLVMBuildShuffleVector(c->builder, parent, LLVMGetUndef(LLVMTypeOf(parent)), LLVMConstVector(mask_val, vec_len), sw_ptr);
	llvm_value_set(value, res, expr->type);
}

static void llvm_emit_default_arg(GenContext *c, BEValue *value, Expr *expr)
{
	if (llvm_use_debug(c))
	{
		SourceSpan location = expr->span;
		const char *name = "[DEFAULT INIT]";
		size_t namelen = strlen(name);
		LLVMMetadataRef file = llvm_get_debug_file(c, location.file_id);
		LLVMMetadataRef init_def = LLVMDIBuilderCreateFunction(c->debug.builder, file, name, namelen, name, namelen,
																 file, location.row, NULL, true, true, location.row, LLVMDIFlagZero, false);
		llvm_emit_debug_location(c, expr->default_arg_expr.loc);
		DebugScope scope = { .lexical_block = init_def, .inline_loc = c->last_loc };
		DebugScope *old = c->debug.block_stack;
		c->debug.block_stack = &scope;
		llvm_emit_expr(c, value, expr->default_arg_expr.inner);
		llvm_value_fold_optional(c, value);
		c->debug.block_stack = old;
		c->last_emitted_loc.a = 0;
	}
	else
	{
		llvm_emit_expr(c, value, expr->default_arg_expr.inner);
		llvm_value_fold_optional(c, value);
	}
}

void llvm_emit_expr_global_value(GenContext *c, BEValue *value, Expr *expr)
{
	sema_cast_const(expr);
	llvm_emit_expr(c, value, expr);
	assert(!llvm_value_is_addr(value));
}
void llvm_emit_expr(GenContext *c, BEValue *value, Expr *expr)
{
	EMIT_LOC(c, expr);
	switch (expr->expr_kind)
	{
		case NON_RUNTIME_EXPR:
		case EXPR_LAMBDA:
		case EXPR_COND:
		case EXPR_ASM:
		case EXPR_VASPLAT:
		case EXPR_GENERIC_IDENT:
		case EXPR_EMBED:
		case EXPR_MACRO_BODY:
		case EXPR_OTHER_CONTEXT:
		case EXPR_DESIGNATOR:
		case EXPR_MEMBER_GET:
		case EXPR_NAMED_ARGUMENT:
			UNREACHABLE
		case EXPR_DEFAULT_ARG:
			llvm_emit_default_arg(c, value, expr);
			return;
		case EXPR_SWIZZLE:
			llvm_emit_swizzle(c, value, expr);
			return;
		case EXPR_BENCHMARK_HOOK:
			llmv_emit_benchmark_hook(c, value, expr);
			return;
		case EXPR_LAST_FAULT:
			llvm_emit_last_fault(c, value);
			return;
		case EXPR_TEST_HOOK:
			llmv_emit_test_hook(c, value, expr);
			return;
		case EXPR_BUILTIN_ACCESS:
			llvm_emit_builtin_access(c, value, expr);
			return;
		case EXPR_RETVAL:
			*value = c->retval;
			return;
		case EXPR_TRY_UNWRAP_CHAIN:
			llvm_emit_try_unwrap_chain(c, value, expr);
			return;
		case EXPR_MACRO_BLOCK:
			llvm_emit_macro_block(c, value, expr);
			return;
		case EXPR_TRY_UNWRAP:
			llvm_emit_try_unwrap(c, value, expr);
			return;
		case EXPR_CATCH_UNWRAP:
			llvm_emit_catch_unwrap(c, value, expr);
			return;
		case EXPR_TYPEID_INFO:
			llvm_emit_typeid_info(c, value, expr);
			return;
		case EXPR_BUILTIN:
			UNREACHABLE;
		case EXPR_DECL:
			llvm_emit_local_decl(c, expr->decl_expr, value);
			return;
		case EXPR_SLICE_COPY:
			llvm_emit_slice_copy(c, value, expr);
			return;
		case EXPR_SLICE_ASSIGN:
			llvm_emit_slice_assign(c, value, expr);
			return;
		case EXPR_SLICE:
			gencontext_emit_slice(c, value, expr);
			return;
		case EXPR_POINTER_OFFSET:
			llvm_emit_pointer_offset(c, value, expr);
			return;
		case EXPR_OPTIONAL:
			llvm_emit_optional(c, value, expr);
			return;
		case EXPR_NOP:
			llvm_value_set(value, NULL, type_void);
			return;
		case EXPR_EXPR_BLOCK:
			llvm_emit_expr_block(c, value, expr);
			return;
		case EXPR_COMPOUND_LITERAL:
		case EXPR_OPERATOR_CHARS:
			UNREACHABLE
		case EXPR_INITIALIZER_LIST:
		case EXPR_DESIGNATED_INITIALIZER_LIST:
			llvm_emit_initializer_list_expr(c, value, expr);
			return;
		case EXPR_UNARY:
			llvm_emit_unary_expr(c, value, expr);
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
		case EXPR_TYPEID:
		case EXPR_SUBSCRIPT_ASSIGN:
			// These are folded in the semantic analysis step.
			UNREACHABLE
		case EXPR_IDENTIFIER:
			llvm_value_set_decl(c, value, expr->identifier_expr.decl);
			return;
		case EXPR_SUBSCRIPT:
			llvm_emit_subscript(c, value, expr);
			return;
		case EXPR_SUBSCRIPT_ADDR:
			llvm_emit_subscript_addr(c, value, expr);
			assert(llvm_value_is_addr(value));
			llvm_value_fold_optional(c, value);
			value->kind = BE_VALUE;
			value->type = type_get_ptr(value->type);
			return;
		case EXPR_ACCESS:
			llvm_emit_access_addr(c, value, expr);
			return;
		case EXPR_CALL:
			llvm_emit_call_expr(c, value, expr, NULL);
			return;
		case EXPR_EXPRESSION_LIST:
			llvm_emit_expression_list_expr(c, value, expr);
			return;
		case EXPR_CAST:
			llvm_emit_cast_expr(c, value, expr);
			return;
		case EXPR_BITACCESS:
			llvm_emit_bitaccess(c, value, expr);
			return;
	}
	UNREACHABLE
}


