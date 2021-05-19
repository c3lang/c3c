// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_internal.h"
#include "compiler_internal.h"
#include "bigint.h"

static void gencontext_emit_unary_expr(GenContext *context, BEValue *value, Expr *expr);
static inline void llvm_emit_post_inc_dec(GenContext *c, BEValue *value, Expr *expr, int diff, bool use_mod);
static inline void llvm_emit_pre_inc_dec(GenContext *c, BEValue *value, Expr *expr, int diff, bool use_mod);
static inline void llvm_emit_inc_dec_change(GenContext *c, bool use_mod, BEValue *addr, BEValue *after, BEValue *before, Expr *expr, int diff);
static void llvm_emit_post_unary_expr(GenContext *context, BEValue *be_value, Expr *expr);
static inline LLVMValueRef llvm_emit_subscript_addr_with_base_new(GenContext *c, BEValue *parent, BEValue *index);
static void llvm_emit_initialize_designated(GenContext *c, BEValue *ref, uint64_t offset, DesignatorElement** current, DesignatorElement **last, Expr *expr, BEValue *emitted_value);

LLVMValueRef llvm_emit_is_no_error_value(GenContext *c, BEValue *value)
{
	assert(value->kind == BE_ADDRESS);
	LLVMValueRef val = LLVMBuildStructGEP2(c->builder, llvm_get_type(c, value->type), value->value, 0, "err_domain");
	LLVMValueRef domain = llvm_emit_load_aligned(c, llvm_get_type(c, type_uptr), val, value->alignment, "");
	return LLVMBuildICmp(c->builder, LLVMIntEQ, domain, llvm_get_zero(c, type_uptr), "not_err");
}

LLVMTypeRef llvm_const_padding_type(GenContext *c, ByteSize size)
{
	assert(size > 0);
	if (size == 1) return llvm_get_type(c, type_char);
	return LLVMArrayType(llvm_get_type(c, type_char), size);
}

LLVMValueRef llvm_emit_const_padding(GenContext *c, ByteSize size)
{
	return LLVMGetUndef(llvm_const_padding_type(c, size));
}

static inline LLVMValueRef llvm_emit_add_int(GenContext *c, Type *type, LLVMValueRef left, LLVMValueRef right)
{
	if (active_target.feature.trap_on_wrap)
	{
		LLVMTypeRef type_to_use = llvm_get_type(c, type->canonical);
		LLVMValueRef args[2] = { left, right };
		assert(type->canonical == type);
		LLVMValueRef add_res;
		if (type_is_unsigned(type))
		{
			add_res = llvm_emit_call_intrinsic(c, intrinsic_id_uadd_overflow, &type_to_use, 1, args, 2);
		}
		else
		{
			add_res = llvm_emit_call_intrinsic(c, intrinsic_id_sadd_overflow, &type_to_use, 1, args, 2);
		}
		LLVMValueRef result = LLVMBuildExtractValue(c->builder, add_res, 0, "");
		LLVMValueRef ok = LLVMBuildExtractValue(c->builder, add_res, 1, "");
		llvm_emit_panic_on_true(c, ok, "Addition overflow");
		return result;
	}

	return LLVMBuildAdd(c->builder, left, right, "add");
}

LLVMValueRef llvm_emit_coerce(GenContext *c, LLVMTypeRef coerced, BEValue *value, Type *original_type)
{
	LLVMValueRef cast;
	AlignSize target_alignment = llvm_abi_alignment(c, coerced);
	AlignSize max_align = MAX(value->alignment, llvm_abi_alignment(c, coerced));

	// If we are loading something with greater alignment than what we have, we cannot directly memcpy.
	if (llvm_value_is_addr(value) && value->alignment < target_alignment)
	{
		// So load it instead.
		llvm_value_rvalue(c, value);
	}

	// In this case we have something nicely aligned, so we just do a cast.
	if (llvm_value_is_addr(value))
	{
		cast = LLVMBuildBitCast(c->builder, value->value, LLVMPointerType(coerced, 0), "");
	}
	else
	{
		cast = llvm_emit_alloca(c, coerced, max_align, "coerce");
		LLVMValueRef target = LLVMBuildBitCast(c->builder, cast, llvm_get_ptr_type(c, value->type), "");
		llvm_store_bevalue_aligned(c, target, value, max_align);
	}
	return llvm_emit_load_aligned(c, coerced, cast, max_align, "coerced");
}

void llvm_emit_convert_value_from_coerced(GenContext *c, BEValue *result, LLVMTypeRef coerced, LLVMValueRef value, Type *original_type)
{
	unsigned max_align = MAX(llvm_abi_alignment(c, coerced), type_abi_alignment(original_type));
	LLVMValueRef temp = llvm_emit_alloca(c, coerced, max_align, "coerce_temp");
	llvm_store_aligned(c, temp, value, max_align);
	temp = LLVMBuildBitCast(c->builder, temp, llvm_get_type(c, type_get_ptr(original_type)), "");
	llvm_value_set_address_align(result, temp, original_type, max_align);
}

static inline LLVMValueRef
llvm_emit_sub_int(GenContext *c, Type *type, LLVMValueRef left, LLVMValueRef right)
{
	if (active_target.feature.trap_on_wrap)
	{
		LLVMTypeRef type_to_use = llvm_get_type(c, type);
		LLVMValueRef args[2] = { left, right };
		assert(type->canonical == type);
		LLVMValueRef add_res;
		if (type_is_unsigned(type))
		{
			add_res = llvm_emit_call_intrinsic(c, intrinsic_id_usub_overflow, &type_to_use, 1, args, 2);
		}
		else
		{
			add_res = llvm_emit_call_intrinsic(c, intrinsic_id_ssub_overflow, &type_to_use, 1, args, 2);
		}
		LLVMValueRef result = LLVMBuildExtractValue(c->builder, add_res, 0, "");
		LLVMValueRef ok = LLVMBuildExtractValue(c->builder, add_res, 1, "");
		llvm_emit_panic_on_true(c, ok, "Subtraction overflow");
		return result;
	}

	return LLVMBuildSub(c->builder, left, right, "sub");
}

static inline void llvm_emit_subscript_addr_base(GenContext *context, BEValue *value, Expr *parent)
{
	LLVMValueRef parent_value;
	Type *type = type_flatten(parent->type);
	llvm_emit_expr(context, value, parent);
	llvm_emit_ptr_from_array(context, value);
}

static inline LLVMValueRef llvm_emit_subscript_addr_with_base(GenContext *c, Type *parent_type, LLVMValueRef parent_value, LLVMValueRef index_value)
{
	Type *type = parent_type;
	switch (type->type_kind)
	{
		case TYPE_POINTER:
			return LLVMBuildInBoundsGEP2(c->builder,
			                             llvm_get_pointee_type(c, type),
			                             parent_value, &index_value, 1, "ptridx");
		case TYPE_ARRAY:
		{
			// TODO insert trap on overflow.
			LLVMValueRef zero = llvm_get_zero(c, type_int);
			LLVMValueRef indices[2] = {
					zero,
					index_value,
			};
			return LLVMBuildInBoundsGEP2(c->builder,
			                             llvm_get_type(c, type),
			                             parent_value, indices, 2, "arridx");
		}
		case TYPE_SUBARRAY:
		{
			// TODO insert trap on overflow.
			return LLVMBuildInBoundsGEP2(c->builder,
			                             llvm_get_type(c, type->array.base),
			                             parent_value, &index_value, 1, "sarridx");
		}
		case TYPE_VARARRAY:
			TODO
		default:
			UNREACHABLE

	}
}

static void llvm_emit_array_bounds_check(GenContext *c, BEValue *index, LLVMValueRef array_max_index)
{
	// Negative values are not allowed.
	if (type_is_signed(index->type))
	{
		LLVMValueRef index_negative = llvm_emit_int_comparison(c, index->type, index->type, index->value,
														 llvm_get_zero(c, index->type), BINARYOP_LT);
		llvm_emit_panic_on_true(c, index_negative, "Negative array indexing");
	}
	LLVMValueRef exceeds_max_index = llvm_emit_int_comparison(c, index->type, index->type,
	                                                          index->value, array_max_index,
	                                                          BINARYOP_GE);
	llvm_emit_panic_on_true(c, exceeds_max_index, "Array index out of bounds");
}

static inline LLVMValueRef llvm_emit_subscript_addr_with_base_new(GenContext *c, BEValue *parent, BEValue *index)
{
	assert(llvm_value_is_addr(parent));
	Type *type = type_flatten(parent->type);
	switch (type->type_kind)
	{
		case TYPE_POINTER:
			return LLVMBuildInBoundsGEP(c->builder, parent->value, &index->value, 1, "ptridx");
		case TYPE_ARRAY:
		{
			if (active_target.feature.safe_mode)
			{
				llvm_emit_array_bounds_check(c, index, llvm_const_int(c, index->type, type->array.len));
			}
			LLVMValueRef zero = llvm_get_zero(c, index->type);
			LLVMValueRef indices[2] = {
					zero,
					index->value,
			};
			return LLVMBuildInBoundsGEP2(c->builder,
			                             llvm_get_type(c, type),
			                             parent->value, indices, 2, "arridx");
		}
		case TYPE_SUBARRAY:
		{
			if (active_target.feature.safe_mode)
			{
				// TODO insert trap on overflow.
			}
			return LLVMBuildInBoundsGEP2(c->builder,
			                             llvm_get_type(c, type->array.base),
			                             parent->value, &index->value, 1, "sarridx");
		}
		case TYPE_VARARRAY:
			// TODO insert trap on overflow.
			TODO
		case TYPE_STRLIT:
			// TODO insert trap on overflow.
			return LLVMBuildInBoundsGEP(c->builder, parent->value, &index->value, 1, "ptridx");
		default:
			UNREACHABLE

	}
}

/**
 * Expand foo[123] or someCall()[n] or some such.
 * Evaluation order is left to right.
 */
static inline void gencontext_emit_subscript(GenContext *c, BEValue *value, Expr *expr)
{
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

	// TODO set alignment
	llvm_value_set_address(value, llvm_emit_subscript_addr_with_base_new(c, &ref, &index), expr->type);
}


static int find_member_index(Decl *parent, Decl *member)
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
		int index = find_member_index(parent, member);
		assert(index > -1);
		found = parent->strukt.members[index];
		const char *name = found->name ? found->name : "anon";
		switch (parent->type->canonical->type_kind)
		{
			case TYPE_UNION:
				llvm_value_addr(c, value);
				llvm_value_set_address_align(value, llvm_emit_bitcast(c, value->value, type_get_ptr(found->type)), found->type, value->alignment);
				break;
			case TYPE_ERRTYPE:
			case TYPE_STRUCT:
				llvm_value_struct_gep(c, value, value, index);
				break;
			default:
				UNREACHABLE
		}
		parent = found;
	} while (found != member);
}


static inline void gencontext_emit_access_addr(GenContext *context, BEValue *be_value, Expr *expr)
{
	Expr *parent = expr->access_expr.parent;
	llvm_emit_expr(context, be_value, parent);
	Decl *member = expr->access_expr.ref;

	gencontext_emit_member_addr(context, be_value, type_flatten(parent->type)->decl, member);
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
static void gencontext_emit_arr_to_subarray_cast(GenContext *c, BEValue *value, Type *to_type, Type *from_type)
{
	llvm_value_rvalue(c, value);
	printf("TODO optimize subarray cast\n");
	ByteSize size = from_type->pointer->array.len;
	LLVMTypeRef subarray_type = llvm_get_type(c, to_type);
	LLVMValueRef result = LLVMGetUndef(subarray_type);
	value->value = llvm_emit_bitcast(c, value->value, type_get_ptr(from_type->pointer->array.base));
	result = LLVMBuildInsertValue(c->builder, result, value->value, 0, "");
	value->value = LLVMBuildInsertValue(c->builder, result, llvm_const_int(c, type_usize, size), 1, "");
}


LLVMValueRef gencontext_emit_value_bitcast(GenContext *context, LLVMValueRef value, Type *to_type, Type *from_type)
{
	LLVMValueRef ptr = llvm_emit_alloca_aligned(context, from_type, "");
	LLVMBuildStore(context->builder, value, ptr);
	LLVMValueRef ptr_cast = llvm_emit_bitcast(context, ptr, type_get_ptr(to_type));
	return gencontext_emit_load(context, to_type, ptr_cast);
}


void llvm_emit_cast(GenContext *c, CastKind cast_kind, BEValue *value, Type *to_type, Type *from_type)
{
	to_type = type_flatten(to_type);
	from_type = type_flatten(from_type);

	switch (cast_kind)
	{
		case CAST_VRBOOL:
		case CAST_VRPTR:
		case CAST_PTRVR:
			TODO

		case CAST_CXBOOL:
			TODO
		case CAST_XIERR:
			// TODO Insert zero check.
			llvm_value_rvalue(c, value);
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
				value->value = LLVMConstBitCast(value->value, llvm_get_type(c, to_type));
			}
			break;
		case CAST_APTSA:
			gencontext_emit_arr_to_subarray_cast(c, value, to_type, from_type);
			break;
		case CAST_SAPTR:
			if (llvm_value_is_addr(value))
			{
				llvm_value_fold_failable(c, value);
				value->value = llvm_emit_load_aligned(c, llvm_get_type(c, to_type),
				                                      LLVMBuildStructGEP(c->builder, value->value, 0, ""),
				                                      value->alignment, "");
			}
			else
			{
				value->value = LLVMBuildExtractValue(c->builder, value->value, 0, "");
			}
			break;
		case CAST_VARPTR:
			break;
		case CAST_ARRPTR:
			TODO
		case CAST_EREU:
		case CAST_EUER:
			TODO // gencontext_emit_value_bitcast(c, value->value, to_type, from_type);
		case CAST_EUBOOL:
			if (value->kind == BE_VALUE)
			{
				value->value = LLVMBuildExtractValue(c->builder, value->value, 0, "");
			}
			else
			{
				value->value = LLVMBuildStructGEP2(c->builder, llvm_get_type(c, type_error), value->value, 0, "");
				value->value = llvm_emit_load_aligned(c,
				                                      llvm_get_type(c, type_usize),
				                                      value->value,
				                                      type_abi_alignment(type_usize),
				                                      "");
			}
			value->value = LLVMBuildICmp(c->builder, LLVMIntNE, value->value, llvm_get_zero(c, type_usize), "eubool");
			value->kind = BE_BOOLEAN;
			break;
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
		case CAST_PTRVAR:
		case CAST_VARSA:
		case CAST_VARVAR:
		case CAST_VARBOOL:
		case CAST_BOOLBOOL:
		case CAST_SABOOL:
			TODO
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
		unsigned index = current_element->index;
		LLVMValueRef val = llvm_value_rvalue_store(c, &res);
		switch (current_element->kind)
		{
			case DESIGNATOR_FIELD:
				return LLVMConstInsertValue(parent, val, &index, 1);
			case DESIGNATOR_ARRAY:
				return LLVMConstInsertElement(parent, val, llvm_const_int(c, type_isize, current_element->index));
			case DESIGNATOR_RANGE:
				for (int64_t i = current_element->index; i <= current_element->index_end; i++)
				{
					parent = LLVMConstInsertElement(parent, val, llvm_const_int(c, type_isize, i));
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
			unsigned index = current_element->index;
			current_val = LLVMConstExtractValue(parent, &index, 1);
			current_val = llvm_recursive_set_value(c, current_element_ptr + 1, current_val, last_element_ptr, value);
			return LLVMConstInsertValue(parent, current_val, &index, 1);
		}
		case DESIGNATOR_ARRAY:
		{
			LLVMValueRef index = llvm_const_int(c, type_isize, current_element->index);
			current_val = LLVMConstExtractElement(parent, index);
			current_val = llvm_recursive_set_value(c, current_element_ptr + 1, current_val, last_element_ptr, value);
			return LLVMConstInsertElement(parent, current_val, index);
		}
		case DESIGNATOR_RANGE:
			for (int64_t i = current_element->index; i <= current_element->index_end; i++)
			{
				LLVMValueRef index = llvm_const_int(c, type_isize, i);
				current_val = LLVMConstExtractElement(parent, index);
				current_val = llvm_recursive_set_value(c, current_element_ptr + 1, current_val, last_element_ptr, value);
				parent = LLVMConstInsertElement(parent, current_val, index);
			}
			return parent;
		default:
			UNREACHABLE
	}
}
static LLVMValueRef llvm_recursive_set_const_value(GenContext *context, DesignatorElement **path, LLVMValueRef value, Type *parent_type, Expr *assign)
{
	unsigned path_count = vec_size(path);
	return llvm_recursive_set_value(context, path, value, path + (path_count - 1), assign);
}


static inline void llvm_emit_initialize_reference_temporary_const(GenContext *c, BEValue *ref, Expr *expr)
{
	bool modified = false;
	// First create the constant value.

	Type *canonical = expr->type->canonical;

	LLVMValueRef value = llvm_emit_const_aggregate(c, expr, &modified);

	// Create a global const.
	LLVMTypeRef type = modified ? LLVMTypeOf(value) : llvm_get_type(c, expr->type);
	LLVMValueRef global_copy = LLVMAddGlobal(c->module, type, ".__const");
	LLVMSetLinkage(global_copy, LLVMPrivateLinkage);

	// Set a nice alignment
	unsigned alignment = type_alloca_alignment(expr->type);
	llvm_set_alignment(global_copy, alignment);

	// Set the value and make it constant
	LLVMSetInitializer(global_copy, value);
	LLVMSetGlobalConstant(global_copy, true);

	// Ensure we have a reference.
	llvm_value_addr(c, ref);

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
			ArrayIndex size = array_type->array.len;
			LLVMTypeRef array_type_llvm = llvm_get_type(c, array_type);
			LLVMTypeRef element_type_llvm = llvm_get_type(c, element_type);
			assert(size <= UINT32_MAX);
			for (ArrayIndex i = 0; i < size; i++)
			{
				LLVMValueRef index = llvm_const_int(c, type_uint, i);
				LLVMValueRef array_pointer = LLVMBuildInBoundsGEP2(c->builder, element_type_llvm, array_ref, &index, 1, "");
				BEValue value;
				llvm_value_set_address(&value, array_pointer, element_type);
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
			LLVMTypeRef element_type_llvm = llvm_get_type(c, element_type);
			ConstInitializer **elements = const_init->init_array.elements;
			unsigned element_count = vec_size(elements);
			ArrayIndex current_index = 0;
			LLVMValueRef *parts = NULL;
			VECEACH(elements, i)
			{
				ConstInitializer *element = elements[i];
				assert(element->kind == CONST_INIT_ARRAY_VALUE);
				ArrayIndex element_index = element->init_array_value.index;
				LLVMValueRef index = llvm_const_int(c, element_index <= UINT32_MAX ? type_uint : type_usize, element_index);
				LLVMValueRef array_pointer = LLVMBuildInBoundsGEP2(c->builder, element_type_llvm, array_ref, &index, 1, "");
				BEValue value;
				llvm_value_set_address(&value, array_pointer, element_type);
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
			MemberIndex count = vec_size(members);
			LLVMValueRef *entries = NULL;
			for (MemberIndex i = 0; i < count; i++)
			{
				BEValue value;
				llvm_value_struct_gep(c, &value, ref, i);
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
	ConstInitializer *initializer = expr->initializer_expr.initializer;

	// Make sure we have an address.
	llvm_value_addr(c, ref);

	llvm_emit_inititialize_reference_const(c, ref, initializer);

}

static inline void llvm_emit_initialize_reference_list(GenContext *c, BEValue *ref, Expr *expr)
{
	// Getting ready to initialize, get the real type.
	Type *real_type = type_flatten(ref->type);
	Expr **elements = expr->initializer_expr.initializer_expr;

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
	Type *array_element_type = is_array ? real_type->array.base : NULL;
	LLVMTypeRef array_element_type_llvm = is_array ? llvm_get_type(c, real_type->array.base) : NULL;
	// Now walk through the elements.
	VECEACH(elements, i)
	{
		Expr *element = elements[i];
		if (element->expr_kind == EXPR_COMPOUND_LITERAL)
		{
			element = element->expr_compound_literal.initializer;
		}
		unsigned offset = 0;
		BEValue pointer;
		if (is_struct)
		{
			Decl *member = real_type->decl->strukt.members[i];
			offset = member->offset;
			llvm_value_struct_gep(c, &pointer, ref, i);
		}
		else if (is_array)
		{
			// Todo optimize
			offset = i * type_size(array_element_type);
			LLVMValueRef index = llvm_const_int(c, type_uint, i);
			LLVMValueRef ptr = LLVMBuildInBoundsGEP2(c->builder, array_element_type_llvm, value, &index, 1, "");
			unsigned alignment = type_min_alignment(offset, ref->alignment);
			llvm_value_set_address_align(&pointer, ptr, element->type, alignment);
		}
		else
		{
			llvm_value_set_address_align(&pointer, value, element->type, ref->alignment);
		}
		// If this is an initializer, we want to actually run the initialization recursively.
		if (element->expr_kind == EXPR_INITIALIZER_LIST)
		{
			llvm_emit_initialize_reference(c, &pointer, element);
			continue;
		}
		BEValue init_value;
		llvm_emit_expr(c, &init_value, element);
		llvm_store_bevalue(c, &pointer, &init_value);
	}
}

static void llvm_emit_initialize_designated_const_range(GenContext *c, BEValue *ref, uint64_t offset, DesignatorElement** current, DesignatorElement **last, Expr *expr, BEValue *emitted_value)
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
	// Assign the index_ptr to the start value.
	LLVMValueRef indices[2] = {
			llvm_get_zero(c, curr->index_expr->type),
			NULL
	};
	for (unsigned i = curr->index; i <= curr->index_end; i++)
	{
		indices[1] = llvm_const_int(c, curr->index_expr->type, i);
		BEValue new_ref;
		LLVMValueRef ptr = LLVMBuildInBoundsGEP2(c->builder, ref_type, ref->value, indices, 2, "");
		llvm_value_set_address(&new_ref, ptr, type_get_indexed_type(ref->type));
		llvm_emit_initialize_designated(c, &new_ref, offset, current + 1, last, expr, emitted_value);
	}
}

static void llvm_emit_initialize_designated(GenContext *c, BEValue *ref, uint64_t offset, DesignatorElement** current,
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
		if (expr->expr_kind == EXPR_INITIALIZER_LIST)
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
				llvm_value_struct_gep(c, &value, ref, curr->index);
			}
			llvm_emit_initialize_designated(c, &value, offset, current + 1, last, expr, emitted_value);
			break;
		}
		case DESIGNATOR_ARRAY:
		{
			Type *type = ref->type->array.base;
			LLVMValueRef indices[2];
			offset += curr->index * type_size(type);
			Type *index_type = curr->index > UINT32_MAX ? type_uint : type_ulong;
			indices[0] = llvm_const_int(c, index_type, 0);
			indices[1] = llvm_const_int(c, index_type, curr->index);
			LLVMValueRef ptr = LLVMBuildInBoundsGEP2(c->builder, llvm_get_type(c, ref->type), ref->value, indices, 2, "");
			llvm_value_set_address_align(&value, ptr, type, type_min_alignment(offset, type_abi_alignment(type)));
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
	Type *real_type = type_flatten(ref->type);
	Expr **elements = expr->initializer_expr.initializer_expr;
	assert(vec_size(elements));

	// Make sure we have an address.
	llvm_value_addr(c, ref);

	// Clear the memory if not union.
	if (real_type->type_kind != TYPE_UNION) llvm_emit_memclear(c, ref);

	LLVMValueRef value = ref->value;

	// Now walk through the elements.
	VECEACH(elements, i)
	{
		Expr *designator = elements[i];
		DesignatorElement **last_element = designator->designator_expr.path + vec_size(designator->designator_expr.path) - 1;
		llvm_emit_initialize_designated(c, ref, 0, designator->designator_expr.path, last_element, designator->designator_expr.value, NULL);
	}
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
	// In the case of a const, we have some optimizations:
	if (expr->initializer_expr.init_type == INITIALIZER_CONST)
	{
		ConstInitializer *initializer = expr->initializer_expr.initializer;
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
	}

	switch (expr->initializer_expr.init_type)
	{
		case INITIALIZER_CONST:
			// Just clear memory
			llvm_emit_initialize_reference_const(c, ref, expr);
			break;
		case INITIALIZER_NORMAL:
			llvm_emit_initialize_reference_list(c, ref, expr);
			break;
		case INITIALIZER_DESIGNATED:
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
			LLVMValueRef add = LLVMConstInt(diff < 0 ? llvm_get_type(c, type_ichar) : llvm_get_type(c, type_char), diff, diff < 0);
			after_value = LLVMBuildGEP2(c->builder, llvm_get_pointee_type(c, type), value.value, &add, 1, "ptrincdec");
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
					? llvm_emit_add_int(c, type, value.value, diff_value)
					: llvm_emit_sub_int(c, type, value.value, diff_value);
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
	LLVMValueRef llvm_value;
	switch (expr->unary_expr.operator)
	{
		case UNARYOP_ERROR:
			FATAL_ERROR("Illegal unary op %s", expr->unary_expr.operator);
		case UNARYOP_TADDR:
			llvm_value = llvm_emit_alloca_aligned(c, expr->unary_expr.expr->type, "taddr");
			llvm_emit_expr(c, value, expr->unary_expr.expr);
			llvm_store_bevalue_dest_aligned(c, llvm_value, value);
			llvm_value_set(value, llvm_value, type);
			return;
		case UNARYOP_NOT:
			llvm_emit_expr(c, value, expr->unary_expr.expr);
			llvm_value_rvalue(c, value);
			if (type_is_float(type))
			{
				llvm_value = LLVMBuildFCmp(c->builder, LLVMRealUNE, value->value, llvm_get_zero(c, type), "not");
			}
			else if (type->type_kind == TYPE_BOOL)
			{
				llvm_value = LLVMBuildNot(c->builder, value->value, "not");
			}
			else
			{
				llvm_value = LLVMBuildICmp(c->builder, LLVMIntEQ, value->value, llvm_get_zero(c, type), "not");
			}
			llvm_value_set_bool(value, llvm_value);
			return;
		case UNARYOP_BITNEG:
			llvm_emit_expr(c, value, expr->unary_expr.expr);
			llvm_value_rvalue(c, value);
			value->value = LLVMBuildNot(c->builder, value->value, "bnot");
			return;
		case UNARYOP_NEG:
			llvm_emit_expr(c, value, expr->unary_expr.expr);
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
				LLVMValueRef call_res = llvm_emit_call_intrinsic(c, intrinsic_id_ssub_overflow,
				                                                 &type_to_use, 1, args, 2);
				value->value = LLVMBuildExtractValue(c->builder, call_res, 0, "");
				LLVMValueRef ok = LLVMBuildExtractValue(c->builder, call_res, 1, "");
				llvm_emit_panic_on_true(c, ok, "Signed negation overflow");
				return;
			}
			value->value = LLVMBuildNeg(c->builder, value->value, "neg");
			return;
		case UNARYOP_ADDR:
			llvm_emit_expr(c, value, expr->unary_expr.expr);
			// Create an addr
			llvm_value_addr(c, value);
			// Transform to value
			value->kind = BE_VALUE;
			value->type = type_flatten(expr->type);
			return;
		case UNARYOP_DEREF:
			llvm_emit_expr(c, value, expr->unary_expr.expr);
			// Load the pointer value.
			llvm_value_rvalue(c, value);
			// Convert pointer to address
			value->kind = BE_ADDRESS;
			value->type = type_flatten(expr->type);
			return;
		case UNARYOP_INC:
			llvm_emit_pre_inc_dec(c, value, expr->unary_expr.expr, 1, false);
			return;
		case UNARYOP_DEC:
			llvm_emit_pre_inc_dec(c, value, expr->unary_expr.expr, -1, false);
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
			LLVMValueRef len_addr = LLVMBuildStructGEP2(c->builder, subarray_type, expr_to_len->value, 1, "len");
			llvm_value_set_address(be_value, len_addr, type_usize);
			break;
		}
		case TYPE_ARRAY:
			llvm_value_set(be_value, llvm_const_int(c, type_usize, expr_to_len->type->array.len), type_usize);
			break;
		case TYPE_STRLIT:
			TODO
			break;
		case TYPE_VARARRAY:
		{
			llvm_value_rvalue(c, be_value);
			LLVMValueRef check = LLVMBuildIsNull(c->builder, be_value->value, "checknull");
			BEValue bool_value;
			llvm_value_set_bool(&bool_value, check);
			LLVMBasicBlockRef null_block = llvm_basic_block_new(c, "lennull");
			LLVMBasicBlockRef non_null_block = llvm_basic_block_new(c, "lennormal");
			LLVMBasicBlockRef exit_block = llvm_basic_block_new(c, "lenend");
			llvm_emit_cond_br(c, &bool_value, null_block, non_null_block);
			llvm_emit_block(c, null_block);
			LLVMValueRef result_null = llvm_get_zero(c, type_usize);
			llvm_emit_br(c, exit_block);
			llvm_emit_block(c, non_null_block);
			LLVMTypeRef struct_type = be_value->type->backend_aux_type;
			LLVMValueRef len_addr = LLVMBuildStructGEP2(c->builder, struct_type, be_value->value, 0, "");
			llvm_value_set_address(be_value, len_addr, type_usize);
			LLVMValueRef result = llvm_value_rvalue_store(c, be_value);
			llvm_emit_br(c, exit_block);
			llvm_emit_block(c, exit_block);
			LLVMValueRef total = LLVMBuildPhi(c->builder, llvm_get_type(c, type_usize), "");
			LLVMValueRef logic_values[2] = { result_null, result };
			LLVMBasicBlockRef blocks[2] = { null_block, non_null_block };
			LLVMAddIncoming(total, logic_values, blocks, 2);
			llvm_value_set(be_value, total, type_usize);
			return;
		}
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
	llvm_emit_panic_on_true(c, ok, error);
}

static void llvm_emit_trap_zero(GenContext *c, Type *type, LLVMValueRef value, const char *error)
{
	if (!active_target.feature.safe_mode) return;

	LLVMValueRef zero = llvm_get_zero(c, type);
	LLVMValueRef ok = type_is_any_integer(type) ? LLVMBuildICmp(c->builder, LLVMIntEQ, value, zero, "zero") : LLVMBuildFCmp(c->builder, LLVMRealUEQ, value, zero, "zero");
	llvm_emit_panic_on_true(c, ok, error);
}


static void llvm_emit_trap_invalid_shift(GenContext *c, LLVMValueRef value, Type *type, const char *error)
{
	if (!active_target.feature.safe_mode) return;
	unsigned type_bit_size = type_size(type) * 8;
	LLVMValueRef max = llvm_const_int(c, type, type_bit_size);
	if (type_is_unsigned(type))
	{
		LLVMValueRef equal_or_greater = LLVMBuildICmp(c->builder, LLVMIntUGE, value, max, "shift_exceeds");
		llvm_emit_panic_on_true(c, equal_or_greater, error);
		return;
	}
	LLVMValueRef zero = llvm_const_int(c, type, 0);
	LLVMValueRef negative = LLVMBuildICmp(c->builder, LLVMIntSLT, value, zero, "shift_underflow");
	llvm_emit_panic_on_true(c, negative, error);
	LLVMValueRef equal_or_greater = LLVMBuildICmp(c->builder, LLVMIntSGE, value, max, "shift_exceeds");
	llvm_emit_panic_on_true(c, equal_or_greater, error);
}

static void
llvm_emit_slice_values(GenContext *c, Expr *slice, Type **parent_type_ref, LLVMValueRef *parent_base_ref,
                       Type **start_type_ref, LLVMValueRef *start_index_ref, Type **end_type_ref,
                       LLVMValueRef *end_index_ref)
{
	assert(slice->expr_kind == EXPR_SLICE);

	Expr *parent_expr = slice->slice_expr.expr;
	Type *parent_type = parent_expr->type->canonical;
	BEValue parent_addr_x;
	llvm_emit_expr(c, &parent_addr_x, parent_expr);
	llvm_value_addr(c, &parent_addr_x);
	LLVMValueRef parent_addr = parent_addr_x.value;
	LLVMValueRef parent_load_value;
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
		case TYPE_VARARRAY:
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

	LLVMValueRef len;
	if (!end || slice->slice_expr.start_from_back || slice->slice_expr.end_from_back || active_target.feature.safe_mode)
	{
		switch (parent_type->type_kind)
		{
			case TYPE_POINTER:
				len = NULL;
				break;
			case TYPE_SUBARRAY:
				len = LLVMBuildExtractValue(c->builder, parent_load_value, 1, "");
				break;
			case TYPE_ARRAY:
				len = llvm_const_int(c, type_usize, parent_type->array.len);
				break;
			case TYPE_VARARRAY:
			case TYPE_STRLIT:
				TODO
			default:
				UNREACHABLE
		}
	}

	// Walk from end if it is slice from the back.
	if (slice->slice_expr.start_from_back)
	{
		start_index.value = llvm_emit_sub_int(c, start_type, len, start_index.value);
	}

	// Check that index does not extend beyond the length.
	if (parent_type->type_kind != TYPE_POINTER && active_target.feature.safe_mode)
	{

		LLVMValueRef exceeds_size = llvm_emit_int_comparison(c,
		                                                     type_usize,
		                                                     start_type,
		                                                     start_index.value,
		                                                     len,
		                                                     BINARYOP_GE);
		llvm_emit_panic_on_true(c, exceeds_size, "Index exceeds array length.");
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
			end_index.value = llvm_emit_sub_int(c, end_type, len, end_index.value);
			llvm_value_rvalue(c, &end_index);
		}

		// This will trap any bad negative index, so we're fine.
		if (active_target.feature.safe_mode)
		{
			LLVMValueRef excess = llvm_emit_int_comparison(c,
			                                               start_type,
			                                               end_type,
			                                               start_index.value,
			                                               end_index.value,
			                                               BINARYOP_GT);
			llvm_emit_panic_on_true(c, excess, "Negative size");

			if (len)
			{
				LLVMValueRef exceeds_size = llvm_emit_int_comparison(c,
				                                                     type_usize,
				                                                     end_type,
				                                                     len,
				                                                     end_index.value,
				                                                     BINARYOP_LT);
				llvm_emit_panic_on_true(c, exceeds_size, "Size exceeds index");
			}
		}
	}
	else
	{
		assert(len && "Pointer should never end up here.");
		// Otherwise everything is fine and dandy. Our len - 1 is our end index.
		end_index.value = LLVMBuildSub(c->builder, len, LLVMConstInt(LLVMTypeOf(len), 1, false), "");
		end_type = type_usize;
	}

	*end_index_ref = end_index.value;
	*end_type_ref = end_type;
	*start_index_ref = start_index.value;
	*start_type_ref = start_type;
	*parent_base_ref = parent_base;
	*parent_type_ref = parent_type;
}

static void gencontext_emit_slice(GenContext *context, BEValue *be_value, Expr *expr)
{
	Type *parent_type;
	Type *end_type;
	LLVMValueRef end_index;
	LLVMValueRef parent_base;
	Type *start_type;
	LLVMValueRef start_index;
	// Use general function to get all the values we need (a lot!)
	llvm_emit_slice_values(context, expr, &parent_type,
	                       &parent_base,
	                       &start_type, &start_index, &end_type, &end_index);


	// Calculate the size
	LLVMValueRef size = LLVMBuildSub(context->builder, LLVMBuildAdd(context->builder, end_index, llvm_const_int(context, start_type, 1), ""), start_index, "size");
	LLVMValueRef start_pointer;
	switch (parent_type->type_kind)
	{
		case TYPE_ARRAY:
		{
			Type *pointer_type = type_get_ptr(parent_type->array.base);
			// Change pointer from Foo[x] to Foo*
			parent_base = llvm_emit_bitcast(context, parent_base, pointer_type);
			// Move pointer
			start_pointer = LLVMBuildInBoundsGEP2(context->builder, llvm_get_pointee_type(context, pointer_type), parent_base, &start_index, 1, "offset");
			break;
		}
		case TYPE_SUBARRAY:
		{
			start_pointer = LLVMBuildInBoundsGEP(context->builder, parent_base, &start_index, 1, "offsetsub");
			break;
		}
		case TYPE_POINTER:
		{
			// Move pointer
			start_pointer = LLVMBuildInBoundsGEP2(context->builder, llvm_get_pointee_type(context, parent_type), parent_base, &start_index, 1, "offset");
			break;
		}
		default:
			TODO
	}

	// Create a new subarray type
	LLVMValueRef result = LLVMGetUndef(llvm_get_type(context, expr->type));
	result = LLVMBuildInsertValue(context->builder, result, start_pointer, 0, "");
	llvm_value_set(be_value, LLVMBuildInsertValue(context->builder, result, size, 1, ""), expr->type);
}

static void gencontext_emit_slice_assign(GenContext *c, BEValue *be_value, Expr *expr)
{
	// We will be replacing the slice assign with code that roughly looks like this:
	// size_t end = slice_end;
	// size_t slice_current = slice_start;
	// while (slice_current <= end) pointer[slice_current++] = value;

	// First, find the value assigned.
	Expr *assigned_value = expr->slice_assign_expr.right;
	llvm_emit_expr(c, be_value, assigned_value);

	Type *parent_type;
	Type *end_type;
	LLVMValueRef end_index;
	LLVMValueRef parent_base;
	Type *start_type;
	LLVMValueRef start_index;
	// Use general function to get all the values we need (a lot!)
	llvm_emit_slice_values(c, expr->slice_assign_expr.left, &parent_type,
	                       &parent_base,
	                       &start_type, &start_index, &end_type, &end_index);

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
	LLVMValueRef offset = LLVMBuildPhi(c->builder, llvm_get_type(c, start_type), "");

	// Check if we're not at the end.
	LLVMValueRef not_at_end = llvm_emit_int_comparison(c, start_type, end_type, offset, end_index, BINARYOP_LE);

	// If jump to the assign block if we're not at the end index.
	BEValue value;
	EMIT_LOC(c, expr);
	llvm_value_set_bool(&value, not_at_end);
	llvm_emit_cond_br(c, &value, assign_block, exit_block);

	// Emit the assign.
	llvm_emit_block(c, assign_block);
	// Reuse this calculation
	LLVMValueRef target = llvm_emit_subscript_addr_with_base(c, parent_type, parent_base, offset);
	// And store the value.
	// TODO correct alignment.
	llvm_store_bevalue_aligned(c, target, be_value, 0);

	// Create the new offset
	LLVMValueRef next_offset = llvm_emit_add_int(c, start_type, offset, llvm_const_int(c, start_type, 1));

	// And jump back
	llvm_emit_br(c, cond_block);

	// Finally set up our phi
	LLVMValueRef logic_values[2] = { start_index, next_offset };
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
	LLVMBasicBlockRef start_block = LLVMGetInsertBlock(c->builder);
	LLVMBasicBlockRef phi_block = llvm_basic_block_new(c, op == BINARYOP_AND ? "and.phi" : "or.phi");
	LLVMBasicBlockRef rhs_block = llvm_basic_block_new(c, op == BINARYOP_AND ? "and.rhs" : "or.rhs");

	// Generate left-hand condition and conditional branch
	llvm_emit_expr(c, be_value, expr->binary_expr.left);
	llvm_value_rvalue(c, be_value);

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



LLVMValueRef llvm_emit_int_comparison(GenContext *c, Type *lhs_type, Type *rhs_type, LLVMValueRef lhs_value, LLVMValueRef rhs_value, BinaryOp binary_op)
{
	bool lhs_signed = type_is_signed(lhs_type);
	bool rhs_signed = type_is_signed(rhs_type);
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
	if (!lhs_signed)
	{
		assert(lhs_signed == rhs_signed);
		// Right and left side are both unsigned.
		switch (binary_op)
		{
			case BINARYOP_EQ:
				return LLVMBuildICmp(c->builder, LLVMIntEQ, lhs_value, rhs_value, "eq");
			case BINARYOP_NE:
				return LLVMBuildICmp(c->builder, LLVMIntNE, lhs_value, rhs_value, "neq");
			case BINARYOP_GE:
				return LLVMBuildICmp(c->builder, LLVMIntUGE, lhs_value, rhs_value, "ge");
			case BINARYOP_GT:
				return LLVMBuildICmp(c->builder, LLVMIntUGT, lhs_value, rhs_value, "gt");
			case BINARYOP_LE:
				return LLVMBuildICmp(c->builder, LLVMIntULE, lhs_value, rhs_value, "le");
			case BINARYOP_LT:
				return LLVMBuildICmp(c->builder, LLVMIntULT, lhs_value, rhs_value, "lt");
			default:
				UNREACHABLE
		}
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
	if (rhs_signed) return comp_value;

	// Otherwise, special handling for left side signed, right side unsigned.
	LLVMValueRef zero = llvm_get_zero(c, lhs_type);
	switch (binary_op)
	{
		case BINARYOP_EQ:
			// Only true if lhs >= 0
			check_value = LLVMBuildICmp(c->builder, LLVMIntSGE, lhs_value, zero, "check");
			return LLVMBuildAnd(c->builder, check_value, comp_value, "siui-eq");
		case BINARYOP_NE:
			// Always true if lhs < 0
			check_value = LLVMBuildICmp(c->builder, LLVMIntSLT, lhs_value, zero, "check");
			return LLVMBuildOr(c->builder, check_value, comp_value, "siui-ne");
		case BINARYOP_GE:
			// Only true if rhs >= 0 when regarded as a signed integer
			check_value = LLVMBuildICmp(c->builder, LLVMIntSGE, rhs_value, zero, "check");
			return LLVMBuildAnd(c->builder, check_value, comp_value, "siui-ge");
		case BINARYOP_GT:
			// Only true if rhs >= 0 when regarded as a signed integer
			check_value = LLVMBuildICmp(c->builder, LLVMIntSGE, rhs_value, zero, "check");
			return LLVMBuildAnd(c->builder, check_value, comp_value, "siui-gt");
		case BINARYOP_LE:
			// Always true if rhs < 0 when regarded as a signed integer
			check_value = LLVMBuildICmp(c->builder, LLVMIntSLT, rhs_value, zero, "check");
			return LLVMBuildOr(c->builder, check_value, comp_value, "siui-le");
		case BINARYOP_LT:
			// Always true if rhs < 0 when regarded as a signed integer
			check_value = LLVMBuildICmp(c->builder, LLVMIntSLT, rhs_value, zero, "check");
			return LLVMBuildOr(c->builder, check_value, comp_value, "siui-lt");
		default:
			UNREACHABLE
	}

}

static LLVMValueRef llvm_emit_ptr_comparison(GenContext *c, LLVMValueRef lhs_value, LLVMValueRef rhs_value, BinaryOp binary_op)
{
	switch (binary_op)
	{
		case BINARYOP_EQ:
			return LLVMBuildICmp(c->builder, LLVMIntEQ, lhs_value, rhs_value, "eq");
		case BINARYOP_NE:
			return LLVMBuildICmp(c->builder, LLVMIntNE, lhs_value, rhs_value, "neq");
		case BINARYOP_GE:
			return LLVMBuildICmp(c->builder, LLVMIntUGE, lhs_value, rhs_value, "ge");
		case BINARYOP_GT:
			return LLVMBuildICmp(c->builder, LLVMIntUGT, lhs_value, rhs_value, "gt");
		case BINARYOP_LE:
			return LLVMBuildICmp(c->builder, LLVMIntULE, lhs_value, rhs_value, "le");
		case BINARYOP_LT:
			return LLVMBuildICmp(c->builder, LLVMIntULT, lhs_value, rhs_value, "lt");
		default:
			UNREACHABLE
	}


}

static inline LLVMValueRef llvm_fixup_shift_rhs(GenContext *c, LLVMValueRef left, LLVMValueRef right)
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

static inline LLVMValueRef llvm_emit_mult_int(GenContext *c, Type *type, LLVMValueRef left, LLVMValueRef right)
{
	if (active_target.feature.trap_on_wrap)
	{
		LLVMTypeRef type_to_use = llvm_get_type(c, type);
		LLVMValueRef args[2] = { left, right };
		LLVMTypeRef types[2] = { type_to_use, type_to_use };
		unsigned operation = type_is_integer_unsigned(type) ? intrinsic_id_umul_overflow
		                                                    : intrinsic_id_smul_overflow;
		LLVMValueRef call_res = llvm_emit_call_intrinsic(c,
		                                                 operation,
		                                                 types,
		                                                 1,
		                                                 args,
		                                                 2);
		LLVMValueRef val = LLVMBuildExtractValue(c->builder, call_res, 0, "mul");
		LLVMValueRef ok = LLVMBuildExtractValue(c->builder, call_res, 1, "");
		llvm_emit_panic_on_true(c, ok, "Integer multiplication overflow");
		return val;
	}
	return LLVMBuildMul(c->builder, left, right, "mul");
}

static void gencontext_emit_binary(GenContext *c, BEValue *be_value, Expr *expr, BEValue *lhs_addr, BinaryOp binary_op)
{

	if (binary_op == BINARYOP_AND || binary_op == BINARYOP_OR)
	{
		gencontext_emit_logical_and_or(c, be_value, expr, binary_op);
		return;
	}
	BEValue lhs;
	if (lhs_addr)
	{
		lhs = *lhs_addr;
	}
	else
	{
		llvm_emit_expr(c, &lhs, expr->binary_expr.left);
	}
	llvm_value_rvalue(c, &lhs);

	BEValue rhs;
	llvm_emit_expr(c, &rhs, expr->binary_expr.right);
	llvm_value_rvalue(c, &rhs);

	Type *lhs_type = type_lowering(lhs.type);
	Type *rhs_type = type_lowering(rhs.type);
	LLVMValueRef lhs_value = lhs.value;
	LLVMValueRef rhs_value = rhs.value;
	EMIT_LOC(c, expr);
	if (type_is_integer(lhs_type) && binary_op >= BINARYOP_GT && binary_op <= BINARYOP_EQ)
	{
		llvm_value_set_bool(be_value, llvm_emit_int_comparison(c, lhs_type, rhs_type, lhs_value, rhs_value, binary_op));
		return;
	}
	if (type_is_pointer(lhs_type) && binary_op >= BINARYOP_GT && binary_op <= BINARYOP_EQ)
	{
		llvm_value_set_bool(be_value, llvm_emit_ptr_comparison(c, lhs_value, rhs_value, binary_op));
		return;
	}
	bool is_float = type_is_float(lhs_type);
	LLVMValueRef val;
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
			val = llvm_emit_mult_int(c, lhs_type, lhs_value, rhs_value);
			break;
		case BINARYOP_SUB:
			if (lhs_type->type_kind == TYPE_POINTER)
			{
				if (lhs_type == rhs_type)
				{
					LLVMTypeRef int_type = llvm_get_type(c, type_iptrdiff);
					val = LLVMBuildSub(c->builder, LLVMBuildPtrToInt(c->builder, lhs_value, int_type, ""),
					                   LLVMBuildPtrToInt(c->builder, rhs_value, int_type, ""), "");
					val = LLVMBuildExactSDiv(c->builder, val, llvm_const_int(c, type_iptrdiff, type_abi_alignment(lhs_type->pointer)), "");
					break;
				}
				rhs_value = LLVMBuildNeg(c->builder, rhs_value, "");
				val = LLVMBuildGEP2(c->builder, llvm_get_pointee_type(c, lhs_type), lhs_value, &rhs_value, 1, "ptrsub");
				break;
			}
			if (is_float)
			{
				val = LLVMBuildFSub(c->builder, lhs_value, rhs_value, "fsub");
				break;
			}
			val = llvm_emit_sub_int(c, lhs_type, lhs_value, rhs_value);
			break;
		case BINARYOP_ADD:
			if (lhs_type->type_kind == TYPE_POINTER)
			{
				assert(type_is_integer(rhs_type));
				val = LLVMBuildGEP2(c->builder, llvm_get_pointee_type(c, lhs_type), lhs_value, &rhs_value, 1, "ptradd");
				break;
			}
			if (is_float)
			{
				val = LLVMBuildFAdd(c->builder, lhs_value, rhs_value, "fadd");
				break;
			}
			val = llvm_emit_add_int(c, lhs_type, lhs_value, rhs_value);
			break;
		case BINARYOP_DIV:
			llvm_emit_trap_zero(c, rhs_type, rhs_value, "% by zero");
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
			llvm_emit_trap_zero(c, rhs_type, rhs_value, "% by zero");
			val = type_is_unsigned(lhs_type)
			      ? LLVMBuildURem(c->builder, lhs_value, rhs_value, "umod")
			      : LLVMBuildSRem(c->builder, lhs_value, rhs_value, "smod");
			break;
		case BINARYOP_SHR:
			rhs_value = llvm_fixup_shift_rhs(c, lhs_value, rhs_value);
			llvm_emit_trap_invalid_shift(c, rhs_value, lhs_type, "Shift amount out of range.");
			val = type_is_unsigned(lhs_type)
			      ? LLVMBuildLShr(c->builder, lhs_value, rhs_value, "lshr")
			      : LLVMBuildAShr(c->builder, lhs_value, rhs_value, "ashr");
			val = LLVMBuildFreeze(c->builder, val, "");
			break;
		case BINARYOP_SHL:
			rhs_value = llvm_fixup_shift_rhs(c, lhs_value, rhs_value);
			llvm_emit_trap_invalid_shift(c, rhs_value, lhs_type, "Shift amount out of range.");
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
			// Unordered?
			assert(type_is_float(lhs_type));
			llvm_value_set_bool(be_value, LLVMBuildFCmp(c->builder, LLVMRealOEQ, lhs_value, rhs_value, "eq"));
			return;
		case BINARYOP_NE:
			// Unordered?
			assert(type_is_float(lhs_type));
			llvm_value_set_bool(be_value, LLVMBuildFCmp(c->builder, LLVMRealONE, lhs_value, rhs_value, "neq"));
			return;
		case BINARYOP_GE:
			assert(type_is_float(lhs_type));
			llvm_value_set_bool(be_value, LLVMBuildFCmp(c->builder, LLVMRealOGE, lhs_value, rhs_value, "ge"));
			return;
		case BINARYOP_GT:
			assert(type_is_float(lhs_type));
			llvm_value_set_bool(be_value, LLVMBuildFCmp(c->builder, LLVMRealOGT, lhs_value, rhs_value, "gt"));
			return;
		case BINARYOP_LE:
			assert(type_is_float(lhs_type));
			llvm_value_set_bool(be_value, LLVMBuildFCmp(c->builder, LLVMRealOLE, lhs_value, rhs_value, "le"));
			return;
		case BINARYOP_LT:
			assert(type_is_float(lhs_type));
			llvm_value_set_bool(be_value, LLVMBuildFCmp(c->builder, LLVMRealOLT, lhs_value, rhs_value, "lt"));
			return;
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
	llvm_value_set(be_value, val, expr->type);
}


static void llvm_emit_post_unary_expr(GenContext *context, BEValue *be_value, Expr *expr)
{

	llvm_emit_post_inc_dec(context,
	                       be_value,
	                       expr->post_expr.expr,
	                       expr->post_expr.operator == POSTUNARYOP_INC ? 1 : -1,
	                       false);
}

static void gencontext_emit_typeid(GenContext *context, BEValue *be_value, Expr *expr)
{
	LLVMValueRef value;
	if (type_is_builtin(expr->typeid_expr->type->type_kind))
	{
		value = llvm_const_int(context, type_usize, expr->typeid_expr->type->type_kind);
	}
	else
	{
		assert(expr->typeid_expr->type->backend_typeid);
		value = expr->typeid_expr->type->backend_typeid;
	}
	llvm_value_set(be_value, value, expr->type);
}

void gencontext_emit_trycatch_expr(GenContext *c, BEValue *value, Expr *expr)
{

	LLVMBasicBlockRef error_block = llvm_basic_block_new(c, "error_block");
	LLVMBasicBlockRef no_err_block = llvm_basic_block_new(c, "noerr_block");
	LLVMBasicBlockRef phi_block = llvm_basic_block_new(c, "phi_block");

	// Store catch/error var
	PUSH_ERROR();

	// Set the catch/error var
	c->error_var = NULL;
	c->catch_block = error_block;

	llvm_emit_expr(c, value, expr->trycatch_expr);

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

	LLVMValueRef phi = LLVMBuildPhi(c->builder, llvm_get_type(c, expr->type), "val");
	LLVMValueRef lhs = llvm_const_int(c, type_bool, expr->expr_kind == EXPR_TRY ? 1 : 0);
	LLVMValueRef rhs = llvm_const_int(c, type_bool, expr->expr_kind == EXPR_TRY ? 0 : 1);

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


	llvm_emit_expr(c, be_value, expr->else_expr.expr);
	llvm_value_rvalue(c, be_value);

	// Restore.
	POP_ERROR();

	// Emit success and to end.
	llvm_emit_br(c, no_err_block);

	// Emit else
	llvm_emit_block(c, else_block);
	llvm_emit_stmt(c, expr->else_expr.else_stmt);
	llvm_emit_br(c, no_err_block);

	llvm_emit_block(c, no_err_block);
}


static void gencontext_emit_else_expr(GenContext *c, BEValue *be_value, Expr *expr)
{
	if (expr->else_expr.is_jump)
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
	llvm_emit_expr(c, &normal_value, expr->else_expr.expr);
	llvm_value_rvalue(c, &normal_value);

	// Restore.
	POP_ERROR();

	// Emit success and jump to phi.
	LLVMBasicBlockRef success_end_block = llvm_get_current_block_if_in_use(c);

	if (success_end_block) llvm_emit_br(c, phi_block);

	// Emit else
	llvm_emit_block(c, else_block);

	BEValue else_value;
	llvm_emit_expr(c, &else_value, expr->else_expr.else_expr);
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
 * This is the foo!! instruction.
 */
static inline void gencontext_emit_guard_expr(GenContext *c, BEValue *be_value, Expr *expr)
{
	LLVMBasicBlockRef guard_block = llvm_basic_block_new(c, "guard_block");
	LLVMBasicBlockRef no_err_block = llvm_basic_block_new(c, "noerr_block");

	// Store catch/error var
	PUSH_ERROR();

	// Set the catch/error var
	LLVMValueRef error_var = llvm_emit_alloca_aligned(c, type_error, "error_var");

	c->error_var = error_var;
	c->catch_block = guard_block;

	llvm_emit_expr(c, be_value, expr->guard_expr.inner);
	printf("TODO: Passed rvalue on guard, consider semantics.");
	llvm_value_rvalue(c, be_value);

	// Restore.
	POP_ERROR();

	// Emit success and to end.
	llvm_emit_br(c, no_err_block);

	POP_ERROR();

	// Emit else
	llvm_emit_block(c, guard_block);

	// Ensure we are on a branch that is non empty.
	if (llvm_emit_check_block_branch(c))
	{
		llvm_emit_defer(c, expr->guard_expr.defer, 0);
		BEValue value;
		llvm_value_set_address(&value, error_var, type_error);
		llvm_emit_return_abi(c, NULL, &value);
		c->current_block = NULL;
		c->current_block_is_target = NULL;
	}

	llvm_emit_block(c, no_err_block);

}

static void gencontext_emit_binary_expr(GenContext *context, BEValue *be_value, Expr *expr)
{
	BinaryOp binary_op = expr->binary_expr.operator;
	if (binary_op > BINARYOP_ASSIGN)
	{
		BinaryOp base_op = binaryop_assign_base_op(binary_op);
		assert(base_op != BINARYOP_ERROR);
		BEValue addr;
		llvm_emit_expr(context, &addr, expr->binary_expr.left);
		llvm_value_addr(context, &addr);
		gencontext_emit_binary(context, be_value, expr, &addr, base_op);
		llvm_store_bevalue(context, &addr, be_value);
		return;
	}
	if (binary_op == BINARYOP_ASSIGN)
	{
		llvm_emit_expr(context, be_value, expr->binary_expr.left);
		assert(llvm_value_is_addr(be_value));
		LLVMValueRef failable_ref = NULL;
		if (expr->binary_expr.left->expr_kind == EXPR_IDENTIFIER)
		{
			failable_ref = decl_failable_ref(expr->binary_expr.left->identifier_expr.decl);
		}
		*be_value = llvm_emit_assign_expr(context, be_value, expr->binary_expr.right, failable_ref);
		return;
	}

	gencontext_emit_binary(context, be_value, expr, NULL, binary_op);
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
	llvm_value_rvalue(c, &lhs);

	LLVMBasicBlockRef lhs_exit = llvm_get_current_block_if_in_use(c);
	if (lhs_exit) llvm_emit_br(c, phi_block);

	llvm_emit_block(c, rhs_block);
	BEValue rhs;
	llvm_emit_expr(c, &rhs, expr->ternary_expr.else_expr);
	llvm_value_rvalue(c, &rhs);

	LLVMBasicBlockRef rhs_exit = llvm_get_current_block_if_in_use(c);
	if (rhs_exit) llvm_emit_br(c, phi_block);

	// Generate phi
	llvm_emit_block(c, phi_block);
	if (!rhs_exit)
	{
		*value = lhs;
	}
	if (!lhs_exit)
	{
		*value = rhs;
	}
	LLVMValueRef phi = LLVMBuildPhi(c->builder, llvm_get_type(c, expr->type), "val");
	LLVMValueRef logic_values[2] = { lhs.value, rhs.value };
	LLVMBasicBlockRef blocks[2] = { lhs_exit, rhs_exit };
	LLVMAddIncoming(phi, logic_values, blocks, 2);

	llvm_value_set(value, phi, expr->type);
}


static void llvm_emit_const_expr(GenContext *c, BEValue *be_value, Expr *expr)
{
	Type *type = type_reduced_from_expr(expr)->canonical;
	switch (expr->const_expr.kind)
	{
		case ALL_INTS:
			if (type_is_unsigned(type))
			{
				llvm_value_set(be_value, llvm_const_int(c, type, bigint_as_unsigned(&expr->const_expr.i)), type);
			}
			else
			{
				llvm_value_set(be_value, llvm_const_int(c, type, bigint_as_signed(&expr->const_expr.i)), type);
			}
			return;
		case TYPE_COMPLEX:
		{
			LLVMTypeRef element_type = llvm_get_type(c, type->complex);
			LLVMValueRef value = LLVMGetUndef(llvm_get_type(c, type));
			unsigned id = 0;
			value = LLVMConstInsertValue(value, LLVMConstReal(element_type, (double)expr->const_expr.complex.r), &id, 1);
			id++;
			value = LLVMConstInsertValue(value, LLVMConstReal(element_type, (double)expr->const_expr.complex.i), &id, 1);
			llvm_value_set(be_value, value, type);
			return;
		}
		case ALL_FLOATS:
			llvm_value_set(be_value, LLVMConstReal(llvm_get_type(c, type), (double) expr->const_expr.f), type);
			return;
		case TYPE_POINTER:
			llvm_value_set(be_value, LLVMConstNull(llvm_get_type(c, type)), type);
			return;
		case TYPE_BOOL:
			llvm_value_set_bool(be_value, LLVMConstInt(c->bool_type, expr->const_expr.b ? 1 : 0, 0));
			return;
		case TYPE_STRLIT:
		{
			LLVMValueRef global_name = LLVMAddGlobal(c->module, LLVMArrayType(llvm_get_type(c, type_char), expr->const_expr.string.len + 1), ".str");
			LLVMSetLinkage(global_name, LLVMPrivateLinkage);
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
		case TYPE_ERRTYPE:
			TODO
		default:
			UNREACHABLE
	}
}

static void llvm_expand_type_to_args(GenContext *context, Type *param_type, LLVMValueRef expand_ptr, LLVMValueRef **values);

static void gencontext_expand_array_to_args(GenContext *c, Type *param_type, LLVMValueRef expand_ptr, LLVMValueRef **values)
{
	LLVMTypeRef element_type = llvm_get_type(c, param_type->array.base);
	LLVMValueRef zero = llvm_get_zero(c, type_int);
	LLVMValueRef indices[2] = { zero, zero, };
	for (ByteSize i = 0; i < param_type->array.len; i++)
	{
		indices[1] = llvm_const_int(c, type_int, i);
		LLVMValueRef element_ptr = LLVMBuildGEP2(c->builder, element_type, expand_ptr, indices, 2, "");
		llvm_expand_type_to_args(c, param_type->array.base, element_ptr, values);
	}
}

static void gencontext_expand_struct_to_args(GenContext *context, Type *param_type, LLVMValueRef expand_ptr, LLVMValueRef **values)
{
	Decl **members = param_type->decl->strukt.members;
	VECEACH(members, i)
	{
		Type *member_type = members[i]->type;
		if (type_is_empty_field(member_type, true)) continue;
		LLVMValueRef member_ptr = LLVMBuildStructGEP(context->builder, expand_ptr, i, "expandmember");
		llvm_expand_type_to_args(context, member_type, member_ptr, values);
	}
}

static void llvm_expand_type_to_args(GenContext *context, Type *param_type, LLVMValueRef expand_ptr, LLVMValueRef **values)
{
	REDO:
	switch (type_flatten(param_type)->type_kind)
	{
		case TYPE_POISONED:
		case TYPE_VOID:
		case TYPE_IXX:
		case TYPE_FXX:
		case TYPE_TYPEID:
		case TYPE_FUNC:
		case TYPE_TYPEINFO:
		case TYPE_MEMBER:
		case TYPE_DISTINCT:
		case TYPE_STRLIT:
		case TYPE_INFERRED_ARRAY:
			UNREACHABLE
			break;
		case TYPE_BOOL:
		case ALL_SIGNED_INTS:
		case ALL_UNSIGNED_INTS:
		case ALL_REAL_FLOATS:
		case TYPE_POINTER:
		case TYPE_ENUM:
		case TYPE_ERRTYPE:
		case TYPE_VARARRAY:
			vec_add(*values, LLVMBuildLoad2(context->builder, llvm_get_type(context, param_type), expand_ptr, "loadexpanded"));
			return;
		case TYPE_TYPEDEF:
			param_type = param_type->canonical;
			goto REDO;
		case TYPE_ERR_UNION:
			TODO
		case TYPE_STRUCT:
			gencontext_expand_struct_to_args(context, param_type, expand_ptr, values);
			break;
		case TYPE_ARRAY:
			gencontext_expand_array_to_args(context, param_type, expand_ptr, values);
			break;
		case TYPE_UNION:
		case TYPE_COMPLEX:
		case TYPE_SUBARRAY:
		case TYPE_VECTOR:
		case TYPE_VIRTUAL_ANY:
		case TYPE_VIRTUAL:
			TODO
			break;
	}
}


LLVMValueRef llvm_emit_struct_gep_raw(GenContext *context, LLVMValueRef ptr, LLVMTypeRef struct_type, unsigned index, unsigned struct_alignment, unsigned offset, unsigned *alignment)
{
	*alignment = type_min_alignment(offset, struct_alignment);
	LLVMValueRef addr = LLVMBuildStructGEP2(context->builder, struct_type, ptr, index, "");
	return addr;
}

void llvm_emit_subarray_len(GenContext *c, BEValue *subarray, BEValue *len)
{
	llvm_value_addr(c, subarray);
	unsigned alignment = 0;
	LLVMValueRef len_addr = llvm_emit_struct_gep_raw(c, subarray->value, llvm_get_type(c, subarray->type), 1, subarray->alignment,
	                                                 type_abi_alignment(type_voidptr), &alignment);
	llvm_value_set_address_align(len, len_addr, type_usize, alignment);
}

void llvm_emit_subarray_pointer(GenContext *c, BEValue *subarray, BEValue *pointer)
{
	llvm_value_addr(c, subarray);
	unsigned alignment = 0;
	LLVMValueRef len_addr = llvm_emit_struct_gep_raw(c, subarray->value, llvm_get_type(c, subarray->type), 0, subarray->alignment,
	                                                 0, &alignment);
	llvm_value_set_address_align(pointer, len_addr, type_get_ptr(subarray->type->array.base), alignment);

}

void llvm_value_struct_gep(GenContext *c, BEValue *element, BEValue *struct_pointer, unsigned index)
{
	llvm_value_fold_failable(c, struct_pointer);
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
	unsigned alignment;
	LLVMValueRef ref = llvm_emit_struct_gep_raw(c,
	                                            struct_pointer->value,
	                                            llvm_get_type(c, struct_pointer->type),
	                                            actual_index,
	                                            struct_pointer->alignment,
	                                            member->offset,
	                                            &alignment);
	llvm_value_set_address(element, ref, member->type);
	element->alignment = alignment;
}

static void llvm_emit_fp_intrinsic_expr(GenContext *c, unsigned intrinsic_id, BEValue *be_value, Expr *expr)
{
	unsigned arguments = vec_size(expr->call_expr.arguments);
	llvm_emit_expr(c, be_value, expr->call_expr.arguments[0]);
	llvm_value_rvalue(c, be_value);
	LLVMTypeRef call_type = llvm_get_type(c, be_value->type);
	LLVMValueRef result = llvm_emit_call_intrinsic(c, intrinsic_id, &call_type, 1, &be_value->value, 1);
	be_value->value = result;
}
void gencontext_emit_call_intrinsic_expr(GenContext *c, BEValue *be_value, Expr *expr)
{
	Decl *function_decl = expr->call_expr.function->identifier_expr.decl;
	function_decl = decl_flatten(function_decl);
	if (function_decl->name == kw___round)
	{
		llvm_emit_fp_intrinsic_expr(c, intrinsic_id_rint, be_value, expr);
		return;
	}
	if (function_decl->name == kw___sqrt)
	{
		llvm_emit_fp_intrinsic_expr(c, intrinsic_id_sqrt, be_value, expr);
		return;
	}
	if (function_decl->name == kw___trunc)
	{
		llvm_emit_fp_intrinsic_expr(c, intrinsic_id_trunc, be_value, expr);
		return;
	}
	if (function_decl->name == kw___ceil)
	{
		llvm_emit_fp_intrinsic_expr(c, intrinsic_id_ceil, be_value, expr);
		return;
	}
	UNREACHABLE
}

void llvm_emit_parameter(GenContext *c, LLVMValueRef **args, ABIArgInfo *info, BEValue *be_value, Type *type)
{
	switch (info->kind)
	{
		case ABI_ARG_IGNORE:
			// Skip.
			return;
		case ABI_ARG_INDIRECT:
		{
			// If we want we could optimize for structs by doing it by reference here.
			unsigned alignment = info->indirect.realignment ?: type_abi_alignment(type);
			LLVMValueRef indirect = llvm_emit_alloca(c,
			                                         llvm_get_type(c, type),
			                                         alignment,
			                                         "indirectarg");
			llvm_store_bevalue_aligned(c, indirect, be_value, alignment);
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
			LLVMValueRef cast;
			AlignSize target_alignment = llvm_abi_alignment(c, coerce_type);
			AlignSize max_align = MAX((be_value->alignment), llvm_abi_alignment(c, coerce_type));

			// If we are loading something with greater alignment than what we have, we cannot directly memcpy.
			if (llvm_value_is_addr(be_value) && be_value->alignment < target_alignment)
			{
				// So load it instead.
				llvm_value_rvalue(c, be_value);
			}

			// In this case we have something nicely aligned, so we just do a cast.
			if (llvm_value_is_addr(be_value))
			{
				cast = LLVMBuildBitCast(c->builder, be_value->value, LLVMPointerType(coerce_type, 0), "");
			}
			else
			{
				cast = llvm_emit_alloca(c, coerce_type, max_align, "coerce");
				LLVMValueRef target = LLVMBuildBitCast(c->builder, cast, llvm_get_ptr_type(c, type), "");
				llvm_store_bevalue_aligned(c, target, be_value, max_align);
			}
			LLVMTypeRef element = llvm_abi_type(c, info->direct_coerce.type);
			for (unsigned idx = 0; idx < info->direct_coerce.elements; idx++)
			{
				LLVMValueRef element_ptr = LLVMBuildStructGEP2(c->builder, coerce_type, cast, idx, "");
				vec_add(*args,
				        llvm_emit_load_aligned(c, element, element_ptr, llvm_abi_alignment(c, element), ""));
			}
			return;
		}
		case ABI_ARG_DIRECT_PAIR:
		{
			llvm_value_addr(c, be_value);
			printf("Handle invalid alignment");
			// Here we do the following transform:
			// struct -> { lo, hi } -> lo, hi
			LLVMTypeRef lo = llvm_abi_type(c, info->direct_pair.lo);
			LLVMTypeRef hi = llvm_abi_type(c, info->direct_pair.hi);
			LLVMTypeRef struct_type = llvm_get_coerce_type(c, info);
            LLVMValueRef cast = LLVMBuildBitCast(c->builder, be_value->value, LLVMPointerType(struct_type, 0), "casttemp");
			// Get the lo value.
			LLVMValueRef lo_ptr = LLVMBuildStructGEP2(c->builder, struct_type, cast, 0, "lo");
			vec_add(*args, llvm_emit_load_aligned(c, lo, lo_ptr, llvm_abi_alignment(c, lo), "lo"));
			// Get the hi value.
			LLVMValueRef hi_ptr = LLVMBuildStructGEP2(c->builder, struct_type, cast, 1, "hi");
			vec_add(*args, llvm_emit_load_aligned(c, hi, hi_ptr, llvm_abi_alignment(c, hi), "hi"));
			return;
		}
		case ABI_ARG_EXPAND_COERCE:
		{
			// Move this to an address (if needed)
			llvm_value_addr(c, be_value);
			LLVMTypeRef coerce_type = llvm_get_coerce_type(c, info);
			LLVMValueRef temp = LLVMBuildBitCast(c->builder, be_value->value, LLVMPointerType(coerce_type, 0), "coerce");
			LLVMValueRef gep_first = LLVMBuildStructGEP2(c->builder, coerce_type, temp, info->coerce_expand.lo_index, "first");
			vec_add(*args, LLVMBuildLoad2(c->builder, llvm_abi_type(c, info->coerce_expand.lo), gep_first, ""));
			if (info->coerce_expand.hi)
			{
				LLVMValueRef gep_second = LLVMBuildStructGEP2(c->builder, coerce_type, temp, info->coerce_expand.hi_index, "second");
				vec_add(*args, LLVMBuildLoad2(c->builder, llvm_abi_type(c, info->coerce_expand.hi), gep_second, ""));
			}
			return;
		}
		case ABI_ARG_EXPAND:
		{
			// Move this to an address (if needed)
			llvm_value_addr(c, be_value);
			llvm_expand_type_to_args(c, type, be_value->value, args);
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
		case TYPE_VARARRAY:
			TODO
		default:
			UNREACHABLE
	}
}
void llvm_emit_call_expr(GenContext *c, BEValue *be_value, Expr *expr)
{
	Expr *function = expr->call_expr.function;
	FunctionSignature *signature;
	LLVMTypeRef func_type;
	LLVMValueRef func;


	// 1. Call through a pointer.
	if (expr->call_expr.is_pointer_call)
	{
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
	else if (expr->call_expr.is_type_method)
	{
		// 2. Call through a type method

		// 2a. Get the function declaration
		Decl *function_decl = expr->call_expr.function->access_expr.ref;

		// 2b. Set signature, function and function type
		signature = &function_decl->func.function_signature;
		func = function_decl->backend_ref;
		func_type = llvm_get_type(c, function_decl->type);
	}
	else
	{
		// 3. Call a regular function.
		Decl *function_decl = expr->call_expr.function->identifier_expr.decl;
		function_decl = decl_flatten(function_decl);

		// 3a. This may be an intrinsic, if so generate an intrinsic call instead.
		if (function_decl->func.is_builtin)
		{
			gencontext_emit_call_intrinsic_expr(c, be_value, expr);
			return;
		}

		// 3b. Set signature, function and function type
		signature = &function_decl->func.function_signature;
		func = function_decl->backend_ref;
		func_type = llvm_get_type(c, function_decl->type);
	}


	LLVMValueRef *values = NULL;

	// 4. Prepare the return abi data.
	ABIArgInfo *ret_info = signature->ret_abi_info;
	Type *return_type = signature->rtype->type->canonical;

	// 5. In the case of a failable, the error is replacing the regular return abi.
	LLVMValueRef error_var = NULL;
	if (signature->failable)
	{
		ret_info = signature->failable_abi_info;
		return_type = type_error;
	}

	// 6. Generate data for the return value.
	switch (ret_info->kind)
	{
		case ABI_ARG_INDIRECT:
			// 6a. We can use the stored error var if there is no redirect.
			if (signature->failable && c->error_var && type_abi_alignment(return_type) < ret_info->indirect.realignment)
			{
				error_var = c->error_var;
				vec_add(values, error_var);
				break;
			}
			// 6b. Return true is indirect, in this case we allocate a local, using the desired alignment on the caller side.
			AlignSize alignment = ret_info->indirect.realignment ? ret_info->indirect.realignment : type_abi_alignment(return_type);
			llvm_value_set_address_align(be_value, llvm_emit_alloca(c, llvm_get_type(c, return_type), alignment, "sretparam"), return_type, alignment);

			// 6c. Add the pointer to the list of arguments.
			vec_add(values, be_value->value);
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
	BEValue synthetic_return_param = {};
	if (signature->failable && signature->ret_abi_info)
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
	if (signature->typed_variadic) non_variadic_params--;
	assert(arguments >= non_variadic_params);
	for (unsigned i = 0; i < non_variadic_params; i++)
	{
		// 8a. Evaluate the expression.
		Expr *arg_expr = expr->call_expr.arguments[i];
		llvm_emit_expr(c, be_value, arg_expr);

		// 8b. Emit the parameter according to ABI rules.
		Decl *param = signature->params[i];
		ABIArgInfo *info = param->var.abi_info;
		llvm_emit_parameter(c, &values, info, be_value, param->type);
	}

	// 9. Typed varargs
	if (signature->typed_variadic)
	{
		printf("All varargs should be called with non-alias!\n");
		Decl *vararg_param = signature->params[non_variadic_params];

		BEValue subarray;

		llvm_value_set_address(&subarray, llvm_emit_alloca_aligned(c, vararg_param->type, "vararg"), vararg_param->type);

		// 9a. Special case, empty argument
		if (arguments == non_variadic_params + 1 && !expr->call_expr.arguments[non_variadic_params])
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
			LLVMValueRef array_ref = llvm_emit_alloca_aligned(c, array, "varargslots");
			LLVMValueRef zero = llvm_get_zero(c, type_usize);
			LLVMValueRef indices[2] = {
					zero,
					zero,
			};
			for (unsigned i = non_variadic_params; i < arguments; i++)
			{
				Expr *arg_expr = expr->call_expr.arguments[i];
				llvm_emit_expr(c, be_value, arg_expr);
				indices[1] = llvm_const_int(c, type_usize, i - non_variadic_params);
				LLVMValueRef slot = LLVMBuildInBoundsGEP2(c->builder, llvm_array_type, array_ref, indices, 2, "");
				llvm_store_bevalue_aligned(c, slot, be_value, 0);
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
			llvm_emit_expr(c, be_value, arg_expr);
			printf("TODO: varargs should be expanded correctly\n");
			vec_add(values, llvm_value_rvalue_store(c, be_value));
		}
	}


	// 10. Create the actual call (remember to emit a loc, because we might have shifted loc emitting the params)
	EMIT_LOC(c, expr);
	LLVMValueRef call_value = LLVMBuildCall2(c->builder, func_type, func, values, vec_size(values), "");

	// 11. Process the return value.
	switch (ret_info->kind)
	{
		case ABI_ARG_EXPAND:
			UNREACHABLE
		case ABI_ARG_IGNORE:
			// 12. Basically void returns or empty structs.
			//     Here we know we don't have a failable or any return value that can be used.
			assert(!signature->failable && "Failable should have produced a return value.");
			*be_value = (BEValue) {};
			return;
		case ABI_ARG_INDIRECT:

			// 13. Indirect, that is passing the result through an out parameter.

			// 13a. In the case of an already present error_var, we don't need to do a load here.
			if (error_var) break;

			// 13b. Otherwise it will be contained in a be_value that is an address
			//      so we don't need to do anything more.
			assert(be_value->kind == BE_ADDRESS);

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
			llvm_emit_convert_value_from_coerced(c, be_value, struct_type, call_value, return_type);
			break;
		}
		case ABI_ARG_EXPAND_COERCE:
		{
			// 15. Expand-coerce, this is similar to "direct pair", but looks like this:
			//     { lo, hi } set into { pad, lo, pad, hi } -> original type.

			// 15a. Create memory to hold the return type.
			LLVMValueRef ret = llvm_emit_alloca_aligned(c, return_type, "");
			llvm_value_set_address(be_value, ret, return_type);

			// 15b. "Convert" this return type pointer in memory to our coerce type which is { pad, lo, pad, hi }
			LLVMTypeRef coerce_type = llvm_get_coerce_type(c, ret_info);
			LLVMValueRef coerce = LLVMBuildBitCast(c->builder, ret, coerce_type, "");

			// 15c. Find the type of the "lo" element.
			LLVMTypeRef lo_type = llvm_abi_type(c, ret_info->coerce_expand.lo);

			// 15d. Find the address to the low value
			unsigned alignment;
			LLVMValueRef lo = llvm_emit_struct_gep_raw(c, coerce, coerce_type, ret_info->coerce_expand.lo_index,
			                                           type_abi_alignment(return_type),
			                                           ret_info->coerce_expand.offset_lo, &alignment);

			// 15e. If there is only a single field, we simply store the value,
			//      so { lo } set into { pad, lo, pad } -> original type.
			if (!ret_info->coerce_expand.hi)
			{
				// Here we do a store to call -> lo (leaving the rest undefined)
				llvm_store_aligned(c, lo, call_value, alignment);

				break;
			}

			// 15f. Calculate the hi type.
			LLVMTypeRef hi_type = llvm_abi_type(c, ret_info->coerce_expand.hi);

			// 15g. We can now extract { lo, hi } to lo_value and hi_value.
			LLVMValueRef lo_value = LLVMBuildExtractValue(c->builder, call_value, 0, "");
			LLVMValueRef hi_value = LLVMBuildExtractValue(c->builder, call_value, 1, "");

			// 15h. Store lo_value into the { pad, lo, pad, hi } struct.
			llvm_store_aligned(c, lo, lo_value, alignment);

			// 15i. Calculate the address to the high value (like for the low in 15d.
			LLVMValueRef hi = llvm_emit_struct_gep_raw(c, coerce, coerce_type, ret_info->coerce_expand.hi_index,
			                                           type_abi_alignment(return_type),
			                                           ret_info->coerce_expand.offset_hi, &alignment);

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
				llvm_value_set(be_value, call_value, return_type);
				break;
			}
			// 16c. We use a normal bitcast coerce.
			assert(!abi_info_should_flatten(ret_info) && "Did not expect flattening on return types.");
			llvm_emit_convert_value_from_coerced(c, be_value, coerce, call_value, return_type);
			break;
		}
	}

	// 17. Handle failables.
	if (signature->failable)
	{
		BEValue no_err;

		// 17a. If we used the error var as the indirect recipient, then that will hold the error.
		//      otherwise it's whatever value in be_value.
		BEValue error_holder = *be_value;
		if (error_var)
		{
			llvm_value_set_address(&error_holder, c->error_var, type_error);
		}
		// 17b. Generate a boolean switch.
		llvm_value_set_bool(&no_err, llvm_emit_is_no_error_value(c, &error_holder));

		// 17c. If we have an error var, or we aren't interested in the error variable
		//      - then it's straightforward. We just jump to the catch block.
		LLVMBasicBlockRef after_block = llvm_basic_block_new(c, "after_check");
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
			llvm_store_bevalue_aligned(c, c->error_var, be_value, 0);
			// 17e. Then jump to the catch.
			llvm_emit_br(c, c->catch_block);
		}

		// 17f. Emit the "after" block.
		llvm_emit_block(c, after_block);

		// 17g. If void, be_value contents should be skipped.
		if (!signature->ret_abi_info)
		{
			*be_value = (BEValue) {};
			return;
		}

		// 17h. Assign the return param to be_value.
		*be_value = synthetic_return_param;
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

	if (type != type_void)
	{
		return_out = llvm_emit_alloca_aligned(context, type, "blockret");
	}
	context->block_error_var = error_out;
	context->block_failable_exit = error_block;
	context->return_out = return_out;
	context->error_var = NULL;
	context->catch_block = NULL;

	VECEACH(stmts, i)
	{
		llvm_emit_stmt(context, stmts[i]);
	}
	llvm_emit_br(context, expr_block);

	// Emit the exit block.
	llvm_emit_block(context, expr_block);

	context->return_out = old_ret_out;
	context->catch_block = error_block;
	context->error_var = error_out;
	context->block_return_exit = saved_block_return_exit;
	context->block_failable_exit = saved_block_failable_exit;
	context->block_error_var = saved_block_error;
	context->in_block--;

	if (return_out)
	{
		llvm_value_set_address(be_value, return_out, type);
	}
	else
	{
		llvm_value_set(be_value, NULL, type_void);
	}
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
			case VARDECL_ALIAS:
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

LLVMValueRef llvm_emit_call_intrinsic(GenContext *context, unsigned intrinsic_id, LLVMTypeRef *types, unsigned type_count,
                                      LLVMValueRef *values, unsigned arg_count)
{
	LLVMValueRef decl = LLVMGetIntrinsicDeclaration(context->module, intrinsic_id, types, type_count);
	LLVMTypeRef type = LLVMIntrinsicGetType(context->context, intrinsic_id, types, arg_count);
	return LLVMBuildCall2(context->builder, type, decl, values, arg_count, "");
}

BEValue llvm_emit_assign_expr(GenContext *c, BEValue *ref, Expr *expr, LLVMValueRef failable)
{
	assert(ref->kind == BE_ADDRESS || ref->kind == BE_ADDRESS_FAILABLE);
	LLVMBasicBlockRef assign_block = NULL;

	PUSH_ERROR();

	if (failable)
	{
		if (expr->failable)
		{
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
	if (expr->expr_kind == EXPR_COMPOUND_LITERAL) expr = expr->expr_compound_literal.initializer;
	if (expr->expr_kind == EXPR_INITIALIZER_LIST)
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
		llvm_store_self_aligned(c, failable, llvm_get_zero(c, type_error), type_error);
	}
	POP_ERROR();

	if (failable && expr->failable)
	{
		llvm_emit_br(c, assign_block);
		llvm_emit_block(c, assign_block);
	}

	return value;
}



static inline void gencontext_emit_failable(GenContext *context, BEValue *be_value, Expr *expr)
{
	Expr *fail = expr->failable_expr;
	if (context->error_var)
	{
		assert(context->error_var);
		llvm_emit_expr(context, be_value, fail);
		printf("TODO // fix failable \n");
		LLVMBuildStore(context->builder, llvm_value_rvalue_store(context, be_value),
		               llvm_emit_bitcast(context, context->error_var, type_get_ptr(fail->type)));
	}
	llvm_emit_br(context, context->catch_block);
	LLVMBasicBlockRef ignored_block = llvm_basic_block_new(context, "postfailed");
	llvm_emit_block(context, ignored_block);
	if (expr->type->canonical == type_void)
	{
		llvm_value_set(be_value, NULL, type_void);
		return;
	}
	llvm_value_set(be_value, LLVMGetUndef(llvm_get_type(context, expr->type)), expr->type);
}

static inline void llvm_emit_initializer_list_expr(GenContext *c, BEValue *value, Expr *expr)
{
	llvm_value_set_address(value, llvm_emit_alloca_aligned(c, expr->type, "literal"), expr->type);
	llvm_emit_initialize_reference(c, value, expr);
}

void llvm_emit_expr(GenContext *c, BEValue *value, Expr *expr)
{
	EMIT_LOC(c, expr);
	switch (expr->expr_kind)
	{
		case EXPR_DESIGNATOR:
		case EXPR_MEMBER_ACCESS:
		case EXPR_POISONED:
		case EXPR_DECL_LIST:
		case EXPR_TYPEINFO:
		case EXPR_ENUM_CONSTANT:
		case EXPR_MACRO_IDENTIFIER:
		case EXPR_MACRO_CT_IDENTIFIER:
		case EXPR_CT_IDENT:
		case EXPR_HASH_IDENT:
			UNREACHABLE
		case EXPR_UNDEF:
			// Should never reach this.
			UNREACHABLE
		case EXPR_SLICE_ASSIGN:
			gencontext_emit_slice_assign(c, value, expr);
			return;
		case EXPR_SLICE:
			gencontext_emit_slice(c, value, expr);
			return;
		case EXPR_LEN:
			llvm_emit_len(c, value, expr);
			return;
		case EXPR_FAILABLE:
			gencontext_emit_failable(c, value, expr);
			return;
		case EXPR_TRY:
		case EXPR_CATCH:
			gencontext_emit_trycatch_expr(c, value, expr);
			return;
		case EXPR_ELSE:
			gencontext_emit_else_expr(c, value, expr);
			return;
		case EXPR_MACRO_BLOCK:
			llvm_emit_macro_block(c, value, expr);
			return;
		case EXPR_COMPOUND_LITERAL:
			UNREACHABLE
		case EXPR_INITIALIZER_LIST:
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
		case EXPR_BINARY:
			gencontext_emit_binary_expr(c, value, expr);
			return;
		case EXPR_TERNARY:
			gencontext_emit_ternary_expr(c, value, expr);
			return;
		case EXPR_POST_UNARY:
			llvm_emit_post_unary_expr(c, value, expr);
			return;
		case EXPR_GUARD:
			gencontext_emit_guard_expr(c, value, expr);
			return;
		case EXPR_TYPEID:
			gencontext_emit_typeid(c, value, expr);
			return;
		case EXPR_TYPEOF:
			// These are folded in the semantic analysis step.
			UNREACHABLE
		case EXPR_IDENTIFIER:
		case EXPR_CONST_IDENTIFIER:
			llvm_value_set_decl_address(value, expr->identifier_expr.decl);
			return;
		case EXPR_SUBSCRIPT:
			gencontext_emit_subscript(c, value, expr);
			return;
		case EXPR_ACCESS:
			gencontext_emit_access_addr(c, value, expr);
			return;
		case EXPR_CALL:
			llvm_emit_call_expr(c, value, expr);
			return;
		case EXPR_GROUP:
			expr = expr->group_expr;
			return;
		case EXPR_EXPRESSION_LIST:
			gencontext_emit_expression_list_expr(c, value, expr);
			return;
		case EXPR_CAST:
			gencontext_emit_cast_expr(c, value, expr);
			return;
	}
	UNREACHABLE
}