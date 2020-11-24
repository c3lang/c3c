// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_internal.h"
#include "compiler_internal.h"
#include "bigint.h"

static LLVMValueRef llvm_emit_int_comparison(GenContext *c, Type *lhs_type, Type *rhs_type, LLVMValueRef lhs_value, LLVMValueRef rhs_value, BinaryOp binary_op);
static void gencontext_emit_unary_expr(GenContext *context, BEValue *value, Expr *expr);
static inline void llvm_emit_post_inc_dec(GenContext *c, BEValue *value, Expr *expr, int diff, bool use_mod);
static inline void llvm_emit_pre_inc_dec(GenContext *c, BEValue *value, Expr *expr, int diff, bool use_mod);
static inline void llvm_emit_inc_dec_change(GenContext *c, bool use_mod, BEValue *addr, BEValue *after, BEValue *before, Expr *expr, int diff);
static void llvm_emit_post_unary_expr(GenContext *context, BEValue *be_value, Expr *expr);
static inline LLVMValueRef llvm_emit_subscript_addr_with_base_new(GenContext *c, BEValue *parent, BEValue *index);

LLVMValueRef llvm_emit_is_no_error(GenContext *c, LLVMValueRef error)
{
	LLVMValueRef domain = LLVMBuildExtractValue(c->builder, error, 0, "");
	return LLVMBuildICmp(c->builder, LLVMIntEQ, domain, llvm_get_zero(c, type_usize), "noerr");
}

static inline LLVMValueRef gencontext_emit_add_int(GenContext *context, Type *type, bool use_mod, LLVMValueRef left, LLVMValueRef right)
{
	if (use_mod)
	{
		return LLVMBuildAdd(context->builder, left, right, "add_mod");
	}

	if (build_options.debug_mode)
	{
		LLVMTypeRef type_to_use = llvm_get_type(context, type->canonical);
		LLVMValueRef args[2] = { left, right };
		assert(type->canonical == type);
		LLVMValueRef add_res;
		if (type_is_unsigned(type))
		{
			add_res = llvm_emit_call_intrinsic(context, intrinsic_id_uadd_overflow, &type_to_use, 1, args, 2);
		}
		else
		{
			add_res = llvm_emit_call_intrinsic(context, intrinsic_id_sadd_overflow, &type_to_use, 1, args, 2);
		}
		LLVMValueRef result = LLVMBuildExtractValue(context->builder, add_res, 0, "");
		LLVMValueRef ok = LLVMBuildExtractValue(context->builder, add_res, 1, "");
		llvm_emit_panic_on_true(context, ok, "Addition overflow");
		return result;
	}
	return type_is_integer_unsigned(type)
		? LLVMBuildNUWAdd(context->builder, left, right, "uadd")
		: LLVMBuildNSWAdd(context->builder, left, right, "add");
}

LLVMValueRef llvm_emit_coerce(GenContext *context, LLVMTypeRef coerced, BEValue *value, Type *original_type)
{
	LLVMValueRef cast;
	unsigned target_alignment = llvm_abi_alignment(coerced);
	unsigned max_align = MAX(((unsigned)value->alignment), llvm_abi_alignment(coerced));

	// If we are loading something with greater alignment than what we have, we cannot directly memcpy.
	if (llvm_value_is_addr(value) && value->alignment < target_alignment)
	{
		// So load it instead.
		llvm_value_rvalue(context, value);
	}

	// In this case we have something nicely aligned, so we just do a cast.
	if (llvm_value_is_addr(value))
	{
		cast = LLVMBuildBitCast(context->builder, value->value, LLVMPointerType(coerced, 0), "");
	}
	else
	{
		cast = llvm_emit_alloca(context, coerced, max_align, "coerce");
		LLVMValueRef target = LLVMBuildBitCast(context->builder, cast, llvm_get_ptr_type(context, value->type), "");
		llvm_store_bevalue_aligned(context, target, value, max_align);
	}
	return llvm_emit_load_aligned(context, coerced, cast, max_align, "coerced");
}

LLVMValueRef llvm_emit_convert_value_from_coerced(GenContext *context, LLVMTypeRef coerced, LLVMValueRef value, Type *original_type)
{
	unsigned max_align = MAX(llvm_abi_alignment(coerced), type_abi_alignment(original_type));
	LLVMValueRef temp = llvm_emit_alloca(context, coerced, max_align, "coerce_temp");
	llvm_store_aligned(context, temp, value, max_align);
	temp = LLVMBuildBitCast(context->builder, temp, llvm_get_type(context, type_get_ptr(original_type)), "");
	return llvm_emit_load_aligned(context, llvm_get_type(context, original_type), temp, max_align, "coerced");
}

static inline LLVMValueRef gencontext_emit_sub_int(GenContext *context, Type *type, bool use_mod, LLVMValueRef left, LLVMValueRef right)
{
	if (use_mod)
	{
		return LLVMBuildSub(context->builder, left, right, "sub_mod");
	}

	if (build_options.debug_mode)
	{
		LLVMTypeRef type_to_use = llvm_get_type(context, type);
		LLVMValueRef args[2] = { left, right };
		assert(type->canonical == type);
		LLVMValueRef add_res;
		if (type_is_unsigned(type))
		{
			add_res = llvm_emit_call_intrinsic(context, intrinsic_id_usub_overflow, &type_to_use, 1, args, 2);
		}
		else
		{
			add_res = llvm_emit_call_intrinsic(context, intrinsic_id_ssub_overflow, &type_to_use, 1, args, 2);
		}
		LLVMValueRef result = LLVMBuildExtractValue(context->builder, add_res, 0, "");
		LLVMValueRef ok = LLVMBuildExtractValue(context->builder, add_res, 1, "");
		llvm_emit_panic_on_true(context, ok, "Subtraction overflow");
		return result;
	}


	return type_is_integer_unsigned(type)
	       ? LLVMBuildNUWSub(context->builder, left, right, "usub")
	       : LLVMBuildNSWSub(context->builder, left, right, "sub");
}

static inline void gencontext_emit_subscript_addr_base(GenContext *context, BEValue *value, Expr *parent)
{
	LLVMValueRef parent_value;
	Type *type = parent->type->canonical;
	switch (type->type_kind)
	{
		case TYPE_POINTER:
			llvm_emit_expr(context, value, parent);
			llvm_value_rvalue(context, value);
			value->kind = BE_ADDRESS;
			return;
		case TYPE_ARRAY:
			llvm_emit_expr(context, value, parent);
			return;
		case TYPE_SUBARRAY:
		{
			// TODO insert trap on overflow.
			LLVMTypeRef subarray_type = llvm_get_type(context, type);
			llvm_emit_expr(context, value, parent);
			assert(value->kind == BE_ADDRESS);
			LLVMValueRef pointer_addr = LLVMBuildStructGEP2(context->builder, subarray_type, value->value, 0, "subarray_ptr");
			LLVMTypeRef pointer_type = llvm_get_type(context, type_get_ptr(type->array.base));
			unsigned alignment = type_abi_alignment(type_voidptr);
			// We need to pick the worst alignment in case this is packed in an array.
			if (value->alignment < alignment) alignment = value->alignment;
			llvm_value_set_address_align(value,
			                             llvm_emit_load_aligned(context, pointer_type, pointer_addr, 0, "subarrptr"), type, alignment);
			return;
		}
		case TYPE_VARARRAY:
		case TYPE_STRING:
			TODO
		default:
			UNREACHABLE

	}
}

static inline LLVMValueRef llvm_emit_subscript_addr_with_base(GenContext *c, Type *parent_type, LLVMValueRef parent_value, LLVMValueRef index_value)
{
	Type *type = parent_type;
	switch (type->type_kind)
	{
		case TYPE_POINTER:
			return LLVMBuildInBoundsGEP2(c->builder,
			                             llvm_get_type(c, type->pointer),
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
		case TYPE_STRING:
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
	Type *type = parent->type->canonical;
	switch (type->type_kind)
	{
		case TYPE_POINTER:
			return LLVMBuildInBoundsGEP(c->builder, parent->value, &index->value, 1, "ptridx");
		case TYPE_ARRAY:
		{
			if (build_options.debug_mode)
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
			if (build_options.debug_mode)
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
		case TYPE_STRING:
			TODO
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
	gencontext_emit_subscript_addr_base(c, &ref, expr->subscript_expr.expr);
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

static LLVMValueRef gencontext_emit_member_addr(GenContext *context, LLVMValueRef value, Decl *parent, Decl *member)
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
				value = LLVMBuildBitCast(context->builder, value, LLVMPointerType(llvm_get_type(context, found->type), 0), name);
				break;
			case TYPE_ERRTYPE:
				value = LLVMBuildStructGEP2(context->builder, llvm_get_type(context, parent->type), value, index + 1, name);
				break;
			case TYPE_STRUCT:
				value = LLVMBuildStructGEP2(context->builder, llvm_get_type(context, parent->type), value, index, name);
				break;
			default:
				UNREACHABLE
		}
		parent = found;
	} while (found != member);
	return value;
}


static inline void gencontext_emit_access_addr(GenContext *context, BEValue *be_value, Expr *expr)
{
	Expr *parent = expr->access_expr.parent;
	llvm_emit_expr(context, be_value, parent);
	Decl *member = expr->access_expr.ref;

	llvm_value_set_address(be_value, gencontext_emit_member_addr(context, be_value->value, parent->type->canonical->decl, member), expr->type);
}

static void gencontext_emit_scoped_expr(GenContext *context, BEValue *value, Expr *expr)
{
	llvm_emit_expr(context, value, expr->expr_scope.expr);
	llvm_emit_defer(context, expr->expr_scope.defers.start, expr->expr_scope.defers.end);
}


static inline LLVMValueRef llvm_emit_initializer_list_expr_addr(GenContext *c, Expr *expr, LLVMValueRef optional_ref);

static inline void gencontext_emit_identifier(GenContext *c, BEValue *value, Decl *decl)
{
	llvm_value_set_address(value, decl_ref(decl), decl->type);
	value->alignment = decl_abi_alignment(decl);

	if (decl->decl_kind == DECL_VAR && decl->var.failable)
	{
		value->kind = BE_ADDRESS_FAILABLE;
		value->failable = decl->var.failable_ref;
	}

}

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
	size_t size = from_type->pointer->array.len;
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
static LLVMValueRef gencontext_emit_cast_inner(GenContext *c, CastKind cast_kind, BEValue *value, Type *to_type, Type *from_type)
{
	switch (cast_kind)
	{
		case CAST_CXBOOL:
			TODO
		case CAST_XIERR:
			// TODO Insert zero check.
			llvm_value_rvalue(c, value);
			return value->value;
		case CAST_ERROR:
			UNREACHABLE
		case CAST_PTRPTR:
			llvm_value_rvalue(c, value);
			return LLVMBuildPointerCast(c->builder, value->value, llvm_get_type(c, to_type), "ptrptr");
		case CAST_PTRXI:
			llvm_value_rvalue(c, value);
			return LLVMBuildPtrToInt(c->builder, value->value, llvm_get_type(c, to_type), "ptrxi");
		case CAST_APTSA:
			gencontext_emit_arr_to_subarray_cast(c, value, to_type, from_type);
			return value->value;
		case CAST_SAPTR:
			if (value->kind == BE_ADDRESS)
			{
				LLVMBuildStructGEP2(c->builder, llvm_get_ptr_type(c, from_type), value->value, 0, "");
			}
			return LLVMBuildExtractValue(c->builder, value->value, 0, "");
		case CAST_VARRPTR:
			return value->value;
		case CAST_ARRPTR:
			TODO
		case CAST_STRPTR:
			TODO
		case CAST_EREU:
		case CAST_EUER:
			TODO
			return gencontext_emit_value_bitcast(c, value->value, to_type, from_type);
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
			return LLVMBuildICmp(c->builder, LLVMIntNE, value->value, llvm_get_zero(c, type_usize), "eubool");
		case CAST_PTRBOOL:
			llvm_value_rvalue(c, value);
			return LLVMBuildIsNotNull(c->builder, value->value, "ptrbool");
		case CAST_BOOLINT:
			llvm_value_rvalue(c, value);
			return LLVMBuildZExt(c->builder, value->value, llvm_get_type(c, to_type), "boolsi");
		case CAST_FPBOOL:
			llvm_value_rvalue(c, value);
			return LLVMBuildFCmp(c->builder, LLVMRealUNE, value->value, llvm_get_zero(c, from_type), "fpbool");
		case CAST_BOOLFP:
			llvm_value_rvalue(c, value);
			return LLVMBuildUIToFP(c->builder, value->value, llvm_get_type(c, to_type), "boolfp");
		case CAST_INTBOOL:
			llvm_value_rvalue(c, value);
			return LLVMBuildICmp(c->builder, LLVMIntNE, value->value, llvm_get_zero(c, from_type), "intbool");
		case CAST_FPFP:
			llvm_value_rvalue(c, value);
			return type_convert_will_trunc(to_type, from_type)
			       ? LLVMBuildFPTrunc(c->builder, value->value, llvm_get_type(c, to_type), "fpfptrunc")
			       : LLVMBuildFPExt(c->builder, value->value, llvm_get_type(c, to_type), "fpfpext");
		case CAST_FPSI:
			llvm_value_rvalue(c, value);
			return LLVMBuildFPToSI(c->builder, value->value, llvm_get_type(c, to_type), "fpsi");
		case CAST_FPUI:
			llvm_value_rvalue(c, value);
			return LLVMBuildFPToUI(c->builder, value->value, llvm_get_type(c, to_type), "fpui");
		case CAST_SISI:
			llvm_value_rvalue(c, value);
			return type_convert_will_trunc(to_type, from_type)
			       ? LLVMBuildTrunc(c->builder, value->value, llvm_get_type(c, to_type), "sisitrunc")
			       : LLVMBuildSExt(c->builder, value->value, llvm_get_type(c, to_type), "sisiext");
		case CAST_SIUI:
			llvm_value_rvalue(c, value);
			return type_convert_will_trunc(to_type, from_type)
			       ? LLVMBuildTrunc(c->builder, value->value, llvm_get_type(c, to_type), "siuitrunc")
			       : LLVMBuildZExt(c->builder, value->value, llvm_get_type(c, to_type), "siuiext");
		case CAST_SIFP:
			llvm_value_rvalue(c, value);
			return LLVMBuildSIToFP(c->builder, value->value, llvm_get_type(c, to_type), "sifp");
		case CAST_XIPTR:
			llvm_value_rvalue(c, value);
			return LLVMBuildIntToPtr(c->builder, value->value, llvm_get_type(c, to_type), "xiptr");
		case CAST_UISI:
			llvm_value_rvalue(c, value);
			return type_convert_will_trunc(to_type, from_type)
			       ? LLVMBuildTrunc(c->builder, value->value, llvm_get_type(c, to_type), "uisitrunc")
			       : LLVMBuildZExt(c->builder, value->value, llvm_get_type(c, to_type), "uisiext");
		case CAST_UIUI:
			llvm_value_rvalue(c, value);
			return type_convert_will_trunc(to_type, from_type)
			       ? LLVMBuildTrunc(c->builder, value->value, llvm_get_type(c, to_type), "uiuitrunc")
			       : LLVMBuildZExt(c->builder, value->value, llvm_get_type(c, to_type), "uiuiext");
		case CAST_UIFP:
			llvm_value_rvalue(c, value);
			return LLVMBuildUIToFP(c->builder, value->value, llvm_get_type(c, to_type), "uifp");
		case CAST_ENUMLOW:
			llvm_value_rvalue(c, value);
			return value->value;
	}
	UNREACHABLE
}

void llvm_emit_cast(GenContext *c, CastKind cast_kind, BEValue *value, Type *to_type, Type *from_type)
{
	value->value = gencontext_emit_cast_inner(c, cast_kind, value, to_type, from_type);
	value->type = to_type;
	value->kind = to_type->type_kind == TYPE_BOOL ? BE_BOOLEAN : BE_VALUE;
}

static inline void gencontext_emit_cast_expr(GenContext *context, BEValue *be_value, Expr *expr)
{
	llvm_emit_expr(context, be_value, expr->cast_expr.expr);
	llvm_emit_cast(context,
	               expr->cast_expr.kind,
	               be_value,
	               expr->type->canonical,
	               expr->cast_expr.expr->type->canonical);
}

static void gencontext_emit_initializer_list_expr_const(GenContext *context, BEValue *be_value, Expr *expr);

static void gencontext_construct_const_value(GenContext *context, BEValue *be_value, Expr *expr)
{
	NESTED_RETRY:
	if (expr->expr_kind == EXPR_COMPOUND_LITERAL)
	{
		expr = expr->expr_compound_literal.initializer;
		goto NESTED_RETRY;
	}
	if (expr->expr_kind == EXPR_INITIALIZER_LIST)
	{
		gencontext_emit_initializer_list_expr_const(context, be_value, expr);
		return;
	}
	llvm_emit_expr(context, be_value, expr);
}

static void gencontext_construct_const_union_struct(GenContext *context, BEValue *be_value, Type *canonical, Expr *value)
{
	LLVMValueRef values[2];
	gencontext_construct_const_value(context, be_value, value);
	values[0] = llvm_value_rvalue_store(context, be_value);
	unsigned size_diff = type_size(canonical) - type_size(value->type);
	if (size_diff > 0)
	{
		LLVMTypeRef size = LLVMArrayType(llvm_get_type(context, type_char), size_diff);
		values[1] = LLVMConstNull(size);
	}
	llvm_value_set(be_value,
	               LLVMConstStructInContext(context->context, values, size_diff > 0 ? 2 : 1, false),
	               canonical);
}
static void gencontext_recursive_set_const_value(GenContext *context, BEValue *be_value, DesignatedPath *path, LLVMValueRef parent, Type *parent_type, Expr *value)
{
	switch (path->kind)
	{
		case DESIGNATED_IDENT:
			if (!path->sub_path)
			{
				if (parent_type->canonical->type_kind == TYPE_UNION)
				{
					gencontext_construct_const_union_struct(context, be_value, parent_type, value);
					return;
				}
				gencontext_construct_const_value(context, be_value, value);
				llvm_value_set(be_value,
				               LLVMConstInsertValue(parent, llvm_value_rvalue_store(context, be_value), &path->index, 1),
				               parent_type);
				return;
			}
			else
			{
				parent_type = path->type;
				gencontext_recursive_set_const_value(context, be_value, path->sub_path,
				                                     LLVMConstExtractValue(parent, &path->index, 1), parent_type,
				                                     value);
				llvm_value_set(be_value,
				               LLVMConstInsertValue(parent, llvm_value_rvalue_store(context, be_value), &path->index, 1),
				               parent_type);
				return;
			}
		case DESIGNATED_SUBSCRIPT:
		{
			// TODO range, more arrays
			assert(path->index_expr->expr_kind == EXPR_CONST);
			unsigned int index = (unsigned int)bigint_as_unsigned(&path->index_expr->const_expr.i);
			if (!path->sub_path)
			{
				gencontext_construct_const_value(context, be_value, value);
				llvm_value_set(be_value,
				               LLVMConstInsertValue(parent, llvm_value_rvalue_store(context, be_value), &index, 1),
				               parent_type);
				return ;
			}
			parent_type = path->type;
			gencontext_recursive_set_const_value(context, be_value, path->sub_path,
			                                     LLVMConstExtractValue(parent, &index, 1), parent_type,
			                                     value);
			llvm_value_set(be_value,
			               LLVMConstInsertValue(parent, llvm_value_rvalue_store(context, be_value), &index, 1),
			               parent_type);
			return;
		}
		default:
			UNREACHABLE;

	}

}

static void gencontext_emit_initializer_list_expr_const(GenContext *context, BEValue *be_value, Expr *expr)
{
	Type *canonical = expr->type->canonical;
	LLVMTypeRef type = llvm_get_type(context, canonical);

	if (expr->expr_initializer.init_type == INITIALIZER_ZERO)
	{
		llvm_value_set(be_value, LLVMConstNull(type), canonical);
		return;
	}

	bool is_error = expr->type->canonical->type_kind == TYPE_ERRTYPE;

	if (is_error)
	{
		TODO
	}

	Expr **elements = expr->expr_initializer.initializer_expr;

	bool is_union = expr->type->canonical->type_kind == TYPE_UNION;

	if (expr->expr_initializer.init_type == INITIALIZER_NORMAL && is_union)
	{
		assert(vec_size(elements) == 1);
		gencontext_construct_const_union_struct(context, be_value, canonical, elements[0]);
		return;
	}

	if (expr->expr_initializer.init_type == INITIALIZER_NORMAL)
	{
		LLVMValueRef value = LLVMGetUndef(type);
		VECEACH(elements, i)
		{
			Expr *element = elements[i];
			if (element->expr_kind == EXPR_CONST)
			{

			}
			llvm_emit_expr(context, be_value, element);
			value = LLVMConstInsertValue(value, llvm_value_rvalue_store(context, be_value), &i, 1);
		}
		llvm_value_set(be_value, value, expr->type);
		return;
	}

	LLVMValueRef value = LLVMConstNull(type);
	VECEACH(elements, i)
	{
		Expr *element = elements[i];
		DesignatedPath *path = element->designated_init_expr.path;
		Type *parent_type = expr->type->canonical;
		gencontext_recursive_set_const_value(context,
		                                     be_value,
		                                     path,
		                                     value,
		                                     parent_type,
		                                     element->designated_init_expr.value);
		value = llvm_value_rvalue_store(context, be_value);
	}
	llvm_value_set(be_value, value, expr->type);
}

/**
 * Emit a Foo { .... } literal.
 *
 * Improve: Direct assign in the case where this is assigning to a variable.
 * Improve: Create constant initializer for the constant case and do a memcopy
 */
static inline LLVMValueRef llvm_emit_initializer_list_expr_addr(GenContext *c, Expr *expr, LLVMValueRef optional_ref)
{
	if (expr->constant && type_size(expr->type) <= type_size(type_voidptr) * 4)
	{
		LLVMTypeRef type = llvm_get_type(c, expr->type);
		BEValue be_value;
		gencontext_emit_initializer_list_expr_const(c, &be_value, expr);
		LLVMValueRef ref = LLVMAddGlobal(c->module, type, "");
		LLVMSetInitializer(ref, llvm_value_rvalue_store(c, &be_value));
		LLVMSetGlobalConstant(ref, true);
		if (optional_ref)
		{
			LLVMBuildMemCpy(c->builder, optional_ref, LLVMGetAlignment(optional_ref), ref, LLVMGetAlignment(ref), LLVMSizeOf(type));
		}
		return ref;
	}
	LLVMTypeRef type = llvm_get_type(c, expr->type);
	LLVMValueRef ref = optional_ref ?: llvm_emit_alloca_aligned(c, expr->type, "literal");

	Type *canonical = expr->type->canonical;
	if (expr->expr_initializer.init_type != INITIALIZER_NORMAL)
	{
		llvm_emit_memclear(c, ref, canonical);
	}

	bool is_error = expr->type->canonical->type_kind == TYPE_ERRTYPE;

	if (is_error)
	{
		LLVMValueRef err_type = LLVMBuildStructGEP2(c->builder, type, ref, 0, "");
		LLVMBuildStore(c->builder, expr->type->canonical->backend_typeid, err_type);
	}

	if (expr->expr_initializer.init_type == INITIALIZER_ZERO)
	{
		return ref;
	}

	Expr **elements = expr->expr_initializer.initializer_expr;

	bool is_union = expr->type->canonical->type_kind == TYPE_UNION;

	if (expr->expr_initializer.init_type == INITIALIZER_NORMAL)
	{
		if (is_union)
		{
			assert(vec_size(elements) == 1);
			BEValue init_value;
			llvm_emit_expr(c, &init_value, elements[0]);
			LLVMValueRef u = LLVMBuildBitCast(c->builder, ref, llvm_get_ptr_type(c, elements[0]->type->canonical), "");
			LLVMBuildStore(c->builder, llvm_value_rvalue_store(c, &init_value), u);
			return ref;
		}
		VECEACH(elements, i)
		{
			Expr *element = elements[i];
			BEValue init_value;
			llvm_emit_expr(c, &init_value, element);
			LLVMValueRef subref = LLVMBuildStructGEP2(c->builder, type, ref, i + (int)is_error, "");
			llvm_store_self_aligned(c, subref, llvm_value_rvalue_store(c, &init_value), element->type);
		}
		return ref;
	}


	VECEACH(elements, i)
	{
		if (is_error) TODO
		Expr *element = elements[i];
		DesignatedPath *path = element->designated_init_expr.path;
		BEValue sub_value;
		llvm_emit_expr(c, &sub_value, element->designated_init_expr.value);
		LLVMValueRef sub_ref = ref;
		Type *parent_type = expr->type->canonical;
		while (path)
		{
			switch (path->kind)
			{
				case DESIGNATED_IDENT:
					if (parent_type->canonical->type_kind == TYPE_UNION)
					{
						sub_ref = LLVMBuildBitCast(c->builder, sub_ref, llvm_get_ptr_type(c, path->type), path->type->name);
					}
					else
					{
						sub_ref = LLVMBuildStructGEP2(c->builder, llvm_get_type(c, parent_type), sub_ref, path->index, path->type->name);
					}
					break;
				case DESIGNATED_SUBSCRIPT:
				{
					// TODO range, more arrays
					LLVMValueRef zero = llvm_get_zero(c, type_int);
					BEValue index;
					llvm_emit_expr(c, &index, path->index_expr);
					LLVMValueRef indices[2] = {
							zero,
							llvm_value_rvalue_store(c, &index),
					};
					sub_ref = LLVMBuildInBoundsGEP2(c->builder,
					                                llvm_get_type(c, parent_type),
					                                sub_ref, indices, 2, "arrsub");
					break;
				}
				default:
					UNREACHABLE;

			}
			parent_type = path->type;
			path = path->sub_path;
		}
		LLVMBuildStore(c->builder, llvm_value_rvalue_store(c, &sub_value), sub_ref);
	}
	return ref;
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
			LLVMValueRef add = LLVMConstInt(diff < 0 ? llvm_get_type(c, type_char) : llvm_get_type(c, type_byte), diff, diff < 0);
			after_value = LLVMBuildGEP2(c->builder, llvm_get_type(c, type->pointer), value.value, &add, 1, "ptrincdec");
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
					? gencontext_emit_add_int(c, type, use_mod, value.value, diff_value)
					: gencontext_emit_sub_int(c, type, use_mod, value.value, diff_value);
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
		case UNARYOP_NEGMOD:
			llvm_emit_expr(c, value, expr->unary_expr.expr);
			llvm_value_rvalue(c, value);
			value->value = LLVMBuildNeg(c->builder, value->value, "negmod");
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
			{
				LLVMValueRef zero = llvm_get_zero(c, expr->unary_expr.expr->type);
				if (build_options.debug_mode)
				{
					// TODO
					LLVMTypeRef type_to_use = llvm_get_type(c, type->canonical);
					LLVMValueRef args[2] = { zero, value->value };
					LLVMValueRef call_res = llvm_emit_call_intrinsic(c, intrinsic_id_ssub_overflow,
					                                                 &type_to_use, 1, args, 2);
					value->value = LLVMBuildExtractValue(c->builder, call_res, 0, "");
					LLVMValueRef ok = LLVMBuildExtractValue(c->builder, call_res, 1, "");
					llvm_emit_panic_on_true(c, ok, "Signed negation overflow");
					return;
				}
				value->value = LLVMBuildNSWSub(c->builder, zero, value->value, "neg");
				return;
			}
		case UNARYOP_ADDR:
			llvm_emit_expr(c, value, expr->unary_expr.expr);
			// Create an addr
			llvm_value_addr(c, value);
			// Transform to value
			value->kind = BE_VALUE;
			value->type = expr->type;
			return;
		case UNARYOP_DEREF:
			llvm_emit_expr(c, value, expr->unary_expr.expr);
			// Load the pointer value.
			llvm_value_rvalue(c, value);
			// Convert pointer to address
			value->kind = BE_ADDRESS;
			value->type = expr->type;
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

static void gencontext_emit_len(GenContext *c, BEValue *be_value, Expr *expr)
{
	Expr *inner = expr->len_expr.inner;
	llvm_emit_expr(c, be_value, inner);
	llvm_value_addr(c, be_value);
	Type *type = inner->type;
	switch (type->canonical->type_kind)
	{
		case TYPE_SUBARRAY:
		{
			LLVMTypeRef subarray_type = llvm_get_type(c, type);
			LLVMValueRef len_addr = LLVMBuildStructGEP2(c->builder, subarray_type, be_value->value, 1, "len");
			llvm_value_set_address(be_value, len_addr, type_usize);
			break;
		}
		case TYPE_ARRAY:
			llvm_value_set(be_value, llvm_const_int(c, type_usize, type->array.len), type_usize);
			break;
		case TYPE_VARARRAY:
		case TYPE_STRING:
			TODO
		default:
			UNREACHABLE
	}
}

static void gencontext_emit_trap_negative(GenContext *context, Expr *expr, LLVMValueRef value, const char *error)
{
	if (!build_options.debug_mode) return;
	if (type_is_integer_unsigned(expr->type->canonical)) return;

	LLVMValueRef zero = llvm_const_int(context, expr->type, 0);
	LLVMValueRef ok = LLVMBuildICmp(context->builder, LLVMIntSLT, value, zero, "underflow");
	llvm_emit_panic_on_true(context, ok, error);
}

static void
gencontext_emit_slice_values(GenContext *context, Expr *slice, Type **parent_type_ref, LLVMValueRef *parent_base_ref,
                             Type **start_type_ref, LLVMValueRef *start_index_ref, Type **end_type_ref,
                             LLVMValueRef *end_index_ref)
{
	assert(slice->expr_kind == EXPR_SLICE);

	Expr *parent_expr = slice->slice_expr.expr;
	Type *parent_type = parent_expr->type->canonical;
	BEValue parent_addr_x;
	llvm_emit_expr(context, &parent_addr_x, parent_expr);
	llvm_value_addr(context, &parent_addr_x);
	LLVMValueRef parent_addr = parent_addr_x.value;
	LLVMValueRef parent_load_value;
	LLVMValueRef parent_base;
	switch (parent_type->type_kind)
	{
		case TYPE_POINTER:
			parent_load_value = parent_base = gencontext_emit_load(context, parent_type, parent_addr);
			break;
		case TYPE_SUBARRAY:
			parent_load_value = gencontext_emit_load(context, parent_type, parent_addr);
			parent_base = LLVMBuildExtractValue(context->builder, parent_load_value, 0, "");
			break;
		case TYPE_ARRAY:
			parent_base = parent_addr;
			break;
		case TYPE_VARARRAY:
		case TYPE_STRING:
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
	llvm_emit_expr(context, &start_index, start);
	llvm_value_rvalue(context, &start_index);

	LLVMValueRef len;
	if (!end || slice->slice_expr.start_from_back || slice->slice_expr.end_from_back || build_options.debug_mode)
	{
		switch (parent_type->type_kind)
		{
			case TYPE_POINTER:
				len = NULL;
				break;
			case TYPE_SUBARRAY:
				len = LLVMBuildExtractValue(context->builder, parent_load_value, 1, "");
				break;
			case TYPE_ARRAY:
				len = llvm_const_int(context, type_usize, parent_type->array.len);
				break;
			case TYPE_VARARRAY:
			case TYPE_STRING:
				TODO
			default:
				UNREACHABLE
		}
	}

	// Walk from end if it is slice from the back.
	if (slice->slice_expr.start_from_back)
	{
		start_index.value = gencontext_emit_sub_int(context, start_type, false, len, start_index.value);
	}

	// Check that index does not extend beyond the length.
	if (parent_type->type_kind != TYPE_POINTER && build_options.debug_mode)
	{
		LLVMValueRef exceeds_size = llvm_emit_int_comparison(context,
		                                                     type_usize,
		                                                     start_type,
		                                                     len,
		                                                     start_index.value,
		                                                     BINARYOP_GE);
		llvm_emit_panic_on_true(context, exceeds_size, "Index exceeds array length.");
	}

	// Insert trap for negative start offset for non pointers.
	if (parent_type->type_kind != TYPE_POINTER)
	{
		gencontext_emit_trap_negative(context, start, start_index.value, "Negative index");
	}

	Type *end_type;
	BEValue end_index;

	if (end)
	{
		// Get the index.
		llvm_emit_expr(context, &end_index, end);
		llvm_value_rvalue(context, &end_index);
		end_type = end->type->canonical;

		// Reverse if it is "from back"
		if (slice->slice_expr.end_from_back)
		{
			end_index.value = gencontext_emit_sub_int(context, end_type, false, len, end_index.value);
		}

		// This will trap any bad negative index, so we're fine.
		if (build_options.debug_mode)
		{
			LLVMValueRef excess = llvm_emit_int_comparison(context,
			                                               start_type,
			                                               end_type,
			                                               start_index.value,
			                                               *end_index_ref,
			                                               BINARYOP_GT);
			llvm_emit_panic_on_true(context, excess, "Negative size");

			if (len)
			{
				LLVMValueRef exceeds_size = llvm_emit_int_comparison(context,
				                                                     type_usize,
				                                                     end_type,
				                                                     len,
				                                                     end_index.value,
				                                                     BINARYOP_LT);
				llvm_emit_panic_on_true(context, exceeds_size, "Size exceeds index");
			}
		}
	}
	else
	{
		assert(len && "Pointer should never end up here.");
		// Otherwise everything is fine and dandy. Our len is our end index.
		end_index.value = len;
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
	gencontext_emit_slice_values(context, expr, &parent_type,
	                             &parent_base,
	                             &start_type, &start_index, &end_type, &end_index);


	// Calculate the size
	LLVMValueRef size = LLVMBuildSub(context->builder, end_index, start_index, "size");

	LLVMValueRef start_pointer;
	switch (parent_type->type_kind)
	{
		case TYPE_ARRAY:
		{
			Type *pointer_type = type_get_ptr(parent_type->array.base);
			// Change pointer from Foo[x] to Foo*
			parent_base = llvm_emit_bitcast(context, parent_base, pointer_type);
			// Move pointer
			start_pointer = LLVMBuildInBoundsGEP2(context->builder, llvm_get_type(context, pointer_type->pointer), parent_base, &start_index, 1, "offset");
			break;
		}
		case TYPE_SUBARRAY:
		{
			start_pointer = LLVMBuildInBoundsGEP(context->builder, parent_base, &start_index, 1, "offsetsub");
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
	// while (slice_current < end) pointer[slice_current++] = value;

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
	gencontext_emit_slice_values(c, expr->slice_assign_expr.left, &parent_type,
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
	LLVMValueRef not_at_end = llvm_emit_int_comparison(c, start_type, end_type, offset, end_index, BINARYOP_LT);

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
	LLVMValueRef next_offset = gencontext_emit_add_int(c, start_type, false, offset, llvm_const_int(c, start_type, 1));

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



static LLVMValueRef llvm_emit_int_comparison(GenContext *c, Type *lhs_type, Type *rhs_type, LLVMValueRef lhs_value, LLVMValueRef rhs_value, BinaryOp binary_op)
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
		llvm_value_set_bool(be_value,
		                    llvm_emit_int_comparison(c,
		                                             lhs_type,
		                                             rhs_type,
		                                             lhs_value,
		                                             rhs_value,
		                                             binary_op));
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
			if (type_is_integer_unsigned(lhs_type))
			{
				if (build_options.debug_mode)
				{
					LLVMTypeRef type_to_use = llvm_get_type(c, lhs_type);
					LLVMValueRef args[2] = { lhs_value, rhs_value };
					LLVMTypeRef types[2] = { type_to_use, type_to_use };
					LLVMValueRef call_res = llvm_emit_call_intrinsic(c,
					                                                 intrinsic_id_umul_overflow,
					                                                 types,
					                                                 1,
					                                                 args,
					                                                 2);
					val = LLVMBuildExtractValue(c->builder, call_res, 0, "");
					LLVMValueRef ok = LLVMBuildExtractValue(c->builder, call_res, 1, "");
					llvm_emit_panic_on_true(c, ok, "Unsigned multiplication overflow");
					break;
				}
				val = LLVMBuildNUWMul(c->builder, lhs_value, rhs_value, "umul");
				break;
			}
			if (build_options.debug_mode)
			{
				LLVMTypeRef type_to_use = llvm_get_type(c, lhs_type);
				LLVMValueRef args[2] = { lhs_value, rhs_value };
				LLVMTypeRef types[2] = { type_to_use, type_to_use };
				LLVMValueRef call_res = llvm_emit_call_intrinsic(c,
				                                                 intrinsic_id_smul_overflow,
				                                                 types,
				                                                 1,
				                                                 args,
				                                                 2);
				val = LLVMBuildExtractValue(c->builder, call_res, 0, "");
				LLVMValueRef ok = LLVMBuildExtractValue(c->builder, call_res, 1, "");
				llvm_emit_panic_on_true(c, ok, "Signed multiplication overflow");
				break;
			}
			val = LLVMBuildNSWMul(c->builder, lhs_value, rhs_value, "mul");
			break;
		case BINARYOP_MULT_MOD:
			val = LLVMBuildMul(c->builder, lhs_value, rhs_value, "mul");
			break;
		case BINARYOP_SUB:
		case BINARYOP_SUB_MOD:
			if (lhs_type->type_kind == TYPE_POINTER)
			{
				if (lhs_type == rhs_type)
				{
					val = LLVMBuildPtrDiff(c->builder, lhs_value, rhs_value, "ptrdiff");
					break;
				}
				rhs_value = LLVMBuildNeg(c->builder, rhs_value, "");
				val = LLVMBuildGEP2(c->builder, llvm_get_type(c, lhs_type->canonical->pointer), lhs_value, &rhs_value, 1, "ptrsub");
				break;
			}
			if (is_float)
			{
				val = LLVMBuildFSub(c->builder, lhs_value, rhs_value, "fsub");
				break;
			}
			val = gencontext_emit_sub_int(c, lhs_type, binary_op == BINARYOP_SUB_MOD, lhs_value, rhs_value);
			break;
		case BINARYOP_ADD:
		case BINARYOP_ADD_MOD:
			if (lhs_type->type_kind == TYPE_POINTER)
			{
				assert(type_is_integer(rhs_type));
				val = LLVMBuildGEP2(c->builder, llvm_get_type(c, lhs_type->pointer), lhs_value, &rhs_value, 1, "ptradd");
				break;
			}
			if (is_float)
			{
				val = LLVMBuildFAdd(c->builder, lhs_value, rhs_value, "fadd");
				break;
			}
			val = gencontext_emit_add_int(c, lhs_type, binary_op == BINARYOP_ADD_MOD, lhs_value, rhs_value);
			break;
		case BINARYOP_DIV:
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
			val = type_is_unsigned(lhs_type)
			      ? LLVMBuildURem(c->builder, lhs_value, rhs_value, "umod")
			      : LLVMBuildSRem(c->builder, lhs_value, rhs_value, "smod");
			break;
		case BINARYOP_SHR:
			val = type_is_unsigned(lhs_type)
			      ? LLVMBuildLShr(c->builder, lhs_value, rhs_value, "lshr")
			      : LLVMBuildAShr(c->builder, lhs_value, rhs_value, "ashr");
			break;
		case BINARYOP_SHL:
			val = LLVMBuildShl(c->builder, lhs_value, rhs_value, "shl");
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
			llvm_value_set_bool(be_value, LLVMBuildFCmp(c->builder, LLVMRealUEQ, lhs_value, rhs_value, "eq"));
			return;
		case BINARYOP_NE:
			// Unordered?
			assert(type_is_float(lhs_type));
			llvm_value_set_bool(be_value, LLVMBuildFCmp(c->builder, LLVMRealUNE, lhs_value, rhs_value, "neq"));
			return;
		case BINARYOP_GE:
			assert(type_is_float(lhs_type));
			llvm_value_set_bool(be_value, LLVMBuildFCmp(c->builder, LLVMRealUGE, lhs_value, rhs_value, "ge"));
			return;
		case BINARYOP_GT:
			assert(type_is_float(lhs_type));
			llvm_value_set_bool(be_value, LLVMBuildFCmp(c->builder, LLVMRealUGT, lhs_value, rhs_value, "gt"));
			return;
		case BINARYOP_LE:
			assert(type_is_float(lhs_type));
			llvm_value_set_bool(be_value, LLVMBuildFCmp(c->builder, LLVMRealULE, lhs_value, rhs_value, "le"));
			return;
		case BINARYOP_LT:
			assert(type_is_float(lhs_type));
			llvm_value_set_bool(be_value, LLVMBuildFCmp(c->builder, LLVMRealULT, lhs_value, rhs_value, "lt"));
			return;
		case BINARYOP_AND:
		case BINARYOP_OR:
		case BINARYOP_ASSIGN:
		case BINARYOP_MULT_ASSIGN:
		case BINARYOP_MULT_MOD_ASSIGN:
		case BINARYOP_ADD_ASSIGN:
		case BINARYOP_ADD_MOD_ASSIGN:
		case BINARYOP_SUB_ASSIGN:
		case BINARYOP_SUB_MOD_ASSIGN:
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
		LLVMValueRef result = llvm_emit_assign_expr(context, be_value->value, expr->binary_expr.right, failable_ref);
		llvm_value_set(be_value, result, expr->type);
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
	Type *cond_type = expr->ternary_expr.cond->type->canonical;

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
		case TYPE_STRING:
		{
			LLVMValueRef global_name = LLVMAddGlobal(c->module, LLVMArrayType(llvm_get_type(c, type_char), expr->const_expr.string.len + 1), "");
			LLVMSetLinkage(global_name, LLVMInternalLinkage);
			LLVMSetGlobalConstant(global_name, 1);
			LLVMSetInitializer(global_name, LLVMConstStringInContext(c->context,
			                                                         expr->const_expr.string.chars,
			                                                         expr->const_expr.string.len,
			                                                         0));
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
	for (size_t i = 0; i < param_type->array.len; i++)
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
	switch (param_type->type_kind)
	{
		case TYPE_POISONED:
		case TYPE_VOID:
		case TYPE_IXX:
		case TYPE_FXX:
		case TYPE_TYPEID:
		case TYPE_FUNC:
		case TYPE_TYPEINFO:
		case TYPE_MEMBER:
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
		case TYPE_STRING:
		case TYPE_SUBARRAY:
		case TYPE_VECTOR:
			TODO
			break;
	}
}

void gencontext_emit_call_expr(GenContext *context, BEValue *be_value, Expr *expr)
{
	FunctionSignature *signature;
	LLVMTypeRef func_type;
	LLVMValueRef func;
	if (expr->call_expr.is_pointer_call)
	{
		signature = expr->call_expr.function->type->canonical->pointer->func.signature;
		BEValue func_value;
		llvm_emit_expr(context, &func_value, expr->call_expr.function);
		func = llvm_value_rvalue_store(context, &func_value);
		func_type = llvm_get_type(context, expr->call_expr.function->type->canonical->pointer);
	}
	else if (expr->call_expr.is_struct_function)
	{
		Decl *function_decl = expr->call_expr.function->access_expr.ref;
		signature = &function_decl->func.function_signature;
		func = function_decl->backend_ref;
		func_type = llvm_get_type(context, function_decl->type);
	}
	else
	{
		Decl *function_decl = expr->call_expr.function->identifier_expr.decl;
		signature = &function_decl->func.function_signature;
		func = function_decl->backend_ref;
		func_type = llvm_get_type(context, function_decl->type);
	}


	LLVMValueRef return_param = NULL;
	LLVMValueRef *values = NULL;
	ABIArgInfo *ret_info = signature->ret_abi_info;
	Type *return_type = signature->rtype->type->canonical;

	switch (ret_info->kind)
	{
		case ABI_ARG_INDIRECT:
			// Create the return parameter
			return_param = llvm_emit_alloca(context, llvm_get_type(context, return_type),
			                                ret_info->indirect.realignment, "sretparam");
			// Add the pointer to the list of arguments.
			vec_add(values, return_param);
			if (ret_info->indirect.realignment)
			{
				LLVMSetAlignment(return_param, ret_info->indirect.realignment);
			}
			break;
		case ABI_ARG_EXPAND:
			UNREACHABLE
		case ABI_ARG_DIRECT_PAIR:
		case ABI_ARG_IGNORE:
		case ABI_ARG_DIRECT_COERCE:
			break;
	}
	unsigned param_index = 0;
	unsigned arguments = vec_size(expr->call_expr.arguments);
	assert(arguments >= vec_size(signature->params));
	VECEACH(signature->params, i)
	{
		Expr *arg_expr = expr->call_expr.arguments[i];
		llvm_emit_expr(context, be_value, arg_expr);
		Decl *param = signature->params[i];
		ABIArgInfo *info = param->var.abi_info;
		switch (info->kind)
		{
			case ABI_ARG_IGNORE:
				// Skip.
				break;
			case ABI_ARG_INDIRECT:
			{
				// If we want we could optimize for structs by doing it by reference here.
				unsigned alignment = info->indirect.realignment ?: type_abi_alignment(param->type);
				LLVMValueRef indirect = llvm_emit_alloca(context,
				                                         llvm_get_type(context, param->type),
				                                         alignment,
				                                         "indirectarg");
				llvm_store_bevalue_aligned(context, indirect, be_value, alignment);
				vec_add(values, indirect);
				break;
			}
			case ABI_ARG_DIRECT_COERCE:
			{
				LLVMTypeRef coerce_type = llvm_get_coerce_type(context, info);
				if (!coerce_type || coerce_type == llvm_get_type(context, param->type))
				{
					vec_add(values, llvm_value_rvalue_store(context, be_value));
					break;
				}

				if (!abi_info_should_flatten(info))
				{
					vec_add(values, llvm_emit_coerce(context, coerce_type, be_value, param->type));
					break;
				}
				LLVMValueRef cast;
				unsigned target_alignment = llvm_abi_alignment(coerce_type);
				unsigned max_align = MAX(((unsigned)be_value->alignment), llvm_abi_alignment(coerce_type));

				// If we are loading something with greater alignment than what we have, we cannot directly memcpy.
				if (llvm_value_is_addr(be_value) && be_value->alignment < target_alignment)
				{
					// So load it instead.
					llvm_value_rvalue(context, be_value);
				}

				// In this case we have something nicely aligned, so we just do a cast.
				if (llvm_value_is_addr(be_value))
				{
					cast = LLVMBuildBitCast(context->builder, be_value->value, LLVMPointerType(coerce_type, 0), "");
				}
				else
				{
					cast = llvm_emit_alloca(context, coerce_type, max_align, "coerce");
					LLVMValueRef target = LLVMBuildBitCast(context->builder, cast, llvm_get_ptr_type(context, param->type), "");
					llvm_store_bevalue_aligned(context, target, be_value, max_align);
				}
				LLVMTypeRef element = llvm_abi_type(context, info->direct_coerce.type);
				for (unsigned idx = 0; idx < info->direct_coerce.elements; idx++)
				{
					LLVMValueRef element_ptr = LLVMBuildStructGEP2(context->builder, coerce_type, cast, idx, "");
					vec_add(values,
					        llvm_emit_load_aligned(context, element, element_ptr, llvm_abi_alignment(element), ""));
				}
				break;
			}
			case ABI_ARG_DIRECT_PAIR:
			{
				printf("TODO: Optimize load\n");
				LLVMValueRef value = llvm_value_rvalue_store(context, be_value);
				// Here we do the following transform:
				// struct -> { lo, hi } -> lo, hi
				LLVMTypeRef lo = llvm_abi_type(context, info->direct_pair.lo);
				LLVMTypeRef hi = llvm_abi_type(context, info->direct_pair.hi);
				LLVMTypeRef struct_type = llvm_get_coerce_type(context, info);
				unsigned max_align = MAX(llvm_abi_alignment(struct_type), type_abi_alignment(param->type));
				// Create the alloca holding the struct.
				LLVMValueRef temp = llvm_emit_alloca(context, struct_type, max_align, "temphold");
				// Bit cast to the original type type.
				LLVMValueRef cast = LLVMBuildBitCast(context->builder, temp, llvm_get_ptr_type(context, param->type), "casttemp");
				// Store the value.
				llvm_store_aligned(context, cast, value, max_align);
				// Get the lo value.
				LLVMValueRef lo_ptr = LLVMBuildStructGEP2(context->builder, struct_type, temp, 0, "lo");
				vec_add(values, llvm_emit_load_aligned(context, lo, lo_ptr, llvm_abi_alignment(lo), "lo"));
				// Get the hi value.
				LLVMValueRef hi_ptr = LLVMBuildStructGEP2(context->builder, struct_type, temp, 1, "hi");
				vec_add(values, llvm_emit_load_aligned(context, hi, hi_ptr, llvm_abi_alignment(hi), "hi"));
				break;
			}
			case ABI_ARG_EXPAND:
			{
				printf("TODO: Optimize load\n");
				LLVMValueRef value = llvm_value_rvalue_store(context, be_value);
				LLVMValueRef expand = llvm_emit_alloca_aligned(context, param->type, "expand");
				llvm_store_aligned(context, expand, value, type_abi_alignment(param->type));
				llvm_expand_type_to_args(context, param->type, expand, &values);
				// Expand the padding here.
				if (info->expand.padding_type)
				{
					vec_add(values, LLVMGetUndef(llvm_get_type(context, info->expand.padding_type)));
				}
				break;
			}
		}
	}
	for (unsigned i = vec_size(signature->params); i < arguments; i++)
	{
		Expr *arg_expr = expr->call_expr.arguments[i];
		llvm_emit_expr(context, be_value, arg_expr);
		printf("TODO: varargs should be expanded correctly\n");
		vec_add(values, llvm_value_rvalue_store(context, be_value));
	}

	LLVMValueRef call = LLVMBuildCall2(context->builder, func_type, func, values, vec_size(values), "");

	switch (ret_info->kind)
	{
		case ABI_ARG_EXPAND:
			UNREACHABLE
		case ABI_ARG_IGNORE:
			// Default behaviour is fine.
			break;
		case ABI_ARG_INDIRECT:
			// TODO look at failable
			call = llvm_emit_load_aligned(context,
			                              llvm_get_type(context, return_type),
			                              return_param,
			                              ret_info->indirect.realignment,
			                              "");
			break;
		case ABI_ARG_DIRECT_PAIR:
		{
			// TODO look at failable
			LLVMTypeRef lo = llvm_abi_type(context, ret_info->direct_pair.lo);
			LLVMTypeRef hi = llvm_abi_type(context, ret_info->direct_pair.hi);
			LLVMTypeRef struct_type = gencontext_get_twostruct(context, lo, hi);
			call = llvm_emit_convert_value_from_coerced(context, struct_type, call, return_type);
			break;
		}

		case ABI_ARG_DIRECT_COERCE:
		{
			// TODO look at failable
			LLVMTypeRef coerce = llvm_get_coerce_type(context, ret_info);
			// Default behaviour.
			if (!coerce || coerce == llvm_get_type(context, return_type)) break;
			assert(!abi_info_should_flatten(ret_info));
			call = llvm_emit_convert_value_from_coerced(context, coerce, call, return_type);
			break;
		}
	}

	if (signature->failable)
	{
		LLVMBasicBlockRef after_block = llvm_basic_block_new(context, "after_check");
		BEValue no_err;
		llvm_value_set_bool(&no_err, llvm_emit_is_no_error(context, call));
		if (context->error_var)
		{
			LLVMBasicBlockRef error_block = llvm_basic_block_new(context, "error");
			llvm_emit_cond_br(context, &no_err, after_block, error_block);
			llvm_emit_block(context, error_block);
			LLVMBuildStore(context->builder,
			               call,
			               llvm_emit_bitcast(context, context->error_var, type_get_ptr(type_error)));
			llvm_emit_br(context, context->catch_block);
		}
		else
		{
			llvm_emit_cond_br(context, &no_err, after_block, context->catch_block);
		}
		llvm_emit_block(context, after_block);
	}
	//gencontext_emit_throw_branch(context, call, signature->throws, expr->call_expr.throw_info, signature->error_return);

	/*
	if (function->func.function_signature.convention)
	{
		LLVMSetFunctionCallConv(call, LLVMX86StdcallCallConv);
	}*/
	llvm_value_set(be_value, call, expr->type);

}




static inline void gencontext_emit_expression_list_expr(GenContext *context, BEValue *be_value, Expr *expr)
{
	VECEACH(expr->expression_list, i)
	{
		llvm_emit_expr(context, be_value, expr->expression_list[i]);
	}
}


static inline void gencontext_emit_expr_block(GenContext *context, BEValue *be_value, Expr *expr)
{
	LLVMValueRef old_ret_out = context->return_out;
	LLVMBasicBlockRef saved_expr_block = context->expr_block_exit;

	LLVMBasicBlockRef expr_block = llvm_basic_block_new(context, "expr_block.exit");
	context->expr_block_exit = expr_block;

	LLVMValueRef return_out = NULL;
	if (expr->type != type_void)
	{
		return_out = llvm_emit_alloca_aligned(context, expr->type, "blockret");
	}
	context->return_out = return_out;

	Ast **stmts = expr->expr_block.stmts;
	VECEACH(stmts, i)
	{
		llvm_emit_stmt(context, stmts[i]);
	}
	llvm_emit_br(context, expr_block);

	// Emit the exit block.
	llvm_emit_block(context, expr_block);

	context->return_out = old_ret_out;
	context->expr_block_exit = saved_expr_block;

	if (return_out)
	{
		llvm_value_set_address(be_value, return_out, expr->type);
	}
	else
	{
		llvm_value_set(be_value, NULL, type_void);
	}
}

static inline void gencontext_emit_macro_block(GenContext *context, BEValue *be_value, Expr *expr)
{
	LLVMValueRef old_ret_out = context->return_out;
	LLVMBasicBlockRef saved_expr_block = context->expr_block_exit;

	LLVMBasicBlockRef expr_block = llvm_basic_block_new(context, "expr_block.exit");
	context->expr_block_exit = expr_block;

	LLVMValueRef return_out = NULL;
	if (expr->type != type_void)
	{
		return_out = llvm_emit_alloca_aligned(context, expr->type, "blockret");
	}
	context->return_out = return_out;

	Ast **stmts = expr->macro_block.stmts;
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
		decl->backend_ref = llvm_emit_decl_alloca(context, decl);
		BEValue value;
		llvm_emit_expr(context, &value, expr->macro_block.args[i]);
		printf("TODO: unoptimized use of BEValue\n");
		llvm_emit_store(context, decl, llvm_value_rvalue_store(context, &value));
	}

	VECEACH(stmts, i)
	{
		llvm_emit_stmt(context, stmts[i]);
	}
	llvm_emit_br(context, expr_block);

	// Emit the exit block.
	llvm_emit_block(context, expr_block);

	context->return_out = old_ret_out;
	context->expr_block_exit = saved_expr_block;

	if (return_out)
	{
		llvm_value_set_address(be_value, return_out, expr->type);
	}
	else
	{
		llvm_value_set(be_value, NULL, type_void);
	}
}

LLVMValueRef llvm_emit_call_intrinsic(GenContext *context, unsigned intrinsic_id, LLVMTypeRef *types, unsigned type_count,
                                      LLVMValueRef *values, unsigned arg_count)
{
	LLVMValueRef decl = LLVMGetIntrinsicDeclaration(context->module, intrinsic_id, types, type_count);
	LLVMTypeRef type = LLVMIntrinsicGetType(context->context, intrinsic_id, types, arg_count);
	return LLVMBuildCall2(context->builder, type, decl, values, arg_count, "");
}

LLVMValueRef llvm_emit_assign_expr(GenContext *c, LLVMValueRef ref, Expr *expr, LLVMValueRef failable)
{
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
	llvm_emit_expr(c, &value, expr);
	printf("TODO: // Optimize store \n");
	llvm_store_bevalue_aligned(c, ref, &value, 0);

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

	return value.value;
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

void llvm_emit_expr(GenContext *c, BEValue *value, Expr *expr)
{
	EMIT_LOC(c, expr);
NESTED_RETRY:
	switch (expr->expr_kind)
	{
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
		case EXPR_DESIGNATED_INITIALIZER:
			// Should only appear when generating designated initializers.
			UNREACHABLE
		case EXPR_SLICE_ASSIGN:
			gencontext_emit_slice_assign(c, value, expr);
			return;
		case EXPR_SLICE:
			gencontext_emit_slice(c, value, expr);
			return;
		case EXPR_LEN:
			gencontext_emit_len(c, value, expr);
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
			gencontext_emit_macro_block(c, value, expr);
			return;
		case EXPR_COMPOUND_LITERAL:
			expr = expr->expr_compound_literal.initializer;
			goto NESTED_RETRY;
		case EXPR_INITIALIZER_LIST:
			llvm_value_set_address(value,
			                       llvm_emit_initializer_list_expr_addr(c, expr, NULL),
			                       expr->type);
			return;
		case EXPR_EXPR_BLOCK:
			gencontext_emit_expr_block(c, value, expr);
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
			gencontext_emit_identifier(c, value, expr->identifier_expr.decl);
			return;
		case EXPR_SUBSCRIPT:
			gencontext_emit_subscript(c, value, expr);
			return;
		case EXPR_ACCESS:
			gencontext_emit_access_addr(c, value, expr);
			return;
		case EXPR_CALL:
			gencontext_emit_call_expr(c, value, expr);
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