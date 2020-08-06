// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_internal.h"
#include "compiler_internal.h"
#include "bigint.h"

static LLVMValueRef gencontext_emit_int_comparison(GenContext *context, Type *lhs_type, Type *rhs_type, LLVMValueRef lhs_value, LLVMValueRef rhs_value, BinaryOp binary_op);

static inline LLVMValueRef gencontext_emit_add_int(GenContext *context, Type *type, bool use_mod, LLVMValueRef left, LLVMValueRef right)
{
	if (use_mod)
	{
		return LLVMBuildAdd(context->builder, left, right, "add_mod");
	}

	if (build_options.debug_mode)
	{
		LLVMTypeRef type_to_use = llvm_type(type->canonical);
		LLVMValueRef args[2] = { left, right };
		assert(type->canonical == type);
		LLVMValueRef add_res;
		if (type_is_unsigned(type))
		{
			add_res = gencontext_emit_call_intrinsic(context, uadd_overflow_intrinsic_id, &type_to_use, 1, args, 2);
		}
		else
		{
			add_res = gencontext_emit_call_intrinsic(context, sadd_overflow_intrinsic_id, &type_to_use, 1, args, 2);
		}
		LLVMValueRef result = LLVMBuildExtractValue(context->builder, add_res, 0, "");
		LLVMValueRef ok = LLVMBuildExtractValue(context->builder, add_res, 1, "");
		gencontext_emit_panic_on_true(context, ok, "Addition overflow");
		return result;
	}
	return type_is_unsigned_integer(type)
		? LLVMBuildNUWAdd(context->builder, left, right, "uadd")
		: LLVMBuildNSWAdd(context->builder, left, right, "add");
}

static inline LLVMValueRef gencontext_emit_sub_int(GenContext *context, Type *type, bool use_mod, LLVMValueRef left, LLVMValueRef right)
{
	if (use_mod)
	{
		return LLVMBuildSub(context->builder, left, right, "sub_mod");
	}

	if (build_options.debug_mode)
	{
		LLVMTypeRef type_to_use = llvm_type(type);
		LLVMValueRef args[2] = { left, right };
		assert(type->canonical == type);
		LLVMValueRef add_res;
		if (type_is_unsigned(type))
		{
			add_res = gencontext_emit_call_intrinsic(context, usub_overflow_intrinsic_id, &type_to_use, 1, args, 2);
		}
		else
		{
			add_res = gencontext_emit_call_intrinsic(context, ssub_overflow_intrinsic_id, &type_to_use, 1, args, 2);
		}
		LLVMValueRef result = LLVMBuildExtractValue(context->builder, add_res, 0, "");
		LLVMValueRef ok = LLVMBuildExtractValue(context->builder, add_res, 1, "");
		gencontext_emit_panic_on_true(context, ok, "Subtraction overflow");
		return result;
	}


	return type_is_unsigned_integer(type)
	       ? LLVMBuildNUWSub(context->builder, left, right, "usub")
	       : LLVMBuildNSWSub(context->builder, left, right, "sub");
}

static inline LLVMValueRef gencontext_emit_subscript_addr_base(GenContext *context, Expr *parent)
{
	LLVMValueRef parent_value;
	Type *type = parent->type->canonical;
	switch (type->type_kind)
	{
		case TYPE_POINTER:
			return gencontext_emit_expr(context, parent);
		case TYPE_ARRAY:
			return gencontext_emit_address(context, parent);
		case TYPE_SUBARRAY:
		{
			// TODO insert trap on overflow.
			LLVMTypeRef subarray_type = llvm_type(type);
			parent_value = gencontext_emit_address(context, parent);
			LLVMValueRef pointer_addr = LLVMBuildStructGEP2(context->builder, subarray_type, parent_value, 0, "");
			LLVMTypeRef pointer_type = llvm_type(type_get_ptr(type->array.base));
			return LLVMBuildLoad2(context->builder, pointer_type, pointer_addr, "");
		}
		case TYPE_VARARRAY:
		case TYPE_STRING:
			TODO
		default:
			UNREACHABLE

	}
}

static inline LLVMValueRef gencontext_emit_subscript_addr_with_base(GenContext *context, Type *parent_type, LLVMValueRef parent_value, LLVMValueRef index_value)
{
	Type *type = parent_type;
	switch (type->type_kind)
	{
		case TYPE_POINTER:
			return LLVMBuildInBoundsGEP2(context->builder,
			                             llvm_type(type->pointer),
			                             parent_value, &index_value, 1, "ptridx");
		case TYPE_ARRAY:
		{
			// TODO insert trap on overflow.
			LLVMValueRef zero = llvm_int(type_int, 0);
			LLVMValueRef indices[2] = {
					zero,
					index_value,
			};
			return LLVMBuildInBoundsGEP2(context->builder,
			                             llvm_type(type),
			                             parent_value, indices, 2, "arridx");
		}
		case TYPE_SUBARRAY:
		{
			// TODO insert trap on overflow.
			return LLVMBuildInBoundsGEP2(context->builder,
			                             llvm_type(type->array.base),
			                             parent_value, &index_value, 1, "sarridx");
		}
		case TYPE_VARARRAY:
		case TYPE_STRING:
			TODO
		default:
			UNREACHABLE

	}
}
static inline LLVMValueRef gencontext_emit_subscript_addr(GenContext *context, Expr *parent, LLVMValueRef index_value)
{
	LLVMValueRef parent_value = gencontext_emit_subscript_addr_base(context, parent);
	return gencontext_emit_subscript_addr_with_base(context, parent->type->canonical, parent_value, index_value);
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
				value = LLVMBuildBitCast(context->builder, value, LLVMPointerType(llvm_type(found->type), 0), name);
				break;
			case TYPE_ERRTYPE:
				value = LLVMBuildStructGEP2(context->builder, llvm_type(parent->type), value, index + 1, name);
				break;
			case TYPE_STRUCT:
				value = LLVMBuildStructGEP2(context->builder, llvm_type(parent->type), value, index, name);
				break;
			default:
				UNREACHABLE
		}
		parent = found;
	} while (found != member);
	return value;
}


static inline LLVMValueRef gencontext_emit_access_addr(GenContext *context, Expr *expr)
{
	Expr *parent = expr->access_expr.parent;
	LLVMValueRef value = gencontext_emit_address(context, parent);
	Decl *member = expr->access_expr.ref;
	return gencontext_emit_member_addr(context, value, parent->type->canonical->decl, member);
}

LLVMValueRef gencontext_emit_scoped_expr(GenContext *context, Expr *expr)
{
	LLVMValueRef value = gencontext_emit_expr(context, expr->expr_scope.expr);
	gencontext_emit_defer(context, expr->expr_scope.defers.start, expr->expr_scope.defers.end);
	return value;
}

LLVMValueRef gencontext_emit_scoped_expr_address(GenContext *context, Expr *expr)
{
	LLVMValueRef value = gencontext_emit_address(context, expr->expr_scope.expr);
	gencontext_emit_defer(context, expr->expr_scope.defers.start, expr->expr_scope.defers.end);
	return value;
}

static inline LLVMValueRef gencontext_emit_initializer_list_expr_addr(GenContext *context, Expr *expr, LLVMValueRef optional_ref);

LLVMValueRef gencontext_emit_address(GenContext *context, Expr *expr)
{
	switch (expr->expr_kind)
	{
		case EXPR_DESIGNATED_INITIALIZER:
			// Should only appear when generating designated initializers.
			UNREACHABLE
		case EXPR_MACRO_BLOCK:
			TODO
		case EXPR_SLICE_ASSIGN:
		case EXPR_SLICE:
			// Should never be an lvalue
			UNREACHABLE
		case EXPR_IDENTIFIER:
			return decl_ref(expr->identifier_expr.decl);
		case EXPR_UNARY:
			assert(expr->unary_expr.operator == UNARYOP_DEREF);
			return gencontext_emit_expr(context, expr->unary_expr.expr);
		case EXPR_COMPOUND_LITERAL:
			return gencontext_emit_initializer_list_expr_addr(context, expr->expr_compound_literal.initializer, NULL);
		case EXPR_ACCESS:
			return gencontext_emit_access_addr(context, expr);
		case EXPR_SUBSCRIPT:
			return gencontext_emit_subscript_addr(context, expr->subscript_expr.expr, gencontext_emit_expr(context, expr->subscript_expr.index));
		case EXPR_SCOPED_EXPR:
			return gencontext_emit_scoped_expr_address(context, expr);
		case EXPR_GROUP:
			return gencontext_emit_address(context, expr->group_expr);
		case EXPR_CONST:
		case EXPR_TYPEID:
		case EXPR_POISONED:
		case EXPR_GUARD:
		case EXPR_BINARY:
		case EXPR_TERNARY:
		case EXPR_POST_UNARY:
		case EXPR_TYPE_ACCESS:
		case EXPR_CALL:
		case EXPR_INITIALIZER_LIST:
		case EXPR_EXPRESSION_LIST:
		case EXPR_CAST:
		case EXPR_TYPEOF:
		case EXPR_FAILABLE:
		case EXPR_CATCH:
		case EXPR_TRY:
		case EXPR_EXPR_BLOCK:
		case EXPR_DECL_LIST:
		case EXPR_ELSE:
		case EXPR_LEN:
			UNREACHABLE
	}
	UNREACHABLE
}

LLVMValueRef gencontext_emit_arr_to_subarray_cast(GenContext *context, LLVMValueRef value, Type *to_type, Type *from_type)
{
	size_t size = from_type->pointer->array.len;
	LLVMTypeRef subarray_type = llvm_type(to_type);
	LLVMValueRef result = LLVMGetUndef(subarray_type);
	value = gencontext_emit_bitcast(context, value, type_get_ptr(from_type->pointer->array.base));
	result = LLVMBuildInsertValue(context->builder, result, value, 0, "");
	return LLVMBuildInsertValue(context->builder, result, llvm_int(type_usize, size), 1, "");
}

LLVMValueRef gencontext_emit_subarray_to_ptr_cast(GenContext *context, LLVMValueRef value, Type *to_type, Type *from_type)
{
	return LLVMBuildExtractValue(context->builder, value, 0, "");
}

LLVMValueRef gencontext_emit_value_bitcast(GenContext *context, LLVMValueRef value, Type *to_type, Type *from_type)
{
	LLVMValueRef ptr = gencontext_emit_alloca(context, llvm_type(from_type), "");
	LLVMBuildStore(context->builder, value, ptr);
	LLVMValueRef ptr_cast = gencontext_emit_bitcast(context, ptr, type_get_ptr(to_type));
	return gencontext_emit_load(context, to_type, ptr_cast);
}
LLVMValueRef gencontext_emit_cast(GenContext *context, CastKind cast_kind, LLVMValueRef value, Type *to_type, Type *from_type)
{
	switch (cast_kind)
	{
		case CAST_XIERR:
			// TODO Insert zero check.
			return value;
		case CAST_ERROR:
			UNREACHABLE
		case CAST_PTRPTR:
			return LLVMBuildPointerCast(context->builder, value, llvm_type(to_type), "ptrptr");
		case CAST_PTRXI:
			return LLVMBuildPtrToInt(context->builder, value, llvm_type(to_type), "ptrxi");
		case CAST_APTSA:
			return gencontext_emit_arr_to_subarray_cast(context, value, to_type, from_type);
		case CAST_SAPTR:
			return gencontext_emit_subarray_to_ptr_cast(context, value, to_type, from_type);
		case CAST_VARRPTR:
			TODO
		case CAST_ARRPTR:
			TODO
		case CAST_STRPTR:
			TODO
		case CAST_EREU:
		case CAST_EUER:
			return gencontext_emit_value_bitcast(context, value, to_type, from_type);
		case CAST_EUBOOL:
			return LLVMBuildICmp(context->builder, LLVMIntNE, value, gencontext_emit_no_error_union(context), "eubool");
		case CAST_PTRBOOL:
			return LLVMBuildIsNotNull(context->builder, value, "ptrbool");
		case CAST_BOOLINT:
			return LLVMBuildTrunc(context->builder, value, llvm_type(to_type), "boolsi");
		case CAST_FPBOOL:
			return LLVMBuildFCmp(context->builder, LLVMRealUNE, value, LLVMConstNull(LLVMTypeOf(value)), "fpbool");
		case CAST_BOOLFP:
			return LLVMBuildSIToFP(context->builder, value, llvm_type(to_type), "boolfp");
		case CAST_INTBOOL:
			return LLVMBuildICmp(context->builder, LLVMIntNE, value, LLVMConstNull(LLVMTypeOf(value)), "intbool");
		case CAST_FPFP:
			return type_convert_will_trunc(to_type, from_type)
			       ? LLVMBuildFPTrunc(context->builder, value, llvm_type(to_type), "fpfptrunc")
			       : LLVMBuildFPExt(context->builder, value, llvm_type(to_type), "fpfpext");
		case CAST_FPSI:
			return LLVMBuildFPToSI(context->builder, value, llvm_type(to_type), "fpsi");
		case CAST_FPUI:
			return LLVMBuildFPToUI(context->builder, value, llvm_type(to_type), "fpui");
		case CAST_SISI:
			return type_convert_will_trunc(to_type, from_type)
			       ? LLVMBuildTrunc(context->builder, value, llvm_type(to_type), "sisitrunc")
			       : LLVMBuildSExt(context->builder, value, llvm_type(to_type), "sisiext");
		case CAST_SIUI:
			return type_convert_will_trunc(to_type, from_type)
			       ? LLVMBuildTrunc(context->builder, value, llvm_type(to_type), "siuitrunc")
			       : LLVMBuildZExt(context->builder, value, llvm_type(to_type), "siuiext");
		case CAST_SIFP:
			return LLVMBuildSIToFP(context->builder, value, llvm_type(to_type), "sifp");
		case CAST_XIPTR:
			return LLVMBuildIntToPtr(context->builder, value, llvm_type(to_type), "xiptr");
		case CAST_UISI:
			return type_convert_will_trunc(to_type, from_type)
			       ? LLVMBuildTrunc(context->builder, value, llvm_type(to_type), "uisitrunc")
			       : LLVMBuildZExt(context->builder, value, llvm_type(to_type), "uisiext");
		case CAST_UIUI:
			return type_convert_will_trunc(to_type, from_type)
			       ? LLVMBuildTrunc(context->builder, value, llvm_type(to_type), "uiuitrunc")
			       : LLVMBuildZExt(context->builder, value, llvm_type(to_type), "uiuiext");
		case CAST_UIFP:
			return LLVMBuildUIToFP(context->builder, value, llvm_type(to_type), "uifp");
		case CAST_ENUMLOW:
			return value;
	}
	UNREACHABLE
}
static inline LLVMValueRef gencontext_emit_cast_expr(GenContext *context, Expr *expr)
{
	LLVMValueRef rhs = gencontext_emit_expr(context, expr->cast_expr.expr);
	return gencontext_emit_cast(context, expr->cast_expr.kind, rhs, expr->type->canonical, expr->cast_expr.expr->type->canonical);
}

static LLVMValueRef gencontext_emit_initializer_list_expr_const(GenContext *context, Expr *expr);

static LLVMValueRef gencontext_construct_const_value(GenContext *context, Expr *expr)
{
	NESTED_RETRY:
	if (expr->expr_kind == EXPR_COMPOUND_LITERAL)
	{
		expr = expr->expr_compound_literal.initializer;
		goto NESTED_RETRY;
	}
	if (expr->expr_kind == EXPR_INITIALIZER_LIST)
	{
		return gencontext_emit_initializer_list_expr_const(context, expr);
	}
	return gencontext_emit_expr(context, expr);
}

static LLVMValueRef gencontext_construct_const_union_struct(GenContext *context, Type *canonical, Expr *value)
{
	LLVMValueRef values[2];
	values[0] = gencontext_construct_const_value(context, value);
	unsigned size_diff = type_size(canonical) - type_size(value->type);
	if (size_diff > 0)
	{
		LLVMTypeRef size = LLVMArrayType(llvm_type(type_char), size_diff);
		values[1] = LLVMConstNull(size);
	}
	return LLVMConstStructInContext(context->context, values, size_diff > 0 ? 2 : 1, false);
}
static LLVMValueRef gencontext_recursive_set_const_value(GenContext *context, DesignatedPath *path, LLVMValueRef parent, Type *parent_type, Expr *value)
{
	switch (path->kind)
	{
		case DESIGNATED_IDENT:
			if (!path->sub_path)
			{
				if (parent_type->canonical->type_kind == TYPE_UNION)
				{
					return gencontext_construct_const_union_struct(context, parent_type, value);
				}
				return LLVMConstInsertValue(parent, gencontext_construct_const_value(context, value), &path->index, 1);
			}
			else
			{
				parent_type = path->type;
				return LLVMConstInsertValue(parent,
				                            gencontext_recursive_set_const_value(context,
				                                                                 path->sub_path,
				                                                                 LLVMConstExtractValue(parent,
				                                                                                       &path->index,
				                                                                                       1),
				                                                                 parent_type,
				                                                                 value), &path->index, 1);
			}
		case DESIGNATED_SUBSCRIPT:
		{
			// TODO range, more arrays
			assert(path->index_expr->expr_kind == EXPR_CONST);
			unsigned int index = (unsigned int)bigint_as_unsigned(&path->index_expr->const_expr.i);
			if (!path->sub_path)
			{
				LLVMValueRef res = gencontext_construct_const_value(context, value);
				return LLVMConstInsertValue(parent, res, &index, 1);
			}
			parent_type = path->type;
			return LLVMConstInsertValue(parent,
			                            gencontext_recursive_set_const_value(context,
			                                                                 path->sub_path,
			                                                                 LLVMConstExtractValue(parent, &index, 1),
			                                                                 parent_type,
			                                                                 value), &index, 1);
		}
		default:
			UNREACHABLE;

	}

}

static LLVMValueRef gencontext_emit_initializer_list_expr_const(GenContext *context, Expr *expr)
{
	Type *canonical = expr->type->canonical;
	LLVMTypeRef type = llvm_type(canonical);

	if (expr->expr_initializer.init_type == INITIALIZER_ZERO)
	{
		return LLVMConstNull(type);
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
		return gencontext_construct_const_union_struct(context, canonical, elements[0]);
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
			value = LLVMConstInsertValue(value, gencontext_emit_expr(context, element), &i, 1);
		}
		return value;
	}

	LLVMValueRef value = LLVMConstNull(type);
	VECEACH(elements, i)
	{
		Expr *element = elements[i];
		DesignatedPath *path = element->designated_init_expr.path;
		Type *parent_type = expr->type->canonical;
		value = gencontext_recursive_set_const_value(context,
		                                             path,
		                                             value,
		                                             parent_type,
		                                             element->designated_init_expr.value);
	}
	return value;
}

/**
 * Emit a Foo { .... } literal.
 *
 * Improve: Direct assign in the case where this is assigning to a variable.
 * Improve: Create constant initializer for the constant case and do a memcopy
 */
static inline LLVMValueRef gencontext_emit_initializer_list_expr_addr(GenContext *context, Expr *expr, LLVMValueRef optional_ref)
{
	if (expr->constant && type_size(expr->type) <= type_size(type_voidptr) * 4)
	{
		LLVMTypeRef type = llvm_type(expr->type);
		LLVMValueRef val = gencontext_emit_initializer_list_expr_const(context, expr);
		LLVMValueRef ref = LLVMAddGlobal(context->module, type, "");
		LLVMSetInitializer(ref, val);
		LLVMSetGlobalConstant(ref, true);
		if (optional_ref)
		{
			LLVMBuildMemCpy(context->builder, optional_ref, LLVMGetAlignment(optional_ref), ref, LLVMGetAlignment(ref), LLVMSizeOf(type));
		}
		return ref;
	}
	LLVMTypeRef type = llvm_type(expr->type);
	LLVMValueRef ref = optional_ref ?: gencontext_emit_alloca(context, type, "literal");

	Type *canonical = expr->type->canonical;
	if (expr->expr_initializer.init_type != INITIALIZER_NORMAL)
	{
		gencontext_emit_memclear(context, ref, canonical);
	}

	bool is_error = expr->type->canonical->type_kind == TYPE_ERRTYPE;

	if (is_error)
	{
		LLVMValueRef err_type = LLVMBuildStructGEP2(context->builder, type, ref, 0, "");
		LLVMBuildStore(context->builder, expr->type->canonical->backend_typeid, err_type);
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
			LLVMValueRef init_value = gencontext_emit_expr(context, elements[0]);
			LLVMValueRef u = LLVMBuildBitCast(context->builder, ref, LLVMPointerType(llvm_type(elements[0]->type->canonical), 0), "");
			LLVMBuildStore(context->builder, init_value, u);
			return ref;
		}
		VECEACH(elements, i)
		{
			Expr *element = elements[i];
			LLVMValueRef init_value = gencontext_emit_expr(context, element);
			LLVMValueRef subref = LLVMBuildStructGEP2(context->builder, type, ref, i + (int)is_error, "");
			LLVMBuildStore(context->builder, init_value, subref);
		}
		return ref;
	}


	VECEACH(elements, i)
	{
		if (is_error) TODO
		Expr *element = elements[i];
		DesignatedPath *path = element->designated_init_expr.path;
		LLVMValueRef sub_value = gencontext_emit_expr(context, element->designated_init_expr.value);
		LLVMValueRef sub_ref = ref;
		Type *parent_type = expr->type->canonical;
		while (path)
		{
			switch (path->kind)
			{
				case DESIGNATED_IDENT:
					if (parent_type->canonical->type_kind == TYPE_UNION)
					{
						sub_ref = LLVMBuildBitCast(context->builder, sub_ref, LLVMPointerType(llvm_type(path->type), 0), path->type->name);
					}
					else
					{
						sub_ref = LLVMBuildStructGEP2(context->builder, llvm_type(parent_type), sub_ref, path->index, path->type->name);
					}
					break;
				case DESIGNATED_SUBSCRIPT:
				{
					// TODO range, more arrays
					LLVMValueRef zero = llvm_int(type_int, 0);
					LLVMValueRef index = gencontext_emit_expr(context, path->index_expr);
					LLVMValueRef indices[2] = {
							zero,
							index,
					};
					sub_ref = LLVMBuildInBoundsGEP2(context->builder,
					                             llvm_type(parent_type),
					                             sub_ref, indices, 2, "arrsub");
					break;
				}
				default:
					UNREACHABLE;

			}
			parent_type = path->type;
			path = path->sub_path;
		}
		LLVMBuildStore(context->builder, sub_value, sub_ref);
	}
	return ref;
}

static inline LLVMValueRef gencontext_emit_inc_dec_change(GenContext *context, bool use_mod, LLVMValueRef current_value, Expr *expr, int diff)
{
	Type *type = type_reduced_from_expr(expr);


	if (type->type_kind == TYPE_POINTER)
	{
		LLVMValueRef add = LLVMConstInt(diff < 0 ? llvm_type(type_isize) : llvm_type(type_usize), diff, diff < 0);
		return LLVMBuildGEP2(context->builder, llvm_type(type->pointer), current_value, &add, 1, "ptrincdec");
	}
	LLVMTypeRef llvm_type = llvm_type(type);

	if (type_is_float(type))
	{
		LLVMValueRef add = LLVMConstReal(llvm_type, (double)diff);
		return LLVMBuildFAdd(context->builder, current_value, add, "fincdec");
	}

	LLVMValueRef diff_value = LLVMConstInt(llvm_type, 1, false);
	return diff > 0
		? gencontext_emit_add_int(context, type, use_mod, current_value, diff_value)
		: gencontext_emit_sub_int(context, type, use_mod, current_value, diff_value);
}

static inline LLVMValueRef gencontext_emit_pre_inc_dec(GenContext *context, Expr *expr, int diff, bool use_mod)
{
	LLVMValueRef addr = gencontext_emit_address(context, expr);
	LLVMValueRef value = gencontext_emit_load(context, expr->type, addr);
	LLVMValueRef result = gencontext_emit_inc_dec_change(context, use_mod, value, expr, diff);
	LLVMBuildStore(context->builder, result, addr);
	return result;
}

static inline LLVMValueRef gencontext_emit_post_inc_dec(GenContext *context, Expr *expr, int diff, bool use_mod)
{
	LLVMValueRef addr = gencontext_emit_address(context, expr);
	LLVMValueRef value = gencontext_emit_load(context, expr->type, addr);
	LLVMValueRef result = gencontext_emit_inc_dec_change(context, use_mod, value, expr, diff);
	LLVMBuildStore(context->builder, result, addr);
	return value;
}

LLVMValueRef gencontext_emit_unary_expr(GenContext *context, Expr *expr)
{
	Type *type = type_reduced_from_expr(expr->unary_expr.expr);
	switch (expr->unary_expr.operator)
	{
		case UNARYOP_ERROR:
			FATAL_ERROR("Illegal unary op %s", expr->unary_expr.operator);
		case UNARYOP_NOT:
			if (type_is_float(type))
			{
				LLVMValueRef val = gencontext_emit_expr(context, expr->unary_expr.expr);
				return LLVMBuildFCmp(context->builder, LLVMRealUNE, val, LLVMConstNull(llvm_type(type)), "");
			}
			else
			{
				LLVMValueRef val = gencontext_emit_expr(context, expr->unary_expr.expr);
				return LLVMBuildICmp(context->builder, LLVMIntEQ, val, LLVMConstNull(llvm_type(type)), "");
			}
		case UNARYOP_BITNEG:
			return LLVMBuildNot(context->builder, gencontext_emit_expr(context, expr->unary_expr.expr), "bnot");
		case UNARYOP_NEGMOD:
			return LLVMBuildNeg(context->builder, gencontext_emit_expr(context, expr->unary_expr.expr), "negmod");
		case UNARYOP_NEG:
			if (type_is_float(type))
			{
				return LLVMBuildFNeg(context->builder, gencontext_emit_expr(context, expr->unary_expr.expr), "fneg");
			}
			assert(!type_is_unsigned(type));
			{
				LLVMValueRef to_negate = gencontext_emit_expr(context, expr->unary_expr.expr);
				LLVMValueRef zero = llvm_int(expr->unary_expr.expr->type, 0);
				if (build_options.debug_mode)
				{
					LLVMTypeRef type_to_use = llvm_type(type->canonical);
					LLVMValueRef args[2] = { zero, to_negate };
					LLVMValueRef call_res = gencontext_emit_call_intrinsic(context,
					                                                       ssub_overflow_intrinsic_id,
					                                                       &type_to_use, 1, args, 2);
					LLVMValueRef result = LLVMBuildExtractValue(context->builder, call_res, 0, "");
					LLVMValueRef ok = LLVMBuildExtractValue(context->builder, call_res, 1, "");
					gencontext_emit_panic_on_true(context, ok, "Signed negation overflow");
					return result;
				}
				return LLVMBuildNSWSub(context->builder, zero, to_negate, "neg");
			}
		case UNARYOP_ADDR:
			return gencontext_emit_address(context, expr->unary_expr.expr);
		case UNARYOP_DEREF:
			// TODO check on deref null
			return gencontext_emit_load(context, expr->type, gencontext_emit_expr(context, expr->unary_expr.expr));
		case UNARYOP_INC:
			return gencontext_emit_pre_inc_dec(context, expr->unary_expr.expr, 1, false);
		case UNARYOP_DEC:
			return gencontext_emit_pre_inc_dec(context, expr->unary_expr.expr, -1, false);
	}
	UNREACHABLE
}

static LLVMValueRef gencontext_emit_len_value(GenContext *context, Expr *inner, LLVMValueRef inner_value)
{
	Type *type = inner->type;
	switch (type->canonical->type_kind)
	{
		case TYPE_SUBARRAY:
		{
			LLVMTypeRef subarray_type = llvm_type(type);
			LLVMValueRef len_addr = LLVMBuildStructGEP2(context->builder, subarray_type, inner_value, 1, "len");
			return gencontext_emit_load(context, type_usize, len_addr);
		}
		case TYPE_ARRAY:
			return gencontext_emit_const_int(context, type_usize, type->array.len);
		default:
			UNREACHABLE
	}

}

static LLVMValueRef gencontext_emit_len(GenContext *context, Expr *expr)
{
	Expr *inner = expr->len_expr.inner;
	LLVMValueRef value = gencontext_emit_address(context, inner);
	return gencontext_emit_len_value(context, inner, value);
}

static void gencontext_emit_trap_negative(GenContext *context, Expr *expr, LLVMValueRef value, const char *error)
{
	if (!build_options.debug_mode) return;
	if (type_is_unsigned_integer(expr->type->canonical)) return;

	LLVMValueRef zero = gencontext_emit_const_int(context, expr->type, 0);
	LLVMValueRef ok = LLVMBuildICmp(context->builder, LLVMIntSLT, value, zero, "underflow");
	gencontext_emit_panic_on_true(context, ok, error);
}

static void
gencontext_emit_slice_values(GenContext *context, Expr *slice, Type **parent_type_ref, LLVMValueRef *parent_base_ref,
                             Type **start_type_ref, LLVMValueRef *start_index_ref, Type **end_type_ref,
                             LLVMValueRef *end_index_ref)
{
	assert(slice->expr_kind == EXPR_SLICE);

	Expr *parent_expr = slice->slice_expr.expr;
	Type *parent_type = parent_expr->type->canonical;
	LLVMValueRef parent_addr = gencontext_emit_address(context, parent_expr);
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
	LLVMValueRef start_index = gencontext_emit_expr(context, start);

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
				len = gencontext_emit_const_int(context, type_usize, parent_type->array.len);
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
		start_index = gencontext_emit_sub_int(context, start_type, false, len, start_index);
	}

	// Check that index does not extend beyond the length.
	if (parent_type->type_kind != TYPE_POINTER && build_options.debug_mode)
	{
		LLVMValueRef exceeds_size = gencontext_emit_int_comparison(context, type_usize, start_type, len, start_index, BINARYOP_GE);
		gencontext_emit_panic_on_true(context, exceeds_size, "Index exceeds array length.");
	}

	// Insert trap for negative start offset for non pointers.
	if (parent_type->type_kind != TYPE_POINTER)
	{
		gencontext_emit_trap_negative(context, start, start_index, "Negative index");
	}

	Type *end_type;
	LLVMValueRef end_index;

	if (end)
	{
		// Get the index.
		end_index = gencontext_emit_expr(context, end);
		end_type = end->type->canonical;

		// Reverse if it is "from back"
		if (slice->slice_expr.end_from_back)
		{
			end_index = gencontext_emit_sub_int(context, end_type, false, len, end_index);
		}

		// This will trap any bad negative index, so we're fine.
		if (build_options.debug_mode)
		{
			LLVMValueRef excess = gencontext_emit_int_comparison(context, start_type, end_type, start_index, *end_index_ref, BINARYOP_GT);
			gencontext_emit_panic_on_true(context, excess, "Negative size");

			if (len)
			{
				LLVMValueRef exceeds_size = gencontext_emit_int_comparison(context, type_usize, end_type, len, end_index, BINARYOP_LT);
				gencontext_emit_panic_on_true(context, exceeds_size, "Size exceeds index");
			}
		}
	}
	else
	{
		assert(len && "Pointer should never end up here.");
		// Otherwise everything is fine and dandy. Our len is our end index.
		end_index = len;
		end_type = type_usize;
	}

	*end_index_ref = end_index;
	*end_type_ref = end_type;
	*start_index_ref = start_index;
	*start_type_ref = start_type;
	*parent_base_ref = parent_base;
	*parent_type_ref = parent_type;
}

static LLVMValueRef gencontext_emit_slice(GenContext *context, Expr *expr)
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
			parent_base = gencontext_emit_bitcast(context, parent_base, pointer_type);
			// Move pointer
			start_pointer = LLVMBuildInBoundsGEP2(context->builder, llvm_type(pointer_type->pointer), parent_base, &start_index, 1, "offset");
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
	LLVMValueRef result = LLVMGetUndef(llvm_type(expr->type));
	result = LLVMBuildInsertValue(context->builder, result, start_pointer, 0, "");
	return LLVMBuildInsertValue(context->builder, result, size, 1, "");

}

static LLVMValueRef gencontext_emit_slice_assign(GenContext *context, Expr *expr)
{
	// We will be replacing the slice assign with code that roughly looks like this:
	// size_t end = slice_end;
	// size_t slice_current = slice_start;
	// while (slice_current < end) pointer[slice_current++] = value;

	// First, find the value assigned.
	Expr *assigned_value = expr->slice_assign_expr.right;
	LLVMValueRef value = gencontext_emit_expr(context, assigned_value);

	Type *parent_type;
	Type *end_type;
	LLVMValueRef end_index;
	LLVMValueRef parent_base;
	Type *start_type;
	LLVMValueRef start_index;
	// Use general function to get all the values we need (a lot!)
	gencontext_emit_slice_values(context, expr->slice_assign_expr.left, &parent_type,
	                             &parent_base,
	                             &start_type, &start_index, &end_type, &end_index);

	// We will need to iterate for the general case.
	LLVMBasicBlockRef start_block = context->current_block;
	LLVMBasicBlockRef cond_block = gencontext_create_free_block(context, "cond");
	LLVMBasicBlockRef exit_block = gencontext_create_free_block(context, "exit");
	LLVMBasicBlockRef assign_block = gencontext_create_free_block(context, "assign");

	// First jump to the cond block.
	gencontext_emit_br(context, cond_block);
	gencontext_emit_block(context, cond_block);

	// We emit a phi here: value is either the start value (start_offset) or the next value (next_offset)
	// but we haven't generated the latter yet, so we defer that.
	LLVMValueRef offset = LLVMBuildPhi(context->builder, llvm_type(start_type), "");

	// Check if we're not at the end.
	LLVMValueRef not_at_end = gencontext_emit_int_comparison(context, start_type, end_type, offset, end_index, BINARYOP_LT);

	// If jump to the assign block if we're not at the end index.
	gencontext_emit_cond_br(context, not_at_end, assign_block, exit_block);

	// Emit the assign.
	gencontext_emit_block(context, assign_block);
	// Reuse this calculation
	LLVMValueRef target = gencontext_emit_subscript_addr_with_base(context, parent_type, parent_base, offset);
	// And store the value.
	LLVMBuildStore(context->builder, value, target);

	// Create the new offset
	LLVMValueRef next_offset = gencontext_emit_add_int(context, start_type, false, offset, gencontext_emit_const_int(context, start_type, 1));

	// And jump back
	gencontext_emit_br(context, cond_block);

	// Finally set up our phi
	LLVMValueRef logic_values[2] = { start_index, next_offset };
	LLVMBasicBlockRef blocks[2] = { start_block, assign_block };
	LLVMAddIncoming(offset, logic_values, blocks, 2);

	// And emit the exit block.
	gencontext_emit_block(context, exit_block);

	return value;
}

static LLVMValueRef gencontext_emit_logical_and_or(GenContext *context, Expr *expr, BinaryOp op)
{
	// Value *ScalarExprEmitter::VisitBinLAnd(const BinaryOperator *E)
	// For vector implementation.

	// Set up basic blocks, following Cone
	LLVMBasicBlockRef start_block = LLVMGetInsertBlock(context->builder);
	LLVMBasicBlockRef phi_block = LLVMCreateBasicBlockInContext(context->context, op == BINARYOP_AND ? "and.phi" : "or.phi");
	LLVMBasicBlockRef rhs_block = LLVMCreateBasicBlockInContext(context->context, op == BINARYOP_AND ? "and.rhs" : "or.rhs");

	// Generate left-hand condition and conditional branch
	LLVMValueRef lhs = gencontext_emit_expr(context, expr->binary_expr.left);

	if (op == BINARYOP_AND)
	{
		gencontext_emit_cond_br(context, lhs, rhs_block, phi_block);
	}
	else
	{
		gencontext_emit_cond_br(context, lhs, phi_block, rhs_block);
	}

	gencontext_emit_block(context, rhs_block);
	LLVMValueRef rhs = gencontext_emit_expr(context, expr->binary_expr.right);
	LLVMBasicBlockRef end_block = context->current_block;
	gencontext_emit_br(context, phi_block);

	// Generate phi
	gencontext_emit_block(context, phi_block);

	// Simplify for LLVM by entering the constants we already know of.
	LLVMValueRef result_on_skip = llvm_int(type_bool, op == BINARYOP_AND ? 0 : 1);

	// One possibility here is that a return happens inside of the expression.
	if (!end_block)
	{
		return result_on_skip;
	}
	LLVMValueRef phi = LLVMBuildPhi(context->builder, llvm_type(type_bool), "val");
	LLVMValueRef logic_values[2] = { result_on_skip, rhs };
	LLVMBasicBlockRef blocks[2] = { start_block, end_block };
	LLVMAddIncoming(phi, logic_values, blocks, 2);

	return phi;
}



static LLVMValueRef gencontext_emit_int_comparison(GenContext *context, Type *lhs_type, Type *rhs_type, LLVMValueRef lhs_value, LLVMValueRef rhs_value, BinaryOp binary_op)
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
				return LLVMBuildICmp(context->builder, LLVMIntEQ, lhs_value, rhs_value, "eq");
			case BINARYOP_NE:
				return LLVMBuildICmp(context->builder, LLVMIntNE, lhs_value, rhs_value, "neq");
			case BINARYOP_GE:
				return LLVMBuildICmp(context->builder, LLVMIntUGE, lhs_value, rhs_value, "ge");
			case BINARYOP_GT:
				return LLVMBuildICmp(context->builder, LLVMIntUGT, lhs_value, rhs_value, "gt");
			case BINARYOP_LE:
				return LLVMBuildICmp(context->builder, LLVMIntULE, lhs_value, rhs_value, "le");
			case BINARYOP_LT:
				return LLVMBuildICmp(context->builder, LLVMIntULT, lhs_value, rhs_value, "lt");
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
			comp_value = LLVMBuildICmp(context->builder, LLVMIntEQ, lhs_value, rhs_value, "eq");
			break;
		case BINARYOP_NE:
			comp_value = LLVMBuildICmp(context->builder, LLVMIntNE, lhs_value, rhs_value, "neq");
			break;
		case BINARYOP_GE:
			comp_value = LLVMBuildICmp(context->builder, LLVMIntSGE, lhs_value, rhs_value, "ge");
			break;
		case BINARYOP_GT:
			comp_value = LLVMBuildICmp(context->builder, LLVMIntSGT, lhs_value, rhs_value, "gt");
			break;
		case BINARYOP_LE:
			comp_value = LLVMBuildICmp(context->builder, LLVMIntSLE, lhs_value, rhs_value, "le");
			break;
		case BINARYOP_LT:
			comp_value = LLVMBuildICmp(context->builder, LLVMIntSLT, lhs_value, rhs_value, "lt");
			break;
		default:
			UNREACHABLE
	}

	// If right side is also signed then this is fine.
	if (rhs_signed) return comp_value;

	// Otherwise, special handling for left side signed, right side unsigned.
	LLVMValueRef zero = llvm_int(lhs_type, 0);
	switch (binary_op)
	{
		case BINARYOP_EQ:
			// Only true if lhs >= 0
			check_value = LLVMBuildICmp(context->builder, LLVMIntSGE, lhs_value, zero, "check");
			return LLVMBuildAnd(context->builder, check_value, comp_value, "siui-eq");
		case BINARYOP_NE:
			// Always true if lhs < 0
			check_value = LLVMBuildICmp(context->builder, LLVMIntSLT, lhs_value, zero, "check");
			return LLVMBuildOr(context->builder, check_value, comp_value, "siui-ne");
		case BINARYOP_GE:
			// Only true if rhs >= 0 when regarded as a signed integer
			check_value = LLVMBuildICmp(context->builder, LLVMIntSGE, rhs_value, zero, "check");
			return LLVMBuildAnd(context->builder, check_value, comp_value, "siui-ge");
		case BINARYOP_GT:
			// Only true if rhs >= 0 when regarded as a signed integer
			check_value = LLVMBuildICmp(context->builder, LLVMIntSGE, rhs_value, zero, "check");
			return LLVMBuildAnd(context->builder, check_value, comp_value, "siui-gt");
		case BINARYOP_LE:
			// Always true if rhs < 0 when regarded as a signed integer
			check_value = LLVMBuildICmp(context->builder, LLVMIntSLT, rhs_value, zero, "check");
			return LLVMBuildOr(context->builder, check_value, comp_value, "siui-le");
		case BINARYOP_LT:
			// Always true if rhs < 0 when regarded as a signed integer
			check_value = LLVMBuildICmp(context->builder, LLVMIntSLT, rhs_value, zero, "check");
			return LLVMBuildOr(context->builder, check_value, comp_value, "siui-lt");
		default:
			UNREACHABLE
	}

}

static LLVMValueRef gencontext_emit_binary(GenContext *context, Expr *expr, LLVMValueRef lhs_addr, BinaryOp binary_op)
{

	if (binary_op == BINARYOP_AND || binary_op == BINARYOP_OR)
	{
		return gencontext_emit_logical_and_or(context, expr, binary_op);
	}
	Expr *lhs = expr->binary_expr.left;
	Expr *rhs = expr->binary_expr.right;

	LLVMValueRef lhs_value;
	LLVMValueRef rhs_value;
	if (lhs_addr)
	{
		lhs_value = gencontext_emit_load(context, lhs->type, lhs_addr);
	}
	else
	{
		lhs_value = gencontext_emit_expr(context, lhs);
	}

	rhs_value = gencontext_emit_expr(context, rhs);
	Type *lhs_type = type_reduced_from_expr(lhs);
	if (type_is_integer(lhs_type) && binary_op >= BINARYOP_GT && binary_op <= BINARYOP_EQ)
	{
		return gencontext_emit_int_comparison(context, lhs_type, type_reduced_from_expr(rhs), lhs_value, rhs_value, binary_op);
	}
	bool is_float = type_is_float(lhs_type);
	switch (binary_op)
	{
		case BINARYOP_ERROR:
			UNREACHABLE
		case BINARYOP_MULT:
			if (is_float) return LLVMBuildFMul(context->builder, lhs_value, rhs_value, "fmul");
			if (type_is_unsigned_integer(lhs_type))
			{
				if (build_options.debug_mode)
				{
					LLVMTypeRef type_to_use = llvm_type(lhs_type);
					LLVMValueRef args[2] = { lhs_value, rhs_value };
					LLVMTypeRef types[2] = { type_to_use, type_to_use };
					LLVMValueRef call_res = gencontext_emit_call_intrinsic(context,
					                                                       umul_overflow_intrinsic_id,
					                                                       types,
					                                                       1,
					                                                       args,
					                                                       2);
					LLVMValueRef result = LLVMBuildExtractValue(context->builder, call_res, 0, "");
					LLVMValueRef ok = LLVMBuildExtractValue(context->builder, call_res, 1, "");
					gencontext_emit_panic_on_true(context, ok, "Unsigned multiplication overflow");
					return result;
				}
				return LLVMBuildNUWMul(context->builder, lhs_value, rhs_value, "umul");
			}
			if (build_options.debug_mode)
			{
				LLVMTypeRef type_to_use = llvm_type(lhs_type);
				LLVMValueRef args[2] = { lhs_value, rhs_value };
				LLVMTypeRef types[2] = { type_to_use, type_to_use };
				LLVMValueRef call_res = gencontext_emit_call_intrinsic(context,
				                                                       smul_overflow_intrinsic_id,
				                                                       types,
				                                                       1,
				                                                       args,
				                                                       2);
				LLVMValueRef result = LLVMBuildExtractValue(context->builder, call_res, 0, "");
				LLVMValueRef ok = LLVMBuildExtractValue(context->builder, call_res, 1, "");
				gencontext_emit_panic_on_true(context, ok, "Signed multiplication overflow");
				return result;
			}
			return LLVMBuildNSWMul(context->builder, lhs_value, rhs_value, "mul");
		case BINARYOP_MULT_MOD:
			return LLVMBuildMul(context->builder, lhs_value, rhs_value, "mul");
		case BINARYOP_SUB:
		case BINARYOP_SUB_MOD:
			if (lhs_type->type_kind == TYPE_POINTER)
			{
				if (lhs->type->canonical == rhs->type->canonical) return LLVMBuildPtrDiff(context->builder, lhs_value, rhs_value, "ptrdiff");
				rhs_value = LLVMBuildNeg(context->builder, rhs_value, "");
				return LLVMBuildGEP2(context->builder, llvm_type(lhs_type->canonical->pointer), lhs_value, &rhs_value, 1, "ptrsub");
			}
			if (is_float) return LLVMBuildFSub(context->builder, lhs_value, rhs_value, "fsub");
			return gencontext_emit_sub_int(context, lhs->type->canonical, binary_op == BINARYOP_SUB_MOD, lhs_value, rhs_value);
		case BINARYOP_ADD:
		case BINARYOP_ADD_MOD:
			if (lhs_type->type_kind == TYPE_POINTER)
			{
				assert(type_is_integer(rhs->type->canonical));
				return LLVMBuildGEP2(context->builder, llvm_type(lhs_type->canonical->pointer), lhs_value, &rhs_value, 1, "ptradd");
			}
			if (is_float) return LLVMBuildFAdd(context->builder, lhs_value, rhs_value, "fadd");
			return gencontext_emit_add_int(context, lhs_type, binary_op == BINARYOP_ADD_MOD, lhs_value, rhs_value);
		case BINARYOP_DIV:
			if (is_float) return LLVMBuildFDiv(context->builder, lhs_value, rhs_value, "fdiv");
			return type_is_unsigned(lhs_type)
				? LLVMBuildUDiv(context->builder, lhs_value, rhs_value, "udiv")
				: LLVMBuildSDiv(context->builder, lhs_value, rhs_value, "sdiv");
		case BINARYOP_MOD:
			return type_is_unsigned(lhs_type)
				? LLVMBuildURem(context->builder, lhs_value, rhs_value, "umod")
				: LLVMBuildSRem(context->builder, lhs_value, rhs_value, "smod");
		case BINARYOP_SHR:
			return type_is_unsigned(lhs_type)
				? LLVMBuildLShr(context->builder, lhs_value, rhs_value, "lshr")
				: LLVMBuildAShr(context->builder, lhs_value, rhs_value, "ashr");
		case BINARYOP_SHL:
			return LLVMBuildShl(context->builder, lhs_value, rhs_value, "shl");
		case BINARYOP_BIT_AND:
			return LLVMBuildAnd(context->builder, lhs_value, rhs_value, "and");
		case BINARYOP_BIT_OR:
			return LLVMBuildOr(context->builder, lhs_value, rhs_value, "or");
		case BINARYOP_BIT_XOR:
			return LLVMBuildXor(context->builder, lhs_value, rhs_value, "xor");
		case BINARYOP_EQ:
			// Unordered?
			assert(type_is_float(lhs_type));
			return LLVMBuildFCmp(context->builder, LLVMRealUEQ, lhs_value, rhs_value, "eq");
		case BINARYOP_NE:
			// Unordered?
			assert(type_is_float(lhs_type));
			return LLVMBuildFCmp(context->builder, LLVMRealUNE, lhs_value, rhs_value, "neq");
		case BINARYOP_GE:
			assert(type_is_float(lhs_type));
			return LLVMBuildFCmp(context->builder, LLVMRealUGE, lhs_value, rhs_value, "ge");
		case BINARYOP_GT:
			assert(type_is_float(lhs_type));
			return LLVMBuildFCmp(context->builder, LLVMRealUGT, lhs_value, rhs_value, "gt");
		case BINARYOP_LE:
			assert(type_is_float(lhs_type));
			return LLVMBuildFCmp(context->builder, LLVMRealULE, lhs_value, rhs_value, "le");
		case BINARYOP_LT:
			assert(type_is_float(lhs_type));
			return LLVMBuildFCmp(context->builder, LLVMRealULE, lhs_value, rhs_value, "lt");
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
	UNREACHABLE
}


LLVMValueRef gencontext_emit_post_unary_expr(GenContext *context, Expr *expr)
{
	return gencontext_emit_post_inc_dec(context, expr->post_expr.expr, expr->post_expr.operator == POSTUNARYOP_INC ? 1 : -1, false);
}

LLVMValueRef gencontext_emit_typeid(GenContext *context, Expr *expr)
{
	if (type_is_builtin(expr->typeid_expr->type->type_kind))
	{
		return gencontext_emit_const_int(context, type_usize, expr->typeid_expr->type->type_kind);
	}
	assert(expr->typeid_expr->type->backend_typeid);
	return expr->typeid_expr->type->backend_typeid;
}

LLVMValueRef gencontext_emit_trycatch_expr(GenContext *context, Expr *expr)
{

	LLVMBasicBlockRef error_block = gencontext_create_free_block(context, "error_block");
	LLVMBasicBlockRef no_err_block = gencontext_create_free_block(context, "noerr_block");
	LLVMBasicBlockRef phi_block = gencontext_create_free_block(context, "phi_block");

	// Store catch/error var
	PUSH_ERROR();

	// Set the catch/error var
	context->error_var = NULL;
	context->catch_block = error_block;

	gencontext_emit_expr(context, expr->trycatch_expr);

	// Restore.
	POP_ERROR();

	// Emit success and jump to phi.
	gencontext_emit_br(context, no_err_block);
	gencontext_emit_block(context, no_err_block);
	gencontext_emit_br(context, phi_block);

	// Emit error and jump to phi
	gencontext_emit_block(context, error_block);
	gencontext_emit_br(context, phi_block);

	gencontext_emit_block(context, phi_block);

	LLVMValueRef phi = LLVMBuildPhi(context->builder, llvm_type(expr->type), "val");
	LLVMValueRef lhs = llvm_int(type_bool, expr->expr_kind == EXPR_TRY ? 1 : 0);
	LLVMValueRef rhs = llvm_int(type_bool, expr->expr_kind == EXPR_TRY ? 0 : 1);

	LLVMValueRef logic_values[2] = { lhs, rhs };
	LLVMBasicBlockRef blocks[2] = { no_err_block, error_block };
	LLVMAddIncoming(phi, logic_values, blocks, 2);

	return phi;
}

static inline LLVMValueRef gencontext_emit_else_jump_expr(GenContext *context, Expr *expr)
{
	LLVMBasicBlockRef else_block = gencontext_create_free_block(context, "else_block");
	LLVMBasicBlockRef no_err_block = gencontext_create_free_block(context, "noerr_block");

	// Store catch/error var
	PUSH_ERROR();

	// Set the catch/error var
	context->error_var = NULL;
	context->catch_block = else_block;

	LLVMValueRef value = gencontext_emit_expr(context, expr->else_expr.expr);

	// Restore.
	POP_ERROR();

	// Emit success and to end.
	gencontext_emit_br(context, no_err_block);

	// Emit else
	gencontext_emit_block(context, else_block);
	gencontext_emit_stmt(context, expr->else_expr.else_stmt);
	gencontext_emit_br(context, no_err_block);

	gencontext_emit_block(context, no_err_block);
	return value;
}


static LLVMValueRef gencontext_emit_else_expr(GenContext *context, Expr *expr)
{
	if (expr->else_expr.is_jump) return gencontext_emit_else_jump_expr(context, expr);
	LLVMBasicBlockRef else_block = gencontext_create_free_block(context, "else_block");
	LLVMBasicBlockRef phi_block = gencontext_create_free_block(context, "phi_block");

	// Store catch/error var
	PUSH_ERROR();

	// Set the catch/error var
	context->error_var = NULL;
	context->catch_block = else_block;

	LLVMValueRef normal_value = gencontext_emit_expr(context, expr->else_expr.expr);

	// Restore.
	POP_ERROR();

	// Emit success and jump to phi.
	LLVMBasicBlockRef success_end_block = gencontext_current_block_if_in_use(context);

	if (success_end_block) gencontext_emit_br(context, phi_block);

	// Emit else
	gencontext_emit_block(context, else_block);
	LLVMValueRef else_value = gencontext_emit_expr(context, expr->else_expr.else_expr);
	LLVMBasicBlockRef else_block_exit = gencontext_current_block_if_in_use(context);

	if (else_block_exit) gencontext_emit_br(context, phi_block);

	gencontext_emit_block(context, phi_block);

	if (!else_block_exit)
	{
		return normal_value;
	}
	if (!success_end_block)
	{
		return else_value;
	}

	LLVMValueRef phi = LLVMBuildPhi(context->builder, llvm_type(expr->type), "val");

	LLVMValueRef logic_values[2] = { normal_value, else_value };
	LLVMBasicBlockRef blocks[2] = { success_end_block, else_block_exit };
	LLVMAddIncoming(phi, logic_values, blocks, 2);

	return phi;

}


static inline LLVMValueRef gencontext_emit_guard_expr(GenContext *context, Expr *expr)
{
	LLVMBasicBlockRef guard_block = gencontext_create_free_block(context, "guard_block");
	LLVMBasicBlockRef no_err_block = gencontext_create_free_block(context, "noerr_block");

	// Store catch/error var
	PUSH_ERROR();

	// Set the catch/error var
	LLVMValueRef error_var = gencontext_emit_alloca(context, llvm_type(type_error), "");

	context->error_var = error_var;
	context->catch_block = guard_block;

	LLVMValueRef value = gencontext_emit_expr(context, expr->guard_expr.inner);

	// Restore.
	POP_ERROR();

	// Emit success and to end.
	gencontext_emit_br(context, no_err_block);

	POP_ERROR();

	// Emit else
	gencontext_emit_block(context, guard_block);

	// Ensure we are on a branch that is non empty.
	if (gencontext_check_block_branch_emit(context))
	{
		gencontext_emit_defer(context, expr->guard_expr.defer, 0);
		LLVMBuildRet(context->builder, gencontext_emit_load(context, type_error, error_var));
		context->current_block = NULL;
		context->current_block_is_target = NULL;
	}
	gencontext_emit_block(context, no_err_block);

	return value;
}

static LLVMValueRef gencontext_emit_binary_expr(GenContext *context, Expr *expr)
{
	BinaryOp binary_op = expr->binary_expr.operator;
	if (binary_op > BINARYOP_ASSIGN)
	{
		BinaryOp base_op = binaryop_assign_base_op(binary_op);
		assert(base_op != BINARYOP_ERROR);
		LLVMValueRef addr = gencontext_emit_address(context, expr->binary_expr.left);
		LLVMValueRef value = gencontext_emit_binary(context, expr, addr, base_op);
		LLVMBuildStore(context->builder, value, addr);
		return value;
	}
	if (binary_op == BINARYOP_ASSIGN)
	{
		LLVMValueRef addr = gencontext_emit_address(context, expr->binary_expr.left);
		LLVMValueRef failable_ref = NULL;
		if (expr->binary_expr.left->expr_kind == EXPR_IDENTIFIER)
		{
			failable_ref = decl_failable_ref(expr->binary_expr.left->identifier_expr.decl);
		}
		return gencontext_emit_assign_expr(context, addr, expr->binary_expr.right, failable_ref);
	}
	return gencontext_emit_binary(context, expr, NULL, binary_op);
}

LLVMValueRef gencontext_emit_elvis_expr(GenContext *context, Expr *expr)
{
	LLVMBasicBlockRef current_block = context->current_block;
	LLVMBasicBlockRef phi_block = LLVMCreateBasicBlockInContext(context->context, "cond.phi");
	LLVMBasicBlockRef rhs_block = LLVMCreateBasicBlockInContext(context->context, "cond.rhs");

	// Generate condition and conditional branch
	LLVMValueRef lhs = gencontext_emit_expr(context, expr->ternary_expr.cond);
	LLVMValueRef cond = lhs;
	Type *cond_type = expr->ternary_expr.cond->type->canonical;
	if (cond_type != type_bool)
	{
		CastKind cast = cast_to_bool_kind(cond_type);
		cond = gencontext_emit_cast(context, cast, cond, cond_type, type_bool);
	}

	gencontext_emit_cond_br(context, cond, phi_block, rhs_block);

	gencontext_emit_block(context, rhs_block);
	LLVMValueRef rhs = gencontext_emit_expr(context, expr->ternary_expr.else_expr);
	LLVMBasicBlockRef end_block = context->current_block;

	gencontext_emit_br(context, phi_block);

	// Generate phi
	gencontext_emit_block(context, phi_block);

	LLVMValueRef phi = LLVMBuildPhi(context->builder, llvm_type(expr->type), "val");

	LLVMValueRef logic_values[2] = { lhs, rhs };
	LLVMBasicBlockRef blocks[2] = { current_block, end_block };
	LLVMAddIncoming(phi, logic_values, blocks, 2);

	return phi;
}

LLVMValueRef gencontext_emit_ternary_expr(GenContext *context, Expr *expr)
{
	if (expr->ternary_expr.then_expr == NULL) return gencontext_emit_elvis_expr(context, expr);

	// Set up basic blocks, following Cone
	LLVMBasicBlockRef phi_block = LLVMCreateBasicBlockInContext(context->context, "cond.phi");
	LLVMBasicBlockRef lhs_block = LLVMCreateBasicBlockInContext(context->context, "cond.lhs");
	LLVMBasicBlockRef rhs_block = LLVMCreateBasicBlockInContext(context->context, "cond.rhs");

	// Generate condition and conditional branch
	LLVMValueRef cond = gencontext_emit_expr(context, expr->ternary_expr.cond);

	gencontext_emit_cond_br(context, cond, lhs_block, rhs_block);

	gencontext_emit_block(context, lhs_block);
	LLVMValueRef lhs = gencontext_emit_expr(context, expr->ternary_expr.then_expr);
	LLVMBasicBlockRef lhs_exit = gencontext_current_block_if_in_use(context);
	if (lhs_exit) gencontext_emit_br(context, phi_block);

	gencontext_emit_block(context, rhs_block);
	LLVMValueRef rhs = gencontext_emit_expr(context, expr->ternary_expr.else_expr);
	LLVMBasicBlockRef rhs_exit = gencontext_current_block_if_in_use(context);
	if (rhs_exit) gencontext_emit_br(context, phi_block);

	// Generate phi
	gencontext_emit_block(context, phi_block);
	if (!rhs_exit)
	{
		return lhs;
	}
	if (!lhs_exit)
	{
		return rhs;
	}
	LLVMValueRef phi = LLVMBuildPhi(context->builder, llvm_type(expr->type), "val");
	LLVMValueRef logic_values[2] = { lhs, rhs };
	LLVMBasicBlockRef blocks[2] = { lhs_exit, rhs_exit };
	LLVMAddIncoming(phi, logic_values, blocks, 2);

	return phi;
}


LLVMValueRef gencontext_emit_const_expr(GenContext *context, Expr *expr)
{
	Type *type = type_reduced_from_expr(expr)->canonical;
	switch (expr->const_expr.kind)
	{
		case ALL_INTS:
			if (type_is_unsigned(type))
			{
				return llvm_int(type, bigint_as_unsigned(&expr->const_expr.i));
			}
			else
			{
				return llvm_int(type, bigint_as_signed(&expr->const_expr.i));
			}
		case ALL_FLOATS:
			return LLVMConstReal(llvm_type(type), (double) expr->const_expr.f);
		case TYPE_POINTER:
			return LLVMConstNull(llvm_type(type));
		case TYPE_BOOL:
			return llvm_int(type, expr->const_expr.b ? 1 : 0);
		case TYPE_STRING:
		{
			LLVMValueRef global_name = LLVMAddGlobal(context->module, LLVMArrayType(llvm_type(type_char), expr->const_expr.string.len + 1), "");
			LLVMSetLinkage(global_name, LLVMInternalLinkage);
			LLVMSetGlobalConstant(global_name, 1);
			LLVMSetInitializer(global_name, LLVMConstStringInContext(context->context,
			                                                         expr->const_expr.string.chars,
			                                                         expr->const_expr.string.len,
			                                                         0));
			return global_name;
		}
		case TYPE_ERRTYPE:
			TODO
		default:
			UNREACHABLE
	}
}

LLVMValueRef gencontext_emit_call_expr(GenContext *context, Expr *expr)
{
	size_t args = vec_size(expr->call_expr.arguments);
	FunctionSignature *signature;
	LLVMValueRef func;
	LLVMTypeRef func_type;

	if (expr->call_expr.is_pointer_call)
	{
		signature = expr->call_expr.function->type->canonical->pointer->func.signature;
		func = gencontext_emit_expr(context, expr->call_expr.function);
		func_type = llvm_type(expr->call_expr.function->type->canonical->pointer);
	}
	else if (expr->call_expr.is_struct_function)
	{
		Decl *function_decl = expr->call_expr.function->access_expr.ref;
		signature = &function_decl->func.function_signature;
		func = function_decl->ref;
		func_type = llvm_type(function_decl->type);
	}
	else
	{
		Decl *function_decl = expr->call_expr.function->identifier_expr.decl;
		signature = &function_decl->func.function_signature;
		func = function_decl->ref;
		func_type = llvm_type(function_decl->type);
	}


	LLVMValueRef return_param = NULL;
	if (signature->return_param)
	{
		return_param = gencontext_emit_alloca(context, llvm_type(signature->rtype->type), "returnparam");
		args++;
	}
	LLVMValueRef *values = args ? malloc_arena(args * sizeof(LLVMValueRef)) : NULL;
	unsigned param_index = 0;
	if (return_param)
	{
		values[param_index++] = return_param;
	}
	VECEACH(expr->call_expr.arguments, i)
	{
		values[param_index++] = gencontext_emit_expr(context, expr->call_expr.arguments[i]);
	}

	LLVMValueRef call = LLVMBuildCall2(context->builder, func_type, func, values, args, "");

	//gencontext_emit_throw_branch(context, call, signature->throws, expr->call_expr.throw_info, signature->error_return);

	// If we used a return param, then load that info here.
	if (return_param)
	{
		call = gencontext_emit_load(context, signature->rtype->type, return_param);
	}
	/*
	if (function->func.function_signature.convention)
	{
		LLVMSetFunctionCallConv(call, LLVMX86StdcallCallConv);
	}*/
	return call;
}




static inline LLVMValueRef gencontext_emit_expression_list_expr(GenContext *context, Expr *expr)
{
	LLVMValueRef value = NULL;
	VECEACH(expr->expression_list, i)
	{
		value = gencontext_emit_expr(context, expr->expression_list[i]);
	}
	return value;
}


static inline LLVMValueRef gencontext_emit_expr_block(GenContext *context, Expr *expr)
{
	LLVMValueRef old_ret_out = context->return_out;
	LLVMBasicBlockRef saved_expr_block = context->expr_block_exit;

	LLVMBasicBlockRef expr_block = gencontext_create_free_block(context, "expr_block.exit");
	context->expr_block_exit = expr_block;

	LLVMValueRef return_out = NULL;
	if (expr->type != type_void)
	{
		return_out = gencontext_emit_alloca(context, llvm_type(expr->type), "blockret");
	}
	context->return_out = return_out;

	Ast **stmts = expr->expr_block.stmts;
	VECEACH(stmts, i)
	{
		gencontext_emit_stmt(context, stmts[i]);
	}
	gencontext_emit_br(context, expr_block);

	// Emit the exit block.
	gencontext_emit_block(context, expr_block);

	context->return_out = old_ret_out;
	context->expr_block_exit = saved_expr_block;

	return return_out ? gencontext_emit_load(context, expr->type, return_out) : NULL;
}

static inline LLVMValueRef gencontext_emit_macro_block(GenContext *context, Expr *expr)
{
	LLVMValueRef old_ret_out = context->return_out;
	LLVMBasicBlockRef saved_expr_block = context->expr_block_exit;

	LLVMBasicBlockRef expr_block = gencontext_create_free_block(context, "expr_block.exit");
	context->expr_block_exit = expr_block;

	LLVMValueRef return_out = NULL;
	if (expr->type != type_void)
	{
		return_out = gencontext_emit_alloca(context, llvm_type(expr->type), "blockret");
	}
	context->return_out = return_out;

	Ast **stmts = expr->macro_block.stmts;
	VECEACH(expr->macro_block.params, i)
	{
		// In case we have a constant, we never do an emit. The value is already folded.
		if (!expr->macro_block.args[i]) continue;
		Decl *decl = expr->macro_block.params[i];
		decl->ref = gencontext_emit_alloca(context, llvm_type(decl->type), decl->name);
		LLVMValueRef value = gencontext_emit_expr(context, expr->macro_block.args[i]);
		gencontext_emit_store(context, decl, value);
	}

	VECEACH(stmts, i)
	{
		gencontext_emit_stmt(context, stmts[i]);
	}
	gencontext_emit_br(context, expr_block);

	// Emit the exit block.
	gencontext_emit_block(context, expr_block);

	context->return_out = old_ret_out;
	context->expr_block_exit = saved_expr_block;

	return return_out ? gencontext_emit_load(context, expr->type, return_out) : NULL;
}

LLVMValueRef gencontext_emit_call_intrinsic(GenContext *context, unsigned intrinsic_id, LLVMTypeRef *types, unsigned type_count,
                               LLVMValueRef *values, unsigned arg_count)
{
	LLVMValueRef decl = LLVMGetIntrinsicDeclaration(context->module, intrinsic_id, types, type_count);
	LLVMTypeRef type = LLVMIntrinsicGetType(context->context, intrinsic_id, types, arg_count);
	return LLVMBuildCall2(context->builder, type, decl, values, arg_count, "");
}

LLVMValueRef gencontext_emit_assign_expr(GenContext *context, LLVMValueRef ref, Expr *expr, LLVMValueRef failable_ref)
{
	LLVMBasicBlockRef assign_block = NULL;

	PUSH_ERROR();

	if (failable_ref)
	{
		assign_block = gencontext_create_free_block(context, "after_assign");

		context->error_var = failable_ref;
		context->catch_block = assign_block;

	}
	LLVMValueRef value;
	switch (expr->expr_kind)
	{
		case EXPR_INITIALIZER_LIST:
			value = gencontext_emit_load(context,
			                            expr->type,
			                            gencontext_emit_initializer_list_expr_addr(context, expr, ref));
			break;
		default:
			value = gencontext_emit_expr(context, expr);
			LLVMBuildStore(context->builder, value, ref);
			break;
	}
	if (failable_ref)
	{
		LLVMBuildStore(context->builder, gencontext_emit_no_error_union(context), failable_ref);
	}
	POP_ERROR();

	if (failable_ref)
	{
		gencontext_emit_br(context, assign_block);
		gencontext_emit_block(context, assign_block);

	}

	return value;
}


static inline LLVMValueRef gencontext_emit_identifier_rvalue(GenContext *context, Decl *decl)
{
	if (decl->decl_kind != DECL_VAR || !decl->var.failable)
	{
		return gencontext_emit_load(context, decl->type, decl_ref(decl));
	}

	assert(context->catch_block);
	LLVMBasicBlockRef after_block = gencontext_create_free_block(context, "after_check");
	LLVMValueRef error_value = gencontext_emit_load(context, type_error, decl_failable_ref(decl));
	LLVMValueRef comp = LLVMBuildICmp(context->builder, LLVMIntEQ, error_value,
	                                  gencontext_emit_no_error_union(context), "");
	if (context->error_var)
	{
		LLVMBasicBlockRef error_block = gencontext_create_free_block(context, "error");
		gencontext_emit_cond_br(context, comp, after_block, error_block);
		gencontext_emit_block(context, error_block);
		LLVMBuildStore(context->builder, error_value, gencontext_emit_bitcast(context, context->error_var, type_get_ptr(type_error)));
		gencontext_emit_br(context, context->catch_block);
	}
	else
	{
		gencontext_emit_cond_br(context, comp, after_block, context->catch_block);
	}
	gencontext_emit_block(context, after_block);
	return gencontext_emit_load(context, decl->type, decl_ref(decl));
}

static inline LLVMValueRef gencontext_emit_failable(GenContext *context, Expr *expr)
{
	Expr *fail = expr->failable_expr;
	if (context->error_var)
	{
		assert(context->error_var);
		LLVMValueRef value = gencontext_emit_expr(context, fail);
		LLVMBuildStore(context->builder, value, gencontext_emit_bitcast(context, context->error_var, type_get_ptr(fail->type)));
	}
	gencontext_emit_br(context, context->catch_block);
	LLVMBasicBlockRef ignored_block = gencontext_create_free_block(context, "postfailed");
	gencontext_emit_block(context, ignored_block);
	if (expr->type->canonical == type_void) return NULL;
	return LLVMGetUndef(llvm_type(expr->type));
}

LLVMValueRef gencontext_emit_expr(GenContext *context, Expr *expr)
{
NESTED_RETRY:
	switch (expr->expr_kind)
	{
		case EXPR_POISONED:
		case EXPR_DECL_LIST:
			UNREACHABLE
		case EXPR_DESIGNATED_INITIALIZER:
			// Should only appear when generating designated initializers.
			UNREACHABLE
		case EXPR_SLICE_ASSIGN:
			return gencontext_emit_slice_assign(context, expr);
		case EXPR_SLICE:
			return gencontext_emit_slice(context, expr);
		case EXPR_LEN:
			return gencontext_emit_len(context, expr);
		case EXPR_FAILABLE:
			return gencontext_emit_failable(context, expr);
		case EXPR_TRY:
		case EXPR_CATCH:
			return gencontext_emit_trycatch_expr(context, expr);
		case EXPR_ELSE:
			return gencontext_emit_else_expr(context, expr);
		case EXPR_MACRO_BLOCK:
			return gencontext_emit_macro_block(context, expr);
		case EXPR_COMPOUND_LITERAL:
			expr = expr->expr_compound_literal.initializer;
			goto NESTED_RETRY;
		case EXPR_INITIALIZER_LIST:
			return gencontext_emit_load(context, expr->type, gencontext_emit_initializer_list_expr_addr(context, expr, NULL));
		case EXPR_EXPR_BLOCK:
			return gencontext_emit_expr_block(context, expr);
		case EXPR_SCOPED_EXPR:
			return gencontext_emit_scoped_expr(context, expr);
		case EXPR_UNARY:
			return gencontext_emit_unary_expr(context, expr);
		case EXPR_CONST:
			return gencontext_emit_const_expr(context, expr);
		case EXPR_BINARY:
			return gencontext_emit_binary_expr(context, expr);
		case EXPR_TERNARY:
			return gencontext_emit_ternary_expr(context, expr);
		case EXPR_POST_UNARY:
			return gencontext_emit_post_unary_expr(context, expr);
		case EXPR_GUARD:
			return gencontext_emit_guard_expr(context, expr);
		case EXPR_TYPEID:
			return gencontext_emit_typeid(context, expr);
		case EXPR_TYPE_ACCESS:
		case EXPR_TYPEOF:
			// These are folded in the semantic analysis step.
			UNREACHABLE
		case EXPR_IDENTIFIER:
			return gencontext_emit_identifier_rvalue(context, expr->identifier_expr.decl);
		case EXPR_SUBSCRIPT:
		case EXPR_ACCESS:
			return gencontext_emit_load(context, expr->type, gencontext_emit_address(context, expr));
		case EXPR_CALL:
			return gencontext_emit_call_expr(context, expr);
		case EXPR_GROUP:
			expr = expr->group_expr;
			goto NESTED_RETRY;
		case EXPR_EXPRESSION_LIST:
			return gencontext_emit_expression_list_expr(context, expr);
		case EXPR_CAST:
			return gencontext_emit_cast_expr(context, expr);
	}
	UNREACHABLE
}