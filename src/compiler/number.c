// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"

#define FLOAT32_LIMIT 340282346638528859811704183484516925440.0000000000000000
#define FLOAT64_LIMIT 179769313486231570814527423731704356798070567525844996598917476803157260780028538760589558632766878171540458953514382464234321326889464182768467546703537516986049910576551282076245490090389328944075868508455133942304583236903222948165808559332123348274797826204144723168738177180919299881250404026184124858368.0000000000000000
#define FLOAT16_LIMIT 65504
#define BFLOAT_LIMIT  338953138925153547590470800371487866880.0000000000000000

static inline bool compare_bool(bool left, bool right, BinaryOp op)
{
	switch (op)
	{
		case BINARYOP_EQ:
			return left == right;
		case BINARYOP_NE:
			return left != right;
		case BINARYOP_GT:
			return left > right;
		case BINARYOP_LT:
			return left < right;
		case BINARYOP_GE:
			return left >= right;
		case BINARYOP_LE:
			return left <= right;
		default:
			UNREACHABLE
	}
}
static inline bool compare_fps(Real left, Real right, BinaryOp op)
{
	switch (op)
	{
		case BINARYOP_GE:
			return left >= right;
		case BINARYOP_LE:
			return left <= right;
		case BINARYOP_NE:
			return left != right;
		case BINARYOP_GT:
			return left > right;
		case BINARYOP_LT:
			return left < right;
		case BINARYOP_EQ:
			return left == right;
		default:
			UNREACHABLE
	}
}
void expr_contract_array(ExprConst *expr_const, ConstKind contract_type)
{
	if (expr_const->const_kind == CONST_SLICE && !expr_const->slice_init)
	{
		*expr_const = (ExprConst) { .const_kind = contract_type };
		return;
	}
	ASSERT0(expr_const->const_kind == CONST_INITIALIZER || expr_const->const_kind == CONST_SLICE);
	ConstInitializer *initializer = expr_const->const_kind == CONST_SLICE
			? expr_const->slice_init
			: expr_const->initializer;
	Type *type = initializer->type;
	ASSERT0(type_is_any_arraylike(type));
	ArraySize len = type->array.len;
	ASSERT0(len > 0);
	char *arr = calloc_arena(len);
	switch (initializer->kind)
	{
		case CONST_INIT_ZERO:
			break;
		case CONST_INIT_STRUCT:
		case CONST_INIT_UNION:
		case CONST_INIT_VALUE:
		case CONST_INIT_ARRAY_VALUE:
			UNREACHABLE
		case CONST_INIT_ARRAY:
		{
			FOREACH(ConstInitializer *, init, initializer->init_array.elements)
			{
				ASSERT0(init->kind == CONST_INIT_ARRAY_VALUE);
				arr[init->init_array_value.index] = (char) int_to_i64(init->init_array_value.element->init_value->const_expr.ixx);
			}
			break;
		}
		case CONST_INIT_ARRAY_FULL:
		{
			FOREACH_IDX(i, ConstInitializer *, init, initializer->init_array_full)
			{
				ASSERT0(init->kind == CONST_INIT_VALUE);
				arr[i] = (char)int_to_i64(init->init_value->const_expr.ixx);
			}
			break;
		}
	}
	*expr_const = (ExprConst) { .const_kind = contract_type, .bytes.ptr = arr, .bytes.len = len };
}

INLINE bool const_is_bytes(ConstKind kind)
{
	return kind == CONST_BYTES || kind == CONST_STRING;
}

bool expr_const_compare(const ExprConst *left, const ExprConst *right, BinaryOp op)
{
	bool is_eq;
	ConstKind left_kind = left->const_kind;
	ConstKind right_kind = right->const_kind;
	ExprConst replace;
	if (left_kind != right_kind)
	{
		if (const_is_bytes(left_kind))
		{
			if (!const_is_bytes(right_kind))
			{
				replace = *right;
				expr_contract_array(&replace, left_kind);
				right = &replace;
			}
		}
		else if (const_is_bytes(right_kind))
		{
			if (!const_is_bytes(left_kind))
			{
				replace = *left;
				expr_contract_array(&replace, right_kind);
				left = &replace;
			}
		}
	}
	switch (left->const_kind)
	{
		case CONST_BOOL:
			return compare_bool(left->b, right->b, op);
		case CONST_INTEGER:
			ASSERT0(right->const_kind != CONST_ENUM);
			return int_comp(left->ixx, right->ixx, op);
		case CONST_REF:
			ASSERT0(right->const_kind == CONST_POINTER || right->const_kind == CONST_REF);
			if (right->const_kind == CONST_POINTER) return false;
			return decl_flatten(right->global_ref) == decl_flatten(left->global_ref);
		case CONST_FLOAT:
			return compare_fps(left->fxx.f, right->fxx.f, op);
		case CONST_POINTER:
		{
			Int a = { .i.low = left->ptr, .type = TYPE_U64 };
			Int b = { .i.low = right->ptr, .type = TYPE_U64 };
			return int_comp(a, b, op);
		}
		case CONST_STRING:
			if (left->bytes.len != right->bytes.len)
			{
				is_eq = false;
				goto RETURN;
			}
			if (right->bytes.ptr == left->bytes.ptr)
			{
				is_eq = true;
				goto RETURN;
			}
			is_eq = !strncmp(left->bytes.ptr, right->bytes.ptr, left->bytes.len);
			goto RETURN;
		case CONST_TYPEID:
			is_eq = left->typeid == right->typeid;
			goto RETURN;
		case CONST_ERR:
		case CONST_ENUM:
		{
			Decl *left_decl = left->enum_err_val;
			// The error case
			ASSERT0(right->const_kind == left->const_kind);
			Decl *right_decl = right->enum_err_val;
			// Non-matching cannot be compared.
			if (right_decl->type != left_decl->type) return false;
			int64_t right_ordinal = right->enum_err_val->enum_constant.ordinal;
			switch (op)
			{
				case BINARYOP_GT:
					return left_decl->enum_constant.ordinal > right_ordinal;
				case BINARYOP_GE:
					return left_decl->enum_constant.ordinal >= right_ordinal;
				case BINARYOP_LT:
					return left_decl->enum_constant.ordinal < right_ordinal;
				case BINARYOP_LE:
					return left_decl->enum_constant.ordinal <= right_ordinal;
				case BINARYOP_NE:
					return left_decl->enum_constant.ordinal != right_ordinal;
				case BINARYOP_EQ:
					return left_decl->enum_constant.ordinal == right_ordinal;
				default:
					return false;
			}
		}
		case CONST_BYTES:
			if (left->bytes.len != right->bytes.len)
			{
				is_eq = false;
				goto RETURN;
			}
			if (right->bytes.ptr == left->bytes.ptr)
			{
				is_eq = true;
				goto RETURN;
			}
			is_eq = !memcmp(left->bytes.ptr, right->bytes.ptr, left->bytes.len);
			goto RETURN;
		case CONST_SLICE:
			return false;
		case CONST_INITIALIZER:
			return false;
		case CONST_UNTYPED_LIST:
			return false;
		case CONST_MEMBER:
			is_eq = left->member.decl == right->member.decl;
			goto RETURN;
	}
	UNREACHABLE
RETURN:
	assert((op == BINARYOP_EQ) || (op == BINARYOP_NE));
	return op == BINARYOP_EQ ? is_eq : !is_eq;
}

bool expr_const_in_range(const ExprConst *left, const ExprConst *right, const ExprConst *right_to)
{
	if (right == right_to)
	{
		return expr_const_compare(left, right, BINARYOP_EQ);
	}
	return expr_const_compare(left, right, BINARYOP_GE) && expr_const_compare(left, right_to, BINARYOP_LE);
}

bool expr_const_float_fits_type(const ExprConst *expr_const, TypeKind kind)
{
	Real hi_limit;
	Real lo_limit;
	switch (kind)
	{
		case TYPE_BF16:
			lo_limit = hi_limit = BFLOAT_LIMIT;
			break;
		case TYPE_F16:
			lo_limit = hi_limit = FLOAT16_LIMIT;
			break;
		case TYPE_F32:
			lo_limit = hi_limit = FLOAT32_LIMIT;
			break;
		case TYPE_F64:
			lo_limit = hi_limit = FLOAT64_LIMIT;
			break;
		case TYPE_F128:
			// Assume this to be true
			return true;
		case TYPE_BOOL:
			return true;
		default:
			UNREACHABLE
	}
	ASSERT0(expr_const->const_kind == CONST_FLOAT);
	return expr_const->fxx.f >= -lo_limit && expr_const->fxx.f <= hi_limit;
}

bool expr_const_will_overflow(const ExprConst *expr, TypeKind kind)
{
	switch (expr->const_kind)
	{
		case CONST_FLOAT:
			return !expr_const_float_fits_type(expr, kind);
		case CONST_INTEGER:
			return !int_fits(expr->ixx, kind);
		case CONST_BOOL:
			return false;
		case CONST_ENUM:
		{
			Int i = { .i = { .low = expr->enum_err_val->var.index }, .type = type_flatten(expr->enum_err_val->type)->type_kind };
			return !int_fits(i, kind);
		}
		case CONST_ERR:
		case CONST_BYTES:
		case CONST_STRING:
		case CONST_POINTER:
		case CONST_TYPEID:
		case CONST_SLICE:
		case CONST_INITIALIZER:
		case CONST_UNTYPED_LIST:
		case CONST_MEMBER:
		case CONST_REF:
			UNREACHABLE;
	}
	UNREACHABLE;
}



const char *expr_const_to_error_string(const ExprConst *expr)
{
	switch (expr->const_kind)
	{
		case CONST_POINTER:
			if (!expr->ptr) return "null";
			return str_printf("%p", (void*)(intptr_t)expr->ptr);
		case CONST_BOOL:
			return expr->b ? "true" : "false";
		case CONST_INTEGER:
			return int_to_str(expr->ixx, 10, false);
		case CONST_FLOAT:
			return str_printf("%g", expr->fxx.f);
		case CONST_STRING:
			return str_printf("\"%*.s\"", expr->bytes.len, expr->bytes.ptr);
		case CONST_BYTES:
			return "<binary data>";
		case CONST_REF:
			return expr->global_ref->name;
		case CONST_ENUM:
		case CONST_ERR:
			return expr->enum_err_val->name;
		case CONST_TYPEID:
			return type_to_error_string(expr->typeid);
		case CONST_MEMBER:
			return "member";
		case CONST_SLICE:
		case CONST_INITIALIZER:
			return "constant list";
		case CONST_UNTYPED_LIST:
			return "untyped list";
	}
	UNREACHABLE
}





