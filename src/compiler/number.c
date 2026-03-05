// Copyright (c) 2020-2025 Christoffer Lerno. All rights reserved.
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
		*expr_const = (ExprConst) { .const_kind = contract_type, .bytes.ptr = NULL, .bytes.len = 0 };
		return;
	}
	ASSERT(expr_const->const_kind == CONST_INITIALIZER || expr_const->const_kind == CONST_SLICE);
	ConstInitializer *initializer = expr_const->const_kind == CONST_SLICE
			? expr_const->slice_init
			: expr_const->initializer;
	Type *type = initializer->type;
	ASSERT(type_is_any_arraylike(type));
	ArraySize len = type->array.len;
	ASSERT(len > 0);
	char *arr = calloc_arena(len);
	switch (initializer->kind)
	{
		case CONST_INIT_ZERO:
			break;
		case CONST_INIT_STRUCT:
		case CONST_INIT_UNION:
		case CONST_INIT_VALUE:
		case CONST_INIT_ARRAY_VALUE:
			UNREACHABLE_VOID
		case CONST_INIT_ARRAY:
		{
			FOREACH(ConstInitializer *, init, initializer->init_array.elements)
			{
				ASSERT(init->kind == CONST_INIT_ARRAY_VALUE);
				arr[init->init_array_value.index] = (char) int_to_i64(init->init_array_value.element->init_value->const_expr.ixx);
			}
			break;
		}
		case CONST_INIT_ARRAY_FULL:
		{
			FOREACH_IDX(i, ConstInitializer *, init, initializer->init_array_full)
			{
				ASSERT(init->kind == CONST_INIT_VALUE);
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

INLINE ConstInitializer *expr_const_array_init_at(ConstInitializer *init, ArraySize index)
{
	switch (init->kind)
	{
		case CONST_INIT_ZERO:
			return NULL;
		case CONST_INIT_STRUCT:
		case CONST_INIT_UNION:
		case CONST_INIT_VALUE:
		case CONST_INIT_ARRAY_VALUE:
			UNREACHABLE
		case CONST_INIT_ARRAY:
		{
			FOREACH(ConstInitializer *, e, init->init_array.elements)
			{
				ArrayIndex idx = e->init_array_value.index;
				if (idx != index) continue;
				return e->init_array_value.element;
			}
			return NULL;
		}
		case CONST_INIT_ARRAY_FULL:
			return init->init_array_full[index];
	}
	UNREACHABLE;
}

INLINE void swap_to_zero_to_left(ConstInitializer **left_ref, ConstInitializer **right_ref)
{
	ConstInitializer *right = *right_ref;
	if (right->kind == CONST_INIT_ZERO)
	{
		*right_ref = *left_ref;
		*left_ref = right;
	}
}


static bool expr_const_compare_bitstruct(const ExprConst *left, const ExprConst *right, BinaryOp op)
{
	ConstInitializer *lhs = left->initializer;
	ConstInitializer *rhs = right->initializer;
	swap_to_zero_to_left(&lhs, &rhs);
	bool find_eq = op == BINARYOP_EQ;
	if (lhs->kind == CONST_INIT_ZERO) return const_init_is_zero(rhs) ? find_eq : !find_eq;
	ConstInitializer **lhs_inits = lhs->init_struct;
	ConstInitializer **rhs_inits = rhs->init_struct;
	Decl **members = lhs->type->decl->strukt.members;
	unsigned len = vec_size(members);
	for (unsigned i = 0; i < len; i++)
	{
		ConstInitializer *init_lhs = lhs_inits[i];
		ConstInitializer *init_rhs = rhs_inits[i];
		swap_to_zero_to_left(&init_lhs, &init_rhs);
		// Zero case
		if (lhs->kind == CONST_INIT_ZERO)
		{
			if (const_init_is_zero(rhs)) continue;
			return !find_eq;
		}
		// Both should have values
		Expr *lhs_expr = init_lhs->init_value;
		Expr *rhs_expr = init_rhs->init_value;

		bool to_const = sema_cast_const(lhs_expr) && sema_cast_const(rhs_expr);
		assert(to_const);
		if (!expr_const_compare(&lhs_expr->const_expr, &rhs_expr->const_expr, BINARYOP_EQ))
		{
			return !find_eq;
		}
	}
	return find_eq;
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
			ASSERT(right->const_kind != CONST_ENUM);
			return int_comp(left->ixx, right->ixx, op);
		case CONST_FAULT:
			ASSERT(right->const_kind == CONST_FAULT);
			if (right->fault == left->fault) return true;
			return right->fault && left->fault && decl_flatten(right->fault) == decl_flatten(left->fault);
		case CONST_REF:
			ASSERT(right->const_kind == CONST_POINTER || right->const_kind == CONST_REF);
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
		case CONST_ENUM:
		{
			Decl *left_decl = left->enum_val;
			// The error case
			ASSERT(right->const_kind == left->const_kind);
			Decl *right_decl = right->enum_val;
			// Non-matching cannot be compared.
			if (right_decl->type != left_decl->type) return false;
			int64_t right_ordinal = right->enum_val->enum_constant.inner_ordinal;
			switch (op)
			{
				case BINARYOP_GT:
					return left_decl->enum_constant.inner_ordinal > right_ordinal;
				case BINARYOP_GE:
					return left_decl->enum_constant.inner_ordinal >= right_ordinal;
				case BINARYOP_LT:
					return left_decl->enum_constant.inner_ordinal < right_ordinal;
				case BINARYOP_LE:
					return left_decl->enum_constant.inner_ordinal <= right_ordinal;
				case BINARYOP_NE:
					return left_decl->enum_constant.inner_ordinal != right_ordinal;
				case BINARYOP_EQ:
					return left_decl->enum_constant.inner_ordinal == right_ordinal;
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
		case CONST_INITIALIZER:
			if (type_kind_is_real_vector(left->initializer->type->type_kind))
			{
				ConstInitializer *lhs = left->initializer;
				ConstInitializer *rhs = right->initializer;
				ArraySize len = lhs->type->canonical->array.len;
				bool rhs_is_zero = rhs->kind == CONST_INIT_ZERO;
				if (lhs->kind == CONST_INIT_ZERO && rhs_is_zero) return op == BINARYOP_EQ;
				for (ArrayIndex i = 0; i < len; i++)
				{
					ConstInitializer *a = expr_const_array_init_at(lhs, i);
					ConstInitializer *b = expr_const_array_init_at(rhs, i);
					bool a_is_zero = !a || const_init_is_zero(a);
					bool b_is_zero = !b || const_init_is_zero(b);
					if (a_is_zero || b_is_zero)
					{
						if (a_is_zero != b_is_zero) goto MISMATCH;
						continue;
					}
					assert(a && b);
					assert(b->kind == CONST_INIT_VALUE && a->kind == CONST_INIT_VALUE);
					Expr *a_value = a->init_value;
					Expr *b_value = b->init_value;
					bool is_equal = expr_const_compare(&a_value->const_expr, &b_value->const_expr, BINARYOP_EQ);
					if (is_equal) continue;
MISMATCH:
					return op != BINARYOP_EQ;
				}
				return op == BINARYOP_EQ;
			}
			if (left->initializer->type->type_kind == TYPE_BITSTRUCT)
			{
				return expr_const_compare_bitstruct(left, right, op);
			}
			FALLTHROUGH;
		case CONST_SLICE:
		case CONST_UNTYPED_LIST:
			UNREACHABLE;
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
	ASSERT(expr_const->const_kind == CONST_FLOAT);
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
			Int i = { .i = { .low = expr->enum_val->var.index }, .type = type_flatten(expr->enum_val->type)->type_kind };
			return !int_fits(i, kind);
		}
		case CONST_FAULT:
		case CONST_BYTES:
		case CONST_STRING:
		case CONST_POINTER:
		case CONST_TYPEID:
		case CONST_SLICE:
		case CONST_INITIALIZER:
		case CONST_UNTYPED_LIST:
		case CONST_MEMBER:
		case CONST_REF:
			UNREACHABLE
	}
	UNREACHABLE
}

void const_init_to_scratch_buffer(ConstInitializer *init)
{
	assert(init != NULL);
	switch (init->kind)
	{
		case CONST_INIT_ZERO:
			scratch_buffer_append("{}");
			return;
		case CONST_INIT_STRUCT:
		{
			scratch_buffer_append("{ ");
			FOREACH_IDX(i, ConstInitializer *, init, init->init_struct)
			{
				if (i != 0) scratch_buffer_append(", ");
				const_init_to_scratch_buffer(init);
			}
			scratch_buffer_append(" }");
			return;
		}
		case CONST_INIT_UNION:
		{
			scratch_buffer_printf("{ .%s = ", init->type->decl->strukt.members[init->init_union.index]->name);
			const_init_to_scratch_buffer(init->init_union.element);
			scratch_buffer_append(" }");
			return;
		}
		case CONST_INIT_VALUE:
			expr_const_to_scratch_buffer(&init->init_value->const_expr);
			return;
		case CONST_INIT_ARRAY:
		{
			scratch_buffer_append("{ ");
			FOREACH_IDX(i, ConstInitializer *, init, init->init_array.elements)
			{
				if (i != 0) scratch_buffer_append(", ");
				const_init_to_scratch_buffer(init);
			}
			scratch_buffer_append(" }");
			return;
		}
		case CONST_INIT_ARRAY_FULL:
		{
			scratch_buffer_append("{ ");
			FOREACH_IDX(i, ConstInitializer *, init, init->init_array_full)
			{
				if (i != 0) scratch_buffer_append(", ");
				const_init_to_scratch_buffer(init);
			}
			scratch_buffer_append(" }");
			return;
		}
		case CONST_INIT_ARRAY_VALUE:
			scratch_buffer_printf("[%lld] = ", (long long)init->init_array_value.index);
			const_init_to_scratch_buffer(init->init_array_value.element);
			return;
	}
	UNREACHABLE_VOID
}
void expr_const_to_scratch_buffer(const ExprConst *expr)
{
	switch (expr->const_kind)
	{
		case CONST_POINTER:
			if (!expr->ptr)
			{
				scratch_buffer_append("null");
			}
			else
			{
				scratch_buffer_printf("%p", (void *)(intptr_t)expr->ptr);
			}
			return;
		case CONST_BOOL:
			scratch_buffer_append(expr->b ? "true" : "false");
			return;
		case CONST_INTEGER:
			scratch_buffer_append(int_to_str(expr->ixx, 10, false));
			return;
		case CONST_FLOAT:
			scratch_buffer_printf("%g", expr->fxx.f);
			return;
		case CONST_STRING:
			scratch_buffer_append_len(expr->bytes.ptr, expr->bytes.len);
			return;
		case CONST_BYTES:
			scratch_buffer_append("<binary data>");
			return;
		case CONST_REF:
			scratch_buffer_append(expr->global_ref->name);
			return;
		case CONST_FAULT:
			scratch_buffer_append(expr->fault ? expr->fault->name : "null");
			return;
		case CONST_ENUM:
			scratch_buffer_append(expr->enum_val->name);
			return;
		case CONST_TYPEID:
			scratch_buffer_append(expr->typeid->name);
			return;
		case CONST_MEMBER:
			scratch_buffer_append(expr->member.decl->name);
			return;
		case CONST_SLICE:
			if (!expr->slice_init)
			{
				scratch_buffer_append("{}");
				return;
			}
			const_init_to_scratch_buffer(expr->slice_init);
			return;
		case CONST_INITIALIZER:
			const_init_to_scratch_buffer(expr->initializer);
			return;
		case CONST_UNTYPED_LIST:
		{
			if (!vec_size(expr->untyped_list))
			{
				scratch_buffer_append("{}");
				return;
			}
			scratch_buffer_append("{ ");
			FOREACH_IDX(i, Expr *, e, expr->untyped_list)
			{
				if (i != 0) scratch_buffer_append(", ");
				expr_const_to_scratch_buffer(&e->const_expr);
			}
			scratch_buffer_append(" }");
			return;
		}
	}
	UNREACHABLE_VOID
}
const char *expr_const_to_error_string(const ExprConst *expr)
{
	switch (expr->const_kind)
	{
		case CONST_POINTER:
			if (!expr->ptr) return "null";
			return str_printf("%p", (void *)(intptr_t)expr->ptr);
		case CONST_BOOL:
			return expr->b ? "true" : "false";
		case CONST_INTEGER:
			return int_to_str(expr->ixx, expr->is_hex ? 16 : 10, true);
		case CONST_FLOAT:
			return str_printf("%g", expr->fxx.f);
		case CONST_STRING:
			return str_printf("\"%*.s\"", (int)expr->bytes.len, expr->bytes.ptr);
		case CONST_BYTES:
			return "<binary data>";
		case CONST_REF:
			return expr->global_ref->name;
		case CONST_FAULT:
			return expr->fault ? expr->fault->name : "null";
		case CONST_ENUM:
			return expr->enum_val->name;
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





