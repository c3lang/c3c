// Copyright (c) 2024 Christoffer Lerno and contributors. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "sema_internal.h"

static bool sema_append_const_array_one(SemaContext *context, Expr *expr, Expr *list, Expr *element);
static bool sema_append_concat_const_bytes(SemaContext *context, Expr *expr, Expr *list, Expr *element);

ArrayIndex sema_len_from_const(Expr *expr)
{
	// We also handle the case where we have a cast from a const array.
	if (!sema_cast_const(expr))
	{
		if (type_flatten(expr->type)->type_kind != TYPE_SLICE) return -1;
		if (expr->expr_kind == EXPR_SLICE)
		{
			return range_const_len(&expr->slice_expr.range);
		}
		if (expr->expr_kind != EXPR_MAKE_SLICE) return -1;
		return (ArrayIndex)expr->make_slice_expr.len;
	}
	ConstInitializer *init;
	switch (expr->const_expr.const_kind)
	{
		case CONST_FLOAT:
		case CONST_INTEGER:
		case CONST_BOOL:
		case CONST_ENUM:
		case CONST_FAULT:
		case CONST_TYPEID:
		case CONST_MEMBER:
		case CONST_REF:
			return -1;
		case CONST_POINTER:
			return -1;
		case CONST_BYTES:
		case CONST_STRING:
			return (ArrayIndex)expr->const_expr.bytes.len;
		case CONST_SLICE:
			if (!expr->const_expr.slice_init) return 0;
			init = expr->const_expr.slice_init;
			goto ARRAY_LEN;
		case CONST_INITIALIZER:
			init = expr->const_expr.initializer;
			goto ARRAY_LEN;
		case CONST_UNTYPED_LIST:
			return (ArrayIndex)vec_size(expr->const_expr.untyped_list);
	}
	UNREACHABLE
ARRAY_LEN:
	if (type_is_arraylike(init->type)) return (ArrayIndex)init->type->array.len;
	return -1;
}

static inline bool sema_expr_const_append(SemaContext *context, Expr *append_expr, Expr *list, Expr *element)
{
	Expr **untyped_list = NULL;
	switch (list->const_expr.const_kind)
	{
		case CONST_INITIALIZER:
		case CONST_SLICE:
			ASSERT(list->type != type_untypedlist);
			return sema_append_const_array_one(context, append_expr, list, element);
		case CONST_UNTYPED_LIST:
			untyped_list = list->const_expr.untyped_list;
			break;
		case CONST_POINTER:
		case CONST_BYTES:
		case CONST_STRING:
			return sema_append_concat_const_bytes(context, append_expr, list, element);
		default:
			RETURN_SEMA_ERROR(list, "Expected some kind of list or vector here.");
	}
	vec_add(untyped_list, element);
	append_expr->expr_kind = EXPR_CONST;
	append_expr->const_expr = (ExprConst) { .untyped_list = untyped_list, .const_kind = CONST_UNTYPED_LIST };
	append_expr->type = type_untypedlist;
	append_expr->resolve_status = RESOLVE_DONE;
	return true;
}

static void expr_concat_const_bytes(Expr *expr, Type *type, bool is_bytes, const char *a, const char *b, ArraySize alen, ArraySize blen)
{
	Type *indexed = type_get_indexed_type(type);
	char *data = malloc_arena(alen + blen + 1);
	char *current = data;
	if (alen) memcpy(current, a, alen);
	current += alen;
	if (blen) memcpy(current, b, blen);
	current += blen;
	current[0] = '\0';
	expr->expr_kind = EXPR_CONST;
	expr->const_expr = (ExprConst) {
			.const_kind = is_bytes ? CONST_BYTES : CONST_STRING,
			.bytes.ptr = data,
			.bytes.len = alen + blen
	};
	expr->resolve_status = RESOLVE_DONE;
	expr->type = is_bytes ? type_get_array(indexed, alen + blen) : type;
}

static void expr_concat_character(Expr *expr, Type *type, const char *a, ArraySize alen, uint64_t c)
{
	char append_array[4];
	int len;
	if (c <= 0x7f)
	{
		append_array[0] = (char) c;
		len = 1;
	}
	else if (c <= 0x7ff)
	{
		append_array[0] = (char) (0xC0 | (c >> 6));
		append_array[1] = (char) (0x80 | (c & 0x3F));
		len = 2;
	}
	else if (c <= 0xffff)
	{
		append_array[0] = (char) (0xE0 | (c >> 12));
		append_array[1] = (char) (0x80 | ((c >> 6) & 0x3F));
		append_array[2] = (char) (0x80 | (c & 0x3F));
		len = 3;
	}
	else
	{
		append_array[0] = (char) (0xF0 | (c >> 18));
		append_array[1] = (char) (0x80 | ((c >> 12) & 0x3F));
		append_array[2] = (char) (0x80 | ((c >> 6) & 0x3F));
		append_array[3] = (char) (0x80 | (c & 0x3F));
		len = 4;
	}
	expr_concat_const_bytes(expr, type, false, a, append_array, alen, len);
}

/**
 * 1. String + Bytes|String
 * 2. Bytes + Bytes|String
 * 3. Bytes + (i)char => Bytes
 * 4. String + character => String
 * 5. String + (i)char array/vector => String // Disallowed for now
 * 6. Bytes + (i)char array/vector => Bytes   // Disallowed for now
 */
static bool sema_concat_bytes_and_other(SemaContext *context, Expr *expr, Expr *left, Expr *right)
{
	ArraySize len = left->const_expr.bytes.len;
	bool is_bytes = left->const_expr.const_kind == CONST_BYTES;
	Type *indexed = type_get_indexed_type(left->type);
	const char *left_bytes = left->const_expr.bytes.ptr;
	RETRY:;
	switch (right->const_expr.const_kind)
	{
		case CONST_INTEGER:
			if (is_bytes)
			{
				// 2. Bytes + (i)char => Bytes
				if (!cast_implicit(context, right, type_char, false)) return false;
				char c = (char) int_to_i64(right->const_expr.ixx);
				expr_concat_const_bytes(expr, left->type, true, left_bytes, &c, len, 1);
				return true;
			}
			// 1. String + character => String
			if (int_is_neg(right->const_expr.ixx) || int_icomp(right->const_expr.ixx, 0x10FFFF, BINARYOP_GT))
			{
				RETURN_SEMA_ERROR(right, "Cannot concatenate a string with an non-unicode value.");
			}
			expr_concat_character(expr, left->type, left_bytes, len, right->const_expr.ixx.i.low);
			return true;
		case CONST_FLOAT:
		case CONST_BOOL:
		case CONST_ENUM:
		case CONST_FAULT:
		case CONST_POINTER:
		case CONST_TYPEID:
		case CONST_MEMBER:
		case CONST_REF:
			RETURN_SEMA_ERROR(expr, "Concatenating %s with %s is not possible at compile time.",
			                  type_quoted_error_string(left->type), type_to_error_string(right->type));
		case CONST_BYTES:
		case CONST_STRING:
			expr_concat_const_bytes(expr, left->type, is_bytes, left_bytes, right->const_expr.bytes.ptr,
				len, right->const_expr.bytes.len);
			return true;
		case CONST_UNTYPED_LIST:
			if (!cast_implicit(context, right, type_get_inferred_array(indexed), false)) return false;
			goto RETRY;
		case CONST_SLICE:
		case CONST_INITIALIZER:
			if (!cast_implicit(context, right, type_get_inferred_array(indexed), false)) return false;
			expr_contract_array(&right->const_expr, left->const_expr.const_kind);
			goto RETRY;
	}
	UNREACHABLE
}
static bool sema_append_concat_const_bytes(SemaContext *context, Expr *expr, Expr *list, Expr *element)
{
	Type *indexed = type_get_indexed_type(list->type);
	ASSERT(indexed && "This should always work");
	if (!cast_implicit(context, element, indexed, false)) return false;
	size_t str_len = list->const_expr.bytes.len;
	bool is_bytes = list->const_expr.const_kind == CONST_BYTES;
	char *data = malloc_arena(str_len + 2);
	char *current = data;
	if (str_len) memcpy(current, list->const_expr.bytes.ptr, str_len);
	current += str_len;
	current[0] = (char)element->const_expr.ixx.i.low;
	current[1] = '\0';
	expr->expr_kind = EXPR_CONST;
	expr->const_expr = (ExprConst) {
			.const_kind = list->const_expr.const_kind,
			.bytes.ptr = data,
			.bytes.len = str_len + 1
	};
	expr->resolve_status = RESOLVE_DONE;
	expr->type = is_bytes ? type_get_array(indexed, str_len + 1) : list->type;
	return true;
}

static bool sema_append_const_array_one(SemaContext *context, Expr *expr, Expr *list, Expr *element)
{
	Type *array_type = type_flatten(list->type);
	if (expr_is_empty_const_slice(list))
	{
		// Create a single length array
		ConstInitializer *first_element = const_init_new_value(element);
		ConstInitializer **list_of_elements = NULL;
		vec_add(list_of_elements, first_element);
		list->const_expr.slice_init = const_init_new_array_full(type_get_array(array_type->array.base, 1), list_of_elements);
		expr_replace(expr, list);
		return true;
	}
	bool is_slice = list->const_expr.const_kind == CONST_SLICE;
	ASSERT(!type_is_inferred(array_type));
	bool is_vector = type_kind_is_real_vector(array_type->type_kind);
	ConstInitializer *init = is_slice ? list->const_expr.slice_init : list->const_expr.initializer;
	unsigned len = sema_len_from_const(list) + 1;
	Type *indexed = type_get_indexed_type(init->type);
	if (!cast_implicit(context, element, indexed, false)) return false;
	Type *new_inner_type = is_vector ? type_get_vector(indexed, array_type->type_kind, len) : type_get_array(indexed, len);
	Type *new_outer_type = list->type;
	if (!is_slice)
	{
		Type *outer_indexed = type_get_indexed_type(init->type);
		new_outer_type = is_vector ? type_get_vector(outer_indexed,  array_type->type_kind, len) : type_get_array(outer_indexed, len);
	}
	switch (init->kind)
	{
		case CONST_INIT_ZERO:
		{
			init->kind = CONST_INIT_ARRAY;
			const_init_set_type(init, new_inner_type);
			ConstInitializer **inits = NULL;
			vec_add(inits, const_init_new_array_value(element, len - 1));
			init->init_array.elements = inits;
			expr_replace(expr, list);
			expr->type = new_outer_type;
			break;
		}
		case CONST_INIT_ARRAY:
			const_init_set_type(init, new_inner_type);
			vec_add(init->init_array.elements, const_init_new_array_value(element, len - 1));
			expr_replace(expr, list);
			expr->type = new_outer_type;
			break;
		case CONST_INIT_ARRAY_FULL:
			const_init_set_type(init, new_inner_type);
			vec_add(init->init_array_full, const_init_new_value(element));
			expr_replace(expr, list);
			expr->type = new_outer_type;
			break;
		case CONST_INIT_ARRAY_VALUE:
		case CONST_INIT_STRUCT:
		case CONST_INIT_UNION:
		case CONST_INIT_VALUE:
			UNREACHABLE
	}
	return true;
}

static inline ConstInitializer *expr_const_initializer_from_expr(Expr *expr)
{
	ASSERT_SPAN(expr, expr->expr_kind == EXPR_CONST);
	switch (expr->const_expr.const_kind)
	{
		case CONST_SLICE:
			return expr->const_expr.slice_init;
		case CONST_INITIALIZER:
			return expr->const_expr.initializer;
		default:
			UNREACHABLE
	}
}


/**
 * The following valid cases exist:
 *
 * 1. String/Bytes + ... => String/Bytes
 * 2. Vector/slice/array + Untyped list => Merged untyped list
 * 3. Vector/slice/array + arraylike => vector/array iff canonical match, otherwise Untyped list
 * 4. Untyped list + Vector/array/slice => Merged untyped list
 * 5. Vector/array/slice + element => Vector/array/slice + 1 len iff canonical match, Untyped list otherwise
 * 6. Untyped list + element => Untyped list
 */
bool sema_expr_analyse_ct_concat(SemaContext *context, Expr *concat_expr, Expr *left, Expr *right, bool *failed_ref)
{
	ASSERT_SPAN(concat_expr, concat_expr->resolve_status == RESOLVE_RUNNING);
	if (!sema_check_left_right_const(context, left, right)) return false;
	ArraySize len = 0;
	TypeKind vec_type = TYPE_POISONED;
	Type *indexed_type = NULL;
	Type *element_type = left->type->canonical;
	Type *right_type = right->type->canonical;
	switch (left->const_expr.const_kind)
	{
		case CONST_POINTER:
		case CONST_FLOAT:
		case CONST_INTEGER:
		case CONST_BOOL:
		case CONST_ENUM:
		case CONST_FAULT:
		case CONST_TYPEID:
		case CONST_REF:
		case CONST_MEMBER:
			CHECK_ON_DEFINED(failed_ref);
			RETURN_SEMA_ERROR(left, "Only bytes, strings and list-like constants can be concatenated.");
		case CONST_BYTES:
		case CONST_STRING:
			return sema_concat_bytes_and_other(context, concat_expr, left, right);
		case CONST_SLICE:
		case CONST_INITIALIZER:
			switch (type_flatten(element_type)->type_kind)
			{
				case TYPE_SIMD_VECTOR:
					vec_type = TYPE_SIMD_VECTOR;
					break;
				case TYPE_VECTOR:
					vec_type = TYPE_VECTOR;
					break;
				case TYPE_INFERRED_VECTOR:
				case TYPE_INFERRED_ARRAY:
					UNREACHABLE
				case TYPE_SLICE:
				case TYPE_ARRAY:
					break;
				case TYPE_STRUCT:
				case TYPE_UNION:
					CHECK_ON_DEFINED(failed_ref);
					RETURN_SEMA_ERROR(left, "Only bytes, strings and array-like constants can be concatenated.");
				default:
					UNREACHABLE
			}
			len = sema_len_from_const(left);
			indexed_type = type_get_indexed_type(element_type);
			break;
		case CONST_UNTYPED_LIST:
			len = vec_size(left->const_expr.untyped_list);
			break;
	}
	ArraySize len_lhs = len;
	switch (right->const_expr.const_kind)
	{
		case CONST_FLOAT:
		case CONST_INTEGER:
		case CONST_BOOL:
		case CONST_ENUM:
		case CONST_FAULT:
		case CONST_POINTER:
		case CONST_TYPEID:
		case CONST_MEMBER:
		case CONST_REF:
			return sema_expr_const_append(context, concat_expr, left, right);
		case CONST_BYTES:
		case CONST_STRING:
			if (left->type == type_untypedlist || indexed_type == right_type) return sema_expr_const_append(context, concat_expr, left, right);
			if (!type_is_integer(indexed_type) || type_size(indexed_type) != 1)
			{
				CHECK_ON_DEFINED(failed_ref);
				RETURN_SEMA_ERROR(right, "You can't concatenate %s and %s.", type_quoted_error_string(left->type),
				                  type_to_error_string(right_type));
			}
			if (!cast_implicit(context, right, type_get_inferred_array(indexed_type), false)) return false;
			expr_contract_array(&left->const_expr, CONST_BYTES);
			return sema_concat_bytes_and_other(context, concat_expr, left, right);
		case CONST_UNTYPED_LIST:
			break;
		case CONST_SLICE:
		case CONST_INITIALIZER:
			if (indexed_type && cast_implicit_silent(context, right, indexed_type, false))
			{
				return sema_expr_const_append(context, concat_expr, left, right);
			}
			break;
	}
	if (indexed_type && !cast_implicit_silent(context, right, type_get_inferred_array(indexed_type), false))
	{
		indexed_type = NULL;
	}
	ArraySize len_rhs = sema_len_from_const(right);
	len += len_rhs;
	if (!indexed_type)
	{
		Expr **untyped_exprs = VECNEW(Expr*, len + 1);
		Expr *exprs[2] = { left, right };
		for (unsigned i = 0; i < 2; i++)
		{
			Expr *single_expr = exprs[i];
			if (expr_is_const_untyped_list(single_expr))
			{
				FOREACH(Expr *, expr_untyped, single_expr->const_expr.untyped_list)
				{
					vec_add(untyped_exprs, expr_untyped);
				}
				continue;
			}
			ConstInitializer *init = expr_const_initializer_from_expr(single_expr);
			// Skip zero arrays from slices.
			if (!init) continue;
			switch (init->kind)
			{
				case CONST_INIT_UNION:
				case CONST_INIT_STRUCT:
				case CONST_INIT_ARRAY_VALUE:
				case CONST_INIT_VALUE:
					UNREACHABLE
				case CONST_INIT_ARRAY_FULL:
				{
					FOREACH(ConstInitializer *, val, init->init_array_full)
					{
						vec_add(untyped_exprs, val->init_value);
					}
					continue;
				}
				case CONST_INIT_ZERO:
				{
					Type *index_type = type_get_indexed_type(type_flatten(init->type));
					ArraySize len_zero = i == 0 ? len_lhs : len_rhs;
					for (ArraySize j = 0; j < len_zero; j++)
					{
						Expr *zero = expr_new_expr(EXPR_CONST, single_expr);
						expr_rewrite_to_const_zero(zero, index_type);
						vec_add(untyped_exprs, zero);
					}
					continue;
				}
				case CONST_INIT_ARRAY:
				{
					Type *index_type = type_get_indexed_type(type_flatten(init->type));
					ArraySize len_zero = i == 0 ? len_lhs : len_rhs;
					ArraySize offset = vec_size(untyped_exprs);
					for (ArraySize j = 0; j < len_zero; j++)
					{
						Expr *zero = expr_new_expr(EXPR_CONST, single_expr);
						expr_rewrite_to_const_zero(zero, index_type);
						vec_add(untyped_exprs, zero);
					}
					FOREACH(ConstInitializer *, element, init->init_array.elements)
					{
						ASSERT_SPAN(right, element->kind == CONST_INIT_ARRAY_VALUE);
						ConstInitializer *inner_element = element->init_array_value.element;
						Expr *inner_expr = untyped_exprs[offset + element->init_array_value.index];
						switch (inner_element->kind)
						{
							case CONST_INIT_ZERO:
								break;
							case CONST_INIT_STRUCT:
							case CONST_INIT_UNION:
							case CONST_INIT_ARRAY:
							case CONST_INIT_ARRAY_FULL:
							case CONST_INIT_ARRAY_VALUE:
								expr_rewrite_const_initializer(inner_expr, index_type, inner_element);
								break;
							case CONST_INIT_VALUE:
								*inner_expr = *inner_element->init_value;
								break;
						}
					}
					continue;
				}
			}
			UNREACHABLE
		}
		concat_expr->expr_kind = EXPR_CONST;
		concat_expr->type = type_untypedlist;
		concat_expr->resolve_status = RESOLVE_DONE;
		concat_expr->const_expr = (ExprConst) {
				.const_kind = CONST_UNTYPED_LIST,
				.untyped_list = untyped_exprs
		};

		return true;
	}
	// Zero slice + zero slice => slice
	if (len == 0)
	{
		expr_rewrite_to_const_zero(concat_expr, type_get_slice(indexed_type));
	}
	Type *type = vec_type == TYPE_POISONED ? type_get_array(indexed_type, len) : type_get_vector(indexed_type, vec_type, len);
	ConstInitializer *lhs_init = expr_const_initializer_from_expr(left);
	ConstInitializer *rhs_init = expr_const_initializer_from_expr(right);
	if (!rhs_init)
	{
		rhs_init = lhs_init;
		lhs_init = NULL;
	}
	if (!lhs_init)
	{
		ASSERT(rhs_init);
		expr_rewrite_const_initializer(concat_expr, type, rhs_init);
		return true;
	}

	switch (lhs_init->kind)
	{
		case CONST_INIT_UNION:
		case CONST_INIT_STRUCT:
		case CONST_INIT_ARRAY_VALUE:
		case CONST_INIT_VALUE:
			UNREACHABLE
		case CONST_INIT_ZERO:
			switch (rhs_init->kind)
			{
				case CONST_INIT_ZERO:
					// { 0, 0, 0 } + { 0, 0, 0 }
					expr_rewrite_const_initializer(concat_expr, type, const_init_new_zero(type));
					return true;
				case CONST_INIT_ARRAY_FULL:
				{
					// { 0, 0, 0 } + { 1, 2, 3 }
					ConstInitializer **inits = VECNEW(ConstInitializer*, len);
					for (ArraySize i = 0; i < len_lhs; i++)
					{
						vec_add(inits, const_init_new_zero(indexed_type));
					}
					FOREACH(ConstInitializer *, init, rhs_init->init_array_full)
					{
						vec_add(inits, init);
					}
					expr_rewrite_const_initializer(concat_expr, type, const_init_new_array_full(type, inits));
					return true;
				}
				case CONST_INIT_ARRAY:
				{
					// { 0, 0, 0 } + { 1 => 3 }
					FOREACH(ConstInitializer *, element, rhs_init->init_array.elements)
					{
						ASSERT_SPAN(right, element->kind == CONST_INIT_ARRAY_VALUE);
						element->init_array_value.index += (ArrayIndex)len_lhs;
					}
					const_init_set_type(rhs_init, type);
					expr_rewrite_const_initializer(concat_expr, type, rhs_init);
					return true;
				}
				case CONST_INIT_STRUCT:
				case CONST_INIT_UNION:
				case CONST_INIT_VALUE:
				case CONST_INIT_ARRAY_VALUE:
					UNREACHABLE
			}
			UNREACHABLE
		case CONST_INIT_ARRAY_FULL:
		{
			switch (rhs_init->kind)
			{
				case CONST_INIT_UNION:
				case CONST_INIT_STRUCT:
				case CONST_INIT_ARRAY_VALUE:
				case CONST_INIT_VALUE:
					UNREACHABLE
				case CONST_INIT_ZERO:
				{
					// { 1, 2, 3 } + { 0, 0, 0 }
					ConstInitializer **inits = VECNEW(ConstInitializer*, len);
					FOREACH(ConstInitializer *, init, lhs_init->init_array_full)
					{
						vec_add(inits, init);
					}
					for (ArraySize i = 0; i < len_rhs; i++)
					{
						vec_add(inits, const_init_new_zero(indexed_type));
					}
					expr_rewrite_const_initializer(concat_expr, type, const_init_new_array_full(type, inits));
					return true;
				}
				case CONST_INIT_ARRAY_FULL:
				{
					// { 1, 2, 3 } + { 1, 2, 3 }
					ConstInitializer **inits = VECNEW(ConstInitializer*, len);
					FOREACH(ConstInitializer *, init, lhs_init->init_array_full)
					{
						vec_add(inits, init);
					}
					FOREACH(ConstInitializer *, init, rhs_init->init_array_full)
					{
						vec_add(inits, init);
					}
					expr_rewrite_const_initializer(concat_expr, type, const_init_new_array_full(type, inits));
					return true;
				}
				case CONST_INIT_ARRAY:
				{
					// { 1, 2, 3 } + { 1 => 3 }
					ConstInitializer **inits = VECNEW(ConstInitializer*, len);
					FOREACH(ConstInitializer *, init, lhs_init->init_array_full)
					{
						vec_add(inits, init);
					}
					for (ArraySize i = 0; i < len_rhs; i++)
					{
						vec_add(inits, const_init_new_zero(indexed_type));
					}
					FOREACH(ConstInitializer *, element, rhs_init->init_array.elements)
					{
						ASSERT_SPAN(right, element->kind == CONST_INIT_ARRAY_VALUE);
						inits[element->init_array_value.index + len_lhs] = element->init_array_value.element;
					}
					expr_rewrite_const_initializer(concat_expr, type, const_init_new_array_full(type, inits));
					return true;
				}
			}
			UNREACHABLE
		}
		case CONST_INIT_ARRAY:
		{
			switch (rhs_init->kind)
			{
				case CONST_INIT_UNION:
				case CONST_INIT_STRUCT:
				case CONST_INIT_ARRAY_VALUE:
				case CONST_INIT_VALUE:
					UNREACHABLE
				case CONST_INIT_ZERO:
				{
					// { 1 => 3 } + { 0, 0, 0 }
					const_init_set_type(lhs_init, type);
					expr_rewrite_const_initializer(concat_expr, type, lhs_init);
					return true;
				}
				case CONST_INIT_ARRAY_FULL:
				{
					// { 1 => 3 } + { 1, 2, 3 }
					ConstInitializer **inits = VECNEW(ConstInitializer*, len);
					for (ArraySize i = 0; i < len_lhs; i++)
					{
						vec_add(inits, const_init_new_zero(indexed_type));
					}
					FOREACH(ConstInitializer *, element, lhs_init->init_array.elements)
					{
						ASSERT_SPAN(left, element->kind == CONST_INIT_ARRAY_VALUE);
						inits[element->init_array_value.index] = element->init_array_value.element;
					}
					FOREACH(ConstInitializer *, init, rhs_init->init_array_full)
					{
						vec_add(inits, init);
					}
					expr_rewrite_const_initializer(concat_expr, type, const_init_new_array_full(type, inits));
					return true;
				}
				case CONST_INIT_ARRAY:
				{
					// { 1 => 3 } + { 1 => 3 }
					ArraySize elements = vec_size(lhs_init->init_array.elements) + vec_size(rhs_init->init_array.elements);
					ConstInitializer **inits = VECNEW(ConstInitializer*, elements);
					FOREACH(ConstInitializer *, element, lhs_init->init_array.elements)
					{
						vec_add(inits, element);
					}
					FOREACH(ConstInitializer *, element, rhs_init->init_array.elements)
					{
						ASSERT_SPAN(right, element->kind == CONST_INIT_ARRAY_VALUE);
						element->init_array_value.index += (ArrayIndex)len_lhs;
						vec_add(inits, element);
					}
					expr_rewrite_const_initializer(concat_expr, type, const_init_new_array(type, inits));
					return true;
				}
			}
			UNREACHABLE
		}
	}
	UNREACHABLE
}
