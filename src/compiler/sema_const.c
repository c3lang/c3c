// Copyright (c) 2024 Christoffer Lerno and contributors. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "sema_internal.h"

static bool sema_append_const_array_one(SemaContext *context, Expr *expr, Expr *list, Expr *element);
static bool sema_append_concat_const_bytes(SemaContext *context, Expr *expr, Expr *list, Expr *element, bool is_append);

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
		return expr->make_slice_expr.len;
	}
	ConstInitializer *init;
	switch (expr->const_expr.const_kind)
	{
		case CONST_FLOAT:
		case CONST_INTEGER:
		case CONST_BOOL:
		case CONST_ENUM:
		case CONST_ERR:
		case CONST_TYPEID:
		case CONST_MEMBER:
		case CONST_REF:
			return -1;
		case CONST_POINTER:
			return -1;
		case CONST_BYTES:
		case CONST_STRING:
			return expr->const_expr.bytes.len;
		case CONST_SLICE:
			if (!expr->const_expr.slice_init) return 0;
			init = expr->const_expr.slice_init;
			goto ARRAY_LEN;
		case CONST_INITIALIZER:
			init = expr->const_expr.initializer;
			goto ARRAY_LEN;
		case CONST_UNTYPED_LIST:
			return vec_size(expr->const_expr.untyped_list);
	}
	UNREACHABLE
ARRAY_LEN:
	if (type_is_arraylike(init->type)) return init->type->array.len;
	return -1;
}

static inline bool sema_expr_const_append(SemaContext *context, Expr *append_expr, Expr *list, Expr *element)
{
	Expr **untyped_list = NULL;
	switch (list->const_expr.const_kind)
	{
		case CONST_INITIALIZER:
		case CONST_SLICE:
			ASSERT0(list->type != type_untypedlist);
			return sema_append_const_array_one(context, append_expr, list, element);
		case CONST_UNTYPED_LIST:
			untyped_list = list->const_expr.untyped_list;
			break;
		case CONST_POINTER:
		case CONST_BYTES:
		case CONST_STRING:
			return sema_append_concat_const_bytes(context, append_expr, list, element, true);
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

static bool sema_concat_const_bytes(SemaContext *context, Expr *expr, Type *type, bool is_bytes, const char *a, const char *b, ArraySize alen, ArraySize blen)
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
	return true;
}

static bool sema_concat_character(SemaContext *context, Expr *expr, Type *type, const char *a, ArraySize alen, uint64_t c)
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
	return sema_concat_const_bytes(context, expr, type, false, a, append_array, alen, len);
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
				return sema_concat_const_bytes(context, expr, left->type, true, left_bytes, &c, len, 1);
			}
			// 1. String + character => String
			if (int_is_neg(right->const_expr.ixx) || int_icomp(right->const_expr.ixx, 0x10FFFF, BINARYOP_GT))
			{
				RETURN_SEMA_ERROR(right, "Cannot concatenate a string with an non-unicode value.");
			}
			return sema_concat_character(context, expr, left->type, left_bytes, len, right->const_expr.ixx.i.low);
		case CONST_FLOAT:
		case CONST_BOOL:
		case CONST_ENUM:
		case CONST_ERR:
		case CONST_POINTER:
		case CONST_TYPEID:
		case CONST_MEMBER:
		case CONST_REF:
			RETURN_SEMA_ERROR(expr, "Concatenating %s with %s is not possible at compile time.",
			                  type_quoted_error_string(left->type), type_to_error_string(right->type));
		case CONST_BYTES:
		case CONST_STRING:
			return sema_concat_const_bytes(context, expr, left->type, is_bytes, left_bytes,
			                               right->const_expr.bytes.ptr,
			                               len, right->const_expr.bytes.len);
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
static bool sema_append_concat_const_bytes(SemaContext *context, Expr *expr, Expr *list, Expr *element, bool is_append)
{
	Type *indexed = type_get_indexed_type(list->type);
	ASSERT0(indexed && "This should always work");
	if (is_append && !cast_implicit(context, element, indexed, false)) return false;
	size_t str_len = list->const_expr.bytes.len;
	size_t element_len = is_append ? 1 : element->const_expr.bytes.len;
	bool is_bytes = list->const_expr.const_kind == CONST_BYTES;
	char *data = malloc_arena(str_len + element_len + 1);
	char *current = data;
	if (str_len) memcpy(current, list->const_expr.bytes.ptr, str_len);
	current += str_len;
	if (is_append)
	{
		current[0] = (unsigned char)element->const_expr.ixx.i.low;
	}
	else
	{
		if (element_len) memcpy(current, element->const_expr.bytes.ptr, element_len);
	}
	current += element_len;
	current[0] = '\0';
	expr->expr_kind = EXPR_CONST;
	expr->const_expr = (ExprConst) {
			.const_kind = list->const_expr.const_kind,
			.bytes.ptr = data,
			.bytes.len = str_len + element_len
	};
	expr->resolve_status = RESOLVE_DONE;
	expr->type = is_bytes ? type_get_array(indexed, str_len + element_len) : list->type;
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
	ASSERT0(!type_is_inferred(array_type));
	bool is_vector = array_type->type_kind == TYPE_VECTOR;
	ConstInitializer *init = is_slice ? list->const_expr.slice_init : list->const_expr.initializer;
	unsigned len = sema_len_from_const(list) + 1;
	Type *indexed = type_get_indexed_type(init->type);
	if (!cast_implicit(context, element, indexed, false)) return false;
	Type *new_inner_type = is_vector ? type_get_vector(indexed, len) : type_get_array(indexed, len);
	Type *new_outer_type = list->type;
	if (!is_slice)
	{
		Type *outer_indexed = type_get_indexed_type(init->type);
		new_outer_type = is_vector ? type_get_vector(outer_indexed, len) : type_get_array(outer_indexed, len);
	}
	switch (init->kind)
	{
		case CONST_INIT_ZERO:
		{
			init->kind = CONST_INIT_ARRAY;
			init->type = new_inner_type;
			ConstInitializer **inits = NULL;
			vec_add(inits, const_init_new_array_value(element, len - 1));
			init->init_array.elements = inits;
			expr_replace(expr, list);
			expr->type = new_outer_type;
			break;
		}
		case CONST_INIT_ARRAY:
			init->type = new_inner_type;
			vec_add(init->init_array.elements, const_init_new_array_value(element, len - 1));
			expr_replace(expr, list);
			expr->type = new_outer_type;
			break;
		case CONST_INIT_ARRAY_FULL:
			init->type = new_inner_type;
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
	ASSERT(expr, expr->expr_kind == EXPR_CONST);
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
 * 3. Vector/slice/array + arraylike => vector/array iff canoncial match, otherwise Untyped list
 * 4. Untyped list + Vector/array/slice => Merged untyped list
 * 5. Vector/array/slice + element => Vector/array/slice + 1 len iff canonical match, Untyped list otherwise
 * 6. Untyped list + element => Untyped list
 */
bool sema_expr_analyse_ct_concat(SemaContext *context, Expr *concat_expr, Expr *left, Expr *right)
{
	ASSERT0(concat_expr->resolve_status == RESOLVE_RUNNING);
	bool join_single = false;
	ArraySize len = 0;
	bool use_array = true;
	Type *indexed_type = NULL;
	if (!sema_analyse_expr(context, left)) return false;
	if (!sema_cast_const(left)) RETURN_SEMA_ERROR(left, "Expected this to evaluate to a constant value.");
	if (!sema_analyse_expr(context, right)) return false;
	if (!sema_cast_const(right)) RETURN_SEMA_ERROR(left, "Expected this to evaluate to a constant value.");
	Type *element_type = left->type->canonical;
	Type *right_type = right->type->canonical;
	switch (left->const_expr.const_kind)
	{
		case CONST_POINTER:
		case CONST_FLOAT:
		case CONST_INTEGER:
		case CONST_BOOL:
		case CONST_ENUM:
		case CONST_ERR:
		case CONST_TYPEID:
		case CONST_REF:
		case CONST_MEMBER:
			RETURN_SEMA_ERROR(left, "Only bytes, strings and list-like constants can be concatenated.");
		case CONST_BYTES:
		case CONST_STRING:
			return sema_concat_bytes_and_other(context, concat_expr, left, right);
		case CONST_SLICE:
		case CONST_INITIALIZER:
			switch (type_flatten(element_type)->type_kind)
			{
				case TYPE_VECTOR:
					use_array = false;
					break;
				case TYPE_INFERRED_VECTOR:
				case TYPE_INFERRED_ARRAY:
					UNREACHABLE
				case TYPE_SLICE:
				case TYPE_ARRAY:
					break;
				case TYPE_STRUCT:
				case TYPE_UNION:
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
	switch (right->const_expr.const_kind)
	{
		case CONST_FLOAT:
		case CONST_INTEGER:
		case CONST_BOOL:
		case CONST_ENUM:
		case CONST_ERR:
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
	len += sema_len_from_const(right);
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
			if (init && init->kind != CONST_INIT_ARRAY_FULL)
			{
				ASSERT0(!init || init->type != type_untypedlist);
				RETURN_SEMA_ERROR(single_expr, "Expected a full array here.");
			}
			FOREACH(ConstInitializer *, val, init->init_array_full)
			{
				vec_add(untyped_exprs, val->init_value);
			}
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
	ConstInitializer **inits = VECNEW(ConstInitializer*, len);
	Expr *exprs[2] = { left, right };
	for (int i = 0; i < 2; i++)
	{
		Expr *element = exprs[i];
		ConstInitializer *inititializer = expr_const_initializer_from_expr(element);
		// Zero sized slice:
		if (!inititializer)
		{
			ASSERT(element, element->const_expr.const_kind == CONST_SLICE);
			continue;
		}
		switch (inititializer->kind)
		{
			case CONST_INIT_ARRAY_FULL:
				break;
			case CONST_INIT_ZERO:
				if (type_flatten(element->type)->type_kind == TYPE_SLICE) continue;
			default:
				RETURN_SEMA_ERROR(element, "Only fully initialized arrays may be concatenated.");
		}
		FOREACH(ConstInitializer *, init, inititializer->init_array_full)
		{
			vec_add(inits, init);
		}
	}
	Type *type = use_array ? type_get_array(indexed_type, len) : type_get_vector(indexed_type, len);
	expr_rewrite_const_initializer(concat_expr, type, const_init_new_array_full(type, inits));

	return true;
}
