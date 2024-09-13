// Copyright (c) 2024 Christoffer Lerno and contributors. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "sema_internal.h"
static inline ArraySize sema_get_const_len(SemaContext *context, Expr *expr);
static bool sema_append_const_array_one(SemaContext *context, Expr *expr, Expr *list, Expr *element);
static ArrayIndex len_from_const_initializer(ConstInitializer *init);
static bool sema_append_concat_const_bytes(SemaContext *context, Expr *expr, Expr *list, Expr *element, bool is_append);


typedef enum
{
	CONCAT_UNKNOWN,
	CONCAT_JOIN_BYTES,
	CONCAT_JOIN_ARRAYS,
	CONCAT_JOIN_LISTS,
} ConcatType;


static ArrayIndex len_from_const_initializer(ConstInitializer *init)
{
	switch (init->kind)
	{
		case CONST_INIT_ZERO:
			return 0;
		case CONST_INIT_STRUCT:
		case CONST_INIT_UNION:
		case CONST_INIT_VALUE:
		case CONST_INIT_ARRAY_VALUE:
			return -1;
		case CONST_INIT_ARRAY:
		{
			ArrayIndex max = 0;
			FOREACH(ConstInitializer *, element, init->init_array.elements)
			{
				assert(element->kind == CONST_INIT_ARRAY_VALUE);
				if (element->init_array_value.index > max) max = element->init_array_value.index;
			}
			return max;
		}
		case CONST_INIT_ARRAY_FULL:
			return vec_size(init->init_array_full);
	}
	UNREACHABLE
}

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
		if (expr->expr_kind != EXPR_CAST) return -1;
		if (expr->cast_expr.kind != CAST_APTSA) return -1;
		Expr *inner = exprptr(expr->cast_expr.expr);
		if (inner->expr_kind != EXPR_UNARY || inner->unary_expr.operator != UNARYOP_ADDR) return -1;
		inner = inner->unary_expr.expr;
		if (!sema_cast_const(inner)) return -1;
		expr = inner;
	}
	switch (expr->const_expr.const_kind)
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
		case CONST_BYTES:
		case CONST_STRING:
			return expr->const_expr.bytes.len;
		case CONST_INITIALIZER:
			return len_from_const_initializer(expr->const_expr.initializer);
		case CONST_UNTYPED_LIST:
			return vec_size(expr->const_expr.untyped_list);
	}
	UNREACHABLE
}

static inline bool sema_expr_const_append(SemaContext *context, Expr *append_expr, Expr *list, Expr *element)
{
	Expr **untyped_list = NULL;
	switch (list->const_expr.const_kind)
	{
		case CONST_INITIALIZER:
			assert(list->type != type_untypedlist);
			return sema_append_const_array_one(context, append_expr, list, element);
		case CONST_UNTYPED_LIST:
			untyped_list = list->const_expr.untyped_list;
			break;
		case CONST_POINTER:
			if (list->type->canonical->type_kind == TYPE_SLICE)
			{
				return sema_append_const_array_one(context, append_expr, list, element);
			}
			FALLTHROUGH;
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
	assert(indexed && "This should always work");
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

// TODO there is also another const_len... look at that.
static inline ArraySize sema_get_const_len(SemaContext *context, Expr *expr)
{
	switch (expr->const_expr.const_kind)
	{
		case CONST_FLOAT:
		case CONST_INTEGER:
		case CONST_BOOL:
		case CONST_ENUM:
		case CONST_ERR:
		case CONST_POINTER:
		case CONST_TYPEID:
		case CONST_REF:
			return 1;
		case CONST_BYTES:
		case CONST_STRING:
			return expr->const_expr.bytes.len;
		case CONST_INITIALIZER:
		{
			bool may_be_array;
			bool is_const_size;
			ArrayIndex len = (ArraySize) sema_get_initializer_const_array_size(context, expr, &may_be_array,
			                                                                   &is_const_size);
			assert(is_const_size);
			return (ArraySize) len;
		}
		case CONST_UNTYPED_LIST:
			return vec_size(expr->const_expr.untyped_list);
		case CONST_MEMBER:
			return 1;
	}
	UNREACHABLE
}

static bool sema_append_const_array_one(SemaContext *context, Expr *expr, Expr *list, Expr *element)
{
	bool is_empty_slice = list->const_expr.const_kind == CONST_POINTER;
	if (!is_empty_slice && list->const_expr.initializer->kind != CONST_INIT_ARRAY_FULL)
	{
		RETURN_SEMA_ERROR(list, "Only fully initialized arrays may be appended to.");
	}
	Type *array_type = type_flatten(list->type);
	bool is_vector = array_type->type_kind == TYPE_VECTOR || array_type->type_kind == TYPE_INFERRED_VECTOR;
	unsigned len = (is_empty_slice ? 0 : sema_get_const_len(context, list)) + 1;
	Type *indexed = type_get_indexed_type(list->type);
	Type *new_type = is_vector ? type_get_vector(indexed, len) : type_get_array(indexed, len);
	ConstInitializer *init = list->const_expr.initializer;
	ConstInitializer **inits = VECNEW(ConstInitializer*, len);
	if (!is_empty_slice)
	{
		FOREACH(ConstInitializer *, i, init->init_array_full) vec_add(inits, i);
	}

	assert(element->resolve_status == RESOLVE_DONE);
	if (!cast_implicit(context, element, indexed, false)) return false;
	ConstInitializer *in = CALLOCS(ConstInitializer);
	in->kind = CONST_INIT_VALUE;
	in->init_value = element;
	vec_add(inits, in);
	expr->expr_kind = EXPR_CONST;
	expr->resolve_status = RESOLVE_DONE;
	expr->type = new_type;
	ConstInitializer *new_init = CALLOCS(ConstInitializer);
	new_init->init_array_full = inits;
	new_init->type = new_type;
	new_init->kind = CONST_INIT_ARRAY_FULL;
	expr->const_expr = (ExprConst) {
			.const_kind = CONST_INITIALIZER,
			.initializer = new_init
	};
	return true;
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
	assert(concat_expr->resolve_status == RESOLVE_RUNNING);
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
	ConstKind right_kind = right->const_expr.const_kind;
	switch (left->const_expr.const_kind)
	{
		case CONST_POINTER:
			if (element_type->type_kind == TYPE_SLICE)
			{
				len = 0;
				indexed_type = type_get_indexed_type(element_type);
				break;
			}
			FALLTHROUGH;
		case CONST_FLOAT:
		case CONST_INTEGER:
		case CONST_BOOL:
		case CONST_ENUM:
		case CONST_ERR:
		case CONST_TYPEID:
		case CONST_REF:
			RETURN_SEMA_ERROR(left, "Only bytes, strings and list-like constants can be concatenated.");
		case CONST_BYTES:
		case CONST_STRING:
			return sema_concat_bytes_and_other(context, concat_expr, left, right);
		case CONST_INITIALIZER:
			switch (type_flatten(element_type)->type_kind)
			{
				case TYPE_VECTOR:
				case TYPE_INFERRED_VECTOR:
					use_array = false;
					FALLTHROUGH;
				case TYPE_SLICE:
				case TYPE_ARRAY:
				case TYPE_INFERRED_ARRAY:
				{
					switch (left->const_expr.initializer->kind)
					{
						case CONST_INIT_ARRAY_FULL:
							break;
						case CONST_INIT_ZERO:
							if (type_flatten(element_type)->type_kind == TYPE_SLICE) break;
							FALLTHROUGH;
						default:
							RETURN_SEMA_ERROR(left, "Only fully initialized arrays may be concatenated.");
					}
					indexed_type = type_get_indexed_type(element_type);
					assert(indexed_type);
					len = sema_get_const_len(context, left);
					break;
				}
				case TYPE_UNTYPED_LIST:
					UNREACHABLE
				default:
					RETURN_SEMA_ERROR(left, "Only bytes, strings and array-like constants can be concatenated.");
			}
			break;
		case CONST_UNTYPED_LIST:
			len = vec_size(left->const_expr.untyped_list);
			break;
		case CONST_MEMBER:
			RETURN_SEMA_ERROR(left, "This can't be concatenated.");
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
	len += sema_get_const_len(context, right);
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
			ConstInitializer *init = single_expr->const_expr.initializer;
			if (init->kind != CONST_INIT_ARRAY_FULL)
			{
				assert(init->type != type_untypedlist);
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
		assert(element->const_expr.const_kind == CONST_INITIALIZER);
		ConstInitType init_type = element->const_expr.initializer->kind;
		switch (init_type)
		{
			case CONST_INIT_ARRAY_FULL:
				break;
			case CONST_INIT_ZERO:
				if (type_flatten(element->type)->type_kind == TYPE_SLICE) continue;
			default:
				RETURN_SEMA_ERROR(element, "Only fully initialized arrays may be concatenated.");
		}
		FOREACH(ConstInitializer *, init, element->const_expr.initializer->init_array_full)
		{
			vec_add(inits, init);
		}
	}
	concat_expr->expr_kind = EXPR_CONST;
	concat_expr->resolve_status = RESOLVE_DONE;
	concat_expr->type = use_array ? type_get_array(indexed_type, len) : type_get_vector(indexed_type, len);
	ConstInitializer *new_init = CALLOCS(ConstInitializer);
	new_init->init_array_full = inits;
	new_init->type = concat_expr->type;
	new_init->kind = CONST_INIT_ARRAY_FULL;
	concat_expr->const_expr = (ExprConst) {
			.const_kind = CONST_INITIALIZER,
			.initializer = new_init
	};
	return true;
}
