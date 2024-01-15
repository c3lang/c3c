// Copyright (c) 2022 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "sema_internal.h"

static inline bool sema_expr_analyse_struct_plain_initializer(SemaContext *context, Decl *assigned, Expr *initializer);
static inline bool sema_expr_analyse_array_plain_initializer(SemaContext *context, Type *assigned, Type *flattened,
															 Expr *initializer);
static inline bool sema_expr_analyse_untyped_initializer(SemaContext *context, Expr *initializer);
static bool sema_expr_analyse_designated_initializer(SemaContext *context, Type *assigned, Type *flattened,
													 Expr *initializer);
static inline void sema_not_enough_elements_error(Expr *initializer, int element);
static inline bool sema_expr_analyse_initializer(SemaContext *context, Type *assigned_type, Type *flattened, Expr *expr);
static void sema_create_const_initializer_value(ConstInitializer *const_init, Expr *value);
static void sema_create_const_initializer_from_designated_init(ConstInitializer *const_init, Expr *initializer);
static Decl *sema_resolve_element_for_name(SemaContext *context, Decl **decls, DesignatorElement ***elements_ref, unsigned *index);
static Type *sema_expr_analyse_designator(SemaContext *context, Type *current, Expr *expr, MemberIndex *max_index, Decl **member_ptr);
INLINE bool sema_initializer_list_is_empty(Expr *value);
static Type *sema_find_type_of_element(SemaContext *context, Type *type, DesignatorElement ***elements_ref, unsigned *curr_index, bool *is_constant, bool *did_report_error, MemberIndex *max_index, Decl **member_ptr);
MemberIndex sema_get_initializer_const_array_size(SemaContext *context, Expr *initializer, bool *may_be_array, bool *is_const_size);
static MemberIndex sema_analyse_designator_index(SemaContext *context, Expr *index);
static void sema_update_const_initializer_with_designator(ConstInitializer *const_init,
														  DesignatorElement **curr,
														  DesignatorElement **end,
														  Expr *value);
static inline void sema_update_const_initializer_with_designator_struct(ConstInitializer *const_init,
																		DesignatorElement **curr,
																		DesignatorElement **end,
																		Expr *value);
static inline void sema_update_const_initializer_with_designator_union(ConstInitializer *const_init,
																	   DesignatorElement **curr,
																	   DesignatorElement **end,
																	   Expr *value);
static inline void sema_update_const_initializer_with_designator_array(ConstInitializer *const_init,
																	   DesignatorElement **curr,
																	   DesignatorElement **end,
																	   Expr *value);
static inline void sema_update_const_initializer_with_designator(
		ConstInitializer *const_init,
		DesignatorElement **curr,
		DesignatorElement **end,
		Expr *value);
static inline ConstantEvalKind env_eval_type(SemaContext *context);



static inline ConstantEvalKind env_eval_type(SemaContext *context)
{
	return context->call_env.kind == CALL_ENV_GLOBAL_INIT ? CONSTANT_EVAL_GLOBAL_INIT : CONSTANT_EVAL_LOCAL_INIT;
}

static inline void sema_not_enough_elements_error(Expr *initializer, int element)
{
	if (element == 0)
	{
		SEMA_ERROR(initializer, "The initializer is missing elements.");
		return;
	}
	SEMA_ERROR(initializer->initializer_list[element - 1], "Too few elements in initializer, there should be elements after this one.");
}

/**
 * Perform analysis for a plain initializer, that is one initializing all fields.
 * @return true if analysis succeeds.
 */
static inline bool sema_expr_analyse_struct_plain_initializer(SemaContext *context, Decl *assigned, Expr *initializer)
{
	assert(assigned->resolve_status == RESOLVE_DONE);
	Expr **elements = initializer->initializer_list;
	Decl **members = assigned->strukt.members;
	MemberIndex size = (MemberIndex)vec_size(elements);
	unsigned elements_needed = decl_count_elements(assigned);

	// 1. For struct number of members must be the same as the size of the struct.
	//    Since we already handled the case with an empty initializer before going here
	//    zero entries must be an error.
	assert(size > 0 && "We should already have handled the size == 0 case.");

	// 2. We don't support this actually, but we used to. Maybe we will in the future.
	if (elements_needed == 0)
	{
		// Generate a nice error message for zero.
		SEMA_ERROR(elements[0], "Too many elements in initializer, it must be empty.");
		return false;
	}

	bool optional = false;

	bool is_bitstruct = assigned->decl_kind == DECL_BITSTRUCT;
	if (is_bitstruct && assigned->bitstruct.overlap)
	{
		if (vec_size(assigned->strukt.members) > 1 && vec_size(elements) > 1)
		{
			SEMA_ERROR(elements[0], "Bitstructs with @overlap must use designated initialization.");
			return false;
		}
	}

	// 3. Loop through all elements.
	MemberIndex max_loop = size > elements_needed ? size : elements_needed;
	for (MemberIndex i = 0; i < max_loop; i++)
	{
		// 4. Check if we exceeded the list of elements in the struct/union.
		//    This way we can check the other elements which might help the
		//    user pinpoint where they put the double elements.
		if (i >= elements_needed)
		{
			assert(i < size);
			SEMA_ERROR(elements[i], "Too many elements in initializer, expected only %d.", elements_needed);
			return false;
		}
		// 5. We might have anonymous members
		Decl *member = members[i];
		if (member->decl_kind != DECL_VAR && !member->name)
		{
			int sub_element_count = decl_count_elements(member);
			if (!sub_element_count)
			{
				vec_add(initializer->initializer_list, NULL);
				for (int j = (int)(size - 1); j > i; j--)
				{
					initializer->initializer_list[j] = initializer->initializer_list[j - 1];
				}
				Expr *new_initializer = expr_new(EXPR_CONST, initializer->span);
				ConstInitializer *empty = CALLOCS(ConstInitializer);
				empty->kind = CONST_INIT_ZERO;
				empty->type = type_flatten(member->type);
				expr_rewrite_const_initializer(new_initializer, member->type, empty);
				initializer->initializer_list[i] = new_initializer;
				size += 1;
				continue;
			}
			if (i >= size)
			{
				sema_not_enough_elements_error(initializer, (int)i);
				return false;
			}
			Expr *new_initializer = expr_new(EXPR_INITIALIZER_LIST, elements[i]->span);
			int max_index_to_copy = i + sub_element_count < size ? i + sub_element_count : size;
			for (int j = i; j < max_index_to_copy; j++)
			{
				vec_add(new_initializer->initializer_list, elements[j]);
			}
			int reduce_by = max_index_to_copy - i - 1;
			size -= reduce_by;
			elements_needed -= reduce_by;
			max_loop = size > elements_needed ? size : elements_needed;
			assert(size <= vec_size(initializer->initializer_list));
			vec_resize(initializer->initializer_list, (unsigned)size);
			elements = initializer->initializer_list;
			elements[i] = new_initializer;
		}
		if (i >= size)
		{
			sema_not_enough_elements_error(initializer, i);
			return false;
		}
		Expr *element = elements[i];
		// 6. We know the required type, so resolve the expression.
		if (!sema_analyse_expr_rhs(context, members[i]->type, element, true, NULL)) return false;
		if (member->decl_kind == DECL_VAR && member->var.kind == VARDECL_BITMEMBER)
		{
			if (!sema_bit_assignment_check(element, members[i])) return false;
		}
		optional = optional || IS_OPTIONAL(element);
	}
	assert(initializer->type);
	if (optional) initializer->type = type_get_optional(initializer->type);

	// 6. There's the case of too few values as well. Mark the last field as wrong.
	assert(elements_needed <= size);
	initializer->resolve_status = RESOLVE_DONE;
	if (expr_is_constant_eval(initializer, env_eval_type(context)))
	{
		bool is_union = type_flatten(initializer->type)->type_kind == TYPE_UNION;
		assert(!is_union || vec_size(elements) == 1);
		ConstInitializer *const_init = CALLOCS(ConstInitializer);
		const_init->kind = is_union ? CONST_INIT_UNION : CONST_INIT_STRUCT;
		const_init->type = type_flatten(initializer->type);
		if (is_union)
		{
			Expr *expr = elements[0];
			const_init->init_union.index = 0;
			if (expr_is_const_initializer(expr))
			{
				const_init->init_union.element = expr->const_expr.initializer;
			}
			else
			{
				ConstInitializer *element_init = MALLOCS(ConstInitializer);
				sema_create_const_initializer_value(element_init, expr);
				const_init->init_union.element = element_init;
			}
			expr_rewrite_const_initializer(initializer, initializer->type, const_init);
			return true;
		}
		ConstInitializer **inits = MALLOC(sizeof(ConstInitializer *) * vec_size(elements));
		VECEACH(elements, i)
		{
			Expr *expr = elements[i];
			if (expr_is_const_initializer(expr))
			{
				inits[i] = expr->const_expr.initializer;
				continue;
			}
			ConstInitializer *element_init = MALLOCS(ConstInitializer);
			sema_create_const_initializer_value(element_init, expr);
			inits[i] = element_init;
		}
		const_init->init_struct = inits;
		expr_rewrite_const_initializer(initializer, initializer->type, const_init);
	}

	// 7. Done!
	return true;

}

/**
 * Perform analysis for a plain initializer, that is one initializing all fields.
 * @return true if analysis succeeds.
 */
static inline bool sema_expr_analyse_array_plain_initializer(SemaContext *context, Type *assigned, Type *flattened,
															 Expr *initializer)
{
	Expr **elements = initializer->initializer_list;
	bool inferred_len = type_len_is_inferred(flattened);

	// We have the case where "Foo = int[*]"
	if (inferred_len && !type_len_is_inferred(assigned))
	{
		assert(assigned->type_kind == TYPE_TYPEDEF);
		assert(assigned->decl->decl_kind == DECL_TYPEDEF);
		while (assigned->type_kind == TYPE_TYPEDEF) assigned = assigned->decl->type;
		assert(type_len_is_inferred(assigned));
	}
	// Prefer the typedef index: define Bar = int; Bar[1] => Bar and not int
	Type *inner_type = type_get_indexed_type(assigned);
	assert(inner_type);
	unsigned count = vec_size(elements);
	unsigned expected_members = flattened->array.len;
	assert(count > 0 && "We should already have handled the size == 0 case.");

	if (expected_members == 0 && !inferred_len)
	{
		// Generate a nice error message for zero.
		SEMA_ERROR(elements[0], "Too many elements in initializer, it must be empty.");
		return false;
	}

	bool optional = false;
	bool is_vector = type_flat_is_vector(assigned);
	bool inner_is_inferred = type_len_is_inferred(inner_type);
	Type *inferred_element = NULL;
	if (!sema_resolve_type_structure(context, inner_type, initializer->span)) return false;
	for (unsigned i = 0; i < count; i++)
	{
		Expr *element = elements[i];
		if (!inferred_len && i >= expected_members)
		{
			SEMA_ERROR(element, "Too many elements in initializer, expected only %d.", expected_members);
			return false;
		}
		if (is_vector)
		{
			if (!sema_analyse_inferred_expr(context, inner_type, element)) return false;
			Type *element_type = element->type;
			Type *element_flat = type_flatten(element_type);
			if (element_flat->type_kind == TYPE_VECTOR
				&& type_flatten(type_get_indexed_type(element_type)) == type_flatten(inner_type))
			{
				unsigned len = element_flat->array.len;
				if (!inferred_len && i + len > expected_members)
				{
					SEMA_ERROR(element, "Too many elements in initializer when expanding, expected only %d.", expected_members);
					return false;
				}
				Expr *expr_list = expr_new_expr(EXPR_EXPRESSION_LIST, element);
				Decl *decl = decl_new_generated_var(element_type, VARDECL_LOCAL, element->span);
				Expr *decl_expr = expr_generate_decl(decl, element);
				vec_add(expr_list->expression_list, decl_expr);
				Expr *sub = expr_new_expr(EXPR_SUBSCRIPT, element);
				sub->subscript_expr.expr = exprid(expr_variable(decl));
				sub->subscript_expr.range.start = exprid(expr_new_const_int(element->span, type_usz, 0));
				vec_add(expr_list->expression_list, sub);
				if (!sema_analyse_expr_rhs(context, inner_type, expr_list, true, NULL)) return false;
				elements[i] = expr_list;
				for (unsigned j = 1; j < len; j++)
				{
					sub = expr_new_expr(EXPR_SUBSCRIPT, element);
					sub->subscript_expr.expr = exprid(expr_variable(decl));
					sub->subscript_expr.range.start = exprid(expr_new_const_int(element->span, type_usz, 1));
					vec_insert_at(elements, i + j, sub);
					if (!sema_analyse_expr_rhs(context, inner_type, sub, true, NULL)) return false;
				}
				initializer->initializer_list = elements;
				count += len - 1;
				i += len - 1;
				optional = optional || IS_OPTIONAL(element);
				continue;
			}
			if (!cast_implicit(context, element, inner_type)) return false;
			optional = optional || IS_OPTIONAL(element);
		}
		else
		{
			if (!sema_analyse_expr_rhs(context, inner_type, element, true, NULL)) return false;
			if (inner_is_inferred)
			{
				if (inferred_element)
				{
					if (!cast_implicit(context, element, inferred_element))
					{
						SEMA_NOTE(elements[0], "Type inferred from here.");
						return false;
					}
				}
				else
				{
					inferred_element = element->type;
				}
			}
		}
		optional = optional || IS_OPTIONAL(element);
	}
	if (inner_is_inferred)
	{
		if (!inferred_element)
		{
			SEMA_ERROR(initializer, "Zero sized elements are not allowed when inferring size.");
			return false;
		}
		inner_type = inferred_element;
	}
	if (inferred_len)
	{
		initializer->type = type_from_inferred(flattened, inner_type, count);
	}
	else
	{
		initializer->type = assigned;
	}

	assert(initializer->type);
	if (optional) initializer->type = type_get_optional(initializer->type);

	if (!inferred_len && expected_members > count)
	{
		SEMA_ERROR(elements[count - 1], "Too few elements in initializer, %d elements are needed.", expected_members);
		return false;
	}

	initializer->resolve_status = RESOLVE_DONE;
	if (expr_is_constant_eval(initializer, env_eval_type(context)))
	{
		ConstInitializer *const_init = CALLOCS(ConstInitializer);
		const_init->kind = CONST_INIT_ARRAY_FULL;
		const_init->type = type_flatten(initializer->type);
		ConstInitializer **inits = VECNEW(ConstInitializer*, vec_size(elements));
		VECEACH(elements, i)
		{
			Expr *expr = elements[i];
			if (expr_is_const_initializer(expr))
			{
				vec_add(inits, expr->const_expr.initializer);
				continue;
			}
			ConstInitializer *element_init = MALLOCS(ConstInitializer);
			sema_create_const_initializer_value(element_init, expr);
			vec_add(inits, element_init);
		}
		const_init->init_array_full = inits;
		expr_rewrite_const_initializer(initializer, initializer->type, const_init);
	}

	// 7. Done!
	return true;
}

static inline bool sema_expr_analyse_untyped_initializer(SemaContext *context, Expr *initializer)
{
	Expr **init_list = initializer->initializer_list;
	FOREACH_BEGIN(Expr *element, init_list)
		if (!sema_analyse_expr(context, element)) return false;
		if (!expr_is_const(element))
		{
			SEMA_ERROR(element, "An untyped list can only have constant elements, you can try to type the list by prefixing the type, e.g. 'int[2] { a, b }'.");
			return false;
		}
	FOREACH_END();
	initializer->expr_kind = EXPR_CONST;
	initializer->const_expr = (ExprConst) { .const_kind = CONST_UNTYPED_LIST, .untyped_list = init_list };
	initializer->type = type_untypedlist;
	return true;
}

static bool sema_expr_analyse_designated_initializer(SemaContext *context, Type *assigned, Type *flattened,
													 Expr *initializer)
{
	Expr **init_expressions = initializer->designated_init_list;
	Type *original = flattened->canonical;
	bool is_bitstruct = original->type_kind == TYPE_BITSTRUCT;
	bool is_structlike = type_is_union_or_strukt(original) || is_bitstruct;
	MemberIndex max_index = -1;
	bool optional = false;
	Type *inner_type = NULL;
	bool is_inferred = type_is_inferred(flattened);
	VECEACH(init_expressions, i)
	{
		Expr *expr = init_expressions[i];
		Decl *member;
		Type *result = sema_expr_analyse_designator(context, original, expr, &max_index, &member);
		if (!result) return false;
		Expr *value = expr->designator_expr.value;
		if (!sema_analyse_expr_rhs(context, result, value, true, NULL)) return false;
		if (member && member->decl_kind == DECL_VAR && member->var.kind == VARDECL_BITMEMBER)
		{
			if (!sema_bit_assignment_check(value, member)) return false;
		}
		optional = optional || IS_OPTIONAL(value);
		expr->resolve_status = RESOLVE_DONE;
		if (!inner_type)
		{
			inner_type = type_no_optional(value->type);
			continue;
		}
	}
	Type *type;
	if (!is_structlike && is_inferred)
	{
		 type = type_from_inferred(flattened, type_get_indexed_type(assigned), (ArraySize)(max_index + 1));
	}
	else
	{
		type = assigned;
	}
	initializer->type = type_add_optional(type, optional);
	initializer->resolve_status = RESOLVE_DONE;
	if (expr_is_constant_eval(initializer, env_eval_type(context)))
	{
		ConstInitializer *const_init = MALLOCS(ConstInitializer);
		sema_create_const_initializer_from_designated_init(const_init, initializer);
		expr_rewrite_const_initializer(initializer, initializer->type, const_init);
	}
	return true;
}


static inline bool sema_expr_analyse_initializer(SemaContext *context, Type *assigned_type, Type *flattened, Expr *expr)
{
	// Note at this point this we either have
	// EXPR_DESIGNATED_INITIALIZER_LIST
	// or EXPR_INITIALIZER_LIST

	// 1. Designated initializer is separately evaluated.
	if (expr->expr_kind == EXPR_DESIGNATED_INITIALIZER_LIST)
	{
		return sema_expr_analyse_designated_initializer(context, assigned_type, flattened, expr);
	}

	assert(expr->expr_kind == EXPR_INITIALIZER_LIST);

	// 2. Grab the expressions inside.
	Expr **init_expressions = expr->initializer_list;

	if (init_expressions)
	{
		expr->initializer_list = init_expressions = sema_expand_vasplat_exprs(context, init_expressions);
	}
	unsigned init_expression_count = vec_size(init_expressions);

	// 3. Zero size init will initialize to empty.
	if (init_expression_count == 0)
	{
		if (type_len_is_inferred(assigned_type))
		{
			SEMA_ERROR(expr, "Zero length arrays / vectors are not permitted.");
			return false;
		}
		ConstInitializer *initializer = CALLOCS(ConstInitializer);
		initializer->kind = CONST_INIT_ZERO;
		initializer->type = flattened;
		expr_rewrite_const_initializer(expr, assigned_type, initializer);
		return true;
	}

	// 4. We might have a complist, because were analyzing $foo = { ... } or similar.
	if (assigned_type == type_untypedlist)
	{
		return sema_expr_analyse_untyped_initializer(context, expr);
	}

	// 5. If not, then we see if we have an array.
	if (flattened->type_kind == TYPE_UNTYPED_LIST ||
		flattened->type_kind == TYPE_ARRAY ||
		flattened->type_kind == TYPE_INFERRED_ARRAY ||
		flattened->type_kind == TYPE_INFERRED_VECTOR ||
		flattened->type_kind == TYPE_SUBARRAY ||
		flattened->type_kind == TYPE_VECTOR)
	{
		return sema_expr_analyse_array_plain_initializer(context, assigned_type, flattened, expr);
	}

	expr->type = assigned_type;
	return sema_expr_analyse_struct_plain_initializer(context, flattened->decl, expr);
}

/**
 * Create a const initializer.
 */
static void sema_create_const_initializer_from_designated_init(ConstInitializer *const_init, Expr *initializer)
{
	const_init->kind = CONST_INIT_ZERO;
	// Flatten the type since the external type might be typedef or a distinct type.
	const_init->type = type_flatten(initializer->type);

	// Loop through the initializers.
	Expr **init_expressions = initializer->initializer_list;
	VECEACH(init_expressions, i)
	{
		Expr *expr = init_expressions[i];
		DesignatorElement **path = expr->designator_expr.path;
		Expr *value = expr->designator_expr.value;
		sema_update_const_initializer_with_designator(const_init, path, path + vec_size(path), value);
	}
}

void sema_invert_bitstruct_const_initializer(ConstInitializer *initializer)
{
	Decl **members = initializer->type->decl->strukt.members;
	unsigned len = vec_size(members);

	// Expand
	if (initializer->kind == CONST_INIT_ZERO)
	{
		ConstInitializer **initializers = MALLOC(sizeof(ConstInitializer*) * len);
		for (unsigned i = 0; i < len; i++)
		{
			initializers[i] = MALLOCS(ConstInitializer);
			initializers[i]->kind = CONST_INIT_ZERO;
			initializers[i]->type = type_flatten(members[i]->type);
		}
		initializer->init_struct = initializers;
		initializer->kind = CONST_INIT_STRUCT;
	}

	ConstInitializer **initializers = initializer->init_struct;
	for (unsigned i = 0; i < len; i++)
	{
		ConstInitializer *init = initializers[i];
		Type *type = type_flatten(init->type);
		if (type == type_bool)
		{
			if (init->kind == CONST_INIT_ZERO)
			{
				init->init_value = expr_new_const_bool(INVALID_SPAN, init->type, true);
				continue;
			}
			init->init_value->const_expr.b = !init->init_value->const_expr.b;
			continue;
		}
		Decl *member = members[i];
		unsigned bits = member->var.end_bit - member->var.start_bit;
		if (init->kind == CONST_INIT_ZERO)
		{
			init->init_value = expr_new_const_int(INVALID_SPAN, init->type, 0);
			init->kind = CONST_INIT_VALUE;
		}
		Int res = init->init_value->const_expr.ixx;
		res = int_not(res);
		Int neg = int_not((Int){ .type = res.type });
		uint32_t bits_used = 128 - i128_clz(&neg.i);
		if (bits_used > bits)
		{
			neg.i = i128_lshr64(neg.i, bits_used - bits);
			res = int_and(res, neg);
		}
		init->init_value->const_expr.ixx = res;
	}
}


ConstInitializer *sema_merge_bitstruct_const_initializers(ConstInitializer *lhs, ConstInitializer *rhs, BinaryOp op)
{
	if (rhs->kind == CONST_INIT_ZERO)
	{
		ConstInitializer *temp = lhs;
		lhs = rhs;
		rhs = temp;
	}

	if (lhs->kind == CONST_INIT_ZERO )
	{
		if (rhs->kind == CONST_INIT_ZERO)
		{
			lhs->kind = CONST_INIT_ZERO;
			return lhs;
		}
		switch (op)
		{
			case BINARYOP_BIT_OR:
			case BINARYOP_BIT_XOR:
				return rhs;
			case BINARYOP_BIT_AND:
				lhs->kind = CONST_INIT_ZERO;
				return lhs;
			default:
				UNREACHABLE
		}
	}
	assert(lhs->kind == CONST_INIT_STRUCT && rhs->kind == CONST_INIT_STRUCT);
	ConstInitializer **lhs_inits = lhs->init_struct;
	ConstInitializer **rhs_inits = rhs->init_struct;
	Decl **members = lhs->type->decl->strukt.members;
	unsigned len = vec_size(members);
	for (unsigned i = 0; i < len; i++)
	{
		ConstInitializer *init_lhs = lhs_inits[i];
		ConstInitializer *init_rhs = rhs_inits[i];
		// Switch to check const int in a simple way.
		if (init_rhs->kind == CONST_INIT_ZERO)
		{
			ConstInitializer *temp = init_lhs;
			init_lhs = init_rhs;
			init_rhs = temp;
		}
		if (init_lhs->kind == CONST_INIT_ZERO)
		{
			lhs_inits[i] = op == BINARYOP_BIT_AND ? init_lhs : init_rhs;
			continue;
		}
		// We know switch happened, init_lhs == init_lhs[i]
		Expr *lhs_expr = init_lhs->init_value;
		Expr *rhs_expr = init_rhs->init_value;
		if (type_flatten(init_lhs->type) == type_bool)
		{
			switch (op)
			{
				case BINARYOP_BIT_OR:
					lhs_expr->const_expr.b |= rhs_expr->const_expr.b;
					break;
				case BINARYOP_BIT_XOR:
					lhs_expr->const_expr.b ^= rhs_expr->const_expr.b;
					break;
				case BINARYOP_BIT_AND:
					lhs_expr->const_expr.b &= rhs_expr->const_expr.b;
					break;
				default:
					UNREACHABLE
			}
			continue;
		}
		assert(type_is_integer(type_flatten(init_lhs->type)));
		switch (op)
		{
			case BINARYOP_BIT_AND:
				lhs_expr->const_expr.ixx = int_and(lhs_expr->const_expr.ixx, rhs_expr->const_expr.ixx);
				break;
			case BINARYOP_BIT_XOR:
				lhs_expr->const_expr.ixx = int_xor(lhs_expr->const_expr.ixx, rhs_expr->const_expr.ixx);
				break;
			case BINARYOP_BIT_OR:
				lhs_expr->const_expr.ixx = int_or(lhs_expr->const_expr.ixx, rhs_expr->const_expr.ixx);
				break;
			default:
				UNREACHABLE;
		}
	}
	return lhs;
}

bool sema_expr_analyse_initializer_list(SemaContext *context, Type *to, Expr *expr)
{
	if (!to) to = type_untypedlist;
	assert(to);
	Type *flattened = type_flatten(to);
	bool is_zero_init = expr->expr_kind == EXPR_INITIALIZER_LIST && !vec_size(expr->initializer_list);
	if (!sema_resolve_type_structure(context, to, expr->span)) return false;
	switch (flattened->type_kind)
	{
		case TYPE_ANY:
		case TYPE_INTERFACE:
			UNREACHABLE
		case TYPE_UNTYPED_LIST:
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_ARRAY:
		case TYPE_BITSTRUCT:
		case TYPE_INFERRED_ARRAY:
		case TYPE_INFERRED_VECTOR:
		case TYPE_VECTOR:
			return sema_expr_analyse_initializer(context, to, flattened, expr);
		case TYPE_SUBARRAY:
		{
			if (is_zero_init)
			{
				expr->expr_kind = EXPR_CONST;
				expr->const_expr.const_kind = CONST_POINTER;
				expr->const_expr.ptr = 0;
				expr->type = to;
				return true;
			}
			// Resolve this as an inferred array.
			Type *type = type_get_inferred_array(flattened->array.base);
			if (!sema_expr_analyse_initializer(context, type, type, expr)) return false;
			expr->resolve_status = RESOLVE_DONE;
			expr_insert_addr(expr);
			if (!sema_analyse_expr(context, expr)) return false;
			return cast_explicit(context, expr, to);
		}
		case TYPE_POINTER:
			if (is_zero_init)
			{
				expr_rewrite_to_const_zero(expr, to);
				return true;
			}
			SEMA_ERROR(expr, "Pointers cannot be initialized using an initializer list, instead you need to take the address of an array.");
			return false;
		case TYPE_VOID:
		case TYPE_POISONED:
		case TYPE_FUNC:
		case TYPE_TYPEDEF:
		case TYPE_OPTIONAL:
		case TYPE_TYPEINFO:
		case TYPE_MEMBER:
			break;
		default:
			if (is_zero_init)
			{
				expr_rewrite_to_const_zero(expr, flattened);
				expr->type = to;
				return true;
			}
			break;
	}
	SEMA_ERROR(expr, "'%s' cannot use compound literal initialization, did you intend to use a cast?", type_to_error_string(to));
	return false;
}


static void sema_create_const_initializer_value(ConstInitializer *const_init, Expr *value)
{
	// Possibly this is already a const initializers, in that case
	// overwrite what is inside, eg [1] = { .a = 1 }
	if (expr_is_const_initializer(value))
	{
		*const_init = *value->const_expr.initializer;
		value->const_expr.initializer = const_init;
		return;
	}
	if (value->expr_kind == EXPR_IDENTIFIER)
	{
		Decl *ident = decl_flatten(value->identifier_expr.decl);
		assert(ident->decl_kind == DECL_VAR);
		assert(ident->var.kind == VARDECL_CONST);
		sema_create_const_initializer_value(const_init, expr_copy(ident->var.init_expr));
		return;
	}
	const_init->init_value = value;
	const_init->type = type_flatten(value->type);
	const_init->kind = CONST_INIT_VALUE;
}

/**
 * Update a struct element, e.g. { .a = 1 } or { .a[12] = { .b } }
 */
static inline void sema_update_const_initializer_with_designator_struct(ConstInitializer *const_init,
																		DesignatorElement **curr,
																		DesignatorElement **end,
																		Expr *value)
{
	// Get the current path element that we're processing
	DesignatorElement *element = curr[0];
	assert(element->kind == DESIGNATOR_FIELD);
	DesignatorElement **next_element = curr + 1;
	bool is_last_path_element = next_element == end;

	// Optimize in case this is a zero, e.g. [12].b = {}
	if (is_last_path_element && sema_initializer_list_is_empty(value))
	{
		const_init->kind = CONST_INIT_ZERO;
		return;
	}
	Decl **elements = const_init->type->decl->strukt.members;

	// Convert a zero struct and expand it into all its parts.
	if (const_init->kind == CONST_INIT_ZERO)
	{
		// Allocate array containing all elements { a, b, c ... }
		ConstInitializer **const_inits = MALLOC(sizeof(ConstInitializer *) * vec_size(elements));
		VECEACH(elements, i)
		{
			// Create zero initializers for each of those { a: zeroinit, b: zeroinit, ... }
			ConstInitializer *element_init = MALLOCS(ConstInitializer);
			element_init->type = type_flatten(elements[i]->type);
			element_init->kind = CONST_INIT_ZERO;
			const_inits[i] = element_init;
		}
		// Change type to CONST_INIT_STRUCT since we expanded.
		const_init->init_struct = const_inits;
		const_init->kind = CONST_INIT_STRUCT;
	}

	// We should always have expanded the struct at this point.
	assert(const_init->kind == CONST_INIT_STRUCT);

	// Find the ConstInitializer to change
	ConstInitializer *sub_element = const_init->init_struct[element->index];

	// If this isn't the last element, we recurse.
	if (!is_last_path_element)
	{
		sema_update_const_initializer_with_designator(sub_element, next_element, end, value);
		return;
	}

	// Otherwise we update the value in that particular element.
	sema_create_const_initializer_value(sub_element, value);
}

/**
 * Update a union element, which is different from structs, since we're here
 * only keeping a single value.
 * Note that if we have two fields "a" and "b", then in this case { .b = 2, .a = 1 },
 * we will completely discard the information in ".b = 2", even if there had been
 * an overlap. In essence we're implicitly clearing the memory when we assign to .a, meaning
 * we are allowed to completely overwrite the "memory" of .b
 */
static inline void sema_update_const_initializer_with_designator_union(ConstInitializer *const_init,
																	   DesignatorElement **curr,
																	   DesignatorElement **end,
																	   Expr *value)
{
	DesignatorElement *element = curr[0];
	assert(element->kind == DESIGNATOR_FIELD);
	ConstInitializer *sub_element = const_init->init_union.element;

	// If it's an empty initializer, just clear everything back to CONST_INIT_ZERO
	// That is we get for example: { .b = { 1, 3 }, .a = { } }. This would then simply
	// become { }
	DesignatorElement **next_element = curr + 1;
	bool is_at_last_path_element = next_element == end;
	if (is_at_last_path_element && sema_initializer_list_is_empty(value))
	{
		const_init->kind = CONST_INIT_ZERO;
		return;
	}

	// If we currently have a zero, then we create a sub element that is zero.
	if (const_init->kind == CONST_INIT_ZERO)
	{
		sub_element = const_init->init_union.element = CALLOCS(ConstInitializer);
		sub_element->kind = CONST_INIT_ZERO;
		const_init->init_union.element = sub_element;
	}
	else if (element->index != const_init->init_union.index)
	{
		// We will zero out the sub element if it wasn't a union
		sub_element->kind = CONST_INIT_ZERO;
	}

	// Update of the sub element.
	sub_element->type = type_flatten(const_init->type->decl->strukt.members[element->index]->type);

	// And the index
	const_init->init_union.index = element->index;

	// And the type
	const_init->kind = CONST_INIT_UNION;

	// If we're not at the last element in the path, descend further.
	if (!is_at_last_path_element)
	{
		sema_update_const_initializer_with_designator(sub_element, next_element, end, value);
		return;
	}

	// Otherwise just set the current type.
	sema_create_const_initializer_value(sub_element, value);
}

/**
 * Update an array { [2] = 1 }
 */
static inline void sema_update_const_initializer_with_designator_array(ConstInitializer *const_init,
																	   DesignatorElement **curr,
																	   DesignatorElement **end,
																	   Expr *value)
{
	DesignatorElement *element = curr[0];
	MemberIndex low_index = element->index;
	MemberIndex high_index = element->kind == DESIGNATOR_RANGE ? element->index_end : element->index;
	assert(element->kind == DESIGNATOR_ARRAY || element->kind == DESIGNATOR_RANGE);

	// Expand zero into array.
	if (const_init->kind == CONST_INIT_ZERO)
	{
		const_init->kind = CONST_INIT_ARRAY;
		const_init->init_array.elements = NULL;
	}

	Type *element_type = type_flatten(const_init->type->array.base);
	DesignatorElement **next_element = curr + 1;
	bool is_last_path_element = next_element == end;

	// Get all the elements in the array
	ConstInitializer **array_elements = const_init->init_array.elements;

	unsigned array_count = vec_size(array_elements);

	MemberIndex insert_index = 0;

	for (MemberIndex index = low_index; index <= high_index; index++)
	{
		// Walk to the insert point or until we reached the end of the array.
		while (insert_index < array_count && array_elements[insert_index]->init_array_value.index < index)
		{
			insert_index++;
		}
		// Pick up the initializer at the insert point.
		ConstInitializer *initializer = insert_index < array_count ? array_elements[insert_index] : NULL;
		ConstInitializer *inner_value;

		// If we don't have an initializer, the location needs to be at the end.
		// Create and append:
		if (!initializer)
		{
			initializer = MALLOCS(ConstInitializer);
			initializer->type = element_type;
			initializer->kind = CONST_INIT_ARRAY_VALUE;
			initializer->init_array_value.index = index;
			inner_value = MALLOCS(ConstInitializer);
			inner_value->type = element_type;
			inner_value->kind = CONST_INIT_ZERO;
			initializer->init_array_value.element = inner_value;
			vec_add(array_elements, initializer);
			array_count++;
		}
		else
		{
			// If we already have an initializer "found"
			// it might be after the index. In this case, we
			// need to do an insert.
			if (initializer->init_array_value.index != insert_index)
			{
				assert(initializer->init_array_value.index > insert_index);

				// First we add a null at the end.
				vec_add(array_elements, NULL);
				// Shift all values one step up:
				for (unsigned i = array_count; i > insert_index; i--)
				{
					array_elements[i] = array_elements[i - 1];
				}
				// Then we create our new entry.
				initializer = MALLOCS(ConstInitializer);
				initializer->type = element_type;
				initializer->kind = CONST_INIT_ARRAY_VALUE;
				initializer->init_array_value.index = index;
				inner_value = MALLOCS(ConstInitializer);
				inner_value->type = element_type;
				inner_value->kind = CONST_INIT_ZERO;
				initializer->init_array_value.element = inner_value;
				// And assign it to the location.
				array_elements[insert_index] = initializer;
			}
		}

		const_init->init_array.elements = array_elements;
		inner_value = initializer->init_array_value.element;

		// Update
		if (!is_last_path_element)
		{
			sema_update_const_initializer_with_designator(inner_value, next_element, end, value);
			continue;
		}
		sema_create_const_initializer_value(inner_value, value);
	}
}

static inline void sema_update_const_initializer_with_designator(
		ConstInitializer *const_init,
		DesignatorElement **curr,
		DesignatorElement **end,
		Expr *value)
{
	switch (const_init->type->type_kind)
	{
		case TYPE_STRUCT:
		case TYPE_BITSTRUCT:
			sema_update_const_initializer_with_designator_struct(const_init, curr, end, value);
			return;
		case TYPE_UNION:
			sema_update_const_initializer_with_designator_union(const_init, curr, end, value);
			return;
		case TYPE_ARRAY:
		case TYPE_VECTOR:
			sema_update_const_initializer_with_designator_array(const_init, curr, end, value);
			return;
		default:
			UNREACHABLE
	}
}

static Type *sema_expr_analyse_designator(SemaContext *context, Type *current, Expr *expr, MemberIndex *max_index, Decl **member_ptr)
{
	DesignatorElement **path = expr->designator_expr.path;

	// Walk down into this path
	bool is_constant = true;
	bool did_report_error = false;
	*member_ptr = NULL;
	for (unsigned i = 0; i < vec_size(path); i++)
	{
		Decl *member_found;
		Type *new_current = sema_find_type_of_element(context, current, &path, &i, &is_constant, &did_report_error, i == 0 ? max_index : NULL, &member_found);
		if (!new_current)
		{
			if (!did_report_error) SEMA_ERROR(expr, "This is not a valid member of '%s'.", type_to_error_string(current));
			return NULL;
		}
		current = new_current;
		*member_ptr = member_found;
	}
	return current;
}

INLINE bool sema_initializer_list_is_empty(Expr *value)
{
	return expr_is_const_initializer(value) && value->const_expr.initializer->kind == CONST_INIT_ZERO;
}

static Type *sema_find_type_of_element(SemaContext *context, Type *type, DesignatorElement ***elements_ref, unsigned *curr_index, bool *is_constant, bool *did_report_error, MemberIndex *max_index, Decl **member_ptr)
{
	Type *type_flattened = type_flatten(type);
	DesignatorElement *element = (*elements_ref)[*curr_index];
	if (element->kind == DESIGNATOR_ARRAY || element->kind == DESIGNATOR_RANGE)
	{
		*member_ptr = NULL;
		ByteSize len;
		Type *base;
		switch (type_flattened->type_kind)
		{
			case TYPE_INFERRED_ARRAY:
			case TYPE_INFERRED_VECTOR:
				len = MAX_ARRAYINDEX;
				base = type_flattened->array.base;
				break;
			case TYPE_ARRAY:
			case TYPE_VECTOR:
				len = type_flattened->array.len;
				base = type_flattened->array.base;
				break;
			default:
				return NULL;
		}
		MemberIndex index = sema_analyse_designator_index(context, element->index_expr);
		if (index < 0)
		{
			*did_report_error = true;
			return NULL;
		}
		if (index >= (MemberIndex)len)
		{
			SEMA_ERROR(element->index_expr, "The index may must be less than the array length (which was %llu).", (unsigned long long)len);
			*did_report_error = true;
			return NULL;
		}

		element->index = index;
		if (max_index && *max_index < index) *max_index = index;
		if (element->kind == DESIGNATOR_RANGE)
		{
			MemberIndex end_index = sema_analyse_designator_index(context, element->index_end_expr);
			if (end_index < 0)
			{
				*did_report_error = true;
				return NULL;
			}
			if (index > end_index)
			{
				SEMA_ERROR(element->index_end_expr, "End index must be greater than start index.");
				*did_report_error = true;
				return NULL;
			}
			if (end_index > (MemberIndex)len)
			{
				*did_report_error = true;
				SEMA_ERROR(element->index_expr, "The index may must be less than the array length (which was %llu).", (unsigned long long)len);
				return NULL;
			}
			element->index_end = end_index;
			if (max_index && *max_index < end_index) *max_index = end_index;
		}
		return base;
	}
	assert(element->kind == DESIGNATOR_FIELD);
	if (!type_is_union_or_strukt(type_flattened) && type_flattened->type_kind != TYPE_BITSTRUCT)
	{
		return NULL;
	}
	Decl *member = sema_resolve_element_for_name(context,
												 type_flattened->decl->strukt.members,
												 elements_ref,
												 curr_index);
	*member_ptr = member;
	if (!member) return NULL;
	return member->type;
}

MemberIndex sema_get_initializer_const_array_size(SemaContext *context, Expr *initializer, bool *may_be_array, bool *is_const_size)
{
	if (expr_is_const(initializer))
	{
		assert(initializer->const_expr.const_kind == CONST_INITIALIZER);
		ConstInitializer *init = initializer->const_expr.initializer;
		Type *type = type_flatten(initializer->type);
		*is_const_size = true;
		switch (init->kind)
		{
			case CONST_INIT_ZERO:
				if (type->type_kind == TYPE_ARRAY)
				{
					*may_be_array = true;
					return (MemberIndex)type->array.len;
				}
				if (type->type_kind == TYPE_SUBARRAY)
				{
					*may_be_array = true;
					return 0;
				}
				*may_be_array = false;
				return 0;
			case CONST_INIT_ARRAY:
				*may_be_array = true;
				return vectail(init->init_array.elements)->init_array_value.index + 1;
			case CONST_INIT_ARRAY_FULL:
				*may_be_array = true;
				return (MemberIndex)vec_size(init->init_array_full);
			case CONST_INIT_ARRAY_VALUE:
				UNREACHABLE;
			case CONST_INIT_STRUCT:
			case CONST_INIT_UNION:
			case CONST_INIT_VALUE:
				*may_be_array = false;
				return 0;
		}
		UNREACHABLE
	}
	switch (initializer->expr_kind)
	{
		case EXPR_INITIALIZER_LIST:
			*may_be_array = true;
			*is_const_size = true;
			return (MemberIndex)vec_size(initializer->initializer_list);
		case EXPR_DESIGNATED_INITIALIZER_LIST:
			break;
		default:
			UNREACHABLE
	}
	Expr **initializers = initializer->designated_init_list;
	MemberIndex size = 0;
	// Otherwise we assume everything's a designator.
	VECEACH(initializers, i)
	{
		Expr *sub_initializer = initializers[i];
		assert(sub_initializer->expr_kind == EXPR_DESIGNATOR);

		DesignatorElement *element = sub_initializer->designator_expr.path[0];
		switch (element->kind)
		{
			case DESIGNATOR_FIELD:
				// Struct, abandon!
				*may_be_array = false;
				return -1;
			case DESIGNATOR_ARRAY:
			{
				MemberIndex index = sema_analyse_designator_index(context, element->index_expr);
				if (index < 0 || element->index_expr->expr_kind != EXPR_CONST)
				{
					*is_const_size = false;
					return -1;
				}
				if (index + 1 > size) size = index + 1;
				break;
			}
			case DESIGNATOR_RANGE:
			{
				MemberIndex index = sema_analyse_designator_index(context, element->index_end_expr);
				if (index < 0 || element->index_end_expr->expr_kind != EXPR_CONST)
				{
					*is_const_size = false;
					return -1;
				}
				if (index + 1 > size) size = index + 1;
				break;
			}
			default:
				UNREACHABLE
		}
	}
	return size;
}

static MemberIndex sema_analyse_designator_index(SemaContext *context, Expr *index)
{
	if (!sema_analyse_expr(context, index))
	{
		return -1;
	}

	// Unless we already have type_usz, cast to type_isz;
	if (!cast_to_index(context, index))
	{
		return -1;
	}
	if (index->expr_kind != EXPR_CONST)
	{
		SEMA_ERROR(index, "The index must be a constant value.");
		return -1;
	}
	if (!int_fits(index->const_expr.ixx, TYPE_I32))
	{
		SEMA_ERROR(index, "The value of the index does not fit in an int.");
		return -1;
	}
	int64_t index_val = int_to_i64(index->const_expr.ixx);
	if (index_val < 0)
	{
		SEMA_ERROR(index, "Negative index values is not allowed.");
		return -1;
	}
	return (MemberIndex)index_val;
}


static Decl *sema_resolve_element_for_name(SemaContext *context, Decl **decls, DesignatorElement ***elements_ref, unsigned *index)
{
	DesignatorElement *element = (*elements_ref)[*index];

	Expr *field = sema_expr_resolve_access_child(context, element->field_expr, NULL);
	if (!field) return poisoned_decl;

	if (field->expr_kind != EXPR_IDENTIFIER)
	{
		SEMA_ERROR(field, "An identifier was expected.");
		return poisoned_decl;
	}
	const char *name = field->identifier_expr.ident;
	unsigned old_index = *index;
	VECEACH(decls, i)
	{
		Decl *decl = decls[i];
		// The simple case, we have a match.
		if (decl->name == name)
		{
			element->index = (MemberIndex)i;
			return decl;
		}
		if (!decl->name)
		{
			assert(type_is_union_or_strukt(decl->type) || decl->decl_kind == DECL_BITSTRUCT);
			// Anonymous struct
			Decl *found = sema_resolve_element_for_name(context, decl->strukt.members, elements_ref, index);
			// No match, continue...
			if (!found) continue;

			// Create our anon field.
			DesignatorElement *anon_element = CALLOCS(DesignatorElement);
			anon_element->kind = DESIGNATOR_FIELD;
			anon_element->index = (MemberIndex)i;
			vec_insert_at(*elements_ref, old_index, anon_element);
			// Advance
			(*index)++;
			return found;
		}
	}
	return NULL;
}
