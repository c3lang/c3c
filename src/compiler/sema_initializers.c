// Copyright (c) 2022 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "sema_internal.h"

static inline bool sema_expr_analyse_struct_plain_initializer(SemaContext *context, Decl *assigned, Expr *initializer, bool *no_match_ref);
static inline bool sema_expr_analyse_array_plain_initializer(SemaContext *context, Type *assigned, Type *flattened,
                                                             Expr *initializer, bool *no_match_ref);
static inline bool sema_expr_analyse_untyped_initializer(SemaContext *context, Expr *initializer, bool *no_match_ref);
static bool sema_expr_analyse_designated_initializer(SemaContext *context, Type *assigned, Type *flattened,
                                                     Expr *initializer, bool *no_match_ref);
static inline void sema_not_enough_elements_error(SemaContext *context, Expr *initializer, int element);
static inline bool sema_expr_analyse_initializer(SemaContext *context, Type *assigned_type, Type *flattened, Expr *expr, bool *no_match_ref);
static void sema_create_const_initializer_from_designated_init(ConstInitializer *const_init, Expr *initializer);
static Decl *sema_resolve_element_for_name(SemaContext *context, Decl **decls, DesignatorElement ***elements_ref, unsigned *index, bool is_substruct);
static Type *sema_expr_analyse_designator(SemaContext *context, Type *current, Expr *expr, ArrayIndex *max_index, Decl **member_ptr);
INLINE bool sema_initializer_list_is_empty(Expr *value);
static Type *sema_find_type_of_element(SemaContext *context, Type *type, DesignatorElement ***elements_ref, unsigned *curr_index, bool *did_report_error, ArrayIndex *max_index, Decl **member_ptr);
static ArrayIndex sema_analyse_designator_index(SemaContext *context, Expr *index);
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


static bool const_init_local_init_may_be_global_inner(ConstInitializer *init, bool top)
{
	ConstInitializer **list = INVALID_PTR;
	unsigned len = (unsigned)-1;
	switch (init->kind)
	{
		case CONST_INIT_ZERO:
			return top;
		case CONST_INIT_STRUCT:
			list = init->init_struct;
			ASSERT(vec_size(init->type->decl->strukt.members) == vec_size(list));
			len = vec_size(list);
			break;
		case CONST_INIT_UNION:
			return const_init_local_init_may_be_global_inner(init->init_union.element, false);
		case CONST_INIT_VALUE:
		{
			Expr *val = init->init_value;
			if (!expr_is_const(val)) return false;
			if (expr_is_const_slice(val)) return false;
			return true;
		}
		case CONST_INIT_ARRAY:
			list = init->init_array.elements;
			len = vec_size(list);
			break;
		case CONST_INIT_ARRAY_FULL:
			list = init->init_array_full;
			len = vec_size(list);
			break;
		case CONST_INIT_ARRAY_VALUE:
			return const_init_local_init_may_be_global_inner(init->init_array_value.element, false);
	}
	for (unsigned i = 0; i < len; i++)
	{
		ConstInitializer *subinit = list[i];
		if (!const_init_local_init_may_be_global_inner(subinit, false)) return false;
	}
	return true;
}

void const_init_rewrite_to_zero(ConstInitializer *init, Type *type)
{
	init->kind = CONST_INIT_ZERO;
	const_init_set_type(init, type);
}

ConstInitializer *const_init_new_array(Type *type, ConstInitializer **elements)
{
	ConstInitializer *init = CALLOCS(ConstInitializer);
	init->kind = CONST_INIT_ARRAY;
	const_init_set_type(init, type);
	init->init_array.elements = elements;
	return init;
}

ConstInitializer *const_init_new_array_full(Type *type, ConstInitializer **elements)
{
	ConstInitializer *init = CALLOCS(ConstInitializer);
	init->kind = CONST_INIT_ARRAY_FULL;
	const_init_set_type(init, type);
	init->init_array_full = elements;
	return init;
}

ConstInitializer *const_init_new_struct(Type *type, Expr **elements)
{
	ConstInitializer *init = CALLOCS(ConstInitializer);
	init->kind = CONST_INIT_STRUCT;
	const_init_set_type(init, type);
	ConstInitializer **values = NULL;
	FOREACH(Expr *, expr, elements)
	{
		if (expr_is_const_initializer(expr))
		{
			vec_add(values, expr->const_expr.initializer);
			continue;
		}
		vec_add(values, const_init_new_value(expr));
	}
	init->init_struct = values;
	return init;
}

ConstInitializer *const_init_new_union(Type *type, ArrayIndex index, Expr *value)
{
	ConstInitializer *init = CALLOCS(ConstInitializer);
	const_init_set_type(init, type);
	init->kind = CONST_INIT_UNION;
	init->init_union.index = index;
	if (expr_is_const_initializer(value))
	{
		init->init_union.element = value->const_expr.initializer;
	}
	else
	{
		init->init_union.element = const_init_new_value(value);
	}
	return init;
}

ConstInitializer *const_init_new_array_value(Expr *expr, ArrayIndex index)
{
	ConstInitializer *init = CALLOCS(ConstInitializer);
	const_init_set_type(init, expr->type);
	init->kind = CONST_INIT_ARRAY_VALUE;
	init->init_array_value.index = index;
	init->init_array_value.element = const_init_new_value(expr);
	return init;
}

ConstInitializer *const_init_new_zero_array_value(Type *type, ArrayIndex index)
{
	ConstInitializer *init = CALLOCS(ConstInitializer);
	const_init_set_type(init, type);
	init->kind = CONST_INIT_ARRAY_VALUE;
	init->init_array_value.index = index;
	init->init_array_value.element = const_init_new_zero(type);
	return init;
}
ConstInitializer *const_init_new_zero(Type *type)
{
	ConstInitializer *init = CALLOCS(ConstInitializer);
	init->kind = CONST_INIT_ZERO;
	const_init_set_type(init, type);
	return init;
}
bool const_init_local_init_may_be_global(ConstInitializer *init)
{
	return const_init_local_init_may_be_global_inner(init, true);
}

static inline void sema_not_enough_elements_error(SemaContext *context, Expr *initializer, int element)
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
static inline bool sema_expr_analyse_struct_plain_initializer(SemaContext *context, Decl *assigned, Expr *initializer, bool *no_match_ref)
{
	ASSERT(assigned->resolve_status == RESOLVE_DONE);
	Expr **elements = initializer->initializer_list;
	Decl **members = assigned->strukt.members;
	ArrayIndex size = (ArrayIndex)vec_size(elements);
	unsigned elements_needed = decl_count_elements(assigned);

	// 1. For struct number of members must be the same as the size of the struct.
	//    Since we already handled the case with an empty initializer before going here
	//    zero entries must be an error.
	ASSERT(size > 0 && "We should already have handled the size == 0 case.");

	// 2. We don't support this actually, but we used to. Maybe we will in the future.
	if (elements_needed == 0)
	{
		if (no_match_ref) goto NO_MATCH;
		// Generate a nice error message for zero.
		RETURN_SEMA_ERROR(elements[0], "Too many elements in initializer, it must be empty.");
	}

	bool optional = false;

	bool is_bitstruct = assigned->decl_kind == DECL_BITSTRUCT;
	if (is_bitstruct && assigned->strukt.overlap)
	{
		if (vec_size(assigned->strukt.members) > 1 && vec_size(elements) > 1)
		{
			RETURN_SEMA_ERROR(elements[0], "Bitstructs with @overlap must use designated initialization.");
		}
	}

	// 3. Loop through all elements.
	ArrayIndex max_loop = size > elements_needed ? size : (ArrayIndex)elements_needed;
	for (ArrayIndex i = 0; i < max_loop; i++)
	{
		// 4. Check if we exceeded the list of elements in the struct/union.
		//    This way we can check the other elements which might help the
		//    user pinpoint where they put the double elements.
		if (i >= elements_needed)
		{
			if (no_match_ref) goto NO_MATCH;
			ASSERT(i < size);
			RETURN_SEMA_ERROR(elements[i], "Too many elements in initializer, expected only %d.", elements_needed);
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
				expr_rewrite_const_initializer(new_initializer, member->type, const_init_new_zero(type_flatten(member->type)));
				initializer->initializer_list[i] = new_initializer;
				size += 1;
				continue;
			}
			if (i >= size)
			{
				if (no_match_ref) goto NO_MATCH;
				sema_not_enough_elements_error(context, initializer, (int)i);
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
			max_loop = size > elements_needed ? size : (ArrayIndex)elements_needed;
			ASSERT(size <= vec_size(initializer->initializer_list));
			vec_resize(initializer->initializer_list, (unsigned)size);
			elements = initializer->initializer_list;
			elements[i] = new_initializer;
		}
		if (i >= size)
		{
			if (no_match_ref) goto NO_MATCH;
			sema_not_enough_elements_error(context, initializer, i);
			return false;
		}
		Expr *element = elements[i];
		// 6. We know the required type, so resolve the expression.
		if (!sema_analyse_expr_rhs(context, members[i]->type, element, true, no_match_ref, false)) return false;
		if (member->decl_kind == DECL_VAR && member->var.kind == VARDECL_BITMEMBER)
		{
			if (!sema_bit_assignment_check(context, element, members[i], no_match_ref)) return false;
		}
		optional = optional || IS_OPTIONAL(element);
	}
	ASSERT(initializer->type);
	if (optional) initializer->type = type_get_optional(initializer->type);

	// 6. There's the case of too few values as well. Mark the last field as wrong.
	ASSERT(elements_needed <= size);
	initializer->resolve_status = RESOLVE_DONE;
	if (expr_is_runtime_const(initializer))
	{
		bool is_union = type_flatten(initializer->type)->type_kind == TYPE_UNION;
		ASSERT(!is_union || vec_size(elements) == 1);
		ConstInitializer *init;
		if (is_union)
		{
			Expr *expr = elements[0];
			init = const_init_new_union(initializer->type, 0, expr);
		}
		else
		{
			init = const_init_new_struct(initializer->type, elements);
		}
		expr_rewrite_const_initializer(initializer, initializer->type, init);
	}

	// 7. Done!
	return true;
NO_MATCH:;
	*no_match_ref = true;
	return false;
}

Expr *sema_create_struct_from_expressions(Decl *struct_decl, SourceSpan span, Expr **exprs)
{
	return expr_new_const_initializer(span, struct_decl->type,
	                                  const_init_new_struct(struct_decl->type, exprs));
}

/**
 * Perform analysis for a plain initializer, that is one initializing all fields.
 * @return true if analysis succeeds.
 */
static inline bool sema_expr_analyse_array_plain_initializer(SemaContext *context, Type *assigned, Type *flattened,
                                                             Expr *initializer, bool *no_match_ref)
{
	Expr **elements = initializer->initializer_list;
	bool inferred_len = type_len_is_inferred(flattened);

	// We have the case where "Foo = int[*]"
	if (inferred_len && !type_len_is_inferred(assigned))
	{
		ASSERT(assigned->type_kind == TYPE_ALIAS);
		ASSERT(assigned->decl->decl_kind == DECL_TYPE_ALIAS);
		while (assigned->type_kind == TYPE_ALIAS) assigned = assigned->decl->type;
		ASSERT(type_len_is_inferred(assigned));
	}
	// Prefer the typedef index: define Bar = int; Bar[1] => Bar and not int
	Type *inner_type = type_get_indexed_type(assigned);
	ASSERT(inner_type);
	unsigned count = vec_size(elements);
	unsigned expected_members = flattened->array.len;
	ASSERT(count > 0 && "We should already have handled the size == 0 case.");

	if (expected_members == 0 && !inferred_len)
	{
		if (no_match_ref) goto NO_MATCH;
		// Generate a nice error message for zero.
		RETURN_SEMA_ERROR(elements[0], "Too many elements in initializer, it must be empty.");
	}
	if (expected_members > 0 && count > 0 && count != expected_members)
	{
		RETURN_SEMA_ERROR(elements[0], "Too %s elements in initializer, expected %u.", count > expected_members ? "many" : "few", expected_members);
	}

	bool optional = false;
	bool is_vector = type_flat_is_vector(assigned);
	bool inner_is_inferred = type_len_is_inferred(inner_type);
	Type *inferred_element = NULL;
	if (!sema_resolve_type_structure(context, inner_type)) return false;
	for (unsigned i = 0; i < count; i++)
	{
		Expr *element = elements[i];
		if (!inferred_len && i >= expected_members)
		{
			if (no_match_ref) goto NO_MATCH;
			RETURN_SEMA_ERROR(element, "Too many elements in initializer, expected only %d.", expected_members);
		}
		if (is_vector)
		{
			if (!sema_analyse_inferred_expr(context, inner_type, element, no_match_ref)) return false;
			Type *element_type = element->type;
			Type *element_flat = type_flatten(element_type);
			if (type_kind_is_real_vector(element_flat->type_kind)
				&& type_flatten(type_get_indexed_type(element_type)) == type_flatten(inner_type))
			{
				unsigned len = element_flat->array.len;
				if (!inferred_len && i + len > expected_members)
				{
					RETURN_SEMA_ERROR(element, "Too many elements in initializer when expanding, expected only %d.", expected_members);
				}
				Expr *expr_two = expr_new_expr(EXPR_TWO, element);
				Decl *decl = decl_new_generated_var(element_type, VARDECL_LOCAL, element->span);
				Expr *decl_expr = expr_generate_decl(decl, element);
				expr_two->two_expr.first = decl_expr;
				Expr *sub = expr_new_expr(EXPR_SUBSCRIPT, element);
				sub->subscript_expr.expr = exprid(expr_variable(decl));
				sub->subscript_expr.index.expr = exprid(expr_new_const_int(element->span, type_usz, 0));
				expr_two->two_expr.last = sub;
				if (!sema_analyse_expr_rhs(context, inner_type, expr_two, true, NULL, false)) return false;
				elements[i] = expr_two;
				for (unsigned j = 1; j < len; j++)
				{
					sub = expr_new_expr(EXPR_SUBSCRIPT, element);
					sub->subscript_expr.expr = exprid(expr_variable(decl));
					sub->subscript_expr.index.expr = exprid(expr_new_const_int(element->span, type_usz, 1));
					vec_insert_at(elements, i + j, sub);
					if (!sema_analyse_expr_rhs(context, inner_type, sub, true, NULL, false)) return false;
				}
				initializer->initializer_list = elements;
				count += len - 1;
				i += len - 1;
				optional = optional || IS_OPTIONAL(element);
				continue;
			}
			if (!cast_implicit_checked(context, element, inner_type, false, no_match_ref)) return false;
			optional = optional || IS_OPTIONAL(element);
		}
		else
		{
			if (!sema_analyse_expr_rhs(context, inner_type, element, true, no_match_ref, false)) return false;
			if (inner_is_inferred)
			{
				if (inferred_element)
				{
					if (!cast_implicit_checked(context, element, inferred_element, false, no_match_ref))
					{
						if (!no_match_ref) SEMA_NOTE(elements[0], "Type inferred from here.");
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
			if (no_match_ref) goto NO_MATCH;
			RETURN_SEMA_ERROR(initializer, "Zero sized elements are not allowed when inferring size.");
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

	ASSERT(initializer->type);
	if (optional) initializer->type = type_get_optional(initializer->type);

	if (!inferred_len && expected_members > count)
	{
		if (no_match_ref) goto NO_MATCH;
		RETURN_SEMA_ERROR(elements[count - 1], "Too few elements in initializer, %d elements are needed.", expected_members);
	}

	initializer->resolve_status = RESOLVE_DONE;
	if (expr_is_runtime_const(initializer))
	{
		ConstInitializer **inits = VECNEW(ConstInitializer*, vec_size(elements));
		FOREACH(Expr *, expr, elements)
		{
			if (expr_is_const_initializer(expr))
			{
				vec_add(inits, expr->const_expr.initializer);
				continue;
			}
			vec_add(inits, const_init_new_value(expr));
		}
		ConstInitializer *const_init = const_init_new_array_full(initializer->type, inits);
		expr_rewrite_const_initializer(initializer, initializer->type, const_init);
	}

	// 7. Done!
	return true;
NO_MATCH:;
	*no_match_ref = true;
	return false;
}

static inline bool sema_expr_analyse_untyped_initializer(SemaContext *context, Expr *initializer, bool *no_match_ref)
{
	Expr **init_list = initializer->initializer_list;
	FOREACH(Expr *, element, init_list)
	{
		if (!sema_analyse_expr_rvalue(context, element)) return false;
		if (!sema_cast_const(element))
		{
			if (no_match_ref)
			{
				*no_match_ref = true;
				return false;
			}
			RETURN_SEMA_ERROR(element, "An untyped list can only have "
									   "constant elements, you can try "
									   "to type the list by prefixing the type and possibly enclosing it in parentheses, "
									   "e.g. '((int[2]){ a, b })'.");
		}
	}
	initializer->expr_kind = EXPR_CONST;
	initializer->const_expr = (ExprConst) { .const_kind = CONST_UNTYPED_LIST, .untyped_list = init_list };
	initializer->type = type_untypedlist;
	initializer->resolve_status = RESOLVE_DONE;
	return true;
}

static bool sema_expr_analyse_designated_initializer(SemaContext *context, Type *assigned, Type *flattened,
                                                     Expr *initializer, bool *no_match_ref)
{
	Expr **init_expressions = initializer->designated_init.list;
	Expr *splat = initializer->designated_init.splat;
	if (splat)
	{
		if (!sema_analyse_expr_rvalue(context, splat)) return false;
		sema_cast_const(splat);
		if (IS_OPTIONAL(splat))
		{
			RETURN_SEMA_ERROR(splat, "An optional splat is not permitted.");
		}
	}
	Type *original = flattened->canonical;
	bool is_structlike = type_is_union_or_strukt(original) || original->type_kind == TYPE_BITSTRUCT;
	ArrayIndex max_index = -1;
	bool optional = false;
	Type *inner_type = NULL;
	bool is_inferred = type_is_infer_type(flattened);
	FOREACH(Expr *, expr, init_expressions)
	{
		Decl *member;
		Type *result = sema_expr_analyse_designator(context, original, expr, &max_index, &member);
		if (!result) return false;
		bool is_bitmember = member && member->decl_kind == DECL_VAR && member->var.kind == VARDECL_BITMEMBER;
		Expr *value = expr->designator_expr.value;
		if (!value && is_bitmember && member->var.start_bit == member->var.end_bit && type_flatten(result) == type_bool)
		{
			value = expr_new_const_bool(INVALID_SPAN, type_bool, true);
			expr->designator_expr.value = value;
		}
		if (!value) RETURN_SEMA_ERROR(expr, "This initializer needs a value.");
		if (!sema_analyse_expr_rhs(context, result, value, true, no_match_ref, false)) return false;
		if (is_bitmember)
		{
			if (!sema_bit_assignment_check(context, value, member, no_match_ref)) return false;
		}
		optional = optional || IS_OPTIONAL(value);
		expr->resolve_status = RESOLVE_DONE;
		if (!inner_type)
		{
			inner_type = type_no_optional(value->type);
		}
	}
	Type *type;
	if (!is_structlike && is_inferred)
	{
		type = type_from_inferred(flattened, flattened->array.base, (ArraySize)(max_index + 1));
	}
	else
	{
		if (!is_structlike && type_is_inferred(flattened))
		{
			RETURN_SEMA_ERROR(initializer, "Inferring size when having non-top inferrence is not supported.");
		}
		type = assigned;
	}
	if (splat && type->canonical != splat->type->canonical)
	{
		if (type_is_subtype(splat->type->canonical, type->canonical))
		{
			Decl *decl = original->decl;
			Expr *designator = expr_new(EXPR_DESIGNATOR, initializer->span);
			DesignatorElement **elements = NULL;
			while (true)
			{
				DesignatorElement *designator_element = MALLOCS(DesignatorElement);
				designator_element->kind = DESIGNATOR_FIELD;
				designator_element->index = 0;
				vec_add(elements, designator_element);
				assert(decl->is_substruct);
				Decl *member = decl->strukt.members[0];
				if (member->type->canonical == splat->type) break;
				decl = member;
			}
			designator->resolve_status = RESOLVE_DONE;
			designator->designator_expr.path = elements;
			designator->designator_expr.value = splat;
			designator->type = splat->type;
			vec_insert_first(initializer->designated_init.list, designator);
			initializer->designated_init.splat = NULL;
		}
		else
		{
			RETURN_SEMA_ERROR(splat, "Splat type does not match initializer type.");
		}
	}
	initializer->type = type_add_optional(type, optional);
	initializer->resolve_status = RESOLVE_DONE;
	if (expr_is_runtime_const(initializer))
	{
		ConstInitializer *const_init = MALLOCS(ConstInitializer);
		sema_create_const_initializer_from_designated_init(const_init, initializer);
		expr_rewrite_const_initializer(initializer, initializer->type, const_init);
	}
	return true;
}


static inline bool sema_expr_analyse_initializer(SemaContext *context, Type *assigned_type, Type *flattened, Expr *expr, bool *no_match_ref)
{
	// Note at this point this we either have
	// EXPR_DESIGNATED_INITIALIZER_LIST
	// or EXPR_INITIALIZER_LIST
	// or EXPR_CONST with a ConstInitializer

	// 1. Designated initializer is separately evaluated.
	if (expr->expr_kind == EXPR_DESIGNATED_INITIALIZER_LIST)
	{
		return sema_expr_analyse_designated_initializer(context, assigned_type, flattened, expr, no_match_ref);
	}

	if (expr->expr_kind == EXPR_CONST)
	{
		ASSERT(expr->const_expr.const_kind == CONST_INITIALIZER);
		return cast_implicit_checked(context, expr, assigned_type, false, no_match_ref);
	}
	ASSERT(expr->expr_kind == EXPR_INITIALIZER_LIST);

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
			if (no_match_ref) goto NO_MATCH;
			RETURN_SEMA_ERROR(expr, "Zero length arrays / vectors are not permitted.");
		}
		if (flattened == type_untypedlist)
		{
			expr_rewrite_const_untyped_list(expr, NULL);
			return true;
		}
		expr_rewrite_const_initializer(expr, assigned_type, const_init_new_zero(flattened));
		return true;
	}

	// 4. We might have a complist, because were analyzing $foo = { ... } or similar.
	if (assigned_type == type_untypedlist)
	{
		return sema_expr_analyse_untyped_initializer(context, expr, no_match_ref);
	}

	// 5. If not, then we see if we have an array.
	if (flattened->type_kind == TYPE_UNTYPED_LIST ||
		flattened->type_kind == TYPE_ARRAY ||
		flattened->type_kind == TYPE_INFERRED_ARRAY ||
		flattened->type_kind == TYPE_FLEXIBLE_ARRAY ||
		flattened->type_kind == TYPE_SLICE ||
		type_kind_is_any_vector(flattened->type_kind))
	{
		return sema_expr_analyse_array_plain_initializer(context, assigned_type, flattened, expr, no_match_ref);
	}

	expr->type = assigned_type;
	return sema_expr_analyse_struct_plain_initializer(context, flattened->decl, expr, no_match_ref);
NO_MATCH:;
	*no_match_ref = true;
	return false;
}

/**
 * Create a const initializer.
 */
static void sema_create_const_initializer_from_designated_init(ConstInitializer *const_init, Expr *initializer)
{
	// Flatten the type since the external type might be typedef or a distinct type.
	Type *flattened = type_flatten(initializer->type);
	if (initializer->designated_init.splat)
	{
		Expr *splat = initializer->designated_init.splat;
		ASSERT_SPAN(splat, expr_is_const_initializer(splat));
		*const_init = *splat->const_expr.initializer;
	}
	else
	{
		const_init_rewrite_to_zero(const_init, flattened);
	}
	ASSERT(type_flatten(initializer->type)->type_kind != TYPE_SLICE);
	// Loop through the initializers.
	FOREACH(Expr *, expr, initializer->initializer_list)
	{
		DesignatorElement **path = expr->designator_expr.path;
		Expr *value = expr->designator_expr.value;
		ASSERT(value);
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
		initializer->kind = CONST_INIT_STRUCT;
		initializer->init_struct = NULL;
		for (unsigned i = 0; i < len; i++)
		{
			vec_add(initializer->init_struct, const_init_new_zero(type_flatten(members[i]->type)));
		}
	}

	ASSERT(vec_size(initializer->init_struct) == len);
	FOREACH_IDX(i, ConstInitializer *, init, initializer->init_struct)
	{
		Type *type = init->type;
		if (type == type_bool)
		{
			if (init->kind == CONST_INIT_ZERO)
			{
				const_init_rewrite_to_value(init, expr_new_const_bool(INVALID_SPAN, init->type, true));
				continue;
			}
			init->init_value->const_expr.b = !init->init_value->const_expr.b;
			continue;
		}
		Decl *member = members[i];
		unsigned bits = member->var.end_bit - member->var.start_bit;
		if (init->kind == CONST_INIT_ZERO)
		{
			const_init_rewrite_to_value(init, expr_new_const_int(INVALID_SPAN, init->type, 0));
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
	ASSERT(lhs->kind == CONST_INIT_STRUCT && rhs->kind == CONST_INIT_STRUCT);
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
		if (init_lhs->type == type_bool)
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
		ASSERT(type_is_integer(type_flatten(init_lhs->type)));
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
				UNREACHABLE
		}
	}
	return lhs;
}

bool sema_expr_analyse_initializer_list(SemaContext *context, Type *to, Expr *expr, bool *no_match_ref)
{

	if (!to) to = type_untypedlist;
	ASSERT(to);
	Type *flattened = type_flatten(to);
	bool is_zero_init = (expr->expr_kind == EXPR_INITIALIZER_LIST && !vec_size(expr->initializer_list)) ||
			(expr->resolve_status == RESOLVE_DONE && sema_initializer_list_is_empty(expr));

	if (!sema_resolve_type_structure(context, to)) return false;
	switch (flattened->type_kind)
	{
		case TYPE_ANY:
		case TYPE_INTERFACE:
			if (is_zero_init)
			{
				expr_rewrite_to_const_zero(expr, to);
				return true;
			}
			break;
		case TYPE_UNTYPED_LIST:
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_BITSTRUCT:
		case ALL_ARRAYLIKE:
			return sema_expr_analyse_initializer(context, to, flattened, expr, no_match_ref);
		case TYPE_SLICE:
		{
			if (is_zero_init)
			{
				if (type_len_is_inferred(flattened->array.base))
				{
					RETURN_SEMA_ERROR(expr, "Inferring the slice inner type from an empty initializer is not possible.");
				}
				expr_rewrite_const_empty_slice(expr, to);
				return true;
			}
			// Resolve this as an inferred array.
			Type *type = type_get_inferred_array(flattened->array.base);
			if (!sema_expr_analyse_initializer(context, type, type, expr, no_match_ref)) return false;
			if (expr_is_const_initializer(expr))
			{
				ConstInitializer *init = expr->const_expr.initializer;
				expr->const_expr.slice_init = init;
				expr->const_expr.const_kind = CONST_SLICE;
				expr->type = to;
				return true;
			}
			expr->resolve_status = RESOLVE_DONE;
			expr_insert_addr(expr);
			if (!sema_analyse_expr_rvalue(context, expr)) return false;
			if (no_match_ref)
			{
				if (!cast_explicit_silent(context, expr, to)) goto NO_MATCH;
				return true;
			}
			return cast_explicit(context, expr, to);
		}
		case TYPE_POINTER:
		case TYPE_FUNC_PTR:
			if (is_zero_init)
			{
				expr_rewrite_to_const_zero(expr, to);
				return true;
			}
			if (no_match_ref) goto NO_MATCH;
			RETURN_SEMA_ERROR(expr, "Pointers cannot be initialized using an initializer list, instead you need to take the address of an array.");
		case TYPE_VOID:
		case TYPE_POISONED:
		case TYPE_FUNC_RAW:
		case TYPE_ALIAS:
		case TYPE_OPTIONAL:
		case TYPE_TYPEINFO:
		case TYPE_MEMBER:
			if (no_match_ref) goto NO_MATCH;
			RETURN_SEMA_ERROR(expr, "You cannot use %s with an initializer list.", type_quoted_error_string(to));
		default:
			if (is_zero_init)
			{
				expr_rewrite_to_const_zero(expr, flattened);
				expr->type = to;
				return true;
			}
			break;
	}
	if (no_match_ref) goto NO_MATCH;
	RETURN_SEMA_ERROR(expr, "You cannot use %s with a non-empty initializer list.", type_quoted_error_string(to));
NO_MATCH:
	*no_match_ref = true;
	return false;
}

void const_init_rewrite_to_value(ConstInitializer *const_init, Expr *value)
{
	// Possibly this is already a const initializers, in that case
	// overwrite what is inside, eg [1] = { .a = 1 }
	if (expr_is_const_initializer(value))
	{
		*const_init = *value->const_expr.initializer;
		value->const_expr.initializer = const_init;
		ASSERT(type_flatten(value->type)->type_kind != TYPE_SLICE);
		return;
	}
	if (value->expr_kind == EXPR_IDENTIFIER)
	{
		Decl *ident = decl_flatten(value->ident_expr);
		ASSERT(ident->decl_kind == DECL_VAR);
		ASSERT(ident->var.kind == VARDECL_CONST);
		const_init_rewrite_to_value(const_init, copy_expr_single(ident->var.init_expr));
		return;
	}
	const_init->init_value = value;
	const_init_set_type(const_init, value->type);
	const_init->kind = CONST_INIT_VALUE;
}

bool const_init_is_zero(ConstInitializer *init)
{
	RETRY:
	switch (init->kind)
	{
		case CONST_INIT_ZERO:
			return true;
		case CONST_INIT_STRUCT:
		{
			FOREACH(ConstInitializer *, i, init->init_struct)
			{
				if (!const_init_is_zero(i)) return false;
			}
			return true;
		}
		case CONST_INIT_UNION:
			init = init->init_union.element;
			goto RETRY;
		case CONST_INIT_VALUE:
			return expr_is_zero(init->init_value);
		case CONST_INIT_ARRAY:
		{
			FOREACH(ConstInitializer *, i, init->init_array.elements)
			{
				if (!const_init_is_zero(i)) return false;
			}
			return true;
		}
		case CONST_INIT_ARRAY_FULL:
		{
			FOREACH(ConstInitializer *, i, init->init_array_full)
			{
				if (!const_init_is_zero(i)) return false;
			}
			return true;
		}
		case CONST_INIT_ARRAY_VALUE:
			return const_init_is_zero(init->init_array_value.element);
	}
	UNREACHABLE
}
ConstInitializer *const_init_new_value(Expr *value)
{
	ConstInitializer *init = CALLOCS(ConstInitializer);
	const_init_rewrite_to_value(init, value);
	return init;
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
	ASSERT(element->kind == DESIGNATOR_FIELD);
	DesignatorElement **next_element = curr + 1;
	bool is_last_path_element = next_element == end;

	Decl **elements = const_init->type->decl->strukt.members;

	// Convert a zero struct and expand it into all its parts.
	if (const_init->kind == CONST_INIT_ZERO)
	{
		if (is_last_path_element && sema_initializer_list_is_empty(value))
		{
			// In this case we can ignore it.
			return;
		}
		const_init->init_struct = NULL;
		// Allocate array containing all elements { a, b, c ... }
		FOREACH_IDX(i, Decl *, el, elements)
		{
			// Create zero initializers for each of those { a: zeroinit, b: zeroinit, ... }
			vec_add(const_init->init_struct, const_init_new_zero(type_flatten(el->type)));
		}
		// Change type to CONST_INIT_STRUCT since we expanded.
		const_init->kind = CONST_INIT_STRUCT;
	}

	// We should always have expanded the struct at this point.
	ASSERT(const_init->kind == CONST_INIT_STRUCT);

	// Find the ConstInitializer to change
	ConstInitializer *sub_element = const_init->init_struct[element->index]; // NOLINT

	// If this isn't the last element, we recurse.
	if (!is_last_path_element)
	{
		sema_update_const_initializer_with_designator(sub_element, next_element, end, value);
		return;
	}

	// Otherwise we update the value in that particular element.
	const_init_rewrite_to_value(sub_element, value);
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
	ASSERT(element->kind == DESIGNATOR_FIELD);
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
	const_init_set_type(sub_element, const_init->type->decl->strukt.members[element->index]->type);
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
	const_init_rewrite_to_value(sub_element, value);
}

static inline ConstInitializer *sema_update_const_initializer_at_index(ConstInitializer *const_init, Type *element_type,
                                                                       ArrayIndex index,
                                                                       ArrayIndex *insert_index_ref)
{
	ConstInitializer **array_elements = const_init->init_array.elements;
	ArrayIndex insert_index = *insert_index_ref;
	ArrayIndex array_count = (ArrayIndex)vec_size(array_elements);
	// Walk to the insert point or until we reached the end of the array.
	while (insert_index < array_count && array_elements[insert_index]->init_array_value.index < index)
	{
		insert_index++;
	}
	// Pick up the initializer at the insert point.
	ConstInitializer *initializer = insert_index < array_count ? array_elements[insert_index] : NULL;

	// If we don't have an initializer, the location needs to be at the end.
	// Create and append:
	if (!initializer)
	{
		initializer = const_init_new_zero_array_value(element_type, index);
		vec_add(array_elements, initializer);
		array_count++;
	}
	else
	{
		// If we already have an initializer "found"
		// it might be after the index. In this case, we
		// need to do an insert.
		if (initializer->init_array_value.index != index)
		{
			ASSERT(initializer->init_array_value.index > insert_index);

			// First we add a null at the end.
			vec_add(array_elements, NULL);
			// Shift all values one step up:
			for (unsigned i = array_count; i > insert_index; i--)
			{
				array_elements[i] = array_elements[i - 1];
			}
			// Then we create our new entry.
			initializer = const_init_new_zero_array_value(element_type, index);
			// And assign it to the location.
			array_elements[insert_index] = initializer;
		}
	}

	const_init->init_array.elements = array_elements;
	*insert_index_ref = insert_index;
	return initializer->init_array_value.element;
}

void const_init_rewrite_array_at(ConstInitializer *const_init, Expr *value, ArrayIndex index)
{
	// Expand zero into array.
	if (const_init->kind == CONST_INIT_ZERO)
	{
		const_init->kind = CONST_INIT_ARRAY;
		const_init->init_array.elements = NULL;
	}
	ConstInitializer *inner_value;
	if (const_init->kind == CONST_INIT_ARRAY_FULL)
	{
		inner_value = const_init->init_array_full[index];
	}
	else
	{
		assert(const_init->kind == CONST_INIT_ARRAY);
		Type *element_type = type_flatten(const_init->type->array.base);

		ArrayIndex insert_index = 0;
		inner_value = sema_update_const_initializer_at_index(const_init, element_type, index, &insert_index);
	}
	const_init_rewrite_to_value(inner_value, value);
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
	ArrayIndex low_index = element->index;
	ArrayIndex high_index = element->kind == DESIGNATOR_RANGE ? element->index_end : element->index;
	ASSERT(element->kind == DESIGNATOR_ARRAY || element->kind == DESIGNATOR_RANGE);

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

	ArrayIndex insert_index = 0;

	for (ArrayIndex index = low_index; index <= high_index; index++)
	{
		ConstInitializer *inner_value = sema_update_const_initializer_at_index(const_init, element_type, index, &insert_index);

		// Update
		if (!is_last_path_element)
		{
			sema_update_const_initializer_with_designator(inner_value, next_element, end, value);
			continue;
		}
		const_init_rewrite_to_value(inner_value, value);
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
		case VECTORS:
			sema_update_const_initializer_with_designator_array(const_init, curr, end, value);
			return;
		default:
			UNREACHABLE_VOID
	}
}

static Type *sema_expr_analyse_designator(SemaContext *context, Type *current, Expr *expr, ArrayIndex *max_index, Decl **member_ptr)
{
	ASSERT(expr->expr_kind == EXPR_DESIGNATOR);
	DesignatorElement **path = expr->designator_expr.path;

	// Walk down into this path
	bool did_report_error = false;
	*member_ptr = NULL;
	for (unsigned i = 0; i < vec_size(path); i++)
	{
		Decl *member_found;
		Type *new_current = sema_find_type_of_element(context, current, &path, &i, &did_report_error, i == 0 ? max_index : NULL, &member_found);
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

static Type *sema_resolve_vector_element_for_name(SemaContext *context, FlatType *type, DesignatorElement *element, bool *did_report_error)
{
	Expr *field = element->field_expr;
	if (field->expr_kind != EXPR_UNRESOLVED_IDENTIFIER) return NULL;
	const char *kw = field->unresolved_ident_expr.ident;
	unsigned len = strlen(kw);
	if (!sema_kw_is_swizzle(kw, len)) return NULL;
	bool is_overlapping = false;
	int index;
	if (!sema_check_swizzle_string(context, field, kw, len, type->array.len, &is_overlapping, &index))
	{
		*did_report_error = true;
		return NULL;
	}
	ArrayIndex first = SWIZZLE_INDEX(kw[0]);
	ArrayIndex last = SWIZZLE_INDEX(kw[len - 1]);

	if (is_overlapping || (first + len != last + 1))
	{
		*did_report_error = true;
		SEMA_ERROR(field, "Designated initializers using swizzling must be a contiguous range, like '.xyz = 123'.");
		return NULL;
	}
	ASSERT(last < type->array.len);
	element->index = first;
	if (len == 1)
	{
		element->kind = DESIGNATOR_ARRAY;
	}
	else
	{
		element->kind = DESIGNATOR_RANGE;
		element->index_end = last;
	}
	return type->array.base;
}

INLINE bool sema_initializer_list_is_empty(Expr *value)
{
	return expr_is_const_initializer(value) && value->const_expr.initializer->kind == CONST_INIT_ZERO;
}

static Type *sema_find_type_of_element(SemaContext *context, Type *type, DesignatorElement ***elements_ref, unsigned *curr_index, bool *did_report_error, ArrayIndex *max_index, Decl **member_ptr)
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
			case VECTORS:
				len = type_flattened->array.len;
				base = type_flattened->array.base;
				break;
			default:
				return NULL;
		}
		ArrayIndex index = sema_analyse_designator_index(context, element->index_expr);
		if (index < 0)
		{
			*did_report_error = true;
			return NULL;
		}
		if (index >= (ArrayIndex)len)
		{
			SEMA_ERROR(element->index_expr, "The index may must be less than the array length (which was %llu).", (unsigned long long)len);
			*did_report_error = true;
			return NULL;
		}

		element->index = index;
		if (max_index && *max_index < index) *max_index = index;
		if (element->kind == DESIGNATOR_RANGE)
		{
			ArrayIndex end_index = sema_analyse_designator_index(context, element->index_end_expr);
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
			if (end_index >= (ArrayIndex)len)
			{
				*did_report_error = true;
				SEMA_ERROR(element->index_end_expr, "The index must be less than the array length (which was %llu).", (unsigned long long)len);
				return NULL;
			}
			element->index_end = end_index;
			if (max_index && *max_index < end_index) *max_index = end_index;
		}
		return base;
	}
	ASSERT(element->kind == DESIGNATOR_FIELD);
	switch (type_flattened->type_kind)
	{
		case TYPE_UNION:
		case TYPE_STRUCT:
		case TYPE_BITSTRUCT:
			break;
		case TYPE_SIMD_VECTOR:
		case TYPE_VECTOR:
			*member_ptr = NULL;
			return sema_resolve_vector_element_for_name(context, type_flattened, element, did_report_error);
		default:
			return NULL;
	}
	Decl *member = sema_resolve_element_for_name(context,
	                                             type_flattened->decl->strukt.members,
	                                             elements_ref,
	                                             curr_index, type_flattened->decl->is_substruct);
	*member_ptr = member;
	if (!member) return NULL;
	return member->type;
}


static ArrayIndex sema_analyse_designator_index(SemaContext *context, Expr *index)
{
	if (!sema_analyse_expr_rvalue(context, index))
	{
		return -1;
	}

	// Unless we already have type_usz, cast to type_isz;
	if (!cast_to_index_len(context, index, false))
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
	return (ArrayIndex)index_val;
}

static Decl *sema_resolve_element_for_name(SemaContext *context, Decl **decls, DesignatorElement ***elements_ref,
                                           unsigned *index, bool is_substruct)
{
	DesignatorElement *element = (*elements_ref)[*index];

	Expr *field = sema_expr_resolve_access_child(context, element->field_expr, NULL);
	if (!field) return poisoned_decl;

	if (field->expr_kind != EXPR_UNRESOLVED_IDENTIFIER)
	{
		SEMA_ERROR(field, "An identifier was expected.");
		return poisoned_decl;
	}
	const char *name = field->unresolved_ident_expr.ident;
	unsigned old_index = *index;
	FOREACH_IDX(i, Decl *, decl, decls)
	{
		// The simple case, we have a match.
		if (decl->name == name)
		{
			element->index = (ArrayIndex)i;
			return decl;
		}
		if (!decl->name)
		{
			ASSERT_SPAN(decl, type_is_union_or_strukt(decl->type) || decl->decl_kind == DECL_BITSTRUCT);
			// Anonymous struct
			Decl *found = sema_resolve_element_for_name(context, decl->strukt.members, elements_ref, index, false);
			// No match, continue...
			if (!found) continue;

			// Create our anon field.
			DesignatorElement *anon_element = CALLOCS(DesignatorElement);
			anon_element->kind = DESIGNATOR_FIELD;
			anon_element->index = (ArrayIndex)i;
			vec_insert_at(*elements_ref, old_index, anon_element);
			// Advance
			(*index)++;
			return found;
		}
	}
	if (!is_substruct) return NULL;
	Decl *first = decls[0];
	Type *flat = type_flatten(first->type);
	if (!type_is_union_or_strukt(flat) && flat->type_kind != TYPE_BITSTRUCT) return NULL;
	if (first->decl_kind == DECL_VAR)
	{
		first = flat->decl;
	}

	Decl *found = sema_resolve_element_for_name(context, first->strukt.members, elements_ref, index, true);
	if (!found) return NULL;
	// Create our ref field.
	DesignatorElement *anon_element = CALLOCS(DesignatorElement);
	anon_element->kind = DESIGNATOR_FIELD;
	anon_element->index = 0;
	vec_insert_at(*elements_ref, old_index, anon_element);
	// Advance
	(*index)++;
	return found;
}

