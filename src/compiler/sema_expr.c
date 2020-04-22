// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "sema_internal.h"
#include "bigint.h"

/*
 * TODOs
 * - Check all returns correctly
 * - Disallow jumping in and out of an expression block.
 */

static Expr **expr_copy_expr_list_from_macro(Context *context, Expr *macro, Expr **expr_list);
static Expr *expr_copy_from_macro(Context *context, Expr *macro, Expr *source_expr);
static Ast *ast_copy_from_macro(Context *context, Expr *macro, Ast *source);
static Ast **ast_copy_list_from_macro(Context *context, Expr *macro, Ast **to_copy);

#define MACRO_COPY_EXPR(x) x = expr_copy_from_macro(context, macro, x)
#define MACRO_COPY_TYPE(x) x = type_info_copy_from_macro(context, macro, x)
#define MACRO_COPY_TYPE_LIST(x) x = type_info_copy_list_from_macro(context, macro, x)
#define MACRO_COPY_EXPR_LIST(x) x = expr_copy_expr_list_from_macro(context, macro, x)
#define MACRO_COPY_AST_LIST(x) x = ast_copy_list_from_macro(context, macro, x)
#define MACRO_COPY_AST(x) x = ast_copy_from_macro(context, macro, x)


static inline bool is_const(Expr *expr)
{
	return expr->expr_kind == EXPR_CONST;
}

static inline bool both_const(Expr *left, Expr *right)
{
	return left->expr_kind == EXPR_CONST && right->expr_kind == EXPR_CONST;
}

static inline bool both_any_integer(Expr *left, Expr *right)
{
	return type_is_any_integer(left->type->canonical) && type_is_any_integer(right->type->canonical);
}

int sema_check_comp_time_bool(Context *context, Expr *expr)
{
	if (!sema_analyse_expr_of_required_type(context, type_bool, expr)) return -1;
	if (expr->expr_kind != EXPR_CONST)
	{
		SEMA_ERROR(expr, "$if requires a compile time constant value.");
		return -1;
	}
	return expr->const_expr.b;
}

static bool expr_is_ltype(Expr *expr)
{
	switch (expr->expr_kind)
	{
		case EXPR_IDENTIFIER:
			return expr->identifier_expr.decl->decl_kind == DECL_VAR && (expr->identifier_expr.decl->var.kind == VARDECL_LOCAL
				|| expr->identifier_expr.decl->var.kind == VARDECL_GLOBAL
				|| expr->identifier_expr.decl->var.kind == VARDECL_PARAM);
		case EXPR_UNARY:
			return expr->unary_expr.operator == UNARYOP_DEREF;
		case EXPR_ACCESS:
			return expr_is_ltype(expr->access_expr.parent);
		case EXPR_GROUP:
			return expr_is_ltype(expr->group_expr);
		case EXPR_SUBSCRIPT:
			return true;
		default:
			return false;
	}
}


static inline bool sema_type_error_on_binop(Expr *expr)
{
	const char *c = token_type_to_string(binaryop_to_token(expr->binary_expr.operator));
	SEMA_ERROR(expr,
	           "%s is not defined in the expression '%s' %s '%s'.",
	           c,
	           type_to_error_string(expr->binary_expr.left->type),
	           c,
	           type_to_error_string(expr->binary_expr.right->type));
	return false;
}


static inline bool sema_expr_analyse_ternary(Context *context, Type *to, Expr *expr)
{
	Expr *left = expr->ternary_expr.then_expr;
	Expr *cond = expr->ternary_expr.cond;
	// Normal
	if (left)
	{
		if (!sema_analyse_expr(context, type_bool, cond)) return expr_poison(expr);
		if (!sema_analyse_expr(context, to, left)) return expr_poison(expr);
	}
	else
	{
		// Elvis
		if (!sema_analyse_expr(context, to, cond)) return expr_poison(expr);
		Type *type = cond->type->canonical;
		if (type->type_kind != TYPE_BOOL && cast_to_bool_kind(type) == CAST_ERROR)
		{
			SEMA_ERROR(cond, "Cannot convert expression to boolean.");
			return false;
		}
		left = cond;
	}

	Expr *right = expr->ternary_expr.else_expr;
	if (!sema_analyse_expr(context, to, right)) return expr_poison(expr);

	Type *left_canonical = left->type->canonical;
	Type *right_canonical = right->type->canonical;
	if (left_canonical != right_canonical)
	{
		Type *max = type_find_max_type(left_canonical, right_canonical);
		if (!max)
		{
			SEMA_ERROR(expr, "Cannot find a common parent type of '%s' and '%s'",
			           type_to_error_string(left_canonical), type_to_error_string(right_canonical));
			return false;
		}
		if (!cast_implicit(left, max) || !cast_implicit(right, max)) return false;
	}

	expr->type = left->type;
	return true;
}


static inline bool sema_expr_analyse_enum_constant(Expr *expr, const char *name, Decl *decl)
{
	VECEACH(decl->enums.values, i)
	{
		Decl *enum_constant = decl->enums.values[i];
		if (enum_constant->name == name)
		{
			assert(enum_constant->resolve_status == RESOLVE_DONE);
			expr->type = enum_constant->type;
			expr->const_expr = enum_constant->enum_constant.expr->const_expr;
			expr->expr_kind = EXPR_CONST;
			return true;
		}
	}
	return false;
}

static inline bool sema_expr_analyse_error_constant(Expr *expr, const char *name, Decl *decl)
{
	VECEACH(decl->error.error_constants, i)
	{
		Decl *error_constant = decl->error.error_constants[i];
		if (error_constant->name == name)
		{
			assert(error_constant->resolve_status == RESOLVE_DONE);
			expr->type = decl->type;
			expr->expr_kind = EXPR_CONST;
			expr_const_set_int(&expr->const_expr, error_constant->error_constant.value, type_error->canonical->type_kind);
			return true;
		}
	}
	return false;
}

static inline bool find_possible_inferred_identifier(Type *to, Expr *expr)
{
	if (to->canonical->type_kind != TYPE_ENUM && to->canonical->type_kind != TYPE_ERROR) return false;
	Decl *parent_decl = to->canonical->decl;
	switch (parent_decl->decl_kind)
	{
		case DECL_ENUM:
			return sema_expr_analyse_enum_constant(expr, expr->identifier_expr.identifier, parent_decl);
		case DECL_ERROR:
			return sema_expr_analyse_error_constant(expr, expr->identifier_expr.identifier, parent_decl);
		case DECL_UNION:
		case DECL_STRUCT:
			return false;
		default:
			UNREACHABLE
	}

}
static inline bool sema_expr_analyse_identifier(Context *context, Type *to, Expr *expr)
{
	Decl *ambiguous_decl;
	Decl *decl = sema_resolve_symbol(context, expr->identifier_expr.identifier, expr->identifier_expr.path, &ambiguous_decl);

	if (!decl && !expr->identifier_expr.path && to)
	{
		if (find_possible_inferred_identifier(to, expr)) return true;
	}

	if (!decl)
	{
		SEMA_ERROR(expr, "The symbol '%s' could not be found.", expr->identifier_expr.identifier);
		return false;
	}

	// Already handled
	if (!decl_ok(decl)) return false;

	if (ambiguous_decl)
	{
		SEMA_ERROR(expr,
		           "Ambiguous symbol '%s' – both defined in %s and %s, please add the module name to resolve the ambiguity",
		           expr->identifier_expr.identifier,
		           decl->module->name->module,
		           ambiguous_decl->module->name->module);
		return false;
	}

	if (decl->decl_kind == DECL_FUNC && !expr->identifier_expr.path && decl->module != context->module)
	{
		SEMA_ERROR(expr, "Functions from other modules, must be prefixed with the module name");
		return false;
	}
	if (decl->decl_kind == DECL_MACRO)
	{
		SEMA_ERROR(expr, "Macro expansions must be prefixed with '@', try using '@%s(...)' instead.", decl->name);
		return false;
	}
	assert(decl->type);
	expr->identifier_expr.decl = decl;
	expr->type = decl->type;
	return true;
}

static inline bool sema_expr_analyse_binary_sub_expr(Context *context, Type *to, Expr *left, Expr *right)
{
	return sema_analyse_expr(context, to, left) & sema_analyse_expr(context, to, right);
}

static inline bool sema_expr_analyse_var_call(Context *context, Type *to, Expr *expr) { TODO }
static inline bool sema_expr_analyse_generic_call(Context *context, Type *to, Expr *expr) { TODO };


static inline int find_index_of_named_parameter(Decl** func_params, Expr *expr)
{
	if (expr->expr_kind != EXPR_IDENTIFIER || expr->identifier_expr.path)
	{
		SEMA_ERROR(expr, "Expected the name of a function parameter here, enclose the assignment expression in ().");
		return -1;
	}
	const char *name = expr->identifier_expr.identifier;
	VECEACH(func_params, i)
	{
		if (func_params[i]->name == name) return (int)i;
	}
	SEMA_ERROR(expr, "There's no parameter with the name '%s', if you want an assignment expression, enclose it in ().", name);
	return -1;
}

static inline bool sema_expr_analyse_func_call(Context *context, Type *to, Expr *expr, Decl *decl)
{
	Expr **args = expr->call_expr.arguments;
	FunctionSignature *signature = &decl->func.function_signature;
	Decl **func_params = signature->params;
	unsigned error_params = signature->throw_any || signature->throws;
	if (error_params)
	{
		if (context->try_nesting == 0)
		{
			SEMA_ERROR(expr, "Function '%s' throws errors, this call must be prefixed 'try'.", decl->name);
			return false;
		}
	}
	unsigned func_param_count = vec_size(func_params);
	unsigned num_args = vec_size(args);
	unsigned entries_needed = func_param_count > num_args ? func_param_count : num_args;
	Expr **actual_args = VECNEW(Expr*, entries_needed);
	for (unsigned i = 0; i < entries_needed; i++) vec_add(actual_args, NULL);
	memset(actual_args, 0, entries_needed * sizeof(Expr*));
	bool uses_named_parameters = false;

	for (unsigned i = 0; i < num_args; i++)
	{
		Expr *arg = args[i];
		// Named parameters
		if (arg->expr_kind == EXPR_BINARY && arg->binary_expr.operator == BINARYOP_ASSIGN)
		{
			uses_named_parameters = true;
			int index = find_index_of_named_parameter(func_params, arg->binary_expr.left);
			if (index < 0) return false;
			if (actual_args[index])
			{
				SEMA_ERROR(arg, "The parameter '%s' was already set once.", func_params[index]->name);
				return false;
			}
			if (!sema_analyse_expr_of_required_type(context, func_params[index]->type, arg->binary_expr.right)) return false;
			actual_args[index] = arg->binary_expr.right;
			continue;
		}

		if (i >= func_param_count)
		{
			if (!signature->variadic)
			{
				SEMA_ERROR(expr, "Too many parameters for this function.");
				return false;
			}
			if (!sema_analyse_expr_of_required_type(context, NULL, arg)) return false;
			actual_args[i] = arg;
			continue;
		}

		if (uses_named_parameters)
		{
			SEMA_ERROR(expr, "A regular parameter cannot follow a named parameter.");
			return false;
		}
		if (!sema_analyse_expr_of_required_type(context, func_params[i]->type, arg)) return false;
		actual_args[i] = arg;
	}
	for (unsigned i = 0; i < entries_needed; i++)
	{
		if (actual_args[i]) continue;

		if (func_params[i]->var.init_expr)
		{
			actual_args[i] = func_params[i]->var.init_expr;
			continue;
		}

		SEMA_ERROR(expr, "Parameter '%s' was not set.", func_params[i]->name);
		return false;
	}
	expr->type = decl->func.function_signature.rtype->type;
	expr->call_expr.arguments = actual_args;
	return true;
}

static inline bool sema_expr_analyse_call(Context *context, Type *to, Expr *expr)
{
	// TODO
	Expr *func_expr = expr->call_expr.function;
	// TODO check
	if (!sema_analyse_expr(context, to, func_expr)) return false;
	Decl *decl;
	switch (func_expr->expr_kind)
	{
		case EXPR_TYPE_ACCESS:
			decl = func_expr->type_access.method;
			break;
		case EXPR_IDENTIFIER:
			decl = func_expr->identifier_expr.decl;
			break;
		default:
			TODO
	}
	switch (decl->decl_kind)
	{
		case DECL_VAR:
			return sema_expr_analyse_var_call(context, to, expr);
		case DECL_FUNC:
			return sema_expr_analyse_func_call(context, to, expr, decl);
		case DECL_MACRO:
			SEMA_ERROR(expr, "Macro calls must be preceeded by '@'.");
			return false;
		case DECL_GENERIC:
			return sema_expr_analyse_generic_call(context, to, expr);
		case DECL_POISONED:
			return false;
		default:
			SEMA_ERROR(expr, "The expression cannot be called.");
			return false;
	}
}

static inline bool sema_expr_analyse_subscript_after_parent_resolution(Context *context, Type *parent, Expr *expr)
{
	assert(expr->expr_kind == EXPR_SUBSCRIPT);
	Expr *subscripted = expr->subscript_expr.expr;
	Type *type = parent ? parent->canonical : subscripted->type->canonical;
	Expr *index = expr->subscript_expr.index;
	Type *inner_type = type_get_indexed_type(type);
	if (!inner_type)
	{
		SEMA_ERROR((parent ? expr : subscripted), "Cannot index '%s'.", type_to_error_string(type));
		return false;
	}

	if (!sema_analyse_expr(context, type_isize, index)) return false;

	// Unless we already have type_usize, cast to type_isize;
	if (index->type->canonical->type_kind != type_usize->canonical->type_kind)
	{
		if (!cast_implicit(index, type_isize)) return false;
	}
	// Check range
	if (index->expr_kind == EXPR_CONST)
	{
		switch (type->type_kind)
		{
			case TYPE_ARRAY:
			{
				BigInt size;
				bigint_init_unsigned(&size, type->array.len);
				if (bigint_cmp(&size, &index->const_expr.i) != CMP_GT)
				{
					SEMA_ERROR(index, "Array index out of bounds, was %s, exceeding max index of %llu.",
							bigint_to_error_string(&index->const_expr.i, 10), type->array.len - 1);
					return false;
				}
				FALLTHROUGH;
			}
			case TYPE_VARARRAY:
			case TYPE_SUBARRAY:
				if (bigint_cmp_zero(&index->const_expr.i) == CMP_LT)
				{
					SEMA_ERROR(index, "Array index out of bounds, was %s.", bigint_to_error_string(&index->const_expr.i, 10));
					return false;
				}
				break;
			default:
				break;
		}
	}
	expr->type = inner_type;
	return true;
}

static inline bool sema_expr_analyse_subscript(Context *context, Type *to, Expr *expr)
{
	if (!sema_analyse_expr(context, NULL, expr->subscript_expr.expr)) return false;

	return sema_expr_analyse_subscript_after_parent_resolution(context, NULL, expr);
}

static inline bool sema_expr_analyse_method_function(Context *context, Expr *expr, Decl *decl, bool is_pointer)
{
	const char *name = expr->access_expr.sub_element.string;
	VECEACH(decl->method_functions, i)
	{
		Decl *function = decl->method_functions[i];
		if (function->name == name)
		{
			// TODO
			return true;
		}
	}
	SEMA_ERROR(expr, "Cannot find method function '%s.%s'", decl->name, name);
	return false;
}


static Decl *strukt_recursive_search_member(Decl *strukt, const char *name)
{
	VECEACH(strukt->strukt.members, i)
	{
		Decl *member = strukt->strukt.members[i];
		if (member->name == name) return member;
		if (!member->name && type_is_structlike(member->type->canonical))
		{
			Decl *result = strukt_recursive_search_member(member->type->canonical->decl, name);
			if (result)
			{
				return result;
			}
		}
	}
	return NULL;
}

static inline bool sema_expr_analyse_group(Context *context, Type *to, Expr *expr)
{
	if (!sema_analyse_expr(context, to, expr->group_expr)) return false;
	*expr = *expr->group_expr;
	return true;
}

static inline bool sema_expr_analyse_access(Context *context, Expr *expr)
{
	if (!sema_analyse_expr(context, NULL, expr->access_expr.parent)) return false;

	assert(expr->expr_kind == EXPR_ACCESS);
	assert(expr->access_expr.parent->resolve_status == RESOLVE_DONE);

	Type *parent_type = expr->access_expr.parent->type;
	Type *type = parent_type->canonical;
	bool is_pointer = type->type_kind == TYPE_POINTER;
	if (is_pointer)
	{
		type = type->pointer;
	}
	if (!type_may_have_method_functions(type))
	{
		SEMA_ERROR(expr, "Cannot access '%s' on '%s'", expr->access_expr.sub_element.string, type_to_error_string(parent_type));
		return false;
	}
	Decl *decl = type->decl;
	switch (decl->decl_kind)
	{
		case DECL_ENUM:
		case DECL_ERROR:
			return sema_expr_analyse_method_function(context, expr, decl, is_pointer);
		case DECL_STRUCT:
		case DECL_UNION:
			break;
		default:
			UNREACHABLE
	}
	Decl *member = strukt_recursive_search_member(decl, expr->access_expr.sub_element.string);
	if (!member)
	{
		SEMA_TOKEN_ERROR(expr->access_expr.sub_element, "There is no element '%s.%s'.", decl->name, expr->access_expr.sub_element.string);
		return false;
	}
	if (is_pointer)
	{
		Expr *deref = expr_new(EXPR_UNARY, expr->span);
		deref->unary_expr.operator = UNARYOP_DEREF;
		deref->unary_expr.expr = expr->access_expr.parent;
		deref->resolve_status = RESOLVE_DONE;
		deref->type = type;
		expr->access_expr.parent = deref;
	}
	expr->type = member->type;
	expr->access_expr.ref = member;
	return true;
}


static inline bool sema_expr_analyse_type_access(Context *context, Type *to, Expr *expr)
{
	TypeInfo *type_info = expr->type_access.type;
	if (!sema_resolve_type_info(context, type_info)) return false;
	Type *canonical = type_info->type->canonical;
	if (expr->type_access.name.type == TOKEN_TYPEID)
	{
		expr->type = type_typeid;
		expr->expr_kind = EXPR_TYPEID;
		expr->typeid_expr = type_info;
		expr->resolve_status = RESOLVE_DONE;
		return true;
	}
	if (!type_may_have_method_functions(canonical))
	{
		SEMA_ERROR(expr, "'%s' does not have method functions.", type_to_error_string(type_info->type));
		return false;
	}
	Decl *decl = canonical->decl;
	// TODO add more constants that can be inspected?
	// e.g. SomeEnum.values, MyUnion.x.offset etc?
	switch (decl->decl_kind)
	{
		case DECL_ENUM:
			if (expr->type_access.name.type == TOKEN_CONST_IDENT)
			{
				if (!sema_expr_analyse_enum_constant(expr, expr->type_access.name.string, decl))
				{
					SEMA_ERROR(expr, "'%s' has no enumeration value '%s'.", decl->name, expr->type_access.name.string);
					return false;
				}
				return true;
			}
			break;
		case DECL_ERROR:
			if (expr->type_access.name.type == TOKEN_CONST_IDENT)
			{
				if (!sema_expr_analyse_error_constant(expr, expr->type_access.name.string, decl))
				{
					SEMA_ERROR(expr, "'%s' has no error type '%s'.", decl->name, expr->type_access.name.string);
					return false;
				}
				return true;
			}
			break;
		case DECL_UNION:
		case DECL_STRUCT:
			break;
		default:
			UNREACHABLE
	}
	VECEACH(type_info->type->decl->method_functions, i)
	{
		Decl *function = type_info->type->decl->method_functions[i];
		if (expr->type_access.name.string == function->name)
		{
			expr->type_access.method = function;
			expr->type = function->func.function_signature.rtype->type;
			return true;
		}
	}
	SEMA_ERROR(expr, "No function '%s.%s' found.", type_to_error_string(type_info->type), expr->type_access.name.string);
	return false;
}

static DesignatedPath *sema_analyse_init_path(Context *context, DesignatedPath *parent, Expr *expr, bool *has_found_match, bool *has_reported_error);

static DesignatedPath *sema_analyse_init_identifier_string(Context *context, DesignatedPath *parent_path, const char *string, bool *has_found_match, bool *has_reported_error)
{
	assert(type_is_structlike(parent_path->type));
	Decl **members = parent_path->type->decl->strukt.members;
	VECEACH(members, i)
	{
		Decl *member = members[i];
		if (member->name == string)
		{
			DesignatedPath *sub_path = CALLOCS(DesignatedPath);
			sub_path->type = member->type;
			sub_path->kind = DESIGNATED_IDENT;
			sub_path->index = i;
			parent_path->sub_path = sub_path;
			*has_found_match = true;
			return sub_path;
		}
		if (!member->name)
		{
			DesignatedPath temp_path;
			temp_path.type = member->type;
			DesignatedPath *found = sema_analyse_init_identifier_string(context, &temp_path, string, has_found_match, has_reported_error);
			if (!found) continue;
			DesignatedPath *real_path = malloc_arena(sizeof(DesignatedPath));
			*real_path = temp_path;
			real_path->index = i;
			real_path->kind = DESIGNATED_IDENT;
			parent_path->sub_path = real_path;
			*has_found_match = true;
			return found;
		}
	}
	return NULL;
}


static DesignatedPath *sema_analyse_init_access(Context *context, DesignatedPath *parent, Expr *access_expr, bool *has_found_match, bool *has_reported_error)
{
	DesignatedPath *last_path = sema_analyse_init_path(context, parent, access_expr->access_expr.parent, has_found_match, has_reported_error);
	if (!last_path) return NULL;
	DesignatedPath *path = sema_analyse_init_identifier_string(context, last_path, access_expr->access_expr.sub_element.string, has_found_match, has_reported_error);
	if (!path && has_found_match && !has_reported_error)
	{
		SEMA_TOKEN_ERROR(access_expr->access_expr.sub_element, "'%s' is not a valid sub member.", access_expr->access_expr.sub_element.string);
		*has_reported_error = true;
	}
	return path;
}

static bool expr_cast_to_index(Expr *index)
{
	if (index->expr_kind == EXPR_RANGE)
	{
		TODO
	}
	if (index->type->canonical->type_kind == type_usize->canonical->type_kind) return true;
	return cast_implicit(index, type_isize);
}

static bool expr_check_index_in_range(Type *type, Expr *index)
{
	if (index->expr_kind == EXPR_RANGE)
	{
		return expr_check_index_in_range(type, index->range_expr.left) & expr_check_index_in_range(type, index->range_expr.right);
	}
	assert(type == type->canonical);
	if (index->expr_kind == EXPR_CONST)
	{
		switch (type->type_kind)
		{
			case TYPE_ARRAY:
			{
				BigInt size;
				bigint_init_unsigned(&size, type->array.len);
				if (bigint_cmp(&size, &index->const_expr.i) != CMP_GT)
				{
					SEMA_ERROR(index, "Array index out of bounds, was %s, exceeding max index of %llu.",
					           bigint_to_error_string(&index->const_expr.i, 10), type->array.len - 1);
					return false;
				}
				FALLTHROUGH;
			}
			case TYPE_VARARRAY:
			case TYPE_SUBARRAY:
				if (bigint_cmp_zero(&index->const_expr.i) == CMP_LT)
				{
					SEMA_ERROR(index, "Array index out of bounds, was %s.", bigint_to_error_string(&index->const_expr.i, 10));
					return false;
				}
				break;
			case TYPE_STRING:
				TODO
			default:
				UNREACHABLE
		}
	}
	return true;
}
static DesignatedPath *sema_analyse_init_subscript(Context *context, DesignatedPath *parent, Expr *expr, bool *has_found_match, bool *has_reported_error)
{
	assert(expr->expr_kind == EXPR_SUBSCRIPT);
	DesignatedPath *path = parent;
	if (expr->subscript_expr.expr)
	{
		path = sema_analyse_init_path(context, parent, expr->subscript_expr.expr, has_found_match, has_reported_error);
	}
	if (!path) return NULL;

	Type *type = path->type;
	if (type->canonical->type_kind == TYPE_POINTER)
	{
		SEMA_ERROR(expr, "It's not possible to subscript a pointer field in a designated initializer.");
		*has_reported_error = true;
		return NULL;
	}

	Expr *index = expr->subscript_expr.index;
	Type *inner_type = type_get_indexed_type(type);
	if (!inner_type)
	{
		SEMA_ERROR(expr, "Not possible to index a value of type '%s'.", type_to_error_string(type));
		*has_reported_error = true;
		return NULL;
	}
	if (!sema_analyse_expr(context, type_isize, index))
	{
		*has_reported_error = true;
		return NULL;
	}

	// Unless we already have type_usize, cast to type_isize;
	if (!expr_cast_to_index(index))
	{
		*has_reported_error = true;
		return NULL;
	}

	// Check range
	if (!expr_check_index_in_range(type->canonical, index))
	{
		*has_reported_error = true;
		return NULL;
	}

	DesignatedPath *sub_path = CALLOCS(DesignatedPath);
	path->sub_path = sub_path;
	sub_path->type = inner_type;
	sub_path->kind = DESIGNATED_SUBSCRIPT;
	sub_path->index_expr = index;
	*has_found_match = true;
	return sub_path;
}

static DesignatedPath *sema_analyse_init_path(Context *context, DesignatedPath *parent, Expr *expr, bool *has_found_match, bool *has_reported_error)
{
	switch (expr->expr_kind)
	{
		case EXPR_ACCESS:
			return sema_analyse_init_access(context, parent, expr, has_found_match, has_reported_error);
		case EXPR_IDENTIFIER:
			return sema_analyse_init_identifier_string(context, parent, expr->identifier_expr.identifier, has_found_match, has_reported_error);
		case EXPR_SUBSCRIPT:
			return sema_analyse_init_subscript(context, parent, expr, has_found_match, has_reported_error);
		default:
			return NULL;
	}
}



static bool sema_expr_analyse_designated_initializer(Context *context, Type *assigned, Expr *initializer)
{
	Expr **init_expressions = initializer->expr_initializer.initializer_expr;
	bool is_structlike = type_is_structlike(assigned->canonical);

	VECEACH(init_expressions, i)
	{
		Expr *expr = init_expressions[i];
		// 1. Ensure that're seeing expr = expr on the top level.
		if (expr->expr_kind != EXPR_BINARY || expr->binary_expr.operator != BINARYOP_ASSIGN)
		{
			if (is_structlike)
			{
				SEMA_ERROR(expr, "Expected an initializer on the format 'foo = 123' here.");
			}
			else
			{
				SEMA_ERROR(expr, "Expected an initializer on the format '[1] = 123' here.");
			}
			return false;
		}
		Expr *init_expr = expr->binary_expr.left;
		DesignatedPath path = { .type = assigned };
		bool has_reported_error = false;
		bool has_found_match = false;
		DesignatedPath *last_path = sema_analyse_init_path(context, &path, init_expr, &has_found_match, &has_reported_error);
		if (!has_reported_error && !last_path)
		{
			SEMA_ERROR(expr, "This is not a valid member of '%s'.", type_to_error_string(assigned));
			return false;
		}
		Expr *value = expr->binary_expr.right;
		if (!sema_analyse_expr_of_required_type(context, last_path->type, value)) return false;
		expr->expr_kind = EXPR_DESIGNATED_INITIALIZER;
		expr->designated_init_expr.path = path.sub_path;
		expr->designated_init_expr.value = value;
		expr->resolve_status = RESOLVE_DONE;
	}
	initializer->expr_initializer.init_type = INITIALIZER_DESIGNATED;
	return true;
}

/**
 * Perform analysis for a plain initializer, that is one initializing all fields.
 * @return true if analysis succeeds.
 */
static inline bool sema_expr_analyse_struct_plain_initializer(Context *context, Decl *assigned, Expr *initializer)
{
	Expr **elements = initializer->expr_initializer.initializer_expr;
	Decl **members = assigned->strukt.members;
	initializer->expr_initializer.init_type = INITIALIZER_NORMAL;
	unsigned size = vec_size(elements);
	unsigned expected_members = vec_size(members);

	// 1. For struct number of members must be the same as the size of the struct.
	//    Since we already handled the case with an empty initializer before going here
	//    zero entries must be an error.
	assert(size > 0 && "We should already have handled the size == 0 case.");
	if (expected_members == 0)
	{
		// Generate a nice error message for zero.
		SEMA_ERROR(elements[0], "Too many elements in initializer, it must be empty.");
		return false;
	}

	// 2. In case of a union, only expect a single entry.
	if (assigned->decl_kind == DECL_UNION) expected_members = 1;


	// 3. Loop through all elements.
	VECEACH(elements, i)
	{
		// 4. Check if we exceeded the list of elements in the struct/union.
		//    This way we can check the other elements which might help the
		//    user pinpoint where they put the double elements.
		if (i >= expected_members)
		{
			SEMA_ERROR(elements[i], "Too many elements in initializer, expected only %d.", expected_members);
			return false;
		}
		// 5. We know the required type, so resolve the expression.
		if (!sema_analyse_expr_of_required_type(context, members[i]->type, elements[i])) return false;
	}

	// 6. There's the case of too few values as well. Mark the last element as wrong.
	if (expected_members > size)
	{
		SEMA_ERROR(elements[size - 1], "Too few elements in initializer, there should be elements after this one.");
		return false;
	}

	// 7. Done!
	return true;
}


/**
 * Perform analysis for a plain initializer, that is one initializing all fields.
 * @return true if analysis succeeds.
 */
static inline bool sema_expr_analyse_array_plain_initializer(Context *context, Type *assigned, Expr *initializer)
{
	Expr **elements = initializer->expr_initializer.initializer_expr;

	assert(assigned->type_kind == TYPE_ARRAY && "The other types are not done yet.");

	Type *inner_type = type_get_indexed_type(assigned);
	assert(inner_type);


	initializer->expr_initializer.init_type = INITIALIZER_NORMAL;
	unsigned size = vec_size(elements);
	unsigned expected_members = assigned->array.len;

	assert(size > 0 && "We should already have handled the size == 0 case.");
	if (expected_members == 0)
	{
		// Generate a nice error message for zero.
		SEMA_ERROR(elements[0], "Too many elements in initializer, it must be empty.");
		return false;
	}

	VECEACH(elements, i)
	{
		if (i >= expected_members)
		{
			SEMA_ERROR(elements[i], "Too many elements in initializer, expected only %d.", expected_members);
			return false;
		}
		if (!sema_analyse_expr_of_required_type(context, inner_type, elements[i])) return false;
	}

	if (expected_members > size)
	{
		SEMA_ERROR(elements[size - 1], "Too few elements in initializer, %d elements are needed.", expected_members);
		return false;
	}

	// 7. Done!
	return true;
}

static inline bool sema_expr_analyse_initializer(Context *context, Type *assigned, Expr *expr)
{
	expr->type = assigned;

	Expr **init_expressions = expr->expr_initializer.initializer_expr;

	// 1. Zero size init will initialize to empty.
	if (vec_size(init_expressions) == 0)
	{
		expr->expr_initializer.init_type = INITIALIZER_ZERO;
		return true;
	}

	// 2. Check if we might have a designated initializer
	//    this means that in this case we're actually not resolving macros here.
	if (init_expressions[0]->expr_kind == EXPR_BINARY && init_expressions[0]->binary_expr.operator == BINARYOP_ASSIGN)
	{
		return sema_expr_analyse_designated_initializer(context, assigned, expr);
	}

	// 3. Otherwise use the plain initializer.
	if (assigned->type_kind == TYPE_ARRAY)
	{
		return sema_expr_analyse_array_plain_initializer(context, assigned, expr);
	}
	else
	{
		return sema_expr_analyse_struct_plain_initializer(context, assigned->decl, expr);
	}
}

static inline bool sema_expr_analyse_initializer_list(Context *context, Type *to, Expr *expr)
{
	assert(to);
	Type *assigned = to->canonical;
	assert(assigned);
	switch (assigned->type_kind)
	{
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_ARRAY:
			return sema_expr_analyse_initializer(context, assigned, expr);
		case TYPE_VARARRAY:
			TODO
		default:
			break;
	}
	// Fix error on compound literals
	SEMA_ERROR(expr, "Cannot assign expression to '%s'.", type_to_error_string(to));
	return false;
}



static inline bool sema_expr_analyse_expr_list(Context *context, Type *to, Expr *expr)
{
	bool success = true;
	size_t last = vec_size(expr->expression_list) - 1;
	VECEACH(expr->expression_list, i)
	{
		success &= sema_analyse_expr_of_required_type(context, i == last ? to : NULL, expr->expression_list[i]);
	}
	return success;
}

static inline bool sema_expr_analyse_cast(Context *context, Type *to, Expr *expr)
{
	Expr *inner = expr->cast_expr.expr;
	if (!sema_resolve_type_info(context, expr->cast_expr.type_info)) return false;
	if (!sema_analyse_expr_of_required_type(context, NULL, inner)) return false;

	if (!cast(inner, expr->cast_expr.type_info->type, CAST_TYPE_EXPLICIT)) return false;

	// TODO above is probably not right, cast type not set.
	// Overwrite cast.
	SourceRange loc = expr->span;
	*expr = *inner;
	expr->span = loc;

	return true;
}

/**
 * Analyse a = b
 * @return true if analysis works
 */
static bool sema_expr_analyse_assign(Context *context, Expr *expr, Expr *left, Expr *right)
{
	// 1. Evaluate left side
	if (!sema_analyse_expr(context, NULL, left)) return false;

	// 2. Check assignability
	if (!expr_is_ltype(left))
	{
		SEMA_ERROR(left, "Expression is not assignable.");
		return false;
	}

	// 3. Evaluate right side to required type.
	if (!sema_analyse_expr_of_required_type(context, left->type, right)) return false;

	// 4. Set the result to the type on the right side.
	expr->type = right->type;

	return true;
}


/**
 * Analyse *%= *= /= %= ^= |= &=
 *
 * @return true if analysis worked.
 */
static bool sema_expr_analyse_common_assign(Context *context, Expr *expr, Expr *left, Expr *right, bool int_only)
{
	// 1. Analyse left side.
	if (!sema_analyse_expr(context, NULL, left)) return false;

	// 2. Verify that the left side is assignable.
	if (!expr_is_ltype(left))
	{
		SEMA_ERROR(left, "Expression is not assignable.");
		return false;
	}

	// 3. If this is only defined for ints (*%, ^= |= &= %=) verify that this is an int.
	if (int_only && !type_is_any_integer(left->type))
	{
		SEMA_ERROR(left, "Expected an integer here.");
		return false;
	}

	// 4. In any case, these ops are only defined on numbers.
	if (!type_is_numeric(left->type))
	{
		SEMA_ERROR(left, "Expected a numeric type here.");
		return false;
	}

	// 5. Cast the right hand side to the one on the left
	if (!sema_analyse_expr_of_required_type(context, left->type->canonical, right)) return false;

	// 6. Check for zero in case of div or mod.
	if (right->expr_kind == EXPR_CONST)
	{
		if (expr->binary_expr.operator == BINARYOP_DIV_ASSIGN)
		{
			switch (right->const_expr.kind)
			{
				case ALL_INTS:
					if (bigint_cmp_zero(&right->const_expr.i) == CMP_EQ)
					{
						SEMA_ERROR(right, "Division by zero not allowed.");
						return false;
					}
					break;
				case ALL_FLOATS:
					if (right->const_expr.f == 0)
					{
						SEMA_ERROR(right, "Division by zero not allowed.");
						return false;
					}
					break;
				default:
					UNREACHABLE
			}
		}
		else if (expr->binary_expr.operator == BINARYOP_MOD_ASSIGN)
		{
			switch (right->const_expr.kind)
			{
				case ALL_INTS:
					if (bigint_cmp_zero(&right->const_expr.i) == CMP_EQ)
					{
						SEMA_ERROR(right, "% by zero not allowed.");
						return false;
					}
					break;
				default:
					UNREACHABLE
			}
		}
	}

	// 7. Assign type
	expr->type = left->type;
	return true;
}


static BinaryOp binary_mod_op_to_non_mod(BinaryOp op)
{
	switch (op)
	{
		case BINARYOP_MULT_MOD:
			return BINARYOP_MULT;
		case BINARYOP_MULT_MOD_ASSIGN:
			return BINARYOP_MULT_ASSIGN;
		case BINARYOP_SUB_MOD:
			return BINARYOP_SUB;
		case BINARYOP_SUB_MOD_ASSIGN:
			return BINARYOP_SUB_ASSIGN;
		case BINARYOP_ADD_MOD:
			return BINARYOP_ADD;
		case BINARYOP_ADD_MOD_ASSIGN:
			return BINARYOP_ADD_ASSIGN;
		default:
			return op;
	}
}
/**
 * Handle a += b, a +%= b, a -= b, a -%= b
 * @return true if analysis succeeded.
 */
static bool sema_expr_analyse_add_sub_assign(Context *context, Expr *expr, Expr *left, Expr *right)
{
	bool is_mod = expr->binary_expr.operator == BINARYOP_ADD_MOD_ASSIGN
	              || expr->binary_expr.operator == BINARYOP_SUB_MOD_ASSIGN;

	// 1. Analyse the left hand side
	if (!sema_analyse_expr(context, NULL, left)) return false;

	// 2. Ensure the left hand side is assignable
	if (!expr_is_ltype(left))
	{
		SEMA_ERROR(left, "Expression is not assignable.");
		return false;
	}

	Type *left_type_canonical = left->type->canonical;
	expr->type = left->type;

	// 3. Attempt to analyse and cast this side to the same type if possible.
	if (!sema_analyse_expr(context, left->type, right)) return false;

	// 4. In the pointer case we have to treat this differently.
	if (left_type_canonical->type_kind == TYPE_POINTER)
	{
		// 5. Prevent +%= and -%=
		if (is_mod)
		{
			SEMA_ERROR(expr, "Cannot use %s with pointer arithmetics, use %s instead.",
					token_type_to_string(binaryop_to_token(expr->binary_expr.operator)),
					token_type_to_string(binaryop_to_token(binary_mod_op_to_non_mod(expr->binary_expr.operator))));
			return false;
		}

		// 5. Convert any compile time values to runtime
		cast_to_smallest_runtime(right);

		// 6. Finally, check that the right side is indeed an integer.
		if (!type_is_integer(right->type->canonical))
		{
			SEMA_ERROR(right, "The right side was '%s' but only integers are valid on the right side of %s when the left side is a pointer.",
			           type_to_error_string(right->type),
			           token_type_to_string(binaryop_to_token(expr->binary_expr.operator)));
			return false;
		}
		return true;
	}

	// 5. Otherwise we cast rhs to lhs
	if (!cast_implicit(right, left->type)) return false;

	// 6. We expect a numeric type on both left and right
	if (!type_is_numeric(left->type))
	{
		SEMA_ERROR(left, "Expected a numeric type here.");
		return false;
	}

	// 7. Prevent +%= and -%= on non integers
	if (is_mod && !type_is_integer(left->type->canonical))
	{
		SEMA_ERROR(expr, "%s can only be used for integer arithmetics, for other cases use %s instead.",
		           token_type_to_string(binaryop_to_token(expr->binary_expr.operator)),
		           token_type_to_string(binaryop_to_token(binary_mod_op_to_non_mod(expr->binary_expr.operator))));
		return false;
	}

	return true;
}

static bool binary_arithmetic_promotion(Expr *left, Expr *right, Type *left_type, Type *right_type)
{
	Type *max = type_find_max_type(left_type, right_type);
	return max && type_is_numeric(max) && cast_implicit(left, max) && cast_implicit(right, max);
}

/**
 * Analyse a - b
 * @return true if analysis succeeded
 */
static bool sema_expr_analyse_sub(Context *context, Type *to, Expr *expr, Expr *left, Expr *right)
{
	// TODO enums

	bool is_mod = expr->binary_expr.operator == BINARYOP_SUB_MOD;

	// 1. Analyse a and b. Do not push down if this is a -%
	if (!sema_expr_analyse_binary_sub_expr(context, is_mod ? NULL : to, left, right)) return false;

	Type *left_type = left->type->canonical;
	Type *right_type = right->type->canonical;

	// 2. Handle the ptr - x and ptr - other_pointer
	if (left_type->type_kind == TYPE_POINTER)
	{
		// 3. Is this -%? That's not ok for pointer maths.
		if (is_mod)
		{
			SEMA_ERROR(expr, "'-%%' is not valid for pointer maths, use '-' instead.");
			return false;
		}

		// 4. ptr - other pointer
		if (right_type->type_kind == TYPE_POINTER)
		{
			// 5. Require that both types are the same.
			if (left_type != right_type)
			{
				SEMA_ERROR(expr, "'%s' - '%s' is not allowed. Subtracting pointers of different types from each other is not possible.", type_to_error_string(left_type), type_to_error_string(right_type));
				return false;
			}
			// 5. usize only if that is the recipient
			if (to && to->canonical->type_kind == type_usize->canonical->type_kind)
			{
				expr->type = to;
				return true;
			}
			expr->type = type_isize;
			return true;
		}

		// 5. Cast any compile time int into smallest runtime version if we have a compile time constant.
		cast_to_smallest_runtime(right);

		// 6. No need for further casts, just it is an integer.
		if (!type_is_integer(right_type))
		{
			SEMA_ERROR(expr, "Cannot subtract '%s' from '%s'", type_to_error_string(left_type), type_to_error_string(right_type));
			return false;
		}

		expr->type = left->type;
		return true;
	}

	// 7. Attempt arithmetic promotion, to promote both to a common type.
	if (!binary_arithmetic_promotion(left, right, left_type, right_type))
	{
		SEMA_ERROR(expr, "Cannot subtract '%s' from '%s'", type_to_error_string(left_type), type_to_error_string(right_type));
		return false;
	}

	// 8. Handle constant folding.
	if (both_const(left, right))
	{
		switch (left->const_expr.kind)
		{
			case ALL_INTS:
				bigint_sub(&expr->const_expr.i, &left->const_expr.i, &right->const_expr.i);
				break;
			case ALL_FLOATS:
				// IMPROVE precision.
				expr->const_expr.f = left->const_expr.f - right->const_expr.f;
				break;
			default:
				UNREACHABLE
		}
		expr->expr_kind = EXPR_CONST;
		expr->const_expr.kind = left->const_expr.kind;
	}

	// 9. Is this -%? That's not ok unless we are adding integers.
	if (is_mod && !type_is_any_integer(left->type->canonical))
	{
		SEMA_ERROR(expr, "'-%%' is only valid for integer subtraction, use '-' instead.");
		return false;
	}

	expr->type = left->type;
	return true;

}

/**
 * Analyse a + b / a +% b
 * @return true if it succeeds.
 */
static bool sema_expr_analyse_add(Context *context, Type *to, Expr *expr, Expr *left, Expr *right)
{
	// TODO enums

	bool is_mod = expr->binary_expr.operator == BINARYOP_ADD_MOD;

	// 1. Promote everything to the recipient type – if possible
	//    this is safe in the pointer case actually.
	if (!sema_expr_analyse_binary_sub_expr(context, is_mod ? NULL : to, left, right)) return false;

	Type *left_type = left->type->canonical;
	Type *right_type = right->type->canonical;


	// 2. To detect pointer additions, reorder if needed
	if (right_type->type_kind == TYPE_POINTER && left_type->type_kind != TYPE_POINTER)
	{
		Expr *temp = right;
		right = left;
		left = temp;
		right_type = left_type;
		left_type = left->type->canonical;
	}

	// 4. The "left" will now always be the pointer.
	//    so check if we want to do the normal pointer add special handling.
	if (left_type->type_kind == TYPE_POINTER)
	{
		// 4a. Check that the other side is an integer of some sort.
		if (!type_is_any_integer(right_type))
		{
			SEMA_ERROR(right, "A value of type '%s' cannot be added to '%s', an integer was expected here.",
			           type_to_error_string(right->type),
			           type_to_error_string(left->type));
			return false;
		}

		// 4b. Cast it to usize or isize depending on underlying type.
		//     Either is fine, but it looks a bit nicer if we actually do this and keep the sign.
		bool success = cast_implicit(right, type_is_unsigned(right_type) ? type_usize : type_isize);
		// No need to check the cast we just ensured it was an integer.
		assert(success && "This should always work");

		// 4c. Set the type.
		expr->type = left->type;

		// 4d. Is this +%? That's not ok for pointers!
		if (is_mod)
		{
			SEMA_ERROR(expr, "You cannot use '+%%' with pointer addition, use '+' instead.");
			return false;
		}
		return true;
	}

	// 5. Do the binary arithmetic promotion (finding a common super type)
	//    If none can be find, send an error.
	if (!binary_arithmetic_promotion(left, right, left_type, right_type))
	{
		SEMA_ERROR(expr, "Cannot add '%s' to '%s'", type_to_error_string(left_type), type_to_error_string(right_type));
		return false;
	}

	// 6. Handle the "both const" case. We should only see ints and floats at this point.
	if (both_const(left, right))
	{
		switch (left->const_expr.kind)
		{
			case ALL_INTS:
				bigint_add(&expr->const_expr.i, &left->const_expr.i, &right->const_expr.i);
				break;
			case ALL_FLOATS:
				expr->const_expr.f = left->const_expr.f + right->const_expr.f;
				break;
			default:
				UNREACHABLE
		}
		expr->expr_kind = EXPR_CONST;
		expr->const_expr.kind = left->const_expr.kind;
	}

	// 7. Is this +%? That's not ok unless we are adding integers.
	if (is_mod && !type_is_any_integer(left->type->canonical))
	{
		SEMA_ERROR(expr, "'+%%' is only valid for integer addition, use '+' instead.");
		return false;
	}

	// 7. Set the type
	expr->type = left->type;
	return true;

}

/**
 * Analyse a * b and a *% b
 *
 * Will analyse a and b and convert them to the "to" type if possible.
 * It will then try to promote both to a common type,
 * check that *% is only used on an integer and then perform constant folding.
 *
 * @return true if analysis worked.
 */
static bool sema_expr_analyse_mult(Context *context, Type *to, Expr *expr, Expr *left, Expr *right)
{
	bool is_mod = expr->binary_expr.operator == BINARYOP_MULT_MOD;

	// 1. Analyse the sub expressions.
	if (!sema_expr_analyse_binary_sub_expr(context, to, left, right)) return false;

	Type *left_type = left->type->canonical;
	Type *right_type = right->type->canonical;

	// 2. Perform promotion to a common type.
	if (!binary_arithmetic_promotion(left, right, left_type, right_type))
	{
		SEMA_ERROR(expr, "Cannot multiply '%s' with '%s'", type_to_error_string(left->type), type_to_error_string(right->type));
		return false;
	}

	// 3. Set the type.
	expr->type = left->type;

	// Might have changed
	left_type = left->type->canonical;

	// 4. Prevent *% use on non-integers.
	if (is_mod && !type_is_any_integer(left_type))
	{
		SEMA_ERROR(expr, "*% can only be used with integer types, try * instead.");
		return false;
	}

	// 5. Handle constant folding.
	if (both_const(left, right))
	{
		expr->expr_kind = EXPR_CONST;
		expr->const_expr.kind = left->const_expr.kind;

		switch (left->const_expr.kind)
		{
			case ALL_INTS:
				// 5a. Do mod mult if applicable.
				if (is_mod && left_type != type_compint)
				{
					bigint_mul_wrap(&expr->const_expr.i,
					                &left->const_expr.i,
					                &right->const_expr.i,
					                is_mod,
					                left_type->builtin.bitsize);
					return true;
				}
				bigint_mul(&expr->const_expr.i, &left->const_expr.i, &right->const_expr.i);
				break;
			case ALL_FLOATS:
				expr->const_expr.f = left->const_expr.f * right->const_expr.f;
				break;
			default:
				UNREACHABLE
		}
	}

	// 6. All done.
	return true;
}

/**
 * Analyse a / b
 * @return true if analysis completed ok.
 */
static bool sema_expr_analyse_div(Context *context, Type *to, Expr *expr, Expr *left, Expr *right)
{
	// 1. Analyse sub expressions.
	if (!sema_expr_analyse_binary_sub_expr(context, to, left, right)) return false;

	Type *left_type = left->type->canonical;
	Type *right_type = right->type->canonical;

	// 2. Perform promotion to a common type.
	if (!binary_arithmetic_promotion(left, right, left_type, right_type))
	{
		SEMA_ERROR(expr, "Cannot divide '%s' by '%s'.", type_to_error_string(left_type), type_to_error_string(right_type));
		return false;
	}

	expr->type = left->type;

	// 3. Check for a constant 0 on the right hand side.
	if (is_const(right))
	{
		switch (right->const_expr.kind)
		{
			case ALL_INTS:
				if (bigint_cmp_zero(&right->const_expr.i) == CMP_EQ)
				{
					SEMA_ERROR(right, "This expression evaluates to zero and division by zero is not allowed.");
					return false;
				}
				break;
			case ALL_FLOATS:
				if (right->const_expr.f == 0)
				{
					SEMA_ERROR(right, "This expression evaluates to zero and division by zero is not allowed.");
					return false;
				}
				break;
			default:
				UNREACHABLE
		}
	}

	// 4. Perform constant folding.
	if (both_const(left, right))
	{
		switch (left->const_expr.kind)
		{
			case ALL_INTS:
				bigint_div_floor(&expr->const_expr.i, &left->const_expr.i, &right->const_expr.i);
				break;
			case ALL_FLOATS:
				expr->const_expr.f = left->const_expr.f / right->const_expr.f;
				break;
			default:
				UNREACHABLE
		}
	}

	// 5. Done.
	return true;

}

/**
 * Analyse a % b
 * @return true if analysis succeeds.
 */
static bool sema_expr_analyse_mod(Context *context, Type *to, Expr *expr, Expr *left, Expr *right)
{
	// 1. Analyse both sides.
	if (!sema_expr_analyse_binary_sub_expr(context, to, left, right)) return false;

	// 2. Make sure we have some sort of integer on both sides.
	if (!type_is_any_integer(right->type->canonical) || !type_is_any_integer(left->type->canonical))
	{
		return sema_type_error_on_binop(expr);
	}

	// 3. a % 0 is not valid, so detect it.
	if (is_const(right) && bigint_cmp_zero(&right->const_expr.i) == CMP_EQ)
	{
		SEMA_ERROR(expr->binary_expr.right, "Cannot perform % with a constant zero.");
		return false;
	}

	// 4. Constant fold
	if (both_const(left, right))
	{
		expr_replace(expr, left);
		// 4a. Remember this is remainder.
		bigint_rem(&expr->const_expr.i, &left->const_expr.i, &right->const_expr.i);
	}

	expr->type = left->type;

	return true;
}

/**
 * Analyse a ^ b, a | b, a & b
 * @return true if the analysis succeeded.
 */
static bool sema_expr_analyse_bit(Context *context, Type *to, Expr *expr, Expr *left, Expr *right)
{
	// 1. Convert to top down type if possible.
	if (!sema_expr_analyse_binary_sub_expr(context, to, left, right)) return false;

	// 2. Check that both are integers.
	if (!both_any_integer(left, right))
	{
		return sema_type_error_on_binop(expr);
	}

	// 3. Promote to the same type.

	Type *left_type = left->type->canonical;
	Type *right_type = right->type->canonical;

	if (!binary_arithmetic_promotion(left, right, left_type, right_type))
	{
		return sema_type_error_on_binop(expr);
	}

	// 4. Do constant folding if both sides are constant.
	if (both_const(left, right))
	{
		expr_replace(expr, left);
		switch (expr->binary_expr.operator)
		{
			case BINARYOP_BIT_AND:
				bigint_and(&expr->const_expr.i, &left->const_expr.i, &right->const_expr.i);
				break;
			case BINARYOP_BIT_XOR:
				bigint_xor(&expr->const_expr.i, &left->const_expr.i, &right->const_expr.i);
				break;
			case BINARYOP_BIT_OR:
				bigint_or(&expr->const_expr.i, &left->const_expr.i, &right->const_expr.i);
				break;
			default:
				UNREACHABLE;
		}
	}

	// 5. Assign the type
	expr->type = left->type;
	return true;
}

/**
 * Analyse >> and << operations.
 * @return true if the analysis succeeded.
 */
static bool sema_expr_analyse_shift(Context *context, Type *to, Expr *expr, Expr *left, Expr *right)
{
	// 1. Analyze the two sub lhs & rhs *without coercion*
	if (!sema_expr_analyse_binary_sub_expr(context, NULL, left, right)) return false;

	// 2. Only integers may be shifted.
	if (!both_any_integer(left, right))
	{
		return sema_type_error_on_binop(expr);
	}

	// 3. For a constant right hand side we will make a series of checks.
	if (is_const(right))
	{
		// 3a. Make sure the value does not exceed the bitsize of
		//     the left hand side. We ignore this check for lhs being a constant.
		if (left->type->canonical->type_kind != TYPE_IXX)
		{
			BigInt bitsize;
			bigint_init_unsigned(&bitsize, left->type->canonical->builtin.bitsize);
			if (bigint_cmp(&right->const_expr.i, &bitsize) == CMP_GT)
			{
				SEMA_ERROR(right, "The shift exceeds bitsize of '%s'.", type_to_error_string(left->type));
				return false;
			}
		}
		// 3b. Make sure that the right hand side is positive.
		if (bigint_cmp_zero(&right->const_expr.i) == CMP_LT)
		{
			SEMA_ERROR(right, "A shift must be a positive number.");
			return false;
		}

		// 2c. Cast the value to the smallest runtime type.
		cast_to_smallest_runtime(right);

		// 4. Fold constant expressions.
		if (is_const(left))
		{
			// 4a. For >> this is always an arithmetic shift.
			if (expr->binary_expr.operator == BINARYOP_SHR)
			{
				expr_replace(expr, left);
				bigint_shr(&expr->const_expr.i, &left->const_expr.i, &right->const_expr.i);
				return true;
			}
			// 4b. The << case needs to behave differently for bigints and fixed bit integers.
			expr_replace(expr, left);
			if (left->const_expr.kind == TYPE_IXX)
			{
				bigint_shl(&expr->const_expr.i, &left->const_expr.i, &right->const_expr.i);
			}
			else
			{
				int bit_count = left->type->canonical->builtin.bitsize;
				bool is_signed = !type_kind_is_unsigned(left->const_expr.kind);
				bigint_shl_trunc(&expr->const_expr.i, &left->const_expr.i, &right->const_expr.i, bit_count, is_signed);
			}
			return true;
		}
	}

	// 5. We might have the case 2 << x. In that case we will to cast the left hand side to the receiving type.
	if (!cast_implicit(left, to)) return false;

	// 6. As a last out, we make sure that a comptime int has a real type. We pick i64 for this.
	if (!cast_to_runtime(left)) return false;

	expr->type = left->type;
	return true;
}

/**
 * Analyse a <<= b a >>= b
 * @return true is the analysis succeeds, false otherwise.
 */
static bool sema_expr_analyse_shift_assign(Context *context, Expr *expr, Expr *left, Expr *right)
{
	// 1. Analyze the two sub lhs & rhs *without coercion*
	if (!sema_expr_analyse_binary_sub_expr(context, NULL, left, right)) return false;

	// 2. Ensure the left hand side is assignable
	if (!expr_is_ltype(left))
	{
		SEMA_ERROR(left, "Expression is not assignable.");
		return false;
	}

	// 2. Only integers may be shifted.
	if (!both_any_integer(left, right)) return sema_type_error_on_binop(expr);

	// 3. For a constant right hand side we will make a series of checks.
	if (is_const(right))
	{
		// 3a. Make sure the value does not exceed the bitsize of
		//     the left hand side.
		BigInt bitsize;
		bigint_init_unsigned(&bitsize, left->type->canonical->builtin.bitsize);
		if (bigint_cmp(&right->const_expr.i, &bitsize) == CMP_GT)
		{
			SEMA_ERROR(right, "The shift exceeds bitsize of '%s'.", type_to_error_string(left->type));
			return false;
		}

		// 3b. Make sure that the right hand side is positive.
		if (bigint_cmp_zero(&right->const_expr.i) == CMP_LT)
		{
			SEMA_ERROR(right, "A shift must be a positive number.");
			return false;
		}

		// 3c. Cast the rhs to the smallest runtime type.
		cast_to_smallest_runtime(right);
	}

	// 4. Set the type
	expr->type = left->type;
	return true;
}


static bool sema_expr_analyse_and(Context *context, Expr *expr, Expr *left, Expr *right)
{
	if (!sema_expr_analyse_binary_sub_expr(context, NULL, left, right)) return false;
	if (!cast_implicit(left, type_bool) || !cast_implicit(right, type_bool)) return false;

	expr->type = type_bool;
	if (both_const(left, right))
	{
		expr_replace(expr, left);
		expr->const_expr.b &= right->const_expr.b;
	}
	return true;
}

static bool sema_expr_analyse_or(Context *context, Expr *expr, Expr *left, Expr *right)
{
	if (!sema_expr_analyse_binary_sub_expr(context, NULL, left, right)) return false;
	if (!cast_implicit(left, type_bool) || !cast_implicit(right, type_bool)) return false;

	if (both_const(left, right))
	{
		expr_replace(expr, left);
		expr->const_expr.b |= right->const_expr.b;
	}
	expr->type = type_bool;
	return true;
}



static void cast_to_max_bit_size(Expr *left, Expr *right, Type *left_type, Type *right_type)
{
	int bit_size_left = left_type->builtin.bitsize;
	int bit_size_right = right_type->builtin.bitsize;
	assert(bit_size_left && bit_size_right);
	if (bit_size_left == bit_size_right) return;
	if (bit_size_left < bit_size_right)
	{
		Type *to = left->type->type_kind < TYPE_U8
				? type_signed_int_by_bitsize(bit_size_right)
				: type_unsigned_int_by_bitsize(bit_size_right);
		bool success = cast_implicit(left, to);
		assert(success);
		return;
	}
	Type *to = right->type->type_kind < TYPE_U8
	           ? type_signed_int_by_bitsize(bit_size_right)
	           : type_unsigned_int_by_bitsize(bit_size_right);
	bool success = cast_implicit(right, to);
	assert(success);
}

/**
 * Analyze a == b, a != b, a > b, a < b, a >= b, a <= b
 * @return
 */
static bool sema_expr_analyse_comp(Context *context, Expr *expr, Expr *left, Expr *right)
{
	// 1. Analyse left and right side without any conversions.
	if (!sema_expr_analyse_binary_sub_expr(context, NULL, left, right)) return false;

	bool is_equality_type_op = expr->binary_expr.operator == BINARYOP_NE || expr->binary_expr.operator == BINARYOP_EQ;

	Type *left_type = left->type->canonical;
	Type *right_type = right->type->canonical;

	// 2. Handle the case of signed comparisons.
	//    This happens when either side has a definite integer type
	//    and those are either signed or unsigned.
	//    If either side is compint, then this does not happen.
	if ((type_is_unsigned(left_type) && type_is_signed(right_type))
		|| (type_is_signed(left_type) && type_is_unsigned(right_type)))
	{
		// 2a. Resize so that both sides have the same bit width. This will always work.
		cast_to_max_bit_size(left, right, left_type, right_type);
	}
	else
	{
		// 3. In the normal case, treat this as a binary op, finding the max type.
		Type *max = type_find_max_type(left_type, right_type);

		// 4. If no common type, then that's an error:
		if (!max)
		{
			SEMA_ERROR(expr, "'%s' and '%s' are different types and cannot be compared.",
					type_to_error_string(left->type), type_to_error_string(right->type));
		};

		// 5. Most types can do equality, but not all can do comparison,
		//    so we need to check that as well.
		if (!is_equality_type_op)
		{
			switch (max->type_kind)
			{
				case TYPE_POISONED:
					return false;
				case TYPE_VOID:
				case TYPE_TYPEDEF:
					UNREACHABLE
				case TYPE_BOOL:
				case TYPE_ENUM:
				case TYPE_ERROR:
				case TYPE_FUNC:
				case TYPE_STRUCT:
				case TYPE_UNION:
				case TYPE_ERROR_UNION:
				case TYPE_STRING:
				case TYPE_ARRAY:
				case TYPE_VARARRAY:
				case TYPE_SUBARRAY:
				case TYPE_META_TYPE:
					// Only != and == allowed.
					goto ERR;
				case ALL_INTS:
				case ALL_FLOATS:
					// All comparisons allowed
					break;
				case TYPE_POINTER:
					// Only comparisons between the same type is allowed. Subtypes not allowed.
					if (left_type != right_type)
					{
						SEMA_ERROR(expr, "Cannot compare pointers of different types.");
						return false;
					}
					break;
			}
		}

		// 6. Do the implicit cast.
		if (!cast_implicit(left, max)) goto ERR;
		if (!cast_implicit(right, max)) goto ERR;
	}

	// 7. Do constant folding.
	if (both_const(left, right))
	{
		expr->const_expr.b = expr_const_compare(&left->const_expr, &right->const_expr, expr->binary_expr.operator);
		expr->const_expr.kind = TYPE_BOOL;
		expr->expr_kind = EXPR_CONST;
	}

	// 8. Set the type to bool
	expr->type = type_bool;
	return true;

	ERR:
	SEMA_ERROR(expr, "Cannot evaluate '%s' %s '%s'", type_to_error_string(left_type), token_type_to_string(binaryop_to_token(expr->binary_expr.operator)), type_to_error_string(right_type));
	return false;
}

/**
 * Analyse *a
 * @return true if analysis succeeds.
 */
static bool sema_expr_analyse_deref(Context *context, Expr *expr, Expr *inner)
{
	Type *canonical = inner->type->canonical;
	// 1. Check that we have a pointer, or dereference is not allowed.
	if (canonical->type_kind != TYPE_POINTER)
	{
		SEMA_ERROR(inner, "Cannot dereference a value of type '%s'", type_to_error_string(inner->type));
		return false;
	}
	// 2. This could be a constant, in which case it is a nil which is an error.
	if (inner->expr_kind == EXPR_CONST)
	{
		SEMA_ERROR(inner, "Dereferencing nil is not allowed.");
		return false;
	}
	// 3. Now the type might not be a pointer because of a typedef,
	//    otherwise we need to use the the canonical representation.
	Type *deref_type = inner->type->type_kind != TYPE_POINTER ? inner->type : canonical;

	// 4. And... set the type.
	expr->type = deref_type->pointer;
	return true;
}

/**
 * Analyse &a
 * @return true if analysis succeeds.
 */
static bool sema_expr_analyse_addr(Context *context, Expr *expr, Expr *inner)
{
	// 1. Check that it is an lvalue.
	if (!expr_is_ltype(inner))
	{
		SEMA_ERROR(inner, "It is not possible to take the address of values, only of variables and memory locations.", type_to_error_string(inner->type));
		return false;
	}

	// 2. Get the pointer of the underlying type.
	expr->type = type_get_ptr(inner->type);
	return true;
}

static bool sema_expr_analyse_neg(Context *context, Type *to, Expr *expr, Expr *inner)
{
	Type *canonical = inner->type->canonical;
	if (!builtin_may_negate(canonical))
	{
		SEMA_ERROR(expr, "Cannot negate %s.", type_to_error_string(inner->type));
		return false;
	}
	if (inner->expr_kind != EXPR_CONST)
	{
		expr->type = inner->type;
		return true;
	}
	bool is_negmod = expr->unary_expr.operator == UNARYOP_NEGMOD;
	expr_replace(expr, inner);
	switch (expr->const_expr.kind)
	{
		case ALL_INTS:
			if (is_negmod)
			{
				if (canonical->type_kind != TYPE_IXX)
				{
					SEMA_ERROR(expr, "Cannot use –% on compile time integers, you need to first cast it to an integer type e.g. -%cast(-128, char).");

					// Continue parsing, pretending this is a -
					bigint_negate(&expr->const_expr.i, &inner->const_expr.i);
					return true;
				}
				bigint_negate_wrap(&expr->const_expr.i,
				                   &inner->const_expr.i,
				                   inner->type->canonical->builtin.bitsize);
				return true;
			}
			bigint_negate(&expr->const_expr.i, &inner->const_expr.i);
			if (expr_const_int_overflowed(&expr->const_expr))
			{
				SEMA_ERROR(expr, "Negating %s overflows '%s'.", expr_const_to_error_string(&expr->const_expr), type_to_error_string(expr->type));
				return false;
			}
			return true;
		case ALL_FLOATS:
			expr->const_expr.f = -expr->const_expr.f;
			break;
		default:
			UNREACHABLE
	}
	return true;
}

/**
 * Analyse ~x and ~123
 *
 * @return
 */
static bool sema_expr_analyse_bit_not(Context *context, Type *to, Expr *expr, Expr *inner)
{
	Type *canonical = inner->type->canonical;
	if (!type_is_any_integer(canonical) && canonical != type_bool)
	{
		SEMA_ERROR(expr, "Cannot bit negate '%s'.", type_to_error_string(inner->type));
		return false;
	}

	// The simple case, non-const.
	if (inner->expr_kind != EXPR_CONST)
	{
		expr->type = inner->type;
		return true;
	}

	expr_replace(expr, inner);
	switch (expr->const_expr.kind)
	{
		case ALL_SIGNED_INTS:
		case ALL_UNSIGNED_INTS:
			bigint_negate_wrap(&expr->const_expr.i, &inner->const_expr.i, canonical->builtin.bitsize);
			break;
		case TYPE_IXX:
			bigint_negate(&expr->const_expr.i, &inner->const_expr.i);
			break;
		case TYPE_BOOL:
			expr->const_expr.b = !expr->const_expr.b;
			break;
		default:
			UNREACHABLE
	}
	return true;
}

static bool sema_expr_analyse_not(Context *context, Type *to, Expr *expr, Expr *inner)
{
	expr->type = type_bool;
	if (inner->expr_kind == EXPR_CONST)
	{
		switch (expr->const_expr.kind)
		{
			case ALL_INTS:
				expr->const_expr.b = bigint_cmp_zero(&inner->const_expr.i) == CMP_EQ;
				break;
			case TYPE_BOOL:
				expr->const_expr.b = !inner->const_expr.b;
				break;
			case ALL_FLOATS:
				expr->const_expr.b = inner->const_expr.f == 0.0;
				break;
			case TYPE_STRING:
				expr->const_expr.b = !inner->const_expr.string.len;
				break;
			case TYPE_ERROR:
			case TYPE_ENUM:
				TODO
			default:
				UNREACHABLE
		}
		expr->const_expr.kind = TYPE_BOOL;
		expr->expr_kind = EXPR_CONST;
		return true;
	}
	Type *canonical = inner->type->canonical;
	switch (canonical->type_kind)
	{
		case TYPE_POISONED:
		case TYPE_IXX:
		case TYPE_FXX:
		case TYPE_TYPEDEF:
		case TYPE_ERROR_UNION:
			UNREACHABLE
		case TYPE_FUNC:
		case TYPE_ARRAY:
		case TYPE_POINTER:
		case TYPE_VARARRAY:
		case TYPE_SUBARRAY:
		case TYPE_BOOL:
		case TYPE_I8:
		case TYPE_I16:
		case TYPE_I32:
		case TYPE_I64:
		case TYPE_U8:
		case TYPE_U16:
		case TYPE_U32:
		case TYPE_U64:
		case TYPE_F32:
		case TYPE_F64:
			return true;
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_VOID:
		case TYPE_STRING:
		case TYPE_ENUM:
		case TYPE_ERROR:
		case TYPE_META_TYPE:
			SEMA_ERROR(expr, "Cannot use 'not' on %s", type_to_error_string(inner->type));
			return false;
	}
	UNREACHABLE
}

static inline bool sema_expr_analyse_incdec(Context *context, Type *to, Expr *expr, Expr *inner)
{
	if (!expr_is_ltype(inner))
	{
		SEMA_ERROR(inner, "Expression cannot be assigned to.");
		return false;
	}
	if (!type_is_numeric(inner->type->canonical) && inner->type->canonical->type_kind != TYPE_POINTER)
	{
		SEMA_ERROR(inner, "Expression must be a number or a pointer.");
		return false;
	}
	expr->type = inner->type;
	return true;
}



static inline bool sema_expr_analyse_binary(Context *context, Type *to, Expr *expr)
{
	assert(expr->resolve_status == RESOLVE_RUNNING);
	Expr *left = expr->binary_expr.left;
	Expr *right = expr->binary_expr.right;
	switch (expr->binary_expr.operator)
	{
		case BINARYOP_ASSIGN:
			return sema_expr_analyse_assign(context, expr, left, right);
		case BINARYOP_MULT:
		case BINARYOP_MULT_MOD:
			return sema_expr_analyse_mult(context, to, expr, left, right);
		case BINARYOP_ADD:
		case BINARYOP_ADD_MOD:
			return sema_expr_analyse_add(context, to, expr, left, right);
		case BINARYOP_ADD_ASSIGN:
		case BINARYOP_ADD_MOD_ASSIGN:
		case BINARYOP_SUB_ASSIGN:
		case BINARYOP_SUB_MOD_ASSIGN:
			return sema_expr_analyse_add_sub_assign(context, expr, left, right);
		case BINARYOP_SUB:
		case BINARYOP_SUB_MOD:
			return sema_expr_analyse_sub(context, to, expr, left, right);
		case BINARYOP_DIV:
			return sema_expr_analyse_div(context, to, expr, left, right);
		case BINARYOP_MULT_ASSIGN:
		case BINARYOP_DIV_ASSIGN:
			return sema_expr_analyse_common_assign(context, expr, left, right, false);
		case BINARYOP_MULT_MOD_ASSIGN:
		case BINARYOP_BIT_AND_ASSIGN:
		case BINARYOP_BIT_OR_ASSIGN:
		case BINARYOP_BIT_XOR_ASSIGN:
		case BINARYOP_MOD_ASSIGN:
			return sema_expr_analyse_common_assign(context, expr, left, right, true);
		case BINARYOP_MOD:
			return sema_expr_analyse_mod(context, to, expr, left, right);
		case BINARYOP_AND:
			return sema_expr_analyse_and(context, expr, left, right);
		case BINARYOP_OR:
			return sema_expr_analyse_or(context, expr, left, right);
		case BINARYOP_BIT_OR:
		case BINARYOP_BIT_XOR:
		case BINARYOP_BIT_AND:
			return sema_expr_analyse_bit(context, to, expr, left, right);
		case BINARYOP_NE:
		case BINARYOP_EQ:
		case BINARYOP_GT:
		case BINARYOP_GE:
		case BINARYOP_LT:
		case BINARYOP_LE:
			return sema_expr_analyse_comp(context, expr, left, right);
		case BINARYOP_SHR:
		case BINARYOP_SHL:
			return sema_expr_analyse_shift(context, to, expr, left, right);
		case BINARYOP_SHR_ASSIGN:
		case BINARYOP_SHL_ASSIGN:
			return sema_expr_analyse_shift_assign(context, expr, left, right);
		case BINARYOP_ERROR:
			break;
	}
	UNREACHABLE
}

static inline bool sema_expr_analyse_unary(Context *context, Type *to, Expr *expr)
{
	assert(expr->resolve_status == RESOLVE_RUNNING);
	Expr *inner = expr->unary_expr.expr;

	if (!sema_analyse_expr(context, NULL, inner)) return false;

	switch (expr->unary_expr.operator)
	{
		case UNARYOP_DEREF:
			return sema_expr_analyse_deref(context, expr, inner);
		case UNARYOP_ADDR:
			return sema_expr_analyse_addr(context, expr, inner);
		case UNARYOP_NEG:
		case UNARYOP_NEGMOD:
			return sema_expr_analyse_neg(context, to, expr, inner);
		case UNARYOP_BITNEG:
			return sema_expr_analyse_bit_not(context, to, expr, inner);
		case UNARYOP_NOT:
			return sema_expr_analyse_not(context, to, expr, inner);
		case UNARYOP_DEC:
		case UNARYOP_INC:
			return sema_expr_analyse_incdec(context, to, expr, inner);
		case UNARYOP_ERROR:
			return false;
	}
	UNREACHABLE
}

static inline bool sema_expr_analyse_post_unary(Context *context, Type *to, Expr *expr)
{
	assert(expr->resolve_status == RESOLVE_RUNNING);
	Expr *inner = expr->post_expr.expr;

	if (!sema_analyse_expr(context, NULL, inner)) return false;

	return sema_expr_analyse_incdec(context, to, expr, inner);
}


static inline bool sema_expr_analyse_try(Context *context, Type *to, Expr *expr)
{
	context->try_nesting++;
	bool success = sema_analyse_expr(context, to, expr->try_expr.expr);
	context->try_nesting--;
	if (!success) return false;
	expr->type = expr->try_expr.expr->type;
	if (expr->try_expr.else_expr)
	{
		if (!sema_analyse_expr(context, to, expr->try_expr.else_expr)) return false;
	}
	// TODO Check errors!
	return true;
}

static Ast *ast_shallow_copy(Ast *source)
{
	Ast *copy = malloc_arena(sizeof(Ast));
	memcpy(copy, source, sizeof(Ast));
	return copy;
}

static Expr *expr_shallow_copy(Expr *source)
{
	Expr *copy = malloc_arena(sizeof(Expr));
	memcpy(copy, source, sizeof(Expr));
	return copy;
}



static TypeInfo *type_info_copy_from_macro(Context *context, Expr *macro, TypeInfo *source)
{
	if (!source) return NULL;
	TypeInfo *copy = malloc_arena(sizeof(TypeInfo));
	memcpy(copy, source, sizeof(TypeInfo));
	switch (source->kind)
	{
		case TYPE_INFO_POISON:
			return copy;
		case TYPE_INFO_IDENTIFIER:
			assert(source->resolve_status == RESOLVE_NOT_DONE);
			TODO
			break;
		case TYPE_INFO_EXPRESSION:
			assert(source->resolve_status == RESOLVE_NOT_DONE);
			copy->unresolved_type_expr = expr_copy_from_macro(context, macro, source->unresolved_type_expr);
			return copy;
		case TYPE_INFO_ARRAY:
			assert(source->resolve_status == RESOLVE_NOT_DONE);
			copy->array.len = expr_copy_from_macro(context, macro, source->array.len);
			copy->array.base = type_info_copy_from_macro(context, macro, source->array.base);
			return copy;
		case TYPE_INFO_INC_ARRAY:
			assert(source->resolve_status == RESOLVE_NOT_DONE);
			copy->array.base = type_info_copy_from_macro(context, macro, source->array.base);
			return copy;
		case TYPE_INFO_POINTER:
			assert(source->resolve_status == RESOLVE_NOT_DONE);
			copy->pointer = type_info_copy_from_macro(context, macro, source->pointer);
			return copy;
	}
	UNREACHABLE
}


static Ast** ast_copy_list_from_macro(Context *context, Expr *macro, Ast **to_copy)
{
	Ast **result = NULL;
	VECEACH(to_copy, i)
	{
		vec_add(result, ast_copy_from_macro(context, macro, to_copy[i]));
	}
	return result;
}


static Expr *expr_copy_from_macro(Context *context, Expr *macro, Expr *source_expr)
{
	if (!source_expr) return NULL;
	Expr *expr = expr_shallow_copy(source_expr);
	switch (source_expr->expr_kind)
	{
		case EXPR_TYPEOF:
			MACRO_COPY_EXPR(expr->typeof_expr);
			return expr;
		case EXPR_COMPOUND_LITERAL:
			MACRO_COPY_EXPR(expr->expr_compound_literal.initializer);
			MACRO_COPY_TYPE(expr->expr_compound_literal.type_info);
			return expr;
		case EXPR_DESIGNATED_INITIALIZER:
			// Created during semantic analysis
			UNREACHABLE
		case EXPR_RANGE:
			MACRO_COPY_EXPR(expr->range_expr.left);
			MACRO_COPY_EXPR(expr->range_expr.right);
			return expr;
		case EXPR_EXPR_BLOCK:
			MACRO_COPY_AST_LIST(expr->expr_block.stmts);
			return expr;
		case EXPR_POISONED:
			return source_expr;
		case EXPR_TRY:
			MACRO_COPY_EXPR(expr->try_expr.expr);
			MACRO_COPY_EXPR(expr->try_expr.else_expr);
			return expr;
		case EXPR_CONST:
			return expr;
		case EXPR_BINARY:
			MACRO_COPY_EXPR(expr->binary_expr.left);
			MACRO_COPY_EXPR(expr->binary_expr.right);
			return expr;
		case EXPR_TERNARY:
			MACRO_COPY_EXPR(expr->ternary_expr.cond);
			MACRO_COPY_EXPR(expr->ternary_expr.then_expr);
			MACRO_COPY_EXPR(expr->ternary_expr.else_expr);
			return expr;
		case EXPR_UNARY:
			MACRO_COPY_EXPR(expr->unary_expr.expr);
			return expr;
		case EXPR_POST_UNARY:
			MACRO_COPY_EXPR(expr->post_expr.expr);
			return expr;
		case EXPR_TYPEID:
			MACRO_COPY_TYPE(expr->typeid_expr);
			return expr;
		case EXPR_IDENTIFIER:
			TODO
			break;
		case EXPR_TYPE_ACCESS:
			MACRO_COPY_TYPE(expr->type_access.type);
			return expr;
		case EXPR_CALL:
			MACRO_COPY_EXPR(expr->call_expr.function);
			MACRO_COPY_EXPR_LIST(expr->call_expr.arguments);
			return expr;
		case EXPR_SUBSCRIPT:
			MACRO_COPY_EXPR(expr->subscript_expr.expr);
			MACRO_COPY_EXPR(expr->subscript_expr.index);
			return expr;
		case EXPR_GROUP:
			MACRO_COPY_EXPR(expr->group_expr->group_expr);
			return expr;
		case EXPR_ACCESS:
			MACRO_COPY_EXPR(expr->access_expr.parent);
			return expr;
		case EXPR_INITIALIZER_LIST:
			MACRO_COPY_EXPR_LIST(expr->expr_initializer.initializer_expr);
			return expr;
		case EXPR_EXPRESSION_LIST:
			MACRO_COPY_EXPR_LIST(expr->expression_list);
			return expr;
		case EXPR_CAST:
			MACRO_COPY_EXPR(expr->cast_expr.expr);
			MACRO_COPY_TYPE(expr->cast_expr.type_info);
			return expr;
		case EXPR_SCOPED_EXPR:
			MACRO_COPY_EXPR(expr->expr_scope.expr);
			return expr;
		case EXPR_MACRO_EXPR:
			MACRO_COPY_EXPR(expr->macro_expr);
			return expr;
	}
	UNREACHABLE
}

static Expr **expr_copy_expr_list_from_macro(Context *context, Expr *macro, Expr **expr_list)
{
	Expr **result = NULL;
	VECEACH(expr_list, i)
	{
		vec_add(result, expr_copy_from_macro(context, macro, expr_list[i]));
	}
	return result;
}


static TypeInfo** type_info_copy_list_from_macro(Context *context, Expr *macro, TypeInfo **to_copy)
{
	TypeInfo **result = NULL;
	VECEACH(to_copy, i)
	{
		vec_add(result, type_info_copy_from_macro(context, macro, to_copy[i]));
	}
	return result;
}

static Ast *ast_copy_from_macro(Context *context, Expr *macro, Ast *source)
{
	Ast *ast = ast_shallow_copy(source);
	switch (source->ast_kind)
	{
		case AST_POISONED:
			return ast;
		case AST_ASM_STMT:
			TODO
		case AST_ATTRIBUTE:
			UNREACHABLE
		case AST_BREAK_STMT:
			return ast;
		case AST_CASE_STMT:
			MACRO_COPY_AST(ast->case_stmt.body);
			MACRO_COPY_EXPR(ast->case_stmt.expr);
			return ast;
			break;
		case AST_CATCH_STMT:
			MACRO_COPY_AST(ast->catch_stmt.body);
			return ast;
		case AST_COMPOUND_STMT:
			MACRO_COPY_AST_LIST(ast->compound_stmt.stmts);
			return ast;
		case AST_CONTINUE_STMT:
			return ast;
		case AST_CT_IF_STMT:
			MACRO_COPY_EXPR(ast->ct_if_stmt.expr);
			MACRO_COPY_AST(ast->ct_if_stmt.elif);
			MACRO_COPY_AST(ast->ct_if_stmt.then);
			return ast;
		case AST_CT_ELIF_STMT:
			MACRO_COPY_EXPR(ast->ct_elif_stmt.expr);
			MACRO_COPY_AST(ast->ct_elif_stmt.then);
			MACRO_COPY_AST(ast->ct_elif_stmt.elif);
			return ast;
		case AST_CT_ELSE_STMT:
			MACRO_COPY_AST(ast->ct_else_stmt);
			return ast;
		case AST_CT_FOR_STMT:
			MACRO_COPY_AST(ast->ct_for_stmt.body);
			MACRO_COPY_EXPR(ast->ct_for_stmt.expr);
			return ast;
		case AST_CT_SWITCH_STMT:
			MACRO_COPY_EXPR(ast->ct_switch_stmt.cond);
			MACRO_COPY_AST_LIST(ast->ct_switch_stmt.body);
			return ast;
		case AST_CT_DEFAULT_STMT:
			MACRO_COPY_AST(ast->ct_default_stmt);
			return ast;
		case AST_CT_CASE_STMT:
			MACRO_COPY_AST(ast->ct_case_stmt.body);
			MACRO_COPY_TYPE_LIST(ast->ct_case_stmt.types);
			return ast;
		case AST_DECLARE_STMT:
			TODO
			return ast;
		case AST_DEFAULT_STMT:
			MACRO_COPY_AST(ast->case_stmt.body);
			return ast;
		case AST_DEFER_STMT:
			assert(!ast->defer_stmt.prev_defer);
			MACRO_COPY_AST(ast->defer_stmt.body);
			return ast;
		case AST_DO_STMT:
			MACRO_COPY_AST(ast->do_stmt.body);
			MACRO_COPY_EXPR(ast->do_stmt.expr);
			return ast;
		case AST_EXPR_STMT:
			MACRO_COPY_EXPR(ast->expr_stmt);
			return ast;
		case AST_FOR_STMT:
			MACRO_COPY_EXPR(ast->for_stmt.cond);
			MACRO_COPY_EXPR(ast->for_stmt.incr);
			MACRO_COPY_AST(ast->for_stmt.body);
			MACRO_COPY_AST(ast->for_stmt.init);
			return ast;
		case AST_GENERIC_CASE_STMT:
			MACRO_COPY_AST(ast->generic_case_stmt.body);
			// ast->generic_case_stmt.types = ...
			TODO
			return ast;
		case AST_GENERIC_DEFAULT_STMT:
			MACRO_COPY_AST(ast->generic_default_stmt);
			return ast;
		case AST_GOTO_STMT:
			MACRO_COPY_AST(ast->goto_stmt.label);
			// TODO fixup name, which needs to be macro local.
			TODO
			return ast;
		case AST_IF_STMT:
			MACRO_COPY_AST(ast->if_stmt.cond);
			MACRO_COPY_AST(ast->if_stmt.decl);
			MACRO_COPY_AST(ast->if_stmt.else_body);
			MACRO_COPY_AST(ast->if_stmt.then_body);
			return ast;
		case AST_LABEL:
			assert(!ast->label_stmt.defer);
			assert(!ast->label_stmt.in_defer);
			// TODO fixup name which needs to be macro local.
			TODO
			return ast;
		case AST_NOP_STMT:
			return ast;
		case AST_RETURN_STMT:
			MACRO_COPY_EXPR(ast->return_stmt.expr);
			// TODO handle conversions?
			TODO
			return ast;
		case AST_DECL_EXPR_LIST:
			MACRO_COPY_AST_LIST(ast->decl_expr_stmt);
			return ast;
		case AST_SWITCH_STMT:
			MACRO_COPY_AST(ast->switch_stmt.decl);
			MACRO_COPY_AST(ast->switch_stmt.cond);
			MACRO_COPY_AST_LIST(ast->switch_stmt.cases);
			return ast;
		case AST_THROW_STMT:
			MACRO_COPY_EXPR(ast->throw_stmt.throw_value);
			return ast;
		case AST_TRY_STMT:
			MACRO_COPY_AST(ast->try_stmt);
			return ast;
		case AST_NEXT_STMT:
			TODO
			return ast;
		case AST_VOLATILE_STMT:
			TODO
			return ast;
		case AST_WHILE_STMT:
			MACRO_COPY_AST(ast->while_stmt.cond);
			MACRO_COPY_AST(ast->while_stmt.decl);
			MACRO_COPY_AST(ast->while_stmt.body);
			return ast;
		case AST_SCOPED_STMT:
			MACRO_COPY_AST(ast->scoped_stmt.stmt);
			return ast;
	}
	UNREACHABLE;
}
static inline bool sema_expr_analyse_macro_call(Context *context, Type *to, Expr *macro, Expr *inner)
{
	Expr *func_expr = inner->call_expr.function;

	if (!sema_analyse_expr(context, NULL, func_expr)) return false;

	Decl *decl;
	switch (func_expr->expr_kind)
	{
		case EXPR_TYPE_ACCESS:
			TODO
		case EXPR_IDENTIFIER:
			decl = func_expr->identifier_expr.decl;
			break;
		default:
			TODO
	}
	if (decl->decl_kind != DECL_MACRO)
	{
		SEMA_ERROR(macro, "A macro was expected here.");
		return false;
	}
	Expr **args =func_expr->call_expr.arguments;
	Decl **func_params = decl->macro_decl.parameters;
	// TODO handle bare macros.
	// TODO handle $ args and # args
	unsigned num_args = vec_size(args);
	// unsigned num_params = vec_size(func_params);
	for (unsigned i = 0; i < num_args; i++)
	{
		Expr *arg = args[i];
		Decl *param = func_params[i];
		if (!sema_analyse_expr(context, param->type, arg)) return false;
	}
	Ast *body = ast_copy_from_macro(context, inner, decl->macro_decl.body);
	TODO
}

static inline bool sema_expr_analyse_macro_call2(Context *context, Type *to, Expr *expr, Decl *macro)
{
	Ast *macro_parent;
	// TODO handle loops
	Decl *stored_macro = context->evaluating_macro;
	Type *stored_rtype = context->rtype;
	context->evaluating_macro = macro;
	context->rtype = macro->macro_decl.rtype->type;
	// Handle escaping macro
	bool success = sema_analyse_statement(context, macro->macro_decl.body);
	context->evaluating_macro = stored_macro;
	context->rtype = stored_rtype;
	if (!success) return false;

	TODO
	return success;
}

static inline bool sema_expr_analyse_macro_expr(Context *context, Type *to, Expr *expr)
{
	Expr *inner = expr->macro_expr;
	switch (inner->expr_kind)
	{
		case EXPR_CALL:
			return sema_expr_analyse_macro_call(context, to, expr, inner);
		case EXPR_ACCESS:
		case EXPR_IDENTIFIER:
			// Allow @f unrolling?
		default:
			SEMA_ERROR(expr, "Expected a macro name after '@'");
			return false;
	}
}

static inline bool sema_expr_analyse_type(Context *context, Type *to, Expr *expr)
{
	if (!sema_resolve_type_info(context, expr->typeid_expr))
	{
		return expr_poison(expr);
	}
	expr->type = type_typeid;
	return true;
}



static inline Ast **context_push_returns(Context *context)
{
	Ast** old_returns = context->returns;
	if (context->returns_cache)
	{
		context->returns = context->returns_cache;
		context->returns_cache = NULL;
		vec_resize(context->returns, 0);
	}
	else
	{
		context->returns = NULL;
	}
	return old_returns;
}

static inline void context_pop_returns(Context *context, Ast **restore)
{
	if (!context->returns_cache && context->returns)
	{
		context->returns_cache = context->returns;
	}
	context->returns = restore;
}


static inline bool sema_expr_analyse_expr_block(Context *context, Type *to, Expr *expr)
{
	bool success = true;
	expr->type = type_void;
	Type *prev_expected_block_type = context->expected_block_type;
	Ast **saved_returns = context_push_returns(context);
	context->expected_block_type = to;
	context_push_scope_with_flags(context, SCOPE_EXPR_BLOCK);

	VECEACH(expr->expr_block.stmts, i)
	{
		if (!sema_analyse_statement(context, expr->expr_block.stmts[i]))
		{
			success = false;
			goto EXIT;
		}
	}

	if (!vec_size(context->returns))
	{
		if (to)
		{
			SEMA_ERROR(expr, "Expected at least one return out of expression block to return a value of type %s.", type_to_error_string(to));
			success = false;
		}
		goto EXIT;
	}

	Expr *first_return_expr = context->returns[0]->return_stmt.expr;
	Type *left_canonical = first_return_expr ? first_return_expr->type->canonical : type_void;
	bool all_returns_needs_casts = false;
	// Let's unify the return statements.
	VECEACH(context->returns, i)
	{
		Ast *return_stmt = context->returns[i];
		Expr *ret_expr = return_stmt->return_stmt.expr;
		bool last_expr_was_void = left_canonical == type_void;
		Type *right_canonical = ret_expr ? ret_expr->type->canonical : type_void;
		bool current_expr_was_void = right_canonical == type_void;
		if (i > 0 && last_expr_was_void != current_expr_was_void)
		{
			SEMA_ERROR(return_stmt, "You can't combine empty returns with value returns.");
			SEMA_PREV(context->returns[i - 1], "Previous return was here.");
			success = false;
			goto EXIT;
		}
		if (to)
		{
			if (current_expr_was_void)
			{
				SEMA_ERROR(return_stmt, "The return must be a value of type '%s'.", type_to_error_string(to));
				success = false;
				goto EXIT;
			}
			if (!cast_implicit(ret_expr, to))
			{
				success = false;
				goto EXIT;
			}
			continue;
		}

		// The simple case.
		if (left_canonical == right_canonical) continue;

		// Try to find a common type:
		Type *max = type_find_max_type(left_canonical, right_canonical);
		if (!max)
		{
			SEMA_ERROR(return_stmt, "Cannot find a common parent type of '%s' and '%s'",
			           type_to_error_string(left_canonical), type_to_error_string(right_canonical));
			SEMA_PREV(context->returns[i - 1], "The previous return was here.");
			success = false;
			goto EXIT;
		}
		left_canonical = max;
		all_returns_needs_casts = true;
	}
	if (all_returns_needs_casts)
	{
		VECEACH(context->returns, i)
		{
			Ast *return_stmt = context->returns[i];
			Expr *ret_expr = return_stmt->return_stmt.expr;
			if (!cast_implicit(ret_expr, left_canonical))
			{
				success = false;
				goto EXIT;
			}
		}
	}
	expr->type = left_canonical;
EXIT:
	context_pop_scope(context);
	context_pop_returns(context, saved_returns);
	context->expected_block_type = prev_expected_block_type;
	return success;
}

static inline bool sema_expr_analyse_range(Context *context, Type *to, Expr *expr)
{
	TODO
}
static inline bool sema_expr_analyse_compound_literal(Context *context, Type *to, Expr *expr)
{
	if (!sema_resolve_type_info(context, expr->expr_compound_literal.type_info)) return false;
	Type *type = expr->expr_compound_literal.type_info->type;
	if (!sema_expr_analyse_initializer_list(context, type, expr->expr_compound_literal.initializer)) return false;
	expr->type = type;
	return true;
}

static inline bool sema_expr_analyse_typeof(Context *context, Expr *expr)
{
	if (!sema_analyse_expr(context, NULL, expr->typeof_expr)) return false;
	Type *type = expr->typeof_expr->type->canonical;
	expr->expr_kind = EXPR_TYPEID;
	expr->typeid_expr = type_info_new_base(type, expr->typeof_expr->span);
	expr->type = type_typeid;
	return true;
}

static inline bool sema_analyse_expr_dispatch(Context *context, Type *to, Expr *expr)
{
	switch (expr->expr_kind)
	{
		case EXPR_POISONED:
			return false;
		case EXPR_DESIGNATED_INITIALIZER:
			// Created during semantic analysis
			UNREACHABLE
		case EXPR_SCOPED_EXPR:
			UNREACHABLE
		case EXPR_TYPEOF:
			return sema_expr_analyse_typeof(context, expr);
		case EXPR_COMPOUND_LITERAL:
			return sema_expr_analyse_compound_literal(context, to, expr);
		case EXPR_EXPR_BLOCK:
			return sema_expr_analyse_expr_block(context, to, expr);
		case EXPR_MACRO_EXPR:
			return sema_expr_analyse_macro_expr(context, to, expr);
		case EXPR_TRY:
			return sema_expr_analyse_try(context, to, expr);
		case EXPR_RANGE:
			return sema_expr_analyse_range(context, to, expr);
		case EXPR_CONST:
			return true;
		case EXPR_BINARY:
			return sema_expr_analyse_binary(context, to, expr);
		case EXPR_TERNARY:
			return sema_expr_analyse_ternary(context, to, expr);
		case EXPR_UNARY:
			return sema_expr_analyse_unary(context, to, expr);
		case EXPR_POST_UNARY:
			return sema_expr_analyse_post_unary(context, to, expr);
		case EXPR_TYPEID:
			return sema_expr_analyse_type(context, to, expr);
		case EXPR_IDENTIFIER:
			return sema_expr_analyse_identifier(context, to, expr);
		case EXPR_TYPE_ACCESS:
			return sema_expr_analyse_type_access(context, to, expr);
		case EXPR_CALL:
			return sema_expr_analyse_call(context, to, expr);
		case EXPR_SUBSCRIPT:
			return sema_expr_analyse_subscript(context, to, expr);
		case EXPR_GROUP:
			return sema_expr_analyse_group(context, to, expr);
		case EXPR_ACCESS:
			return sema_expr_analyse_access(context, expr);
		case EXPR_INITIALIZER_LIST:
			return sema_expr_analyse_initializer_list(context, to, expr);
		case EXPR_CAST:
			return sema_expr_analyse_cast(context, to, expr);
		case EXPR_EXPRESSION_LIST:
			return sema_expr_analyse_expr_list(context, to, expr);
	}
	UNREACHABLE
}


bool sema_analyse_expr_of_required_type(Context *context, Type *to, Expr *expr)
{
	if (!sema_analyse_expr(context, to, expr)) return false;
	return cast_implicit(expr, to);
}

bool sema_analyse_expr(Context *context, Type *to, Expr *expr)
{
	switch (expr->resolve_status)
	{
		case RESOLVE_NOT_DONE:
			expr->resolve_status = RESOLVE_RUNNING;
			break;
		case RESOLVE_RUNNING:
			SEMA_ERROR(expr, "Recursive resolution of expression");
			return expr_poison(expr);
		case RESOLVE_DONE:
			return expr_ok(expr);
	}
	if (!sema_analyse_expr_dispatch(context, to, expr)) return expr_poison(expr);
	expr->resolve_status = RESOLVE_DONE;
	return to ? cast(expr, to, CAST_TYPE_OPTIONAL_IMPLICIT) : true;
}