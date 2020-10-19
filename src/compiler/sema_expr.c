// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "sema_internal.h"
#include "bigint.h"

/*
 * TODOs
 * - Disallow jumping in and out of an expression block.
 */

static Expr **expr_copy_expr_list_from_macro(Context *context, Expr **expr_list);
static Expr *expr_copy_from_macro(Context *context, Expr *source_expr);
static Ast *ast_copy_from_macro(Context *context, Ast *source);
static Ast **ast_copy_list_from_macro(Context *context, Ast **to_copy);
static Decl *decl_copy_local_from_macro(Context *context, Decl *to_copy);
static TypeInfo *type_info_copy_from_macro(Context *context, TypeInfo *source);
static inline bool sema_cast_rvalue(Context *context, Type *to, Expr *expr);

#define MACRO_COPY_DECL(x) x = decl_copy_local_from_macro(context, x)
#define MACRO_COPY_EXPR(x) x = expr_copy_from_macro(context, x)
#define MACRO_COPY_TYPE(x) x = type_info_copy_from_macro(context, x)
#define MACRO_COPY_TYPE_LIST(x) x = type_info_copy_list_from_macro(context, x)
#define MACRO_COPY_EXPR_LIST(x) x = expr_copy_expr_list_from_macro(context, x)
#define MACRO_COPY_AST_LIST(x) x = ast_copy_list_from_macro(context, x)
#define MACRO_COPY_AST(x) x = ast_copy_from_macro(context, x)

static inline bool sema_expr_analyse_binary(Context *context, Type *to, Expr *expr);
static inline bool sema_analyse_expr_value(Context *context, Type *to, Expr *expr);
static inline bool expr_const_int_valid(Expr *expr, Type *type)
{
	if (expr_const_int_overflowed(&expr->const_expr))
	{
		SEMA_ERROR(expr, "Cannot fit '%s' into type '%s'.", expr_const_to_error_string(&expr->const_expr), type_to_error_string(type));
		return false;
	}
	if (bigint_cmp_zero(&expr->const_expr.i) == CMP_LT && type_kind_is_unsigned(expr->const_expr.kind))
	{
		SEMA_ERROR(expr, "'%s' underflows type '%s'.", expr_const_to_error_string(&expr->const_expr), type_to_error_string(type));
		return false;
	}
	return true;
}

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

static inline void context_pop_returns(Context *context, Ast **restore)
{
	if (!context->returns_cache && context->returns)
	{
		context->returns_cache = context->returns;
	}
	context->returns = restore;
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

int sema_check_comp_time_bool(Context *context, Expr *expr)
{
	if (!sema_analyse_expr_of_required_type(context, type_bool, expr, false)) return -1;
	if (expr->expr_kind != EXPR_CONST)
	{
		SEMA_ERROR(expr, "Compile time evaluation requires a compile time constant value.");
		return -1;
	}
	return expr->const_expr.b;
}

static bool expr_is_ltype(Expr *expr)
{
	switch (expr->expr_kind)
	{
		case EXPR_CONST_IDENTIFIER:
		case EXPR_MACRO_CT_IDENTIFIER:
			return false;
		case EXPR_CT_IDENT:
			return true;
		case EXPR_IDENTIFIER:
		{
			Decl *decl = expr->identifier_expr.decl;
			if (decl->decl_kind != DECL_VAR) return false;
			decl = decl_raw(decl);
			switch (decl->var.kind)
			{
				case VARDECL_LOCAL_CT:
				case VARDECL_LOCAL_CT_TYPE:
				case VARDECL_LOCAL:
				case VARDECL_GLOBAL:
				case VARDECL_PARAM:
				case VARDECL_PARAM_REF:
					return true;
				case VARDECL_CONST:
				default:
					return false;
			}
		}
		case EXPR_UNARY:
			return expr->unary_expr.operator == UNARYOP_DEREF;
		case EXPR_ACCESS:
			return expr_is_ltype(expr->access_expr.parent);
		case EXPR_GROUP:
			return expr_is_ltype(expr->group_expr);
		case EXPR_SUBSCRIPT:
		case EXPR_SLICE:
			return true;
		default:
			return false;
	}
}

static inline bool sema_cast_ident_rvalue(Context *context, Type *to, Expr *expr)
{
	Decl *decl = expr->identifier_expr.decl;
	expr->identifier_expr.is_rvalue = true;

	switch (decl->decl_kind)
	{
		case DECL_FUNC:
			SEMA_ERROR(expr, "Expected function followed by (...) or prefixed by &.");
			return expr_poison(expr);
		case DECL_MACRO:
			SEMA_ERROR(expr, "Expected macro followed by (...) or prefixed by '&'.");
			return expr_poison(expr);
		case DECL_ENUM_CONSTANT:
			expr_replace(expr, decl->enum_constant.expr);
			return true;
		case DECL_VAR:
			break;
		case DECL_TYPEDEF:
			UNREACHABLE
		case DECL_POISONED:
			return expr_poison(expr);
		case DECL_LABEL:
			SEMA_ERROR(expr, "Did you intend to use the label '%s' here?", decl->name);
			return expr_poison(expr);
		case DECL_STRUCT:
			SEMA_ERROR(expr, "Expected struct followed by (...) or '.'.");
			return expr_poison(expr);
		case DECL_UNION:
			SEMA_ERROR(expr, "Expected union followed by (...) or '.'.");
			return expr_poison(expr);
		case DECL_ENUM:
			SEMA_ERROR(expr, "Expected enum name followed by '.' and an enum value.");
			return expr_poison(expr);
		case DECL_ERR:
			SEMA_ERROR(expr, "Did you forget a '!' after '%s'?", decl->name);
			return expr_poison(expr);
		case DECL_ARRAY_VALUE:
			UNREACHABLE
		case DECL_IMPORT:
		case DECL_GENERIC:
		case DECL_CT_IF:
		case DECL_CT_ELSE:
		case DECL_CT_ELIF:
		case DECL_CT_SWITCH:
		case DECL_CT_CASE:
		case DECL_ATTRIBUTE:
			UNREACHABLE
		case DECL_DEFINE:
			TODO
	}
	switch (decl->var.kind)
	{
		case VARDECL_CONST:
			expr_replace(expr, expr_copy_from_macro(context, decl->var.init_expr));
			return sema_analyse_expr(context, to, expr);
		case VARDECL_PARAM_EXPR:
			expr_replace(expr, expr_copy_from_macro(context, decl->var.init_expr));
			assert(decl->var.init_expr->resolve_status == RESOLVE_DONE);
			return true;
		case VARDECL_PARAM_CT_TYPE:
			TODO
		case VARDECL_PARAM_REF:
			expr_replace(expr, expr_copy_from_macro(context, decl->var.init_expr));
			return sema_cast_rvalue(context, to, expr);
		case VARDECL_PARAM:
		case VARDECL_GLOBAL:
		case VARDECL_LOCAL:
			break;
		case VARDECL_MEMBER:
			SEMA_ERROR(expr, "Expected '%s' followed by a method call or property.", decl->name);
			return expr_poison(expr);
		case VARDECL_PARAM_CT:
			TODO
			break;
		case VARDECL_LOCAL_CT:
			TODO
			break;
		case VARDECL_LOCAL_CT_TYPE:
			TODO
			break;
		case VARDECL_ALIAS:
			TODO
			break;
	}
	return true;
}

static ExprFailableStatus expr_is_failable(Expr *expr)
{
	if (expr->expr_kind != EXPR_IDENTIFIER) return FAILABLE_NO;
	Decl *decl = expr->identifier_expr.decl;
	if (decl->decl_kind != DECL_VAR) return FAILABLE_NO;
	if (decl->var.kind == VARDECL_ALIAS && decl->var.alias->var.failable) return FAILABLE_UNWRAPPED;
	return decl->var.failable ? FAILABLE_YES : FAILABLE_NO;
}


static inline bool sema_type_error_on_binop(Context *context, Expr *expr)
{
	const char *c = token_type_to_string(binaryop_to_token(expr->binary_expr.operator));
	SEMA_ERROR(expr, "%s is not defined in the expression '%s' %s '%s'.",
	               c, type_to_error_string(expr->binary_expr.left->type),
	               c, type_to_error_string(expr->binary_expr.right->type));
	return false;
}

static bool expr_cast_to_index(Context *context, Expr *index)
{
	if (index->type->canonical->type_kind == type_usize->canonical->type_kind) return true;
	return cast_implicit(context, index, type_isize);
}

static inline bool sema_expr_analyse_ternary(Context *context, Type *to, Expr *expr)
{
	Expr *left = expr->ternary_expr.then_expr;
	Expr *cond = expr->ternary_expr.cond;
	// Normal
	if (left)
	{
		if (!sema_analyse_expr(context, type_bool, cond)) return expr_poison(expr);
		if (!cast_implicit(context, cond, type_bool)) return expr_poison(expr);
		if (!sema_analyse_expr(context, to, left)) return expr_poison(expr);
		expr->failable = left->failable | cond->failable;
	}
	else
	{
		// Elvis
		if (!sema_analyse_expr(context, to, cond)) return expr_poison(expr);
		expr->failable = cond->failable;
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

	expr->pure = cond->pure & left->pure & right->pure;
	expr->constant = cond->constant & left->constant & right->pure;

	expr->failable |= right->failable;
	Type *left_canonical = left->type->canonical;
	Type *right_canonical = right->type->canonical;
	if (type_is_ct(left_canonical) && type_is_ct(right_canonical))
	{
		if (!cast_implicitly_to_runtime(context, left)) return false;
		if (!cast_implicitly_to_runtime(context, right)) return false;
		left_canonical = left->type->canonical;
		right_canonical = right->type->canonical;
	}
	if (left_canonical != right_canonical)
	{
		Type *max = type_find_max_type(left_canonical, right_canonical);
		if (!max)
		{
			SEMA_ERROR(expr, "Cannot find a common parent type of '%s' and '%s'",
			               type_to_error_string(left_canonical), type_to_error_string(right_canonical));
			return false;
		}
		if (!cast_implicit(context, left, max) || !cast_implicit(context, right, max)) return false;
	}

	expr->type = left->type;
	return true;
}

static inline Decl *decl_copy_local_from_macro(Context *context, Decl *to_copy)
{
	if (!to_copy) return NULL;
	assert(to_copy->decl_kind == DECL_VAR);
	Decl *copy = COPY(to_copy);
	MACRO_COPY_TYPE(copy->var.type_info);
	MACRO_COPY_EXPR(copy->var.init_expr);
	return copy;
}

static inline Decl *decl_copy_label_from_macro(Context *context, Decl *to_copy, Ast *ast)
{
	if (!to_copy) return NULL;
	to_copy = decl_copy_local_from_macro(context, to_copy);
	to_copy->label.parent = astid(ast);
	return to_copy;
}

static inline Decl *decl_find_enum_constant(const char *name, Decl *decl)
{
	VECEACH(decl->enums.values, i)
	{
		Decl *enum_constant = decl->enums.values[i];
		if (enum_constant->name == name)
		{
			return enum_constant;
		}
	}
	return NULL;
}

static inline bool sema_expr_analyse_enum_constant(Expr *expr, const char *name, Decl *decl)
{
	Decl *enum_constant = decl_find_enum_constant(name, decl);
	if (!enum_constant) return false;

	assert(enum_constant->resolve_status == RESOLVE_DONE);
	expr->type = decl->type;
	expr->access_expr.ref = enum_constant;
	expr->expr_kind = EXPR_MEMBER_ACCESS;
	return true;
}


static inline bool find_possible_inferred_identifier(Type *to, Expr *expr)
{
	if (to->canonical->type_kind != TYPE_ENUM && to->canonical->type_kind != TYPE_ERRTYPE) return false;
	Decl *parent_decl = to->canonical->decl;
	switch (parent_decl->decl_kind)
	{
		case DECL_ENUM:
			return sema_expr_analyse_enum_constant(expr, expr->identifier_expr.identifier, parent_decl);
		case DECL_UNION:
		case DECL_STRUCT:
			return false;
		default:
			UNREACHABLE
	}

}

static inline bool sema_expr_analyse_identifier_resolve(Context *context, Type *to, Expr *expr, ExprIdentifier *id_expr)
{
	Decl *ambiguous_decl = NULL;
	Decl *private_symbol = NULL;
	expr->pure = true;

	DEBUG_LOG("Now resolving %s", id_expr->identifier);
	Decl *decl = sema_resolve_symbol(context,
	                                 id_expr->identifier,
	                                 id_expr->path,
	                                 &ambiguous_decl,
	                                 &private_symbol);
	if (!decl && !id_expr->path && to)
	{
		if (find_possible_inferred_identifier(to, expr)) return true;
	}

	if (!decl)
	{
		if (private_symbol)
		{
			SEMA_ERROR(expr, "'%s' is not visible from this module.", id_expr->identifier);
		}
		else if (ambiguous_decl)
		{
			SEMA_ERROR(expr, "The name '%s' ambiguous, please add a path.", id_expr->identifier);
		}
		else
		{
			SEMA_ERROR(expr, "'%s' could not be found, did you spell it right?", id_expr->identifier);
		}
		return false;
	}

	// Already handled
	if (!decl_ok(decl)) return false;

	if (ambiguous_decl)
	{
		SEMA_ERROR(expr,
		               "Ambiguous symbol '%s' – both defined in %s and %s, please add the module name to resolve the ambiguity",
		               id_expr->identifier,
		               decl->module->name->module,
		               ambiguous_decl->module->name->module);
		return false;
	}

	if (decl->decl_kind == DECL_FUNC && !id_expr->path && decl->module != context->module)
	{
		SEMA_ERROR(expr, "Functions from other modules must be prefixed with the module name, please add '%s' in front.", decl->module->name->module);
		return false;
	}
	if (decl->decl_kind == DECL_MACRO)
	{
		if (expr->expr_kind != EXPR_MACRO_IDENTIFIER)
		{
			SEMA_ERROR(expr, "Macro expansions must be prefixed with '@', try using '@%s(...)' instead.", decl->name);
			return false;
		}
		id_expr->decl = decl;
		expr->type = type_void;
		return true;
	}
	if (expr->expr_kind == EXPR_MACRO_IDENTIFIER)
	{
		SEMA_ERROR(expr, "Only macro expansions can be prefixed with '@', please try to remove it.", decl->name);
		return false;
	}
	if (decl->resolve_status != RESOLVE_DONE)
	{
		if (!sema_analyse_decl(context, decl)) return poisoned_decl;
	}
	if (decl->decl_kind == DECL_VAR && decl->var.failable)
	{
		expr->failable = true;
	}
	if (expr->expr_kind == EXPR_CONST_IDENTIFIER)
	{
		assert(decl->decl_kind == DECL_VAR);
		assert(decl->var.kind == VARDECL_CONST);
		assert(!decl->var.failable);
	}
	assert(decl->type);
	expr->identifier_expr.decl = decl;
	expr->type = decl->type;
	expr->pure = true;
	expr->constant = false;
	DEBUG_LOG("Resolution successful of %s.", decl->name);
	return true;
}

bool expr_is_constant_eval(Expr *expr)
{
	switch (expr->expr_kind)
	{
		case EXPR_CONST:
			return true;
		case EXPR_COMPOUND_LITERAL:
			return expr_is_constant_eval(expr->expr_compound_literal.initializer);
		case EXPR_INITIALIZER_LIST:
		{
			Expr** init_exprs = expr->expr_initializer.initializer_expr;
			switch (expr->expr_initializer.init_type)
			{
				case INITIALIZER_NORMAL:
				{
					VECEACH(init_exprs, i)
					{
						if (!expr_is_constant_eval(init_exprs[i])) return false;
					}
					return true;
				}
				default:
					return false;
			}
		}
		default:
			return false;
	}
}

static inline bool sema_expr_analyse_identifier(Context *context, Type *to, Expr *expr)
{
	Decl *ambiguous_decl = NULL;
	Decl *private_symbol = NULL;
	expr->pure = true;

	DEBUG_LOG("Now resolving %s", expr->identifier_expr.identifier);
	Decl *decl = sema_resolve_symbol(context,
	                                 expr->identifier_expr.identifier,
	                                 expr->identifier_expr.path,
	                                 &ambiguous_decl,
	                                 &private_symbol);
	if (!decl && !expr->identifier_expr.path && to)
	{
		if (find_possible_inferred_identifier(to, expr)) return true;
	}

	if (!decl)
	{
		if (private_symbol)
		{
			SEMA_ERROR(expr, "'%s' is not visible from this module.", expr->identifier_expr.identifier);
		}
		else if (ambiguous_decl)
		{
			SEMA_ERROR(expr, "The name '%s' ambiguous, please add a path.", expr->identifier_expr.identifier);
		}
		else
		{
			SEMA_ERROR(expr, "Identifier '%s' could not be found.", expr->identifier_expr.identifier);
		}
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
		if (expr->expr_kind != EXPR_MACRO_IDENTIFIER)
		{
			SEMA_ERROR(expr, "Macro expansions must be prefixed with '@', try using '@%s(...)' instead.", decl->name);
			return false;
		}
		expr->identifier_expr.decl = decl;
		expr->type = type_void;
		return true;
	}
	if (expr->expr_kind == EXPR_MACRO_IDENTIFIER)
	{
		SEMA_ERROR(expr, "Only macro expansions can be prefixed with '@', please try to remove it.", decl->name);
	}
	if (decl->resolve_status != RESOLVE_DONE)
	{
		if (!sema_analyse_decl(context, decl)) return poisoned_decl;
	}
	if (decl->decl_kind == DECL_VAR && decl->var.failable)
	{
		expr->failable = true;
	}
	if (decl->decl_kind == DECL_VAR)
	{
		switch (decl->var.kind)
		{
			case VARDECL_CONST:
				if (!decl->type)
				{
					Expr *copy = expr_copy_from_macro(context, decl->var.init_expr);
					if (!sema_analyse_expr(context, to, copy)) return false;
					if (!expr_is_constant_eval(copy))
					{
						SEMA_ERROR(expr, "Constant value did not evaluate to a constant.");
						return false;
					}
					expr_replace(expr, copy);
					return true;
				}
				if (decl->var.failable)
				{
					SEMA_ERROR(expr, "Constants may never be 'failable', please remove the '!'.");
					return false;
				}
				break;
			default:
				break;
		}
	}
	if (!decl->type) decl->type = type_void;
	expr->identifier_expr.decl = decl;
	expr->type = decl->type;
	expr->pure = true;
	expr->constant = false;
	DEBUG_LOG("Resolution successful of %s.", decl->name);
	return true;
}

static inline bool sema_expr_analyse_ct_identifier(Context *context, Type *to __unused, Expr *expr)
{
	Decl *ambiguous_decl = NULL;
	Decl *private_symbol = NULL;
	expr->pure = true;

	DEBUG_LOG("Now resolving %s", expr->ct_ident_expr.identifier);
	Decl *decl = sema_resolve_symbol(context,
	                                 expr->ct_ident_expr.identifier,
	                                 NULL,
	                                 &ambiguous_decl,
	                                 &private_symbol);

	assert(!ambiguous_decl && !private_symbol);
	if (!decl)
	{
		SEMA_ERROR(expr, "Compile time variable '%s' could not be found.", expr->ct_ident_expr.identifier);
		return false;
	}

	// Already handled
	if (!decl_ok(decl))
	{
		return expr_poison(expr);
	}

	DEBUG_LOG("Resolution successful of %s.", decl->name);
	assert(decl->decl_kind == DECL_VAR);
	assert(decl->resolve_status == RESOLVE_DONE);

	expr->ct_ident_expr.decl = decl;
	expr->type = decl->type;
	expr->pure = true;
	expr->constant = true;
	return true;
}

static inline bool sema_expr_analyse_hash_identifier(Context *context, Type *to __unused, Expr *expr)
{
	Decl *ambiguous_decl = NULL;
	Decl *private_symbol = NULL;
	expr->pure = true;

	DEBUG_LOG("Now resolving %s", expr->hash_ident_expr.identifier);
	Decl *decl = sema_resolve_symbol(context,
	                                 expr->hash_ident_expr.identifier,
	                                 NULL,
	                                 &ambiguous_decl,
	                                 &private_symbol);

	assert(!ambiguous_decl && !private_symbol);
	if (!decl)
	{
		SEMA_ERROR(expr, "Compile time variable '%s' could not be found.", expr->ct_ident_expr.identifier);
		return false;
	}

	// Already handled
	if (!decl_ok(decl))
	{
		return expr_poison(expr);
	}

	DEBUG_LOG("Resolution successful of %s.", decl->name);
	assert(decl->decl_kind == DECL_VAR);
	assert(decl->resolve_status == RESOLVE_DONE);

	assert(decl->var.init_expr->resolve_status == RESOLVE_DONE);
	expr_replace(expr, expr_copy_from_macro(context, decl->var.init_expr));
	return sema_analyse_expr(context, to, expr);
}

static inline bool sema_expr_analyse_binary_sub_expr(Context *context, Type *to, Expr *left, Expr *right)
{
	return sema_analyse_expr(context, to, left) & sema_analyse_expr(context, to, right);
}

static inline int find_index_of_named_parameter(Context *context, Decl** func_params, Expr *expr)
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

static inline bool sema_expr_analyse_func_invocation(Context *context, FunctionSignature *signature, Expr *expr, Decl *decl, Type *to, Expr *struct_var)
{
	Decl **func_params = signature->params;
	Expr **args = expr->call_expr.arguments;
	unsigned struct_args = struct_var == NULL ? 0 : 1;
	unsigned func_param_count = vec_size(func_params);
	unsigned num_args = vec_size(args) + struct_args;
	unsigned entries_needed = func_param_count > num_args ? func_param_count : num_args;
	Expr **actual_args = VECNEW(Expr*, entries_needed);
	for (unsigned i = 0; i < entries_needed; i++) vec_add(actual_args, NULL);
	memset(actual_args, 0, entries_needed * sizeof(Expr*));
	bool uses_named_parameters = false;

	for (unsigned i = 0; i < num_args; i++)
	{
		bool is_implicit = i < struct_args;
		Expr *arg = is_implicit ? struct_var : args[i - struct_args];
		// Named parameters
		if (!is_implicit && arg->expr_kind == EXPR_BINARY && arg->binary_expr.operator == BINARYOP_ASSIGN)
		{
			uses_named_parameters = true;
			int index = find_index_of_named_parameter(context, func_params, arg->binary_expr.left);
			if (index < 0) return false;
			if (actual_args[index])
			{
				SEMA_ERROR(arg, "The parameter '%s' was already set once.", func_params[index]->name);
				return false;
			}
			if (!sema_analyse_expr_of_required_type(context, func_params[index]->type, arg->binary_expr.right, 0)) return false;
			actual_args[index] = arg->binary_expr.right;
			expr->failable |= arg->binary_expr.right->failable;
			continue;
		}

		if (i >= func_param_count)
		{
			if (!signature->variadic)
			{
				SEMA_ERROR(expr, "Too many parameters for this function.");
				return false;
			}
			if (!sema_analyse_expr_of_required_type(context, NULL, arg, true)) return false;
			actual_args[i] = arg;
			expr->failable |= arg->failable;
			continue;
		}

		if (uses_named_parameters)
		{
			SEMA_ERROR(expr, "A regular parameter cannot follow a named parameter.");
			return false;
		}
		if (!sema_analyse_expr_of_required_type(context, func_params[i]->type, arg, true)) return false;
		expr->failable |= arg->failable;
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
	expr->type = signature->rtype->type;
	expr->call_expr.arguments = actual_args;
	expr->failable |= signature->failable;
	return true;
}

static inline bool sema_expr_analyse_var_call(Context *context, Type *to, Expr *expr, Decl *var_decl)
{
	Type *func_ptr_type = var_decl->type->canonical;
	expr->failable |= var_decl->var.failable;
	if (func_ptr_type->type_kind != TYPE_POINTER || func_ptr_type->pointer->type_kind != TYPE_FUNC)
	{
		SEMA_ERROR(expr, "Only macros, functions and function pointers maybe invoked, this is of type '%s'.", type_to_error_string(var_decl->type));
		return false;
	}
	expr->call_expr.is_pointer_call = true;
	return sema_expr_analyse_func_invocation(context,
	                                         func_ptr_type->pointer->func.signature,
	                                         expr,
	                                         func_ptr_type->decl,
	                                         to, NULL);

}

static inline bool sema_expr_analyse_generic_call(Context *context, Type *to, Expr *expr) { TODO };




static inline Type *unify_returns(Context *context, Type *to)
{
	bool all_returns_need_casts = false;
	// Let's unify the return statements.
	VECEACH(context->returns, i)
	{
		Ast *return_stmt = context->returns[i];
		Expr *ret_expr = return_stmt->return_stmt.expr;
		bool last_expr_was_void = to == type_void;
		Type *right_canonical = ret_expr ? ret_expr->type->canonical : type_void;
		bool current_expr_was_void = right_canonical == type_void;
		if (i > 0 && last_expr_was_void != current_expr_was_void)
		{
			SEMA_ERROR(return_stmt, "You can't combine empty returns with value returns.");
			SEMA_PREV(context->returns[i - 1], "Previous return was here.");
			return NULL;
		}
		if (to)
		{
			if (current_expr_was_void)
			{
				SEMA_ERROR(return_stmt, "The return must be a value of type '%s'.", type_to_error_string(to));
				return NULL;
			}
			if (!cast_implicit(context, ret_expr, to))
			{
				return NULL;
			}
			continue;
		}

		// The simple case.
		if (to == right_canonical) continue;

		// Try to find a common type:
		Type *max = type_find_max_type(to, right_canonical);
		if (!max)
		{
			SEMA_ERROR(return_stmt, "Cannot find a common parent type of '%s' and '%s'",
			               type_to_error_string(to), type_to_error_string(right_canonical));
			SEMA_PREV(context->returns[i - 1], "The previous return was here.");
			return false;
		}
		to = max;
		all_returns_need_casts = true;
	}
	if (all_returns_need_casts)
	{
		VECEACH(context->returns, i)
		{
			Ast *return_stmt = context->returns[i];
			Expr *ret_expr = return_stmt->return_stmt.expr;
			if (!cast_implicit(context, ret_expr, to))
			{
				return NULL;
			}
		}
	}
	return to;
}

static inline bool sema_expr_analyse_func_call(Context *context, Type *to, Expr *expr, Decl *decl, Expr *struct_var)
{
	expr->call_expr.is_pointer_call = false;
	return sema_expr_analyse_func_invocation(context, &decl->func.function_signature, expr, decl, to, struct_var);
}

static bool sema_check_stmt_compile_time(Context *context, Ast *ast);

static bool sema_check_expr_compile_time(Context *context, Expr *expr)
{
	switch (expr->expr_kind)
	{
		case EXPR_CONST:
			return true;
		case EXPR_MACRO_BLOCK:
			VECEACH(expr->macro_block.stmts, i)
			{
				if (!sema_check_stmt_compile_time(context, expr->macro_block.stmts[i])) return false;
			}
			return true;
		default:
			return false;
	}
	UNREACHABLE
}

static bool sema_check_stmt_compile_time(Context *context, Ast *ast)
{
	switch (ast->ast_kind)
	{
		case AST_NOP_STMT:
			return true;
		case AST_RETURN_STMT:
			if (!ast->return_stmt.expr) return true;
			return expr_is_constant_eval(ast->return_stmt.expr);
		case AST_EXPR_STMT:
			return sema_check_expr_compile_time(context, ast->expr_stmt);
		case AST_CT_COMPOUND_STMT:
		case AST_COMPOUND_STMT:
			VECEACH(ast->compound_stmt.stmts, i)
			{
				if (!sema_check_stmt_compile_time(context, ast->ct_compound_stmt[i])) return false;
			}
			return true;
		default:
			return false;
	}
}

static inline bool sema_expr_analyse_macro_call(Context *context, Type *to, Expr *call_expr, Decl *decl)
{
	// TODO failable
	if (context->macro_nesting >= MAX_MACRO_NESTING)
	{
		SEMA_ERROR(call_expr, "Too deep nesting (more than %d levels) when evaluating this macro.", MAX_MACRO_NESTING);
		return false;
	}

	Expr **args = call_expr->call_expr.arguments;
	Decl **func_params = decl->macro_decl.parameters;

	unsigned num_args = vec_size(args);
	if (num_args != vec_size(func_params))
	{
		// TODO
		SEMA_ERROR(call_expr, "Mismatch on number of arguments.");
		return false;
	}
	Decl **params = num_args > 0 ? VECNEW(Decl *, num_args) : NULL;
	for (unsigned i = 0; i < num_args; i++)
	{
		Expr *arg = args[i];
		Decl *param = decl_copy_local_from_macro(context, func_params[i]);
		vec_add(params, param);
		assert(param->decl_kind == DECL_VAR);
		assert(param->resolve_status == RESOLVE_NOT_DONE);
		param->resolve_status = RESOLVE_RUNNING;
		// Maybe there's a type, but in general a macro param may be
		// typeless.
		// Maybe we should actually do something like:
		// macro var foo(var a, var $b) to make it more uniform.
		if (param->var.type_info)
		{
			// Resolve it
			if (!sema_resolve_type_info(context, param->var.type_info)) return false;
			// And set the type, we're done.
			param->type = param->var.type_info->type;
		}
		switch (param->var.kind)
		{
			case VARDECL_PARAM_REF:
				// &foo
				if (!sema_analyse_expr_value(context, param->type, arg)) return false;
				if (param->type && param->type->canonical != arg->type->canonical)
				{
					SEMA_ERROR(arg, "'%s' cannot be implicitly cast to '%s'.", type_to_error_string(arg->type), type_to_error_string(param->type));
					return false;
				}
				break;
			case VARDECL_PARAM:
				// foo
				if (!sema_analyse_expr_of_required_type(context, param->type, arg, false)) return false;
				break;
			case VARDECL_PARAM_EXPR:
				// #foo
				// We push a scope here as this will prevent the expression from modifying
				// compile time variables during evaluation:
				context_push_scope(context);
				bool ok = sema_analyse_expr_of_required_type(context, param->type, arg, false);
				context_pop_scope(context);
				if (!ok) return false;
				break;
			case VARDECL_PARAM_CT:
				// $foo
				if (!sema_analyse_expr_of_required_type(context, param->type, arg, false)) return false;
				if (!expr_is_constant_eval(arg))
				{
					SEMA_ERROR(arg, "A compile time parameter must always be a constant, did you mistake it for a normal paramter?");
					return false;
				}
				break;
			case VARDECL_PARAM_CT_TYPE:
				// $Foo
				if (!sema_analyse_expr_value(context, NULL, arg)) return false;
				// TODO check typeof
				if (arg->expr_kind != EXPR_TYPEINFO)
				{
					SEMA_ERROR(arg, "A type, like 'int' or 'double' was expected for the parameter '%s'.", param->name);
					return false;
				}
				break;
			case VARDECL_CONST:
			case VARDECL_GLOBAL:
			case VARDECL_LOCAL:
			case VARDECL_MEMBER:
			case VARDECL_LOCAL_CT:
			case VARDECL_LOCAL_CT_TYPE:
			case VARDECL_ALIAS:
				UNREACHABLE
		}
		if (param->type)
		{
			if (!cast_implicit(context, arg, param->type)) return false;
		}
		else
		{
			param->type = arg->type;
		}
		param->var.init_expr = arg;
		param->resolve_status = RESOLVE_DONE;
	}

	context->macro_nesting++;
	context->macro_counter++;
	Decl **old_macro_locals_start = context->macro_locals_start;
	context->macro_locals_start = context->last_local;

	bool ok = true;

	Ast *body = ast_copy_from_macro(context, decl->macro_decl.body);

	TypeInfo *foo = decl->macro_decl.rtype;

	Ast **saved_returns = context_push_returns(context);
	context->expected_block_type = foo ? foo->type : to;
	context_push_scope_with_flags(context, SCOPE_MACRO);

	for (unsigned i = 0; i < num_args; i++)
	{
		Decl *param = params[i];
		sema_add_local(context, param);
	}

	VECEACH(body->compound_stmt.stmts, i)
	{
		if (!sema_analyse_statement(context, body->compound_stmt.stmts[i]))
		{
			ok = false;
			goto EXIT;
		}
	}

	if (!vec_size(context->returns))
	{
		if (to)
		{
			SEMA_ERROR(decl, "Missing return in macro that evaluates to %s.", type_to_error_string(to));
			ok = false;
			goto EXIT;
		}
	}

	Expr *first_return_expr = vec_size(context->returns) ? context->returns[0]->return_stmt.expr : NULL;
	Type *left_canonical = first_return_expr ? first_return_expr->type->canonical : type_void;
	// Let's unify the return statements.
	left_canonical = unify_returns(context, left_canonical);
	if (!left_canonical)
	{
		ok = false;
		goto EXIT;
	}
	call_expr->type = left_canonical;
	if (vec_size(context->returns) == 1)
	{
		Expr *result = context->returns[0]->return_stmt.expr;
		if (result && expr_is_constant_eval(result))
		{
			if (sema_check_stmt_compile_time(context, body))
			{
				expr_replace(call_expr, result);
				goto EXIT;
			}
		}
	}
	call_expr->expr_kind = EXPR_MACRO_BLOCK;
	call_expr->macro_block.stmts = body->compound_stmt.stmts;
	call_expr->macro_block.params = params;
	call_expr->macro_block.args = args;
	EXIT:
	context_pop_scope(context);
	context_pop_returns(context, saved_returns);
	context->macro_nesting--;
	context->macro_locals_start = old_macro_locals_start;
	return ok;
}

static inline bool sema_expr_analyse_call(Context *context, Type *to, Expr *expr)
{
	expr->constant = false;
	expr->pure = false;

	Expr *func_expr = expr->call_expr.function;
	if (!sema_analyse_expr_value(context, NULL, func_expr)) return false;
	expr->failable = func_expr->failable;
	Decl *decl;
	Expr *struct_var = NULL;
	switch (func_expr->expr_kind)
	{
		case EXPR_IDENTIFIER:
			decl = func_expr->identifier_expr.decl;
			break;
		case EXPR_ACCESS:
			decl = func_expr->access_expr.ref;
			if (decl->decl_kind == DECL_FUNC)
			{
				expr->call_expr.is_struct_function = true;
				struct_var = expr_new(EXPR_UNARY, func_expr->access_expr.parent->span);
				struct_var->unary_expr.expr = func_expr->access_expr.parent;
				struct_var->unary_expr.operator = UNARYOP_ADDR;
				struct_var->resolve_status = RESOLVE_DONE;
				assert(func_expr->access_expr.parent->resolve_status == RESOLVE_DONE);
				struct_var->type = type_get_ptr(struct_var->unary_expr.expr->type);
			}
			break;
		case EXPR_MACRO_IDENTIFIER:
			return sema_expr_analyse_macro_call(context, to, expr, func_expr->identifier_expr.decl);
		default:
			TODO
	}
	switch (decl->decl_kind)
	{
		case DECL_VAR:
			return sema_expr_analyse_var_call(context, to, expr, decl);
		case DECL_FUNC:
			return sema_expr_analyse_func_call(context, to, expr, decl, struct_var);
		case DECL_MACRO:
			UNREACHABLE
		case DECL_GENERIC:
			return sema_expr_analyse_generic_call(context, to, expr);
		case DECL_POISONED:
			return false;
		default:
			SEMA_ERROR(expr, "The expression cannot be called.");
			return false;
	}
}

static inline bool sema_expr_analyse_range(Context *context, Type *to, Expr *expr)
{
	Expr *left = expr->range_expr.left;
	Expr *right = expr->range_expr.right;
	bool success = sema_analyse_expr(context, to, left) & (!right || sema_analyse_expr(context, to, right));
	if (!success) return expr_poison(expr);
	Type *left_canonical = left->type->canonical;
	Type *right_canonical = right ? right->type->canonical : left_canonical;
	if (!type_is_any_integer(left_canonical))
	{
		SEMA_ERROR(left, "Expected an integer value in the range expression.");
		return false;
	}
	if (!type_is_any_integer(right_canonical))
	{
		SEMA_ERROR(right, "Expected an integer value in the range expression.");
		return false;
	}
	if (left_canonical != right_canonical)
	{
		Type *type = type_find_max_type(left_canonical, right_canonical);
		if (!cast_implicit(context, left, type) || !cast_implicit(context, right, type)) return expr_poison(expr);
	}
	if (left->expr_kind == EXPR_CONST && right && right->expr_kind == EXPR_CONST)
	{
		if (expr_const_compare(&left->const_expr, &right->const_expr, BINARYOP_GT))
		{
			SEMA_ERROR(expr, "Left side of the range is smaller than the right.");
			return false;
		}
	}
	expr->type = left->type;
	return true;
}

static bool expr_check_index_in_range(Context *context, Type *type, Expr *index_expr, bool end_index, bool from_end)
{
	assert(type == type->canonical);
	if (index_expr->expr_kind != EXPR_CONST) return true;
	if (!bigint_fits_in_bits(&index_expr->const_expr.i, 64, true))
	{
		SEMA_ERROR(index_expr, "Index does not fit into an 64-signed integer.");
		return false;
	}
	int64_t index = bigint_as_signed(&index_expr->const_expr.i);
	if (from_end && index < 0)
	{
		SEMA_ERROR(index_expr, "Negative numbers are not allowed when indexing from the end.");
		return false;
	}
	switch (type->type_kind)
	{
		case TYPE_POINTER:
			assert(!from_end);
			return true;
		case TYPE_ARRAY:
		{
			int64_t len = (int64_t)type->array.len;
			if (from_end)
			{
				index = len - index;
			}
			// Checking end can only be done for arrays.
			if (end_index && index > len)
			{
				SEMA_ERROR(index_expr, "Array end index out of bounds, was %lld, exceeding array length %lld.", (long long)index, (long long)len);
				return false;
			}
			if (!end_index && index >= len)
			{
				SEMA_ERROR(index_expr, "Array index out of bounds, was %lld, exceeding max array index %lld.", (long long)index, (long long)len - 1);
				return false;
			}
			break;
		}
		case TYPE_VARARRAY:
		case TYPE_SUBARRAY:
		case TYPE_STRING:
			// If not from end, just check the negative values.
			if (!from_end) break;
			// From end we can only do sanity checks ^0 is invalid for non-end index. ^-1 and less is invalid for all.
			if (index == 0 && !end_index)
			{
				SEMA_ERROR(index_expr,
				           "Array index out of bounds, index from end (%lld) must be greater than zero or it will exceed the max array index.",
				           (long long) index);
				return false;
			}
			return true;
		default:
			UNREACHABLE
	}
	if (index < 0)
	{
		SEMA_ERROR(index_expr, "Array index out of bounds, using a negative array index is only allowed with pointers.");
		return false;
	}
	return true;
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

	expr->constant = index->constant & subscripted->constant;
	expr->pure = index->pure & subscripted->pure;

	// Unless we already have type_usize, cast to type_isize;
	if (!expr_cast_to_index(context, index)) return false;

	// Check range
	if (!expr_check_index_in_range(context, type, index, false, expr->subscript_expr.from_back)) return false;

	expr->failable |= index->failable;
	expr->type = inner_type;
	return true;
}

static inline bool sema_expr_analyse_subscript(Context *context, Expr *expr)
{
	if (!sema_analyse_expr(context, NULL, expr->subscript_expr.expr)) return false;
	expr->failable = expr->subscript_expr.expr->failable;
	return sema_expr_analyse_subscript_after_parent_resolution(context, NULL, expr);
}

static inline bool sema_expr_analyse_slice_after_parent_resolution(Context *context, Type *parent, Expr *expr)
{
	assert(expr->expr_kind == EXPR_SLICE);
	Expr *subscripted = expr->slice_expr.expr;
	expr->pure = subscripted->pure;
	expr->constant = subscripted->constant;
	Type *type = parent ? parent->canonical : subscripted->type->canonical;
	Expr *start = expr->slice_expr.start;
	Expr *end = expr->slice_expr.end;
	Type *inner_type = type_get_indexed_type(type);
	if (!inner_type)
	{
		SEMA_ERROR((parent ? expr : subscripted), "Cannot slice '%s'.", type_to_error_string(type));
		return false;
	}

	if (!sema_analyse_expr(context, type_isize, start)) return false;
	expr->pure &= start->pure;
	expr->constant &= start->constant;
	if (end && !sema_analyse_expr(context, type_isize, end)) return false;
	expr->pure &= !end || end->pure;
	expr->constant &= !end || end->constant;

	// Unless we already have type_usize, cast to type_isize;
	if (!expr_cast_to_index(context, start)) return false;
	if (end && !expr_cast_to_index(context, end)) return false;

	// Check range
	if (type->type_kind == TYPE_POINTER)
	{
		if (expr->slice_expr.start_from_back)
		{
			SEMA_ERROR(expr->slice_expr.start, "Indexing from the end is not allowed for pointers.");
			return false;
		}
		if (!end)
		{
			SEMA_ERROR(expr, "Omitting end index is not allowed for pointers.");
			return false;
		}
		if (end && expr->slice_expr.end_from_back)
		{
			SEMA_ERROR(expr->slice_expr.end, "Indexing from the end is not allowed for pointers.");
			return false;
		}
	}
	if (!expr_check_index_in_range(context, type, start, false, expr->slice_expr.start_from_back)) return false;
	if (end && !expr_check_index_in_range(context, type, end, true, expr->slice_expr.end_from_back)) return false;

	if (start && end && start->expr_kind == EXPR_CONST && end->expr_kind == EXPR_CONST)
	{
		if (expr->slice_expr.start_from_back && expr->slice_expr.end_from_back)
		{
			if (expr_const_compare(&start->const_expr, &end->const_expr, BINARYOP_LT))
			{
				SEMA_ERROR(start, "Start index greater than end index.");
				return false;
			}
		}
		else
		{
			if (expr_const_compare(&start->const_expr, &end->const_expr, BINARYOP_GT))
			{
				SEMA_ERROR(start, "Start index greater than end index.");
				return false;
			}
		}
	}

	expr->failable |= start->failable;
	expr->type = type_get_subarray(inner_type);
	return true;
}

static inline bool sema_expr_analyse_slice(Context *context, Expr *expr)
{
	if (!sema_analyse_expr(context, NULL, expr->slice_expr.expr)) return false;
	expr->failable = expr->slice_expr.expr->failable;
	return sema_expr_analyse_slice_after_parent_resolution(context, NULL, expr);
}

static inline void insert_access_deref(Expr *expr)
{
	Expr *deref = expr_new(EXPR_UNARY, expr->span);
	deref->unary_expr.operator = UNARYOP_DEREF;
	deref->unary_expr.expr = expr->access_expr.parent;
	deref->resolve_status = RESOLVE_DONE;
	assert(expr->access_expr.parent->type->canonical->type_kind == TYPE_POINTER);
	deref->type = expr->access_expr.parent->type->canonical->pointer;
	deref->failable = expr->access_expr.parent->failable;
	expr->access_expr.parent = deref;
}

static inline bool sema_expr_analyse_group(Context *context, Type *to, Expr *expr)
{
	if (!sema_analyse_expr(context, NULL, expr->group_expr)) return false;
	*expr = *expr->group_expr;
	return true;
}


static inline void expr_rewrite_to_int_const(Expr *expr_to_rewrite, Type *type, uint64_t value)
{
	expr_to_rewrite->expr_kind = EXPR_CONST;
	expr_const_set_int(&expr_to_rewrite->const_expr, value, type->canonical->type_kind);
	expr_to_rewrite->type = type;
	expr_to_rewrite->resolve_status = RESOLVE_DONE;
}

static inline void expr_rewrite_to_string(Expr *expr_to_rewrite, const char *string)
{
	expr_to_rewrite->expr_kind = EXPR_CONST;
	expr_to_rewrite->constant = true;
	expr_to_rewrite->const_expr.kind = TYPE_STRING;
	expr_to_rewrite->const_expr.string.chars = (char *)string;
	expr_to_rewrite->const_expr.string.len = (int)strlen(string);
	expr_to_rewrite->pure = true;
	expr_to_rewrite->resolve_status = RESOLVE_DONE;
	expr_to_rewrite->type = type_string;
}


static bool sema_expr_analyse_typeinfo(Context *context, Expr *expr)
{
	expr->constant = true;
	expr->pure = true;

	TypeInfo *type_info = expr->type_expr;
	if (!sema_resolve_type_info(context, type_info)) return false;
	expr->type = type_typeinfo;
	return true;
}

/*
static inline bool sema_expr_analyse_member_access(Context *context, Expr *expr)
{
	Type *type = expr->access_expr.parent->type->decl->member_decl.type_info->type;
	Type *canonical = type->canonical;
	const char *sub_element = TOKSTR(expr->access_expr.sub_element);
	if (sub_element == kw_sizeof)
	{
		expr_rewrite_to_int_const(expr, type_usize, type_size(canonical));
		return true;
	}
	if (sub_element == kw_offsetof)
	{
		TODO // calculate offset.
	}
	// Possibly alignof
	if (!type_may_have_sub_elements(type))
	{
		SEMA_ERROR(expr, "'%s' does not have a member '%s'.", type_to_error_string(type), sub_element);
		return false;
	}
	Decl *decl = canonical->decl;

	switch (decl->decl_kind)
	{
		case DECL_ENUM:
			if (TOKTYPE(expr->access_expr.sub_element) == TOKEN_CONST_IDENT)
			{
				if (!sema_expr_analyse_enum_constant(expr, sub_element, decl))
				{
					SEMA_ERROR(expr,
					               "'%s' has no enumeration value '%s'.",
					               decl->name,
					               sub_element);
					return false;
				}
				return true;
			}
			break;
		case DECL_ERR:
		case DECL_UNION:
		case DECL_STRUCT:
			break;
		default:
			UNREACHABLE
	}

	VECEACH(decl->methods, i)
	{
		Decl *function = decl->methods[i];
		if (sub_element == function->name)
		{
			expr->access_expr.ref = function;
			expr->type = function->type;
			return true;
		}
	}

	if (decl_is_struct_type(decl))
	{
		VECEACH(decl->strukt.members, i)
		{
			Decl *member = decl->strukt.members[i];
			if (sub_element == member->name)
			{
				expr->access_expr.ref = member;
				expr->type = member->member_decl.reference_type;
				return true;
			}
		}
	}
	SEMA_ERROR(expr,
	               "No function or member '%s.%s' found.",
	               type_to_error_string(type),
	               sub_element);
	return false;
}
*/


static void add_members_to_context(Context *context, Decl *decl)
{
	if (decl_is_struct_type(decl))
	{
		Decl **members = decl->strukt.members;
		VECEACH(members, i)
		{
			Decl *member = members[i];
			if (member->name == NULL)
			{
				add_members_to_context(context, member);
				continue;
			}
			sema_add_member(context, member);
		}
	}
	VECEACH(decl->methods, i)
	{
		Decl *func = decl->methods[i];
		sema_add_member(context, func);
	}
}

static inline bool sema_expr_analyse_type_access(Context *context, Expr *expr, TypeInfo *parent, bool was_group)
{
	if (!was_group && type_kind_is_derived(parent->type->type_kind))
	{
		SEMA_ERROR(expr->access_expr.parent, "Array and pointer types must be enclosed in (), did you forget it?");
		return false;
	}

	expr->constant = true;
	expr->pure = true;

	Type *canonical = parent->type->canonical;
	TokenType type = TOKTYPE(expr->access_expr.sub_element);
	const char *name = TOKSTR(expr->access_expr.sub_element);
	if (type == TOKEN_TYPEID)
	{
		expr->type = type_typeid;
		expr->expr_kind = EXPR_TYPEID;
		expr->typeid_expr = parent;
		expr->resolve_status = RESOLVE_DONE;
		return true;
	}
	if (name == kw_sizeof)
	{
		expr_rewrite_to_int_const(expr, type_usize, type_size(canonical));
		return true;
	}
	if (name == kw_alignof)
	{
		expr_rewrite_to_int_const(expr, type_usize, type_abi_alignment(canonical));
		return true;
	}
	if (name == kw_nameof)
	{
		expr_rewrite_to_string(expr, canonical->name);
		return true;
	}
	if (name == kw_qnameof)
	{
		expr_rewrite_to_string(expr, type_generate_qname(canonical));
		return true;
	}
	if (!type_may_have_sub_elements(canonical))
	{
		SEMA_ERROR(expr, "'%s' does not have a property '%s'.", type_to_error_string(parent->type), name);
		return false;
	}
	Decl *decl = canonical->decl;
	// TODO add more constants that can be inspected?
	// e.g. SomeEnum.values, MyUnion.x.offset etc?
	switch (decl->decl_kind)
	{
		case DECL_ENUM:
			if (type == TOKEN_CONST_IDENT)
			{
				if (!sema_expr_analyse_enum_constant(expr, name, decl))
				{
					SEMA_ERROR(expr, "'%s' has no enumeration value '%s'.", decl->name, name);
					return false;
				}
				return true;
			}
			if (name == kw_sizeof)
			{
				expr_rewrite_to_int_const(expr, type_usize, type_size(decl->enums.type_info->type));
				return true;
			}
			if (name == kw_alignof)
			{
				expr_rewrite_to_int_const(expr, type_usize, type_abi_alignment(decl->enums.type_info->type));
				return true;
			}
			break;
		case DECL_ERR:
		case DECL_UNION:
		case DECL_STRUCT:
			if (name == kw_sizeof)
			{
				expr_rewrite_to_int_const(expr, type_usize, type_size(decl->type));
				return true;
			}
			if (name == kw_alignof)
			{
				expr_rewrite_to_int_const(expr, type_usize, type_abi_alignment(decl->type));
				return true;
			}
			break;
		default:
			UNREACHABLE
	}


	VECEACH(decl->methods, i)
	{
		Decl *function = decl->methods[i];
		if (name == function->name)
		{
			expr->access_expr.ref = function;
			expr->type = function->type;
			return true;
		}
	}
	VECEACH(decl->strukt.members, i)
	{
		Decl *member = decl->strukt.members[i];
		if (name == member->name)
		{
			expr->expr_kind = EXPR_MEMBER_ACCESS;
			expr->access_expr.ref = member;
			expr->type = member->type;
			return true;
		}
	}
	SEMA_ERROR(expr, "No function or member '%s.%s' found.", type_to_error_string(parent->type), name);
	return false;
}

static inline bool sema_expr_analyse_member_access(Context *context, Expr *expr)
{
	Expr *parent = expr->access_expr.parent;

	expr->constant = true;
	expr->pure = true;

	TokenType type = TOKTYPE(expr->access_expr.sub_element);
	const char *name = TOKSTR(expr->access_expr.sub_element);

	Decl *ref = parent->access_expr.ref;

	bool is_plain_member = ref->decl_kind == DECL_VAR;
	if (type == TOKEN_TYPEID)
	{
		expr->type = type_typeid;
		expr->expr_kind = EXPR_TYPEID;
		if (is_plain_member)
		{
			expr->typeid_expr = ref->var.type_info;
		}
		else
		{
			expr->typeid_expr = type_info_new_base(ref->type, parent->span);
		}
		expr->resolve_status = RESOLVE_DONE;
		return true;
	}
	if (name == kw_sizeof)
	{
		expr_rewrite_to_int_const(expr, type_usize, type_size(ref->type));
		return true;
	}
	if (name == kw_alignof)
	{
		expr_rewrite_to_int_const(expr, type_usize, type_abi_alignment(ref->type));
		return true;
	}
	if (name == kw_ordinal)
	{
		if (ref->decl_kind == DECL_ENUM_CONSTANT)
		{
			expr_rewrite_to_int_const(expr, type_usize, ref->enum_constant.ordinal);
			return true;
		}
	}
	if (name == kw_nameof)
	{
		TODO
	}
	if (name == kw_qnameof)
	{
		TODO
	}
	if (name == kw_kindof)
	{
		TODO
	}
	// If we have something like struct Foo { Bar b; int c; struct d { int e; } }
	// If we are inspecting Foo.c we're done here. Otherwise handle struct d / Bar b case
	// The same way.
	if (is_plain_member)
	{
		if (!type_is_structlike(ref->type))
		{
			SEMA_ERROR(expr, "'%s' does not have a member or property '%s'", type_to_error_string(ref->type), name);
			return false;
		}
		// Pretend to be an inline struct.
		ref = ref->type->decl;
	}
	VECEACH(ref->methods, i)
	{
		Decl *function = ref->methods[i];
		if (name == function->name)
		{
			expr->access_expr.ref = function;
			expr->type = function->type;
			return true;
		}
	}
	VECEACH(ref->strukt.members, i)
	{
		Decl *member = ref->strukt.members[i];
		if (name == member->name)
		{
			expr->expr_kind = EXPR_MEMBER_ACCESS;
			expr->access_expr.ref = member;
			expr->type = member->type;
			return true;
		}
	}
	SEMA_ERROR(expr, "No function or member '%s.%s' found.", "todo", name);
	TODO
	return false;
}

static inline bool sema_expr_analyse_access(Context *context, Expr *expr)
{
	Expr *parent = expr->access_expr.parent;
	bool was_group = parent->expr_kind == EXPR_GROUP;
	if (!sema_analyse_expr_value(context, NULL, parent)) return false;

	if (parent->type == type_typeinfo)
	{
		return sema_expr_analyse_type_access(context, expr, parent->type_expr, was_group);
	}
	if (parent->expr_kind == EXPR_MEMBER_ACCESS)
	{
		return sema_expr_analyse_member_access(context, expr);
	}
	expr->failable = parent->failable;

	assert(expr->expr_kind == EXPR_ACCESS);
	assert(parent->resolve_status == RESOLVE_DONE);

	Type *parent_type = parent->type;
	Type *type = parent_type->canonical;

	bool is_pointer = type->type_kind == TYPE_POINTER;
	if (is_pointer)
	{
		type = type->pointer;
	}
	const char *kw = TOKSTR(expr->access_expr.sub_element);
	switch (type->type_kind)
	{
		case TYPE_SUBARRAY:
			if (kw == kw_sizeof)
			{
				expr_rewrite_to_int_const(expr, type_usize, type_size(type));
				return true;
			}
			if (kw == kw_len)
			{
				expr->expr_kind = EXPR_LEN;
				expr->len_expr.inner = parent;
				expr->type = type_usize;
				expr->resolve_status = RESOLVE_DONE;
				return true;
			}
			goto NO_MATCH;
		case TYPE_ARRAY:
			if (kw == kw_sizeof)
			{
				expr_rewrite_to_int_const(expr, type_usize, type_size(type));
				return true;
			}
			if (kw == kw_len)
			{
				expr_rewrite_to_int_const(expr, type_usize, type->array.len);
				return true;
			}
			goto NO_MATCH;
		case TYPE_ENUM:
		case TYPE_ERRTYPE:
		case TYPE_STRUCT:
		case TYPE_UNION:
			break;
		default:
		NO_MATCH:
			SEMA_ERROR(expr, "Cannot access '%s' on '%s'", TOKSTR(expr->access_expr.sub_element), type_to_error_string(parent_type));
			return false;
	}
	Decl *decl = type->decl;
	context_push_scope(context);
	add_members_to_context(context, decl);
	Decl *member = sema_resolve_symbol_in_current_dynamic_scope(context, kw);

	context_pop_scope(context);
	if (!member)
	{
		SEMA_ERROR(expr, "There is no element or method '%s.%s'.", decl->name, kw);
		return false;
	}
	if (is_pointer)
	{
		if (!sema_cast_ident_rvalue(context, NULL, expr->access_expr.parent)) return false;
		insert_access_deref(expr);
	}
	expr->constant = expr->access_expr.parent->constant;
	expr->pure = expr->access_expr.parent->pure;

	expr->type = member->type;
	expr->access_expr.ref = member;
	return true;
}

static DesignatedPath *sema_analyse_init_path(Context *context, DesignatedPath *parent, Expr *expr, bool *has_found_match, bool *has_reported_error);

static DesignatedPath *sema_analyse_init_identifier_string(Context *context, DesignatedPath *parent_path, const char *string, bool *has_found_match, bool *has_reported_error)
{
	assert(type_is_structlike(parent_path->type));
	Decl **members = parent_path->type->decl->strukt.members;
	VECEACH(members, i)
	{
		Decl *member = members[i];
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
		if (member->name == string)
		{
			DesignatedPath *sub_path = CALLOCS(DesignatedPath);
			sub_path->type = member->type;
			sub_path->kind = DESIGNATED_IDENT;
			sub_path->index = i;
			parent_path->sub_path = sub_path;
			sub_path->pure = true;
			sub_path->constant = true;
			*has_found_match = true;
			return sub_path;
		}
	}
	return NULL;
}


static DesignatedPath *sema_analyse_init_access(Context *context, DesignatedPath *parent, Expr *access_expr, bool *has_found_match, bool *has_reported_error)
{
	DesignatedPath *last_path = sema_analyse_init_path(context, parent, access_expr->access_expr.parent, has_found_match, has_reported_error);
	if (!last_path) return NULL;
	DesignatedPath *path = sema_analyse_init_identifier_string(context, last_path,
	                                                           TOKSTR(access_expr->access_expr.sub_element), has_found_match, has_reported_error);
	if (path)
	{
		path->pure = true;
		path->constant = true;
	}
	if (!path && has_found_match && !has_reported_error)
	{
		SEMA_TOKID_ERROR(access_expr->access_expr.sub_element, "'%s' is not a valid sub member.", TOKSTR(access_expr->access_expr.sub_element));
		*has_reported_error = true;
	}
	return path;
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
	if (!expr_cast_to_index(context, index))
	{
		*has_reported_error = true;
		return NULL;
	}

	// Check range
	if (!expr_check_index_in_range(context, type->canonical, index, false, false))
	{
		*has_reported_error = true;
		return NULL;
	}

	DesignatedPath *sub_path = CALLOCS(DesignatedPath);
	path->sub_path = sub_path;
	sub_path->pure = index->pure;
	sub_path->constant = index->constant;
	path->constant &= index->pure;
	path->pure &= index->pure;

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

	initializer->pure = true;
	initializer->constant = true;
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
		path.pure = true;
		path.constant = true;
		bool has_reported_error = false;
		bool has_found_match = false;
		DesignatedPath *last_path = sema_analyse_init_path(context, &path, init_expr, &has_found_match, &has_reported_error);
		if (!has_reported_error && !last_path)
		{
			SEMA_ERROR(expr, "This is not a valid member of '%s'.", type_to_error_string(assigned));
			return false;
		}
		Expr *value = expr->binary_expr.right;
		if (!sema_analyse_expr_of_required_type(context, last_path->type, value, true)) return false;
		expr->pure = value->pure & last_path->pure;
		expr->constant = value->constant & last_path->constant;
		expr->expr_kind = EXPR_DESIGNATED_INITIALIZER;
		expr->designated_init_expr.path = path.sub_path;
		expr->designated_init_expr.value = value;
		expr->failable |= value->failable;
		expr->resolve_status = RESOLVE_DONE;
		initializer->pure &= expr->pure;
		initializer->constant &= expr->constant;
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
	initializer->pure = true;
	initializer->constant = true;

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
		Expr *element = elements[i];
		if (i >= expected_members)
		{
			SEMA_ERROR(element, "Too many elements in initializer, expected only %d.", expected_members);
			return false;
		}
		// 5. We know the required type, so resolve the expression.
		if (!sema_analyse_expr_of_required_type(context, members[i]->type, elements[i], 0)) return false;
		initializer->pure &= element->pure;
		initializer->constant &= element->constant;
		initializer->failable |= element->failable;
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
	initializer->pure = true;
	initializer->constant = true;

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
		Expr *element = elements[i];
		if (i >= expected_members)
		{
			SEMA_ERROR(element, "Too many elements in initializer, expected only %d.", expected_members);
			return false;
		}
		if (!sema_analyse_expr_of_required_type(context, inner_type, element, true)) return false;
		initializer->failable |= element->failable;
		initializer->pure &= element->pure;
		initializer->constant &= element->constant;
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

	return sema_expr_analyse_struct_plain_initializer(context, assigned->decl, expr);
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
		case TYPE_ERRTYPE:
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
	bool constant = true;
	bool pure = true;
	VECEACH(expr->expression_list, i)
	{
		Expr *checked_expr = expr->expression_list[i];
		success &= sema_analyse_expr_of_required_type(context, i == last ? to : NULL, checked_expr, 0);
		expr->failable |= checked_expr->failable;
		constant &= checked_expr->constant;
		pure &= checked_expr->pure;
	}
	expr->pure = pure;
	expr->constant = constant;
	return success;
}

static inline bool sema_expr_analyse_cast(Context *context, Type *to, Expr *expr)
{
	Expr *inner = expr->cast_expr.expr;
	bool success = sema_resolve_type_info(context, expr->cast_expr.type_info);
	success &= sema_analyse_expr(context, NULL, inner);
	expr->pure = inner->pure;
	expr->constant = inner->constant;
	if (!success) return false;

	if (!cast(context, inner, expr->cast_expr.type_info->type, CAST_TYPE_EXPLICIT)) return false;

	// TODO above is probably not right, cast type not set.
	// Overwrite cast.
	SourceSpan loc = expr->span;
	*expr = *inner;
	expr->span = loc;
	expr->failable = expr->failable;

	return true;
}

static inline bool sema_expr_analyse_slice_assign(Context *context, Expr *expr, Type *left_type, Expr *right, ExprFailableStatus lhs_is_failable)
{
	// 1. Evaluate right side to required type.
	if (!sema_analyse_expr_of_required_type(context, left_type->array.base, right, lhs_is_failable != FAILABLE_NO)) return false;

	Expr *left = expr->binary_expr.left;
	expr->type = right->type;
	expr->expr_kind = EXPR_SLICE_ASSIGN;
	expr->slice_assign_expr.left = left;
	expr->slice_assign_expr.right = right;

	return true;
}

bool sema_expr_analyse_assign_right_side(Context *context, Expr *expr, Type *left_type, Expr *right, ExprFailableStatus lhs_is_failable)
{
	if (expr && expr->binary_expr.left->expr_kind == EXPR_SLICE)
	{
		return sema_expr_analyse_slice_assign(context, expr, left_type, right, lhs_is_failable);
	}
	// 1. Evaluate right side to required type.
	if (!sema_analyse_expr_of_required_type(context, left_type, right, lhs_is_failable != FAILABLE_NO)) return false;

	// 2. Set the result to the type on the right side.
	if (expr) expr->type = right->type;

	return true;
}
static inline bool sema_expr_begin_analyse(Expr *expr)
{
	switch (expr->resolve_status)
	{
		case RESOLVE_NOT_DONE:
			expr->resolve_status = RESOLVE_RUNNING;
			return true;
		case RESOLVE_RUNNING:
			SEMA_ERROR(expr, "Recursive resolution of expression");
			return expr_poison(expr);
		case RESOLVE_DONE:
			return false;
	}
	UNREACHABLE
}


static inline bool sema_expr_analyse_ct_identifier_lvalue(Context *context, Expr *expr)
{
	if (!sema_expr_begin_analyse(expr)) return expr_ok(expr);

	Decl *ambiguous_decl = NULL;
	Decl *private_symbol = NULL;
	DEBUG_LOG("Now resolving %s", expr->ct_ident_expr.identifier);
	Decl *decl = sema_resolve_symbol(context,
	                                 expr->ct_ident_expr.identifier,
	                                 NULL,
	                                 &ambiguous_decl,
	                                 &private_symbol);
	assert(!ambiguous_decl && !private_symbol);

	// Skip if poisoned.
	if (!decl_ok(decl)) return false;

	if (!decl)
	{
		SEMA_ERROR(expr, "The compile time variable '%s' was not defined in this scope.", expr->ct_ident_expr.identifier);
		return expr_poison(expr);
	}

	if ((intptr_t)decl->var.scope < (intptr_t)context->current_scope)
	{
		SEMA_ERROR(expr, "Cannot modify '%s' inside of a runtime scope.", decl->name);
		return false;
	}
	expr->ct_ident_expr.decl = decl;
	expr->resolve_status = RESOLVE_DONE;
	return true;
}


static bool sema_expr_analyse_ct_identifier_assign(Context *context, Expr *expr, Expr *left, Expr *right)
{
	if (!sema_expr_analyse_ct_identifier_lvalue(context, left)) return false;

	// 3. Evaluate right side to required type.
	if (!sema_expr_analyse_assign_right_side(context, expr, left->type, right, true)) return false;

	left->ct_ident_expr.decl->var.init_expr = right;
	expr_replace(expr, right);
	return true;
}


/**
 * Analyse a = b
 * @return true if analysis works
 */
static bool sema_expr_analyse_assign(Context *context, Expr *expr, Expr *left, Expr *right)
{
	expr->constant = false;
	expr->pure = false;

	// 1. Evaluate left side
	if (left->expr_kind == EXPR_CT_IDENT)
	{
		return sema_expr_analyse_ct_identifier_assign(context, expr, left, right);
	}
	if (!sema_analyse_expr_value(context, NULL, left)) return false;

	// 2. Check assignability
	if (!expr_is_ltype(left))
	{
		SEMA_ERROR(left, "Expression is not assignable.");
		return false;
	}

	ExprFailableStatus failable_status = expr_is_failable(left);

	// 3. Evaluate right side to required type.
	if (!sema_expr_analyse_assign_right_side(context, expr, left->type, right, failable_status)) return false;

	if (failable_status == FAILABLE_UNWRAPPED && right->failable)
	{
		return sema_rewrap_var(context, left->identifier_expr.decl);
	}
	return true;
}


/**
 * Analyse *%= *= /= %= ^= |= &=
 *
 * @return true if analysis worked.
 */
static bool sema_expr_analyse_ct_common_assign(Context *context, Expr *expr, Expr *left)
{

	// 1. Analyse left side.
	if (!sema_expr_analyse_ct_identifier_lvalue(context, left)) return false;

	Decl *left_var = left->ct_ident_expr.decl;

	Expr *left_value = left_var->var.init_expr;
	assert(left_value);
	assert(!left_value->failable);

	expr->binary_expr.left = left_value;

	expr->binary_expr.operator = binaryop_assign_base_op(expr->binary_expr.operator);

	if (!sema_expr_analyse_binary(context, NULL, expr)) return false;

	if (expr->expr_kind != EXPR_CONST)
	{
		SEMA_ERROR(expr->binary_expr.right, "Expected a constant expression.");
		return false;
	}

	left->ct_ident_expr.decl->var.init_expr = expr;

	return true;
}

/**
 * Analyse *%= *= /= %= ^= |= &=
 *
 * @return true if analysis worked.
 */
static bool sema_expr_analyse_common_assign(Context *context, Expr *expr, Expr *left, Expr *right, bool int_only)
{
	expr->pure = false;
	expr->constant = false;

	if (left->expr_kind == EXPR_CT_IDENT)
	{
		return sema_expr_analyse_ct_common_assign(context, expr, left);
	}
	// 1. Analyse left side.
	if (!sema_analyse_expr_value(context, NULL, left)) return false;

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
	if (!sema_analyse_expr_of_required_type(context, left->type->canonical, right, expr_is_failable(left))) return false;

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
	expr->pure = false;
	expr->constant = false;

	if (left->expr_kind == EXPR_CT_IDENT)
	{
		return sema_expr_analyse_ct_common_assign(context, expr, left);
	}

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
		if (!cast_implicitly_to_runtime(context, right)) return false;

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
	if (!cast_implicit(context, right, left->type)) return false;

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

static bool binary_arithmetic_promotion(Context *context, Expr *left, Expr *right, Type *left_type, Type *right_type, Expr *parent, const char *error_message)
{
	Type *max = type_find_max_type(left_type, right_type);
	if (!max || !type_is_numeric(max))
	{
		if (!error_message)
		{
			return sema_type_error_on_binop(context, parent);
		}
		SEMA_ERROR(parent, error_message, type_to_error_string(left_type), type_to_error_string(right_type));
		return false;
	}
	return cast_implicit(context, left, max) && cast_implicit(context, right, max);
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

		// 5. Cast any compile time int into runtime version if we have a compile time constant.
		if (!cast_implicitly_to_runtime(context, right)) return false;

		right_type = right->type->canonical;
		
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
	if (!binary_arithmetic_promotion(context, left, right, left_type, right_type, expr, "Cannot subtract '%s' from '%s'"))
	{
		return false;
	}

	left_type = left->type->canonical;

	// 8. Handle constant folding.
	if (both_const(left, right))
	{
		expr->expr_kind = EXPR_CONST;
		expr->const_expr.kind = left_type->type_kind;
		switch (left_type->type_kind)
		{
			case ALL_INTS:
				if (is_mod && left_type != type_compint)
				{
					bigint_sub_wrap(&expr->const_expr.i,
					                &left->const_expr.i,
					                &right->const_expr.i,
					                left_type->builtin.bitsize,
					                type_is_signed(left_type));
				}
				else
				{
					bigint_sub(&expr->const_expr.i, &left->const_expr.i, &right->const_expr.i);
				}
				if (!expr_const_int_valid(expr, left_type)) return false;
				break;
			case ALL_FLOATS:
				// IMPROVE precision.
				expr->const_expr.f = left->const_expr.f - right->const_expr.f;
				break;
			default:
				UNREACHABLE
		}
	}

	// 9. Is this -%? That's not ok unless we are adding integers.
	if (is_mod && !type_is_any_integer(left->type->canonical))
	{
		SEMA_ERROR(expr, "'-%%' is only valid for integer subtraction, use '-' instead.");
		return false;
	}

	expr->pure = left->pure & right->pure;
	expr->constant = left->constant & right->constant;
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
		bool success = cast_implicit(context, right, type_is_unsigned(right_type) ? type_usize : type_isize);
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
	if (!binary_arithmetic_promotion(context, left, right, left_type, right_type, expr, "Cannot add '%s' to '%s'"))
	{
		return false;
	}

	left_type = left->type->canonical;

	// 6. Handle the "both const" case. We should only see ints and floats at this point.
	if (both_const(left, right))
	{
		expr->expr_kind = EXPR_CONST;
		expr->const_expr.kind = left_type->type_kind;
		switch (left->const_expr.kind)
		{
			case ALL_INTS:
				if (is_mod && left_type != type_compint)
				{
					bigint_add_wrap(&expr->const_expr.i, &left->const_expr.i, &right->const_expr.i, left_type->builtin.bitsize, type_is_signed(left_type));
				}
				else
				{
					bigint_add(&expr->const_expr.i, &left->const_expr.i, &right->const_expr.i);
				}
				if (!expr_const_int_valid(expr, left_type)) return false;
				break;
			case ALL_FLOATS:
				expr->const_expr.f = left->const_expr.f + right->const_expr.f;
				break;
			default:
				UNREACHABLE
		}
	}

	// 7. Is this +%? That's not ok unless we are adding integers.
	if (is_mod && !type_is_any_integer(left->type->canonical))
	{
		SEMA_ERROR(expr, "'+%%' is only valid for integer addition, use '+' instead.");
		return false;
	}

	// 7. Set the type
	expr->type = left->type;

	expr->pure = left->pure & right->pure;
	expr->constant = left->constant & right->constant;

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
	if (!binary_arithmetic_promotion(context, left, right, left_type, right_type, expr, "Cannot multiply '%s' by '%s'"))
	{
		return false;
	}

	// 3. Set the type.
	expr->type = left->type;

	// Might have changed
	left_type = left->type->canonical;

	// 4. Prevent *% use on non-integers.
	if (is_mod && !type_is_any_integer(left_type))
	{
		SEMA_ERROR(expr, "*%% can only be used with integer types, try * instead.");
		return false;
	}

	// 5. Handle constant folding.
	if (both_const(left, right))
	{
		expr->expr_kind = EXPR_CONST;
		expr->const_expr.kind = left_type->type_kind;

		switch (left->const_expr.kind)
		{
			case ALL_INTS:
				// 5a. Do mod mult if applicable.
				if (is_mod && left_type != type_compint)
				{
					bigint_mul_wrap(&expr->const_expr.i,
					                &left->const_expr.i,
					                &right->const_expr.i,
					                left_type->builtin.bitsize,
					                type_is_signed(left_type));
				}
				else
				{
					bigint_mul(&expr->const_expr.i, &left->const_expr.i, &right->const_expr.i);
				}
				if (!expr_const_int_valid(expr, left_type)) return false;
				break;
			case ALL_FLOATS:
				expr->const_expr.f = left->const_expr.f * right->const_expr.f;
				break;
			default:
				UNREACHABLE
		}
	}

	expr->pure = left->pure & right->pure;
	expr->constant = left->constant & right->constant;
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
	if (!binary_arithmetic_promotion(context, left, right, left_type, right_type, expr, "Cannot divide '%s' by '%s'."))
	{
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
	expr->pure = left->pure & right->pure;
	expr->constant = left->constant & right->constant;

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
		return sema_type_error_on_binop(context, expr);
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
	expr->pure = left->pure & right->pure;
	expr->constant = left->constant & right->constant;

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
		return sema_type_error_on_binop(context, expr);
	}

	// 3. Promote to the same type.

	Type *left_type = left->type->canonical;
	Type *right_type = right->type->canonical;

	if (!binary_arithmetic_promotion(context, left, right, left_type, right_type, expr, NULL))
	{
		return false;
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
	expr->pure = left->pure & right->pure;
	expr->constant = left->constant & right->constant;
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
		return sema_type_error_on_binop(context, expr);
	}

	expr->pure = left->pure & right->pure;
	expr->constant = left->constant & right->constant;

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
	if (!cast_implicit(context, left, to)) return false;

	// 6. As a last out, we make sure that a comptime int has a real type by casting to the right side (which must be non constant)
	if (type_is_ct(left->type))
	{
		assert(!type_is_ct(right->type));
		cast(context, left, right->type, CAST_TYPE_EXPLICIT);
	}

	// 7. On LLVM, left and right types must match.
	cast(context, right, left->type, CAST_TYPE_EXPLICIT);

	expr->type = left->type;
	return true;
}

/**
 * Analyse a <<= b a >>= b
 * @return true is the analysis succeeds, false otherwise.
 */
static bool sema_expr_analyse_shift_assign(Context *context, Expr *expr, Expr *left, Expr *right)
{
	expr->pure = false;
	expr->constant = false;

	// 1. Analyze the two sub lhs & rhs *without coercion*
	if (!sema_expr_analyse_binary_sub_expr(context, NULL, left, right)) return false;

	// 2. Ensure the left hand side is assignable
	if (!expr_is_ltype(left))
	{
		SEMA_ERROR(left, "Expression is not assignable.");
		return false;
	}

	// 2. Only integers may be shifted.
	if (!both_any_integer(left, right)) return sema_type_error_on_binop(context, expr);

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
	}

	// 4. Set the type
	expr->type = left->type;

	// 5. This is already checked as ok
	cast(context, right, left->type, CAST_TYPE_EXPLICIT);

	return true;
}


static bool sema_expr_analyse_and(Context *context, Expr *expr, Expr *left, Expr *right)
{
	if (!sema_analyse_expr(context, type_bool, left) & sema_analyse_expr(context, type_bool, right)) return false;
	if (!cast_implicit(context, left, type_bool) || !cast_implicit(context, right, type_bool)) return false;

	expr->type = type_bool;
	if (both_const(left, right))
	{
		expr_replace(expr, left);
		expr->const_expr.b &= right->const_expr.b;
	}
	expr->pure = left->pure & right->pure;
	expr->constant = left->constant & right->constant;
	return true;
}

static bool sema_expr_analyse_or(Context *context, Expr *expr, Expr *left, Expr *right)
{
	if (!sema_expr_analyse_binary_sub_expr(context, NULL, left, right)) return false;
	if (!cast_implicit(context, left, type_bool) || !cast_implicit(context, right, type_bool)) return false;

	if (both_const(left, right))
	{
		expr_replace(expr, left);
		expr->const_expr.b |= right->const_expr.b;
	}
	expr->pure = left->pure & right->pure;
	expr->constant = left->constant & right->constant;
	expr->type = type_bool;
	return true;
}



static void cast_to_max_bit_size(Context *context, Expr *left, Expr *right, Type *left_type, Type *right_type)
{
	int bit_size_left = left_type->builtin.bitsize;
	int bit_size_right = right_type->builtin.bitsize;
	assert(bit_size_left && bit_size_right);
	if (bit_size_left == bit_size_right) return;
	if (bit_size_left < bit_size_right)
	{
		Type *to = left->type->type_kind < TYPE_U8
				? type_int_signed_by_bitsize(bit_size_right)
				: type_int_unsigned_by_bitsize(bit_size_right);
		bool success = cast_implicit(context, left, to);
		assert(success);
		return;
	}
	Type *to = right->type->type_kind < TYPE_U8
	           ? type_int_signed_by_bitsize(bit_size_right)
	           : type_int_unsigned_by_bitsize(bit_size_right);
	bool success = cast_implicit(context, right, to);
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
		cast_to_max_bit_size(context, left, right, left_type, right_type);
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
				case TYPE_TYPEINFO:
				case TYPE_MEMBER:
				case TYPE_TYPEDEF:
					UNREACHABLE
				case TYPE_BOOL:
				case TYPE_ENUM:
				case TYPE_ERRTYPE:
				case TYPE_FUNC:
				case TYPE_STRUCT:
				case TYPE_UNION:
				case TYPE_ERR_UNION:
				case TYPE_STRING:
				case TYPE_ARRAY:
				case TYPE_VARARRAY:
				case TYPE_SUBARRAY:
				case TYPE_TYPEID:
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
				case TYPE_COMPLEX:
					TODO
				case TYPE_VECTOR:
					TODO
			}
		}

		// 6. Do the implicit cast.
		if (!cast_implicit(context, left, max)) goto ERR;
		if (!cast_implicit(context, right, max)) goto ERR;
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
	expr->pure = left->pure & right->pure;
	expr->constant = left->constant & right->constant;
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
	// 2. This could be a constant, in which case it is a null which is an error.
	if (inner->expr_kind == EXPR_CONST)
	{
		SEMA_ERROR(inner, "Dereferencing null is not allowed.");
		return false;
	}
	// 3. Now the type might not be a pointer because of a typedef,
	//    otherwise we need to use the the canonical representation.
	Type *deref_type = inner->type->type_kind != TYPE_POINTER ? inner->type : canonical;

	// 4. And... set the type.
	expr->type = deref_type->pointer;
	expr->constant = inner->constant;
	expr->pure = inner->pure;

	return true;
}

static inline bool sema_take_addr_of_var(Expr *expr, Decl *decl)
{
	if (decl->decl_kind != DECL_VAR) return false;
	switch (decl->var.kind)
	{
		case VARDECL_LOCAL:
		case VARDECL_GLOBAL:
		case VARDECL_PARAM:
		case VARDECL_PARAM_REF:
			decl->has_addr = true;
			return true;
		case VARDECL_CONST:
			if (!decl->var.type_info)
			{
				SEMA_ERROR(expr, "The constant is not typed, either type it or use && to take the reference to a temporary.");
				SEMA_PREV(decl, "The constant was defined here.");
				return false;
			}
			decl->has_addr = true;
			return true;
		case VARDECL_PARAM_EXPR:
			SEMA_ERROR(expr, "It is not possible to take the address of a captured expression, but you can use && to take a reference to the temporary value.");
			return false;
		case VARDECL_PARAM_CT:
		case VARDECL_PARAM_CT_TYPE:
		case VARDECL_LOCAL_CT_TYPE:
		case VARDECL_LOCAL_CT:
			// May not be reached due to EXPR_CT_IDENT being handled elsewhere.
			UNREACHABLE;
		case VARDECL_MEMBER:
		case VARDECL_ALIAS:
			UNREACHABLE
	}
	UNREACHABLE
}

static inline bool sema_take_addr_of_ident(Expr *inner)
{
	Decl *decl = inner->identifier_expr.decl;
	decl = decl_raw(decl);
	switch (decl->decl_kind)
	{
		case DECL_ENUM_CONSTANT:
		case DECL_FUNC:
			decl->has_addr = true;
			return true;
		case DECL_VAR:
			return sema_take_addr_of_var(inner, decl);
		default:
			SEMA_ERROR(inner, "It is not possible to take the address of a '%s'.", type_to_error_string(inner->type));
			return false;
	}
}

static bool sema_take_addr_of(Expr *inner)
{
	switch (inner->expr_kind)
	{
		case EXPR_CT_IDENT:
		case EXPR_MACRO_CT_IDENTIFIER:
			SEMA_ERROR(inner, "It's not possible to take the address of a compile time value.");
			return false;
		case EXPR_MACRO_IDENTIFIER:
			SEMA_ERROR(inner, "It's not possible to take the address of a macro.");
			return false;
		case EXPR_IDENTIFIER:
		case EXPR_CONST_IDENTIFIER:
			return sema_take_addr_of_ident(inner);
		case EXPR_UNARY:
			if (inner->unary_expr.operator == UNARYOP_DEREF) return true;
			break;
		case EXPR_ACCESS:
			return sema_take_addr_of(inner->access_expr.parent);
		case EXPR_GROUP:
			return sema_take_addr_of(inner->group_expr);
		case EXPR_SUBSCRIPT:
			return sema_take_addr_of(inner->subscript_expr.expr);
		case EXPR_SLICE:
			return sema_take_addr_of(inner->slice_expr.expr);
		default:
			break;
	}
	SEMA_ERROR(inner, "To take the address of a temporary value, use '&&' instead of '&'.", type_to_error_string(inner->type));
	return false;
}

/**
 * Analyse &a
 * @return true if analysis succeeds.
 */
static bool sema_expr_analyse_addr(Context *context, Type *to, Expr *expr, Expr *inner)
{
	Type *pointee = to ? to->canonical->pointer : NULL;

	// 1. Evaluate the expression
	REDO:
	switch (inner->expr_kind)
	{
		case EXPR_POISONED:
			return false;
		case EXPR_GROUP:
			// We want to collapse any grouping here.
			expr_replace(inner, inner->group_expr);
			goto REDO;
		default:
			if (!sema_analyse_expr_value(context, NULL, inner)) return expr_poison(expr);
	}

	// 2. Take the address.
	if (!sema_take_addr_of(inner)) return expr_poison(expr);

	// 3. Get the pointer of the underlying type.
	expr->type = type_get_ptr(inner->type);
	expr->constant = inner->constant;
	expr->pure = inner->pure;

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
	expr->constant = inner->constant;
	expr->pure = inner->pure;

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
	expr->constant = inner->constant;
	expr->pure = inner->pure;

	Type *canonical = inner->type->canonical;
	if (!type_is_integer(canonical) && canonical != type_bool)
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
			bigint_not(&expr->const_expr.i, &inner->const_expr.i, canonical->builtin.bitsize, true);
			break;
		case ALL_UNSIGNED_INTS:
			bigint_not(&expr->const_expr.i, &inner->const_expr.i, canonical->builtin.bitsize, false);
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
	expr->constant = inner->constant;
	expr->pure = inner->pure;

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
			case TYPE_ERRTYPE:
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
		case TYPE_ERR_UNION:
			UNREACHABLE
		case TYPE_FUNC:
		case TYPE_ARRAY:
		case TYPE_POINTER:
		case TYPE_VARARRAY:
		case TYPE_SUBARRAY:
		case TYPE_COMPLEX:
		case TYPE_BOOL:
		case TYPE_VECTOR:
		case ALL_REAL_FLOATS:
		case ALL_UNSIGNED_INTS:
		case ALL_SIGNED_INTS:
			return true;
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_VOID:
		case TYPE_STRING:
		case TYPE_ENUM:
		case TYPE_ERRTYPE:
		case TYPE_TYPEID:
		case TYPE_TYPEINFO:
		case TYPE_MEMBER:
			SEMA_ERROR(expr, "Cannot use 'not' on %s", type_to_error_string(inner->type));
			return false;
	}
	UNREACHABLE
}

static inline bool sema_expr_analyse_ct_incdec(Context *context, Expr *expr, Expr *inner)
{
	assert(inner->expr_kind == EXPR_CT_IDENT);

	if (!sema_expr_analyse_ct_identifier_lvalue(context, inner)) return false;

	Decl *var = inner->ct_ident_expr.decl;
	Expr *start_value = var->var.init_expr;
	assert(start_value->expr_kind == EXPR_CONST);

	switch (start_value->const_expr.kind)
	{
		case ALL_INTS:
			break;
		default:
			SEMA_ERROR(expr, "The compile time variable '%s' does not hold an integer.", var->name);
			return false;
	}

	Expr *end_value = COPY(start_value);

	// Make the change.
	BigInt change;
	bigint_init_signed(&change, expr->unary_expr.operator == UNARYOP_DEC ? -1 : 1);
	bigint_add(&end_value->const_expr.i, &start_value->const_expr.i, &change);
	if (!expr_const_int_valid(end_value, start_value->type)) return false;

	var->var.init_expr = end_value;

	if (expr->expr_kind == EXPR_POST_UNARY)
	{
		expr_replace(expr, start_value);
	}
	else
	{
		expr_replace(expr, end_value);
	}
	return true;
}

/**
 * Analyse foo++ foo-- --foo ++foo
 * @return false if analysis fails.
 */
static inline bool sema_expr_analyse_incdec(Context *context, Expr *expr, Expr *inner)
{
	expr->constant = false;
	expr->pure = false;

	if (!expr_is_ltype(inner))
	{
		SEMA_ERROR(inner, "Expression cannot be assigned to.");
		return false;
	}
	if (inner->expr_kind == EXPR_CT_IDENT)
	{
		return sema_expr_analyse_ct_incdec(context, expr, inner);
	}

	if (!type_is_numeric(inner->type->canonical) && inner->type->canonical->type_kind != TYPE_POINTER)
	{
		SEMA_ERROR(inner, "Expression must be a number or a pointer.");
		return false;
	}
	expr->type = inner->type;
	return true;
}

static inline bool sema_expr_analyse_taddr(Context *context, Type *to, Expr *expr, Expr *inner)
{
	Type *inferred_type = NULL;
	if (to->type_kind == TYPE_POINTER)
	{
		inferred_type = to->pointer;
	}

	if (!sema_analyse_expr(context, inferred_type, inner)) return false;

	expr->constant = inner->constant;
	expr->pure = inner->pure;
	expr->type = type_get_ptr(inner->type);
	return true;
}


static inline bool sema_expr_analyse_binary(Context *context, Type *to, Expr *expr)
{
	assert(expr->resolve_status == RESOLVE_RUNNING);
	Expr *left = expr->binary_expr.left;
	Expr *right = expr->binary_expr.right;
	// Don't push down bool conversions for example.
	if (to && !type_is_numeric(to)) to = NULL;
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
	switch (expr->unary_expr.operator)
	{
		case UNARYOP_DEREF:
			if (!sema_analyse_expr(context, NULL, inner)) return false;
			return sema_expr_analyse_deref(context, expr, inner);
		case UNARYOP_ADDR:
			return sema_expr_analyse_addr(context, to, expr, inner);
		case UNARYOP_NEG:
		case UNARYOP_NEGMOD:
			if (!sema_analyse_expr(context, NULL, inner)) return false;
			return sema_expr_analyse_neg(context, to, expr, inner);
		case UNARYOP_BITNEG:
			if (!sema_analyse_expr(context, to, inner)) return false;
			return sema_expr_analyse_bit_not(context, to, expr, inner);
		case UNARYOP_NOT:
			if (!sema_analyse_expr(context, NULL, inner)) return false;
			return sema_expr_analyse_not(context, to, expr, inner);
		case UNARYOP_DEC:
		case UNARYOP_INC:
			if (!sema_analyse_expr_value(context, NULL, inner)) return false;
			return sema_expr_analyse_incdec(context, expr, inner);
		case UNARYOP_TADDR:
			return sema_expr_analyse_taddr(context, to, expr, inner);
		case UNARYOP_ERROR:
			return false;
	}
	UNREACHABLE
}

static inline bool sema_expr_analyse_post_unary(Context *context, Type *to, Expr *expr)
{
	assert(expr->resolve_status == RESOLVE_RUNNING);
	Expr *inner = expr->post_expr.expr;

	if (!sema_analyse_expr_value(context, NULL, inner)) return false;

	return sema_expr_analyse_incdec(context, expr, inner);
}


static inline bool sema_expr_analyse_try(Context *context, Expr *expr)
{
	Expr *inner = expr->trycatch_expr;
	bool success = sema_analyse_expr(context, NULL, inner);
	expr->pure = inner->pure;
	expr->constant = false;
	if (!success) return false;
	if (!inner->failable)
	{
		SEMA_ERROR(expr->trycatch_expr, "Expected a failable expression to 'try'.");
		return false;
	}
	expr->type = type_bool;
	return true;
}

static inline bool sema_expr_analyse_catch(Context *context, Expr *expr)
{
	Expr *inner = expr->trycatch_expr;
	bool success = sema_analyse_expr(context, NULL, inner);
	expr->pure = inner->pure;
	expr->constant = false;
	if (!success) return false;
	if (!inner->failable)
	{
		SEMA_ERROR(expr->trycatch_expr, "Expected a failable expression to 'catch'.");
		return false;
	}
	expr->type = type_bool;
	return true;
}


static inline bool sema_expr_analyse_else(Context *context, Type *to, Expr *expr)
{
	Expr *inner = expr->else_expr.expr;
	bool success = sema_analyse_expr(context, to, inner);
	expr->pure = inner->pure;
	expr->constant = false;
	if (!success) return false;
	Type *type = inner->type;
	if (!inner->failable)
	{
		SEMA_ERROR(inner, "No failable to 'else' in the expression, please remove the 'else'.");
		return false;
	}
	if (expr->else_expr.is_jump)
	{
		expr->pure = false;
		if (!sema_analyse_statement(context, expr->else_expr.else_stmt)) return false;
		expr->type = type;
		return true;
	}

	// First we analyse the "else" and try to implictly cast.
	if (!sema_analyse_expr(context, type, expr->else_expr.else_expr)) return false;
	expr->pure &= expr->else_expr.else_expr->pure;
	// Here we might need to insert casts.
	Type *common = type_find_max_type(type, expr->else_expr.else_expr->type);
	if (!cast_implicit(context, expr->else_expr.else_expr, common)) return false;

	expr->type = common;
	return cast_implicit(context, expr->else_expr.expr, common);
}

static inline bool sema_expr_analyse_guard(Context *context, Type *to, Expr *expr)
{
	Expr *inner = expr->guard_expr.inner;
	bool success = sema_analyse_expr(context, to, inner);
	expr->guard_expr.defer = context->current_scope->defer_last;
	if (!success) return false;
	expr->type = inner->type;
	expr->pure = false;
	expr->constant = false;
	if (!inner->failable)
	{
		SEMA_ERROR(expr, "No failable to rethrow before '!!' in the expression, please remove '!!'.");
		return false;
	}
	if (!context->failable_return)
	{
		SEMA_ERROR(expr, "This expression implicitly returns with a failable result, but the function does not allow failable results. Did you mean to use 'else' instead?");
		return false;
	}
	return true;
}

static Ast *ast_shallow_copy(Ast *source)
{
	return COPY(source);
}

static Expr *expr_shallow_copy(Expr *source)
{
	return COPY(source);
}



static TypeInfo *type_info_copy_from_macro(Context *context, TypeInfo *source)
{
	if (!source) return NULL;
	TypeInfo *copy = malloc_arena(sizeof(TypeInfo));
	memcpy(copy, source, sizeof(TypeInfo));
	switch (source->kind)
	{
		case TYPE_INFO_POISON:
			return copy;
		case TYPE_INFO_IDENTIFIER:
			return copy;
		case TYPE_INFO_EXPRESSION:
			assert(source->resolve_status == RESOLVE_NOT_DONE);
			copy->unresolved_type_expr = expr_copy_from_macro(context, source->unresolved_type_expr);
			return copy;
		case TYPE_INFO_ARRAY:
			assert(source->resolve_status == RESOLVE_NOT_DONE);
			copy->array.len = expr_copy_from_macro(context, source->array.len);
			copy->array.base = type_info_copy_from_macro(context, source->array.base);
			return copy;
		case TYPE_INFO_INC_ARRAY:
		case TYPE_INFO_VARARRAY:
		case TYPE_INFO_SUBARRAY:
			assert(source->resolve_status == RESOLVE_NOT_DONE);
			copy->array.base = type_info_copy_from_macro(context, source->array.base);
			return copy;
		case TYPE_INFO_POINTER:
			assert(source->resolve_status == RESOLVE_NOT_DONE);
			copy->pointer = type_info_copy_from_macro(context, source->pointer);
			return copy;
	}
	UNREACHABLE
}


static Ast** ast_copy_list_from_macro(Context *context, Ast **to_copy)
{
	Ast **result = NULL;
	VECEACH(to_copy, i)
	{
		vec_add(result, ast_copy_from_macro(context, to_copy[i]));
	}
	return result;
}


static Expr *expr_copy_from_macro(Context *context, Expr *source_expr)
{
	if (!source_expr) return NULL;
	Expr *expr = expr_shallow_copy(source_expr);
	switch (source_expr->expr_kind)
	{
		case EXPR_ENUM_CONSTANT:
		case EXPR_MEMBER_ACCESS:
			UNREACHABLE
		case EXPR_UNDEF:
			return expr;
		case EXPR_CONST_IDENTIFIER:
		case EXPR_MACRO_IDENTIFIER:
		case EXPR_CT_IDENT:
		case EXPR_MACRO_CT_IDENTIFIER:
		case EXPR_HASH_IDENT:
			// TODO
			return expr;
		case EXPR_TYPEINFO:
			MACRO_COPY_TYPE(expr->type_expr);
			return expr;
		case EXPR_SLICE_ASSIGN:
			MACRO_COPY_EXPR(expr->slice_assign_expr.left);
			MACRO_COPY_EXPR(expr->slice_assign_expr.right);
			return expr;
		case EXPR_SLICE:
			MACRO_COPY_EXPR(expr->slice_expr.expr);
			MACRO_COPY_EXPR(expr->slice_expr.start);
			MACRO_COPY_EXPR(expr->slice_expr.end);
			return expr;
		case EXPR_LEN:
			MACRO_COPY_EXPR(expr->len_expr.inner);
			return expr;
		case EXPR_CATCH:
		case EXPR_TRY:
			MACRO_COPY_EXPR(expr->trycatch_expr);
			return expr;
		case EXPR_DECL_LIST:
			MACRO_COPY_AST_LIST(expr->dexpr_list_expr);
			return expr;
		case EXPR_FAILABLE:
			MACRO_COPY_EXPR(expr->failable_expr);
			return expr;
		case EXPR_ELSE:
			MACRO_COPY_EXPR(expr->else_expr.expr);
			if (expr->else_expr.is_jump)
			{
				MACRO_COPY_EXPR(expr->else_expr.else_expr);
			}
			else
			{
				MACRO_COPY_AST(expr->else_expr.else_stmt);
			}
			return expr;
		case EXPR_MACRO_BLOCK:
			UNREACHABLE
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
		case EXPR_EXPR_BLOCK:
			MACRO_COPY_AST_LIST(expr->expr_block.stmts);
			return expr;
		case EXPR_POISONED:
			return source_expr;
		case EXPR_GUARD:
			MACRO_COPY_EXPR(expr->guard_expr.inner);
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
			// TODO
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
	}
	UNREACHABLE
}

static Expr **expr_copy_expr_list_from_macro(Context *context, Expr **expr_list)
{
	Expr **result = NULL;
	VECEACH(expr_list, i)
	{
		vec_add(result, expr_copy_from_macro(context, expr_list[i]));
	}
	return result;
}


static inline void copy_flow(Context *context, Ast *ast)
{
	ast->flow.label = decl_copy_label_from_macro(context, ast->flow.label, ast);
}

static TypeInfo** type_info_copy_list_from_macro(Context *context, TypeInfo **to_copy)
{
	TypeInfo **result = NULL;
	VECEACH(to_copy, i)
	{
		vec_add(result, type_info_copy_from_macro(context, to_copy[i]));
	}
	return result;
}

static Ast *ast_copy_from_macro(Context *context, Ast *source)
{
	if (!source) return NULL;
	Ast *ast = ast_shallow_copy(source);
	switch (source->ast_kind)
	{
		case AST_POISONED:
			return ast;
		case AST_ASM_STMT:
			TODO
		case AST_ASSERT_STMT:
			MACRO_COPY_EXPR(ast->ct_assert_stmt.expr);
			MACRO_COPY_EXPR(ast->ct_assert_stmt.message);
			return ast;
		case AST_BREAK_STMT:
			return ast;
		case AST_CASE_STMT:
			MACRO_COPY_AST(ast->case_stmt.body);
			if (ast->case_stmt.is_type)
			{
				MACRO_COPY_TYPE(ast->case_stmt.type_info);
			}
			else
			{
				MACRO_COPY_EXPR(ast->case_stmt.expr);
			}
			return ast;
			break;
		case AST_CATCH_STMT:
			copy_flow(context, ast);
			if (ast->catch_stmt.has_err_var)
			{
				MACRO_COPY_DECL(ast->catch_stmt.err_var);
			}
			else
			{
				MACRO_COPY_EXPR(ast->catch_stmt.catchable);
			}
			if (ast->catch_stmt.is_switch)
			{
				MACRO_COPY_AST_LIST(ast->catch_stmt.cases);
			}
			else
			{
				MACRO_COPY_AST(ast->catch_stmt.body);
			}
			return ast;
		case AST_COMPOUND_STMT:
			MACRO_COPY_AST_LIST(ast->compound_stmt.stmts);
			return ast;
		case AST_CT_COMPOUND_STMT:
			MACRO_COPY_AST_LIST(ast->ct_compound_stmt);
			return ast;
		case AST_CONTINUE_STMT:
			TODO
			return ast;
		case AST_CT_ASSERT:
			MACRO_COPY_EXPR(ast->ct_assert_stmt.message);
			MACRO_COPY_EXPR(ast->ct_assert_stmt.expr);
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
		case AST_DECLARE_STMT:
			MACRO_COPY_DECL(ast->declare_stmt);
			return ast;
		case AST_DEFAULT_STMT:
			MACRO_COPY_AST(ast->case_stmt.body);
			return ast;
		case AST_DEFINE_STMT:
			ast->define_stmt = decl_copy_local_from_macro(context, ast->define_stmt);
			return ast;
		case AST_DEFER_STMT:
			assert(!ast->defer_stmt.prev_defer);
			MACRO_COPY_AST(ast->defer_stmt.body);
			return ast;
		case AST_DO_STMT:
			copy_flow(context, ast);
			MACRO_COPY_AST(ast->do_stmt.body);
			MACRO_COPY_EXPR(ast->do_stmt.expr);
			return ast;
		case AST_EXPR_STMT:
			MACRO_COPY_EXPR(ast->expr_stmt);
			return ast;
		case AST_FOR_STMT:
			copy_flow(context, ast);
			MACRO_COPY_EXPR(ast->for_stmt.cond);
			MACRO_COPY_EXPR(ast->for_stmt.incr);
			MACRO_COPY_AST(ast->for_stmt.body);
			MACRO_COPY_EXPR(ast->for_stmt.init);
			return ast;
		case AST_IF_STMT:
			copy_flow(context, ast);
			MACRO_COPY_EXPR(ast->if_stmt.cond);
			MACRO_COPY_AST(ast->if_stmt.else_body);
			MACRO_COPY_AST(ast->if_stmt.then_body);
			return ast;
		case AST_NEXT_STMT:
			MACRO_COPY_EXPR(ast->next_stmt.switch_expr);
			TODO
			return ast;
		case AST_NOP_STMT:
			return ast;
		case AST_RETURN_STMT:
			MACRO_COPY_EXPR(ast->return_stmt.expr);
			return ast;
		case AST_SWITCH_STMT:
			copy_flow(context, ast);
			MACRO_COPY_EXPR(ast->switch_stmt.cond);
			MACRO_COPY_AST_LIST(ast->switch_stmt.cases);
			return ast;
		case AST_TRY_STMT:
			MACRO_COPY_EXPR(ast->try_stmt.decl_expr);
			MACRO_COPY_AST(ast->try_stmt.body);
			return ast;
		case AST_UNREACHABLE_STMT:
			return ast;
		case AST_VOLATILE_STMT:
			TODO
			return ast;
		case AST_WHILE_STMT:
			copy_flow(context, ast);
			MACRO_COPY_EXPR(ast->while_stmt.cond);
			MACRO_COPY_AST(ast->while_stmt.body);
			return ast;
		case AST_SCOPED_STMT:
			MACRO_COPY_AST(ast->scoped_stmt.stmt);
			return ast;
	}
	UNREACHABLE;
}



static inline bool sema_expr_analyse_macro_ident(Context *context, Type *to, Expr *expr)
{
	return false;
	/*
	Expr *inner = expr->macro_ident;
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
	}*/
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






static inline bool sema_expr_analyse_expr_block(Context *context, Type *to, Expr *expr)
{
	bool success = true;
	expr->type = type_void;
	bool saved_expr_failable_return = context->expr_failable_return;
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

	if (!context->current_scope->jump_end && to)
	{
		SEMA_ERROR(expr, "Expected the block to return with a value of type %s.", type_to_error_string(to));
		success = false;
	}
	if (!vec_size(context->returns)) goto EXIT;

	Expr *first_return_expr = context->returns[0]->return_stmt.expr;
	Type *left_canonical = first_return_expr ? first_return_expr->type->canonical : type_void;
	// Let's unify the return statements.
	left_canonical = unify_returns(context, left_canonical);
	if (!left_canonical)
	{
		success = false;
		goto EXIT;
	}
	expr->type = left_canonical;
EXIT:
	context_pop_scope(context);
	context_pop_returns(context, saved_returns);
	expr->failable = context->expr_failable_return;
	context->expr_failable_return = saved_expr_failable_return;
	context->expected_block_type = prev_expected_block_type;
	expr->constant = false;
	expr->pure = false;
	return success;
}


static inline bool sema_expr_analyse_compound_literal(Context *context, Type *to, Expr *expr)
{
	if (!sema_resolve_type_info(context, expr->expr_compound_literal.type_info)) return false;
	Type *type = expr->expr_compound_literal.type_info->type;
	if (!sema_expr_analyse_initializer_list(context, type, expr->expr_compound_literal.initializer)) return false;
	expr->pure = expr->expr_compound_literal.initializer->pure;
	expr->constant = expr->expr_compound_literal.initializer->constant;
	expr->type = type;
	expr->failable = expr->expr_compound_literal.initializer->failable;
	return true;
}

static inline bool sema_expr_analyse_typeof(Context *context, Expr *expr)
{
	if (!sema_analyse_expr(context, NULL, expr->typeof_expr)) return false;
	expr->pure = expr->typeof_expr->pure;
	expr->constant = expr->typeof_expr->constant;
	Type *type = expr->typeof_expr->type->canonical;
	expr->expr_kind = EXPR_TYPEID;
	expr->typeid_expr = type_info_new_base(type, expr->typeof_expr->span);
	expr->type = type_typeid;
	return true;
}


static inline bool sema_expr_analyse_failable(Context *context, Type *to, Expr *expr)
{
	Expr *inner = expr->failable_expr;
	if (!sema_analyse_expr(context, NULL, inner)) return false;
	expr->pure = inner->pure;
	expr->constant = inner->constant;
	if (inner->expr_kind == EXPR_TYPEINFO)
	{
		TypeInfo *inner_type_info = inner->type_expr;
		if (inner_type_info->type->type_kind != TYPE_ERRTYPE)
		{
			SEMA_ERROR(inner, "This must be an error type.");
			return false;
		}
		inner->expr_kind = EXPR_COMPOUND_LITERAL;
		inner->expr_compound_literal.type_info = inner_type_info;
		inner->expr_compound_literal.initializer = NULL;
		inner->type = inner_type_info->type;
	}
	if (inner->failable)
	{
		SEMA_ERROR(inner, "The inner expression is already a failable.");
	}
	Type *type = inner->type->canonical;
	if (inner->expr_kind == EXPR_FAILABLE)
	{
		SEMA_ERROR(inner, "It looks like you added one too many '!' after the error.");
		return false;
	}
	if (type->type_kind != TYPE_ERRTYPE && type->type_kind != TYPE_ERR_UNION)
	{
		SEMA_ERROR(inner, "You cannot use the '!' operator on expressions of type '%s'", type_to_error_string(type));
		return false;
	}
	if (!to)
	{
		expr->type = type_void;
		return true;
	}
	expr->failable = true;
	expr->type = to;
	return true;
}


static inline bool sema_analyse_expr_dispatch(Context *context, Type *to, Expr *expr)
{
	switch (expr->expr_kind)
	{
		case EXPR_DECL_LIST:
		case EXPR_UNDEF:
		case EXPR_ENUM_CONSTANT:
		case EXPR_MEMBER_ACCESS:
			UNREACHABLE
		case EXPR_HASH_IDENT:
			return sema_expr_analyse_hash_identifier(context, to, expr);
		case EXPR_MACRO_CT_IDENTIFIER:
		case EXPR_CT_IDENT:
			return sema_expr_analyse_ct_identifier(context, to, expr);
		case EXPR_FAILABLE:
			return sema_expr_analyse_failable(context, to, expr);
		case EXPR_POISONED:
			return false;
		case EXPR_LEN:
		case EXPR_DESIGNATED_INITIALIZER:
		case EXPR_SLICE_ASSIGN:
			// Created during semantic analysis
			UNREACHABLE
		case EXPR_MACRO_BLOCK:
		case EXPR_SCOPED_EXPR:
			UNREACHABLE
		case EXPR_TYPEINFO:
			return sema_expr_analyse_typeinfo(context, expr);
		case EXPR_SLICE:
			return sema_expr_analyse_slice(context, expr);
		case EXPR_CATCH:
			return sema_expr_analyse_catch(context, expr);
		case EXPR_TRY:
			return sema_expr_analyse_try(context, expr);
		case EXPR_TYPEOF:
			return sema_expr_analyse_typeof(context, expr);
		case EXPR_ELSE:
			return sema_expr_analyse_else(context, to, expr);
		case EXPR_COMPOUND_LITERAL:
			return sema_expr_analyse_compound_literal(context, to, expr);
		case EXPR_EXPR_BLOCK:
			return sema_expr_analyse_expr_block(context, to, expr);
		case EXPR_GUARD:
			return sema_expr_analyse_guard(context, to, expr);
		case EXPR_CONST:
			expr->constant = true;
			expr->pure = true;
			return true;
		case EXPR_BINARY:
			if (!sema_expr_analyse_binary(context, to, expr)) return false;
			if (expr->expr_kind == EXPR_BINARY)
			{
				expr->failable = expr->binary_expr.left->failable | expr->binary_expr.right->failable;
			}
			return true;
		case EXPR_TERNARY:
			return sema_expr_analyse_ternary(context, to, expr);
		case EXPR_UNARY:
			if (!sema_expr_analyse_unary(context, to, expr)) return false;
			if (expr->expr_kind == EXPR_UNARY) expr->failable = expr->unary_expr.expr->failable;
			return true;
		case EXPR_POST_UNARY:
			if (!sema_expr_analyse_post_unary(context, to, expr)) return false;
			if (expr->expr_kind == EXPR_UNARY) expr->failable = expr->unary_expr.expr->failable;
			return true;
		case EXPR_TYPEID:
			return sema_expr_analyse_type(context, to, expr);
		case EXPR_CONST_IDENTIFIER:
		case EXPR_IDENTIFIER:
		case EXPR_MACRO_IDENTIFIER:
			return sema_expr_analyse_identifier(context, to, expr);
		case EXPR_CALL:
			return sema_expr_analyse_call(context, to, expr);
		case EXPR_SUBSCRIPT:
			return sema_expr_analyse_subscript(context, expr);
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


bool sema_analyse_expr_of_required_type(Context *context, Type *to, Expr *expr, bool may_be_failable)
{
	if (!sema_analyse_expr(context, to, expr)) return false;
	if (expr->failable && !may_be_failable)
	{
		if (!to) to = expr->type;
		SEMA_ERROR(expr, "'%s!' cannot be implicitly cast to '%s'.", type_to_error_string(expr->type), type_to_error_string(to));
		return false;
	}
	return cast_implicit(context, expr, to);
}


static inline bool sema_cast_ct_ident_rvalue(Context *context, Type *to, Expr *expr)
{
	Decl *decl = expr->ct_ident_expr.decl;
	Expr *copy = expr_copy_from_macro(context, decl->var.init_expr);
	if (!sema_analyse_expr(context, to, copy)) return false;
	expr_replace(expr, copy);
	return true;
}

static inline bool sema_cast_rvalue(Context *context, Type *to, Expr *expr)
{
	if (!expr_ok(expr)) return false;
	switch (expr->expr_kind)
	{
		case EXPR_MEMBER_ACCESS:
			if (expr->access_expr.ref->decl_kind == DECL_ENUM_CONSTANT)
			{
				Type *original_type = expr->type;
				expr_replace(expr, expr->access_expr.ref->enum_constant.expr);
				expr->type = original_type;
				return true;
			}
			SEMA_ERROR(expr, "A member must be followed by '.' plus a property like 'sizeof'.");
			return false;
		case EXPR_TYPEINFO:
			SEMA_ERROR(expr, "A type must be followed by either (...) or '.'.");
			return false;
		case EXPR_ENUM_CONSTANT:
			assert(expr->expr_enum->enum_constant.expr->expr_kind == EXPR_CONST);
			expr->const_expr = expr->expr_enum->enum_constant.expr->const_expr;
			expr->expr_kind = EXPR_CONST;
			break;
		case EXPR_MACRO_CT_IDENTIFIER:
			SEMA_ERROR(expr, "Expected compile time macro variable '%s' followed by (...).", expr->ct_macro_ident_expr.identifier);
			return expr_poison(expr);
		case EXPR_MACRO_IDENTIFIER:
			SEMA_ERROR(expr, "Expected macro '%s' followed by (...).", expr->macro_identifier_expr.identifier);
			return expr_poison(expr);
		case EXPR_CT_IDENT:
			if (!sema_cast_ct_ident_rvalue(context, to, expr)) return false;
			break;
		case EXPR_IDENTIFIER:
		case EXPR_CONST_IDENTIFIER:
			if (!sema_cast_ident_rvalue(context, to, expr)) return false;
			break;
		default:
			break;
	}
	return to ? cast(context, expr, to, CAST_TYPE_OPTIONAL_IMPLICIT) : true;
}

bool sema_analyse_expr_value(Context *context, Type *to, Expr *expr)
{
	switch (expr->resolve_status)
	{
		case RESOLVE_NOT_DONE:
			expr->resolve_status = RESOLVE_RUNNING;
			if (!sema_analyse_expr_dispatch(context, to, expr)) return expr_poison(expr);
			expr->resolve_status = RESOLVE_DONE;
			return true;
		case RESOLVE_RUNNING:
			SEMA_ERROR(expr, "Recursive resolution of expression");
			return expr_poison(expr);
		case RESOLVE_DONE:
			return expr_ok(expr);
		default:
			UNREACHABLE
	}
}

bool sema_analyse_expr(Context *context, Type *to, Expr *expr)
{
	return sema_analyse_expr_value(context, to, expr) && sema_cast_rvalue(context, to, expr);
}

