#include "sema_internal.h"

static inline void mangle_type_param(Type *type, bool mangled)
{
	type = type->canonical;
	if (mangled)
	{
		type_mangle_introspect_name_to_buffer(type);
	}
	else
	{
		scratch_buffer_append(type->name);
	}
}
static bool sema_generate_parameter_suffix_to_scratch(Expr **params, bool mangled)
{
	scratch_buffer_clear();
	scratch_buffer_append(mangled ? "$" : "{");
	FOREACH_IDX(j, Expr *, param, params)
	{
		if (j != 0)
		{
			scratch_buffer_append(mangled ? "$" : ", ");
		}
		if (expr_is_const_typeid(param))
		{
			mangle_type_param(param->const_expr.typeid, mangled);
		}
		else
		{
			Type *type = param->type->canonical;
			if (type->type_kind == TYPE_TYPEDEF)
			{
				mangle_type_param(type, mangled);
				scratch_buffer_append(mangled ? "$" : ", ");
				type = type_flatten(type);
			}
			if (type == type_bool)
			{
				if (mangled)
				{
					scratch_buffer_append_char(param->const_expr.b ? 't' : 'f');
				}
				else
				{
					scratch_buffer_append(param->const_expr.b ? "true" : "false");
				}
			}
			else if (type->type_kind == TYPE_ENUM)
			{
				Decl *enumm = param->const_expr.enum_val;
				type_mangle_introspect_name_to_buffer(enumm->type->canonical);
				scratch_buffer_append(mangled ? "_" : ":");
				scratch_buffer_append(enumm->name);
			}
			else if (type->type_kind == TYPE_ANYFAULT)
			{
				Decl *fault = param->const_expr.fault;
				if (fault)
				{
					type_mangle_introspect_name_to_buffer(fault->type->canonical);
					scratch_buffer_append(mangled ? "_" : ":");
					scratch_buffer_append(fault->name);
				}
				else
				{
					scratch_buffer_append("null");
				}
			}
			else
			{
				char *maybe_neg = &scratch_buffer.str[scratch_buffer.len];
				if (type->type_kind == TYPE_I128 || type->type_kind == TYPE_U128)
				{
					char *str = int_to_str(param->const_expr.ixx, 10, false);
					scratch_buffer_append(str);
				}
				else
				{
					if (type_is_signed(type))
					{
						scratch_buffer_append_signed_int((int64_t)param->const_expr.ixx.i.low);
					}
					else
					{
						scratch_buffer_append_unsigned_int(param->const_expr.ixx.i.low);
					}
				}
				if (mangled)
				{
					// Replace - with _
					if (maybe_neg[0] == '-') maybe_neg[0] = '_';
				}
			}
		}
	}
	scratch_buffer_append(mangled ? "$" : "}");
	return true;
}


static bool sema_analyse_generic_module_contracts(SemaContext *c, Module *module, Decl *instance, Expr **contracts, SourceLocId param_loc, SourceLocId invocation_loc)
{
	ASSERT(contracts);
	FOREACH(Expr *, contract, contracts)
	{
		SemaContext temp_context;
		InliningSpan *old_span = c->inlined_at;
		InliningSpan new_span = { .prev = old_span, .loc = invocation_loc };
		SemaContext *new_context = context_transform_for_eval(c, &temp_context, module->units[0]);
		Decl *old_generic_instance = new_context->generic_instance;
		InliningSpan *old_inlined_at = new_context->inlined_at;
		new_context->generic_instance = instance;
		new_context->inlined_at = &new_span;

		FOREACH(Expr *, expr, contract->contract_expr.decl_exprs->expression_list)
		{
			CondResult res = sema_check_comp_time_bool(new_context, expr);
			if (res == COND_MISSING) goto FAIL;
			if (res == COND_TRUE) continue;
			if (contract->contract_expr.comment)
			{
				sema_error_at(c, param_loc,
				              "Parameter(s) would violate constraint: %s.",
				              contract->contract_expr.comment);
			}
			else
			{
				sema_error_at(c, param_loc, "Parameter(s) failed validation: %s",
				              contract->contract_expr.expr_string);
			}
			FAIL:
			new_context->inlined_at = old_inlined_at;
			sema_context_destroy(&temp_context);
			return false;
		}
		new_context->inlined_at = old_inlined_at;
		new_context->generic_instance = old_generic_instance;
		sema_context_destroy(&temp_context);
	}
	return true;
}

Decl *sema_generate_parameterized_identifier(SemaContext *context, Decl *generic, Decl *alias, Expr **params, Decl **param_decls, const char *suffix, const char *csuffix, SourceLocId
                                             invocation_loc, SourceLocId loc)
{
	Module *module = alias->unit->module;
	Decl *instance = NULL;
	unsigned id = generic->generic_decl.id;
	FOREACH(Decl *, g, alias->unit->module->generic_sections)
	{
		if (g->generic_decl.id != id) continue;
		FOREACH (Decl *, candidate, g->generic_decl.instances)
		{
			if (candidate->name == csuffix)
			{
				instance = candidate;
				goto FOUND;
			}
		}
	}
FOUND:;
	if (!instance)
	{
		DEBUG_LOG("Generate generic instance %s", csuffix);
		if (compiler.context.errors_found) return poisoned_decl;
		instance = decl_new(DECL_GENERIC_INSTANCE, csuffix, generic->loc);
		FOREACH_IDX(i, const char *, param_name, generic->generic_decl.parameters)
		{
			Decl *decl;
			Expr *param = params ? params[i] : copy_expr_single(param_decls[i]->var.init_expr);
			ASSERT_SPAN(param, param->expr_kind == EXPR_CONST);
			if (expr_is_const_typeid(param))
			{
				decl = decl_new_var(param_name, param->loc, NULL, VARDECL_PARAM_CT_TYPE);
			}
			else
			{
				ASSERT(param->expr_kind == EXPR_CONST);
				decl = decl_new_var(param_name, param->loc, NULL, VARDECL_CONST);
			}
			decl->var.init_expr = param;
			decl->unit = alias->unit;
			decl->resolve_status = RESOLVE_DONE;
			decl->type = param->type;
			vec_add(instance->instance_decl.params, decl);
		}
		instance->unit = alias->unit;
		Decl **copied = NULL;
		Decl **copied_cond = NULL;
		if (!suffix)
		{
			if (!sema_generate_parameter_suffix_to_scratch(params, false)) return poisoned_decl;
			suffix = scratch_buffer_interned();
		}
		instance->instance_decl.name_suffix = suffix;
		instance->instance_decl.cname_suffix = csuffix;
		instance->instance_decl.id = id;
		FOREACH(Decl *, g, module->generic_sections)
		{
			if (g->generic_decl.id == generic->generic_decl.id)
			{
				vec_add(instance->instance_decl.templates, g);
				Decl **decls = g->generic_decl.decls;
				Decl **cond_decls = g->generic_decl.conditional_decls;
				decls = decls ? copy_decl_list_single_for_generic(decls, instance) : NULL;
				cond_decls = cond_decls ? copy_decl_list_single_for_generic(cond_decls, instance) : NULL;
				FOREACH(Decl *, d, decls) vec_add(copied, d);
				FOREACH(Decl *, d, cond_decls) vec_add(copied_cond, d);
			}
		}
		vec_add(generic->generic_decl.instances, instance);
		AnalysisStage stage = module->stage;
		ASSERT(stage > ANALYSIS_IMPORTS);
		if (compiler.context.errors_found) return poisoned_decl;

		// Check contracts
		FOREACH(Decl *, decl, module->generic_sections)
		{
			if (decl->generic_decl.id == generic->generic_decl.id)
			{
				Expr **requires = decl->generic_decl.requires;
				if (!requires) continue;
				copy_begin();
				Expr **contract = copy_exprlist_macro(requires);
				copy_end();
				SourceLoc param_loc = params ? extend_loc_with_token(sourcelocptr(params[0]->loc), sourcelocptr(VECLAST(params)->loc)) : extend_loc_with_token(sourcelocptr(param_decls[0]->loc), sourcelocptr(VECLAST(param_decls)->loc)); // NOLINT
				if (!sema_analyse_generic_module_contracts(context, module, instance, contract, make_loc(param_loc), invocation_loc))
				{
					decl_poison(instance);
					decl_poison(alias);
					return alias;
				}
			}
		}

		// Add all the normal top level declarations
		FOREACH(Decl *, decl, copied) unit_register_global_decl(decl->unit, decl);
		// Add all the conditional declarations
		FOREACH(Decl *, decl, copied_cond)
		{
			unit_register_optional_global_decl(decl->unit, decl);
			if (decl->decl_kind != DECL_ERASED) vec_add(copied, decl);
		}

		FOREACH(Decl *, decl, copied)
		{
			if (decl->unit->module->stage < ANALYSIS_METHODS_REGISTER) continue;
			if (decl->decl_kind != DECL_FUNC && decl->decl_kind != DECL_MACRO) continue;
			if (!decl->func_decl.type_parent) continue;
			SemaContext gen_context;
			sema_context_init(&gen_context, decl->unit);
			gen_context.generic_instance = instance;
			if (sema_analyse_method_register(&gen_context, decl) && decl->decl_kind != DECL_ERASED)
			{
				if (decl->decl_kind == DECL_MACRO)
				{
					vec_add(decl->unit->macro_methods, decl);
				}
				else
				{
					vec_add(decl->unit->methods, decl);
				}
			}
		}
		FOREACH(Decl *, decl, copied)
		{
			if (decl->unit->module->stage < ANALYSIS_DECLS) continue;
			SemaContext context_gen;
			sema_context_init(&context_gen, decl->unit);
			DynamicScope empty = { .depth = 0 };
			context_gen.active_scope = empty;
			sema_analyse_decl(&context_gen, decl);
			context_gen.generic_instance = instance;
			sema_analyse_inner_func_ptr(&context_gen, decl);
			FOREACH(TypeInfo *, info, decl->unit->check_type_variable_array)
			{
				sema_check_type_variable_array(&context_gen, info);
			}
			sema_context_destroy(&context_gen);
		}
		if (compiler.context.errors_found) return poisoned_decl;
		FOREACH(Decl *, decl, copied)
		{
			if (decl->unit->module->stage < ANALYSIS_FUNCTIONS) continue;
			SemaContext context_gen;
			switch (decl->decl_kind)
			{
				case DECL_FUNC:
					sema_context_init(&context_gen, decl->unit);
					analyse_func_body(&context_gen, decl);
					sema_context_destroy(&context_gen);
					break;
				default:
					break;
			}
		}
		if (compiler.context.errors_found) return poisoned_decl;
		FOREACH(Decl *, decl, copied)
		{
			if (decl->unit->module->stage < ANALYSIS_INTERFACE) continue;
			SemaContext context_gen;
			switch (decl->decl_kind)
			{
				case DECL_TYPEDEF:
				case DECL_STRUCT:
				case DECL_UNION:
				case DECL_ENUM:
				case DECL_BITSTRUCT:
					break;
				default:
					continue;
			}
			if (decl->interfaces)
			{
				sema_context_init(&context_gen, decl->unit);
				sema_check_interfaces(&context_gen, decl);
				sema_context_destroy(&context_gen);
			}
		}
		if (compiler.context.errors_found) return poisoned_decl;
	}
	Decl *symbol = sema_find_generic_instance(context, module, generic, instance, alias->name);
	if (!symbol)
	{
		sema_error_at(context, loc, "The generic '%s' does not exist for this parameterization.", alias->name);
		return poisoned_decl;
	}

	CompilationUnit *unit = symbol->unit;
	if (unit->module->stage < ANALYSIS_POST_REGISTER)
	{
		vec_add(unit->global_decls, symbol);
	}
	else
	{
		if (!sema_analyse_decl(context, symbol)) return poisoned_decl;
	}
	unit_register_external_symbol(context, symbol);
	return symbol;

}

Decl *sema_analyse_parameterized_identifier(SemaContext *context, Path *decl_path, const char *name, SourceLocId loc,
                                            Expr **params, SourceLocId invocation_loc)
{
	Decl *alias = sema_resolve_parameterized_symbol(context, name, decl_path, loc);
	if (!alias) return poisoned_decl;
	ASSERT_AT(invocation_loc, alias->is_template && alias->generic_id);
	Decl *generic = declptr(alias->generic_id);
	unsigned parameter_count = vec_size(generic->generic_decl.parameters);
	ASSERT(parameter_count > 0);
	unsigned count = vec_size(params);
	if (parameter_count != count)
	{
		if (!count)
		{
			sema_error_at(context, invocation_loc,
			              "'%s' requires generic arguments inside the '{}', did you forget them?", name, (int)parameter_count);

		}
		else
		{
			// 'Foo' expects 2 generic arguments, but you supplied 1, did you make a mistake?
			SourceLoc l = extend_loc_with_token(sourcelocptr(params[0]->loc), sourcelocptr(vectail(params)->loc));
			sema_error_at(context, make_loc(l),
			              "'%s' expects %d %s, but you supplied %d, did you make a mistake?",
			              name,
			              parameter_count,
			              parameter_count == 1 ? "argument" : "arguments",
			              vec_size(params));
		}
		return poisoned_decl;
	}
	// First resolve
	FOREACH_IDX(i, Expr *, param, params)
	{
		if (!sema_analyse_expr_rvalue(context, param)) return poisoned_decl;
		const char *param_name = generic->generic_decl.parameters[i];
		bool is_type = str_is_type(param_name);
		if (expr_is_const_typeid(param))
		{
			if (!is_type)
			{
				SEMA_ERROR(param, "Expected a value, not a type, for parameter '%s'.", param_name);
				return poisoned_decl;
			}
			Type *type = param->const_expr.typeid;
			if (type_is_optional(type))
			{
				RETURN_VAL_SEMA_ERROR(poisoned_decl, param, "The generic type can never be an optional, please use only non-optional types.");
			}
			if (type_is_func_ptr(type))
			{
				if (!sema_resolve_type_decl(context, type->pointer)) return poisoned_decl;
			}
		}
		else
		{
			if (is_type)
			{
				RETURN_VAL_SEMA_ERROR(poisoned_decl, param, "Expected a type, not a value, for parameter '%s'.", param_name);
			}
			if (!sema_analyse_ct_expr(context, param)) return poisoned_decl;
			Type *type = param->type->canonical;
			if (type->type_kind == TYPE_TYPEDEF || type->type_kind == TYPE_CONSTDEF) type = type_flatten(type);
			if (IS_OPTIONAL(param))
			{
				RETURN_VAL_SEMA_ERROR(poisoned_decl, param, "The parameter may never be an optional value.");
			}

			bool is_enum_or_fault = type_kind_is_enum_or_fault(type->type_kind);
			if (!type_is_integer_or_bool_kind(type) && !is_enum_or_fault)
			{
				RETURN_VAL_SEMA_ERROR(poisoned_decl, param, "Only integer, bool, fault and enum values may be generic arguments.");
			}
			ASSERT(expr_is_const(param));
		}
	}
	if (!sema_generate_parameter_suffix_to_scratch(params, true)) return poisoned_decl;
	const char *suffix = scratch_buffer_interned();
	return sema_generate_parameterized_identifier(context, generic, alias, params, NULL, NULL, suffix, invocation_loc, loc);
}
