#include "c_codegen_internal.h"

static void c_emit_const_expr(GenContext *c, CValue *value, Expr *expr)
{
	Type *t = (expr->type && is_valid_type_ptr(expr->type)) ? c_safe_type_lower(expr->type) : type_void;
	c_emit_type_forward_decl(c, t);
	const char *tname = c_type_name(c, t);
	switch (expr->const_expr.const_kind)
	{
		case CONST_FLOAT:
		{
			if (t->type_kind == TYPE_VOID)
			{
				t = type_double;
			}
			PRINTF("%s ___var_%d = ", tname, c_emit_temp_var(c, value, t));
			c_emit_const_float_literal(c, expr->const_expr.fxx.f);
			PRINT(";\n");
			return;
		}
		case CONST_INTEGER:
		{
			if (t->type_kind == TYPE_VOID)
			{
				t = type_int;
			}
			PRINTF("%s ___var_%d = ", tname, c_emit_temp_var(c, value, t));
			c_emit_const_int_literal(c, &expr->const_expr, t);
			PRINT(";\n");
			return;
		}
		case CONST_BOOL:
			if (t->type_kind == TYPE_VOID)
			{
				t = type_bool;
			}
			PRINTF("bool ___var_%d = %s;\n", c_emit_temp_var(c, value, t), expr->const_expr.b ? "true" : "false");
			return;
		case CONST_POINTER:
			if (t->type_kind == TYPE_VOID)
			{
				t = type_voidptr;
			}
			if (expr->const_expr.ptr == 0)
			{
				c_emit_var_zero_init(c, c_emit_temp_var(c, value, t), t);
			}
			else
			{
				if (c_type_is_aggregate(t))
				{
					PRINTF("%s ___var_%d = {0};\n", tname, c_emit_temp_var(c, value, t));
				}
				else
				{
					PRINTF("%s ___var_%d = (%s)(uintptr_t)0x%" PRIx64 "ULL;\n", tname, c_emit_temp_var(c, value, t), tname, expr->const_expr.ptr);
				}
			}
			return;
		case CONST_BYTES:
		case CONST_STRING:
		{
			if (t->type_kind == TYPE_VOID)
			{
				t = type_chars;
			}
			int temp = c_emit_temp_var(c, value, t);
			if (expr->const_expr.bytes.len > 4000)
			{
				int bytes_id = c_create_variable(c);
				PRINTF("static const uint8_t ___bytes_%d[%llu] = { ", bytes_id, (unsigned long long)expr->const_expr.bytes.len + 1);
				for (ArrayIndex bi = 0; bi < expr->const_expr.bytes.len; bi++)
				{
					if (bi > 0)
					{
						PRINT(", ");
					}
					PRINTF("0x%02X", (unsigned char)expr->const_expr.bytes.ptr[bi]);
				}
				PRINT(", 0 };\n");
				if (t->type_kind == TYPE_SLICE)
				{
					PRINTF("%s ___var_%d = (%s){ .ptr = (void*)___bytes_%d, .len = %llu };\n",
					       tname, temp, tname, bytes_id, (unsigned long long)expr->const_expr.bytes.len);
				}
				else if (t->type_kind == TYPE_ARRAY)
				{
					PRINTF("%s ___var_%d;\n", tname, temp);
					PRINTF("__c3_memcpy(___var_%d.ptr, ___bytes_%d, %llu);\n",
					       temp, bytes_id, (unsigned long long)expr->const_expr.bytes.len);
				}
				else
				{
					PRINTF("%s ___var_%d = (void*)___bytes_%d;\n", tname, temp, bytes_id);
				}
				return;
			}
			if (t->type_kind == TYPE_SLICE)
			{
				PRINTF("%s ___var_%d = (%s){ .ptr = (void*)", tname, temp, tname);
				c_emit_string_literal(c, expr->const_expr.bytes.ptr, expr->const_expr.bytes.len);
				PRINTF(", .len = %llu };\n", (unsigned long long)expr->const_expr.bytes.len);
			}
			else if (t->type_kind == TYPE_ARRAY)
			{
				PRINTF("%s ___var_%d;\n", tname, temp);
				PRINTF("__c3_memcpy(___var_%d.ptr, ", temp);
				c_emit_string_literal(c, expr->const_expr.bytes.ptr, expr->const_expr.bytes.len);
				PRINTF(", %llu);\n", (unsigned long long)expr->const_expr.bytes.len);
			}
			else
			{
				PRINTF("%s ___var_%d = (void*)", tname, temp);
				c_emit_string_literal(c, expr->const_expr.bytes.ptr, expr->const_expr.bytes.len);
				PRINT(";\n");
			}
			return;
		}
		case CONST_REF:
		{
			Decl *d = expr->const_expr.global_ref;
			d       = c_decl_unwrap(d);
			if (d && d->decl_kind == DECL_FUNC)
			{
				PRINTF("%s ___var_%d = (%s)&%s;\n", tname, c_emit_temp_var(c, value, t), tname, c_get_decl_name(d));
			}
			else if (c_is_file_global(d))
			{
				c_emit_global_decl(c, d);
				PRINTF("%s ___var_%d = (%s)&%s;\n", tname, c_emit_temp_var(c, value, t), tname, c_get_decl_name(d));
			}
			else
			{
				VariableId vid = c_get_or_create_decl_var(c, d);
				PRINTF("%s ___var_%d = (%s)&___var_%d;\n", tname, c_emit_temp_var(c, value, t), tname, vid);
			}
			return;
		}
		case CONST_ENUM:
			if (t->type_kind == TYPE_VOID)
			{
				t = type_int;
			}
			PRINTF("%s ___var_%d = %d;\n", tname, c_emit_temp_var(c, value, t), expr->const_expr.enum_val->enum_constant.inner_ordinal);
			return;
		case CONST_FAULT:
			if (t->type_kind == TYPE_VOID)
			{
				t = type_fault;
			}
			if (expr->const_expr.fault)
			{
				const char *fsym = c_fault_symbol_name(expr->const_expr.fault);
				PRINTF("%s ___var_%d = (c3fault_t)%s;\n", tname, c_emit_temp_var(c, value, t), fsym);
			}
			else
			{
				c_emit_var_zero_init(c, c_emit_temp_var(c, value, t), t);
			}
			return;
		case CONST_INITIALIZER:
		case CONST_SLICE:
		{
			if (t->type_kind == TYPE_VOID)
			{
				t = (expr->const_expr.const_kind == CONST_SLICE) ? type_chars : type_int;
			}
			int temp = c_emit_temp_var(c, value, t);
			PRINTF("%s ___var_%d = ", tname, temp);
			c_emit_const_init_expr(c, expr, t);
			PRINT(";\n");
			return;
		}
		case CONST_TYPEID:
		{
			Type *tval = expr->const_expr.typeid;
			if (!tval)
			{
				tval = type_void;
			}
			int temp = c_emit_temp_var(c, value, type_typeid);
			PRINTF("c3typeid_t ___var_%d = (c3typeid_t)&%s;\n", temp, c_typeid_name(tval));
			return;
		}
		default:
			if (t->type_kind == TYPE_VOID)
			{
				t = type_int;
			}
			c_emit_var_zero_init(c, c_emit_temp_var(c, value, t), t);
			return;
	}
}

static void c_emit_cond_expr(GenContext *c, CValue *value, Expr *expr)
{
	Expr **list = expr->cond_expr;
	int size    = vec_size(list);
	assert(size);
	for (int i = 0; i < size - 1; i++)
	{
		c_emit_ignored_expr(c, list[i]);
	}
	c_emit_expr(c, value, list[size - 1]);
}

static void c_emit_expression_list_expr(GenContext *c, CValue *value, Expr *expr)
{
	Expr **list = expr->expression_list;
	int size    = vec_size(list);
	assert(size);
	for (int i = 0; i < size - 1; i++)
	{
		c_emit_ignored_expr(c, list[i]);
	}
	c_emit_expr(c, value, list[size - 1]);
}

static void c_emit_identifier_expr(GenContext *c, CValue *value, Expr *expr)
{
	if (!expr || !expr->ident_expr)
	{
		*value = (CValue){.var = 0, .type = type_void, .kind = CV_VALUE};
		return;
	}
	Decl *decl = c_decl_unwrap(expr->ident_expr);
	if (!decl)
	{
		*value = (CValue){.var = 0, .type = type_void, .kind = CV_VALUE};
		return;
	}

	if (decl->decl_kind == DECL_FUNC)
	{
		Type *ptr_t = type_is_func_ptr(decl->type) ? decl->type : type_get_func_ptr(decl->type);
		int temp    = c_emit_temp_var(c, value, ptr_t);
		PRINTF("%s ___var_%d = (void*)&%s;\n", c_type_name(c, ptr_t), temp, c_get_decl_name(decl));
		return;
	}
	if (decl->decl_kind == DECL_ENUM_CONSTANT)
	{
		Type *t  = c_decl_type(decl);
		int temp = c_emit_temp_var(c, value, t);
		PRINTF("%s ___var_%d = %d;\n", c_type_name(c, t), temp, decl->enum_constant.inner_ordinal);
		return;
	}
	if (decl->decl_kind == DECL_FAULT)
	{
		int temp         = c_emit_temp_var(c, value, type_fault);
		const char *fsym = c_fault_symbol_name(decl);
		PRINTF("c3fault_t ___var_%d = (c3fault_t)%s;\n", temp, fsym);
		return;
	}
	if (decl->decl_kind == DECL_VAR && (decl->var.kind == VARDECL_PARAM_EXPR || decl->var.kind == VARDECL_PARAM_CT || decl->var.kind == VARDECL_LOCAL_CT))
	{
		if (decl->var.init_expr)
		{
			c_emit_expr(c, value, decl->var.init_expr);
			return;
		}
	}

	Type *t = c_decl_type(decl);
	if (t->type_kind == TYPE_VOID)
	{
		*value = (CValue){.var = 0, .type = type_void, .kind = CV_VALUE};
		return;
	}

	int temp          = c_emit_temp_var(c, value, t);
	const char *tname = c_type_name(c, t);
	Decl *raw_ident   = expr->ident_expr ? decl_raw(expr->ident_expr) : NULL;
	if (c_is_file_global(decl))
	{
		c_emit_global_decl(c, decl);
		const char *gname = c_get_decl_name(decl);
		if (IS_OPTIONAL(decl) && (!raw_ident || raw_ident->var.kind != VARDECL_UNWRAPPED))
		{
			int fault_temp = c_emit_temp_var(c, NULL, type_fault);
			PRINTF("c3fault_t ___var_%d = %s__f;\n", fault_temp, gname);
			PRINTF("__c3_current_fault = ___var_%d;\n", fault_temp);
			value->optional = fault_temp;
		}
		if (t->type_kind == TYPE_ARRAY || t->type_kind == TYPE_VECTOR || t->type_kind == TYPE_SIMD_VECTOR)
		{
			PRINTF("%s ___var_%d;\n", tname, temp);
			PRINTF("__c3_memcpy(&___var_%d, &%s, sizeof(%s));\n", temp, gname, gname);
		}
		else if (type_is_pointer(t) || t->type_kind == TYPE_FUNC_PTR)
		{
			PRINTF("%s ___var_%d = (%s)%s;\n", tname, temp, tname, gname);
		}
		else
		{
			PRINTF("%s ___var_%d = %s;\n", tname, temp, gname);
		}
	}
	else
	{
		VariableId vid  = c_get_or_create_decl_var(c, decl);
		VariableId fvid = 0;
		if (raw_ident && raw_ident->decl_kind == DECL_VAR && raw_ident->var.kind != VARDECL_UNWRAPPED)
		{
			fvid = c_get_decl_fault_var(c, decl);
		}
		if (fvid != 0)
		{
			value->optional = fvid;
			PRINTF("__c3_current_fault = ___var_%d;\n", fvid);
		}
		if (t->type_kind == TYPE_ARRAY || t->type_kind == TYPE_VECTOR || t->type_kind == TYPE_SIMD_VECTOR)
		{
			PRINTF("%s ___var_%d;\n", tname, temp);
			PRINTF("__c3_memcpy(&___var_%d, &___var_%d, sizeof(___var_%d));\n", temp, vid, vid);
		}
		else if (type_is_pointer(t) || t->type_kind == TYPE_FUNC_PTR)
		{
			// Explicit cast satisfies Clang's strict -Wincompatible-pointer-types
			PRINTF("%s ___var_%d = (%s)___var_%d;\n", tname, temp, tname, vid);
		}
		else
		{
			PRINTF("%s ___var_%d = ___var_%d;\n", tname, temp, vid);
		}
	}
}

static void c_emit_macro_body_expansion(GenContext *c, CValue *value, Expr *body_expr)
{
	Decl **declarations = body_expr->body_expansion_expr.declarations;
	Expr **values       = body_expr->body_expansion_expr.values;
	int count           = vec_size(declarations);
	for (int i = 0; i < count; i++)
	{
		Decl *decl     = declarations[i];
		Expr *val_expr = (i < vec_size(values)) ? values[i] : NULL;
		if (!decl || !val_expr)
		{
			continue;
		}
		CValue v = {0};
		c_emit_expr(c, &v, val_expr);
		c_value_rvalue(c, &v);
		if ((!decl->type || decl->type->type_kind == TYPE_VOID) && v.type && v.type->type_kind != TYPE_VOID)
		{
			decl->type = v.type;
		}
		VariableId vid = c_get_or_create_decl_var(c, decl);
		Type *dt       = c_decl_type(decl);
		c_emit_assign_var(c, vid, dt, &v);
		if (IS_OPTIONAL(decl))
		{
			c_emit_assign_decl_fault(c, decl, &v, val_expr);
		}
	}
	if (body_expr->body_expansion_expr.first_stmt)
	{
		c_emit_stmt(c, astptr(body_expr->body_expansion_expr.first_stmt));
	}
	if (value)
	{
		*value = (CValue){.var = 0, .type = type_void, .kind = CV_VALUE};
	}
}

static void c_emit_call_expr(GenContext *c, CValue *value, Expr *expr)
{
	ExprCall *call = &expr->call_expr;
	if (call->is_builtin)
	{
		c_emit_builtin_call(c, value, expr);
		return;
	}

	Decl *function_decl = call->is_func_ref ? declptrzero(call->func_ref) : NULL;
	if (!function_decl && call->function)
	{
		Expr *fn_expr = exprptrzero(call->function);
		while (fn_expr)
		{
			if (fn_expr->expr_kind == EXPR_IDENTIFIER && fn_expr->ident_expr)
			{
				function_decl = decl_flatten(fn_expr->ident_expr);
				break;
			}
			else if (fn_expr->expr_kind == EXPR_ACCESS_RESOLVED && fn_expr->access_resolved_expr.ref)
			{
				function_decl = decl_flatten(fn_expr->access_resolved_expr.ref);
				break;
			}
			else if (fn_expr->expr_kind == EXPR_DECL && fn_expr->decl_expr)
			{
				function_decl = decl_flatten(fn_expr->decl_expr);
				break;
			}
			else if (fn_expr->expr_kind == EXPR_CONST && fn_expr->const_expr.const_kind == CONST_REF && fn_expr->const_expr.global_ref)
			{
				function_decl = decl_flatten(fn_expr->const_expr.global_ref);
				break;
			}
			else if (fn_expr->expr_kind == EXPR_UNARY && (fn_expr->unary_expr.operator == UNARYOP_ADDR || fn_expr->unary_expr.operator == UNARYOP_TADDR))
			{
				fn_expr = fn_expr->unary_expr.expr;
			}
			else if (fn_expr->expr_kind == EXPR_RECAST || fn_expr->expr_kind == EXPR_ADDR_CONVERSION || fn_expr->expr_kind == EXPR_RVALUE || fn_expr->expr_kind == EXPR_MAYBE_DEREF)
			{
				fn_expr = fn_expr->inner_expr;
			}
			else
			{
				break;
			}
		}
	}
	if (function_decl)
	{
		function_decl = decl_flatten(function_decl);
		if (function_decl->decl_kind != DECL_FUNC)
		{
			function_decl = NULL;
		}
	}
	Signature *sig = function_decl ? &function_decl->func_decl.signature : NULL;
	if (!sig && call->function && exprptr(call->function)->type)
	{
		Type *ft = c_safe_type_lower(exprptr(call->function)->type);
		if (ft)
		{
			if (ft->type_kind == TYPE_FUNC_PTR && ft->pointer && ft->pointer->type_kind == TYPE_FUNC_RAW)
			{
				sig = ft->pointer->function.signature;
			}
			else if (ft->type_kind == TYPE_POINTER && ft->pointer && ft->pointer->type_kind == TYPE_FUNC_RAW)
			{
				sig = ft->pointer->function.signature;
			}
		}
	}

	Expr **args      = call->arguments;
	Expr **varargs   = call->varargs;
	int num_args     = vec_size(args);
	int num_varargs  = vec_size(varargs);
	bool is_variadic = sig && (sig->variadic == VARIADIC_TYPED || sig->variadic == VARIADIC_ANY);
	int vararg_idx   = sig ? sig->vararg_index : (is_variadic ? num_args - 1 : -1);
	int param_count  = sig ? vec_size(sig->params) : num_args;

	int total_args = (param_count > 0 ? param_count : num_args) + (num_varargs > 0 ? num_varargs : 0);
	if (total_args <= 0)
	{
		total_args = 1;
	}
	CValue *c_args = (CValue *)malloc(sizeof(CValue) * total_args);
	int arg_idx    = 0;

	for (int i = 0; i < param_count; i++)
	{
		if (is_variadic && i == vararg_idx)
		{
			if (call->va_is_splat && call->vasplat)
			{
				c_emit_expr(c, &c_args[arg_idx], call->vasplat);
				c_value_rvalue(c, &c_args[arg_idx]);
				arg_idx++;
			}
			else
			{
				Decl *vararg_param = (sig && i < vec_size(sig->params)) ? sig->params[i] : NULL;
				Type *slice_type   = (vararg_param && is_valid_type_ptr(vararg_param->type)) ? c_safe_type_lower(vararg_param->type) : type_get_slice(type_any);
				Type *elem_type    = (slice_type && slice_type->type_kind == TYPE_SLICE && is_valid_type_ptr(slice_type->array.base)) ? c_safe_type_lower(slice_type->array.base) : type_any;
				c_emit_type_forward_decl(c, slice_type);
				const char *slice_tname = c_type_name(c, slice_type);
				const char *elem_tname  = c_type_name(c, elem_type);

				if (num_varargs == 0)
				{
					int slice_var = c_emit_temp_var(c, &c_args[arg_idx++], slice_type);
					PRINTF("___var_%d = (%s){ .ptr = NULL, .len = 0 };\n", slice_var, slice_tname);
				}
				else
				{
					CValue *vvals = (CValue *)malloc(sizeof(CValue) * num_varargs);
					for (int j = 0; j < num_varargs; j++)
					{
						vvals[j] = (CValue){0};
						c_emit_expr(c, &vvals[j], varargs[j]);
						c_value_rvalue(c, &vvals[j]);
					}
					int arr_var = c_create_variable(c);
					PRINTF("%s ___var_%d[%d];\n", elem_tname, arr_var, num_varargs);
					bool elem_is_any = (elem_type->type_kind == TYPE_ANY || elem_type->type_kind == TYPE_INTERFACE || strcmp(elem_tname, "__c3_any__") == 0);
					for (int j = 0; j < num_varargs; j++)
					{
						if (elem_is_any)
						{
							char target_slot[64];
							snprintf(target_slot, sizeof(target_slot), "___var_%d[%d]", arr_var, j);
							c_emit_assign_to_any(c, target_slot, &vvals[j]);
						}
						else
						{
							Type *orig_vt = (varargs[j] && varargs[j]->type) ? varargs[j]->type : (vvals[j].type ? vvals[j].type : type_void);
							Type *vtype   = c_safe_type_lower(orig_vt);
							if (c_type_is_aggregate(elem_type))
							{
								if (elem_type == vtype || strcmp(elem_tname, c_type_name(c, vtype)) == 0)
								{
									PRINTF("___var_%d[%d] = ___var_%d;\n", arr_var, j, vvals[j].var);
								}
								else
								{
									PRINTF("__c3_memcpy(&___var_%d[%d], &___var_%d, sizeof(%s));\n", arr_var, j, vvals[j].var, elem_tname);
								}
							}
							else
							{
								if (elem_type == vtype)
								{
									PRINTF("___var_%d[%d] = ___var_%d;\n", arr_var, j, vvals[j].var);
								}
								else
								{
									PRINTF("___var_%d[%d] = (%s)___var_%d;\n", arr_var, j, elem_tname, vvals[j].var);
								}
							}
						}
					}
					int slice_var = c_emit_temp_var(c, &c_args[arg_idx++], slice_type);
					PRINTF("___var_%d = (%s){ .ptr = ___var_%d, .len = %d };\n", slice_var, slice_tname, arr_var, num_varargs);
					free(vvals);
				}
			}
		}
		else
		{
			if (i < num_args && args[i])
			{
				Decl *param_decl      = (sig && i < vec_size(sig->params)) ? sig->params[i] : NULL;
				Type *exp_type        = param_decl ? c_decl_type(param_decl) : NULL;
				bool is_struct_to_ptr = exp_type && exp_type->type_kind == TYPE_POINTER && exp_type->pointer &&
				                        args[i]->type && !type_is_pointer(args[i]->type) &&
				                        (args[i]->type->type_kind == TYPE_STRUCT || args[i]->type->type_kind == TYPE_UNION) &&
				                        c_safe_type_lower(exp_type->pointer) == c_safe_type_lower(args[i]->type);
				if (is_struct_to_ptr)
				{
					c_emit_lvalue_addr(c, args[i], &c_args[arg_idx++], exp_type);
				}
				else
				{
					c_emit_expr(c, &c_args[arg_idx], args[i]);
					bool exp_is_ptr = exp_type ? type_is_pointer(exp_type) : false;
					bool exp_is_any = exp_type ? (exp_type->type_kind == TYPE_ANY || exp_type->type_kind == TYPE_INTERFACE) : false;
					if (!exp_is_ptr && !exp_is_any)
					{
						c_value_rvalue(c, &c_args[arg_idx]);
					}
					arg_idx++;
				}
			}
			else
			{
				Type *pt          = (sig && i < vec_size(sig->params) && sig->params[i]) ? c_decl_type(sig->params[i]) : type_void;
				c_args[arg_idx++] = (CValue){.var = 0, .type = pt, .kind = CV_VALUE};
			}
		}
	}

	if (sig && sig->variadic == VARIADIC_RAW && num_varargs > 0)
	{
		for (int j = 0; j < num_varargs; j++)
		{
			if (varargs[j])
			{
				c_emit_expr(c, &c_args[arg_idx], varargs[j]);
				c_value_rvalue(c, &c_args[arg_idx]);
				arg_idx++;
			}
		}
	}

	Type *fn_rtype = NULL;
	if (sig)
	{
		Type *raw_rt = typeget(sig->rtype);
		fn_rtype     = (raw_rt && is_valid_type_ptr(raw_rt)) ? c_safe_type_lower(raw_rt) : type_void;
	}
	else if (function_decl)
	{
		Type *raw_rt = typeget(function_decl->func_decl.signature.rtype);
		fn_rtype     = (raw_rt && is_valid_type_ptr(raw_rt)) ? c_safe_type_lower(raw_rt) : type_void;
	}

	Type *return_type = (expr->type && is_valid_type_ptr(expr->type)) ? c_safe_type_lower(expr->type) : type_void;
	bool has_return   = (return_type->type_kind != TYPE_VOID && !call->no_return);
	if (fn_rtype && fn_rtype->type_kind == TYPE_VOID)
	{
		has_return = false;
	}
	if (return_type && c_type_is_resolved(return_type) && type_size(return_type) == 0)
	{
		has_return = false;
		if (value)
		{
			c_emit_type_forward_decl(c, return_type);
			PRINTF("%s ___var_%d = {0};\n", c_type_name(c, return_type), c_emit_temp_var(c, value, return_type));
		}
	}

	if (call->is_dynamic_dispatch && call->is_func_ref)
	{
		Decl *dyn_fn = declptrzero(call->func_ref);
		if (dyn_fn)
		{
			const char *disp_name = c_intern(str_printf("__c3_dyn_%s", c_get_decl_name(dyn_fn)));
			if (has_return)
			{
				c_emit_type_forward_decl(c, return_type);
				int ret_var = c_emit_temp_var(c, value, return_type);
				if (call->has_optional_arg)
				{
					PRINTF("%s ___var_%d = {0};\n", c_type_name(c, return_type), ret_var);
					PRINTF("if (__c3_current_fault == NULL) {\n\t___var_%d = %s(", ret_var, disp_name);
				}
				else
				{
					PRINTF("%s ___var_%d = %s(", c_type_name(c, return_type), ret_var, disp_name);
				}
			}
			else
			{
				if (value && value->var == 0)
				{
					value->var  = 0;
					value->type = type_void;
					value->kind = CV_VALUE;
				}
				if (call->has_optional_arg)
				{
					PRINTF("if (__c3_current_fault == NULL) {\n\t%s(", disp_name);
				}
				else
				{
					PRINTF("%s(", disp_name);
				}
			}
			for (int i = 0; i < arg_idx; i++)
			{
				if (i != 0)
				{
					PRINT(", ");
				}
				PRINTF("___var_%d", c_args[i].var);
			}
			if (call->has_optional_arg)
			{
				PRINT(");\n}\n");
			}
			else
			{
				PRINT(");\n");
			}
			if (call->no_return && !call->has_optional_arg)
			{
				c->current_block_live = false;
			}
			if (value && expr->type && type_is_optional(expr->type))
			{
				int fault_temp = c_emit_temp_var(c, NULL, type_fault);
				PRINTF("c3fault_t ___var_%d = __c3_current_fault;\n", fault_temp);
				value->optional = fault_temp;
			}
			free(c_args);
			return;
		}
	}

	if (!function_decl)
	{
		CValue func_val = {0};
		if (call->function)
		{
			c_emit_expr(c, &func_val, exprptr(call->function));
			c_value_rvalue(c, &func_val);
		}
		else
		{
			func_val = (CValue){.var = 0, .type = type_voidptr, .kind = CV_VALUE};
		}

		if (safe_mode_enabled())
		{
			PRINTF("if (___var_%d == NULL) { __c3_abort(); }\n", func_val.var);
		}

		if (func_val.var == 0)
		{
			PRINT("__c3_abort();\n");
			c->current_block_live = false;
			if (has_return)
			{
				c_emit_type_forward_decl(c, return_type);
				PRINTF("%s ___var_%d = {0};\n", c_type_name(c, return_type), c_emit_temp_var(c, value, return_type));
			}
			else
			{
				if (value && value->var == 0)
				{
					value->var  = 0;
					value->type = type_void;
					value->kind = CV_VALUE;
				}
			}
			free(c_args);
			return;
		}

		const char *fn_cast = NULL;
		char fn_cast_buf[512];
		if (func_val.type && type_is_func_ptr(func_val.type))
		{
			c_emit_type_forward_decl(c, func_val.type);
			fn_cast = c_type_name(c, func_val.type);
		}
		else
		{
			const char *ret_tname = c_type_name(c, return_type);
			snprintf(fn_cast_buf, sizeof(fn_cast_buf), "%s (*)(", ret_tname);
			if (sig && sig->params && vec_size(sig->params) > 0)
			{
				int p_emitted = 0;
				FOREACH(Decl *, p, sig->params)
				{
					Type *ptype = c_decl_type(p);
					if (ptype && is_valid_type_ptr(ptype) && c_type_is_resolved(ptype) && type_size(ptype) == 0) continue;
					if (p_emitted++ > 0) strncat(fn_cast_buf, ", ", sizeof(fn_cast_buf) - strlen(fn_cast_buf) - 1);
					strncat(fn_cast_buf, c_type_name(c, ptype), sizeof(fn_cast_buf) - strlen(fn_cast_buf) - 1);
				}
				if (sig->variadic == VARIADIC_RAW)
				{
					if (p_emitted > 0) strncat(fn_cast_buf, ", ", sizeof(fn_cast_buf) - strlen(fn_cast_buf) - 1);
					strncat(fn_cast_buf, "...", sizeof(fn_cast_buf) - strlen(fn_cast_buf) - 1);
				}
				if (p_emitted == 0) strncat(fn_cast_buf, "void", sizeof(fn_cast_buf) - strlen(fn_cast_buf) - 1);
			}
			else if (arg_idx > 0)
			{
				for (int i = 0; i < arg_idx; i++)
				{
					if (i > 0) strncat(fn_cast_buf, ", ", sizeof(fn_cast_buf) - strlen(fn_cast_buf) - 1);
					Type *at = (c_args[i].type && is_valid_type_ptr(c_args[i].type)) ? c_safe_type_lower(c_args[i].type) : type_voidptr;
					strncat(fn_cast_buf, c_type_name(c, at), sizeof(fn_cast_buf) - strlen(fn_cast_buf) - 1);
				}
			}
			else
			{
				strncat(fn_cast_buf, "void", sizeof(fn_cast_buf) - strlen(fn_cast_buf) - 1);
			}
			strncat(fn_cast_buf, ")", sizeof(fn_cast_buf) - strlen(fn_cast_buf) - 1);
			fn_cast = fn_cast_buf;
		}

		if (has_return)
		{
			c_emit_type_forward_decl(c, return_type);
			int ret_var = c_emit_temp_var(c, value, return_type);
			if (call->has_optional_arg)
			{
				PRINTF("%s ___var_%d = {0};\n", c_type_name(c, return_type), ret_var);
				PRINTF("if (__c3_current_fault == NULL) {\n\t___var_%d = ((%s)___var_%d)(", ret_var, fn_cast, func_val.var);
			}
			else
			{
				PRINTF("%s ___var_%d = ((%s)___var_%d)(", c_type_name(c, return_type), ret_var, fn_cast, func_val.var);
			}
		}
		else
		{
			if (value && value->var == 0)
			{
				value->var  = 0;
				value->type = type_void;
				value->kind = CV_VALUE;
			}
			if (call->has_optional_arg)
			{
				PRINTF("if (__c3_current_fault == NULL) {\n\t((%s)___var_%d)(", fn_cast, func_val.var);
			}
			else
			{
				PRINTF("((%s)___var_%d)(", fn_cast, func_val.var);
			}
		}
	}
	else
	{
		c_emit_function_decl(c, function_decl, false);
		const char *fn_name = c_get_decl_name(function_decl);
		if (has_return)
		{
			c_emit_type_forward_decl(c, return_type);
			int ret_var = c_emit_temp_var(c, value, return_type);
			if (call->has_optional_arg)
			{
				PRINTF("%s ___var_%d = {0};\n", c_type_name(c, return_type), ret_var);
				PRINTF("if (__c3_current_fault == NULL) {\n\t___var_%d = %s(", ret_var, fn_name);
			}
			else
			{
				PRINTF("%s ___var_%d = %s(", c_type_name(c, return_type), ret_var, fn_name);
			}
		}
		else
		{
			if (value && value->var == 0)
			{
				value->var  = 0;
				value->type = type_void;
				value->kind = CV_VALUE;
			}
			if (call->has_optional_arg)
			{
				PRINTF("if (__c3_current_fault == NULL) {\n\t%s(", fn_name);
			}
			else
			{
				PRINTF("%s(", fn_name);
			}
		}
	}

	int emitted_call_args = 0;
	for (int i = 0; i < arg_idx; i++)
	{
		Type *expected_type = NULL;
		if (sig && i < vec_size(sig->params) && sig->params[i])
		{
			expected_type = c_decl_type(sig->params[i]);
		}

		if (expected_type && is_valid_type_ptr(expected_type) && c_type_is_resolved(expected_type) && type_size(expected_type) == 0)
		{
			continue;
		}

		if (emitted_call_args != 0)
		{
			PRINT(", ");
		}
		emitted_call_args++;

		Type *arg_type = (c_args[i].type && is_valid_type_ptr(c_args[i].type)) ? c_safe_type_lower(c_args[i].type) : NULL;

		if (c_args[i].var == 0)
		{
			if (expected_type)
			{
				PRINTF("(%s)%s", c_type_name(c, expected_type), c_type_zero_literal(expected_type));
			}
			else
			{
				PRINT("0");
			}
			continue;
		}

		const char *arg_tname = arg_type ? c_type_name(c, arg_type) : "void*";
		const char *exp_tname = expected_type ? c_type_name(c, expected_type) : "void*";
		bool exp_is_any       = (strcmp(exp_tname, "__c3_any__") == 0);
		bool arg_is_any       = (strcmp(arg_tname, "__c3_any__") == 0);
		bool exp_is_ptr       = expected_type ? (type_is_pointer(expected_type) || exp_tname[strlen(exp_tname) - 1] == '*') : false;

		if (exp_is_any)
		{
			if (arg_is_any)
			{
				PRINTF("___var_%d", c_args[i].var);
			}
			else if (arg_type && type_is_pointer(arg_type))
			{
				Type *ptype = arg_type->pointer ? arg_type->pointer : type_void;
				PRINTF("(__c3_any__){ .ptr = (void*)___var_%d, .typeid = (c3typeid_t)&%s }", c_args[i].var, c_typeid_name(ptype));
			}
			else if (c_args[i].kind == CV_ADDRESS)
			{
				PRINTF("(__c3_any__){ .ptr = (void*)___var_%d, .typeid = (c3typeid_t)&%s }", c_args[i].var, c_typeid_name(arg_type));
			}
			else if (arg_type)
			{
				PRINTF("(__c3_any__){ .ptr = (void*)&___var_%d, .typeid = (c3typeid_t)&%s }", c_args[i].var, c_typeid_name(arg_type));
			}
			else
			{
				PRINTF("(__c3_any__){ .ptr = (void*)&___var_%d, .typeid = NULL }", c_args[i].var);
			}
		}
		else if (arg_is_any)
		{
			if (exp_is_ptr)
			{
				PRINTF("(%s)___var_%d.ptr", exp_tname, c_args[i].var);
			}
			else if (expected_type && type_is_integer(expected_type))
			{
				PRINTF("(%s)(uintptr_t)___var_%d.ptr", exp_tname, c_args[i].var);
			}
			else
			{
				PRINTF("(void*)___var_%d.ptr", c_args[i].var);
			}
		}
		else if (exp_is_ptr)
		{
			if (arg_type && arg_type->type_kind == TYPE_SLICE)
			{
				PRINTF("(%s)___var_%d.ptr", exp_tname, c_args[i].var);
			}
			else if (arg_type && type_is_pointer(arg_type))
			{
				if (strcmp(exp_tname, arg_tname) == 0)
				{
					PRINTF("___var_%d", c_args[i].var);
				}
				else
				{
					PRINTF("(void*)___var_%d", c_args[i].var);
				}
			}
			else if (arg_type && c_type_is_aggregate(arg_type))
			{
				PRINTF("(%s)&___var_%d", exp_tname, c_args[i].var);
			}
			else if (arg_type && type_is_integer(arg_type))
			{
				PRINTF("(%s)(uintptr_t)___var_%d", exp_tname, c_args[i].var);
			}
			else
			{
				PRINTF("(%s)___var_%d", exp_tname, c_args[i].var);
			}
		}
		else if (expected_type && type_is_integer(expected_type))
		{
			if (arg_type && type_is_pointer(arg_type))
			{
				PRINTF("(%s)(uintptr_t)___var_%d", exp_tname, c_args[i].var);
			}
			else if (arg_type && strcmp(exp_tname, arg_tname) != 0)
			{
				PRINTF("(%s)___var_%d", exp_tname, c_args[i].var);
			}
			else
			{
				PRINTF("___var_%d", c_args[i].var);
			}
		}
		else if (expected_type && expected_type->type_kind == TYPE_SLICE && arg_type && (arg_type->type_kind == TYPE_ARRAY || arg_type->type_kind == TYPE_VECTOR || arg_type->type_kind == TYPE_SIMD_VECTOR))
		{
			PRINTF("(%s){ .ptr = (void*)(___var_%d%sptr), .len = %llu }",
			       exp_tname, c_args[i].var, c_arrow(&c_args[i]), (unsigned long long)arg_type->array.len);
		}
		else if (expected_type && expected_type->type_kind == TYPE_SLICE && arg_type && arg_type->type_kind == TYPE_POINTER && arg_type->pointer && (arg_type->pointer->type_kind == TYPE_ARRAY || arg_type->pointer->type_kind == TYPE_VECTOR || arg_type->pointer->type_kind == TYPE_SIMD_VECTOR))
		{
			PRINTF("(%s){ .ptr = (void*)(___var_%d), .len = %llu }",
			       exp_tname, c_args[i].var, (unsigned long long)arg_type->pointer->array.len);
		}
		else if (exp_is_ptr && arg_type && (arg_type->type_kind == TYPE_ARRAY || arg_type->type_kind == TYPE_VECTOR || arg_type->type_kind == TYPE_SIMD_VECTOR))
		{
			PRINTF("(%s)(___var_%d%sptr)", exp_tname, c_args[i].var, c_arrow(&c_args[i]));
		}
		else if (expected_type && c_type_is_aggregate(expected_type) && arg_type && c_type_is_aggregate(arg_type) && strcmp(exp_tname, arg_tname) != 0)
		{
			PRINTF("*(%s*)&___var_%d", exp_tname, c_args[i].var);
		}
		else if (expected_type && strcmp(exp_tname, arg_tname) != 0 && !c_type_is_aggregate(expected_type) && (!arg_type || !c_type_is_aggregate(arg_type)))
		{
			PRINTF("(%s)___var_%d", exp_tname, c_args[i].var);
		}
		else
		{
			PRINTF("___var_%d", c_args[i].var);
		}
	}
	if (call->has_optional_arg)
	{
		PRINT(");\n}\n");
	}
	else
	{
		PRINT(");\n");
	}
	if (call->no_return && !call->has_optional_arg)
	{
		c->current_block_live = false;
	}
	if (value && expr->type && type_is_optional(expr->type))
	{
		int fault_temp = c_emit_temp_var(c, NULL, type_fault);
		PRINTF("c3fault_t ___var_%d = __c3_current_fault;\n", fault_temp);
		value->optional = fault_temp;
	}
	free(c_args);
}

static void c_emit_binary_expr(GenContext *c, CValue *value, Expr *expr)
{
	ExprBinary *binary = &expr->binary_expr;
	BinaryOp op        = binary->operator;

	if (op >= BINARYOP_ASSIGN)
	{
		Expr *left       = exprptr(binary->left);
		Expr *right      = exprptr(binary->right);
		CValue right_val = {0};
		c_emit_expr(c, &right_val, right);
		c_value_rvalue(c, &right_val);

		Type *target_t           = c_expr_type(left);
		const char *target_tname = c_type_name(c, target_t);

		if (!c_type_is_aggregate(target_t) && (op == BINARYOP_SHL_ASSIGN || op == BINARYOP_SHR_ASSIGN))
		{
			if (safe_mode_enabled())
			{
				if (type_is_signed(right_val.type))
				{
					PRINTF("if (___var_%d < 0 || (size_t)___var_%d >= sizeof(%s)*8) { __c3_abort(); }\n",
					       right_val.var, right_val.var, target_tname);
				}
				else
				{
					PRINTF("if ((size_t)___var_%d >= sizeof(%s)*8) { __c3_abort(); }\n",
					       right_val.var, target_tname);
				}
			}
		}
		else if (compiler.build.feature.trap_on_wrap && type_is_signed(target_t) && !c_type_is_aggregate(target_t) &&
		         (op == BINARYOP_ADD_ASSIGN || op == BINARYOP_SUB_ASSIGN || op == BINARYOP_MULT_ASSIGN))
		{
			CValue current_left = {0};
			bool is_ident       = (left->expr_kind == EXPR_IDENTIFIER || left->expr_kind == EXPR_DECL);
			CValue addr_val     = {0};
			if (is_ident)
			{
				c_emit_lvalue_read(c, left, &current_left, target_t);
			}
			else
			{
				c_emit_lvalue_addr(c, left, &addr_val, type_get_ptr(target_t));
				int cur_temp = c_emit_temp_var(c, &current_left, target_t);
				PRINTF("%s ___var_%d = *(%s*)___var_%d;\n", target_tname, cur_temp, target_tname, addr_val.var);
			}
			int res_temp      = c_emit_temp_var(c, NULL, target_t);
			const char *bname = (op == BINARYOP_ADD_ASSIGN) ? "__builtin_add_overflow" : (op == BINARYOP_SUB_ASSIGN) ? "__builtin_sub_overflow"
			                                                                                                         : "__builtin_mul_overflow";
			PRINTF("if (%s(___var_%d, ___var_%d, &___var_%d)) { __c3_abort(); }\n",
			       bname, current_left.var, right_val.var, res_temp);
			CValue res_val = {.var = res_temp, .type = target_t, .kind = CV_VALUE};
			if (is_ident)
			{
				c_emit_lvalue_assign(c, left, &res_val, "=", NULL);
			}
			else
			{
				PRINTF("*(%s*)___var_%d = ___var_%d;\n", target_tname, addr_val.var, res_temp);
			}
			*value = res_val;
			return;
		}

		const char *assign_op = "=";
		switch (op)
		{
			case BINARYOP_ASSIGN: assign_op = "="; break;
			case BINARYOP_ADD_ASSIGN: assign_op = "+="; break;
			case BINARYOP_SUB_ASSIGN: assign_op = "-="; break;
			case BINARYOP_MULT_ASSIGN: assign_op = "*="; break;
			case BINARYOP_DIV_ASSIGN: assign_op = "/="; break;
			case BINARYOP_MOD_ASSIGN: assign_op = "%="; break;
			case BINARYOP_BIT_AND_ASSIGN: assign_op = "&="; break;
			case BINARYOP_BIT_OR_ASSIGN: assign_op = "|="; break;
			case BINARYOP_BIT_XOR_ASSIGN: assign_op = "^="; break;
			case BINARYOP_SHL_ASSIGN: assign_op = "<<="; break;
			case BINARYOP_SHR_ASSIGN: assign_op = ">>="; break;
			default: break;
		}
		c_emit_lvalue_assign(c, left, &right_val, assign_op, value);
		return;
	}

	if (op == BINARYOP_ELSE)
	{
		Type *res_type = (expr->type && is_valid_type_ptr(expr->type)) ? c_safe_type_lower(expr->type) : type_void;
		if (res_type->type_kind == TYPE_VOID)
		{
			res_type = type_int;
		}
		int temp        = c_emit_temp_var(c, value, res_type);
		CValue left_val = {0};
		c_emit_expr(c, &left_val, exprptr(binary->left));
		c_value_rvalue(c, &left_val);
		c_emit_assign_var(c, temp, res_type, &left_val);
		PRINTF("if (__c3_current_fault != NULL) {\n\t__c3_current_fault = NULL;\n");
		CValue right_val = {0};
		c_emit_expr(c, &right_val, exprptr(binary->right));
		c_value_rvalue(c, &right_val);
		c_emit_assign_var(c, temp, res_type, &right_val);
		PRINT("}\n");
		return;
	}

	if (op == BINARYOP_AND || op == BINARYOP_OR)
	{
		CValue left_val = {0};
		c_emit_expr(c, &left_val, exprptr(binary->left));
		c_value_rvalue(c, &left_val);
		int temp    = c_emit_temp_var(c, value, type_bool);
		bool is_and = (op == BINARYOP_AND);
		PRINTF("bool ___var_%d = %s;\n", temp, is_and ? "false" : "true");
		if (left_val.var != 0)
		{
			PRINTF("if (%s___var_%d) {\n", is_and ? "" : "!", left_val.var);
		}
		else
		{
			PRINTF("if (%s0) {\n", is_and ? "" : "!");
		}
		CValue right_val = {0};
		c_emit_expr(c, &right_val, exprptr(binary->right));
		c_value_rvalue(c, &right_val);
		if (right_val.var != 0)
		{
			PRINTF("___var_%d = ___var_%d;\n}\n", temp, right_val.var);
		}
		else
		{
			PRINTF("___var_%d = 0;\n}\n", temp);
		}
		return;
	}

	bool is_comp = (op == BINARYOP_EQ || op == BINARYOP_NE || op == BINARYOP_LT || op == BINARYOP_LE || op == BINARYOP_GT || op == BINARYOP_GE ||
	                op == BINARYOP_VEC_EQ || op == BINARYOP_VEC_NE || op == BINARYOP_VEC_LT || op == BINARYOP_VEC_LE || op == BINARYOP_VEC_GT || op == BINARYOP_VEC_GE);

	Type *expr_type = (expr->type && is_valid_type_ptr(expr->type)) ? c_safe_type_lower(expr->type) : type_void;
	if (expr_type->type_kind == TYPE_VOID)
	{
		expr_type = is_comp ? type_bool : (exprptr(binary->left)->type && is_valid_type_ptr(exprptr(binary->left)->type) ? c_safe_type_lower(exprptr(binary->left)->type) : type_int);
	}
	c_emit_type_forward_decl(c, expr_type);
	const char *type_string = c_type_name(c, expr_type);

	value->var  = c_create_variable(c);
	value->type = expr_type;
	value->kind = CV_VALUE;

	bool is_opt        = (expr->type && type_is_optional(expr->type));
	int opt_skip_label = 0;
	if (is_opt)
	{
		opt_skip_label = c_create_label(c);
	}

	CValue left_value = {0}, right_value = {0};
	c_emit_expr(c, &left_value, exprptr(binary->left));
	c_value_rvalue(c, &left_value);

	if (is_opt)
	{
		PRINTF("if (__c3_current_fault != NULL) goto __C3_LABEL_%d;\n", opt_skip_label);
	}

	c_emit_expr(c, &right_value, exprptr(binary->right));
	c_value_rvalue(c, &right_value);

	if (is_opt)
	{
		PRINTF("if (__c3_current_fault != NULL) goto __C3_LABEL_%d;\n", opt_skip_label);
	}

	c_ensure_cvalue_var(c, &left_value, expr_type);
	c_ensure_cvalue_var(c, &right_value, expr_type);

	Type *left_t  = (left_value.type && is_valid_type_ptr(left_value.type)) ? c_safe_type_lower(left_value.type) : NULL;
	Type *right_t = (right_value.type && is_valid_type_ptr(right_value.type)) ? c_safe_type_lower(right_value.type) : NULL;

	if (!c_type_is_aggregate(expr_type) && (op == BINARYOP_SHR || op == BINARYOP_SHL))
	{
		bool is_shl = (op == BINARYOP_SHL);
		if (safe_mode_enabled())
		{
			if (right_t && type_is_signed(right_t))
			{
				PRINTF("if (___var_%d < 0 || (size_t)___var_%d >= sizeof(%s)*8) { __c3_abort(); }\n",
				       right_value.var, right_value.var, type_string);
			}
			else
			{
				PRINTF("if ((size_t)___var_%d >= sizeof(%s)*8) { __c3_abort(); }\n",
				       right_value.var, type_string);
			}
		}
		if (left_t && type_is_unsigned(left_t))
		{
			PRINTF("%s ___var_%d = ((size_t)___var_%d >= sizeof(%s)*8) ? 0 : (___var_%d %s ___var_%d);\n",
			       type_string, value->var, right_value.var, type_string, left_value.var, is_shl ? "<<" : ">>", right_value.var);
			return;
		}
		else
		{
			PRINTF("%s ___var_%d = ___var_%d %s ___var_%d;\n",
			       type_string, value->var, left_value.var, is_shl ? "<<" : ">>", right_value.var);
			return;
		}
	}

	const char *operator_string = "+";
	switch (op)
	{
		case BINARYOP_MULT:
			if (compiler.build.feature.trap_on_wrap && type_is_signed(expr_type) && !c_type_is_aggregate(expr_type))
			{
				PRINTF("if (__builtin_mul_overflow(___var_%d, ___var_%d, &___var_%d)) { __c3_abort(); }\n",
				       left_value.var, right_value.var, value->var);
				return;
			}
			operator_string = "*";
			break;
		case BINARYOP_SUB:
			if (compiler.build.feature.trap_on_wrap && type_is_signed(expr_type) && !c_type_is_aggregate(expr_type))
			{
				PRINTF("if (__builtin_sub_overflow(___var_%d, ___var_%d, &___var_%d)) { __c3_abort(); }\n",
				       left_value.var, right_value.var, value->var);
				return;
			}
			operator_string = "-";
			break;
		case BINARYOP_ADD:
			if (compiler.build.feature.trap_on_wrap && type_is_signed(expr_type) && !c_type_is_aggregate(expr_type))
			{
				PRINTF("if (__builtin_add_overflow(___var_%d, ___var_%d, &___var_%d)) { __c3_abort(); }\n",
				       left_value.var, right_value.var, value->var);
				return;
			}
			operator_string = "+";
			break;
		case BINARYOP_DIV: operator_string = "/"; break;
		case BINARYOP_MOD:
			if (type_is_float(expr_type))
			{
				PRINTF("%s ___var_%d = __c3_fmod(___var_%d, ___var_%d);\n", type_string, value->var, left_value.var, right_value.var);
				return;
			}
			operator_string = "%";
			break;
		case BINARYOP_BIT_OR: operator_string = "|"; break;
		case BINARYOP_BIT_XOR: operator_string = "^"; break;
		case BINARYOP_BIT_AND: operator_string = "&"; break;
		case BINARYOP_SHL: operator_string = "<<"; break;
		case BINARYOP_SHR: operator_string = ">>"; break;
		case BINARYOP_GT:
		case BINARYOP_VEC_GT: operator_string = ">"; break;
		case BINARYOP_GE:
		case BINARYOP_VEC_GE: operator_string = ">="; break;
		case BINARYOP_LT:
		case BINARYOP_VEC_LT: operator_string = "<"; break;
		case BINARYOP_LE:
		case BINARYOP_VEC_LE: operator_string = "<="; break;
		case BINARYOP_NE:
		case BINARYOP_VEC_NE: operator_string = "!="; break;
		case BINARYOP_EQ:
		case BINARYOP_VEC_EQ: operator_string = "=="; break;
		default: operator_string = "+"; break;
	}

	bool left_is_ptr  = left_t && (type_is_pointer(left_t) || left_t->type_kind == TYPE_FUNC_PTR || left_t->type_kind == TYPE_ANYFAULT || left_t->type_kind == TYPE_TYPEID);
	bool right_is_ptr = right_t && (type_is_pointer(right_t) || right_t->type_kind == TYPE_FUNC_PTR || right_t->type_kind == TYPE_ANYFAULT || right_t->type_kind == TYPE_TYPEID);

	if (left_t && right_t)
	{
		if (left_is_ptr && right_is_ptr && op == BINARYOP_SUB)
		{
			Type *base_t = (left_t->pointer && left_t->pointer->type_kind != TYPE_VOID) ? left_t->pointer : right_t->pointer;
			if (!base_t || base_t->type_kind == TYPE_VOID)
			{
				PRINTF("%s ___var_%d = ((char*)___var_%d) - ((char*)___var_%d);\n",
				       type_string, value->var, left_value.var, right_value.var);
			}
			else
			{
				const char *btn = c_type_name(c, base_t);
				PRINTF("%s ___var_%d = ((%s*)___var_%d) - ((%s*)___var_%d);\n",
				       type_string, value->var, btn, left_value.var, btn, right_value.var);
			}
			return;
		}
		if (left_is_ptr && type_is_integer(right_t) && (op == BINARYOP_ADD || op == BINARYOP_SUB))
		{
			if (left_t->pointer && left_t->pointer->type_kind == TYPE_VOID)
			{
				PRINTF("%s ___var_%d = (void*)(((char*)___var_%d) %s ___var_%d);\n",
				       type_string, value->var, left_value.var, operator_string, right_value.var);
				return;
			}
		}
	}

	if (left_t && right_t && is_comp)
	{
		if (left_t->type_kind == TYPE_SLICE && right_t->type_kind == TYPE_SLICE)
		{
			Type *elem_t       = left_t->array.base ? type_flatten(left_t->array.base) : NULL;
			bool elem_is_slice = elem_t && elem_t->type_kind == TYPE_SLICE;
			bool elem_is_float = elem_t && type_is_float(elem_t);
			bool is_eq         = (op == BINARYOP_EQ || op == BINARYOP_VEC_EQ);
			if (elem_is_slice || elem_is_float)
			{
				int loop_k = c_create_variable(c);
				PRINTF("bool ___var_%d = (___var_%d.len == ___var_%d.len);\n", value->var, left_value.var, right_value.var);
				PRINTF("if (___var_%d) {\n", value->var);
				PRINTF("\tfor (size_t ___var_%d = 0; ___var_%d < ___var_%d.len; ___var_%d++) {\n", loop_k, loop_k, left_value.var, loop_k);
				if (elem_is_slice)
				{
					PRINTF("\t\tif (!__C3_SLICE_EQ(___var_%d.ptr[___var_%d], ___var_%d.ptr[___var_%d])) { ___var_%d = false; break; }\n",
					       left_value.var, loop_k, right_value.var, loop_k, value->var);
				}
				else
				{
					PRINTF("\t\tif (___var_%d.ptr[___var_%d] != ___var_%d.ptr[___var_%d]) { ___var_%d = false; break; }\n",
					       left_value.var, loop_k, right_value.var, loop_k, value->var);
				}
				PRINT("\t}\n");
				PRINT("}\n");
				if (!is_eq)
				{
					PRINTF("___var_%d = !___var_%d;\n", value->var, value->var);
				}
				return;
			}
			PRINTF("bool ___var_%d = %s(___var_%d, ___var_%d);\n",
			       value->var, is_eq ? "__C3_SLICE_EQ" : "__C3_SLICE_NE", left_value.var, right_value.var);
			return;
		}
		if ((left_t->type_kind == TYPE_SLICE && right_t->type_kind == TYPE_ARRAY) ||
		    (left_t->type_kind == TYPE_ARRAY && right_t->type_kind == TYPE_SLICE))
		{
			bool left_is_slice         = (left_t->type_kind == TYPE_SLICE);
			int s_var                  = left_is_slice ? left_value.var : right_value.var;
			int a_var                  = left_is_slice ? right_value.var : left_value.var;
			Type *arr_t                = left_is_slice ? right_t : left_t;
			unsigned long long arr_len = (unsigned long long)arr_t->array.len;
			bool is_eq                 = (op == BINARYOP_EQ || op == BINARYOP_VEC_EQ);
			Type *elem_t               = arr_t->array.base ? type_flatten(arr_t->array.base) : NULL;
			bool elem_is_float         = elem_t && type_is_float(elem_t);
			if (elem_is_float)
			{
				int loop_k = c_create_variable(c);
				PRINTF("bool ___var_%d = (___var_%d.len == %llu);\n", value->var, s_var, arr_len);
				PRINTF("if (___var_%d) {\n", value->var);
				PRINTF("\tfor (size_t ___var_%d = 0; ___var_%d < %llu; ___var_%d++) {\n", loop_k, loop_k, arr_len, loop_k);
				PRINTF("\t\tif (___var_%d.ptr[___var_%d] != ___var_%d.ptr[___var_%d]) { ___var_%d = false; break; }\n",
				       s_var, loop_k, a_var, loop_k, value->var);
				PRINT("\t}\n");
				PRINT("}\n");
				if (!is_eq)
				{
					PRINTF("___var_%d = !___var_%d;\n", value->var, value->var);
				}
				return;
			}
			const char *cmp_op   = is_eq ? "==" : "!=";
			const char *logic_op = is_eq ? "&&" : "||";
			PRINTF("bool ___var_%d = (___var_%d.len %s %llu %s (___var_%d.len == 0 || __c3_memcmp(___var_%d.ptr, ___var_%d.ptr, ___var_%d.len * sizeof(*___var_%d.ptr)) %s 0));\n",
			       value->var, s_var, cmp_op, arr_len, logic_op, s_var, s_var, a_var, s_var, s_var, cmp_op);
			return;
		}
		if ((left_t->type_kind == TYPE_ANY || left_t->type_kind == TYPE_INTERFACE || left_t->type_kind == TYPE_SLICE) && right_is_ptr)
		{
			PRINTF("bool ___var_%d = (___var_%d.ptr %s (void*)___var_%d);\n",
			       value->var, left_value.var, (op == BINARYOP_EQ || op == BINARYOP_VEC_EQ) ? "==" : "!=", right_value.var);
			return;
		}
		if (left_is_ptr && (right_t->type_kind == TYPE_ANY || right_t->type_kind == TYPE_INTERFACE || right_t->type_kind == TYPE_SLICE))
		{
			PRINTF("bool ___var_%d = ((void*)___var_%d %s ___var_%d.ptr);\n",
			       value->var, left_value.var, (op == BINARYOP_EQ || op == BINARYOP_VEC_EQ) ? "==" : "!=", right_value.var);
			return;
		}
		if (left_is_ptr && right_is_ptr)
		{
			PRINTF("bool ___var_%d = ((uintptr_t)___var_%d %s (uintptr_t)___var_%d);\n",
			       value->var, left_value.var, operator_string, right_value.var);
			return;
		}
		if ((left_is_ptr && type_is_integer(right_t)) || (type_is_integer(left_t) && right_is_ptr))
		{
			PRINTF("bool ___var_%d = ((uintptr_t)___var_%d %s (uintptr_t)___var_%d);\n", value->var, left_value.var, operator_string, right_value.var);
			return;
		}
		if (c_type_is_aggregate(left_t) || c_type_is_aggregate(right_t))
		{
			if (expr_type->type_kind == TYPE_BOOL)
			{
				Type *base_t = (left_t && (left_t->type_kind == TYPE_ARRAY || left_t->type_kind == TYPE_VECTOR || left_t->type_kind == TYPE_SIMD_VECTOR)) ? left_t->array.base : NULL;
				if (base_t)
				{
					base_t = type_flatten(base_t);
				}
				if (base_t && type_is_float(base_t))
				{
					int len = (int)left_t->array.len;
					if (len <= 0)
					{
						len = 1;
					}
					bool is_eq           = (op == BINARYOP_EQ || op == BINARYOP_VEC_EQ);
					const char *logic_op = is_eq ? "&&" : "||";
					const char *cmp_op   = is_eq ? "==" : "!=";
					PRINTF("bool ___var_%d = (", value->var);
					for (int k = 0; k < len; k++)
					{
						if (k > 0)
						{
							PRINTF(" %s ", logic_op);
						}
						PRINTF("(___var_%d.ptr[%d] %s ___var_%d.ptr[%d])",
						       left_value.var, k, cmp_op, right_value.var, k);
					}
					PRINT(");\n");
					return;
				}
				PRINTF("bool ___var_%d = (__c3_memcmp(&___var_%d, &___var_%d, sizeof(%s)) %s 0);\n",
				       value->var, left_value.var, right_value.var, c_type_name(c, left_t),
				       (op == BINARYOP_EQ || op == BINARYOP_VEC_EQ) ? "==" : "!=");
				return;
			}
			int len = (expr_type->type_kind == TYPE_ARRAY || expr_type->type_kind == TYPE_VECTOR || expr_type->type_kind == TYPE_SIMD_VECTOR) ? (int)expr_type->array.len : 1;
			if (len <= 0)
			{
				len = 1;
			}
			PRINTF("%s ___var_%d;\n", type_string, value->var);
			for (int k = 0; k < len; k++)
			{
				PRINTF("___var_%d.ptr[%d] = (___var_%d.ptr[%d] %s ___var_%d.ptr[%d]) ? -1 : 0;\n",
				       value->var, k, left_value.var, k, operator_string, right_value.var, k);
			}
			return;
		}
	}

	if (expr_type->type_kind == TYPE_ARRAY || expr_type->type_kind == TYPE_VECTOR || expr_type->type_kind == TYPE_SIMD_VECTOR)
	{
		int len = (int)expr_type->array.len;
		if (len <= 0)
		{
			len = 1;
		}
		PRINTF("%s ___var_%d;\n", type_string, value->var);
		bool left_is_vec  = (left_t && (left_t->type_kind == TYPE_ARRAY || left_t->type_kind == TYPE_VECTOR || left_t->type_kind == TYPE_SIMD_VECTOR));
		bool right_is_vec = (right_t && (right_t->type_kind == TYPE_ARRAY || right_t->type_kind == TYPE_VECTOR || right_t->type_kind == TYPE_SIMD_VECTOR));
		for (int k = 0; k < len; k++)
		{
			PRINTF("___var_%d.ptr[%d] = ", value->var, k);
			if (left_is_vec)
			{
				PRINTF("___var_%d.ptr[%d]", left_value.var, k);
			}
			else
			{
				PRINTF("___var_%d", left_value.var);
			}
			PRINTF(" %s ", operator_string);
			if (right_is_vec)
			{
				PRINTF("___var_%d.ptr[%d];\n", right_value.var, k);
			}
			else
			{
				PRINTF("___var_%d;\n", right_value.var);
			}
		}
		return;
	}

	PRINTF("%s ___var_%d = ___var_%d %s ___var_%d;\n", type_string, value->var, left_value.var, operator_string, right_value.var);
	if (is_opt)
	{
		c_emit_label(c, opt_skip_label);
	}
}

static void c_emit_bitstruct_read_container(GenContext *c, CValue *value, Expr *parent_expr, Decl *bitstruct_decl, Decl *member, Type *member_type, int container_ptr_var, const char *c_tname)
{
	int temp          = c_emit_temp_var(c, value, member_type);
	const char *mname = c_type_name(c, member_type);

	int start_bit = 0, end_bit = 0;
	c_get_bitstruct_member_bits(member, &start_bit, &end_bit);
	int bit_size = end_bit - start_bit + 1;
	if (bit_size <= 0)
	{
		bit_size = 1;
	}
	uint64_t mask  = (bit_size >= 64) ? ~0ULL : (((uint64_t)1 << bit_size) - 1);
	bool is_signed = type_is_signed(member_type);

	bool is_arr = false;
	if (bitstruct_decl)
	{
		is_arr = c_is_bitstruct_array(bitstruct_decl->type, NULL);
		if (!is_arr && bitstruct_decl->decl_kind == DECL_BITSTRUCT && bitstruct_decl->strukt.container_type && is_valid_type_ptr(bitstruct_decl->strukt.container_type->type))
		{
			Type *cft = type_flatten(bitstruct_decl->strukt.container_type->type);
			if (cft && cft->type_kind == TYPE_ARRAY)
			{
				is_arr = true;
			}
		}
	}
	if (!is_arr && parent_expr && parent_expr->type)
	{
		is_arr = c_is_bitstruct_array(parent_expr->type, NULL);
	}

	bool reverse    = (bitstruct_decl && bitstruct_decl->decl_kind == DECL_BITSTRUCT) ? c_is_bitstruct_big_endian(bitstruct_decl) : false;
	bool need_bswap = (bitstruct_decl && bitstruct_decl->decl_kind == DECL_BITSTRUCT) ? c_is_bitstruct_requires_byteswap(bitstruct_decl) : false;

	const char *shift_type = (bit_size > 64) ? "__c3_uint128" : "uint64_t";

	PRINTF("%s ___var_%d;\n", mname, temp);
	PRINTF("{\n");
	if (is_arr)
	{
		PRINTF("\tconst uint8_t *_b = (const uint8_t *)___var_%d;\n", container_ptr_var);
		PRINTF("\t%s _res = 0;\n", shift_type);
		for (int bits_done = 0; bits_done < bit_size; )
		{
			CBitfieldStep s = c_bitfield_step(start_bit, bit_size, bits_done, reverse);
			if (s.bit_in_byte > 0)
			{
				PRINTF("\t_res |= (((%s)(_b[%d] >> %d & 0x%X)) << %d);\n", shift_type, s.byte_idx, s.bit_in_byte, s.step_mask, s.shift);
			}
			else
			{
				PRINTF("\t_res |= (((%s)(_b[%d] & 0x%X)) << %d);\n", shift_type, s.byte_idx, s.step_mask, s.shift);
			}
			bits_done += s.step_bits;
		}
		if (is_signed && bit_size < 128)
		{
			if (bit_size < 64)
			{
				PRINTF("\tif (_res & (1ULL << %d)) _res |= ~((1ULL << %d) - 1);\n", bit_size - 1, bit_size);
			}
			else
			{
				PRINTF("\tif (_res & ((__c3_uint128)1 << %d)) _res |= ~(((__c3_uint128)1 << %d) - 1);\n", bit_size - 1, bit_size);
			}
		}
		PRINTF("\t___var_%d = (%s)_res;\n", temp, mname);
	}
	else
	{
		PRINTF("\tuint64_t _v = (uint64_t)(*(const %s*)___var_%d);\n", c_tname, container_ptr_var);
		if (need_bswap)
		{
			PRINTF("\tif (sizeof(%s) == 2) _v = __builtin_bswap16((uint16_t)_v);\n", c_tname);
			PRINTF("\telse if (sizeof(%s) == 4) _v = __builtin_bswap32((uint32_t)_v);\n", c_tname);
			PRINTF("\telse if (sizeof(%s) == 8) _v = __builtin_bswap64((uint64_t)_v);\n", c_tname);
		}
		PRINTF("\tuint64_t _r = (_v >> %d) & 0x%" PRIx64 "ULL;\n", start_bit, mask);
		if (is_signed && bit_size < 64)
		{
			PRINTF("\tif (_r & (1ULL << %d)) { _r |= ~0x%" PRIx64 "ULL; }\n", bit_size - 1, mask);
		}
		PRINTF("\t___var_%d = (%s)_r;\n", temp, mname);
	}
	PRINTF("}\n");
}

static void c_emit_bitstruct_write_container(GenContext *c, Expr *parent_expr, Decl *bitstruct_decl, Decl *member, int container_ptr_var, const char *c_tname, CValue *val)
{
	c_value_rvalue(c, val);
	Type *m_type = member->type ? c_safe_type_lower(member->type) : type_uint;
	c_ensure_cvalue_var(c, val, m_type);

	int start_bit = 0, end_bit = 0;
	c_get_bitstruct_member_bits(member, &start_bit, &end_bit);
	int bit_size = end_bit - start_bit + 1;
	if (bit_size <= 0)
	{
		bit_size = 1;
	}
	uint64_t mask         = (bit_size >= 64) ? ~0ULL : (((uint64_t)1 << bit_size) - 1);
	uint64_t shifted_mask = (bit_size + start_bit >= 64) ? ~0ULL : (mask << start_bit);

	bool is_arr = false;
	if (bitstruct_decl)
	{
		is_arr = c_is_bitstruct_array(bitstruct_decl->type, NULL);
		if (!is_arr && bitstruct_decl->decl_kind == DECL_BITSTRUCT && bitstruct_decl->strukt.container_type && is_valid_type_ptr(bitstruct_decl->strukt.container_type->type))
		{
			Type *cft = type_flatten(bitstruct_decl->strukt.container_type->type);
			if (cft && cft->type_kind == TYPE_ARRAY)
			{
				is_arr = true;
			}
		}
	}
	if (!is_arr && parent_expr && parent_expr->type)
	{
		is_arr = c_is_bitstruct_array(parent_expr->type, NULL);
	}

	bool reverse    = (bitstruct_decl && bitstruct_decl->decl_kind == DECL_BITSTRUCT) ? c_is_bitstruct_big_endian(bitstruct_decl) : false;
	bool need_bswap = (bitstruct_decl && bitstruct_decl->decl_kind == DECL_BITSTRUCT) ? c_is_bitstruct_requires_byteswap(bitstruct_decl) : false;

	const char *shift_type = (bit_size > 64) ? "__c3_uint128" : "uint64_t";

	PRINTF("{\n");
	if (is_arr)
	{
		PRINTF("\tuint8_t *_b = (uint8_t *)___var_%d;\n", container_ptr_var);
		for (int bits_done = 0; bits_done < bit_size; )
		{
			CBitfieldStep s = c_bitfield_step(start_bit, bit_size, bits_done, reverse);
			if (s.shift > 0)
			{
				PRINTF("\t_b[%d] = (_b[%d] & ~0x%02X) | (((uint8_t)(((%s)___var_%d) >> %d) & 0x%02X) << %d);\n",
				       s.byte_idx, s.byte_idx, s.byte_mask, shift_type, val->var, s.shift, s.step_mask, s.bit_in_byte);
			}
			else
			{
				PRINTF("\t_b[%d] = (_b[%d] & ~0x%02X) | (((uint8_t)(((%s)___var_%d)) & 0x%02X) << %d);\n",
				       s.byte_idx, s.byte_idx, s.byte_mask, shift_type, val->var, s.step_mask, s.bit_in_byte);
			}
			bits_done += s.step_bits;
		}
	}
	else if (need_bswap)
	{
		PRINTF("\tuint64_t _cur = (uint64_t)(*(%s*)___var_%d);\n", c_tname, container_ptr_var);
		PRINTF("\tif (sizeof(%s) == 2) _cur = __builtin_bswap16((uint16_t)_cur);\n", c_tname);
		PRINTF("\telse if (sizeof(%s) == 4) _cur = __builtin_bswap32((uint32_t)_cur);\n", c_tname);
		PRINTF("\telse if (sizeof(%s) == 8) _cur = __builtin_bswap64((uint64_t)_cur);\n", c_tname);
		PRINTF("\t_cur = (_cur & ~(0x%" PRIx64 "ULL)) | (((uint64_t)___var_%d & 0x%" PRIx64 "ULL) << %d);\n",
		       shifted_mask, val->var, mask, start_bit);
		PRINTF("\tif (sizeof(%s) == 2) _cur = __builtin_bswap16((uint16_t)_cur);\n", c_tname);
		PRINTF("\telse if (sizeof(%s) == 4) _cur = __builtin_bswap32((uint32_t)_cur);\n", c_tname);
		PRINTF("\telse if (sizeof(%s) == 8) _cur = __builtin_bswap64((uint64_t)_cur);\n", c_tname);
		PRINTF("\t*(%s*)___var_%d = (%s)_cur;\n", c_tname, container_ptr_var, c_tname);
	}
	else
	{
		PRINTF("\t*(%s*)___var_%d = (%s)((*(%s*)___var_%d & ~(0x%" PRIx64 "ULL)) | (((uint64_t)___var_%d & 0x%" PRIx64 "ULL) << %d));\n",
		       c_tname, container_ptr_var, c_tname, c_tname, container_ptr_var, shifted_mask, val->var, mask, start_bit);
	}
	PRINTF("}\n");
}

static void c_emit_expr_internal(GenContext *c, CValue *value, Expr *expr)
{
	if (!expr)
	{
		*value = (CValue){.var = 0, .type = type_void, .kind = CV_VALUE};
		return;
	}
	switch (expr->expr_kind)
	{
		case EXPR_CONST:
			c_emit_const_expr(c, value, expr);
			return;
		case EXPR_IDENTIFIER:
			c_emit_identifier_expr(c, value, expr);
			return;
		case EXPR_DECL:
			c_emit_local_decl(c, expr->decl_expr, value);
			return;
		case EXPR_CALL:
			c_emit_call_expr(c, value, expr);
			return;
		case EXPR_BINARY:
			c_emit_binary_expr(c, value, expr);
			return;
		case EXPR_COND:
			c_emit_cond_expr(c, value, expr);
			return;
		case EXPR_EXPRESSION_LIST:
			c_emit_expression_list_expr(c, value, expr);
			return;
		case EXPR_TWO:
			c_emit_ignored_expr(c, expr->two_expr.first);
			c_emit_expr(c, value, expr->two_expr.last);
			return;
		case EXPR_TYPEID:
		{
			Type *t = expr->type_expr ? expr->type_expr->type : expr->type;
			if (!t)
			{
				t = type_void;
			}
			int temp = c_emit_temp_var(c, value, type_typeid);
			PRINTF("c3typeid_t ___var_%d = (c3typeid_t)&%s;\n", temp, c_typeid_name(t));
			return;
		}
		case EXPR_SLICE_ASSIGN:
		{
			CValue dst_val = {0}, src_val = {0};
			c_emit_expr(c, &dst_val, exprptr(expr->slice_assign_expr.left));
			c_emit_expr(c, &src_val, exprptr(expr->slice_assign_expr.right));
			c_value_rvalue(c, &src_val);
			c_ensure_cvalue_var(c, &src_val, NULL);
			if (dst_val.var != 0)
			{
				int loop_i = c_create_variable(c);
				PRINTF("for (size_t ___var_%d = 0; ___var_%d < ___var_%d.len; ___var_%d++) { ___var_%d.ptr[___var_%d] = ___var_%d; }\n",
				       loop_i, loop_i, dst_val.var, loop_i, dst_val.var, loop_i, src_val.var);
			}
			*value = dst_val;
			return;
		}
		case EXPR_MACRO_BODY_EXPANSION:
			c_emit_macro_body_expansion(c, value, expr);
			return;
		case EXPR_UNARY:
		{
			UnaryOp uop       = expr->unary_expr.operator;
			Expr *inner       = expr->unary_expr.expr;
			Type *target_type = (expr->type && is_valid_type_ptr(expr->type)) ? c_safe_type_lower(expr->type) : type_void;
			if (target_type->type_kind == TYPE_VOID && inner && inner->type && is_valid_type_ptr(inner->type))
			{
				target_type = c_safe_type_lower(inner->type);
			}
			if (target_type->type_kind == TYPE_VOID)
			{
				target_type = (uop == UNARYOP_NOT) ? type_bool : (uop == UNARYOP_ADDR || uop == UNARYOP_TADDR ? type_voidptr : type_int);
			}
			const char *tname = c_type_name(c, target_type);

			if (uop == UNARYOP_ADDR || uop == UNARYOP_TADDR)
			{
				c_emit_lvalue_addr(c, inner, value, target_type);
				return;
			}
			if (uop == UNARYOP_DEREF)
			{
				c_emit_lvalue_read(c, expr, value, target_type);
				return;
			}
			if (uop == UNARYOP_INC || uop == UNARYOP_DEC)
			{
				if (inner->expr_kind == EXPR_BITACCESS)
				{
					Expr *parent_expr = inner->access_resolved_expr.parent;
					Decl *member      = inner->access_resolved_expr.ref;
					Type *member_type = (inner->type && is_valid_type_ptr(inner->type)) ? c_safe_type_lower(inner->type) : type_uint;
					if (member_type->type_kind == TYPE_VOID)
					{
						member_type = type_uint;
					}
					const char *mname = c_type_name(c, member_type);

					int container_ptr_var = 0;
					Decl *bitstruct_decl  = NULL;
					const char *c_tname   = c_emit_bitstruct_container(c, parent_expr, member, &container_ptr_var, &bitstruct_decl);

					CValue cur_val = {0};
					c_emit_bitstruct_read_container(c, &cur_val, parent_expr, bitstruct_decl, member, member_type, container_ptr_var, c_tname);

					int new_temp = c_emit_temp_var(c, NULL, member_type);
					if (compiler.build.feature.trap_on_wrap && type_is_signed(member_type))
					{
						const char *bname = (uop == UNARYOP_INC) ? "__builtin_add_overflow" : "__builtin_sub_overflow";
						PRINTF("if (%s(___var_%d, 1, &___var_%d)) { __c3_abort(); }\n", bname, cur_val.var, new_temp);
					}
					else
					{
						PRINTF("%s ___var_%d = (%s)(___var_%d %s 1);\n", mname, new_temp, mname, cur_val.var, (uop == UNARYOP_INC ? "+" : "-"));
					}
					CValue new_val = {.var = new_temp, .type = member_type, .kind = CV_VALUE};
					c_emit_bitstruct_write_container(c, parent_expr, bitstruct_decl, member, container_ptr_var, c_tname, &new_val);
					*value = new_val;
					return;
				}
				CValue old_val  = {0};
				bool is_ident   = (inner->expr_kind == EXPR_IDENTIFIER || inner->expr_kind == EXPR_DECL);
				CValue addr_val = {0};
				if (is_ident)
				{
					c_emit_lvalue_read(c, inner, &old_val, target_type);
				}
				else
				{
					c_emit_lvalue_addr(c, inner, &addr_val, type_get_ptr(target_type));
					int cur_temp = c_emit_temp_var(c, &old_val, target_type);
					PRINTF("%s ___var_%d = *(%s*)___var_%d;\n", tname, cur_temp, tname, addr_val.var);
				}
				c_value_rvalue(c, &old_val);
				int new_temp = c_emit_temp_var(c, NULL, target_type);
				if (c_type_is_aggregate(target_type))
				{
					int len = (int)target_type->array.len;
					if (len <= 0)
					{
						len = 1;
					}
					PRINTF("%s ___var_%d;\n", tname, new_temp);
					for (int k = 0; k < len; k++)
					{
						PRINTF("___var_%d.ptr[%d] = ___var_%d.ptr[%d] %s 1;\n", new_temp, k, old_val.var, k, (uop == UNARYOP_INC ? "+" : "-"));
					}
				}
				else if (compiler.build.feature.trap_on_wrap && type_is_signed(target_type))
				{
					const char *bname = (uop == UNARYOP_INC) ? "__builtin_add_overflow" : "__builtin_sub_overflow";
					PRINTF("if (%s(___var_%d, 1, &___var_%d)) { __c3_abort(); }\n", bname, old_val.var, new_temp);
				}
				else
				{
					PRINTF("%s ___var_%d = ___var_%d %s 1;\n", tname, new_temp, old_val.var, (uop == UNARYOP_INC ? "+" : "-"));
				}
				CValue new_val = {.var = new_temp, .type = target_type, .kind = CV_VALUE};
				if (is_ident)
				{
					c_emit_lvalue_assign(c, inner, &new_val, "=", NULL);
				}
				else
				{
					if (c_type_is_aggregate(target_type))
					{
						PRINTF("__c3_memcpy((void*)___var_%d, &___var_%d, sizeof(%s));\n", addr_val.var, new_temp, tname);
					}
					else
					{
						PRINTF("*(%s*)___var_%d = ___var_%d;\n", tname, addr_val.var, new_temp);
					}
				}
				*value = new_val;
				return;
			}

			if (uop == UNARYOP_NOT)
			{
				CValue inner_val = {0};
				c_emit_expr(c, &inner_val, inner);
				c_value_rvalue(c, &inner_val);
				int temp = c_emit_temp_var(c, value, type_bool);
				if (inner_val.type && inner_val.type->type_kind == TYPE_SLICE)
				{
					PRINTF("bool ___var_%d = (___var_%d.len == 0);\n", temp, inner_val.var);
				}
				else if (inner_val.type && (inner_val.type->type_kind == TYPE_ANY || inner_val.type->type_kind == TYPE_INTERFACE))
				{
					PRINTF("bool ___var_%d = (___var_%d.ptr == NULL);\n", temp, inner_val.var);
				}
				else if (inner_val.type && (type_is_pointer(inner_val.type) || inner_val.type->type_kind == TYPE_FUNC_PTR))
				{
					PRINTF("bool ___var_%d = (___var_%d == NULL);\n", temp, inner_val.var);
				}
				else if (inner_val.var != 0)
				{
					PRINTF("bool ___var_%d = !___var_%d;\n", temp, inner_val.var);
				}
				else
				{
					PRINTF("bool ___var_%d = true;\n", temp);
				}
				return;
			}

			CValue inner_val = {0};
			c_emit_expr(c, &inner_val, inner);
			c_value_rvalue(c, &inner_val);
			c_ensure_cvalue_var(c, &inner_val, target_type);

			int temp = c_emit_temp_var(c, value, target_type);

			if (uop == UNARYOP_NEG && compiler.build.feature.trap_on_wrap && type_is_signed(target_type) && !c_type_is_aggregate(target_type))
			{
				PRINTF("if (__builtin_sub_overflow(0, ___var_%d, &___var_%d)) { __c3_abort(); }\n",
				       inner_val.var, temp);
				return;
			}

			const char *op_str = (uop == UNARYOP_NOT) ? "!" : (uop == UNARYOP_BITNEG) ? "~"
			                                              : (uop == UNARYOP_PLUS)     ? "+"
			                                                                          : "-";
			if (target_type->type_kind == TYPE_ARRAY || target_type->type_kind == TYPE_VECTOR || target_type->type_kind == TYPE_SIMD_VECTOR)
			{
				int len = (int)target_type->array.len;
				if (len <= 0)
				{
					len = 1;
				}
				PRINTF("%s ___var_%d;\n", tname, temp);
				for (int k = 0; k < len; k++)
				{
					PRINTF("___var_%d.ptr[%d] = %s___var_%d.ptr[%d];\n", temp, k, op_str, inner_val.var, k);
				}
				return;
			}
			PRINTF("%s ___var_%d = %s___var_%d;\n", tname, temp, op_str, inner_val.var);
			return;
		}
		case EXPR_POST_UNARY:
		{
			UnaryOp uop = expr->unary_expr.operator;
			Expr *inner = expr->unary_expr.expr;
			if (inner->expr_kind == EXPR_BITACCESS)
			{
				Expr *parent_expr = inner->access_resolved_expr.parent;
				Decl *member      = inner->access_resolved_expr.ref;
				Type *member_type = (inner->type && is_valid_type_ptr(inner->type)) ? c_safe_type_lower(inner->type) : type_uint;
				if (member_type->type_kind == TYPE_VOID)
				{
					member_type = type_uint;
				}
				const char *mname = c_type_name(c, member_type);

				int container_ptr_var = 0;
				Decl *bitstruct_decl  = NULL;
				const char *c_tname   = c_emit_bitstruct_container(c, parent_expr, member, &container_ptr_var, &bitstruct_decl);

				CValue cur_val = {0};
				c_emit_bitstruct_read_container(c, &cur_val, parent_expr, bitstruct_decl, member, member_type, container_ptr_var, c_tname);

				int new_temp = c_emit_temp_var(c, NULL, member_type);
				if (compiler.build.feature.trap_on_wrap && type_is_signed(member_type))
				{
					const char *bname = (uop == UNARYOP_INC) ? "__builtin_add_overflow" : "__builtin_sub_overflow";
					PRINTF("if (%s(___var_%d, 1, &___var_%d)) { __c3_abort(); }\n", bname, cur_val.var, new_temp);
				}
				else
				{
					PRINTF("%s ___var_%d = (%s)(___var_%d %s 1);\n", mname, new_temp, mname, cur_val.var, (uop == UNARYOP_INC ? "+" : "-"));
				}
				CValue new_val = {.var = new_temp, .type = member_type, .kind = CV_VALUE};
				c_emit_bitstruct_write_container(c, parent_expr, bitstruct_decl, member, container_ptr_var, c_tname, &new_val);
				*value = cur_val;
				return;
			}
			Type *target_type = (expr->type && is_valid_type_ptr(expr->type)) ? c_safe_type_lower(expr->type) : type_void;
			if (target_type->type_kind == TYPE_VOID && inner && inner->type && is_valid_type_ptr(inner->type))
			{
				target_type = c_safe_type_lower(inner->type);
			}
			if (target_type->type_kind == TYPE_VOID)
			{
				target_type = type_int;
			}
			const char *tname = c_type_name(c, target_type);

			CValue old_val  = {0};
			bool is_ident   = (inner->expr_kind == EXPR_IDENTIFIER || inner->expr_kind == EXPR_DECL);
			CValue addr_val = {0};
			if (is_ident)
			{
				c_emit_lvalue_read(c, inner, &old_val, target_type);
			}
			else
			{
				c_emit_lvalue_addr(c, inner, &addr_val, type_get_ptr(target_type));
				int cur_temp = c_emit_temp_var(c, &old_val, target_type);
				PRINTF("%s ___var_%d = *(%s*)___var_%d;\n", tname, cur_temp, tname, addr_val.var);
			}
			c_value_rvalue(c, &old_val);
			int new_temp = c_emit_temp_var(c, NULL, target_type);
			if (c_type_is_aggregate(target_type))
			{
				int len = (int)target_type->array.len;
				if (len <= 0)
				{
					len = 1;
				}
				PRINTF("%s ___var_%d;\n", tname, new_temp);
				for (int k = 0; k < len; k++)
				{
					PRINTF("___var_%d.ptr[%d] = ___var_%d.ptr[%d] %s 1;\n", new_temp, k, old_val.var, k, (uop == UNARYOP_INC ? "+" : "-"));
				}
			}
			else if (compiler.build.feature.trap_on_wrap && type_is_signed(target_type))
			{
				const char *bname = (uop == UNARYOP_INC) ? "__builtin_add_overflow" : "__builtin_sub_overflow";
				PRINTF("if (%s(___var_%d, 1, &___var_%d)) { __c3_abort(); }\n", bname, old_val.var, new_temp);
			}
			else
			{
				PRINTF("%s ___var_%d = ___var_%d %s 1;\n", tname, new_temp, old_val.var, (uop == UNARYOP_INC ? "+" : "-"));
			}
			CValue new_val = {.var = new_temp, .type = target_type, .kind = CV_VALUE};
			if (is_ident)
			{
				c_emit_lvalue_assign(c, inner, &new_val, "=", NULL);
			}
			else
			{
				if (c_type_is_aggregate(target_type))
				{
					PRINTF("__c3_memcpy((void*)___var_%d, &___var_%d, sizeof(%s));\n", addr_val.var, new_temp, tname);
				}
				else
				{
					PRINTF("*(%s*)___var_%d = ___var_%d;\n", tname, addr_val.var, new_temp);
				}
			}
			*value = old_val;
			return;
		}
		case EXPR_ACCESS_RESOLVED:
		case EXPR_SUBSCRIPT:
		{
			Type *target_type = (expr->type && is_valid_type_ptr(expr->type)) ? c_safe_type_lower(expr->type) : type_void;
			c_emit_lvalue_read(c, expr, value, target_type);
			return;
		}
		case EXPR_BITACCESS:
		{
			if (!expr->access_resolved_expr.parent)
			{
				*value = (CValue){.var = 0, .type = type_void, .kind = CV_VALUE};
				return;
			}
			Decl *member      = expr->access_resolved_expr.ref;
			Type *member_type = (expr->type && is_valid_type_ptr(expr->type)) ? c_safe_type_lower(expr->type) : type_uint;
			if (member_type->type_kind == TYPE_VOID)
			{
				member_type = type_uint;
			}

			int container_ptr_var = 0;
			Decl *bitstruct_decl  = NULL;
			const char *c_tname   = c_emit_bitstruct_container(c, expr->access_resolved_expr.parent, member, &container_ptr_var, &bitstruct_decl);
			c_emit_bitstruct_read_container(c, value, expr->access_resolved_expr.parent, bitstruct_decl, member, member_type, container_ptr_var, c_tname);
			return;
		}
		case EXPR_BITASSIGN:
		{
			Expr *left  = exprptr(expr->binary_expr.left);
			Expr *right = exprptr(expr->binary_expr.right);
			BinaryOp op = expr->binary_expr.operator;

			Decl *member      = left->access_resolved_expr.ref;
			Type *member_type = (left->type && is_valid_type_ptr(left->type)) ? c_safe_type_lower(left->type) : type_uint;
			if (member_type->type_kind == TYPE_VOID)
			{
				member_type = type_uint;
			}
			const char *mname = c_type_name(c, member_type);

			int container_ptr_var = 0;
			Decl *bitstruct_decl  = NULL;
			const char *c_tname   = c_emit_bitstruct_container(c, left->access_resolved_expr.parent, member, &container_ptr_var, &bitstruct_decl);

			CValue result_val = {0};
			if (op != BINARYOP_ASSIGN)
			{
				CValue cur_val = {0};
				c_emit_bitstruct_read_container(c, &cur_val, left->access_resolved_expr.parent, bitstruct_decl, member, member_type, container_ptr_var, c_tname);

				CValue right_val = {0};
				c_emit_expr(c, &right_val, right);
				c_value_rvalue(c, &right_val);
				c_ensure_cvalue_var(c, &right_val, member_type);

				BinaryOp base_op   = binaryop_assign_base_op(op);
				const char *op_str = "+";
				switch (base_op)
				{
					case BINARYOP_ADD: op_str = "+"; break;
					case BINARYOP_SUB: op_str = "-"; break;
					case BINARYOP_MULT: op_str = "*"; break;
					case BINARYOP_DIV: op_str = "/"; break;
					case BINARYOP_MOD: op_str = "%"; break;
					case BINARYOP_BIT_AND: op_str = "&"; break;
					case BINARYOP_BIT_OR: op_str = "|"; break;
					case BINARYOP_BIT_XOR: op_str = "^"; break;
					case BINARYOP_SHL: op_str = "<<"; break;
					case BINARYOP_SHR: op_str = ">>"; break;
					default: op_str = "+"; break;
				}

				if (!c_type_is_aggregate(member_type) && (base_op == BINARYOP_SHL || base_op == BINARYOP_SHR))
				{
					if (safe_mode_enabled())
					{
						if (type_is_signed(right_val.type))
						{
							PRINTF("if (___var_%d < 0 || (size_t)___var_%d >= sizeof(%s)*8) { __c3_abort(); }\n",
							       right_val.var, right_val.var, mname);
						}
						else
						{
							PRINTF("if ((size_t)___var_%d >= sizeof(%s)*8) { __c3_abort(); }\n",
							       right_val.var, mname);
						}
					}
				}

				int res_temp = c_emit_temp_var(c, &result_val, member_type);
				if (compiler.build.feature.trap_on_wrap && type_is_signed(member_type) &&
				    (base_op == BINARYOP_ADD || base_op == BINARYOP_SUB || base_op == BINARYOP_MULT))
				{
					const char *bname = (base_op == BINARYOP_ADD) ? "__builtin_add_overflow" : (base_op == BINARYOP_SUB) ? "__builtin_sub_overflow"
					                                                                                                     : "__builtin_mul_overflow";
					PRINTF("if (%s(___var_%d, ___var_%d, &___var_%d)) { __c3_abort(); }\n",
					       bname, cur_val.var, right_val.var, res_temp);
				}
				else
				{
					PRINTF("%s ___var_%d = (%s)(___var_%d %s ___var_%d);\n",
					       mname, res_temp, mname, cur_val.var, op_str, right_val.var);
				}
			}
			else
			{
				c_emit_expr(c, &result_val, right);
				c_value_rvalue(c, &result_val);
			}

			c_emit_bitstruct_write_container(c, left->access_resolved_expr.parent, bitstruct_decl, member, container_ptr_var, c_tname, &result_val);
			*value = result_val;
			return;
		}
		case EXPR_SUBSCRIPT_ADDR:
		{
			Type *target_type = (expr->type && is_valid_type_ptr(expr->type)) ? c_safe_type_lower(expr->type) : type_void;
			c_emit_lvalue_addr(c, expr, value, target_type);
			return;
		}
		case EXPR_PTR_ACCESS:
		{
			CValue inner_val = {0};
			c_emit_expr(c, &inner_val, expr->inner_expr);
			c_value_rvalue(c, &inner_val);
			c_ensure_cvalue_var(c, &inner_val, type_chars);

			Type *target_type = (expr->type && is_valid_type_ptr(expr->type)) ? c_safe_type_lower(expr->type) : type_void;
			if (target_type->type_kind == TYPE_VOID)
			{
				target_type = type_voidptr;
			}
			int temp = c_emit_temp_var(c, value, target_type);
			PRINTF("%s ___var_%d = (void*)___var_%d.ptr;\n", c_type_name(c, target_type), temp, inner_val.var);
			return;
		}
		case EXPR_SLICE_LEN:
		{
			CValue inner_val = {0};
			c_emit_expr(c, &inner_val, expr->inner_expr);
			c_value_rvalue(c, &inner_val);
			c_ensure_cvalue_var(c, &inner_val, type_chars);

			Type *target_type = (expr->type && is_valid_type_ptr(expr->type)) ? c_safe_type_lower(expr->type) : type_void;
			if (target_type->type_kind == TYPE_VOID)
			{
				target_type = type_sz;
			}
			int temp = c_emit_temp_var(c, value, target_type);
			PRINTF("%s ___var_%d = (size_t)___var_%d.len;\n", c_type_name(c, target_type), temp, inner_val.var);
			return;
		}
		case EXPR_SLICE:
		{
			Expr *parent_expr = exprptr(expr->slice_expr.expr);
			Type *parent_type = c_expr_type(parent_expr);
			if (parent_type->type_kind == TYPE_VOID)
			{
				parent_type = type_chars;
			}
			Type *target_type = c_expr_type(expr);
			Type *elem_type   = (target_type && target_type->type_kind == TYPE_SLICE && is_valid_type_ptr(target_type->array.base)) ? c_safe_type_lower(target_type->array.base) : type_void;
			if (elem_type->type_kind == TYPE_VOID)
			{
				if ((parent_type->type_kind == TYPE_SLICE || parent_type->type_kind == TYPE_ARRAY) && parent_type->array.base && is_valid_type_ptr(parent_type->array.base))
				{
					elem_type = c_safe_type_lower(parent_type->array.base);
				}
				else if (parent_type->type_kind == TYPE_POINTER && parent_type->pointer && is_valid_type_ptr(parent_type->pointer))
				{
					elem_type = c_safe_type_lower(parent_type->pointer);
				}
				else
				{
					elem_type = type_char;
				}
			}
			if (target_type->type_kind == TYPE_VOID)
			{
				target_type = type_is_valid_for_array(elem_type) ? type_get_slice(elem_type) : type_chars;
			}
			const char *elem_tname = c_type_name(c, elem_type);
			int temp               = c_emit_temp_var(c, value, target_type);
			const char *tname      = c_type_name(c, target_type);

			Range range      = expr->slice_expr.range;
			int len_var      = 0;
			int base_ptr_var = c_create_variable(c);

			if (parent_type->type_kind == TYPE_POINTER)
			{
				CValue parent_val = {0};
				c_emit_expr(c, &parent_val, parent_expr);
				c_value_rvalue(c, &parent_val);
				PRINTF("%s* ___var_%d = (%s*)___var_%d;\n", elem_tname, base_ptr_var, elem_tname, parent_val.var);
			}
			else if (parent_type->type_kind == TYPE_SLICE)
			{
				CValue parent_val = {0};
				c_emit_expr(c, &parent_val, parent_expr);
				c_value_rvalue(c, &parent_val);
				PRINTF("%s* ___var_%d = (%s*)___var_%d.ptr;\n", elem_tname, base_ptr_var, elem_tname, parent_val.var);
				len_var = c_create_variable(c);
				PRINTF("size_t ___var_%d = ___var_%d.len;\n", len_var, parent_val.var);
			}
			else
			{
				CValue parent_addr = {0};
				c_emit_lvalue_addr(c, parent_expr, &parent_addr, type_get_ptr(parent_type));
				PRINTF("%s* ___var_%d = (%s*)(((%s*)___var_%d)->ptr);\n",
				       elem_tname, base_ptr_var, elem_tname, c_type_name(c, parent_type), parent_addr.var);
				len_var = c_create_variable(c);
				PRINTF("size_t ___var_%d = %llu;\n", len_var, (unsigned long long)parent_type->array.len);
			}

			// 1. Resolve start_index
			int start_idx_var = c_create_variable(c);
			if (range.range_type == RANGE_CONST_RANGE)
			{
				PRINTF("size_t ___var_%d = %lld;\n", start_idx_var, (long long)range.start_index);
			}
			else
			{
				int raw_start_var = 0;
				if (range.start)
				{
					CValue start_val = {0};
					c_emit_expr(c, &start_val, exprptr(range.start));
					c_value_rvalue(c, &start_val);
					c_ensure_cvalue_var(c, &start_val, type_sz);
					raw_start_var = start_val.var;
				}
				if (range.start_from_end)
				{
					PRINTF("size_t ___var_%d = ___var_%d - ___var_%d;\n", start_idx_var, len_var, raw_start_var);
				}
				else if (raw_start_var)
				{
					PRINTF("size_t ___var_%d = ___var_%d;\n", start_idx_var, raw_start_var);
				}
				else
				{
					PRINTF("size_t ___var_%d = 0;\n", start_idx_var);
				}
			}

			// 2. Resolve end_index
			bool has_end    = (range.range_type != RANGE_DYNAMIC || range.end != 0);
			int end_idx_var = c_create_variable(c);
			if (has_end)
			{
				int raw_end_var = 0;
				if (range.range_type == RANGE_CONST_RANGE)
				{
					raw_end_var = c_create_variable(c);
					PRINTF("size_t ___var_%d = %lld;\n", raw_end_var, (long long)range.len_index);
				}
				else if (range.range_type == RANGE_CONST_LEN || range.range_type == RANGE_CONST_END)
				{
					raw_end_var = c_create_variable(c);
					PRINTF("size_t ___var_%d = %lld;\n", raw_end_var, (long long)range.const_end);
				}
				else if (range.end)
				{
					CValue end_val = {0};
					c_emit_expr(c, &end_val, exprptr(range.end));
					c_value_rvalue(c, &end_val);
					c_ensure_cvalue_var(c, &end_val, type_sz);
					raw_end_var = end_val.var;
				}

				if (range.end_from_end)
				{
					PRINTF("size_t ___var_%d = ___var_%d - ___var_%d;\n", end_idx_var, len_var, raw_end_var);
				}
				else
				{
					PRINTF("size_t ___var_%d = ___var_%d;\n", end_idx_var, raw_end_var);
				}

				if (range.is_len)
				{
					PRINTF("___var_%d += ___var_%d;\n", end_idx_var, start_idx_var);
				}
			}
			else
			{
				PRINTF("size_t ___var_%d = ___var_%d;\n", end_idx_var, len_var);
			}

			// 3. Compute slice_len
			int slice_len_var = c_create_variable(c);
			if (range.is_len || !has_end || range.range_type == RANGE_CONST_RANGE)
			{
				PRINTF("size_t ___var_%d = ___var_%d - ___var_%d;\n", slice_len_var, end_idx_var, start_idx_var);
			}
			else
			{
				PRINTF("size_t ___var_%d = (___var_%d - ___var_%d) + 1;\n", slice_len_var, end_idx_var, start_idx_var);
			}

			PRINTF("%s ___var_%d = (%s){ .ptr = ___var_%d + ___var_%d, .len = ___var_%d };\n",
			       tname, temp, tname, base_ptr_var, start_idx_var, slice_len_var);
			return;
		}
		case EXPR_MAKE_SLICE:
		{
			Type *target_type = (expr->type && is_valid_type_ptr(expr->type)) ? c_safe_type_lower(expr->type) : type_void;
			if (target_type->type_kind == TYPE_VOID)
			{
				target_type = type_chars;
			}
			int temp          = c_emit_temp_var(c, value, target_type);
			const char *tname = c_type_name(c, target_type);
			if (expr->make_slice_expr.ptr)
			{
				CValue ptr_val = {0};
				c_emit_expr(c, &ptr_val, expr->make_slice_expr.ptr);
				c_value_rvalue(c, &ptr_val);
				PRINTF("%s ___var_%d = (%s){ .ptr = (void*)___var_%d, .len = %llu };\n",
				       tname, temp, tname, ptr_val.var, (unsigned long long)expr->make_slice_expr.len);
			}
			else
			{
				PRINTF("%s ___var_%d = (%s){ .ptr = NULL, .len = 0 };\n", tname, temp, tname);
			}
			return;
		}
		case EXPR_SLICE_COPY:
		{
			CValue dst_val = {0}, src_val = {0};
			c_emit_expr(c, &dst_val, exprptr(expr->slice_assign_expr.left));
			c_value_rvalue(c, &dst_val);
			c_emit_expr(c, &src_val, exprptr(expr->slice_assign_expr.right));
			c_value_rvalue(c, &src_val);
			if (dst_val.var != 0 && src_val.var != 0)
			{
				PRINTF("__c3_memmove(___var_%d.ptr, ___var_%d.ptr, ___var_%d.len * sizeof(*___var_%d.ptr));\n",
				       dst_val.var, src_val.var, src_val.var, dst_val.var);
			}
			*value = dst_val;
			return;
		}
		case EXPR_SLICE_TO_VEC_ARRAY:
		{
			CValue inner_val = {0};
			c_emit_expr(c, &inner_val, expr->inner_expr);
			c_value_rvalue(c, &inner_val);
			Type *target_type = (expr->type && is_valid_type_ptr(expr->type)) ? c_safe_type_lower(expr->type) : type_void;
			if (target_type->type_kind == TYPE_VOID)
			{
				target_type = type_int;
			}
			int temp          = c_emit_temp_var(c, value, target_type);
			const char *tname = c_type_name(c, target_type);
			PRINTF("%s ___var_%d;\n", tname, temp);
			PRINTF("__c3_memcpy(&___var_%d, (void*)___var_%d.ptr, sizeof(%s));\n", temp, inner_val.var, tname);
			return;
		}
		case EXPR_SCALAR_TO_VECTOR:
		{
			CValue inner_val = {0};
			c_emit_expr(c, &inner_val, expr->inner_expr);
			c_value_rvalue(c, &inner_val);
			Type *target_type = (expr->type && is_valid_type_ptr(expr->type)) ? c_safe_type_lower(expr->type) : type_void;
			if (target_type->type_kind == TYPE_VOID)
			{
				target_type = type_int;
			}
			int temp          = c_emit_temp_var(c, value, target_type);
			const char *tname = c_type_name(c, target_type);
			int len           = (int)target_type->array.len;
			if (len <= 0)
			{
				len = 1;
			}
			PRINTF("%s ___var_%d;\n", tname, temp);
			for (int k = 0; k < len; k++)
			{
				PRINTF("___var_%d.ptr[%d] = ___var_%d;\n", temp, k, inner_val.var);
			}
			return;
		}
		case EXPR_VECTOR_FROM_ARRAY:
		case EXPR_VECTOR_TO_ARRAY:
		case EXPR_RECAST:
		case EXPR_ADDR_CONVERSION:
		case EXPR_INT_TO_PTR:
		case EXPR_PTR_TO_INT:
		case EXPR_INT_TO_FLOAT:
		case EXPR_FLOAT_TO_INT:
		case EXPR_EXT_TRUNC:
		case EXPR_ENUM_FROM_ORD:
		{
			CValue inner_val = {0};
			c_emit_expr(c, &inner_val, expr->inner_expr);
			c_value_rvalue(c, &inner_val);
			Type *target_type = (expr->type && is_valid_type_ptr(expr->type)) ? c_safe_type_lower(expr->type) : type_void;
			if (target_type->type_kind == TYPE_VOID)
			{
				target_type = type_int;
			}
			int temp          = c_emit_temp_var(c, value, target_type);
			const char *tname = c_type_name(c, target_type);

			if (inner_val.var == 0)
			{
				c_emit_var_zero_init(c, temp, target_type);
				return;
			}

			if (inner_val.type && (inner_val.type->type_kind == TYPE_ANY || inner_val.type->type_kind == TYPE_INTERFACE))
			{
				if (target_type->type_kind == TYPE_POINTER)
				{
					PRINTF("%s ___var_%d = (%s)___var_%d.ptr;\n", tname, temp, tname, inner_val.var);
				}
				else if (type_is_integer(target_type))
				{
					PRINTF("%s ___var_%d = (%s)(uintptr_t)___var_%d.ptr;\n", tname, temp, tname, inner_val.var);
				}
				else if (target_type->type_kind == TYPE_ANY || target_type->type_kind == TYPE_INTERFACE)
				{
					PRINTF("%s ___var_%d = ___var_%d;\n", tname, temp, inner_val.var);
				}
				else if (c_type_is_aggregate(target_type))
				{
					PRINTF("%s ___var_%d;\n", tname, temp);
					PRINTF("__c3_memcpy(&___var_%d, (void*)___var_%d.ptr, sizeof(%s));\n", temp, inner_val.var, tname);
				}
				else
				{
					PRINTF("%s ___var_%d = (%s)(uintptr_t)___var_%d.ptr;\n", tname, temp, tname, inner_val.var);
				}
				return;
			}

			if (target_type->type_kind == TYPE_ANY || target_type->type_kind == TYPE_INTERFACE)
			{
				char target_slot[64];
				snprintf(target_slot, sizeof(target_slot), "___var_%d", temp);
				c_emit_assign_to_any(c, target_slot, &inner_val);
				return;
			}

			if (c_type_is_aggregate(target_type))
			{
				if (inner_val.type && (inner_val.type->type_kind == TYPE_ARRAY || inner_val.type->type_kind == TYPE_VECTOR || inner_val.type->type_kind == TYPE_SIMD_VECTOR) && (target_type->type_kind == TYPE_ARRAY || target_type->type_kind == TYPE_VECTOR || target_type->type_kind == TYPE_SIMD_VECTOR))
				{
					int t_len       = (int)target_type->array.len;
					int i_len       = (int)inner_val.type->array.len;
					Type *t_base    = target_type->array.base ? c_safe_type_lower(target_type->array.base) : type_void;
					Type *i_base    = inner_val.type->array.base ? c_safe_type_lower(inner_val.type->array.base) : type_void;
					bool is_bitcast = (expr->expr_kind == EXPR_RECAST);
					if (t_len == i_len && (t_base->canonical == i_base->canonical || (is_bitcast && type_size(t_base) == type_size(i_base))))
					{
						PRINTF("%s ___var_%d;\n", tname, temp);
						PRINTF("__c3_memcpy(&___var_%d, &___var_%d, sizeof(%s));\n", temp, inner_val.var, tname);
					}
					else
					{
						int min_len            = t_len < i_len ? t_len : i_len;
						const char *base_tname = c_type_name(c, t_base);
						PRINTF("%s ___var_%d = {0};\n", tname, temp);
						for (int k = 0; k < min_len; k++)
						{
							if (i_base->type_kind == TYPE_BOOL && type_is_integer(t_base))
							{
								PRINTF("___var_%d.ptr[%d] = ___var_%d.ptr[%d] ? (%s)-1 : 0;\n", temp, k, inner_val.var, k, base_tname);
							}
							else if (i_base->type_kind == TYPE_BOOL && type_is_float(t_base))
							{
								PRINTF("___var_%d.ptr[%d] = ___var_%d.ptr[%d] ? 1.0 : 0.0;\n", temp, k, inner_val.var, k);
							}
							else if (t_base->type_kind == TYPE_BOOL)
							{
								PRINTF("___var_%d.ptr[%d] = (___var_%d.ptr[%d] != 0);\n", temp, k, inner_val.var, k);
							}
							else
							{
								PRINTF("___var_%d.ptr[%d] = (%s)___var_%d.ptr[%d];\n", temp, k, base_tname, inner_val.var, k);
							}
						}
					}
					return;
				}
				if (target_type->type_kind == TYPE_SLICE && inner_val.type && (inner_val.type->type_kind == TYPE_ARRAY || inner_val.type->type_kind == TYPE_VECTOR || inner_val.type->type_kind == TYPE_SIMD_VECTOR))
				{
					PRINTF("%s ___var_%d = (%s){ .ptr = (void*)(___var_%d%sptr), .len = %llu };\n",
					       tname, temp, tname, inner_val.var, c_arrow(&inner_val), (unsigned long long)inner_val.type->array.len);
					return;
				}
				if (inner_val.type && !c_type_is_aggregate(inner_val.type))
				{
					PRINTF("%s ___var_%d = {0};\n", tname, temp);
					if (type_is_pointer(inner_val.type))
					{
						PRINTF("__c3_memcpy(&___var_%d, (void*)___var_%d, sizeof(%s));\n", temp, inner_val.var, tname);
					}
					else
					{
						PRINTF("__c3_memcpy(&___var_%d, &___var_%d, sizeof(___var_%d));\n", temp, inner_val.var, inner_val.var);
					}
					return;
				}
				PRINTF("%s ___var_%d;\n", tname, temp);
				PRINTF("__c3_memcpy(&___var_%d, &___var_%d, sizeof(%s));\n", temp, inner_val.var, tname);
			}
			else if (target_type->type_kind == TYPE_POINTER && inner_val.type && (inner_val.type->type_kind == TYPE_ARRAY || inner_val.type->type_kind == TYPE_VECTOR || inner_val.type->type_kind == TYPE_SIMD_VECTOR))
			{
				PRINTF("%s ___var_%d = (%s)(___var_%d%sptr);\n", tname, temp, tname, inner_val.var, c_arrow(&inner_val));
				return;
			}
			else if (type_is_pointer(target_type) && inner_val.type && type_is_integer(inner_val.type))
			{
				PRINTF("%s ___var_%d = (%s)(uintptr_t)___var_%d;\n", tname, temp, tname, inner_val.var);
			}
			else if (type_is_integer(target_type) && inner_val.type && type_is_pointer(inner_val.type))
			{
				PRINTF("%s ___var_%d = (%s)(uintptr_t)___var_%d;\n", tname, temp, tname, inner_val.var);
			}
			else if (target_type->type_kind == TYPE_ANYFAULT || (inner_val.type && inner_val.type->type_kind == TYPE_ANYFAULT))
			{
				if (type_is_pointer(target_type))
				{
					PRINTF("%s ___var_%d = (c3fault_t)(uintptr_t)___var_%d;\n", tname, temp, inner_val.var);
				}
				else
				{
					PRINTF("%s ___var_%d = (%s)(uintptr_t)___var_%d;\n", tname, temp, tname, inner_val.var);
				}
			}
			else if (type_is_float(target_type) && (inner_val.type && (type_is_pointer(inner_val.type) || inner_val.type->type_kind == TYPE_ANYFAULT || inner_val.type->type_kind == TYPE_TYPEID)))
			{
				PRINTF("%s ___var_%d = 0.0;\n", tname, temp);
			}
			else if ((type_is_pointer(target_type) || target_type->type_kind == TYPE_ANYFAULT || target_type->type_kind == TYPE_TYPEID) && type_is_float(inner_val.type))
			{
				PRINTF("%s ___var_%d = NULL;\n", tname, temp);
			}
			else if (type_is_pointer(target_type) && inner_val.type && type_is_pointer(inner_val.type))
			{
				PRINTF("%s ___var_%d = (%s)___var_%d;\n", tname, temp, tname, inner_val.var);
			}
			else
			{
				PRINTF("%s ___var_%d = (%s)___var_%d;\n", tname, temp, tname, inner_val.var);
			}
			return;
		}
		case EXPR_INT_TO_BOOL:
		{
			CValue inner_val = {0};
			c_emit_expr(c, &inner_val, expr->int_to_bool_expr.inner);
			c_value_rvalue(c, &inner_val);
			int temp = c_emit_temp_var(c, value, type_bool);
			if (inner_val.var == 0)
			{
				PRINTF("bool ___var_%d = %s;\n", temp, expr->int_to_bool_expr.negate ? "true" : "false");
			}
			else if (inner_val.type && inner_val.type->type_kind == TYPE_SLICE)
			{
				PRINTF("bool ___var_%d = (___var_%d.len %s 0);\n", temp, inner_val.var, expr->int_to_bool_expr.negate ? "==" : "!=");
			}
			else if (inner_val.type && (inner_val.type->type_kind == TYPE_ANY || inner_val.type->type_kind == TYPE_INTERFACE))
			{
				PRINTF("bool ___var_%d = (___var_%d.ptr %s NULL);\n", temp, inner_val.var, expr->int_to_bool_expr.negate ? "==" : "!=");
			}
			else if (inner_val.type && (type_is_pointer(inner_val.type) || inner_val.type->type_kind == TYPE_FUNC_PTR))
			{
				PRINTF("bool ___var_%d = (___var_%d %s NULL);\n", temp, inner_val.var, expr->int_to_bool_expr.negate ? "==" : "!=");
			}
			else
			{
				PRINTF("bool ___var_%d = (___var_%d %s 0);\n", temp, inner_val.var, expr->int_to_bool_expr.negate ? "==" : "!=");
			}
			return;
		}
		case EXPR_MAKE_ANY:
		{
			CValue inner_val = {0}, typeid_val = {0};
			c_emit_expr(c, &inner_val, expr->make_any_expr.inner);
			c_value_rvalue(c, &inner_val);
			c_emit_expr(c, &typeid_val, expr->make_any_expr.typeid);
			c_value_rvalue(c, &typeid_val);
			int temp = c_emit_temp_var(c, value, type_any);
			PRINTF("__c3_any__ ___var_%d = (__c3_any__){ .ptr = (void*)___var_%d, .typeid = (c3typeid_t)___var_%d };\n", temp, inner_val.var, typeid_val.var);
			return;
		}
		case EXPR_TERNARY:
		{
			CValue cond_val = {0};
			c_emit_expr(c, &cond_val, exprptr(expr->ternary_expr.cond));
			c_value_rvalue(c, &cond_val);
			Type *target_type = (expr->type && is_valid_type_ptr(expr->type)) ? c_safe_type_lower(expr->type) : type_void;
			if (target_type->type_kind == TYPE_VOID && expr->ternary_expr.then_expr && exprptr(expr->ternary_expr.then_expr)->type)
			{
				target_type = c_safe_type_lower(exprptr(expr->ternary_expr.then_expr)->type);
			}
			if (target_type->type_kind == TYPE_VOID && expr->ternary_expr.else_expr && exprptr(expr->ternary_expr.else_expr)->type)
			{
				target_type = c_safe_type_lower(exprptr(expr->ternary_expr.else_expr)->type);
			}

			if (target_type->type_kind == TYPE_VOID)
			{
				if (cond_val.var != 0)
				{
					PRINTF("if (___var_%d) {\n", cond_val.var);
				}
				else
				{
					PRINT("if (1) {\n");
				}
				if (expr->ternary_expr.then_expr)
				{
					c_emit_ignored_expr(c, exprptr(expr->ternary_expr.then_expr));
				}
				PRINT("} else {\n");
				if (expr->ternary_expr.else_expr)
				{
					c_emit_ignored_expr(c, exprptr(expr->ternary_expr.else_expr));
				}
				PRINT("}\n");
				value->var  = 0;
				value->type = type_void;
				value->kind = CV_VALUE;
				return;
			}

			int temp = c_emit_temp_var(c, value, target_type);
			c_emit_var_zero_init(c, temp, target_type);

			if (cond_val.var != 0)
			{
				PRINTF("if (___var_%d) {\n", cond_val.var);
			}
			else
			{
				PRINT("if (1) {\n");
			}
			if (expr->ternary_expr.then_expr)
			{
				CValue then_val = {0};
				c_emit_expr(c, &then_val, exprptr(expr->ternary_expr.then_expr));
				c_value_rvalue(c, &then_val);
				c_emit_assign_var(c, temp, target_type, &then_val);
			}
			else
			{
				c_emit_assign_var(c, temp, target_type, &cond_val);
			}
			PRINT("} else {\n");
			if (expr->ternary_expr.else_expr)
			{
				CValue else_val = {0};
				c_emit_expr(c, &else_val, exprptr(expr->ternary_expr.else_expr));
				c_value_rvalue(c, &else_val);
				c_emit_assign_var(c, temp, target_type, &else_val);
			}
			PRINT("}\n");
			return;
		}
		case EXPR_INITIALIZER_LIST:
		{
			Type *raw_type    = expr->type ? type_flatten(expr->type) : NULL;
			Type *target_type = (expr->type && is_valid_type_ptr(expr->type)) ? c_safe_type_lower(expr->type) : type_void;
			if (target_type->type_kind == TYPE_VOID)
			{
				target_type = type_int;
			}
			int temp          = c_emit_temp_var(c, value, target_type);
			const char *tname = c_type_name(c, target_type);
			PRINTF("%s ___var_%d = {0};\n", tname, temp);
			Expr **elements = expr->initializer_list;
			int count       = vec_size(elements);
			if (raw_type && raw_type->type_kind == TYPE_BITSTRUCT)
			{
				Decl *d             = raw_type->decl;
				Decl **members      = d ? d->strukt.members : NULL;
				int member_count    = vec_size(members);
				const char *bs_name = c_get_decl_name(d);
				for (int i = 0; i < count && i < member_count; i++)
				{
					Decl *m       = members[i];
					CValue el_val = {0};
					c_emit_expr(c, &el_val, elements[i]);
					c_value_rvalue(c, &el_val);
					c_ensure_cvalue_var(c, &el_val, type_uint);
					const char *m_name = (m && m->name) ? m->name : "_anon";
					PRINTF("%s_set_%s((void*)&___var_%d, ___var_%d);\n",
					       bs_name, m_name, temp, el_val.var);
				}
				return;
			}
			if (target_type->type_kind == TYPE_ARRAY || target_type->type_kind == TYPE_VECTOR || target_type->type_kind == TYPE_SIMD_VECTOR)
			{
				for (int i = 0; i < count; i++)
				{
					CValue el_val = {0};
					c_emit_expr(c, &el_val, elements[i]);
					c_value_rvalue(c, &el_val);
					c_ensure_cvalue_var(c, &el_val, elements[i]->type);
					if (c_type_is_aggregate(el_val.type))
					{
						PRINTF("__c3_memcpy(&___var_%d.ptr[%d], &___var_%d, sizeof(%s));\n", temp, i, el_val.var, c_type_name(c, el_val.type));
					}
					else
					{
						PRINTF("___var_%d.ptr[%d] = ___var_%d;\n", temp, i, el_val.var);
					}
				}
			}
			else if (target_type->type_kind == TYPE_SLICE)
			{
				Type *base_type        = (target_type->array.base && is_valid_type_ptr(target_type->array.base)) ? c_safe_type_lower(target_type->array.base) : type_void;
				const char *base_tname = c_type_name(c, base_type);
				int arr_var            = c_create_variable(c);
				PRINTF("%s ___var_%d[%d];\n", base_tname, arr_var, count > 0 ? count : 1);
				for (int i = 0; i < count; i++)
				{
					CValue el_val = {0};
					c_emit_expr(c, &el_val, elements[i]);
					c_value_rvalue(c, &el_val);
					c_ensure_cvalue_var(c, &el_val, base_type);
					if (c_type_is_aggregate(el_val.type))
					{
						PRINTF("__c3_memcpy(&___var_%d[%d], &___var_%d, sizeof(%s));\n", arr_var, i, el_val.var, base_tname);
					}
					else
					{
						PRINTF("___var_%d[%d] = ___var_%d;\n", arr_var, i, el_val.var);
					}
				}
				PRINTF("___var_%d = (%s){ .ptr = ___var_%d, .len = %d };\n", temp, tname, arr_var, count);
			}
			else
			{
				Decl *d = target_type->decl;
				for (int i = 0; i < count; i++)
				{
					CValue el_val = {0};
					c_emit_expr(c, &el_val, elements[i]);
					c_value_rvalue(c, &el_val);
					c_ensure_cvalue_var(c, &el_val, elements[i]->type);
					Decl *m  = (d && d->strukt.members && i < vec_size(d->strukt.members)) ? d->strukt.members[i] : NULL;
					Type *mt = m ? c_decl_type(m) : el_val.type;
					if (c_type_is_aggregate(el_val.type))
					{
						PRINTF("__c3_memcpy(&___var_%d.m%d, &___var_%d, sizeof(%s));\n", temp, i, el_val.var, c_type_name(c, el_val.type));
					}
					else if (mt && mt->type_kind == TYPE_POINTER)
					{
						PRINTF("___var_%d.m%d = (%s)___var_%d;\n", temp, i, c_type_name(c, mt), el_val.var);
					}
					else
					{
						PRINTF("___var_%d.m%d = ___var_%d;\n", temp, i, el_val.var);
					}
				}
			}
			return;
		}
		case EXPR_DESIGNATED_INITIALIZER_LIST:
		{
			Type *raw_type    = expr->type ? type_flatten(expr->type) : NULL;
			Type *target_type = (expr->type && is_valid_type_ptr(expr->type)) ? c_safe_type_lower(expr->type) : type_void;
			if (target_type->type_kind == TYPE_VOID)
			{
				target_type = type_int;
			}
			int temp          = c_emit_temp_var(c, value, target_type);
			const char *tname = c_type_name(c, target_type);
			if (expr->designated_init.splat)
			{
				CValue splat_val = {0};
				c_emit_expr(c, &splat_val, expr->designated_init.splat);
				c_value_rvalue(c, &splat_val);
				c_emit_assign_var(c, temp, target_type, &splat_val);
			}
			else
			{
				PRINTF("%s ___var_%d = {0};\n", tname, temp);
			}
			Expr **elements = expr->designated_init.list;
			if (raw_type && raw_type->type_kind == TYPE_BITSTRUCT)
			{
				Decl *d             = raw_type->decl;
				Decl **members      = d ? d->strukt.members : NULL;
				int member_count    = vec_size(members);
				const char *bs_name = c_get_decl_name(d);
				FOREACH(Expr *, des, elements)
				{
					if (des->expr_kind == EXPR_DESIGNATOR && vec_size(des->designator_expr.path))
					{
						DesignatorElement *de = des->designator_expr.path[0];
						int m_idx             = (int)de->index;
						Decl *m               = (m_idx < member_count) ? members[m_idx] : NULL;
						if (!m)
						{
							continue;
						}
						CValue dval = {0};
						c_emit_expr(c, &dval, des->designator_expr.value);
						c_value_rvalue(c, &dval);
						const char *m_name = m->name ? m->name : "_anon";
						PRINTF("%s_set_%s((void*)&___var_%d, ___var_%d);\n",
						       bs_name, m_name, temp, dval.var);
					}
				}
				return;
			}
			FOREACH(Expr *, des, elements)
			{
				if (des->expr_kind == EXPR_DESIGNATOR && vec_size(des->designator_expr.path))
				{
					CValue dval = {0};
					c_emit_expr(c, &dval, des->designator_expr.value);
					c_value_rvalue(c, &dval);
					c_ensure_cvalue_var(c, &dval, des->designator_expr.value->type);
					int path_len           = vec_size(des->designator_expr.path);
					Type *curr_type        = target_type;
					bool is_bitfield       = false;
					Decl *bitstruct_decl   = NULL;
					Decl *bitstruct_member = NULL;

					char path_buf[256];
					path_buf[0] = '\0';
					for (int p = 0; p < path_len; p++)
					{
						DesignatorElement *de = des->designator_expr.path[p];
						if (curr_type->type_kind == TYPE_STRUCT || curr_type->type_kind == TYPE_UNION)
						{
							Decl *d = curr_type->decl;
							Decl *m = (d && d->strukt.members && de->index < vec_size(d->strukt.members)) ? d->strukt.members[de->index] : NULL;
							char seg[32];
							snprintf(seg, sizeof(seg), ".m%d", (int)de->index);
							strncat(path_buf, seg, sizeof(path_buf) - strlen(path_buf) - 1);
							if (m && is_valid_type_ptr(m->type))
							{
								Type *raw_m = type_flatten(m->type);
								if (raw_m && raw_m->type_kind == TYPE_BITSTRUCT && p + 1 < path_len)
								{
									DesignatorElement *next_de = des->designator_expr.path[p + 1];
									if (raw_m->decl && raw_m->decl->strukt.members && next_de->index < vec_size(raw_m->decl->strukt.members))
									{
										Decl *bm         = raw_m->decl->strukt.members[next_de->index];
										bitstruct_decl   = raw_m->decl;
										bitstruct_member = bm;
										is_bitfield      = true;
										p++;
									}
									curr_type = c_safe_type_lower(m->type);
									break;
								}
								curr_type = c_safe_type_lower(m->type);
							}
							else
							{
								curr_type = type_void;
							}
						}
						else if (curr_type->type_kind == TYPE_ARRAY || curr_type->type_kind == TYPE_VECTOR || curr_type->type_kind == TYPE_SIMD_VECTOR)
						{
							char seg[32];
							snprintf(seg, sizeof(seg), ".ptr[%d]", (int)de->index);
							strncat(path_buf, seg, sizeof(path_buf) - strlen(path_buf) - 1);
							curr_type = curr_type->array.base ? c_safe_type_lower(curr_type->array.base) : type_void;
						}
						else
						{
							break;
						}
					}

					if (is_bitfield && bitstruct_decl && bitstruct_member)
					{
						const char *bs_name = c_get_decl_name(bitstruct_decl);
						const char *m_name  = bitstruct_member->name ? bitstruct_member->name : "_anon";
						PRINTF("%s_set_%s((void*)&___var_%d%s, ___var_%d);\n",
						       bs_name, m_name, temp, path_buf, dval.var);
					}
					else if (curr_type && curr_type->type_kind == TYPE_POINTER && dval.type && c_type_is_aggregate(dval.type))
					{
						if (dval.type->type_kind == TYPE_ARRAY || dval.type->type_kind == TYPE_VECTOR || dval.type->type_kind == TYPE_SIMD_VECTOR)
						{
							PRINTF("___var_%d%s = (void*)(___var_%d%sptr);\n", temp, path_buf, dval.var, c_arrow(&dval));
						}
						else
						{
							PRINTF("___var_%d%s = (void*)(&___var_%d);\n", temp, path_buf, dval.var);
						}
					}
					else if (curr_type && curr_type->type_kind == TYPE_POINTER && dval.type && type_is_pointer(dval.type))
					{
						PRINTF("___var_%d%s = (%s)___var_%d;\n", temp, path_buf, c_type_name(c, curr_type), dval.var);
					}
					else if (c_type_is_aggregate(dval.type))
					{
						PRINTF("__c3_memcpy(&___var_%d%s, &___var_%d, sizeof(%s));\n", temp, path_buf, dval.var, c_type_name(c, dval.type));
					}
					else
					{
						if (curr_type && curr_type->type_kind == TYPE_POINTER)
						{
							PRINTF("___var_%d%s = (%s)___var_%d;\n", temp, path_buf, c_type_name(c, curr_type), dval.var);
						}
						else
						{
							PRINTF("___var_%d%s = ___var_%d;\n", temp, path_buf, dval.var);
						}
					}
				}
			}
			return;
		}
		case EXPR_DEFAULT_ARG:
			c_emit_expr(c, value, expr->default_arg_expr.inner);
			return;
		case EXPR_DISCARD:
			c_emit_ignored_expr(c, expr->inner_expr);
			value->var  = 0;
			value->type = type_void;
			value->kind = CV_VALUE;
			return;
		case EXPR_RVALUE:
		case EXPR_MAYBE_DEREF:
		case EXPR_SPLAT:
			c_emit_expr(c, value, expr->inner_expr);
			return;
		case EXPR_OTHER_CONTEXT:
			c_emit_expr(c, value, expr->expr_other_context.inner);
			return;
		case EXPR_CONTRACT:
			if (expr->contract_expr.decl_exprs)
			{
				c_emit_ignored_expr(c, expr->contract_expr.decl_exprs);
			}
			value->var  = 0;
			value->type = type_void;
			value->kind = CV_VALUE;
			return;
		case EXPR_COMPILER_CONST:
		{
			Type *target_type = (expr->type && is_valid_type_ptr(expr->type)) ? c_safe_type_lower(expr->type) : type_void;
			if (target_type->type_kind == TYPE_VOID)
			{
				target_type = type_int;
			}
			int temp = c_emit_temp_var(c, value, target_type);
			PRINTF("%s ___var_%d = {0};\n", c_type_name(c, target_type), temp);
			return;
		}
		case EXPR_BUILTIN_ACCESS:
		{
			Expr *inner      = exprptr(expr->builtin_access_expr.inner);
			CValue inner_val = {0};
			c_emit_expr(c, &inner_val, inner);
			c_value_rvalue(c, &inner_val);
			Type *target_type = (expr->type && is_valid_type_ptr(expr->type)) ? c_safe_type_lower(expr->type) : type_void;
			if (target_type->type_kind == TYPE_VOID)
			{
				target_type = type_chars;
			}
			int temp          = c_emit_temp_var(c, value, target_type);
			const char *tname = c_type_name(c, target_type);
			switch (expr->builtin_access_expr.kind)
			{
				case ACCESS_FAULTNAME:
				{
					const char *ptr_expr = inner_val.var != 0 ? str_printf("___var_%d", inner_val.var) : "NULL";
					if (inner_val.type && (inner_val.type->type_kind == TYPE_ANY || inner_val.type->type_kind == TYPE_INTERFACE))
					{
						ptr_expr = str_printf("___var_%d.ptr", inner_val.var);
					}
					if (target_type->type_kind == TYPE_SLICE)
					{
						PRINTF("%s ___var_%d = (%s != NULL) ? (%s){ .ptr = (void*)(uintptr_t)%s, .len = __builtin_strlen((const char*)(uintptr_t)%s) } : (%s){ .ptr = NULL, .len = 0 };\n",
						       tname, temp, ptr_expr, tname, ptr_expr, ptr_expr, tname);
					}
					else
					{
						PRINTF("%s ___var_%d = (%s)(uintptr_t)%s;\n", tname, temp, tname, ptr_expr);
					}
					return;
				}
				case ACCESS_ENUMNAME:
				{
					Type *inner_type = type_no_optional(inner->type)->canonical;
					const char *sym  = c_typeid_name(inner_type);
					if (target_type->type_kind == TYPE_SLICE)
					{
						PRINTF("%s ___var_%d = (%s){ .ptr = (%s.names && (size_t)___var_%d < %s.len) ? (uint8_t*)%s.names[(size_t)___var_%d].ptr : (uint8_t*)\"\", .len = (%s.names && (size_t)___var_%d < %s.len) ? %s.names[(size_t)___var_%d].len : 0 };\n",
						       tname, temp, tname, sym, inner_val.var, sym, sym, inner_val.var, sym, inner_val.var, sym, sym, inner_val.var);
					}
					else
					{
						PRINTF("%s ___var_%d = (%s)((%s.names && (size_t)___var_%d < %s.len) ? %s.names[(size_t)___var_%d].ptr : \"\");\n",
						       tname, temp, tname, sym, inner_val.var, sym, sym, inner_val.var);
					}
					return;
				}
				case ACCESS_TYPEOFANYFAULT:
				case ACCESS_TYPEOFANY:
					if (inner_val.type && (inner_val.type->type_kind == TYPE_ANY || inner_val.type->type_kind == TYPE_INTERFACE))
					{
						PRINTF("%s ___var_%d = (c3typeid_t)___var_%d.typeid;\n", tname, temp, inner_val.var);
					}
					else
					{
						PRINTF("%s ___var_%d = NULL;\n", tname, temp);
					}
					return;
				default:
					c_emit_var_zero_init(c, temp, target_type);
					return;
			}
		}
		case EXPR_BENCHMARK_HOOK:
		case EXPR_TEST_HOOK:
		{
			BuiltinDefine hook = (expr->expr_kind == EXPR_TEST_HOOK) ? expr->test_hook_expr : expr->benchmark_hook_expr;
			bool is_fns        = (hook == BUILTIN_DEF_TEST_FNS || hook == BUILTIN_DEF_BENCHMARK_FNS);
			Type *target_type  = (expr->type && is_valid_type_ptr(expr->type)) ? c_safe_type_lower(expr->type) : type_void;
			if (target_type->type_kind == TYPE_VOID)
			{
				target_type = type_chars;
			}
			c_emit_type_forward_decl(c, target_type);
			const char *tname = c_type_name(c, target_type);

			Decl **items = NULL;
			bool is_test = (hook == BUILTIN_DEF_TEST_FNS || hook == BUILTIN_DEF_TEST_NAMES);
			bool active  = is_test ? compiler.build.build_test : compiler.build.build_benchmark;
			if (active)
			{
				FOREACH(Module *, mod, compiler.context.module_list)
				{
					if (!mod)
					{
						continue;
					}
					Decl **list = is_test ? mod->tests : mod->benchmarks;
					FOREACH(Decl *, d, list)
					{
						if (d && d->decl_kind == DECL_FUNC && !c_decl_vec_contains(items, d))
						{
							vec_add(items, d);
						}
					}
				}
				if (vec_size(items) == 0)
				{
					FOREACH(Module *, mod, compiler.context.module_list)
					{
						if (!mod)
						{
							continue;
						}
						FOREACH(CompilationUnit *, unit, mod->units)
						{
							if (!unit)
							{
								continue;
							}
							FOREACH(Decl *, d, unit->functions)
							{
								if (!d || d->decl_kind != DECL_FUNC)
								{
									continue;
								}
								bool match = is_test ? d->func_decl.attr_test : d->func_decl.attr_benchmark;
								if (match && !c_decl_vec_contains(items, d))
								{
									vec_add(items, d);
								}
							}
						}
					}
				}
			}

			int count = vec_size(items);
			int temp  = c_emit_temp_var(c, value, target_type);
			if (count == 0)
			{
				PRINTF("%s ___var_%d = (%s){ .ptr = NULL, .len = 0 };\n", tname, temp, tname);
				return;
			}

			Type *elem_t = (target_type->type_kind == TYPE_SLICE && target_type->array.base)
			                   ? c_safe_type_lower(target_type->array.base)
			                   : type_voidptr;
			c_emit_type_forward_decl(c, elem_t);
			const char *elem_tname = c_type_name(c, elem_t);

			int arr_var = c_create_variable(c);
			if (is_fns)
			{
				FOREACH(Decl *, d, items)
				{
					c_emit_function_decl(c, d, false);
				}
				PRINTF("static const %s ___var_%d[%d] = {\n", elem_tname, arr_var, count);
				FOREACH(Decl *, d, items)
				{
					const char *fn_name = c_get_decl_name(d);
					PRINTF("\t(%s)&%s,\n", elem_tname, fn_name);
				}
				PRINT("};\n");
			}
			else
			{
				PRINTF("static const %s ___var_%d[%d] = {\n", elem_tname, arr_var, count);
				FOREACH(Decl *, d, items)
				{
					const char *mod_name  = (d->unit && d->unit->module && d->unit->module->name && d->unit->module->name->module)
					                            ? d->unit->module->name->module
					                            : NULL;
					const char *test_name = (mod_name && strlen(mod_name) > 0)
					                            ? str_printf("%s::%s", mod_name, d->name)
					                            : (d->name ? d->name : "test");
					size_t name_len       = strlen(test_name);
					PRINTF("\t{ .ptr = (void*)\"%s\", .len = %zu },\n", test_name, name_len);
				}
				PRINT("};\n");
			}
			PRINTF("%s ___var_%d = (%s){ .ptr = (void*)___var_%d, .len = %d };\n",
			       tname, temp, tname, arr_var, count);
			return;
		}
		case EXPR_NOP:
			value->var  = 0;
			value->type = type_void;
			value->kind = CV_VALUE;
			return;
		case EXPR_MACRO_BLOCK:
		{
			Type *target_type = (expr->type && is_valid_type_ptr(expr->type)) ? c_safe_type_lower(expr->type) : type_void;
			bool is_opt       = expr->type && type_is_optional(expr->type);
			int ret_var       = 0;
			if (target_type->type_kind != TYPE_VOID)
			{
				ret_var = c_emit_temp_var(c, value, target_type);
				c_emit_var_zero_init(c, ret_var, target_type);
			}
			else
			{
				value->var  = 0;
				value->type = type_void;
				value->kind = CV_VALUE;
			}

			int fault_var = 0;
			if (is_opt)
			{
				fault_var = c_emit_temp_var(c, NULL, type_fault);
				PRINTF("c3fault_t ___var_%d = NULL;\n", fault_var);
				value->optional = fault_var;
			}

			int old_macro_ret           = c->current_macro_ret_var;
			int old_macro_fault         = c->current_macro_fault_var;
			Type *old_macro_ret_type    = c->current_macro_ret_type;
			int old_macro_exit          = c->current_macro_exit_label;
			int macro_exit              = c_create_label(c);
			c->current_macro_ret_var    = ret_var;
			c->current_macro_fault_var  = fault_var;
			c->current_macro_ret_type   = target_type;
			c->current_macro_exit_label = macro_exit;

			BlockExit c_exit = {.block_return_out    = (void *)(uintptr_t)ret_var,
			                    .block_return_exit   = (void *)(uintptr_t)macro_exit,
			                    .block_error_var     = (void *)(uintptr_t)fault_var,
			                    .block_optional_exit = (void *)target_type};
			if (expr->macro_block.block_exit)
			{
				*expr->macro_block.block_exit = &c_exit;
			}

			FOREACH(Decl *, param, expr->macro_block.params)
			{
				if (!param)
				{
					continue;
				}
				if (param->decl_kind == DECL_DECLARRAY)
				{
					FOREACH(Decl *, d, param->decls)
					{
						if (!d || d->decl_kind != DECL_VAR)
						{
							continue;
						}
						if (d->var.kind == VARDECL_PARAM_CT || d->var.kind == VARDECL_PARAM_CT_TYPE || d->var.kind == VARDECL_PARAM_EXPR)
						{
							continue;
						}
						CValue pval = {0};
						c_emit_local_decl(c, d, &pval);
						if (expr->macro_block.had_optional_arg)
						{
							PRINTF("if (__c3_current_fault != NULL) {\n");
							if (fault_var != 0)
							{
								PRINTF("\t___var_%d = __c3_current_fault;\n", fault_var);
							}
							PRINTF("\tgoto __C3_LABEL_%d;\n}\n", macro_exit);
						}
					}
					continue;
				}
				if (param->decl_kind != DECL_VAR)
				{
					continue;
				}
				if (param->var.kind == VARDECL_PARAM_CT || param->var.kind == VARDECL_PARAM_CT_TYPE || param->var.kind == VARDECL_PARAM_EXPR)
				{
					continue;
				}
				if (param->var.no_init && param->var.defaulted)
				{
					continue;
				}
				CValue pval = {0};
				c_emit_local_decl(c, param, &pval);
				if (expr->macro_block.had_optional_arg)
				{
					PRINTF("if (__c3_current_fault != NULL) {\n");
					if (fault_var != 0)
					{
						PRINTF("\t___var_%d = __c3_current_fault;\n", fault_var);
					}
					PRINTF("\tgoto __C3_LABEL_%d;\n}\n", macro_exit);
				}
			}

			if (expr->macro_block.first_stmt)
			{
				c_emit_stmt_chain(c, expr->macro_block.first_stmt);
			}

			c_emit_label(c, macro_exit);
			if (fault_var != 0)
			{
				PRINTF("__c3_current_fault = ___var_%d;\n", fault_var);
			}
			c->current_macro_ret_var    = old_macro_ret;
			c->current_macro_fault_var  = old_macro_fault;
			c->current_macro_ret_type   = old_macro_ret_type;
			c->current_macro_exit_label = old_macro_exit;
			return;
		}
		case EXPR_POINTER_OFFSET:
		{
			CValue ptr_val = {0}, offset_val = {0};
			c_emit_expr(c, &ptr_val, exprptr(expr->pointer_offset_expr.ptr));
			c_value_rvalue(c, &ptr_val);
			c_emit_expr(c, &offset_val, exprptr(expr->pointer_offset_expr.offset));
			c_value_rvalue(c, &offset_val);
			Type *target_type = (expr->type && is_valid_type_ptr(expr->type)) ? c_safe_type_lower(expr->type) : type_void;
			if (target_type->type_kind == TYPE_VOID)
			{
				target_type = type_voidptr;
			}
			int temp          = c_emit_temp_var(c, value, target_type);
			const char *tname = c_type_name(c, target_type);
			if (c_type_is_aggregate(target_type))
			{
				int len = (int)target_type->array.len;
				if (len <= 0)
				{
					len = 1;
				}
				Type *elem_t       = target_type->array.base;
				bool offset_is_vec = (offset_val.type && c_type_is_aggregate(offset_val.type));
				bool ptr_is_vec    = (ptr_val.type && c_type_is_aggregate(ptr_val.type));
				bool is_void_ptr   = elem_t && elem_t->type_kind == TYPE_POINTER && elem_t->pointer && elem_t->pointer->type_kind == TYPE_VOID;
				PRINTF("%s ___var_%d;\n", tname, temp);
				for (int i = 0; i < len; i++)
				{
					const char *p_str = ptr_is_vec ? str_printf("___var_%d.ptr[%d]", ptr_val.var, i) : str_printf("___var_%d", ptr_val.var);
					const char *o_str = offset_is_vec ? str_printf("___var_%d.ptr[%d]", offset_val.var, i) : str_printf("___var_%d", offset_val.var);
					if (is_void_ptr)
					{
						PRINTF("___var_%d.ptr[%d] = (void*)(((char*)%s) + %s);\n", temp, i, p_str, o_str);
					}
					else
					{
						PRINTF("___var_%d.ptr[%d] = %s + %s;\n", temp, i, p_str, o_str);
					}
				}
				return;
			}
			if (target_type->type_kind == TYPE_POINTER && target_type->pointer && target_type->pointer->type_kind == TYPE_VOID)
			{
				PRINTF("%s ___var_%d = (void*)(((char*)___var_%d) + ___var_%d);\n", tname, temp, ptr_val.var, offset_val.var);
			}
			else
			{
				PRINTF("%s ___var_%d = (%s)(((%s)___var_%d) + ___var_%d);\n", tname, temp, tname, tname, ptr_val.var, offset_val.var);
			}
			return;
		}
		case EXPR_SWIZZLE:
		{
			CValue parent_val = {0};
			c_emit_expr(c, &parent_val, exprptr(expr->swizzle_expr.parent));
			c_value_rvalue(c, &parent_val);
			Type *target_type = (expr->type && is_valid_type_ptr(expr->type)) ? c_safe_type_lower(expr->type) : type_void;
			if (target_type->type_kind == TYPE_VOID)
			{
				target_type = type_int;
			}
			int temp = c_emit_temp_var(c, value, target_type);
			PRINTF("%s ___var_%d = {0};\n", c_type_name(c, target_type), temp);
			const char *sw = expr->swizzle_expr.swizzle;
			int len        = (int)strlen(sw);
			for (int i = 0; i < len; i++)
			{
				int idx = SWIZZLE_INDEX(sw[i]);
				PRINTF("___var_%d.ptr[%d] = ___var_%d.ptr[%d];\n", temp, i, parent_val.var, idx);
			}
			return;
		}
		case EXPR_FORCE_UNWRAP:
		{
			if (expr->inner_expr)
			{
				c_emit_expr(c, value, expr->inner_expr);
				c_value_rvalue(c, value);
				PRINTF("if (__c3_current_fault != NULL) { __c3_abort(); }\n");
				value->optional = 0;
			}
			else
			{
				Type *target_type = (expr->type && is_valid_type_ptr(expr->type)) ? c_safe_type_lower(expr->type) : type_void;
				if (target_type->type_kind == TYPE_VOID)
				{
					target_type = type_int;
				}
				int temp = c_emit_temp_var(c, value, target_type);
				c_emit_var_zero_init(c, temp, target_type);
			}
			return;
		}
		case EXPR_OPTIONAL:
		{
			Type *target_type = (expr->type && is_valid_type_ptr(expr->type)) ? c_safe_type_lower(expr->type) : type_void;
			if (target_type->type_kind != TYPE_VOID)
			{
				int temp = c_emit_temp_var(c, value, target_type);
				c_emit_var_zero_init(c, temp, target_type);
			}
			else
			{
				value->var  = 0;
				value->type = type_void;
				value->kind = CV_VALUE;
			}

			if (expr->inner_expr)
			{
				CValue inner_val = {0};
				c_emit_expr(c, &inner_val, expr->inner_expr);
				c_value_rvalue(c, &inner_val);
				if (inner_val.var != 0)
				{
					PRINTF("__c3_current_fault = (c3fault_t)(uintptr_t)___var_%d;\n", inner_val.var);
					if (c->current_macro_fault_var != 0)
					{
						PRINTF("___var_%d = (c3fault_t)(uintptr_t)___var_%d;\n", c->current_macro_fault_var, inner_val.var);
					}
					value->optional = inner_val.var;
					if (target_type == type_fault || target_type->type_kind == TYPE_ANYFAULT)
					{
						value->var = inner_val.var;
					}
				}
			}
			return;
		}
		case EXPR_RETHROW:
		{
			if (expr->rethrow_expr.inner)
			{
				CValue inner_val = {0};
				c_emit_expr(c, &inner_val, expr->rethrow_expr.inner);
				c_value_rvalue(c, &inner_val);
				*value = inner_val;
				if (inner_val.optional != 0)
				{
					PRINTF("if (___var_%d != NULL) {\n", inner_val.optional);
					PRINTF("\t__c3_current_fault = ___var_%d;\n", inner_val.optional);
					if (expr->rethrow_expr.cleanup)
					{
						int err_temp = c_create_variable(c);
						PRINTF("\tc3fault_t ___var_%d = __c3_current_fault;\n", err_temp);
						c_emit_stmt_chain(c, expr->rethrow_expr.cleanup);
						PRINTF("\t__c3_current_fault = ___var_%d;\n", err_temp);
					}
					c_emit_check_fault_and_return(c, expr);
					PRINT("}\n");
					value->optional = 0;
				}
				else
				{
					PRINT("if (__c3_current_fault != NULL) {\n");
					if (expr->rethrow_expr.cleanup)
					{
						int err_temp = c_create_variable(c);
						PRINTF("\tc3fault_t ___var_%d = __c3_current_fault;\n", err_temp);
						c_emit_stmt_chain(c, expr->rethrow_expr.cleanup);
						PRINTF("\t__c3_current_fault = ___var_%d;\n", err_temp);
					}
					c_emit_check_fault_and_return(c, expr);
					PRINT("}\n");
					value->optional = 0;
				}
			}
			else
			{
				Type *target_type = (expr->type && is_valid_type_ptr(expr->type)) ? c_safe_type_lower(expr->type) : type_void;
				if (target_type->type_kind == TYPE_VOID)
				{
					target_type = type_int;
				}
				int temp = c_emit_temp_var(c, value, target_type);
				c_emit_var_zero_init(c, temp, target_type);
			}
			return;
		}
		case EXPR_TRY:
		{
			CValue opt_val = {0};
			if (expr->try_expr.optional)
			{
				c_emit_expr(c, &opt_val, expr->try_expr.optional);
				c_value_rvalue(c, &opt_val);
				if (expr->try_expr.assign_existing && expr->try_expr.lhs)
				{
					c_emit_lvalue_assign(c, expr->try_expr.lhs, &opt_val, "=", NULL);
				}
				else if (expr->try_expr.decl)
				{
					Type *dt = c_decl_type(expr->try_expr.decl);
					if (dt->type_kind == TYPE_VOID && opt_val.type && opt_val.type->type_kind != TYPE_VOID)
					{
						dt                        = c_safe_type_lower(opt_val.type);
						expr->try_expr.decl->type = dt;
					}
					VariableId vid = c_get_or_create_decl_var(c, expr->try_expr.decl);
					c_emit_assign_var(c, vid, dt, &opt_val);
				}
			}
			value->var  = c_create_variable(c);
			value->type = type_bool;
			value->kind = CV_VALUE;
			PRINTF("bool ___var_%d = (__c3_current_fault == NULL);\n", value->var);
			PRINT("__c3_current_fault = NULL;\n");
			return;
		}
		case EXPR_CATCH:
		{
			CValue last_val = {0};
			FOREACH(Expr *, e, expr->catch_expr.exprs)
			{
				c_emit_expr(c, &last_val, e);
				c_value_rvalue(c, &last_val);
			}
			int fault_temp = c_emit_temp_var(c, value, type_fault);
			if (last_val.optional != 0)
			{
				PRINTF("c3fault_t ___var_%d = (c3fault_t)(uintptr_t)___var_%d;\n", fault_temp, last_val.optional);
			}
			else
			{
				PRINTF("c3fault_t ___var_%d = __c3_current_fault;\n", fault_temp);
			}
			PRINT("__c3_current_fault = NULL;\n");
			if (expr->catch_expr.decl)
			{
				Type *dt = c_decl_type(expr->catch_expr.decl);
				if (dt->type_kind == TYPE_VOID)
				{
					dt                          = type_fault;
					expr->catch_expr.decl->type = dt;
				}
				VariableId vid = c_get_or_create_decl_var(c, expr->catch_expr.decl);
				CValue fval    = {.var = fault_temp, .type = type_fault, .kind = CV_VALUE};
				c_emit_assign_var(c, vid, dt, &fval);
			}
			return;
		}
		case EXPR_TRY_UNWRAP_CHAIN:
		{
			int temp = c_emit_temp_var(c, value, type_bool);
			PRINTF("bool ___var_%d = true;\n", temp);
			FOREACH(Expr *, e, expr->try_unwrap_chain_expr)
			{
				PRINTF("if (___var_%d) {\n", temp);
				CValue eval = {0};
				c_emit_expr(c, &eval, e);
				c_value_rvalue(c, &eval);
				if (eval.type && eval.type->type_kind == TYPE_BOOL && eval.var != 0)
				{
					PRINTF("\tif (!___var_%d || __c3_current_fault != NULL) { ___var_%d = false; }\n", eval.var, temp);
				}
				else
				{
					PRINTF("\tif (__c3_current_fault != NULL) { ___var_%d = false; }\n", temp);
				}
				PRINT("}\n");
			}
			PRINT("__c3_current_fault = NULL;\n");
			return;
		}
		case EXPR_LAST_FAULT:
			value->var  = c_create_variable(c);
			value->type = type_fault;
			value->kind = CV_VALUE;
			PRINTF("c3fault_t ___var_%d = __c3_current_fault;\n", value->var);
			return;
		case EXPR_RETVAL:
		{
			if (c->retval.var != 0)
			{
				*value = c->retval;
				return;
			}
			Type *target_type = (expr->type && is_valid_type_ptr(expr->type)) ? c_safe_type_lower(expr->type) : type_void;
			if (target_type->type_kind == TYPE_VOID)
			{
				target_type = type_int;
			}
			int temp = c_emit_temp_var(c, value, target_type);
			c_emit_var_zero_init(c, temp, target_type);
			return;
		}
		case EXPR_TYPEID_INFO:
		{
			CValue parent_val = {0};
			c_emit_expr(c, &parent_val, exprptr(expr->typeid_info_expr.parent));
			c_value_rvalue(c, &parent_val);
			Type *target_type = (expr->type && is_valid_type_ptr(expr->type)) ? c_safe_type_lower(expr->type) : type_void;
			if (target_type->type_kind == TYPE_VOID)
			{
				target_type = type_sz;
			}
			int temp          = c_emit_temp_var(c, value, target_type);
			const char *tname = c_type_name(c, target_type);
			const char *pname = (parent_val.var != 0) ? str_printf("___var_%d", parent_val.var) : "NULL";
			switch (expr->typeid_info_expr.kind)
			{
				case TYPEID_INFO_KIND:
					PRINTF("%s ___var_%d = (%s != NULL) ? ((const c3type_info_t*)%s)->kind : 0;\n", tname, temp, pname, pname);
					return;
				case TYPEID_INFO_SIZEOF:
					PRINTF("%s ___var_%d = (%s != NULL) ? ((const c3type_info_t*)%s)->size : 0;\n", tname, temp, pname, pname);
					return;
				case TYPEID_INFO_LEN:
					PRINTF("%s ___var_%d = (%s != NULL) ? ((const c3type_info_t*)%s)->len : 0;\n", tname, temp, pname, pname);
					return;
				case TYPEID_INFO_INNER:
					PRINTF("%s ___var_%d = (%s != NULL) ? ((const c3type_info_t*)%s)->inner : NULL;\n", tname, temp, pname, pname);
					return;
				case TYPEID_INFO_PARENTOF:
					PRINTF("%s ___var_%d = (%s != NULL && ((const c3type_info_t*)%s)->parentof != NULL) ? ((const c3type_info_t*)%s)->parentof : (c3typeid_t)&__c3_typeid_void;\n", tname, temp, pname, pname, pname);
					return;
				case TYPEID_INFO_NAMES:
					PRINTF("%s ___var_%d = (%s != NULL && ((const c3type_info_t*)%s)->names != NULL) ? (%s){ .ptr = (void*)((const c3type_info_t*)%s)->names, .len = ((const c3type_info_t*)%s)->len } : (%s){ .ptr = NULL, .len = 0 };\n", tname, temp, pname, pname, tname, pname, pname, tname);
					return;
				default:
					c_emit_var_zero_init(c, temp, target_type);
					return;
			}
		}
		default:
		{
			Type *t = (expr->type && is_valid_type_ptr(expr->type)) ? c_safe_type_lower(expr->type) : type_void;
			if (t->type_kind == TYPE_VOID)
			{
				value->var  = 0;
				value->type = type_void;
				value->kind = CV_VALUE;
				return;
			}
			int temp = c_emit_temp_var(c, value, t);
			c_emit_var_zero_init(c, temp, t);
			return;
		}
	}
}

void c_emit_expr(GenContext *c, CValue *value, Expr *expr)
{
	c_emit_expr_internal(c, value, expr);
	if (value && value->optional == 0 && expr && expr->type && is_valid_type_ptr(expr->type) && type_is_optional(expr->type))
	{
		int fault_temp = c_emit_temp_var(c, NULL, type_fault);
		PRINTF("c3fault_t ___var_%d = __c3_current_fault;\n", fault_temp);
		value->optional = fault_temp;
	}
}

void c_emit_ignored_expr(GenContext *c, Expr *expr)
{
	CValue value = {0};
	c_emit_expr(c, &value, expr);
}