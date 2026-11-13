#include "c_codegen_internal.h"

static void c_emit_const_expr(GenContext *c, CValue *value, Expr *expr)
{
	Type *t = c_expr_type(expr);
	ASSERT(t->type_kind != TYPE_VOID);
	c_emit_type_forward_decl(c, t);
	const char *tname = c_type_name(c, t);
	int temp          = c_emit_temp_var(c, value, t);

	if (t->type_kind == TYPE_SLICE && expr->const_expr.const_kind == CONST_SLICE)
	{
		ConstInitializer *init = expr->const_expr.slice_init;
		if (!init || init->kind == CONST_INIT_ZERO)
		{
			PRINTF("%s ___var_%d = (%s){ .ptr = NULL, .len = 0 };\n", tname, temp, tname);
			return;
		}
		Type *elem_t = (init->type && init->type->type_kind == TYPE_ARRAY && init->type->array.base) ? c_safe_type_lower(init->type->array.base) : (t->array.base ? c_safe_type_lower(t->array.base) : type_void);
		ArrayIndex count = (init->type && init->type->type_kind == TYPE_ARRAY) ? init->type->array.len : 0;
		VariableId arr_var = c_emit_alloca_array(c, elem_t, count > 0 ? count : 1);
		ConstInitializer **elements = init->kind == CONST_INIT_ARRAY_FULL ? init->init_array_full : (init->kind == CONST_INIT_ARRAY ? init->init_array.elements : NULL);
		ArrayIndex el_count = (init->kind == CONST_INIT_ARRAY_FULL) ? count : (elements ? vec_size(elements) : 0);
		for (ArrayIndex i = 0; i < el_count; i++)
		{
			ArrayIndex index = i;
			ConstInitializer *el = elements[i];
			if (init->kind == CONST_INIT_ARRAY && el->kind == CONST_INIT_ARRAY_VALUE)
			{
				index = el->init_array_value.index;
				el = el->init_array_value.element;
			}
			PRINTF("___var_%d[%lld] = (%s)", arr_var, (long long)index, c_type_name(c, elem_t));
			c_emit_const_initializer_rec(c, el);
			PRINT(";\n");
		}
		PRINTF("%s ___var_%d = (%s){ .ptr = ___var_%d, .len = %lld };\n", tname, temp, tname, arr_var, (long long)count);
		return;
	}

	if ((expr->const_expr.const_kind == CONST_BYTES || expr->const_expr.const_kind == CONST_STRING) &&
	    expr->const_expr.bytes.len > 4000)
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

	PRINTF("%s ___var_%d = ", tname, temp);
	c_emit_const_init_expr(c, expr, t);
	PRINT(";\n");
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
		PRINTF("%s ___var_%d = (void*)&%s;\n", c_type_name(c, ptr_t), temp, c_decl_ref(c, decl));
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
		const char *fsym = c_fault_ref(c, decl);
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

	const char *fault = c_ident_read_fault_ref(c, expr->ident_expr);
	if (fault) c_emit_jump_to_optional_exit(c, fault);
	const char *src   = c_decl_lvalue_str(c, decl);
	int temp          = c_emit_temp_var(c, value, t);
	const char *tname = c_type_name(c, t);
	if (c_type_is_vec_or_arr(t))
	{
		PRINTF("%s ___var_%d;\n", tname, temp);
		PRINTF("__c3_memcpy(&___var_%d, &%s, sizeof(%s));\n", temp, src, src);
	}
	else if (type_is_pointer(t) || t->type_kind == TYPE_FUNC_PTR)
	{
		// Explicit cast satisfies Clang's strict -Wincompatible-pointer-types
		PRINTF("%s ___var_%d = (%s)%s;\n", tname, temp, tname, src);
	}
	else
	{
		PRINTF("%s ___var_%d = %s;\n", tname, temp, src);
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
		COptionalAssign assign;
		c_optional_assign_begin(c, &assign, c_decl_fault_ref(c, decl), val_expr);
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
		c_optional_assign_end(c, &assign);
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

// Start the call `target(`. An optional call assigns its fault to a new variable, returned here (0 otherwise),
// and passes the address of the value first: `*leading_arg` then tells that the caller's arguments follow a `, `.
static int c_emit_call_invocation(GenContext *c, CValue *value, CRet ret, Type *return_type, bool has_return, const char *target, bool *leading_arg)
{
	*leading_arg = false;
	if (ret.optional)
	{
		int ret_var = 0;
		if (ret.value)
		{
			c_emit_type_forward_decl(c, ret.value);
			ret_var = c_emit_temp_var(c, value, ret.value);
			PRINTF("%s ___var_%d;\n", c_type_name(c, ret.value), ret_var);
		}
		else if (value && value->var == 0)
		{
			c_value_set_void(value);
		}
		int fault = c_create_variable(c);
		PRINTF("c3fault_t ___var_%d = %s(", fault, target);
		if (ret_var)
		{
			PRINTF("&___var_%d", ret_var);
			*leading_arg = true;
		}
		return fault;
	}
	if (has_return)
	{
		c_emit_type_forward_decl(c, return_type);
		int ret_var = c_emit_temp_var(c, value, return_type);
		PRINTF("%s ___var_%d = %s(", c_type_name(c, return_type), ret_var, target);
		return 0;
	}

	if (value && value->var == 0)
	{
		c_value_set_void(value);
	}
	PRINTF("%s(", target);
	return 0;
}

// After an optional call: jump to the catch target when it returned a fault.
static void c_emit_call_fault_check(GenContext *c, int fault)
{
	if (!fault) return;
	c_emit_jump_to_optional_exit(c, str_printf("___var_%d", fault));
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
	// A call through a function value uses the signature of its function pointer type, as LLVM does.
	Signature *sig = function_decl ? &function_decl->func_decl.signature : NULL;
	if (!sig)
	{
		Type *ft = c_safe_type_lower(exprptr(call->function)->type);
		ASSERT(ft->type_kind == TYPE_FUNC_PTR);
		sig = ft->pointer->function.signature;
	}

	Expr **args      = call->arguments;
	Expr **varargs   = call->varargs;
	int num_args     = vec_size(args);
	int num_varargs  = vec_size(varargs);
	bool is_variadic = sig->variadic == VARIADIC_TYPED || sig->variadic == VARIADIC_ANY;
	int vararg_idx   = sig->vararg_index;
	int param_count  = vec_size(sig->params);

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
				Decl *vararg_param = i < vec_size(sig->params) ? sig->params[i] : NULL;
				Type *slice_type   = (vararg_param) ? c_safe_type_lower(vararg_param->type) : type_get_slice(type_any);
				Type *elem_type    = (slice_type && slice_type->type_kind == TYPE_SLICE) ? c_safe_type_lower(slice_type->array.base) : type_any;
				c_emit_type_forward_decl(c, slice_type);
				const char *slice_tname = c_type_name(c, slice_type);
				const char *elem_tname  = c_type_name(c, elem_type);

				if (num_varargs == 0)
				{
					int slice_var = c_emit_temp_var(c, &c_args[arg_idx++], slice_type);
					PRINTF("%s ___var_%d = (%s){ .ptr = NULL, .len = 0 };\n", slice_tname, slice_var, slice_tname);
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
					VariableId arr_var = c_emit_alloca_array(c, elem_type, num_varargs);
					for (int j = 0; j < num_varargs; j++)
					{
						c_emit_store(c, str_printf("___var_%d[%d]", arr_var, j), elem_type, &vvals[j]);
					}
					int slice_var = c_emit_temp_var(c, &c_args[arg_idx++], slice_type);
					PRINTF("%s ___var_%d = (%s){ .ptr = ___var_%d, .len = %d };\n", slice_tname, slice_var, slice_tname, arr_var, num_varargs);
					free(vvals);
				}
			}
		}
		else
		{
			if (i < num_args && args[i])
			{
				c_emit_expr(c, &c_args[arg_idx], args[i]);
				c_value_rvalue(c, &c_args[arg_idx]);
				arg_idx++;
			}
			else
			{
				Type *pt          = (i < vec_size(sig->params) && sig->params[i]) ? c_decl_type(sig->params[i]) : type_void;
				c_args[arg_idx++] = (CValue){.var = 0, .type = pt, .kind = CV_VALUE};
			}
		}
	}

	if (sig->variadic == VARIADIC_RAW && num_varargs > 0)
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

	Type *fn_rtype    = c_safe_type_lower(typeget(sig->rtype));
	CRet ret          = c_signature_ret(sig);
	Type *return_type = c_safe_type_lower(expr->type);
	if (ret.optional) return_type = ret.value ? ret.value : c_safe_type_lower(type_no_optional(expr->type));
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
			c_emit_dynamic_dispatcher(c, dyn_fn);
			const char *disp_name = c_intern(str_printf("__c3_dyn_%s", c_get_decl_name(dyn_fn)));
			bool leading_arg = false;
			int fault        = c_emit_call_invocation(c, value, ret, return_type, has_return, disp_name, &leading_arg);
			for (int i = 0; i < arg_idx; i++)
			{
				if (i != 0 || leading_arg)
				{
					PRINT(", ");
				}
				PRINTF("___var_%d", c_args[i].var);
			}
			PRINT(");\n");
			c_emit_call_fault_check(c, fault);
			if (call->no_return)
			{
				c->current_block_live = false;
			}
			free(c_args);
			return;
		}
	}

	const char *callee_str = NULL;
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

		if (safe_mode_enabled() && call->function)
		{
			scratch_buffer_clear();
			loc_to_scratch(exprptr(call->function)->loc);
			const char *name = scratch_buffer_copy();
			c_emit_panic_if(c, str_printf("___var_%d == NULL", func_val.var), PANIC_FN_NULL_FUNCTION, exprptr(call->function)->loc, name, NULL, NULL);
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
			else if (value && value->var == 0)
			{
				c_value_set_void(value);
			}
			free(c_args);
			return;
		}

		if (func_val.type && type_is_func_ptr(func_val.type))
		{
			c_emit_type_forward_decl(c, func_val.type);
			callee_str = str_printf("((%s)___var_%d)", c_type_name(c, func_val.type), func_val.var);
		}
		else
		{
			const char *fn_cast = c_signature_str(c, sig, "(*)", (CSigOpts){0});
			callee_str = str_printf("((%s)___var_%d)", fn_cast, func_val.var);
		}
	}
	else
	{
		c_emit_function_decl(c, function_decl, false);
		callee_str = c_get_decl_name(function_decl);
		// Extern declarations of one C symbol may disagree between modules, while C allows a single prototype
		// (the first one emitted). A call through a declaration with another signature casts to that signature,
		// as LLVM calls each declaration's own function type.
		if (function_decl->is_extern && sig)
		{
			const char *prototype = htable_get(&c->decl_names, (void *)callee_str);
			if (strcmp(prototype, c_signature_str(c, sig, callee_str, (CSigOpts){ .is_main = c_fn_is_main(function_decl) })) != 0)
			{
				callee_str = str_printf("((%s)%s)", c_signature_str(c, sig, "(*)", (CSigOpts){0}), callee_str);
			}
		}
	}

	bool leading_arg      = false;
	int fault             = c_emit_call_invocation(c, value, ret, return_type, has_return, callee_str, &leading_arg);
	int emitted_call_args = leading_arg ? 1 : 0;
	for (int i = 0; i < arg_idx; i++)
	{
		// NULL for C variadic arguments past the declared parameters.
		Type *expected_type = NULL;
		if (i < vec_size(sig->params) && sig->params[i])
		{
			expected_type = c_decl_type(sig->params[i]);
		}

		if (expected_type && type_size(expected_type) == 0)
		{
			continue;
		}

		if (emitted_call_args != 0)
		{
			PRINT(", ");
		}
		emitted_call_args++;

		Type *arg_type = c_safe_type_lower(c_args[i].type);

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

		// Sema converts every argument to its parameter type. Without a signature the argument's type is used.
		ASSERT(!expected_type || strcmp(c_type_name(c, expected_type), c_type_name(c, arg_type)) == 0);
		PRINTF("___var_%d", c_args[i].var);
	}
	PRINT(");\n");
	c_emit_call_fault_check(c, fault);
	if (call->no_return)
	{
		c->current_block_live = false;
	}
	free(c_args);
}

static void c_emit_slice_elem_eq_loop(GenContext *c, int result_var, int left_var, int right_var, const char *len_str, bool elem_is_slice, bool is_eq)
{
	int loop_k = c_create_variable(c);
	PRINTF("bool ___var_%d = (___var_%d.len == %s);\n", result_var, left_var, len_str);
	PRINTF("if (___var_%d) {\n", result_var);
	PRINTF("\tfor (size_t ___var_%d = 0; ___var_%d < %s; ___var_%d++) {\n", loop_k, loop_k, len_str, loop_k);
	if (elem_is_slice)
	{
		PRINTF("\t\tif (!__C3_SLICE_EQ(___var_%d.ptr[___var_%d], ___var_%d.ptr[___var_%d])) { ___var_%d = false; break; }\n",
		       left_var, loop_k, right_var, loop_k, result_var);
	}
	else
	{
		PRINTF("\t\tif (___var_%d.ptr[___var_%d] != ___var_%d.ptr[___var_%d]) { ___var_%d = false; break; }\n",
		       left_var, loop_k, right_var, loop_k, result_var);
	}
	PRINT("\t}\n}\n");
	if (!is_eq)
	{
		PRINTF("___var_%d = !___var_%d;\n", result_var, result_var);
	}
}

// Load the current value of `left` ahead of a read-modify-write, evaluating its address once.
// `addr` is left empty for plain variables, otherwise it is the address to pass to c_emit_lvalue_assign.
static void c_emit_lvalue_load_for_update(GenContext *c, Expr *left, Type *target_t, CValue *addr, CValue *current)
{
	if (left->expr_kind == EXPR_IDENTIFIER || left->expr_kind == EXPR_DECL)
	{
		c_emit_expr(c, current, left);
		c_value_rvalue(c, current);
		return;
	}
	bool is_swizzle = left->expr_kind == EXPR_SWIZZLE;
	Expr *target    = is_swizzle ? exprptr(left->swizzle_expr.parent) : left;
	while (target->expr_kind == EXPR_RECAST || target->expr_kind == EXPR_ADDR_CONVERSION || target->expr_kind == EXPR_RVALUE)
	{
		target = target->inner_expr;
	}
	Type *parent_t = c_expr_type(target);
	c_emit_lvalue_addr(c, target, addr, type_get_ptr(parent_t));
	const char *target_tname = c_type_name(c, target_t);
	int cur_temp             = c_emit_temp_var(c, current, target_t);
	if (!is_swizzle)
	{
		PRINTF("%s ___var_%d = *(%s*)___var_%d;\n", target_tname, cur_temp, target_tname, addr->var);
		return;
	}
	const char *parent_tname = c_type_name(c, parent_t);
	const char *sw           = left->swizzle_expr.swizzle;
	PRINTF("%s ___var_%d;\n", target_tname, cur_temp);
	if (!c_type_is_vec_or_arr(target_t))
	{
		PRINTF("___var_%d = ((%s*)___var_%d)->ptr[%d];\n", cur_temp, parent_tname, addr->var, SWIZZLE_INDEX(sw[0]));
		return;
	}
	for (int i = 0; sw[i]; i++)
	{
		PRINTF("___var_%d.ptr[%d] = ((%s*)___var_%d)->ptr[%d];\n", cur_temp, i, parent_tname, addr->var, SWIZZLE_INDEX(sw[i]));
	}
}

// Panic if an integer divisor, or any lane of it, is zero.
static void c_emit_zero_check(GenContext *c, CValue *divisor, Type *divisor_t, PanicFunction id, SourceLocId loc)
{
	if (!c_type_is_vec_or_arr(divisor_t))
	{
		c_emit_panic_if(c, str_printf("___var_%d == 0", divisor->var), id, loc, NULL, NULL, NULL);
		return;
	}
	scratch_buffer_clear();
	for (ArrayIndex k = 0; k < divisor_t->array.len; k++)
	{
		scratch_buffer_printf("%s___var_%d.ptr[%u] == 0", k ? " || " : "", divisor->var, (unsigned)k);
	}
	c_emit_panic_if(c, scratch_buffer_copy(), id, loc, NULL, NULL, NULL);
}

// Panic on a shift amount that is negative or not less than the bit width, reporting the amount like the LLVM backend:
// converted to the width of the shifted value, with the signedness of the amount.
static void c_emit_shift_check(GenContext *c, Type *shifted_t, CValue *amount, Type *amount_t, SourceLocId loc)
{
	bool is_vec          = c_type_is_vec_or_arr(shifted_t);
	Type *base_t         = is_vec ? type_flatten(shifted_t->array.base) : shifted_t;
	BitSize bits         = type_size(base_t) * 8;
	bool amount_is_vec   = c_type_is_vec_or_arr(amount_t);
	bool amount_signed   = type_is_signed(amount_is_vec ? type_flatten(amount_t->array.base) : amount_t);
	Type *report_t       = amount_signed ? type_int_signed_by_bitsize(bits) : type_int_unsigned_by_bitsize(bits);
	Type *compare_t      = type_is_unsigned(base_t) ? type_int_unsigned_by_bitsize(bits) : type_int_signed_by_bitsize(bits);
	const char *cmp_name = c_type_name(c, compare_t);
	const char *rep_name = c_type_name(c, report_t);
	const char *min_expr;
	const char *max_expr;
	if (amount_is_vec)
	{
		// Reduce the lanes to their min and max in the compared type.
		int min_var = c_emit_temp_var(c, NULL, compare_t);
		int max_var = c_emit_temp_var(c, NULL, compare_t);
		PRINTF("%s ___var_%d = (%s)(%s)___var_%d.ptr[0];\n", cmp_name, min_var, cmp_name, rep_name, amount->var);
		PRINTF("%s ___var_%d = ___var_%d;\n", cmp_name, max_var, min_var);
		for (ArrayIndex k = 1; k < amount_t->array.len; k++)
		{
			const char *lane = str_printf("(%s)(%s)___var_%d.ptr[%u]", cmp_name, rep_name, amount->var, (unsigned)k);
			PRINTF("if (%s < ___var_%d) ___var_%d = %s;\n", lane, min_var, min_var, lane);
			PRINTF("if (%s > ___var_%d) ___var_%d = %s;\n", lane, max_var, max_var, lane);
		}
		min_expr = str_printf("___var_%d", min_var);
		max_expr = str_printf("___var_%d", max_var);
	}
	else
	{
		min_expr = max_expr = str_printf("(%s)(%s)___var_%d", cmp_name, rep_name, amount->var);
	}
	if (!type_is_unsigned(base_t))
	{
		CPanicValue min_value = { str_printf("(%s)%s", rep_name, min_expr), report_t };
		c_emit_panic_if(c, str_printf("%s < 0", min_expr), PANIC_FN_SHIFT, loc, NULL, &min_value, NULL);
	}
	CPanicValue max_value = { str_printf("(%s)%s", rep_name, max_expr), report_t };
	c_emit_panic_if(c, str_printf("%s >= %u", max_expr, (unsigned)bits), PANIC_FN_SHIFT, loc, NULL, &max_value, NULL);
}

// Emit `left op right` for already evaluated operands. Shared by binary and compound assign.
static void c_emit_binary_op(GenContext *c, CValue *value, BinaryOp op, Type *expr_type, CValue left_value, CValue right_value, SourceLocId loc)
{
	bool is_comp = (op == BINARYOP_EQ || op == BINARYOP_NE || op == BINARYOP_LT || op == BINARYOP_LE || op == BINARYOP_GT || op == BINARYOP_GE ||
	                op == BINARYOP_VEC_EQ || op == BINARYOP_VEC_NE || op == BINARYOP_VEC_LT || op == BINARYOP_VEC_LE || op == BINARYOP_VEC_GT || op == BINARYOP_VEC_GE);

	c_emit_type_forward_decl(c, expr_type);
	const char *type_string = c_type_name(c, expr_type);

	value->var  = c_create_variable(c);
	value->type = expr_type;
	value->kind = CV_VALUE;

	c_ensure_cvalue_var(c, &left_value, expr_type);
	c_ensure_cvalue_var(c, &right_value, expr_type);

	Type *left_t  = c_safe_type_lower(left_value.type);
	Type *right_t = c_safe_type_lower(right_value.type);

	if (safe_mode_enabled() && right_t)
	{
		if (op == BINARYOP_SHR || op == BINARYOP_SHL) c_emit_shift_check(c, expr_type, &right_value, right_t, loc);
		bool is_int = type_is_integer(c_type_is_vec_or_arr(expr_type) ? type_flatten(expr_type->array.base) : expr_type);
		if (is_int && (op == BINARYOP_DIV || op == BINARYOP_MOD))
		{
			c_emit_zero_check(c, &right_value, right_t, op == BINARYOP_DIV ? PANIC_FN_DIV_ZERO : PANIC_FN_REM_ZERO, loc);
		}
	}

	if (!c_type_is_aggregate(expr_type) && (op == BINARYOP_SHR || op == BINARYOP_SHL))
	{
		bool is_shl = (op == BINARYOP_SHL);
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

	if (compiler.build.feature.trap_on_wrap && left_t && right_t && type_is_integer(left_t) && type_is_integer(right_t) &&
	    !c_type_is_aggregate(expr_type) && (op == BINARYOP_ADD || op == BINARYOP_SUB || op == BINARYOP_MULT))
	{
		const char *bname = (op == BINARYOP_ADD) ? "__builtin_add_overflow" : (op == BINARYOP_SUB ? "__builtin_sub_overflow" : "__builtin_mul_overflow");
		PRINTF("%s ___var_%d;\n", type_string, value->var);
		PanicFunction panic_fn = (op == BINARYOP_ADD) ? PANIC_FN_ADD_OVERFLOW : (op == BINARYOP_SUB ? PANIC_FN_SUB_OVERFLOW : PANIC_FN_MUL_OVERFLOW);
		c_emit_panic_if(c, str_printf("%s(___var_%d, ___var_%d, &___var_%d)", bname, left_value.var, right_value.var, value->var), panic_fn, loc, NULL, NULL, NULL);
		return;
	}
	if (op == BINARYOP_MOD && type_is_float(expr_type))
	{
		PRINTF("%s ___var_%d = __c3_fmod(___var_%d, ___var_%d);\n", type_string, value->var, left_value.var, right_value.var);
		return;
	}

	const char *operator_string = c_binary_op_str(op);
	bool left_is_ptr            = c_type_is_c_pointer(left_t);
	bool right_is_ptr           = c_type_is_c_pointer(right_t);

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

	if (is_comp)
	{
		// Sema converts both operands of a comparison to one type (llvm_emit_comp dispatches on the left type alone).
		ASSERT(strcmp(c_type_name(c, left_t), c_type_name(c, right_t)) == 0);
		if (left_t->type_kind == TYPE_SLICE)
		{
			Type *elem_t       = left_t->array.base ? type_flatten(left_t->array.base) : NULL;
			bool elem_is_slice = elem_t && elem_t->type_kind == TYPE_SLICE;
			bool elem_is_float = elem_t && type_is_float(elem_t);
			bool is_eq         = (op == BINARYOP_EQ || op == BINARYOP_VEC_EQ);
			if (elem_is_slice || elem_is_float)
			{
				c_emit_slice_elem_eq_loop(c, value->var, left_value.var, right_value.var,
				                          str_printf("___var_%d.len", right_value.var), elem_is_slice, is_eq);
				return;
			}
			PRINTF("bool ___var_%d = %s(___var_%d, ___var_%d);\n",
			       value->var, is_eq ? "__C3_SLICE_EQ" : "__C3_SLICE_NE", left_value.var, right_value.var);
			return;
		}
		if (left_is_ptr && right_is_ptr)
		{
			PRINTF("bool ___var_%d = ((uintptr_t)___var_%d %s (uintptr_t)___var_%d);\n",
			       value->var, left_value.var, operator_string, right_value.var);
			return;
		}
		if (c_type_is_aggregate(left_t) || c_type_is_aggregate(right_t))
		{
			if (expr_type->type_kind == TYPE_BOOL)
			{
				Type *base_t = (left_t && c_type_is_vec_or_arr(left_t)) ? left_t->array.base : NULL;
				if (base_t)
				{
					base_t = type_flatten(base_t);
				}
				if (base_t && (type_is_float(base_t) || base_t->type_kind == TYPE_SLICE))
				{
					int len              = c_type_aggregate_len(left_t);
					bool is_eq           = (op == BINARYOP_EQ || op == BINARYOP_VEC_EQ);
					bool elem_is_slice   = (base_t->type_kind == TYPE_SLICE);
					const char *logic_op = is_eq ? "&&" : "||";
					const char *cmp_op   = is_eq ? "==" : "!=";
					if (len == 0)
					{
						PRINTF("bool ___var_%d = %s;\n", value->var, is_eq ? "true" : "false");
						return;
					}
					PRINTF("bool ___var_%d = (", value->var);
					for (int k = 0; k < len; k++)
					{
						if (k > 0)
						{
							PRINTF(" %s ", logic_op);
						}
						if (elem_is_slice)
						{
							PRINTF("%s(___var_%d.ptr[%d], ___var_%d.ptr[%d])",
							       is_eq ? "__C3_SLICE_EQ" : "__C3_SLICE_NE",
							       left_value.var, k, right_value.var, k);
						}
						else
						{
							PRINTF("(___var_%d.ptr[%d] %s ___var_%d.ptr[%d])",
							       left_value.var, k, cmp_op, right_value.var, k);
						}
					}
					PRINT(");\n");
					return;
				}
				PRINTF("bool ___var_%d = (__c3_memcmp(&___var_%d, &___var_%d, sizeof(%s)) %s 0);\n",
				       value->var, left_value.var, right_value.var, c_type_name(c, left_t),
				       (op == BINARYOP_EQ || op == BINARYOP_VEC_EQ) ? "==" : "!=");
				return;
			}
			CValue operands[2] = { left_value, right_value };
			c_emit_lanewise(c, value->var, expr_type, str_printf("($0 %s $1) ? -1 : 0", operator_string), operands, 2);
			return;
		}
	}

	if (c_type_is_vec_or_arr(expr_type))
	{
		// C has no `%` on floating point, lanes go through fmod.
		bool is_fmod        = op == BINARYOP_MOD && type_is_float(type_flatten(expr_type->array.base));
		CValue operands[2]  = { left_value, right_value };
		c_emit_lanewise(c, value->var, expr_type, is_fmod ? "__c3_fmod($0, $1)" : str_printf("$0 %s $1", operator_string), operands, 2);
		return;
	}

	PRINTF("%s ___var_%d = ___var_%d %s ___var_%d;\n", type_string, value->var, left_value.var, operator_string, right_value.var);
}

// The fault slot written by assigning to `left`: only an optional variable has one.
static const char *c_assign_target_fault_ref(GenContext *c, Expr *left)
{
	while (left->expr_kind == EXPR_RECAST || left->expr_kind == EXPR_ADDR_CONVERSION || left->expr_kind == EXPR_RVALUE)
	{
		left = left->inner_expr;
	}
	Decl *decl;
	switch (left->expr_kind)
	{
		case EXPR_IDENTIFIER:
			decl = left->ident_expr;
			break;
		case EXPR_DECL:
			decl = left->decl_expr;
			break;
		default:
			return NULL;
	}
	if (!decl) return NULL;
	Decl *unwrapped = c_decl_unwrap(decl);
	if (unwrapped && unwrapped->decl_kind == DECL_VAR && unwrapped->var.kind == VARDECL_PARAM_EXPR && unwrapped->var.init_expr)
	{
		return c_assign_target_fault_ref(c, unwrapped->var.init_expr);
	}
	return c_decl_fault_ref(c, decl);
}

static void c_emit_binary_expr(GenContext *c, CValue *value, Expr *expr)
{
	ExprBinary *binary = &expr->binary_expr;
	BinaryOp op        = binary->operator;

	if (op >= BINARYOP_ASSIGN)
	{
		Expr *left      = exprptr(binary->left);
		Expr *right     = exprptr(binary->right);
		CValue addr_val = {0};

		// `a += e` with optional `e` must store the fault into `a`: emit as `a = a + e`.
		if (op > BINARYOP_ASSIGN && left->expr_kind == EXPR_IDENTIFIER && (IS_OPTIONAL(right) || IS_OPTIONAL(left)))
		{
			Expr *op_expr = expr_new(EXPR_BINARY, expr->loc);
			*op_expr = *expr;
			op_expr->binary_expr.operator = binaryop_assign_base_op(op);
			Expr assign = *expr;
			assign.binary_expr.operator = BINARYOP_ASSIGN;
			assign.binary_expr.right = exprid(op_expr);
			c_emit_binary_expr(c, value, &assign);
			return;
		}

		if (op > BINARYOP_ASSIGN)
		{
			// `a op= b` is: take the address once, load, emit the plain binary op, store.
			Type *target_t = c_expr_type(left);
			CValue current = {0};
			c_emit_lvalue_load_for_update(c, left, target_t, &addr_val, &current);
			CValue right_val = {0};
			c_emit_expr(c, &right_val, right);
			c_value_rvalue(c, &right_val);
			CValue result = {0};
			c_emit_binary_op(c, &result, binaryop_assign_base_op(op), target_t, current, right_val, expr->loc);
			c_emit_lvalue_assign(c, left, addr_val.var ? &addr_val : NULL, &result, NULL);
			if (value) *value = result;
			return;
		}

		COptionalAssign assign;
		c_optional_assign_begin(c, &assign, c_assign_target_fault_ref(c, left), right);
		CValue right_val = {0};
		c_emit_expr(c, &right_val, right);
		c_value_rvalue(c, &right_val);
		c_emit_lvalue_assign(c, left, NULL, &right_val, value);
		c_optional_assign_end(c, &assign);
		return;
	}

	if (op == BINARYOP_ELSE)
	{
		// The value type: a fault in the right operand still leaves through the catch target.
		Type *res_type = c_safe_type_lower(type_no_optional(expr->type));
		bool is_void   = res_type->type_kind == TYPE_VOID;
		int temp       = 0;
		if (is_void)
		{
			c_value_set_void(value);
		}
		else
		{
			temp = c_emit_temp_var(c, value, res_type);
			PRINTF("%s ___var_%d;\n", c_type_name(c, res_type), temp);
		}
		int else_label  = c_create_label(c);
		int end_label   = c_create_label(c);
		CCatch saved    = c->catch;
		c->catch        = (CCatch){ else_label, NULL };
		CValue left_val = {0};
		c_emit_expr(c, &left_val, exprptr(binary->left));
		c_value_rvalue(c, &left_val);
		if (!is_void)
		{
			c_emit_assign_var(c, temp, res_type, &left_val);
		}
		c->catch = saved;
		PRINTF("goto __C3_LABEL_%d;\n", end_label);
		c_emit_label(c, else_label);
		CValue right_val = {0};
		c_emit_expr(c, &right_val, exprptr(binary->right));
		c_value_rvalue(c, &right_val);
		if (!is_void)
		{
			c_emit_assign_var(c, temp, res_type, &right_val);
		}
		c_emit_label(c, end_label);
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

	Type *expr_type = c_safe_type_lower(expr->type);
	if (expr_type->type_kind == TYPE_VOID)
	{
		expr_type = is_comp ? type_bool : (c_safe_type_lower(exprptr(binary->left)->type));
	}
	CValue left_value = {0}, right_value = {0};
	c_emit_expr(c, &left_value, exprptr(binary->left));
	c_value_rvalue(c, &left_value);
	c_emit_expr(c, &right_value, exprptr(binary->right));
	c_value_rvalue(c, &right_value);

	// A fault in an operand has already jumped to the catch target.
	c_emit_binary_op(c, value, op, expr_type, left_value, right_value, expr->loc);
}

static void c_emit_bitstruct_read_container(GenContext *c, CValue *value, Expr *parent_expr, Decl *bitstruct_decl, Decl *member, Type *member_type, int container_ptr_var, const char *c_tname)
{
	ASSERT(bitstruct_decl->decl_kind == DECL_BITSTRUCT);
	c_declare_bitstruct_accessors(c, bitstruct_decl);
	int temp = c_emit_temp_var(c, value, member_type);
	PRINTF("%s ___var_%d = %s_get_%s((const void*)___var_%d);\n", c_type_name(c, member_type), temp,
	       c_get_decl_name(bitstruct_decl), member->name, container_ptr_var);
}

static void c_emit_bitstruct_write_container(GenContext *c, Expr *parent_expr, Decl *bitstruct_decl, Decl *member, int container_ptr_var, const char *c_tname, CValue *val)
{
	ASSERT(bitstruct_decl->decl_kind == DECL_BITSTRUCT);
	c_value_rvalue(c, val);
	c_ensure_cvalue_var(c, val, c_safe_type_lower(member->type));
	c_declare_bitstruct_accessors(c, bitstruct_decl);
	PRINTF("%s_set_%s((void*)___var_%d, ___var_%d);\n", c_get_decl_name(bitstruct_decl), member->name, container_ptr_var, val->var);
}

void c_emit_inc_dec(GenContext *c, CValue *value, Expr *expr, bool is_post)
{
	UnaryOp uop        = expr->unary_expr.operator;
	Expr *inner        = expr->unary_expr.expr;

	if (inner->expr_kind == EXPR_BITACCESS)
	{
		Expr *parent_expr = inner->access_resolved_expr.parent;
		Decl *member      = inner->access_resolved_expr.ref;
		Type *member_type = c_safe_type_lower(inner->type);
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

		CValue one = {0};
		PRINTF("%s ___var_%d = 1;\n", mname, c_emit_temp_var(c, &one, member_type));
		CValue new_val = {0};
		c_emit_binary_op(c, &new_val, uop == UNARYOP_INC ? BINARYOP_ADD : BINARYOP_SUB, member_type, cur_val, one, expr->loc);
		c_emit_bitstruct_write_container(c, parent_expr, bitstruct_decl, member, container_ptr_var, c_tname, &new_val);
		*value = is_post ? cur_val : new_val;
		return;
	}

	Type *target_type = c_safe_type_lower(expr->type);
	if (target_type->type_kind == TYPE_VOID && inner)
	{
		target_type = c_safe_type_lower(inner->type);
	}
	if (target_type->type_kind == TYPE_VOID)
	{
		target_type = type_int;
	}

	// `x++` is `x = x + 1` with a single evaluation of the address.
	CValue old_val  = {0};
	CValue addr_val = {0};
	c_emit_lvalue_load_for_update(c, inner, target_type, &addr_val, &old_val);
	// The step: 1 of the lane type, or an integer 1 for pointers (C scales it by the pointee size, as LLVM's GEP).
	Type *lane_type = c_type_is_vec_or_arr(target_type) ? c_safe_type_lower(target_type->array.base) : target_type;
	Type *one_type  = c_type_is_c_pointer(lane_type) ? type_sz : lane_type;
	CValue one = {0};
	PRINTF("%s ___var_%d = 1;\n", c_type_name(c, one_type), c_emit_temp_var(c, &one, one_type));
	CValue new_val = {0};
	c_emit_binary_op(c, &new_val, uop == UNARYOP_INC ? BINARYOP_ADD : BINARYOP_SUB, target_type, old_val, one, expr->loc);
	c_emit_lvalue_assign(c, inner, addr_val.var ? &addr_val : NULL, &new_val, NULL);
	*value = is_post ? old_val : new_val;
}

// `tmp = (T)x` for the numeric and pointer conversions of cast expressions (ext/trunc, int<->float, enum from
// ordinal, int<->pointer). Through uintptr_t when only one side is a pointer in C (pointers, typeid, fault).
// `sext_bool`: a signed extension from bool gives -1 for true, as LLVM's sext from i1.
static void c_emit_scalar_convert(GenContext *c, int temp, Type *to, CValue *from, bool sext_bool)
{
	const char *tname = c_type_name(c, to);
	if (sext_bool)
	{
		PRINTF("___var_%d = ___var_%d ? (%s)-1 : 0;\n", temp, from->var, tname);
		return;
	}
	bool through_int = c_type_is_c_pointer(to) != c_type_is_c_pointer(from->type);
	PRINTF("___var_%d = (%s)%s___var_%d;\n", temp, tname, through_int ? "(uintptr_t)" : "", from->var);
}

// `tmp` holds the bits of `from` (EXPR_RECAST: same size, other type).
static void c_emit_reinterpret(GenContext *c, int temp, Type *to, CValue *from)
{
	ASSERT(type_size(to) == type_size(c_safe_type_lower(from->type)));
	PRINTF("__c3_memcpy(&___var_%d, &___var_%d, sizeof(___var_%d));\n", temp, from->var, temp);
}

// A bool vector of type `result_type` from the lanes of the vector `inner`: `lane != 0`, or `lane == 0` when `negate`.
static void c_emit_vector_truth(GenContext *c, CValue *value, Type *result_type, CValue *inner, bool negate)
{
	c_ensure_cvalue_var(c, inner, result_type);
	c_emit_lanewise(c, c_emit_temp_var(c, value, result_type), result_type, negate ? "!$0" : "!!$0", inner, 1);
}

static void c_emit_vec_recast(GenContext *c, int temp, const char *tname, Type *target_type, CValue *inner_val, bool is_bitcast)
{
	int t_len    = (int)target_type->array.len;
	int i_len    = (int)inner_val->type->array.len;
	Type *t_base = target_type->array.base ? c_safe_type_lower(target_type->array.base) : type_void;
	Type *i_base = inner_val->type->array.base ? c_safe_type_lower(inner_val->type->array.base) : type_void;
	if (t_len == i_len && (t_base->canonical == i_base->canonical || (is_bitcast && type_size(t_base) == type_size(i_base))))
	{
		PRINTF("%s ___var_%d;\n", tname, temp);
		PRINTF("__c3_memcpy(&___var_%d, &___var_%d, sizeof(%s));\n", temp, inner_val->var, tname);
		return;
	}
	ASSERT(t_len == i_len);
	const char *base_tname = c_type_name(c, t_base);
	const char *lane;
	if (i_base->type_kind == TYPE_BOOL && type_is_integer(t_base))
	{
		lane = str_printf("$0 ? (%s)-1 : 0", base_tname);
	}
	else if (i_base->type_kind == TYPE_BOOL && type_is_float(t_base))
	{
		lane = "$0 ? 1.0 : 0.0";
	}
	else if (t_base->type_kind == TYPE_BOOL)
	{
		lane = "($0 != 0)";
	}
	else
	{
		lane = str_printf("(%s)$0", base_tname);
	}
	c_emit_lanewise(c, temp, target_type, lane, inner_val, 1);
}

// The parts of a slice expression, as llvm_emit_slice_values: the element pointer of the parent and the
// start and end indices (size_t variables). `exclusive`: end is one past the last element.
// `is_signed`: an index expression is signed, so the indices compare as int64_t.
typedef struct
{
	int base_ptr_var;
	int start_idx_var;
	int end_idx_var;
	bool exclusive;
	bool is_signed;
	Type *elem_type;
	Type *target_type;
} CSliceValues;

static CSliceValues c_emit_slice_values(GenContext *c, Expr *expr)
{
	Expr *parent_expr = exprptr(expr->slice_expr.expr);
	Type *parent_type = c_expr_type(parent_expr);
	if (parent_type->type_kind == TYPE_VOID)
	{
		parent_type = type_chars;
	}
	Type *target_type = c_expr_type(expr);
	Type *elem_type   = (target_type && target_type->type_kind == TYPE_SLICE) ? c_safe_type_lower(target_type->array.base) : type_void;
	if (elem_type->type_kind == TYPE_VOID)
	{
		if ((parent_type->type_kind == TYPE_SLICE || parent_type->type_kind == TYPE_ARRAY))
		{
			elem_type = c_safe_type_lower(parent_type->array.base);
		}
		else if (parent_type->type_kind == TYPE_POINTER)
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
	Range range      = expr->slice_expr.range;
	int len_var      = 0;
	bool has_len     = false;
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
		has_len = true;
	}
	else
	{
		CValue parent_addr = {0};
		c_emit_lvalue_addr(c, parent_expr, &parent_addr, type_get_ptr(parent_type));
		PRINTF("%s* ___var_%d = (%s*)(((%s*)___var_%d)->ptr);\n",
		       elem_tname, base_ptr_var, elem_tname, c_type_name(c, parent_type), parent_addr.var);
		len_var = c_create_variable(c);
		PRINTF("size_t ___var_%d = %llu;\n", len_var, (unsigned long long)parent_type->array.len);
		has_len = parent_type->type_kind != TYPE_FLEXIBLE_ARRAY;
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

	// The indices are held as size_t, the checks compare them with the signedness of their expressions.
	bool is_safe       = safe_mode_enabled();
	bool is_dynamic    = range.range_type != RANGE_CONST_RANGE;
	bool start_signed  = is_dynamic && range.start && type_is_signed(c_safe_type_lower(exprptr(range.start)->type));
	Type *start_type   = start_signed ? type_long : type_ulong;
	const char *start_str = str_printf("(%s)___var_%d", start_signed ? "int64_t" : "uint64_t", start_idx_var);
	const char *len_str   = str_printf("(%s)___var_%d", start_signed ? "int64_t" : "uint64_t", len_var);
	CPanicValue start_value = { start_str, start_type };
	CPanicValue len_value   = { len_str, start_type };
	if (is_safe && has_len)
	{
		c_emit_panic_if(c, str_printf("%s > %s", start_str, len_str), PANIC_FN_SLICE_START, expr->loc, NULL, &len_value, &start_value);
	}
	if (is_safe && start_signed && parent_type->type_kind != TYPE_POINTER)
	{
		c_emit_panic_if(c, str_printf("%s < 0", start_str), PANIC_FN_SLICE_START_NEGATIVE, expr->loc, NULL, &start_value, NULL);
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
		if (is_safe)
		{
			bool end_signed = range.range_type == RANGE_DYNAMIC && type_is_signed(c_safe_type_lower(exprptr(range.end)->type));
			bool any_signed = start_signed || end_signed;
			Type *type      = any_signed ? type_long : type_ulong;
			const char *cast = any_signed ? "int64_t" : "uint64_t";
			const char *start_cmp = str_printf("(%s)___var_%d", cast, start_idx_var);
			const char *end_cmp   = str_printf("(%s)___var_%d", cast, end_idx_var);
			const char *len_cmp   = str_printf("(%s)___var_%d", cast, len_var);
			CPanicValue start_v = { start_cmp, type };
			CPanicValue end_v   = { end_cmp, type };
			CPanicValue len_v   = { len_cmp, type };
			if (range.is_len)
			{
				CPanicValue given_len = { str_printf("%s - %s", end_cmp, start_cmp), type };
				c_emit_panic_if(c, str_printf("%s > %s", start_cmp, end_cmp), PANIC_FN_SLICE_LEN_NEGATIVE, expr->loc, NULL, &given_len, NULL);
				if (has_len)
				{
					CPanicValue last = { str_printf("%s - 1", end_cmp), type };
					c_emit_panic_if(c, str_printf("%s > %s", end_cmp, len_cmp), PANIC_FN_SLICE_END, expr->loc, NULL, &last, &len_v);
				}
			}
			else
			{
				c_emit_panic_if(c, str_printf("%s > %s + 1", start_cmp, end_cmp), PANIC_FN_SLICE_RANGE_NEGATIVE, expr->loc, NULL, &start_v, &end_v);
				if (has_len)
				{
					c_emit_panic_if(c, str_printf("%s >= %s", end_cmp, len_cmp), PANIC_FN_SLICE_END, expr->loc, NULL, &end_v, &len_v);
				}
			}
		}
	}
	else
	{
		PRINTF("size_t ___var_%d = ___var_%d;\n", end_idx_var, len_var);
	}


	bool end_signed = has_end && range.range_type == RANGE_DYNAMIC && type_is_signed(c_safe_type_lower(exprptr(range.end)->type));
	return (CSliceValues){ .base_ptr_var  = base_ptr_var,
	                       .start_idx_var = start_idx_var,
	                       .end_idx_var   = end_idx_var,
	                       .exclusive     = range.is_len || !has_end || range.range_type == RANGE_CONST_RANGE,
	                       .is_signed     = start_signed || end_signed,
	                       .elem_type     = elem_type,
	                       .target_type   = target_type };
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
		{
			CValue first = {0};
			c_emit_expr(c, &first, expr->two_expr.first);
			c_emit_expr(c, value, expr->two_expr.last);
			return;
		}
		case EXPR_TYPEID:
		{
			Type *t = expr->type_expr ? expr->type_expr->type : expr->type;
			if (!t)
			{
				t = type_void;
			}
			int temp = c_emit_temp_var(c, value, type_typeid);
			PRINTF("c3typeid_t ___var_%d = (c3typeid_t)&%s;\n", temp, c_typeid_ref(c, t));
			return;
		}
		case EXPR_SLICE_ASSIGN:
		{
			// As llvm_emit_slice_assign: the value first, then `for (i = start; i <= end; i++) ptr[i] = value`,
			// which stores nothing for a reversed range.
			CValue src_val = {0};
			c_emit_expr(c, &src_val, exprptr(expr->slice_assign_expr.right));
			c_value_rvalue(c, &src_val);
			c_ensure_cvalue_var(c, &src_val, NULL);
			CSliceValues slice = c_emit_slice_values(c, exprptr(expr->slice_assign_expr.left));
			const char *cast   = slice.is_signed ? "int64_t" : "uint64_t";
			int loop_i         = c_create_variable(c);
			PRINTF("for (size_t ___var_%d = ___var_%d; (%s)___var_%d %s (%s)___var_%d; ___var_%d++) { ___var_%d[___var_%d] = ___var_%d; }\n",
			       loop_i, slice.start_idx_var, cast, loop_i, slice.exclusive ? "<" : "<=", cast, slice.end_idx_var,
			       loop_i, slice.base_ptr_var, loop_i, src_val.var);
			*value = src_val;
			return;
		}
		case EXPR_MACRO_BODY_EXPANSION:
			c_emit_macro_body_expansion(c, value, expr);
			return;
		case EXPR_UNARY:
		{
			UnaryOp uop       = expr->unary_expr.operator;
			Expr *inner       = expr->unary_expr.expr;
			Type *target_type = c_safe_type_lower(expr->type);
			if (target_type->type_kind == TYPE_VOID && inner)
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
				c_emit_inc_dec(c, value, expr, false);
				return;
			}

			if (uop == UNARYOP_NOT)
			{
				CValue inner_val = {0};
				c_emit_expr(c, &inner_val, inner);
				c_value_rvalue(c, &inner_val);
				if (c_type_is_vec_or_arr(target_type))
				{
					// As LLVM: lane-wise `lane == 0`, giving a bool vector (sema's type for `!vec`).
					c_emit_vector_truth(c, value, target_type, &inner_val, true);
					return;
				}
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
				PRINTF("%s ___var_%d;\n", tname, temp);
				c_emit_panic_if(c, str_printf("__builtin_sub_overflow(0, ___var_%d, &___var_%d)", inner_val.var, temp), PANIC_FN_NEG_OVERFLOW, expr->loc, NULL, NULL, NULL);
				return;
			}

			const char *op_str = (uop == UNARYOP_NOT) ? "!" : (uop == UNARYOP_BITNEG) ? "~"
			                                              : (uop == UNARYOP_PLUS)     ? "+"
			                                                                          : "-";
			if (target_type->type_kind == TYPE_ARRAY || target_type->type_kind == TYPE_VECTOR || target_type->type_kind == TYPE_SIMD_VECTOR)
			{
				c_emit_lanewise(c, temp, target_type, str_printf("%s$0", op_str), &inner_val, 1);
				return;
			}
			PRINTF("%s ___var_%d = %s___var_%d;\n", tname, temp, op_str, inner_val.var);
			return;
		}
		case EXPR_POST_UNARY:
			c_emit_inc_dec(c, value, expr, true);
			return;
		case EXPR_ACCESS_RESOLVED:
		case EXPR_SUBSCRIPT:
		{
			Type *target_type = c_safe_type_lower(expr->type);
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
			Type *member_type = c_safe_type_lower(expr->type);
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
			Type *member_type = c_safe_type_lower(left->type);
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

				c_emit_binary_op(c, &result_val, binaryop_assign_base_op(op), member_type, cur_val, right_val, expr->loc);
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
			Type *target_type = c_safe_type_lower(expr->type);
			c_emit_lvalue_addr(c, expr, value, target_type);
			return;
		}
		case EXPR_PTR_ACCESS:
		{
			CValue inner_val = {0};
			c_emit_expr(c, &inner_val, expr->inner_expr);
			c_value_rvalue(c, &inner_val);
			c_ensure_cvalue_var(c, &inner_val, type_chars);

			Type *target_type = c_safe_type_lower(expr->type);
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

			Type *target_type = c_safe_type_lower(expr->type);
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
			CSliceValues slice = c_emit_slice_values(c, expr);
			int temp           = c_emit_temp_var(c, value, slice.target_type);
			const char *tname  = c_type_name(c, slice.target_type);
			int slice_len_var  = c_create_variable(c);
			PRINTF("size_t ___var_%d = (___var_%d - ___var_%d)%s;\n", slice_len_var, slice.end_idx_var, slice.start_idx_var, slice.exclusive ? "" : " + 1");
			PRINTF("%s ___var_%d = (%s){ .ptr = ___var_%d + ___var_%d, .len = ___var_%d };\n",
			       tname, temp, tname, slice.base_ptr_var, slice.start_idx_var, slice_len_var);
			return;
		}
		case EXPR_MAKE_SLICE:
		{
			Type *target_type = c_safe_type_lower(expr->type);
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
			// The source is evaluated before the destination, as in the LLVM backend.
			CValue dst_val = {0}, src_val = {0};
			c_emit_expr(c, &src_val, exprptr(expr->slice_assign_expr.right));
			c_value_rvalue(c, &src_val);
			c_emit_expr(c, &dst_val, exprptr(expr->slice_assign_expr.left));
			c_value_rvalue(c, &dst_val);
			if (dst_val.var != 0 && src_val.var != 0)
			{
				if (safe_mode_enabled())
				{
					CPanicValue to_len   = { str_printf("___var_%d.len", dst_val.var), type_usz };
					CPanicValue from_len = { str_printf("___var_%d.len", src_val.var), type_usz };
					c_emit_panic_if(c, str_printf("%s != %s", to_len.expr, from_len.expr), PANIC_FN_SLICE_COPY, expr->loc, NULL, &to_len, &from_len);
				}
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
			Type *target_type = c_safe_type_lower(expr->type);
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
			Type *target_type = c_safe_type_lower(expr->type);
			if (target_type->type_kind == TYPE_VOID)
			{
				target_type = type_int;
			}
			int temp          = c_emit_temp_var(c, value, target_type);
			const char *tname = c_type_name(c, target_type);
			c_emit_lanewise(c, temp, target_type, "$0", &inner_val, 1);
			return;
		}
		case EXPR_ADDR_CONVERSION:
		{
			// A struct converted to its inline first member. As LLVM, the struct's memory is read as the member type.
			CValue inner_val = {0};
			c_emit_expr(c, &inner_val, expr->inner_expr);
			c_value_rvalue(c, &inner_val);
			c_ensure_cvalue_var(c, &inner_val, expr->inner_expr->type);
			Type *target_type = c_expr_type(expr);
			c_emit_type_forward_decl(c, target_type);
			int temp = c_emit_temp_var(c, value, target_type);
			PRINTF("%s ___var_%d;\n", c_type_name(c, target_type), temp);
			PRINTF("__c3_memcpy(&___var_%d, &___var_%d, sizeof(___var_%d));\n", temp, inner_val.var, temp);
			return;
		}
		case EXPR_VECTOR_FROM_ARRAY:
		case EXPR_VECTOR_TO_ARRAY:
		case EXPR_RECAST:
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
			Type *target_type = c_expr_type(expr);

			if (expr->expr_kind == EXPR_ENUM_FROM_ORD && safe_mode_enabled())
			{
				// The ordinal must be in 0 ..< number of enum values.
				Decl *decl            = type_flatten(expr->type)->decl;
				unsigned count        = vec_size(decl->enums.values);
				Type *ordinal_type    = c_safe_type_lower(inner_val.type);
				const char *ordinal   = str_printf("___var_%d", inner_val.var);
				CPanicValue ord_value = { ordinal, ordinal_type };
				CPanicValue max_value = { str_printf("%u", count - 1), ordinal_type };
				if (type_is_signed(ordinal_type))
				{
					c_emit_panic_if(c, str_printf("%s < 0", ordinal), PANIC_FN_ENUM_NEGATIVE, expr->loc, decl->name, &ord_value, NULL);
				}
				c_emit_panic_if(c, str_printf("%s >= %u", ordinal, count), PANIC_FN_ENUM_RANGE, expr->loc, decl->name, &ord_value, &max_value);
			}

			if (inner_val.var != 0 && !c_type_is_aggregate(target_type) && inner_val.type &&
			    (target_type == inner_val.type || strcmp(c_type_name(c, target_type), c_type_name(c, inner_val.type)) == 0))
			{
				*value = inner_val;
				return;
			}

			int temp          = c_emit_temp_var(c, value, target_type);
			const char *tname = c_type_name(c, target_type);

			if (inner_val.var == 0)
			{
				c_emit_var_zero_init(c, temp, target_type);
				return;
			}

			if (c_type_is_vec_or_arr(target_type) && inner_val.type && c_type_is_vec_or_arr(inner_val.type))
			{
				c_emit_vec_recast(c, temp, tname, target_type, &inner_val, expr->expr_kind == EXPR_RECAST);
				return;
			}

			PRINTF("%s ___var_%d;\n", tname, temp);
			if (expr->expr_kind == EXPR_RECAST)
			{
				c_emit_reinterpret(c, temp, target_type, &inner_val);
				return;
			}
			bool sext_bool = expr->expr_kind == EXPR_EXT_TRUNC && expr->ext_trunc_expr.is_signed && type_flatten(inner_val.type)->type_kind == TYPE_BOOL;
			c_emit_scalar_convert(c, temp, target_type, &inner_val, sext_bool);
			return;
		}
		case EXPR_INT_TO_BOOL:
		{
			CValue inner_val = {0};
			c_emit_expr(c, &inner_val, expr->int_to_bool_expr.inner);
			c_value_rvalue(c, &inner_val);
			Type *result_type = c_expr_type(expr);
			if (c_type_is_vec_or_arr(result_type))
			{
				// As LLVM: lane-wise `lane != 0` (`== 0` when negated) for int and pointer vectors.
				c_emit_vector_truth(c, value, result_type, &inner_val, expr->int_to_bool_expr.negate);
				return;
			}
			int temp = c_emit_temp_var(c, value, type_bool);
			if (inner_val.var == 0)
			{
				PRINTF("bool ___var_%d = %s;\n", temp, expr->int_to_bool_expr.negate ? "true" : "false");
			}
			else if (c_safe_type_lower(inner_val.type)->type_kind == TYPE_ARRAY)
			{
				// A bitstruct with a char array container, as LLVM: true when any byte is set.
				Type *arr = c_safe_type_lower(inner_val.type);
				ASSERT(arr->array.base == type_char || arr->array.base == type_ichar);
				scratch_buffer_clear();
				for (ArrayIndex i = 0; i < arr->array.len; i++)
				{
					scratch_buffer_printf("%s___var_%d.ptr[%u]", i ? " | " : "", inner_val.var, (unsigned)i);
				}
				PRINTF("bool ___var_%d = ((%s) %s 0);\n", temp, scratch_buffer_to_string(), expr->int_to_bool_expr.negate ? "==" : "!=");
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
			Type *target_type = c_safe_type_lower(expr->type);
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
					CValue ignored = {0};
					c_emit_expr(c, &ignored, exprptr(expr->ternary_expr.then_expr));
				}
				PRINT("} else {\n");
				if (expr->ternary_expr.else_expr)
				{
					CValue ignored = {0};
					c_emit_expr(c, &ignored, exprptr(expr->ternary_expr.else_expr));
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
			Type *target_type = c_safe_type_lower(expr->type);
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
				c_declare_bitstruct_accessors(c, d);
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
					c_emit_store(c, str_printf("___var_%d.ptr[%d]", temp, i), target_type->array.base, &el_val);
				}
			}
			else if (target_type->type_kind == TYPE_SLICE)
			{
				Type *base_type        = c_safe_type_lower(target_type->array.base);
				int arr_len            = count > 0 ? count : 1;
				VariableId arr_var     = c_emit_alloca_array(c, base_type, arr_len);
				for (int i = 0; i < count; i++)
				{
					CValue el_val = {0};
					c_emit_expr(c, &el_val, elements[i]);
					c_value_rvalue(c, &el_val);
					c_emit_store(c, str_printf("___var_%d[%d]", arr_var, i), base_type, &el_val);
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
					Decl *m  = (d && d->strukt.members && i < vec_size(d->strukt.members)) ? d->strukt.members[i] : NULL;
					c_emit_store(c, str_printf("___var_%d.m%d", temp, i), m ? c_decl_type(m) : el_val.type, &el_val);
				}
			}
			return;
		}
		case EXPR_DESIGNATED_INITIALIZER_LIST:
		{
			Type *raw_type    = expr->type ? type_flatten(expr->type) : NULL;
			Type *target_type = c_safe_type_lower(expr->type);
			if (target_type->type_kind == TYPE_VOID)
			{
				target_type = type_int;
			}
			int temp          = c_emit_temp_var(c, value, target_type);
			const char *tname = c_type_name(c, target_type);
			if (expr->designated_init.splat)
			{
				PRINTF("%s ___var_%d;\n", tname, temp);
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
				c_declare_bitstruct_accessors(c, d);
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
							if (m)
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
						c_declare_bitstruct_accessors(c, bitstruct_decl);
						const char *m_name  = bitstruct_member->name ? bitstruct_member->name : "_anon";
						PRINTF("%s_set_%s((void*)&___var_%d%s, ___var_%d);\n",
						       bs_name, m_name, temp, path_buf, dval.var);
					}
					else
					{
						c_emit_store(c, str_printf("___var_%d%s", temp, path_buf), curr_type, &dval);
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
		{
			// Sema's pointer (and pointer vector) casts: the value takes the expression's type, as LLVM.
			// C needs the cast that LLVM's opaque pointers do not.
			c_emit_expr(c, value, expr->inner_expr);
			c_value_rvalue(c, value);
			Type *target_type = c_expr_type(expr);
			if (value->var == 0 || strcmp(c_type_name(c, target_type), c_type_name(c, value->type)) == 0)
			{
				value->type = target_type;
				return;
			}
			CValue inner_val = *value;
			c_emit_type_forward_decl(c, target_type);
			int temp          = c_emit_temp_var(c, value, target_type);
			const char *tname = c_type_name(c, target_type);
			if (c_type_is_vec_or_arr(target_type))
			{
				const char *lane_tname = c_type_name(c, c_safe_type_lower(target_type->array.base));
				c_emit_lanewise(c, temp, target_type, str_printf("(%s)$0", lane_tname), &inner_val, 1);
				return;
			}
			PRINTF("%s ___var_%d = (%s)___var_%d;\n", tname, temp, tname, inner_val.var);
			return;
		}
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
			Type *target_type = c_safe_type_lower(expr->type);
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
			Type *target_type = c_safe_type_lower(expr->type);
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
					const char *sym  = c_typeid_ref(c, inner_type);
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
			Type *target_type  = c_safe_type_lower(expr->type);
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
			Type *target_type = c_safe_type_lower(expr->type);
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

			int macro_exit    = c_create_label(c);
			CCatch outer      = c->catch;
			CBlockExit c_exit = {.return_out = ret_var, .return_type = target_type, .return_exit = macro_exit, .catch = outer};
			if (expr->macro_block.block_exit)
			{
				*expr->macro_block.block_exit = (BlockExit *)&c_exit;
			}

			// A fault in an argument jumps to the outer catch target.
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
			}

			// The body only sees faults through its block exit statements.
			c->catch = NO_CATCH;
			if (expr->macro_block.first_stmt)
			{
				c_emit_stmt_chain(c, expr->macro_block.first_stmt);
			}

			c_emit_label(c, macro_exit);
			c->catch = outer;
			return;
		}
		case EXPR_POINTER_OFFSET:
		{
			CValue ptr_val = {0}, offset_val = {0};
			c_emit_expr(c, &ptr_val, exprptr(expr->pointer_offset_expr.ptr));
			c_value_rvalue(c, &ptr_val);
			c_emit_expr(c, &offset_val, exprptr(expr->pointer_offset_expr.offset));
			c_value_rvalue(c, &offset_val);
			Type *target_type = c_safe_type_lower(expr->type);
			if (target_type->type_kind == TYPE_VOID)
			{
				target_type = type_voidptr;
			}
			int temp          = c_emit_temp_var(c, value, target_type);
			const char *tname = c_type_name(c, target_type);
			if (c_type_is_aggregate(target_type))
			{
				Type *elem_t       = target_type->array.base;
				bool is_void_ptr   = elem_t->type_kind == TYPE_POINTER && elem_t->pointer->type_kind == TYPE_VOID;
				CValue operands[2] = { ptr_val, offset_val };
				c_emit_lanewise(c, temp, target_type, is_void_ptr ? "(void*)(((char*)$0) + $1)" : "$0 + $1", operands, 2);
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
			Type *target_type = c_safe_type_lower(expr->type);
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
			int err_var = c_create_variable(c);
			const char *err = str_printf("___var_%d", err_var);
			int panic_label = c_create_label(c);
			int ok_label    = c_create_label(c);
			PRINTF("c3fault_t %s = NULL;\n", err);
			CCatch saved = c->catch;
			c->catch     = (CCatch){ panic_label, err };
			c_emit_expr(c, value, expr->inner_expr);
			c_value_rvalue(c, value);
			c->catch = saved;
			PRINTF("goto __C3_LABEL_%d;\n", ok_label);
			c_emit_label(c, panic_label);
			c_emit_panic_fn(c, PANIC_FN_UNWRAP, expr->loc, NULL, &(CPanicValue){ err, type_fault });
			c_emit_label(c, ok_label);
			return;
		}
		case EXPR_OPTIONAL:
		{
			Type *target_type = c_safe_type_lower(expr->type);
			CValue inner_val  = {0};
			c_emit_expr(c, &inner_val, expr->inner_expr);
			c_value_rvalue(c, &inner_val);
			c_emit_fault_to_optional_exit(c, str_printf("___var_%d", inner_val.var));
			if (target_type->type_kind != TYPE_VOID)
			{
				int temp = c_emit_temp_var(c, value, target_type);
				c_emit_var_zero_init(c, temp, target_type);
			}
			else
			{
				c_value_set_void(value);
			}
			return;
		}
		case EXPR_RETHROW:
		{
			int err_var = c_create_variable(c);
			const char *err = str_printf("___var_%d", err_var);
			int guard_label = c_create_label(c);
			int noerr_label = c_create_label(c);
			PRINTF("c3fault_t %s = NULL;\n", err);
			CCatch saved = c->catch;
			c->catch     = (CCatch){ guard_label, err };
			c_emit_expr(c, value, expr->rethrow_expr.inner);
			c_value_rvalue(c, value);
			c->catch = saved;
			PRINTF("goto __C3_LABEL_%d;\n", noerr_label);
			c_emit_label(c, guard_label);
			if (expr->rethrow_expr.cleanup)
			{
				const char *saved_defer_error = c->defer_error_var;
				c->defer_error_var            = err;
				PRINT("{\n");
				c_emit_stmt_chain(c, expr->rethrow_expr.cleanup);
				PRINT("}\n");
				c->defer_error_var = saved_defer_error;
			}
			if (expr->rethrow_expr.in_block)
			{
				CBlockExit *exit = (CBlockExit *)*expr->rethrow_expr.in_block;
				ASSERT(exit->catch.label && "Optional evaluated without a catch target");
				if (exit->catch.fault) PRINTF("%s = %s;\n", exit->catch.fault, err);
				PRINTF("goto __C3_LABEL_%d;\n", exit->catch.label);
			}
			else
			{
				c_emit_fault_return(c, err);
			}
			c_emit_label(c, noerr_label);
			// An inner expression of wildcard type always fails (`FAULT~!`), and sema gives the rethrow whatever type its
			// context needs. Nothing reaches this point; the value only has to have that type.
			if (type_no_optional(expr->rethrow_expr.inner->type)->canonical->type_kind == TYPE_WILDCARD)
			{
				Type *type = c_safe_type_lower(expr->type);
				if (type->type_kind == TYPE_VOID)
				{
					c_value_set_void(value);
				}
				else
				{
					c_emit_var_zero_init(c, c_emit_temp_var(c, value, type), type);
				}
			}
			return;
		}
		case EXPR_TRY:
		{
			value->var  = c_create_variable(c);
			value->type = type_bool;
			value->kind = CV_VALUE;
			if (!expr->try_expr.optional)
			{
				const char *fault = c_decl_fault_ref(c, expr->try_expr.decl);
				PRINTF("bool ___var_%d = %s;\n", value->var, fault ? str_printf("(%s == NULL)", fault) : "true");
				return;
			}
			int catch_label = c_create_label(c);
			int end_label   = c_create_label(c);
			PRINTF("bool ___var_%d;\n", value->var);
			CCatch saved = c->catch;
			c->catch     = (CCatch){ catch_label, NULL };
			CValue opt_val = {0};
			c_emit_expr(c, &opt_val, expr->try_expr.optional);
			c_value_rvalue(c, &opt_val);
			if (expr->try_expr.assign_existing && expr->try_expr.lhs)
			{
				c_emit_lvalue_assign(c, expr->try_expr.lhs, NULL, &opt_val, NULL);
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
			c->catch = saved;
			PRINTF("___var_%d = true;\ngoto __C3_LABEL_%d;\n", value->var, end_label);
			c_emit_label(c, catch_label);
			PRINTF("___var_%d = false;\n", value->var);
			c_emit_label(c, end_label);
			return;
		}
		case EXPR_CATCH:
		{
			const char *fault;
			if (expr->catch_expr.decl)
			{
				Type *dt = c_decl_type(expr->catch_expr.decl);
				if (dt->type_kind == TYPE_VOID)
				{
					expr->catch_expr.decl->type = type_fault;
				}
				fault = str_printf("___var_%d", c_get_or_create_decl_var(c, expr->catch_expr.decl));
			}
			else
			{
				fault = str_printf("___var_%d", c_create_variable(c));
				PRINTF("c3fault_t %s;\n", fault);
			}
			int end_label = c_create_label(c);
			CCatch saved  = c->catch;
			c->catch      = (CCatch){ end_label, fault };
			FOREACH(Expr *, e, expr->catch_expr.exprs)
			{
				CValue val = {0};
				c_emit_expr(c, &val, e);
				c_value_rvalue(c, &val);
			}
			c->catch = saved;
			PRINTF("%s = NULL;\n", fault);
			c_emit_label(c, end_label);
			int temp = c_emit_temp_var(c, value, type_fault);
			PRINTF("c3fault_t ___var_%d = (c3fault_t)%s;\n", temp, fault);
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
				PRINTF("___var_%d = ___var_%d;\n", temp, eval.var);
				PRINT("}\n");
			}
			return;
		}
		case EXPR_LAST_FAULT:
		{
			ASSERT(c->defer_error_var);
			int temp = c_emit_temp_var(c, value, type_fault);
			PRINTF("c3fault_t ___var_%d = (c3fault_t)%s;\n", temp, c->defer_error_var);
			return;
		}
		case EXPR_RETVAL:
		{
			if (c->retval.var != 0)
			{
				*value = c->retval;
				return;
			}
			Type *target_type = c_safe_type_lower(expr->type);
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
			Type *target_type = c_safe_type_lower(expr->type);
			if (target_type->type_kind == TYPE_VOID)
			{
				target_type = type_sz;
			}
			int temp          = c_emit_temp_var(c, value, target_type);
			const char *tname = c_type_name(c, target_type);
			const char *pname = (parent_val.var != 0) ? str_printf("___var_%d", parent_val.var) : "NULL";
			if (safe_mode_enabled())
			{
				// Each property only exists for some kinds of types.
				static const IntrospectType inner_kinds[] = { INTROSPECT_TYPE_ARRAY, INTROSPECT_TYPE_POINTER, INTROSPECT_TYPE_VECTOR,
				                                              INTROSPECT_TYPE_ENUM, INTROSPECT_TYPE_SLICE, INTROSPECT_TYPE_DISTINCT,
				                                              INTROSPECT_TYPE_CONSTSET, INTROSPECT_TYPE_BITSTRUCT, INTROSPECT_TYPE_OPTIONAL };
				static const IntrospectType names_kinds[] = { INTROSPECT_TYPE_ENUM };
				static const IntrospectType len_kinds[]   = { INTROSPECT_TYPE_ARRAY, INTROSPECT_TYPE_VECTOR, INTROSPECT_TYPE_ENUM, INTROSPECT_TYPE_SLICE };
				const IntrospectType *kinds = NULL;
				int kind_count              = 0;
				PanicFunction panic_fn      = PANIC_FN_ACCESS_INNER;
				switch (expr->typeid_info_expr.kind)
				{
					case TYPEID_INFO_INNER: kinds = inner_kinds; kind_count = ELEMENTLEN(inner_kinds); break;
					case TYPEID_INFO_NAMES: kinds = names_kinds; kind_count = ELEMENTLEN(names_kinds); panic_fn = PANIC_FN_ACCESS_NAMES; break;
					case TYPEID_INFO_LEN: kinds = len_kinds; kind_count = ELEMENTLEN(len_kinds); panic_fn = PANIC_FN_ACCESS_LEN; break;
					default: break;
				}
				if (kinds)
				{
					scratch_buffer_clear();
					for (int i = 0; i < kind_count; i++)
					{
						scratch_buffer_printf("%s((const c3type_info_t*)%s)->kind != %d", i ? " && " : "", pname, (int)kinds[i]);
					}
					c_emit_panic_if(c, scratch_buffer_copy(), panic_fn, expr->loc, NULL, NULL, NULL);
				}
			}
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
			Type *t = c_safe_type_lower(expr->type);
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
	// Every expression produces its value from scratch; emitters set only the fields they use.
	*value = (CValue){0};
	c_emit_expr_internal(c, value, expr);
}

// Evaluate for side effects. A fault in an optional expression is discarded.
void c_emit_ignored_expr(GenContext *c, Expr *expr)
{
	CValue value = {0};
	if (!IS_OPTIONAL(expr))
	{
		c_emit_expr(c, &value, expr);
		return;
	}
	int label    = c_create_label(c);
	CCatch saved = c->catch;
	c->catch     = (CCatch){ label, NULL };
	c_emit_expr(c, &value, expr);
	c->catch = saved;
	c_emit_label(c, label);
}