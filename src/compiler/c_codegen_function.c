#include "c_codegen_internal.h"

AlignSize c_get_effective_alignment(Decl *decl, Type *type)
{
	AlignSize align = decl ? decl->alignment : 0;
	if (!align && decl && c_type_is_resolved(decl->type))
	{
		align = type_alloca_alignment(decl->type);
	}
	if (!align && c_type_is_resolved(type))
	{
		align = type_alloca_alignment(type);
	}
	Type *vt = type ? c_safe_type_lower(type) : type_int;
	if (align < 16 && c_get_type_size(vt) >= 16)
	{
		align = 16;
	}
	return align;
}

static const char *c_local_align_str(Decl *decl, Type *type)
{
	AlignSize align     = c_get_effective_alignment(decl, type);
	Type *vt            = type ? c_safe_type_lower(type) : type_int;
	AlignSize abi_align = c_type_is_resolved(vt) ? type_abi_alignment(vt) : 1;
	return (align > abi_align) ? str_printf("__c3_aligned(%u) ", (unsigned)align) : "";
}

int c_create_label(GenContext *c)
{
	return ++c->id_gen;
}

int c_create_variable(GenContext *c)
{
	return ++c->id_gen;
}

void c_register_function_local(GenContext *c, VariableId id, Type *type, bool is_static, Decl *decl)
{
	if (!id)
	{
		return;
	}
	Type *lowered = c_safe_type_lower(type);
	if (lowered->type_kind == TYPE_VOID)
	{
		lowered = type_int;
	}
	CLocalVar lv = {.id = id, .type = lowered, .is_static = is_static, .is_alloca = false, .alloca_count = 0, .decl = decl};
	vec_add(c->function_locals, lv);
}

VariableId c_emit_alloca(GenContext *c, Type *type)
{
	Type *lowered = c_safe_type_lower(type);
	if (lowered->type_kind == TYPE_VOID)
	{
		lowered = type_int;
	}
	c_emit_type_forward_decl(c, lowered);
	VariableId id = c_create_variable(c);
	CLocalVar lv = {.id = id, .type = lowered, .is_static = false, .is_alloca = true, .alloca_count = 0, .decl = NULL};
	vec_add(c->function_locals, lv);
	return id;
}

VariableId c_emit_alloca_array(GenContext *c, Type *base_type, ArrayIndex count)
{
	Type *lowered = c_safe_type_lower(base_type);
	if (lowered->type_kind == TYPE_VOID)
	{
		lowered = type_int;
	}
	c_emit_type_forward_decl(c, lowered);
	VariableId id = c_create_variable(c);
	CLocalVar lv = {.id = id, .type = lowered, .is_static = false, .is_alloca = true, .alloca_count = count, .decl = NULL};
	vec_add(c->function_locals, lv);
	return id;
}

VariableId c_emit_temp_var(GenContext *c, CValue *value, Type *type)
{
	Type *lowered = c_safe_type_lower(type);
	if (lowered->type_kind == TYPE_VOID)
	{
		lowered = type_int;
	}
	c_emit_type_forward_decl(c, lowered);
	VariableId id = c_create_variable(c);
	c_register_function_local(c, id, lowered, false, NULL);
	if (value)
	{
		*value = (CValue){.var = id, .kind = CV_VALUE, .type = lowered};
	}
	return id;
}

VariableId c_get_or_create_decl_var(GenContext *c, Decl *decl)
{
	if (!decl)
	{
		VariableId v = c_create_variable(c);
		c_register_function_local(c, v, type_int, false, NULL);
		return v;
	}
	decl    = c_decl_unwrap(decl);
	VariableId v = c_ptr_to_id(htable_get(&c->local_vars, decl));
	if (v)
	{
		return v;
	}

	VariableId var = c_create_variable(c);
	htable_set(&c->local_vars, decl, c_id_to_ptr(var));
	Type *var_type = c_decl_type(decl);
	if (!var_type || var_type->type_kind == TYPE_VOID)
	{
		var_type = type_int;
	}
	bool is_static = (decl->decl_kind == DECL_VAR && (decl->var.is_static || decl->var.kind == VARDECL_CONST));
	c_register_function_local(c, var, var_type, is_static, decl);
	if (decl->decl_kind == DECL_VAR && IS_OPTIONAL(decl))
	{
		c_get_decl_fault_var(c, decl);
	}
	return var;
}

VariableId c_get_decl_fault_var(GenContext *c, Decl *decl)
{
	if (!decl)
	{
		return 0;
	}
	decl = decl_raw(decl);
	if (decl->decl_kind != DECL_VAR || decl->var.kind == VARDECL_UNWRAPPED || !IS_OPTIONAL(decl))
	{
		return 0;
	}
	VariableId v = c_ptr_to_id(htable_get(&c->local_fault_vars, decl));
	if (v)
	{
		return v;
	}
	VariableId fvar = c_create_variable(c);
	htable_set(&c->local_fault_vars, decl, c_id_to_ptr(fvar));
	bool is_static = (decl->var.is_static || decl->var.kind == VARDECL_CONST);
	c_register_function_local(c, fvar, type_fault, is_static, decl);
	return fvar;
}

// The C lvalue holding the fault of an optional variable, NULL when the variable has none.
const char *c_decl_fault_ref(GenContext *c, Decl *decl)
{
	decl = decl_raw(decl);
	if (decl->decl_kind != DECL_VAR || decl->var.kind == VARDECL_UNWRAPPED || !IS_OPTIONAL(decl)) return NULL;
	if (c_is_file_global(decl))
	{
		c_emit_global_decl(c, decl);
		return str_printf("%s__f", c_get_decl_name(decl));
	}
	return str_printf("___var_%d", c_get_decl_fault_var(c, decl));
}

// The fault to check when reading the variable `ident` names, NULL when there is none.
// Reading an unwrapped variable (after `try`) checks nothing, while assigning to it writes the original's fault.
const char *c_ident_read_fault_ref(GenContext *c, Decl *ident)
{
	while (ident->replacement) ident = ident->replacement;
	while (ident->decl_kind == DECL_ALIAS) ident = ident->define_decl.alias;
	if (ident->decl_kind == DECL_VAR && ident->var.kind == VARDECL_UNWRAPPED) return NULL;
	return c_decl_fault_ref(c, ident);
}

// Jump to the catch target with the fault `fault`, which is known to be set.
void c_emit_fault_to_optional_exit(GenContext *c, const char *fault)
{
	ASSERT(c->catch.label && "Optional evaluated without a catch target");
	if (c->catch.fault) PRINTF("%s = (c3fault_t)%s;\n", c->catch.fault, fault);
	PRINTF("goto __C3_LABEL_%d;\n", c->catch.label);
}

// Jump to the catch target if the C expression `fault` is set.
void c_emit_jump_to_optional_exit(GenContext *c, const char *fault)
{
	PRINTF("if (__builtin_expect(%s != NULL, 0)) {\n", fault);
	c_emit_fault_to_optional_exit(c, fault);
	PRINT("}\n");
}

// Storing `rhs` into an optional variable whose fault lives in `fault_ref`:
//   c_optional_assign_begin, emit rhs and store the value, c_optional_assign_end.
// A fault in rhs is written to `fault_ref` and skips the store. If the enclosing expression also wants
// the fault it is passed on, otherwise execution continues after the assignment.
void c_optional_assign_begin(GenContext *c, COptionalAssign *assign, const char *fault_ref, Expr *rhs)
{
	*assign = (COptionalAssign){ .saved = c->catch, .fault_ref = fault_ref };
	if (!fault_ref) return;
	if (!IS_OPTIONAL(rhs))
	{
		c->catch = NO_CATCH;
		return;
	}
	assign->after = c_create_label(c);
	if (c->catch.fault) assign->rejump = c_create_label(c);
	c->catch = (CCatch){ assign->rejump ? assign->rejump : assign->after, fault_ref };
}

void c_optional_assign_end(GenContext *c, COptionalAssign *assign)
{
	if (!assign->fault_ref) return;
	PRINTF("%s = NULL;\n", assign->fault_ref);
	c->catch = assign->saved;
	if (!assign->after) return;
	if (assign->rejump)
	{
		PRINTF("goto __C3_LABEL_%d;\n", assign->after);
		c_emit_label(c, assign->rejump);
		c_emit_fault_to_optional_exit(c, assign->fault_ref);
	}
	c_emit_label(c, assign->after);
}

VariableId c_ensure_cvalue_var(GenContext *c, CValue *val, Type *fallback_type)
{
	if (val->var != 0)
	{
		return val->var;
	}

	Type *t = (val->type->type_kind != TYPE_VOID)
	              ? c_safe_type_lower(val->type)
	              : (fallback_type->type_kind != TYPE_VOID ? c_safe_type_lower(fallback_type) : type_int);
	if (t->type_kind == TYPE_VOID)
	{
		t = type_int;
	}

	val->type = t;
	val->kind = CV_VALUE;
	val->var  = c_create_variable(c);
	c_emit_type_forward_decl(c, t);
	c_emit_var_zero_init(c, val->var, t);
	return val->var;
}

void c_value_rvalue(GenContext *c, CValue *val)
{
	if (val && val->fault)
	{
		c_emit_jump_to_optional_exit(c, val->fault);
		val->fault = NULL;
	}
	if (!val || val->var == 0 || val->kind != CV_ADDRESS)
	{
		return;
	}

	Type *t = c_safe_type_lower(val->type);
	if (t->type_kind == TYPE_VOID)
	{
		return;
	}

	c_emit_type_forward_decl(c, t);
	const char *tname = c_type_name(c, t);
	int temp          = c_create_variable(c);
	c_register_function_local(c, temp, t, false, NULL);

	if (c_type_is_aggregate(t))
	{
		PRINTF("%s ___var_%d;\n", tname, temp);
		PRINTF("__c3_memcpy(&___var_%d, (void*)___var_%d, sizeof(%s));\n", temp, val->var, tname);
	}
	else
	{
		PRINTF("%s ___var_%d = *(%s*)___var_%d;\n", tname, temp, tname, val->var);
	}
	val->var  = temp;
	val->kind = CV_VALUE;
}

void c_value_addr(GenContext *c, CValue *val)
{
	if (!val || val->var == 0 || val->kind == CV_ADDRESS)
	{
		return;
	}

	Type *t = c_safe_type_lower(val->type);
	if (t->type_kind == TYPE_VOID)
	{
		t = type_int;
	}

	VariableId temp = c_emit_alloca(c, t);
	c_emit_assign_var(c, temp, t, val);
	val->var  = temp;
	val->kind = CV_ADDRESS;
}

// A lanewise vector result: declares `result_type ___var_<result_var>` and sets every lane k to `tmpl`, where `$<i>`
// stands for lane k of args[i] (a vector) or args[i] itself (a scalar, shared by all lanes), and `$k` for k.
void c_emit_lanewise(GenContext *c, int result_var, Type *result_type, const char *tmpl, CValue *args, int arg_count)
{
	for (int i = 0; i < arg_count; i++) c_value_rvalue(c, &args[i]);
	int len = (int)result_type->array.len;
	ASSERT(c_type_is_vec_or_arr(result_type) && len > 0);
	PRINTF("%s ___var_%d;\n", c_type_name(c, result_type), result_var);
	for (int k = 0; k < len; k++)
	{
		scratch_buffer_push();
		scratch_buffer_clear();
		for (const char *p = tmpl; *p; p++)
		{
			if (p[0] != '$')
			{
				scratch_buffer_append_char(*p);
				continue;
			}
			p++;
			if (*p == 'k')
			{
				scratch_buffer_append_unsigned_int(k);
				continue;
			}
			int i = *p - '0';
			ASSERT(i >= 0 && i < arg_count);
			scratch_buffer_printf("___var_%d", args[i].var);
			if (c_type_is_vec_or_arr(c_safe_type_lower(args[i].type))) scratch_buffer_printf(".ptr[%d]", k);
		}
		const char *lane = scratch_buffer_copy();
		scratch_buffer_pop();
		PRINTF("___var_%d.ptr[%d] = %s;\n", result_var, k, lane);
	}
}

const char *c_arrow(const CValue *val)
{
	return (val && val->kind == CV_ADDRESS) ? "->" : ".";
}

void c_emit_label(GenContext *c, int label_id)
{
	if (!label_id)
	{
		return;
	}
	PRINTF("__C3_LABEL_%d:;\n", label_id);
	c->current_block_live = true;
}

void c_emit_var_zero_init(GenContext *c, VariableId var_id, Type *type)
{
	const char *tname = c_type_name(c, type);
	PRINTF("%s ___var_%d = %s;\n", tname, var_id, c_type_zero_literal(type));
}

// Store `src` into the C lvalue `dst`. Sema makes every conversion explicit, so the source has the destination's
// type (compared by C name: types such as float16 / bfloat or two Type* for one C type are the same in C).
// Only the C form differs: a zero value (var 0), or an aggregate held at an address.
void c_emit_store(GenContext *c, const char *dst, Type *type, CValue *src)
{
	type = c_safe_type_lower(type);
	if (type->type_kind == TYPE_VOID) return;
	if (!src || src->var == 0)
	{
		if (c_type_is_aggregate(type))
		{
			PRINTF("__c3_memset(&%s, 0, sizeof(%s));\n", dst, c_type_name(c, type));
		}
		else
		{
			PRINTF("%s = (%s)%s;\n", dst, c_type_name(c, type), c_type_zero_literal(type));
		}
		return;
	}
	ASSERT(src->type && strcmp(c_type_name(c, type), c_type_name(c, c_safe_type_lower(src->type))) == 0);
	if (src->kind == CV_ADDRESS)
	{
		if (c_type_is_aggregate(type))
		{
			PRINTF("__c3_memcpy(&%s, (void*)___var_%d, sizeof(%s));\n", dst, src->var, c_type_name(c, type));
			return;
		}
		c_value_rvalue(c, src);
	}
	PRINTF("%s = ___var_%d;\n", dst, src->var);
}

void c_emit_assign_var(GenContext *c, VariableId dst_var, Type *dst_type, CValue *src_val)
{
	c_emit_store(c, str_printf("___var_%d", dst_var), dst_type, src_val);
}

static bool c_type_is_zero_sized(Type *type)
{
	return c_type_is_resolved(type) && type_size(type) == 0;
}

// The return convention for a function returning `rtype`, as LLVM's c_abi_func_create.
CRet c_type_ret(Type *rtype)
{
	Type *value = c_safe_type_lower(type_no_optional(rtype));
	if (!value || value->type_kind == TYPE_VOID || c_type_is_zero_sized(value)) value = NULL;
	return (CRet){ .optional = type_is_optional(rtype), .value = value };
}

CRet c_signature_ret(Signature *sig)
{
	return c_type_ret(typeget(sig->rtype));
}

// c == NULL: declares nothing.
const char *c_signature_str(GenContext *c, Signature *sig, const char *declarator, CSigOpts opts)
{
	CRet ret = c_signature_ret(sig);
	if (c && ret.value) c_emit_type_forward_decl(c, ret.value);
	const char *ret_tname = ret.optional ? "c3fault_t" : (ret.value ? c_type_name(c, ret.value) : "void");
	// The value of an optional comes back through a pointer in front of the other parameters.
	const char *params = (ret.optional && ret.value) ? str_printf("%s *%s", c_type_name(c, ret.value), opts.names ? "___ret_out" : "") : "";
	int count          = params[0] ? 1 : 0;
	int arg_index      = 0;
	FOREACH_IDX(i, Decl *, p, sig->params)
	{
		Type *ptype = (i == 0 && opts.self_type) ? opts.self_type : c_decl_type(p);
		if (c_type_is_zero_sized(ptype)) continue;
		if (c) c_emit_type_forward_decl(c, ptype);
		const char *tname = (opts.is_main && (arg_index == 1 || arg_index == 2)) ? "char**" : c_type_name(c, ptype);
		params            = str_printf("%s%s%s%s%s", params, count ? ", " : "", tname, opts.names ? " " : "", opts.names ? opts.names[i] : "");
		count++;
		arg_index++;
	}
	if (sig->variadic == VARIADIC_RAW)
	{
		params = count ? str_printf("%s, ...", params) : "__C3_VARIADIC_ONLY";
	}
	else if (!count)
	{
		params = "void";
	}
	return str_printf("%s %s(%s)", ret_tname, declarator, params);
}

bool c_fn_is_main(Decl *fn)
{
	return strcmp(c_get_decl_name(fn), "main") == 0 && vec_size(fn->func_decl.signature.params) <= 3;
}

static bool c_emit_function_decl_here(GenContext *c, Decl *fn);

bool c_emit_function_decl(GenContext *c, Decl *fn, bool is_current_module)
{
	(void)is_current_module;
	ByteBuffer *prev = c_decl_stream_enter(c, c->decl_buffer);
	bool emitted     = c_emit_function_decl_here(c, fn);
	c->buffer        = prev;
	return emitted;
}

static bool c_emit_function_decl_here(GenContext *c, Decl *fn)
{
	if (!fn || fn->replacement || fn->decl_kind != DECL_FUNC || fn->is_template || fn->func_decl.attr_interface_method)
	{
		return false;
	}
	const char *fn_name = c_get_decl_name(fn);

	if (htable_get(&c->decl_names, (void *)fn_name))
	{
		return false;
	}
	const char *signature = c_signature_str(c, &fn->func_decl.signature, fn_name, (CSigOpts){ .is_main = c_fn_is_main(fn) });
	// The prototype is kept: calls through an extern declaration with another signature cast to their own.
	htable_set(&c->decl_names, (void *)fn_name, (void *)signature);
	PRINTF("%s%s %s", c_decl_export_attrs(fn, false), c_fn_is_internal(fn) ? "static" : "extern", signature);
	if (c_decl_needs_asm_label(fn))
	{
		PRINTF(" __asm__(\"%s\")", c_get_decl_asm_name(fn));
	}
	PRINT(";\n");
	return !fn->is_extern;
}

void c_emit_local_var_declarations(GenContext *c)
{
	FOREACH(CLocalVar, lv, c->function_locals)
	{
		if (!lv.decl && !lv.is_alloca)
		{
			continue;
		}
		if (htable_get(&c->declared_vars, c_id_to_ptr(lv.id)))
		{
			continue;
		}
		htable_set(&c->declared_vars, c_id_to_ptr(lv.id), (void *)1);
		Type *vt = lv.type ? c_safe_type_lower(lv.type) : type_int;
		if (vt->type_kind == TYPE_VOID)
		{
			vt = type_int;
		}
		c_emit_type_forward_decl(c, vt);
		const char *tname     = c_type_name(c, vt);
		const char *align_str = c_local_align_str(lv.decl, vt);

		if (lv.alloca_count > 0)
		{
			PRINTF("\t%s%s ___var_%d[%lld] = {0};\n", align_str, tname, lv.id, (long long)lv.alloca_count);
		}
		else if (c_type_is_aggregate(vt))
		{
			PRINTF("\t%s%s ___var_%d = {0};\n", align_str, tname, lv.id);
		}
		else if (type_is_pointer(vt) || vt->type_kind == TYPE_ANYFAULT || vt->type_kind == TYPE_TYPEID)
		{
			PRINTF("\t%s%s ___var_%d = NULL;\n", align_str, tname, lv.id);
		}
		else
		{
			PRINTF("\t%s%s ___var_%d = 0;\n", align_str, tname, lv.id);
		}
	}
}

void c_emit_function(GenContext *c, Decl *fn)
{
	if (!fn || fn->decl_kind != DECL_FUNC || fn->is_extern || !fn->func_decl.body || fn->func_decl.attr_interface_method)
	{
		return;
	}

	Signature *sig      = &fn->func_decl.signature;
	const char *fn_name = c_get_decl_name(fn);

	c->cur_ret                  = c_signature_ret(sig);
	c->cur_func_name            = fn->name;
	c->cur_func_decl            = fn;
	c->catch                    = NO_CATCH;
	c->defer_error_var          = NULL;
	c->current_break_label      = 0;
	c->current_continue_label   = 0;
	c->current_block_live       = true;
	c->retval                   = (CValue){0};
	c->function_locals          = NULL;

	memset(c->local_vars.entries, 0, (c->local_vars.mask + 1) * sizeof(HTEntry *));
	memset(c->local_fault_vars.entries, 0, (c->local_fault_vars.mask + 1) * sizeof(HTEntry *));
	memset(c->declared_vars.entries, 0, (c->declared_vars.mask + 1) * sizeof(HTEntry *));

	ByteBuffer body_buf;
	byte_buffer_init(&body_buf, 0);
	ByteBuffer *prev_buf = c->buffer;
	c->buffer            = &body_buf;

	FOREACH(Decl *, d, sig->params)
	{
		Type *ptype = c_decl_type(d);
		c_emit_type_forward_decl(c, ptype);
		VariableId vid = c_create_variable(c);
		if (d)
		{
			d = c_decl_unwrap(d);
			htable_set(&c->local_vars, d, c_id_to_ptr(vid));
			if (IS_OPTIONAL(d))
			{
				c_get_decl_fault_var(c, d);
			}
		}
		if (c_type_is_resolved(ptype) && type_size(ptype) == 0)
		{
			c_register_function_local(c, vid, ptype, false, d);
		}
		else
		{
			htable_set(&c->declared_vars, c_id_to_ptr(vid), (void *)1);
		}
		if (d && d->var.not_null && safe_mode_enabled() && type_is_pointer(ptype))
		{
			c_emit_panic_if(c, str_printf("___var_%d == NULL", vid), PANIC_FN_REF_PARAM, d->loc, d->name ? d->name : "(anon)", NULL, NULL);
		}
	}

	if (strcmp(fn_name, "main") == 0 || strcmp(fn_name, "WinMain") == 0 || strcmp(fn_name, "wWinMain") == 0)
	{
		PRINT("\t__c3_init_runtime();\n");
	}
	else if (fn->func_decl.attr_init)
	{
		PRINT("\tstatic bool ran_init = false;\n");
		PRINT("\tif (ran_init) return;\n");
		PRINT("\tran_init = true;\n");
	}

	c_emit_stmt(c, astptrzero(fn->func_decl.body));

	if (c->current_block_live)
	{
		if (c->cur_ret.optional)
		{
			PRINT("return NULL;\n");
		}
		else if (c->cur_ret.value)
		{
			PRINTF("return (%s)%s;\n", c_type_name(c, c->cur_ret.value), c_type_zero_literal(c->cur_ret.value));
		}
		else
		{
			PRINT("return;\n");
		}
	}

	c->buffer = prev_buf;

	FOREACH(CLocalVar, lv, c->function_locals)
	{
		if (htable_get(&c->declared_vars, c_id_to_ptr(lv.id)) || !lv.is_static)
		{
			continue;
		}
		htable_set(&c->declared_vars, c_id_to_ptr(lv.id), (void *)1);
		Type *vt = lv.type ? c_safe_type_lower(lv.type) : type_int;
		if (vt->type_kind == TYPE_VOID)
		{
			vt = type_int;
		}
		c_emit_type_forward_decl(c, vt);
		const char *tname     = c_type_name(c, vt);
		bool is_tls           = lv.decl && lv.decl->var.is_threadlocal;
		const char *align_str = c_local_align_str(lv.decl, vt);
		PRINTF("static %s%s%s ___var_%d", align_str, is_tls ? "__c3_thread_local " : "", tname, lv.id);
		// The fault slot of an optional static starts empty (no fault); the initializer is the value's.
		bool is_fault_slot = lv.decl && c_ptr_to_id(htable_get(&c->local_fault_vars, decl_raw(lv.decl))) == lv.id;
		if (lv.decl && !is_fault_slot && lv.decl->var.init_expr && expr_is_const(lv.decl->var.init_expr))
		{
			PRINT(" = ");
			c_emit_const_init_expr(c, lv.decl->var.init_expr, vt);
			PRINT(";\n");
		}
		else
		{
			PRINT(" = {0};\n");
		}
	}

	if (fn->func_decl.attr_init || fn->func_decl.attr_finalizer)
	{
		uint32_t prio    = fn->func_decl.priority ? fn->func_decl.priority : MAX_PRIORITY;
		int base_prio    = fn->func_decl.attr_init ? 102 : 101;
		int c_prio       = base_prio + (int)((prio - 1) * (65535 - base_prio) / (MAX_PRIORITY - 1));
		const char *attr = fn->func_decl.attr_init ? "constructor" : "destructor";
		PRINTF("#if defined(__GNUC__) || defined(__clang__)\n__attribute__((%s(%d)))\n#endif\n", attr, c_prio);
	}

	bool is_weak       = fn->is_weak || decl_should_weaken(fn);
	const char **names = NULL;
	FOREACH(Decl *, d, sig->params)
	{
		Decl *rd       = d ? c_decl_unwrap(d) : NULL;
		VariableId vid = c_ptr_to_id(htable_get(&c->local_vars, rd));
		vec_add(names, str_printf("___var_%d", vid));
	}
	// A NULL list means unnamed parameters, but ___ret_out still needs its name without parameters.
	const char *no_params[1] = { NULL };
	const char *signature    = c_signature_str(c, sig, fn_name, (CSigOpts){ .names = names ? names : no_params, .is_main = c_fn_is_main(fn) });
	PRINTF("%s%s%s {\n", c_decl_export_attrs(fn, true), c_fn_is_internal(fn) ? "static " : (is_weak ? "__c3_weak_fn " : ""), signature);

	c_emit_local_var_declarations(c);

	if (body_buf.write_idx > 0)
	{
		c_write_bytes(c, body_buf.bytes.ptr, body_buf.write_idx);
	}
	byte_buffer_free(&body_buf);
	PRINT("}\n\n");
}