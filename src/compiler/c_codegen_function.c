#include "c_codegen_internal.h"

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
	Type *lowered = (type && is_valid_type_ptr(type)) ? c_safe_type_lower(type) : type_int;
	if (lowered->type_kind == TYPE_VOID)
	{
		lowered = type_int;
	}
	CLocalVar lv = {.id = id, .type = lowered, .is_static = is_static, .decl = decl};
	vec_add(c->function_locals, lv);
}

VariableId c_emit_temp_var(GenContext *c, CValue *value, Type *type)
{
	Type *lowered = (type && is_valid_type_ptr(type)) ? c_safe_type_lower(type) : type_int;
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
	decl    = decl_raw(decl);
	void *v = htable_get(&c->local_vars, decl);
	if (v)
	{
		return (VariableId)(uintptr_t)v;
	}

	VariableId var = c_create_variable(c);
	htable_set(&c->local_vars, decl, (void *)(uintptr_t)var);
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
	if (decl->decl_kind != DECL_VAR)
	{
		return 0;
	}
	if (decl->var.kind == VARDECL_UNWRAPPED)
	{
		return 0;
	}
	if (!IS_OPTIONAL(decl))
	{
		return 0;
	}
	void *v = htable_get(&c->local_fault_vars, decl);
	if (v)
	{
		return (VariableId)(uintptr_t)v;
	}
	VariableId fvar = c_create_variable(c);
	htable_set(&c->local_fault_vars, decl, (void *)(uintptr_t)fvar);
	bool is_static = (decl->var.is_static || decl->var.kind == VARDECL_CONST);
	c_register_function_local(c, fvar, type_fault, is_static, decl);
	return fvar;
}

void c_emit_assign_decl_fault(GenContext *c, Decl *decl, CValue *src_val, Expr *src_expr)
{
	VariableId fvid = c_get_decl_fault_var(c, decl);
	if (fvid == 0)
	{
		return;
	}

	if (src_val && src_val->optional != 0)
	{
		PRINTF("___var_%d = (c3fault_t)(uintptr_t)___var_%d;\n", fvid, src_val->optional);
	}
	else if (src_val && src_val->type && (src_val->type->type_kind == TYPE_ANYFAULT || src_val->type == type_fault))
	{
		PRINTF("___var_%d = (c3fault_t)(uintptr_t)___var_%d;\n", fvid, src_val->var);
	}
	else if (src_expr && type_is_optional(src_expr->type))
	{
		PRINTF("___var_%d = __c3_current_fault;\n", fvid);
	}
	else
	{
		PRINTF("___var_%d = NULL;\n", fvid);
	}
	PRINTF("__c3_current_fault = ___var_%d;\n", fvid);
}

VariableId c_ensure_cvalue_var(GenContext *c, CValue *val, Type *fallback_type)
{
	if (val->var != 0)
	{
		return val->var;
	}

	Type *t = (val->type && is_valid_type_ptr(val->type) && val->type->type_kind != TYPE_VOID)
	              ? c_safe_type_lower(val->type)
	              : (fallback_type && is_valid_type_ptr(fallback_type) && fallback_type->type_kind != TYPE_VOID ? c_safe_type_lower(fallback_type) : type_int);
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
	if (!val || val->var == 0)
	{
		return;
	}
	if (val->kind != CV_ADDRESS)
	{
		return;
	}

	Type *t = (val->type && is_valid_type_ptr(val->type)) ? c_safe_type_lower(val->type) : type_void;
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
	if (!val || val->var == 0)
	{
		return;
	}
	if (val->kind == CV_ADDRESS)
	{
		return;
	}

	Type *t = (val->type && is_valid_type_ptr(val->type)) ? c_safe_type_lower(val->type) : type_void;
	if (t->type_kind == TYPE_VOID)
	{
		t = type_int;
	}

	Type *ptr_type = type_get_ptr(t);
	c_emit_type_forward_decl(c, ptr_type);
	const char *ptname = c_type_name(c, ptr_type);
	int temp           = c_create_variable(c);
	c_register_function_local(c, temp, ptr_type, false, NULL);

	PRINTF("%s ___var_%d = (%s)&___var_%d;\n", ptname, temp, ptname, val->var);
	val->var  = temp;
	val->kind = CV_ADDRESS;
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

void c_emit_assign_to_any(GenContext *c, const char *dst_expr, CValue *src)
{
	if (!src || src->var == 0)
	{
		PRINTF("%s = (__c3_any__){ .ptr = NULL, .typeid = NULL };\n", dst_expr);
		return;
	}

	Type *src_type  = (src->type && is_valid_type_ptr(src->type)) ? c_safe_type_lower(src->type) : type_void;
	bool src_is_any = (src_type->type_kind == TYPE_ANY || src_type->type_kind == TYPE_INTERFACE);

	if (src_is_any)
	{
		PRINTF("%s = ___var_%d;\n", dst_expr, src->var);
		return;
	}

	if (src_type->type_kind == TYPE_STRUCT && src_type->decl && src_type->decl->strukt.members && vec_size(src_type->decl->strukt.members) > 0 &&
	    (src_type->decl->strukt.members[0]->type->type_kind == TYPE_ANY || src_type->decl->strukt.members[0]->type->type_kind == TYPE_INTERFACE))
	{
		PRINTF("%s = ___var_%d%sm0;\n", dst_expr, src->var, c_arrow(src));
		return;
	}

	if (type_is_pointer(src_type))
	{
		Type *pointee = (src_type->pointer && is_valid_type_ptr(src_type->pointer)) ? src_type->pointer : type_void;
		if (pointee->type_kind == TYPE_STRUCT && pointee->decl && pointee->decl->strukt.members && vec_size(pointee->decl->strukt.members) > 0 &&
		    (pointee->decl->strukt.members[0]->type->type_kind == TYPE_ANY || pointee->decl->strukt.members[0]->type->type_kind == TYPE_INTERFACE))
		{
			PRINTF("%s = ((%s*)___var_%d)->m0;\n", dst_expr, c_type_name(c, pointee), src->var);
		}
		else
		{
			PRINTF("%s = (__c3_any__){ .ptr = (void*)___var_%d, .typeid = (c3typeid_t)&%s };\n",
			       dst_expr, src->var, c_typeid_name(pointee));
		}
		return;
	}

	if (src->kind == CV_ADDRESS)
	{
		PRINTF("%s = (__c3_any__){ .ptr = (void*)___var_%d, .typeid = (c3typeid_t)&%s };\n",
		       dst_expr, src->var, c_typeid_name(src_type));
	}
	else
	{
		PRINTF("%s = (__c3_any__){ .ptr = (void*)&___var_%d, .typeid = (c3typeid_t)&%s };\n",
		       dst_expr, src->var, c_typeid_name(src_type));
	}
}

void c_emit_assign_var(GenContext *c, VariableId dst_var, Type *dst_type, CValue *src_val)
{
	dst_type = (dst_type && is_valid_type_ptr(dst_type)) ? c_safe_type_lower(dst_type) : type_void;
	if (dst_type->type_kind == TYPE_VOID)
	{
		return;
	}
	Type *src_type        = (src_val && src_val->type && is_valid_type_ptr(src_val->type)) ? c_safe_type_lower(src_val->type) : dst_type;
	const char *dst_tname = c_type_name(c, dst_type);

	if (!src_val || src_val->var == 0)
	{
		if (c_type_is_aggregate(dst_type))
		{
			PRINTF("__c3_memset(&___var_%d, 0, sizeof(%s));\n", dst_var, dst_tname);
		}
		else if (type_is_pointer(dst_type) || dst_type->type_kind == TYPE_FUNC_PTR || dst_type->type_kind == TYPE_ANYFAULT || dst_type->type_kind == TYPE_TYPEID)
		{
			PRINTF("___var_%d = NULL;\n", dst_var);
		}
		else
		{
			PRINTF("___var_%d = 0;\n", dst_var);
		}
		return;
	}

	if (c_type_is_aggregate(dst_type) || c_type_is_aggregate(src_type))
	{
		if (dst_type->type_kind == TYPE_POINTER && (src_type->type_kind == TYPE_ANY || src_type->type_kind == TYPE_INTERFACE))
		{
			PRINTF("___var_%d = (%s)___var_%d.ptr;\n", dst_var, dst_tname, src_val->var);
		}
		else if (dst_type->type_kind == TYPE_POINTER && src_type->type_kind == TYPE_SLICE)
		{
			PRINTF("___var_%d = (%s)___var_%d.ptr;\n", dst_var, dst_tname, src_val->var);
		}
		else if (dst_type->type_kind == TYPE_POINTER && (src_type->type_kind == TYPE_ARRAY || src_type->type_kind == TYPE_VECTOR || src_type->type_kind == TYPE_SIMD_VECTOR))
		{
			PRINTF("___var_%d = (%s)(___var_%d%sptr);\n", dst_var, dst_tname, src_val->var, c_arrow(src_val));
		}
		else if (dst_type->type_kind == TYPE_SLICE && (src_type->type_kind == TYPE_ARRAY || src_type->type_kind == TYPE_VECTOR || src_type->type_kind == TYPE_SIMD_VECTOR))
		{
			PRINTF("___var_%d = (%s){ .ptr = (void*)(___var_%d%sptr), .len = %llu };\n",
			       dst_var, dst_tname, src_val->var, c_arrow(src_val), (unsigned long long)src_type->array.len);
		}
		else if (dst_type->type_kind == TYPE_SLICE && src_type->type_kind == TYPE_POINTER && src_type->pointer && (src_type->pointer->type_kind == TYPE_ARRAY || src_type->pointer->type_kind == TYPE_VECTOR || src_type->pointer->type_kind == TYPE_SIMD_VECTOR))
		{
			PRINTF("___var_%d = (%s){ .ptr = (void*)(___var_%d), .len = %llu };\n",
			       dst_var, dst_tname, src_val->var, (unsigned long long)src_type->pointer->array.len);
		}
		else if (dst_type->type_kind == TYPE_ANY || dst_type->type_kind == TYPE_INTERFACE)
		{
			char target_slot[64];
			snprintf(target_slot, sizeof(target_slot), "___var_%d", dst_var);
			c_emit_assign_to_any(c, target_slot, src_val);
		}
		else if (dst_type->type_kind == src_type->type_kind && strcmp(dst_tname, c_type_name(c, src_type)) == 0)
		{
			if (src_val->kind == CV_ADDRESS)
			{
				PRINTF("__c3_memcpy(&___var_%d, (void*)___var_%d, sizeof(%s));\n", dst_var, src_val->var, dst_tname);
			}
			else
			{
				PRINTF("___var_%d = ___var_%d;\n", dst_var, src_val->var);
			}
		}
		else
		{
			if (src_val->kind == CV_ADDRESS)
			{
				PRINTF("__c3_memcpy(&___var_%d, (void*)___var_%d, sizeof(%s));\n", dst_var, src_val->var, dst_tname);
			}
			else
			{
				PRINTF("__c3_memcpy(&___var_%d, &___var_%d, sizeof(%s));\n", dst_var, src_val->var, dst_tname);
			}
		}
	}
	else
	{
		if (src_val->kind == CV_ADDRESS)
		{
			c_value_rvalue(c, src_val);
		}

		if (type_is_integer(dst_type) && (type_is_pointer(src_type) || src_type->type_kind == TYPE_ANYFAULT || src_type->type_kind == TYPE_TYPEID))
		{
			PRINTF("___var_%d = (%s)(uintptr_t)___var_%d;\n", dst_var, dst_tname, src_val->var);
		}
		else if ((type_is_pointer(dst_type) || dst_type->type_kind == TYPE_ANYFAULT || dst_type->type_kind == TYPE_TYPEID) && type_is_integer(src_type))
		{
			PRINTF("___var_%d = (%s)(uintptr_t)___var_%d;\n", dst_var, dst_tname, src_val->var);
		}
		else if (type_is_float(dst_type) && (type_is_pointer(src_type) || src_type->type_kind == TYPE_ANYFAULT || src_type->type_kind == TYPE_TYPEID))
		{
			PRINTF("___var_%d = 0.0;\n", dst_var);
		}
		else if ((type_is_pointer(dst_type) || dst_type->type_kind == TYPE_ANYFAULT || dst_type->type_kind == TYPE_TYPEID) && type_is_float(src_type))
		{
			PRINTF("___var_%d = NULL;\n", dst_var);
		}
		else if ((type_is_pointer(dst_type) || dst_type->type_kind == TYPE_FUNC_PTR) && (type_is_pointer(src_type) || src_type->type_kind == TYPE_FUNC_PTR))
		{
			PRINTF("___var_%d = (%s)___var_%d;\n", dst_var, dst_tname, src_val->var);
		}
		else if (dst_type != src_type)
		{
			PRINTF("___var_%d = (%s)___var_%d;\n", dst_var, dst_tname, src_val->var);
		}
		else
		{
			PRINTF("___var_%d = ___var_%d;\n", dst_var, src_val->var);
		}
	}
}

bool c_emit_function_decl(GenContext *c, Decl *fn, bool is_current_module)
{
	(void)is_current_module;
	if (!fn || fn->replacement || fn->decl_kind != DECL_FUNC || fn->is_template || fn->func_decl.attr_interface_method)
	{
		return false;
	}
	const char *fn_name = c_get_decl_name(fn);

	if (htable_get(&c->decl_names, (void *)fn_name))
	{
		return false;
	}
	htable_set(&c->decl_names, (void *)fn_name, (void *)1);

	Signature *sig = &fn->func_decl.signature;
	Type *rtype    = typeget(sig->rtype);
	if (is_valid_type_ptr(rtype))
	{
		c_emit_type_forward_decl(c, rtype);
	}
	if (!rtype || (c_type_is_resolved(rtype) && type_size(rtype) == 0))
	{
		rtype = type_void;
	}

	FOREACH(Decl *, d, sig->params)
	{
		Type *ptype = c_decl_type(d);
		if (is_valid_type_ptr(ptype))
		{
			c_emit_type_forward_decl(c, ptype);
		}
	}

	PRINTF("extern %s %s(", c_type_name(c, rtype), fn_name);
	int emitted_count = 0;
	FOREACH(Decl *, d, sig->params)
	{
		Type *ptype = c_decl_type(d);
		if (ptype && is_valid_type_ptr(ptype) && c_type_is_resolved(ptype) && type_size(ptype) == 0)
		{
			continue;
		}
		if (emitted_count != 0)
		{
			PRINT(", ");
		}
		// Standard C requires main's argv to be char**, not uint8_t**
		if (strcmp(fn_name, "main") == 0 && vec_size(sig->params) <= 3 && (emitted_count == 1 || emitted_count == 2))
		{
			PRINT("char**");
		}
		else
		{
			PRINTF("%s", c_type_name(c, ptype));
		}
		emitted_count++;
	}
	if (sig->variadic == VARIADIC_RAW)
	{
		if (emitted_count > 0)
		{
			PRINT(", ");
		}
		PRINT("...");
		emitted_count++;
	}
	if (emitted_count == 0)
	{
		PRINT("void");
	}
	PRINT(")");
	const char *asm_name = c_get_decl_asm_name(fn);
	if (asm_name && (strncmp(asm_name, "__atomic_", 9) == 0 || strncmp(asm_name, "__builtin_", 10) == 0))
	{
		PRINTF(" __asm__(\"%s\")", asm_name);
	}
	PRINT(";\n");
	return !fn->is_extern;
}

void c_emit_local_var_declarations(GenContext *c)
{
	FOREACH(CLocalVar, lv, c->function_locals)
	{
		if (htable_get(&c->declared_vars, (void *)(uintptr_t)lv.id))
		{
			continue;
		}
		htable_set(&c->declared_vars, (void *)(uintptr_t)lv.id, (void *)1);
		Type *vt = lv.type ? c_safe_type_lower(lv.type) : type_int;
		if (vt->type_kind == TYPE_VOID)
		{
			vt = type_int;
		}
		c_emit_type_forward_decl(c, vt);
		const char *tname = c_type_name(c, vt);

		AlignSize align = lv.decl ? lv.decl->alignment : 0;
		if (!align && lv.decl && is_valid_type_ptr(lv.decl->type) && c_type_is_resolved(lv.decl->type))
		{
			align = type_alloca_alignment(lv.decl->type);
		}
		if (!align && lv.type && is_valid_type_ptr(lv.type) && c_type_is_resolved(lv.type))
		{
			align = type_alloca_alignment(lv.type);
		}
		if (align < 16 && c_get_type_size(vt) >= 16)
		{
			align = 16;
		}
		AlignSize abi_align   = (c_type_is_resolved(vt)) ? type_abi_alignment(vt) : 1;
		const char *align_str = (align > abi_align) ? str_printf("__c3_aligned(%u) ", (unsigned)align) : "";

		if (c_type_is_aggregate(vt))
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

	Signature *sig = &fn->func_decl.signature;
	Type *rtype    = typeget(sig->rtype);
	if (is_valid_type_ptr(rtype))
	{
		c_emit_type_forward_decl(c, rtype);
	}
	if (!rtype || (c_type_is_resolved(rtype) && type_size(rtype) == 0))
	{
		rtype = type_void;
	}

	const char *fn_name       = c_get_decl_name(fn);
	const char *ret_type_name = c_type_name(c, rtype);

	c->current_return_type      = rtype;
	c->current_macro_ret_var    = 0;
	c->current_macro_fault_var  = 0;
	c->current_macro_ret_type   = NULL;
	c->current_macro_exit_label = 0;
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
		if (is_valid_type_ptr(ptype))
		{
			c_emit_type_forward_decl(c, ptype);
		}
		VariableId vid = c_create_variable(c);
		if (d)
		{
			d = decl_raw(d);
			htable_set(&c->local_vars, d, (void *)(uintptr_t)vid);
			if (IS_OPTIONAL(d))
			{
				c_get_decl_fault_var(c, d);
			}
		}
		if (ptype && is_valid_type_ptr(ptype) && c_type_is_resolved(ptype) && type_size(ptype) == 0)
		{
			c_register_function_local(c, vid, ptype, false, d);
		}
		else
		{
			htable_set(&c->declared_vars, (void *)(uintptr_t)vid, (void *)1);
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
		bool is_fn_opt = rtype && type_is_optional(rtype);
		if (is_fn_opt)
		{
			PRINT("__c3_current_fault = NULL;\n");
		}
		if (c_safe_type_lower(rtype) == type_void)
		{
			PRINT("return;\n");
		}
		else if (rtype && rtype->type_kind == TYPE_OPTIONAL && (rtype->optional == NULL || rtype->optional->type_kind == TYPE_VOID))
		{
			PRINT("return NULL;\n");
		}
		else
		{
			PRINTF("return (%s)%s;\n", ret_type_name, c_type_zero_literal(c_safe_type_lower(rtype)));
		}
	}

	c->buffer = prev_buf;

	FOREACH(CLocalVar, lv, c->function_locals)
	{
		if (htable_get(&c->declared_vars, (void *)(uintptr_t)lv.id))
		{
			continue;
		}
		if (!lv.is_static)
		{
			continue;
		}
		htable_set(&c->declared_vars, (void *)(uintptr_t)lv.id, (void *)1);
		Type *vt = lv.type ? c_safe_type_lower(lv.type) : type_int;
		if (vt->type_kind == TYPE_VOID)
		{
			vt = type_int;
		}
		c_emit_type_forward_decl(c, vt);
		const char *tname = c_type_name(c, vt);
		bool is_tls       = lv.decl && lv.decl->var.is_threadlocal;
		AlignSize align   = lv.decl ? lv.decl->alignment : 0;
		if (!align && lv.decl && is_valid_type_ptr(lv.decl->type) && c_type_is_resolved(lv.decl->type))
		{
			align = type_alloca_alignment(lv.decl->type);
		}
		if (!align && lv.type && is_valid_type_ptr(lv.type) && c_type_is_resolved(lv.type))
		{
			align = type_alloca_alignment(lv.type);
		}
		if (align < 16 && c_get_type_size(vt) >= 16)
		{
			align = 16;
		}
		AlignSize abi_align   = (c_type_is_resolved(vt)) ? type_abi_alignment(vt) : 1;
		const char *align_str = (align > abi_align) ? str_printf("__c3_aligned(%u) ", (unsigned)align) : "";
		PRINTF("static %s%s%s ___var_%d", align_str, is_tls ? "__c3_thread_local " : "", tname, lv.id);
		if (lv.decl && lv.decl->var.init_expr && expr_is_const(lv.decl->var.init_expr))
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

	if (fn->func_decl.attr_init)
	{
		uint32_t prio = fn->func_decl.priority ? fn->func_decl.priority : MAX_PRIORITY;
		int c_prio    = 102 + (int)((prio - 1) * (65535 - 102) / (MAX_PRIORITY - 1));
		PRINTF("#if defined(__GNUC__) || defined(__clang__)\n__attribute__((constructor(%d)))\n#endif\n", c_prio);
	}
	else if (fn->func_decl.attr_finalizer)
	{
		uint32_t prio = fn->func_decl.priority ? fn->func_decl.priority : MAX_PRIORITY;
		int c_prio    = 101 + (int)((prio - 1) * (65535 - 101) / (MAX_PRIORITY - 1));
		PRINTF("#if defined(__GNUC__) || defined(__clang__)\n__attribute__((destructor(%d)))\n#endif\n", c_prio);
	}

	PRINTF("%s %s(", ret_type_name, fn_name);
	int emitted_params = 0;
	FOREACH(Decl *, d, sig->params)
	{
		Type *ptype = c_decl_type(d);
		if (ptype && is_valid_type_ptr(ptype) && c_type_is_resolved(ptype) && type_size(ptype) == 0)
		{
			continue;
		}
		if (emitted_params != 0)
		{
			PRINT(", ");
		}
		Decl *rd       = d ? decl_raw(d) : NULL;
		VariableId vid = (VariableId)(uintptr_t)htable_get(&c->local_vars, rd);
		if (strcmp(fn_name, "main") == 0 && vec_size(sig->params) <= 3 && (emitted_params == 1 || emitted_params == 2))
		{
			PRINTF("char** ___var_%d", vid);
		}
		else
		{
			PRINTF("%s ___var_%d", c_type_name(c, ptype), vid);
		}
		emitted_params++;
	}
	if (sig->variadic == VARIADIC_RAW)
	{
		if (emitted_params > 0)
		{
			PRINT(", ");
		}
		PRINT("...");
		emitted_params++;
	}
	if (emitted_params == 0)
	{
		PRINT("void");
	}
	PRINT(") {\n");

	c_emit_local_var_declarations(c);

	if (body_buf.write_idx > 0)
	{
		c_write_bytes(c, body_buf.bytes.ptr, body_buf.write_idx);
	}
	byte_buffer_free(&body_buf);
	PRINT("}\n\n");
}