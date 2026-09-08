#include "c_codegen_internal.h"

void c_emit_local_decl(GenContext *c, Decl *decl, CValue *value)
{
	if (!decl || !c->current_block_live)
	{
		return;
	}

	if (c_is_file_global(decl))
	{
		if (value)
		{
			value->type = c_decl_type(decl);
			c_emit_type_forward_decl(c, value->type);
			value->var        = c_create_variable(c);
			value->kind       = CV_VALUE;
			const char *gname = c_get_decl_name(decl);
			if (value->type->type_kind == TYPE_ARRAY)
			{
				PRINTF("%s ___var_%d;\n", c_type_name(c, value->type), value->var);
				PRINTF("__c3_memcpy(&___var_%d, &%s, sizeof(%s));\n", value->var, gname, gname);
			}
			else
			{
				PRINTF("%s ___var_%d = %s;\n", c_type_name(c, value->type), value->var, gname);
			}
		}
		return;
	}

	Type *var_type = c_decl_type(decl);
	if (var_type->type_kind == TYPE_VOID && decl->var.init_expr)
	{
		Type *it = c_expr_type(decl->var.init_expr);
		if (it && it->type_kind != TYPE_VOID)
		{
			var_type   = it;
			decl->type = it;
		}
	}
	if (var_type->type_kind == TYPE_VOID)
	{
		var_type = type_int;
	}
	c_emit_type_forward_decl(c, var_type);
	VariableId vid  = c_get_or_create_decl_var(c, decl);
	VariableId fvid = c_get_decl_fault_var(c, decl);
	if (value)
	{
		*value = (CValue){.var = vid, .kind = CV_VALUE, .type = var_type, .optional = fvid};
	}

	if (decl->var.init_expr && !decl->var.is_static)
	{
		CValue init_val = {0};
		c_emit_expr(c, &init_val, decl->var.init_expr);
		if (init_val.var != 0 && init_val.type && init_val.type->type_kind != TYPE_VOID)
		{
			c_emit_assign_var(c, vid, var_type, &init_val);
		}
		if (fvid != 0)
		{
			c_emit_assign_decl_fault(c, decl, &init_val, decl->var.init_expr);
		}
	}
	else if (!decl->var.is_static && !decl->var.no_init)
	{
		c_emit_assign_var(c, vid, var_type, NULL);
		if (fvid != 0)
		{
			c_emit_assign_decl_fault(c, decl, NULL, NULL);
		}
	}
}

void c_emit_check_fault_and_return(GenContext *c, Expr *expr)
{
	if (!c->current_block_live)
	{
		return;
	}

	if (expr && expr->rethrow_expr.in_block && *expr->rethrow_expr.in_block)
	{
		BlockExit *exit = *expr->rethrow_expr.in_block;
		int blbl        = (int)(uintptr_t)exit->block_return_exit;
		int bfault      = (int)(uintptr_t)exit->block_error_var;
		if (bfault != 0)
		{
			PRINTF("\t___var_%d = __c3_current_fault;\n", bfault);
		}
		PRINTF("\tgoto __C3_LABEL_%d;\n", blbl);
		return;
	}

	Type *cur_ret_t = (c->current_return_type && is_valid_type_ptr(c->current_return_type)) ? c_safe_type_lower(c->current_return_type) : type_void;
	if (cur_ret_t->type_kind == TYPE_VOID)
	{
		PRINT("\treturn;\n");
	}
	else if (cur_ret_t->type_kind == TYPE_ANYFAULT || cur_ret_t == type_fault)
	{
		PRINT("\treturn (c3fault_t)__c3_current_fault;\n");
	}
	else
	{
		PRINTF("\treturn (%s)%s;\n",
		       c_type_name(c, cur_ret_t), c_type_zero_literal(cur_ret_t));
	}
}

static const char *c_cond_expr_str(const CValue *cval, bool invert)
{
	if (!cval || cval->var == 0)
	{
		return invert ? "0" : "1";
	}
	if (cval->type && cval->type->type_kind == TYPE_SLICE)
	{
		return str_printf("___var_%d.len %s 0", cval->var, invert ? "==" : "!=");
	}
	if (cval->type && (cval->type->type_kind == TYPE_ANY || cval->type->type_kind == TYPE_INTERFACE))
	{
		return str_printf("___var_%d.ptr %s NULL", cval->var, invert ? "==" : "!=");
	}
	if (cval->type && (type_is_pointer(cval->type) || cval->type->type_kind == TYPE_FUNC_PTR))
	{
		return str_printf("___var_%d %s NULL", cval->var, invert ? "==" : "!=");
	}
	return str_printf("%s___var_%d", invert ? "!" : "", cval->var);
}

static void c_emit_if_stmt(GenContext *c, Ast *stmt)
{
	AstIfStmt *if_stmt = &stmt->if_stmt;
	ExprId cond_id     = if_stmt->cond;
	AstId then_body_id = if_stmt->then_body;
	AstId else_body_id = if_stmt->else_body;
	Decl *label        = declptrzero(if_stmt->flow.label);
	bool has_break     = if_stmt->flow.has_break;

	int break_label = 0;
	if (label || has_break)
	{
		break_label = c_create_label(c);
		if (label)
		{
			label->label.break_target = (void *)(uintptr_t)break_label;
		}
	}
	if_stmt->codegen.break_block = (void *)(uintptr_t)break_label;

	CValue condition = {0};
	if (cond_id)
	{
		c_emit_expr(c, &condition, exprptr(cond_id));
		c_value_rvalue(c, &condition);
	}

	PRINTF("if (%s) {\n", c_cond_expr_str(&condition, false));

	bool entry_live = c->current_block_live;

	c->current_block_live = true;
	if (then_body_id)
	{
		c_emit_stmt(c, astptr(then_body_id));
	}
	bool then_live = c->current_block_live;

	bool else_live = false;
	if (else_body_id)
	{
		PRINT("} else {\n");
		c->current_block_live = true;
		c_emit_stmt(c, astptr(else_body_id));
		else_live = c->current_block_live;
	}
	else
	{
		else_live = entry_live;
	}
	PRINT("}\n");

	c->current_block_live = then_live || else_live || (break_label != 0 && has_break);

	if (break_label)
	{
		c_emit_label(c, break_label);
	}
}

static void c_emit_for_stmt(GenContext *c, Ast *stmt)
{
	AstForStmt *for_stmt = &stmt->for_stmt;
	ExprId init_id       = for_stmt->init;
	ExprId cond_id       = for_stmt->cond;
	ExprId incr_id       = for_stmt->incr;
	AstId body_id        = for_stmt->body;
	bool skip_first      = stmt->flow.skip_first;

	int loop_start            = c_create_label(c);
	int loop_body             = skip_first ? c_create_label(c) : 0;
	int loop_cont             = c_create_label(c);
	int loop_exit             = c_create_label(c);
	int old_break             = c->current_break_label;
	int old_cont              = c->current_continue_label;
	c->current_break_label    = loop_exit;
	c->current_continue_label = loop_cont;

	stmt->for_stmt.codegen.continue_block = (void *)(uintptr_t)loop_cont;
	stmt->for_stmt.codegen.exit_block     = (void *)(uintptr_t)loop_exit;
	Decl *label                           = declptrzero(stmt->for_stmt.flow.label);
	if (label)
	{
		label->label.break_target    = (void *)(uintptr_t)loop_exit;
		label->label.continue_target = (void *)(uintptr_t)loop_cont;
	}

	if (init_id)
	{
		c_emit_ignored_expr(c, exprptr(init_id));
	}
	if (skip_first)
	{
		PRINTF("goto __C3_LABEL_%d;\n", loop_body);
		c->current_block_live = false;
	}

	c_emit_label(c, loop_start);
	if (cond_id)
	{
		CValue cond_val = {0};
		c_emit_expr(c, &cond_val, exprptr(cond_id));
		c_value_rvalue(c, &cond_val);
		if (cond_val.var != 0)
		{
			PRINTF("if (%s) goto __C3_LABEL_%d;\n", c_cond_expr_str(&cond_val, true), loop_exit);
		}
	}
	if (skip_first)
	{
		c_emit_label(c, loop_body);
	}
	if (body_id)
	{
		c_emit_stmt(c, astptr(body_id));
	}

	c_emit_label(c, loop_cont);
	if (incr_id)
	{
		c_emit_ignored_expr(c, exprptr(incr_id));
	}
	PRINTF("goto __C3_LABEL_%d;\n", loop_start);
	c->current_block_live = false;

	c_emit_label(c, loop_exit);

	c->current_break_label    = old_break;
	c->current_continue_label = old_cont;
}

static bool is_constant_int_switch(AstSwitchStmt *sw, ExprId cond_id)
{
	if (!cond_id)
	{
		return false;
	}
	Type *cond_type = (exprptr(cond_id)->type && is_valid_type_ptr(exprptr(cond_id)->type)) ? c_safe_type_lower(exprptr(cond_id)->type) : type_void;
	if (cond_type->type_kind == TYPE_BOOL)
	{
		return false;
	}
	if (!type_is_integer(cond_type) && cond_type->type_kind != TYPE_ENUM)
	{
		return false;
	}
	FOREACH(Ast *, case_stmt, sw->cases)
	{
		if (case_stmt->case_stmt.to_expr)
		{
			return false;
		}
		if (!case_stmt->case_stmt.expr)
		{
			continue;
		}
		Expr *c_expr = exprptr(case_stmt->case_stmt.expr);
		if (c_expr->expr_kind != EXPR_CONST)
		{
			return false;
		}
		if (c_expr->const_expr.const_kind != CONST_INTEGER &&
		    c_expr->const_expr.const_kind != CONST_ENUM &&
		    c_expr->const_expr.const_kind != CONST_BOOL)
		{
			return false;
		}
	}
	return true;
}

static void c_emit_nextcase_stmt(GenContext *c, Ast *stmt)
{
	if (!c->current_block_live)
	{
		return;
	}

	Ast *parent = astptr(stmt->nextcase_stmt.switch_stmt);
	if (!stmt->nextcase_stmt.is_expr)
	{
		if (stmt->nextcase_stmt.defer_id)
		{
			c_emit_stmt_chain(c, stmt->nextcase_stmt.defer_id);
		}
		Ast *target_case = parent->switch_stmt.cases[stmt->nextcase_stmt.case_number];
		int target_label = (int)(uintptr_t)target_case->case_stmt.backend_block;
		PRINTF("goto __C3_LABEL_%d;\n", target_label);
		c->current_block_live = false;
		return;
	}

	CValue next_val = {0};
	c_emit_expr(c, &next_val, stmt->nextcase_stmt.nextcase_value);
	c_value_rvalue(c, &next_val);
	c_ensure_cvalue_var(c, &next_val, stmt->nextcase_stmt.nextcase_value->type);

	int retry_var   = (int)(uintptr_t)parent->switch_stmt.codegen.retry.var;
	int retry_block = (int)(uintptr_t)parent->switch_stmt.codegen.retry.block;

	PRINTF("___var_%d = ___var_%d;\n", retry_var, next_val.var);
	if (stmt->nextcase_stmt.defer_id)
	{
		c_emit_stmt_chain(c, stmt->nextcase_stmt.defer_id);
	}
	PRINTF("goto __C3_LABEL_%d;\n", retry_block);
	c->current_block_live = false;
}

static int c_emit_switch_case_labels(GenContext *c, Ast **cases, int case_count, Ast *default_case, int switch_exit)
{
	int default_label = switch_exit;
	if (default_case && default_case->case_stmt.body)
	{
		default_label = c_create_label(c);
	}

	for (int i = 0; i < case_count; i++)
	{
		Ast *cs = cases[i];
		if (cs == default_case)
		{
			cs->case_stmt.backend_block = cs->case_stmt.body ? (void *)(uintptr_t)default_label : NULL;
		}
		else
		{
			cs->case_stmt.backend_block = cs->case_stmt.body ? (void *)(uintptr_t)c_create_label(c) : NULL;
		}
	}

	void *next_block = (void *)(uintptr_t)switch_exit;
	for (int i = case_count; i > 0; i--)
	{
		Ast *cs = cases[i - 1];
		if (cs->case_stmt.backend_block)
		{
			next_block = cs->case_stmt.backend_block;
			continue;
		}
		cs->case_stmt.backend_block = next_block;
		if (cs == default_case)
		{
			default_label = (int)(uintptr_t)next_block;
		}
	}
	return default_label;
}

static void c_emit_switch_stmt(GenContext *c, Ast *stmt)
{
	AstSwitchStmt *sw = &stmt->switch_stmt;
	ExprId cond_id    = sw->cond;
	Ast **cases       = sw->cases;
	bool is_const_int = is_constant_int_switch(sw, cond_id);

	int old_break                        = c->current_break_label;
	int switch_exit                      = c_create_label(c);
	c->current_break_label               = switch_exit;
	stmt->switch_stmt.codegen.exit_block = (void *)(uintptr_t)switch_exit;

	Ast *default_case = NULL;
	FOREACH(Ast *, cs, cases)
	{
		if (!cs->case_stmt.expr)
		{
			default_case = cs;
			break;
		}
	}
	int case_count = vec_size(cases);

	if (is_const_int)
	{
		Type *cond_type = exprptr(cond_id)->type;
		int cond_var    = c_create_variable(c);
		c_emit_var_zero_init(c, cond_var, cond_type);
		CValue cond_val = {0};
		c_emit_expr(c, &cond_val, exprptr(cond_id));
		c_value_rvalue(c, &cond_val);
		c_ensure_cvalue_var(c, &cond_val, cond_type);
		PRINTF("___var_%d = ___var_%d;\n", cond_var, cond_val.var);

		int switch_entry                      = c_create_label(c);
		stmt->switch_stmt.codegen.retry.block = (void *)(uintptr_t)switch_entry;
		stmt->switch_stmt.codegen.retry.var   = (void *)(uintptr_t)cond_var;

		c_emit_switch_case_labels(c, cases, case_count, NULL, switch_exit);

		c_emit_label(c, switch_entry);
		PRINTF("switch (___var_%d) {\n", cond_var);
		FOREACH_IDX(case_i, Ast *, case_stmt, cases)
		{
			if (!case_stmt->case_stmt.expr)
			{
				PRINT("default:\n");
			}
			else
			{
				Expr *c_expr = exprptr(case_stmt->case_stmt.expr);
				int64_t val  = 0;
				if (c_expr->expr_kind == EXPR_CONST)
				{
					if (c_expr->const_expr.const_kind == CONST_INTEGER)
					{
						val = int_to_i64(c_expr->const_expr.ixx);
					}
					else if (c_expr->const_expr.const_kind == CONST_ENUM)
					{
						val = (int64_t)c_expr->const_expr.enum_val->enum_constant.inner_ordinal;
					}
					else if (c_expr->const_expr.const_kind == CONST_BOOL)
					{
						val = c_expr->const_expr.b ? 1 : 0;
					}
				}
				PRINTF("case %" PRId64 ":\n", val);
			}
			if (case_stmt->case_stmt.body)
			{
				c_emit_label(c, (int)(uintptr_t)case_stmt->case_stmt.backend_block);
				PRINT("{\n");
				c->current_block_live = true;
				c_emit_stmt(c, case_stmt->case_stmt.body);
				if (c->current_block_live)
				{
					PRINT("break;\n");
				}
				PRINT("}\n");
			}
			else if (case_i == case_count - 1)
			{
				PRINT("break;\n");
			}
		}
		PRINT("}\n");
	}
	else
	{
		int cond_var        = 0;
		bool has_cond       = (cond_id != 0);
		Type *cond_type     = NULL;
		bool is_type_switch = false;
		if (has_cond)
		{
			cond_type      = (exprptr(cond_id)->type && is_valid_type_ptr(exprptr(cond_id)->type)) ? c_safe_type_lower(exprptr(cond_id)->type) : type_void;
			is_type_switch = (exprptr(cond_id)->type && exprptr(cond_id)->type->canonical == type_typeid);
			cond_var       = c_create_variable(c);
			c_emit_var_zero_init(c, cond_var, cond_type);
			CValue cond_val = {0};
			c_emit_expr(c, &cond_val, exprptr(cond_id));
			c_value_rvalue(c, &cond_val);
			c_ensure_cvalue_var(c, &cond_val, cond_type);
			PRINTF("___var_%d = ___var_%d;\n", cond_var, cond_val.var);
		}

		int switch_entry                      = c_create_label(c);
		stmt->switch_stmt.codegen.retry.block = (void *)(uintptr_t)switch_entry;
		stmt->switch_stmt.codegen.retry.var   = (void *)(uintptr_t)cond_var;

		int default_label = c_emit_switch_case_labels(c, cases, case_count, default_case, switch_exit);

		c_emit_label(c, switch_entry);

		int else_depth = 0;
		FOREACH_IDX(i, Ast *, case_stmt, cases)
		{
			if (!case_stmt->case_stmt.expr)
			{
				continue;
			}
			CValue case_expr_val = {0};
			c_emit_expr(c, &case_expr_val, exprptr(case_stmt->case_stmt.expr));
			c_value_rvalue(c, &case_expr_val);

			CValue to_expr_val = {0};
			bool has_to        = (case_stmt->case_stmt.to_expr != 0);
			if (has_to)
			{
				c_emit_expr(c, &to_expr_val, exprptr(case_stmt->case_stmt.to_expr));
				c_value_rvalue(c, &to_expr_val);
			}

			if (has_cond)
			{
				Type *ct    = cond_type;
				Type *caset = (case_expr_val.type && is_valid_type_ptr(case_expr_val.type)) ? c_safe_type_lower(case_expr_val.type) : NULL;
				if (is_type_switch && cond_var != 0 && case_expr_val.var != 0)
				{
					PRINTF("if (__c3_type_matches((c3typeid_t)___var_%d, (c3typeid_t)___var_%d)) {\n", cond_var, case_expr_val.var);
				}
				else if (has_to && cond_var != 0 && case_expr_val.var != 0 && to_expr_val.var != 0)
				{
					const char *c_cast  = (ct && type_is_pointer(ct)) ? "(uintptr_t)" : "";
					const char *cs_cast = (caset && type_is_pointer(caset)) ? "(uintptr_t)" : "";
					PRINTF("if (%s___var_%d >= %s___var_%d && %s___var_%d <= %s___var_%d) {\n",
					       c_cast, cond_var, cs_cast, case_expr_val.var,
					       c_cast, cond_var, cs_cast, to_expr_val.var);
				}
				else if (cond_var != 0 && case_expr_val.var != 0)
				{
					if (ct && caset && ct->type_kind == TYPE_SLICE && caset->type_kind == TYPE_SLICE)
					{
						PRINTF("if (__C3_SLICE_EQ(___var_%d, ___var_%d)) {\n", cond_var, case_expr_val.var);
					}
					else if ((ct && c_type_is_aggregate(ct)) || (caset && c_type_is_aggregate(caset)))
					{
						const char *tname = c_type_name(c, ct ? ct : caset);
						PRINTF("if (__c3_memcmp(&___var_%d, &___var_%d, sizeof(%s)) == 0) {\n", cond_var, case_expr_val.var, tname);
					}
					else if (ct && type_is_pointer(ct) && caset && type_is_integer(caset))
					{
						PRINTF("if ((uintptr_t)___var_%d == (uintptr_t)___var_%d) {\n", cond_var, case_expr_val.var);
					}
					else if (ct && type_is_integer(ct) && caset && type_is_pointer(caset))
					{
						PRINTF("if ((uintptr_t)___var_%d == (uintptr_t)___var_%d) {\n", cond_var, case_expr_val.var);
					}
					else
					{
						PRINTF("if (___var_%d == ___var_%d) {\n", cond_var, case_expr_val.var);
					}
				}
				else if (cond_var != 0)
				{
					if (ct && ct->type_kind == TYPE_SLICE)
					{
						PRINTF("if (___var_%d.len == 0) {\n", cond_var);
					}
					else if (ct && c_type_is_aggregate(ct))
					{
						PRINT("if (0) {\n");
					}
					else if (ct && type_is_pointer(ct))
					{
						PRINTF("if (___var_%d == NULL) {\n", cond_var);
					}
					else
					{
						PRINTF("if (___var_%d == 0) {\n", cond_var);
					}
				}
				else if (case_expr_val.var != 0)
				{
					if (caset && caset->type_kind == TYPE_SLICE)
					{
						PRINTF("if (___var_%d.len == 0) {\n", case_expr_val.var);
					}
					else if (caset && c_type_is_aggregate(caset))
					{
						PRINT("if (0) {\n");
					}
					else if (caset && type_is_pointer(caset))
					{
						PRINTF("if (___var_%d == NULL) {\n", case_expr_val.var);
					}
					else
					{
						PRINTF("if (___var_%d == 0) {\n", case_expr_val.var);
					}
				}
				else
				{
					PRINT("if (1) {\n");
				}
			}
			else
			{
				if (case_expr_val.var != 0)
				{
					PRINTF("if (___var_%d) {\n", case_expr_val.var);
				}
				else
				{
					PRINT("if (1) {\n");
				}
			}

			int target_label = (int)(uintptr_t)case_stmt->case_stmt.backend_block;
			PRINTF("goto __C3_LABEL_%d;\n} else {\n", target_label);
			else_depth++;
		}
		PRINTF("goto __C3_LABEL_%d;\n", default_label);
		for (int j = 0; j < else_depth; j++)
		{
			PRINT("}\n");
		}

		FOREACH_IDX(case_idx, Ast *, case_stmt, cases)
		{
			if (!case_stmt->case_stmt.body)
			{
				continue;
			}
			c_emit_label(c, (int)(uintptr_t)case_stmt->case_stmt.backend_block);
			c_emit_stmt(c, case_stmt->case_stmt.body);
			if (c->current_block_live)
			{
				PRINTF("goto __C3_LABEL_%d;\n", switch_exit);
				c->current_block_live = false;
			}
		}
	}

	c_emit_label(c, switch_exit);
	c->current_break_label = old_break;
}

static void c_emit_defer_and_fault_cleanup(GenContext *c, int fault_var, AstId cleanup, AstId cleanup_fail)
{
	if (fault_var != 0 && cleanup_fail)
	{
		PRINTF("if (___var_%d != NULL) {\n", fault_var);
		c_emit_stmt_chain(c, cleanup_fail);
		PRINT("} else {\n");
		if (cleanup)
		{
			c_emit_stmt_chain(c, cleanup);
		}
		PRINT("}\n");
	}
	else if (cleanup)
	{
		c_emit_stmt_chain(c, cleanup);
	}

	if (fault_var != 0)
	{
		PRINTF("__c3_current_fault = ___var_%d;\n", fault_var);
	}
}

static void c_emit_return(GenContext *c, Ast *stmt)
{
	if (!c->current_block_live)
	{
		return;
	}

	CValue ret_val = {0};
	bool has_ret   = false;
	if (stmt->return_stmt.expr)
	{
		c_emit_expr(c, &ret_val, stmt->return_stmt.expr);
		c_value_rvalue(c, &ret_val);
		has_ret   = true;
		c->retval = ret_val;
	}

	bool is_fn_optional = c->current_return_type && type_is_optional(c->current_return_type);
	Type *cur_ret_t     = (c->current_return_type && is_valid_type_ptr(c->current_return_type)) ? c_safe_type_lower(c->current_return_type) : type_void;

	int saved_fault = 0;
	if (is_fn_optional)
	{
		saved_fault = c_create_variable(c);
		if (stmt->return_stmt.expr && type_is_optional(stmt->return_stmt.expr->type))
		{
			if (ret_val.optional != 0)
			{
				PRINTF("c3fault_t ___var_%d = (c3fault_t)(uintptr_t)___var_%d;\n", saved_fault, ret_val.optional);
			}
			else
			{
				PRINTF("c3fault_t ___var_%d = __c3_current_fault;\n", saved_fault);
			}
		}
		else if (has_ret && (ret_val.type && (ret_val.type->type_kind == TYPE_ANYFAULT || ret_val.type == type_fault)))
		{
			PRINTF("c3fault_t ___var_%d = (c3fault_t)(uintptr_t)___var_%d;\n", saved_fault, ret_val.var);
		}
		else
		{
			PRINTF("c3fault_t ___var_%d = NULL;\n", saved_fault);
		}
	}

	c_emit_defer_and_fault_cleanup(c, is_fn_optional ? saved_fault : 0, stmt->return_stmt.cleanup, stmt->return_stmt.cleanup_fail);

	if (cur_ret_t->type_kind == TYPE_VOID)
	{
		PRINT("return;\n");
		c->current_block_live = false;
		return;
	}

	if (has_ret)
	{
		Type *ret_t = (ret_val.type && is_valid_type_ptr(ret_val.type)) ? c_safe_type_lower(ret_val.type) : type_void;

		if (cur_ret_t->type_kind == TYPE_ANYFAULT || cur_ret_t == type_fault)
		{
			if (is_fn_optional)
			{
				PRINTF("return ___var_%d;\n", saved_fault);
			}
			else if (ret_val.var != 0)
			{
				PRINTF("return (c3fault_t)(uintptr_t)___var_%d;\n", ret_val.var);
			}
			else
			{
				PRINT("return NULL;\n");
			}
			c->current_block_live = false;
			return;
		}

		if ((ret_t->type_kind == TYPE_ANYFAULT || ret_t == type_fault) &&
		    cur_ret_t->type_kind != TYPE_ANYFAULT && cur_ret_t != type_fault && cur_ret_t->type_kind != TYPE_VOID)
		{
			PRINTF("return (%s)%s;\n", c_type_name(c, cur_ret_t), c_type_zero_literal(cur_ret_t));
			c->current_block_live = false;
			return;
		}

		if (ret_val.var == 0)
		{
			PRINTF("return (%s)%s;\n", c_type_name(c, cur_ret_t), c_type_zero_literal(cur_ret_t));
			c->current_block_live = false;
			return;
		}

		if ((cur_ret_t->type_kind == TYPE_ANY || cur_ret_t->type_kind == TYPE_INTERFACE) && (ret_t->type_kind == TYPE_ANY || ret_t->type_kind == TYPE_INTERFACE))
		{
			PRINTF("return ___var_%d;\n", ret_val.var);
		}
		else if ((cur_ret_t->type_kind == TYPE_ANY || cur_ret_t->type_kind == TYPE_INTERFACE) && ret_t->type_kind == TYPE_POINTER)
		{
			Type *ptype = (ret_t->pointer && is_valid_type_ptr(ret_t->pointer)) ? ret_t->pointer : type_void;
			PRINTF("return (__c3_any__){ .ptr = (void*)___var_%d, .typeid = (c3typeid_t)&%s };\n", ret_val.var, c_typeid_name(ptype));
		}
		else if (cur_ret_t->type_kind == TYPE_ANY || cur_ret_t->type_kind == TYPE_INTERFACE)
		{
			PRINTF("return (__c3_any__){ .ptr = (void*)&___var_%d, .typeid = (c3typeid_t)&%s };\n", ret_val.var, c_typeid_name(ret_t));
		}
		else if (cur_ret_t->type_kind == TYPE_POINTER && (ret_t->type_kind == TYPE_ANY || ret_t->type_kind == TYPE_INTERFACE))
		{
			PRINTF("return (%s)___var_%d.ptr;\n", c_type_name(c, cur_ret_t), ret_val.var);
		}
		else if (cur_ret_t->type_kind == TYPE_POINTER && ret_t->type_kind == TYPE_SLICE)
		{
			PRINTF("return (%s)___var_%d.ptr;\n", c_type_name(c, cur_ret_t), ret_val.var);
		}
		else if (c_type_is_aggregate(cur_ret_t) || c_type_is_aggregate(ret_t))
		{
			if (strcmp(c_type_name(c, cur_ret_t), c_type_name(c, ret_t)) != 0 && c_type_is_aggregate(cur_ret_t) && c_type_is_aggregate(ret_t))
			{
				PRINTF("return *(%s*)&___var_%d;\n", c_type_name(c, cur_ret_t), ret_val.var);
			}
			else
			{
				PRINTF("return ___var_%d;\n", ret_val.var);
			}
		}
		else if ((type_is_pointer(cur_ret_t) || cur_ret_t->type_kind == TYPE_FUNC_PTR) && type_is_integer(ret_t))
		{
			PRINTF("return (%s)(uintptr_t)___var_%d;\n", c_type_name(c, cur_ret_t), ret_val.var);
		}
		else if (type_is_integer(cur_ret_t) && (type_is_pointer(ret_t) || ret_t->type_kind == TYPE_FUNC_PTR))
		{
			PRINTF("return (%s)(uintptr_t)___var_%d;\n", c_type_name(c, cur_ret_t), ret_val.var);
		}
		else if (cur_ret_t->type_kind == TYPE_ANYFAULT && type_is_integer(ret_t))
		{
			PRINTF("return (c3fault_t)(uintptr_t)___var_%d;\n", ret_val.var);
		}
		else if (type_is_integer(cur_ret_t) && ret_t->type_kind == TYPE_ANYFAULT)
		{
			PRINTF("return (%s)(uintptr_t)___var_%d;\n", c_type_name(c, cur_ret_t), ret_val.var);
		}
		else if (cur_ret_t != ret_t && cur_ret_t->type_kind != TYPE_VOID)
		{
			PRINTF("return (%s)___var_%d;\n", c_type_name(c, cur_ret_t), ret_val.var);
		}
		else
		{
			PRINTF("return ___var_%d;\n", ret_val.var);
		}
	}
	else
	{
		if (cur_ret_t->type_kind == TYPE_VOID)
		{
			PRINT("return;\n");
		}
		else if (cur_ret_t->type_kind == TYPE_ANYFAULT || cur_ret_t == type_fault)
		{
			PRINT("return NULL;\n");
		}
		else
		{
			PRINTF("return (%s)%s;\n", c_type_name(c, cur_ret_t), c_type_zero_literal(cur_ret_t));
		}
	}
	c->current_block_live = false;
}

static void c_emit_foreach_stmt(GenContext *c, Ast *stmt)
{
	AstForeachStmt *fe = &stmt->foreach_stmt;
	Expr *enum_expr    = exprptrzero(fe->enumeration);
	if (!enum_expr)
	{
		return;
	}

	CValue enum_val = {0};
	c_emit_expr(c, &enum_val, enum_expr);
	if (enum_val.var == 0)
	{
		return;
	}

	Type *enum_type = (enum_val.type && is_valid_type_ptr(enum_val.type)) ? c_safe_type_lower(enum_val.type) : NULL;
	if (!enum_type)
	{
		enum_type = c_expr_type(enum_expr);
	}
	if (!enum_type || enum_type->type_kind == TYPE_VOID)
	{
		return;
	}

	const char *arrow = c_arrow(&enum_val);

	Type *elem_type = NULL;
	if (enum_type->type_kind == TYPE_SLICE || c_type_is_vec_or_arr(enum_type))
	{
		elem_type = enum_type->array.base ? c_safe_type_lower(enum_type->array.base) : type_char;
	}
	else if (enum_type->type_kind == TYPE_POINTER && enum_type->pointer)
	{
		Type *pt = c_safe_type_lower(enum_type->pointer);
		if (c_type_is_vec_or_arr(pt))
		{
			elem_type = pt->array.base ? c_safe_type_lower(pt->array.base) : type_char;
			arrow     = "->";
		}
		else
		{
			elem_type = pt;
		}
	}
	if (!elem_type)
	{
		elem_type = type_char;
	}
	c_emit_type_forward_decl(c, elem_type);

	int len_var = c_create_variable(c);
	if (enum_type->type_kind == TYPE_SLICE)
	{
		PRINTF("size_t ___var_%d = ___var_%d%slen;\n", len_var, enum_val.var, arrow);
	}
	else if (c_type_is_vec_or_arr(enum_type))
	{
		PRINTF("size_t ___var_%d = %llu;\n", len_var, (unsigned long long)enum_type->array.len);
	}
	else if (enum_type->type_kind == TYPE_POINTER && enum_type->pointer && c_type_is_vec_or_arr(enum_type->pointer))
	{
		PRINTF("size_t ___var_%d = %llu;\n", len_var, (unsigned long long)enum_type->pointer->array.len);
	}
	else
	{
		PRINTF("size_t ___var_%d = 0;\n", len_var);
	}

	int loop_idx              = c_create_variable(c);
	int loop_exit             = c_create_label(c);
	int loop_cont             = c_create_label(c);
	int old_break             = c->current_break_label;
	int old_cont              = c->current_continue_label;
	c->current_break_label    = loop_exit;
	c->current_continue_label = loop_cont;

	stmt->for_stmt.codegen.continue_block = (void *)(uintptr_t)loop_cont;
	stmt->for_stmt.codegen.exit_block     = (void *)(uintptr_t)loop_exit;
	Decl *label                           = declptrzero(fe->flow.label);
	if (label)
	{
		label->label.break_target    = (void *)(uintptr_t)loop_exit;
		label->label.continue_target = (void *)(uintptr_t)loop_cont;
	}

	if (fe->is_reverse)
	{
		PRINTF("for (size_t ___var_%d = ___var_%d; ___var_%d > 0; ) {\n", loop_idx, len_var, loop_idx);
		PRINTF("\t___var_%d--;\n", loop_idx);
	}
	else
	{
		PRINTF("for (size_t ___var_%d = 0; ___var_%d < ___var_%d; ___var_%d++) {\n", loop_idx, loop_idx, len_var, loop_idx);
	}

	Decl *idx_decl = declptrzero(fe->index);
	if (idx_decl)
	{
		VariableId idx_vid = c_get_or_create_decl_var(c, idx_decl);
		Type *idx_type     = c_decl_type(idx_decl);
		if (fe->index_by_ref)
		{
			PRINTF("\t___var_%d = (%s)&___var_%d;\n", idx_vid, c_type_name(c, idx_type), loop_idx);
		}
		else
		{
			PRINTF("\t___var_%d = (%s)___var_%d;\n", idx_vid, c_type_name(c, idx_type), loop_idx);
		}
	}

	Decl *var_decl = declptrzero(fe->variable);
	if (var_decl)
	{
		VariableId var_vid  = c_get_or_create_decl_var(c, var_decl);
		Type *var_type      = c_decl_type(var_decl);
		const char *vt_name = c_type_name(c, var_type);
		const char *et_name = c_type_name(c, elem_type);
		if (fe->value_by_ref)
		{
			PRINTF("\t___var_%d = (%s)&(___var_%d%sptr[___var_%d]);\n", var_vid, vt_name, enum_val.var, arrow, loop_idx);
		}
		else if (c_type_is_aggregate(elem_type))
		{
			PRINTF("\t__c3_memcpy(&___var_%d, &(___var_%d%sptr[___var_%d]), sizeof(%s));\n", var_vid, enum_val.var, arrow, loop_idx, et_name);
		}
		else
		{
			PRINTF("\t___var_%d = (%s)___var_%d%sptr[___var_%d];\n", var_vid, vt_name, enum_val.var, arrow, loop_idx);
		}
	}

	c->current_block_live = true;
	if (fe->body)
	{
		c_emit_stmt(c, astptr(fe->body));
	}

	c_emit_label(c, loop_cont);
	PRINT("}\n");
	c_emit_label(c, loop_exit);

	c->current_break_label    = old_break;
	c->current_continue_label = old_cont;
}

static char *c_convert_asm_template(const char *data)
{
	scratch_buffer_clear();
	for (const char *p = data; *p != '\0'; p++)
	{
		if (*p == '$')
		{
			if (p[1] == '$')
			{
				scratch_buffer_append_char('$');
				p++;
				continue;
			}
			if (p[1] >= '0' && p[1] <= '9')
			{
				scratch_buffer_append_char('%');
				continue;
			}
			if (strncmp(p, "${:private}", 11) == 0)
			{
				scratch_buffer_append("__c3_asm_label_");
				p += 10;
				continue;
			}
			scratch_buffer_append_char('$');
			continue;
		}
		if (*p == '.' && strncmp(p, ".${:uid}", 8) == 0)
		{
			scratch_buffer_append("_%=");
			p += 7;
			continue;
		}
		if (*p == '%')
		{
			scratch_buffer_append("%%");
			continue;
		}
		scratch_buffer_append_char(*p);
	}
	return scratch_buffer_copy();
}

static const char *c_get_asm_operand_name(GenContext *c, Decl *decl)
{
	decl = c_decl_unwrap(decl);
	if (!decl)
	{
		return "0";
	}
	if (c_is_file_global(decl))
	{
		c_emit_global_decl(c, decl);
		return c_get_decl_name(decl);
	}
	VariableId vid = c_get_or_create_decl_var(c, decl);
	return c_intern(str_printf("___var_%d", vid));
}

void c_emit_asm_block_stmt(GenContext *c, Ast *stmt)
{
	if (stmt->asm_block_stmt.is_string)
	{
		Expr *str_expr   = exprptr(stmt->asm_block_stmt.asm_string);
		const char *data = str_expr->const_expr.bytes.ptr;
		PRINT("\t__asm__ __volatile__ (");
		c_emit_string_literal(c, data, (ArrayIndex)strlen(data));
		PRINT(");\n");
		return;
	}

	AsmInlineBlock *block = stmt->asm_block_stmt.block;
	char *raw_asm         = str_dup(codegen_create_asm(stmt));
	char *gnu_asm         = c_convert_asm_template(raw_asm);

	int out_count                = (block && block->output_vars) ? vec_size(block->output_vars) : 0;
	const char **out_constraints = NULL;
	const char **out_names       = NULL;
	for (int i = 0; i < out_count; i++)
	{
		ExprAsmArg *var         = block->output_vars[i];
		const char *constraint = (var->kind == ASM_ARG_MEMVAR)
		                             ? (var->ident.early_clobber ? "=&m" : "=m")
		                             : (var->ident.early_clobber ? "=&r" : "=r");
		const char *name       = c_get_asm_operand_name(c, var->ident.ident_decl);
		vec_add(out_constraints, constraint);
		vec_add(out_names, name);
		if (IS_OPTIONAL(var->ident.ident_decl))
		{
			c_emit_assign_decl_fault(c, var->ident.ident_decl, NULL, NULL);
		}
	}

	int in_count                = (block && block->input) ? vec_size(block->input) : 0;
	const char **in_constraints = NULL;
	const char **in_names       = NULL;
	for (int i = 0; i < in_count; i++)
	{
		ExprAsmArg *val         = block->input[i];
		const char *constraint = NULL;
		const char *name       = NULL;
		switch (val->kind)
		{
			case ASM_ARG_MEMADDR:
			{
				constraint       = "r";
				const char *base = c_get_asm_operand_name(c, val->ident.ident_decl);
				name             = c_intern(str_printf("&%s", base));
				break;
			}
			case ASM_ARG_MEMVAR:
			{
				constraint = "m";
				name       = c_get_asm_operand_name(c, val->ident.ident_decl);
				break;
			}
			case ASM_ARG_REGVAR:
			{
				if (val->ident.copy_output)
				{
					char buf[16];
					snprintf(buf, sizeof(buf), "%d", val->index);
					constraint = c_intern(buf);
				}
				else
				{
					constraint = "r";
				}
				name = c_get_asm_operand_name(c, val->ident.ident_decl);
				break;
			}
			case ASM_ARG_VALUE:
			{
				CValue eval   = {0};
				Expr *in_expr = exprptr(val->expr_id);
				c_emit_expr(c, &eval, in_expr);
				c_value_rvalue(c, &eval);
				c_ensure_cvalue_var(c, &eval, in_expr->type);
				constraint = "r";
				name       = c_intern(str_printf("___var_%d", eval.var));
				break;
			}
			default:
				UNREACHABLE_VOID
		}
		vec_add(in_constraints, constraint);
		vec_add(in_names, name);
	}

	const char **clobbers = NULL;
	if (compiler.platform.arch == ARCH_TYPE_X86_64 || compiler.platform.arch == ARCH_TYPE_X86)
	{
		vec_add(clobbers, "cc");
	}
	if (block)
	{
		for (int i = 0; i < CLOBBER_FLAG_ELEMENTS; i++)
		{
			uint64_t clobber_mask = block->clobbers.mask[i];
			if (!clobber_mask)
			{
				continue;
			}
			uint64_t mask = 1;
			for (int j = 0; j < 64; j++)
			{
				if (mask & clobber_mask)
				{
					int clobber_index        = i * 64 + j;
					const char *clobber_name = asm_clobber_by_index(clobber_index);
					if (clobber_name && *clobber_name)
					{
						if (strcmp(clobber_name, "flags") == 0 || strcmp(clobber_name, "dirflag") == 0 ||
						    strcmp(clobber_name, "fpsr") == 0 || strcmp(clobber_name, "cc") == 0)
						{
							clobber_name = "cc";
						}
						bool exists = false;
						FOREACH(const char *, existing, clobbers)
						{
							if (strcmp(existing, clobber_name) == 0)
							{
								exists = true;
								break;
							}
						}
						if (!exists)
						{
							vec_add(clobbers, clobber_name);
						}
					}
				}
				mask <<= 1;
			}
		}
	}

	PRINT("\t__asm__ __volatile__ (\n\t\t");
	c_emit_string_literal(c, gnu_asm, (ArrayIndex)strlen(gnu_asm));

	PRINT("\n\t\t: ");
	for (int i = 0; i < out_count; i++)
	{
		if (i > 0)
		{
			PRINT(", ");
		}
		PRINTF("\"%s\" (%s)", out_constraints[i], out_names[i]);
	}

	if (in_count > 0 || vec_size(clobbers) > 0)
	{
		PRINT("\n\t\t: ");
		for (int i = 0; i < in_count; i++)
		{
			if (i > 0)
			{
				PRINT(", ");
			}
			PRINTF("\"%s\" (%s)", in_constraints[i], in_names[i]);
		}
	}

	if (vec_size(clobbers) > 0)
	{
		PRINT("\n\t\t: ");
		FOREACH_IDX(i, const char *, clobber, clobbers)
		{
			if (i > 0)
			{
				PRINT(", ");
			}
			PRINTF("\"%s\"", clobber);
		}
	}

	PRINT("\n\t);\n");
}

void c_emit_stmt(GenContext *c, Ast *stmt)
{
	if (!stmt || !c->current_block_live)
	{
		return;
	}

	switch (stmt->ast_kind)
	{
		case AST_POISONED:
			UNREACHABLE_VOID
		case AST_COMPOUND_STMT:
			PRINT("{\n");
			c_emit_stmt_chain(c, stmt->compound_stmt.first_stmt);
			PRINT("}\n");
			return;
		case AST_CT_COMPOUND_STMT:
			c_emit_stmt_chain(c, stmt->ct_compound_stmt);
			return;
		case AST_DECLARE_STMT:
		{
			CValue value = {0};
			c_emit_local_decl(c, stmt->declare_stmt, &value);
			return;
		}
		case AST_DECLS_STMT:
		{
			FOREACH(Decl *, decl, stmt->decls_stmt)
			{
				if (decl)
				{
					CValue value = {0};
					c_emit_local_decl(c, decl, &value);
				}
			}
			return;
		}
		case AST_EXPR_STMT:
			c_emit_ignored_expr(c, stmt->expr_stmt);
			return;
		case AST_IF_STMT:
			c_emit_if_stmt(c, stmt);
			return;
		case AST_FOR_STMT:
			c_emit_for_stmt(c, stmt);
			return;
		case AST_FOREACH_STMT:
			c_emit_foreach_stmt(c, stmt);
			return;
		case AST_RETURN_STMT:
			c_emit_return(c, stmt);
			return;
		case AST_BLOCK_EXIT_STMT:
		{
			CValue ret_val    = {0};
			bool has_ret_expr = (stmt->return_stmt.expr != NULL);
			if (has_ret_expr)
			{
				c_emit_expr(c, &ret_val, stmt->return_stmt.expr);
				c_value_rvalue(c, &ret_val);
				c->retval = ret_val;
			}

			int target_ret_var    = c->current_macro_ret_var;
			int target_fault_var  = c->current_macro_fault_var;
			int exit_label        = c->current_macro_exit_label;
			Type *target_ret_type = c->current_macro_ret_type;
			if (stmt->return_stmt.block_exit_ref && *stmt->return_stmt.block_exit_ref)
			{
				BlockExit *exit = *stmt->return_stmt.block_exit_ref;
				int bret        = (int)(uintptr_t)exit->block_return_out;
				int blbl        = (int)(uintptr_t)exit->block_return_exit;
				int bfault      = (int)(uintptr_t)exit->block_error_var;
				Type *btype     = (Type *)exit->block_optional_exit;
				if (blbl)
				{
					exit_label       = blbl;
					target_ret_var   = bret;
					target_fault_var = bfault;
				}
				if (btype)
				{
					target_ret_type = btype;
				}
			}

			if (has_ret_expr)
			{
				if (target_fault_var != 0)
				{
					if (ret_val.type && (ret_val.type->type_kind == TYPE_ANYFAULT || ret_val.type == type_fault))
					{
						PRINTF("___var_%d = (c3fault_t)(uintptr_t)___var_%d;\n", target_fault_var, ret_val.var);
					}
					else if (ret_val.optional != 0)
					{
						PRINTF("___var_%d = (c3fault_t)(uintptr_t)___var_%d;\n", target_fault_var, ret_val.optional);
						if (target_ret_var)
						{
							c_emit_assign_var(c, target_ret_var, target_ret_type, &ret_val);
						}
					}
					else if (stmt->return_stmt.expr && type_is_optional(stmt->return_stmt.expr->type))
					{
						PRINTF("___var_%d = __c3_current_fault;\n", target_fault_var);
						if (target_ret_var)
						{
							c_emit_assign_var(c, target_ret_var, target_ret_type, &ret_val);
						}
					}
					else
					{
						PRINTF("___var_%d = NULL;\n", target_fault_var);
						if (target_ret_var)
						{
							c_emit_assign_var(c, target_ret_var, target_ret_type, &ret_val);
						}
					}
				}
				else if (target_ret_var)
				{
					c_emit_assign_var(c, target_ret_var, target_ret_type, &ret_val);
				}
			}
			else if (target_fault_var != 0)
			{
				PRINTF("___var_%d = NULL;\n", target_fault_var);
			}

			c_emit_defer_and_fault_cleanup(c, target_fault_var, stmt->return_stmt.cleanup, stmt->return_stmt.cleanup_fail);

			if (exit_label)
			{
				PRINTF("goto __C3_LABEL_%d;\n", exit_label);
			}
			c->current_block_live = false;
			return;
		}
		case AST_BREAK_STMT:
		{
			if (stmt->contbreak_stmt.defers)
			{
				c_emit_stmt_chain(c, stmt->contbreak_stmt.defers);
			}
			Ast *target = stmt->contbreak_stmt.ast ? astptrzero(stmt->contbreak_stmt.ast) : NULL;
			if (target)
			{
				int lbl = 0;
				if (target->ast_kind == AST_FOR_STMT || target->ast_kind == AST_FOREACH_STMT)
				{
					lbl = (int)(uintptr_t)target->for_stmt.codegen.exit_block;
				}
				else if (target->ast_kind == AST_IF_STMT)
				{
					lbl = (int)(uintptr_t)target->if_stmt.codegen.break_block;
				}
				else if (target->ast_kind == AST_SWITCH_STMT)
				{
					lbl = (int)(uintptr_t)target->switch_stmt.codegen.exit_block;
				}
				if (lbl)
				{
					PRINTF("goto __C3_LABEL_%d;\n", lbl);
					c->current_block_live = false;
					return;
				}
			}
			if (c->current_break_label)
			{
				PRINTF("goto __C3_LABEL_%d;\n", c->current_break_label);
			}
			else
			{
				PRINT("break;\n");
			}
			c->current_block_live = false;
			return;
		}
		case AST_CONTINUE_STMT:
		{
			if (stmt->contbreak_stmt.defers)
			{
				c_emit_stmt_chain(c, stmt->contbreak_stmt.defers);
			}
			Ast *target = stmt->contbreak_stmt.ast ? astptrzero(stmt->contbreak_stmt.ast) : NULL;
			if (target)
			{
				int lbl = 0;
				if (target->ast_kind == AST_FOR_STMT || target->ast_kind == AST_FOREACH_STMT)
				{
					lbl = (int)(uintptr_t)target->for_stmt.codegen.continue_block;
				}
				if (lbl)
				{
					PRINTF("goto __C3_LABEL_%d;\n", lbl);
					c->current_block_live = false;
					return;
				}
			}
			if (c->current_continue_label)
			{
				PRINTF("goto __C3_LABEL_%d;\n", c->current_continue_label);
			}
			else
			{
				PRINT("continue;\n");
			}
			c->current_block_live = false;
			return;
		}
		case AST_SWITCH_STMT:
			c_emit_switch_stmt(c, stmt);
			return;
		case AST_NEXTCASE_STMT:
			c_emit_nextcase_stmt(c, stmt);
			return;
		case AST_ASSERT_STMT:
			if (compile_asserts())
			{
				CValue cval = {0};
				c_emit_expr(c, &cval, exprptr(stmt->assert_stmt.expr));
				c_value_rvalue(c, &cval);
				if (cval.var != 0)
				{
					PRINTF("if (!___var_%d) { __c3_abort(); }\n", cval.var);
				}
			}
			return;
		case AST_ASM_BLOCK_STMT:
			c_emit_asm_block_stmt(c, stmt);
			return;
		case AST_NOP_STMT:
			PRINT(";\n");
			return;
		case AST_DEFER_STMT:
			return;
		default:
			PRINT("/* STMT */\n");
			return;
	}
}

void c_emit_stmt_chain(GenContext *c, AstId current)
{
	while (current)
	{
		Ast *stmt = ast_next(&current);
		if (!c->current_block_live)
		{
			continue;
		}
		c_emit_stmt(c, stmt);
	}
}