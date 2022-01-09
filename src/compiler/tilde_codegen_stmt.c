// Copyright (c) 2022 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "tilde_internal.h"

#if TB_BACKEND

void tilde_emit_compound_stmt(TbContext *context, Ast *ast)
{
	assert(ast->ast_kind == AST_COMPOUND_STMT);
	AstId current = ast->compound_stmt.first_stmt;
	while (current)
	{
		tilde_emit_stmt(context, ast_next(&current));
	}
	tilde_emit_defer(context, ast->compound_stmt.defer_list.start, ast->compound_stmt.defer_list.end);
}

static void llvm_emit_ct_compound_stmt(TbContext *context, Ast *ast)
{
	assert(ast->ast_kind == AST_CT_COMPOUND_STMT);
	AstId current = ast->compound_stmt.first_stmt;
	while (current)
	{
		tilde_emit_stmt(context, ast_next(&current));
	}
}

TB_Reg tilde_emit_local_decl(TbContext *c, Decl *decl)
{
	// 1. Get the declaration and the TB type.
	Type *var_type = type_lowering(type_no_fail(decl->type));

	// 2. In the case we have a static variable,
	//    then we essentially treat this as a global.
	if (decl->var.is_static)
	{
		if (IS_FAILABLE(decl))
		{
			scratch_buffer_clear();
			scratch_buffer_append(decl->external_name);
			scratch_buffer_append(".f");
			TB_InitializerID initializer = tb_initializer_create(c->module, type_size(type_anyerr), type_alloca_alignment(type_anyerr), 1);
			decl->var.tb_failable_reg = tb_global_create(c->module, scratch_buffer_to_string(), initializer);
		}
		TB_InitializerID static_initializer = tb_initializer_create(c->module, type_size(var_type), type_alloca_alignment(var_type), 1);
		decl->tb_register = tb_global_create(c->module, "tempglobal", static_initializer);
		tilde_emit_global_initializer(c, decl);
		return decl->tb_register;
	}
	tilde_emit_local_var_alloca(c, decl);
	TB_Register reg = decl->tb_register;
	Expr *init = decl->var.init_expr;
	if (IS_FAILABLE(decl))
	{
		scratch_buffer_clear();
		scratch_buffer_append(decl->name);
		scratch_buffer_append(".f");
		decl->var.tb_failable_reg = tb_inst_local(c->f, type_size(type_anyerr), type_alloca_alignment(type_anyerr));
		// Only clear out the result if the assignment isn't a failable.
	}

	TBEValue value;
	value_set_decl(&value, decl);
	if (init)
	{
		// If we don't have undef, then make an assign.
		if (init->expr_kind != EXPR_UNDEF)
		{
			tilde_emit_assign_expr(c, &value, decl->var.init_expr, decl->var.tb_failable_reg);
		}
		// TODO trap on undef in debug mode.
	}
	else
	{
		if (decl->var.tb_failable_reg)
		{
			tilde_store_zero(c, type_anyerr, decl->var.tb_failable_reg, type_abi_alignment(type_anyerr));
		}
		tilde_store_value_zero(c, &value);
	}
	return reg;

}

void tilde_emit_defer(TbContext *c, AstId defer_start, AstId defer_end)
{
	if (defer_start == defer_end) return;
	AstId defer = defer_start;
	while (defer && defer != defer_end)
	{
		Ast *def = astptr(defer);
		TB_Label exit = tb_inst_new_label_id(c->f);
		Ast *body = def->defer_stmt.body;
		def->defer_stmt.codegen.exit_val = exit;
		tilde_emit_stmt(c, body);
		tb_inst_goto(c->f, exit);
		tb_inst_label(c->f, exit);
		defer = def->defer_stmt.prev_defer;
	}
}



void tilde_emit_stmt(TbContext *c, Ast *ast)
{
	switch (ast->ast_kind)
	{
		case AST_DECLARE_STMT:
			tilde_emit_local_decl(c, ast->declare_stmt);
			break;
		case AST_RETURN_STMT:
			//TbContext_emit_return(c, ast);
			tb_inst_ret(c->f, TB_NULL_REG);
			break;
		default:
			break;
	}
}

#endif