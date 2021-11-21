// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_internal.h"

static void gencontext_emit_switch_body(GenContext *c, BEValue *switch_value, Ast *switch_ast);

static bool ast_is_not_empty(Ast *ast)
{
	if (!ast) return false;
	if (ast->ast_kind != AST_COMPOUND_STMT) return true;
	if (vec_size(ast->compound_stmt.stmts)) return true;
	return ast->compound_stmt.defer_list.start != ast->compound_stmt.defer_list.end;
}


void llvm_emit_compound_stmt(GenContext *context, Ast *ast)
{
	if (llvm_use_debug(context))
	{
		llvm_debug_push_lexical_scope(context, ast->span);
	}
	assert(ast->ast_kind == AST_COMPOUND_STMT);
	VECEACH(ast->compound_stmt.stmts, i)
	{
		llvm_emit_stmt(context, ast->compound_stmt.stmts[i]);
	}
	llvm_emit_defer(context, ast->compound_stmt.defer_list.start, ast->compound_stmt.defer_list.end);
	if (llvm_use_debug(context))
	{
		llvm_debug_scope_pop(context);
	}
}

void gencontext_emit_ct_compound_stmt(GenContext *context, Ast *ast)
{
	assert(ast->ast_kind == AST_CT_COMPOUND_STMT);
	VECEACH(ast->compound_stmt.stmts, i)
	{
		llvm_emit_stmt(context, ast->compound_stmt.stmts[i]);
	}
}

/**
 * This emits a local declaration.
 */
LLVMValueRef llvm_emit_local_decl(GenContext *c, Decl *decl)
{
	// 1. Get the declaration and the LLVM type.
	Type *var_type = type_lowering(type_no_fail(decl->type));
	LLVMTypeRef alloc_type = llvm_get_type(c, var_type);

	// 2. In the case we have a static variable,
	//    then we essentially treat this as a global.
	if (decl->var.is_static)
	{
		void *builder = c->builder;
		c->builder = NULL;
		decl->backend_ref = LLVMAddGlobal(c->module, alloc_type, "tempglobal");
		if (IS_FAILABLE(decl))
		{
			scratch_buffer_clear();
			scratch_buffer_append(decl->external_name);
			scratch_buffer_append(".f");
			decl->var.failable_ref = LLVMAddGlobal(c->module, llvm_get_type(c, type_anyerr), scratch_buffer_to_string());
		}
		llvm_emit_global_variable_init(c, decl);
		c->builder = builder;
		return decl->backend_ref;
	}
	llvm_emit_local_var_alloca(c, decl);
	Expr *init = decl->var.init_expr;
	if (IS_FAILABLE(decl))
	{
		scratch_buffer_clear();
		scratch_buffer_append(decl->name);
		scratch_buffer_append(".f");
		decl->var.failable_ref = llvm_emit_alloca_aligned(c, type_anyerr, scratch_buffer_to_string());
		// Only clear out the result if the assignment isn't a failable.
	}

	if (init)
	{
		// If we don't have undef, then make an assign.
		if (init->expr_kind != EXPR_UNDEF)
		{
			BEValue value;
			llvm_value_set_decl_address(&value, decl);
			value.kind = BE_ADDRESS;
			llvm_emit_assign_expr(c, &value, decl->var.init_expr, decl->var.failable_ref);
		}
		// TODO trap on undef in debug mode.
	}
	else
	{
		if (decl->var.failable_ref)
		{
			LLVMBuildStore(c->builder, LLVMConstNull(llvm_get_type(c, type_anyerr)), decl->var.failable_ref);
		}

		Type *type = type_lowering(decl->type);
		// Normal case, zero init.
		if (type_is_builtin(type->type_kind) || type->type_kind == TYPE_POINTER)
		{
			llvm_emit_store(c, decl, LLVMConstNull(alloc_type));
		}
		else
		{
			BEValue value;
			llvm_value_set_decl_address(&value, decl);
			value.kind = BE_ADDRESS;
			llvm_emit_memclear(c, &value);
		}
	}
	return decl->backend_ref;
}

void llvm_emit_decl_expr_list_ignore_result(GenContext *context, Expr *expr)
{
	assert(expr->expr_kind == EXPR_COND);
	VECEACH(expr->cond_expr, i)
	{
		BEValue value;
		llvm_emit_expr(context, &value, expr->cond_expr[i]);
	}
}

void llvm_emit_decl_expr_list(GenContext *context, BEValue *be_value, Expr *expr, bool bool_cast)
{
	assert(expr->expr_kind == EXPR_COND);
	ByteSize size = vec_size(expr->cond_expr);
	ByteSize last_index = size - 1;
	for (ByteSize i = 0; i < last_index; i++)
	{
		BEValue value;
		llvm_emit_expr(context, &value, expr->cond_expr[i]);
	}
	Expr *last = expr->cond_expr[last_index];
	Type *type = last->type;
	llvm_emit_expr(context, be_value, last);
	if (last->expr_kind == EXPR_DECL)
	{
		type = last->decl_expr->var.type_info->type;
		LLVMValueRef decl_value = llvm_emit_local_decl(context, last->decl_expr);
		if (bool_cast && last->decl_expr->var.unwrap)
		{
			llvm_value_set_bool(be_value, LLVMConstInt(context->bool_type, 1, false));
			return;
		}
		llvm_value_set_address(be_value, decl_value, type);
	}
	if (bool_cast)
	{
		type = type_lowering(type);
		if (type->type_kind != TYPE_BOOL)
		{
			CastKind cast = cast_to_bool_kind(type);
			llvm_emit_cast(context, cast, be_value, type, type_bool);
		}
	}
}

void llvm_emit_jmp(GenContext *context, LLVMBasicBlockRef block)
{
	llvm_emit_br(context, block);
	LLVMBasicBlockRef post_jump_block = llvm_basic_block_new(context, "jmp");
	llvm_emit_block(context, post_jump_block);
}

static inline void gencontext_emit_return(GenContext *c, Ast *ast)
{
	// Ensure we are on a branch that is non empty.
	if (!llvm_emit_check_block_branch(c)) return;

	bool in_expression_block = c->in_block > 0;

	PUSH_ERROR();

	LLVMBasicBlockRef error_return_block = NULL;
	LLVMValueRef error_out = NULL;
	if (in_expression_block)
	{
		c->error_var = c->block_error_var;
		c->catch_block = c->block_failable_exit;
	}
	else if (IS_FAILABLE(c->cur_func_decl->func_decl.function_signature.rtype))
	{
		error_return_block = llvm_basic_block_new(c, "err_retblock");
		error_out = llvm_emit_alloca_aligned(c, type_anyerr, "reterr");
		c->error_var = error_out;
		c->catch_block = error_return_block;
	}

	bool has_return_value = ast->return_stmt.expr != NULL;
	BEValue return_value = { 0 };
	if (has_return_value)
	{
		llvm_emit_expr(c, &return_value, ast->return_stmt.expr);
		llvm_value_fold_failable(c, &return_value);
	}

	POP_ERROR();

	llvm_emit_defer(c, ast->return_stmt.defer, 0);

	// Are we in an expression block?
	if (in_expression_block)
	{
		if (c->return_out)
		{
			llvm_store_bevalue_aligned(c, c->return_out, &return_value, 0);
		}
		llvm_emit_jmp(c, c->block_return_exit);
		return;
	}

	if (!has_return_value)
	{
		llvm_emit_return_implicit(c);
	}
	else
	{
		llvm_emit_return_abi(c, &return_value, NULL);
	}
	c->current_block = NULL;
	if (error_return_block && LLVMGetFirstUse(LLVMBasicBlockAsValue(error_return_block)))
	{
		llvm_emit_block(c, error_return_block);
		BEValue value;
		llvm_value_set_address(&value, error_out, type_anyerr);
		llvm_emit_return_abi(c, NULL, &value);
		c->current_block = NULL;
	}
	LLVMBasicBlockRef post_ret_block = llvm_basic_block_new(c, "ret");
	llvm_emit_block(c, post_ret_block);
}



/**
 * Emit if (...) { ... } else { ... }
 *
 * This code is slightly optimized to omit branches when not needed. This is something LLVM
 * will optimize as well, but it is convenient to make the code slightly smaller for LLVM to work with:
 * 1. If the "then" branch is empty, replace it with "exit".
 * 2. If the "else" branch is empty or missing, replace if with "exit".
 * 3. If both "else" and "then" branches are empty, replace it with just the condition and remove the "exit"
 */
void llvm_emit_if(GenContext *c, Ast *ast)
{
	// We need at least the exit block and the "then" block.
	LLVMBasicBlockRef exit_block = llvm_basic_block_new(c, "if.exit");
	LLVMBasicBlockRef then_block = exit_block;
	LLVMBasicBlockRef else_block = exit_block;

	Ast *then_body = ast->if_stmt.then_body;
	// Only generate a target if
	if (ast_is_not_empty(then_body))
	{
		then_block = llvm_basic_block_new(c, "if.then");
	}

	// We have an optional else block.
	if (ast_is_not_empty(ast->if_stmt.else_body))
	{
		else_block = llvm_basic_block_new(c, "if.else");
	}

	ast->if_stmt.break_block = exit_block;

	// Output boolean value and switch.

	Decl *label = ast->if_stmt.flow.label;
	if (label)
	{
		label->label.break_target = exit_block;
	}

	BEValue be_value = { 0 };

	bool exit_in_use = true;

	if (then_body->ast_kind == AST_IF_CATCH_SWITCH_STMT)
	{
		llvm_emit_decl_expr_list(c, &be_value, ast->if_stmt.cond, false);
		llvm_value_rvalue(c, &be_value);
		BEValue comp;
		llvm_emit_int_comp_zero(c, &comp, &be_value, BINARYOP_NE);
		llvm_emit_cond_br(c, &comp, then_block, else_block);
		llvm_emit_br(c, then_block);
		llvm_emit_block(c, then_block);
		gencontext_emit_switch_body(c, &be_value, then_body);
		llvm_emit_br(c, exit_block);
		goto EMIT_ELSE;
	}

	llvm_emit_decl_expr_list(c, &be_value, ast->if_stmt.cond, true);

	llvm_value_rvalue(c, &be_value);

	assert(llvm_value_is_bool(&be_value));

	if (llvm_value_is_const(&be_value) && then_block != else_block)
	{
		if (LLVMConstIntGetZExtValue(be_value.value))
		{
			llvm_emit_br(c, then_block);
			else_block = exit_block;
		}
		else
		{
			llvm_emit_br(c, else_block);
			then_block = exit_block;
		}
	}
	else
	{
		if (then_block != else_block)
		{
			llvm_emit_cond_br(c, &be_value, then_block, else_block);
		}
		else
		{
			exit_in_use = LLVMGetFirstUse(LLVMBasicBlockAsValue(exit_block)) != NULL;
			if (exit_in_use) llvm_emit_br(c, exit_block);
		}
	}

	// Emit the 'then' code.
	if (then_block != exit_block)
	{
		llvm_emit_block(c, then_block);
		llvm_emit_stmt(c, ast->if_stmt.then_body);

		// Jump to exit.
		llvm_emit_br(c, exit_block);
	}

	EMIT_ELSE:
	// Emit the 'else' branch if present.
	if (else_block != exit_block)
	{
		llvm_emit_block(c, else_block);
		llvm_emit_stmt(c, ast->if_stmt.else_body);
		llvm_emit_br(c, exit_block);
	}

	// And now we just emit the exit block.
	if (exit_in_use)
	{
		llvm_emit_block(c, exit_block);
	}
}


void gencontext_emit_for_stmt(GenContext *c, Ast *ast)
{
	// First, emit all inits.

	if (ast->for_stmt.init) llvm_emit_decl_expr_list_ignore_result(c, ast->for_stmt.init);

	// We have 3 optional parts, which makes this code bit complicated.
	LLVMBasicBlockRef exit_block = llvm_basic_block_new(c, "for.exit");
	LLVMBasicBlockRef inc_block = ast->for_stmt.incr ? llvm_basic_block_new(c, "for.inc") : NULL;
	LLVMBasicBlockRef body_block = ast_is_not_empty(ast->for_stmt.body) ? llvm_basic_block_new(c, "for.body") : NULL;
	LLVMBasicBlockRef cond_block = ast->for_stmt.cond ? llvm_basic_block_new(c, "for.cond") : NULL;

	// Break is simple it always jumps out.
	// For continue:
	// 1. If there is inc, jump to the condition
	// 2. If there is no condition, jump to the body.
	LLVMBasicBlockRef continue_block = inc_block ? inc_block : (cond_block ? cond_block : body_block);

	ast->for_stmt.continue_block = continue_block;
	ast->for_stmt.exit_block = exit_block;

	LLVMBasicBlockRef loopback_block = cond_block;
	if (cond_block)
	{
		// Emit cond
		llvm_emit_br(c, cond_block);
		llvm_emit_block(c, cond_block);

		BEValue be_value;
		llvm_emit_expr(c, &be_value, ast->for_stmt.cond);
		llvm_value_rvalue(c, &be_value);

		assert(llvm_value_is_bool(&be_value));

		// If we have a body, conditionally jump to it.
		if (body_block)
		{
			llvm_emit_cond_br(c, &be_value, body_block, exit_block);
		}
		else
		{
			// Otherwise jump to inc or cond depending on what's available.
			llvm_emit_cond_br(c, &be_value, inc_block ? inc_block : cond_block, exit_block);
		}
	}

	if (body_block)
	{
		if (!cond_block)
		{
			// We don't have a cond, so we need to unconditionally jump here.
			loopback_block = body_block;
			llvm_emit_br(c, body_block);
		}
		llvm_emit_block(c, body_block);
		llvm_emit_stmt(c, ast->for_stmt.body);
		// IMPROVE handle continue/break.
		if (inc_block)
		{
			llvm_emit_br(c, inc_block);
		}
	}

	if (inc_block)
	{
		if (!body_block && !cond_block)
		{
			// We have neither cond nor body, so jump here
			loopback_block = inc_block;
			llvm_emit_br(c, inc_block);
		}
		// Emit the block
		llvm_emit_block(c, inc_block);
		BEValue dummy;
		llvm_emit_expr(c, &dummy, ast->for_stmt.incr);
	}

	if (!loopback_block)
	{
		loopback_block = llvm_basic_block_new(c, "infiniteloop");
		llvm_emit_br(c, loopback_block);
		llvm_emit_block(c, loopback_block);
	}
	// Loop back.
	llvm_emit_br(c, loopback_block);

	// And insert exit block
	llvm_emit_block(c, exit_block);
}

static void llvm_emit_foreach_stmt(GenContext *c, Ast *ast)
{
	// First we generate an exit.
	LLVMBasicBlockRef exit_block = llvm_basic_block_new(c, "foreach.exit");

	PUSH_ERROR();

	llvm_set_error_exit(c, exit_block);

	// First evaluate the enumerated collection
	BEValue enum_value;
	llvm_emit_expr(c, &enum_value, ast->foreach_stmt.enumeration);

	// Get the length
	BEValue len;
	llvm_emit_len_for_expr(c, &len, &enum_value);
	llvm_value_rvalue(c, &len);

	// We pop the error here.
	POP_ERROR();

	llvm_emit_ptr_from_array(c, &enum_value);

	// Create the index and optionally the index var
	LLVMTypeRef real_index_type = llvm_get_type(c, type_usize);
	BEValue index_var = { 0 };
	LLVMTypeRef index_type = ast->foreach_stmt.index ? llvm_get_type(c, ast->foreach_stmt.index->type) : NULL;
	BEValue index = { 0 };

	bool extend_bits = false;

	// In case we have an index, set it up.
	if (ast->foreach_stmt.index)
	{
		llvm_emit_local_var_alloca(c, ast->foreach_stmt.index);
		llvm_value_set_address(&index_var, ast->foreach_stmt.index->backend_ref, ast->foreach_stmt.index->type);
		// And set it to zero.
		llvm_store_bevalue_raw(c, &index_var, llvm_get_zero(c, index_var.type));
		index = index_var;
		extend_bits = type_size(index_var.type) > type_size(type_isize);
	}

	// If types don't match (either index has a different type or it doesn't exist)
	// then create the address for the internal index and set it to zero.
	if (index_type != real_index_type)
	{
		llvm_value_set_address(&index, llvm_emit_alloca(c, real_index_type, type_abi_alignment(type_isize), "idx"), type_usize);
		llvm_store_bevalue_raw(c, &index, llvm_get_zero(c, index.type));
	}

	Type *actual_type = type_get_indexed_type(ast->foreach_stmt.enumeration->type);
	LLVMTypeRef actual_type_llvm = llvm_get_type(c, actual_type);

	llvm_emit_local_var_alloca(c, ast->foreach_stmt.variable);
	Type *var_type = type_lowering(ast->foreach_stmt.variable->type);
	LLVMTypeRef var_type_llvm = llvm_get_type(c, var_type);
	BEValue var;
	llvm_value_set_address(&var, ast->foreach_stmt.variable->backend_ref, var_type);

	LLVMBasicBlockRef inc_block = llvm_basic_block_new(c, "foreach.inc");
	LLVMBasicBlockRef body_block = llvm_basic_block_new(c, "foreach.body");
	LLVMBasicBlockRef cond_block = llvm_basic_block_new(c, "foreach.cond");

	ast->foreach_stmt.continue_block = inc_block;
	ast->foreach_stmt.exit_block = exit_block;

	// Emit cond
	llvm_emit_br(c, cond_block);
	llvm_emit_block(c, cond_block);

	BEValue index_value;
	llvm_value_set(&index_value, llvm_value_rvalue_store(c, &index), type_usize);
	BEValue b;
	llvm_emit_int_comparison(c, &b, &index_value, &len, BINARYOP_LT);
	llvm_emit_cond_br(c, &b, body_block, exit_block);

	llvm_emit_block(c, body_block);

	// In the case where we have an index that is smaller, we need to do a cast.
	if (index_var.value && index.value != index_var.value)
	{
		LLVMValueRef stored_value;
		if (extend_bits)
		{
			// Note that we zero extend. We never deal in negative indices.
			stored_value = LLVMBuildZExt(c->builder, index_value.value, index_type, "");
		}
		else
		{
			stored_value = LLVMBuildTrunc(c->builder, index_value.value, index_type, "");
		}
		llvm_store_bevalue_raw(c, &index_var, stored_value);
	}

	assert(llvm_value_is_addr(&enum_value));

	LLVMValueRef ref_to_element = llvm_emit_pointer_inbounds_gep_raw(c, actual_type_llvm, enum_value.value, index_value.value);
	BEValue result;
	if (ast->foreach_stmt.value_by_ref)
	{
		llvm_value_set(&result, ref_to_element, type_get_ptr(actual_type));
		LLVMTypeRef pointer_llvm = llvm_get_ptr_type(c, actual_type);
		if (pointer_llvm != var_type_llvm)
		{
			llvm_emit_cast(c, ast->foreach_stmt.cast, &result, var_type, result.type);
		}
	}
	else
	{
		llvm_value_set_address(&result, ref_to_element, actual_type);
		if (var_type_llvm != actual_type_llvm)
		{
			llvm_emit_cast(c, ast->foreach_stmt.cast, &result, var_type, actual_type);
		}
	}
	llvm_store_bevalue(c, &var, &result);

	llvm_emit_stmt(c, ast->foreach_stmt.body);

	llvm_emit_br(c, inc_block);

	llvm_emit_block(c, inc_block);
	index_value.value = LLVMBuildAdd(c->builder, llvm_value_rvalue_store(c, &index), llvm_const_int(c, type_isize, 1), "");
	llvm_store_bevalue(c, &index, &index_value);

	// Loop back.
	llvm_emit_br(c, cond_block);

	// And insert exit block
	llvm_emit_block(c, exit_block);
}

void gencontext_emit_while_stmt(GenContext *context, Ast *ast)
{
	// First, emit all inits.
	LLVMBasicBlockRef exit_block = llvm_basic_block_new(context, "while.exit");
	LLVMBasicBlockRef begin_block = llvm_basic_block_new(context, "while.begin");
	LLVMBasicBlockRef body_block = ast->while_stmt.body->compound_stmt.stmts ? llvm_basic_block_new(context,																										"while.body") : NULL;

	ast->while_stmt.continue_block = begin_block;
	ast->while_stmt.break_block = exit_block;

	Expr *cond = ast->while_stmt.cond;

	bool is_infinite_loop = false;

	// Is this while (false) or while (true)
	if (cond->expr_kind == EXPR_COND && vec_size(cond->cond_expr) == 1)
	{
		Expr *expr = cond->cond_expr[0];
		if (expr->expr_kind == EXPR_CONST && expr->const_expr.const_kind == CONST_BOOL)
		{
			is_infinite_loop = expr->const_expr.b;
			// This is a NOP
			if (!is_infinite_loop) return;
			assert(body_block);
		}
	}

	DeferList defers = { 0, 0 };

	// Emit cond
	llvm_emit_br(context, begin_block);

	if (is_infinite_loop)
	{
		body_block = begin_block;
		goto EMIT_BODY;
	}

	llvm_emit_block(context, begin_block);

	if (cond->expr_kind == EXPR_SCOPED_EXPR)
	{
		defers = cond->expr_scope.defers;
		cond = cond->expr_scope.expr;
	}
	BEValue be_value;
	llvm_emit_decl_expr_list(context, &be_value, cond, true);
	llvm_value_rvalue(context, &be_value);

	// If we have a body, conditionally jump to it.
	if (body_block)
	{
		llvm_emit_cond_br(context, &be_value, body_block, exit_block);
	}
	else
	{
		// Emit defers
		llvm_emit_defer(context, defers.start, defers.end);

		// Otherwise jump to inc or cond depending on what's available.
		llvm_emit_cond_br(context, &be_value, begin_block, exit_block);
	}

EMIT_BODY:
	if (body_block)
	{
		llvm_emit_block(context, body_block);
		llvm_emit_stmt(context, ast->while_stmt.body);

		// Emit defers
		llvm_emit_defer(context, defers.start, defers.end);
	}

	// Loop back.
	llvm_emit_br(context, begin_block);

	// And insert exit block
	llvm_emit_block(context, exit_block);

	// Emit defers
	llvm_emit_defer(context, defers.start, defers.end);
}

void gencontext_emit_do_stmt(GenContext *c, Ast *ast)
{
	LLVMBasicBlockRef exit_block = llvm_basic_block_new(c, "do.exit");
	LLVMBasicBlockRef cond_block = ast->do_stmt.expr ? llvm_basic_block_new(c, "do.cond") : NULL;
	LLVMBasicBlockRef body_block = llvm_basic_block_new(c, "do.body");

	// Break is simple it always jumps out.
	// For continue: if there is no condition, exit.
	LLVMBasicBlockRef cont_block = cond_block ? cond_block : exit_block;

	Expr *do_expr = ast->do_stmt.expr;
	Ast *do_body = ast->do_stmt.body;

	// Overwrite:
	ast->do_stmt.break_block = exit_block;
	ast->do_stmt.continue_block = cont_block;

	// Emit the body
	llvm_emit_br(c, body_block);
	llvm_emit_block(c, body_block);
	llvm_emit_stmt(c, do_body);

	if (cond_block)
	{
		llvm_emit_br(c, cond_block);
		llvm_emit_block(c, cond_block);
		BEValue be_value = { 0 };
		llvm_emit_expr(c, &be_value, do_expr);
		llvm_value_rvalue(c, &be_value);
		if (llvm_value_is_const(&be_value))
		{
			unsigned long v =  LLVMConstIntGetZExtValue(be_value.value);
			llvm_emit_br(c, v ? body_block : exit_block);
		}
		else
		{
			llvm_emit_cond_br(c, &be_value, body_block, exit_block);
		}
	}
	else
	{
		// Branch to the exit
		llvm_emit_br(c, exit_block);
	}

	// Emit the exit block.
	llvm_emit_block(c, exit_block);

}




static void llvm_emit_switch_body_if_chain(GenContext *c,
										   Ast **cases,
										   Ast *default_case,
										   BEValue *switch_value,
										   LLVMBasicBlockRef exit_block)
{
	LLVMBasicBlockRef next = NULL;
	VECEACH(cases, i)
	{
		Ast *case_stmt = cases[i];
		LLVMBasicBlockRef block = case_stmt->case_stmt.backend_block;
		if (case_stmt == default_case) continue;
		BEValue be_value;
		llvm_emit_expr(c, &be_value, case_stmt->case_stmt.expr);
		llvm_value_rvalue(c, &be_value);
		BEValue equals;
		llvm_emit_comparison(c, &equals, &be_value, switch_value, BINARYOP_EQ);
		next = llvm_basic_block_new(c, "next_if");
		llvm_emit_cond_br(c, &equals, block, next);
		if (case_stmt->case_stmt.body)
		{
			llvm_emit_block(c, block);
			c->current_block_is_target = true;
			llvm_emit_stmt(c, case_stmt->case_stmt.body);
			llvm_emit_br(c, exit_block);
		}
		llvm_emit_block(c, next);
	}
	if (default_case && default_case->case_stmt.body)
	{
		llvm_emit_br(c, default_case->case_stmt.backend_block);
		llvm_emit_block(c, default_case->case_stmt.backend_block);
		c->current_block_is_target = true;
		llvm_emit_stmt(c, default_case->case_stmt.body);
		llvm_emit_br(c, exit_block);
	}
	else
	{
		llvm_emit_br(c, exit_block);
	}
	llvm_emit_block(c, exit_block);
	return;
}

static void gencontext_emit_switch_body(GenContext *c, BEValue *switch_value, Ast *switch_ast)
{
	bool is_if_chain = switch_ast->switch_stmt.if_chain;
	Ast **cases = switch_ast->switch_stmt.cases;
	ArraySize case_count = vec_size(cases);
	if (!case_count)
	{
		// No body or default is empty, just exit after the value.
		return;
	}

	Ast *default_case = NULL;
	for (unsigned i = 0; i < case_count; i++)
	{
		Ast *case_stmt = cases[i];
		if (!case_stmt->case_stmt.expr)
		{
			if (case_stmt->case_stmt.body)
			{
				case_stmt->case_stmt.backend_block = llvm_basic_block_new(c, "switch.default");
			}
			default_case = case_stmt;
		}
		else if (case_stmt->case_stmt.body)
		{
			case_stmt->case_stmt.backend_block = llvm_basic_block_new(c, "switch.case");
		}
	}

	LLVMBasicBlockRef exit_block = llvm_basic_block_new(c, "switch.exit");
	LLVMBasicBlockRef switch_block = llvm_basic_block_new(c, "switch.entry");
	switch_ast->switch_stmt.codegen.retry_block = switch_block;
	switch_ast->switch_stmt.codegen.exit_block = exit_block;

	// We will now treat the fallthrough cases:
	// switch (i)
	// {
	//    case 1:
	//    case 2:
	//      do_something();
	//    default:
	// }
	LLVMBasicBlockRef next_block = exit_block;
	for (unsigned i = case_count; i > 0; i--)
	{
		Ast *case_stmt = cases[i - 1];
		if (case_stmt->case_stmt.backend_block)
		{
			next_block = case_stmt->case_stmt.backend_block;
			continue;
		}
		case_stmt->case_stmt.backend_block = next_block;
	}


	Type *switch_type = switch_ast->ast_kind == AST_IF_CATCH_SWITCH_STMT ? type_lowering(type_anyerr) : switch_ast->switch_stmt.cond->type;
	BEValue switch_var;
	llvm_value_set_address(&switch_var, llvm_emit_alloca_aligned(c, switch_type, "switch"), switch_type);
	switch_ast->switch_stmt.codegen.retry_var = &switch_var;
	llvm_store_bevalue(c, &switch_var, switch_value);

	llvm_emit_br(c, switch_block);
	llvm_emit_block(c, switch_block);

	BEValue switch_current_val = switch_var;
	llvm_value_rvalue(c, &switch_current_val);

	if (is_if_chain)
	{
		llvm_emit_switch_body_if_chain(c, cases, default_case, &switch_current_val, exit_block);
		return;
	}

	c->current_block = NULL;
	LLVMValueRef switch_stmt = LLVMBuildSwitch(c->builder, switch_current_val.value, default_case ? default_case->case_stmt.backend_block : exit_block, case_count);
	for (unsigned i = 0; i < case_count; i++)
	{
		Ast *case_stmt = cases[i];
		LLVMBasicBlockRef block = case_stmt->case_stmt.backend_block;
		if (case_stmt != default_case)
		{
			LLVMValueRef case_value;
			BEValue be_value;
			llvm_emit_expr(c, &be_value, case_stmt->case_stmt.expr);
			llvm_value_rvalue(c, &be_value);
			case_value = be_value.value;
			LLVMAddCase(switch_stmt, case_value, block);
		}

		// Skip fallthroughs.
		if (!case_stmt->case_stmt.body) continue;

		llvm_emit_block(c, block);
		// IMPORTANT!
		c->current_block_is_target = true;

		llvm_emit_stmt(c, case_stmt->case_stmt.body);
		llvm_emit_br(c, exit_block);
	}
	llvm_emit_block(c, exit_block);
}

void gencontext_emit_switch(GenContext *context, Ast *ast)
{
	BEValue switch_value;
	llvm_emit_decl_expr_list(context, &switch_value, ast->switch_stmt.cond, false);
	gencontext_emit_switch_body(context, &switch_value, ast);
}

void llvm_emit_defer(GenContext *c, AstId defer_start, AstId defer_end)
{
	if (defer_start == defer_end) return;
	AstId defer = defer_start;
	while (defer && defer != defer_end)
	{
		Ast *def = astptr(defer);
		LLVMBasicBlockRef exit = llvm_basic_block_new(c, "exit");
		Ast *body = def->defer_stmt.body;
		def->defer_stmt.codegen.exit_block = exit;
		llvm_emit_stmt(c, body);
		llvm_emit_br(c, exit);
		llvm_emit_block(c, exit);
		defer = def->defer_stmt.prev_defer;
	}
}


void gencontext_emit_break(GenContext *context, Ast *ast)
{
	llvm_emit_defer(context, ast->contbreak_stmt.defers.start, ast->contbreak_stmt.defers.end);
	Ast *jump_target = astptr(ast->contbreak_stmt.ast);
	LLVMBasicBlockRef jump;
	switch (jump_target->ast_kind)
	{
		case AST_IF_STMT:
			jump = jump_target->if_stmt.break_block;
			break;
		case AST_WHILE_STMT:
			jump = jump_target->while_stmt.break_block;
			break;
		case AST_FOREACH_STMT:
			jump = jump_target->foreach_stmt.exit_block;
			break;
		case AST_FOR_STMT:
			jump = jump_target->for_stmt.exit_block;
			break;
		case AST_DO_STMT:
			jump = jump_target->do_stmt.break_block;
			break;
		case AST_IF_CATCH_SWITCH_STMT:
		case AST_SWITCH_STMT:
			jump = jump_target->switch_stmt.codegen.exit_block;
			break;
		case AST_DEFER_STMT:
			jump = jump_target->defer_stmt.codegen.exit_block;
			break;
		default:
			UNREACHABLE
	}
	llvm_emit_jmp(context, jump);
}

void gencontext_emit_continue(GenContext *context, Ast *ast)
{
	llvm_emit_defer(context, ast->contbreak_stmt.defers.start, ast->contbreak_stmt.defers.end);
	Ast *jump_target = astptr(ast->contbreak_stmt.ast);
	LLVMBasicBlockRef jump;
	switch (jump_target->ast_kind)
	{
		case AST_IF_STMT:
		case AST_SWITCH_STMT:
			UNREACHABLE
		case AST_WHILE_STMT:
			jump = jump_target->while_stmt.continue_block;
			break;
		case AST_DO_STMT:
			jump = jump_target->do_stmt.continue_block;
			break;
		case AST_FOREACH_STMT:
			jump = jump_target->foreach_stmt.continue_block;
			break;
		case AST_FOR_STMT:
			jump = jump_target->for_stmt.continue_block;
			break;
		default:
			UNREACHABLE
	}
	llvm_emit_jmp(context, jump);
}

void gencontext_emit_next_stmt(GenContext *context, Ast *ast)
{
	Ast *jump_target = astptr(ast->next_stmt.case_switch_stmt);
	if (jump_target->ast_kind != AST_SWITCH_STMT)
	{
		llvm_emit_defer(context, ast->next_stmt.defers.start, ast->next_stmt.defers.end);
		llvm_emit_jmp(context, jump_target->case_stmt.backend_block);
		return;
	}
	BEValue be_value;
	llvm_emit_expr(context, &be_value, ast->next_stmt.switch_expr);
	llvm_store_bevalue(context, jump_target->switch_stmt.codegen.retry_var, &be_value);
	llvm_emit_defer(context, ast->next_stmt.defers.start, ast->next_stmt.defers.end);
	llvm_emit_jmp(context, jump_target->switch_stmt.codegen.retry_block);
}

void gencontext_emit_scoped_stmt(GenContext *context, Ast *ast)
{
	llvm_emit_stmt(context, ast->scoped_stmt.stmt);
	llvm_emit_defer(context, ast->scoped_stmt.defers.start, ast->scoped_stmt.defers.end);
}


static inline void llvm_emit_assume(GenContext *c, Expr *expr)
{
	// 1. Convert x > 0 && y > 2 => llvm.assume(x > 0) + llvm.assume(y > 2)
	if (expr->expr_kind == EXPR_BINARY && expr->binary_expr.operator == BINARYOP_AND)
	{
		llvm_emit_assume(c, expr->binary_expr.left);
		llvm_emit_assume(c, expr->binary_expr.right);
		return;
	}

	// 2. Convert !(x > 0 || y > 2) => llvm.assume(!(x > 0)) + llvm.assume(!(y > 2))
	if (expr->expr_kind == EXPR_UNARY && expr->unary_expr.operator == UNARYOP_NOT)
	{
		Expr *inner = expr->unary_expr.expr;
		if (inner->expr_kind == EXPR_BINARY && inner->binary_expr.operator == BINARYOP_OR)
		{
			Expr *left = inner->binary_expr.left;
			Expr *right = inner->binary_expr.right;

			expr->unary_expr.expr = left;
			llvm_emit_assume(c, expr);


			expr->unary_expr.expr = right;
			llvm_emit_assume(c, expr);

			return;
		}
	}

	// 3. Check if pure, if so we emit the assume.
	if (expr_is_pure(expr))
	{
		BEValue value;
		llvm_emit_expr(c, &value, expr);
		llvm_value_rvalue(c, &value);
		assert(value.kind == BE_BOOLEAN);
		EMIT_LOC(c, expr);
		llvm_emit_call_intrinsic(c, intrinsic_id_assume, NULL, 0, &(value.value), 1);
	}
}



static inline void llvm_emit_assert_stmt(GenContext *c, Ast *ast)
{
	if (active_target.feature.safe_mode)
	{
		BEValue value;
		llvm_emit_expr(c, &value, ast->assert_stmt.expr);
		llvm_value_rvalue(c, &value);
		LLVMBasicBlockRef on_fail = llvm_basic_block_new(c, "assert_fail");
		LLVMBasicBlockRef on_ok = llvm_basic_block_new(c, "assert_ok");
		assert(value.kind == BE_BOOLEAN);
		llvm_emit_cond_br(c, &value, on_ok, on_fail);
		llvm_emit_block(c, on_fail);
		SourceLocation *loc = TOKLOC(ast->assert_stmt.expr->span.loc);
		const char *error;
		if (ast->assert_stmt.message)
		{
			error = ast->assert_stmt.message->const_expr.string.chars;
		}
		else
		{
			error = "Assert violation";
		}
		llvm_emit_debug_output(c, error, loc->file->name, c->cur_func_decl->name, loc->line);
		llvm_emit_call_intrinsic(c, intrinsic_id_trap, NULL, 0, NULL, 0);
		llvm_emit_br(c, on_ok);
		llvm_emit_block(c, on_ok);
		return;
	}
	llvm_emit_assume(c, ast->assert_stmt.expr);
}

static inline void add_target_clobbers_to_buffer(GenContext *c)
{
	switch (platform_target.arch)
	{
		case ARCH_TYPE_X86_64:
		case ARCH_TYPE_X86:
			scratch_buffer_append("~{dirflag},~{fpsr},~{flags}");
			break;
		case ARCH_TYPE_MIPS:
		case ARCH_TYPE_MIPS64:
		case ARCH_TYPE_MIPS64EL:
		case ARCH_TYPE_MIPSEL:
			// Currently Clang does this
			scratch_buffer_append("~{$1}");
			break;
		default:
			// In Clang no other platform has automatic clobbers
			break;
	}
}
static inline void llvm_emit_asm_stmt(GenContext *c, Ast *ast)
{
	LLVMTypeRef asm_fn_type = LLVMFunctionType(llvm_get_type(c, type_void), NULL, 0, 0);
	scratch_buffer_clear();
	add_target_clobbers_to_buffer(c);
	LLVMValueRef asm_fn = LLVMGetInlineAsm(asm_fn_type,
	                                       (char *)ast->asm_stmt.body->const_expr.string.chars,
	                                       ast->asm_stmt.body->const_expr.string.len,
	                                       scratch_buffer_to_string(), global_context.scratch_buffer_len,
	                                       ast->asm_stmt.is_volatile,
	                                       true,
	                                       LLVMInlineAsmDialectIntel
#if LLVM_VERSION_MAJOR > 12
											, /* can throw */ false
#endif
	                                       );
	LLVMBuildCall2(c->builder, asm_fn_type, asm_fn, NULL, 0, "");
}

static inline void gencontext_emit_unreachable_stmt(GenContext *context, Ast *ast)
{
	SourceLocation *loc = TOKLOC(ast->span.loc);
	llvm_emit_debug_output(context, "Unreachable statement reached.", loc->file->name, context->cur_func_decl->external_name, loc->line);
	llvm_emit_call_intrinsic(context, intrinsic_id_trap, NULL, 0, NULL, 0);
	LLVMBuildUnreachable(context->builder);
	LLVMBasicBlockRef block = llvm_basic_block_new(context, "unreachable_block");
	context->current_block = NULL;
	context->current_block_is_target = false;
	llvm_emit_block(context, block);
}

void gencontext_emit_expr_stmt(GenContext *c, Ast *ast)
{
	BEValue value;
	if (IS_FAILABLE(ast->expr_stmt))
	{
		PUSH_ERROR();
		LLVMBasicBlockRef discard_fail = llvm_basic_block_new(c, "voiderr");
		c->catch_block = discard_fail;
		c->error_var = NULL;
		llvm_emit_expr(c, &value, ast->expr_stmt);
		llvm_value_rvalue(c, &value);
		EMIT_LOC(c, ast);
		llvm_emit_br(c, discard_fail);
		llvm_emit_block(c, discard_fail);
		POP_ERROR();
		return;
	}
	llvm_emit_expr(c, &value, ast->expr_stmt);
	llvm_value_rvalue(c, &value);
}

static LLVMValueRef llvm_emit_string(GenContext *c, const char *str)
{
	LLVMTypeRef char_type = llvm_get_type(c, type_char);
	unsigned len = (unsigned)strlen(str);
	LLVMTypeRef char_array_type = LLVMArrayType(char_type, len + 1);
	LLVMValueRef global_string = LLVMAddGlobal(c->module, char_array_type, "");
	LLVMSetLinkage(global_string, LLVMInternalLinkage);
	LLVMSetGlobalConstant(global_string, 1);
	LLVMSetInitializer(global_string, LLVMConstStringInContext(c->context, str, len, 0));
	AlignSize alignment;
	// TODO alignment
	LLVMValueRef string = llvm_emit_array_gep_raw(c, global_string, char_array_type, 0,
	                                              1, &alignment);
	return LLVMBuildBitCast(c->builder, string, LLVMPointerType(char_type, 0), "");
}
void llvm_emit_debug_output(GenContext *c, const char *message, const char *file, const char *func, unsigned line)
{
	LLVMTypeRef char_ptr_type = llvm_get_ptr_type(c, type_char);
	LLVMTypeRef cint_type = llvm_get_type(c, type_cint());
	const char *name;
	int file_index;
	int line_index;
	int expr_index;
	int func_index = -1;
	switch (platform_target.os)
	{
		case OS_TYPE_WIN32:
			name = "_assert";
			expr_index = 0;
			file_index = 1;
			line_index = 2;
			break;
		case OS_DARWIN_TYPES:
			name = "__assert_rtn";
			func_index = 0;
			expr_index = 3;
			file_index = 1;
			line_index = 2;
			break;
		case OS_TYPE_SOLARIS:
			name = "__assert_c99";
			expr_index = 0;
			file_index = 1;
			line_index = 2;
			func_index = 3;
			break;
		case OS_TYPE_LINUX:
			name = "__assert_fail";
			expr_index = 0;
			file_index = 1;
			line_index = 2;
			func_index = 3;
			break;
		case OS_TYPE_OPENBSD:
			name = "__assert2";
			file_index = 0;
			line_index = 1;
			func_index = 2;
			expr_index = 3;
			break;
		default:
			name = "__assert";
			expr_index = 0;
			file_index = 1;
			line_index = 2;
			func_index = 3;
			break;
	}
	LLVMValueRef assert_func = LLVMGetNamedFunction(c->module, name);
	if (!assert_func)
	{
		LLVMTypeRef type;
		LLVMTypeRef void_type = LLVMVoidTypeInContext(c->context);
		switch (platform_target.os)
		{
			case OS_TYPE_WIN32:
			case OS_TYPE_FREE_BSD:
			case OS_TYPE_DRAGON_FLY:
			{
				LLVMTypeRef args[3] = { char_ptr_type, char_ptr_type, cint_type };
				type = LLVMFunctionType(void_type, args, 3, false);
				break;
			}
			case OS_DARWIN_TYPES:
			case OS_TYPE_LINUX:
			case OS_TYPE_SOLARIS:
			{
				LLVMTypeRef args[4] = { char_ptr_type, char_ptr_type, cint_type, char_ptr_type };
				type = LLVMFunctionType(void_type, args, 4, false);
				break;
			}
			case OS_TYPE_OPENBSD:
			{
				LLVMTypeRef args[4] = { char_ptr_type, cint_type, char_ptr_type, char_ptr_type };
				type = LLVMFunctionType(void_type, args, 4, false);
				break;
			}
			case OS_TYPE_NETBSD:
			{
				LLVMTypeRef args[3] = { char_ptr_type, cint_type, char_ptr_type };
				type = LLVMFunctionType(void_type, args, 3, false);
				break;
			}
			default:
			{
				LLVMTypeRef args[3] = { char_ptr_type, char_ptr_type, cint_type };
				type = LLVMFunctionType(void_type, args, 3, false);
				break;
			}
		}
		assert_func = LLVMAddFunction(c->module, name, type);
	}

	LLVMValueRef args[4];

	if (func_index == -1)
	{
		scratch_buffer_clear();
		scratch_buffer_append(file);
		scratch_buffer_append(" : ");
		scratch_buffer_append(func);
		file = scratch_buffer_to_string();
	}
	else
	{
		args[func_index] = llvm_emit_string(c, func);
	}
	args[file_index] = llvm_emit_string(c, file);
	args[expr_index] = llvm_emit_string(c, message);
	args[line_index] = llvm_const_int(c, type_cint(), line);

	LLVMBuildCall(c->builder, assert_func, args, func_index > -1 ? 4 : 3, "");

}

void llvm_emit_panic_if_true(GenContext *c, BEValue *value, const char *panic_name, SourceLocation *loc)
{
	LLVMBasicBlockRef panic_block = llvm_basic_block_new(c, "panic");
	LLVMBasicBlockRef ok_block = llvm_basic_block_new(c, "checkok");
	assert(llvm_value_is_bool(value));
	llvm_emit_cond_br(c, value, panic_block, ok_block);
	llvm_emit_block(c, panic_block);
	llvm_emit_debug_output(c, panic_name, loc->file->name, c->cur_func_decl->name, loc->line);
	llvm_emit_call_intrinsic(c, intrinsic_id_trap, NULL, 0, NULL, 0);
	llvm_emit_br(c, ok_block);
	llvm_emit_block(c, ok_block);
}

void llvm_emit_panic_on_true(GenContext *c, LLVMValueRef value, const char *panic_name, SourceLocation *loc)
{
	LLVMBasicBlockRef panic_block = llvm_basic_block_new(c, "panic");
	LLVMBasicBlockRef ok_block = llvm_basic_block_new(c, "checkok");
	BEValue be_value;
	llvm_value_set_bool(&be_value, value);
	llvm_emit_cond_br(c, &be_value, panic_block, ok_block);
	llvm_emit_block(c, panic_block);
	llvm_emit_debug_output(c, panic_name, loc->file->name, c->cur_func_decl->name, loc->line);
	llvm_emit_call_intrinsic(c, intrinsic_id_trap, NULL, 0, NULL, 0);
	llvm_emit_br(c, ok_block);
	llvm_emit_block(c, ok_block);
}


void llvm_emit_stmt(GenContext *c, Ast *ast)
{
	EMIT_LOC(c, ast);
	assert(!c->catch_block && "Did not expect a catch block here.");
	switch (ast->ast_kind)
	{
		case AST_DOCS:
		case AST_DOC_DIRECTIVE:
		case AST_POISONED:
		case AST_VAR_STMT:
		case AST_IF_CATCH_SWITCH_STMT:
		case AST_SCOPING_STMT:
			UNREACHABLE
		case AST_SCOPED_STMT:
			gencontext_emit_scoped_stmt(c, ast);
			break;
		case AST_EXPR_STMT:
			gencontext_emit_expr_stmt(c, ast);
			break;
		case AST_DECLARE_STMT:
			llvm_emit_local_decl(c, ast->declare_stmt);
			break;
		case AST_BREAK_STMT:
			gencontext_emit_break(c, ast);
			break;
		case AST_CONTINUE_STMT:
			gencontext_emit_continue(c, ast);
			break;
		case AST_IF_STMT:
			llvm_emit_if(c, ast);
			break;
		case AST_RETURN_STMT:
			gencontext_emit_return(c, ast);
			break;
		case AST_COMPOUND_STMT:
			llvm_emit_compound_stmt(c, ast);
			break;
		case AST_CT_COMPOUND_STMT:
			gencontext_emit_ct_compound_stmt(c, ast);
			break;
		case AST_FOR_STMT:
			gencontext_emit_for_stmt(c, ast);
			break;
		case AST_FOREACH_STMT:
			llvm_emit_foreach_stmt(c, ast);
			break;
		case AST_WHILE_STMT:
			gencontext_emit_while_stmt(c, ast);
			break;
		case AST_DO_STMT:
			gencontext_emit_do_stmt(c, ast);
			break;
		case AST_NEXT_STMT:
			gencontext_emit_next_stmt(c, ast);
			break;
		case AST_DEFER_STMT:
		case AST_NOP_STMT:
			break;
		case AST_ASM_STMT:
			llvm_emit_asm_stmt(c, ast);
			break;
		case AST_ASSERT_STMT:
			llvm_emit_assert_stmt(c, ast);
			break;;
		case AST_CT_ASSERT:
		case AST_CT_IF_STMT:
		case AST_CT_ELIF_STMT:
		case AST_CT_ELSE_STMT:
		case AST_CT_FOR_STMT:
		case AST_CT_SWITCH_STMT:
		case AST_CASE_STMT:
		case AST_DEFAULT_STMT:
			UNREACHABLE
		case AST_SWITCH_STMT:
			gencontext_emit_switch(c, ast);
			break;
		case AST_UNREACHABLE_STMT:
			gencontext_emit_unreachable_stmt(c, ast);
			break;
	}
}

