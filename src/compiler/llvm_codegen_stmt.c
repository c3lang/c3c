// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_internal.h"

static void gencontext_emit_switch_body(GenContext *c, BEValue *switch_value, Ast *switch_ast);

static bool ast_is_not_empty(Ast *ast)
{
	if (!ast) return false;
	if (ast->ast_kind != AST_COMPOUND_STMT) return true;
	uint32_t stmts = vec_size(ast->compound_stmt.stmts);
	if (stmts > 0)
	{
		if (stmts > 1) return true;
		if (ast->compound_stmt.defer_list.start != ast->compound_stmt.defer_list.end) return true;
		Ast *first = ast->compound_stmt.stmts[0];
		return ast_is_not_empty(first);
	}
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

typedef enum
{
	LOOP_NORMAL,
	LOOP_INFINITE,
	LOOP_NONE
}  LoopType;

static inline LoopType loop_type_for_cond(Expr *cond, bool skip_first)
{
	if (!cond)
	{
		// We may have do-while (0)
		if (skip_first) return LOOP_NONE;

		// OR we have for (int x;;x++)
		return LOOP_INFINITE;
	}

	// Fold simple conds
	if (cond->expr_kind == EXPR_COND && vec_size(cond->cond_expr) == 1)
	{
		cond = cond->cond_expr[0];
	}

	// Do we have a constant cond?
	if (cond->expr_kind == EXPR_CONST)
	{
		assert(cond->const_expr.const_kind == CONST_BOOL);
		// The result is either infinite or no loop
		return cond->const_expr.b ? LOOP_INFINITE : LOOP_NONE;
	}

	// Otherwise we have a normal loop.
	return LOOP_NORMAL;
}

void llvm_emit_for_stmt(GenContext *c, Ast *ast)
{
	// First, emit all inits.
	BEValue value;
	if (ast->for_stmt.init) llvm_emit_expr(c, &value, ast->for_stmt.init);

	bool no_exit = ast->for_stmt.flow.no_exit;
	Expr *incr = ast->for_stmt.incr;

	LLVMBasicBlockRef inc_block = incr ? llvm_basic_block_new(c, "loop.inc") : NULL;
	LLVMBasicBlockRef body_block = ast_is_not_empty(ast->for_stmt.body) ? llvm_basic_block_new(c, "loop.body") : NULL;
	LLVMBasicBlockRef cond_block = NULL;

	// Skipping first cond? This is do-while semantics
	bool skip_first = ast->for_stmt.flow.skip_first;

	Expr *cond = ast->for_stmt.cond;
	LoopType loop = loop_type_for_cond(cond, skip_first);

	// This is the starting block to loop back to, and may either be cond, body or inc
	LLVMBasicBlockRef loop_start_block = body_block ? body_block : inc_block;

	// We only emit a cond block if we have a normal loop.
	if (loop == LOOP_NORMAL)
	{
		cond_block = llvm_basic_block_new(c, "loop.cond");
		loop_start_block = cond_block;
	}

	// In the case that *none* of the blocks exist.
	if (!inc_block && !body_block && !cond_block)
	{
		if (loop == LOOP_INFINITE)
		{
			SourceLocation *loc = TOKLOC(ast->span.loc);
			File  *file = source_file_by_id(loc->file_id);
			llvm_emit_debug_output(c, "Infinite loop found", file->name, c->cur_func_decl->external_name, loc->row);
			LLVMBuildUnreachable(c->builder);
			LLVMBasicBlockRef block = llvm_basic_block_new(c, "unreachable_block");
			c->current_block = NULL;
			c->current_block_is_target = false;
			llvm_emit_block(c, block);
			return;
		}
		return;
	}

	assert(loop_start_block != NULL);

	LLVMBasicBlockRef exit_block = llvm_basic_block_new(c, "loop.exit");

	// Break is simple it always jumps out.
	// For continue:
	// 1. If there is inc, jump to the condition
	// 2. If this is not looping, jump to the exit, otherwise go to cond/body depending on what the start is.
	LLVMBasicBlockRef continue_block = inc_block;
	if (!continue_block)
	{
		continue_block = loop == LOOP_NONE ? exit_block : loop_start_block;
	}

	ast->for_stmt.continue_block = continue_block;
	ast->for_stmt.exit_block = exit_block;

	// We have a normal loop, so we emit a cond.
	if (loop == LOOP_NORMAL)
	{
		// Emit a jump for do-while semantics, to skip the initial cond.
		if (skip_first)
		{
			LLVMBasicBlockRef do_while_start = body_block ? body_block : inc_block;
			// Only jump if we have a body / inc
			// if the case is do {} while (...) then we basically can treat this as while (...) {}
			llvm_emit_br(c, do_while_start ? do_while_start : cond_block);
		}
		else
		{
			llvm_emit_br(c, cond_block);
		}

		// Emit the block
		llvm_emit_block(c, cond_block);
		BEValue be_value;
		if (cond->expr_kind == EXPR_COND)
		{
			llvm_emit_decl_expr_list(c, &be_value, ast->for_stmt.cond, true);
		}
		else
		{
			llvm_emit_expr(c, &be_value, cond);
		}
		llvm_value_rvalue(c, &be_value);
		assert(llvm_value_is_bool(&be_value));

		// If we have a body, conditionally jump to it.
		LLVMBasicBlockRef cond_success = body_block ? body_block : inc_block;
		// If there is a while (...) { } we need to set the success to this block
		if (!cond_success) cond_success = cond_block;
		// Otherwise jump to inc or cond depending on what's available.
		llvm_emit_cond_br(c, &be_value, cond_success, exit_block);
	}

	// The optional cond is emitted, so emit the body
	if (body_block)
	{
		// If we have LOOP_NONE, then we don't need a new block here
		// since we will just exit. That leaves the infinite loop.
		switch (loop)
		{
			case LOOP_NORMAL:
				// If we have LOOP_NORMAL, we already emitted a br to the body.
				// so emit the block
				llvm_emit_block(c, body_block);
				break;
				case LOOP_INFINITE:
					// In this case we have no cond, so we need to emit the br and
					// then the block
					llvm_emit_br(c, body_block);
					llvm_emit_block(c, body_block);
					case LOOP_NONE:
						// If there is no loop, then we will just fall through and the
						// block is needed.
						body_block = NULL;
						break;
		}
		// Now emit the body
		llvm_emit_stmt(c, ast->for_stmt.body);

		// Did we have a jump to inc yet?
		if (inc_block && !llvm_basic_block_is_unused(inc_block))
		{
			// If so we emit the jump to the inc block.
			llvm_emit_br(c, inc_block);
		}
		else
		{
			inc_block = NULL;
		}
	}

	if (incr)
	{
		// We might have neither body nor cond
		// In that case we do a jump from the init.
		if (loop_start_block == inc_block)
		{
			llvm_emit_br(c, inc_block);
		}
		if (inc_block)
		{
			// Emit the block if it exists.
			// The inc block might also be the end of the body block.
			llvm_emit_block(c, inc_block);
		}
		BEValue dummy;
		llvm_emit_expr(c, &dummy, ast->for_stmt.incr);
	}

	// Loop back.
	if (loop != LOOP_NONE)
	{
		llvm_emit_br(c, loop_start_block);
	}
	else
	{
		// If the exit block is unused, just skip it.
		if (llvm_basic_block_is_unused(exit_block)) return;
		llvm_emit_br(c, exit_block);
	}

	// And insert exit block
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
		Expr *to_expr = case_stmt->case_stmt.to_expr;
		if (to_expr)
		{
			BEValue to_value;
			llvm_emit_expr(c, &to_value, to_expr);
			llvm_value_rvalue(c, &to_value);
			BEValue le;
			llvm_emit_comparison(c, &le, &be_value, switch_value, BINARYOP_LE);
			BEValue ge;
			llvm_emit_comparison(c, &ge, &to_value, switch_value, BINARYOP_GE);
			llvm_value_set_bool(&equals, LLVMBuildAnd(c->builder, le.value, ge.value, ""));
		}
		else
		{
			llvm_emit_comparison(c, &equals, &be_value, switch_value, BINARYOP_EQ);
		}
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

static void llvm_emit_switch_jump_table(GenContext *c,
                                        Ast **cases,
                                        Ast *default_case,
                                        BEValue *switch_value,
                                        LLVMBasicBlockRef exit_block)
{
#ifdef jump_table_done
	c->current_block = NULL;
	unsigned case_count = vec_size(cases);
	Int min = { .type = TYPE_VOID };
	Int max = { .type = TYPE_VOID };
	for (unsigned i = 0; i < case_count; i++)
	{
		Ast *case_ast = cases[i];
		Expr *from = case_ast->case_stmt.expr;
		Expr *to = case_ast->case_stmt.to_expr;
		assert(type_is_integer(from->type) && from->expr_kind == EXPR_CONST);
		Int value = from->const_expr.ixx;
		Int to_value = to ? to->const_expr.ixx : value;
		if (min.type == TYPE_VOID)
		{
			min = value;
			max = to_value;
		}
		else if (int_comp(value, min, BINARYOP_LT))
		{
			min = value;
		}
		else if (int_comp(to_value, max, BINARYOP_GT))
		{
			max = to_value;
		}
	}
	switch_value->value = LLVMBuildSub(c->builder, switch_value->value, llvm_const_int(c, switch_value->type, min.i.low), "");
	max = int_sub(max, min);
	Type *create_array = type_get_array(type_voidptr, max.i.low);
	LLVMTypeRef llvm_array_type = llvm_get_type(c, create_array);
	AlignSize alignment = type_alloca_alignment(switch_value->type);
	LLVMValueRef array_ref = llvm_emit_alloca(c, llvm_array_type, alignment, "");
	BEValue array_value;
	llvm_value_set_address(&array_value, array_ref, create_array);
	for (int i = 0; i < max.i.low; i++)
	{
		AlignSize align;
		LLVMValueRef ptr = llvm_emit_array_gep_raw(c, array_ref, llvm_array_type, i, alignment, &align);

	}
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
 #endif
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
			Expr *from = case_stmt->case_stmt.expr;
			assert(from->expr_kind == EXPR_CONST);
			llvm_emit_expr(c, &be_value, case_stmt->case_stmt.expr);
			llvm_value_rvalue(c, &be_value);
			case_value = be_value.value;
			LLVMAddCase(switch_stmt, case_value, block);
			Expr *to = case_stmt->case_stmt.to_expr;
			if (to)
			{
				BEValue to_value;
				llvm_emit_expr(c, &to_value, case_stmt->case_stmt.to_expr);
				assert(LLVMIsAConstant(to_value.value));
				LLVMValueRef one = llvm_const_int(c, to_value.type, 1);
				while (LLVMConstIntGetZExtValue(LLVMConstICmp(LLVMIntEQ, to_value.value, case_value)) != 1)
				{
					case_value = LLVMConstAdd(case_value, one);
					LLVMAddCase(switch_stmt, case_value, block);
				}
			}
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
		case AST_FOREACH_STMT:
			jump = jump_target->foreach_stmt.exit_block;
			break;
		case AST_FOR_STMT:
			jump = jump_target->for_stmt.exit_block;
			break;
		case AST_DO_STMT:
		case AST_WHILE_STMT:
			UNREACHABLE
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
		case AST_WHILE_STMT:
		case AST_DO_STMT:
			UNREACHABLE
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
		llvm_emit_call_intrinsic(c, intrinsic_id.assume, NULL, 0, &(value.value), 1);
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
		File  *file = source_file_by_id(loc->file_id);
		llvm_emit_debug_output(c, error, file->name, c->cur_func_decl->name, loc->row);
		llvm_emit_call_intrinsic(c, intrinsic_id.trap, NULL, 0, NULL, 0);
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
	File  *file = source_file_by_id(loc->file_id);
	llvm_emit_debug_output(context, "Unreachable statement reached.", file->name, context->cur_func_decl->external_name, loc->row);
	llvm_emit_call_intrinsic(context, intrinsic_id.trap, NULL, 0, NULL, 0);
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
	llvm_set_internal_linkage(global_string);
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
	OsType os = platform_target.os;
	if (platform_target.arch == ARCH_TYPE_WASM32 || platform_target.arch == ARCH_TYPE_WASM64) os = OS_TYPE_WASI;
	switch (os)
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
		case OS_TYPE_WASI:
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
			func_index = -1;
			break;
	}

	LLVMTypeRef type;
	LLVMTypeRef void_type = LLVMVoidTypeInContext(c->context);
	switch (os)
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
		case OS_TYPE_WASI:
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
	LLVMValueRef assert_func = LLVMGetNamedFunction(c->module, name);
	if (!assert_func)
	{
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

	LLVMBuildCall2(c->builder, type, assert_func, args, func_index > -1 ? 4 : 3, "");

}

void llvm_emit_panic_if_true(GenContext *c, BEValue *value, const char *panic_name, SourceLocation *loc)
{
	LLVMBasicBlockRef panic_block = llvm_basic_block_new(c, "panic");
	LLVMBasicBlockRef ok_block = llvm_basic_block_new(c, "checkok");
	assert(llvm_value_is_bool(value));
	llvm_emit_cond_br(c, value, panic_block, ok_block);
	llvm_emit_block(c, panic_block);
	File  *file = source_file_by_id(loc->file_id);
	llvm_emit_debug_output(c, panic_name, file->name, c->cur_func_decl->name, loc->row);
	llvm_emit_call_intrinsic(c, intrinsic_id.trap, NULL, 0, NULL, 0);
	llvm_emit_br(c, ok_block);
	llvm_emit_block(c, ok_block);
}

void llvm_emit_panic_on_true(GenContext *c, LLVMValueRef value, const char *panic_name, SourceLocation *loc)
{
	File  *file = source_file_by_id(loc->file_id);
	LLVMBasicBlockRef panic_block = llvm_basic_block_new(c, "panic");
	LLVMBasicBlockRef ok_block = llvm_basic_block_new(c, "checkok");
	BEValue be_value;
	llvm_value_set_bool(&be_value, value);
	llvm_emit_cond_br(c, &be_value, panic_block, ok_block);
	llvm_emit_block(c, panic_block);
	llvm_emit_debug_output(c, panic_name, file->name, c->cur_func_decl->name, loc->row);
	llvm_emit_call_intrinsic(c, intrinsic_id.trap, NULL, 0, NULL, 0);
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
		case AST_FOREACH_STMT:
		case AST_WHILE_STMT:
		case AST_DO_STMT:
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
			llvm_emit_for_stmt(c, ast);
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

