// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_internal.h"

static void gencontext_emit_switch_body(GenContext *c, BEValue *switch_value, Ast *switch_ast);


void llvm_emit_compound_stmt(GenContext *c, Ast *ast)
{
	if (llvm_use_debug(c))
	{
		llvm_debug_push_lexical_scope(c, ast->span);
	}
	assert(ast->ast_kind == AST_COMPOUND_STMT);
	llvm_emit_statement_chain(c, ast->compound_stmt.first_stmt);
	if (llvm_use_debug(c))
	{
		llvm_debug_scope_pop(c);
	}
}

/**
 * This emits a local declaration.
 */
void llvm_emit_local_decl(GenContext *c, Decl *decl, BEValue *value)
{
	// 1. Get the declaration and the LLVM type.
	Type *var_type = type_lowering(type_no_fail(decl->type));
	LLVMTypeRef alloc_type = llvm_get_type(c, var_type);

	// 2. In the case we have a static variable,
	//    then we essentially treat this as a global.
	if (decl->var.is_static)
	{
		// In defers we might already have generated this variable.
		if (decl->backend_ref)
		{
			llvm_value_set_decl(c, value, decl);
			return;
		}
		void *builder = c->builder;
		c->builder = NULL;
		decl->backend_ref = llvm_add_global_var(c, "tempglobal", var_type, decl->alignment);
		if (IS_FAILABLE(decl))
		{
			scratch_buffer_clear();
			scratch_buffer_append(decl->extname);
			scratch_buffer_append(".f");
			decl->var.failable_ref = llvm_add_global_var(c, scratch_buffer_to_string(), type_anyerr, 0);
		}
		llvm_emit_global_variable_init(c, decl);
		c->builder = builder;
		llvm_value_set_decl(c, value, decl);
		return;
	}
	assert(!decl->backend_ref);
	llvm_emit_local_var_alloca(c, decl);
	Expr *init = decl->var.init_expr;
	bool is_failable = IS_FAILABLE(decl);
	if (is_failable)
	{
		scratch_buffer_clear();
		scratch_buffer_append(decl->name);
		scratch_buffer_append(".f");
		decl->var.failable_ref = llvm_emit_alloca_aligned(c, type_anyerr, scratch_buffer_to_string());
		// Only clear out the result if the assignment isn't a failable.
	}

	if (init)
	{
		llvm_value_set_decl_address(c, value, decl);
		value->kind = BE_ADDRESS;
		BEValue res = llvm_emit_assign_expr(c, value, decl->var.init_expr, decl->var.failable_ref);
		if (!is_failable) *value = res;
	}
	else if (decl->var.no_init)
	{
		llvm_value_set(value, LLVMGetUndef(alloc_type), decl->type);
		if (decl->var.failable_ref)
		{
			LLVMBuildStore(c->builder, LLVMGetUndef(llvm_get_type(c, type_anyerr)), decl->var.failable_ref);
		}
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
			llvm_value_set(value, LLVMConstNull(alloc_type), type);
		}
		else
		{
			llvm_value_set_decl_address(c, value, decl);
			value->kind = BE_ADDRESS;
			llvm_store_zero(c, value);
			llvm_value_set(value, llvm_get_zero(c, type), type);
		}
	}
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

		LLVMValueRef decl_value = llvm_get_ref(context, last->decl_expr);
		if (bool_cast && last->decl_expr->var.unwrap)
		{
			llvm_value_set_bool(be_value, LLVMConstInt(context->bool_type, 1, false));
			return;
		}
		llvm_value_set_address_abi_aligned(be_value, decl_value, type);
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

static inline void llvm_emit_return(GenContext *c, Ast *ast)
{

	PUSH_ERROR();

	Expr *expr = ast->return_stmt.expr;
	if (expr && expr->expr_kind == EXPR_FAILABLE)
	{
		BEValue be_value;
		llvm_emit_expr(c, &be_value, expr->inner_expr);
		llvm_emit_statement_chain(c, ast->return_stmt.cleanup);
		llvm_emit_return_abi(c, NULL, &be_value);
		return;
	}

	LLVMBasicBlockRef error_return_block = NULL;
	LLVMValueRef error_out = NULL;
	if (type_is_failable(c->cur_func_decl->type->func.prototype->rtype))
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
		c->retval = return_value;
	}

	POP_ERROR();


	llvm_emit_statement_chain(c, ast->return_stmt.cleanup);

	// Are we in an expression block?
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
		llvm_value_set_address_abi_aligned(&value, error_out, type_anyerr);
		llvm_emit_return_abi(c, NULL, &value);
		c->current_block = NULL;
	}
	LLVMBasicBlockRef post_ret_block = llvm_basic_block_new(c, "postreturn");
	llvm_emit_block(c, post_ret_block);
}

static inline void llvm_emit_block_exit_return(GenContext *c, Ast *ast)
{

	PUSH_ERROR();

	LLVMBasicBlockRef error_return_block = NULL;
	LLVMValueRef error_out = NULL;
	c->error_var = c->block_error_var;
	c->catch_block = c->block_failable_exit;

	LLVMBasicBlockRef err_cleanup_block = NULL;
	Expr *ret_expr = ast->return_stmt.expr;

	BEValue return_value = { 0 };
	if (ret_expr)
	{
		if (ast->return_stmt.cleanup && IS_FAILABLE(ret_expr))
		{
			assert(c->catch_block);
			err_cleanup_block = llvm_basic_block_new(c, "opt_block_cleanup");
			c->catch_block = err_cleanup_block;
		}
		llvm_emit_expr(c, &return_value, ast->return_stmt.expr);
		llvm_value_fold_failable(c, &return_value);
	}

	POP_ERROR();

	llvm_emit_statement_chain(c, ast->return_stmt.cleanup);
	if (c->return_out && return_value.value)
	{
		llvm_store_value_aligned(c, c->return_out, &return_value, type_alloca_alignment(return_value.type));
	}

	if (err_cleanup_block)
	{
		llvm_emit_br(c, c->block_return_exit);
		llvm_emit_block(c, err_cleanup_block);
		llvm_emit_statement_chain(c, ast->return_stmt.cleanup);
		llvm_emit_jmp(c, c->block_failable_exit);
	}
	else
	{
		llvm_emit_jmp(c, c->block_return_exit);
	}
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

	Ast *then_body = astptr(ast->if_stmt.then_body);
	// Only generate a target if
	if (ast_is_not_empty(then_body))
	{
		then_block = llvm_basic_block_new(c, "if.then");
	}

	// We have an optional else block.
	AstId else_id = ast->if_stmt.else_body;
	Ast *else_body = else_id ? astptr(else_id) : NULL;
	if (ast_is_not_empty(else_body))
	{
		else_block = llvm_basic_block_new(c, "if.else");
	}

	Expr *cond = exprptr(ast->if_stmt.cond);
	ast->if_stmt.codegen.break_block = exit_block;

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
		llvm_emit_decl_expr_list(c, &be_value, cond, false);
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

	llvm_emit_decl_expr_list(c, &be_value, cond, true);

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
		llvm_emit_stmt(c, then_body);

		// Jump to exit.
		llvm_emit_br(c, exit_block);
	}

	EMIT_ELSE:
	// Emit the 'else' branch if present.
	if (else_block != exit_block)
	{
		llvm_emit_block(c, else_block);
		llvm_emit_stmt(c, else_body);
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

static inline LoopType loop_type_for_cond(Expr *cond, bool do_while)
{
	if (!cond)
	{
		// We may have do-while (0)
		if (do_while) return LOOP_NONE;

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
	if (ast->for_stmt.init) llvm_emit_expr(c, &value, exprptr(ast->for_stmt.init));

	bool no_exit = ast->for_stmt.flow.no_exit;
	ExprId incr = ast->for_stmt.incr;

	LLVMBasicBlockRef inc_block = incr ? llvm_basic_block_new(c, "loop.inc") : NULL;
	Ast *body = astptr(ast->for_stmt.body);
	LLVMBasicBlockRef body_block = ast_is_not_empty(body) ? llvm_basic_block_new(c, "loop.body") : NULL;
	LLVMBasicBlockRef cond_block = NULL;

	// Skipping first cond? This is do-while semantics
	bool skip_first = ast->for_stmt.flow.skip_first;

	ExprId cond_id = ast->for_stmt.cond;
	Expr *cond = cond_id ? exprptr(cond_id) : NULL;
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
			SourceSpan loc = ast->span;
			File  *file = source_file_by_id(loc.file_id);

			llvm_emit_panic(c, "Infinite loop found", file->name, c->cur_func_decl->extname, loc.row ? loc.row : 1);
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

	ast->for_stmt.codegen.continue_block = continue_block;
	ast->for_stmt.codegen.exit_block = exit_block;

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
			llvm_emit_decl_expr_list(c, &be_value, cond, true);
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
		llvm_emit_stmt(c, body);

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
		llvm_emit_expr(c, &dummy, incr ? exprptr(incr) : NULL);
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
	bool is_if_chain = switch_ast->switch_stmt.flow.if_chain;
	Type *switch_type = switch_ast->ast_kind == AST_IF_CATCH_SWITCH_STMT ? type_lowering(type_anyerr) : exprptr(switch_ast->switch_stmt.cond)->type;

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

	BEValue switch_var;
	llvm_value_set_address_abi_aligned(&switch_var, llvm_emit_alloca_aligned(c, switch_type, "switch"), switch_type);
	switch_ast->switch_stmt.codegen.retry_var = &switch_var;
	llvm_store_value(c, &switch_var, switch_value);

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
	llvm_emit_decl_expr_list(context, &switch_value, exprptr(ast->switch_stmt.cond), false);
	gencontext_emit_switch_body(context, &switch_value, ast);
}


void llvm_emit_break(GenContext *c, Ast *ast)
{
	llvm_emit_statement_chain(c, ast->contbreak_stmt.defers);
	Ast *jump_target = astptr(ast->contbreak_stmt.ast);
	LLVMBasicBlockRef jump;
	switch (jump_target->ast_kind)
	{
		case AST_IF_STMT:
			jump = jump_target->if_stmt.codegen.break_block;
			break;
		case AST_FOR_STMT:
			jump = jump_target->for_stmt.codegen.exit_block;
			break;
		case AST_IF_CATCH_SWITCH_STMT:
		case AST_SWITCH_STMT:
			jump = jump_target->switch_stmt.codegen.exit_block;
			break;
		case AST_FOREACH_STMT:
		default:
			UNREACHABLE
	}
	llvm_emit_jmp(c, jump);
}

void llvm_emit_continue(GenContext *c, Ast *ast)
{
	llvm_emit_statement_chain(c, ast->contbreak_stmt.defers);
	Ast *jump_target = astptr(ast->contbreak_stmt.ast);
	LLVMBasicBlockRef jump;
	switch (jump_target->ast_kind)
	{
		case AST_IF_STMT:
		case AST_SWITCH_STMT:
		case AST_FOREACH_STMT:
			UNREACHABLE
			break;
		case AST_FOR_STMT:
			jump = jump_target->for_stmt.codegen.continue_block;
			break;
		default:
			UNREACHABLE
	}
	llvm_emit_jmp(c, jump);
}

void gencontext_emit_next_stmt(GenContext *context, Ast *ast)
{
	Ast *jump_target = astptr(ast->nextcase_stmt.case_switch_stmt);
	if (jump_target->ast_kind != AST_SWITCH_STMT)
	{
		llvm_emit_statement_chain(context, ast->nextcase_stmt.defer_id);
		llvm_emit_jmp(context, jump_target->case_stmt.backend_block);
		return;
	}
	BEValue be_value;
	llvm_emit_expr(context, &be_value, ast->nextcase_stmt.switch_expr);
	llvm_store_value(context, jump_target->switch_stmt.codegen.retry_var, &be_value);
	llvm_emit_statement_chain(context, ast->nextcase_stmt.defer_id);
	llvm_emit_jmp(context, jump_target->switch_stmt.codegen.retry_block);
}


static inline void llvm_emit_assume(GenContext *c, Expr *expr)
{
	// 1. Convert x > 0 && y > 2 => llvm.assume(x > 0) + llvm.assume(y > 2)
	if (expr->expr_kind == EXPR_BINARY && expr->binary_expr.operator == BINARYOP_AND)
	{
		llvm_emit_assume(c, exprptr(expr->binary_expr.left));
		llvm_emit_assume(c, exprptr(expr->binary_expr.right));
		return;
	}

	// 2. Convert !(x > 0 || y > 2) => llvm.assume(!(x > 0)) + llvm.assume(!(y > 2))
	if (expr->expr_kind == EXPR_UNARY && expr->unary_expr.operator == UNARYOP_NOT)
	{
		Expr *inner = expr->unary_expr.expr;
		if (inner->expr_kind == EXPR_BINARY && inner->binary_expr.operator == BINARYOP_OR)
		{
			Expr *left = exprptr(inner->binary_expr.left);
			Expr *right = exprptr(inner->binary_expr.right);

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
	ExprId exprid = ast->assert_stmt.expr;
	Expr *assert_expr = exprptr(exprid);

	if (active_target.feature.safe_mode)
	{
		BEValue value;
		llvm_emit_expr(c, &value, assert_expr);
		llvm_value_rvalue(c, &value);
		LLVMBasicBlockRef on_fail = llvm_basic_block_new(c, "assert_fail");
		LLVMBasicBlockRef on_ok = llvm_basic_block_new(c, "assert_ok");
		assert(value.kind == BE_BOOLEAN);
		llvm_emit_cond_br(c, &value, on_ok, on_fail);
		llvm_emit_block(c, on_fail);
		SourceSpan loc = assert_expr->span;
		const char *error;
		if (ast->assert_stmt.message)
		{
			error = exprptr(ast->assert_stmt.message)->const_expr.string.chars;
		}
		else
		{
			error = "Assert violation";
		}
		File  *file = source_file_by_id(loc.file_id);
		llvm_emit_panic(c, error, file->name, c->cur_func_decl->name, loc.row ? loc.row : 1);
		llvm_emit_br(c, on_ok);
		llvm_emit_block(c, on_ok);
	}
	llvm_emit_assume(c, exprptr(ast->assert_stmt.expr));
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
	                                       scratch_buffer_to_string(), scratch_buffer.len,
	                                       ast->asm_stmt.is_volatile,
	                                       true,
	                                       LLVMInlineAsmDialectIntel
#if LLVM_VERSION_MAJOR > 12
											, /* can throw */ false
#endif
	                                       );
	LLVMBuildCall2(c->builder, asm_fn_type, asm_fn, NULL, 0, "");
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
		llvm_value_fold_failable(c, &value);
		EMIT_LOC(c, ast);
		llvm_emit_br(c, discard_fail);
		llvm_emit_block(c, discard_fail);
		POP_ERROR();
		return;
	}
	llvm_emit_expr(c, &value, ast->expr_stmt);
}

LLVMValueRef llvm_emit_zstring(GenContext *c, const char *str)
{
	LLVMTypeRef char_type = llvm_get_type(c, type_char);
	unsigned len = (unsigned)strlen(str);
	LLVMTypeRef char_array_type = LLVMArrayType(char_type, len + 1);
	LLVMValueRef global_string = llvm_add_global_type(c, ".zstr", char_array_type, 0);
	llvm_set_internal_linkage(global_string);
	LLVMSetGlobalConstant(global_string, 1);
	LLVMSetInitializer(global_string, LLVMConstStringInContext(c->context, str, len, 0));
	AlignSize alignment;
	// TODO alignment
	LLVMValueRef string = llvm_emit_array_gep_raw(c, global_string, char_array_type, 0,
	                                              1, &alignment);
	return LLVMBuildBitCast(c->builder, string, LLVMPointerType(char_type, 0), "");
}


void llvm_emit_panic(GenContext *c, const char *message, const char *file, const char *func, unsigned line)
{
	if (c->debug.stack_slot_row)
	{
		llvm_store(c, c->debug.stack_slot_row, llvm_const_int(c, type_uint, line), type_abi_alignment(type_uint));
	}

	Decl *panicfn = c->panicfn;
	if (!panicfn)
	{
		llvm_emit_call_intrinsic(c, intrinsic_id.trap, NULL, 0, NULL, 0);
		return;
	}
	LLVMTypeRef char_ptr_type = llvm_get_ptr_type(c, type_char);
	LLVMValueRef args[4] = {
			llvm_emit_zstring(c, message),
			llvm_emit_zstring(c, file),
			func ? llvm_emit_zstring(c, func) : LLVMConstNull(char_ptr_type),
			llvm_const_int(c, type_uint, line)
	};

	LLVMBuildCall2(c->builder, llvm_get_type(c, panicfn->type), llvm_get_ref(c, panicfn), args, 4, "");
}

void llvm_emit_panic_if_true(GenContext *c, BEValue *value, const char *panic_name, SourceSpan loc)
{
	LLVMBasicBlockRef panic_block = llvm_basic_block_new(c, "panic");
	LLVMBasicBlockRef ok_block = llvm_basic_block_new(c, "checkok");
	assert(llvm_value_is_bool(value));
	llvm_emit_cond_br(c, value, panic_block, ok_block);
	llvm_emit_block(c, panic_block);
	File  *file = source_file_by_id(loc.file_id);
	llvm_emit_panic(c, panic_name, file->name, c->cur_func_decl->name, loc.row);
	llvm_emit_br(c, ok_block);
	llvm_emit_block(c, ok_block);
}

void llvm_emit_panic_on_true(GenContext *c, LLVMValueRef value, const char *panic_name, SourceSpan loc)
{
	File  *file = source_file_by_id(loc.file_id);
	LLVMBasicBlockRef panic_block = llvm_basic_block_new(c, "panic");
	LLVMBasicBlockRef ok_block = llvm_basic_block_new(c, "checkok");
	BEValue be_value;
	llvm_value_set_bool(&be_value, value);
	llvm_emit_cond_br(c, &be_value, panic_block, ok_block);
	llvm_emit_block(c, panic_block);
	llvm_emit_panic(c, panic_name, file->name, c->cur_func_decl->name, loc.row ? loc.row : 1);
	llvm_emit_br(c, ok_block);
	llvm_emit_block(c, ok_block);
}


void llvm_emit_stmt(GenContext *c, Ast *ast)
{
	EMIT_LOC(c, ast);
	assert(!c->catch_block && "Did not expect a catch block here.");
	switch (ast->ast_kind)
	{
		case AST_POISONED:
		case AST_IF_CATCH_SWITCH_STMT:
		case AST_FOREACH_STMT:
		case AST_DOC_STMT:
			UNREACHABLE
		case AST_EXPR_STMT:
			gencontext_emit_expr_stmt(c, ast);
			break;
		case AST_DECLARE_STMT:
		{
			BEValue value;
			llvm_emit_local_decl(c, ast->declare_stmt, &value);
			break;
		}
		case AST_BREAK_STMT:
			llvm_emit_break(c, ast);
			break;
		case AST_CONTINUE_STMT:
			llvm_emit_continue(c, ast);
			break;
		case AST_IF_STMT:
			llvm_emit_if(c, ast);
			break;
		case AST_RETURN_STMT:
			llvm_emit_return(c, ast);
			break;
		case AST_BLOCK_EXIT_STMT:
			llvm_emit_block_exit_return(c, ast);
			break;
		case AST_COMPOUND_STMT:
			llvm_emit_compound_stmt(c, ast);
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
			break;
		case AST_CT_ASSERT:
		case AST_CT_IF_STMT:
		case AST_CT_ELSE_STMT:
		case AST_CT_FOR_STMT:
		case AST_CT_SWITCH_STMT:
		case AST_CASE_STMT:
		case AST_DEFAULT_STMT:
		case AST_CT_FOREACH_STMT:
			UNREACHABLE
		case AST_SWITCH_STMT:
			gencontext_emit_switch(c, ast);
			break;
	}
}

