// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "llvm_codegen_internal.h"

void gencontext_emit_try_stmt(GenContext *context, Ast *pAst);


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

static LLVMValueRef llvm_emit_decl(GenContext *c, Ast *ast)
{
	Decl *decl = ast->declare_stmt;

	LLVMTypeRef alloc_type = llvm_get_type(c, type_lowering(decl->type));

	if (decl->var.is_static)
	{
		decl->backend_ref = LLVMAddGlobal(c->module, llvm_get_type(c, decl->type), "tempglobal");
		llvm_emit_global_variable_init(c, decl);
		if (decl->var.failable)
		{
			decl->var.failable_ref = LLVMAddGlobal(c->module, llvm_get_type(c, type_error), decl->name);
			LLVMBuildStore(c->builder, LLVMConstNull(llvm_get_type(c, type_error)), decl->var.failable_ref);
		}
		return decl->backend_ref;
	}
	llvm_emit_local_var_alloca(c, decl);
	if (decl->var.failable)
	{
		decl->var.failable_ref = llvm_emit_alloca_aligned(c, type_error, decl->name);
		LLVMBuildStore(c->builder, LLVMConstNull(llvm_get_type(c, type_error)), decl->var.failable_ref);
	}

	Expr *init = decl->var.init_expr;
	if (init)
	{
		// If we don't have undef, then make an assign.
		if (init->expr_kind != EXPR_UNDEF)
		{
			BEValue value;
			llvm_value_set_decl_address(&value, decl);
			llvm_emit_assign_expr(c, &value, decl->var.init_expr, decl->var.failable_ref);
		}
		// TODO trap on undef in debug mode.
	}
	else
	{
		Type *type = type_flatten(decl->type);
		// Normal case, zero init.
		if (type_is_builtin(type->type_kind) || type->type_kind == TYPE_POINTER)
		{
			llvm_emit_store(c, decl, LLVMConstNull(alloc_type));
		}
		else
		{
			llvm_emit_memclear_size_align(c, decl->backend_ref, type_size(decl->type), decl->alignment, true);
		}
	}
	return decl->backend_ref;
}

void llvm_emit_decl_expr_list_ignore_result(GenContext *context, Expr *expr)
{
	assert(expr->expr_kind == EXPR_DECL_LIST);
	VECEACH(expr->dexpr_list_expr, i)
	{
		llvm_emit_stmt(context, expr->dexpr_list_expr[i]);
	}
}

void gencontext_emit_decl_expr_list(GenContext *context, BEValue *be_value, Expr *expr, bool bool_cast)
{
	assert(expr->expr_kind == EXPR_DECL_LIST);
	ByteSize size = vec_size(expr->dexpr_list_expr);
	ByteSize last_index = size - 1;
	for (ByteSize i = 0; i < last_index; i++)
	{
		Ast *ast = expr->dexpr_list_expr[i];
		if (ast->ast_kind == AST_DECLARE_STMT)
		{
			llvm_emit_decl(context, ast);
		}
		else
		{
			assert(ast->ast_kind == AST_EXPR_STMT);
			BEValue value;
			llvm_emit_expr(context, &value, ast->expr_stmt);
		}
	}
	Ast *last = expr->dexpr_list_expr[last_index];
	Type *type;
	switch (last->ast_kind)
	{
		case AST_EXPR_STMT:
			type = last->expr_stmt->type;
			llvm_emit_expr(context, be_value, last->expr_stmt);
			break;
		case AST_DECLARE_STMT:
			type = last->declare_stmt->var.type_info->type;
			{
				LLVMValueRef decl_value = llvm_emit_decl(context, last);
				if (bool_cast && last->declare_stmt->var.unwrap)
				{
					llvm_value_set_bool(be_value, LLVMConstInt(context->bool_type, 1, false));
					return;
				}
				llvm_value_set_address(be_value, decl_value, type);
			}
			break;
		default:
			UNREACHABLE
	}
	if (bool_cast)
	{
		type = type_flatten(type);
		if (type->type_kind != TYPE_BOOL)
		{
			CastKind cast = cast_to_bool_kind(type);
			llvm_emit_cast(context, cast, be_value, type, type_bool);
		}
	}
}

void gencontext_emit_jmp(GenContext *context, LLVMBasicBlockRef block)
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
	else if (c->cur_func_decl->func.function_signature.failable)
	{
		error_return_block = llvm_basic_block_new(c, "err_retblock");
		error_out = llvm_emit_alloca_aligned(c, type_error, "reterr");
		c->error_var = error_out;
		c->catch_block = error_return_block;
	}

	bool has_return_value = ast->return_stmt.expr != NULL;
	BEValue return_value = {};
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
		gencontext_emit_jmp(c, c->block_return_exit);
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
		llvm_value_set_address(&value, error_out, type_error);
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

	// Only generate a target if
	if (ast_is_not_empty(ast->if_stmt.then_body))
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

	BEValue be_value = {};
	gencontext_emit_decl_expr_list(c, &be_value, ast->if_stmt.cond, true);
	llvm_value_rvalue(c, &be_value);

	assert(llvm_value_is_bool(&be_value));

	bool exit_in_use = true;

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
	LLVMValueRef len_value = llvm_value_rvalue_store(c, &len);

	// We pop the error here.
	POP_ERROR();

	llvm_emit_ptr_from_array(c, &enum_value);

	// Create the index and optionally the index var
	LLVMTypeRef real_index_type = llvm_get_type(c, type_isize);
	BEValue index_var = {};
	LLVMTypeRef index_type = ast->foreach_stmt.index ? llvm_get_type(c, ast->foreach_stmt.index->type) : NULL;
	BEValue index = {};

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
		llvm_value_set_address(&index, llvm_emit_alloca(c, real_index_type, type_abi_alignment(type_isize), "idx"), type_isize);
		llvm_store_bevalue_raw(c, &index, llvm_get_zero(c, index.type));
	}

	Type *actual_type = type_get_indexed_type(ast->foreach_stmt.enumeration->type);
	LLVMTypeRef actual_type_llvm = llvm_get_type(c, actual_type);

	llvm_emit_local_var_alloca(c, ast->foreach_stmt.variable);
	Type *var_type = type_flatten(ast->foreach_stmt.variable->type);
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
	LLVMValueRef index_value = llvm_value_rvalue_store(c, &index);
	LLVMValueRef may_loop = llvm_emit_int_comparison(c, type_isize, type_isize, index_value, len_value, BINARYOP_LT);
	BEValue b;
	llvm_value_set_bool(&b, may_loop);
	llvm_emit_cond_br(c, &b, body_block, exit_block);

	llvm_emit_block(c, body_block);

	// In the case where we have an index that is smaller, we need to do a cast.
	if (index_var.value && index.value != index_var.value)
	{
		LLVMValueRef stored_value;
		// Note that the case of index_var being usize and index being isize
		// does not occur here, since they are considered the same LLVM type.
		if (extend_bits)
		{
			// Note that we zero extend. We never deal in negative indices.
			stored_value = LLVMBuildZExt(c->builder, index_value, index_type, "");
		}
		else
		{
			stored_value = LLVMBuildTrunc(c->builder, index_value, index_type, "");
		}
		llvm_store_bevalue_raw(c, &index_var, stored_value);
	}

	assert(llvm_value_is_addr(&enum_value));

	LLVMValueRef ref_to_element = LLVMBuildInBoundsGEP2(c->builder, actual_type_llvm, enum_value.value, &index_value, 1, "");
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
	index_value = llvm_value_rvalue_store(c, &index);
	index_value = LLVMBuildAdd(c->builder, index_value, llvm_const_int(c, type_isize, 1), "");
	llvm_store_bevalue_raw(c, &index, index_value);

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
	LLVMBasicBlockRef body_block = ast->while_stmt.body->compound_stmt.stmts ? llvm_basic_block_new(context,
	                                                                                                  "while.body") : NULL;

	ast->while_stmt.continue_block = begin_block;
	ast->while_stmt.break_block = exit_block;

	// Emit cond
	llvm_emit_br(context, begin_block);
	llvm_emit_block(context, begin_block);
	DeferList defers = { 0, 0 };
	Expr *cond = ast->while_stmt.cond;
	if (cond->expr_kind == EXPR_SCOPED_EXPR)
	{
		defers = cond->expr_scope.defers;
		cond = cond->expr_scope.expr;
	}
	BEValue be_value;
	gencontext_emit_decl_expr_list(context, &be_value, cond, true);
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
		BEValue be_value = {};
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




void gencontext_emit_if_switch_body(GenContext *context, LLVMValueRef switch_value, Ast **cases)
{
	ByteSize case_count = vec_size(cases);
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
				case_stmt->case_stmt.backend_block = llvm_basic_block_new(context, "switch.default");
			}
			default_case = case_stmt;
		}
		else if (case_stmt->case_stmt.body)
		{
			case_stmt->case_stmt.backend_block = llvm_basic_block_new(context, "switch.case");
		}
	}


	LLVMBasicBlockRef exit_block = llvm_basic_block_new(context, "switch.exit");


	for (unsigned i = 0; i < case_count; i++)
	{
		Ast *case_stmt = cases[i];
		if (case_stmt == default_case) continue;
		LLVMValueRef case_value;
		LLVMBasicBlockRef case_block = case_stmt->case_stmt.backend_block;
		BEValue be_value;
		// TODO handle string.
		if (case_stmt->case_stmt.is_type)
		{
			case_value = case_stmt->case_stmt.type_info->type->backend_typeid;
		}
		else
		{
			llvm_emit_expr(context, &be_value, case_stmt->case_stmt.expr);
			llvm_value_rvalue(context, &be_value);
			case_value = be_value.value;
		}
		llvm_value_set_bool(&be_value, LLVMBuildICmp(context->builder, LLVMIntNE, switch_value, case_value, ""));
		LLVMBasicBlockRef next_block = llvm_basic_block_new(context, "");
		llvm_emit_cond_br(context, &be_value, next_block, case_block);
		llvm_emit_block(context, next_block);
	}
	if (default_case)
	{
		llvm_emit_br(context, default_case->case_stmt.backend_block);
	}
	else
	{
		llvm_emit_br(context, exit_block);
	}
	for (unsigned i = 0; i < case_count; i++)
	{
		Ast *case_stmt = cases[i];
		LLVMBasicBlockRef case_block = case_stmt->case_stmt.backend_block;
		llvm_emit_block(context, case_block);
		if (!case_stmt->case_stmt.body)
		{
			llvm_emit_br(context, i == case_count - 1 ? exit_block : cases[i + 1]->case_stmt.backend_block);
			continue;
		}

		// IMPORTANT!
		context->current_block_is_target = true;
		llvm_emit_stmt(context, case_stmt->case_stmt.body);
		llvm_emit_br(context, exit_block);

	}
	llvm_emit_block(context, exit_block);
}

static void gencontext_emit_switch_body(GenContext *context, BEValue *switch_value, Ast *switch_ast)
{
	Ast **cases = switch_ast->switch_stmt.cases;
	ByteSize case_count = vec_size(cases);
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
				case_stmt->case_stmt.backend_block = llvm_basic_block_new(context, "switch.default");
			}
			default_case = case_stmt;
		}
		else if (case_stmt->case_stmt.body)
		{
			case_stmt->case_stmt.backend_block = llvm_basic_block_new(context, "switch.case");
		}
	}

	LLVMBasicBlockRef exit_block = llvm_basic_block_new(context, "switch.exit");
	LLVMBasicBlockRef switch_block = llvm_basic_block_new(context, "switch.entry");
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


	Type *switch_type = switch_ast->switch_stmt.cond->type;
	LLVMValueRef switch_var = llvm_emit_alloca_aligned(context, switch_type, "switch");
	switch_ast->switch_stmt.codegen.retry_var = switch_var;
	LLVMBuildStore(context->builder, llvm_value_rvalue_store(context, switch_value), switch_var);

	llvm_emit_br(context, switch_block);
	llvm_emit_block(context, switch_block);

	LLVMValueRef switch_load = gencontext_emit_load(context, switch_type, switch_var);

	LLVMValueRef switch_stmt = LLVMBuildSwitch(context->builder, switch_load, default_case ? default_case->case_stmt.backend_block : exit_block, case_count);
	context->current_block = NULL;
	for (unsigned i = 0; i < case_count; i++)
	{
		Ast *case_stmt = cases[i];
		LLVMBasicBlockRef block = case_stmt->case_stmt.backend_block;
		if (case_stmt != default_case)
		{
			LLVMValueRef case_value;
			if (case_stmt->case_stmt.is_type)
			{
				case_value = case_stmt->case_stmt.type_info->type->backend_typeid;
			}
			else
			{
				BEValue be_value;
				llvm_emit_expr(context, &be_value, case_stmt->case_stmt.expr);
				llvm_value_rvalue(context, &be_value);
				case_value = be_value.value;
			}
			LLVMAddCase(switch_stmt, case_value, block);
		}

		// Skip fallthroughs.
		if (!case_stmt->case_stmt.body) continue;

		llvm_emit_block(context, block);
		// IMPORTANT!
		context->current_block_is_target = true;

		llvm_emit_stmt(context, case_stmt->case_stmt.body);
		llvm_emit_br(context, exit_block);
	}
	llvm_emit_block(context, exit_block);
}

void gencontext_emit_switch(GenContext *context, Ast *ast)
{
	BEValue switch_value;
	gencontext_emit_decl_expr_list(context, &switch_value, ast->switch_stmt.cond, false);
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
		case AST_SWITCH_STMT:
			jump = jump_target->switch_stmt.codegen.exit_block;
			break;
		case AST_DEFER_STMT:
			jump = jump_target->defer_stmt.codegen.exit_block;
			break;
		default:
			UNREACHABLE
	}
	gencontext_emit_jmp(context, jump);
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
	gencontext_emit_jmp(context, jump);
}

void gencontext_emit_next_stmt(GenContext *context, Ast *ast)
{
	Ast *jump_target = astptr(ast->next_stmt.case_switch_stmt);
	if (jump_target->ast_kind != AST_SWITCH_STMT)
	{
		llvm_emit_defer(context, ast->next_stmt.defers.start, ast->next_stmt.defers.end);
		gencontext_emit_jmp(context, jump_target->case_stmt.backend_block);
		return;
	}
	BEValue be_value;
	llvm_emit_expr(context, &be_value, ast->next_stmt.switch_expr);
	LLVMBuildStore(context->builder,
	               llvm_value_rvalue_store(context, &be_value), jump_target->switch_stmt.codegen.retry_var);
	llvm_emit_defer(context, ast->next_stmt.defers.start, ast->next_stmt.defers.end);
	gencontext_emit_jmp(context, jump_target->switch_stmt.codegen.retry_block);
}

void gencontext_emit_scoped_stmt(GenContext *context, Ast *ast)
{
	llvm_emit_stmt(context, ast->scoped_stmt.stmt);
	llvm_emit_defer(context, ast->scoped_stmt.defers.start, ast->scoped_stmt.defers.end);
}

void gencontext_emit_try_stmt(GenContext *c, Ast *ast)
{
	// Create after try block
	LLVMBasicBlockRef after_try = llvm_basic_block_new(c, "after_try");
	EMIT_LOC(c, ast);

	// Store catch/error var
	PUSH_ERROR();

	// Set the catch/error var
	c->error_var = NULL;
	c->catch_block = after_try;

	// Emit the checks, which will create jumps like we want them.
	Ast **decl_expr = ast->try_stmt.decl_expr->dexpr_list_expr;
	BEValue be_value;
	VECEACH(decl_expr, i)
	{
		Ast *dexpr = decl_expr[i];
		switch (dexpr->ast_kind)
		{
			case AST_EXPR_STMT:
				llvm_emit_expr(c, &be_value, dexpr->expr_stmt);
				llvm_value_rvalue(c, &be_value);
				break;
			case AST_DECLARE_STMT:
				llvm_emit_decl(c, dexpr);
				break;
			default:
				UNREACHABLE
		}
	}

	// Restore.
	POP_ERROR();

	// Emit the statement
	llvm_emit_stmt(c, ast->try_stmt.body);

	// Jump to after.
	llvm_emit_br(c, after_try);
	llvm_emit_block(c, after_try);

}


static inline void gencontext_emit_assume(GenContext *c, Expr *expr)
{
	// 1. Convert x > 0 && y > 2 => llvm.assume(x > 0) + llvm.assume(y > 2)
	if (expr->expr_kind == EXPR_BINARY && expr->binary_expr.operator == BINARYOP_AND)
	{
		gencontext_emit_assume(c, expr->binary_expr.left);
		gencontext_emit_assume(c, expr->binary_expr.right);
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
			gencontext_emit_assume(c, expr);


			expr->unary_expr.expr = right;
			gencontext_emit_assume(c, expr);

			return;
		}
	}

	// 3. Check if pure, if so we emit the assume.
	if (expr->pure)
	{
		BEValue value;
		llvm_emit_expr(c, &value, expr);
		llvm_value_rvalue(c, &value);
		assert(value.kind == BE_BOOLEAN);
		EMIT_LOC(c, expr);
		llvm_emit_call_intrinsic(c, intrinsic_id_assume, NULL, 0, &(value.value), 1);
	}
}



static inline void gencontext_emit_assert_stmt(GenContext *c, Ast *ast)
{
	if (active_target.feature.safe_mode)
	{
		BEValue value;
		llvm_emit_expr(c, &value, ast->assert_stmt.expr);
		llvm_value_rvalue(c, &value);
		LLVMBasicBlockRef on_fail = llvm_basic_block_new(c, "assert_fail");
		LLVMBasicBlockRef on_ok = llvm_basic_block_new(c, "assert_ok");
		assert(value.kind == BE_BOOLEAN);
		llvm_emit_cond_br(c, &value, on_fail, on_ok);
		llvm_emit_block(c, on_fail);
		// TODO emit message
		llvm_emit_call_intrinsic(c, intrinsic_id_trap, NULL, 0, NULL, 0);
		llvm_emit_block(c, on_ok);
		return;
	}
	gencontext_emit_assume(c, ast->assert_stmt.expr);
}

static inline void gencontext_emit_unreachable_stmt(GenContext *context, Ast *ast)
{
	// TODO emit message
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
	if (ast->expr_stmt->failable)
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

void gencontext_emit_catch_stmt(GenContext *c, Ast *ast)
{
	Expr *catch_expr;
	LLVMValueRef error_result = NULL;
	if (ast->catch_stmt.has_err_var)
	{
		Decl *error_var = ast->catch_stmt.err_var;
		assert(error_var->type->canonical == type_error);
		error_result = llvm_emit_alloca_aligned(c, type_error, error_var->name);
		error_var->backend_ref = error_result;
		catch_expr = error_var->var.init_expr;

	}
	else
	{
		if (ast->catch_stmt.is_switch)
		{
			error_result = llvm_emit_alloca_aligned(c, type_error, "catchval");
		}
		catch_expr = ast->catch_stmt.catchable;
	}

	// Create catch block.
	LLVMBasicBlockRef catch_block = llvm_basic_block_new(c, "catch");

	// Store catch/error var
	PUSH_ERROR();

	// Set the catch/error var
	c->error_var = error_result;
	c->catch_block = catch_block;

	// Emit the catch, which will create jumps like we want them.
	BEValue value;
	llvm_emit_expr(c, &value, catch_expr);
	llvm_value_fold_failable(c, &value);

	// Restore.
	POP_ERROR();

	// Create the bloch after and jump to it.
	LLVMBasicBlockRef after_catch = llvm_basic_block_new(c, "after_catch");
	llvm_emit_br(c, after_catch);

	// Emit catch.
	llvm_emit_block(c, catch_block);

	if (ast->catch_stmt.is_switch)
	{
		LLVMValueRef ref = llvm_emit_bitcast(c, error_result, type_get_ptr(type_typeid));
		gencontext_emit_if_switch_body(c, gencontext_emit_load(c, type_typeid, ref), ast->catch_stmt.cases);
	}
	else
	{
		llvm_emit_stmt(c, ast->catch_stmt.body);
	}

	// Jump to after.
	llvm_emit_br(c, after_catch);
	llvm_emit_block(c, after_catch);
}

void llvm_emit_puts_output(GenContext *c, const char *message)
{
	LLVMTypeRef char_ptr_type = llvm_get_ptr_type(c, type_char);
	LLVMTypeRef type = LLVMFunctionType(LLVMVoidTypeInContext(c->context), &char_ptr_type, 1, false);
	LLVMValueRef puts_func = LLVMGetNamedFunction(c->module, "puts");
	if (!puts_func)
	{
		puts_func = LLVMAddFunction(c->module, "puts", type);
	}
	LLVMValueRef global_name = LLVMAddGlobal(c->module, LLVMArrayType(llvm_get_type(c, type_char), strlen(message) + 1), "");
	LLVMSetLinkage(global_name, LLVMInternalLinkage);
	LLVMSetGlobalConstant(global_name, 1);
	LLVMSetInitializer(global_name, LLVMConstStringInContext(c->context, message, strlen(message), 0));

	LLVMValueRef zero = llvm_get_zero(c, type_usize);
	LLVMValueRef string = LLVMBuildInBoundsGEP2(c->builder, LLVMTypeOf(global_name), global_name, &zero, 1, "");
	string = LLVMBuildBitCast(c->builder, string, char_ptr_type, "");
	LLVMBuildCall(c->builder, puts_func, &string, 1, "");

}
void llvm_emit_panic_on_true(GenContext *c, LLVMValueRef value, const char *panic_name)
{
	LLVMBasicBlockRef panic_block = llvm_basic_block_new(c, "panic");
	LLVMBasicBlockRef ok_block = llvm_basic_block_new(c, "checkok");
	BEValue be_value;
	llvm_value_set_bool(&be_value, value);
	llvm_emit_cond_br(c, &be_value, panic_block, ok_block);
	llvm_emit_block(c, panic_block);
	llvm_emit_puts_output(c, panic_name);
	llvm_emit_call_intrinsic(c, intrinsic_id_trap, NULL, 0, NULL, 0);
	llvm_emit_br(c, ok_block);
	llvm_emit_block(c, ok_block);
}

void llvm_emit_yield_stmt(GenContext *c, Ast *ast)
{
	Decl **declarations = ast->yield_stmt.declarations;
	Expr **values = ast->yield_stmt.values;
	// Create backend refs on demand.
	foreach(declarations, i)
	{
		Decl *decl = declarations[i];
		if (!decl->backend_ref) llvm_emit_local_var_alloca(c, decl);
	}
	// Set the values
	foreach(values, i)
	{
		Expr *expr = values[i];
		BEValue value;
		llvm_emit_expr(c, &value, expr);
		llvm_store_bevalue_aligned(c, declarations[i]->backend_ref, &value, declarations[i]->alignment);
	}
	llvm_emit_stmt(c, ast->yield_stmt.ast);
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
		case AST_DEFINE_STMT:
			UNREACHABLE
		case AST_YIELD_STMT:
			llvm_emit_yield_stmt(c, ast);
			break;
		case AST_TRY_STMT:
			gencontext_emit_try_stmt(c, ast);
			break;
		case AST_SCOPED_STMT:
			gencontext_emit_scoped_stmt(c, ast);
			break;
		case AST_EXPR_STMT:
			gencontext_emit_expr_stmt(c, ast);
			break;
		case AST_DECLARE_STMT:
			llvm_emit_decl(c, ast);
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
		case AST_CATCH_STMT:
			gencontext_emit_catch_stmt(c, ast);
			break;
		case AST_ASM_STMT:
			TODO
		case AST_ASSERT_STMT:
			gencontext_emit_assert_stmt(c, ast);
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
		case AST_VOLATILE_STMT:
			TODO
	}
}

