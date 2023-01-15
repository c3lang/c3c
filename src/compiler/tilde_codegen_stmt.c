// Copyright (c) 2022 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "tilde_internal.h"


bool tilde_emit_goto_if_needed(TildeContext *c, TB_Label jump)
{
	if (tb_basic_block_is_complete(c->f, tb_inst_get_label(c->f))) return false;
	tb_inst_goto(c->f, jump);
	return true;
}

INLINE void tilde_emit_statement_chain(TildeContext *c, AstId current)
{
	while (current)
	{
		tilde_emit_stmt(c, ast_next(&current));
	}
}

INLINE void tilde_emit_compound_stmt(TildeContext *c, Ast *ast)
{
	// Push debug scope
	tilde_emit_statement_chain(c, ast->compound_stmt.first_stmt);
	// Pop debug scope
}

static void tilde_emit_return_abi(TildeContext *c, TBEValue *return_value, TBEValue *optional)
{
	FunctionPrototype *prototype = c->cur_func.prototype;

	// If there is no prototype, this is a static initializer, so bail.
	if (!prototype)
	{
		tb_inst_ret(c->f, TB_NULL_REG);
		return;
	}

	ABIArgInfo *info = prototype->ret_abi_info;

	// If we have an optional it's always the return argument, so we need to copy
	// the return value into the return value holder.
	TB_Reg return_out = c->return_out;
	Type *call_return_type = prototype->abi_ret_type;

	TBEValue no_fail;

	// In this case we use the optional as the actual return.
	if (prototype->is_optional)
	{
		if (return_value && return_value->type != type_void)
		{
			assert(return_value->type);
			tilde_store_to_ptr_aligned(c, c->return_out, return_value, type_alloca_alignment(return_value->type));
		}
		return_out = c->optional_out;
		if (!optional)
		{
			value_set(&no_fail, tilde_get_zero(c, type_anyerr), type_anyerr);
			optional = &no_fail;
		}
		return_value = optional;
	}
	assert(return_value || info->kind == ABI_ARG_IGNORE);

	switch (info->kind)
	{
		case ABI_ARG_INDIRECT:
			assert(return_value);
			tilde_store_to_ptr_aligned(c, return_out, return_value, info->indirect.alignment);
			tb_inst_ret(c->f, TB_NULL_REG);
			return;
		case ABI_ARG_IGNORE:
			tb_inst_ret(c->f, TB_NULL_REG);
			return;
		case ABI_ARG_DIRECT_SPLIT_STRUCT:
		case ABI_ARG_EXPAND:
			// Expands to multiple slots -
			// Not applicable to return values.
			UNREACHABLE
		case ABI_ARG_EXPAND_COERCE:
		{
			TODO
			/*
			// Pick the return as an address.
			value_addr(c, return_value);
			// Get the coerce type.
			LLVMTypeRef coerce_type = llvm_get_coerce_type(c, info);
			// Create the new pointer
			assert(return_value);
			LLVMValueRef coerce = LLVMBuildBitCast(c->builder, return_value->value, coerce_type, "");
			// We might have only one value, in that case, build a GEP to that one.
			LLVMValueRef lo_val;
			AlignSize alignment;
			LLVMValueRef lo = llvm_emit_struct_gep_raw(c, coerce, coerce_type, info->coerce_expand.lo_index,
			                                           return_value->alignment, &alignment);
			LLVMTypeRef lo_type = llvm_abi_type(c, info->coerce_expand.lo);
			lo_val = llvm_load(c, lo_type, lo, alignment, "");

			// We're done if there's a single field.
			if (!abi_type_is_valid(info->coerce_expand.hi))
			{
				llvm_emit_return_value(c, lo_val);
				return;
			}

			// Let's make a first class aggregate
			LLVMValueRef hi = llvm_emit_struct_gep_raw(c, coerce, coerce_type, info->coerce_expand.hi_index,
			                                           return_value->alignment, &alignment);
			LLVMTypeRef hi_type = llvm_abi_type(c, info->coerce_expand.hi);
			LLVMValueRef hi_val = llvm_load(c, hi_type, hi, alignment, "");

			LLVMTypeRef unpadded_type = llvm_get_twostruct(c, lo_type, hi_type);
			LLVMValueRef composite = llvm_get_undef_raw(unpadded_type);

			composite = llvm_emit_insert_value(c, composite, lo_val, 0);
			composite = llvm_emit_insert_value(c, composite, hi_val, 1);

			// And return that unpadded result
			llvm_emit_return_value(c, composite);
			break;*/
		}
		case ABI_ARG_DIRECT:
		DIRECT_RETURN:
			// The normal return
			tb_inst_ret(c->f, tilde_load_value_store(c, return_value));
			return;
		case ABI_ARG_DIRECT_PAIR:
		{
			TODO
			goto DIRECT_RETURN;
			/*
			LLVMTypeRef coerce_type = llvm_get_coerce_type(c, info);
			if (coerce_type == llvm_get_type(c, call_return_type)) goto DIRECT_RETURN;
			llvm_emit_return_value(c, llvm_emit_coerce(c, coerce_type, return_value, call_return_type));
			return;*/
		}
		case ABI_ARG_DIRECT_COERCE_INT:
		{
			TODO
			/*
			LLVMTypeRef coerce_type = LLVMIntTypeInContext(c->context, type_size(call_return_type) * 8);
			if (coerce_type == llvm_get_type(c, call_return_type)) goto DIRECT_RETURN;
			llvm_emit_return_value(c, llvm_emit_coerce(c, coerce_type, return_value, call_return_type));*/
			return;
		}
		case ABI_ARG_DIRECT_COERCE:
		{
			TODO
			/*
			LLVMTypeRef coerce_type = llvm_get_type(c, info->direct_coerce_type);
			if (coerce_type == llvm_get_type(c, call_return_type)) goto DIRECT_RETURN;
			llvm_emit_return_value(c, llvm_emit_coerce(c, coerce_type, return_value, call_return_type));*/
			return;
		}
	}
}

static void tilde_emit_return_implicit(TildeContext *c)
{
	Type *rtype_real = c->cur_func.prototype ? c->cur_func.prototype->rtype : type_void;
	if (type_lowering(type_no_optional(rtype_real)) != type_void)
	{
		tb_inst_unreachable(c->f);
		return;
	}
	if (type_is_optional(rtype_real))
	{
		tilde_emit_return_abi(c, NULL, NULL);
		return;
	}
	TBEValue value;
	value_set(&value, tb_inst_ptr(c->f, 0), type_anyerr);
	tilde_emit_return_abi(c, NULL, &value);
}

static void tilde_emit_decl_expr_list(TildeContext *c, TBEValue *be_value, Expr *expr, bool bool_cast)
{
	assert(expr->expr_kind == EXPR_COND);
	ByteSize size = vec_size(expr->cond_expr);
	ByteSize last_index = size - 1;
	for (ByteSize i = 0; i < last_index; i++)
	{
		TBEValue value;
		tilde_emit_expr(c, &value, expr->cond_expr[i]);
	}
	Expr *last = expr->cond_expr[last_index];
	Type *type = last->type;
	tilde_emit_expr(c, be_value, last);
	if (last->expr_kind == EXPR_DECL)
	{
		type = last->decl_expr->var.type_info->type;

		TB_Reg decl_value = tilde_get_ref(c, last->decl_expr);
		if (bool_cast && last->decl_expr->var.unwrap)
		{
			value_set(be_value, tb_inst_bool(c->f, true), type_bool);
			return;
		}
		value_set_address_abi_aligned(be_value, decl_value, type);
	}
	if (bool_cast)
	{
		type = type_lowering(type);
		if (type->type_kind != TYPE_BOOL)
		{
			CastKind cast = cast_to_bool_kind(type);
			tilde_emit_cast(c, cast, last, be_value, type, type_bool);
		}
	}
}

INLINE void tilde_emit_return_stmt(TildeContext *c, Ast *ast)
{
	PUSH_OPT();

	Expr *expr = ast->return_stmt.expr;
	if (expr && expr->expr_kind == EXPR_OPTIONAL)
	{
		TBEValue be_value;
		tilde_emit_expr(c, &be_value, expr->inner_expr);
		tilde_emit_statement_chain(c, ast->return_stmt.cleanup);
		tilde_emit_return_abi(c, NULL, &be_value);
		return;
	}

	TB_Label error_return_block = 0;
	TB_Reg error_out = TB_NULL_REG;
	if (c->cur_func.prototype && type_is_optional(c->cur_func.prototype->rtype))
	{
		error_return_block = tb_basic_block_create(c->f);
		error_out = tilde_emit_alloca(c, type_anyerr, 0);
		c->opt_var = error_out;
		c->catch_block = error_return_block;
	}

	bool has_return_value = ast->return_stmt.expr != NULL;
	TBEValue return_value = { 0 };
	if (has_return_value)
	{
		tilde_emit_expr(c, &return_value, ast->return_stmt.expr);
		value_fold_optional(c, &return_value);
		c->retval = return_value;
	}

	POP_OPT();

	tilde_emit_statement_chain(c, ast->return_stmt.cleanup);

	// Are we in an expression block?
	if (!has_return_value)
	{
		tilde_emit_return_implicit(c);
	}
	else
	{
		tilde_emit_return_abi(c, &return_value, NULL);
	}
	if (error_return_block)
	{
		tilde_emit_block(c, error_return_block);
		TBEValue value;
		value_set_address_abi_aligned(&value, error_out, type_anyerr);
		tilde_emit_return_abi(c, NULL, &value);
	}
}

void tilde_emit_local_decl(TildeContext *c, Decl *decl, TBEValue *value)
{
	// 1. Get the declaration and the LLVM type.
	Type *var_type = type_lowering(type_no_optional(decl->type));

	// 2. In the case we have a static variable,
	//    then we essentially treat this as a global.
	if (decl->var.is_static)
	{
		TODO
		/*
		// In defers we might already have generated this variable.
		if (decl->backend_ref)
		{
			llvm_value_set_decl(c, value, decl);
			return;
		}
		void *builder = c->builder;
		c->builder = c->global_builder;
		decl->backend_ref = llvm_add_global(c, "tempglobal", var_type, decl->alignment);
		if (IS_OPTIONAL(decl))
		{
			scratch_buffer_clear();
			scratch_buffer_append(decl->extname);
			scratch_buffer_append("$f");
			decl->var.optional_ref = llvm_add_global(c, scratch_buffer_to_string(), type_anyerr, 0);
		}
		llvm_emit_global_variable_init(c, decl);
		c->builder = builder;
		llvm_value_set_decl(c, value, decl);
		return;*/
	}
	assert(!decl->backend_ref);
	decl->tb_register = tb_inst_local(c->f, type_size(var_type), type_alloca_alignment(var_type));
	Expr *init = decl->var.init_expr;
	bool is_optional = IS_OPTIONAL(decl);
	if (is_optional)
	{
		scratch_buffer_clear();
		scratch_buffer_append(decl->name);
		scratch_buffer_append(".f");
		decl->var.tb_optional_reg = tb_inst_local(c->f, type_size(type_anyerr), type_alloca_alignment(type_anyerr));
		// Only clear out the result if the assignment isn't an optional.
	}

	if (init)
	{
		value_set_decl_address(c, value, decl);
		value->kind = TBE_ADDRESS;
		TBEValue val = tilde_emit_assign_expr(c, value, decl->var.init_expr, decl->var.tb_optional_reg);
		if (!is_optional) *value = val;
	}
	else if (decl->var.no_init)
	{
		value_set(value, tb_inst_poison(c->f), decl->type);
		if (decl->var.tb_optional_reg)
		{
			tilde_store_to_ptr_raw(c, decl->var.tb_optional_reg, tb_inst_poison(c->f), type_anyerr);
		}
	}
	else
	{
		if (decl->var.tb_optional_reg)
		{
			tilde_store_zero(c, type_anyerr, decl->var.tb_optional_reg, 0);
		}

		Type *type = type_lowering(decl->type);
		// Normal case, zero init.
		if (type_is_builtin(type->type_kind) || type->type_kind == TYPE_POINTER)
		{
			tilde_store_zero(c, type_anyerr, decl->tb_register, decl->alignment);
		}
		else
		{
			tb_inst_memclr(c->f, decl->tb_register, type_bit_size(type), decl->alignment);
		}
	}

}

static void tilde_emit_expr_stmt(TildeContext *c, Ast *ast)
{
	TBEValue value;
	if (IS_OPTIONAL(ast->expr_stmt))
	{
		PUSH_OPT();
		TB_Label discard_fail = tb_basic_block_create(c->f);
		c->catch_block = discard_fail;
		c->opt_var = TB_NULL_REG;
		tilde_emit_expr(c, &value, ast->expr_stmt);
		value_fold_optional(c, &value);
		EMIT_LOC(c, ast);
		tilde_emit_goto_if_needed(c, discard_fail);
		tilde_emit_block(c, discard_fail);
		POP_OPT();
		return;
	}

	tilde_emit_expr(c, &value, ast->expr_stmt);
}

// See llvm_emit_if_stmt
static void tilde_emit_if_stmt(TildeContext *c, Ast *ast)
{
	// We need at least the exit block and the "then" block.
	TB_Label exit_block = tb_basic_block_create(c->f);
	TB_Label then_block = exit_block;
	TB_Label else_block = exit_block;

	Ast *then_body = astptr(ast->if_stmt.then_body);
	// Only generate a target if
	if (ast_is_not_empty(then_body))
	{
		then_block = tb_basic_block_create(c->f);
	}

	// We have an optional else block.
	AstId else_id = ast->if_stmt.else_body;
	Ast *else_body = else_id ? astptr(else_id) : NULL;
	if (ast_is_not_empty(else_body))
	{
		else_block = tb_basic_block_create(c->f);
	}

	Expr *cond = exprptr(ast->if_stmt.cond);
	ast->if_stmt.codegen.tb_break_block = exit_block;

	// Output boolean value and switch.

	Decl *label = ast->if_stmt.flow.label;
	if (label)
	{
		label->label.tb_break_target = exit_block;
	}

	TBEValue be_value = { 0 };

	bool exit_in_use = true;

	if (then_body->ast_kind == AST_IF_CATCH_SWITCH_STMT)
	{
		TODO
		/*
		tilde_emit_decl_expr_list(c, &be_value, cond, false);
		value_rvalue(c, &be_value);
		TBEValue comp;
		tilde_emit_int_comp_zero(c, &comp, &be_value, BINARYOP_NE);
		tb_inst_if(c->f, comp.reg, then_block, else_block);
		tb_inst_set_label(c->f, then_block);
		tilde_emit_switch_body(c, &be_value, then_body);
		tb_inst_goto(c->f, exit_block);
		goto EMIT_ELSE;*/
	}

	tilde_emit_decl_expr_list(c, &be_value, cond, true);

	value_rvalue(c, &be_value);

	if (then_block != else_block)
	{
		tb_inst_if(c->f, be_value.reg, then_block, else_block);
	}

	// Emit the 'then' code.
	if (then_block != exit_block)
	{
		tilde_emit_block(c, then_block);
		tilde_emit_stmt(c, then_body);

		// Jump to exit.
		tilde_emit_goto_if_needed(c, exit_block);
	}

	// Emit the 'else' branch if present.
	if (else_block != exit_block)
	{
		tilde_emit_block(c, else_block);
		tilde_emit_stmt(c, else_body);
		tilde_emit_goto_if_needed(c, exit_block);
	}

	tilde_emit_block(c, exit_block);
}

void tilde_emit_stmt(TildeContext *c, Ast *ast)
{
	switch (ast->ast_kind)
	{
		case AST_COMPOUND_STMT:
			tilde_emit_compound_stmt(c, ast);
			return;
		case AST_RETURN_STMT:
			tilde_emit_return_stmt(c, ast);
			return;
		case AST_IF_STMT:
			tilde_emit_if_stmt(c, ast);
			return;
		case AST_DECLARE_STMT:
		{
			TBEValue value;
			tilde_emit_local_decl(c, ast->declare_stmt, &value);
			return;
		}
		case AST_EXPR_STMT:
			tilde_emit_expr_stmt(c, ast);
			return;
		default:
			TODO
	}
}

void tilde_emit_jump_to_optional_exit(TildeContext *c, TB_Reg opt_value)
{
	assert(c->catch_block && "unexpected emit");
	bool is_constant_opt_zero = tb_node_is_constant_zero(c->f, opt_value);

	// Maybe we don't need to emit anything?
	if (is_constant_opt_zero) return;

	bool is_constant_opt = false;

	TB_Label after_block = tb_basic_block_create(c->f);

	// No error variable
	if (!c->opt_var)
	{
		// No error var and a constant error means jumping to the "catch" block
		if (is_constant_opt)
		{
			tb_inst_goto(c->f, c->catch_block);
		}
		else
		{
			tb_inst_if(c->f, tilde_emit_is_no_opt(c, opt_value), after_block, c->catch_block);
		}
		tilde_emit_block(c, after_block);
		return;
	}

	// If it's not a constant, then jump conditionally
	if (!is_constant_opt)
	{
		TB_Reg was_ok = tilde_emit_is_no_opt(c, opt_value);
		TB_Label error_block = tb_basic_block_create(c->f);
		tb_inst_if(c->f, was_ok, after_block, error_block);
		tilde_emit_block(c, error_block);
	}

	tilde_store_to_ptr_raw(c, c->opt_var, opt_value, type_anyerr);
	tb_inst_goto(c->f, c->catch_block);
	tilde_emit_block(c, after_block);

}
