// Copyright (c) 2022 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.
#include "codegen_internal.h"

// Macro to cast an unsigned value `x` to a signed integer of `n` bits, then extend to int64_t
#define CAST_AND_EXTEND(x, n) \
    (((int64_t)((x) << (64 - (n)))) >> (64 - (n)))

INLINE bool codegen_asm_label(Ast *ast)
{
	if (ast->ast_kind != AST_ASM_LABEL) return false;
	scratch_buffer_printf("${:private}%s.${:uid}:\n", ast->asm_label);
	return true;
}

static inline void codegen_create_x86att_arg(AsmInlineBlock *block, unsigned input_offset, Expr *expr)
{
	ExprAsmArg *arg = &expr->expr_asm_arg;
	switch (arg->kind)
	{
		case ASM_ARG_INT:
			scratch_buffer_append("$$");
			scratch_buffer_append_unsigned_int(arg->value);
			return;
		case ASM_ARG_REG:
		{
			AsmRegister *reg = arg->reg.ref;
			scratch_buffer_append_char('%');
			scratch_buffer_append(&reg->name[1]);
			return;
		}
		case ASM_ARG_VALUE:
			scratch_buffer_append_char('$');
			scratch_buffer_append_unsigned_int(arg->index + input_offset);
			return;
		case ASM_ARG_MEMVAR:
		case ASM_ARG_REGVAR:
			scratch_buffer_append_char('$');
			if (arg->ident.is_input && !arg->ident.copy_output)
			{
				scratch_buffer_append_unsigned_int(arg->index + input_offset);
			}
			else
			{
				scratch_buffer_append_unsigned_int(arg->index);
			}
			return;
		case ASM_ARG_ADDR:
			if (arg->offset)
			{
				if (arg->neg_offset) scratch_buffer_append_char('-');
				scratch_buffer_append_unsigned_int(arg->offset);
			}
			scratch_buffer_append_char('(');
			if (arg->base)
			{
				codegen_create_x86att_arg(block, input_offset, exprptr(arg->base));
			}
			if (arg->idx)
			{
				scratch_buffer_append_char(',');
				codegen_create_x86att_arg(block, input_offset, exprptr(arg->idx));
				scratch_buffer_append_char(',');
				switch (arg->offset_type)
				{
					case ASM_SCALE_1:
						scratch_buffer_append_char('1');
						break;
					case ASM_SCALE_2:
						scratch_buffer_append_char('2');
						break;
					case ASM_SCALE_4:
						scratch_buffer_append_char('4');
						break;
					case ASM_SCALE_8:
						scratch_buffer_append_char('8');
						break;
					default:
						UNREACHABLE
				}
			}
			scratch_buffer_append_char(')');
			return;
		case ASM_ARG_ADDROF:
			TODO
	}
	UNREACHABLE
}

static inline void codegen_create_aarch64_arg(AsmInlineBlock *block, unsigned input_offset, Expr *expr)
{
	ExprAsmArg *arg = &expr->expr_asm_arg;
	switch (arg->kind)
	{
		case ASM_ARG_INT:
			scratch_buffer_append_unsigned_int(arg->value);
			return;
		case ASM_ARG_REG:
			scratch_buffer_append(&arg->reg.ref->name[1]);
			return;
		case ASM_ARG_VALUE:
			scratch_buffer_append_char('$');
			scratch_buffer_append_unsigned_int(arg->index + input_offset);
			return;
		case ASM_ARG_MEMVAR:
		case ASM_ARG_REGVAR:
			scratch_buffer_append_char('$');
			if (arg->ident.is_input && !arg->ident.copy_output)
			{
				scratch_buffer_append_unsigned_int(arg->index + input_offset);
			}
			else
			{
				scratch_buffer_append_unsigned_int(arg->index);
			}
			return;
		case ASM_ARG_ADDR:
			TODO
		case ASM_ARG_ADDROF:
			TODO
	}
	UNREACHABLE
}

static inline void codegen_create_riscv_arg(AsmInlineBlock *block, unsigned input_offset, Expr *expr)
{
	ExprAsmArg *arg = &expr->expr_asm_arg;
	switch (arg->kind)
	{
		case ASM_ARG_INT:
			if (arg->is_neg)
			{
				scratch_buffer_append_signed_int(CAST_AND_EXTEND(arg->value, arg->bits));
			}
			else
			{
				scratch_buffer_append_unsigned_int(arg->value);
			}
			return;
		case ASM_ARG_REG:
			scratch_buffer_append(&arg->reg.ref->name[1]);
			return;
		case ASM_ARG_VALUE:
			scratch_buffer_append_char('$');
			scratch_buffer_append_unsigned_int(arg->index + input_offset);
			return;
		case ASM_ARG_MEMVAR:
		case ASM_ARG_REGVAR:
			scratch_buffer_append_char('$');
			if (arg->ident.is_input && !arg->ident.copy_output)
			{
				scratch_buffer_append_unsigned_int(arg->index + input_offset);
			}
			else
			{
				scratch_buffer_append_unsigned_int(arg->index);
			}
			return;
		case ASM_ARG_ADDR:
			if (arg->idx || arg->offset_type) TODO;
			if (arg->neg_offset) scratch_buffer_append_char('-');
			scratch_buffer_append_unsigned_int(arg->offset);
			scratch_buffer_append_char('(');
			if (arg->base)
			{
				codegen_create_riscv_arg(block, input_offset, exprptr(arg->base));
			}
			scratch_buffer_append_char(')');
			return;
		case ASM_ARG_ADDROF:
			TODO
	}
	UNREACHABLE
}


static inline char *codegen_create_x86_att_asm(AsmInlineBlock *block)
{
	AstId next = block->asm_stmt;
	scratch_buffer_clear();
	unsigned input_arg_offset = vec_size(block->output_vars);
	while (next)
	{
		Ast *ast = astptr(next);
		next = ast->next;
		if (codegen_asm_label(ast)) continue;
		scratch_buffer_append(ast->asm_stmt.instruction);
		Expr** args = ast->asm_stmt.args;
		unsigned arg_count = vec_size(args);
		scratch_buffer_append_char(' ');
		for (unsigned i = arg_count; i > 0; i--)
		{
			if (i != arg_count) scratch_buffer_append(", ");
			codegen_create_x86att_arg(block, input_arg_offset, args[i - 1]);
		}
		scratch_buffer_append_char('\n');
	}

	return scratch_buffer_to_string();
}

static inline char *codegen_create_aarch64_asm(AsmInlineBlock *block)
{
	AstId next = block->asm_stmt;
	scratch_buffer_clear();
	unsigned input_arg_offset = vec_size(block->output_vars);
	while (next)
	{
		Ast *ast = astptr(next);
		next = ast->next;
		if (codegen_asm_label(ast)) continue;
		scratch_buffer_append(ast->asm_stmt.instruction);
		Expr** args = ast->asm_stmt.args;
		unsigned arg_count = vec_size(args);
		scratch_buffer_append_char(' ');
		for (unsigned i = 0; i < arg_count; i++)
		{
			if (i > 0) scratch_buffer_append(", ");
			codegen_create_aarch64_arg(block, input_arg_offset, args[i]);
		}
		scratch_buffer_append_char('\n');
	}

	return scratch_buffer_to_string();
}

static inline char *codegen_create_riscv_asm(AsmInlineBlock *block)
{
	AstId next = block->asm_stmt;
	scratch_buffer_clear();
	unsigned input_arg_offset = vec_size(block->output_vars);
	while (next)
	{
		Ast *ast = astptr(next);
		next = ast->next;
		if (codegen_asm_label(ast)) continue;
		scratch_buffer_append(ast->asm_stmt.instruction);
		Expr** args = ast->asm_stmt.args;
		unsigned arg_count = vec_size(args);
		scratch_buffer_append_char(' ');
		for (unsigned i = 0; i < arg_count; i++)
		{
			if (i > 0) scratch_buffer_append(", ");
			codegen_create_riscv_arg(block, input_arg_offset, args[i]);
		}
		scratch_buffer_append_char('\n');
	}

	return scratch_buffer_to_string();
}

const char *codegen_create_asm(Ast *ast)
{
	ASSERT0(ast->ast_kind == AST_ASM_BLOCK_STMT);
	scratch_buffer_clear();
	AsmInlineBlock *block = ast->asm_block_stmt.block;
	if (compiler.platform.arch == ARCH_TYPE_X86_64 || compiler.platform.arch == ARCH_TYPE_X86)
	{
		return codegen_create_x86_att_asm(block);
	}
	if (compiler.platform.arch == ARCH_TYPE_AARCH64)
	{
		return codegen_create_aarch64_asm(block);
	}
	if (compiler.platform.arch == ARCH_TYPE_RISCV32 || compiler.platform.arch == ARCH_TYPE_RISCV64)
	{
		return codegen_create_riscv_asm(block);
	}
	UNREACHABLE
}