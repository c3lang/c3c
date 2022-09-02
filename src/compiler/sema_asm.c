#include "sema_internal.h"
#include "x86.h"


static inline void sema_add_clobber(AsmInlineBlock *block, unsigned index)
{
	clobbers_add(&block->clobbers, index);
}

static inline void sema_add_clobbers(AsmInlineBlock *block, Clobbers *clobbers)
{
	for (unsigned i = 0; i < CLOBBER_FLAG_ELEMENTS; i++)
	{
		block->clobbers.mask[i] |= clobbers->mask[i];
	}
}

static inline Type *max_supported_imm_int(bool is_signed, AsmArgGroup arg)
{
	AsmArgGroup group = arg & AARG_IMM;
	if (is_signed && (group & AARG_IMMI32)) return type_int;
	if (!is_signed && (group & AARG_IMMU32)) return type_uint;
	if (group & AARG_IMM16) return is_signed ? type_short : type_ushort;
	if (is_signed && (group & AARG_IMMI8)) return type_ichar;
	if (!is_signed && (group & AARG_IMMU8)) return type_char;
	return NULL;
}

static inline AsmArgGroup sema_ireg_for_type(Type *type)
{
	switch (type_size(type))
	{
		case 1:
			return AARG_R8;
		case 2:
			return AARG_R16;
		case 4:
			return AARG_R32;
		case 8:
			return AARG_R64;
		case 16:
			return AARG_R128;
		default:
			UNREACHABLE
	}
}
static inline bool sema_reg_int_suported_type(AsmArgGroup arg, Type *type)
{
	assert(type_flatten(type) == type);
	AsmArgGroup group = arg & AARG_IREG;
	switch (type_size(type))
	{
		case 1:
			return (group & AARG_R8) != 0;
		case 2:
			return (group & AARG_R16) != 0;
		case 4:
			return (group & AARG_R32) != 0;
		case 8:
			return (group & AARG_R64) != 0;
		case 16:
			return (group & AARG_R128) != 0;
		default:
			UNREACHABLE
	}
}

static inline bool sema_reg_float_suported_type(AsmArgGroup arg, Type *type)
{
	assert(type_flatten(type) == type);
	AsmArgGroup group = arg & AARG_XMM;
	return false;
}

static inline Type *max_supported_mem_int(bool is_signed, AsmArgGroup arg)
{
	AsmArgGroup group = arg & AARG_MINT;
	if (group & AARG_M64) return is_signed ? type_long : type_ulong;
	if (group & AARG_M32) return is_signed ? type_int : type_uint;
	if (group & AARG_M16) return is_signed ? type_short : type_ushort;
	if (group & AARG_M8) return is_signed ? type_ichar : type_char;
	return NULL;
}

static inline bool sema_check_asm_arg_const_int(SemaContext *context, AsmInlineBlock *block, AsmInstruction *instr, AsmArgGroup arg_type, Expr *expr, Expr *int_expr)
{
	if ((arg_type & AARG_ANY_INT) == 0)
	{
		SEMA_ERROR(expr, "'%s' does not support an int parameter here.", instr->name);
		return false;
	}
	bool is_signed = type_kind_is_signed(int_expr->const_expr.ixx.type);
	bool imm = false;
	bool reg = false;
	bool mem = false;
	Type *type = max_supported_imm_int(is_signed, arg_type);
	if (!type)
	{
		SEMA_ERROR(expr, "'%s' does not support a direct value here, try using () around the value.", instr->name);
		return false;
	}
	Int i = int_expr->const_expr.ixx;
	if (!type || !int_fits(i, type->type_kind))
	{
		SEMA_ERROR(expr, "'%s' expected %s.", instr->name, type_quoted_error_string(type));
		return false;
	}
	if (is_signed)
	{
		switch (type->type_kind)
		{
			case TYPE_I8:
				i.i.low &= 0xFF;
				break;
			case TYPE_I16:
				i.i.low &= 0xFFFF;
				break;
			case TYPE_I32:
				i.i.low &= 0xFFFFFFFF;
				break;
			default:
				break;
		}
	}
	expr->expr_asm_arg.kind = ASM_ARG_INT;
	expr->expr_asm_arg.value = i.i.low;
	expr->type = type;
	return true;
}

static inline bool sema_check_asm_arg(SemaContext *context, AsmInlineBlock *block, AsmInstruction *instr, AsmArgGroup arg_type, Expr *expr);

static inline bool sema_check_asm_arg_addr(SemaContext *context, AsmInlineBlock *block, AsmInstruction *instr, AsmArgGroup arg_type, Expr *expr)
{
	if ((arg_type & (AARG_MINT | AARG_MXMM)) == 0)
	{
		SEMA_ERROR(expr, "An address cannot appear in this slot.");
		return false;
	}
	ExprAsmArg *asm_arg = &expr->expr_asm_arg;
	Expr *base = exprptr(asm_arg->base);
	assert(base->expr_kind == EXPR_ASM);
	ExprAsmArg *base_arg = &base->expr_asm_arg;
	AsmArgGroup base_group = AARG_IREG;
	switch (base_arg->kind)
	{
		case ASM_ARG_REG:
			if (!sema_check_asm_arg(context, block, instr, AARG_IREG, base)) return false;
			base_group = base_arg->reg.ref->type;
			break;
		case ASM_ARG_REGVAR:
			if (!sema_check_asm_arg(context, block, instr, AARG_IREG, base)) return false;
			base_group = sema_ireg_for_type(base_arg->ident.ident_decl->type);
			break;
		case ASM_ARG_ADDROF:
			TODO
			break;
		default:
			SEMA_ERROR(expr, "Expected a register here.");
			return false;
	}
	Expr *index = exprptrzero(asm_arg->idx);

	if (index)
	{
		AsmArgGroup index_group;
		ExprAsmArg *index_arg = &index->expr_asm_arg;
		switch (index_arg->kind)
		{
			case ASM_ARG_REG:
				if (!sema_check_asm_arg(context, block, instr, AARG_IREG, index)) return false;
				index_group = index_arg->reg.ref->type;
				break;
			case ASM_ARG_REGVAR:
				if (!sema_check_asm_arg(context, block, instr, AARG_IREG, index)) return false;
				index_group = sema_ireg_for_type(index_arg->ident.ident_decl->type);
				break;
			default:
				SEMA_ERROR(expr, "Expected a register here.");
				return false;
		}
		if (index_group != base_group)
		{
			SEMA_ERROR(index, "Expected the same register size as for the base value.");
			return false;
		}
	}
	REMINDER("check if addressing mode is supported");
	return true;
}

static inline bool sema_check_asm_arg_reg(SemaContext *context, AsmInlineBlock *block, AsmInstruction *instr, AsmArgGroup arg_type, Expr *expr)
{
	const char *name = expr->expr_asm_arg.reg.name;
	AsmRegister *reg = expr->expr_asm_arg.reg.ref = asm_reg_by_name(name);
	if (!reg)
	{
		SEMA_ERROR(expr, "Expected a valid register name.");
		return false;
	}
	if ((arg_type & reg->type) == 0)
	{
		SEMA_ERROR(expr, "'%s' is not valid in this slot.", reg->name);
		return false;
	}
	if (arg_type & (AARG_W | AARG_CLOB))
	{
		sema_add_clobber(block, reg->clobber_index);
	}
	return true;
}

static inline ExprAsmArg *asm_reg_find_decl(ExprAsmArg **args, Decl *decl, AsmArgKind kind)
{
	foreach(ExprAsmArg *, args)
	{
		if (val->kind == kind && val->ident.ident_decl == decl) return val;
	}
	return NULL;

}
static inline void asm_reg_add_output(AsmInlineBlock *block, ExprAsmArg *arg)
{
	Decl *decl = arg->ident.ident_decl;

	// Check if this is already in the outputs
	ExprAsmArg *out = asm_reg_find_decl(block->output_vars, decl, arg->kind);
	if (out)
	{
		// Just copy
		arg->ident = out->ident;
	}
	else
	{
		// Add a new
		unsigned out_count = vec_size(block->output_vars);
		if (out_count > 0xFFFF) error_exit("Too many output vars.");
		arg->index = out_count;
		vec_add(block->output_vars, arg);

		// Ignore for memvar
		if (arg->kind == ASM_ARG_MEMVAR) return;

		// It might be in the inputs
		ExprAsmArg *in = asm_reg_find_decl(block->input, decl, arg->kind);
		if (in)
		{
			// It is, so make the in a copy.
			in->ident.copy_output = true;
			in->index = arg->index;
		}
	}

}

static inline void asm_add_input(AsmInlineBlock *block, ExprAsmArg *arg)
{
	unsigned in_count = vec_size(block->input);
	if (in_count > 0xFFFF) error_exit("Too many input vars.");
	arg->index = in_count;
	vec_add(block->input, arg);
}

static inline void asm_reg_add_input(AsmInlineBlock *block, ExprAsmArg *arg)
{
	Decl *decl = arg->ident.ident_decl;

	// 1. It might be an output var, if so then copy the index.
	ExprAsmArg *out = arg->kind == ASM_ARG_MEMVAR ? NULL : asm_reg_find_decl(block->output_vars, decl, arg->kind);
	if (out)
	{
		arg->ident.copy_output = true;
		out->ident.copy_output = true;
		arg->index = out->index;
		goto ADD_CLOBBER;
	}

	// 2. Look through existing inputs, if it's found then copy the index.
	ExprAsmArg *in = asm_reg_find_decl(block->input, decl, arg->kind);
	if (in)
	{
		arg->index = in->index;
	}
	else
	{
		asm_add_input(block, arg);
	}
ADD_CLOBBER:;
	foreach(ExprAsmArg *, block->output_vars)
	{
		val->ident.early_clobber = true;
	}
}

static inline bool sema_check_asm_var(SemaContext *context, AsmInlineBlock *block, AsmInstruction *instr, AsmArgGroup arg_type, Expr *expr)
{
	ExprAsmArg *arg = &expr->expr_asm_arg;
	const char *name = arg->ident.name;
	Decl *decl = sema_resolve_symbol(context, name, NULL, expr->span);
	if (!decl) return false;
	assert(arg->kind == ASM_ARG_REGVAR || arg->kind == ASM_ARG_MEMVAR);
	arg->ident.ident_decl = decl;
	if (decl->decl_kind != DECL_VAR)
	{
		SEMA_ERROR(expr, "Expected a global or local variable.");
		return false;
	}
	if (IS_OPTIONAL(decl))
	{
		SEMA_ERROR(expr, "Optional variables are not allowed in asm.");
		return false;
	}
	bool is_write = (arg_type & AARG_W) != 0;
	bool is_read = !is_write || (arg_type & AARG_R) != 0;
	arg->ident.is_input = !is_write;
	if (is_read)
	{
		decl->var.is_read = true;
		if (decl->var.may_not_read)
		{
			SEMA_ERROR(expr, "An 'out' variable may not be read from.");
			return false;
		}
		asm_reg_add_input(block, arg);
	}
	if (is_write)
	{
		decl->var.is_written = true;
		if (decl->var.may_not_write)
		{
			SEMA_ERROR(expr, "An 'in' variable may not be written to.");
			return false;
		}
		asm_reg_add_output(block, arg);
	}
	Type *type = type_flatten(decl->type);
	if (type_is_pointer(type))
	{
		type = type_uptr->canonical;
	}
	if (type_is_integer(type))
	{
		if ((arg_type & AARG_IREG) == 0)
		{
			if (arg_type & AARG_MINT)
			{
				SEMA_ERROR(expr, "You need to pass the variable by address.");
				return false;
			}
			SEMA_ERROR(expr, "An integer variable was not expected here.");
			return false;
		}
		if (!sema_reg_int_suported_type(arg_type, type))
		{
			SEMA_ERROR(expr, "%s is not supported in this position, convert it to a valid type.",
			           type_quoted_error_string(decl->type));
			return false;
		}
		return true;
	}
	if (type_is_float(type))
	{
		if ((arg_type & AARG_XMM) == 0)
		{
			if (arg_type & AARG_MXMM)
			{
				SEMA_ERROR(expr, "You need to pass the variable by address.");
				return false;
			}
			SEMA_ERROR(expr, "An floating point variable was not expected here.");
			return false;
		}
		if (!sema_reg_float_suported_type(arg_type, type))
		{
			SEMA_ERROR(expr, "%s is not supported in this position, convert it to a valid type.",
			           type_quoted_error_string(decl->type));
			return false;
		}
		return true;

	}
	SEMA_ERROR(expr, "%s is not supported as an argument.", type_quoted_error_string(decl->type));
	return false;
}

static inline bool sema_check_asm_arg_value(SemaContext *context, AsmInlineBlock *block, AsmInstruction *instr, AsmArgGroup arg_type, Expr *expr)
{
	Expr *inner = exprptr(expr->expr_asm_arg.expr_id);
	if (!sema_analyse_expr(context, inner)) return false;
	if (expr_is_const_int(inner)) return sema_check_asm_arg_const_int(context, block, instr, arg_type, expr, inner);
	if (arg_type & AARG_W)
	{
		SEMA_ERROR(expr, "This position is written to, you can't use an expression for that.");
		return false;
	}
	Type *type = type_flatten(inner->type);
	if (type_is_pointer(type)) type = type_uptr;
	if (type_is_integer(type))
	{
		if (!sema_reg_int_suported_type(arg_type, type))
		{
			SEMA_ERROR(expr, "%s is not valid for this slot.", type_quoted_error_string(inner->type));
			return false;
		}
		asm_reg_add_input(block, &expr->expr_asm_arg);
		expr->type = type;
		return true;
	}
	if (type_is_float(type))
	{
		if (!sema_reg_float_suported_type(arg_type, type))
		{
			SEMA_ERROR(expr, "%s is not valid for this slot.", type_quoted_error_string(inner->type));
			return false;
		}
		asm_reg_add_input(block, &expr->expr_asm_arg);
		expr->type = type;
		return true;
	}
	TODO
}
static inline bool sema_check_asm_arg(SemaContext *context, AsmInlineBlock *block, AsmInstruction *instr, AsmArgGroup arg_type, Expr *expr)
{
	switch (expr->expr_asm_arg.kind)
	{
		case ASM_ARG_INT:
			return true;
		case ASM_ARG_REG:
			return sema_check_asm_arg_reg(context, block, instr, arg_type, expr);
		case ASM_ARG_ADDR:
			return sema_check_asm_arg_addr(context, block, instr, arg_type, expr);
		case ASM_ARG_VALUE:
			return sema_check_asm_arg_value(context, block, instr, arg_type, expr);
		case ASM_ARG_REGVAR:
		case ASM_ARG_MEMVAR:
			return sema_check_asm_var(context, block, instr, arg_type, expr);
		case ASM_ARG_ADDROF:
			TODO
			break;
	}
	UNREACHABLE
}
bool sema_check_asm(SemaContext *context, AsmInlineBlock *block, Ast *asm_stmt)
{
	if (platform_target.arch != ARCH_TYPE_X86_64)
	{
		SEMA_ERROR(asm_stmt, "Unsupported architecture for asm.");
		return false;
	}
	init_asm();
	AsmInstruction *instr = asm_instr_by_name(asm_stmt->asm_stmt.instruction);
	if (!instr)
	{
		SEMA_ERROR(asm_stmt, "Unknown instruction");
		return false;
	}
	Expr **args = asm_stmt->asm_stmt.args;
	unsigned expected_params = instr->param_count;
	unsigned arg_count = vec_size(args);
	if (expected_params != arg_count)
	{
		SEMA_ERROR(asm_stmt, "Too %s arguments to instruction '%s', expected %d.",
				   expected_params > arg_count ? "few" : "many",
				   instr->name, expected_params);
		return false;
	}
	for (unsigned i = arg_count; i > 0; i--)
	{
		if (!sema_check_asm_arg(context, block, instr, instr->param[i - 1], args[i - 1])) return false;
	}
	sema_add_clobbers(block, &instr->mask);
	//const char *variant = asm_stmt->asm_stmt.variant;
	return true;
}


