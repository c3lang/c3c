// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "llvm_codegen_internal.h"
#include "compiler_internal.h"

LLVMValueRef gencontext_emit_address(GenContext *context, Expr *expr)
{
	switch (expr->expr_kind)
	{
		case EXPR_IDENTIFIER:
			return expr->identifier_expr.decl->var.backend_ref;
		case EXPR_CONST:
		case EXPR_TYPE:
			UNREACHABLE
		case EXPR_UNARY:
		{
			UnaryOp op = unaryop_from_token(expr->unary_expr.operator);
			assert(op == UNARYOP_DEREF);
			return gencontext_emit_expr(context, expr->unary_expr.expr);
		}
		case EXPR_ACCESS:
			TODO;
		{
		//	LLVMValueRef value = gencontext_emit_address(context, expr->access_expr.parent);
		//	LLVMBuildExtractValue(context->builder, value, (unsigned)expr->access_expr.index, expr->access_expr.ref->name.string);
		}
		case EXPR_POISONED:
		case EXPR_TRY:
		case EXPR_SIZEOF:
			UNREACHABLE
		case EXPR_BINARY:
			TODO;
		case EXPR_CONDITIONAL:
			TODO;
		case EXPR_POST_UNARY:
			TODO;
		case EXPR_TYPE_ACCESS:
			TODO
		case EXPR_CALL:
			TODO
		case EXPR_SUBSCRIPT:
			TODO
		case EXPR_STRUCT_VALUE:
			TODO
		case EXPR_STRUCT_INIT_VALUES:
			TODO
		case EXPR_INITIALIZER_LIST:
			TODO
		case EXPR_EXPRESSION_LIST:
			TODO
		case EXPR_CAST:
			TODO
	}
	return NULL;
}

static inline LLVMValueRef gencontext_emit_cast_expr(GenContext *context, Expr *expr)
{
	LLVMValueRef rhs = gencontext_emit_expr(context, expr->expr_cast.expr);
	switch (expr->expr_cast.kind)
	{
		case CAST_ERROR:
			UNREACHABLE
		case CAST_PTRPTR:
			return LLVMBuildPointerCast(context->builder, rhs, BACKEND_TYPE(expr->type), "ptrptr");
		case CAST_PTRXI:
			return LLVMBuildPtrToInt(context->builder, rhs, BACKEND_TYPE(expr->type), "ptrxi");
		case CAST_VARRPTR:
			TODO
		case CAST_ARRPTR:
			TODO
		case CAST_STRPTR:
			TODO
		case CAST_PTRBOOL:
			return LLVMBuildICmp(context->builder, LLVMIntNE, rhs, LLVMConstPointerNull(BACKEND_TYPE(expr->type->canonical->pointer)), "ptrbool");
		case CAST_BOOLINT:
			return LLVMBuildTrunc(context->builder, rhs, BACKEND_TYPE(expr->type), "boolsi");
		case CAST_FPBOOL:
			return LLVMBuildFCmp(context->builder, LLVMRealUNE, rhs, LLVMConstNull(LLVMTypeOf(rhs)), "fpbool");
		case CAST_BOOLFP:
			return LLVMBuildSIToFP(context->builder, rhs, BACKEND_TYPE(expr->type), "boolfp");
		case CAST_INTBOOL:
			return LLVMBuildICmp(context->builder, LLVMIntNE, rhs, LLVMConstNull(LLVMTypeOf(rhs)), "intbool");
		case CAST_FPFP:
			return type_convert_will_trunc(expr->type, expr->expr_cast.expr->type)
				? LLVMBuildFPTrunc(context->builder, rhs, BACKEND_TYPE(expr->type), "fpfptrunc")
				: LLVMBuildFPExt(context->builder, rhs, BACKEND_TYPE(expr->type), "fpfpext");
		case CAST_FPSI:
			return LLVMBuildFPToSI(context->builder, rhs, BACKEND_TYPE(expr->type), "fpsi");
		case CAST_FPUI:
			return LLVMBuildFPToUI(context->builder, rhs, BACKEND_TYPE(expr->type), "fpui");
		case CAST_SISI:
			return type_convert_will_trunc(expr->type, expr->expr_cast.expr->type)
			       ? LLVMBuildTrunc(context->builder, rhs, BACKEND_TYPE(expr->type), "sisitrunc")
			       : LLVMBuildSExt(context->builder, rhs, BACKEND_TYPE(expr->type), "sisiext");
		case CAST_SIUI:
			return type_convert_will_trunc(expr->type, expr->expr_cast.expr->type)
			       ? LLVMBuildTrunc(context->builder, rhs, BACKEND_TYPE(expr->type), "siuitrunc")
			       : LLVMBuildZExt(context->builder, rhs, BACKEND_TYPE(expr->type), "siuiext");
			break;
		case CAST_SIFP:
			return LLVMBuildSIToFP(context->builder, rhs, BACKEND_TYPE(expr->type), "sifp");
		case CAST_XIPTR:
			return LLVMBuildIntToPtr(context->builder, rhs, BACKEND_TYPE(expr->type), "xiptr");
		case CAST_UISI:
			return type_convert_will_trunc(expr->type, expr->expr_cast.expr->type)
			       ? LLVMBuildTrunc(context->builder, rhs, BACKEND_TYPE(expr->type), "uisitrunc")
			       : LLVMBuildZExt(context->builder, rhs, BACKEND_TYPE(expr->type), "uisiext");
		case CAST_UIUI:
			return type_convert_will_trunc(expr->type, expr->expr_cast.expr->type)
			       ? LLVMBuildTrunc(context->builder, rhs, BACKEND_TYPE(expr->type), "uiuitrunc")
			       : LLVMBuildZExt(context->builder, rhs, BACKEND_TYPE(expr->type), "uiuiext");
		case CAST_UIFP:
			return LLVMBuildUIToFP(context->builder, rhs, BACKEND_TYPE(expr->type), "uifp");
		case CAST_ENUMSI:
			TODO
	}
}

LLVMValueRef gencontext_emit_unary_expr(GenContext *context, Expr *expr)
{
	UnaryOp unary_op = unaryop_from_token(expr->unary_expr.operator);
	switch (unary_op)
	{
		case UNARYOP_ERROR:
			FATAL_ERROR("Illegal unary op %s", expr->unary_expr.operator);
		case UNARYOP_NOT:
			return LLVMBuildXor(context->builder, gencontext_emit_expr(context, expr->unary_expr.expr), LLVMConstInt(type_bool->backend_type, 1, 0), "not");
		case UNARYOP_BITNEG:
			return LLVMBuildNot(context->builder, gencontext_emit_expr(context, expr->unary_expr.expr), "bnot");
		case UNARYOP_NEG:
			if (type_is_float(expr->unary_expr.expr->type->canonical))
			{
				return LLVMBuildFNeg(context->builder, gencontext_emit_expr(context, expr->unary_expr.expr), "fneg");
			}
			return LLVMBuildNeg(context->builder, gencontext_emit_expr(context, expr->unary_expr.expr), "neg");
		case UNARYOP_ADDR:
			return gencontext_emit_address(context, expr->unary_expr.expr);
		case UNARYOP_DEREF:
			return LLVMBuildLoad(context->builder, gencontext_emit_expr(context, expr->unary_expr.expr), "deref");
		case UNARYOP_INC:
			TODO
		case UNARYOP_DEC:
			TODO
	}
}



static LLVMValueRef gencontext_emit_assign(GenContext *context, Expr *left, LLVMValueRef right)
{
	LLVMValueRef addr = gencontext_emit_address(context, left);
	return LLVMBuildStore(context->builder, right, addr);
}

static LLVMValueRef gencontext_emit_logical_and_or(GenContext *context, Expr *expr, BinaryOp op)
{
	// Value *ScalarExprEmitter::VisitBinLAnd(const BinaryOperator *E)
	// For vector implementation.

	// Set up basic blocks, following Cone
	LLVMBasicBlockRef start_block = LLVMGetInsertBlock(context->builder);
	LLVMBasicBlockRef phi_block = LLVMCreateBasicBlockInContext(context->context, op == BINARYOP_AND ? "and.phi" : "or.phi");
	LLVMBasicBlockRef rhs_block = LLVMCreateBasicBlockInContext(context->context, op == BINARYOP_AND ? "and.rhs" : "or.rhs");

	// Generate left-hand condition and conditional branch
	LLVMValueRef lhs = gencontext_emit_expr(context, expr->binary_expr.left);

	if (op == BINARYOP_AND)
	{
		gencontext_emit_cond_br(context, lhs, rhs_block, phi_block);
	}
	else
	{
		gencontext_emit_cond_br(context, lhs, phi_block, rhs_block);
	}

	gencontext_emit_block(context, rhs_block);
	LLVMValueRef rhs = gencontext_emit_expr(context, expr->binary_expr.right);
	gencontext_emit_br(context, phi_block);

			// Generate phi
	gencontext_emit_block(context, phi_block);
	LLVMValueRef phi = LLVMBuildPhi(context->builder, type_bool->backend_type, "val");

	// Simplify for LLVM by entering the constants we already know of.
	LLVMValueRef result_on_skip = LLVMConstInt(LLVMInt1TypeInContext(context->context), op == BINARYOP_AND ? 0 : 1, false);
	LLVMValueRef logicValues[2] = { result_on_skip, rhs };
	LLVMBasicBlockRef blocks[2] = { start_block, rhs_block };
	LLVMAddIncoming(phi, logicValues, blocks, 2);

	return phi;
}

static LLVMValueRef gencontext_emit_binary(GenContext *context, Expr *expr, bool load_lhs_after_rhs, BinaryOp binary_op)
{

	if (binary_op == BINARYOP_AND || binary_op == BINARYOP_OR)
	{
		return gencontext_emit_logical_and_or(context, expr, binary_op);
	}
	Type *type = expr->type->canonical;
	Expr *lhs = expr->binary_expr.left;
	Expr *rhs = expr->binary_expr.right;

	LLVMValueRef lhs_value;
	LLVMValueRef rhs_value;
	if (load_lhs_after_rhs)
	{
		rhs_value = gencontext_emit_expr(context, rhs);
		lhs_value = gencontext_emit_expr(context, lhs);
	}
	else
	{
		lhs_value = gencontext_emit_expr(context, lhs);
		rhs_value = gencontext_emit_expr(context, rhs);
	}

	bool is_float = type_is_float(type);

	switch (binary_op)
	{
		case BINARYOP_ERROR:
			UNREACHABLE
		case BINARYOP_MULT:
			return is_float ? LLVMBuildFMul(context->builder, lhs_value, rhs_value, "fmul") : LLVMBuildMul(context->builder, lhs_value, rhs_value, "mul");
		case BINARYOP_SUB:
			if (lhs->type->canonical->type_kind == TYPE_POINTER)
			{
				if (lhs->type->canonical == rhs->type->canonical) return LLVMBuildPtrDiff(context->builder, lhs_value, rhs_value, "ptrdiff");
				rhs_value = LLVMBuildNeg(context->builder, rhs_value, "");
				return LLVMBuildGEP2(context->builder, BACKEND_TYPE(lhs->type), lhs_value, &rhs_value, 1, "ptrsub");
			}
			if (is_float) return LLVMBuildFSub(context->builder, lhs_value, rhs_value, "fsub");
			// Consider UB version instead.
			return LLVMBuildSub(context->builder, lhs_value, rhs_value, "sub");
		case BINARYOP_ADD:
			if (lhs->type->canonical->type_kind == TYPE_POINTER)
			{
				assert(type_is_integer(lhs->type->canonical));
				return LLVMBuildGEP2(context->builder, BACKEND_TYPE(lhs->type), lhs_value, &rhs_value, 1, "ptradd");
			}
			if (is_float) return LLVMBuildFAdd(context->builder, lhs_value, rhs_value, "fadd");
			// Consider UB version instead.
			return LLVMBuildAdd(context->builder, lhs_value, rhs_value, "add");
		case BINARYOP_DIV:
			if (is_float) return LLVMBuildFDiv(context->builder, lhs_value, rhs_value, "fdiv");
			return type_is_unsigned(type)
				? LLVMBuildUDiv(context->builder, lhs_value, rhs_value, "udiv")
				: LLVMBuildSDiv(context->builder, lhs_value, rhs_value, "sdiv");
		case BINARYOP_MOD:
			return type_is_unsigned(type)
				? LLVMBuildURem(context->builder, lhs_value, rhs_value, "umod")
				: LLVMBuildSRem(context->builder, lhs_value, rhs_value, "smod");
		case BINARYOP_SHR:
			return type_is_unsigned(type)
				? LLVMBuildLShr(context->builder, lhs_value, rhs_value, "lshr")
				: LLVMBuildAShr(context->builder, lhs_value, rhs_value, "ashr");
		case BINARYOP_SHL:
			return LLVMBuildShl(context->builder, lhs_value, rhs_value, "shr");
		case BINARYOP_BIT_AND:
			return LLVMBuildAnd(context->builder, lhs_value, rhs_value, "and");
		case BINARYOP_BIT_OR:
			return LLVMBuildOr(context->builder, lhs_value, rhs_value, "or");
		case BINARYOP_BIT_XOR:
			return LLVMBuildXor(context->builder, lhs_value, rhs_value, "xor");
		case BINARYOP_EQ:
			// Unordered?
			if (type_is_float(type)) LLVMBuildFCmp(context->builder, LLVMRealUEQ, lhs_value, rhs_value, "eq");
			return LLVMBuildICmp(context->builder, LLVMIntEQ, lhs_value, rhs_value, "eq");
		case BINARYOP_NE:
			// Unordered?
			if (type_is_float(type)) LLVMBuildFCmp(context->builder, LLVMRealUNE, lhs_value, rhs_value, "neq");
			return LLVMBuildICmp(context->builder, LLVMIntNE, lhs_value, rhs_value, "neq");
		case BINARYOP_GE:
			if (type_is_float(type)) LLVMBuildFCmp(context->builder, LLVMRealUGE, lhs_value, rhs_value, "ge");
			return type_is_unsigned(type)
				? LLVMBuildICmp(context->builder, LLVMIntUGE, lhs_value, rhs_value, "ge")
				: LLVMBuildICmp(context->builder, LLVMIntSGE, lhs_value, rhs_value, "ge");
		case BINARYOP_GT:
			if (type_is_float(type)) LLVMBuildFCmp(context->builder, LLVMRealUGT, lhs_value, rhs_value, "gt");
			return type_is_unsigned(type)
			       ? LLVMBuildICmp(context->builder, LLVMIntUGT, lhs_value, rhs_value, "gt")
			       : LLVMBuildICmp(context->builder, LLVMIntSGT, lhs_value, rhs_value, "gt");
		case BINARYOP_LE:
			if (type_is_float(type)) LLVMBuildFCmp(context->builder, LLVMRealULE, lhs_value, rhs_value, "le");
			return type_is_unsigned(type)
			       ? LLVMBuildICmp(context->builder, LLVMIntULE, lhs_value, rhs_value, "le")
			       : LLVMBuildICmp(context->builder, LLVMIntSLE, lhs_value, rhs_value, "le");
		case BINARYOP_LT:
			if (type_is_float(type)) LLVMBuildFCmp(context->builder, LLVMRealULE, lhs_value, rhs_value, "lt");
			return type_is_unsigned(type)
			       ? LLVMBuildICmp(context->builder, LLVMIntULT, lhs_value, rhs_value, "lt")
			       : LLVMBuildICmp(context->builder, LLVMIntSLT, lhs_value, rhs_value, "lt");
		case BINARYOP_AND:
		case BINARYOP_OR:
			UNREACHABLE
		case BINARYOP_ASSIGN:
		case BINARYOP_MULT_ASSIGN:
		case BINARYOP_ADD_ASSIGN:
		case BINARYOP_SUB_ASSIGN:
		case BINARYOP_DIV_ASSIGN:
		case BINARYOP_MOD_ASSIGN:
		case BINARYOP_AND_ASSIGN:
		case BINARYOP_OR_ASSIGN:
		case BINARYOP_BIT_AND_ASSIGN:
		case BINARYOP_BIT_OR_ASSIGN:
		case BINARYOP_BIT_XOR_ASSIGN:
		case BINARYOP_SHR_ASSIGN:
		case BINARYOP_SHL_ASSIGN:
			UNREACHABLE
	}
}

LLVMValueRef gencontext_emit_post_unary_expr(GenContext *context, Expr *expr)
{
	Expr *lhs = expr->unary_expr.expr;
	LLVMValueRef value = gencontext_emit_expr(context, lhs);
	bool is_add = expr->post_expr.operator == TOKEN_PLUSPLUS;
/*	if (expr->type->canonical->type_kind == TYPE_POINTER)
	{
		LLVMValueRef offset = LLVMConstInt(is_add ? type_isize->backend_type : type_usize->backend_type, is_add ? 1 : -1, true);
		LLVMBuildStore(context->builder, LLVMBuildGEP2(context->builder, gencontext_get_llvm_type(context, expr->type->canonical), value, &offset, 1, "postunary"), gencontext_emit_address(context, left);)
		return ;
	}
	if (type_is_float(expr->type->canonical))
	{
		LLVMValueRef offset = LLVMConstReal(LLVMTypeOf(value), is_add ? 1);
		LLVMBuildAdd(context->builder, value, offset, name);
	}
	if (lhs->type->canonical->type_kind == TYPE_POINTER)
	{
		rhs_value = LLVMBuildNeg(context->builder, rhs_value, "");
		return LLVMBuildGEP2(context->builder, , lhs_value, &rhs_value, 1, "ptrsub");
	}
 */
	const char *name = is_add ? "add" : "sub";
	LLVMValueRef constVal = LLVMConstInt(LLVMTypeOf(value), 1, !is_add);
	LLVMValueRef result = is_add ? LLVMBuildAdd(context->builder, value, constVal, name)
	                             : LLVMBuildSub(context->builder, value, constVal, name);
	LLVMBuildStore(context->builder, result, gencontext_emit_address(context, lhs));
	return value;
}

static LLVMValueRef gencontext_emit_binary_expr(GenContext *context, Expr *expr)
{
	BinaryOp binary_op = binaryop_from_token(expr->binary_expr.operator);
	if (binary_op > BINARYOP_ASSIGN)
	{
		BinaryOp base_op = binaryop_assign_base_op(binary_op);
		assert(base_op != BINARYOP_ERROR);
		LLVMValueRef value = gencontext_emit_binary(context, expr, true, base_op);
		gencontext_emit_assign(context, expr->binary_expr.left, value);
		return value;
	}
	if (binary_op == BINARYOP_ASSIGN)
	{
		LLVMValueRef value = gencontext_emit_expr(context, expr->binary_expr.right);
		gencontext_emit_assign(context, expr->binary_expr.left, value);
		return value;
	}
	return gencontext_emit_binary(context, expr, false, binary_op);
}

static LLVMValueRef gencontext_emit_identifier_expr(GenContext *context, Expr *expr)
{
	return LLVMBuildLoad2(context->builder, expr->identifier_expr.decl->type->canonical->backend_type,
			expr->identifier_expr.decl->var.backend_ref, expr->identifier_expr.decl->name.string);
}

LLVMValueRef gencontext_emit_const_expr(GenContext *context, Expr *expr)
{
	LLVMTypeRef type = BACKEND_TYPE(expr->type);
	switch (expr->const_expr.type)
	{
		case CONST_INT:
			return LLVMConstInt(type, expr->const_expr.i, type_is_unsigned(expr->type->canonical) ? false : true);
		case CONST_FLOAT:
			return LLVMConstReal(type, (double) expr->const_expr.f);
		case CONST_NIL:
			return LLVMConstNull(type);
		case CONST_BOOL:
			return LLVMConstInt(type, expr->const_expr.b ? 1 : 0, false);
		case CONST_STRING:
		{
			LLVMValueRef global_name = LLVMAddGlobal(context->module, type, "string");
			LLVMSetLinkage(global_name, LLVMInternalLinkage);
			LLVMSetGlobalConstant(global_name, 1);
			LLVMSetInitializer(global_name, LLVMConstStringInContext(context->context,
			                                                         expr->const_expr.string.chars,
			                                                         expr->const_expr.string.len,
			                                                         1));
			return global_name;
		}
	}
	UNREACHABLE
}

LLVMValueRef gencontext_emit_call_expr(GenContext *context, Expr *expr)
{
	size_t args = vec_size(expr->call_expr.arguments);
	LLVMValueRef *values = args ? malloc_arena(args * sizeof(LLVMValueRef)) : NULL;
	VECEACH(expr->call_expr.arguments, i)
	{
		values[i] = gencontext_emit_expr(context, expr->call_expr.arguments[i]);
	}
	LLVMValueRef func = expr->call_expr.function->identifier_expr.decl->func.backend_value;

	return LLVMBuildCall2(context->builder, LLVMTYPE(expr->call_expr.function->identifier_expr.decl->type), func, values, args, "call");
	/*
	if (fndcl->flags & FlagSystem) {
		LLVMSetInstructionCallConv(fncallret, LLVMX86StdcallCallConv);
	}*/
}
LLVMValueRef gencontext_emit_expr(GenContext *context, Expr *expr)
{
	switch (expr->expr_kind)
	{
		case EXPR_POISONED:
			UNREACHABLE
		case EXPR_UNARY:
			return gencontext_emit_unary_expr(context, expr);
		case EXPR_TRY:
			break;
		case EXPR_CONST:
			return gencontext_emit_const_expr(context, expr);
		case EXPR_BINARY:
			return gencontext_emit_binary_expr(context, expr);
		case EXPR_CONDITIONAL:
			break;
		case EXPR_POST_UNARY:
			return gencontext_emit_post_unary_expr(context, expr);
		case EXPR_TYPE:
			break;
		case EXPR_IDENTIFIER:
			return gencontext_emit_identifier_expr(context, expr);
		case EXPR_TYPE_ACCESS:
			break;
		case EXPR_CALL:
			return gencontext_emit_call_expr(context, expr);
		case EXPR_SIZEOF:
			break;
		case EXPR_SUBSCRIPT:
			break;
		case EXPR_ACCESS:
			break;
		case EXPR_STRUCT_VALUE:
			break;
		case EXPR_STRUCT_INIT_VALUES:
			break;
		case EXPR_INITIALIZER_LIST:
			break;
		case EXPR_EXPRESSION_LIST:
			break;
		case EXPR_CAST:
			return gencontext_emit_cast_expr(context, expr);
	}
	TODO
}