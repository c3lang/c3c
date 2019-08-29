// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "compiler_internal.h"

#define COMP_OP(comp) \
switch (left->type)\
{ case CONST_NIL: result->b = 0 comp 0; break;\
case CONST_BOOL: result->b = left->b comp right->b; break;\
case CONST_STRING: UNREACHABLE;\
case CONST_FLOAT: result->b = left->f comp right->f; break;\
case CONST_INT: result->b = left->i comp right->i; break;\
default: UNREACHABLE }\
result->type = CONST_BOOL; expr->expr_kind = EXPR_CONST; expr->type = &type_bool; return true;

#define BOOL_COMP(comp) \
switch (left->type)\
{ case CONST_NIL: result->b = 0 comp 0; break;\
case CONST_BOOL: result->b = left->b comp right->b; break;\
case CONST_STRING: UNREACHABLE;\
case CONST_FLOAT: result->b = (left->f != 0) comp (right->f != 0); break;\
case CONST_INT: result->b = (left->i != 0) comp (right->i != 0); break;\
default: break; }\
result->type = CONST_BOOL; expr->expr_kind = EXPR_CONST; expr->type = &type_bool; return true

#define BITOP(op)\
switch (left->type)\
{ case CONST_NIL: break; \
case CONST_BOOL: result->b = left->b op right->b; break; \
case CONST_STRING: case CONST_FLOAT: UNREACHABLE; \
case CONST_INT: result->b = left->i op right->i; break; \
default: UNREACHABLE; \
} break

#define ARITH(op)\
switch (left->type)\
{ case CONST_NIL: case CONST_BOOL: case CONST_STRING: UNREACHABLE; \
case CONST_FLOAT: result->f = left->f op right->f; break;\
case CONST_INT: result->i = left->i op right->i; break; \
default: UNREACHABLE; \
} break

bool sema_const_fold_binary(Context *context, Expr *expr)
{
	Expr *left_expr = expr->binary_expr.left;
	Expr *right_expr = expr->binary_expr.right;

	if (left_expr->expr_kind != EXPR_CONST || right_expr->expr_kind != EXPR_CONST) return true;
	Type *type = left_expr->type->canonical;
	assert(type == right_expr->type->canonical);
	ExprConst *left = &left_expr->const_expr;
	ExprConst *right = &right_expr->const_expr;
	ExprConst *result = &expr->const_expr;
	switch (expr->binary_expr.operator)
	{
		case BINOP_ERROR:
		case BINOP_ASSIGN:
		case BINOP_MULT_ASSIGN:
		case BINOP_ADD_ASSIGN:
		case BINOP_SUB_ASSIGN:
		case BINOP_DIV_ASSIGN:
		case BINOP_MOD_ASSIGN:
		case BINOP_AND_ASSIGN:
		case BINOP_OR_ASSIGN:
		case BINOP_BIT_AND_ASSIGN:
		case BINOP_BIT_OR_ASSIGN:
		case BINOP_BIT_XOR_ASSIGN:
		case BINOP_SHR_ASSIGN:
		case BINOP_SHL_ASSIGN:
			SEMA_ERROR(expr->loc, "Invalid operation '%s' %s '%s.", left_expr->type->name_loc.string, token_type_to_string(binop_to_token(expr->binary_expr.operator)), right_expr->type->name_loc.string);
			return false;
		case BINOP_MULT:
			ARITH(*);
		case BINOP_ADD:
			ARITH(+);
		case BINOP_SUB:
			ARITH(-);
		case BINOP_DIV:
			switch (left->type)
			{
				case CONST_NIL:
				case CONST_BOOL:
				case CONST_STRING:
					UNREACHABLE;
				case CONST_INT:
					if (right->i == 0)
					{
						SEMA_ERROR(expr->binary_expr.right->loc, "Division by zero not allowed.");
						return false;
					}
					result->i = left->i / right->i;
					break;
				case CONST_FLOAT:
					if (right->f == 0)
					{
						SEMA_ERROR(expr->binary_expr.right->loc, "Division by zero not allowed.");
						return false;
					}
					expr->const_expr.f = left->f / right->f;
					expr->const_expr.type = CONST_FLOAT;
					break;
			}
			break;
		case BINOP_MOD:
			switch (left->type)
			{
				case CONST_NIL:
				case CONST_BOOL:
				case CONST_STRING:
				case CONST_FLOAT:
					UNREACHABLE;
				case CONST_INT:
					if (right->i == 0)
					{
						SEMA_ERROR(expr->binary_expr.right->loc, "Cannot do mod by zero.");
						return false;
					}
					result->i = left->i % right->i;
					break;
			}
			break;
		case BINOP_AND:
			BOOL_COMP(&&);
		case BINOP_OR:
			BOOL_COMP(||);
		case BINOP_BIT_AND:
			BITOP(&);
		case BINOP_BIT_OR:
			BITOP(|);
		case BINOP_BIT_XOR:
			BITOP(^);
		case BINOP_NE:
			COMP_OP(!=)
		case BINOP_EQ:
			COMP_OP(==)
		case BINOP_GE:
			COMP_OP(>=)
		case BINOP_GT:
			COMP_OP(>)
		case BINOP_LE:
			COMP_OP(<=)
		case BINOP_LT:
			COMP_OP(<)
		case BINOP_SHR:
			TODO
			break;
		case BINOP_SHL:
			TODO
			break;
		case BINOP_ELVIS:
			TODO
			break;
	}
	expr->type = type;
	expr->expr_kind = EXPR_CONST;
	return true;
}
