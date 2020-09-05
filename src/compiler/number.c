// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"
#include "bigint.h"

#define CHECK_SI_KIND(_kind) assert(_kind >= TYPE_I8 && _kind <= TYPE_I64)
#define CHECK_IXX_KIND(_kind) assert(_kind == TYPE_IXX)
#define CHECK_UI_KIND(_kind) assert(_kind >= TYPE_U8 && _kind <= TYPE_U64)
#define CHECK_INT_KIND(_kind) assert(_kind >= TYPE_I8 && _kind <= TYPE_U64)
#define CHECK_CONVERSION(_kind) assert(i->kind != _kind && "Unnecessary conversion")
#define TYPE_MATCH assert(left->kind == right->kind && left != res && right != res)

static uint64_t type_mask[TYPE_U64 + 1] = {
		[TYPE_U8] = 0xFF,
		[TYPE_I8] = 0xFF,
		[TYPE_U16] = 0xFFFF,
		[TYPE_I16] = 0xFFFF,
		[TYPE_U32] = 0xFFFFFFFFU,
		[TYPE_I32] = 0xFFFFFFFFU,
		[TYPE_U64] = 0xFFFFFFFFFFFFFFFFLLU,
		[TYPE_I64] = 0xFFFFFFFFFFFFFFFFLLU,
};

static int type_bits[TYPE_U64 + 1] = {
		[TYPE_U8] = 8,
		[TYPE_I8] = 8,
		[TYPE_U16] = 16,
		[TYPE_I16] = 16,
		[TYPE_U32] = 32,
		[TYPE_I32] = 32,
		[TYPE_U64] = 64,
		[TYPE_I64] = 64,
};


void expr_const_fprint(FILE *__restrict file, ExprConst *expr)
{
	switch (expr->kind)
	{
		case TYPE_POINTER:
			fprintf(file, "null");
			break;
		case TYPE_BOOL:
			fprintf(file, expr->b ? "true" : "false");
			break;
		case TYPE_I8:
		case TYPE_I16:
		case TYPE_I32:
		case TYPE_I64:
		case TYPE_U8:
		case TYPE_U16:
		case TYPE_U32:
		case TYPE_U64:
		case TYPE_IXX:
			bigint_fprint(file, &expr->i, 10);
			break;
		case TYPE_F32:
		case TYPE_F64:
		case TYPE_FXX:
			fprintf(file, "%Lf", expr->f);
			break;
		case TYPE_ENUM:
			fprintf(file, "%s", expr->enum_constant->name);
			break;
		case TYPE_STRING:
			fprintf(file, "%.*s", expr->string.len, expr->string.chars);
			break;
		default:
			UNREACHABLE
	}

}

void expr_const_set_int(ExprConst *expr, uint64_t v, TypeKind kind)
{
	if (type_kind_is_signed(kind))
	{
		bigint_init_signed(&expr->i, (int64_t)v);
	}
	else
	{
		bigint_init_unsigned(&expr->i, v);
	}
	expr->kind = kind;
}

void expr_const_set_bool(ExprConst *expr, bool b)
{
	expr->b = b;
	expr->kind = TYPE_BOOL;
}

void expr_const_set_null(ExprConst *expr)
{
	expr->i.digit_count = 0;
	expr->i.digit = 0;
	expr->kind = TYPE_POINTER;
}

static inline bool compare_bool(bool left, bool right, BinaryOp op)
{
	switch (op)
	{
		case BINARYOP_EQ:
			return left == right;
		case BINARYOP_NE:
			return left != right;
		case BINARYOP_GT:
			return left > right;
		case BINARYOP_LT:
			return left < right;
		case BINARYOP_GE:
			return left >= right;
		case BINARYOP_LE:
			return left <= right;
		default:
			UNREACHABLE
	}
}

static inline bool compare_ints(const BigInt *left, const BigInt *right, BinaryOp op)
{
	CmpRes res = bigint_cmp(left, right);
	switch (op)
	{
		case BINARYOP_GE:
			return res != CMP_LT;
		case BINARYOP_LE:
			return res != CMP_GT;
		case BINARYOP_NE:
			return res != CMP_EQ;
		case BINARYOP_GT:
			return res == CMP_GT;
		case BINARYOP_LT:
			return res == CMP_LT;
		case BINARYOP_EQ:
			return res == CMP_EQ;
		default:
			UNREACHABLE
	}
}

static inline bool compare_fps(long double left, long double right, BinaryOp op)
{
	switch (op)
	{
		case BINARYOP_GE:
			return left >= right;
		case BINARYOP_LE:
			return left <= right;
		case BINARYOP_NE:
			return left != right;
		case BINARYOP_GT:
			return left > right;
		case BINARYOP_LT:
			return left < right;
		case BINARYOP_EQ:
			return left == right;
		default:
			UNREACHABLE
	}
}

bool expr_const_compare(const ExprConst *left, const ExprConst *right, BinaryOp op)
{
	bool is_eq;
	switch (left->kind)
	{
		case TYPE_BOOL:
			return compare_bool(left->b, right->b, op);
		case ALL_INTS:
			return compare_ints(&left->i, &right->i, op);
		case ALL_FLOATS:
			return compare_fps(left->f, right->f, op);
		case TYPE_POINTER:
			return true;
		case TYPE_STRING:
			if (left->string.len != right->string.len)
			{
				is_eq = false;
				break;
			}
			if (right->string.chars == left->string.chars)
			{
				is_eq = true;
				break;
			}
			is_eq = strncmp(left->string.chars, right->string.chars, left->string.len);
			break;
		default:
			UNREACHABLE
	}
	assert((op == BINARYOP_EQ) || (op == BINARYOP_NE));
	return (op == BINARYOP_EQ) && is_eq;
}

bool expr_const_int_overflowed(const ExprConst *expr)
{
	switch (expr->kind)
	{
		case TYPE_I8:
			return !bigint_fits_in_bits(&expr->i, 8, true);
		case TYPE_I16:
			return !bigint_fits_in_bits(&expr->i, 16, true);
		case TYPE_I32:
			return !bigint_fits_in_bits(&expr->i, 32, true);
		case TYPE_I64:
			return !bigint_fits_in_bits(&expr->i, 64, true);
		case TYPE_U8:
			return !bigint_fits_in_bits(&expr->i, 8, false);
		case TYPE_U16:
			return !bigint_fits_in_bits(&expr->i, 16, false);
		case TYPE_U32:
			return !bigint_fits_in_bits(&expr->i, 32, false);
		case TYPE_U64:
			return !bigint_fits_in_bits(&expr->i, 64, false);
		case TYPE_IXX:
			return false;
		default:
			UNREACHABLE
	}
}
const char *expr_const_to_error_string(const ExprConst *expr)
{
	char *buff = NULL;
	switch (expr->kind)
	{
		case TYPE_POINTER:
			return "null";
		case TYPE_BOOL:
			return expr->b ? "true" : "false";
		case TYPE_I8:
		case TYPE_I16:
		case TYPE_I32:
		case TYPE_I64:
		case TYPE_U8:
		case TYPE_U16:
		case TYPE_U32:
		case TYPE_U64:
		case TYPE_IXX:
			return bigint_to_error_string(&expr->i, 10);
		case TYPE_F32:
		case TYPE_F64:
		case TYPE_FXX:
			asprintf(&buff, "%Lf", expr->f);
			return buff;
		case TYPE_ENUM:
			asprintf(&buff, "%s.%s", expr->enum_constant->type->name, expr->enum_constant->name);
			return buff;
		case TYPE_STRING:
			asprintf(&buff, "\"%*.s\"", expr->string.len, expr->string.chars);
			return buff;
		default:
			UNREACHABLE
	}
}


void expr_const_set_float(ExprConst *expr, long double d, TypeKind kind)
{
	switch (kind)
	{
		case TYPE_F32:
			expr->f = (float)d;
			break;
		case TYPE_F64:
			expr->f = (double)d;
			break;
		default:
			expr->f = d;
	}
	expr->kind = kind;
}



