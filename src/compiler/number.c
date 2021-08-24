// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"

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
	switch (expr->const_kind)
	{
		case CONST_POINTER:
			fprintf(file, "null");
			break;
		case CONST_BOOL:
			fprintf(file, expr->b ? "true" : "false");
			break;
		case CONST_INTEGER:
			bigint_fprint(file, &expr->i, 10);
			break;
		case CONST_FLOAT:
#if LONG_DOUBLE
			fprintf(file, "%Lg", expr->f);
#else
			fprintf(file, "%g", expr->f);
#endif
			break;
		case CONST_STRING:
			fprintf(file, "%.*s", expr->string.len, expr->string.chars);
			break;
		case CONST_BYTES:
			fprintf(file, "[byte data]");
			break;
		case CONST_ENUM:
			fprintf(file, "%s", expr->enum_val->name);
			break;
		case CONST_ERR:
			fprintf(file, "%s", expr->err_val->name);
			break;
		case CONST_TYPEID:
			fprintf(file, "%s", expr->typeid->name);
			break;

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
	expr->const_kind = CONST_INTEGER;
	expr->int_type = kind;
}

void expr_const_set_bool(ExprConst *expr, bool b)
{
	expr->b = b;
	expr->const_kind = CONST_BOOL;
}

void expr_const_set_null(ExprConst *expr)
{
	expr->i.digit_count = 0;
	expr->i.digit = 0;
	expr->const_kind = CONST_POINTER;
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

static inline bool compare_fps(Real left, Real right, BinaryOp op)
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
	switch (left->const_kind)
	{
		case CONST_BOOL:
			return compare_bool(left->b, right->b, op);
		case CONST_INTEGER:
			return compare_ints(&left->i, &right->i, op);
		case CONST_FLOAT:
			return compare_fps(left->f, right->f, op);
		case CONST_POINTER:
			return true;
		case CONST_STRING:
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
			is_eq = !strncmp(left->string.chars, right->string.chars, left->string.len);
			break;
		case CONST_TYPEID:
			return left->typeid == right->typeid;
		case CONST_ERR:
		{
			Decl *left_decl = left->err_val;
			// The error case
			uint64_t right_ordinal;
			assert(right->const_kind == CONST_ERR);
			Decl *right_decl = right->err_val;
			// Non matching cannot be compared.
			if (right_decl->type != left_decl->type) return false;
			right_ordinal = right->err_val->enum_constant.ordinal;
			switch (op)
			{
				case BINARYOP_GT:
					return left_decl->enum_constant.ordinal > right_ordinal;
				case BINARYOP_GE:
					return left_decl->enum_constant.ordinal >= right_ordinal;
				case BINARYOP_LT:
					return left_decl->enum_constant.ordinal < right_ordinal;
				case BINARYOP_LE:
					return left_decl->enum_constant.ordinal <= right_ordinal;
				case BINARYOP_NE:
					return left_decl->enum_constant.ordinal != right_ordinal;
				case BINARYOP_EQ:
					return left_decl->enum_constant.ordinal == right_ordinal;
				default:
					UNREACHABLE
			}
		}
		case CONST_ENUM:
			{
				Decl *left_decl = left->enum_val;
				// The enum case
				Expr *left_const = left_decl->enum_constant.expr;
				assert(left_const->expr_kind == EXPR_CONST);
				const ExprConst *right_const = right;
				if (right->const_kind == CONST_ENUM)
				{
					assert(right->enum_val->enum_constant.expr->expr_kind == EXPR_CONST);
					right_const = &right->enum_val->enum_constant.expr->const_expr;
				}
				return expr_const_compare(&left_const->const_expr, right_const, op);
			}
		case CONST_BYTES:
			if (left->bytes.len != right->bytes.len)
			{
				is_eq = false;
				break;
			}
			if (right->bytes.ptr == left->bytes.ptr)
			{
				is_eq = true;
				break;
			}
			is_eq = !memcmp(left->bytes.ptr, right->bytes.ptr, left->bytes.len);
			break;
		default:
			UNREACHABLE
	}
	assert((op == BINARYOP_EQ) || (op == BINARYOP_NE));
	return (op == BINARYOP_EQ) && is_eq;
}

bool expr_const_will_overflow(const ExprConst *expr, TypeKind kind)
{
	switch (kind)
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
			return expr->i.is_negative || !bigint_fits_in_bits(&expr->i, 8, false);
		case TYPE_U16:
			return expr->i.is_negative || !bigint_fits_in_bits(&expr->i, 16, false);
		case TYPE_U32:
			return expr->i.is_negative || !bigint_fits_in_bits(&expr->i, 32, false);
		case TYPE_U64:
			return expr->i.is_negative || !bigint_fits_in_bits(&expr->i, 64, false);
		case TYPE_F16:
			return !bigint_fits_in_bits(&expr->i, 17, false);
		case TYPE_IXX:
		case TYPE_F32:
		case TYPE_F64:
		case TYPE_F128:
		case TYPE_FXX:
		case TYPE_BOOL:
			return false;
		default:
			UNREACHABLE
	}
}

bool expr_const_int_overflowed(const ExprConst *expr)
{
	return expr_const_will_overflow(expr, expr->int_type);
}
const char *expr_const_to_error_string(const ExprConst *expr)
{
	char *buff = NULL;
	switch (expr->const_kind)
	{
		case CONST_POINTER:
			return "null";
		case CONST_BOOL:
			return expr->b ? "true" : "false";
		case CONST_INTEGER:
			return bigint_to_error_string(&expr->i, 10);
		case CONST_FLOAT:
#if LONG_DOUBLE
			asprintf(&buff, "%Lg", expr->f);
#else
			asprintf(&buff, "%g", expr->f);
#endif
			return buff;
		case CONST_STRING:
			asprintf(&buff, "\"%*.s\"", expr->string.len, expr->string.chars);
			return buff;
		case CONST_BYTES:
			return "<binary data>";
		case CONST_ENUM:
			return expr->enum_val->name;
		case CONST_ERR:
			return expr->err_val->name;
		case CONST_TYPEID:
			return expr->typeid->name;
	}
	UNREACHABLE
}


void expr_const_set_float(ExprConst *expr, Real d, TypeKind kind)
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
	expr->const_kind = CONST_FLOAT;
	expr->float_type = kind;
}



