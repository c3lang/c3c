// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"

#define EXIT_T_MISMATCH() return sema_type_mismatch(left, canonical, cast_type)


static inline void insert_cast(Expr *expr, CastKind kind, Type *canonical)
{
	Expr *inner = malloc_arena(sizeof(Expr));
	assert(expr->resolve_status == RESOLVE_DONE);
	assert(expr->type);
	assert(canonical->canonical == canonical);
	*inner = *expr;
	expr->expr_kind = EXPR_CAST;
	expr->cast_expr.kind = kind;
	expr->cast_expr.expr = inner;
	expr->cast_expr.type_info = NULL;
	expr->type = canonical;
}

static bool sema_type_mismatch(Expr *expr, Type *type, CastType cast_type)
{
	const char *action = "";
	switch (cast_type)
	{
		case CAST_TYPE_EXPLICIT:
			action = "cast";
			break;
		case CAST_TYPE_IMPLICIT:
			action = "implicitly cast";
			break;
		case CAST_TYPE_IMPLICIT_ASSIGN:
		case CAST_TYPE_IMPLICIT_ASSIGN_ADD:
			action = "assign";
			break;

	}
	SEMA_ERROR(expr->loc, "Cannot %s '%s' to '%s'", action, type_to_error_string(expr->type), type_to_error_string(type));
	return false;
}


bool erro(Expr* left, Type *from, Type *canonical, Type *type, CastType cast_type)
{
	EXIT_T_MISMATCH();
}

bool ptxi(Expr* left, Type *from, Type *canonical, Type *type, CastType cast_type)
{
	if (cast_type != CAST_TYPE_EXPLICIT) EXIT_T_MISMATCH();
	if (left->expr_kind == EXPR_CONST)
	{
		assert(left->const_expr.type == CONST_NIL);
		left->const_expr.type = sign_from_type(canonical);
		left->const_expr.i = 0;
		left->type = type;
		return true;
	}
	insert_cast(left, CAST_PTRXI, canonical);
	return true;
}

bool ptbo(Expr* left, Type *ignored, Type *canonical, Type *type, CastType cast_type)
{
	if (cast_type >= CAST_TYPE_IMPLICIT_ASSIGN) EXIT_T_MISMATCH();
	if (left->expr_kind == EXPR_CONST)
	{
		assert(left->const_expr.type == CONST_NIL);
		left->const_expr.type = CONST_BOOL;
		left->const_expr.b = false;
		left->type = type;
		return true;
	}
	insert_cast(left, CAST_PTRBOOL, canonical);
	return true;
}

static inline bool may_implicitly_cast_ptr_to_ptr(Type *current_type, Type *target_type)
{
	assert(current_type->canonical == current_type);
	assert(target_type->canonical == target_type);

	// Neither is void* or have matching bases:
	if (target_type->pointer != type_void && current_type->pointer != type_void && target_type->pointer != current_type->pointer) return false;

	return true;
}


bool ptfu(Expr* left, Type *from_canonical, Type *canonical, Type *type, CastType cast_type)
{
	TODO
}

bool fupt(Expr* left, Type *from_canonical, Type *canonical, Type *type, CastType cast_type)
{
	TODO
}

bool stst(Expr* left, Type *from_canonical, Type *canonical, Type *type, CastType cast_type)
{
	TODO
}

bool unst(Expr* left, Type *from_canonical, Type *canonical, Type *type, CastType cast_type)
{
	TODO
}

bool stun(Expr* left, Type *from_canonical, Type *canonical, Type *type, CastType cast_type)
{
	TODO
}

bool unun(Expr* left, Type *from_canonical, Type *canonical, Type *type, CastType cast_type)
{
	TODO
}

bool arpt(Expr* left, Type *from_canonical, Type *canonical, Type *type, CastType cast_type)
{
	TODO
}

bool arsa(Expr* left, Type *from_canonical, Type *canonical, Type *type, CastType cast_type)
{
	TODO
}

bool ptpt(Expr* left, Type *from_canonical, Type *canonical, Type *type, CastType cast_type)
{
	if (cast_type != CAST_TYPE_EXPLICIT && !may_implicitly_cast_ptr_to_ptr(from_canonical, canonical))
	{
		return sema_type_mismatch(left, type, cast_type);
	}
	if (left->expr_kind == EXPR_CONST)
	{
		assert(left->const_expr.type == CONST_NIL);
		left->type = type;
		return true;
	}
	insert_cast(left, CAST_PTRPTR, canonical);
	return true;
}

bool strpt(Expr* left, Type *from_canonical, Type *canonical, Type *type, CastType cast_type)
{
	// TODO
	insert_cast(left, CAST_PTRPTR, canonical);
	return true;
}

bool stpt(Expr* left, Type *from, Type *canonical, Type *type, CastType cast_type)
{
	if (canonical->pointer != type_char && canonical->pointer != type_byte)
	{
		return sema_type_mismatch(left, type, cast_type);
	}
	left->type = canonical;
	return true;
}


bool boxi(Expr* left, Type *from, Type *canonical, Type *type, CastType cast_type)
{
	if (cast_type >= CAST_TYPE_IMPLICIT_ASSIGN) EXIT_T_MISMATCH();
	if (left->expr_kind == EXPR_CONST)
	{
		assert(left->const_expr.type == CONST_BOOL);
		left->const_expr.type = CONST_INT;
		left->const_expr.i = left->const_expr.b ? 1 : 0;
		left->type = type;
		return true;
	}
	insert_cast(left, CAST_BOOLINT, canonical);
	return true;
}

bool bofp(Expr* left, Type *from, Type *canonical, Type *type, CastType cast_type)
{
	if (cast_type >= CAST_TYPE_IMPLICIT_ASSIGN) EXIT_T_MISMATCH();
	if (left->expr_kind == EXPR_CONST)
	{
		assert(left->const_expr.type == CONST_NIL);
		left->const_expr.type = CONST_FLOAT;
		left->const_expr.f = left->const_expr.b ? 1.0 : 0.0;
		left->type = type;
		return true;
	}
	insert_cast(left, CAST_BOOLFP, canonical);
	return true;
}

bool xibo(Expr* left, Type *from, Type *canonical, Type *type, CastType cast_type)
{
	if (cast_type >= CAST_TYPE_IMPLICIT_ASSIGN) EXIT_T_MISMATCH();
	if (left->expr_kind == EXPR_CONST)
	{
		assert(left->const_expr.type == CONST_INT);
		left->const_expr.type = CONST_BOOL;
		left->const_expr.b = left->const_expr.i != 0;
		left->type = type;
		return true;
	}
	insert_cast(left, CAST_INTBOOL, canonical);
	return true;
}

bool fpbo(Expr* left, Type *from, Type *canonical, Type *type, CastType cast_type)
{
	if (cast_type >= CAST_TYPE_IMPLICIT_ASSIGN) EXIT_T_MISMATCH();
	if (left->expr_kind == EXPR_CONST)
	{
		assert(left->const_expr.type == CONST_FLOAT);
		left->const_expr.type = CONST_BOOL;
		left->const_expr.b = left->const_expr.f != 0;
		left->type = type;
		return true;
	}
	insert_cast(left, CAST_INTBOOL, canonical);
	return true;
}


bool fpfp(Expr* left, Type *from, Type *canonical, Type *type, CastType cast_type)
{
	bool is_narrowing = from->builtin.bytesize < canonical->builtin.bytesize && from->type_kind != TYPE_FXX;

	// Is this correct? TODO
	if (is_narrowing && cast_type == CAST_TYPE_IMPLICIT_ASSIGN) EXIT_T_MISMATCH();

	if (left->expr_kind == EXPR_CONST)
	{
		assert(left->const_expr.type == CONST_FLOAT);
		if (type->type_kind == TYPE_F32)
		{
			left->const_expr.f = (float)left->const_expr.f;
		}
		left->type = type;
		return true;
	}
	insert_cast(left, CAST_FPFP, canonical);
	return true;
}

bool fpui(Expr* left, Type *from, Type *canonical, Type *type, CastType cast_type)
{
	if (cast_type != CAST_TYPE_EXPLICIT) EXIT_T_MISMATCH();
	if (left->expr_kind == EXPR_CONST)
	{
		assert(left->const_expr.type == CONST_FLOAT);
		assert(canonical->type_kind >= TYPE_U8 && canonical->type_kind <= TYPE_U64);
		left->const_expr.i = (uint64_t)left->const_expr.f;
		left->type = type;
		return true;
	}
	insert_cast(left, CAST_FPUI, canonical);
	return true;
}

bool fpsi(Expr* left, Type *from, Type *canonical, Type *type, CastType cast_type)
{
	if (cast_type != CAST_TYPE_EXPLICIT) EXIT_T_MISMATCH();
	if (left->expr_kind == EXPR_CONST)
	{
		assert(left->const_expr.type == CONST_FLOAT);
		assert(canonical->type_kind >= TYPE_I8 && canonical->type_kind <= TYPE_I64);
		left->const_expr.i = (uint64_t)((int64_t)left->const_expr.f);
		left->type = type;
		return true;
	}
	insert_cast(left, CAST_FPUI, canonical);
	return true;
}

static uint64_t type_mask[4] = { 0xFF, 0xFFFF, 0xFFFFFFFFU, 0xFFFFFFFFFFFFFFFFLLU};
static int64_t int_type_max[4] = { INT8_MAX, INT16_MAX, INT32_MAX, INT64_MAX };
static uint64_t uint_type_max[4] = { UINT8_MAX, UINT16_MAX, UINT32_MAX, UINT64_MAX };
static int64_t int_type_min[4] = { INT8_MIN, INT16_MIN, INT32_MIN, INT64_MIN };

bool sisi(Expr* left, Type *from, Type *canonical, Type *type, CastType cast_type)
{
	bool is_narrowing = from->builtin.bytesize > canonical->builtin.bytesize && from->type_kind != TYPE_IXX;

	if (is_narrowing && (cast_type != CAST_TYPE_IMPLICIT_ASSIGN_ADD && cast_type != CAST_TYPE_EXPLICIT)) EXIT_T_MISMATCH();
	if (left->expr_kind == EXPR_CONST)
	{
		int64_t i = (int64_t)left->const_expr.i;
		int index = canonical->type_kind - TYPE_I8;
		if (from->type_kind == TYPE_IXX)
		{
			if ((i > int_type_max[index]) || (i < int_type_min[index]))
			{
				SEMA_ERROR(left->loc, "'%lld' does not fit into '%s'", i, canonical->name);
				return false;
			}
		}
		assert(left->const_expr.type == CONST_INT);
		assert(canonical->type_kind >= TYPE_I8 && canonical->type_kind <= TYPE_I64);
		uint64_t mask = type_mask[index];
		if (i < 0)
		{
			left->const_expr.i = ~((~((uint64_t)i)) & mask);
		}
		else
		{
			left->const_expr.i &= mask;
		}
		left->type = type;
		return true;
	}
	assert(from->type_kind != TYPE_IXX);
	insert_cast(left, CAST_SISI, canonical);
	return true;
}

bool siui(Expr* left, Type *from, Type *canonical, Type *type, CastType cast_type)
{
	bool is_narrowing = from->builtin.bytesize < canonical->builtin.bytesize && from->type_kind != TYPE_IXX;
	if (is_narrowing && (cast_type != CAST_TYPE_IMPLICIT_ASSIGN_ADD && cast_type != CAST_TYPE_EXPLICIT)) EXIT_T_MISMATCH();
	if (left->expr_kind == EXPR_CONST)
	{
		assert(left->const_expr.type == CONST_INT);
		assert(canonical->type_kind >= TYPE_U8 && canonical->type_kind <= TYPE_U64);
		left->const_expr.i &= type_mask[canonical->type_kind - TYPE_U8];
		left->type = type;
		return true;
	}
	insert_cast(left, CAST_SIUI, canonical);
	return true;
}

bool xiptr(Expr* left, Type *from, Type *canonical, Type *type)
{
	if (left->expr_kind == EXPR_CONST)
	{
		assert(left->const_expr.type == CONST_INT);
		assert(canonical->type_kind >= TYPE_U8 && canonical->type_kind <= TYPE_U64);
		assert(left->const_expr.i == 0);
		left->const_expr.type = CONST_NIL;
		left->type = type;
		return true;
	}
	insert_cast(left, CAST_XIPTR, canonical);
	return true;
}

bool sifp(Expr* left, Type *from, Type *canonical, Type *type, CastType cast_type)
{
	// TODO
	if (left->expr_kind == EXPR_CONST)
	{
		assert(left->const_expr.type == CONST_INT);
		assert(canonical->type_kind >= TYPE_F32 && canonical->type_kind <= TYPE_FXX);
		if (left->const_expr.i > (uint64_t)INT64_MAX)
		{
			left->const_expr.f = -((long double)(~left->const_expr.i));
		}
		else
		{
			left->const_expr.f = left->const_expr.i;
		}
		left->const_expr.type = CONST_FLOAT;
		left->type = type;
		return true;
	}
	insert_cast(left, CAST_SIFP, canonical);
	return true;
}


bool uisi(Expr* left, Type *from, Type *canonical, Type *type, CastType cast_type)
{
	if (left->expr_kind == EXPR_CONST)
	{
		assert(canonical->type_kind >= TYPE_I8 && canonical->type_kind <= TYPE_I64);
		int index = canonical->type_kind - TYPE_I8;
		uint64_t mask = type_mask[index];
		if (cast_type != CAST_TYPE_EXPLICIT && from->type_kind == TYPE_UXX)
		{
			if (left->const_expr.i > (uint64_t)int_type_max[index])
			{
				SEMA_ERROR(left->loc, "Cannot implicitly convert value '%llu' into %s - it will not fit.", left->const_expr.i, type_to_error_string(type));
				return false;
			}
		}
		if (left->const_expr.i > (uint64_t)INT64_MAX)
		{
			left->const_expr.i = ~((~left->const_expr.i) & mask);
		}
		else
		{
			left->const_expr.i &= mask;
		}
		left->type = type;
		return true;
	}
	insert_cast(left, CAST_UISI, canonical);
	return true;
}

bool uiui(Expr* left, Type *from, Type *canonical, Type *type, CastType cast_type)
{
	if (left->expr_kind == EXPR_CONST)
	{
		assert(left->const_expr.type == CONST_INT && type_is_unsigned(from));
		assert(canonical->type_kind >= TYPE_U8 && canonical->type_kind <= TYPE_U64);
		int index = canonical->type_kind - TYPE_U8;
		if (cast_type != CAST_TYPE_EXPLICIT && from->type_kind == TYPE_UXX)
		{
			if (left->const_expr.i > uint_type_max[index])
			{
				SEMA_ERROR(left->loc, "Cannot implicitly convert value '%llu' into %s - it will not fit.", type_to_error_string(type));
				return false;
			}
		}
		left->const_expr.i &= type_mask[index];
		left->type = type;
		return true;
	}
	insert_cast(left, CAST_UIUI, canonical);
	return true;
}


bool uifp(Expr* left, Type *from, Type *canonical, Type *type, CastType cast_type)
{
	TODO
	if (left->expr_kind == EXPR_CONST)
	{
		assert(left->const_expr.type == CONST_INT && type_is_unsigned(from));
		assert(canonical->type_kind >= TYPE_F32 && canonical->type_kind <= TYPE_F64);
		left->const_expr.f = left->const_expr.i;
		left->type = type;
		return true;
	}
	insert_cast(left, CAST_UIFP, canonical);
	return true;
}

bool ussi(Expr* left, Type *from, Type *canonical, Type *type, CastType cast_type)
{
	if (type->type_kind != TYPE_ENUM) return sema_type_mismatch(left, type, CAST_TYPE_EXPLICIT);
	if (cast_type != CAST_TYPE_EXPLICIT) EXIT_T_MISMATCH();

	if (left->expr_kind == EXPR_IDENTIFIER && left->identifier_expr.decl->decl_kind == DECL_ENUM_CONSTANT)
	{
		// TODO
		Expr *value = left->identifier_expr.decl->enum_constant.expr;
		assert(value->expr_kind == EXPR_CONST);
		assert(value->const_expr.type == CONST_INT);
		left->const_expr.i = value->const_expr.i;
		// TODO narrowing
	}
	insert_cast(left, CAST_ENUMSI, canonical);
	return true;
}

bool sius(Expr* left, Type *from, Type *canonical, Type *type, CastType cast_type)
{
	TODO
}

bool uius(Expr* left, Type *from, Type *canonical, Type *type, CastType cast_type)
{
	TODO
}

bool xipt(Expr* left, Type *from, Type *canonical, Type *type, CastType cast_type)
{
	TODO
}

bool usus(Expr* left, Type *from, Type *canonical, Type *type, CastType cast_type)
{
	assert(canonical->canonical == canonical);
	assert(canonical->type_kind == TYPE_POINTER);
	assert(from->type_kind == TYPE_POINTER);

	if (cast_type != CAST_TYPE_EXPLICIT)
	{
		if (type_is_subtype(from->pointer, canonical->pointer))
		{
			insert_cast(left, CAST_PTRPTR, canonical);
			return true;
		}
		sema_type_mismatch(left, type, cast_type);
		return false;
	}
	insert_cast(left, CAST_PTRPTR, canonical);
	return true;
}

bool enxi(Expr* left, Type *from, Type *canonical, Type *type, CastType cast_type)
{
	TODO
}
bool erxi(Expr* left, Type *from, Type *canonical, Type *type, CastType cast_type)
{
	TODO
}

bool vava(Expr* left, Type *from, Type *canonical, Type *type, CastType cast_type)
{
	TODO
}

bool sapt(Expr* left, Type *from, Type *canonical, Type *type, CastType cast_type)
{
	TODO
}

bool vasa(Expr* left, Type *from, Type *canonical, Type *type, CastType cast_type)
{
	TODO
}

bool usui(Expr* left, Type *from, Type *canonical, Type *type, CastType cast_type)
{
	TODO
}

bool ptva(Expr* left, Type *from, Type *canonical, Type *type, CastType cast_type)
{
	TODO
}

bool usbo(Expr* left, Type *from, Type *canonical, Type *type, CastType cast_type)
{
	TODO
}

bool vapt(Expr* left, Type *from, Type *canonical, Type *type, CastType cast_type)
{
	TODO
}



bool cast_to_runtime(Expr *expr)
{
	Type *canonical = expr->type->canonical;
	switch (canonical->type_kind)
	{
		case TYPE_IXX:
			return cast(expr, type_long, CAST_TYPE_IMPLICIT);
		case TYPE_UXX:
			return cast(expr, type_ulong, CAST_TYPE_IMPLICIT);
		case TYPE_FXX:
			return cast(expr, type_double, CAST_TYPE_IMPLICIT);
		default:
			return true;
	}
}

bool cast_implicit(Expr *expr, Type *to_type)
{
	if (!to_type) return true;
	return cast(expr, to_type, CAST_TYPE_IMPLICIT);
}

CastKind cast_to_bool_kind(Type *type)
{
	switch (type->type_kind)
	{
		case TYPE_TYPEDEF:
			return cast_to_bool_kind(type->canonical);
		case TYPE_POISONED:
		case TYPE_VOID:
		case TYPE_ERROR_UNION:
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_STRING:
		case TYPE_ERROR:
		case TYPE_ENUM:
		case TYPE_FUNC:
		case TYPE_ARRAY:
		case TYPE_VARARRAY:
		case TYPE_SUBARRAY:
			// Improve consider vararray / subarray conversion to boolean.
			return CAST_ERROR;
		case TYPE_BOOL:
			UNREACHABLE
		case TYPE_I8:
		case TYPE_I16:
		case TYPE_I32:
		case TYPE_I64:
		case TYPE_IXX:
		case TYPE_U8:
		case TYPE_U16:
		case TYPE_U32:
		case TYPE_U64:
		case TYPE_UXX:
			return CAST_INTBOOL;
		case TYPE_F32:
		case TYPE_F64:
		case TYPE_FXX:
			return CAST_FPBOOL;
		case TYPE_POINTER:
			return CAST_PTRBOOL;
	}
	UNREACHABLE
}

bool cast(Expr *expr, Type *to_type, CastType cast_type)
{
	Type *from_type = expr->type->canonical;
	Type *canonical = to_type->canonical;
	if (from_type == canonical) return true;
	switch (from_type->type_kind)
	{
		case TYPE_POISONED:
		case TYPE_VOID:
			break;
		case TYPE_BOOL:
			if (type_is_integer(canonical)) return boxi(expr, from_type, canonical, to_type, cast_type);
			if (type_is_float(canonical)) return bofp(expr, from_type, canonical, to_type, cast_type);
			break;
		case TYPE_ERROR_UNION:
			TODO
		case TYPE_I8:
		case TYPE_I16:
		case TYPE_I32:
		case TYPE_I64:
		case TYPE_IXX:
			if (type_is_unsigned_integer(canonical)) return siui(expr, from_type, canonical, to_type, cast_type);
			if (type_is_signed_integer(canonical)) return sisi(expr, from_type, canonical, to_type, cast_type);
			if (type_is_float(canonical)) return sifp(expr, from_type, canonical, to_type, cast_type);
			if (canonical == type_bool) return xibo(expr, from_type, canonical, to_type, cast_type);
			if (canonical->type_kind == TYPE_POINTER) return ptxi(expr, from_type, canonical, to_type, cast_type);
			break;
		case TYPE_U8:
		case TYPE_U16:
		case TYPE_U32:
		case TYPE_U64:
		case TYPE_UXX:
			if (type_is_unsigned_integer(canonical)) return uiui(expr, from_type, canonical, to_type, cast_type);
			if (type_is_signed_integer(canonical)) return uisi(expr, from_type, canonical, to_type, cast_type);
			if (type_is_float(canonical)) return uifp(expr, from_type, canonical, to_type, cast_type);
			if (canonical == type_bool) return xibo(expr, from_type, canonical, to_type, cast_type);
			if (canonical->type_kind == TYPE_POINTER) return ptxi(expr, from_type, canonical, to_type, cast_type);
			break;
		case TYPE_F32:
		case TYPE_F64:
		case TYPE_FXX:
			if (type_is_unsigned_integer(canonical)) return fpui(expr, from_type, canonical, to_type, cast_type);
			if (type_is_signed_integer(canonical)) return fpsi(expr, from_type, canonical, to_type, cast_type);
			if (canonical == type_bool) return fpbo(expr, from_type, canonical, to_type, cast_type);
			if (type_is_float(canonical)) return fpfp(expr, from_type, canonical, to_type, cast_type);
			break;
		case TYPE_POINTER:
			if (type_is_integer(canonical)) return ptxi(expr, from_type, canonical, to_type, cast_type);
			if (canonical->type_kind == TYPE_BOOL) return ptbo(expr, from_type, canonical, to_type, cast_type);
			if (canonical->type_kind == TYPE_POINTER) return ptpt(expr, from_type, canonical, to_type, cast_type);
			if (canonical->type_kind == TYPE_FUNC) return ptfu(expr, from_type, canonical, to_type, cast_type);
			if (canonical->type_kind == TYPE_VARARRAY) return ptva(expr, from_type, canonical, to_type, cast_type);
			break;
		case TYPE_ENUM:
			if (type_is_integer(canonical)) return enxi(expr, from_type, canonical, to_type, cast_type);
			break;
		case TYPE_ERROR:
			if (type_is_integer(canonical)) return erxi(expr, from_type, canonical, to_type, cast_type);
			break;
		case TYPE_FUNC:
			if (type_is_integer(canonical)) return ptxi(expr, from_type, canonical, to_type, cast_type);
			if (canonical->type_kind == TYPE_POINTER) return fupt(expr, from_type, canonical, to_type, cast_type);
			break;
		case TYPE_STRUCT:
			if (canonical->type_kind == TYPE_STRUCT) return stst(expr, from_type, canonical, to_type, cast_type);
			if (canonical->type_kind == TYPE_UNION) return stun(expr, from_type, canonical, to_type, cast_type);
			break;
		case TYPE_UNION:
			if (canonical->type_kind == TYPE_STRUCT) return unst(expr, from_type, canonical, to_type, cast_type);
			if (canonical->type_kind == TYPE_UNION) return unun(expr, from_type, canonical, to_type, cast_type);
			break;
		case TYPE_TYPEDEF:
			UNREACHABLE
		case TYPE_STRING:
			if (canonical->type_kind == TYPE_POINTER) return strpt(expr, from_type, canonical, to_type, cast_type);
			break;
		case TYPE_ARRAY:
			// There is no valid cast from array to anything else.
			break;
		case TYPE_VARARRAY:
			if (canonical->type_kind == TYPE_SUBARRAY) return vasa(expr, from_type, canonical, to_type, cast_type);
			if (canonical->type_kind == TYPE_VARARRAY) return vava(expr, from_type, canonical, to_type, cast_type);
			if (canonical->type_kind == TYPE_POINTER) return vapt(expr, from_type, canonical, to_type, cast_type);
			break;
		case TYPE_SUBARRAY:
			if (canonical->type_kind == TYPE_POINTER) return sapt(expr, from_type, canonical, to_type, cast_type);
			break;
	}
	return sema_type_mismatch(expr, canonical, cast_type);
}
