// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"
#include "bigint.h"

#define EXIT_T_MISMATCH() return sema_type_mismatch(left, canonical, cast_type)
#define IS_EXPLICT()
#define RETURN_NON_CONST_CAST(kind) do { if (left->expr_kind != EXPR_CONST) { insert_cast(left, kind, canonical); return true; } } while (0)
#define REQUIRE_EXPLICIT_CAST(_cast_type)\
  do { if (_cast_type == CAST_TYPE_EXPLICIT) break;\
  if (_cast_type == CAST_TYPE_OPTIONAL_IMPLICIT) return true;\
  EXIT_T_MISMATCH(); } while (0)

static inline void insert_cast(Expr *expr, CastKind kind, Type *canonical)
{
	assert(expr->resolve_status == RESOLVE_DONE);
	assert(expr->type);
	assert(canonical->canonical == canonical);
	Expr *inner = COPY(expr);
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
		case CAST_TYPE_OPTIONAL_IMPLICIT:
			UNREACHABLE
	}
	Type *expr_type = expr->type;
	if (expr_type == expr_type->canonical)
	{
		if (type->canonical == type)
		{
			SEMA_ERROR(expr, "Cannot %s '%s' to '%s'.", action, type_to_error_string(expr_type), type_to_error_string(type));
		}
		else
		{
			SEMA_ERROR(expr, "Cannot %s '%s' to '%s' ('%s').", action, type_to_error_string(expr_type), type_to_error_string(type), type_to_error_string(type->canonical));
		}
	}
	else
	{
		if (type->canonical == type)
		{
			SEMA_ERROR(expr, "Cannot %s '%s' (%s) to '%s'.", action, type_to_error_string(expr_type), type_to_error_string(expr_type->canonical), type_to_error_string(type));
		}
		else
		{
			SEMA_ERROR(expr, "Cannot %s '%s' (%s) to '%s' ('%s').", action, type_to_error_string(expr_type), type_to_error_string(expr_type->canonical), type_to_error_string(type), type_to_error_string(type->canonical));
		}
	}
	return false;
}


bool erro(Expr* left, Type *from, Type *canonical, Type *type, CastType cast_type)
{
	EXIT_T_MISMATCH();
}

bool ptxi(Expr *left, Type *canonical, Type *type, CastType cast_type)
{
	REQUIRE_EXPLICIT_CAST(cast_type);

	RETURN_NON_CONST_CAST(CAST_PTRXI);
	assert(left->const_expr.kind == TYPE_POINTER);
	expr_const_set_int(&left->const_expr, 0, type->type_kind);
	left->type = type;

	return true;
}


bool ptbo(Expr *left, Type *canonical, Type *type, CastType cast_type)
{
	RETURN_NON_CONST_CAST(CAST_PTRBOOL);

	assert(left->const_expr.kind == TYPE_POINTER);
	left->const_expr.b = false;

	left->type = type;

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
		if (cast_type == CAST_TYPE_OPTIONAL_IMPLICIT) return true;
		return sema_type_mismatch(left, type, cast_type);
	}
	RETURN_NON_CONST_CAST(CAST_PTRPTR);
	assert(left->const_expr.kind == TYPE_POINTER);
	left->type = type;
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

void const_int_to_fp_cast(Expr *left, Type *canonical, Type *type)
{
	long double f = bigint_as_float(&left->const_expr.i);
	switch (canonical->type_kind)
	{
		case TYPE_F32:
			left->const_expr.f = (float)f;
			break;
		case TYPE_F64:
			left->const_expr.f = (double)f;
			break;
		default:
			left->const_expr.f = f;
			break;
	}
	left->type = type;
	left->const_expr.kind = canonical->type_kind;
}

/**
 * Bool into a signed or unsigned int. Explicit conversions only.
 * @return true unless this is not an explicit conversion.
 */
bool boxi(Expr *left, Type *canonical, Type *type, CastType cast_type)
{
	if (cast_type == CAST_TYPE_OPTIONAL_IMPLICIT) return true;
	if (cast_type != CAST_TYPE_EXPLICIT) EXIT_T_MISMATCH();
	RETURN_NON_CONST_CAST(CAST_BOOLINT);
	assert(left->const_expr.kind == TYPE_BOOL);
	expr_const_set_int(&left->const_expr, left->const_expr.b ? 1 : 0, canonical->type_kind);
	left->type = type;
	return true;
}

/**
 * Bool into a float. Explicit conversions only.
 * @return true unless this is not an explicit conversion.
 */
bool bofp(Expr *left, Type *canonical, Type *type, CastType cast_type)
{
//	if (cast_type >= CAST_TYPE_IMPLICIT_ASSIGN) EXIT_T_MISMATCH();
	RETURN_NON_CONST_CAST(CAST_BOOLFP);

	assert(left->const_expr.kind == TYPE_BOOL);
	expr_const_set_float(&left->const_expr, left->const_expr.b ? 1.0 : 0.0, canonical->type_kind);
	left->type = type;
	return true;
}

/**
 * Convert from any into to bool.
 * @return true for any implicit conversion except assign and assign add.
 */
bool xibo(Expr *left, Type *canonical, Type *type, CastType cast_type)
{
	if (cast_type == CAST_TYPE_OPTIONAL_IMPLICIT) return true;
//	if (cast_type >= CAST_TYPE_IMPLICIT_ASSIGN) EXIT_T_MISMATCH();
	RETURN_NON_CONST_CAST(CAST_INTBOOL);

	expr_const_set_bool(&left->const_expr, bigint_cmp_zero(&left->const_expr.i) != CMP_EQ);
	left->type = type;
	return true;
}

/**
 * Convert from any float to bool
 * @return true for any implicit conversion except assign and assign add
 */
bool fpbo(Expr *left, Type *canonical, Type *type, CastType cast_type)
{
//	if (cast_type >= CAST_TYPE_IMPLICIT_ASSIGN) EXIT_T_MISMATCH();
	RETURN_NON_CONST_CAST(CAST_FPBOOL);

	expr_const_set_bool(&left->const_expr, left->const_expr.f != 0.0);
	left->type = type;
	return true;
}


/**
 * Convert from any fp to fp
 * @return true for all except implicit assign (but allowing assign add)
 */
bool fpfp(Expr* left, Type *from, Type *canonical, Type *type, CastType cast_type)
{
	bool is_narrowing = from->builtin.bytesize < canonical->builtin.bytesize && from->type_kind != TYPE_FXX;

//	if (is_narrowing && cast_type == CAST_TYPE_IMPLICIT_ASSIGN) EXIT_T_MISMATCH();

	RETURN_NON_CONST_CAST(CAST_FPFP);

	expr_const_set_float(&left->const_expr, left->const_expr.f, canonical->type_kind);
	left->type = type;
	return true;
}

/**
 * Convert from any floating point to int
 * @return true only on explicit conversions.
 */
bool fpxi(Expr *left, Type *canonical, Type *type, CastType cast_type)
{
	REQUIRE_EXPLICIT_CAST(cast_type);

	RETURN_NON_CONST_CAST(CAST_FPUI);

	assert(canonical->type_kind >= TYPE_I8 && canonical->type_kind <= TYPE_U64);
	long double d = left->const_expr.f;
	BigInt temp;
	if (canonical->type_kind >= TYPE_U8)
	{
		bigint_init_unsigned(&temp, (uint64_t)d);
		bigint_truncate(&left->const_expr.i, &temp, canonical->builtin.bitsize, false);
	}
	else
	{
		bigint_init_signed(&temp, (int64_t)d);
		bigint_truncate(&left->const_expr.i, &temp, canonical->builtin.bitsize, true);
	}
	left->const_expr.kind = canonical->type_kind;
	left->type = type;
	return true;
}


/**
 * Convert from compile time int to any signed or unsigned int
 * @return true unless the conversion was lossy.
 */
bool ixxxi(Expr *left, Type *canonical, Type *type, CastType cast_type)
{
	bool is_signed = canonical->type_kind < TYPE_U8;
	int bitsize = canonical->builtin.bitsize;
	if (cast_type != CAST_TYPE_EXPLICIT && !bigint_fits_in_bits(&left->const_expr.i, bitsize, is_signed))
	{
		if (cast_type == CAST_TYPE_OPTIONAL_IMPLICIT) return true;
		SEMA_ERROR(left, "'%s' does not fit into '%s'", expr_const_to_error_string(&left->const_expr), canonical->name);
		return false;
	}
	BigInt temp;
	bigint_truncate(&temp, &left->const_expr.i, canonical->builtin.bitsize, is_signed);
	left->const_expr.i = temp;
	left->const_expr.kind = canonical->type_kind;
	left->type = type;
	return true;
}

/**
 * Convert from compile time int to any signed or unsigned int
 * @return true unless the conversion was lossy.
 */
bool ixxen(Expr *left, Type *canonical, Type *type, CastType cast_type)
{
	assert(canonical->type_kind == TYPE_ENUM);
	canonical = canonical->decl->enums.type_info->type->canonical;
	return ixxxi(left, canonical, type, cast_type);
}

/**
 * Convert from compile time int to error union
 */
bool ixxeu(Expr *left, Type *type)
{
	UNREACHABLE
}

/**
 * Cast signed int -> signed int
 * @return true if this is a widening, an explicit cast or if it is an implicit assign add
 */
bool sisi(Expr* left, Type *from_canonical, Type *canonical, Type *type, CastType cast_type)
{
	bool is_narrowing = from_canonical->builtin.bytesize > canonical->builtin.bytesize;

	if (is_narrowing && cast_type != CAST_TYPE_EXPLICIT)
	{
		if (cast_type == CAST_TYPE_OPTIONAL_IMPLICIT) return true;
		EXIT_T_MISMATCH();
	}

	RETURN_NON_CONST_CAST(CAST_SISI);

	BigInt temp;
	bigint_truncate(&temp, &left->const_expr.i, canonical->builtin.bitsize, true);
	left->const_expr.i = temp;
	left->const_expr.kind = canonical->type_kind;
	left->type = type;
	return true;
}

/**
 * Cast unsigned int -> unsigned int
 * @return true if this was not a narrowing implicit assign or narrowing implicit assign add
 */
bool uiui(Expr* left, Type *from_canonical, Type *canonical, Type *type, CastType cast_type)
{
	bool is_narrowing = from_canonical->builtin.bytesize > canonical->builtin.bytesize;

	if (is_narrowing && cast_type != CAST_TYPE_EXPLICIT)
	{
		if (cast_type == CAST_TYPE_OPTIONAL_IMPLICIT) return true;
		EXIT_T_MISMATCH();
	}

	RETURN_NON_CONST_CAST(CAST_UIUI);

	BigInt temp;
	bigint_truncate(&temp, &left->const_expr.i, canonical->builtin.bitsize, false);
	left->const_expr.i = temp;
	left->const_expr.kind = canonical->type_kind;
	left->type = type;
	return true;
}


/**
 * Cast unsigned int -> signed int
 * @return true if this is an explicit cast or if it is an implicit assign add or if it is a widening cast.
 */
bool uisi(Expr* left, Type *from_canonical, Type *canonical, Type *type, CastType cast_type)
{
	bool is_widening = from_canonical->builtin.bytesize < canonical->builtin.bytesize;

	if (!is_widening && cast_type != CAST_TYPE_EXPLICIT)
	{
		if (cast_type == CAST_TYPE_OPTIONAL_IMPLICIT) return true;
		EXIT_T_MISMATCH();
	}

	RETURN_NON_CONST_CAST(CAST_UISI);

	BigInt temp;
	bigint_truncate(&temp, &left->const_expr.i, canonical->builtin.bitsize, true);
	left->const_expr.i = temp;
	left->const_expr.kind = canonical->type_kind;
	left->type = type;
	return true;
}

/*
 * Cast signed int -> unsigned int
 * @return true if this was an implicit add or or explicit cast.
 */
bool siui(Expr *left, Type *canonical, Type *type, CastType cast_type)
{
	REQUIRE_EXPLICIT_CAST(cast_type);

	RETURN_NON_CONST_CAST(CAST_SIUI);

	BigInt temp;
	bigint_truncate(&temp, &left->const_expr.i, canonical->builtin.bitsize, false);
	left->const_expr.i = temp;
	left->const_expr.kind = canonical->type_kind;
	left->type = type;
	return true;
}


/**
 * Cast a signed or unsigned integer -> floating point
 * @return true always
 */
bool sifp(Expr *left, Type *canonical, Type *type)
{
	RETURN_NON_CONST_CAST(CAST_SIFP);
	const_int_to_fp_cast(left, canonical, type);
	return true;
}

/**
 * Cast a signed or unsigned integer -> floating point
 * @return true always
 */
bool uifp(Expr *left, Type *canonical, Type *type)
{
	RETURN_NON_CONST_CAST(CAST_UIFP);
	const_int_to_fp_cast(left, canonical, type);
	return true;
}

bool ixxfp(Expr *left, Type *canonical, Type *type, CastType cast_type)
{
	assert(type_is_float(canonical));
	assert(left->expr_kind == EXPR_CONST);
	const_int_to_fp_cast(left, canonical, type);
	return true;
}

/**
 * Convert a compile time into to a boolean.
 * @return true always
 */
bool ixxbo(Expr *left, Type *type)
{
	assert(left->expr_kind == EXPR_CONST);
	expr_const_set_bool(&left->const_expr, bigint_cmp_zero(&left->const_expr.i) != CMP_EQ);
	left->type = type;
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
//		assert(value->const_expr.type == CONST_INT);
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

/**
 * Cast comptime, signed or unsigned -> pointer.
 * @return true unless the constant value evaluates to zero.
 */
bool xipt(Expr *left, Type *from, Type *canonical, Type *type, CastType cast_type)
{
	if (cast_type == CAST_TYPE_EXPLICIT && left->expr_kind == EXPR_CONST)
	{
		RETURN_NON_CONST_CAST(CAST_XIPTR);
		if (bigint_cmp_zero(&left->const_expr.i) != CMP_EQ)
		{
			SEMA_ERROR(left, "Cannot cast non zero constants into pointers.");
			return false;
		}
		expr_const_set_nil(&left->const_expr);
		left->type = type;
	}
	return cast(left, type_is_unsigned(from) ? type_usize : type_isize, cast_type);
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

bool xixi(Expr *left, Type *from_canonical, Type *canonical, Type *type, CastType cast_type)
{
	assert(from_canonical->canonical == from_canonical);
	switch (from_canonical->type_kind)
	{
		case TYPE_IXX:
			return ixxxi(left, canonical, type, cast_type);
		case ALL_SIGNED_INTS:
			if (type_is_unsigned(canonical)) return siui(left, canonical, type, cast_type);
			return sisi(left, from_canonical, canonical, type, cast_type);
		case ALL_UNSIGNED_INTS:
			if (type_is_unsigned(canonical)) return uiui(left, from_canonical, canonical, type, cast_type);
			return uisi(left, from_canonical, canonical, type, cast_type);
		default:
			UNREACHABLE
	}
}

bool enxi(Expr* left, Type *from, Type *canonical, Type *type, CastType cast_type)
{
	Type *enum_type = from->decl->enums.type_info->type;
	Type *enum_type_canonical = enum_type->canonical;
	// 1. If the underlying type is the same, this is just setting the type.
	if (canonical == enum_type_canonical)
	{
		left->type = type;
		return true;
	}
	// 2. See if we can convert to the target type.
	if (cast_type != CAST_TYPE_EXPLICIT && type_find_max_type(enum_type_canonical, canonical) != canonical)
	{
		if (cast_type == CAST_TYPE_OPTIONAL_IMPLICIT) return true;
		SEMA_ERROR(left, "Cannot implictly convert '%s' with underlying type of '%s' to '%s',"
		                 " use an explicit cast if this is what you want.", type_to_error_string(from),
		           type_to_error_string(enum_type_canonical), type_to_error_string(canonical));
		return false;
	}
	// 3. Dispatch to the right cast:
	return xixi(left, enum_type_canonical, canonical, type, cast_type);
}

bool vava(Expr* left, Type *from, Type *canonical, Type *type, CastType cast_type)
{
	TODO
}

bool sapt(Expr* left, Type *from, Type *canonical, Type *type, CastType cast_type)
{
	bool is_subtype = type_is_subtype(canonical->pointer, from->array.base);
	if (!is_subtype)
	{
		if (cast_type == CAST_TYPE_OPTIONAL_IMPLICIT) return true;
		return sema_type_mismatch(left, type, cast_type);
	}
	insert_cast(left, CAST_SAPTR, canonical);
	return true;
}

bool vasa(Expr* left, Type *from, Type *canonical, Type *type, CastType cast_type)
{
	TODO
}




bool xieu(Expr *left, Type *canonical, Type *type, CastType cast_type)
{
	TODO
}

bool xierr(Expr *left, Type *canonical, Type *type, CastType cast_type)
{
	TODO
}


/**
 * Convert error union to error. This is always a required cast.
 * @return false if an error was reported.
 */
bool eubool(Expr *left, Type *canonical, Type *type, CastType cast_type)
{
	insert_cast(left, CAST_EUBOOL, canonical);
	return true;
}


bool euer(Expr *left, Type *canonical, Type *type, CastType cast_type)
{
	REQUIRE_EXPLICIT_CAST(cast_type);
	insert_cast(left, CAST_EUER, canonical);
	return true;
}


bool ereu(Expr *left, Type *canonical, Type *type, CastType cast_type)
{
	insert_cast(left, CAST_EREU, canonical);
	return true;
}
bool ptva(Expr* left, Type *from, Type *canonical, Type *type, CastType cast_type)
{
	TODO
}

bool ptsa(Expr* left, Type *from, Type *canonical, Type *type, CastType cast_type)
{
	if (from->pointer->type_kind != TYPE_ARRAY)
	{
		if (cast_type == CAST_TYPE_OPTIONAL_IMPLICIT) return true;
		sema_type_mismatch(left, type, CAST_TYPE_EXPLICIT);
		return false;
	}
	if (!type_is_subtype(canonical->array.base, from->pointer->array.base))
	{
		if (cast_type == CAST_TYPE_OPTIONAL_IMPLICIT) return true;
		sema_type_mismatch(left, type, CAST_TYPE_EXPLICIT);
		return false;
	}
	insert_cast(left, CAST_APTSA, canonical);
	return true;
}

bool usbo(Expr* left, Type *from, Type *canonical, Type *type, CastType cast_type)
{
	TODO
}

bool vapt(Expr* left, Type *from, Type *canonical, Type *type, CastType cast_type)
{
	TODO
}


bool cast_implicitly_to_runtime(Expr *expr)
{
	Type *canonical = expr->type->canonical;
	int success;
	switch (canonical->type_kind)
	{
		case TYPE_IXX:
			return cast(expr, type_long, CAST_TYPE_IMPLICIT);
		case TYPE_FXX:
			return cast(expr, type_double, CAST_TYPE_IMPLICIT);
		default:
			return true;
	}
	assert(success && "This should always work");
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
		case TYPE_ERR_UNION:
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_STRING:
		case TYPE_ERRTYPE:
		case TYPE_ENUM:
		case TYPE_FUNC:
		case TYPE_MEMBER:
		case TYPE_ARRAY:
		case TYPE_VARARRAY:
		case TYPE_SUBARRAY:
		case TYPE_TYPEID:
			// Improve consider vararray / subarray conversion to boolean.
			return CAST_ERROR;
		case TYPE_BOOL:
			UNREACHABLE
		case TYPE_IXX:
		case TYPE_I8:
		case TYPE_I16:
		case TYPE_I32:
		case TYPE_I64:
		case TYPE_U8:
		case TYPE_U16:
		case TYPE_U32:
		case TYPE_U64:
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
		case TYPE_TYPEID:
		case TYPE_MEMBER:
			break;
		case TYPE_BOOL:
			// Bool may convert into integers and floats but only explicitly.
			if (type_is_integer(canonical)) return boxi(expr, canonical, to_type, cast_type);
			if (type_is_float(canonical)) return bofp(expr, canonical, to_type, cast_type);
			break;
		case TYPE_ERR_UNION:
			if (to_type->type_kind == TYPE_BOOL) return eubool(expr, canonical, to_type, cast_type);
			if (to_type->type_kind == TYPE_ERRTYPE) return euer(expr, canonical, to_type, cast_type);
			break;
		case TYPE_IXX:
			// Compile time integers may convert into ints, floats, bools
			if (type_is_integer(canonical)) return ixxxi(expr, canonical, to_type, cast_type);
			if (type_is_float(canonical)) return ixxfp(expr, canonical, to_type, cast_type);
			if (canonical == type_bool) return ixxbo(expr, to_type);
			if (canonical->type_kind == TYPE_POINTER) return xipt(expr, from_type, canonical, to_type, cast_type);
			if (canonical->type_kind == TYPE_ENUM) return ixxen(expr, canonical, to_type, cast_type);
			break;
		case TYPE_I8:
		case TYPE_I16:
		case TYPE_I32:
		case TYPE_I64:
			if (type_is_unsigned_integer(canonical)) return siui(expr, canonical, to_type, cast_type);
			if (type_is_signed_integer(canonical)) return sisi(expr, from_type, canonical, to_type, cast_type);
			if (type_is_float(canonical)) return sifp(expr, canonical, to_type);
			if (canonical == type_bool) return xibo(expr, canonical, to_type, cast_type);
			if (canonical->type_kind == TYPE_POINTER) return xipt(expr, from_type, canonical, to_type, cast_type);
			break;
		case TYPE_U8:
		case TYPE_U16:
		case TYPE_U32:
		case TYPE_U64:
			if (type_is_unsigned_integer(canonical)) return uiui(expr, from_type, canonical, to_type, cast_type);
			if (type_is_signed_integer(canonical)) return uisi(expr, from_type, canonical, to_type, cast_type);
			if (type_is_float(canonical)) return uifp(expr, canonical, to_type);
			if (canonical == type_bool) return xibo(expr, canonical, to_type, cast_type);
			if (canonical->type_kind == TYPE_POINTER) return xipt(expr, from_type, canonical, to_type, cast_type);
			break;
		case TYPE_F32:
		case TYPE_F64:
		case TYPE_FXX:
			if (type_is_integer(canonical)) return fpxi(expr, canonical, to_type, cast_type);
			if (canonical == type_bool) return fpbo(expr, canonical, to_type, cast_type);
			if (type_is_float(canonical)) return fpfp(expr, from_type, canonical, to_type, cast_type);
			break;
		case TYPE_POINTER:
			if (type_is_integer(canonical)) return ptxi(expr, canonical, to_type, cast_type);
			if (canonical->type_kind == TYPE_BOOL) return ptbo(expr, canonical, to_type, cast_type);
			if (canonical->type_kind == TYPE_POINTER) return ptpt(expr, from_type, canonical, to_type, cast_type);
			if (canonical->type_kind == TYPE_VARARRAY) return ptva(expr, from_type, canonical, to_type, cast_type);
			if (canonical->type_kind == TYPE_SUBARRAY) return ptsa(expr, from_type, canonical, to_type, cast_type);
			break;
		case TYPE_ENUM:
			if (type_is_integer(canonical)) return enxi(expr, from_type, canonical, to_type, cast_type);
			break;
		case TYPE_ERRTYPE:
			if (canonical->type_kind == TYPE_ERR_UNION) return ereu(expr, canonical, to_type, cast_type);
			break;
		case TYPE_FUNC:
			SEMA_ERROR(expr, "The function call is missing (...), if you want to take the address of a function it must be prefixed with '&'.");
			return false;
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
	if (cast_type == CAST_TYPE_OPTIONAL_IMPLICIT) return true;
	return sema_type_mismatch(expr, canonical, cast_type);
}
