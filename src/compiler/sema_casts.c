// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"

#pragma clang diagnostic push
#pragma ide diagnostic ignored "ConstantFunctionResult"



static bool bitstruct_cast(Expr *expr, Type *from_type, Type *to, Type *to_type);
static void sema_error_const_int_out_of_range(Expr *expr, Expr *problem, Type *to_type);

static inline bool insert_cast(Expr *expr, CastKind kind, Type *type)
{
	assert(expr->resolve_status == RESOLVE_DONE);
	assert(expr->type);
	Expr *inner = expr_copy(expr);
	expr->expr_kind = EXPR_CAST;
	expr->cast_expr.kind = kind;
	expr->cast_expr.expr = inner;
	expr->cast_expr.type_info = NULL;
	expr->type = type;
	return true;
}

bool sema_failed_cast(Expr *expr, Type *from, Type *to)
{
	SEMA_ERROR(expr, "The cast %s to %s is not allowed.", type_quoted_error_string(from), type_quoted_error_string(to));
	return false;
}

static inline bool insert_runtime_cast_unless_const(Expr *expr, CastKind kind, Type *type)
{
	if (expr->expr_kind == EXPR_CONST) return false;
	return insert_cast(expr, kind, type);
}


bool pointer_to_integer(Expr *expr, Type *type)
{
	if (insert_runtime_cast_unless_const(expr, CAST_PTRXI, type)) return true;

	// Must have been a null
	expr_const_set_int(&expr->const_expr, 0, TYPE_POINTER);
	expr->type = type;
	expr->const_expr.narrowable = false;
	expr->const_expr.is_hex = false;
	return true;
}

bool pointer_to_bool(Expr *expr, Type *type)
{
	if (insert_runtime_cast_unless_const(expr, CAST_PTRBOOL, type)) return true;

	// Must have been a null
	expr->const_expr.b = false;
	expr->type = type;
	expr->const_expr.narrowable = false;
	expr->const_expr.is_hex = false;
	return true;
}


bool pointer_to_pointer(Expr* expr, Type *type)
{
	if (insert_runtime_cast_unless_const(expr, CAST_PTRPTR, type)) return true;

	if (expr->const_expr.const_kind == CONST_STRING)
	{
		return insert_cast(expr, CAST_PTRPTR, type);
	}
	// Must have been a null
	expr->type = type;
	expr->const_expr.narrowable = false;
	expr->const_expr.is_hex = false;
	return true;
}


bool string_literal_to_subarray(Expr* left, Type *type)
{
	Type *array_type = type_get_array(type_char, left->const_expr.string.len);
	insert_cast(left, CAST_STRPTR, type_get_ptr(array_type));
	insert_cast(left, CAST_APTSA, type);
	return true;
}

static void const_int_to_fp_cast(Expr *expr, Type *canonical, Type *type)
{
	Real f = int_to_real(expr->const_expr.ixx);
	switch (canonical->type_kind)
	{
		case TYPE_F32:
			expr->const_expr.fxx = (Float) { (float)f, TYPE_F32 };
			break;
		case TYPE_F64:
			expr->const_expr.fxx = (Float) { (double)f, TYPE_F64 };
			break;
		default:
			expr->const_expr.fxx = (Float) { f, canonical->type_kind };
			break;
	}
	expr->type = type;
	expr->const_expr.const_kind = CONST_FLOAT;
	expr->const_expr.narrowable = false;
	expr->const_expr.is_hex = false;
}


/**
 * Bool into a signed or unsigned int.
 */
bool bool_to_int(Expr *expr, Type *canonical, Type *type)
{
	if (insert_runtime_cast_unless_const(expr, CAST_BOOLINT, type)) return true;
	expr_const_set_int(&expr->const_expr, expr->const_expr.b ? 1 : 0, canonical->type_kind);
	expr->type = type;
	expr->const_expr.narrowable = false;
	expr->const_expr.is_hex = false;
	return true;
}


/**
 * Cast bool to float.
 */
bool bool_to_float(Expr *expr, Type *canonical, Type *type)
{
	if (insert_runtime_cast_unless_const(expr, CAST_BOOLFP, type)) return true;

	assert(expr->const_expr.const_kind == CONST_BOOL);
	expr_const_set_float(&expr->const_expr, expr->const_expr.b ? 1.0 : 0.0, canonical->type_kind);
	expr->type = type;
	expr->const_expr.narrowable = false;
	expr->const_expr.is_hex = false;
	return true;
}

/**
 * Cast bool to float.
 */
bool voidfail_to_error(Expr *expr, Type *canonical, Type *type)
{
	Expr *inner = expr_copy(expr);
	expr->expr_kind = EXPR_CATCH;
	expr->inner_expr = inner;
	expr->type = type;
	return true;
}

/**
 * Convert from any into to bool.
 * @return true for any implicit conversion except assign and assign add.
 */
bool integer_to_bool(Expr *expr, Type *type)
{
	if (insert_runtime_cast_unless_const(expr, CAST_INTBOOL, type)) return true;

	expr_const_set_bool(&expr->const_expr, !int_is_zero(expr->const_expr.ixx));
	expr->type = type;
	expr->const_expr.narrowable = false;
	expr->const_expr.is_hex = false;
	return true;
}

/**
 * Convert from any float to bool
 */
bool float_to_bool(Expr *expr, Type *type)
{
	if (insert_runtime_cast_unless_const(expr, CAST_FPBOOL, type)) return true;

	expr_const_set_bool(&expr->const_expr, expr->const_expr.fxx.f != 0.0);
	expr->type = type;
	expr->const_expr.narrowable = false;
	expr->const_expr.is_hex = false;
	return true;
}


/**
 * Convert from any fp to fp
 */
static bool float_to_float(Expr* expr, Type *canonical, Type *type)
{
	if (insert_runtime_cast_unless_const(expr, CAST_FPFP, type)) return true;

	expr_const_set_float(&expr->const_expr, expr->const_expr.fxx.f, canonical->type_kind);
	expr->type = type;
	expr->const_expr.is_hex = false;
	expr->const_expr.narrowable = false;
	return true;
}

/**
 * Convert from any floating point to int
 */
bool float_to_integer(Expr *expr, Type *canonical, Type *type)
{
	bool is_signed = type_is_unsigned(canonical);
	if (insert_runtime_cast_unless_const(expr, is_signed ? CAST_FPSI : CAST_FPUI, type)) return true;

	assert(type_is_integer(canonical));
	Real d = expr->const_expr.fxx.f;
	expr->const_expr.ixx = int_from_real(d, canonical->type_kind);
	expr->const_expr.const_kind = CONST_INTEGER;
	expr->type = type;
	expr->const_expr.narrowable = false;
	expr->const_expr.is_hex = false;
	return true;
}


/**
 * Convert from compile time int to any signed or unsigned int
 * @return true unless the conversion was lossy.
 */
static bool int_literal_to_int(Expr *expr, Type *canonical, Type *type)
{
	if (expr->expr_kind != EXPR_CONST)
	{
		SEMA_ERROR(expr, "This expression could not be resolved to a concrete type. Please add more type annotations.");
		UNREACHABLE
	}
	expr->const_expr.ixx = int_conv(expr->const_expr.ixx, canonical->type_kind);
	assert(expr->const_expr.const_kind == CONST_INTEGER);
	expr->type = type;
	expr->const_expr.narrowable = false;
	expr->const_expr.is_hex = false;
	return true;
}

/**
 * Convert from compile time int to any enum
 */
bool lit_integer_to_enum(Expr *expr, Type *canonical, Type *type)
{
	assert(canonical->type_kind == TYPE_ENUM);
	canonical = type_flatten(canonical->decl->enums.type_info->type);
	return int_literal_to_int(expr, canonical, type);
}


static bool int_conversion(Expr *expr, CastKind kind, Type *canonical, Type *type)
{
	if (insert_runtime_cast_unless_const(expr, kind, type)) return true;

	expr->const_expr.ixx = int_conv(expr->const_expr.ixx, canonical->type_kind);
	expr->const_expr.const_kind = CONST_INTEGER;
	expr->type = type;
	expr->const_expr.narrowable = false;
	expr->const_expr.is_hex = false;
	return true;
}


/**
 * Cast a signed or unsigned integer -> floating point
 */
static bool int_to_float(Expr *expr, CastKind kind, Type *canonical, Type *type)
{
	if (insert_runtime_cast_unless_const(expr, kind, type)) return true;
	const_int_to_fp_cast(expr, canonical, type);
	return true;
}


/**
 * Convert a compile time into to a boolean.
 */
static bool int_literal_to_bool(Expr *expr, Type *type)
{
	assert(expr->expr_kind == EXPR_CONST);
	expr_const_set_bool(&expr->const_expr, !int_is_zero(expr->const_expr.ixx));
	expr->const_expr.narrowable = false;
	expr->const_expr.is_hex = false;
	expr->type = type;
	return true;
}


/**
 * Cast any int to a pointer -> pointer.
 */
static bool int_to_pointer(Expr *expr, Type *type)
{
	if (expr->expr_kind == EXPR_CONST)
	{
		if (int_is_zero(expr->const_expr.ixx))
		{
			expr_const_set_null(&expr->const_expr);
			expr->type = type;
			return true;
		}
	}
	cast(expr, type_uptr);
	return insert_cast(expr, CAST_XIPTR, type);
}


static bool int_to_int(Expr *left, Type *from_canonical, Type *canonical, Type *type)
{
	assert(from_canonical->canonical == from_canonical);
	switch (from_canonical->type_kind)
	{
		case ALL_SIGNED_INTS:
			return int_conversion(left, type_is_unsigned(canonical) ? CAST_SIUI : CAST_SISI, canonical, type);
		case ALL_UNSIGNED_INTS:
			return int_conversion(left, type_is_unsigned(canonical) ? CAST_UIUI : CAST_UISI, canonical, type);
		default:
			UNREACHABLE
	}
}

static Type *enum_lowering(Expr* expr, Type *from)
{
	if (expr->expr_kind == EXPR_CONST && expr->const_expr.const_kind == CONST_ENUM)
	{
		expr_replace(expr, expr->const_expr.enum_val->enum_constant.expr);
		assert(!IS_FAILABLE(expr));
		return expr->type;
	}
	Type *result = from->decl->enums.type_info->type;
	insert_cast(expr, CAST_ENUMLOW, type_get_opt_fail(result, IS_FAILABLE(expr)));
	return result;
}

static bool enum_to_integer(Expr* expr, Type *from, Type *canonical, Type *type)
{
	Type *result = enum_lowering(expr, from);
	return int_to_int(expr, result->canonical, canonical, type);
}

static bool enum_to_float(Expr* expr, Type *from, Type *canonical, Type *type)
{
	Type *result = enum_lowering(expr, from);
	return int_to_float(expr, type_is_unsigned(result->canonical) ? CAST_UIFP : CAST_SIFP, canonical, type);
}

bool enum_to_bool(Expr* expr, Type *from, Type *type)
{
	enum_lowering(expr, from);
	return integer_to_bool(expr, type);
}

bool enum_to_pointer(Expr* expr, Type *from, Type *type)
{
	enum_lowering(expr, from);
	return int_to_pointer(expr, type);
}


CastKind cast_to_bool_kind(Type *type)
{
	switch (type_flatten(type)->type_kind)
	{
		case TYPE_TYPEDEF:
		case TYPE_DISTINCT:
		case TYPE_INFERRED_ARRAY:
			UNREACHABLE
		case TYPE_BOOL:
			return CAST_BOOLBOOL;
		case TYPE_ANYERR:
			return CAST_EUBOOL;
		case TYPE_SUBARRAY:
			return CAST_SABOOL;
		case ALL_INTS:
			return CAST_INTBOOL;
		case ALL_FLOATS:
			return CAST_FPBOOL;
		case TYPE_POINTER:
			return CAST_PTRBOOL;
		case TYPE_ERRTYPE:
			return CAST_ERBOOL;
		case TYPE_POISONED:
		case TYPE_VOID:
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_ENUM:
		case TYPE_FUNC:
		case TYPE_ARRAY:
		case TYPE_TYPEID:
		case TYPE_TYPEINFO:
		case TYPE_VECTOR:
		case TYPE_BITSTRUCT:
		case TYPE_UNTYPED_LIST:
		case TYPE_FAILABLE:
		case TYPE_ANY:
		case TYPE_FAILABLE_ANY:
		case TYPE_FLEXIBLE_ARRAY:
			return CAST_ERROR;
	}
	UNREACHABLE
}

bool cast_may_explicit(Type *from_type, Type *to_type, bool ignore_failability, bool is_const)
{
	// 1. failable -> non-failable can't be cast unless we ignore failability.
	// *or* we're converting a void! to an error code
	if (type_is_failable(from_type) && !type_is_failable(to_type))
	{
		if (from_type->failable == type_void || !from_type->failable)
		{
			// void! x; anyerr y = (anyerr)(x);
			if (to_type->type_kind == TYPE_ERRTYPE || to_type->type_kind == TYPE_ANYERR) return true;
		}
		if (!ignore_failability) return false;
	}

	// 2. Remove failability and flatten distinct
	from_type = type_no_fail(from_type);
	to_type = type_no_fail(to_type);

	// 3. We flatten the distinct types, since they should be freely convertible
	from_type = type_flatten_distinct_failable(from_type);
	to_type = type_flatten_distinct_failable(to_type);

	// 2. Same underlying type, always ok
	if (from_type == to_type) return true;

	if (to_type->type_kind == TYPE_INFERRED_ARRAY)
	{
		if (from_type->type_kind == TYPE_ARRAY && type_flatten_distinct(from_type->array.base) == type_flatten_distinct(to_type->array.base)) return true;
		return false;
	}

	TypeKind to_kind = to_type->type_kind;
	switch (from_type->type_kind)
	{
		case TYPE_FAILABLE_ANY:
			return true;
		case TYPE_DISTINCT:
		case TYPE_TYPEDEF:
		case TYPE_FAILABLE:
			UNREACHABLE
		case TYPE_POISONED:
		case TYPE_INFERRED_ARRAY:
		case TYPE_VOID:
		case TYPE_TYPEINFO:
		case TYPE_FUNC:
			return false;
		case TYPE_TYPEID:
			// May convert to anything pointer sized or larger, no enums
			return type_is_pointer_sized_or_more(to_type);
		case TYPE_BOOL:
			// May convert to any integer / distinct integer / float, no enums
			return type_is_integer(to_type) || type_is_float(to_type);
		case TYPE_BITSTRUCT:
			// A bitstruct can convert to:
			// 1. An int of the same length
			// 2. An integer array of the same length
			if (type_size(to_type) != type_size(from_type)) return false;
			if (type_is_integer(to_type)) return true;
			return to_type->type_kind == TYPE_ARRAY && type_is_integer(to_type->array.base);
		case TYPE_ANYERR:
			// May convert to a bool, an error type or an integer
			return to_type == type_bool || to_kind == TYPE_ERRTYPE || type_is_integer(to_type);
		case ALL_SIGNED_INTS:
		case ALL_UNSIGNED_INTS:
			// We don't have to match pointer size if it's a constant.
			if (to_kind == TYPE_POINTER && is_const) return true;
			FALLTHROUGH;
		case TYPE_ENUM:
			// Allow conversion int/enum -> float/bool/enum int/enum -> pointer is only allowed if the int/enum is pointer sized.
			if (type_is_integer(to_type) || type_is_float(to_type) || to_type == type_bool || to_kind == TYPE_ENUM) return true;
			// TODO think about this, maybe we should require a bitcast?
			if (to_kind == TYPE_POINTER && type_is_pointer_sized(from_type)) return true;
			return false;
		case ALL_FLOATS:
			// Allow conversion float -> float/int/bool/enum
			return type_is_integer(to_type) || type_is_float(to_type) || to_type == type_bool || to_kind == TYPE_ENUM;
		case TYPE_POINTER:
			// Allow conversion ptr -> int (min pointer size)/bool/pointer/vararray
			if ((type_is_integer(to_type) && type_size(to_type) >= type_size(type_iptr)) || to_type == type_bool || to_kind == TYPE_POINTER) return true;
			// Special subarray conversion: someType[N]* -> someType[]
			if (to_kind == TYPE_SUBARRAY && from_type->pointer->type_kind == TYPE_ARRAY && from_type->pointer->array.base == to_type->array.base) return true;
			return false;
		case TYPE_ANY:
			return to_kind == TYPE_POINTER;
		case TYPE_ERRTYPE:
			// Allow MyError.A -> error, to an integer or to bool
			return to_type->type_kind == TYPE_ANYERR || type_is_integer(to_type) || to_type == type_bool;
		case TYPE_FLEXIBLE_ARRAY:
			return false;
		case TYPE_ARRAY:
			if (to_kind == TYPE_VECTOR)
			{
				return to_type->array.len == from_type->vector.len && to_type->array.base == from_type->array.base;
			}
			FALLTHROUGH;
		case TYPE_STRUCT:
			if (type_is_substruct(from_type))
			{
				if (cast_may_explicit(from_type->decl->strukt.members[0]->type, to_type, false, false)) return true;
			}
			FALLTHROUGH;
		case TYPE_UNION:
			return type_is_structurally_equivalent(from_type, to_type);
		case TYPE_SUBARRAY:
			return to_kind == TYPE_POINTER;
		case TYPE_VECTOR:
			return type_is_structurally_equivalent(type_get_array(from_type->vector.base, (uint32_t)from_type->vector.len), to_type);
		case TYPE_UNTYPED_LIST:
			REMINDER("Look at untyped list explicit conversions");
			return false;
	}
	UNREACHABLE
}

bool type_may_convert_to_anyerr(Type *type)
{
	if (type_is_failable_any(type)) return true;
	if (!type_is_failable_type(type)) return false;
	return type->failable->canonical == type_void;
}
/**
 * Can the conversion occur implicitly?
 */
bool cast_may_implicit(Type *from_type, Type *to_type, bool is_simple_expr, bool failable_allowed)
{
	Type *to = to_type->canonical;

	// 1. First handle void! => any error
	if (to == type_anyerr && type_may_convert_to_anyerr(from_type)) return true;

	// 2. any! => may implicitly to convert to any.
	if (type_is_failable_any(from_type)) return failable_allowed;

	Type *from = from_type->canonical;
	if (type_is_failable_type(from_type))
	{
		if (!failable_allowed) return false;
		from = from_type->failable->canonical;
	}

	// 4. Same canonical type - we're fine.
	if (from == to) return true;

	// 2. Handle floats
	if (type_is_float(to))
	{
		// 2a. Any integer may convert to a float.
		if (type_is_integer(from)) return true;

		// 2b. Any narrower float
		if (type_is_float(from))
		{
			ByteSize to_size = type_size(to);
			ByteSize from_size = type_size(from);
			if (to_size == from_size) return true;
			return to_size > from_size && is_simple_expr;
		}
		return false;
	}

	if (to == type_anyerr && from->type_kind == TYPE_ERRTYPE) return true;

	// 3. Handle ints
	if (type_is_integer(to))
	{
		// For an enum, lower to the underlying enum type.
		if (from->type_kind == TYPE_ENUM)
		{
			from = from->decl->enums.type_info->type->canonical;
		}

		// 3a. Any narrower int may convert to a wider or same int, regardless of signedness.
		if (type_is_integer(from))
		{
			ByteSize to_size = type_size(to);
			ByteSize from_size = type_size(from);
			if (to_size == from_size) return true;
			return to_size > from_size && is_simple_expr;
		}
		return false;
	}

	// 4. Handle pointers
	if (type_is_pointer(to))
	{
		// 4a. Assigning a subarray to a pointer of the same base type is fine
		if (from->type_kind == TYPE_SUBARRAY)
		{
			// void* conversion always work.
			if (to == type_voidptr) return true;

			// Use subtype matching
			return type_is_subtype(to->pointer->canonical, from->array.base->canonical);
		}

		// 4b. Assigning a pointer
		if (from->type_kind == TYPE_POINTER)
		{
			// For void* on either side, no checks.
			if (to == type_voidptr || from == type_voidptr) return true;

			// Special handling of int* = int[4]*
			if (from->pointer->type_kind == TYPE_ARRAY)
			{
				if (type_is_subtype(to->pointer, from->pointer->array.base))
				{
					return true;
				}
			}

			// Use subtype matching
			return type_is_subtype(to->pointer, from->pointer);
		}

		return false;
	}

	if (to_type->type_kind == TYPE_INFERRED_ARRAY)
	{
		if (from_type->type_kind == TYPE_ARRAY && type_flatten_distinct(from_type->array.base) == type_flatten_distinct(to_type->array.base)) return true;
		return false;
	}

	// 5. Handle sub arrays
	if (to->type_kind == TYPE_SUBARRAY)
	{
		// 5a. char[] foo = "test"
		Type *base = to->array.base;

		// 5b. Assign sized array pointer int[] = int[4]*
		if (type_is_pointer(from))
		{
			return from->pointer->type_kind == TYPE_ARRAY && from->pointer->array.base == base;
		}
		return false;
	}



	// 8. Check if we may cast this to bool. It is safe for many types.
	if (to->type_kind == TYPE_BOOL)
	{
		return cast_to_bool_kind(from) != CAST_ERROR;
	}

	// 9. Any cast
	if (to->type_kind == TYPE_ANY)
	{
		return from_type->type_kind == TYPE_POINTER;
	}


	// 11. Substruct cast, if the first member is inline, see if we can cast to this member.
	if (type_is_substruct(from))
	{
		return cast_may_implicit(from->decl->strukt.members[0]->type, to, is_simple_expr, failable_allowed);
	}

	return false;
}

bool may_convert_float_const_implicit(Expr *expr, Type *to_type)
{
	if (!float_const_fits_type(&expr->const_expr, type_flatten(to_type)->type_kind))
	{
#if LONG_DOUBLE
		SEMA_ERROR(expr, "The value '%Lg' is out of range for %s, so you need an explicit cast to truncate the value.", expr->const_expr.fxx.f, type_quoted_error_string(to_type));
#else
		SEMA_ERROR(expr, "The value '%g' is out of range for %s, so you need an explicit cast to truncate the value.", expr->const_expr.fxx.f, type_quoted_error_string(to_type));
#endif
		return false;
	}
	return true;
}


bool may_convert_int_const_implicit(Expr *expr, Type *to_type)
{
	Type *to_type_flat = type_flatten(to_type);
	if (expr_const_will_overflow(&expr->const_expr, to_type_flat->type_kind))
	{
		sema_error_const_int_out_of_range(expr, expr, to_type);
		return false;
	}
	return true;
}

Expr *recursive_may_narrow_float(Expr *expr, Type *type)
{
	switch (expr->expr_kind)
	{
		case EXPR_BINARY:
		case EXPR_BITASSIGN:
			switch (expr->binary_expr.operator)
			{
				case BINARYOP_ERROR:
					UNREACHABLE
				case BINARYOP_MULT:
				case BINARYOP_SUB:
				case BINARYOP_ADD:
				case BINARYOP_DIV:
				case BINARYOP_MOD:
				{
					Expr *res = recursive_may_narrow_float(expr->binary_expr.left, type);
					if (res) return res;
					return recursive_may_narrow_float(expr->binary_expr.right, type);
				}
				case BINARYOP_BIT_OR:
				case BINARYOP_BIT_XOR:
				case BINARYOP_BIT_AND:
				case BINARYOP_AND:
				case BINARYOP_OR:
				case BINARYOP_GT:
				case BINARYOP_GE:
				case BINARYOP_LT:
				case BINARYOP_LE:
				case BINARYOP_NE:
				case BINARYOP_EQ:
				case BINARYOP_SHR:
				case BINARYOP_SHL:
				case BINARYOP_BIT_AND_ASSIGN:
				case BINARYOP_BIT_OR_ASSIGN:
				case BINARYOP_BIT_XOR_ASSIGN:
				case BINARYOP_SHR_ASSIGN:
				case BINARYOP_SHL_ASSIGN:
					UNREACHABLE
				case BINARYOP_ASSIGN:
				case BINARYOP_ADD_ASSIGN:
				case BINARYOP_DIV_ASSIGN:
				case BINARYOP_MOD_ASSIGN:
				case BINARYOP_MULT_ASSIGN:
				case BINARYOP_SUB_ASSIGN:
					return recursive_may_narrow_float(expr->binary_expr.left, type);
			}
			UNREACHABLE
		case EXPR_MACRO_BODY_EXPANSION:
		case EXPR_CALL:
		case EXPR_POISONED:
		case EXPR_BITACCESS:
		case EXPR_ACCESS:
		case EXPR_CATCH_UNWRAP:
		case EXPR_COMPOUND_LITERAL:
		case EXPR_COND:
		case EXPR_DECL:
		case EXPR_CT_IDENT:
		case EXPR_DESIGNATOR:
		case EXPR_EXPR_BLOCK:
		case EXPR_MACRO_BLOCK:
		case EXPR_MACRO_EXPANSION:
		case EXPR_IDENTIFIER:
		case EXPR_SLICE_ASSIGN:
		case EXPR_SLICE:
		case EXPR_SUBSCRIPT:
			if (type_size(expr->type) > type_size(type)) return expr;
			return NULL;
		case EXPR_OR_ERROR:
		{
			Expr *res = recursive_may_narrow_float(expr->or_error_expr.expr, type);
			if (res) return res;
			if (expr->or_error_expr.is_jump) return NULL;
			return recursive_may_narrow_float(expr->or_error_expr.or_error_expr, type);
		}
		case EXPR_EXPRESSION_LIST:
			return recursive_may_narrow_float(VECLAST(expr->expression_list), type);
		case EXPR_GROUP:
		case EXPR_FORCE_UNWRAP:
			return recursive_may_narrow_float(expr->inner_expr, type);
		case EXPR_RETHROW:
			return recursive_may_narrow_float(expr->rethrow_expr.inner, type);
		case EXPR_TERNARY:
		{
			Expr *res = recursive_may_narrow_float(expr->ternary_expr.then_expr ? expr->ternary_expr.then_expr
			                                                                    : expr->ternary_expr.cond, type);
			if (res) return res;
			return recursive_may_narrow_float(expr->ternary_expr.else_expr, type);
		}
		case EXPR_CAST:
			if (expr->cast_expr.implicit)
			{
				return recursive_may_narrow_float(expr->cast_expr.expr, type);
			}
			return type_size(expr->type) > type_size(type) ? expr : NULL;
		case EXPR_CONST:
			if (!expr->const_expr.narrowable)
			{
				return type_size(expr->type) > type_size(type) ? expr : NULL;
			}
			assert(expr->const_expr.const_kind == CONST_FLOAT);
			if (!float_const_fits_type(&expr->const_expr, type_flatten(type)->type_kind))
			{
				return expr;
			}
			return NULL;
		case EXPR_CONST_IDENTIFIER:
			return type_size(expr->type) > type_size(type) ? expr : NULL;
		case EXPR_FAILABLE:
		case EXPR_HASH_IDENT:
		case EXPR_FLATPATH:
		case EXPR_INITIALIZER_LIST:
		case EXPR_DESIGNATED_INITIALIZER_LIST:
		case EXPR_PLACEHOLDER:
		case EXPR_TYPEID:
		case EXPR_TYPEINFO:
		case EXPR_UNDEF:
		case EXPR_CT_CALL:
		case EXPR_NOP:
		case EXPR_LEN:
		case EXPR_CATCH:
		case EXPR_BUILTIN:
		case EXPR_TRY_UNWRAP:
		case EXPR_TRY_UNWRAP_CHAIN:
		case EXPR_SUBSCRIPT_ADDR:
		case EXPR_TYPEOFANY:
		case EXPR_PTR:
		case EXPR_VARIANTSWITCH:
		case EXPR_ARGV_TO_SUBARRAY:
			UNREACHABLE
		case EXPR_POST_UNARY:
			return recursive_may_narrow_float(expr->unary_expr.expr, type);
		case EXPR_SCOPED_EXPR:
			return recursive_may_narrow_float(expr->expr_scope.expr, type);
		case EXPR_TRY:
			return recursive_may_narrow_float(expr->inner_expr, type);
		case EXPR_UNARY:
		{
			switch (expr->unary_expr.operator)
			{
				case UNARYOP_ERROR:
				case UNARYOP_DEREF:
				case UNARYOP_ADDR:
				case UNARYOP_NOT:
				case UNARYOP_TADDR:
					UNREACHABLE
				case UNARYOP_NEG:
				case UNARYOP_BITNEG:
				case UNARYOP_INC:
				case UNARYOP_DEC:
					return recursive_may_narrow_float(expr->unary_expr.expr, type);
			}
		}
	}
	UNREACHABLE
}

Expr *recursive_may_narrow_int(Expr *expr, Type *type)
{
	switch (expr->expr_kind)
	{
		case EXPR_BITASSIGN:
		case EXPR_BINARY:
			switch (expr->binary_expr.operator)
			{
				case BINARYOP_ERROR:
					UNREACHABLE
				case BINARYOP_MULT:
				case BINARYOP_SUB:
				case BINARYOP_ADD:
				case BINARYOP_DIV:
				case BINARYOP_MOD:
				case BINARYOP_BIT_OR:
				case BINARYOP_BIT_XOR:
				case BINARYOP_BIT_AND:
				{
					Expr *res = recursive_may_narrow_int(expr->binary_expr.left, type);
					if (res) return res;
					return recursive_may_narrow_int(expr->binary_expr.right, type);
				}
				case BINARYOP_AND:
				case BINARYOP_OR:
				case BINARYOP_GT:
				case BINARYOP_GE:
				case BINARYOP_LT:
				case BINARYOP_LE:
				case BINARYOP_NE:
				case BINARYOP_EQ:
					return NULL;
				case BINARYOP_SHR:
				case BINARYOP_SHL:
				case BINARYOP_ASSIGN:
				case BINARYOP_ADD_ASSIGN:
				case BINARYOP_BIT_AND_ASSIGN:
				case BINARYOP_BIT_OR_ASSIGN:
				case BINARYOP_BIT_XOR_ASSIGN:
				case BINARYOP_DIV_ASSIGN:
				case BINARYOP_MOD_ASSIGN:
				case BINARYOP_MULT_ASSIGN:
				case BINARYOP_SHR_ASSIGN:
				case BINARYOP_SHL_ASSIGN:
				case BINARYOP_SUB_ASSIGN:
					return recursive_may_narrow_int(expr->binary_expr.left, type);
			}
			UNREACHABLE
		case EXPR_MACRO_BODY_EXPANSION:
		case EXPR_CALL:
		case EXPR_POISONED:
		case EXPR_BITACCESS:
		case EXPR_ACCESS:
		case EXPR_CATCH_UNWRAP:
		case EXPR_COMPOUND_LITERAL:
		case EXPR_COND:
		case EXPR_DECL:
		case EXPR_CT_IDENT:
		case EXPR_DESIGNATOR:
		case EXPR_EXPR_BLOCK:
		case EXPR_MACRO_BLOCK:
		case EXPR_MACRO_EXPANSION:
		case EXPR_IDENTIFIER:
		case EXPR_SLICE_ASSIGN:
		case EXPR_SLICE:
		case EXPR_SUBSCRIPT:
			if (type_size(expr->type) > type_size(type)) return expr;
			return NULL;
		case EXPR_LEN:
			if (type_size(type) < type_size(type_cint())) return expr;
			return NULL;
		case EXPR_OR_ERROR:
		{
			Expr *res = recursive_may_narrow_int(expr->or_error_expr.expr, type);
			if (res) return res;
			if (expr->or_error_expr.is_jump) return NULL;
			return recursive_may_narrow_int(expr->or_error_expr.or_error_expr, type);
		}
		case EXPR_EXPRESSION_LIST:
			return recursive_may_narrow_int(VECLAST(expr->expression_list), type);
		case EXPR_RETHROW:
			return recursive_may_narrow_int(expr->rethrow_expr.inner, type);
		case EXPR_TERNARY:
		{
			Expr *res = recursive_may_narrow_int(expr->ternary_expr.then_expr ? expr->ternary_expr.then_expr
			                                                                  : expr->ternary_expr.cond, type);
			if (res) return res;
			return recursive_may_narrow_int(expr->ternary_expr.else_expr, type);
		}
		case EXPR_CAST:
			if (expr->cast_expr.implicit)
			{
				return recursive_may_narrow_int(expr->cast_expr.expr, type);
			}
			return type_size(expr->type) > type_size(type) ? expr : NULL;
		case EXPR_CONST:
			if (!expr->const_expr.narrowable)
			{
				return type_size(expr->type) > type_size(type) ? expr : NULL;
			}
			assert(expr->const_expr.const_kind == CONST_INTEGER);
			if (expr_const_will_overflow(&expr->const_expr, type_flatten(type)->type_kind))
			{
				return expr;
			}
			return NULL;
		case EXPR_CONST_IDENTIFIER:
			return type_size(expr->type) > type_size(type) ? expr : NULL;
		case EXPR_FAILABLE:
		case EXPR_HASH_IDENT:
		case EXPR_FLATPATH:
		case EXPR_INITIALIZER_LIST:
		case EXPR_DESIGNATED_INITIALIZER_LIST:
		case EXPR_PLACEHOLDER:
		case EXPR_TYPEID:
		case EXPR_TYPEINFO:
		case EXPR_UNDEF:
		case EXPR_CT_CALL:
		case EXPR_NOP:
		case EXPR_BUILTIN:
		case EXPR_TRY_UNWRAP:
		case EXPR_TRY_UNWRAP_CHAIN:
		case EXPR_SUBSCRIPT_ADDR:
		case EXPR_TYPEOFANY:
		case EXPR_PTR:
		case EXPR_ARGV_TO_SUBARRAY:
		case EXPR_VARIANTSWITCH:
			UNREACHABLE
		case EXPR_POST_UNARY:
			return recursive_may_narrow_int(expr->unary_expr.expr, type);
		case EXPR_SCOPED_EXPR:
			return recursive_may_narrow_int(expr->expr_scope.expr, type);
		case EXPR_TRY:
		case EXPR_CATCH:
		case EXPR_GROUP:
		case EXPR_FORCE_UNWRAP:
			return recursive_may_narrow_int(expr->inner_expr, type);
		case EXPR_UNARY:
		{
			switch (expr->unary_expr.operator)
			{
				case UNARYOP_ERROR:
				case UNARYOP_DEREF:
				case UNARYOP_ADDR:
				case UNARYOP_NOT:
				case UNARYOP_TADDR:
					UNREACHABLE
				case UNARYOP_NEG:
				case UNARYOP_BITNEG:
				case UNARYOP_INC:
				case UNARYOP_DEC:
					return recursive_may_narrow_int(expr->unary_expr.expr, type);
			}
		}
	}
	UNREACHABLE
}

static void sema_error_const_int_out_of_range(Expr *expr, Expr *problem, Type *to_type)
{
	assert(expr->expr_kind == EXPR_CONST);
	if (expr->const_expr.is_character)
	{
		SEMA_ERROR(problem, "The unicode character U+%04x cannot fit in a %s.", (uint32_t)expr->const_expr.ixx.i.low, type_quoted_error_string(to_type));
		return;
	}
	const char *error_value = expr->const_expr.is_hex ? int_to_str(expr->const_expr.ixx, 16) : expr_const_to_error_string(&expr->const_expr);
	SEMA_ERROR(problem, "The value '%s' is out of range for %s, so you need an explicit cast to truncate the value.", error_value,
			   type_quoted_error_string(to_type));
}

static inline bool cast_maybe_string_lit_to_char_array(Expr *expr, Type *expr_canonical, Type *to_canonical)
{
	if (expr->expr_kind != EXPR_CONST || expr->const_expr.const_kind != CONST_STRING) return false;
	if (expr_canonical->type_kind != TYPE_POINTER) return false;
	if (to_canonical->type_kind != TYPE_ARRAY && to_canonical->type_kind != TYPE_INFERRED_ARRAY) return false;
	if (to_canonical->array.base != type_char) return false;
	Type *pointer = expr_canonical->pointer;
	if (pointer->type_kind != TYPE_ARRAY) return false;
	if (pointer->array.base != type_char) return false;
	expr_insert_deref(expr);
	return true;
}
bool cast_implicit(Expr *expr, Type *to_type)
{
	assert(!type_is_failable(to_type));
	Type *expr_type = expr->type;
	Type *expr_canonical = expr_type->canonical;
	Type *to_canonical = to_type->canonical;
	if (cast_maybe_string_lit_to_char_array(expr, expr_canonical, to_canonical))
	{
		expr_type = expr->type;
		expr_canonical = expr_type->canonical;
	}
	if (expr_canonical == to_canonical) return true;
	bool is_simple = expr_is_simple(expr);
	if (!cast_may_implicit(expr_canonical, to_canonical, is_simple, true))
	{
		if (!cast_may_explicit(expr_canonical, to_canonical, false, expr->expr_kind == EXPR_CONST))
		{
			if (expr_canonical->type_kind == TYPE_FAILABLE && to_canonical->type_kind != TYPE_FAILABLE)
			{
				SEMA_ERROR(expr, "A failable %s cannot be converted to %s.", type_quoted_error_string(expr->type), type_quoted_error_string(to_type));
				return false;
			}
			if (to_canonical->type_kind == TYPE_ANY)
			{
				SEMA_ERROR(expr, "You can only convert pointers to 'variant', take the address of this expression first.");
				return false;
			}
			SEMA_ERROR(expr, "You cannot cast %s into %s even with an explicit cast, so this looks like an error.", type_quoted_error_string(expr->type), type_quoted_error_string(to_type));
			return false;
		}
		bool is_narrowing = type_size(expr_canonical) >= type_size(to_canonical);
		if (expr->expr_kind == EXPR_CONST && expr->const_expr.narrowable && is_narrowing)
		{
			Type *expr_flatten = type_flatten_distinct(expr_canonical);
			Type *to_flatten = type_flatten_distinct(to_canonical);
			if (type_is_integer(expr_flatten) && type_is_integer(to_flatten))
			{
				Expr *problem = recursive_may_narrow_int(expr, to_canonical);
				if (problem)
				{
					sema_error_const_int_out_of_range(expr, problem, to_type);
					return false;
				}
				goto OK;
			}
			if (type_is_float(expr_flatten) && type_is_float(to_flatten))
			{
				Expr *problem = recursive_may_narrow_float(expr, to_canonical);
				if (problem)
				{
					SEMA_ERROR(problem, "The value '%s' is out of range for %s, so you need an explicit cast to truncate the value.", expr_const_to_error_string(&expr->const_expr),
							   type_quoted_error_string(to_type));
					return false;
				}
				goto OK;
			}
		}
		if (type_is_integer(expr_canonical) && type_is_integer(to_canonical) && is_narrowing)
		{
			Expr *problem = recursive_may_narrow_int(expr, to_canonical);
			if (problem)
			{
				SEMA_ERROR(problem, "Cannot narrow %s to %s.", type_quoted_error_string(problem->type),
				           type_quoted_error_string(to_type));
				return false;
			}
			goto OK;
		}
		if (type_is_float(expr_canonical) && type_is_float(to_canonical) && is_narrowing)
		{
			Expr *problem = recursive_may_narrow_float(expr, to_canonical);
			if (problem)
			{
				SEMA_ERROR(problem, "The value '%s' is out of range for %s.", expr_const_to_error_string(&expr->const_expr),
						   type_quoted_error_string(to_type));
				return false;
			}
			goto OK;
		}
		SEMA_ERROR(expr, "Implicitly casting %s to %s is not permitted, but you can do an explicit cast using '(<type>)(value)'.", type_quoted_error_string(
				type_no_fail(expr->type)), type_quoted_error_string(type_no_fail(to_type)));
		return false;
	}

	OK:
	// Additional checks for compile time values.
	if (expr->expr_kind == EXPR_CONST && expr->const_expr.narrowable)
	{
		if (type_is_float(expr->type))
		{
			if (!may_convert_float_const_implicit(expr, to_type)) return false;
		}
		else if (type_is_integer(expr->type))
		{
			if (!may_convert_int_const_implicit(expr, to_type)) return false;
		}
	}
	cast(expr, to_type);
	// Allow narrowing after widening
	if (type_is_numeric(to_type) && expr->expr_kind == EXPR_CONST && type_size(expr_canonical) < type_size(to_canonical))
	{
		expr->const_expr.narrowable = true;
	}
	if (expr->expr_kind == EXPR_CAST) expr->cast_expr.implicit = true;
	return true;
}

static bool err_to_anyerr(Expr *expr, Type *to_type)
{
	expr->type = to_type;
	return true;
}

static bool err_to_bool(Expr *expr, Type *to_type)
{
	if (expr->expr_kind == EXPR_CONST)
	{
		switch (expr->const_expr.const_kind)
		{
			case CONST_INTEGER:
				return int_literal_to_bool(expr, to_type);
			case CONST_ERR:
				expr_const_set_bool(&expr->const_expr, expr->const_expr.err_val != NULL);
				return true;
			default:
				UNREACHABLE
		}
	}
	return insert_cast(expr, CAST_ERBOOL, to_type);
}

bool cast_implicit_bit_width(Expr *expr, Type *to_type)
{
	Type *to_canonical = to_type->canonical;
	Type *from_canonical = expr->type->canonical;
	if (type_is_integer(to_canonical) && type_is_integer(from_canonical))
	{
		if (type_is_unsigned(to_canonical) != type_is_unsigned(from_canonical))
		{
			if (type_is_unsigned(from_canonical))
			{
				to_type = type_int_unsigned_by_bitsize((uint32_t)type_size(to_canonical) * 8);
			}
			else
			{
				to_type = type_int_signed_by_bitsize((uint32_t)type_size(to_canonical) * 8);
			}
		}
	}
	return cast_implicit(expr, to_type);
}

static inline bool subarray_to_bool(Expr *expr)
{
	if (expr->expr_kind == EXPR_CONST && expr->const_expr.const_kind == CONST_LIST)
	{
		ConstInitializer *list = expr->const_expr.list;
		switch (list->kind)
		{
			case CONST_INIT_ZERO:
				expr_const_set_bool(&expr->const_expr, false);
				return true;
			case CONST_INIT_ARRAY:
				expr_const_set_bool(&expr->const_expr, vec_size(list->init_array.elements) > 0);
				return true;
			case CONST_INIT_ARRAY_FULL:
				expr_const_set_bool(&expr->const_expr, vec_size(list->init_array_full) > 0);
				return true;
			case CONST_INIT_STRUCT:
			case CONST_INIT_UNION:
			case CONST_INIT_VALUE:
			case CONST_INIT_ARRAY_VALUE:
				break;
		}
	}
	return insert_cast(expr, CAST_SABOOL, type_bool);
}

static bool cast_inner(Expr *expr, Type *from_type, Type *to, Type *to_type)
{
	switch (from_type->type_kind)
	{
		case TYPE_FAILABLE_ANY:
		case TYPE_FAILABLE:
			UNREACHABLE
		case TYPE_VOID:
			UNREACHABLE
		case TYPE_TYPEID:
		case TYPE_DISTINCT:
		case TYPE_FUNC:
		case TYPE_TYPEDEF:
		case CT_TYPES:
			UNREACHABLE
		case TYPE_BITSTRUCT:
			return bitstruct_cast(expr, from_type, to, to_type);
		case TYPE_BOOL:
			// Bool may convert into integers and floats but only explicitly.
			if (type_is_integer(to)) return bool_to_int(expr, to, to_type);
			if (type_is_float(to)) return bool_to_float(expr, to, to_type);
			break;
		case TYPE_ANYERR:
			if (to->type_kind == TYPE_BOOL) return insert_cast(expr, CAST_EUBOOL, to_type);
			if (to->type_kind == TYPE_ERRTYPE) return insert_cast(expr, CAST_EUER, to_type);
			if (type_is_integer(to)) return insert_cast(expr, CAST_EUINT, to_type);
			break;
		case ALL_SIGNED_INTS:
			if (type_is_integer_unsigned(to)) return int_conversion(expr, CAST_SIUI, to, to_type);
			if (type_is_integer_signed(to)) return int_conversion(expr, CAST_SISI, to, to_type);
			if (type_is_float(to)) return int_to_float(expr, CAST_SIFP, to, to_type);
			if (to == type_bool) return integer_to_bool(expr, to_type);
			if (to->type_kind == TYPE_POINTER) return int_to_pointer(expr, to_type);
			if (to->type_kind == TYPE_ENUM) return lit_integer_to_enum(expr, to, to_type);
			break;
		case ALL_UNSIGNED_INTS:
			if (type_is_integer_unsigned(to)) return int_conversion(expr, CAST_UIUI, to, to_type);
			if (type_is_integer_signed(to)) return int_conversion(expr, CAST_UISI, to, to_type);
			if (type_is_float(to)) return int_to_float(expr, CAST_UIFP, to, to_type);
			if (to == type_bool) return integer_to_bool(expr, to_type);
			if (to->type_kind == TYPE_POINTER) return int_to_pointer(expr, to_type);
			break;
		case ALL_FLOATS:
			if (type_is_integer(to)) return float_to_integer(expr, to, to_type);
			if (to == type_bool) return float_to_bool(expr, to_type);
			if (type_is_float(to)) return float_to_float(expr, to, to_type);
			break;
		case TYPE_POINTER:
			if (type_is_integer(to)) return pointer_to_integer(expr, to_type);
			if (to->type_kind == TYPE_BOOL) return pointer_to_bool(expr, to_type);
			if (to->type_kind == TYPE_POINTER) return pointer_to_pointer(expr, to_type);
			if (to->type_kind == TYPE_SUBARRAY) return insert_cast(expr, CAST_APTSA, to_type);
			if (to->type_kind == TYPE_ANY) return insert_cast(expr, CAST_PTRANY, to_type);
			break;
		case TYPE_ANY:
			if (to->type_kind == TYPE_POINTER) return insert_cast(expr, CAST_ANYPTR, to_type);
			break;
		case TYPE_ENUM:
			if (type_is_integer(to)) return enum_to_integer(expr, from_type, to, to_type);
			if (type_is_float(to)) return enum_to_float(expr, from_type, to, to_type);
			if (to == type_bool) return enum_to_bool(expr, from_type, to_type);
			if (to->type_kind == TYPE_POINTER) return enum_to_pointer(expr, from_type, to_type);
			break;
		case TYPE_ERRTYPE:
			if (to->type_kind == TYPE_ANYERR) return err_to_anyerr(expr, to_type);
			if (to == type_bool) return err_to_bool(expr, to_type);
			if (type_is_integer(to)) return insert_cast(expr, CAST_ERINT, to_type);
			break;
		case TYPE_FLEXIBLE_ARRAY:
			return false;
		case TYPE_ARRAY:
			if (to->type_kind == TYPE_VECTOR) return insert_cast(expr, CAST_ARRVEC, to_type);
			FALLTHROUGH;
		case TYPE_STRUCT:
		case TYPE_UNION:

			if (to->type_kind == TYPE_ARRAY || to->type_kind == TYPE_STRUCT || to->type_kind == TYPE_UNION)
			{
				return insert_cast(expr, CAST_STST, to_type);
			} // Starting in a little while...
			break;
		case TYPE_SUBARRAY:
			if (to->type_kind == TYPE_POINTER) return insert_cast(expr, CAST_SAPTR, to);
			if (to->type_kind == TYPE_BOOL) return subarray_to_bool(expr);
			break;
		case TYPE_VECTOR:
			if (to->type_kind == TYPE_ARRAY) return insert_cast(expr, CAST_VECARR, to);
			break;
	}
	UNREACHABLE
}

static bool bitstruct_cast(Expr *expr, Type *from_type, Type *to, Type *to_type)
{
	Type *base_type = type_flatten_distinct(from_type->decl->bitstruct.base_type->type);
	assert(type_size(to) == type_size(base_type));
	if (type_is_integer(base_type) && type_is_integer(to))
	{
		expr->type = to_type;
		return true;
	}
	if (base_type->type_kind == TYPE_ARRAY && to->type_kind == TYPE_ARRAY)
	{
		expr->type = to_type;
		return true;
	}
	if (type_is_integer(base_type))
	{
		assert(to->type_kind == TYPE_ARRAY);
		return insert_cast(expr, CAST_BSARRY, to_type);
	}
	assert(base_type->type_kind == TYPE_ARRAY);
	return insert_cast(expr, CAST_BSINT, to_type);
}

bool cast(Expr *expr, Type *to_type)
{
	assert(!type_is_failable(to_type));
	Type *from_type = expr->type;
	bool from_is_failable = false;
	Type *to = type_flatten(to_type);

	// Special case *! => error
	if (to == type_anyerr || to->type_kind == TYPE_ERRTYPE)
	{
		if (type_is_failable(from_type)) return voidfail_to_error(expr, to, to_type);
	}

	if (type_is_failable_any(from_type))
	{
		expr->type = type_get_failable(to_type);
		return true;
	}

	if (type_is_failable_type(from_type))
	{
		from_type = from_type->failable;
		from_is_failable = true;
	}
	from_type = type_flatten_distinct(from_type);
	if (to_type->type_kind == TYPE_INFERRED_ARRAY)
	{
		to_type = from_type;
		to = type_flatten(from_type);
	}
	if (from_type == to)
	{
		expr->type = type_get_opt_fail(to_type, from_is_failable);
		if (expr->expr_kind == EXPR_CONST)
		{
			expr->const_expr.narrowable = false;
			expr->const_expr.is_hex = false;
		}
		return true;
	}
	bool result = cast_inner(expr, from_type, to, to_type);
	assert(result == true);

	Type *result_type = expr->type;
	if (from_is_failable && !type_is_failable(result_type))
	{
		expr->type = type_get_failable(result_type);
	}
	return true;
}

#pragma clang diagnostic pop