// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"

#define FLOAT32_LIMIT 340282346638528859811704183484516925440.0000000000000000
#define FLOAT64_LIMIT 179769313486231570814527423731704356798070567525844996598917476803157260780028538760589558632766878171540458953514382464234321326889464182768467546703537516986049910576551282076245490090389328944075868508455133942304583236903222948165808559332123348274797826204144723168738177180919299881250404026184124858368.0000000000000000
#define FLOAT16_LIMIT 65504

static inline bool insert_cast(Expr *expr, CastKind kind, Type *type)
{
	assert(expr->resolve_status == RESOLVE_DONE);
	assert(expr->type);
	Expr *inner = expr_copy(expr);
	expr->expr_kind = EXPR_CAST;
	expr->cast_expr.kind = kind;
	expr->cast_expr.expr = inner;
	expr->cast_expr.type_info = NULL;
	expr_set_type(expr, type);
	return true;
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
	expr_set_type(expr, type);
	return true;
}

bool pointer_to_bool(Expr *expr, Type *type)
{
	if (insert_runtime_cast_unless_const(expr, CAST_PTRBOOL, type)) return true;

	// Must have been a null
	expr->const_expr.b = false;
	expr_set_type(expr, type);
	return true;
}


bool pointer_to_pointer(Expr* expr, Type *type)
{
	if (insert_runtime_cast_unless_const(expr, CAST_PTRPTR, type)) return true;

	// Must have been a null
	expr_set_type(expr, type);
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
	Real f = bigint_as_float(&expr->const_expr.i);
	switch (canonical->type_kind)
	{
		case TYPE_F32:
			expr->const_expr.f = (float)f;
			break;
		case TYPE_F64:
			expr->const_expr.f = (double)f;
			break;
		default:
			expr->const_expr.f = f;
			break;
	}
	expr_set_type(expr, type);
	expr->const_expr.kind = canonical->type_kind;
}


/**
 * Bool into a signed or unsigned int.
 */
bool bool_to_int(Expr *expr, Type *canonical, Type *type)
{
	if (insert_runtime_cast_unless_const(expr, CAST_BOOLINT, type)) return true;
	expr_const_set_int(&expr->const_expr, expr->const_expr.b ? 1 : 0, canonical->type_kind);
	expr_set_type(expr, type);
	return true;
}


/**
 * Cast bool to float.
 */
bool bool_to_float(Expr *expr, Type *canonical, Type *type)
{
	if (insert_runtime_cast_unless_const(expr, CAST_BOOLFP, type)) return true;

	assert(expr->const_expr.kind == TYPE_BOOL);
	expr_const_set_float(&expr->const_expr, expr->const_expr.b ? 1.0 : 0.0, canonical->type_kind);
	expr_set_type(expr, type);
	return true;
}

/**
 * Convert from any into to bool.
 * @return true for any implicit conversion except assign and assign add.
 */
bool integer_to_bool(Expr *expr, Type *type)
{
	if (insert_runtime_cast_unless_const(expr, CAST_INTBOOL, type)) return true;

	expr_const_set_bool(&expr->const_expr, bigint_cmp_zero(&expr->const_expr.i) != CMP_EQ);
	expr_set_type(expr, type);
	return true;
}

/**
 * Convert from any float to bool
 */
bool float_to_bool(Expr *expr, Type *type)
{
	if (insert_runtime_cast_unless_const(expr, CAST_FPBOOL, type)) return true;

	expr_const_set_bool(&expr->const_expr, expr->const_expr.f != 0.0);
	expr_set_type(expr, type);
	return true;
}


/**
 * Convert from any fp to fp
 */
static bool float_to_float(Expr* expr, Type *canonical, Type *type)
{
	if (insert_runtime_cast_unless_const(expr, CAST_FPFP, type)) return true;

	expr_const_set_float(&expr->const_expr, expr->const_expr.f, canonical->type_kind);
	expr_set_type(expr, type);
	return true;
}

/**
 * Convert from any floating point to int
 */
bool float_to_integer(Expr *expr, Type *canonical, Type *type)
{
	bool is_signed = type_is_unsigned(canonical);
	if (insert_runtime_cast_unless_const(expr, is_signed ? CAST_FPSI : CAST_FPUI, type)) return true;

	assert(canonical->type_kind >= TYPE_I8 && canonical->type_kind < TYPE_IXX);
	Real d = expr->const_expr.f;
	BigInt temp;
	if (is_signed)
	{
		bigint_init_signed(&temp, (int64_t)d);
		bigint_truncate(&expr->const_expr.i, &temp, canonical->builtin.bitsize, true);
	}
	else
	{
		bigint_init_unsigned(&temp, (uint64_t)d);
		bigint_truncate(&expr->const_expr.i, &temp, canonical->builtin.bitsize, false);
	}
	expr->const_expr.kind = canonical->type_kind;
	expr_set_type(expr, type);
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
	bool is_signed = canonical->type_kind < TYPE_U8;
	BigInt temp;
	bigint_truncate(&temp, &expr->const_expr.i, canonical->builtin.bitsize, is_signed);
	expr->const_expr.i = temp;
	expr->const_expr.kind = canonical->type_kind;
	expr_set_type(expr, type);
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

	BigInt temp;
	bigint_truncate(&temp, &expr->const_expr.i, canonical->builtin.bitsize, kind == CAST_UISI || kind == CAST_SISI);
	expr->const_expr.i = temp;
	expr->const_expr.kind = canonical->type_kind;
	expr_set_type(expr, type);
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


static bool int_literal_to_float(Expr *expr, Type *canonical, Type *type)
{
	assert(type_is_float(canonical));
	assert(expr->expr_kind == EXPR_CONST);
	const_int_to_fp_cast(expr, canonical, type);
	return true;
}

/**
 * Convert a compile time into to a boolean.
 */
static bool int_literal_to_bool(Expr *expr, Type *type)
{
	assert(expr->expr_kind == EXPR_CONST);
	expr_const_set_bool(&expr->const_expr, bigint_cmp_zero(&expr->const_expr.i) != CMP_EQ);
	expr_set_type(expr, type);
	return true;
}


/**
 * Cast any int to a pointer -> pointer.
 */
static bool int_to_pointer(Expr *expr, Type *type)
{
	if (expr->expr_kind == EXPR_CONST)
	{
		if (bigint_cmp_zero(&expr->const_expr.i) == CMP_EQ)
		{
			expr_const_set_null(&expr->const_expr);
			expr_set_type(expr, type);
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
		case TYPE_IXX:
			return int_literal_to_int(left, canonical, type);
		case ALL_SIGNED_INTS:
			return int_conversion(left, type_is_unsigned(canonical) ? CAST_SIUI : CAST_SISI, canonical, type);
		case ALL_UNSIGNED_INTS:
			return int_conversion(left, type_is_unsigned(canonical) ? CAST_UIUI : CAST_UISI, canonical, type);
		default:
			UNREACHABLE
	}
}

static bool enum_to_integer(Expr* expr, Type *from, Type *canonical, Type *type)
{
	Type *enum_type = from->decl->enums.type_info->type;
	Type *enum_type_canonical = type_flatten(enum_type);
	// 1. If the underlying type is the same, this is just setting the type.
	if (canonical == enum_type_canonical)
	{
		expr_set_type(expr, type);
		return true;
	}
	// 2. Dispatch to the right cast:
	// TODO can be inlined if enums are constants
	insert_cast(expr, CAST_ENUMLOW, enum_type_canonical);
	return int_to_int(expr, enum_type_canonical, canonical, type);
}

static bool enum_to_float(Expr* expr, Type *from, Type *canonical, Type *type)
{
	Type *enum_type = from->decl->enums.type_info->type;
	Type *enum_type_canonical = type_flatten(enum_type);
	// TODO can be inlined if enums are constants
	insert_cast(expr, CAST_ENUMLOW, enum_type_canonical);
	return int_to_float(expr, type_is_unsigned(enum_type_canonical) ? CAST_UIFP : CAST_SIFP, canonical, type);
}

bool enum_to_bool(Expr* expr, Type *from, Type *type)
{
	Type *enum_type = from->decl->enums.type_info->type;
	Type *enum_type_canonical = type_flatten(enum_type);
	// TODO can be inlined if enums are constants
	insert_cast(expr, CAST_ENUMLOW, enum_type_canonical);
	return integer_to_bool(expr, type);
}

bool enum_to_pointer(Expr* expr, Type *from, Type *type)
{
	Type *enum_type = from->decl->enums.type_info->type;
	Type *enum_type_canonical = type_flatten(enum_type);
	// TODO can be inlined if enums are constants
	insert_cast(expr, CAST_ENUMLOW, enum_type_canonical);
	return int_to_pointer(expr, type);
}

Type *type_by_expr_range(ExprConst *expr)
{
	if (expr->kind == TYPE_FXX)
	{
		return type_double;
	}
	assert(expr->kind == TYPE_IXX);
	// 1. Does it fit in a C int? If so, that's the type.
	Type *type = type_cint();
	if (!expr_const_will_overflow(expr, type->type_kind)) return type;

	int width_max = platform_target.int128 ? 128 : 64;
	int current_width = platform_target.width_c_int * 2;
	while (current_width <= width_max)
	{
		type = type_int_signed_by_bitsize(current_width);
		if (!expr_const_will_overflow(expr, type->type_kind)) return type;
		type = type_int_unsigned_by_bitsize(current_width);
		if (!expr_const_will_overflow(expr, type->type_kind)) return type;
		current_width *= width_max;
	}
	return NULL;
}

bool cast_implicitly_to_runtime(Expr *expr)
{
	Type *canonical = expr->type->canonical;
	Type *type;
	switch (canonical->type_kind)
	{
		case TYPE_IXX:
		case TYPE_FXX:
			type = type_by_expr_range(&expr->const_expr);
			return type && cast(expr, type);
		default:
			return true;
	}
}


CastKind cast_to_bool_kind(Type *type)
{
	switch (type_flatten(type)->type_kind)
	{
		case TYPE_TYPEDEF:
		case TYPE_DISTINCT:
		case TYPE_INFERRED_ARRAY:
			UNREACHABLE
		case TYPE_VIRTUAL_ANY:
		case TYPE_VIRTUAL:
			return CAST_VRBOOL;
		case TYPE_BOOL:
			return CAST_BOOLBOOL;
		case TYPE_ERR_UNION:
			return CAST_EUBOOL;
		case TYPE_SUBARRAY:
			return CAST_SABOOL;
		case ALL_INTS:
			return CAST_INTBOOL;
		case ALL_FLOATS:
			return CAST_FPBOOL;
		case TYPE_POINTER:
			return CAST_PTRBOOL;
		case TYPE_POISONED:
		case TYPE_VOID:
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_STRLIT:
		case TYPE_ERRTYPE:
		case TYPE_ENUM:
		case TYPE_FUNC:
		case TYPE_ARRAY:
		case TYPE_TYPEID:
		case TYPE_TYPEINFO:
		case TYPE_VECTOR:
			return CAST_ERROR;
	}
	UNREACHABLE
}

bool cast_may_explicit(Type *from_type, Type *to_type)
{
	// 1. We flatten the distinct types, since they should be freely convertible
	Type *from = type_flatten_distinct(from_type);
	Type *to = type_flatten_distinct(to_type);

	// 2. Same underlying type, always ok
	if (from == to) return true;

	TypeKind to_kind = to->type_kind;
	switch (from->type_kind)
	{
		case TYPE_INFERRED_ARRAY:
		case TYPE_POISONED:
		case TYPE_VOID:
		case TYPE_TYPEINFO:
		case TYPE_DISTINCT:
		case TYPE_FUNC:
		case TYPE_TYPEDEF:
			UNREACHABLE
		case TYPE_TYPEID:
			// May convert to anything pointer sized or larger, no enums
			return type_is_pointer_sized_or_more(to);
		case TYPE_BOOL:
			// May convert to any integer / distinct integer / float, no enums
			return type_is_integer(to) || type_is_float(to);
		case TYPE_ERR_UNION:
			// May convert to a bool, or an error type.
			return to == type_bool || to_kind == TYPE_ERRTYPE;
		case TYPE_IXX:
		case ALL_SIGNED_INTS:
		case ALL_UNSIGNED_INTS:
		case TYPE_ENUM:
			// Allow conversion int/enum -> float/bool/enum int/enum -> pointer is only allowed if the int/enum is pointer sized.
			if (type_is_integer(to) || type_is_float(to) || to == type_bool || to_kind == TYPE_ENUM) return true;
			// TODO think about this, maybe we should require a bitcast?
			if (to_kind == TYPE_POINTER && (from_type == type_compint || type_is_pointer_sized(from_type))) return true;
			return false;
		case ALL_FLOATS:
			// Allow conversion float -> float/int/bool/enum
			return type_is_any_integer(to) || type_is_float(to) || to == type_bool || to_kind == TYPE_ENUM;
		case TYPE_POINTER:
			// Allow conversion ptr -> int (min pointer size)/bool/pointer/vararray
			if ((type_is_integer(to) && type_size(to) >= type_size(type_iptr)) || to == type_bool || to_kind == TYPE_POINTER) return true;
			// Special subarray conversion: someType[N]* -> someType[]
			if (to_kind == TYPE_SUBARRAY && from->pointer->type_kind == TYPE_ARRAY && from->pointer->array.base == to->array.base) return true;
			return false;
		case TYPE_VIRTUAL_ANY:
		case TYPE_VIRTUAL:
			return to_kind == TYPE_POINTER;
		case TYPE_ERRTYPE:
			// Allow only MyError.A -> error
			return to->type_kind == TYPE_ERR_UNION;
		case TYPE_ARRAY:
			if (to_kind == TYPE_VECTOR)
			{
				return to->array.len == from->vector.len && to->array.base == from->array.base;
			}
			FALLTHROUGH;
		case TYPE_STRUCT:
			if (type_is_substruct(from_type))
			{
				if (cast_may_explicit(from_type->decl->strukt.members[0]->type, to_type)) return true;
			}
			FALLTHROUGH;
		case TYPE_UNION:
			return type_is_structurally_equivalent(from, to);
		case TYPE_STRLIT:
			if (to_kind == TYPE_POINTER) return true;
			if (to_kind == TYPE_SUBARRAY && (to->array.base == type_char || to->array.base == type_ichar)) return true;
			return false;
		case TYPE_SUBARRAY:
			return to_kind == TYPE_POINTER;
		case TYPE_VECTOR:
			return type_is_structurally_equivalent(type_get_array(from->vector.base, from->vector.len), to);
	}
	UNREACHABLE
}

static bool may_cast_to_virtual(Type *virtual, Type *from)
{
	assert(from->canonical == from);

	// 1. We need a pointer, we can't cast from a non pointer.
	if (from->type_kind != TYPE_POINTER) return false;

	// 2. Virtual* converts to anything, including ints
	if (virtual->type_kind == TYPE_VIRTUAL_ANY) return true;

	// 3. Get the data.
	Decl *virtual_decl = virtual->decl;
	Decl **methods = virtual_decl->interface_decl.functions;

	// 4. No variables nor members? Then this is essentially a virtual*
	if (!vec_size(methods) && !vec_size(virtual_decl->strukt.members)) return true;

	// 5. Look at the pointer.
	Type *pointee = from->pointer;

	// 6. Is this an array, if so it doesn't have any functions,
	//    so we implicitly lower to the first element.
	if (pointee->type_kind == TYPE_ARRAY)
	{
		pointee = pointee->array.base;
	}

	// Do this: create a function that returns a matching interface method.
	// store this decl.
	// Same with looking at members -> store the Decl.
	// Later, generating the table we provide the decl backend ref and the offset.
	// Note that matching types should take into account the first element.
	// Also go recursively into substructs structs
	// Note that this resolution cannot be cached completely due to the module import lookup

	TODO;
}
/**
 * Can the conversion occur implicitly?
 */
bool cast_may_implicit(Type *from_type, Type *to_type)
{
	Type *from = from_type->canonical;
	Type *to = to_type->canonical;

	// 1. Same canonical type - we're fine.
	if (from == to) return true;

	// 2. Handle floats
	if (type_is_float(to))
	{
		// 2a. Any integer may convert to a float.
		if (type_is_any_integer(from)) return true;

		// 2b. Any narrower float or FXX may convert to a float.
		if (type_is_float(from))
		{
			// This works because the type_size of FXX = 0
			return type_size(to) >= type_size(from);
		}
		return false;
	}

	// 3. Handle ints
	if (type_is_integer(to))
	{
		// TODO, consider size here, and maybe the type should b removed.
		if (from->type_kind == TYPE_IXX) return true;

		// For an enum, lower to the underlying enum type.
		if (from->type_kind == TYPE_ENUM)
		{
			from = from->decl->enums.type_info->type->canonical;
		}

		// 3a. Any narrower int may convert to a wider or same int, regardless of signedness.
		if (type_is_integer(from))
		{
			return type_size(to) >= type_size(from);
		}
		return false;
	}

	// 4. Handle pointers
	if (type_is_pointer(to))
	{
		// 4a. Assigning a subarray or vararray to a pointer of the same base type is fine
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
				if (type_is_subtype(to->pointer, from->pointer->array.base)) return true;
			}

			// Use subtype matching
			return type_is_subtype(to->pointer, from->pointer);
		}

		// 4c. Assigning a compile time string to char* is fine. TODO fix correct later
		if (from->type_kind == TYPE_STRLIT && to->pointer == type_char) return true;

		return false;
	}

	// 5. Handle sub arrays
	if (to->type_kind == TYPE_SUBARRAY)
	{
		// 5a. Assign sized array pointer int[] = int[4]*
		if (type_is_pointer(from))
		{
			return from->pointer->type_kind == TYPE_ARRAY && from->pointer->array.base == to->array.base;
		}
		return false;
	}

	// 7. In the case of distinct types, we allow implicit conversion from literal types.
	if (to->type_kind == TYPE_DISTINCT)
	{
		if (from->type_kind == TYPE_STRLIT || from->type_kind == TYPE_FXX || from->type_kind == TYPE_IXX)
		{
			return cast_may_implicit(from, type_flatten(to));
		}
	}


	// 8. Check if we may cast this to bool. It is safe for many types.
	if (to->type_kind == TYPE_BOOL)
	{
		return cast_to_bool_kind(from) != CAST_ERROR;
	}

	// 9. Virtual any cast
	if (to->type_kind == TYPE_VIRTUAL_ANY)
	{
		return from_type->type_kind == TYPE_POINTER;
	}

	// 10. Virtual cast
	if (to->type_kind == TYPE_VIRTUAL)
	{
		return may_cast_to_virtual(to, from);
	}

	// 11. Substruct cast, if the first member is inline, see if we can cast to this member.
	if (type_is_substruct(from))
	{
		return cast_may_implicit(from->decl->strukt.members[0]->type, to);
	}

	return false;
}

bool may_convert_float_const_implicit(Expr *expr, Type *to_type)
{
	Type *to_type_flat = type_flatten(to_type);
	Real limit;
	switch (to_type_flat->type_kind)
	{
		case TYPE_F16:
			limit = FLOAT16_LIMIT;
			break;
		case TYPE_F32:
			limit = FLOAT32_LIMIT;
			break;
		case TYPE_F64:
			limit = FLOAT64_LIMIT;
			break;
		case TYPE_F128:
			// Assume this to be true
			return true;
		default:
			UNREACHABLE
	}
	if (expr->const_expr.f < -limit || expr->const_expr.f > limit)
	{
#if LONG_DOUBLE
		SEMA_ERROR(expr, "The value '%Lg' is out of range for %s, so you need an explicit cast to truncate the value.", expr->const_expr.f, type_quoted_error_string(to_type));
#else
		SEMA_ERROR(expr, "The value '%g' is out of range for %s, so you need an explicit cast to truncate the value.", expr->const_expr.f, type_quoted_error_string(to_type));
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
		SEMA_ERROR(expr, "The value '%s' is out of range for %s, it can be truncated with an explicit cast.", bigint_to_error_string(&expr->const_expr.i, 10), type_quoted_error_string(to_type));
		return false;
	}
	return true;
}

bool may_convert_const_implicit(Expr *expr, Type *to_type)
{
	switch (expr->type->canonical->type_kind)
	{
		case TYPE_FXX:
			return may_convert_float_const_implicit(expr, to_type);
		case TYPE_IXX:
			return may_convert_int_const_implicit(expr, to_type);
		default:
			UNREACHABLE

	}
}
bool cast_implicit(Expr *expr, Type *to_type)
{
	assert(expr->original_type);
	if (expr->type == to_type) return true;
	if (!cast_may_implicit(expr->original_type, to_type) && !cast_may_implicit(expr->type->canonical, to_type))
	{
		SEMA_ERROR(expr, "Cannot implicitly cast %s to %s.", type_quoted_error_string(expr->original_type), type_quoted_error_string(to_type));
		return false;
	}
	// Additional checks for compile time values.
	if (expr->expr_kind == EXPR_CONST)
	{
		if (expr->type->type_kind == TYPE_FXX)
		{
			if (!may_convert_float_const_implicit(expr, to_type)) return false;
		}
		else if (expr->type->type_kind == TYPE_IXX)
		{
			if (!may_convert_int_const_implicit(expr, to_type)) return false;
		}
	}
	Type *original_type = expr->original_type ? expr->original_type : expr->type;
	cast(expr, to_type);
	expr->original_type = original_type;
	return true;
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
				to_type = type_int_unsigned_by_bitsize(type_size(to_canonical) * 8);
			}
			else
			{
				to_type = type_int_signed_by_bitsize(type_size(to_canonical) * 8);
			}
		}
	}
	return cast_implicit(expr, to_type);
}

bool cast(Expr *expr, Type *to_type)
{
	Type *from_type = type_flatten(expr->type->canonical);
	Type *canonical = type_lowering(to_type);
	if (from_type == canonical)
	{
		expr_set_type(expr, to_type);
		return true;
	}
	switch (from_type->type_kind)
	{
		case TYPE_INFERRED_ARRAY:
		case TYPE_POISONED:
		case TYPE_VOID:
		case TYPE_TYPEID:
		case TYPE_TYPEINFO:
		case TYPE_DISTINCT:
		case TYPE_FUNC:
		case TYPE_TYPEDEF:
			UNREACHABLE
		case TYPE_BOOL:
			// Bool may convert into integers and floats but only explicitly.
			if (type_is_integer(canonical)) return bool_to_int(expr, canonical, to_type);
			if (type_is_float(canonical)) return bool_to_float(expr, canonical, to_type);
			break;
		case TYPE_ERR_UNION:
			if (canonical->type_kind == TYPE_BOOL) return insert_cast(expr, CAST_EUBOOL, to_type);
			if (canonical->type_kind == TYPE_ERRTYPE) return insert_cast(expr, CAST_EUER, to_type);
			break;
		case TYPE_IXX:
			if (type_is_integer(canonical)) return int_literal_to_int(expr, canonical, to_type);
			if (type_is_float(canonical)) return int_literal_to_float(expr, canonical, to_type);
			if (canonical == type_bool) return int_literal_to_bool(expr, to_type);
			if (canonical->type_kind == TYPE_POINTER) return int_to_pointer(expr, to_type);
			if (canonical->type_kind == TYPE_ENUM) return lit_integer_to_enum(expr, canonical, to_type);
			break;
		case ALL_SIGNED_INTS:
			if (type_is_integer_unsigned(canonical)) return int_conversion(expr, CAST_SIUI, canonical, to_type);
			if (type_is_integer_signed(canonical)) return int_conversion(expr, CAST_SISI, canonical, to_type);
			if (type_is_float(canonical)) return int_to_float(expr, CAST_SIFP, canonical, to_type);
			if (canonical == type_bool) return integer_to_bool(expr, to_type);
			if (canonical->type_kind == TYPE_POINTER) return int_to_pointer(expr, to_type);
			if (canonical->type_kind == TYPE_ENUM) return lit_integer_to_enum(expr, canonical, to_type);
			break;
		case ALL_UNSIGNED_INTS:
			if (type_is_integer_unsigned(canonical)) return int_conversion(expr, CAST_UIUI, canonical, to_type);
			if (type_is_integer_signed(canonical)) return int_conversion(expr, CAST_UISI, canonical, to_type);
			if (type_is_float(canonical)) return int_to_float(expr, CAST_UIFP, canonical, to_type);
			if (canonical == type_bool) return integer_to_bool(expr, to_type);
			if (canonical->type_kind == TYPE_POINTER) return int_to_pointer(expr, to_type);
			break;
		case ALL_FLOATS:
			if (type_is_integer(canonical)) return float_to_integer(expr, canonical, to_type);
			if (canonical == type_bool) return float_to_bool(expr, to_type);
			if (type_is_float(canonical)) return float_to_float(expr, canonical, to_type);
			break;
		case TYPE_POINTER:
			if (type_is_integer(canonical)) return pointer_to_integer(expr, to_type);
			if (canonical->type_kind == TYPE_BOOL) return pointer_to_bool(expr, to_type);
			if (canonical->type_kind == TYPE_POINTER) return pointer_to_pointer(expr, to_type);
			if (canonical->type_kind == TYPE_SUBARRAY) return insert_cast(expr, CAST_APTSA, to_type);
			break;
		case TYPE_VIRTUAL:
		case TYPE_VIRTUAL_ANY:
			if (canonical->type_kind == TYPE_POINTER) return insert_cast(expr, CAST_VRPTR, to_type);
			break;
		case TYPE_ENUM:
			if (type_is_integer(canonical)) return enum_to_integer(expr, from_type, canonical, to_type);
			if (type_is_float(canonical)) return enum_to_float(expr, from_type, canonical, to_type);
			if (canonical == type_bool) return enum_to_bool(expr, from_type, to_type);
			if (canonical->type_kind == TYPE_POINTER) return enum_to_pointer(expr, from_type, to_type);
			break;
		case TYPE_ERRTYPE:
			if (canonical->type_kind == TYPE_ERR_UNION) return insert_cast(expr, CAST_EREU, to_type);
			break;
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_ARRAY:
			if (canonical->type_kind == TYPE_ARRAY || canonical->type_kind == TYPE_STRUCT || canonical->type_kind == TYPE_UNION)
			{
				return insert_cast(expr, CAST_STST, to_type);
			} // Starting in a little while...
			break;
		case TYPE_STRLIT:
			canonical = type_flatten(canonical);
			if (canonical->type_kind == TYPE_POINTER) return insert_cast(expr, CAST_STRPTR, to_type);
			if (canonical->type_kind == TYPE_SUBARRAY) return string_literal_to_subarray(expr, to_type);
			break;
		case TYPE_SUBARRAY:
			if (canonical->type_kind == TYPE_POINTER) return insert_cast(expr, CAST_SAPTR, canonical);
			break;
		case TYPE_VECTOR:
			TODO
	}
	UNREACHABLE
}
