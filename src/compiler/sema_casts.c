// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "sema_internal.h"

#pragma clang diagnostic push
#pragma ide diagnostic ignored "ConstantFunctionResult"


static inline bool sema_error_cannot_convert(Expr *expr, Type *to, bool may_cast_explicit, bool silent);
static bool cast_expr_inner(SemaContext *context, Expr *expr, Type *to_type, bool is_explicit, bool silent,
                            bool may_not_be_optional);
static bool bitstruct_cast(Expr *expr, Type *from_type, Type *to, Type *to_type);
static void sema_error_const_int_out_of_range(Expr *expr, Expr *problem, Type *to_type);
static Expr *recursive_may_narrow_float(Expr *expr, Type *type);
static Expr *recursive_may_narrow_int(Expr *expr, Type *type);
static void recursively_rewrite_untyped_list(Expr *expr, Expr **list);
static inline bool cast_may_implicit_ptr(Type *from_pointee, Type *to_pointee);
static inline bool cast_may_array(Type *from, Type *to, bool is_explicit);

static inline bool insert_cast(Expr *expr, CastKind kind, Type *type)
{
	assert(expr->resolve_status == RESOLVE_DONE);
	assert(expr->type);
	Expr *inner = expr_copy(expr);
	expr->expr_kind = EXPR_CAST;
	expr->cast_expr.kind = kind;
	expr->cast_expr.expr = exprid(inner);
	expr->cast_expr.type_info = 0;
	expr->type = type;
	return true;
}

bool sema_error_failed_cast(Expr *expr, Type *from, Type *to)
{
	SEMA_ERROR(expr, "The cast %s to %s is not allowed.", type_quoted_error_string(from), type_quoted_error_string(to));
	return false;
}


Type *cast_infer_len(Type *to_infer, Type *actual_type)
{
	Type *may_infer = to_infer->canonical;
	Type *actual = actual_type->canonical;
	if (may_infer == actual) return to_infer;
	bool canonical_same_kind = may_infer->type_kind == to_infer->type_kind;
	assert(type_is_arraylike(actual_type));
	if (may_infer->type_kind == TYPE_INFERRED_ARRAY)
	{
		Type *base_type = cast_infer_len(canonical_same_kind ? to_infer->array.base :
		                                 may_infer->array.base, actual->array.base);
		return type_get_array(base_type, actual->array.len);
	}
	if (may_infer->type_kind == TYPE_INFERRED_VECTOR)
	{
		Type *base_type = cast_infer_len(canonical_same_kind ? to_infer->array.base : may_infer->array.base, actual->array.base);
		if (actual_type->type_kind == TYPE_SCALED_VECTOR)
		{
			return type_get_scaled_vector(base_type);
		}
		return type_get_vector(base_type, actual->array.len);
	}
	if (may_infer->type_kind == TYPE_POINTER)
	{
		assert(actual->type_kind == TYPE_POINTER);
		Type *base_type = cast_infer_len(canonical_same_kind ? to_infer->array.base : may_infer->pointer, actual->pointer);
		return type_get_ptr(base_type);
	}
	UNREACHABLE
}

static inline bool insert_runtime_cast_unless_const(Expr *expr, CastKind kind, Type *type)
{
	if (expr->expr_kind == EXPR_CONST) return false;
	return insert_cast(expr, kind, type);
}


bool pointer_to_integer(Expr *expr, Type *type)
{
	if (insert_runtime_cast_unless_const(expr, CAST_PTRXI, type)) return true;

	// Revisit this to support pointers > 64 bits.
	expr_rewrite_const_int(expr, type, expr->const_expr.ptr, false);
	return true;
}

bool pointer_to_bool(Expr *expr, Type *type)
{
	if (insert_runtime_cast_unless_const(expr, CAST_PTRBOOL, type)) return true;

	if (expr->const_expr.const_kind == CONST_POINTER)
	{
		expr_rewrite_const_bool(expr, type, expr->const_expr.ptr != 0);
		return true;
	}
	assert(expr->const_expr.const_kind == CONST_STRING);
	expr_rewrite_const_bool(expr, type, true);
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
	expr_rewrite_const_int(expr, type, expr->const_expr.b ? 1 : 0, false);
	return true;
}


/**
 * Cast bool to float.
 */
bool bool_to_float(Expr *expr, Type *canonical, Type *type)
{
	if (insert_runtime_cast_unless_const(expr, CAST_BOOLFP, type)) return true;

	assert(expr->const_expr.const_kind == CONST_BOOL);
	expr_rewrite_const_float(expr, type, expr->const_expr.b ? 1.0 : 0.0);
	return true;
}

/**
 * Cast bool to float.
 */
bool voidfail_to_error(Expr *expr, Type *type)
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

	expr_rewrite_const_bool(expr, type, !int_is_zero(expr->const_expr.ixx));
	return true;
}

/**
 * Convert from any float to bool
 */
bool float_to_bool(Expr *expr, Type *type)
{
	if (insert_runtime_cast_unless_const(expr, CAST_FPBOOL, type)) return true;

	expr_rewrite_const_bool(expr, type, expr->const_expr.fxx.f != 0.0);
	return true;
}


/**
 * Convert from any fp to fp
 */
static bool float_to_float(Expr* expr, Type *canonical, Type *type)
{
	if (insert_runtime_cast_unless_const(expr, CAST_FPFP, type)) return true;
	expr_rewrite_const_float(expr, type, expr->const_expr.fxx.f);
	return true;
}

/**
 * Convert from any floating point to int
 */
bool float_to_integer(Expr *expr, Type *canonical, Type *type)
{
	bool is_signed = type_is_signed(canonical);
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
bool integer_to_enum(Expr *expr, Type *canonical, Type *type)
{
	assert(canonical->type_kind == TYPE_ENUM);
	Decl *enum_decl = canonical->decl;
	if (expr->expr_kind != EXPR_CONST)
	{
		Type *underlying_type = enum_decl->enums.type_info->type->canonical;
		if (!cast(expr, underlying_type)) return false;
		return insert_cast(expr, CAST_INTENUM, type);
	}
	unsigned max_enums = vec_size(enum_decl->enums.values);
	Int to_convert = expr->const_expr.ixx;
	if (int_is_neg(to_convert))
	{
		SEMA_ERROR(expr, "A negative number cannot be converted to an enum.");
		return false;
	}
	Int max = { .i.low = max_enums, .type = TYPE_I32 };
	if (int_comp(to_convert, max, BINARYOP_GE))
	{
		SEMA_ERROR(expr, "This value exceeds the number of enums in %s.", canonical->decl->name);
		return false;
	}
	Decl *decl = enum_decl->enums.values[to_convert.i.low];
	expr->const_expr = (ExprConst) {
		.enum_err_val = decl,
		.const_kind = CONST_ENUM
	};
	expr->type = type;
	return true;
}


static bool int_conversion(Expr *expr, CastKind kind, Type *canonical, Type *type)
{
	// Fold pointer casts if narrowing
	if (expr->expr_kind == EXPR_CAST && expr->cast_expr.kind == CAST_PTRXI
	    && type_size(type) <= type_size(expr->type))
	{
		expr->type = type;
		return true;
	}
	if (insert_runtime_cast_unless_const(expr, kind, type)) return true;

	expr->const_expr.ixx = int_conv(expr->const_expr.ixx, canonical->type_kind);
	expr->const_expr.const_kind = CONST_INTEGER;
	expr->type = type;
	expr->const_expr.narrowable = false;
	expr->const_expr.is_hex = false;
	return true;
}

static bool int_vector_conversion(Expr *expr, Type *canonical, Type *type)
{
	// Fold pointer casts if narrowing
	Type *base = type_get_indexed_type(type);
	cast(expr, base);
	return insert_cast(expr, CAST_NUMVEC, type);
}

static bool float_vector_conversion(Expr *expr, Type *canonical, Type *type)
{
	// Fold pointer casts if narrowing
	Type *base = type_get_indexed_type(type);
	cast(expr, base);
	return insert_cast(expr, CAST_NUMVEC, type);
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
	expr_rewrite_const_bool(expr, type, !int_is_zero(expr->const_expr.ixx));
	return true;
}


/**
 * Cast any int to a pointer -> pointer.
 */
static bool int_to_pointer(Expr *expr, Type *type)
{
	assert(type_bit_size(type_uptr) <= 64 && "For > 64 bit pointers, this code needs updating.");
	if (expr->expr_kind == EXPR_CONST)
	{
		if (!int_fits(expr->const_expr.ixx, type_uptr->canonical->type_kind))
		{
			SEMA_ERROR(expr, "'0x%s' does not fit in a pointer.", int_to_str(expr->const_expr.ixx, 16));
			return false;
		}
		expr->const_expr.ptr = expr->const_expr.ixx.i.low;
		expr->type = type;
		expr->const_expr.const_kind = CONST_POINTER;
		return true;
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

static Type *enum_to_int_cast(Expr* expr, Type *from)
{
	assert(from->type_kind == TYPE_ENUM);
	Type *original = from->decl->enums.type_info->type;
	expr->type = original;
	if (expr->expr_kind == EXPR_CONST && expr->const_expr.const_kind == CONST_ENUM)
	{
		expr_rewrite_const_int(expr, original, expr->const_expr.enum_err_val->enum_constant.ordinal, false);
		return original;
	}
	insert_cast(expr, CAST_ENUMLOW, type_add_optional(original, IS_OPTIONAL(expr)));
	return original;
}

static bool enum_to_integer(Expr* expr, Type *from, Type *canonical, Type *type)
{
	Type *result = enum_to_int_cast(expr, from);
	return int_to_int(expr, result->canonical, canonical, type);
}

static bool enum_to_float(Expr* expr, Type *from, Type *canonical, Type *type)
{
	Type *result = enum_to_int_cast(expr, from);
	return int_to_float(expr, type_is_unsigned(result->canonical) ? CAST_UIFP : CAST_SIFP, canonical, type);
}

bool enum_to_bool(Expr* expr, Type *from, Type *type)
{
	enum_to_int_cast(expr, from);
	return integer_to_bool(expr, type);
}

bool enum_to_pointer(Expr* expr, Type *from, Type *type)
{
	enum_to_int_cast(expr, from);
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
		case TYPE_FAULTTYPE:
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
		case TYPE_OPTIONAL:
		case TYPE_ANY:
		case TYPE_OPTIONAL_ANY:
		case TYPE_FLEXIBLE_ARRAY:
		case TYPE_SCALED_VECTOR:
		case TYPE_INFERRED_VECTOR:
		case TYPE_MEMBER:
			return CAST_ERROR;
	}
	UNREACHABLE
}


bool type_may_convert_to_anyerr(Type *type)
{
	if (type_is_optional_any(type)) return true;
	if (!type_is_optional_type(type)) return false;
	return type->optional->canonical == type_void;
}

static inline bool cast_may_array(Type *from, Type *to, bool is_explicit)
{
	RETRY:;
	assert(!type_is_optional(from) && !type_is_optional(to) && "Optional should already been handled");

	bool compare_len = true;
	if (from->type_kind != to->type_kind)
	{
		switch (to->type_kind)
		{
			case TYPE_INFERRED_ARRAY:
				switch (from->type_kind)
				{
					case TYPE_INFERRED_VECTOR:
					case TYPE_VECTOR:
						if (!is_explicit) return false;
						FALLTHROUGH;
					case TYPE_ARRAY:
						compare_len = false;
						break;
					default:
						return false;
				}
				break;
			case TYPE_ARRAY:
				switch (from->type_kind)
				{
					case TYPE_INFERRED_VECTOR:
						compare_len = false;
						FALLTHROUGH;
					case TYPE_VECTOR:
						if (!is_explicit) return false;
						break;
					case TYPE_INFERRED_ARRAY:
						compare_len = false;
						break;
					default:
						return false;
				}
				break;
			case TYPE_INFERRED_VECTOR:
				switch (from->type_kind)
				{
					case TYPE_INFERRED_ARRAY:
					case TYPE_ARRAY:
						if (!is_explicit) return false;
						FALLTHROUGH;
					case TYPE_VECTOR:
					case TYPE_SCALED_VECTOR:
						compare_len = false;
						break;
					default:
						return false;
				}
				break;
			case TYPE_VECTOR:
				switch (from->type_kind)
				{
					case TYPE_INFERRED_ARRAY:
						compare_len = false;
						FALLTHROUGH;
					case TYPE_ARRAY:
						if (!is_explicit) return false;
						break;
					case TYPE_INFERRED_VECTOR:
						compare_len = false;
						break;
					default:
						return false;
				}
				break;
			case TYPE_SCALED_VECTOR:
				if (from->type_kind != TYPE_INFERRED_VECTOR) return false;
				compare_len = false;
				break;
			default:
				return false;
		}
	}
	if (compare_len && to->array.len != from->array.len) return false;

	Type *from_base = from->array.base;
	Type *to_base = to->array.base;
	if (is_explicit)
	{
		from_base = type_flatten(from_base);
		to_base = type_flatten(to_base);
	}

	if (from_base == to_base) return true;

	switch (to_base->type_kind)
	{
		case TYPE_POINTER:
			if (from_base->type_kind == TYPE_POINTER)
			{
				if (is_explicit) return true;
				return cast_may_implicit_ptr(to_base->pointer, from_base->pointer);
			}
			return false;
		case TYPE_ARRAY:
		case TYPE_INFERRED_ARRAY:
		case TYPE_VECTOR:
		case TYPE_INFERRED_VECTOR:
			to = to_base;
			from = from_base;
			goto RETRY;
		default:
			return is_explicit && type_is_structurally_equivalent(to_base, from_base);
	}
}

static inline bool cast_may_implicit_ptr(Type *from_pointee, Type *to_pointee)
{
	assert(!type_is_optional(from_pointee) && !type_is_optional(to_pointee) && "Optional should already been handled");
	if (from_pointee == to_pointee) return true;

	// For void* on either side, no checks.
	if (to_pointee == type_voidptr || from_pointee == type_voidptr) return true;

	// Step through all *:
	while (from_pointee->type_kind == TYPE_POINTER && to_pointee->type_kind == TYPE_POINTER)
	{
		if (from_pointee == type_voidptr || to_pointee == type_voidptr) return true;
		from_pointee = from_pointee->pointer;
		to_pointee = to_pointee->pointer;
	}

	assert(to_pointee != from_pointee);

	// Functions compare raw types.
	if (from_pointee->type_kind == TYPE_FUNC && to_pointee->type_kind == TYPE_FUNC)
	{
		return to_pointee->function.prototype->raw_type == from_pointee->function.prototype->raw_type;
	}

	// Special handling of int* = int[4]* (so we have int[4] -> int)
	if (type_is_arraylike(from_pointee))
	{
		if (cast_may_implicit_ptr(to_pointee, from_pointee->array.base)) return true;
	}

	if (type_is_any_arraylike(to_pointee) || type_is_any_arraylike(from_pointee))
	{
		return cast_may_array(from_pointee, to_pointee, false);
	}
	// Use subtype matching
	return type_is_subtype(to_pointee, from_pointee);
}

bool cast_may_bool_convert(Type *type)
{
	switch (type_flatten_distinct(type)->type_kind)
	{
		case TYPE_BOOL:
		case ALL_INTS:
		case ALL_FLOATS:
		case TYPE_SUBARRAY:
		case TYPE_POINTER:
		case TYPE_ANY:
		case TYPE_FAULTTYPE:
		case TYPE_ANYERR:
			return true;
		default:
			return false;
	}
	return true;
}

INLINE Expr *recursive_may_narrow_floatid(ExprId expr, Type *type)
{
	return recursive_may_narrow_float(exprptr(expr), type);
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
				case BINARYOP_ELSE:
				{
					Expr *res = recursive_may_narrow_float(exprptr(expr->binary_expr.left), type);
					if (res) return res;
					return recursive_may_narrow_float(exprptr(expr->binary_expr.right), type);
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
					return recursive_may_narrow_float(exprptr(expr->binary_expr.left), type);
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
		case EXPR_IDENTIFIER:
		case EXPR_SLICE_ASSIGN:
		case EXPR_SLICE_COPY:
		case EXPR_SLICE:
		case EXPR_SUBSCRIPT:
		case EXPR_RETVAL:
		case EXPR_TYPEID_INFO:
			if (type_size(expr->type) > type_size(type)) return expr;
			return NULL;
		case EXPR_EXPRESSION_LIST:
			return recursive_may_narrow_float(VECLAST(expr->expression_list), type);
		case EXPR_GROUP:
		case EXPR_FORCE_UNWRAP:
			return recursive_may_narrow_float(expr->inner_expr, type);
		case EXPR_RETHROW:
			return recursive_may_narrow_float(expr->rethrow_expr.inner, type);
		case EXPR_TERNARY:
		{
			Expr *res = recursive_may_narrow_floatid(expr->ternary_expr.then_expr ? expr->ternary_expr.then_expr
			                                                                      : expr->ternary_expr.cond, type);
			if (res) return res;
			return recursive_may_narrow_floatid(expr->ternary_expr.else_expr, type);
		}
		case EXPR_CAST:
			return recursive_may_narrow_floatid(expr->cast_expr.expr, type);
		case EXPR_CONST:
			if (!expr->const_expr.narrowable)
			{
				return type_size(expr->type) > type_size(type) ? expr : NULL;
			}
			assert(expr->const_expr.const_kind == CONST_FLOAT);
			if (!expr_const_float_fits_type(&expr->const_expr, type_flatten(type)->type_kind))
			{
				return expr;
			}
			return NULL;
		case EXPR_OPTIONAL:
		case EXPR_HASH_IDENT:
		case EXPR_FLATPATH:
		case EXPR_INITIALIZER_LIST:
		case EXPR_DESIGNATED_INITIALIZER_LIST:
		case EXPR_TYPEID:
		case EXPR_TYPEINFO:
		case EXPR_CT_CALL:
		case EXPR_NOP:
		case EXPR_CATCH:
		case EXPR_BUILTIN:
		case EXPR_TRY_UNWRAP:
		case EXPR_TRY_UNWRAP_CHAIN:
		case EXPR_SUBSCRIPT_ADDR:
		case EXPR_VARIANTSWITCH:
		case EXPR_COMPILER_CONST:
		case EXPR_STRINGIFY:
		case EXPR_CT_EVAL:
		case EXPR_VARIANT:
		case EXPR_POINTER_OFFSET:
		case EXPR_CT_ARG:
		case EXPR_ASM:
		case EXPR_VASPLAT:
		case EXPR_OPERATOR_CHARS:
		case EXPR_CT_CHECKS:
		case EXPR_SUBSCRIPT_ASSIGN:
		case EXPR_SWIZZLE:
		case EXPR_LAMBDA:
			UNREACHABLE
		case EXPR_BUILTIN_ACCESS:
		case EXPR_TEST_HOOK:
			return false;
		case EXPR_POST_UNARY:
			return recursive_may_narrow_float(expr->unary_expr.expr, type);
		case EXPR_TRY:
			return recursive_may_narrow_float(expr->inner_expr, type);
		case EXPR_UNARY:
		{
			switch (expr->unary_expr.operator)
			{
				case UNARYOP_DEREF:
					return false;
				case UNARYOP_ERROR:
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

INLINE Expr *recursive_may_narrow_intid(ExprId expr, Type *type)
{
	assert(expr);
	return recursive_may_narrow_int(exprptr(expr), type);
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
				case BINARYOP_ELSE:
				{
					Expr *res = recursive_may_narrow_int(exprptr(expr->binary_expr.left), type);
					if (res) return res;
					return recursive_may_narrow_int(exprptr(expr->binary_expr.right), type);
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
					return recursive_may_narrow_int(exprptr(expr->binary_expr.left), type);
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
		case EXPR_IDENTIFIER:
		case EXPR_SLICE_ASSIGN:
		case EXPR_SLICE_COPY:
		case EXPR_SLICE:
		case EXPR_SUBSCRIPT:
		case EXPR_RETVAL:
		case EXPR_SUBSCRIPT_ASSIGN:
		case EXPR_TYPEID_INFO:
			if (type_size(expr->type) > type_size(type)) return expr;
			return NULL;
		case EXPR_BUILTIN_ACCESS:
			switch (expr->builtin_access_expr.kind)
			{
				case ACCESS_LEN:
					if (type_size(type) < type_size(type_cint)) return expr;
					return NULL;
				case ACCESS_TYPEOFANY:
				case ACCESS_PTR:
				case ACCESS_ENUMNAME:
				case ACCESS_FAULTNAME:
					return NULL;
			}
			UNREACHABLE;
		case EXPR_EXPRESSION_LIST:
			return recursive_may_narrow_int(VECLAST(expr->expression_list), type);
		case EXPR_RETHROW:
			return recursive_may_narrow_int(expr->rethrow_expr.inner, type);
		case EXPR_TERNARY:
		{
			Expr *res = recursive_may_narrow_intid(expr->ternary_expr.then_expr ? expr->ternary_expr.then_expr
			                                                                  : expr->ternary_expr.cond, type);
			if (res) return res;
			return recursive_may_narrow_intid(expr->ternary_expr.else_expr, type);
		}
		case EXPR_CAST:
			return recursive_may_narrow_intid(expr->cast_expr.expr, type);
		case EXPR_CONST:
			assert(expr->const_expr.const_kind == CONST_INTEGER || expr->const_expr.const_kind == CONST_ENUM);
			if (expr_const_will_overflow(&expr->const_expr, type_flatten(type)->type_kind))
			{
				return expr;
			}
			return NULL;
		case EXPR_OPTIONAL:
		case EXPR_HASH_IDENT:
		case EXPR_FLATPATH:
		case EXPR_INITIALIZER_LIST:
		case EXPR_DESIGNATED_INITIALIZER_LIST:
		case EXPR_TYPEID:
		case EXPR_TYPEINFO:
		case EXPR_CT_CALL:
		case EXPR_NOP:
		case EXPR_BUILTIN:
		case EXPR_TRY_UNWRAP:
		case EXPR_TRY_UNWRAP_CHAIN:
		case EXPR_SUBSCRIPT_ADDR:
		case EXPR_VARIANTSWITCH:
		case EXPR_COMPILER_CONST:
		case EXPR_STRINGIFY:
		case EXPR_CT_EVAL:
		case EXPR_VARIANT:
		case EXPR_POINTER_OFFSET:
		case EXPR_CT_ARG:
		case EXPR_ASM:
		case EXPR_VASPLAT:
		case EXPR_OPERATOR_CHARS:
		case EXPR_CT_CHECKS:
		case EXPR_SWIZZLE:
		case EXPR_LAMBDA:
			UNREACHABLE
		case EXPR_TEST_HOOK:
			return false;
		case EXPR_POST_UNARY:
			return recursive_may_narrow_int(expr->unary_expr.expr, type);
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
	if (expr->const_expr.const_kind == CONST_ENUM)
	{
		SEMA_ERROR(problem, "The ordinal '%d' is out of range for %s, so you need an explicit cast to truncate the value.",
				   expr->const_expr.enum_err_val->var.index,
		           type_quoted_error_string(to_type));
		return;
	}
	const char *error_value = expr->const_expr.is_hex ? int_to_str(expr->const_expr.ixx, 16) : expr_const_to_error_string(&expr->const_expr);
	SEMA_ERROR(problem, "The value '%s' is out of range for %s, so you need an explicit cast to truncate the value.", error_value,
	           type_quoted_error_string(to_type));
}


static inline bool cast_maybe_string_lit_to_char_array(Expr *expr, Type *expr_canonical, Type *to_canonical, Type *to_original)
{
	if (expr->expr_kind != EXPR_CONST || expr->const_expr.const_kind != CONST_STRING) return false;
	if (expr_canonical->type_kind != TYPE_POINTER) return false;
	if (to_canonical->type_kind != TYPE_ARRAY && to_canonical->type_kind != TYPE_INFERRED_ARRAY) return false;
	if (to_canonical->array.base != type_char) return false;
	Type *pointer = expr_canonical->pointer;
	if (pointer->type_kind != TYPE_ARRAY) return false;
	if (pointer->array.base != type_char) return false;
	assert(!type_is_optional(expr->type));
	if (to_canonical->type_kind == TYPE_INFERRED_ARRAY)
	{
		assert(to_original->type_kind == TYPE_INFERRED_ARRAY);
		to_original = type_get_array(to_original->array.base, pointer->array.len);
	}
	expr->type = to_original;
	return true;
}

bool cast_untyped_to_type(SemaContext *context, Expr *expr, Type *to_type)
{
	recursively_rewrite_untyped_list(expr, expr->const_expr.untyped_list);
	if (!sema_expr_analyse_initializer_list(context, type_flatten(to_type), expr)) return false;
	expr->type = to_type;
	return true;
}

static void recursively_rewrite_untyped_list(Expr *expr, Expr **list)
{
	expr->expr_kind = EXPR_INITIALIZER_LIST;
	expr->initializer_list = list;
	expr->resolve_status = RESOLVE_NOT_DONE;
	FOREACH_BEGIN(Expr *inner, list)
		if (expr_is_const_untyped_list(inner))
		{
			recursively_rewrite_untyped_list(inner, inner->const_expr.untyped_list);
		}
	FOREACH_END();
}

bool cast_implicit(SemaContext *context, Expr *expr, Type *to_type)
{
	return cast_expr_inner(context, expr, to_type, false, false, false);
}

bool cast_implicit_maybe_optional(SemaContext *context, Expr *expr, Type *to_type, bool may_be_optional)
{
	return cast_expr_inner(context, expr, to_type, false, false, !may_be_optional);
}

bool cast_implicit_silent(SemaContext *context, Expr *expr, Type *to_type)
{
	return cast_expr_inner(context, expr, to_type, false, true, false);
}

bool cast_explicit(SemaContext *context, Expr *expr, Type *to_type)
{
	if (!cast_expr_inner(context, expr, to_type, true, false, false)) return false;
	if (expr_is_const(expr)) expr->const_expr.narrowable = false;
	return true;
}

static inline bool cast_with_optional(Expr *expr, Type *to_type, bool add_optional)
{
	if (!cast(expr, to_type)) return false;
	if (add_optional) expr->type = type_add_optional(expr->type, true);
	return true;
}

static inline bool sema_error_cannot_convert(Expr *expr, Type *to, bool may_cast_explicit, bool silent)
{
	if (silent) return false;
	if (may_cast_explicit)
	{
		SEMA_ERROR(expr,
		           "Implicitly casting %s to %s is not permitted, but you can do an explicit cast by placing '(%s)' before the expression.",
		           type_quoted_error_string(type_no_optional(expr->type)),
		           type_quoted_error_string(to),
		           type_to_error_string(type_no_optional(to)));
	}
	else
	{
		SEMA_ERROR(expr,
		           "It is not possible to convert %s to %s.",
				   type_quoted_error_string(type_no_optional(expr->type)), type_quoted_error_string(to));
	}
	return false;

}

static inline bool cast_subarray(SemaContext *context, Expr *expr, Type *from, Type *to, Type *to_type, bool add_optional, bool is_explicit, bool silent)
{
	switch (to->type_kind)
	{
		case TYPE_SUBARRAY:
			if (type_array_element_is_equivalent(from->array.base, to->array.base, is_explicit)) goto CAST;
			return sema_error_cannot_convert(expr, to, !is_explicit && type_array_element_is_equivalent(from->array.base, to->array.base, true), silent);
		case TYPE_POINTER:
			if (to == type_voidptr) goto CAST;
			if (type_array_element_is_equivalent(from->array.base, to->pointer, is_explicit)) goto CAST;
			return sema_error_cannot_convert(expr, to, !is_explicit && type_array_element_is_equivalent(from->array.base, to->pointer, true), silent);
		case TYPE_BOOL:
			if (!is_explicit) goto CAST_MAY_EXPLICIT;
			goto CAST;
		default:
			goto CAST_ILLEGAL;
	}
CAST_ILLEGAL:
	return sema_error_cannot_convert(expr, to_type, false, silent);
CAST_MAY_EXPLICIT:
	return sema_error_cannot_convert(expr, to_type, true, silent);
CAST:
	return cast_with_optional(expr, to_type, add_optional);
}

static inline bool cast_array(SemaContext *context, Expr *expr, Type *from, Type *to, Type *to_type, bool add_optional, bool is_explicit, bool silent)
{
	bool infer_type = false;
	switch (to->type_kind)
	{
		case TYPE_INFERRED_ARRAY:
		case TYPE_INFERRED_VECTOR:
			infer_type = true;
			goto CAST_ELEMENT;
		case TYPE_VECTOR:
		case TYPE_ARRAY:
			if (to->array.len != from->array.len) goto CAST_ILLEGAL;
			goto CAST_ELEMENT;
		case TYPE_STRUCT:
			if (type_is_structurally_equivalent(from, to))
			{
				if (is_explicit) goto CAST;
				if (silent) return false;
				return sema_error_cannot_convert(expr, to_type, true, silent);
			}
		default:
			goto CAST_ILLEGAL;
	}
CAST_ELEMENT:
	if (!type_array_element_is_equivalent(from->array.base, to->array.base, is_explicit))
	{
		if (!silent && !is_explicit && type_array_element_is_equivalent(from->array.base, to->array.base, true))
		{
			return sema_error_cannot_convert(expr, to_type, true, silent);
		}
		goto CAST_ILLEGAL;
	}
CAST:
	if (infer_type)
	{
		to_type = cast_infer_len(to_type, from);
	}
	return cast_with_optional(expr, to_type, add_optional);
CAST_ILLEGAL:
	return sema_error_cannot_convert(expr, to_type, false, silent);
}

static bool cast_may_number_convert(Expr *expr, Type *from, Type *to, bool is_explicit, bool cast_from_bool)
{
	// Same canonical type - we're fine.
	if (from == to) return true;

	if (from->type_kind == TYPE_DISTINCT || to->type_kind == TYPE_DISTINCT) return false;

	// Handle floats
	if (type_is_float(to))
	{
		// Any integer may convert to a float.
		if (type_is_integer(from)) return true;

		// Cast from bool if explicit or cast from bool.
		if (from == type_bool) return cast_from_bool || is_explicit;

		// Any narrower float
		if (type_is_float(from))
		{
			if (is_explicit) return true;
			ByteSize to_size = type_size(to);
			ByteSize from_size = type_size(from);
			if (to_size == from_size) return true;
			return to_size > from_size && expr_is_simple(expr);
		}

		UNREACHABLE;
	}

	// Handle ints
	if (type_is_integer(to))
	{
		if (type_is_float(from)) return is_explicit;

		// Cast from bool if explicit or cast from bool.
		if (from == type_bool) return cast_from_bool || is_explicit;

		if (type_is_integer(from))
		{
			if (is_explicit) return true;
			ByteSize to_size = type_size(to);
			ByteSize from_size = type_size(from);
			if (to_size == from_size) return true;
			return to_size > from_size && expr_is_simple(expr);
		}

		UNREACHABLE
	}
	assert(to == type_bool);
	if (!is_explicit) return false;
	return true;
}
static inline bool cast_vector(SemaContext *context, Expr *expr, Type *from, Type *to, Type *to_type, bool add_optional, bool is_explicit, bool silent)
{
	bool is_scaled = from->type_kind == TYPE_SCALED_VECTOR;
	bool to_vector = type_kind_is_any_vector(to->type_kind);
	bool infer_len = false;
	switch (to->type_kind)
	{
		case TYPE_INFERRED_ARRAY:
			infer_len = true;
			if (is_scaled) goto CAST_ILLEGAL;
			goto TRY_CAST;
		case TYPE_INFERRED_VECTOR:
			infer_len = true;
			if (is_scaled) goto CAST_ILLEGAL;
			goto TRY_CONVERT;
		case TYPE_ARRAY:
			if (is_scaled || to->array.len != from->array.len) goto CAST_ILLEGAL;
			goto TRY_CAST;
		case TYPE_VECTOR:
			if (is_scaled || to->array.len != from->array.len) goto CAST_ILLEGAL;
			goto TRY_CONVERT;
		case TYPE_SCALED_VECTOR:
			if (!is_scaled) goto CAST_ILLEGAL;
			goto TRY_CONVERT;
		default:
			goto CAST_ILLEGAL;
	}
TRY_CONVERT:
	if (cast_may_number_convert(expr, from->array.base, to->array.base, is_explicit, to_vector)) goto CAST;
	return sema_error_cannot_convert(expr, to, !is_explicit && cast_may_number_convert(expr, from->array.base, to->array.base, true, to_vector), silent);
TRY_CAST:
	if (type_array_element_is_equivalent(from->array.base, to->array.base, is_explicit)) goto CAST;
	if (silent) return false;
	return sema_error_cannot_convert(expr, to, !is_explicit && type_array_element_is_equivalent(from->array.base, to->array.base, true), true);
CAST_ILLEGAL:
	return sema_error_cannot_convert(expr, to_type, false, silent);
CAST:
	if (infer_len) to_type = cast_infer_len(to_type, from);
	return cast_with_optional(expr, to_type, add_optional);
}

static inline bool cast_integer(SemaContext *context, Expr *expr, Type *from, Type *to, Type *to_type, bool add_optional, bool is_explicit, bool silent)
{
	bool no_report = silent;
RETRY:
	switch (to->type_kind)
	{
		case ALL_FLOATS:
			goto CAST;
		case TYPE_BOOL:
			if (is_explicit) goto CAST;
			goto REQUIRE_CAST;
		case TYPE_VECTOR:
			to = to->array.base->canonical;
			goto RETRY;
		case TYPE_ENUM:
			if (from->type_kind == TYPE_ENUM) break;
			to = to->decl->enums.type_info->type->canonical;
			if (is_explicit) to = type_flatten_distinct(to);
			FALLTHROUGH;
		case ALL_INTS:
		{
			if (is_explicit) goto CAST;
			ByteSize to_size = type_size(to);
			ByteSize from_size = type_size(from);
			if (to_size > from_size) goto ONLY_SIMPLE;
			if (expr_is_const(expr) && expr_const_will_overflow(&expr->const_expr, type_flatten(to)->type_kind))
			{
				sema_error_const_int_out_of_range(expr, expr, to_type);
				return false;
			}
			if (to_size == from_size) goto CAST;
			Expr *problem = recursive_may_narrow_int(expr, to);
			if (problem)
			{
				if (no_report) return false;
				goto REQUIRE_CAST;
			}
			goto CAST;
		}
		case TYPE_DISTINCT:
			if (expr_is_const(expr))
			{
				to = type_flatten_distinct(to);
				goto RETRY;
			}
			else
			{
				if (no_report) return false;
				bool may_explicit = cast_expr_inner(context, expr_copy(expr), to_type, true, true, false);
				if (may_explicit) goto REQUIRE_CAST;
				break;
			}
		case TYPE_POINTER:
		{
			if (from->type_kind == TYPE_ENUM) break;
			bool may_cast = expr_is_const(expr) || type_size(from) >= type_size(type_voidptr);
			if (!is_explicit)
			{
				if (may_cast) goto REQUIRE_CAST;
				break;
			}
			if (!may_cast)
			{
				if (no_report) return false;
				SEMA_ERROR(expr, "You cannot cast from a type smaller than %s.",
				           type_quoted_error_string(type_iptr));
				return false;
			}
			goto CAST;
		}
		default:
			break;
	}
	return sema_error_cannot_convert(expr, to_type, false, no_report);
ONLY_SIMPLE:
	if (expr_is_simple(expr)) goto CAST;
	if (no_report) return false;
	SEMA_ERROR(expr, "This conversion requires an explicit cast to %s, because the widening of the expression may be done in more than one way.", type_quoted_error_string(to_type));
	return false;
REQUIRE_CAST:
	if (no_report) return false;
	SEMA_ERROR(expr, "%s cannot implicitly be converted to %s, but you may use a cast.", type_quoted_error_string(expr->type), type_quoted_error_string(to_type));
	return false;
CAST:
	return cast_with_optional(expr, to_type, add_optional);
}

static inline bool cast_float(SemaContext *context, Expr *expr, Type *from, Type *to, Type *to_type, bool add_optional, bool is_explicit, bool silent)
{
RETRY:
	switch (to->type_kind)
	{
		case ALL_INTS:
		case TYPE_BOOL:
			if (is_explicit) goto CAST;
			goto REQUIRE_CAST;
		case TYPE_VECTOR:
			to = to->array.base->canonical;
			goto RETRY;
		case ALL_FLOATS:
		{
			if (is_explicit) goto CAST;
			ByteSize to_size = type_size(to);
			ByteSize from_size = type_size(from);
			if (to_size > from_size) goto ONLY_SIMPLE;
			if (expr_is_const(expr) && expr_const_will_overflow(&expr->const_expr, type_flatten(to)->type_kind))
			{
				if (silent) return false;
				SEMA_ERROR(expr, "The value '%s' is out of range for %s, so you need an explicit cast to truncate the value.", expr_const_to_error_string(&expr->const_expr),
					           type_quoted_error_string(to_type));
				return false;
			}
			if (to_size == from_size) goto CAST;
			Expr *problem = recursive_may_narrow_float(expr, to);
			if (problem)
			{
				if (silent) return false;
				goto REQUIRE_CAST;
			}
			goto CAST;
		}
		case TYPE_DISTINCT:
			if (expr_is_const(expr))
			{
				to = type_flatten_distinct(to);
				goto RETRY;
			}
			else
			{
				if (silent) return false;
				bool may_explicit = cast_expr_inner(context, expr_copy(expr), to_type, true, true, false);
				if (may_explicit) goto REQUIRE_CAST;
				break;
			}
		default:
			break;
	}
	return sema_error_cannot_convert(expr, to_type, false, silent);
ONLY_SIMPLE:
	if (expr_is_simple(expr)) goto CAST;
	if (silent) return false;
	SEMA_ERROR(expr, "This conversion requires an explicit cast to %s, because the widening of the expression may be done in more than one way.", type_quoted_error_string(to_type));
	return false;
REQUIRE_CAST:
	if (silent) return false;
	SEMA_ERROR(expr, "%s cannot implicitly be converted to %s, but you may use a cast.", type_quoted_error_string(expr->type), type_quoted_error_string(to_type));
	return false;
CAST:
	return cast_with_optional(expr, to_type, add_optional);
}

static inline bool cast_pointer(SemaContext *context, Expr *expr, Type *from, Type *to, Type *to_type, bool add_optional, bool is_explicit, bool silent)
{
	// pointer -> any, void* -> pointer pointer -> void*
	if (to == type_any || to == type_voidptr || (from == type_voidptr && type_is_pointer(to))) return cast_with_optional(expr, to_type, add_optional);

	Type *pointee = from->pointer;
	pointee = is_explicit ? type_flatten(pointee) : pointee->canonical;
	TypeKind pointee_kind = pointee->type_kind;
	switch (to->type_kind)
	{
		case ALL_INTS:
			if (!is_explicit) return sema_error_cannot_convert(expr, to_type, true, silent);
			if (type_size(to_type) < type_size(type_iptr))
			{
				SEMA_ERROR(expr, "Casting %s to %s is not allowed because '%s' is smaller than a pointer. "
								 "Use (%s)(iptr) if you want this lossy cast.",
								 type_quoted_error_string(expr->type), type_quoted_error_string(to_type),
								 type_to_error_string(to_type), type_to_error_string(to_type));
				return false;
			}
			return cast_with_optional(expr, to_type, add_optional);
		case TYPE_SUBARRAY:
			// int[<2>], int[2], int[<*>]
			if (pointee_kind == TYPE_ARRAY || pointee_kind == TYPE_VECTOR || pointee_kind == TYPE_SCALED_VECTOR)
			{
				Type *subarray_base = to->array.base->canonical;
				Type *from_base = pointee->array.base;
				if (is_explicit)
				{
					subarray_base = type_flatten_distinct(subarray_base);
					from_base = type_flatten_distinct(from_base);
				}
				if (subarray_base == from_base) return cast_with_optional(expr, to_type, add_optional);
				if (subarray_base->type_kind == TYPE_POINTER && from_base->type_kind == TYPE_POINTER)
				{
					if (type_is_pointer_equivalent(subarray_base, from_base, is_explicit)) return cast_with_optional(expr, to_type, add_optional);
				}
				if (silent) return false;
				bool would_work_explicit = false;
				if (!is_explicit)
				{
					would_work_explicit = cast_pointer(context, expr_copy(expr), from, to, to_type, add_optional, true, true);
				}
				return sema_error_cannot_convert(expr, to_type, would_work_explicit, false);
			}
			return sema_error_cannot_convert(expr, to_type, false, silent);
		case TYPE_BOOL:
			if (is_explicit) return cast_with_optional(expr, to_type, add_optional);
			return sema_error_cannot_convert(expr, to_type, true, silent);
		case TYPE_POINTER:
			if (is_explicit) return cast_with_optional(expr, to_type, add_optional);
			if (cast_may_implicit_ptr(from->pointer, to->pointer))
			{
				return cast_with_optional(expr, to_type, add_optional);
			}
			return sema_error_cannot_convert(expr, to_type, true, silent);
		case TYPE_OPTIONAL_ANY:
		case TYPE_OPTIONAL:
			UNREACHABLE
		default:
			return sema_error_cannot_convert(expr, to_type, false, silent);
	}
}
/**
 * Do the following:
 * 1. Special optional conversions.
 * 2. String literal conversions
 * 3. Constant pointer conversions
 */
static bool cast_expr_inner(SemaContext *context, Expr *expr, Type *to_type, bool is_explicit, bool silent,
                            bool may_not_be_optional)
{
	Type *from_type = expr->type;

	assert(!type_is_optional(to_type) || may_not_be_optional);
	Type *to = is_explicit ? type_flatten_distinct_optional(to_type) : type_no_optional(to_type)->canonical;

	// Step one, cast from optional.
	// This handles:
	// 1. *! -> any type
	// 2. void! -> anyerr
	// 3. void! -> SomeFault (explicit)
	if (type_is_optional(from_type))
	{
		// *! -> int => ok, gives int!
		if (from_type == type_anyfail)
		{
			if (may_not_be_optional)
			{
				if (silent) return false;
				SEMA_ERROR(expr, "An optional value cannot be converted to a non-optional %s.", type_quoted_error_string(to_type));
				return false;
			}
			// Just add the optional.
			expr->type = type_add_optional(to_type, true);
			return true;
		}

		// Here we have something like int!
		assert(from_type->type_kind == TYPE_OPTIONAL);

		// If it is void!, then there are special rules:
		if (from_type->optional == type_void)
		{
			// void! x; anyerr y = x;
			if (!type_is_optional(to_type) && to == type_anyerr)
			{
				cast(expr, to_type);
				return true;
			}

			// void! x; FooFault y = (FooFault)x;
			// Only allowed if explicit.
			if (to->type_kind == TYPE_FAULTTYPE)
			{
				if (!is_explicit)
				{
					if (silent) return false;
					SEMA_ERROR(expr, "A 'void!' can only be cast into %s using an explicit cast. You can try using (%s)",
					           type_quoted_error_string(to_type), type_to_error_string(to_type));
					return false;
				}
				cast(expr, to_type);
				return true;
			}
		}
		if (may_not_be_optional)
		{
			if (silent) return false;
			char *format = is_explicit ? "Cannot cast an optional %s to %s." : "Cannot convert an optional %s to %s.";
			SEMA_ERROR(expr, format, type_quoted_error_string(from_type), type_quoted_error_string(to_type));
			return false;
		}
	}

	// We're now done and can remove the optional
	bool add_optional = type_is_optional(to_type) || type_is_optional(from_type);
	to_type = type_no_optional(to_type);
	from_type = type_no_optional(from_type);

	// Grab the underlying expression type.
	Type *from = is_explicit ? type_flatten_distinct(from_type) : from_type->canonical;

	// We may already be done.
	if (from == to)
	{
		expr->type = type_add_optional(to_type, add_optional);
		return true;
	}


	// Handle strings, these don't actually mess with the underlying data,
	// just the type.
	if (cast_maybe_string_lit_to_char_array(expr, from, to, to_type)) return true;

	// For constant pointers cast into anything pointer-like:
	if (expr_is_const_pointer(expr) && from == type_voidptr && type_flatten_distinct(to)->type_kind == TYPE_POINTER)
	{
		assert(!add_optional);
		expr->type = to_type;
		return true;
	}
	switch (from->type_kind)
	{
		case TYPE_UNTYPED_LIST:
			if (!cast_untyped_to_type(context, expr, to_type)) return false;
			if (add_optional) expr->type = type_add_optional(expr->type, true);
			return true;
		case TYPE_FAULTTYPE:
			// Allow MyError.A -> error, to an integer or to bool
			if (to == type_anyerr) return cast(expr, to_type);
			if (type_is_integer(to) || to == type_bool) goto CAST_IF_EXPLICIT;
			goto CAST_FAILED;
		case TYPE_ANYERR:
			if (to_type == type_bool || to->type_kind == TYPE_FAULTTYPE || type_is_integer(to))
			{
				goto CAST_IF_EXPLICIT;
			}
			goto CAST_FAILED;
		case TYPE_POINTER:
			return cast_pointer(context, expr, from, to, to_type, add_optional, is_explicit, silent);
		case TYPE_SUBARRAY:
			return cast_subarray(context, expr, from, to, to_type, add_optional, is_explicit, silent);
		case TYPE_BOOL:
			// Bool may convert into integers and floats but only explicitly.
			if (type_is_integer(to) || type_is_float(to)) goto CAST_IF_EXPLICIT;
			goto CAST_FAILED;
		case TYPE_VECTOR:
		case TYPE_SCALED_VECTOR:
			return cast_vector(context, expr, from, to, to_type, add_optional, is_explicit, silent);
		case TYPE_ARRAY:
			return cast_array(context, expr, from, to, to_type, add_optional, is_explicit, silent);
		case TYPE_ENUM:
		case ALL_INTS:
			return cast_integer(context, expr, from, to, to_type, add_optional, is_explicit, silent);
		case ALL_FLOATS:
			return cast_float(context, expr, from, to, to_type, add_optional, is_explicit, silent);
		case TYPE_POISONED:
			return false;
		case TYPE_VOID:
		case TYPE_INFERRED_ARRAY:
		case TYPE_TYPEINFO:
		case TYPE_FUNC:
		case TYPE_FLEXIBLE_ARRAY:
		case TYPE_INFERRED_VECTOR:
		case TYPE_MEMBER:
			goto CAST_FAILED;
		case TYPE_TYPEID:
			if (!type_is_pointer_sized_or_more(to_type)) goto CAST_FAILED;
			goto CAST_IF_EXPLICIT;
		case TYPE_OPTIONAL:
		case TYPE_OPTIONAL_ANY:
		case TYPE_TYPEDEF:
			UNREACHABLE;
		case TYPE_ANY:
			if (to->type_kind != TYPE_POINTER) goto CAST_FAILED;
			goto CAST_IF_EXPLICIT;
		case TYPE_STRUCT:
			if (type_is_substruct(from))
			{
				Type *type;
				Expr *access = expr_access_inline_member(expr_copy(expr), from->decl->strukt.members[0]);
				while (1)
				{
					type = access->type->canonical;
					if (type == to) break;
					if (!type_is_substruct(type)) goto CAST_FAILED;
				}
				expr_replace(expr, access);
				return true;
			}
			goto CAST_FAILED;
		case TYPE_UNION:
			goto CAST_FAILED;
		case TYPE_BITSTRUCT:
			// A bitstruct can convert to:
			// 1. An int of the same length
			// 2. An integer array of the same length
			if (type_size(to_type) != type_size(from_type)) goto CAST_FAILED;
			if (type_is_integer(to_type)) goto CAST_IF_EXPLICIT;
			if (to_type->type_kind == TYPE_ARRAY && type_is_integer(to_type->array.base)) goto CAST_IF_EXPLICIT;
			goto CAST_FAILED;
			UNREACHABLE
		case TYPE_DISTINCT:
			assert(!is_explicit);
			if (cast_expr_inner(context, expr, to_type, true, true, false))
			{
				goto CAST_IF_EXPLICIT;
			}
			goto CAST_FAILED;
	}
	UNREACHABLE

CAST_IF_EXPLICIT:
	if (!is_explicit)
	{
		if (silent) return false;
		SEMA_ERROR(expr,
		           "Implicitly casting %s to %s is not permitted, but you can do an explicit cast by placing '(%s)' before the expression.",
		           type_quoted_error_string(type_no_optional(from_type)),
		           type_quoted_error_string(type_no_optional(to_type)),
		           type_to_error_string(to_type));
		return false;
	}
	return cast(expr, to_type);
CAST_FAILED:
	if (!silent)
	{
		if (!is_explicit)
		{
			SEMA_ERROR(expr, "You cannot cast %s into %s even with an explicit cast, so this looks like an error.", type_quoted_error_string(expr->type), type_quoted_error_string(to_type));
			return false;
		}
		return sema_error_failed_cast(expr, expr->type, to_type);
	}
	return false;
}

static bool arr_to_vec(Expr *expr, Type *to_type)
{
	if (insert_runtime_cast_unless_const(expr, CAST_ARRVEC, to_type)) return true;

	assert(expr->const_expr.const_kind == CONST_INITIALIZER);
	ConstInitializer *list = expr->const_expr.initializer;
	list->type = to_type;
	expr->type = to_type;
	return true;
}

static bool vec_to_arr(Expr *expr, Type *to_type)
{
	if (insert_runtime_cast_unless_const(expr, CAST_VECARR, to_type)) return true;

	assert(expr->const_expr.const_kind == CONST_INITIALIZER);
	ConstInitializer *list = expr->const_expr.initializer;
	list->type = to_type;
	expr->type = to_type;
	return true;
}

static void vec_const_init_to_type(ConstInitializer *initializer, Type *to_type)
{
	switch (initializer->kind)
	{
		case CONST_INIT_ARRAY:
		{
			Type *element_type = type_flatten(to_type)->array.base;
			FOREACH_BEGIN(ConstInitializer *element, initializer->init_array.elements)
				vec_const_init_to_type(element, element_type);
			FOREACH_END();
			break;
		}
		case CONST_INIT_ARRAY_FULL:
		{
			Type *element_type = type_flatten(to_type)->array.base;
			FOREACH_BEGIN(ConstInitializer *element, initializer->init_array_full)
				vec_const_init_to_type(element, element_type);
			FOREACH_END();
			break;
		}
		case CONST_INIT_VALUE:
		{
			Type *to_flat = type_flatten(to_type);
			bool is_neg_conversion = to_flat && type_flatten(initializer->type) == type_bool;
			if (is_neg_conversion)
			{
				bool is_true = initializer->init_value->const_expr.b;
				initializer->init_value->const_expr.ixx = (Int)
						{ .i = is_true ? (Int128) { UINT64_MAX, UINT64_MAX } : (Int128) { 0, 0 },
						  .type = to_flat->type_kind };
				initializer->init_value->type = to_type;
			}
			else
			{
				cast(initializer->init_value, to_type);
			}
			break;
		}
		case CONST_INIT_ZERO:
			break;
		case CONST_INIT_UNION:
		case CONST_INIT_STRUCT:
			UNREACHABLE
		case CONST_INIT_ARRAY_VALUE:
			vec_const_init_to_type(initializer->init_array_value.element, to_type);
			break;
	}
	initializer->type = to_type;
}
static bool vec_to_vec(Expr *expr, Type *to_type)
{
	if (expr->expr_kind != EXPR_CONST)
	{
		Type *from_type = type_flatten(expr->type);
		Type *from_element = from_type->array.base;
		to_type = type_flatten(to_type);
		Type *to_element = to_type->array.base;
		if (type_is_float(from_element))
		{
			if (to_element == type_bool) return insert_cast(expr, CAST_FPBOOL, to_type);
			if (type_is_unsigned(to_element)) return insert_cast(expr, CAST_FPUI, to_type);
			if (type_is_signed(to_element)) return insert_cast(expr, CAST_FPSI, to_type);
			if (type_is_float(to_element)) return insert_cast(expr, CAST_FPFP, to_type);
			UNREACHABLE;
		}
		if (from_element == type_bool)
		{
			if (type_is_integer(to_element)) return insert_cast(expr, CAST_BOOLVECINT, to_type);
			if (type_is_float(to_element)) return insert_cast(expr, CAST_FPFP, to_type);
			UNREACHABLE;
		}
		if (type_is_signed(from_element))
		{
			if (to_element == type_bool) return insert_cast(expr, CAST_INTBOOL, to_type);
			if (type_is_unsigned(to_element)) return insert_cast(expr, CAST_SIUI, to_type);
			if (type_is_signed(to_element)) return insert_cast(expr, CAST_SISI, to_type);
			if (type_is_float(to_element)) return insert_cast(expr, CAST_SIFP, to_type);
			UNREACHABLE
		}
		assert(type_is_unsigned(from_element));
		if (to_element == type_bool) return insert_cast(expr, CAST_INTBOOL, to_type);
		if (type_is_unsigned(to_element)) return insert_cast(expr, CAST_UIUI, to_type);
		if (type_is_signed(to_element)) return insert_cast(expr, CAST_UISI, to_type);
		if (type_is_float(to_element)) return insert_cast(expr, CAST_UIFP, to_type);
		UNREACHABLE
	}

	assert(expr->const_expr.const_kind == CONST_INITIALIZER);

	ConstInitializer *list = expr->const_expr.initializer;
	vec_const_init_to_type(list, to_type);
	expr->type = to_type;
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
				expr_rewrite_const_bool(expr, type_bool, expr->const_expr.enum_err_val != NULL);
				return true;
			default:
				UNREACHABLE
		}
	}
	return insert_cast(expr, CAST_ERBOOL, to_type);
}

static inline bool subarray_to_subarray(Expr *expr, Type *to_type)
{
	if (expr_is_const(expr))
	{
		expr->type = to_type;
	}
	return insert_cast(expr, CAST_SASA, to_type);
}
static inline bool subarray_to_bool(Expr *expr)
{
	if (expr->expr_kind == EXPR_CONST && expr->const_expr.const_kind == CONST_INITIALIZER)
	{
		ConstInitializer *list = expr->const_expr.initializer;
		switch (list->kind)
		{
			case CONST_INIT_ZERO:
				expr_rewrite_const_bool(expr, type_bool, false);
				return true;
			case CONST_INIT_ARRAY:
				expr_rewrite_const_bool(expr, type_bool, vec_size(list->init_array.elements) > 0);
				return true;
			case CONST_INIT_ARRAY_FULL:
				expr_rewrite_const_bool(expr, type_bool, vec_size(list->init_array_full) > 0);
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
		case TYPE_OPTIONAL_ANY:
		case TYPE_OPTIONAL:
		case TYPE_VOID:
			UNREACHABLE
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
			if (to->type_kind == TYPE_FAULTTYPE)
			{
				REMINDER("Improve anyerr -> fault conversion.");
				return insert_cast(expr, CAST_EUER, to_type);
			}
			if (type_is_integer(to)) return insert_cast(expr, CAST_EUINT, to_type);
			break;
		case ALL_SIGNED_INTS:
			if (type_is_integer_unsigned(to)) return int_conversion(expr, CAST_SIUI, to, to_type);
			if (type_is_integer_signed(to)) return int_conversion(expr, CAST_SISI, to, to_type);
			if (type_is_float(to)) return int_to_float(expr, CAST_SIFP, to, to_type);
			if (to == type_bool) return integer_to_bool(expr, to_type);
			if (to->type_kind == TYPE_POINTER) return int_to_pointer(expr, to_type);
			if (to->type_kind == TYPE_ENUM) return integer_to_enum(expr, to, to_type);
			if (type_kind_is_any_vector(to->type_kind)) return int_vector_conversion(expr, to, to_type);
			break;
		case ALL_UNSIGNED_INTS:
			if (type_is_integer_unsigned(to)) return int_conversion(expr, CAST_UIUI, to, to_type);
			if (type_is_integer_signed(to)) return int_conversion(expr, CAST_UISI, to, to_type);
			if (type_is_float(to)) return int_to_float(expr, CAST_UIFP, to, to_type);
			if (to == type_bool) return integer_to_bool(expr, to_type);
			if (to->type_kind == TYPE_POINTER) return int_to_pointer(expr, to_type);
			if (to->type_kind == TYPE_ENUM) return integer_to_enum(expr, to, to_type);
			if (type_kind_is_any_vector(to->type_kind)) return int_vector_conversion(expr, to, to_type);
			break;
		case ALL_FLOATS:
			if (type_is_integer(to)) return float_to_integer(expr, to, to_type);
			if (to == type_bool) return float_to_bool(expr, to_type);
			if (type_is_float(to)) return float_to_float(expr, to, to_type);
			if (type_kind_is_any_vector(to->type_kind)) return float_vector_conversion(expr, to, to_type);
			break;
		case TYPE_TYPEID:
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
			if (to->type_kind == TYPE_ENUM)
			{
				Type *temp = type_flatten(to);
				if (!enum_to_integer(expr, from_type, temp, temp)) return false;
				return integer_to_enum(expr, to, to_type);
			}
			break;
		case TYPE_FAULTTYPE:
			if (to->type_kind == TYPE_ANYERR) return err_to_anyerr(expr, to_type);
			if (to == type_bool) return err_to_bool(expr, to_type);
			if (type_is_integer(to)) return insert_cast(expr, CAST_ERINT, to_type);
			break;
		case TYPE_FLEXIBLE_ARRAY:
		case TYPE_SCALED_VECTOR:
			return false;
		case TYPE_ARRAY:
			if (to->type_kind == TYPE_VECTOR) return arr_to_vec(expr, to_type);
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
			if (to->type_kind == TYPE_SUBARRAY) return subarray_to_subarray(expr, to);
			break;
		case TYPE_VECTOR:
			if (to->type_kind == TYPE_ARRAY) return vec_to_arr(expr, to_type);
			if (to->type_kind == TYPE_VECTOR) return vec_to_vec(expr, to_type);
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
	if (to_type == type_void)
	{
		expr->type = type_void;
		return true;
	}

	assert(!type_is_optional(to_type));
	Type *from_type = expr->type;
	bool from_is_optional = false;
	Type *to = type_flatten_distinct(to_type);

	// Special case *! => error
	if (to == type_anyerr || to->type_kind == TYPE_FAULTTYPE)
	{
		if (type_is_optional(from_type)) return voidfail_to_error(expr, to_type);
	}

	if (type_is_optional_any(from_type))
	{
		expr->type = type_get_optional(to_type);
		return true;
	}

	if (type_is_optional_type(from_type))
	{
		from_type = from_type->optional;
		from_is_optional = true;
	}
	from_type = type_flatten_distinct(from_type);
	if (type_len_is_inferred(to_type))
	{
		to_type = from_type;
		to = type_flatten(from_type);
	}
	if (from_type == to)
	{
		expr->type = type_add_optional(to_type, from_is_optional);
		if (expr->expr_kind == EXPR_CONST)
		{
			expr->const_expr.narrowable = false;
			expr->const_expr.is_hex = false;
		}
		return true;
	}

	if (!cast_inner(expr, from_type, to, to_type)) return false;

	Type *result_type = expr->type;
	if (from_is_optional && !type_is_optional(result_type))
	{
		expr->type = type_get_optional(result_type);
	}
	return true;
}

bool cast_to_index(Expr *index)
{
	Type *type = index->type->canonical;
	RETRY:
	switch (type->type_kind)
	{
		case TYPE_I8:
		case TYPE_I16:
		case TYPE_I32:
		case TYPE_I64:
			return cast(index, type_isz);
		case TYPE_U8:
		case TYPE_U16:
		case TYPE_U32:
		case TYPE_U64:
			return cast(index, type_usz);
		case TYPE_U128:
			SEMA_ERROR(index, "You need to explicitly cast this to a uint or ulong.");
			return false;
		case TYPE_I128:
			SEMA_ERROR(index, "index->type->canonical this to an int or long.");
			return false;
		case TYPE_ENUM:
			type = type->decl->enums.type_info->type->canonical;
			goto RETRY;
		default:
			SEMA_ERROR(index, "Cannot implicitly convert '%s' to an index.", type_to_error_string(index->type));
			return false;
	}
}

bool cast_widen_top_down(SemaContext *context, Expr *expr, Type *type)
{
	Type *to = type_no_optional(type);
	Type *from = type_no_optional(expr->type);
	RETRY:
	if (type_is_integer(from) && type_is_integer(to)) goto CONVERT_IF_BIGGER;
	if (type_is_float(from) && type_is_float(to)) goto CONVERT_IF_BIGGER;
	if (type_is_integer(from) && type_is_float(to)) goto CONVERT;
	if (type_flat_is_vector(from) && type_flat_is_vector(to))
	{
		to = type_vector_type(to);
		from = type_vector_type(from);
		goto RETRY;
	}
	return true;
	CONVERT_IF_BIGGER:
	if (type_size(to) <= type_size(from)) return true;
	CONVERT:
	return cast_implicit(context, expr, type);
}

bool cast_promote_vararg(Expr *arg)
{
	Type *arg_type = arg->type->canonical;

	// 2. Promote any integer or bool to at least CInt
	if (type_is_promotable_integer(arg_type) || arg_type == type_bool)
	{
		return cast(arg, type_cint);
	}
	// 3. Promote any float to at least double
	if (type_is_promotable_float(arg->type))
	{
		return cast(arg, type_double);
	}
	return true;
}

Type *cast_numeric_arithmetic_promotion(Type *type)
{
	if (!type) return NULL;
	switch (type->type_kind)
	{
		case ALL_SIGNED_INTS:
			if (type->builtin.bitsize < platform_target.width_c_int) return type_cint;
			return type;
		case ALL_UNSIGNED_INTS:
			if (type->builtin.bitsize < platform_target.width_c_int) return type_cuint;
			return type;
		case TYPE_F16:
			// Promote F16 to a real type.
			return type_float;
		case TYPE_OPTIONAL:
			UNREACHABLE
		default:
			return type;
	}
}

bool cast_decay_array_pointers(SemaContext *context, Expr *expr)
{
	CanonicalType *pointer_type = type_pointer_type(type_no_optional(expr->type));
	if (!pointer_type || !type_is_arraylike(pointer_type)) return true;
	return cast_expr_inner(context,
	                       expr,
	                       type_add_optional(type_get_ptr(pointer_type->array.base), IS_OPTIONAL(expr)),
	                       true, false, false);
}

void cast_to_max_bit_size(SemaContext *context, Expr *left, Expr *right, Type *left_type, Type *right_type)
{
	unsigned bit_size_left = left_type->builtin.bitsize;
	unsigned bit_size_right = right_type->builtin.bitsize;
	assert(bit_size_left && bit_size_right);
	if (bit_size_left == bit_size_right) return;
	if (bit_size_left < bit_size_right)
	{
		Type *to = left->type->type_kind < TYPE_U8
		           ? type_int_signed_by_bitsize(bit_size_right)
		           : type_int_unsigned_by_bitsize(bit_size_right);
		bool success = cast(left, to);
		assert(success);
		return;
	}
	Type *to = right->type->type_kind < TYPE_U8
	           ? type_int_signed_by_bitsize(bit_size_left)
	           : type_int_unsigned_by_bitsize(bit_size_left);
	bool success = cast(right, to);
	assert(success);
}


#pragma clang diagnostic pop