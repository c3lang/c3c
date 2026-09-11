#include "c_codegen_internal.h"
#include <math.h>

bool c_is_file_global(Decl *decl)
{
	if (!decl)
	{
		return false;
	}
	decl = c_decl_unwrap(decl);
	if (decl->decl_kind != DECL_VAR)
	{
		return false;
	}
	if (decl->var.kind == VARDECL_MEMBER || decl->var.kind == VARDECL_BITMEMBER ||
	    decl->var.kind == VARDECL_PARAM || decl->var.kind == VARDECL_PARAM_EXPR ||
	    decl->var.kind == VARDECL_PARAM_CT || decl->var.kind == VARDECL_PARAM_CT_TYPE ||
	    decl->var.kind == VARDECL_LOCAL_CT || decl->var.kind == VARDECL_LOCAL_CT_TYPE ||
	    decl->var.kind == VARDECL_LOCAL)
	{
		return false;
	}
	if (decl->is_extern || decl->var.kind == VARDECL_GLOBAL)
	{
		return true;
	}
	if (decl->var.kind == VARDECL_CONST && decl->unit && decl->unit->vars)
	{
		FOREACH(Decl *, v, decl->unit->vars)
		{
			if (c_decl_unwrap(v) == decl)
			{
				return true;
			}
		}
	}
	return false;
}

bool c_can_emit_static_initializer(Expr *expr)
{
	if (!expr)
	{
		return false;
	}
	switch (expr->expr_kind)
	{
		case EXPR_CONST:
		case EXPR_TYPEID:
			return true;
		case EXPR_INITIALIZER_LIST:
		{
			Expr **elements = expr->initializer_list;
			int count       = vec_size(elements);
			for (int i = 0; i < count; i++)
			{
				if (!c_can_emit_static_initializer(elements[i]))
				{
					return false;
				}
			}
			return true;
		}
		case EXPR_DESIGNATED_INITIALIZER_LIST:
		{
			if (expr->designated_init.splat && !c_can_emit_static_initializer(expr->designated_init.splat))
			{
				return false;
			}
			Expr **elements = expr->designated_init.list;
			int count       = vec_size(elements);
			for (int i = 0; i < count; i++)
			{
				if (elements[i]->expr_kind == EXPR_DESIGNATOR && elements[i]->designator_expr.value)
				{
					if (!c_can_emit_static_initializer(elements[i]->designator_expr.value))
					{
						return false;
					}
				}
			}
			return true;
		}
		case EXPR_MAKE_ANY:
			return (!expr->make_any_expr.inner || c_can_emit_static_initializer(expr->make_any_expr.inner)) &&
			       (!expr->make_any_expr.typeid || c_can_emit_static_initializer(expr->make_any_expr.typeid));
		case EXPR_MAKE_SLICE:
			return (!expr->make_slice_expr.ptr || c_can_emit_static_initializer(expr->make_slice_expr.ptr));
		case EXPR_UNARY:
		{
			UnaryOp uop = expr->unary_expr.operator;
			if (uop == UNARYOP_ADDR || uop == UNARYOP_TADDR)
			{
				Expr *inner = expr->unary_expr.expr;
				if (inner && (inner->expr_kind == EXPR_IDENTIFIER || inner->expr_kind == EXPR_DECL))
				{
					Decl *d = (inner->expr_kind == EXPR_DECL) ? inner->decl_expr : c_decl_unwrap(inner->ident_expr);
					if (d && (d->decl_kind == DECL_FUNC || c_is_file_global(d)))
					{
						return true;
					}
				}
				return c_can_emit_static_initializer(inner);
			}
			if (uop == UNARYOP_NEG || uop == UNARYOP_BITNEG || uop == UNARYOP_NOT)
			{
				return c_can_emit_static_initializer(expr->unary_expr.expr);
			}
			return false;
		}
		case EXPR_BINARY:
			return c_can_emit_static_initializer(exprptr(expr->binary_expr.left)) &&
			       c_can_emit_static_initializer(exprptr(expr->binary_expr.right));
		case EXPR_IDENTIFIER:
		{
			Decl *d = c_decl_unwrap(expr->ident_expr);
			if (!d)
			{
				return false;
			}
			if (d->decl_kind == DECL_FUNC || d->decl_kind == DECL_ENUM_CONSTANT || d->decl_kind == DECL_FAULT)
			{
				return true;
			}
			if (d->decl_kind == DECL_VAR && d->var.kind == VARDECL_CONST && d->var.init_expr)
			{
				return c_can_emit_static_initializer(d->var.init_expr);
			}
			return false;
		}
		case EXPR_RECAST:
		case EXPR_ADDR_CONVERSION:
		case EXPR_RVALUE:
		case EXPR_INT_TO_PTR:
		case EXPR_PTR_TO_INT:
		case EXPR_INT_TO_FLOAT:
		case EXPR_FLOAT_TO_INT:
		case EXPR_EXT_TRUNC:
			return c_can_emit_static_initializer(expr->inner_expr);
		case EXPR_TWO:
			return c_can_emit_static_initializer(expr->two_expr.last);
		default:
			return false;
	}
}

static void c_bit_set_bytes_internal(uint8_t *bytes, int start_bit, int end_bit, bool reverse, uint64_t val)
{
	int bit_size = end_bit - start_bit + 1;
	for (int bits_done = 0; bits_done < bit_size; )
	{
		CBitfieldStep s = c_bitfield_step(start_bit, bit_size, bits_done, reverse);
		uint8_t chunk    = (uint8_t)((val >> s.shift) & s.step_mask);
		bytes[s.byte_idx] = (bytes[s.byte_idx] & ~((uint8_t)s.byte_mask)) | (chunk << s.bit_in_byte);
		bits_done += s.step_bits;
	}
}

static uint64_t c_get_const_init_val(ConstInitializer *val)
{
	if (!val || val->kind != CONST_INIT_VALUE || !val->init_value || val->init_value->expr_kind != EXPR_CONST)
	{
		return 0;
	}
	ExprConst *ec = &val->init_value->const_expr;
	switch (ec->const_kind)
	{
		case CONST_BOOL:
			return ec->b ? 1 : 0;
		case CONST_INTEGER:
			return ec->ixx.i.low;
		case CONST_ENUM:
			return (uint64_t)ec->enum_val->enum_constant.inner_ordinal;
		default:
			return 0;
	}
}

static void c_emit_const_bitstruct_initializer(GenContext *c, ConstInitializer *init)
{
	Type *bit_type       = type_flatten(init->type);
	Decl *decl           = bit_type ? bit_type->decl : NULL;
	Type *container_type = (decl && decl->strukt.container_type) ? decl->strukt.container_type->type : type_uint;
	Type *flat_container = type_flatten(container_type);

	if (init->kind == CONST_INIT_ZERO)
	{
		PRINT(flat_container->type_kind == TYPE_ARRAY ? "{0}" : "0");
		return;
	}

	if (flat_container->type_kind == TYPE_ARRAY)
	{
		int len = (int)flat_container->array.len;
		if (len <= 0)
		{
			len = 1;
		}
		uint8_t *bytes = (uint8_t *)calloc(len, 1);
		if (init->kind == CONST_INIT_STRUCT)
		{
			Decl **members = decl ? decl->strukt.members : NULL;
			int m_count    = vec_size(members);
			int init_count = vec_size(init->init_struct);
			bool rev       = c_is_bitstruct_big_endian(decl);
			for (int i = 0; i < m_count && i < init_count; i++)
			{
				ConstInitializer *val = init->init_struct[i];
				if (!val || val->kind == CONST_INIT_ZERO)
				{
					continue;
				}
				uint64_t v = c_get_const_init_val(val);
				int start_bit = 0, end_bit = 0;
				c_get_bitstruct_member_bits(members[i], &start_bit, &end_bit);
				c_bit_set_bytes_internal(bytes, start_bit, end_bit, rev, v);
			}
		}
		PRINT("{ { ");
		for (int i = 0; i < len; i++)
		{
			if (i > 0)
			{
				PRINT(", ");
			}
			PRINTF("0x%02X", bytes[i]);
		}
		PRINT(" } }");
		free(bytes);
		return;
	}

	uint64_t total = 0;
	if (init->kind == CONST_INIT_STRUCT)
	{
		Decl **members = decl ? decl->strukt.members : NULL;
		int m_count    = vec_size(members);
		int init_count = vec_size(init->init_struct);
		for (int i = 0; i < m_count && i < init_count; i++)
		{
			ConstInitializer *val = init->init_struct[i];
			if (!val || val->kind == CONST_INIT_ZERO)
			{
				continue;
			}
			uint64_t v = c_get_const_init_val(val);
			int start_bit = 0, end_bit = 0;
			c_get_bitstruct_member_bits(members[i], &start_bit, &end_bit);
			int bit_size = end_bit - start_bit + 1;
			if (bit_size <= 0)
			{
				bit_size = 1;
			}
			uint64_t mask = (bit_size >= 64) ? ~0ULL : (((uint64_t)1 << bit_size) - 1);
			v &= mask;
			total |= (start_bit >= 64 ? 0 : (v << start_bit));
		}
	}
	else
	{
		total = c_get_const_init_val(init);
	}
	if (c_is_bitstruct_requires_byteswap(decl))
	{
		TypeSize sz = type_size(flat_container);
		if (sz == 2)
		{
			total = c_bswap16((uint16_t)total);
		}
		else if (sz == 4)
		{
			total = c_bswap32((uint32_t)total);
		}
		else if (sz == 8)
		{
			total = c_bswap64((uint64_t)total);
		}
	}
	PRINTF("0x%" PRIx64 "ULL", total);
}

void c_emit_const_initializer_rec(GenContext *c, ConstInitializer *init)
{
	if (!init || init->kind == CONST_INIT_ZERO)
	{
		Type *t = init ? init->type : NULL;
		PRINTF("%s", c_type_zero_literal(t));
		return;
	}
	if (init->kind == CONST_INIT_VALUE)
	{
		c_emit_const_init_expr(c, init->init_value, init->type);
		return;
	}
	if (init->type)
	{
		Type *flat = type_flatten(init->type);
		if (flat && flat->type_kind == TYPE_BITSTRUCT)
		{
			c_emit_const_bitstruct_initializer(c, init);
			return;
		}
		if (flat && flat->type_kind == TYPE_SLICE &&
		    (init->kind == CONST_INIT_ARRAY || init->kind == CONST_INIT_ARRAY_FULL))
		{
			Type *elem_t                = flat->array.base ? c_safe_type_lower(flat->array.base) : type_char;
			ConstInitializer **elements = init->kind == CONST_INIT_ARRAY_FULL ? init->init_array_full : init->init_array.elements;
			int el_count                = vec_size(elements);
			int len                     = init->kind == CONST_INIT_ARRAY_FULL ? el_count : (int)init->type->array.len;
			PRINTF("{ .ptr = (%s[])", c_type_name(c, elem_t));
			PRINT("{ ");
			for (int i = 0; i < el_count; i++)
			{
				if (i > 0)
				{
					PRINT(", ");
				}
				if (init->kind == CONST_INIT_ARRAY && elements[i]->kind == CONST_INIT_ARRAY_VALUE)
				{
					PRINTF("[%d] = ", (int)elements[i]->init_array_value.index);
					c_emit_const_initializer_rec(c, elements[i]->init_array_value.element);
				}
				else
				{
					c_emit_const_initializer_rec(c, elements[i]);
				}
			}
			PRINTF(" }, .len = %d }", len);
			return;
		}
	}
	switch (init->kind)
	{
		case CONST_INIT_ZERO:
		{
			Type *t = init ? init->type : NULL;
			PRINTF("%s", c_type_zero_literal(t));
			return;
		}
		case CONST_INIT_VALUE:
			c_emit_const_init_expr(c, init->init_value, init->type);
			return;
		case CONST_INIT_STRUCT:
		{
			PRINT("{ ");
			int count = vec_size(init->init_struct);
			for (int i = 0; i < count; i++)
			{
				if (i > 0)
				{
					PRINT(", ");
				}
				c_emit_const_initializer_rec(c, init->init_struct[i]);
			}
			PRINT(" }");
			return;
		}
		case CONST_INIT_UNION:
		{
			PRINTF("{ .m%d = ", (int)init->init_union.index);
			c_emit_const_initializer_rec(c, init->init_union.element);
			PRINT(" }");
			return;
		}
		case CONST_INIT_ARRAY:
		case CONST_INIT_ARRAY_FULL:
		{
			PRINT("{ { ");
			ConstInitializer **elements = init->kind == CONST_INIT_ARRAY_FULL ? init->init_array_full : init->init_array.elements;
			int count                   = vec_size(elements);
			for (int i = 0; i < count; i++)
			{
				if (i > 0)
				{
					PRINT(", ");
				}
				if (init->kind == CONST_INIT_ARRAY && elements[i]->kind == CONST_INIT_ARRAY_VALUE)
				{
					PRINTF("[%d] = ", (int)elements[i]->init_array_value.index);
					c_emit_const_initializer_rec(c, elements[i]->init_array_value.element);
				}
				else
				{
					c_emit_const_initializer_rec(c, elements[i]);
				}
			}
			PRINT(" } }");
			return;
		}
		case CONST_INIT_ARRAY_VALUE:
			c_emit_const_initializer_rec(c, init->init_array_value.element);
			return;
	}
}

void c_emit_const_float_literal(GenContext *c, double f)
{
	if (isnan(f))
	{
		PRINT("(0.0 / 0.0)");
		return;
	}
	if (isinf(f))
	{
		PRINT(f < 0 ? "(-1.0 / 0.0)" : "(1.0 / 0.0)");
		return;
	}
	if (f == 0.0 && signbit(f))
	{
		PRINT("-0.0");
		return;
	}
	PRINTF("%.17g", f);
}

void c_emit_const_int_literal(GenContext *c, const ExprConst *ec, Type *type)
{
	TypeKind tkind = type ? type->type_kind : TYPE_VOID;
	if (tkind == TYPE_U128)
	{
		if (ec->ixx.i.high != 0)
		{
			PRINTF("(((__c3_uint128)%" PRIu64 "ULL << 64) | ((__c3_uint128)%" PRIu64 "ULL))",
			       ec->ixx.i.high, ec->ixx.i.low);
		}
		else
		{
			PRINTF("((__c3_uint128)%" PRIu64 "ULL)", ec->ixx.i.low);
		}
		return;
	}
	if (tkind == TYPE_I128)
	{
		if (ec->ixx.i.high == 0)
		{
			PRINTF("((__c3_int128)(__c3_uint128)%" PRIu64 "ULL)", ec->ixx.i.low);
		}
		else if (ec->ixx.i.high == UINT64_MAX && (int64_t)ec->ixx.i.low < 0)
		{
			if ((int64_t)ec->ixx.i.low == INT64_MIN)
			{
				PRINT("((__c3_int128)(-9223372036854775807LL - 1LL))");
			}
			else
			{
				PRINTF("((__c3_int128)%" PRId64 "LL)", (int64_t)ec->ixx.i.low);
			}
		}
		else
		{
			PRINTF("((__c3_int128)(((__c3_uint128)%" PRIu64 "ULL << 64) | ((__c3_uint128)%" PRIu64 "ULL)))",
			       ec->ixx.i.high, ec->ixx.i.low);
		}
		return;
	}
	if (type && type_is_unsigned(type))
	{
		PRINTF("%" PRIu64 "ULL", int_to_u64(ec->ixx));
		return;
	}

	int64_t val = int_to_i64(ec->ixx);
	if (val == INT64_MIN)
	{
		PRINT("(-9223372036854775807LL - 1LL)");
	}
	else if (val == INT32_MIN)
	{
		PRINT("(-2147483647 - 1)");
	}
	else
	{
		PRINTF("%" PRId64 "LL", val);
	}
}

void c_emit_const_init_expr(GenContext *c, Expr *expr, Type *type)
{
	type              = (type && is_valid_type_ptr(type)) ? c_safe_type_lower(type) : type_void;
	const char *tname = c_type_name(c, type);
	if (!expr)
	{
		PRINTF("%s", c_type_zero_literal(type));
		return;
	}
	if (expr->expr_kind == EXPR_TYPEID)
	{
		Type *t = expr->type_expr ? expr->type_expr->type : expr->type;
		if (!t)
		{
			t = type_void;
		}
		PRINTF("(c3typeid_t)&%s", c_typeid_name(t));
		return;
	}
	if (expr->expr_kind == EXPR_MAKE_ANY)
	{
		PRINT("(__c3_any__){ .ptr = ");
		if (expr->make_any_expr.inner)
		{
			PRINT("(void*)(");
			c_emit_const_init_expr(c, expr->make_any_expr.inner, type_voidptr);
			PRINT(")");
		}
		else
		{
			PRINT("NULL");
		}
		PRINT(", .typeid = ");
		if (expr->make_any_expr.typeid)
		{
			c_emit_const_init_expr(c, expr->make_any_expr.typeid, type_typeid);
		}
		else
		{
			PRINT("NULL");
		}
		PRINT(" }");
		return;
	}
	if (expr->expr_kind == EXPR_MAKE_SLICE)
	{
		PRINTF("(%s){ .ptr = ", tname);
		if (expr->make_slice_expr.ptr)
		{
			PRINT("(void*)(");
			c_emit_const_init_expr(c, expr->make_slice_expr.ptr, type_voidptr);
			PRINT(")");
		}
		else
		{
			PRINT("NULL");
		}
		PRINTF(", .len = %llu }", (unsigned long long)expr->make_slice_expr.len);
		return;
	}
	if (expr->expr_kind == EXPR_UNARY)
	{
		UnaryOp uop = expr->unary_expr.operator;
		if (uop == UNARYOP_ADDR || uop == UNARYOP_TADDR)
		{
			Expr *inner = expr->unary_expr.expr;
			if (inner && (inner->expr_kind == EXPR_IDENTIFIER || inner->expr_kind == EXPR_DECL))
			{
				Decl *d = (inner->expr_kind == EXPR_DECL) ? inner->decl_expr : c_decl_unwrap(inner->ident_expr);
				if (d && (d->decl_kind == DECL_FUNC || c_is_file_global(d)))
				{
					c_emit_global_decl(c, d);
					PRINTF("(%s)&%s", tname, c_get_decl_name(d));
					return;
				}
			}
			Type *inner_t = (inner && inner->type && is_valid_type_ptr(inner->type)) ? c_safe_type_lower(inner->type) : type_void;
			if (inner_t->type_kind == TYPE_VOID)
			{
				inner_t = type_int;
			}
			if (c_type_is_aggregate(inner_t))
			{
				PRINTF("(%s)&((%s)", tname, c_type_name(c, inner_t));
				c_emit_const_init_expr(c, inner, inner_t);
				PRINT(")");
			}
			else
			{
				PRINTF("(%s)&((%s){ ", tname, c_type_name(c, inner_t));
				c_emit_const_init_expr(c, inner, inner_t);
				PRINT(" })");
			}
			return;
		}
		if (uop == UNARYOP_NEG || uop == UNARYOP_BITNEG || uop == UNARYOP_NOT)
		{
			const char *uop_str = (uop == UNARYOP_NEG) ? "-" : (uop == UNARYOP_BITNEG ? "~" : "!");
			PRINTF("%s(", uop_str);
			c_emit_const_init_expr(c, expr->unary_expr.expr, type);
			PRINT(")");
			return;
		}
	}
	if (expr->expr_kind == EXPR_BINARY)
	{
		const char *op_str = "+";
		switch (expr->binary_expr.operator)
		{
			case BINARYOP_MULT: op_str = "*"; break;
			case BINARYOP_SUB: op_str = "-"; break;
			case BINARYOP_ADD: op_str = "+"; break;
			case BINARYOP_DIV: op_str = "/"; break;
			case BINARYOP_MOD: op_str = "%"; break;
			case BINARYOP_SHR: op_str = ">>"; break;
			case BINARYOP_SHL: op_str = "<<"; break;
			case BINARYOP_BIT_OR: op_str = "|"; break;
			case BINARYOP_BIT_XOR: op_str = "^"; break;
			case BINARYOP_BIT_AND: op_str = "&"; break;
			default: break;
		}
		PRINT("(");
		c_emit_const_init_expr(c, exprptr(expr->binary_expr.left), type);
		PRINTF(" %s ", op_str);
		c_emit_const_init_expr(c, exprptr(expr->binary_expr.right), type);
		PRINT(")");
		return;
	}
	if (expr->expr_kind == EXPR_IDENTIFIER && expr->ident_expr)
	{
		Decl *d = c_decl_unwrap(expr->ident_expr);
		if (d)
		{
			if (d->decl_kind == DECL_FUNC)
			{
				PRINTF("(%s)&%s", tname, c_get_decl_name(d));
				return;
			}
			if (d->decl_kind == DECL_ENUM_CONSTANT)
			{
				PRINTF("%d", d->enum_constant.inner_ordinal);
				return;
			}
			if (d->decl_kind == DECL_FAULT)
			{
				const char *fsym = c_fault_symbol_name(d);
				PRINTF("(c3fault_t)%s", fsym);
				return;
			}
			if (d->decl_kind == DECL_VAR && d->var.kind == VARDECL_CONST && d->var.init_expr)
			{
				c_emit_const_init_expr(c, d->var.init_expr, type);
				return;
			}
			if (c_is_file_global(d))
			{
				c_emit_global_decl(c, d);
				Type *dt = c_decl_type(d);
				if (type->type_kind == TYPE_SLICE && dt && c_type_is_vec_or_arr(dt))
				{
					PRINTF("(%s){ .ptr = (void*)%s.ptr, .len = %llu }", tname, c_get_decl_name(d), (unsigned long long)dt->array.len);
					return;
				}
				PRINTF("%s", c_get_decl_name(d));
				return;
			}
		}
	}
	if (expr->expr_kind == EXPR_DECL && expr->decl_expr)
	{
		Decl *d = expr->decl_expr;
		if (c_is_file_global(d))
		{
			c_emit_global_decl(c, d);
			PRINTF("%s", c_get_decl_name(d));
			return;
		}
		if (d->var.init_expr)
		{
			c_emit_const_init_expr(c, d->var.init_expr, type);
			return;
		}
	}
	if (expr->expr_kind == EXPR_RECAST || expr->expr_kind == EXPR_ADDR_CONVERSION || expr->expr_kind == EXPR_RVALUE ||
	    expr->expr_kind == EXPR_INT_TO_PTR || expr->expr_kind == EXPR_PTR_TO_INT || expr->expr_kind == EXPR_INT_TO_FLOAT ||
	    expr->expr_kind == EXPR_FLOAT_TO_INT || expr->expr_kind == EXPR_EXT_TRUNC)
	{
		PRINTF("(%s)(", tname);
		c_emit_const_init_expr(c, expr->inner_expr, type);
		PRINT(")");
		return;
	}
	if (expr->expr_kind == EXPR_TWO)
	{
		c_emit_const_init_expr(c, expr->two_expr.last, type);
		return;
	}
	if (expr->expr_kind == EXPR_INITIALIZER_LIST)
	{
		Type *elem_t    = (type && type->type_kind == TYPE_SLICE && type->array.base) ? c_safe_type_lower(type->array.base) : type_char;
		Expr **elements = expr->initializer_list;
		int count       = vec_size(elements);
		if (type && type->type_kind == TYPE_SLICE)
		{
			PRINTF("{ .ptr = (%s[])", c_type_name(c, elem_t));
			PRINT("{ ");
			for (int i = 0; i < count; i++)
			{
				if (i > 0)
				{
					PRINT(", ");
				}
				c_emit_const_init_expr(c, elements[i], elem_t);
			}
			PRINTF(" }, .len = %d }", count);
			return;
		}
		if (type && type->type_kind == TYPE_STRUCT)
		{
			Decl *d        = type->decl;
			Decl **members = d ? d->strukt.members : NULL;
			PRINT("{ ");
			for (int i = 0; i < count; i++)
			{
				if (i > 0)
				{
					PRINT(", ");
				}
				Type *mt = (members && i < vec_size(members)) ? members[i]->type : elem_t;
				c_emit_const_init_expr(c, elements[i], mt);
			}
			PRINT(" }");
			return;
		}
		PRINT("{ { ");
		for (int i = 0; i < count; i++)
		{
			if (i > 0)
			{
				PRINT(", ");
			}
			c_emit_const_init_expr(c, elements[i], elem_t);
		}
		PRINT(" } }");
		return;
	}
	if (expr->expr_kind == EXPR_DESIGNATED_INITIALIZER_LIST)
	{
		Decl *d          = (type && is_valid_type_ptr(type)) ? type->decl : NULL;
		Decl **members   = d ? d->strukt.members : NULL;
		int member_count = vec_size(members);
		Expr **elements  = expr->designated_init.list;
		PRINT("{ ");
		int des_count = vec_size(elements);
		for (int i = 0; i < des_count; i++)
		{
			Expr *des = elements[i];
			if (des && des->expr_kind == EXPR_DESIGNATOR && vec_size(des->designator_expr.path))
			{
				if (i > 0)
				{
					PRINT(", ");
				}
				DesignatorElement *de = des->designator_expr.path[0];
				Type *mt              = (members && de->index < member_count) ? members[de->index]->type : NULL;
				PRINTF(".m%d = ", (int)de->index);
				c_emit_const_init_expr(c, des->designator_expr.value, mt);
			}
		}
		PRINT(" }");
		return;
	}
	if (expr->expr_kind == EXPR_CONST)
	{
		switch (expr->const_expr.const_kind)
		{
			case CONST_TYPEID:
			{
				Type *tval = expr->const_expr.typeid;
				if (!tval)
				{
					tval = type_void;
				}
				PRINTF("(c3typeid_t)&%s", c_typeid_name(tval));
				return;
			}
			case CONST_INTEGER:
				c_emit_const_int_literal(c, &expr->const_expr, type);
				return;
			case CONST_FLOAT:
				c_emit_const_float_literal(c, expr->const_expr.fxx.f);
				return;
			case CONST_BOOL:
				PRINT(expr->const_expr.b ? "true" : "false");
				return;
			case CONST_POINTER:
				if (expr->const_expr.ptr == 0)
				{
					PRINTF("%s", c_type_zero_literal(type));
				}
				else
				{
					if (c_type_is_aggregate(type))
					{
						PRINT("{0}");
					}
					else
					{
						PRINTF("(void*)(uintptr_t)0x%" PRIx64 "ULL", expr->const_expr.ptr);
					}
				}
				return;
			case CONST_STRING:
			case CONST_BYTES:
				if (type->type_kind == TYPE_SLICE)
				{
					PRINT("{ .ptr = (void*)");
					c_emit_string_literal(c, expr->const_expr.bytes.ptr, expr->const_expr.bytes.len);
					PRINTF(", .len = %llu }", (unsigned long long)expr->const_expr.bytes.len);
				}
				else if (type->type_kind == TYPE_ARRAY)
				{
					PRINT("{ { ");
					c_emit_string_literal(c, expr->const_expr.bytes.ptr, expr->const_expr.bytes.len);
					PRINT(" } }");
				}
				else
				{
					c_emit_string_literal(c, expr->const_expr.bytes.ptr, expr->const_expr.bytes.len);
				}
				return;
			case CONST_REF:
			{
				Decl *d = expr->const_expr.global_ref;
				d       = c_decl_unwrap(d);
				if (d && d->decl_kind == DECL_FUNC)
				{
					PRINTF("(%s)&%s", tname, c_get_decl_name(d));
				}
				else if (c_is_file_global(d))
				{
					PRINTF("(%s)&%s", tname, c_get_decl_name(d));
				}
				else
				{
					VariableId vid = c_get_or_create_decl_var(c, d);
					PRINTF("(%s)&___var_%d", tname, vid);
				}
				return;
			}
			case CONST_ENUM:
				PRINTF("%d", expr->const_expr.enum_val->enum_constant.inner_ordinal);
				return;
			case CONST_FAULT:
				if (expr->const_expr.fault)
				{
					const char *fsym = c_fault_symbol_name(expr->const_expr.fault);
					PRINTF("(c3fault_t)%s", fsym);
				}
				else
				{
					PRINTF("%s", c_type_zero_literal(type));
				}
				return;
			case CONST_INITIALIZER:
			case CONST_SLICE:
			{
				ConstInitializer *s_init = (expr->const_expr.const_kind == CONST_SLICE) ? expr->const_expr.slice_init : expr->const_expr.initializer;
				if (type && type->type_kind == TYPE_SLICE)
				{
					if (!s_init || s_init->kind == CONST_INIT_ZERO)
					{
						PRINT("{0}");
						return;
					}
					if (s_init->kind == CONST_INIT_VALUE)
					{
						c_emit_const_init_expr(c, s_init->init_value, type);
						return;
					}
					Type *elem_t                = type->array.base ? c_safe_type_lower(type->array.base) : type_char;
					ConstInitializer **elements = NULL;
					int count                   = 0;
					if (s_init->kind == CONST_INIT_ARRAY_FULL)
					{
						elements = s_init->init_array_full;
						count    = vec_size(elements);
					}
					else if (s_init->kind == CONST_INIT_ARRAY)
					{
						elements = s_init->init_array.elements;
						count    = (int)s_init->type->array.len;
					}
					else if (s_init->kind == CONST_INIT_STRUCT)
					{
						elements = s_init->init_struct;
						count    = vec_size(elements);
					}
					if (elements)
					{
						int el_count = vec_size(elements);
						PRINTF("{ .ptr = (%s[])", c_type_name(c, elem_t));
						PRINT("{ ");
						for (int i = 0; i < el_count; i++)
						{
							if (i > 0)
							{
								PRINT(", ");
							}
							if (s_init->kind == CONST_INIT_ARRAY && elements[i]->kind == CONST_INIT_ARRAY_VALUE)
							{
								c_emit_const_initializer_rec(c, elements[i]->init_array_value.element);
							}
							else
							{
								c_emit_const_initializer_rec(c, elements[i]);
							}
						}
						PRINTF(" }, .len = %d }", count);
						return;
					}
				}
				if (s_init)
				{
					c_emit_const_initializer_rec(c, s_init);
				}
				else
				{
					PRINTF("%s", c_type_zero_literal(type));
				}
				return;
			}
			default:
				break;
		}
	}
	PRINTF("%s", c_type_zero_literal(type));
}

static const char *c_get_align_attr_str(Decl *decl, Type *type)
{
	AlignSize align = decl ? decl->alignment : 0;
	if (!align && decl && is_valid_type_ptr(decl->type) && c_type_is_resolved(decl->type))
	{
		align = type_alloca_alignment(decl->type);
	}
	if (!align && type && is_valid_type_ptr(type) && c_type_is_resolved(type))
	{
		align = type_alloca_alignment(type);
	}
	if (align < 16 && c_get_type_size(type) >= 16)
	{
		align = 16;
	}
	AlignSize abi_align = (c_type_is_resolved(type)) ? type_abi_alignment(type) : 1;
	return (align > abi_align) ? str_printf("__c3_aligned(%u) ", (unsigned)align) : "";
}

void c_emit_global_decl(GenContext *c, Decl *var)
{
	if (!var || var->replacement || var->is_template)
	{
		return;
	}
	var = decl_raw(var);
	if (!var || var->decl_kind != DECL_VAR || !c_is_file_global(var))
	{
		return;
	}

	const char *vname = c_get_decl_name(var);
	if (htable_get(&c->emitted_global_decls, (void *)vname))
	{
		return;
	}

	Type *var_type = c_decl_type(var);
	if (!var_type || var_type->type_kind == TYPE_VOID)
	{
		var_type = type_voidptr;
	}

	htable_set(&c->emitted_global_decls, (void *)vname, (void *)1);
	c_emit_type_forward_decl(c, var_type);
	const char *tname     = c_type_name(c, var_type);
	const char *align_str = c_get_align_attr_str(var, var_type);
	PRINT("extern ");
	if (var->var.kind == VARDECL_CONST)
	{
		PRINT("const ");
	}
	if (var->var.is_threadlocal)
	{
		PRINT("__c3_thread_local ");
	}
	PRINTF("%s%s %s", align_str, tname, vname);
	const char *asm_name = c_get_decl_asm_name(var);
	if (asm_name && (strncmp(asm_name, "__atomic_", 9) == 0 || strncmp(asm_name, "__builtin_", 10) == 0))
	{
		PRINTF(" __asm__(\"%s\")", asm_name);
	}
	PRINT(";\n");
	if (IS_OPTIONAL(var))
	{
		PRINTF("extern __c3_thread_local c3fault_t %s__f;\n", vname);
	}
}

void c_emit_global_def(GenContext *c, Decl *var)
{
	if (!var || var->replacement || var->is_template)
	{
		return;
	}
	var = decl_raw(var);
	if (!var || var->decl_kind != DECL_VAR || !c_is_file_global(var))
	{
		return;
	}
	if (var->is_extern)
	{
		c_emit_global_decl(c, var);
		return;
	}

	const char *vname = c_get_decl_name(var);
	if (htable_get(&c->emitted_global_defs, (void *)vname))
	{
		return;
	}

	Type *var_type = c_decl_type(var);
	if (!var_type || var_type->type_kind == TYPE_VOID)
	{
		var_type = type_voidptr;
	}

	htable_set(&c->emitted_global_defs, (void *)vname, (void *)1);
	c_emit_type_forward_decl(c, var_type);
	const char *tname     = c_type_name(c, var_type);
	const char *align_str = c_get_align_attr_str(var, var_type);
	if (var->var.kind == VARDECL_CONST)
	{
		PRINT("const ");
	}
	if (var->var.is_threadlocal)
	{
		PRINT("__c3_thread_local ");
	}
	PRINTF("%s%s %s", align_str, tname, vname);
	if (var->var.init_expr && c_can_emit_static_initializer(var->var.init_expr))
	{
		PRINT(" = ");
		c_emit_const_init_expr(c, var->var.init_expr, var_type);
		PRINT(";\n");
	}
	else
	{
		PRINT(" = {0};\n");
	}
	if (IS_OPTIONAL(var))
	{
		PRINTF("__c3_thread_local c3fault_t %s__f = NULL;\n", vname);
	}
}

typedef struct
{
	Decl *dyn_fn;
	Decl ***out_impls;
} DynamicImplSearch;

static void c_find_dyn_impl_visitor(GenContext *c, Decl *m, void *ud)
{
	(void)c;
	DynamicImplSearch *search = (DynamicImplSearch *)ud;
	Decl *dyn_fn              = search->dyn_fn;
	if (!m || m->decl_kind != DECL_FUNC || !m->name)
	{
		return;
	}
	if (m->func_decl.attr_interface_method)
	{
		return;
	}
	Decl *def_method = declptrzero(dyn_fn->func_decl.default_method);
	if (m == def_method)
	{
		return;
	}
	Type *target_type = c_get_method_target_type(m);
	if (!target_type || !is_valid_type_ptr(target_type))
	{
		return;
	}
	if (strcmp(m->name, dyn_fn->name) != 0)
	{
		return;
	}
	if (strip_unused() && !m->is_live)
	{
		return;
	}

	Signature *dyn_sig  = &dyn_fn->func_decl.signature;
	Signature *m_sig    = &m->func_decl.signature;
	int dyn_param_count = vec_size(dyn_sig->params);
	if (vec_size(m_sig->params) != dyn_param_count)
	{
		return;
	}

	Type *dyn_rtype = typeget(dyn_sig->rtype);
	if (!dyn_rtype)
	{
		dyn_rtype = type_void;
	}
	dyn_rtype = c_safe_type_lower(dyn_rtype);

	Type *m_rtype = typeget(m_sig->rtype);
	if (!m_rtype)
	{
		m_rtype = type_void;
	}
	m_rtype = c_safe_type_lower(m_rtype);

	if (m_rtype != dyn_rtype && strcmp(c_type_name(NULL, m_rtype), c_type_name(NULL, dyn_rtype)) != 0)
	{
		return;
	}

	for (int p_i = 1; p_i < dyn_param_count; p_i++)
	{
		Type *pt1 = c_decl_type(dyn_sig->params[p_i]);
		Type *pt2 = c_decl_type(m_sig->params[p_i]);
		if (pt1 != pt2 && strcmp(c_type_name(NULL, pt1), c_type_name(NULL, pt2)) != 0)
		{
			return;
		}
	}

	FOREACH(Decl *, existing, *(search->out_impls))
	{
		if (existing == m)
		{
			return;
		}
	}
	vec_add(*(search->out_impls), m);
}

void c_emit_dynamic_dispatcher(GenContext *c, Decl *dyn_fn)
{
	if (!dyn_fn || !dyn_fn->name)
	{
		return;
	}
	dyn_fn = c_decl_unwrap(dyn_fn);
	if (!dyn_fn || !dyn_fn->name)
	{
		return;
	}

	const char *fn_decl_name = c_get_decl_name(dyn_fn);
	const char *disp_name    = c_intern(str_printf("__c3_dyn_%s", fn_decl_name));
	if (htable_get(&c->decl_names, (void *)disp_name))
	{
		return;
	}
	htable_set(&c->decl_names, (void *)disp_name, (void *)1);

	Decl **impls             = NULL;
	DynamicImplSearch search = {.dyn_fn = dyn_fn, .out_impls = &impls};
	c_traverse_all_modules(c, c_find_dyn_impl_visitor, &search);

	FOREACH(Decl *, impl, impls)
	{
		c_emit_function_decl(c, impl, false);
	}

	Decl *def_method = declptrzero(dyn_fn->func_decl.default_method);
	if (def_method && strip_unused() && !def_method->is_live)
	{
		def_method = NULL;
	}
	if (def_method)
	{
		c_emit_function_decl(c, def_method, false);
	}

	const char *resolve_name = c_intern(str_printf("__c3_dyn_resolve_%s", fn_decl_name));
	htable_set(&c->decl_names, (void *)resolve_name, (void *)1);

	PRINTF("static inline void *%s(c3typeid_t ___typeid) {\n", resolve_name);
	PRINTF("\tif (!___typeid) { return NULL; }\n");
	PRINTF("\tfor (c3typeid_t ___t = ___typeid; ___t != NULL; ___t = ___t->parentof) {\n");
	FOREACH(Decl *, impl, impls)
	{
		Type *target_type        = c_get_method_target_type(impl);
		const char *impl_fn_name = c_get_decl_name(impl);
		if (target_type)
		{
			const char *tsym = c_typeid_name(target_type);
			PRINTF("\t\tif (___t == (c3typeid_t)&%s) { return (void *)&%s; }\n", tsym, impl_fn_name);
		}
	}
	PRINTF("\t}\n");
	if (def_method)
	{
		const char *def_fn_name = c_get_decl_name(def_method);
		PRINTF("\treturn (void *)&%s;\n", def_fn_name);
	}
	else
	{
		PRINTF("\treturn NULL;\n");
	}
	PRINTF("}\n\n");

	Signature *sig = &dyn_fn->func_decl.signature;
	Type *rtype    = typeget(sig->rtype);
	if (!rtype)
	{
		rtype = type_void;
	}
	rtype = c_safe_type_lower(rtype);
	c_emit_type_forward_decl(c, rtype);
	const char *ret_tname = c_type_name(c, rtype);

	PRINTF("static inline %s %s(__c3_any__ ___self", ret_tname, disp_name);
	int param_count = vec_size(sig->params);
	for (int i = 1; i < param_count; i++)
	{
		Decl *p     = sig->params[i];
		Type *ptype = c_decl_type(p);
		c_emit_type_forward_decl(c, ptype);
		PRINTF(", %s ___p%d", c_type_name(c, ptype), i);
	}
	PRINTF(") {\n");

	for (int pass = 0; pass < 2; pass++)
	{
		FOREACH(Decl *, impl, impls)
		{
			Type *target_type = c_get_method_target_type(impl);
			if (!target_type)
			{
				continue;
			}
			c_emit_type_forward_decl(c, target_type);
			Signature *m_sig  = &impl->func_decl.signature;
			Decl *first_p     = (m_sig->params && vec_size(m_sig->params) > 0) ? m_sig->params[0] : NULL;
			Type *first_ptype = first_p ? c_decl_type(first_p) : NULL;
			if (first_ptype && is_valid_type_ptr(first_ptype))
			{
				c_emit_type_forward_decl(c, first_ptype);
			}
			bool pass_by_val     = first_ptype && !type_is_pointer(first_ptype);
			const char *self_arg = pass_by_val
			                           ? str_printf("*(%s*)___self.ptr", c_type_name(c, first_ptype))
			                           : str_printf("(%s)___self.ptr", c_type_name(c, first_ptype));

			const char *tsym         = c_typeid_name(target_type);
			const char *impl_fn_name = c_get_decl_name(impl);
			if (pass == 0)
			{
				PRINTF("\tif ((c3typeid_t)___self.typeid == (c3typeid_t)&%s) {\n", tsym);
			}
			else
			{
				PRINTF("\tif (__c3_type_matches((c3typeid_t)___self.typeid, (c3typeid_t)&%s)) {\n", tsym);
			}
			PRINTF("\t\t%s%s(%s", (rtype->type_kind != TYPE_VOID) ? "return " : "", impl_fn_name, self_arg);
			for (int i = 1; i < param_count; i++)
			{
				PRINTF(", ___p%d", i);
			}
			PRINTF(");\n");
			if (rtype->type_kind == TYPE_VOID)
			{
				PRINTF("\t\treturn;\n");
			}
			PRINTF("\t}\n");
		}
	}

	if (def_method)
	{
		Signature *def_sig = &def_method->func_decl.signature;
		Decl *first_dp     = (def_sig->params && vec_size(def_sig->params) > 0) ? def_sig->params[0] : NULL;
		Type *first_dptype = first_dp ? c_decl_type(first_dp) : NULL;
		if (first_dptype && is_valid_type_ptr(first_dptype))
		{
			c_emit_type_forward_decl(c, first_dptype);
		}
		bool def_is_any          = first_dptype && (first_dptype->type_kind == TYPE_ANY || first_dptype->type_kind == TYPE_INTERFACE);
		bool def_is_any_ptr      = first_dptype && type_is_pointer(first_dptype) && first_dptype->pointer && (first_dptype->pointer->type_kind == TYPE_ANY || first_dptype->pointer->type_kind == TYPE_INTERFACE);
		bool def_pass_by_val     = first_dptype && !type_is_pointer(first_dptype);
		const char *def_self_arg = def_is_any ? "___self"
		                                      : (def_is_any_ptr ? "&___self"
		                                                        : (def_pass_by_val ? str_printf("*(%s*)___self.ptr", c_type_name(c, first_dptype))
		                                                                           : "(void*)___self.ptr"));

		const char *def_fn_name = c_get_decl_name(def_method);
		PRINTF("\t%s%s(%s", (rtype->type_kind != TYPE_VOID) ? "return " : "", def_fn_name, def_self_arg);
		for (int i = 1; i < param_count; i++)
		{
			PRINTF(", ___p%d", i);
		}
		PRINTF(");\n");
		if (rtype->type_kind == TYPE_VOID)
		{
			PRINTF("\treturn;\n");
		}
	}
	else
	{
		PRINTF("\t__c3_abort();\n");
		if (rtype->type_kind != TYPE_VOID)
		{
			PRINTF("\treturn (%s)%s;\n", ret_tname, c_type_zero_literal(rtype));
		}
	}
	PRINTF("}\n\n");
}

static void c_collect_and_emit_bitstructs(FILE *f, HTable *emitted, Decl *d)
{
	if (!d || d->is_template || d->replacement)
	{
		return;
	}
	if (d->decl_kind == DECL_BITSTRUCT)
	{
		if (!d->strukt.container_type || !is_valid_type_ptr(d->strukt.container_type->type))
		{
			return;
		}
		const char *bname = c_get_decl_name(d);
		if (!htable_get(emitted, (void *)bname))
		{
			htable_set(emitted, (void *)bname, (void *)1);
			c_emit_bitstruct_accessors_to_file(f, d);
		}
	}
	if (decl_has_members(d) && d->strukt.members)
	{
		FOREACH(Decl *, m, d->strukt.members)
		{
			c_collect_and_emit_bitstructs(f, emitted, m);
		}
	}
}

static const char c_runtime_header_boilerplate[] =
    "/* Generated by C3 Compiler */\n"
    "#ifndef __C3_RUNTIME_H__\n"
    "#define __C3_RUNTIME_H__\n\n"
    "#if defined(__clang__)\n"
    "#pragma clang diagnostic ignored \"-Wunknown-warning-option\"\n"
    "#elif defined(__GNUC__)\n"
    "#pragma GCC diagnostic ignored \"-Wbuiltin-declaration-mismatch\"\n"
    "#endif\n\n"
    "#include <stdint.h>\n"
    "#include <stddef.h>\n"
    "#include <stdbool.h>\n"
    "#if defined(_MSC_VER)\n"
    "#define __c3_thread_local __declspec(thread)\n"
    "#else\n"
    "#define __c3_thread_local __thread\n"
    "#endif\n\n"
    "#if defined(__GNUC__) || defined(__clang__)\n"
    "#define __c3_aligned(n) __attribute__((aligned(n)))\n"
    "#define __c3_packed     __attribute__((packed))\n"
    "#elif defined(_MSC_VER)\n"
    "#define __c3_aligned(n) __declspec(align(n))\n"
    "#define __c3_packed\n"
    "#else\n"
    "#define __c3_aligned(n)\n"
    "#define __c3_packed\n"
    "#endif\n\n"
    "#define __c3_memcpy   __builtin_memcpy\n"
    "#define __c3_memset   __builtin_memset\n"
    "#define __c3_memmove  __builtin_memmove\n"
    "#define __c3_memcmp   __builtin_memcmp\n"
    "#define __c3_fmod     __builtin_fmod\n\n"
    "#if defined(__TINYC__)\n"
    "extern double round(double);\n"
    "extern double copysign(double, double);\n"
    "extern double fmod(double, double);\n"
    "extern double fabs(double);\n"
    "extern double floor(double);\n"
    "extern double ceil(double);\n"
    "extern double trunc(double);\n"
    "extern double rint(double);\n"
    "extern double nearbyint(double);\n"
    "extern long lrint(double);\n"
    "extern long lround(double);\n"
    "extern double sqrt(double);\n"
    "extern double sin(double);\n"
    "extern double cos(double);\n"
    "extern double tan(double);\n"
    "extern double asin(double);\n"
    "extern double acos(double);\n"
    "extern double atan(double);\n"
    "extern double sinh(double);\n"
    "extern double cosh(double);\n"
    "extern double tanh(double);\n"
    "extern double exp(double);\n"
    "extern double exp2(double);\n"
    "extern double log(double);\n"
    "extern double log2(double);\n"
    "extern double log10(double);\n"
    "extern double pow(double, double);\n"
    "extern double fma(double, double, double);\n"
    "#define __builtin_round       round\n"
    "#define __builtin_copysign    copysign\n"
    "#define __builtin_fmod        fmod\n"
    "#define __builtin_fabs        fabs\n"
    "#define __builtin_floor       floor\n"
    "#define __builtin_ceil        ceil\n"
    "#define __builtin_trunc       trunc\n"
    "#define __builtin_rint        rint\n"
    "#define __builtin_nearbyint   nearbyint\n"
    "#define __builtin_lrint       lrint\n"
    "#define __builtin_lround      lround\n"
    "#define __builtin_sqrt        sqrt\n"
    "#define __builtin_sin         sin\n"
    "#define __builtin_cos         cos\n"
    "#define __builtin_tan         tan\n"
    "#define __builtin_asin        asin\n"
    "#define __builtin_acos        acos\n"
    "#define __builtin_atan        atan\n"
    "#define __builtin_sinh        sinh\n"
    "#define __builtin_cosh        cosh\n"
    "#define __builtin_tanh        tanh\n"
    "#define __builtin_exp         exp\n"
    "#define __builtin_exp2        exp2\n"
    "#define __builtin_exp10(x)    pow(10.0, (x))\n"
    "#define __builtin_log         log\n"
    "#define __builtin_log2        log2\n"
    "#define __builtin_log10       log10\n"
    "#define __builtin_pow         pow\n"
    "#define __builtin_fma         fma\n"
    "#define __builtin_prefetch(addr, ...) ((void)0)\n"
    "static inline int __c3_tcc_clz64(uint64_t x) {\n"
    "\tif (x == 0) return 64;\n"
    "\tint n = 0;\n"
    "\tif ((x >> 32) == 0) { n += 32; x <<= 32; }\n"
    "\tif ((x >> 48) == 0) { n += 16; x <<= 16; }\n"
    "\tif ((x >> 56) == 0) { n += 8;  x <<= 8;  }\n"
    "\tif ((x >> 60) == 0) { n += 4;  x <<= 4;  }\n"
    "\tif ((x >> 62) == 0) { n += 2;  x <<= 2;  }\n"
    "\tif ((x >> 63) == 0) { n += 1; }\n"
    "\treturn n;\n"
    "}\n"
    "#define __builtin_clzll __c3_tcc_clz64\n"
    "#define __builtin_clzl  __c3_tcc_clz64\n"
    "#define __builtin_clz(x) (__c3_tcc_clz64(x) - 32)\n"
    "static inline int __c3_tcc_ctz64(uint64_t x) {\n"
    "\tif (x == 0) return 64;\n"
    "\tint n = 0;\n"
    "\tif ((x & 0xFFFFFFFFULL) == 0) { n += 32; x >>= 32; }\n"
    "\tif ((x & 0x0000FFFFULL) == 0) { n += 16; x >>= 16; }\n"
    "\tif ((x & 0x000000FFULL) == 0) { n += 8;  x >>= 8;  }\n"
    "\tif ((x & 0x0000000FULL) == 0) { n += 4;  x >>= 4;  }\n"
    "\tif ((x & 0x00000003ULL) == 0) { n += 2;  x >>= 2;  }\n"
    "\tif ((x & 0x00000001ULL) == 0) { n += 1; }\n"
    "\treturn n;\n"
    "}\n"
    "#define __builtin_ctzll __c3_tcc_ctz64\n"
    "#define __builtin_ctzl  __c3_tcc_ctz64\n"
    "#define __builtin_ctz   __c3_tcc_ctz64\n"
    "static inline int __c3_tcc_popcount64(uint64_t x) {\n"
    "\tx = x - ((x >> 1) & 0x5555555555555555ULL);\n"
    "\tx = (x & 0x3333333333333333ULL) + ((x >> 2) & 0x3333333333333333ULL);\n"
    "\tx = (x + (x >> 4)) & 0x0F0F0F0F0F0F0F0FULL;\n"
    "\treturn (int)((x * 0x0101010101010101ULL) >> 56);\n"
    "}\n"
    "#define __builtin_popcountll __c3_tcc_popcount64\n"
    "#define __builtin_popcountl  __c3_tcc_popcount64\n"
    "#define __builtin_popcount   __c3_tcc_popcount64\n"
    "static inline uint16_t __c3_tcc_bswap16(uint16_t x) { return (uint16_t)((x << 8) | (x >> 8)); }\n"
    "static inline uint32_t __c3_tcc_bswap32(uint32_t x) {\n"
    "\treturn ((x << 24) & 0xff000000u) | ((x << 8) & 0x00ff0000u) | ((x >> 8) & 0x0000ff00u) | ((x >> 24) & 0x000000ffu);\n"
    "}\n"
    "static inline uint64_t __c3_tcc_bswap64(uint64_t x) {\n"
    "\treturn ((x << 56) & 0xff00000000000000ull) | ((x << 40) & 0x00ff000000000000ull) |\n"
    "\t       ((x << 24) & 0x0000ff0000000000ull) | ((x << 8)  & 0x000000ff00000000ull) |\n"
    "\t       ((x >> 8)  & 0x00000000ff000000ull) | ((x >> 24) & 0x0000000000ff0000ull) |\n"
    "\t       ((x >> 40) & 0x000000000000ff00ull) | ((x >> 56) & 0x00000000000000ffull);\n"
    "}\n"
    "#define __builtin_bswap16 __c3_tcc_bswap16\n"
    "#define __builtin_bswap32 __c3_tcc_bswap32\n"
    "#define __builtin_bswap64 __c3_tcc_bswap64\n"
    "#define __c3_abort()  (*(volatile int*)0 = 0)\n"
    "#ifndef __ATOMIC_SEQ_CST\n"
    "#define __ATOMIC_RELAXED 0\n"
    "#define __ATOMIC_CONSUME 1\n"
    "#define __ATOMIC_ACQUIRE 2\n"
    "#define __ATOMIC_RELEASE 3\n"
    "#define __ATOMIC_ACQ_REL 4\n"
    "#define __ATOMIC_SEQ_CST 5\n"
    "#endif\n"
    "#define __atomic_load_n(ptr, memorder) (*(ptr))\n"
    "#define __atomic_store_n(ptr, val, memorder) (*(ptr) = (val))\n"
    "#define __atomic_thread_fence(memorder) ((void)0)\n"
    "static inline uint64_t __c3_tcc_atomic_fetch_add(void *ptr, uint64_t val, int size) {\n"
    "#if defined(__x86_64__)\n"
    "\tif (size == 1) { uint8_t r = (uint8_t)val; __asm__ __volatile__(\"lock xaddb %0, %1\" : \"+r\"(r), \"+m\"(*(uint8_t*)ptr) : : \"memory\"); return r; }\n"
    "\tif (size == 2) { uint16_t r = (uint16_t)val; __asm__ __volatile__(\"lock xaddw %0, %1\" : \"+r\"(r), \"+m\"(*(uint16_t*)ptr) : : \"memory\"); return r; }\n"
    "\tif (size == 4) { uint32_t r = (uint32_t)val; __asm__ __volatile__(\"lock xaddl %0, %1\" : \"+r\"(r), \"+m\"(*(uint32_t*)ptr) : : \"memory\"); return r; }\n"
    "\tif (size == 8) { uint64_t r = (uint64_t)val; __asm__ __volatile__(\"lock xaddq %0, %1\" : \"+r\"(r), \"+m\"(*(uint64_t*)ptr) : : \"memory\"); return r; }\n"
    "#endif\n"
    "\tuint64_t old = (size == 8) ? *(uint64_t*)ptr : ((size == 4) ? *(uint32_t*)ptr : ((size == 2) ? *(uint16_t*)ptr : *(uint8_t*)ptr));\n"
    "\tif (size == 8) *(uint64_t*)ptr += val;\n"
    "\telse if (size == 4) *(uint32_t*)ptr += (uint32_t)val;\n"
    "\telse if (size == 2) *(uint16_t*)ptr += (uint16_t)val;\n"
    "\telse *(uint8_t*)ptr += (uint8_t)val;\n"
    "\treturn old;\n"
    "}\n"
    "#define __atomic_fetch_add(ptr, val, memorder) __c3_tcc_atomic_fetch_add((void*)(ptr), (uint64_t)(val), sizeof(*(ptr)))\n"
    "static inline uint64_t __c3_tcc_atomic_fetch_sub(void *ptr, uint64_t val, int size) {\n"
    "\treturn __c3_tcc_atomic_fetch_add(ptr, (uint64_t)(-(int64_t)val), size);\n"
    "}\n"
    "#define __atomic_fetch_sub(ptr, val, memorder) __c3_tcc_atomic_fetch_sub((void*)(ptr), (uint64_t)(val), sizeof(*(ptr)))\n"
    "static inline uint64_t __c3_tcc_atomic_exchange_n(void *ptr, uint64_t val, int size) {\n"
    "#if defined(__x86_64__)\n"
    "\tif (size == 1) { uint8_t r = (uint8_t)val; __asm__ __volatile__(\"xchgb %0, %1\" : \"+r\"(r), \"+m\"(*(uint8_t*)ptr) : : \"memory\"); return r; }\n"
    "\tif (size == 2) { uint16_t r = (uint16_t)val; __asm__ __volatile__(\"xchgw %0, %1\" : \"+r\"(r), \"+m\"(*(uint16_t*)ptr) : : \"memory\"); return r; }\n"
    "\tif (size == 4) { uint32_t r = (uint32_t)val; __asm__ __volatile__(\"xchgl %0, %1\" : \"+r\"(r), \"+m\"(*(uint32_t*)ptr) : : \"memory\"); return r; }\n"
    "\tif (size == 8) { uint64_t r = (uint64_t)val; __asm__ __volatile__(\"xchgq %0, %1\" : \"+r\"(r), \"+m\"(*(uint64_t*)ptr) : : \"memory\"); return r; }\n"
    "#endif\n"
    "\tuint64_t old = (size == 8) ? *(uint64_t*)ptr : ((size == 4) ? *(uint32_t*)ptr : ((size == 2) ? *(uint16_t*)ptr : *(uint8_t*)ptr));\n"
    "\tif (size == 8) *(uint64_t*)ptr = val;\n"
    "\telse if (size == 4) *(uint32_t*)ptr = (uint32_t)val;\n"
    "\telse if (size == 2) *(uint16_t*)ptr = (uint16_t)val;\n"
    "\telse *(uint8_t*)ptr = (uint8_t)val;\n"
    "\treturn old;\n"
    "}\n"
    "#define __atomic_exchange_n(ptr, val, memorder) __c3_tcc_atomic_exchange_n((void*)(ptr), (uint64_t)(val), sizeof(*(ptr)))\n"
    "static inline bool __c3_tcc_atomic_compare_exchange_n(void *ptr, void *expected, uint64_t desired, int size) {\n"
    "#if defined(__x86_64__)\n"
    "\tbool success;\n"
    "\tif (size == 1) {\n"
    "\t\tuint8_t exp = *(uint8_t*)expected;\n"
    "\t\t__asm__ __volatile__(\"lock cmpxchgb %2, %1; sete %0\" : \"=q\"(success), \"+m\"(*(uint8_t*)ptr), \"+r\"((uint8_t)desired), \"+a\"(exp) : : \"memory\");\n"
    "\t\tif (!success) *(uint8_t*)expected = exp;\n"
    "\t\treturn success;\n"
    "\t}\n"
    "\tif (size == 2) {\n"
    "\t\tuint16_t exp = *(uint16_t*)expected;\n"
    "\t\t__asm__ __volatile__(\"lock cmpxchgw %2, %1; sete %0\" : \"=q\"(success), \"+m\"(*(uint16_t*)ptr), \"+r\"((uint16_t)desired), \"+a\"(exp) : : \"memory\");\n"
    "\t\tif (!success) *(uint16_t*)expected = exp;\n"
    "\t\treturn success;\n"
    "\t}\n"
    "\tif (size == 4) {\n"
    "\t\tuint32_t exp = *(uint32_t*)expected;\n"
    "\t\t__asm__ __volatile__(\"lock cmpxchgl %2, %1; sete %0\" : \"=q\"(success), \"+m\"(*(uint32_t*)ptr), \"+r\"((uint32_t)desired), \"+a\"(exp) : : \"memory\");\n"
    "\t\tif (!success) *(uint32_t*)expected = exp;\n"
    "\t\treturn success;\n"
    "\t}\n"
    "\tif (size == 8) {\n"
    "\t\tuint64_t exp = *(uint64_t*)expected;\n"
    "\t\t__asm__ __volatile__(\"lock cmpxchgq %2, %1; sete %0\" : \"=q\"(success), \"+m\"(*(uint64_t*)ptr), \"+r\"(desired), \"+a\"(exp) : : \"memory\");\n"
    "\t\tif (!success) *(uint64_t*)expected = exp;\n"
    "\t\treturn success;\n"
    "\t}\n"
    "#endif\n"
    "\tuint64_t cur = (size == 8) ? *(uint64_t*)ptr : ((size == 4) ? *(uint32_t*)ptr : ((size == 2) ? *(uint16_t*)ptr : *(uint8_t*)ptr));\n"
    "\tuint64_t exp = (size == 8) ? *(uint64_t*)expected : ((size == 4) ? *(uint32_t*)expected : ((size == 2) ? *(uint16_t*)expected : *(uint8_t*)expected));\n"
    "\tif (cur == exp) {\n"
    "\t\tif (size == 8) *(uint64_t*)ptr = desired;\n"
    "\t\telse if (size == 4) *(uint32_t*)ptr = (uint32_t)desired;\n"
    "\t\telse if (size == 2) *(uint16_t*)ptr = (uint16_t)desired;\n"
    "\t\telse *(uint8_t*)ptr = (uint8_t)desired;\n"
    "\t\treturn true;\n"
    "\t} else {\n"
    "\t\tif (size == 8) *(uint64_t*)expected = cur;\n"
    "\t\telse if (size == 4) *(uint32_t*)expected = (uint32_t)cur;\n"
    "\t\telse if (size == 2) *(uint16_t*)expected = (uint16_t)cur;\n"
    "\t\telse *(uint8_t*)expected = (uint8_t)cur;\n"
    "\t\treturn false;\n"
    "\t}\n"
    "}\n"
    "#define __atomic_compare_exchange_n(ptr, exp, des, weak, succ, fail) __c3_tcc_atomic_compare_exchange_n((void*)(ptr), (void*)(exp), (uint64_t)(des), sizeof(*(ptr)))\n"
    "static inline bool __c3_tcc_add_overflow(uint64_t a, uint64_t b, void *res, int size, bool is_signed) {\n"
    "\tif (size == 8) {\n"
    "\t\tif (is_signed) {\n"
    "\t\t\tint64_t sa = (int64_t)a, sb = (int64_t)b;\n"
    "\t\t\tint64_t r = (int64_t)((uint64_t)sa + (uint64_t)sb);\n"
    "\t\t\t*(int64_t*)res = r;\n"
    "\t\t\treturn (sa > 0 && sb > 0 && r < sa) || (sa < 0 && sb < 0 && r > sa);\n"
    "\t\t} else {\n"
    "\t\t\tuint64_t r = a + b;\n"
    "\t\t\t*(uint64_t*)res = r;\n"
    "\t\t\treturn r < a;\n"
    "\t\t}\n"
    "\t}\n"
    "\tif (is_signed) {\n"
    "\t\tint64_t sa = (size == 4) ? (int64_t)(int32_t)a : ((size == 2) ? (int64_t)(int16_t)a : (int64_t)(int8_t)a);\n"
    "\t\tint64_t sb = (size == 4) ? (int64_t)(int32_t)b : ((size == 2) ? (int64_t)(int16_t)b : (int64_t)(int8_t)b);\n"
    "\t\tint64_t r = sa + sb;\n"
    "\t\tint64_t min = (size == 4) ? INT32_MIN : ((size == 2) ? INT16_MIN : INT8_MIN);\n"
    "\t\tint64_t max = (size == 4) ? INT32_MAX : ((size == 2) ? INT16_MAX : INT8_MAX);\n"
    "\t\tif (size == 4) *(int32_t*)res = (int32_t)r;\n"
    "\t\telse if (size == 2) *(int16_t*)res = (int16_t)r;\n"
    "\t\telse *(int8_t*)res = (int8_t)r;\n"
    "\t\treturn r < min || r > max;\n"
    "\t} else {\n"
    "\t\tuint64_t r = a + b;\n"
    "\t\tuint64_t max = (size == 4) ? UINT32_MAX : ((size == 2) ? UINT16_MAX : UINT8_MAX);\n"
    "\t\tif (size == 4) *(uint32_t*)res = (uint32_t)r;\n"
    "\t\telse if (size == 2) *(uint16_t*)res = (uint16_t)r;\n"
    "\t\telse *(uint8_t*)res = (uint8_t)r;\n"
    "\t\treturn r > max;\n"
    "\t}\n"
    "}\n"
    "#define __builtin_add_overflow(a, b, res) __c3_tcc_add_overflow((uint64_t)(a), (uint64_t)(b), (void*)(res), sizeof(*(res)), ((__typeof__(*(res)))-1 < 0))\n"
    "static inline bool __c3_tcc_sub_overflow(uint64_t a, uint64_t b, void *res, int size, bool is_signed) {\n"
    "\tif (size == 8) {\n"
    "\t\tif (is_signed) {\n"
    "\t\t\tint64_t sa = (int64_t)a, sb = (int64_t)b;\n"
    "\t\t\tint64_t r = (int64_t)((uint64_t)sa - (uint64_t)sb);\n"
    "\t\t\t*(int64_t*)res = r;\n"
    "\t\t\treturn (sb > 0 && sa < INT64_MIN + sb) || (sb < 0 && sa > INT64_MAX + sb);\n"
    "\t\t} else {\n"
    "\t\t\t*(uint64_t*)res = a - b;\n"
    "\t\t\treturn a < b;\n"
    "\t\t}\n"
    "\t}\n"
    "\tif (is_signed) {\n"
    "\t\tint64_t sa = (size == 4) ? (int64_t)(int32_t)a : ((size == 2) ? (int64_t)(int16_t)a : (int64_t)(int8_t)a);\n"
    "\t\tint64_t sb = (size == 4) ? (int64_t)(int32_t)b : ((size == 2) ? (int64_t)(int16_t)b : (int64_t)(int8_t)b);\n"
    "\t\tint64_t r = sa - sb;\n"
    "\t\tint64_t min = (size == 4) ? INT32_MIN : ((size == 2) ? INT16_MIN : INT8_MIN);\n"
    "\t\tint64_t max = (size == 4) ? INT32_MAX : ((size == 2) ? INT16_MAX : INT8_MAX);\n"
    "\t\tif (size == 4) *(int32_t*)res = (int32_t)r;\n"
    "\t\telse if (size == 2) *(int16_t*)res = (int16_t)r;\n"
    "\t\telse *(int8_t*)res = (int8_t)r;\n"
    "\t\treturn r < min || r > max;\n"
    "\t} else {\n"
    "\t\tuint64_t r = a - b;\n"
    "\t\treturn a < b;\n"
    "\t}\n"
    "}\n"
    "#define __builtin_sub_overflow(a, b, res) __c3_tcc_sub_overflow((uint64_t)(a), (uint64_t)(b), (void*)(res), sizeof(*(res)), ((__typeof__(*(res)))-1 < 0))\n"
    "static inline bool __c3_tcc_mul_overflow(uint64_t a, uint64_t b, void *res, int size, bool is_signed) {\n"
    "\tif (size == 8) {\n"
    "\t\tif (is_signed) {\n"
    "\t\t\tint64_t sa = (int64_t)a, sb = (int64_t)b;\n"
    "\t\t\tint64_t r = (int64_t)((uint64_t)sa * (uint64_t)sb);\n"
    "\t\t\t*(int64_t*)res = r;\n"
    "\t\t\tif (sa == 0 || sb == 0) return false;\n"
    "\t\t\tif (sa == -1 && sb == INT64_MIN) return true;\n"
    "\t\t\tif (sb == -1 && sa == INT64_MIN) return true;\n"
    "\t\t\treturn (r / sa) != sb;\n"
    "\t\t} else {\n"
    "\t\t\tuint64_t r = a * b;\n"
    "\t\t\t*(uint64_t*)res = r;\n"
    "\t\t\treturn a != 0 && (r / a) != b;\n"
    "\t\t}\n"
    "\t}\n"
    "\tif (is_signed) {\n"
    "\t\tint64_t sa = (size == 4) ? (int64_t)(int32_t)a : ((size == 2) ? (int64_t)(int16_t)a : (int64_t)(int8_t)a);\n"
    "\t\tint64_t sb = (size == 4) ? (int64_t)(int32_t)b : ((size == 2) ? (int64_t)(int16_t)b : (int64_t)(int8_t)b);\n"
    "\t\tint64_t r = sa * sb;\n"
    "\t\tint64_t min = (size == 4) ? INT32_MIN : ((size == 2) ? INT16_MIN : INT8_MIN);\n"
    "\t\tint64_t max = (size == 4) ? INT32_MAX : ((size == 2) ? INT16_MAX : INT8_MAX);\n"
    "\t\tif (size == 4) *(int32_t*)res = (int32_t)r;\n"
    "\t\telse if (size == 2) *(int16_t*)res = (int16_t)r;\n"
    "\t\telse *(int8_t*)res = (int8_t)r;\n"
    "\t\treturn r < min || r > max;\n"
    "\t} else {\n"
    "\t\tuint64_t r = a * b;\n"
    "\t\tuint64_t max = (size == 4) ? UINT32_MAX : ((size == 2) ? UINT16_MAX : UINT8_MAX);\n"
    "\t\tif (size == 4) *(uint32_t*)res = (uint32_t)r;\n"
    "\t\telse if (size == 2) *(uint16_t*)res = (uint16_t)r;\n"
    "\t\telse *(uint8_t*)res = (uint8_t)r;\n"
    "\t\treturn r > max;\n"
    "\t}\n"
    "}\n"
    "#define __builtin_mul_overflow(a, b, res) __c3_tcc_mul_overflow((uint64_t)(a), (uint64_t)(b), (void*)(res), sizeof(*(res)), ((__typeof__(*(res)))-1 < 0))\n"
    "#else\n"
    "#define __c3_abort    __builtin_trap\n"
    "#endif\n\n"
    "#define __C3_SLICE_EQ(a, b) \\\n"
    "\t(((a).len == (b).len) && ((a).len == 0 || (a).ptr == (b).ptr || __c3_memcmp((a).ptr, (b).ptr, (a).len * sizeof(*(a).ptr)) == 0))\n"
    "#define __C3_SLICE_NE(a, b) (!__C3_SLICE_EQ(a, b))\n\n"
    "typedef struct {\n\tconst char *ptr;\n\tsize_t len;\n} c3string_t;\n\n"
    "typedef struct c3type_info__ {\n"
    "\tuint8_t kind;\n\tconst struct c3type_info__ *parentof;\n\tvoid *dtable;\n\tsize_t size;\n\tconst struct c3type_info__ *inner;\n\tsize_t len;\n\tconst char *name;\n\tconst c3string_t *names;\n} c3type_info_t;\n\n"
    "typedef const c3type_info_t* c3typeid_t;\n"
    "typedef void* c3fault_t;\n"
    "extern __c3_thread_local c3fault_t __c3_current_fault;\n"
    "typedef struct { void* ptr; c3typeid_t typeid; } __c3_any__;\n\n"
    "#if !defined(_WIN32) && !defined(_WIN64)\n"
    "extern long syscall(long number, ...);\n"
    "#endif\n\n"
    "#if defined(__TINYC__)\n"
    "typedef uint64_t __c3_uint128;\n"
    "typedef int64_t __c3_int128;\n"
    "#elif defined(__SIZEOF_INT128__) || (defined(__clang__) && !defined(_MSC_VER)) || (defined(__GNUC__) && defined(__x86_64__))\n"
    "typedef unsigned __int128 __c3_uint128;\n"
    "typedef __int128 __c3_int128;\n"
    "#else\n"
    "typedef uint64_t __c3_uint128;\n"
    "typedef int64_t __c3_int128;\n"
    "#endif\n\n"
    "static inline bool __c3_type_matches(c3typeid_t self_type, c3typeid_t target_type) {\n"
    "\tif (!self_type || !target_type) { return false; }\n"
    "\tfor (c3typeid_t t = self_type; t != NULL; t = t->parentof) {\n"
    "\t\tif (t == target_type) { return true; }\n"
    "\t}\n"
    "\treturn false;\n"
    "}\n\n"
    "void __c3_init_runtime(void);\n\n";

static Type **c_collect_runtime_types(HTable *emitted_types)
{
	htable_init(emitted_types, 4096);
	Type **all_types = NULL;

	int type_count = vec_size(compiler.context.type);
	for (int i = 0; i < type_count; i++)
	{
		Type *t = compiler.context.type[i];
		if (!c_type_needs_emission(t))
		{
			continue;
		}
		t = c_unwrap_alias(t);
		if (!c_type_is_resolved(t))
		{
			continue;
		}
		if (t->type_kind == TYPE_OPTIONAL && (!t->optional || t->optional->type_kind == TYPE_VOID || t->optional->type_kind == TYPE_WILDCARD))
		{
			continue;
		}
		const char *tsym = c_typeid_name(t);
		if (!htable_get(emitted_types, (void *)tsym))
		{
			htable_set(emitted_types, (void *)tsym, (void *)1);
			vec_add(all_types, t);
		}
	}
	return all_types;
}

void c_emit_runtime_header(const char *dir)
{
	const char *header_path = file_append_path(dir, "__c3_runtime.h");
	FILE *f                 = fopen(header_path, "wb");
	if (!f)
	{
		error_exit("Failed to open output runtime header '%s'.", header_path);
	}

	fputs(c_runtime_header_boilerplate, f);

	HTable emitted_types;
	Type **all_types = c_collect_runtime_types(&emitted_types);

	fputs("extern const c3type_info_t __c3_typeid_void;\n", f);
	FOREACH(Type *, t, all_types)
	{
		const char *sym = c_typeid_name(t);
		if (strcmp(sym, "__c3_typeid_void") == 0)
		{
			continue;
		}
		fprintf(f, "extern const c3type_info_t %s;\n", sym);
	}
	fputs("\n", f);

	HTable emitted_bs;
	htable_init(&emitted_bs, 256);
	int type_count = vec_size(compiler.context.type);
	for (int i = 0; i < type_count; i++)
	{
		Type *t = compiler.context.type[i];
		if (!t || !is_valid_type_ptr(t) || t->type_kind != TYPE_BITSTRUCT || !t->decl)
		{
			continue;
		}
		Decl *d = t->decl;
		if (d->is_template || d->replacement)
		{
			continue;
		}
		if (!d->strukt.container_type || !is_valid_type_ptr(d->strukt.container_type->type))
		{
			continue;
		}
		const char *bname = c_get_decl_name(d);
		if (htable_get(&emitted_bs, (void *)bname))
		{
			continue;
		}
		htable_set(&emitted_bs, (void *)bname, (void *)1);
		c_emit_bitstruct_accessors_to_file(f, d);
	}
	FOREACH(Module *, mod, compiler.context.module_list)
	{
		if (!mod)
		{
			continue;
		}
		FOREACH(CompilationUnit *, unit, mod->units)
		{
			if (!unit)
			{
				continue;
			}
			FOREACH(Decl *, d, unit->types)
			{
				c_collect_and_emit_bitstructs(f, &emitted_bs, d);
			}
		}
	}

	HTable emitted_faults;
	htable_init(&emitted_faults, 1024);
	FOREACH(Module *, mod, compiler.context.module_list)
	{
		if (!mod)
		{
			continue;
		}
		FOREACH(CompilationUnit *, unit, mod->units)
		{
			if (!unit)
			{
				continue;
			}
			FOREACH(Decl *, d, unit->faults)
			{
				if (!d || d->decl_kind != DECL_FAULT)
				{
					continue;
				}
				const char *sym = c_fault_symbol_name(d);
				if (htable_get(&emitted_faults, (void *)sym))
				{
					continue;
				}
				htable_set(&emitted_faults, (void *)sym, (void *)1);
				fprintf(f, "extern const char %s[];\n", sym);
			}
		}
	}
	fputs("\n#endif /* __C3_RUNTIME_H__ */\n", f);
	fclose(f);
}

GenContext *c_emit_runtime_c(const char *dir)
{
	const char *base_name  = "__c3_runtime";
	const char *c_filename = file_append_path(dir, "__c3_runtime.c");
	FILE *f                = fopen(c_filename, "wb");
	if (!f)
	{
		error_exit("Failed to open output runtime C file '%s'.", c_filename);
	}

	GenContext *c = cmalloc(sizeof(GenContext));
	*c            = (GenContext){
	    .file            = f,
	    .base_name       = base_name,
	    .c_filename      = c_filename,
	    .object_filename = file_append_path(dir, str_printf("%s%s", base_name, get_object_extension())),
	    .current_module  = NULL,
	};

	fputs("/* Generated by C3 Compiler */\n", f);
	fputs("#include \"__c3_runtime.h\"\n\n", f);
	fputs("__c3_thread_local c3fault_t __c3_current_fault = NULL;\n\n", f);
	fputs("#if defined(__TINYC__)\nvoid *__dso_handle = (void *)&__dso_handle;\n#endif\n\n", f);
	fputs("const c3type_info_t __c3_typeid_void = { .kind = 0, .parentof = NULL, .dtable = NULL, .size = 0, .inner = NULL, .len = 0, .name = \"void\", .names = NULL };\n", f);

	HTable emitted_types;
	Type **all_types = c_collect_runtime_types(&emitted_types);

	FOREACH(Type *, t, all_types)
	{
		const char *sym = c_typeid_name(t);
		if (strcmp(sym, "__c3_typeid_void") == 0)
		{
			continue;
		}
		int kind               = c_get_type_introspection_kind(t);
		const char *parent_sym = "NULL";
		if (t->type_kind == TYPE_TYPEDEF && t->decl && t->decl->distinct && is_valid_type_ptr(t->decl->distinct->type))
		{
			parent_sym = str_printf("&%s", c_typeid_name(t->decl->distinct->type));
		}
		else if (t->type_kind == TYPE_CONSTDEF && t->decl && t->decl->is_substruct && t->decl->enums.type_info && is_valid_type_ptr(t->decl->enums.type_info->type))
		{
			parent_sym = str_printf("&%s", c_typeid_name(t->decl->enums.type_info->type));
		}
		else if (t->type_kind == TYPE_STRUCT && t->decl && t->decl->is_substruct && t->decl->strukt.members && vec_size(t->decl->strukt.members) > 0)
		{
			Decl *first_m = t->decl->strukt.members[0];
			if (first_m && is_valid_type_ptr(first_m->type))
			{
				parent_sym = str_printf("&%s", c_typeid_name(first_m->type));
			}
		}

		size_t sz             = c_get_type_size(t);
		const char *inner_sym = "NULL";
		if (t->type_kind == TYPE_POINTER && t->pointer && is_valid_type_ptr(t->pointer))
		{
			inner_sym = str_printf("&%s", c_typeid_name(t->pointer));
		}
		else if ((t->type_kind == TYPE_SLICE || c_type_is_vec_or_arr(t)) && t->array.base && is_valid_type_ptr(t->array.base))
		{
			inner_sym = str_printf("&%s", c_typeid_name(t->array.base));
		}
		else if ((t->type_kind == TYPE_ENUM || t->type_kind == TYPE_CONSTDEF) && t->decl)
		{
			Type *it = enum_inner_type(t);
			if (it && is_valid_type_ptr(it))
			{
				inner_sym = str_printf("&%s", c_typeid_name(it));
			}
		}
		else if (t->type_kind == TYPE_TYPEDEF && t->decl && t->decl->distinct && is_valid_type_ptr(t->decl->distinct->type))
		{
			inner_sym = str_printf("&%s", c_typeid_name(t->decl->distinct->type));
		}
		else if (t->type_kind == TYPE_BITSTRUCT && t->decl && t->decl->strukt.container_type && is_valid_type_ptr(t->decl->strukt.container_type->type))
		{
			inner_sym = str_printf("&%s", c_typeid_name(t->decl->strukt.container_type->type));
		}
		else if (t->type_kind == TYPE_OPTIONAL && t->optional && is_valid_type_ptr(t->optional))
		{
			inner_sym = str_printf("&%s", c_typeid_name(t->optional));
		}

		size_t len = 0;
		if (c_type_is_vec_or_arr(t))
		{
			len = (size_t)t->array.len;
		}
		else if ((t->type_kind == TYPE_ENUM || t->type_kind == TYPE_CONSTDEF) && t->decl && t->decl->enums.values)
		{
			len = (size_t)vec_size(t->decl->enums.values);
		}

		const char *name_str = t->name ? t->name : "anon";
		int val_count        = (t->type_kind == TYPE_ENUM && t->decl && t->decl->enums.values && !t->decl->obfuscate)
		                           ? vec_size(t->decl->enums.values)
		                           : 0;

		if (val_count > 0)
		{
			fprintf(f, "const c3type_info_t %s = { .kind = %d, .parentof = (const struct c3type_info__ *)%s, .dtable = NULL, .size = %zu, .inner = (const struct c3type_info__ *)%s, .len = %zu, .name = \"%s\", .names = (const c3string_t[]){ ",
			        sym, kind, parent_sym, sz, inner_sym, len, name_str);
			FOREACH_IDX(i, Decl *, ev, t->decl->enums.values)
			{
				if (i > 0)
				{
					fputs(", ", f);
				}
				const char *en_name = (ev && ev->name) ? ev->name : "";
				fprintf(f, "{ \"%s\", %zu }", en_name, strlen(en_name));
			}
			fputs(" } };\n", f);
		}
		else
		{
			fprintf(f, "const c3type_info_t %s = { .kind = %d, .parentof = (const struct c3type_info__ *)%s, .dtable = NULL, .size = %zu, .inner = (const struct c3type_info__ *)%s, .len = %zu, .name = \"%s\", .names = NULL };\n",
			        sym, kind, parent_sym, sz, inner_sym, len, name_str);
		}
	}

	HTable emitted_fault_defs;
	htable_init(&emitted_fault_defs, 1024);
	FOREACH(Module *, mod, compiler.context.module_list)
	{
		if (!mod)
		{
			continue;
		}
		FOREACH(CompilationUnit *, unit, mod->units)
		{
			if (!unit)
			{
				continue;
			}
			FOREACH(Decl *, d, unit->faults)
			{
				if (!d || d->decl_kind != DECL_FAULT)
				{
					continue;
				}
				const char *sym = c_fault_symbol_name(d);
				if (htable_get(&emitted_fault_defs, (void *)sym))
				{
					continue;
				}
				htable_set(&emitted_fault_defs, (void *)sym, (void *)1);

				const char *module_name = (d->unit && d->unit->module && d->unit->module->name && d->unit->module->name->module)
				                              ? d->unit->module->name->module
				                              : "";
				size_t last             = 0;
				for (size_t i = 0;; i++)
				{
					if (module_name[i] == 0)
					{
						break;
					}
					if (module_name[i] == ':')
					{
						i++;
						last = i + 1;
					}
				}
				const char *new_name = (strlen(&module_name[last]) > 0)
				                           ? str_printf("%s::%s", &module_name[last], d->name ? d->name : "fault")
				                           : (d->name ? d->name : "fault");
				fprintf(f, "const char %s[] = \"%s\";\n", sym, new_name);
			}
		}
	}
	fputs("\n", f);
	fclose(f);
	return c;
}