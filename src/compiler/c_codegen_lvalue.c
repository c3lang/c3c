#include "c_codegen_internal.h"

static inline Type *c_flatten_deref(Type *type)
{
	Type *flat = type_flatten(type);
	if (flat && flat->type_kind == TYPE_POINTER)
	{
		flat = type_flatten(flat->pointer);
	}
	return flat;
}

Decl *get_struct_decl_from_type(Type *type)
{
	Decl *d = c_type_get_decl(c_flatten_deref(type));
	if (d && (d->decl_kind == DECL_STRUCT || d->decl_kind == DECL_UNION || d->decl_kind == DECL_BITSTRUCT))
	{
		return d;
	}
	return NULL;
}

// The member indices from `parent` down to `member` through anonymous members, as llvm_emit_member_addr.
// Returns the depth, 0 when `member` is not a field of `parent`.
int c_member_path(Decl *parent, Decl *member, int indices[MAX_MEMBER_PATH])
{
	int depth = 0;
	while (true)
	{
		ArrayIndex index = find_member_index(parent, member);
		if (index < 0) return 0;
		ASSERT(depth < MAX_MEMBER_PATH);
		indices[depth++] = (int)index;
		Decl *found      = parent->strukt.members[index];
		if (found == member) return depth;
		parent = found;
	}
}

// The C member path `.a_0.b_1` of the `indices` from c_member_path below `parent`.
const char *c_member_path_str(Decl *parent, const int *indices, int depth)
{
	const char *path = "";
	for (int i = 0; i < depth; i++)
	{
		path   = str_printf("%s.%s", path, c_member_name(parent, indices[i]));
		parent = parent->strukt.members[indices[i]];
	}
	return path;
}

// The low `bits` bits set, as a __c3_uint128 expression.
static const char *c_bit_mask128_str(int bits)
{
	return str_printf("__c3_u128_lshr(__c3_u128_not(__c3_u128_from_u64(0)), %d)", 128 - bits);
}

// Sign extends the `bits` wide field in the __c3_uint128 variable `var`.
static void c_emit_sign_extend128(GenContext *c, const char *var, int bits)
{
	PRINTF("\tif (__c3_u128_nonzero(__c3_u128_and(%s, __c3_u128_shl(__c3_u128_from_u64(1), %d)))) %s = __c3_u128_or(%s, __c3_u128_not(%s));\n",
	       var, bits - 1, var, var, c_bit_mask128_str(bits));
}

static void c_emit_bitstruct_accessors(GenContext *c, Decl *decl)
{
	if (!decl || decl->decl_kind != DECL_BITSTRUCT || !decl->strukt.members)
	{
		return;
	}
	if (!decl->strukt.container_type)
	{
		return;
	}

	Type *container_type = decl->strukt.container_type->type;
	Type *flat_c         = type_flatten(container_type);
	if (!flat_c)
	{
		return;
	}

	bool is_array       = (flat_c->type_kind == TYPE_ARRAY);
	bool reverse        = bitstruct_is_big_endian(decl);
	const char *bs_name = c_get_decl_name(decl);

	FOREACH(Decl *, m, decl->strukt.members)
	{
		if (!m || m->decl_kind != DECL_VAR)
		{
			continue;
		}
		Type *m_type          = m->type ? c_safe_type_lower(m->type) : type_uint;
		const char *ret_tname = c_type_name(NULL, m_type);
		const char *m_name    = m->name ? m->name : "_anon";
		int start_bit = (int)m->var.start_bit;
		int end_bit   = (int)m->var.end_bit;
		int bit_size = end_bit - start_bit + 1;
		if (bit_size <= 0)
		{
			bit_size = 1;
		}
		bool is_signed = type_is_signed(m_type);
		// A container of 2, 4 or 8 bytes is swapped with __builtin_bswapN.
		int swap_bits  = bitstruct_requires_byteswap(decl) && !is_array && type_size(container_type) > 1 ? (int)type_size(container_type) * 8 : 0;
		// Fields wider than 64 bits are computed as __c3_uint128, others as uint64_t.
		bool is_wide       = bit_size > 64;
		Type *res_type     = is_wide ? type_u128 : (is_signed ? type_long : type_ulong);
		const char *result = c_scalar_convert_str(c, m_type, res_type, "res");

		PRINTF("static inline %s %s_get_%s(const void *p) {\n", ret_tname, bs_name, m_name);
		if (is_array)
		{
			PRINT("\tconst uint8_t *b = (const uint8_t *)p;\n");
			char expr_buf[2048];
			expr_buf[0]    = '\0';
			int term_count = 0;
			for (int bits_done = 0; bits_done < bit_size; )
			{
				CBitfieldStep s = c_bitfield_step(start_bit, bit_size, bits_done, reverse);
				bits_done += s.step_bits;
				if (is_wide)
				{
					const char *byte = str_printf("b[%d]", s.byte_idx);
					if (s.bit_in_byte > 0) byte = str_printf("(%s >> %d)", byte, s.bit_in_byte);
					if (s.step_bits != 8) byte = str_printf("(%s & 0x%X)", byte, s.step_mask);
					const char *term = str_printf("__c3_u128_shl(__c3_u128_from_u64(%s), %d)", byte, s.shift);
					if (term_count++)
					{
						PRINTF("\tres = __c3_u128_or(res, %s);\n", term);
					}
					else
					{
						PRINTF("\t__c3_uint128 res = %s;\n", term);
					}
					continue;
				}
				char term[128];
				if (s.bit_in_byte == 0 && s.step_bits == 8)
				{
					if (s.shift > 0)
					{
						snprintf(term, sizeof(term), "((uint64_t)b[%d] << %d)", s.byte_idx, s.shift);
					}
					else
					{
						snprintf(term, sizeof(term), "(uint64_t)b[%d]", s.byte_idx);
					}
				}
				else
				{
					if (s.bit_in_byte > 0 && s.shift > 0)
					{
						snprintf(term, sizeof(term), "(((uint64_t)(b[%d] >> %d & 0x%X) << %d))", s.byte_idx, s.bit_in_byte, s.step_mask, s.shift);
					}
					else if (s.bit_in_byte > 0)
					{
						snprintf(term, sizeof(term), "((uint64_t)(b[%d] >> %d & 0x%X))", s.byte_idx, s.bit_in_byte, s.step_mask);
					}
					else if (s.shift > 0)
					{
						snprintf(term, sizeof(term), "(((uint64_t)(b[%d] & 0x%X) << %d))", s.byte_idx, s.step_mask, s.shift);
					}
					else
					{
						snprintf(term, sizeof(term), "((uint64_t)(b[%d] & 0x%X))", s.byte_idx, s.step_mask);
					}
				}
				if (term_count++)
				{
					strncat(expr_buf, " | ", sizeof(expr_buf) - strlen(expr_buf) - 1);
				}
				strncat(expr_buf, term, sizeof(expr_buf) - strlen(expr_buf) - 1);
			}
			if (is_wide)
			{
				if (is_signed && bit_size < 128) c_emit_sign_extend128(c, "res", bit_size);
			}
			else
			{
				PRINTF("\tuint64_t res = %s;\n", expr_buf);
				if (is_signed && bit_size < 64) PRINTF("\tif (res & ((uint64_t)1 << %d)) res |= ~(((uint64_t)1 << %d) - 1);\n", bit_size - 1, bit_size);
			}
			PRINTF("\treturn %s;\n", result);
		}
		else if (type_size(container_type) > 8)
		{
			PRINT("\t__c3_uint128 res;\n\t__c3_memcpy(&res, p, sizeof(res));\n");
			if (bitstruct_requires_byteswap(decl)) PRINT("\tres = __c3_u128_bswap(res);\n");
			if (bit_size < 128)
			{
				PRINTF("\tres = __c3_u128_and(__c3_u128_lshr(res, %d), %s);\n", start_bit, c_bit_mask128_str(bit_size));
				if (is_signed) c_emit_sign_extend128(c, "res", bit_size);
			}
			PRINTF("\treturn %s;\n", c_scalar_convert_str(c, m_type, type_u128, "res"));
		}
		else
		{
			const char *c_tname = c_type_name(NULL, container_type);
			uint64_t mask       = (bit_size >= 64) ? ~0ULL : (((uint64_t)1 << bit_size) - 1);
			const char *v       = swap_bits ? str_printf("__builtin_bswap%d((uint%d_t)c)", swap_bits, swap_bits) : "c";
			PRINTF("\t%s c;\n\t__c3_memcpy(&c, p, sizeof(c));\n", c_tname);
			PRINTF("\tuint64_t res = ((uint64_t)%s >> %d) & 0x%" PRIx64 "ULL;\n", v, start_bit, mask);
			if (is_signed && bit_size < 64)
			{
				PRINTF("\tif (res & (1ULL << %d)) res |= ~0x%" PRIx64 "ULL;\n", bit_size - 1, mask);
			}
			PRINTF("\treturn %s;\n", result);
		}
		PRINT("}\n");

		PRINTF("static inline void %s_set_%s(void *p, %s val) {\n", bs_name, m_name, ret_tname);
		bool is_wide_container = !is_array && type_size(container_type) > 8;
		const char *val        = "val";
		if (c_type_is_int128(m_type) || is_wide || is_wide_container)
		{
			// Below, `_val` is a uint64_t or a __c3_uint128.
			Type *val_type = (is_wide || is_wide_container) ? type_u128 : type_ulong;
			PRINTF("\t%s _val = %s;\n", c_type_name(c, val_type), c_scalar_convert_str(c, val_type, m_type, "val"));
			val = "_val";
		}
		if (is_array)
		{
			PRINT("\tuint8_t *b = (uint8_t *)p;\n");
			for (int bits_done = 0; bits_done < bit_size; )
			{
				CBitfieldStep s = c_bitfield_step(start_bit, bit_size, bits_done, reverse);
				const char *byte = is_wide ? str_printf("(uint8_t)__c3_u128_lo(__c3_u128_lshr(%s, %d))", val, s.shift)
				                 : s.shift > 0 ? str_printf("(uint8_t)((uint64_t)%s >> %d)", val, s.shift)
				                               : str_printf("(uint8_t)%s", val);
				if (s.bit_in_byte == 0 && s.step_bits == 8)
				{
					PRINTF("\tb[%d] = %s;\n", s.byte_idx, byte);
				}
				else
				{
					PRINTF("\tb[%d] = (b[%d] & ~0x%02X) | ((%s & 0x%02X) << %d);\n",
					        s.byte_idx, s.byte_idx, s.byte_mask, byte, s.step_mask, s.bit_in_byte);
				}
				bits_done += s.step_bits;
			}
		}
		else if (type_size(container_type) > 8)
		{
			bool swap = bitstruct_requires_byteswap(decl);
			PRINT("\t__c3_uint128 _cur;\n");
			if (bit_size >= 128)
			{
				PRINT("\t_cur = _val;\n");
			}
			else
			{
				const char *field_mask = c_bit_mask128_str(bit_size);
				PRINT("\t__c3_memcpy(&_cur, p, sizeof(_cur));\n");
				if (swap) PRINT("\t_cur = __c3_u128_bswap(_cur);\n");
				PRINTF("\t__c3_uint128 mask = __c3_u128_shl(%s, %d);\n", field_mask, start_bit);
				PRINTF("\t_cur = __c3_u128_or(__c3_u128_and(_cur, __c3_u128_not(mask)), __c3_u128_shl(__c3_u128_and(_val, %s), %d));\n", field_mask, start_bit);
			}
			if (swap) PRINT("\t_cur = __c3_u128_bswap(_cur);\n");
			PRINT("\t__c3_memcpy(p, &_cur, sizeof(_cur));\n");
		}
		else
		{
			const char *c_tname   = c_type_name(NULL, container_type);
			uint64_t mask         = (bit_size >= 64) ? ~0ULL : (((uint64_t)1 << bit_size) - 1);
			uint64_t shifted_mask = (bit_size >= 64) ? ~0ULL : (mask << start_bit);
			const char *swap      = swap_bits ? str_printf("__builtin_bswap%d((uint%d_t)", swap_bits, swap_bits) : "(";
			PRINTF("\t%s v;\n\t__c3_memcpy(&v, p, sizeof(v));\n", c_tname);
			PRINTF("\tuint64_t _cur = %sv);\n", swap);
			PRINTF("\t_cur = (_cur & ~0x%" PRIx64 "ULL) | (((uint64_t)(%s)%s & 0x%" PRIx64 "ULL) << %d);\n", shifted_mask, c_tname, val, mask, start_bit);
			PRINTF("\tv = (%s)%s_cur);\n\t__c3_memcpy(p, &v, sizeof(v));\n", c_tname, swap);
		}
		PRINT("}\n\n");
	}
}

void c_declare_bitstruct_accessors(GenContext *c, Decl *decl)
{
	if (htable_get(&c->declared_bitstructs, decl)) return;
	ASSERT(c->decl_buffer);
	htable_set(&c->declared_bitstructs, decl, decl);
	ByteBuffer *prev = c_decl_stream_enter(c, c->decl_buffer);
	c_emit_bitstruct_accessors(c, decl);
	c->buffer = prev;
}

const char *c_emit_bitstruct_container(GenContext *c, Expr *parent_expr, Decl *member, int *out_container_var, Decl **out_bitstruct_decl)
{
	Type *parent_type  = c_expr_type(parent_expr);
	CValue parent_addr = {0};
	c_emit_lvalue_addr(c, parent_expr, &parent_addr, type_get_ptr(parent_type));

	Decl *parent_decl = get_struct_decl_from_type(parent_expr ? parent_expr->type : parent_type);
	int indices[MAX_MEMBER_PATH];
	int depth = parent_decl ? c_member_path(parent_decl, member, indices) : 0;

	Decl *container_decl = NULL;
	int temp_ptr         = c_create_variable(c);

	if (depth > 1 && parent_decl && parent_decl->strukt.members)
	{
		Decl *curr = parent_decl;
		for (int i = 0; i < depth - 1; i++)
		{
			if (!curr || !decl_has_members(curr) || !curr->strukt.members || indices[i] >= vec_size(curr->strukt.members))
			{
				break;
			}
			Decl *m = curr->strukt.members[indices[i]];
			curr    = (m->decl_kind == DECL_STRUCT || m->decl_kind == DECL_UNION || m->decl_kind == DECL_BITSTRUCT)
			              ? m
			              : (m->type ? type_flatten(m->type)->decl : NULL);
		}
		container_decl = curr;

		Type *c_type = type_uint;
		if (container_decl)
		{
			if (container_decl->decl_kind == DECL_BITSTRUCT && container_decl->strukt.container_type)
			{
				c_type = c_safe_type_lower(container_decl->strukt.container_type->type);
			}
			else
			{
				Type *ft = type_flatten(container_decl->type);
				if (ft->type_kind == TYPE_BITSTRUCT && ft->decl && ft->decl->strukt.container_type)
				{
					c_type = c_safe_type_lower(ft->decl->strukt.container_type->type);
				}
				else
				{
					c_type = c_safe_type_lower(container_decl->type);
				}
			}
		}
		const char *c_tname = c_type_name(c, c_type);
		PRINTF("%s* ___var_%d = (%s*)&((*((%s*)___var_%d))",
		       c_tname, temp_ptr, c_tname, c_type_name(c, parent_type), parent_addr.var);
		PRINTF("%s);\n", c_member_path_str(parent_decl, indices, depth - 1));
		*out_container_var = temp_ptr;
		if (out_bitstruct_decl)
		{
			*out_bitstruct_decl = container_decl;
		}
		return c_tname;
	}

	Type *flat = c_flatten_deref(parent_type);
	container_decl = (flat && flat->type_kind == TYPE_BITSTRUCT) ? flat->decl : parent_decl;

	Type *c_type = type_uint;
	if (container_decl)
	{
		if (container_decl->decl_kind == DECL_BITSTRUCT && container_decl->strukt.container_type)
		{
			c_type = c_safe_type_lower(container_decl->strukt.container_type->type);
		}
		else
		{
			Type *ft = type_flatten(container_decl->type);
			if (ft->type_kind == TYPE_BITSTRUCT && ft->decl && ft->decl->strukt.container_type)
			{
				c_type = c_safe_type_lower(ft->decl->strukt.container_type->type);
			}
			else
			{
				c_type = c_safe_type_lower(container_decl->type);
			}
		}
	}
	const char *c_tname = c_type_name(c, c_type);
	PRINTF("%s* ___var_%d = (%s*)___var_%d;\n", c_tname, temp_ptr, c_tname, parent_addr.var);
	*out_container_var = temp_ptr;
	if (out_bitstruct_decl)
	{
		*out_bitstruct_decl = container_decl;
	}
	return c_tname;
}

// The address of `member` in the struct at `ptr_var`, known to be `parent_align` aligned (0: its type's alignment).
// Returns the alignment of the member's address, 0 when it has its type's.
static AlignSize c_emit_struct_member_addr(GenContext *c, const char *tname, int temp, Type *struct_type, int ptr_var, Decl *member, AlignSize parent_align)
{
	Decl *parent_decl = get_struct_decl_from_type(struct_type);
	int indices[MAX_MEMBER_PATH];
	int depth = parent_decl ? c_member_path(parent_decl, member, indices) : 0;
	if (depth == 0 || !parent_decl)
	{
		if (member && member->decl_kind == DECL_FUNC)
		{
			PRINTF("%s ___var_%d = (%s)&%s;\n", tname, temp, tname, c_decl_ref(c, member));
			return 0;
		}
		PRINTF("%s ___var_%d = (%s)___var_%d;\n", tname, temp, tname, ptr_var);
		return member && parent_align < c_type_alignment(member->type) ? parent_align : 0;
	}
	AlignSize offset = decl_find_member_offset(parent_decl, member);
	if (parent_align || c_struct_is_packed(parent_decl))
	{
		// Through a misaligned struct pointer even `&p->m` is undefined, and a packed struct may keep members as bytes.
		PRINTF("%s ___var_%d = (%s)((uint8_t*)___var_%d + %u);\n", tname, temp, tname, ptr_var, offset);
	}
	else
	{
		PRINTF("%s ___var_%d = (%s)&((*((%s*)___var_%d))", tname, temp, tname, c_type_name(c, struct_type), ptr_var);
		PRINTF("%s);\n", c_member_path_str(parent_decl, indices, depth));
	}
	AlignSize align = type_min_alignment(offset, parent_align ? parent_align : type_abi_alignment(struct_type));
	return align < c_type_alignment(member->type) ? align : 0;
}

static void c_emit_enum_assoc_access(GenContext *c, Decl *enum_decl, Decl *member, Expr *parent_expr, bool parent_is_ptr, int temp, const char *tname, Type *target_type, bool is_addr)
{
	c_emit_enum_assoc_decl(c, enum_decl, member);
	const char *arr_name = c_get_enum_assoc_name(enum_decl, member);
	CValue parent_val    = {0};
	c_emit_expr(c, &parent_val, parent_expr);
	c_value_rvalue(c, &parent_val);
	c_ensure_cvalue_var(c, &parent_val, type_sz);
	const char *idx_str = parent_is_ptr
	                          ? str_printf("(size_t)(*___var_%d)", parent_val.var)
	                          : str_printf("(size_t)___var_%d", parent_val.var);
	if (is_addr)
	{
		PRINTF("%s ___var_%d = (%s)&%s[%s];\n", tname, temp, tname, arr_name, idx_str);
		return;
	}
	if (c_type_is_aggregate(target_type))
	{
		PRINTF("%s ___var_%d;\n", tname, temp);
		PRINTF("__c3_memcpy(&___var_%d, &%s[%s], sizeof(%s));\n", temp, arr_name, idx_str, tname);
		return;
	}
	PRINTF("%s ___var_%d = (%s)%s[%s];\n", tname, temp, tname, arr_name, idx_str);
}

// Panic unless 0 <= index < len, where `len` is a C expression.
static void c_emit_index_check(GenContext *c, CValue *index, const char *len, SourceLocId loc)
{
	Type *index_type      = c_safe_type_lower(index->type);
	const char *index_str = str_printf("___var_%d", index->var);
	CPanicValue index_value = { index_str, index_type };
	CPanicValue len_value   = { len, index_type };
	if (type_is_signed(index_type))
	{
		c_emit_panic_if(c, str_printf("%s < 0", index_str), PANIC_FN_INDEX_NEGATIVE, loc, NULL, &index_value, NULL);
	}
	c_emit_panic_if(c, str_printf("(uint64_t)%s >= (uint64_t)(%s)", index_str, len), PANIC_FN_INDEX_BOUNDS, loc, NULL, &len_value, &index_value);
}

// Panic when dereferencing a null or misaligned pointer.
static void c_emit_deref_check(GenContext *c, Expr *deref, VariableId ptr_var)
{
	Expr *inner = deref->unary_expr.expr;
	if (!safe_mode_enabled() || expr_is_const(inner)) return;
	// The address of a variable or an element is known to be valid.
	if (inner->expr_kind == EXPR_SUBSCRIPT_ADDR) return;
	if (inner->expr_kind == EXPR_UNARY && (inner->unary_expr.operator == UNARYOP_ADDR || inner->unary_expr.operator == UNARYOP_TADDR)) return;
	scratch_buffer_clear();
	loc_to_scratch(inner->loc);
	const char *text = scratch_buffer_copy();
	c_emit_panic_if(c, str_printf("___var_%d == NULL", ptr_var), PANIC_FN_NULL_POINTER, inner->loc, text, NULL, NULL);
	AlignSize alignment = type_abi_alignment(deref->type);
	if (alignment > 1)
	{
		const char *rem = str_printf("((uintptr_t)___var_%d & %uU)", ptr_var, (unsigned)(alignment - 1));
		CPanicValue align_value = { str_printf("%u", (unsigned)alignment), type_sz };
		CPanicValue rem_value   = { rem, type_sz };
		c_emit_panic_if(c, str_printf("%s != 0", rem), PANIC_FN_UNALIGNED, inner->loc, NULL, &align_value, &rem_value);
	}
}

static void c_emit_lvalue_addr_of(GenContext *c, Expr *expr, CValue *out_val, Type *target_type);

void c_emit_lvalue_addr(GenContext *c, Expr *expr, CValue *out_val, Type *target_type)
{
	c_emit_lvalue_addr_of(c, expr, out_val, target_type);
	// A C3 type less aligned than its C form, like a bitstruct with `@align(1)`, is misaligned for C anywhere.
	TypeKind kind = expr ? type_flatten(expr->type)->type_kind : TYPE_VOID;
	if (!out_val->align && (kind == TYPE_BITSTRUCT || kind == TYPE_ARRAY || kind == TYPE_VECTOR))
	{
		AlignSize align = type_abi_alignment(expr->type);
		if (align < c_type_alignment(expr->type)) out_val->align = align;
	}
}

static void c_emit_lvalue_addr_of(GenContext *c, Expr *expr, CValue *out_val, Type *target_type)
{
	target_type = c_safe_type_lower(target_type);
	if (target_type->type_kind == TYPE_VOID && expr)
	{
		target_type = c_expr_type(expr);
		if (target_type->type_kind != TYPE_VOID)
		{
			target_type = type_get_ptr(target_type);
		}
	}
	ASSERT(target_type->type_kind != TYPE_VOID);
	ASSERT(c_type_is_c_pointer(target_type));
	int temp          = c_emit_temp_var(c, out_val, target_type);
	const char *tname = c_type_name(c, target_type);

	if (!expr)
	{
		PRINTF("%s ___var_%d = NULL;\n", tname, temp);
		return;
	}
	if (expr->expr_kind == EXPR_RECAST || expr->expr_kind == EXPR_ADDR_CONVERSION || expr->expr_kind == EXPR_RVALUE)
	{
		Type *inner_t   = c_expr_type(expr->inner_expr);
		Type *pointee_t = (target_type->type_kind == TYPE_POINTER && target_type->pointer) ? c_safe_type_lower(target_type->pointer) : target_type;
		if (type_size(inner_t) == type_size(pointee_t))
		{
			c_emit_lvalue_addr(c, expr->inner_expr, out_val, target_type);
			return;
		}
	}
	if (expr->expr_kind == EXPR_UNARY && expr->unary_expr.operator == UNARYOP_DEREF)
	{
		CValue ptr_val = {0};
		c_emit_expr(c, &ptr_val, expr->unary_expr.expr);
		c_value_rvalue(c, &ptr_val);
		if (ptr_val.var != 0)
		{
			c_emit_deref_check(c, expr, ptr_val.var);
			PRINTF("%s ___var_%d = (%s)___var_%d;\n", tname, temp, tname, ptr_val.var);
		}
		else
		{
			PRINTF("%s ___var_%d = NULL;\n", tname, temp);
		}
		return;
	}
	if (expr->expr_kind == EXPR_IDENTIFIER || expr->expr_kind == EXPR_DECL)
	{
		Decl *d = (expr->expr_kind == EXPR_DECL) ? expr->decl_expr : c_decl_unwrap(expr->ident_expr);
		if (d)
		{
			if (d->decl_kind == DECL_VAR && d->var.kind == VARDECL_PARAM_EXPR && d->var.init_expr)
			{
				c_emit_lvalue_addr(c, d->var.init_expr, out_val, target_type);
				return;
			}
			if (d->decl_kind == DECL_FUNC)
			{
				PRINTF("%s ___var_%d = (%s)&%s;\n", tname, temp, tname, c_decl_ref(c, d));
				return;
			}
			// An optional variable is only assigned directly, so taking its address reads it.
			const char *fault = IS_OPTIONAL(expr) ? c_ident_read_fault_ref(c, d) : NULL;
			if (fault) c_emit_jump_to_optional_exit(c, fault);
			PRINTF("%s ___var_%d = (%s)&%s;\n", tname, temp, tname, c_decl_lvalue_str(c, d));
			return;
		}
	}
	if (expr->expr_kind == EXPR_ACCESS_RESOLVED && expr->access_resolved_expr.parent)
	{
		Type *raw_parent_type = type_flatten(expr->access_resolved_expr.parent->type);
		bool parent_is_ptr    = false;
		if (raw_parent_type && raw_parent_type->type_kind == TYPE_POINTER && raw_parent_type->pointer)
		{
			parent_is_ptr   = true;
			raw_parent_type = type_flatten(raw_parent_type->pointer);
		}
		if (raw_parent_type && raw_parent_type->type_kind == TYPE_ENUM && raw_parent_type->decl)
		{
			Decl *enum_decl = raw_parent_type->decl;
			Decl *member    = c_decl_unwrap(expr->access_resolved_expr.ref);
			if (member && member->decl_kind == DECL_VAR)
			{
				c_emit_enum_assoc_access(c, enum_decl, member, expr->access_resolved_expr.parent, parent_is_ptr, temp, tname, target_type, true);
				return;
			}
		}
		Type *parent_type = c_expr_type(expr->access_resolved_expr.parent);
		Decl *member      = c_decl_unwrap(expr->access_resolved_expr.ref);
		bool is_ptr       = (parent_type->type_kind == TYPE_POINTER);
		if (is_ptr)
		{
			CValue parent_val = {0};
			c_emit_expr(c, &parent_val, expr->access_resolved_expr.parent);
			c_value_rvalue(c, &parent_val);
			c_ensure_cvalue_var(c, &parent_val, parent_type);

			Type *base_struct_type = c_safe_type_lower(parent_type->pointer);
			if (base_struct_type->type_kind == TYPE_VOID)
			{
				base_struct_type = parent_type;
			}
			if (c_type_is_any(base_struct_type))
			{
				if (member && member->decl_kind == DECL_FUNC)
				{
					c_emit_dynamic_dispatcher(c, member);
					const char *disp_resolver = c_intern(str_printf("__c3_dyn_resolve_%s", c_get_decl_name(member)));
					PRINTF("%s ___var_%d = (%s)%s((c3typeid_t)(((__c3_any__*)___var_%d)->typeid));\n", tname, temp, tname, disp_resolver, parent_val.var);
					return;
				}
				const char *fname = (member && member->name && (strcmp(member->name, "typeid") == 0 || strcmp(member->name, "type") == 0)) ? "typeid" : "ptr";
				PRINTF("%s ___var_%d = (%s)&(((__c3_any__*)___var_%d)->%s);\n", tname, temp, tname, parent_val.var, fname);
				return;
			}
			out_val->align = c_emit_struct_member_addr(c, tname, temp, base_struct_type, parent_val.var, member, 0);
			return;
		}
		if (parent_type->type_kind == TYPE_SLICE)
		{
			CValue parent_val = {0};
			c_emit_expr(c, &parent_val, expr->access_resolved_expr.parent);
			c_value_rvalue(c, &parent_val);
			c_ensure_cvalue_var(c, &parent_val, parent_type);

			if (member && member->decl_kind == DECL_FUNC)
			{
				PRINTF("%s ___var_%d = (%s)&%s;\n", tname, temp, tname, c_decl_ref(c, member));
				return;
			}
			const char *fname = (member && member->name && strcmp(member->name, "len") == 0) ? "len" : "ptr";
			PRINTF("%s ___var_%d = (%s)&(___var_%d.%s);\n", tname, temp, tname, parent_val.var, fname);
			return;
		}
		if (c_type_is_any(parent_type))
		{
			CValue parent_val = {0};
			c_emit_expr(c, &parent_val, expr->access_resolved_expr.parent);
			c_value_rvalue(c, &parent_val);
			c_ensure_cvalue_var(c, &parent_val, parent_type);

			if (member && member->decl_kind == DECL_FUNC)
			{
				c_emit_dynamic_dispatcher(c, member);
				const char *disp_resolver = c_intern(str_printf("__c3_dyn_resolve_%s", c_get_decl_name(member)));
				PRINTF("%s ___var_%d = (%s)%s((c3typeid_t)___var_%d.typeid);\n", tname, temp, tname, disp_resolver, parent_val.var);
				return;
			}
			const char *fname = (member && member->name && (strcmp(member->name, "typeid") == 0 || strcmp(member->name, "type") == 0)) ? "typeid" : "ptr";
			PRINTF("%s ___var_%d = (%s)&(___var_%d.%s);\n", tname, temp, tname, parent_val.var, fname);
			return;
		}
		CValue parent_addr = {0};
		Expr *parent       = expr->access_resolved_expr.parent;
		if (expr_is_deref(parent))
		{
			// `p.member` only offsets the pointer, nothing is loaded through `p` here, so there is no deref check.
			c_emit_expr(c, &parent_addr, parent->unary_expr.expr);
			c_value_rvalue(c, &parent_addr);
		}
		else
		{
			c_emit_lvalue_addr(c, parent, &parent_addr, type_get_ptr(parent_type));
		}
		c_ensure_cvalue_var(c, &parent_addr, type_get_ptr(parent_type));
		out_val->align = c_emit_struct_member_addr(c, tname, temp, parent_type, parent_addr.var, member, parent_addr.align);
		return;
	}
	if (expr->expr_kind == EXPR_SUBSCRIPT || expr->expr_kind == EXPR_SUBSCRIPT_ADDR)
	{
		Expr *parent_expr = exprptr(expr->subscript_expr.expr);
		Expr *index_expr  = exprptr(expr->subscript_expr.index.expr);
		Type *parent_type = c_expr_type(parent_expr);
		bool is_safe      = !expr->subscript_expr.no_check && safe_mode_enabled();

		// Evaluation order is left to right: the parent, then the index.
		// `base` is the C expression of the first element, `len` the element count where known.
		const char *base;
		const char *len          = NULL;
		AlignSize parent_align   = 0;
		int parent_addr_var      = 0;
		if (parent_type->type_kind == TYPE_POINTER)
		{
			CValue parent_val = {0};
			c_emit_expr(c, &parent_val, parent_expr);
			c_value_rvalue(c, &parent_val);
			Type *pointee = c_safe_type_lower(parent_type->pointer);
			base = pointee->type_kind == TYPE_VOID ? str_printf("((char*)___var_%d)", parent_val.var)
			                                       : str_printf("((%s*)___var_%d)", c_type_name(c, pointee), parent_val.var);
		}
		else if (parent_type->type_kind == TYPE_SLICE)
		{
			CValue parent_val = {0};
			c_emit_expr(c, &parent_val, parent_expr);
			c_value_rvalue(c, &parent_val);
			base = str_printf("___var_%d.ptr", parent_val.var);
			len  = str_printf("___var_%d.len", parent_val.var);
		}
		else
		{
			CValue parent_addr = {0};
			c_emit_lvalue_addr(c, parent_expr, &parent_addr, type_get_ptr(parent_type));
			parent_align    = parent_addr.align;
			parent_addr_var = parent_addr.var;
			if (parent_type->type_kind == TYPE_FLEXIBLE_ARRAY)
			{
				// A flexible array member has no wrapper and no known length.
				base = str_printf("((%s*)___var_%d)", c_type_name(c, parent_type->array.base), parent_addr.var);
			}
			else
			{
				base = str_printf("((%s*)___var_%d)->ptr", c_type_name(c, parent_type), parent_addr.var);
				len  = str_printf("%lluULL", (unsigned long long)parent_type->array.len);
			}
			// A constant index into a fixed size array was checked by sema.
			if (expr_is_const(index_expr)) is_safe = false;
		}

		CValue index_val = {0};
		c_emit_expr(c, &index_val, index_expr);
		c_value_rvalue(c, &index_val);
		c_ensure_cvalue_var(c, &index_val, type_sz);
		if (expr->subscript_expr.index.start_from_end)
		{
			Type *index_type = c_safe_type_lower(index_val.type);
			const char *iname = c_type_name(c, index_type);
			int from_end      = c_emit_temp_var(c, NULL, index_type);
			PRINTF("%s ___var_%d = (%s)(%s) - ___var_%d;\n", iname, from_end, iname, len, index_val.var);
			index_val.var = from_end;
		}
		if (is_safe && len)
		{
			c_emit_index_check(c, &index_val, len, index_expr->loc);
		}
		if (parent_align && parent_align < c_type_alignment(parent_type->array.base))
		{
			out_val->align = parent_align;
			PRINTF("%s ___var_%d = (%s)((uint8_t*)___var_%d + ___var_%d * sizeof(%s));\n", tname, temp, tname,
			       parent_addr_var, index_val.var, c_type_name(c, parent_type->array.base));
			return;
		}
		PRINTF("%s ___var_%d = (%s)&(%s[___var_%d]);\n", tname, temp, tname, base, index_val.var);
		return;
	}

	CValue inner_val = {0};
	c_emit_expr(c, &inner_val, expr);
	if (inner_val.var != 0)
	{
		if (inner_val.kind == CV_ADDRESS)
		{
			PRINTF("%s ___var_%d = (%s)___var_%d;\n", tname, temp, tname, inner_val.var);
		}
		else
		{
			c_value_addr(c, &inner_val);
			PRINTF("%s ___var_%d = (%s)&___var_%d;\n", tname, temp, tname, inner_val.var);
		}
	}
	else
	{
		PRINTF("%s ___var_%d = NULL;\n", tname, temp);
	}
}

const char *c_decl_ref(GenContext *c, Decl *decl)
{
	if (decl->decl_kind == DECL_FUNC)
	{
		c_emit_function_decl(c, decl, false);
	}
	else
	{
		c_emit_global_decl(c, decl);
	}
	return c_get_decl_name(decl);
}

// The C lvalue that holds a variable: the global's name (declared on first use) or the local's ___var_N.
const char *c_decl_lvalue_str(GenContext *c, Decl *decl)
{
	if (c_is_file_global(decl))
	{
		c_emit_global_decl(c, decl);
		return c_get_decl_name(decl);
	}
	return str_printf("___var_%d", c_get_or_create_decl_var(c, decl));
}

void c_emit_lvalue_read(GenContext *c, Expr *expr, CValue *out_val, Type *target_type)
{
	target_type = c_safe_type_lower(target_type);
	if (target_type->type_kind == TYPE_VOID && expr)
	{
		target_type = c_expr_type(expr);
	}
	ASSERT(target_type->type_kind != TYPE_VOID);

	if (!expr)
	{
		int temp = c_emit_temp_var(c, out_val, target_type);
		c_emit_var_zero_init(c, temp, target_type);
		return;
	}
	switch (expr->expr_kind)
	{
		case EXPR_RECAST:
		case EXPR_ADDR_CONVERSION:
		case EXPR_RVALUE:
			// The conversion expressions produce their own type.
		case EXPR_IDENTIFIER:
		case EXPR_DECL:
			// Variables are read by the expression emitter (c_emit_identifier_expr, c_emit_local_decl).
			c_emit_expr(c, out_val, expr);
			c_value_rvalue(c, out_val);
			return;
		default:
			break;
	}

	int temp          = c_emit_temp_var(c, out_val, target_type);
	const char *tname = c_type_name(c, target_type);

	if (expr->expr_kind == EXPR_ACCESS_RESOLVED && expr->access_resolved_expr.parent)
	{
		Type *raw_parent_type = type_flatten(expr->access_resolved_expr.parent->type);
		bool parent_is_ptr    = false;
		if (raw_parent_type && raw_parent_type->type_kind == TYPE_POINTER && raw_parent_type->pointer)
		{
			parent_is_ptr   = true;
			raw_parent_type = type_flatten(raw_parent_type->pointer);
		}
		if (raw_parent_type && raw_parent_type->type_kind == TYPE_ENUM && raw_parent_type->decl)
		{
			Decl *enum_decl = raw_parent_type->decl;
			Decl *member    = c_decl_unwrap(expr->access_resolved_expr.ref);
			if (member && member->decl_kind == DECL_VAR)
			{
				c_emit_enum_assoc_access(c, enum_decl, member, expr->access_resolved_expr.parent, parent_is_ptr, temp, tname, target_type, false);
				return;
			}
		}
		Type *parent_type = c_expr_type(expr->access_resolved_expr.parent);
		Decl *member      = c_decl_unwrap(expr->access_resolved_expr.ref);
		Type *base_type   = parent_type;
		if (base_type && base_type->type_kind == TYPE_POINTER && base_type->pointer)
		{
			base_type = c_safe_type_lower(base_type->pointer);
		}
		if (member && member->decl_kind == DECL_FUNC)
		{
			if (base_type && c_type_is_any(base_type))
			{
				CValue parent_val = {0};
				c_emit_expr(c, &parent_val, expr->access_resolved_expr.parent);
				c_value_rvalue(c, &parent_val);
				c_emit_dynamic_dispatcher(c, member);
				const char *disp_resolver = c_intern(str_printf("__c3_dyn_resolve_%s", c_get_decl_name(member)));
				if (parent_val.var != 0)
				{
					const char *tid = (parent_type->type_kind == TYPE_POINTER)
					                      ? str_printf("((__c3_any__*)___var_%d)->typeid", parent_val.var)
					                      : str_printf("___var_%d.typeid", parent_val.var);
					PRINTF("%s ___var_%d = (%s)%s((c3typeid_t)%s);\n", tname, temp, tname, disp_resolver, tid);
				}
				else
				{
					PRINTF("%s ___var_%d = NULL;\n", tname, temp);
				}
				return;
			}
			PRINTF("%s ___var_%d = (%s)&%s;\n", tname, temp, tname, c_decl_ref(c, member));
			return;
		}
	}

	CValue addr_val = {0};
	c_emit_lvalue_addr(c, expr, &addr_val, type_get_ptr(target_type));

	PRINTF("%s ___var_%d;\n", tname, temp);
	if (!addr_val.var)
	{
		PRINTF("__c3_memset(&___var_%d, 0, sizeof(%s));\n", temp, tname);
	}
	else if (c_type_is_aggregate(target_type) || c_type_is_int128(target_type) || addr_val.align)
	{
		PRINTF("__c3_memcpy(&___var_%d, %s, sizeof(%s));\n", temp, c_addr_bytes(&addr_val), tname);
	}
	else
	{
		PRINTF("___var_%d = *(%s*)___var_%d;\n", temp, tname, addr_val.var);
	}
}

// The value of an assignment expression: the stored value.
static void c_assign_result(GenContext *c, Type *type, CValue *right_val, CValue *out_val)
{
	if (!out_val) return;
	if (right_val && right_val->var)
	{
		*out_val = *right_val;
		return;
	}
	c_emit_var_zero_init(c, c_emit_temp_var(c, out_val, type), type);
}

void c_emit_lvalue_assign(GenContext *c, Expr *left, CValue *addr, CValue *right_val, CValue *out_val)
{
	if (!left)
	{
		return;
	}
	if (left->expr_kind == EXPR_SWIZZLE)
	{
		Expr *parent       = exprptr(left->swizzle_expr.parent);
		Type *parent_type  = c_expr_type(parent);
		CValue parent_addr = (addr && addr->var) ? *addr : (CValue){0};
		if (!parent_addr.var) c_emit_lvalue_addr(c, parent, &parent_addr, type_get_ptr(parent_type));
		const char *ptname = c_type_name(c, parent_type);
		// The lanes of a misaligned vector are changed in an aligned copy.
		const char *vec    = str_printf("((%s*)___var_%d)->ptr", ptname, parent_addr.var);
		int copy           = parent_addr.align ? c_create_variable(c) : 0;
		if (copy)
		{
			PRINTF("%s ___var_%d;\n__c3_memcpy(&___var_%d, %s, sizeof(%s));\n", ptname, copy, copy, c_addr_bytes(&parent_addr), ptname);
			vec = str_printf("___var_%d.ptr", copy);
		}
		const char *sw     = left->swizzle_expr.swizzle;
		int sw_len         = (int)strlen(sw);
		Type *target_type  = c_expr_type(left);
		bool r_is_vec      = (right_val && right_val->type && c_type_is_vec_or_arr(right_val->type));
		for (int i = 0; i < sw_len; i++)
		{
			int p_idx = SWIZZLE_INDEX(sw[i]);
			if (r_is_vec)
			{
				if (right_val->kind == CV_ADDRESS)
				{
					PRINTF("%s[%d] = ((%s*)___var_%d)->ptr[%d];\n", vec, p_idx, c_type_name(c, right_val->type), right_val->var, i);
				}
				else
				{
					PRINTF("%s[%d] = ___var_%d.ptr[%d];\n", vec, p_idx, right_val->var, i);
				}
			}
			else if (right_val && right_val->var != 0)
			{
				PRINTF("%s[%d] = ___var_%d;\n", vec, p_idx, right_val->var);
			}
			else
			{
				Type *elem_t = c_safe_type_lower(parent_type->array.base);
				PRINTF("%s[%d] = (%s)%s;\n", vec, p_idx, c_type_name(c, elem_t), c_type_zero_literal(elem_t));
			}
		}
		if (copy) PRINTF("__c3_memcpy(%s, &___var_%d, sizeof(%s));\n", c_addr_bytes(&parent_addr), copy, ptname);
		if (out_val)
		{
			int res_temp = c_emit_temp_var(c, out_val, target_type);
			PRINTF("%s ___var_%d = {0};\n", c_type_name(c, target_type), res_temp);
			for (int i = 0; i < sw_len; i++)
			{
				int p_idx = SWIZZLE_INDEX(sw[i]);
				PRINTF("___var_%d.ptr[%d] = %s[%d];\n", res_temp, i, vec, p_idx);
			}
		}
		return;
	}
	if (left->expr_kind == EXPR_IDENTIFIER || left->expr_kind == EXPR_DECL)
	{
		Decl *left_decl = (left->expr_kind == EXPR_DECL) ? left->decl_expr : c_decl_unwrap(left->ident_expr);
		if (left_decl && left_decl->decl_kind == DECL_VAR && left_decl->var.kind == VARDECL_PARAM_EXPR && left_decl->var.init_expr)
		{
			c_emit_lvalue_assign(c, left_decl->var.init_expr, addr, right_val, out_val);
			return;
		}
		Type *lt = c_decl_type(left_decl);
		c_emit_store(c, c_decl_lvalue_str(c, left_decl), lt, right_val);
		c_assign_result(c, lt, right_val, out_val);
		return;
	}

	// Any other lvalue, including a recast one, is stored through its address, as LLVM.
	Type *target_type = c_expr_type(left);
	CValue addr_val   = (addr && addr->var) ? *addr : (CValue){0};
	if (!addr_val.var)
	{
		c_emit_lvalue_addr(c, left, &addr_val, type_get_ptr(target_type));
	}
	const char *tname = c_type_name(c, target_type);
	bool has_value    = right_val && right_val->var;
	if (addr_val.align && !has_value)
	{
		PRINTF("__c3_memset(%s, 0, sizeof(%s));\n", c_addr_bytes(&addr_val), tname);
	}
	else if (has_value && (c_type_is_int128(target_type) || addr_val.align))
	{
		// An int128 address may not be 16 byte aligned.
		ASSERT(strcmp(tname, c_type_name(c, c_safe_type_lower(right_val->type))) == 0);
		c_value_rvalue(c, right_val);
		PRINTF("__c3_memcpy(%s, &___var_%d, sizeof(%s));\n", c_addr_bytes(&addr_val), right_val->var, tname);
	}
	else
	{
		c_emit_store(c, str_printf("(*(%s*)___var_%d)", tname, addr_val.var), target_type, right_val);
	}
	c_assign_result(c, target_type, right_val, out_val);
}